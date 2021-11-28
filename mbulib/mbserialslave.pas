{$REGION 'license'}
//{$define DEBUG}
{
 * This file is a part of mbulib.
 * Copyright (C) 2021 by Jean-Pierre Mandon <jp.mandon@gmail.com>.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
}
{$ENDREGION}
unit
  mbserialslave;

interface

uses
  SysUtils, synaser, mbubase, Classes;

type
  TMBIOState = (waitSlaveNumber, waitFunctionCode, waitAddress, waitLength, waitData, waitCRC, ignoreNbByte, sendReponse);
  
  { TMBSerialSlave }

  TNotifyEvent = procedure(Sender : TObject) of object;

  TMBSerialSlave = class(TMBIOThread)
  private
    procedure setMBdata(i:byte;value:pointer);
    procedure   Showstatus;
  protected
    FrxFrameEvent       : TNotifyEvent;
    FwriteEvent         : TNotifyEvent;
    FSDev               : TBlockSerial;             // serial device
    FSlaveNumber        : byte;                     // slave number
    FPortName		: string;                   // serial port name
    FBaudrate		: integer;
    FParity		: char;
    FDataBits		: integer;
    FStopBits		: integer;
    FMBdata             : array[0..255] of ^uint16; // pointer to external data
    FMBstartAddress     : uint16  ;                 // start address of modbus field
    FMBdataLength       : byte;                     // length of modbus data
    FState		: TMBIOState;               // position for state machine
    FTxBuffer		: array of byte;
    FRxBuffer		: array of byte;
    FrxIndex            : integer;                  // index in the current received frame
    FBufferSize		: integer;	            // Maximum length of Tx and Rx buffers
    FReplyTimeout	: cardinal;	            // Timeout for wating a reply (1s)
    FRxTimeout		: cardinal;	            // Intercharacter timeout (1.5 * ct)
    FSilencePeriod	: cardinal;	            // Time between exchanges on the bus (3.5 * ct)
    FForMe              : boolean;                  // frame is for me ?
    FFrameReady         : boolean;                  // frame is avalaible
    rtuSlaveCriticalSection : TRTLCriticalSection;

    procedure Execute;    override;
    procedure Initialise;
    procedure Finalise;   override;
    function computeNFMFrameLength:integer;         // compute length of 'not for me' answer
    procedure computeFMresponse;                    // compute and send modbus response
    procedure buildInvalidRequest(functionType : byte);  // invalid request

  public
    queryStack          : TStringList;              // list of query

    constructor Create;
    destructor  Destroy;override;
    function    Connect:boolean;override;
    procedure   Disconnect;override;

    property rxFrameEvent  : TNotifyEvent                      write FrxFrameEvent;
    property writeEvent    : TNotifyEvent                      write FwriteEvent;
    property sdev          : TBlockSerial read FSDev;
    property slaveNumber   : byte         read FSlaveNumber    write FSlaveNumber;
    property PortName      : string       read FPortName       write FPortName;
    property BaudRate      : integer      read FBaudRate       write FBaudRate;
    property Parity        : char         read FParity         write FParity;
    property DataBits      : integer      read FDataBits       write FDataBits;
    property StopBits      : integer      read FStopBits       write FStopBits;
    property MBdata[i:byte]: pointer                           write setMBdata;
    property MBstartaddress: uint16                            write FMBstartAddress;
    property MBdataLength  : byte                              write FMBdataLength;
    property ReplyTimeout  : cardinal     read FReplyTimeout   write FReplyTimeout;
    property RxTimeout     : cardinal     read FRxTimeout;
    property SilencePeriod : cardinal     read FSilencePeriod;

  end; 
  
  implementation

uses
  synautil;

{ TMBSerialSlave }


constructor TMBSerialSlave.Create;
begin
  inherited Create;
  FPortName := '';
  FBaudrate := 19200;
  FDataBits := 8;
  FStopBits := 1;
  FParity := 'N';
  FSDev := TBlockSerial.Create;
  FSDev.LinuxLock := false;	// questionable feature, so disable it
  FReplyTimeout := 1000;
  SetLength(FRxBuffer, 256);
  SetLength(FTxBuffer, 256);
  queryStack:=TStringList.create;
  InitCriticalSection(rtuSlaveCriticalSection);
end;

destructor TMBSerialSlave.Destroy;
begin
  Disconnect;
  FreeAndNil(fsdev);
  inherited;
end;

function TMBSerialSlave.Connect:boolean;
var
  timo : double;
  pbits	: integer;
begin
  result := Connected;
  if result then
    exit;
  FSDev.RaiseExcept := true;
  try
    FSDev.Connect(FPortName);
    FSDev.Config(FBaudrate, FDataBits, FParity, (FStopBits - 1) * 2, false, false);
  finally
    FSDev.RaiseExcept := false;
  end;
  if FParity = 'N' then
    pbits := 0
  else
    pbits := 1;
  timo := (1e3 * (1 + FDataBits + FStopBits + pbits) * 1.55 / FBaudrate); // in milliseconds
  if (timo < 0.5) then
    timo := 1;
  FRxTimeout := Round(timo);
  FSilencePeriod := Round(FRxTimeout * 3.55);
  try
    FSDev.Purge;
    MustDie := false;
    start;
  except
    FSDev.CloseSocket;
  end;
  FConnected := true;
  result := true;
end;

procedure TMBSerialSlave.Disconnect;
begin
  if not Connected then
    exit;
  try
    FSDev.CloseSocket;
    FreeOnTerminate := false;
    MustDie := true;
    FSubmitEvent.SetEvent;
    while not Terminated do begin
	  CheckSynchronize(0);
      Sleep(1);
    end;
  finally
    FConnected := false;
  end;
end;

procedure TMBSerialSlave.Initialise;
begin
  inherited;
end;

procedure TMBSerialSlave.Finalise;
begin
//
end;

procedure TMBSerialSlave.Execute;
var
  received : integer;
  tstart	   : longword;
  // frame info
  codeFonction     : byte;
  adresse          : uint16;
  longueur         : uint16;
  // byte to read
  readLeft         : integer;
  rpcrc            : integer;
begin
  try
    Initialise;
    FSDev.InterPacketTimeout := false;
    while not MustDie do begin
  try
      case FState of
        waitSlaveNumber:      begin
                              FrxIndex:=0;
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], 1, 1);
                              if received=1 then
                                 begin
                                 inc(FrxIndex);
                                 if FRxBuffer[0]=FSlaveNumber then FForMe:=true      // frame is for me
                                 else FForMe:=false;
                                 FState:=waitFunctionCode;
                                 tstart := synautil.GetTick;
                                 end
                              else continue;
                              end;
        waitFunctionCode:     begin
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], 1, 1);
                              if received=1 then
                                 begin
                                 inc(FrxIndex);
                                 codeFonction:=FRxBuffer[1];
                                 FState:=waitAddress;
                                 tstart := synautil.GetTick;
                                 end
                              else
                                 begin
                                 if (synautil.GetTick-tstart)>FRxTimeout then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end;
                                 continue;
                                 end;
                              end;
        waitAddress:          begin
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], 2, 1);
                              if received=2 then
                                 begin
                                 inc(FrxIndex,2);
                                 adresse:= uint16(FRxBuffer[2] shl 8 + FRxBuffer[3]);
                                 FState:=waitLength;
                                 tstart := synautil.GetTick;
                                 end
                                 else
                                 begin
                                 if (synautil.GetTick-tstart)>(2*FRxTimeout) then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end;
                                 end;
                              end;
        waitLength:           begin
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], 2, 1);
                              if received=2 then
                                 begin
                                 inc(FrxIndex,2);
                                 longueur:= uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]);
                                 case codeFonction of
                                        MB_FUNC_READ_INPUT_REGISTER:       begin
                                                                           readLeft:=2;
                                                                           FState:=waitCRC;
                                                                           end;
                                        MB_FUNC_READ_HOLDING_REGISTER:     begin
                                                                           readLeft:=2;
                                                                           FState:=waitCRC;
                                                                           end;
                                        MB_FUNC_WRITE_REGISTER:            begin
                                                                           readLeft:=2;
                                                                           FState:=waitCRC;
                                                                           end;
                                        MB_FUNC_WRITE_MULTIPLE_REGISTERS:  begin
                                                                           readLeft:=(longueur*2)+1;
                                                                           FState:=waitData;
                                                                           end;
                                        end;
                                 tstart := synautil.GetTick;
                                 end
                                 else
                                 begin
                                 if (synautil.GetTick-tstart)>(2*FRxTimeout) then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end;
                                 end;
                              end;
        waitData:             begin
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], readLeft, 1);
                              if received=readLeft then
                                 begin
                                 inc(FrxIndex,readLeft);
                                 FState:=waitCRC;
                                 tstart := synautil.GetTick;
                                 end
                                 else
                                 begin
                                 if (synautil.GetTick-tstart)>(readLeft*FRxTimeout) then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end;
                                 end;
                              end;
        waitCRC:              begin
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], 2, 1);
                              if received=2 then
                                 begin
                                 inc(FrxIndex,2);
                                 rpcrc := mbu_CRC(FRxBuffer[0], FrxIndex);
                                 if rpcrc<>0 then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end
                                   else
                                   begin
                                   FState:=waitSlaveNumber;
                                   FFrameReady:=True;
                                   EnterCriticalsection(rtuSlaveCriticalSection);
                                   Showstatus;
                                   LeaveCriticalsection(rtuSlaveCriticalSection);
                                   if not(FForMe) then
                                     begin
                                     readLeft:=computeNFMFrameLength;
                                     FState:=ignoreNbByte;
                                     FrxIndex:=0;
                                     tstart := synautil.GetTick;
                                     continue;
                                     end
                                     else
                                     begin
                                     FState:=sendReponse;
                                     continue;
                                     end;
                                   end;
                                 end;
                              end;
        ignoreNbByte:         begin
                              received := FSDev.RecvBufferEx(@FRxBuffer[FrxIndex], 1, 1);
                              if received>0 then
                                begin
                                inc(FrxIndex,1);
                                if FrxIndex=readLeft then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end;
                                tstart := synautil.GetTick;
                                end
                              else
                                if (synautil.GetTick-tstart)>FRxTimeout then
                                   begin
                                   FState:=waitSlaveNumber;
                                   continue;
                                   end;
                              end;
        sendReponse:          begin
                              computeFMresponse;
                              FState:=waitSlaveNumber;
                              continue;
                              end;
        end;
      finally
      end;
    end;
  finally
    Terminate;
    Finalise;
  end;
end;

procedure TMBSerialSlave.setMBdata(i:byte;value:pointer);
begin
  FMBdata[i]:=value;
end;

{
when frame is for another slave, we must overlook response
this function compute length of the response
}
function TMBSerialSlave.computeNFMFrameLength:integer;
const
  headerLength = 3;
  crcLength    = 2;
var
  retFunction   : byte;
  retDataLength : uint16;
  frameLength   : uint16;
begin
     retFunction:=FRxBuffer[1];
     case retFunction of
       MB_FUNC_READ_INPUT_REGISTER:       begin
                                          retDataLength:=uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]);
                                          frameLength:=headerLength+(2*retDataLength)+crcLength;
                                          end;
       MB_FUNC_READ_HOLDING_REGISTER:     begin
                                          retDataLength:=uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]);
                                          frameLength:=headerLength+(2*retDataLength)+crcLength;
                                          end;
       MB_FUNC_WRITE_REGISTER:            begin
                                          frameLength:=8;
                                          end;
       MB_FUNC_WRITE_MULTIPLE_REGISTERS:  begin
                                          frameLength:=8;
                                          end;
       else
         begin

         end;
       end;
     result:=frameLength;
end;

procedure TMBSerialSlave.computeFMresponse;
var
  MBfunction       : byte;
  MBaddress        : uint16;
  MBlength         : uint16;
  MBvalue          : array[0..255] of uint16;
  i                : integer;
  responseLength   : integer;
  responseCrc      : uint16;

begin
  MBfunction:=FRxBuffer[1];
  MBaddress:=uint16(FRxBuffer[2] shl 8 + FRxBuffer[3]);
  // compute data length
  case MBfunction of
     MB_FUNC_WRITE_REGISTER:           begin
                                       MBlength:=1;
                                       end;
     MB_FUNC_WRITE_MULTIPLE_REGISTERS: begin
                                       MBlength:=uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]);
                                       end;
     MB_FUNC_READ_INPUT_REGISTER:      begin
                                       MBlength:=uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]);
                                       end;
     MB_FUNC_READ_HOLDING_REGISTER:    begin
                                       MBlength:=uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]);
                                       end;
  end;
  // check if request is valid
  if MBlength>125 then
     begin
     buildInvalidRequest(MBfunction);
     exit;
     end;
  if (MBaddress<FMBstartAddress) or (MBaddress>FMBstartAddress+FMBdataLength) then
     begin
     buildInvalidRequest(MBfunction);
     exit;
     end;
  if (MBaddress+MBlength)>(FMBstartAddress+FMBdataLength) then
     begin
     buildInvalidRequest(MBfunction);
     exit;
     end;
  // reponse frame header
  FTxBuffer[0]:=FSlaveNumber;
  FTxBuffer[1]:=MBfunction;
  responseLength:=2;
  // extract and write data for write instruction / build response frame
  case MBfunction of
       MB_FUNC_WRITE_REGISTER:              begin
                                            EnterCriticalsection(rtuSlaveCriticalSection);
                                            FMBdata[MBaddress-FMBstartAddress]^:=uint16(FRxBuffer[6+(i*2)] shl 8 + FRxBuffer[7+(i*2)]);
                                            if FwriteEvent <> nil then
                                               FwriteEvent(Self);
                                            LeaveCriticalsection(rtuSlaveCriticalSection);
                                            FTxBuffer[2]:=byte(MBaddress shr 8);
                                            FTxBuffer[3]:=byte(MBaddress);
                                            FTxBuffer[4]:=FRxBuffer[6];
                                            FTxBuffer[5]:=FRxBuffer[7];
                                            responseLength:=6;
                                            end;
       MB_FUNC_WRITE_MULTIPLE_REGISTERS:    begin
                                            EnterCriticalsection(rtuSlaveCriticalSection);
                                            for i:=0 to MBlength-1 do
                                                FMBdata[MBaddress-FMBstartAddress+i]^:=uint16(FRxBuffer[7+(i*2)] shl 8 + FRxBuffer[8+(i*2)]);
                                            if FwriteEvent <> nil then
                                               FwriteEvent(Self);
                                            LeaveCriticalsection(rtuSlaveCriticalSection);
                                            FTxBuffer[2]:=byte(MBaddress shr 8);
                                            FTxBuffer[3]:=byte(MBaddress);
                                            FTxBuffer[4]:=hi(MBlength);
                                            FTxBuffer[5]:=lo(MBlength);
                                            responseLength:=6;
                                            end;
       MB_FUNC_READ_INPUT_REGISTER:         begin
                                            FTxBuffer[2]:=byte(2*uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]));
                                            EnterCriticalsection(rtuSlaveCriticalSection);
                                            for i:=0 to MBlength-1 do
                                                begin
                                                  FTxBuffer[3+(2*i)]:=hi(FMBdata[MBaddress-FMBstartAddress+i]^);
                                                  FTxBuffer[4+(2*i)]:=lo(FMBdata[MBaddress-FMBstartAddress+i]^);
                                                end;
                                            LeaveCriticalsection(rtuSlaveCriticalSection);
                                            responseLength:=3+FTxBuffer[2];
                                            end;
       MB_FUNC_READ_HOLDING_REGISTER:       begin
                                            FTxBuffer[2]:=byte(2*uint16(FRxBuffer[4] shl 8 + FRxBuffer[5]));
                                            EnterCriticalsection(rtuSlaveCriticalSection);
                                            for i:=0 to MBlength-1 do
                                                begin
                                                  FTxBuffer[3+(2*i)]:=hi(FMBdata[MBaddress-FMBstartAddress+i]^);
                                                  FTxBuffer[4+(2*i)]:=lo(FMBdata[MBaddress-FMBstartAddress+i]^);
                                                end;
                                            LeaveCriticalsection(rtuSlaveCriticalSection);
                                            responseLength:=3+FTxBuffer[2];
                                            end;
       end;
  // build crc for response frame
  responseCrc := mbu_CRC(FTxBuffer[0], responseLength);
  FTxBuffer[responseLength] := lo(responseCrc);
  FTxBuffer[responseLength+1] := hi(responseCrc);
  inc(responseLength,2);
  // send response
  FSDev.SendBuffer(@FTxBuffer[0], responseLength);
end;

procedure TMBSerialSlave.buildInvalidRequest(functionType : byte);
var
  invalidRqCrc : uint16;
begin
  FTxBuffer[0]:=FSlaveNumber;
  FTxBuffer[1]:=functionType+$80;
  invalidRqCrc:= mbu_CRC(FTxBuffer[0], 2);
  FTxBuffer[2] := lo(invalidRqCrc);
  FTxBuffer[3] := hi(invalidRqCrc);
  FSDev.SendBuffer(@FTxBuffer[0], 4);
end;

procedure TMBSerialSlave.Showstatus;
var
  returnString : string;
begin
returnString:='';
if FFrameReady then
   begin
   SetString(returnString,PChar(@FRxBuffer[0]),FrxIndex);
   queryStack.Add(returnString);
   FFrameReady:=false;
   if FrxFrameEvent <> nil then
      FrxFrameEvent(Self);
   end;
end;

end.
