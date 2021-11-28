{$REGION 'license'}
//{$define DEBUG}
{
 * This file is a part of mbulib.
 * Copyright (C) 2008-2010 by Boris Popov <borisxm@gmail.com>.
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
  mbuserial;

interface

uses
  SysUtils, synaser, mbubase, Classes;

type
  TMBIOState = (mbiosIdle, mbiosTx, mbiosRx, mbiosSilence);

  { TMBSerialMaster }

  {: @abstract(Base class for RTU and ASCII mode clients.) }
  TMBSerialMaster = class(TMBClient)
  private
    procedure FinishQuery(AQ: TMBQuery; AStatus: TMBErrors);
  protected
    FPortName		: string;
    FBaudrate		: integer;
    FDataBits		: integer;
    FStopBits		: integer;
    FParity		: char;
    FSDev               : TBlockSerial;
    FState		: TMBIOState;
    FTxBuffer		: array of byte;
    FRxBuffer		: array of byte;
    FCQ			: TMBQuery;	// Currently processed query
    FBufferSize		: integer;	// Maximum length of Tx and Rx buffers
    FReplyTimeout	: cardinal;	// Timeout for wating a reply (1s)
    FRxTimeout		: cardinal;	// Intercharacter timeout (1.5 * ct)
    FSilencePeriod	: cardinal;	// Time between exchanges on the bus (3.5 * ct)

    procedure Execute; override;
    procedure Receive;virtual;abstract;
    procedure SendQuery(AQ: TMBQuery);virtual;abstract;
    procedure Initialise;
    procedure Finalise;override;
  public
    constructor Create;
    destructor Destroy;override;
    function Connect:boolean;override;
    procedure Disconnect;override;

    property sdev: TBlockSerial read FSDev;
    property PortName: string read FPortName write FPortName;
    property BaudRate: integer read FBaudRate write FBaudRate;
    property DataBits: integer read FDataBits write FDataBits;
    property StopBits: integer read FStopBits write FStopBits;
    property Parity: char read FParity write FParity;
    property ReplyTimeout: cardinal read FReplyTimeout write FReplyTimeout;
    property RxTimeout: cardinal read FRxTimeout;
    property SilencePeriod: cardinal read FSilencePeriod;
  end;

  { TMBRTUMaster }

  TMBRTUMaster = class(TMBSerialMaster)
  protected
    procedure Receive;override;

  public
    procedure SendQuery(AQ: TMBQuery);override;  
    constructor Create;
  end;


implementation

uses
  synautil;

{ TMBSerialMaster }

constructor TMBSerialMaster.Create;
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
end;

destructor TMBSerialMaster.Destroy;
begin
  Disconnect;
  FreeAndNil(fsdev);
  inherited;
end;

function TMBSerialMaster.Connect:boolean;
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

procedure TMBSerialMaster.Disconnect;
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

procedure TMBSerialMaster.Initialise;
begin
  inherited;
end;

procedure TMBSerialMaster.Finalise;
begin
//
end;

// Complete query processing, set error code if any
procedure TMBSerialMaster.FinishQuery(AQ: TMBQuery; AStatus: TMBErrors);
begin
  FState := mbiosSilence;
  FCQ := nil;
  AQ.Complete(AStatus);
  FRxQueue.Push(AQ);
end;

procedure TMBSerialMaster.Execute;
var
  rq	: TMBQuery;
begin
  try
    Initialise;
    while not MustDie do begin
  try
      case FState of
        mbiosIdle: begin
                   rq := FTxQueue.Pop as TMBQuery;
                   if (rq = nil) then begin
                      FSubmitEvent.WaitFor(10000000);
                      if  MustDie then
                          break;
                      rq := FTxQueue.Pop as TMBQuery;
                      if (rq = nil) then
                         continue;
                         end;
                   SendQuery(rq);
                   end;
        mbiosTx:   begin
                   end;
        mbiosRx:   begin
                   Receive;
                   end;
        mbiosSilence: begin
                      Sleep(FSilencePeriod);
                      FState := mbiosIdle;
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

{ TMBRTUMaster }

constructor TMBRTUMaster.Create;
begin
  inherited Create;
  FBufferSize := MB_RTU_PKT_LEN_MAX;
  SetLength(FRxBuffer, FBufferSize);
  SetLength(FTxBuffer, FBufferSize);
end;

procedure TMBRTUMaster.SendQuery(AQ: TMBQuery);
var
  rqlen, rqcrc	: word;
  {$ifdef DEBUG}
  i:integer;
  {$endif}
begin
  if FCQ <> nil then
    raise EMBError.Create('FCQ is not NIL in SendQuery()');

  FCQ := AQ;

  FTxBuffer[0] := AQ.SlaveAddr;
  AQ.Rq.GetPDU(FTxBuffer[1]);
  rqlen := AQ.Rq.Len;
  inc(rqlen, 1);		// slaveid
  rqcrc := mbu_CRC(FTxBuffer[0], rqlen);
  FTxBuffer[rqlen] := lo(rqcrc);
  FTxBUffer[rqlen + 1] := hi(rqcrc);
  inc(rqlen, 2);
  FSDev.Purge;
  if FSDev.SendBuffer(@FTxBuffer[0], rqlen) = rqlen then begin
    FState := mbiosRx;
  end else begin
    FinishQuery(AQ, mbeTxFailed);
  end;
  {$ifdef DEBUG}
  writeln('-----transmit-----');
  for i:=0 to rqlen do
  writeln(inttostr(FTxBuffer[i])+',');
  writeln('------------------');
  {$endif}
end;

procedure TMBRTUMaster.Receive;
var
  rp	: TMBPDU;
  nRead, toRead, readleft: integer;
  rplen, rpcrc: word;
  tstart	: longword;
  {$ifdef DEBUG}
  i:integer;
  {$endif}
begin
  tstart := synautil.GetTick;
  rp := FCQ.Rp;

  toRead := FBufferSize;
  readleft := toread;
  rplen := 0;

  fsdev.InterPacketTimeout := false;
  while true do begin
    nread := FSDev.RecvBufferEx(@FRxBuffer[rplen], readleft, 1);
    inc(rplen, nread);
    dec(readleft, nread);
    if (rplen >= toread) then
      break;
    if (rplen = 5) and (FRxBuffer[0] = FCQ.SlaveAddr) and (FRxBuffer[1] = FTxBuffer[1] or $80) then
      break;	// Exception detected

    if synautil.TickDelta(tstart, synautil.GetTick) > FReplyTimeout then begin
      FinishQuery(FCQ, mbeTimeout);
      exit;
    end;
    if (toread = FBufferSize) and (rplen > 2) then begin
      // User didn't supplied the reply length. To save time, we'll calculate
      // intermediate crc and if it correct, bail out of the loop
      rpcrc := mbu_CRC(FRxBuffer[0], rplen);
      if rpcrc = 0 then
        break;
    end;
  end;

  rpcrc := mbu_CRC(FRxBuffer[0], rplen);
  if rpcrc <> 0 then begin
    FinishQuery(FCQ, mbeCRCMismatch);
    exit;
  end;
  rp.SetPDU(FRxBuffer[1], rplen - 3);
  FinishQuery(FCQ, rp.ParseReply);
  {$ifdef DEBUG}
  writeln('-----receive------');
  for i:=0 to rplen do
  writeln(inttostr(FRxBuffer[i])+',');
  writeln('------------------');
  {$endif}
end;

end.

