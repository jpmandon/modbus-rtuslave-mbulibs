{$REGION 'license'}
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
unit mbutcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  blcksock, mbubase;

type
   TMBIOState = (mbiosIdle, mbiosTx, mbiosRx, mbiosSilence);
 { TMBTCPClient }

  TMBTCPClient = class(TMBClient)
  private
    function GetQWait(AIndex: integer): TMBquery;
  private
    procedure SendQuery(AQ: TMBQuery);
    procedure Receive;
    procedure PurgeOld;
    function LookupByQID(AQID: cardinal): TMBQuery;
    procedure PutOnWait(AQ: TMBQuery);
    procedure TakeFromWait(AQ: TMBQuery);

    property QWait[AIndex: integer]: TMBquery read GetQWait;
  protected
    FTargetHost		: string;
    FTargetPort		: string;
    FIPInterface	: string;
    FSock		: TTCPBlockSocket;
    FState		: TMBIOState;
    FMaxPendingQueries	: cardinal;	// Maximum number of queries which will be enqueued to server
    FWaitCnt		: cardinal;	// Number of queries on hold
    FQWait		: TList;	// List of queries wating for respose
    FTxBuffer		: array of byte;
    FRxBuffer		: array of byte;
    FBufferSize		: integer;	// Maximum length of Tx and Rx buffers
    FReplyTimeout	: cardinal;	// Timeout for wating a reply (1s)
    FRxTimeout		: cardinal;	// Intercharacter timeout (1.5 * ct)
    FSilencePeriod	: cardinal;	// Time between exchanges on the bus (3.5 * ct)

    procedure Execute;override;
    procedure Initialise;
    procedure Finalise;override;
  public
    constructor Create;
    destructor Destroy;override;
    function Connect:boolean;override;
    procedure Disconnect;override;

    property Sock: TTCPBlockSocket read FSock;
    property TargetHost: string read FTargetHost write FTargetHost;
    property TargetPort: string read FTargetPort write FTargetPort;
    property ReplyTimeout: cardinal read FReplyTimeout write FReplyTimeout;
    property RxTimeout: cardinal read FRxTimeout;
    property SilencePeriod: cardinal read FSilencePeriod;
  end;

implementation

uses
  synautil;

{ TMBTCPClient }

constructor TMBTCPClient.Create;
begin
  inherited Create;
  FBufferSize := MB_RTU_PKT_LEN_MAX + 7;
  FMaxPendingQueries := 10;
  SetLength(FRxBuffer, FBufferSize);
  SetLength(FTxBuffer, FBufferSize);
  FTargetHost := 'localhost';
  FTargetPort := '502';
  FIPInterface := cAnyHost;
  FReplyTimeout := 1000;

  FQWait := TList.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
end;

destructor TMBTCPClient.Destroy;
begin
  Disconnect;
  FreeAndNil(FQWait);
  FreeAndNil(FSock);
  inherited Destroy;
end;

function TMBTCPClient.Connect: boolean;
begin
  result := Connected;
  if result then
    exit;

  FSock.Bind(FIPInterface, cAnyPort);
  if FSock.LastError <> 0 then
    exit;
  FSock.Connect(FTargetHost, FTargetPort);
  try
    MustDie := false;
    Resume;
  except
    FSock.CloseSocket;
  end;
  FConnected := true;
  result := true;
end;

procedure TMBTCPClient.Disconnect;
var
  i	: integer;
  q	: TMBQuery;
begin
  if not Connected then
    exit;
  try
    FSock.CloseSocket;
    FreeOnTerminate := false;
    MustDie := true;
    FSubmitEvent.SetEvent;
    while not Terminated do begin
//	  CheckSynchronize(0);
      Sleep(1);
    end;
    for i := 0 to pred(FQWait.Count) do begin
      q := QWait[i];
      if q = nil then
        continue;
      q.Complete(mbeCommFailed);
    end;
  finally
    FConnected := false;
  end;
end;

function TMBTCPClient.LookupByQID(AQID: cardinal): TMBQuery;
var
  i	: integer;
begin
  for i := 0 to pred(FQWait.Count) do begin
    result := QWait[i];
    if result = nil then
      continue;
    if result.QId = AQid then
      exit;
  end;
  result := nil;
end;

procedure TMBTCPClient.PutOnWait(AQ: TMBQuery);
var
  i	: integer;
begin
  i := FQWait.IndexOf(nil);
  if i <> -1 then
    FQWait[i] := AQ
  else
    FQWait.Add(AQ);
  inc(FWaitCnt);
end;

procedure TMBTCPClient.TakeFromWait(AQ: TMBQuery);
begin
  dec(FWaitCnt);
  FQWait[FQWait.IndexOf(AQ)] := nil;
end;

function TMBTCPClient.GetQWait(AIndex: integer): TMBquery;
begin
  result := TMBQuery(FQWait[AIndex]);
end;

procedure TMBTCPClient.SendQuery(AQ: TMBQuery);
var
  rqlen, qid	: word;
begin
  rqlen := AQ.Rq.Len;
  inc(rqlen, 1);		// slaveid
  qid := NextSeq;
  AQ.QId := qid;
  // Fill MBAP
  mbu_PutU16BE(FTxBuffer[0], qid);	// TID
  mbu_PutU16BE(FTxBuffer[2], 0);		// Protocol ID
  mbu_PutU16BE(FTxBuffer[4], rqlen);	// Length
  FTxBuffer[6] := AQ.SlaveAddr;		// UnitId
  AQ.Rq.GetPDU(FTxBuffer[7]);
  inc(rqlen, 6);
  FSock.Purge;
  if FSock.SendBuffer(@FTxBuffer[0], rqlen) = rqlen then begin
    AQ.StartTime := synautil.GetTick;
    PutOnWait(AQ);
  end else begin
    aq.Complete(mbeTxFailed);
  end;
end;

type
  TMBAP = packed record
    TID		: word;
    ProtoID	: word;
    PktLength	: word;
    SlaveId	: byte;
  end;

procedure TMBTCPClient.Receive;
var
  rp	: TMBPDU;
  nRead, toRead: integer;
  mbap	: TMBAP;
  q	: TMBQuery;
begin
  nread := FSock.RecvBufferEx(@mbap, sizeof(mbap), 1);
  if nread <> sizeof(mbap) then
    exit;

  q := LookupByQID(mbu_GetU16BE(mbap.TID));
  toRead := mbu_GetU16BE(mbap.PktLength);
  if (q = nil) or (toRead > MB_PDU_LEN_MAX) then begin
    FSock.RecvBufferEx(@FRxBuffer[0], MB_RTU_PKT_LEN_MAX, 1);	// try to discard the rest of packet
    exit;
  end;

  nread := FSock.RecvBufferEx(@FRxBuffer[0], toRead, 1);

  rp := q.rp;

  rp.SetPDU(FRxBuffer[0], toRead - 1);
  TakeFromWait(q);
  q.Complete(rp.ParseReply);
  FRxQueue.Push(q);
end;

procedure TMBTCPClient.PurgeOld;
var
  i	: integer;
  q	: TMBQuery;
  tnow	: longword;
begin
  tnow := synautil.GetTick;
  for i := 0 to pred(FQWait.Count) do begin
    q := QWait[i];
    if q = nil then
      continue;
    if synautil.TickDelta(q.StartTime, tnow) > FReplyTimeout then begin
      q.Complete(mbeTimeout);
      TakeFromwait(q);
    end;
  end;
end;

procedure TMBTCPClient.Execute;
var
  rq	: TMBQuery;
begin
  try
    Initialise;
    while not MustDie do begin
      try
        // Process incoming data
        if FSock.CanReadEx(1) then begin
          Receive;
        end;

        // Try to send data
        if FWaitCnt < FMaxPendingQueries then begin
          rq := FTxQueue.Pop as TMBQuery;
          if (rq <> nil) then begin
            SendQuery(rq);
          end;
        end;

        // Look through, and discard timedout queries
        PurgeOld;
      finally
      end;
    end;
  finally
    Terminate;
    Finalise;
  end;
end;

procedure TMBTCPClient.Initialise;
begin

end;

procedure TMBTCPClient.Finalise;
begin
  inherited Finalise;
end;

end.

