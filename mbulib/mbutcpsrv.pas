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
unit mbutcpsrv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synsock,
  blcksock, blogger, mbubase;

const
  MB_MAX_TCP_CONN = 20;
  mbu_tcpsrvnm: string = 'tcpsrv';

type

  {// TMBClientConnection }
  TMBTCPServer = class;

  { TMBTCPClientConnection }

  TMBTCPClientConnection = class(TMBClientConnection)
  private
    FRemoteHost	: string;
    FRemotePort	: string;
    FSock		: TTCPBlockSocket;
    FRxBuffer	: array[0 .. MB_TCP_PKT_LEN_MAX - 1] of byte;
    FTxBuffer	: array[0 .. MB_TCP_PKT_LEN_MAX - 1] of byte;
    FRxLen	: integer;	// number of bytes received
    FRxLeft	: integer;	// number of bytes to receive
    FPDULen	: integer;	// PDU length specified in MBAP
    FDead	: boolean;
    function Receive: TMBQuery;
  public
    constructor Create(AOwner: TMBTCPServer; ASocket: TSocket);
    destructor Destroy;override;
    procedure Transmit(AQuery: TMBQuery);override;

    property Dead: boolean read FDead;
  end;

  { TMBTCPServer }

  TMBTCPServer = class(TMBServer)
  private
    FLocalHost		: string;
    FLocalPort		: string;
    FIPInterface	: string;
    FListenSock		: TTCPBlockSocket;
    FConnList		: array[0 .. MB_MAX_TCP_CONN - 1] of TMBTCPClientConnection;
    FConnCount		: integer;
    procedure ProcessConnections;
    procedure NewConnection(ASocket: TSocket);
    function ClientExists(AClient: TMBClientConnection): boolean;
  protected
    procedure Execute;override;
  public
    constructor Create;
    destructor Destroy;override;
    function Connect:boolean;override;
    procedure Disconnect;override;

    property LocalHost: string read FLocalHost write FLocalHost;
    property LocalPort: string read FLocalPort write FLocalPort;
  end;


implementation

uses
  synautil;

{ TMBTCPServer }

constructor TMBTCPServer.Create;
begin
  inherited Create;
//  SetLength(FRxBuffer, FBufferSize);
//  SetLength(FTxBuffer, FBufferSize);
  FLocalHost := 'localhost';
  FLocalPort := '502';
  FIPInterface := cAnyHost;

  FListenSock := TTCPBlockSocket.Create;
  FListenSock.Owner := self;
end;

destructor TMBTCPServer.Destroy;
begin
  FreeAndNil(FListenSock);
  inherited Destroy;
end;

function TMBTCPServer.Connect: boolean;
begin
  result := Connected;
  if result then
    exit;

  FListenSock.Bind(FLocalHost, FLocalPort);
  if FListenSock.LastError <> 0 then
    exit;
  try
    FListenSock.SetLinger(true, 20);	// block for 20ms until bail out
    FListenSock.Listen;
    MustDie := false;
    Resume;
  except
    FListenSock.CloseSocket;
  end;
  FConnected := true;
end;

procedure TMBTCPServer.Disconnect;
begin
  if not Connected then
    exit;
  try
    FListenSock.CloseSocket;
    FreeOnTerminate := false;
    MustDie := true;
//    FSubmitEvent.SetEvent;
    while not Terminated do begin
//	  CheckSynchronize(0);
      Sleep(1);
    end;
  finally
    FConnected := false;
  end;
end;

procedure TMBTCPServer.NewConnection(ASocket: TSocket);
var
  conn	: TMBTCPClientConnection;
  i	: integer;
begin
  conn := TMBTCPClientConnection.Create(self, ASocket);
  for i := 0 to pred(MB_MAX_TCP_CONN) do begin
    if FConnList[i] = nil then begin
      FConnList[i] := conn;
      inc(FConnCount);
      exit;
    end;
  end;
end;

function TMBTCPServer.ClientExists(AClient: TMBClientConnection): boolean;
var
  i	: integer;
begin
  result := false;
  for i := 0 to pred(MB_MAX_TCP_CONN) do begin
    if FConnList[i] = AClient then begin
      result := true;
      break;
    end;
  end;
end;

procedure TMBTCPServer.ProcessConnections;
var
  i	: integer;
  conn	: TMBTCPClientConnection;
  q	: TMBQuery;
begin
  for i := 0 to pred(MB_MAX_TCP_CONN) do begin
    conn := FConnList[i];
    if conn = nil then
      continue;
    q := conn.Receive;
    if q <> nil then
      RxQueue.Push(q);
    if not conn.Dead then
      continue;
    FConnList[i] := nil;
    conn.Free;
    dec(FConnCount);
  end;

  while true do begin
    q := TxQueue.Pop;
    if q = nil then
      break;
    if ClientExists(q.Client) then
      q.Client.Transmit(q)
    else
      q.Free;
  end;
end;

procedure TMBTCPServer.Execute;
var
  newsock	: TSocket;
begin
  FConnCount := 0;
  FillChar(FConnList, sizeof(FConnList), 0);
  try
//    Initialise;
    while not MustDie do begin
      try
        ProcessConnections;
        // Check for incoming connections
        if FListenSock.CanRead(1) then begin
          if FConnCount >= MB_MAX_TCP_CONN then
            continue;
          newsock := FListenSock.Accept;
          NewConnection(newsock);
        end;
      finally
      end;
    end;
  finally
    Terminate;
//    Finalise;
  end;
end;

{ TMBTCPClientConnection }

constructor TMBTCPClientConnection.Create(AOwner: TMBTCPServer; ASocket: TSocket);
begin
  inherited Create;
  FOwner := AOwner;
  FSock := TTCPBlockSocket.Create;
  FSock.Socket := ASocket;
  FRemoteHost := FSock.GetRemoteSinIP;
  FRemotePort := IntToStr(FSock.GetRemoteSinPort);
  FRxLen := 0;
  FPDULen := 0;
  FDead := false;
  FRxLeft := Length(FRxBuffer);
  blogger.log.Add(bllNotice, '%s: Connection from: %s:%s', [mbu_tcpsrvnm, FRemoteHost, FRemotePort]);
end;

destructor TMBTCPClientConnection.Destroy;
begin
  FreeAndNil(FSock);
  inherited Destroy;
end;

function TMBTCPClientConnection.Receive: TMBQuery;
var
  nread	: integer;
  q	: TMBQuery;
begin
  result := nil;
  if not FSock.CanRead(0) then
    exit;

  nread := FSock.RecvBufferEx(@FRxBuffer[FRxLen], FRxLeft, 1);
  if nread = 0 then begin
    blogger.log.Add(bllNotice, '%s: Lost connection to: %s:%s', [mbu_tcpsrvnm, FRemoteHost, FRemotePort]);
    FDead := true;
    exit;
  end;
  inc(FRxLen, nread);
  dec(FRxLeft, nread);
  if FRxLen < 7 then
    exit;
  if (FPDULen = 0) then begin
    FPDULen := mbu_GetU16BE(FRxBuffer[4]);
    FRxLeft := FPDULen - FRxLen + 6;
  end;
  if (FRxLen < FPDULen + 6) then
    exit;

  if FOwner.DumpPackets then
    blogger.log.Add(bllNotice, '%s: Rx from %s:%s: %s', [mbu_tcpsrvnm, FRemoteHost, FRemotePort,
      mbu_DumpPacket(FRxBuffer[0], FRxLen)]);

  q := TMBQuery.Create(FRxBuffer[6]);
  q.QId := mbu_GetU16BE(FRxBuffer[0]);
  q.Rq.SetPDU(FRxBuffer[7], FPDULen - 1);
  q.Client := self;
  result := q;
  FRxLen := 0;
  FPDULen := 0;
  FRxLeft := Length(FRxBuffer);
end;

procedure TMBTCPClientConnection.Transmit(AQuery: TMBQuery);
begin
  FillChar(FTxBuffer, Length(FTxBuffer), 0);
  mbu_PutU16BE(FTxBuffer[0], AQuery.QId);
  mbu_PutU16BE(FTxBuffer[4], AQuery.rp.Len + 1);
  FTxBuffer[6] := AQuery.SlaveAddr;
  AQuery.rp.GetPDU(FTxBuffer[7]);
  FSock.SendBuffer(@FTxBuffer[0], AQuery.rp.Len + 7);

  if FOwner.DumpPackets then
    blogger.log.Add(bllNotice, '%s: Tx to   %s:%s: %s', [mbu_tcpsrvnm, FRemoteHost, FRemotePort,
      mbu_DumpPacket(FTxBuffer[0], AQuery.rp.Len + 7)]);

  AQuery.Free;
end;


end.

