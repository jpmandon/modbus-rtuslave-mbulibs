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
{:
  @abstract(Common module for an entire Modbus library.)

  This unit contains definitions of constants, base/abstract classes and support functions
  used in mbutils.
}
unit
  mbubase;

interface

uses
  SysUtils, Classes, syncobjs, bqueue, blogger;

const
  MBU_VERSION_HI = 1;
  MBU_VERSION_LO = 2;
  MBU_VERSION_ST = 'beta';

  MB_ADDRESS_BROADCAST = 0;	// Modbus broadcast address
  MB_ADDRESS_MIN = 1;		// Smallest possible slave address
  MB_ADDRESS_MAX = 247;		// Biggest possible slave address

  MB_FUNC_NONE = 0;
  MB_FUNC_READ_COILS = 1;
  MB_FUNC_READ_DISCRETE_INPUTS = 2;
  MB_FUNC_READ_HOLDING_REGISTER = 3;
  MB_FUNC_READ_INPUT_REGISTER = 4;
  MB_FUNC_WRITE_SINGLE_COIL = 5;
  MB_FUNC_WRITE_REGISTER = 6;
  MB_FUNC_DIAG_READ_EXCEPTION = 7;
  MB_FUNC_DIAG_DIAGNOSTIC = 8;
  MB_FUNC_DIAG_GET_COM_EVENT_CNT = 11;
  MB_FUNC_DIAG_GET_COM_EVENT_LOG = 12;
  MB_FUNC_WRITE_MULTIPLE_COILS = 15;
  MB_FUNC_WRITE_MULTIPLE_REGISTERS = 16;
  MB_FUNC_OTHER_REPORT_SLAVEID = 17;
  MB_FUNC_READWRITE_MULTIPLE_REGISTERS = 23;

  MB_PDU_LEN_MAX = 253;			// maximum length of the protocol data unit
  MB_RTU_PKT_LEN_MAX = 256;		// maximum packet length in the RTU mode
  MB_ASCII_PKT_LEN_MAX = 513;
  MB_TCP_MBAP_LEN = 7;
  MB_TCP_PKT_LEN_MAX = MB_PDU_LEN_MAX + MB_TCP_MBAP_LEN;

type
  {: Any exception raised by mbulib should be of type EMBError. }
  EMBError = class(Exception);

  {: Query error codes. }
  TMBErrors = (
    mbeNoError,
    mbeIllegalFunction,
    mbeInvalidDataAddress,
    mbeInvalidDataValue,
    mbeSlaveDeviceFailure,
    mbeAcknowledge,
    mbeSlaveBusy,
    mbeTimeout,
    mbeMemoryParityError,
    mbeDeviceException,
    mbeCommFailed,
    mbeTxFailed,
    mbeNotConnected,
    mbeCRCMismatch);

  { Forward declarations }
  TMBQuery = class;
  TMBClientConnection = class;
  TMBRoute = class;
  TMBServer = class;
  TMBQueryEvent = procedure(AQuery: TMBQuery) of object;

  TMBPDUData = array[0 .. MB_PDU_LEN_MAX - 1] of byte;

  { TMBPDU }

  {: @abstract(PDU data of the Modbus protocol.)

     This type represents a buffer which contains only protocol data unit
     part of the entire Modbus message. It is up to the transport level
     to wrap or extract data from the data stream.

     It is possible to specialize this class for the particular request/reply. }
  TMBPDU = class
  private
    FLen	: word;			// Total length of the valid part of PDU
    FPDU	: TMBPDUData;

    procedure CheckPDUIndex(AIndex: cardinal);
    function Getu16le(AOffset: word): word;
    function Getu32le(AOffset: word): longword;
    function Getu8(AOffset: word): byte;
    function Getu16be(AOffset: word): word;
    function Getu32be(AOffset: word): longword;
    function Gets8(AOffset: word): shortint;
    function Gets16be(AOffset: word): smallint;
    function Gets32be(AOffset: word): longint;
    function GetFnCode: byte;
    procedure Setu16le(AOffset: word; AValue: word);
    procedure Setu32le(AOffset: word; AValue: longword);
    procedure Setu8(AOffset: word; const AValue: byte);
    procedure Setu16be(AOffset: word; const AValue: word);
    procedure Setu32be(AOffset: word; const AValue: longword);
    procedure Sets8(AOffset: word; const AValue: shortint);
    procedure Sets16be(AOffset: word; const AValue: smallint);
    procedure Sets32be(AOffset: word; const AValue: longint);
    procedure SetFnCode(const AValue: byte);
  protected
    procedure CheckField(AField: cardinal);
  public
    constructor Create;
    function ParseReply:TMBErrors;
    {: Copy an entire PDU content to destination. }
    procedure GetPDU(var ADest);
    {: Set an entire PDU content from source. }
    procedure SetPDU(const ASrc; ALen: word);
    function GetAsciizString(AOffset: word; AMaxLen: integer): string;

    {: Function code, the first byte in the PDU. }
    property FnCode: byte read GetFnCode write SetFnCode;
    {: Length of the PDU. }
    property Len: word read FLen write FLen;
    {: Get or set 8-bit unsigned value started at the given offset. }
    property u8[AOffset: word]: byte read Getu8 write Setu8;
    {: Get or set big-endian 32-bit unsigned value started at the given offset. }
    property u32be[AOffset: word]: longword read Getu32be write Setu32be;
    {: Get or set little-endian 32-bit unsigned value started at the given offset. }
    property u32le[AOffset: word]: longword read Getu32le write Setu32le;
    {: Get or set big-endian 16-bit unsigned value started at the given offset. }
    property u16be[AOffset: word]: word read Getu16be write Setu16be;
    {: Get or set little-endian 16-bit unsigned value started at the given offset. }
    property u16le[AOffset: word]: word read Getu16le write Setu16le;
    {: Get or set 8-bit signed value started at the given offset. }
    property s8[AOffset: word]: shortint read Gets8 write Sets8;
    {: Get or set big-endian 16-bit signed value started at the given offset. }
    property s16be[AOffset: word]: smallint read Gets16be write Sets16be;
    {: Get or set big-endian 32-bit signed value started at the given offset. }
    property s32be[AOffset: word]: longint read Gets32be write Sets32be;
  end;

  TMBPDUClass = class of TMBPDU;

  { TMBQuery }

  {: @abstract(This class represents any request/reply pair processed by mbulib.)

    Class provides complete functionality for all (even unknown) types of
    queries. If more elegant acesss to the PDU data required on the client side,
    then user are free to derive wrappers from this class (see @link(mbustdq) for
    example). }
  TMBQuery = class(TQueueNode)
  private
    FTag	: longint;
    FStartTime	: longword;
    FError	: TMBErrors;
    FClient	: TMBClientConnection;
    FRoute	: TMBRoute;
    FSlaveAddr	: byte;
    FCompleted	: boolean;
  protected
    FRq		: TMBPDU;
    FRp		: TMBPDU;
    FQId	: word;			// Unique query ID
    FOnRxDone	: TMBQueryEvent;

  public
    constructor Create(ASlaveAddr: byte);virtual;
    destructor Destroy;override;
    procedure Cleanup;
    {: Allow query to tie any lose ends before request will be sent. Not a user level method. }
    procedure BeforeSend;virtual;
    {: Provide PDU class for request. }
    function GetRqClass:TMBPDUClass;virtual;
    {: Provide PDU class for reply. }
    function GetRpClass:TMBPDUClass;virtual;
    {: Indicate requets/reply round completion. }
    procedure Complete(AStatus: TMBErrors);
    {: Block until request will be processed. }
    function WaitForCompletion: TMBErrors;
    {: Unique short name of the request (like 'rdhold' or 'rdinput'). }
    class function ShortName:string;virtual;abstract;
    {: Unique Long name of the request (like 'Read Holding Registers'). }
    class function LongName:string;virtual;abstract;

    {: Address of slave involved in the request. }
    property SlaveAddr: byte read FSlaveAddr write FSlaveAddr;
    {: User tag, not used by mbulib. }
    property Tag: longint read FTag write FTag;
    {: Unique query id. }
    property QId: word read FQId write FQId;
    {: True, if query processing complete. User should inspect an Error field to get
      completion status. }
    property Completed: boolean read FCompleted write FCompleted;
    {: Contains error code if query failed, or mbeNoError if no erros occured. }
    property Error: TMBErrors read FError write FError;
    property StartTime: longword read FStartTime write FStartTime;
    {: Access to the request PDU. }
    property rq: TMBPDU read FRq;
    {: Access to the reply PDU. }
    property rp: TMBPDU read FRp;
    {: Inidcates client connection. Valid only for servers. }
    property Client: TMBClientConnection read FClient write FClient;
    {: User specific field. Not used by core library. }
    property Route: TMBRoute read FRoute write FRoute;
    {: User may specify an event. Not used bye core library. }
    property OnRxDone: TMBQueryEvent read FOnRxDone write FOnRxDone;
  end;

  TMBQueryClass = class of TMBQuery;

  { TMBQueryFIFO }

  {: @abstract(This class implements specialization of lockless FIFO queue for TMBQuery type.) }
  TMBQueryFIFO = class(TBQueueObj)
  public
    destructor Destroy;override;
    {: Add a query to queue. This call will block if queue size exceeded. }
    procedure Push(const ANode: TMBQuery);
    {: Extract an object from queue. The NIL will be returned if queue is empty. }
    function Pop: TMBQuery;
  end;

  { TMBClientConnection }

  { @abstract(Abstract class to represent details of the client connected to server.) }
  TMBClientConnection = class
  protected
    FOwner	: TMBServer;
  public
    procedure Transmit(AQuery: TMBQuery);virtual;abstract;

    {: Parent server IO thread. }
    property Owner: TMBServer read FOwner;
  end;

  { TMBIOThread }

  {: @abstract(Parent class for all client and server implementations.) }
  TMBIOThread = class(TThread)
  private
    FMustDie		: boolean;
    FDumpPackets	: boolean;		// dump packet contents log file
  protected
    {: Notify channel when IO thread have data for application. }
    FSignalEvent	: TEventObject;
    {: Notify channel application have data for IO thread. }
    FSubmitEvent	: TEventObject;
    {: True when thread successfully connected to the underlying media. }
    FConnected		: boolean;
    FError              : TMBErrors;

    procedure Initialize;virtual;
    procedure Finalise;virtual;
  public
    constructor Create;
    destructor Destroy;override;
    {: Connect to underlying media. }
    function Connect:boolean;virtual;abstract;
    {: Disconnect from the underlying media. }
    procedure Disconnect;virtual;abstract;

    {: True when client successfully connected to the underlying media. }
    property Connected: boolean read FConnected;
    {: Thread should terminate as soon as possible. }
    property MustDie: boolean read FMustDie write FMustDie;
    property Error: TMBErrors read FError;
    {: If true, then dump contents of all received and trasmitted packets. }
    property DumpPackets: boolean read FDumpPackets write FDumpPackets;
  end;

  { TMBClient }

  {: @abstract(Base class for all Client/Master implementations.
    This class provides a set of methods and properties which is common to all
    clients. }
  TMBClient = class(TMBIOThread)
  private
    FQuerySeq		: word;		// Unique query number
  protected
    FTxQueue		: TMBQueryFIFO;	// Requests
    FRxQueue		: TMBQueryFIFO;	// Replies

    {: Generates next query id. }
    function NextSeq:word;
    property RxQueue: TMBQueryFIFO read FRxQueue;
  public
    constructor Create;
    destructor Destroy;override;
    {: Place query in the trasmit queue and initiate its processing.
      An exception will be raised if any of preconditions aren't met. }
    procedure SubmitQuery(AQuery: TMBQuery);virtual;
    {: Return a pointer to the completely processed query and nil if there
      isn't any. Application should check query fields for any possible errors. }
    function GetCompletedQuery: TMBQuery;virtual;

  end;

  { TMBClientList }

  {: @abstract(General purpose class to construct a list of clients.) }
  TMBClientList = class(TStringList)
  private
    function GetClients(AIndex: integer): TMBClient;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Add(const AName: string; AClient: TMBClient);reintroduce;
    function LookupByName(const AName: string): TMBClient;

    property Clients[AIndex: integer]: TMBClient read GetClients; default;
  end;

  { TMBServer }

  {: @abstract(Base class to construct Servers/Slaves).
    This class provides a set of methods and properties which is common to
    all servers. }
  TMBServer = class(TMBIOThread)
  private
    FRxQueue		: TMBQueryFIFO;		// received queries
    FTxQueue		: TMBQueryFIFO;		// response queries
  protected
    property RxQueue: TMBQueryFIFO read FRxQueue;
    property TxQueue: TMBQueryFIFO read FTxQueue;
  public
    constructor Create;
    destructor Destroy;override;
    function GetReceivedQuery: TMBQuery;virtual;
    procedure SubmitReply(AQuery: TMBQuery);virtual;
  end;

  { TMBServerList }

  {: @abstract(General purpose class to construct a list of servers.) }
  TMBServerList = class(TStringList)
  private
    function GetServers(AIndex: integer): TMBServer;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Add(const AName: string; AServer: TMBServer);reintroduce;
    function LookupByName(const AName: string): TMBServer;

    property Servers[AIndex: integer]: TMBServer read GetServers; default;
  end;

  { TMBRoute }

  {: @abstract(Genral purpose class to record query routing history.) }
  TMBRoute = class
  private
    FServer	: TMBServer;
    FClient	: TMBClient;
    FFirst	: byte;
    FLast	: byte;
  public
    constructor Create(AServer: TMBServer; AClient: TMBClient; AFirst: integer = 0; ALast: integer = 255);

    property Client: TMBClient read FClient;
    property Server: TMBServer read FServer;
    property First: byte read FFirst;
    property Last: byte read FLast;
  end;

{: Return library version as string. }
function mbu_Version:string;
{: Calculate RTU CRC. }
function mbu_CRC(var ABuf; ALen: word):word;
{: Convert an error code to the human readable representation. }
function mbu_ErrorToStr(AError: TMBErrors):string;
{: Dump contents of ABuf as hexadecimal string. }
function mbu_DumpPacket(var ABuf; ALen: word):string;

{: Extract a big-endian unsigned 16 bit integer from the given buffer. }
function mbu_GetU16BE(var ABuf):word;
{: Put a big-endian unsigned 16 bit integer to the given buffer. }
procedure mbu_PutU16BE(var ABuf; AWord: word);

implementation

function mbu_Version:string;
begin
  result := Format('%d.%d%s', [MBU_VERSION_HI, MBU_VERSION_LO, MBU_VERSION_ST]);
end;

{$region 'crc table'}

const
  crctabhi: array[0..255] of byte = (
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40
  );

  crctablo: array[0..255] of byte = (
    $00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05, $C5, $C4, $04,
    $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA, $CB, $0B, $C9, $09, $08, $C8,
    $D8, $18, $19, $D9, $1B, $DB, $DA, $1A, $1E, $DE, $DF, $1F, $DD, $1D, $1C, $DC,
    $14, $D4, $D5, $15, $D7, $17, $16, $D6, $D2, $12, $13, $D3, $11, $D1, $D0, $10,
    $F0, $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7, $37, $F5, $35, $34, $F4,
    $3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A, $3B, $FB, $39, $F9, $F8, $38,
    $28, $E8, $E9, $29, $EB, $2B, $2A, $EA, $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C,
    $E4, $24, $25, $E5, $27, $E7, $E6, $26, $22, $E2, $E3, $23, $E1, $21, $20, $E0,
    $A0, $60, $61, $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67, $A5, $65, $64, $A4,
    $6C, $AC, $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB, $69, $A9, $A8, $68,
    $78, $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE, $7E, $7F, $BF, $7D, $BD, $BC, $7C,
    $B4, $74, $75, $B5, $77, $B7, $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0,
    $50, $90, $91, $51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95, $94, $54,
    $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99, $59, $58, $98,
    $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E, $8F, $4F, $8D, $4D, $4C, $8C,
    $44, $84, $85, $45, $87, $47, $46, $86, $82, $42, $43, $83, $41, $81, $80, $40
  );
{$endregion}

function mbu_CRC(var ABuf; ALen: word):word;
var
  crclo, crchi, idx	: byte;
  i 	: integer;
  pb	: pbyte;
begin
  crclo := $ff;
  crchi := $ff;
  pb := pbyte(@ABuf);
  for i := 0 to pred(ALen) do begin
    idx := crclo xor pb^;
    crclo := crchi xor crctabhi[idx];
    crchi := crctablo[idx];
    inc(pb);
  end;
  result := (crchi shl 8) or crclo;
end;

function mb_hex_nibble(abyte: byte): char;
begin
  if abyte >= 10 then
    abyte := abyte - 10 + $41
  else
    abyte := $30 + abyte;
  result := chr(abyte);
end;

procedure mb_hex_char(apc: pchar; abyte: byte);
begin
  apc[0] := mb_hex_nibble(abyte shr 4);
  apc[1] := mb_hex_nibble(abyte and $f);
end;

function mbu_DumpPacket(var ABuf; ALen: word):string;
var
  i : integer;
  pb	: pbyte;
  pc	: pchar;
begin
  SetLength(result, ALen * 3 - 1);
  pc := pchar(result);
  pb := pbyte(@ABuf);
  for i := 1 to ALen do begin
    mb_hex_char(pc, pb^);
    if i <> ALen then
      pc[2] := ' ';
    inc(pc, 3);
    inc(pb, 1);
  end;
end;

function mbu_ErrorToStr(AError: TMBErrors):string;
begin
  case AError of
    mbeNoError:	result := 'none';
    mbeIllegalFunction: result := 'Illegal function number';
    mbeInvalidDataAddress: result := 'Invalid data address';
    mbeInvalidDataValue: result := 'Invalid data value';
    mbeSlaveDeviceFailure: result := 'Slave device failure';
    mbeSlaveBusy: result := 'Slave are busy';
    mbeTimeout: result := 'Request timeout';
    mbeMemoryParityError: result := 'Memory parity error';
    mbeDeviceException: result := 'Device exception';
    mbeCommFailed: result := 'Communications failed';
    mbeTxFailed: result := 'Transmit failed';
    mbeNotConnected: result := 'Transport not connected';
    mbeCRCMismatch: result := 'CRC mismatch';
  else
    result := 'Unknown error'
  end;
end;

function mb_Bytes2Bits(ABytes: byte): word;
begin
  result := Round(ABytes / 8.0 + 0.49)
end;

procedure mbu_PutU16BE(var ABuf; AWord: word);
begin
  pchar(@ABuf)[0] := char(Hi(AWord));
  pchar(@ABuf)[1] := char(Lo(AWord));
end;

function mbu_GetU16BE(var ABuf):word;
begin
  result := byte(pchar(@ABuf)[0]) shl 8 + byte(pchar(@ABuf)[1]);
end;

{ TMBPDU }

constructor TMBPDU.Create;
begin
  inherited;
  SetFnCode(MB_FUNC_NONE);
  FillChar(FPDU, sizeof(FPDU), 0);
end;

procedure TMBPDU.CheckPDUIndex(AIndex: cardinal);
begin
  if AIndex >= FLen then
    raise Exception.CreateFmt('Index in the PDU data is out of bounds (%u >= %u).',
      [AIndex, Flen]);
end;

procedure TMBPDU.SetFnCode(const AValue: byte);
begin
  FPDU[0] := AValue;
end;

procedure TMBPDU.CheckField(AField: cardinal);
begin
  if AField = 0 then
    raise Exception.CreateFmt('Field is inaccessible', [0]);
end;

function TMBPDU.GetAsciizString(AOffset: word; AMaxLen: integer): string;
var
 len1	: integer;
begin
  len1 := 0;
  while (FPDU[AOffset + len1] <> 0) and (len1 < AMaxLen) do
    inc(Len1);
  SetLength(result, Len1);
  while len1 <> 0 do begin
    result[len1] := char(FPDU[AOffset + len1]);
    dec(Len1);
  end;
end;

function TMBPDU.Getu8(AOffset: word): byte;
begin
  CheckPDUIndex(AOffset);
  result := FPDU[AOffset];
end;

function TMBPDU.GetFnCode: byte;
begin
  result := FPDU[0];
end;

procedure TMBPDU.Setu8(AOffset: word; const AValue: byte);
begin
  CheckPDUIndex(AOffset);
  FPDU[AOffset] := AValue;
end;

function TMBPDU.Getu16be(AOffset: word): word;
begin
  CheckPDUIndex(AOffset + 1);
  result := FPDU[AOffset] shl 8 + FPDU[AOffset + 1];
end;

procedure TMBPDU.Setu16be(AOffset: word; const AValue: word);
begin
  CheckPDUIndex(AOffset + 1);
  FPDU[AOffset] := Hi(AValue);
  FPDU[AOffset + 1] := Lo(AValue);
end;

function TMBPDU.Getu16le(AOffset: word): word;
begin
  CheckPDUIndex(AOffset + 1);
  result := FPDU[AOffset] + FPDU[AOffset + 1] shl 8;
end;

procedure TMBPDU.Setu16le(AOffset: word; AValue: word);
begin
  CheckPDUIndex(AOffset + 1);
  FPDU[AOffset] := Lo(AValue);
  FPDU[AOffset + 1] := Hi(AValue);
end;

function TMBPDU.Getu32be(AOffset: word): longword;
begin
  CheckPDUIndex(AOffset + 3);
  result := FPDU[AOffset] shl 24 + FPDU[AOffset + 1] shl 16 + FPDU[AOffset + 2] shl 8 + FPDU[AOffset + 3];
end;

function TMBPDU.Getu32le(AOffset: word): longword;
begin
  CheckPDUIndex(AOffset + 3);
  result := FPDU[AOffset + 3] shl 24 + FPDU[AOffset + 2] shl 16 + FPDU[AOffset + 1] shl 8 + FPDU[AOffset];
end;

procedure TMBPDU.Setu32be(AOffset: word; const AValue: longword);
begin
  CheckPDUIndex(AOffset + 3);
  FPDU[AOffset] := byte(AValue shr 24);
  FPDU[AOffset + 1] := byte(AValue shr 16);
  FPDU[AOffset + 2] := byte(AValue shr 8);
  FPDU[AOffset + 3] := byte(AValue);
end;

procedure TMBPDU.Setu32le(AOffset: word; AValue: longword);
begin
  CheckPDUIndex(AOffset + 3);
  FPDU[AOffset] := byte(AValue);
  FPDU[AOffset + 1] := byte(AValue shr 8);
  FPDU[AOffset + 2] := byte(AValue shr 16);
  FPDU[AOffset + 3] := byte(AValue shr 24);
end;

function TMBPDU.Gets8(AOffset: word): shortint;
begin
  CheckPDUIndex(AOffset);
  result := shortint(FPDU[AOffset]);
end;

procedure TMBPDU.Sets8(AOffset: word; const AValue: shortint);
begin
  CheckPDUIndex(AOffset);
  FPDU[AOffset] := byte(AValue);
end;

function TMBPDU.Gets16be(AOffset: word): smallint;
begin
  CheckPDUIndex(AOffset + 1);
  result := smallint(FPDU[AOffset] shl 8 + FPDU[AOffset + 1]);
end;

procedure TMBPDU.Sets16be(AOffset: word; const AValue: smallint);
begin
  CheckPDUIndex(AOffset + 1);
  FPDU[AOffset] := Hi(AValue);
  FPDU[AOffset + 1] := Lo(AValue);
end;

function TMBPDU.Gets32be(AOffset: word): longint;
begin
  CheckPDUIndex(AOffset + 3);
  result := longint(FPDU[AOffset] shl 24 + FPDU[AOffset + 1] shl 16 +
    FPDU[AOffset + 2] shl 8 + FPDU[AOffset + 3]);
end;

procedure TMBPDU.Sets32be(AOffset: word; const AValue: longint);
begin
  CheckPDUIndex(AOffset + 3);
  FPDU[AOffset] := byte(AValue shr 24);
  FPDU[AOffset + 1] := byte(AValue shr 16);
  FPDU[AOffset + 2] := byte(AValue shr 8);
  FPDU[AOffset + 3] := byte(AValue);
end;

procedure TMBPDU.GetPDU(var ADest);
begin
  Move(FPDU, ADest, FLen);
end;

procedure TMBPDU.SetPDU(const ASrc; ALen: word);
begin
  if ALen > MB_PDU_LEN_MAX then
    raise EMBError.CreateFmt('SetPDU: data too long: %d', [ALen]);
  Move(ASrc, FPDU, ALen);
  FLen := ALen;
end;

function TMBPDU.ParseReply:TMBErrors;
begin
  if (FnCode and $80) <> 0 then begin // An Exception occured
    case FPDU[1] of
      1: result := mbeIllegalFunction;
      2: result := mbeInvalidDataAddress;
      3: result := mbeInvaliddataValue;
      4: result := mbeSlaveDeviceFailure;
      5: result := mbeAcknowledge;
      6: result := mbeSlaveBusy;
      8: result := mbeMemoryParityError;
      else
         result := mbeDeviceException;
    end;
    exit;
  end;

  result := mbeNoError;
end;

{ TMBQuery }

constructor TMBQuery.Create(ASlaveAddr: byte);
begin
  FSlaveAddr := ASlaveAddr;
  FRq := GetRqClass.Create;
  FRp := GetRpClass.Create;
  FOnRxDone := nil;
  Cleanup;
end;

destructor TMBQuery.Destroy;
begin
  FreeAndNil(FRq);
  FreeAndNil(FRp);
  inherited Destroy;
end;

procedure TMBQuery.Cleanup;
begin
  FQId := 0;
  FError := mbeNoError;
  FCompleted := false;
end;

// Perform last minute fixes before sending query to slave
procedure TMBQuery.BeforeSend;
begin
end;

function TMBQuery.GetRqClass: TMBPDUClass;
begin
  result := TMBPDU;
end;

function TMBQuery.GetRpClass: TMBPDUClass;
begin
  result := TMBPDU;
end;

function TMBQuery.WaitForCompletion: TMBErrors;
begin
  while not Completed do
    CheckSynchronize(1);
  result := Error;
end;

procedure TMBQuery.Complete(AStatus: TMBErrors);
begin
  Completed := true;
  FError := AStatus;
end;

{ TMBIOThread }

procedure TMBIOThread.Initialize;
begin
  FSignalEvent := TEventObject.Create(NIL, false, false, 'MBIO_SUBMIT');
end;

procedure TMBIOThread.Finalise;
begin
  FreeAndNil(FSignalEvent);
end;

constructor TMBIOThread.Create;
begin
  inherited Create(true);	// create suspended
  FSubmitEvent := TEventObject.Create(NIL, false, false, 'MBIO_SUBMIT');
end;

destructor TMBIOThread.Destroy;
begin
  FreeandNil(FSubmitEvent);
  inherited Destroy;
end;

{ TMBClient }

constructor TMBClient.Create;
begin
  inherited Create;
  FTxQueue := TMBQueryFIFO.Create;
  FRxQueue := TMBQueryFIFO.Create;
  FError := mbeNoError;
end;

destructor TMBClient.Destroy;
begin
  FreeAndNil(FTxQueue);
  FreeAndNil(FRxQueue);
  inherited Destroy;
end;

function TMBClient.NextSeq: word;
begin
  inc(FQuerySeq);
  result := FQuerySeq;
end;

procedure TMBClient.SubmitQuery(AQuery: TMBQuery);
begin
  FError := mbeNotConnected;

  if not Connected then
    raise EMBError.Create('Not connected');

  AQuery.BeforeSend;
  if AQuery.Rq.Len = 0 then
    raise EMBError.Create('Request length not set');

  AQuery.Completed := false;
  FTxQueue.Push(AQuery);
  FSubmitEvent.SetEvent;
end;

function TMBClient.GetCompletedQuery: TMBQuery;
begin
  result := RxQueue.Pop;
end;

{ TMBQueryFIFO }

destructor TMBQueryFIFO.Destroy;
var
  q	: TMBQuery;
begin
  while true do begin
    q := Pop;
    if q = nil then
      break;
    q.Free
  end;
  inherited Destroy;
end;

procedure TMBQueryFIFO.Push(const ANode: TMBQuery);
begin
  inherited Push(TQueueNode(ANode));
end;

function TMBQueryFIFO.Pop: TMBQuery;
begin
  result := TMBQuery(inherited Pop);
end;

{ TMBClientList }

constructor TMBClientList.Create;
begin
  inherited Create;
  Sorted := true;
  Duplicates := dupError;
end;

destructor TMBClientList.Destroy;
var
  i	: integer;
begin
  for i := 0 to pred(Count) do
    Objects[i].Free;
  inherited Destroy;
end;

function TMBClientList.GetClients(AIndex: integer): TMBClient;
begin
  result := TMBClient(Objects[AIndex]);
end;

procedure TMBClientList.Add(const AName: string; AClient: TMBClient);
begin
  inherited AddObject(AName, TObject(AClient));
end;

function TMBClientList.LookupByName(const AName: string): TMBClient;
var
  i	: integer;
begin
  i := IndexOf(AName);
  if i >=0 then
    result := Clients[i]
  else
    result := nil;
end;

{ TMBServerList }

constructor TMBServerList.Create;
begin
  inherited Create;
  Sorted := true;
  Duplicates := dupError;
end;

destructor TMBServerList.Destroy;
var
  i	: integer;
begin
  for i := 0 to pred(Count) do
    Objects[i].Free;
  inherited Destroy;
end;

function TMBServerList.GetServers(AIndex: integer): TMBServer;
begin
  result := TMBServer(Objects[AIndex]);
end;

procedure TMBServerList.Add(const AName: string; AServer: TMBServer);
begin
  inherited AddObject(AName, TObject(AServer));
end;

function TMBServerList.LookupByName(const AName: string): TMBServer;
var
  i	: integer;
begin
  i := IndexOf(AName);
  if i >=0 then
    result := Servers[i]
  else
    result := nil;
end;

{ TMBServer }

constructor TMBServer.Create;
begin
  inherited Create;
  FRxQueue := TMBQueryFIFO.Create;
  FTxQueue := TMBQueryFIFO.Create;
end;

destructor TMBServer.Destroy;
begin
  FreeAndNil(FRxQueue);
  FreeAndNil(FTxQueue);
  inherited Destroy;
end;

function TMBServer.GetReceivedQuery: TMBQuery;
begin
  result := RxQueue.Pop
end;

procedure TMBServer.SubmitReply(AQuery: TMBQuery);
begin
  TxQueue.Push(AQuery);
end;

{ TMBRoute }

constructor TMBRoute.Create(AServer: TMBServer; AClient: TMBClient; AFirst: integer; ALast: integer);
begin
  inherited Create;
  FServer := AServer;
  FClient := AClient;
  FFirst := AFirst;
  FLast := ALast;
end;

end.

