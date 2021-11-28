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
  @abstract(Implementation of standard queries)

  This unit implements processing for some of the standard queries.
}
unit mbustdq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mbubase;

type

  { TMBQCustomReadBits }

  {: @abstract(Common class for requests which reads bit-sized data.)

     End users should not instantiate this class directly and use derivative classes instead.
   }
  TMBQCustomReadBits = class(TMBQuery)
  private
    function GetAsBytes(AIndex: cardinal): byte;
    function GetBitCnt: word;
    function GetBits(AIndex: cardinal): boolean;
    function GetByteCount: byte;
    function GetStartBit: word;
    procedure SetBitCnt(const AValue: word);
    procedure SetStartBit(const AValue: word);
  protected
    {: Offset of the 'Starting Address' field in the request PDU. }
    FrqStartBitOfs	: cardinal;
    {: Offset of the 'No. of Points' field in the request PDU. }
    FrqBitCntOfs	: cardinal;
    {: Offset of the 'Byte Count' field in the reply PDU. }
    FrpByteCountOfs	: cardinal;
    {: Offset of the 'Data' field in the reply PDU. }
    FrpBitsOfs		: cardinal;
  public
    constructor Create(ASlaveAddr: byte);override;

    {: First bit number to query, starting from 1. }
    property StartBit: word read GetStartBit write SetStartBit;
    {: Number of bits to retrieve. }
    property BitCnt: word read GetBitCnt write SetBitCnt;
    {: Byte count field extracted from the reply. }
    property ByteCount: byte read GetByteCount;
    {: A zero based array of returned bits. }
    property Bits[AIndex: cardinal]: boolean read GetBits;
    {: A zero based array of returned bits grouped into bytes. }
    property AsBytes[AIndex: cardinal]: byte read GetAsBytes;
  end;

  { TMBQReadCoils }

  {: @abstract(The 'Read Coil Status' (0x01) query.)
     For a description of access methods see @link(TMBQCustomReadBits) class. }
  TMBQReadCoils = class(TMBQCustomReadBits)
  public
    constructor Create(ASlaveAddr: byte);override;
    class function ShortName:string;override;
    class function LongName:string;override;
  end;

  { TMBQReadDiscreteInputs }

  {: @abstract(The 'Read Input Status' (0x02) query.)
     For a description of access methods see @link(TMBQCustomReadBits) class. }
  TMBQReadDiscreteInputs = class(TMBQCustomReadBits)	// 2
  public
    constructor Create(ASlaveAddr: byte);override;
    class function ShortName:string;override;
    class function LongName:string;override;
  end;

  { TMBCustomRegistersPDU }

  TMBCustomRegistersPDU = class(TMBPDU)
  private
    function GetByteCount: byte;
    function GetRegCnt: word;
    function GetRegS32BE(AOffset: word): longword;
    function GetRegS8(AOffset: word): shortint;
    function GetRegU16BE(AOffset: word): word;
    function GetRegU32BE(AOffset: word): longword;
    function GetRegU8(AOffset: word): byte;
    function GetRegS16BE(AOffset: word): smallint;
    function GetStartReg: word;
    procedure SetByteCount(const AValue: byte);
    procedure SetReg32BE(AOffset: word; const AValue: longword);
    procedure SetRegCnt(const AValue: word);
    procedure SetRegS16BE(AOffset: word; const AValue: smallint);
    procedure SetRegS32BE(AOffset: word; const AValue: longword);
    procedure SetRegS8(AOffset: word; const AValue: shortint);
    procedure SetRegU16BE(AOffset: word; const AValue: word);
    procedure SetRegU8(AOffset: word; const AValue: byte);
    procedure SetStartReg(const AValue: word);
    procedure CheckArrayIndex(AIndex: cardinal; AMaxIndex: cardinal);
  protected
    {: Offset of the 'Starting Address' field. }
    FStartRegOfs	: cardinal;
    {: Offset of the 'No. of Points' field. }
    FRegCntOfs		: cardinal;
    {: Offset of the 'Byte Count' field. }
    FByteCountOfs	: cardinal;
    {: Offset of the 'Data' field. }
    FDataOfs		: cardinal;
  public
    {: First register number to query. Registers numbering starts from 1. }
    property StartReg: word read GetStartReg write SetStartReg;
    {: Number of registers to retrieve. }
    property RegCnt: word read GetRegCnt write SetRegCnt;
    {: Byte count field extracted from the reply. }
    property ByteCount: byte read GetByteCount write SetByteCount;
    {: Treat registers as zero based array of 8-bit values. }
    property RegU8[AOffset: word]: byte read GetRegU8 write SetRegU8;
    {: Treat registers as zero based array of big-endian 16-bit values. }
    property RegU16BE[AOffset: word]: word read GetRegU16BE write SetRegU16BE;
    {: Treat registers zero based array of big-endian 32-bit values. }
    property RegU32BE[AOffset: word]: longword read GetRegU32BE write SetReg32BE;
    {: Treat registers as zero based array of signed 8-bit values. }
    property RegS8[AOffset: word]: shortint read GetRegS8 write SetRegS8;
    {: Treat registers as zero based array of signed big-endian 16-bit values. }
    property RegS16BE[AOffset: word]: smallint read GetRegS16BE write SetRegS16BE;
    {: Treat registers zero based array of signed big-endian 32-bit values. }
    property RegS32BE[AOffset: word]: longword read GetRegS32BE write SetRegS32BE;
  end;

  { TMBQCustomReadRegisters }

  {: @abstract(Base class to retrieve a word-sized data.)

     End users should not instantiate this class directly and use derivative classes instead.
  }
  TMBQCustomRWRegisters = class(TMBQuery)
  private
    function GetLocalRp: TMBCustomRegistersPDU;
    function GetLocalRq: TMBCustomRegistersPDU;
  protected
    {: Check index in the array of register and raise an exception if it is out of bounds. }
  public
    constructor Create(ASlaveAddr: byte);override;
    function GetRqClass:TMBPDUClass;override;
    function GetRpClass:TMBPDUClass;override;
    procedure BeforeSend;override;

    property rq: TMBCustomRegistersPDU read GetLocalRq;
    property rp: TMBCustomRegistersPDU read GetLocalRp;
  end;

  { TMBQReadHoldingRegisters }

  {: @abstract(The 'Read Holding Registers' (0x03) query.)
     For a description of access methods see @link(TMBQCustomReadRegisters) class. }
  TMBQReadHoldingRegisters = class(TMBQCustomRWRegisters)
  public
    constructor Create(ASlaveAddr: byte);override;
    class function ShortName:string;override;
    class function LongName:string;override;
  end;

  { TMBQReadInputRegisters }

  {: @abstract(The 'Read Input Registers' (0x04) query.)
     For a description of access methods see @link(TMBQCustomReadRegisters) class. }
  TMBQReadInputRegisters = class(TMBQCustomRWRegisters)
  public
    constructor Create(ASlaveAddr: byte);override;
    class function ShortName:string;override;
    class function LongName:string;override;
  end;

  {: @abstract(The 'Preset Multiple Registers' (0x10) query.)
     For a description of access methods see @link(TMBQCustomReadRegisters) class. }

  { TMBQWriteHoldingRegisters }

  TMBQWriteHoldingRegisters = class(TMBQCustomRWRegisters)
  public
    constructor Create(ASlaveAddr: byte);override;
    class function ShortName:string;override;
    class function LongName:string;override;
    procedure BeforeSend;override;
  end;


implementation

{ TMBQCustomReadBits }

constructor TMBQCustomReadBits.Create(ASlaveAddr: byte);
begin
  inherited;
  SlaveAddr := ASlaveAddr;
  Error := mbeNoError;
  Rq.Len := 5;
  FrqStartBitOfs := 1;
  FrqBitCntOfs := 3;
  Rp.Len := 0;
  FrpByteCountOfs := 1;
  FrpBitsOfs := 2;
end;

function TMBQCustomReadBits.GetAsBytes(AIndex: cardinal): byte;
begin
  result := rp.u8[AIndex + FrpBitsOfs];
end;

function TMBQCustomReadBits.GetBitCnt: word;
begin
  result := rq.u16be[FrqBitCntOfs];
end;

function TMBQCustomReadBits.GetBits(AIndex: cardinal): boolean;
begin
  if (AIndex < 1) or (AIndex > BitCnt) then
    raise Exception.CreateFmt('Bit index out of range (%d).', [AIndex]);

  dec(AIndex);
  result := (rp.u8[AIndex div 8] and (1 shl (AIndex and 7))) <> 0;
end;

function TMBQCustomReadBits.GetByteCount: byte;
begin
  result := rp.u8[FrpByteCountOfs];
end;

function TMBQCustomReadBits.GetStartBit: word;
begin
  result := rq.u16be[FrqStartBitOfs];
end;

procedure TMBQCustomReadBits.SetBitCnt(const AValue: word);
begin
  if (AValue < 1) or (AValue > 1984) then
    raise Exception.CreateFmt('Bit index out of range (%d).', [AValue]);
  rq.u16be[FrqBitCntOfs] := AValue;
end;

procedure TMBQCustomReadBits.SetStartBit(const AValue: word);
begin
  rq.u16be[FrqStartBitOfs] := AValue;
end;

{ TMBQReadCoils }

constructor TMBQReadCoils.Create(ASlaveAddr: byte);
begin
  inherited;
  Rq.FnCode := MB_FUNC_READ_COILS;	// 1
end;

class function TMBQReadCoils.ShortName: string;
begin
  result := 'rdcoils';
end;

class function TMBQReadCoils.LongName: string;
begin
  result := 'Read Coil Status';
end;

{ TMBQReadDiscreteInputs }

constructor TMBQReadDiscreteInputs.Create(ASlaveAddr: byte);
begin
  inherited;
  Rq.FnCode := MB_FUNC_READ_DISCRETE_INPUTS;	// 2
end;

class function TMBQReadDiscreteInputs.ShortName: string;
begin
  result := 'rddisc';
end;

class function TMBQReadDiscreteInputs.LongName: string;
begin
  result := 'Read Input Status';
end;


{ TMBCustomRegistersPDU }
procedure TMBCustomRegistersPDU.CheckArrayIndex(AIndex: cardinal; AMaxIndex: cardinal);
begin
  if AIndex >= AMaxIndex then
    raise EMBError.CreateFmt('Query %s: Register index out of bounds (%u >= %u).',
      [AIndex, AMaxIndex]);
end;

function TMBCustomRegistersPDU.GetByteCount: byte;
begin
  CheckField(FByteCountOfs);
  result := U8[FByteCountOfs];
end;

procedure TMBCustomRegistersPDU.SetByteCount(const AValue: byte);
begin
  CheckField(FByteCountOfs);
  U8[FByteCountOfs] := AValue;
end;

function TMBCustomRegistersPDU.GetRegCnt: word;
begin
  CheckField(FRegCntOfs);
  result := U16BE[FRegCntOfs];
end;

function TMBCustomRegistersPDU.GetRegU8(AOffset: word): byte;
begin
  CheckField(FDataOfs);
  result := U8[FDataOfs + AOffset];
end;

procedure TMBCustomRegistersPDU.SetRegU8(AOffset: word; const AValue: byte);
begin
  CheckField(FDataOfs);
  U8[FDataOfs + AOffset] := AValue;
end;

function TMBCustomRegistersPDU.GetRegU16BE(AOffset: word): word;
begin
  CheckField(FDataOfs);
  result := U16BE[FDataOfs + AOffset * 2];
end;

procedure TMBCustomRegistersPDU.SetRegU16BE(AOffset: word; const AValue: word);
begin
  CheckField(FDataOfs);
  U16BE[FDataOfs + AOffset * 2] := AValue;
end;

function TMBCustomRegistersPDU.GetRegU32BE(AOffset: word): longword;
begin
  CheckField(FDataOfs);
  result := U32BE[FDataOfs + AOffset * 4];
end;

procedure TMBCustomRegistersPDU.SetReg32BE(AOffset: word; const AValue: longword);
begin
  CheckField(FDataOfs);
  U32BE[FDataOfs + AOffset * 4] := AValue;
end;

function TMBCustomRegistersPDU.GetRegS8(AOffset: word): shortint;
begin
  CheckField(FDataOfs);
  result := S8[FDataOfs + AOffset];
end;

procedure TMBCustomRegistersPDU.SetRegS8(AOffset: word; const AValue: shortint);
begin
  CheckField(FDataOfs);
  S8[FDataOfs + AOffset] := AValue;
end;

function TMBCustomRegistersPDU.GetRegS16BE(AOffset: word): smallint;
begin
  CheckField(FDataOfs);
  result := S16BE[FDataOfs + AOffset * 2];
end;

procedure TMBCustomRegistersPDU.SetRegS16BE(AOffset: word; const AValue: smallint);
begin
  CheckField(FDataOfs);
  S16BE[FDataOfs + AOffset * 2] := AValue;
end;

function TMBCustomRegistersPDU.GetRegS32BE(AOffset: word): longword;
begin
  CheckField(FDataOfs);
  result := S32BE[FDataOfs + AOffset * 4];
end;

procedure TMBCustomRegistersPDU.SetRegS32BE(AOffset: word; const AValue: longword);
begin
  CheckField(FDataOfs);
  S32BE[FDataOfs + AOffset * 4] := AValue;
end;

procedure TMBCustomRegistersPDU.SetRegCnt(const AValue: word);
begin
  CheckField(FRegCntOfs);
  if (AValue < 1) or (AValue > 124) then
    raise Exception.CreateFmt('Invalid number of registers (%d).', [AValue]);

  U16BE[FRegCntOfs] := AValue;
end;

function TMBCustomRegistersPDU.GetStartReg: word;
begin
  CheckField(FStartRegOfs);
  result := U16BE[FStartRegOfs] + 1;
end;

procedure TMBCustomRegistersPDU.SetStartReg(const AValue: word);
begin
  CheckField(FStartRegOfs);
  if AValue < 1 then
    raise Exception.CreateFmt('Register address is out of range (%d).', [AValue]);
  U16BE[FStartRegOfs] := AValue - 1;
end;

function TMBQCustomRWRegisters.GetLocalRp: TMBCustomRegistersPDU;
begin
  result := TMBCustomRegistersPDU(frp);
end;

function TMBQCustomRWRegisters.GetLocalRq: TMBCustomRegistersPDU;
begin
  result := TMBCustomRegistersPDU(frq);
end;

{ TMBQCustomRWRegisters }
constructor TMBQCustomRWRegisters.Create(ASlaveAddr: byte);
begin
  inherited;
  SlaveAddr := ASlaveAddr;
  Error := mbeNoError;
  rq.Len := 5;
  rq.FStartRegOfs := 1;
  rq.FRegCntOfs := 3;
  rq.FByteCountOfs := 0;
  rq.FDataOfs := 0;
  rp.Len := 0;
  rp.FByteCountOfs := 1;
  rp.FDataOfs := 2;
end;

procedure TMBQCustomRWRegisters.BeforeSend;
begin
  inherited BeforeSend;
  rp.Len := 2 + rq.RegCnt * 2;
end;

function TMBQCustomRWRegisters.GetRqClass: TMBPDUClass;
begin
  result := TMBCustomRegistersPDU;
end;

function TMBQCustomRWRegisters.GetRpClass: TMBPDUClass;
begin
  result := TMBCustomRegistersPDU;
end;

{ TMBQReadHoldingRegisters }

constructor TMBQReadHoldingRegisters.Create(ASlaveAddr: byte);
begin
  inherited;
  rq.FnCode := MB_FUNC_READ_HOLDING_REGISTER;	// 3
end;

class function TMBQReadHoldingRegisters.ShortName: string;
begin
  result := 'rdhold';
end;

class function TMBQReadHoldingRegisters.LongName: string;
begin
  result := 'Read Holding Registers';
end;

{ TMBQReadInputRegisters }

constructor TMBQReadInputRegisters.Create(ASlaveAddr: byte);
begin
  inherited;
  rq.FnCode := MB_FUNC_READ_INPUT_REGISTER;	// 4
end;

class function TMBQReadInputRegisters.ShortName: string;
begin
  result := 'rdinput';
end;

class function TMBQReadInputRegisters.LongName: string;
begin
  Result := 'Read Input Registers';
end;

{ TMBQWriteHoldingRegisters }

constructor TMBQWriteHoldingRegisters.Create(ASlaveAddr: byte);
begin
  inherited Create(ASlaveAddr);
  rq.FnCode := MB_FUNC_WRITE_MULTIPLE_REGISTERS;	// 0x10
  rq.Len := MB_PDU_LEN_MAX;	// we'll update it in BeforeSend
  rq.FByteCountOfs := 5;
  rq.FDataOfs := 6;
  rp.FStartRegOfs := 1;
  rp.FRegCntOfs := 3;
  rp.FByteCountOfs := 0;
  rp.FDataOfs := 0;
end;

class function TMBQWriteHoldingRegisters.ShortName: string;
begin
  result := 'wrhold';
end;

class function TMBQWriteHoldingRegisters.LongName: string;
begin
  result := 'Write Holding Registers';
end;

procedure TMBQWriteHoldingRegisters.BeforeSend;
begin
  inherited BeforeSend;
  rq.ByteCount := rq.RegCnt * 2;
  rq.Len := 6 + rq.ByteCount;
  rp.Len := 5;
end;

end.

