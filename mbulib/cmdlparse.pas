{$REGION 'license'}


{
 * Simple, yet powerful and extensible command line parser.
 * Copyright (C) 2001-2010 by Boris Popov <borisxm@gmail.com>.
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
  @abstract(Simple, yet powerful and extensible command line parser.)
}
unit cmdlparse;

interface
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  SysUtils, Classes;

const
  {: Used to inidicate that a 'novalue' condition for integer fields. }
  TCLP_NoIntegerValue	= $0ABBCCDD;

type
  {: All exceptions specific to parser should be of this type. }
  ECommandLineParser = class(Exception);

  { forward declarations }
  TCommandLineParser = class;

  { TCommandLineItem }

  {: @abstract(Parent class for all types of command line switches. }
  TCommandLineItem = class
  protected
    FLongName	: string;		// 'help', 'verbose', etc
    FDesc	: string;		// 'Print this text and exit'
    FArgCount	: cardinal;		// Number of required arguments
    FShortName	: char;			// 'h', 'v'
    FPresent	: boolean;
    FRequired	: boolean;		// option must be present

    procedure ParseArgument(const Arg: string);virtual;
    function ArgumentType:string;virtual;
  public
    constructor Create(AOwner: TCommandLineParser; const ALongName: string; AShortName: char; const ADesc: string);
    function ExplainDefaults:string;virtual;
    function Explanation:string;

    property LongName: string read FLongName;
    property ShortName: char read FShortName;
    property Desc: string read FDesc;
    property Present: boolean read FPresent;
    property ArgCount: cardinal read FArgCount;
    property Required: boolean read FRequired write FRequired;
  end;

  {: @abstract(Parameterless options like -h or -v.) }
  TCommandLineFlag = class(TCommandLineItem)
  protected
  end;

  { TCommandLineString }

  {: @abstract(Option which requires a string as argument.) }
  TCommandLineString = class(TCommandLineItem)
  protected
    FValue		: string;
    FDefault		: string;
    FValidValues	: TStrings;

    procedure ParseArgument(const Arg: string);override;
    function ArgumentType:string;override;
  public
    constructor Create(AOwner: TCommandLineParser; const ALongName: string; AShortName: char;
      const ADesc: string; ADefault: string = '');
    constructor Create(AOwner: TCommandLineParser; const ALongName: string; AShortName: char;
      const ADesc: string; ADefault: string; const AValidValues:array of string);
    destructor Destroy;override;
    function ExplainDefaults:string;override;

    property Value: string read FValue;
  end;

  { TCommandLineInteger }

  {: @abstract(Option which requires a an integer as argument.) }

  TCommandLineInteger = class(TCommandLineItem)
  protected
    FValue	: integer;
    FDefault	: integer;
    FMin	: integer;
    FMax	: integer;
    procedure ParseArgument(const Arg: string);override;
    function ArgumentType:string;override;
  public
    constructor Create(AOwner: TCommandLineParser; const ALongName: string; AShortName: char;
      const ADesc: string; ADefault: integer = 0;
      AMin: integer = TCLP_NoIntegerValue; AMax: integer = TCLP_NoIntegerValue);
    function ExplainDefaults:string;override;

    property Value: integer read FValue;
  end;

  { TCommandLineParser }

  {: @abstract(The parser by itself.) }
  TCommandLineParser = class
  private
    function GetOption(const AIndex: cardinal): TCommandLineItem;
  protected
    FProgramName	: string;
    FFullProgramPath	: string;
    FOptionsList	: TList;
    FArgumentsList	: TStrings;
    FShortOptionPrefix	: char;
    FParseDone		: boolean;
    FHaveProgramPath	: boolean;

    function LookupOption(const AName: string): TCommandLineItem;
    function LookupOptionByShortName(AName: char): TCommandLineItem;
  public
    constructor Create(AHaveProgramPath: boolean = true);
    destructor Destroy;override;
    procedure Add(AOption: TCommandLineItem);
    function OptionsCount: cardinal;
    function OptionsHelp:string;
    function OptionPresent(const AName: string): boolean;
    {: Parse command line. }
    procedure Parse;overload;
    {: Parse provided string. }
    procedure Parse(const cline:string);overload;
    {: Parse provided string. }
    procedure Parse(cp: pchar);overload;
    class function GetToken(cp: pchar; var cnext: pchar): string;

    property Options[const AIndex: cardinal]: TCommandLineItem read GetOption;
    property ShortOptionPrefix: char read FShortOptionPrefix write FShortOptionPrefix;
    property Arguments: TStrings read FArgumentsList;
    property ProgramName: string read FProgramName write FProgramName;
    property FullProgramPath: string read FFullProgramPath;
  end;

const
  {: Width of options column in the help messages. }
  OptionsColumnWidth: integer = 25;

implementation

{ TCommandLineItem }
constructor TCommandLineItem.Create(AOwner: TCommandLineParser; const ALongName: string; AShortName: char; const ADesc: string);
begin
  FLongName := ALongName;
  FShortName := AShortName;
  FDesc := ADesc;
  FPresent := false;
  if AOwner <> NIL then
    AOwner.Add(self);
end;

function TCommandLineItem.ExplainDefaults: string;
begin
  result := '';
end;

function TCommandLineItem.Explanation: string;
var
  i	: integer;
  argtype : string;
begin
  if FShortName <> #0 then
    result := '-' + FShortName
  else
    result := '';

  if FLongName[1] <> '-' then begin
    if result <> '' then
      AppendStr(result, ', ');
    AppendStr(result, '--' + FLongName);
  end;

  argtype := ArgumentType;
  if argtype <> '' then
    AppendStr(result, ' <' + argtype + '>');

  i := OptionsColumnWidth - Length(result);
  while i > 0 do begin
    AppendStr(result, ' ');
    dec(i);
  end;
  AppendStr(result, FDesc);
  argtype := ExplainDefaults;
  if argtype <> '' then
    AppendStr(result, ' (' + argtype + ')');
end;

procedure TCommandLineItem.ParseArgument(const Arg: string);
begin
  // do nothing, or better raise an error
end;

function TCommandLineItem.ArgumentType: string;
begin
  result := '';
end;


{ TCommandLineString }

constructor TCommandLineString.Create(AOwner: TCommandLineParser;
  const ALongName: string; AShortName: char; const ADesc: string;
  ADefault: string);
begin
  inherited Create(AOwner, ALongName, AShortName, ADesc);
  FValue := ADefault;
  FDefault := ADefault;
  FArgCount := 1;
  FValidValues := nil;
end;

constructor TCommandLineString.Create(AOwner: TCommandLineParser; const ALongName: string;
   AShortName: char; const ADesc: string; ADefault: string; const AValidValues:array of string);
var
  i : integer;
begin
  Create(AOwner, ALongName, AShortName, ADesc, ADefault);

  FValidValues := TStringList.Create;
  for i := low(AValidValues) to high(AValidValues) do
    FValidValues.Add(AValidValues[i]);
end;

destructor TCommandLineString.Destroy;
begin
  FreeAndNil(FValidValues);
  inherited Destroy;
end;

function TCommandLineString.ExplainDefaults: string;
var
  i : integer;
begin
  if FValidValues <> nil then begin
    result := '';
    for i := 0 to pred(FValidValues.Count) do begin
       if result <> '' then
         if i <> (FValidValues.Count - 1) then
           AppendStr(result, ', ')
         else
           AppendStr(result, ' or ');
       AppendStr(result, '''' + FValidValues[i] + '''');
    end;
  end else
    result := '';

  if FDefault <> '' then begin
    if result <> '' then
      AppendStr(result, ', ');
    AppendStr(result, '''' + FDefault + ''' is default');
  end;
end;

procedure TCommandLineString.ParseArgument(const Arg: string);
begin
  FValue := Arg;
  if FValidValues = NIL then
    exit;

  if FValidValues.IndexOf(Arg) <> -1 then
    exit;
  raise ECommandLineParser.CreateFmt('Value ''%s'' is not allowed here', [Arg]);
end;

function TCommandLineString.ArgumentType: string;
begin
  result := 'string';
end;

{ TCommandLineInteger }

constructor TCommandLineInteger.Create(AOwner: TCommandLineParser; const ALongName: string;
  AShortName: char; const ADesc: string; ADefault: integer;
  AMin: integer = TCLP_NoIntegerValue; AMax: integer = TCLP_NoIntegerValue);
begin
  inherited Create(AOwner, ALongName, AShortName, ADesc);
  FValue := ADefault;
  FDefault := ADefault;
  FMin := AMin;
  FMax := AMax;
  FArgCount := 1;
end;

function TCommandLineInteger.ExplainDefaults: string;
begin
  case FMax - FMin of
    0: result := '';
    1: result := Format('%d or %d', [FMin, FMax]);
    2: result := Format('%d, %d or %d', [FMin, FMax - 1, FMax]);
  else
    result := Format('%d..%d', [FMin, FMax]);
  end;
  if (result = '') and (FDefault = TCLP_NoIntegerValue) then
    exit;
  if FDefault <> TCLP_NoIntegerValue then begin
    if result <> '' then
      AppendStr(result, ', ');
    AppendStr(result, IntToStr(FDefault) + ' is default');
  end;
end;

procedure TCommandLineInteger.ParseArgument(const Arg: string);
begin
  FValue := StrToInt(Arg);
  if (FMin <> TCLP_NoIntegerValue) and (FValue < FMin) then
    raise ECommandLineParser.CreateFmt('Value %d are less than %d', [FValue, FMin]);
  if (FMin <> TCLP_NoIntegerValue) and (FValue > FMax) then
    raise ECommandLineParser.CreateFmt('Value %d are greater than %d', [FValue, FMax]);
end;

function TCommandLineInteger.ArgumentType: string;
begin
  result := 'number';
end;

{ TCommandLineParser }

constructor TCommandLineParser.Create(AHaveProgramPath: boolean);
begin
  FOptionsList := TList.Create;
  FArgumentsList := TStringList.Create;
  FShortOptionPrefix := '-';
  FHaveProgramPath := AHaveProgramPath;
  FParseDone := false;
end;

destructor TCommandLineParser.Destroy;
var
  i	: integer;
begin
  for i := 0 to pred(OptionsCount) do
    Options[i].Free;
  FreeAndNil(FOptionsList);
  FreeAndNil(FArgumentsList);
  inherited;
end;

procedure TCommandLineParser.Add(AOption: TCommandLineItem);
begin
  FOptionsList.Add(AOption);
end;

function TCommandLineParser.OptionsCount: cardinal;
begin
  result := FOptionsList.Count;
end;

function TCommandLineParser.OptionsHelp: string;
var
  i: cardinal;
begin
  result := '';
  for i := 0 to pred(OptionsCount) do begin
    AppendStr(result, Options[i].Explanation);
{$ifdef Win32}
    AppendStr(result, #13#10);
{$else}
    AppendStr(result, #10);
{$endif}
  end;
end;

function TCommandLineParser.OptionPresent(const AName: string): boolean;
var
  opt	: TCommandLineItem;
begin
  opt := LookupOption(AName);
  if opt = NIL then
    raise ECommandLineParser.CreateFmt('Asking for undeclared option %s', [AName]);
  result := opt.FPresent;
end;

class function TCommandLineParser.GetToken(cp: pchar; var cnext: pchar):string;
const
  SPaces = [#9, #10, #13, #32];
var
  sp : pchar;
  st : char;
begin
  // skip leading spaces
  while cp[0] in Spaces do
    inc(cp);

  // determine quotation character
  if (cp[0] = '''') or (cp[0] = '"') then begin
    st := cp[0];
    inc(cp);
  end else
    st := #0;

  sp := cp;
  // look through token
  while not (cp[0] in Spaces + [st]) do
    inc(cp);
  if (st <> #0) and (cp[0] <> st) then
    raise ECommandLineParser.Create('Unterminated quoted string');
  result := Copy(sp, 1, cp - sp);
  if st <> #0 then
    inc(cp);
  cnext := cp;
end;

procedure TCommandLineParser.Parse;
begin
  Parse(cmdline);
end;

procedure TCommandLineParser.Parse(cp: pchar);
  procedure ParseError(const AMsg: string);
  begin
    raise ECommandLineParser.CreateFmt('Error: %s at %s', [AMsg, Copy(string(cp), 1, 5)]);
  end;

  procedure skipSpaces;
  begin
    while cp[0] in [' ', #9, #10, #13] do
      inc(cp);
  end;
var
  opt : TCommandLineItem;
  optname: string;
  optidx: cardinal;
  optend: pchar;
  i	: integer;
begin
  for optidx := 0 to pred(OptionsCount) do
    Options[optidx].FPresent := false;

  skipSpaces;
  if FHaveProgramPath then
    FFullProgramPath := GetToken(cp, cp);

  skipSpaces;
  while cp[0] <> #0 do begin
    if cp[0] <> FShortOptionPrefix then
      break;	// First non option
    inc(cp);
    if cp[0] = '-' then begin	// long option
      inc(cp);
      if cp[0] in [#0, ' ', #9] then // '--' - the end of the options list
        break;
      optend := cp;
      // Skip to the end of option name
      while not (optend[0] in [#0, ' ', #9, '=']) do
        inc(optend);
      optname := Copy(cp, 1, optend - cp);
      opt := LookupOption(optname);
      if opt = NIL then
        ParseError('Unknown option --' + optname);
      cp := optend;
      if (opt.FArgCount = 0) and (cp[0] = '=') then
        ParseError('Option may not have an argument');
      opt.FPresent := true;
      if opt.FArgCount = 0 then begin
        opt := NIL;
      end else begin
        inc(cp);
        skipSpaces;
        opt.ParseArgument(GetToken(cp, cp));
      end;
      skipSpaces;
      continue;
    end;

    while true do begin // short options list
      opt := LookupOptionByShortName(cp[0]);
      if opt = NIL then
        ParseError('Unknown option');
      opt.FPresent := true;
      inc(cp);
      if opt.FArgCount <> 0 then begin
        skipSpaces;
        opt.ParseArgument(GetToken(cp, cp));
        break;
      end;
      if cp[0] in [#0, #9, ' '] then
        break;
    end;
    skipSpaces;
  end; // while in options

  for i := 0 to pred(OptionsCount) do begin
    opt := Options[i];
    if opt.Required and not opt.Present then
      raise ECommandLineParser.CreateFmt('%s: Option ''%s'' must be present.', [FProgramName, opt.Desc]);
  end;

  while true do begin
    skipSpaces;
    if cp[0] = #0 then
      break;
    FArgumentsList.Add(GetToken(cp, cp));
  end;
  FParseDone := true;
end;

procedure TCommandLineParser.Parse(const cline: string);
begin
  Parse(pchar(cline));
end;

function TCommandLineParser.GetOption(const AIndex: cardinal): TCommandLineItem;
begin
  result := TCommandLineItem(FOptionsList[AIndex]);
end;

function TCommandLineParser.LookupOption(const AName: string): TCommandLineItem;
var
  i	: cardinal;
begin
  for i := 0 to pred(OptionsCount) do begin
    result := Options[i];
    if result.LongName = AName then
      exit;
  end;
  result := NIL;
end;

function TCommandLineParser.LookupOptionByShortName(AName: char): TCommandLineItem;
var
  i	: cardinal;
begin
  for i := 0 to pred(OptionsCount) do begin
    result := Options[i];
    if result.FShortName = AName then
      exit;
  end;
  result := NIL;
end;

end.

