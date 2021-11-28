{$REGION 'license'}
{
 * Multistream message logger.
 * Copyright (C) 2001-2003 by Boris Popov <borisxm@gmail.com>.
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
unit blogger;

interface

uses
  SysUtils, Classes, bqueue;

type
  TBLogLevel = (bllError, bllWarning, bllNotice, bllInfo, bllDebug);

  { TBLogConsumer }

  TBLogConsumer = class
  private
    FLevel	: TBLogLevel;
  public
    constructor Create;
    procedure Write(ALevel: TBLogLevel; const AMsg: string);virtual;abstract;

    property Level: TBLogLevel read FLevel write FLevel;
  end;

  { TBLogToConsole }

  TBLogToConsole = class(TBLogConsumer)
  public
    procedure Write(ALevel: TBLogLevel; const AMsg: string);override;
  end;

  { TBLogToFile }

  TBLogToFile = class(TBLogConsumer)
  private
    FFileName	: string;
    FHandle	: integer;
    FFilePos	: longint;
    FMaxSIze	: longint;
  public
    constructor Create(const AFileName: string; ATruncate: boolean = false);
    destructor Destroy;override;
    procedure Write(ALevel: TBLogLevel; const AMsg: string);override;
  end;

  { TBLogger }

  TBLogger = class
  private
    FConsumers	: TList;
    FFacility	: string;
    FProcName	: string;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Add(ALevel: TBLogLevel; const AMsg: string);overload;
    procedure Add(ALevel: TBLogLevel; const AMsg: string; AFmt: array of const);overload;
    procedure RegisterConsumer(ALC: TBLogConsumer);

    property Facility: string read FFacility write FFacility;
    property ProcName: string read FProcName write FProcName;
  end;

const
  Log: TBLogger = nil;

implementation

{ TBLogger }

constructor TBLogger.Create;
begin
  inherited;
  FConsumers := TList.Create;
  FFacility := '';
  FProcName := '';
end;

destructor TBLogger.Destroy;
var
  i	: integer;
begin
  for i := 0 to pred(FConsumers.Count) do
    if FConsumers[i] <> nil then
      TBLogConsumer(FConsumers[i]).Free;
  FreeAndNil(FConsumers);
  inherited Destroy;
end;

procedure TBLogger.Add(ALevel: TBLogLevel; const AMsg: string; AFmt: array of const);
begin
  if FConsumers.Count = 0 then
    exit;
  Add(ALevel, Format(AMsg, AFmt));
end;

procedure TBLogger.Add(ALevel: TBLogLevel; const AMsg: string);
var
  i	: integer;
  lc	: TBLogConsumer;
  msg	: string;
begin
  if FConsumers.Count = 0 then
    exit;
  msg := Format('%s %s[%u]: %s', [FormatDateTime('m d hh:nn:ss', Now), FProcName, system.GetProcessID, AMsg]);

  for i := 0 to pred(FConsumers.Count) do begin
    lc := TBLogConsumer(FConsumers[i]);
    if lc = nil then
      continue;
    lc.Write(ALevel, msg);
  end;
end;

procedure TBLogger.RegisterConsumer(ALC: TBLogConsumer);
begin
  FConsumers.Add(ALC);
end;

{ TBLogConsumer }

constructor TBLogConsumer.Create;
begin
  FLevel := bllError;
end;


{ TBLogToConsole }

procedure TBLogToConsole.Write(ALevel: TBLogLevel; const AMsg: string);
begin
  if ALevel > FLevel then
    exit;
  writeln(stderr, AMsg);
end;

{ TBLogToFile }

constructor TBLogToFile.Create(const AFileName: string; ATruncate: boolean);
begin
  inherited Create;
  FFileName := AFileName;

  if ATruncate or not FileExists(FFileName) then begin
    FHandle := FileCreate(FFileName, fmCreate);
    if FHandle < 0 then
      raise Exception.CreateFmt('%s: Unable to create file %s.', [self.ClassName, FFileName]);
    FileClose(FHandle);
  end;

  FHandle := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyWrite);
  if FHandle < 0 then
      raise Exception.CreateFmt('%s: Unable to open file %s.', [self.ClassName, FFileName]);
  FFilePos := FileSeek(FHandle, 0, fsFromEnd);
end;

destructor TBLogToFile.Destroy;
begin
  if FHandle >= 0 then
    FileClose(FHandle);
  inherited Destroy;
end;

procedure TBLogToFile.Write(ALevel: TBLogLevel; const AMsg: string);
const
  crlf:string = #10;
var
  nwr	: longint;
begin
  if ALevel > FLevel then
    exit;
  if (FMaxSize <> 0) and (FFilePos > FMaxSize) then
    exit;	// TODO: rotate file?
  nwr := FileWrite(FHandle, AMsg[1], Length(AMsg));
  if nwr < 0 then begin
    writeln(stderr, Format('%s: Unable to write file %s.', [ClassName, FFileName]));
    exit;
  end;
  FileWrite(FHandle, crlf, 1);
  inc(FFilePos, nwr + 1);
end;

initialization
  log := TBLogger.Create;

finalization
  FreeAndNil(log);
end.

