unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  connection,view,mbrtuslave;

type

  { TForm1 }

  TForm1 = class(TForm)
    Connect: TButton;
    View: TButton;
    procedure ConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ViewClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  SerialLink : TMBrtuSlave;
  modbusTable : array[0..10] of word;
  startAddress : word;
  tableLength : byte;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ConnectClick(Sender: TObject);
begin
  Form2.show;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if SerialLink<>nil then
     if SerialLink.Connected then SerialLink.Disconnect;
end;

procedure TForm1.ViewClick(Sender: TObject);
begin
  Form3.show;
end;

end.

