unit view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TForm3 }

  TForm3 = class(TForm)
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MBoffset8: TEdit;
    MBoffset9: TEdit;
    MBfieldLength: TEdit;
    MBoffset2: TEdit;
    MBoffset3: TEdit;
    MBoffset4: TEdit;
    MBoffset5: TEdit;
    MBoffset6: TEdit;
    MBoffset7: TEdit;
    Memo1: TMemo;
    nbChars: TEdit;
    startAddress: TEdit;
    MBoffset0: TEdit;
    MBoffset1: TEdit;
    Shape1: TShape;
    Shape2: TShape;
    UpDownFielLength: TUpDown;
    procedure MBfieldLengthChange(Sender: TObject);
    procedure MBoffset0Change(Sender: TObject);
    procedure MBoffset1Change(Sender: TObject);
    procedure MBoffset2Change(Sender: TObject);
    procedure MBoffset3Change(Sender: TObject);
    procedure MBoffset4Change(Sender: TObject);
    procedure MBoffset5Change(Sender: TObject);
    procedure MBoffset6Change(Sender: TObject);
    procedure MBoffset7Change(Sender: TObject);
    procedure MBoffset8Change(Sender: TObject);
    procedure MBoffset9Change(Sender: TObject);
    procedure startAddressChange(Sender: TObject);
    procedure updateReceivedFrame(Sender: TObject);
    procedure updateModbusField(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpDownFielLengthChanging(Sender: TObject; var AllowChange: Boolean
      );
  private

  public

  end;

var
  Form3: TForm3;
  receivedStr : string;

implementation

{$R *.lfm}

uses main;

var
  nbFrame : integer;



{ TForm3 }

{
         update the Frame counter field and the frame memo
}
procedure TForm3.updateReceivedFrame(Sender: TObject);
var
  i : integer;
  frameString : string;
begin
while SerialLink.queryStack.Count>0 do
  begin
  inc(nbFrame);
  nbChars.Text:=inttostr(nbFrame);
  receivedStr:=SerialLink.queryStack.Strings[0];
  SerialLink.queryStack.delete(0);
  frameString:='';
  for i:=1 to length(receivedStr) do
           frameString:=frameString+'$'+hexstr(ord(receivedStr[i]),2)+' ';
  memo1.Append(frameString);
  end;
end;

{
         update the modbus field at the corresponding offset
}

procedure TForm3.updateModbusField(Sender: TObject);
begin
     MBoffset0.text:=inttostr(main.modbusTable[0]);
     MBoffset1.text:=inttostr(main.modbusTable[1]);
     MBoffset2.text:=inttostr(main.modbusTable[2]);
     MBoffset3.text:=inttostr(main.modbusTable[3]);
     MBoffset4.text:=inttostr(main.modbusTable[4]);
     MBoffset5.text:=inttostr(main.modbusTable[5]);
     MBoffset6.text:=inttostr(main.modbusTable[6]);
     MBoffset7.text:=inttostr(main.modbusTable[7]);
     MBoffset8.text:=inttostr(main.modbusTable[8]);
     MBoffset9.text:=inttostr(main.modbusTable[9]);
end;

procedure TForm3.MBoffset0Change(Sender: TObject);
begin
  main.modbusTable[0]:=strtoint(MBoffset0.text);
end;

procedure TForm3.MBoffset1Change(Sender: TObject);
begin
  main.modbusTable[1]:=strtoint(MBoffset1.text);
end;

procedure TForm3.MBoffset2Change(Sender: TObject);
begin
  main.modbusTable[2]:=strtoint(MBoffset2.text);
end;

procedure TForm3.MBoffset3Change(Sender: TObject);
begin
  main.modbusTable[3]:=strtoint(MBoffset3.text);
end;

procedure TForm3.MBoffset4Change(Sender: TObject);
begin
  main.modbusTable[4]:=strtoint(MBoffset4.text);
end;

procedure TForm3.MBoffset5Change(Sender: TObject);
begin
  main.modbusTable[5]:=strtoint(MBoffset5.text);
end;

procedure TForm3.MBoffset6Change(Sender: TObject);
begin
  main.modbusTable[6]:=strtoint(MBoffset6.text);
end;

procedure TForm3.MBoffset7Change(Sender: TObject);
begin
  main.modbusTable[7]:=strtoint(MBoffset7.text);
end;

procedure TForm3.MBoffset8Change(Sender: TObject);
begin
  main.modbusTable[8]:=strtoint(MBoffset8.text);
end;

procedure TForm3.MBoffset9Change(Sender: TObject);
begin
  main.modbusTable[9]:=strtoint(MBoffset9.text);
end;

procedure TForm3.startAddressChange(Sender: TObject);
begin
  main.SerialLink.MBstartaddress:=strtoint(startAddress.text);
end;

procedure TForm3.MBfieldLengthChange(Sender: TObject);
begin
  main.SerialLink.MBdataLength:=strtoint(MBfieldLength.text);;
end;

procedure TForm3.FormShow(Sender: TObject);
var
  i : integer;
begin
   // when a request frame is received, updateReceivedFrame is called
   SerialLink.rxFrameEvent := @updateReceivedFrame;
   // when a write request frame is received, updateModbusField is called
   SerialLink.writeEvent := @updateModbusField;
   // init of the modbus registers value with modbusTable
   for i:=0 to 10 do
      main.SerialLink.MBdata[i]:=@main.modbusTable[i];
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  SerialLink.rxFrameEvent := nil;
  SerialLink.writeEvent := nil;
end;


end.

