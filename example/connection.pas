unit connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  mbrtuslave;

type

  { TForm2 }

  TForm2 = class(TForm)
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    databits: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    parity: TEdit;
    port: TEdit;
    esclave: TEdit;
    voyantConnection: TShape;
    connection: TStaticText;
    stopbits: TEdit;
    vitesse: TEdit;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure updateState;
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses main;

{ TForm2 }

{
update the light color and the label connected/not connected
}
procedure TForm2.updateState;
begin
  if (SerialLink<>nil) and (SerialLink.Connected) then
     begin
       voyantConnection.Brush.Color:=clLime;
       connection.Caption:='connected';
     end
  else
     begin
       voyantConnection.Brush.Color:=clRed;
       connection.Caption:='not connected';
       end;
end;

procedure TForm2.ButtonConnectClick(Sender: TObject);
var
  i : integer;
begin
  // constructor for the modbus serial slave
  main.SerialLink:=TMBrtuSlave.create( port.Text,
                                       strtoint(vitesse.Text),
                                       parity.text[1],
                                       strtoint(databits.Text),
                                       strtoint(stopbits.text),
                                       strtoint(esclave.text));
  // update display
  updateState;
  // define register first address of the modbus slave
  main.SerialLink.MBstartaddress:=0;
  // define slave
  main.SerialLink.slaveNumber:=1;
  // define number of modbus register of the modbus slave
  main.SerialLink.MBdataLength:=10;
  // init pointers for modbus slave register values
  for i:=0 to 10 do
      begin
        main.modbusTable[i]:=i;
        main.SerialLink.MBdata[i]:=@main.modbusTable[i];
      end;
end;

procedure TForm2.ButtonDisconnectClick(Sender: TObject);
begin
  if SerialLink<>nil then
     if SerialLink.Connected then SerialLink.Disconnect;
  updateState;
end;

end.

