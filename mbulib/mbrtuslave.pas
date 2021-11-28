unit mbrtuslave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,mbserialslave;

type
  TMBrtuSlave = class(TMBSerialSlave)
    protected
      receivedFrame : string;
    public
      constructor create(port:string;br:integer;parite:char;bits:integer;stop:integer;slave:byte);
  end;

implementation

constructor TMBrtuSlave.create(port:string;br:integer;parite:char;bits:integer;stop:integer;slave:byte);
begin
inherited create;
with self do
     begin
     PortName:=port;
     BaudRate:=br;
     Parity:=parite;
     DataBits:=bits;
     StopBits:=stop;
     FSlaveNumber:=slave;
     end;
if self.connect then self.start;
end;


end.

