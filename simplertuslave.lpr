program simplertuslave;

{$mode objfpc}{$H+}

uses
  cthreads,classes,mbserialslave,sysutils;

var
  serialSlave : TMBSerialSlave;
  word0 : uint16;
  word1 : uint16;
  word2 : uint16;
  oldword0 : uint16;
  oldword1 : uint16;
  oldword2 : uint16;

function initMBserial:boolean;
begin
     serialSlave:=TMBSerialSlave.Create;
     with serialSlave do
          begin
          PortName:='/dev/ttyUSB1';
          BaudRate:=9600;
          Parity:='N';
          DataBits:=8;
          StopBits:=1;
          end;
     result:=serialSlave.Connect;
end;

procedure initMBsettings;
begin
     with serialSlave do
          begin
          slaveNumber:=1;        // slave 1
          MBstartaddress:=100;   // modbus data start at register address 100
          MBdataLength:=3;       // length of data is 3 16 bits registers
          end;
end;

procedure initMBdata;
begin
     with serialSlave do
          begin
          MBdata[0]:=@word0;
          MBdata[1]:=@word1;
          MBdata[2]:=@word2;
          end;
end;

procedure startSlave;
begin
     serialSlave.start;
end;

begin
  writeln('MODBUS RTU SLAVE');
  if initMBserial then writeln('Serial Port /dev/ttyUSB1 connected')
  else
    begin
    writeln('default on /dev/ttyUSB1 Port');
    exit;
    end;
  initMBsettings;
  writeln(' Modbus slave 1 with 3 registers starting at address 100');
  initMBdata;
  writeln(' word0 is at address 100');
  writeln(' word1 is at address 101');
  writeln(' word2 is at address 102');
  startSlave;
  writeln('Modbus slave start');
  word0:=0;
  word1:=1;
  word2:=2;
  oldword0:=word0;
  oldword1:=word1;
  oldword2:=word2;
  writeln(inttostr(word0)+'-'+inttostr(word1)+'-'+inttostr(word2));
  while True do
        begin
        sleep(1);
        if (oldword0<>word0) or (oldword1<>word1) or (oldword2<>word2) then
           begin
           writeln(inttostr(word0)+'-'+inttostr(word1)+'-'+inttostr(word2));
           oldword0:=word0;
           oldword1:=word1;
           oldword2:=word2;
           end;
        end;
end.

