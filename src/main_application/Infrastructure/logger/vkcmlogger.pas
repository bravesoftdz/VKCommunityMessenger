unit vkcmlogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

procedure OutDebug(ADebugStr: string);

implementation

var CS: TCriticalSection;

procedure OutDebug(ADebugStr: string);
var StrList: TStringList;
begin
  {$IFDEF DEBUG}
  StrList := TStringList.Create;
  CS.Enter;
  try
  if FileExists('log.log') then
     StrList.LoadFromFile('log.log');
  StrList.Add(ADebugStr);
  StrList.SaveToFile('log.log');
  finally
  CS.Leave;
  FreeAndNil(StrList);
  end;
  {$ENDIF}
end;

initialization

CS := TCriticalSection.Create;

finalization

FreeAndNil(CS);

end.

