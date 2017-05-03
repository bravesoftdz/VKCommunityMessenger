unit longpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLongPollWorker }

  TLongPollWorker = class(TThread)
    procedure Execute; override;
  end;

  { TLongPollClient }

  TLongPollClient = class
  private
    Worker: TLongPollWorker;
  public
    constructor Create;
    destructor Destroy;
  end;

var
  LongPollClient: TLongPollClient;

implementation

{ TLongPollWorker }

procedure TLongPollWorker.Execute;
begin
  while not Terminated do
    Sleep(0);
end;

{ TLongPollClient }

constructor TLongPollClient.Create;
begin
  Worker := TLongPollWorker.Create(True);
  Worker.Start;
end;

destructor TLongPollClient.Destroy;
begin
  Worker.Terminate;
  Worker.WaitFor;
  FreeAndNil(Worker);
end;

initialization

  LongPollClient := TLongPollClient.Create;

finalization

  FreeAndNil(LongPollClient);

end.
