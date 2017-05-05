unit longpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vkgsobserver, fphttpclient, fpjson, vkdao, sqldb, DB, sqlite3conn, vkgsconfig;

type

  { TLongPollWorker }

  TLongPollWorker = class(TThread)
  private
    FObservable: TVKGSObservable;
    ServerAddress: string;
    Key: string;
    HTTPClient: TFPHTTPClient;
    procedure SetObservable(AValue: TVKGSObservable);
    {Function returns first ts}
    function MakeFirstCall: string;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  { TLongPollClient }

  TLongPollClient = class
  private
    Worker: TLongPollWorker;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  LongPollClient: TLongPollClient;

implementation

{ TLongPollWorker }

procedure TLongPollWorker.SetObservable(AValue: TVKGSObservable);
begin
  if FObservable = AValue then
    Exit;
  FObservable := AValue;
end;

function TLongPollWorker.MakeFirstCall: string;
var
  Response: TJSONObject;
  Connection: TSQLConnection;
  Transaction: TSQLTransaction;
  Communities: TDataset;
  i: integer;
  AccessKeys: TStringList;
begin
  {Get access keys of communities}
  Connection := TSQLite3Connection.Create(nil);
  AccessKeys:= TStringList.Create;
  try
    Connection.DatabaseName := DATABASE_NAME;
    Communities := DAO.Database.LoadDatabaseDataset(Connection, Transaction);
    Communities.Open;
    Communities.First;
    for i:=0 to Communities.RecordCount-1 do
    begin
      AccessKeys.Add(Communities.FieldByName('AccessKey').AsString);
    end;
  finally
    FreeAndNil(Transaction);
    FreeAndNil(Connection);
    FreeAndNil(AccessKeys);
  end;
end;

procedure TLongPollWorker.Execute;
begin
  while not Terminated do
  begin

  end;
end;

constructor TLongPollWorker.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  HTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TLongPollWorker.Destroy;
begin
  FreeAndNil(HTTPClient);
  inherited Destroy;
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
