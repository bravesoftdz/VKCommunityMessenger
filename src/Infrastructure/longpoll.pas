unit longpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vkgsobserver, fphttpclient, fpjson, vkdao, sqldb, DB, sqlite3conn, vkgsconfig, entities;

type

  { TLongPollWorker }

  TLongPollWorker = class(TThread)
  private
    Observable: TVKGSObservable;
    ServerAddress: string;
    Key: string;
    HTTPClient: TFPHTTPClient;
    {Function returns first ts}
    function MakeFirstCall: string;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; Communities: TCommunityList);
    procedure SubscribeForNotifications(Me: TVKGSObserver);
    destructor Destroy; override;
  end;

implementation

{ TLongPollWorker }

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
    Observable.NotifyObservers;
    Sleep(10000);
  end;
end;

constructor TLongPollWorker.Create(CreateSuspended: boolean;
  Communities: TCommunityList);
begin
  inherited Create(CreateSuspended);
  HTTPClient := TFPHTTPClient.Create(nil);
  Observable := TVKGSObservable.Create;
end;

procedure TLongPollWorker.SubscribeForNotifications(Me: TVKGSObserver);
begin
  Me.Subscribe(Observable);
end;

destructor TLongPollWorker.Destroy;
begin
  FreeAndNil(HTTPClient);
  inherited Destroy;
end;

end.
