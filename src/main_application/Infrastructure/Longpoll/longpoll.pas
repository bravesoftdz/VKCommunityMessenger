unit longpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vkgsobserver, fphttpclient, fpjson, vkdao, sqldb,
  entities, fgl;

type

  { TLongPollServer }

  TLongPollServer = class
  private
    FTS: string;
    FKey: string;
    FServer: string;
    procedure SetTS(AValue: string);
    procedure SetKey(AValue: string);
    procedure SetServer(AValue: string);
  public
    property Key: string read FKey write SetKey;
    property Server: string read FServer write SetServer;
    property TS: string read FTS write SetTS;
  end;

  TLongPollServersObjectList = specialize TFPGObjectList<TLongpollServer>;

  { TLongPollWorker }

  TLongPollWorker = class(TThread)
  private
    Observable: TVKGSObservable;
    HTTPClient: TFPHTTPClient;
    CommunitiesKeys: TStringList;
    procedure InitializeServerList(var ServersList: TLongPollServersObjectList);
    {Function returns first ts}
    function MakeFirstCall(AccessKey: string): TLongPollServer;
    {Function decides whether to send update notification}
    function ProcessServer(Server: TLongPollServer): boolean;
    procedure NotifyObserversThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; Communities: TCommunityList);
    procedure SubscribeForNotifications(Me: TVKGSObserver);
    destructor Destroy; override;
  end;

implementation

{ TLongPollServer }

procedure TLongPollServer.SetKey(AValue: string);
begin
  if FKey = AValue then
    Exit;
  FKey := AValue;
end;

procedure TLongPollServer.SetTS(AValue: string);
begin
  if FTS = AValue then
    Exit;
  FTS := AValue;
end;

procedure TLongPollServer.SetServer(AValue: string);
begin
  if FServer = AValue then
    Exit;
  FServer := AValue;
end;

{ TLongPollWorker }

function TLongPollWorker.MakeFirstCall(AccessKey: string): TLongPollServer;
var
  JSONResponse, Response: TJSONObject;
begin
  JSONResponse := DAO.Messages.GetLongPollServer(HTTPClient, AccessKey);
  Response := (JSONResponse['response'] as TJSONObject);

  Result := TLongPollServer.Create;
  Result.Key := Response['key'].AsString;
  Result.Server := Response['server'].AsString;
  Result.TS := Response['ts'].AsString;

  FreeAndNil(JSONResponse);
end;

function TLongPollWorker.ProcessServer(Server: TLongPollServer): boolean;
var
  URL, Response: string;
  JSONResponse: TJSONObject;
  Updates: TJSONArray;
  Update: TJSONArray;
  i: integer;
begin
  URL := 'https://' + Server.Server + '?act=a_check&key=' + Server.Key +
    '&ts=' + Server.TS + '&wait=5&mode=2&version=1';
  Response := HTTPClient.Get(URL);
  JSONResponse := GetJSON(Response) as TJSONObject;
  Updates := JSONResponse['updates'] as TJSONArray;
  if Updates.Count > 0 then
    for i := 0 to Updates.Count - 1 do
    begin
      Update := (Updates[i] as TJSONArray);
      if Update.Items[0].AsInt64 = 4 then
      begin
        Result := True;
        Server.TS := JSONResponse['ts'].AsString;
        break;
      end
      else
        Result := False;
    end
  else
    Result := False;
  FreeAndNil(JSONResponse);
end;

procedure TLongPollWorker.NotifyObserversThreadMethod;
begin
  Observable.NotifyObservers;
end;

procedure TLongPollWorker.InitializeServerList(
  var ServersList: TLongPollServersObjectList);
var
  i: integer;
begin
  for i := 0 to CommunitiesKeys.Count - 1 do
  begin
    ServersList.Add(MakeFirstCall(CommunitiesKeys[i]));
  end;
end;

procedure TLongPollWorker.Execute;
var
  ServersList: TLongPollServersObjectList;
  i: integer;
  CurrentServer: TLongPollServer;
begin
  ServersList := TLongPollServersObjectList.Create(True);
  InitializeServerList(ServersList);

  while not Terminated do
  begin
    for i := 0 to ServersList.Count - 1 do
    begin
      CurrentServer := ServersList[i];

      if ProcessServer(CurrentServer) then
        Queue(@NotifyObserversThreadMethod);

      if terminated then
        break;
    end;
  end;
  FreeAndNil(ServersList);
end;

constructor TLongPollWorker.Create(CreateSuspended: boolean;
  Communities: TCommunityList);
var
  i: integer;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  Observable := TVKGSObservable.Create;
  CommunitiesKeys := TStringList.Create;
  for i := 0 to Communities.Count - 1 do
    CommunitiesKeys.Add(Communities[i].AccessKey);
  FreeAndNil(Communities);
  inherited Create(CreateSuspended);
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