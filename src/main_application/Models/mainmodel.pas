unit MainModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, entities, Graphics, AbstractModel, fphttpclient,
  Dialogs, fpjson, jsonparser, VKDAO, sqlite3conn, vkcmconfig, DB,
  sqldb, longpoll, vkcmobserver;

type

  { IMainModel }

  {Interface for mainview's model with long comments}
  IMainModel = interface(IModel)
    {Returns full information about community}
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    {Saves community information in local databse}
    procedure SaveCommunityInfo(Communty: TCommunity);
    {Reads access keys from local database and loads community information from internet
     or (in case when there is no internet) from local storage}
    function GetCommunities: TCommunityList;
    {Returns photo that is used as avatar for communities with no photo}
    function GetNoPhotoAvatar: TPicture;
    {Returns photo for "Add new community" button}
    function GetAddNewCommunityPhoto: TPicture;
    {Loads frame image for toolbar}
    function LoadFrameImage: TPicture;
  end;

  { TMainModel }

  TMainModel = class(TObserverModel, IMainModel, IModel)
  private
    Connection: TSQLite3Connection;
    LongpollWorker: TLongPollWorker;
    procedure ParseGroupGetByIdResponse(const JSONResponseDocument: TJSONObject;
      const AccessKey: string; var Community: TCommunity);
    procedure OnNotified;
  public
    constructor Create;
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfo(Community: TCommunity);
    function GetCommunities: TCommunityList;
    function GetNoPhotoAvatar: TPicture;
    function GetAddNewCommunityPhoto: TPicture;
    function LoadFrameImage: TPicture;
    destructor Destroy; override;
  end;

var
  LMainModel: TMainModel;

implementation

{ TMainModel }

procedure TMainModel.ParseGroupGetByIdResponse(const JSONResponseDocument: TJSONObject;
  const AccessKey: string; var Community: TCommunity);
var
  JSONCommunityObject: TJSONObject;
  ResponseArray: TJSONArray;
  HTTPClient: TFPHTTPClient;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  ResponseArray := (JSONResponseDocument['response'] as TJSONArray);
  JSONCommunityObject := (ResponseArray[0] as TJSONObject);
  {Serialize}
  Community.AccessKey := AccessKey;
  Community.CommunityType := StringToCommunityType(
    JSONCommunityObject['type'].AsString);
  if Assigned(JSONCommunityObject.Find('deactivated')) then
    Community.Deactivated := True
  else
    Community.Deactivated := False;
  Community.HasPhoto := JSONCommunityObject['has_photo'].AsBoolean;
  Community.Id := JSONCommunityObject['id'].AsString;
  Community.IsClosed := JSONCommunityObject['is_closed'].AsBoolean;
  Community.Name := JSONCommunityObject['name'].AsString;
  Community.ScreenName := JSONCommunityObject['screen_name'].AsString;
  if Community.HasPhoto then
    Community.Photo := DAO.LoadPhoto(HTTPClient,
      JSONCommunityObject['photo_50'].AsString);
  FreeAndNil(HTTPClient);
end;

procedure TMainModel.OnNotified;
begin
  Observable.NotifyObservers;
end;

constructor TMainModel.Create;
begin
  Connection := TSQLite3Connection.Create(nil);
  Connection.DatabaseName := DATABASE_PATH;
  if not FileExists(DATABASE_PATH) then
    try
      DAO.Database.ExecuteDatabaseCreationScript(Connection);
    except
      raise Exception.Create('Ошибка при создании базы');
    end;
  Connection.Open;

  Observer := TVKCMObserver.Create;
  Observer.Notify := @OnNotified;

  Observable := TVKCMObservable.Create;

  LongpollWorker := TLongPollWorker.Create(True, GetCommunities);
  LongpollWorker.SubscribeForNotifications(Observer);
  LongpollWorker.Start;
end;

function TMainModel.GetExtendedCommunityInformation(CommunityId,
  AccessKey: string): TCommunity;
var
  JSONResponseDocument: TJSONObject;
  HTTPClient: TFPHTTPClient;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  Result := TCommunity.Create;
  try
    JSONResponseDocument := DAO.Groups.GetById(HTTPClient, AccessKey, CommunityId);
    if Assigned(JSONResponseDocument.Find('error')) then
      raise Exception.Create('Неправильные Id и/или ключ доступа');
    ParseGroupGetByIdResponse(JSONResponseDocument, AccessKey, Result);
  except
    raise;
  end;
  FreeAndNil(HTTPClient);
end;

procedure TMainModel.SaveCommunityInfo(Community: TCommunity);
begin
  try
    DAO.Database.SaveCommunity(Connection, Community);
  except
    raise;
  end;
  LongpollWorker.FreeOnTerminate := True;
  LongpollWorker.Terminate;
  LongpollWorker := TLongPollWorker.Create(False, GetCommunities);
end;

function TMainModel.GetCommunities: TCommunityList;
var
  Community: TCommunity;
  CommunitiesDataset: TDataset;
  QueryTransaction: TSQLTransaction;
  i: integer;
begin
  Result := TCommunityList.Create;
  CommunitiesDataset := DAO.Database.LoadDatabaseDataset(Connection, QueryTransaction);
  CommunitiesDataset.Open;
  CommunitiesDataset.First;
  for i := 0 to CommunitiesDataset.RecordCount - 1 do
  begin
    Community := TCommunity.Create;
    Community.HasPhoto := CommunitiesDataset.FieldByName('HasPhoto').AsBoolean;
    if Community.HasPhoto then
    begin
      Community.Photo := TPicture.Create;
      Community.Photo.LoadFromStream(CommunitiesDataset.CreateBlobStream(
        CommunitiesDataset.FieldByName('Photo'), bmRead));
    end;
    Community.AccessKey := CommunitiesDataset.FieldByName('AccessKey').AsString;
    Community.CommunityType :=
      StringToCommunityType(CommunitiesDataset.FieldByName('CommunityType').AsString);
    Community.Deactivated := CommunitiesDataset.FieldByName('Deactivated').AsBoolean;
    Community.Id := CommunitiesDataset.FieldByName('Id').AsString;
    Community.IsClosed := CommunitiesDataset.FieldByName('IsClosed').AsBoolean;
    Community.Name := CommunitiesDataset.FieldByName('Name').AsString;
    Community.ScreenName := CommunitiesDataset.FieldByName('ScreenName').AsString;
    Community.Chatbot.FillFromString(CommunitiesDataset.FieldByName(
      'SerializedChatBot').AsString);
    Result.Add(Community);
    CommunitiesDataset.Next;
  end;
  FreeAndNil(CommunitiesDataset);
  FreeAndNil(QueryTransaction);
end;


function TMainModel.GetNoPhotoAvatar: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('img/noavatar.bmp');
end;

function TMainModel.GetAddNewCommunityPhoto: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('img/newuser.bmp');
end;

function TMainModel.LoadFrameImage: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('img/frame.bmp');
end;

destructor TMainModel.Destroy;
begin
  Connection.Close();
  FreeAndNil(Connection);
  LongpollWorker.Terminate;
  LongpollWorker.WaitFor;
  FreeAndNil(LongpollWorker);
  inherited Destroy;
end;

end.
