unit MainModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, entities, Graphics, AbstractModel, fphttpclient,
  Dialogs, fpjson, jsonparser, VKDAO, sqlite3conn, VKGSConfig;

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

  TMainModel = class(TInterfacedObject, IMainModel, IModel)
  private
    HTTPClient: TFPHTTPClient;
    Connection: TSQLite3Connection;
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

constructor TMainModel.Create;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  Connection := TSQLite3Connection.Create(nil);
  Connection.DatabaseName:=DATABASE_NAME;
  if not FileExists(DATABASE_NAME) then
    try
       DAO.Database.ExecuteDatabaseCreationScript(Connection);
    except
      raise Exception.Create('Ошибка при создании базы');
    end;
  Connection.Open;
end;

function TMainModel.GetExtendedCommunityInformation(CommunityId,
  AccessKey: string): TCommunity;
var
  JSONResponseDocument: TJSONObject;
  ResponseArray: TJSONArray;
  JSONCommunityObject: TJSONObject;
begin
  Result := TCommunity.Create;
  try
    JSONResponseDocument := DAO.Groups.GetById(HTTPClient, AccessKey, CommunityId);
    if Assigned(JSONResponseDocument.Find('error')) then
      raise Exception.Create('Неправильные Id и/или ключ доступа');
    ResponseArray := (JSONResponseDocument['response'] as TJSONArray);
    JSONCommunityObject := (ResponseArray[0] as TJSONObject);
    {Serialize}
    Result.AccessKey := AccessKey;
    Result.CommunityType := StringToCommunityType(JSONCommunityObject['type'].AsString);
    if Assigned(JSONCommunityObject.Find('deactivated')) then
      Result.Deactivated := True
    else
      Result.Deactivated := False;
    Result.HasPhoto := JSONCommunityObject['has_photo'].AsBoolean;
    Result.Id := JSONCommunityObject['id'].AsString;
    Result.IsClosed := JSONCommunityObject['is_closed'].AsBoolean;
    Result.Name := JSONCommunityObject['name'].AsString;
    Result.ScreenName := JSONCommunityObject['screen_name'].AsString;
    if Result.HasPhoto then
      Result.Photo := DAO.LoadPhoto(HTTPClient, JSONCommunityObject['photo_50'].AsString);
  except
    raise;
  end;
end;

procedure TMainModel.SaveCommunityInfo(Community: TCommunity);
begin
  try
  DAO.Database.SaveCommunity(Connection,Community);
  except
    raise;
  end;
end;

function TMainModel.GetCommunities: TCommunityList;
var
  Community: TCommunity;
begin
  Result := TCommunityList.Create;
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
  FreeAndNil(HTTPClient);
  FreeAndNil(Connection);
  inherited Destroy;
end;

end.
