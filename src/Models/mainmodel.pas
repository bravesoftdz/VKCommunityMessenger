unit MainModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, entities, Graphics, AbstractModel, fphttpclient,
  VKGSConfig, Dialogs, fpjson, jsonparser;

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
  public
    constructor Create;
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfo(Communty: TCommunity);
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
  HTTPClient:=TFPHTTPClient.Create(nil);
end;

function TMainModel.GetExtendedCommunityInformation(CommunityId, AccessKey: string):
TCommunity;
var
  URL: string;
  Response: string;
  JSONDoc: TJSONObject;
  ResponseArray: TJSONArray;

begin
  Result := TCommunity.Create;
  try
    URL := VK_API_BASE_URL + 'groups.getById?' + '&access_token=' +
      AccessKey + '&v=' + USED_API_VERSION + '&group_id=' + CommunityId;
    Response := HTTPClient.Get(URL);
    JSONDoc:=(GetJSON(Response) as TJSONObject);
    if Assigned(JSONDoc.Find('error')) then
       raise Exception.Create('Неправильные Id и/или ключ доступа');
    ResponseArray:=JSONDoc['response'];
    {TODO}
  except
    raise;
  end;
end;

procedure TMainModel.SaveCommunityInfo(Communty: TCommunity);
begin

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
  FreeAndNil(HTTPClient);
  inherited Destroy;
end;

end.
