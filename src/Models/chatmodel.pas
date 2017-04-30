unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, Graphics, fphttpclient, entities, VKDAO, fpjson;

type

  { IChatModel }

  IChatModel = interface(IModel)
    ['{F58AF832-F8DE-46DF-AC0C-85B19585DB0E}']
    function GetSendPicture: TPicture;
    function GetLastDialogs(Community: TCommunity): TDialogsList;
    function GetNoAvatarPicture: TPicture;
  end;

  { TChatModel }

  TChatModel = class(TInterfacedObject, IChatModel, IModel)
  private
    HTTPClient: TFPHTTPClient;
  public
    constructor Create;
    function GetSendPicture: TPicture;
    function GetNoAvatarPicture: TPicture;
    function GetLastDialogs(Community: TCommunity): TDialogsList;
    destructor Destroy; override;
  end;

var
  LChatModel: TChatModel;

implementation

{ TChatModel }

constructor TChatModel.Create;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
end;

function TChatModel.GetSendPicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\send.png');
end;

function TChatModel.GetNoAvatarPicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\no_userimage.png');
end;

function TChatModel.GetLastDialogs(Community: TCommunity): TDialogsList;
var
  NewDialog: TDialog;
  JSONDialogsResponse, JSONUserResponse: TJSONObject;
  DialogsResponse: TJSONObject;
  UserResponse: TJSONArray;
  Items: TJSONArray;
  i: integer;
  Message: TJSONObject;
  UserId: string;
  UserIds: TStringList;
  CurrentUser, City: TJSONObject;
  NewUser: TUser;
begin
  {Create resources}
  UserIds:= TStringList.Create;
  Result := TDialogsList.Create;
  JSONDialogsResponse := DAO.Messages.GetDialogs(HTTPClient, Community.AccessKey, 10, 0);
  DialogsResponse := (JSONDialogsResponse['response'] as TJSONObject);
  Items := (DialogsResponse['items'] as TJSONArray);
  for i := 0 to Items.Count - 1 do
  begin
    Message := ((Items[i] as TJSONObject)['message'] as TJSONObject);
    UserId := Message['user_id'].AsString;
    UserIds.Add(UserId);
  end;
  JSONUserResponse:=DAO.Users.Get(HTTPClient,UserIds);
  UserResponse := (JSONUserResponse['response'] as TJSONArray);
  for i:=0 to UserResponse.Count-1 do
  begin
    CurrentUser := (UserResponse[i] as TJSONObject);
    NewUser := TUser.Create;
    NewUser.FirstName:=CurrentUser['first_name'].AsString;
    NewUser.LastName:=CurrentUser['last_name'].AsString;
    NewUser.Photo50:=DAO.LoadPhoto(HTTPClient,CurrentUser['photo_50'].AsString);
    NewUser.Photo200:=DAO.LoadPhoto(HTTPClient,CurrentUser['photo_200'].AsString);
    NewDialog := TDialog.Create;
    NewDialog.Person:=NewUser;
    Result.Add(NewDialog);
  end;
  FreeAndNil(UserIds);
end;

destructor TChatModel.Destroy;
begin
  FreeAndNil(HTTPClient);
  inherited Destroy;
end;

end.
