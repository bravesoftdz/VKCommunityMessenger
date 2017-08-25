unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, Graphics, fphttpclient, entities,
  VKDAO, fpjson, dateutils, syncobjs;

type

  { IChatModel }

  IChatModel = interface(IModel)
    ['{F58AF832-F8DE-46DF-AC0C-85B19585DB0E}']
    function GetSendPicture: TPicture;
    function GetLastDialogs(Community: TCommunity): TDialogsList;
    function GetNoAvatarPicture: TPicture;
    procedure SendMessage(Community: TCommunity; Message: TMessage);
  end;

  { TChatModel }

  TChatModel = class(TInterfacedObject, IChatModel, IModel)
  private
    HTTPClient: TFPHTTPClient;
    procedure LoadMessagesIntoDialog(Dialog: TDialog; AccesKey: string);
  public
    constructor Create;
    function GetSendPicture: TPicture;
    function GetNoAvatarPicture: TPicture;
    function GetLastDialogs(Community: TCommunity): TDialogsList;
    procedure SendMessage(Community: TCommunity; Message: TMessage);
    destructor Destroy; override;
  end;

var
  LChatModel: TChatModel;

implementation

{ TChatModel }

procedure TChatModel.LoadMessagesIntoDialog(Dialog: TDialog; AccesKey: string);
var
  JSONResponse: TJSONObject;
  Response: TJSONObject;
  Items: TJSONArray;
  i: integer;
  Message: TJSONObject;
  NewMessage: TMessage;
begin
  JSONResponse := DAO.Messages.GetHistory(HTTPClient, AccesKey, Dialog.Person.Id, 50);
  Response := (JSONResponse['response'] as TJSONObject);
  Items := (Response['items'] as TJSONArray);
  for i := Items.Count - 1 downto 0 do
  begin
    Message := (Items[i] as TJSONObject);
    NewMessage := TMessage.Create;
    NewMessage.Message := Message['body'].AsString;
    NewMessage.Id := Message['id'].AsString;
    NewMessage.Date := UnixToDateTime(Message['date'].AsInt64);
    NewMessage.Deleted := False;
    NewMessage.Emoji := False;
    NewMessage.FromId := Message['from_id'].AsString;
    if Message['out'].AsInt64 = 0 then
      NewMessage.Out := otRecieved
    else
      NewMessage.Out := otSent;
    if Message['read_state'].AsInt64 = 0 then
      NewMessage.ReadState := rsUnread
    else
      NewMessage.ReadState := rsRead;
    NewMessage.Title := '';
    NewMessage.UserId := Dialog.Person.Id;
    Dialog.Messages.Add(NewMessage);
  end;
end;

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
  UserIds := TStringList.Create;
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
  JSONUserResponse := DAO.Users.Get(HTTPClient, UserIds);
  UserResponse := (JSONUserResponse['response'] as TJSONArray);
  for i := 0 to UserResponse.Count - 1 do
  begin
    CurrentUser := (UserResponse[i] as TJSONObject);
    NewUser := TUser.Create;
    NewUser.Id := CurrentUser['id'].AsString;
    NewUser.FirstName := CurrentUser['first_name'].AsString;
    NewUser.LastName := CurrentUser['last_name'].AsString;
    NewUser.Photo50 := DAO.LoadPhoto(HTTPClient, CurrentUser['photo_50'].AsString);
    NewUser.Photo200 := DAO.LoadPhoto(HTTPClient, CurrentUser['photo_200'].AsString);
    NewDialog := TDialog.Create;
    NewDialog.Person := NewUser;
    LoadMessagesIntoDialog(NewDialog, Community.AccessKey);
    Result.Add(NewDialog);
  end;
  FreeAndNil(UserIds);
end;

procedure TChatModel.SendMessage(Community: TCommunity; Message: TMessage);
begin
  if Message.Message = '' then
    exit;
  DAO.Messages.Send(HTTPClient, Community.AccessKey, Message.UserId, Message.Message);
end;

destructor TChatModel.Destroy;
begin
  FreeAndNil(HTTPClient);
  inherited Destroy;
end;

end.
