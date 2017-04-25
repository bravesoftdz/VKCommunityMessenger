unit chatviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, AbstractModel, ChatModel,
  entities, Graphics, vkgschat, Dialogs;

type

  { IChatViewModel }

  IChatViewModel = interface(IViewModel)
    function GetSendButtonCaption: string;
    {Get users which have to be assigned to tabs in tabview}
    function GetUsersForTabs(Community: TCommunity): TUserList;
    function GetUserById(Id: string): TUser;
    function GetSendButtonGlyph: TBitmap;
    function GetLastMessages(Community: TCommunity; User: TUser): TUIMessagesList;
    {Send message to a user}
    procedure SendMessage(User: TUser; Message: TMessage);
    {Get image for users with no avatar}
    function GetNoAvatarImage: TPicture;
    {Expand/Hide button images}
    function GetHidePicture: TPicture;
    function GetExpandPicture: TPicture;
    function GetSettingsPicture: TPicture;
  end;

  { TChatViewModel }

  TChatViewModel = class(TInterfacedObject, IViewModel, IChatViewModel)
  private
    FModel: IModel;
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
  public
    property Model: IModel read GetModel write SetModel;
    function GetSendButtonCaption: string;
    function GetUsersForTabs(Community: TCommunity): TUserList;
    function GetUserById(Id: string): TUser;
    function GetSendButtonGlyph: TBitmap;
    function GetLastMessages(Community: TCommunity; User: TUser): TUIMessagesList;
    procedure SendMessage(User: TUser; Message: TMessage);
    function GetNoAvatarImage: TPicture;
    function GetHidePicture: TPicture;
    function GetExpandPicture: TPicture;
    function GetSettingsPicture: TPicture;
  end;

var
  LChatViewModel: TChatViewModel;

implementation

{ TChatViewModel }

function TChatViewModel.GetModel: IModel;
begin
  Result := FModel;
end;

procedure TChatViewModel.SetModel(AValue: IModel);
begin
  if AValue <> FModel then
    FModel := AValue;
end;

function TChatViewModel.GetSendButtonCaption: string;
begin
  Result := 'Отправить';
end;

function TChatViewModel.GetUsersForTabs(Community: TCommunity): TUserList;
var
  DialogsList: TDialogsList;
  i: integer;
  Dialog: TDialog;
begin
  Result := TUserList.Create;
  DialogsList := (Model as IChatModel).GetLastDialogs;
  for i := 0 to DialogsList.Count - 1 do
  begin
    Dialog := DialogsList[i];
    Result.Add(Dialog.Person);
  end;
end;

function TChatViewModel.GetUserById(Id: string): TUser;
begin

end;

function TChatViewModel.GetSendButtonGlyph: TBitmap;
begin
  Result := (Model as IChatModel).GetSendPicture.Bitmap;
end;

function TChatViewModel.GetLastMessages(Community: TCommunity;
  User: TUser): TUIMessagesList;
var
  NewMessage: TUIMessage;
begin
  Result := TUIMessagesList.Create(True);

  NewMessage := TUIMessage.Create;
  NewMessage.Message := 'Привет!';
  NewMessage.Out := otRecieved;
  Result.Add(NewMessage);

  NewMessage := TUIMessage.Create;
  NewMessage.Message := 'Ну привет!';
  NewMessage.Out := otSent;
  Result.Add(NewMessage);
end;

procedure TChatViewModel.SendMessage(User: TUser; Message: TMessage);
begin
  ShowMessage('Отправлено :)');
end;

function TChatViewModel.GetNoAvatarImage: TPicture;
begin
  Result := (Model as IChatModel).GetNoAvatarPicture;
end;

function TChatViewModel.GetHidePicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\hide.png');
end;

function TChatViewModel.GetExpandPicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\expand.png');
end;

function TChatViewModel.GetSettingsPicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\settings.png');
end;

end.
