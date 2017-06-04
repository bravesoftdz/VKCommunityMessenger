unit chatviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, AbstractModel, ChatModel,
  entities, Graphics, vkcmchat, Dialogs;

type

  { IChatViewModel }

  IChatViewModel = interface(IViewModel)
    function GetSendButtonCaption: string;
    {Get users which have to be assigned to tabs in tabview}
    function GetDialogs(Community: TCommunity): TDialogsList;
    function GetSendButtonGlyph: TBitmap;
    function GetLastMessages(Community: TCommunity; User: TUser): TUIMessagesObjectList;
    {Send message to a user}
    procedure SendMessage(Community: TCommunity; User: TUser; Message: string);
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
    function GetDialogs(Community: TCommunity): TDialogsList;
    function GetSendButtonGlyph: TBitmap;
    function GetLastMessages(Community: TCommunity; User: TUser): TUIMessagesObjectList;
    procedure SendMessage(Community: TCommunity; User: TUser; MessageText: string);
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

function TChatViewModel.GetDialogs(Community: TCommunity): TDialogsList;
begin
  Result := (Model as IChatModel).GetLastDialogs(Community);
end;

function TChatViewModel.GetSendButtonGlyph: TBitmap;
begin
  Result := (Model as IChatModel).GetSendPicture.Bitmap;
end;

function TChatViewModel.GetLastMessages(Community: TCommunity;
  User: TUser): TUIMessagesObjectList;
begin
  Result := TUIMessagesObjectList.Create(true);
end;

procedure TChatViewModel.SendMessage(Community: TCommunity; User: TUser; MessageText: string);
var Message: TMessage;
begin
  Message:=TMessage.Create;
  Message.Date:=Now;
  Message.Deleted:=false;
  Message.Emoji:=false;
  Message.Out:=otSent;
  Message.UserId:=User.Id;
  Message.Title:='';
  Message.ReadState:=rsUnread;
  Message.FromId:=Community.Id;
  Message.Message := MessageText;
  (Model as IChatModel).SendMessage(Community,Message);
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
