unit ChatView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, vkgschat,
  Graphics, StdCtrls, Buttons, chatviewmodel, entities, Dialogs;

type

  { TChatFrameView }

  TChatFrameView = class(TFrame)
    ExpandMenuImage: TImage;
    UserAvatar: TImage;
    SettingsButton: TImage;
    RightMenu: TPanel;
    SendButton: TBitBtn;
    Chat: TVKGSChat;
    ChatPanel: TPanel;
    ChatMemo: TMemo;
    Memopanel: TPanel;
    TabControl: TTabControl;
    procedure ExpandMenuImageClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
  private
    FRightMenuExpanded: boolean;
    TabUsers: TUserList;
    FCommunity: TCommunity;
    FViewModel: IChatViewModel;
    procedure SetCommunity(AValue: TCommunity);
    procedure SetRightMenuExpanded(AValue: boolean);
    procedure SetViewModel(AValue: IChatViewModel);
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ViewModel: IChatViewModel read FViewModel write SetViewModel;
    property Community: TCommunity read FCommunity write SetCommunity;
    procedure UpdateGUI;
    procedure InitializeFrame;
    procedure LoadUserMessages(User: TUser);
    procedure OpenNewDialog;
    property RightMenuExpanded: boolean read FRightMenuExpanded write SetRightMenuExpanded;
  end;

var
  LChatView: TChatFrameView;

implementation

{$R *.lfm}

{ TChatFrameView }

procedure TChatFrameView.SetViewModel(AValue: IChatViewModel);
begin
  if FViewModel = AValue then
    Exit;
  FViewModel := AValue;
end;

procedure TChatFrameView.SendButtonClick(Sender: TObject);
var
  NewMessage: TMessage;
  SelectedUser: TUser;
begin
  if TabUsers.Count<1 then exit;
  SelectedUser := TabUsers[TabControl.TabIndex];
  NewMessage := TMessage.Create;
  NewMessage.Message := ChatMemo.Text;
  ViewModel.SendMessage(SelectedUser, NewMessage);
  UpdateGUI;
end;

procedure TChatFrameView.ExpandMenuImageClick(Sender: TObject);
begin
  RightMenuExpanded:=not RightMenuExpanded;
end;

procedure TChatFrameView.TabControlChange(Sender: TObject);
var
  SelectedUser: TUser;
begin
  if TabControl.TabIndex = TabUsers.Count then
  begin
    OpenNewDialog;
    TabControl.TabIndex := 0;
  end
  else
  begin
    try
      SelectedUser := TabUsers[TabControl.TabIndex];
      LoadUserMessages(SelectedUser);
    except
      ShowMessage('Ошибка приложения (несоответствие индексов вкладок и количества пользователей)');
      UpdateGUI;
    end;
  end;
end;

procedure TChatFrameView.SetCommunity(AValue: TCommunity);
begin
  if FCommunity = AValue then
    Exit;
  FCommunity := AValue;
end;

procedure TChatFrameView.SetRightMenuExpanded(AValue: boolean);
begin
  if FRightMenuExpanded=AValue then Exit;
  FRightMenuExpanded:=AValue;
  if FRightMenuExpanded then
     RightMenu.Width:=ChatPanel.Width-300
  else
     RightMenu.Width := 82;
end;

constructor TChatFrameView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Chat := TVKGSChat.Create(Self);
  Chat.Align := alClient;
  Chat.BoxBorder := 10;
  Chat.DistanceBetweenMessages := 20;
  Chat.BoxColor := RGBToColor(93, 139, 201);
  Chat.FrameColor := clWhite;
  Chat.Overlapping := 40;
  Chat.PaddingLeft := 30;
  Chat.PaddingRight := 30;
  Chat.PaddingBottom := 20;
  Chat.Font.Color := clWhite;
  Chat.Font.Name := 'Segoe UI';
  Chat.Font.Size := 12;
  Chat.Parent := ChatPanel;
end;

destructor TChatFrameView.Destroy;
begin
  FreeAndNil(Chat);
  inherited Destroy;
end;

procedure TChatFrameView.UpdateGUI;
var
  i: integer;
  NewTab: string;
  User: TUser;
begin
  TabControl.Tabs.Clear;
  if Assigned(TabUsers) then
    FreeAndNil(TabUsers);

  TabUsers := ViewModel.GetUsersForTabs(Community);
  TabControl.BeginUpdate;
  for i := 0 to TabUsers.Count - 1 do
  begin
    User := TabUsers[i];
    NewTab := User.FirstName + ' ' + User.LastName;
    TabControl.Tabs.Add(NewTab);
  end;
  {"Write new message" tab}
  TabControl.Tabs.Add('Открыть новый диалог');
  TabControl.TabIndex := 0;
  TabControl.EndUpdate;

  if TabUsers.Count > 0 then
    LoadUserMessages(TabUsers[0]);
end;

procedure TChatFrameView.InitializeFrame;
begin
  SendButton.Caption := ViewModel.GetSendButtonCaption;
  SendButton.Glyph := ViewModel.GetSendButtonGlyph;
  RightMenuExpanded:=false;
end;

procedure TChatFrameView.LoadUserMessages(User: TUser);
begin
  Chat.Messages := ViewModel.GetLastMessages(Community, User);
  if Assigned(User.Photo) then
     UserAvatar.Picture:=User.Photo
  else
     UserAvatar.Picture := ViewModel.GetNoAvatarImage;
end;

procedure TChatFrameView.OpenNewDialog;
begin
  ShowMessage('Not ready');
end;

end.
