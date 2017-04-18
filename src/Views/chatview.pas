unit ChatView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, vkgschat,
  Graphics, StdCtrls, Buttons, chatviewmodel, entities;

type

  { TChatFrameView }

  TChatFrameView = class(TFrame)
    SendButton: TBitBtn;
    Chat: TVKGSChat;
    ChatPanel: TPanel;
    ChatMemo: TMemo;
    Memopanel: TPanel;
    TabControl: TTabControl;
  private
    TabUsers: TUserList;
    FCommunity: TCommunity;
    FViewModel: IChatViewModel;
    procedure SetCommunity(AValue: TCommunity);
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

procedure TChatFrameView.SetCommunity(AValue: TCommunity);
begin
  if FCommunity = AValue then
    Exit;
  FCommunity := AValue;
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
  TabControl.Tabs.Add('Написать новое сообщение');
  TabControl.TabIndex := 0;
  TabControl.EndUpdate;

  LoadUserMessages(TabUsers[0]);
end;

procedure TChatFrameView.InitializeFrame;
begin
  SendButton.Caption := ViewModel.GetSendButtonCaption;
  SendButton.Glyph := ViewModel.GetSendButtonGlyph;
end;

procedure TChatFrameView.LoadUserMessages(User: TUser);
begin
  Chat.Messages:=ViewModel.GetLastMessages(Community,User);
end;

end.
