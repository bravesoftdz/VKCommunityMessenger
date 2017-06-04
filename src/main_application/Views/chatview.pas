unit ChatView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, vkcmchat,
  Graphics, StdCtrls, Buttons, chatviewmodel, entities, Dialogs, vkcmobserver;

type

  { TChatFrameView }

  TChatFrameView = class(TFrame)
    SendButton: TSpeedButton;
    ExpandButton: TSpeedButton;
    SettingsButton: TSpeedButton;
    UserAvatar: TImage;
    RightMenu: TPanel;
    Chat: TVKCMChat;
    ChatPanel: TPanel;
    ChatMemo: TMemo;
    Memopanel: TPanel;
    TabControl: TTabControl;
    procedure ExpandMenuImageClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
  private
    FObserver: TVKCMObserver;
    FRightMenuExpanded: boolean;
    TabDialogs: TDialogsList;
    FCommunity: TCommunity;
    FViewModel: IChatViewModel;
    ExpandPicture: TPicture;
    HidePicture: TPicture;
    procedure SetCommunity(AValue: TCommunity);
    procedure SetObserver(AValue: TVKCMObserver);
    procedure SetRightMenuExpanded(AValue: boolean);
    procedure SetViewModel(AValue: IChatViewModel);
    procedure OnNotify;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ViewModel: IChatViewModel read FViewModel write SetViewModel;
    property Community: TCommunity read FCommunity write SetCommunity;
    procedure UpdateGUI;
    procedure InitializeFrame;
    procedure LoadMessages(Dialog: TDialog);
    procedure OpenNewDialog;
    property RightMenuExpanded: boolean read FRightMenuExpanded
      write SetRightMenuExpanded;
    property Observer: TVKCMObserver read FObserver write SetObserver;
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

procedure TChatFrameView.OnNotify;
begin
  UpdateGUI;
end;

procedure TChatFrameView.SendButtonClick(Sender: TObject);
var
  SelectedUser: TUser;
  SelectedDialog: TDialog;
  MessageText: string;
begin
  if TabDialogs.Count < 1 then
    exit;
  SelectedDialog := TabDialogs[TabControl.TabIndex];
  SelectedUser := SelectedDialog.Person;
  MessageText := ChatMemo.Text;
  ChatMemo.Clear;
  ChatMemo.Repaint;
  ViewModel.SendMessage(Community, SelectedUser, MessageText);
  TabControl.TabIndex := 0; {Our dialog will appear first}
end;

procedure TChatFrameView.SettingsButtonClick(Sender: TObject);
begin
  ShowMessage('We have no settings');
end;

procedure TChatFrameView.ExpandMenuImageClick(Sender: TObject);
begin
  RightMenuExpanded := not RightMenuExpanded;
end;

procedure TChatFrameView.TabControlChange(Sender: TObject);
var
  SelectedDialog: TDialog;
begin
  if TabControl.TabIndex = TabDialogs.Count then
  begin
    OpenNewDialog;
    TabControl.TabIndex := 0;
  end
  else
  begin
    try
      SelectedDialog := TabDialogs[TabControl.TabIndex];
      LoadMessages(SelectedDialog);
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
  FreeAndNil(FCommunity);
  FCommunity := AValue.Copy;
end;

procedure TChatFrameView.SetObserver(AValue: TVKCMObserver);
begin
  if FObserver = AValue then
    Exit;
  FObserver := AValue;
end;

procedure TChatFrameView.SetRightMenuExpanded(AValue: boolean);
begin
  FRightMenuExpanded := AValue;
  if FRightMenuExpanded then
  begin
    RightMenu.Width := ChatPanel.Width - 300;
    ExpandButton.Glyph := HidePicture.Bitmap;
  end
  else
  begin
    RightMenu.Width := 82;
    ExpandButton.Glyph := ExpandPicture.Bitmap;
  end;
end;

constructor TChatFrameView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Observer := TVKCMObserver.Create;
  Observer.Notify := @OnNotify;
  Chat := TVKCMChat.Create(Self);
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
  TabName: string;
  NewTabIndex: integer;
begin
  SendButton.Enabled := False;

  try
    if Assigned(TabDialogs) then
      FreeAndNil(TabDialogs);

    TabDialogs := ViewModel.GetDialogs(Community);

    TabControl.BeginUpdate;
    TabName := TabControl.Tabs[TabControl.TabIndex];
    TabControl.Tabs.Clear;
    for i := 0 to TabDialogs.Count - 1 do
    begin
      User := TabDialogs[i].Person;
      NewTab := User.FirstName + ' ' + User.LastName;
      TabControl.Tabs.Add(NewTab);
    end;
    {"Write new message" tab}
    TabControl.Tabs.Add('Открыть новый диалог');
    NewTabIndex := TabControl.IndexOfTabWithCaption(TabName);
    if (NewTabIndex >= 0) and (NewTabIndex < TabControl.Tabs.Count - 1) then
      TabControl.TabIndex := NewTabIndex
    else
      TabControl.TabIndex := 0;
    TabControl.EndUpdate;

    if TabDialogs.Count > 0 then
      LoadMessages(TabDialogs[0]);
  finally
    SendButton.Enabled := True;
  end;
end;

procedure TChatFrameView.InitializeFrame;
begin
  SendButton.Glyph := ViewModel.GetSendButtonGlyph;
  ExpandPicture := ViewModel.GetExpandPicture;
  HidePicture := ViewModel.GetHidePicture;
  RightMenuExpanded := False;
  SettingsButton.Glyph := ViewModel.GetSettingsPicture.Bitmap;
end;

procedure TChatFrameView.LoadMessages(Dialog: TDialog);
var
  User: TUser;
  i: integer;
  NewUIMessage: TUIMessage;
begin
  Chat.Messages.Clear;
  for i := 0 to Dialog.Messages.Count - 1 do
  begin
    NewUIMessage := TUIMessage.Create;
    NewUIMessage.Id := Dialog.Messages[i].Id;
    NewUIMessage.Message := Dialog.Messages[i].Message;
    NewUIMessage.FromId := Dialog.Messages[i].FromId;
    NewUIMessage.Emoji := Dialog.Messages[i].Emoji;
    NewUIMessage.Deleted := Dialog.Messages[i].Deleted;
    NewUIMessage.Date := Dialog.Messages[i].Date;
    NewUIMessage.Out := Dialog.Messages[i].Out;
    NewUIMessage.ReadState := Dialog.Messages[i].ReadState;
    NewUIMessage.Title := Dialog.Messages[i].Title;
    NewUIMessage.UserId := Dialog.Messages[i].UserId;
    Chat.Messages.Add(NewUIMessage);
  end;
  Chat.Repaint;
  User := Dialog.Person;
  if Assigned(User.Photo50) then
    UserAvatar.Picture := User.Photo50
  else
    UserAvatar.Picture := ViewModel.GetNoAvatarImage;
end;

procedure TChatFrameView.OpenNewDialog;
begin
  ShowMessage('Not ready');
end;

end.
