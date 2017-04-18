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
    ChatView: TVKGSChat;
    ChatPanel: TPanel;
    ChatMemo: TMemo;
    Memopanel: TPanel;
    TabControl: TTabControl;
  private
    FCommunity: TCommunity;
    FViewModel: IChatViewModel;
    procedure SetCommunity(AValue: TCommunity);
    procedure SetViewModel(AValue: IChatViewModel);
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ViewModel:IChatViewModel read FViewModel write SetViewModel;
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
  if FViewModel=AValue then Exit;
  FViewModel:=AValue;
end;

procedure TChatFrameView.SetCommunity(AValue: TCommunity);
begin
  if FCommunity=AValue then Exit;
  FCommunity:=AValue;
end;

constructor TChatFrameView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ChatView:=TVKGSChat.Create(Self);
  ChatView.Align:=alClient;
  ChatView.BoxBorder:=10;
  ChatView.DistanceBetweenMessages:=20;
  ChatView.BoxColor:=RGBToColor(93,139,201);
  ChatView.FrameColor:=clWhite;
  ChatView.Overlapping:=40;
  ChatView.PaddingLeft:=30;
  ChatView.PaddingRight:=30;
  ChatView.PaddingBottom:=20;
  ChatView.Font.Color:=clWhite;
  ChatView.Font.Name:='Segoe UI';
  ChatView.Font.Size:=12;
  ChatView.Parent:=ChatPanel;
end;

destructor TChatFrameView.Destroy;
begin
  FreeAndnil(ChatView);
  inherited Destroy;
end;

procedure TChatFrameView.UpdateGUI;
var TabUsers: TUserList;
    i: integer;
    NewTab: string;
    User: TUser;
begin
  TabControl.Tabs.Clear;

  TabUsers := ViewModel.GetUsersForTabs(Community);
  TabControl.BeginUpdate;
  for i:=0 to TabUsers.Count-1 do
  begin
    User := TabUsers[i];
    NewTab:= User.FirstName + ' ' + User.LastName;
    TabControl.Tabs.Add(NewTab);
  end;
  {"Write new message" tab}
  TabControl.TabIndex:=0;
  TabControl.EndUpdate;
end;

procedure TChatFrameView.InitializeFrame;
begin
  SendButton.Caption:=ViewModel.GetSendButtonCaption;
  SendButton.Glyph:=ViewModel.GetSendButtonGlyph;
end;

procedure TChatFrameView.LoadUserMessages(User: TUser);
begin

end;

end.
