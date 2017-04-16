unit ChatView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, vkgschat,
  Graphics, StdCtrls, Buttons, chatviewmodel;

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
    FViewModel: IChatViewModel;
    procedure SetViewModel(AValue: IChatViewModel);
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ViewModel:IChatViewModel read FViewModel write SetViewModel;
    procedure UpdateGUI;
    procedure InitializeFrame;
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
begin
  TabControl.Tabs.Clear;

end;

procedure TChatFrameView.InitializeFrame;
begin
  SendButton.Caption:=ViewModel.GetSendButtonCaption;
end;

end.
