unit ChatView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, vkgschat;

type

  { TChatFrameView }

  TChatFrameView = class(TFrame)
    ChatView: TVKGSChat;
    Panel1: TPanel;
    TabControl: TTabControl;
  private
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  LChatView: TChatFrameView;

implementation

{$R *.lfm}

{ TChatFrameView }

constructor TChatFrameView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ChatView:=TVKGSChat.Create(Self);
end;

destructor TChatFrameView.Destroy;
begin
  FreeAndnil(ChatView);
  inherited Destroy;
end;

end.
