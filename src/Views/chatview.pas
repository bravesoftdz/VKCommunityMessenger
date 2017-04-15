unit ChatView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, vkgschat, Graphics, chatviewmodel;

type

  { TChatFrameView }

  TChatFrameView = class(TFrame)
    ChatView: TVKGSChat;
    ChatPanel: TPanel;
    TabControl: TTabControl;
  private
    FViewModel: IChatViewModel;
    procedure SetViewModel(AValue: IChatViewModel);
    { private declarations }
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ViewModel:IChatViewModel read FViewModel write SetViewModel;
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
  ChatView.BoxBorder:=2;
  ChatView.DistanceBetweenMessages:=10;
  ChatView.BoxColor:=RGBToColor(93,139,201);
  ChatView.FrameColor:=clWhite;
  ChatView.Overlapping:=40;
  ChatView.PaddingLeft:=30;
  ChatView.PaddingRight:=30;
  ChatView.PaddingBottom:=10;
  ChatView.Font.Color:=clWhite;
  ChatView.Font.Name:='Segoe UI';
  ChatView.Font.Size:=12;
  ChatView.Messages.Add(TMessage.Create);
  ChatView.Messages[0].Left:=true;
  ChatView.Messages[0].Message:='Здравствуйте, Иван! '
  +'Мы прислушались к Вашей просьбе и решили что-то сделать, что понравится Вам! '
  +'От лица всей фирмы ООО "Пупкин" мы дарим вам танк, который поможет Вам быстрее '
  +'передвигаться по городу в условиях пробок! За этот невероятный подарок Вам '
  +'следует поблагодарить Дартаньяна!' + #13#10
  +'Он ждет Вас :)';
  ChatView.Parent:=ChatPanel;
end;

destructor TChatFrameView.Destroy;
begin
  FreeAndnil(ChatView);
  inherited Destroy;
end;

end.
