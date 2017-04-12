unit vkgschat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fgl, LCLIntf, LCLType, Graphics;

type

  { TMessage }

  TMessage = class
  private
    FLeft: boolean;
    FMessage: string;
    procedure SetLeft(AValue: boolean);
    procedure SetMessage(AValue: string);
  published
    property Left: boolean read FLeft write SetLeft;
    property Message: string read FMessage write SetMessage;
  end;

  TMessegesList = specialize TFPGObjectList<TMessage>;

  { TVKGSChat }

  TVKGSChat = class(TCustomControl)
  private
    FMessages: TMessegesList;
    procedure SetMessages(AValue: TMessegesList);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Messages: TMessegesList read FMessages write SetMessages;
  end;

implementation

{ TMessage }

procedure TMessage.SetMessage(AValue: string);
begin
  if FMessage = AValue then
    Exit;
  FMessage := AValue;
end;

procedure TMessage.SetLeft(AValue: boolean);
begin
  if FLeft = AValue then
    Exit;
  FLeft := AValue;
end;

{ TVKGSChat }

procedure TVKGSChat.SetMessages(AValue: TMessegesList);
begin
  if FMessages = AValue then
    Exit;
  FMessages := AValue;
end;

procedure TVKGSChat.Paint;
var
  LeftBorder, RightBorder, LeftCenter, RightCenter: integer;
  i: integer;
  BottomLine, TopLine: integer;
  DistanceBetween: integer;
  BottomPadding: integer;
  Message: TMessage;
  LRect: TRect;
begin
  inherited Paint;

  Canvas.Brush.Color := clBlue;
  Canvas.Pen.Color := clWhite;
  Canvas.Font.Color:=clWhite;

  LeftBorder := 30;
  DistanceBetween := 10;
  BottomPadding := 10;
  RightBorder := Canvas.Width - 30;
  LeftCenter := Trunc((RightBorder - LeftBorder) / 2) - 30;
  RightCenter := LeftCenter + 60;

  BottomLine := Canvas.Height - BottomPadding;

  for i := Messages.Count - 1 downto 0 do
  begin
    Message := Messages[i];
    LRect := Rect(LeftBorder, BottomLine - Canvas.TextHeight('A'), RightCenter, BottomLine);
    DrawText(Canvas.Handle, PChar(Message.Message), Message.Message.Length,
      LRect, DT_CALCRECT or DT_WORDBREAK);
    Topline := BottomLine - LRect.Height;
    LRect.Top := Topline;
    LRect.Bottom := BottomLine;
    Canvas.Rectangle(LRect);
    DrawText(Canvas.Handle, PChar(Message.Message), Message.Message.Length,
      LRect, DT_WORDBREAK);
    BottomLine := TopLine - DistanceBetween;
    if BottomLine < 0 then
      exit;
  end;

end;

constructor TVKGSChat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessages := TMessegesList.Create(True);
end;

destructor TVKGSChat.Destroy;
begin
  FreeAndNil(FMessages);
end;

end.
