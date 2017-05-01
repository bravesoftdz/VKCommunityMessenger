unit vkgschat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fgl, LCLIntf, LCLType, Graphics, entities;

type

  { TUIMessage }

  TUIMessage = class(TMessage)
  private
    function GetLeft: boolean;
  public
    property Left: boolean read GetLeft;
  end;

  TUIMessagesObjectList = specialize TFPGObjectList<TUIMessage>;

  { TVKGSChat }

  TVKGSChat = class(TCustomControl)
  private
    FBoxBorder: integer;
    FBoxColor: TColor;
    FDistanceBetweenMessages: integer;
    FFrameColor: TColor;
    FMessages: TUIMessagesObjectList;
    FOverlapping: integer;
    FPaddingBottom: integer;
    FPaddingLeft: integer;
    FPaddingRight: integer;
    procedure SetBoxBorder(AValue: integer);
    procedure SetBoxColor(AValue: TColor);
    procedure SetDistanceBetweenMessages(AValue: integer);
    procedure SetFrameColor(AValue: TColor);
    procedure SetMessages(AValue: TUIMessagesObjectList);
    procedure SetOverlapping(AValue: integer);
    procedure SetPaddingBottom(AValue: integer);
    procedure SetPaddingLeft(AValue: integer);
    procedure SetPaddingRight(AValue: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {Last message in list will be drawn most bottom}
    property Messages: TUIMessagesObjectList read FMessages write SetMessages;
    property PaddingLeft: integer read FPaddingLeft write SetPaddingLeft;
    property PaddingRight: integer read FPaddingRight write SetPaddingRight;
    property PaddingBottom: integer read FPaddingBottom write SetPaddingBottom;
    property Overlapping: integer read FOverlapping write SetOverlapping;
    property DistanceBetweenMessages: integer
      read FDistanceBetweenMessages write SetDistanceBetweenMessages;
    property BoxColor: TColor read FBoxColor write SetBoxColor;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property BoxBorder: integer read FBoxBorder write SetBoxBorder;
    property Font;
  end;

implementation

{ TUIMessage }

function TUIMessage.GetLeft: boolean;
begin
  Result := Out = otRecieved;
end;

{ TVKGSChat }

procedure TVKGSChat.SetMessages(AValue: TUIMessagesObjectList);
begin
  if FMessages=AValue then Exit;
  FreeAndNil(FMessages);
  FMessages:=AValue;
end;

procedure TVKGSChat.SetDistanceBetweenMessages(AValue: integer);
begin
  if FDistanceBetweenMessages = AValue then
    Exit;
  FDistanceBetweenMessages := AValue;
end;

procedure TVKGSChat.SetFrameColor(AValue: TColor);
begin
  if FFrameColor = AValue then
    Exit;
  FFrameColor := AValue;
end;

procedure TVKGSChat.SetBoxColor(AValue: TColor);
begin
  if FBoxColor = AValue then
    Exit;
  FBoxColor := AValue;
end;

procedure TVKGSChat.SetBoxBorder(AValue: integer);
begin
  if FBoxBorder = AValue then
    Exit;
  FBoxBorder := AValue;
end;

procedure TVKGSChat.SetOverlapping(AValue: integer);
begin
  if FOverlapping = AValue then
    Exit;
  FOverlapping := AValue;
end;

procedure TVKGSChat.SetPaddingBottom(AValue: integer);
begin
  if FPaddingBottom = AValue then
    Exit;
  FPaddingBottom := AValue;
end;

procedure TVKGSChat.SetPaddingLeft(AValue: integer);
begin
  if FPaddingLeft = AValue then
    Exit;
  FPaddingLeft := AValue;
end;

procedure TVKGSChat.SetPaddingRight(AValue: integer);
begin
  if FPaddingRight = AValue then
    Exit;
  FPaddingRight := AValue;
end;

procedure TVKGSChat.Paint;
var
  LeftBorder, RightBorder, LeftCenter, RightCenter: integer;
  i: integer;
  TopLine: integer;
  BottomLine: integer;
  Message: TUIMessage;
  LRect, LBox: TRect;
  Buf: integer;
begin
  inherited Paint;

  Canvas.Brush.Color := BoxColor;
  Canvas.Pen.Color := FrameColor;
  Canvas.Font := Font;

  LeftBorder := ClientRect.Left + PaddingLeft;
  RightBorder := ClientRect.Right - PaddingRight;
  LeftCenter := Trunc((RightBorder - PaddingLeft) / 2) - Overlapping;
  RightCenter := LeftCenter + 2 * Overlapping;

  BottomLine := ClientRect.Bottom - PaddingBottom;

  for i := Messages.Count - 1 downto 0 do
  begin
    Message := (Messages[i] as TUIMessage);

    {Find size of rectangle for text}
    LRect := Rect(LeftBorder, BottomLine - Canvas.TextHeight('A'),
      RightCenter, BottomLine);
    DrawText(Canvas.Handle, PChar(Message.Message), Message.Message.Length,
      LRect, DT_CALCRECT or DT_WORDBREAK);
    Topline := BottomLine - LRect.Height;
    LRect.Top := Topline;
    LRect.Bottom := BottomLine;
    if not Message.Left then
    begin
      Buf := LRect.Width;
      LRect.Left := RightBorder - Buf;
      LRect.Right := RightBorder;
    end;
    LBox.Left := LRect.Left - BoxBorder;
    LBox.Right := LRect.Right + BoxBorder;
    LBox.Top := LRect.Top - BoxBorder;
    LBox.Bottom := LRect.Bottom + BoxBorder;
    Canvas.Rectangle(Lbox);
    DrawText(Canvas.Handle, PChar(Message.Message), Message.Message.Length,
      LRect, DT_WORDBREAK);
    BottomLine := TopLine - DistanceBetweenMessages;
    if BottomLine < ClientRect.Top then
      exit;
  end;

end;

constructor TVKGSChat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessages := TUIMessagesObjectList.Create(True);
  DoubleBuffered:=true;
end;

destructor TVKGSChat.Destroy;
begin
  inherited;
  Messages.Free;
end;

end.
