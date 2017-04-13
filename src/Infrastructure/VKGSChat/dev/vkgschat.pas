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
    FBoxColor: TColor;
    FDistanceBetweenMessages: integer;
    FFrameColor: TColor;
    FMessages: TMessegesList;
    FOverlapping: integer;
    FPaddingBottom: integer;
    FPaddingLeft: integer;
    FPaddingRight: integer;
    procedure SetBoxColor(AValue: TColor);
    procedure SetDistanceBetweenMessages(AValue: integer);
    procedure SetFrameColor(AValue: TColor);
    procedure SetMessages(AValue: TMessegesList);
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
    property Messages: TMessegesList read FMessages write SetMessages;
    property PaddingLeft: integer read FPaddingLeft write SetPaddingLeft;
    property PaddingRight: integer read FPaddingRight write SetPaddingRight;
    property PaddingBottom: integer read FPaddingBottom write SetPaddingBottom;
    property Overlapping: integer read FOverlapping write SetOverlapping;
    property DistanceBetweenMessages: integer read FDistanceBetweenMessages write SetDistanceBetweenMessages;
    property BoxColor: TColor read FBoxColor write SetBoxColor;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property Font;
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
  Repaint;
end;

procedure TVKGSChat.SetDistanceBetweenMessages(AValue: integer);
begin
  if FDistanceBetweenMessages=AValue then Exit;
  FDistanceBetweenMessages:=AValue;
  Repaint;
end;

procedure TVKGSChat.SetFrameColor(AValue: TColor);
begin
  if FFrameColor=AValue then Exit;
  FFrameColor:=AValue;
  Repaint;
end;

procedure TVKGSChat.SetBoxColor(AValue: TColor);
begin
  if FBoxColor=AValue then Exit;
  FBoxColor:=AValue;
  Repaint;
end;

procedure TVKGSChat.SetOverlapping(AValue: integer);
begin
  if FOverlapping=AValue then Exit;
  FOverlapping:=AValue;
  Repaint;
end;

procedure TVKGSChat.SetPaddingBottom(AValue: integer);
begin
  if FPaddingBottom=AValue then Exit;
  FPaddingBottom:=AValue;
  Repaint;
end;

procedure TVKGSChat.SetPaddingLeft(AValue: integer);
begin
  if FPaddingLeft=AValue then Exit;
  FPaddingLeft:=AValue;
  Repaint;
end;

procedure TVKGSChat.SetPaddingRight(AValue: integer);
begin
  if FPaddingRight=AValue then Exit;
  FPaddingRight:=AValue;
  Repaint;
end;

procedure TVKGSChat.Paint;
var
  RightBorder, LeftCenter, RightCenter: integer;
  i: integer;
  TopLine: integer;
  BottomLine: integer;
  Message: TMessage;
  LRect: TRect;
begin
  inherited Paint;

  Canvas.Brush.Color := BoxColor;
  Canvas.Pen.Color := FrameColor;
  Canvas.Font:=Font;

  RightBorder := Canvas.Width - PaddingRight;
  LeftCenter := Trunc((RightBorder - PaddingLeft) / 2) - Overlapping;
  RightCenter := LeftCenter + 2*Overlapping;

  BottomLine := Canvas.Height - PaddingBottom;

  for i := Messages.Count - 1 downto 0 do
  begin
    Message := Messages[i];
    LRect := Rect(PaddingLeft, BottomLine - Canvas.TextHeight('A'), RightCenter, BottomLine);
    DrawText(Canvas.Handle, PChar(Message.Message), Message.Message.Length,
      LRect, DT_CALCRECT or DT_WORDBREAK);
    Topline := BottomLine - LRect.Height;
    LRect.Top := Topline;
    LRect.Bottom := BottomLine;
    Canvas.Rectangle(LRect);
    DrawText(Canvas.Handle, PChar(Message.Message), Message.Message.Length,
      LRect, DT_WORDBREAK);
    BottomLine := TopLine - DistanceBetweenMessages;
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
