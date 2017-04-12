unit vkgschat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fgl, LCLIntf;

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
  published
    property Messages: TMessegesList read FMessages write SetMessages;
  end;

implementation

{ TMessage }

procedure TMessage.SetMessage(AValue: string);
begin
  if FMessage=AValue then Exit;
  FMessage:=AValue;
end;

procedure TMessage.SetLeft(AValue: boolean);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
end;

{ TVKGSChat }

procedure TVKGSChat.SetMessages(AValue: TMessegesList);
begin
  if FMessages=AValue then Exit;
  FMessages:=AValue;
end;

procedure TVKGSChat.Paint;
var LeftBorder, RightBorder, Center, BottomBorder, i, Overlapping: integer;
    Message: TMessage;
begin
  inherited Paint;

  Overlapping := 20;
  LeftBorder:=30;
  RightBorder:=Canvas.Width-30;
  Center := Trunc((RightBorder - LeftBorder)/2);
  BottomBorder :=0

  for i:=Messages.Count-1 downto 0 do
  begin


  end;

end;

end.

