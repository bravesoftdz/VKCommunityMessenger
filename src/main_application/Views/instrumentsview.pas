unit instrumentsview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, entities;

type

  { TInstrumentsFrame }

  TInstrumentsFrame = class(TFrame)
    BitBtn1: TBitBtn;
    CreateChatbotButton: TBitBtn;
    CaptionLabel: TLabel;
  private
    FCommunity: TCommunity;
    procedure SetCommunity(AValue: TCommunity);
    { private declarations }
  public
    procedure InitializeFrame;
    property Community: TCommunity read FCommunity write SetCommunity;
  end;

  var LInstrumentsView: TInstrumentsFrame;

implementation

{$R *.lfm}

{ TInstrumentsFrame }

procedure TInstrumentsFrame.SetCommunity(AValue: TCommunity);
begin
  if FCommunity=AValue then Exit;
  FCommunity:=AValue;
end;

procedure TInstrumentsFrame.InitializeFrame;
begin

end;

end.

