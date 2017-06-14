unit instrumentsview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, entities,
  Dialogs, ExtCtrls;

type

  { TInstrumentsFrame }

  TInstrumentsFrame = class(TFrame)
    BitBtn1: TBitBtn;
    CreateChatbotButton: TBitBtn;
    CaptionLabel: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure CreateChatbotButtonClick(Sender: TObject);
  private
    FCommunity: TCommunity;
    procedure SetCommunity(AValue: TCommunity);
    { private declarations }
  public
    procedure InitializeFrame;
    property Community: TCommunity read FCommunity write SetCommunity;
  end;

var
  LInstrumentsView: TInstrumentsFrame;

implementation

{$R *.lfm}

{ TInstrumentsFrame }

procedure TInstrumentsFrame.CreateChatbotButtonClick(Sender: TObject);
begin
  ShowMessage('Not ready yet');
end;

procedure TInstrumentsFrame.BitBtn1Click(Sender: TObject);
begin
  ShowMessage('Not ready yet');
end;

procedure TInstrumentsFrame.SetCommunity(AValue: TCommunity);
begin
  if FCommunity = AValue then
    Exit;
  FCommunity := AValue;
end;

procedure TInstrumentsFrame.InitializeFrame;
begin

end;

end.
