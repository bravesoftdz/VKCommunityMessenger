unit instrumentsview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, entities,
  Dialogs, ExtCtrls;

type

  { TInstrumentsFrame }

  TInstrumentsFrame = class(TFrame)
    SaveAnswerButton: TButton;
    CommandsLabel: TLabel;
    Label1: TLabel;
    ListBox1: TListBox;
    AnswerMemo: TMemo;
    ShowChatbotsButton: TBitBtn;
    CreateChatbotButton: TBitBtn;
    CaptionLabel: TLabel;
    procedure SaveAnswerButtonClick(Sender: TObject);
    procedure ShowChatbotsButtonClick(Sender: TObject);
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

procedure TInstrumentsFrame.ShowChatbotsButtonClick(Sender: TObject);
begin
  ShowMessage('Not ready yet');
end;

procedure TInstrumentsFrame.SaveAnswerButtonClick(Sender: TObject);
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
