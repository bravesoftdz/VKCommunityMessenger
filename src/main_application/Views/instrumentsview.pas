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
    CommandListBox: TListBox;
    AnswerMemo: TMemo;
    DeleteCommandButton: TBitBtn;
    AddCommandButton: TBitBtn;
    CaptionLabel: TLabel;
    procedure SaveAnswerButtonClick(Sender: TObject);
    procedure DeleteCommandButtonClick(Sender: TObject);
    procedure AddCommandButtonClick(Sender: TObject);
  private
    FCommunity: TCommunity;
    procedure SetCommunity(AValue: TCommunity);
    { private declarations }
  public
    procedure InitializeFrame;
    procedure UpdateGUI;
    property Community: TCommunity read FCommunity write SetCommunity;
  end;

var
  LInstrumentsView: TInstrumentsFrame;

implementation

{$R *.lfm}

{ TInstrumentsFrame }

procedure TInstrumentsFrame.AddCommandButtonClick(Sender: TObject);
begin
  Community.Chatbot.AddCommand('//newcommand//','');
  UpdateGUI;
end;

procedure TInstrumentsFrame.DeleteCommandButtonClick(Sender: TObject);
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
  UpdateGUI;
end;

procedure TInstrumentsFrame.UpdateGUI;
var i: integer;
begin
  CommandListBox.Clear;
  for i:=0 to Community.Chatbot.Commands.Count-1 do
  begin
    CommandListBox.AddItem(Community.Chatbot.Commands[i].Command,Community.Chatbot.Commands[i]);
  end;
end;

end.
