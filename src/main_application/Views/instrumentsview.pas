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
  Community.Chatbot.AddCommand('//newcommand//', '');
  UpdateGUI;
end;

procedure TInstrumentsFrame.DeleteCommandButtonClick(Sender: TObject);
var
  i: integer;
  CommandsToRemove: TChatBotCommandsObjectList;
begin
  CommandsToRemove := TChatBotCommandsObjectList.Create(False);
  try
    for i := 0 to CommandListBox.Count - 1 do
    begin
      if CommandListBox.Selected[i] then
      begin
        CommandsToRemove.Add(CommandListBox.Items.Objects[i] as TChatBotCommand);
      end;
    end;
    for i := 0 to CommandsToRemove.Count - 1 do
    begin
      Community.Chatbot.Commands.Remove(CommandsToRemove[i]);
    end;
  finally
    FreeAndNil(CommandsToRemove);
  end;
  UpdateGUI;
end;

procedure TInstrumentsFrame.SaveAnswerButtonClick(Sender: TObject);
begin

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
var
  i: integer;
  SelectedIndex: integer;
begin
  if CommandListBox.Count = 0 then
    SelectedIndex := -1
  else
    SelectedIndex := 0;
  for i := 0 to CommandListBox.Count - 1 do
    if CommandListBox.Selected[i] then
    begin
      SelectedIndex := i;
      break;
    end;
  CommandListBox.Clear;
  for i := 0 to Community.Chatbot.Commands.Count - 1 do
  begin
    CommandListBox.AddItem(Community.Chatbot.Commands[i].Command,
      Community.Chatbot.Commands[i]);
  end;
  //Sometimes after deletion SelectedIndex can be bigger as Count
  if (SelectedIndex <> -1) and (SelectedIndex<CommandListBox.Count) then
    CommandListBox.Selected[SelectedIndex] := True;
end;

end.
