unit instrumentsview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, entities,
  Dialogs, ExtCtrls, Types, instrumentsviewmodel;

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
    procedure CommandListBoxDblClick(Sender: TObject);
    procedure CommandListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure SaveAnswerButtonClick(Sender: TObject);
    procedure DeleteCommandButtonClick(Sender: TObject);
    procedure AddCommandButtonClick(Sender: TObject);
    procedure EditDone(Sender: TObject);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EditKeyPress(Sender: TObject; var Key: char);
  private
    FViewModel: TInstrumentsViewModel;
    procedure SetViewModel(AValue: TInstrumentsViewModel);
  private
    FInPlaceEditor: TEdit;
    FCommunity: TCommunity;
    procedure SetInPlaceEditor(AValue: TEdit);
    procedure SetCommunity(AValue: TCommunity);
    property InPlaceEditor: TEdit read FInPlaceEditor write SetInPlaceEditor;
  public
    procedure InitializeFrame;
    procedure UpdateGUI;
    property Community: TCommunity read FCommunity write SetCommunity;
    property ViewModel: TInstrumentsViewModel read FViewModel write SetViewModel;
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

procedure TInstrumentsFrame.EditDone(Sender: TObject);
var
  EditSender: TEdit;
begin
  if (Sender is TEdit) then
    EditSender := Sender as TEdit
  else
    exit;
  (CommandListBox.Items.Objects[CommandListBox.ItemIndex] as TChatBotCommand).Command :=
    EditSender.Text;
  EditSender.Visible := False;
  UpdateGUI;
end;

procedure TInstrumentsFrame.EditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not PtInRect((Sender as TControl).ClientRect, Point(X, y)) then
    EditDone(Sender);
end;

procedure TInstrumentsFrame.EditKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
    EditDone(Sender);
end;

procedure TInstrumentsFrame.SetViewModel(AValue: TInstrumentsViewModel);
begin
  if FViewModel = AValue then
    Exit;
  FViewModel := AValue;
end;

procedure TInstrumentsFrame.SetInPlaceEditor(AValue: TEdit);
begin
  if FInPlaceEditor = AValue then
    Exit;
  FInPlaceEditor := AValue;
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
var
  SelectedCommand: TChatBotCommand;
begin
  if not ((CommandListBox.ItemIndex >= 0) and
    (CommandListBox.ItemIndex < CommandListBox.Count)) then
    exit;
  SelectedCommand := (CommandListBox.Items.Objects[CommandListBox.ItemIndex] as
    TChatBotCommand);
  SelectedCommand.Response := AnswerMemo.Text;
  UpdateGUI;
end;

procedure TInstrumentsFrame.CommandListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  SelectedCommand: TChatBotCommand;
begin
  SelectedCommand := (CommandListBox.Items.Objects[CommandListBox.ItemIndex] as
    TChatBotCommand);
  AnswerMemo.Text := SelectedCommand.Response;
end;

procedure TInstrumentsFrame.CommandListBoxDblClick(Sender: TObject);
var
  Rect: TRect;
  ListBox: TListbox;
  Edit: TEdit;
begin
  ListBox := (Sender as TListbox);
  if (ListBox.ItemIndex >= 0) and (ListBox.ItemIndex < ListBox.Count) then
  begin
    Edit := InPlaceEditor;
    Rect := ListBox.ItemRect(ListBox.ItemIndex);
    Rect.TopLeft := ListBox.ClientToScreen(Rect.TopLeft);
    Rect.BottomRight := ListBox.ClientToScreen(Rect.bottomright);
    Rect.TopLeft := ScreenToClient(Rect.TopLeft);
    Rect.BottomRight := ScreenToClient(Rect.BottomRight);
    Edit.Text := ListBox.Items[ListBox.ItemIndex];
    Edit.SetBounds(Rect.left, Rect.top - 2,
      ListBox.ClientWidth,
      Rect.bottom - Rect.top + 4);
    Edit.Visible := True;
    SetCapturecontrol(Edit);
    Edit.SetFocus;
  end;
end;

procedure TInstrumentsFrame.SetCommunity(AValue: TCommunity);
begin
  if FCommunity = AValue then
    Exit;
  FCommunity := AValue;
end;

procedure TInstrumentsFrame.InitializeFrame;
begin
  if not Assigned(InPlaceEditor) then
  begin
    InPlaceEditor := TEdit.Create(Self);
    InPlaceEditor.Visible := False;
    InPlaceEditor.OnExit := @EditDone;
    InPlaceEditor.OnMouseDown := @EditMouseDown;
    InPlaceEditor.OnKeyPress := @EditKeyPress;
    InPlaceEditor.Parent := Self;
  end;
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
  if (SelectedIndex <> -1) and (SelectedIndex < CommandListBox.Count) then
    CommandListBox.Selected[SelectedIndex] := True;
  if Assigned(Community) then
    ViewModel.UpdateCommunityForChatBotSubSystem(Community);
end;

end.
