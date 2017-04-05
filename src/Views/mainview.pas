unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, MainViewModel, IdKeyDialog;

type

  { TfMainView }

  TfMainView = class(TForm)
    CommunitiesImageList: TImageList;
    ToolBar1: TToolBar;
    procedure ChangeWorkspaceExecute(Sender: TObject);
    procedure PrepareWorkspaceForCommunity(Sender: TObject);
  private
    FViewModel: IMainViewModel;
    procedure SetViewModel(AValue: IMainViewModel);
  public
    property ViewModel: IMainViewModel read FViewModel write SetViewModel;
    procedure UpdateGUI;
    {Creates a button for adding communities}
    procedure AddNewCommunityButton;
    {Event of "Add new community" button}
    procedure AddNewCommunity(Sender: TObject);
  end;

var
  LMainView: TfMainView;

implementation

{$R *.lfm}

{ TfMainView }

procedure TfMainView.PrepareWorkspaceForCommunity(Sender: TObject);
begin

end;

procedure TfMainView.ChangeWorkspaceExecute(Sender: TObject);
begin

end;

procedure TfMainView.SetViewModel(AValue: IMainViewModel);
begin
  if FViewModel = AValue then
    Exit;
  FViewModel := AValue;
end;

procedure TfMainView.UpdateGUI;
var
  i: integer;
  NewButton: TToolButton;
begin
  CommunitiesImageList.Clear;
  ToolBar1.ButtonList.Clear;
  FViewModel.FillImageCommunitiesList(CommunitiesImageList);
  {1, becuase index 0 stands for new community icon}
  for i := 1 to CommunitiesImageList.Count - 1 do
  begin
    NewButton := TToolButton.Create(ToolBar1);
    NewButton.ImageIndex := i;
    NewButton.Parent := ToolBar1;
  end;
  AddNewCommunityButton;
end;

procedure TfMainView.AddNewCommunityButton;
var
  NewButton: TToolButton;
begin
  NewButton := TToolButton.Create(Toolbar1);
  NewButton.ImageIndex := 0;
  NewButton.Parent := ToolBar1;
  NewButton.OnClick := @AddNewCommunity;
end;

procedure TfMainView.AddNewCommunity(Sender: TObject);
var
  Id, AccessKey: string;
begin
  Dialog.ShowModal;
  if not Dialog.Done then exit;
  Id := Dialog.Id;
  AccessKey := Dialog.AccessKey;
  try
    ViewModel.SaveNewCommunity(AccessKey, Id);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.
