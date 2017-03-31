unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, MainViewModel, welcomepageview;

type

  { TfMainView }

  TfMainView = class(TForm)
    CommunitiesImageList: TImageList;
    Frame1_1: TFrame1;
    ToolBar1: TToolBar;
    procedure ChangeWorkspaceExecute(Sender: TObject);
    procedure PrepareWorkspaceForCommunity(Sender: TObject);
  private
    FViewModel: IMainViewModel;
    procedure SetViewModel(AValue: IMainViewModel);
  public
    property ViewModel: IMainViewModel read FViewModel write SetViewModel;
    procedure InitializeForm;
    procedure AddNewCommunityButton;
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

procedure TfMainView.InitializeForm;
var
  i: integer;
  NewButton: TToolButton;
begin
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
  NewButton.ImageIndex:=0;
  NewButton.Parent:=ToolBar1;
end;

end.