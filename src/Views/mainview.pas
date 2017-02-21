unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, MainViewModel;

type

  { TfMainView }

  TfMainView = class(TForm)
    CommunitiesImageList: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure ChangeWorkspaceExecute(Sender: TObject);
    procedure AddNewCommunity(Sender: TObject);
    procedure PrepareWorkspaceForCommunity(Sender: TObject);
  private
    FViewModel: IMainViewModel;
    procedure SetViewModel(AValue: IMainViewModel);
  public
    property ViewModel: IMainViewModel read FViewModel write SetViewModel;
    procedure InitializeForm;
  end;

var
  fMainView: TfMainView;

implementation

{$R *.lfm}

{ TfMainView }

procedure TfMainView.AddNewCommunity(Sender: TObject);
begin

end;

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
  for i := 1 to CommunitiesImageList.Count - 1 do
  begin
    NewButton := TToolButton.Create(ToolBar1);
    NewButton.ImageIndex := i;
    NewButton.Parent := ToolBar1;
  end;
  ToolButton1.OnClick := @AddNewCommunity;
end;

end.
