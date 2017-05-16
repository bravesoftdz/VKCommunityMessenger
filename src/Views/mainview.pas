unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, MainViewModel, IdKeyDialog, helpers, entities, ChatView, vkgsobserver, AbstractViewModel;

type

  { TfMainView }

  TfMainView = class(TForm)
    CommunitiesImageList: TImageList;
    ToolBar1: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure PrepareWorkspaceForCommunity(Sender: TObject);
  private
    FCurrentFrame: TFrame;
    FObservable: TVKGSObservable;
    FObserver: TVKGSObserver;
    FViewModel: IMainViewModel;
    procedure SetCurrentFrame(AValue: TFrame);
    procedure SetObservable(AValue: TVKGSObservable);
    procedure SetObserver(AValue: TVKGSObserver);
    procedure SetViewModel(AValue: IMainViewModel);
    procedure OnNotify;
  public
    property ViewModel: IMainViewModel read FViewModel write SetViewModel;
    procedure UpdateGUI;
    {Creates a button for adding communities}
    procedure AddNewCommunityButton;
    {Event of "Add new community" button}
    procedure AddNewCommunity(Sender: TObject);
    property CurrentFrame: TFrame read FCurrentFrame write SetCurrentFrame;
    property Observer: TVKGSObserver read FObserver write SetObserver;
    property Observable: TVKGSObservable read FObservable write SetObservable;
  end;

var
  LMainView: TfMainView;

implementation

{$R *.lfm}

{ TfMainView }

procedure TfMainView.PrepareWorkspaceForCommunity(Sender: TObject);
var
  Community: TCommunity;
begin
  Community := ((Sender as TToolButton).DataObject as TCommunity);
  LChatView.Community := Community;
  LChatView.UpdateGUI;
  LChatView.Observer.Subscribe(Observable);
  CurrentFrame := LChatView;
end;

procedure TfMainView.FormCreate(Sender: TObject);
begin
  Observer := TVKGSObserver.Create;
  Observer.Notify:=@OnNotify;
  Observable := TVKGSObservable.Create;
end;

procedure TfMainView.SetViewModel(AValue: IMainViewModel);
begin
  if FViewModel = AValue then
    Exit;
  FViewModel := AValue;
  if (FViewModel is TObserverViewModel) then
    Observer.Subscribe((FViewModel as TObserverViewModel).Observable);
end;

procedure TfMainView.OnNotify;
begin
  UpdateGUI;
  Observable.NotifyObservers;
end;

procedure TfMainView.SetCurrentFrame(AValue: TFrame);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Parent := nil;
  AValue.Parent := self;
end;

procedure TfMainView.SetObservable(AValue: TVKGSObservable);
begin
  if FObservable=AValue then Exit;
  FObservable:=AValue;
end;

procedure TfMainView.SetObserver(AValue: TVKGSObserver);
begin
  if FObserver=AValue then Exit;
  FObserver:=AValue;
end;

procedure TfMainView.UpdateGUI;
var
  i: integer;
  NewButton: TToolButton;
  Data: TUIData;
begin
  {Clean imagelist and toolbar}
  CommunitiesImageList.Clear;
  while ToolBar1.ControlCount > 0 do
  begin
    ToolBar1.Controls[0].DataObject.Free;
    ToolBar1.Controls[0].Free;
  end;

  {Fill imagelist}
  Data := FViewModel.GetDataForUIUpdate;
  CommunitiesImageList.Add(Data.NewCommunityPicture, nil);
  for i := 0 to Data.ButtonPictures.Count - 1 do
    CommunitiesImageList.Add(Data.ButtonPictures[i], nil);

  {1, becuase index 0 stands for new community icon}
  for i := 1 to CommunitiesImageList.Count - 1 do
  begin
    NewButton := TToolButton.Create(ToolBar1);
    NewButton.ImageIndex := i;
    NewButton.DataObject := Data.Communities[i - 1];
    NewButton.Parent := ToolBar1;
    NewButton.OnClick := @PrepareWorkspaceForCommunity;
  end;
  AddNewCommunityButton;

  FreeAndNil(Data);
end;

procedure TfMainView.AddNewCommunityButton;
var
  NewButton: TToolButton;
begin
  NewButton := TToolButton.Create(Toolbar1);
  NewButton.ImageIndex := 0;
  NewButton.Parent := ToolBar1;
  {Hack: NewButton will be always first}
  NewButton.Top := -1;
  NewButton.OnClick := @AddNewCommunity;
end;

procedure TfMainView.AddNewCommunity(Sender: TObject);
var
  Id, AccessKey: string;
begin
  Dialog.ShowModal;
  if not Dialog.Done then
    exit;
  Id := Dialog.Id;
  AccessKey := Dialog.AccessKey;
  try
    ViewModel.SaveNewCommunity(AccessKey, Id);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  UpdateGUI;
end;

end.
