unit MainViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, Controls, Model, entities, Graphics;

type
  IMainViewModel = interface(IViewModel)
     procedure FillImageCommunitiesList(var List: TImageList);
  end;

  { TMainViewModel }

  TMainViewModel = class(TInterfacedObject, IMainViewModel)
  private
    FModel: IModel;
    procedure SetModel(AValue: IModel);
    function GetModel: IModel;
  public
    procedure FillImageCommunitiesList(var ImageList: TImageList);
    property Model: IModel read GetModel write SetModel;
  end;

  var LMainViewModel: TMainViewModel;

implementation

{ TMainViewModel }

procedure TMainViewModel.SetModel(AValue: IModel);
begin
  if FModel=AValue then Exit;
  FModel:=AValue;
end;

function TMainViewModel.GetModel: IModel;
begin
  Result:=FModel;
end;

procedure TMainViewModel.FillImageCommunitiesList(var ImageList: TImageList);
var CommunitiesList: TCommunityList;
    i: integer;
    Picture, NoPhoto: TPicture;
begin
  ImageList.Clear;

  {Prepare preload avatars}

  {Get pictures from local storage}
  CommunitiesList:=Model.GetCommunitiesLocal;
  for i:=0 to CommunitiesList.Count-1 do
  begin
    if CommunitiesList[i].HasPhoto then
       Picture:=CommunitiesList[i].Photo
    else
       Picture:=NoPhoto;
    ImageList.Add(Picture.Bitmap,nil);
  end;
end;

end.

