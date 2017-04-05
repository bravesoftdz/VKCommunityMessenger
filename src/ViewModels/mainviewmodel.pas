unit MainViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, Controls, AbstractModel,
  MainModel, entities, Graphics;

type

  { IMainViewModel }

  IMainViewModel = interface(IViewModel)
    {Fills imagelist of buttons with communities}
    procedure FillImageCommunitiesList(var List: TImageList);
    {Saves acces key of community in local database}
    procedure SaveNewCommunity(AccessKey, Id: string);
  end;

  { TMainViewModel }

  TMainViewModel = class(TInterfacedObject, IMainViewModel, IViewModel)
  private
    FModel: IModel;
    procedure SetModel(AValue: IModel);
    function GetModel: IModel;
    procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
  public
    procedure FillImageCommunitiesList(var ImageList: TImageList);
    property Model: IModel read GetModel write SetModel;
    procedure SaveNewCommunity(AccessKey, Id: string);
  end;

var
  LMainViewModel: TMainViewModel;

implementation

{ TMainViewModel }

procedure TMainViewModel.ResizeBitmap(Bitmap: TBitmap;
  const NewWidth, NewHeight: integer);
begin
  Bitmap.Canvas.StretchDraw(
    Rect(0, 0, NewWidth, NewHeight),
    Bitmap);
  Bitmap.SetSize(NewWidth, NewHeight);
end;

procedure TMainViewModel.SetModel(AValue: IModel);
begin
  if FModel = AValue then
    Exit;
  FModel := AValue;
end;

function TMainViewModel.GetModel: IModel;
begin
  Result := FModel;
end;

procedure TMainViewModel.FillImageCommunitiesList(var ImageList: TImageList);
var
  CommunitiesList: TCommunityList;
  i: integer;
  Picture, NoPhoto, NewCommunity, Frame, CommunityPhoto: TPicture;
  x, y: integer;
begin
  ImageList.Clear;

  {Prepare preload avatars}
  NoPhoto := (Model as TMainModel).GetNoPhotoAvatar;

  {First image - new community button}
  NewCommunity := (Model as TMainModel).GetAddNewCommunityPhoto;
  ImageList.Add(NewCommunity.Bitmap, nil);

  {Load frame for avatars}
  Frame := (Model as TMainModel).LoadFrameImage;

  {Get pictures from local storage}
  CommunitiesList := (Model as TMainModel).GetCommunities;
  for i := 0 to CommunitiesList.Count - 1 do
  begin
    {Load photo or set default photo}
    if CommunitiesList[i].HasPhoto then
      CommunityPhoto := CommunitiesList[i].Photo
    else
      CommunityPhoto := NoPhoto;

    {if photo is not 50x50 then resize it}
    if (CommunityPhoto.Width <> 50) or (CommunityPhoto.Height <> 50) then
      ResizeBitmap(CommunityPhoto.Bitmap, 50, 50);

    {Put photo in frame}
    Picture := TPicture.Create;
    Picture.Assign(Frame);
    x := Frame.Width div 2 - CommunityPhoto.Width div 2;
    y := Frame.Height div 2 - CommunityPhoto.Height div 2;
    Picture.Bitmap.Canvas.Draw(x, y, CommunityPhoto.Bitmap);

    ImageList.Add(Picture.Bitmap, nil);
  end;
end;

procedure TMainViewModel.SaveNewCommunity(AccessKey, Id: string);
var
  NewCommunity: TCommunity;
begin
  try
    NewCommunity := (Model as TMainModel).GetExtendedCommunityInformation(Id, AccessKey);
  except
    raise;
  end;
  (Model as TMainModel).SaveCommunityInfo(NewCommunity);
end;

end.
