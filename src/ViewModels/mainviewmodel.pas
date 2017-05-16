unit MainViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, Controls, AbstractModel,
  MainModel, entities, Graphics, fgl, vkgsobserver;

type

  TBitmapList = specialize TFPGList<TBitmap>;

  { TUIData }

  TUIData = class
  private
    FButtonPictures: TBitmapList;
    FCommunities: TCommunityList;
    FNewCommunityPicture: TBitmap;
    procedure SetButtonPictures(AValue: TBitmapList);
    procedure SetCommunities(AValue: TCommunityList);
    procedure SetNewCommunityPicture(AValue: TBitmap);
  public
    constructor Create;
    property ButtonPictures: TBitmapList read FButtonPictures write SetButtonPictures;
    property Communities: TCommunityList read FCommunities write SetCommunities;
    property NewCommunityPicture: TBitmap read FNewCommunityPicture write SetNewCommunityPicture;
    destructor Destroy; override;
  end;

  { IMainViewModel }

  IMainViewModel = interface(IViewModel)
    {Fills imagelist of buttons with communities}
    function GetDataForUIUpdate: TUIData;
    {Saves acces key of community in local database}
    procedure SaveNewCommunity(AccessKey, Id: string);
  end;

  { TMainViewModel }

  TMainViewModel = class(TObserverViewModel, IMainViewModel, IViewModel)
  private
    procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
    procedure OnNotify;
  public
    constructor Create;
    function GetDataForUIUpdate: TUIData;
    property Model: IModel read GetModel write SetModel;
    procedure SaveNewCommunity(AccessKey, Id: string);
  end;

var
  LMainViewModel: TMainViewModel;

implementation

{ TUIData }

procedure TUIData.SetButtonPictures(AValue: TBitmapList);
begin
  if FButtonPictures = AValue then
    Exit;
  FButtonPictures := AValue;
end;

procedure TUIData.SetCommunities(AValue: TCommunityList);
begin
  if FCommunities = AValue then
    Exit;
  FCommunities := AValue;
end;

procedure TUIData.SetNewCommunityPicture(AValue: TBitmap);
begin
  if FNewCommunityPicture=AValue then Exit;
  FNewCommunityPicture:=AValue;
end;

constructor TUIData.Create;
begin
  ButtonPictures := TBitmapList.Create;
end;

destructor TUIData.Destroy;
begin
  FreeAndNil(FButtonPictures);
  if Assigned(FCommunities) then
    FreeAndNil(FCommunities);
  inherited Destroy;
end;

{ TMainViewModel }

procedure TMainViewModel.ResizeBitmap(Bitmap: TBitmap;
  const NewWidth, NewHeight: integer);
begin
  Bitmap.Canvas.StretchDraw(
    Rect(0, 0, NewWidth, NewHeight),
    Bitmap);
  Bitmap.SetSize(NewWidth, NewHeight);
end;

procedure TMainViewModel.OnNotify;
begin
  Observable.NotifyObservers;
end;

constructor TMainViewModel.Create;
begin
  inherited Create;
  Observer.Notify:=@OnNotify;
end;

function TMainViewModel.GetDataForUIUpdate: TUIData;
var
  CommunitiesList: TCommunityList;
  i: integer;
  Picture, NoPhoto, NewCommunity, Frame, CommunityPhoto: TPicture;
  x, y: integer;
begin
  Result := TUIData.Create;

  {Prepare preload avatars}
  NoPhoto := (Model as TMainModel).GetNoPhotoAvatar;

  {First image - new community button}
  NewCommunity := (Model as TMainModel).GetAddNewCommunityPhoto;
  Result.NewCommunityPicture := NewCommunity.Bitmap;

  {Load frame for avatars}
  Frame := (Model as TMainModel).LoadFrameImage;

  {Get pictures from local storage}
  CommunitiesList := (Model as TMainModel).GetCommunities;
  Result.Communities := CommunitiesList;

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

    Result.ButtonPictures.Add(Picture.Bitmap);
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
