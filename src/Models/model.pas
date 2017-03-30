unit Model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, entities, Graphics;

type

  { IModel }

  {Interface for models with long comments}
  IModel = interface
    {Returns full information about community}
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    {Saves community information in local databse}
    procedure SaveCommunityInfo(Communty: TCommunity);
    {Reads access keys from local database and loads community information from internet
     or (in case when there is no internet) from local storage}
    function GetCommunities: TCommunityList;
    {Returns photo that is used as avatar for communities with no photo}
    function GetNoPhotoAvatar: TPicture;
    {Returns photo for "Add new community" button}
    function GetAddNewCommunityPhoto: TPicture;
    {Loads frame image for toolbar}
    function LoadFrameImage: TPicture;
  end;

  { TModel }

  TModel = class(TInterfacedObject, IModel)
  public
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfo(Communty: TCommunity);
    function GetCommunities: TCommunityList;
    function GetNoPhotoAvatar: TPicture;
    function GetAddNewCommunityPhoto: TPicture;
    function LoadFrameImage: TPicture;
  end;

var
  LModel: TModel;

implementation

{ TModel }

function TModel.GetExtendedCommunityInformation(CommunityId, AccessKey: string):
TCommunity;
begin

end;

procedure TModel.SaveCommunityInfo(Communty: TCommunity);
begin

end;

function TModel.GetCommunities: TCommunityList;
var
  Community: TCommunity;
  Photo: TPicture;
begin
  Result := TCommunityList.Create;

  {Create some fake communities}
  Community := TCommunity.Create;
  Community.Photo := TPicture.Create;
  Community.Photo.LoadFromFile('testdata\habr.jpg');
  Community.HasPhoto := True;
  Community.AccessKey := 'jikernmkgp555wm';
  Community.CommunityType := ctPage;
  Community.Deactivated := False;
  Community.Id := '585934949';
  Community.IsClosed := False;
  Community.Name := 'Хабрахабр';
  Community.ScreenName := 'habr';
  Result.Add(Community);

  Community := TCommunity.Create;
  Community.HasPhoto := True;
  Community.Photo := TPicture.Create;
  Community.Photo.LoadFromFile('testdata\itc.jpg');
  Community.AccessKey := 'thgo453zht';
  Community.CommunityType := ctEvent;
  Community.Deactivated := False;
  Community.Id := '6666676769';
  Community.IsClosed := False;
  Community.Name := 'ITc';
  Community.ScreenName := 'its';
  Result.Add(Community);

  Community := TCommunity.Create;
  Community.HasPhoto := False;
  Community.AccessKey := 'gjkls4784nkl';
  Community.CommunityType := ctPage;
  Community.Deactivated := False;
  Community.Id := '68688686';
  Community.IsClosed := False;
  Community.Name := 'Тестовая группа';
  Community.ScreenName := 'test';
  Result.Add(Community);
end;


function TModel.GetNoPhotoAvatar: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('img/noavatar.bmp');
end;

function TModel.GetAddNewCommunityPhoto: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('img/newuser.bmp');
end;

function TModel.LoadFrameImage: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('img/frame.bmp');
end;

end.
