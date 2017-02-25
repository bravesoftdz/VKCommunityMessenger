unit Model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, entities, Graphics;

type

  { IModel }

  IModel = interface
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfoLocally(Communty: TCommunity);
    function GetCommunitiesLocal: TCommunityList;
    function GetCommunityLocal(Name: string): TCommunity;
    function GetNoPhotoAvatar: TPicture;
    function GetAddNewCommunityPhoto: TPicture;
  end;

  { TModel }

  TModel = class(TInterfacedObject, IModel)
  public
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfoLocally(Communty: TCommunity);
    function GetCommunitiesLocal: TCommunityList;
    function GetCommunityLocal(Name: string): TCommunity;
    function GetNoPhotoAvatar: TPicture;
    function GetAddNewCommunityPhoto: TPicture;
  end;

var
  LModel: TModel;

implementation

{ TModel }

function TModel.GetExtendedCommunityInformation(CommunityId, AccessKey: string):
TCommunity;
begin

end;

procedure TModel.SaveCommunityInfoLocally(Communty: TCommunity);
begin

end;

function TModel.GetCommunitiesLocal: TCommunityList;
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

function TModel.GetCommunityLocal(Name: string): TCommunity;
begin

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

end.
