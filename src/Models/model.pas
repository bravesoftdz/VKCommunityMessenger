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
  end;

  { TModel }

  TModel = class(TInterfacedObject, IModel)
  public
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfoLocally(Communty: TCommunity);
    function GetCommunitiesLocal: TCommunityList;
    function GetCommunityLocal(Name: string): TCommunity;
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
var Community: TCommunity;
    Photo: TPicture;
begin
  Result:=TCommunityList.Create;

  {Create some fake communities}
  Community:=TCommunity.Create;
  Community.Photo:=TPicture.Create;
  Community.Photo.LoadFromFile('testdata\habr.jpg');
  Community.HasPhoto:=true;
  Community.AccessKey:='jikernmkgp555wm';
  Community.CommunityType:=ctPage;
  Community.Deactivated:=false;
  Community.Id:='585934949';
  Community.IsClosed:=false;
  Community.Name:='Хабрахабр';
  Community.ScreenName:='habr';
  Result.Add(Community);
end;

function TModel.GetCommunityLocal(Name: string): TCommunity;
begin

end;

end.
