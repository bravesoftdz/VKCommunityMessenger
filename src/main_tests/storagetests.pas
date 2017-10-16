unit StorageTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ModelDataModel, ModelStorage;

type

  { TMockDAO }

  TMockDAO = class(TInterfacedObject, IDAOAdapter)
    procedure UpsertCommunity(Community: ICommunity);
    function ReadCommunitiesList: TCommunitiesList;
    procedure SaveCommunitiesList(List: TCommunitiesList);
    function ExtendCommunityInformation(CommunityId: string;
      AccessKey: string): ICommunity;
  end;

  { TStorageTests }

  TStorageTests = class(TTestCase)
  private
    FStorage: IStorageService;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published

  end;

var
  DAO: TMockDAO;

implementation

{ TMockDAO }

procedure TMockDAO.UpsertCommunity(Community: ICommunity);
begin

end;

function TMockDAO.ReadCommunitiesList: TCommunitiesList;
begin

end;

procedure TMockDAO.SaveCommunitiesList(List: TCommunitiesList);
begin

end;

function TMockDAO.ExtendCommunityInformation(CommunityId: string;
  AccessKey: string): ICommunity;
begin

end;

{ TStorageTests }

procedure TStorageTests.Setup;
begin
  FStorage := TModelStorageService.Create(DAO);
end;

procedure TStorageTests.TearDown;
begin
  FStorage := nil;
end;

initialization
  DAO := TMockDAO.Create;
  RegisterTest(TStorageTests);

finalization
  DAO := nil;

end.
