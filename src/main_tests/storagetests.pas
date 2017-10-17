unit StorageTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ModelDataModel, ModelStorage;

type

  { TMockDAO }

  TMockDAO = class(TInterfacedObject, IDAOAdapter)
  public
    function ExtendCommunityInformation(CommunityId: string; AccessKey: string
      ): ICommunity;
    function FirstCallLongpoll(AccessKey: string): TLongPollServer;
    function NeedsUpdate(Server: TLongPollServer): boolean;
    function ReadCommunitiesList: TCommunitiesList;
    procedure SaveCommunitiesList(List: TCommunitiesList);
    procedure UpsertCommunity(Community: ICommunity);
  end;

  { TStorageTests }

  TStorageTests = class(TTestCase)
  private
    FDAO: TMockDAO;
    FStorage: IStorageService;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published

  end;

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

function TMockDAO.FirstCallLongpoll(AccessKey: string): TLongPollServer;
begin

end;

function TMockDAO.NeedsUpdate(Server: TLongPollServer): boolean;
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
