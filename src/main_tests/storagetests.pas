unit StorageTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ModelDataModel, ModelStorage;

type

  { TMockDAO
  is a mock of DAO created for unit testing. You can set result (and other options)
  of each function. Each method * that is inherited from IDAOAdapter has
  Configure* method that you can use to change behavior of this mock}
  TMockDAO = class(TInterfacedObject, IDAOAdapter)
  private

  public
    {Creates mock with default return values}
    constructor Create;
    {Implementation of IDAOAdapter method. Can be configured
     in ConfigureExtendCommunityInformation method}
    function ExtendCommunityInformation(CommunityId: string;
      AccessKey: string): ICommunity;
    {Configures return value of TMockDAO.ExtendCommunityInformation
    @param(AResult is a community that should be returned in
     ExtendCommunityInformation call)}
    procedure ConfigureExtendCommunityInformation(AResult: ICommunity);
    {Implementation of IDAOAdapter method. Can be configured in
     ConfigureFirstCallLongPoll method}
    function FirstCallLongpoll(AccessKey: string): TLongPollServer;
    {Configures TMockDAO.FirstCallLongpoll method
    @param(AResult is a result that should be returned by calling
    FirstCallLongPoll)}
    procedure ConfigureFirstCallLongpoll(AResult: TLongPollServer);
    {Implementation of IDAOAdapter method. Can be configured in
     ConfigureNeedsUpdate method}
    function NeedsUpdate(Server: TLongPollServer): boolean;
    {Configures TMockDAO.NeedsUpdate method
    @param(AResult is a result of NeedsUpdate call)}
    procedure ConfigureNeedsUpdate(AResult: boolean);
    {Implementation of IDAOAdapter interface. This method can be
     configured in ConfigureReadCommunitiesList}
    function ReadCommunitiesList: TCommunitiesList;
    {Configuration method for ReadCommunitiesList
    @param(AResult is a result of ReadCommunitiesList call)}
    procedure ConfigureReadCommunitiesList(AResult: TCommunitiesList);
    {Implementation of IDAOAdapter interface}
    procedure SaveCommunitiesList(List: TCommunitiesList);
    {Implementation of IDAOAdapter interface}
    procedure UpsertCommunity(Community: ICommunity);
    {Implementation of IDAOAdapter interface. Can be configured in a method.
     Can be configured in method ConfigureFirstCallLongPoll}
    function FirstCallLongpoll(CommunityId, AccessKey: string): TLongPollServer;
    {Configures behavior of FirstCallLongpoll
     @param(AResult is a result of FirstCallLongPoll)}
    procedure ConfigureFirstCallLongPoll(AResult: TLongPollServer);
    {Implementation of IDAOAdapter interface. Can be configured in a method
     ConfigureLoadDialogsForCommunity()}
    function LoadDialogsForCommunity(Community: ICommunity): TDialogsList;
    {Configures behavior of LoadDialogsForCommunity
     @param(AResult is a result of LoadDialogsForCommunity)}
    procedure ConfigureLoadDialogsForCommunity(AResult: TDialogsList);
  end;

  { TStorageTests }

  TStorageTests = class(TTestCase)
  private
    FDAO: IDAOAdapter;
    {FDAO as TMockDAO}
    function FDAO_M: TMockDAO;
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
  //Nothing
end;

function TMockDAO.FirstCallLongpoll(CommunityId, AccessKey: string): TLongPollServer;
begin

end;

function TMockDAO.LoadDialogsForCommunity(Community: ICommunity): TDialogsList;
begin

end;

function TMockDAO.ReadCommunitiesList: TCommunitiesList;
begin

end;

procedure TMockDAO.SaveCommunitiesList(List: TCommunitiesList);
begin

end;

constructor TMockDAO.Create;
begin
  ConfigureExtendCommunityInformation(nil);
end;

function TMockDAO.ExtendCommunityInformation(CommunityId: string;
  AccessKey: string): ICommunity;
begin

end;

procedure TMockDAO.ConfigureExtendCommunityInformation(AResult: ICommunity);
begin

end;

function TMockDAO.FirstCallLongpoll(AccessKey: string): TLongPollServer;
begin

end;

function TMockDAO.NeedsUpdate(Server: TLongPollServer): boolean;
begin

end;

{ TStorageTests }

function TStorageTests.FDAO_M: TMockDAO;
begin
  Result := (FDAO as TMockDAO);
end;

procedure TStorageTests.Setup;
begin
  FDAO := TMockDAO.Create;
end;

procedure TStorageTests.TearDown;
begin
  FDAO := nil;
end;

initialization
  RegisterTest(TStorageTests);

end.
