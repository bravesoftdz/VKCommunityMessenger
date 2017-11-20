unit StorageTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ModelDataModel, ModelStorage,
  ModelEntitiesImplementation;

type

  { TMockDAO
  is a mock of DAO created for unit testing. You can set result (and other options)
  of each function. Each method * that is inherited from IDAOAdapter has
  Configure* method that you can use to change behavior of this mock}
  TMockDAO = class(TInterfacedObject, IDAOAdapter)
  private
    FResultExtendCommunityInformation: ICommunity;
    FResultFirstCallLongPoll: TLongPollServer;
    FResultNeedsUpdate: boolean;
    FResultReadCommunitiesList: TCommunitiesList;
    FResultLoadDialogsForCommunity: TDialogsList;
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
    function FirstCallLongpoll(CommunityId, AccessKey: string): TLongPollServer;
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
  protected
  published
    {Test tries to create full datamodel and saves it in storage}
    procedure CreationTest;
  end;

var
  DAO: TMockDAO;

implementation

{ TStorageTests }

procedure TStorageTests.CreationTest;
var
  DAO: IDAOAdapter;
  Storage: IStorageService;
  Communities: TCommunitiesList;
  ChatBot: IChatBot;
  Commands: TChatBotCommandsList;
  Dialogs: TDialogsList;
  MessagesList: TMessageList;
  User: IUser;
begin
  DAO := TMockDAO.Create;
  {Create chatbot}
  Commands := TChatBotCommandsList.Create;
  Commands.Add(TChatBotCommand.Create('hi!', 'hi!'));
  ChatBot := TChatBot.Create(Commands);
  {Create user}
  User := TUser.Create('Andrei','zawwww','Aleksandrov',nil,nil);
  {Create dialogs}
  Dialogs := TDialogsList.Create;
  MessagesList := TMessageList.Create;
  MessagesList.Add(TMessage.Create(Now(), False, False, 'someID',
    'anotherID', 'Hallo!', otRecieved, rsUnread, 'Hallo!'));
  Dialogs.Add(TDialog.Create(User,MessagesList));
  {Create community}
  Communities := TCommunitiesList.Create;
  Communities.Add(TCommunity.Create('ac', ChatBot, ctGroup, False,
    Dialogs, False, 'lol', False, 'MyGroup', 'mgroup', nil));
  (DAO as TMockDAO).ConfigureReadCommunitiesList(Communities);
  Storage := TModelStorageService.Create(DAO);
  //TODO: Test that all entities were delivered
end;

{ TMockDAO }

procedure TMockDAO.UpsertCommunity(Community: ICommunity);
begin
  //Nothing
end;

function TMockDAO.FirstCallLongpoll(CommunityId, AccessKey: string): TLongPollServer;
begin
  Result := FResultFirstCallLongPoll;
end;

function TMockDAO.LoadDialogsForCommunity(Community: ICommunity): TDialogsList;
begin
  Result := FResultLoadDialogsForCommunity;
end;

procedure TMockDAO.ConfigureLoadDialogsForCommunity(AResult: TDialogsList);
begin
  FResultLoadDialogsForCommunity := AResult;
end;

function TMockDAO.ReadCommunitiesList: TCommunitiesList;
begin
  Result := FResultReadCommunitiesList;
end;

procedure TMockDAO.ConfigureReadCommunitiesList(AResult: TCommunitiesList);
begin
  FResultReadCommunitiesList := AResult;
end;

procedure TMockDAO.SaveCommunitiesList(List: TCommunitiesList);
begin
  //nothing
end;

constructor TMockDAO.Create;
begin
  ConfigureExtendCommunityInformation(nil);
  ConfigureFirstCallLongpoll(nil);
  ConfigureLoadDialogsForCommunity(nil);
  ConfigureNeedsUpdate(False);
  ConfigureReadCommunitiesList(nil);
end;

function TMockDAO.ExtendCommunityInformation(CommunityId: string;
  AccessKey: string): ICommunity;
begin
  Result := FResultExtendCommunityInformation;
end;

procedure TMockDAO.ConfigureExtendCommunityInformation(AResult: ICommunity);
begin
  FResultExtendCommunityInformation := AResult;
end;

procedure TMockDAO.ConfigureFirstCallLongpoll(AResult: TLongPollServer);
begin
  FResultFirstCallLongPoll := AResult;
end;

function TMockDAO.NeedsUpdate(Server: TLongPollServer): boolean;
begin
  Result := FResultNeedsUpdate;
end;

procedure TMockDAO.ConfigureNeedsUpdate(AResult: boolean);
begin
  FResultNeedsUpdate := AResult;
end;

initialization
  RegisterTest(TStorageTests);

end.
