unit ModelStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModelDataModel, sqldb, DB, sqlite3conn, vkcmconfig, vkdao, syncobjs;

type

  { TLongPollServer }

  TLongPollServer = class
  private
    FId: string;
    FTS: string;
    FKey: string;
    FServer: string;
    procedure SetId(AValue: string);
    procedure SetTS(AValue: string);
    procedure SetKey(AValue: string);
    procedure SetServer(AValue: string);
  public
    property Key: string read FKey write SetKey;
    property Server: string read FServer write SetServer;
    property TS: string read FTS write SetTS;
    property Id: string read FId write SetId;
  end;

  TLongPollServersObjectList = specialize TFPGObjectList<TLongpollServer>;


  { IDAOAdapter }

  IDAOAdapter = interface
    procedure UpsertCommunity(Community: ICommunity);
    function ReadCommunitiesList: TCommunitiesList;
    procedure SaveCommunitiesList(List: TCommunitiesList);
    function ExtendCommunityInformation(CommunityId: string;
      AccessKey: string): ICommunity;
    function NeedsUpdate(Server: TLongPollServer): boolean;
    function FirstCallLongpoll(AccessKey: string): TLongPollServer;
  end;

  { TDAOAdapter }

  TDAOAdapter = class(TInterfacedObject, IDAOAdapter)
  private
    FConnection: TSQLite3Connection;
  public
    constructor Create;
    procedure UpsertCommunity(Community: ICommunity);
    function ReadCommunitiesList: TCommunitiesList;
    procedure SaveCommunitiesList(List: TCommunitiesList);
    function ExtendCommunityInformation(CommunityId: string;
      AccessKey: string): ICommunity;
    destructor Destroy; override;
  end;

  TModelStorageService = class;

  { TLongpollThread
    is a thread that continuously updates storage (uses VK LongPoll server)
    You should fill DAO and Storage properties before start}
  TLongpollThread = class(TThread)
  private
    FDAO: IDAOAdapter;
    FStorage: TModelStorageService;
    FObserver: IObserver;
    procedure SetDAO(AValue: IDAOAdapter);
    procedure SetStorage(AValue: TModelStorageService);
    procedure ProcessServer(Server: TLongPollServer);
    procedure UpdateDialogsFor(Server: TLongPollServer);
  protected
    procedure Execute;
  public
    {Storage that should be updated}
    property Storage: TModelStorageService read FStorage write SetStorage;
    {Adapter to Data Acess Object}
    property DAO: IDAOAdapter read FDAO write SetDAO;
  end;

  { TModelStorageService
    is a implementation of IStorageService }
  TModelStorageService = class(TInterfacedObject, IStorageService)
  private
    FDAOAdapter: IDAOAdapter;
    {Critical section for access to storage}
    FCS: TCriticalSection;
    FCommunityStorage: TCommunitiesList;
    function GetCommunityById(Id: string): ICommunity;
    function GetIndexOfCommunity(Id: string): integer;
  public
    constructor Create(DAOAdapter: IDAOAdapter);
    function GetLastDialogsForCommunity(CommunityId: string): TDialogsList;
    procedure UpdateCommunityInformation(Community: ICommunity);
    function GetCommunities: TCommunitiesList;
    procedure AddNewCommunity(CommunityID, AccessKey: string);
    destructor Destroy; override;
  end;

implementation

{ TLongPollServer }

procedure TLongPollServer.SetId(AValue: string);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TLongPollServer.SetTS(AValue: string);
begin
  FTS := AValue;
end;

procedure TLongPollServer.SetKey(AValue: string);
begin
  FKey := AValue;
end;

procedure TLongPollServer.SetServer(AValue: string);
begin
  FServer := AValue;
end;

{ TDAOAdapter }

constructor TDAOAdapter.Create;
begin
  FConnection := TSQLite3Connection.Create(nil);
  FConnection.DatabaseName := DATABASE_PATH;
  if not FileExists(DATABASE_PATH) then
    try
      DAO.Database.ExecuteDatabaseCreationScript(FConnection);
    except
      raise Exception.Create('Ошибка при создании базы');
    end;
  FConnection.Open;
end;

procedure TDAOAdapter.UpsertCommunity(Community: ICommunity);
begin

end;

function TDAOAdapter.ReadCommunitiesList: TCommunitiesList;
begin

end;

procedure TDAOAdapter.SaveCommunitiesList(List: TCommunitiesList);
begin

end;

function TDAOAdapter.ExtendCommunityInformation(CommunityId: string;
  AccessKey: string): ICommunity;
begin

end;

destructor TDAOAdapter.Destroy;
begin
  FConnection.Close;
  FreeAndNil(FConnection);
  inherited Destroy;
end;

{ TLongpollThread }

procedure TLongpollThread.SetStorage(AValue: TModelStorageService);
begin
  if FStorage = AValue then
    Exit;
  FStorage := AValue;
end;

procedure TLongpollThread.ProcessServer(Server: TLongPollServer);
begin
  if FDAO.NeedsUpdate(Server) then
  begin
    UpdateDialogsFor(Server);
  end;
end;

procedure TLongpollThread.UpdateDialogsFor(Server: TLongPollServer);
begin

end;

procedure TLongpollThread.SetDAO(AValue: IDAOAdapter);
begin
  if FDAO = AValue then
    Exit;
  FDAO := AValue;
end;

procedure TLongpollThread.Execute;
var
  i: integer;
  CurrentServer: TLongPollServer;
  Communities: TCommunitiesList;
  LServersList: TLongPollServersObjectList;
begin
  if (not Assigned(FStorage)) or (not Assigned(FDAO)) then
    raise Exception.Create('Not all properties of longpoll are filled');

  Communities := FStorage.GetCommunities;

  LServersList := TLongPollServersObjectList.Create(True);
  for i := 0 to Communities.Count - 1 do
  begin
    LServersList.Add(FDAO.FirstCallLongpoll(Communities[i].AccessKey));
  end;

  while not Terminated do
  begin
    try
      for i := 0 to LServersList.Count - 1 do
      begin
        CurrentServer := LServersList[i];
        ProcessServer(CurrentServer);
        if terminated then
          break;
      end;
    except
    end;
  end;
  FreeAndNil(LServersList);

end;

{ TModelStorageService }

function TModelStorageService.GetCommunityById(Id: string): ICommunity;
begin
  Result := FCommunityStorage[GetIndexOfCommunity(Id)];
end;

function TModelStorageService.GetIndexOfCommunity(Id: string): integer;
var
  i: integer;
begin
  FCS.Enter;
  try
    for i := 0 to FCommunityStorage.Count - 1 do
      if FCommunityStorage[i].Id = Id then
      begin
        Result := i;
        exit;
      end;
  finally
    FCS.Leave;
  end;
end;

constructor TModelStorageService.Create(DAOAdapter: IDAOAdapter);
begin
  FDAOAdapter := DAOAdapter;
  FCS :=
    TCriticalSection.Create;
  FCommunityStorage := DAOAdapter.ReadCommunitiesList;
end;

function TModelStorageService.GetLastDialogsForCommunity(CommunityId:
  string): TDialogsList;
var
  Community: ICommunity;
begin
  FCS.Enter;
  try
    Community := GetCommunityById(CommunityId);
    Result := Community.Dialogs;
  finally
    FCS.Leave;
  end;
end;

procedure TModelStorageService.UpdateCommunityInformation(Community: ICommunity);
var
  index: integer;
begin
  FCS.Enter;
  try
    index := GetIndexOfCommunity(Community.Id);
    Community.Dialogs := FCommunityStorage[index].Dialogs;
    FCommunityStorage[index] := Community;
  finally
    FCS.Leave;
  end;
end;

function TModelStorageService.GetCommunities: TCommunitiesList;
begin
  FCS.Enter;
  try
    Result := FCommunityStorage;
  finally
    FCS.Leave;
  end;
end;

procedure TModelStorageService.AddNewCommunity(CommunityID, AccessKey: string);
var
  NewCommunity: ICommunity;
begin
  NewCommunity := FDAOAdapter.ExtendCommunityInformation(CommunityID, AccessKey);
  FCS.Enter;
  try
    FCommunityStorage.Add(NewCommunity);
  finally
    FCS.Leave;
  end;
end;

destructor TModelStorageService.Destroy;
begin
  FDAOAdapter.SaveCommunitiesList(FCommunityStorage);
  FDAOAdapter := nil;
  FreeAndNil(FCommunityStorage);
  FreeAndNil(FCS);
  inherited Destroy;
end;

end.
