{ ModelAPI unit contains facade class for model
  that should be used
  by every client }
unit ModelAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModelDataModel;

type

  { IModelAPI is a
    facade for model }

  IModelAPI = interface
    { The function returns list of dialogs with messages for a specified community
      @param(CommunityId is a ID of community whose messages you need)
      @returns(List of dialogs filled with actual messages)}
    function GetLastDialogsFor(CommunityId: string): TDialogsList;
    { This procedure sends a message to a specified person
      @param(CommunityId is a ID of community that should send your message)
      @param(MessageText is a text of your message)}
    procedure SendMessage(CommunityId: string; MessageText: string; PersonId: string);
    { This procedure updates specified community
      @param(Community is a community that should replace old community information
             Community should contain a ID to recognize, which community should be replaced)}
    procedure UpdateCommunityInfo(Community: ICommunity);
    { This procedure subscribes an observer to a model updates
      @param(Me is an observer that should be subscribed for model updates)}
    procedure Subscribe(Me: IObserver);
    { This function returns list of communities with actual dialogs and messages
      @returns(List of communities)}
    function GetCommunities: TCommunitiesList;
    { This function adds new community to the program and forces model to update
      information about this community
      @param(CommunityId is an ID of new community)
      @param(AccessKey is an API key of new community)}
    procedure AddNewCommunity(CommunityId: string; AccessKey: string);
  end;

  { TModelAPIDefaultImplementation is a
    production implementation of IModelAPI. See also docs of IModelAPI}
  TModelAPIDefaultImplementation = class(TInterfacedObject, IModelAPI)
  private
    FMessageStorage: IStorageService;
    FObserver: IObserver;
  public
    {Constructor
     @param(MessageService is a storage of messages)
     @param(Broker is a broker of the model)}
    constructor Create(MessageStorage: IStorageService; Broker: IBroker);
    function GetLastDialogsFor(CommunityId: string): TDialogsList;
    procedure SendMessage(CommunityId: string; MessageText: string; PersonId: string);
    procedure UpdateCommunityInfo(Community: ICommunity);
    procedure Subscribe(Me: IObserver);
    function GetCommunities: TCommunitiesList;
    procedure AddNewCommunity(CommunityId: string; AccessKey: string);
    destructor Destroy; override;
  end;

{ This function returns ModelAPI.
  It's global, because we have only one model and only one API
  @returns(IModelAPI object)}
function ModelAPI: IModelAPI;

implementation

var
  gModelAPI: IModelAPI;

function ModelAPI: IModelAPI;
begin
  if not Assigned(gModelAPI) then
    gModelAPI := TModelAPIDefaultImplementation.Create;
  Result := gModelAPI;
end;

{ TModelAPIDefaultImplementation }

constructor TModelAPIDefaultImplementation.Create(MessageStorage: IStorageService;
  Broker: IBroker);
begin
  FMessageStorage := MessageStorage;
  FObserver := //concrete;
    FObserver.Subrscribe(Broker);
end;

function TModelAPIDefaultImplementation.GetLastDialogsFor(CommunityId:
  string): TDialogsList;
begin

end;

procedure TModelAPIDefaultImplementation.SendMessage(CommunityId: string;
  MessageText: string; PersonId: string);
begin

end;

procedure TModelAPIDefaultImplementation.UpdateCommunityInfo(Community: ICommunity);

begin

end;

procedure TModelAPIDefaultImplementation.Subscribe(Me: IObserver);
begin

end;

function TModelAPIDefaultImplementation.GetCommunities: TCommunitiesList;
begin

end;

procedure TModelAPIDefaultImplementation.AddNewCommunity(CommunityId: string;
  AccessKey: string);
begin

end;

destructor TModelAPIDefaultImplementation.Destroy;
begin
  FMessageStorage := nil;
  FObserver.UnsubscribeAll;
  inherited Destroy;
end;

end.
