{This unit contains whole abstractions and globals
 that exist in the model}
unit ModelDataModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Graphics;

type
  {Type defines types of communities}
  TCommunityType = (
    {Group}
    ctGroup,
    {Page}
    ctPage,
    {Event}
    ctEvent);

  { IChatBotCommand is a
    command of chatbot. When chatbot recieves command that is equal to
    Command, chatbot responses with Response}
  IChatBotCommand = interface
    function GetCommand: string;
    function GetResponse: string;
    property Command: string read GetCommand;
    property Response: string read GetResponse;
  end;

  TChatBotCommandsList = specialize TFPGInterfacedObjectList<IChatBotCommand>;

  { IChatBot
    is a container of multiple commands }
  IChatBot = interface
    function GetCommands: TChatBotCommandsList;
    property Commands: TChatBotCommandsList read GetCommands;
  end;

  { IUser
    is a representation of social network user in model }
  IUser = interface
    function GetFirstName: string;
    function GetId: string;
    function GetLastName: string;
    function GetPicture200: TPicture;
    function GetPicture50: TPicture;
    {Id of user in social network}
    property Id: string read GetId;
    property FirstName: string read GetFirstName;
    property LastName: string read GetLastName;
    {Avatar of user 50x50 px}
    property Picture50: TPicture read GetPicture50;
    {Avatar of user 200x200 px}
    property Picture200: TPicture read GetPicture200;
  end;

  TUserList = specialize TFPGInterfacedObjectList<IUser>;

  {Enum that defines whether somethig (message) is read or not}
  TReadState = (rsRead, rsUnread);

  {Enum that shows whether something (message) is sent or recieved}
  TOutType = (otSent, otRecieved);

  { IMessage
    is a representation of message in the model}
  IMessage = interface
    function GetDate: TDateTime;
    function GetDeleted: boolean;
    function GetEmoji: boolean;
    function GetFromId: string;
    function GetId: string;
    function GetMessage: string;
    function GetOut: TOutType;
    function GetReadState: TReadState;
    function GetTitle: string;
    {Id of message}
    property Id: string read GetId;
    {Text of message}
    property Message: string read GetMessage;
    {Date and time when message was sent}
    property Date: TDateTime read GetDate;
    {Is message read or not?}
    property ReadState: TReadState read GetReadState;
    {Is message sent or recieved?}
    property Out: TOutType read GetOut;
    {Title of message}
    property Title: string read GetTitle;
    {Is message deleted or not?}
    property Deleted: boolean read GetDeleted;
    {Does the message contain emoji?}
    property Emoji: boolean read GetEmoji;
  end;

  TMessageList = specialize TFPGInterfacedObjectList<IMessage>;

  { IDialog
    is a representation of dialog in the model }

  IDialog = interface
    function GetMessages: TMessageList;
    function GetPerson: IUser;
    {The person you are talking to in this dialog}
    property Person: IUser read GetPerson;
    {List of messages, sorted by time (first message is the newest)}
    property Messages: TMessageList read GetMessages;
  end;

  TDialogsList = specialize TFPGInterfacedObjectList<IDIalog>;

  { ICommunity
    is a representation of community in the model }
  ICommunity = interface
    function GetAccessKey: string;
    function GetChatbot: IChatBot;
    function GetCommunityType: TCommunityType;
    function GetDeactivated: boolean;
    function GetDialogs: TDialogsList;
    function GetHasPhoto: boolean;
    function GetId: string;
    function GetIsClosed: boolean;
    function GetName: string;
    function GetPhoto: TPicture;
    function GetScreenName: string;
    procedure SetDialogs(AValue: TDialogsList);
    {Name of community}
    property Name: string read GetName;
    {Unique id of community}
    property Id: string read GetId;
    {Short name of community}
    property ScreenName: string read GetScreenName;
    {Is community closed?}
    property IsClosed: boolean read GetIsClosed;
    {Is community dectivated?}
    property Deactivated: boolean read GetDeactivated;
    {Type of community}
    property CommunityType: TCommunityType read GetCommunityType;
    {Does the community has photo?}
    property HasPhoto: boolean read GetHasPhoto;
    {Photo of community (if exists, chech HasPhoto property}
    property Photo: TPicture read GetPhoto;
    {AccessKey for API of social network}
    property AccessKey: string read GetAccessKey;
    {Chatbot of community}
    property Chatbot: IChatBot read GetChatbot;
    {Dialogs of community.
     Dialogs has a setter, because it should be possible to update
     dialogs without updating community}
    property Dialogs: TDialogsList read GetDialogs write SetDialogs;
  end;

  TCommunitiesList = specialize TFPGInterfacedObjectList<ICommunity>;

  IObserver = interface;

  { IBroker
    is an observable with a difference that broker has own control flow (own thread, for example) }
  IBroker = interface
    { Subscribes observer to broker }
    procedure Subscribe(AObserver: IObserver);
    { Unsubscribes observer from broker }
    procedure Unsubscribe(AObserver: IObserver);
    { Notifies all observers }
    procedure NotifyAllObservers;
  end;

  TVKCMNotifyEvent = procedure of object;

  { IObserver
    is an abstract observer (see Observer pattern)}
  IObserver = interface
    function GetNotify: TVKCMNotifyEvent;
    procedure SetNotify(AValue: TVKCMNotifyEvent);
    {Event that executes by notification}
    property Notify: TVKCMNotifyEvent read GetNotify write SetNotify;
    {Subscribes to broker}
    procedure Subscribe(ABroker: IBroker);
    {Unsubscribes from broker}
    procedure Unsubscribe(ABroker: IBroker);
    {Sends message to broker. This forces broker to notify every subscriber}
    procedure MessageToBrokers;
    {Unsubscribe from all brokers}
    procedure UnsubscribeFromAll;
  end;


  { IChatbotService
    is a service that provides chatbot functionality}
  IChatbotService = interface

  end;

  { IStorageService
    is a service that provides storage for communities, messages and
    dialogs}
  IStorageService = interface
    {Function returns list of last 10 dialogs
     @param(CommunityId is a string, unique id of community)}
    function GetLastDialogsForCommunity(CommunityId: string): TDialogsList;
    {This function updates information about community (properties and chatbot structure, not dialogs)
     @param(Community is a community that will replace old community)}
    procedure UpdateCommunityInformation(Community: ICommunity);
    {This function returns list of all communities that are stored in this application/database}
    function GetCommunities: TCommunitiesList;
    {This function adds new community, loads full information from internet and saves it in the storage
     @param(CommunityId is an ID of community that should be added)
     @param(AccessKey is a key of community)}
    procedure AddNewCommunity(CommunityID, AccessKey: string);
    {This function returns community by id
     @param(Id of community)
     @return(Community with specified ID)}
    function GetCommunityById(Id: string): ICommunity;
  end;

  { IMessageSender
    is a service for sending messages }
  IMessageSender = interface
    {Send message to a user from community
     @param(CommunityID is an id of community that should send the message)
     @param(Message is a text of message)
     @param(PersonID is a id of reciever)}
    procedure SendMessage(CommunityID, Message, PersonID: string);
  end;

{Converts string to TCommunityType
 @param('PAGE' or 'GROUP' or 'EVENT' (case-insensitive))
 @returns(Community type)}
function StringToCommunityType(Str: string): TCommunityType;

{Converts TCommunityType to a string
 @param(AType is a TCommunityType)
 @returns('event' or 'group' or 'event')}
function CommunintyTypeToString(AType: TCommunityType): string;

implementation

function StringToCommunityType(Str: string): TCommunityType;
begin
  if Uppercase(Str) = 'PAGE' then
    Result := ctPage
  else if Uppercase(Str) = 'GROUP' then
    Result := ctGroup
  else if Uppercase(Str) = 'EVENT' then
    Result := ctEvent
  else
    raise Exception.Create('StringToCommunityType: неизвестный тип сообщества');
end;

function CommunintyTypeToString(AType: TCommunityType): string;
begin
  case AType of
    ctEvent: Result := 'event';
    ctGroup: Result := 'group';
    ctPage: Result := 'page';
  end;
end;


end.



