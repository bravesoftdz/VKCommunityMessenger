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

  TChatBotCommandsList = specialize TFPGList<IChatBotCommand>;

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

  TUserList = specialize TFPGList<IUser>;

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
    function GetUserId: string;
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

  TMessageList = specialize TFPGList<IMessage>;

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

  TDialogsList = specialize TFPGList<IDIalog>;

  { ICommunity
    is a representation of community in the model }
  ICommunity = interface
    function GetAccessKey: string;
    function GetChatbot: IChatBot;
    function GetCommunityType: TCommunityType;
    function GetDeactivated: boolean;
    function GetHasPhoto: boolean;
    function GetId: string;
    function GetIsClosed: boolean;
    function GetName: string;
    function GetPhoto: TPicture;
    function GetScreenName: string;
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
  end;

  TCommunitiesList = specialize TFPGList<ICommunity>;

  { IBroker
    is an observable with a difference that broker has own control flow (own thread, for example) }
  IBroker = class

  end;

  TVKCMNotifyEvent = procedure of object;

  { IObserver
    is an abstract observer (see Observer pattern)}
  IObserver = interface
    function GetNotify: TVKCMNotifyEvent;
    {Event that executes by notification}
    property Notify: TVKCMNotifyEvent read GetNotify;
    {Subscribes to broker}
    procedure Subscribe(ABroker: IBroker);
    {Unsubscribes from broker}
    procedure Unsubscribe(ABroker: IBroker);
    {Sends message to broker. This forces broker to notify every subscriber}
    procedure MessageToBroker;
    {Unsubscribe from all brokers}
    procedure UnsubscribeAll;
  end;


  { IChatbotService
    is a service that provides chatbot functionality}
  IChatbotService = interface

  end;

  { IStorageService
    is a service that provides storage for communities, messages and
    dialogs}
  IStorageService = interface

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



