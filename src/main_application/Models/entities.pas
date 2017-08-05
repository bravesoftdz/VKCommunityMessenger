unit entities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl;

type

  TCommunityType = (ctGroup, ctPage, ctEvent);

  { TChatBotCommand }

  TChatBotCommand = class
  private
    FCommand: string;
    FResponse: string;
    procedure SetCommand(AValue: string);
    procedure SetResponse(AValue: string);
  public
    property Command: string read FCommand write SetCommand;
    property Response: string read FResponse write SetResponse;
  end;

  TChatBotCommandsObjectList = specialize TFPGObjectList<TChatBotCommand>;

  TChatBot = class
  private
    FCommands: TChatBotCommandsObjectList;
    procedure SetCommands(AValue: TChatBotCommandsObjectList);
  public
    property Commands: TChatBotCommandsObjectList read FCommands write SetCommands;
    constructor Create;
    procedure AddCommand(Command: string; Response: string);
    destructor Destroy; override;
  end;

  { TCommunity }

  TCommunity = class
  private
    FAccessKey: string;
    FChatbot: TChatBot;
    FCommunityType: TCommunityType;
    FDeactivated: boolean;
    FHasPhoto: boolean;
    FId: string;
    FIsClosed: boolean;
    FName: string;
    FPhoto: TPicture;
    FScreenName: string;
    procedure SetAccessKey(AValue: string);
    procedure SetChatbot(AValue: TChatBot);
    procedure SetCommunityType(AValue: TCommunityType);
    procedure SetDeactivated(AValue: boolean);
    procedure SetHasPhoto(AValue: boolean);
    procedure SetId(AValue: string);
    procedure SetIsClosed(AValue: boolean);
    procedure SetName(AValue: string);
    procedure SetPhoto(AValue: TPicture);
    procedure SetScreenName(AValue: string);
  public
    property Name: string read FName write SetName;
    property Id: string read FId write SetId;
    property ScreenName: string read FScreenName write SetScreenName;
    property IsClosed: boolean read FIsClosed write SetIsClosed;
    property Deactivated: boolean read FDeactivated write SetDeactivated;
    property CommunityType: TCommunityType read FCommunityType write SetCommunityType;
    property HasPhoto: boolean read FHasPhoto write SetHasPhoto;
    property Photo: TPicture read FPhoto write SetPhoto;
    property AccessKey: string read FAccessKey write SetAccessKey;
    property Chatbot: TChatBot read FChatbot write SetChatbot;
    constructor Create;
    function Copy: TCommunity;
    destructor Destroy; override;
  end;

  TCommunityList = specialize TFPGList<TCommunity>;

  { TUser }

  TUser = class
  private
    FFirstName: string;
    FId: string;
    FLastName: string;
    FPhoto: TPicture;
    FPhoto200: TPicture;
    procedure SetFirstName(AValue: string);
    procedure SetId(AValue: string);
    procedure SetLastName(AValue: string);
    procedure SetPhoto(AValue: TPicture);
    procedure SetPhoto200(AValue: TPicture);
  public
    property Id: string read FId write SetId;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Photo50: TPicture read FPhoto write SetPhoto;
    property Photo200: TPicture read FPhoto200 write SetPhoto200;
  end;

  TUserList = specialize TFPGList<TUser>;

  TReadState = (rsRead, rsUnread);

  TOutType = (otSent, otRecieved);

  { TMessage }

  TMessage = class
  private
    FDate: TDateTime;
    FDeleted: boolean;
    FEmoji: boolean;
    FFromId: string;
    FId: string;
    FMessage: string;
    FOut: TOutType;
    FReadState: TReadState;
    FTitle: string;
    FUserId: string;
    procedure SetDate(AValue: TDateTime);
    procedure SetDeleted(AValue: boolean);
    procedure SetEmoji(AValue: boolean);
    procedure SetFromId(AValue: string);
    procedure SetId(AValue: string);
    procedure SetMessage(AValue: string);
    procedure SetOut(AValue: TOutType);
    procedure SetReadState(AValue: TReadState);
    procedure SetTitle(AValue: string);
    procedure SetUserId(AValue: string);
  public
    property Id: string read FId write SetId;
    property Message: string read FMessage write SetMessage;
    property UserId: string read FUserId write SetUserId;
    property FromId: string read FFromId write SetFromId;
    property Date: TDateTime read FDate write SetDate;
    property ReadState: TReadState read FReadState write SetReadState;
    property Out: TOutType read FOut write SetOut;
    property Title: string read FTitle write SetTitle;
    property Deleted: boolean read FDeleted write SetDeleted;
    property Emoji: boolean read FEmoji write SetEmoji;
  end;

  TMessagesObjectList = specialize TFPGObjectList<TMessage>;

  { TDialog }

  TDialog = class
  private
    FMessages: TMessagesObjectList;
    FPerson: TUser;
    procedure SetMessages(AValue: TMessagesObjectList);
    procedure SetPerson(AValue: TUser);
  public
    constructor Create;
    {Person to which you are talking}
    property Person: TUser read FPerson write SetPerson;
    property Messages: TMessagesObjectList read FMessages write SetMessages;
    destructor Destroy; override;
  end;

  TDialogsList = specialize TFPGList<TDialog>;

function StringToCommunityType(Str: string): TCommunityType;
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

{ TChatBot }

procedure TChatBot.SetCommands(AValue: TChatBotCommandsObjectList);
begin
  if FCommands = AValue then
    Exit;
  FCommands := AValue;
end;

constructor TChatBot.Create;
begin
  FCommands := TChatBotCommandsObjectList.Create(True);
end;

procedure TChatBot.AddCommand(Command: string; Response: string);
var
  NewCommand: TChatBotCommand;
begin
  NewCommand := TChatBotCommand.Create;
  NewCommand.Command := Command;
  NewCommand.Response := Response;
  FCommands.Add(NewCommand);
end;

destructor TChatBot.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
end;

{ TChatBotCommand }

procedure TChatBotCommand.SetCommand(AValue: string);
begin
  if FCommand = AValue then
    Exit;
  FCommand := AValue;
end;

procedure TChatBotCommand.SetResponse(AValue: string);
begin
  if FResponse = AValue then
    Exit;
  FResponse := AValue;
end;

{ TMessage }

procedure TMessage.SetMessage(AValue: string);
begin
  if FMessage = AValue then
    Exit;
  FMessage := Trim(AValue);
end;

procedure TMessage.SetDate(AValue: TDateTime);
begin
  if FDate = AValue then
    Exit;
  FDate := AValue;
end;

procedure TMessage.SetDeleted(AValue: boolean);
begin
  if FDeleted = AValue then
    Exit;
  FDeleted := AValue;
end;

procedure TMessage.SetEmoji(AValue: boolean);
begin
  if FEmoji = AValue then
    Exit;
  FEmoji := AValue;
end;

procedure TMessage.SetFromId(AValue: string);
begin
  if FFromId = AValue then
    Exit;
  FFromId := AValue;
end;

procedure TMessage.SetId(AValue: string);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TMessage.SetOut(AValue: TOutType);
begin
  if FOut = AValue then
    Exit;
  FOut := AValue;
end;

procedure TMessage.SetReadState(AValue: TReadState);
begin
  if FReadState = AValue then
    Exit;
  FReadState := AValue;
end;

procedure TMessage.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure TMessage.SetUserId(AValue: string);
begin
  if FUserId = AValue then
    Exit;
  FUserId := AValue;
end;

{ TUser }

procedure TUser.SetFirstName(AValue: string);
begin
  if FFirstName = AValue then
    Exit;
  FFirstName := AValue;
end;

procedure TUser.SetId(AValue: string);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TUser.SetLastName(AValue: string);
begin
  if FLastName = AValue then
    Exit;
  FLastName := AValue;
end;

procedure TUser.SetPhoto(AValue: TPicture);
begin
  if FPhoto = AValue then
    Exit;
  FPhoto := AValue;
end;

procedure TUser.SetPhoto200(AValue: TPicture);
begin
  if FPhoto200 = AValue then
    Exit;
  FPhoto200 := AValue;
end;

{ TDialog }

procedure TDialog.SetPerson(AValue: TUser);
begin
  if FPerson = AValue then
    Exit;
  FPerson := AValue;
end;

constructor TDialog.Create;
begin
  FMessages := TMessagesObjectList.Create(True);
end;

destructor TDialog.Destroy;
begin
  FreeAndNil(FMessages);
  inherited Destroy;
end;

procedure TDialog.SetMessages(AValue: TMessagesObjectList);
begin
  if FMessages = AValue then
    Exit;
  FMessages := AValue;
end;

{ TCommunity }

procedure TCommunity.SetName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TCommunity.SetPhoto(AValue: TPicture);
begin
  if FPhoto = AValue then
    Exit;
  FPhoto := AValue;
end;

procedure TCommunity.SetScreenName(AValue: string);
begin
  if FScreenName = AValue then
    Exit;
  FScreenName := AValue;
end;

constructor TCommunity.Create;
begin
  FChatbot := TChatBot.Create;
end;

function TCommunity.Copy: TCommunity;
begin
  Result := TCommunity.Create;
  Result.AccessKey := Self.AccessKey;
  Result.CommunityType := Self.CommunityType;
  Result.Deactivated := Self.Deactivated;
  Result.HasPhoto := Self.HasPhoto;
  Result.Id := Self.Id;
  Result.IsClosed := Self.IsClosed;
  Result.Name := Self.Name;
  if Result.HasPhoto then
  begin
    Result.Photo := TPicture.Create;
    Result.Photo.Assign(Self.Photo);
  end;
end;

destructor TCommunity.Destroy;
begin
  FreeAndNil(FChatbot);
  inherited Destroy;
end;

procedure TCommunity.SetId(AValue: string);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TCommunity.SetDeactivated(AValue: boolean);
begin
  if FDeactivated = AValue then
    Exit;
  FDeactivated := AValue;
end;

procedure TCommunity.SetHasPhoto(AValue: boolean);
begin
  if FHasPhoto = AValue then
    Exit;
  FHasPhoto := AValue;
end;

procedure TCommunity.SetCommunityType(AValue: TCommunityType);
begin
  if FCommunityType = AValue then
    Exit;
  FCommunityType := AValue;
end;

procedure TCommunity.SetAccessKey(AValue: string);
begin
  if FAccessKey = AValue then
    Exit;
  FAccessKey := AValue;
end;

procedure TCommunity.SetChatbot(AValue: TChatBot);
begin
  if FChatbot = AValue then
    Exit;
  FChatbot := AValue;
end;

procedure TCommunity.SetIsClosed(AValue: boolean);
begin
  if FIsClosed = AValue then
    Exit;
  FIsClosed := AValue;
end;

end.
