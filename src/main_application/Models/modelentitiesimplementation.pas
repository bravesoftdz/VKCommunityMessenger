{This unit contains implementation of entites interfaces from
ModelDataModel}
unit ModelEntitiesImplementation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModelDataModel, Graphics;

type

  { TCommunity
    is an implementation of ICommunityInterface}
  TCommunity = class(TInterfacedObject, ICommunity)
  private
    FAccessKey: string;
    FChatBot: IChatBot;
    FCommunityType: TCommunityType;
    FDeactivated: boolean;
    FDialogs: TDialogsList;
    FHasPhoto: boolean;
    FID: string;
    FIsClosed: boolean;
    FName: string;
    FScreenName: string;
    FPhoto: TPicture;
  public
    constructor Create(AAccessKey: string; AChatBot: IChatBot;
      ACommunityType: TCommunityType; ADeactivated: boolean;
      ADialogs: TDialogsList; AHasPhoto: boolean; AId: string;
      AIsClosed: boolean; AName: string; AScreenName: string; APhoto: TPicture);
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
    {SetDialogs frees old dialog list}
    procedure SetDialogs(AValue: TDialogsList);
    property Name: string read GetName;
    property Id: string read GetId;
    property ScreenName: string read GetScreenName;
    property IsClosed: boolean read GetIsClosed;
    property Deactivated: boolean read GetDeactivated;
    property CommunityType: TCommunityType read GetCommunityType;
    property HasPhoto: boolean read GetHasPhoto;
    property Photo: TPicture read GetPhoto;
    property AccessKey: string read GetAccessKey;
    property Chatbot: IChatBot read GetChatbot;
    property Dialogs: TDialogsList read GetDialogs write SetDialogs;
    destructor Destroy; override;
  end;

  { TChatBot }

  TChatBot = class(TInterfacedObject, IChatBot)
  private
    FCommands: TChatBotCommandsList;
  public
    constructor Create(ACommands: TChatBotCommandsList);
    function GetCommands: TChatBotCommandsList;
    property Commands: TChatBotCommandsList read GetCommands;
    {Frees commands}
    destructor Destroy; override;
  end;

  { TChatBotCommand }

  TChatBotCommand = class(TInterfacedObject, IChatBotCommand)
  private
    FCommand: string;
    FResponse: string;
  public
    constructor Create(ACommand: string; AResponse: string);
    function GetCommand: string;
    function GetResponse: string;
    property Command: string read GetCommand;
    property Response: string read GetResponse;
  end;

  { TDialog }

  TDialog = class(TInterfacedObject, IDialog)
  private
    FMessages: TMessageList;
    FPerson: IUser;
  public
    constructor Create(APerson: IUser; AMessages: TMessageList);
    function GetMessages: TMessageList;
    function GetPerson: IUser;
    property Person: IUser read GetPerson;
    property Messages: TMessageList read GetMessages;
    {Frees messages}
    destructor Destroy; override;
  end;

  { TMessage }

  TMessage = class(TInterfacedObject, IMessage)
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
  public
    constructor Create(ADate: TDateTime; ADeleted: boolean; AEmoji: boolean;
      AFromId: string; AId: string; AMessage: string; AOut: TOutType;
      AReadState: TReadState; ATitle: string);
    function GetDate: TDateTime;
    function GetDeleted: boolean;
    function GetEmoji: boolean;
    function GetFromId: string;
    function GetId: string;
    function GetMessage: string;
    function GetOut: TOutType;
    function GetReadState: TReadState;
    function GetTitle: string;
    property Id: string read GetId;
    property Message: string read GetMessage;
    property Date: TDateTime read GetDate;
    property ReadState: TReadState read GetReadState;
    property Out: TOutType read GetOut;
    property Title: string read GetTitle;
    property Deleted: boolean read GetDeleted;
    property Emoji: boolean read GetEmoji;
  end;

  { TUser }

  TUser = class(TInterfacedObject, IUser)
    FFirstName: string;
    Fid: string;
    FLastName: string;
    FPicture200: TPicture;
    FPicture50: TPicture;
  public
    constructor Create(AFirstName: string; AId: string; ALastName: string;
      APicture200: TPicture; APicture50: TPicture);
    function GetFirstName: string;
    function GetId: string;
    function GetLastName: string;
    function GetPicture200: TPicture;
    function GetPicture50: TPicture;
    property Id: string read GetId;
    property FirstName: string read GetFirstName;
    property LastName: string read GetLastName;
    property Picture50: TPicture read GetPicture50;
    property Picture200: TPicture read GetPicture200;
    {Frees Picture200 and Picture50}
    destructor Destroy; overload;
  end;

implementation

{ TUser }

constructor TUser.Create(AFirstName: string; AId: string; ALastName: string;
  APicture200: TPicture; APicture50: TPicture);
begin
  FFirstName := AFirstName;
  Fid := AId;
  FPicture50 := APicture50;
  FPicture200 := APicture200;
end;

function TUser.GetFirstName: string;
begin
  Result := FFirstName;
end;

function TUser.GetId: string;
begin
  Result := Fid;
end;

function TUser.GetLastName: string;
begin
  Result := FLastName;
end;

function TUser.GetPicture200: TPicture;
begin
  Result := FPicture200;
end;

function TUser.GetPicture50: TPicture;
begin
  Result := FPicture50;
end;

destructor TUser.Destroy;
begin
  if Assigned(FPicture50) then
    FreeAndNil(FPicture50);
  if Assigned(FPicture200) then
    FreeAndNil(FPicture200);
end;

{ TMessage }

constructor TMessage.Create(ADate: TDateTime; ADeleted: boolean;
  AEmoji: boolean; AFromId: string; AId: string; AMessage: string;
  AOut: TOutType; AReadState: TReadState; ATitle: string);
begin
  FDate := ADate;
  FDeleted := ADeleted;
  FEmoji := AEmoji;
  FFromId := AFromId;
  FId := AId;
  FMessage := AMessage;
  FOut := AOut;
  FReadState := AReadState;
  FTitle := ATitle;
end;

function TMessage.GetDate: TDateTime;
begin
  Result := FDate;
end;

function TMessage.GetDeleted: boolean;
begin
  Result := FDeleted;
end;

function TMessage.GetEmoji: boolean;
begin
  Result := FEmoji;
end;

function TMessage.GetFromId: string;
begin
  Result := FFromId;
end;

function TMessage.GetId: string;
begin
  Result := FId;
end;

function TMessage.GetMessage: string;
begin
  Result := FMessage;
end;

function TMessage.GetOut: TOutType;
begin
  Result := FOut;
end;

function TMessage.GetReadState: TReadState;
begin
  Result := FReadState;
end;

function TMessage.GetTitle: string;
begin
  Result := FTitle;
end;

{ TDialog }

constructor TDialog.Create(APerson: IUser; AMessages: TMessageList);
begin
  FPerson := APerson;
  AMessages := AMessages;
end;

function TDialog.GetMessages: TMessageList;
begin
  Result := FMessages;
end;

function TDialog.GetPerson: IUser;
begin
  Result := FPerson;
end;

destructor TDialog.Destroy;
begin
  FPerson := nil;
  FreeAndNil(FMessages);
  inherited Destroy;
end;

{ TChatBotCommand }

constructor TChatBotCommand.Create(ACommand: string; AResponse: string);
begin
  FCommand := ACommand;
  FResponse := AResponse;
end;

function TChatBotCommand.GetCommand: string;
begin
  Result := FCommand;
end;

function TChatBotCommand.GetResponse: string;
begin
  Result := FResponse;
end;

{ TChatBot }

constructor TChatBot.Create(ACommands: TChatBotCommandsList);
begin
  FCommands := ACommands;
end;

function TChatBot.GetCommands: TChatBotCommandsList;
begin
  Result := FCommands;
end;

destructor TChatBot.Destroy;
begin
  FreeAndNil(FCommands);
end;

{ TCommunity }

constructor TCommunity.Create(AAccessKey: string; AChatBot: IChatBot;
  ACommunityType: TCommunityType; ADeactivated: boolean; ADialogs: TDialogsList;
  AHasPhoto: boolean; AId: string; AIsClosed: boolean; AName: string;
  AScreenName: string; APhoto: TPicture);
begin
  FAccessKey := AAccessKey;
  FChatBot := AChatBot;
  FCommunityType := ACommunityType;
  FDeactivated := ADeactivated;
  FDialogs := ADialogs;
  FHasPhoto := AHasPhoto;
  FID := AId;
  FIsClosed := AIsClosed;
  FName := AName;
  FScreenName := AScreenName;
  FPhoto := APhoto;
end;

function TCommunity.GetAccessKey: string;
begin
  Result := FAccessKey;
end;

function TCommunity.GetChatbot: IChatBot;
begin
  Result := FChatBot;
end;

function TCommunity.GetCommunityType: TCommunityType;
begin
  Result := FCommunityType;
end;

function TCommunity.GetDeactivated: boolean;
begin
  Result := FDeactivated;
end;

function TCommunity.GetDialogs: TDialogsList;
begin
  Result := FDialogs;
end;

function TCommunity.GetHasPhoto: boolean;
begin
  Result := FHasPhoto;
end;

function TCommunity.GetId: string;
begin
  Result := FID;
end;

function TCommunity.GetIsClosed: boolean;
begin
  Result := FIsClosed;
end;

function TCommunity.GetName: string;
begin
  Result := FName;
end;

function TCommunity.GetPhoto: TPicture;
begin
  Result := FPhoto;
end;

function TCommunity.GetScreenName: string;
begin
  Result := FScreenName;
end;

procedure TCommunity.SetDialogs(AValue: TDialogsList);
begin
  FreeAndNil(FDialogs);
  FDialogs := AValue;
end;

destructor TCommunity.Destroy;
begin
  FChatBot := nil;
  FDialogs := nil;
  inherited Destroy;
end;

end.
