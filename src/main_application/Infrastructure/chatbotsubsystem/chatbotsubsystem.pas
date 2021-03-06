unit chatbotsubsystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vkcmobserver, MainModel, ChatModel, entities;

type

  { TChatBotSystemWorker }

  TChatBotSystemWorker = class(TThread)
  private
    FChanged: boolean;
    FChatModel: IChatModel;
    FMainModel: IMainModel;
    procedure SetChanged(AValue: boolean);
    procedure SetChatModel(AValue: IChatModel);
    procedure SetMainModel(AValue: IMainModel);
  protected
    procedure Execute; override;
  public
    property Changed: boolean read FChanged write SetChanged;
    property MainModel: IMainModel read FMainModel write SetMainModel;
    property ChatModel: IChatModel read FChatModel write SetChatModel;
  end;

  { TChatBotSubSystem }

  TChatBotSubSystem = class
  private
    FObserver: TVKCMObserver;
    FWorker: TChatBotSystemWorker;
    procedure SetObserver(AValue: TVKCMObserver);
    procedure OnNotification;
  public
    constructor Create(AMainModel: IMainModel; AChatModel: IChatModel);
    property Observer: TVKCMObserver read FObserver write SetObserver;
    destructor Destroy; override;
  end;

var
  LChatBotSubSystem: TChatBotSubSystem;

implementation

{ TChatBotSystemWorker }

procedure TChatBotSystemWorker.SetChanged(AValue: boolean);
begin
  if FChanged = AValue then
    Exit;
  FChanged := AValue;
end;

procedure TChatBotSystemWorker.SetChatModel(AValue: IChatModel);
begin
  if FChatModel = AValue then
    Exit;
  FChatModel := AValue;
end;

procedure TChatBotSystemWorker.SetMainModel(AValue: IMainModel);
begin
  if FMainModel = AValue then
    Exit;
  FMainModel := AValue;
end;

procedure TChatBotSystemWorker.Execute;
var
  CommunityList: TCommunityList;
  Community: TCommunity;
  Dialogs: TDialogsList;
  Dialog: TDialog;
  Command: TChatBotCommand;
  i, j, k: integer;
  NewMessage: TMessage;
  RecievedMessage: string;
begin
  while not Terminated do
    if Changed then
    begin
      CommunityList := MainModel.GetCommunities;
      for i := 0 to CommunityList.Count - 1 do
      begin
        Community := CommunityList[i];
        Dialogs := ChatModel.GetLastDialogs(Community);
        for j := 0 to Dialogs.Count - 1 do
        begin
          Dialog := Dialogs[j];
          if Dialog.Messages[Dialog.Messages.Count - 1].Out = otRecieved then
          begin
            RecievedMessage := Dialog.Messages[Dialog.Messages.Count - 1].Message;
            for k := 0 to Community.Chatbot.Commands.Count - 1 do
            begin
              Command := Community.Chatbot.Commands[k];
              if SameText(RecievedMessage, Command.Command) then
              begin
                NewMessage := TMessage.Create;
                NewMessage.Message := Command.Response;
                NewMessage.Date := Now();
                NewMessage.Deleted := False;
                NewMessage.Emoji := False;
                NewMessage.FromId := Community.Id;
                NewMessage.UserId := Dialog.Person.Id;
                NewMessage.Out := otSent;
                ChatModel.SendMessage(Community, NewMessage);
              end;
            end;
          end;
        end;
      end;
      Changed := false;
    end;
end;

{ TChatBotSubSystem }

procedure TChatBotSubSystem.SetObserver(AValue: TVKCMObserver);
begin
  if FObserver = AValue then
    Exit;
  FObserver := AValue;
end;

procedure TChatBotSubSystem.OnNotification;
begin
  FWorker.Changed := True;
end;

constructor TChatBotSubSystem.Create(AMainModel: IMainModel; AChatModel: IChatModel);
begin
  FWorker := TChatBotSystemWorker.Create(True);
  FWorker.MainModel := AMainModel;
  FWorker.ChatModel := AChatModel;
  FWorker.FreeOnTerminate := True;
  FWorker.Changed := False;
  FWorker.Start;
  FObserver := TVKCMObserver.Create;
  FObserver.Notify := @OnNotification;
end;

destructor TChatBotSubSystem.Destroy;
begin
  FWorker.Terminate;
  FWorker.WaitFor;
  FreeAndNil(FObserver);
  inherited Destroy;
end;

end.
