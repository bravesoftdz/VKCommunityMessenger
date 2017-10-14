unit ModelBroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModelDataModel, fgl, syncobjs;

type

  TObserverList = specialize TFPGInterfacedObjectList<IObserver>;
  TModelBroker = class;

  { TBrokerThread
    is a thread that is used in TModelBroker and notifies all observers }
  TBrokerThread = class(TThread)
  private
    FBroker: TModelBroker;
    procedure SetBroker(AValue: TModelBroker);
  protected
    procedure Execute; override;
  public
    { A broker that owns this thread }
    property Broker: TModelBroker read FBroker write SetBroker;
  end;

  { TModelBroker
    is a default implementation of IBroker }
  TModelBroker = class(TInterfacedObject, IBroker)
  private
    { List of subscribed observers }
    FSubscribedObservers: TObserverList;
    { This flag indicates whether observers should be notified }
    FNotify: boolean;
    { Critical section for access to other fields }
    FCS: TCriticalSection;
    { A thread that notifies all observers }
    FInternalThread: TBrokerThread;
  public
    constructor Create;
    procedure Subscribe(AObserver: IObserver);
    procedure Unsubscribe(AObserver: IObserver);
    procedure NotifyAllObservers;
    destructor Destroy; override;
  end;

  TBrokerList = specialize TFPGInterfacedObjectList<IBroker>;

  { TModelObserver
    is a default implementation of IObserver}
  TModelObserver = class(TInterfacedObject, IObserver)
  private
    {List of brokers for which this observer is subscribed
     WARNING: if you subscribe observer vie IBroker.Subscribe,
     this list will not be filled}
    FBrokerList: TBrokerList;
    FNotify: TVKCMNotifyEvent;
  public
    constructor Create;
    function GetNotify: TVKCMNotifyEvent;
    procedure SetNotify(AValue: TVKCMNotifyEvent);
    property Notify: TVKCMNotifyEvent read FNotify write SetNotify;
    procedure Subscribe(ABroker: IBroker);
    procedure Unsubscribe(ABroker: IBroker);
    procedure MessageToBrokers;
    procedure UnsubscribeFromAll;
    destructor Destroy; override;
  end;

{Instance of global broker for model}
function ModelBroker: IBroker;

implementation

var
  gBroker: IBroker;

function ModelBroker: IBroker;
begin
  if not Assigned(gBroker) then
    gBroker := TModelBroker.Create;
  Result := gBroker;
end;

{ TModelObserver }

constructor TModelObserver.Create;
begin
  FBrokerList := TBrokerList.Create;
end;

function TModelObserver.GetNotify: TVKCMNotifyEvent;
begin
  Result := FNotify;
end;

procedure TModelObserver.SetNotify(AValue: TVKCMNotifyEvent);
begin
  FNotify := AValue;
end;

procedure TModelObserver.Subscribe(ABroker: IBroker);
begin
  ABroker.Subscribe(Self);
  FBrokerList.Add(ABroker);
end;

procedure TModelObserver.Unsubscribe(ABroker: IBroker);
begin
  ABroker.Unsubscribe(Self);
  FBrokerList.Remove(ABroker);
end;

procedure TModelObserver.MessageToBrokers;
var
  i: integer;
begin
  for i := 0 to FBrokerList.Count - 1 do
  begin
    FBrokerList[i].NotifyAllObservers;
  end;
end;

procedure TModelObserver.UnsubscribeFromAll;
var
  i: integer;
begin
  for i := 0 to FBrokerList.Count - 1 do
  begin
    Unsubscribe(FBrokerList[i]);
  end;
end;

destructor TModelObserver.Destroy;
begin
  FreeAndNil(FBrokerList);
  inherited Destroy;
end;

{ TBrokerThread }

procedure TBrokerThread.SetBroker(AValue: TModelBroker);
begin
  if FBroker = AValue then
    Exit;
  FBroker := AValue;
end;

procedure TBrokerThread.Execute;
var
  i: integer;
begin
  if not Assigned(FBroker) then
    raise Exception.Create('Thread started with no broker');
  while not Terminated do
  begin
    FBroker.FCS.Acquire;
    try
      if FBroker.FNotify then
      begin
        FBroker.FNotify := False;
        for i := 0 to FBroker.FSubscribedObservers.Count - 1 do
        begin
          if Assigned(FBroker.FSubscribedObservers[i].Notify) then
            FBroker.FSubscribedObservers[i].Notify();
        end;
      end;
    finally
      FBroker.FCS.Release;
    end;
  end;
end;

{ TModelBroker }

constructor TModelBroker.Create;
begin
  FNotify := False;
  FCS := TCriticalSection.Create;
  FSubscribedObservers := TObserverList.Create;
  FInternalThread := TBrokerThread.Create(True);
  FInternalThread.Broker := Self;
  FInternalThread.FreeOnTerminate := False;
  FInternalThread.Start;
end;

procedure TModelBroker.Subscribe(AObserver: IObserver);
begin
  FCS.Enter;
  try
    if FSubscribedObservers.IndexOf(AObserver) <> -1 then
      raise Exception.Create(
        'Trying to subscribe an observer that is currently subscribed');
    FSubscribedObservers.Add(AObserver);
  finally
    FCS.Leave;
  end;
end;

procedure TModelBroker.Unsubscribe(AObserver: IObserver);
begin
  FCS.Enter;
  try
    FSubscribedObservers.Remove(AObserver);
  finally
    FCS.Leave;
  end;
end;

procedure TModelBroker.NotifyAllObservers;
begin
  FCS.Enter;
  try
    FNotify := True;
  finally
    FCS.Leave;
  end;
end;

destructor TModelBroker.Destroy;
begin
  FInternalThread.Terminate;
  FInternalThread.WaitFor;
  FreeAndNil(FInternalThread);
  FreeAndNil(FSubscribedObservers);
  FreeAndNil(FCS);
end;

end.
