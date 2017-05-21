unit vkgsobserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TVKGSObserver = class;

  TVKGSObserversList = specialize TFPGList<TVKGSObserver>;

  { TVKGSObservable }

  TVKGSObservable = class
  private
    FObservers: TVKGSObserversList;
    procedure SetObservers(AValue: TVKGSObserversList);
  public
    constructor Create;
    procedure NotifyObservers;
    property Observers: TVKGSObserversList read FObservers write SetObservers;
    destructor Destroy; override;
  end;

  TVKGSNotifyEvent = procedure of object;

  { TVKGSObserver }

  TVKGSObserver = class
  private
    FNotify: TVKGSNotifyEvent;
    procedure SetNotify(AValue: TVKGSNotifyEvent);
  public
    property Notify: TVKGSNotifyEvent read FNotify write SetNotify;
    procedure Subscribe(Observable: TVKGSObservable);
    procedure Unsubscribe(Observable: TVKGSObservable);
  end;

implementation

{ TVKGSObserver }

procedure TVKGSObserver.SetNotify(AValue: TVKGSNotifyEvent);
begin
  if FNotify = AValue then
    Exit;
  FNotify := AValue;
end;

procedure TVKGSObserver.Subscribe(Observable: TVKGSObservable);
begin
  Observable.Observers.Add(Self);
end;

procedure TVKGSObserver.Unsubscribe(Observable: TVKGSObservable);
begin
  Observable.Observers.Remove(Self);
end;

{ TVKGSObservable }

procedure TVKGSObservable.SetObservers(AValue: TVKGSObserversList);
begin
  if FObservers=AValue then Exit;
  FObservers:=AValue;
end;

constructor TVKGSObservable.Create;
begin
  Observers := TVKGSObserversList.Create;
end;

procedure TVKGSObservable.NotifyObservers;
var
  i: integer;
begin
  for i := 0 to Observers.Count - 1 do
    Observers[i].Notify;
end;

destructor TVKGSObservable.Destroy;
begin
  FreeAndNil(FObservers);
  inherited Destroy;
end;

end.
