unit vkcmobserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TVKCMObserver = class;

  TVKCMObserversList = specialize TFPGList<TVKCMObserver>;

  { TVKCMObservable }

  TVKCMObservable = class
  private
    FObservers: TVKCMObserversList;
    procedure SetObservers(AValue: TVKCMObserversList);
  public
    constructor Create;
    procedure NotifyObservers;
    property Observers: TVKCMObserversList read FObservers write SetObservers;
    destructor Destroy; override;
  end;

  TVKCMNotifyEvent = procedure of object;

  { TVKCMObserver }

  TVKCMObserver = class
  private
    FNotify: TVKCMNotifyEvent;
    procedure SetNotify(AValue: TVKCMNotifyEvent);
  public
    property Notify: TVKCMNotifyEvent read FNotify write SetNotify;
    procedure Subscribe(Observable: TVKCMObservable);
    procedure Unsubscribe(Observable: TVKCMObservable);
  end;

implementation

{ TVKCMObserver }

procedure TVKCMObserver.SetNotify(AValue: TVKCMNotifyEvent);
begin
  if FNotify = AValue then
    Exit;
  FNotify := AValue;
end;

procedure TVKCMObserver.Subscribe(Observable: TVKCMObservable);
begin
  Observable.Observers.Add(Self);
end;

procedure TVKCMObserver.Unsubscribe(Observable: TVKCMObservable);
begin
  Observable.Observers.Remove(Self);
end;

{ TVKCMObservable }

procedure TVKCMObservable.SetObservers(AValue: TVKCMObserversList);
begin
  if FObservers = AValue then
    Exit;
  FObservers := AValue;
end;

constructor TVKCMObservable.Create;
begin
  Observers := TVKCMObserversList.Create;
end;

procedure TVKCMObservable.NotifyObservers;
var
  i: integer;
begin
  for i := 0 to Observers.Count - 1 do
    Observers[i].Notify;
end;

destructor TVKCMObservable.Destroy;
begin
  FreeAndNil(FObservers);
  inherited Destroy;
end;

end.
