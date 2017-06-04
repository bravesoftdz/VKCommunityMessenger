unit AbstractModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vkcmobserver;

type
  IModel = interface
    ['{B5F05DB1-2E42-485F-9318-A27EB0155148}']
  end;

  { TObserverModel }

  TObserverModel = class(TInterfacedObject, IModel)
  private
    FObservable: TVKCMObservable;
    FObserver: TVKCMObserver;
    procedure SetObservable(AValue: TVKCMObservable);
    procedure SetObserver(AValue: TVKCMObserver);
  published
     property Observer: TVKCMObserver read FObserver write SetObserver;
    property Observable: TVKCMObservable read FObservable write SetObservable;
  end;

implementation

{ TObserverModel }

procedure TObserverModel.SetObservable(AValue: TVKCMObservable);
begin
  if FObservable=AValue then Exit;
  FObservable:=AValue;
end;

procedure TObserverModel.SetObserver(AValue: TVKCMObserver);
begin
  if FObserver=AValue then Exit;
  FObserver:=AValue;
end;

end.
