unit AbstractModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vkgsobserver;

type
  IModel = interface
    ['{B5F05DB1-2E42-485F-9318-A27EB0155148}']
  end;

  { TObserverModel }

  TObserverModel = class(TInterfacedObject, IModel)
  private
    FObservable: TVKGSObservable;
    FObserver: TVKGSObserver;
    procedure SetObservable(AValue: TVKGSObservable);
    procedure SetObserver(AValue: TVKGSObserver);
  published
     property Observer: TVKGSObserver read FObserver write SetObserver;
    property Observable: TVKGSObservable read FObservable write SetObservable;
  end;

implementation

{ TObserverModel }

procedure TObserverModel.SetObservable(AValue: TVKGSObservable);
begin
  if FObservable=AValue then Exit;
  FObservable:=AValue;
end;

procedure TObserverModel.SetObserver(AValue: TVKGSObserver);
begin
  if FObserver=AValue then Exit;
  FObserver:=AValue;
end;

end.
