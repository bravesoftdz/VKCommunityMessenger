unit AbstractViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, vkgsobserver;

type

  { IViewModel }

  IViewModel = interface
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
    property Model: IModel read GetModel write SetModel;
  end;

  { TBaseViewModel }

  { TObserverViewModel }

  TObserverViewModel = class(TInterfacedObject, IViewModel)
  private
    procedure SetObservable(AValue: TVKGSObservable);
    procedure SetObserver(AValue: TVKGSObserver);
  protected
    FObserver: TVKGSObserver;
    FObservable: TVKGSObservable;
    FModel: IModel;
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
  public
    constructor Create;
    property Model: IModel read GetModel write SetModel;
    property Observable: TVKGSObservable read FObservable write SetObservable;
    property Observer: TVKGSObserver read FObserver write SetObserver;
    destructor Destroy; override;
  end;

implementation

{ TObserverViewModel }

procedure TObserverViewModel.SetObservable(AValue: TVKGSObservable);
begin
  if FObservable=AValue then Exit;
  FObservable:=AValue;
end;

procedure TObserverViewModel.SetObserver(AValue: TVKGSObserver);
begin
  if FObserver=AValue then Exit;
  FObserver:=AValue;
end;

function TObserverViewModel.GetModel: IModel;
begin
 Result := FModel;
end;

procedure TObserverViewModel.SetModel(AValue: IModel);
begin
  if AValue = FModel then
     exit;
  FModel := nil;
  FModel := AValue;
  if (FModel is TObserverModel) then
     Observer.Subscribe((FModel as TObserverModel).Observable);
end;

constructor TObserverViewModel.Create;
begin
  Observable := TVKGSObservable.Create;
  Observer := TVKGSObserver.Create;
end;

destructor TObserverViewModel.Destroy;
begin
  FreeAndNil(FObserver);
  FreeAndNil(FObservable);
  inherited Destroy;
end;

end.

