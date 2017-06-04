unit AbstractViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, vkcmobserver;

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
    procedure SetObservable(AValue: TVKCMObservable);
    procedure SetObserver(AValue: TVKCMObserver);
  protected
    FObserver: TVKCMObserver;
    FObservable: TVKCMObservable;
    FModel: IModel;
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
  public
    constructor Create;
    property Model: IModel read GetModel write SetModel;
    property Observable: TVKCMObservable read FObservable write SetObservable;
    property Observer: TVKCMObserver read FObserver write SetObserver;
    destructor Destroy; override;
  end;

implementation

{ TObserverViewModel }

procedure TObserverViewModel.SetObservable(AValue: TVKCMObservable);
begin
  if FObservable=AValue then Exit;
  FObservable:=AValue;
end;

procedure TObserverViewModel.SetObserver(AValue: TVKCMObserver);
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
  Observable := TVKCMObservable.Create;
  Observer := TVKCMObserver.Create;
end;

destructor TObserverViewModel.Destroy;
begin
  FreeAndNil(FObserver);
  FreeAndNil(FObservable);
  inherited Destroy;
end;

end.

