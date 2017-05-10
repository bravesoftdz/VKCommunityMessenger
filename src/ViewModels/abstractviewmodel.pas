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
  protected
    Observer: TVKGSObserver;
    Observable: TVKGSObservable;
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
  public
    constructor Create;
    property Model: IModel read GetModel write SetModel;
    destructor Destroy; override;
  end;

implementation

{ TObserverViewModel }

function TObserverViewModel.GetModel: IModel;
begin

end;

procedure TObserverViewModel.SetModel(AValue: IModel);
begin

end;

constructor TObserverViewModel.Create;
begin
  Observable := TVKGSObservable.Create;
  Observer := TVKGSObserver.Create;
end;

destructor TObserverViewModel.Destroy;
begin
  FreeAndNil(Observer);
  FreeAndNil(Observable);
  inherited Destroy;
end;

end.

