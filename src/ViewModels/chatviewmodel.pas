unit chatviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, AbstractModel;

type
  IChatViewModel = interface(IViewModel)
  end;

  { TChatViewModel }

  TChatViewModel = class(TInterfacedObject, IViewModel, IChatViewModel)
  private
    FModel: IModel;
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
  published
    property Model: IModel read GetModel write SetModel;
  end;

implementation

{ TChatViewModel }

function TChatViewModel.GetModel: IModel;
begin
  Result := FModel;
end;

procedure TChatViewModel.SetModel(AValue: IModel);
begin
  if AValue <> FModel then
    FModel := AValue;
end;

end.

