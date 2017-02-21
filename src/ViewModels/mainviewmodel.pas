unit MainViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, Controls, Model;

type
  IMainViewModel = interface(IViewModel)
     procedure FillImageCommunitiesList(var List: TImageList);
  end;

  { TMainViewModel }

  TMainViewModel = class(TInterfacedObject, IMainViewModel)
  private
    FModel: IModel;
    procedure SetModel(AValue: IModel);
    function GetModel: IModel;
  public
    procedure FillImageCommunitiesList(var List: TImageList);
    property Model: IModel read GetModel write SetModel;
  end;

  var LMainViewModel: TMainViewModel;

implementation

{ TMainViewModel }

procedure TMainViewModel.SetModel(AValue: IModel);
begin
  if FModel=AValue then Exit;
  FModel:=AValue;
end;

function TMainViewModel.GetModel: IModel;
begin
  Result:=FModel;
end;

procedure TMainViewModel.FillImageCommunitiesList(var List: TImageList);
begin

end;

end.

