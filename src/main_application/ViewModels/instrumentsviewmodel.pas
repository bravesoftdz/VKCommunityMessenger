unit instrumentsviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, AbstractModel, entities, MainModel;

type

  { TInstrumentsViewModel }

  TInstrumentsViewModel = class(TInterfacedObject, IViewModel)
  private
    FModel: IModel;
    procedure SetModel(AModel: IModel);
    function GetModel: IModel;
  public
    procedure UpdateCommunityForChatBotSubSystem(Community: TCommunity);
    property Model: IModel read GetModel write SetModel;
  end;

var
  LInstrumentsViewModel: TInstrumentsViewModel;

implementation

{ TInstrumentsViewModel }

procedure TInstrumentsViewModel.SetModel(AModel: IModel);
begin
  if FModel <> AModel then
    FModel := AModel;
end;

function TInstrumentsViewModel.GetModel: IModel;
begin
  Result := FModel;
end;

procedure TInstrumentsViewModel.UpdateCommunityForChatBotSubSystem(
  Community: TCommunity);
begin
  (FModel as TMainModel).SaveCommunityInfo(Community);
end;

end.
