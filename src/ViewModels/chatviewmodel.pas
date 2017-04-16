unit chatviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, AbstractModel, ChatModel;

type

  { IChatViewModel }

  IChatViewModel = interface(IViewModel)
    function GetSendButtonCaption: string;
  end;

  { TChatViewModel }

  TChatViewModel = class(TInterfacedObject, IViewModel, IChatViewModel)
  private
    FModel: IModel;
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
  public
    property Model: IModel read GetModel write SetModel;
    function GetSendButtonCaption: string;
  end;

var
  LChatViewModel: TChatViewModel;

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

function TChatViewModel.GetSendButtonCaption: string;
begin
  Result := (Model as IChatModel).GetSendButtonName;
end;

end.
