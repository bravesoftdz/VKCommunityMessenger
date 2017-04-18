unit chatviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, AbstractModel, ChatModel, entities, Graphics;

type

  { IChatViewModel }

  IChatViewModel = interface(IViewModel)
    function GetSendButtonCaption: string;
    {Get users which have to be assigned to tabs in tabview}
    function GetUsersForTabs(Community: TCommunity): TUserList;
    function GetUserById(Id: string): TUser;
    function GetSendButtonGlyph: TBitmap;
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
    function GetUsersForTabs(Community: TCommunity): TUserList;
    function GetUserById(Id: string): TUser;
    function GetSendButtonGlyph: TBitmap;
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

function TChatViewModel.GetUsersForTabs(Community: TCommunity): TUserList;
var
  NewUser: TUser;
begin
  Result := TUserList.Create;
  NewUser := TUser.Create;
  NewUser.CityId := '666';
  NewUser.CityTitle := 'Moscow';
  NewUser.FirstName := 'Нубито';
  NewUser.Id := 'anubis';
  NewUser.LastName := 'Гангстерито';
  NewUser.Verified := False;
  Result.Add(NewUser);
end;

function TChatViewModel.GetUserById(Id: string): TUser;
begin

end;

function TChatViewModel.GetSendButtonGlyph: TBitmap;
begin
  Result := (Model as TChatModel).GetSendPicture.Bitmap;
end;

end.
