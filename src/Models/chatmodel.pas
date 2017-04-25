unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, Graphics, fphttpclient, entities;

type

  { IChatModel }

  IChatModel = interface(IModel)
    ['{F58AF832-F8DE-46DF-AC0C-85B19585DB0E}']
    function GetSendPicture: TPicture;
    function GetLastDialogs: TDialogsList;
    function GetNoAvatarPicture: TPicture;
  end;

  { TChatModel }

  TChatModel = class(TInterfacedObject, IChatModel, IModel)
    private
    HTTPClient: TFPHTTPClient;
    public
    constructor Create;
    function GetSendPicture: TPicture;
    function GetNoAvatarPicture: TPicture;
    function GetLastDialogs: TDialogsList;
    destructor Destroy; override;
  end;

var
  LChatModel: TChatModel;

implementation

{ TChatModel }

constructor TChatModel.Create;
begin
  HTTPClient:=TFPHTTPClient.Create(nil);
end;

function TChatModel.GetSendPicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\send.png');
end;

function TChatModel.GetNoAvatarPicture: TPicture;
begin
  Result:= TPicture.Create;
  Result.LoadFromFile('.\img\no_userimage.png');
end;

function TChatModel.GetLastDialogs: TDialogsList;
begin
  Result := TDialogsList.Create;
end;

destructor TChatModel.Destroy;
begin
  FreeAndNil(HTTPClient);
  inherited Destroy;
end;

end.
