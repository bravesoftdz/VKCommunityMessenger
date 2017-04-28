unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, Graphics, fphttpclient, entities, VKDAO, fpjson;

type

  { IChatModel }

  IChatModel = interface(IModel)
    ['{F58AF832-F8DE-46DF-AC0C-85B19585DB0E}']
    function GetSendPicture: TPicture;
    function GetLastDialogs(Community: TCommunity): TDialogsList;
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
    function GetLastDialogs(Community: TCommunity): TDialogsList;
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

function TChatModel.GetLastDialogs(Community: TCommunity): TDialogsList;
var NewDialog: TDialog;
    JSONResponse: TJSONObject;
    Items: TJSONArray;
begin
  Result := TDialogsList.Create;
  JSONResponse:=DAO.Messages.GetDialogs(HTTPClient,Community.AccessKey,10,0);
  Items := JSONResponse['items'] as TJSONArray;

end;

destructor TChatModel.Destroy;
begin
  FreeAndNil(HTTPClient);
  inherited Destroy;
end;

end.
