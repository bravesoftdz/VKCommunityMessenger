unit welcomepagemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, fphttpclient, VKGSConfig;

type

  IWelcomePageModel = interface(IModel)
    {Returns a title of text for welcome page}
    function GetWelcomeTitle: string;
    {Returns a text of welcome page}
    function GetWelcomeText: string;
    {Returns text with news}
    function GetNews: string;
  end;

  { TWelcomePageModel }

  TWelcomePageModel = class(TInterfacedObject, IWelcomePageModel, IModel)
    function GetWelcomeTitle: string;
    function GetWelcomeText: string;
    function GetNews: string;
  end;

var
  LWelcomePageModel: TWelcomePageModel;

implementation

{ TWelcomePageModel }

function TWelcomePageModel.GetWelcomeTitle: string;
begin
  Result := 'Добро пожаловать в VKCommunityMessenger!';
end;

function TWelcomePageModel.GetWelcomeText: string;
begin
  Result :=
    'Нажмите на +, чтобы добавить новое сообщество. ' +
    'Для добавления сообщества необходим ключ доступа к сообществу,' +
    'который вы можете создать в настройках сообщества в разделе "Работа с API"';
end;

function TWelcomePageModel.GetNews: string;
var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    try
      Result := Client.Get(NEWS_WEBPAGE);
    finally
      FreeAndNil(Client);
    end;
  except
    Result := 'Невозможно подключиться к сервису новостей';
  end;
end;

end.
