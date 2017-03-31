unit welcomepagemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel;

type

  IWelcomePageModel = interface(IModel)
    {Returns a title of text for welcome page}
    function GetWelcomeTitle: string;
    {Returns a text of welcome page}
    function GetWelcomeText: string;
  end;

  { TWelcomePageModel }

  TWelcomePageModel = class(TInterfacedObject, IWelcomePageModel, IModel)
    function GetWelcomeTitle: string;
    function GetWelcomeText: string;
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
    'Нажмите на +, чтобы добавить новое сообщество.' +
    'Для добавления сообщества необходим ключ доступа к сообществу,' +
    'который вы можете создать в настройках сообщества в разделе "Работа с API"';
end;

end.
