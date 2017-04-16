unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel;

type

  { IChatModel }

  IChatModel = interface(IModel)
    ['{F58AF832-F8DE-46DF-AC0C-85B19585DB0E}']
    function GetSendButtonName: string;
  end;

  { TChatModel }

  TChatModel = class(TInterfacedObject, IChatModel, IModel)
    function GetSendButtonName: string;
  end;

var
  LChatModel: TChatModel;

implementation

{ TChatModel }

function TChatModel.GetSendButtonName: string;
begin
  Result := 'Отправить';
end;

end.
