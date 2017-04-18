unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel, Graphics;

type

  { IChatModel }

  IChatModel = interface(IModel)
    ['{F58AF832-F8DE-46DF-AC0C-85B19585DB0E}']
    function GetSendButtonName: string;
    function GetSendPicture: TPicture;
  end;

  { TChatModel }

  TChatModel = class(TInterfacedObject, IChatModel, IModel)
    function GetSendPicture: TPicture;
    function GetSendButtonName: string;
  end;

var
  LChatModel: TChatModel;

implementation

{ TChatModel }

function TChatModel.GetSendPicture: TPicture;
begin
  Result := TPicture.Create;
  Result.LoadFromFile('.\img\send.bmp');
end;

function TChatModel.GetSendButtonName: string;
begin
  Result := 'Отправить';
end;

end.
