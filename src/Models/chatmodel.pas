unit chatmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractModel;

type
  IChatModel = interface(IModel)
  end;

  TChatModel = class(TInterfacedObject, IChatModel, IModel)

  end;

implementation

end.

