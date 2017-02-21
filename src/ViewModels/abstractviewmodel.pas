unit AbstractViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Model;

type

  { IViewModel }

  IViewModel = interface
    function GetModel: IModel;
    procedure SetModel(AValue: IModel);
    property Model: IModel read GetModel write SetModel;
  end;

implementation

end.

