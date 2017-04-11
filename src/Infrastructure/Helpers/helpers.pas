unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TVKGSComponentHelper }

  TVKGSComponentHelper = class helper for TComponent
  private
    function GetDataObject: TObject;
    procedure SetDataObject(AValue: TObject);
  public
    property DataObject: TObject read GetDataObject write SetDataObject;
  end;

implementation

{ TVKGSComponentHelper }

procedure TVKGSComponentHelper.SetDataObject(AValue: TObject);
begin
  Tag:=PtrInt(AValue);
end;

function TVKGSComponentHelper.GetDataObject: TObject;
begin
  Result := TObject(Tag);
end;

end.

