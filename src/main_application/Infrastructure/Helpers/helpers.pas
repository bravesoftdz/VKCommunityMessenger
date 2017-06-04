unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TVKCMComponentHelper }

  TVKCMComponentHelper = class helper for TComponent
  private
    function GetDataObject: TObject;
    procedure SetDataObject(AValue: TObject);
  public
    property DataObject: TObject read GetDataObject write SetDataObject;
  end;

implementation

{ TVKCMComponentHelper }

procedure TVKCMComponentHelper.SetDataObject(AValue: TObject);
begin
  Tag:=PtrInt(AValue);
end;

function TVKCMComponentHelper.GetDataObject: TObject;
begin
  Result := TObject(Tag);
end;

end.

