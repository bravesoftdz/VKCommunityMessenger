unit StorageTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ModelDataModel, ModelStorage;

type

  TMockDAO = class(TInterfacedObject, IDAOAdapter)

  end;

  { TStorageTests }

  TStorageTests = class(TTestCase)
  private
    FStorage: IStorageService;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published

  end;

var
  DAO: TMockDAO;

implementation

{ TStorageTests }

procedure TStorageTests.Setup;
begin
  FStorage := TModelStorageService.Create(DAO);
end;

procedure TStorageTests.TearDown;
begin
  FStorage := nil;
end;

initialization
  DAO := TMockDAO.Create;
  RegisterTest(TStorageTests);

finalization
  DAO := nil;

end.
