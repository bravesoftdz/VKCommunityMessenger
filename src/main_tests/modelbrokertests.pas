unit ModelBrokerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ModelDataModel, ModelBroker;

type

  { TBrokerTest }

  TBrokerTest = class(TTestCase)
  private
    FVariable: boolean;
    FBroker: IBroker;
    FObserver: IObserver;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  public
    procedure TestNotify;
  published
    procedure SubscribeAndNotify;
    procedure SubscribeFromBroker;
  end;

implementation



{ TBrokerTest }

procedure TBrokerTest.TestNotify;
begin
  FVariable := True;
end;

procedure TBrokerTest.Setup;
begin
  FVariable := False;
  FBroker := TModelBroker.Create;
  FObserver := TModelObserver.Create;
  FObserver.Notify := @TestNotify;
end;

procedure TBrokerTest.TearDown;
begin
  FBroker := nil;
  FObserver := nil;
end;

procedure TBrokerTest.SubscribeAndNotify;
begin
  FObserver.Subscribe(FBroker);
  FObserver.MessageToBrokers;
  Sleep(1000);
  AssertTrue(FVariable=true);
end;

procedure TBrokerTest.SubscribeFromBroker;
begin
  FBroker.Subscribe(FObserver);
  FObserver.MessageToBrokers;
  Sleep(1000);
  AssertTrue(FVariable=true);
end;

initialization

  RegisterTest(TBrokerTest);
end.

