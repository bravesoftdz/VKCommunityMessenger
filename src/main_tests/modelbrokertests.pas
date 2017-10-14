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
    procedure TestNotify;
    procedure TestSubscribe;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    {Observer should be capable to
     subscribe yourself to broker and
     notify all observers}
    procedure SubscribeAndNotify;
    {Broker should be able to subscribe an observer
     and notify all observers, but in this case observer
     can't notify broker}
    procedure SubscribeFromBroker;
    {If one observer will subscribe yourself more than
     once then broker should raise an exception}
    procedure DoubleSubscribe;
  end;

implementation

{ TBrokerTest }

procedure TBrokerTest.TestNotify;
begin
  FVariable := True;
end;

procedure TBrokerTest.TestSubscribe;
begin
  FBroker.Subscribe(FObserver);
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
  AssertTrue(FVariable = True);
end;

procedure TBrokerTest.SubscribeFromBroker;
begin
  FBroker.Subscribe(FObserver);
  FBroker.NotifyAllObservers;
  Sleep(1000);
  AssertTrue(FVariable = True);
end;

procedure TBrokerTest.DoubleSubscribe;
begin
  FObserver.Subscribe(FBroker);
  AssertException(Exception,@TestSubscribe);
end;

initialization

  RegisterTest(TBrokerTest);
end.
