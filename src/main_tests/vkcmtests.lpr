program vkcmtests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, ModelBrokerTests,
  ModelAPI, ModelStorage, StorageTests, modelentitiesimplementation;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

