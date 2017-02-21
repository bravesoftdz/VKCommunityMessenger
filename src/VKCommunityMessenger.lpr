program VKCommunityMessenger;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainView { you can add units after this },
  Controls,
  Graphics,
  MainViewModel,
  Model,
  AbstractViewModel, entities;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  {Create models and viewmodels}
  LMainViewModel:=TMainViewModel.Create;

  Application.Initialize;

  {Create views}
  Application.CreateForm(TfMainView, fMainView);

  {Bind mvvm}
  fMainView.ViewModel:=LMainViewModel;
  fMainView.InitializeForm;

  Application.Run;
end.
