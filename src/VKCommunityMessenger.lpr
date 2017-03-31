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
  MainModel,
  AbstractViewModel,
  entities,
  welcomepageview,
  AbstractModel;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  {Create models and viewmodels}
  LMainViewModel := TMainViewModel.Create;
  LModel := TMainModel.Create;

  Application.Initialize;

  {Create views}
  Application.CreateForm(TfMainView, LMainView);

  {Bind mvvm}
  LMainViewModel.Model := LModel;
  LMainView.ViewModel := LMainViewModel;

  LMainView.InitializeForm;
  Application.Run;
end.
