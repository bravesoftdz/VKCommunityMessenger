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
  AbstractModel,
  welcomepageviewmodel,
  welcomepagemodel;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  {Create models and viewmodels}
  LMainViewModel := TMainViewModel.Create;
  LMainModel := TMainModel.Create;
  LWelcomePageModel := TWelcomePageModel.Create;
  LWelcomePageViewModel := TWelcomePageViewModel.Create;

  Application.Initialize;

  {Create views}
  Application.CreateForm(TfMainView, LMainView);
  LWelcomePageView := TFrame1.Create(LMainView);
  LWelcomePageView.Parent := LMainView;

  {Bind mvvm}
  LMainViewModel.Model := LMainModel;
  LMainView.ViewModel := LMainViewModel;
  LWelcomePageView.ViewModel := LWelcomePageViewModel;
  LWelcomePageViewModel.Model := LWelcomePageModel;

  LWelcomePageView.InitializeFrame;
  LMainView.UpdateGUI;
  Application.Run;
end.
