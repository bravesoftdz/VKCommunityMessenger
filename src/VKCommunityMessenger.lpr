program VKCommunityMessenger;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainView { you can add units after this },
  Controls, Graphics, MainViewModel, MainModel, AbstractViewModel, entities,
  welcomepageview, AbstractModel, welcomepageviewmodel, welcomepagemodel,
  vkgsobserver, IdKeyDialog, VKGSConfig, VKDAO, ChatView, sqlite3dyn, helpers,
  vkgschat, chatviewmodel, chatmodel;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  {Setup enivroement variables}
   SQLiteDefaultLibrary:=SQLITE_LIBRARY_NAME;

  {Create models and viewmodels}
  LMainViewModel := TMainViewModel.Create;
  LMainModel := TMainModel.Create;
  LWelcomePageModel := TWelcomePageModel.Create;
  LWelcomePageViewModel := TWelcomePageViewModel.Create;

  Application.Initialize;

  {Create views}
  Application.CreateForm(TfMainView, LMainView);
  Application.CreateForm(TDialog, Dialog);
  LWelcomePageView := TWelcomePageFrameView.Create(LMainView);
  LChatView:=TChatFrameView.Create(LMainView);

  {Bind mvvm}
  LMainView.CurrentFrame:=LWelcomePageView;
  LMainViewModel.Model := LMainModel;
  LMainView.ViewModel := LMainViewModel;
  LWelcomePageView.ViewModel := LWelcomePageViewModel;
  LWelcomePageViewModel.Model := LWelcomePageModel;

  LWelcomePageView.InitializeFrame;
  LMainView.UpdateGUI;
  Application.Run;
end.
