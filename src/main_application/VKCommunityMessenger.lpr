program VKCommunityMessenger;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainView { you can add units after this },
  Controls, Graphics, MainViewModel, MainModel, AbstractViewModel, entities,
  welcomepageview, AbstractModel, welcomepageviewmodel, welcomepagemodel,
  vkcmobserver, IdKeyDialog, vkcmconfig, VKDAO, ChatView, sqlite3dyn, helpers,
  vkcmchat, chatviewmodel, chatmodel, urlencoder, longpoll;

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
  LChatModel := TChatModel.Create;
  LChatViewModel := TChatViewModel.Create;

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
  LChatViewModel.Model := LChatModel;
  LChatView.ViewModel := LChatViewModel;

  LWelcomePageView.InitializeFrame;
  LChatView.InitializeFrame;
  LMainView.UpdateGUI;
  Application.Run;
end.
