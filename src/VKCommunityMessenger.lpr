program VKCommunityMessenger;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainView { you can add units after this },
  Controls,
  Graphics, MainViewModel, Model, AbstractViewModel;

{$R *.res}

  procedure HighDPI;
  var
    i: integer;
  begin
    for i := 0 to Screen.FormCount - 1 do
      Screen.Forms[i].AutoAdjustLayout(lapAutoAdjustForDPI,
        Screen.Forms[i].DesignTimeDPI, Screen.PixelsPerInch, Screen.Forms[i].Width,
        ScaleX(Screen.Forms[i].Width, Screen.Forms[i].DesignTimeDPI));
  end;

begin
  RequireDerivedFormResource := True;

  {Create models and viewmodels}

  Application.Initialize;

  {Create views}
  Application.CreateForm(TfMainView, fMainView);

  {Bind mvvm}

  HighDPI;
  Application.Run;
end.
