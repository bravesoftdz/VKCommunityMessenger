unit welcomepageview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, StdCtrls, ExtCtrls,
  welcomepageviewmodel, Dialogs;

type

  { TWelcomePageFrameView }

  TWelcomePageFrameView = class(TFrame)
    NewsLabel: TLabel;
    WelcomeLabel: TLabel;
    IntroductionText: TLabel;
  private
    FViewModel: IWelcomePageViewModel;
    procedure SetViewModel(AValue: IWelcomePageViewModel);
    { private declarations }
  public
    procedure InitializeFrame;
    property ViewModel: IWelcomePageViewModel read FViewModel write SetViewModel;
  end;

var
  LWelcomePageView: TWelcomePageFrameView;

implementation

{$R *.lfm}

{ TWelcomePageFrameView }

procedure TWelcomePageFrameView.SetViewModel(AValue: IWelcomePageViewModel);
begin
  if FViewModel = AValue then
    Exit;
  FViewModel := AValue;
end;

procedure TWelcomePageFrameView.InitializeFrame;
begin
  WelcomeLabel.Caption := ViewModel.GetWelcomeCaption;
  IntroductionText.Caption := ViewModel.GetWelcomeText;
  NewsLabel.Caption := ViewModel.GetNews;
end;

end.
