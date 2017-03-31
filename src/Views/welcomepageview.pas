unit welcomepageview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  welcomepageviewmodel;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
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

var LWelcomePageView: TFrame1;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.SetViewModel(AValue: IWelcomePageViewModel);
begin
  if FViewModel=AValue then Exit;
  FViewModel:=AValue;
end;

procedure TFrame1.InitializeFrame;
begin
  WelcomeLabel.Caption:=ViewModel.GetWelcomeCaption;
  IntroductionText.Caption:=ViewModel.GetWelcomeText;
end;

end.

