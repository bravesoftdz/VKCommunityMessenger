unit welcomepageview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    WelcomeLabel: TLabel;
    IntroductionText: TLabel;
    procedure IntroductionTextResize(Sender: TObject);
  private
    { private declarations }
  public
    procedure InitializeFrame;
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.IntroductionTextResize(Sender: TObject);
begin

end;

procedure TFrame1.InitializeFrame;
begin

end;

end.

