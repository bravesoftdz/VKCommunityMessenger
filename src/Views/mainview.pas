unit MainView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, MainViewModel;

type

  { TfMainView }

  TfMainView = class(TForm)
  private
    FViewModel: IMainViewModel;
    procedure SetViewModel(AValue: IMainViewModel);
  public
    property ViewModel: IMainViewModel read FViewModel write SetViewModel;
  end;

var
  fMainView: TfMainView;

implementation

{$R *.lfm}

{ TfMainView }

procedure TfMainView.SetViewModel(AValue: IMainViewModel);
begin
  if FViewModel=AValue then Exit;
  FViewModel:=AValue;
end;

end.

