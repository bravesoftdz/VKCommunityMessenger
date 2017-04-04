unit IdKeyDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TDialog }

  TDialog = class(TForm)
    Button1: TButton;
    IdEdit: TEdit;
    AccesKeyEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Dialog: TDialog;

implementation

{$R *.lfm}

{ TDialog }

procedure TDialog.Button1Click(Sender: TObject);
begin
  AccesKeyEdit.Clear;
  IdEdit.Clear;
  Close;
end;

end.

