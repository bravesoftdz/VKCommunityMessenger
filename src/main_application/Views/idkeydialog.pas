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
    procedure FormShow(Sender: TObject);
  private
    FAccessKey: string;
    FDone: boolean;
    FId: string;
    procedure SetAccessKey(AValue: string);
    procedure SetDone(AValue: boolean);
    procedure SetId(AValue: string);
    { private declarations }
  public
    property AccessKey: string read FAccessKey write SetAccessKey;
    property Id: string read FId write SetId;
    property Done: boolean read FDone write SetDone;
  end;

var
  Dialog: TDialog;

implementation

{$R *.lfm}

{ TDialog }

procedure TDialog.Button1Click(Sender: TObject);
begin
  AccessKey := AccesKeyEdit.Text;
  Id := IdEdit.Text;
  Done := true;
  AccesKeyEdit.Clear;
  IdEdit.Clear;
  Close;
end;

procedure TDialog.FormShow(Sender: TObject);
begin
  Done := false;
end;

procedure TDialog.SetAccessKey(AValue: string);
begin
  if FAccessKey = AValue then
    Exit;
  FAccessKey := AValue;
end;

procedure TDialog.SetDone(AValue: boolean);
begin
  if FDone=AValue then Exit;
  FDone:=AValue;
end;

procedure TDialog.SetId(AValue: string);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

end.
