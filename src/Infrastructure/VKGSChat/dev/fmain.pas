unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VKGSChat;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chat: TVKGSChat;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chat := TVKGSChat.Create(Self);
  Chat.Parent:=Self;
  Chat.Align:=alClient;
end;

end.

