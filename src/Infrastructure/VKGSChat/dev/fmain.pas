unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VKGSChat;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Chat: TVKGSChat;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TForm1.FormShow(Sender: TObject);
begin
  Chat := TVKGSChat.Create(Self);
  Chat.Parent := Panel2;
  Chat.Align := alClient;
  Chat.PaddingLeft := 30;
  Chat.PaddingRight := 30;
  Chat.PaddingBottom := 10;
  Chat.DistanceBetweenMessages := 10;
  Chat.Font.Color := clWhite;
  Chat.BoxColor := clBlue;
  Chat.FrameColor := clWhite;
  Chat.BoxBorder:=5;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  NewMessage: TMessage;
begin
  NewMessage := TMessage.Create;
  NewMessage.Left := True;
  NewMessage.Message := Memo1.Text;
  Chat.Messages.Add(NewMessage);
  Chat.Repaint;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  NewMessage: TMessage;
begin
  NewMessage := TMessage.Create;
  NewMessage.Left := False;
  NewMessage.Message := Memo2.Text;
  Chat.Messages.Add(NewMessage);
  Chat.Repaint;
end;

end.
