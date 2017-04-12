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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chat := TVKGSChat.Create(Self);
  Chat.Parent := Self;
  Chat.Align := alClient;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Chat.Messages.Add(TMessage.Create);
  Chat.Messages.Add(TMessage.Create);
  Chat.Messages.Add(TMessage.Create);
  Chat.Messages[0].Message := 'olfmrieogtmje';
  Chat.Messages[0].Left := True;
  Chat.Messages[1].Message := 'Oligofrenot';
  Chat.Messages[1].Left := False;
  Chat.Messages[2].Message := 'Pffffffffffffff pffffffffffffffffffffffff vpffffffff';
  Chat.Messages[2].Left := True;
end;

end.

