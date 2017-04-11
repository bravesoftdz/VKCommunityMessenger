unit vkgschat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TVKGSChat }

  TVKGSChat = class(TCustomControl)
    protected
      procedure Paint; override;
  end;

implementation

{ TVKGSChat }

procedure TVKGSChat.Paint;
begin
  inherited Paint;

end;

end.

