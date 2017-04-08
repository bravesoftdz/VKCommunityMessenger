unit entities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl;

type

  TCommunityType = (ctGroup, ctPage, ctEvent);

  { TCommunity }

  TCommunity = class
  private
    FAccessKey: string;
    FCommunityType: TCommunityType;
    FDeactivated: boolean;
    FHasPhoto: boolean;
    FId: string;
    FIsClosed: boolean;
    FName: string;
    FPhoto: TPicture;
    FScreenName: string;
    procedure SetAccessKey(AValue: string);
    procedure SetCommunityType(AValue: TCommunityType);
    procedure SetDeactivated(AValue: boolean);
    procedure SetHasPhoto(AValue: boolean);
    procedure SetId(AValue: string);
    procedure SetIsClosed(AValue: boolean);
    procedure SetName(AValue: string);
    procedure SetPhoto(AValue: TPicture);
    procedure SetScreenName(AValue: string);
  public
    property Name: string read FName write SetName;
    property Id: string read FId write SetId;
    property ScreenName: string read FScreenName write SetScreenName;
    property IsClosed: boolean read FIsClosed write SetIsClosed;
    property Deactivated: boolean read FDeactivated write SetDeactivated;
    property CommunityType: TCommunityType read FCommunityType write SetCommunityType;
    property HasPhoto: boolean read FHasPhoto write SetHasPhoto;
    property Photo: TPicture read FPhoto write SetPhoto;
    property AccessKey: string read FAccessKey write SetAccessKey;
  end;

  TCommunityList = specialize TFPGList<TCommunity>;

function StringToCommunityType(Str: string): TCommunityType;
function CommunintyTypeToString(AType: TCommunityType): string;

implementation

function StringToCommunityType(Str: string): TCommunityType;
begin
  if Uppercase(Str) = 'PAGE' then
    Result := ctPage
  else if Uppercase(Str) = 'GROUP' then
    Result := ctGroup
  else if Uppercase(Str) = 'EVENT' then
    Result := ctEvent
  else
    raise Exception.Create('StringToCommunityType: неизвестный тип сообщества');
end;

function CommunintyTypeToString(AType: TCommunityType): string;
begin
  case AType of
    ctEvent: Result := 'event';
    ctGroup: Result := 'group';
    ctPage: Result := 'page';
  end;
end;

{ TCommunity }

procedure TCommunity.SetName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TCommunity.SetPhoto(AValue: TPicture);
begin
  if FPhoto = AValue then
    Exit;
  FPhoto := AValue;
end;

procedure TCommunity.SetScreenName(AValue: string);
begin
  if FScreenName = AValue then
    Exit;
  FScreenName := AValue;
end;

procedure TCommunity.SetId(AValue: string);
begin
  if FId = AValue then
    Exit;
  FId := AValue;
end;

procedure TCommunity.SetDeactivated(AValue: boolean);
begin
  if FDeactivated = AValue then
    Exit;
  FDeactivated := AValue;
end;

procedure TCommunity.SetHasPhoto(AValue: boolean);
begin
  if FHasPhoto = AValue then
    Exit;
  FHasPhoto := AValue;
end;

procedure TCommunity.SetCommunityType(AValue: TCommunityType);
begin
  if FCommunityType = AValue then
    Exit;
  FCommunityType := AValue;
end;

procedure TCommunity.SetAccessKey(AValue: string);
begin
  if FAccessKey = AValue then
    Exit;
  FAccessKey := AValue;
end;

procedure TCommunity.SetIsClosed(AValue: boolean);
begin
  if FIsClosed = AValue then
    Exit;
  FIsClosed := AValue;
end;

end.
