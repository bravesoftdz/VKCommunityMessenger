unit Model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, entities;

type

  { IModel }

  IModel = interface
    function GetAccessToken: string;
    procedure SetAccessToken(AValue: string);
    property AccessToken: string read GetAccessToken write SetAccessToken;
    function GetExtendedCommunityInformation(CommunityId, AccessKey: string): TCommunity;
    procedure SaveCommunityInfoLocally(Communty: TCommunity);
    function GetCommunities: TCommunityList;
  end;

implementation

end.

