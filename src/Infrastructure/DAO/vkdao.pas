unit VKDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, VKGSConfig, Graphics;

type

  { TGroupsVKDAO }

  TGroupsVKDAO = class
    class function GetById(Client: TFPHTTPClient; AccessKey: string; CommunityId: string): TJSONObject;
  end;

  TGroupsVKDAOType = class of TGroupsVKDAO;

  { DAO }

  DAO = class
    class var
    Groups: TGroupsVKDAOType;
    class function LoadPhoto(Client: TFPHTTPClient; URL: string): TPicture;
  end;

implementation

{ DAO }

class function DAO.LoadPhoto(Client: TFPHTTPClient; URL: string): TPicture;
var Stream: TStream;
begin
  Stream:=TStream.Create;
  Client.Get(URL,Stream);
  Result:=TPicture.Create;
  Result.LoadFromStream(Stream);
end;

{ TGroupsVKDAO }

class function TGroupsVKDAO.GetById(Client: TFPHTTPClient; AccessKey: string;
  CommunityId: string): TJSONObject;
var URL, Response: string;
begin
  URL := VK_API_BASE_URL + 'groups.getById?' + '&access_token=' +
    AccessKey + '&v=' + USED_API_VERSION + '&group_id=' + CommunityId + '&fields=has_photo';
  Response := Client.Get(URL);
  Result:=(GetJSON(Response) as TJSONObject);
end;

end.

