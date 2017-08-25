unit VKDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, vkcmconfig,
  Graphics, sqldb, entities, DB, urlencoder, syncobjs;

type

  { TGroupsVKDAO }

  TGroupsVKDAO = class
    class function GetById(Client: TFPHTTPClient; AccessKey: string;
      CommunityId: string): TJSONObject;
  end;

  TGroupsVKDAOType = class of TGroupsVKDAO;

  { TDatabaseDAO }

  TDatabaseDAO = class
    class procedure SaveCommunity(Database: TSQLConnection; Community: TCommunity);
    class procedure ExecuteDatabaseCreationScript(Database: TSQLConnection);
    class function LoadDatabaseDataset(Database: TSQLConnection;
      out QueryTransaction: TSQLTransaction): TDataSet;
  end;

  TDatabaseDAOType = class of TDatabaseDAO;

  { TMessagesDAO }

  TMessagesDAO = class
    class function GetDialogs(Client: TFPHTTPCLient; AccessToken: string;
      Count, Offset: integer): TJSONObject;
    class function GetHistory(Client: TFPHTTPClient; AccessToken: string;
      UserId: string; Count: integer): TJSONObject;
    class procedure Send(Client: TFPHTTPClient; AccessToken: string;
      UserID: string; Message: string);
    class function GetLongPollServer(Client: TFPHTTPClient;
      AccessToken: string): TJSONObject;
  end;

  TMessagesDAOType = class of TMessagesDAO;

  { TUsersDAO }

  TUsersDAO = class
  private
    class function StringifyUserIds(Ids: TStringList): string;
  public
    class function Get(Client: TFPHTTPClient; UserIds: TStringList): TJSONObject;
  end;

  TUsersDAOType = class of TUsersDAO;

  { DAO }

  {DAO doesn't handle any exceptions}
  DAO = class
    class var
    Groups: TGroupsVKDAOType;
    Database: TDatabaseDAOType;
    Messages: TMessagesDAOType;
    Users: TUsersDAOType;
    class function LoadPhoto(Client: TFPHTTPClient; URL: string): TPicture;
    class function ExecuteGetRequest(Client: TFPHTTPClient; URL: string): string;
  end;

implementation

var
  HTTPCS: TCriticalSection;

{ TUsersDAO }

class function TUsersDAO.StringifyUserIds(Ids: TStringList): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Ids.Count - 1 do
    Result := Result + Ids[i] + ',';
  Result.Remove(Length(Result) - 1, 1);
end;

class function TUsersDAO.Get(Client: TFPHTTPClient; UserIds: TStringList): TJSONObject;
var
  URL: string;
  Response: string;
begin
  URL := VK_API_BASE_URL + 'users.get?' + '&v=' + USED_API_VERSION +
    '&fields=city,photo_50,photo_200' + '&user_ids=' + StringifyUserIds(UserIds);
  HTTPCS.Enter;
  try
    Response := Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;
  Result := (GetJSON(Response) as TJSONObject);
end;

{ TMessagesDAO }

class function TMessagesDAO.GetDialogs(Client: TFPHTTPCLient;
  AccessToken: string; Count, Offset: integer): TJSONObject;
var
  URL: string;
  Response: string;
begin
  URL := VK_API_BASE_URL + 'messages.getDialogs?' + '&access_token=' +
    AccessToken + '&v=' + USED_API_VERSION + '&count=' + IntToStr(Count) +
    '&offset=' + IntToStr(Offset) + '&preview_length=1';

  HTTPCS.Enter;
  try
    Response := Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;

  Result := (GetJSON(Response) as TJSONObject);
end;

class function TMessagesDAO.GetHistory(Client: TFPHTTPClient;
  AccessToken: string; UserId: string; Count: integer): TJSONObject;
var
  URL, Response: string;
begin
  URL := VK_API_BASE_URL + 'messages.getHistory?' + '&access_token=' +
    AccessToken + '&v=' + USED_API_VERSION + '&user_id=' + UserId +
    '&count=' + IntToStr(Count);
  HTTPCS.Enter;
  try
    Response := Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;
  Result := (GetJSON(Response) as TJSONObject);
end;

class procedure TMessagesDAO.Send(Client: TFPHTTPClient; AccessToken: string;
  UserID: string; Message: string);
var
  URL: string;
begin
  URL := VK_API_BASE_URL + 'messages.send?' + '&access_token=' +
    AccessToken + '&v=' + USED_API_VERSION + '&user_id=' + UserID +
    '&message=' + EncodeURL(Message);
  HTTPCS.Enter;
  try
    Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;
end;

class function TMessagesDAO.GetLongPollServer(Client: TFPHTTPClient;
  AccessToken: string): TJSONObject;
var
  URL: string;
  Response: string;
begin
  URL := VK_API_BASE_URL + 'messages.getLongPollServer?' + '&access_token=' +
    AccessToken + '&v=' + USED_API_VERSION;
  HTTPCS.Enter;
  try
    Response := Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;
  Result := GetJSON(Response) as TJSONObject;
end;

{ TDatabaseDAO }

class procedure TDatabaseDAO.SaveCommunity(Database: TSQLConnection;
  Community: TCommunity);
var
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
  MemoryStream: TMemoryStream;
begin
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  Database.Transaction := Transaction;
  Query.Database := Database;

  Query.SQL.LoadFromFile(SQL_INSERT_COMMUNITY_QUERY);

  Query.Params.ParamByName('ID').AsString := Community.Id;
  Query.Params.ParamByName('NAME').AsString := Community.Name;
  Query.Params.ParamByName('SCREENNAME').AsString := Community.ScreenName;
  Query.Params.ParamByName('COMMUNITYTYPE').AsString :=
    CommunintyTypeToString(Community.CommunityType);
  Query.Params.ParamByName('ISCLOSED').AsBoolean := Community.IsClosed;
  Query.Params.ParamByName('DEACTIVATED').AsBoolean := Community.Deactivated;
  Query.Params.ParamByName('HASPHOTO').AsBoolean := Community.HasPhoto;
  if Community.HasPhoto then
  begin
    MemoryStream := TMemoryStream.Create;
    Community.Photo.SaveToStream(MemoryStream);
    Query.Params.ParamByName('PHOTO').LoadFromStream(MemoryStream, ftBlob);
    FreeAndNil(MemoryStream);
  end
  else
    Query.Params.ParamByName('PHOTO').Clear;
  Query.Params.ParamByName('ACCESSKEY').AsString := Community.AccessKey;
  Query.Params.ParamByName('SERIALIZEDCHATBOT').AsString := Community.Chatbot.ToString;

  Query.ExecSQL;
  Transaction.Commit;

  FreeAndNil(Query);
  FreeAndNil(Transaction);
end;

class procedure TDatabaseDAO.ExecuteDatabaseCreationScript(Database: TSQLConnection);
{I don't use TSQLScript here, because it doesn't work with triggers correctly}
var
  Transaction: TSQLTransaction;
  Script: TSQLScript;
begin
  Transaction := TSQLTransaction.Create(nil);
  Script := TSQLScript.Create(nil);
  Database.Transaction := Transaction;
  Script.Transaction := Transaction;

  Script.Script.LoadFromFile(DATABASE_CREATION_SCRIPT);

  Transaction.Active := True;
  Script.Terminator := '--**';
  Script.Execute;
  Transaction.Commit;
  Transaction.Active := False;

  FreeAndNil(Transaction);
end;

class function TDatabaseDAO.LoadDatabaseDataset(Database: TSQLConnection;
  out QueryTransaction: TSQLTransaction): TDataSet;
var
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);

  Database.Transaction := Transaction;
  Query.Transaction := Transaction;

  Query.SQL.Text := SQL_SELECT_WHOLE_DATABASE_SELECT_QUERY;

  Result := Query;
  QueryTransaction := Transaction;
end;

{ DAO }

class function DAO.LoadPhoto(Client: TFPHTTPClient; URL: string): TPicture;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  HTTPCS.Enter;
  try
    Client.SimpleGet(URL, Stream);
  finally
    HTTPCS.Leave;
  end;
  Stream.Position := 0;

  Result := TPicture.Create;
  Result.LoadFromStream(Stream);
end;

class function DAO.ExecuteGetRequest(Client: TFPHTTPClient; URL: string): string;
begin
  HTTPCS.Enter;
  try
    Result := Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;
end;

{ TGroupsVKDAO }

class function TGroupsVKDAO.GetById(Client: TFPHTTPClient; AccessKey: string;
  CommunityId: string): TJSONObject;
var
  URL, Response: string;
begin
  URL := VK_API_BASE_URL + 'groups.getById?' + '&access_token=' +
    AccessKey + '&v=' + USED_API_VERSION + '&group_id=' + CommunityId +
    '&fields=has_photo';
  HTTPCS.Enter;
  try
    Response := Client.Get(URL);
  finally
    HTTPCS.Leave;
  end;
  Result := (GetJSON(Response) as TJSONObject);
end;

initialization

  HTTPCS := TCriticalSection.Create;

finalization

  FreeAndNil(HTTPCS);

end.
