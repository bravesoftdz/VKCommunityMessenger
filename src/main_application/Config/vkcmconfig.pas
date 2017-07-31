unit vkcmconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  VK_API_BASE_URL = 'https://api.vk.com/method/';
  USED_API_VERSION = '5.63';
  DATABASE_CREATION_SCRIPT = 'queries/databaseinit.sql';
  {$IFDEF WIN32}
  SQLITE_LIBRARY_NAME = './libraries/sqlite3_32.dll';
  {$ENDIF}
  {$IFDEF WIN64}
  SQLITE_LIBRARY_NAME = './libraries/sqlite3_64.dll';
  {$ENDIF}
  SQL_INSERT_COMMUNITY_QUERY = 'queries/insert.sql';
  SQL_SELECT_WHOLE_DATABASE_SELECT_QUERY = 'SELECT * FROM communitiesview';
  NEWS_WEBPAGE = 'http://vkcmnews.azurewebsites.net/news.txt';
  {$IFDEF WINDOWS}
  OPENSSLLIB1 = 'libraries\\ssleay32.dll';
  OPENSSLLIB2 = 'libraries\\libeay32.dll';
  {$ENDIF}

function APPLICATION_APPDATA_PATH: string;
function DATABASE_PATH: string;

implementation

function APPLICATION_APPDATA_PATH: string;
var
  AppDataPath: string;
begin
  AppDataPath := GetEnvironmentVariable('APPDATA') + '\VKCommunityMessenger\';
  if not DirectoryExists(AppDataPath) then
     if not CreateDir(AppDataPath) then
        raise Exception.Create('APPLICATION_APPDATA_PATH: Cannot create folder in AppData');
  Result := AppDataPath;
end;

function DATABASE_PATH: string;
begin
  Result := APPLICATION_APPDATA_PATH + 'communitiesdb.db';
end;

end.
