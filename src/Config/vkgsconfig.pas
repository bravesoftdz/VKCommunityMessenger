unit VKGSConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  VK_API_BASE_URL = 'https://api.vk.com/method/';
  USED_API_VERSION = '5.63';
  DATABASE_NAME = 'communitiesdb.db';
  DATABASE_CREATION_SCRIPT = 'queries/databaseinit.sql';
  SQLITE_LIBRARY_NAME='libraries/sqlite3.dll';
  SQL_INSERT_COMMUNITY_QUERY = 'queries/insert.sql';


implementation

end.

