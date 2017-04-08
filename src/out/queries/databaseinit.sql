--Two dashes and two stars is a terminator (separator) for TSQLScript 

CREATE TABLE communities ( 
    Id VARCHAR PRIMARY KEY NOT NULL, 
    Name VARCHAR NOT NULL, 
    ScreenName VARCHAR, 
    CommunityType COMMUNITYTYPE, 
    IsClosed BOOLEAN, 
    Deactivated BOOLEAN, 
    HasPhoto BOOLEAN, 
    Photo BLOB, 
    AccesKey VARCHAR 
);
--**

CREATE VIEW communitiesview AS
     SELECT * FROM communities;
--**

CREATE TRIGGER communitiesview_insert INSTEAD OF INSERT ON communitiesview FOR EACH ROW 
BEGIN  
  INSERT INTO communities
    VALUES (New.Id, New.Name, New.ScreenName, New.CommunityType, New.IsClosed, New.Deactivated, New.HasPhoto, New.Photo, New.AccesKey);
END

--**