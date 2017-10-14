unit ModelStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ModelDataModel;

type

  IDAOAdapter = interface

  end;

  TDAOAdapter = class(TInterfacedObject, IDAOAdapter)

  end;

  { TModelStorageService
    is a implementation of IStorageService }
  TModelStorageService = class(TInterfacedObject, IStorageService)
  private
    FDAOAdapter: IDAOAdapter;
  public
    constructor Create(DAOAdapter: IDAOAdapter);
    function GetLastDialogsForCommunity(CommunityId: string): TDialogsList;
    procedure UpdateCommunityInformation(Community: ICommunity);
    function GetCommunities: TCommunitiesList;
    procedure AddNewCommunity(CommunityID, AccessKey: string);
    destructor Destroy; override;
  end;

implementation

{ TModelStorageService }

constructor TModelStorageService.Create(DAOAdapter: IDAOAdapter);
begin
  FDAOAdapter := DAOAdapter;
end;

function TModelStorageService.GetLastDialogsForCommunity(CommunityId:
  string): TDialogsList;
begin

end;

procedure TModelStorageService.UpdateCommunityInformation(Community: ICommunity);
begin

end;

function TModelStorageService.GetCommunities: TCommunitiesList;
begin

end;

procedure TModelStorageService.AddNewCommunity(CommunityID, AccessKey: string);
begin

end;

destructor TModelStorageService.Destroy;
begin
  FDAOAdapter := nil;
  inherited Destroy;
end;

end.


