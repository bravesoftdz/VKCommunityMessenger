unit welcomepageviewmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, welcomepagemodel, AbstractModel;

type
  IWelcomePageViewModel = interface(IViewModel)
    {Returns caption of text on welcome page}
    function GetWelcomeCaption: string;
    {Returns content of welcome paged}
    function GetWelcomeText: string;
    {Returns text with all news}
    function GetNews: string;
  end;

  { TWelcomePageViewModel }

  TWelcomePageViewModel = class(TInterfacedObject, IWelcomePageViewModel, IViewModel)
  private
    FModel: IModel;
    procedure SetModel(AValue: IModel);
    function GetModel: IModel;
  public
    function GetWelcomeCaption: string;
    function GetWelcomeText: string;
    function GetNews: string;
    property Model: IModel read GetModel write SetModel;
  end;

var
  LWelcomePageViewModel: TWelcomePageViewModel;

implementation

{ TWelcomePageViewModel }

procedure TWelcomePageViewModel.SetModel(AValue: IModel);
begin
  if FModel = AValue then
    Exit;
  FModel := AValue;
end;

function TWelcomePageViewModel.GetModel: IModel;
begin
  Result := FModel;
end;

function TWelcomePageViewModel.GetWelcomeCaption: string;
begin
  Result := (Model as TWelcomePageModel).GetWelcomeTitle;
end;

function TWelcomePageViewModel.GetWelcomeText: string;
begin
  Result := (Model as TWelcomePageModel).GetWelcomeText;
end;

function TWelcomePageViewModel.GetNews: string;
begin
  Result := (Model as TWelcomePageModel).GetNews;
end;

end.
