unit MainViewModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractViewModel, Controls;

type
  IMainViewModel = interface(IViewModel)
     procedure FillImageCommunitiesList(var List: TImageList);
  end;

implementation

end.

