unit uDM;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset;

type

  { TDM }

  TDM = class(TDataModule)
    ZConnection: TZConnection;
  private

  public
    function GetQuery: TZQuery;
  end;

var
  DM: TDM;

implementation

{$R *.lfm}

{ TDM }

function TDM.GetQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := ZConnection;
  Result.Close;
  Result.SQL.Clear;
end;

end.

