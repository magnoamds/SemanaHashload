unit App.Service;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZDataset, fpjson, jsonparser;

type

  { TService }

  TService = class
  private
    class var FInstance: TService;
    class function GetInstance: TService; static;

    function ExistContato(AID: Integer): Boolean;
    function InsertContato(AContato: TJSONObject): string;
    function UpdateContrato(AContato: TJSONObject): string;
  public
    constructor Create;

    function Tipos: string;
    function Contatos: string;
    function Save(AID: Integer; AContato: string): string;
    function Delete(AID: Integer): string;

    class property Instance: TService read GetInstance;
  end;

implementation

uses uDM, DataSet.Serialize, Json.Result;

{ TService }

function TService.Contatos: string;
var
  lDM: TDM;
  lQry: TZQuery;
begin
  try
    lDM := TDM.Create(nil);
    try
      lQry := lDM.GetQuery;
      try
        lQry.SQL.Add('SELECT a.id                                     ');
        lQry.SQL.Add('      ,a.nome                                   ');
        lQry.SQL.Add('      ,a.id_tipo                                ');
        lQry.SQL.Add('      ,t.descricao as tipo                      ');
        lQry.SQL.Add('      ,a.contato                                ');
        lQry.SQL.Add('  FROM agenda a                                 ');
        lQry.SQL.Add(' INNER JOIN tipo_contato t ON (a.id_tipo = t.id)');
        lQry.SQL.Add(' ORDER BY a.nome                                ');
        lQry.Open;
        Result := TJSONResult.New
                             .Success(true)
                             .Data(lQry.ToJSONArray())
                             .ToString;
      finally
        lQry.Close;
        lQry.Free;
      end;
    finally
      lDM.Free;
    end;
  except
    on E: exception do
      Result := TJSONResult.New
                           .Success(False)
                           .Message(E.Message)
                           .ToString;
  end;
end;

function TService.Delete(AID: Integer): string;
var
  lDM: TDM;
  lQry: TZQuery;
begin
  try
    if not ExistContato(AID) then
      raise Exception.Create('Contato não localizado!');

    lDM := TDM.Create(nil);
    try
      lQry := lDM.GetQuery;
      try
        lQry.SQL.Add('DELETE FROM agenda');
        lQry.SQL.Add('WHERE id = :id');
        lQry.ParamByName('id').AsInteger := AID;
        lQry.ExecSQL;
        Result := TJSONResult.New
                             .Success(true)
                             .Message('Registro excluído com sucesso.')
                             .ToString;
      finally
        lQry.Free;
      end;
    finally
      lDM.Free;
    end;
  except
    on E: exception do
    Result := TJSONResult.New
                         .Success(False)
                         .Message(E.Message)
                         .ToString;
  end;
end;

function TService.ExistContato(AID: Integer): Boolean;
var
  lDM: TDM;
  lQry: TZQuery;
begin
  try
    lDM := TDM.Create(nil);
    try
      lQry := lDM.GetQuery;
      try
        lQry.SQL.Add('SELECT id      ');
        lQry.SQL.Add('  FROM agenda  ');
        lQry.SQL.Add(' WHERE id = :id');
        lQry.ParamByName('id').AsInteger := AID;
        lQry.Open;
        Result := not lQry.IsEmpty;
      finally
        lQry.Close;
        lQry.Free;
      end;
    finally
      lDM.Free;
    end;
  except
    on E: exception do
      raise Exception.Create(E.Message);
  end;
end;

class function TService.GetInstance: TService;
begin
  if not Assigned(FInstance) then
    FInstance := TService.Create;

  Result := FInstance;
end;

function TService.InsertContato(AContato: TJSONObject): string;
var
  lDM: TDM;
  lQry: TZQuery;
begin
  try
    lDM := TDM.Create(nil);
    try
      lQry := lDM.GetQuery;
      try
        lQry.SQL.Add('INSERT INTO agenda (nome, id_tipo, contato)');
        lQry.SQL.Add('VALUES (:nome, :id_tipo, :contato)');
        lQry.ParamByName('nome').AsString := AContato['nome'].AsString;
        lQry.ParamByName('id_tipo').AsInteger := AContato['id_tipo'].AsInteger;
        lQry.ParamByName('contato').AsString := AContato['contato'].AsString;
        lQry.ExecSQL;
        Result := TJSONResult.New
                             .Success(true)
                             .Message('Registro incluído com sucesso.')
                             .ToString;
      finally
        lQry.Free;
      end;
    finally
      lDM.Free;
    end;
  except
    on E: exception do
      raise Exception.Create(E.Message);
  end;
end;

function TService.Save(AID: Integer; AContato: string): string;
var
  lJSONObject: TJSONObject;
begin
  try
    if not AContato.StartsWith('{') and not AContato.EndsWith('}')  then
      raise Exception.Create('JSON Invalid!');

    lJSONObject := GetJSON(AContato) as TJSONObject;

    if (lJSONObject.Count = 0) then
      raise Exception.Create('Dados não informado!');

    if lJSONObject['nome'].IsNull or (Trim(lJSONObject['nome'].AsString) = '') then
      raise Exception.Create('Nome não informado!');

    if (lJSONObject['id_tipo'].AsInteger = 0) then
      raise Exception.Create('Tipo não informado!');

    if lJSONObject['contato'].IsNull or (Trim(lJSONObject['contato'].AsString) = '') then
      raise Exception.Create('Contato não informado!');

    if (AID > 0) then
    begin
      if not ExistContato(AID) then
        raise Exception.Create('Contato not found!');

      lJSONObject.Delete('id');
      lJSONObject.Add('id', AID);
      Result := UpdateContrato(lJSONObject);
    end
    else
      Result := InsertContato(lJSONObject);
  except
    on E: exception do
    Result := TJSONResult.New
                         .Success(False)
                         .Message(E.Message)
                         .ToString;
  end;
end;

function TService.Tipos: string;
var
  lDM: TDM;
  lQry: TZQuery;
begin
  try
    lDM := TDM.Create(nil);
    try
      lQry := lDM.GetQuery;
      try
        lQry.SQL.Add('SELECT *           ');
        lQry.SQL.Add('  FROM tipo_contato');
        lQry.SQL.Add(' ORDER BY descricao');
        lQry.Open;
        Result := TJSONResult.New
                             .Success(true)
                             .Data(lQry.ToJSONArray())
                             .ToString;
      finally
        lQry.Close;
        lQry.Free;
      end;
    finally
      lDM.Free;
    end;
  except
    on E: exception do
    Result := TJSONResult.New
                         .Success(False)
                         .Message(E.Message)
                         .ToString;
  end;
end;

function TService.UpdateContrato(AContato: TJSONObject): string;
var
  lDM: TDM;
  lQry: TZQuery;
begin
  try
    lDM := TDM.Create(nil);
    try
      lQry := lDM.GetQuery;
      try
        lQry.SQL.Add('UPDATE agenda       ');
        lQry.SQL.Add('SET nome = :nome,   ');
        lQry.SQL.Add(' id_tipo = :id_tipo,');
        lQry.SQL.Add(' contato = :contato ');
        lQry.SQL.Add('WHERE id = :id      ');
        lQry.ParamByName('id').AsInteger := AContato['id'].AsInteger;
        lQry.ParamByName('nome').AsString := AContato['nome'].AsString;
        lQry.ParamByName('id_tipo').AsInteger := AContato['id_tipo'].AsInteger;
        lQry.ParamByName('contato').AsString := AContato['contato'].AsString;
        lQry.ExecSQL;
        Result := TJSONResult.New
                             .Success(true)
                             .Message('Registro alterado com sucesso.')
                             .ToString;
      finally
        lQry.Free;
      end;
    finally
      lDM.Free;
    end;
  except
    on E: exception do
      raise Exception.Create(E.Message);
  end;
end;

constructor TService.Create;
begin
  TDataSetSerializeConfig.GetInstance.CaseNameDefinition := TCaseNameDefinition.cndLower;
end;

initialization

finalization
  TService.FInstance.Free;

end.
