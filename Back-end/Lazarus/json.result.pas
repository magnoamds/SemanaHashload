unit Json.Result;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  IJSONResult = interface
    ['{F52CF755-8805-4F2E-AE84-4D504BC3D5CF}']
    function Success(const Value: Boolean): IJSONResult;
    function Message(const Value: string): IJSONResult;
    function Data(const Value: string): IJSONResult; overload;
    function Data(const Value: TJSONObject): IJSONResult; overload;
    function Data(const Value: TJSONArray): IJSONResult; overload;
    function AsJSONObject: TJSONObject;
    function ToString: string; override;
  end;

  { TJSONResult }

  TJSONResult = class(TInterfacedObject, IJSONResult)
  private
    FJSONObject: TJSONObject;
  public
    constructor Create;
    destructor destroy; override;
    class function New: IJSONResult;

    function Success(const Value: Boolean): IJSONResult;
    function Message(const Value: string): IJSONResult;
    function Data(const Value: string): IJSONResult; overload;
    function Data(const Value: TJSONObject): IJSONResult; overload;
    function Data(const Value: TJSONArray): IJSONResult; overload;
    function AsJSONObject: TJSONObject;
    function ToString: string; override;
  end;

implementation

{ TJSONResult }

constructor TJSONResult.Create;
begin
  FJSONObject := TJSONObject.Create;
end;

destructor TJSONResult.destroy;
begin
  FreeAndNil(FJSONObject);

  inherited destroy;
end;

class function TJSONResult.New: IJSONResult;
begin
  Result := Self.Create;
end;

function TJSONResult.Success(const Value: Boolean): IJSONResult;
begin
  Result := Self;
  FJSONObject.Delete('success');
  FJSONObject.Add('success', TJSONBoolean.Create(Value));
end;

function TJSONResult.Message(const Value: string): IJSONResult;
begin
  Result := Self;
  FJSONObject.Delete('message');
  FJSONObject.Add('message', TJSONString.Create(Value));
end;

function TJSONResult.Data(const Value: string): IJSONResult;
begin
  Result := Self;
  FJSONObject.Delete('data');
  FJSONObject.Add('data', TJSONString.Create(Value));
end;

function TJSONResult.Data(const Value: TJSONObject): IJSONResult;
begin
  Result := Self;
  FJSONObject.Delete('data');
  FJSONObject.Add('data', Value);
end;

function TJSONResult.Data(const Value: TJSONArray): IJSONResult;
begin
  Result := Self;
  FJSONObject.Delete('data');
  FJSONObject.Add('data', Value);
end;

function TJSONResult.AsJSONObject: TJSONObject;
begin
  Result := FJSONObject;
end;

function TJSONResult.ToString: string;
begin
  Result := FJSONObject.AsJSON;
end;

end.


