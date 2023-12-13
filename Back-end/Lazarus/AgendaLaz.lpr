program AgendaLaz;

{$mode delphi}{$H+}

uses Classes, SysUtils,
  Horse,
  Horse.CORS,
  App.Router,
  uDM, App.Service, Json.Result;

procedure OnListen;
begin
  WriteLn(Format('Server active on port %d', [THorse.Port]));
end;

begin
  THorse.Use(CORS());

  TAppRouter.Load();

  THorse.Listen(9000, OnListen);
end.

