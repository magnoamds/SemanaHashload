program AgendaSrv;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes,
  System.SysUtils,
  Horse,
  Horse.CORS,
  App.Router in 'App.Router.pas',
  uDM in 'uDM.pas' {DM: TDataModule},
  App.Service in 'App.Service.pas';

begin
  THorse.Use(CORS);

  TAppRouter.Load;

  THorse.Listen(9000,
    procedure
    begin
      Writeln(Format('Server active %s:%d', [THorse.Host, THorse.Port]));
    end);
end.
