unit App.Router;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses Classes, SysUtils, Horse;

type
  TAppRouter = class
  public
    class procedure Load();
  end;

implementation

{ TAppRouter }

uses App.Service;

procedure OnStatus(AReq: THorseRequest; ARes: THorseResponse);
begin
  ARes.ContentType('text/html')
      .Send(Format('<h1>Server Agenda On-Line - Horse version %s</h1>', [THorse.Version]));
end;

procedure OnTipos(AReq: THorseRequest; ARes: THorseResponse);
begin
  ARes.ContentType('application/json')
      .Send(TService.Instance.Tipos());
end;

procedure OnContatosGet(AReq: THorseRequest; ARes: THorseResponse);
begin
  ARes.ContentType('application/json')
      .Send(TService.Instance.Contatos());
end;

procedure OnContatoPost(AReq: THorseRequest; ARes: THorseResponse);
begin
  ARes.ContentType('application/json')
      .Send(TService.Instance.Save(0, AReq.Body));
end;

procedure OnContatoPut(AReq: THorseRequest; ARes: THorseResponse);
begin
  ARes.ContentType('application/json')
      .Send(TService.Instance.Save(AReq.Params.Field('id').AsInteger, AReq.Body));
end;

procedure OnContatoDelete(AReq: THorseRequest; ARes: THorseResponse);
begin
  ARes.ContentType('application/json')
      .Send(TService.Instance.Delete(AReq.Params.Field('id').AsInteger));
end;

class procedure TAppRouter.Load;
begin
  THorse.Get('/', OnStatus);

  THorse.Get('/tipos', OnTipos)
        .Get('/contatos', OnContatosGet)
        .Post('/contatos', OnContatoPost)
        .Put('/contatos/:id', OnContatoPut)
        .Delete('/contatos/:id', OnContatoDelete);
end;

end.
