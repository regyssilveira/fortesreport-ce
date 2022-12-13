
{******************************************}
{                                          }
{             FastReport VCL               }
{            Registration unit             }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxRegCS;

{$I frx.inc}

{$IFDEF FPC}
{$R 'frxRegCS.dcr'}
{$ENDIF}

interface

procedure Register;

implementation

uses
  {$IFNDEF Linux}
  Windows,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFNDEF Delphi6}
  DsgnIntf,
{$ELSE}
{$IFNDEF FPC}
  DesignIntf, DesignEditors,
{$ENDIF}
{$ENDIF}
  frxServer, frxServerClient, frxHTTPClient;

{-----------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('FastReport VCL Client/Server',
    [TfrxReportServer, TfrxServerConnection, TfrxReportClient, 
    TfrxHTTPClient]);
end;

end.
