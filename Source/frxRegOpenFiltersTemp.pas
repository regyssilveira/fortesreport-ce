{******************************************}
{                                          }
{             FastReport VCL               }
{            Registration unit             }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxRegOpenFiltersTemp;

{$I frx.inc}

interface


procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
{$IFNDEF Delphi6}
  DsgnIntf,
{$ELSE}
  DesignIntf, DesignEditors,
{$ENDIF}
  frxOpenFilterQR, frxOpenFilterRB;

procedure Register;
begin
  RegisterComponents('FastReport VCL Open Filters Template',
    [TfrxOpenFilterQR, TfrxOpenFilterRB]);
end;

end.
