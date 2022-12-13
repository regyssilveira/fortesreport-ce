{******************************************}
{                                          }
{             FastReport VCL               }
{         Fib enduser components           }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{******************************************}
{                                          }
{       Improved by Butov Konstantin       }
{  Improved by  Serge Buzadzhy             }
{             buzz@devrace.com             }
{                                          }
{******************************************}

unit frxFIBReg;

interface

{$I frx.inc}

procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes, frxFIBComponents;

procedure Register;
begin
  RegisterComponents('FastReport VCL', [TfrxFIBComponents]);
  {$IFDEF DELPHI16}
//GroupDescendentsWith(TfrxFIBComponents, TControl);
{$ENDIF}
end;

end.
