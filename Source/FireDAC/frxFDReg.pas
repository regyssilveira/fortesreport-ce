{ --------------------------------------------------------------------------- }
{ AnyDAC FastReport v 6.0 enduser components                                  }
{                                                                             }
{ (c)opyright DA-SOFT Technologies 2004-2013.                                 }
{ All rights reserved.                                                        }
{                                                                             }
{ Initially created by: Serega Glazyrin <glserega@mezonplus.ru>               }
{ Extended by: Francisco Armando Duenas Rodriguez <fduenas@gmail.com>         }
{ --------------------------------------------------------------------------- }
{$I frx.inc}

unit frxFDReg;

interface

procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes, DesignIntf, DesignEditors,
  frxFDComponents;

procedure Register;
begin
 RegisterComponents('FastReport VCL', [TfrxFDComponents]);
end;

end.
