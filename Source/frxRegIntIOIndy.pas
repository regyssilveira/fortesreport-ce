
{******************************************}
{                                          }
{             FastReport VCL               }
{            Registration unit             }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxRegIntIOIndy;

{$I frx.inc}

interface


procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
{$IFNDEF Delphi6}
  DsgnIntf,
{$ELSE}
  DesignIntf, DesignEditors,
{$ENDIF}
  frxIOTransportFTP, frxIOTransportDropboxIndy, frxIOTransportOneDriveIndy, 
  frxIOTransportBoxComIndy, frxIOTransportGoogleDriveIndy, frxIOTransportGMailIndy,
  frxIOTransportYandexDiskIndy, frxIOTransportOutlookIndy;

procedure Register;
begin
  RegisterComponents('FastReport VCL Internet transports',
    [TfrxFTPIOTransport, TfrxDropboxIOTransportIndy, TfrxOneDriveIOTransportIndy, TfrxBoxComIOTransportIndy, TfrxGoogleDriveIOTransportIndy, TfrxGMailIOTransportIndy, TfrxYandexDiskIOTransportIndy, TfrxOutlookIOTransportIndy]);
end;

end.
