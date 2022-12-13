
{******************************************}
{                                          }
{             FastReport VCL               }
{            Graphic routines              }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxSVGGraphics;

interface

{$I frx.inc}

uses
  SysUtils, {$IFNDEF FPC}Windows, Messages,{$ENDIF}
  Classes, Graphics, frxPictureGraphics, frxSVGGraphic;

const
  frxSVGFileFormat = 6;

implementation

type

  TfrxSVGGraphicFormat = class(TfrxCustomVectorGraphicFormat)
  public
    class function ConvertFrom(Graphic: TGraphic; DestPixelFormat: TPixelFormat; DestQuality: Integer = 100): TGraphic; override;
    class function CreateNew(Width: Integer; Height: Integer; PixelFormat: TPixelFormat; Transparent: Boolean; Quality: Integer = 100): TGraphic; override;
    class procedure DrawExt(const GProps: TfrxDrawGraphicExt; Canvas: TCanvas; AGraphic: TGraphic; const Area: TRect; ScaleX: Double; ScaleY: Double); override;
    class function HasAlphaChanel(Graphic: TGraphic): Boolean; override;
    class function ScaleGraphicToNew(Graphic: TGraphic; NewWidth, NewHeight: Integer): TGraphic; override;
    class function GetGraphicClass: TGraphicClass; override;
    class function GetGraphicProps(Graphic: TGraphic): TfrxGraphicProps; override;
    class function GetGraphicMime: String; override;
    class function GetGraphicName: String; override;
    class function GetGraphicExt: String; override;
    class function GetGraphicConst: Integer; override;
    class function GetFormatCapabilities: TfrxGraphicFormatCaps; override;
    class function IsSupportedFormat(const Stream: TStream): Boolean; override;
    class function IsTranslucent: Boolean; override;
  end;


{ TfrxEMFGraphicFormat }

class function TfrxSVGGraphicFormat.GetGraphicConst: Integer;
begin
  Result := frxSVGFileFormat;
end;

class function TfrxSVGGraphicFormat.GetGraphicExt: String;
begin
  Result := '.svg';
end;

class function TfrxSVGGraphicFormat.GetGraphicMime: String;
begin
  Result := 'image/svg+xml';
end;

class function TfrxSVGGraphicFormat.GetGraphicName: String;
begin
  Result := 'SVG';
end;

class function TfrxSVGGraphicFormat.IsSupportedFormat(
  const Stream: TStream): Boolean;
var
  SVGHeader: Array[0..7] of AnsiChar;
  pos: Integer;
begin
  Result := False;
  if (Stream.Size - Stream.Position) >= SizeOf(SVGHeader) then
  begin
    pos := Stream.Position;
    Stream.ReadBuffer(SVGHeader[0], SizeOf(SVGHeader));
    Stream.Position := pos;
    pos := 0;
    if (SVGHeader[0] = #239) and (SVGHeader[1] = #187) and (SVGHeader[2] = #191) then
      pos := 3;
    if ((SVGHeader[pos] = '<') and ((SVGHeader[pos + 1] = 's') or (SVGHeader[pos + 1] = 'S')) and ((SVGHeader[pos + 2] = 'v') or (SVGHeader[pos + 2] = 'V')) and ((SVGHeader[pos + 3] = 'g') or (SVGHeader[pos + 3] = 'G'))) or
       ((SVGHeader[pos] = '<') and (SVGHeader[pos + 1] = '?') and ((SVGHeader[pos + 2] = 'x') or (SVGHeader[pos + 2] = 'X')) and ((SVGHeader[pos + 3] = 'm') or (SVGHeader[pos + 3] = 'M')) and ((SVGHeader[pos + 4] = 'l') or (SVGHeader[pos + 4] = 'L'))) then
      Result := True;
  end;
end;

class function TfrxSVGGraphicFormat.IsTranslucent: Boolean;
begin
  Result := True;
end;

class function TfrxSVGGraphicFormat.ScaleGraphicToNew(Graphic: TGraphic;
  NewWidth, NewHeight: Integer): TGraphic;
var
  GProps: TfrxGraphicProps;
begin
  GProps := GetGraphicProps(Graphic);
  Result := CreateNew(Graphic.Width, Graphic.Height, GProps.PixelFormat, GProps.Transparent, GProps.Quality);
  Result.Assign(Graphic);
end;

{ TfrxBaseMetafileGraphicFormat }

type
  TfrxHackSVGGraphic = class(TfrxSVGGraphic);

class function TfrxSVGGraphicFormat.ConvertFrom(Graphic: TGraphic;
  DestPixelFormat: TPixelFormat; DestQuality: Integer): TGraphic;
var
  GHelper: TfrxCustomGraphicFormatClass;
  Bitmap: TBitmap;
  SVG: TfrxSVGGraphic absolute Result;
begin
 Result := CreateNew(Graphic.Width, Graphic.Height, DestPixelFormat, True, DestQuality);
 if Graphic is TfrxSVGGraphic then
    Result.Assign(Graphic)
  else
  begin
    GHelper := GetGraphicFormats.FindByGraphic(TGraphicClass(Graphic.ClassType));
    if Assigned(GHelper) then
    begin
      Bitmap := TBitmap(GHelper.ConvertToBitmap(Graphic, pf32bit));
      Bitmap.Canvas.Lock;
      try
        TfrxHackSVGGraphic(SVG).Draw(Bitmap.Canvas, Rect(0, 0, SVG.Width, SVG.Height));
      finally
        Bitmap.Canvas.Unlock;
        Bitmap.Free;
      end;
    end;
  end;
end;

class function TfrxSVGGraphicFormat.CreateNew(Width, Height: Integer;
  PixelFormat: TPixelFormat; Transparent: Boolean; Quality: Integer): TGraphic;
begin
  Result := TfrxSVGGraphic.Create;
  Result.Width := Width;
  Result.Height := Height;
end;

class procedure TfrxSVGGraphicFormat.DrawExt(const GProps: TfrxDrawGraphicExt;
  Canvas: TCanvas; AGraphic: TGraphic; const Area: TRect; ScaleX,
  ScaleY: Double);
begin
  if TfrxHackSVGGraphic(AGraphic).IsAutoSize then
    DoDrawExt(GProps, Canvas, AGraphic, Area, ScaleX, ScaleY)
  else
    inherited DrawExt(GProps, Canvas, AGraphic, Area, ScaleX, ScaleY);
end;

class function TfrxSVGGraphicFormat.GetFormatCapabilities: TfrxGraphicFormatCaps;
begin
  Result := inherited GetFormatCapabilities - [gcGetCanvas];
end;

class function TfrxSVGGraphicFormat.GetGraphicClass: TGraphicClass;
begin
  Result := TfrxSVGGraphic;
end;

class function TfrxSVGGraphicFormat.GetGraphicProps(
  Graphic: TGraphic): TfrxGraphicProps;
var
  Metafile: TMetafile absolute Graphic;
begin
  Result.HasAlpha := True;
  Result.Transparent := True;
  Result.TransparentColor := clNone;
  Result.Quality := 100;
  Result.PixelFormat := pfDevice;
end;

class function TfrxSVGGraphicFormat.HasAlphaChanel(Graphic: TGraphic): Boolean;
begin
  Result := True;
end;

initialization
  GetGraphicFormats.RegisterFormat(TfrxSVGGraphicFormat);

finalization
  GetGraphicFormats.UnregisterFormat(TfrxSVGGraphicFormat);

end.
