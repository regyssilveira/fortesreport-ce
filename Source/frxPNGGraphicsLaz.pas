
{******************************************}
{                                          }
{             FastReport v6.0              }
{            Graphic routines              }
{                                          }
{         Copyright (c) 1998-2018          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit frxPNGGraphicsLaz;

interface

{$I frx.inc}

uses
  SysUtils, {$IFNDEF FPC}Windows, Messages,{$ENDIF}
  Classes, Graphics, frxPictureGraphics, frxBaseGraphicsTypes;

const
  frxPNGFileFormat = 5;

implementation

uses
 frxUtils
{$IFNDEF FPC}
{$IFDEF Delphi12}
 , pngimage
{$ELSE}
 , frxpngimage
{$ENDIF}
{$ENDIF}
;

type
{$IFDEF Delphi12}
  TPNGObject = class(TPngImage);
{$ENDIF}
{$IFDEF FPC}
  TPNGObject = class(TPortableNetworkGraphic);
{$ENDIF}
  THackGraphic = class(TGraphic);
  TfrxPNGGraphicFormat = class(TfrxFullAbilitiesGraphicFormat)
  protected
    class function GetCanvasHelperClass: TfrxGraphicCanvasHelperClass; override;
{$IFNDEF FPC}
    class procedure AssignBitmapToAPNG(PNG: TPngObject; ABitmap: TBitmap);
{$ENDIF}
  public
    class function ConvertToBitmap(Graphic: TGraphic; DestPixelFormat: TPixelFormat): TGraphic; override;
    class function ConvertFrom(Graphic: TGraphic; DestPixelFormat: TPixelFormat; DestQuality: Integer = 100): TGraphic; override;
    class function CreateNew(Width: Integer; Height: Integer; PixelFormat: TPixelFormat; Transparent: Boolean; Quality: Integer = 100): TGraphic; override;
    class function CreateFromStream(const Stream: TStream): TGraphic; override;
    class procedure DrawTransparent(Canvas: TCanvas; AGraphic: TGraphic; Area: TRect; MaskColor: TColor; Quality: TfrxGraphicQuality); override;
    class procedure Draw(Canvas: TCanvas; AGraphic: TGraphic; Area: TRect; Quality: TfrxGraphicQuality); override;
    class function GetGraphicClass: TGraphicClass; override;
    class function GetGraphicMime: String; override;
    class function GetGraphicName: String; override;
    class function GetGraphicExt: String; override;
    class function GetGraphicConst: Integer; override;
    class function GetAlphaBitmap(Graphic: TGraphic): TBitmap; override;
    class function GetGraphicProps(Graphic: TGraphic): TfrxGraphicProps; override;
    class function HasAlphaChanel(Graphic: TGraphic): Boolean; override;
    class function IsSupportedFormat(const Stream: TStream): Boolean; override;
    class function LoadFromStream(const aPictire: TPicture; Stream: TStream): Boolean; override;
    class procedure SetTransparent(Graphic: TGraphic; Transparent: Boolean); override;
    class procedure SetTransparentColor(Graphic: TGraphic; Color: TColor); override;
    class function GetTransparentColor(Graphic: TGraphic): TColor; override;
  end;

  TfrxPNGCanvasHelper = class(TfrxGraphicCanvasHelper)
  private
    FBitmap: TBitmap;
  protected
    function GetCanvas: TCanvas; override;
    function GetCanvasGraphic: TGraphic; override;
  public
    procedure ReleaseCanvas; override;
  end;

{$IFNDEF FPC}

function PNGToPNGA(PNG: TPngObject): TPngObject;
var
  PNGA: TPngObject;
  tRNS: TChunktRNS;
  PLTE: TChunkPLTE;
  dst: pRGBLine;
  src, alpha: {$IFDEF Delphi12}pngimage.{$ELSE}frxpngimage.{$ENDIF}pByteArray;
  X, Y: Integer;
  i: byte;
begin
  PNGA := TPngObject.CreateBlank(COLOR_RGBALPHA, 8, PNG.Width, PNG.Height);
  case PNG.Header.ColorType of
    COLOR_PALETTE:
      begin
        tRNS := PNG.Chunks.ItemFromClass(TChunktRNS) as TChunktRNS;
        PLTE := PNG.Chunks.ItemFromClass(TChunkPLTE) as TChunkPLTE;
        for Y := 0 to PNG.Height - 1 do
        begin
          dst := PNGA.Scanline[Y];
          src := PNG.Scanline[Y];
          alpha := PNGA.AlphaScanline[Y];
          for X := 0 to PNG.Width - 1 do
          begin
            case PNG.Header.BitDepth of
              8:
                i := src[X];
              2, 4:
                i := src[X div 2] shr ((1 - (X mod 2)) * 4) and $0F;
              1:
                i := src[X div 8] shr (7 - (X mod 8)) and 1;
            else
              raise Exception.Create('Unknown PNG BitDepth');
            end;
            dst[X].rgbtBlue := PLTE.Item[i].rgbBlue;
            dst[X].rgbtGreen := PLTE.Item[i].rgbGreen;
            dst[X].rgbtRed := PLTE.Item[i].rgbRed;
            if tRNS <> nil then
              alpha[X] := tRNS.PaletteValues[i]
            else
              alpha[X] := 255;
          end;
        end;
      end;

    COLOR_RGB, COLOR_GRAYSCALE:
      begin
        PNGA.Canvas.Lock;
        try
          BitBlt(PNGA.Canvas.Handle, 0, 0, PNGA.Width, PNGA.Height,
            PNG.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          PNGA.Canvas.Unlock;
        end;
        for Y := 0 to PNG.Height - 1 do
          FillChar(PNGA.AlphaScanline[Y]^, PNG.Width, 255);
      end;

    COLOR_GRAYSCALEALPHA:
      begin
        PNGA.Canvas.Lock;
        try
          BitBlt(PNGA.Canvas.Handle, 0, 0, PNGA.Width, PNGA.Height,
            PNG.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          PNGA.Canvas.Unlock;
        end;
        for Y := 0 to PNG.Height - 1 do
        begin
          src := PNG.AlphaScanline[Y];
          alpha := PNGA.AlphaScanline[Y];
          Move(src^, alpha^, PNG.Width);
        end;
      end;

  else
    PNGA.Assign(PNG);
  end;
  Result := PNGA;
end;

procedure Png32ToTransparentBitmap32(PNG: TPngObject; out Bitmap: TBitmap);
var
  AlphaScanline: {$IFDEF Delphi12}pngimage.{$ELSE}frxpngimage.{$ENDIF}pByteArray;
  Alpha: Byte;
  Dest: PByte;
  Source: PByte;
  y, x: Integer;
begin
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf32bit;
  Bitmap.Width := PNG.Width;
  Bitmap.Height := PNG.Height;
//  Bitmap.Canvas.Lock;
  Bitmap.Canvas.Draw(0, 0, PNG);

  for y := 0 to PNG.Height - 1 do
  begin
    AlphaScanline := PNG.AlphaScanline[y];
    Dest := Bitmap.ScanLine[y];
    Source := PNG.Scanline[y];
    for x := 0 to PNG.Width - 1 do
    begin
      Alpha := AlphaScanline^[x];
      Dest^ := MulDiv(Source^, Alpha, 255); Inc(Dest); Inc(Source);
      Dest^ := MulDiv(Source^, Alpha, 255); Inc(Dest); Inc(Source);
      Dest^ := MulDiv(Source^, Alpha, 255); Inc(Dest); Inc(Source);
      Dest^ := Alpha;                     Inc(Dest);
    end;
  end;
end;


function IsCanPngToTransparentBitmap32(PNG: TPngObject; out Bitmap: TBitmap): boolean;
var
  PNGA: TPngObject;
begin
  Result := PNG.TransparencyMode in [ptmBit, ptmPartial];
  if Result then
    if PNG.Header.ColorType = COLOR_RGBALPHA then
      Png32ToTransparentBitmap32(PNG, Bitmap)
    else
    begin
      PNGA := PNGToPNGA(PNG);
      Png32ToTransparentBitmap32(PNGA, Bitmap);
      PNGA.Free;
    end;
end;

const
  Bitmap32BF: TBlendFunction = ( BlendOp: AC_SRC_OVER; BlendFlags: 0;
    SourceConstantAlpha: 255; AlphaFormat: AC_SRC_ALPHA );
{$ENDIF}

{ TfrxPNGGraphicFormat }
{$IFNDEF FPC}
class procedure TfrxPNGGraphicFormat.AssignBitmapToAPNG(PNG: TPngObject; ABitmap: TBitmap);
var
  SourceBM, DestPNG, AlphaPNG: PByte;
  y, x: Integer;
  S1, S2, S3, Alpha: Byte;
  Factor: Double;
begin
  for Y := 0 to ABitmap.Height - 1 do
  begin
    SourceBM := ABitmap.Scanline[Y];
    DestPNG := PNG.Scanline[Y];
    AlphaPNG := Pointer(PNG.AlphaScanline[Y]);
    for X := 0 to ABitmap.Width - 1 do
    begin
      S1 := SourceBM^; Inc(SourceBM);
      S2 := SourceBM^; Inc(SourceBM);
      S3 := SourceBM^; Inc(SourceBM);
      alpha := SourceBM^; Inc(SourceBM);

      if alpha > 0 then
        Factor := 255 / alpha
      else
        Factor := 0;

      DestPNG^ := Round(S1 * Factor); Inc(DestPNG);
      DestPNG^ := Round(S2 * Factor); Inc(DestPNG);
      DestPNG^ := Round(S3 * Factor); Inc(DestPNG);
      if AlphaPNG <> nil then
      begin
       AlphaPNG^ := alpha;
        Inc(AlphaPNG);
      end;
    end;
  end
end;
{$ENDIF}

class function TfrxPNGGraphicFormat.ConvertFrom(Graphic: TGraphic; DestPixelFormat: TPixelFormat; DestQuality: Integer): TGraphic;
var
  Bitmap: TBitmap absolute Graphic;
  HasAlpha, NeedBitmap: Boolean;
begin
  if Graphic is GetGraphicClass then
  begin
    Result := CreateNew(Graphic.Width, Graphic.Height, DestPixelFormat, DestPixelFormat = pf32bit, DestQuality);
    TPngObject(Result).Canvas.Brush.Color := clWhite;
    TPngObject(Result).Canvas.FillRect(0, 0, Graphic.Width, Graphic.Height);
    Result.Assign(Graphic);
    Exit;
  end;
  NeedBitmap := not (Graphic is TBitmap);
  if NeedBitmap then
    Graphic := GetGraphicFormats.ConvertToBitmap(Graphic, DestPixelFormat);
  try
    HasAlpha := (DestPixelFormat = pf32bit) and (Bitmap.PixelFormat = pf32bit);
    Result := CreateNew(Bitmap.Width, Bitmap.Height, DestPixelFormat, HasAlpha, DestQuality);
    // TODO: Alpha
    if HasAlpha then
      Result.Assign(Graphic)
    else Result.Assign(Graphic);
    //if not HasAlpha then
    //  TPngObject(Result).TransparentColor := $FFFFFF;
  finally
    if NeedBitmap then Graphic.Free;
  end;
end;

class function TfrxPNGGraphicFormat.ConvertToBitmap(Graphic: TGraphic;
  DestPixelFormat: TPixelFormat): TGraphic;
var
  ABitmap: TBitmap;
//  PNGA: TPngObject absolute Graphic;
//  ResBitmap: TBitmap absolute Result;
begin
  Result := nil;
//  if (PNGA.TransparencyMode in [ptmBit, ptmPartial]) and (PNGA.Header.ColorType = COLOR_RGBALPHA) then
//     Png32ToTransparentBitmap32(PNGA, ResBitmap);

  if Result = nil then
    Result := inherited ConvertToBitmap(Graphic, DestPixelFormat);
  { invert black transparent color of PNG image with alpha to white fill color }
  { used when convert PNG with alpha to non transcarent image like JPEG }
  if (TPngObject(Graphic).Transparent) and (DestPixelFormat <> pf32bit) then
  begin
    ABitmap := TBitmap(GetAlphaBitmap(Graphic));
    try
      Result := inherited ConvertToBitmap(Graphic, DestPixelFormat);
      TBitmap(Result).Canvas.Lock;
      TBitmap(Result).Canvas.CopyMode := cmMergePaint;
      TBitmap(Result).Canvas.Draw(0, 0, ABitmap);
      TBitmap(Result).Canvas.Unlock;
    finally
      ABitmap.Free;
    end;
  end;

end;

class function TfrxPNGGraphicFormat.CreateFromStream(
  const Stream: TStream): TGraphic;
begin
  Result := inherited CreateFromStream(Stream);
end;

class function TfrxPNGGraphicFormat.CreateNew(Width, Height: Integer;
  PixelFormat: TPixelFormat; Transparent: Boolean; Quality: Integer): TGraphic;
begin
  Result := TPortableNetworkGraphic.Create;
  TPortableNetworkGraphic(Result).PixelFormat := PixelFormat;
  TPortableNetworkGraphic(Result).SetSize(Width, Height);
end;

class procedure TfrxPNGGraphicFormat.Draw(Canvas: TCanvas; AGraphic: TGraphic;
  Area: TRect; Quality: TfrxGraphicQuality);
begin
{$IFNDEF FPC}
  if (Quality in [gqPrint, gqHiQuality]) and HasAlphaChanel(AGraphic) or (Canvas.ClassType = frxDefaultMetaCanvasClass) then
    DrawTransparent(Canvas, AGraphic, Area, clNone, Quality)
  else
{$ENDIF}
    inherited Draw(Canvas, AGraphic, Area, Quality);
end;

class procedure TfrxPNGGraphicFormat.DrawTransparent(Canvas: TCanvas;
  AGraphic: TGraphic; Area: TRect; MaskColor: TColor;
  Quality: TfrxGraphicQuality);
var
  Bitmap: TBitmap;
begin
{$IFNDEF FPC}
  if (Quality = gqFast) and (Canvas.ClassType <> frxDefaultMetaCanvasClass) and (MaskColor = clNone) then
    Draw(Canvas, AGraphic, Area, Quality)// faster and bad smooth
  else if (MaskColor = clNone) and ((AGraphic is TPngObject){$IFDEF Delphi12} or (AGraphic is TPngImage){$ENDIF}) and
     IsCanPngToTransparentBitmap32(TPngObject(AGraphic), Bitmap) then
  begin
    { print, metafile and HQ pnly}
    SetStretchBltMode(Canvas.Handle, STRETCH_HALFTONE);
    AlphaBlend(Canvas.Handle,
                 Area.Left, Area.Top,
                 Area.Right - Area.Left, Area.Bottom - Area.Top,
               Bitmap.Canvas.Handle,
                 0, 0,
                 AGraphic.Width, AGraphic.Height,
               Bitmap32BF);
    Bitmap.Free;
  end
  else
{$ENDIF}
    inherited DrawTransparent(Canvas, AGraphic, Area, MaskColor, Quality);
end;

class function TfrxPNGGraphicFormat.GetAlphaBitmap(Graphic: TGraphic): TBitmap;
var
  PNGA: TPNGObject absolute Graphic;
  i: Integer;
  AlphaBitmap: TBitmap absolute Result;
begin
  Result := nil;
  if not (Graphic is TPngObject)
{$IFDEF Delphi12}
     and not (Graphic is TPngImage)
{$ENDIF}
  then Exit;
  Result := TBitmap.Create;
  AlphaBitmap.PixelFormat := pf8bit;
  AlphaBitmap.Width := PNGA.Width;
  AlphaBitmap.Height := PNGA.Height;
  //TODO: Alpha
  //for i := 0 to AlphaBitmap.Height - 1 do
  //  CopyMemory(AlphaBitmap.ScanLine[i], PNGA.AlphaScanline[i], AlphaBitmap.Width);

end;

class function TfrxPNGGraphicFormat.GetCanvasHelperClass: TfrxGraphicCanvasHelperClass;
begin
  Result := TfrxPNGCanvasHelper;
end;

class function TfrxPNGGraphicFormat.GetGraphicClass: TGraphicClass;
begin
{$IFDEF Delphi12}
  Result := TPngImage;
{$ELSE}
  Result := TPortableNetworkGraphic;
{$ENDIF}
end;

class function TfrxPNGGraphicFormat.GetGraphicConst: Integer;
begin
  Result := frxPNGFileFormat;
end;

class function TfrxPNGGraphicFormat.GetGraphicExt: String;
begin
  Result := '.png';
end;

class function TfrxPNGGraphicFormat.GetGraphicMime: String;
begin
  Result := 'image/png';
end;

class function TfrxPNGGraphicFormat.GetGraphicName: String;
begin
  Result := 'PNG';
end;

class function TfrxPNGGraphicFormat.GetGraphicProps(
  Graphic: TGraphic): TfrxGraphicProps;
var
  PNG: TPNGObject absolute Graphic;
begin
  Result.HasAlpha := PNG.Transparent;
  Result.Transparent := PNG.Transparent;
  Result.TransparentColor := PNG.TransparentColor;
  Result.Quality := 100;
  Result.PixelFormat := PNG.PixelFormat;
end;

class function TfrxPNGGraphicFormat.GetTransparentColor(
  Graphic: TGraphic): TColor;
begin
  Result := clNone;
  if (TPngObject(Graphic).Transparent)then
    Result := TPngObject(Graphic).TransparentColor;
end;
class function TfrxPNGGraphicFormat.HasAlphaChanel(Graphic: TGraphic): Boolean;
begin
  Result := False; // TODO;
end;

class function TfrxPNGGraphicFormat.IsSupportedFormat(
  const Stream: TStream): Boolean;
const
  OriginalPngHeader: array[0..7] of AnsiChar = (#137, #80, #78, #71, #13, #10, #26, #10);
var
  PNGHeader: array[0..7] of AnsiChar;
  pos: Integer;
begin
  Result := False;
  if (Stream.Size - Stream.Position) >= SizeOf(PNGHeader) then
  begin
    pos := Stream.Position;
    Stream.ReadBuffer(PNGHeader, SizeOf(PNGHeader));
    Stream.Position := pos;
    if PNGHeader = OriginalPngHeader then
      Result := True;
  end;
end;

class function TfrxPNGGraphicFormat.LoadFromStream(const aPictire: TPicture;
  Stream: TStream): Boolean;
begin
  Result := False;
  Result := inherited LoadFromStream(aPictire, Stream);
  //THackGraphic(aPictire.Graphic).Changed(nil);
  Result := True;
end;

class procedure TfrxPNGGraphicFormat.SetTransparent(Graphic: TGraphic;
  Transparent: Boolean);
begin
  TPngObject(Graphic).Transparent := Transparent;
end;

class procedure TfrxPNGGraphicFormat.SetTransparentColor(Graphic: TGraphic;
  Color: TColor);
begin
  TPngObject(Graphic).TransparentColor := Color;
end;

{ TfrxPNGCanvasHelper }

function TfrxPNGCanvasHelper.GetCanvas: TCanvas;
begin
  if not TPngObject(FGraphic).Transparent then
    Result := TPngObject(FGraphic).Canvas
  else
  begin
    Result := TPngObject(FGraphic).Canvas
    //FBitmap := TBitmap(TfrxPNGGraphicFormat.ConvertToBitmap(FGraphic, pf32bit));
    //Result := FBitmap.Canvas;
  end;
end;

function TfrxPNGCanvasHelper.GetCanvasGraphic: TGraphic;
begin
  Result := FBitmap;
end;

procedure TfrxPNGCanvasHelper.ReleaseCanvas;
begin
  if Assigned(FBitmap) then
  begin
    //TfrxPNGGraphicFormat.AssignBitmapToAPNG(TPngObject(FGraphic), FBitmap);
    FreeAndNil(FBitmap);
  end;
  inherited;
end;

initialization
{$IFDEF Delphi12}
  TPicture.RegisterFileFormat('PNG_OLD', 'Portable Network Graphics', TPNGObject);
{$ENDIF}
  GetGraphicFormats.RegisterFormat(TfrxPNGGraphicFormat);

finalization
  GetGraphicFormats.UnregisterFormat(TfrxPNGGraphicFormat);
{$IFDEF Delphi12}
  TPicture.UnregisterGraphicClass(TPNGObject);
{$ENDIF}

end.
