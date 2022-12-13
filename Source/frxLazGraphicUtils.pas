{******************************************}
{                                          }
{             FastReport VCL               }
{         Win platform routines            }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxLazGraphicUtils;

interface

{$I frx.inc}

uses
  SysUtils,
  {$IFNDEF Linux}
  Windows,
  {$ELSE}
  LCLType, LCLIntf, LazHelper,
  {$ENDIF}
  Messages, Classes, Graphics, frxBaseGraphicsTypes
{$IFDEF DELPHI16}
, System.UITypes
{$ENDIF}
;
procedure frxApplyMask(MaskBM: TBitmap; SourceBM: TBitmap; MaskColor: TColor);
function IsBlendingCompatibleDevice(Canvas: TCanvas): Boolean;
procedure frxDrawGraphic(Canvas: TCanvas; DestRect: TRect; aGraph: TGraphic;
  IsPrinting, Smooth, Transparent: Boolean; TransparentColor: TColor; Alpha: TBitmap);

procedure frxDrawGraphicBlend(Canvas: TCanvas; Area: TRect; PF32: TBitmap; Quality: TfrxGraphicQuality);

implementation

function IsBlendingCompatibleDevice(Canvas: TCanvas): Boolean;
begin
  Result := False;
end;

procedure frxDrawGraphicBlend(Canvas: TCanvas; Area: TRect; PF32: TBitmap; Quality: TfrxGraphicQuality);
begin
  Canvas.StretchDraw(Area, PF32);
end;

procedure PrintBitmap(aHandle: HDC; Dest: TRect; Bitmap: TBitmap);
var
  Info: PBitmapInfo;
  HInfo: HGLOBAL;
  InfoSize: DWord;
  Image: Pointer;
  HImage: HGLOBAL;
  ImageSize: DWord;
  PrevStretchBltMode: Integer;
begin
  {$IFDEF FPC}
  {$warning TODO fix frxUtils.PrintBitmap}
  {$ELSE}
  with Bitmap do
  begin
    GetDIBSizes(Handle, InfoSize, ImageSize);
    HInfo := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, InfoSize);
    Info := PBitmapInfo(GlobalLock(HInfo));
    try
      HImage := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, ImageSize);
      Image := Pointer(GlobalLock(HImage));
      try
        GetDIB(Handle, Palette, Info^, Image^);
        with Info^.bmiHeader do
        begin
          PrevStretchBltMode := SetStretchBltMode(aHandle, STRETCH_HALFTONE);
          StretchDIBits(aHandle, Dest.Left, Dest.Top,
            Dest.RIght - Dest.Left, Dest.Bottom - Dest.Top,
            0, 0, biWidth, biHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
          SetStretchBltMode(aHandle, PrevStretchBltMode);
        end;
      finally
        GlobalUnlock(HImage);
        GlobalFree(HImage);
      end;
    finally
      GlobalUnlock(HInfo);
      GlobalFree(HInfo);
    end;
  end;
  {$ENDIF}
end;

{ Draw with smooth }
{$IFDEF FPC}
procedure DrawBitmap(aHandle: HDC; Dest: TRect; Bitmap: TBitmap);
var
  PrevStretchBltMode: Integer;
begin
  try
    PrevStretchBltMode := SetStretchBltMode(aHandle, STRETCH_HALFTONE);
    with Dest do
      StretchBlt(aHandle, Left, Top, Right - Left, Bottom - Top, Bitmap.Canvas.GetUpdatedHandle([csHandleValid]),
            0, 0, Bitmap.Width, Bitmap.Height, SRCCOPY);

  finally
    SetStretchBltMode(aHandle, PrevStretchBltMode);
  end;
end;
{$ELSE}
procedure DrawBitmap(aHandle: HDC; Dest: TRect; Bitmap: TBitmap);
var
  hMemDC: HDC;
  PrevStretchBltMode: Integer;
  hBmp: HBITMAP;
begin
  hMemDC := CreateCompatibleDC(aHandle);
  hBmp := SelectObject(hMemDC, Bitmap.Handle);
  try
    PrevStretchBltMode := SetStretchBltMode(aHandle, STRETCH_HALFTONE);
    with Dest do
      StretchBlt(aHandle, Left, Top, Right - Left, Bottom - Top, hMemDC,
            0, 0, Bitmap.Width, Bitmap.Height, SRCCOPY);
    SetStretchBltMode(aHandle, PrevStretchBltMode);
  finally
    SelectObject(hMemDC, hBmp);
    DeleteDC(hMemDC);
  end;
end;
{$ENDIF}


procedure frxApplyMask(MaskBM: TBitmap; SourceBM: TBitmap; MaskColor: TColor);
var
  hMaskDC, hStrechedDC, SourceMaskDC, DestDC: HDC;
  hMask, hStrechedBM: HBITMAP;
  oldFore, oldBack, oldBkColor: Cardinal;
begin
  SourceMaskDC := CreateCompatibleDC(GetDC(0));
  SelectObject(SourceMaskDC, SourceBM.Handle);
  hMaskDC := CreateCompatibleDC(GetDC(0));
  hMask := CreateBitmap(MaskBM.Width, MaskBM.Height, 1, 1, nil);
  DestDC := CreateCompatibleDC(GetDC(0));
  SelectObject(DestDC, MaskBM.Handle);
  SelectObject(hMaskDC, hMask);
  try
    if (MaskBM.Width <> SourceBM.Width) or (MaskBM.Height <> SourceBM.Height) then
    begin
      hStrechedDC := CreateCompatibleDC(SourceMaskDC);
      hStrechedBM := CreateCompatibleBitmap(SourceMaskDC, SourceBM.Width, SourceBM.Height);
      try
        SetStretchBltMode(hStrechedDC, STRETCH_HALFTONE);
        SelectObject(hStrechedDC, hStrechedBM);
        StretchBlt(hStrechedDC, 0, 0, MaskBM.Width, MaskBM.Height, SourceMaskDC, 0, 0, SourceBM.Width, SourceBM.Height,  SRCCOPY);
        SetBkColor(hStrechedDC, ColorToRGB(MaskColor));
        BitBlt(hMaskDC, 0, 0, MaskBM.Width, MaskBM.Height, hStrechedDC, 0, 0,  NOTSRCCOPY);
      finally
        DeleteObject(hStrechedBM);
        DeleteDC(hStrechedDC);
      end;
    end
    else
    begin
      oldBkColor := SetBkColor(SourceMaskDC, ColorToRGB(MaskColor));
      BitBlt(hMaskDC, 0, 0, MaskBM.Width, MaskBM.Height, SourceMaskDC, 0, 0,  NOTSRCCOPY);
      SetBkColor(SourceMaskDC, oldBkColor);
    end;
    oldFore := SetTextColor(DestDC, 0);
    oldBack := SetBkColor(DestDC, RGB(255,255,255));
    BitBlt(DestDC, 0, 0, MaskBM.Width, MaskBM.Height, hMaskDC, 0, 0, SRCAND);
    SetTextColor(DestDC, oldFore);
    SetBkColor(DestDC, oldBack);
  finally
    DeleteDC(DestDC);
    DeleteDC(hMaskDC);
    DeleteDC(SourceMaskDC);
    DeleteObject(hMask);
  end;
end;

{ function create 2-bits mask from SourceDC with specify color value }
{ and draw it with White fill on DestDC                              }
procedure ApplyMask(DestDC: HDC; X, Y, Width, Height: Integer; SourceDC: HDC;
  SrcX, SrcY, SrcWidth, SrcHeight: Integer; MaskColor: TColor; AMask: TBitmap);
var
  hMaskDC, hStrechedDC, SourceMaskDC: HDC;
  hMask, hStrechedBM: HBITMAP;
  oldFore, oldBack, oldBkColor: Cardinal;
begin
  hMaskDC := CreateCompatibleDC(DestDC);
  hMask := CreateBitmap(Width, Height, 1, 1, nil);
  SelectObject(hMaskDC, hMask);
  SourceMaskDC := SourceDC;
  if Assigned(AMask) then
  begin
    SourceMaskDC := AMask.Canvas.Handle;
    MaskColor := clBlack;
  end;

  if (SrcWidth <> Width) or (SrcHeight <> Height) then
  begin
    hStrechedDC := CreateCompatibleDC(SourceMaskDC);
    hStrechedBM := CreateCompatibleBitmap(SourceMaskDC, Width, Height);
    try
      //SetStretchBltMode(hStrechedDC, STRETCH_HALFTONE);
      SelectObject(hStrechedDC, hStrechedBM);
      StretchBlt(hStrechedDC, 0, 0, Width, Height, SourceMaskDC, SrcX, SrcY, SrcWidth, SrcHeight,  SRCCOPY);
      SetBkColor(hStrechedDC, ColorToRGB(MaskColor));
      BitBlt(hMaskDC, 0, 0, Width, Height, hStrechedDC, 0, 0,  SRCCOPY);
    finally
      DeleteObject(hStrechedBM);
      DeleteDC(hStrechedDC);
    end;
  end
  else
  begin
    oldBkColor := SetBkColor(SourceMaskDC, ColorToRGB(MaskColor));
    BitBlt(hMaskDC, 0, 0, Width, Height, SourceMaskDC, 0, 0,  SRCCOPY);
    SetBkColor(SourceMaskDC, oldBkColor);
  end;
  try
    oldFore := SetTextColor(DestDC, 0);
    oldBack := SetBkColor(DestDC, RGB(255,255,255));
    BitBlt(DestDC, X, Y, Width, Height, hMaskDC, 0, 0, SRCAND);
    SetTextColor(DestDC, oldFore);
    SetBkColor(DestDC, oldBack);
  finally
    DeleteObject(hMask);
    DeleteDC(hMaskDC);
  end;
end;

{ function draws an image with transparent color, it has 2 mode: }
{ 1. Draws DIB directly on canvas, this used for printing.       }
{ Using memory DC could cause unexpected behaviour on printer    }
{ like color losing and etc (i.e. both StretchDIBits             }
{ with SRCINVERT should use printer DC).                         }
{ 2. Draw bitmap with using memory compatible DC,                }
{ it used for sreen draw.                                        }
{ TransparentBlt doesn't work with EMF,                          }
{ so it replaced  with 3 ROP's operation for correct export.     }
{$IFNDEF FPC}
procedure frxDrawTransparentBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap; Alpha: TBitmap; TransparenColor: TColor; UseDIB: Boolean);
var
  hMemDC: HDC;
  rWidth, rHeight: Integer;
  Info: PBitmapInfo;
  HInfo: HGLOBAL;
  InfoSize: DWord;
  Image: Pointer;
  HImage: HGLOBAL;
  ImageSize: DWord;
  StretchBltMode, PrevStretchBltMode, DIBColor: Integer;

  procedure CreateDIB;
  begin
    with Bitmap do
    begin
      GetDIBSizes(Handle, InfoSize, ImageSize);
      HInfo := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, InfoSize);
      Info := PBitmapInfo(GlobalLock(HInfo));
      try
        HImage := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, ImageSize);
        Image := Pointer(GlobalLock(HImage));
        try
          GetDIB(Handle, Palette, Info^, Image^);
        except
          GlobalUnlock(HImage);
          GlobalFree(HImage);
          HImage := 0;
        end;
      except
        GlobalUnlock(HInfo);
        GlobalFree(HInfo);
        HInfo := 0;
      end;
    end;
  end;

  procedure FreeDIB;
  begin
    if HImage <> 0 then
    begin
      GlobalUnlock(HImage);
      GlobalFree(HImage);
    end;
    if HInfo <> 0 then
    begin
      GlobalUnlock(HInfo);
      GlobalFree(HInfo);
    end;
  end;

  procedure DrawDIB;
  begin
    with Info^.bmiHeader, DestRect do
    begin
      SetStretchBltMode(Canvas.Handle, StretchBltMode);
      if UseDIB then
        StretchDIBits(Canvas.Handle, Left, Top, rWidth, rHeight,
          0, 0, biWidth, biHeight, Image, Info^, DIBColor, SRCINVERT)
      else
         StretchBlt(Canvas.Handle, Left, Top, rWidth, rHeight, hMemDC,
          0, 0, Bitmap.Width, Bitmap.Height, SRCINVERT);
    end;
  end;

begin
  StretchBltMode := STRETCH_HALFTONE;
  DIBColor := DIB_RGB_COLORS;
  rWidth := DestRect.Right - DestRect.Left;
  rHeight := DestRect.Bottom - DestRect.Top;

  case   Bitmap.PixelFormat of
    pf1bit: StretchBltMode := BLACKONWHITE;
    pf4bit..pf16bit: DIBColor := DIB_PAL_COLORS;
  end;

  if UseDIB then
    CreateDIB;
  with  DestRect do
  begin
    PrevStretchBltMode := SetStretchBltMode(Canvas.Handle, StretchBltMode);
    try
      hMemDC := CreateCompatibleDC(Canvas.Handle);
      SelectObject(hMemDC, Bitmap.Handle);
      DrawDIB;
      ApplyMask(Canvas.Handle, Left, Top, rWidth, rHeight, hMemDC, 0, 0, Bitmap.Width, Bitmap.Height, TransparenColor, Alpha);
      DrawDIB;
  finally
      DeleteDC(hMemDC);
      SetStretchBltMode(Canvas.Handle, PrevStretchBltMode);
      if UseDIB then
        FreeDIB;
    end;
  end;
end;
{$ENDIF}

{procedure frxDrawTransparentBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap; TransparenColor: TColor);
begin
  with DestRect do
    TransparentBlt(Canvas.Handle, Left, Top, Right - Left, Bottom - Top,
      Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, ColorToRGB(TransparenColor));
end;}

{$IFDEF FPC}
procedure frxDrawGraphic(Canvas: TCanvas; DestRect: TRect; aGraph: TGraphic;
  IsPrinting, Smooth, Transparent: Boolean; TransparentColor: TColor; Alpha: TBitmap);
var
  Bitmap: TBitmap;
begin
  if not IsPrinting then
    Canvas.StretchDraw(DestRect, aGraph)
  else
  begin
  {$IFDEF LCLGTK2}
    Canvas.StretchDraw(DestRect, aGraph);
  {$ELSE}
    Canvas.StretchDraw(DestRect, aGraph);
  {
    Bitmap := TBitmap.Create;
    Bitmap.Canvas.Lock;
    try
      Bitmap.Width := aGraph.Width;
      Bitmap.Height := aGraph.Height;
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Canvas.Draw(0, 0, aGraph);
      DrawBitmap(Canvas.Handle, DestRect, Bitmap);
    finally
      Bitmap.Canvas.Unlock;
      Bitmap.Free;
    end;
  }
  {$ENDIF}
  end;
end;
{$ELSE}
procedure frxDrawGraphic(Canvas: TCanvas; DestRect: TRect; aGraph: TGraphic;
  IsPrinting, Smooth, Transparent: Boolean; TransparentColor: TColor; Alpha: TBitmap);
var
  Bitmap: TBitmap;
  OldColor: TColor;
begin
  Canvas.Brush.Color := clWhite;// reset brush style
  // metafile shold be printed here
  //Bitmaps and other non-transparet pictures draw here
  if (aGraph is TMetaFile) or
    (not IsPrinting and (not Transparent or (aGraph is TBitmap)) and
      not Smooth)  then
  begin
    //aGraph.Transparent := Transparent;
    if (aGraph is TBitmap) and Transparent then
      frxDrawTransparentBitmap(Canvas, DestRect, TBitmap(aGraph), Alpha,
        TransparentColor, False)
    else
      Canvas.StretchDraw(DestRect, aGraph);
  end
  // used for printing pictures and drawing transparent pictures like JPEG/PNG
  // for printiong use DIB, for drawing memory DC
  else
  begin
    Bitmap := TBitmap.Create;
    Bitmap.Canvas.Lock;
    try
      Bitmap.HandleType := bmDIB;
      if IsPrinting then
        Bitmap.PixelFormat := pf32Bit// for print output
      else
        Bitmap.PixelFormat := pf24Bit;//for screen
      Bitmap.Width := aGraph.Width;
      Bitmap.Height := aGraph.Height;

      if Transparent then
      begin
        OldColor := Bitmap.Canvas.Brush.Color;
        Bitmap.Canvas.Brush.Color := ColorToRGB(TransparentColor);
        Bitmap.Canvas.FillRect(Rect(0,0,Bitmap.Width, Bitmap.Height));
        Bitmap.Canvas.Brush.Color := OldColor;
      end;
      Bitmap.Canvas.Draw(0, 0, aGraph);

      if Transparent{ and (TransparentColor <> clNone)} then
        frxDrawTransparentBitmap(Canvas, DestRect, Bitmap, Alpha, TransparentColor, IsPrinting)
      else if IsPrinting then
        PrintBitmap(Canvas.Handle, DestRect, Bitmap)
      else
        DrawBitmap(Canvas.Handle, DestRect, Bitmap);
    finally
      Bitmap.Canvas.Unlock;
      Bitmap.Free;
    end;
  end
end;
{$ENDIF}

end.
