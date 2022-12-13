
{******************************************}
{                                          }
{             FastReport VCL               }
{               SVG Graphic                }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}
unit frxSVGGraphic;

interface

{$I frx.inc}

uses
  Windows, Classes, Graphics,
  frxSVGBase, frxSVGHelpers, frxHelpers;

type
  TfrxSVGGraphic = class(TGraphic)
  private
    FRootObj: TSVGRootObj;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure DrawToSize(ACanvas: TCanvas; const Rect: TRect);

    function GetEmpty: Boolean; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    procedure AssignTo(Dest: TPersistent); override;

    procedure SetExternalParams(BoundsRect: TRect; ScaleX, ScaleY: Single; Centered: Boolean);
    procedure Clear;
    function IsAutoSize: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromText(const Text: string);
    procedure LoadFromFile(const Filename: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
  end;

  TfrxSVGGraphicCache = class (TOwnObjList)
  protected
    function IsFindHash(MD5: AnsiString; out Index: Integer): Boolean;
  public
    function GraphicFromText(const Text: string): TGraphic;
    function GraphicFromFile(const Filename: string): TGraphic;
    function GraphicFromStream(Stream: TStream): TGraphic;
  end;

implementation

uses
  Math, frxSVGComponents, frxmd5;

type
  TSVGGraphicCacheObj = class
  private
    FGraphic: TfrxSVGGraphic;
    FMD5: AnsiString;
  public
    constructor CreateFromFile(const Filename: string; AMD5: AnsiString = '');
    constructor CreateFromStream(Stream: TStream; AMD5: AnsiString = '');
    constructor CreateFromText(const Text: string; AMD5: AnsiString = '');
    destructor Destroy; override;


    property Graphic: TfrxSVGGraphic read FGraphic;
    property MD5: AnsiString read FMD5;
  end;

function CacheObj(O: TObject): TSVGGraphicCacheObj;
begin
  Result := O as TSVGGraphicCacheObj;
end;

{ TfrxSVGGraphic }

procedure TfrxSVGGraphic.AssignTo(Dest: TPersistent);
var
  D: TfrxSVGGraphic;
begin
  if Dest is TfrxSVGGraphic then
  begin
    D := TfrxSVGGraphic(Dest);

//    D.FRootObj.LoadFromText(FRootObj.Source);
    D.FRootObj.Free;
    D.FRootObj := TSVGRootObj(FRootObj.Clone(nil));

    Changed(Dest);
  end;
end;

procedure TfrxSVGGraphic.Clear;
begin
  FRootObj.Clear;
  Changed(Self);
end;

constructor TfrxSVGGraphic.Create;
begin
  inherited Create;
  FRootObj := TSVGRootObj.Create;
end;

procedure TfrxSVGGraphic.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

destructor TfrxSVGGraphic.Destroy;
begin
  FRootObj.Free;
  inherited Destroy;
end;

procedure TfrxSVGGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);

  function IsNeedsToIncrease(const Rect: TRect; out IncreasedRect: TRect): boolean;
  const
    MetaSize = 8000;
  var
    w, h: Integer;
    Factor: Double;
  begin
    w := Rect.Right - Rect.Left;
    h := Rect.Bottom - Rect.Top;
    Factor := Min(MetaSize / Max(1, w), MetaSize / Max(1, h));
    Result := Factor > 1;
    if Result then
      IncreasedRect := Bounds(0, 0, Round(w * Factor), Round(h * Factor));
  end;

var
  IncreasedRect: TRect;
  Metafile: TMetafile;
  MetafileCanvas: TMetafileCanvas;
begin
  if Empty then
    Exit;

  if (ACanvas is TMetafileCanvas) and IsNeedsToIncrease(Rect, IncreasedRect) then
  begin
    Metafile := TMetafile.Create;
    try
      Metafile.Width := IncreasedRect.Right - IncreasedRect.Left;
      Metafile.Height := IncreasedRect.Bottom - IncreasedRect.Top;

      MetafileCanvas := TMetafileCanvas.Create(Metafile, 0);
      try
        try
          MetafileCanvas.Lock;
          DrawToSize(MetafileCanvas, IncreasedRect);
        finally
          MetafileCanvas.Unlock;
        end;
      finally
        MetafileCanvas.Free;
      end;

      ACanvas.StretchDraw(Rect, Metafile);

    finally
      Metafile.Free;
    end;
  end
  else
    DrawToSize(ACanvas, Rect);
end;

procedure TfrxSVGGraphic.DrawToSize(ACanvas: TCanvas; const Rect: TRect);
var
  SaveBounds: TSingleBounds;
  ScaleX, ScaleY: Single;
begin
  SaveBounds := FRootObj.ExternalBounds;

  if FRootObj.atIsPercent(at_width) then
    ScaleX := SaveBounds.Width / FRootObj.atLengthAuto(at_width)
  else
    ScaleX := (Rect.Right - Rect.Left) / FRootObj.GetWidth;

  if FRootObj.atIsPercent(at_height) then
    ScaleY := SaveBounds.Height / FRootObj.atLengthAuto(at_height)
  else
    ScaleY := (Rect.Bottom - Rect.Top) / FRootObj.GetHeight;

  try
    SetExternalParams(Rect, ScaleX, ScaleY, False);
    FRootObj.PaintTo(ACanvas.Handle);
  finally
    FRootObj.ExternalBounds := SaveBounds;
  end;
end;

function TfrxSVGGraphic.GetEmpty: Boolean;
begin
  Result := FRootObj.Count = 0;
end;

function TfrxSVGGraphic.GetHeight: Integer;
begin
  Result := Round(FRootObj.GetOuterHeight);
end;

function TfrxSVGGraphic.GetWidth: Integer;
begin
  Result := Round(FRootObj.GetOuterWidth);
end;

function TfrxSVGGraphic.IsAutoSize: Boolean;
begin
  Result := FRootObj.atIsPercent(at_width) or FRootObj.atIsPercent(at_height);
end;

procedure TfrxSVGGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin
  inherited;
end;

procedure TfrxSVGGraphic.LoadFromFile(const Filename: string);
begin
  FRootObj.LoadFromFile(FileName);
  Changed(Self);
end;

procedure TfrxSVGGraphic.LoadFromStream(Stream: TStream);
begin
  try
    FRootObj.LoadFromStream(Stream);
  except
  end;
  Changed(Self);
end;

procedure TfrxSVGGraphic.LoadFromText(const Text: string);
begin
  try
    FRootObj.LoadFromText(Text);
  except
  end;
  Changed(Self);
end;

procedure TfrxSVGGraphic.ReadData(Stream: TStream);
var
  Size: LongInt;
  MemoryStream: TMemoryStream;
begin
  Stream.Read(Size, SizeOf(Size));
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.CopyFrom(Stream, Size);
    MemoryStream.Position := 0;
    FRootObj.LoadFromStream(MemoryStream);
  finally
    MemoryStream.Free;
  end;
end;

procedure TfrxSVGGraphic.SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE);
begin
  inherited;
end;

procedure TfrxSVGGraphic.SaveToStream(Stream: TStream);
begin
  FRootObj.SaveToStream(Stream);
end;

procedure TfrxSVGGraphic.SetExternalParams(BoundsRect: TRect; ScaleX, ScaleY: Single; Centered: Boolean);
begin
  FRootObj.ExternalBounds := ToSingleBounds(BoundsRect);
  FRootObj.ExternalScale := ToSinglePoint(ScaleX, ScaleY);
  FRootObj.ExternalCentered := Centered;
end;

procedure TfrxSVGGraphic.SetHeight(Value: Integer);
var
  Bounds: TSingleBounds;
begin
  if not SameValue(FRootObj.ExternalBounds.Height, Value) then
  begin
    Bounds := FRootObj.ExternalBounds;
    Bounds.Height := Value;
    FRootObj.ExternalBounds := Bounds;
  end;
end;

procedure TfrxSVGGraphic.SetWidth(Value: Integer);
var
  Bounds: TSingleBounds;
begin
  if not SameValue(FRootObj.ExternalBounds.Width, Value) then
  begin
    Bounds := FRootObj.ExternalBounds;
    Bounds.Width := Value;
    FRootObj.ExternalBounds := Bounds;
  end;
end;

procedure TfrxSVGGraphic.WriteData(Stream: TStream);
var
  Size: LongInt;
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    FRootObj.SaveToStream(MemoryStream);
    Size := MemoryStream.Size;
    Stream.Write(Size, SizeOf(Size));
    MemoryStream.Position := 0;
    MemoryStream.SaveToStream(Stream);
  finally
    MemoryStream.Free;
  end;
end;

{ TfrxSVGGraphicCacheObj }

constructor TSVGGraphicCacheObj.CreateFromFile(const Filename: string; AMD5: AnsiString = '');
begin
  FGraphic := TfrxSVGGraphic.Create;
  FGraphic.LoadFromFile(FileName);
  if AMD5 <> '' then
    FMD5 := AMD5
  else
    FMD5 := MD5File(Filename);
end;

constructor TSVGGraphicCacheObj.CreateFromStream(Stream: TStream; AMD5: AnsiString = '');
begin
  FGraphic := TfrxSVGGraphic.Create;
  FGraphic.LoadFromStream(Stream);
  if AMD5 <> '' then
    FMD5 := AMD5
  else
    FMD5 := MD5Stream(Stream);
end;

constructor TSVGGraphicCacheObj.CreateFromText(const Text: string; AMD5: AnsiString = '');
begin
  FGraphic := TfrxSVGGraphic.Create;
  FGraphic.LoadFromText(Text);
  if AMD5 <> '' then
    FMD5 := AMD5
  else
    FMD5 := MD5String(AnsiString(FGraphic.FRootObj.Source));
end;

destructor TSVGGraphicCacheObj.Destroy;
begin
  FGraphic.Free;
  inherited;
end;

{ TfrxSVGGraphicCache }

function TfrxSVGGraphicCache.GraphicFromFile(const Filename: string): TGraphic;
var
  MD5: AnsiString;
  Index: Integer;
begin
  MD5 := MD5File(Filename);
  if not IsFindHash(MD5, Index) then
    Index := Add(TSVGGraphicCacheObj.CreateFromFile(Filename, MD5));

  Result := CacheObj(Items[Index]).Graphic;
end;

function TfrxSVGGraphicCache.GraphicFromStream(Stream: TStream): TGraphic;
var
  MD5: AnsiString;
  Index: Integer;
begin
  MD5 := MD5Stream(Stream);
  if not IsFindHash(MD5, Index) then
    Index := Add(TSVGGraphicCacheObj.CreateFromStream(Stream, MD5));

  Result := CacheObj(Items[Index]).Graphic;
end;

function TfrxSVGGraphicCache.GraphicFromText(const Text: string): TGraphic;
var
  MD5: AnsiString;
  Index: Integer;
begin
  MD5 := MD5String(AnsiString(Text));
  if not IsFindHash(MD5, Index) then
    Index := Add(TSVGGraphicCacheObj.CreateFromText(Text, MD5));

  Result := CacheObj(Items[Index]).Graphic;
end;

function TfrxSVGGraphicCache.IsFindHash(MD5: AnsiString; out Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0  to Count - 1 do
    if CacheObj(Items[i]).MD5 = MD5 then
    begin
      Result := True;
      Index := i;
      Exit;
    end;

end;

initialization
  TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TfrxSVGGraphic);

finalization
  TPicture.UnregisterGraphicClass(TfrxSVGGraphic);
end.
