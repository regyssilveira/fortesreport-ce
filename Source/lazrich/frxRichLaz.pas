unit frxRichLaz;

{$I frx.inc}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Variants,
  frxClass, RichMemo, frxRes, LazHelper;

type

  TfrxRichObject = class(TComponent);  // fake component
  TRxRichMemo = class;

  TRxParaAttributes = class(TPersistent)
  private
    RichMemo: TRxRichMemo;
    ParaMetric: TParaMetric;
    ParaNumbering: TParaNumbering;
    function GetAlignment: TParaAlignment;
    function GetFirstIndent: Longint;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetNumbering: TParaNumStyle;

    procedure SetAlignment(Value: TParaAlignment);
    procedure SetFirstIndent(Value: Longint);
    procedure SetLeftIndent(Value: Longint);
    procedure SetRightIndent(Value: Longint);
    procedure SetNumbering(Value: TParaNumStyle);
  protected
    //procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TRxRichMemo);
    //procedure Assign(Source: TPersistent); override;

    property Alignment: TParaAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Longint read GetFirstIndent write SetFirstIndent;
    property LeftIndent: Longint read GetLeftIndent write SetLeftIndent;
    property RightIndent: Longint read GetRightIndent write SetRightIndent;
    property Numbering: TParaNumStyle read GetNumbering write SetNumbering;
  end;

  TRxRichMemo = class(TRichMemo)
  private
    FParagraph: TRxParaAttributes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindText(const SearchStr: string; sStartPos, sLength: Integer): Integer;
    procedure SetSelection(StartPos, EndPos: Longint; ScrollCaret: Boolean = False);
    function GetSelection: TCharRange;
    function LoadRichText(path: String): Boolean; overload;
    function LoadRichFromString(AString: String): Boolean;
    procedure SaveSelection(st: TStream);

    property Paragraph: TRxParaAttributes read FParagraph;
  end;

  { TfrxRichView }

  TfrxRichView = class(TfrxStretcheable)
  private
    FAllowExpressions: Boolean;
    FExpressionDelimiters: String;
    FFlowTo: TfrxRichView;
    FGapX: Extended;
    FGapY: Extended;
    FParaBreak: Boolean;
    FRichMemo: TRxRichMemo;
    FTempStream: TMemoryStream;
    FTempStream1: TMemoryStream;
    FWysiwyg: Boolean;
    FHasNextDataPart: Boolean;
    FStopSplit: Boolean;
    FLastChar: Integer;
    FFileLink: String;

    procedure RichDraw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended);
    function IsExprDelimitersStored: Boolean;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    //function UsePrinterCanvas: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AComponent:TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;
    procedure AfterPrint; override;
    procedure BeforePrint; override;
    procedure GetData; override;
    procedure InitPart; override;
    function CalcHeight: Extended; override;
    function DrawPart: Extended; override;
    function GetComponentText: String; override;
    function HasNextDataPart(aFreeSpace: Extended): Boolean; override;
    property RichMemo: TRxRichMemo read FRichMemo write FRichMemo;
  published
    property AllowExpressions: Boolean read FAllowExpressions write FAllowExpressions default True;
    property ExpressionDelimiters: String read FExpressionDelimiters write FExpressionDelimiters stored IsExprDelimitersStored;
    property FlowTo: TfrxRichView read FFlowTo write FFlowTo;
    property FileLink: String read FFileLink write FFileLink;
    property GapX: Extended read FGapX write FGapX;
    property GapY: Extended read FGapY write FGapY;
    property Wysiwyg: Boolean read FWysiwyg write FWysiwyg default True;
    property FillType;
    property Fill;
    property Frame;
    property BrushStyle;
    property Color;
    property Cursor;
    property DataField;
    property DataSet;
    property DataSetName;
    property TagStr;
    property URL;
  end;

procedure frxAssignRich(RichFrom, RichTo: TRichMemo);

implementation

uses
  frxLazRichRTTI, Forms, Controls, Printers, frxUtils, frxPrinter, frxDsgnIntf,
  frxRichEditorLaz, frxPlatformServices;

var
  SupForm: TForm;

{ TRxParaAttributes }

constructor TRxParaAttributes.Create(AOwner: TRxRichMemo);
begin
  inherited Create;
  RichMemo := AOwner;
end;

function TRxParaAttributes.GetAlignment: TParaAlignment;
begin
  RichMemo.GetParaAlignment(RichMemo.SelStart);
end;

function TRxParaAttributes.GetFirstIndent: Longint;
begin
  RichMemo.GetParaMetric(RichMemo.SelStart, ParaMetric);
  Result := Round(ParaMetric.FirstLine);
end;

function TRxParaAttributes.GetLeftIndent: Longint;
begin
  RichMemo.GetParaMetric(RichMemo.SelStart, ParaMetric);
  Result := Round(ParaMetric.HeadIndent);
end;

function TRxParaAttributes.GetRightIndent: Longint;
begin
  RichMemo.GetParaMetric(RichMemo.SelStart, ParaMetric);
  Result := Round(ParaMetric.TailIndent);
end;

function TRxParaAttributes.GetNumbering: TParaNumStyle;
begin
  RichMemo.GetParaNumbering(RichMemo.SelStart, ParaNumbering);
  Result := ParaNumbering.Style;
end;

procedure TRxParaAttributes.SetAlignment(Value: TParaAlignment);
begin
  RichMemo.SetParaAlignment(RichMemo.SelStart, RichMemo.SelLength, Value);
end;

procedure TRxParaAttributes.SetFirstIndent(Value: Longint);
begin
  ParaMetric.FirstLine := Value;
  RichMemo.SetRangeParaParams(RichMemo.SelStart, RichMemo.SelLength, [pmm_FirstLine], ParaMetric);
end;

procedure TRxParaAttributes.SetLeftIndent(Value: Longint);
begin
  ParaMetric.HeadIndent := Value;
  RichMemo.SetRangeParaParams(RichMemo.SelStart, RichMemo.SelLength, [pmm_HeadIndent], ParaMetric);
end;

procedure TRxParaAttributes.SetRightIndent(Value: Longint);
begin
  ParaMetric.TailIndent := Value;
  RichMemo.SetRangeParaParams(RichMemo.SelStart, RichMemo.SelLength, [pmm_TailIndent], ParaMetric);
end;

procedure TRxParaAttributes.SetNumbering(Value: TParaNumStyle);
begin
  if Value <> pnNone then
  begin
    Value := pnBullet;
    ParaNumbering.Indent := 10;
  end
  else
    ParaNumbering.Indent := 0;
  ParaNumbering.Style := Value;
  RichMemo.SetParaNumbering(RichMemo.SelStart, RichMemo.SelLength, ParaNumbering);
end;

{ TRxRichMemo }

constructor TRxRichMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParagraph := TRxParaAttributes.Create(Self);
end;

destructor TRxRichMemo.Destroy;
begin
  inherited;
  FParagraph.Free;
end;

function TRxRichMemo.FindText(const SearchStr: string; sStartPos, sLength: Integer): Integer;
begin
  if sLength = -1 then
    sLength := MaxInt;
  Result := Search(SearchStr, sStartPos, sLength, []);
  SetSelection(Result, Length(SearchStr));
end;

procedure TRxRichMemo.SetSelection(StartPos, EndPos: Longint; ScrollCaret: Boolean = False);
begin
  SelStart := StartPos;
  SelLength := EndPos;
end;

function TRxRichMemo.GetSelection: TCharRange;
begin
  Result.cpMin := SelStart;
  Result.cpMax := SelLength;
end;

function TRxRichMemo.LoadRichText(path: String): Boolean;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(path, fmOpenRead);
  Self.LoadRichText(FS);
  FS.Free;
end;

function TRxRichMemo.LoadRichFromString(AString: String): Boolean;
var
  AStream: TStream;
begin
  AStream := TStringStream.Create(AString);
  Self.LoadRichText(AStream);
  AStream.Free;
end;

procedure TRxRichMemo.SaveSelection(st: TStream);
var
  selst, sellen: Integer;
  bufStream: TStream;
begin
  selst := Self.SelStart;
  sellen := Self.SelLength;
  bufStream := TMemoryStream.Create;
  Self.SaveRichText(bufStream);
  Self.SelStart := 0;
  Self.SelLength := selst;
  Self.ClearSelection;
  Self.SaveRichText(st);
  bufStream.Position:=0;
  Self.LoadRichText(bufStream);
  bufStream.Free;
  Self.SetSelection(selst, sellen);
end;

{ TfrxRichView }

const
  GapX = 2;
  GapY = 1;

procedure TfrxRichView.RichDraw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended);
var
  Bmp: TBitmap;
  bRect1, bRect2: TRect;
{$IFDEF Windows}
  PrinterHandle: THandle;
  aScaleX, aScaleY: Extended;

  procedure PaintRich(rec: TRect; can: TCanvas);
  var
    Range: TFormatRange;
  begin
    FillChar(Range, SizeOf(TFormatRange), 0);

    Range.rc := rec;
    Range.rcPage := rec;
    Range.hdc := can.Handle;
    Range.hdcTarget := can.Handle;
    Range.chrg.cpMax := -1;
    Range.chrg.cpMin := 0;

    SendMessage(FRichMemo.Handle, EM_FORMATRANGE, 1, frxInteger(@Range));
    SendMessage(FRichMemo.Handle, EM_FORMATRANGE, 0, 0);
  end;
{$ENDIF}
  function CreateBitMap(bWidth, bHeight: Integer): TBitmap;
  begin
    Result := TBitmap.Create;
    Result.Width := bWidth;
    Result.Height := bHeight;
    Result.Canvas.Brush.Color := clNone;
    Result.Canvas.FillRect(Result.Canvas.ClipRect);
  end;
begin
  bRect2 := Rect(FX, FY, FX1, FY1);
  FRichMemo.Color := Self.Color;
  {$IFDEF Windows}
  if IsPrinter(Canvas) then
    PrinterHandle := Canvas.Handle
  else
    PrinterHandle := GetDC(0);

  GetDisplayScale(PrinterHandle, IsPrinter(Canvas), aScaleX, aScaleY);

  if IsPrinter(Canvas) or ((ScaleX = 1) and (ScaleY = 1)) then
  begin
    bRect1 := Rect(Round((FX + GapX) * 1440 / 96 / aScaleX), Round((FY + GapY) * 1440 / 96 / aScaleY),
      Round((FX1 - GapX) * 1440 / 96 / aScaleX), Round((FY1 - GapY) * 1440 / 96 / aScaleY));
    PaintRich(bRect1, Canvas);
  end
  else
  begin
    Bmp := CreateBitMap(Round(Width), Round(Height));

    PaintRich(Rect(0, 0, Round(Bmp.Width * 1440 / 96), Round(Bmp.Height * 1440 / 96)), Bmp.Canvas);

    Canvas.CopyRect(bRect2, Bmp.Canvas, Bmp.Canvas.ClipRect);
    Bmp.Free;
  end;
  if not IsPrinter(Canvas) then
    ReleaseDC(0, PrinterHandle);
  {$ELSE}
  Bmp := CreateBitMap(Round(Width * ScaleX), Round(Height * ScaleY));

  FRichMemo.Width := bRect2.Width;
  FRichMemo.Height := bRect2.Height;
  FRichMemo.ZoomFactor := ScaleX;
  FRichMemo.BorderStyle := bsNone;
  FRichMemo.SelStart := 0;
  FRichMemo.SelLength := 0;
  SupForm.Show;
  SupForm.Visible := False;

  frxPaintWidget(TWinControl(FRichMemo), Bmp.Canvas);
  Canvas.Lock;
  Canvas.Draw(FX, FY, Bmp);
  Canvas.Unlock;
  Bmp.Free;
  {$ENDIF}
end;

function TfrxRichView.IsExprDelimitersStored: Boolean;
begin
  Result := FExpressionDelimiters <> '[,]';
end;

{function TfrxRichView.UsePrinterCanvas: Boolean;
begin
  Result := frxPrinters.HasPhysicalPrinters and FWysiwyg;
end;}

procedure TfrxRichView.ReadData(Stream: TStream);
begin
  FRichMemo.LoadRichText(Stream);
end;

procedure TfrxRichView.WriteData(Stream: TStream);
begin
  FRichMemo.SaveRichText(Stream);
end;

procedure TfrxRichView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('RichEdit', ReadData, WriteData, True);
end;

procedure TfrxRichView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFlowTo) then
    FFlowTo := nil;
end;

constructor TfrxRichView.Create(AComponent:TComponent);
begin
  inherited Create(AComponent);
  FRichMemo := TRxRichMemo.Create(SupForm);
  FRichMemo.Parent := SupForm;
  SendMessage(SupForm.Handle, WM_CREATEHANDLE, frxInteger(FRichMemo), 0);

  FTempStream := TMemoryStream.Create;
  FTempStream1 := TMemoryStream.Create;

  FAllowExpressions := True;
  FExpressionDelimiters := '[,]';
  FGapX := 2;
  FGapY := 1;
  FWysiwyg := True;
  FHasNextDataPart := True;
  FLastChar := 0;
end;

destructor TfrxRichView.Destroy;
begin
  SendMessage(SupForm.Handle, WM_DESTROYHANDLE, frxInteger(FRichMemo), 0);
  FRichMemo.Free;
  FTempStream.Free;
  FTempStream1.Free;
  inherited;
end;

class function TfrxRichView.GetDescription: String;
begin
  Result := frxResources.Get('obRich');
end;

procedure TfrxRichView.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended);
begin
  if Height < 0 then Height := Height * (-1);
  BeginDraw(Canvas, ScaleX, ScaleY, OffsetX, OffsetY);
  DrawBackground;
  RichDraw(Canvas, ScaleX, ScaleY, OffsetX, OffsetY);

  if not FObjAsMetafile then
    DrawFrame;
end;

procedure TfrxRichView.BeforePrint;
begin
  inherited;
  FTempStream.Position := 0;
  FRichMemo.SaveRichText(FTempStream);
end;

procedure TfrxRichView.AfterPrint;
begin
  FTempStream.Position := 0;
  FRichMemo.LoadRichText(FTempStream);
  inherited;
end;

procedure TfrxRichView.GetData;
const
  RTFHeader = '{\rtf';
  URTFHeader = '{urtf';
type
  tag_settextex = record
    flags: DWORD;
    codepage: UINT;
  end;
var
  ss: TStringStream;
  i, j, TextLen, sStart, sLen: Integer;
  s1, s2, dc1, dc2: String;
  SetText: tag_settextex;
  AnsiStr: AnsiString;

  function GetSpecial(const s: String; Pos: Integer): Integer;
  {$IFNDEF FPC}
  var
    i: Integer;
  {$ENDIF}
  begin
    Result := 0;
    {$IFNDEF FPC}//May be needed in the future
    for i := 1 to Pos do
      if CharInSet(s[i], [#10, #13]) then
        Inc(Result);
    {$ENDIF}
  end;
{$IFDEF Win8}
  { this function search expression broken by new line}
  { [                                                 }
  {  IIF(1>2,'oh',                                    }
  {  'OK')                                            }
  {  ]                                                }
  { EM_FINDTEXTEX coldn't search text with new line   }
  function SearchForText(const s: String; StartPos: Integer): Boolean;
  var
    i, sPos, sLen, SelStart, SelEnd: Integer;
    sText: String;
    Sel: TCharRange;
  begin
    sLen := Length(s);
    SelStart := -1;
    sPos := 1;
    for i := 1 to sLen do
      if CharInSet(s[i], [#10, #13]) then
      begin
        if (sPos = i) and (i <> 2) then
        begin
          sPos := i + 1;
          continue;
        end;
        sText := Copy(s, sPos, i - sPos);
        Result := (FRichMemo.FindText(sText, StartPos - 1 - GetSpecial(FRichMemo.Text, StartPos) div 2, -1) > 0);
        if not Result then Exit;

        Sel := FRichMemo.GetSelection;
        if sPos = 1 then
          SelStart := Sel.cpMin;
        Inc(StartPos, Sel.cpMax - Sel.cpMin);
        sPos := i + 1;
      end;
    if SelStart = -1 then
      Result := (FRichMemo.FindText(s, StartPos - 1 - GetSpecial(FRichMemo.Text, StartPos) div 2, -1) >= 0)
    else
    begin
      Result := (FRichMemo.FindText(Copy(s, sPos, Length(s) - (sPos - 1)), StartPos - 1 - GetSpecial(FRichMemo.Text, StartPos) div 2, -1) > 0);
      Sel := FRichMemo.GetSelection;
      SelEnd := Sel.cpMax;
      FRichMemo.SetSelection(SelStart, SelEnd, False);
    end;
  end;
{$ENDIF}
begin
  inherited;
  if FFileLink <> '' then
  begin
    s1 := FFileLink;
    if Pos('[', s1) <> 0 then
      ExpandVariables(s1);
    if FileExists(s1) then
      FRichMemo.LoadRichText(s1);
  end
  else if IsDataField then
  begin
    if DataSet.IsBlobField(DataField) then
    begin
      ss := TStringStream.Create('');
      DataSet.AssignBlobTo(DataField, ss)
    end
    else
      ss := TStringStream.Create(VarToStr(DataSet.Value[DataField]));
    try
      FRichMemo.LoadRichText(ss);
    finally
      ss.Free;
    end;
  end;

  if FAllowExpressions then
  begin
    dc1 := FExpressionDelimiters;
    dc2 := Copy(dc1, Pos(',', dc1) + 1, 255);
    dc1 := Copy(dc1, 1, Pos(',', dc1) - 1);
    TextLen := 0;
    with FRichMemo do
    try
      Lines.BeginUpdate;
      i := frxPos(dc1, Text);
      while i > 0 do
      begin
        s1 := frxGetBrackedVariableW(Text, dc1, dc2, i, j);
       {$IFDEF Win8}
        // win8.1 old detection of expression doesn't work anymore
        // search it by using control, maybe litle slower , but works
        // temprary solution - TODO search expressions in RAW rtf
        if not SearchForText(dc1 + s1 + dc2, i) then
          raise Exception.Create('Could not search for expression in RichText.');
       {$ELSE}
        SelStart := i - 1 - GetSpecial(Text, i) div 2;
       {$ENDIF}

        s2 := VarToStr(Report.Calc(s1));
       {$IFDEF Win8}
        i := i + frxLength(s2);
       {$ELSE}
          SelLength := j - i + 1;
          TextLen := frxLength(s2);
       {$ENDIF}

        if (Copy(s2, 1, 5) = RTFHeader) or (Copy(s2, 1, 6) = URTFHeader) then
        begin
          LoadRichFromString(s2);
          //TextLen := Length(Text); //for AllowExpressions rtf in DB
        end
        else
          SelText := s2;
       {$IFDEF Win8}
        i := frxPosEx(dc1, Text, i)
       {$ELSE}
        i := frxPosEx(dc1, Text, i + TextLen);
       {$ENDIF}
      end;
    finally
      Lines.EndUpdate;
    end;
  end;
  {$IFNDEF Linux}
  if FFlowTo <> nil then
  begin
    InitPart;
    DrawPart;
    FTempStream1.Position := 0;
    FlowTo.RichMemo.LoadRichText(FTempStream1);
    FFlowTo.AllowExpressions := False;
  end;
  {$ENDIF}
end;

procedure TfrxRichView.InitPart;
begin
  FTempStream1.Clear;
  FRichMemo.SaveRichText(FTempStream1);
  FParaBreak := False;
  FHasNextDataPart := True;
  FStopSplit := False;
end;

function TfrxRichView.CalcHeight: Extended;
var
  Range: TFormatRange;
  chrgRange: TCharRange;
  rcBottom: Integer;
begin
  Result := 0;
  {$IFNDEF Linux}
  FillChar(Range, SizeOf(TFormatRange), 0);
  with Range do
  begin
    rc := Rect(0, 0, Round((Width - GapX * 2) * 1440 / 96),
      Round(1000000 * 1440.0 / 96));
    rcPage := rc;

    hdc := GetDC(0);
    hdcTarget := hdc;
    chrgRange.cpMax := FRichMemo.GetTextLen;

    chrg.cpMax := -1;
    chrg.cpMin := 0;
    rcBottom := 0;
    { process all pages in RichEdit, maybe it contain /PAGE tag }
    // FRichMemo.Perform(EM_SETSEL, -1, -1);
    repeat
      rc := rcPage;
      chrg.cpMin := SendMessage(FRichMemo.Handle, EM_FORMATRANGE, 0, frxInteger(@Range));
      rcBottom := rcBottom + rc.Bottom;
    until (chrg.cpMin >= chrgRange.cpMax);

    if chrgRange.cpMax = 0 then
      Result := 0
    else
      Result := Result + Round(rcBottom / (1440.0 / 96)) + 2 * GapY + 2;

    ReleaseDC(0, hdc);
  end;
  SendMessage(FRichMemo.Handle, EM_FORMATRANGE, 0, 0);
  {$ELSE}
  RichMemo.AutoSize := True;
  SupForm.Show;
  SupForm.Visible := False;
  Result := RichMemo.Height;
  {$ENDIF}
end;

function TfrxRichView.DrawPart: Extended;
var
  Range: TFormatRange;
  LastChar: Integer;
begin
  if (Round((Height - GapY * 2)) <= 0)then
  begin
    Result := Height;
    FHasNextDataPart := True;
    Exit;
  end;

  FTempStream1.Position := 0;
  FRichMemo.LoadRichText(FTempStream1);
  if FParaBreak then
    with FRichMemo.Paragraph do
    begin
      FirstIndent := FirstIndent + LeftIndent;
      FRichMemo.Paragraph.LeftIndent := 0;
    end;

  FillChar(Range, SizeOf(TFormatRange), 0);

  with Range do
  begin
    rc := Rect(0, 0, Round((Width - GapX * 2) * 1440 / 96),
      Round((Height - GapY * 2) * 1440 / 96));
    rcPage := rc;
    hdc := GetDC(0);
    hdcTarget := hdc;
    chrg.cpMin := 0;
    chrg.cpMax := -1;

    FRichMemo.SetSelection(-1, -1);
    LastChar := SendMessage(FRichMemo.Handle, EM_FORMATRANGE, 0, frxInteger(@Range));
    SendMessage(FRichMemo.Handle, EM_FORMATRANGE, 0, 0);
    ReleaseDC(0, hdc);

    Result := GapY * 2 + 2;
  end;

  try
    if (Result < 0) then
    begin
      Result := Height;
      if FLastChar = LastChar then
      begin
        FHasNextDataPart := False;
        FStopSplit := True;
      end;
      exit;
    end;
  finally
    FLastChar := LastChar;
  end;

  try
    if LastChar > 1 then
    begin
      FRichMemo.SelStart := LastChar - 1;
      FRichMemo.SelLength := 1;
      FParaBreak := FRichMemo.SelText <> #13;
    end;

    FRichMemo.SelStart := LastChar;
    FRichMemo.SelLength := RichMemo.GetTextLen - LastChar + 1;

    if FRichMemo.SelLength <= 1 then
    begin
      Result := 0;
      FHasNextDataPart := False;
    end
    else
      FHasNextDataPart := True;

    FTempStream1.Clear;
    if FHasNextDataPart then
      FRichMemo.SaveSelection(FTempStream1);
    FRichMemo.SelText := '';
  finally
    FRichMemo.SetSelection(-1, -1);
  end;
end;

function TfrxRichView.GetComponentText: String;
begin
  if PlainText then
  begin
    FTempStream.Clear;
    FRichMemo.Lines.SaveToStream(FTempStream);
    SetLength(Result, FTempStream.Size);
    FTempStream.Position := 0;
    FTempStream.Read(Result[1], FTempStream.Size);
  end
  else
  begin
    FTempStream.Clear;
    FRichMemo.SaveRichText(FTempStream);
    SetLength(Result, FTempStream.Size);
    FTempStream.Position := 0;
    FTempStream.Read(Result[1], FTempStream.Size);
  end;
end;

function TfrxRichView.HasNextDataPart(aFreeSpace: Extended): Boolean;
begin
  Result := FHasNextDataPart and (StretchMode <> smDontStretch) or
    (Inherited HasNextDataPart(aFreeSpace) and not FStopSplit {and (StretchMode = smDontStretch)});
end;

procedure frxAssignRich(RichFrom, RichTo: TRichMemo);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  try
    RichFrom.SaveRichText(st);
    st.Position := 0;
    RichTo.LoadRichText(st);
  finally
    st.Free;
  end;
end;

initialization
  frxObjects.RegisterObject1(TfrxRichView, nil, '', '', 0, 26);
  SupForm := TForm.CreateNew(nil);

finalization
  frxObjects.UnRegister(TfrxRichView);
  SupForm.Free;

end.
