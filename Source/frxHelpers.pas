
{******************************************}
{                                          }
{             FastReport VCL               }
{                Helpers                   }
{                                          }
{         Copyright (c) 1998-2022          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxHelpers;

{$I frx.inc}

interface

uses
  Classes, SysUtils,
  frxXML;

type
  TOwnObjList = class(TList)
  private
    FOwnsObjects: Boolean;
  protected
    function GetItem(Index: Integer): TObject;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    procedure Clear; override;
    procedure AddNonZero(Item: Pointer);
    procedure Assign(SourceList: TList);

    property Items[Index: Integer]: TObject read GetItem; default;
  end;

  TValueDlm = (vdUnknown, vdApostrophe, vdQuote);

  TfrxValuedXMLReader = class(TfrxXMLReader)
  protected
    function IsLastSlash(const InSt: String): Boolean;
    function IsFirstSlash(const InSt: String): Boolean;
    function IsFirstNumberSign(const InSt: String): Boolean;
    procedure ReadValuedItem(out {$IFDEF Delphi12}NameS, ValueS{$ELSE}Name, Value{$ENDIF}, Text: String); virtual;
    procedure ProcessSecondLeftBrocket; virtual; abstract;
    function CreateItem: TfrxXMLItem; virtual;
    procedure AddChar(var st: AnsiString; var stPos: Integer; ch: AnsiChar);
  public
    function IsReadValuedXMLRootItem(Item: TfrxXMLItem): Boolean;
    function IsReadValuedXMLItem(Item: TfrxXMLItem): Boolean;
  end;

  TfrxValuedXMLDocument = class(TfrxXMLDocument)
  protected
    FValuedXMLStream: TStream;
    FValuedXMLStreamReader: TfrxValuedXMLReader;

    procedure ReadXMLHeader; virtual; abstract;
    function CreateReader: TfrxValuedXMLReader; virtual; abstract;
  public
    procedure InitValuedXMLFile(const FileName: String);
    procedure DoneValuedXMLFile;
    function IsReadItem(Item: TfrxXMLItem): Boolean;
  end;

  ESignException = class(Exception);

  TLogList = class(TStringList)
  public
    procedure Save(FileName: TFileName);
    procedure SaveAppend(FileName: TFileName);
  end;

function IncPointer(P: Pointer; Offset: Cardinal): Pointer;
function IntToAnsiStr(Value: Integer): AnsiString;
function ROL(const Value: LongWord; const Bits: Byte): LongWord;
function ROL16(const Value: Word; const Bits: Byte): Word;
function ROR16(const Value: Word; const Bits: Byte): Word;
function AnsiToHex(st: AnsiString; UpCase: Boolean = True): AnsiString;
function BufferToHex(Buffer: Pointer; Len: Cardinal; UpCase: Boolean = True): AnsiString;

function ByteSwap(Value: Cardinal): Cardinal; overload;
function ByteSwap(Value: Integer): Cardinal; overload;
function ByteSwap(Value: Int64): Int64; overload;

function AnsiStringOfChar(ACh: AnsiChar; Count: Integer): AnsiString;
function ToStringList(const S: string; const Delimiter: Char): TStrings;
function NormalizeScientificNotation(const S: string): string;

function Limit(const Value, MinValue, MaxValue: Single): Single; overload;
function Limit(const Value, MinValue, MaxValue: Double): Double; overload;
function Limit(const Value, MinValue, MaxValue: Integer): Integer; overload;

function ReadExtended8(Stream: TStream): Extended;
function ReadExtended10(Stream: TStream): Extended;
function ReadExtended16(Stream: TStream): Extended;

function ReadWideStringFromStream(Stream: TStream): WideString;
procedure WriteWideStringToStream(Stream: TStream; const ws: WideString);

implementation

uses
  Math, StrUtils,
  frxUtils;

{ Utility routines }

function ReadWideStringFromStream(Stream: TStream): WideString;
var
  nChars: Cardinal;
begin
  Stream.ReadBuffer(nChars, SizeOf(nChars));
  SetLength(Result, nChars);
  if nChars > 0 then
    Stream.ReadBuffer(Result[1], nChars * SizeOf(Result[1]));
end;

procedure WriteWideStringToStream(Stream: TStream; const ws: WideString);
var
  nChars: Cardinal;
begin
  nChars := Length(ws);
  Stream.WriteBuffer(nChars, SizeOf(nChars));
  if nChars > 0 then
    Stream.WriteBuffer(ws[1], nChars * SizeOf(ws[1]));
end;

function ReadExtended16(Stream: TStream): Extended;
begin
  case SizeOf(Extended) of
    8, 10:
      begin
        Result := ReadExtended10(Stream);
        Stream.Position := Stream.Position + 6;
      end;
    16:
      Stream.ReadBuffer(Result, SizeOf(Result));
  end;
end;

function ReadExtendedViaDouble(Stream: TStream): Double;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function ReadExtended8(Stream: TStream): Extended;
begin
  case SizeOf(Extended) of
    8:
      Stream.ReadBuffer(Result, SizeOf(Result));
    10, 16:
      Result := ReadExtendedViaDouble(Stream);
  end;
end;

// https://stackoverflow.com/questions/2943660/delphi-extended-to-c-sharp
function ConvertDelphiExtended(Stream: TStream): Extended;
const
  extendedSize = 10;
var
  buf: array [0 .. extendedSize - 1] of Byte;
  bf: Int64 absolute buf[0];
  sign, integral, mantissa, value: Extended;
  exp: word;
  iexp: Integer;
  fractal: Int64;
begin
  Stream.Read(buf[0], extendedSize);
  sign := IfReal(buf[extendedSize - 1] and $80 = $80, -1, 1);
  buf[extendedSize - 1] := (buf[extendedSize - 1] and $7F);
  Move(buf[8], exp, 2);
  exp := word(buf[extendedSize - 1]) shl 8 + buf[extendedSize - 2];
  integral := IfReal(buf[extendedSize - 3] and $80 = $80, 1, 0);
  // Calculate mantissa
  mantissa := 0.0;
  value := 1.0;
  fractal := bf;

  while fractal <> 0 do
  begin
    value := value / 2;
    if fractal and $4000000000000000 = $4000000000000000 then // Latest bit is sign, just skip it
      mantissa := mantissa + value;
    fractal := fractal shl 1;
  end;

  iexp := Integer(exp) - 16383;
  if ((iexp < -307) or (iexp > 307)) and (SizeOf(Extended) = 8) then
    Result := 0
  else
    Result := sign * Power(2, iexp) * (integral + mantissa);
end;

function ReadExtended10(Stream: TStream): Extended;
begin
  case SizeOf(Extended) of
    10:
      Stream.ReadBuffer(Result, SizeOf(Result));
    8, 16:
      Result := ConvertDelphiExtended(Stream);
  end;
end;

function Limit(const Value, MinValue, MaxValue: Double): Double; overload;
begin
  Result := Max(MinValue, Min(MaxValue, Value));
end;

function Limit(const Value, MinValue, MaxValue: Single): Single;
begin
  Result := Max(MinValue, Min(MaxValue, Value));
end;

function Limit(const Value, MinValue, MaxValue: Integer): Integer;
begin
  Result := Max(MinValue, Min(MaxValue, Value));
end;

function NormalizeScientificNotation(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, 'e-', 'e<', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'e+', 'e>', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '-', ' -', [rfReplaceAll]);
  Result := StringReplace(Result, '+', ' +', [rfReplaceAll]);
  Result := StringReplace(Result, 'e<', 'e-', [rfReplaceAll]);
  Result := StringReplace(Result, 'e>', 'e+', [rfReplaceAll]);
end;

function ToStringList(const S: string; const Delimiter: Char): TStrings;
var
  C: Integer;
begin
  Result := TStringList.Create;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := S;

  for C := Result.Count - 1 downto 0 do
  begin
    Result[C] := Trim(Result[C]);
    if Result[C] = '' then
      Result.Delete(C);
  end;
end;

function AnsiStringOfChar(ACh: AnsiChar; Count: Integer): AnsiString;
begin
  Result := AnsiString(StringOfChar(ACh, Count));
end;

function ByteSwap(Value: Integer): Cardinal; overload;
begin
  Result := ByteSwap(Cardinal(Value));
end;

function ByteSwap(Value: Cardinal): Cardinal; overload;
begin
  Result := ((Value and $FF) shl 24) or ((Value and $FF00) shl 8) or
    ((Value and $FF0000) shr 8) or ((Value and $FF000000) shr 24);
end;

function ByteSwap(Value: Int64): Int64; overload;
begin
  Result := (Int64(ByteSwap(LongWord(Value and $FFFFFFFF))) shl 32) or
    ByteSwap(LongWord(Value shr 32));
end;

function BufferToHex(Buffer: Pointer; Len: Cardinal; UpCase: Boolean = True): AnsiString;
const
  HEX: array [Boolean, 0..15] of ANSIChar = (
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'),
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
  );
var
  i: Integer;
  b: PByte;
begin
  SetLength(Result, Len * 2);
  b := Buffer;
  for i := 0 to Len - 1 do
  begin
    Result[i * 2 + 1] := HEX[UpCase, b^ shr 4];
    Result[i * 2 + 2] := HEX[UpCase, b^ and $F];
    Inc(b);
  end;
end;

function AnsiToHex(st: AnsiString; UpCase: Boolean = True): AnsiString;
begin
  Result := BufferToHex(@st[1], Length(st), UpCase);
end;

function ROR16(const Value: Word; const Bits: Byte): Word;
begin
  Result := (Value shr Bits) or (Value shl (16 - Bits));
end;

function ROL16(const Value: Word; const Bits: Byte): Word;
begin
  Result := (Value shl Bits) or (Value shr (16 - Bits));
end;

function ROL(const Value: LongWord; const Bits: Byte): LongWord;
begin
  Result := (Value shl Bits) or (Value shr (32 - Bits));
end;

function IntToAnsiStr(Value: Integer): AnsiString;
begin
  Result := AnsiString(IntToStr(Value));
end;

function IncPointer(P: Pointer; Offset: Cardinal): Pointer;
begin
  Result := Pointer({$ifdef win64}UInt64{$else}Cardinal{$endif}(P) + Offset);
end;

{ TOwnObjList }

procedure TOwnObjList.AddNonZero(Item: Pointer);
begin
  if Item <> nil then
    Add(Item);
end;

procedure TOwnObjList.Assign(SourceList: TList);
begin
  inherited Assign(SourceList);
  if SourceList is TOwnObjList then
    FOwnsObjects := TOwnObjList(SourceList).FOwnsObjects;
end;

procedure TOwnObjList.Clear;
var
  i: Integer;
begin
  if FOwnsObjects then
    for i := 0 to Count - 1 do
      Items[i].Free;
  inherited Clear;
end;

constructor TOwnObjList.Create(AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function TOwnObjList.GetItem(Index: Integer): TObject;
begin
  Result := inherited Items[Index]; // Pointer => TObject
end;

{ TfrxValuedXMLReader }

procedure TfrxValuedXMLReader.AddChar(var st: AnsiString; var stPos: Integer; ch: AnsiChar);
begin
  Inc(stPos);
  if stPos > Length(st) then
    SetLength(st, Length(st) * 2);
  st[stPos] := ch;
end;

function TfrxValuedXMLReader.CreateItem: TfrxXMLItem;
begin
  Result := TfrxXMLItem.Create;
end;

function TfrxValuedXMLReader.IsFirstNumberSign(const InSt: String): Boolean;
begin
  Result := (InSt <> '') and (InSt[1] = '#');
end;

function TfrxValuedXMLReader.IsFirstSlash(const InSt: String): Boolean;
begin
  Result := (InSt <> '') and (InSt[1] = '/');
end;

function TfrxValuedXMLReader.IsLastSlash(const InSt: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(InSt);
  Result := (Len > 0) and (InSt[Len] = '/');
end;

function TfrxValuedXMLReader.IsReadValuedXMLItem(Item: TfrxXMLItem): Boolean;
var
  ChildItem: TfrxXMLItem;
begin
  Result := IsReadValuedXMLRootItem(Item);
  if not Result or IsLastSlash(Item.Text) or IsFirstNumberSign(Item.Name) or
     IsLastSlash(Item.Name) or (Item.Name = '![CDATA[') then
    Exit;

  repeat
    ChildItem := CreateItem;
    if IsReadValuedXMLItem(ChildItem) then
      Item.AddItem(ChildItem)
    else
    begin
      if IsFirstSlash(ChildItem.Name) then
        Item.Value := ChildItem.Value;
      ChildItem.Free;
      Break;
    end;
  until False;
end;

function TfrxValuedXMLReader.IsReadValuedXMLRootItem(Item: TfrxXMLItem): Boolean;

  procedure ApostropheToQuote(var Text: string);
  var
    First, Second, i: Integer;
  begin
    Second := 0;
    repeat
      First := PosEx('=''', Text, Second + 1);
      if First = 0 then
        Break;
      Second := PosEx('''', Text, First + 2);
      if Second = 0 then
        Break;
      Text[First + 1] := '"';
      Text[Second] := '"';
      for i := First + 2 to Second - 1 do
        if Text[i] = '"' then
          Text[i] := '''';
    until False;
  end;

  function IsSkip(Name: String): Boolean;
  begin
    Result := (Name = '?xml') or (Name = '!--') or (Name = '!DOCTYPE');
  end;
var
  Name, Text, Value, SumValue : String;
begin
  SumValue := '';
  repeat
    ReadValuedItem(Name, Value, Text);
    SumValue := SumValue + IfStr(SumValue <> '', ' ') + Trim(Value);
  until EndOfStream or ((Name + Text <> '') and not IsSkip(Name));

  ApostropheToQuote(Text);
  Item.SetData(Name, Text, SumValue);
  Result := not IsFirstSlash(Name) and (Name + Text <> '');
end;

procedure TfrxValuedXMLReader.ReadValuedItem(out {$IFDEF Delphi12}NameS, ValueS{$ELSE}Name, Value{$ENDIF}, Text: String);
const
  LenPiece = 512;

var
  c: Byte;
  curposName, curPosValue: Integer;
  i, SpacePos, LineEndPos: Integer;
{$IFDEF Delphi12}
  Name, Value: AnsiString;
{$ENDIF}
begin
  if EndOfStream then
    Exit;
  c := 0;
  Text := '';
  curposName := 0;
  SetLength(Name, LenPiece);

  curposValue := 0;
  SetLength(Value, LenPiece);

  while not EndOfStream do
  begin
    c := ReadFromBuffer;
    if c = Ord('<') then
      Break
    else
      AddChar(Value, curposValue, AnsiChar(Chr(c)));
  end;
  SetLength(Value, curposValue);

  while not EndOfStream do
  begin
    c := ReadFromBuffer;
    if      c = Ord('<') then
      ProcessSecondLeftBrocket
    else if c = Ord('>') then
      Break
    else
    begin
      AddChar(Name, curposName, AnsiChar(Chr(c)));
      if (curposName = 3) and (Copy(Name, 1, 3) = '!--') then // Comment
        AddChar(Name, curposName, AnsiChar(' '));
    end;
  end;
  SetLength(Name, curposName);

  if c <> Ord('>') then
    if EndOfStream then
    begin
      {$IFDEF Delphi12}
      NameS := String(Name);
      ValueS := UTF8Decode(Copy(Value, 1, curposValue));
      {$ENDIF}
      Exit;
    end
    else
      RaiseException;

  SpacePos := Pos(AnsiString(' '), Name);
  LineEndPos := Pos(AnsiString(#$D#$A), Name);
  i := Min(SpacePos, LineEndPos);
  if i = 0 then
    i := Max(SpacePos, LineEndPos);

  if i <> 0 then
  begin
    Text := {$IFDEF Delphi12} UTF8Decode(Copy(Name, i + 1, curposName - i));
            {$ELSE}           Copy(Name, i + 1, curposName - i);
            {$ENDIF}
    SetLength(Name, i - 1);
  end;

{$IFDEF Delphi12}
  NameS := String(Name);
  ValueS := UTF8Decode(Copy(Value, 1, curposValue));
{$ENDIF}
end;

{ TfrxValuedXMLDocument }

procedure TfrxValuedXMLDocument.DoneValuedXMLFile;
begin
  FValuedXMLStreamReader.Free;
  FTempStream := FValuedXMLStream;
end;

procedure TfrxValuedXMLDocument.InitValuedXMLFile(const FileName: String);
begin
  DeleteTempFile;
  FValuedXMLStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

  FValuedXMLStreamReader := CreateReader;

  Root.Clear;
  Root.Offset := 0;

  ReadXMLHeader;

  FValuedXMLStreamReader.IsReadValuedXMLRootItem(Root);
end;

function TfrxValuedXMLDocument.IsReadItem(Item: TfrxXMLItem): Boolean;
begin
  Result := FValuedXMLStreamReader.IsReadValuedXMLItem(Item);
end;

{ TLogList }

procedure TLogList.Save(FileName: TFileName);
begin
  SaveToFile(FileName);
end;

procedure TLogList.SaveAppend(FileName: TFileName);
var
  AFile: TextFile;
begin
  if FileExists(FileName) then
  begin
    AssignFile(AFile, Filename);
    System.Append(AFile);
    try
      Writeln(AFile, Self.Text);
    finally
      CloseFile(AFile);
    end;
  end
  else
    Save(FileName);
end;

{ Test ConvertDelphiExtended }

type
  TExt10 = array[0..9] of byte;
  TTestExt10 = record
    Ext10: TExt10;
    Value: Double;
  end;

const
  TestExt10Array: array [0 .. 5] of TTestExt10 = (
    (Ext10: ($00, $00, $00, $00, $00, $00, $00, $80, $ff, $3f); Value: 1.0),
    (Ext10: ($00, $50, $AA, $7D, $3A, $1E, $33, $D3, $01, $40); Value: 6.59999),
    (Ext10: ($00, $00, $00, $00, $00, $00, $A0, $8C, $0B, $40); Value: 4500),
    (Ext10: ($00, $D0, $F7, $53, $E3, $A5, $9B, $C4, $F7, $3F); Value: 0.006),
    (Ext10: ($00, $A0, $70, $3D, $0A, $D7, $A3, $B0, $FD, $3F); Value: 0.345),
    (Ext10: ($00, $68, $66, $66, $66, $66, $66, $A2, $02, $40); Value: 10.15)
  );

procedure TestConvertDelphiExtended;
var
  i: Integer;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    for i := Low(TestExt10Array) to High(TestExt10Array) do
    begin
      Stream.Position := 0;
      Stream.WriteBuffer(TestExt10Array[i].Ext10, SizeOf(TExt10));
      Stream.Position := 0;
      if ConvertDelphiExtended(Stream) <> TestExt10Array[i].Value then
        raise Exception.Create('Error ConvertDelphiExtended N ' + IntToStr(i));
    end;
  finally
    Stream.Free;
  end;
end;

initialization
//  TestConvertDelphiExtended;

end.

