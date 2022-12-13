unit frxPlatformServices;

{$I frx.inc}

interface

uses
  Classes, SysUtils, StrUtils, frxUtils
{$IFDEF FPC}
  , lazutf8
{$ENDIF};

type
  // from frxClass
  {$IFDEF DELPHI16}
    frxInteger = NativeInt;
  {$ELSE}
    frxInteger = {$IFDEF FPC}PtrInt{$ELSE}Integer{$ENDIF};
  {$ENDIF}

const
  {$IFDEF NONWINFPC}
  LineBreak = #$0A;
  {$ELSE}
  LineBreak = #$0D#$0A;
  {$ENDIF}

function frxLength(s: string): frxInteger;{$IFNDEF FPC}overload;{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}
function frxPos(const substr, s: string): frxInteger;{$IFNDEF FPC}{$IFNDEF DELPHI12}overload;{$ENDIF}{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}
//frxPosEx function has been tested only on Lazarus.(not used in Delphi yet)
function frxPosEx(const substr, s: string; StartPos: frxInteger = 1): frxInteger;{$IFNDEF FPC}{$IFNDEF DELPHI12}overload;{$ENDIF}{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}
function frxCopy(const s: string; Index, Count: frxInteger): string; {$IFNDEF FPC}{$IFNDEF DELPHI12}overload;{$ENDIF}{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}
function frxUpperCase(const s: string): string; {$IFNDEF FPC}overload;{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}
function frxGetSymbol(const str: String; Index: frxInteger): {$IFDEF FPC}String{$ELSE}Char{$ENDIF}; {$IFNDEF FPC}overload;{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}

procedure frxInsert(const source: string; var s: string; Index: frxInteger); {$IFNDEF FPC}overload;{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}
procedure frxDelete(var s: string; Index, Count: frxInteger);{$IFNDEF FPC}overload;{$ENDIF}{$IFDEF Delphi9} inline;{$ENDIF}

{$IFNDEF FPC}
{$IFNDEF DELPHI12}
function frxPos(const substr, s: Widestring): frxInteger; overload;{$IFDEF Delphi9} inline;{$ENDIF}
function frxPosEx(const substr, s: Widestring; StartPos: frxInteger = 1): frxInteger; overload;{$IFDEF Delphi9} inline;{$ENDIF}
function frxCopy(const s: Widestring; Index, Count: frxInteger): Widestring; overload;{$IFDEF Delphi9} inline;{$ENDIF}
{$ENDIF}
function frxUpperCase(const s: Widestring): Widestring; overload;{$IFDEF Delphi9} inline;{$ENDIF}
function frxLength(s: Widestring): frxInteger; overload;{$IFDEF Delphi9} inline;{$ENDIF}
function frxGetSymbol(const str: WideString; Index: frxInteger): WideChar; overload;{$IFDEF Delphi9} inline;{$ENDIF}
procedure frxInsert(const source: Widestring; var s: Widestring; Index: frxInteger); overload;{$IFDEF Delphi9} inline;{$ENDIF}
procedure frxDelete(var s: WideString; Index, Count: frxInteger); overload;{$IFDEF Delphi9} inline;{$ENDIF}
{$ENDIF}


implementation

function frxLength(s: String): frxInteger;
begin
{$IFDEF FPC}
   Result := UTF8Length(s);
{$ELSE}
   Result := Length(s);
{$ENDIF}
end;

function frxPos(const substr, s: String): frxInteger;
begin
  {$IFDEF FPC}
    Result := UTF8Pos(substr, s);
  {$ELSE}
    Result := Pos(substr, s);
  {$ENDIF}
end;

function frxPosEx(const substr, s: string; StartPos: frxInteger = 1): frxInteger;
begin
  {$IFDEF FPC}
    Result := UTF8Pos(substr, s, StartPos);
  {$ELSE}
    Result := PosEx(substr, s, StartPos);
  {$ENDIF}
end;

procedure frxDelete(var s: String; Index, Count: frxInteger);
begin
  {$IFDEF FPC}
   UTF8Delete(s, Index, Count);
  {$ELSE}
   Delete(s, Index, Count);
  {$ENDIF}
end;

function frxCopy(const s: String; Index, Count: frxInteger): String;
begin
  {$IFDEF FPC}
  Result := UTF8Copy(s, Index, Count);
  {$ELSE}
  Result := Copy(s, Index, Count);
  {$ENDIF}
end;

procedure frxInsert(const Source: String; var s: String; Index: frxInteger);
begin
  {$IFDEF FPC}
  UTF8Insert(Source,s, Index);
  {$ELSE}
  Insert(Source, s, Index);
  {$ENDIF}
end;

function frxUpperCase(const s: String): String;
begin
  {$IFDEF FPC}
  Result := UTF8UpperCase(s);
  {$ELSE}
  Result := AnsiUpperCase(s);
  {$ENDIF}
end;

function frxGetSymbol(const str: String; Index: frxInteger): {$IFDEF FPC}String{$ELSE}Char{$ENDIF};
begin
  {$IFDEF FPC}
  Result := frxCopy(str, Index, 1);
  {$ELSE}
  Result := str[Index];
  {$ENDIF}
end;

{$IFNDEF FPC}
{$IFNDEF DELPHI12}
function frxPos(const substr, s: WideString): frxInteger;
begin
  Result := Pos(substr, s);
end;

function frxPosEx(const substr, s: Widestring; StartPos: frxInteger = 1): frxInteger;
begin
  Result := PosEx(substr, s, StartPos);
end;

function frxCopy(const s: WideString; Index, Count: frxInteger): WideString;
begin
  Result := Copy(s, Index, Count);
end;
{$ENDIF}

function frxUpperCase(const s: WideString): WideString;
begin
  Result := WideUpperCase(s);
end;

function frxLength(s: WideString): frxInteger; overload;
begin
  Result := Length(s);
end;

procedure frxInsert(const Source: WideString; var s: WideString; Index: frxInteger);
begin
  Insert(Source, s, Index);
end;

procedure frxDelete(var s: WideString; Index, Count: frxInteger);
begin
  Delete(s, Index, Count);
end;

function frxGetSymbol(const str: WideString; Index: frxInteger): WideChar;
begin
  Result := str[Index];
end;
{$ENDIF}
end.

