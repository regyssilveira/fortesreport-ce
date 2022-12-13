
{******************************************}
{                                          }
{             FastReport VCL               }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

{ it used to reduce precompiler directives }
{ some interfaces was introduces only in lastest Delphi }
unit frxDPIAwareInt;

interface

{$I frx.inc}

uses
  {$IFNDEF FPC}Windows, Messages, {$ENDIF}
  SysUtils, Classes
{$IFDEF FPC}
  , LazarusPackageIntf, LazHelper
{$ENDIF};

const
  FRX_WM_DPICHANGED = $02E0;
  FRX_WM_DPICHANGED_AFTERPARENT = $02E3;

type
  DPI_AWARENESS_CONTEXT_ = record
    unused_: Integer;
  end;

  IfrxDPIAwareControl = interface ['{391B5E86-16E3-4935-B2F9-5EFC2A47071B}']
    procedure DoPPIChanged(aNewPPI: Integer);
  end;

  FRX_DPI_AWARENESS_CONTEXT = ^DPI_AWARENESS_CONTEXT_;
{$IFNDEF FPC}
  TfrxGetDpiForWindow = function(hwnd: HWND): UINT; stdcall;
  TfrxSetProcessDpiAwarenessContext = function(value: FRX_DPI_AWARENESS_CONTEXT): BOOL; stdcall;
  TfrxSetThreadDpiAwarenessContext = function(value: FRX_DPI_AWARENESS_CONTEXT): BOOL; stdcall;
  TfrxGetThreadDpiAwarenessContext = function(): FRX_DPI_AWARENESS_CONTEXT; stdcall;
{$ENDIF}

const
  FRX_DPI_AWARENESS_CONTEXT_UNAWARE: FRX_DPI_AWARENESS_CONTEXT = FRX_DPI_AWARENESS_CONTEXT(-1);
  FRX_DPI_AWARENESS_CONTEXT_SYSTEM_AWARE: FRX_DPI_AWARENESS_CONTEXT = FRX_DPI_AWARENESS_CONTEXT(-2);
  FRX_DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE: FRX_DPI_AWARENESS_CONTEXT = FRX_DPI_AWARENESS_CONTEXT(-3);
  FRX_DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2: FRX_DPI_AWARENESS_CONTEXT = FRX_DPI_AWARENESS_CONTEXT(-4);
  FRX_DPI_AWARENESS_CONTEXT_UNAWARE_GDISCALED: FRX_DPI_AWARENESS_CONTEXT = FRX_DPI_AWARENESS_CONTEXT(-5);


function frxGetDpiForWindow(aHwnd: NativeInt): UINT;
function frxSetProcessDpiAwarenessContext(value: FRX_DPI_AWARENESS_CONTEXT): LongBool;
function frxSetThreadDpiAwarenessContext(value: FRX_DPI_AWARENESS_CONTEXT): LongBool;
function frxGetThreadDpiAwarenessContext: FRX_DPI_AWARENESS_CONTEXT;


implementation

uses Forms;


{$IFNDEF FPC}
var
  FLibHandle: THandle;
  FGetDpiForWindow: TfrxGetDpiForWindow;
  FSetProcessDpiAwarenessContext: TfrxSetProcessDpiAwarenessContext;
  FGetThreadDpiAwarenessContext: TfrxGetThreadDpiAwarenessContext;
  FSetThreadDpiAwarenessContext: TfrxSetThreadDpiAwarenessContext;
{$ENDIF}

function frxGetDpiForWindow(aHwnd: NativeInt): UINT;
begin
{$IFNDEF FPC}
  if Assigned(FGetDpiForWindow) then
    Result := FGetDpiForWindow(aHwnd)
  else
{$ENDIF}
    Result := Screen.PixelsPerInch;
end;

function frxSetProcessDpiAwarenessContext(value: FRX_DPI_AWARENESS_CONTEXT): LongBool;
begin
  Result := False;
{$IFNDEF FPC}
  if Assigned(FSetProcessDpiAwarenessContext) then
    Result := FSetProcessDpiAwarenessContext(value);
{$ENDIF}
end;


function frxSetThreadDpiAwarenessContext(value: FRX_DPI_AWARENESS_CONTEXT): LongBool;
begin
  Result := False;
{$IFNDEF FPC}
  if Assigned(FSetThreadDpiAwarenessContext) then
    Result := FSetThreadDpiAwarenessContext(value);
{$ENDIF}
end;

function frxGetThreadDpiAwarenessContext: FRX_DPI_AWARENESS_CONTEXT;
begin
    Result := FRX_DPI_AWARENESS_CONTEXT_UNAWARE;
{$IFNDEF FPC}
  if Assigned(FGetThreadDpiAwarenessContext) then
    Result := FGetThreadDpiAwarenessContext();
{$ENDIF}
end;

{$IFNDEF FPC}
initialization
  FLibHandle := LoadLibrary('user32.dll');
  FGetDpiForWindow := nil;
  FSetProcessDpiAwarenessContext := nil;
  if FLibHandle <> 0 then
  begin
    FGetDpiForWindow := TfrxGetDpiForWindow(GetProcAddress(FLibHandle, 'GetDpiForWindow'));
    FSetProcessDpiAwarenessContext := TfrxSetProcessDpiAwarenessContext(GetProcAddress(FLibHandle, 'SetProcessDpiAwarenessContext'));
    FGetThreadDpiAwarenessContext := TfrxGetThreadDpiAwarenessContext(GetProcAddress(FLibHandle, 'GetThreadDpiAwarenessContext'));
    FSetThreadDpiAwarenessContext := TfrxSetThreadDpiAwarenessContext(GetProcAddress(FLibHandle, 'SetThreadDpiAwarenessContext'));
  end;

finalization
  FGetDpiForWindow := nil;
  FSetProcessDpiAwarenessContext := nil;
  FGetThreadDpiAwarenessContext := nil;
  FSetThreadDpiAwarenessContext := nil;
{$ENDIF}
end.
