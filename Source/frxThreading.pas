
{******************************************}
{                                          }
{             FastReport VCL               }
{        GUI Thread Synchonization         }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxThreading;

interface

{$I frx.inc}

uses
{$IFNDEF FPC}
  Windows, Messages,
{$ENDIF}
{$IFDEF FPC}
  LResources, LCLType, LazHelper, LMessages,
{$ENDIF}
  SysUtils, Classes;

{$IFNDEF FPC}
const
  WM_FRX_SYNC_THREAD = WM_USER + 250;
  WM_FRX_SYNC_MESSAGE = WM_USER + 251;

type
  TfrxThreadSynchronizer = class
  private
    FIsMain: Boolean;
    FWindowHandle: HWND;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    property WindowHandle: HWND read FWindowHandle;
  end;

  TfrxGuiThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  function GetThreadSynchronizer: TfrxThreadSynchronizer;
  function IsThreadSynchronizerActive: Boolean;
  function frxSynchSendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
{$ENDIF}
  procedure frxThreadSynchronize(Method: TThreadMethod);

{$IFNDEF FPC}
var
  frxDisableThreadSynchronizer: Boolean;
{$ENDIF}

implementation
{$IFNDEF FPC}
uses Forms, frxClass;

var
  frxThreadSynchronizer: TfrxThreadSynchronizer;
  frxGuiThread: TfrxGuiThread;
  ThreadRunEvent: THandle;

type
  PTThreadMethod = ^TThreadMethod;


procedure InitSynchronizer;
begin
  if Assigned(frxThreadSynchronizer) and (not frxThreadSynchronizer.FIsMain) or
    (Application.Handle <> 0) or not IsLibrary then Exit;

  if (Application.Handle = 0) and (frxGUIThreadID <> GetCurrentThreadID) then
  begin
    FreeAndNil(frxThreadSynchronizer);
    frxGuiThread := TfrxGuiThread.Create;
    ThreadRunEvent := CreateEvent(nil, true, false, 'FRX_GUI_THREAD_R');
    frxGuiThread.Resume;
    WaitForSingleObject(ThreadRunEvent, 100000);
    CloseHandle(ThreadRunEvent);
  end;
end;

{ TfrxThreadSynchronizer }

{$WARNINGS OFF}
constructor TfrxThreadSynchronizer.Create;
begin
  FWindowHandle := AllocateHWnd(WndProc);
end;

destructor TfrxThreadSynchronizer.Destroy;
begin
  DeallocateHWnd(FWindowHandle);
end;
{$WARNINGS ON}
procedure TfrxThreadSynchronizer.WndProc(var Message: TMessage);
var
  lMessage: PMessage;
begin
  if Message.Msg = WM_FRX_SYNC_THREAD then
  begin
    if Message.WParam <> 0 then
      PTThreadMethod(Message.WParam)^();
  end
  else if Message.Msg = WM_FRX_SYNC_MESSAGE then
  begin
    lMessage :=  PMessage(Message.WParam);
    lMessage^.Result := SendMessage(Message.LParam, lMessage^.Msg, lMessage^.WParam, lMessage^.LParam);
  end

  else
    Message.Result := DefWindowProc(FWindowHandle, Message.Msg, Message.wParam, Message.lParam);
end;

function GetThreadSynchronizer: TfrxThreadSynchronizer;
begin
  Result := frxThreadSynchronizer;
end;

function IsThreadSynchronizerActive: Boolean;
begin
  Result := Assigned(frxThreadSynchronizer);
end;

function frxSynchSendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  lMessage: TMessage;
begin
  lMessage.Msg := Msg;
  lMessage.WParam := wParam;
  lMessage.LParam := lParam;
  lMessage.Result := 0;
  Result := 0;
  if Assigned(frxThreadSynchronizer) then
    Result := SendMessage(frxThreadSynchronizer.WindowHandle, WM_FRX_SYNC_MESSAGE, NativeInt(@lMessage), NativeInt(hWnd));
end;

procedure frxThreadSynchronize(Method: TThreadMethod);
begin
  if frxDisableThreadSynchronizer and Assigned(Method) then
  begin
    Method;
    Exit;
  end;
  InitSynchronizer;
  if not Assigned(Method) then Exit;

  if Assigned(frxThreadSynchronizer) and (frxThreadSynchronizer.WindowHandle <> 0) then
    SendMessage(frxThreadSynchronizer.WindowHandle, WM_FRX_SYNC_THREAD, NativeInt(@TMethod(Method)), 0)
  else if (Application.Handle = 0) or (frxGUIThreadID = GetCurrentThreadID) then
    Method
  else
    TThread.Synchronize(nil, Method);
end;

{ TTestThread }

constructor TfrxGuiThread.Create;
begin
  inherited Create(True);
end;

destructor TfrxGuiThread.Destroy;
begin
  Terminate;
  if Assigned(frxThreadSynchronizer) then
    SendMessage(frxThreadSynchronizer.FWindowHandle, WM_QUIT, 0, 0);
  inherited;
end;

procedure TfrxGuiThread.Execute;
var
  Msg: TMsg;
  ThSynch: TfrxThreadSynchronizer;
  IsUnicode, IsMsgExists: Boolean;
begin

  ThSynch := TfrxThreadSynchronizer.Create;
{$IFDEF DELPHI16}
  InterlockedExchangePointer(Pointer(frxThreadSynchronizer), ThSynch);
{$ELSE}
  InterlockedExchange(frxInteger(frxThreadSynchronizer), frxInteger(ThSynch));
{$ENDIF}

{$IFDEF MSWINDOWS}
  InterlockedExchange(Integer(frxGUIThreadID), GetCurrentThreadID);
{$ENDIF}
{$IFDEF POSIX}
  InterlockedExchange64(Int64(frxGUIThreadID), GetCurrentThreadID);
{$ENDIF}

  SetEvent(ThreadRunEvent);
  while not Terminated and (ThSynch.FWindowHandle <> 0) do
  begin
    if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE)  then
    begin
      IsUnicode := (Msg.hwnd = 0) or IsWindowUnicode(Msg.hwnd);
      if IsUnicode then
        IsMsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
      else
        IsMsgExists := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);

      if IsMsgExists then
      begin
        if Msg.Message = WM_QUIT then
          Break
        else
        begin
          TranslateMessage(Msg);
          if IsUnicode then
            DispatchMessageW(Msg)
          else
            DispatchMessageA(Msg);
        end;
      end
    end
    else if not Terminated then
      WaitMessage;
  end;

{$IFDEF DELPHI16}
  InterlockedExchangePointer(Pointer(frxThreadSynchronizer), nil);
{$ELSE}
  InterlockedExchange(frxInteger(frxThreadSynchronizer), 0);
{$ENDIF}
  FreeAndNil(ThSynch);
end;

initialization
  frxDisableThreadSynchronizer := False;
  if IsLibrary then
  begin
    frxThreadSynchronizer := TfrxThreadSynchronizer.Create;
    frxThreadSynchronizer.FIsMain := True;
  end;
  frxGuiThread := nil;

finalization
  FreeAndNil(frxGuiThread);
  FreeAndNil(frxThreadSynchronizer);
{$ELSE}
 procedure frxThreadSynchronize(Method: TThreadMethod); inline;
 begin
   Method;
 end;
{$ENDIF}
end.

