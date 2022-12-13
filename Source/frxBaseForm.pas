
{******************************************}
{                                          }
{             FastReport VCL               }
{              Tool controls               }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

/// <summary>
///   This unit contains classes of the base dialog form used for every form in
///   FastReport. This form handles save and load state, HIDPI messages,
///   localization messages.
/// </summary>
unit frxBaseForm;

interface

{$I frx.inc}

uses
{$IFNDEF FPC}Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, IniFiles, frxDPIAwareInt
{$IFDEF FPC}
  , LResources, LCLType, LMessages, LCLIntf, LCLProc, LazarusPackageIntf, LazHelper
{$ENDIF}
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  /// <summary>
  ///   This enumeration defines action of preference state. It used to
  ///   save/load/restore form state.
  /// </summary>
  TfrxPreferencesAction = (
    /// <summary>
    ///   Loads preference setting for the form.
    /// </summary>
    frPaLoad,
    /// <summary>
    ///   Saves preference setting for the form. <br />
    /// </summary>
    frPaSave,
    /// <summary>
    ///   Reset preference setting for the form to default. <br />
    /// </summary>
    frPaRestore);
  /// <summary>
  ///   Type of the form settings which should be processed by current form.
  /// </summary>
  TfrxPreferencesType = (
    /// <summary>
    ///   Position of the form.
    /// </summary>
    frPtFormPos,
    /// <summary>
    ///   Form size. <br />
    /// </summary>
    frPtFormSize,
    /// <summary>
    ///   Form visibility at start. <br />
    /// </summary>
    frPtFormVisibility,
    /// <summary>
    ///   Dock information of the form.
    /// </summary>
    frPtFormDockInfo,
    /// <summary>
    ///   Dock position of the form.
    /// </summary>
    frPtFormDockPos,
    /// <summary>
    ///   Dock size of the form.
    /// </summary>
    frPtFormDockSize,
    /// <summary>
    ///   Custom settings(used for controls state).
    /// </summary>
    frPtFormCustom);
  /// <summary>
  ///   Set of preference types.
  /// </summary>
  TfrxPreferencesTypes = set of TfrxPreferencesType;
  /// <summary>
  ///   Defines when form should apply saved preference settings.
  /// </summary>
  TfrxPreferencesLoadEvent = (
    /// <summary>
    ///   At form creation.
    /// </summary>
    peAtCreate,
    /// <summary>
    ///   When form shows. Only at first show.
    /// </summary>
    peAtShowOnce,
    /// <summary>
    ///   When form shows(applies every time form shows).
    /// </summary>
    peAtShow);

    TfrxShortcutAction = (saCopy, saPaste, saCut, saSelectAll, saUndo, saRedo);

  /// <summary>
  ///   This class used to pass messages of the form. internal use only.
  /// </summary>
  TfrxMessageObject = class(TObject)
  public
    Msg: Cardinal;
    WParam: WPARAM;
    FormRect: TRect;
  end;

  /// <summary>
  ///   The base class for dialog forms used in FastReport. This class handles
  ///   save and load state, HIDPI messages,localization messages.
  /// </summary>
  TfrxBaseForm = class(TForm)
  private
    FCurrentFormPPI: Integer;
    FShowed: Boolean;
    FSavedPPI: Integer;
    FNeedUpdatePPI: Boolean;
    FUpdatingPPI: TfrxMessageObject;
    FPreferences: TfrxPreferencesTypes;
    FIsPPIChanging: Boolean;
    FPrefIsLoaded: Boolean;
    FPrefIsSaved: Boolean;
    FPrefIsLocked: Boolean;
    procedure WMDpiChanged(var Message: TMessage); message FRX_WM_DPICHANGED;
    function GetCurrentFormPPI: Integer;
  protected
    FPrefEvent: TfrxPreferencesLoadEvent;
    FHostedControls: array of TControl;
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure ChangeScale(M, D: Integer); overload; override;
{$IFDEF DELPHI24}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); overload; override;
{$ENDIF}
    procedure BeforePPIChange; virtual;
    procedure AfterPPIChange; virtual;
    procedure AfterPPIMessage; virtual;
    procedure DoUpdateFormPPI(aNewPPI: Integer);
    { FormShowBeforeLoad and FormShowAfterLoad to prevent form flikering on load }
    procedure FormShowBeforeLoad; virtual;
    procedure FormShowAfterLoad; virtual;
    procedure LoadFormPreferences(PreferencesStorage: TObject; DefPreferencesStorage: TObject); virtual;
    procedure SaveFormPreferences(PreferencesStorage: TObject; DefPreferencesStorage: TObject); virtual;
    procedure ResetFormPreferences(PreferencesStorage: TObject); virtual;
    function GetFormSectionName: String; virtual;
    function GetAvailablePreferences: TfrxPreferencesTypes; virtual;
    procedure TranslateControlsByTag(AControl: TControl);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   This method should return reference to an object used to save
    ///   preference. By default it's a registry or an ini file object. The top
    ///   most window should mage this object. In FastReport tthe report
    ///   designer and the report preview return this object.
    /// </summary>
    function GetPreferencesStorage(aDefault: Boolean): TObject; virtual;
    /// <summary>
    ///   This method used to process preference action(load/save/restore) with
    ///   default type for current form.
    /// </summary>
    /// <param name="aPrefArction">
    ///   Preference action type.
    /// </param>
    procedure ProcessPreferences(aPrefArction: TfrxPreferencesAction); overload;
    /// <summary>
    ///   This method used to process preference action(load/save/restore) with
    ///   non-default type which passed as a parameter.
    /// </summary>
    /// <param name="aPrefArction">
    ///   Preference action type.
    /// </param>
    /// <param name="PrefTyp">
    ///   Preference types.
    /// </param>
    procedure ProcessPreferences(aPrefArction: TfrxPreferencesAction; PrefTyp: TfrxPreferencesTypes); overload;
    /// <summary>
    ///   Loads form settings from storage object with using preference type
    ///   parameter.
    /// </summary>
    /// <param name="PreferencesStorage">
    ///   Reference to a storage object.
    /// </param>
    /// <param name="DefPreferencesStorage">
    ///   Reference to a storage object with default settings.
    /// </param>
    /// <param name="PrefTyp">
    ///   Settings type.
    /// </param>
    procedure LoadFormPrefType(PreferencesStorage: TObject; DefPreferencesStorage: TObject; PrefTyp: TfrxPreferencesTypes);
    /// <summary>
    ///   Called when window receive message for localization. Updates form
    ///   resource. Each form should override this method for custom resources.
    /// </summary>
    procedure UpdateResouces; virtual;
    /// <summary>
    ///   Moves controls from current form to host control. Each form should
    ///   fill the list with control it can move by filling FHostedControls
    ///   array.
    /// </summary>
    /// <param name="Host">
    ///   Host control for form objects.
    /// </param>
    procedure HostControls(Host: TWinControl); virtual;
    /// <summary>
    ///   Returns back all moved by HostControls function controls back to
    ///   owner form and pass modal result to the form.
    /// </summary>
    /// <param name="AModalResult">
    ///   Modal result of an action.
    /// </param>
    procedure UnhostControls(AModalResult: TModalResult); virtual;
    /// <summary>
    ///   Form calls this method when receives messages that current PPI was
    ///   changed. If Form has controls that doesn't support automatic scaling,
    ///   those controls should be corrected in this method.
    /// </summary>
    /// <param name="aNewPPI">
    ///   New PPI value.
    /// </param>
    procedure UpdateFormPPI(aNewPPI: Integer); virtual;
    /// <summary>
    ///   Returns current PPI value of the form.
    /// </summary>
    property CurrentFormPPI: Integer read GetCurrentFormPPI;
    /// <summary>
    ///   Sends PPI message to a form or a control and all child dock forms.
    /// </summary>
    /// <param name="aWinControl">
    ///   Control that should receive message.
    /// </param>
    /// <param name="aNewPPI">
    ///   New PPI value.
    /// </param>
    procedure SendPPIMessage(aWinControl:TWinControl; aNewPPI: Integer);
    /// <summary>
    ///   Saves form settings to storage object with using preference type
    ///   parameter.
    /// </summary>
    /// <param name="PreferencesStorage">
    ///   Reference to a storage object.
    /// </param>
    /// <param name="DefPreferencesStorage">
    ///   Reference to a storage object with default settings.
    /// </param>
    /// <param name="PrefTyp">
    ///   Settings type.
    /// </param>
    procedure SaveFormPrefType(PreferencesStorage: TObject; DefPreferencesStorage: TObject; PrefTyp: TfrxPreferencesTypes);
{$IFDEF DELPHI24}
    procedure ScaleForPPI(NewPPI: Integer); override;
{$ENDIF}
    property IsPPIChanging: Boolean read FIsPPIChanging;
  end;

  /// <summary>
  ///   The base class for the forms which should save position and sized by
  ///   default to setting storage(registry).
  /// </summary>
  TfrxBaseLoadSavePrefForm = class(TfrxBaseForm)
  protected
    function GetAvailablePreferences: TfrxPreferencesTypes; override;
  end;

implementation


uses frxClass, frxUtils, frxRes, frxDock, Types;

const
  rsForm      = 'Form5';
  rsToolBar   = 'ToolBar5';
  rsDock      = 'Dock5';
  rsWidth     = 'Width';
  rsHeight    = 'Height';
  rsTop       = 'Top';
  rsLeft      = 'Left';
  rsFloat     = 'Float';
  rsVisible   = 'Visible';
  rsMaximized = 'Maximized';
  rsData      = 'Data';
  rsSize      = 'Size';

{ TfrxBaseForm }

{$IFDEF DELPHI24}
procedure TfrxBaseForm.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if M <> D then
  begin
    FNeedUpdatePPI := True;
    DoUpdateFormPPI(D);
  end;
end;
{$ENDIF}

procedure TfrxBaseForm.AfterConstruction;
begin
  inherited;
  //Position := poDefault;
  BeforePPIChange;
  DoUpdateFormPPI(Screen.PixelsPerInch);
  AfterPPIChange;
  if FPrefEvent = peAtCreate then
    ProcessPreferences(frPaLoad);
  UpdateResouces;
end;

procedure TfrxBaseForm.AfterPPIChange;
begin
  FIsPPIChanging := False;
end;

procedure TfrxBaseForm.AfterPPIMessage;
begin

end;

procedure TfrxBaseForm.BeforeDestruction;
begin
  inherited;
  if (FPrefEvent in [peAtCreate, peAtShowOnce])  and not FPrefIsSaved then
    ProcessPreferences(frPaSave);
  Setlength(FHostedControls, 0);
end;

procedure TfrxBaseForm.BeforePPIChange;
begin
  FIsPPIChanging := True;
end;

procedure TfrxBaseForm.ChangeScale(M, D: Integer);
begin
  inherited;
  if M <> D then
  begin
    FNeedUpdatePPI := True;
    DoUpdateFormPPI(M);
  end;
end;

constructor TfrxBaseForm.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentFormPPI := 0;
  FNeedUpdatePPI := True;
  FPreferences := GetAvailablePreferences;
  FPrefEvent := peAtShowOnce;
end;

procedure TfrxBaseForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  if (FPrefEvent = peAtShow) or ((FPrefEvent = peAtShowOnce) and not FPrefIsSaved) then
    ProcessPreferences(frPaSave);
end;

procedure TfrxBaseForm.DoShow;
var
  NewPPI: Integer;
  aRect: TRect;
begin
{$IFDEF FPC}
  if csDesigning in ComponentState then
     Exit;
{$ENDIF}
  FormShowBeforeLoad;
  if (FPrefEvent = peAtShow) or ((FPrefEvent = peAtShowOnce) and not FPrefIsLoaded) then
    ProcessPreferences(frPaLoad);
//  UpdateResouces;
  if FNeedUpdatePPI and (FSavedPPI = 0) then
    DoUpdateFormPPI(CurrentFormPPI);
  FormShowAfterLoad;
  inherited;
  FShowed := True;
  if (FSavedPPI > 0) then
  begin
    NewPPI := FSavedPPI;
    aRect := Rect(Self.Left, Self.Top, Self.Left + Round(Self.Width * NewPPI / Screen.PixelsPerInch), Self.Top + Round(Self.Height * NewPPI / Screen.PixelsPerInch));
    FSavedPPI := 0;
    SendMessage(Self.Handle, FRX_WM_DPICHANGED, MakeWParam(NewPPI, NewPPI), Nativeint(@aRect));
  end;
end;

procedure TfrxBaseForm.DoUpdateFormPPI(aNewPPI: Integer);
begin
  if FNeedUpdatePPI then
  begin
    FNeedUpdatePPI := False;
//{$IFDEF FPC}
//    aNewPPI := frx_DefaultPPI;
//{$ENDIF}
    UpdateFormPPI(aNewPPI);
    FCurrentFormPPI := aNewPPI;
  end;
end;

procedure TfrxBaseForm.FormShowAfterLoad;
begin
//
end;

procedure TfrxBaseForm.FormShowBeforeLoad;
begin

end;

function TfrxBaseForm.GetAvailablePreferences: TfrxPreferencesTypes;
begin
  Result := [];
end;

function TfrxBaseForm.GetCurrentFormPPI: Integer;
begin
  Result := FCurrentFormPPI;
  if Result = 0 then
    Result := Screen.PixelsPerInch;
end;

function TfrxBaseForm.GetFormSectionName: String;
begin
  Result := rsForm + '.' + ClassName;
end;

function TfrxBaseForm.GetPreferencesStorage(aDefault: Boolean): TObject;
begin
  Result := nil;
  { top most windows like designer or preview has access to report Ini file }
  if Assigned(Parent) and (Parent is TfrxBaseForm) then
    Result := TfrxBaseForm(Parent).GetPreferencesStorage(aDefault);
  if (Result = nil) and Assigned(Owner) and (Owner is TfrxBaseForm) then
    Result := TfrxBaseForm(Owner).GetPreferencesStorage(aDefault);
end;

procedure TfrxBaseForm.HostControls(Host: TWinControl);
var
  i: Integer;
begin
  for i := Low(FHostedControls) to High(FHostedControls) do
    FHostedControls[i].Parent := Host;
  ProcessPreferences(frPaLoad);
  FormShowBeforeLoad;
//  UpdateResouces;
  if Assigned(OnShow) then
    OnShow(Self);
end;

function RectInMonitor(aWndRect: TRect): Boolean;
var
  i: Integer;
begin
  Result := False;
  for I := 0 to Screen.MonitorCount - 1 do
    if PtInRect(Screen.Monitors[i].BoundsRect, aWndRect.TopLeft) or
      PtInRect(Screen.Monitors[i].BoundsRect, aWndRect.BottomRight) then
      begin
        Result := True;
        Exit;
      end;
end;


procedure TfrxBaseForm.LoadFormPreferences(PreferencesStorage: TObject; DefPreferencesStorage: TObject);
var
  Ini: TCustomIniFile;
  lName: String;
  lLeft, lTop, lWidth, lHeight: Integer;
  sDock: String;
  cDock: TWinControl;

  procedure ScaleValue(var aVal: Integer; aOriginalVal: Integer);
  begin
    if aOriginalVal <> aVal then
      aVal := Round(aVal * CurrentFormPPI / frx_DefaultPPI)
  end;

begin
  if (PreferencesStorage is TCustomIniFile) then
    Ini := TCustomIniFile(PreferencesStorage)
  else
    Exit;
  lName := GetFormSectionName;
  if FormStyle <> fsMDIChild then
  begin
    if Assigned(DefPreferencesStorage) and not Ini.SectionExists(lName) then
      Ini := TCustomIniFile(DefPreferencesStorage);
    if Ini.ReadBool(lName, rsMaximized, False) then
      WindowState := wsMaximized
    else
    begin
      lLeft := Left;
      lTop := Top;
      lWidth := Width;
      lHeight := Height;
      if frPtFormPos in FPreferences then
      begin
        lLeft := Ini.ReadInteger(lName, rsLeft, lLeft);
        lTop := Ini.ReadInteger(lName, rsTop, lTop);
      end;
      if frPtFormSize in FPreferences then
      begin
        lWidth := Ini.ReadInteger(lName, rsWidth, lWidth);
        lHeight := Ini.ReadInteger(lName, rsHeight, lHeight);
      end;
      if CurrentFormPPI <> Screen.PixelsPerInch then
      begin
        ScaleValue(lLeft, Left);
        ScaleValue(lTop, Top);
      end;
      ScaleValue(lWidth, Width);
      ScaleValue(lHeight, Height);

      if RectInMonitor(Rect(lLeft, lTop, lLeft + lWidth, lTop + lHeight)) then
      begin
        //Position := poDefault;
        SetBounds(lLeft, lTop, lWidth, lHeight);
      end;
    end;
  end;
  if frPtFormDockInfo in FPreferences then
  begin
    sDock := Ini.ReadString(lName, rsDock, '');
    cDock := frxFindComponent(Owner, sDock) as TWinControl;
    if cDock <> nil then
      ManualDock(cDock);
  end;
  if frPtFormVisibility in FPreferences then
    Visible := Ini.ReadBool(lName, rsVisible, True);
end;

procedure TfrxBaseForm.LoadFormPrefType(PreferencesStorage: TObject; DefPreferencesStorage: TObject;
  PrefTyp: TfrxPreferencesTypes);
var
  OldPrefType: TfrxPreferencesTypes;
begin
  OldPrefType := FPreferences;
  try
    FPreferences := PrefTyp;
    FPrefIsLocked := True;
    LoadFormPreferences(PreferencesStorage, DefPreferencesStorage);
  finally
    FPreferences := OldPrefType;
    FPrefIsLocked := False;
  end;
end;

procedure TfrxBaseForm.ProcessPreferences(aPrefArction: TfrxPreferencesAction;
  PrefTyp: TfrxPreferencesTypes);
var
  OldPrefType: TfrxPreferencesTypes;
begin
  OldPrefType := FPreferences;
  try
    FPreferences := PrefTyp;
    ProcessPreferences(aPrefArction);
  finally
    FPreferences := OldPrefType;
  end;
end;

procedure TfrxBaseForm.ProcessPreferences(aPrefArction: TfrxPreferencesAction);
var
  Ini, DefIni: TObject;
begin
  if (FPreferences = []) or FPrefIsLocked then Exit;
  DefIni := GetPreferencesStorage(True);
  Ini := GetPreferencesStorage(False);
  if Ini = nil then Exit;
  try
    FPrefIsLocked := True;
    case aPrefArction of
      frPaLoad:
      begin
        //if Assigned(DefIni) then
        //  LoadFormPreferences(DefIni);
        LoadFormPreferences(Ini, DefIni);
        FPrefIsLoaded := True;
      end;
      frPaSave:
      begin
        try
          SaveFormPreferences(Ini, DefIni);
          FPrefIsSaved := True;
        except
          ResetFormPreferences(Ini);
        end;
      end;
      frPaRestore: ResetFormPreferences(Ini);
    end;
  finally
    FPrefIsLocked := False;
    Ini.Free;
    DefIni.Free;
  end;
end;

procedure TfrxBaseForm.ResetFormPreferences(PreferencesStorage: TObject);
begin

end;

procedure TfrxBaseForm.SaveFormPreferences(PreferencesStorage: TObject; DefPreferencesStorage: TObject);
var
  Ini: TCustomIniFile;
  lName: String;
  lWidth, lHeight: Integer;
begin
  if (PreferencesStorage is TCustomIniFile) then
    Ini := TCustomIniFile(PreferencesStorage)
  else
    Exit;
  lName := GetFormSectionName;
  Ini.WriteInteger(lName, rsLeft, Left);
  Ini.WriteInteger(lName, rsTop, Top);
  lWidth := Round(Width / (CurrentFormPPI / frx_DefaultPPI));
  lHeight := Round(Height / (CurrentFormPPI / frx_DefaultPPI));
  Ini.WriteBool(lName, rsMaximized, WindowState = wsMaximized);
  Ini.WriteBool(lName, rsVisible, Visible);
  if HostDockSite <> nil then
    Ini.WriteString(lName, rsDock, HostDockSite.Name)
  else
    Ini.WriteString(lName, rsDock, '');
  Ini.WriteInteger(lName, rsWidth, lWidth);
  Ini.WriteInteger(lName, rsHeight, lHeight);
end;

procedure TfrxBaseForm.SaveFormPrefType(PreferencesStorage: TObject; DefPreferencesStorage: TObject;
  PrefTyp: TfrxPreferencesTypes);
var
  OldPrefType: TfrxPreferencesTypes;
begin
  OldPrefType := FPreferences;
  try
    FPreferences := PrefTyp;
    SaveFormPreferences(PreferencesStorage, DefPreferencesStorage);
  finally
    FPreferences := OldPrefType;
  end;
end;

{$IFDEF DELPHI24}
procedure TfrxBaseForm.ScaleForPPI(NewPPI: Integer);
begin
  inherited;
  if (NewPPI = FCurrentFormPPI) or (csLoading in ComponentState) then Exit;
  UpdateFormPPI(NewPPI);
  FCurrentFormPPI := NewPPI;
end;
{$ENDIF}

procedure TfrxBaseForm.SendPPIMessage(aWinControl:TWinControl; aNewPPI: Integer);
var
  aRect: TRect;
begin
  if aWinControl is TfrxBaseForm then
    FShowed := True;
  aRect := Rect(aWinControl.Left, aWinControl.Top, aWinControl.Left + Round(aWinControl.Width * aNewPPI / Screen.PixelsPerInch), aWinControl.Top + aWinControl.Height * Round(aNewPPI / Screen.PixelsPerInch));
  SendMessage(aWinControl.Handle, FRX_WM_DPICHANGED, MakeWParam(aNewPPI, aNewPPI), Nativeint(@aRect));
end;

procedure TfrxBaseForm.TranslateControlsByTag(AControl: TControl);

  function GetStr(const Id: string): string;
  begin
    Result := frxResources.Get(Id)
  end;

var
  i: Integer;
begin
  with AControl do
  begin
    if Tag > 0 then
      SetTextBuf(PChar(GetStr(IntToStr(Tag))));
    if AControl is TWinControl then
      with AControl as TWinControl do
        for i := 0 to ControlCount - 1 do
          if Controls[i] is TControl then
            TranslateControlsByTag(Controls[i] as TControl);
  end;
end;

procedure TfrxBaseForm.UnhostControls(AModalResult: TModalResult);
var
  i: Integer;
begin
  ModalResult := AModalResult;
  for i := Low(FHostedControls) to High(FHostedControls) do
    FHostedControls[i].Parent := Self;
  if Assigned(OnHide) then
    OnHide(Self);
end;

procedure TfrxBaseForm.UpdateFormPPI(aNewPPI: Integer);

  procedure SyncControls(AParentControl: TWinControl);
  var
    i: Integer;
    DPIControl: IfrxDPIAwareControl;
  begin
    for i := 0 to AParentControl.ControlCount - 1 do
    begin
      if AParentControl.Controls[i] is TWinControl then
       SyncControls(TWinControl(AParentControl.Controls[i]));
      if Supports(AParentControl.Controls[i], IfrxDPIAwareControl, DPIControl) then
        DPIControl.DoPPIChanged(aNewPPI);
    end;
  end;
begin
  SyncControls(Self);
  frxResources.ImagesPPI := aNewPPI;
end;

procedure TfrxBaseForm.UpdateResouces;
begin

end;

procedure TfrxBaseForm.WMDpiChanged(var Message: TMessage);
//var
//  OldPPI: Integer;
  { look for dock windows }
  procedure DoSendMesssage(aParentControl: TWinControl);
  var
    i: Integer;
    DPIControl: IfrxDPIAwareControl;
  begin
    for i := 0 to aParentControl.ControlCount - 1 do
    begin
      if aParentControl.Controls[i] is TForm then
        SendMessage(TForm(aParentControl.Controls[i]).Handle, Message.Msg, Message.WParam, Message.LParam)
      else if aParentControl.Controls[i] is TWinControl then
        DoSendMesssage(TWinControl(aParentControl.Controls[i]));
      if Supports(aParentControl.Controls[i], IfrxDPIAwareControl, DPIControl) then
        DPIControl.DoPPIChanged(CurrentFormPPI);
    end;
  end;
var
  lRect: TRect;
  lWParam, lPPI: Integer;
  lMsg: Cardinal;
begin
  if Assigned(FUpdatingPPI) then
  begin
    FUpdatingPPI.Msg := Message.Msg;
    FUpdatingPPI.WParam := Message.WParam;
    FUpdatingPPI.FormRect.Top := PRect(Message.LParam)^.Top;
    FUpdatingPPI.FormRect.Left := PRect(Message.LParam)^.Left;
    FUpdatingPPI.FormRect.Bottom := PRect(Message.LParam)^.Bottom;
    FUpdatingPPI.FormRect.Right := PRect(Message.LParam)^.Right;
    Exit;
  end;
  try
    FUpdatingPPI := TfrxMessageObject.Create;
    FUpdatingPPI.Msg := 0;
    lPPI := HiWord(Message.WParam);

    if not FShowed then
    begin
      FSavedPPI := lPPI; // send message later from FormShow
      Exit;
    end;
    if FCurrentFormPPI <> lPPI then
      BeforePPIChange;
    Inherited;
    if (FCurrentFormPPI <> lPPI) and (FSavedPPI = 0) then
    begin
      // OldPPI := FCurrentFormPPI;
      UpdateFormPPI(lPPI);
{$IFNDEF FPC}
{$IFNDEF DELPHI12}
      DisableAlign;
{$IFNDEF Linux}
      if not(Parent is TWinControl) then
       ChangeScale(Message.WParamHi, FCurrentFormPPI);
{$ENDIF}
      EnableAlign;
      Width := PRect(Message.LParam).Right - PRect(Message.LParam).Left;
      Height := PRect(Message.LParam).Bottom - PRect(Message.LParam).Top;
{$ENDIF}
{$ENDIF}
      FCurrentFormPPI := lPPI;
      UpdateResouces;

      DoSendMesssage(Self);
      AfterPPIChange;
      AfterPPIMessage;
    end;

  finally
    lMsg := FUpdatingPPI.Msg;
    lWParam := FUpdatingPPI.WParam;
    lRect.Top := FUpdatingPPI.FormRect.Top;
    lRect.Left := FUpdatingPPI.FormRect.Left;
    lRect.Right := FUpdatingPPI.FormRect.Right;
    lRect.Bottom := FUpdatingPPI.FormRect.Bottom;
    FreeAndNil(FUpdatingPPI);
    if lMsg = FRX_WM_DPICHANGED then
    begin
      SendMessage(Self.Handle, FRX_WM_DPICHANGED, lWParam, Nativeint(@lRect));
    end;

  end;
end;

{ TfrxBaseLoadSavePrefForm }

function TfrxBaseLoadSavePrefForm.GetAvailablePreferences: TfrxPreferencesTypes;
begin
  Result := [frPtFormPos, frPtFormSize, frPtFormCustom];
end;

end.
