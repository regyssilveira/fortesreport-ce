
{******************************************}
{                                          }
{             FastReport VCL               }
{         ComboBox InPlace Editor          }
{                                          }
{         Copyright (c) 1998-2022          }
{           by Fast Reports Inc.           }
{                                          }
{******************************************}

unit frxComboBoxInPlaceEditor;

interface

{$I frx.inc}

uses
  Types,
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLType, LCLIntf, LCLProc,
{$ENDIF}
  Classes, Graphics, frxClass;

implementation

uses
  Controls, SysUtils, Math, StdCtrls, ComCtrls, Buttons, Forms,
  frxComboBox, frxInPlaceEditors, frxPopupForm, frxRes, frxUtils, frxHelpers;

type
  TfrxComboBoxInPlaceEditor = class(TfrxInPlaceBaseChoiceEditor)
  private
    FLastScale: Extended;
    procedure SetScaleOffset(EventParams: TfrxInteractiveEventsParams);
    function ComboBox: TfrxComboBoxView;
  protected
    procedure DoLBClick(Sender: TObject); override;
    function IsFillListBox: Boolean; override;
    function GetButtonSize: Integer; override;
    procedure CalcPopupBounds(out TopLeft: TPoint; out w, h: Integer); override;
    function ListBoxCreate: TListBox; override;
    procedure LBDrawItem(Control: TWinControl; Index: Integer; aRect: TRect;
      State: TOwnerDrawState); override;
  public
    function ShowPopup(aParent: TComponent; aRect: TRect; X, Y: Integer): Boolean; override;
    procedure InitializeUI(var EventParams: TfrxInteractiveEventsParams); override;
    procedure FinalizeUI(var EventParams: TfrxInteractiveEventsParams); override;

    function DoMouseDown(X, Y: Integer; Button: TMouseButton; Shift: TShiftState;
      var EventParams: TfrxInteractiveEventsParams): Boolean; override;
  end;

{ TfrxComboBoxInPlaceEditor }

procedure TfrxComboBoxInPlaceEditor.CalcPopupBounds(out TopLeft: TPoint; out w, h: Integer);
var
  LineCount: Integer;
begin
  TopLeft.X := FRect.Left;
  TopLeft.Y := FRect.Bottom + 1;
  w := FRect.Right - FRect.Left;
  LineCount := Limit(ComboBox.Items.Count, 1, ComboBox.DropDownCount);
  h := LineCount * FListBox.ItemHeight + 3;
end;

function TfrxComboBoxInPlaceEditor.ComboBox: TfrxComboBoxView;
begin
  Result := Component as TfrxComboBoxView;
end;

procedure TfrxComboBoxInPlaceEditor.DoLBClick(Sender: TObject);
begin
  FModified := ComboBox.ItemIndex <> FListBox.ItemIndex;
  if FModified then
    ComboBox.ItemIndex := FListBox.ItemIndex;

  FPopupForm.Hide;
end;

function TfrxComboBoxInPlaceEditor.DoMouseDown(X, Y: Integer;
  Button: TMouseButton; Shift: TShiftState; var EventParams: TfrxInteractiveEventsParams): Boolean;
begin
  SetScaleOffset(EventParams);
  Result := inherited DoMouseDown(X, Y, Button, Shift, EventParams);
end;

procedure TfrxComboBoxInPlaceEditor.FinalizeUI(var EventParams: TfrxInteractiveEventsParams);
begin
  FDrawButton := False;
  ComboBox.NeedReDraw := True;
  EventParams.Refresh := True;
  EventParams.Modified := True;
end;

function TfrxComboBoxInPlaceEditor.GetButtonSize: Integer;
begin
  Result := FRect.Bottom - FRect.Top - 1;
  Result := Result - Result mod 2;
end;

procedure TfrxComboBoxInPlaceEditor.InitializeUI(var EventParams: TfrxInteractiveEventsParams);
begin
  inherited;
  FModified := False;
  FDrawButton := True;
  ComboBox.NeedReDraw := True;
  FDrawDragDrop := False;
  SetScaleOffset(EventParams);
  UpdateRect;
end;

function TfrxComboBoxInPlaceEditor.IsFillListBox: Boolean;
begin
  FListBox.Items.Text := ComboBox.ItemsText;
  FListBox.ItemIndex := ComboBox.ItemIndex;
  Result := True;
end;

procedure TfrxComboBoxInPlaceEditor.LBDrawItem(Control: TWinControl; Index: Integer; aRect: TRect; State: TOwnerDrawState);
begin
  with FListBox do
  begin
    Canvas.FillRect(aRect);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + 1, Items[Index]);
  end;
end;

function TfrxComboBoxInPlaceEditor.ListBoxCreate: TListBox;
begin
  Result := inherited ListBoxCreate;
  Result.Font := Component.Font;
  Result.Font.Height := Round(Result.Font.Height * FLastScale * frx_DefaultPPI / FDevicePPI);
  Result.ItemHeight := Ceil((ComboBox.ContentHeight + 3) * FLastScale);
  Result.Color := IfColor(ComboBox.Color = clNone, clWhite, ComboBox.Color);
end;

procedure TfrxComboBoxInPlaceEditor.SetScaleOffset(EventParams: TfrxInteractiveEventsParams);
begin
  FLastScale := EventParams.Scale;
end;

function TfrxComboBoxInPlaceEditor.ShowPopup(aParent: TComponent; aRect: TRect; X, Y: Integer): Boolean;
begin
  ComboBox.PreviewCoordinates(X, Y);
  Result := inherited ShowPopup(aParent, aRect, X, Y);
end;

initialization
  frxRegEditorsClasses.Register(TfrxComboBoxView, [TfrxComboBoxInPlaceEditor], [[evDesigner, evPreview]]);

finalization
  frxUnregisterEditorsClass(TfrxComboBoxView, TfrxComboBoxInPlaceEditor);
end.
