unit frxRichEditorLaz;

{$I frx.inc}

interface

uses
  {$IFDEF Windows}
  Windows,
  {$ELSE}
  LCLIntf, LCLType,
  {$ENDIF}
  Classes, Messages, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, frxClass, frxDock, frxCtrls, frxCustomEditors, RichMemo,
  frxRichLaz, frxBaseFormEditorDataTree;

type

  { TfrxRichEditor }

  TfrxRichEditor = class(TfrxViewEditor)
  public
    function Edit: Boolean; override;
    function HasEditor: Boolean; override;
    procedure GetMenuItems; override;
    function Execute(Tag: Integer; Checked: Boolean): Boolean; override;
  end;

  { TfrxRichEditorForm }

  TfrxRichEditorForm = class(TfrxBaseFormEditorDataTree)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SpeedBar: TToolBar;
    Ruler: TPanel;
    FontDialog1: TFontDialog;
    FirstInd: TLabel;
    LeftInd: TLabel;
    RulerLine: TBevel;
    RightInd: TLabel;
    BoldB: TToolButton;
    ItalicB: TToolButton;
    LeftAlignB: TToolButton;
    CenterAlignB: TToolButton;
    RightAlignB: TToolButton;
    UnderlineB: TToolButton;
    BulletsB: TToolButton;
    TTB: TToolButton;
    CancelB: TToolButton;
    OkB: TToolButton;
    ExprB: TToolButton;
    FontNameCB: TfrxFontComboBox;
    FontSizeCB: TfrxComboBox;
    OpenB: TToolButton;
    SaveB: TToolButton;
    UndoB: TToolButton;
    Sep1: TToolButton;
    Sep2: TToolButton;
    Sep3: TfrxTBPanel;
    Sep4: TToolButton;
    Sep5: TToolButton;
    BlockAlignB: TToolButton;
    FillColorB: TToolButton;
    FillColorPopupMenu: TPopupMenu;

    procedure FormDestroy(Sender: TObject);
    procedure SelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure SelectFont(Sender: TObject);
    procedure RulerResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BoldBClick(Sender: TObject);
    procedure AlignButtonClick(Sender: TObject);
    procedure FontNameCBChange(Sender: TObject);
    procedure BulletsBClick(Sender: TObject);
    procedure RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RulerItemMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RightIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CancelBClick(Sender: TObject);
    procedure OkBClick(Sender: TObject);
    procedure ExprBClick(Sender: TObject);
    procedure FontSizeCBChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FillColorBClick(Sender: TObject);
    procedure FillColorPopupMenuPopup(Sender: TObject);
    procedure FontSizeCBKeyPress(Sender: TObject; var Key: Char);
  private
    FDragging: Boolean;
    FDragOfs: Integer;
    FFillColor: TColor;
    FUpdating: Boolean;
    FRichView: TfrxRichView;
    RichEdit1: TRxRichMemo;
    LazTabLab: TLabel;

    procedure FontStyleModify(fs: TFontStyle);
    function CurrText: TFontParams;

    procedure SetupRuler;
    procedure SetEditRect;
    procedure OnColorChanged(Sender: TObject);
    procedure ExprMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
  protected
    procedure OnDataTreeDblClick(Sender: TObject); override;
  public
    procedure UpdateResouces; override;
    procedure UpdateFormPPI(aNewPPI: Integer); override;
    property RichView: TfrxRichView read FRichView write FRichView;
  end;

implementation

uses frxDsgnIntf, frxRes, frxDPIAwareInt, frxPopupForm, frxDesgnCtrls,
  frxUtils, frxDesgn, RichMemoUtils;

{$R *.lfm}

const
  RulerAdj = 4/3;
  GutterWid = 6;

{ TfrxRichEditor }

function TfrxRichEditor.Edit: Boolean;
begin
  with TfrxRichEditorForm.Create(Designer) do
  begin
    RichView := TfrxRichView(Component);
    Result := ShowModal = mrOk;
    Free;
  end;
end;

function TfrxRichEditor.HasEditor: Boolean;
begin
  Result := True;
end;

procedure TfrxRichEditor.GetMenuItems;
var
  v: TfrxRichView;
begin
  v := TfrxRichView(Component);

  AddItem(frxResources.Get('mvHyperlink'), 50);
  AddItem('-', -1);
  AddItem(frxResources.Get('mvExpr'), 1, v.AllowExpressions);
  AddItem(frxResources.Get('mvStretch'), 2, v.StretchMode = smActualHeight);
  AddItem(frxResources.Get('mvStretchToMax'), 3, v.StretchMode = smMaxHeight);

  inherited;
end;

function TfrxRichEditor.Execute(Tag: Integer; Checked: Boolean): Boolean;
var
  i: Integer;
  c: TfrxComponent;
  v: TfrxRichView;
begin
  Result := inherited Execute(Tag, Checked);

  for i := 0 to Designer.SelectedObjects.Count - 1 do
  begin
    c := Designer.SelectedObjects[i];
    if (c is TfrxRichView) and not (rfDontModify in c.Restrictions) then
    begin
      v := TfrxRichView(c);
      case Tag of
        1: v.AllowExpressions := Checked;
        2: if Checked then
             v.StretchMode := smActualHeight else
             v.StretchMode := smDontStretch;
        3: if Checked then
             v.StretchMode := smMaxHeight else
             v.StretchMode := smDontStretch;
      end;

      Result := True;
    end;
  end;
end;

{ TfrxRichEditorForm }

procedure TfrxRichEditorForm.SelectionChange(Sender: TObject);
var
  f: TFontParams;
begin
  with RichEdit1.Paragraph do
  try
    FUpdating := True;
    FirstInd.Left := Trunc(FirstIndent * RulerAdj) - 4 + GutterWid;
    LeftInd.Left := Trunc(LeftIndent * RulerAdj) - 4 + GutterWid;
    RightInd.Left := Ruler.ClientWidth - 6 - Trunc((RightIndent + GutterWid) * RulerAdj);
    UndoB.Enabled := RichEdit1.CanUndo;
    f := CurrText;
    BoldB.Down := fsBold in f.Style;
    ItalicB.Down := fsItalic in f.Style;
    UnderlineB.Down := fsUnderline in f.Style;
    BulletsB.Down := Boolean(Numbering);
    FontSizeCB.Text := IntToStr(f.Size);
    FontNameCB.Text := f.Name;
    case Alignment of
      paLeft: LeftAlignB.Down := True;
      paCenter: CenterAlignB.Down := True;
      paRight: RightAlignB.Down := True;
      paJustify: BlockAlignB.Down := True;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TfrxRichEditorForm.FormDestroy(Sender: TObject);
begin
  LazTabLab.Free;
end;

procedure TfrxRichEditorForm.FontStyleModify(fs: TFontStyle);
var
  f : TFontParams;
  rm  : TFontStyles;
  add : TFontStyles;
begin
  RichEdit1.GetTextAttributes(RichEdit1.SelStart, f);
  if fs in f.Style then begin
    rm:=[fs]; add:=[];
  end else begin
    rm:=[]; add:=[fs];
  end;
  RichEdit1.SetRangeParams(RichEdit1.SelStart, RichEdit1.SelLength, [tmm_Styles] , '', 0, 0, add, rm);
end;

function TfrxRichEditorForm.CurrText: TFontParams;
begin
  RichEdit1.GetTextAttributes(RichEdit1.SelStart, Result);
  RichEdit1.Repaint; //Delete when fix
end;

procedure TfrxRichEditorForm.SetupRuler;
var
  I: Integer;
  S: String;
begin
  SetLength(S, 201);
  I := 1;
  while I < 200 do
  begin
    S[I] := #9;
    S[I+1] := '|';
    Inc(I, 2);
  end;

  LazTabLab := TLabel.Create(Ruler);
  LazTabLab.Parent := Ruler;
  LazTabLab.Font.Assign(Ruler.Font);
  LazTabLab.AutoSize := False;
  LazTabLab.Width := Ruler.Width;
  LazTabLab.Height := Ruler.Height;
  LazTabLab.Left := 0;
  LazTabLab.Top := 0;
  LazTabLab.Layout := tlCenter;
  LazTabLab.SendToBack;

  LazTabLab.Caption := S;
end;

procedure TfrxRichEditorForm.UpdateFormPPI(aNewPPI: Integer);  //TODO: Test editor in other monitors
var
  i: Integer;
begin
  inherited;
  SpeedBar.Images := frxResources.MainButtonImages;
  SpeedBar.ImagesWidth := SpeedBar.Images.Width;
  for i := 0 to SpeedBar.ButtonCount - 1 do
    SpeedBar.Buttons[i].AutoSize:= true;
  //SpeedBar.ButtonWidth := 0;  //breaks separators
  //SpeedBar.ButtonHeight := 0;
end;

procedure TfrxRichEditorForm.UpdateResouces;
begin
  inherited;
  Caption := frxGet(4200);
  OpenB.Hint := frxGet(4201);
  SaveB.Hint := frxGet(4202);
  UndoB.Hint := frxGet(4203);
  TTB.Hint := frxGet(4204);
  ExprB.Hint := frxGet(4205);
  CancelB.Hint := frxGet(2);
  OkB.Hint := frxGet(1);
  BoldB.Hint := frxGet(4206);
  ItalicB.Hint := frxGet(4207);
  UnderlineB.Hint := frxGet(4208);
  LeftAlignB.Hint := frxGet(4209);
  CenterAlignB.Hint := frxGet(4210);
  RightAlignB.Hint := frxGet(4211);
  BlockAlignB.Hint := frxGet(4212);
  BulletsB.Hint := frxGet(4213);
  FontNameCB.Hint := frxGet(2322);
  FontSizeCB.Hint := frxGet(2323);
  FillColorB.Hint := frxGet(2345);
end;

procedure TfrxRichEditorForm.SetEditRect;
var
  R: TRect;
begin
  {$IFNDEF Linux}
  with RichEdit1 do
  begin
    R := Rect(GutterWid, 0, ClientWidth - GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, LPARAM(@R));
  end;
  {$ENDIF}
end;

{ Event Handlers }

procedure TfrxRichEditorForm.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TfrxRichEditorForm.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TfrxRichEditorForm.FileOpen(Sender: TObject);
begin
  OpenDialog.Filter := frxResources.Get('ftRichFile') + ' (*.rtf)|*.rtf';
  if OpenDialog.Execute then
  begin
    LoadRTFFile(RichEdit1, OpenDialog.FileName);
    RichEdit1.SetFocus;
    SelectionChange(Self);
  end;
end;

procedure TfrxRichEditorForm.FileSaveAs(Sender: TObject);
begin
  SaveDialog.Filter := frxResources.Get('ftRichFile') + ' (*.rtf)|*.rtf|' +
                       frxResources.Get('ftTextFile') + ' (*.txt)|*.txt';
  if SaveDialog.Execute then
    SaveRTFFile(RichEdit1, ChangeFileExt(SaveDialog.FileName, '.rtf'));
end;

procedure TfrxRichEditorForm.EditUndo(Sender: TObject);
begin
  RichEdit1.Undo;
end;

procedure TfrxRichEditorForm.SelectFont(Sender: TObject);
var
  f: TFontParams;
begin
  f := CurrText;
  FontDialog1.Font.Name := f.Name;
  FontDialog1.Font.Size := f.Size;
  FontDialog1.Font.Color := f.Color;
  FontDialog1.Font.Underline := UnderlineB.Down;
  FontDialog1.Font.StrikeThrough := fsStrikeOut in f.Style;

  if FontDialog1.Execute then
  begin
    RichEdit1.SetRangeParams(RichEdit1.SelStart, RichEdit1.SelLength
      , [tmm_Color, tmm_Size, tmm_Name]
      , FontDialog1.Font.Name
      , FontDialog1.Font.Size
      , FontDialog1.Font.Color, [], []);
    if FontDialog1.Font.Underline <> UnderlineB.Down then
      UnderlineB.Click;
    if FontDialog1.Font.StrikeThrough <> (fsStrikeOut in f.Style) then
      FontStyleModify(fsStrikeOut);
    SelectionChange(Sender);
  end;
  RichEdit1.SetFocus;
end;

procedure TfrxRichEditorForm.RulerResize(Sender: TObject);
begin
  RulerLine.Width := Ruler.ClientWidth - RulerLine.Left * 2;
  LazTabLab.Width := Ruler.Width;
end;

procedure TfrxRichEditorForm.BoldBClick(Sender: TObject);
var
  s: TFontStyle;
begin
  if FUpdating then Exit;
  if Sender = BoldB then s := fsBold else
  if Sender = ItalicB then s := fsItalic else
  if Sender = UnderlineB then s := fsUnderline;
  FontStyleModify(s);
end;

procedure TfrxRichEditorForm.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  case TControl(Sender).Tag of
    0: RichEdit1.Paragraph.Alignment := paLeft;
    1: RichEdit1.Paragraph.Alignment := paCenter;
    2: RichEdit1.Paragraph.Alignment := paRight;
    3: RichEdit1.Paragraph.Alignment := paJustify;
  end;
end;

procedure TfrxRichEditorForm.FontNameCBChange(Sender: TObject);
begin
  if FUpdating then Exit;
  RichEdit1.SetRangeParams(RichEdit1.SelStart, RichEdit1.SelLength,
    [tmm_Name], FontNameCB.Text, 0, 0, [], []);
  RichEdit1.SetFocus;
end;

procedure TfrxRichEditorForm.BulletsBClick(Sender: TObject);
begin
  if FUpdating then Exit;
  RichEdit1.Paragraph.Numbering := TParaNumStyle(BulletsB.Down);
end;

{ Ruler Indent Dragging }

procedure TfrxRichEditorForm.RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragOfs := (TLabel(Sender).Width div 2);
  TLabel(Sender).Left := TLabel(Sender).Left + X - FDragOfs;
  FDragging := True;
end;

procedure TfrxRichEditorForm.RulerItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then
    TLabel(Sender).Left :=  TLabel(Sender).Left + X - FDragOfs
end;

procedure TfrxRichEditorForm.FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.FirstIndent :=
    Trunc((FirstInd.Left + FDragOfs - GutterWid) / RulerAdj);
  LeftIndMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TfrxRichEditorForm.LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.LeftIndent :=
    Trunc((LeftInd.Left + FDragOfs - GutterWid) / RulerAdj);
  SelectionChange(Sender);
end;

procedure TfrxRichEditorForm.RightIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.RightIndent :=
    Trunc((Ruler.ClientWidth - RightInd.Left + FDragOfs - 2) / RulerAdj) - 2 * GutterWid;
  SelectionChange(Sender);
end;

procedure TfrxRichEditorForm.CancelBClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrxRichEditorForm.OkBClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrxRichEditorForm.ExprBClick(Sender: TObject);
var
  s, s1, s2: String;

  function BracketCount: Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(s) do
      if s[i] = '<' then
        Inc(Result);
  end;

begin
  s := TfrxCustomDesigner(Owner).InsertExpression('');
  if s <> '' then
  begin
    s1 := RichView.ExpressionDelimiters;
    s2 := Copy(s1, Pos(',', s1) + 1, 255);
    s1 := Copy(s1, 1, Pos(',', s1) - 1);
    if (s[1] = '<') and (s[Length(s)] = '>') and (BracketCount = 1) then
      s := Copy(s, 2, Length(s) - 2);
    RichEdit1.SelText := s1 + s + s2;
    RichEdit1.SelLength := Length(s1 + s + s2);
  end;
end;

procedure TfrxRichEditorForm.FontSizeCBChange(Sender: TObject);
var
  old, new: Integer;
begin
  try
    old := CurrText.Size;
    new := StrToIntDef(FontSizeCB.Text, old);
    if old <> new then
    RichEdit1.SetRangeParams(RichEdit1.SelStart, RichEdit1.SelLength,
      [tmm_Size], '', new, 0, [], []);
  except
  end;
end;

procedure TfrxRichEditorForm.FontSizeCBKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0', '1'..'9', #8]) then
    Key := #0;
end;

procedure TfrxRichEditorForm.FormCreate(Sender: TObject);
begin
  FontSizeCB.DropDownCount := FontNameCB.Items.Count;
  RichEdit1 := TRxRichMemo.Create(SpeedBar.Parent);
  RichEdit1.Parent := SpeedBar.Parent;
  RichEdit1.Align := alClient;
  RichEdit1.OnSelectionChange := SelectionChange;
  RichEdit1.OnDragOver := ExprMemoDragOver;
  RichEdit1.OnDragDrop := ExprMemoDragDrop;

  Icon := TForm(Owner).Icon;
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  SetupRuler;
  SelectionChange(Self);
  FontSizeCB.OnKeyPress := FontSizeCBKeyPress;

  if UseRightToLeftAlignment then
    FlipChildren(True);
  FFillColor := clNone;
end;

procedure TfrxRichEditorForm.FormShow(Sender: TObject);
begin
  frxAssignRich(RichView.RichMemo, RichEdit1);
  ChangeGlyphColor(38, FFillColor);
  RichEdit1.SetFocus;
end;

procedure TfrxRichEditorForm.FormHide(Sender: TObject);
begin
  if Self.Owner is TfrxDesignerForm then
    ChangeGlyphColor(38, TfrxDesignerForm(Self.Owner).FillColor);
  if ModalResult = mrOk then
    frxAssignRich(RichEdit1, RichView.RichMemo);
end;


procedure TfrxRichEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    frxResources.Help(Self);
end;

procedure TfrxRichEditorForm.FillColorBClick(Sender: TObject);
var
  fnt : TFontParams;
begin
  InitFontParams(fnt);
  fnt.HasBkClr := FFillColor <> clNone;
  fnt.BkColor := FFillColor;
  RichEdit1.SetRangeParams(RichEdit1.SelStart, RichEdit1.SelLength, [tmm_BackColor], fnt, [], []);
end;

procedure TfrxRichEditorForm.FillColorPopupMenuPopup(Sender: TObject);
begin
  if GetTickCount - frxPopupFormCloseTime > 50 then
  with TfrxColorSelector.Create(FillColorB) do
  begin
    Color := FFillColor;
    BtnCaption := frxResources.Get('dsColorOth');
    OnColorChanged := Self.OnColorChanged;
  end;
end;

procedure TfrxRichEditorForm.OnColorChanged(Sender: TObject);
begin
  with TfrxColorSelector(Sender) do
  begin
    case TfrxColorSelector(Sender).Tag of
      26: FFillColor := Color;
    end;
    ChangeGlyphColor(38, FFillColor);
    FillColorBClick(Sender);
    FillColorB.Repaint;
  end;
end;

procedure TfrxRichEditorForm.ExprMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Ret:  LongInt;
  P:    TPoint;
begin
 {$IFNDEF Linux}
  P := Point(x,y);
  Ret := SendMessage(RichEdit1.Handle, EM_CHARFROMPOS, 0, LongInt(@p));
  RichEdit1.CaretPos := Point(Loword(Ret), Hiword(Ret));
 {$ENDIF}
  RichEdit1.SelText := FDataTree.GetFieldName;
  RichEdit1.SelStart := RichEdit1.SelStart + RichEdit1.SelLength;
  RichEdit1.SetFocus;
end;

procedure TfrxRichEditorForm.OnDataTreeDblClick(Sender: TObject);
begin
  RichEdit1.SelText := FDataTree.GetFieldName;
  RichEdit1.SelStart := RichEdit1.SelStart + RichEdit1.SelLength;
  RichEdit1.SetFocus;
end;

initialization

frxComponentEditors.Register(TfrxRichView, TfrxRichEditor);

end.

