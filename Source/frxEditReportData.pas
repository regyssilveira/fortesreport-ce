
{******************************************}
{                                          }
{             FastReport VCL               }
{         Report datasets selector         }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxEditReportData;

interface

{$I frx.inc}

uses
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frxClass, CheckLst, frxBaseForm, frxComCtrls, Menus, ExtCtrls
  {$IFDEF FPC}
  , LCLType
  {$ENDIF}
{$IFDEF Delphi6}
, Variants
{$ENDIF}
{$IFDEF Delphi16}
, System.Types
{$ENDIF}
;



type
  TfrxDataSetsActions = ( dsaSortSelectedData = 59, dsaSortData = 60,
    dsaUnsorted = 127, dsaAscending = 125, dsaDescending = 126,
    dsaUnselected = 113, dsaAll = 128, dsaSelected = 129);

  TfrxDataSetsSortType = (dssUnsorted, dssAscending, dssDescending);
  TfrxDataSetsSortSelectedType = (dsssAll, dsssSelected, dsssUnselected);

  TfrxReportDataForm = class(TfrxBaseForm)
    OKB: TButton;
    CancelB: TButton;
    DatasetsLB: TCheckListBox;
    SelAllCB: TCheckBox;
    DSPanel: TPanel;
    FooterPanel: TPanel;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DatasetsLBClickCheck(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelAllCBClick(Sender: TObject);
  private
    FStandalone: Boolean;
    FToolWithFilterPanel: TfrxToolWithFilterPanel;
    FSortButton: TfrxToolPanelButton;
    FSortSelectedButton: TfrxToolPanelButton;
    FDataSetsSortType: TfrxDataSetsSortType;
    FDataSetsSortSelectedType: TfrxDataSetsSortSelectedType;
    FSortPopUp: TPopupMenu;
    FSortSelectedPopUp: TPopupMenu;
    FFilter: String;
    procedure BuildConnectionList;
    procedure BuildDatasetList;
    procedure UpdateCBState;
    procedure UpdateSelectedDS;
    procedure ToolOnClick(Sender: TObject);
    procedure CreatefrxToolWithFilterPanel(var ToolWithFilterPanel: TfrxToolWithFilterPanel);
    procedure CreateSortPopup;
    procedure CreateSortSelectedPopup;
    procedure EditChange(Sender: TObject);
  public
    Report: TfrxReport;
    procedure UpdateResouces; override;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses frxDesgn, frxRes
  {$IFNDEF FPC} , frxUtils ,IniFiles , Registry{$ENDIF};

var
  PrevWidth: Integer = 0;
  PrevHeight: Integer = 0;

procedure TfrxReportDataForm.FormCreate(Sender: TObject);
begin
  FStandalone := (frxDesignerComp <> nil) and frxDesignerComp.Standalone;
  if UseRightToLeftAlignment then
    FlipChildren(True);
  FFilter := '';
  CreatefrxToolWithFilterPanel(FToolWithFilterPanel);
  FSortButton := FToolWithFilterPanel.ToolPanel.AddButton(ord(dsaUnsorted), frxGet(4117), ord(dsaSortData), fbsDropDownButton);
  FSortSelectedButton := FToolWithFilterPanel.ToolPanel.AddButton(ord(dsaAll), frxGet(3104), ord(dsaSortSelectedData), fbsDropDownButton);
  FToolWithFilterPanel.ToolPanel.AddCustomButton(fbkSeparator);
 {$IFNDEF FPC}
  FToolWithFilterPanel.BevelKind := bkNone;
 {$ENDIF}
  CreateSortPopup;
  CreateSortSelectedPopup;
end;

procedure TfrxReportDataForm.UpdateSelectedDS;
var
  i: Integer;
begin
    if FStandalone then
    Report.ReportOptions.ConnectionName := '';

  for i := 0 to DatasetsLB.Items.Count - 1 do
    if DatasetsLB.Checked[i] then
      begin
        if FStandalone then
          Report.ReportOptions.ConnectionName := DatasetsLB.Items[i]
        else
        if Report.DataSets.Find(TfrxDataSet(DatasetsLB.Items.Objects[i])) = nil then
          Report.DataSets.Add(TfrxDataSet(DatasetsLB.Items.Objects[i]))
      end
    else
      Report.DataSets.Delete(DatasetsLB.Items[i]);
end;

procedure TfrxReportDataForm.FormShow(Sender: TObject);
begin
  if PrevWidth <> 0 then
  begin
    Width := PrevWidth;
    Height := PrevHeight;
  end;

  if FStandalone then
    BuildConnectionList
  else
    BuildDatasetList;
  UpdateCBState;
end;

procedure TfrxReportDataForm.UpdateResouces;
begin
  inherited;
  if FStandalone then
    Caption := frxGet(5800)
  else
    Caption := frxGet(2800);
  OKB.Caption := frxGet(1);
  CancelB.Caption := frxGet(2);
  SelAllCB.Caption := frxGet(2414);
end;

procedure TfrxReportDataForm.FormHide(Sender: TObject);
begin
  PrevWidth := Width;
  PrevHeight := Height;
  if ModalResult <> mrOk then Exit;

  UpdateSelectedDS;
end;

procedure TfrxReportDataForm.BuildConnectionList;
{$IFNDEF FPC}
var
  i: Integer;
  ini: TRegistry;
  sl: TStringList;
  s2: TStringList;
{$ENDIF}
begin
  {$IFNDEF FPC}
  ini := TRegistry.Create;
  try
    sl := TStringList.Create;
    s2 := TStringList.Create;
    try
      ini.RootKey := HKEY_LOCAL_MACHINE;
      if ini.OpenKeyReadOnly(DEF_REG_CONNECTIONS)  then
      begin
        ini.GetValueNames(sl);
        ini.CloseKey;
      end;

      ini.RootKey := HKEY_CURRENT_USER;
      if ini.OpenKeyReadOnly(DEF_REG_CONNECTIONS)  then
      begin
        ini.GetValueNames(s2);
        ini.CloseKey;
      end;

      sl.AddStrings(s2);

      for i := 0 to sl.Count - 1 do
      begin
        DataSetsLB.Items.Add(sl[i]);
        DataSetsLB.Checked[i] := CompareText(sl[i], Report.ReportOptions.ConnectionName) = 0;
      end;
    finally
      sl.Free;
      s2.Free;
    end;
  finally
    ini.Free;
  end;
  {$ENDIF}
end;

procedure TfrxReportDataForm.BuildDatasetList;
var
  i, nInc, nDSCount: Integer;
  ds: TfrxDataSet;
  dsList: TStringList;
  lFilter: String;
  CheckMark: Boolean;
begin
  lFilter := AnsiUpperCase(FFilter);
  case FDataSetsSortType of
    dssUnsorted: FSortButton.ImageIndex := Ord(dsaUnsorted);
    dssAscending: FSortButton.ImageIndex := Ord(dsaAscending);
    dssDescending: FSortButton.ImageIndex := Ord(dsaDescending);
  end;

  case FDataSetsSortSelectedType of
    dsssAll: 
      begin
        FSortSelectedButton.ImageIndex := Ord(dsaAll);
        FSortSelectedButton.Hint := frxGet(3104);
      end;
    dsssSelected: 
      begin
        FSortSelectedButton.ImageIndex := Ord(dsaSelected);
        FSortSelectedButton.Hint := frxGet(3105);
      end;
    dsssUnselected: 
      begin
        FSortSelectedButton.ImageIndex := Ord(dsaUnselected);
        FSortSelectedButton.Hint := frxGet(3106);
      end;
  end;
  dsList := TStringList.Create;

  if Report.EnabledDataSets.Count > 0 then
  begin
    for i := 0 to Report.EnabledDataSets.Count - 1 do
    begin
      ds := Report.EnabledDataSets[i].DataSet;
      if ds <> nil then
        dsList.AddObject(ds.UserName, ds);
    end;
  end
  else
    Report.GetActiveDataSetList(dsList);

  if FDataSetsSortType <> dssUnsorted then dsList.Sort;

  i := 0;
  nDSCount := dsList.Count - 1;
  nInc := 1;

  if FDataSetsSortType = dssDescending then
  begin
    i := dsList.Count - 1;
    nDSCount := 0;
    nInc := -1;
  end;

  DataSetsLB.Items.Clear;
  CheckMark := True;
  while i * nInc <= nDSCount do
  begin
    ds := TfrxDataSet(dsList.Objects[i]);
    if (csDesigning in Report.ComponentState) and
      ((ds.Owner is TForm) or (ds.Owner is TDataModule){$IFDEF Delphi5} or (ds.Owner is TFrame){$ENDIF}) then
      if lFilter <> '' then
        begin
          CheckMark := (Pos(lFilter, AnsiUpperCase(ds.UserName + '  (' + ds.Owner.Name + '.' + ds.Name + ')')) > 0);
          if (CheckMark) then
            DataSetsLB.Items.AddObject(ds.UserName + '  (' + ds.Owner.Name + '.' + ds.Name + ')', ds);
        end
        else
          DataSetsLB.Items.AddObject(ds.UserName + '  (' + ds.Owner.Name + '.' + ds.Name + ')', ds)
    else
    begin
      if not (ds.Owner is TfrxReport) or (ds.Owner = Report) then
        if lFilter <> '' then
        begin
          CheckMark := (Pos(lFilter, AnsiUpperCase(ds.UserName)) > 0);
          if (CheckMark) then
            DataSetsLB.Items.AddObject(ds.UserName, ds);
        end
        else
          DataSetsLB.Items.AddObject(ds.UserName, ds);
    end;
    if DataSetsLB.Items.Count <> 0 then
      if ((CheckMark) and (Report.Datasets.Find(ds) <> nil)) then
        DataSetsLB.Checked[DataSetsLB.Items.Count - 1] := True;
    Inc(i, nInc);
  end;

  nDSCount := DataSetsLB.Items.Count;
  i := 0;
  if (DataSetsLB.Items.Count <> 0) and (FDataSetsSortSelectedType <> dsssAll) then
      while i < nDSCount do
      begin
        if (FDataSetsSortSelectedType = dsssSelected) and (not DataSetsLB.Checked[i]) then
        begin
          DataSetsLB.Items.Delete(i);
          nDSCount := nDSCount - 1;
          Dec(i, 1);
        end
        else if (FDataSetsSortSelectedType = dsssUnselected) and (DataSetsLB.Checked[i]) then
        begin
          DataSetsLB.Items.Delete(i);
          nDSCount := nDSCount - 1;
          Dec(i, 1);
        end;
        Inc(i, 1);
      end;
  dsList.Free;
end;

procedure TfrxReportDataForm.DatasetsLBClickCheck(Sender: TObject);
var
  i: Integer;
begin
  if FStandalone then
    for i := 0 to DatasetsLB.Items.Count - 1 do
      if i <> DatasetsLB.ItemIndex then
        DatasetsLB.Checked[i] := False;
  UpdateCBState;
end;

procedure TfrxReportDataForm.EditChange(Sender: TObject);
begin
  FFilter := FToolWithFilterPanel.FilterEdit.EditControl.Text;
  BuildDatasetList;
end;

procedure TfrxReportDataForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    frxResources.Help(Self);
end;

procedure TfrxReportDataForm.SelAllCBClick(Sender: TObject);
var
  i: Integer;
begin
  if SelAllCB.State = cbGrayed then Exit;
  for i := 0 to DatasetsLB.Items.Count - 1 do
    DatasetsLB.Checked[i] := SelAllCB.Checked;
  UpdateSelectedDS;
end;

procedure TfrxReportDataForm.UpdateCBState;
var
  i: Integer;
  cbs: TCheckBoxState;
begin
  cbs := cbUnchecked;
  for i := 0 to DatasetsLB.Items.Count - 1 do
  begin
    if (i = 0) and DatasetsLB.Checked[i] then
      cbs := cbChecked;
    if (i > 0) and (not DatasetsLB.Checked[i] and (cbs = cbChecked)) or
      (DatasetsLB.Checked[i] and (cbs = cbUnchecked)) then
    begin
      cbs := cbGrayed;
      break;
    end;
  end;
  SelAllCB.State := cbs;
  UpdateSelectedDS;
end;

procedure TfrxReportDataForm.ToolOnClick(Sender: TObject);
var
  BtnID: TfrxDataSetsActions;
  SenderBtn: TfrxToolPanelButton;
  NewSortType: TfrxDataSetsSortType;
  NewSortSelectedType: TfrxDataSetsSortSelectedType;
  pt: TPoint;
begin
  if Sender is TMenuItem then
  begin
    BtnID := TfrxDataSetsActions(TMenuItem(Sender).Tag);
    NewSortType := dssUnsorted;
    NewSortSelectedType := dsssAll;
    case BtnID of
      dsaUnsorted: NewSortType := dssUnsorted;
      dsaAscending: NewSortType := dssAscending;
      dsaDescending: NewSortType := dssDescending;
      dsaAll: NewSortSelectedType := dsssAll;
      dsaSelected: NewSortSelectedType := dsssSelected;
      dsaUnselected: NewSortSelectedType := dsssUnselected;
    end;

    if (FDataSetsSortType <> NewSortType) and
      ((BtnID = dsaUnsorted)
      or (BtnID = dsaAscending)
      or (BtnID = dsaDescending))
    then
      FDataSetsSortType := NewSortType;

    if (FDataSetsSortSelectedType <> NewSortSelectedType) and
      ((BtnID = dsaAll)
      or (BtnID = dsaSelected)
      or (BtnID = dsaUnselected))
     then
      FDataSetsSortSelectedType := NewSortSelectedType;
    BuildDatasetList;
    Exit;
  end;

  if not Sender.InheritsFrom(TfrxToolPanelButton) then Exit;
  SenderBtn := TfrxToolPanelButton(Sender);
  BtnID := TfrxDataSetsActions(TComponent(Sender).Tag);
  pt := SenderBtn.ClientToScreen(Point(0, SenderBtn.Height));
  case BtnID of
    dsaSortData: FSortPopUp.Popup(pt.X, pt.Y);
    dsaSortSelectedData : FSortSelectedPopUp.Popup(pt.X, pt.Y);
  end;
end;

procedure TfrxReportDataForm.CreatefrxToolWithFilterPanel(var ToolWithFilterPanel: TfrxToolWithFilterPanel);
begin
  ToolWithFilterPanel := TfrxToolWithFilterPanel.Create(Self);
  ToolWithFilterPanel.Parent := Self;
  ToolWithFilterPanel.BorderStyle := bsNone;
  ToolWithFilterPanel.BorderWidth := 0;
{$IFNDEF FPC}
  ToolWithFilterPanel.BevelKind := bkFlat;
  ToolWithFilterPanel.BevelWidth := 1;
  ToolWithFilterPanel.ToolPanel.BevelKind := bkNone;
{$ENDIF}
  ToolWithFilterPanel.AutoSize := False;
  ToolWithFilterPanel.ToolPanel.AutoSize := False;
  ToolWithFilterPanel.ToolPanel.BorderStyle := bsNone;
  ToolWithFilterPanel.ToolPanel.ImageList := frxResources.MainButtonImages;
  ToolWithFilterPanel.FilterActiveImageIndex := 121;
  ToolWithFilterPanel.FilterUnactiveImageIndex := 122;
  ToolWithFilterPanel.ToolPanel.OnBtnClick := ToolOnClick;
  ToolWithFilterPanel.FilterColor := clWindow;
  ToolWithFilterPanel.Align := alTop;
  ToolWithFilterPanel.Height := 30;
  ToolWithFilterPanel.OnFilterChanged := EditChange;
end;

procedure TfrxReportDataForm.CreateSortPopup;
var
  m: TMenuItem;
  procedure CreateItem(sName: String; ImgIdx: Integer);
  begin
      m := TMenuItem.Create(FSortPopUp);
      FSortPopUp.Items.Add(m);
      m.RadioItem := True;
      m.Caption := sName;
      m.ImageIndex := ImgIdx;
      m.Tag := ImgIdx;
      m.OnClick := ToolOnClick;
  end;
begin
  FSortPopUp := TPopupMenu.Create(Self);
  FSortPopUp.Alignment := paLeft;
  FSortPopUp.Images := frxResources.MainButtonImages;
  CreateItem(frxGet(4330), ord(dsaUnsorted));
  CreateItem(frxGet(4328), ord(dsaAscending));
  CreateItem(frxGet(4329), ord(dsaDescending));
end;

procedure TfrxReportDataForm.CreateSortSelectedPopup;
var
  m: TMenuItem;
  procedure CreateItem(sName: String; ImgIdx: Integer);
  begin
      m := TMenuItem.Create(FSortSelectedPopUp);
      FSortSelectedPopUp.Items.Add(m);
      m.RadioItem := True;
      m.Caption := sName;
      m.ImageIndex := ImgIdx;
      m.Tag := ImgIdx;
      m.OnClick := ToolOnClick;
  end;
begin
  FSortSelectedPopUp := TPopupMenu.Create(Self);
  FSortSelectedPopUp.Alignment := paLeft;
  FSortSelectedPopUp.Images := frxResources.MainButtonImages;
  CreateItem(frxGet(3104), ord(dsaAll));
  CreateItem(frxGet(3105), ord(dsaSelected));
  CreateItem(frxGet(3106), ord(dsaUnselected));
end;

end.
