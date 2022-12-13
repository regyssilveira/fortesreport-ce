
{******************************************}
{                                          }
{             FastReport VCL               }
{             Data Band editor             }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxEditDataBand;

interface

{$I frx.inc}

uses
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, frxClass, ComCtrls, frxBaseForm
  {$IFDEF FPC}
  , LCLType, LCLIntf
  {$ENDIF}
{$IFDEF Delphi6}
, Variants, frxCtrls
{$ENDIF};


type
  TfrxDataBandEditorForm = class(TfrxBaseForm)
    OkB: TButton;
    CancelB: TButton;
    DatasetGB: TGroupBox;
    DatasetsLB: TListBox;
    RecordsL: TLabel;
    RecordsE: TEdit;
    RecordsUD: TUpDown;
    FilterGB: TGroupBox;
    FilterE: TfrxComboEdit;
    procedure DatasetsLBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure DatasetsLBDblClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DatasetsLBClick(Sender: TObject);
    procedure FilterEButtonClick(Sender: TObject);
  private
    { Private declarations }
    FDataBand: TfrxDataBand;
    FDesigner: TfrxCustomDesigner;
  public
    { Public declarations }
    procedure UpdateResouces; override;
    property DataBand: TfrxDataBand read FDataBand write FDataBand;
  end;


implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses {$IFNDEF FPC}frxUtils,{$ENDIF} frxRes;


procedure TfrxDataBandEditorForm.FormShow(Sender: TObject);
var
  i: Integer;
  dsList: TStringList;
begin
  FDesigner := TfrxCustomDesigner(Owner);

  dsList := TStringList.Create;
  FDesigner.Report.GetDatasetList(dsList);
  dsList.Sort;
  DatasetsLB.Items := dsList;
  DatasetsLB.Items.InsertObject(0, frxResources.Get('dbNotAssigned'), nil);
  dsList.Free;

  i := DatasetsLB.Items.IndexOfObject(FDataBand.DataSet);
  if i = -1 then
    i := 0;
  DatasetsLB.ItemIndex := i;

  RecordsUD.Position := FDataBand.RowCount;
  FilterE.Text := FDataBand.Filter;
end;

procedure TfrxDataBandEditorForm.UpdateResouces;
begin
  inherited;
  Caption := frxGet(3100);
  RecordsL.Caption := frxGet(3101);
  DatasetGB.Caption := frxGet(3102);
  FilterGB.Caption := frxGet(3103);
  OkB.Caption := frxGet(1);
  CancelB.Caption := frxGet(2);
end;

procedure TfrxDataBandEditorForm.FormHide(Sender: TObject);
begin
  if ModalResult = mrOk then
  begin
    if DatasetsLB.ItemIndex = 0 then
    begin
      FDataBand.DataSet := nil;
      FDataBand.RowCount := RecordsUD.Position;
    end
    else
    begin
      FDataBand.DataSet := TfrxDataSet(DatasetsLB.Items.Objects[DatasetsLB.ItemIndex]);
      FDataBand.RowCount := RecordsUD.Position;
    end;
    FDataBand.Filter := FilterE.Text;
  end;
end;

procedure TfrxDataBandEditorForm.DatasetsLBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  bSize: Integer;
begin
  r := ARect;
  bSize := frxResources.MainButtonImages.Width;
  r.Right := r.Left + 2 + bSize;
  r.Bottom := r.Top + bSize;
  OffsetRect(r, 2, 0);
  with TListBox(Control) do
  begin
    Canvas.FillRect(ARect);
    if Index > 0 then
      frxResources.MainButtonImages.Draw(Canvas, ARect.Left + 2, ARect.Top + 1, 53);
    Canvas.TextOut(ARect.Left + bSize + 8, ARect.Top + 2, Items[Index]);
  end;
end;

procedure TfrxDataBandEditorForm.DatasetsLBDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrxDataBandEditorForm.FormCreate(Sender: TObject);
begin
  if UseRightToLeftAlignment then
    FlipChildren(True);
end;

procedure TfrxDataBandEditorForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F1 then
    frxResources.Help(Self);
end;

procedure TfrxDataBandEditorForm.DatasetsLBClick(Sender: TObject);
begin
  if DatasetsLB.ItemIndex <> 0 then
    RecordsUD.Position := 0;
end;

procedure TfrxDataBandEditorForm.FilterEButtonClick(Sender: TObject);
var
  s: String;
begin
  s := TfrxCustomDesigner(Owner).InsertExpression((Sender as TfrxComboEdit).Text);
  if s <> '' then
    (Sender as TfrxComboEdit).Text := s;
end;

end.

