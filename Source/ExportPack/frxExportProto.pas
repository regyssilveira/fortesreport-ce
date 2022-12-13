
{******************************************}
{                                          }
{             FastReport VCL               }
{         Export Filter Prototype          }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxExportProto;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ShellAPI,
  ComCtrls,
  frxClass;

type
  TfrxProtoExportDialog = class(TForm)
    OkB: TButton;
    CancelB: TButton;
    GroupPageRange: TGroupBox;
    DescrL: TLabel;
    AllRB: TRadioButton;
    CurPageRB: TRadioButton;
    PageNumbersRB: TRadioButton;
    PageNumbersE: TEdit;
    gbOptions: TGroupBox;
    OpenCB: TCheckBox;
    cbPreciseQuality: TCheckBox;
    sd: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure PageNumbersEChange(Sender: TObject);
    procedure PageNumbersEKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

  TfrxProtoExport = class(TfrxCustomExportFilter)
  private

    FOpenAfterExport: Boolean;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDescription: string; override;
    function ShowModal: TModalResult; override;

    function Start: Boolean; override;
    procedure StartPage(Page: TfrxReportPage; Index: Integer); override;
    procedure ExportObject(Obj: TfrxComponent); override;
    procedure FinishPage(Page: TfrxReportPage; Index: Integer); override;
    procedure Finish; override;

  published

    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport;
    property OverwritePrompt;

  end;

implementation

uses
  frxUtils,
  frxRes,
  frxrcExports;

{$R *.dfm}

{ TfrxProtoExportDialog }

procedure TfrxProtoExportDialog.FormCreate(Sender: TObject);

  procedure AssignTexts(Root: TWinControl);
  var
    c: TControl;
    i: Integer;
  begin
    with Root do
      for i := 0 to ControlCount - 1 do
      begin
        c := Controls[i];

        with c do
          if Tag > 0 then
            SetTextBuf(PChar(frxGet(Tag)));

        if c is TWinControl then
          AssignTexts(c as TWinControl);
      end;
  end;

begin
  AssignTexts(Self);

  if UseRightToLeftAlignment then
    FlipChildren(True);
  {$IFDEF DELPHI24}
    ScaleForPPI(Screen.PixelsPerInch);
  {$ENDIF}
end;

procedure TfrxProtoExportDialog.PageNumbersEChange(Sender: TObject);
begin
  PageNumbersRB.Checked := True
end;

procedure TfrxProtoExportDialog.PageNumbersEKeyPress(Sender: TObject; var Key: Char);
begin
  if not (AnsiChar(Key) in ['0'..'9', #9, '-', ',']) then
    Key := #0
end;

procedure TfrxProtoExportDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    frxResources.Help(Self)
end;

{ TfrxProtoExport }

constructor TfrxProtoExport.Create(AOwner: TComponent);
begin
  DefaultExt := '.abc';
  inherited
end;

destructor TfrxProtoExport.Destroy;
begin
  inherited
end;

class function TfrxProtoExport.GetDescription: string;
begin
  Result := frxResources.Get('xProto')
end;

function TfrxProtoExport.ShowModal: TModalResult;
begin
  Result := mrOk;

  if Assigned(Stream) then
    Exit;

  with TfrxProtoExportDialog.Create(nil) do
  begin
    if SlaveExport then
    begin
      OpenCB.Enabled := False;
      OpenCB.State := cbGrayed;
      OpenAfterExport := False;
    end;

    if OverwritePrompt then
      sd.Options := sd.Options + [ofOverwritePrompt];

    sd.FileName := FileName;

    if (FileName = '') and not SlaveExport then
      sd.FileName := ChangeFileExt(
        ExtractFileName(
          frxUnixPath2WinPath(
            Report.FileName)),
          sd.DefaultExt);

    OpenCB.Checked := FOpenAfterExport;

    if PageNumbers <> '' then
    begin
      PageNumbersE.Text     := PageNumbers;
      PageNumbersRB.Checked := True;
    end;

    Result := ShowModal;

    if Result = mrOk then
    begin
      PageNumbers := '';
      CurPage := False;

      if CurPageRB.Checked then
        CurPage := True
      else if PageNumbersRB.Checked then
        PageNumbers := PageNumbersE.Text;

      OpenAfterExport := OpenCB.Checked;

      if not SlaveExport then
      begin
        if DefaultPath <> '' then
          sd.InitialDir := DefaultPath;

        if sd.Execute then
          FileName := sd.FileName
        else
          Result := mrCancel;
      end;
    end;

    Free;
  end;
end;

function TfrxProtoExport.Start: Boolean;
begin
  Result := False;

  if SlaveExport and (FileName = '') then
    if Report.FileName <> '' then
      FileName := ChangeFileExt(
        GetTemporaryFolder + ExtractFileName(Report.FileName),
        DefaultExt)
    else
      FileName := ChangeFileExt(GetTempFile, DefaultExt);

  if (FileName = '') and not Assigned(Stream) then
    Exit;

  if (ExtractFilePath(FileName) = '') and (DefaultPath <> '') then
    FileName := DefaultPath + '\' + FileName;

  Result := True;
end;

procedure TfrxProtoExport.StartPage(Page: TfrxReportPage; Index: Integer);
begin
  inherited
end;

procedure TfrxProtoExport.ExportObject(Obj: TfrxComponent);
begin
  inherited
end;

procedure TfrxProtoExport.FinishPage(Page: TfrxReportPage; Index: Integer);
begin
  inherited
end;

procedure TfrxProtoExport.Finish;
begin
  if OpenAfterExport then
    ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_SHOW)
end;

end.
