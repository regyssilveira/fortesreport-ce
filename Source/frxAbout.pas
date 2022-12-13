
{******************************************}
{                                          }
{             FastReport VCL               }
{              About window                }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxAbout;

interface

{$I frx.inc}

uses
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, frxBaseForm
  {$IFDEF FPC}
  , LCLType, LCLIntf
  {$ENDIF}
  ;

type
  TfrxAboutForm = class(TfrxBaseForm)
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Bevel2: TBevel;
    Label5: TLabel;
    Shape1: TShape;
    Label1: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    procedure UpdateResouces; override;
    { Public declarations }
  end;


implementation

uses frxClass, frxRes

{$IFNDEF FPC} , frxUtils , ShellApi {$ENDIF}
{$IFDEF FR_COM}, Registry{$ENDIF};

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
{$ENDIF}

procedure TfrxAboutForm.FormCreate(Sender: TObject);
begin
  if UseRightToLeftAlignment then
    FlipChildren(True);
end;

procedure TfrxAboutForm.LabelClick(Sender: TObject);
begin
  {$IFDEF FPC}
  if TLabel(Sender).Tag = 1 then
    LCLIntf.OpenURL(TLabel(Sender).Caption)
  else
  if TLabel(Sender).Tag = 2 then
  begin
    LCLIntf.OpenURL('mailto:' + TLabel(Sender).Caption);
  end;
  {$ELSE}
  case TLabel(Sender).Tag of
    1: ShellExecute(GetDesktopWindow, 'open',
      PChar(TLabel(Sender).Caption), nil, nil, sw_ShowNormal);
    2: ShellExecute(GetDesktopWindow, 'open',
      PChar('mailto:' + TLabel(Sender).Caption), nil, nil, sw_ShowNormal);
  end;
  {$ENDIF}
end;

procedure TfrxAboutForm.UpdateResouces;
begin
  inherited;
  Caption := frxGet(2600);
  Label4.Caption := frxGet(2601);
  Label6.Caption := frxGet(2602);
  Label8.Caption := frxGet(2603);
  Label2.Caption := 'Version ' + FR_VERSION;
  Label10.Caption := Chr(174);
  Label3.Caption := '(c) 1998-' + FormatDateTime('YYYY', Now) + ' by Alexander Tzyganenko, Fast Reports Inc.';
end;

procedure TfrxAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

end.

