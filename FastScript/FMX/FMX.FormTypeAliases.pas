
{******************************************}
{                                          }
{             FastScript v1.9              }
{    Graphics.pas classes and functions    }
{                                          }
{  (c) 2003-2007 by Alexander Tzyganenko,  }
{             Fast Reports Inc             }
{                                          }
{******************************************}

unit FMX.FormTypeAliases;

interface

{$i fs.inc}

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Objects, System.UITypes, System.UIConsts, FMX.Graphics, System.Types, System.TypInfo, FMX.Forms;

type
  TFmxFormBorderStyleHelper = record helper for TFmxFormBorderStyle
  const
    bsNone = TFmxFormBorderStyle.None deprecated 'Use TFmxFormBorderStyle.None';
    bsSingle = TFmxFormBorderStyle.Single deprecated 'Use TFmxFormBorderStyle.Single';
    bsSizeable = TFmxFormBorderStyle.Sizeable deprecated 'Use TFmxFormBorderStyle.Sizeable';
    bsToolWindow = TFmxFormBorderStyle.ToolWindow deprecated 'Use TFmxFormBorderStyle.ToolWindow';
    bsSizeToolWin = TFmxFormBorderStyle.SizeToolWin deprecated 'Use TFmxFormBorderStyle.SizeToolWin';
  end;

  TFormPositionHelper = record helper for TFormPosition
  const
    poDesigned = TFormPosition.Designed deprecated 'Use TFormPosition.Designed';
    poDefault = TFormPosition.Default deprecated 'Use TFormPosition.Default';
    poDefaultPosOnly = TFormPosition.DefaultPosOnly deprecated 'Use TFormPosition.DefaultPosOnly';
    poDefaultSizeOnly = TFormPosition.DefaultSizeOnly deprecated 'Use TFormPosition.DefaultSizeOnly';
    poScreenCenter = TFormPosition.ScreenCenter deprecated 'Use TFormPosition.ScreenCenter';
    poDesktopCenter = TFormPosition.DesktopCenter deprecated 'Use TFormPosition.DesktopCenter';
    poMainFormCenter = TFormPosition.MainFormCenter deprecated 'Use TFormPosition.MainFormCenter';
    poOwnerFormCenter = TFormPosition.OwnerFormCenter deprecated 'Use TFormPosition.OwnerFormCenter';
  end;
  
  TFmxFormStateHelper = record helper for TFmxFormState
  const
    fsRecreating = TFmxFormState.Recreating deprecated 'Use TFmxFormState.Recreating';
    fsModal = TFmxFormState.Modal deprecated 'Use TFmxFormState.Modal';
    fsReleased = TFmxFormState.Released deprecated 'Use TFmxFormState.Released';
    fsInDesigner = TFmxFormState.InDesigner deprecated 'Use TFmxFormState.InDesigner';
    fsWasNotShown = TFmxFormState.WasNotShown deprecated 'Use TFmxFormState.WasNotShown';
    fsShowing = TFmxFormState.Showing deprecated 'Use TFmxFormState.Showing';
    fsUpdateBorder = TFmxFormState.UpdateBorder deprecated 'Use TFmxFormState.UpdateBorder';
    fsActivation = TFmxFormState.Activation deprecated 'Use TFmxFormState.Activation';
  end;


implementation

initialization
  AddEnumElementAliases(TypeInfo(TFmxFormBorderStyle), ['bsNone', 'bsSingle', 'bsSizeable', 'bsToolWindow', 'bsSizeToolWin']);
  AddEnumElementAliases(TypeInfo(TFormPosition), ['poDesigned', 'poDefault', 'poDefaultPosOnly', 'poDefaultSizeOnly', 'poScreenCenter', 'poDesktopCenter',
    'poMainFormCenter', 'poOwnerFormCenter']);
  AddEnumElementAliases(TypeInfo(TFmxFormState), ['fsRecreating', 'fsModal', 'fsReleased', 'fsInDesigner', 'fsWasNotShown', 'fsShowing', 'fsUpdateBorder',
    'fsActivation']);
  
finalization
  RemoveEnumElementAliases(TypeInfo(TFmxFormBorderStyle));
  RemoveEnumElementAliases(TypeInfo(TFormPosition));
  RemoveEnumElementAliases(TypeInfo(TFmxFormState));
  
end.
