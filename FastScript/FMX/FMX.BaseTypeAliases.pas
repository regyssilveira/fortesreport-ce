
{******************************************}
{                                          }
{             FastScript v1.9              }
{    Graphics.pas classes and functions    }
{                                          }
{  (c) 2003-2007 by Alexander Tzyganenko,  }
{             Fast Reports Inc             }
{                                          }
{******************************************}

unit FMX.BaseTypeAliases;

interface

{$i fs.inc}

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Objects, System.UITypes, System.UIConsts, FMX.Graphics, FMX.Controls, System.Types, System.TypInfo;

type
  
  TCalloutPositionHelper = record helper for TCalloutPosition
  const
    cpTop = TCalloutPosition.Top deprecated 'Use TCalloutPosition.Top';
    cpLeft = TCalloutPosition.Left deprecated 'Use TCalloutPosition.Left';
    cpBottom = TCalloutPosition.Bottom deprecated 'Use TCalloutPosition.Bottom';
    cpRight = TCalloutPosition.Right deprecated 'Use TCalloutPosition.Right';
  end;
  
  TCornerHelper = record helper for TCorner
  const
    crTopLeft = TCorner.TopLeft deprecated 'Use TCorner.TopLeft';
    crTopRight = TCorner.TopRight deprecated 'Use TCorner.TopRight';
    crBottomLeft = TCorner.BottomLeft deprecated 'Use TCorner.BottomLeft';
    crBottomRight = TCorner.BottomRight deprecated 'Use TCorner.BottomRight';
  end;
  
  TCornerTypeHelper = record helper for TCornerType
  const
    ctRound = TCornerType.Round deprecated 'Use TCornerType.Round';
    ctBevel = TCornerType.Bevel deprecated 'Use TCornerType.Bevel';
    ctInnerRound = TCornerType.InnerRound deprecated 'Use TCornerType.InnerRound';
    ctInnerLine = TCornerType.InnerLine deprecated 'Use TCornerType.InnerLine';
  end;
  
  TSideHelper = record helper for TSide
  const
    sdTop = TSide.Top deprecated 'Use TSide.Top';
    sdLeft = TSide.Left deprecated 'Use TSide.Left';
    sdBottom = TSide.Bottom deprecated 'Use TSide.Bottom';
    sdRight = TSide.Right deprecated 'Use TSide.Right';
  end;
  
  TTextAlignHelper = record helper for TTextAlign
  const
    taCenter = TTextAlign.Center deprecated 'Use TTextAlign.Center';
    taLeading = TTextAlign.Leading deprecated 'Use TTextAlign.Leading';
    taTrailing = TTextAlign.Trailing deprecated 'Use TTextAlign.Trailing';
  end;
  
  TTextTrimmingHelper = record helper for TTextTrimming
  const
    ttNone = TTextTrimming.None deprecated 'Use TTextTrimming.None';
    ttCharacter = TTextTrimming.Character deprecated 'Use TTextTrimming.Character';
    ttWord = TTextTrimming.Word deprecated 'Use TTextTrimming.Word';
  end;
  
  TStyledSettingHelper = record helper for TStyledSetting
  const
    ssFamily = TStyledSetting.Family deprecated 'Use TStyledSetting.Family';
    ssSize = TStyledSetting.Size deprecated 'Use TStyledSetting.Size';
    ssStyle = TStyledSetting.Style deprecated 'Use TStyledSetting.Style';
    ssFontColor = TStyledSetting.FontColor deprecated 'Use TStyledSetting.FontColor';
    ssOther = TStyledSetting.Other deprecated 'Use TStyledSetting.Other';
  end;
  
  TMenuItemChangeHelper = record helper for TMenuItemChange
  const
    mcEnabled = TMenuItemChange.Enabled deprecated 'Use TMenuItemChange.Enabled';
    mcVisible = TMenuItemChange.Visible deprecated 'Use TMenuItemChange.Visible';
    mcText = TMenuItemChange.Text deprecated 'Use TMenuItemChange.Text';
    mcShortcut = TMenuItemChange.Shortcut deprecated 'Use TMenuItemChange.Shortcut';
    mcChecked = TMenuItemChange.Checked deprecated 'Use TMenuItemChange.Checked';
    mcBitmap = TMenuItemChange.Bitmap deprecated 'Use TMenuItemChange.Bitmap';
  end;
  
  TScreenOrientationHelper = record helper for TScreenOrientation
  const
    soPortrait = TScreenOrientation.Portrait deprecated 'Use TScreenOrientation.Portrait';
    soLandscape = TScreenOrientation.Landscape deprecated 'Use TScreenOrientation.Landscape';
    soInvertedPortrait = TScreenOrientation.InvertedPortrait deprecated 'Use TScreenOrientation.InvertedPortrait';
    soInvertedLandscape = TScreenOrientation.InvertedLandscape deprecated 'Use TScreenOrientation.InvertedLandscape';
  end;
  
  TFormStyleHelper = record helper for TFormStyle
  const
    fsNormal = TFormStyle.Normal deprecated 'Use TFormStyle.Normal';
    fsPopup = TFormStyle.Popup deprecated 'Use TFormStyle.Popup';
    fsStayOnTop = TFormStyle.StayOnTop deprecated 'Use TFormStyle.StayOnTop';
  end;
  
  TAlignLayoutHelper = record helper for TAlignLayout
  const
    alNone = TAlignLayout.None deprecated 'Use TAlignLayout.None';
    alTop = TAlignLayout.Top deprecated 'Use TAlignLayout.Top';
    alLeft = TAlignLayout.Left deprecated 'Use TAlignLayout.Left';
    alRight = TAlignLayout.Right deprecated 'Use TAlignLayout.Right';
    alBottom = TAlignLayout.Bottom deprecated 'Use TAlignLayout.Bottom';
    alMostTop = TAlignLayout.MostTop deprecated 'Use TAlignLayout.MostTop';
    alMostBottom = TAlignLayout.MostBottom deprecated 'Use TAlignLayout.MostBottom';
    alMostLeft = TAlignLayout.MostLeft deprecated 'Use TAlignLayout.MostLeft';
    alMostRight = TAlignLayout.MostRight deprecated 'Use TAlignLayout.MostRight';
    alClient = TAlignLayout.Client deprecated 'Use TAlignLayout.Client';
    alContents = TAlignLayout.Contents deprecated 'Use TAlignLayout.Contents';
    alCenter = TAlignLayout.Center deprecated 'Use TAlignLayout.Center';
    alVertCenter = TAlignLayout.VertCenter deprecated 'Use TAlignLayout.VertCenter';
    alHorzCenter = TAlignLayout.HorzCenter deprecated 'Use TAlignLayout.HorzCenter';
    alHorizontal = TAlignLayout.Horizontal deprecated 'Use TAlignLayout.Horizontal';
    alVertical = TAlignLayout.Vertical deprecated 'Use TAlignLayout.Vertical';
    alScale = TAlignLayout.Scale deprecated 'Use TAlignLayout.Scale';
    alFit = TAlignLayout.Fit deprecated 'Use TAlignLayout.Fit';
    alFitLeft = TAlignLayout.FitLeft deprecated 'Use TAlignLayout.FitLeft';
    alFitRight = TAlignLayout.FitRight deprecated 'Use TAlignLayout.FitRight';
  end;
  
  TGradientStyleHelper = record helper for TGradientStyle
  const
    gsLinear = TGradientStyle.Linear deprecated 'Use TGradientStyle.Linear';
    gsRadial = TGradientStyle.Radial deprecated 'Use TGradientStyle.Radial';
  end;
  
  TBrushKindHelper = record helper for TBrushKind
  const
    bkNone = TBrushKind.None deprecated 'Use TBrushKind.None';
    bkSolid = TBrushKind.Solid deprecated 'Use TBrushKind.Solid';
    bkGradient = TBrushKind.Gradient deprecated 'Use TBrushKind.Gradient';
    bkBitmap = TBrushKind.Bitmap deprecated 'Use TBrushKind.Bitmap';
    bkResource = TBrushKind.Resource deprecated 'Use TBrushKind.Resource';
  end;

  TStrokeCapHelper = record helper for TStrokeCap
  const
    scFlat = TStrokeCap.Flat deprecated 'Use TStrokeCap.Flat';
    scRound = TStrokeCap.Round deprecated 'Use TStrokeCap.Round';
  end;

  TStrokeJoinHelper = record helper for TStrokeJoin
  const
    sjMiter = TStrokeJoin.Miter deprecated 'Use TStrokeJoin.Miter';
    sjRound = TStrokeJoin.Round deprecated 'Use TStrokeJoin.Round';
    sjBevel = TStrokeJoin.Bevel deprecated 'Use TStrokeJoin.Bevel';
  end;

  TStrokeDashHelper = record helper for TStrokeDash
  const
    sdSolid = TStrokeDash.Solid deprecated 'Use TStrokeDash.Solid';
    sdDash = TStrokeDash.Dash deprecated 'Use TStrokeDash.Dash';
    sdDot = TStrokeDash.Dot deprecated 'Use TStrokeDash.Dot';
    sdDashDot = TStrokeDash.DashDot deprecated 'Use TStrokeDash.DashDot';
    sdDashDotDot = TStrokeDash.DashDotDot deprecated 'Use TStrokeDash.DashDotDot';
    sdCustom = TStrokeDash.Custom deprecated 'Use TStrokeDash.Custom';
  end;
  
  TFillTextFlagHelper = record helper for TFillTextFlag
  const
    ftRightToLeft = TFillTextFlag.RightToLeft deprecated 'Use TFillTextFlag.RightToLeft';
  end;
  
  TMapAccessHelper = record helper for TMapAccess
  const
    maRead = TMapAccess.Read deprecated 'Use TMapAccess.Read';
    maWrite = TMapAccess.Write deprecated 'Use TMapAccess.Write';
    maReadWrite = TMapAccess.ReadWrite deprecated 'Use TMapAccess.ReadWrite';
  end;
  
  TOrientationHelper = record helper for TOrientation
  const
    orHorizontal = TOrientation.Horizontal deprecated 'Use TOrientation.Horizontal';
    orVertical = TOrientation.Vertical deprecated 'Use TOrientation.Vertical';
  end;
  
  TPlacementHelper = record helper for TPlacement
  const
    plBottom = TPlacement.Bottom deprecated 'Use TPlacement.Bottom';
    plTop = TPlacement.Top deprecated 'Use TPlacement.Top';
    plLeft = TPlacement.Left deprecated 'Use TPlacement.Left';
    plRight = TPlacement.Right deprecated 'Use TPlacement.Right';
    plCenter = TPlacement.Center deprecated 'Use TPlacement.Center';
    plBottomCenter = TPlacement.BottomCenter deprecated 'Use TPlacement.BottomCenter';
    plTopCenter = TPlacement.TopCenter deprecated 'Use TPlacement.TopCenter';
    plLeftCenter = TPlacement.LeftCenter deprecated 'Use TPlacement.LeftCenter';
    plRightCenter = TPlacement.RightCenter deprecated 'Use TPlacement.RightCenter';
    plAbsolute = TPlacement.Absolute deprecated 'Use TPlacement.Absolute';
    plMouse = TPlacement.Mouse deprecated 'Use TPlacement.Mouse';
    plMouseCenter = TPlacement.MouseCenter deprecated 'Use TPlacement.MouseCenter';
  end;
  
  TVirtualKeyboardTypeHelper = record helper for TVirtualKeyboardType
  const
    vktDefault = TVirtualKeyboardType.Default deprecated 'Use TVirtualKeyboardType.Default';
    vktNumbersAndPunctuation = TVirtualKeyboardType.NumbersAndPunctuation deprecated 'Use TVirtualKeyboardType.NumbersAndPunctuation';
    vktNumberPad = TVirtualKeyboardType.NumberPad deprecated 'Use TVirtualKeyboardType.NumberPad';
    vktPhonePad = TVirtualKeyboardType.PhonePad deprecated 'Use TVirtualKeyboardType.PhonePad';
    vktAlphabet = TVirtualKeyboardType.Alphabet deprecated 'Use TVirtualKeyboardType.Alphabet';
    vktURL = TVirtualKeyboardType.URL deprecated 'Use TVirtualKeyboardType.URL';
    vktNamePhonePad = TVirtualKeyboardType.NamePhonePad deprecated 'Use TVirtualKeyboardType.NamePhonePad';
    vktEmailAddress = TVirtualKeyboardType.EmailAddress deprecated 'Use TVirtualKeyboardType.EmailAddress';
  end;

  TVirtualKeyboardStateHelper = record helper for TVirtualKeyboardState
  const
    vksAutoShow = TVirtualKeyboardState.AutoShow deprecated 'Use TVirtualKeyboardState.AutoShow';
    vksVisible = TVirtualKeyboardState.Visible deprecated 'Use TVirtualKeyboardState.Visible';
    vksError = TVirtualKeyboardState.Error deprecated 'Use TVirtualKeyboardState.Error';
    vksTransient = TVirtualKeyboardState.Transient deprecated 'Use TVirtualKeyboardState.Transient';
  end;
  
  TReturnKeyTypeHelper = record helper for TReturnKeyType
  const
    rktDefault = TReturnKeyType.Default deprecated 'Use TReturnKeyType.Default';
    rktDone = TReturnKeyType.Done deprecated 'Use TReturnKeyType.Done';
    rktGo = TReturnKeyType.Go deprecated 'Use TReturnKeyType.Go';
    rktNext = TReturnKeyType.Next deprecated 'Use TReturnKeyType.Next';
    rktSearch = TReturnKeyType.Search deprecated 'Use TReturnKeyType.Search';
    rktSend = TReturnKeyType.Send deprecated 'Use TReturnKeyType.Send';
  end;

implementation

initialization
  AddEnumElementAliases(TypeInfo(TCalloutPosition), ['cpTop', 'cpLeft', 'cpBottom', 'cpRight']);
  AddEnumElementAliases(TypeInfo(TCorner), ['crTopLeft', 'crTopRight', 'crBottomLeft', 'crBottomRight']);
  AddEnumElementAliases(TypeInfo(TCornerType), ['ctRound', 'ctBevel', 'ctInnerRound', 'ctInnerLine']);
  AddEnumElementAliases(TypeInfo(TSide), ['sdTop', 'sdLeft', 'sdBottom', 'sdRight']);
  AddEnumElementAliases(TypeInfo(TTextAlign), ['taCenter', 'taLeading', 'taTrailing']);
  AddEnumElementAliases(TypeInfo(TTextTrimming), ['ttNone', 'ttCharacter', 'ttWord']);
  AddEnumElementAliases(TypeInfo(TStyledSetting), ['ssFamily', 'ssSize', 'ssStyle', 'ssFontColor', 'ssOther']);
  AddEnumElementAliases(TypeInfo(TMenuItemChange), ['mcEnabled', 'mcVisible', 'mcText', 'mcShortcut', 'mcChecked', 'mcBitmap']);
  AddEnumElementAliases(TypeInfo(TScreenOrientation), ['soPortrait', 'soLandscape', 'soInvertedPortrait', 'soInvertedLandscape']);
  AddEnumElementAliases(TypeInfo(TFormStyle), ['fsNormal', 'fsPopup', 'fsStayOnTop']);
  AddEnumElementAliases(TypeInfo(TAlignLayout), ['alNone', 'alTop', 'alLeft', 'alRight', 'alBottom', 'alMostTop', 'alMostBottom', 'alMostLeft', 'alMostRight', 'alClient', 'alContents', 'alCenter', 'alVertCenter', 'alHorzCenter', 'alHorizontal', 'alVertical', 'alScale', 'alFit', 'alFitLeft', 'alFitRight']);  
  AddEnumElementAliases(TypeInfo(TGradientStyle), ['gsLinear', 'gsRadial']);
  AddEnumElementAliases(TypeInfo(TBrushKind), ['bkNone', 'bkSolid', 'bkGradient', 'bkBitmap', 'bkResource']);
  AddEnumElementAliases(TypeInfo(TStrokeCap), ['scFlat', 'scRound']);
  AddEnumElementAliases(TypeInfo(TStrokeJoin), ['sjMiter', 'sjRound', 'sjBevel']);
  AddEnumElementAliases(TypeInfo(TStrokeDash), ['sdSolid', 'sdDash', 'sdDot', 'sdDashDot', 'sdDashDotDot', 'sdCustom']);
  AddEnumElementAliases(TypeInfo(TMapAccess), ['maRead', 'maWrite', 'maReadWrite']);
  AddEnumElementAliases(TypeInfo(TFillTextFlag), ['ftRightToLeft']);
  AddEnumElementAliases(TypeInfo(TOrientation), ['orHorizontal', 'orVertical']);
  AddEnumElementAliases(TypeInfo(TPlacement), ['plBottom', 'plTop', 'plLeft', 'plRight', 'plCenter', 'plBottomCenter', 'plTopCenter', 'plLeftCenter', 'plRightCenter', 'plAbsolute', 'plMouse', 'plMouseCenter']);
  AddEnumElementAliases(TypeInfo(TVirtualKeyboardType), ['vktDefault', 'vktNumbersAndPunctuation', 'vktNumberPad', 'vktPhonePad', 'vktAlphabet', 'vktURL', 'vktNamePhonePad', 'vktEmailAddress']);
  AddEnumElementAliases(TypeInfo(TReturnKeyType), ['rktDefault', 'rktDone', 'rktGo', 'rktNext', 'rktSearch']);
  AddEnumElementAliases(TypeInfo(TVirtualKeyboardState), ['vksAutoShow', 'vksVisible', 'vksError', 'vksTransient']);

  
finalization
  RemoveEnumElementAliases(TypeInfo(TCalloutPosition));
  RemoveEnumElementAliases(TypeInfo(TCorner));
  RemoveEnumElementAliases(TypeInfo(TCornerType));
  RemoveEnumElementAliases(TypeInfo(TSide));
  RemoveEnumElementAliases(TypeInfo(TTextAlign));
  RemoveEnumElementAliases(TypeInfo(TTextTrimming));
  RemoveEnumElementAliases(TypeInfo(TStyledSetting));
  RemoveEnumElementAliases(TypeInfo(TMenuItemChange));
  RemoveEnumElementAliases(TypeInfo(TScreenOrientation));
  RemoveEnumElementAliases(TypeInfo(TFormStyle));
  RemoveEnumElementAliases(TypeInfo(TAlignLayout));
  RemoveEnumElementAliases(TypeInfo(TGradientStyle));
  RemoveEnumElementAliases(TypeInfo(TBrushKind));
  RemoveEnumElementAliases(TypeInfo(TStrokeCap));
  RemoveEnumElementAliases(TypeInfo(TStrokeJoin));
  RemoveEnumElementAliases(TypeInfo(TStrokeDash));
  RemoveEnumElementAliases(TypeInfo(TMapAccess));
  RemoveEnumElementAliases(TypeInfo(TFillTextFlag));
  RemoveEnumElementAliases(TypeInfo(TOrientation));
  RemoveEnumElementAliases(TypeInfo(TPlacement));
  RemoveEnumElementAliases(TypeInfo(TVirtualKeyboardType));
  RemoveEnumElementAliases(TypeInfo(TReturnKeyType));
  RemoveEnumElementAliases(TypeInfo(TVirtualKeyboardState));

  
end.
