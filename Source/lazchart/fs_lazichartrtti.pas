{******************************************}
{                                          }
{             FastReport VCL               }
{               Chart RTTI                 }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit fs_lazichartrtti;

{$i frx.inc}

interface

uses
  SysUtils, Classes, fs_iinterpreter, fs_itools, fs_iformsrtti;

implementation

uses TAGraph, TACustomSeries, TASeries, TAChartTeeChart, TARadialSeries, TAMultiSeries,
  TATransformations, TAChartAxis, TAChartAxisUtils, TALegend, TATextElements, TATypes;

type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
    procedure SetProp(Instance: TObject; ClassType: TClass;
      const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;

  { TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    AddEnum('TLegendAlignment', 'laTopLeft, laCenterLeft, laBottomLeft,' +
            'laTopCenter, laBottomCenter, laTopRight, laCenterRight, laBottomRight');
    AddEnum('TSeriesMarksStyle', 'smsCustom, smsNone, smsValue, smsPercent, smsLabel, smsLabelPercent,' +
            'smsLabelValue, smsLegend, smsPercentTotal, smsLabelPercentTotal, smsXValue');
    AddEnum('TSeriesPointerStyle', 'psNone, psRectangle, psCircle, psCross, psDiagCross, psStar,' +
            'psLowBracket, psHighBracket, psLeftBracket, psRightBracket, psDiamond,' +
            'psTriangle, psLeftTriangle, psRightTriangle, psVertBar, psHorBar, psPoint,' +
            'psDownTriangle, psHexagon, psFullStar');

    AddClass(TChartAxisTitle, 'TPersistent');
    AddClass(TChartAxis, 'TPersistent');
    AddClass(TChartTitle, 'TPersistent');
    AddClass(TChartPen, 'TComponent');

    with AddClass(TChartSeries, 'TComponent') do
    begin
      AddMethod('procedure Clear', CallMethod);
      AddMethod('procedure Delete(AIndex: Integer)', CallMethod);
      AddMethod('function Count: Integer', CallMethod);
      AddProperty('Active','Boolean', GetProp, SetProp);
    end;

    AddClass(TSeriesPointer, 'TPersistent');
    AddClass(TLineSeries, 'TCustomChartSeries');
    AddClass(TPointSeries, 'TCustomChartSeries');
    AddClass(TAreaSeries, 'TCustomChartSeries');
    AddClass(TBarSeries, 'TCustomChartSeries');
    AddClass(THorizBarSeries, 'TCustomChartSeries');
    AddClass(TPieSeries, 'TCustomChartSeries');
    AddClass(TCustomChart, 'TCustomControl');
    AddClass(TChart, 'TCustomChart');
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;

  if ClassType = TChartSeries then
  begin
    if MethodName = 'CLEAR' then
      TChartSeries(Instance).Clear
    else if MethodName = 'DELETE' then
      TChartSeries(Instance).Delete(Caller.Params[0])
    else if MethodName = 'COUNT' then
      Result := TChartSeries(Instance).Count
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TChartSeries then
  begin
    if PropName = 'ACTIVE' then
      Result := TChartSeries(Instance).Active;
  end;
end;

procedure TFunctions.SetProp(Instance: TObject; ClassType: TClass;
  const PropName: String; Value: Variant);
begin
  if ClassType = TChartSeries then
  begin
    if PropName = 'ACTIVE' then
      TChartSeries(Instance).Active := Value;
  end;
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  fsRTTIModules.Remove(TFunctions);

end.

