
{******************************************}
{                                          }
{             FastReport VCL               }
{               Chart RTTI                 }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxLazChartRTTI;

interface

{$I frx.inc}

uses
  SysUtils, Classes, fs_lazichartrtti;

type
  TfsChartRTTI = class(TComponent); // fake component

implementation

uses
  Forms, fs_iinterpreter, Variants, frxChartLaz;

type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    AddEnum('TfrxSeriesDataType', 'dtDBData, dtBandData, dtFixedData');
    with AddClass(TfrxChartView, 'TfrxView') do
    begin
      AddProperty('Chart', 'TChart', GetProp, nil);
      AddProperty('SeriesCount', 'Integer', GetProp, nil);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;
  if ClassType = TfrxChartView then
  begin
    if MethodName = 'SERIES.GET' then
      Result := frxInteger(TfrxChartView(Instance).Chart.Series[Caller.Params[0]]);
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TfrxChartView then
  begin
    if PropName = 'CHART' then
      Result := frxInteger(TfrxChartView(Instance).Chart)
    else if PropName = 'SERIESCOUNT' then
      Result:=TfrxChartView(Instance).Chart.SeriesCount;
  end
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.
