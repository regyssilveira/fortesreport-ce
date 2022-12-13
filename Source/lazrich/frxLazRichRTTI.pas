
{******************************************}
{                                          }
{             FastReport VCL               }
{                Rich RTTI                 }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxLazRichRTTI;

interface

{$I frx.inc}

implementation

uses
  Classes, Types, SysUtils, Forms, fs_iinterpreter, fs_iformsrtti, frxRichLaz,
  RichMemo, ExtCtrls, ComCtrls, frxClassRTTI, Variants;


type
  TFunctions = class(TfsRTTIModule)
  private
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
    AddClass(TRichMemo, 'TWinControl');
    with AddClass(TfrxRichView, 'TfrxView') do
      AddProperty('RichMemo', 'TRichMemo', GetProp, nil);
  end;
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TfrxRichView then
  begin
    if PropName = 'RICHMEMO' then
      Result := frxInteger(TfrxRichView(Instance).RichMemo)
  end
end;


initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.
