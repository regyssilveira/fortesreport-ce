{******************************************}
{                                          }
{             FastReport VCL               }
{           OpenFilterQR object            }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxOpenFilterQR;

interface
{$I frx.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Printers, TypInfo, Jpeg, DB, System.Generics.Collections,
  frxClass, frxVariables, frxPrinter, frxDCtrl, frxBarcode, frxBarcod, StrUtils,
{$IFDEF DELPHI16}
  VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series, VCLTee.TeCanvas
{$ELSE}
  TeeProcs, TeEngine, Chart, Series, TeCanvas
{$ENDIF}
{$IFDEF DELPHI16}
 {$IFDEF TeeChartPro}, VCLTEE.TeeEdit{$IFNDEF TeeChart4}, VCLTEE.TeeEditCha{$ENDIF} {$ENDIF}
{$ELSE}
 {$IFDEF TeeChartPro}, TeeEdit{$IFNDEF TeeChart4}, TeeEditCha{$ENDIF} {$ENDIF}
{$ENDIF}
 ,frxChart, frxChBox, frxOLE, frxRich,
  frxCross, frxDBSet, frxUnicodeUtils, frxUtils, fs_ipascal,
  frxCustomDB,frxBarcode2D ,frxADOComponents
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  TppDuplex = (dpNone, dpHorizontal, dpVertical);
  TShapeType = (stRectangle, stRoundRect, stEllipse, stSquare,
                stRoundSquare ,stCircle);

  TUnits =    (Characters, Inches, MM, Pixel, Native) ;

  TFilterType = (tDFM, tQR2);

  TAssignProp = procedure ();

{$IFDEF DELPHI16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TfrxOpenFilterQR = class(TfrxIOTransportFile)
  private
    FOpenFilter: TFilterType;
    FSplitPages: Boolean;
  protected
    function GetFilterString: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetOpenFilter(Value:TFilterType);
    function DoFilterProcessStream(aStream: TStream; ProcesssingObject: TObject): Boolean; override;
    function OpenFilter: Boolean; override;
    class function GetDescription: String; override;
    procedure AssignFilter(Source: TfrxCustomIOTransport); override;

published
	property Filter: TFilterType read FOpenFilter write SetOpenFilter;
  property SplitPages: Boolean  read FSplitPages write FSplitPages;

  end;

implementation
 var ParentBands : TStringList;
 PageZoom: Double;

{LoadFromQR}
function LoadFromQR(AReport: TfrxReport; AStream: TStream; Split: Boolean): Boolean;
Var
  Reader: TReader;
  SaveSeparator: Char;
  ClassName,ObjectName,PropName: string;
  Flags: TFilerFlags;
  Position: Integer;
  Val:Variant;
  LastObj: TfrxComponent;
  Parent: TfrxComponent;
  isBin: Boolean;
  Sig: AnsiString;
  DataBand: TfrxBand;
  DSName: String;
  CntPages: integer;
  DTDataSource: TDictionary<String, String>;
  DMaster: TDictionary<String, String>;

 function GetBoolValue(Str: String): Boolean;
 begin
  Result := False;
  If CompareStr(Str,'True') = 0 then
    Result := True;
 end;


function FindDS_in_DFM(NameDS : string): TfrxDataSet;
var
   ReportDFM: TfrxReport;
   DataSet: TfrxDataSet;
   Database: TfrxADODatabase;
   Database1: TfrxADODatabase;
   CurrentDir : string;
   SR      : TSearchRec;
   DirList : TStrings;
   i : integer;
begin
    CurrentDir := ExtractFilePath(AReport.FileName);
    DataSet := nil;

    DirList:=TStringList.Create;
    try
          if FindFirst(CurrentDir + '*.dfm', faArchive, SR) = 0 then
          begin
            repeat
                DirList.Add(CurrentDir+SR.Name); //Fill the list
            until FindNext(SR) <> 0;
            //FindClose(SR);
          end;
     for i:=0 to DirList.Count-1 do
     begin
        ReportDFM := TfrxReport.Create(nil);
        ReportDFM.LoadFromFile(DirList[i]);
        if ReportDFM.FindObject(NameDS) = nil then
          break
        else
        begin
          DataSet := ReportDFM.DataSets.Find(NameDS).DataSet;
          if DataSet.ClassName =  'TfrxADOTable' then
          begin
            Database := TfrxADODatabase(ReportDFM.FindObject(TfrxADOTable(DataSet).Database.Name));
            Database1:= TfrxADODatabase.Create (AReport.Pages[0]);
            Database1.AssignAll(Database);
            Database1.Name := Database.Name;
          end
          else
          begin
            Database := TfrxADODatabase(ReportDFM.FindObject(TfrxADOQuery(DataSet).Database.Name));
            Database1:= TfrxADODatabase.Create (AReport.Pages[0]);
            Database1.AssignAll(Database);
            Database1.Name := Database.Name;
          end;

          break
        end;

     end;

    finally
     DirList.Free;
    end;

    if DataSet = nil then
      Result := nil
    else
      Result := DataSet;

end;

procedure PSplitPages;
 var
  SReport: TfrxReport;
  ADataSet : TfrxDataSet;
  SDataSet : TfrxDataSet;
  APage : TfrxReportPage;
  SPage : TfrxReportPage;
  i : Integer;
  j : Integer;
  CurrentDir : string;
 begin
     SReport := TfrxReport.Create(nil);
     CurrentDir := ExtractFilePath( AReport.FileName);

    if not AReport.PagesCount <2 then
       for i := 1 to AReport.PagesCount-1 do
       begin
          SReport.Clear;
          with TfrxDataPage.Create(SReport) do
          begin
            CreateUniqueName;
          end;

          for j := 0 to AReport.DataSets.Count-1 do
          begin
            ADataSet := AReport.DataSets[j].DataSet;
            SDataSet := TfrxDataSet(ADataSet.NewInstance);
            SDataSet.Create(SReport.Pages[0]);
            SDataSet.AssignAll(ADataSet);
            SDataSet.Name := ADataSet.Name;
          end;

          APage := TfrxReportPage(AReport.Pages[i]);
          SPage := TfrxReportPage.Create(SReport);
          SPage.AssignAll(APage);
          SPage.Name := APage.Name;
          SReport.SaveToFile(CurrentDir+SPage.Name+'.fr3');

       end;

 end;

 procedure AssignDataSource;
 begin
   if PropName = 'DataSet' then
   begin
      if not DTDataSource.ContainsKey(ObjectName) then
        DTDataSource.Add(ObjectName,Val)
      else
      begin
        DTDataSource.Remove(ObjectName);
        DTDataSource.Add(ObjectName,Val);
      end;

   end
 end;

 procedure AssignAllDataSource;
 var
    Value: String;
    Item: TPair<string, string>;
    DataSet : TfrxCustomDataset;
 begin
  for Item in DMaster do
    if DTDataSource.ContainsKey(Item.Value) then
      begin
        if not (Item.Key = ' ') then
        begin
          DataSet:= TfrxCustomDataset(AReport.DataSets.Find(Item.Key).DataSet);
          Value:= DTDataSource.Items[Item.Value];
          DataSet.Master := TfrxCustomDataset(AReport.DataSets.Find(Value).DataSet);
        end;

      end
 end;

 procedure AssignAllChildBands;
 var
    Value: String;
    Index : Integer;
    FindBandParent : TfrxComponent;
    FindBandChild : TfrxComponent;
 begin
    for Index := 0 to ParentBands.Count-1 do
    begin
      FindBandParent := AReport.FindObject(ParentBands.ValueFromIndex[Index]);
      FindBandChild := AReport.FindObject(ParentBands.Names[Index]);
      if FindBandChild <>nil then
        TfrxBand(FindBandParent).Child := TfrxChild(FindBandChild);
    end;
 end;

 function CreateDataSet(Name: String):TfrxDBDataset;
 var
   DataSet: String;
 begin

   if DMaster.ContainsKey(Name) then
   begin
      DMaster.TryGetValue(Name,DataSet);

      if DTDataSource.ContainsKey(DataSet) then
      begin
        DTDataSource.TryGetValue(DataSet,DataSet);
        Result:= AReport.FindObject(DataSet) as TfrxDBDataset
      end
      else Result := nil;
   end
   else
   begin
      DMaster.Add(ObjectName, Name);
      if not DTDataSource.ContainsKey(Name) then
        DTDataSource.Add(Name,'');
      Result := nil;
   end;


 end;

 procedure AssignADOQuery;
 var
   Query: TfrxADOQuery;
   Database: TfrxADODatabase;
 begin
   Query := LastObj as TfrxADOQuery;
   if Query = nil then exit;
   Query.UserName := ObjectName;

   if PropName = 'DatabaseName' then
   begin
      if (AReport.FindObject(Val) = nil) then
      begin
         Database := TfrxADODatabase.Create(AReport.Pages[0]);
         Database.Name := Val;
         Database.LoginPrompt := false;
         Database.Connected := false;
         Query.Database := Database;
      end
      else
      begin
        Database := TfrxADODatabase(AReport.FindObject(Val));
        Query.Database := Database;
      end


   end
   else if PropName = 'SQL.Strings' then
    Query.SQL.Add(Val)
 end;

 procedure AssignADOTable;
 var
   Table: TfrxADOTable;
   Database: TfrxADODatabase;
 begin
   Table := LastObj as TfrxADOTable;
   if Table = nil then exit;
    Table.UserName := ObjectName;

   if PropName = 'Active' then

   else if PropName = 'DatabaseName' then
    if (AReport.FindObject(Val) = nil) then
      begin
         Database := TfrxADODatabase.Create(AReport.Pages[0]);
         Database.Name := Val;
         Database.LoginPrompt := false;
         Database.Connected := false;
         Table.Database := Database;
      end
    else
    begin
      Database := TfrxADODatabase(AReport.FindObject(Val));
      Database.Connected:= false;
      Table.Database := Database;
    end

   else if PropName = 'IndexName' then
     Table.IndexName := Val
   else if PropName = 'MasterFields' then
     Table.MasterFields := Val
   else if PropName = 'MasterSource' then
     Table.Master := CreateDataSet(Val)

   else if PropName = 'TableName' then
     Table.TableName := Val
   else if PropName = 'ConnectionString' then


 end;

procedure AssignDBProp;
 var
  View: TfrxView;
  i: Integer;
 begin
   View := LastObj as TfrxView;
   if PropName = 'DataSet' then
   begin
     i := pos('.', Val) + 1;
     View.DataSetName := Val;
     if i <> - 1 then
       View.DataSetName := copy(Val, i, length(View.DataSetName) - i)
     else View.DataSetName := '';
   end
   else
   if PropName = 'DataField' then
    View.DataField := Val
   else if PropName = 'DataPipelineName' then
     View.DataSetName := Val
 end;


 function GetCharsetByName(cName: String):TFontCharset;
 begin
   if cName = 'ANSI_CHARSET' then
     Result := ANSI_CHARSET
   else if cName = 'DEFAULT_CHARSET' then
     Result := DEFAULT_CHARSET
   else if cName = 'SYMBOL_CHARSET' then
     Result := SYMBOL_CHARSET
   else if cName = 'MAC_CHARSET' then
     Result := MAC_CHARSET
   else if cName = 'SHIFTJIS_CHARSET' then
     Result := SHIFTJIS_CHARSET
   else if cName = 'HANGEUL_CHARSET' then
     Result := HANGEUL_CHARSET
   else if cName = 'JOHAB_CHARSET' then
     Result := JOHAB_CHARSET
   else if cName = 'GB2312_CHARSET' then
     Result := GB2312_CHARSET
   else if cName = 'CHINESEBIG5_CHARSET' then
     Result := CHINESEBIG5_CHARSET
   else if cName = 'GREEK_CHARSET' then
     Result := GREEK_CHARSET
   else if cName = 'TURKISH_CHARSET' then
     Result := TURKISH_CHARSET
   else if cName = 'HEBREW_CHARSET' then
     Result := HEBREW_CHARSET
   else if cName = 'ARABIC_CHARSET' then
     Result := ARABIC_CHARSET
   else if cName = 'BALTIC_CHARSET' then
     Result := BALTIC_CHARSET
   else if cName = 'RUSSIAN_CHARSET' then
     Result := RUSSIAN_CHARSET
   else if cName = 'THAI_CHARSETT' then
     Result := THAI_CHARSET
   else if cName = 'EASTEUROPE_CHARSET' then
     Result := EASTEUROPE_CHARSET
   else if cName = 'OEM_CHARSET' then
     Result := OEM_CHARSET
   else
    Result := 1;
 end;

 function GetFrameStyle(PStyle : String): TfrxFrameStyle;
 var  PenStyle: TPenStyle;
begin
  PenStyle :=   TPenStyle(GetEnumValue(TypeInfo(TPenStyle), Val));


  if PenStyle = psDash            then  Result := TfrxFrameStyle.fsDash
  else if PenStyle = psDot        then  Result := TfrxFrameStyle.fsDot
  else if PenStyle = psDashDot    then  Result := TfrxFrameStyle.fsDashDot
  else if PenStyle = psDashDotDot then  Result := TfrxFrameStyle.fsDashDotDot
  else Result := TfrxFrameStyle.fsSolid;
end;

function GetStretch(AutoStretch:boolean): TfrxStretchMode;
begin
  if AutoStretch then result:= smMaxHeight
  else result:= smDontStretch;
end;

procedure AssignFont;
 var
  View: TfrxView;
 begin
   View := LastObj as TfrxView;
   if View = nil then exit;
   if PropName = 'Font.Charset' then
    View.Font.Charset :=  GetCharsetByName(Val)
   else if PropName = 'Font.Color' then
    View.Font.Color := StringToColor(Val)
   else if PropName = 'Font.Height' then
    View.Font.Height := Val
   else if PropName = 'Font.Name' then
    View.Font.Name := Val
   else if PropName = 'FontSize' then
    View.Font.Size := Val / PageZoom
   else if PropName = 'Font.Style' then
    View.Font.Style := View.Font.Style + [TFontStyle(GetEnumValue(TypeInfo(TFontStyle), Val))]
 end;

procedure AssignReport();
 var
   Page: TfrxReportPage;
   DS, DS1 : TfrxDataSet;
 begin

    Page := LastObj as TfrxReportPage;
  {Page property}
    if  (ClassName = 'TQuickRep') or (ClassName = 'TDesignQuickReport') then
      Page.Name := ObjectName;

    if PropName = 'Page.mmPaperHeight' then
      Page.PaperHeight := Val
    else if PropName = 'PrinterSetup.mmPaperWidth' then
      Page.PaperWidth := Val
    else if PropName = 'PrinterSetup.mmMarginTop' then
      Page.TopMargin := Val
    else if PropName = 'PrinterSetup.mmMarginBottom' then
      Page.BottomMargin := Val

    else if PropName = 'Width' then
      AReport.Width := Val
    else if PropName = 'Height' then
      AReport.Height := Val

    else if PropName = 'Frame.Color' then
      Page.Frame.Color :=  StringToColor(Val)
    else if PropName = 'Frame.DrawTop' then
      if GetBoolValue(Val) then
        Page.Frame.Typ := Page.Frame.Typ +[ftTop]
      else
        Page.Frame.Typ := Page.Frame.Typ -[ftTop]

    else if PropName = 'Frame.DrawBottom' then
      if GetBoolValue(Val) then
        Page.Frame.Typ := Page.Frame.Typ +[ftBottom]
      else
        Page.Frame.Typ := Page.Frame.Typ -[ftBottom]

    else if PropName = 'Frame.DrawLeft' then
      if GetBoolValue(Val) then
        Page.Frame.Typ := Page.Frame.Typ +[ftLeft]
      else
        Page.Frame.Typ := Page.Frame.Typ -[ftLeft]

    else if PropName = 'Frame.DrawRight' then
      if GetBoolValue(Val) then
        Page.Frame.Typ := Page.Frame.Typ +[ftRight]
      else
        Page.Frame.Typ := Page.Frame.Typ -[ftRight]

    else if PropName = 'DataSet' then
            if ClassName = 'TDesignQuickReport' then
            begin

              if string(Val).Substring(0,Pos('.',Val)-1) = 'RuntimeDatamodule'
              then
                Page.DataSetName:= string(Val).Substring(Pos('.',Val))
              else
              begin
                DS := FindDS_in_DFM(string(Val).Substring(Pos('.',Val)));
                if DS <> nil then
                begin
                  DS1 := TfrxDataSet(DS.NewInstance);
                  DS1.Create(AReport.Pages[0]);
                  DS1.AssignAll(DS);
                  DS1.Name := Ds.Name;

                  AReport.DataSets.Add(DS1);
                  Page.DataSet := DS;
                end;

              end;

            end
            else
              Page.DataSetName:= Val

    else if PropName = 'Font.Charset' then
      Page.Font.Charset :=  GetCharsetByName(Val)
    else if PropName = 'Font.Color' then
      Page.Font.Color := StringToColor(Val)
    else if PropName = 'Font.Height' then
      Page.Font.Height := Val
    else if PropName = 'Font.Name' then
      Page.Font.Name := Val
    else if PropName = 'FontSize' then
      Page.Font.Size := Val
    else if PropName = 'Font.Style' then
      Page.Font.Style := Page.Font.Style + [TFontStyle(GetEnumValue
                                      (TypeInfo(TFontStyle), Val))]

    else if PropName = 'Page.Columns' then
    begin
      Page.Columns := Val;
      Page.ColumnPositions.Clear;
    end
    else if PropName = 'Page.PaperSize' then
      //Page.PaperSize := Val
    else if PropName = 'Page.Orientation' then
      if (Val='poPortrait') then  Page.Orientation := poPortrait
      else Page.Orientation := poLandscape

    else if PropName = 'PrinterSetup.Copies' then
      AReport.PrintOptions.Copies := Val
    else if PropName  = 'PrinterSetup.Duplex' then
      Page.Duplex := TfrxDuplexMode(GetEnumValue(TypeInfo(TppDuplex),Val))

    else if PropName = 'PrinterSettings.OutputBin' then
      Page.Bin := frxPrinters.Printer.BinNameToNumber(Val)

    else if PropName = 'PrintIfEmpty' then
      Page.PrintIfEmpty :=  GetBoolValue(Val)
    else if PropName = 'Zoom' then
      PageZoom := 100 / Val;
 end;

 //QRExpr
function ReplaceExpr(BandName, ItemName, s: string;
                      IsCondition, IsFilter:boolean): string;
var sExpr, sSub, sDS :string;
    i:integer;
    bQS:boolean;

const  cExprOper: array[1..17] of string = ( '+' , '-' , '/' , '*' , '>' , '<' ,
        '=' , '>=' , '<=' , '<>' , '(' , ')', '[' , ']' , '''' , '.' , ',' );
       cExprFun : array[1..45] of string =
       //QR
       ( 'IF', 'STR', 'UPPER', 'LOWER', 'PRETTY', 'TIME', 'DATE', 'COPY', 'SUM',
       'COUNT', 'MAX', 'MIN','AVERAGE', 'TRUE', 'FALSE', 'INT', 'FRAC', 'SQRT',
       'DIV', 'TYPEOF','FORMATNUMERIC',
        //QRDesign
       'ABS', 'CALCDATE', 'DAYOFWEEK', 'DAYSTRING', 'EXTRACTDAY','EXTRACTMONTH',
       'EXTRACTYEAR', 'FIELDLEN','GETCAPTION','ISNUL','MONTHSTRING', 'PADLEFT',
       'PADRIGHT','PRINTDATE', 'QUERYNAME', 'READINI','READREGISTRY',
       'REFORMATDATE', 'STRTONUM', 'TRIM', 'VAR', 'ISEMPTY','PAGENUMBER',
       'PAGECOUNT' );
  //--------------------------------------------------
  Function IsNum(s:string):Boolean;
  var x:integer;
  begin
    Result := False;
    for x := 1 to Length(s) do
      case s[x] of
        '0'..'9','.': Result := True;
      else
        Result := False;
        break;
      end;
  end;
  //--------------------------------------------------
  Function TrimControl(s:string):string;
  var i:integer;
  begin
    for i := 1 to Length(s) do
      if not CharInSet(s[i],[#9, #10, #13]) then Result := Result+s[i];
  end;
  //--------------------------------------------------
  Function GetSub(sOper:string):string;
  begin
    if MatchText(sSub,cExprFun) then
    begin
      { some are translated, some the same, some need to be
      checked/changed manualy }
      if      sSub = 'PAGENUMBER'     then sSub:='Page#'
      else if sSub = 'PAGECOUNT'      then sSub:='TotalPages#'
      else if sSub = 'PRINTDATE'      then sSub:='Date'
      else if sSub = 'EXTRACTDAY'     then sSub:='DayOf'
      else if sSub = 'EXTRACTMONTH'   then sSub:='MonthOf'
      else if sSub = 'EXTRACTYEAR'    then sSub:='YearOf'
      else if sSub = 'STRTONUM'       then sSub:='StrToFloat'
      else if sSub = 'FORMATNUMERIC'  then sSub:='FormatFloat'
      else if sSub = 'STR'            then sSub:='StrToInt'
      else if sSub = 'IF'             then sSub:='IIF'
      else if sSub = 'AVERAGE'        then sSub:='AVG'
      else if sSub = 'TRUE'           then sSub:='BoolToStr(True)'
      else if sSub = 'FALSE'          then sSub:='BoolToStr(False)'
      else if sSub = 'COUNT'          then sSub:='Count()'
      else if sSub = 'UPPER'          then sSub:='Uppercase'
      else if sSub = 'LOWER'          then sSub:='Lowercase'
      else if sSub = 'PRETTY'         then sSub:='NameCase'
      else if sSub = 'INT'            then sSub:='IntToStr'
      { change name }
      else if sSub = 'TRIM'         then sSub:='Trim'
      else if sSub = 'SUM'          then sSub:='Sum'
      else if sSub = 'MIN'          then sSub:='Min'
      else if sSub = 'MAX'          then sSub:='Max'
      else if sSub = 'DATE'         then sSub:='Date'
      else if sSub = 'TIME'         then sSub:='Time'
      else if sSub = 'COPY'         then sSub:='Copy'
      else if sSub = 'FRAC'         then sSub:='Frac'
      else if sSub = 'SQRT'         then sSub:='Sqrt' ;

      if (sSub='Page#') or (sSub='TotalPages#') or (sSub='Date') then
         result := sExpr + 'VarToStr(<' + sSub + '>)'  + sOper
      else
         result := sExpr + sSub  + sOper;

    { Field found (if not num then it will probabely a field with or without DS) }
    end
    else if (sSub<>'') and (not IsNum(sSub)) and (not bQS) then
    begin
      if sDS = '' then sDS := AReport.DataSetName;
      Result := sExpr + Format('<%s."%s">', [sDS, sSub]) + sOper ;
      sDs := '';

    { Get subString}
    end
    else
    begin
      Result := sExpr + sSub + sOper;
    end;
  end;
  //--------------------------------------------------

begin
  { init }
  sSub := '';
  bQS := False;

  { trim control chars}
  s := TrimControl(s);

  {empty expression }
  if s = '' then
  begin
    { if group band(condition), then FR needs an condition
    in GroupHeader using main dataset + '' (ex: Order."")}
    if IsCondition then
    begin
      Result := AReport.DataSetName+'.""';
    end
    else Result := '';
    exit;
  end;

  {check string}
  for i := 1 to length(s) do
  begin
    {check for operator chars}
    if MatchStr(s[i],cExprOper) then
    begin
      { Quoted string found}
      if (s[i] = '''') then
      begin
        if bQS then
        begin
          bQS := False;
          sExpr := sExpr + sSub + s[i];
        end
        else
        begin
          bQS := True;
          sExpr := sExpr + s[i];
        end;

      { DataSet found }
      end
      else if (s[i] = '.') and (not IsNum(sSub)) then
      begin
        sDS := sSub;

      { get substring }
      end
      else sExpr := GetSub(s[i]);

      sSub := '';

    { if Quoted string or no space char then add to subString}
    end
    else if bQS or (s[i]<>' ') then
    begin
      sSub := sSub + s[i];
    end;

    {add last subString if exist}
    if  (i = length(s)) and (sSub<>'') then
       sExpr := GetSub('');

  end;

  { set expression in brackets if not a condition or filter }
   if not (IsCondition or IsFilter) then
      sExpr := '[' + sExpr + ']';

  {result}
  Result:=sExpr;
end;

//----------------------------------------------------------------------------

function ReplaceMemo(BandName, ItemName, s: string): string;
var iSPos, iEPos:integer;
    sSub, sFR:string;
begin
  { get first expression}
  iSPos := Pos('%', s)+2;
  iEPos := PosEx('%' , s, iSPos);
  while (iSPos > 0) and (iEPos > iSPos) do
  begin
    { get expression}
    sSub := Copy(s, iSPos, iEPos - iSPos);

    { replace expression }
    sFR:= ReplaceExpr(BandName, ItemName, sSub,False,False);

    { insert expression }
    s:= Copy(s,1,iSPos-3) + sFR + Copy(s,iEPos+2, Length(s) - iSPos);

    { get next expression }
    iSPos := Pos('%', s)+2;
    iEPos := PosEx('%' , s, iSPos);

  end;
  Result := s;
end;

procedure AssignBandData;
 var
  B: TfrxDataBand;
 begin
   B := LastObj as TfrxDataBand;
   if B = nil then exit;
   B.DataSetName := DSName;
 end;

procedure AssignMemo();
 var
   Memo: TfrxMemoView;
   MF     : string;
 const  cMaskDate: array[1..24] of string = ( 'c' , 'd' , 'dd' , 'ddd' ,
  'dddd','ddddd' , 'dddddd' , 'm' , 'mmm' , 'mmm' , 'mmmm' ,'y', 'yyy' ,
  'h' ,'hh' , 'n', 'nn' , 's' , 'ss' , 't', 'tt' , 'am/pm' , 'a/p' , 'ampm' );

  function CntCh(InputStr: string; InputSubStr: char): integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 1 to length(InputStr) do
      if InputStr[i] = InputSubStr then inc(result);
  end;
  function GetFormatKind(const S: string): TfrxFormatKind;
  begin
    if MatchStr(s,cMaskDate) or (CntCh(s,'/')=2) or (CntCh(s,'-')=2)
    or (CntCh(s,':')=1) then result:=fkDateTime
    else result:=fkNumeric;
  end;

 begin
    Memo := LastObj as TfrxMemoView;
    //Memo.Name := ObjectName;

    if PropName = 'Height' then
      Memo.Height := Val
    else if PropName = 'Width' then
      Memo.Width := Val + 5
    else if PropName = 'Left' then
      Memo.Left := Val
    else if PropName = 'Top' then
      Memo.Top := Val

    else if PropName = 'Frame.Color' then
      Memo.Frame.Color :=  StringToColor(Val)

    else if PropName = 'Frame.DrawTop' then
      if GetBoolValue(Val) then
        Memo.Frame.Typ := Memo.Frame.Typ +[ftTop]
      else
        Memo.Frame.Typ := Memo.Frame.Typ -[ftTop]

    else if PropName = 'Frame.DrawBottom' then
      if GetBoolValue(Val) then
        Memo.Frame.Typ := Memo.Frame.Typ +[ftBottom]
      else
        Memo.Frame.Typ := Memo.Frame.Typ -[ftBottom]

    else if PropName = 'Frame.DrawLeft' then
      if GetBoolValue(Val) then
        Memo.Frame.Typ := Memo.Frame.Typ +[ftLeft]
      else
        Memo.Frame.Typ := Memo.Frame.Typ -[ftLeft]

    else if PropName = 'Frame.DrawRight' then
      if GetBoolValue(Val) then
        Memo.Frame.Typ := Memo.Frame.Typ +[ftRight]
      else
        Memo.Frame.Typ := Memo.Frame.Typ -[ftRight]

    else if PropName = 'Alignment' then
          begin
            if Val = 'taLeftJustify' then
              Memo.HAlign := haLeft
            else if Val = 'taRightJustify' then
              Memo.HAlign := haRight
            else if Val = 'taCenter' then
              Memo.HAlign := haCenter;
          end

    else if (PropName = 'AutoSize') then
        Memo.AutoWidth := Val
    else if PropName= 'Color' then
        Memo.Color := StringToColor(Val)

    else if (PropName = 'DataSet') then
        if ((ClassName = 'TQRDBText') or (ClassName = 'TQRPDBText')
            or (ClassName = 'TQRDesignDBText')) then
          Memo.DataSetName :=  string(Val).Substring(Pos('.',Val))
        else
          Memo.DataSetName := Val
    else if (PropName = 'DataField') then
    begin
        Memo.DataField := Val
    end


    else if (PropName = 'Caption') and (Memo.Text = '') then
        Memo.Text := ReplaceMemo(Parent.Name,Memo.Name,Val)

    else if ((ClassName = 'TQRDBText') or (ClassName = 'TQRPDBText')
            or (ClassName = 'TQRDesignDBText')) then
    begin
       Memo.Text := Format('[%s."%s"]',
       [Memo.DataSetName.Substring(Pos('.',Memo.DataSetName)), Memo.DataField]);
       if (PropName = 'Mask') then
       begin
          MF := Val;
          if MF<>'' then begin
            Memo.DisplayFormat.FormatStr:= MF;
            Memo.DisplayFormat.Kind:=GetFormatKind(MF);
          end;
       end;
    end

    else if ((ClassName = 'TQRMemo') or (ClassName = 'TQRPMemo')
    or (ClassName = 'TPIQRMemo'))
          and (PropName = 'Lines.Strings') then
        Memo.Text := Memo.Text + ReplaceMemo(Parent.Name,Memo.Name,Val)

    else if ((ClassName = 'TQRExpr') or (ClassName = 'TQRDBCalc')
            or (ClassName = 'TQRPExpr') or (ClassName = 'TQRDesignExpr')
            or (ClassName = 'TQRDesignDBText')
            )
          and (PropName = 'Expression') then
    begin
       Memo.Text := ReplaceExpr(Parent.Name,Memo.Name,Val,False,False);
       if (PropName = 'Mask') then
       begin
          MF := Val;
          if MF<>'' then begin
            Memo.DisplayFormat.FormatStr:= MF;
            Memo.DisplayFormat.Kind:=GetFormatKind(MF);
          end;
       end;

    end


    else if Pos('Font', PropName) = 1 then
        AssignFont

    else if PropName = 'BlankIfZero' then
        Memo.HideZeros := Val

    else if PropName = 'WordWrap' then
        Memo.WordWrap := Val

    else if ((ClassName = 'TQRSysData') or (ClassName = 'TQRDesignSysdata'))
          and (PropName = 'Data') then
         begin
                 if Val = 'qrsTime'        then   Memo.Text := '[Time]'
            else if Val = 'qrsDate'        then   Memo.Text := '[Date]'
            else if Val = 'qrsDateTime'    then   Memo.Text := '[Now]'
            else if Val = 'qrsPageNumber'  then   Memo.Text := '[Page#]'
            else if Val = 'qrsDetailNo'    then   Memo.Text := '[Line#]'
            else if Val = 'qrsDetailCount' then   Memo.Text := '[Count()]'
            else Memo.Text := 'Unknown Variable';
         end
    else if (ClassName = 'TQRHTMLLabel') then Memo.AllowHTMLTags := true;
 end;


 procedure AssignShape();
 var
   Shape: TfrxShapeView;
 begin
 if not(LastObj is TfrxLineView) then
    Shape := LastObj as TfrxShapeView;
    //Shape.Name := ObjectName;

    if PropName = 'Brush.Color' then
      Shape.Color :=  StringToColor(Val)

    else if PropName = 'Pen.Color' then
      Shape.Frame.Color :=  StringToColor(Val)

    else if PropName = 'Shape' then
         begin
                 if Val = 'qrsRectangle' then   Shape.Shape := skRectangle
            else if Val = 'qrsCircle'    then   Shape.Shape := skEllipse
            else if Val = 'qrsRoundRect' then   Shape.Shape := skRoundRectangle
         end

    else if PropName = 'Pen.Width' then
      Shape.Frame.Width := Val

    else if PropName = 'Pen.Style' then
      Shape.Frame.Style := GetFrameStyle(Val)

    else if PropName = 'Brush.Style' then
      Shape.BrushStyle  := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle), Val))

    else if PropName = 'RoundFactor' then
      Shape.Curve  := Round(Val)

 end;


 procedure AssignRich();
 var
   Rich: TfrxRichView;
 begin
    Rich := LastObj as TfrxRichView;
    //Rich.Name := ObjectName;

    if Pos('Font', PropName) = 1 then
      AssignFont

    else if PropName = 'AutoStretch' then
      Rich.StretchMode := GetStretch(Val)

    else if PropName = 'Alignment' then
          begin
            if Val = 'taLeftJustify' then
              Rich.Align := baLeft
            else if Val = 'taRightJustify' then
              Rich.Align := baRight
            else if Val = 'taCenter' then
              Rich.Align := baCenter;
          end

    else if PropName = 'Color' then
        Rich.Color :=  StringToColor(Val)

    else if (PropName = 'DataSet') then
        Rich.DataSetName := Val
    else if (PropName = 'DataField') then
        Rich.DataField := Val

    else if (PropName = 'Lines.Strings') then
        Rich.RichEdit.Lines.Text := Rich.RichEdit.Lines.Text
                                + ReplaceMemo(Parent.Name,Rich.Name,Val)

    else if ClassName = 'TQRDBRichText' then
        Rich.RichEdit.Text := Format('[%s."%s"]', [Rich.DataSetName, Rich.DataField])
 end;

 procedure AssignBarcodeView();
 var
   BarCode: TfrxBarcode2DView;
 begin
    BarCode := LastObj as TfrxBarcode2DView;
    //BarCode.Name := ObjectName;

    if (ClassName = 'TQRQRBarcode') or (ClassName = 'TQRQRDBBarcode') then
      BarCode.BarType := bcCodeQR
    else if (ClassName = 'TQRDMBarcode') or (ClassName = 'TQRDbDMBarcode') then
      BarCode.BarType := bcCodeDataMatrix;

    if (PropName = 'BarcodeText') or (PropName = 'Text') then
    begin
      if Val ='' then BarCode.Text :=' '
      else BarCode.Text := Val
    end
    else if (PropName = 'DataSet') then
        BarCode.DataSetName := Val
    else if (PropName = 'DataField') then
        BarCode.DataField := Val

 end;


 procedure ObjectCreator(Name:String);
 begin
    if (Name = 'TDesignQuickReport') or  (Name = 'TQuickRep')
        or  (Name = 'TQRPQuickrep') then
    begin
        AssignAllDataSource();
        CntPages := CntPages + 1;
        LastObj := TfrxReportPage.Create(AReport);
        Parent := LastObj;
        TfrxReportPage(LastObj).CreateUniqueName;
        TfrxReportPage(LastObj).SetDefaults;
        TfrxReportPage(LastObj).TitleBeforeHeader := false;
    end;

    if (Name = 'TdaSQL') then
    begin
      LastObj := TfrxADOQuery.Create(AReport.Pages[0]);
      LastObj.CreateUniqueName;
    end;

    if   (Name = 'TQRLabel')
      or (Name = 'TQRSysData')
      or (Name = 'TQRExpr')
      or (Name = 'TQRMemo')
      or (Name = 'TQRDBText')
      or (Name = 'TQRExprMemo')
      or (Name = 'TQRDBCalc')
      or (Name = 'TQRHTMLLabel')


      or (Name = 'TQRDesignLabel')
	  or (Name = 'TQRDesignDBText')
      or (Name = 'TQRDesignExpr')
      or (Name = 'TQRDesignSysdata')
	  or (Name = 'TQRDesignMemo')

      or (Name = 'TQRPLabel')
      or (Name = 'TQRPDBText')
      or (Name = 'TQRPExpr')
      or (Name = 'TQRPMemo')
      or (Name = 'TPIQRMemo')
      then
    begin
      LastObj := TfrxMemoView.Create(Parent);
      LastObj.CreateUniqueName;
      TfrxMemoView(LastObj).AutoWidth := True;
    end

  else if (Name = 'TQRImage')
       or (Name = 'TQRDBImage')
       or (Name = 'TQRGraphicCanvas')
       or (Name = 'TQRGrImage')
       or (Name = 'TQRGrDBImage')

	   or (Name = 'TQRDesignImage')
	   or (Name = 'TQRDesignDBImage')

       or (Name = 'TQRDBJPGlmage')
       or (Name = 'TQRPDBlmage')
       then
    begin
      LastObj := TfrxPictureView.Create(Parent);
      LastObj.CreateUniqueName;
    end

  else if (Name = 'TQRRichText')
       or (Name = 'TQRDBRichText')

       or (Name = 'TQRDesignRichtext')
       or (Name = 'TQRDesignDBRichtext')

       or (Name = 'TQRPRichtext')
      then
    begin
      LastObj := TfrxRichView.Create(Parent);
      LastObj.CreateUniqueName;
    end
  else if (Name = 'TQRQRBarcode') or (Name = 'TQRQRDBBarcode')
        or (Name = 'TQRDMBarcode') or (Name = 'TQRDbDMBarcode') then
    begin
      LastObj := TfrxBarcode2DView.Create(Parent);
      LastObj.CreateUniqueName;
    end
  else if (Name = 'TQRFrameline') then
   begin
     LastObj := TfrxLineView.Create(Parent);
     LastObj.CreateUniqueName;
   end

  else if (Name = 'TQRLineGraph') then
   begin
     LastObj := TfrxChartView.Create(Parent);
     LastObj.CreateUniqueName;
   end
  else if (Name = 'TADOQuery')
       or (Name = 'TQRDQuery')
       or (Name = 'TQuery')
       or (Name = 'TFDQuery')
       or (Name = 'TwwQuery') then
    begin
      LastObj := TfrxADOQuery.Create(AReport.Pages[0]);
      LastObj.CreateUniqueName;
      AReport.DataSets.Add(LastObj as TfrxADOQuery);
    end
  else if (Name = 'TADOTable')
       or (Name = 'TQRDTable')
       or (Name = 'TTable')
       or (Name = 'TFDTable')
       or (Name = 'TwwTable') then
    begin
      LastObj := TfrxADOTable.Create(AReport.Pages[0]);
      LastObj.CreateUniqueName;
      AReport.DataSets.Add(LastObj as TfrxADOTable);
    end
 end;

 procedure BandCreator(Name:String);
 begin
    if  (Name = 'rbPageHeader') then
    begin
      LastObj := TfrxPageHeader.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbPageFooter') then
    begin
      LastObj := TfrxPageFooter.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbTitle')then
    begin
      LastObj := TfrxReportTitle.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbColumnHeader') then
    begin
      LastObj := TfrxColumnHeader.Create(Parent);
      LastObj.CreateUniqueName;
    end

    else if (Name = 'rbGroupHeader') then
    begin
      LastObj := TfrxGroupHeader.Create(Parent);
      LastObj.CreateUniqueName;
    end

    else if (Name = 'rbGroupFooter') then
    begin
      LastObj := TfrxGroupFooter.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbChild') then
    begin
      LastObj := TfrxChild.Create(Parent);
      LastObj.CreateUniqueName;
    end
     else if (Name = 'rbDetail') then
    begin
      LastObj := TfrxMasterData.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbSubDetail') then
    begin
      LastObj := TfrxDetailData.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbSummary') then
    begin
      LastObj := TfrxReportSummary.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else if (Name = 'rbOverlay') then
    begin
      LastObj := TfrxOverlay.Create(Parent);
      LastObj.CreateUniqueName;
    end

 end;

  procedure ShapeCreator(Name:String);
  begin
    if (Name = 'qrsVertLine')  or  (Name = 'qrpsVertLine')
      or (Name = 'qrsHorLine') or  (Name = 'qrpsHorLine')
      or (Name = 'qrsLeftDiagonal') or  (Name = 'qrpsLeftDiagonal')
      or (Name = 'qrsRightDiagonal')or  (Name = 'qrpsRightDiagonal')
      or (Name = 'qrsTopAndBottom') or  (Name = 'qrpsTopAndBottom')
      or (Name = 'qrsRightAndLeft') or  (Name = 'qrpsRightAndLeft')
    then
    begin
      LastObj := TfrxLineView.Create(Parent);
      LastObj.CreateUniqueName;
    end
    else
    if   (Name = 'qrsRectangle') or (Name = 'qrpsRectangle')
      or (Name = 'qrsCircle')    or (Name = 'qrpsCircle')
      or (Name = 'qrsRoundRect') or (Name = 'qrpsRoundRect')
      then
    begin
      LastObj := TfrxShapeView.Create(Parent);
      LastObj.CreateUniqueName;
    end;

  end;

 procedure AssignView;
 begin

    if ObjectName = '' then
      LastObj.CreateUniqueName()
    else
      LastObj.Name := ObjectName;

    if PropName = 'Height' then
      LastObj.Height := Val
    else if PropName = 'Width' then
      LastObj.Width := Val
    else if PropName = 'Left' then
      LastObj.Left := Val
    else if PropName = 'Top' then
      LastObj.Top := Val
    else if PropName = 'Visible' then
       LastObj.Visible := Val
 end;

 procedure AssignPicture;
 var
   Stream: TMemoryStream;
   Cn: Integer;
   Image : TfrxPictureView;
 begin
    Image  := LastObj as TfrxPictureView;
    //Image.Name := ObjectName;

    if PropName = 'Picture.Data' then
    begin
      Stream := TMemoryStream.Create;
      Cn := 0;
      TMemoryStream(frxInteger(Val)).Position := 0;
      TMemoryStream(frxInteger(Val)).Read(Cn, 1);
      TMemoryStream(frxInteger(Val)).Position := Cn + 1;
      Stream.SetSize(TMemoryStream(frxInteger(Val)).Size - (Cn + 1));
      Stream.CopyFrom(TMemoryStream(frxInteger(Val)), Stream.Size);
      TfrxPictureView(LastObj).LoadPictureFromStream(Stream);
      Stream.Free;
    end
    else if PropName = 'AutoSize' then
        Image.AutoSize:= Val
    else if PropName = 'Stretch' then
        Image.Stretched := Val
    else if PropName = 'Center' then
        Image.Center := Val
    else if PropName = 'Picture.Bitmap.Transparent' then
        Image.Transparent := Val
    else if PropName = 'DataField' then
        Image.DataField  := Val
    else if PropName = 'DataSet' then
        Image.DataSetName:= Val

    else if (ClassName = 'TQRGrImage') or (ClassName = 'TQRGrDBImage') then
        Image.KeepAspectRatio:= true;
 end;

 procedure AssignProp;
 var FindBand : TfrxComponent;
 begin
   if (Pos('DB', ClassName) = 4) or (Pos('DB', ClassName) = 10) then
    AssignDBProp;
   if (PropName = 'UserName') and not (LastObj is TfrxPage) then
    LastObj.Name := Val
   else if (ClassName = 'TQuickRep') or (ClassName = 'TDesignQuickReport')
        or  (ClassName = 'TQRPQuickrep') then
     AssignReport

   else if (ClassName = 'TQRDesignBand')
        or (ClassName = 'TQRBand')
        or (ClassName = 'TQRPBand')
 //       or (ClassName = 'TQRGroup')
        or (ClassName = 'TQRSubDetail')
        or (ClassName = 'TQRDesignSubdetail')
        or (ClassName = 'TQRChildBand')
        or (ClassName = 'TQRPChildBand')
        or (ClassName = 'TQRLoopBand') then
         begin
           if PropName = 'Height' then
             begin
               TfrxBand(LastObj).Height := Val ;
             end
           else if (Val = 'rbGroupHeader') then
             begin
                if DataBand <> nil then
                begin
                  DataBand.FGroup := TfrxGroupHeader(LastObj);
                  LastObj.Top := DataBand.Top ;
                end
             end
           else if (Val = 'rbGroupFooter') then
             begin
                if DataBand <> nil then
                begin
                  LastObj.Top := DataBand.Top + DataBand.Height;
                end
             end

           else if PropName = 'Top' then
              begin
                LastObj.Top := Val
              end

           else if PropName = 'ForceNewPage' then
               begin
                  TfrxBand(LastObj).StartNewPage:= Val;
               end

           else if (PropName = 'ParentBand') and ((ClassName = 'TQRChildBand')
                or (ClassName = 'TQRPChildBand'))
              then
               begin
                  //FindBand := AReport.FindObject(Val);
                  //TfrxBand(FindBand).Child := TfrxChild(LastObj);
                  ParentBands.CommaText := ParentBands.CommaText
                                          + LastObj.Name + '=' + Val+',';
               end
           else if (PropName = 'PrintCount') and (ClassName = 'TQRLoopBand')
              then
               begin
                  TfrxMasterData(LastObj).RowCount := Val;
               end
           else if (PropName = 'Expression') and( (ClassName = 'TQRPBand')
                or (ClassName = 'TQRDesignSubdetail')) and (LastObj is TfrxDataBand)
              then
               begin
                  if (Val = '') then TfrxDataBand(LastObj).Filter :=''
                  else
                  begin
                  TfrxDataBand(LastObj).Filter := ReplaceExpr(Parent.Name,
                                                 LastObj.Name,Val,False,False);
                  TfrxDataBand(LastObj).Filter :=
                        TfrxDataBand(LastObj).Filter.Substring(1,
                        TfrxDataBand(LastObj).Filter.Length-2)
                  end;
               end
           else if (PropName = 'DataSet') then
              begin
                if ClassName.Substring(0,9) = 'TQRDesign' then
                begin
                  if not (AReport.DataSets.Find(
                      string(Val).Substring(Pos('.',Val))) = nil) then

                    TfrxDataBand(LastObj).DataSetName:=
                      string(Val).Substring(Pos('.',Val))

                  else
                    TfrxDataBand(LastObj).DataSetName:= Val;
                end

                else
                  TfrxDataBand(LastObj).DataSetName:= Val
              end;
         end

   else if (ClassName = 'TQRLabel')
        or (ClassName = 'TQRDBText')
        or (ClassName = 'TQRGroup')
        or (ClassName = 'TQRMemo')
        or (ClassName = 'TQRExpr')
        or (ClassName = 'TQRSysData')
        or (ClassName = 'TQRDBCalc')
        or (ClassName = 'TQRHTMLLabel')

        or (ClassName = 'TQRDesignLabel')
        or (ClassName = 'TQRDesignExpr')
        or (ClassName = 'TQRDesignDBText')
        or (ClassName = 'TQRDesignSysdata')

        or (ClassName = 'TQRPLabel')
        or (ClassName = 'TQRPDBText')
        or (ClassName = 'TQRPExpr')
        or (ClassName = 'TQRPMemo')
        or (ClassName = 'TPIQRMemo')
        then
        begin
          AssignView;
          if(LastObj is  TfrxMemoView) then
          AssignMemo;
        end
   else if ((ClassName = 'TQRShape') or (ClassName = 'TQRPShape') )
        or (ClassName = 'TQRDesignShape')
        and (LastObj is TfrxShapeView) then
   begin
      AssignView;
      AssignShape;
   end
   else if (ClassName = 'TQRImage')
        or (ClassName = 'TQRDBImage')
        or (ClassName = 'TQRGraphicCanvas')
        or (ClassName = 'TQRGrImage')
        or (ClassName = 'TQRGrDBImage')

		or (ClassName = 'TQRDesignImage')
		or (ClassName = 'TQRDesignDBImage')

        or (ClassName = 'TQRDBJPGlmage')
        or (ClassName = 'TQRPDBlmage')
        then
   begin
     AssignView;
     AssignPicture;
   end
   else if (ClassName = 'TQRRichText')
		or (ClassName = 'TQRDBRichText')
        or (ClassName = 'TQRPRichtext')

		or (ClassName = 'TQRDesignRichtext')
		or (ClassName = 'TQRDesignDBRichtext')
		then
   begin
     AssignView;
     AssignRich;
   end
   else if (ClassName = 'TQRQRBarcode') or (ClassName = 'TQRQRDBBarcode')
          or (ClassName = 'TQRDMBarcode') or (ClassName = 'TQRDbDMBarcode')then
   begin
     AssignView;
     AssignBarcodeView;
   end
   else if (ClassName = 'TQRFrameline') then
   begin
     AssignView;
     LastObj.Anchors := [fraLeft,fraTop,fraBottom];
     LastObj.Top := 0;
     LastObj.Height := Parent.Height;
   end
   else if (ClassName = 'TQRLineGraph') then
   begin
     AssignView;
   end
   else if (ClassName = 'TDataSource') then
   begin
      //AssignView;
      AssignDataSource;
   end
   else if (ClassName = 'TADOQuery')
        or (ClassName = 'TQRDQuery')
        or (ClassName = 'TQuery')
        or (ClassName = 'TFDQuery')
        or (ClassName = 'TwwQuery') then
   begin
      AssignView;
      AssignADOQuery;
   end
   else if (ClassName = 'TADOTable')
        or (ClassName = 'TQRDTable')
        or (ClassName = 'TTable')
        or (ClassName = 'TFDTable')
        or (ClassName = 'TwwTable') then
   begin
      AssignView;
      AssignADOTable;
   end
 end;

 procedure ConvertBinary;
  var
    Count: Longint;
    Stream: TMemoryStream;
  begin
    Reader.ReadValue;
    Reader.Read(Count, SizeOf(Count));
    Stream := TMemoryStream.Create;
    Stream.SetSize(Count);
    Reader.Read(Stream.Memory^, Count);
    Val := frxInteger(Stream);
  end;

 procedure ReadProperty; forward;

 procedure ConvertValue;
  var
    L: Integer;
    S: string;
    W: WideString;
  begin
    case Reader.NextValue of
      vaList:
        begin
          Reader.ReadValue;
          while not Reader.EndOfList do
          begin
            ConvertValue;
          end;
          Reader.ReadListEnd;
          exit;
        end;
      vaInt8, vaInt16, vaInt32:
        Val := IntToStr(Round(Reader.ReadInteger * PageZoom));
      vaExtended:
        Val := FloatToStrF(Reader.ReadFloat * PageZoom, ffFixed, 16, 18);
      vaSingle:
        Val := FloatToStr(Reader.ReadSingle * PageZoom) + 's';
      vaCurrency:
        Val := FloatToStr(Reader.ReadCurrency * PageZoom * 10000) + 'c';
      vaDate:
        Val := FloatToStr(Reader.ReadDate) + 'd';
      vaWString, vaUTF8String:
        begin
          W := Reader.ReadWideString;
          L := Length(W);
          if L = 0 then W :=  '';
          Val := W;
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          L := Length(S);
          if L = 0 then S :=  '';
          Val := S;
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        Val := Reader.ReadIdent;
      vaBinary:
        begin
          isBin := True;
          ConvertBinary;
        end;
      vaSet:
        begin
          Reader.ReadValue;

          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then exit;
            Val := S;
            AssignProp;
          end;
        end;
      vaCollection:
        begin
          Reader.ReadValue;
          while not Reader.EndOfList do
          begin
            if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
            begin
              ConvertValue;
            end;
            Reader.CheckValue(vaList);
            while not Reader.EndOfList do ReadProperty;
            Reader.ReadListEnd;

          end;
          Reader.ReadListEnd;
        end;
      vaInt64:
        Val := IntToStr(Round(Reader.ReadInt64 * PageZoom));
    end;
      AssignProp;
  end;


procedure ReadProperty;
begin
   PropName := Reader.ReadStr;
   ConvertValue;
end;

procedure ReadObject;
  var
    LastParent: TfrxComponent;
    Band : TfrxBand;
    Shape : TfrxShapeView;
  begin
    Reader.ReadPrefix(Flags, Position);
    if (ffInherited in Flags) or(ffInline in Flags)  then exit;
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;

    ObjectCreator(ClassName);

      if (ClassName = 'TQRDesignBand') or  (ClassName = 'TQRBand')
          or (ClassName = 'TQRPBand') or (ClassName = 'TQRDesignSubdetail')
          then
       begin
            Band := TfrxBand.Create(Parent);
            LastObj := Band;

            while not Reader.EndOfList do
            begin
              ReadProperty;
              if PropName = 'BandType' then
              begin
                  BandCreator(Val);
                  LastObj.AssignAll(Band);
                  try
                     LastObj.Name := ObjectName;
                  except
                    on EDuplicateName do LastObj.CreateUniqueName;
                  end;
                  FreeAndNil(Band);
              end;

              if ((PropName = 'Master') and (Val = 'Owner')) then
              begin
                BandCreator('rbDetail');
                LastObj.AssignAll(Band);
                try
                  LastObj.Name := ObjectName;
                except
                  on EDuplicateName do
                    LastObj.CreateUniqueName;
                end;
                FreeAndNil(Band);
              end
              else if (PropName = 'Master') then
              begin
                BandCreator('rbSubDetail');
                LastObj.AssignAll(Band);
                try
                  LastObj.Name := ObjectName;
                except
                  on EDuplicateName do
                    LastObj.CreateUniqueName;
                end;
                FreeAndNil(Band);
              end;

              if isBin then
              begin
                TMemoryStream(frxInteger(Val)).Free;
                isBin := False;
              end;
            end;
       end;

        if (ClassName = 'TQRDesignGroup') or  (ClassName = 'TQRGroup') then
       begin
            LastObj := TfrxGroupHeader.Create(Parent);

            while not Reader.EndOfList do
            begin
              ReadProperty;
              if PropName = 'Expression' then
              begin
               TfrxGroupHeader(LastObj).Condition:=ReplaceExpr(ObjectName,
                                                  'Condition',Val,True,False);
               LastObj.Name := ObjectName;
              end;

              if isBin then
              begin
                TMemoryStream(frxInteger(Val)).Free;
                isBin := False;
              end;
            end;
         end;

       if (ClassName = 'TQRSubDetail') or (ClassName = 'TQRDesignSubdetail')
        or (ClassName = 'TQRChildBand') or (ClassName = 'TQRPChildBand')
        or (ClassName = 'TQRLoopBand') then
       begin
            if (ClassName = 'TQRSubDetail') then
              LastObj := TfrxDetailData.Create(Parent)
            else if (ClassName = 'TQRChildBand')
                  or (ClassName = 'TQRPChildBand') then
              LastObj := TfrxChild.Create(Parent)
            else if (ClassName = 'TQRLoopBand') then
              LastObj := TfrxMasterData.Create(Parent);



            while not Reader.EndOfList do
            begin
              ReadProperty;
              LastObj.Name := ObjectName;

              if isBin then
              begin
                TMemoryStream(frxInteger(Val)).Free;
                isBin := False;
              end;
            end;
       end;

       if (ClassName = 'TQRDesignShape') or  (ClassName = 'TQRShape')
            or (ClassName = 'TQRPShape') then
       begin

            Shape  := TfrxShapeView.Create(nil);
            //Shape.Name := ObjectName;
            LastObj := Shape;


            while not Reader.EndOfList do
            begin
              ReadProperty;

              if (PropName = 'Shape') then
              begin
                  ShapeCreator(Val);
                  LastObj.AssignAll(Shape);

                  if  (Val = 'qrsTopAndBottom') or (Val = 'qrpsTopAndBottom')
                  then
                    begin
                       LastObj.Height := 0;
                       LastObj := TfrxLineView.Create(Parent);
                       LastObj.CreateUniqueName;
                       LastObj.AssignAll(Shape);
                       LastObj.Top := LastObj.Top + LastObj.Height;
                       LastObj.Height := 0;
                    end;
                  if  (Val = 'qrsRightAndLeft') or (Val = 'qrpsRightAndLeft')
                   then
                    begin
                       LastObj.Width:=0;
                       LastObj := TfrxLineView.Create(Parent);
                       LastObj.CreateUniqueName;
                       LastObj.AssignAll(Shape);
                       LastObj.Left:= LastObj.Left + LastObj.Width;
                       LastObj.Width:=0;
                       TfrxLineView(LastObj).Diagonal := true
                    end;
                  if  (Val = 'qrsLeftDiagonal') or (Val = 'qrsRightDiagonal')
                    or (Val = 'qrpsLeftDiagonal') or (Val = 'qrpsRightDiagonal')
                  then
                    begin
                       TfrxLineView(LastObj).Diagonal := true;
                    end;

                 FreeAndNil(Shape);

              end;

              try
               if ObjectName = '' then
                  LastObj.CreateUniqueName()
               else
                  LastObj.Name := ObjectName+'_1';
              except
                LastObj.Name := ObjectName+'_1';
              end;

              if isBin then
              begin
                TMemoryStream(frxInteger(Val)).Free;
                isBin := False;
              end;
            end;
       end;
    LastParent := LastObj;

    while not Reader.EndOfList do
    begin
      ReadProperty;
      if isBin then
      begin
        TMemoryStream(frxInteger(Val)).Free;
        isBin := False;
      end;
    end;
    if (LastObj <> nil) and (LastObj.Parent <> nil) and not (LastObj.Parent is TfrxReport)  then
      LastObj := LastObj.Parent;
    Reader.ReadListEnd;
    while not Reader.EndOfList do
    begin
      Parent := LastParent;
      ReadObject;
    end;

    Reader.ReadListEnd;
  end;


/////////////////////////////////////////////////////

 begin
  ParentBands := TStringList.Create;
  DTDataSource := TDictionary<String, String>.Create;
  DMaster := TDictionary<String, String>.Create;
  CntPages := 0;
  Result := False;
  SetLength(Sig, 3);
  AStream.Position := 0;
  AStream.Read(Sig[1], 3);
  AStream.Position := 0;
  if Sig <> 'TPF' then exit;
  AReport.Clear;
  with TfrxDataPage.Create(AReport) do
  begin
    CreateUniqueName;
  end;
  Reader := TReader.Create(AStream, 4096);
{$IFDEF Delphi16}
  SaveSeparator := FormatSettings.DecimalSeparator;
{$ELSE}
  SaveSeparator := DecimalSeparator;
{$ENDIF}

  isBin  := False;
{$IFDEF Delphi16}
  FormatSettings.DecimalSeparator := '.';
{$ELSE}
  DecimalSeparator := '.';
{$ENDIF}

  try
    Reader.ReadSignature;
    Reader.ReadPrefix(Flags, Position);
    LastObj := nil;
    ReadObject;
    if Split = True then
      PSplitPages();
    Result := True;
  finally
    Reader.Free;
  end;

  AssignAllChildBands;
  FreeAndNil(ParentBands);
{$IFDEF Delphi16}
  FormatSettings.DecimalSeparator := SaveSeparator;
{$ELSE}
  DecimalSeparator := SaveSeparator;
{$ENDIF}
end;
{/LoadFromQR}

{ TfrxOpenFilterQR }

constructor TfrxOpenFilterQR.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FilterString := GetDescription;
  FVisibility := [fvDesignerFileFilter];
  Filter := tDFM;
  self.DefaultExt := '.dfm';
  self.FSplitPages := false;
end;

function TfrxOpenFilterQR.DoFilterProcessStream(aStream: TStream; ProcesssingObject: TObject): Boolean;
var
  Sig: AnsiString;
  TmpStream: TMemoryStream;
  Sender : TfrxReport;
  TmpBStream: TMemoryStream;
  strbegin : AnsiString;
  strend : AnsiString;
begin
  if (Report <> nil) then
  begin
      PageZoom := 1;
      strbegin := 'object displayfrm: Tdisplayfrm'+slinebreak;
      strend := slinebreak + 'end';
      TmpBStream := TMemoryStream.Create();
      TmpBStream.Write(strbegin[1], Length(strbegin));
      TmpBStream.CopyFrom(aStream,aStream.Size);
      TmpBStream.Write(strend[1], Length(strend));

      Sender := Report;
      SetLength(Sig, 6);
      aStream.Position := 0;
      aStream.Read(Sig[1], 6);
      aStream.Position := 0;
      Sender.FileName := FileName;

      if (Sig = 'inheri') then
      begin
        SetLength(Sig, 9);
        aStream.Position := 0;
        aStream.Read(Sig[1], 9);
        aStream.Position := 0;
      end;

      if (Sig = 'object') or (Sig = 'inherited') then
      begin
        TmpStream := TMemoryStream.Create;
        try
         TmpBStream.Position := 0;
         ObjectTextToBinary(TmpBStream, TmpStream);
         Result := LoadFromQR(Sender, TmpStream,FSplitPages);
        finally
          TmpStream.Free;
        end;
      end
      else
      Result := LoadFromQR(Sender, aStream,FSplitPages);
  end
  else Result := false;
end;

procedure  TfrxOpenFilterQR.SetOpenFilter(Value:TFilterType);
begin
  if (Value = tDFM) then
  begin
    self.DefaultExt := '.dfm';
    self.FOpenFilter := tDFM;
  end;

  if (Value = tQR2) then
  begin
    self.DefaultExt := '.qr2';
    self.FOpenFilter := tQR2;
  end;

end;

class function TfrxOpenFilterQR.GetDescription: String;
begin
   Result := 'Form or QR template (*.dfm or *.qr2)|*.dfm;*.qr2';
end;

function TfrxOpenFilterQR.GetFilterString: String;
begin
      FilterString := 'Form or QR template (*.dfm or *.qr2)|*.dfm;*.qr2';
  Result := Inherited GetFilterString;
end;

procedure TfrxOpenFilterQR.AssignFilter(Source: TfrxCustomIOTransport);
var
  lFilter: TfrxOpenFilterQR;
begin
  inherited;
  if Source is TfrxOpenFilterQR then
  begin
    FSplitPages := lFilter.FSplitPages;
  end;
end;

function TfrxOpenFilterQR.OpenFilter: Boolean;
begin
  Result := True;
end;

end.
