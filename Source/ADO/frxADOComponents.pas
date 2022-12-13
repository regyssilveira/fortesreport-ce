
{******************************************}
{                                          }
{             FastReport VCL               }
{         ADO enduser components           }
{                                          }
{         Copyright (c) 1998-2021          }
{            by Fast Reports Inc.          }
{                                          }
{******************************************}

unit frxADOComponents;

interface

{$I frx.inc}

uses
  Windows, Classes, SysUtils, frxClass, frxCustomDB
{$IFDEF DELPHI16}
, Data.DB, Data.Win.ADODB, Winapi.ADOInt
{$ELSE}
, DB, ADODB, ADOInt
{$ENDIF}
, Variants
{$IFDEF QBUILDER}
, fqbClass
{$ENDIF}
;


type
{$IFDEF DELPHI16}
[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  /// <summary>
  ///   The TfrxADOComponents component allows the use of internal ADO data
  ///   objects in your report.
  /// </summary>
  TfrxADOComponents = class(TfrxDBComponents)
  private
    FDefaultDatabase: TADOConnection;
    FOldComponents: TfrxADOComponents;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDefaultDatabase(Value: TADOConnection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String; override;
  published
    /// <summary>
    ///   Link to the default database connection that will be used in the
    ///   report.
    /// </summary>
    property DefaultDatabase: TADOConnection read FDefaultDatabase write SetDefaultDatabase;
  end;

  /// <summary>
  ///   The TfrxADODatabase component represents a database connection.
  /// </summary>
  TfrxADODatabase = class(TfrxCustomDatabase)
  private
    FDatabase: TADOConnection;
  protected
    procedure SetConnected(Value: Boolean); override;
    procedure SetDatabaseName(const Value: String); override;
    procedure SetLoginPrompt(Value: Boolean); override;
    function GetConnected: Boolean; override;
    function GetDatabaseName: String; override;
    function GetLoginPrompt: Boolean; override;
    procedure ADOBeforeConnect(Sende: TObject);
    procedure ADOAfterDisconnect(Sende: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
    procedure SetLogin(const Login, Password: String); override;
    function ToString: WideString; override;
    procedure FromString(const Connection: WideString); override;
    /// <summary>
    ///   Reference to internal TADOConnection object.
    /// </summary>
    property Database: TADOConnection read FDatabase;
  published
    property DatabaseName;
    property LoginPrompt;
    property Connected;
  end;

  /// <summary>
  ///   The TfrxADOTable represents a table.
  /// </summary>
  TfrxADOTable = class(TfrxCustomTable)
  private
    FDatabase: TfrxADODatabase;
    FTable: TADOTable;
    procedure SetDatabase(Value: TfrxADODatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetMasterFields(const Value: String); override;
    procedure SetIndexFieldNames(const Value: String); override;
    procedure SetIndexName(const Value: String); override;
    procedure SetTableName(const Value: String); override;
    function GetIndexFieldNames: String; override;
    function GetIndexName: String; override;
    function GetTableName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: String; override;
    procedure BeforeStartReport; override;
    /// <summary>
    ///   Reference to internal TADOTable object.
    /// </summary>
    property Table: TADOTable read FTable;
  published
    /// <summary>
    ///   Link to the database connection object. If this property is nil,
    ///   TfrxADOComponents.DefaultDatabase is used.
    /// </summary>
    property Database: TfrxADODatabase read FDatabase write SetDatabase;
  end;

  /// <summary>
  ///   The TfrxADOQuery component represents a query.
  /// </summary>
  TfrxADOQuery = class(TfrxCustomQuery)
  private
    FDatabase: TfrxADODatabase;
    FQuery: TADOQuery;
    FStrings: TStrings;
    FLock: Boolean;
    procedure SetDatabase(Value: TfrxADODatabase);
    function GetCommandTimeout: Integer;
    procedure SetCommandTimeout(const Value: Integer);
    function GetLockType: TADOLockType;
    procedure SetLockType(const Value: TADOLockType);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnChangeSQL(Sender: TObject); override;
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetSQL(Value: TStrings); override;
    function GetSQL: TStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    procedure BeforeStartReport; override;
    procedure UpdateParams; override;
{$IFDEF QBUILDER}
    function QBEngine: TfqbEngine; override;
{$ENDIF}
    /// <summary>
    ///   Reference to internal TADOQuery object.
    /// </summary>
    property Query: TADOQuery read FQuery;
  published
    /// <summary>
    ///   Use CommandTimeout to specify the amount of time that expires before
    ///   an attempt to execute a command is considered unsuccessful. Measured
    ///   in seconds, the default value for CommandTimeout is 30.
    /// </summary>
    property CommandTimeout: Integer read GetCommandTimeout write SetCommandTimeout;
    /// <summary>
    ///   Link to the database connection object. If this property is nil,
    ///   TfrxADOComponents.DefaultDatabase is used.
    /// </summary>
    property Database: TfrxADODatabase read FDatabase write SetDatabase;
    /// <summary>
    ///   Lock type.
    /// </summary>
    property LockType: TADOLockType read GetLockType write SetLockType;
  end;

{$IFDEF QBUILDER}
  TfrxEngineADO = class(TfqbEngine)
  private
    FQuery: TADOQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadTableList(ATableList: TStrings); override;
    procedure ReadFieldList(const ATableName: string; var AFieldList: TfqbFieldList); override;
    function ResultDataSet: TDataSet; override;
    procedure SetSQL(const Value: string); override;
  end;
{$ENDIF}

procedure frxParamsToTParameters(Query: TfrxCustomQuery; Params: TParameters);
procedure frxADOGetTableNames(conADO: TADOConnection; List: TStrings; SystemTables: Boolean);

var
  ADOComponents: TfrxADOComponents;

implementation

uses
  frxADORTTI,
{$IFNDEF NO_EDITORS}
  frxADOEditor,
{$ENDIF}
  frxDsgnIntf, frxRes;

type
  THackQuery = class(TADOQuery);


{ frxParamsToTParameters }

procedure frxParamsToTParameters(Query: TfrxCustomQuery; Params: TParameters);
var
  i: Integer;
  Item: TfrxParamItem;
begin
  for i := 0 to Params.Count - 1 do
    if Query.Params.IndexOf(Params[i].Name) <> -1 then
    begin
      Item := Query.Params[Query.Params.IndexOf(Params[i].Name)];
      Params[i].DataType := Item.DataType;
      Params[i].Attributes := [paNullable];
      if Trim(Item.Expression) <> '' then
        if not (Query.IsLoading or Query.IsDesigning) then
        begin
          Query.Report.CurObject := Query.Name;
          Item.Value := Query.Report.Calc(Item.Expression);
        end;
      Params[i].Value := Item.Value;
    end;
end;

procedure frxADOGetTableNames(conADO: TADOConnection; List: TStrings; SystemTables: Boolean);
var
  tbl: TADODataSet;
  s: string;
begin
  tbl := TADODataSet.Create(nil);
  List.Clear;
  try
    conADO.OpenSchema(siTables, EmptyParam, EmptyParam, tbl);
    tbl.First;
    while not tbl.Eof do
    begin
      s := Trim(tbl.FieldByName('TABLE_SCHEMA').AsString);
      if s <> '' then
        List.Add(s + '.' + tbl.FieldByName('TABLE_NAME').AsString)
      else
        List.Add(tbl.FieldByName('TABLE_NAME').AsString);
      tbl.Next;
    end;
  finally
    tbl.Free;
  end;
end;

{ TfrxDBComponents }

constructor TfrxADOComponents.Create(AOwner: TComponent);
begin
  inherited;
  FOldComponents := ADOComponents;
  FDefaultDatabase := nil;
  ADOComponents := Self;
end;

destructor TfrxADOComponents.Destroy;
begin
  if ADOComponents = Self then
    ADOComponents := FOldComponents;
  inherited;
end;

function TfrxADOComponents.GetDescription: String;
begin
  Result := 'ADO';
end;

procedure TfrxADOComponents.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDefaultDatabase) and (Operation = opRemove) then
    FDefaultDatabase := nil;
end;


{ TfrxADODatabase }

constructor TfrxADODatabase.Create(AOwner: TComponent);
begin
  inherited;
  FDatabase := TADOConnection.Create(nil);
  FDatabase.BeforeConnect := ADOBeforeConnect;
  FDatabase.AfterDisconnect := ADOAfterDisconnect;
  Component := FDatabase;
end;

class function TfrxADODatabase.GetDescription: String;
begin
  Result := frxResources.Get('obADODB');
end;

function TfrxADODatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TfrxADODatabase.GetDatabaseName: String;
begin
  Result := FDatabase.ConnectionString;
end;

function TfrxADODatabase.GetLoginPrompt: Boolean;
begin
  Result := FDatabase.LoginPrompt;
end;

procedure TfrxADODatabase.SetConnected(Value: Boolean);
begin
  BeforeConnect(Value);
  FDatabase.Connected := Value;
end;

procedure TfrxADODatabase.SetDatabaseName(const Value: String);
begin
  FDatabase.ConnectionString := Value;
end;

procedure TfrxADODatabase.SetLoginPrompt(Value: Boolean);
begin
  FDatabase.LoginPrompt := Value;
end;

procedure TfrxADODatabase.SetLogin(const Login, Password: String);
var
  i, j: Integer;
  s: String;
begin
  s := DatabaseName;
  i := Pos('USER ID=', AnsiUppercase(s));
  if i <> 0 then
  begin
    for j := i to Length(s) do
      if s[j] = ';' then
        break;
    Delete(s, i, j - i);
    Insert('User ID=' + Login, s, i);
  end
  else
    s := s + ';User ID=' + Login;

  i := Pos('PASSWORD=', AnsiUppercase(s));
  if i <> 0 then
  begin
    for j := i to Length(s) do
      if s[j] = ';' then
        break;
    Delete(s, i, j - i);
    Insert('Password=' + Password, s, i);
  end
  else
    s := s + ';Password=' + Password;

  DatabaseName := s;
end;

procedure TfrxADODatabase.FromString(const Connection: WideString);
begin
  FDatabase.ConnectionString := Connection;
end;

function TfrxADODatabase.ToString: WideString;
begin
  Result := FDatabase.ConnectionString;
end;

procedure TfrxADODatabase.ADOBeforeConnect(Sende: TObject);
var
 Param: Boolean;
begin
  Param := True;
  BeforeConnect(Param);
end;

procedure TfrxADODatabase.ADOAfterDisconnect(Sende: TObject);
begin
 AfterDisconnect;
end;

procedure TfrxADOComponents.SetDefaultDatabase(Value: TADOConnection);
begin
  if (Value <> nil) then
    Value.FreeNotification(Self);

  if FDefaultDatabase <> nil then
      FDefaultDatabase.RemoveFreeNotification(Self);

  FDefaultDatabase := Value;
end;

{ TfrxADOTable }

constructor TfrxADOTable.Create(AOwner: TComponent);
begin
  FTable := TADOTable.Create(nil);
  DataSet := FTable;
  SetDatabase(nil);
  inherited;
end;

constructor TfrxADOTable.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxADODatabase then
    begin
      SetDatabase(TfrxADODatabase(l[i]));
      break;
    end;
end;

class function TfrxADOTable.GetDescription: String;
begin
  Result := frxResources.Get('obADOTb');
end;

procedure TfrxADOTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then
    SetDatabase(nil);
end;

procedure TfrxADOTable.SetDatabase(Value: TfrxADODatabase);
begin
  FDatabase := Value;
  if Value <> nil then
    FTable.Connection := Value.Database
  else if ADOComponents <> nil then
    FTable.Connection := ADOComponents.DefaultDatabase
  else
    FTable.Connection := nil;
  DBConnected := FTable.Connection <> nil;
end;

function TfrxADOTable.GetIndexFieldNames: String;
begin
  Result := FTable.IndexFieldNames;
end;

function TfrxADOTable.GetIndexName: String;
begin
  Result := FTable.IndexName;
end;

function TfrxADOTable.GetTableName: String;
begin
  Result := FTable.TableName;
end;

procedure TfrxADOTable.SetIndexFieldNames(const Value: String);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TfrxADOTable.SetIndexName(const Value: String);
begin
  FTable.IndexName := Value;
end;

procedure TfrxADOTable.SetTableName(const Value: String);
begin
  FTable.TableName := Value;
end;

procedure TfrxADOTable.SetMaster(const Value: TDataSource);
begin
  FTable.MasterSource := Value;
end;

procedure TfrxADOTable.SetMasterFields(const Value: String);
begin
  FTable.MasterFields := Value;
end;

procedure TfrxADOTable.BeforeStartReport;
begin
  SetDatabase(FDatabase);
end;


{ TfrxADOQuery }

constructor TfrxADOQuery.Create(AOwner: TComponent);
begin
  FStrings := TStringList.Create;
  FQuery := TADOQuery.Create(nil);
  FQuery.LockType := ltReadOnly;
  Dataset := FQuery;
  SetDatabase(nil);
  FLock := False;
  inherited;
end;

constructor TfrxADOQuery.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxADODatabase then
    begin
      SetDatabase(TfrxADODatabase(l[i]));
      break;
    end;
end;

destructor TfrxADOQuery.Destroy;
begin
  FStrings.Free;
  inherited;
end;

class function TfrxADOQuery.GetDescription: String;
begin
  Result := frxResources.Get('obADOQ');
end;

procedure TfrxADOQuery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then
    SetDatabase(nil);
end;

function TfrxADOQuery.GetSQL: TStrings;
begin
  FLock := True;
  FStrings.Assign(FQuery.SQL);
  FLock := False;
  Result := FStrings;
end;

procedure TfrxADOQuery.SetSQL(Value: TStrings);
begin
  FQuery.SQL.Assign(Value);
  FStrings.Assign(FQuery.SQL);
end;

procedure TfrxADOQuery.SetDatabase(Value: TfrxADODatabase);
begin
  FDatabase := Value;
  if Value <> nil then
    FQuery.Connection := Value.Database
  else if ADOComponents <> nil then
    FQuery.Connection := ADOComponents.DefaultDatabase
  else
    FQuery.Connection := nil;
  DBConnected := FQuery.Connection <> nil;
end;

procedure TfrxADOQuery.SetMaster(const Value: TDataSource);
begin
  FQuery.DataSource := Value;
end;

function TfrxADOQuery.GetCommandTimeout: Integer;
begin
  Result := THackQuery(FQuery).CommandTimeout;
end;

procedure TfrxADOQuery.SetCommandTimeout(const Value: Integer);
begin
  THackQuery(FQuery).CommandTimeout := Value;
end;

procedure TfrxADOQuery.UpdateParams;
begin
  frxParamsToTParameters(Self, FQuery.Parameters);
end;

procedure TfrxADOQuery.OnChangeSQL(Sender: TObject);
var
  i, ind: Integer;
  Param: TfrxParamItem;
  QParam: TParameter;
begin
  if not FLock then
  begin
    { needed to update parameters }
    FQuery.SQL.Text := '';
    FQuery.SQL.Assign(FStrings);
    inherited;

    { fill datatype automatically, if possible }
    for i := 0 to FQuery.Parameters.Count - 1 do
    begin
      QParam := FQuery.Parameters[i];
      ind := Params.IndexOf(QParam.Name);
      if ind <> -1 then
      begin
        Param := Params[ind];
        if (Param.DataType = ftUnknown) and (QParam.DataType <> ftUnknown) then
          Param.DataType := QParam.DataType
        { synchronize types when possible }
        else if (QParam.DataType = ftUnknown) and (Param.DataType <> ftUnknown) then
          QParam.DataType := Param.DataType
      end;
    end;
  end;
end;

procedure TfrxADOQuery.BeforeStartReport;
begin
  SetDatabase(FDatabase);
  { needed to update parameters }
  SQL.Text := SQL.Text;
end;

{$IFDEF QBUILDER}
function TfrxADOQuery.QBEngine: TfqbEngine;
begin
  Result := TfrxEngineADO.Create(nil);
  TfrxEngineADO(Result).FQuery.Connection := FQuery.Connection;
end;
{$ENDIF}

function TfrxADOQuery.GetLockType: TADOLockType;
begin
  Result := FQuery.LockType;
end;

procedure TfrxADOQuery.SetLockType(const Value: TADOLockType);
begin
  FQuery.LockType := Value;
end;


{$IFDEF QBUILDER}
constructor TfrxEngineADO.Create(AOwner: TComponent);
begin
  inherited;
  FQuery := TADOQuery.Create(Self);
end;

destructor TfrxEngineADO.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TfrxEngineADO.ReadFieldList(const ATableName: string;
  var AFieldList: TfqbFieldList);
var
  TempTable: TADOTable;
  Fields: TFieldDefs;
  i: Integer;
  tmpField: TfqbField;
begin
  AFieldList.Clear;
  TempTable := TADOTable.Create(Self);
  TempTable.Connection := FQuery.Connection;
  TempTable.TableName := ATableName;
  Fields := TempTable.FieldDefs;
  try
    try
      TempTable.Active := True;
      tmpField:= TfqbField(AFieldList.Add);
      tmpField.FieldName := '*';
      for i := 0 to Fields.Count - 1 do
      begin
        tmpField := TfqbField(AFieldList.Add);
        tmpField.FieldName := Fields.Items[i].Name;
        tmpField.FieldType := Ord(Fields.Items[i].DataType)
      end;
    except
    end;
  finally
    TempTable.Free;
  end;
end;

procedure TfrxEngineADO.ReadTableList(ATableList: TStrings);
begin
  ATableList.Clear;
  frxADOGetTableNames(FQuery.Connection, ATableList, ShowSystemTables);
//  FQuery.Connection.GetTableNames(ATableList, ShowSystemTables);
end;

function TfrxEngineADO.ResultDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TfrxEngineADO.SetSQL(const Value: string);
begin
  FQuery.SQL.Text := Value;
end;
{$ENDIF}


initialization
  frxObjects.RegisterObject1(TfrxADODataBase, nil, '', {$IFDEF DB_CAT}'DATABASES'{$ELSE}''{$ENDIF}, 0, 51);
  frxObjects.RegisterObject1(TfrxADOTable, nil, '', {$IFDEF DB_CAT}'TABLES'{$ELSE}''{$ENDIF}, 0, 52);
  frxObjects.RegisterObject1(TfrxADOQuery, nil, '', {$IFDEF DB_CAT}'QUERIES'{$ELSE}''{$ENDIF}, 0, 53);


finalization
  frxObjects.UnRegister(TfrxADODataBase);
  frxObjects.UnRegister(TfrxADOTable);
  frxObjects.UnRegister(TfrxADOQuery);

end.
