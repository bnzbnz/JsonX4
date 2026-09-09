unit uJSONableStringList;

interface
uses
  Classes
  , JSON
  , RTTI
  , uJX4Object
  , SysUtils
  ;

type

  TJSONableStringListByRTTI = class(TStringList)
  private
    FIsManaged: Boolean;
  public
    procedure JSONCreate(AManaged: Boolean);
    function  JSONDestroy: Boolean;
    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure JSONClone(ADestObj: TObject; AOptions: TJX4Options);
    function  JSONMerge(AMergedWith: TStringList; AOptions: TJX4Options): TValue;
    procedure JSONClear(AOptions: TJX4Options = []);
  end;

  TJSONableStringListByIntf = class(TStringList, IJX4Jsonable)
  private
    FIsManaged: Boolean;
  protected
    { IInterface }
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
  public
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
    procedure JX4Clear(AOptions: TJX4Options = []);
  end;

implementation
uses System.Generics.Collections;

{ TJSONableStringListByRTTI }

procedure TJSONableStringListByRTTI.JSONCreate(AManaged: Boolean);
begin
  FIsManaged := AManaged;  // AManaged : true if the object is created by the json engine.
end;

function TJSONableStringListByRTTI.JSONDestroy: Boolean;
begin
  Result := FIsManaged; // send the Managed back to the engine
end;

function TJSONableStringListByRTTI.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LArr: TJSONArray;   // using std JSON libraries
  LStr: string;
begin
  // Custom serialization
  Result := Nil;
  if Count = 0 then Exit;
  LArr := TJSONArray.Create;
  for LStr in Self do
  begin
    TJX4Object.EscapeJSONStr(LStr, False); // String escape to JSON Format
    LArr.Add(LStr);
  end;
  if AIOBlock.JsonName.IsEmpty then
    Result := Format('%s', [LArr.ToJSON])
  else
    Result := Format('"%s":%s', [AIOBlock.JsonName, LArr.ToJSON]) ;
  LArr.Free;
end;

procedure TJSONableStringListByRTTI.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LArr: TJSONArray;
  LStr: TJSONValue;
begin
  // Custom deserialization
  Clear;
  LArr := AIOBlock.JObj.Pairs[0].JsonValue  as TJSONArray;
  for LStr in LArr do Self.Add(LStr.AsType<string>);
end;

// Optional
procedure TJSONableStringListByRTTI.JSONClone(ADestObj: TObject; AOptions: TJX4Options);
var
  AStr: string;
begin
  for AStr in Self do
    TJSONableStringListByRTTI(ADestObj).Add(AStr);
end;

// Optional
function TJSONableStringListByRTTI.JSONMerge(AMergedWith: TStringList; AOptions: TJX4Options): TValue;
var
  AStr: string;
begin
  Clear;
  for AStr in AMergedWith do
  begin
    Self.Add(AStr);
  end;
end;

// Optional
procedure TJSONableStringListByRTTI.JSONClear(AOptions: TJX4Options = []);
begin
  Clear;
end;

{ TJSONableStringListByIntf }

function TJSONableStringListByIntf.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;
function TJSONableStringListByIntf._AddRef: Integer; begin Result := -1; end;
function TJSONableStringListByIntf._Release: Integer; begin Result := -1; end;


function  TJSONableStringListByIntf.JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
var
  LArr: TJSONArray;   // using std JSON libraries
  LStr: string;
begin
  // Custom serialization
  Result := Nil;
  if Count = 0 then Exit;
  LArr := TJSONArray.Create;
  for LStr in Self do
  begin
    TJX4Object.EscapeJSONStr(LStr, False); // String escape to JSON Format
    LArr.Add(LStr);
  end;
  Result := Format('"%s":%s', [AIOBlock.JsonName, LArr.ToJSON]) ;
  LArr.Free;
end;

procedure TJSONableStringListByIntf.JX4Deserialize(AIOBlock: TJX4IOBlock);
var
  LArr: TJSONArray;
  LStr: TJSONValue;
begin
  // Custom deserialization
  Clear;
  LArr := AIOBlock.JObj.Pairs[0].JsonValue  as TJSONArray;
  for LStr in LArr do Self.Add(LStr.AsType<string>);
end;

procedure TJSONableStringListByIntf.JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
begin
  FIsManaged := AManaged;  // AManaged : true if the object is created by the json engine.
end;

procedure TJSONableStringListByIntf.JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
begin

end;

procedure TJSONableStringListByIntf.JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
var
  AStr: string;
begin
  TJSONableStringListByIntf(ADestObj).Clear;
  for AStr in Self do
    TJSONableStringListByIntf(ADestObj).Add(AStr);
end;

function  TJSONableStringListByIntf.JX4Destroy(AOptions: TJX4Options = []): Boolean;
begin
  Result := FIsManaged; // send the Managed back to the engine
end;

procedure TJSONableStringListByIntf.JX4Clear(AOptions: TJX4Options = []);
begin
  Clear;
end;

end.
