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

  TJSONableStringList = class(TStringList)
  private
    FIsManaged: Boolean;
  public
    procedure JSONCreate(AManaged: Boolean);
    function  JSONDestroy: Boolean;
    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure JSONClone(ADestObj: TObject; AOptions: TJX4Options);
    function  JSONMerge(AMergedWith: TStringList; AOptions: TJX4Options): TValue;
  end;

implementation
uses System.Generics.Collections;

procedure TJSONableStringList.JSONCreate(AManaged: Boolean);
begin
  FIsManaged := AManaged;  // AManaged : true if the object is created by the json engine.
end;

function TJSONableStringList.JSONDestroy: Boolean;
begin
  Result := FIsManaged; // send it back to the engine
end;

function TJSONableStringList.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
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

procedure TJSONableStringList.JSONDeserialize(AIOBlock: TJX4IOBlock);
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
procedure TJSONableStringList.JSONClone(ADestObj: TObject; AOptions: TJX4Options);
var
  AStr: string;
begin
  for AStr in Self do
    TJSONableStringList(ADestObj).Add(AStr);
end;

// Optional
function TJSONableStringList.JSONMerge(AMergedWith: TStringList; AOptions: TJX4Options): TValue;
var
  AStr: string;
begin
  Clear;
  for AStr in AMergedWith do
  begin
    Self.Add(AStr);
  end;
end;


end.
