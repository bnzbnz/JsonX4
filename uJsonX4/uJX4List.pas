(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2025 Laurent Meyer JsonX3@ea4d.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
******************************************************************************)
unit uJX4List;

interface
uses
    System.Generics.Collections
  , Classes
  , SysUtils
  , RTTI
  , uJX4Object
  , zLib
  ;

type

  TJX4ListOfValues  = class(TList<TValue>)
  private
    FAdded:    TList<TValue>;
    FDeleted:  TList<TValue>;
  public
    constructor Create;
    destructor  Destroy; override;
    function    JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure   JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure   JSONClone(ADestList: TJX4ListOfValues; AOptions: TJX4Options = []);
    procedure   JSONMerge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options = []);

    class function  New: TJX4ListOfValues;
    class function  NewAdd(AValue: TValue): TJX4ListOfValues;
    class function  NewAddRange(const AValues: array of TValue): TJX4ListOfValues; overload;
    function        First: TValue;
    function        Last: TValue;
    function        IndexOfTValue(From: TValue): Integer;

    function        Clone(AOptions: TJX4Options = []): TJX4ListOfValues;
    procedure       Merge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options = []);

    function        SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = Nil; AZipIT: TCompressionLevel = clNone; AUseBOM: Boolean = False): Int64;

    property        EleAdded:    TList<TValue> read FAdded;
    property        EleDeleted:  TList<TValue> read FDeleted;
  end;

  TJX4ValList = class(TJX4ListOfValues);
  TJX4ValLst  = class(TJX4ListOfValues);

  TJX4List<T: class, constructor> = class(TObjectList<T>)
  private
    FAdded:    TStringList;
    FModified: TStringList;
    FDeleted:  TStringList;
  public
    constructor Create;
    destructor  Destroy; override;

    function    JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure   JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure   JSONClone(ADestList: TJX4List<T>; AOptions: TJX4Options = []);
    procedure   JSONMerge(AMergedWith: TJX4List<T>; AOptions: TJX4Options);

    class function  New: TJX4List<T>;
    class function  NewAdd(AValue: T): TJX4List<T>;
    class function  NewAddRange(const AValues: array of T): TJX4List<T>; overload;
    function        First:T;
    function        Last: T;

    function        Clone<V:class, constructor>(AOptions: TJX4Options = []): V; overload;

    function        SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = Nil; AZipIT: TCompressionLevel = clNone; AUseBOM: Boolean = False): Int64;

    property       EleAdded:    TStringList read FAdded;
    property       EleModified: TStringList read FModified;
    property       EleDeleted:  TStringList read FDeleted;
  end;

   TJX4Lst<V:class, constructor> = class(TJX4List<V>);

implementation
uses
    Generics.Defaults
  , uJX4Rtti
  , uJX4Value
  , JSON
  , System.TypInfo
  ;

{ TJX4ListOfValues }

constructor TJX4ListOfValues.Create;
begin
  inherited Create;
  FAdded :=  TList<TValue>.Create;
  FDeleted := TList<TValue>.Create;
end;

destructor TJX4ListOfValues.Destroy;
begin
  FreeAndNil(FAdded);
  FreeAndNil(FDeleted);
  inherited Destroy;
end;

function TJX4ListOfValues.Clone(AOptions: TJX4Options): TJX4ListOfValues;
begin
  try
    Result := TJX4ListOfValues.Create;
    TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
  except
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

procedure TJX4ListOfValues.JSONClone(ADestList: TJX4ListOfValues; AOptions: TJX4Options);
var
  LList: TValue;
begin
  ADestList.Clear;
  for LList in Self do
    ADestList.Add(LList);
end;

procedure TJX4ListOfValues.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LEle:       TJSONValue;
  LIOBlock:   TJX4IOBlock;
  LJObj:      TJSONObject;
  LTValue:    TValue;
begin
  if not Assigned(AIOBlock.JObj) then begin Clear; Exit; end;
  if AIOBlock.JObj.Count = 0 then begin Clear; Exit end;;
  if not Assigned(AIOBlock.JObj.Pairs[0].JsonValue) then begin Clear; Exit end;
  if AIOBlock.JObj.Pairs[0].JsonValue.Null then begin Clear; Exit end;;
  if not (AIOBlock.JObj.Pairs[0].JsonValue is TJSONArray) then begin Clear; Exit end;

  LIOBlock := TJX4IOBlock.Create;
  for LEle in TJSONArray(AIOBlock.JObj.Pairs[0].JsonValue) do
  begin
    if LELe is TJSONObject then
    begin
      LJObj :=  LEle as TJSONObject;
      LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options, AIOBlock.PAbort);
      LTValue.JSONDeserialize(LIOBlock);
      Add(LTValue);
    end else begin
      LEle.Owned := False;
      LJObj :=  TJSONObject.Create(TJSONPair.Create('', LEle));
      LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options, AIOBlock.PAbort);
      LTValue.JSONDeserialize(LIOBlock);
      Add(LTValue);
      LJObj.Free;;
      LEle.Owned := True;
    end;
  end;
  LIOBlock.Free;
end;

function TJX4ListOfValues.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TList<string>;
  LRes:       string;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LNameAttr:  TCustomAttribute;
  LEle:       TValue;
  LTValue:    TValue;
begin
  Result := TValue.Empty;

  LName := AIOBlock.JsonName;
  if Assigned(AIOBlock) and Assigned(AIOBlock.Field) then
  begin
    LNameAttr := TJX4Name(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Name));
    if Assigned(LNameAttr) then LName := TJX4Name(LNameAttr).Name;
  end;
  LName := TJX4Object.NameDecode(LName);

  if Count = 0 then
  begin
    if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
      raise Exception.Create(Format('"%s" (TJX4ListValue) : a value is required', [LName]));
    if joNullToEmpty in AIOBlock.Options then Exit;
    if LName.IsEmpty then
      Result :=  '[]'
    else
      Result := '"' + LName + '":null';
    Exit;
  end;

  LParts := TList<string>.Create;
  LParts.Capacity := Self.Count;
  LIOBlock := TJX4IOBlock.Create;
  try
    for LEle in Self do
    begin
      if Assigned(AIOBlock.PAbort) and AIOBlock.PAbort^ then Exit;
      LIOBlock.Init('', Nil, Nil, AIOBlock.Options, AIOBlock.PAbort);
      LTValue := LEle.JSONSerialize(LIOBlock);
      if not LTValue.IsEmpty then LParts.Add(LTValue.AsString);
    end;
    LRes := TJX4Object.JsonListToJsonString(LParts);
    if LName.IsEmpty then
      Result := '[' + LRes + ']'
    else
      Result := '"' + LName + '":[' + LRes + ']';
  finally
    LIOBlock.Free;
    LParts.Free;
  end;
end;

function TJX4ListOfValues.First: TValue;
begin
  Result := Self[0];
end;

function TJX4ListOfValues.Last: TValue;
begin
  Result := Self[Count - 1];
end;

class function TJX4ListOfValues.New: TJX4ListOfValues;
begin
  Result := TJX4ListOfValues.Create;
end;

class function TJX4ListOfValues.NewAdd(AValue: TValue): TJX4ListOfValues;
begin
  Result := New;
  Result.Add(AValue);
end;

class function TJX4ListOfValues.NewAddRange(const AValues: array of TValue): TJX4ListOfValues;
begin
  Result := New;
  Result.AddRange(AValues);
end;

function TJX4ListOfValues.SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = Nil; AZipIT: TCompressionLevel = clNone; AUseBOM: Boolean = False): Int64;
begin
  if not Assigned(AEncoding) then AEncoding := TEncoding.UTF8;
  Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self, AOptions), AEncoding, AZipIT, AUseBOM);
end;

procedure TJX4ListOfValues.Merge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options);
begin
  try
    TxRTTI.CallMethodProc('JSONMerge', Self, [ AMergedWith, TValue.From<TJX4Options>(AOptions) ]);
  except
    on Ex: Exception do
    begin
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

{ TJX4List<T> }

constructor TJX4List<T>.Create;
  var
  LFields:    TArray<TRttiField>;
  LField:     TRTTIField;
  LNewObj:    TObject;
begin
  inherited Create(True);
  FAdded :=  TStringList.Create;
  FAdded.Duplicates := dupIgnore;
  FModified := TStringList.Create;
  FModified.Duplicates := dupIgnore;
  FDeleted := TStringList.Create;
  FDeleted.Duplicates := dupIgnore;
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
  begin
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      if not Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then
      begin
        LNewObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
        if not Assigned(LNewObj) then Continue;
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        LField.SetValue(Self, LNewObj);
      end else begin
        LField.SetValue(Self, Nil);
      end;
    end;
  end;
end;

destructor TJX4List<T>.Destroy;
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
begin
  FreeAndNil(FAdded);
  FreeAndNil(FModified);
  FreeAndNil(FDeleted);
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic, mvPublished]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then
      begin
        if TxRTTI.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then
          FreeAndNil(LObj);
      end else
        FreeAndNil(LObj);
    end;
  inherited;
end;

procedure TJX4List<T>.JSONClone(ADestList: TJX4List<T>; AOptions: TJX4Options);
var
  LNewObj: TObject;
  LList: TObject;
begin
  ADestList.Clear;
  if Count = 0 then Exit;
  for LList in Self do
  begin
    LNewObj := T.Create;
    TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
    TxRTTI.CallMethodProc('JSONClone', LList, [LNewObj, TValue.From<TJX4Options>(AOptions)]);
    ADestList.Add(LNewObj);
  end;
end;

procedure TJX4List<T>.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LEle:       TJSONValue;
  LNewObj:    TObject;
  LIOBlock:   TJX4IOBlock;
  LJObj:      TJSONObject;
begin
  if not Assigned(AIOBlock.JObj) then begin Clear; Exit; end;
  if AIOBlock.JObj.Count = 0 then begin Clear; Exit end;;
  if not Assigned(AIOBlock.JObj.Pairs[0].JsonValue) then begin Clear; Exit end;
  if AIOBlock.JObj.Pairs[0].JsonValue.Null then begin Clear; Exit end;
  if not (AIOBlock.JObj.Pairs[0].JsonValue is TJSONArray) then begin Clear; Exit end;

  LIOBlock := TJX4IOBlock.Create;
  try
    for LEle in TJSONArray(AIOBlock.JObj.Pairs[0].JsonValue) do
    begin
      if Assigned(AIOBlock.PAbort) and AIOBlock.PAbort^ then Exit;
      if LELe is TJSONObject then
      begin
        LNewObj := T.Create;
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        Add(LNewObj);
        LJObj :=  LEle as TJSONObject;
        LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options, AIOBlock.PAbort);
        TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ LIOBlock ] );
      end else begin
        LNewObj := T.Create;
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        Add(LNewObj);
        LEle.Owned := False;
        LJObj :=  TJSONObject.Create(TJSONPair.Create('', LEle));
        LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options, AIOBlock.PAbort);
        TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ LIOBlock ] );
        LJObj.Free;;
        LEle.Owned := True;
      end;
    end;
  finally
    LIOBlock.Free;
  end;
end;

procedure TJX4List<T>.JSONMerge(AMergedWith: TJX4List<T>; AOptions: TJX4Options);
var
  AList: TValue;
  LObj: T;
begin
  if AMergedWith.Count > 0 then
  begin
    for AList in AMergedWith do
    begin
      LObj := T.Create;
      TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
      TxRTTI.CallMethodProc('JSONMerge', LObj, [ AList, TValue.From<TJX4Options>(AOptions)]);
      Self.Add(LObj);
    end
  end;
end;

function TJX4List<T>.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TList<string>;
  LRes:       string;
  LEle:       T;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LNameAttr:  TCustomAttribute;
  LTValue:    TValue;
begin
  Result := TValue.Empty;
  LName := AIOBlock.JsonName;
  if Assigned(AIOBlock) and Assigned(AIOBlock.Field) then
  begin
    LNameAttr := TJX4Name(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Name));
    if Assigned(LNameAttr) then LName := TJX4Name(LNameAttr).Name;
  end;
  LName := TJX4Object.NameDecode(LName);

  if Count = 0 then
  begin
    if Assigned(AIOBlock.Field) and  Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
      raise Exception.Create(Format('"%s" (TJX4List) : a value is required', [LName]));
    if joNullToEmpty in AIOBlock.Options then Exit;
    if LName.IsEmpty then
      Result := '[]'
    else
      Result := '"' + LName + '":null';
    Exit;
  end;

  LParts := TList<string>.Create;
  LParts.Capacity := Self.Count;
  LIOBlock := TJX4IOBlock.Create;
  for LEle in Self do
  begin
    LIOBlock.Init('', Nil, Nil, AIOBlock.Options, AIOBlock.PAbort);
    LTValue := TxRTTI.CallMethodFunc('JSONSerialize', LEle, [ LIOBlock ]);
    if not LTValue.IsEmpty then LParts.Add(LTValue.AsString);
  end;
  LIOBlock.Free;
  LRes := TJX4Object.JsonListToJsonString(LParts);

  if LName.IsEmpty then
    Result := '[' + LRes + ']'
  else
    Result := '"' + LName + '":[' + LRes + ']';
  LParts.Free;
end;

function TJX4List<T>.Clone<V>(AOptions: TJX4Options): V;
begin
  try
    Result := V.Create;
    TxRTTI.CallMethodProc('JSONCreate', Result, [True]);
    TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
  except
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

function TJX4List<T>.First: T;
begin
  Result := Self[0];
end;

function TJX4List<T>.Last: T;
begin
  Result := Self[Count - 1];
end;

class function TJX4List<T>.New: TJX4List<T>;
begin
  Result := TJX4List<T>.Create;
end;

class function TJX4List<T>.NewAdd(AValue: T): TJX4List<T>;
begin
  Result := TJX4List<T>.Create;
  Result.Add(AValue);
end;

class function TJX4List<T>.NewAddRange(const AValues: array of T): TJX4List<T>;
begin
  Result := TJX4List<T>.Create;
  Result.AddRange(AValues);
end;

function TJX4ListOfValues.IndexOfTValue(From: TValue): Integer;
begin
  Result := -1;
  for var i := 0 to Self.count - 1  do
  begin
    case Self.Items[i].TypeKind of
      tkvString:
        if CompareText(From.AsString, Self.Items[i].AsString) = 0 then
        begin
          Result := i;
          Break;
        end;
      tkvInteger:
        if (From.AsInt64 = Self.Items[i].AsInt64) then
        begin
          Result := i;
          Break;
        end;
      tkvFloat:
        if (From.AsExtended = Self.Items[i].AsExtended) then
        begin
          Result := i;
          Break;
        end;
    end;
  end;
end;

procedure TJX4ListOfValues.JSONMerge(AMergedWith: TJX4ListOfValues;
  AOptions: TJX4Options);
begin
  if AMergedWith.Count = 0  then Exit;
  if (jmoStats in AOptions) then
  begin
    FAdded.Clear;
    FDeleted.Clear;
  end;
  for var i := AMergedWith.Count  - 1 downto 0 do
  begin
    var LIdx := Self.IndexOfTValue(AMergedWith.Items[i]);
    if (jmoAdd in AOptions)  and (LIdx = -1)  then
    begin
      if (jmoStats in AOptions) then FAdded.Add(AMergedWith.Items[i]);
      Self.Add(AMergedWith.Items[i]);
    end
    else if (jmoDelete in AOptions)  and (LIdx <> -1)  then
    begin
      if (jmoStats in AOptions) then FDeleted.Add(Self[LIdx]);
      Self.Delete(LIdx);
    end;
  end;
end;

function TJX4List<T>.SaveToJSONFile(const AFilename: string; AOptions: TJX4Options; AEncoding: TEncoding; AZipIT: TCompressionLevel; AUseBOM: Boolean): Int64;
begin
  if not Assigned(AEncoding) then AEncoding := TEncoding.UTF8;
  Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self, AOptions), AEncoding, AZipIt, AUseBOM);
end;


end.
