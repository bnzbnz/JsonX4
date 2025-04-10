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
*******************************************************************************)
unit uJX4Dict;

interface
uses
    System.Generics.Collections
  , RTTI
  , uJX4List
  , uJX4Object
  , Classes
  ;

type

  TJX4DictOfValues = class(System.Generics.Collections.TObjectDictionary<string, TValue>)
  public
    constructor     Create;
    destructor      Destroy; override;

    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure JSONClone(ADestDict: TJX4DictOfValues; AOptions: TJX4Options = []);
    procedure JSONMerge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options = []);

    class function New: TJX4DictOfValues;
    class function NewAdd(AKey: string; AValue: TValue): TJX4DictOfValues;
    class function NewAddRange(const AKeys: array of string; const AValues: array of TValue): TJX4DictOfValues;

    function       Clone(AOptions: TJX4Options): TJX4DictOfValues; overload;

  end;

  TJX4ValDic  = class(TJX4DictOfValues);
  TJX4ValDict = class(TJX4DictOfValues);

  TObjectDictionary<V> = class(System.Generics.Collections.TObjectDictionary<string,V>);
  TJX4Dict<V:class, constructor> = class(TObjectDictionary<V>)
  private
    FAdded:    TStringList;
    FModified: TStringList;
    FDeleted:  TStringList;
  public

    constructor Create;
    destructor  Destroy; override;

    function    JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure   JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure   JSONClone(ADestDict:  TJX4Dict<V>; AOptions: TJX4Options = []);
    procedure   JSONMerge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options = []);

    class function New: TJX4Dict<V>;
    class function NewAdd(AKey: string; AValue: V): TJX4Dict<V>;
    class function NewAddRange(const AKeys: array of string; const AValues: array of V): TJX4Dict<V>;
    function       GetValue<T>(const Key: string): T;

    function       Clone<T:class, constructor>(AOptions: TJX4Options = []): T; overload;
    procedure      Merge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options = []); overload;
    procedure      Merge(AMergedWith: TJX4ValList; AOptions: TJX4Options = []); overload;

    property       EleAdded:    TStringList read FAdded;
    property       EleModified: TStringList read FModified;
    property       EleDeleted:  TStringList read FDeleted;
  end;

  TJX4Dictionary<V:class, constructor> = class(TJX4Dict<V>);
  TJX4Dic<V:class, constructor> = class(TJX4Dict<V>);


implementation
uses
    uJX4Rtti
  , uJX4Value
  , SysUtils
  , JSON
  ;

{ TJX3DictOfValues }

function TJX4DictOfValues.Clone(AOptions: TJX4Options): TJX4DictOfValues;
begin
  try
    Result := TJX4DictOfValues.Create;
    TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
  except
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

constructor TJX4DictOfValues.Create;
begin
  inherited Create;
end;

destructor TJX4DictOfValues.Destroy;
begin
  inherited Destroy;
end;

function TJX4DictOfValues.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TList<string>;
  LRes:       string;
  Lkp:        TPair<string, TValue>;
  LTValue:    TValue;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LNameAttr:  TCustomAttribute;
begin
  Result := TValue.Empty;
  if Assigned(AIOBlock.Field) then
  begin
    LName := AIOBlock.Field.Name;
    LNameAttr := TJX4Name(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Name));
    if Assigned(LNameAttr) then LName := TJX4Name(LNameAttr).Name;
  end else
    LName := AIOBlock.JsonName;
  LName := TJX4Object.NameDecode(LName);

  if Count = 0 then
  begin
    if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
      raise Exception.Create(Format('"%s" (TJX3Dic) : a value is required', [LName]));

    if joNullToEmpty in AIOBlock.Options then Exit;

    if AIOBlock.JsonName.IsEmpty then
      Result := 'null'
    else
      Result := '"' + LName + '":null';
    Exit;
  end;

  LParts := TList<string>.Create;
  LParts.Capacity := Self.Count;
  LIOBlock := TJX4IOBlock.Create;
  for Lkp in Self do
  begin
      LIOBlock.Init(LKp.Key, Nil, Nil, AIOBlock.Options, AIOBlock.PAbort);
      LTValue := Lkp.Value.JSONSerialize(LIOBlock);
      if not LTValue.IsEmpty then LParts.Add(LTValue.AsString);
  end;
  LIOBlock.Free;
  LRes := TJX4Object.JsonListToJsonString(LParts);
  LParts.Free;

  if AIOBlock.JsonName.IsEmpty then
    Result := '{' + LRes + '}'
  else
    Result := '"' + LName + '":{'+ LRes + '}';
end;

procedure TJX4DictOfValues.JSONClone(ADestDict: TJX4DictOfValues; AOptions: TJX4Options = []);
var
  LPair:  TPair<string, TValue>;
begin
  ADestDict.Clear;
  for LPair in Self do
    ADestDict.Add(LPair.Key, LPair.Value);
end;

procedure TJX4DictOfValues.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LPair: TJSONPair;
  LNewObj: TValue;
  LIOBlock: TJX4IOBlock;
  LJObj: TJSONObject;
  LJObjDestroy: Boolean;
begin
  if not Assigned(AIOBlock.JObj) then begin Clear; Exit end;;
  if AIOBlock.JObj.Count = 0 then begin Clear; Exit end;
  if not Assigned(AIOBlock.JObj.Pairs[0].JsonValue) then begin Clear; Exit end;
  if AIOBlock.JObj.Pairs[0].JsonValue.Null then begin Clear; Exit end;;

  LIOBlock := TJX4IOBlock.Create;
  for LPair in AIOBlock.JObj do
  begin
    Add(LPair.JsonString.value, LNewObj);
    LPair.JsonValue.Owned := False;
    LPair.Owned := False;
    LJObjDestroy := True;
    if LPair.JsonValue is TJSONObject then
    begin
       LJObjDestroy := False;
       LJObj := LPair.JsonValue as TJSONObject;
    end else
    if LPair.JsonValue is TJSONArray then
    begin
      LJObj := TJSONObject.Create(TJSONPAir.Create('', LPair.JsonValue));
    end else
      LJObj := TJSONObject.Create(LPair);

    LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options, AIOBlock.PAbort);
    LNewObj.JSONDeserialize(LIOBlock);

    if LJObjDestroy then FreeAndNil(LJObj);
    LPair.Owned := True;
    LPair.JsonValue.Owned := True;
  end;
  LIOBlock.Free;
end;

procedure TJX4DictOfValues.JSONMerge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options);
begin
  //
end;

class function TJX4DictOfValues.New: TJX4DictOfValues;
begin
  Result := TJX4DictOfValues.Create;
end;

class function TJX4DictOfValues.NewAdd(AKey: string;
  AValue: TValue): TJX4DictOfValues;
begin
  Result := New;
  Result.Add(AKey, AValue);
end;

class function TJX4DictOfValues.NewAddRange(const AKeys: array of string;
  const AValues: array of TValue): TJX4DictOfValues;
var
  LCnt: Integer;
begin
  Result := TJX4DictOfValues.New;
  for LCnt := 0  to Length(AKeys) -1 do
    Result.Add(AKeys[LCnt], AValues[LCnt]);
end;

{ TJX3Dic<V> }

function TJX4Dict<V>.Clone<T>(AOptions: TJX4Options): T;
begin
  try
    Result := T.Create;
    TxRTTI.CallMethodProc('JSONCreate', Result, [True]);;
    TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
  except
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if joRaiseException in AOptions then Raise;
    end;
  end;
end;

constructor TJX4Dict<V>.Create;
begin
  inherited Create([doOwnsValues]);
  FAdded :=  TStringList.Create;
  FAdded.Duplicates := dupIgnore;
  FModified := TStringList.Create;
  FModified.Duplicates := dupIgnore;
  FDeleted := TStringList.Create;
  FDeleted.Duplicates := dupIgnore;
end;

destructor TJX4Dict<V>.Destroy;
begin
  FreeAndNil(FAdded);
  FreeAndNil(FModified);
  FreeAndNil(FDeleted);
  inherited Destroy;
end;

function TJX4Dict<V>.GetValue<T>(const Key: string): T;
begin
  TryGetValue(Key, Result);
end;

function TJX4Dict<V>.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TList<string>;
  LRes:       string;
  Lkp:        TPair<string, V>;
  LObj:       TObject;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LNameAttr:  TCustomAttribute;
  LTValue:    TValue;
begin
  Result := TValue.Empty;
  if Assigned(AIOBlock.Field) then
  begin
    LName := AIOBlock.Field.Name;
    LNameAttr := TJX4Name(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Name));
    if Assigned(LNameAttr) then LName := TJX4Name(LNameAttr).Name;
  end else
    LName := AIOBlock.JsonName;
  LName := TJX4Object.NameDecode(LName);

  if Count = 0 then
  begin
    if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
      raise Exception.Create(Format('"%s" (TJX3Dic) : a value is required', [LName]));

    if joNullToEmpty in AIOBlock.Options then Exit;

    if AIOBlock.JsonName.IsEmpty then
      Result := 'null'
    else
      Result := '"' + LName + '":null';
    Exit;
  end;

  LParts := TList<string>.Create;
  LIOBlock := TJX4IOBlock.Create;
  try
    LParts.Capacity := Self.Count;
    for Lkp in Self do
    begin
      if Assigned(AIOBlock.PAbort) and AIOBlock.PAbort^ then Exit;
      LObj := TValue.From<V>(Lkp.Value).AsObject;
      if Assigned(LObj) then
      begin
        LIOBlock.Init(LKp.Key, Nil, Nil, AIOBlock.Options, AIOBlock.PAbort);
        LTValue := TxRTTI.CallMethodFunc('JSONSerialize', LObj, [ LIOBlock ]);
        if not LTValue.IsEmpty then LParts.Add(LTValue.AsString);
      end;
    end;
    LRes := TJX4Object.JsonListToJsonString(LParts);
  finally
    LIOBlock.Free;
    LParts.Free;
  end;

  if AIOBlock.JsonName.IsEmpty then
    Result := '{' + LRes + '}'
  else
    Result := '"' + LName + '":{'+ LRes + '}';
end;

procedure TJX4Dict<V>.JSONClone(ADestDict: TJX4Dict<V>; AOptions: TJX4Options);
var
  LNewObj: TObject;
  LPair:  TPair<string, V>;
begin
  ADestDict.Clear;
  if Count = 0 then Exit;
  for LPair in Self do
  begin
    LNewObj := V.Create;
    TxRTTI.CallMethodProc('JSONClone', LPair.Value, [LNewObj, TValue.From<TJX4Options>(AOptions)]);
    ADestDict.Add(LPair.Key, LNewObj);
  end;
end;

procedure TJX4Dict<V>.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LPair: TJSONPair;
  LNewObj: TObject;
  LIOBlock: TJX4IOBlock;
  LJObj: TJSONObject;
  LJObjDestroy: Boolean;
begin
  if not Assigned(AIOBlock.JObj) then begin Clear; Exit end;;
  if AIOBlock.JObj.Count = 0 then begin Clear; Exit end;
  if not Assigned(AIOBlock.JObj.Pairs[0].JsonValue) then begin Clear; Exit end;
  if AIOBlock.JObj.Pairs[0].JsonValue.Null then begin Clear; Exit end;;

  LIOBlock := TJX4IOBlock.Create;
  try
    for LPair in AIOBlock.JObj do
    begin
      if Assigned(AIOBlock.PAbort) and AIOBlock.PAbort^ then Exit;
      LNewObj := V.Create;
      Add(LPair.JsonString.value, LNewObj);

      LPair.JsonValue.Owned := False;
      LPair.Owned := False;
      LJObjDestroy := True;
      if LPair.JsonValue is TJSONObject then
      begin
         LJObjDestroy := False;
         LJObj := LPair.JsonValue as TJSONObject;
      end else
      if LPair.JsonValue is TJSONArray then
      begin
        LJObj := TJSONObject.Create(TJSONPAir.Create('', LPair.JsonValue));
      end else
        LJObj := TJSONObject.Create(LPair);

      LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options, AIOBlock.PAbort);
      TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ LIOBlock ]);

      if LJObjDestroy then FreeAndNil(LJObj);
      LPair.Owned := True;
      LPair.JsonValue.Owned := True;
    end;
  finally
    LIOBlock.Free;
  end;
end;

procedure TJX4Dict<V>.JSONMerge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options);
begin
  //
end;

class function TJX4Dict<V>.New: TJX4Dict<V>;
begin
  Result := TJX4Dict<V>.Create;
end;

class function TJX4Dict<V>.NewAdd(AKey: string; AValue: V): TJX4Dict<V>;
begin
  Result := TJX4Dict<V>.New;
  Result.Add(AKey, AValue);
end;

class function TJX4Dict<V>.NewAddRange(const AKeys: array of string;
  const AValues: array of V): TJX4Dict<V>;
var
  LCnt: Integer;
begin
  Result := TJX4Dict<V>.New;
  for LCnt := 0  to Length(AKeys) -1 do
    Result.Add(AKeys[LCnt], AValues[LCnt]);
end;

procedure TJX4Dict<V>.Merge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options = []);
var
  LEle: TPair<string, V>;
  LValue, LClone: V;
  LExists: Boolean;
begin
  if (jmoStats in AOptions) then
  begin
    FAdded.Clear;
    FModified.Clear;
    FDeleted.Clear;
  end;
  if AMergedWith.Count = 0  then Exit;
  for LEle in AMergedWith do
  begin
    LExists := Self.TryGetValue(LEle.Key, LValue);
    if not LExists and (jmoAdd in AOptions) then
    begin
      LClone := V.create;
      TxRTTI.CallMethodProc('JSONClone', LEle.Value, [LClone, TValue.From<TJX4Options>(AOptions)]);
      Self.Add(LEle.Key, LClone);
       if (jmoStats in AOptions) then FAdded.Add(LEle.Key);
    end
    else if LExists and (jmoDelete in AOptions) then
    begin
      Self.Remove(LEle.Key);
       if (jmoStats in AOptions) then FDeleted.Add(LEle.Key);
    end
    else if LExists and (jmoUpdate in AOptions) then
    begin
      TxRTTI.CallMethodProc('JSONMerge', LValue, [LEle.Value, TValue.From<TJX4Options>([jmoUpdate])]);
      Self.TryGetValue(LEle.Key, LValue);
      if (jmoStats in AOptions) then FModified.Add(LEle.Key);
    end;
  end;
end;

procedure TJX4Dict<V>.Merge(AMergedWith: TJX4ValList; AOptions: TJX4Options);
var
  LEle:     TValue;
  LValue:   V;
  LExists:  Boolean;
begin
  if (jmoStats in AOptions) then
  begin
    FAdded.Clear;
    FModified.Clear;
    FDeleted.Clear;
  end;
  if AMergedWith.Count = 0  then Exit;
  for LEle in AMergedWith do
  begin
    case LEle.Kind of
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
    begin
      LExists := Self.TryGetValue(LEle.AsString, LValue);
      if LExists and (jmoDelete in AOptions) then
      begin
         if (jmoStats in AOptions) then FDeleted.Add(LEle.AsString);
        Self.Remove(LEle.AsString);
      end;
    end;
    tkInteger, tkInt64:
    begin
      LExists := Self.TryGetValue(LEle.AsInt64.ToString, LValue);
      if LExists and (jmoDelete in AOptions) then
      begin
         if (jmoStats in AOptions) then FDeleted.Add(LEle.AsInt64.ToString);
        Self.Remove(LEle.AsInt64.ToString)
      end;
    end;
    tkFloat:
    begin
      LExists := Self.TryGetValue(LEle.AsExtended.ToString, LValue);
      if LExists and (jmoDelete in AOptions) then
      begin
        if (jmoStats in AOptions) then FDeleted.Add(LEle.AsExtended.ToString);
        Self.Remove(LEle.AsExtended.ToString)
      end;
    end;
  end;
  end;
end;


end.
