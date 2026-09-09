(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2027 Laurent Meyer JsonX3@ea4d.com

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
  SysUtils
  , RTTI
  , uJX4List
  , uJX4Object
  , Classes
  , zLib
  , System.Generics.Collections
  ;

type

  TJX4DictOfValues =  class;

  TObjectDictionary<V> = class(System.Generics.Collections.TObjectDictionary<string,V>);
  TJX4Dict<V:class, constructor> = class(TObjectDictionary<V>, IJX4Jsonable)
  private
    FAdded:   TList<string>;
    FUpdated: TList<string>;
    FDeleted: TList<string>;
  private
    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure JSONCreate(AManaged: Boolean; AOptions: TJX4Options = []);
    procedure JSONClone(ADestDict:  TJX4Dict<V>; AOptions: TJX4Options = []);
    procedure JSONMerge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options = []);
    procedure JSONClear(AOptions: TJX4Options = []);

    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    procedure JX4Clear(AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
  public
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;

    constructor Create;
    destructor  Destroy; override;

    function       Clone<T:class, constructor>(AOptions: TJX4Options = []): T; overload;
    function       Merge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options = []): Boolean; overload;
    function       Merge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options = []): Boolean; overload;
    function       Merge(AMergedWith: array of string; AOptions: TJX4Options = []): Boolean; overload;
    function       Merge(AMergedWith: TArray<TValue>; AOptions: TJX4Options = []): Boolean; overload;
    function       Merge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options = []): Boolean; overload;

    function       ToJSON(AOptions: TJX4Options = []): string;
    function       ToYAML(AOptions: TJX4Options = []): string;
    function       Format(AIndentation: Integer): string;

    class function New: TJX4Dict<V>;
    class function NewAdd(AKey: string; AValue: V; AOptions: TJX4Options = []): TJX4Dict<V>;
    class function NewAddRange(const AKeys: array of string; const AValues: array of V; AOptions: TJX4Options = []): TJX4Dict<V>; overload;
    class function NewAddRange(const AKeys: TArray<TValue>; const AValues: array of V; AOptions: TJX4Options = []): TJX4Dict<V>; overload;
    function       AddRange(const AKeys: array of string; const AValues: array of V; AOptions: TJX4Options = []): Boolean; overload;
    function       AddRange(const AKeys: TArray<TValue>; const AValues: array of V; AOptions: TJX4Options = []): Boolean; overload;

    function       SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = NIl; AZipIt: TCompressionLEvel = clNone; AUseBOM: Boolean = False): Int64;

    property       EleAdded:    TList<string> read FAdded;
    property       EleUpdated:  TList<string> read FUpdated;
    property       EleDeleted:  TList<string> read FDeleted;
  end;

  TJX4Dictionary<V:class, constructor> = class(TJX4Dict<V>);
  TJX4Dic<V:class, constructor> = class(TJX4Dict<V>);

  TJX4DictOfValues = class(System.Generics.Collections.TObjectDictionary<string, TValue>, IJX4Jsonable)
  private
    FAdded:     TList<string>;
    FUpdated:   TList<string>;
    FDeleted:   TList<string>;
  private
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure JSONCreate(AManaged: Boolean; AOptions: TJX4Options = []);
    procedure JSONClone(ADestDict: TJX4DictOfValues; AOptions: TJX4Options = []);
    procedure JSONMerge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options = []);
    procedure JSONClear(AOptions: TJX4Options = []);
    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    procedure JX4Clear(AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
  public
    constructor     Create; overload;
    destructor      Destroy; override;

    class function New: TJX4DictOfValues;
    class function NewAdd(AKey: string; AValue: TValue; AOptions: TJX4Options = []): TJX4DictOfValues;
    class function NewAddRange(const AKeys: array of string; const AValues: array of TValue; AOptions: TJX4Options = []): TJX4DictOfValues;
    function       AddRange(const AKeys: array of string; const AValues: array of TValue; AOptions: TJX4Options = []): Boolean;

    function       Clone(AOptions: TJX4Options = []): TJX4DictOfValues; overload;
    function       Merge<T:class, constructor>(AMergedWith: TJX4Dict<T>; AOptions: TJX4Options): Boolean; overload;
    function       Merge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options = []): Boolean; overload;
    function       Merge(AMergedWith: array of string; AOptions: TJX4Options = []): Boolean; overload;
    function       Merge(AMergedWith: TArray<string>; AOptions: TJX4Options = []): Boolean; overload;
    function       ToJSON(AOptions: TJX4Options = []): string;
    function       ToYAML(AOptions: TJX4Options = []): string;
    function       Format(AIndentation: Integer): string;

    function       SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = Nil; AZipIt: TCompressionLevel = clNone;  AUseBOM: Boolean = False): Int64;

    property       EleAdded:    TList<string> read FAdded;
    property       EleUpdated:  TList<string> read FUpdated;
    property       EleDeleted:  TList<string> read FDeleted;
  end;

  TJX4ValDic  = class(TJX4DictOfValues);
  TJX4ValDict = class(TJX4DictOfValues);

  MyTThread = class(TThread); // TThread Protected Access

implementation
uses
    uJX4Rtti
  , uJX4Value
  , JSON
  , Threading
  ;

{ TJX4DictOfValues }

function TJX4DictOfValues.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;
function TJX4DictOfValues._AddRef: Integer; begin Result := -1; end;
function TJX4DictOfValues._Release: Integer;begin Result := -1; end;

function TJX4DictOfValues.Clone(AOptions: TJX4Options): TJX4DictOfValues;
begin
  Result := TJX4DictOfValues.Create;
  try
    JX4Clone(Result, AOptions);
  except
    on TJX4ExceptionAborted do
    begin
      FreeAndNil(Result);
      if joRaiseOnAbort in AOptions then raise;
    end;
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4DictOfValues.Merge<T>(AMergedWith: TJX4Dict<T>; AOptions: TJX4Options): Boolean;
var
  LDictV: TJX4DictOfValues;
  LPair: TPair<string, T>;
begin
  Result := False;
  if AMergedWith.Count = 0 then Exit;
  LDictV := TJX4DictOfValues.Create;
  try
    if (jmoAdd in AOptions) or (jmoUpdate in AOptions) then
      raise Exception.Create('TJX4DictOfValues.Merge (TJX4Dict<T>) : "jmoUpdate", "jmoPurge" Only');
    try
      for LPair in AMergedWith do LDictV.Add(LPair.Key, Nil);
      JX4Merge(LDictV, AOptions);
      Result := True;
    finally
      LDictV.Free;
    end;
  except
    on TJX4ExceptionAborted do
    begin
      if joRaiseOnAbort in AOptions then raise;
    end;
    on Ex: Exception do
    begin
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4DictOfValues.Merge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options): Boolean;
begin
  Result := False;
  if AMergedWith.Count = 0 then Exit;
  try
    JX4Merge(AMergedWith, AOptions);
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end
end;

function TJX4DictOfValues.Merge(AMergedWith: array of string; AOptions: TJX4Options): Boolean;
var
  LDict: TJX4DictOfValues;
begin
  Result := False;
  if Length(AMergedWith) = 0 then Exit;
  try
    if (jmoAdd in AOptions) or (jmoUpdate in AOptions) then
      raise Exception.Create('TJX4Dict<V>.Merge (array of string) : "jmoUpdate", "jmoPurge" Only');
    LDict := TJX4DictOfValues.Create;
    try
      for var LKey in AMergedWith do LDict.Add(LKey, '');
      JX4Merge(LDict, AOptions);
      Result := True;
    finally
      LDict.Free;
    end;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4DictOfValues.Merge(AMergedWith: TArray<string>; AOptions: TJX4Options): Boolean;
var
  LDict: TJX4DictOfValues;
begin
  Result := False;
  if Length(AMergedWith) = 0 then Exit;
  try
    if (jmoAdd in AOptions) or (jmoUpdate in AOptions) then
      raise Exception.Create('TJX4Dict<V>.Merge (array of string) : "jmoUpdate", "jmoPurge" Only');
    LDict := TJX4DictOfValues.Create;
    try
      for var LKey in AMergedWith do LDict.Add(LKey, '');
      JX4Merge(LDict, AOptions);
      Result := True;
    finally
      LDict.Free;
    end;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

constructor TJX4DictOfValues.Create;
begin
  inherited Create;
  FAdded := Nil;
  FDeleted := Nil;
  FUpdated := Nil;
end;

destructor TJX4DictOfValues.Destroy;
begin
  FreeAndNil(FUpdated);
  FreeAndNil(FDeleted);
  FreeAndNil(FAdded);
  inherited Destroy;
end;

function TJX4DictOfValues.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TArray<string>;
  LPartsIdx:  Integer;
  LRes:       string;
  Lkp:        TPair<string, TValue>;
  LTValue:    TValue;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
begin
  if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient)) then Exit;
  Result := TValue.Empty;
  TJX4Object.RaiseIfAborted(AIOBlock.Options);
  LPartsIdx := 0;

  if Self.count > 0 then
  begin
    LIOBlock := TJX4IOBlock.Create;
    try
      SetLength(LParts, Self.Count);
      for Lkp in Self do
      begin
        LIOBlock.Init(LKp.Key, Nil, Nil, AIOBlock.Options);
        LTValue := Lkp.Value.JSONSerialize(LIOBlock);
        if not LTValue.IsEmpty then
          begin
            LParts[LPartsIdx] := LTValue.AsString;
            Inc(LPartsIdx);
          end;
      end;
      SetLength(LParts, LPartsIdx);
    finally
      LIOBlock.Free;
    end;
  end;

  if LPartsIdx = 0 then
  begin
    LName := TJX4Object.ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);
    if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
      raise Exception.Create(SysUtils.Format('"%s" (TJX4List<T>.JSONSerialize) : a value is required', [LName]));
    if (joNullToEmpty in AIOBlock.Options) then Exit;
    if LName.IsEmpty then Result := 'null' else Result := '"' + LName + '":null';
    Exit;
  end;

  LRes := string.Join(',', LParts);
  LName := TJX4Object.ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);
  if LName.IsEmpty then Result := '{' + LRes + '}' else Result := '"' + LName + '":{'+ LRes + '}';
end;

procedure TJX4DictOfValues.JX4Clone(ADestObj: TObject; AOptions: TJX4Options);
begin
  JSONClone(TJX4DictOfValues(ADestObj), AOptions);
end;

procedure TJX4DictOfValues.JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
begin
   JSONCreate(AManaged, AOptions);
end;

procedure TJX4DictOfValues.JX4Deserialize(AIOBlock: TJX4IOBlock);
begin
  JSONDeserialize(AIOBlock);
end;

function TJX4DictOfValues.JX4Destroy(AOptions: TJX4Options = []): Boolean;
begin
  Result := True;
end;

procedure TJX4DictOfValues.JX4Merge(AMergedWith: TObject;
  AOptions: TJX4Options);
begin
  JSONMerge(TJX4DictOfValues(AMergedWith), AOptions);
end;

function TJX4DictOfValues.JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
begin
  Result := JSONSerialize(AIOBlock);
end;

procedure TJX4DictOfValues.JSONCreate(AManaged: Boolean; AOptions: TJX4Options = []);
begin
//
end;

procedure TJX4DictOfValues.JSONClone(ADestDict: TJX4DictOfValues; AOptions: TJX4Options = []);
var
  LPair:  TPair<string, TValue>;
begin
  TJX4Object.RaiseIfAborted(AOptions);
  ADestDict.Clear;
  for LPair in Self do
    ADestDict.Add(LPair.Key, LPair.Value);
end;

procedure TJX4DictOfValues.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LPair:  TJSONPair;
  LValue: TValue;
begin
  if not Assigned(AIOBlock.JObj) then begin Clear; Exit end;;
  if AIOBlock.JObj.Count = 0 then begin Clear; Exit end;
  if not Assigned(AIOBlock.JObj.Pairs[0].JsonValue) then begin Clear; Exit end;
  TJX4Object.RaiseIfAborted(AIOBlock.Options);
  for LPair in AIOBlock.JObj do
  begin
    LValue.FromJSONValue(LPair.JsonValue);
    Add(LPair.JsonString.value, LValue);
  end;
end;

procedure TJX4DictOfValues.JSONMerge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options);
var
  LEle: TPair<string, TValue>;
  LExists: Boolean;
begin
  if AMergedWith.Count = 0 then Exit;
  TJX4Object.RaiseIfAborted(AOptions);

  if (jmoStat in AOptions) then
  begin
    if not Assigned(FDeleted) then FDeleted := TList<string>.Create else FDeleted.Clear;
    if not Assigned(FAdded) then FAdded := TList<string>.Create else FAdded.Clear;
    if not Assigned(FUpdated) then FUpdated := TList<string>.Create else FUpdated.Clear;
  end;

  for LEle in AMergedWith do
  begin
    LExists := Self.ContainsKey(LEle.Key);
    if LExists and (jmoDelete in AOptions) then
    begin
      Self.Remove(LEle.Key);
      if (jmoStat in AOptions) then FDeleted.Add(LEle.Key);
    end
    else if LExists and (jmoUpdate in AOptions) then
    begin
      Self.AddOrSetValue(LEle.Key, LEle.Value);
      if (jmoStat in AOptions) then FUpdated.Add(LEle.Key);
    end else
    if not LExists and (jmoAdd in AOptions) then
    begin
      Self.Add(LEle.Key, LEle.Value);
      if (jmoStat in AOptions) then FAdded.Add(LEle.Key);
    end;
  end;
  if (jmoPurge in AOptions) then
    for LEle in Self do
    begin
      if not AMergedWith.ContainsKey(LEle.Key) and (jmoPurge in AOptions) then
      begin
        Self.Remove(LEle.Key);
        if (jmoStat in AOptions) then FDeleted.Add(LEle.Key);
      end;
    end;
end;

procedure TJX4DictOfValues.JSONClear(AOptions: TJX4Options);
begin
  Self.Clear;
end;

procedure TJX4DictOfValues.JX4Clear(AOptions: TJX4Options = []);
begin
  JSONClear(AOptions);
end;

class function TJX4DictOfValues.New: TJX4DictOfValues;
begin
  Result := TJX4DictOfValues.Create;
end;

class function TJX4DictOfValues.NewAdd(AKey: string; AValue: TValue; AOptions: TJX4Options): TJX4DictOfValues;
begin
  Result := New;
  try
    Result.Add(AKey, AValue);
    except
      on TJX4ExceptionAborted do
      begin
        FreeAndNil(Result);
        if joRaiseOnAbort in AOptions then raise;
      end;
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4DictOfValues.AddRange(const AKeys: array of string;
  const AValues: array of TValue; AOptions: TJX4Options): Boolean;
var
  LCnt: Integer;
begin
  Result := False;
  try
    if Length(AKeys) <> Length(AKeys) then
      raise Exception.Create(' TJX4DictOfValues.NewAddRange : Arrays length do not match');
    for LCnt := 0  to Length(AKeys) -1 do
      Self.Add(AKeys[LCnt], AValues[LCnt]);
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

class function TJX4DictOfValues.NewAddRange(const AKeys: array of string;
  const AValues: array of TValue; AOptions: TJX4Options): TJX4DictOfValues;
begin
  Result := TJX4DictOfValues.New;
  try
    Result.AddRange(AKeys, AValues, AOptions);
  except
    on TJX4ExceptionAborted do
    begin
      FreeAndNil(Result);
      if joRaiseOnAbort in AOptions then raise;
    end;
  on Ex: Exception do
    begin
      FreeAndNil(Result);
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4DictOfValues.ToJSON(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToJSON(Self, AOptions);
end;

function TJX4DictOfValues.ToYAML(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToYAML(Self.ToJSON(AOptions));
end;

function TJX4DictOfValues.Format(AIndentation: Integer): string;
begin
  Result := TJX4Object.FormatJSON(Self.ToJSON([]), True, AIndentation);
end;

function TJX4DictOfValues.SaveToJSONFile(const AFilename: string; AOptions: TJX4Options; AEncoding: TEncoding; AZipIt: TCompressionLevel; AUseBOM: Boolean): Int64;
begin
  Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self, AOptions), AEncoding, AZipIt, AUseBOM);
end;

{ TJX4Dic<V> }

function TJX4Dict<V>.Clone<T>(AOptions: TJX4Options): T;
var
  LIntf: IJX4Jsonable;
begin
  try
    Result := T.Create;
    TJX4Object.RaiseIfAborted(AOptions);
    if Supports(Result, IJX4Jsonable, LIntf) then
      LIntf.JX4Create(True)
    else
      TxRTTI.CallMethodProc('JSONCreate', Result, [True]);
    if Supports(Self, IJX4Jsonable, LIntf) then
      LIntf.JX4Clone(Result, AOptions)
    else
      TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
  except
    on TJX4ExceptionAborted do
    begin
      FreeAndNil(Result);
      if joRaiseOnAbort in AOptions then raise;
    end;
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

constructor TJX4Dict<V>.Create;
begin
  inherited Create([doOwnsValues]);
  FDeleted:= Nil;
  FUpdated:= Nil;
  FAdded:= Nil;
end;

destructor TJX4Dict<V>.Destroy;
begin
  FreeAndNil(FDeleted);
  FreeAndNil(FUpdated);
  FreeAndNil(FAdded);
  inherited Destroy;
end;

function TJX4Dict<V>.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TArray<string>;
  LPartsIdx:  Integer;
  LRes:       string;
  Lkp:        TPair<string, V>;
  LObj:       TObject;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LTValue:    TValue;
  LIntf:      IJX4Jsonable;
begin
  if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient)) then Exit;
  Result := TValue.Empty;
  TJX4Object.RaiseIfAborted(AIOBlock.Options);
  LPartsIdx := 0;

  if Self.count > 0 then
  begin
    SetLength(LParts, Self.Count);
    LIOBlock := TJX4IOBlock.Create;
    try
      for Lkp in Self do
      begin
        LObj := TValue.From<V>(Lkp.Value).AsObject;
        if Assigned(LObj) then
        begin
          LIOBlock.Init(LKp.Key, Nil, Nil, AIOBlock.Options);
          if Supports(LObj, IJX4Jsonable, LIntf) then
            LTValue := LIntf.JX4Serialize(LIOBlock)
          else
            LTValue := TxRTTI.CallMethodFunc('JSONSerialize', LObj, [ LIOBlock ]);
            if not LTValue.IsEmpty then
            begin
              LPArts[LPartsIdx] := LTValue.AsString;
              Inc(LPartsIdx);
            end;
        end else begin
          if not (joNullToEmpty in AIOBlock.Options) then
          begin
            LPArts[LPartsIdx] := '"' + LKp.Key + '":{null}';
            Inc(LPartsIdx);
          end;
        end;
      end;
    finally
      LIOBlock.Free;
    end;
  end;

  LName := TJX4Object.ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);
  if LPartsIdx = 0 then
  begin
    if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
      raise Exception.Create(System.SysUtils.Format('"%s" (TJX4Dict<V>) : a value is required', [LName]));
    if joNullToEmpty in AIOBlock.Options then Exit;
    if AIOBlock.JsonName.IsEmpty then Result := 'null' else Result := '"' + LName + '":null';
    Exit;
  end;

   SetLength(LParts, LPartsIdx);
   LRes := string.Join(',', LParts);
   if AIOBlock.JsonName.IsEmpty then Result := '{' + LRes + '}' else Result := '"' + LName + '":{'+ LRes + '}';

end;

procedure TJX4Dict<V>.JSONCreate(AManaged: Boolean; AOptions: TJX4Options = []);
begin
  //
end;

procedure TJX4Dict<V>.JSONClone(ADestDict: TJX4Dict<V>; AOptions: TJX4Options);
var
  LNewObj:TObject;
  LPair:  TPair<string, V>;
  LIntf:  IJX4Jsonable;
begin
  if Count = 0 then Exit;
  TJX4Object.RaiseIfAborted(AOptions);
  ADestDict.Clear;
  for LPair in Self do
  begin
    LNewObj := V.Create;
    if Supports(LNewObj, IJX4Jsonable, LIntf) then
      LIntf.JX4Create(True)
    else
      TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True, TValue.From<TJX4Options>(AOptions)]);
    if Supports(LPair.Value, IJX4Jsonable, LIntf) then
    begin
      LIntf.JX4Clone(LNewObj, AOptions);
    end else
      TxRTTI.CallMethodProc('JSONClone', LPair.Value, [LNewObj, TValue.From<TJX4Options>(AOptions)]);
    ADestDict.Add(LPair.Key, LNewObj);
  end;
end;

procedure TJX4Dict<V>.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LPair:        TJSONPair;
  LNewObj:      TObject;
  LIOBlock:     TJX4IOBlock;
  LJObj:        TJSONObject;
  LJObjDestroy: Boolean;
  LIntf:        IJX4Jsonable;
begin
  if not Assigned(AIOBlock.JObj) then begin Clear; Exit end;;
  if AIOBlock.JObj.Count = 0 then begin Clear; Exit end;
  if not Assigned(AIOBlock.JObj.Pairs[0].JsonValue) then begin Clear; Exit end;
  if Assigned(AIOBlock.Field) and Assigned(TJX4Transient(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient))) then begin Exit; end;

  TJX4Object.RaiseIfAborted(AIOBlock.Options);
  LIOBlock := TJX4IOBlock.Create;
  try
    for LPair in AIOBlock.JObj do
    begin
      LPair.JsonValue.Owned := False;
      LPair.Owned := False;
      LJObjDestroy := True;
      try
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
          LNewObj := V.Create;
        try
          LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options);
          if Supports(LNewObj, IJX4Jsonable, LIntf) then
            LIntf.JX4Deserialize(LIOBlock)
          else
            TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ LIOBlock ]);
          Add(LPair.JsonString.value, LNewObj);
        except
          FreeAndNil(LNewObj);
          raise;
        end;
      finally
        if LJObjDestroy then FreeAndNil(LJObj);
        LPair.Owned := True;
        LPair.JsonValue.Owned := True;
      end;
    end;
  finally
    LIOBlock.Free;
  end;
end;

procedure TJX4Dict<V>.JSONMerge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options);
var
  LEle: TPair<string, V>;
  LValue, LObj: V;
  LExists: Boolean;
  LIntf: IJX4Jsonable;
  LPair: TPair<string, V>;
begin
  if AMergedWith.Count = 0 then Exit;
  TJX4Object.RaiseIfAborted(AOptions);

  if (jmoStat in AOptions) then
  begin
    if not Assigned(FDeleted) then FDeleted := TList<string>.Create else FDeleted.Clear;
    if not Assigned(FAdded) then FAdded := TList<string>.Create else FAdded.Clear;
    if not Assigned(FUpdated) then FUpdated := TList<string>.Create else FUpdated.Clear;
  end;

  for LEle in AMergedWith do
  begin
    LExists := Self.TryGetValue(LEle.Key, LValue);
    // jmpDelete
    if LExists and (jmoDelete in AOptions) then
    begin
       Remove(LEle.Key);
       if (jmoStat in AOptions) then FDeleted.Add(LEle.Key);
       Continue
    end
    // jmpUpdate
    else if LExists and (jmoUpdate in AOptions) then
    begin
      if not Assigned(LEle.Value) then
      begin
        AddOrSetValue(LEle.Key, Nil);
        if (jmoStat in AOptions) then FUpdated.Add(LEle.Key);
        Continue;
      end;
      if (jmoByMoving in AOptions) then
      begin
        LPair := AMergedWith.ExtractPair(LEle.Key);
        AddOrSetValue(LEle.Key, LPair.Value);
        if (jmoStat in AOptions) then FUpdated.Add(LEle.Key);
        Continue;
      end;
      LObj := V.Create;
      try
        if Supports(LEle.Value, IJX4Jsonable, LIntf) then
          LIntf.JX4Clone(LObj)
        else
          TxRTTI.CallMethodProc('JSONClone', LEle.Value, [LObj, TValue.From<TJX4Options>(AOptions)]);
      except
        LObj.Free;
        raise;
      end;
      AddOrSetValue(LEle.Key, LObj);
      if (jmoStat in AOptions) then FUpdated.Add(LEle.Key);
      Continue;
    end else
    // jmoAdd
    if not LExists and (jmoAdd in AOptions) then
    begin
      if not Assigned(LEle.Value) then
      begin
        Add(LEle.Key, Nil);
        if (jmoStat in AOptions) then FAdded.Add(LEle.Key);
        Continue;
      end;
      if (jmoByMoving in AOptions) then
      begin
        LPair := AMergedWith.ExtractPair(LEle.Key);
        Add(LEle.Key, LPair.Value);
        if (jmoStat in AOptions) then FAdded.Add(LEle.Key);
        Continue;
      end;
      LObj := V.Create;
      try
        if Supports(LEle.Value, IJX4Jsonable, LIntf) then
          LIntf.JX4Clone(LObj)
        else
          TxRTTI.CallMethodProc('JSONClone', LEle.Value, [LObj, TValue.From<TJX4Options>(AOptions)]);
      except
        LObj.Free;
        raise;
      end;
      AddOrSetValue(LEle.Key, LObj);
      if (jmoStat in AOptions) then FAdded.Add(LEle.Key);
      Continue;
    end;
  end;
  // jmoPurge
  if (jmoPurge in AOptions) then
    for LEle in Self do
    begin
      if not AMergedWith.ContainsKey(LEle.Key) then
      begin
        Remove(LEle.Key);
        if (jmoStat in AOptions) then FDeleted.Add(LEle.Key);
      end;
    end;
end;

procedure TJX4Dict<V>.JSONClear(AOptions: TJX4Options = []);
var
  LObj: TPair<string, V>;
begin
  TJX4Object.RaiseIfAborted(AOptions);
  for LObj in Self do
    TJX4Object(LObj.Value).JSONClear(AOptions);
  Self.CLear;
end;

class function TJX4Dict<V>.New: TJX4Dict<V>;
begin
  Result := TJX4Dict<V>.Create;
end;

class function TJX4Dict<V>.NewAdd(AKey: string; AValue: V; AOptions: TJX4Options): TJX4Dict<V>;
begin
  Result := TJX4Dict<V>.New;
  Result.Add(AKey, AValue);
end;

function TJX4Dict<V>.AddRange(const AKeys: array of string; const AValues: array of V; AOptions: TJX4Options): Boolean;
var
  LCnt: Integer;
begin
Result := False;
  try
    for LCnt := 0  to Length(AKeys) -1 do
      Self.Add(AKeys[LCnt], AValues[LCnt]);
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4Dict<V>.AddRange(const AKeys: TArray<TValue>; const AValues: array of V; AOptions: TJX4Options): Boolean;
var
  LKValue: TValue;
begin
  Result := False;
  try
    for LKValue in AKeys do
      Self.Add(LKValue.AsString, Nil);
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

class function TJX4Dict<V>.NewAddRange(const AKeys: array of string;
  const AValues: array of V; AOptions: TJX4Options): TJX4Dict<V>;
begin
  Result := TJX4Dict<V>.New;
  Result.AddRange(AKeys, AValues, AOptions);
end;

class function TJX4Dict<V>.NewAddRange(const AKeys: TArray<TValue>;
  const AValues: array of V; AOptions: TJX4Options): TJX4Dict<V>;
begin
  Result := TJX4Dict<V>.New;
  Result.AddRange(AKeys, AValues, AOptions);
end;

function TJX4Dict<V>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;
function TJX4Dict<V>._AddRef: Integer; begin Result := -1; end;
function TJX4Dict<V>._Release: Integer; begin Result := -1; end;

function TJX4Dict<V>.Merge(AMergedWith: TJX4Dict<V>; AOptions: TJX4Options): Boolean;
begin
  Result := False;
  if AMergedWith.Count = 0 then Exit;
  try
    JX4Merge(AMergedWith, AOptions);
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4Dict<V>.Merge(AMergedWith: TJX4DictOfValues; AOptions: TJX4Options): Boolean;
var
  LDict:  TJX4Dict<V>;
  LPair:  TPair<string, TValue>;
begin
  Result := False;
  if AMergedWith.Count = 0 then Exit;
  try
    LDict := TJX4Dict<V>.Create;
    try
      for LPair in AMergedWith do LDict.Add(LPair.Key, Nil);
      JX4Merge(LDict, AOptions);
      Result := True;
    finally
      LDict.Free;
    end;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4Dict<V>.Merge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options): Boolean;
var
  LDict: TJX4Dict<V>;
begin
  Result := True;
  if AMergedWith.Count = 0 then Exit;
  try
    if    (jmoAdd in AOptions)
       or (jmoUpdate in AOptions)
    then
      raise Exception.Create('TJX4Dict<V>.Merge (TJX4ListOfValues) : jmoDelete, jmoPurge Only...');
    LDict := TJX4Dict<V>.NewAddRange(AMergedWith.ToArray, [NIl, NIl]);
    try
      JX4Merge(LDict, AOptions);
      Result := True;
    finally
      LDict.Free;
    end;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4Dict<V>.Merge(AMergedWith: array of string; AOptions: TJX4Options): Boolean;
var
  LDict: TJX4Dict<V>;
begin
  Result := False;
  if Length(AMergedWith) = 0 then Exit;
  try
    if    (jmoAdd in AOptions)
       or (jmoUpdate in AOptions)
    then
      raise Exception.Create('TJX4Dict<V>.Merge (array of string) : jmoDelete, jmoPurge Only...');
    LDict := TJX4Dict<V>.Create;
    try
      for var LKey in AMergedWith do LDict.Add(LKey, Nil);
      JX4Merge(LDict, AOptions);
      Result := True;
    finally
      LDict.Free;
    end;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4Dict<V>.Merge(AMergedWith: TArray<TValue>; AOptions: TJX4Options): Boolean;
var
  Dict: TJX4Dict<V>;
begin
  Result := False;
  if Length(AMergedWith) = 0 then Exit;
  try
    if    (jmoAdd in AOptions)
       or (jmoUpdate in AOptions)
    then
      raise Exception.Create('TJX4Dict<V>.Merge (TArray<TValue>) : jmoDelete, jmoPurge Only...');
      Dict := TJX4Dict<V>.Create;
    try
      for var LKey in AMergedWith do Dict.Add(LKey.AsString, Nil);
      JX4Merge(Dict, AOptions);
      Result := True;
    finally
      Dict.Free;
    end;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function  TJX4Dict<V>.JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
begin
  Result := JSONSerialize(AIOBlock);
end;

procedure TJX4Dict<V>.JX4Deserialize(AIOBlock: TJX4IOBlock);
begin
  JSONDeserialize(AIOBlock);
end;

procedure TJX4Dict<V>.JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
begin
  JSONCreate(AManaged, AOptions);
end;

procedure TJX4Dict<V>.JX4Merge(AMergedWith: TObject; AOptions: TJX4Options);
begin
  JSONMerge(TJX4Dict<V>(AMergedWith), AOptions);
end;

procedure TJX4Dict<V>.JX4Clone(ADestObj: TObject; AOptions: TJX4Options);
begin
  JSONClone(TJX4Dict<V>(ADestObj), AOptions);
end;

procedure TJX4Dict<V>.JX4Clear(AOptions: TJX4Options = []);
begin
  JSONClear(AOptions);
end;

function  TJX4Dict<V>.JX4Destroy(AOptions: TJX4Options = []): Boolean;
begin
  Result := True;
end;

function TJX4Dict<V>.ToJSON(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToJSON(Self, AOptions);
end;

function TJX4Dict<V>.ToYAML(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToYAML(Self.ToJSON(AOptions));
end;

function TJX4Dict<V>.Format(AIndentation: Integer): string;
begin
  Result := TJX4Object.FormatJSON(Self.ToJSON([]), True, AIndentation);
end;

function TJX4Dict<V>.SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = NIl; AZipIt: TCompressionLEvel = clNone; AUseBOM: Boolean = False): Int64;
begin
  Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self, AOptions), AEncoding, AZipIt, AUseBOM);
end;

end.
