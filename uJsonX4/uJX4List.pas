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
AUTHORS OR COPYRIGHT HOLDERS BE LIACreateBLE FOR ANY CLAIM, DAMAGES OR OTHER
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

  TJX4ListOfValues  = class(TList<TValue>, IJX4Jsonable)
  private
    FAdded:     TList<NativeInt>;
    FDeleted:   TList<NativeInt>;
  private
    { Interface }
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;

    { Interface }
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
    procedure JX4Clear(AOptions: TJX4Options = []);

    { TJX4ListOfValues }
    function    JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure   JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure   JSONClone(ADestList: TJX4ListOfValues; AOptions: TJX4Options = []);
    procedure   JSONMerge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options = [jmoAdd]);
    procedure   JSONClear(AIOBlock: TJX4Options);
  public
    constructor     Create;
    destructor      Destroy; override;

    class function  New: TJX4ListOfValues;
    class function  NewAdd(AValue: TValue): TJX4ListOfValues;
    class function  NewAddRange(const AValues: array of TValue): TJX4ListOfValues; overload;
    function        First: TValue;
    function        Last: TValue;
    function        IndexOfTValue(const From: TValue): Integer;
    function        Clone(AOptions: TJX4Options = []): TJX4ListOfValues;
    function        Merge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options = []): Boolean; overload;
    function        Merge(AMergedWith: array of string; AOptions: TJX4Options = []): Boolean; overload;
    function        Merge(AMergedWith: TArray<string>; AOptions: TJX4Options = []): Boolean; overload;
    function        ToJSON(AOptions: TJX4Options = []): string;
    function        ToYAML(AOptions: TJX4Options = []): string;
    function        Format(AIndentation: Integer): string;
    function        SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = Nil; AZipIT: TCompressionLevel = clNone; AUseBOM: Boolean = False): Int64;
    property        EleAdded:  TList<NativeInt> read FAdded;
    property        EleDelete: TList<NativeInt> read FDeleted;
  end;

  TJX4ValList = class(TJX4ListOfValues);
  TJX4ValLst  = class(TJX4ListOfValues);

  TJX4List<T: class, constructor> = class(TObjectList<T>, IJX4Jsonable)
  private
    FAdded:     TList<NativeInt>;
    FUpdated:   TList<NativeInt>;
    FDeleted:   TList<NativeInt>;
    { IInterface }
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
    procedure JX4Clear(AOptions: TJX4Options = []);
    { RTTI }
    function   JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure  JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure  JSONClone(ADestList: TJX4List<T>; AOptions: TJX4Options = []);
    procedure  JSONMerge(AMergedWith: TJX4List<T>; AOptions: TJX4Options = [jmoAdd]);
    procedure  JSONClear(AOptions: TJX4Options = []);
  public
    constructor     Create;
    destructor      Destroy; override;

    class function  New: TJX4List<T>;
    class function  NewAdd(AValue: T): TJX4List<T>;
    class function  NewAddRange(const AValues: array of T): TJX4List<T>; overload;
    function        First:T;
    function        Last: T;
    function        ToJSON(Options: TJX4Options = []): string;
    function        ToYAML(AOptions: TJX4Options = []): string; overload;
    class function  ToYAML(AStr: string): string; overload;
    function        Clone<V:class, constructor>(AOptions: TJX4Options = []): V; overload;
    procedure       Merge(AMergedWith: TJX4List<T>; AOptions: TJX4Options = [jmoAdd]);
    function        SaveToJSONFile(const AFilename: string; AOptions: TJX4Options = [joNullToEmpty]; AEncoding: TEncoding = Nil; AZipIT: TCompressionLevel = clNone; AUseBOM: Boolean = False): Int64;
    function        Format(AIndentation: Integer = 2): string;

    property        EleAdded:  TList<NativeInt> read FAdded;
    property        EleDelete: TList<NativeInt> read FDeleted;
  end;

   TJX4Lst<V:class, constructor> = class(TJX4List<V>);

   TJX4ListNotOwned<V:class, constructor> = class(TJX4List<V>)
   public
    constructor Create; overload;
   end;

   MyTThread = class(TThread); // TThread Protected Access

implementation
uses
    Generics.Defaults
  , uJX4Rtti
  , uJX4Value
  , JSON
  , System.TypInfo
  , Threading
  , Math
  , StrUtils
  ;


{ TJX4ListOfValues }

function TJX4ListOfValues.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;
function TJX4ListOfValues._AddRef: Integer; begin Result := -1; end;
function TJX4ListOfValues._Release: Integer; begin Result := -1; end;

procedure TJX4ListOfValues.JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
begin
  //
end;

procedure TJX4ListOfValues.JX4Deserialize(AIOBlock: TJX4IOBlock);
begin
  JSONDeserialize(AIOBlock);
end;

function TJX4ListOfValues.JX4Destroy(AOptions: TJX4Options = []): Boolean;
begin
  Result := True;
end;

procedure TJX4ListOfValues.JX4Merge(AMergedWith: TObject;
  AOptions: TJX4Options);
begin
  JSONMerge(TJX4ListOfValues(AMergedWith), AOptions);
end;

function TJX4ListOfValues.JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
begin
  Result := JSONSerialize(AIOBlock);
end;

constructor TJX4ListOfValues.Create;
begin
  inherited Create;
  FAdded := Nil;
  FDeleted := Nil;
end;

destructor TJX4ListOfValues.Destroy;
begin
  FreeAndNil(FAdded);
  inherited Destroy;
end;

procedure TJX4ListOfValues.JSONClone(ADestList: TJX4ListOfValues; AOptions: TJX4Options);
var
  LList: TValue;
begin
  ADestList.Clear;
  for LList in Self do
    ADestList.Add(LList);
end;

procedure TJX4ListOfValues.JX4Clone(ADestObj: TObject; AOptions: TJX4Options);
begin
  JSONClone(TJX4ListOfValues(ADestObj), AOptions);
end;

function TJX4ListOfValues.Clone(AOptions: TJX4Options): TJX4ListOfValues;
begin
  Result := TJX4ListOfValues.Create;
  try
    JSONClone(TJX4ListOfValues(Result), AOptions);
  except
    on TJX4ExceptionAborted do
    begin
      FreeAndNil(Result);
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

procedure TJX4ListOfValues.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LEle:       TJSONValue;
  LIOBlock:   TJX4IOBlock;
  LJObj:      TJSONObject;
  LTValue:    TValue;
  LArr:       TJSONArray;
begin

  TJX4Object.RaiseIfAborted(AIOBlock.Options);

  if Assigned(AIOBlock.JArr) and (AIOBlock.JArr.count > 0) then
    LArr := (AIOBlock.JArr)
  else
  if Assigned(AIOBlock.JObj) and (TJSONArray(AIOBlock.JObj).Count > 0) then
    LArr :=  TJSONArray(AIOBlock.JObj.Pairs[0].JsonValue)
  else
   begin
     Clear;
     Exit;
   end;

  LIOBlock := TJX4IOBlock.Create;
  try
    Capacity := LArr.Count;
    for LEle in LArr do
    begin
      LEle.Owned := False;
      LJObj := TJSONObject.Create(TJSONPair.Create('', LEle));
      try
        LIOBlock.Init(AIOBlock.JsonName, LJObj, AIOBlock.Field, AIOBlock.Options);
        LTValue.JSONDeserialize(LIOBlock);
        Add(LTValue);
      finally
        LJObj.Free;
        LEle.Owned := True;
      end;
    end;
  finally
    LIOBlock.Free;
  end;
end;

function TJX4ListOfValues.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TArray<string>;
  LPartsIdx:  Integer;
  LRes:       string;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LEle:       TValue;
  LTValue:    TValue;
begin
  if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient)) then Exit;
  Result := TValue.Empty;
  TJX4Object.RaiseIfAborted(AIOBlock.Options);

  LPartsIdx := 0;
  SetLength(LParts, Self.count);
  LIOBlock := TJX4IOBlock.Create('', Nil, Nil, AIOBlock.Options);
  try
    if Self.count > 0 then
    begin
      for LEle in Self do
      begin
        LTValue := LEle.JSONSerialize(LIOBlock);
        if not LTValue.IsEmpty then
        begin
          LParts[LPartsIdx] := LTValue.AsString;
          Inc(LPartsIdx);
        end;
      end;
    end;
  finally
    LIOBlock.Free;
  end;

  if LPartsIdx = 0 then
  begin
    LName := TJX4Object.ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);
    if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
    raise Exception.Create(SysUtils.Format('"%s" (TJX4List<T>.JSONSerialize) : a value is required', [LName]));
    if (joNullToEmpty in AIOBlock.Options) then Exit;
    if not LName.IsEmpty then Result := '"' + LName + '":null';
    Exit;
  end;

  SetLength(LParts, LPartsIdx);
  LRes := TJX4Object.JoinStrings(LParts, ',');
  //LRes := string.Join(',', LParts);
  LName := TJX4Object.ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);
  if LName.IsEmpty then Result := '[' + LRes + ']' else Result := '"' + LName + '":[' + LRes + ']';

end;

function TJX4ListOfValues.First: TValue;
begin
  Result := Nil;
  if Self.Count >0 then Result := Self[0];
end;

function TJX4ListOfValues.Last: TValue;
begin
  Result := Nil;
  if Self.Count >0 then Result := Self[Count - 1];
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
  try
    if not Assigned(AEncoding) then AEncoding := TEncoding.UTF8;
    Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self, AOptions), AEncoding, AZipIT, AUseBOM);
  except
    on TJX4ExceptionAborted do
    begin
      Result := -1;
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      Result := -1;
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4ListOfValues.Merge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options): Boolean;
var
  LIntf: IJX4Jsonable;
begin
  Result := False;
  try
    TJX4Object.RaiseIfAborted(AOptions);
    if (jmoDelete in AOptions) and (jmoUpdate in AOptions) then
      raise Exception.Create('TJX4ListOfValues.Merge : jmoUpdate and jmoDelete are not compatible');
    if AMergedWith.Count = 0 then Exit;
    if Supports(Self, IJX4Jsonable, LIntf) then
    begin
      LIntf.JX4Create(True);
      LIntf.JX4Merge(AMergedWith, AOptions);
    end else
      TxRTTI.CallMethodProc('JSONMerge', Self, [ AMergedWith, TValue.From<TJX4Options>(AOptions) ]);
    Result := True;
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

function TJX4ListOfValues.Merge(AMergedWith: array of string; AOptions: TJX4Options): Boolean;
var
  LList: TJX4ListOfValues;
begin
  Result := False;
  if Length(AMergedWith) = 0 then Exit;
  try
    Exclude(AOptions, jmoUpdate);
    if not (jmoDelete in AOptions) then
      raise Exception.Create('TJX4ListOfValues<V>.Merge (array of string) : jmoAdd, jmoDelete Only...');
    LList := TJX4ListOfValues.Create;
    try
      for var LKey in AMergedWith do LList.Add(LKey);
      Self.Merge(LList, AOptions);
    finally
      LList.Free;
    end;
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4ListOfValues.Merge(AMergedWith: TArray<string>; AOptions: TJX4Options): Boolean;
var
  LList: TJX4ListOfValues;
begin
  Result := False;
  if Length(AMergedWith) = 0 then Exit;
  try
    Exclude(AOptions, jmoAdd);
    Exclude(AOptions, jmoUpdate);
    if not (jmoDelete in AOptions)  then
      raise Exception.Create('TJX4ListOfValues<V>.Merge (TArray<string>) : jmoAdd, jmoDelete Only...');
    LList := TJX4ListOfValues.Create;
    try
      for var LKey in AMergedWith do LList.Add(LKey);
      Self.Merge(LList, AOptions);
    finally
      LList.Free;
    end;
    Result := True;
  except
    on TJX4ExceptionAborted do
      if joRaiseOnAbort in AOptions then raise;
    on Ex: Exception do
      if not (joNoException in AOptions) then raise;
  end;
end;

function TJX4ListOfValues.ToJSON(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToJSON(Self, AOptions);
end;

function TJX4ListOfValues.ToYAML(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToYAML(Self.ToJSON(AOptions));
end;

function TJX4ListOfValues.Format(AIndentation: Integer): string;
begin
  Result := TJX4Object.FormatJSON(Self.ToJSON([]), True, AIndentation);
end;

procedure TJX4ListOfValues.JSONClear(AIOBlock: TJX4Options);
begin
  Self.Clear;
end;

procedure TJX4ListOfValues.JX4Clear(AOptions: TJX4Options = []);
begin
  JSONClear(AOptions);
end;

{ TJX4List<T> }

function TJX4List<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;
function TJX4List<T>._AddRef: Integer; begin Result := -1; end;
function TJX4List<T>._Release: Integer; begin Result := -1; end;

constructor TJX4List<T>.Create;
var
  LField:     TRTTIField;
  LNewObj:    TObject;
  LIntf:      IJX4Jsonable;
begin
  inherited Create(True);
  FAdded := Nil;
  FUpdated := Nil;
  FDeleted := Nil;
  for LField in TxRTTI.GetFields(Self) do
  begin
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      if not Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then
      begin
        LNewObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
        if not Assigned(LNewObj) then Continue;
        if Supports(LNewObj, IJX4Jsonable, LIntf) then
          LIntf.JX4Create(True)
        else
          TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        LField.SetValue(Self, LNewObj);
      end else
        LField.SetValue(Self, Nil);
      end;
    end;
end;

destructor TJX4List<T>.Destroy;
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
  LIntf:      IJX4Jsonable;
begin
  FreeAndNil(FDeleted);
  FreeAndNil(FUpdated);
  FreeAndNil(FAdded);
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
    if (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic, mvPublished]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then
      begin
        if Supports(LObj, IJX4Jsonable, LIntf) then
          if LIntf.JX4Destroy then FreeAndNil(LObj)
        else
          if TxRTTI.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then FreeAndNil(LObj);
      end else
        FreeAndNil(LObj);
    end;
  inherited;
end;

procedure TJX4List<T>.JSONClone(ADestList: TJX4List<T>; AOptions: TJX4Options);
var
  LNewObj:    TObject;
  LEle :      TObject;
  LIntfFrom:  IJX4Jsonable;
  LIntfTo:    IJX4Jsonable;
begin
  if Count = 0 then Exit;
  TJX4Object.RaiseIfAborted(AOptions);
  ADestList.Clear;
  for LEle in Self do
  begin
    LNewObj := T.Create;
    try
      if Supports(LNewObj, IJX4Jsonable, LIntfTo) and Supports(LEle, IJX4Jsonable, LIntfFrom) then
      begin
        LIntfTo.JX4Create(True);
        LIntfFrom.JX4Clone(LNewObj, AOptions);
      end else begin
        TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
        TxRTTI.CallMethodProc('JSONClone', LEle, [LNewObj, TValue.From<TJX4Options>(AOptions)]);
      end;
      ADestList.Add(LNewObj);
    except
      FreeAndNil(LNewObj);
      raise;
    end;
   end;
end;

procedure TJX4List<T>.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LNewObj:    TObject;
  LIOBlock:   TJX4IOBlock;
  LVal:       TJSONValue;
  LArr:       TJSONArray;
  LIntf:      IJX4Jsonable;
begin

  TJX4Object.RaiseIfAborted(AIOBlock.Options);

  if Assigned(AIOBlock.JArr) and (AIOBlock.JArr.count > 0) then
    LArr := (AIOBlock.JArr)
  else
  if Assigned(AIOBlock.JObj) and (TJSONArray(AIOBlock.JObj).Count > 0) then
    LArr :=  TJSONArray(AIOBlock.JObj.Pairs[0].JsonValue)
  else begin
    Clear;
    Exit;
  end;

  LIOBlock := TJX4IOBlock.Create;
  try
    for LVal in LArr do
    begin
      LNewObj := T.Create;
      try
        LIOBlock.Init(AIOBlock.JsonName, LVal, AIOBlock.Field, AIOBlock.Options);
        if Supports(LNewObj, IJX4Jsonable, LIntf) then
        begin
          LIntf.JX4Create(True);
          LIntf.JX4Deserialize(LIOBlock);
        end else begin
          TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
          TxRTTI.CallMethodProc( 'JSONDeserialize', LNewObj, [ LIOBlock ] );
        end;
      except
        FreeAndNil(LNewObj);
        raise;
      end;
      Add(LNewObj);
    end;
  finally
    LIOBlock.Free;
  end;
end;

procedure TJX4List<T>.JSONMerge(AMergedWith: TJX4List<T>; AOptions: TJX4Options);
var
  LObj: T;
  LEle: T;
  LIntf: IJX4Jsonable;
begin

  if AMergedWith.Count = 0 then Exit;
  TJX4Object.RaiseIfAborted(AOptions);

  if (jmoStat in AOptions) then
  begin
    if not Assigned(FDeleted) then FDeleted := TList<NativeInt>.Create else FDeleted.Clear;
    if not Assigned(FAdded) then FAdded := TList<NativeInt>.Create else FAdded.Clear;
  end;

  if (jmoAdd in AOptions) then
    for LEle in AMergedWith do
    begin
      LObj := T.Create;
      try
        if Supports(LObj, IJX4Jsonable, LIntf) then
        begin
          LIntf.JX4Create(True);
          LIntf.JX4Merge(LEle, AOptions);
        end else begin
          TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
          TxRTTI.CallMethodProc('JSONMerge', LObj, [ LEle, TValue.From<TJX4Options>(AOptions)]);
        end;
        Self.Add(LObj);
      except
        FreeAndNil(LObj);
        raise;
      end;
    end;
end;

function TJX4List<T>.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TArray<string>;
  LPartsIdx:  Integer;
  LRes:       string;
  LEle:       T;
  LIOBlock:   TJX4IOBlock;
  LName:      string;
  LTValue:    TValue;
  LIntf:      IJX4Jsonable;
begin

  if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient)) then Exit;
  TJX4Object.RaiseIfAborted(AIOBlock.Options);
  Result := TValue.Empty;

  LPartsIdx := 0;
  if Self.count > 0 then
  begin
    SetLength(LParts, Self.Count);
    LIOBlock := TJX4IOBlock.Create;
    LIOBlock.Init('', Nil, Nil, AIOBlock.Options);
    try
      for LEle in Self do
      begin
        if Supports(LEle, IJX4Jsonable, LIntf) then
        begin
          LTValue := LIntf.JX4Serialize(LIOBlock);
        end else
          LTValue := TxRTTI.CallMethodFunc('JSONSerialize', LEle, [ LIOBlock ]);
        if not LTValue.IsEmpty then
        begin
          LParts[LPartsIdx] := LTValue.AsString;
          Inc(LPartsIdx);
        end;
      end;
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

  SetLength(LParts, LPartsIdx);
  LRes := TJX4Object.JoinStrings(LParts, ',');
  // LRes := string.Join(',', LParts);

  LName := TJX4Object.ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);
  if LName.IsEmpty then Result := '[' + LRes + ']' else Result := '"' + LName + '":[' + LRes + ']';

end;

procedure TJX4List<T>.JX4Clone(ADestObj: TObject; AOptions: TJX4Options);
begin
  JSONClone(TJX4List<T>(ADestObj), AOptions);
end;

procedure TJX4List<T>.JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
begin
  //
end;

procedure TJX4List<T>.JX4Deserialize(AIOBlock: TJX4IOBlock);
begin
  JSONDeserialize(AIOBlock);
end;

function TJX4List<T>.JX4Destroy(AOptions: TJX4Options = []): Boolean;
begin
  Result := True;
end;

procedure TJX4List<T>.JX4Merge(AMergedWith: TObject;
  AOptions: TJX4Options);
begin
  JSONMerge(TJX4List<T>(AMergedWith), AOptions);
end;

function TJX4List<T>.JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
begin
  Result := JSONSerialize(AIOBlock);
end;

function TJX4List<T>.Clone<V>(AOptions: TJX4Options): V;
var
  LIntfSelf: IJX4Jsonable;
  LIntfRes: IJX4Jsonable;
begin
  try
    Result := V.Create;
    TJX4Object.RaiseIfAborted(AOptions);

    if Supports(Self, IJX4Jsonable, LIntfSelf) and supports(Result, IJX4Jsonable, LIntfRes) then
    begin
      LIntfRes.JX4Create(True);
      LIntfSelf.JX4Clone(Result, AOptions);
    end else begin
      TxRTTI.CallMethodProc('JSONCreate', Result, [True]);
      TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
    end;
  except
    on TJX4ExceptionAborted do
    begin
      FreeAndNil(Result);
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      FreeAndNil(Result);
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4List<T>.First: T;
begin
  Result := Nil;
  if Count > 0 then
    Result := Self[0];
end;

function TJX4List<T>.Last: T;
begin
  Result := Nil;
  if Count > 0 then
    Result := Self[Count - 1];
end;

procedure TJX4List<T>.Merge(AMergedWith: TJX4List<T>; AOptions: TJX4Options);
var
  LIntf: IJX4Jsonable;
begin
  try
    TJX4Object.RaiseIfAborted(AOptions);
    if Supports(Self, IJX4Jsonable, LIntf) then
      LIntf.JX4Merge(AMergedWith, AOptions)
    else
      TxRTTI.CallMethodProc('JSONMerge', Self, [ AMergedWith, TValue.From<TJX4Options>(AOptions) ]);
  except
    on TJX4ExceptionAborted do
    begin
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      if not (joNoException in AOptions) then raise;
    end;
  end;
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

function TJX4ListOfValues.IndexOfTValue(const From: TValue): Integer;
begin
  for var i := 0 to Self.count - 1  do
  begin
      case Self.Items[i].TypeKind of
      tkvString:
        if string.Compare(From.AsString, Self.Items[i].AsString, [coIgnoreCase]) = 0 then
          Exit(i);
      tkvInteger:
        if (From.AsInt64 = Self.Items[i].AsInt64) then
          Exit(i);
      tkvFloat:
        if (From.AsExtended = Self.Items[i].AsExtended) then
          Exit(i);
      tkvBool:
        if (From.AsBoolean = Self.Items[i].AsBoolean) then
          Exit(i);
      else
        if (From.AsVariant = Self.Items[i].AsVariant) then
          Exit(i);
      end;
  end;
  Exit(-1);
end;

procedure TJX4ListOfValues.JSONMerge(AMergedWith: TJX4ListOfValues; AOptions: TJX4Options);
var
  LEle: TValue;
  LIdx: Integer;
begin

  if AMergedWith.Count = 0 then Exit;
  if (jmoDelete in AOptions) and (jmoUpdate in AOptions) then
      raise Exception.Create('TJX4ListOfValues.Merge : jmoUpdate and jmoDelete are not compatible');
  TJX4Object.RaiseIfAborted(AOptions);

  if (jmoStat in AOptions) then
  begin
    if not Assigned(FDeleted) then FDeleted := TList<NativeInt>.Create else FDeleted.Clear;
    if not Assigned(FAdded) then FAdded := TList<NativeInt>.Create else FAdded.Clear;
  end;

  for LEle in AMergedWith do
  begin
    LIdx := IndexOf(LEle);
    if (jmoAdd in AOptions) and (LIdx = -1) then
    begin
      Add(LEle);
      Continue;
    end;
    if (jmoDelete in AOptions) and (LIdx <> 1) then
    begin
      Remove(LEle);
      Continue;
    end;
    if (jmoUpdate in AOptions) then
      raise Exception.Create('TJX4ListOfValues.JSONMerge: jmoUpdate not supported');
  end;
end;

function TJX4List<T>.SaveToJSONFile(const AFilename: string; AOptions: TJX4Options; AEncoding: TEncoding; AZipIT: TCompressionLevel; AUseBOM: Boolean): Int64;
begin
  try
    if not Assigned(AEncoding) then AEncoding := TEncoding.UTF8;
    Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self, AOptions), AEncoding, AZipIt, AUseBOM);
  except
    on TJX4ExceptionAborted do
    begin
      Result := -1;
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
       Result := -1;
       if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4List<T>.ToJSON(Options: TJX4Options): string;
begin
  Result := TJX4Object.ToJSON(Self, Options);
end;

function TJX4List<T>.ToYAML(AOptions: TJX4Options): string;
begin
  Result := TJX4Object.ToYAML(Self.ToJSON(AOptions));
end;


class function TJX4List<T>.ToYAML(AStr: string): string;
begin
  Result := TJX4Object.ToYAML(AStr);
end;

procedure TJX4List<T>.JSONClear(AOptions: TJX4Options = []);
begin
  Self.Clear;
end;

procedure TJX4List<T>.JX4Clear(AOptions: TJX4Options = []);
begin
  JSONClear(AOptions);
end;

function TJX4List<T>.Format(AIndentation: Integer): string;
begin
  Result := TJX4Object.FormatJSON(Self.ToJSON, True, AIndentation);
end;

{ TJX4ListNull<V> }

constructor TJX4ListNotOwned<V>.Create;
begin
  inherited Create;
  Self.OwnsObjects := False;
end;

end.
