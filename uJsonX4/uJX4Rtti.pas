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
unit uJX4Rtti;

interface
uses
    System.Generics.Collections
  , SyncObjs
  , RTTI
  , System.TypInfo
  ;

{$DEFINE JX4RTTICACHE} // Highly recommended : 200% SpeedUp !

type
  TxRTTI = class abstract
    class function  GetPropsList(Instance: Pointer; ObjectClass: TClass): TDictionary<string, variant>;
    class function  GetField(AObj: TObject; AField: string): TRTTIFIeld; static;
    class function  GetFields(aObj: TObject): TArray<TRTTIField>; overload;
    class function  GetFields(AClass: TClass): TArray<TRttiField>; overload;
    class function  GetProps(aObj: TObject): TArray<TRTTIProperty>; static;
    class function  GetMethods(aObj: TObject): TArray<TRTTIMethod>; static;
    class function  GetMethod(aObj: TObject; const AName: string): TRTTIMethod; overload; static;
    class function  GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod overload; static;
    class function  GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute; static;
    class function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType; static;
    class function  CreateObject(AInstance: TRTTIInstanceType): TObject; overload; inline;
    class function  CreateObject(AClass: TClass): TObject; overload; inline;
    class procedure CallMethodProc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue); overload;
    class function  CallMethodFunc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue): TValue;
    class function  FieldIsTValue(AField: TRttiField; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
    class function  FieldAsTValue(AObj: TObject; AField: TRttiField; var AValue: TValue; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
    class function  FieldIsTObject(AField: TRttiField; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
    class function  FieldAsTObject(ASelf: TObject; AField: TRttiField; var AObject: TObject; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
  end;

  {$IFDEF JX4RTTICACHE}
var
  _RTTIctx: TRttiContext;
  _RTTILock1: TCriticalSection;
  _RTTILock2: TCriticalSection;
  _RTTILock3: TCriticalSection;
  _RTTILock4: TCriticalSection;
  _RTTILock5: TCriticalSection;
  _RTTILock6: TCriticalSection;
  _RTTILock7: TCriticalSection;
  _RTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _RTTIMethsCacheDic: TDictionary<TClass, TArray<TRTTIMethod>>;
  _RTTIMethObjCacheDic: TDictionary<NativeInt, TRTTIMethod>;
  _RTTIInstMethsCacheDic: TDictionary<TRttiInstanceType, TRTTIMethod>;
  _RTTIInstCacheDic: TDictionary<TRTTIField, TRttiInstanceType>;

{$ELSE}
var
  _RTTIctx: TRttiContext;
{$ENDIF}
implementation
uses
    StrUtils
  , Sysutils
  ;

class function TxRTTI.GetPropsList(Instance: Pointer; ObjectClass: TClass): TDictionary<string, variant>;
begin
  Result := TDictionary<string, variant>.Create;
  var AValue: TValue;
     for var AField in TxRtti.GetFields(Instance) do
  begin
     if (AField.FieldType.TypeKind in [tkRecord])
      and (AField.FieldType.Handle = TypeInfo(TValue))
      and (AField.GetValue(Instance).TryAsType<TValue>(AValue))
   then
      Result.Add(AField.Name, AValue.AsVariant);
  end;
end;

class function TxRTTI.FieldIsTValue(AField: TRttiField; AVisibilities: TMemberVisibilities): Boolean;
begin
  Result := (AField.Visibility in AVisibilities)
    and (AField.FieldType.TypeKind in [tkRecord])
    and (AField.FieldType.Handle = TypeInfo(TValue));
end;

class function TxRTTI.FieldAsTValue(AObj: TObject; AField: TRttiField; var AValue: TValue; AVisibilities: TMemberVisibilities): Boolean;
begin
  Result := (AField.Visibility in AVisibilities)
    and (AField.FieldType.TypeKind in [tkRecord])
    and (AField.FieldType.Handle = TypeInfo(TValue))
    and (AField.GetValue(AObj).TryAsType<TValue>(AValue));
end;

class function TxRTTI.FieldIsTObject(AField: TRttiField; AVisibilities: TMemberVisibilities): Boolean;
begin
  Result := (AField.Visibility in AVisibilities) and (AField.FieldType.TypeKind in [tkClass]);
end;

class function TxRTTI.FieldAsTObject(ASelf: TObject; AField: TRttiField; var AObject: TObject; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
begin
  Result := (AField.Visibility in AVisibilities) and (AField.FieldType.TypeKind in [tkClass]);
  if Result then AObject := AField.GetValue(ASelf).AsObject else AObject := Nil;
end;

class procedure TxRTTI.CallMethodProc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue);
var
  LMeth: TRttiMethod;
begin
  LMeth := TxRTTI.GetMethod(AObj, AMethod);
  if Assigned(LMeth) then LMeth.Invoke(AObj, AArgs);
end;

class function TxRTTI.CallMethodFunc(const AMethod: string; const AObj: TObject; const AArgs: array of TValue): TValue;
var
  LMeth: TRttiMethod;
begin
  LMeth := TxRTTI.GetMethod(AObj, AMethod);
  if not Assigned(LMeth) then Exit(TValue.Empty);
  Result := LMeth.Invoke(AObj, AArgs);
  if not Result.IsEmpty then Result := Result.AsType<TValue>;
end;

class function TxRTTI.CreateObject(AInstance: TRTTIInstanceType): TObject;
var
  LMethod: TRTTIMethod;
begin
  Result := Nil;
  LMethod := GetMethod(AInstance, 'Create');
  if not Assigned(LMethod) then Exit;
  Result := LMethod.Invoke(AInstance.MetaclassType,[]).AsObject;
end;

class function TxRTTI.CreateObject(AClass: TClass): TObject;
begin
   Result := TxRTTI.CreateObject(_RTTIctx.GetType(AClass).AsInstance);
end;

class function TxRTTI.GetField(AObj: TObject; AField: string): TRTTIFIeld;
begin
  Result := _RTTIctx.GetType(AObj.ClassType).GetField(AField);
end;

class function TxRTTI.GetFields(aObj: TObject): TArray<TRTTIField>;
{$IFDEF JX4RTTICACHE}
var
  CType: TClass;
begin
  _RTTILock1.Enter;
  CType := aObj.ClassType;
  if not _RTTIFieldsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetFields;
    _RTTIFieldsCacheDic.Add(CType, Result);
  end;
    _RTTILock1.Leave;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetFields;
end;
{$ENDIF}

class function TxRTTI.GetFields(AClass: TClass): TArray<TRTTIField>;
{$IFDEF JX4RTTICACHE}
var
  CType: TClass;
begin
  _RTTILock7.Enter;
  if not _RTTIFieldsCacheDic.TryGetValue(AClass, Result) then
  begin
    Result :=  _RTTIctx.GetType(AClass).GetFields;
    _RTTIFieldsCacheDic.Add(AClass, Result);
  end;
    _RTTILock7.Leave;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(AClass).GetFields;
end;
{$ENDIF}


class function TxRTTI.GetProps(aObj: TObject): TArray<TRTTIProperty>;
{$IFDEF JX4RTTICACHE}
var
  CType: TClass;
begin
  _RTTILock2.Enter;
  CType := aObj.ClassType;
  if not _RTTIPropsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetProperties;
    _RTTIPropsCacheDic.Add(CType, Result);
  end;
    _RTTILock2.Leave;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
end;
{$ENDIF}

class function TxRTTI.GetMethods(aObj: TObject): TArray<TRTTIMethod>;
{$IFDEF JX4RTTICACHE}
var
  CType: TClass;
begin
  _RTTILock3.Enter;
  CType := aObj.ClassType;
  if not _RTTIMethsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetMethods;
    _RTTIMethsCacheDic.Add(CType, Result);
  end;
    _RTTILock3.Leave;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetMethods;
end;
{$ENDIF}

class function TxRTTI.GetMethod(AObj: TObject; const AName: string): TRTTIMethod;
{$IFDEF JX4RTTICACHE}
var
  Lx: NativeInt;
begin
  _RTTILock4.Enter;
  Lx := NativeInt(AObj.ClassType) + AName.GetHashCode;
  if  not _RTTIMethObjCacheDic.TryGetValue(Lx, Result)  then
  begin
    Result :=  _RTTIctx.GetType(AObj.ClassType).GetMethod(AName);
    _RTTIMethObjCacheDic.Add(Lx, Result);
  end;
  _RTTILock4.Leave;
end;
{$ELSE}
begin
  Result :=  _RTTIctx.GetType(AObj.ClassType).GetMethod(AName);
end;
{$IFEND}


class function TxRTTI.GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod;
{$IFDEF JX4RTTICACHE}
begin
  _RTTILock5.Enter;
  if not _RTTIInstMethsCacheDic.TryGetValue(AInstance, Result) then
  begin
    Result := AInstance.GetMethod(AName);
    _RTTIInstMethsCacheDic.Add(AInstance, Result);
  end;
  _RTTILock5.Leave;
end;
{$ELSE}
begin
  Result :=  AInstance.GetMethod(AName);
end;
{$IFEND}

class function TxRTTI.GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;

  function GetRTTIFieldAttribute(RTTIField: TRTTIField; AttrClass: TClass): TCustomAttribute; inline;
  begin
  {$IF CompilerVersion >= 35.0} // Alexandria 11.0
   Result := RTTIField.GetAttribute(TCustomAttributeClass(AttrClass));
  {$ELSE}
    Result := Nil;
    for var Attr in RTTIField.GetAttributes do
      if Attr.ClassType = AttrClass then
      begin
          Result := Attr;
          Break;
      end;
  {$IFEND}
  end;

begin
  Result := GetRTTIFieldAttribute(Field, AttrClass);
end;

class function TxRTTI.GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;
{$IFDEF JX4RTTICACHE}
begin
  _RTTILock6.Enter;
  if not _RTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
    Result := Field.FieldType.AsInstance;
    _RTTIInstCacheDic.Add(Field, Result);
  end;
  _RTTILock6.Leave;
end;
{$ELSE}
begin
    Result := Field.FieldType.AsInstance;
end;
{$ENDIF}

initialization
{$IFDEF JX4RTTICACHE}
  _RTTIFieldsCacheDic := TDictionary<TClass, TArray<TRttiField>>.Create;
  _RTTIPropsCacheDic := TDictionary<TClass, TArray<TRttiProperty>>.Create;
  _RTTIMethsCacheDic := TDictionary<TClass, TArray<TRttiMEthod>>.Create;
  _RTTIInstCacheDic := TDictionary<TRTTIField, TRttiInstanceType>.Create;
  _RTTIInstMethsCacheDic := TDictionary<TRttiInstanceType, TRTTIMethod>.Create;
  _RTTIMethObjCacheDic := TDictionary<NativeInt, TRTTIMethod>.Create;
  _RTTILock1 := TCriticalSection.Create;
  _RTTILock2 := TCriticalSection.Create;
  _RTTILock3 := TCriticalSection.Create;
  _RTTILock4 := TCriticalSection.Create;
  _RTTILock5 := TCriticalSection.Create;
  _RTTILock6 := TCriticalSection.Create;
  _RTTILock7 := TCriticalSection.Create;
{$ENDIF}
finalization
{$IFDEF JX4RTTICACHE}
  _RTTILock7.Free;
  _RTTILock6.Free;
  _RTTILock5.Free;
  _RTTILock4.Free;
  _RTTILock3.Free;
  _RTTILock2.Free;
  _RTTILock1.Free;
  _RTTIFieldsCacheDic.Free;
  _RTTIPropsCacheDic.Free;
  _RTTIMethsCacheDic.Free;
  _RTTIMethObjCacheDic.Free;
  _RTTIInstMethsCacheDic.Free;
  _RTTIInstCacheDic.Free;
{$ENDIF}
end.
