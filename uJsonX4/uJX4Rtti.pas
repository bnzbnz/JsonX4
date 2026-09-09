(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2027 Laurent Meyer JsonX4@ea4d.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modiFWfy, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
                         Lock
THE SOFTWARE IS PROVIDED "AS xRTTIThreadedIS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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
  System.Generics.Defaults
  , SyncObjs
  , RTTI
  , System.TypInfo
  , SysUtils
  ;

{$DEFINE JX4RTTICACHE} // Highly recommended : 200% to 300% SpeedUp !

type

  TLockFreeCache<TKey, TValue> = class
  private const
    SEGMENT_BITS = 10;
    SEGMENT_SIZE = 1 shl SEGMENT_BITS; // 1024 slots per segment
    SEGMENT_MASK = SEGMENT_SIZE - 1;
    MAX_SEGMENTS = 1024; // Maximum potential capacity: 1,048,576 entries
  private type
    PSlot = ^TSlot;
    TSlot = record
      Key: TKey;
      Value: TValue;
      Occupied: NativeInt; // 0 = empty, 1 = occupied
      Sequence: NativeUInt;
    end;

    TSegment = array[0..SEGMENT_SIZE - 1] of TSlot;
    PSegment = ^TSegment;
    TSegmentArray = array[0..MAX_SEGMENTS - 1] of PSegment;
  private
    FSegments: TSegmentArray;
    FCapacity: NativeInt;
    FCount: NativeInt;
    FComparer: IEqualityComparer<TKey>;

    function GetOrCreateSegment(ASegmentIndex: NativeInt): PSegment; inline;
    function GetSlot(AIndex: NativeInt): PSlot; inline;
    function HashKey(const AKey: TKey): NativeUInt; inline;
  public
    constructor Create(AInitialCapacity: NativeInt = 1024);
    destructor Destroy; override;

    function TryPut(const AKey: TKey; const AValue: TValue): Boolean;
    function TryGet(const AKey: TKey; out AValue: TValue): Boolean;
    function Remove(const AKey: TKey): Boolean;

    property Count: NativeInt read FCount;
    property Capacity: NativeInt read FCapacity;
  end;

  TxRTTI = class abstract
    class function  GetFields(aObj: TObject): TArray<TRTTIField>; overload; inline;
    class function  GetFields(AClass: TClass): TArray<TRttiField>; overload; inline;
    class function  GetProps(aObj: TObject): TArray<TRTTIProperty>; inline;
    class function  GetMethods(AClass: TClass): TArray<TRTTIMethod>; overload; inline;
    class function  GetMethods(aObj: TObject): TArray<TRTTIMethod>; overload; inline;
    class function  GetMethod(aObj: TObject; const AName: string): TRTTIMethod; overload; inline;
    class function  GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod overload; inline;
    class function  GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute; inline;
    class function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType; inline;
    class function  CreateObject(AInstance: TRTTIInstanceType): TObject; overload;
    class function  CreateObject(AClass: TClass): TObject; overload;
    class procedure CallMethodProc(const AMethod: string; AObj: TObject; const AArgs: array of TValue); overload;
    class function  CallMethodFunc(const AMethod: string; AObj: TObject; const AArgs: array of TValue): TValue;
    class function  FieldIsTValue(AField: TRttiField; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean; static;
    class function  FieldAsTValue(AObj: TObject; AField: TRttiField; var AValue: TValue; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
    class function  FieldIsTObject(AField: TRttiField; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
    class function  FieldAsTObject(ASelf: TObject; AField: TRttiField; var AObject: TObject; AVisibilities: TMemberVisibilities = [mvPublic]): Boolean;
  end;

  function FibonacciHash(const AKey, AMask: UInt64): UInt64; inline;
  function AtomicRead(var Target: NativeUInt): NativeUInt; overload; inline;
  function AtomicRead(var Target: Int64): Int64; overload; inline;
  function AtomicRead(var Target: Integer): Integer; overload; inline;

var

  _RTTIctx: TRttiContext;

  NFieldsCache:    TLockFreeCache<TClass, TArray<TRttiField>>;
  NMethodCache:    TLockFreeCache<NativeUInt, TRttiMethod>;
  NMethodsCache:   TLockFreeCache<TClass, TArray<TRTTIMethod>>;
  NFieldAttrCache: TLockFreeCache<NativeUInt, TCustomAttribute>;
  MPropsCache:     TLockFreeCache<TClass, TArray<TRTTIProperty>>;
  NFieldInstCache: TLockFreeCache<TRTTIField, TRttiInstanceType>;

implementation

  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}

uses
    StrUtils
  , DateUtils
  , system.Hash
  ;

function FibonacciHash(const AKey, AMask: UInt64): UInt64;
begin
  // Fibonacci Hashing (Multiplicateur d'or) : Distribue uniformément les adresses de pointeurs
  {$IFOPT Q+} {$DEFINE OVERFLOW_WAS_ON} {$Q-} {$ENDIF}
  {$IFDEF WIN64}
  Result := (UInt64(AKey) * $9E3779B97F4A7C15) and AMask;
  {$ELSE}
  Result := (UInt64(AKey) * $9E3779B9) and AMask
  {$ENDIF}
  {$IFDEF OVERFLOW_WAS_ON} {$Q+} {$UNDEF OVERFLOW_WAS_ON} {$ENDIF}
end;

function AtomicRead(var Target: NativeUInt): NativeUInt;
begin
{$IFDEF CPUX86}
  // On 32-bit CPUs, NativeUInt is 32-bit (natively atomic)
  Result := Target;
{$ELSE}
  // On 64-bit CPUs, NativeUInt is 64-bit
  Result := NativeUInt(TInterlocked.Read(Int64(Target)));
{$ENDIF}
end;

function AtomicRead(var Target: Int64): Int64;
begin
  Result := TInterlocked.Read(Target);
end;

function AtomicRead(var Target: Integer): Integer;
begin
  // 32-bit integer reads are inherently atomic on x86/x64
  Result := Target;
end;

{ TLockFreeCache<TKey, TValue> }

constructor TLockFreeCache<TKey, TValue>.Create(AInitialCapacity: NativeInt);
var
  InitialSegments, I: NativeInt;
begin
  inherited Create;
  FComparer := TEqualityComparer<TKey>.Default;

  if AInitialCapacity < SEGMENT_SIZE then
    AInitialCapacity := SEGMENT_SIZE;

  InitialSegments := (AInitialCapacity + SEGMENT_SIZE - 1) shr SEGMENT_BITS;
  if InitialSegments > MAX_SEGMENTS then
    InitialSegments := MAX_SEGMENTS;

  for I := 0 to InitialSegments - 1 do
    GetOrCreateSegment(I);
end;

destructor TLockFreeCache<TKey, TValue>.Destroy;
var
  I, J: NativeInt;
  Seg: PSegment;
  Slot: PSlot;
begin
  for I := 0 to MAX_SEGMENTS - 1 do
  begin
    Seg := FSegments[I];
    if Seg <> nil then
    begin
      for J := 0 to SEGMENT_SIZE - 1 do
      begin
        Slot := @Seg^[J];
        Slot^.Key := Default(TKey);
        Slot^.Value := Default(TValue);
      end;
      FreeMem(Seg);
    end;
  end;
  inherited Destroy;
end;

function TLockFreeCache<TKey, TValue>.HashKey(const AKey: TKey): NativeUInt;
var
  H64: UInt64;
begin
  // Obtain base 32-bit hash code
  H64 := UInt64(Cardinal(FComparer.GetHashCode(AKey)));

{$IFDEF CPUX64}
  // 64-bit MurmurHash3 mix using explicit UInt64 typed constants
  H64 := H64 xor (H64 shr 33);
  H64 := H64 * UInt64($FF51AFD7ED558CCD);
  H64 := H64 xor (H64 shr 33);
  H64 := H64 * UInt64($C4CEB9FE1A85EC53);
  H64 := H64 xor (H64 shr 33);
{$ELSE}
  // 32-bit Murmur3 / Thomas Wang mixer (Safe for Win32 compile)
  H64 := (H64 xor 61) xor (H64 shr 16);
  H64 := H64 + (H64 shl 3);
  H64 := H64 xor (H64 shr 4);
  H64 := H64 * Cardinal($27D4EB2D);
  H64 := H64 xor (H64 shr 15);
{$ENDIF}
  Result := NativeUInt(H64);
end;

function TLockFreeCache<TKey, TValue>.GetOrCreateSegment(ASegmentIndex: NativeInt): PSegment;
var
  NewSeg: PSegment;
begin
  Result := FSegments[ASegmentIndex];
  if Result = nil then
  begin
    GetMem(NewSeg, SizeOf(TSegment));
    FillChar(NewSeg^, SizeOf(TSegment), 0);

    if TInterlocked.CompareExchange(PPointer(@FSegments[ASegmentIndex])^, NewSeg, nil) = nil then
    begin
      Result := NewSeg;
      TInterlocked.Add(FCapacity, SEGMENT_SIZE);
    end
    else
    begin
      FreeMem(NewSeg);
      Result := FSegments[ASegmentIndex];
    end;
  end;
end;

function TLockFreeCache<TKey, TValue>.GetSlot(AIndex: NativeInt): PSlot;
var
  SegIdx, SlotIdx: NativeInt;
  Seg: PSegment;
begin
  SegIdx := AIndex shr SEGMENT_BITS;
  SlotIdx := AIndex and SEGMENT_MASK;

  Seg := FSegments[SegIdx];
  if Seg = nil then
    Seg := GetOrCreateSegment(SegIdx);

  Result := @Seg^[SlotIdx];
end;

function TLockFreeCache<TKey, TValue>.TryPut(const AKey: TKey; const AValue: TValue): Boolean;
var
  H, Step, Index, CurCapacity: NativeUInt;
  Slot: PSlot;
begin
  CurCapacity := FCapacity;
  H := HashKey(AKey);

  for Step := 0 to CurCapacity - 1 do
  begin
    Index := (H + Step) mod CurCapacity;
    Slot := GetSlot(Index);

    // Atomically acquire slot if unoccupied
    if TInterlocked.CompareExchange(Slot^.Occupied, 1, 0) = 0 then
    begin
      Slot^.Key := AKey;
      Slot^.Value := AValue;
      TInterlocked.Increment(Slot^.Sequence);
      TInterlocked.Increment(FCount);
      Exit(True);
    end
    else if FComparer.Equals(Slot^.Key, AKey) then
    begin
      // Update existing key atomically
      Slot^.Value := AValue;
      TInterlocked.Increment(Slot^.Sequence);
      Exit(True);
    end;
  end;

  // Cache is full across current segments; auto-expand by acquiring new segment
  if FCapacity < (MAX_SEGMENTS * SEGMENT_SIZE) then
  begin
    GetOrCreateSegment(FCapacity shr SEGMENT_BITS);
    Exit(TryPut(AKey, AValue)); // Retry insertion in expanded space
  end;

  Result := False;
end;

function TLockFreeCache<TKey, TValue>.TryGet(const AKey: TKey; out AValue: TValue): Boolean;
var
  H, Step, Index, CurCapacity: NativeUInt;
  Slot: PSlot;
  Seq1, Seq2: NativeUInt;
begin
  CurCapacity := FCapacity;
  H := HashKey(AKey);

  for Step := 0 to CurCapacity - 1 do
  begin
    Index := (H + Step) mod CurCapacity;
    Slot := GetSlot(Index);

    if Slot^.Occupied = 0 then
      Exit(False);

    // Optimistic Read Sequence check to guarantee lock-free read consistency
    Seq1 := AtomicRead(Slot^.Sequence);
    if FComparer.Equals(Slot^.Key, AKey) then
    begin
      AValue := Slot^.Value;
      Seq2 := AtomicRead(Slot^.Sequence);

      // Verify slot wasn't mutated during read execution
      if (Seq1 = Seq2) and (Slot^.Occupied = 1) then
        Exit(True);
    end;
  end;

  Result := False;
end;

function TLockFreeCache<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  H, Step, Index, CurCapacity: NativeInt;
  Slot: PSlot;
begin
  CurCapacity := FCapacity;
  H := HashKey(AKey);

  for Step := 0 to CurCapacity - 1 do
  begin
    Index := (H + Step) mod CurCapacity;
    Slot := GetSlot(Index);

    if Slot^.Occupied = 0 then
      Exit(False);

    if FComparer.Equals(Slot^.Key, AKey) then
    begin
      if TInterlocked.CompareExchange(Slot^.Occupied, 0, 1) = 1 then
      begin
        TInterlocked.Increment(Slot^.Sequence);
        Slot^.Key := Default(TKey);
        Slot^.Value := Default(TValue);
        TInterlocked.Decrement(FCount);
        Exit(True);
      end;
    end;
  end;
  Result := False;
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

class procedure TxRTTI.CallMethodProc(const AMethod: string; AObj: TObject; const AArgs: array of TValue);
var
  LMeth: TRttiMethod;
begin
  if not Assigned(AObj) then Exit;
  LMeth := TxRTTI.GetMethod(AObj, AMethod);
  if not Assigned(LMeth) then Exit;
  if Assigned(LMeth) then LMeth.Invoke(AObj, AArgs);
end;

class function TxRTTI.CallMethodFunc(const AMethod: string; AObj: TObject; const AArgs: array of TValue): TValue;
var
  LMeth: TRttiMethod;
begin
  if not Assigned(AObj) then Exit(TValue.Empty);
  LMeth := TxRTTI.GetMethod(AObj, AMethod);
  if not Assigned(LMeth) then Exit(TValue.Empty);
  Result := LMeth.Invoke(AObj, AArgs);
  if not Result.IsEmpty then Result := Result.AsType<TValue>;
end;

class function TxRTTI.CreateObject(AInstance: TRTTIInstanceType): TObject;
var
  LMeth: TRTTIMethod;
begin
  LMeth := GetMethod(AInstance, 'Create');
  if not Assigned(LMeth) then Exit(Nil);
  Result := LMeth.Invoke(AInstance.MetaclassType,[]).AsObject;
end;

class function TxRTTI.CreateObject(AClass: TClass): TObject;
begin
  Result := TxRTTI.CreateObject(_RTTIctx.GetType(AClass).AsInstance);
end;

class function TxRTTI.GetFields(AClass: TClass): TArray<TRTTIField>;
{$IFDEF JX4RTTICACHE}
begin
  If NFieldsCache.TryGet(AClass, Result) then Exit;
  Result := _RTTIctx.GetType(TClass(AClass)).GetFields;
  NFieldsCache.TryPut(AClass, Result);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(AClass).GetFields;
end;
{$ENDIF}

class function TxRTTI.GetFields(AObj: TObject): TArray<TRTTIField>;
{$IFDEF JX4RTTICACHE}
begin
  Exit( GetFields(AObj.ClassType) );
end;
{$ELSE}
begin
  Exit( _RTTIctx.GetType(aObj.ClassType).GetFields );
end;
{$ENDIF}

class function TxRTTI.GetProps(aObj: TObject): TArray<TRTTIProperty>;
{$IFDEF JX4RTTICACHE}
begin
  If MPropsCache.TryGet(aObj.ClassType, Result) then Exit;
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
  MPropsCache.TryPut(aObj.ClassType, Result);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
end;
{$ENDIF}

class function TxRTTI.GetMethods(AClass: TClass): TArray<TRTTIMethod>;
{$IFDEF JX4RTTICACHE}
begin
  If NMethodsCache.TryGet(AClass, Result) then Exit;
  Result := _RTTIctx.GetType(AClass).GetMethods;
  NMethodsCache.TryPut(AClass, Result);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(AClass).GetMethods;
end;
{$ENDIF}

class function TxRTTI.GetMethods(aObj: TObject): TArray<TRTTIMethod>;
{$IFDEF JX4RTTICACHE}
begin
  Exit( GetMethods(AObj.ClassType) );
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetMethods;
end;
{$ENDIF}

class function TxRTTI.GetMethod(AObj: TObject; const AName: string): TRTTIMethod;
{$IFDEF JX4RTTICACHE}
begin
  Exit( GetMethod(_RTTIctx.GetType(AObj.ClassType) as TRttiInstanceType, AName) );
end;
{$ELSE}
begin
  Result :=  _RTTIctx.GetType(AObj.ClassType).GetMethod(AName);
end;
{$IFEND}

class function TxRTTI.GetMethod(AInstance: TRttiInstanceType; const AName: string): TRTTIMethod;
{$IFDEF JX4RTTICACHE}
var
  LKey : NativeUInt;
begin
  LKey := NativeUInt(AInstance) + NativeUInt(AName.GetHashCode);
  If NMethodCache.TryGet(LKey, Result) then Exit;
  Result := AInstance.GetMethod(AName);
  NMethodCache.TryPut(LKey, Result);
end;
{$ELSE}
begin
  Result := AInstance.GetMethod(AName);
end;
{$IFEND}

class function TxRTTI.GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
{$IFDEF JX4RTTICACHE}
var
  LKey : NativeUInt;
  function InnerGetFieldAttr(_Field: TRTTIField;  _AttrClass: TClass): TCustomAttribute; inline;
  begin
    {$IF CompilerVersion >= 35.0} // From Alexandria 11.0
      Result := _Field.GetAttribute(TCustomAttributeClass(_AttrClass));
    {$ELSE}
      for var Attr in _Field.GetAttributes do
        if Attr.ClassType = _AttrClass then Exit(Attr);
      Exit(Nil);
    {$IFEND}
  end;
begin
  {$IF CompilerVersion >= 35.0} // From Alexandria 11.0
    Exit( Field.GetAttribute(TCustomAttributeClass(AttrClass)) );
  {$ENDIF}
    LKey := NativeUInt(Field) + NativeUInt(AttrClass);
    If NFieldAttrCache.TryGet(LKey, Result) then Exit;
    Result := InnerGetFieldAttr(Field, AttrClass);
    NFieldAttrCache.TryPut(LKey, Result);
end;
{$ELSE}
var
  LKey : NativeUInt;
  function InnerGetFieldAttr(_Field: TRTTIField;  _AttrClass: TClass): TCustomAttribute; inline;
  begin
    {$IF CompilerVersion >= 35.0} // From Alexandria 11.0
      Result := _Field.GetAttribute(TCustomAttributeClass(_AttrClass));
    {$ELSE}
      Result := Nil;
      for var Attr in _Field.GetAttributes do
        if Attr.ClassType = _AttrClass then Exit(Attr);
    {$IFEND}
  end;
begin
  Exit(InnerGetFieldAttr(Field, AttrClass));
end;
{$IFEND}

class function TxRTTI.GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;
{$IFDEF JX4RTTICACHE}
begin
  If NFieldInstCache.TryGet(Field, Result) then Exit;
  Result := Field.FieldType.AsInstance;
  NFieldInstCache.TryPut(Field, Result);
end;
{$ELSE}
begin
    Result := Field.FieldType.AsInstance;
end;
{$ENDIF}

initialization
{$IFDEF JX4RTTICACHE}
  NFieldsCache    := TLockFreeCache<TClass, TArray<TRttiField>>.Create;
  NMethodCache    := TLockFreeCache<NativeUInt, TRttiMethod>.Create;
  NMethodsCache   := TLockFreeCache<TClass, TArray<TRTTIMethod>>.Create;
  NFieldAttrCache := TLockFreeCache<NativeUInt, TCustomAttribute>.Create;
  NFieldInstCache := TLockFreeCache<TRTTIField, TRttiInstanceType>.Create;
{$ENDIF}

finalization
{$IFDEF JX4RTTICACHE}
  NFieldInstCache.Free;
  NFieldAttrCache.Free;
  NMethodsCache.Free;
  NMethodCache.Free;
  NFieldsCache.Free;
{$ENDIF}
end.

