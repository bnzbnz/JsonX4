
(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2027 Laurent Meyer JsonX4@lmeyer.fr

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software withoForatut restriction, including without limitation the rights
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
unit uJX4Object;
{$HINTS OFF}

interface
uses
  Classes
  , System.Generics.Collections
  , RTTI
  , TypInfo
  , JSON
  , SysUtils
  , zLib
  ;

const
  CJX4Version = $0432; // 04.50
  CBoolToStr: array[Boolean] of string = ('false','true');
  null = Nil;

type


  PValueRecord = ^TValueRecord;
  TValueRecord = record
    FData: TValueData;
  end;

  PStrRec = ^TStrRec;
  TStrRec = packed record
    {$IF Defined(CPUX64)}
    _Padding: LongInt;    // Required for 16-byte CPU alignment layout on 64-bit
    {$IFEnd}
    codePage: Word;       // Offset -12: String encoding code page
    elemSize: Word;       // Offset -10: Element size in bytes (2 for UnicodeString)
    refCnt: Longint;      // Offset -8 : Reference count indicator
    length: Longint;      // Offset -4 : Total string character length
  end;

  PValue = ^TValue;
  PValueData = ^TValueData;

  TValueTag = (vtEmpty, vtInteger, vtCardinal, vtInt64, vtFloat,
               vtString, vtObject, vtInterface, vtPointer, vtRecord);

  PValueHeader = ^TValueHeader;
  TValueHeader = record
    FData: Pointer;        // raw pointer to the value
    FTag: TValueTag;       // small type discriminator
    FExtra: NativeInt;     // optional: size, refcount pointer, etc.
  end;


  sFormatType= (sftYAML, sftJSON);

  TJX4Option  = (
      // Remove Null fields when serializing
        joNullToEmpty
      // Do NOT Re-Raise internal exceptions
      , joNoException
      // Abort the current thread/task and raise this exception
      , joRaiseOnAbort
      // Raise Exception when a JSON field is not define in the matching Delphi object
      , joRaiseOnMissingField
      // Envode slash, usefull for use as HTML
      , joSlashEncode
      // Merge Options
      // Enable statistics lists: EleAdded, EleUpdated, EleDeleted
      , jmoStat
      // Add Merge Operation
      , jmoAdd
      // Add Merge Operation :
      , jmoUpdate, jmoByMoving
      // Delete Merge Operation: [1,2,3] delete by ['2'] = [1,3]
      , jmoDelete
      // Purge Merge Operation : [1,2,3] purge by [2'] = [2]
      , jmoPurge
  );

  TJX4Options = set of TJX4Option;

  TJX4Name = class(TCustomAttribute)
  public
    Name:       string;
    constructor Create(const AName: string);
  end;

  TJX4Default = class(TCustomAttribute)
  public
    Value:      TValue;
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: Int64); overload;
    constructor Create(const AValue: Boolean); overload;
    constructor Create(const AValue: Extended); overload;
    constructor Create(const ANilValue: Pointer); overload;
  end;

  TJX4Required = class(TCustomAttribute);

  TJX4Transient = class(TCustomAttribute);

  TJX4Unmanaged = class(TCustomAttribute);

  TJX4NotOwned = class(TCustomAttribute);

  TJX4IOBlock = class
    FVal:       TJSONValue;
    // In
    JObj:       TJSONObject;
    JArr:       TJSONArray;
    JsonName:   string;
    Field:      TRttiField;
    Options:    TJX4Options;
    procedure   SetVal(AJVal: TJSONValue);
    property    JVal: TJSONValue read FVal write SetVal;
    // Out
    constructor Create(const AJsonName: string = ''; AJVal: TJSONValue = Nil; AField: TRttiField = Nil; AOptions: TJX4Options = []);
    procedure   Init(const AJsonName: string; AJVal: TJSONValue; AField: TRttiField; AOptions: TJX4Options);
  end;

  TJX4ExceptionAborted = class(Exception);

  IJX4Jsonable = interface(IInterface)
    ['{08F124FF-EC83-40FC-BB7F-4C70379E08DE}']
    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    procedure JX4Clear(AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
  end;

  TJX4Object = class(TObject, IJX4Jsonable)
  private
    FRefCount:      Int64;
  protected
    { IInterface }
    function        QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function        _AddRef: Integer; stdcall;
    function        _Release: Integer; stdcall;
    { TJX4Object }
    class function  GetStreamEncoding(AStream: TStream): TEncoding;
  public
    function  JX4Serialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JX4Deserialize(AIOBlock: TJX4IOBlock);
    procedure JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
    procedure JX4Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure JX4Clone(ADestObj: TObject; AOptions: TJX4Options = []);
    function  JX4Destroy(AOptions: TJX4Options = []): Boolean;
    procedure JX4Clear(AOptions: TJX4Options = []);

    constructor     Create;
    destructor      Destroy; override;
    class procedure RaiseIfAborted(AOptions: TJX4Options); static; inline;

    function        JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure       JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure       JSONClone(ADestObj: TObject; AOptions: TJX4Options);
    procedure       JSONMerge(AMergedWith: TObject; AOptions: TJX4Options = []);
    procedure       JSONClear(AOptions: TJX4Options);

    class function  New<T:class, constructor>: T;
    class function  ToJSON(AObj: TObject; AOptions: TJX4Options = []): string; overload;
    function        ToJSON(AOptions: TJX4Options = []): string; overload;
    class function  FromJSON<T:class, constructor>(const AJson: string; AOptions: TJX4Options = []): T; overload;
    class function  ToJSONStream(AObj: TObject; AOptions: TJX4Options = []): TStream; overload;
    class function  ToYAML(const AStr: string; AOptions: TJX4Options = []): string; overload;
    function        ToYAML(AOptions: TJX4Options = []): string; overload;
    class function  FromYAML<T:class, constructor>(const AYaml: string; AOptions: TJX4Options = []): T;

    function        Clone<T:class, constructor>(AOptions: TJX4Options= []): T; overload;
    function        Merge(AMergedWith: TObject; AOptions: TJX4Options = []): Boolean;
    function        Format(ABeautify: Boolean = True; AIndentation: Integer = 2; AOptions: TJX4Options = []): string;
    procedure       Clear(AOptions: TJX4Options);

    // Utils
    class function  Version: string;
    class function  VersionValue: integer;
    class function  Author: string;
    class function  Contact: string;

    class function  NameDecode(const ToDecode: string): string; static; static;
    class function  NameEncode(const ToEncode: string): string; static; static; inline;
    class function  ExtractFieldName(const AField: TRttiField; ADefault: string = ''): string; static;
    class procedure VarEscapeJSONStr(var AStr: string; const SlashEncode: Boolean); overload; static;
    class function  EscapeJSONStr(const AStr: string; const SlashEncode: Boolean): string; overload; static;
    class function  JoinStrings(const AArray: TArray<string>; const ADelimiter: string): string;
    class function  FormatJSON(const AJson: string; ABeautify: Boolean = True; AIndentation: Integer = 2): string; static;

    class function  Validate(const AJson: string): Boolean; static;
    class function  IsJSON(AStr: string): Boolean; static;

    // Common
    class function  LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding = Nil): Int64; overload;
    class function  SaveToFile(const AFilename: string; const AStr: string; AEncoding: TEncoding; AZipIt: TCompressionLevel = clNone; UseBOM: Boolean = False): Int64; overload;

    // JSON
    class function  LoadFromJSONFile<T:class, constructor>(const AFilename: string; AOptions: TJX4Options = []; AEncoding: TEncoding = Nil): T; overload;
    function        SaveToJSONFile(  const AFilename: string;
                      ABeautify: Boolean = False;
                      AOptions: TJX4Options = [];
                      AEncoding: TEncoding = Nil;
                      AZip: TCompressionLevel = clNone
                    ): Int64; overload;

     // YAML

    class function  LoadFromYAMLFile<T:class, constructor>(const AFilename: string; AEncoding: TEncoding = Nil; AOptions: TJX4Options = []): T;
    function        SaveToYAMLFile(
      const AFilename: string;
      AOptions: TJX4Options = [];
      AEncoding: TEncoding = Nil;
      AZip: TCompressionLevel = clNone
    ): Int64; overload;

    // Tools

    class function  YAMLStrtoJSONStr(const AYaml: string; AOptions: TJX4Options = [ joNullToEmpty ]): string;
    class function  JSONStrtoYAMLStr(const AJson: string; AOptions: TJX4Options = [ joNullToEmpty ]): string;

  end;

  MyTThread = class(TThread);  //  TThread Protected Access

  TJX4Obj = TJX4Object;
  TJX4    = TJX4Object;

implementation
uses
    StrUtils
  , uJX4Value
  , uJX4YAML
  , uJX4List
  , Threading
  , windows
  , Diagnostics
  , uJX4RTTI
  ;

constructor TJX4Name.Create(const AName: string);
begin
  Name := AName;
end;

constructor TJX4Default.Create(const AValue: string);
begin
  Value := AValue;
end;

constructor TJX4Default.Create(const AValue: Int64);
begin
  Value := AValue;
end;

constructor TJX4Default.Create(const AValue: Boolean);
begin
  Value := AValue;
end;

constructor TJX4Default.Create(const AValue: Extended);
begin
  Value := AValue;
end;

constructor TJX4Default.Create(const ANilValue: Pointer);
begin
  Value := Nil;
end;

constructor TJX4IOBlock.Create(const AJsonName: string; AJVal: TJSONValue; AField: TRttiField; AOptions: TJX4Options);
begin
  Init(AJsonName, AJVal, AField, AOptions);
end;

procedure TJX4IOBlock.Init(const AJsonName: string; AJVal: TJSONValue; AField: TRttiField; AOptions: TJX4Options);
begin
  JVal :=       AJVal;
  JsonName :=   AJsonName;
  Field :=      AField;
  Options :=    AOptions;
end;

procedure TJX4IOBlock.SetVal(AJVal: TJSONValue);
begin
  FVal := AJVal;
  if AJVal is TJSONArray then
  begin
    JObj := nil;
    JArr := AJVal as TJSONArray;
    Exit;
  end;

  if AJVal is TJSONObject then
  begin
    JObj := AJVal as TJSONObject;
    JArr := nil;
    Exit;
  end;

  JObj := nil;
  JArr := nil;
end;

{ TJX4Object }

constructor TJX4Object.Create;
var
  LField:     TRTTIField;
  LFields:    TArray<TRttiField>;
  LNewObj:    TObject;
  LAttr:      TCustomAttribute;
  LIntf:      IJX4Jsonable;
begin
  inherited Create;
  LFields :=  TxRTTI.GetFields(Self);
  for LField in LFields do
  begin
    if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Transient)) then Continue;
    if  (LField.Visibility in [mvPublic]) then
    begin
      if LField.FieldType.TypeKind in [tkRecord] then
      begin
        LAttr := TxRTTI.GetFieldAttribute(LField, TJX4Default);
        if Assigned(LAttr) then LField.SetValue(Self, TJX4Default(LAttr).Value);
      end else
        if (LField.FieldType.TypeKind in [tkClass]) then
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
end;

destructor TJX4Object.Destroy;
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
  LIntf:    IJX4Jsonable;
  LRes:     Boolean;
begin
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
  begin
    if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Transient)) then Continue;
    if  (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
    begin
      LObj := LField.GetValue(Self).AsObject;
      if not Assigned(LObj) then Continue;
      if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then
      begin
        if Supports(LObj, IJX4Jsonable, LIntf) then
          LRes := LIntf.JX4Destroy
        else
          LRes := TxRTTI.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean;
        if LRes then
        begin
          FreeAndNil(LObj);
          LField.SetValue(Self, Nil);
        end;
        Continue;
      end;
      FreeAndNil(LObj);
      LField.SetValue(Self, Nil);
    end;
  end;
  inherited Destroy;
end;

function TJX4Object.Clone<T>(AOptions: TJX4Options): T;
var
  LIntf: IJX4Jsonable;
begin
  Result := T.Create;
  try
    RaiseIfAborted(AOptions);
    if Supports(Result, IJX4Jsonable, LIntf) then
    begin
      LIntf.JX4Create(True, AOptions);
      Self.JX4Clone(Result, AOptions);
    end else begin
      TxRTTI.CallMethodProc('JSONCreate', Result, [True,TValue.From<TJX4Options>(AOptions)]);
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

function TJX4Object.JX4Serialize(AIOBlock: TJX4IOBlock):TValue;
begin
  Result := JSONSerialize(AIOBlock);
end;

function TJX4Object.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LParts:     TArray<string>;
  LPartsIdx:  Integer;
  LField:     TRTTIField;
  LFields:    TArray<TRTTIField>;
  LRes:       string;
  LIOBlock:   TJX4IOBlock;
  LObj:       TOBject;
  LTValue:    TValue;
  LTValueRec: TValue;
  LIntf:      IJX4Jsonable;
  LName:      string;
  LAttr:      TCustomAttribute;
begin
  Result := TValue.Empty;
  RaiseIfAborted(AIOBlock.Options);

  LPartsIdx := 0;
  LIOBlock := TJX4IOBlock.Create;
  try
    LFields := TxRTTI.GetFields(Self);
    SetLength(LParts, Length(LFields));
    for LField in LFields do
    begin
      if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Transient)) then Continue;
      if TxRTTI.FieldAsTObject(Self, LField, LObj, [mvPublic]) then
      begin
        if not Assigned(LObj) then Continue; // Unmanaged
        LIOBlock.Init(LField.Name, Nil, LField, AIOBlock.Options);
        if Supports(LObj, IJX4Jsonable, LIntf) then
          LTValue := LIntf.JX4Serialize(LIOBlock)
        else
          LTValue := TxRTTI.CallMethodFunc('JSONSerialize', LObj, [LIOBlock]);
        if not LTValue.IsEmpty then
        begin
          LPArts[LPartsIdx] := LTValue.AsString;
          Inc(LPartsIdx);
        end;
        Continue;
      end
      else if TxRTTI.FieldAsTValue(Self, LField, LTValue, [mvPublic]) then
      begin
        if LTValue.IsEmpty and Assigned((TxRTTI.GetFieldAttribute(LField, TJX4Required))) then
          raise Exception.Create(SysUtils.Format('TJX4Object.JSONSerialize : "%s" : a value is required', [LField.Name]));
        if not ((joNullToEmpty in AIOBlock.Options) and LTValue.IsEmpty) then
        begin
          LIOBlock.Init(LField.Name, Nil, LField, AIOBlock.Options);
          LTValueRec := LTValue.JSONSerialize(LIOBlock);
          if not LTValueRec.IsEmpty then
          begin
            LParts[LPartsIdx] := LTValueRec.AsString;
            Inc(LPartsIdx);
          end;
        end;
      end;
    end;

    LName := ExtractFieldName(AIOBlock.Field, AIOBlock.JsonName);

    if (LPartsIdx = 0) then
    begin
      if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
        raise Exception.Create(SysUtils.Format('TJX4Object.JSONSerialize : "%s" : a value is required', [LName]));
      if (joNullToEmpty in AIOBlock.Options) then Exit;
      if LName.IsEmpty then Result := 'null' else Result := '"' + LName + '":null';
      Exit;
    end;

    SetLength(LParts, LPartsIdx);
    LRes := TJX4Object.JoinStrings(LParts, ',');
    if LName.IsEmpty then Result := '{' + LRes + '}' else Result := '"' + LName + '":{' + LRes + '}';
  finally
    LIOBlock.Free;
  end;
end;

procedure TJX4Object.JSONClone(ADestObj: TObject; AOptions: TJX4Options);
var
  LSrcField:  TRTTIField;
  LDestField: TRTTIField;
  LNewObj:    TObject;
  LSrc:       TArray<TRTTIField>;
  LTValue:    TValue;
  LIntf:      IJX4Jsonable;
begin
  RaiseIfAborted(AOptions);
  LSrc := TxRTTI.GetFields(Self);
  for LDestField in TxRTTI.GetFields(ADestObj) do
    begin
    if Assigned(TxRTTI.GetFieldAttribute(LDestField, TJX4Transient)) then Continue;
    for LSrcField in LSrc do
    begin
      if LSrcField.Name = LDestField.Name then
      begin
        if TxRtti.FieldAsTValue(Self, LSrcField, LTValue) then
        begin
          LDestField.SetValue(ADestObj, LTValue.JSONClone(AOptions));
          Break
        end
        else if TxRtti.FieldAsTObject(ADestObj, LDestField, LNewObj) then
        begin
          if not Assigned(LNewObj) then // Unmanaged
          begin
            LNewObj := TxRTTI.CreateObject(LDestField.FieldType.AsInstance);
            if Supports(LNewObj, IJX4Jsonable, LIntf) then LIntf.JX4Create(True) else TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
            LDestField.SetValue(ADestObj, LNewObj);
          end;
          if Supports(LSrcField.GetValue(Self).AsObject, IJX4Jsonable, LIntf) then
            LIntf.JX4Clone(LNewObj, AOptions)
          else
            TxRTTI.CallMethodProc('JSONClone',  LSrcField.GetValue(Self).AsObject, [LNewObj,  TValue.From<TJX4Options>(AOptions)]);
        Break;
        end;
      end;
      Continue;
    end;
  end;
end;

procedure TJX4Object.JX4Clone(ADestObj: TObject; AOptions: TJX4Options);
begin
  JSONClone(TJX4Object(ADestObj), AOptions);
end;

procedure TJX4Object.JX4Create(const AManaged: Boolean; AOptions: TJX4Options = []);
begin
 //
end;

procedure TJX4Object.JX4Deserialize(AIOBlock: TJX4IOBlock);
begin
  JSONDeserialize(AIOBlock);
end;

function TJX4Object.JX4Destroy(AOptions: TJX4Options = []): Boolean;
begin
  Result := True;
end;

procedure TJX4Object.JX4Merge(AMergedWith: TObject; AOptions: TJX4Options);
begin
  JSONMerge(TJX4Object(AMergedWith), AOptions);
end;

procedure RawTValueToTValueField(const [ref] ASrcTValue: TValue; const AField: TRttiField; const AInstance: Pointer);
var
    LSrcFieldPtr: Pointer;
    LDestStrPtr: Pointer;
    LSrcStrPtr: Pointer;
    LSrcValHdrPtr: PValueData;
    LSrcStrHdrPtr: PStrRec;
    LSrcStr: string;
    LDestFieldPtr: Pointer;
    LDestValHdrPtr: PValueData;
    DestVal : PValueData;
    SrcVal : PValueData;
    LDestStrHdrPtr : PStrRec;

begin
  {$IF not Defined(MSWINDOWS)}
    AField.SetValue(AInstance, ASrcTValue);
    Exit;
  {$IFEND}

  if not Assigned(AField) or not Assigned(AInstance) then Exit;
  LSrcValHdrPtr := PValueData(@ASrcTValue);
  if (not ASrcTValue.IsString) or (LSrcValHdrPtr = nil ) then
  begin
    AField.SetValue(AInstance, ASrcTValue);
    Exit;
  end;

  // LSrcFieldPtr  := ASrcTValue.GetReferenceToRawData;
  LSrcValHdrPtr := PValueData(@ASrcTValue);
  if LSrcValHdrPtr = nil then
  begin
    AField.SetValue(AInstance, ASrcTValue);
    Exit;
  end;
  // LSrcStrPtr    := Pointer(LSrcFieldPtr);
  // LSrcStr       := PString(LSrcStrPtr)^;
  // LSrcStrHdrPtr := PStrRec(PByte(LSrcStrPtr^) - SizeOf(TStrRec));
  LDestFieldPtr := Pointer(NativeInt(AInstance) + AField.Offset);
  LDestValHdrPtr:= PValueData(LDestFieldPtr);

  if  (LDestValHdrPtr^.FValueData) = nil then
  begin
    // FillChar(LDestFieldPtr, SizeOf(TValue), 0);
   // PFastData(LDestFieldPtr)^.TypeInfo  := _RTTITTypeInfoString;
   // PFastData(LDestFieldPtr)^.ValueData := Nil;
    LDestValHdrPtr := PValueData(LDestFieldPtr);
  end else begin
    LDestStrPtr := Pointer(IValueData(LDestValHdrPtr^.FValueData).GetReferenceToRawData);
    LDestStrHdrPtr := PStrRec(PByte(LDestStrPtr^) - SizeOf(TStrRec));
  end;

  DestVal := LDestValHdrPtr;
  SrcVal := LSrcValHdrPtr;

  DestVal^.FTypeInfo := TypeInfo(string);
  Pointer(DestVal^.FValueData) := Pointer(IValueData(SrcVal^.FValueData));
  // if LDestValHdrPtr^.FValueData<> nil then LDestValHdrPtr^.FValueData._AddRef;
  // LSrcValHdrPtr^.FValueData._Release;
  FillChar(SrcVal^, SizeOf(TValue), 0);
end;

procedure TJX4Object.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LField:       TRTTIField;
  LFields:      TArray<TRTTIField>;
  LJPair:       TJSONPAir;
  LJObj:        TJSONObject;
  LIOBlock:     TJX4IOBlock;
  LName:        string;
  LObj:         TObject;
  LFieldFound:  Boolean;
  LAttr:        TCustomAttribute;
  LTValue:      TValue;
  LIntf:        IJX4Jsonable;
  LVal:         TValue;
begin

  RaiseIfAborted(AIOBlock.Options);

  LIOBlock := TJX4IOBlock.Create;
  try
    if (JoRaiseOnMissingField in AIOBlock.Options) and Assigned(AIOBlock.JVal) and (AIOBlock.JVal is TJSONObject) then
    begin
      for LJPair in (AIOBlock.JVal as TJSONObject) do
      begin
        RaiseIfAborted(AIOBlock.Options);
        LFieldFound := False;
        for LField in TxRTTI.GetFields(Self) do
        begin
          if Assigned(TJX4Transient(TxRTTI.GetFieldAttribute(LField, TJX4Transient))) then Continue;
          LName := ExtractFieldName(LField, AIOBlock.JsonName);
          if LName = LJPair.JsonString.Value then
          begin
            LFieldFound := True;
            Break;
          end;
        end;
       if not LFieldFound then raise Exception.Create(SysUtils.Format('Missing Property "%s" in Class "%s"', [LJPair.JsonString.Value, Self.ClassName]));
      end;
    end;

    LFields := TxRTTI.GetFields(Self);
    for LField in LFields do
    begin
      if not (TXRtti.FieldIsTValue(LField, [mvPublic]) or (TXRtti.FieldIsTObject(LField, [mvPublic]))) then Continue;
      if Assigned(TJX4Transient(TxRTTI.GetFieldAttribute(LField, TJX4Transient))) then Continue;
      LName := ExtractFieldName(LField);
      LFieldFound := False;
      if Assigned(AIOBlock.JObj) then
      for LJPair in  AIOBlock.JObj do
      begin
        if LName = LJPair.JsonString.Value then
        begin
          LFieldFound := True;
          LJPair.Owned := False;
          LJPair.JsonString.Owned := False;
          LJPair.JsonValue.Owned := False;
          if (LJPair.JsonValue is TJSONObject) then
            LJObj := (LJPair.JsonValue as TJSONObject)
          else
            LJObj := TJSONObject.Create(LJPair);
          try
            LIOBlock.Init(LField.Name, LJObj, LField, AIOBlock.Options);
            if TxRtti.FieldAsTValue(Self, LField, LTValue) then
            begin
              LTValue.JSONDeserialize(LIOBlock);
              if LTValue.IsEmpty then
                LField.SetValue(Self, Nil)
              else
                LField.SetValue(Self, LTValue);
                //RawTValueToTValueField(LTValue, LField, Self);
            end else begin
              LObj := LField.GetValue(Self).AsObject;
              try
                if not Assigned(LObj) then
                begin
                  LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
                  if Supports(LObj, IJX4Jsonable, LIntf) then
                    LIntf.JX4Create(True)
                  else
                    TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
                end;
                if Supports(LObj, IJX4Jsonable, LIntf) then
                  LIntf.JX4Deserialize(LIOBlock)
                else
                  TxRTTI.CallMethodProc('JSONDeserialize', LObj, [LIOBlock]);
                except
                  FreeAndNil(LObj);
                  LField.SetValue(Self, Nil);
                  Raise;
                end;
              LField.SetValue(Self, LObj);
            end;
          finally
            if not (LJPair.JsonValue is TJSONObject) then
            begin
              LJObj.Pairs[0].JsonString.Owned := False;
              LJObj.Pairs[0].JsonValue.Owned := False;
              LJObj.RemovePair(LJObj.Pairs[0].JsonString.Value);
              LJObj.Free;
            end;
            LJPair.JsonString.Owned := True;
            LJPair.JsonValue.Owned := True;
            LJPair.Owned := True;
          end;
          Break;
        end;
      end;
      if (not LFieldFound) and Assigned(TJX4Required(TxRTTI.GetFieldAttribute(LField, TJX4Required))) then
        raise Exception.Create(SysUtils.Format('Undefined Property "%s" in Class "%s"', [LName, Self.ClassName]));
    end;
  finally
    LIOBlock.Free;
  end;
end;

class function TJX4Object.ToJSON(AObj: TObject; AOptions: TJX4Options): string;
var
  LIOBlock: TJX4IOBlock;
  LResult: TValue;
  LIntf: IJX4Jsonable;
begin
  LIOBlock := Nil;
  try
    try
      RaiseIfAborted(AOptions);
      LIOBlock := TJX4IOBlock.Create('', nil, nil, AOptions);
      if Supports(AObj, IJX4Jsonable, LIntf) then
        LResult := LIntf.JX4Serialize(LIOBlock)
      else
        LResult := TxRTTI.CallMethodFunc('JSONSerialize', AObj, [LIOBlock]);
      if LResult.IsEmpty then
      begin
        if (AObj is TJX4ListOfValues) or (AObj is TList) then Result := '[]' else Result := '{}';
      end else begin
        Result := LResult.AsString
      end;
    finally
      FreeAndNil(LIOBlock);
    end;
  except
    on TJX4ExceptionAborted do
    begin
      Result := '';
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      Result := '';
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4Object.ToJSON(AOptions: TJX4Options): string;
begin
  try
    if Self = Nil then raise Exception.Create('TJX4Object.ToJSON, JSON Object is undefined');
    Result := ToJSON(Self, AOptions);
  except
    on TJX4ExceptionAborted do
    begin
      Result := '';
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      Result := '';
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

class function TJX4Object.FromJSON<T>(const AJson: string; AOptions: TJX4Options): T;
var
  LIOBlock: TJX4IOBlock;
  LJVal:    TJSONValue;
  Tick:     Cardinal;
  LIntf:    IJX4Jsonable;
begin
  Result := Nil;
  LIOBlock := Nil;
  LJVal := Nil;
  try
    RaiseIfAborted(AOptions);
    if AJson.Trim.IsEmpty then Exit;
    try
      LJVal := TJSONValue.ParseJSONValue(AJson, True, not (joNoException in AOptions));
      if not Assigned(LJVal) then Exit;
      Result := T.Create;
      LIOBlock := TJX4IOBlock.Create('', LJVal, Nil, AOptions);
      if Supports(Result, IJX4Jsonable, LIntf) then
        LIntf.JX4Deserialize(LIOBlock)
      else
        TxRTTI.CallMethodProc('JSONDeserialize', Result, [LIOBlock]);
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
   finally
    LJVal.Free;
    LIOBlock.Free;
  end;
end;

class function TJX4Object.ToJSONStream(AObj: TObject; AOptions: TJX4Options): TStream;
var
  LIOBlock: TJX4IOBlock;
  LIntf: IJX4Jsonable;
begin
  LIOBlock := Nil;
  try
    try
      LIOBlock := TJX4IOBlock.Create('', nil, nil, AOptions);
      if Supports(AObj, IJX4Jsonable, LIntf) then
        LIntf.JX4Deserialize(LIOBlock)
      else
        TxRTTI.CallMethodProc('JSONDeserialize', AObj, [LIOBlock]);
      if Assigned(Result) then Result.Position := 0;
    finally
      LIOBlock.Free;
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

class function TJX4Object.FromYAML<T>(const AYaml: string; AOptions: TJX4Options = []): T;
begin
  try
    Result := TJX4Object.FromJSON<T>(TYAMLUtils.YamlToJson(AYaml), AOptions);
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

class function TJX4Object.IsJSON(AStr: string): Boolean;
begin
  Result := False;
  var Sub := Copy(AStr,1, 1000).Trim;
  if Sub.IsEmpty then Exit;
  Result := (Pos('{', AStr, 1) = 1) or (Pos(AStr, '[') = 1) ;
end;

class function TJX4Object.Version: string;
begin
  Result := SysUtils.Format('%0.2d.%0.2d', [
              (CJX4Version and $FF00) shr 8,
              (CJX4Version and $00FF)
            ]);
end;

class function TJX4Object.VersionValue: integer;
begin
  Result := (((CJX4Version and $FF00) shr 8) * 100) + (CJX4Version and $00FF);
end;

class function TJX4Object.Author: string;
begin
  Result := 'L.Meyer (bnzbnz @ GitHub)';
end;

class function TJX4Object.Contact: string;
begin
  Result := 'JsonX4@lmeyer.fr';
end;

class function TJX4Object.NameDecode(const ToDecode: string): string;
var
  Index: Integer;
  CharCode: Integer;
begin;
  if Pos('_', ToDecode) <> 1 then Exit(ToDecode);
  Result := ''; Index := 2;
  while (Index <= Length(ToDecode)) do
    begin
      if (ToDecode[Index] = '_') and TryStrToInt('$' + Copy(ToDecode, Index + 1, 2), CharCode) then
      begin
        Result := Result + Chr(CharCode);
        Inc(Index, 3);
      end
        else
      begin
        Result := Result + ToDecode[Index];
        Inc(Index, 1);
      end;
    end;
end;

class function TJX4Object.NameEncode(const ToEncode: string): string;
var
  Encoded: Boolean;
begin
  Result := '';
  Encoded := False;
  for var i := 1 to Length(ToEncode) do
    if CharInSet(ToEncode[i], ['0'..'9', 'a'..'z', 'A'..'Z']) then
      Result := Result + ToEncode[i]
    else begin
      Encoded := True;
      Result := Result + '_' + SysUtils.Format('%2x', [Ord(ToEncode[i])]);
    end;
  if Encoded then Result := '_'  + Result;
end;

class function TJX4Object.New<T>: T;
begin
  Result := T.Create;
end;

function TJX4Object.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;
function TJX4Object._AddRef: Integer; begin Result := -1; end;
function TJX4Object._Release: Integer; begin Result := -1; end;

class procedure TJX4Object.RaiseIfAborted(AOptions: TJX4Options);
begin
  if not( joRaiseOnAbort in AOptions ) then Exit;
  if Assigned(TThread.CurrentThread) and (MyTThread(TThread.CurrentThread).Terminated) then
    raise TJX4ExceptionAborted.Create('JSON: Operation Aborted');
  if (TTask.CurrentTask <> nil) and (TTaskStatus.Canceled = TTask.CurrentTask.Status) then
    raise TJX4ExceptionAborted.Create('JSON: Operation Aborted');
end;

class procedure TJX4Object.VarEscapeJSONStr(var AStr: string; const SlashEncode: Boolean);
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  LP: PChar;
  LEndP: PChar;
  LSb: TStringBuilder;
  LMatch: Pointer;
begin
  LMatch := nil ;
  LP := PChar(Pointer(AStr));
  LEndP := LP + Length(AStr);
  while LP < LendP do
  begin
    if SlashEncode then
    begin
      case LP^ of
        #0..#31, '\', '/', '"' : begin LMatch := LP; Break; end;
      end;
    end else begin
      case LP^ of
        #0..#31, '\', '"' : begin LMatch := LP; Break; end;
      end;
    end;
    Inc(LP);
  end;

  if not Assigned(LMatch) then Exit;

  LSb := TStringBuilder.Create(Copy(AStr, 1, LMatch - PChar(Pointer(AStr))));
  LP := LMatch;
  while LP < LendP do
  begin
    case LP^ of
      #0..#7, #11, #14..#31:
        begin
          LSb.Append('\u00');
          LSb.Append(HexChars[Word(LP^) shr 4]);
          LSb.Append(HexChars[Word(LP^) and $F]);
        end;
      #8: LSb.Append('\b');
      #9: LSb.Append('\t');
      #10: LSb.Append('\n');
      #12: LSb.Append('\f');
      #13: LSb.Append('\r');
      '\': LSb.Append('\\');
      '"': LSb.Append('\"');
      '/': if SlashEncode then LSb.Append('\/') else LSb.Append('/')
    else
      LSb.Append(LP^);
    end;
    Inc(LP);
  end;
  AStr := LSb.ToString;
  LSb.Free;
end;

procedure TJX4Object.JSONClear(AOptions: TJX4Options);
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
  LValue:   TValue;
  LIntf:    IJX4Jsonable;
begin
  LFields := TxRTTI.GetFields(Self);
  RaiseIfAborted(AOptions);
  for LField in LFields do
  begin
    if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Transient)) then Continue;
    if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then Continue;
    if TxRTTI.FieldAsTValue(Self, LField, LValue, [mvPublic]) then
      LField.SetValue(Self, Nil)
    else
    if TxRTTI.FieldAsTObject(Self, LField, LObj, [mvPublic]) then
    begin
      if not Assigned(LObj) then Continue;

    if Supports(LObj, IJX4Jsonable, LIntf) then
      LIntf.JX4Clear(AOptions)
    else
      TxRTTI.CallMethodProc('JSONClear', LObj, [TValue.From<TJX4Options>(AOptions)]);
    end;
  end;
end;

procedure TJX4Object.JX4Clear(AOptions: TJX4Options);
begin
  JSONClear(AOptions);
end;

procedure TJX4Object.Clear(AOptions: TJX4Options);
begin
  JSONClear(AOptions);
end;

class function TJX4Object.EscapeJSONStr(const AStr: string; const SlashEncode: Boolean): string;
begin
  Result := AStr;
  VarEscapeJSONStr(Result, SlashEncode);
end;

class function TJX4Object.ExtractFieldName(const AField: TRttiField; ADefault: string): string;
var
  LAttr: TCustomAttribute;
begin
  Result := ADefault;
  if not Assigned(AField) then Exit;
  LAttr  := TJX4Name(TxRTTI.GetFieldAttribute(AField, TJX4Name));
  if Assigned(LAttr) then
    Result := NameDecode(TJX4Name(LAttr).Name)
  else
    Result := NameDecode(AField.Name);
end;

class function TJX4Object.JoinStrings(const AArray: TArray<string>; const ADelimiter: string): string;
var
  TotalChars: Integer;
  DelimLen: Integer;
  I: Integer;
  SrcLen: Integer;
  DestPtr: PChar;
begin
  if Length(AArray) = 0 then Exit('');

  DelimLen := Length(ADelimiter);
  TotalChars := 0;

  // PASS 1: Calculate raw character length needed (No assignment)
  for I := 0 to length(AArray) - 1 do
    Inc(TotalChars, Length(AArray[I]));

  Inc(TotalChars, DelimLen * (Length(AArray) - 1));

  // Single atomic heap allocation allocation for the output string buffer
  SetLength(Result, TotalChars);
  DestPtr := PChar(Result);

  // PASS 2: Blit memory blocks directly using PChar arithmetic offsets
  for I := 0 to length(AArray) - 1 do
  begin
    SrcLen := Length(AArray[I]);
    if SrcLen > 0 then
    begin
      // Direct hardware block transfer: copies character memory without touching ref counts
      System.Move(PChar(AArray[I])^, DestPtr^, SrcLen * SizeOf(Char));
      Inc(DestPtr, SrcLen);
    end;

    // Inject the delimiter text block
    if (I < Length(AArray) - 1) and (DelimLen > 0) then
    begin
      System.Move(PChar(ADelimiter)^, DestPtr^, DelimLen * SizeOf(Char));
      Inc(DestPtr, DelimLen);
    end;
  end;
end;

class function TJX4Object.FormatJSON(const AJson: string; ABeautify: Boolean; AIndentation: Integer): string;
var
  TmpVal: TJSONAncestor;
begin
  if ABeautify then
  begin
    TmpVal := (TJSONValue.ParseJSONValue(AJson) as TJSONAncestor);
    Result := TmpVal.Format(AIndentation);
    FreeAndNil(TmpVal);
  end else
    Result := TYamlUtils.JsonMinify(AJson);
end;

function TJX4Object.Format(ABeautify: Boolean; AIndentation: Integer; AOptions: TJX4Options): string;
var
  TmpVal: TJSONAncestor;
  LJson: string;
begin
  LJson := ToJSON(Self, AOptions);
  Result := FormatJSON( ToJSON(Self, AOptions), ABeautify, 2 );
end;

class function TJX4Object.Validate(const AJson: string): Boolean;
var
  LJVal: TJSONValue;
begin
  Result := False;
  LJVal := Nil;
  try
    LJVal := TJSONValue.ParseJSONValue(AJson, True, True);
    Result := True;
  except
    on Ex:Exception do
    begin
      Result := False;
    end;
  end;
  LJVal.Free;
end;

function TJX4Object.Merge(AMergedWith: TObject; AOptions: TJX4Options): Boolean;
var
  LIntf: IJX4JSonable;
begin
  Result := False;
  try
    RaiseIfAborted(AOptions);
    if Supports(Self, IJX4Jsonable, LIntf) then
      LIntf.JX4Merge(AMergedWith, AOptions)
    else
      TxRTTI.CallMethodProc('JSONMerge', Self, [ AMergedWith, TValue.From<TJX4Options>(AOptions) ]);
    Result := True;
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

procedure TJX4Object.JSONMerge(AMergedWith: TObject; AOptions: TJX4Options);
var
  LSrcField:  TRTTIField;
  LMrgField:  TRTTIField;
  LSrcValue:  TValue;
  LMgrValue:  TValue;
  LSrcObj:    TObject;
  LMrgObj:    TObject;
  LIntf:      IJX4Jsonable;
begin
  if not Assigned(AMergedWith) then Exit;
  RaiseIfAborted(AOptions);
  for LSrcField in TxRTTI.GetFields(Self) do
  begin
    for LMrgField in TxRTTI.GetFields(AMergedWith) do
    begin
      if Assigned(TxRTTI.GetFieldAttribute(LMrgField, TJX4Transient)) then Continue;
      if (LSrcField.Name = LMrgField.Name) then
      begin
        if TxRtti.FieldAsTValue(Self, LSrcField, LSrcValue) and TxRtti.FieldAsTValue(AMergedWith, LMrgField, LMgrValue) then
        begin
          LSrcValue.JSONMerge(LMgrValue, AOptions);
          if LSrcValue.IsEmpty then LSrcField.SetValue(self, Nil) else LSrcField.SetValue(self, LSrcValue);
          Break;
        end;
        if TxRtti.FieldAsTObject(Self, LSrcField, LSrcObj) and TxRtti.FieldAsTObject(AMergedWith, LMrgField, LMrgObj) then
        begin
          if Assigned(LSrcObj) and Assigned(LMrgObj) then
          begin
            if Supports(LSrcObj, IJX4Jsonable, LIntf) then
              LIntf.JX4Merge(LMrgObj, AOptions)
            else
              TxRTTI.CallMethodProc('JSONMerge', LSrcObj, [ LMrgObj, TValue.From<TJX4Options>(AOptions)]);
          end;
          Break;
        end;
      end;
    end;
    Continue;
  end;
end;

class function TJX4Object.GetStreamEncoding(AStream: TStream): TEncoding;
var
  LBytes: TBytes;
  LIdx: Integer;
  LByteCount: Integer;
  LIsAscii, LIsUTF8: Boolean;
begin
  Result := TEncoding.ANSI;;
  LIsAscii := True; LIsUTF8 := True;
  LIdx := 0;
  LByteCount := 0;

  if AStream.Size < 4 then Exit;

  AStream.Position := 0;
  SetLength(LBytes, 4);
  AStream.Read(LBytes, 4);
  AStream.Position := 0;

  // BOM
  if (LBytes[0] = $EF) and (LBytes[1] = $BB) and (LBytes[2] = $BF) then
    begin Result := TEncoding.UTF8; AStream.Position := 3; exit end
  else if (LBytes[0] = $FE) and (LBytes[1] = $FF) then
    begin Result := TEncoding.BigEndianUnicode; AStream.Position := 2; exit; end
  else if (LBytes[0] = $FF) and (LBytes[1] = $FE) then
    begin Result := TEncoding.Unicode; AStream.Position := 2; exit; end
  else if (LBytes[0] = $00) and (LBytes[1] = $00) and (LBytes[2] = $FE) and (LBytes[3] = $FF) then
      raise Exception.Create('UTF-32 BE Encoding not implemented')  // UTF-32 BE
  else if (LBytes[0] = $FF) and (LBytes[1] = $FE) and (LBytes[2] = $00) and (LBytes[3] = $00) then
      raise Exception.Create('UTF-32 LE Encoding not implemented'); // UTF-32 LE

  // No BOM
  AStream.Position := 0;
  SetLength(LBytes, AStream.Size);
  AStream.Read(LBytes, AStream.Size);
  AStream.Position := 0;

  while LIdx < Length(LBytes) do
  begin
    LIsAscii := LIsAscii and (LBytes[LIdx] and $80 = 0);
    if LByteCount = 0 then
    begin
      if (LBytes[LIdx] and $80) = 0 then
        LByteCount := 0
      else if (LBytes[LIdx] and $E0) = $C0 then
        LByteCount := 1
      else if (LBytes[LIdx] and $F0) = $E0 then
        LByteCount := 2
      else if (LBytes[LIdx] and $F8) = $F0 then
        LByteCount := 3
      else
      begin
        LIsUTF8 := False;
        Break;
      end;
    end
    else
    begin
      if (LBytes[LIdx] and $C0) <> $80 then
      begin
        LIsUTF8 := False;
        Break;
      end;
      Dec(LByteCount);
    end;
    Inc(LIdx);
  end;
  if LByteCount > 0 then LIsUTF8 := False;

  if LIsAscii then
    Result := TEncoding.ASCII
  else if LIsUTF8 then
    Result := TEncoding.UTF8;
end;

class function TJX4Object.LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64;
var
  &In : TStream;
  &Out: TStream;
  &Tmp: Tstream;
  Res: TStringStream;
  LBytes: TBytes;
  DecompressionStream: TZDecompressionStream;
begin
  AStr := '';
  &In := nil;
  &Out:= Nil;
  Res := Nil;
  DecompressionStream := Nil;
  Result := 0;
  try
    if not FileExists(AFilename) then Exit;

    &In := TFileStream.Create(AFilename, fmOpenRead + fmShareDenyNone);
    if not Assigned(&In) then Exit;

    if &In.Size < 2 then Exit;
    &In.Position := 0;
    SetLength(LBytes, 2);
    &In.Read(LBytes, 2);
    &In.Position := 0;

    if    ((LBytes[0] = $78) and (LBytes[1] = $01))  // No Compression/low
       or ((LBytes[0] = $78) and (LBytes[1] = $5E))  // Fast Compression
       or ((LBytes[0] = $78) and (LBytes[1] = $9C))  // Default Compression
       or ((LBytes[0] = $78) and (LBytes[1] = $DA))  // Best Compression
    then begin
      &Out:= TMemoryStream.Create;
      DecompressionStream  := TZDecompressionStream.Create(&In);
      DecompressionStream.Position := 0;
      for var Blk :=  1 to (&In.Size div 65536) do &Out.CopyFrom(DecompressionStream, 65536);
      &Out.CopyFrom(DecompressionStream, DecompressionStream.Size - DecompressionStream.Position);
      &Out.Position := 0;
      &Tmp := &Out;
    end else begin
      &Tmp := &In;
    end;

    if Assigned(AEncoding) then
      Res := TStringStream.Create('', AEncoding)
    else
      Res := TStringStream.Create('', GetStreamEncoding(&Tmp));

    Result := Res.CopyFrom(&Tmp);
    AStr := Res.DataString;
  finally
    DecompressionStream.Free;
    &Out.Free;
    &In.Free;
    Res.Free;
  end;
end;

class function TJX4Object.LoadFromJSONFile<T>(const AFilename: string; AOptions: TJX4Options; AEncoding: TEncoding): T;
var
  LJstr: string;
begin
  Result := Nil;
  LoadFromFile(AFilename, LJStr, AEncoding);
  if LJStr.IsEmpty then Result := Nil else Result := TJX4Object.FromJSON<T>(LJStr, AOptions);
end;

class function TJX4Object.SaveToFile(const AFilename: string; const AStr: string; AEncoding: TEncoding; AZipIt: TCompressionLevel; UseBOM: Boolean): Int64;
var
  Zip:  TZCompressionStream;
  &Out: TFileStream;
  &In:  TStringStream;
  Blk:  Integer;
begin
  Result:= 0;
  &Out  := Nil;
  &In   := Nil;
  Zip   := Nil;
  try
    CreateDir(ExtractFilePath(AFilename));
    &Out := TFileStream.Create(AFilename, fmCreate);

    if not Assigned(AEncoding) then AEncoding := TEncoding.UTF8;
    if (AEncoding = TEncoding.UTF8) and UseBOM then &Out.writeData($00BFBBEF, 3);
    if  AEncoding = TEncoding.BigEndianUnicode then &Out.writeData($FFFE, 2);
    if  AEncoding = TEncoding.Unicode then &Out.writeData($FEFF, 2);

    &In := TStringStream.Create(AStr, AEncoding);
    if AZipIt <> clNone then
    begin
      Zip := TZCompressionStream.Create(AZipIt, &Out);
      for Blk :=  1 to (&In.Size div 65536) do
      begin
       &Zip.CopyFrom(&In, 65536);
      end;
      &Zip.CopyFrom(&In, &In.size - &Zip.position);
      Result := &Out.Size; &Out.Position := 0;
    end else begin
      for Blk :=  1 to (&In.Size div 65536) do
      begin
        &Out.CopyFrom(&In, 65536);
      end;
      &Out.CopyFrom(&In, &In.Size - &In.Position);
      Result := &Out.Size; &Out.Position := 0;
    end;
  finally
    Zip.Free;
    &Out.Free;
    &In.Free;
  end;
end;

class function TJX4Object.ToYAML(const AStr: string; AOptions: TJX4Options): string;
begin
  try
    RaiseIfAborted(AOptions);
    Result := TYAMLUtils.JsonToYaml(AStr);
  except
  on TJX4ExceptionAborted do
    begin
       Result := '';
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      Result := '';
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

function TJX4Object.ToYAML(AOptions: TJX4Options): string;
begin
  Result := ToYAML(TJX4Object.ToJSON(Self, AOptions));
end;

function TJX4Object.SaveToJSONFile(
  const AFilename: string;
  ABeautify: Boolean = False;
  AOptions: TJX4Options = [];
  AEncoding: TEncoding = Nil;
  AZip: TCompressionLevel = clNone
): Int64;
begin
  Result := 0;
  RaiseIfAborted(AOptions);
  if ABeautify then
    Result := TJX4Object.SaveToFile(AFilename,  TJX4Object.FormatJSON( TJX4Object.ToJSON(Self, AOptions) ) , AEncoding, AZip, False)
  else
    Result := TJX4Object.SaveToFile(AFilename,  TJX4Object.ToJSON(Self, AOptions), AEncoding, AZip, False);
end;

function TJX4Object.SaveToYAMLFile(
  const AFilename: string;
  AOptions: TJX4Options = [];
  AEncoding: TEncoding = Nil;
  AZip: TCompressionLevel = clNone
): Int64;
begin
  try
    RaiseIfAborted(AOptions);
    Result := TJX4Object.SaveToFile(AFilename, Self.ToYAML, AEncoding, AZip, False);
  except
    on TJX4ExceptionAborted do
    begin
      Result := -1;
      if joRaiseOnAbort in AOptions then raise;
      Exit
    end;
    on Ex: Exception do
    begin
      Result := -1;
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

class function TJX4Object.LoadFromYAMLFile<T>(const AFilename: string; AEncoding: TEncoding;AOptions: TJX4Options): T;
var
  LJstr: string;
begin
  Result := Nil;
  try
    RaiseIfAborted(AOptions);
    LoadFromFile(AFilename, LJStr, AEncoding);
    Result := TJX4Object.FromJSON<T>(TYAMLUtils.YAMLToJSON(LJStr,0));
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

class function TJX4Object.JSONStrtoYAMLStr(const AJson: string; AOptions: TJX4Options): string;
begin
  try
    RaiseIfAborted(AOptions);
    Result := TYAMLUtils.JsonToYaml(AJson);
  except
    on TJX4ExceptionAborted do
    begin
      Result := '';
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      Result := '';
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

class function TJX4Object.YAMLStrtoJSONStr(const AYaml: string; AOptions: TJX4Options): string;
begin
  try
    RaiseIfAborted(AOptions);
    Result := TYAMLUtils.YamlToJson(AYaml);
 except
    on TJX4ExceptionAborted do
    begin
      Result := '';
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      Result := '';
      if not (joNoException in AOptions) then raise;
    end;
  end;
end;

initialization
finalization
end.

