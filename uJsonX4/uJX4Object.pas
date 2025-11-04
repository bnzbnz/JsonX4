(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2025 Laurent Meyer JsonX4@lmeyer.fr

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
unit uJX4Object;
{$HINTS OFF}

interface
uses
  Classes
  , System.Generics.Collections
  , RTTI
  , JSON
  , SysUtils
  , uJX4Rtti
  , zLib
  , windows
  ;

const
  CJX4Version = $0104; // 01.04
  CBoolToStr: array[Boolean] of string = ('false','true');

type

  sFormatType= (sftYAML, sftJSON);

  TJX4Option  = (
        joNullToEmpty
      , joRaiseOnException
      , joRaiseOnAbort
      , joRaiseOnMissingField
      , joSlashEncode
      , joStats
      //Merge
      , jmoDelete
      , jmoAdd
      , jmoUpdate
      , jmoStats
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
    // In
    JObj:       TJSONObject;
    JsonName:   string;
    Field:      TRttiField;
    Options:    TJX4Options;
    // Out
    constructor Create(AJsonName: string = ''; AJObj: TJSONObject = Nil; AField: TRttiField = Nil; AOptions: TJX4Options = []);
    procedure   Init(AJsonName: string; AJObj: TJSONObject; AField: TRttiField; AOptions: TJX4Options);
  end;

  TJX4ExceptionAborted = class(Exception);

  TJX4Object = class(TObject)
  protected
    class function  GetStreamEncoding(AStream: TStream): TEncoding;
  public
    constructor     Create;
    destructor      Destroy; override;
    class procedure RaiseIfCanceled(AOptions: TJX4Options);

    function        JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure       JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure       JSONClone(ADestObj: TObject; AOptions: TJX4Options);
    procedure       JSONMerge(AMergedWith: TObject; AOptions: TJX4Options);
    procedure       JSONClear(AOptions: TJX4Options);

    class function  New<T:class, constructor>: T;
    class function  ToJSON(AObj: TObject; AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    function        ToJSON(AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    class function  FromJSON<T:class, constructor>(const AJson: string; AOptions: TJX4Options = []): T; overload;
    class function  ToJSONStream(AObj: TObject; AOptions: TJX4Options = []): TStream; overload;
    class function  ToYAML(AObj: TJX4Object; AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    class function  ToYAML(AStr: string; AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    function        ToYAML(AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    class function  FromYAML<T:class, constructor>(const AYaml: string; AOptions: TJX4Options = []): T;

    function        Clone<T:class, constructor>(AOptions: TJX4Options= []): T; overload;
    procedure       Merge(AMergedWith: TObject; AOptions: TJX4Options = []);
    function        Format(AIndentation: Integer = 2): string;
    procedure       Clear(AOptions: TJX4Options);

    // Utils
    class function  Version: string;
    class function  VersionValue: integer;

    class function  NameDecode(const ToDecode: string): string; static;
    class function  NameEncode(const ToEncode: string): string; static;
    class procedure VarEscapeJSONStr(var AStr: string; const SlashEncode: Boolean); overload; static;
    class function  EscapeJSONStr(const AStr: string; const SlashEncode: Boolean): string; overload; static;
    class function  JsonListToJsonString(const AList: TList<string>): string; static;
    class function  FormatJSON(const AJson: string; ABeautify: Boolean = True; AIndentation: Integer = 2): string; static;

    class function  ValidateJSON(const AJson: string): string; static;
    class function  IsJSON(AStr: string): Boolean; static;

    // Common
    class function  LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding = Nil): Int64; overload;
    class function  SaveToFile(const AFilename: string; const AStr: string; AEncoding: TEncoding; AZipIt: TCompressionLevel = clNone; UseBOM: Boolean = False): Int64; overload;

    // JSON
    class function  LoadFromJSONFile<T:class, constructor>(const AFilename: string; AEncoding: TEncoding = Nil): T; overload;
    function        SaveToJSONFile(  const AFilename: string;
                      ABeautify: Boolean = False;
                      AOptions: TJX4Options = [ joNullToEmpty ];
                      AEncoding: TEncoding = Nil;
                      AZip: TCompressionLevel = clNone
                    ): Int64; overload;

     // YAML

    class function  LoadFromYAMLFile<T:class, constructor>(const AFilename: string; AEncoding: TEncoding = Nil; AOptions: TJX4Options = [ joNullToEmpty ]): T;
    function        SaveToYAMLFile(
      const AFilename: string;
      AOptions: TJX4Options = [ joNullToEmpty ];
      AEncoding: TEncoding = Nil;
      AZip: TCompressionLevel = clNone
    ): Int64; overload;

    // Tools

    class function  YAMLtoJSON(const AYaml: string; AOptions: TJX4Options = [ joNullToEmpty ]): string;
    class function  JSONtoYAML(const AJson: string; AOptions: TJX4Options = [ joNullToEmpty ]): string;

  end;

  MyTThread = class(TThread);  //  TThread Protected Access

  TJX4Obj = TJX4Object;
  TJX4    = TJX4Object;

implementation
uses
    TypInfo
  , StrUtils
  , uJX4Value
  , uJX4YAML
  , Threading
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

constructor TJX4IOBlock.Create(AJsonName: string; AJObj: TJSONObject; AField: TRttiField; AOptions: TJX4Options);
begin
  Init(AJsonName, AJObj, AField, AOptions);
end;

procedure TJX4IOBlock.Init(AJsonName: string; AJObj: TJSONObject; AField: TRttiField; AOptions: TJX4Options);
begin
  JObj :=       AJObj;
  JsonName :=   AJsonName;
  Field :=      AField;
  Options :=    AOptions;
end;

{ TJX4Object }

constructor TJX4Object.Create;
var
  LField:     TRTTIField;
  LNewObj:    TObject;
  LAttr:      TCustomAttribute;
begin
  inherited Create;
  for LField in TxRTTI.GetFields(Self) do
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
        if TxRTTI.CallMethodFunc('JSONDestroy', LObj, []).AsBoolean then
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
begin
  Result := Nil;
  try
    RaiseIfCanceled(AOptions);
    if not Assigned(Self) then exit;
    Result := T.Create;
    TxRTTI.CallMethodProc('JSONCreate', Result, [True]);
    TxRTTI.CallMethodProc('JSONClone', Self, [Result, TValue.From<TJX4Options>(AOptions)]);
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

function TJX4Object.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LField:     TRTTIField;
  LFields:    TArray<TRTTIField>;
  LParts:     TList<string>;
  LRes:       string;
  LIOBlock:   TJX4IOBlock;
  LObj:       TOBject;
  LTValue:    TValue;
  LTValueRec: TValue;
begin
  Result := TValue.Empty;
  RaiseIfCanceled(AIOBlock.Options);

  LIOBlock := TJX4IOBlock.Create;
  LParts := TList<string>.Create;
  try

    LFields := TxRTTI.GetFields(Self);
    LParts.Capacity := Length(LFields);
    for LField in LFields do
    begin
      RaiseIfCanceled(AIOBlock.Options);
      if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Transient)) then Continue;
      if TxRTTI.FieldAsTObject(Self, LField, LObj, [mvPublic]) then
      begin
        if not Assigned(LObj) then Continue; // Unmanaged
        LIOBlock.Init(LField.Name, Nil, LField, AIOBlock.Options);
        LTValue := TxRTTI.CallMethodFunc('JSONSerialize', LObj, [LIOBlock]);
        if not LTValue.IsEmpty then LParts.Add(LTValue.AsString);
        Continue;
      end
      else if TxRTTI.FieldAsTValue(Self, LField, LTValue, [mvPublic]) then
      begin
        if not ((joNullToEmpty in AIOBlock.Options) and LTValue.IsEmpty) then
        begin
          LIOBlock.Init(LField.Name, Nil, LField, AIOBlock.Options);
          LTValueRec := LTValue.JSONSerialize(LIOBlock);
          if not LTValueRec.IsEmpty then LParts.Add(LTValueRec.AsString);
        end;
      end;
    end;

    LRes := JsonListToJsonString(LParts);
    if not AIOBlock.JsonName.IsEmpty then
    begin
      if LRes.IsEmpty then
      begin
        if Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
          raise Exception.Create(SysUtils.Format('"%s" (TJX3Object) : a value is required', [AIOBlock.JsonName]));

        if joNullToEmpty in AIOBlock.Options then Exit;
        Result := '"' + AIOBlock.JsonName + '":null';
      end else begin
        Result := '"' + AIOBlock.JsonName + '":{' + LRes + '}';
      end;
    end
    else begin
      Result := '{' + LRes + '}';
    end;

  finally
    LParts.Free;
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
begin
  LSrc := TxRTTI.GetFields(Self);
  for LDestField in TxRTTI.GetFields(ADestObj) do
    begin
    if Assigned(TxRTTI.GetFieldAttribute(LDestField, TJX4Transient)) then Continue;
    for LSrcField in LSrc do
    begin
      RaiseIfCanceled(AOptions);
      if LSrcField.Name = LDestField.Name then
      begin
        if MyTThread(TThread.Current).Terminated then Exit;
        if TxRtti.FieldAsTValue(Self, LSrcField, LTValue, [mvPublic]) then
        begin
          LDestField.SetValue(ADestObj, LTValue.JSONClone(AOptions));
          Break
        end
        else if TxRtti.FieldAsTObject(ADestObj, LDestField, LNewObj, [mvPublic]) then
        begin
          if not Assigned(LNewObj) then // Unmanaged
          begin
            LNewObj := TxRTTI.CreateObject(LDestField.FieldType.AsInstance);
            TxRTTI.CallMethodProc('JSONCreate', LNewObj, [True]);
            LDestField.SetValue(ADestObj, LNewObj);
          end;
          TxRTTI.CallMethodProc('JSONClone',  LSrcField.GetValue(Self).AsObject, [LNewObj,  TValue.From<TJX4Options>(AOptions)]);
          Break;
        end;
      end;
      Continue;
    end;
  end;
end;

procedure TJX4Object.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LField:       TRTTIField;
  LJPair:       TJSONPAir;
  LJObj:        TJSONObject;
  LIOBlock:     TJX4IOBlock;
  LName:        string;
  LObj:         TObject;
  LFieldFound:  Boolean;
  LAttr:        TCustomAttribute;
  LTValue:      TValue;
begin
  RaiseIfCanceled(AIOBlock.Options);
  LIOBlock := TJX4IOBlock.Create;
  try
    if (JoRaiseOnMissingField in AIOBlock.Options) then
    begin
      for LJPair in  AIOBlock.JObj do
      begin
        RaiseIfCanceled(AIOBlock.Options);
        LFieldFound := False;
        for LField in TxRTTI.GetFields(Self) do
        begin
          if Assigned(TJX4Transient(TxRTTI.GetFieldAttribute(LField, TJX4Transient))) then Continue;
          LName := NameDecode(LField.Name);
          LAttr := TJX4Name(TxRTTI.GetFieldAttribute(LField, TJX4Name));
          if Assigned(LAttr) then LName := TJX4Name(LAttr).Name;
          if LName = LJPair.JsonString.Value then
          begin
            LFieldFound := True;
            Break;
          end;
         end;
         if not LFieldFound then raise Exception.Create(SysUtils.Format('Missing Property "%s" in Class "%s"', [LJPair.JsonString.Value, Self.ClassName]));
      end;
    end;

    for LField in TxRTTI.GetFields(Self) do
    begin
      RaiseIfCanceled(AIOBlock.Options);
      if not (TXRtti.FieldIsTValue(LField, [mvPublic]) or (TXRtti.FieldIsTObject(LField, [mvPublic]))) then Continue;
      if Assigned(TJX4Transient(TxRTTI.GetFieldAttribute(LField, TJX4Transient))) then Continue;

      LName := NameDecode(LField.Name);
      LAttr := TJX4Name(TxRTTI.GetFieldAttribute(LField, TJX4Name));
      if Assigned(LAttr) then LName := TJX4Name(LAttr).Name;

      LFieldFound := False;
      for LJPair in  AIOBlock.JObj do
      begin
        RaiseIfCanceled(AIOBlock.Options);;
        if LName = LJPair.JsonString.Value then
        begin
          LFieldFound := True;
          if LJPair.JsonValue is TJSONNull then Break;
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
              begin
                LAttr := TJX4Default(TxRTTI.GetFieldAttribute(LField, TJX4Default));
                if Assigned(LAttr) then LTValue := TJX4Default(LAttr).Value else LTValue := Nil;
              end;
              LField.SetValue(Self, LTValue);
            end else begin

              LObj := LField.GetValue(Self).AsObject;
              if not Assigned(LObj) then
              begin
                LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
                TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
              end;
              try
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
begin
  LIOBlock := Nil;
    RaiseIfCanceled(AOptions);
  try
  try
    LIOBlock := TJX4IOBlock.Create('', nil, nil, AOptions);
    LResult := TxRTTI.CallMethodFunc('JSONSerialize', AObj, [LIOBlock]);
    if not LResult.IsEmpty then Result := LResult.AsString;
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

function TJX4Object.ToJSON(AOptions: TJX4Options): string;
begin
  Result := ToJSON(Self, AOptions);
end;

class function TJX4Object.FromJSON<T>(const AJson: string; AOptions: TJX4Options): T;
var
  LIOBlock: TJX4IOBlock;
  LJObj:    TJSONObject;
begin
  Result := Nil;
  LIOBlock := Nil;
  LJObj := Nil;
  RaiseIfCanceled(AOptions);
  try
    if AJson.Trim.IsEmpty then Exit;
    try
      LJObj := TJSONObject.ParseJSONValue(AJson, True, joRaiseOnException in AOptions) as TJSONObject;
      if not Assigned(LJObj) then Exit;
      Result := T.Create;
      LIOBlock := TJX4IOBlock.Create('', LJObj, Nil, AOptions);
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
        if joRaiseOnException in AOptions then raise;
      end;
    end;
   finally
    LJObj.Free;
    LIOBlock.Free;
  end;
end;

class function TJX4Object.ToJSONStream(AObj: TObject; AOptions: TJX4Options): TStream;
var
  LIOBlock: TJX4IOBlock;
begin
  LIOBlock := Nil;
  try
    try
      LIOBlock := TJX4IOBlock.Create('', nil, nil, AOptions);
      Result := TStringStream.Create( TxRTTI.CallMethodFunc('JSONSerialize', AObj, [LIOBlock]).AsString );
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
      if joRaiseOnException in AOptions then raise;
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
      if joRaiseOnException in AOptions then raise;
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

class procedure TJX4Object.RaiseIfCanceled(AOptions: TJX4Options);
begin
  if not( joRaiseOnAbort in AOptions ) then Exit;
  try

    if Assigned(TThread.CurrentThread) and (MyTThread(TThread.CurrentThread).Terminated) then
      raise TJX4ExceptionAborted.Create('Operation Aborted');

    if (TTask.CurrentTask <> nil) and (TTaskStatus.Canceled = TTask.CurrentTask.Status) then
      raise TJX4ExceptionAborted.Create('Operation Aborted');

  except
    raise TJX4ExceptionAborted.Create('Operation Aborted');
  end;
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
begin
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
  begin
    RaiseIfCanceled(AOptions);
    if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Transient)) then Continue;
    if Assigned(TxRTTI.GetFieldAttribute(LField, TJX4Unmanaged)) then Continue;
    if TxRTTI.FieldAsTValue(Self, LField, LValue, [mvPublic]) then
      LField.SetValue(Self, Nil)
    else
    if TxRTTI.FieldAsTObject(Self, LField, LObj, [mvPublic]) then
    begin
      if not Assigned(LObj) then Continue;
       TxRTTI.CallMethodProc('JSONClear', LObj, [TValue.From<TJX4Options>(AOptions)]);
      Continue;
    end;
  end;
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

class function TJX4Object.JsonListToJsonString(const AList: TList<string>): string;
var
  LSb:  TStringBuilder;
  LIdx: integer;
begin
  if AList.Count = 0 then Exit('');
  if AList.Count = 1 then Exit(AList[0]);
  LSb := TStringBuilder.Create;
  try
    for LIdx:= 0 to AList.Count -1 do
    begin
      LSb.Append(AList[LIdx]);
      if LIdx <> AList.Count -1 then LSb.Append(',') ;
    end;
    Result := LSb.ToString;;
  finally
    LSb.Free;
  end;
end;

class function TJX4Object.FormatJSON(const AJson: string; ABeautify: Boolean; AIndentation: Integer): string;
var
  TmpJson: TJsonObject;
begin
  if ABeautify then
  begin
    TmpJson := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
    Result := TJSONAncestor(TmpJson).Format(AIndentation);
    FreeAndNil(TmpJson);
  end else
   Result := TYamlUtils.JsonMinify(AJson);
end;

function TJX4Object.Format(AIndentation: Integer): string;
begin
  Result := TJX4Object.FormatJSON(Self.ToJSON, True, AIndentation);
end;

class function TJX4Object.ValidateJSON(const AJson: string): string;
var
  LJObj: TJSONObject;
begin
  LJObj := Nil;
  try
    LJObj := TJSONObject.ParseJSONValue(AJson, True, True) as TJSONObject;
  except
    on Ex:Exception do
    begin
      Result := Ex.Message;
    end;
  end;
  LJObj.Free;
end;

procedure TJX4Object.Merge(AMergedWith: TObject; AOptions: TJX4Options);
begin
  try
    RaiseIfCanceled(AOptions);
    TxRTTI.CallMethodProc('JSONMerge', Self, [ AMergedWith, TValue.From<TJX4Options>(AOptions) ]);
    except
    on TJX4ExceptionAborted do
    begin
      if joRaiseOnAbort in AOptions then raise;
      Exit;
    end;
    on Ex: Exception do
    begin
      if joRaiseOnException in AOptions then raise;
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
begin
  for LSrcField in TxRTTI.GetFields(Self) do
  begin
    for LMrgField in TxRTTI.GetFields(AMergedWith) do
    begin
      RaiseIfCanceled(AOptions);
      if Assigned(TxRTTI.GetFieldAttribute(LMrgField, TJX4Transient)) then Continue;
      if (LSrcField.Name = LMrgField.Name) then
      begin
        if TxRtti.FieldAsTValue(Self, LSrcField, LSrcValue) and  TxRtti.FieldAsTValue(AMergedWith, LMrgField, LMgrValue) then
        begin
          LSrcValue.JSONMerge(LMgrValue, AOptions);
          if LSrcValue.IsEmpty then LSrcValue  := '';
          LSrcField.SetValue(self, LSrcValue);
          Break;
        end;
        if TxRtti.FieldAsTObject(Self, LSrcField, LSrcObj) and  TxRtti.FieldAsTObject(AMergedWith, LMrgField, LMrgObj) then
        begin
          if Assigned(LSrcObj) and Assigned(LMrgObj) then
             TxRTTI.CallMethodProc('JSONMerge', LSrcObj, [ LMrgObj, TValue.From<TJX4Options>(AOptions)]);
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

class function TJX4Object.LoadFromJSONFile<T>(const AFilename: string; AEncoding: TEncoding): T;
var
  LJstr: string;
begin
  Result := Nil;
  LoadFromFile(AFilename, LJStr, AEncoding);
  if LJStr.IsEmpty then Result := Nil else Result := TJX4Object.FromJSON<T>(LJStr);
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

class function TJX4Object.ToYAML(AObj: TJX4Object; AOptions: TJX4Options = [ joNullToEmpty ]): string;
begin
  try
    RaiseIfCanceled(AOptions);
    Result := TYAMLUtils.JsonToYaml(TJX4Object.ToJSON(AObj));
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

class function TJX4Object.ToYAML(AStr: string; AOptions: TJX4Options = [ joNullToEmpty ]): string;
begin
  try
    RaiseIfCanceled(AOptions);
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

function TJX4Object.ToYAML(AOptions: TJX4Options = [ joNullToEmpty ]): string;
begin
  try
    RaiseIfCanceled(AOptions);
    Result := ToYAML(Self, AOptions);
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
        if joRaiseOnException in AOptions then raise;
      end;
  end;
end;

function TJX4Object.SaveToJSONFile(
  const AFilename: string;
  ABeautify: Boolean = False;
  AOptions: TJX4Options = [ joNullToEmpty ];
  AEncoding: TEncoding = Nil;
  AZip: TCompressionLevel = clNone
): Int64;
begin
  Result := 0;
  RaiseIfCanceled(AOptions);
  if ABeautify then
    Result := TJX4Object.SaveToFile(AFilename,  TJX4Object.FormatJSON( TJX4Object.ToJSON(Self, AOptions) ) , AEncoding, AZip, False)
  else
    Result := TJX4Object.SaveToFile(AFilename,  TJX4Object.ToJSON(Self, AOptions), AEncoding, AZip, False);
end;

function TJX4Object.SaveToYAMLFile(
  const AFilename: string;
  AOptions: TJX4Options = [ joNullToEmpty ];
  AEncoding: TEncoding = Nil;
  AZip: TCompressionLevel = clNone
): Int64;
begin
  try
    RaiseIfCanceled(AOptions);
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

class function TJX4Object.LoadFromYAMLFile<T>(const AFilename: string; AEncoding: TEncoding;AOptions: TJX4Options): T;
var
  LJstr: string;
begin
  Result := Nil;
  try
    RaiseIfCanceled(AOptions);
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

class function TJX4Object.JSONtoYAML(const AJson: string; AOptions: TJX4Options): string;
begin
  try
    RaiseIfCanceled(AOptions);
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

class function TJX4Object.YAMLtoJSON(const AYaml: string; AOptions: TJX4Options): string;
begin
  try
    RaiseIfCanceled(AOptions);
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
      if joRaiseOnException in AOptions then raise;
    end;
  end;
end;

end.

