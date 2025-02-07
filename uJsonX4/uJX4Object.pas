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
  System.Generics.Collections
  , RTTI
  , JSON
  , SysUtils
  , uJX4Rtti
  ;

const
  CJX4Version = $0102; // 01.02
  CBoolToStr: array[Boolean] of string = ('false','true');
  
type

  TJX4Option  = (
        joNullToEmpty
      , joRaiseException
      , joRaiseOnMissingField
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
    Value:      string;
    constructor Create(const AValue: string);
  end;

  TJX4Required = class(TCustomAttribute);

  TJX4Excluded = class(TCustomAttribute);

  TJX4Unmanaged = class(TCustomAttribute);
  
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

  TJX4Object = class(TObject)
  public
    constructor     Create;
    destructor      Destroy; override;

    function        JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure       JSONDeserialize(AIOBlock: TJX4IOBlock);
    procedure       JSONClone(ADestObj: TObject; AOptions: TJX4Options);
    procedure       JSONMerge(AMergedWith: TObject; AOptions: TJX4Options);

    class function  New<T:class, constructor>: T;
    class function  ToJSON(AObj: TObject; AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    function        ToJSON(AOptions: TJX4Options = [ joNullToEmpty ]): string; overload;
    class function  FromJSON<T:class, constructor>(const AJson: string; AOptions: TJX4Options = []): T; overload;

    function        Clone<T:class, constructor>(AOptions: TJX4Options= []): T; overload;
    procedure       Merge(AMergedWith: TObject; AOptions: TJX4Options = []);

    // Utils
    class function  Version: string;
    class function  VersionValue: integer;

    class function  NameDecode(const ToDecode: string): string; static;
    class procedure VarEscapeJSONStr(var AStr: string); overload; static;
    class function  EscapeJSONStr(const AStr: string): string; overload; static;
    class function  JsonListToJsonString(const AList: TList<string>): string; static;
    class function  FormatJSON(const AJson: string; AIndentation: Integer = 2): string; static;

    class function  LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64; overload;
    class function  SaveToFile(const Filename: string; const AStr: string; AEncoding: TEncoding): Int64; overload;
    class function  LoadFromFile<T:class, constructor>(const AFilename: string; AEncoding: TEncoding): T; overload;
    function        SaveToFile(const AFilename: string; AEncoding: TEncoding): Int64; overload;

 end;
 TJX4Obj = TJX4Object;
 TJX4    = TJX4Object;


implementation
uses
    TypInfo
  , Classes
  , DateUtils
  , uJX4Value
  ;

constructor TJX4Name.Create(const AName: string);
begin
  Name := AName;
end;

constructor TJX4Default.Create(const AValue: string);
begin
  Value := AValue;
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
begin
  inherited Create;
  for LField in TxRTTI.GetFields(Self) do
  begin
    if  (LField.FieldType.TypeKind in [tkClass]) and (LField.Visibility in [mvPublic]) then
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

destructor TJX4Object.Destroy;
var
  LField:   TRTTIField;
  LFields:  TArray<TRttiField>;
  LObj:     TOBject;
begin
  LFields := TxRTTI.GetFields(Self);
  for LField in LFields do
  begin
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
  try
    Result := T.Create;
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

  LIOBlock := TJX4IOBlock.Create;
  LParts := TList<string>.Create;
  try

    LFields := TxRTTI.GetFields(Self);
    LParts.Capacity := Length(LFields);
    for LField in LFields do
    begin
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
    for LSrcField in LSrc do
    begin
      if LSrcField.Name = LDestField.Name then
      begin
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
  LJPairList:   TStringList;
  LTValue:      TValue;
begin
  LJPairList := Nil;
  LIOBlock := TJX4IOBlock.Create;
  try

    for LField in TxRTTI.GetFields(Self) do
    begin

      if  not ((LField.Visibility in [mvPublic])
          and ((LField.FieldType.TypeKind in [tkClass])
          or (LField.FieldType.TypeKind in [tkRecord]))) then Continue;

      LName := NameDecode(LField.Name);
      LAttr := TJX4Name(TxRTTI.GetFieldAttribute(LField, TJX4Name));
      if Assigned(LAttr) then LName := TJX4Name(LAttr).Name;

      if (JoRaiseOnMissingField in AIOBlock.Options) and (Length(TxRTTI.GetFields(Self)) < AIOBlock.JObj.count) then
      begin
        LFieldFound := AIOBlock.JObj.Count > 0;
        for LJPair in  AIOBlock.JObj do
         if LName = LJPair.JsonString.Value then
         begin
           LFieldFound := True;
           Break;
         end;
        if not LFieldFound then
          raise Exception.Create(SysUtils.Format('Missing Property %s in class %s, from JSON fields: %s%s', [LName, Self.ClassName, sLineBreak, LJPairList.Text]));
      end;

      LFieldFound := False;
      for LJPair in  AIOBlock.JObj do
      begin
        if LJPair.JsonValue is TJSONNull then Break;
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

          LIOBlock.Init(LField.Name, LJObj, LField, AIOBlock.Options);
          if TxRtti.FieldAsTValue(Self, LField, LTValue) then
          begin
            LTValue.JSONDeserialize(LIOBlock);
            if LTValue.IsEmpty then LTValue := '';
            LField.SetValue(Self, LTValue);
          end else begin
            LObj := LField.GetValue(Self).AsObject;
            if not Assigned(LObj) then
            begin
              LObj := TxRTTI.CreateObject(LField.FieldType.AsInstance);
              TxRTTI.CallMethodProc('JSONCreate', LObj, [True]);
              LField.SetValue(Self, LObj);
            end;
            TxRTTI.CallMethodProc('JSONDeserialize', LObj, [LIOBlock]);
          end;

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

          Break;
        end;

        if not LFieldFound then
        begin

          LAttr := TJX4Default(TxRTTI.GetFieldAttribute(LField, TJX4Default));
          if Assigned(LAttr) then
          begin
            if TxRtti.FieldIsTValue(LField, [mvPublic]) then
              LField.SetValue(Self, TJX4Default(LAttr).Value)
            else begin
              LObj := LField.GetValue(Self).AsObject;
              TxRTTI.CallMethodProc('JSONSeltValue', LObj, [LName, TJX4Default(LAttr).Value, LIOBlock]);
            end;
          end;
          continue;
        end;

          if Assigned(TJX4Required(TxRTTI.GetFieldAttribute(LField, TJX4Required))) then
            raise Exception.Create(SysUtils.Format('Class: %s : "%s" is required but not defined', [Self.ClassName, LName]));

        end;
        Continue;
      end;

  finally
    LJPairList.Free;
    LIOBlock.Free;
  end;
end;

class function TJX4Object.ToJSON(AObj: TObject; AOptions: TJX4Options): string;
var
  LIOBlock: TJX4IOBlock;
  LResult: TValue;
begin
  LIOBlock := Nil;
  try
  try
    LIOBlock := TJX4IOBlock.Create('', nil, nil, AOptions);
    LResult := TxRTTI.CallMethodFunc('JSONSerialize', AObj, [LIOBlock]);
    if not LResult.IsEmpty then Result := LResult.AsString;
  finally
    LIOBlock.Free;
  end;
  except
    on Ex: Exception do
    begin
      Result := '';
      if joRaiseException in AOptions then Raise;
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
  LIOBlock := Nil;
  LJObj := Nil;
  try
    Result := T.Create;
    if AJson.Trim.IsEmpty then Exit;
    try
      LJObj := TJSONObject.ParseJSONValue(AJson, True, joRaiseException in AOptions) as TJSONObject;
      LIOBlock := TJX4IOBlock.Create('', LJObj, Nil, AOptions);
      TxRTTI.CallMethodProc('JSONDeserialize', Result, [LIOBlock]);
    except
      on Ex: Exception do
      begin
        FreeAndNil(Result);
        if joRaiseException in AOptions then Raise;
      end;
    end;
  finally
    LJObj.Free;
    LIOBlock.Free;
  end;
end;

class function TJX4Object.Version: string;
begin
  Result := Format('%0.2d.%0.2d', [
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

class function TJX4Object.New<T>: T;
begin
  Result := T.Create;
end;

class procedure TJX4Object.VarEscapeJSONStr(var AStr: string);
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
    case LP^ of
      #0..#31, '\', '"' : begin LMatch := LP; Break; end;
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
    else
      LSb.Append(LP^);
    end;
    Inc(LP);
  end;
  AStr := LSb.ToString;
  LSb.Free;
end;

class function TJX4Object.EscapeJSONStr(const AStr: string): string;
begin
  Result := AStr;
  VarEscapeJSONStr(Result);
end;

class function TJX4Object.JsonListToJsonString(const AList: TList<string>): string;
var
  LSb:  TStringBuilder;
  LIdx: integer;
begin
  if AList.Count = 0 then Exit('');
  if AList.Count = 1 then Exit(AList[0]);
  LSb := TStringBuilder.Create;
  for LIdx:= 0 to AList.Count -1 do
  begin
    LSb.Append(AList[LIdx]);
    if LIdx <> AList.Count -1 then LSb.Append(',') ;
  end;
  Result := LSb.ToString;
  LSb.Free;
end;

class function TJX4Object.FormatJSON(const AJson: string; AIndentation: Integer): string;
var
  TmpJson: TJsonObject;
begin
  TmpJson := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  Result := TJSONAncestor(TmpJson).Format(AIndentation);
  FreeAndNil(TmpJson);
end;

procedure TJX4Object.Merge(AMergedWith: TObject; AOptions: TJX4Options);
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
    for LMrgField in  TxRTTI.GetFields(AMergedWith) do
    begin
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

class function TJX4Object.LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64;
var
  LFS : TFileStream;
  LSS: TStringStream;
begin
  LFS := nil;
  LSS := Nil;
  try
    LFS := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    LSS := TStringStream.Create('', AEncoding, True);
    Result := LSS.CopyFrom(LFS, -1);
    AStr := LSS.DataString;
  finally
    LSS.Free;
    LFS.Free;
  end;
end;

class function TJX4Object.LoadFromFile<T>(const AFilename: string; AEncoding: TEncoding): T;
var
  LJstr: string;
begin
  Result := Nil;
  if not FileExists(AFilename) then Exit;
  LoadFromFile(AFilename, LJStr, AEncoding);
  Result := TJX4Object.FromJSON<T>(LJStr);
end;

class function TJX4Object.SaveToFile(const Filename: string; const AStr: string; AEncoding: TEncoding): Int64;
var
  LFS: TFileStream;
  LSS: TStringStream;
begin
  LFS := nil;
  LSS := Nil;
  try
    LFS := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
    LSS := TStringStream.Create(AStr, AEncoding);
    Result := LFS.CopyFrom(LSS, -1);
  finally
    LSS.Free;
    LFS.Free;
  end;
end;

function TJX4Object. SaveToFile(const AFilename: string; AEncoding: TEncoding): Int64;
begin
  Result := TJX4Object.SaveToFile(AFilename, TJX4Object.ToJSON(Self), AEncoding);
end;

end.
