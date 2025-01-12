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
  cJX4Version = 01002; // 01.002
  cBoolToStr: array[Boolean] of string = ('false','true');
  
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

  TJX4TValueKind = (tkvUnknown, tkvString, tkvBool, tkvInteger, tkvFloat);

  TJX4TValueHelper = record helper for TValue
  private
    function  GetISO8601: TDateTime;
    function  GetISO8601Utc: TDateTime;
    procedure SetISO8601(const AValue: TDateTime);
    procedure SetISO8601Utc(const AValue: TDateTime);
    function  GetTimestamp: TDateTime;
    procedure SetTimestamp(const AValue: TDateTime);
    function  GetTimestampUtc: TDateTime;
    procedure SetTimestampUtc(const AValue: TDateTime);
    function  GetDateTime: TDateTime;
    procedure SetDateTime(const AValue: TDateTime);
  public
  
    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    function  JSONClone(AOptions: TJX4Options = []): TValue;
    function  JSONMerge(AMergedWith: TValue; AOptions: TJX4Options): TValue;

    //Conversion Tools
    function  TypeKind:                           TJX4TValueKind;
    function  ToBKiBMiB:                          string;
    function  ToPercent(Decimal: Integer = 2):    string;
    function  ToStrFloat(Decimal: Integer = 2):   string;
    function  ToSecondsFromNow:                   string;
    function  ToSecToDuration:                    string;
    function  ToString:                           string;
    function  ToInteger:                          int64;
    function  ToFloat:                            Extended;


    property  ISO8601:      TDateTime read GetISO8601 write SetISO8601;
    property  ISO8601Utc:   TDateTime read GetISO8601Utc write SetISO8601Utc;
    property  Timestamp:    TDateTime read GetTimestamp write SetTimestamp;
    property  TimestampUtc: TDateTime read GetTimestampUtc write SetTimestampUtc;
    property  DateTime :    TDateTime read GetDateTime write SetDateTime;
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
    class function GetVersionStr: string; overload;
    class function GetVersion:    Integer; overload;

    class function  NameDecode(const ToDecode: string): string; static;
    class procedure VarEscapeJSONStr(var AStr: string); overload; static;
    class function  EscapeJSONStr(const AStr: string): string; overload; static;
    class function  JsonListToJsonString(const AList: TList<string>): string; static;
    class function  FormatJSON(const AJson: string; AIndentation: Integer = 2): string; static;

    class function  LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64;
    class function  SaveToFile(const Filename: string; const AStr: string; AEncoding: TEncoding): Int64;

 end;
 TJX4Obj = TJX4Object;
 TJX4    = TJX4Object;


implementation
uses
    TypInfo
  , Classes
  , DateUtils
  ;

function TJX4TValueHelper.TypeKind: TJX4TValueKind;
begin
  case Self.Kind of
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := tkvString;
    tkEnumeration: Result := tkvBool;
    tkInteger, tkInt64: Result := tkvInteger;
    tkFloat:Result := tkvFloat;
  else
    Result := tkvUnknown;
  end;
end;

function TJX4TValueHelper.ToString: String;
begin
  case self.TypeKind of
    tkvString: Result := Self.AsString;
    tkvBool: Result := cBoolToStr[Self.AsBoolean];
    tkvInteger: Result := Self.AsInt64.toString;
    tkvFloat: Result := Self.AsExtended.ToString;
  else
    Result := '';
  end;
end;

function TJX4TValueHelper.ToInteger: Int64;
begin
  case self.TypeKind of
    tkvString: Result := Self.AsString.ToInt64;
    tkvBool: Result := Self.AsBoolean.ToInteger;
    tkvInteger: Result := Self.AsInt64;
    tkvFloat: Result := Trunc(Self.AsExtended);
  else
    Result := 0;
  end;
end;

function TJX4TValueHelper.ToFloat: Extended;
begin
  case self.TypeKind of
    tkvString: Result := Self.AsExtended;
    tkvBool: Result := Self.AsBoolean.ToInteger;
    tkvInteger: Result := Self.AsInt64;
    tkvFloat: Result := Self.AsExtended;
  else
    Result := 0;
  end;
end;

function TJX4TValueHelper.ToBKiBMiB: string;
var
  x: Extended;
begin
  Result := '0 B';
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  if x < 0 then
  begin
    Result := 'N/A';
    Exit;
  end else
  if (x / 1099511627776 >= 1) then
  begin
    Result := Format('%.2f', [x / 1099511627776 ])+ ' TiB';
    Exit;
  end else

  if (x / (1024 * 1024 * 1024) >= 1) then
  begin
    Result := Format('%.2f', [x /(1024 * 1024 * 1024)] )+ ' GiB';
    Exit;
  end else
  if (x / (1024 * 1024)>= 1) then
  begin
    Result := Format('%.2f', [x /(1024 * 1024)] )+ ' MiB';
    Exit;
  end else
  if (x / (1024)) >= 1 then
  begin
    Result := Format('%.2f', [x /(1024)] )+ ' KiB';
  end else begin
    Result := Format('%.0f', [x] )+ ' B';
  end;
end;

function TJX4TValueHelper.ToSecToDuration: string;
var
  Days, Hours, Mins, Secs: word;
  totalsecs: Int64;
begin
  if Self.TypeKind = tkvString then totalsecs := Self.AsString.ToInt64 else totalsecs := Self.AsInt64;
  days := totalsecs div SecsPerDay;
  totalsecs := totalsecs mod SecsPerDay;
  hours := totalsecs div SecsPerHour;
  totalsecs := totalsecs mod SecsPerHour;
  mins := totalsecs div SecsPerMin;
  totalsecs := totalsecs mod SecsPerMin;
  secs := totalsecs;
  if days >0 then
    Result := Result + days.ToString + 'd ';
  Result := Result + hours.ToString + 'h ';
  Result := Result + mins.ToString + 'm ';
  if days = 0 then
    Result := Result + secs.ToString + 's ';
end;

function TJX4TValueHelper.ToPercent(Decimal: Integer): string;
var
  x: Double;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToDouble else x := Self.AsExtended;
  Result := '0 %';
  if x < 0 then Exit;
  Result := Format('%.' + Decimal.ToString + 'f', [x * 100] ) + ' %';
end;

function TJX4TValueHelper.ToSecondsFromNow: string;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := DateTimeToStr(IncSecond(Now, x));
end;

function TJX4TValueHelper.ToStrFloat(Decimal: Integer): string;
var
  x: Double;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToDouble else x := Self.AsExtended;
  Result := Format('%.' + Decimal.ToString + 'f', [x ]);
end;

procedure TJX4TValueHelper.SetDateTime(const AValue: TDateTime);
begin
  Self := AValue;
end;

procedure TJX4TValueHelper.SetISO8601(const AValue: TDateTime);
begin
  Self := DateToISO8601(AValue, False);  
end;

procedure TJX4TValueHelper.SetISO8601Utc(const AValue: TDateTime);
begin
  Self := DateToISO8601(AValue, True);
end;

procedure TJX4TValueHelper.SetTimestamp(const AValue: TDateTime);
begin
  Self := DateTimeToUnix(AValue, False);
end;

procedure TJX4TValueHelper.SetTimestampUtc(const AValue: TDateTime);
begin
  Self := DateTimeToUnix(AValue, True);
end;

function TJX4TValueHelper.GetDateTime: TDateTime;
var
  x: Double;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToDouble else x := Self.AsExtended;
  Result := TDateTime(x);
end;

function TJX4TValueHelper.GetISO8601: TDateTime;
begin
  Result := ISO8601ToDate(Self.AsString, False);
end;

function TJX4TValueHelper.GetISO8601Utc: TDateTime;
begin
  Result := ISO8601ToDate(Self.AsString, True);
end;

function TJX4TValueHelper.GetTimestamp: TDateTime;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := UnixToDateTime(x, False);
end;

function TJX4TValueHelper.GetTimestampUtc: TDateTime;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := UnixToDateTime(Self.AsInt64, True);
end;


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

function TJX4TValueHelper.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LName:  string;
  LValue: string;
  LAttr:  TCustomAttribute;
begin
  Result := Nil;
  if Assigned(AIOBlock.Field) then
  begin
    LAttr := TJX4Excluded(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Excluded));
    if Assigned(LAttr) then Exit;
  end;
  case Self.Kind of
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      LValue := '"' + TJX4Object.EscapeJSONStr(Self.AsString) + '"';
    tkEnumeration: LValue := cBoolToStr[Self.AsBoolean];
    tkInteger, tkInt64: LValue := Self.AsInt64.ToString;
    tkFloat:
      begin
        if  Self.AsExtended.ToString.IndexOf('.') = -1 then
          LValue := Self.AsExtended.ToString + '.0'
        else
          LValue := Self.AsExtended.ToString;
      end;
  else
    if joNullToEmpty in AIOBlock.Options then Exit;
  end;

  if Assigned(AIOBlock.Field) then
  begin
    LName := AIOBlock.Field.Name;
    LAttr := TJX4Name(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Name));
    if Assigned(LAttr) then LName := TJX4Name(LAttr).Name;
  end else
    LName := AIOBlock.JsonName;
  LName := TJX4Object.NameDecode(LName);

  if Self.IsEmpty then
  begin
    LAttr := Nil;
    if Assigned(AIOBlock.Field) then
    begin
      LAttr := TJX4Default(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Default));
      if Assigned(LAttr) then LValue := TJX4Default(LAttr).Value;
    end;
    if not Assigned(LAttr) then
    begin
      if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Required)) then
        raise Exception.Create(Format('"%s" : value required', [LName]));

      if joNullToEmpty in AIOBlock.Options then Exit;
      if LName.IsEmpty then
        Result := 'null'
      else
        Result := '"' + LName + '":null';
      Exit;
    end;
  end;

  if Assigned(AIOBlock) and AIOBlock.JsonName.IsEmpty then
    Result := LValue
  else
    Result := '"' + LName + '":' + LValue;
end;

procedure TJX4TValueHelper.JSONDeserialize(AIOBlock: TJX4IOBlock);
var
  LJPair:         TJSONPair;
  LAttr:          TCustomAttribute;
begin
  Self := Nil;
  if Assigned(AIOBlock.Field) then
  begin
    LAttr := TJX4Excluded(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Excluded));
    if Assigned(LAttr) then Exit;
  end;
  LJPair := AIOBlock.JObj.Pairs[0];
  if not(Assigned(LJPair) and  (not LJPair.null) and not (LJPair.JsonValue is TJSONNull) and not (LJPair.JsonValue.Value.IsEmpty)) then
  begin
    LAttr := TJX4Default(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Default));
    if Assigned(LAttr) then Self := TJX4Default(LAttr).Value else Self := Nil;
    Exit;
  end;

  if LJPair.JsonValue.Value.IsEmpty then
  begin
    LAttr := TJX4Default(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Default));
    if Assigned(LAttr) then Self := TJX4Default(LAttr).Value;
  end
  else if LJPair.JsonValue.ClassType = TJSONString then Self := LJPair.JsonValue.Value
  else if LJPair.JsonValue.ClassType = TJSONBool then Self := StrToBool(LJPair.JsonValue.Value)
  else if LJPair.JsonValue.ClassType = TJSONNumber then
  begin
      if LJPair.JsonValue.ToString.IndexOf('.') = -1 then
        Self := TJSONNumber(LJPair.JsonValue).AsInt64
      else
        Self := TJSONNumber(LJPair.JsonValue).AsDouble;
  end else begin
    LAttr := TJX4Default(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Default));
    if Assigned(LAttr) then Self := TJX4Default(LAttr).Value else Self := Nil;
  end;
end;

function TJX4TValueHelper.JSONMerge(AMergedWith: TValue; AOptions: TJX4Options): TValue;
begin
  if jmoUpdate in AOptions then
  begin
    if (not AMergedWith.IsEmpty) then
       Self := AMergedWith;
    Exit;
  end;
end;

function TJX4TValueHelper.JSONClone(AOptions: TJX4Options): TValue;
begin
  Result := Self;
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

class function TJX4Object.GetVersion: Integer;
begin
  Result := cJX4Version;
end;

class function TJX4Object.GetVersionStr: string;
begin
  Result := Format('%4.3f', [GetVersion / 1000]);
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

class function TJX4Object.LoadFromFile(const AFilename: string; var AStr: string; AEncoding: TEncoding): Int64;
var
  FS : TFileStream;
  SS: TStringStream;
begin
  FS := nil;
  SS := Nil;
  try
    FS := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    SS := TStringStream.Create('', AEncoding, True);
    Result := SS.CopyFrom(FS, -1);
    AStr := SS.DataString;
  finally
    SS.Free;
    FS.Free;
  end;
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
          LSrcField.SetValue(Self, LSrcValue);
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

class function TJX4Object.SaveToFile(const Filename: string; const AStr: string; AEncoding: TEncoding): Int64;
var
  FS: TFileStream;
  SS: TStringStream;
begin
  FS := nil;
  SS := Nil;
  try
    FS := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
    SS := TStringStream.Create(AStr, AEncoding);
    Result := FS.CopyFrom(SS, -1);
  finally
    SS.Free;
    FS.Free;
  end;
end;

end.
