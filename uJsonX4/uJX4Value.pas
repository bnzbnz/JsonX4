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
unit uJX4Value;

interface
uses
    uJX4Object
  , RTTI
  , Classes
  , SysUtils
  ;

type

  TJX4TValueKind = (tkvUnknown, tkvEmpty, tkvString, tkvBool, tkvInteger, tkvFloat);

  TJX4TValueHelper = record helper for TValue
  private
    function  GetDateTime: TDateTime;
    procedure SetDateTime(const AValue: TDateTime);
  public

    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    function  JSONClone(AOptions: TJX4Options = []): TValue;
    function  JSONMerge(AMergedWith: TValue; AOptions: TJX4Options): TValue;
    procedure JSONClear;

    function  TypeKind:                           TJX4TValueKind;
    function  IsString:                           Boolean;
    function  ToString(Decimal: Integer = 2):     string;
    function  IsInteger:                          Boolean;
    function  ToInteger:                          int64;
    function  IsFloat:                            Boolean;
    function  ToFloat:                            Extended;
    function  IsBoolean:                          Boolean;
    function  ToBoolean:                          Boolean;

    //Conversion Tools

    //Conversion Tools

    function  ToKiBMiBGiBTiB:                     string;
    function  ToMB:                               real;
    function  ToMiB:                              real;
    function  ToGB:                               real;
    function  ToGiB:                              real;
    function  ToTB:                               real;
    function  ToTiB:                              real;

    function  ToPercent(Decimal: Integer = 2; Symbol: Boolean = True): string;
    function  ToLimit:                            string;
    function  ToNow:                              TDateTime;
    function  Duration:                           string;

    function  TimeStamp:                          Int64;

    property  DateTime:  TDateTime read GetDateTime write SetDateTime;
  end;

  MyTThread = class(TThread);  //  TThread Protected Access

var
  GFormatSettings: TFormatSettings;

implementation
uses
    System.Generics.Collections
  , DateUtils
  , uJX4Rtti
  , JSON
  ;

function TJX4TValueHelper.JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
var
  LName:      string;
  LValue:     string;
  LAttr:      TCustomAttribute;
begin
  Result := Nil;
  TJX4Object.RaiseIfCanceled(AIOBlock.Options);
  if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient)) then Exit;
  case Self.TypeKind of
    tkvString:  LValue := '"' + TJX4Object.EscapeJSONStr(Self.AsString, joSlashEncode in AIOBlock.Options) + '"';
    tkvBool:    LValue := cBoolToStr[Self.AsBoolean];
    tkvInteger: LValue := Self.AsInt64.ToString;
    tkvFloat:   begin
      LValue := FormatFloat('0.0#########', Self.AsExtended, GFormatSettings);
    end;
  else
    if joNullToEmpty in AIOBlock.Options then Exit;
    Self := Nil;
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
      if Assigned(LAttr) then LValue := TJX4Default(LAttr).Value.ToString;
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
  TJX4Object.RaiseIfCanceled(AIOBlock.Options);
  if Assigned(AIOBlock.Field) and Assigned(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Transient)) then Exit;
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
      if LJPair.JsonValue.ToString.IndexOf(TFormatSettings.Invariant.DecimalSeparator) = -1 then
        Self := TJSONNumber(LJPair.JsonValue).AsInt64
      else
        begin
        Self := TJSONNumber(LJPair.JsonValue).AsDouble;
        var a := Self;
        a:=a;
        end;
  end else begin
    LAttr := TJX4Default(TxRTTI.GetFieldAttribute(AIOBlock.Field, TJX4Default));
    if Assigned(LAttr) then Self := TJX4Default(LAttr).Value else Self := Nil;
  end;
end;

function TJX4TValueHelper.JSONMerge(AMergedWith: TValue; AOptions: TJX4Options): TValue;
begin
  if jmoUpdate in AOptions then
     if (not AMergedWith.IsEmpty) then
       Self := AMergedWith;
end;

procedure TJX4TValueHelper.JSONClear;
begin
  Self := Nil;
end;

function TJX4TValueHelper.JSONClone(AOptions: TJX4Options): TValue;
begin
  Result := Self;
end;


function TJX4TValueHelper.TypeKind: TJX4TValueKind;
begin
  if Self.IsEmpty then
  begin
    Result := tkvEmpty;
    Exit;
  end;
  case Self.Kind of
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := tkvString;
    tkEnumeration: Result := tkvBool;
    tkInteger, tkInt64: Result := tkvInteger;
    tkFloat: Result := tkvFloat;
  else
    Result := tkvUnknown;
  end;
end;

function TJX4TValueHelper.IsString: Boolean;
begin
  Result := TypeKind = tkvString;
end;

function TJX4TValueHelper.ToString(Decimal: Integer): string;
begin
  case self.TypeKind of
    tkvString: Result := Self.AsString;
    tkvBool: Result := cBoolToStr[Self.AsBoolean];
    tkvInteger: Result := Self.AsInt64.toString;
    tkvFloat: Result := FloatToStrF(Self.AsExtended, ffFixed, 16, Decimal);
  else
    Result := '';
  end;
end;

function TJX4TValueHelper.IsInteger: Boolean;
begin
  Result := TypeKind = tkvInteger;
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

function TJX4TValueHelper.IsFloat: Boolean;
begin
  Result := TypeKind = tkvFloat;
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

function TJX4TValueHelper.IsBoolean: Boolean;
begin
  Result := TypeKind = tkvBool;
end;

function TJX4TValueHelper.ToBoolean: Boolean;
begin
  case self.TypeKind of
    tkvString: Result := StrToBool(Self.AsString);
    tkvBool: Result := Self.AsBoolean;
    tkvInteger: Result := Self.AsInt64 = 0;
    tkvFloat: Result := Self.AsExtended = 0;
  else
    Result := False;
  end;
end;

function TJX4TValueHelper.ToLimit: string;
begin
  Result := '∞';
  if Self.AsInt64 > 0  then Result := Self.ToString;
end;

function TJX4TValueHelper.ToKiBMiBGiBTiB: string;
var
  x: Extended;
begin
  Result := '0 B';
  x := Self.ToInteger;
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

function TJX4TValueHelper.ToMB: real;
begin
  Result := Self.AsInt64 / 1000000;
end;

function TJX4TValueHelper.ToMiB: real;
begin
  Result := Self.AsInt64 / 1048576;
end;

function TJX4TValueHelper.ToGB: real;
begin
  Result := Self.AsInt64 / 1000000000;
end;

function TJX4TValueHelper.ToGiB: real;
begin
  Result := Self.AsInt64 / 1073741824
end;

function TJX4TValueHelper.ToTB: real;
begin
  Result := Self.AsInt64 / 1000000000000;
end;

function TJX4TValueHelper.ToTiB: real;
begin
  Result := Self.AsInt64 / 1099511627776;
end;

function TJX4TValueHelper.Duration: string;
var
  Days, Hours, Mins, Secs: word;
  totalsecs: Int64;
begin
  Result := '∞';
  totalsecs := Self.ToInteger;
  if totalsecs >= 8640000 then Exit;
  days := totalsecs div SecsPerDay;
  totalsecs := totalsecs mod SecsPerDay;
  hours := totalsecs div SecsPerHour;
  totalsecs := totalsecs mod SecsPerHour;
  mins := totalsecs div SecsPerMin;
  totalsecs := totalsecs mod SecsPerMin;
  Result := '';
  secs := totalsecs;
  if days > 0 then
    Result := Result + days.ToString + 'd ';
  if hours > 0 then
    Result := Result + hours.ToString + 'h ';
  if mins > 0 then
    Result := Result + mins.ToString + 'm ';
  if days = 0 then
    Result := Result + secs.ToString + 's ';
end;

function TJX4TValueHelper.ToPercent(Decimal: Integer; Symbol: Boolean): string;
var
  x: Double;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToDouble else x := Self.AsExtended;
  Result := '0';
  if x < 0 then Exit;
  Result := Format('%.' + Decimal.ToString + 'f', [x * 100] );
  if Symbol then Result := Result + ' %';
end;

function TJX4TValueHelper.ToNow: TDateTime;
begin
  Result := IncSecond(Now, Self.AsInt64);
end;

function TJX4TValueHelper.GetDateTime: TDateTime;
begin
  if Self.IsEmpty then Exit(0);
  if Self.TypeKind <> tkvString then Exit(0);
  try
    Result := ISO8601ToDate(Self.AsString, True);
  except
    Result := 0;
  end;
end;

procedure TJX4TValueHelper.SetDateTime(const AValue: TDateTime);
begin
  try
    Self := DateToISO8601(AValue, True);
  except
    Self.Empty;
  end;
end;

function TJX4TValueHelper.Timestamp: Int64;
begin
  if Self.IsEmpty then Exit(0);
  if Self.TypeKind <> tkvString then Exit(0);
  try
    Result := DateTimeToUnix( ISO8601ToDate(Self.AsString, True), True);
  except
    Result := 0;
  end;
end;

initialization
  GFormatSettings := TFormatSettings.Create;
  GFormatSettings.DecimalSeparator := '.';
  GFormatSettings.ThousandSeparator := #0;
end.
