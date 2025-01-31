unit uJX4Value;

interface
uses
    uJX4Object
  , RTTI
  ;

type

  TJX4TValueKind = (tkvUnknown, tkvEmpty, tkvString, tkvBool, tkvInteger, tkvFloat);

  TJX4TValueHelper = record helper for TValue
  private
    function  GetISO8601: TDateTime;
    function  GetISO8601Utc: TDateTime;
    procedure SetISO8601(const AValue: TDateTime);
    procedure SetISO8601Utc(const AValue: TDateTime);
    function  GetTimestamp: TDateTime;
    procedure SetTimestamp(const AValue: TDateTime);
    function  GetTimestampStr: string;
    function  GetTimestampUtc: TDateTime;
    function  GetTimestampUtcStr: string;
    procedure SetTimestampUtc(const AValue: TDateTime);
    function  GetDateTime: TDateTime;
    function  GetDateTimeStr: string;
    procedure SetDateTime(const AValue: TDateTime);
  public

    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    function  JSONClone(AOptions: TJX4Options = []): TValue;
    function  JSONMerge(AMergedWith: TValue; AOptions: TJX4Options): TValue;

    function  TypeKind:                           TJX4TValueKind;
    function  ToString(Decimal: Integer = 2):     string;
    function  ToInteger:                          int64;
    function  ToFloat:                            Extended;
    function  ToBoolean:                          Boolean;

    //Conversion Tools

    //Conversion Tools

    function  ToBKiBMiB:                          string;
    function  ToMB:                               real;
    function  ToMiB:                              real;
    function  ToGB:                               real;
    function  ToGiB:                              real;
    function  ToTB:                               real;
    function  ToTiB:                              real;

    function  ToPercent(Decimal: Integer = 2; Symbol: Boolean = True):string;
    function  ToLimit:                            string;
    function  FromSecFromNow:                     string;
    function  FromSecToDuration:                  string;

    property  ISO8601:      TDateTime read GetISO8601 write SetISO8601;
    property  ISO8601Utc:   TDateTime read GetISO8601Utc write SetISO8601Utc;
    property  Timestamp:    TDateTime read GetTimestamp write SetTimestamp;
    property  TimestampUtc: TDateTime read GetTimestampUtc write SetTimestampUtc;
    property  TimestampStr:    string read GetTimestampStr;
    property  TimestampUtcStr: string read GetTimestampUtcStr;
    property  DateTime :    TDateTime read GetDateTime write SetDateTime;
    property  DateTimeStr :    string read GetDateTimeStr;

  end;

implementation
uses
    System.Generics.Collections
  , Sysutils
  , DateUtils
  , uJX4Rtti
  , JSON
  ;

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
  case Self.TypeKind of
    tkvString:  LValue := '"' + TJX4Object.EscapeJSONStr(Self.AsString) + '"';
    tkvBool:    LValue := cBoolToStr[Self.AsBoolean];
    tkvInteger: LValue := Self.AsInt64.ToString;
    tkvFloat:
      begin
        if  Self.AsExtended.ToString.IndexOf('.') = -1 then
          LValue := Self.AsExtended.ToString + '.0'
        else
          LValue := Self.AsExtended.ToString;
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
     if (not AMergedWith.IsEmpty) then
       Self := AMergedWith;
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

function TJX4TValueHelper.ToString(Decimal: Integer): string;
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

function TJX4TValueHelper.ToBKiBMiB: string;
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

function TJX4TValueHelper.FromSecToDuration: string;
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

function TJX4TValueHelper.FromSecFromNow: string;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := DateTimeToStr(IncSecond(Now, x));
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

function TJX4TValueHelper.GetDateTimeStr: string;
begin
  Result := DateTimeToStr(Self.GetDateTime);
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

function TJX4TValueHelper.GetTimestampStr: string;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := DateTimeToStr(UnixToDateTime(x, False));
end;

function TJX4TValueHelper.GetTimestampUtc: TDateTime;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := UnixToDateTime(x, True);
end;

function TJX4TValueHelper.GetTimestampUtcStr: string;
var
  x: Int64;
begin
  if Self.TypeKind = tkvString then x := Self.AsString.ToInt64 else x := Self.AsInt64;
  Result := DateTimeToStr(UnixToDateTime(x, True));
end;

end.
