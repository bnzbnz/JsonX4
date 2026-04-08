unit uDemo01;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Value
  , uJX4Object
  , uJX4List
  , uJX4Dict
  ;

type

  TForm4 = class(TForm)
    Memo1: TMemo;
    Button: TButton;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TPrimitives = class(TJX4Object)
    Str:        TValue; // as UTF8 String
    Bool:       TValue; // as Boolean
    Int:        TValue; // as Int64
    Dec:        TValue; // as Decimal
    Cur:        TValue; // as Currenty
    Dte:        TValue; // as DateTime(utc);
    Tms:        TValue; // as Unix TimStamp(utc);
    NullValue:  TValue; // as a Null String Value
  end;

  // I'm seeing it coming.... Why use TValue's instead of the native types ?
  // simply, because a json value can be null : for exemple, a json boolean can be True, False... or Null.
  // a pascal boolean can't!
  // So any json engine using native types is, basicaly, wrong ! (Delphi Rest, GBJSON, neon, etc...)

var
  Form4: TForm4;

implementation
uses
    System.Diagnostics
  , DateUtils
  ;

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Primitives: TPrimitives;
  NewPrimitives: TPrimitives;
  Json: string;
  LWatch: TStopWatch;
begin

  // GetJsonX4 version
  Caption := 'JsonX4 Version : ' + TJX4Object.Version;
    Memo1.Lines.Clear;
    LWatch := TStopWatch.StartNew;

  // Init the base object
  Primitives := TPrimitives.Create;
  Primitives.Str          := 'testing 😜';
  Primitives.Bool         := True;
  Primitives.Int          := -999;
  Primitives.Dec          := 2.0;  // Make sure this is a decimal value not an Integer
  Primitives.Cur          := 22.2;
  Primitives.Dte.DateTime := Now; // ISO8601Utc
  Primitives.Tms          := DateTimeToUnix(Now); // Int64
  Primitives.NullValue    := Nil; // a "null" value, to be removed when serialize

  // Serialize : Raw Json
  Json := Primitives.ToJson([]);
    Memo1.lines.add('Serialized Raw Object:');
    Memo1.lines.add(Json);

  // Serialize : Optimized Json (null removed
    Memo1.lines.add('');
  Json := Primitives.ToJson([joNullToEmpty]);
    Memo1.lines.add('Serialized and Optimized Object (null removed):');
    Memo1.lines.add(Json);

  // Deserializing, to a "NewPrimitive"
  NewPrimitives := TJX4Object.FromJSON<TPrimitives>(Json);
  // In fact, we should Clone it's wey faster => NewClone := Primitives.Clone<TPrimitives>;

  // Checking Values
    Memo1.lines.add('');
    Memo1.lines.add('Checking New Object Values:');
    Memo1.lines.add('Str: ' + NewPrimitives.Str.AsString);
    Memo1.lines.add('Int64: ' + NewPrimitives.Int.AsOrdinal.ToString);
    Memo1.lines.add('Decimal: ' + NewPrimitives.Dec.AsExtended .ToString);
    Memo1.lines.add('Currency: ' + NewPrimitives.Cur.AsCurrency.ToString);
    Memo1.lines.add('DateTime: ' + DateTimeToStr(NewPrimitives.Dte.DateTime));
    Memo1.lines.add('Timestamp: ' + NewPrimitives.Dte.Timestamp.ToString);

   // Memo1.lines.add('Date: ' + NewPrimitives.Dte.ISO8601Utc);

  // Formatted Json
    Memo1.lines.add('');
    Memo1.lines.add('Formatted Serialized Object:');
  Memo1.lines.add(NewPrimitives.Format);

  NewPrimitives.Free;
  Primitives.Free;

    Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));

end;

end.
