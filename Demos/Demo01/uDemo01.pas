unit uDemo01;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
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

  TSubObj = class(TJX4Object)
    M: TJX4DictOfValues;
  end;

  TPrimitives = class(TJX4Object)
    Str:  TValue;  // a String
    Bool: TValue;  // a Boolean
    Int: TValue;  // an Int64
    Dec: TValue;  // a Decimal
    Cur: TValue;  // a Currenty
    NullStr: TValue; // a Null String Value
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Primitives, NewPrimitives: TPrimitives;
  Json: string;
begin
  caption := 'Version : ' + TJX4Object.Version;

  Memo1.Lines.Clear;

  Primitives := TPrimitives.Create;
  Primitives.Str := 'testing 😜';
  Primitives.Bool := True;
  Primitives.Int := -999;
  Primitives.Dec := 2.2;
  Primitives.Cur := 22.0; // Make sure this is a decimal value not an Integer
  Primitives.NullStr := Nil;

  // Raw Json
  Json := Primitives.ToJson([]);
  Memo1.lines.add('Raw String Object:');
  Memo1.lines.add(Json);

  // Optimized Json
  Memo1.lines.add('');
  Json := Primitives.ToJson([joNullToEmpty]);
  Memo1.lines.add('Optimized String Object (null removed):');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewPrimitives := TJX4Object.FromJSON<TPrimitives>(Json);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := Primitives.ToJson;
  Memo1.lines.add('New Cloned Object:');
  Memo1.lines.add(Json);

  // Checking Values
  Memo1.lines.add('');
  Memo1.lines.add('Checking the New Object Values:');
  Memo1.lines.add('Str: ' + NewPrimitives.Str.AsString);
  Memo1.lines.add('Int64: ' + NewPrimitives.Int.AsOrdinal.ToString);
  Memo1.lines.add('Decimal: ' + NewPrimitives.Dec.AsExtended .ToString);
  Memo1.lines.add('Currency: ' + NewPrimitives.Cur.AsCurrency.ToString);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(NewPrimitives.Format);

  NewPrimitives.Free;
  Primitives.Free;

end;

end.
