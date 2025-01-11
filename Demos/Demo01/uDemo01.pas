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
    Str:     TValue;
    Bool:    TValue;
    Num1:    TValue;
    Num2:    TValue;
    Num3:    TValue;
    Num4:    TValue; // as UInt64
    NullStr: TValue;
    // ...
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
  caption := 'Version : ' + TJX4Object.GetVersionStr;

  Memo1.Lines.Clear;

  Primitives := TPrimitives.Create;
  Primitives.Str := 'testing 😜';
  Primitives.Bool := True;

  Primitives.Num1 := -999;
  Primitives.Num2 := 999;
  Primitives.Num3 := 2.2;
  Primitives.Num4 := 22.22;
  Primitives.NullStr := Nil;

  // Raw Json
  Json := Primitives.ToJson([]);
  Memo1.lines.add('Raw Original Object:');
  Memo1.lines.add(Json);

  //Optimized Json
  Memo1.lines.add('');
  Json := Primitives.ToJson([joNullToEmpty]);
  Memo1.lines.add('Optimized Original Object:');
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
  Memo1.lines.add('Int64: ' + NewPrimitives.Num2.AsOrdinal.ToString);
  Memo1.lines.add('Currency: ' + NewPrimitives.Num4.AsCurrency.ToString);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX4Object.FormatJSON(Json));

  NewPrimitives.Free;
  Primitives.Free;

end;

end.
