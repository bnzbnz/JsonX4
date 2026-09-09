unit uDemo02;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4Value
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
    Str: TValue;
    Bool: TValue;
    b: TValue; // as Int
    c: TValue; // as UInt
    d: TValue; // as Int64
    e: TValue; // as UInt64
    NullValue: TValue;
    // ...
  end;

  TSubClassDemo = class(TJX4Object)
    X: TValue;
    PClass: TPrimitives
  end;

  TInnerObjectDemo = class(TJX4Object)
    S: TValue;
    SubClass: TSubClassDemo; // a class
  end;

var
  Form4: TForm4;

implementation
uses
    System.Diagnostics
  ;

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo, NewDemo, CloneDemo: TInnerObjectDemo;
  Json: string;
  LWatch: TStopWatch;
begin

  LWatch := TStopWatch.StartNew;
  Memo1.Lines.Clear;

  // PLease note that JSX4 owns all objects !
  // It handles construction and destruction of them for you...
  // You may add any number of nested classes.

  Demo := TInnerObjectDemo.Create;
  Demo.S := '~~😃~~'; // UTF8 Support
  Demo.SubClass.X := 222;
  Demo.SubClass.PClass.Bool := True;
  Demo.SubClass.PClass.B := 1234;
  Demo.SubClass.PClass.D:= 2.22;

  Demo.SubClass.PClass.Str:= 'ABC';

  // Serializing RawObject to Json string
  Json := Demo.ToJson;
  Memo1.lines.add('Serialized Raw Object to Json string :');
  Memo1.lines.add(Json);

  // Serializing RawObject to OPTIMIZED Json string (remove "null" fields)
  Json := Demo.ToJson([joNullToEmpty]);
  Memo1.lines.add('');
  Memo1.lines.add('Serialized RawObject to OPTIMIZED Json string (remove "null" fields) :');
  Memo1.lines.add(Json);

  // Deserializing Json string to NewObject of type
  NewDemo := TJX4Object.FromJSON<TInnerObjectDemo>(Json, []);
  Memo1.lines.add('');
  Memo1.lines.add('Deserialized Json string to NewObject of type: ');
  Memo1.lines.add(NewDemo.ClassName);

  // Cloning, Serializing and Optimizing NewObject to CloneObject
  CloneDemo := Demo.Clone<TInnerObjectDemo>;
  Json := CloneDemo.ToJson([joNullToEmpty]);
  Memo1.lines.add('');
  Memo1.lines.add('Cloned, Serialized and Optimized NewObject to CloneObject: ');
  Memo1.lines.add(Json);

  // Fomatting CloneObject
  Memo1.lines.add('');
  Memo1.lines.add('Formatting CloneObject:');
  Memo1.lines.add(CloneDemo.Format(True, 2));

  // Fomatting optimized CloneObject
  Memo1.lines.add('');
  Memo1.lines.add('Formatted Optimized CloneObject:');
  Memo1.lines.add(CloneDemo.Format(True, 2, [joNullToEmpty]));

  CloneDemo.Free;
  NewDemo.Free;
  Demo.Free;

  // Caution with this value; FMX TMemo is extremly slow !!!
  Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));

end;

end.
