unit uDemo02;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
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
    Null: TValue;
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

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo, NewDemo, CloneDemo: TInnerObjectDemo;
  Json: string;
begin
  Memo1.Lines.Clear;

  // PLease note that JSX4 owns all objects !
  // It handles construction and destruction of them for you...
  // You may add any number of inner/classes.

  Demo := TInnerObjectDemo.Create;
  Demo.S := '~~😃~~'; // UTF8 Support
  Demo.SubClass.X := 222;
  Demo.SubClass.PClass.Bool := True;
  Demo.SubClass.PClass.B := 1234;
  Demo.SubClass.PClass.D:= 2.22;

  Demo.SubClass.PClass.Str:= 'ABC';

  // Raw Json
  Json := Demo.ToJson([joRaiseException]);
  Memo1.lines.add('Raw Original Object:');
  Memo1.lines.add(Json);

  // Optimized Json

  Json := Demo.ToJson([joNullToEmpty]);
  Memo1.lines.add('Optimized Original Object:');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewDemo := TJX4Object.FromJSON<TInnerObjectDemo>(Json, [joRaiseException]);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := NewDemo.ToJson([joNullToEmpty]);
  Memo1.lines.add('Duplicate Object:');
  Memo1.lines.add(Json);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX4Object.Format(Json));

  // You may also cloned any JSX3 Objects.
  CloneDemo := Demo.Clone<TInnerObjectDemo>;

  CloneDemo.Free;
  NewDemo.Free;
  Demo.Free;

end;

end.
