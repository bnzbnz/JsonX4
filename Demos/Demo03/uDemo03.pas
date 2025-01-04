unit uDemo03;

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

  TPrimitive = class(TJX4Object)
    Str: TValue;
    Bool: TValue;
    I: TValue; // as Int
    UI: TValue; // as UInt
    Dble: TValue; // as Double
    Curr: TValue; // as Vurrency
    NullStr: TValue;
    // ...
  end;

  TObjectDemo = class(TJX4Object)
    Str:  TValue;
    Keys: TJX4ValList;                            // an array(List) of strings : TArray<string>
    Nums: TJX4ValDict;                             // An dictionary of Numbers (<string, number>)  *JSON allows only strings as key
    Primitives: TJX4List<TPrimitive>;                   // A list of TPrimitives
    SLists: TJX4List<TJX4ValList>;                // A list of string Lists
    PDicList: TJX4List<TJX4Dic<TJX4List<TPrimitive>>>;  // ouch ! A List of dictionaries of TPrimitives Objects Lists !!!
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo: TObjectDemo;
  Json: string;
  s: TJX4ValList;
begin
  Memo1.Lines.Clear;

  Demo := TObjectDemo.Create;
  Demo.Str := '~~😃~~'; // UTF8 Support

  // TJX3List<TJX3Str> : Array<string>
  Memo1.lines.add('TJX3List<TJX3Str> : Array<string> :');
  Demo.Keys.Add('Q W E R T Y');
  Demo.Keys.Add('A Z E R T Y');
  Json := TJX4Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // + TJX3Dic<TJX3Num> : Dictionary<string, number> (JSON only allows strings as keys)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3Dic<TJX3Num> : Dictionary<string, number> :');
  Demo.Nums.Add('Int', 1111);
  Demo.Nums.Add('Int64', 2222);
  Demo.Nums.Add('Double', 33.33);
  Demo.Nums.Add('Currency', 44.44);
  Json := TJX4Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // TJX3List<TPrimitives> : Array<TPrimitives)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TPrimitives>  : Array<TPrimitives) :');
  Demo.Primitives.Add(TPrimitive.Create);
  Demo.Primitives.Last.Bool := True;
  Demo.Primitives.Last.I := 111;
  Demo.Primitives.Add(TPrimitive.Create);
  Demo.Primitives.Last.Bool := False;
  Demo.Primitives.Last.Dble := 333.33;
  Json := TJX4Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);

  // TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>>
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TJX3List<TJX3Str>> : Array<Array<string>>> :');
  S := TJX4ValList.Create;
  S.Add('TTT');
  S.Add('OOO');
  Demo.SLists.Add(S);
  S := TJX4ValList.Create;
  S.AddRange(['XXX', 'YYY', 'ZZZ']);
  Demo.SLists.Add(S);
  Json := TJX4Object.ToJson(Demo, [joNullToEmpty]);
  Memo1.lines.add(Json);
  Memo1.lines.add('');
  Json := TJX4Object.ToJson(Demo, [joNullToEmpty]);;
  Memo1.lines.add(Json);

  // TJX3List<TJX3Dic<TJX3List<TPrimitives>>> : Array<Dictionary<string, Array<TPrimitives>>> :)
  Memo1.lines.add('');
  Memo1.lines.add('TJX3List<TJX3Dic<TJX3List<TPrimitives>>>  : Array<Dictionary<string, Array<TPrimitives>>>');
  var p1 := TJX4List<TPrimitive>.NewAdd(TPrimitive.Create);              // Create a 2 elements Primitives array
  p1.First.Str := 'Boolean1';                                            // Acdess the Last item (which is also the first in this case)
  p1.First.Bool := True;
  var p2 := TJX4List<TPrimitive>.NewAdd(TPrimitive.Create);              // Create a 2 elements Primitives array
  p2[0].Str := 'Boolean2';
  p2[0].Bool := True;
  var d1 := TJX4Dic<TJX4List<TPrimitive>>.Create;                        // Create the dictionary ownning the 2 lists
  d1.Add('DicVal1', p1);
  d1.Add('DicVal2', p2);
  Demo.PDicList.Add(d1);
  Demo.PDicList.Add( d1.Clone<TJX4Dic<TJX4List<TPrimitive>>>);            // Adding the Dict and its clone to the main list
  Json := Demo.ToJson([joNullToEmpty]);
  Memo1.lines.add(Json);

  Memo1.lines.add('');                                                    // Format
  Memo1.lines.add('Formatted:');
  Memo1.lines.add(TJX4Object.Format(Json, 4));

  Demo.Free;
end;

end.
