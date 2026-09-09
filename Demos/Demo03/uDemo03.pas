unit uDemo03;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJX4List
  , uJX4Dict
  ;

type

  TPrimitive = class(TJX4Object)
    Str: TValue;
    Bool: TValue;
    I: TValue;      // as Int
    UI: TValue;     // as UInt
    Dble: TValue;   // as Double
    Curr: TValue;   // as Vurrency
    // ...
  end;

  TObjectDemo = class(TJX4Object)
    aStr:  TValue;                                        // string
    aDate: TValue;                                        // Datetime
    NullStr: TValue;                                      // Nil
    Keys: TJX4ValList;                                    // An array(List) of strings : TArray<string>
    Nums: TJX4ValDict;                                    // A dictionary of Numbers (<string, number>)  *JSON allows only strings as key
    Primitives: TJX4List<TPrimitive>;                     // A list of TPrimitives
    SLists: TJX4List< TJX4ListOfValues >;                 // A list of string Lists
    DicList: TJX4List<TJX4Dic<TJX4List<TPrimitive>>>;     // ouch ! A List of dictionaries of TPrimitives Objects Lists !!! (It's a nonesense !)
  end;

  TForm4 = class(TForm)
    Memo1: TMemo;
    Button: TButton;
    procedure ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SB: TStringBuilder;
    procedure ShowResult(Json: string);
  end;

var
  Form4: TForm4;

implementation
uses
    System.Diagnostics
  ;

{$R *.fmx}

procedure TForm4.FormCreate(Sender: TObject);
begin
  SB := TStringBuilder.Create;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  SB.Free;
end;

procedure TForm4.ShowResult(Json: string);
var
  BeautifiedJson: TObjectDemo;
begin
  SB.AppendLine(Json);
  SB.AppendLine('Deserialized, Optimized & Beautified : ');
  BeautifiedJson := TJX4Object.FromJSON<TObjectDemo>(Json);
  SB.AppendLine( BeautifiedJson.Format(True, 2 , [joNullToEmpty]) );
  BeautifiedJson.Free;
end;

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo: TObjectDemo;
  Json: string;
  s: TJX4ListOfValues;
  LWatch: TStopWatch;
begin
  LWatch := TStopWatch.StartNew;
  SB.Clear;

  Demo := TObjectDemo.Create;
  Demo.aStr := '~~😃~~'; // UTF8 Support
  Demo.aDate.DateTime := Now;

  // TJX4List<TJX4Str> : Array<string>
  Memo1.lines.add('TJX4List<TJX4Str> : Array<string> :');
  Demo.Keys.Add('Q W E R T Y');
  Demo.Keys.Add('A Z E R T Y');
  ShowResult( TJX4Object.ToJson(Demo, [joNullToEmpty]) );

  // TJX4Dic<TValue> : Dictionary<string, number> (JSON only allows strings as keys)
  Memo1.lines.add(sLineBreak);
  Memo1.lines.add('TJX4Dic<TJX4Num> : Dictionary<string, number> :');
  Demo.Nums.Add('Int', 1111);
  Demo.Nums.Add('Int64', 2222);
  Demo.Nums.Add('Double', 33.33);
  Demo.Nums.Add('Currency', 44.44);
  ShowResult( TJX4Object.ToJson(Demo, [joNullToEmpty]) );

  // TJX4List<TPrimitives> : Array<TPrimitives>
  Memo1.lines.add(sLineBreak);
  Memo1.lines.add('TJX4List<TPrimitives>  : Array<TPrimitives> :');
  Demo.Primitives.Add(TPrimitive.Create);
  Demo.Primitives.Last.Bool := True;
  Demo.Primitives.Last.I := 111;
  Demo.Primitives.Add(TPrimitive.Create);
  Demo.Primitives.Last.Bool := False;
  Demo.Primitives.Last.Dble := 333.33;
  ShowResult( TJX4Object.ToJson(Demo, [joNullToEmpty]) );

  // TJX4List<TJX4List<TJX4Str>> : Array<Array<string>>>
  Memo1.lines.add(sLineBreak);
  Memo1.lines.add('TJX4List<TJX4List<TJX4Str>> : Array<Array<string>>> :');
  S := TJX4ListOfValues.Create;
  S.Add('TTT');
  S.Add('OOO');
  Demo.SLists.Add(S);
  S := TJX4ListOfValues.Create;
  S.AddRange(['XXX', 'YYY', 'ZZZ']);
  Demo.SLists.Add(S);
  Json := TJX4Object.ToJson(Demo, [joNullToEmpty]);
  ShowResult( TJX4Object.ToJson(Demo, [joNullToEmpty]) );

  // TJX4List<TJX4Dic<TJX4List<TPrimitives>>> : Array<Dictionary<string, Array<TPrimitives>>> :)
  Memo1.lines.add(sLineBreak);
  Memo1.lines.add('TJX4List<TJX4Dic<TJX4List<TPrimitives>>>  : Array<Dictionary<string, Array<TPrimitives>>>');

  var p1 := TJX4List<TPrimitive>.NewAdd(TPrimitive.Create);              // Create a 2 elements Primitives(TPrimitive) array
  p1.First.Str := 'Boolean1';                                            // Init the first item, which is also the last (which is also the first in this case)
  p1.First.Bool := True;

  var p2 := TJX4List<TPrimitive>.NewAdd(TPrimitive.Create);              // Create a second elements Primitives array
  p2[0].Str := 'Boolean2';
  p2[0].Bool := True;

  var d1 := TJX4Dic<TJX4List<TPrimitive>>.Create;                        // Create the dictionary ownning the 2 lists
  d1.Add('DicVal1', p1);                                                 // Add list1 as Dicval1
  d1.Add('DicVal2', p2);                                                 // Add list2 as Dicval2
  Demo.DicList.Add(d1);
  Demo.DicList.Add( d1.Clone<TJX4Dic<TJX4List<TPrimitive>>>);           // Adding the Dict and its clones to the main list
  ShowResult( TJX4Object.ToJson(Demo, [joNullToEmpty]) );
  Demo.Format();


  Demo.Free;
  SB.AppendLine(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  Memo1.Text := SB.ToString;

end;

end.
