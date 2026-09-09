unit uDemo05;

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

  // You may nedd additional property control attributes :

  TDemo = class(TJX4Object)
    [TJX4Required]
    Str:     TValue;          // a value is required for serializisation

    [Default('22')]       // a defualt value to be used at deserialization, if the field is null
    Num1:    TValue;

    // name encoding : sometimes a Json Name may not be compatible with Delphi naming conventions
    // in this case there are three way to "encode" the json or Delphi name:
    // using & for Delphi property name
    &type: TValue;           //  for Json Name "type"

    // name Arribute:
    [TJX4Name('#href')]  // The json name mathcing the property HrefVar : {"#href":"Test Value"}
    HrefVar: TValue;

    // inline encoding:
    __23href2: TValue;       // name encoding :  __23href = #hef    ('_'+'_'+Hex('#')+'href')

    // multiples attributes :
    [TJX4Default(true)]
    [TJX4Name('NewMix')]
    Mix: TValue;
  end;


var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Demo, Demo3, JDemo: TDemo;
  JsonStr: string;
begin

  Demo := Nil;
  JDemo := Nil;
  Demo3 := Nil;
  Memo1.Lines.Clear;
  try

    Demo := TDemo.Create;
    Demo.Str := 'Need a Value';
    Demo.HrefVar := 'http://';

    Memo1.Lines.Add('JX4Default Attribute : Num1 default to 22 :');
    Memo1.Lines.Add(Demo.ToJSON([joNulltoEmpty]));

    Memo1.Lines.Add('');
    Memo1.Lines.Add('JX4Name Attribute, Name conversion :');
    JsonStr := '{"Str":"Needs a Value","#href":"http","Num1":22, "NewMix":true}';
    JDemo := TJX4Object.FromJSON<TDemo>(JsonStr, []);
    Memo1.Lines.Add('Deserialization: #href value is : ' + JDemo.HrefVar.AsString);
    JDemo.HrefVar :='ftp';
    Memo1.Lines.Add('Serialization: ' + JDemo.ToJSON([joNulltoEmpty]));

    Memo1.Lines.Add('');
    Demo.__23href2 := 'auto enc/decoding';          // Name encoding: start with '_' and special characters: '_'+Hex Value : # => _23
    Memo1.Lines.Add('Name encoding : ' +  JDemo.ToJSON([joNulltoEmpty]));

    //Cloning :
    Memo1.Lines.Add('');
    Demo3 := Demo.Clone<TDemo>;                     // new object Demo3 = Demo
    Memo1.Lines.Add('Clone : ' +  Demo3.ToJSON([joNulltoEmpty]));

    // Options flags:
    //  joNullToEmpty         : Remove null fields
    //  joNoException         : do not raise ecxceptions (in this case, the function call will be Nil or Empty)
    //  joRaiseOnMissingField : Raise an exception when json field is missing in the delphi object; (Debug)
    //  joStats               : Calc. stats (see Large demo)

    Memo1.Lines.Add('');
    Memo1.Lines.Add('JX4Required : NO exception: joNoException');
    Demo3.Str := Nil;                               // Demo.Str is null but required >> Exception;
    JsonStr := Demo3.ToJSON([joNoException]);       // Exception but not raised, the result will be empt Ni or "" depending of the functiony
    Memo1.Lines.Add((Format('Serialization Error : "%s"', [JSonStr])));
    Memo1.Lines.Add('JX4Required : RAISE exception');
    JsonStr := Demo3.ToJSON([]);                    // by default : raise exceptions

  finally
    Demo3.Free;
    JDemo.Free;
    Demo.Free;
  end;

end;

initialization

end.
