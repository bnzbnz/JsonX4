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

  TDemo = class(TJX4Object)
    [TJX4Required]
    Str:     TValue;                     // A value is required when serializing (Exception)
    [TJX4Name('#href')]                  // a name of the matching json name
    HrefVar: TValue;                     // a name of the json value mapped to "Bool"
    [TJX4Default('22')]                  // a defualt value to be used at deserialization, if the field is null
    Num1:    TValue;
    __23href2: TValue;                   // name encoding :  __23href = #hef    ('_'+'_'+Hex('#')+'href')
    [TJX4Default('true')]                //                                       ^-- Header
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

    Memo1.Lines.Add('JX3Default Attribute : Num1 default to 22 :');
    Memo1.Lines.Add(Demo.ToJSON([joNulltoEmpty]));

    Memo1.Lines.Add('');
    Memo1.Lines.Add('JX3Name Attribute, Name conversion :');
    JsonStr := '{"Str":"Need a Value","#href":"http","Num1":22}';
    JDemo := TJX4Object.FromJSON<TDemo>(JsonStr);
    Memo1.Lines.Add('Deserialization: #href value is : ' + JDemo.HrefVar.AsString);
    JDemo.HrefVar :='ftp';
    Memo1.Lines.Add('Serialization: ' + JDemo.ToJSON([joNulltoEmpty]));

    Memo1.Lines.Add('');
    Demo.__23href2 := 'auto enc/dec oding';       // Name encoding: start by '_' and special characters: '_'+Hex Value : # => _23
    Memo1.Lines.Add('Name encoding : ' +  JDemo.ToJSON([joNulltoEmpty]));

    //Cloning :
    Memo1.Lines.Add('');
    Demo3 := Demo.Clone<TDemo>;
    Memo1.Lines.Add('Clone : ' +  Demo3.ToJSON([joNulltoEmpty]));

    // Options flags:
    //  joNullToEmpty         : Remove null fields
    //  joRaiseException      : Re-raise ecxceptions
    //  joRaiseOnMissingField : Raise an exception when json field is missing in the delphi object; (Debug)
    //  joStats               : Calc. stats (see Lage demo)

    Memo1.Lines.Add('');
    Memo1.Lines.Add('JX3Required exception');
    Demo3.Str := Nil;
    //Demo.Str is null but required >> Exception;
    JsonStr := Demo3.ToJSON([joRaiseException]);      // This flag will re-raise internal exceptions

  finally
    Demo3.Free;
    JDemo.Free;
    Demo.Free;
  end;

end;

end.
