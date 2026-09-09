unit uDemo07;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJX4List
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

  TBatter = class(TJX4Object)
    id: TValue;
    [TJX4Name('type'), TJX4Required]                        // Name Attributs
    atype: TValue;
  end;

  TTopping = TBatter;

  TDonut = class(TJX4Object)
    id: TValue;
    &type: TValue;                                          // Delphi name encoding
    name: TValue;
    ppu: TValue;
    batters: TJX4List<TBatter>;
    topping: TJX4List<TTopping>;
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
  Str: string;
  LWatch: TStopWatch;
  Donuts: TJX4List<TDonut>;
begin
  Memo1.Lines.Clear;

  Str := '''
    [{"id":"0001","type":"donut","name":"Cake","ppu":0.55,"batters":{"batter":[{"id":"1001","type":"Regular"},{"id":"1002","type":"Chocolate"},{"id":"1003","type":"Blueberry"},{"id":"1004","type":"Devil's Food"}]},"topping":[{"id":"5001","type":"None"},{"id":"5002","type":"Glazed"},{"id":"5005","type":"Sugar"},{"id":"5007","type":"Powdered Sugar"},{"id":"5006","type":"Chocolate with Sprinkles"},{"id":"5003","type":"Chocolate"},{"id":"5004","type":"Maple"}]},{"id":"0002","type":"donut","name":"Raised","ppu":0.55,"batters":{"batter":[{"id":"1001","type":"Regular"}]},"topping":[{"id":"5001","type":"None"},{"id":"5002","type":"Glazed"},{"id":"5005","type":"Sugar"},{"id":"5003","type":"Chocolate"},{"id":"5004","type":"Maple"}]},{"id":"0003","type":"donut","name":"Old Fashioned","ppu":0.55,"batters":{"batter":[{"id":"1001","type":"Regular"},{"id":"1002","type":"Chocolate"}]},"topping":[{"id":"5001","type":"None"},{"id":"5002","type":"Glazed"},{"id":"5003","type":"Chocolate"},{"id":"5004","type":"Maple"}]}]
  ''' ;
  Memo1.Lines.add('Json String: ');
  Memo1.Lines.add(Str);

  LWatch := TStopWatch.StartNew;
  Donuts := Nil;
  try
  Memo1.Lines.add(sLineBreak + 'Json Object: ');
    Donuts := TJX4Obj.FromJSON< TJX4List<TDonut> >(Str, [joRaiseOnMissingField]);
    Memo1.Lines.Add(Donuts.Format);
  finally
    Donuts.Free;
  end;

  Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));
end;

end.
