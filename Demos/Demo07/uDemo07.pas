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
    [TJX4Name('type'), TJX4Required]
    atype: TValue;
  end;

  TTopping = TBatter;

  TBatters = class(TJX4Object)
    batter: TJX4List<TBatter>;
  end;

  TDonut = class(TJX4Object)
    id: TValue;
    [TJX4Name('type')]
    &type: TValue;
    name: TValue;
    ppu: TValue;
    batters: TBatters;
    topping: TJX4List<TTopping>;
  end;

  TEx7 = class(TJX4Object)
    container: TJX4List<TDonut>;
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
  LWatch: TStopWatch;
  Ex7: TEx7;
  Ex7Clone: TEx7;
begin

  LWatch := TStopWatch.StartNew;
  EX7 := Nil;
  Ex7Clone := Nil;
  try

    var Json := '{"container":' + Memo1.Lines.Text + '}';     // << because the provided json is an array, we enclose it with a TJXObject container
    Ex7 := TJX4Obj.FromJSON<TEx7>(Json, [joRaiseOnMissingField, joRaiseOnException]);

    if assigned(Ex7) then Ex7Clone := Ex7.Clone<TEx7>;                        // for the fun we clone Ex7... :)

  finally
    if assigned(Ex7Clone) then  Memo1.Text := Ex7Clone.Format;
    Ex7Clone.Free;
    Ex7.Free;
  end;

  Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));

end;

end.
