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

  TDonut = class(TJX4Object)
    id: TValue;
    [TJX4Name('type')]
    &type: TValue;
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
  LWatch: TStopWatch;
  Ex7: TDonut;
  Donuts: TJX4List<TDonut>;
begin
  Button.Enabled := False;
  LWatch := TStopWatch.StartNew;
  EX7 := Nil;
  Donuts := Nil;
  try

    Donuts := TJX4Obj.FromJSON< TJX4List<TDonut> >(Memo1.Lines.Text, [joRaiseOnMissingField, joRaiseOnException]);
    Memo1.Text := Donuts.Format;

  finally
    Donuts.Free;
    Ex7.Free;
  end;

  Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));

end;

end.
