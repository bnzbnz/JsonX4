unit uDemo06;

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

  TQuestion = class(TJX4Object)
    question: TValue;
    options:  TJX4ValList;
    answer:   TValue;
  end;

  TGame = class(TJX4Object)
    quiz: TJX4Dic<TJX4Dic<TQuestion>>;   // << Double dictionaries
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
begin
  // Random json found on the internet
  var GameStr :=
    '''
    {"quiz":{"sport":{"q1":{"question":"Which one is correct team name in NBA?","options":["New York Bulls",
    "Los Angeles Kings","Golden State Warriros","Huston Rocket"],"answer":"Huston Rocket"}},
    "maths":{"q1":{"question":"5 + 7 = ?","options":["10","11","12","13"],"answer":"12"},
    "q2":{"question":"12 - 8 = ?","options":["1","2","3","4"],"answer":"4"}}}}
    ''';

  // TJX3 = TJX3Object
  var Game := TJX4Object.FromJSON<TGame>(GameStr);         // Get the Object from Json
  Memo1.Text := TJX4Object.Format(  TJX4Object.ToJSON(Game) );     // Get the Json from the Object, and print the formated result

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Questions - Options :');
  for var LPk1 in Game.quiz do                             //Dump  Questions - Options
    for var LPk2 in LPk1.Value do
    begin
      Memo1.Lines.Add(LPk1.Key + ' - ' + LPk2.Value.question.AsString+' : ');
      for var LP in LPk2.Value.options do
        Memo1.Lines.Add('  ' + LP.AsString);
      Memo1.Lines.Add('==> Answer : ' + LPk2.Value.answer.AsString);
    end;

  Game.Free;                                               // Cleanup
end;

end.
