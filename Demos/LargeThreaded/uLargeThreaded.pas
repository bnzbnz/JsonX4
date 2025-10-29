unit uLargeThreaded;

interface



uses
    System.SysUtils
  , System.Types
  , System.UITypes
  , System.Classes
  , System.Variants
  , FMX.Types
  , FMX.Controls
  , FMX.Forms
  , FMX.Graphics
  , FMX.Dialogs
  , FMX.Memo.Types
  , FMX.Controls.Presentation
  , FMX.ScrollBox
  , FMX.Memo
  , FMX.StdCtrls
  , System.Generics.Defaults
  , System.Generics.Collections
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJX4List
  , uJX4Dict
  ;

type

  TForm4 = class( TForm )
    Button : TButton;
    Memo1 : TMemo;
    procedure ButtonClick( Sender : TObject );
    private
      { Private declarations }
    public
      { Public declarations }
  end;


  TPeople = class(TJX4Object)
    name: TValue;
    language: TValue;
    id: TValue;
    bio: TValue;
    version: TValue;
  end;

  TPeopleContainter  = class(TJX4Object)
    ctnr: TJX4List<TPeople>;
  end;

var
  Form4 : TForm4;

implementation

uses
    System.Diagnostics
  , Threading
  ;

{$R *.fmx}

procedure TForm4.ButtonClick( Sender : TObject );
var
  Task: ITask;
begin
  Task := TTask.Create(
    procedure
      var
        LJSize: Int64;
        LJsonStr : string;
        LJObj, LJObjClone, LJObjMerge: TPeopleContainter;
        LWatch: TStopWatch;
        procedure Log(Text: string);
        begin
          TThread.Synchronize(Nil,
            procedure begin
              if Assigned(Form4.Memo1) then Form4.Memo1.Lines.Add(Text);
            end
          );
        end;
    begin
      try
      try
        Log('Starting...');
        LJObj:= Nil;
        LJObjClone := Nil;
        LJObjMerge := Nil;
        LJSize := TJX4Object.LoadFromFile('Peoples.json', LJsonStr);
        Log( Format('==> Stream size: %n KB', [ (LJSize / 1024)]));

        LJsonStr := '{"ctnr":' + LJsonStr + '}';
        LWatch := TStopWatch.StartNew;
        Log('Convert Json String to JSX4 Objects (Deserialize):');
        LJObj := TJX4Object.FromJSON<TPeopleContainter>(LJsonStr, [ joRaiseOnAbort ] );
        Log(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

        LWatch := TStopWatch.StartNew;
        Log('JSX4 Object Cloning (by RTTI):');
        LJObjClone := LJObj.Clone<TPeopleContainter>([ joRaiseOnAbort ] );
        Log(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

        LWatch := TStopWatch.StartNew;
        Log('Revert JX4 Objects to Json String (Serialize)):');
        LJObjMerge := TPeopleContainter.Create;
        LJObjMerge.Merge(LJObjClone, [ joRaiseOnAbort ]);
        Log(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

        Log('Done... Peoples : '  + LJObj.ctnr.count.ToString);
      finally
        FreeAndNIl(LJObjMerge);
        FreeAndNIl(LJObjClone);
        FreeAndNIl(LJObj);
      end;
      except
        on Ex: TJX4ExceptionAborted do
        ShowMessage('Abort');
      end;
    end
  );
  Task.Start;
end;

end.
