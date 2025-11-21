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
  , uJX4RTTI
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJX4List
  , uJX4Dict
  , Threading
  , SyncObjs
  ;

type

  TJsonThread = class(TThread)
  protected
    Lock : TCriticalSection;
    procedure Execute; override;
  end;

  TForm4 = class( TForm )
    TaskBtn: TButton;
    Memo1 : TMemo;
    ThreadBtn: TButton;
    CancelBtn: TButton;
    procedure TaskBtnClick( Sender : TObject );
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThreadBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    private
      { Private declarations }
    public
      { Public declarations }
      TaskList: TList<ITask>;
      ThreadList: TList<TJsonThread>;
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
  , System.JSON
  ;

{$R *.fmx}

procedure TForm4.ThreadBtnClick(Sender: TObject);
begin
  for var j := 1 to 4 do
  begin
    var TJThread := TJsonThread.Create(True);
    ThreadList.Add(TJThread);
    TJThread.Start;
  end;
end;

procedure TForm4.TaskBtnClick( Sender : TObject );
var
  Task: ITask;
begin
  Task := TTask.Create(
    procedure
    var
      LJSize: Int64;
      LJsonStr : string;
      LJObj, LJObjClone, LJObjMerge: TPeopleContainter;
      LWatch, LWatchAll: TStopWatch;
      procedure Log(Text: string);
      begin
        Form4.Memo1.Lines.Add(Text); // ???
      end;
    begin
    try
      LJObj:= Nil;
      LJObjClone := Nil;
      LJObjMerge := Nil;
      try
        Log('Starting a New Task : ' + Task.Id.ToString );
        LWatch := TStopWatch.StartNew;
        LWatchAll := TStopWatch.StartNew;
          LJSize := TJX4Object.LoadFromFile('Peoples.json', LJsonStr);
          LJObj := TJX4Object.FromJSON<TPeopleContainter>('{"ctnr":' + LJsonStr + '}', [ joRaiseOnAbort ] );
        Log( Task.Id.ToString + ' FromJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatch := TStopWatch.StartNew;
          LJObjClone := LJObj.Clone<TPeopleContainter>( [ joRaiseOnAbort ] );
        Log( Task.Id.ToString + ' Clone:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatch := TStopWatch.StartNew;
          LJObjMerge := TPeopleContainter.Create;
          LJObjMerge.Merge(LJObjClone, [ jmoAdd, joRaiseOnAbort ]);
        Log( Task.Id.ToString + ' Merge:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatch := TStopWatch.StartNew;
            LJsonStr := LJObj.ToJson([ joNullToEmpty, joRaiseOnAbort ]);
        Log( Task.Id.ToString + ' ToJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatchAll.Stop;
        Log( Task.Id.ToString + ' Done Task ' + ' in ' + LWatchAll.ElapsedMilliseconds.ToString + ' ms');
        Log('Peoples : (' + Task.Id.ToString + ') : ' + LJObjClone.ctnr.Count.ToString);
      finally
        FreeAndNIl(LJObjMerge);
        FreeAndNIl(LJObjClone);
        FreeAndNIl(LJObj);
      end;
    except
      on Ex: TJX4ExceptionAborted do
        Log( Task.Id.ToString + ' Abort Task ');
    end;
  end
  );
  TaskList.Add(Task);
  Task.Start;
end;

{ TJsonThread }


procedure TJsonThread.Execute;
var
  LJSize: Int64;
  LJsonStr : string;
  LJObj, LJObjClone, LJObjMerge: TPeopleContainter;
  LWatch, LWatchAll: TStopWatch;
  procedure Log(Text: string);
  begin
    TThread.CurrentThread.Synchronize(TThread.Current, procedure begin Form4.Memo1.Lines.Add(Text); end);
  end;
begin
try
    LJObj:= Nil;
    LJObjClone := Nil;
    LJObjMerge := Nil;
    try
      Log('Starting a New Thread : ' + ThreadId.ToString);
        LJSize := TJX4Object.LoadFromFile('Peoples.json', LJsonStr);
      LWatchAll := TStopWatch.StartNew;
      LWatch := TStopWatch.StartNew;
        LJObj := TJX4Object.FromJSON<TPeopleContainter>('{"ctnr":' + LJsonStr + '}', [ joRaiseOnAbort ] );
      Log( ThreadId.ToString + ' FromJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      LWatch := TStopWatch.StartNew;
        LJObjClone := LJObj.Clone<TPeopleContainter>( [ joRaiseOnAbort ] );
        Log( ThreadId.ToString + ' Clone:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      LWatch := TStopWatch.StartNew;
        LJObjMerge := TPeopleContainter.Create;
        LJObjMerge.Merge(LJObjClone, [ jmoAdd, joRaiseOnAbort ]);
      Log( ThreadId.ToString + ' Merge:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      LWatch := TStopWatch.StartNew;
        LJsonStr := LJObj.ToJson([ joNullToEmpty, joRaiseOnAbort ]);
      Log( ThreadId.ToString + ' ToJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      Log(ThreadId.ToString + ' Thread Done : ' + LJObjMerge.ctnr.Count.toString);

      Log( ThreadId.ToString + ' Done Task ' + ' in ' + LWatchAll.ElapsedMilliseconds.ToString + ' ms');
      Log('Peoples : (' + ThreadId.ToString + ') : ' + LJObjClone.ctnr.Count.ToString );

    finally
      FreeAndNIl(LJObjMerge);
      FreeAndNIl(LJObjClone);
      FreeAndNIl(LJObj);
    end;
  except
    on Ex: TJX4ExceptionAborted do
      Log( ThreadId.ToString + ' Abort Thread ');
  end;
  Terminate;
end;

procedure TForm4.CancelBtnClick(Sender: TObject);
begin
  if TaskList.Count > 0 then
  begin
    for var I := TaskList.Count -1  downto 0 do TaskList[I].Cancel;
    try TTask.WaitForAll(TaskList.ToArray); except end;
    TaskList.Clear;
  end;
  for var Thread in ThreadList do Thread.Terminate;
  for var Thread in ThreadList do
  begin
    Thread.WaitFor;
    Thread.Free;
  end;
  ThreadList.Clear;
  Self.Memo1.Lines.Clear;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  TaskList := TList<ITask>.Create;
  ThreadList := TList<TJsonThread>.Create;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  CancelBtnClick(Self);
  TaskList.Free;
  ThreadList.Free;
end;

initialization
  xRTTIThreaded := True;
end.
