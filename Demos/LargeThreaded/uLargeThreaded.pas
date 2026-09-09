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
    procedure Execute; override;
  end;

  TForm4 = class( TForm )
    TaskBtn: TButton;
    Memo1 : TMemo;
    ThreadBtn: TButton;
    CancelBtn: TButton;
    Button1: TButton;
    procedure TaskBtnClick( Sender : TObject );
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThreadBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    private
      { Private declarations }
    public
      { Public declarations }
      TaskList: TList<ITask>;
      ThreadList: TList<TJsonThread>;
      procedure MainLog(AText: string);
  end;

  TPeople = class(TJX4Object)
    name: TValue;
    language: TValue;
    id: TValue;
    bio: TValue;
    version: TValue;
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

procedure TForm4.MainLog(AText: string);
begin
  Memo1.Lines.Add(AText);
end;

procedure TForm4.TaskBtnClick( Sender : TObject );
var
  Task: ITask;
begin
  Task := TTask.Create(
    procedure
    var
      LJsonStr : string;
      LJObj, LJObjClone, LJObjMerge: TJX4List<TPeople>;
      LWatch, LWatchAll: TStopWatch;

      procedure Log(AText: string);
      begin
        TThread.Queue(nil,
          procedure
          begin
            Form4.Memo1.Lines.Add(AText);
          end);
      end;

    begin
      LJObj:= Nil;
      LJObjClone := Nil;
      LJObjMerge := Nil;
      try
        try
          var TaskId := TTask.CurrentTask.Id.ToString;
          Log('Starting New Task : ' + TaskId );

          TJX4Object.LoadFromFile('Peoples.json', LJsonStr);
          LWatchAll := TStopWatch.StartNew;

          LWatch := TStopWatch.StartNew;
            LJObj := TJX4Object.FromJSON< TJX4List<TPeople> >(LJsonStr, [ joRaiseOnAbort ] );
          Log( 'Task : ' + TaskId + ' FromJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

          LWatch := TStopWatch.StartNew;
            LJObjClone := LJObj.Clone< TJX4List<TPeople> >( [ joRaiseOnAbort ] );
         Log( 'Task : ' + TaskId + ' Clone:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

          LWatch := TStopWatch.StartNew;
            LJObjMerge := TJX4List<TPeople>.Create;
            LJObjMerge.Merge(LJObjClone, [ jmoAdd, joRaiseOnAbort ]);
          Log( 'Task : ' + TaskId + ' Merge:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

          LWatch := TStopWatch.StartNew;
            LJsonStr := LJObjMerge.ToJSON([ joNullToEmpty, joRaiseOnAbort ]);
          Log( 'Task : ' + TaskId + ' ToJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

          Log( 'Task : ' + TaskId + ' Done ' + ' in ' + LWatchAll.ElapsedMilliseconds.ToString + ' ms');
          Log( 'Task : ' + TaskId + ' Peoples : ' + LJObjClone.Count.ToString);

        finally
          FreeAndNIl(LJObjMerge);
          FreeAndNIl(LJObjClone);
          FreeAndNIl(LJObj);
        end;
      except
        on Ex: TJX4ExceptionAborted do
          Log( 'Task : ' + TTask.CurrentTask.Id.ToString + ' ABORTED ');
      end;
    end
  );
  TaskList.Add(Task);
  Task.Start;
end;

{ TJsonThread }

procedure TJsonThread.Execute;
var
  LJsonStr : string;
  LJObj, LJObjClone, LJObjMerge: TJX4List<TPeople>;
  LWatch, LWatchAll: TStopWatch;
  procedure Log(AText: string);
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        Form4.Memo1.Lines.Add(AText);
      end);
  end;
begin
try
    LJObj:= Nil;
    LJObjClone := Nil;
    LJObjMerge := Nil;
    try
      Log('Starting New Thread : ' + ThreadId.ToString);
        TJX4Object.LoadFromFile('Peoples.json', LJsonStr, TEncoding.UTF8);
      LWatchAll := TStopWatch.StartNew;
      LWatch := TStopWatch.StartNew;
        LJObj := TJX4Object.FromJSON< TJX4List<TPeople> >(LJsonStr, [ joRaiseOnAbort ] );
      Log( 'Thread : ' + ThreadId.ToString + ' FromJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

      LWatch := TStopWatch.StartNew;
        LJObjClone := LJObj.Clone< TJX4List<TPeople> >( [ joRaiseOnAbort ] );
        Log( 'Thread : ' + ThreadId.ToString + ' Clone:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

      LWatch := TStopWatch.StartNew;
        LJObjMerge := TJX4List<TPeople>.Create;
        LJObjMerge.Merge(LJObjClone, [ jmoAdd, joRaiseOnAbort ]);
      Log( 'Thread : ' + ThreadId.ToString  + ' Merge:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

      LWatch := TStopWatch.StartNew;
        LJsonStr := LJObj.ToJson([ joNullToEmpty, joRaiseOnAbort ]);
      Log(  'Thread : ' + ThreadId.ToString + ' ToJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');

      Log( Format('Thread : %s Done in %d ms, (ppl %d)', [ThreadId.ToString, LWatchAll.ElapsedMilliseconds, LJObjClone. Count ]));

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

procedure TForm4.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm4.CancelBtnClick(Sender: TObject);
begin
  if not Assigned(TaskList) and not Assigned(ThreadList) then Exit;
  if TaskList.Count > 0 then for var I := TaskList.Count -1  downto 0 do TaskList[I].Cancel;
  if ThreadList.Count > 0 then for var Thread in ThreadList do Thread.Terminate;
  if TaskList.Count > 0 then try TTask.WaitForAll(TaskList.ToArray); except end;
  for var Thread in ThreadList do
  begin
    Thread.WaitFor;
    Thread.Free;
  end;
  TaskList.Clear;
  ThreadList.Clear;
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

initialization;
end.
