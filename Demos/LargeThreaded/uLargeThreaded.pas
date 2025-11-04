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
  , Threading
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

  TvalueConst = class( TJX4Object )
    applicableForLocalizedAspectName : TValue;
    applicableForLocalizedAspectValues : TJX4ListOfValues;
  end;

  TvalueConstraint = class( TJX4Object )
    localizedValue : TValue;
    valueConstraints : TJX4List< TvalueConst >;
    applicableForLocalizedAspectName : TValue;
    applicableForLocalizedAspectValues : TJX4ListOfValues;
  end;

  TaspectValues = class( TJX4Object )
    localizedValue : TValue;
    valueConstraints : TJX4List< TvalueConstraint >;
  end;

  TaspectConstraint = class( TJX4Object )
    aspectDataType : TValue;
    itemToAspectCardinality : TValue;
    aspectMode : TValue;
    aspectRequired : TValue;
    aspectUsage : TValue;
    aspectEnabledForVariations : TValue;
    aspectApplicableTo : TJX4ListOfValues;
    aspectMaxLength : TValue;
    expectedRequiredByDate : TValue;
    aspectFormat : TValue;
  end;

  TcategoryAspectName = class( TJX4Object )
    categoryId : TValue;
    categoryName : TValue;
  end;

  TcategoryAspect = class( TJX4Object )
    localizedAspectName : TValue;
    aspectConstraint : TaspectConstraint;
    aspectValues : TJX4List< TaspectValues >;
  end;

  TcategoryAspects = class( TJX4Object )
    category : TcategoryAspectName;
    aspects : TJX4List< TcategoryAspect >;
  end;

  TfetchItemAspectsContentType = class( TJX4Object )
  public
    categoryTreeId : TValue;
    categoryTreeVersion : TValue;
    categoryAspects : TJX4List< TcategoryAspects >;
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
  ;

{$R *.fmx}


procedure TForm4.ThreadBtnClick(Sender: TObject);
begin
  var TJThread := TJsonThread.Create(True);
  ThreadList.Add(TJThread);
  TJThread.Start;
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
      LJObj, LJObjClone, LJObjMerge: TfetchItemAspectsContentType;
      LWatch: TStopWatch;
      procedure Log(Text: string);
      begin
        Form4.Memo1.Lines.Add(Text); // ???
      end;
    begin
    try
      try
        Log('Starting a New Task : ' + Task.Id.ToString );
          LJObj:= Nil;
          LJObjClone := Nil;
          LJObjMerge := Nil;
        LWatch := TStopWatch.StartNew;
          LJSize := TJX4Object.LoadFromFile('aspects100.json', LJsonStr);
          LJObj := TJX4Object.FromJSON<TfetchItemAspectsContentType>(LJsonStr, [ joRaiseOnAbort ] );
        Log( Task.Id.ToString + ' FromJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatch := TStopWatch.StartNew;
          LJObjClone := LJObj.Clone<TfetchItemAspectsContentType>( [ joRaiseOnAbort ] );
        Log( Task.Id.ToString + ' Clone:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatch := TStopWatch.StartNew;
          LJObjMerge := TfetchItemAspectsContentType.Create;
          LJObjMerge.Merge(LJObjClone, [ jmoAdd, joRaiseOnAbort ]);
        Log( Task.Id.ToString + ' Merge:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        LWatch := TStopWatch.StartNew;
            LJsonStr := LJObj.ToJson([ joNullToEmpty, joRaiseOnAbort ]);
        Log( Task.Id.ToString + ' ToJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
        Log('Done Task : ' + Task.Id.ToString);
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
  LJObj, LJObjClone, LJObjMerge: TfetchItemAspectsContentType;
  LWatch: TStopWatch;
  procedure Log(Text: string);
  begin
    TThread.CurrentThread.Synchronize(TThread.Current, procedure begin Form4.Memo1.Lines.Add(Text); end);
  end;
begin
  try
    try
      Log('Starting a New Thread : ' + ThreadId.ToString);
        LJObj:= Nil;
        LJObjClone := Nil;
        LJObjMerge := Nil;
      LWatch := TStopWatch.StartNew;
        LJSize := TJX4Object.LoadFromFile('aspects100.json', LJsonStr);
        LJObj := TJX4Object.FromJSON<TfetchItemAspectsContentType>(LJsonStr, [ joRaiseOnAbort ] );
      Log( ThreadId.ToString + ' FromJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      LWatch := TStopWatch.StartNew;
        LJObjClone := LJObj.Clone<TfetchItemAspectsContentType>( [ joRaiseOnAbort ] );
      Log( ThreadId.ToString + ' Clone:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      LWatch := TStopWatch.StartNew;
        LJObjMerge := TfetchItemAspectsContentType.Create;
        LJObjMerge.Merge(LJObjClone, [ jmoAdd, joRaiseOnAbort ]);
      Log( ThreadId.ToString + ' Merge:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      LWatch := TStopWatch.StartNew;
        LJsonStr := LJObj.ToJson([ joNullToEmpty, joRaiseOnAbort ]);
      Log( ThreadId.ToString + ' ToJSON:  ' + LWatch.ElapsedMilliseconds.ToString + ' ms');
      Log(ThreadId.ToString + ' Thread Done ');
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

end.
