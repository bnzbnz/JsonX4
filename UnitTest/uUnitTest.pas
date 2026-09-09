unit uUnitTest;

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
const
  CBoolToValidation: array[Boolean] of string = ('**** INVALID ****','Valid');
type

  TForm4 = class( TForm )
    Button : TButton;
    Memo1 : TMemo;
    procedure ButtonClick( Sender : TObject );
    private
      { Private declarations }
    public
      { Public declarations }
    procedure ProcessJSON(JsonStr:string);
    procedure ShowResults(
                Title: string;
                From: string;
                Op: string;
                Res: string;
                EleA: TObject;
                EleD: TObject;
                EleU: TObject;
                UT: string = ''
//                Comment: string = ''
              );
    procedure ShowResultOptimized<T:class, constructor>(JSonStr: string; Res: string = '');
  end;

  TUnitTestType = class( TJX4Object )
    UTName : TValue;
    UTName2 : TValue;
    constructor Create(Value: TValue); overload;
  end;
  UT = class(TUnitTestType);

  TTESTArr = TJX4ListOfValues;

  TUTEST = class(TJX4Object)
    aaa: TValue;
    zzz: TValue;
    bbb: TJX4ListOfValues;
    ccc: TJX4DictOfValues;
    ddd: TJX4Dict<TUnitTestType>;
    eee: TJX4List<TUnitTestType>;
    procedure Clear;
  end;

  TTNull = class(TJX4Object)
    zzz: TValue;
  end;
  TTestB = class(TJX4Object)
    aaa: TValue;
    ddd: TJX4DictOfValues;
  end;
  TTestC = class(TJX4Object)
    aaa: TValue;
    ddd: TJX4Dict<TTNull>;
  end;

var
  Form4 : TForm4;

implementation

uses
  System.Diagnostics
  , uJX4YAML
  , uJX4RTTI
  ;

{$R *.fmx}

{ TUnitTestType }

constructor TUnitTestType.Create(Value: TValue);
begin
  Inherited Create;
  Self.UTName := Value;
end;

{ TForm4 }

procedure TForm4.ProcessJSON(JsonStr: string);
begin

  if not TJX4Object.Validate(JsonStr) then
  begin
     Memo1.Lines.Add('INVALID JSON : ' + JsonStr );
     Exit;
  end;
  var UT := TJX4Object.FromJSON<TUTEST>(JsonStr);
  Memo1.Lines.Add('==> Raw: ' + JsonStr);
  Memo1.Lines.Add('Serialized: ' + UT.ToJSON([]));
  Memo1.Lines.Add('Optimized : ' + UT.ToJSON([joNullToEmpty]));
  Memo1.Lines.Add('');
  UT.Free;
end;

procedure TForm4.ShowResults(
            Title: string;
            From: string;
            Op: string;
            Res: string;
            EleA: TObject;
            EleD: TObject;
            EleU: TObject;
            UT: string = ''
            // Comment: string = ''
          );
begin
  Form4.Memo1.Lines.Add(Title);
  Form4.Memo1.Lines.Add('  Target:');
  Form4.Memo1.Lines.Add('     '+ From);
  Form4.Memo1.Lines.Add('  Source:');
  Form4.Memo1.Lines.Add('     '+ Op);
  Form4.Memo1.Lines.Add('  Result:');
  Form4.Memo1.Lines.Add('     '+ Res);
  if Assigned(EleA) and (EleA is TList<string>) then Memo1.Lines.Add('  Elements Added   :'   + TList<string>(EleA).Count.ToString);
  if Assigned(EleD) and (EleD is TList<string>) then Memo1.Lines.Add('  Elements Deleted :'   + TList<string>(EleD).Count.ToString);
  if Assigned(EleU) and (EleD is TList<string>) then Memo1.Lines.Add('  Elements Updated :'   + TList<string>(EleU).Count.ToString);
  if Res.ToUpper.Trim = UT.ToUpper.Trim then
    Form4.Memo1.Lines.add('PASS')
  else
    if Res.Trim.IsEmpty then
      Form4.Memo1.Lines.add('Not Validated')
    else
      Memo1.Lines.add('FAIL');
  // if not Comment.IsEmpty then Memo1.Lines.add(Comment);
  Form4.Memo1.Lines.add('');
end ;

procedure TForm4.ShowResultOptimized<T>(JsonStr: string; Res: string = '');
var
  A: T;
begin
  Form4.Memo1.Lines.Add('From: ' + JsonStr);
  A := TJX4OBject.FromJSON<T>(JsonStr, [joNoException]);
  if not assigned(A) then
  begin
     Memo1.Lines.Add('Invalid JSON (FromJSON): ' + JsonStr);
     Exit;
  end;
  JsonStr := TJX4OBject(A).ToJSON([joNoException]);
  if JsonStr.IsEmpty then
  begin
     Memo1.Lines.Add('Invalid JSON (ToJSON Raw) : ' + JsonStr);
     Exit;
  end;
  Memo1.Lines.Add('Unfold: ' + JsonStr);
  JsonStr := TJX4OBject(A).ToJSON([joNullToEmpty, joNoException]);
  if JsonStr.IsEmpty then
  begin
     Memo1.Lines.Add('Invalid JSON (ToJSON joNullToEmpty) : ' + JsonStr);
     Exit;
  end;
  Memo1.Lines.Add('Optimize: ' + JsonStr);
  if JsonStr.ToLower.Trim = Res.ToLower.Trim then
      Memo1.Lines.add('PASS')
  else
    Memo1.Lines.add('FAIL');
  Memo1.Lines.add('');
  A.Free;
end;

procedure TForm4.ButtonClick( Sender : TObject );
var
  DictOfValue1, DictOfValue2: TJX4DictOfValues;
  MasterDictObj, Dict1, Dict2, Dict3: TJX4Dict<TUnitTestType>;
begin
  Memo1.Lines.Clear;

  // DEV

  //TJX4Dict<T>

  Self.Memo1.Lines.Add('>>> ------------------------- : TJX4Dict<T>' + sLineBreak);

  ShowResultOptimized<TUTEST>(' {"ddd":{"1":{},"2":null }} ','{}');
  ShowResultOptimized<TUTEST>(' {"ddd":{"1":{"UTName":10},"2":{"UTName2":20} }} ',' {"ddd":{"2":{"UTName2":20},"1":{"UTName":10}}} ');
  ShowResultOptimized<TUTEST>(' {"ddd":{"1":{"UTName":10},"2":{"UTName2":null} }} ', ' {"ddd":{"1":{"UTName":10}}} ');
  ShowResultOptimized<TUTEST>(' {"ccc":{ "1":null, "2":22.2 }} ', ' {"ccc":{"2":22.2}} ');
  ShowResultOptimized<TUTEST>(' {"ccc":{ "1":11, "2":22 }} ', ' {"ccc":{"2":22,"1":11}} ');

  //TJX4DictOfValues

   Self.Memo1.Lines.Add('>>> ------------------------- : TJX4DictOfValues' + sLineBreak);

  ShowResultOptimized<TUTEST>(' {"ccc":{ "1":null, "2":22.2 }} ', ' {"ccc":{"2":22.2}} ');
  ShowResultOptimized<TUTEST>(' {"ccc":{ "1":11, "2":22 }} ', ' {"ccc":{"2":22,"1":11}} ');
  ShowResultOptimized<TUTEST>(' {"ccc":{ "1":null, "2":null }} ', ' {} ');

  // TJX4List<T>

  Self.Memo1.Lines.Add('>>> ------------------------- : TJX4List<T>' + sLineBreak);

  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 01", "bbb":[null],"eee":[], "ddd":{}} ', ' {"aaa":"a TValue 01"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 01", "bbb":[1,2,3,4],"eee":[{"UTName":null}, {"UTName":10} ], "ddd":{}} ', ' {"aaa":"a TValue 01","bbb":[1,2,3,4],"eee":[{"UTName":10}]} ');
  ShowResultOptimized<TUTEST>(' {"eee":[ {"UTName":5, "UTName2":6}, {"UTName":10, "UTName2":11} ] } ', ' {"eee":[{"UTName":5,"UTName2":6},{"UTName":10,"UTName2":11}]} ');
  ShowResultOptimized<TTESTArr>(' [null, null] ', ' [] ');
  ShowResultOptimized<TTESTArr>(' [] ', ' [] ');

  // TJX4ListOfValues

  Self.Memo1.Lines.Add('>>> ------------------------- : TJX4ListOfValues' + sLineBreak);

  ShowResultOptimized<TTESTArr>(' [1,2,null,4,null] ', '[1,2,4]  ');
  ShowResultOptimized<TTESTArr>(' [1,2,3,4,5] ', ' [1,2,3,4,5] ');
  ShowResultOptimized<TTESTArr>(' [null] ', ' [] ');
  ShowResultOptimized<TTESTArr>(' [] ',' [] ' );

   // TJX4Object

  Self.Memo1.Lines.Add('>>> ------------------------- : TJX4Object' + sLineBreak);

  ShowResultOptimized<TUTEST>(' {} ', ' {} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 01", "zzz":null, "bbb":[]} ', ' {"aaa":"a TValue 01"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 02", "zzz":null, "bbb":[1,2]} ', ' {"aaa":"a TValue 02","bbb":[1,2]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 03", "zzz":null, "bbb":[1,null]} ', ' {"aaa":"a TValue 03","bbb":[1]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 04", "zzz":null, "bbb":[null,null]}', ' {"aaa":"a TValue 04"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 05", "zzz":null, "eee":[ {"UTName":1, "Unknown":2}]} ', ' {"aaa":"a TValue 05","eee":[{"UTName":1}]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 06", "zzz":null, "eee":[ {"UTName":null}]} ', ' {"aaa":"a TValue 06"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 07", "eee":[ {"UTName":null} ], "ccc":{}} ', ' {"aaa":"a TValue 07"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 08", "zzz":"ZZZ Value", "eee":[ {"UTName":null} ], "ccc":{"1":1,"2":2}} ', ' {"aaa":"a TValue 08","zzz":"ZZZ Value","ccc":{"2":2,"1":1}} ' );
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 09", "zzz":22.2, "bbb":[1,2,3,4], "eee":[ {"UTName":null}, {"UTName2":null} ], "ddd":{}} ', ' {"aaa":"a TValue 09","zzz":22.2,"bbb":[1,2,3,4]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 10", "zzz":22, "bbb":[1,2,3,4], "eee":[ {"UTName":{"UTName":5, "UTName2":10} } ], "ddd":{}} ', ' {"aaa":"a TValue 10","zzz":22,"bbb":[1,2,3,4],"eee":[{"UTName":5}]} ');

  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 01", "bbb":[1,2,3,4], "eee":[ {"UTName":null} ], "ddd":{}} ', ' {"aaa":"a TValue 01","bbb":[1,2,3,4]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 02", "eee":[ {"UTName":null} ], "ccc":{"1":1,"2":2}} ', ' {"aaa":"a TValue 02","ccc":{"2":2,"1":1}} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 03", "eee":[ {"UTName":null} ], "ccc":{}} ', ' {"aaa":"a TValue 03"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 04", "eee":[ {"UTName":null}]} ', ' {"aaa":"a TValue 04"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 05", "eee":[ {"UTName":1, "Unknown":2}]} ', ' {"aaa":"a TValue 05","eee":[{"UTName":1}]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 06", "bbb":[null,null]} ', ' {"aaa":"a TValue 06"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 07", "bbb":[1,null]} ', ' {"aaa":"a TValue 07","bbb":[1]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 08", "bbb":[1,2]} ', ' {"aaa":"a TValue 08","bbb":[1,2]} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 09", "bbb":[]} ', ' {"aaa":"a TValue 09"} ');
  ShowResultOptimized<TUTEST>(' {"aaa":"a TValue 10"}', ' {"aaa":"a TValue 10"} ');


  var MasterDictOfValues := TJX4DictOfValues.NewAddRange(['1','2','3','4'], ['a','b','c','d']);
  MasterDictObj := TJX4Dict<TUnitTestType>.NewAddRange(['1','2','3','4'], [UT.Create('UT1'), UT.Create('UT2'), UT.Create('UT3'), UT.Create('UT4')]);

  try
    Self.Memo1.Lines.Add('>>> ------------------------- : TJX4Dict<T>: Add, Update Purge TJX4Dict<T>' + sLineBreak);

    // TJX4Dict<T>: Add, Update Purge TJX4Dict<T>
    Dict1 := Nil; Dict2 := Nil;
    try
      Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
      Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['1', '2'],[Nil,Nil]); // (['1','Add2','2','5'], [TUnitTestType.Create('NewUTT1'), TUnitTestType.Create('NewUTT2'), Nil, Nil]);
      Dict1.Merge(Dict2, [jmoStat, jmoAdd, jmoUpdate, jmoPurge]);       // Add : Add1, Add2, '5'(Nil);  Update: '2' to Nil  ; Purge: '1', '3', '4'

      ShowResults(
        'TJX4Dict<T>: jmoAdd, jmoUpdate, jmoPurge "TJX4Dict<T>" by Cloning (Default) ',
        MasterDictObj.ToJSON,
        'jmoAdd + jmoUpdate + jmoPurge: ' + Dict2.ToJSON,
        Dict1.ToJSON,
        Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
        ' {"2":{null},"1":{null}} '
      );
    finally
      Dict1.Free;
      Dict2.Free;
    end;

{$REGION 'TJX4DictOfValues'}

    {$REGION 'TJX4DictOfValues Clone'}
    try
      DictOfValue1 := TJX4DictOfValues.NewAddRange(['1','2','3','4'], ['a','b','c','d']);
      DictOfValue2 := DictOfValue1.Clone;
      ShowResults(
            'TJX4DictOfValue: Clone : ',
            DictOfValue1.ToJSON,
            'Clone ',
            DictOfValue2.ToJSON,
            DictOfValue2.EleAdded,
            DictOfValue2.EleDeleted,
            DictOfValue2.EleUpdated,
            ' {"2":"b","4":"d","1":"a","3":"c"} '
        );
    finally
      DictOfValue1.Free;
      DictOfValue2.Free;
    end;

    {$ENDREGION 'TJX4DictOfValues Clone'}

    {$REGION 'TJX4DictOfValues jmoDelete'}

      // TJX4DictOfValues: jmoDelete - Dict<V> Keys Only
      DictOfValue1 := MasterDictOfValues.Clone;
      Dict1 := TJX4Dict<TUnitTestType>.NewAddRange(['3','4'], [Nil, Nil]); // We dont care of the Values
      try
        DictOfValue1.Merge<TUnitTestType>(Dict1, [jmoStat, jmoDelete]);
        ShowResults(
          'DictOfValue Merge jmoDelete "TJX4Dict<V> : Keys Only" :',
          MasterDictOfValues.ToJSON,
          'Delete : ' + DictOfValue2.ToJSON,
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"2":"b","1":"a"} '
        );
      finally
        Dict1.Free;
        DictOfValue1.Free;
      end;

      // DictOfValues: jmoDelete - DictOfValues by Keys
      DictOfValue1 := MasterDictOfValues.Clone;
      DictOfValue2 := TJX4DictOfValues.NewAddRange(['3','4'], [nil, 'OrAnyOtherValue']); // Any Dummy values
      DictOfValue1.Merge(DictOfValue2, [jmoStat, jmoDelete]);

      ShowResults(
        'DictOfValue Merge jmoDelete "TJX4DictOfValues by Keys" :',
        MasterDictOfValues.ToJSON,
        'Delete : ' + DictOfValue2.ToJSON,
        DictOfValue1.ToJSON,
        DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
        '{"2":"b","1":"a"} '
      );
      DictOfValue2.Free;
      DictOfValue1.Free;

      // DictOfValues: jmoDelete - Array of string
      DictOfValue1 := MasterDictOfValues.Clone;
      DictOfValue1.Merge(['3','4'], [jmoStat, jmoDelete]);

      ShowResults(
        'DictOfValue jmoDelete Merge "array of string" :',
        MasterDictOfValues.ToJSON,
        'Delete : ' + '[''3'',''4'']',
        DictOfValue1.ToJSON,
        DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
        ' {"2":"b","1":"a"} '
      );
      DictOfValue1.Free;

      // DictOfValues: jmoDelete - TArray<string>
      DictOfValue1 := MasterDictOfValues.Clone;
      var LArr : TArray<string> := ['1','2'];
      DictOfValue1.Merge(LArr, [jmoStat, jmoDelete]);

      ShowResults(
        'DictOfValue jmoDelete Merge "TArray<string>" :',
        MasterDictOfValues.ToJSON,
        'Delete : ' + '[''3'',''4'']',
        DictOfValue1.ToJSON,
        DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
        ' {"4":"d","3":"c"} '
      );
      DictOfValue1.Free;

    {$ENDREGION 'TJX4DictOfValues jmoDelete'}

    {$REGION 'TJX4DictOfValues Purge'}

      // DictOfValues: jmoPurge by TJX4DictOfValue
      DictOfValue1 := MasterDictOfValues.Clone;
      DictOfValue2 := TJX4DictOfValues.NewAddRange(['1','2','3'], ['', '', '']);
      try
        DictOfValue1.Merge(DictOfValue2, [jmoStat, jmoPurge]);
        ShowResults(
          'DictOfValues: jmoPurge by "TJX4DictOfValue" :',
          MasterDictOfValues.ToJSON,
          'Purge : ' + DictOfValue2.ToJSON,
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"2":"b","1":"a","3":"c"} '
        );
      finally
        DictOfValue2.Free;
        DictOfValue1.Free;
      end;

      // DictOfValues: jmoPurge by TJX4Dict<V> Keys Only
      DictOfValue1 := MasterDictOfValues.Clone;
      try
        Dict1 := TJX4Dict<TUnitTestType>.NewAddRange(['3','4'], [Nil, Nil]); // We dont care of the Values
        DictOfValue1.Merge<TUnitTestType>(Dict1, [jmoStat, jmoPurge]);
        ShowResults(
          'DictOfValues: jmoPurge by "TJX4Dict<V>" Keys Only :',
          MasterDictOfValues.ToJSON,
          'Purge : ' + Dict1.ToJSON,
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"4":"d","3":"c"} '
        );
      finally
        Dict1.Free;
        DictOfValue1.Free;
      end;

      // DictOfValues: jmoPurge by array of string
      DictOfValue1 := MasterDictOfValues.Clone;
      try
        DictOfValue1.Merge(['3','4'], [jmoStat, jmoPurge]);
        ShowResults(
          'DictOfValues: jmoPurge by "array of string" :',
          MasterDictOfValues.ToJSON,
          'Purge : ' + '[''3'',''4'']',
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"4":"d","3":"c"} '
        );
      finally
        DictOfValue1.Free;
      end;

      // DictOfValues: jmoPurge by TArray<string>
      DictOfValue1 := MasterDictOfValues.Clone;
      try
        LArr := ['1','2'];
        DictOfValue1.Merge(LArr, [jmoStat, jmoPurge]);

        ShowResults(
          'DictOfValues: jmoPurge by "TArray<string>" :',
          MasterDictOfValues.ToJSON,
          'Purge : ' + '[''3'',''4'']',
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"2":"b","1":"a"} '
        );
      finally
        DictOfValue1.Free;
      end;

    {$ENDREGION 'TJX4DictOfValues Purge'}

    {$REGION 'TJX4DictOfValues Add'}

      /// DictOfValues: jmoAdd by TJX4DictOfValue
      try
        DictOfValue1 := MasterDictOfValues.Clone;
        DictOfValue2 := TJX4DictOfValues.NewAddRange(['5','6'], ['e', 'f']);
        DictOfValue1.Merge(DictOfValue2, [jmoStat, jmoAdd]);
        ShowResults(
           'TJX4DictOfValue: jmoAdd by "TJX4DictOfValue" :',
          MasterDictOfValues.ToJSON,
          'jmoAdd : ' + DictOfValue2.ToJSON,
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"2":"b","5":"e","1":"a","3":"c","4":"d","6":"f"} '
        );
      finally
        DictOfValue2.Free;
        DictOfValue1.Free;
      end;

    {$ENDREGION 'TJX4DictOfValues Add'}

    {$REGION 'TJX4DictOfValues Update'}

      /// DictOfValues: Update by TJX4DictOfValue
      try
        DictOfValue1 := MasterDictOfValues.Clone;
        DictOfValue2 := TJX4DictOfValues.NewAddRange(['1','2','10'], ['111', '222', '000']);
        DictOfValue1.Merge(DictOfValue2, [jmoStat, jmoUpdate]);
        ShowResults(
           'TJX4DictOfValue: jmoUpdate by "TJX4DictOfValue" :',
          MasterDictOfValues.ToJSON,
          'jmoUpdate : ' + DictOfValue2.ToJSON,
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"2":"222","4":"d","1":"111","3":"c"} '
        );
      finally
        DictOfValue2.Free;
        DictOfValue1.Free;
      end;

    {$ENDREGION 'TJX4DictOfValues Update'}

    {$REGION 'TJX4DictOfValues Multi'}

    // TJX4DictOfValues: Add + Update by TJX4DictOfValue
    try
      DictOfValue1 := MasterDictOfValues.Clone;
      DictOfValue2 := TJX4DictOfValues.NewAddRange(['1','2','5','6'], ['V1', 'V2', 'V5', 'V6']);
      DictOfValue1.Merge(DictOfValue2, [jmoStat, jmoAdd, jmoUpdate]);
      ShowResults(
        'TJX4DictOfValue: jmoAdd + jmoUpdate by TJX4DictOfValues :',
        MasterDictOfValues.ToJSON,
        'jmoAdd + jmoUpdate: ' + DictOfValue2.ToJSON,
        DictOfValue1.ToJSON,
        DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
        ' {"2":"V2","5":"V5","1":"V1","3":"c","4":"d","6":"V6"} '
      );
    finally
      FreeAndNil(DictOfValue2);
      FreeAndNil(DictOfValue1);
    end;

    {$ENDREGION 'TJX4DictOfValues Multi'}

{$ENDREGION 'TJX4DictOfValue'}

{$REGION 'TJX4Dict<T>'}

    {$REGION 'TJX4Dict<T> Clone'}
    Dict1 := Nil; Dict2 := Nil;
    try
      Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
      Dict2 := Dict1.Clone< TJX4Dict<TUnitTestType> >;
      ShowResults(
          'Dict<T> Clone : ',
          Dict1.ToJSON,
          'Clone',
          Dict2.ToJSON,
          Dict2.EleAdded, Dict2.EleDeleted, Dict2.EleUpdated,
          ' {"2":{"UTName":"UT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"1":{"UTName":"UT1","UTName2":null},"3":{"UTName":"UT3","UTName2":null}} '
      );
    finally
      Dict2.Free;
      Dict1.Free;
    end;
    {$ENDREGION}

    {$REGION 'TJX4Dict<T> jmoDelete'}

      //  TJX4Dict<T> jmoDelete - TJX4Dict<T> Keys Only
      Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['1'], [Nil]);
        Dict1.Merge(Dict2, [jmoStat, jmoDelete]);
        ShowResults(
          'Dict<T> jmoDelete "TJX4Dict<T>" ',
          MasterDictObj.ToJSON,
          'Delete : ' + Dict2.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,  // Stats...
          ' {"2":{"UTName":"UT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"1":{"UTName":"UT1","UTName2":null},"3":{"UTName":"UT3","UTName2":null}} '
        );
      finally
        FreeAndNil(Dict1);
        FreeAndNil(Dict2);
      end;

      //  TJX4Dict<T> jmoDelete - TJX4DictOfValues<T> By Keys
      Dict1 := Nil; DictOfValue1 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        DictOfValue1 := TJX4DictOfValues.NewAddRange(['1'], ['111']);
        Dict1.Merge(DictOfValue1, [jmoStat, jmoDelete]);
        ShowResults(
          'Dict<T> jmoDelete "TJX4DictOfValues by Keys" ',
          MasterDictObj.ToJSON,
          'Delete : ' + DictOfValue1.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,  // Stats...
          ' {"2":{"UTName":"UT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"3":{"UTName":"UT3","UTName2":null}}'
        );
      finally
        FreeAndNil(Dict1);
        FreeAndNil(DictOfValue1);
      end;

      //  TJX4Dict<T> jmoDelete - Array of string
      Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['1'], [Nil]);
        Dict1.Merge(Dict2, [jmoStat, jmoDelete]);
      ShowResults(
        'TJX4Dict<T> jmoDelete Merge "array of string" :',
        MasterDictObj.ToJSON,
        'Delete : ' + '[''2'',''3'']', // 3 does not exists
        Dict1.ToJSON,
        Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
        ' {"2":{"UTName":"UT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"3":{"UTName":"UT3","UTName2":null}} '
      );
      finally
        FreeAndNil(Dict2);
        FreeAndNil(Dict1);
      end;

      //  TJX4Dict<T> jmoDelete - TArray<string>
      Dict1 := Nil;
      try
       Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        var Arr : TArray<string> := ['2'];
        Dict1.Merge(Arr, [jmoStat, jmoDelete]);
      ShowResults(
        'TJX4Dict<T> jmoDelete Merge "TArray<string>" :',
        MasterDictObj.ToJSON,
        'Delete : ' + '[''2'',''3'']', // 3 does not exists
        Dict1.ToJSON,
        Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
        ' {"4":{"UTName":"UT4","UTName2":null},"1":{"UTName":"UT1","UTName2":null},"3":{"UTName":"UT3","UTName2":null}} '
      );
      finally
        FreeAndNil(Dict2);
        FreeAndNil(Dict1);
      end;
    {$ENDREGION}

    {$REGION 'TJX4Dict<T> jmoAdd'}

      //  TJX4Dict<T> jmoAdd - TJX4Dict<T>
      Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['8'], [TUnitTestType.Create]);
        Dict1.Merge(Dict2, [jmoStat, jmoAdd]);
        ShowResults(
          'Dict<T> jmoAdd "TJX4Dict<T>" ',
          MasterDictObj.ToJSON,
          'Add : ' + Dict2.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated, // jmoStats
          ' {"2":{"UTName":"UT2","UTName2":null},"1":{"UTName":"UT1","UTName2":null},"8":{"UTName":null,"UTName2":null},"3":{"UTName":"UT3","UTName2":null},"4":{"UTName":"UT4","UTName2":null}} '
        );
      finally
        FreeAndNil(Dict1);
        FreeAndNil(Dict2);
      end;
    {$ENDREGION}

    {$REGION 'TJX4Dict<T> Purge'}

      // TJX4Dict<T> jmoPrurge - TJX4Dict<T>
      Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict< TUnitTestType >.NewAddRange(['1'], [Nil]);
        Dict1.Merge(Dict2, [jmoStat, jmoPurge]);
        ShowResults(
          'TJX4Dict<T>: jmoPurge by "TJX4Dict<T>" :',
          MasterDictObj.ToJSON,
          'Purge : ' + Dict2.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"1":{"UTName":"UT1","UTName2":null}} '
        );
      finally
        Dict2.Free;
        Dict1.Free;
      end;

      // TJX4Dict<T>: jmoPurge by "TJX4DictOfValue" Keys Only
      Dict1 := Nil; DictOfValue1 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        DictOfValue1 := TJX4DictOfValues.NewAddRange(['3', '4'], ['X', 'Y']);
        Dict1.Merge(DictOfValue1, [jmoStat, jmoPurge]);
        ShowResults(
          'TJX4Dict<T>: jmoPurge by "TJX4DictOfValue" Keys Only :',
          MasterDictOfValues.ToJSON,
          'Purge : ' + Dict1.ToJSON,
          DictOfValue1.ToJSON,
          DictOfValue1.EleAdded, DictOfValue1.EleDeleted, DictOfValue1.EleUpdated,
          ' {"4":"Y","3":"X"} '
        );
      finally
        Dict1.Free;
        DictOfValue1.Free;
      end;

      // TJX4Dict<T>: jmoPurge by "array of string"
      Dict1 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict1.Merge(['3','4','5'], [jmoStat, jmoPurge]);
        ShowResults(
          'TJX4Dict<T>: jmoPurge by "array of string" :',
          MasterDictObj.ToJSON,
          'Purge : ' + '[''3'',''4'',''5'']',
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"4":{"UTName":"UT4","UTName2":null},"3":{"UTName":"UT3","UTName2":null}} '
        );
      finally
        Dict1.Free;
      end;

      // TJX4Dict<T>: jmoPurge by "TArray<string>"
      Dict1 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        var Arr : TArray<string> := ['1','3','4'];
        Dict1.Merge(Arr, [jmoStat, jmoPurge]);
        ShowResults(
          'TJX4Dict<T>: jmoPurge by "TArray<string>" :',
          MasterDictObj.ToJSON,
          'Purge : ' + '[''1'',''3'',''4'']',
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"4":{"UTName":"UT4","UTName2":null},"1":{"UTName":"UT1","UTName2":null},"3":{"UTName":"UT3","UTName2":null}} '
        );
      finally
        Dict1.Free;
      end;

    {$ENDREGION 'TJX4Dict<T> Purge'}

    {$REGION 'TJX4Dict<T> Update'}

      // TJX4Dict<T>: Update by TJX4Dict<T> (Cloning Dict2 Objects by default)
      Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['1','3'], [TUnitTestType.Create('NewUTT1'), TUnitTestType.Create('NewUTT2')]);
        Dict1.Merge(Dict2, [jmoStat, jmoUpdate]);
        ShowResults(
          'TJX4Dict<T>: jmoUpdate "TJX4Dict<T>" by Cloning (Default):',
          MasterDictObj.ToJSON,
          'jmoUpdate : ' + Dict2.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"2":{"UTName":"UT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"1":{"UTName":"NewUTT1","UTName2":null},"3":{"UTName":"NewUTT2","UTName2":null}} '
          // 'Source is now (Cloning): ' + Dict2.ToJSON
        );
      finally
        Dict1.Free;
        Dict2.Free;
      end;

      // TJX4Dict<T>: Update by TJX4Dict<T> (Moving Dict2 Objects to Dict1, faster but Dict2 will be null)
      Dict1 := Nil; Dict2 := Nil; Dict3 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['1','3'], [TUnitTestType.Create('NewUT1'), TUnitTestType.Create('NewUT2')]);
        Dict3 := Dict2.Clone< TJX4Dict<TUnitTestType> >;  // For Display Only
        Dict1.Merge(Dict2, [jmoStat, jmoUpdate, jmoByMoving]);
        ShowResults(
          'TJX4Dict<T>: jmoUpdate "TJX4Dict<T>" by Moving :',
          MasterDictObj.ToJSON,
          'jmoUpdate : ' + Dict3.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"2":{"UTName":"UT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"1":{"UTName":"NewUT1","UTName2":null},"3":{"UTName":"NewUT2","UTName2":null}} '
        );
      finally
        Dict1.Free;
        Dict2.Free;
        Dict3.Free;
      end;

    {$ENDREGION 'TJX4Dict<T> Update'}

    {$REGION 'TJX4Dict<T> Add'}

    // TJX4Dict<T>: add TJX4Dict<T> Cloning
    Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['Add1','Add2'], [TUnitTestType.Create('NewUTT1'), TUnitTestType.Create('NewUTT2')]);
        Dict1.Merge(Dict2, [jmoStat, jmoAdd]);
        ShowResults(
          'TJX4Dict<T>: jmoAdd "TJX4Dict<T>" by Cloning (Default) :',
          MasterDictObj.ToJSON,
          'jmoAdd: ' + Dict2.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"Add1":{"UTName":"NewUTT1","UTName2":null},"Add2":{"UTName":"NewUTT2","UTName2":null}} '
        );
      finally
        Dict1.Free;
        Dict2.Free;
      end;

    // TJX4Dict<T>: add TJX4Dict<T> Moving
    Dict1 := Nil; Dict2 := Nil;
      try
        Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
        Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['Add1','Add2'], [TUnitTestType.Create('NewUTT1'), TUnitTestType.Create('NewUTT2')]);
        Dict1.Merge(Dict2, [jmoStat, jmoAdd, jmoByMoving]);
        ShowResults(
          'TJX4Dict<T>: jmoAdd "TJX4Dict<T>" by Cloning (Default) :',
          MasterDictObj.ToJSON,
          'jmoAdd : ' + Dict2.ToJSON,
          Dict1.ToJSON,
          Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
          ' {"2":{"UTName":"UT2","UTName2":null},"1":{"UTName":"UT1","UTName2":null},"3":{"UTName":"UT3","UTName2":null},"Add2":{"UTName":"NewUTT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"Add1":{"UTName":"NewUTT1","UTName2":null}} '
        );
      finally
        Dict1.Free;
        Dict2.Free;
      end;

    {$ENDREGION 'TJX4Dict<T> Add'}

    {$REGION 'TJX4Dict<T> Multi'}

    // TJX4Dict<T>: Add + Update TJX4Dict<T>
    Dict1 := Nil; Dict2 := Nil;
    try
      Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
      Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['Add1','Add2','2'], [TUnitTestType.Create('NewUTT1'), TUnitTestType.Create('NewUTT2'), Nil]);
      Dict1.Merge(Dict2, [jmoStat, jmoAdd, jmoUpdate]);       // Add : TUnitTestType('NewUTT2'), TUnitTestType('NrwUTT2'), '5'(Nil)
      ShowResults(                                            // Update: '2' to Nil
        'TJX4Dict<T>: jmoAdd, jmoUpdate "TJX4Dict<T>" by Cloning (Default) :',
        MasterDictObj.ToJSON,
        'jmoAdd + jmoUpdate: ' + Dict2.ToJSON,
        Dict1.ToJSON,
        Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
        ' {"2":{null},"1":{"UTName":"UT1","UTName2":null},"3":{"UTName":"UT3","UTName2":null},"Add2":{"UTName":"NewUTT2","UTName2":null},"4":{"UTName":"UT4","UTName2":null},"Add1":{"UTName":"NewUTT1","UTName2":null}} '
      );
    finally
      Dict1.Free;
      Dict2.Free;
    end;

    // TJX4Dict<T>: Add, Update Purge TJX4Dict<T>
    // Completely useless in real world... (this merge returns a clone of Dict2)
    Dict1 := Nil; Dict2 := Nil;
    try
      Dict1 := MasterDictObj.Clone< TJX4Dict<TUnitTestType> >;
      Dict2 := TJX4Dict<TUnitTestType>.NewAddRange(['1','Add2','2','5'], [TUnitTestType.Create('NewUTT1'), TUnitTestType.Create('NewUTT2'), Nil, Nil]);
      Dict1.Merge(Dict2, [jmoStat, jmoAdd, jmoUpdate, jmoPurge]);       // Add : Add1, Add2, '5'(Nil);  Update: '2' to Nil  ; Purge: '1', '3', '4'
      ShowResults(
        'TJX4Dict<T>: jmoAdd, jmoUpdate, jmoPurge "TJX4Dict<T>" by Cloning (Default) :',
        MasterDictObj.ToJSON,
        'jmoAdd + jmoUpdate + jmoPurge: ' + Dict2.ToJSON,
        Dict1.ToJSON,
        Dict1.EleAdded, Dict1.EleDeleted, Dict1.EleUpdated,
        ' {"2":{null},"5":{null},"1":{"UTName":"NewUTT1","UTName2":null},"Add2":{"UTName":"NewUTT2","UTName2":null}} '
      );
    finally
      Dict1.Free;
      Dict2.Free;
    end

    {$ENDREGION 'TJX4Dict<T> Multi'}

{$ENDREGION 'Dict<T>'}

  finally
    MasterDictObj.Free;
    MasterDictOfValues.Free;
  end;
end;

{ TUTEST }

procedure TUTEST.Clear;
begin
  aaa := Tvalue.Empty;
  bbb.Clear;
  ccc.Clear;
  ddd.Clear;
  eee.Clear
end;

end.
