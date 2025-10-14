unit uLarge;

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

var
  Form4 : TForm4;

implementation

uses
    System.Diagnostics
    , uJX4YAML
    , Windows
    , PSApi
    , ZLib
    , uJX4RTTI
  ;

{$R *.fmx}

function GetMemoryUsed: SIZE_T;
var
  pmc: PROCESS_MEMORY_COUNTERS;
begin
  {$IFDEF MSWINDOWS}
  pmc.cb := SizeOf(pmc);
  if GetProcessMemoryInfo(GetCurrentProcess, @pmc, pmc.cb) then
    Result := pmc.WorkingSetSize
  else
  {$ENDIF}
    Result := 0;
end;

procedure TForm4.ButtonClick( Sender : TObject );
  var
    LJObj, LJObjClone, LJObjMerge: TfetchItemAspectsContentType;
    LYAMLstr: string;
    LJsonStr : string;
    LWatch : TStopWatch;
    LGWatch : TStopWatch;
    LJSize: Int64;
begin

    Memo1.Lines.Clear;
    LGWatch := TStopWatch.StartNew;

    var MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Loading ebay''s "aspects" json file :' );
  LJSize := TJX4Object.LoadFromFile('aspects100.json', LJsonStr);
    Memo1.Lines.add( Format( '  Stream size: %n KB', [ (LJSize / 1024) ] ));
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Convert Json String to JSX4 Objects (Deserialize):' );
    LWatch := TStopWatch.StartNew;
  LJObj := TJX4Object.FromJSON<TfetchItemAspectsContentType>(LJsonStr, [ joRaiseException] );
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'JSX4 Object Cloning (by RTTI):' );
    LWatch := TStopWatch.StartNew;
  LJObjClone := LJObj.Clone<TfetchItemAspectsContentType>;
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n MB/s', [(LJSize /  (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'JSX4 Object Cloning (by Merging):' );
  LJObjMerge := TfetchItemAspectsContentType.Create;
    LWatch := TStopWatch.StartNew;
  LJObjMerge.Merge(LJObjClone, [ jmoAdd ]);
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n KM/s', [(LJSize /  (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Revert JX4 Objects to Json String (Serialize)):' );
    LWatch := TStopWatch.StartNew;
  LJsonStr := LJObj.ToJson([ joNullToEmpty ]);
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'YAMLize' );
    LWatch := TStopWatch.StartNew;
  LYAMLstr := LJObj.ToYAML;
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Saving Cloned Json file (jsx4.json), Beautified (jsx4_formatted.json), Compressed (jsx4.cjson), ' );
    Memo1.Lines.add( 'YAML (jsx4.yaml) and Compressed YAML (jsx4.cyaml) :' );

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Saving Serialized Clone String to jsx4.json' );
    LWatch := TStopWatch.StartNew;
  TJX4Object.SaveToFile('jsx4.json', LJsonStr, TEncoding.UTF8, clNone);
    Memo1.Lines.add( Format('jsx4.json: %n KB, %d ms', [(LJSize / 1024) , LWatch.ElapsedMilliseconds ]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Saving Cloned Object to jsx4-2.json' );
    LWatch := TStopWatch.StartNew;
  LJSize := LJObjClone.SaveToJSONFile( 'jsx4-2.json', False);
    Memo1.Lines.add( Format('jsx4-2.json: %n KB; %d ms', [(LJSize / 1024), LWatch.ElapsedMilliseconds]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Saving Formatted Cloned Object to jsx4_formatted.json' );
    LWatch := TStopWatch.StartNew;
  LJSize := LJObjClone.SaveToJSONFile('jsx4_formatted.json', True, [joNullToEmpty], TEncoding.UTF8, clNone);
    Memo1.Lines.add( Format('jsx4_formatted.json: %n KB; %d ms', [(LJSize / 1024), LWatch.ElapsedMilliseconds]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Saving Compressed Cloned Object to jsx4.Cjson' );
    LWatch := TStopWatch.StartNew;
  LJSize := LJObjClone.SaveToJSONFile( 'jsx4.Cjson', False, [joNullToEmpty], TEncoding.UTF8, clFastest);
    Memo1.Lines.add( Format('jsx4.Cjson: %n KB; %d ms', [(LJSize / 1024), LWatch.ElapsedMilliseconds]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Saving YAML Object to jsx4.yaml' );
    LWatch := TStopWatch.StartNew;
  LJSize := LJObj.SaveToYAMLFile('jsx4.yaml');
    Memo1.Lines.add( Format('jsx4.yaml: %n KB; %d ms', [(LJSize / 1024), LWatch.ElapsedMilliseconds]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Saving Compressed YAML Object to jsx4.cyaml' );
    LWatch := TStopWatch.StartNew;
  LJSize := LJObj.SaveToYAMLFile('jsx4.Cyaml',[joNullToEmpty], TEncoding.UTF8, clFastest);
    Memo1.Lines.add( Format('jsx4.Cyaml: %n KB; %d ms', [(LJSize / 1024), LWatch.ElapsedMilliseconds]));

    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Free Json Objects :' );
  LJObj.Free;
  LJObjClone.Free;
  LJObjMerge.Free;
    Memo1.Lines.add( Format( '  Freed in %d ms', [ LWatch.ElapsedMilliseconds ] ) );
    MB := GetMemoryUsed div (1024*1024);
    Memo1.Lines.add( Format( 'Used Memory %d MB', [ MB ] ) );

  Memo1.Lines.add( '' );
  Memo1.Lines.add( Format( '==>  Total Time %d ms', [ LGWatch.ElapsedMilliseconds ] ) );
end;

initialization
  xRTTIThreaded := False;
end.
