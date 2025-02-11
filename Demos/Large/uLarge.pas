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
  ;

{$R *.fmx}

procedure TForm4.ButtonClick( Sender : TObject );
  var
    LJObj, LJObjClone, LJObjMerge: TfetchItemAspectsContentType;
    LJsonStr : string;
    LWatch : TStopWatch;
    LJSize: Int64;
begin
    Memo1.Lines.Clear;

    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Loading ebay''s Aspects json file :' );
    LJSize := TJX4Object.LoadFromFile('aspects100.json', LJsonStr, TEncoding.UTF8);
    Memo1.Lines.add( Format( '  Stream size: %n KB', [ (LJSize / 1024) ] ));
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));


    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Convert Json String to JSX3 Objects (Deserialize):' );
    LWatch := TStopWatch.StartNew;
  LJObj := TJX4Object.FromJSON<TfetchItemAspectsContentType>(LJsonStr, [ joRaiseException] );
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));


    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'JSX3 Object Cloning (by RTTI):' );
    LWatch := TStopWatch.StartNew;
  LJObjClone := LJObj.Clone<TfetchItemAspectsContentType>;
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n MB/s', [(LJSize /  (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));


    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'JSX3 Object Cloning (by Merging):' );
  LJObjMerge := TfetchItemAspectsContentType.Create;
    LWatch := TStopWatch.StartNew;
  LJObjMerge.Merge(LJObjClone, [ jmoAdd ]);
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n KM/s', [(LJSize /  (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'Revert JX4 Objects to Json String (Serialize)):' );
    LWatch := TStopWatch.StartNew;
  LJsonStr := LJObj.ToJson([ joNullToEmpty] );
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
    Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / (1024*1000)) / (LWatch.ElapsedMilliseconds / 1000)]));

    Memo1.Lines.add( '' );
    Memo1.Lines.add( 'YAMLize' );
    LWatch := TStopWatch.StartNew;
    var YAMLstr := LJObj.ToYAML;
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Free Json Objects :' );
  LJObj.Free;
  LJObjClone.Free;
  LJObjMerge.Free;
    Memo1.Lines.add( Format( '  Freed in %d ms', [ LWatch.ElapsedMilliseconds ] ) );

    Memo1.Lines.add( '' );
    LWatch := TStopWatch.StartNew;
    Memo1.Lines.add( 'Saving Cloned Json file (jsx4.json) and YAML (jsx4.yml):' );
  LJSize := TJX4Object.SaveToFile( 'jsx4.json', LJsonStr, TEncoding.UTF8);
  TJX4Object.SaveToYAMLFile( 'jsx4.yaml', YAMLstr);
    Memo1.Lines.add( Format( '  Stream size: %n KB', [ (LJSize / 1024) ] ));
    Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  end;

end.
