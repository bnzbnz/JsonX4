unit uDemo04;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJSONableStringList
  ;

type

  // See uJSONableStringList.pas, a de/serialize StringList

  TForm4 = class(TForm)
    Memo1: TMemo;
    Button: TButton;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDemoContainer = class(TJX4Object)
  public
    StringListRTTI : TJSONableStringListByRTTI;             // Construction and destruction will be handled automagically
    StringListItntf: TJSONableStringListByIntf;             // Construction and destruction will be handled automagically
    [TJX4Unmanaged]                                         // NOT MANAGED : You have to take care of the Construction/Destruction of these Objects;
    StringListRTTINotManaged : TJSONableStringListByRTTI;   // Using RTTI
    [TJX4Unmanaged]                                         // NOT MANAGED : You have to take care of the Construction/Destruction of these Objects;
    StringListIntfNotManaged : TJSONableStringListByIntf;   // Using Interface
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
  Json:       string;
  Obj, NewObj:TDemoContainer;
  MyListRTTI: TJSONableStringListByRTTI;
  MyListIntf: TJSONableStringListByIntf;
  LWatch:     TStopWatch;
begin

  LWatch := TStopWatch.StartNew;

  Memo1.Lines.Clear;

  MyListRTTI := TJSONableStringListByRTTI.Create; // we create "jsonable" object using RTTI
  MyListIntf := TJSONableStringListByIntf.Create; // we create "jsonable" object using an Interface

  MyListRTTI.Add('By RTTI');                      // MyListRTTI, MyListIntf are TJSONableStringList, we add a value to each of them
  MyListIntf.Add('By Intf');
  Obj := TDemoContainer.Create;                   // we create other TDemoContainer...

  Obj.StringListRTTI.Add('A');                    // we fill the RTTI managed StringList
  Obj.StringListRTTI.Add('B');
  Obj.StringListRTTI.Add('C');
  Obj.StringListRTTI.Add('D');

  Obj.StringListItntf.Add('A');                   // we fill the RTTI interfaced StringList
  Obj.StringListItntf.Add('B');
  Obj.StringListItntf.Add('C');
  Obj.StringListItntf.Add('D');

  Obj.StringListRTTINotManaged := MyListRTTI;     // TStringListRTTINotManaged, Obj.StringListIntfNotManaged are not managed
  Obj.StringListIntfNotManaged := MyListIntf;     // We assign the RTTI/Intf objects created previously

  // Raw Json
  Json := Obj.ToJson([]);                         // all properties are initialize we car serialize them

  // Formatted Json
  Memo1.lines.add('Raw:');                        // show result
  Memo1.lines.add(Json);

  Obj.StringListRTTI.Strings[0] := '>>';          // We update the lists with dummy values (as demo)
  Obj.StringListRTTI.Strings[2] := '<<';
  Obj.StringListItntf.Strings[0] := '>>';
  Obj.StringListItntf.Strings[2] := '<<';

  MyListRTTI.Add('Not Managed');                  // add values to the unmanaged lists (as demo)
  MyListIntf.Add('Not Managed');

  // Updated Json
  Json := Obj.ToJson([]);                         //show serialized result
  Memo1.lines.add('');
  Memo1.lines.add('Update:');
  Memo1.lines.add(Json);

  // Cloned Json
  NewObj := Obj.Clone<TDemoContainer>;            // we clone the json object as NewObj: in this clone the unmanaged list will be copied and managed...
  Json := NewObj.ToJson(NewObj, []);              // serialize the new object; unmanaged lists being will be managed
  Memo1.lines.add('');                            //show serialized result
  Memo1.lines.add('Clone "Updated":');
  Memo1.lines.add(Json);
  Memo1.lines.add('cloned StringListRTTINotManaged in now MANAGED');
  Memo1.lines.add('cloned StringListIntfNotManaged in now MANAGED');

  NewObj.Free;                                    // destroy clone
  Obj.Free;                                       // destroy object
  MyListRTTI.Free;                                // destroy the unmanaged lists... Requierd as the are unmanaged (memory leak)
  MyListIntf.Free;

  Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));

end;

end.
