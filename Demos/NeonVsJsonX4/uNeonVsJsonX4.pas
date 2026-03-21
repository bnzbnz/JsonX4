unit uNeonVsJsonx4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4List
  , uJX4Dict
  , uJX4Value
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

  TUser = class( TJX4Object )
    ID: TValue; // Integer,
    Name: TValue; // String
    BirthDate: TValue; // String
  end;

  TUserArray = class( TJX4Object )
    container: TJX4List<TUser>;  // >> Array of TUser
  end;

  TAddress = class( TJX4Object )
    AddressType: TValue; // String
    Street: TValue; // String,
    City: TValue; // String
  end;

  TContact = class( TJX4Object )
    Dept: TValue; // String
    Name: TValue; // String
    Email: TValue; // String
    Phone: TValue; // String
    Address: TAddress;
  end;

  TCustomer = class( TJX4Object )
    ID : TValue; // string
    CompanyName: TValue; // String
    Address: TAddress;  // TAddres Object
    Contacts: TJX4List<TContact>  // List of Contacts
  end;

  TCustomerArray = class(TJX4Object)
    container: TJX4List<TCustomer>;  // >> Array of TCustomer
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
  Size: TValue;
  JsonStr: string;
  Json5K: TCustomerArray;
  Json50K: TUserArray;
  Sw: TStopWatch;
begin
  Memo1.Lines.Clear;
  if TJX4Obj.LoadFromFile('customers-5k.json', JsonStr) >0 then
  begin
    // Deserialize
    Sw := TStopWatch.StartNew;
    Json5K  := TJX4Obj.FromJSON<TCustomerArray>( '{"container":' + JsonStr + '}' , [joRaiseOnMissingField, joRaiseOnException]);
    Size := JsonStr.Length;
    Memo1.Lines.Add('Neon 5K Cutomers json file : ' +  Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('  Deserialization: ');
    Memo1.Lines.Add('    Loaded Customers: ' + Json5K.container.Count.ToString);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + 'ms (vs Neon: 187ms)');
    // Serialize
    Sw := TStopWatch.StartNew;
    JSonStr := TJX4Obj.ToJSON(Json5K.container);
    Memo1.Lines.Add('  Serialization: ');
    Size := JSonStr.Length; Memo1.Lines.Add('    Compact Json File Size: ' + Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + 'ms (vs Neon: 173ms)');
    Json5K.Free;
  end;

   Memo1.Lines.Add('');
   Memo1.Lines.Add('-------------------------------');
   Memo1.Lines.Add('');

  if TJX4Obj.LoadFromFile('users-50k.json', JsonStr) >0 then
  begin
    // Deserialize
    Sw := TStopWatch.StartNew;
    Json50K := TJX4Obj.FromJSON<TUserArray>( '{"container":' + JsonStr + '}' , [joRaiseOnMissingField, joRaiseOnException]);
    Size := JsonStr.Length;
    Memo1.Lines.Add('Neon 50K Users Json File : ' +  Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('  Deserialization: ');
    Memo1.Lines.Add('    Loaded Users: ' + Json50K.container.Count.ToString);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + ' ms');
    // Serialize
    Sw := TStopWatch.StartNew;
    JSonStr := TJX4Obj.ToJSON(Json50K.container);
    Memo1.Lines.Add('  Serialization: ');
    Size := JSonStr.Length; Memo1.Lines.Add('    Compact Json File Size: ' + Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + ' ms');
    Json50K.Free;
  end;

//users-50k.json
end;


(*
procedure TForm4.ButtonClick(Sender: TObject);
var
  Primitives, NewPrimitives: TPrimitives;
  Json: string;
  LWatch: TStopWatch;
begin

  LWatch := TStopWatch.StartNew;

  caption := 'JsonX4 Version : ' + TJX4Object.Version;

  Memo1.Lines.Clear;

  Primitives := TPrimitives.Create;
  Primitives.Str := 'testing 😜';
  Primitives.Bool := True;
  Primitives.Int := -999;
  Primitives.Dec := 2.2; // Make sure this is a decimal value not an Integer
  Primitives.Cur := 22.0;
  Primitives.NullStr := Nil;

  // Raw Json
  Json := Primitives.ToJson([]);
  Memo1.lines.add('Serialized Object:');
  Memo1.lines.add(Json);

  // Optimized Json
  Memo1.lines.add('');
  Json := Primitives.ToJson([joNullToEmpty]);
  Memo1.lines.add('Serialized and Optimized Object (null removed):');
  Memo1.lines.add(Json);

  // Converting back to a Primitives Object;
  NewPrimitives := TJX4Object.FromJSON<TPrimitives>(Json);

  // Serializing the New Object
  Memo1.lines.add('');
  Json := Primitives.ToJson;
  Memo1.lines.add('Cloned Object:');
  Memo1.lines.add(Json);

  // Checking Values
  Memo1.lines.add('');
  Memo1.lines.add('Checking Cloned Object Values:');
  Memo1.lines.add('Str: ' + NewPrimitives.Str.AsString);
  Memo1.lines.add('Int64: ' + NewPrimitives.Int.AsOrdinal.ToString);
  Memo1.lines.add('Decimal: ' + NewPrimitives.Dec.AsExtended .ToString);
  Memo1.lines.add('Currency: ' + NewPrimitives.Cur.AsCurrency.ToString);

  // Formatted Json
  Memo1.lines.add('');
  Memo1.lines.add('Formatted Serialized Object:');
  Memo1.lines.add(NewPrimitives.Format);

  NewPrimitives.Free;
  Primitives.Free;

  Memo1.Lines.add(Format('Processing Duration ==> %d ms', [ LWatch.ElapsedMilliseconds ]));

end;
*)

end.
