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
  Json5K:  TJX4List<TCustomer>;
  Json50K: TJX4List<TUser>;
  Sw: TStopWatch;
begin
  Memo1.Lines.Clear;
  if TJX4Obj.LoadFromFile('customers-5k.json', JsonStr) >0 then
  begin
    // Deserialize
    Sw := TStopWatch.StartNew;
    Json5K := TJX4Object.FromJSON< TJX4List<TCustomer> >(JsonStr, []);
    Size := JsonStr.Length;
    Memo1.Lines.Add('Neon 5K Cutomers json file : ' +  Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('  Deserialization: ');
    Memo1.Lines.Add('    Loaded Customers: ' + Json5K.Count.ToString);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + 'ms (vs Neon: 187ms)');
    // Serialize
    Sw := TStopWatch.StartNew;
    JSonStr := TJX4Object.ToJSON(Json5K);
    Memo1.Lines.Add('  Serialization: ');
    Size := JSonStr.Length; Memo1.Lines.Add('    Compact Json File Size: ' + Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + 'ms (vs Neon: 173ms)');

    Json5K.SaveToJSONFile('d:\json.json');
    Json5K.Free;
  end;

   Memo1.Lines.Add('');
   Memo1.Lines.Add('-------------------------------');
   Memo1.Lines.Add('');

  if TJX4Obj.LoadFromFile('users-50k.json', JsonStr) >0 then
  begin
    // Deserialize
    Sw := TStopWatch.StartNew;
    Json50K := TJX4Object.FromJSON< TJX4List<TUser> >(JsonStr, []);
    Size := JsonStr.Length;
    Memo1.Lines.Add('Neon 50K Users Json File : ' +  Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('  Deserialization: ');
    Memo1.Lines.Add('    Loaded Users: ' + Json50K.Count.ToString);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + ' ms');
    // Serialize
    Sw := TStopWatch.StartNew;
    JSonStr := TJX4Object.ToJSON(Json50K);
    Memo1.Lines.Add('  Serialization: ');
    Size := JSonStr.Length; Memo1.Lines.Add('    Compact Json File Size: ' + Size.ToKiBMiBGiBTiB);
    Memo1.Lines.Add('    Duration:: ' + Sw.ElapsedMilliseconds.ToString + ' ms');
    Json50K.Free;
  end;

end;

end.
