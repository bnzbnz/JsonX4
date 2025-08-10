Delphi JsonX4 (Json eXtended version 4) : JSON & YAML
=================

JsonX4 is a Delphi JSON/YAML to Objects, Objects to JSON/YAML parser. It is fast (1M/s Ops), light still simple to use
and, of course, the full json specifications are supported ... Tested on Delphi 11.3, 12.3 and Community Edition.

- This project is sponsored by EA4D "Ebay Api 4 Delphi" (https://www.ea4d.com)
- Projetcs using JsonX4 : qBit4DelphiV2 - qNOXifyV2 [https://github.com/bnzbnz/qBit4DelphiV2](https://github.com/bnzbnz/qBit4DelphiV2-qNOXifyV2) a qbittorent API for Delphi.
- Contact : Laurent MEYER JsonX4@lmeyer.fr

Projects using JsonX4
--------------
-  qBit4DelphiV2 - qNOXifyV2 : https://github.com/bnzbnz/qBit4DelphiV2 a qbittorent API for Delphi.
-  ConsoAPI : (https://github.com/bnzbnz/ConsoAPI4Delphi) a Delphi API to get data from Enedis  (French)

How to install
--------------
1. Clone the JsonX4 repository, demos should work out of the box.
2. Add the units from the JsonX4/uJsonX4 folder to your project.

Usage
-----
Example : using primitives (Demo01)

```Delphi
  // Definition
  TPrimitives = class(TJX4Object)
    Str:  TValue;  // a String
    Bool: TValue;  // a Boolean
    Int: TValue;  // an Int64
    Dec: TValue;  // a Decimal
    Cur: TValue;  // a Currenty
    NullStr: TValue; // a Null String Value
  end;

  // Initialization
  Primitives := TPrimitives.Create;
  Primitives.Str := 'testing 😜';
  Primitives.Bool := True;
  Primitives.Int := -999;
  Primitives.Dec := 2.2;
  Primitives.Cur := 22.0; // Make sure this is a decimal value not an Integer
  Primitives.NullStr := Nil;

```
  JX4 will take care of all owned objects (Constructor/Destrutor), for exmaple 'Primitives.Str" is created and will be destroyed automatically, you don't have take care of it!
```Delphi
  // Primitives Serialization
  // Exact Json representation of "Primitives"
  Json := Primitives.ToJson([]);
  
  // Json representation of "Primitives" without the null value  
  Json := Primitives.ToJson([joNullToEmpty]);  
 ```
Json =
```Json
{"Str":"testing 😜","Bool":true,"Int":-999,"Dec":2.2,"Cur":22.0,"NullStr":null}
{"Str":"testing 😜","Bool":true,"Int":-999,"Dec":2.2,"Cur":22.0}
```
```Delphi
  // Json Serialization to NewPrimitives
   NewPrimitives := TJX4Object.FromJSON<TPrimitives>(Json);
 ```
  By deserializing the Json string we basically making a copy of the TPrimtive object
  (we should use .Clone, It's faster...)
```Delphi
   Memo1.lines.add('');
   Memo1.lines.add('Checking the New Object Values:');
   Memo1.lines.add('Str: ' + NewPrimitives.Str.AsString);
   Memo1.lines.add('Int64: ' + NewPrimitives.Int.AsOrdinal.ToString);
   Memo1.lines.add('Decimal: ' + NewPrimitives.Dec.AsExtended .ToString);
   Memo1.lines.add('Currency: ' + NewPrimitives.Cur.AsCurrency.ToString);
  ...
```
-----
Example : using inner classes (Demo02) 
```Delphi

TSubClassDemo = class(TJX4Object)
  X: TValue;
  PClass: TPrimitives
end;

TInnerObjectDemo = class(TJX4Object)
  S: TValue;
  SubClass: TSubClassDemo; // a class TJX4Object
end;

```
```Delphi
  Json := Demo.ToJson([joNullToEmpty]); // Remove null fields
```
```Json
   {"S":"~~😃~~","SubClass":{"X":222}}
```
Obviously you may deserialize the Json string to an TInnerObjectDemo Object.
Again, JX4 will create the inner classes for you. An inner class may contains any number of sub inner classes...

Example : using arrays and dictionaries (Demo03)
-
It's where JX4 excel ! You can create any complex types
```Delphi
  TObjectDemo = class(TJX4Object)
    Str:  TValue;
    Keys: TJX4ValList;                                // an array(List) of strings : TArray<of any primitives>
    Nums: TJX4ValDict;                                // An dictionary of any primitives (<string, number>)  *JSON allows only strings as key
    Primitives: TJX4List<TPrimitive>;                 // A list of TPrimitives (object)
    SLists: TJX4List<TJX4ValList>;                    // A list of primitive Lists
    PDicList: TJX4List<TJX4Dic<TJX4List<TPrimitive>>>;// ouch ! A List of dictionaries of TPrimitives Objects Lists !!!
  end;
```
Please note that JX4 uses TLists instead of arrays, TList being way easier to use.
Filled with value, serializing this (joNullToEmpty), will give you ;
```Json
{"Str":"~~😃~~","Keys":["Q W E R T Y","A Z E R T Y"],"Nums":{"Double":33.33,"Currency":44.44,"Int64":2222,"Int":1111},"Primitives":[{"Bool":true,"I":111},{"Bool":false,"Dble":333.33}],"SLists":[["TTT","OOO"],["XXX","YYY","ZZZ"]],"PDicList":[{"DicVal1":[{"Str":"Boolean1","Bool":true}],"DicVal2":[{"Str":"Boolean2","Bool":true}]},{"DicVal1":[{"Str":"Boolean1","Bool":true}],"DicVal2":[{"Str":"Boolean2","Bool":true}]}]}
```
This is perfectly deserializable back to a TObjectDemo Object. You may validate this json string at : https://jsonformatter.org/json-parser

-----
Example : mapping any type of Objects for JSON serialization/deserialization (Demo04)
-
JX4 is able to handle any type of Objects as long as they implement 4 mandatory public methods, without any inheritence...
```Delphi
  TJSONableStringList = class(TStringList)  // A SringList to be parsed.
  private
    FIsManaged: Boolean;
  public
    procedure JSONCreate(AManaged: Boolean); // After the constructor being called, JX4 will tell the object if it is managed
    function  JSONDestroy: Boolean; // see Demo04
    function  JSONSerialize(AIOBlock: TJX4IOBlock): TValue;
    procedure JSONDeserialize(AIOBlock: TJX4IOBlock);
    // Optionals
    procedure JSONClone(ADestObj: TObject; AOptions: TJX4Options);
    function  JSONMerge(AMergedWith: TStringList; AOptions: TJX4Options): TValue;
  end;
```

```Delphi
  TDemoContainer = class(TJX4Object)
  public
    StringList : TJSONableStringList; // Creation and destruction will be handle automatically
    [JX4Unmanaged]
    StringListNotManaged : TJSONableStringList; // NOT MANAGED : You HAVE to take care of the Creation/Destruction of this Object;
  end;                                          // It will still be serialized/deserialized and created if necessary (clone for ex.)

  MyList :=  TJSONableStringList.Create; // In your code, create and use your own Object
  ...Fill the List
  
  Obj := TDemoContainer.Create; // Create the container
  Obj.StringList.Add('A');      // StringList has been created automagically   
  ... Serialize / Deserialize
  Obj.Free // will not destroy MyList
  
  MyList.Free; // When necessary
```
  So, you will be able to ser/deserialize any object by adding to them the corresponding methods.

Example : Attributes and Options (Demo05)
-
```Delphi
  TDemo = class(TJX4Object)
    [TJX4Required]
    Str:     TValue;                     // A value is required when serializing (Exception)
    [TJX4Name('#href')]                  // a name of the matching json name
    HrefVar: TValue;                     // a name of the json value mapped to "Bool"
    [TJX4Default('22')]                  // a defualt value to be used at deserialization, if the field is null
    Num1:    TValue;
    __23href2: TValue;                   // name encoding :  __23href = #hef    ('_'+'_'+Hex('#')+'href')
    [TJX4Default('true')]                //                                       ^-- Header
    [TJX4Name('NewMix')]
    Mix: TValue;
  end;
```
```Json
 {"Str":"Need a Value","#href":"http://","Num1":22,"#href2":"auto enc/dec oding","NewMix":true}
```
Example : A Random YAML file parsing (Demo06)
-
As simple as that :
```Delphi

  TQuestion = class(TJ43Object)
    question: TValue;
    options:  TJX4ValList;
    answer:   TValue;
  end;

  TGame = class(TJX4Object)
    quiz: TJX4Dic<TJX4Dic<TQuestion>>;   // << Double dictionaries !! 
  end;

  var GameStr :=
    '''
    quiz:
      sport:
        q1:
          question: Which one is correct team name in NBA?
          options:
            - New York Bulls
            - Los Angeles Kings
            - Golden State Warriros
            - Huston Rocket
          answer: Huston Rocket
      maths:
        q1:
          question: 5 + 7 = ?
          options:
            - '10'
            - '11'
            - '12'
            - '13'
          answer: '12'
        q2:
          question: 12 - 8 = ?
          options:
            - '1'
            - '2'
            - '3'
            - '4'
          answer: '4'
    ''';


  var Game := TJX4Object.FromTAML<TGame>(GameStr);               // Get the Object from YAML
  Memo1.Text := TJX4Object.Format( TJX4Object.ToJSON(Game) );    // Get the Json from the Object, and print the formated result

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Questions - Options :');
  for var LPk1 in Game.quiz do                             //Dump  Questions - Options
    for var LPk2 in LPk1.Value do
    begin
      Memo1.Lines.Add(LPk1.Key + ' - ' + LPk2.Value.question.V +' : ');
      for var LP in LPk2.Value.options do
        Memo1.Lines.Add('  ' + LP.Value);
    end;

  Game.Free; 
```
Example : Parse an Array as payload (Demo07)
-
Example : Large JSON, Benchmark.
-
In this example we read, serialize, clone (RTTI/Meerging), deserialize and finally save a large ebay's aspects  JSON and YAML file (around 1M json fields)
You will be able to benchmark and compare the output generated json file 'jsx4.json' vs 'aspects100.json' the original ebay's on. (You'll find also the generated jsx4.yml file)
```
Used Memory 58 MB
Loading ebay's "aspects" json file :
  Stream size: 14,358.14 KB
==> 36 ms
Used Memory 86 MB

Convert Json String to JSX4 Objects (Deserialize):
==> 986 ms
==> 14.56 MB/s
Used Memory 383 MB

JSX4 Object Cloning (by RTTI):
==> 804 ms
==> 17.86 MB/s
Used Memory 517 MB

JSX4 Object Cloning (by Merging):
==> 916 ms
==> 15.67 KM/s
Used Memory 706 MB

Revert JX4 Objects to Json String (Serialize)):
==> 948 ms
==> 15.13 MB/s
Used Memory 709 MB

YAMLize
==> 1242 ms
Used Memory 743 MB

Saving Cloned Json file (jsx4.json) and YAML (jsx4.yaml):
  Stream size: 14,358.14 KB
==> 40 ms
Used Memory 743 MB

Free Json Objects :
  Freed in 559 ms
Used Memory 140 MB
```



