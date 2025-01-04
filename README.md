JsonX4 (Json eXtended version 4)
=================

JsonX3 is a Delphi Json to Objects, Objects to Json parser. It is fast (1M/s Ops), light still simple to use
It supports Delphi 10.3 to 12.2 on all the platforms. And, of course, 100% of the json format is supported...

- This project is sponsored by EA4D "Ebay Api 4 Delphi" (https://www.ea4d.com)
- Contact : Laurent MEYER JsonX4@lmeyer.fr

How to install
--------------
1. Clone the uJsonX4 repository
2. Add the the unit from the JsonX3/JsonX3 folder to your project.

Usage
-----
Example : using primitives (Demo01)

```Delphi

  TPrimitives = class(TJX3Object)
    Str:   TValue; // As Str
    Bool: TValue; // As Bool
    Num:  TValue; // as Int64
  end;

  Primitives := TPrimitives.Create;
  Primitives.Str := 'testing 😜';
  Primitives.Bool := True;

  Primitives.Num1 := -999;
  Primitives.Num2 := 999;
  Primitives.Num3 := 2.2;
  Primitives.Num4 := 22.22;
  Primitives.NullStr := Nil;

```
  JX4 will take care of all owned objects (Constructor/Destrutor), for exmaple 'Primitives.Str" is created and will be destroyed automatically (or pooled) , you don't have take care of it!
```Delphi
   Json := Primitives.ToJson([]); // Serialization
```
```Json
{"Str":"testing 😜","Bool":true,"Num":-99}
```
```Delphi
   NewPrimitives := TJX4Object.FromJSON<TPrimitives>(Json);
 ```
  By deserializing from the Json string we made a copy of the TPrimtive object
```Delphi
Result =
  DeserPrim.Str.AsString ==> 'testing 😜';
  DeserPrim.Bool.AsBoolean ==> True;       
  DeserPrim.Num.AsOrdinal ==> -99;
```
-----
Example : using inner classes (Demo02) 
```Delphi

TSubClassDemo = class(TJX3Object)
  X: TJX3Num;
end;

TInnerObjectDemo = class(TJX3Object)
  S: TJX3Str;
  NullStr: TJX3Str;
  SubClass: TSubClassDemo; // a class
end;

```
```Delphi
  Json := TJX3Object.ToJson(Demo, [joNullToEmpty]);  // Remove null fields
```
```Json
   {"S":"~~😃~~","SubClass":{"X":222}}
```
Obviously you may deserialize the Json string to an TInnerObjectDemo Object.
Again, JX3 will create the inner classes for you. An inner class can containt any number of sub inner classes...

Example : using arrays and dictionaries (Demo03)
-
It's where JX3 excel ! You can create any complex types
```Delphi
  TObjectDemo = class(TJX3Object)
    S: TJX3Str;
    X: TJX3List<TJX3Str>;                         // an array(List) of strings : TArray<string>
    Y: TJX3Dic<TJX3Num>;                          // An dictionary of Numbers (<string, number>)  *JSON allows only strings as key
    Z: TJX3List<TPrimitives>;                     // A list of TPrimitives
    Q: TJX3List<TJX3List<TJX3Str>>;               // A list of string Lists
    W: TJX3List<TJX3Dic<TJX3List<TPrimitives>>>;  // ouch ! A List of dictionaries of TPrimitives Objects Lists !!!
  end;
```
Please note that JX3 uses TLists instead of arrays, TList being way easier to use.
Filled with value, serializing this (joNullToEmpty), will give you ;
```Json

{"S":"~~😃~~","X":["@@@@","EEZZ","OOOO"],"Y":{"Value1":1111,"Value4":4444,"Value2":2222,"Value3":3333},"Z":[{"Bool":true,"e":111},{"Bool":false,"e":333}],"Q":[["TTT","OOO"],["UUU","III"]],"W":[{"DicVal1":[{"Str":"Boolean1","Bool":true}],"DicVal2":[{"Str":"Boolean3","Bool":true}]},{"DicVal1":[{"Str":"Boolean1","Bool":true}],"DicVal2":[{"Str":"Boolean3","Bool":true}]}]}
```
This is perfectly deserializable back to a TObjectDemo Object! You may validate this json string at : https://jsonformatter.org/json-parser

-----
Example : mapping any type of Objects for JSON serialization/deserialization (Demo04)
JX3 is able to handle any type of Objects as long as they implement 4 mandatory public methods
```Delphi
  TJSONableStringList = class(TStringList)  // A SringList to be parsed.
  private
    FIsManaged: Boolean;
  public
    procedure   JSONCreate(AManaged: Boolean); // After the constructor being called, JX3 will tell the object if it is managed
    function    JSONSerialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock): TValue; // see Demo04
    procedure   JSONDeserialize(AInfoBlock: TJX3InfoBlock; AInOutBlock: TJX3InOutBlock); // see Demo04
    function    JSONDestroy: Boolean // Return the previous manage flag from JSONCreate;
  end;
```

```Delphi
  TDemoContainer = class(TJX3Object)
  public
    StringList : TJSONableStringList; // Creation and destruction will be handle automatically
    [JX3Unmanaged]
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
It's where JX3 excel ! You can create any complex types
```Delphi
  TDemo = class(TJX3Object)
    [JS3Required]
    Str:     TJX3Str;                   // A value is required when serializing (Exception)
    [JX3Name('#href')]                 
    HrefVar: TJX3Str;                   // a JSON field name to be read/write from/to the Json file as HrefVar.
    [JX3Default('22')]                  // a default value to be used during deserialization if the field is null
    Num1:    TJX3Num;
    __23href2: TJX3Str;                 // name encoding :  __23href = #hef  ('_'+'_'+Hex('#')+'href')
                                        // instead of usin JX3Name attribute you may use this inline encoding, it is usefull for code generators like OpenAPI 
    [JX3Default('true')]                // functions NameEncode in uJX3OBject...
    [JX3Name('NewMix')]
    Mix: TJX3Bool;                      // Using NewMix as JSON field name with a default value of True;
  end;
```
```Json
 {"Str":"Need a Value","#href":"http://","Num1":22,"#href2":"auto enc/dec oding","NewMix":true}
```
Example : A Random Json file parsing (Demo06)
-
Extract from : https://github.com/dmjio/json-test/blob/master/example.json

As simple as that :
```Delphi

  TQuestion = class(TJX3Object)
    question: TJX3Str;
    options:  TJX3List<TJX3Str>;
    answer:   TJX3Str;
  end;

  TGame = class(TJX3Object)
    quiz: TJX3Dic<TJX3Dic<TQuestion>>;   // << Double dictionaries
  end;

  var GameStr :=
    '''
    {"quiz":{"sport":{"q1":{"question":"Which one is correct team name in NBA?","options":["New York Bulls",
    "Los Angeles Kings","Golden State Warriros","Huston Rocket"],"answer":"Huston Rocket"}},
    "maths":{"q1":{"question":"5 + 7 = ?","options":["10","11","12","13"],"answer":"12"},
    "q2":{"question":"12 - 8 = ?","options":["1","2","3","4"],"answer":"4"}}}}
    ''';

  var Game := TJX3Object.FromJSON<TGame>(GameStr);         // Get the Object from Json
  Memo1.Text := TJX3.FormatJSON(  TJX3.ToJSON(Game) );     // Get the Json from the Object, and print the formated result

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
In this example we read, serialize, clone (RTTI/Meerging), deserialize and finally save a large ebay's aspects json file (around 1M json fields)
You will be able to benchmark and compare the output generated json file 'jsx3.json' vs 'aspects100.json' the original ebay's on. (please note that you should enable $DEFINE JX3SPEEDUP in uTJX2Object.pas for large file caching...)  :

```

Loading ebay's Aspects json file :
  Stream size: 14358 KB

Convert Json String to JSX3 Objects (Deserialize):
==> 918196 ops in 916 ms
==> 1002397 /s

JSX3 Object Cloning (RTTI):
==> 918196 ops in 912 ms
==> 1006793 /s

JSX3 Object Cloning (Merging):
==> 918196 ops in 907 ms
==> 1012343 /s

Revert JSX3 Objects to Json String (Serialize)):
==> 918196 ops in 692 ms
==> 1326872 /s

Free Json Object :
  Freed in 206 ms

Saving Cloned Json file (jsx3.json) :
  Stream size: 14358 KB

```



