Delphi JsonX4 (Json eXtended version 4.5) : JSON & YAML
=================

JsonX4 is a Delphi JSON/YAML to Objects, Objects to JSON/YAML parser. It is light still simple to use
and, of course, the full JSON specification is supported...\ 
Tested with Delphi 11.3 to 13.1 (+ Community Edition).

The Dilemma: JsonX4 or an other Framework ?
-
- JsonX4 is fast but is limited by the speed of the default delphi JSON library. Though, the RTTI cache give it a big boost.
- It is light but stil powerful
- It fully support ALL json specifications
- It is not limited to JSON but understand YAML too
- I know that it can be overwhelming at start, but, in fact, JsonX4 is really easy to use
- It brings powerful function, like "cloning" and "merging" which can reduce greatly your network traffic and processing (just send deltas)
- Sarting v4.50, all Jsonx4 releases will stay compatible. You work with it now, no new release will break your code. I will only fix or bring new functionalities to the stack
- Finally, JsonX4 is used by REAL customers in REAL production. They simply care about ease of use, feature, update, or bug fix
- The cherry on the top: if any issue arise, I am not only a username on GitIhub, you can contact me to this dedicated email address:
  jsonx4@lmeyer.fr
  I'll answer you ASAP to make JsonX4 better...
- If, by now, there is no dilemma (or you are simply curious) : Enjoy!, Laurent
  
The V4 version has been refactored, partially rewrote, a lot of fixes, add Interfaces support, new Demos and more. The speed gain is around 30%.\
(I am too lazy to bump it up to V5 :) ).

- This project is sponsored by EA4D "ebay Api 4 Delphi" (https://www.ea4d.com), and used in there products.

Projects using JsonX4
--------------
- qBit4DelphiV2 - qNOXifyV2 : https://github.com/bnzbnz/qBit4DelphiV2 a qBitTorrent API for Delphi.
- ConsoAPI : (https://github.com/bnzbnz/ConsoAPI4Delphi) a Delphi API to get data from Enedis (French).
- These projects are currently updated to V4.5 <<<<<<

How to install
--------------
1. Clone the JsonX4 repository, demos should work out of the box.
2. Add the units from the JsonX4/uJsonX4 folder to your project.

Usage
-----
preamble: I had this question a few times about timings.
These demos use JSON files, which are in size or fields count: substantial
They are not real world apps (maybe for ebay!?!?), timings may not reflect the framework performaance nor capacity.
For example, the Large demo is procesing 1 Million fields (pairs name/value), they are serilized, deserialized, copied, cloned, merged, save as JSON/YAML and more...
This result in a total of 7.5s on my lappy. (To be beaten!)
The first demos is what your going to encounter in real life. They are processed in few ms.\
Now, let's talk shop :\
\
Example 1 : Pseudo Pascal primitives (Demo01)

```Delphi
  // Definition
  TPrimitives = class(TJX4Object)
    Str:  TValue;    // a String
    Bool: TValue;    // a Boolean
    Int: TValue;     // an Int64
    Dec: TValue;     // a Decimal
    Cur: TValue;     // a Currency
    NullStr: TValue; // a Null String Value
  end;

  // Initialization
  Primitives         := TPrimitives.Create;
  Primitives.Str     := 'testing 😜';
  Primitives.Bool    := True;
  Primitives.Int     := -999;
  Primitives.Dec     := 2.2;
  Primitives.Cur     := 22.0; // Make sure this is a decimal value not an Integer
  Primitives.NullStr := Nil;

```

```Delphi
  // Primitives Serialization
  // Exact Json representation of "Primitives" : Serialization
  Json := Primitives.ToJson([]);
  
  // Json representation of "Primitives" without the null value : deserialization 
  Json := Primitives.ToJson([joNullToEmpty]);  
 ```

We serialize and deserialize the object. The option [joNullToEmpty] remove null/nil values from the generated JSON.\
So, "Primitives.NullStr" is removed.\
It is really useful to reduce the size of the final JSON string (It can be massive!).

```Json
{"Str":"testing 😜","Bool":true,"Int":-999,"Dec":2.2,"Cur":22.0,"NullStr":null}
{"Str":"testing 😜","Bool":true,"Int":-999,"Dec":2.2,"Cur":22.0}
```

```Delphi
  // Json Serialization to NewPrimitives
   NewPrimitives := TJX4Object.FromJSON< TPrimitives >( Json );
 ```

By deserializing the Json string, we are basically making a copy of the TPrimtive object. This work with any JX4 Objects, dictionaries and Arrays.\
Though, You should use the "clone" function: It's way faster!
  
```Delphi
   Memo1.lines.add('');
   Memo1.lines.add('Checking the New Object Values:');
   Memo1.lines.add('Str: ' + NewPrimitives.Str.AsString);
   Memo1.lines.add('Int64: ' + NewPrimitives.Int.AsOrdinal.ToString);
   Memo1.lines.add('Decimal: ' + NewPrimitives.Dec.AsExtended .ToString);
   Memo1.lines.add('Currency: ' + NewPrimitives.Cur.AsCurrency.ToString);
```
We just check if all is fine!!

-----
Example 2 : nested classes (Demo02) 
-

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
  Demo := TInnerObjectDemo.Create;
  Demo.S := '~~😃~~'; // UTF8 Support
  Demo.SubClass.X := 222;
  Demo.SubClass.PClass.Bool := True;
  Demo.SubClass.PClass.B := 1234;
  Demo.SubClass.PClass.D:= 2.22;
  Demo.SubClass.PClass.Str:= 'ABC';

  Json := Demo.ToJson([joNullToEmpty]);
```

```Json
   {"S":"~~😃~~","SubClass":{"X":222}}
```

Fairly quickly, you will need to add objects to your main JSON class. TInnerObjectDemo contains TSubClassDemo : TSubClassDemo -> TInnerObjectDemo.\
PLEASE NOTE: JX4 handle the creation/destruction of nested objects. It means that, while creating “Demo :=  TInnerObjectDemo.Create", JX4 is going to create automagically the "Demo.SubClass" and "Demo.SubClass.PClass" nested classes, so you can access directly Demo.SubClass.PClass.Str:= 'ABC'
You can disable this behavior using the Option: TJX4Unmanaged
the same apply for freeing, you just need: Demo.Free.
This is important because you will create complex JSON nested objects, dictionaries, arrays; with no worry about freeing. (see Demo08)

Obviously, you can deserialize the Json string to an TInnerObjectDemo Object.
A nested class or nested class can contain any number of sub nested classes...

-----
Example 3 : arrays and dictionaries (Demo03)
-
It's where JX4 excel! Dictionary, Arrays and mixing them...

```Delphi
  TObjectDemo = class(TJX4Object)
    Str:  TValue;
    Keys: TJX4ValList;                                 // an array(List) of strings : TArray<of any primitives>
    Nums: TJX4ValDict;                                 // An dictionary of any primitives (<string, number>)  *JSON allows only strings as key
    Primitives: TJX4List<TPrimitive>;                  // A list of TPrimitives (object)
    SLists: TJX4List<TJX4ValList>;                     // A list of primitive Lists
    PDicList: TJX4List<<TJX4List<TPrimitive>>>;        // ouch ! A List of dictionaries of TPrimitives Objects Lists !!!
  end;
```

All is said! This piece of code is self-explicated. Again (I insist!) you don't create any TJX4... Objects (obviously, except for the top most JX4 object)
Note that JX4 uses TLists instead of Array/TArray, TList being easier to use with way more powerful methods.
So TJX4ValList, TJX4List<T> have the same properties as TList and TJX4ValDict, TJX4Dic<T> as TDictionary
The Demo03's code seems a bit "Overwhelming", but this is not. Just take it step by step.
Filled with value, serializing with(joNullToEmpty which remove empty fields, will give you;

```Json
{"Str":"~~😃~~","Keys":["Q W E R T Y","A Z E R T Y"],"Nums":{"Double":33.33,"Currency":44.44,"Int64":2222,"Int":1111},"Primitives":[{"Bool":true,"I":111},{"Bool":false,"Dble":333.33}],"SLists":[["TTT","OOO"],["XXX","YYY","ZZZ"]],"PDicList":[{"DicVal1":[{"Str":"Boolean1","Bool":true}],"DicVal2":[{"Str":"Boolean2","Bool":true}]},{"DicVal1":[{"Str":"Boolean1","Bool":true}],"DicVal2":[{"Str":"Boolean2","Bool":true}]}]}
```

You want to use a tool to format the resulting JSON: https://jsonformatter.org/json-parser
So, it is deserializable back to a TObjectDemo Object. 
Dirty Secret:  use "Demo.Format();" to directly beautify the result, from the code.

-----
Example 4 : mapping any type of Objects for JSON serialization/deserialization (Demo04)
-

JX4 is able to handle any type of object as long as it implements the mandatory public methods (RTTI/interfaces).
I won’t explain here, but please review the Demo04 code to see how to implement, for example, a TStringList as jsonable.

-----
Example 5 : Attributes and Options (Demo05)
-

```Delphi
  TDemo = class(TJX4Object)
    [TJX4Required]
    Str:     TValue;                     // a value is required when serializing (if not: Exception)
    [TJX4Name('#href')]                  // the name of the matching json name
    HrefVar: TValue;                     // the name of the json value mapped to "#href"
    [TJX4Default('22')]                  // if the field is null, a default value to be used when deserialiazing
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
Attributes:
- TJX4Required : The field can't be null.
- TJX4Name : Name used to ser/deserialize the property :  [TJX4Name('type')] for "type".
- [TJX4Default('22')] : a default value ( not compatible with TJX4Required ).
- Inline encoding: makes a field readable by Delphi by using a property name encoding: ('_'+'_'+Hex('#')+'href') for "#href".
- TJX4Transient: No serialization nor deserialization of the field object
- TJX4Unmanaged: No automated creation and destruction of the field object

Options for ser/deserialization:
- joNullToEmpty:  Remove Null fields when serializing.
- joNoException:  Do NOT Re-Raise internal exceptions.
- joRaiseOnAbort: Abort the current thread/task and raise this exception (TJX4ExceptionAborted).
- joRaiseOnMissingField: Raise an exception when a JSON field is not defined in the matching Delphi object (useful for debug).
- joSlashEncode: Envode slash, useful for use as HTML.
- jmoStat enable statistique while mergin: EleAdded, EleUpdated, EleDeleted.
- jmoAdd, jmoUpdate, jmoByMoving, jmoDelete: Merge operation flags (see UnitTest, It can be complex, but very useful)

-----
Example 6 : randome YAML file parsing (Demo06)
-
Really simple:
 Result := TJX4Object.FromYAML<YAMLObject>( YAMLString)

```Delphi
// TYAMLObject the object containing the YAML mapping (same as JSON)
  TYAMLObject = class(TJX4Object)
    RValue: TValue;
  end;
// Result is a TYAMLObject object
// YAMLObject a TJX4Objectt ‘(mapping)
// YAMLString contains a YAML string
Result := TJX4Object.FromYAML<YAMLObject>( YAMLString );  // (YAML to Objects)
NewYAMLString := Result.ToYAML(); // Object to YAML
```

An that’s it!!

-----
Example 7 : Parse an Array as payload (Demo07)
-
Use TJX4List as json object:

```Delphi

TArrayValue = class(TJX4Object)
  id: TValue;
end;
Str := '["id":"0001"]';
Result := TJX4Obj.FromJSON< TJX4List< TArrayValue > >(Str, [joRaiseOnMissingField]);
```
-----
Example 8 :[ GitHub Extract](https://jsonplaceholder.typicode.com/)  (Demo08)
-
This is old JSON extract from GitHup with 11351 objects in a 34.71 MB. It is dowloaded, deserialized, serialized, and converted to YAML.\
As you can see, JSON file complexity impact speed. (Look at the JX4 classes mapping!). For info the file is a gigantic array.

-----
Example 9 : multipart json donwload from https://jsonplaceholder.typicode.com (Demo09)
-
jsonplaceholder is a free fake and reliable API for testing and prototyping.
We download parts,  and then reassemble them in a unique Json object / string.
TJX4Object classes are pretty nice too. :)

-----
Example 10 : Large JSON, Benchmark mono threaded.
-

In this example we read, serialize, clone (RTTI/Merging), deserialize, convert to YAML, and finally save JSON and YAML files (compressed or not).\
You may compare the output generated json file "jsx4.json" to original aspects100.json' the original "aspects100.json" (Demos folder) : size must be identical and match at 100%. (It's what says "Beyond Compare" :))\
Caution: we are loading and processing 1 million Json fields. Dependig of your system, it may take a while\
As reference my laptop runs it in 7.5s

```Delphi
Used Memory 59 MB
Loading ebay's "aspects" json file :
  Stream size: 14,358.14 KB
==> 39 ms
Used Memory 87 MB

Convert Json String to JSX4 Objects (Deserialize):
==> 923 ms
==> 15.56 MB/s
Used Memory 256 MB

JSX4 Object Cloning (by RTTI):
==> 546 ms
==> 26.25 MB/s
Used Memory 314 MB

JSX4 Object Cloning (by Merging):
==> 608 ms
==> 23.62 KM/s
Used Memory 376 MB

Revert JX4 Objects to Json String (Serialize)):
==> 516 ms
==> 27.77 MB/s
Used Memory 378 MB

YAMLize
==> 308 ms
Used Memory 413 MB

Saving Cloned Json file (jsx4.json), Beautified (jsx4_formatted.json), Compressed (jsx4.cjson), 
YAML (jsx4.yaml) and Compressed YAML (jsx4.cyaml) :

Saving Serialized Clone String to jsx4.json
jsx4.json: 14,358.14 KB, 19 ms

Saving Cloned Object to jsx4-2.json
jsx4-2.json: 14,358.14 KB; 535 ms

Saving Formatted Cloned Object to jsx4_formatted.json
jsx4_formatted.json: 34,398.18 KB; 842 ms

Saving Compressed Cloned Object to jsx4.Cjson
jsx4.Cjson: 2,816.00 KB; 634 ms

Saving YAML Object to jsx4.yaml
jsx4.yaml: 32,174.10 KB; 1172 ms

Saving Compressed YAML Object to jsx4.cyaml
jsx4.Cyaml: 3,008.00 KB; 1291 ms
Used Memory 413 MB

Free Json Objects :
  Freed in 391 ms
Used Memory 135 MB

==>  Total Time 7545 ms
```
-----
Example 11 : Threaded JSON (LargeThreded)
-
JX4 as been design to be thread-safe : no-lock RTTI Cache and automatically Abort on thread destroy.\
In this Demo, you will be able to start threads, tasks, and abort them through Exception.\
joRaiseOnAbort flag is Required.
```Delphi
   LJObj := TJX4Object.FromJSON< TJX4List<TPeople> >(LJsonStr, [ joRaiseOnAbort ] );
```
-----
Example : Unit Tests:
-
A lot of testing to validate the library.\ 
Very useful to learn merging and cloning. (with or whitout Options)\
Clone : when you need an objects exact copy, ex: sync thread with a cloned copy instead the original to be thread safe.\
Merge: Update a destination object with a source object, ex: you maintain a large main object and receive only updates (Deltas), you will merge to apply changes to this main object. (see qBit4DelphiV2 - qNOXifyV2)\
\
Now, it's your turn to work...


 
 

