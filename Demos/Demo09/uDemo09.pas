unit uDemo09;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJX4List
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

  // Posts:
  TJPost = class(TJX4Object)
    userId: TValue; // string
    id: TValue; // Integer
    title: TValue; // string
    body: TValue; // string
  end;

  // Comments:
   TJComment = class(TJX4Object)
    postId: TValue; // Integer
    id: TValue; // Integer
    name: TValue; // string
    email: TValue; // string
    body: TValue; // string
  end;

  TJComments = class(TJX4Object)
    container: TJX4List<TJComment>;
  end;

  // Photos
  TJPhoto = class(TJX4Object)
    albumId: TValue; // Integer
    id: TValue; // Integer
    title: TValue; // string
    url: TValue; // string
    thumbnailUrl: TValue; // string
  end;

  // Albums:
  TJAlbum = class(TJX4Object)
    userId: TValue; // Integer
    id: TValue; // Integer
    title: TValue; // string

    Photos: TJX4List<TJPhoto>;  // Generated
  end;

  // Todos
  TJTodo = class(TJX4Object)
    userId: TValue; // Integer
    id: TValue; // Integer
    title: TValue; // string
    completed: TValue; // Boolean
  end;

  // Users
  TJGeo = class(TJX4Object)
    lat: TValue; // Float;
    lng: TValue; // Float;
  end;

  TJAddress = class(TJX4Object)
    street: TValue; // string
    suite: TValue; // string
    city: TValue; // string
    zipcode: TValue; // string;
    geo: TJGeo;
  end;

  TJCompany = class(TJX4Object)
    name: TValue; // string
    catchPhrase: TValue; // string
    bs: TValue; // string
  end;

  TJUser = class(TJX4Object)
    id: TValue; // Integer
    name: TValue; // string
    username: TValue; // string
    email: TValue; // string
    address: TJAddress;
    phone: TValue; // string
    website: TValue; // string
    company: TJCompany;

    Albums: TJX4List<TJAlbum>; // Generated
    Posts: TJX4List<TJPost>; // Generated
    Todos: TJX4List<TJTodo>; //Generated

  end;

 var
  Form4: TForm4;

implementation
uses
    Math
  , UJX4Rtti
  , System.Net.HttpClient
  , System.Diagnostics
  , System.Net.URLClient
  , System.NetConsts
  , System.Generics.Defaults
  ;

{$R *.fmx}

procedure TForm4.ButtonClick(Sender: TObject);
var
  Res: IHTTPResponse;
  Http: THTTPClient;
  Json: string;
  JPosts: TJX4List<TJPost>;
  JComments: TJComments;
  JAlbums: TJX4List<TJAlbum>;
  JPhotos : TJX4List<TJPhoto>;
  JTodos: TJX4List<TJTodo>;
  JUSers: TJX4List<TJUser>;
  Watch: TStopwatch;
  TimeProcess, TimeHTTP : Int64;

begin
  Memo1.Lines.Clear;

  Watch := TStopwatch.StartNew;

  Http := THTTPClient.Create;
  Http.AutomaticDecompression := [THTTPCompressionMethod.Any];
  Http.ContentType := 'application/json';

  // Posts
  Res := Http.Get('https://jsonplaceholder.typicode.com/posts');
  JPosts := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
    JPosts := TJX4Object.FromJSON< TJX4List<TJPost> >(Res.ContentAsString(TEncoding.UTF8));

    // Albums
  Res := Http.Get('https://jsonplaceholder.typicode.com/albums');
  JAlbums := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
    JAlbums := TJX4Object.FromJSON< TJX4List<TJAlbum> >(Res.ContentAsString(TEncoding.UTF8));

  // Photos
  Res := Http.Get('https://jsonplaceholder.typicode.com/photos');
  JPhotos := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
    JPhotos := TJX4Object.FromJSON<  TJX4List<TJPhoto> >(Res.ContentAsString(TEncoding.UTF8));

  // Todos
  Res := Http.Get('https://jsonplaceholder.typicode.com/todos');
  JTodos := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
    JTodos := TJX4Object.FromJSON< TJX4List<TJToDo> >(Res.ContentAsString(TEncoding.UTF8));

  // Users
  Res := Http.Get('https://jsonplaceholder.typicode.com/users');
  JUsers := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
    JUsers := TJX4Object.FromJSON< TJX4List<TJUser> >(Res.ContentAsString(TEncoding.UTF8));

  HTTP.Free;
  TimeHTTP := Watch.ElapsedMilliseconds;
  Watch := TStopwatch.StartNew;

  if assigned(JPhotos) and assigned(JAlbums) then
    for var Photo := 0 to JPhotos.count - 1 do
      for var Album := 0 to JAlbums.count -1 do
        if JPhotos[Photo].albumId.AsInteger = JAlbums[Album].id.AsInteger then
           JAlbums[Album].Photos.add( JPhotos[Photo].Clone<TJPhoto> );
  JPhotos.Free;

  if assigned(JAlbums) and assigned(JUsers) then
    for var Album := 0 to JAlbums.count - 1 do
      for var User := 0 to JUsers.count -1 do
        if JAlbums[Album].userId.AsInteger = JUsers[User].id.AsInteger then
           JUsers[User].Albums.add( JAlbums[Album].Clone<TJAlbum> );
  JAlbums.Free;

  if assigned(JTodos) and assigned(JUsers) then
    for var Todo := 0 to JTodos.count - 1 do
      for var User := 0 to JUsers.count -1 do
        if JTodos[Todo].userId.AsInteger = JUsers[User].id.AsInteger then
          JUsers[User].Todos.add( JTodos[Todo].Clone<TJTodo> );
  JTodos.Free;

  if assigned(JPosts) and assigned(JUsers) then
    for var Post := 0 to JPosts.count - 1 do
      for var User := 0 to JUsers.count -1 do
        if JPosts[Post].userId.AsInteger = JUsers[User].id.AsInteger then
          JUsers[User].Posts.add( JPosts[Post].Clone<TJPost> );
  JPosts.Free;

  TimeProcess := Watch.ElapsedMilliseconds;

  Memo1.Lines.Add('>> HTTP : ' + TimeHTTP.ToString + ' ms');
  Memo1.Lines.Add('>> Processing : ' + TimeProcess.ToString + ' ms');
  Memo1.Lines.Add('>> Total Duration : ' + (TimeHTTP + TimeProcess).ToString + ' ms');

  Memo1.Lines.add(JUsers.Format);
  Memo1.Lines.Add('Lines : '+ Memo1.Lines.count.ToString);
  JUsers.Free;

  end;

end.
