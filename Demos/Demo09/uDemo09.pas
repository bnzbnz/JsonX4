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

  TJPosts = class(TJX4Object)
    ctnr: TJX4List<TJPost>;
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

  TJPhotos = class(TJX4Object)
    ctnr: TJX4List<TJPhoto>;
  end;

  // Albums:
  TJAlbum = class(TJX4Object)
    userId: TValue; // Integer
    id: TValue; // Integer
    title: TValue; // string

    Photos: TJX4List<TJPhoto>;  // Generated
  end;

  TJAlbums = class(TJX4Object)
    ctnr: TJX4List<TJAlbum>;
  end;

  // Todos
  TJTodo = class(TJX4Object)
    userId: TValue; // Integer
    id: TValue; // Integer
    title: TValue; // string
    completed: TValue; // Boolean
  end;

  TJTodos = class(TJX4Object)
    ctnr: TJX4List<TJTodo>;
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

  TJUsers = class(TJX4Object)
    ctnr: TJX4List<TJUser>;
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
  JPosts: TJPosts;
  JComments: TJComments;
  JAlbums: TJAlbums;
  JPhotos : TJPhotos;
  JTodos: TJTodos;
  JUSers: TJUsers;
  Watch: TStopwatch;
  TimePocess, TimeHTTP : Int64;

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
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JPosts := TJX4Object.FromJSON<TJPosts>(Json);
  end;

    // Albums
  Res := Http.Get('https://jsonplaceholder.typicode.com/albums');
  JAlbums := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JAlbums := TJX4Object.FromJSON<TJAlbums>(Json);
  end;

  // Photos
  Res := Http.Get('https://jsonplaceholder.typicode.com/photos');
  JPhotos := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JPhotos := TJX4Object.FromJSON<TJPhotos>(Json);
  end;

  // Todos
  Res := Http.Get('https://jsonplaceholder.typicode.com/todos');
  JTodos := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JTodos := TJX4Object.FromJSON<TJTodos>(Json);
  end;

  // Users
  Res := Http.Get('https://jsonplaceholder.typicode.com/users');
  JUsers := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JUsers := TJX4Object.FromJSON<TJUsers>(Json);
  end;

  HTTP.Free;
  TimeHTTP := Watch.ElapsedMilliseconds;
  Watch := TStopwatch.StartNew;

  if assigned(JPhotos) and assigned(JAlbums) then
    for var Photo := 0 to JPhotos.ctnr.count - 1 do
      for var Album := 0 to JAlbums.ctnr.count -1 do
        if JPhotos.ctnr[Photo].albumId.AsInteger = JAlbums.ctnr[Album].id.AsInteger then
           JAlbums.ctnr[Album].Photos.add( JPhotos.ctnr[Photo].Clone<TJPhoto> );
  JPhotos.Free;

  if assigned(JAlbums) and assigned(JUsers) then
    for var Album := 0 to JAlbums.ctnr.count - 1 do
      for var User := 0 to JUsers.ctnr.count -1 do
        if JAlbums.ctnr[Album].userId.AsInteger = JUsers.ctnr[User].id.AsInteger then
           JUsers.ctnr[User].Albums.add( JAlbums.ctnr[Album].Clone<TJAlbum> );
  JAlbums.Free;

  if assigned(JTodos) and assigned(JUsers) then
    for var Todo := 0 to JTodos.ctnr.count - 1 do
      for var User := 0 to JUsers.ctnr.count -1 do
        if JTodos.ctnr[Todo].userId.AsInteger = JUsers.ctnr[User].id.AsInteger then
          JUsers.ctnr[User].Todos.add( JTodos.ctnr[Todo].Clone<TJTodo> );
  JTodos.Free;

  if assigned(JPosts) and assigned(JUsers) then
    for var Post := 0 to JPosts.ctnr.count - 1 do
      for var User := 0 to JUsers.ctnr.count -1 do
        if JPosts.ctnr[Post].userId.AsInteger = JUsers.ctnr[User].id.AsInteger then
          JUsers.ctnr[User].Posts.add( JPosts.ctnr[Post].Clone<TJPost> );
  JPosts.Free;

  TimePocess := Watch.ElapsedMilliseconds;

  Memo1.Lines.Add('>> HTTP : ' + TimeHTTP.ToString + ' ms');
  Memo1.Lines.Add('>> Processing : ' + TimePocess.ToString + ' ms');
  Memo1.Lines.Add('>> Total Duration : ' + (TimeHTTP + TimePocess).ToString + ' ms');

  Memo1.Lines.add(JUsers.Format);
  Memo1.Lines.Add('Lines :'+ Memo1.Lines.count.ToString);
  JUsers.Free;

  end;

end.
