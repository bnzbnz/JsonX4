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
    container: TJX4List<TJPost>;
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

  // Albums:
  TJAlbum = class(TJX4Object)
    userId: TValue; // Integer
    id: TValue; // Integer
    title: TValue; // string
  end;

  TJAlbums = class(TJX4Object)
    ctnr: TJX4List<TJAlbum>;
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
  end;

  TJUsers = class(TJX4Object)
    ctnr: TJX4List<TJUser>;
  end;


var
  Form4: TForm4;

implementation
uses
  System.Net.HttpClient,
  System.Diagnostics;

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
  i: Integer;
begin
  Memo1.Lines.Clear;

  var Watch := TStopwatch.StartNew;

  Http := THTTPClient.Create;
  Http.AutomaticDecompression := [THTTPCompressionMethod.Any];
  Http.ContentType := 'application/json';

  // Posts
  Memo1.lines.add('Get Posts:');
  Res := Http.Get('https://jsonplaceholder.typicode.com/posts');
  JPosts := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"container":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JPosts := TJX4Object.FromJSON<TJPosts>(Json);
    Memo1.Lines.Add('Count: ' + JPosts.container.Count.ToString + ': ');
    for i := 0 to 1 do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  Id:     ' + JPosts.container[i].id.ToString);
      Memo1.Lines.Add('  UserId: ' + JPosts.container[i].userId.ToString);
      Memo1.Lines.Add('  Title:  ' + JPosts.container[i].title.ToString);
      Memo1.Lines.Add('  Body:   ' + JPosts.container[i].body.ToString);
    end;
    Memo1.Lines.Add('');
    Memo1.Lines.Add('>>>>>>>>>>>>> And More...');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('');
  end;
  JPosts.Free;

    // Albums
  Memo1.lines.add('Get Comments:');
  Res := Http.Get('https://jsonplaceholder.typicode.com/albums');
  JAlbums := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JAlbums := TJX4Object.FromJSON<TJAlbums>(Json);
    Memo1.Lines.Add('Count: ' + JAlbums.ctnr.Count.ToString + ': ');
    for i := 0 to 1 do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  UserId: ' + JAlbums.ctnr[i].userId.ToString);
      Memo1.Lines.Add('  Id:     ' + JAlbums.ctnr[i].id.ToString);
      Memo1.Lines.Add('  Title:  ' + JAlbums.ctnr[i].Title.ToString);
    end;
    Memo1.Lines.Add('');
    Memo1.Lines.Add('>>>>>>>>>>>>> And More...');
    Memo1.Lines.Add('');
    Memo1.Lines.Add('');
  end;
  JAlbums.Free;


    // Photos
  Memo1.lines.add('Get Photos:');
  Res := Http.Get('https://jsonplaceholder.typicode.com/photos');
  JPhotos := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JPhotos := TJX4Object.FromJSON<TJPhotos>(Json);
    Memo1.Lines.Add('Count: ' + JPhotos.ctnr.Count.ToString + ': ');
    for i := 0 to 1 do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  AlbumId: ' + JPhotos.ctnr[i].albumId.ToString);
      Memo1.Lines.Add('  Id:      ' + JPhotos.ctnr[i].Id.ToString);
      Memo1.Lines.Add('  Title:   ' + JPhotos.ctnr[i].title.ToString);
      Memo1.Lines.Add('  Url:     ' + JPhotos.ctnr[i].url.ToString);
    end;
    Memo1.Lines.Add('');
    Memo1.Lines.Add('>>>>>>>>>>>>> And More...');
    Memo1.Lines.Add('');
  end;
  JPhotos.Free;

  // Todos
  Memo1.lines.add('Get Todos:');
  Res := Http.Get('https://jsonplaceholder.typicode.com/todos');
  JTodos := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JTodos := TJX4Object.FromJSON<TJTodos>(Json);
    Memo1.Lines.Add('Count: ' + JTodos.ctnr.Count.ToString + ': ');
    for i := 0 to 1 do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  UserId:    ' + JTodos.ctnr[i].userId.ToString);
      Memo1.Lines.Add('  Id:        ' + JTodos.ctnr[i].Id.ToString);
      Memo1.Lines.Add('  Title:     ' + JTodos.ctnr[i].title.ToString);
      Memo1.Lines.Add('  Completed: ' + JTodos.ctnr[i].completed.ToString);
    end;
    Memo1.Lines.Add('');
    Memo1.Lines.Add('>>>>>>>>>>>>> And More...');
    Memo1.Lines.Add('');
  end;
  JTodos.Free;

  // Users
  Memo1.lines.add('Get Users:');
  Res := Http.Get('https://jsonplaceholder.typicode.com/users');
  JUsers := Nil;
  if Assigned(Res) and (Res.StatusCode = 200) then
  begin
    Json := '{"ctnr":' + Res.ContentAsString(TEncoding.UTF8) + '}';
    JUsers := TJX4Object.FromJSON<TJUsers>(Json);
    Memo1.Lines.Add('Count: ' + JUsers.ctnr.Count.ToString + ': ');
    for i := 0 to 1 do
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add(TJX4Object.FormatJSON(JUsers.ctnr[i].ToJSON()));
    end;
    Memo1.Lines.Add('');
    Memo1.Lines.Add('>>>>>>>>>>>>> And More...');
    Memo1.Lines.Add('');
  end;
  JUsers.Free;

  HTTP.Free;

  Memo1.Lines.Add('>> Total Duration : ' + Watch.ElapsedMilliseconds.ToString + ' ms');

end;

end.
