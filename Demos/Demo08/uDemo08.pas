unit uDemo08;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls
  , RTTI
  , uJX4Object
  , uJX4Value
  , uJX4List
  , uJX4Dict
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

  TUser = class(TJX4Object)
    name: TValue;
    email: TValue;
    login: TValue;
    avatar_url: TValue;
    gravatar_id: TValue;
    id: TValue;
    url: TValue;
    html_url: TValue;
    followers_url: TValue;
    following_url: TValue;
    gists_url: TValue;
    starred_url: TValue;
    subscriptions_url: TValue;
    organizations_url: TValue;
    repos_url: TValue;
    events_url: TValue;
    received_events_url: TValue;
    &type: TValue;
    site_admin: TValue;
  end;

  TOrg = class(TJX4Object)
    id: TValue;
    login: TValue;
    gravatar_id: TValue;
    url: TValue;
    avatar_url: TValue;
  end;

  TAsset =  class(TJX4Object)
    url: TValue;
    id: TValue;
    name : TValue;
    &label: TValue;
    uploader: TUser;
    content_type: TValue;
    state: TValue;
    size: TValue;
    download_count: TValue;
    created_at: TValue;
    updated_at: TValue;
    browser_download_url: TValue;
  end;

  TRelease = class(TJX4Object)
    url: TValue;
    assets_url: TValue;
    upload_url: TValue;
    html_url: TValue;
    id: TValue;
    tag_name: TValue;
    target_commitish: TValue;
    name: TValue;
    draft: TValue;
    author: TUser;
    prerelease: TValue;
    created_at: TValue;
    published_at: TValue;
    assets: TJX4List<TAsset>;
    tarball_url: TValue;
    zipball_url: TValue;
    body: TValue;
  end;

  TCommit = class(TJX4Object)
    sha: TValue;
    author: TUser;
    &message: TValue;
    distinct: TValue;
    url: TValue;
  end;

  TRepo = class(TJX4Object)
    id: TValue;
    name: TValue;
    full_name: TValue;
    owner: TUser;
    url: TValue;
    &private: TValue;
    html_url: TValue;
    description: TValue;
    fork: TValue;
    keys_url:  TValue;
    forks_url:  TValue;
    teams_url: TValue;
    hooks_url: TValue;
    issue_events_url: TValue;
    events_url: TValue;
    assignees_url: TValue;
    branches_url: TValue;
    tags_url: TValue;
    blobs_url: TValue;
    git_tags_url: TValue;
    git_refs_url: TValue;
    trees_url: TValue;
    statuses_url: TValue;
    languages_url: TValue;
    stargazers_url: TValue;
    contributors_url: TValue;
    subscribers_url: TValue;
    subscription_url: TValue;
    commits_url: TValue;
    git_commits_url: TValue;
    comments_url: TValue;
    issue_comment_url: TValue;
    contents_url: TValue;
    compare_url: TValue;
    collaborators_url: TValue;
    merges_url: TValue;
    archive_url: TValue;
    downloads_url: TValue;
    issues_url: TValue;
    pulls_url: TValue;
    milestones_url: TValue;
    notifications_url: TValue;
    labels_url: TValue;
    releases_url: TValue;
    created_at: TValue;
    updated_at: TValue;
    pushed_at: TValue;
    git_url: TValue;
    ssh_url: TValue;
    clone_url: TValue;
    svn_url: TValue;
    homepage: TValue;
    size: TValue;
    stargazers_count: TValue;
    watchers_count: TValue;
    language: TValue;
    has_issues: TValue;
    has_downloads: TValue;
    has_wiki: TValue;
    has_pages: TValue;
    forks_count: TValue;
    mirror_url: TValue;
    open_issues_count: TValue;
    forks: TValue;
    open_issues: TValue;
    watchers: TValue;
    default_branch: TValue;
    &public: TValue;
  end;

  THead = class(TJX4Object)
    &label: TValue;
    ref: TValue;
    sha: TValue;
    user: TUser;
    repo: TRepo;
  end;

  TLinks = class(TJX4Object)
    href: TValue;
  end;

  TPull_Request = class(TJX4Object)
    id: TValue;
    url: TValue;
    html_url: TValue;
    diff_url: TValue;
    patch_url: TValue;
    issue_url: TValue;
    state: TValue;
    number: TValue;
    locked: TValue;
    title: TValue;
    user: TUser;
    body: TValue;
    created_at: TValue;
    updated_at: TValue;
    closed_at: TValue;
    merged_at: TValue;
    merge_commit_sha: TValue;
    assignee: TValue;
    milestone: TValue;
    commits_url: TValue;
    review_comments_url: TValue;
    review_comment_url: TValue;
    comments_url: TValue;
    statuses_url: TValue;
    head: THead;
    base: THead;
    [TJX4Name('_links')]
    links: TJX4Dic<TLinks>;
    merged: TValue;
    mergeable: TValue;
    mergeable_state: TValue;
    merged_by: TValue;
    comments: TValue;
    review_comments: TValue;
    commits: TValue;
    additions: TValue;
    deletions: TValue;
    changed_files: TValue;
  end;

  TIssue = class(TJX4Object)
    url: TValue;
    labels_url: TValue;
    comments_url: TValue;
    events_url: TValue;
    html_url: TValue;
    id: TValue;
    number: TValue;
    title: TValue;
    user: TUser;
    labels: TJX4ValList;
    state: TValue;
    locked: TValue;
    assignee: TValue;
    milestone: TValue;
    comments: TValue;
    created_at: TValue;
    updated_at: TValue;
    closed_at: TValue;
    pull_request: TPull_Request;
    body: TValue;
  end;

  TPage = class(TJX4Object)
    page_name: TValue;
    title: TValue;
    summary: TValue;
    action: TValue;
    sha: TValue;
    html_url: TValue;
  end;

  TPayload = class(TJX4Object)
    ref: TValue;
    ref_type: TValue;
    number: TValue;
    action: TValue;
    master_branch: TValue;
    description: TValue;
    pusher_type: TValue;
    push_id: TValue;
    size: TValue;
    distinct_size: TValue;
    head: TValue;
    before: TValue;
    commits: TJX4List<TCommit>;
    release: TRelease;
    pull_request: TPull_Request;
    issue: TIssue;
    forkee: TRepo;
    pages: TJX4List<TPage>;
    comment: TValue;
    member: TUser;
  end;

  TElement = class(TJX4Object)
    id: TValue;
    &type: TValue;
    actor: TUser;
    repo: TRepo;
    payload: TPayload;
    &public: TValue;
    created_at: TValue;
    org: TOrg;
  end;

  TGitHubExtract = class(TJX4Object)
    GitHub:  TJX4List<TElement>
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
  LGitHubExtract: TGitHubExtract;
  LJsonStr : string;
  LWatch : TStopWatch;
  LJSize: Int64;
begin
  Memo1.Lines.Clear;

  LWatch := TStopWatch.StartNew;
  Memo1.Lines.add( 'Loading GitHub json file :' );
  LJSize := TJX4Object.LoadFromFile('GitHubExtract.json', LJsonStr, TEncoding.UTF8);
  Memo1.Lines.add( Format( '  Stream size: %n MB', [ (LJSize / 1024000) ] ));
  Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));

  Memo1.Lines.add( '' );
  Memo1.Lines.add( 'Convert Json String to JSX4 Objects (Deserialize):' );
  LWatch := TStopWatch.StartNew;
  LGitHubExtract := TJX4Object.FromJSON<TGitHubExtract>(LJsonStr, [ joStats, joRaiseException] );
  Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / 1024000) / (LWatch.ElapsedMilliseconds / 1000)]));

  Memo1.Lines.add( '' );
  Memo1.Lines.add( 'Convert JSX4 Objects to Json String (Serialize):' );
  LWatch := TStopWatch.StartNew;
  LGitHubExtract.ToJSON;
  Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / 1024000) / (LWatch.ElapsedMilliseconds / 1000)]));

  Memo1.Lines.add( '' );
  Memo1.Lines.add( 'Convert JSX4 Objects to YAML String' );
  LWatch := TStopWatch.StartNew;
  LGitHubExtract.ToYAML;
  Memo1.Lines.add(Format('==> %d ms', [ LWatch.ElapsedMilliseconds ]));
  Memo1.Lines.add(Format('==> %n MB/s', [(LJSize / 1024000) / (LWatch.ElapsedMilliseconds / 1000)]));

  Memo1.Lines.add( '' );
  Memo1.Lines.Add(Format('Projects Count: %d', [LGitHubExtract.GitHub.Count]));

  LGitHubExtract.Free;
end;

end.
