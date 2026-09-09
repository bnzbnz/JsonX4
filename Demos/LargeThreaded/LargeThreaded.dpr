program LargeThreaded;

{mode delphi}
uses
  ScaleMM2,
  FMX.Forms,
  uJX4Dict in '..\..\uJsonX4\uJX4Dict.pas',
  uJX4List in '..\..\uJsonX4\uJX4List.pas',
  uJX4Object in '..\..\uJsonX4\uJX4Object.pas',
  uJX4Value in '..\..\uJsonX4\uJX4Value.pas',
  uJX4YAML in '..\..\uJsonX4\uJX4YAML.pas',
  uLargeThreaded in 'uLargeThreaded.pas' {Form4},
  uJX4Rtti in '..\..\uJsonX4\uJX4Rtti.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.











