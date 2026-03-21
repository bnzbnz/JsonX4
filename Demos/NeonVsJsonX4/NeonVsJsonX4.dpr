program NeonVsJsonX4;

{mode delphi}
uses
  FMX.Forms,
  uNeonVsJsonX4 in 'uNeonVsJsonX4.pas',
  uJX4Dict in '..\..\uJsonX4\uJX4Dict.pas',
  uJX4List in '..\..\uJsonX4\uJX4List.pas',
  uJX4Object in '..\..\uJsonX4\uJX4Object.pas',
  uJX4Rtti in '..\..\uJsonX4\uJX4Rtti.pas',
  uJX4Value in '..\..\uJsonX4\uJX4Value.pas',
  uJX4YAML in '..\..\uJsonX4\uJX4YAML.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
