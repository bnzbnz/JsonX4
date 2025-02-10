program Demo08;

{mode delphi}
uses
  FMX.Forms,
  uJX4Dict in '..\..\uJsonX4\uJX4Dict.pas',
  uJX4YAML in '..\..\uJsonX4\uJX4YAML.pas',
  uJX4List in '..\..\uJsonX4\uJX4List.pas',
  uJX4Object in '..\..\uJsonX4\uJX4Object.pas',
  uJX4Rtti in '..\..\uJsonX4\uJX4Rtti.pas',
  uDemo08 in 'uDemo08.pas' {Form4},
  uJX4Value in '..\..\uJsonX4\uJX4Value.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
