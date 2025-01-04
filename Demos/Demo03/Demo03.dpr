program Demo03;

{mode delphi}
uses
  FMX.Forms,
  uDemo03 in 'uDemo03.pas' {Form4},
  uJX4Dict in '..\..\uJsonX4\uJX4Dict.pas',
  uJX4List in '..\..\uJsonX4\uJX4List.pas',
  uJX4Object in '..\..\uJsonX4\uJX4Object.pas',
  uJX4Rtti in '..\..\uJsonX4\uJX4Rtti.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
