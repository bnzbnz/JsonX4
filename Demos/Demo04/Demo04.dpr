program Demo04;

{mode delphi}
uses
  FMX.Forms,
  uDemo04 in 'uDemo04.pas' {Form4},
  uJX4Dict in '..\..\uJsonX4\uJX4Dict.pas',
  uJX4List in '..\..\uJsonX4\uJX4List.pas',
  uJX4Object in '..\..\uJsonX4\uJX4Object.pas',
  uJX4Rtti in '..\..\uJsonX4\uJX4Rtti.pas',
  uJSONableStringList in 'uJSONableStringList.pas' {$R *.res},
  uJX4Value in '..\..\uJsonX4\uJX4Value.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
