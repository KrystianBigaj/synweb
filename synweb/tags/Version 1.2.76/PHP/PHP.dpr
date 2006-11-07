program PHP;

uses
  Forms,
  SysUtils,
  Unit1 in 'Unit1.pas' {Form1},
  TestParser in 'parserbuilder_v0_9_0_213\Output\TestParser.pas',
  TestScanner in 'parserbuilder_v0_9_0_213\Output\TestScanner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
