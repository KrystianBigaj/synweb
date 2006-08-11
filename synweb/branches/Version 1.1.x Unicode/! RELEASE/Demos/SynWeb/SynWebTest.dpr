program SynWebTest;

{%ToDo 'SynWebTest.todo'}
{%File '..\!Docs\SynHighlighterHtmlMulti-Range.txt'}

uses
  Forms,
  Main in 'Main.pas' {Form1},
  SynHighlighterWeb in '..\SynHighlighterWeb.pas',
  SynHighlighterWebData in '..\SynHighlighterWebData.pas',
  SynHighlighterWebMisc in '..\SynHighlighterWebMisc.pas',
  SynTokenMatch in '..\..\SynTokenMatch\SynTokenMatch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
