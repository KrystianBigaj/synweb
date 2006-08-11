{$IFNDEF QSYNWEBREG}
unit SynWebReg;
{$ENDIF}

{$I SynWeb.inc}

interface

uses
{$IFDEF SYN_CLX}
  // SynWeb components
  QSynHighlighterWeb,
  QSynHighlighterWebData,
  QSynHighlighterWebMisc,
  QSynTokenMatch,
  QSynEditStrConst,
{$ELSE}
  // SynWeb components   
  SynHighlighterWeb,
  SynHighlighterWebData,
  SynHighlighterWebMisc,
  SynTokenMatch,
  SynEditStrConst,
{$ENDIF}
  Classes;

procedure Register;

implementation

procedure Register;
begin
// SynWeb highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynWebEngine,
    TSynWebHtmlSyn,
    TSynWebCssSyn,
    TSynWebEsSyn,
    TSynWebPhpCliSyn
  ]);
end;

end.
