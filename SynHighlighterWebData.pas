unit SynHighlighterWebData;

interface

uses
{$IFDEF SYN_CLX}
  QSynEditHighlighter;
{$ELSE}
  SynEditHighlighter;
{$ENDIF}

// Global ----------------------------------------------------------------------
type
  THashTable=array[#0..#255] of Longword;

  TSynHighlighterType=(
    shtHtml, shtCss, shtES, shtPHP_inHtml, shtPHP_inCss, shtPHP_inES
    );

  TSynHighlighterTypes=set of TSynHighlighterType;

  TSynHighlighterMode=(
    shmHtml, shmCss, shmES, shmPhp, shmPhpCgi
    );
                           
  TtkTokenKind=(
    // HTML
    tkHtmlSpace, tkHtmlText, tkHtmlEscape, tkHtmlComment, tkHtmlSymbol,
    tkHtmlTag, tkHtmlTagName,tkHtmlTagNameUndef, tkHtmlTagKey,
    tkHtmlTagKeyUndef, tkHtmlTagKeyValue, tkHtmlTagKeyValueQuoted, tkHtmlError,
    // CSS
    tkCssSpace, tkCssSelector, tkCssSelectorUndef, tkCssSelectorClass,
    tkCssSelectorId, tkCssSpecial, tkCssComment, tkCssProp, tkCssPropUndef,
    tkCssVal, tkCssValUndef, tkCssValString, tkCssValNumber, tkCssSymbol,
    tkCssError,
    // ECMAScript
    tkESSpace, tkESIdentifier, tkESKeyword, tkESString, tkESComment, tkESSymbol,
    tkESNumber, tkESError,
    // PHP
    tkPhpSpace, tkPhpIdentifier, tkPhpKeyword, tkPhpFunction, tkPhpVariable,
    tkPhpConst, tkPhpString, tkPhpStringSpecial, tkPhpComment, tkPhpSymbol,
    tkPhpNumber, tkPhpError,
    // Other
    tkNull);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  PIdent2FuncTableFunc = ^TIdent2FuncTableFunc;
  TIdent2FuncTableFunc = function: Boolean of object;

  PTokenAttributeTable=^TTokenAttributeTable;
  TTokenAttributeTable=array[Low(TtkTokenKind)..High(TtkTokenKind)] of
    TSynHighlighterAttributes;

// HTML ------------------------------------------------------------------------
type
  THtmlVersion=(hvHtml401Strict, hvHtml401Transitional, hvHtml401Frameset,
    hvXHtml10Strict, hvXHtml10Transitional, hvXHtml10Frameset);

  THtmlRangeState=(rsHtmlText, rsHtmlComment, rsHtmlCommentClose, rsHtmlTag,
    rsHtmlTagClose, rsHtmlTagDOCTYPE, rsHtmlTagCDATA, rsHtmlTagKey,
    rsHtmlTagKeyEq, rsHtmlTagKeyValue, rsHtmlTagKeyValueQuoted1,
    rsHtmlTagKeyValueQuoted2);

const
  THtmlVersionStr:array[Low(THtmlVersion)..High(THtmlVersion)] of String=(
    'HTML 4.01 Strict',
    'HTML 4.01 Transitional',
    'HTML 4.01 Frameset',
    'XHTML 1.0 Strict',
    'XHTML 1.0 Transitional',
    'XHTML 1.0 Frameset'
    );

// CSS -------------------------------------------------------------------------
type
  TCssVersion=(
    cvCss1, cvCss21
    );

  TCssRangeState=(rsCssRuleset, rsCssSelectorAttrib, rsCssSelectorPseudo,
    rsCssAtKeyword, rsCssProp, rsCssPropVal, rsCssPropValStr, rsCssPropValRgb,
    rsCssPropValFunc, rsCssPropValSpecial, rsCssPropValImportant,
    rsCssPropValUrl, rsCssPropValRect,
    rsCssComment);

const
  TCssRangeState_RulesetBegin=rsCssProp;
  TCssRangeState_RulesetEnd=rsCssPropValRect;

  TCssVersionStr:array[Low(TCssVersion)..High(TCssVersion)] of String=(
    'CSS 1',
    'CSS 2.1'
    );

  TCssString39 = 4;
  TCssString34 = 5;

// ECMAScript ------------------------------------------------------------------
type
  TESRangeState=(rsESDefault, rsESComment, rsESCommentMulti, rsESString34,
    rsESString39);

// PHP -------------------------------------------------------------------------
type
  TPhpVersion=(
    pvPhp4, pvPhp5
    );

  TPhpRangeState=(
    rsPhpSubProc, rsPhpDefault, rsPhpComment, rsPhpString34, rsPhpString39,
    rsPhpStringShell, rsPhpHeredoc
    );

  TPhpOpenTag=(potPhp, potPhpShort, potHTML, potASP);
  TPhpOpenTags=set of TPhpOpenTag;

const
  TPhpVersionStr:array[Low(TPhpVersion)..High(TPhpVersion)] of String=(
{$I SynHighlighterWeb_PhpVersion.inc}
    );

// HTML ------------------------------------------------------------------------
const
  {$I SynHighlighterWeb_Tags.inc}

  {$I SynHighlighterWeb_Attrs.inc}

  {$I SynHighlighterWeb_Special.inc}

// CSS -------------------------------------------------------------------------
const
  {$I SynHighlighterWeb_CssProps.inc}

  {$I SynHighlighterWeb_CssVals.inc}

  {$I SynHighlighterWeb_CssSpecial.inc}

// ECAMScript ------------------------------------------------------------------
const
  {$I SynHighlighterWeb_ESKeywords.inc}

// PHP -------------------------------------------------------------------------
const
{$I SynHighlighterWeb_PhpKeywords.inc}

// Global ----------------------------------------------------------------------
const
{$I SynHighlighterWeb_Tables.inc}

  TCrc8_Table:array[$00..$FF] of Byte=(
    $00, $07, $0e, $09, $1c, $1b, $12, $15,
    $38, $3f, $36, $31, $24, $23, $2a, $2d,
    $70, $77, $7e, $79, $6c, $6b, $62, $65,
    $48, $4f, $46, $41, $54, $53, $5a, $5d,
    $e0, $e7, $ee, $e9, $fc, $fb, $f2, $f5,
    $d8, $df, $d6, $d1, $c4, $c3, $ca, $cd,
    $90, $97, $9e, $99, $8c, $8b, $82, $85,
    $a8, $af, $a6, $a1, $b4, $b3, $ba, $bd,
    $c7, $c0, $c9, $ce, $db, $dc, $d5, $d2,
    $ff, $f8, $f1, $f6, $e3, $e4, $ed, $ea,
    $b7, $b0, $b9, $be, $ab, $ac, $a5, $a2,
    $8f, $88, $81, $86, $93, $94, $9d, $9a,
    $27, $20, $29, $2e, $3b, $3c, $35, $32,
    $1f, $18, $11, $16, $03, $04, $0d, $0a,
    $57, $50, $59, $5e, $4b, $4c, $45, $42,
    $6f, $68, $61, $66, $73, $74, $7d, $7a,
    $89, $8e, $87, $80, $95, $92, $9b, $9c,
    $b1, $b6, $bf, $b8, $ad, $aa, $a3, $a4,
    $f9, $fe, $f7, $f0, $e5, $e2, $eb, $ec,
    $c1, $c6, $cf, $c8, $dd, $da, $d3, $d4,
    $69, $6e, $67, $60, $75, $72, $7b, $7c,
    $51, $56, $5f, $58, $4d, $4a, $43, $44,
    $19, $1e, $17, $10, $05, $02, $0b, $0c,
    $21, $26, $2f, $28, $3d, $3a, $33, $34,
    $4e, $49, $40, $47, $52, $55, $5c, $5b,
    $76, $71, $78, $7f, $6a, $6d, $64, $63,
    $3e, $39, $30, $37, $22, $25, $2c, $2b,
    $06, $01, $08, $0f, $1a, $1d, $14, $13,
    $ae, $a9, $a0, $a7, $b2, $b5, $bc, $bb,
    $96, $91, $98, $9f, $8a, $8d, $84, $83,
    $de, $d9, $d0, $d7, $c2, $c5, $cc, $cb,
    $e6, $e1, $e8, $ef, $fa, $fd, $f4, $f3
    );

implementation

end.
