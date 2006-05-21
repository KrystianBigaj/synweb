{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterWeb.pas,v 1.0 2005/05/21 00:00:00 flatdev Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
- TSynWebSyn support only single line SetLine (don't use more than one line).
- Doesn't support #13#10, #10 or #13 as new line. Always use #0 as line break.
- PHP: Doesn't support multi-line encapsuled strings in string, only single line:
  eg. "somestring {$a["some array{$b['key'].... <- only single line encapsuled values
-------------------------------------------------------------------------------}
{
@abstract(Provides an web-files (Multi Html/Css/ECAMScript/Php) highlighter for SynEdit
@author(FlatDev <flatdev@mail.ru>)
@created(2005-05-21)
@lastmod(2006-02-10)
The TSynWebSyn unit provides SynEdit with an Multi Html/Css/ECAMScript/Php highlighter.
}

 // SYNWEB_FIXNULL - fix lines containing #0 character (#0 goes into #32)
 {.$DEFINE SYNWEB_FIXNULL}

{$IFNDEF QSYNHIGHLIGHTERWEB}
unit SynHighlighterWeb;

{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  Classes,
  SysUtils,
  SynHighlighterWebData;

// Highlighter -----------------------------------------------------------------

type
  TSynWebEngine = class;

  PSynWebConfig = ^TSynWebConfig;

  TSynWebConfig = record
    FRun: longint;
    FRange: longword;
    FLine: PChar;
    FLineRef: string;
    FLineNumber: integer;
    FToken_LastID: integer;
    FTokenPos: integer;
    FTokenID: TtkTokenKind;
    FStringLen, FStringLenClean: integer;
    FToIdent: PChar;
    FHashTable: THashTable;
    FNextClearBits: boolean;
    FNextUseNextAH: boolean;
    FUseNextAH: boolean;
    FHighlighterType, FPrevHighlighterType, FNextHighlighterType: TSynHighlighterType;
    FHighlighterSW: boolean;
    FHighlighterMode: TSynHighlighterMode;
    FCssMask: longword;
    FNextProcTable: TProcTableProc;
    
    FHtmlVersion: THtmlVersion;
    FCssVersion: TCssVersion;
    FPhpVersion: TPhpVersion;
    FPhpShortOpenTag: boolean;
    FPhpAspTags: boolean;

    FPhpEmbeded: boolean;
    FCssEmbeded: boolean;
    FEsEmbeded: boolean;

    FSYN_ATTR_COMMENT: TSynHighlighterAttributes;
    FSYN_ATTR_STRING: TSynHighlighterAttributes;
    FSYN_ATTR_WHITESPACE: TSynHighlighterAttributes;
  end;

  TSynWebBase = class(TSynCustomHighlighter)
  private
    FConfig: TSynWebConfig;
    FEngine: TSynWebEngine;
    FActiveHighlighter: boolean;
    FActiveHighlighters: TSynHighlighterTypes;

    function GetHtmlVersion: THtmlVersion;
    procedure SetCssVersion(const Value: TCssVersion);
    function GetCssVersion: TCssVersion;
    procedure SetHtmlVersion(const Value: THtmlVersion);
    function GetPhpVersion: TPhpVersion;
    procedure SetPhpVersion(const Value: TPhpVersion);
    function GetPhpAspTags: boolean;
    procedure SetPhpAspTags(const Value: boolean);
    function GetPhpShortOpenTag: boolean;
    procedure SetPhpShortOpenTag(const Value: boolean);
    function GetCssEmbeded: boolean;
    procedure SetCssEmbeded(const Value: boolean);
    function GetEsEmbeded: boolean;
    procedure SetEsEmbeded(const Value: boolean);
    function GetPhpEmbeded: boolean;
    procedure SetPhpEmbeded(const Value: boolean);

    procedure SetActiveHighlighter(const Value: boolean);
    procedure SetupActiveHighlighter; virtual; abstract;
    function GetActiveHighlighters: TSynHighlighterTypes;
    procedure SetEngine(const Value: TSynWebEngine);
  protected
    procedure DoDefHighlightChange;
    function GetAttribCount: integer; override;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; override;
    function GetIdentChars: TSynIdentChars; override;

    property ActiveSwitchHighlighter: boolean
      read FActiveHighlighter write SetActiveHighlighter;
    property ActiveHighlighters: TSynHighlighterTypes read GetActiveHighlighters;
    function UpdateActiveHighlighter(ARange: Pointer; ALine: string;
      ACaretX, ACaretY: integer): boolean;

    property HtmlVersion: THtmlVersion read GetHtmlVersion write SetHtmlVersion;
    property CssVersion: TCssVersion read GetCssVersion write SetCssVersion;
    property PhpVersion: TPhpVersion read GetPhpVersion write SetPhpVersion;
    property PhpShortOpenTag: boolean read GetPhpShortOpenTag write SetPhpShortOpenTag;
    property PhpAspTags: boolean read GetPhpAspTags write SetPhpAspTags;

    property CssEmbeded: boolean read GetCssEmbeded write SetCssEmbeded;
    property PhpEmbeded: boolean read GetPhpEmbeded write SetPhpEmbeded;
    property EsEmbeded: boolean read GetEsEmbeded write SetEsEmbeded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenLen: integer;
    function GetTokenPos: integer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetRange: Pointer; override;
    function GetEol: boolean; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(NewValue: string; LineNumber: integer); override;
    procedure Next; override;
  published
    property Engine: TSynWebEngine read FEngine write SetEngine;
  end;

  TSynWebHtmlSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property ActiveSwitchHighlighter;
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property CssEmbeded;
    property PhpEmbeded;
    property EsEmbeded;
  end;

  TSynWebPHPCliSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
  end;

  TSynWebCSSSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property ActiveSwitchHighlighter;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
  end;

  TSynWebESSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property ActiveSwitchHighlighter;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
  end;

  TSynWebEngine = class(TComponent)
  private
    // Global ------------------------------------------------------------------
    FNotifyList: TList;
    FConfig: PSynWebConfig;
    fAttributes: TStringList;
    fInactiveAttri: TSynHighlighterAttributes;
    fTokenAttributeTable: TTokenAttributeTable;

    // HTML --------------------------------------------------------------------
    fHtml_TagIdentFuncTable: array[0..Html_TagMaxKeyHash] of TIdentFuncTableFunc;
    fHtml_AttrIdentFuncTable: array[0..Html_AttrMaxKeyHash] of TIdentFuncTableFunc;
    fHtml_SpecialIdentFuncTable: array[0..Html_SpecialMaxKeyHash] of
    TIdent2FuncTableFunc;
    fHtml_RangeProcTable: array[Low(THtmlRangeState)..High(THtmlRangeState)] of
    TProcTableProc;

    fHtml_WhitespaceAttri: TSynHighlighterAttributes;
    fHtml_CommentAttri: TSynHighlighterAttributes;
    fHtml_TextAttri: TSynHighlighterAttributes;
    fHtml_EscapeAmpsAttri: TSynHighlighterAttributes;
    fHtml_SymbolAttri: TSynHighlighterAttributes;
    fHtml_TagAttri: TSynHighlighterAttributes;
    fHtml_TagNameAttri: TSynHighlighterAttributes;
    fHtml_TagNameUndefAttri: TSynHighlighterAttributes;
    fHtml_TagKeyAttri: TSynHighlighterAttributes;
    fHtml_TagKeyUndefAttri: TSynHighlighterAttributes;
    fHtml_TagKeyValueAttri: TSynHighlighterAttributes;
    fHtml_TagKeyValueQuotedAttri: TSynHighlighterAttributes;
    fHtml_ErrorAttri: TSynHighlighterAttributes;

    // CSS ---------------------------------------------------------------------
    fCss_ProcTable: array[#0..#255] of TProcTableProc;
    fCss_PropIdentFuncTable: array[0..Css_PropMaxKeyHash] of TIdentFuncTableFunc;
    fCss_ValIdentFuncTable: array[0..Css_ValMaxKeyHash] of TIdentFuncTableFunc;
    fCss_SpecialIdentFuncTable: array[0..Css_SpecialMaxKeyHash] of
    TIdent2FuncTableFunc;
    fCss_RangeProcTable: array[Low(TCssRangeState)..High(TCssRangeState)] of
    TProcTableProc;

    fCss_WhitespaceAttri: TSynHighlighterAttributes;
    fCss_RulesetWhitespaceAttri: TSynHighlighterAttributes;
    fCss_SelectorAttri: TSynHighlighterAttributes;
    fCss_SelectorUndefAttri: TSynHighlighterAttributes;
    fCss_SelectorClassAmpsAttri: TSynHighlighterAttributes;
    fCss_SelectorIdAttri: TSynHighlighterAttributes;
    fCss_SpecialAttri: TSynHighlighterAttributes;
    fCss_CommentAttri: TSynHighlighterAttributes;
    fCss_PropAttri: TSynHighlighterAttributes;
    fCss_PropUndefAttri: TSynHighlighterAttributes;
    fCss_ValAttri: TSynHighlighterAttributes;
    fCss_ValUndefAttri: TSynHighlighterAttributes;
    fCss_ValStringAttri: TSynHighlighterAttributes;
    fCss_ValNumberAttri: TSynHighlighterAttributes;
    fCss_SymbolAttri: TSynHighlighterAttributes;
    fCss_ErrorAttri: TSynHighlighterAttributes;

    // ECMAScript --------------------------------------------------------------
    fES_ProcTable: array[#0..#255] of TProcTableProc;
    fES_IdentFuncTable: array[0..ES_KeywordsMaxKeyHash] of TIdentFuncTableFunc;
    fES_RangeProcTable: array[Low(TESRangeState)..High(TESRangeState)] of
    TProcTableProc;

    fES_WhitespaceAttri: TSynHighlighterAttributes;
    fES_IdentifierAttri: TSynHighlighterAttributes;
    fES_KeyAttri: TSynHighlighterAttributes;
    fES_CommentAttri: TSynHighlighterAttributes;
    fES_StringAttri: TSynHighlighterAttributes;
    fES_NumberAttri: TSynHighlighterAttributes;
    fES_SymbolAttri: TSynHighlighterAttributes;
    fES_ErrorAttri: TSynHighlighterAttributes;

    // PHP ---------------------------------------------------------------------
    fPhp_ProcTable: array[#0..#255] of TProcTableProc;
    fPhp_IdentFuncTable: array[0..Php_KeywordsMaxKeyHash] of TIdentFuncTableFunc;
    fPhp_RangeProcTable: array[Low(TPhpRangeState)..High(TPhpRangeState)] of
    TProcTableProc;

    fPhp_WhitespaceAttri: TSynHighlighterAttributes;
    fPhp_IdentifierAttri: TSynHighlighterAttributes;
    fPhp_KeyAttri: TSynHighlighterAttributes;
    fPhp_FunctionAttri: TSynHighlighterAttributes;
    fPhp_VariableAttri: TSynHighlighterAttributes;
    fPhp_ConstAttri: TSynHighlighterAttributes;
    fPhp_StringAttri: TSynHighlighterAttributes;
    fPhp_StringSpecialAttri: TSynHighlighterAttributes;
    fPhp_CommentAttri: TSynHighlighterAttributes;
    fPhp_SymbolAttri: TSynHighlighterAttributes;
    fPhp_NumberAttri: TSynHighlighterAttributes;
    fPhp_ErrorAttri: TSynHighlighterAttributes;

    // HTML --------------------------------------------------------------------
    procedure Html_MakeMethodTables;
    procedure Html_Next;
    function Html_GetRange: THtmlRangeState;
    procedure Html_SetRange(const ARange: THtmlRangeState);
    function Html_GetTag: integer;
    procedure Html_SetTag(const ATag: integer);
    function Html_CheckNull(ADo: boolean = True): boolean;

    procedure Html_SpaceProc;
    procedure Html_AmpersandProc;
    procedure Html_BraceOpenProc;
    procedure Html_ErrorProc;

    procedure Html_RangeTextProc;
    procedure Html_RangeCommentProc;
    procedure Html_RangeCommentCloseProc;
    procedure Html_RangeTagDOCTYPEProc;
    procedure Html_RangeTagCDATAProc;
    procedure Html_RangeTagProc;
    procedure Html_RangeTagCloseProc;
    procedure Html_RangeTagKeyProc;
    procedure Html_RangeTagKeyEqProc;
    procedure Html_RangeTagKeyValueProc;
    procedure Html_RangeTagKeyValueQuoted1Proc;
    procedure Html_RangeTagKeyValueQuoted2Proc;

    function Html_TagKeyComp(const ID: integer): boolean;
    function Html_TagCheck: TtkTokenKind;
    {$I SynHighlighterWeb_TagsFuncList.inc}

    function Html_AttrKeyComp(const ID: integer): boolean;
    function Html_AttrCheck: TtkTokenKind;
    {$I SynHighlighterWeb_AttrsFuncList.inc}

    function Html_SpecialKeyComp(const ID: integer): boolean;
    function Html_SpecialCheck(AStart, ALen: integer): integer;
    {$I SynHighlighterWeb_SpecialFuncList.inc}

    // CSS ---------------------------------------------------------------------
    procedure Css_MakeMethodTables;
    procedure Css_NextBg;
    procedure Css_Next;
    procedure Css_UpdateBg;
    function Css_GetRange: TCssRangeState;
    procedure Css_SetRange(const ARange: TCssRangeState);
    function Css_GetProp: integer;
    procedure Css_SetProp(const AProp: integer);
    function Css_CheckNull(ADo: boolean = True): boolean;

    procedure Css_SpaceProc;
    procedure Css_AtKeywordProc;
    procedure Css_SlashProc;
    procedure Css_BraceOpenProc;
    procedure Css_CurlyBraceOpenProc;
    procedure Css_CurlyBraceCloseProc;
    procedure Css_ChildAnySelectorProc;
    procedure Css_AttribProc;
    procedure Css_HashProc;
    procedure Css_DotProc;
    procedure Css_CommaProc;
    procedure Css_ColonProc;
    procedure Css_SemiColonProc;
    procedure Css_ExclamationProc;
    procedure Css_StringProc;
    procedure Css_PlusProc;
    procedure Css_MinusProc;
    procedure Css_NumberProc;
    procedure Css_NumberDefProc;
    procedure Css_IdentProc;
    function Css_IdentStartProc: boolean;
    function Css_CustomStringProc(AShl: longword; ADo: boolean = True): boolean;
    function Css_NotWhitespace: boolean;
    procedure Css_SymbolProc;
    procedure Css_ErrorProc;

    procedure Css_RangeRulesetProc;
    procedure Css_RangeSelectorAttribProc;
    procedure Css_RangeSelectorPseudoProc;
    procedure Css_RangeAtKeywordProc;
    procedure Css_RangePropProc;
    procedure Css_RangePropValProc;
    procedure Css_RangePropValStrProc;
    procedure Css_RangePropValRgbProc;
    procedure Css_RangePropValFuncProc;
    procedure Css_RangePropValSpecialProc;
    procedure Css_RangePropValImportantProc;
    procedure Css_RangePropValUrlProc;
    procedure Css_RangePropValRectProc;
    procedure Css_RangeCommentProc;

    function Css_PropKeyComp(const ID: integer): boolean;
    function Css_PropCheck: TtkTokenKind;
    {$I SynHighlighterWeb_CssPropsFuncList.inc}

    function Css_ValKeyComp(const ID: integer): boolean;
    function Css_ValCheck: TtkTokenKind;
    {$I SynHighlighterWeb_CssValsFuncList.inc}

    function Css_SpecialKeyComp(const ID: integer): boolean;
    function Css_SpecialCheck(AStart, ALen: integer): integer;
    {$I SynHighlighterWeb_CssSpecialFuncList.inc}

    // ECMAScript --------------------------------------------------------------
    procedure ES_MakeMethodTables;
    procedure ES_Next;
    function ES_GetRange: TESRangeState;
    procedure ES_SetRange(const ARange: TESRangeState);
    function ES_CheckNull(ADo: boolean = True): boolean;

    procedure ES_SpaceProc;
    procedure ES_SlashProc;
    procedure ES_LowerProc;
    procedure ES_EqualNotProc;
    procedure ES_GreaterProc;
    procedure ES_AndProc;
    procedure ES_PlusProc;
    procedure ES_MinusProc;
    procedure ES_OrProc;
    procedure ES_MulModXorProc;
    procedure ES_NumberProc;
    procedure ES_String34Proc;
    procedure ES_String39Proc;
    procedure ES_SymbolProc;
    procedure ES_IdentProc;
    procedure ES_ErrorProc;

    procedure ES_RangeDefaultProc;
    procedure ES_RangeCommentProc;
    procedure ES_RangeCommentMultiProc;
    procedure ES_RangeString34Proc;
    procedure ES_RangeString39Proc;

    function ES_KeywordComp(const ID: integer): boolean;

    function ES_IdentCheck: TtkTokenKind;
    {$I SynHighlighterWeb_ESKeywordsFuncList.inc}

    // PHP ---------------------------------------------------------------------
    procedure Php_MakeMethodTables;
    procedure Php_Next;
    function Php_GetRange: TPhpRangeState;
    procedure Php_SetRange(const ARange: TPhpRangeState);
    function Php_GetOpenTag: TPhpOpenTag;
    procedure Php_SetOpenTag(APhpOpenTag: TPhpOpenTag);

    function Php_CheckBegin(ABegin: boolean = True): boolean;
    procedure Php_Begin(ATagKind: TPhpOpenTag);
    procedure Php_End;

    procedure Php_SpaceProc;
    procedure Php_QuestionProc;
    procedure Php_NumberProc;
    function Php_CheckNumberProc: boolean;
    procedure Php_String34Proc;
    procedure Php_String39Proc;
    procedure Php_StringShellProc;
    procedure Php_AndProc;
    procedure Php_OrProc;
    procedure Php_AtSymbolProc;
    procedure Php_EqualProc;
    procedure Php_GreaterProc;
    procedure Php_LowerProc;
    procedure Php_PlusProc;
    procedure Php_MinusProc;
    procedure Php_MulDivModXorProc;
    procedure Php_SlashProc;
    procedure Php_PercentProc;
    procedure Php_HashProc;
    procedure Php_NotProc;
    procedure Php_DotProc;
    procedure Php_SymbolProc;
    procedure Php_VarProc;
    procedure Php_IdentProc;
    procedure Php_ErrorProc;
    function Php_DoStringDouble(AIsHeredoc: boolean = False): boolean;

    procedure Php_RangeTagProc;
    procedure Php_RangeDefaultProc;
    procedure Php_RangeCommentProc;
    procedure Php_RangeString34Proc;
    procedure Php_RangeString39Proc;
    procedure Php_RangeStringShellProc;
    procedure Php_RangeHeredocProc;

    function Php_KeywordComp(const ID: integer): boolean;
    function Php_ConstComp: boolean;
    function Php_FunctionComp(const ID: integer): boolean;

    function Php_IdentCheck: TtkTokenKind;
    {$I SynHighlighterWeb_PhpKeywordsFuncList.inc}

    // Other -------------------------------------------------------------------
    procedure AddAttribute(AAttrib: TSynHighlighterAttributes);
    procedure AddToNotifyList(ASynWeb: TSynWebBase);
    procedure RemoveFromNotifyList(ASynWeb: TSynWebBase);

    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure DefHighlightChange(Sender: TObject);

    function GetCRC8_String(AString: string): byte;
    function GetRange_Bit(ABit: longword): boolean;
    procedure SetRange_Bit(ABit: longword; AVal: boolean);
    function GetRange_Int(ALen, APos: longword): longword;
    procedure SetRange_Int(ALen, APos, AVal: longword);

    procedure NullProc;
    procedure NextSetHighlighterType;
    procedure SetHighlighterType(const AHighlighterType: TSynHighlighterType;
      AClearBits: boolean; ASetAtNextToken: boolean; AUseNextAH: boolean);
    procedure SetupHighlighterType(AClearBits: boolean = False);
    procedure SetLine(NewValue: string; LineNumber: integer);
    procedure Next;
    function GetToken: string;
    function GetTokenLen: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Global
    property InactiveAttri: TSynHighlighterAttributes
      read fInactiveAttri write fInactiveAttri;

    // HTML
    property HtmlWhitespaceAttri: TSynHighlighterAttributes
      read fHtml_WhitespaceAttri write fHtml_WhitespaceAttri;
    property HtmlCommentAttri: TSynHighlighterAttributes
      read fHtml_CommentAttri write fHtml_CommentAttri;
    property HtmlTextAttri: TSynHighlighterAttributes
      read fHtml_TextAttri write fHtml_TextAttri;
    property HtmlEscapeAttri: TSynHighlighterAttributes
      read fHtml_EscapeAmpsAttri write fHtml_EscapeAmpsAttri;
    property HtmlSymbolAttri: TSynHighlighterAttributes
      read fHtml_SymbolAttri write fHtml_SymbolAttri;
    property HtmlTagAttri: TSynHighlighterAttributes
      read fHtml_TagAttri write fHtml_TagAttri;
    property HtmlTagNameAttri: TSynHighlighterAttributes
      read fHtml_TagNameAttri write fHtml_TagNameAttri;
    property HtmlTagNameUndefAttri: TSynHighlighterAttributes
      read fHtml_TagNameUndefAttri write fHtml_TagNameUndefAttri;
    property HtmlTagKeyAttri: TSynHighlighterAttributes
      read fHtml_TagKeyAttri write fHtml_TagKeyAttri;
    property HtmlTagKeyUndefAttri: TSynHighlighterAttributes
      read fHtml_TagKeyUndefAttri write fHtml_TagKeyUndefAttri;
    property HtmlTagKeyValueAttri: TSynHighlighterAttributes
      read fHtml_TagKeyValueAttri write fHtml_TagKeyValueAttri;
    property HtmlTagKeyValueQuotedAttri: TSynHighlighterAttributes
      read fHtml_TagKeyValueQuotedAttri write fHtml_TagKeyValueQuotedAttri;
    property HtmlErrorAttri: TSynHighlighterAttributes
      read fHtml_ErrorAttri write fHtml_ErrorAttri;

    // CSS
    property CssWhitespaceAttri: TSynHighlighterAttributes
      read fCss_WhitespaceAttri write fCss_WhitespaceAttri;
    property CssRulesetWhitespaceAttri: TSynHighlighterAttributes
      read fCss_RulesetWhitespaceAttri write fCss_RulesetWhitespaceAttri;
    property CssSelectorAttri: TSynHighlighterAttributes
      read fCss_SelectorAttri write fCss_SelectorAttri;
    property CssSelectorUndefAttri: TSynHighlighterAttributes
      read fCss_SelectorUndefAttri write fCss_SelectorUndefAttri;
    property CssSelectorClassAttri: TSynHighlighterAttributes
      read fCss_SelectorClassAmpsAttri write fCss_SelectorClassAmpsAttri;
    property CssSelectorIdAttri: TSynHighlighterAttributes
      read fCss_SelectorIdAttri write fCss_SelectorIdAttri;
    property CssSpecialAttri: TSynHighlighterAttributes
      read fCss_SpecialAttri write fCss_SpecialAttri;
    property CssCommentAttri: TSynHighlighterAttributes
      read fCss_CommentAttri write fCss_CommentAttri;
    property CssPropAttri: TSynHighlighterAttributes
      read fCss_PropAttri write fCss_PropAttri;
    property CssPropUndefAttri: TSynHighlighterAttributes
      read fCss_PropUndefAttri write fCss_PropUndefAttri;
    property CssValAttri: TSynHighlighterAttributes
      read fCss_ValAttri write fCss_ValAttri;
    property CssValUndefAttri: TSynHighlighterAttributes
      read fCss_ValUndefAttri write fCss_ValUndefAttri;
    property CssValStringAttri: TSynHighlighterAttributes
      read fCss_ValStringAttri write fCss_ValStringAttri;
    property CssValNumberAttri: TSynHighlighterAttributes
      read fCss_ValNumberAttri write fCss_ValNumberAttri;
    property CssSymbolAttri: TSynHighlighterAttributes
      read fCss_SymbolAttri write fCss_SymbolAttri;
    property CssErrorAttri: TSynHighlighterAttributes
      read fCss_ErrorAttri write fCss_ErrorAttri;

    // ECMAScript
    property ESWhitespaceAttri: TSynHighlighterAttributes
      read fES_WhitespaceAttri write fES_WhitespaceAttri;
    property ESIdentifierAttri: TSynHighlighterAttributes
      read fES_IdentifierAttri write fES_IdentifierAttri;
    property ESKeyAttri: TSynHighlighterAttributes read fES_KeyAttri write fES_KeyAttri;
    property ESCommentAttri: TSynHighlighterAttributes
      read fES_CommentAttri write fES_CommentAttri;
    property ESStringAttri: TSynHighlighterAttributes
      read fES_StringAttri write fES_StringAttri;
    property ESNumberAttri: TSynHighlighterAttributes
      read fES_NumberAttri write fES_NumberAttri;
    property ESSymbolAttri: TSynHighlighterAttributes
      read fES_SymbolAttri write fES_SymbolAttri;
    property ESErrorAttri: TSynHighlighterAttributes
      read fES_ErrorAttri write fES_ErrorAttri;

    // PHP
    property PhpWhitespaceAttri: TSynHighlighterAttributes
      read fPhp_WhitespaceAttri write fPhp_WhitespaceAttri;
    property PhpIdentifierAttri: TSynHighlighterAttributes
      read fPhp_IdentifierAttri write fPhp_IdentifierAttri;
    property PhpKeyAttri: TSynHighlighterAttributes
      read fPhp_KeyAttri write fPhp_KeyAttri;
    property PhpFunctionAttri: TSynHighlighterAttributes
      read fPhp_FunctionAttri write fPhp_FunctionAttri;
    property PhpVariableAttri: TSynHighlighterAttributes
      read fPhp_VariableAttri write fPhp_VariableAttri;
    property PhpConstAttri: TSynHighlighterAttributes
      read fPhp_ConstAttri write fPhp_ConstAttri;
    property PhpStringAttri: TSynHighlighterAttributes
      read fPhp_StringAttri write fPhp_StringAttri;
    property PhpStringSpecialAttri: TSynHighlighterAttributes
      read fPhp_StringSpecialAttri write fPhp_StringSpecialAttri;
    property PhpCommentAttri: TSynHighlighterAttributes
      read fPhp_CommentAttri write fPhp_CommentAttri;
    property PhpSymbolAttri: TSynHighlighterAttributes
      read fPhp_SymbolAttri write fPhp_SymbolAttri;
    property PhpNumberAttri: TSynHighlighterAttributes
      read fPhp_NumberAttri write fPhp_NumberAttri;
    property PhpErrorAttri: TSynHighlighterAttributes
      read fPhp_ErrorAttri write fPhp_ErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst, StrUtils;
{$ELSE}
  SynEditStrConst;

{$ENDIF}

{ TSynWebEngine }

constructor TSynWebEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifyList:=TList.Create;
  
  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupError;
  fAttributes.Sorted := True;

  // HTML
  Html_MakeMethodTables;

  fHtml_WhitespaceAttri := TSynHighlighterAttributes.Create('Html: Whitespace');
  AddAttribute(fHtml_WhitespaceAttri);
  fHtml_CommentAttri := TSynHighlighterAttributes.Create('Html: Comment');
  AddAttribute(fHtml_CommentAttri);
  fHtml_TextAttri := TSynHighlighterAttributes.Create('Html: Text');
  AddAttribute(fHtml_TextAttri);
  fHtml_EscapeAmpsAttri := TSynHighlighterAttributes.Create('Html: Escaped amps');
  AddAttribute(fHtml_EscapeAmpsAttri);
  fHtml_SymbolAttri := TSynHighlighterAttributes.Create('Html: Symbol');
  AddAttribute(fHtml_SymbolAttri);
  fHtml_TagAttri := TSynHighlighterAttributes.Create('Html: Tag');
  AddAttribute(fHtml_TagAttri);
  fHtml_TagNameAttri := TSynHighlighterAttributes.Create('Html: Tag name');
  AddAttribute(fHtml_TagNameAttri);
  fHtml_TagNameUndefAttri := TSynHighlighterAttributes.Create(
    'Html: Undefined tag name');
  AddAttribute(fHtml_TagNameUndefAttri);
  fHtml_TagKeyAttri := TSynHighlighterAttributes.Create('Html: Key');
  AddAttribute(fHtml_TagKeyAttri);
  fHtml_TagKeyUndefAttri := TSynHighlighterAttributes.Create('Html: Undefined key');
  AddAttribute(fHtml_TagKeyUndefAttri);
  fHtml_TagKeyValueAttri := TSynHighlighterAttributes.Create('Html: Value');
  AddAttribute(fHtml_TagKeyValueAttri);
  fHtml_TagKeyValueQuotedAttri := TSynHighlighterAttributes.Create('Html: Quoted value');
  AddAttribute(fHtml_TagKeyValueQuotedAttri);
  fHtml_ErrorAttri := TSynHighlighterAttributes.Create('Html: Error');
  AddAttribute(fHtml_ErrorAttri);

  fTokenAttributeTable[tkHtmlSpace] := fHtml_WhitespaceAttri;
  fTokenAttributeTable[tkHtmlComment] := fHtml_CommentAttri;
  fTokenAttributeTable[tkHtmlText] := fHtml_TextAttri;
  fTokenAttributeTable[tkHtmlEscape] := fHtml_EscapeAmpsAttri;
  fTokenAttributeTable[tkHtmlSymbol] := fHtml_SymbolAttri;
  fTokenAttributeTable[tkHtmlTag] := fHtml_TagAttri;
  fTokenAttributeTable[tkHtmlTagName] := fHtml_TagNameAttri;
  fTokenAttributeTable[tkHtmlTagNameUndef] := fHtml_TagNameUndefAttri;
  fTokenAttributeTable[tkHtmlTagKey] := fHtml_TagKeyAttri;
  fTokenAttributeTable[tkHtmlTagKeyUndef] := fHtml_TagKeyUndefAttri;
  fTokenAttributeTable[tkHtmlTagKeyValue] := fHtml_TagKeyValueAttri;
  fTokenAttributeTable[tkHtmlTagKeyValueQuoted] := fHtml_TagKeyValueQuotedAttri;
  fTokenAttributeTable[tkHtmlError] := fHtml_ErrorAttri;

  // CSS
  Css_MakeMethodTables;

  fCss_WhitespaceAttri := TSynHighlighterAttributes.Create('Css: Whitespace');
  AddAttribute(fCss_WhitespaceAttri);
  fCss_RulesetWhitespaceAttri :=
    TSynHighlighterAttributes.Create('Css: Ruleset whitespace');
  AddAttribute(fCss_RulesetWhitespaceAttri);
  fCss_SelectorAttri := TSynHighlighterAttributes.Create('Css: Selector');
  AddAttribute(fCss_SelectorAttri);
  fCss_SelectorUndefAttri := TSynHighlighterAttributes.Create('Css: Undefined selector');
  AddAttribute(fCss_SelectorUndefAttri);
  fCss_SelectorClassAmpsAttri := TSynHighlighterAttributes.Create('Css: Class selector');
  AddAttribute(fCss_SelectorClassAmpsAttri);
  fCss_SelectorIdAttri := TSynHighlighterAttributes.Create('Css: Id selector');
  AddAttribute(fCss_SelectorIdAttri);
  fCss_SpecialAttri := TSynHighlighterAttributes.Create('Css: Special');
  AddAttribute(fCss_SpecialAttri);
  fCss_CommentAttri := TSynHighlighterAttributes.Create('Css: Comment');
  AddAttribute(fCss_CommentAttri);
  fCss_PropAttri := TSynHighlighterAttributes.Create('Css: Property');
  AddAttribute(fCss_PropAttri);
  fCss_PropUndefAttri := TSynHighlighterAttributes.Create('Css: Undefined property');
  AddAttribute(fCss_PropUndefAttri);
  fCss_ValAttri := TSynHighlighterAttributes.Create('Css: Value');
  AddAttribute(fCss_ValAttri);
  fCss_ValUndefAttri := TSynHighlighterAttributes.Create('Css: Undefined value');
  AddAttribute(fCss_ValUndefAttri);
  fCss_ValStringAttri := TSynHighlighterAttributes.Create('Css: String value');
  AddAttribute(fCss_ValStringAttri);
  fCss_ValNumberAttri := TSynHighlighterAttributes.Create('Css: Number value');
  AddAttribute(fCss_ValNumberAttri);
  fCss_SymbolAttri := TSynHighlighterAttributes.Create('Css: Symbol');
  AddAttribute(fCss_SymbolAttri);
  fCss_ErrorAttri := TSynHighlighterAttributes.Create('Css: Error');
  AddAttribute(fCss_ErrorAttri);

  fTokenAttributeTable[tkCssSpace] := fCss_WhitespaceAttri;
  fTokenAttributeTable[tkCssSelector] := fCss_SelectorAttri;
  fTokenAttributeTable[tkCssSelectorUndef] := fCss_SelectorUndefAttri;
  fTokenAttributeTable[tkCssSelectorClass] := fCss_SelectorClassAmpsAttri;
  fTokenAttributeTable[tkCssSelectorId] := fCss_SelectorIdAttri;
  fTokenAttributeTable[tkCssSpecial] := fCss_SpecialAttri;
  fTokenAttributeTable[tkCssComment] := fCss_CommentAttri;
  fTokenAttributeTable[tkCssProp] := fCss_PropAttri;
  fTokenAttributeTable[tkCssPropUndef] := fCss_PropUndefAttri;
  fTokenAttributeTable[tkCssVal] := fCss_ValAttri;
  fTokenAttributeTable[tkCssValUndef] := fCss_ValUndefAttri;
  fTokenAttributeTable[tkCssValString] := fCss_ValStringAttri;
  fTokenAttributeTable[tkCssValNumber] := fCss_ValNumberAttri;
  fTokenAttributeTable[tkCssSymbol] := fCss_SymbolAttri;
  fTokenAttributeTable[tkCssError] := fCss_ErrorAttri;

  // ECMAScript
  ES_MakeMethodTables;

  fES_WhitespaceAttri := TSynHighlighterAttributes.Create('ES: Whitespace');
  AddAttribute(fES_WhitespaceAttri);
  fES_IdentifierAttri := TSynHighlighterAttributes.Create('ES: Identifier');
  AddAttribute(fES_IdentifierAttri);
  fES_KeyAttri := TSynHighlighterAttributes.Create('ES: Key');
  AddAttribute(fES_KeyAttri);
  fES_CommentAttri := TSynHighlighterAttributes.Create('ES: Comment');
  AddAttribute(fES_CommentAttri);
  fES_StringAttri := TSynHighlighterAttributes.Create('ES: String');
  AddAttribute(fES_StringAttri);
  fES_NumberAttri := TSynHighlighterAttributes.Create('ES: Number');
  AddAttribute(fES_NumberAttri);
  fES_SymbolAttri := TSynHighlighterAttributes.Create('ES: Symbol');
  AddAttribute(fES_SymbolAttri);
  fES_ErrorAttri := TSynHighlighterAttributes.Create('ES: Error');
  AddAttribute(fES_ErrorAttri);

  fTokenAttributeTable[tkESSpace] := fES_WhitespaceAttri;
  fTokenAttributeTable[tkESIdentifier] := fES_IdentifierAttri;
  fTokenAttributeTable[tkESKeyword] := fES_KeyAttri;
  fTokenAttributeTable[tkESComment] := fES_CommentAttri;
  fTokenAttributeTable[tkESString] := fES_StringAttri;
  fTokenAttributeTable[tkESNumber] := fES_NumberAttri;
  fTokenAttributeTable[tkESSymbol] := fES_SymbolAttri;
  fTokenAttributeTable[tkESError] := fES_ErrorAttri;

  // PHP
  Php_MakeMethodTables;

  fPhp_WhitespaceAttri := TSynHighlighterAttributes.Create('Php: Whitespace');
  AddAttribute(fPhp_WhitespaceAttri);
  fPhp_IdentifierAttri := TSynHighlighterAttributes.Create('Php: Identifier');
  AddAttribute(fPhp_IdentifierAttri);
  fPhp_KeyAttri := TSynHighlighterAttributes.Create('Php: Keyword');
  AddAttribute(fPhp_KeyAttri);
  fPhp_FunctionAttri := TSynHighlighterAttributes.Create('Php: Function');
  AddAttribute(fPhp_FunctionAttri);
  fPhp_VariableAttri := TSynHighlighterAttributes.Create('Php: Variable');
  AddAttribute(fPhp_VariableAttri);
  fPhp_ConstAttri := TSynHighlighterAttributes.Create('Php: Constant');
  AddAttribute(fPhp_ConstAttri);
  fPhp_StringAttri := TSynHighlighterAttributes.Create('Php: String');
  AddAttribute(fPhp_StringAttri);
  fPhp_StringSpecialAttri := TSynHighlighterAttributes.Create('Php: String special');
  AddAttribute(fPhp_StringSpecialAttri);
  fPhp_CommentAttri := TSynHighlighterAttributes.Create('Php: Comment');
  AddAttribute(fPhp_CommentAttri);
  fPhp_SymbolAttri := TSynHighlighterAttributes.Create('Php: Symbol');
  AddAttribute(fPhp_SymbolAttri);
  fPhp_NumberAttri := TSynHighlighterAttributes.Create('Php: Number');
  AddAttribute(fPhp_NumberAttri);
  fPhp_ErrorAttri := TSynHighlighterAttributes.Create('Php: Error');
  AddAttribute(fPhp_ErrorAttri);

  fTokenAttributeTable[tkPhpSpace] := fHtml_WhitespaceAttri;
  fTokenAttributeTable[tkPhpIdentifier] := fPhp_IdentifierAttri;
  fTokenAttributeTable[tkPhpKeyword] := fPhp_KeyAttri;
  fTokenAttributeTable[tkPhpFunction] := fPhp_FunctionAttri;
  fTokenAttributeTable[tkPhpVariable] := fPhp_VariableAttri;
  fTokenAttributeTable[tkPhpConst] := fPhp_ConstAttri;
  fTokenAttributeTable[tkPhpString] := fPhp_StringAttri;
  fTokenAttributeTable[tkPhpStringSpecial] := fPhp_StringSpecialAttri;
  fTokenAttributeTable[tkPhpComment] := fPhp_CommentAttri;
  fTokenAttributeTable[tkPhpSymbol] := fPhp_SymbolAttri;
  fTokenAttributeTable[tkPhpNumber] := fPhp_NumberAttri;
  fTokenAttributeTable[tkPhpError] := fPhp_ErrorAttri;

  // Global
  fInactiveAttri := TSynHighlighterAttributes.Create('Global: Inactive');
  with fInactiveAttri do
  begin
    Background := clNone;
    Foreground := clInactiveCaption;
    Style := [];
  end;
  AddAttribute(fInactiveAttri);

  fTokenAttributeTable[tkNull] := nil;
  SetAttributesOnChange(DefHighlightChange);
end;

destructor TSynWebEngine.Destroy;
var
  i: integer;
begin
  for i := fAttributes.Count - 1 downto 0 do
    TSynHighlighterAttributes(fAttributes.Objects[i]).Free;
  fAttributes.Clear;
  for i:=0 to FNotifyList.Count-1 do
    TSynWebBase(FNotifyList[i]).Engine := nil;
  FNotifyList.Free;
  inherited Destroy;
end;

procedure TSynWebEngine.AddAttribute(AAttrib: TSynHighlighterAttributes);
begin
  fAttributes.AddObject(AAttrib.Name, AAttrib);
end;

procedure TSynWebEngine.AddToNotifyList(ASynWeb: TSynWebBase);
begin
  FNotifyList.Add(ASynWeb);
end;

procedure TSynWebEngine.RemoveFromNotifyList(ASynWeb: TSynWebBase);
begin
  FNotifyList.Remove(ASynWeb);
end;

procedure TSynWebEngine.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  i: integer;
  Attri: TSynHighlighterAttributes;
begin
  for i := fAttributes.Count - 1 downto 0 do begin
    Attri := TSynHighlighterAttributes(fAttributes.Objects[i]);
    if Attri <> nil then
    begin
      Attri.OnChange := AEvent;
      Attri.InternalSaveDefaultValues;
    end;
  end;
end;

procedure TSynWebEngine.DefHighlightChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
    TSynWebBase(FNotifyList[i]).DoDefHighlightChange;
end;

function TSynWebEngine.GetCRC8_String(AString: string): byte;
var
  i: integer;
begin
  Result := Length(AString);
  for i := 1 to Length(AString) do
    Result := TCrc8_Table[Result xor byte(AString[i])];
end;

function TSynWebEngine.GetRange_Bit(ABit: longword): boolean;
begin
  Result := FConfig^.FRange and (1 shl ABit) <> 0;
end;

procedure TSynWebEngine.SetRange_Bit(ABit: longword; AVal: boolean);
begin
  if AVal then
    FConfig^.FRange := FConfig^.FRange or (1 shl ABit)
  else
    FConfig^.FRange := FConfig^.FRange and not (1 shl ABit);
end;

function TSynWebEngine.GetRange_Int(ALen, APos: longword): longword;
begin
  Result := (FConfig^.FRange shr APos) and not ($FFFFFFFF shl ALen);
end;

procedure TSynWebEngine.SetRange_Int(ALen, APos, AVal: longword);
var
  i: longword;
begin
  i := $FFFFFFFF shl ALen;
  //todo: Does it work in CLX? Should be [EBX].APos? I don't know :(
  asm
    mov ecx, APos
    rol i, cl
  end;
  FConfig^.FRange := (FConfig^.FRange and i) or ((AVal shl APos) and not i);
end;

procedure TSynWebEngine.NullProc;
begin
  FConfig^.FTokenID := tkNull;
end;

procedure TSynWebEngine.NextSetHighlighterType;
begin
  SetHighlighterType(FConfig^.FNextHighlighterType, FConfig^.FNextClearBits,
    False, FConfig^.FNextUseNextAH);
  Next;
  FConfig^.FHighlighterSW := True;
end;

procedure TSynWebEngine.SetHighlighterType(const AHighlighterType: TSynHighlighterType;
  AClearBits: boolean; ASetAtNextToken: boolean; AUseNextAH: boolean);
begin
  if ASetAtNextToken then
  begin
    FConfig^.FNextUseNextAH := AUseNextAH;
    FConfig^.FNextHighlighterType := AHighlighterType;
    FConfig^.FNextClearBits := AClearBits;
    FConfig^.FNextProcTable := NextSetHighlighterType;
  end
  else
  begin
    FConfig^.FUseNextAH := AUseNextAH;
    FConfig^.FHighlighterSW := True;
    FConfig^.FPrevHighlighterType := FConfig^.FHighlighterType;
    FConfig^.FHighlighterType := AHighlighterType;
    SetRange_Int(3, 29, Longword(AHighlighterType));
    SetupHighlighterType(AClearBits);
  end;
end;

procedure TSynWebEngine.SetupHighlighterType(AClearBits: boolean);
begin
  case FConfig^.FHighlighterType of
    shtHtml:
    begin
      if AClearBits then
        SetRange_Int(17, 0, 0);
      FConfig^.FSYN_ATTR_COMMENT := fHtml_CommentAttri;
      FConfig^.FSYN_ATTR_STRING := fHtml_TagKeyValueQuotedAttri;
      FConfig^.FSYN_ATTR_WHITESPACE := fHtml_WhitespaceAttri;
      FConfig^.FNextProcTable := Html_Next;
    end;
    shtCss:
    begin
      if AClearBits then
        SetRange_Int(17, 0, 0);
      FConfig^.FSYN_ATTR_COMMENT := fCss_CommentAttri;
      FConfig^.FSYN_ATTR_STRING := fCss_ValStringAttri;
      Css_UpdateBg;
      FConfig^.FNextProcTable := Css_Next;
    end;
    shtES:
    begin
      if AClearBits then
        SetRange_Int(17, 0, 0);
      FConfig^.FSYN_ATTR_COMMENT := fES_CommentAttri;
      FConfig^.FSYN_ATTR_STRING := fES_StringAttri;
      FConfig^.FSYN_ATTR_WHITESPACE := fES_WhitespaceAttri;
      FConfig^.FNextProcTable := ES_Next;
    end;
    else // PHP
      if AClearBits then
        SetRange_Int(12, 17, 0);
      FConfig^.FSYN_ATTR_COMMENT := fPhp_CommentAttri;
      FConfig^.FSYN_ATTR_STRING := fPhp_StringAttri;
      FConfig^.FSYN_ATTR_WHITESPACE := fPhp_WhitespaceAttri;
      FConfig^.FNextProcTable := Php_Next;
  end;
end;

procedure TSynWebEngine.SetLine(NewValue: string; LineNumber: integer);
{$IFDEF SYNWEB_FIXNULL}
var
  i:Integer;
{$ENDIF}
begin
  FConfig^.FLineRef := NewValue;
{$IFDEF SYNWEB_FIXNULL}
  for i:=1 to Length(FConfig^.FLineRef) do
    if FConfig^.FLineRef[i]=#0 then
      FConfig^.FLineRef[i]:=' ';
{$ENDIF}
  FConfig^.FLine := PChar(FConfig^.FLineRef);
  FConfig^.FRun := 0;
  FConfig^.FLineNumber := LineNumber;
  FConfig^.FHighlighterType := TSynHighlighterType(GetRange_Int(3, 29));
  FConfig^.FPrevHighlighterType := FConfig^.FHighlighterType;
  FConfig^.FHighlighterSW := False;
  SetupHighlighterType;
  FConfig^.FNextProcTable;
end;

procedure TSynWebEngine.Next;
begin
  FConfig^.FHighlighterSW := False;
  FConfig^.FNextProcTable;
end;

function TSynWebEngine.GetToken: string;
var
  Len: longint;
begin
  Len := FConfig^.FRun - FConfig^.FTokenPos;
  SetString(Result, (FConfig^.FLine + FConfig^.FTokenPos), Len);
end;

function TSynWebEngine.GetTokenLen: integer;
begin
  Result := FConfig^.FRun - FConfig^.FTokenPos;
end;

// HTML ------------------------------------------------------------------------

procedure TSynWebEngine.Html_MakeMethodTables;
var
  i: integer;
  pF: PIdentFuncTableFunc;
  pF2: PIdent2FuncTableFunc;
begin
  fHtml_RangeProcTable[rsHtmlText] := Html_RangeTextProc;
  fHtml_RangeProcTable[rsHtmlComment] := Html_RangeCommentProc;
  fHtml_RangeProcTable[rsHtmlCommentClose] := Html_RangeCommentCloseProc;
  fHtml_RangeProcTable[rsHtmlTag] := Html_RangeTagProc;
  fHtml_RangeProcTable[rsHtmlTagClose] := Html_RangeTagCloseProc;
  fHtml_RangeProcTable[rsHtmlTagDOCTYPE] := Html_RangeTagDOCTYPEProc;
  fHtml_RangeProcTable[rsHtmlTagCDATA] := Html_RangeTagCDATAProc;
  fHtml_RangeProcTable[rsHtmlTagKey] := Html_RangeTagKeyProc;
  fHtml_RangeProcTable[rsHtmlTagKeyEq] := Html_RangeTagKeyEqProc;
  fHtml_RangeProcTable[rsHtmlTagKeyValue] := Html_RangeTagKeyValueProc;
  fHtml_RangeProcTable[rsHtmlTagKeyValueQuoted1] := Html_RangeTagKeyValueQuoted1Proc;
  fHtml_RangeProcTable[rsHtmlTagKeyValueQuoted2] := Html_RangeTagKeyValueQuoted2Proc;

  pF := PIdentFuncTableFunc(@fHtml_TagIdentFuncTable);
  for I := Low(fHtml_TagIdentFuncTable) to High(fHtml_TagIdentFuncTable) do
  begin
    pF^ := Html_TagUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_TagsFuncTable.inc}

  pF := PIdentFuncTableFunc(@fHtml_AttrIdentFuncTable);
  for I := Low(fHtml_TagIdentFuncTable) to High(fHtml_AttrIdentFuncTable) do
  begin
    pF^ := Html_AttrUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_AttrsFuncTable.inc}

  pF2 := PIdent2FuncTableFunc(@fHtml_SpecialIdentFuncTable);
  for I := Low(fHtml_SpecialIdentFuncTable) to High(fHtml_SpecialIdentFuncTable) do
  begin
    pF2^ := Html_SpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_SpecialFuncTable.inc}
end;

procedure TSynWebEngine.Html_Next;
begin
  FConfig^.FTokenPos := FConfig^.FRun;
  fHtml_RangeProcTable[Html_GetRange];
end;

function TSynWebEngine.Html_GetRange: THtmlRangeState;
begin
  Result := THtmlRangeState(GetRange_Int(4, 13));
end;

procedure TSynWebEngine.Html_SetRange(const ARange: THtmlRangeState);
begin
  SetRange_Int(4, 13, Longword(ARange));
end;

function TSynWebEngine.Html_GetTag: integer;
begin
  Result := GetRange_Int(7, 0);
end;

procedure TSynWebEngine.Html_SetTag(const ATag: integer);
begin
  SetRange_Int(7, 0, Longword(ATag));
end;

function TSynWebEngine.Html_CheckNull(ADo: boolean = True): boolean;
begin
  if FConfig^.FLine[FConfig^.FRun] = #0 then
  begin
    Result := True;
    if ADo then
      NullProc;
  end
  else
    Result := False;
end;

procedure TSynWebEngine.Html_SpaceProc;
begin
  repeat
    Inc(FConfig^.FRun);
  until not (FConfig^.FLine[FConfig^.FRun] in [#1..#32]);
  FConfig^.FTokenID := tkHtmlSpace;
end;

procedure TSynWebEngine.Html_AmpersandProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkHtmlEscape;
  if FConfig^.FLine[FConfig^.FRun] = '#' then
  begin
    Inc(FConfig^.FRun);
    if FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun]] = FConfig^.FHashTable['x'] then
    begin
      Inc(FConfig^.FRun);
      if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) = 0 then
        // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) then
        FConfig^.FTokenID := tkHtmlError
      else
        repeat
          Inc(FConfig^.FRun)
        until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) = 0;
      // until not (FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']);
    end
    else
    if not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9']) then
      FConfig^.FTokenID := tkHtmlError
    else
      repeat
        Inc(FConfig^.FRun)
      until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9']);
  end
  else
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0 then
    // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z'] then
    FConfig^.FTokenID := tkHtmlError
  else
  begin
    repeat
      Inc(FConfig^.FRun)
    until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0;
    // until not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z'];
    if Html_SpecialCheck(FConfig^.FTokenPos + 1, FConfig^.FRun -
      FConfig^.FTokenPos - 1) = -1 then
      FConfig^.FTokenID := tkHtmlError;
  end;
  if FConfig^.FLine[FConfig^.FRun] = ';' then
    Inc(FConfig^.FRun)
  else
    FConfig^.FTokenID := tkHtmlError;
end;

procedure TSynWebEngine.Html_BraceOpenProc;
begin
  if Php_CheckBegin then
    Exit;
  Inc(FConfig^.FRun);
  case FConfig^.FLine[FConfig^.FRun] of
    '/':
    begin
      Inc(FConfig^.FRun);
      SetRange_Bit(12, True);
    end;
    '?':
    begin
      if FConfig^.FHtmlVersion >= hvXHtml10Strict then
        Inc(FConfig^.FRun);
      SetRange_Bit(12, False);
    end;
    '!':
    begin
      Inc(FConfig^.FRun);
      if (FConfig^.FLine[FConfig^.FRun] = '-') and
        (FConfig^.FLine[FConfig^.FRun + 1] = '-') then
      begin
        Inc(FConfig^.FRun, 2);
        Html_SetRange(rsHtmlComment);
        if (FConfig^.FLine[FConfig^.FRun] = #0) or Php_CheckBegin(False) then
          FConfig^.FTokenID := tkHtmlComment
        else
          Html_RangeCommentProc;
      end
      else
      if (FConfig^.FHtmlVersion >= hvXHtml10Strict) and
        (FConfig^.FLine[FConfig^.FRun] = '[') and
        (FConfig^.FLine[FConfig^.FRun + 1] = 'C') and
        (FConfig^.FLine[FConfig^.FRun + 2] = 'D') and
        (FConfig^.FLine[FConfig^.FRun + 3] = 'A') and
        (FConfig^.FLine[FConfig^.FRun + 4] = 'T') and
        (FConfig^.FLine[FConfig^.FRun + 5] = 'A') and
        (FConfig^.FLine[FConfig^.FRun + 6] = '[') then
      begin
        Inc(FConfig^.FRun, 7);
        FConfig^.FTokenID := tkHtmlTag;
        Html_SetRange(rsHtmlTagCDATA);
      end
      else
      if (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun]] =
        FConfig^.FHashTable['D']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 1]] =
        FConfig^.FHashTable['O']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 2]] =
        FConfig^.FHashTable['C']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 3]] =
        FConfig^.FHashTable['T']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 4]] =
        FConfig^.FHashTable['Y']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 5]] =
        FConfig^.FHashTable['P']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 6]] =
        FConfig^.FHashTable['E']) and
        (fIdentTable[FConfig^.FLine[FConfig^.FRun + 7]] and (1 shl 0) = 0) then
        // (not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z'])) then
      begin
        FConfig^.FTokenID := tkHtmlTag;
        SetRange_Int(2, 7, 0);
        Html_SetRange(rsHtmlTagDOCTYPE);
      end
      else
        FConfig^.FTokenID := tkHtmlError;
      Exit;
    end;
    else
      SetRange_Bit(12, False);
  end;
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) <> 0 then
    // if FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z'] then
  begin
    FConfig^.FTokenID := tkHtmlTag;
    Html_SetRange(rsHtmlTag);
  end
  else
    FConfig^.FTokenID := tkHtmlError;
end;

procedure TSynWebEngine.Html_ErrorProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkHtmlError;
end;

procedure TSynWebEngine.Html_RangeTextProc;
begin
  case FConfig^.FLine[FConfig^.FRun] of
    #0:
      NullProc;
    #1..#32:
      Html_SpaceProc;
    '<':
      Html_BraceOpenProc;
    '>':
      Html_ErrorProc;
    '&':
      Html_AmpersandProc;
    else
      repeat
        Inc(FConfig^.FRun);
      until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 6) <> 0;
      // until FConfig^.FLine[FConfig^.FRun] In [#0..#32, '<', '>', '&'];
      FConfig^.FTokenID := tkHtmlText;
  end;
end;

procedure TSynWebEngine.Html_RangeCommentProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 19) = 0 do
      // while not (FConfig^.FLine[FConfig^.FRun) in [#0, '-', '<']) do
      Inc(FConfig^.FRun);
    case FConfig^.FLine[FConfig^.FRun] of
      #0:
        Break;
      '-':
      begin
        Inc(FConfig^.FRun);
        if FConfig^.FLine[FConfig^.FRun] = '-' then
        begin
          Inc(FConfig^.FRun);
          if FConfig^.FLine[FConfig^.FRun] = '>' then
          begin
            Inc(FConfig^.FRun);
            Html_SetRange(rsHtmlText);
          end
          else
          begin
            Html_SetRange(rsHtmlCommentClose);
            if (FConfig^.FLine[FConfig^.FRun] <> #0) and not Php_CheckBegin(False) then
            begin
              Html_RangeCommentCloseProc;
              Exit;
            end;
          end;
          Break;
        end;
      end;
      '<':
        if Php_CheckBegin(False) then
          Break
        else
          Inc(FConfig^.FRun);
    end;
  until False;
  FConfig^.FTokenID := tkHtmlComment;
end;

procedure TSynWebEngine.Html_RangeCommentCloseProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 20) = 0 do
      // while not (FConfig^.FLine[FConfig^.FRun) in [#0, '<', '>']) do
      Inc(FConfig^.FRun);
    case FConfig^.FLine[FConfig^.FRun] of
      #0:
        Break;
      '>':
      begin
        Inc(FConfig^.FRun);
        Html_SetRange(rsHtmlText);
        Break;
      end;
      '<':
        if Php_CheckBegin(False) then
          Break
        else
          Inc(FConfig^.FRun);
    end;
  until False;
  FConfig^.FTokenID := tkHtmlComment;
end;

procedure TSynWebEngine.Html_RangeTagDOCTYPEProc;
begin
  case GetRange_Int(2, 7) of
    0:
    begin
      Inc(FConfig^.FRun, 7);
      FConfig^.FTokenID := tkHtmlTagName;
      SetRange_Int(2, 7, 1);
    end;
    1:
      if not Html_CheckNull and not Php_CheckBegin then
        case FConfig^.FLine[FConfig^.FRun] of
          #1..#32:
          begin
            Html_SpaceProc;
            Exit;
          end;
          '>':
          begin
            Inc(FConfig^.FRun);
            FConfig^.FTokenID := tkHtmlTag;
            SetRange_Int(2, 7, 0);
            Html_SetRange(rsHtmlText);
            Exit;
          end;
          #39:
          begin
            Inc(FConfig^.FRun);
            if FConfig^.FLine[FConfig^.FRun] = #0 then
              FConfig^.FTokenID := tkHtmlError
            else
            begin
              SetRange_Int(2, 7, 2);
              if Php_CheckBegin(False) then
                FConfig^.FTokenID := tkHtmlTagKeyValueQuoted
              else
                Html_RangeTagDOCTYPEProc;
            end;
          end;
          '"':
          begin
            Inc(FConfig^.FRun);
            if FConfig^.FLine[FConfig^.FRun] = #0 then
              FConfig^.FTokenID := tkHtmlError
            else
            begin
              SetRange_Int(2, 7, 3);
              if Php_CheckBegin(False) then
                FConfig^.FTokenID := tkHtmlTagKeyValueQuoted
              else
                Html_RangeTagDOCTYPEProc;
            end;
          end;
          else
            if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0 then
              // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z']) then
            begin
              Inc(FConfig^.FRun);
              FConfig^.FTokenID := tkHtmlError;
              Exit;
            end;
            repeat
              Inc(FConfig^.FRun);
            until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0;
            // until not (FConfig^.FLine[FConfig^.FRun] In ['a'..'z', 'A'..'Z']);
            FConfig^.FTokenID := tkHtmlTagKey;
        end;
    2:
    begin
      if not Html_CheckNull then
        if Php_CheckBegin then
          Exit
        else
          repeat
            while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 21) = 0 do
              // while not (FConfig^.FLine[FConfig^.FRun] in [#0, #39, '<']) do
              Inc(FConfig^.FRun);
            case FConfig^.FLine[FConfig^.FRun] of
              #0:
              begin
                FConfig^.FTokenID := tkHtmlError;
                Break;
              end;
              '<':
                if Php_CheckBegin(False) then
                begin
                  FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
                  Exit;
                end
                else
                  Inc(FConfig^.FRun);
              #39:
              begin
                Inc(FConfig^.FRun);
                FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
                Break;
              end;
            end;
          until False;
      SetRange_Int(2, 7, 1);
    end;
    3:
    begin
      if not Html_CheckNull then
        if Php_CheckBegin then
          Exit
        else
          repeat
            while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 22) = 0 do
              // while not (FConfig^.FLine[FConfig^.FRun] in [#0, '"', '<']) do
              Inc(FConfig^.FRun);
            case FConfig^.FLine[FConfig^.FRun] of
              #0:
              begin
                FConfig^.FTokenID := tkHtmlError;
                Break;
              end;
              '<':
                if Php_CheckBegin(False) then
                begin
                  FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
                  Exit;
                end
                else
                  Inc(FConfig^.FRun);
              '"':
              begin
                Inc(FConfig^.FRun);
                FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
                Break;
              end;
            end;
          until False;
      SetRange_Int(2, 7, 1);
    end;
  end;
end;

procedure TSynWebEngine.Html_RangeTagCDATAProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  if FConfig^.FLine[FConfig^.FRun] in [#1..#32] then
  begin
    Html_SpaceProc;
    Exit;
  end
  else
  if (FConfig^.FLine[FConfig^.FRun] = ']') and
    (FConfig^.FLine[FConfig^.FRun + 1] = ']') and
    (FConfig^.FLine[FConfig^.FRun + 2] = '>') then
  begin
    Inc(FConfig^.FRun, 3);
    FConfig^.FTokenID := tkHtmlTag;
    Html_SetRange(rsHtmlText);
  end
  else
  begin
    repeat
      repeat
        Inc(FConfig^.FRun);
      until fIdentTable2[FConfig^.FLine[FConfig^.FRun]] and (1 shl 1) <> 0;
      // until FConfig^.FLine[FConfig^.FRun] in [#0..#32, '<', ']'];
      case FConfig^.FLine[FConfig^.FRun] of
        #0..#32, ']':
          Break;
        '<':
          if Php_CheckBegin(False) then
            Break;
      end;
    until False;
    FConfig^.FTokenID := tkHtmlText;
  end;
end;

procedure TSynWebEngine.Html_RangeTagProc;
var
  ID: integer;
begin
  repeat
    Inc(FConfig^.FRun);
  until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 16) = 0;
  // until not (FConfig^.FLine[FConfig^.FRun] In ['a'..'z', 'A'..'Z', '_', '0'..'9']);
  FConfig^.FTokenID := Html_TagCheck;
  ID := Html_GetTag - 1;
  if GetRange_Bit(12) then
  begin
    if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      FConfig^.FTokenID := tkHtmlError;
    Html_SetRange(rsHtmlTagClose);
  end
  else
  begin
    if (ID <> -1) and ((FConfig^.FLine[FConfig^.FTokenPos - 1] = '?') xor
      (TSynWeb_TagsData[ID] and (1 shl 29) <> 0)) then
      FConfig^.FTokenID := tkHtmlError;
    Html_SetRange(rsHtmlTagKey);
  end;
end;

procedure TSynWebEngine.Html_RangeTagCloseProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case FConfig^.FLine[FConfig^.FRun] of
    #1..#32:
      Html_SpaceProc;
    '>':
    begin
      Inc(FConfig^.FRun);
      FConfig^.FTokenID := tkHtmlTag;
      Html_SetRange(rsHtmlText);
    end;
    else
      FConfig^.FTokenID := tkHtmlError;
      repeat
        repeat
          Inc(FConfig^.FRun);
        until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 1) <> 0;
        // until not (FConfig^.FLine[FConfig^.FRun] In [#0..#32, '<', '>']) do
        if (FConfig^.FLine[FConfig^.FRun] = '<') and not Php_CheckBegin(False) then
          Continue
        else
          Break;
      until False;
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyProc;
var
  ID: integer;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  ID := Html_GetTag - 1;
  if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 29) <> 0) then
    if (FConfig^.FLine[FConfig^.FRun] = '?') and
      (FConfig^.FLine[FConfig^.FRun + 1] = '>') then
    begin
      Inc(FConfig^.FRun, 2);
      FConfig^.FTokenID := tkHtmlTag;
      Html_SetRange(rsHtmlText);
      Exit;
    end
    else
    if FConfig^.FLine[FConfig^.FRun] = '>' then
    begin
      Inc(FConfig^.FRun);
      FConfig^.FTokenID := tkHtmlError;
      Html_SetRange(rsHtmlText);
      Exit;
    end;
  case FConfig^.FLine[FConfig^.FRun] of
    #1..#32:
      Html_SpaceProc;
    '/':
      if not GetRange_Bit(12) and (FConfig^.FLine[FConfig^.FRun + 1] = '>') and
        (FConfig^.FHtmlVersion >= hvXHtml10Strict) and
        (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      begin
        Inc(FConfig^.FRun, 2);
        FConfig^.FTokenID := tkHtmlTag;
        Html_SetRange(rsHtmlText);
      end
      else
      begin
        Inc(FConfig^.FRun);
        FConfig^.FTokenID := tkHtmlError;
      end;
    '>':
    begin
      Inc(FConfig^.FRun);
      FConfig^.FTokenID := tkHtmlTag;
      if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) and
        (FConfig^.FHtmlVersion >= hvXHtml10Strict) then
        FConfig^.FTokenID := tkHtmlError
      else
      if not GetRange_Bit(12) and ((FConfig^.FRun = 0) or
        (FConfig^.FLine[FConfig^.FRun - 2] <> '/')) then
        if (ID = Html_TagID_Style) and FConfig^.FCssEmbeded then
        begin
          SetHighlighterType(shtCss, True, True, True);
          Exit;
        end
        else
        if (ID = Html_TagID_Script) then
          if GetRange_Bit(18) and FConfig^.FPhpEmbeded then
          begin
            Php_Begin(potHTML);
            Exit;
          end
          else
            if FConfig^.FEsEmbeded then
            begin
              SetHighlighterType(shtES, True, True, True);
              Exit;
            end;
      Html_SetRange(rsHtmlText);
    end;
    else
      if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0 then
        // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z']) then
        Html_ErrorProc
      else
      begin
        repeat
          Inc(FConfig^.FRun);
        until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 7) = 0;
        // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', ':', '-']);
        if ID = -1 then
          FConfig^.FTokenID := tkHtmlTagKeyUndef
        else
        begin
          FConfig^.FTokenID := Html_AttrCheck;
          if ID = Html_TagID_Script then
            SetRange_Bit(17, FConfig^.FToken_LastID = Html_AttrID_Language);
        end;
      end;
      Html_SetRange(rsHtmlTagKeyEq);
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyEqProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case FConfig^.FLine[FConfig^.FRun] of
    #1..#32:
      Html_SpaceProc;
    '=':
    begin
      Inc(FConfig^.FRun);
      FConfig^.FTokenID := tkHtmlSymbol;
      Html_SetRange(rsHtmlTagKeyValue);
    end;
    else
      Html_SetRange(rsHtmlTagKey);
      Html_RangeTagKeyProc;
      if FConfig^.FHtmlVersion >= hvXHtml10Strict then
        FConfig^.FTokenID := tkHtmlError;
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyValueProc;
var
  ID: integer;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case FConfig^.FLine[FConfig^.FRun] of
    #1..#32:
      Html_SpaceProc;
    #39:
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] = #0 then
      begin
        Html_SetRange(rsHtmlTagKey);
        FConfig^.FTokenID := tkHtmlError;
      end
      else
      begin
        Html_SetRange(rsHtmlTagKeyValueQuoted1);
        if Php_CheckBegin(False) then
          FConfig^.FTokenID := tkHtmlTagKeyValueQuoted
        else
          Html_RangeTagKeyValueQuoted1Proc;
      end;
    end;
    '"':
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] = #0 then
      begin
        Html_SetRange(rsHtmlTagKey);
        FConfig^.FTokenID := tkHtmlError;
      end
      else
      begin
        Html_SetRange(rsHtmlTagKeyValueQuoted2);
        if Php_CheckBegin(False) then
          FConfig^.FTokenID := tkHtmlTagKeyValueQuoted
        else
          Html_RangeTagKeyValueQuoted2Proc;
      end;
    end;
    else
      if (FConfig^.FLine[FConfig^.FRun] = '>') or
        ((FConfig^.FHtmlVersion >= hvXHtml10Strict) and
        (FConfig^.FLine[FConfig^.FRun] = '/') and
        (FConfig^.FLine[FConfig^.FRun + 1] = '>')) then
      begin
        if FConfig^.FLine[FConfig^.FRun] = '/' then
          Inc(FConfig^.FRun, 2)
        else
          Inc(FConfig^.FRun);
        FConfig^.FTokenID := tkHtmlError;
        if not GetRange_Bit(12) and ((FConfig^.FRun = 0) or
          (FConfig^.FLine[FConfig^.FRun - 2] <> '/')) then
        begin
          ID := Html_GetTag - 1;
          if (ID = Html_TagID_Style) and FConfig^.FCssEmbeded then
          begin
            SetHighlighterType(shtCss, True, True, True);
            Exit;
          end
          else
          if (ID = Html_TagID_Script) then
            if GetRange_Bit(18) and FConfig^.FPhpEmbeded then
            begin
              Php_Begin(potHTML);
              Exit;
            end
            else
              if FConfig^.FEsEmbeded then
              begin
                SetHighlighterType(shtES, True, True, True);
                Exit;
              end;
        end;
        Html_SetRange(rsHtmlText);
      end
      else
      begin
        repeat
          repeat
            Inc(FConfig^.FRun);
          until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 23) <> 0;
          // until FConfig^.FLine[FConfig^.FRun] in [#0..#32, '<', '>', '/'];
          case FConfig^.FLine[FConfig^.FRun] of
            '/':
              if (FConfig^.FLine[FConfig^.FRun + 1] = '>') and
                (FConfig^.FHtmlVersion >= hvXHtml10Strict) then
                Break;
            '<':
              if Php_CheckBegin(False) then
                Break
              else
                Inc(FConfig^.FRun);
            else
              Break;
          end;
        until False;
        if FConfig^.FHtmlVersion >= hvXHtml10Strict then
          FConfig^.FTokenID := tkHtmlError
        else
          FConfig^.FTokenID := tkHtmlTagKeyValue;
        if GetRange_Bit(17) then
          SetRange_Bit(18, UpperCase(GetToken) = 'PHP');
        Html_SetRange(rsHtmlTagKey);
      end;
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyValueQuoted1Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 21) = 0 do
          // while not (FConfig^.FLine[FConfig^.FRun] in [#0, #39, '<']) do
          Inc(FConfig^.FRun);
        case FConfig^.FLine[FConfig^.FRun] of
          #0:
          begin
            FConfig^.FTokenID := tkHtmlError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
              Exit;
            end
            else
              Inc(FConfig^.FRun);
          #39:
          begin
            Inc(FConfig^.FRun);
            FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
            if GetRange_Bit(17) then
              SetRange_Bit(18, UpperCase(GetToken) = #39'PHP'#39);
            Break;
          end;
        end;
      until False;
  Html_SetRange(rsHtmlTagKey);
end;

procedure TSynWebEngine.Html_RangeTagKeyValueQuoted2Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 22) = 0 do
          // while not (FConfig^.FLine[FConfig^.FRun] in [#0, '"', '<']) do
          Inc(FConfig^.FRun);
        case FConfig^.FLine[FConfig^.FRun] of
          #0:
          begin
            FConfig^.FTokenID := tkHtmlError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
              Exit;
            end
            else
              Inc(FConfig^.FRun);
          '"':
          begin
            Inc(FConfig^.FRun);
            FConfig^.FTokenID := tkHtmlTagKeyValueQuoted;
            if GetRange_Bit(17) then
              SetRange_Bit(18, UpperCase(GetToken) = '"PHP"');
            Break;
          end;
        end;
      until False;
  Html_SetRange(rsHtmlTagKey);
end;

function TSynWebEngine.Html_TagKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  if TSynWeb_TagsData[ID] and (1 shl Longword(FConfig^.FHtmlVersion)) = 0 then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Tags[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if FConfig^.FHashTable[Temp^] <> FConfig^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Html_TagCheck: TtkTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := FConfig^.FRun - FConfig^.FTokenPos;
    for i := 0 to FConfig^.FStringLen - 1 do
    begin
      Inc(HashKey, FConfig^.FHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[FConfig^.FTokenPos];
  KeyHash(FConfig^.FToIdent);
  FConfig^.FToken_LastID := -1;
  if HashKey <= Html_TagMaxKeyHash then
    Result := fHtml_TagIdentFuncTable[HashKey]
  else
    Result := tkHtmlTagNameUndef;
  Html_SetTag(FConfig^.FToken_LastID + 1);
end;

{$I SynHighlighterWeb_TagsFunc.inc}

function TSynWebEngine.Html_AttrKeyComp(const ID: integer): boolean;
var
  I, tag: integer;
  Temp: PChar;
  aKey: string;
begin
  tag := Html_GetTag - 1;
  if (tag = -1) or (TSynWeb_AttrsData[ID][Longword(FConfig^.FHtmlVersion)]
    [tag div 32] and (1 shl (tag mod 32)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Attrs[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if FConfig^.FHashTable[Temp^] <> FConfig^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Html_AttrCheck: TtkTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := FConfig^.FRun - FConfig^.FTokenPos;
    for i := 0 to FConfig^.FStringLen - 1 do
    begin
      Inc(HashKey, FConfig^.FHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[FConfig^.FTokenPos];
  KeyHash(FConfig^.FToIdent);
  FConfig^.FToken_LastID := -1;
  if HashKey <= Html_AttrMaxKeyHash then
    Result := fHtml_AttrIdentFuncTable[HashKey]
  else
    Result := tkHtmlTagKeyUndef;
end;

{$I SynHighlighterWeb_AttrsFunc.inc}

function TSynWebEngine.Html_SpecialKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_Special[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if FConfig^.FHashTable[Temp^] <> FConfig^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Html_SpecialCheck(AStart, ALen: integer): integer;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := ALen;
    for i := 0 to ALen - 1 do
    begin
      Inc(HashKey, FConfig^.FHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[AStart];
  KeyHash(FConfig^.FToIdent);
  if (HashKey > Html_SpecialMaxKeyHash) or not fHtml_SpecialIdentFuncTable[HashKey] then
    FConfig^.FToken_LastID := -1;
  Result := FConfig^.FToken_LastID;
end;

{$I SynHighlighterWeb_SpecialFunc.inc}

// CSS -------------------------------------------------------------------------

procedure TSynWebEngine.Css_MakeMethodTables;
var
  c: char;
  i: integer;
  pF: PIdentFuncTableFunc;
  pF2: PIdent2FuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        fCss_ProcTable[c] := NullProc;
      #1..#32:
        fCss_ProcTable[c] := Css_SpaceProc;
      '@':
        fCss_ProcTable[c] := Css_AtKeywordProc;
      '/':
        fCss_ProcTable[c] := Css_SlashProc;
      '<':
        fCss_ProcTable[c] := Css_BraceOpenProc;
      '{':
        fCss_ProcTable[c] := Css_CurlyBraceOpenProc;
      '}':
        fCss_ProcTable[c] := Css_CurlyBraceCloseProc;
      '*', '>':
        fCss_ProcTable[c] := Css_ChildAnySelectorProc;
      '[':
        fCss_ProcTable[c] := Css_AttribProc;
      '#':
        fCss_ProcTable[c] := Css_HashProc;
      '.':
        fCss_ProcTable[c] := Css_DotProc;
      ',':
        fCss_ProcTable[c] := Css_CommaProc;
      ':':
        fCss_ProcTable[c] := Css_ColonProc;
      ';':
        fCss_ProcTable[c] := Css_SemiColonProc;
      '!':
        fCss_ProcTable[c] := Css_ExclamationProc;
      #39, '"':
        fCss_ProcTable[c] := Css_StringProc;
      '+':
        fCss_ProcTable[c] := Css_PlusProc;
      '-':
        fCss_ProcTable[c] := Css_MinusProc;
      '0'..'9':
        fCss_ProcTable[c] := Css_NumberProc;
      'a'..'z', 'A'..'Z', '\':
        fCss_ProcTable[c] := Css_IdentProc;
      else
        fCss_ProcTable[c] := Css_ErrorProc;
    end;

  fCss_RangeProcTable[rsCssRuleset] := Css_RangeRulesetProc;
  fCss_RangeProcTable[rsCssSelectorAttrib] := Css_RangeSelectorAttribProc;
  fCss_RangeProcTable[rsCssSelectorPseudo] := Css_RangeSelectorPseudoProc;
  fCss_RangeProcTable[rsCssAtKeyword] := Css_RangeAtKeywordProc;
  fCss_RangeProcTable[rsCssComment] := Css_RangeCommentProc;
  fCss_RangeProcTable[rsCssProp] := Css_RangePropProc;
  fCss_RangeProcTable[rsCssPropVal] := Css_RangePropValProc;
  fCss_RangeProcTable[rsCssPropValStr] := Css_RangePropValStrProc;
  fCss_RangeProcTable[rsCssPropValRgb] := Css_RangePropValRgbProc;
  fCss_RangeProcTable[rsCssPropValSpecial] := Css_RangePropValSpecialProc;
  fCss_RangeProcTable[rsCssPropValImportant] := Css_RangePropValImportantProc;
  fCss_RangeProcTable[rsCssPropValUrl] := Css_RangePropValUrlProc;
  fCss_RangeProcTable[rsCssPropValRect] := Css_RangePropValRectProc;
  fCss_RangeProcTable[rsCssPropValFunc] := Css_RangePropValFuncProc;

  pF := PIdentFuncTableFunc(@fCss_PropIdentFuncTable);
  for I := Low(fCss_PropIdentFuncTable) to High(fCss_PropIdentFuncTable) do
  begin
    pF^ := Css_PropUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssPropsFuncTable.inc}

  pF := PIdentFuncTableFunc(@fCss_ValIdentFuncTable);
  for I := Low(fCss_ValIdentFuncTable) to High(fCss_ValIdentFuncTable) do
  begin
    pF^ := Css_ValUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssValsFuncTable.inc}

  pF2 := PIdent2FuncTableFunc(@fCss_SpecialIdentFuncTable);
  for I := Low(fCss_SpecialIdentFuncTable) to High(fCss_SpecialIdentFuncTable) do
  begin
    pF2^ := CSS_SpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_CssSpecialFuncTable.inc}
end;

procedure TSynWebEngine.Css_NextBg;
begin
  Css_UpdateBg;
  FConfig^.FNextProcTable := Css_Next;
  Css_Next;
end;

procedure TSynWebEngine.Css_Next;
begin
  FConfig^.FTokenPos := FConfig^.FRun;
  fCss_RangeProcTable[Css_GetRange];
end;

procedure TSynWebEngine.Css_UpdateBg;
begin
  if TCssRangeState(GetRange_Int(4, 13)) in
    [TCssRangeState_RulesetBegin..TCssRangeState_RulesetEnd] then
    FConfig^.FSYN_ATTR_WHITESPACE := fCss_RulesetWhitespaceAttri
  else
    FConfig^.FSYN_ATTR_WHITESPACE := fCss_WhitespaceAttri;
  fTokenAttributeTable[tkCssSpace] := FConfig^.FSYN_ATTR_WHITESPACE;
end;

function TSynWebEngine.Css_GetRange: TCssRangeState;
begin
  if GetRange_Bit(12) then
    Result := rsCssComment
  else
    Result := TCssRangeState(GetRange_Int(4, 13));
end;

procedure TSynWebEngine.Css_SetRange(const ARange: TCssRangeState);
begin
  if ARange = rsCssComment then
    SetRange_Bit(12, True)
  else
  begin
    if not (ARange in [TCssRangeState_RulesetBegin..TCssRangeState_RulesetEnd]) and
      (TCssRangeState(GetRange_Int(4, 13)) in
      [TCssRangeState_RulesetBegin..TCssRangeState_RulesetEnd]) then
    begin
      SetRange_Int(4, 13, Longword(ARange));
      FConfig^.FNextProcTable := Css_NextBg;
    end
    else
    begin
      SetRange_Int(4, 13, Longword(ARange));
      Css_UpdateBg;
    end;
    if ARange = rsCssRuleset then
      SetRange_Int(11, 0, 0);
  end;
end;

function TSynWebEngine.Css_GetProp: integer;
begin
  Result := GetRange_Int(8, 0);
end;

procedure TSynWebEngine.Css_SetProp(const AProp: integer);
begin
  SetRange_Int(8, 0, Longword(AProp));
end;

function TSynWebEngine.Css_CheckNull(ADo: boolean = True): boolean;
begin
  case FConfig^.FLine[FConfig^.FRun] of
    #0:
    begin
      Result := True;
      if ADo then
        NullProc;
    end;
    '<':
      if (FConfig^.FLine[FConfig^.FRun + 1] = '/') and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 2]] =
        FConfig^.FHashTable['s']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 3]] =
        FConfig^.FHashTable['t']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 4]] =
        FConfig^.FHashTable['y']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 5]] =
        FConfig^.FHashTable['l']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 6]] =
        FConfig^.FHashTable['e']) and
        (fIdentTable2[FConfig^.FLine[FConfig^.FRun + 7]] and (1 shl 0) <> 0) and
        // (FConfig^.FLine[FConfig^.FRun+7] in [#0..#32, '>']) and
        (FConfig^.FHighlighterMode=shmHtml) then
      begin
        Result := True;
        if ADo then
        begin
          FConfig^.FTokenID := tkHtmlTag;
          SetHighlighterType(shtHtml, True, False, False);
        end;
      end
      else
        Result := False;
    else
      Result := False;
  end;
end;

procedure TSynWebEngine.Css_SpaceProc;
begin
  repeat
    Inc(FConfig^.FRun);
  until not (FConfig^.FLine[FConfig^.FRun] in [#1..#32]);
  FConfig^.FTokenID := tkCssSpace;
end;

procedure TSynWebEngine.Css_AtKeywordProc;
begin
  if fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 0) = 0 then
    // if not (FConfig^.FLine[FConfig^.FRun+1] in ['a'..'z', 'A'..'Z']) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssAtKeyword);
  end;
end;

procedure TSynWebEngine.Css_SlashProc;
begin
  if FConfig^.FLine[FConfig^.FRun + 1] = '*' then
  begin
    Inc(FConfig^.FRun, 2);
    SetRange_Bit(12, True); // Css_SetRange(rsCssComment);
    if Css_CheckNull(False) or Php_CheckBegin(False) then
      FConfig^.FTokenID := tkCssComment
    else
      Css_RangeCommentProc;
  end
  else
  if (Css_GetRange = rsCssPropVal) and GetRange_Bit(8) then
  begin
    SetRange_Bit(8, False);
    Css_SymbolProc;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_BraceOpenProc;
begin
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  if (FConfig^.FLine[FConfig^.FRun + 1] = '!') and
    (FConfig^.FLine[FConfig^.FRun + 2] = '-') and
    (FConfig^.FLine[FConfig^.FRun + 3] = '-') then
  begin
    Inc(FConfig^.FRun, 4);
    FConfig^.FTokenID := tkHtmlComment;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_CurlyBraceOpenProc;
begin
  Css_SymbolProc;
  Css_SetRange(rsCssProp);
end;

procedure TSynWebEngine.Css_CurlyBraceCloseProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssRuleset);
  end
  else
  if GetRange_Bit(11) then
  begin
    SetRange_Bit(11, False);
    Css_SymbolProc;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_ChildAnySelectorProc;
begin
  if FConfig^.FCssVersion = cvCss21 then
    Css_SymbolProc
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_AttribProc;
begin
  if FConfig^.FCssVersion = cvCss1 then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssSelectorAttrib);
  end;
end;

procedure TSynWebEngine.Css_HashProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    if (fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 10) <> 0) and
      // if FConfig^.FLine[FConfig^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9'] and
      (fIdentTable[FConfig^.FLine[FConfig^.FRun + 2]] and (1 shl 10) <> 0) and
      //   FConfig^.FLine[FConfig^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9'] and
      (fIdentTable[FConfig^.FLine[FConfig^.FRun + 3]] and (1 shl 10) <> 0) then
      //   FConfig^.FLine[FConfig^.FRun+3] in ['a'..'f', 'A'..'F', '0'..'9'] then
    begin
      Css_SymbolProc;
      Css_SetRange(rsCssPropValSpecial);
    end
    else
      Css_ErrorProc;
  end
  else
  if (fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 8) = 0) or
    // if not (FConfig^.FLine[FConfig^.FRun+1] in ['a'..'z', 'A'..'Z', '\']) or
    ((FConfig^.FLine[FConfig^.FRun + 1] = '\') and
    (FConfig^.FLine[FConfig^.FRun + 2] in [#0..#31])) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    SetRange_Bit(8, True);
  end;
end;

procedure TSynWebEngine.Css_DotProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    if FConfig^.FLine[FConfig^.FRun + 1] in ['0'..'9'] then
    begin
      FConfig^.FCssMask := $F5000000;
      Css_NumberDefProc;
    end
    else
      Css_ErrorProc;
  end
  else
  begin
    if (fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 8) = 0) or
      // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
      ((FConfig^.FLine[FConfig^.FRun + 1] = '\') and
      (FConfig^.FLine[FConfig^.FRun + 2] in [#0..#31])) then
    begin
      Css_ErrorProc;
      Exit;
    end;
    Css_SymbolProc;
    SetRange_Bit(9, True);
  end;
end;

procedure TSynWebEngine.Css_CommaProc;
var
  prop: integer;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    prop := Css_GetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 16) = 0) then
    begin
      Css_ErrorProc;
      Exit;
    end;
  end;
  Css_SymbolProc;
end;

procedure TSynWebEngine.Css_ColonProc;
begin
  if fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 0) = 0 then
    // if not (FConfig^.FLine[FConfig^.FRun+1] in ['a'..'z', 'A'..'Z']) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssSelectorPseudo);
  end;
end;

procedure TSynWebEngine.Css_SemiColonProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssProp);
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_ExclamationProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssPropValImportant);
    SetRange_Bit(8, False);
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_StringProc;
var
  prop: integer;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    FConfig^.FTokenID := tkCssValString;
    if FConfig^.FLine[FConfig^.FRun] = #39 then
    begin
      Inc(FConfig^.FRun);
      if not Css_CustomStringProc(TCssString39, False) then
      begin
        Css_SetRange(rsCssPropValStr);
        SetRange_Bit(8, True);
      end;
    end
    else
    begin
      Inc(FConfig^.FRun);
      if not Css_CustomStringProc(TCssString34, False) then
      begin
        Css_SetRange(rsCssPropValStr);
        SetRange_Bit(9, True);
      end;
    end;
    if FConfig^.FTokenID = tkCssValString then
    begin
      prop := Css_GetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
        FConfig^.FTokenID := tkCssValUndef;
    end;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_PlusProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    Inc(FConfig^.FRun);
    if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 13) <> 0 then
      // if FConfig^.FLine[FConfig^.FRun] in ['0'..'9', '.'] then
    begin
      FConfig^.FCssMask := $F5400000;
      Css_NumberDefProc;
    end
    else
      FConfig^.FTokenID := tkCssError;
  end
  else
  if FConfig^.FCssVersion = cvCss21 then
    Css_SymbolProc
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_MinusProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    Inc(FConfig^.FRun);
    if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 13) <> 0 then
      // if FConfig^.FLine[FConfig^.FRun] in ['0'..'9', '.'] then
    begin
      FConfig^.FCssMask := $8AA00000;
      Css_NumberDefProc;
    end
    else
      FConfig^.FTokenID := tkCssError;
  end
  else
  if (Css_GetRange = rsCssRuleset) and (FConfig^.FLine[FConfig^.FRun + 1] = '-') and
    (FConfig^.FLine[FConfig^.FRun + 2] = '>') then
  begin
    Inc(FConfig^.FRun, 3);
    FConfig^.FTokenID := tkHtmlComment;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_NumberProc;
begin
  if Css_GetRange = rsCssPropVal then
  begin
    FConfig^.FCssMask := $F5400000;
    Css_NumberDefProc;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_NumberDefProc;
var
  prop, OldRun: integer;

  procedure CheckOther;
  begin
    if (FConfig^.FRun - FConfig^.FTokenPos = 1) and
      (FConfig^.FLine[FConfig^.FRun - 1] = '0') then
      FConfig^.FCssMask := FConfig^.FCssMask and $F5400000
    else
      FConfig^.FCssMask := FConfig^.FCssMask and $01E00000;
    if (FConfig^.FTokenPos > 1) and ((FConfig^.FLine[FConfig^.FTokenPos - 1] = '/') and
      (FConfig^.FLine[FConfig^.FTokenPos - 2] <> '*')) then
      FConfig^.FCssMask := FConfig^.FCssMask or $18000000;
  end;

begin
  while FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] do
    Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '.' then
  begin
    FConfig^.FCssMask := FConfig^.FCssMask and $FF800000;
    Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
      repeat
        Inc(FConfig^.FRun);
      until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9'])
    else
    begin
      FConfig^.FTokenID := tkCssError;
      Exit;
    end;
  end;
  if (FConfig^.FLine[FConfig^.FRun] = '%') then
  begin
    FConfig^.FCssMask := FConfig^.FCssMask and $06000000;
    Css_SetRange(rsCssPropValSpecial);
  end
  else
  begin
    OldRun := FConfig^.FRun;
    if Css_IdentStartProc then
    begin
      prop := Css_SpecialCheck(OldRun, FConfig^.FRun - OldRun);
      if prop <> -1 then
      begin
        FConfig^.FCssMask := FConfig^.FCssMask and TSynWeb_CssSpecialData[prop];
        Css_SetRange(rsCssPropValSpecial);
        if (FConfig^.FLine[FConfig^.FRun] = '/') and
          (FConfig^.FLine[FConfig^.FRun + 1] <> '*') then
          SetRange_Bit(8, True);
        FConfig^.FRun := OldRun;
      end
      else
      if FConfig^.FCssVersion = cvCss1 then
      begin
        FConfig^.FRun := OldRun;
        CheckOther;
      end
      else
      begin
        FConfig^.FTokenID := tkCssError;
        Exit;
      end;
    end
    else
      CheckOther;
  end;
  prop := Css_GetProp - 1;
  if (prop = -1) or (TSynWeb_CssPropsData[prop] and FConfig^.FCssMask = 0) then
    FConfig^.FTokenID := tkCssValUndef
  else
    FConfig^.FTokenID := tkCssValNumber;
end;

procedure TSynWebEngine.Css_IdentProc;
begin
  if Css_IdentStartProc then
  begin
    if (Html_TagCheck = tkHtmlTagName) and
      (TSynWeb_TagsData[Html_GetTag - 1] and (1 shl 30) = 0) then
      FConfig^.FTokenID := tkCssSelector
    else
      FConfig^.FTokenID := tkCssSelectorUndef;
  end
  else
    Css_ErrorProc;
end;

function TSynWebEngine.Css_IdentStartProc: boolean;
begin
  if (fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 8) = 0) or
    // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
    ((FConfig^.FLine[FConfig^.FRun] = '\') and
    (FConfig^.FLine[FConfig^.FRun + 1] in [#0..#31])) then
  begin
    Result := False;
    Exit;
  end;
  FConfig^.FStringLenClean := 0;
  repeat
    if FConfig^.FLine[FConfig^.FRun] <> '\' then
      Inc(FConfig^.FRun)
    else
    if not (FConfig^.FLine[FConfig^.FRun + 1] in [#0..#31]) then
    begin
      Inc(FConfig^.FStringLenClean);
      Inc(FConfig^.FRun, 2);
    end
    else
      Break;
  until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 9) = 0;
  // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '\', '0'..'9', '-', '_']);
  FConfig^.FStringLenClean := FConfig^.FRun - FConfig^.FTokenPos -
    FConfig^.FStringLenClean;
  Result := True;
end;

function TSynWebEngine.Css_CustomStringProc(AShl: longword; ADo: boolean): boolean;
begin
  if Css_CheckNull(ADo) then
  begin
    if not ADo then
      FConfig^.FTokenID := tkCssError;
    Result := True;
    Exit;
  end
  else
  if Php_CheckBegin(ADo) then
  begin
    if not ADo then
      FConfig^.FTokenID := tkCssValString;
    Result := False;
    Exit;
  end;
  Result := True;
  AShl := 1 shl AShl;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and AShl = 0 do
      // while not (FConfig^.FLine[FConfig^.FRun] in [#0, AChar, '\', '<']) do
      Inc(FConfig^.FRun);
    case FConfig^.FLine[FConfig^.FRun] of
      #39, '"':
      begin
        Inc(FConfig^.FRun);
        FConfig^.FTokenID := tkCssValString;
        Exit;
      end;
      '\':
      begin
        Inc(FConfig^.FRun);
        if FConfig^.FLine[FConfig^.FRun] = #0 then
        begin
          if FConfig^.FCssVersion = cvCss1 then
          begin
            FConfig^.FTokenID := tkCssError;
            Exit;
          end
          else
          begin
            FConfig^.FTokenID := tkCssValString;
            Result := False;
            Exit;
          end;
        end
        else
        if not Css_CheckNull(False) and not Php_CheckBegin(False) then
          Inc(FConfig^.FRun);
      end;
      else
        if Css_CheckNull(False) then
        begin
          FConfig^.FTokenID := tkCssError;
          Exit;
        end
        else
        if Php_CheckBegin(False) then
        begin
          FConfig^.FTokenID := tkCssValString;
          Result := False;
          Exit;
        end
        else
          Inc(FConfig^.FRun);
    end;
  until False;
end;

function TSynWebEngine.Css_NotWhitespace: boolean;
begin
  Result := False;
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 11) <> 0 then
    // if FConfig^.FLine[FConfig^.FRun] in [#0..#32, '/'] then
    fCss_ProcTable[FConfig^.FLine[FConfig^.FRun]]
  else
    Result := True;
end;

procedure TSynWebEngine.Css_SymbolProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkCssSymbol;
end;

procedure TSynWebEngine.Css_ErrorProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkCssError;
end;

procedure TSynWebEngine.Css_RangeRulesetProc;
begin
  if GetRange_Bit(8) then
  begin
    SetRange_Bit(8, False);
    Css_IdentStartProc;
    FConfig^.FTokenID := tkCssSelectorId;
  end
  else
  if GetRange_Bit(9) then
  begin
    SetRange_Bit(9, False);
    Css_IdentStartProc;
    FConfig^.FTokenID := tkCssSelectorClass;
  end
  else
    fCss_ProcTable[FConfig^.FLine[FConfig^.FRun]];
end;

procedure TSynWebEngine.Css_RangeSelectorAttribProc;

  procedure DoError;
  begin
    Css_SetRange(rsCssRuleset);
    fCss_ProcTable[FConfig^.FLine[FConfig^.FRun]];
    FConfig^.FTokenID := tkCssError;
  end;

  procedure DoEndAttrib;
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssRuleset);
  end;

begin
  case GetRange_Int(3, 8) of
    0:
      if Css_NotWhitespace then
        if Css_IdentStartProc then
        begin
          FConfig^.FTokenID := tkCssVal;
          SetRange_Int(3, 8, 1);
        end
        else
          DoError;
    1:
      if Css_NotWhitespace then
        case FConfig^.FLine[FConfig^.FRun] of
          '=':
          begin
            Css_SymbolProc;
            SetRange_Int(3, 8, 2);
          end;
          '|', '~':
          begin
            SetRange_Int(3, 8, 2);
            if FConfig^.FLine[FConfig^.FRun + 1] = '=' then
            begin
              Inc(FConfig^.FRun, 2);
              FConfig^.FTokenID := tkCssSymbol;
            end
            else
              Css_ErrorProc;
          end;
          ']':
            DoEndAttrib;
          else
            DoError;
        end;
    2:
      if Css_NotWhitespace then
        case FConfig^.FLine[FConfig^.FRun] of
          #39:
          begin
            Inc(FConfig^.FRun);
            if Css_CustomStringProc(TCssString39, False) then
              SetRange_Int(3, 8, 5)
            else
              SetRange_Int(3, 8, 3);
          end;
          '"':
          begin
            Inc(FConfig^.FRun);
            if Css_CustomStringProc(TCssString34, False) then
              SetRange_Int(3, 8, 5)
            else
              SetRange_Int(3, 8, 4);
          end;
          else
            if Css_IdentStartProc then
            begin
              FConfig^.FTokenID := tkCssValString;
              SetRange_Int(3, 8, 5);
            end
            else
              DoError;
        end;
    3:
      if Css_CustomStringProc(TCssString39) then
        SetRange_Int(3, 8, 5);
    4:
      if Css_CustomStringProc(TCssString34) then
        SetRange_Int(3, 8, 5);
    5:
      if Css_NotWhitespace then
        if FConfig^.FLine[FConfig^.FRun] = ']' then
          DoEndAttrib
        else
          DoError;
  end;
end;

procedure TSynWebEngine.Css_RangeSelectorPseudoProc;
var
  prop: integer;
begin
  if not GetRange_Bit(10) then
  begin
    repeat
      Inc(FConfig^.FRun);
    until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0;
    // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z']);
    prop := Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun - FConfig^.FTokenPos);
    if (prop = -1) or (TSynWeb_CssSpecialData[prop] and
      (1 shl (15 - Longword(FConfig^.FCssVersion))) = 0) then
    begin
      FConfig^.FTokenID := tkCssError;
      Css_SetRange(rsCssRuleset);
    end
    else
    if (prop <> Css_SpecialID_Lang) then
    begin
      FConfig^.FTokenID := tkCssSpecial;
      Css_SetRange(rsCssRuleset);
    end
    else
    if (FConfig^.FLine[FConfig^.FRun] = '(') then
    begin
      FConfig^.FTokenID := tkCssSpecial;
      SetRange_Bit(10, True);
    end
    else
    begin
      FConfig^.FTokenID := tkCssError;
      Css_SetRange(rsCssRuleset);
    end;
  end
  else
  if not GetRange_Bit(9) then
  begin
    Css_SymbolProc;
    SetRange_Bit(9, True);
  end
  else
  if Css_NotWhitespace then
    case FConfig^.FLine[FConfig^.FRun] of
      ',':
        if GetRange_Bit(8) then
        begin
          SetRange_Bit(8, False);
          Css_SymbolProc;
        end
        else
          Css_ErrorProc;
      ')':
      begin
        if GetRange_Bit(8) then
          Css_SymbolProc
        else
          Css_ErrorProc;
        Css_SetRange(rsCssRuleset);
      end;
      else
        if Css_IdentStartProc then
          if GetRange_Bit(8) then
            FConfig^.FTokenID := tkCssError
          else
          begin
            FConfig^.FTokenID := tkCssVal;
            SetRange_Bit(8, True);
          end
        else
        begin
          Css_SetRange(rsCssRuleset);
          fCss_ProcTable[FConfig^.FLine[FConfig^.FRun]];
          FConfig^.FTokenID := tkCssError;
        end;
    end;
end;

procedure TSynWebEngine.Css_RangeAtKeywordProc;
var
  prop: integer;

  procedure DoError;
  begin
    Css_SetRange(rsCssRuleset);
    fCss_ProcTable[FConfig^.FLine[FConfig^.FRun]];
    FConfig^.FTokenID := tkCssError;
  end;

  procedure AtImport;

    procedure AtImport_Medium(ASimple: boolean);
    begin
      if Css_NotWhitespace then
        if not ASimple and (FConfig^.FLine[FConfig^.FRun] = ';') then
        begin
          Css_SymbolProc;
          Css_SetRange(rsCssRuleset);
        end
        else
        if Css_IdentStartProc then
        begin
          prop := Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun -
            FConfig^.FTokenPos);
          if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
            FConfig^.FTokenID := tkCssValUndef
          else
            FConfig^.FTokenID := tkCssVal;
          SetRange_Int(4, 4, 9);
        end
        else
          DoError;
    end;

  begin
    case GetRange_Int(4, 4) of
      0:
        if Css_NotWhitespace then
          case FConfig^.FLine[FConfig^.FRun] of
            #39:
            begin
              Inc(FConfig^.FRun);
              if Css_CustomStringProc(TCssString39, False) then
                SetRange_Int(4, 4, 8)
              else
                SetRange_Int(4, 4, 1);
            end;
            '"':
            begin
              Inc(FConfig^.FRun);
              if Css_CustomStringProc(TCssString34, False) then
                SetRange_Int(4, 4, 8)
              else
                SetRange_Int(4, 4, 2);
            end;
            else
              if not Css_IdentStartProc then
                DoError
              else
              if (Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun -
                FConfig^.FTokenPos) = Css_SpecialID_Url) and
                (FConfig^.FLine[FConfig^.FRun] = '(') then
              begin
                FConfig^.FTokenID := tkCssVal;
                SetRange_Int(4, 4, 3);
              end
              else
              begin
                FConfig^.FTokenID := tkCssValUndef;
                SetRange_Int(4, 4, 8);
              end;
          end;
      1:
        if Css_CustomStringProc(TCssString39) then
          SetRange_Int(4, 4, 8);
      2:
        if Css_CustomStringProc(TCssString34) then
          SetRange_Int(4, 4, 8);
      3:
      begin
        Css_SymbolProc;
        SetRange_Int(4, 4, 4);
      end;
      4:
        case FConfig^.FLine[FConfig^.FRun] of
          #39:
          begin
            Inc(FConfig^.FRun);
            if Css_CustomStringProc(TCssString39, False) then
              SetRange_Int(4, 4, 7)
            else
              SetRange_Int(4, 4, 5);
          end;
          '"':
          begin
            Inc(FConfig^.FRun);
            if Css_CustomStringProc(TCssString34, False) then
              SetRange_Int(4, 4, 7)
            else
              SetRange_Int(4, 4, 6);
          end;
          #0..#32:
            Css_SpaceProc;
          else
            if (FConfig^.FLine[FConfig^.FRun] = '/') and
              (FConfig^.FLine[FConfig^.FRun + 1] = '*') then
              Css_SlashProc
            else
            begin
              if Css_CheckNull or Php_CheckBegin then
                Exit;
              repeat
                while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 14) = 0 do
                  // while not (FConfig^.FLine[FConfig^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
                  Inc(FConfig^.FRun);
                case FConfig^.FLine[FConfig^.FRun] of
                  '\':
                  begin
                    Inc(FConfig^.FRun);
                    if FConfig^.FLine[FConfig^.FRun] <> #0 then
                      Inc(FConfig^.FRun)
                    else
                      Break;
                  end;
                  '<':
                    if Css_CheckNull(False) or Php_CheckBegin(False) then
                      Break
                    else
                      Inc(FConfig^.FRun);
                  else
                    Break;
                end;
              until False;
              FConfig^.FTokenID := tkCssValString;
              SetRange_Int(4, 4, 7);
            end;
        end;
      5:
        if Css_CustomStringProc(TCssString39) then
          SetRange_Int(4, 4, 7);
      6:
        if Css_CustomStringProc(TCssString34) then
          SetRange_Int(4, 4, 7);
      7:
        if Css_NotWhitespace then
          if FConfig^.FLine[FConfig^.FRun] = ')' then
          begin
            Css_SymbolProc;
            SetRange_Int(4, 4, 8);
          end
          else
            DoError;
      8:
        AtImport_Medium(False);
      9:
        if Css_NotWhitespace then
          case FConfig^.FLine[FConfig^.FRun] of
            ';':
            begin
              Css_SymbolProc;
              Css_SetRange(rsCssRuleset);
            end;
            ',':
            begin
              Css_SymbolProc;
              SetRange_Int(4, 4, 10);
            end;
            else
              DoError;
          end;
      10:
        AtImport_Medium(True);
    end;
  end;

  procedure AtMedia;
  var
    prop: integer;
  begin
    if Css_NotWhitespace then
      if GetRange_Bit(7) then
      begin
        SetRange_Bit(7, False);
        case FConfig^.FLine[FConfig^.FRun] of
          ',':
            Css_SymbolProc;
          '{':
          begin
            Css_SymbolProc;
            SetRange_Bit(11, True);
            Css_SetRange(rsCssRuleset);
          end;
          else
            DoError;
        end;
      end
      else
      if Css_IdentStartProc then
      begin
        prop := Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun - FConfig^.FTokenPos);
        if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
          FConfig^.FTokenID := tkCssValUndef
        else
          FConfig^.FTokenID := tkCssVal;
        SetRange_Bit(7, True);
      end
      else
        DoError;
  end;

  procedure AtPage;
  var
    prop: integer;

    procedure AtPage_Declaration;
    begin
      SetRange_Int(11, 0, 0);
      Css_SymbolProc;
      Css_SetRange(rsCssProp);
    end;

  begin
    case GetRange_Int(2, 6) of
      0:
        if Css_NotWhitespace then
          case FConfig^.FLine[FConfig^.FRun] of
            '{':
              AtPage_Declaration;
            ':':
              if (fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 8) = 0) or
                // if not (FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
                ((FConfig^.FLine[FConfig^.FRun + 1] = '\') and
                (FConfig^.FLine[FConfig^.FRun + 2] in [#0..#31])) then
                DoError
              else
              begin
                SetRange_Int(2, 6, 1);
                Css_SymbolProc;
              end;
            else
              DoError;
          end;
      1:
      begin
        Css_IdentStartProc;
        prop := Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun - FConfig^.FTokenPos);
        if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 11) = 0) then
          FConfig^.FTokenID := tkCssError
        else
          FConfig^.FTokenID := tkCssSpecial;
        SetRange_Int(2, 6, 2);
      end;
      2:
        if Css_NotWhitespace then
          if FConfig^.FLine[FConfig^.FRun] = '{' then
            AtPage_Declaration
          else
            DoError;
    end;
  end;

  procedure AtCharset;
  begin
    case GetRange_Int(2, 6) of
      0:
        if Css_NotWhitespace then
          case FConfig^.FLine[FConfig^.FRun] of
            #39:
            begin
              Inc(FConfig^.FRun);
              if Css_CustomStringProc(TCssString39, False) then
                SetRange_Int(2, 6, 3)
              else
                SetRange_Bit(6, True);
            end;
            '"':
            begin
              Inc(FConfig^.FRun);
              if Css_CustomStringProc(TCssString34, False) then
                SetRange_Int(2, 6, 3)
              else
                SetRange_Bit(7, True);
            end;
            else
              DoError;
          end;
      1:
        if Css_CustomStringProc(TCssString39) then
          SetRange_Int(2, 6, 3);
      2:
        if Css_CustomStringProc(TCssString34) then
          SetRange_Int(2, 6, 3);
      3:
        if Css_NotWhitespace then
          if FConfig^.FLine[FConfig^.FRun] = ';' then
          begin
            Css_SymbolProc;
            Css_SetRange(rsCssRuleset);
          end
          else
            DoError;
    end;
  end;

begin
  if not GetRange_Bit(10) then
  begin
    SetRange_Bit(10, True);
    repeat
      Inc(FConfig^.FRun);
    until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 0) = 0;
    // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z']);
    if GetRange_Bit(11) then
    begin
      FConfig^.FTokenID := tkCssError;
      Css_SetRange(rsCssRuleset);
    end
    else
      case Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun - FConfig^.FTokenPos) of
        Css_SpecialID_Import:
        begin
          SetRange_Int(2, 8, 0);
          FConfig^.FTokenID := tkCssSpecial;
        end;
        Css_SpecialID_Media:
          if FConfig^.FCssVersion = cvCss1 then
          begin
            FConfig^.FTokenID := tkCssError;
            Css_SetRange(rsCssRuleset);
          end
          else
          begin
            SetRange_Int(2, 8, 1);
            FConfig^.FTokenID := tkCssSpecial;
          end;
        Css_SpecialID_Page:
          if FConfig^.FCssVersion = cvCss1 then
          begin
            FConfig^.FTokenID := tkCssError;
            Css_SetRange(rsCssRuleset);
          end
          else
          begin
            SetRange_Int(2, 8, 2);
            FConfig^.FTokenID := tkCssSpecial;
          end;
        Css_SpecialID_Charset:
          if FConfig^.FCssVersion = cvCss1 then
          begin
            FConfig^.FTokenID := tkCssError;
            Css_SetRange(rsCssRuleset);
          end
          else
          begin
            SetRange_Int(2, 8, 3);
            FConfig^.FTokenID := tkCssSpecial;
          end;
        else
          FConfig^.FTokenID := tkCssError;
          Css_SetRange(rsCssRuleset);
      end;
  end
  else
    case GetRange_Int(2, 8) of
      0:
        AtImport;
      1:
        AtMedia;
      2:
        AtPage;
      3:
        AtCharset;
    end;
end;

procedure TSynWebEngine.Css_RangePropProc;
begin
  if GetRange_Bit(8) then
  begin
    if Css_NotWhitespace then
      case FConfig^.FLine[FConfig^.FRun] of
        '}':
        begin
          Css_ErrorProc;
          Css_SetRange(rsCssRuleset);
        end;
        ':':
        begin
          Css_SymbolProc;
          Css_SetRange(rsCssPropVal);
          SetRange_Bit(8, False);
        end;
        else
          Css_ErrorProc;
      end;
  end
  else
  if Css_NotWhitespace then
    if Css_IdentStartProc then
    begin
      FConfig^.FTokenID := Css_PropCheck;
      SetRange_Bit(8, True);
    end
    else
    begin
      Css_SetProp(0);
      case FConfig^.FLine[FConfig^.FRun] of
        '}':
          Css_SetRange(rsCssRuleset);
        ':':
          Css_SetRange(rsCssPropVal);
      end;
      Css_ErrorProc;
    end;
end;

procedure TSynWebEngine.Css_RangePropValProc;
begin
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 12) <> 0 then
    // if FConfig^.FLine[FConfig^.FRun] in [#0..#32, '/', '#', '!', ';', '}', '+', '-', '0'..'9', '.', ',', '"', #39, '<'] then
    fCss_ProcTable[FConfig^.FLine[FConfig^.FRun]]
  else
  if Css_IdentStartProc then
  begin
    FConfig^.FTokenID := Css_ValCheck;
    if TSynWeb_CssValsData[FConfig^.FToken_LastID][Longword(FConfig^.FCssVersion)]
      [3] and (1 shl 31) <> 0 then
      if FConfig^.FLine[FConfig^.FRun] = '(' then
      begin
        SetRange_Int(3, 8, 0);
        case FConfig^.FToken_LastID of
          Css_ValID_Rgb:
            Css_SetRange(rsCssPropValRgb);
          Css_ValID_Url:
            Css_SetRange(rsCssPropValUrl);
          Css_ValID_Rect:
            Css_SetRange(rsCssPropValRect);
          else
            Css_SetRange(rsCssPropValFunc);
        end;
      end
      else
        FConfig^.FTokenID := tkCssValUndef;
  end
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_RangePropValStrProc;
var
  prop: integer;
begin
  if GetRange_Bit(8) then
  begin
    if Css_CustomStringProc(TCssString39) then
    begin
      Css_SetRange(rsCssPropVal);
      SetRange_Bit(8, False);
    end;
  end
  else
  if Css_CustomStringProc(TCssString34) then
  begin
    Css_SetRange(rsCssPropVal);
    SetRange_Bit(9, False);
  end;
  if FConfig^.FTokenID = tkCssValString then
  begin
    prop := Css_GetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
      FConfig^.FTokenID := tkCssValUndef;
  end;
end;

procedure TSynWebEngine.Css_RangePropValRgbProc;

  procedure NumberProc;
  begin
    if GetRange_Bit(8) then
      FConfig^.FTokenID := tkCssError
    else
      FConfig^.FTokenID := tkCssValNumber;
    SetRange_Bit(8, True);
    if FConfig^.FLine[FConfig^.FRun] = '+' then
      if FConfig^.FLine[FConfig^.FRun + 1] in ['0'..'9', '.'] then
        Inc(FConfig^.FRun)
      else
      begin
        Css_ErrorProc;
        Exit;
      end;
    while FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] do
      Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '.' then
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
      begin
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9']);
        if FConfig^.FLine[FConfig^.FRun] = '%' then
          Exit;
      end;
      FConfig^.FTokenID := tkCssError;
    end;
  end;

begin
  if GetRange_Bit(10) then
  begin
    if Css_NotWhitespace then
      case FConfig^.FLine[FConfig^.FRun] of
        ',':
          if GetRange_Bit(8) then
          begin
            SetRange_Bit(8, False);
            Css_SymbolProc;
          end
          else
            Css_ErrorProc;
        '0'..'9', '.', '+':
          NumberProc;
        '%':
          if (FConfig^.FRun > 0) and (FConfig^.FLine[FConfig^.FRun - 1] in
            ['0'..'9']) then
            Css_SymbolProc
          else
            Css_ErrorProc;
        ';':
        begin
          Css_ErrorProc;
          Css_SetRange(rsCssProp);
          SetRange_Int(3, 8, 0);
        end;
        '}':
        begin
          Css_ErrorProc;
          Css_SetRange(rsCssRuleset);
        end;
        ')':
        begin
          if GetRange_Bit(8) then
            Css_SymbolProc
          else
            Css_ErrorProc;
          Css_SetRange(rsCssPropVal);
          SetRange_Int(3, 8, 0);
        end;
        else
          Css_SetRange(rsCssPropVal);
          SetRange_Int(3, 8, 0);
          Css_RangePropValProc;
          FConfig^.FTokenID := tkCssError;
      end;
  end
  else
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end;
end;

procedure TSynWebEngine.Css_RangePropValFuncProc;
begin
  if GetRange_Bit(10) then
    case GetRange_Int(2, 8) of
      0:
        if Css_NotWhitespace then
          case FConfig^.FLine[FConfig^.FRun] of
            #39:
            begin
              Inc(FConfig^.FRun);
              if not Css_CustomStringProc(TCssString39, False) then
                SetRange_Bit(8, True);
            end;
            '"':
            begin
              Inc(FConfig^.FRun);
              if not Css_CustomStringProc(TCssString34, False) then
                SetRange_Bit(9, True);
            end;
            ',':
              Css_SymbolProc;
            ';':
            begin
              Css_ErrorProc;
              Css_SetRange(rsCssProp);
              SetRange_Int(3, 8, 0);
            end;
            '}':
            begin
              Css_ErrorProc;
              Css_SetRange(rsCssRuleset);
            end;
            ')':
            begin
              Css_SymbolProc;
              Css_SetRange(rsCssPropVal);
              SetRange_Int(3, 8, 0);
            end;
            else
              if Css_IdentStartProc then
                FConfig^.FTokenID := tkCssVal
              else
                Css_ErrorProc;
          end;
      1:
        if Css_CustomStringProc(TCssString39) then
          SetRange_Bit(8, False);
      2:
        if Css_CustomStringProc(TCssString34) then
          SetRange_Bit(9, False);
    end
  else
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end;
end;

procedure TSynWebEngine.Css_RangePropValSpecialProc;
var
  prop: integer;
begin
  if FConfig^.FLine[FConfig^.FRun] = '%' then
    Css_SymbolProc
  else
  if (FConfig^.FRun > 0) and (FConfig^.FLine[FConfig^.FRun - 1] = '#') then
  begin
    Inc(FConfig^.FRun, 3);
    if (fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) <> 0) and
      // if (FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) and
      (fIdentTable[FConfig^.FLine[FConfig^.FRun + 1]] and (1 shl 10) <> 0) and
      // if (FConfig^.FLine[FConfig^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9']) and
      (fIdentTable[FConfig^.FLine[FConfig^.FRun + 2]] and (1 shl 10) <> 0) then
      // if (FConfig^.FLine[FConfig^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9']) then
      Inc(FConfig^.FRun, 3);
    prop := Css_GetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 18) = 0) then
      FConfig^.FTokenID := tkCssValUndef
    else
      FConfig^.FTokenID := tkCssValNumber;
  end
  else
  begin
    Css_IdentStartProc;
    FConfig^.FTokenID := tkCssSymbol;
  end;
  Css_SetRange(rsCssPropVal);
end;

procedure TSynWebEngine.Css_RangePropValImportantProc;

  procedure DoSymbol;
  begin
    if GetRange_Bit(8) then
    begin
      SetRange_Bit(8, False);
      Css_SymbolProc;
    end
    else
      Css_ErrorProc;
  end;

begin
  if Css_NotWhitespace then
    case FConfig^.FLine[FConfig^.FRun] of
      ';':
      begin
        DoSymbol;
        Css_SetRange(rsCssProp);
      end;
      '}':
      begin
        DoSymbol;
        Css_SetRange(rsCssRuleset);
      end;
      else
        if Css_IdentStartProc then
        begin
          if GetRange_Bit(8) then
            FConfig^.FTokenID := tkCssError
          else
          begin
            Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun - FConfig^.FTokenPos);
            if FConfig^.FToken_LastID = Css_SpecialID_Important then
              FConfig^.FTokenID := tkCssSpecial
            else
              FConfig^.FTokenID := tkCssError;
            SetRange_Bit(8, True);
          end;
        end
        else
          Css_ErrorProc;
    end;
end;

procedure TSynWebEngine.Css_RangePropValUrlProc;
begin
  if GetRange_Bit(10) then
    case GetRange_Int(2, 8) of
      0:
        case FConfig^.FLine[FConfig^.FRun] of
          #39:
          begin
            Inc(FConfig^.FRun);
            if Css_CustomStringProc(TCssString39, False) then
              SetRange_Int(2, 8, 3)
            else
              SetRange_Bit(8, True);
          end;
          '"':
          begin
            Inc(FConfig^.FRun);
            if Css_CustomStringProc(TCssString34, False) then
              SetRange_Int(2, 8, 3)
            else
              SetRange_Bit(9, True);
          end;
          #1..#32:
            Css_SpaceProc;
          ';':
          begin
            Css_ErrorProc;
            Css_SetRange(rsCssProp);
            SetRange_Int(3, 8, 0);
          end;
          '}':
          begin
            Css_ErrorProc;
            Css_SetRange(rsCssRuleset);
          end;
          ')':
          begin
            Css_ErrorProc;
            Css_SetRange(rsCssPropVal);
            SetRange_Int(3, 8, 0);
          end;
          else
            if (FConfig^.FLine[FConfig^.FRun] = '/') and
              (FConfig^.FLine[FConfig^.FRun + 1] = '*') then
              Css_SlashProc
            else
            begin
              if Css_CheckNull or Php_CheckBegin then
                Exit;
              repeat
                while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 14) = 0 do
                  // while not (FConfig^.FLine[FConfig^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
                  Inc(FConfig^.FRun);
                case FConfig^.FLine[FConfig^.FRun] of
                  '\':
                  begin
                    Inc(FConfig^.FRun);
                    if FConfig^.FLine[FConfig^.FRun] <> #0 then
                      Inc(FConfig^.FRun)
                    else
                      Break;
                  end;
                  '<':
                    if Css_CheckNull(False) or Php_CheckBegin(False) then
                      Break
                    else
                      Inc(FConfig^.FRun);
                  else
                    Break;
                end;
              until False;
              FConfig^.FTokenID := tkCssValString;
              SetRange_Int(2, 8, 3);
            end;
        end;
      1:
        if Css_CustomStringProc(TCssString39) then
          SetRange_Int(2, 8, 3);
      2:
        if Css_CustomStringProc(TCssString34) then
          SetRange_Int(2, 8, 3);
      3:
        if FConfig^.FLine[FConfig^.FRun] = ')' then
        begin
          Css_SymbolProc;
          SetRange_Int(3, 8, 0);
          Css_SetRange(rsCssPropVal);
        end
        else
        if Css_NotWhitespace then
        begin
          SetRange_Int(3, 8, 0);
          Css_SetRange(rsCssPropVal);
          Css_RangePropValProc;
          FConfig^.FTokenID := tkCssError;
        end;
    end
  else
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end;
end;

procedure TSynWebEngine.Css_RangePropValRectProc;

  procedure Shape_LengthProc;
  var
    prop, OldRun: integer;

    procedure CheckOther;
    begin
      if GetRange_Bit(8) then
        FConfig^.FTokenID := tkCssError
      else
      if (FConfig^.FRun - FConfig^.FTokenPos = 1) and
        (FConfig^.FLine[FConfig^.FRun - 1] = '0') then
        FConfig^.FTokenID := tkCssValNumber
      else
        FConfig^.FTokenID := tkCssValUndef;
      SetRange_Bit(8, True);
    end;

  begin
    while FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] do
      Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '.' then
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9'])
      else
      begin
        FConfig^.FTokenID := tkCssError;
        Exit;
      end;
    end;
    OldRun := FConfig^.FRun;
    if Css_IdentStartProc then
    begin
      prop := Css_SpecialCheck(OldRun, FConfig^.FRun - OldRun);
      if prop <> -1 then
      begin
        FConfig^.FRun := OldRun;
        SetRange_Bit(9, True);
        if (TSynWeb_CssSpecialData[prop] and (1 shl 28) = 0) or GetRange_Bit(8) then
          FConfig^.FTokenID := tkCssError
        else
          FConfig^.FTokenID := tkCssValNumber;
      end
      else
      if FConfig^.FCssVersion = cvCss1 then
      begin
        FConfig^.FRun := OldRun;
        CheckOther;
      end
      else
      begin
        FConfig^.FTokenID := tkCssError;
        Exit;
      end;
    end
    else
      CheckOther;
  end;

begin
  if not GetRange_Bit(10) then
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end
  else
  if GetRange_Bit(9) then
  begin
    Css_IdentStartProc;
    if GetRange_Bit(8) then
      FConfig^.FTokenID := tkCssError
    else
      FConfig^.FTokenID := tkCssSymbol;
    SetRange_Bit(9, False);
    SetRange_Bit(8, True);
  end
  else
  if Css_NotWhitespace then
    case FConfig^.FLine[FConfig^.FRun] of
      ',':
        if GetRange_Bit(8) then
        begin
          SetRange_Bit(8, False);
          Css_SymbolProc;
        end
        else
          Css_ErrorProc;
      '0'..'9', '.':
        Shape_LengthProc;
      ')':
      begin
        if GetRange_Bit(8) then
          Css_SymbolProc
        else
          Css_ErrorProc;
        Css_SetRange(rsCssPropVal);
        SetRange_Int(3, 8, 0);
      end;
      ';':
      begin
        Css_ErrorProc;
        Css_SetRange(rsCssProp);
        SetRange_Int(3, 8, 0);
      end;
      '}':
      begin
        Css_ErrorProc;
        Css_SetRange(rsCssRuleset);
        SetRange_Int(3, 8, 0);
      end;
      else
        if not Css_IdentStartProc then
          Css_ErrorProc
        else
        begin
          if GetRange_Bit(8) then
            FConfig^.FTokenID := tkCssError
          else
          if Css_SpecialCheck(FConfig^.FTokenPos, FConfig^.FRun -
            FConfig^.FTokenPos) = Css_SpecialID_Auto then
            FConfig^.FTokenID := tkCssVal
          else
            FConfig^.FTokenID := tkCssValUndef;
          SetRange_Bit(8, True);
        end;
    end;
end;

procedure TSynWebEngine.Css_RangeCommentProc;
begin
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 26) = 0 do
      // while not (FConfig^.FLine[FConfig^.FRun] in [#0, '*', '<']) do
      Inc(FConfig^.FRun);
    case FConfig^.FLine[FConfig^.FRun] of
      #0:
        Break;
      '<':
        if Css_CheckNull(False) or Php_CheckBegin(False) then
          Break
        else
          Inc(FConfig^.FRun);
      '*':
      begin
        Inc(FConfig^.FRun);
        if FConfig^.FLine[FConfig^.FRun] = '/' then
        begin
          Inc(FConfig^.FRun);
          SetRange_Bit(12, False);
          Break;
        end;
      end;
    end;
  until False;
  FConfig^.FTokenID := tkCssComment;
end;

function TSynWebEngine.Css_PropKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_CssProps[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLenClean then
  begin
    if FConfig^.FStringLenClean = FConfig^.FStringLen then
      for i := 1 to FConfig^.FStringLen do
      begin
        if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end
    else
      for i := 1 to FConfig^.FStringLenClean do
      begin
        if Temp^ = '\' then
          Inc(Temp);
        if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Css_PropCheck: TtkTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := FConfig^.FRun - FConfig^.FTokenPos;
    for i := 0 to FConfig^.FStringLen - 1 do
    begin
      Inc(HashKey, fInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[FConfig^.FTokenPos];
  KeyHash(FConfig^.FToIdent);
  FConfig^.FToken_LastID := -1;
  if HashKey <= Css_PropMaxKeyHash then
  begin
    Result := fCss_PropIdentFuncTable[HashKey];
    if (FConfig^.FToken_LastID <> -1) and
      (TSynWeb_CssPropsData[FConfig^.FToken_LastID] and
      (1 shl Longword(FConfig^.FCssVersion)) = 0) then
      Result := tkCssPropUndef;
  end
  else
    Result := tkCssPropUndef;
  Css_SetProp(FConfig^.FToken_LastID + 1);
end;

{$I SynHighlighterWeb_CssPropsFunc.inc}

function TSynWebEngine.Css_ValKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_CssVals[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLenClean then
  begin
    if FConfig^.FStringLenClean = FConfig^.FStringLen then
      for i := 1 to FConfig^.FStringLen do
      begin
        if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end
    else
      for i := 1 to FConfig^.FStringLenClean do
      begin
        if Temp^ = '\' then
          Inc(Temp);
        if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Css_ValCheck: TtkTokenKind;
var
  HashKey: longword;
  prop: integer;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := FConfig^.FRun - FConfig^.FTokenPos;
    for i := 0 to FConfig^.FStringLen - 1 do
    begin
      Inc(HashKey, fInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[FConfig^.FTokenPos];
  KeyHash(FConfig^.FToIdent);
  FConfig^.FToken_LastID := -1;
  if HashKey <= Css_ValMaxKeyHash then
  begin
    Result := fCss_ValIdentFuncTable[HashKey];
    if Result = tkCssVal then
    begin
      prop := Css_GetProp - 1;
      if (prop = -1) or (TSynWeb_CssValsData[FConfig^.FToken_LastID]
        [Longword(FConfig^.FCssVersion)][prop div 32] and (1 shl (prop mod 32)) = 0) then
        Result := tkCssValUndef;
    end;
  end
  else
    Result := tkCssValUndef;
  if Result = tkCssValUndef then
  begin
    prop := Css_GetProp - 1;
    if (prop <> -1) and (TSynWeb_CssPropsData[prop] and (1 shl 20) <> 0) then
      Result := tkCssSymbol;
  end;
end;

{$I SynHighlighterWeb_CssValsFunc.inc}

function TSynWebEngine.Css_SpecialKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_CssSpecial[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Css_SpecialCheck(AStart, ALen: integer): integer;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := ALen;
    for i := 0 to ALen - 1 do
    begin
      Inc(HashKey, fInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[AStart];
  KeyHash(FConfig^.FToIdent);
  if (HashKey > Css_SpecialMaxKeyHash) or not fCss_SpecialIdentFuncTable[HashKey] then
    FConfig^.FToken_LastID := -1;
  Result := FConfig^.FToken_LastID;
end;

{$I SynHighlighterWeb_CssSpecialFunc.inc}

// ECMAScript ------------------------------------------------------------------

procedure TSynWebEngine.ES_MakeMethodTables;
var
  c: char;
  i: integer;
  pF: PIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        fES_ProcTable[c] := NullProc;
      #1..#32:
        fES_ProcTable[c] := ES_SpaceProc;
      '/':
        fES_ProcTable[c] := ES_SlashProc;
      '<':
        fES_ProcTable[c] := ES_LowerProc;
      '=', '!':
        fES_ProcTable[c] := ES_EqualNotProc;
      '>':
        fES_ProcTable[c] := ES_GreaterProc;
      '&':
        fES_ProcTable[c] := ES_AndProc;
      '+':
        fES_ProcTable[c] := ES_PlusProc;
      '-':
        fES_ProcTable[c] := ES_MinusProc;
      '|':
        fES_ProcTable[c] := ES_OrProc;
      '*', '%', '^':
        fES_ProcTable[c] := ES_MulModXorProc;
      '0'..'9':
        fES_ProcTable[c] := ES_NumberProc;
      '"':
        fES_ProcTable[c] := ES_String34Proc;
      #39:
        fES_ProcTable[c] := ES_String39Proc;
      '{', '}', '[', ']', '(', ')', '.', ';', ',', '?', ':', '~':
        fES_ProcTable[c] :=
          ES_SymbolProc;
      '$', 'a'..'z', 'A'..'Z', '_':
        fES_ProcTable[c] := ES_IdentProc;
      else
        fES_ProcTable[c] := ES_ErrorProc;
    end;

  fES_RangeProcTable[rsESDefault] := ES_RangeDefaultProc;
  fES_RangeProcTable[rsESComment] := ES_RangeCommentProc;
  fES_RangeProcTable[rsESCommentMulti] := ES_RangeCommentMultiProc;
  fES_RangeProcTable[rsESString34] := ES_RangeString34Proc;
  fES_RangeProcTable[rsESString39] := ES_RangeString39Proc;

  pF := PIdentFuncTableFunc(@fES_IdentFuncTable);
  for I := Low(fES_IdentFuncTable) to High(fES_IdentFuncTable) do
  begin
    pF^ := ES_KeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_ESKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.ES_Next;
begin
  FConfig^.FTokenPos := FConfig^.FRun;
  fES_RangeProcTable[ES_GetRange];
end;

function TSynWebEngine.ES_GetRange: TESRangeState;
begin
  Result := TESRangeState(GetRange_Int(2, 15));
end;

procedure TSynWebEngine.ES_SetRange(const ARange: TESRangeState);
begin
  SetRange_Int(2, 15, Longword(ARange));
end;

function TSynWebEngine.ES_CheckNull(ADo: boolean = True): boolean;
begin
  case FConfig^.FLine[FConfig^.FRun] of
    #0:
    begin
      Result := True;
      if ADo then
        NullProc;
    end;
    '<':
      if (FConfig^.FLine[FConfig^.FRun + 1] = '/') and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 2]] =
        FConfig^.FHashTable['s']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 3]] =
        FConfig^.FHashTable['c']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 4]] =
        FConfig^.FHashTable['r']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 5]] =
        FConfig^.FHashTable['i']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 6]] =
        FConfig^.FHashTable['p']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 7]] =
        FConfig^.FHashTable['t']) and
        (fIdentTable2[FConfig^.FLine[FConfig^.FRun + 8]] and (1 shl 0) <> 0) and
        // (FConfig^.FLine[FConfig^.FRun+8] in [#0..#32, '>']) and
        (FConfig^.FHighlighterMode=shmHtml) then
      begin
        Result := True;
        if ADo then
        begin
          FConfig^.FTokenID := tkHtmlTag;
          SetHighlighterType(shtHtml, True, False, False);
        end;
      end
      else
        Result := False;
    else
      Result := False;
  end;
end;

procedure TSynWebEngine.ES_SpaceProc;
begin
  repeat
    Inc(FConfig^.FRun);
  until not (FConfig^.FLine[FConfig^.FRun] in [#1..#32]);
  FConfig^.FTokenID := tkESSpace;
end;

procedure TSynWebEngine.ES_SlashProc;
begin
  Inc(FConfig^.FRun);
  case FConfig^.FLine[FConfig^.FRun] of
    '*':
    begin
      Inc(FConfig^.FRun);
      ES_SetRange(rsESCommentMulti);
      if ES_CheckNull(False) or Php_CheckBegin(False) then
        FConfig^.FTokenID := tkESComment
      else
        ES_RangeCommentMultiProc;
      Exit;
    end;
    '=':
      Inc(FConfig^.FRun);
    '/':
    begin
      Inc(FConfig^.FRun);
      ES_SetRange(rsESComment);
      if ES_CheckNull(False) or Php_CheckBegin(False) then
        FConfig^.FTokenID := tkESComment
      else
        ES_RangeCommentProc;
      Exit;
    end;
  end;
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_LowerProc;
begin
  if ES_CheckNull or Php_CheckBegin then
    Exit;
  Inc(FConfig^.FRun);
  case FConfig^.FLine[FConfig^.FRun] of
    '=':
      Inc(FConfig^.FRun);
    '<':
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] = '=' then
        Inc(FConfig^.FRun);
    end;
  end;
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_EqualNotProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '=' then
  begin
    Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '=' then
      Inc(FConfig^.FRun);
  end;
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_GreaterProc;
begin
  Inc(FConfig^.FRun);
  case FConfig^.FLine[FConfig^.FRun] of
    '=':
      Inc(FConfig^.FRun);
    '>':
    begin
      Inc(FConfig^.FRun);
      case FConfig^.FLine[FConfig^.FRun] of
        '=':
          Inc(FConfig^.FRun);
        '>':
        begin
          Inc(FConfig^.FRun);
          if FConfig^.FLine[FConfig^.FRun] = '=' then
            Inc(FConfig^.FRun);
        end;
      end;
    end;
  end;
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_AndProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['=', '&'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_PlusProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['=', '+'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_MinusProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['=', '-'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_OrProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['=', '|'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_MulModXorProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '=' then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_NumberProc;
begin
  FConfig^.FTokenID := tkESError;
  if (FConfig^.FLine[FConfig^.FRun] = '0') and
    (FConfig^.FLine[FConfig^.FRun + 1] in ['x', 'X']) then
  begin
    Inc(FConfig^.FRun, 2);
    if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) <> 0 then
      // if FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
      repeat
        Inc(FConfig^.FRun);
      until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) =
        0 // until not (FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end
  else
  begin
    while FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] do
      Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '.' then
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
    if FConfig^.FLine[FConfig^.FRun] in ['e', 'E'] then
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['+', '-'] then
        Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  FConfig^.FTokenID := tkESNumber;
end;

procedure TSynWebEngine.ES_String34Proc;
begin
  Inc(FConfig^.FRun);
  if ES_CheckNull(False) then
    FConfig^.FTokenID := tkESError
  else
  begin
    ES_SetRange(rsESString34);
    if Php_CheckBegin(False) then
      FConfig^.FTokenID := tkESString
    else
      ES_RangeString34Proc;
  end;
end;

procedure TSynWebEngine.ES_String39Proc;
begin
  Inc(FConfig^.FRun);
  if ES_CheckNull(False) then
    FConfig^.FTokenID := tkESError
  else
  begin
    ES_SetRange(rsESString39);
    if Php_CheckBegin(False) then
      FConfig^.FTokenID := tkESString
    else
      ES_RangeString39Proc;
  end;
end;

procedure TSynWebEngine.ES_SymbolProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESSymbol;
end;

procedure TSynWebEngine.ES_IdentProc;
begin
  repeat
    Inc(FConfig^.FRun);
  until fIdentTable2[FConfig^.FLine[FConfig^.FRun]] and (1 shl 2) = 0;
  // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$']);
  FConfig^.FTokenID := ES_IdentCheck;
end;

procedure TSynWebEngine.ES_ErrorProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkESError;
end;

procedure TSynWebEngine.ES_RangeDefaultProc;
begin
  fES_ProcTable[FConfig^.FLine[FConfig^.FRun]];
end;

procedure TSynWebEngine.ES_RangeCommentProc;
begin
  if not ES_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        repeat
          Inc(FConfig^.FRun);
        until FConfig^.FLine[FConfig^.FRun] in [#0, '<'];
        case FConfig^.FLine[FConfig^.FRun] of
          #0:
          begin
            FConfig^.FTokenID := tkESComment;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FConfig^.FTokenID := tkESComment;
              Exit;
            end
            else
            if ES_CheckNull(False) then
            begin
              FConfig^.FTokenID := tkESComment;
              Break;
            end
            else
              Inc(FConfig^.FRun);
        end;
      until False;
  ES_SetRange(rsESDefault);
end;

procedure TSynWebEngine.ES_RangeCommentMultiProc;
begin
  if ES_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 26) = 0 do
      // while not (FConfig^.FLine[FConfig^.FRun] in [#0, '*', '<']) do
      Inc(FConfig^.FRun);
    case FConfig^.FLine[FConfig^.FRun] of
      #0:
        Break;
      '<':
        if ES_CheckNull(False) or Php_CheckBegin(False) then
          Break
        else
          Inc(FConfig^.FRun);
      '*':
      begin
        Inc(FConfig^.FRun);
        if FConfig^.FLine[FConfig^.FRun] = '/' then
        begin
          Inc(FConfig^.FRun);
          ES_SetRange(rsESDefault);
          Break;
        end;
      end;
    end;
  until False;
  FConfig^.FTokenID := tkESComment;
end;

procedure TSynWebEngine.ES_RangeString34Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while fIdentTable2[FConfig^.FLine[FConfig^.FRun]] and (1 shl 3) = 0 do
          // while not (FConfig^.FLine[FConfig^.FRun] in [#0, #34, '<', '\']) do
          Inc(FConfig^.FRun);
        case FConfig^.FLine[FConfig^.FRun] of
          #0:
          begin
            FConfig^.FTokenID := tkESError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FConfig^.FTokenID := tkESString;
              Exit;
            end
            else
              Inc(FConfig^.FRun);
          #34:
          begin
            Inc(FConfig^.FRun);
            FConfig^.FTokenID := tkESString;
            Break;
          end;
          '\':
          begin
            Inc(FConfig^.FRun);
            if FConfig^.FLine[FConfig^.FRun] = #34 then
              Inc(FConfig^.FRun);
          end;
        end;
      until False;
  ES_SetRange(rsESDefault);
end;

procedure TSynWebEngine.ES_RangeString39Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while fIdentTable2[FConfig^.FLine[FConfig^.FRun]] and (1 shl 4) = 0 do
          // while not (FConfig^.FLine[FConfig^.FRun] in [#0, #39, '<', '\']) do
          Inc(FConfig^.FRun);
        case FConfig^.FLine[FConfig^.FRun] of
          #0:
          begin
            FConfig^.FTokenID := tkESError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FConfig^.FTokenID := tkESString;
              Exit;
            end
            else
              Inc(FConfig^.FRun);
          #39:
          begin
            Inc(FConfig^.FRun);
            FConfig^.FTokenID := tkESString;
            Break;
          end;
          '\':
          begin
            Inc(FConfig^.FRun);
            if FConfig^.FLine[FConfig^.FRun] = #39 then
              Inc(FConfig^.FRun);
          end;
        end;
      until False;
  ES_SetRange(rsESDefault);
end;

function TSynWebEngine.ES_KeywordComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_ESKeywords[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.ES_IdentCheck: TtkTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := FConfig^.FRun - FConfig^.FTokenPos;
    for i := 0 to FConfig^.FStringLen - 1 do
    begin
      Inc(HashKey, fInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[FConfig^.FTokenPos];
  KeyHash(FConfig^.FToIdent);
  FConfig^.FToken_LastID := -1;
  if HashKey <= ES_KeywordsMaxKeyHash then
    Result := fES_IdentFuncTable[HashKey]
  else
    Result := tkESIdentifier;
end;

{$I SynHighlighterWeb_ESKeywordsFunc.inc}

// PHP -------------------------------------------------------------------------

procedure TSynWebEngine.Php_MakeMethodTables;
var
  c: char;
  i: integer;
  pF: PIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        fPhp_ProcTable[c] := NullProc;
      #1..#32:
        fPhp_ProcTable[c] := Php_SpaceProc;
      '?':
        fPhp_ProcTable[c] := Php_QuestionProc;
      '0'..'9':
        fPhp_ProcTable[c] := Php_NumberProc;
      '"':
        fPhp_ProcTable[c] := Php_String34Proc;
      #39:
        fPhp_ProcTable[c] := Php_String39Proc;
      '`':
        fPhp_ProcTable[c] := Php_StringShellProc;
      '&':
        fPhp_ProcTable[c] := Php_AndProc;
      '|':
        fPhp_ProcTable[c] := Php_OrProc;
      '@':
        fPhp_ProcTable[c] := Php_AtSymbolProc;
      '=':
        fPhp_ProcTable[c] := Php_EqualProc;
      '>':
        fPhp_ProcTable[c] := Php_GreaterProc;
      '<':
        fPhp_ProcTable[c] := Php_LowerProc;
      '+':
        fPhp_ProcTable[c] := Php_PlusProc;
      '-':
        fPhp_ProcTable[c] := Php_MinusProc;
      '*', '^':
        fPhp_ProcTable[c] := Php_MulDivModXorProc;
      '/':
        fPhp_ProcTable[c] := Php_SlashProc;
      '%':
        fPhp_ProcTable[c] := Php_PercentProc;
      '#':
        fPhp_ProcTable[c] := Php_HashProc;
      '!':
        fPhp_ProcTable[c] := Php_NotProc;
      '.':
        fPhp_ProcTable[c] := Php_DotProc;
      '{', '}', '[', ']', '(', ')', '~', ',', ';', ':':
        fPhp_ProcTable[c] :=
          Php_SymbolProc;
      '$':
        fPhp_ProcTable[c] := Php_VarProc;
      'a'..'z', 'A'..'Z', '_', #$7F..#$FF:
        fPhp_ProcTable[c] := Php_IdentProc;
      else
        fPhp_ProcTable[c] := Php_ErrorProc;
    end;

  fPhp_RangeProcTable[rsPhpSubProc] := Php_RangeTagProc;
  fPhp_RangeProcTable[rsPhpDefault] := Php_RangeDefaultProc;
  fPhp_RangeProcTable[rsPhpComment] := Php_RangeCommentProc;
  fPhp_RangeProcTable[rsPhpString34] := Php_RangeString34Proc;
  fPhp_RangeProcTable[rsPhpString39] := Php_RangeString39Proc;
  fPhp_RangeProcTable[rsPhpStringShell] := Php_RangeStringShellProc;
  fPhp_RangeProcTable[rsPhpHeredoc] := Php_RangeHeredocProc;

  pF := PIdentFuncTableFunc(@fPhp_IdentFuncTable);
  for I := Low(fPhp_IdentFuncTable) to High(fPhp_IdentFuncTable) do
  begin
    pF^ := Php_KeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_PhpKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.Php_Next;
begin
  FConfig^.FTokenPos := FConfig^.FRun;
  if FConfig^.FLine[FConfig^.FRun] = #0 then
    NullProc
  else
    fPhp_RangeProcTable[Php_GetRange];
end;

function TSynWebEngine.Php_GetRange: TPhpRangeState;
begin
  if GetRange_Bit(26) then
    Result := rsPhpHeredoc
  else
    Result := TPhpRangeState(GetRange_Int(3, 23));
end;

procedure TSynWebEngine.Php_SetRange(const ARange: TPhpRangeState);
begin
  if ARange = rsPhpHeredoc then
    SetRange_Bit(26, True)
  else
  begin
    SetRange_Bit(26, False);
    SetRange_Int(3, 23, Longword(ARange));
  end;
end;

function TSynWebEngine.Php_GetOpenTag: TPhpOpenTag;
begin
  Result := TPhpOpenTag(GetRange_Int(2, 27));
end;

procedure TSynWebEngine.Php_SetOpenTag(APhpOpenTag: TPhpOpenTag);
begin
  SetRange_Int(2, 27, Longword(APhpOpenTag));
end;

function TSynWebEngine.Php_CheckBegin(ABegin: boolean): boolean;
begin
  Result := False;
  if (FConfig^.FLine[FConfig^.FRun] = '<') and FConfig^.FPhpEmbeded then
    case FConfig^.FLine[FConfig^.FRun + 1] of
      '?':
        if (UpCase(FConfig^.FLine[FConfig^.FRun + 2]) = 'P') and
          (UpCase(FConfig^.FLine[FConfig^.FRun + 3]) = 'H') and
          (UpCase(FConfig^.FLine[FConfig^.FRun + 4]) = 'P') and
          (fIdentTable[FConfig^.FLine[FConfig^.FRun + 5]] and (1 shl 29) = 0) then
          // not (FConfig^.FLine[FConfig^.FRun+5] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]) then
        begin
          if ABegin then
            Php_Begin(potPhp);
        end
        else
        if FConfig^.FPhpShortOpenTag then
        begin
          if ABegin then
            Php_Begin(potPhpShort);
        end
        else
          Exit;
      '%':
        if FConfig^.FPhpAspTags then
        begin
          if ABegin then
            Php_Begin(potASP);
        end
        else
          Exit;
      else
        Exit;
    end
  else
    Exit;
  Result := True;
end;

procedure TSynWebEngine.Php_Begin(ATagKind: TPhpOpenTag);
begin
  SetHighlighterType(
    TSynHighlighterType(Longword(FConfig^.FHighlighterType) + Longword(shtPHP_inHtml)),
    False,
    True,
    ATagKind = potHtml);
  SetRange_Int(12, 17, 0);
  Php_SetOpenTag(ATagKind);
  if ATagKind = potHTML then
    Php_SetRange(rsPhpDefault)
  else
    Next;
end;

procedure TSynWebEngine.Php_End;
var
  t: TPhpOpenTag;
begin
  t := Php_GetOpenTag;
  SetRange_Int(12, 17, 0);
  if FConfig^.FLine[FConfig^.FRun] = #0 then
    SetRange_Int(3, 29, Longword(FConfig^.FHighlighterType) - Longword(shtPHP_inHtml))
  else
  begin
    SetHighlighterType(
      TSynHighlighterType(Longword(FConfig^.FHighlighterType) - Longword(shtPHP_inHtml)),
      t = potHTML,
      True,
      t <> potHTML);
    if t = potHTML then
      Next;
  end;
end;

procedure TSynWebEngine.Php_SpaceProc;
begin
  repeat
    Inc(FConfig^.FRun);
  until not (FConfig^.FLine[FConfig^.FRun] in [#1..#32]);
  FConfig^.FTokenID := tkPhpSpace;
end;

procedure TSynWebEngine.Php_QuestionProc;
begin
  Inc(FConfig^.FRun);
  if (FConfig^.FLine[FConfig^.FRun] = '>') and FConfig^.FPhpEmbeded then
  begin
    Inc(FConfig^.FRun);
    if Php_GetOpenTag in [potPhp, potPhpShort] then
    begin
      FConfig^.FTokenID := tkHtmlTag;
      Php_End;
    end
    else
      FConfig^.FTokenID := tkPhpError;
  end
  else
    FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_NumberProc;
begin
  if Php_CheckNumberProc then
    FConfig^.FTokenID := tkPhpNumber
  else
    FConfig^.FTokenID := tkPhpError;
end;

function TSynWebEngine.Php_CheckNumberProc: boolean;
begin
  Result := False;
  if (FConfig^.FLine[FConfig^.FRun] = '0') and
    (FConfig^.FLine[FConfig^.FRun + 1] = 'x') then
  begin
    Inc(FConfig^.FRun, 2);
    if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) <> 0 then
      // if FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
      repeat
        Inc(FConfig^.FRun);
      until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) =
        0 // until not (FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end
  else
  begin
    while FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] do
      Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '.' then
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
    if FConfig^.FLine[FConfig^.FRun] in ['e', 'E'] then
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['+', '-'] then
        Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] in ['0'..'9'] then
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  Result := True;
end;

procedure TSynWebEngine.Php_String34Proc;
begin
  Inc(FConfig^.FRun);
  Php_SetRange(rsPhpString34);
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 30) <> 0 then
    // if FConfig^.FLine[FConfig^.FRun] in [#0, '\', '{', '$'] then
    FConfig^.FTokenID := tkPhpString
  else
    Php_RangeString34Proc;
end;

procedure TSynWebEngine.Php_String39Proc;
begin
  Inc(FConfig^.FRun);
  Php_SetRange(rsPhpString39);
  if FConfig^.FLine[FConfig^.FRun] in [#0, '\'] then
    FConfig^.FTokenID := tkPhpString
  else
    Php_RangeString39Proc;
end;

procedure TSynWebEngine.Php_StringShellProc;
begin
  Inc(FConfig^.FRun);
  Php_SetRange(rsPhpStringShell);
  if FConfig^.FLine[FConfig^.FRun] in [#0, '`'] then
    FConfig^.FTokenID := tkPhpString
  else
    Php_RangeStringShellProc;
end;

procedure TSynWebEngine.Php_AndProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['=', '&'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_OrProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['=', '|'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_AtSymbolProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpKeyword;
end;

procedure TSynWebEngine.Php_EqualProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '=' then
  begin
    Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '=' then
      Inc(FConfig^.FRun);
  end;
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_GreaterProc;
begin
  Inc(FConfig^.FRun);
  case FConfig^.FLine[FConfig^.FRun] of
    '=':
      Inc(FConfig^.FRun);
    '>':
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] = '=' then
        Inc(FConfig^.FRun);
    end;
  end;
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_LowerProc;
var
  tmpRun: longword;
begin
  Inc(FConfig^.FRun);
  case FConfig^.FLine[FConfig^.FRun] of
    '/':
      if (Php_GetOpenTag = potHTML) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 1]] =
        FConfig^.FHashTable['s']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 2]] =
        FConfig^.FHashTable['c']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 3]] =
        FConfig^.FHashTable['r']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 4]] =
        FConfig^.FHashTable['i']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 5]] =
        FConfig^.FHashTable['p']) and
        (FConfig^.FHashTable[FConfig^.FLine[FConfig^.FRun + 6]] =
        FConfig^.FHashTable['t']) and
        (fIdentTable2[FConfig^.FLine[FConfig^.FRun + 7]] and (1 shl 0) <> 0) then
        // (FConfig^.FLine[FConfig^.FRun+7] in [#0..#32, '>']) then
      begin
        Dec(FConfig^.FRun);
        Php_End;
        Exit;
      end;
    '=':
      Inc(FConfig^.FRun);
    '<':
    begin
      Inc(FConfig^.FRun);
      case FConfig^.FLine[FConfig^.FRun] of
        '=':
          Inc(FConfig^.FRun);
        '<':
        begin
          Inc(FConfig^.FRun);
          tmpRun := FConfig^.FRun;
          while FConfig^.FLine[tmpRun] in [#1..#32] do
            Inc(tmpRun);
          if fIdentTable[FConfig^.FLine[tmpRun]] and (1 shl 28) = 0 then
            // if not (FConfig^.FLine[tmpRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF]) then
          begin
            FConfig^.FTokenID := tkPhpError;
            Exit;
          end;
          Php_SetRange(rsPhpSubProc);
          SetRange_Int(3, 20, 2);
        end;
      end;
    end;
  end;
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_PlusProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['+', '='] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_MinusProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] in ['-', '=', '>'] then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_MulDivModXorProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '=' then
    Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_SlashProc;
begin
  case FConfig^.FLine[FConfig^.FRun + 1] of
    '/':
    begin
      Inc(FConfig^.FRun);
      Php_HashProc;
    end;
    '*':
    begin
      Inc(FConfig^.FRun, 2);
      Php_SetRange(rsPhpComment);
      if FConfig^.FLine[FConfig^.FRun] = #0 then
        FConfig^.FTokenID := tkPhpComment
      else
        Php_RangeCommentProc;
    end;
    else
      Php_MulDivModXorProc;
  end;
end;

procedure TSynWebEngine.Php_PercentProc;
begin
  if (FConfig^.FLine[FConfig^.FRun + 1] = '>') and
    FConfig^.FPhpEmbeded then
  begin
    Inc(FConfig^.FRun, 2);
    if Php_GetOpenTag = potASP then
    begin
      FConfig^.FTokenID := tkHtmlTag;
      Php_End;
    end
    else
      FConfig^.FTokenID := tkPhpError;
  end
  else
    Php_MulDivModXorProc;
end;

procedure TSynWebEngine.Php_HashProc;
begin
  FConfig^.FTokenID := tkPhpComment;
  repeat
    repeat
      Inc(FConfig^.FRun)
    until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 17) <> 0;
    // until FConfig^.FLine[FConfig^.FRun] in [#0, #10, #13, '%', '?'];
    case FConfig^.FLine[FConfig^.FRun] of
      #0:
        Exit;
      '?':
        if (FConfig^.FLine[FConfig^.FRun + 1] = '>') and
          (Php_GetOpenTag in [potPhp, potPhpShort]) and
          FConfig^.FPhpEmbeded then
          Exit;
      '%':
        if (FConfig^.FLine[FConfig^.FRun + 1] = '>') and
          (Php_GetOpenTag = potASP) and FConfig^.FPhpEmbeded then
          Exit;
      else
        Exit;
    end;
  until False;
end;

procedure TSynWebEngine.Php_NotProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '=' then
  begin
    Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = '=' then
      Inc(FConfig^.FRun);
  end;
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_DotProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '=' then
  begin
    Inc(FConfig^.FRun);
    FConfig^.FTokenID := tkPhpSymbol;
  end
  else
    Php_NumberProc;
end;

procedure TSynWebEngine.Php_SymbolProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpSymbol;
end;

procedure TSynWebEngine.Php_VarProc;
begin
  Inc(FConfig^.FRun);
  if FConfig^.FLine[FConfig^.FRun] = '$' then
    Inc(FConfig^.FRun);
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 28) <> 0 then
    // if FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    FConfig^.FTokenID := tkPhpKeyword
  else
    FConfig^.FTokenID := tkPhpError;
end;

procedure TSynWebEngine.Php_IdentProc;
begin
  repeat
    Inc(FConfig^.FRun);
  until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 29) = 0;
  // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  if (FConfig^.FTokenPos > 0) and (FConfig^.FLine[FConfig^.FTokenPos - 1] = '$') then
    FConfig^.FTokenID := tkPhpVariable
  else
    FConfig^.FTokenID := Php_IdentCheck;
end;

procedure TSynWebEngine.Php_ErrorProc;
begin
  Inc(FConfig^.FRun);
  FConfig^.FTokenID := tkPhpError;
end;

function TSynWebEngine.Php_DoStringDouble(AIsHeredoc: boolean): boolean;

  procedure TryDoSpace;
  begin
    while FConfig^.FLine[FConfig^.FRun] in [#1..#32] do
      Inc(FConfig^.FRun);
  end;

  procedure DoIdent;
  begin
    repeat
      Inc(FConfig^.FRun);
    until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 29) = 0;
    // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  end;

  function TryDoIdent: boolean;
  begin
    if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 28) <> 0 then
      // if FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    begin
      DoIdent;
      Result := True;
    end
    else
      Result := False;
  end;

  function DoStringSingle: boolean;
  begin
    Result := True;
    repeat
      if FConfig^.FLine[FConfig^.FRun] = '\' then
      begin
        Inc(FConfig^.FRun);
        if FConfig^.FLine[FConfig^.FRun] in [#39, '\'] then
          Inc(FConfig^.FRun);
      end;
      while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 24) = 0 do
        // while not(FConfig^.FLine[FConfig^.FRun] in [#0, #39, '\'] do
        Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] = '\' then
        Continue
      else
      begin
        if FConfig^.FLine[FConfig^.FRun] <> #39 then
          Result := False;
        Exit;
      end;
    until False;
  end;

  function DoStringObject(AAllowSpaces: boolean = True): boolean;
  begin
    Inc(FConfig^.FRun, 2);
    if AAllowSpaces then
    begin
      TryDoSpace;
      Result := TryDoIdent;
      TryDoSpace;
    end
    else
      Result := TryDoIdent;
  end;

  function DoStringVar: boolean;
  begin
    Inc(FConfig^.FRun);
    Result := TryDoIdent;
  end;

  function DoStringVar2: boolean;
  begin
    TryDoSpace;
    Result := True;
    case FConfig^.FLine[FConfig^.FRun] of
      '-':
        if (FConfig^.FLine[FConfig^.FRun + 1] = '>') and
          ((not DoStringObject) or ((FConfig^.FLine[FConfig^.FRun] in ['[', '-']) and
          not DoStringVar2)) then
          Result := False;
      '[':
      begin
        Inc(FConfig^.FRun);
        TryDoSpace;
        case FConfig^.FLine[FConfig^.FRun] of
          '$':
            if (not DoStringVar) or ((FConfig^.FLine[FConfig^.FRun] in ['[', '-']) and
              not DoStringVar2) then
              Result := False;
          '0'..'9', '.':
            if not Php_CheckNumberProc then
              Result := False;
          #39:
          begin
            Inc(FConfig^.FRun);
            if DoStringSingle then
              Inc(FConfig^.FRun)
            else
              Result := False;
          end;
          '"':
          begin
            Inc(FConfig^.FRun);
            while not Php_DoStringDouble and (FConfig^.FLine[FConfig^.FRun] <> #0) and
              (FConfig^.FTokenID <> tkPhpError) do
            ;
            if (FConfig^.FLine[FConfig^.FRun] = '"') and
              (FConfig^.FTokenID <> tkPhpError) then
            begin
              FConfig^.FTokenID := tkPhpStringSpecial;
              Inc(FConfig^.FRun);
            end
            else
              Result := False;
          end;
          else
            if not TryDoIdent then
              Result := False;
        end;
        TryDoSpace;
        if not Result or (FConfig^.FLine[FConfig^.FRun] <> ']') then
          Result := False
        else
        begin
          Inc(FConfig^.FRun);
          TryDoSpace;
          if (FConfig^.FLine[FConfig^.FRun] in ['[', '-']) and not DoStringVar2 then
            Result := False;
        end;
      end;
    end;
  end;

begin
  Result := False;
  FConfig^.FTokenID := tkPhpStringSpecial;
  case FConfig^.FLine[FConfig^.FRun] of
    '$':
    begin
      Inc(FConfig^.FRun);
      if TryDoIdent then
      begin
        case FConfig^.FLine[FConfig^.FRun] of
          '-':
            if FConfig^.FLine[FConfig^.FRun + 1] = '>' then
              if not DoStringObject(False) then
                FConfig^.FTokenID := tkPhpError;
          '[':
          begin
            Inc(FConfig^.FRun);
            case FConfig^.FLine[FConfig^.FRun] of
              '$':
                if not DoStringVar then
                  FConfig^.FTokenID := tkPhpError;
              '0'..'9', '.':
                if not Php_CheckNumberProc then
                  FConfig^.FTokenID := tkPhpError;
              else
                if not TryDoIdent then
                  FConfig^.FTokenID := tkPhpError;
            end;
            if FConfig^.FLine[FConfig^.FRun] = ']' then
              Inc(FConfig^.FRun)
            else
              FConfig^.FTokenID := tkPhpError;
          end;
        end;
        Exit;
      end
      else
      if FConfig^.FLine[FConfig^.FRun] = '{' then
      begin
        Inc(FConfig^.FRun);
        if not TryDoIdent or not DoStringVar2 or
          (FConfig^.FLine[FConfig^.FRun] <> '}') then
          FConfig^.FTokenID := tkPhpError
        else
          Inc(FConfig^.FRun);
        Exit;
      end;
    end;
    '{':
    begin
      Inc(FConfig^.FRun);
      if FConfig^.FLine[FConfig^.FRun] = '$' then
      begin
        Inc(FConfig^.FRun);
        if not TryDoIdent or not DoStringVar2 or
          (FConfig^.FLine[FConfig^.FRun] <> '}') then
          FConfig^.FTokenID := tkPhpError
        else
          Inc(FConfig^.FRun);
        Exit;
      end;
    end;
    '\':
    begin
      Inc(FConfig^.FRun);
      if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 18) <> 0 then
        // if FConfig^.FLine[FConfig^.FRun] in ['n', 'r', 't', '\', '$', #34, '0'..'7', 'x'] then
      begin
        Inc(FConfig^.FRun);
        case FConfig^.FLine[FConfig^.FRun - 1] of
          '0'..'7':
          begin
            if FConfig^.FLine[FConfig^.FRun] in ['0'..'7'] then
            begin
              Inc(FConfig^.FRun);
              if FConfig^.FLine[FConfig^.FRun] in ['0'..'7'] then
                Inc(FConfig^.FRun);
            end;
            Exit;
          end;
          'x':
            if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) <> 0 then
              // if FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
            begin
              Inc(FConfig^.FRun);
              if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 10) <> 0 then
                // if FConfig^.FLine[FConfig^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
                Inc(FConfig^.FRun);
              Exit;
            end;
          else
            Exit;
        end;
      end;
    end;
  end;
  FConfig^.FTokenID := tkPhpString;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 25) = 0 do
      // while not(FConfig^.FLine[FConfig^.FRun] in [#0, #34, '\', '{', '$'] do
      Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] = #34 then
      if AIsHeredoc then
      begin
        Inc(FConfig^.FRun);
        Continue;
      end
      else
        Result := True;
    Exit;
  until False;
end;

procedure TSynWebEngine.Php_RangeTagProc;

  procedure DoDefault;
  begin
    SetRange_Int(3, 20, 0);
    Php_SetRange(rsPhpDefault);
  end;

begin
  case GetRange_Int(3, 20) of
    0:
    begin
      Inc(FConfig^.FRun, 2);
      FConfig^.FTokenID := tkHtmlTag;
      SetRange_Int(3, 20, 1);
    end;
    1:
    begin
      DoDefault;
      if Php_GetOpenTag = potPhp then
      begin
        Inc(FConfig^.FRun, 3);
        FConfig^.FTokenID := tkPhpKeyword;
      end
      else // potPhpShort, potASP
      if FConfig^.FLine[FConfig^.FRun] = '=' then
      begin
        Inc(FConfig^.FRun);
        FConfig^.FTokenID := tkPhpKeyword;
      end
      else
        Php_RangeDefaultProc;
    end;
    2:
    begin
      if FConfig^.FLine[FConfig^.FRun] in [#1..#32] then
      begin
        repeat
          Inc(FConfig^.FRun);
        until not (FConfig^.FLine[FConfig^.FRun] in [#1..#32]);
        FConfig^.FTokenID := tkPhpSpace;
        Exit;
      end;
      repeat
        Inc(FConfig^.FRun);
      until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 29) = 0;
      // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
      if FConfig^.FLine[FConfig^.FRun] <> #0 then
      begin
        FConfig^.FTokenID := tkPhpError;
        Php_SetRange(rsPhpDefault);
        Exit;
      end;
      FConfig^.FTokenID := tkPhpKeyword;
      Php_SetRange(rsPhpHeredoc);
      SetRange_Int(8, 17, GetCRC8_String(GetToken));
      SetRange_Bit(25, GetTokenLen mod 2 = 0);
    end;
  end;
end;

procedure TSynWebEngine.Php_RangeDefaultProc;
begin
  fPhp_ProcTable[FConfig^.FLine[FConfig^.FRun]];
end;

procedure TSynWebEngine.Php_RangeCommentProc;
begin
  repeat
    if (FConfig^.FLine[FConfig^.FRun] = '*') and
      (FConfig^.FLine[FConfig^.FRun + 1] = '/') then
    begin
      Inc(FConfig^.FRun, 2);
      Php_SetRange(rsPhpDefault);
      Break;
    end;
    Inc(FConfig^.FRun);
  until FConfig^.FLine[FConfig^.FRun] = #0;
  FConfig^.FTokenID := tkPhpComment;
end;

procedure TSynWebEngine.Php_RangeString34Proc;
begin
  if Php_DoStringDouble then
  begin
    Inc(FConfig^.FRun);
    Php_SetRange(rsPhpDefault);
  end;
end;

procedure TSynWebEngine.Php_RangeString39Proc;
begin
  if ES_CheckNull then
    Exit;

  if FConfig^.FLine[FConfig^.FRun] = '\' then
  begin
    Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] in [#39, '\'] then
    begin
      Inc(FConfig^.FRun);
      FConfig^.FTokenID := tkPhpStringSpecial;
      Exit;
    end;
  end;
  FConfig^.FTokenID := tkPhpString;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 24) = 0 do
      // while not(FConfig^.FLine[FConfig^.FRun] in [#0, #39, '\'] do
      Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] <> #39 then
      Exit
    else
    begin
      Inc(FConfig^.FRun);
      Php_SetRange(rsPhpDefault);
      Exit;
    end;
  until False;
  ES_SetRange(rsESDefault);
end;

procedure TSynWebEngine.Php_RangeStringShellProc;
begin
  if FConfig^.FLine[FConfig^.FRun] = '\' then
  begin
    Inc(FConfig^.FRun);
    if FConfig^.FLine[FConfig^.FRun] in ['`', '\'] then
    begin
      Inc(FConfig^.FRun);
      FConfig^.FTokenID := tkPhpStringSpecial;
      Exit;
    end;
  end;
  FConfig^.FTokenID := tkPhpString;
  repeat
    while fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 27) = 0 do
      // while not(FConfig^.FLine[FConfig^.FRun] in [#0, '`', '\'] do
      Inc(FConfig^.FRun);
    case FConfig^.FLine[FConfig^.FRun] of
      #0:
        Exit;
      '`':
      begin
        Inc(FConfig^.FRun);
        Php_SetRange(rsPhpDefault);
        Exit;
      end;
      '\':
        if FConfig^.FLine[FConfig^.FRun + 1] in ['`', '\'] then
          Exit
        else
          Inc(FConfig^.FRun);
    end;
  until False;
end;

procedure TSynWebEngine.Php_RangeHeredocProc;
var
  OldRun: longint;
begin
  if fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 28) <> 0 then
    // if FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  begin
    OldRun := FConfig^.FRun;
    repeat
      Inc(FConfig^.FRun);
    until fIdentTable[FConfig^.FLine[FConfig^.FRun]] and (1 shl 29) = 0;
    // until not(FConfig^.FLine[FConfig^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
    if ((FConfig^.FLine[FConfig^.FRun] = ';') and
      (FConfig^.FLine[FConfig^.FRun + 1] = #0)) or
      (FConfig^.FLine[FConfig^.FRun] = #0) then
      if (GetRange_Bit(25) = (GetTokenLen mod 2 = 0)) and
        (GetRange_Int(8, 17) = GetCRC8_String(GetToken)) then
      begin
        FConfig^.FTokenID := tkPhpKeyword;
        Php_SetRange(rsPhpDefault);
        Exit;
      end;
    FConfig^.FRun := OldRun;
  end;
  if Php_DoStringDouble(True) then
  begin
    Inc(FConfig^.FRun);
    Php_SetRange(rsPhpDefault);
  end;
end;

function TSynWebEngine.Php_KeywordComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
  Data: longword;
begin
  Data := TSynWeb_PhpKeywordsData[ID];
  if (Data and $0F <> $01) or ((Data shr 16) and
    (1 shl Longword(FConfig^.FPhpVersion)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_PhpKeywords[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Php_ConstComp: boolean;
var
  I: integer;
  Temp: PChar;
begin
  Temp := FConfig^.FToIdent;
  for i := 1 to FConfig^.FStringLen do
  begin
    if UpCase(Temp^) <> Temp^ then
    begin
      Result := False;
      Exit;
    end;
    Inc(Temp);
  end;
  Result := True;
end;

function TSynWebEngine.Php_FunctionComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
  Data: longword;
begin
  Data := TSynWeb_PhpKeywordsData[ID];
  if (Data and $0F <> $08) or ((Data shr 16) and
    (1 shl Longword(FConfig^.FPhpVersion)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_PhpKeywords[ID];
  Temp := FConfig^.FToIdent;
  if Length(aKey) = FConfig^.FStringLen then
  begin
    for i := 1 to FConfig^.FStringLen do
    begin
      if fInsensitiveHashTable[Temp^] <> fInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FConfig^.FToken_LastID := ID;
    Result := True;
  end
  else
    Result := False;
end;

function TSynWebEngine.Php_IdentCheck: TtkTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FConfig^.FStringLen := FConfig^.FRun - FConfig^.FTokenPos;
    for i := 0 to FConfig^.FStringLen - 1 do
    begin
      Inc(HashKey, fInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FConfig^.FToIdent := @FConfig^.FLine[FConfig^.FTokenPos];
  KeyHash(FConfig^.FToIdent);
  FConfig^.FToken_LastID := -1;
  if HashKey <= Php_KeywordsMaxKeyHash then
    Result := fPhp_IdentFuncTable[HashKey]
  else
    Result := tkPhpIdentifier;
  if Result = tkPhpIdentifier then
    if Php_ConstComp then
      Result := tkPhpConst;
end;

{$I SynHighlighterWeb_PhpKeywordsFunc.inc}

{ TSynWebBase }

function TSynWebBase.GetActiveHighlighters: TSynHighlighterTypes;
begin
  Result := FActiveHighlighters;
end;

function TSynWebBase.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
 {?? if not (FConfig^.FHighlighterType in FConfig^.FActiveHighlighters) then
    Result:=fInactiveAttri
  else    }
  case Index of
    // SYN_ATTR_IDENTIFIER: ??
    // SYN_ATTR_KEYWORD: ??
    // SYN_ATTR_SYMBOL: ??
    SYN_ATTR_WHITESPACE:
      //todo: hmmm... ?     if Enabled then
      Result := FConfig.fSYN_ATTR_WHITESPACE
    {  else
        Result := FConfig.fHtml_WhitespaceAttri};
    SYN_ATTR_COMMENT:
      Result := FConfig.fSYN_ATTR_COMMENT;
    SYN_ATTR_STRING:
      Result := FConfig.fSYN_ATTR_STRING;
    else
      Result := nil;
  end;
end;

function TSynWebBase.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynWebBase.GetRange: Pointer;
begin
  Result := Pointer(FConfig.FRange);
end;

function TSynWebBase.GetToken: string;
var
  Len: longint;
begin
  Len := FConfig.FRun - FConfig.FTokenPos;
  SetString(Result, (FConfig.FLine + FConfig.FTokenPos), Len);
end;

function TSynWebBase.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if (FConfig.FHighlighterType in FActiveHighlighters) {or
    (FConfig^.FTokenID in [tkHtmlSpace, tkCssSpace, tkESSpace, tkPhpSpace])} then
    Result := FEngine.fTokenAttributeTable[FConfig.FTokenID]
  else
    Result := FEngine.fInactiveAttri;
end;

function TSynWebBase.GetTokenID: TtkTokenKind;
begin
  Result := FConfig.FTokenID;
end;

function TSynWebBase.GetTokenKind: integer;
begin
  Result := Ord(FConfig.FTokenID);
end;

function TSynWebBase.GetTokenLen: integer;
begin
  Result := FConfig.FRun - FConfig.FTokenPos;
end;

function TSynWebBase.GetTokenPos: integer;
begin
  Result := FConfig.FTokenPos;
end;

procedure TSynWebBase.Next;
begin
  FEngine.FConfig := @FConfig;
  FEngine.Next;
end;

procedure TSynWebBase.SetActiveHighlighter(const Value: boolean);
begin
  FActiveHighlighter := Value;
  if Value then
    SetupActiveHighlighter
  else
    FActiveHighlighters := [shtHtml, shtCss, shtES, shtPHP_inHtml,
      shtPHP_inCss, shtPHP_inES];
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetCssVersion(const Value: TCssVersion);
begin
  FConfig.FCssVersion := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetHtmlVersion(const Value: THtmlVersion);
begin
  FConfig.FHtmlVersion := Value;
  if FConfig.FHtmlVersion >= hvXHtml10Strict then
    FConfig.FHashTable := fSensitiveHashTable
  else
    FConfig.FHashTable := fInsensitiveHashTable;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetLine(NewValue: string; LineNumber: integer);
begin
  FEngine.FConfig := @FConfig;
  FEngine.SetLine(NewValue, LineNumber);
end;

procedure TSynWebBase.SetPhpVersion(const Value: TPhpVersion);
begin
  FConfig.FPhpVersion := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetPhpAspTags(const Value: boolean);
begin
  FConfig.FPhpAspTags := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetPhpShortOpenTag(const Value: boolean);
begin
  FConfig.FPhpShortOpenTag := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetRange(Value: Pointer);
begin
  FConfig.FRange := Longword(Value);
end;

function TSynWebBase.UpdateActiveHighlighter(ARange: Pointer;
  ALine: string; ACaretX, ACaretY: integer): boolean;
var
  f: TSynHighlighterTypes;
  lPos, lLen: integer;
  lHinghlighter, ActiveHL: TSynHighlighterType;
begin
  Result := True;
  if not FActiveHighlighter or not (FConfig.FPhpEmbeded or FConfig.FCssEmbeded or FConfig.FEsEmbeded) then
    Exit;
  FEngine.fConfig := @FConfig;
  f := FActiveHighlighters;
  Dec(ACaretX);
  SetRange(ARange);
  lHinghlighter := TSynHighlighterType(FEngine.GetRange_Int(3, 29));
  SetLine(ALine, ACaretY);
  lPos := GetTokenPos;
  lLen := GetTokenLen;
  while (GetTokenPos < ACaretX) and not GetEol do
  begin
    lHinghlighter := FConfig.FHighlighterType;
    lPos := GetTokenPos;
    lLen := GetTokenLen;
    Next;
  end;
  if FConfig.FUseNextAH and (ACaretX >= lPos + lLen) then
    ActiveHL := FConfig.FHighlighterType
  else
  if FConfig.FHighlighterSW and (ACaretX >= lPos + lLen) then
    ActiveHL := FConfig.FPrevHighlighterType
  else
    ActiveHL := lHinghlighter;
  if ActiveHL >= shtPHP_inHtml then
    FActiveHighlighters := [shtPHP_inHtml, shtPHP_inCss, shtPHP_inES]
  else
    FActiveHighlighters := [ActiveHL];
  Result := f <> FActiveHighlighters;
end;

function TSynWebBase.GetAttribCount: integer;
begin
  Result := FEngine.fAttributes.Count;
end;

function TSynWebBase.GetAttribute(idx: integer): TSynHighlighterAttributes;
begin
  Result := nil;
  if (idx >= 0) and (idx < FEngine.fAttributes.Count) then
    Result := TSynHighlighterAttributes(FEngine.fAttributes.Objects[idx]);
end;

function TSynWebBase.GetCssVersion: TCssVersion;
begin
  Result := FConfig.FCssVersion;
end;

function TSynWebBase.GetHtmlVersion: THtmlVersion;
begin
  Result := FConfig.FHtmlVersion;
end;

function TSynWebBase.GetPhpAspTags: boolean;
begin
  Result := FConfig.FPhpAspTags;
end;

function TSynWebBase.GetPhpShortOpenTag: boolean;
begin
  Result := FConfig.FPhpShortOpenTag;
end;

function TSynWebBase.GetPhpVersion: TPhpVersion;
begin
  Result := FConfig.FPhpVersion;
end;

function TSynWebBase.GetEol: boolean;
begin
  Result := FConfig.FTokenID = tkNull;
end;

{ TSynWebSynES }

constructor TSynWebESSyn.Create(AOwner: TComponent);
begin
  FConfig.FHighlighterMode := shmES;
  inherited Create(AOwner);
  PhpEmbeded := False;
  CssEmbeded := False;
  EsEmbeded := False;
end;

function TSynWebESSyn.GetSampleSource: string;
begin

end;

procedure TSynWebESSyn.ResetRange;
begin
  with FConfig do
  begin
    FRange := $00000000;
    FRange := FRange or (Longword(shtES) shl 29);
  end;
end;

{ TSynWebSynCSS }

constructor TSynWebCSSSyn.Create(AOwner: TComponent);
begin
  FConfig.FHighlighterMode := shmCss;
  inherited Create(AOwner);
  PhpEmbeded := False;
  CssEmbeded := False;
  EsEmbeded := False;
end;

function TSynWebCSSSyn.GetSampleSource: string;
begin

end;

procedure TSynWebCSSSyn.ResetRange;
begin
  with FConfig do
  begin
    FRange := $00000000;
    FRange := FRange or (Longword(shtCss) shl 29);
  end;
end;

{ TSynWebSynHtml }

constructor TSynWebHtmlSyn.Create(AOwner: TComponent);
begin
  FConfig.FHighlighterMode := shmHtml;
  inherited Create(AOwner);
  PhpEmbeded := True;
  CssEmbeded := True;
  EsEmbeded := True;
end;

function TSynWebHtmlSyn.GetSampleSource: string;
begin

end;

procedure TSynWebHtmlSyn.ResetRange;
begin
  FConfig.FRange := $00000000;
end;

{ TSynWebSynPHP }

constructor TSynWebPHPCliSyn.Create(AOwner: TComponent);
begin
  FConfig.FHighlighterMode := shmPhpCli;
  inherited Create(AOwner);
  PhpEmbeded := False;
  CssEmbeded := False;
  EsEmbeded := False;
end;

function TSynWebPHPCliSyn.GetSampleSource: string;
begin

end;

procedure TSynWebPHPCliSyn.ResetRange;
begin
  with FConfig do
  begin
    FRange := $00000000;
    FRange := FRange or (Longword(shtPHP_inHtml) shl 29);
    FRange := FRange or (Longword(rsPhpDefault) shl 23);
  end;
end;

constructor TSynWebBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEngine := nil;
  FDefaultFilter := '';
  FActiveHighlighter := False;
  FActiveHighlighters := [shtHtml, shtCss, shtES, shtPHP_inHtml,
    shtPHP_inCss, shtPHP_inES];

  HtmlVersion := hvXHtml10Transitional;
  CssVersion := cvCss21;
  PhpVersion := pvPhp5;
  PhpShortOpenTag := True;
  PhpAspTags := False;

  SetAttributesOnChange(DefHighlightChange);
  ResetRange;
end;

destructor TSynWebBase.Destroy;
begin
  inherited Destroy;
end;

procedure TSynWebHtmlSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtHtml];
end;

procedure TSynWebESSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtES];
end;

procedure TSynWebCSSSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtCSS];
end;

procedure TSynWebPHPCliSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtPHP_inHtml, shtPHP_inCss, shtPHP_inES];
end;

function TSynWebBase.GetCssEmbeded: boolean;
begin
  Result := FConfig.FCssEmbeded;
end;

function TSynWebBase.GetEsEmbeded: boolean;
begin
  Result := FConfig.FEsEmbeded;
end;

function TSynWebBase.GetPhpEmbeded: boolean;
begin
  Result := FConfig.FPhpEmbeded;
end;

procedure TSynWebBase.SetCssEmbeded(const Value: boolean);
begin
  FConfig.FCssEmbeded := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetEsEmbeded(const Value: boolean);
begin
  FConfig.FEsEmbeded := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetPhpEmbeded(const Value: boolean);
begin
  FConfig.FPhpEmbeded := Value;
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetEngine(const Value: TSynWebEngine);
begin
  if FEngine<>nil then
    FEngine.RemoveFromNotifyList(Self);
  FEngine := Value;
  if FEngine<>nil then
    FEngine.AddToNotifyList(Self);  
end;

procedure TSynWebBase.DoDefHighlightChange;
begin
  DefHighlightChange(Self);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynWebHtmlSyn);
  RegisterPlaceableHighlighter(TSynWebPHPCliSyn);
  RegisterPlaceableHighlighter(TSynWebCssSyn);
  RegisterPlaceableHighlighter(TSynWebEsSyn);
{$ENDIF}
end.

