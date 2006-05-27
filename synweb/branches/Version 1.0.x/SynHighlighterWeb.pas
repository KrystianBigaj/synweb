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
- Php: Doesn't support multi-line encapsuled strings in string, only single line:
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
  TSynWebBase = class;

  PSynWebOptions = ^TSynWebOptions;

  TSynWebOptions = record
    FHtmlVersion: TSynWebHtmlVersion;
    FCssVersion: TSynWebCssVersion;
    FPhpVersion: TSynWebPhpVersion;
    FPhpShortOpenTag: boolean;
    FPhpAspTags: boolean;

    FPhpEmbeded: boolean;
    FCssEmbeded: boolean;
    FEsEmbeded: Boolean;
  end;

  PSynWebInstance = ^TSynWebInstance;

  TSynWebInstance = record
    FRun: longint;
    FRange: longword;
    FLine: PChar;
    FLineRef: string;
    FLineNumber: integer;
    FToken_LastID: integer;
    FTokenPos: integer;
    FTokenID: TSynWebTokenKind;
    FStringLen, FStringLenClean: integer;
    FToIdent: PChar;
    FHashTable: TSynWebHashTable;
    FNextClearBits: boolean;
    FNextUseNextAH: boolean;
    FUseNextAH: boolean;
    FHighlighterType, FPrevHighlighterType, FNextHighlighterType: TSynHighlighterType;
    FHighlighterSW: boolean;
    FHighlighterMode: TSynHighlighterMode;
    FCssMask: longword;
    FNextProcTable: TSynWebProcTableProc;
    FSYN_ATTR_COMMENT: TSynHighlighterAttributes;
    FSYN_ATTR_STRING: TSynHighlighterAttributes;
    FSYN_ATTR_WHITESPACE: TSynHighlighterAttributes;
    FOptions: TSynWebOptions;
  end;

  TSynWebOptionsBase = class(TPersistent)
  private
    FOptions: PSynWebOptions;
    FEngineOptions: PSynWebOptions;
    FUseEngineOptions: Boolean;
    FOnChange: TNotifyEvent;
    
    function GetHtmlVersion: TSynWebHtmlVersion;
    procedure SetHtmlVersion(const Value: TSynWebHtmlVersion);
    function GetCssVersion: TSynWebCssVersion;
    procedure SetCssVersion(const Value: TSynWebCssVersion);
    function GetPhpVersion: TSynWebPhpVersion;
    procedure SetPhpVersion(const Value: TSynWebPhpVersion);
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
    
    procedure SetUseEngineOptions(const Value: Boolean);
    procedure SetEngineOptions(AEngine: PSynWebOptions);
    procedure DoOnChange;
    procedure UpdateOptions;
  protected
    property HtmlVersion: TSynWebHtmlVersion read GetHtmlVersion write SetHtmlVersion;
    property CssVersion: TSynWebCssVersion read GetCssVersion write SetCssVersion;
    property PhpVersion: TSynWebPhpVersion read GetPhpVersion write SetPhpVersion;
    property PhpShortOpenTag: boolean read GetPhpShortOpenTag write SetPhpShortOpenTag;
    property PhpAspTags: boolean read GetPhpAspTags write SetPhpAspTags;

    property CssEmbeded: boolean read GetCssEmbeded write SetCssEmbeded;
    property PhpEmbeded: boolean read GetPhpEmbeded write SetPhpEmbeded;
    property EsEmbeded: Boolean read GetEsEmbeded write SetEsEmbeded;

    property UseEngineOptions: Boolean read FUseEngineOptions write SetUseEngineOptions;
  public
    constructor Create(AOptions: PSynWebOptions);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSynWebHtmlOptions = class(TSynWebOptionsBase)
  published
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property CssEmbeded;
    property PhpEmbeded;
    property EsEmbeded;
    property UseEngineOptions;
  end;

  TSynWebCssOptions = class(TSynWebOptionsBase)
  published
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

  TSynWebEsOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

  TSynWebPhpCliOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property UseEngineOptions;
  end;

  TSynWebEngineOptions = class(TSynWebOptionsBase)
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
  end;

  TSynWebBase = class(TSynCustomHighlighter)
  private
    FInstance: TSynWebInstance;
    FEngine: TSynWebEngine;
    FActiveHighlighter: boolean;
    FActiveHighlighters: TSynHighlighterTypes;
                                                         
    procedure SetupActiveHighlighter; virtual; abstract;
    procedure SetActiveHighlighter(const Value: boolean);
    function GetActiveHighlighters: TSynHighlighterTypes;
    procedure SetEngine(const Value: TSynWebEngine);
  protected              
    FOptions: TSynWebOptionsBase;
    procedure DoDefHighlightChange;
    function GetAttribCount: integer; override;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; override;
    function GetIdentChars: TSynIdentChars; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenLen: integer;
    function GetTokenPos: integer; override;
    function GetTokenID: TSynWebTokenKind;
    function GetTokenKind: integer; override;
    function GetRange: Pointer; override;
    function GetEol: boolean; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(NewValue: string; LineNumber: integer); override;
    procedure Next; override;

    function UpdateActiveHighlighter(ARange: Pointer; ALine: string;
      ACaretX, ACaretY: integer): boolean;
    property ActiveHighlighters: TSynHighlighterTypes read GetActiveHighlighters;
  published
    property ActiveSwitchHighlighter: boolean
      read FActiveHighlighter write SetActiveHighlighter;
    property Engine: TSynWebEngine read FEngine write SetEngine;
  end;

  TSynWebHtmlSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebHtmlOptions;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property Options: TSynWebHtmlOptions read GetOptions;
  end;

  TSynWebCssSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebCssOptions;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property Options: TSynWebCssOptions read GetOptions;
  end;

  TSynWebEsSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebEsOptions;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property Options: TSynWebEsOptions read GetOptions;
  end;

  TSynWebPhpCliSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebPhpCliOptions;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
    function GetSampleSource: string; override;
  published
    property Options: TSynWebPhpCliOptions read GetOptions;
  end;

  TSynWebEngine = class(TComponent)
  private
    // Global ------------------------------------------------------------------
    FNotifyList: TList;
    FInstance: PSynWebInstance;
    FAttributes: TStringList;
    FInactiveAttri: TSynHighlighterAttributes;
    FTokenAttributeTable: TSynWebTokenAttributeTable;
    FPhpHereDocList: TStringList;
    FEngineOptions: TSynWebOptions;
    FOptions: TSynWebEngineOptions;

    // Html --------------------------------------------------------------------
    fHtml_TagIdentFuncTable: array[0..Html_TagMaxKeyHash] of TSynWebIdentFuncTableFunc;
    fHtml_AttrIdentFuncTable: array[0..Html_AttrMaxKeyHash] of TSynWebIdentFuncTableFunc;
    fHtml_SpecialIdentFuncTable: array[0..Html_SpecialMaxKeyHash] of
    TSynWebIdent2FuncTableFunc;
    fHtml_RangeProcTable: array[Low(TSynWebHtmlRangeState)..High(
      TSynWebHtmlRangeState)] of
    TSynWebProcTableProc;

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

    // Css ---------------------------------------------------------------------
    fCss_ProcTable: array[#0..#255] of TSynWebProcTableProc;
    fCss_PropIdentFuncTable: array[0..Css_PropMaxKeyHash] of TSynWebIdentFuncTableFunc;
    fCss_ValIdentFuncTable: array[0..Css_ValMaxKeyHash] of TSynWebIdentFuncTableFunc;
    fCss_SpecialIdentFuncTable: array[0..Css_SpecialMaxKeyHash] of
    TSynWebIdent2FuncTableFunc;
    fCss_RangeProcTable: array[Low(TSynWebCssRangeState)..High(TSynWebCssRangeState)] of
    TSynWebProcTableProc;

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
    fEs_ProcTable: array[#0..#255] of TSynWebProcTableProc;
    fEs_IdentFuncTable: array[0..Es_KeywordsMaxKeyHash] of TSynWebIdentFuncTableFunc;
    fEs_RangeProcTable: array[Low(TSynWebEsRangeState)..High(TSynWebEsRangeState)] of
    TSynWebProcTableProc;

    fEs_WhitespaceAttri: TSynHighlighterAttributes;
    fEs_IdentifierAttri: TSynHighlighterAttributes;
    fEs_KeyAttri: TSynHighlighterAttributes;
    fEs_CommentAttri: TSynHighlighterAttributes;
    fEs_StringAttri: TSynHighlighterAttributes;
    fEs_NumberAttri: TSynHighlighterAttributes;
    fEs_SymbolAttri: TSynHighlighterAttributes;
    fEs_ErrorAttri: TSynHighlighterAttributes;

    // Php ---------------------------------------------------------------------
    fPhp_ProcTable: array[#0..#255] of TSynWebProcTableProc;
    fPhp_IdentFuncTable: array[0..Php_KeywordsMaxKeyHash] of TSynWebIdentFuncTableFunc;
    fPhp_RangeProcTable: array[Low(TSynWebPhpRangeState)..High(TSynWebPhpRangeState)] of
    TSynWebProcTableProc;

    fPhp_WhitespaceAttri: TSynHighlighterAttributes;
    fPhp_InlineTextAttri: TSynHighlighterAttributes;
    fPhp_IdentifierAttri: TSynHighlighterAttributes;
    fPhp_KeyAttri: TSynHighlighterAttributes;
    fPhp_FunctionAttri: TSynHighlighterAttributes;
    fPhp_VariableAttri: TSynHighlighterAttributes;
    fPhp_ConstAttri: TSynHighlighterAttributes;
    fPhp_StringAttri: TSynHighlighterAttributes;
    fPhp_StringSpecialAttri: TSynHighlighterAttributes;
    fPhp_CommentAttri: TSynHighlighterAttributes;
    fPhp_DocCommentAttri: TSynHighlighterAttributes;
    fPhp_SymbolAttri: TSynHighlighterAttributes;
    fPhp_NumberAttri: TSynHighlighterAttributes;
    fPhp_ErrorAttri: TSynHighlighterAttributes;

    // Html --------------------------------------------------------------------
    procedure Html_MakeMethodTables;
    procedure Html_Next;
    function Html_GetRange: TSynWebHtmlRangeState;
    procedure Html_SetRange(const ARange: TSynWebHtmlRangeState);
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
    function Html_TagCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_TagsFuncList.inc}

    function Html_AttrKeyComp(const ID: integer): boolean;
    function Html_AttrCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_AttrsFuncList.inc}

    function Html_SpecialKeyComp(const ID: integer): boolean;
    function Html_SpecialCheck(AStart, ALen: integer): integer;
    {$I SynHighlighterWeb_SpecialFuncList.inc}

    // Css ---------------------------------------------------------------------
    procedure Css_MakeMethodTables;
    procedure Css_NextBg;
    procedure Css_Next;
    procedure Css_UpdateBg;
    function Css_GetRange: TSynWebCssRangeState;
    procedure Css_SetRange(const ARange: TSynWebCssRangeState);
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
    function Css_PropCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_CssPropsFuncList.inc}

    function Css_ValKeyComp(const ID: integer): boolean;
    function Css_ValCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_CssValsFuncList.inc}

    function Css_SpecialKeyComp(const ID: integer): boolean;
    function Css_SpecialCheck(AStart, ALen: integer): integer;
    {$I SynHighlighterWeb_CssSpecialFuncList.inc}

    // ECMAScript --------------------------------------------------------------
    procedure Es_MakeMethodTables;
    procedure Es_Next;
    function Es_GetRange: TSynWebEsRangeState;
    procedure Es_SetRange(const ARange: TSynWebEsRangeState);
    function Es_CheckNull(ADo: boolean = True): boolean;

    procedure Es_SpaceProc;
    procedure Es_SlashProc;
    procedure Es_LowerProc;
    procedure Es_EqualNotProc;
    procedure Es_GreaterProc;
    procedure Es_AndProc;
    procedure Es_PlusProc;
    procedure Es_MinusProc;
    procedure Es_OrProc;
    procedure Es_MulModXorProc;
    procedure Es_NumberProc;
    procedure Es_String34Proc;
    procedure Es_String39Proc;
    procedure Es_SymbolProc;
    procedure Es_IdentProc;
    procedure Es_ErrorProc;

    procedure Es_RangeDefaultProc;
    procedure Es_RangeCommentProc;
    procedure Es_RangeCommentMultiProc;
    procedure Es_RangeString34Proc;
    procedure Es_RangeString39Proc;

    function Es_KeywordComp(const ID: integer): boolean;

    function Es_IdentCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_EsKeywordsFuncList.inc}

    // Php ---------------------------------------------------------------------
    procedure Php_MakeMethodTables;
    procedure Php_Next;
    procedure PhpCli_Next;
    function Php_GetRange: TSynWebPhpRangeState;
    procedure Php_SetRange(const ARange: TSynWebPhpRangeState);

    function Php_CheckBegin(ABegin: boolean = True): boolean;
    procedure Php_Begin(ATagKind: TSynWebPhpOpenTag);
    procedure Php_End(AHtmlTag: boolean);

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
    function Php_DoStringDouble(AIsHeredoc: boolean = False;
      ARangeChar: boolean = True): boolean;

    procedure Php_SubProcProc;
    procedure Php_RangeDefaultProc;
    procedure Php_RangeCommentProc;
    procedure Php_RangeString34Proc;
    procedure Php_RangeString39Proc;
    procedure Php_RangeStringShellProc;
    procedure Php_RangeHeredocProc;

    function Php_KeywordComp(const ID: integer): boolean;
    function Php_ConstComp: boolean;
    function Php_FunctionComp(const ID: integer): boolean;

    function Php_IdentCheck: TSynWebTokenKind;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Global
    property InactiveAttri: TSynHighlighterAttributes
      read FInactiveAttri write FInactiveAttri;
    property Options: TSynWebEngineOptions read FOptions;

    // Html
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

    // Css
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
    property EsWhitespaceAttri: TSynHighlighterAttributes
      read fEs_WhitespaceAttri write fEs_WhitespaceAttri;
    property EsIdentifierAttri: TSynHighlighterAttributes
      read fEs_IdentifierAttri write fEs_IdentifierAttri;
    property EsKeyAttri: TSynHighlighterAttributes read fEs_KeyAttri write fEs_KeyAttri;
    property EsCommentAttri: TSynHighlighterAttributes
      read fEs_CommentAttri write fEs_CommentAttri;
    property EsStringAttri: TSynHighlighterAttributes
      read fEs_StringAttri write fEs_StringAttri;
    property EsNumberAttri: TSynHighlighterAttributes
      read fEs_NumberAttri write fEs_NumberAttri;
    property EsSymbolAttri: TSynHighlighterAttributes
      read fEs_SymbolAttri write fEs_SymbolAttri;
    property EsErrorAttri: TSynHighlighterAttributes
      read fEs_ErrorAttri write fEs_ErrorAttri;

    // Php
    property PhpHereDocList: TStringList read FPhpHereDocList;
    property PhpWhitespaceAttri: TSynHighlighterAttributes
      read fPhp_WhitespaceAttri write fPhp_WhitespaceAttri;
    property PhpCliInlineTextAttri: TSynHighlighterAttributes
      read fPhp_InlineTextAttri write fPhp_InlineTextAttri;
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
    property PhpDocCommentAttri: TSynHighlighterAttributes
      read fPhp_DocCommentAttri write fPhp_DocCommentAttri;
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
  SynEditStrConst, Controls;

{$ENDIF}


{ TSynWebOptionsBase }

function TSynWebOptionsBase.GetHtmlVersion: TSynWebHtmlVersion;
begin
  Result := FOptions^.FHtmlVersion;
end;

procedure TSynWebOptionsBase.SetHtmlVersion(const Value: TSynWebHtmlVersion);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FHtmlVersion := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetCssVersion: TSynWebCssVersion;
begin
  Result := FOptions^.FCssVersion;
end;

procedure TSynWebOptionsBase.SetCssVersion(const Value: TSynWebCssVersion);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FCssVersion := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpVersion: TSynWebPhpVersion;
begin
  Result := FOptions^.FPhpVersion;
end;

procedure TSynWebOptionsBase.SetPhpVersion(const Value: TSynWebPhpVersion);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FPhpVersion := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpAspTags: boolean;
begin
  Result := FOptions^.FPhpAspTags;
end;

procedure TSynWebOptionsBase.SetPhpAspTags(const Value: boolean);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FPhpAspTags := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpShortOpenTag: boolean;
begin
  Result := FOptions^.FPhpShortOpenTag;
end;

procedure TSynWebOptionsBase.SetPhpShortOpenTag(const Value: boolean);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FPhpShortOpenTag := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetCssEmbeded: boolean;
begin
  Result := FOptions^.FCssEmbeded;
end;

procedure TSynWebOptionsBase.SetCssEmbeded(const Value: boolean);
begin
  FOptions^.FCssEmbeded := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetEsEmbeded: boolean;
begin
  Result := FOptions^.FEsEmbeded;
end;

procedure TSynWebOptionsBase.SetEsEmbeded(const Value: boolean);
begin
  FOptions^.FEsEmbeded := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpEmbeded: boolean;
begin
  Result := FOptions^.FPhpEmbeded;
end;

procedure TSynWebOptionsBase.SetPhpEmbeded(const Value: boolean);
begin
  FOptions^.FPhpEmbeded := Value;
  DoOnChange;
end;

procedure TSynWebOptionsBase.SetUseEngineOptions(const Value: Boolean);
begin
  if (FUseEngineOptions = Value) or (FEngineOptions = nil) then
    Exit;
  FUseEngineOptions := Value;
  UpdateOptions;
  DoOnChange;
end;

procedure TSynWebOptionsBase.SetEngineOptions(AEngine: PSynWebOptions);
begin
  if AEngine = FEngineOptions then
    Exit;
  FEngineOptions := AEngine;
  if UseEngineOptions and (AEngine <> nil) then
  begin
    UpdateOptions;
    DoOnChange;
  end;
end;

procedure TSynWebOptionsBase.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynWebOptionsBase.UpdateOptions;
begin
  if UseEngineOptions and (FEngineOptions <> nil) then
  begin
    FOptions^.FHtmlVersion := FEngineOptions^.FHtmlVersion;
    FOptions^.FCssVersion := FEngineOptions^.FCssVersion;
    FOptions^.FPhpVersion := FEngineOptions^.FPhpVersion;
    FOptions^.FPhpShortOpenTag := FEngineOptions^.FPhpShortOpenTag;
    FOptions^.FPhpAspTags := FEngineOptions^.FPhpAspTags;
  end;
end;

constructor TSynWebOptionsBase.Create(AOptions: PSynWebOptions);
begin
  FOnChange := nil;
  FEngineOptions := nil;
  FOptions := AOptions;
  FUseEngineOptions := True;

  FOptions^.FHtmlVersion := shvXHtml10Transitional;
  FOptions^.FCssVersion := scvCss21;
  FOptions^.FPhpVersion := spvPhp5;
  FOptions^.FPhpShortOpenTag := True;
  FOptions^.FPhpAspTags := False;

  FOptions^.FPhpEmbeded := False;
  FOptions^.FCssEmbeded := False;
  FOptions^.FEsEmbeded := False;
end;

{ TSynWebEngineOptions }

constructor TSynWebEngineOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  FUseEngineOptions := False;
end;

{ TSynWebBase }

procedure TSynWebBase.SetActiveHighlighter(const Value: boolean);
begin
  FActiveHighlighter := Value;
  if Value then
    SetupActiveHighlighter
  else
    FActiveHighlighters := [Low(TSynHighlighterType)..High(TSynHighlighterType)];
  DefHighlightChange(Self);
end;

function TSynWebBase.GetActiveHighlighters: TSynHighlighterTypes;
begin
  Result := FActiveHighlighters;
end;

procedure TSynWebBase.SetEngine(const Value: TSynWebEngine);
begin
  if FEngine <> nil then
    FEngine.RemoveFromNotifyList(Self);
  FEngine := Value;
  if FEngine <> nil then
  begin
    FEngine.AddToNotifyList(Self);
    FOptions.SetEngineOptions(@FEngine.FEngineOptions);
  end
  else
    FOptions.SetEngineOptions(nil);
end;

procedure TSynWebBase.DoDefHighlightChange;
begin
  FOptions.UpdateOptions;
  if FInstance.FOptions.FHtmlVersion >= shvXHtml10Strict then
    FInstance.FHashTable := TSynWebSensitiveHashTable
  else
    FInstance.FHashTable := TSynWebInsensitiveHashTable;
  DefHighlightChange(Self);
end;

function TSynWebBase.GetAttribCount: integer;
begin
  if FEngine = nil then
    Result := 0
  else
    Result := FEngine.FAttributes.Count;
end;

function TSynWebBase.GetAttribute(idx: integer): TSynHighlighterAttributes;
begin
  Result := nil;
  if (FEngine <> nil) and (idx >= 0) and (idx < FEngine.FAttributes.Count) then
    Result := TSynHighlighterAttributes(FEngine.FAttributes.Objects[idx]);
end;

function TSynWebBase.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

constructor TSynWebBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions.FOnChange := DefHighlightChange;
  FEngine := nil;
  FDefaultFilter := '';
  FActiveHighlighter := False;
  FActiveHighlighters := [shtHtml, shtCss, shtEs, shtPhp_inHtml,
    shtPhp_inCss, shtPhp_inEs];
  ResetRange;
  DoDefHighlightChange;
end;

destructor TSynWebBase.Destroy;
begin
  Engine := nil;
  inherited Destroy;
end;

function TSynWebBase.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  if FEngine = nil then
    Result := nil
  else
    case Index of
      // SYN_ATTR_IDENTIFIER: ??
      // SYN_ATTR_KEYWORD: ??
      // SYN_ATTR_SYMBOL: ??
      SYN_ATTR_WHITEsPACE:
      begin
        Result := FInstance.fSYN_ATTR_WHITEsPACE;
        if not Enabled then
          case FInstance.FHighlighterMode of
            shmHtml:
              Result := fEngine.fHtml_WhitespaceAttri;
            shmCss:
              Result := fEngine.fCss_WhitespaceAttri;
            shmEs:
              Result := fEngine.fEs_WhitespaceAttri;
            shmPhpCli:
              Result := fEngine.fPhp_InlineTextAttri;
          end;
      end;
      SYN_ATTR_COMMENT:
        Result := FInstance.fSYN_ATTR_COMMENT;
      SYN_ATTR_STRING:
        Result := FInstance.fSYN_ATTR_STRING;
      else
        Result := nil;
    end;
end;

function TSynWebBase.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if FEngine = nil then
    Result := nil
  else
    if (FInstance.FHighlighterType in FActiveHighlighters) then
      Result := FEngine.FTokenAttributeTable[FInstance.FTokenID]
    else
      Result := FEngine.FInactiveAttri;
end;

function TSynWebBase.GetToken: string;
var
  Len: longint;
begin
  Len := FInstance.FRun - FInstance.FTokenPos;
  SetString(Result, (FInstance.FLine + FInstance.FTokenPos), Len);
end;

function TSynWebBase.GetTokenLen: integer;
begin
  Result := FInstance.FRun - FInstance.FTokenPos;
end;

function TSynWebBase.GetTokenPos: integer;
begin
  Result := FInstance.FTokenPos;
end;

function TSynWebBase.GetTokenID: TSynWebTokenKind;
begin
  Result := FInstance.FTokenID;
end;

function TSynWebBase.GetTokenKind: integer;
begin
  Result := Ord(FInstance.FTokenID);
end;

function TSynWebBase.GetRange: Pointer;
begin
  Result := Pointer(FInstance.FRange);
end;

function TSynWebBase.GetEol: boolean;
begin
  Result := FInstance.FTokenID = stkNull;
end;

procedure TSynWebBase.SetRange(Value: Pointer);
begin
  FInstance.FRange := Longword(Value);
end;

procedure TSynWebBase.SetLine(NewValue: string; LineNumber: integer);
begin
  if FEngine = nil then
    Exit;
  FEngine.FInstance := @FInstance;
  FEngine.SetLine(NewValue, LineNumber);
end;

procedure TSynWebBase.Next;
begin
  if FEngine = nil then
    FInstance.FTokenID := stkNull
  else
  begin
    FEngine.FInstance := @FInstance;
    FEngine.Next;
  end;
end;

function TSynWebBase.UpdateActiveHighlighter(ARange: Pointer;
  ALine: string; ACaretX, ACaretY: integer): boolean;
var
  f: TSynHighlighterTypes;
  lPos, lLen: integer;
  lHinghlighter, ActiveHL: TSynHighlighterType;
begin
  Result := True;
  if not FActiveHighlighter or not (FInstance.FOptions.FPhpEmbeded or FInstance.FOptions.FCssEmbeded or
    FInstance.FOptions.FEsEmbeded) then
    Exit;
  f := FActiveHighlighters;
  Dec(ACaretX);
  SetRange(ARange);
  lHinghlighter := TSynHighlighterType((FInstance.FRange shr 29) and not ($FFFFFFFF shl 3));
  SetLine(ALine, ACaretY);
  lPos := GetTokenPos;
  lLen := GetTokenLen;
  while (GetTokenPos < ACaretX) and not GetEol do
  begin
    lHinghlighter := FInstance.FHighlighterType;
    lPos := GetTokenPos;
    lLen := GetTokenLen;
    Next;
  end;
  if FInstance.FUseNextAH and (ACaretX >= lPos + lLen) then
    ActiveHL := FInstance.FHighlighterType
  else
    if FInstance.FHighlighterSW and (ACaretX >= lPos + lLen) then
      ActiveHL := FInstance.FPrevHighlighterType
    else
      ActiveHL := lHinghlighter;
  if ActiveHL >= shtPhp_inHtml then
    FActiveHighlighters := [shtPhp_inHtml, shtPhp_inCss, shtPhp_inEs]
  else
    FActiveHighlighters := [ActiveHL];
  Result := f <> FActiveHighlighters;
end;

{ TSynWebHtmlSyn }

procedure TSynWebHtmlSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtHtml];
end;
          
function TSynWebHtmlSyn.GetOptions: TSynWebHtmlOptions;
begin
  Result := TSynWebHtmlOptions(FOptions);
end;

constructor TSynWebHtmlSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebHtmlOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmHtml;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := True;
  FOptions.EsEmbeded := True;
end;

procedure TSynWebHtmlSyn.ResetRange;
begin
  FInstance.FRange := $00000000;
end;

function TSynWebHtmlSyn.GetSampleSource: string;
begin

end;

{ TSynWebCssSyn }

procedure TSynWebCssSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtCss];
end;

function TSynWebCssSyn.GetOptions: TSynWebCssOptions;
begin
  Result := TSynWebCssOptions(FOptions);
end;

constructor TSynWebCssSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebCssOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmCss;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebCssSyn.ResetRange;
begin
  with FInstance do
  begin
    FRange := $00000000;
    FRange := FRange or (Longword(shtCss) shl 29);
  end;
end;

function TSynWebCssSyn.GetSampleSource: string;
begin

end;

{ TSynWebEsSyn }

procedure TSynWebEsSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtEs];
end;

function TSynWebEsSyn.GetOptions: TSynWebEsOptions;
begin
  Result := TSynWebEsOptions(FOptions);
end;

constructor TSynWebEsSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebEsOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmEs;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebEsSyn.ResetRange;
begin
  with FInstance do
  begin
    FRange := $00000000;
    FRange := FRange or (Longword(shtEs) shl 29);
  end;
end;

function TSynWebEsSyn.GetSampleSource: string;
begin

end;

{ TSynWebPhpCliSyn }

procedure TSynWebPhpCliSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtHtml];
end;

function TSynWebPhpCliSyn.GetOptions: TSynWebPhpCliOptions;
begin
  Result := TSynWebPhpCliOptions(FOptions);
end;

constructor TSynWebPhpCliSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebPhpCliOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmPhpCli;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebPhpCliSyn.ResetRange;
begin
  FInstance.FRange := $00000000;
end;

function TSynWebPhpCliSyn.GetSampleSource: string;
begin

end;

{ TSynWebEngine }

// Html ------------------------------------------------------------------------

procedure TSynWebEngine.Html_MakeMethodTables;
var
  i: integer;
  pF: PSynWebIdentFuncTableFunc;
  pF2: PSynWebIdent2FuncTableFunc;
begin
  fHtml_RangeProcTable[srsHtmlText] := Html_RangeTextProc;
  fHtml_RangeProcTable[srsHtmlComment] := Html_RangeCommentProc;
  fHtml_RangeProcTable[srsHtmlCommentClose] := Html_RangeCommentCloseProc;
  fHtml_RangeProcTable[srsHtmlTag] := Html_RangeTagProc;
  fHtml_RangeProcTable[srsHtmlTagClose] := Html_RangeTagCloseProc;
  fHtml_RangeProcTable[srsHtmlTagDOCTYPE] := Html_RangeTagDOCTYPEProc;
  fHtml_RangeProcTable[srsHtmlTagCDATA] := Html_RangeTagCDATAProc;
  fHtml_RangeProcTable[srsHtmlTagKey] := Html_RangeTagKeyProc;
  fHtml_RangeProcTable[srsHtmlTagKeyEq] := Html_RangeTagKeyEqProc;
  fHtml_RangeProcTable[srsHtmlTagKeyValue] := Html_RangeTagKeyValueProc;
  fHtml_RangeProcTable[srsHtmlTagKeyValueQuoted1] := Html_RangeTagKeyValueQuoted1Proc;
  fHtml_RangeProcTable[srsHtmlTagKeyValueQuoted2] := Html_RangeTagKeyValueQuoted2Proc;

  pF := PSynWebIdentFuncTableFunc(@fHtml_TagIdentFuncTable);
  for I := Low(fHtml_TagIdentFuncTable) to High(fHtml_TagIdentFuncTable) do
  begin
    pF^ := Html_TagUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_TagsFuncTable.inc}

  pF := PSynWebIdentFuncTableFunc(@fHtml_AttrIdentFuncTable);
  for I := Low(fHtml_TagIdentFuncTable) to High(fHtml_AttrIdentFuncTable) do
  begin
    pF^ := Html_AttrUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_AttrsFuncTable.inc}

  pF2 := PSynWebIdent2FuncTableFunc(@fHtml_SpecialIdentFuncTable);
  for I := Low(fHtml_SpecialIdentFuncTable) to High(fHtml_SpecialIdentFuncTable) do
  begin
    pF2^ := Html_SpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_SpecialFuncTable.inc}
end;

procedure TSynWebEngine.Html_Next;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  fHtml_RangeProcTable[Html_GetRange];
end;

function TSynWebEngine.Html_GetRange: TSynWebHtmlRangeState;
begin
  Result := TSynWebHtmlRangeState(GetRange_Int(4, 13));
end;

procedure TSynWebEngine.Html_SetRange(const ARange: TSynWebHtmlRangeState);
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
  if FInstance^.FLine[FInstance^.FRun] = #0 then
  begin
    Result := True;
    if ADo then
      NullProc;
  end else
    Result := False;
end;

procedure TSynWebEngine.Html_SpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkHtmlSpace;
end;

procedure TSynWebEngine.Html_AmpersandProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkHtmlEscape;
  if FInstance^.FLine[FInstance^.FRun] = '#' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun]] = FInstance^.FHashTable['x'] then
    begin
      Inc(FInstance^.FRun);
      // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0 then
        FInstance^.FTokenID := stkHtmlError
      else
        repeat
          Inc(FInstance^.FRun)
          // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0;
    end else
      if not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']) then
        FInstance^.FTokenID := stkHtmlError
      else
        repeat
          Inc(FInstance^.FRun)
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']);
  end else
  // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
      FInstance^.FTokenID := stkHtmlError
    else
    begin
      repeat
        Inc(FInstance^.FRun)
        // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'];
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
      if Html_SpecialCheck(FInstance^.FTokenPos + 1, FInstance^.FRun -
        FInstance^.FTokenPos - 1) = -1 then
        FInstance^.FTokenID := stkHtmlError;
    end;
  if FInstance^.FLine[FInstance^.FRun] = ';' then
    Inc(FInstance^.FRun)
  else
    FInstance^.FTokenID := stkHtmlError;
end;

procedure TSynWebEngine.Html_BraceOpenProc;
begin
  if Php_CheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '/':
    begin
      Inc(FInstance^.FRun);
      SetRange_Bit(12, True);
    end;
    '?':
    begin
      if FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict then
        Inc(FInstance^.FRun);
      SetRange_Bit(12, False);
    end;
    '!':
    begin
      Inc(FInstance^.FRun);
      if (FInstance^.FLine[FInstance^.FRun] = '-') and
        (FInstance^.FLine[FInstance^.FRun + 1] = '-') then
      begin
        Inc(FInstance^.FRun, 2);
        Html_SetRange(srsHtmlComment);
        if (FInstance^.FLine[FInstance^.FRun] = #0) or Php_CheckBegin(False) then
          FInstance^.FTokenID := stkHtmlComment
        else
          Html_RangeCommentProc;
      end else
        if (FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict) and
          (FInstance^.FLine[FInstance^.FRun] = '[') and
          (FInstance^.FLine[FInstance^.FRun + 1] = 'C') and
          (FInstance^.FLine[FInstance^.FRun + 2] = 'D') and
          (FInstance^.FLine[FInstance^.FRun + 3] = 'A') and
          (FInstance^.FLine[FInstance^.FRun + 4] = 'T') and
          (FInstance^.FLine[FInstance^.FRun + 5] = 'A') and
          (FInstance^.FLine[FInstance^.FRun + 6] = '[') then
        begin
          Inc(FInstance^.FRun, 7);
          FInstance^.FTokenID := stkHtmlTag;
          Html_SetRange(srsHtmlTagCDATA);
        end else
          if (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun]] =
            FInstance^.FHashTable['D']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 1]] =
            FInstance^.FHashTable['O']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
            FInstance^.FHashTable['C']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
            FInstance^.FHashTable['T']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
            FInstance^.FHashTable['Y']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
            FInstance^.FHashTable['P']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
            FInstance^.FHashTable['E']) and
            // (not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'])) then
            (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) = 0) then
          begin
            FInstance^.FTokenID := stkHtmlTag;
            SetRange_Int(2, 7, 0);
            Html_SetRange(srsHtmlTagDOCTYPE);
          end else
            FInstance^.FTokenID := stkHtmlError;
      Exit;
    end;
    else
      SetRange_Bit(12, False);
  end;
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) <> 0 then
  begin
    FInstance^.FTokenID := stkHtmlTag;
    Html_SetRange(srsHtmlTag);
  end else
    FInstance^.FTokenID := stkHtmlError;
end;

procedure TSynWebEngine.Html_ErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkHtmlError;
end;

procedure TSynWebEngine.Html_RangeTextProc;
begin
  case FInstance^.FLine[FInstance^.FRun] of
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
        Inc(FInstance^.FRun);
        // until FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>', '&'];
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) <> 0;
      FInstance^.FTokenID := stkHtmlText;
  end;
end;

procedure TSynWebEngine.Html_RangeCommentProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun) in [#0, '-', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 19) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '-':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '-' then
        begin
          Inc(FInstance^.FRun);
          if FInstance^.FLine[FInstance^.FRun] = '>' then
          begin
            Inc(FInstance^.FRun);
            Html_SetRange(srsHtmlText);
          end else
          begin
            Html_SetRange(srsHtmlCommentClose);
            if (FInstance^.FLine[FInstance^.FRun] <> #0) and not Php_CheckBegin(False) then
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
          Inc(FInstance^.FRun);
    end;
  until False;
  FInstance^.FTokenID := stkHtmlComment;
end;

procedure TSynWebEngine.Html_RangeCommentCloseProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun) in [#0, '<', '>']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 20) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '>':
      begin
        Inc(FInstance^.FRun);
        Html_SetRange(srsHtmlText);
        Break;
      end;
      '<':
        if Php_CheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
    end;
  until False;
  FInstance^.FTokenID := stkHtmlComment;
end;

procedure TSynWebEngine.Html_RangeTagDOCTYPEProc;
begin
  case GetRange_Int(2, 7) of
    0:
    begin
      Inc(FInstance^.FRun, 7);
      FInstance^.FTokenID := stkHtmlTagName;
      SetRange_Int(2, 7, 1);
    end;
    1:
      if not Html_CheckNull and not Php_CheckBegin then
        case FInstance^.FLine[FInstance^.FRun] of
          #1..#32:
          begin
            Html_SpaceProc;
            Exit;
          end;
          '>':
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkHtmlTag;
            SetRange_Int(2, 7, 0);
            Html_SetRange(srsHtmlText);
            Exit;
          end;
          #39:
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #0 then
              FInstance^.FTokenID := stkHtmlError
            else
            begin
              SetRange_Int(2, 7, 2);
              if Php_CheckBegin(False) then
                FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
              else
                Html_RangeTagDOCTYPEProc;
            end;
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #0 then
              FInstance^.FTokenID := stkHtmlError
            else
            begin
              SetRange_Int(2, 7, 3);
              if Php_CheckBegin(False) then
                FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
              else
                Html_RangeTagDOCTYPEProc;
            end;
          end;
          else
            if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
              // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
            begin
              Inc(FInstance^.FRun);
              FInstance^.FTokenID := stkHtmlError;
              Exit;
            end;
            repeat
              Inc(FInstance^.FRun);
            until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
            // until not (FInstance^.FLine[FInstance^.FRun] In ['a'..'z', 'A'..'Z']);
            FInstance^.FTokenID := stkHtmlTagKey;
        end;
    2:
    begin
      if not Html_CheckNull then
        if Php_CheckBegin then
          Exit
        else
          repeat
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
              // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<']) do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
              #0:
              begin
                FInstance^.FTokenID := stkHtmlError;
                Break;
              end;
              '<':
                if Php_CheckBegin(False) then
                begin
                  FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
                  Exit;
                end else
                  Inc(FInstance^.FRun);
              #39:
              begin
                Inc(FInstance^.FRun);
                FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
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
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
              // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<']) do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
              #0:
              begin
                FInstance^.FTokenID := stkHtmlError;
                Break;
              end;
              '<':
                if Php_CheckBegin(False) then
                begin
                  FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
                  Exit;
                end else
                  Inc(FInstance^.FRun);
              '"':
              begin
                Inc(FInstance^.FRun);
                FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
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
  if FInstance^.FLine[FInstance^.FRun] in [#1..#32] then
  begin
    Html_SpaceProc;
    Exit;
  end else
    if (FInstance^.FLine[FInstance^.FRun] = ']') and
      (FInstance^.FLine[FInstance^.FRun + 1] = ']') and
      (FInstance^.FLine[FInstance^.FRun + 2] = '>') then
    begin
      Inc(FInstance^.FRun, 3);
      FInstance^.FTokenID := stkHtmlTag;
      Html_SetRange(srsHtmlText);
    end else
    begin
      repeat
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 1) <> 0;
        // until FInstance^.FLine[FInstance^.FRun] in [#0..#32, '<', ']'];
        case FInstance^.FLine[FInstance^.FRun] of
          #0..#32, ']':
            Break;
          '<':
            if Php_CheckBegin(False) then
              Break;
        end;
      until False;
      FInstance^.FTokenID := stkHtmlText;
    end;
end;

procedure TSynWebEngine.Html_RangeTagProc;
var
  ID: integer;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 16) = 0;
  // until not (FInstance^.FLine[FInstance^.FRun] In ['a'..'z', 'A'..'Z', '_', '0'..'9']);
  FInstance^.FTokenID := Html_TagCheck;
  ID := Html_GetTag - 1;
  if GetRange_Bit(12) then
  begin
    if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      FInstance^.FTokenID := stkHtmlError;
    Html_SetRange(srsHtmlTagClose);
  end else
  begin
    if (ID <> -1) and ((FInstance^.FLine[FInstance^.FTokenPos - 1] = '?') xor
      (TSynWeb_TagsData[ID] and (1 shl 29) <> 0)) then
      FInstance^.FTokenID := stkHtmlError;
    Html_SetRange(srsHtmlTagKey);
  end;
end;

procedure TSynWebEngine.Html_RangeTagCloseProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      Html_SpaceProc;
    '>':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkHtmlTag;
      Html_SetRange(srsHtmlText);
    end;
    else
      FInstance^.FTokenID := stkHtmlError;
      repeat
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 1) <> 0;
        // until not (FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>']) do
        if (FInstance^.FLine[FInstance^.FRun] = '<') and not Php_CheckBegin(False) then
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
    if (FInstance^.FLine[FInstance^.FRun] = '?') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '>') then
    begin
      Inc(FInstance^.FRun, 2);
      FInstance^.FTokenID := stkHtmlTag;
      Html_SetRange(srsHtmlText);
      Exit;
    end else
      if FInstance^.FLine[FInstance^.FRun] = '>' then
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkHtmlError;
        Html_SetRange(srsHtmlText);
        Exit;
      end;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      Html_SpaceProc;
    '/':
      if not GetRange_Bit(12) and (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
        (FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict) and
        (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      begin
        Inc(FInstance^.FRun, 2);
        FInstance^.FTokenID := stkHtmlTag;
        Html_SetRange(srsHtmlText);
      end else
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkHtmlError;
      end;
    '>':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkHtmlTag;
      if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) and
        (FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict) then
        FInstance^.FTokenID := stkHtmlError
      else
        if not GetRange_Bit(12) and ((FInstance^.FRun = 0) or
          (FInstance^.FLine[FInstance^.FRun - 2] <> '/')) then
          if (ID = Html_TagID_Style) and FInstance^.FOptions.FCssEmbeded then
          begin
            SetHighlighterType(shtCss, True, True, True);
            Exit;
          end else
            if (ID = Html_TagID_Script) then
              if GetRange_Bit(28) and FInstance^.FOptions.FPhpEmbeded then
              begin
                SetRange_Int(17, 0, 0);
                Php_Begin(spotHtml);
                Exit;
              end else
                if FInstance^.FOptions.FEsEmbeded then
                begin
                  SetHighlighterType(shtEs, True, True, True);
                  Exit;
                end;
      Html_SetRange(srsHtmlText);
    end;
    else
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
        // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
        Html_ErrorProc
      else
      begin
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 7) = 0;
        // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', ':', '-']);
        if ID = -1 then
          FInstance^.FTokenID := stkHtmlTagKeyUndef
        else
        begin
          FInstance^.FTokenID := Html_AttrCheck;
          if ID = Html_TagID_Script then
            SetRange_Bit(27, FInstance^.FToken_LastID = Html_AttrID_Language);
        end;
      end;
      Html_SetRange(srsHtmlTagKeyEq);
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyEqProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      Html_SpaceProc;
    '=':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkHtmlSymbol;
      Html_SetRange(srsHtmlTagKeyValue);
    end;
    else
      Html_SetRange(srsHtmlTagKey);
      Html_RangeTagKeyProc;
      if FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict then
        FInstance^.FTokenID := stkHtmlError;
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyValueProc;
var
  ID: integer;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      Html_SpaceProc;
    #39:
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = #0 then
      begin
        Html_SetRange(srsHtmlTagKey);
        FInstance^.FTokenID := stkHtmlError;
      end else
      begin
        Html_SetRange(srsHtmlTagKeyValueQuoted1);
        if Php_CheckBegin(False) then
          FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
        else
          Html_RangeTagKeyValueQuoted1Proc;
      end;
    end;
    '"':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = #0 then
      begin
        Html_SetRange(srsHtmlTagKey);
        FInstance^.FTokenID := stkHtmlError;
      end else
      begin
        Html_SetRange(srsHtmlTagKeyValueQuoted2);
        if Php_CheckBegin(False) then
          FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
        else
          Html_RangeTagKeyValueQuoted2Proc;
      end;
    end;
    else
      if (FInstance^.FLine[FInstance^.FRun] = '>') or
        ((FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict) and
        (FInstance^.FLine[FInstance^.FRun] = '/') and
        (FInstance^.FLine[FInstance^.FRun + 1] = '>')) then
      begin
        if FInstance^.FLine[FInstance^.FRun] = '/' then
          Inc(FInstance^.FRun, 2)
        else
          Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkHtmlError;
        if not GetRange_Bit(12) and ((FInstance^.FRun = 0) or
          (FInstance^.FLine[FInstance^.FRun - 2] <> '/')) then
        begin
          ID := Html_GetTag - 1;
          if (ID = Html_TagID_Style) and FInstance^.FOptions.FCssEmbeded then
          begin
            SetHighlighterType(shtCss, True, True, True);
            Exit;
          end else
            if (ID = Html_TagID_Script) then
              if GetRange_Bit(28) and FInstance^.FOptions.FPhpEmbeded then
              begin
                SetRange_Int(17, 0, 0);
                Php_Begin(spotHtml);
                Exit;
              end else
                if FInstance^.FOptions.FEsEmbeded then
                begin
                  SetHighlighterType(shtEs, True, True, True);
                  Exit;
                end;
        end;
        Html_SetRange(srsHtmlText);
      end else
      begin
        repeat
          repeat
            Inc(FInstance^.FRun);
          until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 23) <> 0;
          // until FInstance^.FLine[FInstance^.FRun] in [#0..#32, '<', '>', '/'];
          case FInstance^.FLine[FInstance^.FRun] of
            '/':
              if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
                (FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict) then
                Break;
            '<':
              if Php_CheckBegin(False) then
                Break
              else
                Inc(FInstance^.FRun);
            else
              Break;
          end;
        until False;
        if FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict then
          FInstance^.FTokenID := stkHtmlError
        else
          FInstance^.FTokenID := stkHtmlTagKeyValue;
        if GetRange_Bit(27) then
          SetRange_Bit(28, UpperCase(GetToken) = 'Php');
        Html_SetRange(srsHtmlTagKey);
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
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
          // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<']) do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkHtmlError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
              Exit;
            end else
              Inc(FInstance^.FRun);
          #39:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
            if GetRange_Bit(27) then
              SetRange_Bit(28, UpperCase(GetToken) = #39'Php'#39);
            Break;
          end;
        end;
      until False;
  Html_SetRange(srsHtmlTagKey);
end;

procedure TSynWebEngine.Html_RangeTagKeyValueQuoted2Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
          // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<']) do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkHtmlError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
              Exit;
            end else
              Inc(FInstance^.FRun);
          '"':
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
            if GetRange_Bit(27) then
              SetRange_Bit(28, UpperCase(GetToken) = '"Php"');
            Break;
          end;
        end;
      until False;
  Html_SetRange(srsHtmlTagKey);
end;

function TSynWebEngine.Html_TagKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  if TSynWeb_TagsData[ID] and (1 shl Longword(FInstance^.FOptions.FHtmlVersion)) = 0 then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Tags[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if FInstance^.FHashTable[Temp^] <> FInstance^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Html_TagCheck: TSynWebTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, FInstance^.FHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FToken_LastID := -1;
  if HashKey <= Html_TagMaxKeyHash then
    Result := fHtml_TagIdentFuncTable[HashKey]
  else
    Result := stkHtmlTagNameUndef;
  Html_SetTag(FInstance^.FToken_LastID + 1);
end;

{$I SynHighlighterWeb_TagsFunc.inc}

function TSynWebEngine.Html_AttrKeyComp(const ID: integer): boolean;
var
  I, tag: integer;
  Temp: PChar;
  aKey: string;
begin
  tag := Html_GetTag - 1;
  if (tag = -1) or (TSynWeb_AttrsData[ID][Longword(FInstance^.FOptions.FHtmlVersion)]
    [tag div 32] and (1 shl (tag mod 32)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Attrs[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if FInstance^.FHashTable[Temp^] <> FInstance^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Html_AttrCheck: TSynWebTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, FInstance^.FHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FToken_LastID := -1;
  if HashKey <= Html_AttrMaxKeyHash then
    Result := fHtml_AttrIdentFuncTable[HashKey]
  else
    Result := stkHtmlTagKeyUndef;
end;

{$I SynHighlighterWeb_AttrsFunc.inc}

function TSynWebEngine.Html_SpecialKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_Special[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if FInstance^.FHashTable[Temp^] <> FInstance^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
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
    FInstance^.FStringLen := ALen;
    for i := 0 to ALen - 1 do
    begin
      Inc(HashKey, FInstance^.FHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[AStart];
  KeyHash(FInstance^.FToIdent);
  if (HashKey > Html_SpecialMaxKeyHash) or not fHtml_SpecialIdentFuncTable[HashKey] then
    FInstance^.FToken_LastID := -1;
  Result := FInstance^.FToken_LastID;
end;

{$I SynHighlighterWeb_SpecialFunc.inc}

// Css -------------------------------------------------------------------------

procedure TSynWebEngine.Css_MakeMethodTables;
var
  c: char;
  i: integer;
  pF: PSynWebIdentFuncTableFunc;
  pF2: PSynWebIdent2FuncTableFunc;
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

  fCss_RangeProcTable[srsCssRuleset] := Css_RangeRulesetProc;
  fCss_RangeProcTable[srsCssSelectorAttrib] := Css_RangeSelectorAttribProc;
  fCss_RangeProcTable[srsCssSelectorPseudo] := Css_RangeSelectorPseudoProc;
  fCss_RangeProcTable[srsCssAtKeyword] := Css_RangeAtKeywordProc;
  fCss_RangeProcTable[srsCssComment] := Css_RangeCommentProc;
  fCss_RangeProcTable[srsCssProp] := Css_RangePropProc;
  fCss_RangeProcTable[srsCssPropVal] := Css_RangePropValProc;
  fCss_RangeProcTable[srsCssPropValStr] := Css_RangePropValStrProc;
  fCss_RangeProcTable[srsCssPropValRgb] := Css_RangePropValRgbProc;
  fCss_RangeProcTable[srsCssPropValSpecial] := Css_RangePropValSpecialProc;
  fCss_RangeProcTable[srsCssPropValImportant] := Css_RangePropValImportantProc;
  fCss_RangeProcTable[srsCssPropValUrl] := Css_RangePropValUrlProc;
  fCss_RangeProcTable[srsCssPropValRect] := Css_RangePropValRectProc;
  fCss_RangeProcTable[srsCssPropValFunc] := Css_RangePropValFuncProc;

  pF := PSynWebIdentFuncTableFunc(@fCss_PropIdentFuncTable);
  for I := Low(fCss_PropIdentFuncTable) to High(fCss_PropIdentFuncTable) do
  begin
    pF^ := Css_PropUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssPropsFuncTable.inc}

  pF := PSynWebIdentFuncTableFunc(@fCss_ValIdentFuncTable);
  for I := Low(fCss_ValIdentFuncTable) to High(fCss_ValIdentFuncTable) do
  begin
    pF^ := Css_ValUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssValsFuncTable.inc}

  pF2 := PSynWebIdent2FuncTableFunc(@fCss_SpecialIdentFuncTable);
  for I := Low(fCss_SpecialIdentFuncTable) to High(fCss_SpecialIdentFuncTable) do
  begin
    pF2^ := Css_SpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_CssSpecialFuncTable.inc}
end;

procedure TSynWebEngine.Css_NextBg;
begin
  Css_UpdateBg;
  FInstance^.FNextProcTable := Css_Next;
  Css_Next;
end;

procedure TSynWebEngine.Css_Next;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  fCss_RangeProcTable[Css_GetRange];
end;

procedure TSynWebEngine.Css_UpdateBg;
begin
  if TSynWebCssRangeState(GetRange_Int(4, 13)) in
    [TSynWebCssRangeState_RulesetBegin..TSynWebCssRangeState_RulesetEnd] then
    FInstance^.FSYN_ATTR_WHITESPACE := fCss_RulesetWhitespaceAttri
  else
    FInstance^.FSYN_ATTR_WHITESPACE := fCss_WhitespaceAttri;
  FTokenAttributeTable[stkCssSpace] := FInstance^.FSYN_ATTR_WHITESPACE;
end;

function TSynWebEngine.Css_GetRange: TSynWebCssRangeState;
begin
  if GetRange_Bit(12) then
    Result := srsCssComment
  else
    Result := TSynWebCssRangeState(GetRange_Int(4, 13));
end;

procedure TSynWebEngine.Css_SetRange(const ARange: TSynWebCssRangeState);
begin
  if ARange = srsCssComment then
    SetRange_Bit(12, True)
  else
  begin
    if not (ARange in [TSynWebCssRangeState_RulesetBegin..
      TSynWebCssRangeState_RulesetEnd]) and
      (TSynWebCssRangeState(GetRange_Int(4, 13)) in
      [TSynWebCssRangeState_RulesetBegin..TSynWebCssRangeState_RulesetEnd]) then
    begin
      SetRange_Int(4, 13, Longword(ARange));
      FInstance^.FNextProcTable := Css_NextBg;
    end else
    begin
      SetRange_Int(4, 13, Longword(ARange));
      Css_UpdateBg;
    end;
    if ARange = srsCssRuleset then
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
  case FInstance^.FLine[FInstance^.FRun] of
    #0:
    begin
      Result := True;
      if ADo then
        NullProc;
    end;
    '<':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '/') and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
        FInstance^.FHashTable['s']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
        FInstance^.FHashTable['t']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
        FInstance^.FHashTable['y']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
        FInstance^.FHashTable['l']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
        FInstance^.FHashTable['e']) and
        (TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) <> 0) and
        // (FInstance^.FLine[FInstance^.FRun+7] in [#0..#32, '>']) and
        (FInstance^.FHighlighterMode = shmHtml) then
      begin
        Result := True;
        if ADo then
        begin
          FInstance^.FTokenID := stkHtmlTag;
          SetHighlighterType(shtHtml, True, False, False);
        end;
      end else
        Result := False;
    else
      Result := False;
  end;
end;

procedure TSynWebEngine.Css_SpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkCssSpace;
end;

procedure TSynWebEngine.Css_AtKeywordProc;
begin
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) = 0 then
    // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z']) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssAtKeyword);
  end;
end;

procedure TSynWebEngine.Css_SlashProc;
begin
  if FInstance^.FLine[FInstance^.FRun + 1] = '*' then
  begin
    Inc(FInstance^.FRun, 2);
    SetRange_Bit(12, True); // Css_SetRange(srsCssComment);
    if Css_CheckNull(False) or Php_CheckBegin(False) then
      FInstance^.FTokenID := stkCssComment
    else
      Css_RangeCommentProc;
  end else
    if (Css_GetRange = srsCssPropVal) and GetRange_Bit(8) then
    begin
      SetRange_Bit(8, False);
      Css_SymbolProc;
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_BraceOpenProc;
begin
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  if (FInstance^.FLine[FInstance^.FRun + 1] = '!') and
    (FInstance^.FLine[FInstance^.FRun + 2] = '-') and
    (FInstance^.FLine[FInstance^.FRun + 3] = '-') then
  begin
    Inc(FInstance^.FRun, 4);
    FInstance^.FTokenID := stkHtmlComment;
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_CurlyBraceOpenProc;
begin
  Css_SymbolProc;
  Css_SetRange(srsCssProp);
end;

procedure TSynWebEngine.Css_CurlyBraceCloseProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssRuleset);
  end else
    if GetRange_Bit(11) then
    begin
      SetRange_Bit(11, False);
      Css_SymbolProc;
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_ChildAnySelectorProc;
begin
  if FInstance^.FOptions.FCssVersion = scvCss21 then
    Css_SymbolProc
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_AttribProc;
begin
  if FInstance^.FOptions.FCssVersion = scvCss1 then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssSelectorAttrib);
  end;
end;

procedure TSynWebEngine.Css_HashProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 10) <> 0) and
      // if FInstance^.FLine[FInstance^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9'] and
      (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 10) <> 0) and
      //   FInstance^.FLine[FInstance^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9'] and
      (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 3]] and (1 shl 10) <> 0) then
      //   FInstance^.FLine[FInstance^.FRun+3] in ['a'..'f', 'A'..'F', '0'..'9'] then
    begin
      Css_SymbolProc;
      Css_SetRange(srsCssPropValSpecial);
    end else
      Css_ErrorProc;
  end else
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 8) = 0) or
      // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z', '\']) or
      ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
      (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
      Css_ErrorProc
    else
    begin
      Css_SymbolProc;
      SetRange_Bit(8, True);
    end;
end;

procedure TSynWebEngine.Css_DotProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    if FInstance^.FLine[FInstance^.FRun + 1] in ['0'..'9'] then
    begin
      FInstance^.FCssMask := $F5000000;
      Css_NumberDefProc;
    end else
      Css_ErrorProc;
  end else
  begin
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 8) = 0) or
      // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
      ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
      (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
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
  if Css_GetRange = srsCssPropVal then
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
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) = 0 then
    // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z']) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssSelectorPseudo);
  end;
end;

procedure TSynWebEngine.Css_SemiColonProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssProp);
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_ExclamationProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssPropValImportant);
    SetRange_Bit(8, False);
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_StringProc;
var
  prop: integer;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    FInstance^.FTokenID := stkCssValString;
    if FInstance^.FLine[FInstance^.FRun] = #39 then
    begin
      Inc(FInstance^.FRun);
      if not Css_CustomStringProc(TSynWebCssString39, False) then
      begin
        Css_SetRange(srsCssPropValStr);
        SetRange_Bit(8, True);
      end;
    end else
    begin
      Inc(FInstance^.FRun);
      if not Css_CustomStringProc(TSynWebCssString34, False) then
      begin
        Css_SetRange(srsCssPropValStr);
        SetRange_Bit(9, True);
      end;
    end;
    if FInstance^.FTokenID = stkCssValString then
    begin
      prop := Css_GetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
        FInstance^.FTokenID := stkCssValUndef;
    end;
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_PlusProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 13) <> 0 then
      // if FInstance^.FLine[FInstance^.FRun] in ['0'..'9', '.'] then
    begin
      FInstance^.FCssMask := $F5400000;
      Css_NumberDefProc;
    end else
      FInstance^.FTokenID := stkCssError;
  end else
    if FInstance^.FOptions.FCssVersion = scvCss21 then
      Css_SymbolProc
    else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_MinusProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 13) <> 0 then
      // if FInstance^.FLine[FInstance^.FRun] in ['0'..'9', '.'] then
    begin
      FInstance^.FCssMask := $8AA00000;
      Css_NumberDefProc;
    end else
      FInstance^.FTokenID := stkCssError;
  end else
    if (Css_GetRange = srsCssRuleset) and (FInstance^.FLine[FInstance^.FRun + 1] = '-') and
      (FInstance^.FLine[FInstance^.FRun + 2] = '>') then
    begin
      Inc(FInstance^.FRun, 3);
      FInstance^.FTokenID := stkHtmlComment;
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_NumberProc;
begin
  if Css_GetRange = srsCssPropVal then
  begin
    FInstance^.FCssMask := $F5400000;
    Css_NumberDefProc;
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_NumberDefProc;
var
  prop, OldRun: integer;

  procedure CheckOther;
  begin
    if (FInstance^.FRun - FInstance^.FTokenPos = 1) and
      (FInstance^.FLine[FInstance^.FRun - 1] = '0') then
      FInstance^.FCssMask := FInstance^.FCssMask and $F5400000
    else
      FInstance^.FCssMask := FInstance^.FCssMask and $01E00000;
    if (FInstance^.FTokenPos > 1) and ((FInstance^.FLine[FInstance^.FTokenPos - 1] = '/') and
      (FInstance^.FLine[FInstance^.FTokenPos - 2] <> '*')) then
      FInstance^.FCssMask := FInstance^.FCssMask or $18000000;
  end;

begin
  while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
    Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '.' then
  begin
    FInstance^.FCssMask := FInstance^.FCssMask and $FF800000;
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
      repeat
        Inc(FInstance^.FRun);
      until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
    else
    begin
      FInstance^.FTokenID := stkCssError;
      Exit;
    end;
  end;
  if (FInstance^.FLine[FInstance^.FRun] = '%') then
  begin
    FInstance^.FCssMask := FInstance^.FCssMask and $06000000;
    Css_SetRange(srsCssPropValSpecial);
  end else
  begin
    OldRun := FInstance^.FRun;
    if Css_IdentStartProc then
    begin
      prop := Css_SpecialCheck(OldRun, FInstance^.FRun - OldRun);
      if prop <> -1 then
      begin
        FInstance^.FCssMask := FInstance^.FCssMask and TSynWeb_CssSpecialData[prop];
        Css_SetRange(srsCssPropValSpecial);
        if (FInstance^.FLine[FInstance^.FRun] = '/') and
          (FInstance^.FLine[FInstance^.FRun + 1] <> '*') then
          SetRange_Bit(8, True);
        FInstance^.FRun := OldRun;
      end else
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FRun := OldRun;
          CheckOther;
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          Exit;
        end;
    end else
      CheckOther;
  end;
  prop := Css_GetProp - 1;
  if (prop = -1) or (TSynWeb_CssPropsData[prop] and FInstance^.FCssMask = 0) then
    FInstance^.FTokenID := stkCssValUndef
  else
    FInstance^.FTokenID := stkCssValNumber;
end;

procedure TSynWebEngine.Css_IdentProc;
begin
  if Css_IdentStartProc then
  begin
    if (Html_TagCheck = stkHtmlTagName) and
      (TSynWeb_TagsData[Html_GetTag - 1] and (1 shl 30) = 0) then
      FInstance^.FTokenID := stkCssSelector
    else
      FInstance^.FTokenID := stkCssSelectorUndef;
  end else
    Css_ErrorProc;
end;

function TSynWebEngine.Css_IdentStartProc: boolean;
begin
  if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 8) = 0) or
    // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
    ((FInstance^.FLine[FInstance^.FRun] = '\') and
    (FInstance^.FLine[FInstance^.FRun + 1] in [#0..#31])) then
  begin
    Result := False;
    Exit;
  end;
  FInstance^.FStringLenClean := 0;
  repeat
    if FInstance^.FLine[FInstance^.FRun] <> '\' then
      Inc(FInstance^.FRun)
    else
      if not (FInstance^.FLine[FInstance^.FRun + 1] in [#0..#31]) then
      begin
        Inc(FInstance^.FStringLenClean);
        Inc(FInstance^.FRun, 2);
      end else
        Break;
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 9) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\', '0'..'9', '-', '_']);
  FInstance^.FStringLenClean := FInstance^.FRun - FInstance^.FTokenPos -
    FInstance^.FStringLenClean;
  Result := True;
end;

function TSynWebEngine.Css_CustomStringProc(AShl: longword; ADo: boolean): boolean;
begin
  if Css_CheckNull(ADo) then
  begin
    if not ADo then
      FInstance^.FTokenID := stkCssError;
    Result := True;
    Exit;
  end else
    if Php_CheckBegin(ADo) then
    begin
      if not ADo then
        FInstance^.FTokenID := stkCssValString;
      Result := False;
      Exit;
    end;
  Result := True;
  AShl := 1 shl AShl;
  repeat
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and AShl = 0 do
      // while not (FInstance^.FLine[FInstance^.FRun] in [#0, AChar, '\', '<']) do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #39, '"':
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkCssValString;
        Exit;
      end;
      '\':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = #0 then
        begin
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            Exit;
          end else
          begin
            FInstance^.FTokenID := stkCssValString;
            Result := False;
            Exit;
          end;
        end else
          if not Css_CheckNull(False) and not Php_CheckBegin(False) then
            Inc(FInstance^.FRun);
      end;
      else
        if Css_CheckNull(False) then
        begin
          FInstance^.FTokenID := stkCssError;
          Exit;
        end else
          if Php_CheckBegin(False) then
          begin
            FInstance^.FTokenID := stkCssValString;
            Result := False;
            Exit;
          end else
            Inc(FInstance^.FRun);
    end;
  until False;
end;

function TSynWebEngine.Css_NotWhitespace: boolean;
begin
  Result := False;
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 11) <> 0 then
    // if FInstance^.FLine[FInstance^.FRun] in [#0..#32, '/'] then
    fCss_ProcTable[FInstance^.FLine[FInstance^.FRun]]
  else
    Result := True;
end;

procedure TSynWebEngine.Css_SymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkCssSymbol;
end;

procedure TSynWebEngine.Css_ErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkCssError;
end;

procedure TSynWebEngine.Css_RangeRulesetProc;
begin
  if GetRange_Bit(8) then
  begin
    SetRange_Bit(8, False);
    Css_IdentStartProc;
    FInstance^.FTokenID := stkCssSelectorId;
  end else
    if GetRange_Bit(9) then
    begin
      SetRange_Bit(9, False);
      Css_IdentStartProc;
      FInstance^.FTokenID := stkCssSelectorClass;
    end else
      fCss_ProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.Css_RangeSelectorAttribProc;

  procedure DoError;
  begin
    Css_SetRange(srsCssRuleset);
    fCss_ProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

  procedure DoEndAttrib;
  begin
    Css_SymbolProc;
    Css_SetRange(srsCssRuleset);
  end;

begin
  case GetRange_Int(3, 8) of
    0:
      if Css_NotWhitespace then
        if Css_IdentStartProc then
        begin
          FInstance^.FTokenID := stkCssVal;
          SetRange_Int(3, 8, 1);
        end else
          DoError;
    1:
      if Css_NotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          '=':
          begin
            Css_SymbolProc;
            SetRange_Int(3, 8, 2);
          end;
          '|', '~':
          begin
            SetRange_Int(3, 8, 2);
            if FInstance^.FLine[FInstance^.FRun + 1] = '=' then
            begin
              Inc(FInstance^.FRun, 2);
              FInstance^.FTokenID := stkCssSymbol;
            end else
              Css_ErrorProc;
          end;
          ']':
            DoEndAttrib;
          else
            DoError;
        end;
    2:
      if Css_NotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          #39:
          begin
            Inc(FInstance^.FRun);
            if Css_CustomStringProc(TSynWebCssString39, False) then
              SetRange_Int(3, 8, 5)
            else
              SetRange_Int(3, 8, 3);
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if Css_CustomStringProc(TSynWebCssString34, False) then
              SetRange_Int(3, 8, 5)
            else
              SetRange_Int(3, 8, 4);
          end;
          else
            if Css_IdentStartProc then
            begin
              FInstance^.FTokenID := stkCssValString;
              SetRange_Int(3, 8, 5);
            end else
              DoError;
        end;
    3:
      if Css_CustomStringProc(TSynWebCssString39) then
        SetRange_Int(3, 8, 5);
    4:
      if Css_CustomStringProc(TSynWebCssString34) then
        SetRange_Int(3, 8, 5);
    5:
      if Css_NotWhitespace then
        if FInstance^.FLine[FInstance^.FRun] = ']' then
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
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']);
    prop := Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
    if (prop = -1) or (TSynWeb_CssSpecialData[prop] and
      (1 shl (15 - Longword(FInstance^.FOptions.FCssVersion))) = 0) then
    begin
      FInstance^.FTokenID := stkCssError;
      Css_SetRange(srsCssRuleset);
    end else
      if (prop <> Css_SpecialID_Lang) then
      begin
        FInstance^.FTokenID := stkCssSpecial;
        Css_SetRange(srsCssRuleset);
      end else
        if (FInstance^.FLine[FInstance^.FRun] = '(') then
        begin
          FInstance^.FTokenID := stkCssSpecial;
          SetRange_Bit(10, True);
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          Css_SetRange(srsCssRuleset);
        end;
  end else
    if not GetRange_Bit(9) then
    begin
      Css_SymbolProc;
      SetRange_Bit(9, True);
    end else
      if Css_NotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          ',':
            if GetRange_Bit(8) then
            begin
              SetRange_Bit(8, False);
              Css_SymbolProc;
            end else
              Css_ErrorProc;
          ')':
          begin
            if GetRange_Bit(8) then
              Css_SymbolProc
            else
              Css_ErrorProc;
            Css_SetRange(srsCssRuleset);
          end;
          else
            if Css_IdentStartProc then
              if GetRange_Bit(8) then
                FInstance^.FTokenID := stkCssError
              else
              begin
                FInstance^.FTokenID := stkCssVal;
                SetRange_Bit(8, True);
              end else
            begin
              Css_SetRange(srsCssRuleset);
              fCss_ProcTable[FInstance^.FLine[FInstance^.FRun]];
              FInstance^.FTokenID := stkCssError;
            end;
        end;
end;

procedure TSynWebEngine.Css_RangeAtKeywordProc;
var
  prop: integer;

  procedure DoError;
  begin
    Css_SetRange(srsCssRuleset);
    fCss_ProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

  procedure AtImport;

    procedure AtImport_Medium(ASimple: boolean);
    begin
      if Css_NotWhitespace then
        if not ASimple and (FInstance^.FLine[FInstance^.FRun] = ';') then
        begin
          Css_SymbolProc;
          Css_SetRange(srsCssRuleset);
        end else
          if Css_IdentStartProc then
          begin
            prop := Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
              FInstance^.FTokenPos);
            if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
              FInstance^.FTokenID := stkCssValUndef
            else
              FInstance^.FTokenID := stkCssVal;
            SetRange_Int(4, 4, 9);
          end else
            DoError;
    end;

  begin
    case GetRange_Int(4, 4) of
      0:
        if Css_NotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            #39:
            begin
              Inc(FInstance^.FRun);
              if Css_CustomStringProc(TSynWebCssString39, False) then
                SetRange_Int(4, 4, 8)
              else
                SetRange_Int(4, 4, 1);
            end;
            '"':
            begin
              Inc(FInstance^.FRun);
              if Css_CustomStringProc(TSynWebCssString34, False) then
                SetRange_Int(4, 4, 8)
              else
                SetRange_Int(4, 4, 2);
            end;
            else
              if not Css_IdentStartProc then
                DoError
              else
                if (Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
                  FInstance^.FTokenPos) = Css_SpecialID_Url) and
                  (FInstance^.FLine[FInstance^.FRun] = '(') then
                begin
                  FInstance^.FTokenID := stkCssVal;
                  SetRange_Int(4, 4, 3);
                end else
                begin
                  FInstance^.FTokenID := stkCssValUndef;
                  SetRange_Int(4, 4, 8);
                end;
          end;
      1:
        if Css_CustomStringProc(TSynWebCssString39) then
          SetRange_Int(4, 4, 8);
      2:
        if Css_CustomStringProc(TSynWebCssString34) then
          SetRange_Int(4, 4, 8);
      3:
      begin
        Css_SymbolProc;
        SetRange_Int(4, 4, 4);
      end;
      4:
        case FInstance^.FLine[FInstance^.FRun] of
          #39:
          begin
            Inc(FInstance^.FRun);
            if Css_CustomStringProc(TSynWebCssString39, False) then
              SetRange_Int(4, 4, 7)
            else
              SetRange_Int(4, 4, 5);
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if Css_CustomStringProc(TSynWebCssString34, False) then
              SetRange_Int(4, 4, 7)
            else
              SetRange_Int(4, 4, 6);
          end;
          #0..#32:
            Css_SpaceProc;
          else
            if (FInstance^.FLine[FInstance^.FRun] = '/') and
              (FInstance^.FLine[FInstance^.FRun + 1] = '*') then
              Css_SlashProc
            else
            begin
              if Css_CheckNull or Php_CheckBegin then
                Exit;
              repeat
                while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
                  (1 shl 14) = 0 do
                  // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
                  Inc(FInstance^.FRun);
                case FInstance^.FLine[FInstance^.FRun] of
                  '\':
                  begin
                    Inc(FInstance^.FRun);
                    if FInstance^.FLine[FInstance^.FRun] <> #0 then
                      Inc(FInstance^.FRun)
                    else
                      Break;
                  end;
                  '<':
                    if Css_CheckNull(False) or Php_CheckBegin(False) then
                      Break
                    else
                      Inc(FInstance^.FRun);
                  else
                    Break;
                end;
              until False;
              FInstance^.FTokenID := stkCssValString;
              SetRange_Int(4, 4, 7);
            end;
        end;
      5:
        if Css_CustomStringProc(TSynWebCssString39) then
          SetRange_Int(4, 4, 7);
      6:
        if Css_CustomStringProc(TSynWebCssString34) then
          SetRange_Int(4, 4, 7);
      7:
        if Css_NotWhitespace then
          if FInstance^.FLine[FInstance^.FRun] = ')' then
          begin
            Css_SymbolProc;
            SetRange_Int(4, 4, 8);
          end else
            DoError;
      8:
        AtImport_Medium(False);
      9:
        if Css_NotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            ';':
            begin
              Css_SymbolProc;
              Css_SetRange(srsCssRuleset);
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
        case FInstance^.FLine[FInstance^.FRun] of
          ',':
            Css_SymbolProc;
          '{':
          begin
            Css_SymbolProc;
            SetRange_Bit(11, True);
            Css_SetRange(srsCssRuleset);
          end;
          else
            DoError;
        end;
      end else
        if Css_IdentStartProc then
        begin
          prop := Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
            FInstance^.FTokenPos);
          if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
            FInstance^.FTokenID := stkCssValUndef
          else
            FInstance^.FTokenID := stkCssVal;
          SetRange_Bit(7, True);
        end else
          DoError;
  end;

  procedure AtPage;
  var
    prop: integer;

    procedure AtPage_Declaration;
    begin
      SetRange_Int(11, 0, 0);
      Css_SymbolProc;
      Css_SetRange(srsCssProp);
    end;

  begin
    case GetRange_Int(2, 6) of
      0:
        if Css_NotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            '{':
              AtPage_Declaration;
            ':':
              if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and
                (1 shl 8) = 0) or
                // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
                ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
                (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
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
        prop := Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
        if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 11) = 0) then
          FInstance^.FTokenID := stkCssError
        else
          FInstance^.FTokenID := stkCssSpecial;
        SetRange_Int(2, 6, 2);
      end;
      2:
        if Css_NotWhitespace then
          if FInstance^.FLine[FInstance^.FRun] = '{' then
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
          case FInstance^.FLine[FInstance^.FRun] of
            #39:
            begin
              Inc(FInstance^.FRun);
              if Css_CustomStringProc(TSynWebCssString39, False) then
                SetRange_Int(2, 6, 3)
              else
                SetRange_Bit(6, True);
            end;
            '"':
            begin
              Inc(FInstance^.FRun);
              if Css_CustomStringProc(TSynWebCssString34, False) then
                SetRange_Int(2, 6, 3)
              else
                SetRange_Bit(7, True);
            end;
            else
              DoError;
          end;
      1:
        if Css_CustomStringProc(TSynWebCssString39) then
          SetRange_Int(2, 6, 3);
      2:
        if Css_CustomStringProc(TSynWebCssString34) then
          SetRange_Int(2, 6, 3);
      3:
        if Css_NotWhitespace then
          if FInstance^.FLine[FInstance^.FRun] = ';' then
          begin
            Css_SymbolProc;
            Css_SetRange(srsCssRuleset);
          end else
            DoError;
    end;
  end;

begin
  if not GetRange_Bit(10) then
  begin
    SetRange_Bit(10, True);
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']);
    if GetRange_Bit(11) then
    begin
      FInstance^.FTokenID := stkCssError;
      Css_SetRange(srsCssRuleset);
    end else
      case Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos) of
        Css_SpecialID_Import:
        begin
          SetRange_Int(2, 8, 0);
          FInstance^.FTokenID := stkCssSpecial;
        end;
        Css_SpecialID_Media:
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            Css_SetRange(srsCssRuleset);
          end else
          begin
            SetRange_Int(2, 8, 1);
            FInstance^.FTokenID := stkCssSpecial;
          end;
        Css_SpecialID_Page:
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            Css_SetRange(srsCssRuleset);
          end else
          begin
            SetRange_Int(2, 8, 2);
            FInstance^.FTokenID := stkCssSpecial;
          end;
        Css_SpecialID_Charset:
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            Css_SetRange(srsCssRuleset);
          end else
          begin
            SetRange_Int(2, 8, 3);
            FInstance^.FTokenID := stkCssSpecial;
          end;
        else
          FInstance^.FTokenID := stkCssError;
          Css_SetRange(srsCssRuleset);
      end;
  end else
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
      case FInstance^.FLine[FInstance^.FRun] of
        '}':
        begin
          Css_ErrorProc;
          Css_SetRange(srsCssRuleset);
        end;
        ':':
        begin
          Css_SymbolProc;
          Css_SetRange(srsCssPropVal);
          SetRange_Bit(8, False);
        end;
        else
          Css_ErrorProc;
      end;
  end else
    if Css_NotWhitespace then
      if Css_IdentStartProc then
      begin
        FInstance^.FTokenID := Css_PropCheck;
        SetRange_Bit(8, True);
      end else
      begin
        Css_SetProp(0);
        case FInstance^.FLine[FInstance^.FRun] of
          '}':
          begin
            Css_SetRange(srsCssRuleset);
            Css_SymbolProc;
            Exit;
          end;
          ':':
            Css_SetRange(srsCssPropVal);
        end;
        Css_ErrorProc;
      end;
end;

procedure TSynWebEngine.Css_RangePropValProc;
begin
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 12) <> 0 then
    // if FInstance^.FLine[FInstance^.FRun] in [#0..#32, '/', '#', '!', ';', '}', '+', '-', '0'..'9', '.', ',', '"', #39, '<'] then
    fCss_ProcTable[FInstance^.FLine[FInstance^.FRun]]
  else
    if Css_IdentStartProc then
    begin
      FInstance^.FTokenID := Css_ValCheck;
      if TSynWeb_CssValsData[FInstance^.FToken_LastID][Longword(FInstance^.FOptions.FCssVersion)]
        [3] and (1 shl 31) <> 0 then
        if FInstance^.FLine[FInstance^.FRun] = '(' then
        begin
          SetRange_Int(3, 8, 0);
          case FInstance^.FToken_LastID of
            Css_ValID_Rgb:
              Css_SetRange(srsCssPropValRgb);
            Css_ValID_Url:
              Css_SetRange(srsCssPropValUrl);
            Css_ValID_Rect:
              Css_SetRange(srsCssPropValRect);
            else
              Css_SetRange(srsCssPropValFunc);
          end;
        end else
          FInstance^.FTokenID := stkCssValUndef;
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_RangePropValStrProc;
var
  prop: integer;
begin
  if GetRange_Bit(8) then
  begin
    if Css_CustomStringProc(TSynWebCssString39) then
    begin
      Css_SetRange(srsCssPropVal);
      SetRange_Bit(8, False);
    end;
  end else
    if Css_CustomStringProc(TSynWebCssString34) then
    begin
      Css_SetRange(srsCssPropVal);
      SetRange_Bit(9, False);
    end;
  if FInstance^.FTokenID = stkCssValString then
  begin
    prop := Css_GetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
      FInstance^.FTokenID := stkCssValUndef;
  end;
end;

procedure TSynWebEngine.Css_RangePropValRgbProc;

  procedure NumberProc;
  begin
    if GetRange_Bit(8) then
      FInstance^.FTokenID := stkCssError
    else
      FInstance^.FTokenID := stkCssValNumber;
    SetRange_Bit(8, True);
    if FInstance^.FLine[FInstance^.FRun] = '+' then
      if FInstance^.FLine[FInstance^.FRun + 1] in ['0'..'9', '.'] then
        Inc(FInstance^.FRun)
      else
      begin
        Css_ErrorProc;
        Exit;
      end;
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
      begin
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']);
        if FInstance^.FLine[FInstance^.FRun] = '%' then
          Exit;
      end;
      FInstance^.FTokenID := stkCssError;
    end;
  end;

begin
  if GetRange_Bit(10) then
  begin
    if Css_NotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
        ',':
          if GetRange_Bit(8) then
          begin
            SetRange_Bit(8, False);
            Css_SymbolProc;
          end else
            Css_ErrorProc;
        '0'..'9', '.', '+':
          NumberProc;
        '%':
          if (FInstance^.FRun > 0) and (FInstance^.FLine[FInstance^.FRun - 1] in
            ['0'..'9']) then
            Css_SymbolProc
          else
            Css_ErrorProc;
        ';':
        begin
          Css_ErrorProc;
          Css_SetRange(srsCssProp);
          SetRange_Int(3, 8, 0);
        end;
        '}':
        begin
          Css_ErrorProc;
          Css_SetRange(srsCssRuleset);
        end;
        ')':
        begin
          if GetRange_Bit(8) then
            Css_SymbolProc
          else
            Css_ErrorProc;
          Css_SetRange(srsCssPropVal);
          SetRange_Int(3, 8, 0);
        end;
        else
          Css_SetRange(srsCssPropVal);
          SetRange_Int(3, 8, 0);
          Css_RangePropValProc;
          FInstance^.FTokenID := stkCssError;
      end;
  end else
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
          case FInstance^.FLine[FInstance^.FRun] of
            #39:
            begin
              Inc(FInstance^.FRun);
              if not Css_CustomStringProc(TSynWebCssString39, False) then
                SetRange_Bit(8, True);
            end;
            '"':
            begin
              Inc(FInstance^.FRun);
              if not Css_CustomStringProc(TSynWebCssString34, False) then
                SetRange_Bit(9, True);
            end;
            ',':
              Css_SymbolProc;
            ';':
            begin
              Css_ErrorProc;
              Css_SetRange(srsCssProp);
              SetRange_Int(3, 8, 0);
            end;
            '}':
            begin
              Css_ErrorProc;
              Css_SetRange(srsCssRuleset);
            end;
            ')':
            begin
              Css_SymbolProc;
              Css_SetRange(srsCssPropVal);
              SetRange_Int(3, 8, 0);
            end;
            else
              if Css_IdentStartProc then
                FInstance^.FTokenID := stkCssVal
              else
                Css_ErrorProc;
          end;
      1:
        if Css_CustomStringProc(TSynWebCssString39) then
          SetRange_Bit(8, False);
      2:
        if Css_CustomStringProc(TSynWebCssString34) then
          SetRange_Bit(9, False);
    end else
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end;
end;

procedure TSynWebEngine.Css_RangePropValSpecialProc;
var
  prop: integer;
begin
  if FInstance^.FLine[FInstance^.FRun] = '%' then
    Css_SymbolProc
  else
    if (FInstance^.FRun > 0) and (FInstance^.FLine[FInstance^.FRun - 1] = '#') then
    begin
      Inc(FInstance^.FRun, 3);
      if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0) and
        // if (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) and
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 10) <> 0) and
        // if (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9']) and
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 10) <> 0) then
        // if (FInstance^.FLine[FInstance^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9']) then
        Inc(FInstance^.FRun, 3);
      prop := Css_GetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 18) = 0) then
        FInstance^.FTokenID := stkCssValUndef
      else
        FInstance^.FTokenID := stkCssValNumber;
    end else
    begin
      Css_IdentStartProc;
      FInstance^.FTokenID := stkCssSymbol;
    end;
  Css_SetRange(srsCssPropVal);
end;

procedure TSynWebEngine.Css_RangePropValImportantProc;

  procedure DoSymbol;
  begin
    if GetRange_Bit(8) then
    begin
      SetRange_Bit(8, False);
      Css_SymbolProc;
    end else
      Css_ErrorProc;
  end;

begin
  if Css_NotWhitespace then
    case FInstance^.FLine[FInstance^.FRun] of
      ';':
      begin
        DoSymbol;
        Css_SetRange(srsCssProp);
      end;
      '}':
      begin
        DoSymbol;
        Css_SetRange(srsCssRuleset);
      end;
      else
        if Css_IdentStartProc then
        begin
          if GetRange_Bit(8) then
            FInstance^.FTokenID := stkCssError
          else
          begin
            Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
            if FInstance^.FToken_LastID = Css_SpecialID_Important then
              FInstance^.FTokenID := stkCssSpecial
            else
              FInstance^.FTokenID := stkCssError;
            SetRange_Bit(8, True);
          end;
        end else
          Css_ErrorProc;
    end;
end;

procedure TSynWebEngine.Css_RangePropValUrlProc;
begin
  if GetRange_Bit(10) then
    case GetRange_Int(2, 8) of
      0:
        case FInstance^.FLine[FInstance^.FRun] of
          #39:
          begin
            Inc(FInstance^.FRun);
            if Css_CustomStringProc(TSynWebCssString39, False) then
              SetRange_Int(2, 8, 3)
            else
              SetRange_Bit(8, True);
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if Css_CustomStringProc(TSynWebCssString34, False) then
              SetRange_Int(2, 8, 3)
            else
              SetRange_Bit(9, True);
          end;
          #1..#32:
            Css_SpaceProc;
          ';':
          begin
            Css_ErrorProc;
            Css_SetRange(srsCssProp);
            SetRange_Int(3, 8, 0);
          end;
          '}':
          begin
            Css_ErrorProc;
            Css_SetRange(srsCssRuleset);
          end;
          ')':
          begin
            Css_ErrorProc;
            Css_SetRange(srsCssPropVal);
            SetRange_Int(3, 8, 0);
          end;
          else
            if (FInstance^.FLine[FInstance^.FRun] = '/') and
              (FInstance^.FLine[FInstance^.FRun + 1] = '*') then
              Css_SlashProc
            else
            begin
              if Css_CheckNull or Php_CheckBegin then
                Exit;
              repeat
                while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
                  (1 shl 14) = 0 do
                  // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
                  Inc(FInstance^.FRun);
                case FInstance^.FLine[FInstance^.FRun] of
                  '\':
                  begin
                    Inc(FInstance^.FRun);
                    if FInstance^.FLine[FInstance^.FRun] <> #0 then
                      Inc(FInstance^.FRun)
                    else
                      Break;
                  end;
                  '<':
                    if Css_CheckNull(False) or Php_CheckBegin(False) then
                      Break
                    else
                      Inc(FInstance^.FRun);
                  else
                    Break;
                end;
              until False;
              FInstance^.FTokenID := stkCssValString;
              SetRange_Int(2, 8, 3);
            end;
        end;
      1:
        if Css_CustomStringProc(TSynWebCssString39) then
          SetRange_Int(2, 8, 3);
      2:
        if Css_CustomStringProc(TSynWebCssString34) then
          SetRange_Int(2, 8, 3);
      3:
        if FInstance^.FLine[FInstance^.FRun] = ')' then
        begin
          Css_SymbolProc;
          SetRange_Int(3, 8, 0);
          Css_SetRange(srsCssPropVal);
        end else
          if Css_NotWhitespace then
          begin
            SetRange_Int(3, 8, 0);
            Css_SetRange(srsCssPropVal);
            Css_RangePropValProc;
            FInstance^.FTokenID := stkCssError;
          end;
    end else
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
        FInstance^.FTokenID := stkCssError
      else
        if (FInstance^.FRun - FInstance^.FTokenPos = 1) and
          (FInstance^.FLine[FInstance^.FRun - 1] = '0') then
          FInstance^.FTokenID := stkCssValNumber
        else
          FInstance^.FTokenID := stkCssValUndef;
      SetRange_Bit(8, True);
    end;

  begin
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
      begin
        FInstance^.FTokenID := stkCssError;
        Exit;
      end;
    end;
    OldRun := FInstance^.FRun;
    if Css_IdentStartProc then
    begin
      prop := Css_SpecialCheck(OldRun, FInstance^.FRun - OldRun);
      if prop <> -1 then
      begin
        FInstance^.FRun := OldRun;
        SetRange_Bit(9, True);
        if (TSynWeb_CssSpecialData[prop] and (1 shl 28) = 0) or GetRange_Bit(8) then
          FInstance^.FTokenID := stkCssError
        else
          FInstance^.FTokenID := stkCssValNumber;
      end else
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FRun := OldRun;
          CheckOther;
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          Exit;
        end;
    end else
      CheckOther;
  end;

begin
  if not GetRange_Bit(10) then
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end else
    if GetRange_Bit(9) then
    begin
      Css_IdentStartProc;
      if GetRange_Bit(8) then
        FInstance^.FTokenID := stkCssError
      else
        FInstance^.FTokenID := stkCssSymbol;
      SetRange_Bit(9, False);
      SetRange_Bit(8, True);
    end else
      if Css_NotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          ',':
            if GetRange_Bit(8) then
            begin
              SetRange_Bit(8, False);
              Css_SymbolProc;
            end else
              Css_ErrorProc;
          '0'..'9', '.':
            Shape_LengthProc;
          ')':
          begin
            if GetRange_Bit(8) then
              Css_SymbolProc
            else
              Css_ErrorProc;
            Css_SetRange(srsCssPropVal);
            SetRange_Int(3, 8, 0);
          end;
          ';':
          begin
            Css_ErrorProc;
            Css_SetRange(srsCssProp);
            SetRange_Int(3, 8, 0);
          end;
          '}':
          begin
            Css_ErrorProc;
            Css_SetRange(srsCssRuleset);
            SetRange_Int(3, 8, 0);
          end;
          else
            if not Css_IdentStartProc then
              Css_ErrorProc
            else
            begin
              if GetRange_Bit(8) then
                FInstance^.FTokenID := stkCssError
              else
                if Css_SpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
                  FInstance^.FTokenPos) = Css_SpecialID_Auto then
                  FInstance^.FTokenID := stkCssVal
                else
                  FInstance^.FTokenID := stkCssValUndef;
              SetRange_Bit(8, True);
            end;
        end;
end;

procedure TSynWebEngine.Css_RangeCommentProc;
begin
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<']) do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '<':
        if Css_CheckNull(False) or Php_CheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
      '*':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '/' then
        begin
          Inc(FInstance^.FRun);
          SetRange_Bit(12, False);
          Break;
        end;
      end;
    end;
  until False;
  FInstance^.FTokenID := stkCssComment;
end;

function TSynWebEngine.Css_PropKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_CssProps[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLenClean then
  begin
    if FInstance^.FStringLenClean = FInstance^.FStringLen then
      for i := 1 to FInstance^.FStringLen do
      begin
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end else
      for i := 1 to FInstance^.FStringLenClean do
      begin
        if Temp^ = '\' then
          Inc(Temp);
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Css_PropCheck: TSynWebTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FToken_LastID := -1;
  if HashKey <= Css_PropMaxKeyHash then
  begin
    Result := fCss_PropIdentFuncTable[HashKey];
    if (FInstance^.FToken_LastID <> -1) and
      (TSynWeb_CssPropsData[FInstance^.FToken_LastID] and
      (1 shl Longword(FInstance^.FOptions.FCssVersion)) = 0) then
      Result := stkCssPropUndef;
  end else
    Result := stkCssPropUndef;
  Css_SetProp(FInstance^.FToken_LastID + 1);
end;

{$I SynHighlighterWeb_CssPropsFunc.inc}

function TSynWebEngine.Css_ValKeyComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_CssVals[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLenClean then
  begin
    if FInstance^.FStringLenClean = FInstance^.FStringLen then
      for i := 1 to FInstance^.FStringLen do
      begin
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end else
      for i := 1 to FInstance^.FStringLenClean do
      begin
        if Temp^ = '\' then
          Inc(Temp);
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Css_ValCheck: TSynWebTokenKind;
var
  HashKey: longword;
  prop: integer;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FToken_LastID := -1;
  if HashKey <= Css_ValMaxKeyHash then
  begin
    Result := fCss_ValIdentFuncTable[HashKey];
    if Result = stkCssVal then
    begin
      prop := Css_GetProp - 1;
      if (prop = -1) or (TSynWeb_CssValsData[FInstance^.FToken_LastID]
        [Longword(FInstance^.FOptions.FCssVersion)][prop div 32] and (1 shl (prop mod 32)) = 0) then
        Result := stkCssValUndef;
    end;
  end else
    Result := stkCssValUndef;
  if Result = stkCssValUndef then
  begin
    prop := Css_GetProp - 1;
    if (prop <> -1) and (TSynWeb_CssPropsData[prop] and (1 shl 20) <> 0) then
      Result := stkCssSymbol;
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
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
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
    FInstance^.FStringLen := ALen;
    for i := 0 to ALen - 1 do
    begin
      Inc(HashKey, TSynWebInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[AStart];
  KeyHash(FInstance^.FToIdent);
  if (HashKey > Css_SpecialMaxKeyHash) or not fCss_SpecialIdentFuncTable[HashKey] then
    FInstance^.FToken_LastID := -1;
  Result := FInstance^.FToken_LastID;
end;

{$I SynHighlighterWeb_CssSpecialFunc.inc}

// ECMAScript ------------------------------------------------------------------

procedure TSynWebEngine.Es_MakeMethodTables;
var
  c: char;
  i: integer;
  pF: PSynWebIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        fEs_ProcTable[c] := NullProc;
      #1..#32:
        fEs_ProcTable[c] := Es_SpaceProc;
      '/':
        fEs_ProcTable[c] := Es_SlashProc;
      '<':
        fEs_ProcTable[c] := Es_LowerProc;
      '=', '!':
        fEs_ProcTable[c] := Es_EqualNotProc;
      '>':
        fEs_ProcTable[c] := Es_GreaterProc;
      '&':
        fEs_ProcTable[c] := Es_AndProc;
      '+':
        fEs_ProcTable[c] := Es_PlusProc;
      '-':
        fEs_ProcTable[c] := Es_MinusProc;
      '|':
        fEs_ProcTable[c] := Es_OrProc;
      '*', '%', '^':
        fEs_ProcTable[c] := Es_MulModXorProc;
      '0'..'9':
        fEs_ProcTable[c] := Es_NumberProc;
      '"':
        fEs_ProcTable[c] := Es_String34Proc;
      #39:
        fEs_ProcTable[c] := Es_String39Proc;
      '{', '}', '[', ']', '(', ')', '.', ';', ',', '?', ':', '~':
        fEs_ProcTable[c] :=
          Es_SymbolProc;
      '$', 'a'..'z', 'A'..'Z', '_':
        fEs_ProcTable[c] := Es_IdentProc;
      else
        fEs_ProcTable[c] := Es_ErrorProc;
    end;

  fEs_RangeProcTable[srsEsDefault] := Es_RangeDefaultProc;
  fEs_RangeProcTable[srsEsComment] := Es_RangeCommentProc;
  fEs_RangeProcTable[srsEsCommentMulti] := Es_RangeCommentMultiProc;
  fEs_RangeProcTable[srsEsString34] := Es_RangeString34Proc;
  fEs_RangeProcTable[srsEsString39] := Es_RangeString39Proc;

  pF := PSynWebIdentFuncTableFunc(@fEs_IdentFuncTable);
  for I := Low(fEs_IdentFuncTable) to High(fEs_IdentFuncTable) do
  begin
    pF^ := Es_KeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_EsKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.Es_Next;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  fEs_RangeProcTable[Es_GetRange];
end;

function TSynWebEngine.Es_GetRange: TSynWebEsRangeState;
begin
  Result := TSynWebEsRangeState(GetRange_Int(2, 15));
end;

procedure TSynWebEngine.Es_SetRange(const ARange: TSynWebEsRangeState);
begin
  SetRange_Int(2, 15, Longword(ARange));
end;

function TSynWebEngine.Es_CheckNull(ADo: boolean = True): boolean;
begin
  case FInstance^.FLine[FInstance^.FRun] of
    #0:
    begin
      Result := True;
      if ADo then
        NullProc;
    end;
    '<':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '/') and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
        FInstance^.FHashTable['s']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
        FInstance^.FHashTable['c']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
        FInstance^.FHashTable['r']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
        FInstance^.FHashTable['i']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
        FInstance^.FHashTable['p']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 7]] =
        FInstance^.FHashTable['t']) and
        (TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 8]] and (1 shl 0) <> 0) and
        // (FInstance^.FLine[FInstance^.FRun+8] in [#0..#32, '>']) and
        (FInstance^.FHighlighterMode = shmHtml) then
      begin
        Result := True;
        if ADo then
        begin
          FInstance^.FTokenID := stkHtmlTag;
          SetHighlighterType(shtHtml, True, False, False);
        end;
      end else
        Result := False;
    else
      Result := False;
  end;
end;

procedure TSynWebEngine.Es_SpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkEsSpace;
end;

procedure TSynWebEngine.Es_SlashProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '*':
    begin
      Inc(FInstance^.FRun);
      Es_SetRange(srsEsCommentMulti);
      if Es_CheckNull(False) or Php_CheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        Es_RangeCommentMultiProc;
      Exit;
    end;
    '=':
      Inc(FInstance^.FRun);
    '/':
    begin
      Inc(FInstance^.FRun);
      Es_SetRange(srsEsComment);
      if Es_CheckNull(False) or Php_CheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        Es_RangeCommentProc;
      Exit;
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_LowerProc;
begin
  if Es_CheckNull or Php_CheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '=':
      Inc(FInstance^.FRun);
    '<':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
        Inc(FInstance^.FRun);
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_EqualNotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
      Inc(FInstance^.FRun);
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_GreaterProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '=':
      Inc(FInstance^.FRun);
    '>':
    begin
      Inc(FInstance^.FRun);
      case FInstance^.FLine[FInstance^.FRun] of
        '=':
          Inc(FInstance^.FRun);
        '>':
        begin
          Inc(FInstance^.FRun);
          if FInstance^.FLine[FInstance^.FRun] = '=' then
            Inc(FInstance^.FRun);
        end;
      end;
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_AndProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '&'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_PlusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '+'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_MinusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '-'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_OrProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '|'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_MulModXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_NumberProc;
begin
  FInstance^.FTokenID := stkEsError;
  if (FInstance^.FLine[FInstance^.FRun] = '0') and
    (FInstance^.FLine[FInstance^.FRun + 1] in ['x', 'X']) then
  begin
    Inc(FInstance^.FRun, 2);
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
      // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
        (1 shl 10) = 0
    // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end else
  begin
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
    if FInstance^.FLine[FInstance^.FRun] in ['e', 'E'] then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['+', '-'] then
        Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  FInstance^.FTokenID := stkEsNumber;
end;

procedure TSynWebEngine.Es_String34Proc;
begin
  Inc(FInstance^.FRun);
  if Es_CheckNull(False) then
    FInstance^.FTokenID := stkEsError
  else
  begin
    Es_SetRange(srsEsString34);
    if Php_CheckBegin(False) then
      FInstance^.FTokenID := stkEsString
    else
      Es_RangeString34Proc;
  end;
end;

procedure TSynWebEngine.Es_String39Proc;
begin
  Inc(FInstance^.FRun);
  if Es_CheckNull(False) then
    FInstance^.FTokenID := stkEsError
  else
  begin
    Es_SetRange(srsEsString39);
    if Php_CheckBegin(False) then
      FInstance^.FTokenID := stkEsString
    else
      Es_RangeString39Proc;
  end;
end;

procedure TSynWebEngine.Es_SymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.Es_IdentProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 2) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$']);
  FInstance^.FTokenID := Es_IdentCheck;
end;

procedure TSynWebEngine.Es_ErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsError;
end;

procedure TSynWebEngine.Es_RangeDefaultProc;
begin
  fEs_ProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.Es_RangeCommentProc;
begin
  if not Es_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        repeat
          Inc(FInstance^.FRun);
        until FInstance^.FLine[FInstance^.FRun] in [#0, '<'];
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkEsComment;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FInstance^.FTokenID := stkEsComment;
              Exit;
            end else
              if Es_CheckNull(False) then
              begin
                FInstance^.FTokenID := stkEsComment;
                Break;
              end else
                Inc(FInstance^.FRun);
        end;
      until False;
  Es_SetRange(srsEsDefault);
end;

procedure TSynWebEngine.Es_RangeCommentMultiProc;
begin
  if Es_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<']) do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '<':
        if Es_CheckNull(False) or Php_CheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
      '*':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '/' then
        begin
          Inc(FInstance^.FRun);
          Es_SetRange(srsEsDefault);
          Break;
        end;
      end;
    end;
  until False;
  FInstance^.FTokenID := stkEsComment;
end;

procedure TSynWebEngine.Es_RangeString34Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 3) = 0 do
          // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #34, '<', '\']) do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FInstance^.FTokenID := stkEsString;
              Exit;
            end else
              Inc(FInstance^.FRun);
          #34:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkEsString;
            Break;
          end;
          '\':
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #34 then
              Inc(FInstance^.FRun);
          end;
        end;
      until False;
  Es_SetRange(srsEsDefault);
end;

procedure TSynWebEngine.Es_RangeString39Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 4) = 0 do
          // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<', '\']) do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
          '<':
            if Php_CheckBegin(False) then
            begin
              FInstance^.FTokenID := stkEsString;
              Exit;
            end else
              Inc(FInstance^.FRun);
          #39:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkEsString;
            Break;
          end;
          '\':
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #39 then
              Inc(FInstance^.FRun);
          end;
        end;
      until False;
  Es_SetRange(srsEsDefault);
end;

function TSynWebEngine.Es_KeywordComp(const ID: integer): boolean;
var
  I: integer;
  Temp: PChar;
  aKey: string;
begin
  aKey := TSynWeb_EsKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Es_IdentCheck: TSynWebTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FToken_LastID := -1;
  if HashKey <= Es_KeywordsMaxKeyHash then
    Result := fEs_IdentFuncTable[HashKey]
  else
    Result := stkEsIdentifier;
end;

{$I SynHighlighterWeb_EsKeywordsFunc.inc}

// Php -------------------------------------------------------------------------

procedure TSynWebEngine.Php_MakeMethodTables;
var
  c: char;
  i: integer;
  pF: PSynWebIdentFuncTableFunc;
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

  fPhp_RangeProcTable[srsPhpSubProc] := Php_SubProcProc;
  fPhp_RangeProcTable[srsPhpDefault] := Php_RangeDefaultProc;
  fPhp_RangeProcTable[srsPhpComment] := Php_RangeCommentProc;
  fPhp_RangeProcTable[srsPhpString34] := Php_RangeString34Proc;
  fPhp_RangeProcTable[srsPhpString39] := Php_RangeString39Proc;
  fPhp_RangeProcTable[srsPhpStringShell] := Php_RangeStringShellProc;
  fPhp_RangeProcTable[srsPhpHeredoc] := Php_RangeHeredocProc;

  pF := PSynWebIdentFuncTableFunc(@fPhp_IdentFuncTable);
  for I := Low(fPhp_IdentFuncTable) to High(fPhp_IdentFuncTable) do
  begin
    pF^ := Php_KeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_PhpKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.Php_Next;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  if FInstance^.FLine[FInstance^.FRun] = #0 then
    NullProc
  else
    fPhp_RangeProcTable[Php_GetRange];
end;

procedure TSynWebEngine.PhpCli_Next;
begin
  FInstance^.FTokenID := stkPhpInlineText;
  FInstance^.FTokenPos := FInstance^.FRun;
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while not (FInstance^.FLine[FInstance^.FRun] in [#0, '<']) do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '<':
        if Php_CheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
    end;
  until False;
end;

function TSynWebEngine.Php_GetRange: TSynWebPhpRangeState;
begin
  if GetRange_Bit(26) then
    Result := srsPhpHeredoc
  else
    Result := TSynWebPhpRangeState(GetRange_Int(3, 23));
end;

procedure TSynWebEngine.Php_SetRange(const ARange: TSynWebPhpRangeState);
begin
  if ARange = srsPhpHeredoc then
    SetRange_Bit(26, True)
  else
  begin
    SetRange_Bit(26, False);
    SetRange_Int(3, 23, Longword(ARange));
  end;
end;

function TSynWebEngine.Php_CheckBegin(ABegin: boolean): boolean;
begin
  Result := False;
  if (FInstance^.FLine[FInstance^.FRun] = '<') and FInstance^.FOptions.FPhpEmbeded then
    case FInstance^.FLine[FInstance^.FRun + 1] of
      '?':
        if (UpCase(FInstance^.FLine[FInstance^.FRun + 2]) = 'P') and
          (UpCase(FInstance^.FLine[FInstance^.FRun + 3]) = 'H') and
          (UpCase(FInstance^.FLine[FInstance^.FRun + 4]) = 'P') and
          (FInstance^.FLine[FInstance^.FRun + 5] <= #32) then
        begin
          if ABegin then
            Php_Begin(spotPhp);
        end else
          if FInstance^.FOptions.FPhpShortOpenTag then
          begin
            if ABegin then
              Php_Begin(spotPhpShort);
          end else
            Exit;
      '%':
        if FInstance^.FOptions.FPhpAspTags then
        begin
          if ABegin then
            Php_Begin(spotASP);
        end else
          Exit;
      else
        Exit;
    end else
    Exit;
  Result := True;
end;

procedure TSynWebEngine.Php_Begin(ATagKind: TSynWebPhpOpenTag);
begin
  SetHighlighterType(
    TSynHighlighterType(Longword(FInstance^.FHighlighterType) + Longword(shtPhp_inHtml)),
    False,
    True,
    ATagKind = spotHtml);
  SetRange_Int(12, 17, 0);
  if ATagKind = spotHtml then
    Php_SetRange(srsPhpDefault)
  else
  begin
    if ATagKind = spotPhp then
      SetRange_Bit(19, True);
    Next;
  end;
end;

procedure TSynWebEngine.Php_End(AHtmlTag: boolean);
begin
  SetRange_Int(12, 17, 0);
  if FInstance^.FLine[FInstance^.FRun] = #0 then
    SetRange_Int(3, 29, Longword(FInstance^.FHighlighterType) - Longword(shtPhp_inHtml))
  else
  begin
    SetHighlighterType(
      TSynHighlighterType(Longword(FInstance^.FHighlighterType) - Longword(shtPhp_inHtml)),
      AHtmlTag,
      True, not AHtmlTag);
    if AHtmlTag then
      Next;
  end;
end;

procedure TSynWebEngine.Php_SpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkPhpSpace;
end;

procedure TSynWebEngine.Php_QuestionProc;
begin
  Inc(FInstance^.FRun);
  if (FInstance^.FLine[FInstance^.FRun] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun);
    FInstance^.FTokenID := stkHtmlTag;
    Php_End(False);
  end else
    FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_NumberProc;
begin
  if Php_CheckNumberProc then
    FInstance^.FTokenID := stkPhpNumber
  else
    FInstance^.FTokenID := stkPhpError;
end;

function TSynWebEngine.Php_CheckNumberProc: boolean;
begin
  Result := False;
  if (FInstance^.FLine[FInstance^.FRun] = '0') and
    (FInstance^.FLine[FInstance^.FRun + 1] = 'x') then
  begin
    Inc(FInstance^.FRun, 2);
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
      // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0
    // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end else
  begin
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
    if FInstance^.FLine[FInstance^.FRun] in ['e', 'E'] then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['+', '-'] then
        Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  Result := True;
end;

procedure TSynWebEngine.Php_String34Proc;
begin
  Inc(FInstance^.FRun);
  Php_SetRange(srsPhpString34);
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 30) <> 0 then
    // if FInstance^.FLine[FInstance^.FRun] in [#0, '\', '{', '$'] then
    FInstance^.FTokenID := stkPhpString
  else
    Php_RangeString34Proc;
end;

procedure TSynWebEngine.Php_String39Proc;
begin
  Inc(FInstance^.FRun);
  Php_SetRange(srsPhpString39);
  if FInstance^.FLine[FInstance^.FRun] in [#0, '\'] then
    FInstance^.FTokenID := stkPhpString
  else
    Php_RangeString39Proc;
end;

procedure TSynWebEngine.Php_StringShellProc;
begin
  Inc(FInstance^.FRun);
  Php_SetRange(srsPhpStringShell);
  if FInstance^.FLine[FInstance^.FRun] in [#0, '`'] then
    FInstance^.FTokenID := stkPhpString
  else
    Php_RangeStringShellProc;
end;

procedure TSynWebEngine.Php_AndProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '&'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_OrProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '|'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_AtSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpKeyword;
end;

procedure TSynWebEngine.Php_EqualProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
      Inc(FInstance^.FRun);
  end;
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_GreaterProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '=':
      Inc(FInstance^.FRun);
    '>':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
        Inc(FInstance^.FRun);
    end;
  end;
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_LowerProc;
var
  tmpRun: longword;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '/':
      if (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 1]] =
        FInstance^.FHashTable['s']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
        FInstance^.FHashTable['c']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
        FInstance^.FHashTable['r']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
        FInstance^.FHashTable['i']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
        FInstance^.FHashTable['p']) and
        (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
        FInstance^.FHashTable['t']) and
        (TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) <> 0) then
        // (FInstance^.FLine[FInstance^.FRun+7] in [#0..#32, '>']) then
      begin
        Dec(FInstance^.FRun);
        Php_End(True);
        Exit;
      end;
    '=':
      Inc(FInstance^.FRun);
    '<':
    begin
      Inc(FInstance^.FRun);
      case FInstance^.FLine[FInstance^.FRun] of
        '=':
          Inc(FInstance^.FRun);
        '<':
        begin
          Inc(FInstance^.FRun);
          tmpRun := FInstance^.FRun;
          while FInstance^.FLine[tmpRun] in [#1..#32] do
            Inc(tmpRun);
          if TSynWebIdentTable[FInstance^.FLine[tmpRun]] and (1 shl 28) = 0 then
            // if not (FInstance^.FLine[tmpRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF]) then
          begin
            FInstance^.FTokenID := stkPhpError;
            Exit;
          end;
          Php_SetRange(srsPhpSubProc);
          SetRange_Int(3, 20, 2);
        end;
      end;
    end;
  end;
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_PlusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['+', '='] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_MinusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['-', '=', '>'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_MulDivModXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_SlashProc;
var
  b: boolean;
begin
  case FInstance^.FLine[FInstance^.FRun + 1] of
    '/':
    begin
      Inc(FInstance^.FRun);
      Php_HashProc;
    end;
    '*':
    begin
      Inc(FInstance^.FRun, 2);
      Php_SetRange(srsPhpComment);
      b := (FInstance^.FLine[FInstance^.FRun] = '*') and
        (FInstance^.FLine[FInstance^.FRun + 1] <= #32);
      if b then
        Inc(FInstance^.FRun);
      SetRange_Bit(19, b);
      if FInstance^.FLine[FInstance^.FRun] <> #0 then
        Php_RangeCommentProc
      else
        if b then
          FInstance^.FTokenID := stkPhpDocComment
        else
          FInstance^.FTokenID := stkPhpComment;
    end;
    else
      Php_MulDivModXorProc;
  end;
end;

procedure TSynWebEngine.Php_PercentProc;
begin
  if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun, 2);
    if FInstance^.FOptions.FPhpAspTags then
    begin
      FInstance^.FTokenID := stkHtmlTag;
      Php_End(False);
    end else
      FInstance^.FTokenID := stkPhpError;
  end else
    Php_MulDivModXorProc;
end;

procedure TSynWebEngine.Php_HashProc;
begin
  FInstance^.FTokenID := stkPhpComment;
  repeat
    repeat
      Inc(FInstance^.FRun)
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 17) <> 0;
    // until FInstance^.FLine[FInstance^.FRun] in [#0, #10, #13, '%', '?'];
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Exit;
      '?':
        if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
          FInstance^.FOptions.FPhpEmbeded then
          Exit;
      '%':
        if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
          FInstance^.FOptions.FPhpAspTags and FInstance^.FOptions.FPhpEmbeded then
          Exit;
      else
        Exit;
    end;
  until False;
end;

procedure TSynWebEngine.Php_NotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
      Inc(FInstance^.FRun);
  end;
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_DotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    FInstance^.FTokenID := stkPhpSymbol;
  end else
    Php_NumberProc;
end;

procedure TSynWebEngine.Php_SymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.Php_VarProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '$' then
    Inc(FInstance^.FRun);
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    FInstance^.FTokenID := stkPhpKeyword
  else
    FInstance^.FTokenID := stkPhpError;
end;

procedure TSynWebEngine.Php_IdentProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  if (FInstance^.FTokenPos > 0) and (FInstance^.FLine[FInstance^.FTokenPos - 1] = '$') then
    FInstance^.FTokenID := stkPhpVariable
  else
    FInstance^.FTokenID := Php_IdentCheck;
end;

procedure TSynWebEngine.Php_ErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpError;
end;

function TSynWebEngine.Php_DoStringDouble(AIsHeredoc: boolean;
  ARangeChar: boolean): boolean;
var
  StringChar: char;

  procedure TryDoSpace;
  begin
    while FInstance^.FLine[FInstance^.FRun] in [#1..#32] do
      Inc(FInstance^.FRun);
  end;

  procedure DoIdent;
  begin
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  end;

  function TryDoIdent: boolean;
  begin
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
      // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    begin
      DoIdent;
      Result := True;
    end else
      Result := False;
  end;

  function DoStringSingle: boolean;
  begin
    Result := True;
    repeat
      if FInstance^.FLine[FInstance^.FRun] = '\' then
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] in [#39, '\'] then
          Inc(FInstance^.FRun);
      end;
      while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 24) = 0 do
        // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #39, '\'] do
        Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '\' then
        Continue
      else
      begin
        if FInstance^.FLine[FInstance^.FRun] <> #39 then
          Result := False;
        Exit;
      end;
    until False;
  end;

  function DoStringObject(AAllowSpaces: boolean = True): boolean;
  begin
    Inc(FInstance^.FRun, 2);
    if AAllowSpaces then
    begin
      TryDoSpace;
      Result := TryDoIdent;
      TryDoSpace;
    end else
      Result := TryDoIdent;
  end;

  function DoStringVar: boolean;
  begin
    Inc(FInstance^.FRun);
    Result := TryDoIdent;
  end;

  function DoStringVar2: boolean;
  begin
    TryDoSpace;
    Result := True;
    case FInstance^.FLine[FInstance^.FRun] of
      '-':
        if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
          ((not DoStringObject) or ((FInstance^.FLine[FInstance^.FRun] in ['[', '-']) and
          not DoStringVar2)) then
          Result := False;
      '[':
      begin
        Inc(FInstance^.FRun);
        TryDoSpace;
        case FInstance^.FLine[FInstance^.FRun] of
          '$':
            if (not DoStringVar) or ((FInstance^.FLine[FInstance^.FRun] in ['[', '-']) and
              not DoStringVar2) then
              Result := False;
          '0'..'9', '.':
            if not Php_CheckNumberProc then
              Result := False;
          #39:
          begin
            Inc(FInstance^.FRun);
            if DoStringSingle then
              Inc(FInstance^.FRun)
            else
              Result := False;
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            while not Php_DoStringDouble(False, False) and
              (FInstance^.FLine[FInstance^.FRun] <> #0) and (FInstance^.FTokenID <>
                stkPhpError) do
            ;
            if (FInstance^.FLine[FInstance^.FRun] = '"') and
              (FInstance^.FTokenID <> stkPhpError) then
            begin
              FInstance^.FTokenID := stkPhpStringSpecial;
              Inc(FInstance^.FRun);
            end else
              Result := False;
          end;
          else
            if not TryDoIdent then
              Result := False;
        end;
        TryDoSpace;
        if not Result or (FInstance^.FLine[FInstance^.FRun] <> ']') then
          Result := False
        else
        begin
          Inc(FInstance^.FRun);
          TryDoSpace;
          if (FInstance^.FLine[FInstance^.FRun] in ['[', '-']) and not DoStringVar2 then
            Result := False;
        end;
      end;
    end;
  end;

begin
  if not ARangeChar or (Php_GetRange = srsPhpString34) then
    StringChar := #34
  else
    StringChar := '`';
  Result := False;
  FInstance^.FTokenID := stkPhpStringSpecial;
  case FInstance^.FLine[FInstance^.FRun] of
    '$':
    begin
      Inc(FInstance^.FRun);
      if TryDoIdent then
      begin
        case FInstance^.FLine[FInstance^.FRun] of
          '-':
            if FInstance^.FLine[FInstance^.FRun + 1] = '>' then
              if not DoStringObject(False) then
                FInstance^.FTokenID := stkPhpError;
          '[':
          begin
            Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
              '$':
                if not DoStringVar then
                  FInstance^.FTokenID := stkPhpError;
              '0'..'9', '.':
                if not Php_CheckNumberProc then
                  FInstance^.FTokenID := stkPhpError;
              else
                if not TryDoIdent then
                  FInstance^.FTokenID := stkPhpError;
            end;
            if FInstance^.FLine[FInstance^.FRun] = ']' then
              Inc(FInstance^.FRun)
            else
              FInstance^.FTokenID := stkPhpError;
          end;
        end;
        Exit;
      end else
        if FInstance^.FLine[FInstance^.FRun] = '{' then
        begin
          Inc(FInstance^.FRun);
          if not TryDoIdent or not DoStringVar2 or
            (FInstance^.FLine[FInstance^.FRun] <> '}') then
            FInstance^.FTokenID := stkPhpError
          else
            Inc(FInstance^.FRun);
          Exit;
        end;
    end;
    '{':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '$' then
      begin
        Inc(FInstance^.FRun);
        if not TryDoIdent or not DoStringVar2 or
          (FInstance^.FLine[FInstance^.FRun] <> '}') then
          FInstance^.FTokenID := stkPhpError
        else
          Inc(FInstance^.FRun);
        Exit;
      end;
    end;
    '\':
    begin
      Inc(FInstance^.FRun);
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 18) <> 0 then
        // if FInstance^.FLine[FInstance^.FRun] in ['n', 'r', 't', '\', '$', #34, '0'..'7', 'x'] then
      begin
        Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun - 1] of
          '0'..'7':
          begin
            if FInstance^.FLine[FInstance^.FRun] in ['0'..'7'] then
            begin
              Inc(FInstance^.FRun);
              if FInstance^.FLine[FInstance^.FRun] in ['0'..'7'] then
                Inc(FInstance^.FRun);
            end;
            Exit;
          end;
          'x':
            if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
              // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
            begin
              Inc(FInstance^.FRun);
              if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
                (1 shl 10) <> 0 then
                // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
                Inc(FInstance^.FRun);
              Exit;
            end;
          else
            Exit;
        end;
      end;
    end;
  end;
  FInstance^.FTokenID := stkPhpString;
  repeat
    if StringChar = #34 then
      while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 25) = 0 do
        // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #34, '\', '{', '$'] do
        Inc(FInstance^.FRun)
    else
      while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 5) = 0 do
        // while not(FInstance^.FLine[FInstance^.FRun] in [#0, '`', '\', '{', '$'] do
        Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = StringChar then
      if AIsHeredoc then
      begin
        Inc(FInstance^.FRun);
        Continue;
      end else
        Result := True;
    Exit;
  until False;
end;

procedure TSynWebEngine.Php_SubProcProc;
var
  s: string;
  i: integer;

  procedure DoDefault;
  begin
    SetRange_Int(3, 20, 0);
    Php_SetRange(srsPhpDefault);
  end;

begin
  case GetRange_Int(3, 20) of
    0:
    begin
      Inc(FInstance^.FRun, 2);
      FInstance^.FTokenID := stkHtmlTag;
      SetRange_Int(3, 20, 1);
    end;
    1:
    begin
      DoDefault;
      if GetRange_Bit(19) then
      begin
        SetRange_Bit(19, False);
        Inc(FInstance^.FRun, 3);
        FInstance^.FTokenID := stkPhpKeyword;
      end else
        if (FInstance^.FLine[FInstance^.FRun] = '=') and (FInstance^.FOptions.FPhpShortOpenTag) then
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkPhpKeyword;
        end else
          Php_RangeDefaultProc;
    end;
    2:
    begin
      if FInstance^.FLine[FInstance^.FRun] in [#1..#32] then
      begin
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
        FInstance^.FTokenID := stkPhpSpace;
        Exit;
      end;
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
      // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
      if FInstance^.FLine[FInstance^.FRun] <> #0 then
      begin
        FInstance^.FTokenID := stkPhpError;
        Php_SetRange(srsPhpDefault);
        Exit;
      end;
      FInstance^.FTokenID := stkPhpKeyword;
      Php_SetRange(srsPhpHeredoc);
      s := GetToken;
      i := FPhpHereDocList.IndexOf(s);
      if i in [0..255] then
      begin
        SetRange_Int(8, 17, i);
        SetRange_Bit(25, True);
      end else
      begin
        SetRange_Int(8, 17, GetCRC8_String(s));
        SetRange_Bit(25, False);
      end;
    end;
  end;
end;

procedure TSynWebEngine.Php_RangeDefaultProc;
begin
  fPhp_ProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.Php_RangeCommentProc;
begin
  repeat
    if (FInstance^.FLine[FInstance^.FRun] = '*') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
    begin
      Inc(FInstance^.FRun, 2);
      Php_SetRange(srsPhpDefault);
      Break;
    end;
    Inc(FInstance^.FRun);
  until FInstance^.FLine[FInstance^.FRun] = #0;
  if GetRange_Bit(19) then
    FInstance^.FTokenID := stkPhpDocComment
  else
    FInstance^.FTokenID := stkPhpComment;
end;

procedure TSynWebEngine.Php_RangeString34Proc;
begin
  if Php_DoStringDouble then
  begin
    Inc(FInstance^.FRun);
    Php_SetRange(srsPhpDefault);
  end;
end;

procedure TSynWebEngine.Php_RangeString39Proc;
begin
  if FInstance^.FLine[FInstance^.FRun] = '\' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] in [#39, '\'] then
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkPhpStringSpecial;
      Exit;
    end;
  end;
  FInstance^.FTokenID := stkPhpString;
  repeat
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 24) = 0 do
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #39, '\'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] <> #39 then
      Exit
    else
    begin
      Inc(FInstance^.FRun);
      Php_SetRange(srsPhpDefault);
      Exit;
    end;
  until False;
  Es_SetRange(srsEsDefault);
end;

procedure TSynWebEngine.Php_RangeStringShellProc;
begin
  if Php_DoStringDouble then
  begin
    Inc(FInstance^.FRun);
    Php_SetRange(srsPhpDefault);
  end;
end;

procedure TSynWebEngine.Php_RangeHeredocProc;
var
  OldRun: longint;
  s: string;
begin
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  begin
    OldRun := FInstance^.FRun;
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
    if ((FInstance^.FLine[FInstance^.FRun] = ';') and
      (FInstance^.FLine[FInstance^.FRun + 1] = #0)) or
      (FInstance^.FLine[FInstance^.FRun] = #0) then
    begin
      s := GetToken;
      if (GetRange_Bit(25) and (s = FPhpHereDocList[GetRange_Int(8, 17)])) or
        (not GetRange_Bit(25) and (GetRange_Int(8, 17) = GetCRC8_String(GetToken))) then
      begin
        FInstance^.FTokenID := stkPhpKeyword;
        Php_SetRange(srsPhpDefault);
        Exit;
      end;
    end;
    FInstance^.FRun := OldRun;
  end;
  if Php_DoStringDouble(True) then
  begin
    Inc(FInstance^.FRun);
    Php_SetRange(srsPhpDefault);
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
    (1 shl Longword(FInstance^.FOptions.FPhpVersion)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_PhpKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Php_ConstComp: boolean;
var
  I: integer;
  Temp: PChar;
begin
  Temp := FInstance^.FToIdent;
  for i := 1 to FInstance^.FStringLen do
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
    (1 shl Longword(FInstance^.FOptions.FPhpVersion)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_PhpKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FToken_LastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.Php_IdentCheck: TSynWebTokenKind;
var
  HashKey: longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebInsensitiveHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FToken_LastID := -1;
  if HashKey <= Php_KeywordsMaxKeyHash then
    Result := fPhp_IdentFuncTable[HashKey]
  else
    Result := stkPhpIdentifier;
  if Result = stkPhpIdentifier then
    if Php_ConstComp then
      Result := stkPhpConst;
end;

{$I SynHighlighterWeb_PhpKeywordsFunc.inc}

// Other -----------------------------------------------------------------------

procedure TSynWebEngine.AddAttribute(AAttrib: TSynHighlighterAttributes);
begin
  FAttributes.AddObject(AAttrib.Name, AAttrib);
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
  for i := FAttributes.Count - 1 downto 0 do
  begin
    Attri := TSynHighlighterAttributes(FAttributes.Objects[i]);
    if Attri <> nil then
    begin
      Attri.OnChange := AEvent;
      Attri.InternalSaveDefaultValues;
    end;
  end;
end;

procedure TSynWebEngine.DefHighlightChange(Sender: TObject);
var
  i: integer;
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
    Result := TCrc8Table[Result xor Byte(AString[i])];
end;

function TSynWebEngine.GetRange_Bit(ABit: longword): boolean;
begin
  Result := FInstance^.FRange and (1 shl ABit) <> 0;
end;

procedure TSynWebEngine.SetRange_Bit(ABit: longword; AVal: boolean);
begin
  if AVal then
    FInstance^.FRange := FInstance^.FRange or (1 shl ABit)
  else
    FInstance^.FRange := FInstance^.FRange and not (1 shl ABit);
end;

function TSynWebEngine.GetRange_Int(ALen, APos: longword): longword;
begin
  Result := (FInstance^.FRange shr APos) and not ($FFFFFFFF shl ALen);
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
  FInstance^.FRange := (FInstance^.FRange and i) or ((AVal shl APos) and not i);
end;

procedure TSynWebEngine.NullProc;
begin
  FInstance^.FTokenID := stkNull;
end;

procedure TSynWebEngine.NextSetHighlighterType;
begin
  SetHighlighterType(FInstance^.FNextHighlighterType, FInstance^.FNextClearBits,
    False, FInstance^.FNextUseNextAH);
  Next;
  FInstance^.FHighlighterSW := True;
end;

procedure TSynWebEngine.SetHighlighterType(const AHighlighterType: TSynHighlighterType;
  AClearBits: boolean; ASetAtNextToken: boolean; AUseNextAH: boolean);
begin
  if ASetAtNextToken then
  begin
    FInstance^.FNextUseNextAH := AUseNextAH;
    FInstance^.FNextHighlighterType := AHighlighterType;
    FInstance^.FNextClearBits := AClearBits;
    FInstance^.FNextProcTable := NextSetHighlighterType;
  end else
  begin
    FInstance^.FUseNextAH := AUseNextAH;
    FInstance^.FHighlighterSW := True;
    FInstance^.FPrevHighlighterType := FInstance^.FHighlighterType;
    FInstance^.FHighlighterType := AHighlighterType;
    SetRange_Int(3, 29, Longword(AHighlighterType));
    SetupHighlighterType(AClearBits);
  end;
end;

procedure TSynWebEngine.SetupHighlighterType(AClearBits: boolean);
begin
  case FInstance^.FHighlighterType of
    shtHtml:
      if FInstance^.FHighlighterMode = shmPhpCli then
      begin
        if AClearBits then
          SetRange_Int(17, 0, 0);
        FInstance^.FSYN_ATTR_COMMENT := fPhp_InlineTextAttri;
        FInstance^.FSYN_ATTR_STRING := fPhp_InlineTextAttri;
        FInstance^.FSYN_ATTR_WHITESPACE := fPhp_InlineTextAttri;
        FInstance^.FNextProcTable := PhpCli_Next;
      end else
      begin
        if AClearBits then
          SetRange_Int(17, 0, 0);
        FInstance^.FSYN_ATTR_COMMENT := fHtml_CommentAttri;
        FInstance^.FSYN_ATTR_STRING := fHtml_TagKeyValueQuotedAttri;
        FInstance^.FSYN_ATTR_WHITESPACE := fHtml_WhitespaceAttri;
        FInstance^.FNextProcTable := Html_Next;
      end;
    shtCss:
    begin
      if AClearBits then
        SetRange_Int(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := fCss_CommentAttri;
      FInstance^.FSYN_ATTR_STRING := fCss_ValStringAttri;
      Css_UpdateBg;
      FInstance^.FNextProcTable := Css_Next;
    end;
    shtEs:
    begin
      if AClearBits then
        SetRange_Int(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := fEs_CommentAttri;
      FInstance^.FSYN_ATTR_STRING := fEs_StringAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := fEs_WhitespaceAttri;
      FInstance^.FNextProcTable := Es_Next;
    end;
    else
      if AClearBits then
        SetRange_Int(12, 17, 0);
      FInstance^.FSYN_ATTR_COMMENT := fPhp_CommentAttri;
      FInstance^.FSYN_ATTR_STRING := fPhp_StringAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := fPhp_WhitespaceAttri;
      FInstance^.FNextProcTable := Php_Next;
  end;
end;

procedure TSynWebEngine.SetLine(NewValue: string; LineNumber: integer);
{$IFDEF SYNWEB_FIXNULL}
var
  i:Integer;
{$ENDIF}
begin
  FInstance^.FLineRef := NewValue;
{$IFDEF SYNWEB_FIXNULL}
  for i:=1 to Length(FInstance^.FLineRef) do
    if FInstance^.FLineRef[i]=#0 then
      FInstance^.FLineRef[i]:=' ';
{$ENDIF}
  FInstance^.FLine := PChar(FInstance^.FLineRef);
  FInstance^.FRun := 0;
  FInstance^.FLineNumber := LineNumber;
  FInstance^.FHighlighterType := TSynHighlighterType(GetRange_Int(3, 29));
  FInstance^.FPrevHighlighterType := FInstance^.FHighlighterType;
  FInstance^.FHighlighterSW := False;
  SetupHighlighterType;
  FInstance^.FNextProcTable;
end;

procedure TSynWebEngine.Next;
begin
  FInstance^.FHighlighterSW := False;
  FInstance^.FNextProcTable;
end;

function TSynWebEngine.GetToken: string;
var
  Len: longint;
begin
  Len := FInstance^.FRun - FInstance^.FTokenPos;
  SetString(Result, (FInstance^.FLine + FInstance^.FTokenPos), Len);
end;

constructor TSynWebEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TSynWebEngineOptions.Create(@FEngineOptions);
  FOptions.FOnChange := DefHighlightChange;
  FNotifyList := TList.Create;
  FPhpHereDocList := TStringList.Create;
  with FPhpHereDocList do
  begin
    Add('EOF');
    Add('eof');
    Add('EOT');
    Add('eot');
    Add('EOL');
    Add('eol');
    Add('Html');
    Add('html');
    Add('CONTENT');
    Add('content');
    Add('HEREDOC');
    Add('heredoc');
    Add('OUT');
    Add('out');
    Add('STRING');
    Add('string');
    CaseSensitive := True;
    Sorted := True;
  end;

  FAttributes := TStringList.Create;
  FAttributes.Duplicates := dupError;
  FAttributes.Sorted := True;

  // Html
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

  FTokenAttributeTable[stkHtmlSpace] := fHtml_WhitespaceAttri;
  FTokenAttributeTable[stkHtmlComment] := fHtml_CommentAttri;
  FTokenAttributeTable[stkHtmlText] := fHtml_TextAttri;
  FTokenAttributeTable[stkHtmlEscape] := fHtml_EscapeAmpsAttri;
  FTokenAttributeTable[stkHtmlSymbol] := fHtml_SymbolAttri;
  FTokenAttributeTable[stkHtmlTag] := fHtml_TagAttri;
  FTokenAttributeTable[stkHtmlTagName] := fHtml_TagNameAttri;
  FTokenAttributeTable[stkHtmlTagNameUndef] := fHtml_TagNameUndefAttri;
  FTokenAttributeTable[stkHtmlTagKey] := fHtml_TagKeyAttri;
  FTokenAttributeTable[stkHtmlTagKeyUndef] := fHtml_TagKeyUndefAttri;
  FTokenAttributeTable[stkHtmlTagKeyValue] := fHtml_TagKeyValueAttri;
  FTokenAttributeTable[stkHtmlTagKeyValueQuoted] := fHtml_TagKeyValueQuotedAttri;
  FTokenAttributeTable[stkHtmlError] := fHtml_ErrorAttri;

  // Css
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

  FTokenAttributeTable[stkCssSpace] := fCss_WhitespaceAttri;
  FTokenAttributeTable[stkCssSelector] := fCss_SelectorAttri;
  FTokenAttributeTable[stkCssSelectorUndef] := fCss_SelectorUndefAttri;
  FTokenAttributeTable[stkCssSelectorClass] := fCss_SelectorClassAmpsAttri;
  FTokenAttributeTable[stkCssSelectorId] := fCss_SelectorIdAttri;
  FTokenAttributeTable[stkCssSpecial] := fCss_SpecialAttri;
  FTokenAttributeTable[stkCssComment] := fCss_CommentAttri;
  FTokenAttributeTable[stkCssProp] := fCss_PropAttri;
  FTokenAttributeTable[stkCssPropUndef] := fCss_PropUndefAttri;
  FTokenAttributeTable[stkCssVal] := fCss_ValAttri;
  FTokenAttributeTable[stkCssValUndef] := fCss_ValUndefAttri;
  FTokenAttributeTable[stkCssValString] := fCss_ValStringAttri;
  FTokenAttributeTable[stkCssValNumber] := fCss_ValNumberAttri;
  FTokenAttributeTable[stkCssSymbol] := fCss_SymbolAttri;
  FTokenAttributeTable[stkCssError] := fCss_ErrorAttri;

  // ECMAScript
  Es_MakeMethodTables;

  fEs_WhitespaceAttri := TSynHighlighterAttributes.Create('Es: Whitespace');
  AddAttribute(fEs_WhitespaceAttri);
  fEs_IdentifierAttri := TSynHighlighterAttributes.Create('Es: Identifier');
  AddAttribute(fEs_IdentifierAttri);
  fEs_KeyAttri := TSynHighlighterAttributes.Create('Es: Key');
  AddAttribute(fEs_KeyAttri);
  fEs_CommentAttri := TSynHighlighterAttributes.Create('Es: Comment');
  AddAttribute(fEs_CommentAttri);
  fEs_StringAttri := TSynHighlighterAttributes.Create('Es: String');
  AddAttribute(fEs_StringAttri);
  fEs_NumberAttri := TSynHighlighterAttributes.Create('Es: Number');
  AddAttribute(fEs_NumberAttri);
  fEs_SymbolAttri := TSynHighlighterAttributes.Create('Es: Symbol');
  AddAttribute(fEs_SymbolAttri);
  fEs_ErrorAttri := TSynHighlighterAttributes.Create('Es: Error');
  AddAttribute(fEs_ErrorAttri);

  FTokenAttributeTable[stkEsSpace] := fEs_WhitespaceAttri;
  FTokenAttributeTable[stkEsIdentifier] := fEs_IdentifierAttri;
  FTokenAttributeTable[stkEsKeyword] := fEs_KeyAttri;
  FTokenAttributeTable[stkEsComment] := fEs_CommentAttri;
  FTokenAttributeTable[stkEsString] := fEs_StringAttri;
  FTokenAttributeTable[stkEsNumber] := fEs_NumberAttri;
  FTokenAttributeTable[stkEsSymbol] := fEs_SymbolAttri;
  FTokenAttributeTable[stkEsError] := fEs_ErrorAttri;

  // Php
  Php_MakeMethodTables;

  fPhp_WhitespaceAttri := TSynHighlighterAttributes.Create('Php: Whitespace');
  AddAttribute(fPhp_WhitespaceAttri);
  fPhp_InlineTextAttri := TSynHighlighterAttributes.Create('PhpCli: Inline text');
  AddAttribute(fPhp_InlineTextAttri);
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
  fPhp_DocCommentAttri := TSynHighlighterAttributes.Create('Php: DocComment');
  AddAttribute(fPhp_DocCommentAttri);
  fPhp_SymbolAttri := TSynHighlighterAttributes.Create('Php: Symbol');
  AddAttribute(fPhp_SymbolAttri);
  fPhp_NumberAttri := TSynHighlighterAttributes.Create('Php: Number');
  AddAttribute(fPhp_NumberAttri);
  fPhp_ErrorAttri := TSynHighlighterAttributes.Create('Php: Error');
  AddAttribute(fPhp_ErrorAttri);

  FTokenAttributeTable[stkPhpSpace] := fHtml_WhitespaceAttri;
  FTokenAttributeTable[stkPhpIdentifier] := fPhp_IdentifierAttri;
  FTokenAttributeTable[stkPhpKeyword] := fPhp_KeyAttri;
  FTokenAttributeTable[stkPhpFunction] := fPhp_FunctionAttri;
  FTokenAttributeTable[stkPhpVariable] := fPhp_VariableAttri;
  FTokenAttributeTable[stkPhpConst] := fPhp_ConstAttri;
  FTokenAttributeTable[stkPhpString] := fPhp_StringAttri;
  FTokenAttributeTable[stkPhpStringSpecial] := fPhp_StringSpecialAttri;
  FTokenAttributeTable[stkPhpComment] := fPhp_CommentAttri;
  FTokenAttributeTable[stkPhpDocComment] := fPhp_DocCommentAttri;
  FTokenAttributeTable[stkPhpSymbol] := fPhp_SymbolAttri;
  FTokenAttributeTable[stkPhpNumber] := fPhp_NumberAttri;
  FTokenAttributeTable[stkPhpError] := fPhp_ErrorAttri;

  // PhpCli
  FTokenAttributeTable[stkPhpInlineText] := fPhp_InlineTextAttri;

  // Global
  FInactiveAttri := TSynHighlighterAttributes.Create('Global: Inactive');
  with FInactiveAttri do
  begin
    Background := clNone;
    Foreground := clInactiveCaption;
    Style := [];
  end;
  AddAttribute(FInactiveAttri);

  FTokenAttributeTable[stkNull] := nil;
  SetAttributesOnChange(DefHighlightChange);
end;

destructor TSynWebEngine.Destroy;
var
  i: integer;
begin
  for i := FAttributes.Count - 1 downto 0 do
    TSynHighlighterAttributes(FAttributes.Objects[i]).Free;
  FAttributes.Clear;
  for i := 0 to FNotifyList.Count - 1 do
    TSynWebBase(FNotifyList[i]).Engine := nil;
  FNotifyList.Free;
  FPhpHereDocList.Free;
  inherited Destroy;
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynWebHtmlSyn);
  RegisterPlaceableHighlighter(TSynWebPhpCliSyn);
  RegisterPlaceableHighlighter(TSynWebCssSyn);
  RegisterPlaceableHighlighter(TSynWebEsSyn);
{$ENDIF}

end.

