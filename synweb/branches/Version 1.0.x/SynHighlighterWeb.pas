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
- PHP: Doesn't support multi-line encapsuled strings in string :
  eg. "somestring {$a["some array{$b['key'].... <- only single line encapsuled values
-------------------------------------------------------------------------------}
{
@abstract(Provides an web-files (Multi Html/Css/ECAMScript/Php) highlighter for SynEdit
@author(FlatDev <flatdev@mail.ru>)
@created(2005-05-21)
@lastmod(2006-02-10)
The TSynWebSyn unit provides SynEdit with an Multi Html/Css/ECAMScript/Php highlighter.
}

// SYNWEB_FIXNULL - try fix lines containing #0 character (#0 goes into #32)
{.$DEFINE SYNWEB_FIXNULL}

{$IFNDEF QSYNHIGHLIGHTERINI}
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
    Run: Longint;
    fRange: Longword;
    fLine: PChar;
    fLineRef: String;
    fLineNumber: Integer;
    fToken_LastID:Integer;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fStringLen, fStringLenClean: Integer;
    fToIdent: PChar;
    fHashTable: THashTable;
    fNextClearBits: Boolean;
    fNextUseNextAH: Boolean;
    fUseNextAH: Boolean;
    fHighlighterType, fPrevHighlighterType, fNextHighlighterType: TSynHighlighterType;
    fHighlighterSW: Boolean;
    fActiveHighlighters: TSynHighlighterTypes;
    fActiveHighlighter: Boolean;
    fHighlighterMode: TSynHighlighterMode;
    fHtml_Version:THtmlVersion;
    fCss_Version:TCssVersion;
    fCss_Mask:Longword;
    fPhp_Version:TPhpVersion;
    fPhpShortOpenTag:Boolean;
    fPhpAspTags:Boolean;
    fNextProcTable: TProcTableProc;

  end;

  TSynWebBase = class(TSynCustomHighlighter)
  private
    FConfig: TSynWebConfig;
    FEngine: TSynWebEngine;
    procedure SetEngine(const Value: TSynWebEngine);

  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenLen: Integer;
    function GetTokenPos: Integer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    procedure Next; override;

  published
    property Engine:TSynWebEngine read FEngine write SetEngine;
  end;

  TSynWebSynPHPMulti = class(TSynWebBase)
  private
  public
    procedure ResetRange; override;
  end;

  TSynWebSynPHP = class(TSynWebBase)
  private
  public
    procedure ResetRange; override;
  end;

  TSynWebSynHtml = class(TSynWebBase)
  private
  public
    procedure ResetRange; override;
  end;

  TSynWebSynCSS = class(TSynWebBase)
  private
  public
    procedure ResetRange; override;
  end;

  TSynWebSynES = class(TSynWebBase)
  private
  public
    procedure ResetRange; override;
  end;

  TSynWebEngine = class
  private                                                                           
    fConfig: PSynWebConfig;
    // Global ------------------------------------------------------------------
    fInactiveAttri: TSynHighlighterAttributes;
    fTokenAttributeTable: TTokenAttributeTable;
    fSYN_ATTR_COMMENT: TSynHighlighterAttributes;
    fSYN_ATTR_STRING: TSynHighlighterAttributes;
    fSYN_ATTR_WHITESPACE: TSynHighlighterAttributes;

    // HTML --------------------------------------------------------------------
    fHtml_TagIdentFuncTable: array[0..Html_TagMaxKeyHash] of
      TIdentFuncTableFunc;
    fHtml_AttrIdentFuncTable: array[0..Html_AttrMaxKeyHash] of
      TIdentFuncTableFunc;
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
    fCss_PropIdentFuncTable: array[0..Css_PropMaxKeyHash] of
      TIdentFuncTableFunc;
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
    fPhp_IdentFuncTable: array[0..Php_KeywordsMaxKeyHash] of
      TIdentFuncTableFunc;
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
    function Html_GetRange:THtmlRangeState;
    procedure Html_SetRange(const ARange:THtmlRangeState);
    function Html_GetTag:Integer;
    procedure Html_SetTag(const ATag:Integer);
    procedure SetHtml_Version(const Value: THtmlVersion);
    function Html_CheckNull(ADo:Boolean=True):Boolean;

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

    function Html_TagKeyComp(const ID:Integer): Boolean;
    function Html_TagCheck: TtkTokenKind;
    {$I SynHighlighterWeb_TagsFuncList.inc}

    function Html_AttrKeyComp(const ID:Integer): Boolean;
    function Html_AttrCheck: TtkTokenKind;
    {$I SynHighlighterWeb_AttrsFuncList.inc}

    function Html_SpecialKeyComp(const ID:Integer): Boolean;
    function Html_SpecialCheck(AStart, ALen:Integer): Integer;
    {$I SynHighlighterWeb_SpecialFuncList.inc}

    // CSS ---------------------------------------------------------------------
    procedure Css_MakeMethodTables;
    procedure Css_NextBg;
    procedure Css_Next;
    procedure Css_UpdateBg;
    function Css_GetRange:TCssRangeState;
    procedure Css_SetRange(const ARange:TCssRangeState);
    function Css_GetProp:Integer;
    procedure Css_SetProp(const AProp:Integer);
    procedure SetCss_Version(const Value: TCssVersion);
    function Css_CheckNull(ADo:Boolean=True):Boolean;

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
    function Css_IdentStartProc:Boolean;
    function Css_CustomStringProc(AShl:Longword; ADo:Boolean=True):Boolean;
    function Css_NotWhitespace:Boolean;
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

    function Css_PropKeyComp(const ID:Integer): Boolean;
    function Css_PropCheck: TtkTokenKind;
    {$I SynHighlighterWeb_CssPropsFuncList.inc}

    function Css_ValKeyComp(const ID:Integer): Boolean;
    function Css_ValCheck: TtkTokenKind;
    {$I SynHighlighterWeb_CssValsFuncList.inc}

    function Css_SpecialKeyComp(const ID:Integer): Boolean;
    function Css_SpecialCheck(AStart, ALen:Integer): Integer;
    {$I SynHighlighterWeb_CssSpecialFuncList.inc}

    // ECMAScript --------------------------------------------------------------
    procedure ES_MakeMethodTables;
    procedure ES_Next;
    function ES_GetRange:TESRangeState;
    procedure ES_SetRange(const ARange:TESRangeState);
    function ES_CheckNull(ADo:Boolean=True):Boolean;

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

    function ES_KeywordComp(const ID:Integer): Boolean;

    function ES_IdentCheck: TtkTokenKind;
    {$I SynHighlighterWeb_ESKeywordsFuncList.inc}

    // PHP ---------------------------------------------------------------------
    procedure Php_MakeMethodTables;
    procedure Php_Next;
    function Php_GetRange:TPhpRangeState;
    procedure Php_SetRange(const ARange:TPhpRangeState);
    function Php_GetOpenTag:TPhpOpenTag;
    procedure Php_SetOpenTag(APhpOpenTag:TPhpOpenTag);
    procedure SetPhp_Version(const Value: TPhpVersion);
    procedure SetPhpAspTags(const Value: Boolean);
    procedure SetPhpShortOpenTag(const Value: Boolean);

    function Php_CheckBegin(ABegin:Boolean=True):Boolean;
    procedure Php_Begin(ATagKind:TPhpOpenTag);
    procedure Php_End;

    procedure Php_SpaceProc;
    procedure Php_QuestionProc;
    procedure Php_NumberProc;
    function Php_CheckNumberProc:Boolean;
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
    function Php_DoStringDouble(AIsHeredoc:Boolean=False):Boolean;

    procedure Php_RangeTagProc;
    procedure Php_RangeDefaultProc;
    procedure Php_RangeCommentProc;
    procedure Php_RangeString34Proc;
    procedure Php_RangeString39Proc;
    procedure Php_RangeStringShellProc;
    procedure Php_RangeHeredocProc;

    function Php_KeywordComp(const ID:Integer): Boolean;
    function Php_ConstComp: Boolean;
    function Php_FunctionComp(const ID:Integer): Boolean;

    function Php_IdentCheck: TtkTokenKind;
    {$I SynHighlighterWeb_PhpKeywordsFuncList.inc}

    // Other -------------------------------------------------------------------
    function GetCRC8_String(AString:String):Byte;
    procedure NullProc;
    function GetRange_Bit(ABit:Longword):Boolean;
    procedure SetRange_Bit(ABit:Longword; AVal:Boolean);
    function GetRange_Int(ALen, APos:Longword):Longword;
    procedure SetRange_Int(ALen, APos, AVal:Longword);
    procedure NextSetHighligterType;
    procedure SetHighligterType(const AHighlighterType:TSynHighlighterType;
      AClearBits:Boolean; ASetAtNextToken: Boolean; AUseNextAH: Boolean);
    procedure SetupHighligterType(AClearBits:Boolean=False);
    procedure SetHighlighterMode(const Value: TSynHighlighterMode);
    procedure SetActiveHighlighter(const Value: Boolean);
    procedure SetupActiveHighlighter;
    function GetEol: Boolean;
    procedure SetLine(NewValue: string; LineNumber:Integer);
    procedure Next;
    function UpdateActiveHighlighter(ARange: Pointer; ALine: String; ACaretX,
      ACaretY: Integer):Boolean;
    function GetCurrentActiveHighlighters:TSynHighlighterTypes;

    //todo: reorganize
    property ActiveHighlighter:Boolean read FActiveHighlighter write SetActiveHighlighter;
    property HighlighterMode:TSynHighlighterMode read FHighlighterMode write
      SetHighlighterMode;
    property HtmlVersion:THtmlVersion read fHtml_Version write SetHtml_Version;
    property CssVersion:TCssVersion read fCss_Version write SetCss_Version;
    property PhpVersion:TPhpVersion read fPhp_Version write SetPhp_Version;
    property PhpShortOpenTag:Boolean read fPhpShortOpenTag write SetPhpShortOpenTag;
    property PhpAspTags:Boolean read fPhpAspTags write SetPhpAspTags;
  public
    constructor Create;
  published
    // Global
    property InactiveAttri: TSynHighlighterAttributes read fInactiveAttri write
      fInactiveAttri;

    // HTML
    property HtmlWhitespaceAttri: TSynHighlighterAttributes read
      fHtml_WhitespaceAttri write fHtml_WhitespaceAttri;
    property HtmlCommentAttri: TSynHighlighterAttributes read
      fHtml_CommentAttri write fHtml_CommentAttri;
    property HtmlTextAttri: TSynHighlighterAttributes read fHtml_TextAttri
      write fHtml_TextAttri;
    property HtmlEscapeAttri: TSynHighlighterAttributes read
      fHtml_EscapeAmpsAttri write fHtml_EscapeAmpsAttri;
    property HtmlSymbolAttri: TSynHighlighterAttributes read fHtml_SymbolAttri
      write fHtml_SymbolAttri;
    property HtmlTagAttri: TSynHighlighterAttributes read fHtml_TagAttri write
      fHtml_TagAttri;
    property HtmlTagNameAttri: TSynHighlighterAttributes read
      fHtml_TagNameAttri write fHtml_TagNameAttri;
    property HtmlTagNameUndefAttri: TSynHighlighterAttributes read
      fHtml_TagNameUndefAttri write fHtml_TagNameUndefAttri;
    property HtmlTagKeyAttri: TSynHighlighterAttributes read fHtml_TagKeyAttri
      write fHtml_TagKeyAttri;
    property HtmlTagKeyUndefAttri: TSynHighlighterAttributes read
      fHtml_TagKeyUndefAttri write fHtml_TagKeyUndefAttri;
    property HtmlTagKeyValueAttri: TSynHighlighterAttributes read
      fHtml_TagKeyValueAttri write fHtml_TagKeyValueAttri;
    property HtmlTagKeyValueQuotedAttri: TSynHighlighterAttributes read
      fHtml_TagKeyValueQuotedAttri write fHtml_TagKeyValueQuotedAttri;
    property HtmlErrorAttri: TSynHighlighterAttributes read fHtml_ErrorAttri
      write fHtml_ErrorAttri;

    // CSS
    property CssWhitespaceAttri: TSynHighlighterAttributes read
      fCss_WhitespaceAttri write fCss_WhitespaceAttri;
    property CssRulesetWhitespaceAttri: TSynHighlighterAttributes read
      fCss_RulesetWhitespaceAttri write fCss_RulesetWhitespaceAttri;
    property CssSelectorAttri: TSynHighlighterAttributes read
      fCss_SelectorAttri write fCss_SelectorAttri;
    property CssSelectorUndefAttri: TSynHighlighterAttributes read
      fCss_SelectorUndefAttri write fCss_SelectorUndefAttri;
    property CssSelectorClassAttri: TSynHighlighterAttributes read
      fCss_SelectorClassAmpsAttri write fCss_SelectorClassAmpsAttri;
    property CssSelectorIdAttri: TSynHighlighterAttributes read
      fCss_SelectorIdAttri write fCss_SelectorIdAttri;
    property CssSpecialAttri: TSynHighlighterAttributes read fCss_SpecialAttri
      write fCss_SpecialAttri;
    property CssCommentAttri: TSynHighlighterAttributes read fCss_CommentAttri
      write fCss_CommentAttri;
    property CssPropAttri: TSynHighlighterAttributes read fCss_PropAttri write
      fCss_PropAttri;
    property CssPropUndefAttri: TSynHighlighterAttributes read
      fCss_PropUndefAttri write fCss_PropUndefAttri;
    property CssValAttri: TSynHighlighterAttributes read fCss_ValAttri write
      fCss_ValAttri;
    property CssValUndefAttri: TSynHighlighterAttributes read
      fCss_ValUndefAttri write fCss_ValUndefAttri;
    property CssValStringAttri: TSynHighlighterAttributes read
      fCss_ValStringAttri write fCss_ValStringAttri;
    property CssValNumberAttri: TSynHighlighterAttributes read
      fCss_ValNumberAttri write fCss_ValNumberAttri;
    property CssSymbolAttri: TSynHighlighterAttributes read fCss_SymbolAttri
      write fCss_SymbolAttri;
    property CssErrorAttri: TSynHighlighterAttributes read fCss_ErrorAttri
      write fCss_ErrorAttri;

    // ECMAScript
    property ESWhitespaceAttri: TSynHighlighterAttributes read fES_WhitespaceAttri write fES_WhitespaceAttri;
    property ESIdentifierAttri: TSynHighlighterAttributes read fES_IdentifierAttri write fES_IdentifierAttri;
    property ESKeyAttri: TSynHighlighterAttributes read fES_KeyAttri write fES_KeyAttri;
    property ESCommentAttri: TSynHighlighterAttributes read fES_CommentAttri write fES_CommentAttri;
    property ESStringAttri: TSynHighlighterAttributes read fES_StringAttri write fES_StringAttri;
    property ESNumberAttri: TSynHighlighterAttributes read fES_NumberAttri write fES_NumberAttri;
    property ESSymbolAttri: TSynHighlighterAttributes read fES_SymbolAttri write fES_SymbolAttri;
    property ESErrorAttri: TSynHighlighterAttributes read fES_ErrorAttri write fES_ErrorAttri;

    // PHP
    property PhpWhitespaceAttri: TSynHighlighterAttributes read fPhp_WhitespaceAttri write fPhp_WhitespaceAttri;
    property PhpIdentifierAttri: TSynHighlighterAttributes read fPhp_IdentifierAttri write fPhp_IdentifierAttri;
    property PhpKeyAttri: TSynHighlighterAttributes read fPhp_KeyAttri write fPhp_KeyAttri;
    property PhpFunctionAttri: TSynHighlighterAttributes read fPhp_FunctionAttri write fPhp_FunctionAttri;
    property PhpVariableAttri: TSynHighlighterAttributes read fPhp_VariableAttri write fPhp_VariableAttri;
    property PhpConstAttri: TSynHighlighterAttributes read fPhp_ConstAttri write fPhp_ConstAttri;
    property PhpStringAttri: TSynHighlighterAttributes read fPhp_StringAttri write fPhp_StringAttri;
    property PhpStringSpecialAttri: TSynHighlighterAttributes read fPhp_StringSpecialAttri write fPhp_StringSpecialAttri;
    property PhpCommentAttri: TSynHighlighterAttributes read fPhp_CommentAttri write fPhp_CommentAttri;
    property PhpSymbolAttri: TSynHighlighterAttributes read fPhp_SymbolAttri write fPhp_SymbolAttri;
    property PhpNumberAttri: TSynHighlighterAttributes read fPhp_NumberAttri write fPhp_NumberAttri;
    property PhpErrorAttri: TSynHighlighterAttributes read fPhp_ErrorAttri write fPhp_ErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst, StrUtils;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

constructor TSynWebEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // HTML
  Html_MakeMethodTables;

  fHtml_WhitespaceAttri:=TSynHighlighterAttributes.Create('Html: Whitespace');
  AddAttribute(fHtml_WhitespaceAttri);
  fHtml_CommentAttri:=TSynHighlighterAttributes.Create('Html: Comment');
  AddAttribute(fHtml_CommentAttri);
  fHtml_TextAttri:=TSynHighlighterAttributes.Create('Html: Text');
  AddAttribute(fHtml_TextAttri);
  fHtml_EscapeAmpsAttri:=TSynHighlighterAttributes.Create('Html: Escaped amps');
  AddAttribute(fHtml_EscapeAmpsAttri);
  fHtml_SymbolAttri:=TSynHighlighterAttributes.Create('Html: Symbol');
  AddAttribute(fHtml_SymbolAttri);
  fHtml_TagAttri:=TSynHighlighterAttributes.Create('Html: Tag');
  AddAttribute(fHtml_TagAttri);
  fHtml_TagNameAttri:=TSynHighlighterAttributes.Create('Html: Tag name');
  AddAttribute(fHtml_TagNameAttri);
  fHtml_TagNameUndefAttri:=TSynHighlighterAttributes.Create('Html: Undefined tag name');
  AddAttribute(fHtml_TagNameUndefAttri);
  fHtml_TagKeyAttri:=TSynHighlighterAttributes.Create('Html: Key');
  AddAttribute(fHtml_TagKeyAttri);
  fHtml_TagKeyUndefAttri:=TSynHighlighterAttributes.Create('Html: Undefined key');
  AddAttribute(fHtml_TagKeyUndefAttri);
  fHtml_TagKeyValueAttri:=TSynHighlighterAttributes.Create('Html: Value');
  AddAttribute(fHtml_TagKeyValueAttri);
  fHtml_TagKeyValueQuotedAttri:=TSynHighlighterAttributes.Create('Html: Quoted value');
  AddAttribute(fHtml_TagKeyValueQuotedAttri);
  fHtml_ErrorAttri:=TSynHighlighterAttributes.Create('Html: Error');
  AddAttribute(fHtml_ErrorAttri);

  fTokenAttributeTable[tkHtmlSpace]:=fHtml_WhitespaceAttri;
  fTokenAttributeTable[tkHtmlComment]:=fHtml_CommentAttri;
  fTokenAttributeTable[tkHtmlText]:=fHtml_TextAttri;
  fTokenAttributeTable[tkHtmlEscape]:=fHtml_EscapeAmpsAttri;
  fTokenAttributeTable[tkHtmlSymbol]:=fHtml_SymbolAttri;
  fTokenAttributeTable[tkHtmlTag]:=fHtml_TagAttri;
  fTokenAttributeTable[tkHtmlTagName]:=fHtml_TagNameAttri;
  fTokenAttributeTable[tkHtmlTagNameUndef]:=fHtml_TagNameUndefAttri;
  fTokenAttributeTable[tkHtmlTagKey]:=fHtml_TagKeyAttri;
  fTokenAttributeTable[tkHtmlTagKeyUndef]:=fHtml_TagKeyUndefAttri;
  fTokenAttributeTable[tkHtmlTagKeyValue]:=fHtml_TagKeyValueAttri;
  fTokenAttributeTable[tkHtmlTagKeyValueQuoted]:=fHtml_TagKeyValueQuotedAttri;
  fTokenAttributeTable[tkHtmlError]:=fHtml_ErrorAttri;

  // CSS
  Css_MakeMethodTables;

  fCss_WhitespaceAttri:=TSynHighlighterAttributes.Create('Css: Whitespace');
  AddAttribute(fCss_WhitespaceAttri);
  fCss_RulesetWhitespaceAttri:=TSynHighlighterAttributes.Create('Css: Ruleset whitespace');
  AddAttribute(fCss_RulesetWhitespaceAttri);
  fCss_SelectorAttri:=TSynHighlighterAttributes.Create('Css: Selector');
  AddAttribute(fCss_SelectorAttri);
  fCss_SelectorUndefAttri:=TSynHighlighterAttributes.Create('Css: Undefined selector');
  AddAttribute(fCss_SelectorUndefAttri);
  fCss_SelectorClassAmpsAttri:=TSynHighlighterAttributes.Create('Css: Class selector');
  AddAttribute(fCss_SelectorClassAmpsAttri);
  fCss_SelectorIdAttri:=TSynHighlighterAttributes.Create('Css: Id selector');
  AddAttribute(fCss_SelectorIdAttri);
  fCss_SpecialAttri:=TSynHighlighterAttributes.Create('Css: Special');
  AddAttribute(fCss_SpecialAttri);
  fCss_CommentAttri:=TSynHighlighterAttributes.Create('Css: Comment');
  AddAttribute(fCss_CommentAttri);
  fCss_PropAttri:=TSynHighlighterAttributes.Create('Css: Property');
  AddAttribute(fCss_PropAttri);
  fCss_PropUndefAttri:=TSynHighlighterAttributes.Create('Css: Undefined property');
  AddAttribute(fCss_PropUndefAttri);
  fCss_ValAttri:=TSynHighlighterAttributes.Create('Css: Value');
  AddAttribute(fCss_ValAttri);
  fCss_ValUndefAttri:=TSynHighlighterAttributes.Create('Css: Undefined value');
  AddAttribute(fCss_ValUndefAttri);
  fCss_ValStringAttri:=TSynHighlighterAttributes.Create('Css: String value');
  AddAttribute(fCss_ValStringAttri);
  fCss_ValNumberAttri:=TSynHighlighterAttributes.Create('Css: Number value');
  AddAttribute(fCss_ValNumberAttri);
  fCss_SymbolAttri:=TSynHighlighterAttributes.Create('Css: Symbol');
  AddAttribute(fCss_SymbolAttri);
  fCss_ErrorAttri:=TSynHighlighterAttributes.Create('Css: Error');
  AddAttribute(fCss_ErrorAttri);

  fTokenAttributeTable[tkCssSpace]:=fCss_WhitespaceAttri;
  fTokenAttributeTable[tkCssSelector]:=fCss_SelectorAttri;
  fTokenAttributeTable[tkCssSelectorUndef]:=fCss_SelectorUndefAttri;
  fTokenAttributeTable[tkCssSelectorClass]:=fCss_SelectorClassAmpsAttri;
  fTokenAttributeTable[tkCssSelectorId]:=fCss_SelectorIdAttri;
  fTokenAttributeTable[tkCssSpecial]:=fCss_SpecialAttri;
  fTokenAttributeTable[tkCssComment]:=fCss_CommentAttri;
  fTokenAttributeTable[tkCssProp]:=fCss_PropAttri;
  fTokenAttributeTable[tkCssPropUndef]:=fCss_PropUndefAttri;
  fTokenAttributeTable[tkCssVal]:=fCss_ValAttri;
  fTokenAttributeTable[tkCssValUndef]:=fCss_ValUndefAttri;
  fTokenAttributeTable[tkCssValString]:=fCss_ValStringAttri;
  fTokenAttributeTable[tkCssValNumber]:=fCss_ValNumberAttri;
  fTokenAttributeTable[tkCssSymbol]:=fCss_SymbolAttri;
  fTokenAttributeTable[tkCssError]:=fCss_ErrorAttri;

  // ECMAScript
  ES_MakeMethodTables;

  fES_WhitespaceAttri:=TSynHighlighterAttributes.Create('ES: Whitespace');
  AddAttribute(fES_WhitespaceAttri);
  fES_IdentifierAttri:=TSynHighlighterAttributes.Create('ES: Identifier');
  AddAttribute(fES_IdentifierAttri);
  fES_KeyAttri:=TSynHighlighterAttributes.Create('ES: Key');
  AddAttribute(fES_KeyAttri);
  fES_CommentAttri:=TSynHighlighterAttributes.Create('ES: Comment');
  AddAttribute(fES_CommentAttri);
  fES_StringAttri:=TSynHighlighterAttributes.Create('ES: String');
  AddAttribute(fES_StringAttri);
  fES_NumberAttri:=TSynHighlighterAttributes.Create('ES: Number');
  AddAttribute(fES_NumberAttri);
  fES_SymbolAttri:=TSynHighlighterAttributes.Create('ES: Symbol');
  AddAttribute(fES_SymbolAttri);
  fES_ErrorAttri:=TSynHighlighterAttributes.Create('ES: Error');
  AddAttribute(fES_ErrorAttri);

  fTokenAttributeTable[tkESSpace]:=fES_WhitespaceAttri;
  fTokenAttributeTable[tkESIdentifier]:=fES_IdentifierAttri;
  fTokenAttributeTable[tkESKeyword]:=fES_KeyAttri;
  fTokenAttributeTable[tkESComment]:=fES_CommentAttri;
  fTokenAttributeTable[tkESString]:=fES_StringAttri;
  fTokenAttributeTable[tkESNumber]:=fES_NumberAttri;
  fTokenAttributeTable[tkESSymbol]:=fES_SymbolAttri;
  fTokenAttributeTable[tkESError]:=fES_ErrorAttri;

  // PHP
  Php_MakeMethodTables;

  fPhp_WhitespaceAttri:=TSynHighlighterAttributes.Create('Php: Whitespace');
  AddAttribute(fPhp_WhitespaceAttri);
  fPhp_IdentifierAttri:=TSynHighlighterAttributes.Create('Php: Identifier');
  AddAttribute(fPhp_IdentifierAttri);
  fPhp_KeyAttri:=TSynHighlighterAttributes.Create('Php: Keyword');
  AddAttribute(fPhp_KeyAttri);
  fPhp_FunctionAttri:=TSynHighlighterAttributes.Create('Php: Function');
  AddAttribute(fPhp_FunctionAttri);
  fPhp_VariableAttri:=TSynHighlighterAttributes.Create('Php: Variable');
  AddAttribute(fPhp_VariableAttri);
  fPhp_ConstAttri:=TSynHighlighterAttributes.Create('Php: Constant');
  AddAttribute(fPhp_ConstAttri);
  fPhp_StringAttri:=TSynHighlighterAttributes.Create('Php: String');
  AddAttribute(fPhp_StringAttri);
  fPhp_StringSpecialAttri:=TSynHighlighterAttributes.Create('Php: String special');
  AddAttribute(fPhp_StringSpecialAttri);
  fPhp_CommentAttri:=TSynHighlighterAttributes.Create('Php: Comment');
  AddAttribute(fPhp_CommentAttri);
  fPhp_SymbolAttri:=TSynHighlighterAttributes.Create('Php: Symbol');
  AddAttribute(fPhp_SymbolAttri);
  fPhp_NumberAttri:=TSynHighlighterAttributes.Create('Php: Number');
  AddAttribute(fPhp_NumberAttri);
  fPhp_ErrorAttri:=TSynHighlighterAttributes.Create('Php: Error');
  AddAttribute(fPhp_ErrorAttri);

  fTokenAttributeTable[tkPhpSpace]:=fHtml_WhitespaceAttri;
  fTokenAttributeTable[tkPhpIdentifier]:=fPhp_IdentifierAttri;
  fTokenAttributeTable[tkPhpKeyword]:=fPhp_KeyAttri;
  fTokenAttributeTable[tkPhpFunction]:=fPhp_FunctionAttri;
  fTokenAttributeTable[tkPhpVariable]:=fPhp_VariableAttri;
  fTokenAttributeTable[tkPhpConst]:=fPhp_ConstAttri;
  fTokenAttributeTable[tkPhpString]:=fPhp_StringAttri;
  fTokenAttributeTable[tkPhpStringSpecial]:=fPhp_StringSpecialAttri;
  fTokenAttributeTable[tkPhpComment]:=fPhp_CommentAttri;
  fTokenAttributeTable[tkPhpSymbol]:=fPhp_SymbolAttri;
  fTokenAttributeTable[tkPhpNumber]:=fPhp_NumberAttri;
  fTokenAttributeTable[tkPhpError]:=fPhp_ErrorAttri;

  // Global
  fInactiveAttri:=TSynHighlighterAttributes.Create('Global: Inactive');
  with fInactiveAttri do
  begin
    Background:=clNone;
    Foreground:=clInactiveCaption;
    Style:=[];
  end;
  AddAttribute(fInactiveAttri);

  fTokenAttributeTable[tkNull]:=nil;
end;

function TSynWebEngine.GetIdentChars:TSynIdentChars;
begin
  Result:=TSynValidStringChars;
end;

function TSynWebEngine.GetToken:string;
var
  Len:LongInt;
begin
  Len:=Run-fTokenPos;
  SetString(Result, (FLine+fTokenPos), Len);
end;

function TSynWebEngine.GetTokenLen: Integer;
begin
  Result:=Run-fTokenPos;
end;

function TSynWebEngine.GetTokenPos:Integer;
begin
  Result:=fTokenPos;
end;

function TSynWebEngine.GetTokenID:TtkTokenKind;
begin
  Result:=FTokenID;
end;

function TSynWebEngine.GetTokenKind:Integer;
begin
  Result:=Ord(FTokenID);
end;

function TSynWebEngine.GetEol:Boolean;
begin
  Result:=FConfig.fTokenID=tkNull;
end;


procedure TSynWebEngine.SetLine(NewValue:string; LineNumber:Integer);
{$IFDEF SYNWEB_FIXNULL}
var
  i:Integer;
{$ENDIF}
begin
  fLineRef:=NewValue;
{$IFDEF SYNWEB_FIXNULL}
  for i:=1 to Length(fLineRef) do
    if fLineRef[i]=#0 then
      fLineRef[i]:=' ';
{$ENDIF}
  fLine:=PChar(fLineRef);
  Run:=0;
  fLineNumber:=LineNumber;
  fHighlighterType:=TSynHighlighterType(GetRange_Int(3, 29));
  fPrevHighlighterType:=fHighlighterType;
  fHighlighterSW:=False;
  SetupHighligterType;
  fNextProcTable;
end;

procedure TSynWebEngine.Next;
begin
  fHighlighterSW:=False;
  fNextProcTable;
end;

function TSynWebEngine.UpdateActiveHighlighter(ARange: Pointer; ALine: String; ACaretX, ACaretY: Integer):Boolean;
var
  f:TSynHighlighterTypes;
  lPos,lLen:Integer;
  lHinghlighter,ActiveHL:TSynHighlighterType;
begin
  Result:=True;
  if not FActiveHighlighter or not (fHighlighterMode in [shmHtml, shmPhp]) then
    Exit;
  f:=fActiveHighlighters;
  Dec(ACaretX);
  SetRange(ARange);
  lHinghlighter:=TSynHighlighterType(GetRange_Int(3, 29));
  SetLine(ALine, ACaretY);
  lPos:=GetTokenPos;
  lLen:=GetTokenLen;
  while (GetTokenPos<ACaretX) and not GetEol do
  begin
    lHinghlighter:=fHighlighterType;
    lPos:=GetTokenPos;
    lLen:=GetTokenLen;
    Next;
  end;
  if fUseNextAH and (ACaretX>=lPos+lLen) then
    ActiveHL:=fHighlighterType
  else
    if fHighlighterSW and (ACaretX>=lPos+lLen) then
      ActiveHL:=fPrevHighlighterType
    else
      ActiveHL:=lHinghlighter;
  if ActiveHL>=shtPHP_inHtml then
    fActiveHighlighters:=[shtPHP_inHtml, shtPHP_inCss, shtPHP_inES]
  else
    fActiveHighlighters:=[ActiveHL];
  Result:=f<>fActiveHighlighters;
end;

function TSynWebEngine.GetCRC8_String(AString:String):Byte;
var
  i:Integer;
begin
  Result:=Length(AString);
  for i:=1 to Length(AString) do
    Result:=TCrc8_Table[Result xor Byte(AString[i])];
end;

function TSynWebEngine.GetCurrentActiveHighlighters: TSynHighlighterTypes;
begin
  Result:=fActiveHighlighters;
end;

procedure TSynWebEngine.NullProc;
begin
  fTokenID:=tkNull;
end;

function TSynWebEngine.GetRange_Bit(ABit:Longword):Boolean;
begin
  Result:=fRange and (1 shl ABit)<>0;
end;

procedure TSynWebEngine.SetRange_Bit(ABit:Longword; AVal:Boolean);
begin
  if AVal then
    fRange:=fRange or (1 shl ABit)
  else
    fRange:=fRange and not (1 shl ABit);
end;

function TSynWebEngine.GetRange_Int(ALen, APos: Longword): Longword;
begin
  Result:=(fRange shr APos) and not ($FFFFFFFF shl ALen);
end;

procedure TSynWebEngine.SetRange_Int(ALen, APos, AVal: Longword);
var
  i:Longword;
begin
  i:=$FFFFFFFF shl ALen;
  //todo: Does it work in CLX? Should be [EBX].APos? I don't know :(
  asm
    mov ecx, APos
    rol i, cl
  end;
  fRange:=(fRange and i) or ((AVal shl APos) and not i);
end;

procedure TSynWebEngine.NextSetHighligterType;
begin
  SetHighligterType(fNextHighlighterType, fNextClearBits, False, fNextUseNextAH);
  Next;
  fHighlighterSW:=True;
end;

procedure TSynWebEngine.SetHighligterType(const AHighlighterType:TSynHighlighterType;
  AClearBits:Boolean; ASetAtNextToken: Boolean; AUseNextAH:Boolean);
begin
  if ASetAtNextToken then
  begin
    fNextUseNextAH:=AUseNextAH;
    fNextHighlighterType:=AHighlighterType;
    fNextClearBits:=AClearBits;
    fNextProcTable:=NextSetHighligterType;
  end else
  begin
    fUseNextAH:=AUseNextAH;
    fHighlighterSW:=True;
    fPrevHighlighterType:=fHighlighterType;
    fHighlighterType:=AHighlighterType;
    SetRange_Int(3, 29, Longword(AHighlighterType));
    SetupHighligterType(AClearBits);
  end;
end;

procedure TSynWebEngine.SetupHighligterType(AClearBits:Boolean);
begin
  case fHighlighterType of
  shtHtml:
    begin
      if AClearBits then
        SetRange_Int(17, 0, 0);
      fSYN_ATTR_COMMENT:=fHtml_CommentAttri;
      fSYN_ATTR_STRING:=fHtml_TagKeyValueQuotedAttri;
      fSYN_ATTR_WHITESPACE:=fHtml_WhitespaceAttri;
      fNextProcTable:=Html_Next;
    end;
  shtCss:
    begin
      if AClearBits then       
        SetRange_Int(17, 0, 0);
      fSYN_ATTR_COMMENT:=fCss_CommentAttri;
      fSYN_ATTR_STRING:=fCss_ValStringAttri;
      Css_UpdateBg;
      fNextProcTable:=Css_Next;
    end;
  shtES:
    begin
      if AClearBits then             
        SetRange_Int(17, 0, 0);
      fSYN_ATTR_COMMENT:=fES_CommentAttri;
      fSYN_ATTR_STRING:=fES_StringAttri;
      fSYN_ATTR_WHITESPACE:=fES_WhitespaceAttri;
      fNextProcTable:=ES_Next;
    end;
  else // PHP
    if AClearBits then
      SetRange_Int(12, 17, 0);
    fSYN_ATTR_COMMENT:=fPhp_CommentAttri;
    fSYN_ATTR_STRING:=fPhp_StringAttri;
    fSYN_ATTR_WHITESPACE:=fPhp_WhitespaceAttri;
    fNextProcTable:=Php_Next;
  end;
end;

procedure TSynWebEngine.SetHighlighterMode(
  const Value: TSynHighlighterMode);
begin
  if fHighlighterMode=Value then
    Exit;
  fHighlighterMode:=Value;  
  SetupActiveHighlighter;
  DefHighlightChange(Self);
end;

procedure TSynWebEngine.SetActiveHighlighter(const Value: Boolean);
begin
  FActiveHighlighter := Value;
  if Value then
    SetupActiveHighlighter
  else
    fActiveHighlighters:=[shtHtml, shtCss, shtES, shtPHP_inHtml, shtPHP_inCss, shtPHP_inES];
end;

procedure TSynWebEngine.SetupActiveHighlighter;
begin
  case fHighlighterMode of
  shmHtml, shmPhp:
    fActiveHighlighters:=[shtHtml];
  shmCss:
    fActiveHighlighters:=[shtCss];
  shmES:
    fActiveHighlighters:=[shtES];
  shmPhpCgi:
    fActiveHighlighters:=[shtPHP_inHtml, shtPHP_inCss, shtPHP_inES];
  end;
end;

// HTML ------------------------------------------------------------------------

procedure TSynWebEngine.Html_MakeMethodTables;
var
  i:Integer;
  pF:PIdentFuncTableFunc;
  pF2:PIdent2FuncTableFunc;
begin
  fHtml_RangeProcTable[rsHtmlText]:=Html_RangeTextProc;
  fHtml_RangeProcTable[rsHtmlComment]:=Html_RangeCommentProc;
  fHtml_RangeProcTable[rsHtmlCommentClose]:=Html_RangeCommentCloseProc;
  fHtml_RangeProcTable[rsHtmlTag]:=Html_RangeTagProc;
  fHtml_RangeProcTable[rsHtmlTagClose]:=Html_RangeTagCloseProc;
  fHtml_RangeProcTable[rsHtmlTagDOCTYPE]:=Html_RangeTagDOCTYPEProc;
  fHtml_RangeProcTable[rsHtmlTagCDATA]:=Html_RangeTagCDATAProc;
  fHtml_RangeProcTable[rsHtmlTagKey]:=Html_RangeTagKeyProc;
  fHtml_RangeProcTable[rsHtmlTagKeyEq]:=Html_RangeTagKeyEqProc;
  fHtml_RangeProcTable[rsHtmlTagKeyValue]:=Html_RangeTagKeyValueProc;
  fHtml_RangeProcTable[rsHtmlTagKeyValueQuoted1]:=Html_RangeTagKeyValueQuoted1Proc;
  fHtml_RangeProcTable[rsHtmlTagKeyValueQuoted2]:=Html_RangeTagKeyValueQuoted2Proc;

  pF:=PIdentFuncTableFunc(@fHtml_TagIdentFuncTable);
  for I:=Low(fHtml_TagIdentFuncTable) to High(fHtml_TagIdentFuncTable) do
  begin
    pF^:=Html_TagUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_TagsFuncTable.inc}

  pF:=PIdentFuncTableFunc(@fHtml_AttrIdentFuncTable);
  for I:=Low(fHtml_TagIdentFuncTable) to High(fHtml_AttrIdentFuncTable) do
  begin
    pF^:=Html_AttrUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_AttrsFuncTable.inc}

  pF2:=PIdent2FuncTableFunc(@fHtml_SpecialIdentFuncTable);
  for I:=Low(fHtml_SpecialIdentFuncTable) to High(fHtml_SpecialIdentFuncTable) do
  begin
    pF2^:=Html_SpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_SpecialFuncTable.inc}
end;

procedure TSynWebEngine.Html_Next;
begin
  fTokenPos:=Run;
  fHtml_RangeProcTable[Html_GetRange];
end;

function TSynWebEngine.Html_GetRange: THtmlRangeState;
begin
  Result:=THtmlRangeState(GetRange_Int(4, 13));
end;

procedure TSynWebEngine.Html_SetRange(const ARange: THtmlRangeState);
begin
  SetRange_Int(4, 13, Longword(ARange));
end;

function TSynWebEngine.Html_GetTag: Integer;
begin
  Result:=GetRange_Int(7, 0);
end;

procedure TSynWebEngine.Html_SetTag(const ATag: Integer);
begin
  SetRange_Int(7, 0, Longword(ATag));
end;

procedure TSynWebEngine.SetHtml_Version(const Value: THtmlVersion);
begin
  fHtml_Version:=Value;
  if fHtml_Version>=hvXHtml10Strict then
    fHashTable:=fSensitiveHashTable
  else
    fHashTable:=fInsensitiveHashTable;
  DefHighlightChange(Self);
end;

function TSynWebEngine.Html_CheckNull(ADo:Boolean=True):Boolean;
begin
  if fLine[Run]=#0 then
  begin
    Result:=True;
    if ADo then
      NullProc;
  end else
    Result:=False;
end;

procedure TSynWebEngine.Html_SpaceProc;
begin
  repeat
    Inc(Run);
  until not(fLine[Run] in [#1..#32]); 
  fTokenID:=tkHtmlSpace;
end;

procedure TSynWebEngine.Html_AmpersandProc;
begin
  Inc(Run);
  fTokenID:=tkHtmlEscape;
  if fLine[Run]='#' then
  begin
    Inc(Run);
    if fHashTable[fLine[Run]]=fHashTable['x'] then
    begin
      Inc(Run);
      if fIdentTable[fLine[Run]] and (1 shl 10)=0 then // if not (fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9']) then
        fTokenID:=tkHtmlError
      else
        repeat
          Inc(Run)
        until fIdentTable[fLine[Run]] and (1 shl 10)=0; // until not (fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9']);
    end else
      if not (fLine[Run] in ['0'..'9']) then
        fTokenID:=tkHtmlError
      else
        repeat
          Inc(Run)
        until not (fLine[Run] in ['0'..'9']);
  end else
    if fIdentTable[fLine[Run]] and (1 shl 0)=0 then // if not (fLine[Run] in ['a'..'z', 'A'..'Z'] then
      fTokenID:=tkHtmlError
    else
    begin
      repeat
        Inc(Run)
      until fIdentTable[fLine[Run]] and (1 shl 0)=0; // until not (fLine[Run] in ['a'..'z', 'A'..'Z'];
      if Html_SpecialCheck(fTokenPos+1,Run-fTokenPos-1)=-1 then
        fTokenID:=tkHtmlError;
    end;
  if fLine[Run]=';' then
    Inc(Run)
  else
    fTokenID:=tkHtmlError;
end;

procedure TSynWebEngine.Html_BraceOpenProc;
begin
  if Php_CheckBegin then
    Exit;
  Inc(Run);
  case fLine[Run] of
  '/':
    begin
      Inc(Run);
      SetRange_Bit(12, True);
    end;
  '?':
    begin
      if fHtml_Version>=hvXHtml10Strict then
        Inc(Run);
      SetRange_Bit(12, False);
    end;
  '!':
    begin
      Inc(Run);
      if (fLine[Run]='-') and (fLine[Run+1]='-') then
      begin
        Inc(Run, 2);
        Html_SetRange(rsHtmlComment);
        if (fLine[Run]=#0) or Php_CheckBegin(False) then
          fTokenID:=tkHtmlComment
        else
          Html_RangeCommentProc;
      end else
        if (fHtml_Version>=hvXHtml10Strict) and
           (fLine[Run]='[') and
           (fLine[Run+1]='C') and
           (fLine[Run+2]='D') and
           (fLine[Run+3]='A') and
           (fLine[Run+4]='T') and
           (fLine[Run+5]='A') and
           (fLine[Run+6]='[') then
        begin
          Inc(Run, 7);
          fTokenID:=tkHtmlTag;
          Html_SetRange(rsHtmlTagCDATA);
        end else
          if (fHashTable[fLine[Run]]=fHashTable['D']) and
             (fHashTable[fLine[Run+1]]=fHashTable['O']) and
             (fHashTable[fLine[Run+2]]=fHashTable['C']) and
             (fHashTable[fLine[Run+3]]=fHashTable['T']) and
             (fHashTable[fLine[Run+4]]=fHashTable['Y']) and
             (fHashTable[fLine[Run+5]]=fHashTable['P']) and
             (fHashTable[fLine[Run+6]]=fHashTable['E']) and
             (fIdentTable[fLine[Run+7]] and (1 shl 0)=0) then // (not (fLine[Run] in ['a'..'z', 'A'..'Z'])) then
          begin
            fTokenID:=tkHtmlTag;
            SetRange_Int(2, 7, 0);
            Html_SetRange(rsHtmlTagDOCTYPE);
          end else
            fTokenID:=tkHtmlError;
      Exit;
    end;
  else
    SetRange_Bit(12, False);
  end;
  if fIdentTable[fLine[Run]] and (1 shl 0)<>0 then // if fLine[Run] in ['a'..'z', 'A'..'Z'] then
  begin
    fTokenID:=tkHtmlTag;
    Html_SetRange(rsHtmlTag);
  end else
    fTokenID:=tkHtmlError;
end;

procedure TSynWebEngine.Html_ErrorProc;
begin
  Inc(Run);
  fTokenID:=tkHtmlError;
end;

procedure TSynWebEngine.Html_RangeTextProc;
begin
  case fLine[Run] of
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
      Inc(Run);
    until fIdentTable[fLine[Run]] and (1 shl 6)<>0; // until fLine[Run] In [#0..#32, '<', '>', '&'];
    fTokenID:=tkHtmlText;
  end;
end;

procedure TSynWebEngine.Html_RangeCommentProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 19)=0 do // while not (fLine[Run) in [#0, '-', '<']) do
      Inc(Run);
    case fLine[Run] of
    #0:
      Break;
    '-':
      begin
        Inc(Run);
        if fLine[Run]='-' then
        begin
          Inc(Run);
          if fLine[Run]='>' then
          begin
            Inc(Run);
            Html_SetRange(rsHtmlText);
          end else
          begin
            Html_SetRange(rsHtmlCommentClose);
            if (fLine[Run]<>#0) and not Php_CheckBegin(False) then
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
        Inc(Run);
    end;
  until False;
  fTokenID:=tkHtmlComment;
end;

procedure TSynWebEngine.Html_RangeCommentCloseProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 20)=0 do // while not (fLine[Run) in [#0, '<', '>']) do
      Inc(Run);
    case fLine[Run] of
    #0:
      Break;
    '>':
      begin
        Inc(Run);
        Html_SetRange(rsHtmlText);
        Break;
      end;
    '<':
      if Php_CheckBegin(False) then
        Break
      else
        Inc(Run);
    end;
  until False;
  fTokenID:=tkHtmlComment;
end;

procedure TSynWebEngine.Html_RangeTagDOCTYPEProc;
begin
  case GetRange_Int(2, 7) of
  0:
    begin
      Inc(Run, 7);
      fTokenID:=tkHtmlTagName;
      SetRange_Int(2, 7, 1);
    end;
  1:
    if not Html_CheckNull and not Php_CheckBegin then
      case fLine[Run] of
      #1..#32:
        begin
          Html_SpaceProc;
          Exit;
        end;
      '>':
        begin
          Inc(Run);
          fTokenID:=tkHtmlTag;
          SetRange_Int(2, 7, 0);
          Html_SetRange(rsHtmlText);
          Exit;
        end;
      #39:
        begin
          Inc(Run);
          if fLine[Run]=#0 then
            fTokenID:=tkHtmlError
          else
          begin
            SetRange_Int(2, 7, 2);
            if Php_CheckBegin(False) then
              fTokenID:=tkHtmlTagKeyValueQuoted
            else
              Html_RangeTagDOCTYPEProc;
          end;
        end;
      '"':
        begin
          Inc(Run);
          if fLine[Run]=#0 then
            fTokenID:=tkHtmlError
          else
          begin
            SetRange_Int(2, 7, 3);
            if Php_CheckBegin(False) then
              fTokenID:=tkHtmlTagKeyValueQuoted
            else
              Html_RangeTagDOCTYPEProc;
          end;
        end;
      else
        if fIdentTable[fLine[Run]] and (1 shl 0)=0 then // if not (fLine[Run] in ['a'..'z', 'A'..'Z']) then
        begin
          Inc(Run);
          fTokenID:=tkHtmlError;
          Exit;
        end;
        repeat
          Inc(Run);
        until fIdentTable[fLine[Run]] and (1 shl 0)=0; // until not (fLine[Run] In ['a'..'z', 'A'..'Z']);
        fTokenID:=tkHtmlTagKey;
      end;
  2:
    begin
      if not Html_CheckNull then
        if Php_CheckBegin then
          Exit
        else
          repeat
            while fIdentTable[fLine[Run]] and (1 shl 21)=0 do // while not (fLine[Run] in [#0, #39, '<']) do
              Inc(Run);
            case fLine[Run] of
            #0:
              begin
                fTokenID:=tkHtmlError;
                Break;
              end;
            '<':
              if Php_CheckBegin(False) then
              begin
                fTokenID:=tkHtmlTagKeyValueQuoted;
                Exit;
              end else
                Inc(Run);
            #39:
              begin
                Inc(Run);
                fTokenID:=tkHtmlTagKeyValueQuoted;
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
            while fIdentTable[fLine[Run]] and (1 shl 22)=0 do // while not (fLine[Run] in [#0, '"', '<']) do
              Inc(Run);
            case fLine[Run] of
            #0:
              begin
                fTokenID:=tkHtmlError;
                Break;
              end;
            '<':
              if Php_CheckBegin(False) then
              begin
                fTokenID:=tkHtmlTagKeyValueQuoted;
                Exit;
              end else
                Inc(Run);
            '"':
              begin
                Inc(Run);
                fTokenID:=tkHtmlTagKeyValueQuoted;
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
  if fLine[Run] in [#1..#32] then
  begin
    Html_SpaceProc;
    Exit;
  end else
    if (fLine[Run]=']') and (fLine[Run+1]=']') and (fLine[Run+2]='>') then
    begin
      Inc(Run, 3);
      fTokenID:=tkHtmlTag;
      Html_SetRange(rsHtmlText);
    end else
    begin
      repeat
        repeat
          Inc(Run);
        until fIdentTable2[fLine[Run]] and (1 shl 1)<>0; // until fLine[Run] in [#0..#32, '<', ']'];
        case fLine[Run] of
        #0..#32, ']':
          Break;
        '<':
          if Php_CheckBegin(False) then
            Break;
        end;
      until False;
      fTokenID:=tkHtmlText;
    end;
end;

procedure TSynWebEngine.Html_RangeTagProc;
var
  ID:Integer;
begin
  repeat
    Inc(Run);
  until fIdentTable[fLine[Run]] and (1 shl 16)=0; // until not (fLine[Run] In ['a'..'z', 'A'..'Z', '_', '0'..'9']);
  fTokenID:=Html_TagCheck;
  ID:=Html_GetTag-1;
  if GetRange_Bit(12) then
  begin
    if (ID<>-1) and (TSynWeb_TagsData[ID] and (1 shl 31)<>0) then
      fTokenID:=tkHtmlError;
    Html_SetRange(rsHtmlTagClose);
  end else
  begin
    if (ID<>-1) and ((fLine[fTokenPos-1]='?') xor (TSynWeb_TagsData[ID] and (1 shl 29)<>0)) then
      fTokenID:=tkHtmlError;
    Html_SetRange(rsHtmlTagKey);
  end;
end;

procedure TSynWebEngine.Html_RangeTagCloseProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case fLine[Run] of
  #1..#32:
    Html_SpaceProc;
  '>':
    begin
      Inc(Run);
      fTokenID:=tkHtmlTag;
      Html_SetRange(rsHtmlText);
    end;
  else
    fTokenID:=tkHtmlError;
    repeat
      repeat
        Inc(Run);
      until fIdentTable[fLine[Run]] and (1 shl 1)<>0; // until not (fLine[Run] In [#0..#32, '<', '>']) do
      if (fLine[Run]='<') and not Php_CheckBegin(False) then
        Continue
      else
        Break;
    until False;
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyProc;
var
  ID:Integer;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  ID:=Html_GetTag-1;
  if (ID<>-1) and (TSynWeb_TagsData[ID] and (1 shl 29)<>0) then
    if (fLine[Run]='?') and (fLine[Run+1]='>') then
    begin
      Inc(Run, 2);
      fTokenID:=tkHtmlTag;
      Html_SetRange(rsHtmlText);
      Exit;
    end else
      if fLine[Run]='>' then
      begin
        Inc(Run);
        fTokenID:=tkHtmlError;
        Html_SetRange(rsHtmlText);
        Exit;
      end;
  case fLine[Run] of
  #1..#32:
    Html_SpaceProc;
  '/':
    if not GetRange_Bit(12) and (fLine[Run+1]='>') and
      (fHtml_Version>=hvXHtml10Strict) and (TSynWeb_TagsData[ID] and (1 shl 31)<>0) then
    begin
      Inc(Run,2);
      fTokenID:=tkHtmlTag;
      Html_SetRange(rsHtmlText);
    end else
    begin
      Inc(Run);
      fTokenID:=tkHtmlError;
    end;
  '>':
    begin
      Inc(Run);
      fTokenID:=tkHtmlTag;
      if (ID<>-1) and (TSynWeb_TagsData[ID] and (1 shl 31)<>0) and (fHtml_Version>=hvXHtml10Strict) then
        fTokenID:=tkHtmlError
      else
        if not GetRange_Bit(12) and ((Run=0) or (fLine[Run-2]<>'/'))   then
          if (ID=Html_TagID_Style) then
          begin
            SetHighligterType(shtCss, True, True, True);
            Exit;
          end else
            if (ID=Html_TagID_Script) then
            begin
              if GetRange_Bit(18) and (fHighlighterMode=shmPhp) then
              begin
                Php_Begin(potHTML);
                Exit;
              end else
                begin
                  SetHighligterType(shtES, True, True, True);
                  Exit;
                end;
            end;
      Html_SetRange(rsHtmlText);
    end;
  else
    if fIdentTable[fLine[Run]] and (1 shl 0)=0 then // if not (fLine[Run] in ['a'..'z', 'A'..'Z']) then
      Html_ErrorProc
    else
    begin
      repeat
        Inc(Run);
      until fIdentTable[fLine[Run]] and (1 shl 7)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', ':', '-']);
      if ID=-1 then
        fTokenID:=tkHtmlTagKeyUndef
      else
      begin
        fTokenID:=Html_AttrCheck;
        if ID=Html_TagID_Script then
          SetRange_Bit(17, fToken_LastID=Html_AttrID_Language);
      end;
    end;
    Html_SetRange(rsHtmlTagKeyEq);
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyEqProc;
begin
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case fLine[Run] of
  #1..#32:
    Html_SpaceProc;
  '=':
    begin
      Inc(Run);
      fTokenID:=tkHtmlSymbol;
      Html_SetRange(rsHtmlTagKeyValue);
    end;
  else
    Html_SetRange(rsHtmlTagKey);
    Html_RangeTagKeyProc;
    if fHtml_Version>=hvXHtml10Strict then
      fTokenID:=tkHtmlError;
  end;
end;

procedure TSynWebEngine.Html_RangeTagKeyValueProc;
var
  ID:Integer;
begin    
  if Html_CheckNull or Php_CheckBegin then
    Exit;
  case fLine[Run] of
  #1..#32:
    Html_SpaceProc;
  #39:
    begin
      Inc(Run);
      if fLine[Run]=#0 then
      begin
        Html_SetRange(rsHtmlTagKey);
        fTokenID:=tkHtmlError;
      end else
      begin
        Html_SetRange(rsHtmlTagKeyValueQuoted1);
        if Php_CheckBegin(False) then
          fTokenID:=tkHtmlTagKeyValueQuoted
        else
          Html_RangeTagKeyValueQuoted1Proc;
      end;
    end;
  '"':
    begin
      Inc(Run);
      if fLine[Run]=#0 then
      begin
        Html_SetRange(rsHtmlTagKey);
        fTokenID:=tkHtmlError;
      end else
      begin
        Html_SetRange(rsHtmlTagKeyValueQuoted2);
        if Php_CheckBegin(False) then
          fTokenID:=tkHtmlTagKeyValueQuoted
        else
          Html_RangeTagKeyValueQuoted2Proc;
      end;
    end;
  else
    if (fLine[Run]='>') or ((fHtml_Version>=hvXHtml10Strict) and (fLine[Run]='/') and (fLine[Run+1]='>')) then
    begin
      if fLine[Run]='/' then
        Inc(Run,2)
      else
        Inc(Run);
      fTokenID:=tkHtmlError;
      if not GetRange_Bit(12) and ((Run=0) or (fLine[Run-2]<>'/')) then
      begin
        ID:=Html_GetTag-1;
        if (ID=Html_TagID_Style) then
        begin
          SetHighligterType(shtCss, True, True, True);
          Exit;
        end else
          if (ID=Html_TagID_Script) then
          begin
            if GetRange_Bit(18) and (fHighlighterMode=shmPhp) then
            begin
              Php_Begin(potHTML);
              Exit;
            end else
              begin
                SetHighligterType(shtES, True, True, True);
                Exit;
              end;
          end;
      end;
      Html_SetRange(rsHtmlText);
    end else
    begin
      repeat
        repeat
          Inc(Run);
        until fIdentTable[fLine[Run]] and (1 shl 23)<>0; // until fLine[Run] in [#0..#32, '<', '>', '/'];
        case fLine[Run] of
        '/':
          if (fLine[Run+1]='>') and (fHtml_Version>=hvXHtml10Strict) then
            Break;
        '<':
          if Php_CheckBegin(False) then
            Break
          else
            Inc(Run);
        else
          Break;
        end;
      until False;
      if fHtml_Version>=hvXHtml10Strict then
        fTokenID:=tkHtmlError
      else
        fTokenID:=tkHtmlTagKeyValue;
      if GetRange_Bit(17) then
        SetRange_Bit(18, UpperCase(GetToken)='PHP');
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
        while fIdentTable[fLine[Run]] and (1 shl 21)=0 do // while not (fLine[Run] in [#0, #39, '<']) do
          Inc(Run);
        case fLine[Run] of
        #0:
          begin
            fTokenID:=tkHtmlError;
            Break;
          end;
        '<':
          if Php_CheckBegin(False) then
          begin
            fTokenID:=tkHtmlTagKeyValueQuoted;
            Exit;
          end else
            Inc(Run);
        #39:
          begin
            Inc(Run);
            fTokenID:=tkHtmlTagKeyValueQuoted;
            if GetRange_Bit(17) then
              SetRange_Bit(18, UpperCase(GetToken)=#39'PHP'#39);
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
        while fIdentTable[fLine[Run]] and (1 shl 22)=0 do // while not (fLine[Run] in [#0, '"', '<']) do
          Inc(Run);
        case fLine[Run] of
        #0:
          begin
            fTokenID:=tkHtmlError;
            Break;
          end;
        '<':
          if Php_CheckBegin(False) then
          begin
            fTokenID:=tkHtmlTagKeyValueQuoted;
            Exit;
          end else
            Inc(Run);
        '"':
          begin
            Inc(Run);
            fTokenID:=tkHtmlTagKeyValueQuoted;
            if GetRange_Bit(17) then
              SetRange_Bit(18, UpperCase(GetToken)='"PHP"');
            Break;
          end;
        end;
      until False;
  Html_SetRange(rsHtmlTagKey);
end;

function TSynWebEngine.Html_TagKeyComp(const ID:Integer):Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
begin
  if TSynWeb_TagsData[ID] and (1 shl Longword(fHtml_Version))=0 then
  begin
    Result:=False;
    Exit;
  end;
  aKey:=TSynWeb_Tags[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fHashTable[Temp^]<>fHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Html_TagCheck: TtkTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=Run-fTokenPos;
    for i:=0 to fStringLen-1 do
    begin
      inc(HashKey, fHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[fTokenPos];
  KeyHash(fToIdent);
  fToken_LastID:=-1;
  if HashKey<=Html_TagMaxKeyHash then
    Result:=fHtml_TagIdentFuncTable[HashKey]
  else
    Result:=tkHtmlTagNameUndef;
  Html_SetTag(fToken_LastID+1);
end;

{$I SynHighlighterWeb_TagsFunc.inc}

function TSynWebEngine.Html_AttrKeyComp(const ID: Integer): Boolean;
var
  I, tag:Integer;
  Temp:PChar;
  aKey:String;
begin
  tag:=Html_GetTag-1;
  if (tag=-1) or (TSynWeb_AttrsData[ID][Longword(fHtml_Version)][tag div 32] and (1 shl (tag mod 32))=0) then
  begin
    Result:=False;
    Exit;
  end;
  aKey:=TSynWeb_Attrs[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fHashTable[Temp^]<>fHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Html_AttrCheck: TtkTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=Run-fTokenPos;
    for i:=0 to fStringLen-1 do
    begin
      inc(HashKey, fHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[fTokenPos];
  KeyHash(fToIdent);
  fToken_LastID:=-1;
  if HashKey<=Html_AttrMaxKeyHash then
    Result:=fHtml_AttrIdentFuncTable[HashKey]
  else
    Result:=tkHtmlTagKeyUndef;
end;

{$I SynHighlighterWeb_AttrsFunc.inc}

function TSynWebEngine.Html_SpecialKeyComp(const ID: Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
begin
  aKey:=TSynWeb_Special[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fHashTable[Temp^]<>fHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Html_SpecialCheck(AStart, ALen:Integer): Integer;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=ALen;
    for i:=0 to ALen-1 do
    begin
      inc(HashKey, fHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[AStart];
  KeyHash(fToIdent);
  if (HashKey>Html_SpecialMaxKeyHash) or not fHtml_SpecialIdentFuncTable[HashKey] then
    fToken_LastID:=-1;
  Result:=fToken_LastID;
end;

{$I SynHighlighterWeb_SpecialFunc.inc}

// CSS -------------------------------------------------------------------------

procedure TSynWebEngine.Css_MakeMethodTables;
var
  c:Char;
  i:Integer;
  pF:PIdentFuncTableFunc;
  pF2:PIdent2FuncTableFunc;
begin
  for c:=#0 to #255 do
    case c of
      #0: fCss_ProcTable[c]:=NullProc;
      #1..#32: fCss_ProcTable[c]:=Css_SpaceProc;
      '@': fCss_ProcTable[c]:=Css_AtKeywordProc;
      '/': fCss_ProcTable[c]:=Css_SlashProc;
      '<': fCss_ProcTable[c]:=Css_BraceOpenProc;
      '{': fCss_ProcTable[c]:=Css_CurlyBraceOpenProc;
      '}': fCss_ProcTable[c]:=Css_CurlyBraceCloseProc;
      '*', '>': fCss_ProcTable[c]:=Css_ChildAnySelectorProc;
      '[': fCss_ProcTable[c]:=Css_AttribProc;
      '#': fCss_ProcTable[c]:=Css_HashProc;
      '.': fCss_ProcTable[c]:=Css_DotProc;
      ',': fCss_ProcTable[c]:=Css_CommaProc;
      ':': fCss_ProcTable[c]:=Css_ColonProc;
      ';': fCss_ProcTable[c]:=Css_SemiColonProc;
      '!': fCss_ProcTable[c]:=Css_ExclamationProc;
      #39, '"': fCss_ProcTable[c]:=Css_StringProc;
      '+': fCss_ProcTable[c]:=Css_PlusProc;
      '-': fCss_ProcTable[c]:=Css_MinusProc;
      '0'..'9': fCss_ProcTable[c]:=Css_NumberProc;
      'a'..'z', 'A'..'Z', '\': fCss_ProcTable[c]:=Css_IdentProc;
    else
      fCss_ProcTable[c]:=Css_ErrorProc;
    end;

  fCss_RangeProcTable[rsCssRuleset]:=Css_RangeRulesetProc;
  fCss_RangeProcTable[rsCssSelectorAttrib]:=Css_RangeSelectorAttribProc;
  fCss_RangeProcTable[rsCssSelectorPseudo]:=Css_RangeSelectorPseudoProc;
  fCss_RangeProcTable[rsCssAtKeyword]:=Css_RangeAtKeywordProc;
  fCss_RangeProcTable[rsCssComment]:=Css_RangeCommentProc;
  fCss_RangeProcTable[rsCssProp]:=Css_RangePropProc;
  fCss_RangeProcTable[rsCssPropVal]:=Css_RangePropValProc;
  fCss_RangeProcTable[rsCssPropValStr]:=Css_RangePropValStrProc;
  fCss_RangeProcTable[rsCssPropValRgb]:=Css_RangePropValRgbProc;
  fCss_RangeProcTable[rsCssPropValSpecial]:=Css_RangePropValSpecialProc;
  fCss_RangeProcTable[rsCssPropValImportant]:=Css_RangePropValImportantProc;
  fCss_RangeProcTable[rsCssPropValUrl]:=Css_RangePropValUrlProc;
  fCss_RangeProcTable[rsCssPropValRect]:=Css_RangePropValRectProc;
  fCss_RangeProcTable[rsCssPropValFunc]:=Css_RangePropValFuncProc;

  pF:=PIdentFuncTableFunc(@fCss_PropIdentFuncTable);
  for I:=Low(fCss_PropIdentFuncTable) to High(fCss_PropIdentFuncTable) do
  begin
    pF^:=Css_PropUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssPropsFuncTable.inc}

  pF:=PIdentFuncTableFunc(@fCss_ValIdentFuncTable);
  for I:=Low(fCss_ValIdentFuncTable) to High(fCss_ValIdentFuncTable) do
  begin
    pF^:=Css_ValUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssValsFuncTable.inc}

  pF2:=PIdent2FuncTableFunc(@fCss_SpecialIdentFuncTable);
  for I:=Low(fCss_SpecialIdentFuncTable) to High(fCss_SpecialIdentFuncTable) do
  begin
    pF2^:=CSS_SpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_CssSpecialFuncTable.inc}
end;

procedure TSynWebEngine.Css_NextBg;
begin
  Css_UpdateBg;
  fNextProcTable:=Css_Next;
  Css_Next;
end;

procedure TSynWebEngine.Css_Next;
begin
  fTokenPos:=Run;
  fCss_RangeProcTable[Css_GetRange];
end;

procedure TSynWebEngine.Css_UpdateBg;
begin
  if TCssRangeState(GetRange_Int(4, 13)) in [TCssRangeState_RulesetBegin..TCssRangeState_RulesetEnd] then
    fSYN_ATTR_WHITESPACE:=fCss_RulesetWhitespaceAttri
  else
    fSYN_ATTR_WHITESPACE:=fCss_WhitespaceAttri;
  fTokenAttributeTable[tkCssSpace]:=fSYN_ATTR_WHITESPACE;
end;

function TSynWebEngine.Css_GetRange: TCssRangeState;
begin
  if GetRange_Bit(12) then
    Result:=rsCssComment
  else
    Result:=TCssRangeState(GetRange_Int(4, 13));
end;

procedure TSynWebEngine.Css_SetRange(const ARange: TCssRangeState);
begin
  if ARange=rsCssComment then
    SetRange_Bit(12, True)
  else
  begin
    if not (ARange in [TCssRangeState_RulesetBegin..TCssRangeState_RulesetEnd]) and
      (TCssRangeState(GetRange_Int(4, 13)) in [TCssRangeState_RulesetBegin..TCssRangeState_RulesetEnd]) then
    begin   
      SetRange_Int(4, 13, Longword(ARange));
      fNextProcTable:=Css_NextBg;
    end else
    begin   
      SetRange_Int(4, 13, Longword(ARange));
      Css_UpdateBg;
    end;
    if ARange=rsCssRuleset then
      SetRange_Int(11, 0, 0);
  end;
end;

function TSynWebEngine.Css_GetProp: Integer;
begin
  Result:=GetRange_Int(8, 0);
end;

procedure TSynWebEngine.Css_SetProp(const AProp: Integer);
begin
  SetRange_Int(8, 0, Longword(AProp));
end;

procedure TSynWebEngine.SetCss_Version(const Value: TCssVersion);
begin
  fCss_Version:=Value;
  DefHighlightChange(Self);
end;

function TSynWebEngine.Css_CheckNull(ADo:Boolean=True):Boolean;
begin
  case fLine[Run] of
  #0:
    begin
      Result:=True;
      if ADo then
        NullProc;
    end;
  '<':
    if (fLine[Run+1]='/') and
       (fHashTable[fLine[Run+2]]=fHashTable['s']) and
       (fHashTable[fLine[Run+3]]=fHashTable['t']) and
       (fHashTable[fLine[Run+4]]=fHashTable['y']) and
       (fHashTable[fLine[Run+5]]=fHashTable['l']) and
       (fHashTable[fLine[Run+6]]=fHashTable['e']) and
       (fIdentTable2[fLine[Run+7]] and (1 shl 0)<>0) and // (fLine[Run+7] in [#0..#32, '>']) and
       (fHighlighterMode in [shmHtml, shmPhp]) then
    begin
      Result:=True;
      if ADo then
      begin
        fTokenID:=tkHtmlTag;
        SetHighligterType(shtHtml, True, False, False);
      end;
    end else
      Result:=False;
  else
    Result:=False;
  end;
end;

procedure TSynWebEngine.Css_SpaceProc;
begin
  repeat
    Inc(Run);
  until not(fLine[Run] in [#1..#32]);
  fTokenID:=tkCssSpace;
end;

procedure TSynWebEngine.Css_AtKeywordProc;
begin
  if fIdentTable[fLine[Run+1]] and (1 shl 0)=0 then // if not (fLine[Run+1] in ['a'..'z', 'A'..'Z']) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssAtKeyword);
  end;
end;

procedure TSynWebEngine.Css_SlashProc;
begin
  if fLine[Run+1]='*' then
  begin
    Inc(Run,2);
    SetRange_Bit(12, True); // Css_SetRange(rsCssComment);
    if Css_CheckNull(False) or Php_CheckBegin(False) then
      fTokenID:=tkCssComment
    else
      Css_RangeCommentProc;
  end else
    if (Css_GetRange=rsCssPropVal) and GetRange_Bit(8) then
    begin
      SetRange_Bit(8, False);
      Css_SymbolProc
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_BraceOpenProc;
begin
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  if (fLine[Run+1]='!') and
    (fLine[Run+2]='-') and
    (fLine[Run+3]='-') then
  begin
    Inc(Run,4);
    fTokenID:=tkHtmlComment;
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_CurlyBraceOpenProc;
begin
  Css_SymbolProc;
  Css_SetRange(rsCssProp);
end;

procedure TSynWebEngine.Css_CurlyBraceCloseProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssRuleset);
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
  if fCss_Version=cvCss21 then
    Css_SymbolProc
  else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_AttribProc;
begin
  if fCss_Version=cvCss1 then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssSelectorAttrib);
  end;
end;

procedure TSynWebEngine.Css_HashProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    if (fIdentTable[fLine[Run+1]] and (1 shl 10)<>0) and // if fLine[Run+1] in ['a'..'f', 'A'..'F', '0'..'9'] and
       (fIdentTable[fLine[Run+2]] and (1 shl 10)<>0) and //   fLine[Run+2] in ['a'..'f', 'A'..'F', '0'..'9'] and
       (fIdentTable[fLine[Run+3]] and (1 shl 10)<>0) then //   fLine[Run+3] in ['a'..'f', 'A'..'F', '0'..'9'] then
    begin
      Css_SymbolProc;
      Css_SetRange(rsCssPropValSpecial);
    end else
      Css_ErrorProc;
  end else
    if (fIdentTable[fLine[Run+1]] and (1 shl 8)=0) or // if not (fLine[Run+1] in ['a'..'z', 'A'..'Z', '\']) or
       ((fLine[Run+1]='\') and (fLine[Run+2] in [#0..#31])) then
      Css_ErrorProc
    else
    begin
      Css_SymbolProc;
      SetRange_Bit(8, True);
    end;
end;

procedure TSynWebEngine.Css_DotProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    if fLine[Run+1] in ['0'..'9'] then
    begin
      fCss_Mask:=$F5000000;
      Css_NumberDefProc;
    end else
      Css_ErrorProc;
  end else
  begin
    if (fIdentTable[fLine[Run+1]] and (1 shl 8)=0) or // if not (fLine[Run] in ['a'..'z', 'A'..'Z', '\']) or
      ((fLine[Run+1]='\') and (fLine[Run+2] in [#0..#31])) then
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
  prop:Integer;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    prop:=Css_GetProp-1;
    if (prop=-1) or (TSynWeb_CssPropsData[prop] and (1 shl 16)=0) then
    begin
      Css_ErrorProc;
      Exit;
    end;
  end;
  Css_SymbolProc;
end;

procedure TSynWebEngine.Css_ColonProc;
begin
  if fIdentTable[fLine[Run+1]] and (1 shl 0)=0 then // if not (fLine[Run+1] in ['a'..'z', 'A'..'Z']) then
    Css_ErrorProc
  else
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssSelectorPseudo);
  end;
end;

procedure TSynWebEngine.Css_SemiColonProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssProp);
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_ExclamationProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    Css_SymbolProc;
    Css_SetRange(rsCssPropValImportant);
    SetRange_Bit(8, False);
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_StringProc;
var
  prop:Integer;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    fTokenID:=tkCssValString;
    if fLine[Run]=#39 then
    begin
      Inc(Run);
      if not Css_CustomStringProc(TCssString39, False) then
      begin
        Css_SetRange(rsCssPropValStr);
        SetRange_Bit(8, True);
      end;
    end else
    begin     
      Inc(Run);
      if not Css_CustomStringProc(TCssString34, False) then
      begin
        Css_SetRange(rsCssPropValStr);
        SetRange_Bit(9, True);
      end;
    end;
    if fTokenID=tkCssValString then
    begin
      prop:=Css_GetProp-1;
      if (prop=-1) or (TSynWeb_CssPropsData[prop] and (1 shl 19)=0) then
        fTokenID:=tkCssValUndef;
    end;    
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_PlusProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    Inc(Run);   
    if fIdentTable[fLine[Run]] and (1 shl 13)<>0 then // if fLine[Run] in ['0'..'9', '.'] then
    begin
      fCss_Mask:=$F5400000;
      Css_NumberDefProc;
    end else
      fTokenID:=tkCssError;
  end else
    if fCss_Version=cvCss21 then
      Css_SymbolProc
    else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_MinusProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    Inc(Run);
    if fIdentTable[fLine[Run]] and (1 shl 13)<>0 then // if fLine[Run] in ['0'..'9', '.'] then
    begin
      fCss_Mask:=$8AA00000;
      Css_NumberDefProc;
    end else
      fTokenID:=tkCssError;
  end else
    if (Css_GetRange=rsCssRuleset) and
      (fLine[Run+1]='-') and
      (fLine[Run+2]='>') then
    begin
      Inc(Run,3);
      fTokenID:=tkHtmlComment;
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_NumberProc;
begin
  if Css_GetRange=rsCssPropVal then
  begin
    fCss_Mask:=$F5400000;
    Css_NumberDefProc;
  end else
    Css_ErrorProc;
end;

procedure TSynWebEngine.Css_NumberDefProc;
var
  prop, OldRun:Integer;

  procedure CheckOther;
  begin
    if (Run-fTokenPos=1) and (fLine[Run-1]='0') then
      fCss_Mask:=fCss_Mask and $F5400000
    else
      fCss_Mask:=fCss_Mask and $01E00000;      
    if (fTokenPos>1) and ((fLine[fTokenPos-1]='/') and (fLine[fTokenPos-2]<>'*')) then
      fCss_Mask:=fCss_Mask or $18000000;
  end;

begin
  while fLine[Run] in ['0'..'9'] do
    Inc(Run);
  if fLine[Run]='.' then
  begin
    fCss_Mask:=fCss_Mask and $FF800000;
    Inc(Run);
    if fLine[Run] in ['0'..'9'] then
      repeat
        Inc(Run);
      until not (fLine[Run] in ['0'..'9'])
    else
    begin
      fTokenID:=tkCssError;
      Exit;
    end;
  end;
  if (fLine[Run]='%') then
  begin
    fCss_Mask:=fCss_Mask and $06000000;
    Css_SetRange(rsCssPropValSpecial);
  end else
  begin
    OldRun:=Run;
    if Css_IdentStartProc then
    begin
      prop:=Css_SpecialCheck(OldRun,Run-OldRun);
      if prop<>-1 then 
      begin
        fCss_Mask:=fCss_Mask and TSynWeb_CssSpecialData[prop];
        Css_SetRange(rsCssPropValSpecial);
        if (fLine[Run]='/') and (fLine[Run+1]<>'*') then
          SetRange_Bit(8, True);
        Run:=OldRun;
      end else
        if fCss_Version=cvCss1 then
        begin
          Run:=OldRun;
          CheckOther;
        end else
        begin
          fTokenID:=tkCssError;
          Exit;
        end;
    end else
      CheckOther;
  end;
  prop:=Css_GetProp-1;
  if (prop=-1) or (TSynWeb_CssPropsData[prop] and fCss_Mask=0) then
    fTokenID:=tkCssValUndef
  else
    fTokenID:=tkCssValNumber;
end;

procedure TSynWebEngine.Css_IdentProc;
begin
  if Css_IdentStartProc then
  begin
    if (Html_TagCheck=tkHtmlTagName) and (TSynWeb_TagsData[Html_GetTag-1] and (1 shl 30)=0) then
      fTokenID:=tkCssSelector
    else
      fTokenID:=tkCssSelectorUndef;
  end else
    Css_ErrorProc;
end;

function TSynWebEngine.Css_IdentStartProc:Boolean;
begin
  if (fIdentTable[fLine[Run]] and (1 shl 8)=0) or // if not (fLine[Run] in ['a'..'z', 'A'..'Z', '\']) or
    ((fLine[Run]='\') and (fLine[Run+1] in [#0..#31])) then
  begin
    Result:=False;
    Exit;
  end;
  fStringLenClean:=0;
  repeat                     
    if fLine[Run]<>'\' then
      Inc(Run)
    else
      if not (fLine[Run+1] in [#0..#31]) then
      begin
        Inc(fStringLenClean);
        Inc(Run,2);
      end else
        Break;
  until fIdentTable[fLine[Run]] and (1 shl 9)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', '\', '0'..'9', '-', '_']);
  fStringLenClean:=Run-fTokenPos-fStringLenClean;
  Result:=True;
end;

function TSynWebEngine.Css_CustomStringProc(AShl:Longword; ADo:Boolean):Boolean;
begin
  if Css_CheckNull(ADo) then
  begin
    if not ADo then
      fTokenID:=tkCssError;
    Result:=True;
    Exit;
  end else
    if Php_CheckBegin(ADo) then
    begin
      if not ADo then
        fTokenID:=tkCssValString;
      Result:=False;
      Exit;
    end;
  Result:=True;
  AShl:=1 shl AShl;
  repeat
    while fIdentTable[fLine[Run]] and AShl=0 do // while not (fLine[Run] in [#0, AChar, '\', '<']) do
      Inc(Run);
    case fLine[Run] of
    #39, '"':
      begin
        Inc(Run);
        fTokenID:=tkCssValString;
        Exit;
      end;
    '\':
      begin
        Inc(Run);
        if fLine[Run]=#0 then
        begin
          if fCss_Version=cvCss1 then
          begin
            fTokenID:=tkCssError;
            Exit;
          end else
          begin
            fTokenID:=tkCssValString;
            Result:=False;
            Exit;
          end;
        end else
          if not Css_CheckNull(False) and not Php_CheckBegin(False) then
            Inc(Run);
      end;
    else
      if Css_CheckNull(False) then
      begin
        fTokenID:=tkCssError;
        Exit;
      end else
        if Php_CheckBegin(False) then
        begin
          fTokenID:=tkCssValString;
          Result:=False;
          Exit;
        end else
          Inc(Run);
    end;
  until False;
end;

function TSynWebEngine.Css_NotWhitespace:Boolean;
begin
  Result:=False;
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  if fIdentTable[fLine[Run]] and (1 shl 11)<>0 then // if fLine[Run] in [#0..#32, '/'] then
    fCss_ProcTable[fLine[Run]]
  else
    Result:=True;
end;

procedure TSynWebEngine.Css_SymbolProc;
begin
  Inc(Run);
  fTokenID:=tkCssSymbol;
end;

procedure TSynWebEngine.Css_ErrorProc;
begin
  Inc(Run);
  fTokenID:=tkCssError;
end;

procedure TSynWebEngine.Css_RangeRulesetProc;
begin
  if GetRange_Bit(8) then
  begin
    SetRange_Bit(8, False);
    Css_IdentStartProc;
    fTokenID:=tkCssSelectorId;
  end else
    if GetRange_Bit(9) then
    begin  
      SetRange_Bit(9, False);   
      Css_IdentStartProc;
      fTokenID:=tkCssSelectorClass;
    end else
      fCss_ProcTable[fLine[Run]]; 
end;

procedure TSynWebEngine.Css_RangeSelectorAttribProc;

  procedure DoError;
  begin
    Css_SetRange(rsCssRuleset);
    fCss_ProcTable[fLine[Run]];
    fTokenID:=tkCssError;
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
        fTokenID:=tkCssVal;
        SetRange_Int(3, 8, 1);
      end else
        DoError;
  1:
    if Css_NotWhitespace then
      case fLine[Run] of
      '=':
        begin
          Css_SymbolProc;
          SetRange_Int(3, 8, 2);
        end;
      '|', '~':
        begin
          SetRange_Int(3, 8, 2);
          if fLine[Run+1]='=' then
          begin
            Inc(Run, 2);
            fTokenID:=tkCssSymbol;
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
      case fLine[Run] of
      #39:
        begin
          Inc(Run);
          if Css_CustomStringProc(TCssString39, False) then
            SetRange_Int(3, 8, 5)
          else
            SetRange_Int(3, 8, 3);
        end;
      '"':
        begin
          Inc(Run);
          if Css_CustomStringProc(TCssString34, False) then
            SetRange_Int(3, 8, 5)
          else
            SetRange_Int(3, 8, 4);
        end;
      else
        if Css_IdentStartProc then
        begin
          fTokenID:=tkCssValString;
          SetRange_Int(3, 8, 5);
        end else
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
      if fLine[Run]=']' then
        DoEndAttrib
      else
        DoError;
  end;
end;

procedure TSynWebEngine.Css_RangeSelectorPseudoProc;
var
  prop:Integer;
begin
  if not GetRange_Bit(10) then
  begin
    repeat
      Inc(Run);
    until fIdentTable[fLine[Run]] and (1 shl 0)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z']);
    prop:=Css_SpecialCheck(fTokenPos,Run-fTokenPos);
    if (prop=-1) or (TSynWeb_CssSpecialData[prop] and (1 shl (15-Longword(fCss_Version)))=0) then
    begin
      fTokenID:=tkCssError;
      Css_SetRange(rsCssRuleset);
    end else
      if (prop<>Css_SpecialID_Lang) then
      begin
        fTokenID:=tkCssSpecial;
        Css_SetRange(rsCssRuleset);
      end else
        if (fLine[Run]='(') then
        begin
          fTokenID:=tkCssSpecial;
          SetRange_Bit(10, True);
        end else
        begin
          fTokenID:=tkCssError;
          Css_SetRange(rsCssRuleset);
        end;
  end else
    if not GetRange_Bit(9) then
    begin
      Css_SymbolProc;
      SetRange_Bit(9, True);
    end else
      if Css_NotWhitespace then
        case fLine[Run] of
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
            Css_SetRange(rsCssRuleset);
          end;
        else
          if Css_IdentStartProc then
            if GetRange_Bit(8) then
              fTokenID:=tkCssError
            else
            begin
              fTokenID:=tkCssVal;
              SetRange_Bit(8, True);
            end
          else
          begin
            Css_SetRange(rsCssRuleset);
            fCss_ProcTable[fLine[Run]];
            fTokenID:=tkCssError;
          end;
        end;
end;

procedure TSynWebEngine.Css_RangeAtKeywordProc; 
var
  prop:Integer;

  procedure DoError;
  begin
    Css_SetRange(rsCssRuleset);
    fCss_ProcTable[fLine[Run]];
    fTokenID:=tkCssError;
  end;

  procedure AtImport;

    procedure AtImport_Medium(ASimple: Boolean);
    begin
      if Css_NotWhitespace then
        if not ASimple and (fLine[Run]=';') then
        begin
          Css_SymbolProc;
          Css_SetRange(rsCssRuleset);
        end else
          if Css_IdentStartProc then
          begin
            prop:=Css_SpecialCheck(fTokenPos,Run-fTokenPos);
            if (prop=-1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13)=0) then
              fTokenID:=tkCssValUndef
            else
              fTokenID:=tkCssVal;
            SetRange_Int(4, 4, 9);
          end else
            DoError;
    end;
    
  begin
    case GetRange_Int(4, 4) of
    0:
      if Css_NotWhitespace then
        case fLine[Run] of
        #39:
          begin
            Inc(Run);
            if Css_CustomStringProc(TCssString39, False) then
              SetRange_Int(4, 4, 8)
            else
              SetRange_Int(4, 4, 1);
          end;
        '"':
          begin
            Inc(Run);
            if Css_CustomStringProc(TCssString34, False) then
              SetRange_Int(4, 4, 8)
            else
              SetRange_Int(4, 4, 2);
          end;
        else
          if not Css_IdentStartProc then
            DoError
          else
            if (Css_SpecialCheck(fTokenPos, Run-fTokenPos)=Css_SpecialID_Url) and (fLine[Run]='(') then
            begin
              fTokenID:=tkCssVal;
              SetRange_Int(4, 4, 3);
            end else
            begin
              fTokenID:=tkCssValUndef;
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
      case fLine[Run] of
      #39:
        begin
          Inc(Run);
          if Css_CustomStringProc(TCssString39, False) then
            SetRange_Int(4, 4, 7)
          else
            SetRange_Int(4, 4, 5);
        end;
      '"':
        begin
          Inc(Run);
          if Css_CustomStringProc(TCssString34, False) then
            SetRange_Int(4, 4, 7)
          else
            SetRange_Int(4, 4, 6);
         end;
      #0..#32:
        Css_SpaceProc;
      else
        if (fLine[Run]='/') and (fLine[Run+1]='*') then      
          Css_SlashProc
        else
        begin
          if Css_CheckNull or Php_CheckBegin then
            Exit;
          repeat
            while fIdentTable[fLine[Run]] and (1 shl 14)=0 do // while not (fLine[Run] in [#0..#32, '(', ')', ',', '\', '<']) do
              Inc(Run);
            case fLine[Run] of
            '\':
              begin
                Inc(Run);
                if fLine[Run]<>#0 then
                  Inc(Run)
                else
                  Break;
              end;
            '<':
              if Css_CheckNull(False) or Php_CheckBegin(False) then
                Break
              else
                Inc(Run);
            else
              Break;
            end;
          until False;
          fTokenID:=tkCssValString;
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
        if fLine[Run]=')' then
        begin
          Css_SymbolProc;
          SetRange_Int(4, 4, 8);
        end else
          DoError;
    8:
      AtImport_Medium(False);
    9:
      if Css_NotWhitespace then
        case fLine[Run] of
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
    prop:Integer;
  begin
    if Css_NotWhitespace then                     
      if GetRange_Bit(7) then
      begin
        SetRange_Bit(7, False);
        case fLine[Run] of
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
      end else
        if Css_IdentStartProc then
        begin
          prop:=Css_SpecialCheck(fTokenPos,Run-fTokenPos);
          if (prop=-1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13)=0) then
            fTokenID:=tkCssValUndef
          else
            fTokenID:=tkCssVal;
          SetRange_Bit(7, True);
        end else
          DoError;
  end;

  procedure AtPage;             
  var
    prop:Integer;

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
        case fLine[Run] of
        '{':
          AtPage_Declaration;
        ':':
          if (fIdentTable[fLine[Run+1]] and (1 shl 8)=0) or // if not (fLine[Run] in ['a'..'z', 'A'..'Z', '\']) or
            ((fLine[Run+1]='\') and (fLine[Run+2] in [#0..#31])) then
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
        prop:=Css_SpecialCheck(fTokenPos,Run-fTokenPos);
        if (prop=-1) or (TSynWeb_CssSpecialData[prop] and (1 shl 11)=0) then
          fTokenID:=tkCssError
        else
          fTokenID:=tkCssSpecial;
        SetRange_Int(2, 6, 2);
      end;
    2:
      if Css_NotWhitespace then
        if fLine[Run]='{' then
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
        case fLine[Run] of
        #39:
          begin
            Inc(Run);
            if Css_CustomStringProc(TCssString39, False) then
              SetRange_Int(2, 6, 3)
            else
              SetRange_Bit(6, True);
          end;
        '"':
          begin
            Inc(Run);
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
        if fLine[Run]=';' then
        begin
          Css_SymbolProc;
          Css_SetRange(rsCssRuleset);
        end else
          DoError;
    end;
  end;

begin
  if not GetRange_Bit(10) then
  begin
    SetRange_Bit(10, True);
    repeat
      Inc(Run);
    until fIdentTable[fLine[Run]] and (1 shl 0)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z']);
    if GetRange_Bit(11) then
    begin
      fTokenID:=tkCssError;
      Css_SetRange(rsCssRuleset);
    end else
      case Css_SpecialCheck(fTokenPos,Run-fTokenPos) of
      Css_SpecialID_Import:
        begin
          SetRange_Int(2, 8, 0);
          fTokenID:=tkCssSpecial;
        end;
      Css_SpecialID_Media:
        if fCss_Version=cvCss1 then
        begin
          fTokenID:=tkCssError;
          Css_SetRange(rsCssRuleset);
        end else
        begin
          SetRange_Int(2, 8, 1);
          fTokenID:=tkCssSpecial;
        end;
      Css_SpecialID_Page:
        if fCss_Version=cvCss1 then
        begin
          fTokenID:=tkCssError;
          Css_SetRange(rsCssRuleset);
        end else
        begin
          SetRange_Int(2, 8, 2);
          fTokenID:=tkCssSpecial;
        end;
      Css_SpecialID_Charset:  
        if fCss_Version=cvCss1 then
        begin
          fTokenID:=tkCssError;
          Css_SetRange(rsCssRuleset);
        end else
        begin
          SetRange_Int(2, 8, 3);
          fTokenID:=tkCssSpecial;
        end;
      else
        fTokenID:=tkCssError;
        Css_SetRange(rsCssRuleset);
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
      case fLine[Run] of
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
  end else
    if Css_NotWhitespace then
      if Css_IdentStartProc then
      begin
        fTokenID:=Css_PropCheck;
        SetRange_Bit(8, True);
      end else
      begin
        Css_SetProp(0);
        case fLine[Run] of
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
  if fIdentTable[fLine[Run]] and (1 shl 12)<>0 then // if fLine[Run] in [#0..#32, '/', '#', '!', ';', '}', '+', '-', '0'..'9', '.', ',', '"', #39, '<'] then
    fCss_ProcTable[fLine[Run]]
  else
    if Css_IdentStartProc then
    begin
      fTokenID:=Css_ValCheck;
      if TSynWeb_CssValsData[fToken_LastID][Longword(fCss_Version)][3] and (1 shl 31)<>0 then
        if fLine[Run]='(' then
        begin
          SetRange_Int(3, 8, 0);
          case fToken_LastID of
          Css_ValID_Rgb:
            Css_SetRange(rsCssPropValRgb);
          Css_ValID_Url:
            Css_SetRange(rsCssPropValUrl);
          Css_ValID_Rect:                 
            Css_SetRange(rsCssPropValRect);
          else
            Css_SetRange(rsCssPropValFunc);
          end;
        end else
          fTokenID:=tkCssValUndef;
    end else
      Css_ErrorProc;
end;

procedure TSynWebEngine.Css_RangePropValStrProc;  
var
  prop:Integer;
begin
  if GetRange_Bit(8) then
  begin
    if Css_CustomStringProc(TCssString39) then
    begin
      Css_SetRange(rsCssPropVal);
      SetRange_Bit(8, False);
    end;
  end else
    if Css_CustomStringProc(TCssString34) then
    begin
      Css_SetRange(rsCssPropVal);
      SetRange_Bit(9, False);
    end;
  if fTokenID=tkCssValString then
  begin
    prop:=Css_GetProp-1;
    if (prop=-1) or (TSynWeb_CssPropsData[prop] and (1 shl 19)=0) then
      fTokenID:=tkCssValUndef;
  end;
end;

procedure TSynWebEngine.Css_RangePropValRgbProc;

  procedure NumberProc;
  begin
    if GetRange_Bit(8) then
      fTokenID:=tkCssError
    else
      fTokenID:=tkCssValNumber;
    SetRange_Bit(8, True);
    if fLine[Run]='+' then
      if fLine[Run+1] in ['0'..'9', '.'] then
        Inc(Run)
      else
      begin
        Css_ErrorProc;
        Exit;
      end;
    while fLine[Run] in ['0'..'9'] do
      Inc(Run);
    if fLine[Run]='.' then
    begin
      Inc(Run);
      if fLine[Run] in ['0'..'9'] then
      begin
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9']);
        if fLine[Run]='%' then
          Exit;
      end;
      fTokenID:=tkCssError;
    end;
  end;

begin
  if GetRange_Bit(10) then
  begin
    if Css_NotWhitespace then
      case fLine[Run] of
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
        if (Run>0) and (fLine[Run-1] in ['0'..'9']) then
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
        fTokenID:=tkCssError;
      end;
  end else
  begin
    Css_SymbolProc;
    SetRange_Bit(10, True);
  end;
end;

procedure TSynWebEngine.Css_RangePropValSpecialProc;
var
  prop:Integer;
begin
  if fLine[Run]='%' then
    Css_SymbolProc
  else
    if (Run>0) and (fLine[Run-1]='#') then
    begin
      Inc(Run,3);
      if (fIdentTable[fLine[Run]] and (1 shl 10)<>0) and // if (fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9']) and
         (fIdentTable[fLine[Run+1]] and (1 shl 10)<>0) and // if (fLine[Run+1] in ['a'..'f', 'A'..'F', '0'..'9']) and
         (fIdentTable[fLine[Run+2]] and (1 shl 10)<>0) then // if (fLine[Run+2] in ['a'..'f', 'A'..'F', '0'..'9']) then
       Inc(Run,3);
      prop:=Css_GetProp-1;
      if (prop=-1) or (TSynWeb_CssPropsData[prop] and (1 shl 18)=0) then
        fTokenID:=tkCssValUndef
      else
        fTokenID:=tkCssValNumber;
    end else
    begin
      Css_IdentStartProc;
      fTokenID:=tkCssSymbol;
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
    end else
      Css_ErrorProc;
  end;

begin
  if Css_NotWhitespace then
  begin
    case fLine[Run] of
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
          fTokenID:=tkCssError
        else
        begin
          Css_SpecialCheck(fTokenPos, Run-fTokenPos);
          if fToken_LastID=Css_SpecialID_Important then
            fTokenID:=tkCssSpecial
          else
            fTokenID:=tkCssError;
          SetRange_Bit(8, True);
        end;
      end else
        Css_ErrorProc;
    end;
  end;
end;

procedure TSynWebEngine.Css_RangePropValUrlProc;
begin
  if GetRange_Bit(10) then
    case GetRange_Int(2, 8) of
    0:
      case fLine[Run] of
      #39:
        begin
          Inc(Run);
          if Css_CustomStringProc(TCssString39, False) then
            SetRange_Int(2, 8, 3)
          else
            SetRange_Bit(8, True);
        end;
      '"':
        begin
          Inc(Run);
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
        if (fLine[Run]='/') and (fLine[Run+1]='*') then
          Css_SlashProc
        else
        begin       
          if Css_CheckNull or Php_CheckBegin then
            Exit;
          repeat
            while fIdentTable[fLine[Run]] and (1 shl 14)=0 do // while not (fLine[Run] in [#0..#32, '(', ')', ',', '\', '<']) do
              Inc(Run);
            case fLine[Run] of
            '\':
              begin
                Inc(Run);
                if fLine[Run]<>#0 then
                  Inc(Run)
                else
                  Break;
              end;
            '<':
              if Css_CheckNull(False) or  Php_CheckBegin(False) then
                Break
              else
                Inc(Run);
            else
              Break;
            end;
          until False;
          fTokenID:=tkCssValString;
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
      if fLine[Run]=')' then
      begin
        Css_SymbolProc;          
        SetRange_Int(3, 8, 0);
        Css_SetRange(rsCssPropVal);
      end else
        if Css_NotWhitespace then
        begin
          SetRange_Int(3, 8, 0);
          Css_SetRange(rsCssPropVal);
          Css_RangePropValProc;
          fTokenID:=tkCssError;
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
    prop, OldRun:Integer;

    procedure CheckOther;
    begin
      if GetRange_Bit(8) then
        fTokenID:=tkCssError
      else
        if (Run-fTokenPos=1) and (fLine[Run-1]='0') then
          fTokenID:=tkCssValNumber
        else
          fTokenID:=tkCssValUndef;
      SetRange_Bit(8, True);
    end;
    
  begin       
    while fLine[Run] in ['0'..'9'] do
      Inc(Run);
    if fLine[Run]='.' then
    begin
      Inc(Run);
      if fLine[Run] in ['0'..'9'] then
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9'])
      else
      begin
        fTokenID:=tkCssError;
        Exit;
      end;
    end;
    OldRun:=Run;
    if Css_IdentStartProc then
    begin
      prop:=Css_SpecialCheck(OldRun,Run-OldRun);
      if prop<>-1 then 
      begin
        Run:=OldRun;
        SetRange_Bit(9, True);
        if (TSynWeb_CssSpecialData[prop] and (1 shl 28)=0) or GetRange_Bit(8) then
          fTokenID:=tkCssError
        else
          fTokenID:=tkCssValNumber;
      end else
        if fCss_Version=cvCss1 then
        begin
          Run:=OldRun;
          CheckOther;
        end else
        begin
          fTokenID:=tkCssError;
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
        fTokenID:=tkCssError
      else
        fTokenID:=tkCssSymbol;
      SetRange_Bit(9, False);
      SetRange_Bit(8, True);
    end else
      if Css_NotWhitespace then
        case fLine[Run] of
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
              fTokenID:=tkCssError
            else
              if Css_SpecialCheck(fTokenPos, Run-fTokenPos)=Css_SpecialID_Auto then
                fTokenID:=tkCssVal
              else
                fTokenID:=tkCssValUndef;
            SetRange_Bit(8, True);
          end;
        end;
end;

procedure TSynWebEngine.Css_RangePropValFuncProc;
begin
  if GetRange_Bit(10) then
    case GetRange_Int(2, 8)  of
    0:
      if Css_NotWhitespace then
        case fLine[Run] of
        #39:
          begin
            Inc(Run);
            if not Css_CustomStringProc(TCssString39, False) then
              SetRange_Bit(8, True);
          end;
        '"':
          begin
            Inc(Run);
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
            fTokenID:=tkCssVal
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

procedure TSynWebEngine.Css_RangeCommentProc;
begin
  if Css_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 26)=0 do // while not (fLine[Run] in [#0, '*', '<']) do
      Inc(Run);
    case fLine[Run] of
    #0:
      Break;
    '<':
      if Css_CheckNull(False) or Php_CheckBegin(False) then
        Break
      else
        Inc(Run);
    '*':
      begin
        Inc(Run);
        if fLine[Run]='/' then
        begin
          Inc(Run);
          SetRange_Bit(12, False);
          Break;
        end;
      end;
    end;
  until False;
  fTokenID:=tkCssComment;
end;

function TSynWebEngine.Css_PropKeyComp(const ID: Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
begin
  aKey:=TSynWeb_CssProps[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLenClean then
  begin
    if fStringLenClean=fStringLen then
      for i:=1 to fStringLen do
      begin
        if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
        begin
          Result:=False;
          Exit;
        end;
        Inc(Temp);
      end
    else
      for i:=1 to fStringLenClean do
      begin
        if Temp^='\' then
          Inc(Temp);
        if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
        begin
          Result:=False;
          Exit;
        end;
        Inc(Temp);
      end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Css_PropCheck: TtkTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=Run-fTokenPos;
    for i:=0 to fStringLen-1 do
    begin     
      inc(HashKey, fInsensitiveHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[fTokenPos];
  KeyHash(fToIdent);
  fToken_LastID:=-1;
  if HashKey<=Css_PropMaxKeyHash then
  begin
    Result:=fCss_PropIdentFuncTable[HashKey];
    if (fToken_LastID<>-1) and (TSynWeb_CssPropsData[fToken_LastID] and (1 shl Longword(fCss_Version))=0) then
      Result:=tkCssPropUndef;
  end else
    Result:=tkCssPropUndef;
  Css_SetProp(fToken_LastID+1);
end;

{$I SynHighlighterWeb_CssPropsFunc.inc}

function TSynWebEngine.Css_ValKeyComp(const ID: Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
begin
  aKey:=TSynWeb_CssVals[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLenClean then
  begin
    if fStringLenClean=fStringLen then
      for i:=1 to fStringLen do
      begin
        if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
        begin
          Result:=False;
          Exit;
        end;
        Inc(Temp);
      end
    else
      for i:=1 to fStringLenClean do
      begin
        if Temp^='\' then
          Inc(Temp);
        if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
        begin
          Result:=False;
          Exit;
        end;
        Inc(Temp);
      end;       
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Css_ValCheck: TtkTokenKind;
var
  HashKey: Longword;
  prop:Integer;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=Run-fTokenPos;
    for i:=0 to fStringLen-1 do
    begin     
      inc(HashKey, fInsensitiveHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[fTokenPos];
  KeyHash(fToIdent);
  fToken_LastID:=-1;
  if HashKey<=Css_ValMaxKeyHash then
  begin 
    Result:=fCss_ValIdentFuncTable[HashKey];
    if Result=tkCssVal then
    begin
      prop:=Css_GetProp-1;
      if (prop=-1) or (TSynWeb_CssValsData[fToken_LastID][Longword(fCss_Version)][prop div 32] and (1 shl (prop mod 32))=0) then
        Result:=tkCssValUndef;
    end;
  end else
    Result:=tkCssValUndef;
  if Result=tkCssValUndef then
  begin
    prop:=Css_GetProp-1;
    if (prop<>-1) and (TSynWeb_CssPropsData[prop] and (1 shl 20)<>0) then
      Result:=tkCssSymbol;
  end;
end;

{$I SynHighlighterWeb_CssValsFunc.inc}

function TSynWebEngine.Css_SpecialKeyComp(const ID: Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
begin
  aKey:=TSynWeb_CssSpecial[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Css_SpecialCheck(AStart, ALen:Integer): Integer;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=ALen;
    for i:=0 to ALen-1 do
    begin
      inc(HashKey, fInsensitiveHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[AStart];
  KeyHash(fToIdent);
  if (HashKey>Css_SpecialMaxKeyHash) or not fCss_SpecialIdentFuncTable[HashKey] then
    fToken_LastID:=-1;
  Result:=fToken_LastID;
end;

{$I SynHighlighterWeb_CssSpecialFunc.inc}

// ECMAScript ------------------------------------------------------------------

procedure TSynWebEngine.ES_MakeMethodTables;
var
  c:Char;
  i:Integer;
  pF:PIdentFuncTableFunc;
begin
  for c:=#0 to #255 do
    case c of
      #0: fES_ProcTable[c]:=NullProc;
      #1..#32: fES_ProcTable[c]:=ES_SpaceProc;
      '/': fES_ProcTable[c]:=ES_SlashProc;
      '<': fES_ProcTable[c]:=ES_LowerProc;
      '=', '!': fES_ProcTable[c]:=ES_EqualNotProc;
      '>': fES_ProcTable[c]:=ES_GreaterProc;   
      '&': fES_ProcTable[c]:=ES_AndProc;
      '+': fES_ProcTable[c]:=ES_PlusProc;
      '-': fES_ProcTable[c]:=ES_MinusProc;
      '|': fES_ProcTable[c]:=ES_OrProc;
      '*', '%', '^': fES_ProcTable[c]:=ES_MulModXorProc;
      '0'..'9': fES_ProcTable[c]:=ES_NumberProc;
      '"': fES_ProcTable[c]:=ES_String34Proc;
      #39: fES_ProcTable[c]:=ES_String39Proc;
      '{', '}', '[', ']', '(', ')', '.', ';', ',', '?', ':', '~': fES_ProcTable[c]:=ES_SymbolProc;
      '$', 'a'..'z', 'A'..'Z', '_': fES_ProcTable[c]:=ES_IdentProc;
    else
      fES_ProcTable[c]:=ES_ErrorProc;
    end;

  fES_RangeProcTable[rsESDefault]:=ES_RangeDefaultProc;
  fES_RangeProcTable[rsESComment]:=ES_RangeCommentProc;
  fES_RangeProcTable[rsESCommentMulti]:=ES_RangeCommentMultiProc;
  fES_RangeProcTable[rsESString34]:=ES_RangeString34Proc;
  fES_RangeProcTable[rsESString39]:=ES_RangeString39Proc;

  pF:=PIdentFuncTableFunc(@fES_IdentFuncTable);
  for I:=Low(fES_IdentFuncTable) to High(fES_IdentFuncTable) do
  begin
    pF^:=ES_KeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_ESKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.ES_Next;  
begin
  fTokenPos:=Run;
  fES_RangeProcTable[ES_GetRange];
end;

function TSynWebEngine.ES_GetRange:TESRangeState;                 
begin
  Result:=TESRangeState(GetRange_Int(2, 15));
end;

procedure TSynWebEngine.ES_SetRange(const ARange:TESRangeState);   
begin
  SetRange_Int(2, 15, Longword(ARange));
end;

function TSynWebEngine.ES_CheckNull(ADo:Boolean=True):Boolean;
begin
  case fLine[Run] of
  #0:
    begin
      Result:=True;
      if ADo then
        NullProc;
    end;
  '<':
    if (fLine[Run+1]='/') and
       (fHashTable[fLine[Run+2]]=fHashTable['s']) and
       (fHashTable[fLine[Run+3]]=fHashTable['c']) and
       (fHashTable[fLine[Run+4]]=fHashTable['r']) and
       (fHashTable[fLine[Run+5]]=fHashTable['i']) and
       (fHashTable[fLine[Run+6]]=fHashTable['p']) and
       (fHashTable[fLine[Run+7]]=fHashTable['t']) and
       (fIdentTable2[fLine[Run+8]] and (1 shl 0)<>0) and // (fLine[Run+8] in [#0..#32, '>']) and
       (fHighlighterMode in [shmHtml, shmPhp]) then
    begin
      Result:=True;
      if ADo then
      begin
        fTokenID:=tkHtmlTag;
        SetHighligterType(shtHtml, True, False, False);
      end;
    end else
      Result:=False;
  else
    Result:=False;
  end;
end;

procedure TSynWebEngine.ES_SpaceProc;
begin
  repeat
    Inc(Run);
  until not(fLine[Run] in [#1..#32]);
  fTokenID:=tkESSpace;
end;

procedure TSynWebEngine.ES_SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
  '*':
    begin
      Inc(Run);
      ES_SetRange(rsESCommentMulti);
      if ES_CheckNull(False) or Php_CheckBegin(False) then
        fTokenID:=tkESComment
      else
        ES_RangeCommentMultiProc;
      Exit;
    end;
  '=':
    Inc(Run);
  '/':
    begin
      Inc(Run);
      ES_SetRange(rsESComment);
      if ES_CheckNull(False) or Php_CheckBegin(False) then
        fTokenID:=tkESComment
      else
        ES_RangeCommentProc;
      Exit;
    end;
  end;
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_LowerProc;
begin
  if ES_CheckNull or Php_CheckBegin then
    Exit;
  Inc(Run);
  case fLine[Run] of
  '=':
    Inc(Run);
  '<':
    begin
      Inc(Run);
      if fLine[Run]='=' then
        Inc(Run);
    end;
  end;
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_EqualNotProc;
begin
  Inc(Run);
  if fLine[Run]='=' then
  begin
    Inc(Run);
    if fLine[Run]='=' then
      Inc(Run);
  end;
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_GreaterProc;
begin
  Inc(Run);
  case fLine[Run] of
  '=':
    Inc(Run);
  '>':
    begin
      Inc(Run);
      case fLine[Run] of
      '=':
        Inc(Run);
      '>':
        begin
          Inc(Run);
          if fLine[Run]='=' then
            Inc(Run);
        end;
      end;
    end;
  end;
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_AndProc;
begin
  Inc(Run);
  if fLine[Run] in ['=', '&'] then
    Inc(Run);
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_PlusProc;
begin
  Inc(Run);
  if fLine[Run] in ['=', '+'] then
    Inc(Run);
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_MinusProc;
begin
  Inc(Run);
  if fLine[Run] in ['=', '-'] then
    Inc(Run);
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_OrProc;
begin
  Inc(Run);
  if fLine[Run] in ['=', '|'] then
    Inc(Run);
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_MulModXorProc;
begin
  Inc(Run);
  if fLine[Run]='=' then
    Inc(Run);
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_NumberProc;
begin
  fTokenID:=tkESError;
  if (fLine[Run]='0') and (fLine[Run+1] in ['x', 'X']) then
  begin
    Inc(Run, 2);
    if fIdentTable[fLine[Run]] and (1 shl 10)<>0 then // if fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9'] then
      repeat
        Inc(Run);
      until fIdentTable[fLine[Run]] and (1 shl 10)=0 // until not (fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end else
  begin
    while fLine[Run] in ['0'..'9'] do
      Inc(Run);
    if fLine[Run]='.' then
    begin
      Inc(Run);
      if fLine[Run] in ['0'..'9'] then
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9'])
      else
        Exit;
    end;
    if fLine[Run] in ['e', 'E'] then
    begin
      Inc(Run);
      if fLine[Run] in ['+', '-'] then
        Inc(Run);
      if fLine[Run] in ['0'..'9'] then
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  fTokenID:=tkESNumber;
end;

procedure TSynWebEngine.ES_String34Proc;
begin   
  Inc(Run);
  if ES_CheckNull(False) then
    fTokenID:=tkESError
  else
  begin
    ES_SetRange(rsESString34);
    if Php_CheckBegin(False) then
      fTokenID:=tkESString
    else
      ES_RangeString34Proc;
  end;
end;

procedure TSynWebEngine.ES_String39Proc;
begin   
  Inc(Run);
  if ES_CheckNull(False) then
    fTokenID:=tkESError
  else
  begin
    ES_SetRange(rsESString39);
    if Php_CheckBegin(False) then
      fTokenID:=tkESString
    else
      ES_RangeString39Proc;
  end;
end;

procedure TSynWebEngine.ES_SymbolProc;
begin
  Inc(Run);
  fTokenID:=tkESSymbol;
end;

procedure TSynWebEngine.ES_IdentProc;
begin
  repeat
    Inc(Run);
  until fIdentTable2[fLine[Run]] and (1 shl 2)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$']);
  fTokenID:=ES_IdentCheck;
end;

procedure TSynWebEngine.ES_ErrorProc;
begin
  Inc(Run);
  fTokenID:=tkESError;
end;

procedure TSynWebEngine.ES_RangeDefaultProc;
begin
  fES_ProcTable[fLine[Run]];
end;

procedure TSynWebEngine.ES_RangeCommentProc;
begin
  if not ES_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        repeat
          Inc(Run);
        until fLine[Run] in [#0, '<'];
        case fLine[Run] of
        #0:
          begin
            fTokenID:=tkESComment;
            Break;
          end;
        '<':
          if Php_CheckBegin(False) then
          begin
            fTokenID:=tkESComment;
            Exit;
          end else
            if ES_CheckNull(False) then
            begin
              fTokenID:=tkESComment;
              Break;
            end else
              Inc(Run);
        end;
      until False;
  ES_SetRange(rsESDefault);
end;

procedure TSynWebEngine.ES_RangeCommentMultiProc;
begin
  if ES_CheckNull or Php_CheckBegin then
    Exit;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 26)=0 do // while not (fLine[Run] in [#0, '*', '<']) do
      Inc(Run);
    case fLine[Run] of
    #0:
      Break;
    '<':
      if ES_CheckNull(False) or Php_CheckBegin(False) then
        Break
      else
        Inc(Run);
    '*':
      begin
        Inc(Run);
        if fLine[Run]='/' then
        begin
          Inc(Run);
          ES_SetRange(rsESDefault);
          Break;
        end;
      end;
    end;
  until False;
  fTokenID:=tkESComment;
end;

procedure TSynWebEngine.ES_RangeString34Proc;
begin
  if not Html_CheckNull then
    if Php_CheckBegin then
      Exit
    else
      repeat
        while fIdentTable2[fLine[Run]] and (1 shl 3)=0 do // while not (fLine[Run] in [#0, #34, '<', '\']) do
          Inc(Run);
        case fLine[Run] of
        #0:
          begin
            fTokenID:=tkESError;
            Break;
          end;
        '<':
          if Php_CheckBegin(False) then
          begin
            fTokenID:=tkESString;
            Exit;
          end else
            Inc(Run);
        #34:
          begin
            Inc(Run);
            fTokenID:=tkESString;
            Break;
          end;
        '\':
          begin
            Inc(Run);
            if fLine[Run]=#34 then
              Inc(Run);
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
        while fIdentTable2[fLine[Run]] and (1 shl 4)=0 do // while not (fLine[Run] in [#0, #39, '<', '\']) do
          Inc(Run);
        case fLine[Run] of
        #0:
          begin
            fTokenID:=tkESError;
            Break;
          end;
        '<':
          if Php_CheckBegin(False) then
          begin
            fTokenID:=tkESString;
            Exit;
          end else
            Inc(Run);
        #39:
          begin
            Inc(Run);
            fTokenID:=tkESString;
            Break;
          end;  
        '\':
          begin
            Inc(Run);
            if fLine[Run]=#39 then
              Inc(Run);
          end;
        end;
      until False;
  ES_SetRange(rsESDefault);
end;

function TSynWebEngine.ES_KeywordComp(const ID:Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
begin
  aKey:=TSynWeb_ESKeywords[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.ES_IdentCheck: TtkTokenKind; 
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=Run-fTokenPos;
    for i:=0 to fStringLen-1 do
    begin     
      inc(HashKey, fInsensitiveHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[fTokenPos];
  KeyHash(fToIdent);
  fToken_LastID:=-1;
  if HashKey<=ES_KeywordsMaxKeyHash then
    Result:=fES_IdentFuncTable[HashKey]
   else
    Result:=tkESIdentifier;
end;

{$I SynHighlighterWeb_ESKeywordsFunc.inc}

// PHP -------------------------------------------------------------------------

procedure TSynWebEngine.Php_MakeMethodTables;
var
  c:Char;
  i:Integer;
  pF:PIdentFuncTableFunc;
begin
  for c:=#0 to #255 do
    case c of
      #0: fPhp_ProcTable[c]:=NullProc;
      #1..#32: fPhp_ProcTable[c]:=Php_SpaceProc;
      '?': fPhp_ProcTable[c]:=Php_QuestionProc;
      '0'..'9': fPhp_ProcTable[c]:=Php_NumberProc;
      '"': fPhp_ProcTable[c]:=Php_String34Proc;
      #39: fPhp_ProcTable[c]:=Php_String39Proc;
      '`': fPhp_ProcTable[c]:=Php_StringShellProc;
      '&': fPhp_ProcTable[c]:=Php_AndProc;
      '|': fPhp_ProcTable[c]:=Php_OrProc;
      '@': fPhp_ProcTable[c]:=Php_AtSymbolProc;
      '=': fPhp_ProcTable[c]:=Php_EqualProc;
      '>': fPhp_ProcTable[c]:=Php_GreaterProc;
      '<': fPhp_ProcTable[c]:=Php_LowerProc;
      '+': fPhp_ProcTable[c]:=Php_PlusProc;
      '-': fPhp_ProcTable[c]:=Php_MinusProc;
      '*', '^': fPhp_ProcTable[c]:=Php_MulDivModXorProc;
      '/': fPhp_ProcTable[c]:=Php_SlashProc;
      '%': fPhp_ProcTable[c]:=Php_PercentProc;
      '#': fPhp_ProcTable[c]:=Php_HashProc;
      '!': fPhp_ProcTable[c]:=Php_NotProc;
      '.': fPhp_ProcTable[c]:=Php_DotProc;
      '{', '}', '[', ']', '(', ')', '~', ',', ';', ':': fPhp_ProcTable[c]:=Php_SymbolProc;
      '$': fPhp_ProcTable[c]:=Php_VarProc;
      'a'..'z', 'A'..'Z', '_', #$7F..#$FF: fPhp_ProcTable[c]:=Php_IdentProc;
    else
      fPhp_ProcTable[c]:=Php_ErrorProc;
    end;
    
  fPhp_RangeProcTable[rsPhpSubProc]:=Php_RangeTagProc;
  fPhp_RangeProcTable[rsPhpDefault]:=Php_RangeDefaultProc;
  fPhp_RangeProcTable[rsPhpComment]:=Php_RangeCommentProc;
  fPhp_RangeProcTable[rsPhpString34]:=Php_RangeString34Proc;
  fPhp_RangeProcTable[rsPhpString39]:=Php_RangeString39Proc;
  fPhp_RangeProcTable[rsPhpStringShell]:=Php_RangeStringShellProc;
  fPhp_RangeProcTable[rsPhpHeredoc]:=Php_RangeHeredocProc;

  pF:=PIdentFuncTableFunc(@fPhp_IdentFuncTable);
  for I:=Low(fPhp_IdentFuncTable) to High(fPhp_IdentFuncTable) do
  begin
    pF^:=Php_KeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_PhpKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.Php_Next;
begin
  fTokenPos:=Run;
  if fLine[Run]=#0 then
    NullProc
  else
    fPhp_RangeProcTable[Php_GetRange];
end;

function TSynWebEngine.Php_GetRange: TPhpRangeState;
begin
  if GetRange_Bit(26) then
    Result:=rsPhpHeredoc
  else
    Result:=TPhpRangeState(GetRange_Int(3, 23));
end;

procedure TSynWebEngine.Php_SetRange(const ARange: TPhpRangeState);
begin
  if ARange=rsPhpHeredoc then
    SetRange_Bit(26, True)
  else
  begin
    SetRange_Bit(26, False);
    SetRange_Int(3, 23, Longword(ARange));
  end;
end;

function TSynWebEngine.Php_GetOpenTag:TPhpOpenTag;
begin
  Result:=TPhpOpenTag(GetRange_Int(2, 27));
end;

procedure TSynWebEngine.Php_SetOpenTag(APhpOpenTag:TPhpOpenTag);
begin
  SetRange_Int(2, 27, Longword(APhpOpenTag));
end;

procedure TSynWebEngine.SetPhp_Version(const Value: TPhpVersion);
begin
  fPhp_Version:=Value;
  DefHighlightChange(Self);
end;

procedure TSynWebEngine.SetPhpAspTags(const Value: Boolean);
begin
  fPhpAspTags:=Value;
  DefHighlightChange(Self);
end;

procedure TSynWebEngine.SetPhpShortOpenTag(const Value: Boolean);
begin
  fPhpShortOpenTag:=Value;
  DefHighlightChange(Self);
end;

function TSynWebEngine.Php_CheckBegin(ABegin:Boolean):Boolean;
begin
  Result:=False;
  if (fLine[Run]='<') and (fHighlighterMode=shmPhp) then
    case fLine[Run+1] of
    '?':
      if  (UpCase(fLine[Run+2])='P') and
          (UpCase(fLine[Run+3])='H') and
          (UpCase(fLine[Run+4])='P') and
          (fIdentTable[fLine[Run+5]] and (1 shl 29)=0) then // not (fLine[Run+5] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]) then
      begin
        if ABegin then
          Php_Begin(potPhp);
      end else
        if fPhpShortOpenTag then
        begin    
          if ABegin then
            Php_Begin(potPhpShort)
        end else
          Exit;
    '%':
      if fPhpAspTags then
      begin
        if ABegin then
          Php_Begin(potASP);
      end else
        Exit;
    else
      Exit;
    end
  else
    Exit;
  Result:=True;
end;

procedure TSynWebEngine.Php_Begin(ATagKind:TPhpOpenTag);
begin
  SetHighligterType(
    TSynHighlighterType(Longword(fHighlighterType)+Longword(shtPHP_inHtml)),
    False,
    True,
    ATagKind=potHtml);
  SetRange_Int(12, 17, 0);
  Php_SetOpenTag(ATagKind);
  if ATagKind=potHTML then
    Php_SetRange(rsPhpDefault)
  else
    Next;
end;

procedure TSynWebEngine.Php_End;
var
  t:TPhpOpenTag;
begin
  t:=Php_GetOpenTag;
  SetRange_Int(12, 17, 0);
  if fLine[Run]=#0 then
    SetRange_Int(3, 29, Longword(fHighlighterType)-Longword(shtPHP_inHtml))
  else
  begin
    SetHighligterType(
      TSynHighlighterType(Longword(fHighlighterType)-Longword(shtPHP_inHtml)),
      t=potHTML,
      True,
      t<>potHTML);
    if t=potHTML then
      Next;
  end;
end;

procedure TSynWebEngine.Php_SpaceProc;
begin
  repeat
    Inc(Run);
  until not(fLine[Run] in [#1..#32]);
  fTokenID:=tkPhpSpace;
end;

procedure TSynWebEngine.Php_QuestionProc;
begin
  Inc(Run);
  if (fLine[Run]='>') and (fHighlighterMode=shmPhp) then
  begin
    Inc(Run);
    if Php_GetOpenTag in [potPhp, potPhpShort] then
    begin
      fTokenID:=tkHtmlTag;
      Php_End;
    end else
      fTokenID:=tkPhpError;
  end else
    fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_NumberProc;
begin
  if Php_CheckNumberProc then
    fTokenID:=tkPhpNumber
  else
    fTokenID:=tkPhpError;
end;

function TSynWebEngine.Php_CheckNumberProc:Boolean;
begin
  Result:=False;
  if (fLine[Run]='0') and (fLine[Run+1]='x') then
  begin
    Inc(Run, 2);
    if fIdentTable[fLine[Run]] and (1 shl 10)<>0 then // if fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9'] then
      repeat
        Inc(Run);
      until fIdentTable[fLine[Run]] and (1 shl 10)=0 // until not (fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end else
  begin
    while fLine[Run] in ['0'..'9'] do
      Inc(Run);
    if fLine[Run]='.' then
    begin
      Inc(Run);
      if fLine[Run] in ['0'..'9'] then
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9'])
      else
        Exit;
    end;
    if fLine[Run] in ['e', 'E'] then
    begin
      Inc(Run);
      if fLine[Run] in ['+', '-'] then
        Inc(Run);
      if fLine[Run] in ['0'..'9'] then
        repeat
          Inc(Run);
        until not (fLine[Run] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  Result:=True;
end;

procedure TSynWebEngine.Php_String34Proc;
begin
  Inc(Run);
  Php_SetRange(rsPhpString34);
  if fIdentTable[fLine[Run]] and (1 shl 30)<>0 then // if fLine[Run] in [#0, '\', '{', '$'] then
    fTokenID:=tkPhpString
  else
    Php_RangeString34Proc;
end;

procedure TSynWebEngine.Php_String39Proc;
begin
  Inc(Run);
  Php_SetRange(rsPhpString39);
  if fLine[Run] in [#0, '\'] then
    fTokenID:=tkPhpString
  else
    Php_RangeString39Proc;
end;

procedure TSynWebEngine.Php_StringShellProc;
begin
  Inc(Run);
  Php_SetRange(rsPhpStringShell);
  if fLine[Run] in [#0, '`'] then
    fTokenID:=tkPhpString
  else
    Php_RangeStringShellProc;
end;

procedure TSynWebEngine.Php_AndProc;
begin
  Inc(Run);
  if fLine[Run] in ['=', '&'] then
    Inc(Run);
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_OrProc;
begin
  Inc(Run);
  if fLine[Run] in ['=', '|'] then
    Inc(Run);
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_AtSymbolProc;
begin
  Inc(Run);
  fTokenID:=tkPhpKeyword;
end;

procedure TSynWebEngine.Php_EqualProc;
begin
  Inc(Run);
  if fLine[Run]='=' then
  begin
    Inc(Run);
    if fLine[Run]='=' then
      Inc(Run);
  end;
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_GreaterProc;
begin
  Inc(Run);
  case fLine[Run] of
  '=':
    Inc(Run);
  '>':
    begin
      Inc(Run);
      if fLine[Run]='=' then
        Inc(Run);
    end;
  end;
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_LowerProc;
var
  tmpRun:Longword;
begin
  Inc(Run);
  case fLine[Run] of
  '/':
    if (Php_GetOpenTag=potHTML) and
       (fHashTable[fLine[Run+1]]=fHashTable['s']) and
       (fHashTable[fLine[Run+2]]=fHashTable['c']) and
       (fHashTable[fLine[Run+3]]=fHashTable['r']) and
       (fHashTable[fLine[Run+4]]=fHashTable['i']) and
       (fHashTable[fLine[Run+5]]=fHashTable['p']) and
       (fHashTable[fLine[Run+6]]=fHashTable['t']) and
       (fIdentTable2[fLine[Run+7]] and (1 shl 0)<>0) then // (fLine[Run+7] in [#0..#32, '>']) then
    begin
      Dec(Run);
      Php_End;
      Exit;
    end;
  '=':
    Inc(Run);
  '<':
    begin
      Inc(Run);
      case fLine[Run] of
      '=':
        Inc(Run);
      '<':
        begin
          Inc(Run);
          tmpRun:=Run;
          while fLine[tmpRun] in [#1..#32] do
            Inc(tmpRun);
          if fIdentTable[fLine[tmpRun]] and (1 shl 28)=0 then // if not (fLine[tmpRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF]) then
          begin
            fTokenID:=tkPhpError;
            Exit;
          end;
          Php_SetRange(rsPhpSubProc);
          SetRange_Int(3, 20, 2);
        end;
      end;
    end;
  end;
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_PlusProc;
begin
  Inc(Run);
  if fLine[Run] in ['+', '='] then
    Inc(Run);
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_MinusProc; 
begin
  Inc(Run);
  if fLine[Run] in ['-', '=', '>'] then
    Inc(Run);
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_MulDivModXorProc;
begin
  Inc(Run);
  if fLine[Run]='=' then
    Inc(Run);
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_SlashProc;
begin
  case fLine[Run+1] of
  '/':
    begin
      Inc(Run);
      Php_HashProc;
    end;
  '*':
    begin
      Inc(Run,2);
      Php_SetRange(rsPhpComment);
      if fLine[Run]=#0 then
        fTokenID:=tkPhpComment
      else
        Php_RangeCommentProc;
    end;
  else
    Php_MulDivModXorProc;
  end;
end;

procedure TSynWebEngine.Php_PercentProc;
begin
  if (fLine[Run+1]='>') and (fHighlighterMode=shmPhp) then
  begin
    Inc(Run, 2);
    if Php_GetOpenTag=potASP then
    begin
      fTokenID:=tkHtmlTag;
      Php_End;
    end else
      fTokenID:=tkPhpError;
  end else
    Php_MulDivModXorProc
end;

procedure TSynWebEngine.Php_HashProc;
begin
  fTokenID:=tkPhpComment;
  repeat
    repeat
      Inc(Run)
    until fIdentTable[fLine[Run]] and (1 shl 17)<>0; // until fLine[Run] in [#0, #10, #13, '%', '?'];
    case fLine[Run] of
    #0:
      Exit;
    '?':
      if (fLine[Run+1]='>') and (Php_GetOpenTag in [potPhp, potPhpShort]) and (fHighlighterMode=shmPhp) then
        Exit;
    '%':   
      if (fLine[Run+1]='>') and (Php_GetOpenTag=potASP) and (fHighlighterMode=shmPhp) then
        Exit;
    else
      Exit;
    end;
  until False;
end;

procedure TSynWebEngine.Php_NotProc;
begin
  Inc(Run);
  if fLine[Run]='=' then
  begin
    Inc(Run);
    if fLine[Run]='=' then
      Inc(Run);
  end;
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_DotProc;
begin
  Inc(Run);
  if fLine[Run]='=' then
  begin
    Inc(Run);
     fTokenID:=tkPhpSymbol;
  end else
    Php_NumberProc;
end;

procedure TSynWebEngine.Php_SymbolProc;
begin
  Inc(Run);
  fTokenID:=tkPhpSymbol;
end;

procedure TSynWebEngine.Php_VarProc;
begin
  Inc(Run);
  if fLine[Run]='$' then
    Inc(Run);
  if fIdentTable[fLine[Run]] and (1 shl 28)<>0 then // if fLine[Run] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    fTokenID:=tkPhpKeyword
  else
    fTokenID:=tkPhpError;
end;

procedure TSynWebEngine.Php_IdentProc;
begin
  repeat
    Inc(Run);
  until fIdentTable[fLine[Run]] and (1 shl 29)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  if (fTokenPos>0) and (fLine[fTokenPos-1]='$') then
    fTokenID:=tkPhpVariable
  else
    fTokenID:=Php_IdentCheck;
end;

procedure TSynWebEngine.Php_ErrorProc;
begin
  Inc(Run);
  fTokenID:=tkPhpError;
end;

function TSynWebEngine.Php_DoStringDouble(AIsHeredoc:Boolean):Boolean;

  procedure TryDoSpace;
  begin
    while fLine[Run] in [#1..#32] do
      Inc(Run);
  end;

  procedure DoIdent;
  begin
    repeat
      Inc(Run);
    until fIdentTable[fLine[Run]] and (1 shl 29)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  end;

  function TryDoIdent:Boolean;
  begin
    if fIdentTable[fLine[Run]] and (1 shl 28)<>0 then // if fLine[Run] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    begin
      DoIdent;
      Result:=True;
    end else
      Result:=False;
  end;

  function DoStringSingle:Boolean;
  begin
    Result:=True;
    repeat
      if fLine[Run]='\' then
      begin
        Inc(Run);
        if fLine[Run] in [#39, '\'] then
          Inc(Run);
      end;
      while fIdentTable[fLine[Run]] and (1 shl 24)=0 do // while not(fLine[Run] in [#0, #39, '\'] do
        Inc(Run);
      if fLine[Run]='\' then
        Continue
      else
      begin
        if fLine[Run]<>#39 then
          Result:=False;
        Exit;
      end;
    until False;
  end;

  function DoStringObject(AAllowSpaces:Boolean=True):Boolean;
  begin
    Inc(Run, 2);
    if AAllowSpaces then
    begin
      TryDoSpace;
      Result:=TryDoIdent;
      TryDoSpace;
    end else
      Result:=TryDoIdent;
  end;

  function DoStringVar:Boolean;
  begin
    Inc(Run);
    Result:=TryDoIdent;
  end;

  function DoStringVar2:Boolean;
  begin
    TryDoSpace;
    Result:=True;
    case fLine[Run] of
    '-':
      if (fLine[Run+1]='>') and ((not DoStringObject) or ((fLine[Run] in ['[', '-']) and not DoStringVar2)) then
        Result:=False;
    '[':
      begin
        Inc(Run);
        TryDoSpace;
        case fLine[Run] of
        '$':
          if (not DoStringVar) or ((fLine[Run] in ['[', '-']) and not DoStringVar2) then
            Result:=False;
        '0'..'9', '.':
          if not Php_CheckNumberProc then
            Result:=False;
        #39:
          begin
            Inc(Run);
            if DoStringSingle then
              Inc(Run)
            else
              Result:=False;
          end;
        '"':
          begin
            Inc(Run);
            while not Php_DoStringDouble and (fLine[Run]<>#0) and (fTokenID<>tkPhpError) do
              ;
            if (fLine[Run]='"') and (fTokenID<>tkPhpError) then
            begin
              fTokenID:=tkPhpStringSpecial;
              Inc(Run);
            end else
              Result:=False
          end;
        else
          if not TryDoIdent then
            Result:=False;
        end;
        TryDoSpace;
        if not Result or (fLine[Run]<>']') then
          Result:=False
        else begin
          Inc(Run);
          TryDoSpace;
          if (fLine[Run] in ['[', '-']) and not DoStringVar2 then
            Result:=False;
        end;
      end;
    end;
  end;

begin
  Result:=False;
  fTokenID:=tkPhpStringSpecial;
  case fLine[Run] of
  '$':
    begin
      Inc(Run);
      if TryDoIdent then
      begin
        case fLine[Run] of
        '-':
          if fLine[Run+1]='>' then
            if not DoStringObject(False) then   
              fTokenID:=tkPhpError;
        '[':
          begin
            Inc(Run);
            case fLine[Run] of
            '$':
              if not DoStringVar then
                fTokenID:=tkPhpError;
            '0'..'9', '.':
              if not Php_CheckNumberProc then
                fTokenID:=tkPhpError;
            else
              if not TryDoIdent then
                fTokenID:=tkPhpError;
            end;
            if fLine[Run]=']' then
              Inc(Run)
            else      
              fTokenID:=tkPhpError;
          end;
        end;
        Exit;
      end else
        if fLine[Run]='{' then
        begin
          Inc(Run);
          if not TryDoIdent or not DoStringVar2 or (fLine[Run]<>'}') then
            fTokenID:=tkPhpError
          else
            Inc(Run);
          Exit;
        end;
    end;
  '{':
    begin
      Inc(Run);
      if fLine[Run]='$' then
      begin
        Inc(Run);
        if not TryDoIdent or not DoStringVar2 or (fLine[Run]<>'}') then
          fTokenID:=tkPhpError
        else
          Inc(Run);
        Exit;
      end;

    end;
  '\':
    begin
      Inc(Run);
      if fIdentTable[fLine[Run]] and (1 shl 18)<>0 then // if fLine[Run] in ['n', 'r', 't', '\', '$', #34, '0'..'7', 'x'] then
      begin
        Inc(Run);
        case fLine[Run-1] of
        '0'..'7':
          begin
            if fLine[Run] in ['0'..'7'] then
            begin
              Inc(Run);
              if fLine[Run] in ['0'..'7'] then
                Inc(Run);
            end;
            Exit;
          end;
        'x':
          if fIdentTable[fLine[Run]] and (1 shl 10)<>0 then // if fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9'] then
          begin
            Inc(Run);
            if fIdentTable[fLine[Run]] and (1 shl 10)<>0 then // if fLine[Run] in ['a'..'f', 'A'..'F', '0'..'9'] then
              Inc(Run);
            Exit;
          end;
        else
          Exit;
        end;
      end;
    end;
  end;
  fTokenID:=tkPhpString;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 25)=0 do // while not(fLine[Run] in [#0, #34, '\', '{', '$'] do
      Inc(Run);
    if fLine[Run]=#34 then
      if AIsHeredoc then
      begin
        Inc(Run);
        Continue;
      end else
        Result:=True;
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
      Inc(Run, 2);
      fTokenID:=tkHtmlTag;
      SetRange_Int(3, 20, 1);
    end;
  1:
    begin
      DoDefault;
      if Php_GetOpenTag=potPhp then
      begin
        Inc(Run, 3);
        fTokenID:=tkPhpKeyword;
      end else // potPhpShort, potASP
        if fLine[Run]='=' then
        begin
          Inc(Run);
          fTokenID:=tkPhpKeyword;
        end else
          Php_RangeDefaultProc;
    end;
  2:
    begin
      if fLine[Run] in [#1..#32] then
      begin
        repeat
          Inc(Run);
        until not (fLine[Run] in [#1..#32]);
        fTokenID:=tkPhpSpace;
        Exit;
      end;
      repeat
        Inc(Run);
      until fIdentTable[fLine[Run]] and (1 shl 29)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
      if fLine[Run]<>#0 then
      begin
        fTokenID:=tkPhpError;
        Php_SetRange(rsPhpDefault);
        Exit;
      end;
      fTokenID:=tkPhpKeyword;
      Php_SetRange(rsPhpHeredoc);
      SetRange_Int(8, 17, GetCRC8_String(GetToken));
      SetRange_Bit(25, GetTokenLen mod 2=0);
    end;
  end;
end;

procedure TSynWebEngine.Php_RangeDefaultProc;
begin
  fPhp_ProcTable[fLine[Run]];
end;

procedure TSynWebEngine.Php_RangeCommentProc;
begin
  repeat
    if (fLine[Run]='*') and (fLine[Run+1]='/') then
    begin
      Inc(Run,2);
      Php_SetRange(rsPhpDefault);
      Break;
    end;
    Inc(Run);
  until fLine[Run]=#0;
  fTokenID:=tkPhpComment;
end;

procedure TSynWebEngine.Php_RangeString34Proc;
begin
  if Php_DoStringDouble then
  begin                          
    Inc(Run);
    Php_SetRange(rsPhpDefault);
  end;
end;

procedure TSynWebEngine.Php_RangeString39Proc;
begin
  if ES_CheckNull then
    Exit;

  if fLine[Run]='\' then
  begin
    Inc(Run);
    if fLine[Run] in [#39, '\'] then
    begin
      Inc(Run);
      fTokenID:=tkPhpStringSpecial;
      Exit;
    end;
  end;
  fTokenID:=tkPhpString;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 24)=0 do // while not(fLine[Run] in [#0, #39, '\'] do
      Inc(Run);
    if fLine[Run]<>#39 then
      Exit
    else
    begin
      Inc(Run);
      Php_SetRange(rsPhpDefault);
      Exit;
    end;
  until False;
  ES_SetRange(rsESDefault);
end;

procedure TSynWebEngine.Php_RangeStringShellProc;
begin
  if fLine[Run]='\' then
  begin
    Inc(Run);
    if fLine[Run] in ['`', '\'] then
    begin
      Inc(Run);
      fTokenID:=tkPhpStringSpecial;
      Exit;
    end;
  end;  
  fTokenID:=tkPhpString;
  repeat
    while fIdentTable[fLine[Run]] and (1 shl 27)=0 do // while not(fLine[Run] in [#0, '`', '\'] do
      Inc(Run);
    case fLine[Run] of
    #0:
        Exit;
    '`':
      begin
        Inc(Run);
        Php_SetRange(rsPhpDefault);
        Exit;
      end;
    '\':
      begin
        if fLine[Run+1] in ['`', '\'] then
          Exit
        else
          Inc(Run);
      end;
    end;
  until False;
end;

procedure TSynWebEngine.Php_RangeHeredocProc;
var
  OldRun:Longint;
begin
  if fIdentTable[fLine[Run]] and (1 shl 28)<>0 then // if fLine[Run] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  begin
    OldRun:=Run;
    repeat
      Inc(Run);
    until fIdentTable[fLine[Run]] and (1 shl 29)=0; // until not(fLine[Run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
    if ((fLine[Run]=';') and (fLine[Run+1]=#0)) or (fLine[Run]=#0) then
    begin
      if (GetRange_Bit(25)=(GetTokenLen mod 2=0)) and (GetRange_Int(8, 17)=GetCRC8_String(GetToken)) then
      begin
        fTokenID:=tkPhpKeyword;
        Php_SetRange(rsPhpDefault);
        Exit;
      end;
    end;
    Run:=OldRun;
  end;
  if Php_DoStringDouble(True) then
  begin
    Inc(Run);
    Php_SetRange(rsPhpDefault);
  end;
end;

function TSynWebEngine.Php_KeywordComp(const ID:Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
  Data:Longword;
begin
  Data:=TSynWeb_PhpKeywordsData[ID];
  if (Data and $0F<>$01) or ((Data shr 16) and (1 shl Longword(fPhp_Version))=0) then
  begin
    Result:=False;
    Exit;
  end;
  aKey:=TSynWeb_PhpKeywords[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Php_ConstComp: Boolean;
var
  I:Integer;
  Temp:PChar;
begin
  Temp:=fToIdent;
  for i:=1 to fStringLen do
  begin
    if UpCase(Temp^)<>Temp^ then
    begin
      Result:=False;
      Exit;
    end;
    Inc(Temp);
  end;
  Result:=True;
end;

function TSynWebEngine.Php_FunctionComp(const ID:Integer): Boolean;
var
  I:Integer;
  Temp:PChar;
  aKey:String;
  Data:Longword;
begin
  Data:=TSynWeb_PhpKeywordsData[ID];
  if (Data and $0F<>$08) or ((Data shr 16) and (1 shl Longword(fPhp_Version))=0) then
  begin
    Result:=False;
    Exit;
  end;
  aKey:=TSynWeb_PhpKeywords[ID];
  Temp:=fToIdent;
  if Length(aKey)=fStringLen then
  begin
    for i:=1 to fStringLen do
    begin
      if fInsensitiveHashTable[Temp^]<>fInsensitiveHashTable[aKey[i]] then
      begin
        Result:=False;
        Exit;
      end;
      Inc(Temp);
    end;
    fToken_LastID:=ID;
    Result:=True;
  end else
    Result:=False;
end;

function TSynWebEngine.Php_IdentCheck: TtkTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i:Integer;
  begin
    HashKey:=0;
    fStringLen:=Run-fTokenPos;
    for i:=0 to fStringLen-1 do
    begin     
      inc(HashKey, fInsensitiveHashTable[ToHash^]);
      inc(ToHash);
    end;
  end;

begin
  fToIdent:=@FLine[fTokenPos];
  KeyHash(fToIdent);
  fToken_LastID:=-1;
  if HashKey<=Php_KeywordsMaxKeyHash then
    Result:=fPhp_IdentFuncTable[HashKey]
   else
    Result:=tkPhpIdentifier;
  if Result=tkPhpIdentifier then
    if Php_ConstComp then
      Result:=tkPhpConst;
end;

{$I SynHighlighterWeb_PhpKeywordsFunc.inc}

{ TSynWebBase }

function TSynWebBase.GetDefaultAttribute(
  Index: integer): TSynHighlighterAttributes;
begin
 {?? if not (fHighlighterType in fActiveHighlighters) then
    Result:=fInactiveAttri
  else    }
    case Index of
  // SYN_ATTR_IDENTIFIER: ??
  // SYN_ATTR_KEYWORD: ??
  // SYN_ATTR_SYMBOL: ??
      SYN_ATTR_WHITESPACE:
        if Enabled then           //todo: hmmm... ?
          Result:=FEngine.fSYN_ATTR_WHITESPACE
        else
          Result:=FEngine.fHtml_WhitespaceAttri;
      SYN_ATTR_COMMENT:
        Result:=FEngine.fSYN_ATTR_COMMENT;
      SYN_ATTR_STRING:
        Result:=FEngine.fSYN_ATTR_STRING;
    else
      Result:=nil;
    end;
end;

function TSynWebBase.GetIdentChars: TSynIdentChars;
begin

end;

function TSynWebBase.GetRange: Pointer;
begin
  Result:=Pointer(FConfig.fRange);
end;

function TSynWebBase.GetSampleSource: string;
begin

end;

function TSynWebBase.GetToken: string;
begin

end;

function TSynWebBase.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if (FConfig.fHighlighterType in FConfig.fActiveHighlighters) {or
    (FTokenID in [tkHtmlSpace, tkCssSpace, tkESSpace, tkPhpSpace])} then
    Result:=FEngine.fTokenAttributeTable[FConfig.FTokenID]
  else
    Result:=FEngine.fInactiveAttri;
end;

function TSynWebBase.GetTokenID: TtkTokenKind;
begin

end;

function TSynWebBase.GetTokenKind: Integer;
begin

end;

function TSynWebBase.GetTokenLen: Integer;
begin

end;

function TSynWebBase.GetTokenPos: Integer;
begin

end;

procedure TSynWebBase.Next;
begin
  FEngine.FConfig = @FConfig;
  FEngine.Next;
end;

procedure TSynWebBase.SetEngine(const Value: TSynWebEngine);
begin
  FEngine := Value;
end;

procedure TSynWebBase.SetLine(NewValue: string; LineNumber: Integer);
begin
  FEngine.FConfig = @FConfig;
  FEngine.SetLine(NewValue, LineNumber);
end;

procedure TSynWebBase.SetRange(Value: Pointer);
begin
  FConfig.fRange:=Longword(Value);
end;

{ TSynWebSynPHPMulti }

procedure TSynWebSynPHPMulti.ResetRange;
begin
  FConfig.fRange:=$00000000;
end;

{ TSynWebSynES }

procedure TSynWebSynES.ResetRange;
begin
  FConfig.fRange:=$00000000;
  FEngine.FConfig = @FConfig;
  FEngine.SetRange_Int(3, 29, Longword(shtES));
end;

{ TSynWebSynCSS }

procedure TSynWebSynCSS.ResetRange;
begin
  FConfig.fRange:=$00000000;
  FEngine.FConfig = @FConfig;
  FEngine.SetRange_Int(3, 29, Longword(shtCss));
end;

{ TSynWebSynHtml }

procedure TSynWebSynHtml.ResetRange;
begin
  fRange:=$00000000;
end;

{ TSynWebSynPHP }

procedure TSynWebSynPHP.ResetRange;
begin       
  FConfig.fRange:=$00000000;
  FEngine.FConfig = @FConfig;
  FEngine.SetRange_Int(3, 29, Longword(shtPHP_inHtml));
  FEngine.Php_SetRange(rsPhpDefault);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  //todo: RegisterPlaceableHighlighter(TSynWebEngine);
{$ENDIF}
end.

