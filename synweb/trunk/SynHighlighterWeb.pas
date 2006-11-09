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
- TSynWeb support only single line SetLine (don't use more than one line).
- Doesn't support #13#10, #10 or #13 as new line. Always use #0 as line break.
- Php: Doesn't support multi-line encapsuled strings in String, only single line:
  eg. "somestring {$a["some array{$b['key'].... <- only single line encapsuled values
-------------------------------------------------------------------------------}
{
@abstract(Provides an web-files (Multi Html/Css/ECMAScript/Php) highlighter for SynEdit
@author(FlatDev <krystian.bigaj@gmail.com>)
@created(2005-05-21)
@lastmod(2006-02-10)
The TSynWeb unit provides SynEdit with an Multi Html/Css/ECMAScript/Php highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERWEB}
unit SynHighlighterWeb;
{$ENDIF}

{$I SynWeb.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,  
  QSynHighlighterWebData,
{$ELSE}
  Graphics,
  SynEditTypes,                 
  SynEditHighlighter, 
  SynHighlighterWebData,
{$ENDIF}
  Classes,
  SysUtils;

// Highlighter -----------------------------------------------------------------

type
  TSynWebEngine = class;
  TSynWebBase = class;

  PSynWebOptions = ^TSynWebOptions;

  TSynWebOptions = record
    FHtmlVersion: TSynWebHtmlVersion;
    FCssVersion: TSynWebCssVersion;
    FPhpVersion: TSynWebPhpVersion;
    FPhpShortOpenTag: Boolean;
    FPhpAspTags: Boolean;

    FPhpEmbeded: Boolean;
    FCssEmbeded: Boolean;
    FEsEmbeded: Boolean;
  end;

  PSynWebInstance = ^TSynWebInstance;

  TSynWebInstance = record
    FRun: longint;
    FRange: Longword;
    FLine: PChar;
    FLineRef: String;
    FLineNumber: Integer;
    FTokenLastID: Integer;
    FTokenPos: Integer;
    FTokenID: TSynWebTokenKind;
    FStringLen, FStringLenClean: Integer;
    FToIdent: PChar;
    FHashTable: TSynWebHashTable;
    FNextClearBits: Boolean;
    FNextUseNextAH: Boolean;
    FUseNextAH: Boolean;
    FHighlighterType, FPrevHighlighterType, FNextHighlighterType: TSynWebHighlighterType;
    FHighlighterSW: Boolean;
    FHighlighterMode: TSynWebHighlighterMode;
    FCssMask: Longword;
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
    function GetPhpAspTags: Boolean;
    procedure SetPhpAspTags(const Value: Boolean);
    function GetPhpShortOpenTag: Boolean;
    procedure SetPhpShortOpenTag(const Value: Boolean);

    function GetCssEmbeded: Boolean;
    procedure SetCssEmbeded(const Value: Boolean);
    function GetEsEmbeded: Boolean;
    procedure SetEsEmbeded(const Value: Boolean);
    function GetPhpEmbeded: Boolean;
    procedure SetPhpEmbeded(const Value: Boolean);

    procedure SetUseEngineOptions(const Value: Boolean);
    procedure SetEngineOptions(AEngine: PSynWebOptions);
    procedure DoOnChange;
    procedure UpdateOptions;
  protected
    property HtmlVersion: TSynWebHtmlVersion read GetHtmlVersion write SetHtmlVersion;
    property CssVersion: TSynWebCssVersion read GetCssVersion write SetCssVersion;
    property PhpVersion: TSynWebPhpVersion read GetPhpVersion write SetPhpVersion;
    property PhpShortOpenTag: Boolean read GetPhpShortOpenTag write SetPhpShortOpenTag;
    property PhpAspTags: Boolean read GetPhpAspTags write SetPhpAspTags;

    property CssEmbeded: Boolean read GetCssEmbeded write SetCssEmbeded;
    property PhpEmbeded: Boolean read GetPhpEmbeded write SetPhpEmbeded;
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
    FActiveHighlighter: Boolean;
    FActiveHighlighters: TSynWebHighlighterTypes;	
    FOptions: TSynWebOptionsBase;
    procedure SetupActiveHighlighter; virtual; abstract;
    procedure SetActiveHighlighter(const Value: Boolean);
    function GetActiveHighlighters: TSynWebHighlighterTypes;
    procedure SetEngine(const Value: TSynWebEngine);
  protected
{$IFDEF UNISYNEDIT}
    procedure DoSetLine(const Value: WideString; LineNumber: Integer); override;
{$ENDIF}
    procedure DoDefHighlightChange;
    function GetAttribCount: Integer; override;
    function GetAttribute(idx: Integer): TSynHighlighterAttributes; override;
{$IFNDEF UNISYNEDIT}
    function GetIdentChars: TSynIdentChars; override;
{$ENDIF}
{$IFDEF UNISYNEDIT}
    function GetSampleSource: WideString; override;
{$ELSE}
    function GetSampleSource: String; override;
{$ENDIF}
  public
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: WideString; override;
    class function SynWebSample: WideString; virtual; abstract;
{$ELSE}
    class function SynWebSample: String; virtual; abstract;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
{$IFNDEF UNISYNEDIT}
    function GetToken: String; override;
{$ENDIF}
    function GetTokenLen: Integer;
    function GetTokenPos: Integer; override;
    function GetTokenID: TSynWebTokenKind;
    function GetTokenKind: Integer; override;
    function GetRange: Pointer; override;
    function GetEol: Boolean; override;
    function GetHighlighterType: TSynWebHighlighterType;
    procedure SetRange(Value: Pointer); override;
{$IFNDEF UNISYNEDIT}
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
{$ENDIF}
    procedure Next; override;       
{$IFDEF UNISYNEDIT}       
    function UpdateActiveHighlighter(ARange: Pointer; ALine: WideString;
      ACaretX, ACaretY: Integer): Boolean;
{$ELSE}    
    function UpdateActiveHighlighter(ARange: Pointer; ALine: String;
      ACaretX, ACaretY: Integer): Boolean;
{$ENDIF}
    property ActiveHighlighters: TSynWebHighlighterTypes read GetActiveHighlighters;
  published
    property ActiveSwitchHighlighter: Boolean
      read FActiveHighlighter write SetActiveHighlighter;
    property Engine: TSynWebEngine read FEngine write SetEngine;
  end;

  TSynWebHtmlSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebHtmlOptions;
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;

    function GetTagID: Integer;
    function GetTagKind: Integer;
  published
    property Options: TSynWebHtmlOptions read GetOptions;
  end;

  TSynWebCssSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebCssOptions;
  public        
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebCssOptions read GetOptions;
  end;

  TSynWebEsSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebEsOptions;
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebEsOptions read GetOptions;
  end;

  TSynWebPhpCliSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebPhpCliOptions;
  public
    class function GetLanguageName: string; override;  
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
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
    FHtmlTagIdentFuncTable: array[0..HtmlTagMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FHtmlAttrIdentFuncTable: array[0..HtmlAttrMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FHtmlSpecialIdentFuncTable: array[0..HtmlSpecialMaxKeyHash] of TSynWebIdent2FuncTableFunc;
    FHtmlRangeProcTable: array[Low(TSynWebHtmlRangeState)..High(TSynWebHtmlRangeState)] of TSynWebProcTableProc;

    FHtmlWhitespaceAttri: TSynHighlighterAttributes;
    FHtmlCommentAttri: TSynHighlighterAttributes;
    FHtmlTextAttri: TSynHighlighterAttributes;
    FHtmlEscapeAttri: TSynHighlighterAttributes;
    FHtmlSymbolAttri: TSynHighlighterAttributes;
    FHtmlTagAttri: TSynHighlighterAttributes;
    FHtmlTagNameAttri: TSynHighlighterAttributes;
    FHtmlTagNameUndefAttri: TSynHighlighterAttributes;
    FHtmlTagKeyAttri: TSynHighlighterAttributes;
    FHtmlTagKeyUndefAttri: TSynHighlighterAttributes;
    FHtmlTagKeyValueAttri: TSynHighlighterAttributes;
    FHtmlTagKeyValueQuotedAttri: TSynHighlighterAttributes;
    FHtmlErrorAttri: TSynHighlighterAttributes;

    // Css ---------------------------------------------------------------------
    FCssProcTable: array[#0..#255] of TSynWebProcTableProc;
    FCssPropIdentFuncTable: array[0..CssPropMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FCssValIdentFuncTable: array[0..CssValMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FCssSpecialIdentFuncTable: array[0..CssSpecialMaxKeyHash] of TSynWebIdent2FuncTableFunc;
    FCssRangeProcTable: array[Low(TSynWebCssRangeState)..High(TSynWebCssRangeState)] of TSynWebProcTableProc;

    FCssWhitespaceAttri: TSynHighlighterAttributes;
    FCssRulesetWhitespaceAttri: TSynHighlighterAttributes;
    FCssSelectorAttri: TSynHighlighterAttributes;
    FCssSelectorUndefAttri: TSynHighlighterAttributes;
    FCssSelectorClassAttri: TSynHighlighterAttributes;
    FCssSelectorIdAttri: TSynHighlighterAttributes;
    FCssSpecialAttri: TSynHighlighterAttributes;
    FCssCommentAttri: TSynHighlighterAttributes;
    FCssPropAttri: TSynHighlighterAttributes;
    FCssPropUndefAttri: TSynHighlighterAttributes;
    FCssValAttri: TSynHighlighterAttributes;
    FCssValUndefAttri: TSynHighlighterAttributes;
    FCssValStringAttri: TSynHighlighterAttributes;
    FCssValNumberAttri: TSynHighlighterAttributes;
    FCssSymbolAttri: TSynHighlighterAttributes;
    FCssErrorAttri: TSynHighlighterAttributes;

    // ECMAScript --------------------------------------------------------------
    FEsProcTable: array[#0..#255] of TSynWebProcTableProc;
    FEsIdentFuncTable: array[0..EsKeywordsMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FEsRangeProcTable: array[Low(TSynWebEsRangeState)..High(TSynWebEsRangeState)] of TSynWebProcTableProc;

    FEsWhitespaceAttri: TSynHighlighterAttributes;
    FEsIdentifierAttri: TSynHighlighterAttributes;
    FEsKeyAttri: TSynHighlighterAttributes;
    FEsCommentAttri: TSynHighlighterAttributes;
    FEsStringAttri: TSynHighlighterAttributes;
    FEsNumberAttri: TSynHighlighterAttributes;
    FEsSymbolAttri: TSynHighlighterAttributes;
    FEsErrorAttri: TSynHighlighterAttributes;

    // Php ---------------------------------------------------------------------
    FPhpProcTable: array[#0..#255] of TSynWebProcTableProc;
    FPhpIdentFuncTable: array[0..PhpKeywordsMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FPhpRangeProcTable: array[Low(TSynWebPhpRangeState)..High(TSynWebPhpRangeState)] of TSynWebProcTableProc;

    FPhpWhitespaceAttri: TSynHighlighterAttributes;
    FPhpInlineTextAttri: TSynHighlighterAttributes;
    FPhpIdentifierAttri: TSynHighlighterAttributes;
    FPhpKeyAttri: TSynHighlighterAttributes;
    FPhpFunctionAttri: TSynHighlighterAttributes;
    FPhpVariableAttri: TSynHighlighterAttributes;
    FPhpConstAttri: TSynHighlighterAttributes;
    FPhpStringAttri: TSynHighlighterAttributes;
    FPhpStringSpecialAttri: TSynHighlighterAttributes;
    FPhpCommentAttri: TSynHighlighterAttributes;
    FPhpDocCommentAttri: TSynHighlighterAttributes;
    FPhpSymbolAttri: TSynHighlighterAttributes;
    FPhpNumberAttri: TSynHighlighterAttributes;
    FPhpErrorAttri: TSynHighlighterAttributes;

    // Html --------------------------------------------------------------------
    procedure HtmlMakeMethodTables;
    procedure HtmlNext;
    function HtmlGetRange: TSynWebHtmlRangeState;
    procedure HtmlSetRange(const ARange: TSynWebHtmlRangeState);
    function HtmlGetTag: Integer;
    procedure HtmlSetTag(const ATag: Integer);
    function HtmlCheckNull(ADo: Boolean = True): Boolean;

    procedure HtmlSpaceProc;
    procedure HtmlAmpersandProc;
    procedure HtmlBraceOpenProc;
    procedure HtmlErrorProc;

    procedure HtmlRangeTextProc;
    procedure HtmlRangeCommentProc;
    procedure HtmlRangeCommentCloseProc;
    procedure HtmlRangeTagDOCTYPEProc;
    procedure HtmlRangeTagCDATAProc;
    procedure HtmlRangeTagProc;
    procedure HtmlRangeTagCloseProc;
    procedure HtmlRangeTagKeyProc;
    procedure HtmlRangeTagKeyEqProc;
    procedure HtmlRangeTagKeyValueProc;
    procedure HtmlRangeTagKeyValueQuoted1Proc;
    procedure HtmlRangeTagKeyValueQuoted2Proc;

    function HtmlTagKeyComp(const ID: Integer): Boolean;
    function HtmlTagCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_TagsFuncList.inc}

    function HtmlAttrKeyComp(const ID: Integer): Boolean;
    function HtmlAttrCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_AttrsFuncList.inc}

    function HtmlSpecialKeyComp(const ID: Integer): Boolean;
    function HtmlSpecialCheck(AStart, ALen: Integer): Integer;
    {$I SynHighlighterWeb_SpecialFuncList.inc}

    // Css ---------------------------------------------------------------------
    procedure CssMakeMethodTables;
    procedure CssNextBg;
    procedure CssNext;
    procedure CssUpdateBg;
    function CssGetRange: TSynWebCssRangeState;
    procedure CssSetRange(const ARange: TSynWebCssRangeState);
    function CssGetProp: Integer;
    procedure CssSetProp(const AProp: Integer);
    function CssCheckNull(ADo: Boolean = True): Boolean;

    procedure CssSpaceProc;
    procedure CssAtKeywordProc;
    procedure CssSlashProc;
    procedure CssBraceOpenProc;
    procedure CssCurlyBraceOpenProc;
    procedure CssCurlyBraceCloseProc;
    procedure CssChildAnySelectorProc;
    procedure CssAttribProc;
    procedure CssHashProc;
    procedure CssDotProc;
    procedure CssCommaProc;
    procedure CssColonProc;
    procedure CssSemiColonProc;
    procedure CssExclamationProc;
    procedure CssStringProc;
    procedure CssPlusProc;
    procedure CssMinusProc;
    procedure CssNumberProc;
    procedure CssNumberDefProc;
    procedure CssIdentProc;
    function CssIdentStartProc: Boolean;
    function CssCustomStringProc(AShl: Longword; ADo: Boolean = True): Boolean;
    function CssNotWhitespace: Boolean;
    procedure CssSymbolProc;
    procedure CssErrorProc;

    procedure CssRangeRulesetProc;
    procedure CssRangeSelectorAttribProc;
    procedure CssRangeSelectorPseudoProc;
    procedure CssRangeAtKeywordProc;
    procedure CssRangePropProc;
    procedure CssRangePropValProc;
    procedure CssRangePropValStrProc;
    procedure CssRangePropValRgbProc;
    procedure CssRangePropValFuncProc;
    procedure CssRangePropValSpecialProc;
    procedure CssRangePropValImportantProc;
    procedure CssRangePropValUrlProc;
    procedure CssRangePropValRectProc;
    procedure CssRangeCommentProc;

    function CssPropKeyComp(const ID: Integer): Boolean;
    function CssPropCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_CssPropsFuncList.inc}

    function CssValKeyComp(const ID: Integer): Boolean;
    function CssValCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_CssValsFuncList.inc}

    function CssSpecialKeyComp(const ID: Integer): Boolean;
    function CssSpecialCheck(AStart, ALen: Integer): Integer;
    {$I SynHighlighterWeb_CssSpecialFuncList.inc}

    // ECMAScript --------------------------------------------------------------
    procedure EsMakeMethodTables;
    procedure EsNext;
    function EsGetRange: TSynWebEsRangeState;
    procedure EsSetRange(const ARange: TSynWebEsRangeState);
    function EsCheckNull(ADo: Boolean = True): Boolean;

    procedure EsSpaceProc;
    procedure EsSlashProc;
    procedure EsLowerProc;
    procedure EsEqualNotProc;
    procedure EsGreaterProc;
    procedure EsAndProc;
    procedure EsPlusProc;
    procedure EsMinusProc;
    procedure EsOrProc;
    procedure EsMulModXorProc;
    procedure EsNumberProc;
    procedure EsString34Proc;
    procedure EsString39Proc;
    procedure EsSymbolProc;
    procedure EsIdentProc;
    procedure EsErrorProc;

    procedure EsRangeDefaultProc;
    procedure EsRangeCommentProc;
    procedure EsRangeCommentMultiProc;
    procedure EsRangeString34Proc;
    procedure EsRangeString39Proc;

    function EsKeywordComp(const ID: Integer): Boolean;

    function EsIdentCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_EsKeywordsFuncList.inc}

    // Php ---------------------------------------------------------------------
    procedure PhpMakeMethodTables;
    procedure PhpNext;
    procedure PhpCliNext;
    function PhpGetRange: TSynWebPhpRangeState;
    procedure PhpSetRange(const ARange: TSynWebPhpRangeState);

    function PhpCheckBegin(ABegin: Boolean = True): Boolean;
    procedure PhpBegin(ATagKind: TSynWebPhpOpenTag);
    procedure PhpEnd(AHtmlTag: Boolean);

    procedure PhpSpaceProc;
    procedure PhpQuestionProc;
    procedure PhpNumberProc;
    function PhpCheckNumberProc: Boolean;
    procedure PhpString34Proc;
    procedure PhpString39Proc;
    procedure PhpStringShellProc;
    procedure PhpAndProc;
    procedure PhpOrProc;
    procedure PhpAtSymbolProc;
    procedure PhpEqualProc;
    procedure PhpGreaterProc;
    procedure PhpLowerProc;
    procedure PhpPlusProc;
    procedure PhpMinusProc;
    procedure PhpMulDivModXorProc;
    procedure PhpSlashProc;
    procedure PhpPercentProc;
    procedure PhpHashProc;
    procedure PhpNotProc;
    procedure PhpDotProc;
    procedure PhpSymbolProc;
    procedure PhpVarProc;
    procedure PhpIdentProc;
    procedure PhpErrorProc;
    function PhpDoStringDouble(AIsHeredoc: Boolean = False;
      ARangeChar: Boolean = True): Boolean;

    procedure PhpSubProcProc;
    procedure PhpRangeDefaultProc;
    procedure PhpRangeCommentProc;
    procedure PhpRangeString34Proc;
    procedure PhpRangeString39Proc;
    procedure PhpRangeStringShellProc;
    procedure PhpRangeHeredocProc;

    function PhpKeywordComp(const ID: Integer): Boolean;
    function PhpConstComp: Boolean;
    function PhpFunctionComp(const ID: Integer): Boolean;

    function PhpIdentCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_PhpKeywordsFuncList.inc}

    // Other -------------------------------------------------------------------
    procedure AddAttribute(AAttrib: TSynHighlighterAttributes);
    procedure AddToNotifyList(ASynWeb: TSynWebBase);
    procedure RemoveFromNotifyList(ASynWeb: TSynWebBase);

    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure DefHighlightChange(Sender: TObject);

    function GetCrc8String(AString: String): byte;
    function GetRangeBit(ABit: Longword): Boolean;
    procedure SetRangeBit(ABit: Longword; AVal: Boolean);
    function GetRangeInt(ALen, APos: Longword): Longword;
    procedure SetRangeInt(ALen, APos, AVal: Longword);

    procedure NullProc;
    procedure NextSetHighlighterType;
    procedure SetHighlighterType(const AHighlighterType: TSynWebHighlighterType;
      AClearBits: Boolean; ASetAtNextToken: Boolean; AUseNextAH: Boolean);
    procedure SetupHighlighterType(AClearBits: Boolean = False);
    procedure SetLine(NewValue: String; LineNumber: Integer);
    procedure Next;
    function GetToken: String;
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
      read FHtmlWhitespaceAttri write FHtmlWhitespaceAttri;
    property HtmlCommentAttri: TSynHighlighterAttributes
      read FHtmlCommentAttri write FHtmlCommentAttri;
    property HtmlTextAttri: TSynHighlighterAttributes
      read FHtmlTextAttri write FHtmlTextAttri;
    property HtmlEscapeAttri: TSynHighlighterAttributes
      read FHtmlEscapeAttri write FHtmlEscapeAttri;
    property HtmlSymbolAttri: TSynHighlighterAttributes
      read FHtmlSymbolAttri write FHtmlSymbolAttri;
    property HtmlTagAttri: TSynHighlighterAttributes
      read FHtmlTagAttri write FHtmlTagAttri;
    property HtmlTagNameAttri: TSynHighlighterAttributes
      read FHtmlTagNameAttri write FHtmlTagNameAttri;
    property HtmlTagNameUndefAttri: TSynHighlighterAttributes
      read FHtmlTagNameUndefAttri write FHtmlTagNameUndefAttri;
    property HtmlTagKeyAttri: TSynHighlighterAttributes
      read FHtmlTagKeyAttri write FHtmlTagKeyAttri;
    property HtmlTagKeyUndefAttri: TSynHighlighterAttributes
      read FHtmlTagKeyUndefAttri write FHtmlTagKeyUndefAttri;
    property HtmlTagKeyValueAttri: TSynHighlighterAttributes
      read FHtmlTagKeyValueAttri write FHtmlTagKeyValueAttri;
    property HtmlTagKeyValueQuotedAttri: TSynHighlighterAttributes
      read FHtmlTagKeyValueQuotedAttri write FHtmlTagKeyValueQuotedAttri;
    property HtmlErrorAttri: TSynHighlighterAttributes
      read FHtmlErrorAttri write FHtmlErrorAttri;

    // Css
    property CssWhitespaceAttri: TSynHighlighterAttributes
      read FCssWhitespaceAttri write FCssWhitespaceAttri;
    property CssRulesetWhitespaceAttri: TSynHighlighterAttributes
      read FCssRulesetWhitespaceAttri write FCssRulesetWhitespaceAttri;
    property CssSelectorAttri: TSynHighlighterAttributes
      read FCssSelectorAttri write FCssSelectorAttri;
    property CssSelectorUndefAttri: TSynHighlighterAttributes
      read FCssSelectorUndefAttri write FCssSelectorUndefAttri;
    property CssSelectorClassAttri: TSynHighlighterAttributes
      read FCssSelectorClassAttri write FCssSelectorClassAttri;
    property CssSelectorIdAttri: TSynHighlighterAttributes
      read FCssSelectorIdAttri write FCssSelectorIdAttri;
    property CssSpecialAttri: TSynHighlighterAttributes
      read FCssSpecialAttri write FCssSpecialAttri;
    property CssCommentAttri: TSynHighlighterAttributes
      read FCssCommentAttri write FCssCommentAttri;
    property CssPropAttri: TSynHighlighterAttributes
      read FCssPropAttri write FCssPropAttri;
    property CssPropUndefAttri: TSynHighlighterAttributes
      read FCssPropUndefAttri write FCssPropUndefAttri;
    property CssValAttri: TSynHighlighterAttributes
      read FCssValAttri write FCssValAttri;
    property CssValUndefAttri: TSynHighlighterAttributes
      read FCssValUndefAttri write FCssValUndefAttri;
    property CssValStringAttri: TSynHighlighterAttributes
      read FCssValStringAttri write FCssValStringAttri;
    property CssValNumberAttri: TSynHighlighterAttributes
      read FCssValNumberAttri write FCssValNumberAttri;
    property CssSymbolAttri: TSynHighlighterAttributes
      read FCssSymbolAttri write FCssSymbolAttri;
    property CssErrorAttri: TSynHighlighterAttributes
      read FCssErrorAttri write FCssErrorAttri;

    // ECMAScript
    property EsWhitespaceAttri: TSynHighlighterAttributes
      read FEsWhitespaceAttri write FEsWhitespaceAttri;
    property EsIdentifierAttri: TSynHighlighterAttributes
      read FEsIdentifierAttri write FEsIdentifierAttri;
    property EsKeyAttri: TSynHighlighterAttributes read FEsKeyAttri write FEsKeyAttri;
    property EsCommentAttri: TSynHighlighterAttributes
      read FEsCommentAttri write FEsCommentAttri;
    property EsStringAttri: TSynHighlighterAttributes
      read FEsStringAttri write FEsStringAttri;
    property EsNumberAttri: TSynHighlighterAttributes
      read FEsNumberAttri write FEsNumberAttri;
    property EsSymbolAttri: TSynHighlighterAttributes
      read FEsSymbolAttri write FEsSymbolAttri;
    property EsErrorAttri: TSynHighlighterAttributes
      read FEsErrorAttri write FEsErrorAttri;

    // Php
    property PhpHereDocList: TStringList read FPhpHereDocList;
    property PhpWhitespaceAttri: TSynHighlighterAttributes
      read FPhpWhitespaceAttri write FPhpWhitespaceAttri;
    property PhpCliInlineTextAttri: TSynHighlighterAttributes
      read FPhpInlineTextAttri write FPhpInlineTextAttri;
    property PhpIdentifierAttri: TSynHighlighterAttributes
      read FPhpIdentifierAttri write FPhpIdentifierAttri;
    property PhpKeyAttri: TSynHighlighterAttributes
      read FPhpKeyAttri write FPhpKeyAttri;
    property PhpFunctionAttri: TSynHighlighterAttributes
      read FPhpFunctionAttri write FPhpFunctionAttri;
    property PhpVariableAttri: TSynHighlighterAttributes
      read FPhpVariableAttri write FPhpVariableAttri;
    property PhpConstAttri: TSynHighlighterAttributes
      read FPhpConstAttri write FPhpConstAttri;
    property PhpStringAttri: TSynHighlighterAttributes
      read FPhpStringAttri write FPhpStringAttri;
    property PhpStringSpecialAttri: TSynHighlighterAttributes
      read FPhpStringSpecialAttri write FPhpStringSpecialAttri;
    property PhpCommentAttri: TSynHighlighterAttributes
      read FPhpCommentAttri write FPhpCommentAttri;
    property PhpDocCommentAttri: TSynHighlighterAttributes
      read FPhpDocCommentAttri write FPhpDocCommentAttri;
    property PhpSymbolAttri: TSynHighlighterAttributes
      read FPhpSymbolAttri write FPhpSymbolAttri;
    property PhpNumberAttri: TSynHighlighterAttributes
      read FPhpNumberAttri write FPhpNumberAttri;
    property PhpErrorAttri: TSynHighlighterAttributes
      read FPhpErrorAttri write FPhpErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst, StrUtils;
{$ELSE}
  SynEditStrConst, Controls;
{$ENDIF}

{ TSynWebOptionsBase }

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

function TSynWebOptionsBase.GetPhpAspTags: Boolean;
begin
  Result := FOptions^.FPhpAspTags;
end;

procedure TSynWebOptionsBase.SetPhpAspTags(const Value: Boolean);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FPhpAspTags := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpShortOpenTag: Boolean;
begin
  Result := FOptions^.FPhpShortOpenTag;
end;

procedure TSynWebOptionsBase.SetPhpShortOpenTag(const Value: Boolean);
begin
  if UseEngineOptions then
    Exit;
  FOptions^.FPhpShortOpenTag := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetCssEmbeded: Boolean;
begin
  Result := FOptions^.FCssEmbeded;
end;

procedure TSynWebOptionsBase.SetCssEmbeded(const Value: Boolean);
begin
  FOptions^.FCssEmbeded := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetEsEmbeded: Boolean;
begin
  Result := FOptions^.FEsEmbeded;
end;

procedure TSynWebOptionsBase.SetEsEmbeded(const Value: Boolean);
begin
  FOptions^.FEsEmbeded := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpEmbeded: Boolean;
begin
  Result := FOptions^.FPhpEmbeded;
end;

procedure TSynWebOptionsBase.SetPhpEmbeded(const Value: Boolean);
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

{ TSynWebEngineOptions }

constructor TSynWebEngineOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  FUseEngineOptions := False;
end;

{ TSynWebBase }

constructor TSynWebBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  FOptions.FOnChange := DefHighlightChange;
  FEngine := nil;
  FDefaultFilter := '';
  FActiveHighlighter := False;
  FActiveHighlighters := [shtHtml, shtCss, shtEs, shtPhpInHtml,
    shtPhpInCss, shtPhpInEs];
  ResetRange;
  DoDefHighlightChange;
end;

destructor TSynWebBase.Destroy;
begin
  Engine := nil;
  if FOptions <> nil then
    FOptions.Free;
  inherited Destroy;
end;

procedure TSynWebBase.SetActiveHighlighter(const Value: Boolean);
begin
  FActiveHighlighter := Value;
  if Value then
    SetupActiveHighlighter
  else
    FActiveHighlighters := [Low(TSynWebHighlighterType)..High(TSynWebHighlighterType)];
  DefHighlightChange(Self);
end;

function TSynWebBase.GetActiveHighlighters: TSynWebHighlighterTypes;
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

{$IFDEF UNISYNEDIT}
procedure TSynWebBase.DoSetLine(const Value: WideString; LineNumber: Integer);
begin
  inherited DoSetLine(Value, LineNumber);
  if FEngine = nil then
    Exit;
  FEngine.FInstance := @FInstance;
  FEngine.SetLine(Value, LineNumber);
end;
{$ENDIF}

procedure TSynWebBase.DoDefHighlightChange;
begin
  FOptions.UpdateOptions;
  if FInstance.FOptions.FHtmlVersion >= shvXHtml10Strict then
    FInstance.FHashTable := TSynWebSensitiveHashTable
  else
    FInstance.FHashTable := TSynWebInsensitiveHashTable;
  DefHighlightChange(Self);
end;

function TSynWebBase.GetAttribCount: Integer;
begin
  if FEngine = nil then
    Result := 0
  else
    Result := FEngine.FAttributes.Count;
end;

function TSynWebBase.GetAttribute(idx: Integer): TSynHighlighterAttributes;
begin
  Result := nil;
  if (FEngine <> nil) and (idx >= 0) and (idx < FEngine.FAttributes.Count) then
    Result := TSynHighlighterAttributes(FEngine.FAttributes.Objects[idx]);
end;

{$IFNDEF UNISYNEDIT}
function TSynWebBase.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;
{$ENDIF}

{$IFDEF UNISYNEDIT}
function TSynWebBase.GetSampleSource: WideString;
{$ELSE}
function TSynWebBase.GetSampleSource: String;
{$ENDIF}
begin
  Result := SynWebSample;
end;

{$IFDEF UNISYNEDIT}
class function TSynWebBase.GetFriendlyLanguageName: WideString;
begin
  Result := GetLanguageName;
end;    
{$ENDIF}

function TSynWebBase.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  if FEngine = nil then
    Result := nil
  else
    case Index of
      // SYN_ATTR_IDENTIFIER: ??
      // SYN_ATTR_KEYWORD: ??
      // SYN_ATTR_SYMBOL: ??
      SYN_ATTR_WHITESPACE:
      begin
        Result := FInstance.fSYN_ATTR_WHITEsPACE;
        if not Enabled then
          case FInstance.FHighlighterMode of
            shmHtml:
              Result := fEngine.FHtmlWhitespaceAttri;
            shmCss:
              Result := fEngine.FCssWhitespaceAttri;
            shmEs:
              Result := fEngine.FEsWhitespaceAttri;
            shmPhpCli:
              Result := fEngine.FPhpInlineTextAttri;
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

{$IFNDEF UNISYNEDIT}
function TSynWebBase.GetToken: String;
var
  Len: longint;
begin
  Len := FInstance.FRun - FInstance.FTokenPos;
  SetString(Result, (FInstance.FLine + FInstance.FTokenPos), Len);
end;
{$ENDIF}

function TSynWebBase.GetTokenLen: Integer;
begin
  Result := FInstance.FRun - FInstance.FTokenPos;
end;

function TSynWebBase.GetTokenPos: Integer;
begin
  Result := FInstance.FTokenPos;
end;

function TSynWebBase.GetTokenID: TSynWebTokenKind;
begin
  Result := FInstance.FTokenID;
end;

function TSynWebBase.GetTokenKind: Integer;
begin
  Result := Ord(FInstance.FTokenID);
end;

function TSynWebBase.GetRange: Pointer;
begin
  Result := Pointer(FInstance.FRange);
end;

function TSynWebBase.GetEol: Boolean;
begin
  Result := FInstance.FTokenID = stkNull;
end;

function TSynWebBase.GetHighlighterType: TSynWebHighlighterType;
begin
  Result := FInstance.FHighlighterType;
end;

procedure TSynWebBase.SetRange(Value: Pointer);
begin
  FInstance.FRange := Longword(Value);
end;

{$IFNDEF UNISYNEDIT}
procedure TSynWebBase.SetLine(NewValue: String; LineNumber: Integer);
begin
  if FEngine = nil then
    Exit;
  FEngine.FInstance := @FInstance;
  FEngine.SetLine(NewValue, LineNumber);
end;
{$ENDIF}

procedure TSynWebBase.Next;
begin
  if FEngine = nil then
    FInstance.FTokenID := stkNull
  else
  begin
    FEngine.FInstance := @FInstance;
    FEngine.Next;
{$IFDEF UNISYNEDIT}
    Run := FInstance.FRun;
    fTokenPos := FInstance.FTokenPos;
    inherited;          
{$ENDIF}
  end;
end;

{$IFDEF UNISYNEDIT}
function TSynWebBase.UpdateActiveHighlighter(ARange: Pointer;
  ALine: WideString; ACaretX, ACaretY: Integer): Boolean;
{$ELSE}
function TSynWebBase.UpdateActiveHighlighter(ARange: Pointer;
  ALine: String; ACaretX, ACaretY: Integer): Boolean;
{$ENDIF}
var
  f: TSynWebHighlighterTypes;
  lPos, lLen: Integer;
  lHinghlighter, ActiveHL: TSynWebHighlighterType;
begin
  Result := True;
  if not FActiveHighlighter or not (FInstance.FOptions.FPhpEmbeded or FInstance.FOptions.FCssEmbeded or
    FInstance.FOptions.FEsEmbeded) then
    Exit;
  f := FActiveHighlighters;
  Dec(ACaretX);
  SetRange(ARange);
  lHinghlighter := TSynWebHighlighterType((FInstance.FRange shr 29) and not ($FFFFFFFF shl 3));
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
  if ActiveHL >= shtPhpInHtml then
    FActiveHighlighters := [shtPhpInHtml, shtPhpInCss, shtPhpInEs]
  else
    FActiveHighlighters := [ActiveHL];
  Result := f <> FActiveHighlighters;
end;

{ TSynWebHtmlSyn }

constructor TSynWebHtmlSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebHtmlOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmHtml;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := True;
  FOptions.EsEmbeded := True;
end;

procedure TSynWebHtmlSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtHtml];
end;

function TSynWebHtmlSyn.GetOptions: TSynWebHtmlOptions;
begin
  Result := TSynWebHtmlOptions(FOptions);
end;

class function TSynWebHtmlSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: HTML (+CSS, +ES, +PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebHtmlSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebHtmlSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '<!DOCTYPE html public "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml">'#13#10 +
    '<head>'#13#10 +
    '  <title><!-- comment > -- >TSynWeb<!-- space after two ''-'' allowed --></title>'#13#10 +
    '<style type="text/css">'#13#10 +
    ''#13#10 +
    TSynWebCssSyn.SynWebSample +
    ''#13#10 +
    '<?php // php works also in css ?>'#13#10 +
    ''#13#10 +
    '/* <?php // in comments ?> */'#13#10 +
    ''#13#10 +
    'span {'#13#10 +
    '  background-image: url("<?= $secure ? ''https://'' : ''http://'''#13#10 +
    '  // php in css-string ?>www.example.com/img.png"); }'#13#10 +
    ''#13#10 +
    '</style>'#13#10 +
    '  '#13#10 +
    '</head>'#13#10 +
    ''#13#10 +
    '<body>'#13#10 +
    ''#13#10 +
    '<![CDATA['#13#10 +
    '  <a href="test"> CDATA Support </a> Warning! CDATA supported only in XHTML'#13#10 +
    '    <?php // no html highlight in CDATA,everything goes here as plain texte, except PHP of course ?>'#13#10 +
    ']]>'#13#10 +
    ''#13#10 +
    '&amp; &copy;'#13#10 +
    '&earth; &copy <!-- invalid amp-tags, ''earth'' not supported and '';'' missed -->'#13#10 +
    ''#13#10 +
    '<script language="php">  // php long open tag (html)'#13#10 +
    ''#13#10 +
    '$b = ''ple'';'#13#10 +
    '$a = <<< my_custom_heredoc'#13#10 +
    'exam$b'#13#10 +
    'my_custom_heredoc;'#13#10 +
    ''#13#10 +
    '</script>'#13#10 +
    ''#13#10 +
    '<a href="http://www.<?= $a; ?>.com">Example.com</a>'#13#10 +
    '<br />'#13#10 +
    ''#13#10 +
    '<div href="whoops" style="someDiv">'#13#10 +
    ''#13#10 +
    '</div>'#13#10 +
    ''#13#10 +
    TSynWebPhpCliSyn.SynWebSample +
    ''#13#10 +
    '<script type="text/javascript" language="javascript">'#13#10 +
    ''#13#10 +
    TSynWebEsSyn.SynWebSample +
    ''#13#10 +
    '// comm<?php'#13#10 +
    '?>'#13#10 +
    'ent'#13#10 +
    ''#13#10 +
    '/* comment <?= ''2''; ?> */'#13#10 +
    ''#13#10 +
    'new s = "test <?= ''3''; ?>";'#13#10 +
    ''#13#10 +
    '</script>'#13#10 +
    ''#13#10 +
    '</body>'#13#10 +
    '</html>'#13#10;
end;

procedure TSynWebHtmlSyn.ResetRange;
begin
  FInstance.FRange := $00000000;
end;

function TSynWebHtmlSyn.GetTagID: Integer;
begin
  if (FEngine <> nil) and (FInstance.FHighlighterType = shtHtml) and
    (FEngine.HtmlGetRange in [srsHtmlTag, srsHtmlTagClose, srsHtmlTagKey, srsHtmlTagKeyEq,
    srsHtmlTagKeyValue, srsHtmlTagKeyValueQuoted1, srsHtmlTagKeyValueQuoted2]) then
    Result := FEngine.HtmlGetTag - 1
  else
    Result := -1;
end;

function TSynWebHtmlSyn.GetTagKind: Integer;
begin
  if (FEngine = nil) or (FInstance.FHighlighterType <> shtHtml) then
    Result := 0
  else
    if FEngine.HtmlGetRange = srsHtmlTagClose then
      Result := -1
    else
      Result := 1;
end;

{ TSynWebCssSyn }

constructor TSynWebCssSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebCssOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmCss;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;  

procedure TSynWebCssSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtCss];
end;

function TSynWebCssSyn.GetOptions: TSynWebCssOptions;
begin
  Result := TSynWebCssOptions(FOptions);
end;

class function TSynWebCssSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: CSS (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebCssSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebCssSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '@import url(style.css);'#13#10 +
    ''#13#10 +
    '@media all, invalid {'#13#10 +
    '  #some-id:first-child,'#13#10 +
    '  .some-class:second-child /* invalid pseudo class */,'#13#10 +
    '    div:hover, /* html-tag */'#13#10 +
    '    #attrib[title~="test"] {'#13#10 +
    '    border-top :  1px solid black;'#13#10 +
    '    border-left: -1px solid rgb(0,0,0); /* // negative value not supported here */'#13#10 +
    '    margin:      -1px -1px; /* negative supported in margins */'#13#10 +
    '    background-color: #222 ! important;'#13#10 +
    '    background-image: url(style.css);'#13#10 +
    '    color:     1px solid #fffff  important;  /* errors */'#13#10 +
    '    something: 1px solid #222222 url("invalid tag ''something''");'#13#10 +
    '  }'#13#10 +
    '}'#13#10;
end;

procedure TSynWebCssSyn.ResetRange;
begin
  FInstance.FRange := $00000000 or (Longword(shtCss) shl 29);
end;

{ TSynWebEsSyn }

constructor TSynWebEsSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebEsOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmEs;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebEsSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtEs];
end;

function TSynWebEsSyn.GetOptions: TSynWebEsOptions;
begin
  Result := TSynWebEsOptions(FOptions);
end;

class function TSynWebEsSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: ES (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebEsSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebEsSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '// comment'#13#10 +
    '/* comment 2 */'#13#10 +
    ''#13#10 +
    'function test()'#13#10 +
    '{'#13#10 +
    '  window.location.href = ''http://www.example.com'';'#13#10 +
    '  new b = 22;'#13#10 +
    '  return b;'#13#10 +
    '}'#13#10;
end;

procedure TSynWebEsSyn.ResetRange;
begin
  FInstance.FRange := $00000000 or (Longword(shtEs) shl 29);
end;

{ TSynWebPhpCliSyn }

constructor TSynWebPhpCliSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebPhpCliOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmPhpCli;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;      

procedure TSynWebPhpCliSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtHtml];
end;

function TSynWebPhpCliSyn.GetOptions: TSynWebPhpCliOptions;
begin
  Result := TSynWebPhpCliOptions(FOptions);
end;

class function TSynWebPhpCliSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: PHP-Cli';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebPhpCliSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebPhpCliSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '<?php'#13#10 +
    ''#13#10 +
    'echo ''<?xml version="1.0" encoding="iso-8859-2"?>'';'#13#10 +
    ''#13#10 +
    '// single line comment 1'#13#10 +
    '# single line comment 2'#13#10 +
    '/* multi line comment - ?> */'#13#10 +
    '/** doc-style comment - ?> */'#13#10 +
    ''#13#10 +
    'if( SOME_CONSTANT ) // indet''s typed upper-case are become as constants attrib'#13#10 +
    '{'#13#10 +
    '  $a = ''single quote string \n'';'#13#10 +
    '  $b = "double quote string; $someobject->result->a[$b]    ?>'#13#10 +
    '    {$someobject->result->a[$b]} $t[2]; $t[3 error in string; \n octal \222  \2222222 ";'#13#10 +
    '  $x = ''-a'';'#13#10 +
    '  $c = `ls $x`;'#13#10 +
    '  $z = 2. ; // error'#13#10 +
    '  $z = 2.1 + .2;  // numbers'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '  // custom tags supported, spaces/tabs allowed between ''<<<'' and HEREDOC ident'#13#10 +
    '  $output = <<< what_ever_you_TYPE'#13#10 +
    'what_ever_you_type;'#13#10 +
    'what_ever_you_TYPE ;'#13#10 +
    ' what_ever_you_TYPE;'#13#10 +
    'what_ever_you_TYPE;;'#13#10 +
    'what_ever_you_TYPE;'#13#10 +
    ''#13#10 +
    'while( my_function($arg) && mysql_query($query) )'#13#10 +
    '{'#13#10 +
    '/// do sth'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '?>'#13#10;
end;

procedure TSynWebPhpCliSyn.ResetRange;
begin
  FInstance.FRange := $00000000;
end;

{ TSynWebEngine }

constructor TSynWebEngine.Create(AOwner: TComponent);

  function CreateAttrib(const AName:String): TSynHighlighterAttributes;
  begin
{$IFDEF UNISYNEDIT}
    Result := TSynHighlighterAttributes.Create(AName, AName);
{$ELSE}                                                      
    Result := TSynHighlighterAttributes.Create(AName);
{$ENDIF}
  end;

begin
  inherited Create(AOwner);
  FOptions := TSynWebEngineOptions.Create(@FEngineOptions);
  FOptions.FOnChange := DefHighlightChange;
  FNotifyList := TList.Create;
  FPhpHereDocList := TStringList.Create;
  with FPhpHereDocList do
  begin
    Text :=
      'EOF'#13#10+
      'eof'#13#10+
      'EOT'#13#10+
      'eot'#13#10+
      'EOL'#13#10+
      'eol'#13#10+
      'EOD'#13#10+
      'eod'#13#10+
      'HTML'#13#10+
      'html'#13#10+
      'CONTENT'#13#10+
      'content'#13#10+
      'HEREDOC'#13#10+
      'heredoc'#13#10+
      'OUT'#13#10+
      'out'#13#10+
      'STRING'#13#10+
      'string';
{$IFDEF SYN_COMPILER_6_UP}
    CaseSensitive := True;
{$ENDIF}
    Sorted := True;
  end;

  FAttributes := TStringList.Create;
  FAttributes.Duplicates := dupError;
  FAttributes.Sorted := True;

  // Html
  HtmlMakeMethodTables;

  FHtmlWhitespaceAttri := CreateAttrib('Html: Whitespace');
  AddAttribute(FHtmlWhitespaceAttri);

  FHtmlCommentAttri := CreateAttrib('Html: Comment');
  FHtmlCommentAttri.Foreground := $A4A0A0;
  AddAttribute(FHtmlCommentAttri);

  FHtmlTextAttri := CreateAttrib('Html: Text');
  AddAttribute(FHtmlTextAttri);

  FHtmlEscapeAttri := CreateAttrib('Html: Escaped amps');
  FHtmlEscapeAttri.Foreground := clTeal;
  AddAttribute(FHtmlEscapeAttri);

  FHtmlSymbolAttri := CreateAttrib('Html: Symbol'); 
  FHtmlSymbolAttri.Foreground := clBlack;
  AddAttribute(FHtmlSymbolAttri);

  FHtmlTagAttri := CreateAttrib('Html: Tag');
  FHtmlTagAttri.Foreground := clNavy;
  AddAttribute(FHtmlTagAttri);

  FHtmlTagNameAttri := CreateAttrib('Html: Tag name');  
  FHtmlTagNameAttri.Foreground := clBlue;
  AddAttribute(FHtmlTagNameAttri);

  FHtmlTagNameUndefAttri := CreateAttrib('Html: Undefined tag name');
  FHtmlTagNameUndefAttri.Foreground := clBlue;
  FHtmlTagNameUndefAttri.Style := [fsUnderline];
  AddAttribute(FHtmlTagNameUndefAttri);

  FHtmlTagKeyAttri := CreateAttrib('Html: Key'); 
  FHtmlTagKeyAttri.Foreground := clRed;
  AddAttribute(FHtmlTagKeyAttri);

  FHtmlTagKeyUndefAttri := CreateAttrib('Html: Undefined key');  
  FHtmlTagKeyUndefAttri.Foreground := clRed;
  FHtmlTagKeyUndefAttri.Style := [fsUnderline];
  AddAttribute(FHtmlTagKeyUndefAttri);

  FHtmlTagKeyValueAttri := CreateAttrib('Html: Value'); 
  FHtmlTagKeyValueAttri.Foreground := clFuchsia;
  AddAttribute(FHtmlTagKeyValueAttri);

  FHtmlTagKeyValueQuotedAttri := CreateAttrib('Html: Quoted value'); 
  FHtmlTagKeyValueQuotedAttri.Foreground := clFuchsia;
  AddAttribute(FHtmlTagKeyValueQuotedAttri);

  FHtmlErrorAttri := CreateAttrib('Html: Error');
  FHtmlErrorAttri.Foreground := clRed;
  FHtmlErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FHtmlErrorAttri);

  FTokenAttributeTable[stkHtmlSpace] := FHtmlWhitespaceAttri;
  FTokenAttributeTable[stkHtmlComment] := FHtmlCommentAttri;
  FTokenAttributeTable[stkHtmlText] := FHtmlTextAttri;
  FTokenAttributeTable[stkHtmlEscape] := FHtmlEscapeAttri;
  FTokenAttributeTable[stkHtmlSymbol] := FHtmlSymbolAttri;
  FTokenAttributeTable[stkHtmlTag] := FHtmlTagAttri;
  FTokenAttributeTable[stkHtmlTagName] := FHtmlTagNameAttri;
  FTokenAttributeTable[stkHtmlTagNameUndef] := FHtmlTagNameUndefAttri;
  FTokenAttributeTable[stkHtmlTagKey] := FHtmlTagKeyAttri;
  FTokenAttributeTable[stkHtmlTagKeyUndef] := FHtmlTagKeyUndefAttri;
  FTokenAttributeTable[stkHtmlTagKeyValue] := FHtmlTagKeyValueAttri;
  FTokenAttributeTable[stkHtmlTagKeyValueQuoted] := FHtmlTagKeyValueQuotedAttri;
  FTokenAttributeTable[stkHtmlError] := FHtmlErrorAttri;

  // Css
  CssMakeMethodTables;

  FCssWhitespaceAttri := CreateAttrib('Css: Whitespace');    
  FCssWhitespaceAttri.Background := $F0FFFF;
  AddAttribute(FCssWhitespaceAttri);

  FCssRulesetWhitespaceAttri := CreateAttrib('Css: Ruleset whitespace'); 
  FCssRulesetWhitespaceAttri.Background := clInfoBk;
  AddAttribute(FCssRulesetWhitespaceAttri);

  FCssSelectorAttri := CreateAttrib('Css: Selector'); 
  FCssSelectorAttri.Foreground := clBlue;
  FCssSelectorAttri.Style := [fsBold];
  AddAttribute(FCssSelectorAttri);

  FCssSelectorUndefAttri := CreateAttrib('Css: Undefined selector');   
  FCssSelectorUndefAttri.Foreground := clBlue;
  FCssSelectorUndefAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FCssSelectorUndefAttri);

  FCssSelectorClassAttri := CreateAttrib('Css: Class selector');   
  FCssSelectorClassAttri.Foreground := $C08000;
  FCssSelectorClassAttri.Style := [fsBold];
  AddAttribute(FCssSelectorClassAttri);

  FCssSelectorIdAttri := CreateAttrib('Css: Id selector'); 
  FCssSelectorIdAttri.Foreground := clGreen;
  FCssSelectorIdAttri.Style := [fsBold];
  AddAttribute(FCssSelectorIdAttri);

  FCssSpecialAttri := CreateAttrib('Css: Special');
  FCssSpecialAttri.Foreground := clNavy;
  AddAttribute(FCssSpecialAttri);

  FCssCommentAttri := CreateAttrib('Css: Comment');
  FCssCommentAttri.Foreground := $A4A0A0;
  FCssCommentAttri.Style := [fsItalic];
  AddAttribute(FCssCommentAttri);

  FCssPropAttri := CreateAttrib('Css: Property'); 
  FCssPropAttri.Foreground := clBlue;
  AddAttribute(FCssPropAttri);

  FCssPropUndefAttri := CreateAttrib('Css: Undefined property'); 
  FCssPropUndefAttri.Foreground := clBlue;
  FCssPropUndefAttri.Style := [fsUnderline];
  AddAttribute(FCssPropUndefAttri);

  FCssValAttri := CreateAttrib('Css: Value');   
  FCssValAttri.Foreground := clRed;
  AddAttribute(FCssValAttri);

  FCssValUndefAttri := CreateAttrib('Css: Undefined value'); 
  FCssValUndefAttri.Foreground := clRed;
  FCssValUndefAttri.Style := [fsUnderline];
  AddAttribute(FCssValUndefAttri);

  FCssValStringAttri := CreateAttrib('Css: String value');
  FCssValStringAttri.Foreground := clFuchsia ;
  AddAttribute(FCssValStringAttri);

  FCssValNumberAttri := CreateAttrib('Css: Number value');   
  FCssValNumberAttri.Foreground := clGreen;
  AddAttribute(FCssValNumberAttri);

  FCssSymbolAttri := CreateAttrib('Css: Symbol');   
  FCssSymbolAttri.Foreground := clBlack;
  AddAttribute(FCssSymbolAttri);

  FCssErrorAttri := CreateAttrib('Css: Error');    
  FCssErrorAttri.Foreground := clRed;
  FCssErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FCssErrorAttri);

  FTokenAttributeTable[stkCssSpace] := FCssWhitespaceAttri;
  FTokenAttributeTable[stkCssSelector] := FCssSelectorAttri;
  FTokenAttributeTable[stkCssSelectorUndef] := FCssSelectorUndefAttri;
  FTokenAttributeTable[stkCssSelectorClass] := FCssSelectorClassAttri;
  FTokenAttributeTable[stkCssSelectorId] := FCssSelectorIdAttri;
  FTokenAttributeTable[stkCssSpecial] := FCssSpecialAttri;
  FTokenAttributeTable[stkCssComment] := FCssCommentAttri;
  FTokenAttributeTable[stkCssProp] := FCssPropAttri;
  FTokenAttributeTable[stkCssPropUndef] := FCssPropUndefAttri;
  FTokenAttributeTable[stkCssVal] := FCssValAttri;
  FTokenAttributeTable[stkCssValUndef] := FCssValUndefAttri;
  FTokenAttributeTable[stkCssValString] := FCssValStringAttri;
  FTokenAttributeTable[stkCssValNumber] := FCssValNumberAttri;
  FTokenAttributeTable[stkCssSymbol] := FCssSymbolAttri;
  FTokenAttributeTable[stkCssError] := FCssErrorAttri;

  // ECMAScript
  EsMakeMethodTables;

  FEsWhitespaceAttri := CreateAttrib('Es: Whitespace'); 
  FEsWhitespaceAttri.Background := $FFF0F0;
  AddAttribute(FEsWhitespaceAttri);

  FEsIdentifierAttri := CreateAttrib('Es: Identifier');
  FEsIdentifierAttri.Foreground := clBlue;
  AddAttribute(FEsIdentifierAttri);

  FEsKeyAttri := CreateAttrib('Es: Key'); 
  FEsKeyAttri.Style := [fsBold];
  AddAttribute(FEsKeyAttri);

  FEsCommentAttri := CreateAttrib('Es: Comment');    
  FEsCommentAttri.Foreground := clGreen;
  AddAttribute(FEsCommentAttri);

  FEsStringAttri := CreateAttrib('Es: String');
  FEsStringAttri.Foreground := clRed;
  AddAttribute(FEsStringAttri);

  FEsNumberAttri := CreateAttrib('Es: Number');
  FEsNumberAttri.Foreground := clFuchsia;
  AddAttribute(FEsNumberAttri);

  FEsSymbolAttri := CreateAttrib('Es: Symbol');
  AddAttribute(FEsSymbolAttri);

  FEsErrorAttri := CreateAttrib('Es: Error');  
  FEsErrorAttri.Foreground := clRed;
  FEsErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FEsErrorAttri);

  FTokenAttributeTable[stkEsSpace] := FEsWhitespaceAttri;
  FTokenAttributeTable[stkEsIdentifier] := FEsIdentifierAttri;
  FTokenAttributeTable[stkEsKeyword] := FEsKeyAttri;
  FTokenAttributeTable[stkEsComment] := FEsCommentAttri;
  FTokenAttributeTable[stkEsString] := FEsStringAttri;
  FTokenAttributeTable[stkEsNumber] := FEsNumberAttri;
  FTokenAttributeTable[stkEsSymbol] := FEsSymbolAttri;
  FTokenAttributeTable[stkEsError] := FEsErrorAttri;

  // Php
  PhpMakeMethodTables;

  FPhpWhitespaceAttri := CreateAttrib('Php: Whitespace');   
  FPhpWhitespaceAttri.Background := $F5F5F5;
  AddAttribute(FPhpWhitespaceAttri);

  FPhpInlineTextAttri := CreateAttrib('PhpCli: Inline text');
  AddAttribute(FPhpInlineTextAttri);

  FPhpIdentifierAttri := CreateAttrib('Php: Identifier');  
  FPhpIdentifierAttri.Foreground := clMaroon;
  AddAttribute(FPhpIdentifierAttri);

  FPhpKeyAttri := CreateAttrib('Php: Keyword'); 
  FPhpKeyAttri.Foreground := clBlue;
  AddAttribute(FPhpKeyAttri);

  FPhpFunctionAttri := CreateAttrib('Php: Function');  
  FPhpFunctionAttri.Foreground := clRed;
  AddAttribute(FPhpFunctionAttri);

  FPhpVariableAttri := CreateAttrib('Php: Variable'); 
  FPhpVariableAttri.Foreground := clTeal;
  AddAttribute(FPhpVariableAttri);

  FPhpConstAttri := CreateAttrib('Php: Constant');  
  FPhpConstAttri.Foreground := $0080FF;
  AddAttribute(FPhpConstAttri);

  FPhpStringAttri := CreateAttrib('Php: String'); 
  FPhpStringAttri.Foreground := clFuchsia;
  AddAttribute(FPhpStringAttri);

  FPhpStringSpecialAttri := CreateAttrib('Php: String special');
  FPhpStringSpecialAttri.Background := $EAEAEA;
  FPhpStringSpecialAttri.Foreground := clFuchsia;
  AddAttribute(FPhpStringSpecialAttri);

  FPhpCommentAttri := CreateAttrib('Php: Comment');  
  FPhpCommentAttri.Foreground := clGreen;
  FPhpCommentAttri.Style := [fsItalic];
  AddAttribute(FPhpCommentAttri);

  FPhpDocCommentAttri := CreateAttrib('Php: DocComment'); 
  FPhpDocCommentAttri.Foreground := clGreen;
  FPhpDocCommentAttri.Style := [fsBold, fsItalic];
  AddAttribute(FPhpDocCommentAttri);

  FPhpSymbolAttri := CreateAttrib('Php: Symbol');
  AddAttribute(FPhpSymbolAttri);

  FPhpNumberAttri := CreateAttrib('Php: Number'); 
  FPhpNumberAttri.Foreground := clPurple;
  AddAttribute(FPhpNumberAttri);

  FPhpErrorAttri := CreateAttrib('Php: Error'); 
  FPhpErrorAttri.Foreground := clRed;
  FPhpErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FPhpErrorAttri);

  FTokenAttributeTable[stkPhpSpace] := FHtmlWhitespaceAttri;
  FTokenAttributeTable[stkPhpIdentifier] := FPhpIdentifierAttri;
  FTokenAttributeTable[stkPhpKeyword] := FPhpKeyAttri;
  FTokenAttributeTable[stkPhpFunction] := FPhpFunctionAttri;
  FTokenAttributeTable[stkPhpVariable] := FPhpVariableAttri;
  FTokenAttributeTable[stkPhpConst] := FPhpConstAttri;
  FTokenAttributeTable[stkPhpString] := FPhpStringAttri;
  FTokenAttributeTable[stkPhpStringSpecial] := FPhpStringSpecialAttri;
  FTokenAttributeTable[stkPhpComment] := FPhpCommentAttri;
  FTokenAttributeTable[stkPhpDocComment] := FPhpDocCommentAttri;
  FTokenAttributeTable[stkPhpSymbol] := FPhpSymbolAttri;
  FTokenAttributeTable[stkPhpNumber] := FPhpNumberAttri;
  FTokenAttributeTable[stkPhpError] := FPhpErrorAttri;

  // PhpCli
  FTokenAttributeTable[stkPhpInlineText] := FPhpInlineTextAttri;

  // Global
  FInactiveAttri := CreateAttrib('Global: Inactive');
  FInactiveAttri.Foreground := clInactiveCaptionText;
  AddAttribute(FInactiveAttri);

  FTokenAttributeTable[stkNull] := nil;
  SetAttributesOnChange(DefHighlightChange);
end;

destructor TSynWebEngine.Destroy;
var
  i: Integer;
begin
  for i := FAttributes.Count - 1 downto 0 do
    TSynHighlighterAttributes(FAttributes.Objects[i]).Free;                                        
  FAttributes.Free;
  FOptions.Free;
  for i := 0 to FNotifyList.Count - 1 do
    TSynWebBase(FNotifyList[i]).Engine := nil;
  FNotifyList.Free;
  FPhpHereDocList.Free;
  inherited Destroy;
end;

// Html ------------------------------------------------------------------------

procedure TSynWebEngine.HtmlMakeMethodTables;
var
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
  pF2: PSynWebIdent2FuncTableFunc;
begin
  FHtmlRangeProcTable[srsHtmlText] := HtmlRangeTextProc;
  FHtmlRangeProcTable[srsHtmlComment] := HtmlRangeCommentProc;
  FHtmlRangeProcTable[srsHtmlCommentClose] := HtmlRangeCommentCloseProc;
  FHtmlRangeProcTable[srsHtmlTag] := HtmlRangeTagProc;
  FHtmlRangeProcTable[srsHtmlTagClose] := HtmlRangeTagCloseProc;
  FHtmlRangeProcTable[srsHtmlTagDOCTYPE] := HtmlRangeTagDOCTYPEProc;
  FHtmlRangeProcTable[srsHtmlTagCDATA] := HtmlRangeTagCDATAProc;
  FHtmlRangeProcTable[srsHtmlTagKey] := HtmlRangeTagKeyProc;
  FHtmlRangeProcTable[srsHtmlTagKeyEq] := HtmlRangeTagKeyEqProc;
  FHtmlRangeProcTable[srsHtmlTagKeyValue] := HtmlRangeTagKeyValueProc;
  FHtmlRangeProcTable[srsHtmlTagKeyValueQuoted1] := HtmlRangeTagKeyValueQuoted1Proc;
  FHtmlRangeProcTable[srsHtmlTagKeyValueQuoted2] := HtmlRangeTagKeyValueQuoted2Proc;

  pF := PSynWebIdentFuncTableFunc(@FHtmlTagIdentFuncTable);
  for I := Low(FHtmlTagIdentFuncTable) to High(FHtmlTagIdentFuncTable) do
  begin
    pF^ := HtmlTagUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_TagsFuncTable.inc}

  pF := PSynWebIdentFuncTableFunc(@FHtmlAttrIdentFuncTable);
  for I := Low(FHtmlTagIdentFuncTable) to High(FHtmlAttrIdentFuncTable) do
  begin
    pF^ := HtmlAttrUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_AttrsFuncTable.inc}

  pF2 := PSynWebIdent2FuncTableFunc(@FHtmlSpecialIdentFuncTable);
  for I := Low(FHtmlSpecialIdentFuncTable) to High(FHtmlSpecialIdentFuncTable) do
  begin
    pF2^ := HtmlSpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_SpecialFuncTable.inc}
end;

procedure TSynWebEngine.HtmlNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FHtmlRangeProcTable[HtmlGetRange];
end;

function TSynWebEngine.HtmlGetRange: TSynWebHtmlRangeState;
begin
  Result := TSynWebHtmlRangeState(GetRangeInt(4, 13));
end;

procedure TSynWebEngine.HtmlSetRange(const ARange: TSynWebHtmlRangeState);
begin
  SetRangeInt(4, 13, Longword(ARange));
end;

function TSynWebEngine.HtmlGetTag: Integer;
begin
  Result := GetRangeInt(7, 0);
end;

procedure TSynWebEngine.HtmlSetTag(const ATag: Integer);
begin
  SetRangeInt(7, 0, Longword(ATag));
end;

function TSynWebEngine.HtmlCheckNull(ADo: Boolean = True): Boolean;
begin
  if FInstance^.FLine[FInstance^.FRun] = #0 then
  begin
    Result := True;
    if ADo then
      NullProc;
  end else
    Result := False;
end;

procedure TSynWebEngine.HtmlSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkHtmlSpace;
end;

procedure TSynWebEngine.HtmlAmpersandProc;
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
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0;
        // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']);
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
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
      // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'];
      if HtmlSpecialCheck(FInstance^.FTokenPos + 1, FInstance^.FRun -
        FInstance^.FTokenPos - 1) = -1 then
        FInstance^.FTokenID := stkHtmlError;
    end;
  if FInstance^.FLine[FInstance^.FRun] = ';' then
    Inc(FInstance^.FRun)
  else
    FInstance^.FTokenID := stkHtmlError;
end;

procedure TSynWebEngine.HtmlBraceOpenProc;
begin
  if PhpCheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '/':
    begin
      Inc(FInstance^.FRun);
      SetRangeBit(12, True);
    end;
    '?':
    begin
      if FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict then
        Inc(FInstance^.FRun);
      SetRangeBit(12, False);
    end;
    '!':
    begin
      Inc(FInstance^.FRun);
      if (FInstance^.FLine[FInstance^.FRun] = '-') and
        (FInstance^.FLine[FInstance^.FRun + 1] = '-') then
      begin
        Inc(FInstance^.FRun, 2);
        HtmlSetRange(srsHtmlComment);
        if (FInstance^.FLine[FInstance^.FRun] = #0) or PhpCheckBegin(False) then
          FInstance^.FTokenID := stkHtmlComment
        else
          HtmlRangeCommentProc;
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
          HtmlSetRange(srsHtmlTagCDATA);
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
            SetRangeInt(2, 7, 0);
            HtmlSetRange(srsHtmlTagDOCTYPE);
          end else
            FInstance^.FTokenID := stkHtmlError;
      Exit;
    end;
    else
      SetRangeBit(12, False);
  end;
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) <> 0 then
  begin
    FInstance^.FTokenID := stkHtmlTag;
    HtmlSetRange(srsHtmlTag);
  end else
    FInstance^.FTokenID := stkHtmlError;
end;

procedure TSynWebEngine.HtmlErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkHtmlError;
end;

procedure TSynWebEngine.HtmlRangeTextProc;
begin
  case FInstance^.FLine[FInstance^.FRun] of
    #0:
      NullProc;
    #1..#32:
      HtmlSpaceProc;
    '<':
      HtmlBraceOpenProc;
    '>':
      HtmlErrorProc;
    '&':
      HtmlAmpersandProc;
    else
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) <> 0;
      // until FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>', '&'];
      FInstance^.FTokenID := stkHtmlText;
  end;
end;

procedure TSynWebEngine.HtmlRangeCommentProc;
begin
  if HtmlCheckNull or PhpCheckBegin then
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
            HtmlSetRange(srsHtmlText);
          end else
          begin
            HtmlSetRange(srsHtmlCommentClose);
            if (FInstance^.FLine[FInstance^.FRun] <> #0) and not PhpCheckBegin(False) then
            begin
              HtmlRangeCommentCloseProc;
              Exit;
            end;
          end;
          Break;
        end;
      end;
      '<':
        if PhpCheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
    end;
  until False;
  FInstance^.FTokenID := stkHtmlComment;
end;

procedure TSynWebEngine.HtmlRangeCommentCloseProc;
begin
  if HtmlCheckNull or PhpCheckBegin then
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
        HtmlSetRange(srsHtmlText);
        Break;
      end;
      '<':
        if PhpCheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
    end;
  until False;
  FInstance^.FTokenID := stkHtmlComment;
end;

procedure TSynWebEngine.HtmlRangeTagDOCTYPEProc;
begin
  case GetRangeInt(2, 7) of
    0:
    begin
      Inc(FInstance^.FRun, 7);
      FInstance^.FTokenID := stkHtmlTagName;
      SetRangeInt(2, 7, 1);
    end;
    1:
      if not HtmlCheckNull and not PhpCheckBegin then
        case FInstance^.FLine[FInstance^.FRun] of
          #1..#32:
          begin
            HtmlSpaceProc;
            Exit;
          end;
          '>':
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkHtmlTag;
            SetRangeInt(2, 7, 0);
            HtmlSetRange(srsHtmlText);
            Exit;
          end;
          #39:
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #0 then
              FInstance^.FTokenID := stkHtmlError
            else
            begin
              SetRangeInt(2, 7, 2);
              if PhpCheckBegin(False) then
                FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
              else
                HtmlRangeTagDOCTYPEProc;
            end;
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #0 then
              FInstance^.FTokenID := stkHtmlError
            else
            begin
              SetRangeInt(2, 7, 3);
              if PhpCheckBegin(False) then
                FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
              else
                HtmlRangeTagDOCTYPEProc;
            end;
          end;
          else
            // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
            if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
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
      if not HtmlCheckNull then
        if PhpCheckBegin then
          Exit
        else
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
              #0:
              begin
                FInstance^.FTokenID := stkHtmlError;
                Break;
              end;
              '<':
                if PhpCheckBegin(False) then
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
      SetRangeInt(2, 7, 1);
    end;
    3:
    begin
      if not HtmlCheckNull then
        if PhpCheckBegin then
          Exit
        else
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
              #0:
              begin
                FInstance^.FTokenID := stkHtmlError;
                Break;
              end;
              '<':
                if PhpCheckBegin(False) then
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
      SetRangeInt(2, 7, 1);
    end;
  end;
end;

procedure TSynWebEngine.HtmlRangeTagCDATAProc;
begin
  if HtmlCheckNull or PhpCheckBegin then
    Exit;
  if FInstance^.FLine[FInstance^.FRun] in [#1..#32] then
  begin
    HtmlSpaceProc;
    Exit;
  end else
    if (FInstance^.FLine[FInstance^.FRun] = ']') and
      (FInstance^.FLine[FInstance^.FRun + 1] = ']') and
      (FInstance^.FLine[FInstance^.FRun + 2] = '>') then
    begin
      Inc(FInstance^.FRun, 3);
      FInstance^.FTokenID := stkHtmlTag;
      HtmlSetRange(srsHtmlText);
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
            if PhpCheckBegin(False) then
              Break;
        end;
      until False;
      FInstance^.FTokenID := stkHtmlText;
    end;
end;

procedure TSynWebEngine.HtmlRangeTagProc;
var
  ID: Integer;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 16) = 0;
  // until not (FInstance^.FLine[FInstance^.FRun] In ['a'..'z', 'A'..'Z', '_', '0'..'9']);
  FInstance^.FTokenID := HtmlTagCheck;
  ID := HtmlGetTag - 1;
  if GetRangeBit(12) then
  begin
    if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      FInstance^.FTokenID := stkHtmlError;
    HtmlSetRange(srsHtmlTagClose);
  end else
  begin
    if (ID <> -1) and ((FInstance^.FLine[FInstance^.FTokenPos - 1] = '?') xor
      (TSynWeb_TagsData[ID] and (1 shl 29) <> 0)) then
      FInstance^.FTokenID := stkHtmlError;
    HtmlSetRange(srsHtmlTagKey);
  end;
end;

procedure TSynWebEngine.HtmlRangeTagCloseProc;
begin
  if HtmlCheckNull or PhpCheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      HtmlSpaceProc;
    '>':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkHtmlTag;
      HtmlSetRange(srsHtmlText);
    end;
    else
      FInstance^.FTokenID := stkHtmlError;
      repeat
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 1) <> 0;
        // until not (FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>']) do
        if (FInstance^.FLine[FInstance^.FRun] = '<') and not PhpCheckBegin(False) then
          Continue
        else
          Break;
      until False;
  end;
end;

procedure TSynWebEngine.HtmlRangeTagKeyProc;
var
  ID: Integer;
begin
  if HtmlCheckNull or PhpCheckBegin then
    Exit;
  ID := HtmlGetTag - 1;
  if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 29) <> 0) then
    if (FInstance^.FLine[FInstance^.FRun] = '?') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '>') then
    begin
      Inc(FInstance^.FRun, 2);
      FInstance^.FTokenID := stkHtmlTag;
      HtmlSetRange(srsHtmlText);
      Exit;
    end else
      if FInstance^.FLine[FInstance^.FRun] = '>' then
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkHtmlError;
        HtmlSetRange(srsHtmlText);
        Exit;
      end;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      HtmlSpaceProc;
    '/':
      if not GetRangeBit(12) and (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
        (FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict) and
        (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      begin
        Inc(FInstance^.FRun, 2);
        FInstance^.FTokenID := stkHtmlTag;
        HtmlSetRange(srsHtmlText);
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
        if not GetRangeBit(12) and ((FInstance^.FRun < 2) or
          (FInstance^.FLine[FInstance^.FRun - 2] <> '/')) then
          if (ID = HtmlTagID_Style) and FInstance^.FOptions.FCssEmbeded then
          begin
            SetHighlighterType(shtCss, True, True, True);
            Exit;
          end else
            if (ID = HtmlTagID_Script) then
              if GetRangeBit(28) and FInstance^.FOptions.FPhpEmbeded then
              begin
                SetRangeInt(17, 0, 0);
                PhpBegin(spotHtml);
                Exit;
              end else
                if FInstance^.FOptions.FEsEmbeded then
                begin
                  SetHighlighterType(shtEs, True, True, True);
                  Exit;
                end;
      HtmlSetRange(srsHtmlText);
    end;
    else
      // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
        HtmlErrorProc
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
          FInstance^.FTokenID := HtmlAttrCheck;
          if ID = HtmlTagID_Script then
            SetRangeBit(27, FInstance^.FTokenLastID = HtmlAttrID_Language);
        end;
      end;
      HtmlSetRange(srsHtmlTagKeyEq);
  end;
end;

procedure TSynWebEngine.HtmlRangeTagKeyEqProc;
begin
  if HtmlCheckNull or PhpCheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      HtmlSpaceProc;
    '=':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkHtmlSymbol;
      HtmlSetRange(srsHtmlTagKeyValue);
    end;
    else
      HtmlSetRange(srsHtmlTagKey);
      HtmlRangeTagKeyProc;
      if FInstance^.FOptions.FHtmlVersion >= shvXHtml10Strict then
        FInstance^.FTokenID := stkHtmlError;
  end;
end;

procedure TSynWebEngine.HtmlRangeTagKeyValueProc;
var
  ID: Integer;
begin
  if HtmlCheckNull or PhpCheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      HtmlSpaceProc;
    #39:
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = #0 then
      begin
        HtmlSetRange(srsHtmlTagKey);
        FInstance^.FTokenID := stkHtmlError;
      end else
      begin
        HtmlSetRange(srsHtmlTagKeyValueQuoted1);
        if PhpCheckBegin(False) then
          FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
        else
          HtmlRangeTagKeyValueQuoted1Proc;
      end;
    end;
    '"':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = #0 then
      begin
        HtmlSetRange(srsHtmlTagKey);
        FInstance^.FTokenID := stkHtmlError;
      end else
      begin
        HtmlSetRange(srsHtmlTagKeyValueQuoted2);
        if PhpCheckBegin(False) then
          FInstance^.FTokenID := stkHtmlTagKeyValueQuoted
        else
          HtmlRangeTagKeyValueQuoted2Proc;
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
        if not GetRangeBit(12) and ((FInstance^.FRun = 0) or
          (FInstance^.FLine[FInstance^.FRun - 2] <> '/')) then
        begin
          ID := HtmlGetTag - 1;
          if (ID = HtmlTagID_Style) and FInstance^.FOptions.FCssEmbeded then
          begin
            SetHighlighterType(shtCss, True, True, True);
            Exit;
          end else
            if (ID = HtmlTagID_Script) then
              if GetRangeBit(28) and FInstance^.FOptions.FPhpEmbeded then
              begin
                SetRangeInt(17, 0, 0);
                PhpBegin(spotHtml);
                Exit;
              end else
                if FInstance^.FOptions.FEsEmbeded then
                begin
                  SetHighlighterType(shtEs, True, True, True);
                  Exit;
                end;
        end;
        HtmlSetRange(srsHtmlText);
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
              if PhpCheckBegin(False) then
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
        if GetRangeBit(27) then
          SetRangeBit(28, UpperCase(GetToken) = 'PHP');
        HtmlSetRange(srsHtmlTagKey);
      end;
  end;
end;

procedure TSynWebEngine.HtmlRangeTagKeyValueQuoted1Proc;
begin
  if not HtmlCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<']) do
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkHtmlError;
            Break;
          end;
          '<':
            if PhpCheckBegin(False) then
            begin
              FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
              Exit;
            end else
              Inc(FInstance^.FRun);
          #39:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
            if GetRangeBit(27) then
              SetRangeBit(28, UpperCase(GetToken) = #39'PHP'#39);
            Break;
          end;
        end;
      until False;
  HtmlSetRange(srsHtmlTagKey);
end;

procedure TSynWebEngine.HtmlRangeTagKeyValueQuoted2Proc;
begin
  if not HtmlCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<']) do
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkHtmlError;
            Break;
          end;
          '<':
            if PhpCheckBegin(False) then
            begin
              FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
              Exit;
            end else
              Inc(FInstance^.FRun);
          '"':
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkHtmlTagKeyValueQuoted;
            if GetRangeBit(27) then
              SetRangeBit(28, UpperCase(GetToken) = '"PHP"');
            Break;
          end;
        end;
      until False;
  HtmlSetRange(srsHtmlTagKey);
end;

function TSynWebEngine.HtmlTagKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.HtmlTagCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  FInstance^.FTokenLastID := -1;
  if HashKey <= HtmlTagMaxKeyHash then
    Result := FHtmlTagIdentFuncTable[HashKey]
  else
    Result := stkHtmlTagNameUndef;
  HtmlSetTag(FInstance^.FTokenLastID + 1);
end;

{$I SynHighlighterWeb_TagsFunc.inc}

function TSynWebEngine.HtmlAttrKeyComp(const ID: Integer): Boolean;
var
  I, tag: Integer;
  Temp: PChar;
  aKey: String;
begin
  tag := HtmlGetTag - 1;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.HtmlAttrCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  FInstance^.FTokenLastID := -1;
  if HashKey <= HtmlAttrMaxKeyHash then
    Result := FHtmlAttrIdentFuncTable[HashKey]
  else
    Result := stkHtmlTagKeyUndef;
end;

{$I SynHighlighterWeb_AttrsFunc.inc}

function TSynWebEngine.HtmlSpecialKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialCheck(AStart, ALen: Integer): Integer;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  if (HashKey > HtmlSpecialMaxKeyHash) or not FHtmlSpecialIdentFuncTable[HashKey] then
    FInstance^.FTokenLastID := -1;
  Result := FInstance^.FTokenLastID;
end;

{$I SynHighlighterWeb_SpecialFunc.inc}

// Css -------------------------------------------------------------------------

procedure TSynWebEngine.CssMakeMethodTables;
var
  c: char;
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
  pF2: PSynWebIdent2FuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        FCssProcTable[c] := NullProc;
      #1..#32:
        FCssProcTable[c] := CssSpaceProc;
      '@':
        FCssProcTable[c] := CssAtKeywordProc;
      '/':
        FCssProcTable[c] := CssSlashProc;
      '<':
        FCssProcTable[c] := CssBraceOpenProc;
      '{':
        FCssProcTable[c] := CssCurlyBraceOpenProc;
      '}':
        FCssProcTable[c] := CssCurlyBraceCloseProc;
      '*', '>':
        FCssProcTable[c] := CssChildAnySelectorProc;
      '[':
        FCssProcTable[c] := CssAttribProc;
      '#':
        FCssProcTable[c] := CssHashProc;
      '.':
        FCssProcTable[c] := CssDotProc;
      ',':
        FCssProcTable[c] := CssCommaProc;
      ':':
        FCssProcTable[c] := CssColonProc;
      ';':
        FCssProcTable[c] := CssSemiColonProc;
      '!':
        FCssProcTable[c] := CssExclamationProc;
      #39, '"':
        FCssProcTable[c] := CssStringProc;
      '+':
        FCssProcTable[c] := CssPlusProc;
      '-':
        FCssProcTable[c] := CssMinusProc;
      '0'..'9':
        FCssProcTable[c] := CssNumberProc;
      'a'..'z', 'A'..'Z', '\':
        FCssProcTable[c] := CssIdentProc;
      else
        FCssProcTable[c] := CssErrorProc;
    end;

  FCssRangeProcTable[srsCssRuleset] := CssRangeRulesetProc;
  FCssRangeProcTable[srsCssSelectorAttrib] := CssRangeSelectorAttribProc;
  FCssRangeProcTable[srsCssSelectorPseudo] := CssRangeSelectorPseudoProc;
  FCssRangeProcTable[srsCssAtKeyword] := CssRangeAtKeywordProc;
  FCssRangeProcTable[srsCssComment] := CssRangeCommentProc;
  FCssRangeProcTable[srsCssProp] := CssRangePropProc;
  FCssRangeProcTable[srsCssPropVal] := CssRangePropValProc;
  FCssRangeProcTable[srsCssPropValStr] := CssRangePropValStrProc;
  FCssRangeProcTable[srsCssPropValRgb] := CssRangePropValRgbProc;
  FCssRangeProcTable[srsCssPropValSpecial] := CssRangePropValSpecialProc;
  FCssRangeProcTable[srsCssPropValImportant] := CssRangePropValImportantProc;
  FCssRangeProcTable[srsCssPropValUrl] := CssRangePropValUrlProc;
  FCssRangeProcTable[srsCssPropValRect] := CssRangePropValRectProc;
  FCssRangeProcTable[srsCssPropValFunc] := CssRangePropValFuncProc;

  pF := PSynWebIdentFuncTableFunc(@FCssPropIdentFuncTable);
  for I := Low(FCssPropIdentFuncTable) to High(FCssPropIdentFuncTable) do
  begin
    pF^ := CssPropUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssPropsFuncTable.inc}

  pF := PSynWebIdentFuncTableFunc(@FCssValIdentFuncTable);
  for I := Low(FCssValIdentFuncTable) to High(FCssValIdentFuncTable) do
  begin
    pF^ := CssValUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssValsFuncTable.inc}

  pF2 := PSynWebIdent2FuncTableFunc(@FCssSpecialIdentFuncTable);
  for I := Low(FCssSpecialIdentFuncTable) to High(FCssSpecialIdentFuncTable) do
  begin
    pF2^ := CssSpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_CssSpecialFuncTable.inc}
end;

procedure TSynWebEngine.CssNextBg;
begin
  CssUpdateBg;
  FInstance^.FNextProcTable := CssNext;
  CssNext;
end;

procedure TSynWebEngine.CssNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FCssRangeProcTable[CssGetRange];
end;

procedure TSynWebEngine.CssUpdateBg;
begin
  if TSynWebCssRangeState(GetRangeInt(4, 13)) in
    [TSynWebCssRangeStateRulesetBegin..TSynWebCssRangeStateRulesetEnd] then
    FInstance^.FSYN_ATTR_WHITESPACE := FCssRulesetWhitespaceAttri
  else
    FInstance^.FSYN_ATTR_WHITESPACE := FCssWhitespaceAttri;
  FTokenAttributeTable[stkCssSpace] := FInstance^.FSYN_ATTR_WHITESPACE;
end;

function TSynWebEngine.CssGetRange: TSynWebCssRangeState;
begin
  if GetRangeBit(12) then
    Result := srsCssComment
  else
    Result := TSynWebCssRangeState(GetRangeInt(4, 13));
end;

procedure TSynWebEngine.CssSetRange(const ARange: TSynWebCssRangeState);
begin
  if ARange = srsCssComment then
    SetRangeBit(12, True)
  else
  begin
    if not (ARange in [TSynWebCssRangeStateRulesetBegin..
      TSynWebCssRangeStateRulesetEnd]) and
      (TSynWebCssRangeState(GetRangeInt(4, 13)) in
      [TSynWebCssRangeStateRulesetBegin..TSynWebCssRangeStateRulesetEnd]) then
    begin
      SetRangeInt(4, 13, Longword(ARange));
      FInstance^.FNextProcTable := CssNextBg;
    end else
    begin
      SetRangeInt(4, 13, Longword(ARange));
      CssUpdateBg;
    end;
    if ARange = srsCssRuleset then
      SetRangeInt(11, 0, 0);
  end;
end;

function TSynWebEngine.CssGetProp: Integer;
begin
  Result := GetRangeInt(8, 0);
end;

procedure TSynWebEngine.CssSetProp(const AProp: Integer);
begin
  SetRangeInt(8, 0, Longword(AProp));
end;

function TSynWebEngine.CssCheckNull(ADo: Boolean = True): Boolean;
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
          SetHighlighterType(shtHtml, True, False, False);
          Next;
        end;
      end else
        Result := False;
    else
      Result := False;
  end;
end;

procedure TSynWebEngine.CssSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkCssSpace;
end;

procedure TSynWebEngine.CssAtKeywordProc;
begin
  // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z']) then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) = 0 then
    CssErrorProc
  else
  begin
    CssSymbolProc;
    CssSetRange(srsCssAtKeyword);
  end;
end;

procedure TSynWebEngine.CssSlashProc;
begin
  if FInstance^.FLine[FInstance^.FRun + 1] = '*' then
  begin
    Inc(FInstance^.FRun, 2);
    SetRangeBit(12, True); // CssSetRange(srsCssComment);
    if CssCheckNull(False) or PhpCheckBegin(False) then
      FInstance^.FTokenID := stkCssComment
    else
      CssRangeCommentProc;
  end else
    if (CssGetRange = srsCssPropVal) and GetRangeBit(8) then
    begin
      SetRangeBit(8, False);
      CssSymbolProc;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssBraceOpenProc;
begin
  if CssCheckNull or PhpCheckBegin then
    Exit;
  if (FInstance^.FLine[FInstance^.FRun + 1] = '!') and
    (FInstance^.FLine[FInstance^.FRun + 2] = '-') and
    (FInstance^.FLine[FInstance^.FRun + 3] = '-') then
  begin
    Inc(FInstance^.FRun, 4);
    FInstance^.FTokenID := stkHtmlComment;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssCurlyBraceOpenProc;
begin
  CssSymbolProc;
  CssSetRange(srsCssProp);
end;

procedure TSynWebEngine.CssCurlyBraceCloseProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    CssSymbolProc;
    CssSetRange(srsCssRuleset);
  end else
    if GetRangeBit(11) then
    begin
      SetRangeBit(11, False);
      CssSymbolProc;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssChildAnySelectorProc;
begin
  if FInstance^.FOptions.FCssVersion = scvCss21 then
    CssSymbolProc
  else
    CssErrorProc;
end;

procedure TSynWebEngine.CssAttribProc;
begin
  if FInstance^.FOptions.FCssVersion = scvCss1 then
    CssErrorProc
  else
  begin
    CssSymbolProc;
    CssSetRange(srsCssSelectorAttrib);
  end;
end;

procedure TSynWebEngine.CssHashProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    // if FInstance^.FLine[FInstance^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9'] and
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 10) <> 0) and
      // FInstance^.FLine[FInstance^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9'] and
      (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 10) <> 0) and
      // FInstance^.FLine[FInstance^.FRun+3] in ['a'..'f', 'A'..'F', '0'..'9'] then
      (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 3]] and (1 shl 10) <> 0) then
    begin
      CssSymbolProc;
      CssSetRange(srsCssPropValSpecial);
    end else
      CssErrorProc;
  end else
    // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z', '\']) or
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 8) = 0) or
      ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
      (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
      CssErrorProc
    else
    begin
      CssSymbolProc;
      SetRangeBit(8, True);
    end;
end;

procedure TSynWebEngine.CssDotProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    if FInstance^.FLine[FInstance^.FRun + 1] in ['0'..'9'] then
    begin
      FInstance^.FCssMask := $F5000000;
      CssNumberDefProc;
    end else
      CssErrorProc;
  end else
  begin
    // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 8) = 0) or
      ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
      (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
    begin
      CssErrorProc;
      Exit;
    end;
    CssSymbolProc;
    SetRangeBit(9, True);
  end;
end;

procedure TSynWebEngine.CssCommaProc;
var
  prop: Integer;
begin
  if CssGetRange = srsCssPropVal then
  begin
    prop := CssGetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 16) = 0) then
    begin
      CssErrorProc;
      Exit;
    end;
  end;
  CssSymbolProc;
end;

procedure TSynWebEngine.CssColonProc;
begin
  // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z']) then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) = 0 then
    CssErrorProc
  else
  begin
    CssSymbolProc;
    CssSetRange(srsCssSelectorPseudo);
  end;
end;

procedure TSynWebEngine.CssSemiColonProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    CssSymbolProc;
    CssSetRange(srsCssProp);
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssExclamationProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    CssSymbolProc;
    CssSetRange(srsCssPropValImportant);
    SetRangeBit(8, False);
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssStringProc;
var
  prop: Integer;
begin
  if CssGetRange = srsCssPropVal then
  begin
    FInstance^.FTokenID := stkCssValString;
    if FInstance^.FLine[FInstance^.FRun] = #39 then
    begin
      Inc(FInstance^.FRun);
      if not CssCustomStringProc(TSynWebCssString39, False) then
      begin
        CssSetRange(srsCssPropValStr);
        SetRangeBit(8, True);
      end;
    end else
    begin
      Inc(FInstance^.FRun);
      if not CssCustomStringProc(TSynWebCssString34, False) then
      begin
        CssSetRange(srsCssPropValStr);
        SetRangeBit(9, True);
      end;
    end;
    if FInstance^.FTokenID = stkCssValString then
    begin
      prop := CssGetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
        FInstance^.FTokenID := stkCssValUndef;
    end;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssPlusProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);
    // if FInstance^.FLine[FInstance^.FRun] in ['0'..'9', '.'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 13) <> 0 then
    begin
      FInstance^.FCssMask := $F5400000;
      CssNumberDefProc;
    end else
      FInstance^.FTokenID := stkCssError;
  end else
    if FInstance^.FOptions.FCssVersion = scvCss21 then
      CssSymbolProc
    else
      CssErrorProc;
end;

procedure TSynWebEngine.CssMinusProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);
    // if FInstance^.FLine[FInstance^.FRun] in ['0'..'9', '.'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 13) <> 0 then
    begin
      FInstance^.FCssMask := $8AA00000;
      CssNumberDefProc;
    end else
      FInstance^.FTokenID := stkCssError;
  end else
    if (CssGetRange = srsCssRuleset) and (FInstance^.FLine[FInstance^.FRun + 1] = '-') and
      (FInstance^.FLine[FInstance^.FRun + 2] = '>') then
    begin
      Inc(FInstance^.FRun, 3);
      FInstance^.FTokenID := stkHtmlComment;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssNumberProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    FInstance^.FCssMask := $F5400000;
    CssNumberDefProc;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssNumberDefProc;
var
  prop, OldRun: Integer;

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
    CssSetRange(srsCssPropValSpecial);
  end else
  begin
    OldRun := FInstance^.FRun;
    if CssIdentStartProc then
    begin
      prop := CssSpecialCheck(OldRun, FInstance^.FRun - OldRun);
      if prop <> -1 then
      begin
        FInstance^.FCssMask := FInstance^.FCssMask and TSynWeb_CssSpecialData[prop];
        CssSetRange(srsCssPropValSpecial);
        if (FInstance^.FLine[FInstance^.FRun] = '/') and
          (FInstance^.FLine[FInstance^.FRun + 1] <> '*') then
          SetRangeBit(8, True);
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
  prop := CssGetProp - 1;
  if (prop = -1) or (TSynWeb_CssPropsData[prop] and FInstance^.FCssMask = 0) then
    FInstance^.FTokenID := stkCssValUndef
  else
    FInstance^.FTokenID := stkCssValNumber;
end;

procedure TSynWebEngine.CssIdentProc;
begin
  if CssIdentStartProc then
  begin
    if (HtmlTagCheck = stkHtmlTagName) and
      (TSynWeb_TagsData[HtmlGetTag - 1] and (1 shl 30) = 0) then
      FInstance^.FTokenID := stkCssSelector
    else
      FInstance^.FTokenID := stkCssSelectorUndef;
  end else
    CssErrorProc;
end;

function TSynWebEngine.CssIdentStartProc: Boolean;
begin
  // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
  if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 8) = 0) or
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

function TSynWebEngine.CssCustomStringProc(AShl: Longword; ADo: Boolean): Boolean;
begin
  if CssCheckNull(ADo) then
  begin
    if (FInstance^.FOptions.FCssVersion > scvCss1) and (FInstance^.FRun > 0) and
      (FInstance^.FLine[FInstance^.FRun - 1] = '\') then
    begin
      Result := False;
      Exit;
    end;
    if not ADo then
      FInstance^.FTokenID := stkCssError;
    Result := True;
    Exit;
  end else
    if PhpCheckBegin(ADo) then
    begin
      if not ADo then
        FInstance^.FTokenID := stkCssValString;
      Result := False;
      Exit;
    end;
  Result := True;
  AShl := 1 shl AShl;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, AChar, '\', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and AShl = 0 do
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
          if not CssCheckNull(False) and not PhpCheckBegin(False) then
            Inc(FInstance^.FRun);
      end;
      else
        if CssCheckNull(False) then
        begin
          FInstance^.FTokenID := stkCssError;
          Exit;
        end else
          if PhpCheckBegin(False) then
          begin
            FInstance^.FTokenID := stkCssValString;
            Result := False;
            Exit;
          end else
            Inc(FInstance^.FRun);
    end;
  until False;
end;

function TSynWebEngine.CssNotWhitespace: Boolean;
begin
  Result := False;
  if CssCheckNull or PhpCheckBegin then
    Exit;
  // if FInstance^.FLine[FInstance^.FRun] in [#0..#32, '/'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 11) <> 0 then
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]]
  else
    Result := True;
end;

procedure TSynWebEngine.CssSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkCssSymbol;
end;

procedure TSynWebEngine.CssErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkCssError;
end;

procedure TSynWebEngine.CssRangeRulesetProc;
begin
  if GetRangeBit(8) then
  begin
    SetRangeBit(8, False);
    CssIdentStartProc;
    FInstance^.FTokenID := stkCssSelectorId;
  end else
    if GetRangeBit(9) then
    begin
      SetRangeBit(9, False);
      CssIdentStartProc;
      FInstance^.FTokenID := stkCssSelectorClass;
    end else
      FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.CssRangeSelectorAttribProc;

  procedure DoError;
  begin
    CssSetRange(srsCssRuleset);
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

  procedure DoEndAttrib;
  begin
    CssSymbolProc;
    CssSetRange(srsCssRuleset);
  end;

begin
  case GetRangeInt(3, 8) of
    0:
      if CssNotWhitespace then
        if CssIdentStartProc then
        begin
          FInstance^.FTokenID := stkCssVal;
          SetRangeInt(3, 8, 1);
        end else
          DoError;
    1:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          '=':
          begin
            CssSymbolProc;
            SetRangeInt(3, 8, 2);
          end;
          '|', '~':
          begin
            SetRangeInt(3, 8, 2);
            if FInstance^.FLine[FInstance^.FRun + 1] = '=' then
            begin
              Inc(FInstance^.FRun, 2);
              FInstance^.FTokenID := stkCssSymbol;
            end else
              CssErrorProc;
          end;
          ']':
            DoEndAttrib;
          else
            DoError;
        end;
    2:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          #39:
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString39, False) then
              SetRangeInt(3, 8, 5)
            else
              SetRangeInt(3, 8, 3);
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString34, False) then
              SetRangeInt(3, 8, 5)
            else
              SetRangeInt(3, 8, 4);
          end;
          else
            if CssIdentStartProc then
            begin
              FInstance^.FTokenID := stkCssValString;
              SetRangeInt(3, 8, 5);
            end else
              DoError;
        end;
    3:
      if CssCustomStringProc(TSynWebCssString39) then
        SetRangeInt(3, 8, 5);
    4:
      if CssCustomStringProc(TSynWebCssString34) then
        SetRangeInt(3, 8, 5);
    5:
      if CssNotWhitespace then
        if FInstance^.FLine[FInstance^.FRun] = ']' then
          DoEndAttrib
        else
          DoError;
  end;
end;

procedure TSynWebEngine.CssRangeSelectorPseudoProc;
var
  prop: Integer;
begin
  if not GetRangeBit(10) then
  begin
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '-']);
    prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
    if (prop = -1) or (TSynWeb_CssSpecialData[prop] and
      (1 shl (15 - Longword(FInstance^.FOptions.FCssVersion))) = 0) then
    begin
      FInstance^.FTokenID := stkCssError;
      CssSetRange(srsCssRuleset);
    end else
      if (prop <> CssSpecialID_Lang) then
      begin
        FInstance^.FTokenID := stkCssSpecial;
        CssSetRange(srsCssRuleset);
      end else
        if (FInstance^.FLine[FInstance^.FRun] = '(') then
        begin
          FInstance^.FTokenID := stkCssSpecial;
          SetRangeBit(10, True);
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          CssSetRange(srsCssRuleset);
        end;
  end else
    if not GetRangeBit(9) then
    begin
      CssSymbolProc;
      SetRangeBit(9, True);
    end else
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          ',':
            if GetRangeBit(8) then
            begin
              SetRangeBit(8, False);
              CssSymbolProc;
            end else
              CssErrorProc;
          ')':
          begin
            if GetRangeBit(8) then
              CssSymbolProc
            else
              CssErrorProc;
            CssSetRange(srsCssRuleset);
          end;
          else
            if CssIdentStartProc then
              if GetRangeBit(8) then
                FInstance^.FTokenID := stkCssError
              else
              begin
                FInstance^.FTokenID := stkCssVal;
                SetRangeBit(8, True);
              end else
            begin
              CssSetRange(srsCssRuleset);
              FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
              FInstance^.FTokenID := stkCssError;
            end;
        end;
end;

procedure TSynWebEngine.CssRangeAtKeywordProc;
var
  prop: Integer;

  procedure DoError;
  begin
    CssSetRange(srsCssRuleset);
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

  procedure AtImport;

    procedure AtImport_Medium(ASimple: Boolean);
    begin
      if CssNotWhitespace then
        if not ASimple and (FInstance^.FLine[FInstance^.FRun] = ';') then
        begin
          CssSymbolProc;
          CssSetRange(srsCssRuleset);
        end else
          if CssIdentStartProc then
          begin
            prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
              FInstance^.FTokenPos);
            if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
              FInstance^.FTokenID := stkCssValUndef
            else
              FInstance^.FTokenID := stkCssVal;
            SetRangeInt(4, 4, 9);
          end else
            DoError;
    end;

  begin
    case GetRangeInt(4, 4) of
      0:
        if CssNotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            #39:
            begin
              Inc(FInstance^.FRun);
              if CssCustomStringProc(TSynWebCssString39, False) then
                SetRangeInt(4, 4, 8)
              else
                SetRangeInt(4, 4, 1);
            end;
            '"':
            begin
              Inc(FInstance^.FRun);
              if CssCustomStringProc(TSynWebCssString34, False) then
                SetRangeInt(4, 4, 8)
              else
                SetRangeInt(4, 4, 2);
            end;
            else
              if not CssIdentStartProc then
                DoError
              else
                if (CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
                  FInstance^.FTokenPos) = CssSpecialID_Url) and
                  (FInstance^.FLine[FInstance^.FRun] = '(') then
                begin
                  FInstance^.FTokenID := stkCssVal;
                  SetRangeInt(4, 4, 3);
                end else
                begin
                  FInstance^.FTokenID := stkCssValUndef;
                  SetRangeInt(4, 4, 8);
                end;
          end;
      1:
        if CssCustomStringProc(TSynWebCssString39) then
          SetRangeInt(4, 4, 8);
      2:
        if CssCustomStringProc(TSynWebCssString34) then
          SetRangeInt(4, 4, 8);
      3:
        if FInstance^.FLine[FInstance^.FRun + 1] = #0 then
          DoError
        else
        begin
          CssSymbolProc;
          SetRangeInt(4, 4, 4);
        end;
      4:
        case FInstance^.FLine[FInstance^.FRun] of
          #39:
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString39, False) then
              SetRangeInt(4, 4, 7)
            else
              SetRangeInt(4, 4, 5);
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString34, False) then
              SetRangeInt(4, 4, 7)
            else
              SetRangeInt(4, 4, 6);
          end;
          #1..#32:
            CssSpaceProc;
          else
            if (FInstance^.FLine[FInstance^.FRun] = '/') and
              (FInstance^.FLine[FInstance^.FRun + 1] = '*') then
              CssSlashProc
            else
            begin
              if CssCheckNull or PhpCheckBegin then
                Exit;
              repeat
                // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
                while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
                  (1 shl 14) = 0 do
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
                    if CssCheckNull(False) or PhpCheckBegin(False) then
                      Break
                    else
                      Inc(FInstance^.FRun);
                  else
                    Break;
                end;
              until False;
              FInstance^.FTokenID := stkCssValString;
              SetRangeInt(4, 4, 7);
            end;
        end;
      5:
        if CssCustomStringProc(TSynWebCssString39) then
          SetRangeInt(4, 4, 7);
      6:
        if CssCustomStringProc(TSynWebCssString34) then
          SetRangeInt(4, 4, 7);
      7:
        if CssNotWhitespace then
          if FInstance^.FLine[FInstance^.FRun] = ')' then
          begin
            CssSymbolProc;
            SetRangeInt(4, 4, 8);
          end else
            DoError;
      8:
        AtImport_Medium(False);
      9:
        if CssNotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            ';':
            begin
              CssSymbolProc;
              CssSetRange(srsCssRuleset);
            end;
            ',':
            begin
              CssSymbolProc;
              SetRangeInt(4, 4, 10);
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
    prop: Integer;
  begin
    if CssNotWhitespace then
      if GetRangeBit(7) then
      begin
        SetRangeBit(7, False);
        case FInstance^.FLine[FInstance^.FRun] of
          ',':
            CssSymbolProc;
          '{':
          begin
            CssSymbolProc;
            SetRangeBit(11, True);
            CssSetRange(srsCssRuleset);
          end;
          else
            DoError;
        end;
      end else
        if CssIdentStartProc then
        begin
          prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
            FInstance^.FTokenPos);
          if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
            FInstance^.FTokenID := stkCssValUndef
          else
            FInstance^.FTokenID := stkCssVal;
          SetRangeBit(7, True);
        end else
          DoError;
  end;

  procedure AtPage;
  var
    prop: Integer;

    procedure AtPage_Declaration;
    begin
      SetRangeInt(11, 0, 0);
      CssSymbolProc;
      CssSetRange(srsCssProp);
    end;

  begin
    case GetRangeInt(2, 6) of
      0:
        if CssNotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            '{':
              AtPage_Declaration;
            ':':
              // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
              if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and
                (1 shl 8) = 0) or
                ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
                (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
                DoError
              else
              begin
                SetRangeInt(2, 6, 1);
                CssSymbolProc;
              end;
            else
              DoError;
          end;
      1:
      begin
        CssIdentStartProc;
        prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
        if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 11) = 0) then
          FInstance^.FTokenID := stkCssError
        else
          FInstance^.FTokenID := stkCssSpecial;
        SetRangeInt(2, 6, 2);
      end;
      2:
        if CssNotWhitespace then
          if FInstance^.FLine[FInstance^.FRun] = '{' then
            AtPage_Declaration
          else
            DoError;
    end;
  end;

  procedure AtCharset;
  begin
    case GetRangeInt(2, 6) of
      0:
        if CssNotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            #39:
            begin
              Inc(FInstance^.FRun);
              if CssCustomStringProc(TSynWebCssString39, False) then
                SetRangeInt(2, 6, 3)
              else
                SetRangeBit(6, True);
            end;
            '"':
            begin
              Inc(FInstance^.FRun);
              if CssCustomStringProc(TSynWebCssString34, False) then
                SetRangeInt(2, 6, 3)
              else
                SetRangeBit(7, True);
            end;
            else
              DoError;
          end;
      1:
        if CssCustomStringProc(TSynWebCssString39) then
          SetRangeInt(2, 6, 3);
      2:
        if CssCustomStringProc(TSynWebCssString34) then
          SetRangeInt(2, 6, 3);
      3:
        if CssNotWhitespace then
          if FInstance^.FLine[FInstance^.FRun] = ';' then
          begin
            CssSymbolProc;
            CssSetRange(srsCssRuleset);
          end else
            DoError;
    end;
  end;

begin
  if not GetRangeBit(10) then
  begin
    SetRangeBit(10, True);
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']);
    if GetRangeBit(11) then
    begin
      FInstance^.FTokenID := stkCssError;
      CssSetRange(srsCssRuleset);
    end else
      case CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos) of
        CssSpecialID_Import:
        begin
          SetRangeInt(2, 8, 0);
          FInstance^.FTokenID := stkCssSpecial;
        end;
        CssSpecialID_Media:
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            CssSetRange(srsCssRuleset);
          end else
          begin
            SetRangeInt(2, 8, 1);
            FInstance^.FTokenID := stkCssSpecial;
          end;
        CssSpecialID_Page:
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            CssSetRange(srsCssRuleset);
          end else
          begin
            SetRangeInt(2, 8, 2);
            FInstance^.FTokenID := stkCssSpecial;
          end;
        CssSpecialID_Charset:
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            CssSetRange(srsCssRuleset);
          end else
          begin
            SetRangeInt(2, 8, 3);
            FInstance^.FTokenID := stkCssSpecial;
          end;
        else
          FInstance^.FTokenID := stkCssError;
          CssSetRange(srsCssRuleset);
      end;
  end else
    case GetRangeInt(2, 8) of
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

procedure TSynWebEngine.CssRangePropProc;
begin
  if GetRangeBit(8) then
  begin
    if CssNotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
        '}':
        begin
          CssErrorProc;
          CssSetRange(srsCssRuleset);
        end;
        ':':
        begin
          CssSymbolProc;
          CssSetRange(srsCssPropVal);
          SetRangeBit(8, False);
        end;
        else
          CssErrorProc;
      end;
  end else
    if CssNotWhitespace then
      if CssIdentStartProc then
      begin
        FInstance^.FTokenID := CssPropCheck;
        SetRangeBit(8, True);
      end else
      begin
        CssSetProp(0);
        case FInstance^.FLine[FInstance^.FRun] of
          '}':
          begin
            CssSetRange(srsCssRuleset);
            CssSymbolProc;
            Exit;
          end;
          ':':
            CssSetRange(srsCssPropVal);
        end;
        CssErrorProc;
      end;
end;

procedure TSynWebEngine.CssRangePropValProc;
begin
  // if FInstance^.FLine[FInstance^.FRun] in [#0..#32, '/', '#', '!', ';', '}', '+', '-', '0'..'9', '.', ',', '"', #39, '<'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 12) <> 0 then
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]]
  else
    if CssIdentStartProc then
    begin
      FInstance^.FTokenID := CssValCheck;
      if TSynWeb_CssValsData[FInstance^.FTokenLastID][Longword(FInstance^.FOptions.FCssVersion)]
        [3] and (1 shl 31) <> 0 then
        if FInstance^.FLine[FInstance^.FRun] = '(' then
        begin
          SetRangeInt(3, 8, 0);
          case FInstance^.FTokenLastID of
            CssValID_Rgb:
              CssSetRange(srsCssPropValRgb);
            CssValID_Url:
              CssSetRange(srsCssPropValUrl);
            CssValID_Rect:
              CssSetRange(srsCssPropValRect);
            else
              CssSetRange(srsCssPropValFunc);
          end;
        end else
          FInstance^.FTokenID := stkCssValUndef;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssRangePropValStrProc;
var
  prop: Integer;
begin
  if GetRangeBit(8) then
  begin
    if CssCustomStringProc(TSynWebCssString39) then
    begin
      CssSetRange(srsCssPropVal);
      SetRangeBit(8, False);
    end;
  end else
    if CssCustomStringProc(TSynWebCssString34) then
    begin
      CssSetRange(srsCssPropVal);
      SetRangeBit(9, False);
    end;
  if FInstance^.FTokenID = stkCssValString then
  begin
    prop := CssGetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
      FInstance^.FTokenID := stkCssValUndef;
  end;
end;

procedure TSynWebEngine.CssRangePropValRgbProc;

  procedure NumberProc;
  begin
    if GetRangeBit(8) then
      FInstance^.FTokenID := stkCssError
    else
      FInstance^.FTokenID := stkCssValNumber;
    SetRangeBit(8, True);
    if FInstance^.FLine[FInstance^.FRun] = '+' then
      if FInstance^.FLine[FInstance^.FRun + 1] in ['0'..'9', '.'] then
        Inc(FInstance^.FRun)
      else
      begin
        CssErrorProc;
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
  if GetRangeBit(10) then
  begin
    if CssNotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
        ',':
          if GetRangeBit(8) then
          begin
            SetRangeBit(8, False);
            CssSymbolProc;
          end else
            CssErrorProc;
        '0'..'9', '.', '+':
          NumberProc;
        '%':
          if (FInstance^.FRun > 0) and (FInstance^.FLine[FInstance^.FRun - 1] in
            ['0'..'9']) then
            CssSymbolProc
          else
            CssErrorProc;
        ';':
        begin
          CssErrorProc;
          CssSetRange(srsCssProp);
          SetRangeInt(3, 8, 0);
        end;
        '}':
        begin
          CssErrorProc;
          CssSetRange(srsCssRuleset);
        end;
        ')':
        begin
          if GetRangeBit(8) then
            CssSymbolProc
          else
            CssErrorProc;
          CssSetRange(srsCssPropVal);
          SetRangeInt(3, 8, 0);
        end;
        else
          CssSetRange(srsCssPropVal);
          SetRangeInt(3, 8, 0);
          CssRangePropValProc;
          FInstance^.FTokenID := stkCssError;
      end;
  end else
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end;
end;

procedure TSynWebEngine.CssRangePropValFuncProc;
begin
  if GetRangeBit(10) then
    case GetRangeInt(2, 8) of
      0:
        if CssNotWhitespace then
          case FInstance^.FLine[FInstance^.FRun] of
            #39:
            begin
              Inc(FInstance^.FRun);
              if not CssCustomStringProc(TSynWebCssString39, False) then
                SetRangeBit(8, True);
            end;
            '"':
            begin
              Inc(FInstance^.FRun);
              if not CssCustomStringProc(TSynWebCssString34, False) then
                SetRangeBit(9, True);
            end;
            ',':
              CssSymbolProc;
            ';':
            begin
              CssErrorProc;
              CssSetRange(srsCssProp);
              SetRangeInt(3, 8, 0);
            end;
            '}':
            begin
              CssErrorProc;
              CssSetRange(srsCssRuleset);
            end;
            ')':
            begin
              CssSymbolProc;
              CssSetRange(srsCssPropVal);
              SetRangeInt(3, 8, 0);
            end;
            else
              if CssIdentStartProc then
                FInstance^.FTokenID := stkCssVal
              else
                CssErrorProc;
          end;
      1:
        if CssCustomStringProc(TSynWebCssString39) then
          SetRangeBit(8, False);
      2:
        if CssCustomStringProc(TSynWebCssString34) then
          SetRangeBit(9, False);
    end else
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end;
end;

procedure TSynWebEngine.CssRangePropValSpecialProc;
var
  prop: Integer;
begin
  if FInstance^.FLine[FInstance^.FRun] = '%' then
    CssSymbolProc
  else
    if (FInstance^.FRun > 0) and (FInstance^.FLine[FInstance^.FRun - 1] = '#') then
    begin
      Inc(FInstance^.FRun, 3);
      // if (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) and
      if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0) and
        // if (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9']) and
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 10) <> 0) and
        // if (FInstance^.FLine[FInstance^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9']) then
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 10) <> 0) then
        Inc(FInstance^.FRun, 3);
      prop := CssGetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 18) = 0) then
        FInstance^.FTokenID := stkCssValUndef
      else
        FInstance^.FTokenID := stkCssValNumber;
    end else
    begin
      CssIdentStartProc;
      FInstance^.FTokenID := stkCssSymbol;
    end;
  CssSetRange(srsCssPropVal);
end;

procedure TSynWebEngine.CssRangePropValImportantProc;

  procedure DoSymbol;
  begin
    if GetRangeBit(8) then
    begin
      SetRangeBit(8, False);
      CssSymbolProc;
    end else
      CssErrorProc;
  end;

begin
  if CssNotWhitespace then
    case FInstance^.FLine[FInstance^.FRun] of
      ';':
      begin
        DoSymbol;
        CssSetRange(srsCssProp);
      end;
      '}':
      begin
        DoSymbol;
        CssSetRange(srsCssRuleset);
      end;
      else
        if CssIdentStartProc then
        begin
          if GetRangeBit(8) then
            FInstance^.FTokenID := stkCssError
          else
          begin
            CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
            if FInstance^.FTokenLastID = CssSpecialID_Important then
              FInstance^.FTokenID := stkCssSpecial
            else
              FInstance^.FTokenID := stkCssError;
            SetRangeBit(8, True);
          end;
        end else
          CssErrorProc;
    end;
end;

procedure TSynWebEngine.CssRangePropValUrlProc;
begin
  if GetRangeBit(10) then
    case GetRangeInt(2, 8) of
      0:
        case FInstance^.FLine[FInstance^.FRun] of
          #39:
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString39, False) then
              SetRangeInt(2, 8, 3)
            else
              SetRangeBit(8, True);
          end;
          '"':
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString34, False) then
              SetRangeInt(2, 8, 3)
            else
              SetRangeBit(9, True);
          end;
          #1..#32:
            CssSpaceProc;
          ';':
          begin
            CssErrorProc;
            CssSetRange(srsCssProp);
            SetRangeInt(3, 8, 0);
          end;
          '}':
          begin
            CssErrorProc;
            CssSetRange(srsCssRuleset);
          end;
          ')':
          begin
            CssErrorProc;
            CssSetRange(srsCssPropVal);
            SetRangeInt(3, 8, 0);
          end;
          else
            if (FInstance^.FLine[FInstance^.FRun] = '/') and
              (FInstance^.FLine[FInstance^.FRun + 1] = '*') then
              CssSlashProc
            else
            begin
              if CssCheckNull or PhpCheckBegin then
                Exit;
              repeat
                // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
                while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
                  (1 shl 14) = 0 do
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
                    if CssCheckNull(False) or PhpCheckBegin(False) then
                      Break
                    else
                      Inc(FInstance^.FRun);
                  else
                    Break;
                end;
              until False;
              FInstance^.FTokenID := stkCssValString;
              SetRangeInt(2, 8, 3);
            end;
        end;
      1:
        if CssCustomStringProc(TSynWebCssString39) then
          SetRangeInt(2, 8, 3);
      2:
        if CssCustomStringProc(TSynWebCssString34) then
          SetRangeInt(2, 8, 3);
      3:
        if FInstance^.FLine[FInstance^.FRun] = ')' then
        begin
          CssSymbolProc;
          SetRangeInt(3, 8, 0);
          CssSetRange(srsCssPropVal);
        end else
          if CssNotWhitespace then
          begin
            SetRangeInt(3, 8, 0);
            CssSetRange(srsCssPropVal);
            CssRangePropValProc;
            FInstance^.FTokenID := stkCssError;
          end;
    end else
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end;
end;

procedure TSynWebEngine.CssRangePropValRectProc;

  procedure Shape_LengthProc;
  var
    prop, OldRun: Integer;

    procedure CheckOther;
    begin
      if GetRangeBit(8) then
        FInstance^.FTokenID := stkCssError
      else
        if (FInstance^.FRun - FInstance^.FTokenPos = 1) and
          (FInstance^.FLine[FInstance^.FRun - 1] = '0') then
          FInstance^.FTokenID := stkCssValNumber
        else
          FInstance^.FTokenID := stkCssValUndef;
      SetRangeBit(8, True);
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
    if CssIdentStartProc then
    begin
      prop := CssSpecialCheck(OldRun, FInstance^.FRun - OldRun);
      if prop <> -1 then
      begin
        FInstance^.FRun := OldRun;
        SetRangeBit(9, True);
        if (TSynWeb_CssSpecialData[prop] and (1 shl 28) = 0) or GetRangeBit(8) then
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
  if not GetRangeBit(10) then
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end else
    if GetRangeBit(9) then
    begin
      CssIdentStartProc;
      if GetRangeBit(8) then
        FInstance^.FTokenID := stkCssError
      else
        FInstance^.FTokenID := stkCssSymbol;
      SetRangeBit(9, False);
      SetRangeBit(8, True);
    end else
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
          ',':
            if GetRangeBit(8) then
            begin
              SetRangeBit(8, False);
              CssSymbolProc;
            end else
              CssErrorProc;
          '0'..'9', '.':
            Shape_LengthProc;
          ')':
          begin
            if GetRangeBit(8) then
              CssSymbolProc
            else
              CssErrorProc;
            CssSetRange(srsCssPropVal);
            SetRangeInt(3, 8, 0);
          end;
          ';':
          begin
            CssErrorProc;
            CssSetRange(srsCssProp);
            SetRangeInt(3, 8, 0);
          end;
          '}':
          begin
            CssErrorProc;
            CssSetRange(srsCssRuleset);
            SetRangeInt(3, 8, 0);
          end;
          else
            if not CssIdentStartProc then
              CssErrorProc
            else
            begin
              if GetRangeBit(8) then
                FInstance^.FTokenID := stkCssError
              else
                if CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
                  FInstance^.FTokenPos) = CssSpecialID_Auto then
                  FInstance^.FTokenID := stkCssVal
                else
                  FInstance^.FTokenID := stkCssValUndef;
              SetRangeBit(8, True);
            end;
        end;
end;

procedure TSynWebEngine.CssRangeCommentProc;
begin
  if CssCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '<':
        if CssCheckNull(False) or PhpCheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
      '*':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '/' then
        begin
          Inc(FInstance^.FRun);
          SetRangeBit(12, False);
          Break;
        end;
      end;
    end;
  until False;
  FInstance^.FTokenID := stkCssComment;
end;

function TSynWebEngine.CssPropKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.CssPropCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  FInstance^.FTokenLastID := -1;
  if HashKey <= CssPropMaxKeyHash then
  begin
    Result := FCssPropIdentFuncTable[HashKey];
    if (FInstance^.FTokenLastID <> -1) and
      (TSynWeb_CssPropsData[FInstance^.FTokenLastID] and
      (1 shl Longword(FInstance^.FOptions.FCssVersion)) = 0) then
      Result := stkCssPropUndef;
  end else
    Result := stkCssPropUndef;
  CssSetProp(FInstance^.FTokenLastID + 1);
end;

{$I SynHighlighterWeb_CssPropsFunc.inc}

function TSynWebEngine.CssValKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.CssValCheck: TSynWebTokenKind;
var
  HashKey: Longword;
  prop: Integer;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  FInstance^.FTokenLastID := -1;
  if HashKey <= CssValMaxKeyHash then
  begin
    Result := FCssValIdentFuncTable[HashKey];
    if Result = stkCssVal then
    begin
      prop := CssGetProp - 1;
      if (prop = -1) or (TSynWeb_CssValsData[FInstance^.FTokenLastID]
        [Longword(FInstance^.FOptions.FCssVersion)][prop div 32] and (1 shl (prop mod 32)) = 0) then
        Result := stkCssValUndef;
    end;
  end else
    Result := stkCssValUndef;
  if Result = stkCssValUndef then
  begin
    prop := CssGetProp - 1;
    if (prop <> -1) and (TSynWeb_CssPropsData[prop] and (1 shl 20) <> 0) then
      Result := stkCssSymbol;
  end;
end;

{$I SynHighlighterWeb_CssValsFunc.inc}

function TSynWebEngine.CssSpecialKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.CssSpecialCheck(AStart, ALen: Integer): Integer;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  if (HashKey > CssSpecialMaxKeyHash) or not FCssSpecialIdentFuncTable[HashKey] then
    FInstance^.FTokenLastID := -1;
  Result := FInstance^.FTokenLastID;
end;

{$I SynHighlighterWeb_CssSpecialFunc.inc}

// ECMAScript ------------------------------------------------------------------

procedure TSynWebEngine.EsMakeMethodTables;
var
  c: char;
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        FEsProcTable[c] := NullProc;
      #1..#32:
        FEsProcTable[c] := EsSpaceProc;
      '/':
        FEsProcTable[c] := EsSlashProc;
      '<':
        FEsProcTable[c] := EsLowerProc;
      '=', '!':
        FEsProcTable[c] := EsEqualNotProc;
      '>':
        FEsProcTable[c] := EsGreaterProc;
      '&':
        FEsProcTable[c] := EsAndProc;
      '+':
        FEsProcTable[c] := EsPlusProc;
      '-':
        FEsProcTable[c] := EsMinusProc;
      '|':
        FEsProcTable[c] := EsOrProc;
      '*', '%', '^':
        FEsProcTable[c] := EsMulModXorProc;
      '0'..'9':
        FEsProcTable[c] := EsNumberProc;
      '"':
        FEsProcTable[c] := EsString34Proc;
      #39:
        FEsProcTable[c] := EsString39Proc;
      '{', '}', '[', ']', '(', ')', '.', ';', ',', '?', ':', '~':
        FEsProcTable[c] :=
          EsSymbolProc;
      '$', 'a'..'z', 'A'..'Z', '_':
        FEsProcTable[c] := EsIdentProc;
      else
        FEsProcTable[c] := EsErrorProc;
    end;

  FEsRangeProcTable[srsEsDefault] := EsRangeDefaultProc;
  FEsRangeProcTable[srsEsComment] := EsRangeCommentProc;
  FEsRangeProcTable[srsEsCommentMulti] := EsRangeCommentMultiProc;
  FEsRangeProcTable[srsEsString34] := EsRangeString34Proc;
  FEsRangeProcTable[srsEsString39] := EsRangeString39Proc;

  pF := PSynWebIdentFuncTableFunc(@FEsIdentFuncTable);
  for I := Low(FEsIdentFuncTable) to High(FEsIdentFuncTable) do
  begin
    pF^ := EsKeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_EsKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.EsNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FEsRangeProcTable[EsGetRange];
end;

function TSynWebEngine.EsGetRange: TSynWebEsRangeState;
begin
  Result := TSynWebEsRangeState(GetRangeInt(2, 15));
end;

procedure TSynWebEngine.EsSetRange(const ARange: TSynWebEsRangeState);
begin
  SetRangeInt(2, 15, Longword(ARange));
end;

function TSynWebEngine.EsCheckNull(ADo: Boolean = True): Boolean;
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
          SetHighlighterType(shtHtml, True, False, False);
          Next;
        end;
      end else
        Result := False;
    else
      Result := False;
  end;
end;

procedure TSynWebEngine.EsSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkEsSpace;
end;

procedure TSynWebEngine.EsSlashProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
    '*':
    begin
      Inc(FInstance^.FRun);
      EsSetRange(srsEsCommentMulti);
      if EsCheckNull(False) or PhpCheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        EsRangeCommentMultiProc;
      Exit;
    end;
    '=':
      Inc(FInstance^.FRun);
    '/':
    begin
      Inc(FInstance^.FRun);
      EsSetRange(srsEsComment);
      if EsCheckNull(False) or PhpCheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        EsRangeCommentProc;
      Exit;
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsLowerProc;
begin
  if EsCheckNull or PhpCheckBegin then
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

procedure TSynWebEngine.EsEqualNotProc;
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

procedure TSynWebEngine.EsGreaterProc;
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

procedure TSynWebEngine.EsAndProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '&'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsPlusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '+'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsMinusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '-'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsOrProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '|'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsMulModXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsNumberProc;
begin
  FInstance^.FTokenID := stkEsError;
  if (FInstance^.FLine[FInstance^.FRun] = '0') and
    (FInstance^.FLine[FInstance^.FRun + 1] in ['x', 'X']) then
  begin
    Inc(FInstance^.FRun, 2);
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
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
  FInstance^.FTokenID := stkEsNumber;
end;

procedure TSynWebEngine.EsString34Proc;
begin
  Inc(FInstance^.FRun);
  if EsCheckNull(False) then
    FInstance^.FTokenID := stkEsError
  else
  begin
    EsSetRange(srsEsString34);
    if PhpCheckBegin(False) then
      FInstance^.FTokenID := stkEsString
    else
      EsRangeString34Proc;
  end;
end;

procedure TSynWebEngine.EsString39Proc;
begin
  Inc(FInstance^.FRun);
  if EsCheckNull(False) then
    FInstance^.FTokenID := stkEsError
  else
  begin
    EsSetRange(srsEsString39);
    if PhpCheckBegin(False) then
      FInstance^.FTokenID := stkEsString
    else
      EsRangeString39Proc;
  end;
end;

procedure TSynWebEngine.EsSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsIdentProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 2) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$']);
  FInstance^.FTokenID := EsIdentCheck;
end;

procedure TSynWebEngine.EsErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsError;
end;

procedure TSynWebEngine.EsRangeDefaultProc;
begin
  FEsProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.EsRangeCommentProc;
begin
  if not EsCheckNull then
    if PhpCheckBegin then
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
            if PhpCheckBegin(False) then
            begin
              FInstance^.FTokenID := stkEsComment;
              Exit;
            end else
              if EsCheckNull(False) then
              begin
                FInstance^.FTokenID := stkEsComment;
                Break;
              end else
              begin
                Inc(FInstance^.FRun);
                if FInstance^.FLine[FInstance^.FRun] = #0 then
                begin
                  FInstance^.FTokenID := stkEsComment;
                  Break;
                end;
              end;
        end;
      until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.EsRangeCommentMultiProc;
begin
  if EsCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '<':
        if EsCheckNull(False) or PhpCheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
      '*':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '/' then
        begin
          Inc(FInstance^.FRun);
          EsSetRange(srsEsDefault);
          Break;
        end;
      end;
    end;
  until False;
  FInstance^.FTokenID := stkEsComment;
end;

procedure TSynWebEngine.EsRangeString34Proc;
begin
  if not HtmlCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #34, '<', '\']) do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 3) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
          '<':
            if PhpCheckBegin(False) then
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
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.EsRangeString39Proc;
begin
  if not HtmlCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<', '\']) do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 4) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
          #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
          '<':
            if PhpCheckBegin(False) then
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
  EsSetRange(srsEsDefault);
end;

function TSynWebEngine.EsKeywordComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.EsIdentCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  FInstance^.FTokenLastID := -1;
  if HashKey <= EsKeywordsMaxKeyHash then
    Result := FEsIdentFuncTable[HashKey]
  else
    Result := stkEsIdentifier;
end;

{$I SynHighlighterWeb_EsKeywordsFunc.inc}

// Php -------------------------------------------------------------------------

procedure TSynWebEngine.PhpMakeMethodTables;
var
  c: char;
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
      #0:
        FPhpProcTable[c] := NullProc;
      #1..#32:
        FPhpProcTable[c] := PhpSpaceProc;
      '?':
        FPhpProcTable[c] := PhpQuestionProc;
      '0'..'9':
        FPhpProcTable[c] := PhpNumberProc;
      '"':
        FPhpProcTable[c] := PhpString34Proc;
      #39:
        FPhpProcTable[c] := PhpString39Proc;
      '`':
        FPhpProcTable[c] := PhpStringShellProc;
      '&':
        FPhpProcTable[c] := PhpAndProc;
      '|':
        FPhpProcTable[c] := PhpOrProc;
      '@':
        FPhpProcTable[c] := PhpAtSymbolProc;
      '=':
        FPhpProcTable[c] := PhpEqualProc;
      '>':
        FPhpProcTable[c] := PhpGreaterProc;
      '<':
        FPhpProcTable[c] := PhpLowerProc;
      '+':
        FPhpProcTable[c] := PhpPlusProc;
      '-':
        FPhpProcTable[c] := PhpMinusProc;
      '*', '^':
        FPhpProcTable[c] := PhpMulDivModXorProc;
      '/':
        FPhpProcTable[c] := PhpSlashProc;
      '%':
        FPhpProcTable[c] := PhpPercentProc;
      '#':
        FPhpProcTable[c] := PhpHashProc;
      '!':
        FPhpProcTable[c] := PhpNotProc;
      '.':
        FPhpProcTable[c] := PhpDotProc;
      '{', '}', '[', ']', '(', ')', '~', ',', ';', ':':
        FPhpProcTable[c] :=
          PhpSymbolProc;
      '$':
        FPhpProcTable[c] := PhpVarProc;
      'a'..'z', 'A'..'Z', '_', #$7F..#$FF:
        FPhpProcTable[c] := PhpIdentProc;
      else
        FPhpProcTable[c] := PhpErrorProc;
    end;

  FPhpRangeProcTable[srsPhpSubProc] := PhpSubProcProc;
  FPhpRangeProcTable[srsPhpDefault] := PhpRangeDefaultProc;
  FPhpRangeProcTable[srsPhpComment] := PhpRangeCommentProc;
  FPhpRangeProcTable[srsPhpString34] := PhpRangeString34Proc;
  FPhpRangeProcTable[srsPhpString39] := PhpRangeString39Proc;
  FPhpRangeProcTable[srsPhpStringShell] := PhpRangeStringShellProc;
  FPhpRangeProcTable[srsPhpHeredoc] := PhpRangeHeredocProc;

  pF := PSynWebIdentFuncTableFunc(@FPhpIdentFuncTable);
  for I := Low(FPhpIdentFuncTable) to High(FPhpIdentFuncTable) do
  begin
    pF^ := PhpKeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_PhpKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.PhpNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  if FInstance^.FLine[FInstance^.FRun] = #0 then
    NullProc
  else
    FPhpRangeProcTable[PhpGetRange];
end;

procedure TSynWebEngine.PhpCliNext;
begin
  FInstance^.FTokenID := stkPhpInlineText;
  FInstance^.FTokenPos := FInstance^.FRun;
  if HtmlCheckNull or PhpCheckBegin then
    Exit;
  repeat
    while not (FInstance^.FLine[FInstance^.FRun] in [#0, '<']) do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '<':
        if PhpCheckBegin(False) then
          Break
        else
          Inc(FInstance^.FRun);
    end;
  until False;
end;

function TSynWebEngine.PhpGetRange: TSynWebPhpRangeState;
begin
  if GetRangeBit(26) then
    Result := srsPhpHeredoc
  else
    Result := TSynWebPhpRangeState(GetRangeInt(3, 23));
end;

procedure TSynWebEngine.PhpSetRange(const ARange: TSynWebPhpRangeState);
begin
  if ARange = srsPhpHeredoc then
    SetRangeBit(26, True)
  else
  begin
    SetRangeBit(26, False);
    SetRangeInt(3, 23, Longword(ARange));
  end;
end;

function TSynWebEngine.PhpCheckBegin(ABegin: Boolean): Boolean;
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
            PhpBegin(spotPhp);
        end else
          if FInstance^.FOptions.FPhpShortOpenTag then
          begin
            if ABegin then
              PhpBegin(spotPhpShort);
          end else
            Exit;
      '%':
        if FInstance^.FOptions.FPhpAspTags then
        begin
          if ABegin then
            PhpBegin(spotASP);
        end else
          Exit;
      else
        Exit;
    end else
    Exit;
  Result := True;
end;

procedure TSynWebEngine.PhpBegin(ATagKind: TSynWebPhpOpenTag);
begin
  SetHighlighterType(
    TSynWebHighlighterType(Longword(FInstance^.FHighlighterType) + Longword(shtPhpInHtml)),
    False,
    True,
    ATagKind = spotHtml);
  SetRangeInt(12, 17, 0);
  if ATagKind = spotHtml then
    PhpSetRange(srsPhpDefault)
  else
  begin
    if ATagKind = spotPhp then
      SetRangeBit(19, True);
    Next;
  end;
end;

procedure TSynWebEngine.PhpEnd(AHtmlTag: Boolean);
begin
  SetRangeInt(12, 17, 0);
  if FInstance^.FLine[FInstance^.FRun] = #0 then
    SetRangeInt(3, 29, Longword(FInstance^.FHighlighterType) - Longword(shtPhpInHtml))
  else
  begin
    SetHighlighterType(
      TSynWebHighlighterType(Longword(FInstance^.FHighlighterType) - Longword(shtPhpInHtml)),
      AHtmlTag,
      True, not AHtmlTag);
    if AHtmlTag then
      Next;
  end;
end;

procedure TSynWebEngine.PhpSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkPhpSpace;
end;

procedure TSynWebEngine.PhpQuestionProc;
begin
  Inc(FInstance^.FRun);
  if (FInstance^.FLine[FInstance^.FRun] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun);
    FInstance^.FTokenID := stkHtmlTag;
    PhpEnd(False);
  end else
    FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpNumberProc;
begin
  if PhpCheckNumberProc then
    FInstance^.FTokenID := stkPhpNumber
  else
    FInstance^.FTokenID := stkPhpError;
end;

function TSynWebEngine.PhpCheckNumberProc: Boolean;
begin
  Result := False;
  if (FInstance^.FLine[FInstance^.FRun] = '0') and
    (FInstance^.FLine[FInstance^.FRun + 1] = 'x') then
  begin
    Inc(FInstance^.FRun, 2);
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
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

procedure TSynWebEngine.PhpString34Proc;
begin
  Inc(FInstance^.FRun);
  PhpSetRange(srsPhpString34);
  // if FInstance^.FLine[FInstance^.FRun] in [#0, '\', '{', '$'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 30) <> 0 then
    FInstance^.FTokenID := stkPhpString
  else
    PhpRangeString34Proc;
end;

procedure TSynWebEngine.PhpString39Proc;
begin
  Inc(FInstance^.FRun);
  PhpSetRange(srsPhpString39);
  if FInstance^.FLine[FInstance^.FRun] in [#0, '\'] then
    FInstance^.FTokenID := stkPhpString
  else
    PhpRangeString39Proc;
end;

procedure TSynWebEngine.PhpStringShellProc;
begin
  Inc(FInstance^.FRun);
  PhpSetRange(srsPhpStringShell);
  if FInstance^.FLine[FInstance^.FRun] in [#0, '`'] then
    FInstance^.FTokenID := stkPhpString
  else
    PhpRangeStringShellProc;
end;

procedure TSynWebEngine.PhpAndProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '&'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpOrProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '|'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpAtSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpKeyword;
end;

procedure TSynWebEngine.PhpEqualProc;
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

procedure TSynWebEngine.PhpGreaterProc;
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

procedure TSynWebEngine.PhpLowerProc;
var
  tmpRun: Longword;
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
        PhpEnd(True);
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
          // if not (FInstance^.FLine[tmpRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF]) then
          if TSynWebIdentTable[FInstance^.FLine[tmpRun]] and (1 shl 28) = 0 then
          begin
            FInstance^.FTokenID := stkPhpError;
            Exit;
          end;
          PhpSetRange(srsPhpSubProc);
          SetRangeInt(3, 20, 2);
        end;
      end;
    end;
  end;
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpPlusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['+', '='] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpMinusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['-', '=', '>'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpMulDivModXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpSlashProc;
var
  b: Boolean;
begin
  case FInstance^.FLine[FInstance^.FRun + 1] of
    '/':
    begin
      Inc(FInstance^.FRun);
      PhpHashProc;
    end;
    '*':
    begin
      Inc(FInstance^.FRun, 2);
      PhpSetRange(srsPhpComment);
      b := (FInstance^.FLine[FInstance^.FRun] = '*') and
        (FInstance^.FLine[FInstance^.FRun + 1] <= #32);
      if b then
        Inc(FInstance^.FRun);
      SetRangeBit(19, b);
      if FInstance^.FLine[FInstance^.FRun] <> #0 then
        PhpRangeCommentProc
      else
        if b then
          FInstance^.FTokenID := stkPhpDocComment
        else
          FInstance^.FTokenID := stkPhpComment;
    end;
    else
      PhpMulDivModXorProc;
  end;
end;

procedure TSynWebEngine.PhpPercentProc;
begin
  if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun, 2);
    if FInstance^.FOptions.FPhpAspTags then
    begin
      FInstance^.FTokenID := stkHtmlTag;
      PhpEnd(False);
    end else
      FInstance^.FTokenID := stkPhpError;
  end else
    PhpMulDivModXorProc;
end;

procedure TSynWebEngine.PhpHashProc;
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

procedure TSynWebEngine.PhpNotProc;
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

procedure TSynWebEngine.PhpDotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    FInstance^.FTokenID := stkPhpSymbol;
  end else
    PhpNumberProc;
end;

procedure TSynWebEngine.PhpSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpSymbol;
end;

procedure TSynWebEngine.PhpVarProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '$' then
    Inc(FInstance^.FRun);
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
    FInstance^.FTokenID := stkPhpKeyword
  else
    FInstance^.FTokenID := stkPhpError;
end;

procedure TSynWebEngine.PhpIdentProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  if (FInstance^.FTokenPos > 0) and (FInstance^.FLine[FInstance^.FTokenPos - 1] = '$') then
    FInstance^.FTokenID := stkPhpVariable
  else
    FInstance^.FTokenID := PhpIdentCheck;
end;

procedure TSynWebEngine.PhpErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpError;
end;

function TSynWebEngine.PhpDoStringDouble(AIsHeredoc: Boolean;
  ARangeChar: Boolean): Boolean;
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

  function TryDoIdent: Boolean;
  begin
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
    begin
      DoIdent;
      Result := True;
    end else
      Result := False;
  end;

  function DoStringSingle: Boolean;
  begin
    Result := True;
    repeat
      if FInstance^.FLine[FInstance^.FRun] = '\' then
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] in [#39, '\'] then
          Inc(FInstance^.FRun);
      end;
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #39, '\'] do
      while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 24) = 0 do
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

  function DoStringObject(AAllowSpaces: Boolean = True): Boolean;
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

  function DoStringVar: Boolean;
  begin
    Inc(FInstance^.FRun);
    Result := TryDoIdent;
  end;

  function DoStringVar2: Boolean;
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
            if not PhpCheckNumberProc then
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
            while not PhpDoStringDouble(False, False) and
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
  if not ARangeChar or (PhpGetRange = srsPhpString34) then
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
                if not PhpCheckNumberProc then
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
      // if FInstance^.FLine[FInstance^.FRun] in ['n', 'r', 't', '\', '$', #34, '0'..'7', 'x'] then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 18) <> 0 then
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
            // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
            if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
            begin
              Inc(FInstance^.FRun);
              // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
              if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
                (1 shl 10) <> 0 then
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
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #34, '\', '{', '$'] do
      while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 25) = 0 do
        Inc(FInstance^.FRun)
    else
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, '`', '\', '{', '$'] do
      while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 5) = 0 do
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

procedure TSynWebEngine.PhpSubProcProc;
var
  s: String;
  i: Integer;

  procedure DoDefault;
  begin
    SetRangeInt(3, 20, 0);
    PhpSetRange(srsPhpDefault);
  end;

begin
  case GetRangeInt(3, 20) of
    0:
    begin
      Inc(FInstance^.FRun, 2);
      FInstance^.FTokenID := stkHtmlTag;
      SetRangeInt(3, 20, 1);
    end;
    1:
    begin
      DoDefault;
      if GetRangeBit(19) then
      begin
        SetRangeBit(19, False);
        Inc(FInstance^.FRun, 3);
        FInstance^.FTokenID := stkPhpKeyword;
      end else
        if (FInstance^.FLine[FInstance^.FRun] = '=') and (FInstance^.FOptions.FPhpShortOpenTag) then
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkPhpKeyword;
        end else
          PhpRangeDefaultProc;
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
        PhpSetRange(srsPhpDefault);
        Exit;
      end;
      FInstance^.FTokenID := stkPhpKeyword;
      PhpSetRange(srsPhpHeredoc);
      s := GetToken;
      i := FPhpHereDocList.IndexOf(s);
      if i in [0..255] then
      begin
        SetRangeInt(8, 17, i);
        SetRangeBit(25, True);
      end else
      begin
        SetRangeInt(8, 17, GetCrc8String(s));
        SetRangeBit(25, False);
      end;
    end;
  end;
end;

procedure TSynWebEngine.PhpRangeDefaultProc;
begin
  FPhpProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.PhpRangeCommentProc;
begin
  repeat
    if (FInstance^.FLine[FInstance^.FRun] = '*') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
    begin
      Inc(FInstance^.FRun, 2);
      PhpSetRange(srsPhpDefault);
      Break;
    end;
    Inc(FInstance^.FRun);
  until FInstance^.FLine[FInstance^.FRun] = #0;
  if GetRangeBit(19) then
    FInstance^.FTokenID := stkPhpDocComment
  else
    FInstance^.FTokenID := stkPhpComment;
end;

procedure TSynWebEngine.PhpRangeString34Proc;
begin
  if PhpDoStringDouble then
  begin
    Inc(FInstance^.FRun);
    PhpSetRange(srsPhpDefault);
  end;
end;

procedure TSynWebEngine.PhpRangeString39Proc;
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
    // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #39, '\'] do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 24) = 0 do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] <> #39 then
      Exit
    else
    begin
      Inc(FInstance^.FRun);
      PhpSetRange(srsPhpDefault);
      Exit;
    end;
  until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.PhpRangeStringShellProc;
begin
  if PhpDoStringDouble then
  begin
    Inc(FInstance^.FRun);
    PhpSetRange(srsPhpDefault);
  end;
end;

procedure TSynWebEngine.PhpRangeHeredocProc;
var
  OldRun: longint;
  s: String;
begin
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
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
      if (GetRangeBit(25) and (s = FPhpHereDocList[GetRangeInt(8, 17)])) or
        (not GetRangeBit(25) and (GetRangeInt(8, 17) = GetCrc8String(GetToken))) then
      begin
        FInstance^.FTokenID := stkPhpKeyword;
        PhpSetRange(srsPhpDefault);
        Exit;
      end;
    end;
    FInstance^.FRun := OldRun;
  end;
  if PhpDoStringDouble(True) then
  begin
    Inc(FInstance^.FRun);
    PhpSetRange(srsPhpDefault);
  end;
end;

function TSynWebEngine.PhpKeywordComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
  Data: Longword;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.PhpConstComp: Boolean;
var
  I: Integer;
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

function TSynWebEngine.PhpFunctionComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
  Data: Longword;
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
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.PhpIdentCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
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
  FInstance^.FTokenLastID := -1;
  if HashKey <= PhpKeywordsMaxKeyHash then
    Result := FPhpIdentFuncTable[HashKey]
  else
    Result := stkPhpIdentifier;
  if Result = stkPhpIdentifier then
    if PhpConstComp then
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
  i: Integer;
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
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
    TSynWebBase(FNotifyList[i]).DoDefHighlightChange;
end;

function TSynWebEngine.GetCrc8String(AString: String): byte;
var
  i: Integer;
begin
  Result := Length(AString);
  for i := 1 to Length(AString) do
    Result := TCrc8Table[Result xor Byte(AString[i])];
end;

function TSynWebEngine.GetRangeBit(ABit: Longword): Boolean;
begin
  Result := FInstance^.FRange and (1 shl ABit) <> 0;
end;

procedure TSynWebEngine.SetRangeBit(ABit: Longword; AVal: Boolean);
begin
  if AVal then
    FInstance^.FRange := FInstance^.FRange or (1 shl ABit)
  else
    FInstance^.FRange := FInstance^.FRange and not (1 shl ABit);
end;

function TSynWebEngine.GetRangeInt(ALen, APos: Longword): Longword;
begin
  Result := (FInstance^.FRange shr APos) and not ($FFFFFFFF shl ALen);
end;

procedure TSynWebEngine.SetRangeInt(ALen, APos, AVal: Longword);
var
  i: Longword;
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
var
  OldRun: Integer;
begin
  SetHighlighterType(FInstance^.FNextHighlighterType, FInstance^.FNextClearBits,
    False, FInstance^.FNextUseNextAH);
  OldRun := FInstance^.FRun;
  Next;
  if OldRun = FInstance^.FRun then
    Next;
  FInstance^.FHighlighterSW := True;

end;

procedure TSynWebEngine.SetHighlighterType(const AHighlighterType: TSynWebHighlighterType;
  AClearBits: Boolean; ASetAtNextToken: Boolean; AUseNextAH: Boolean);
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
    SetRangeInt(3, 29, Longword(AHighlighterType));
    SetupHighlighterType(AClearBits);
  end;
end;

procedure TSynWebEngine.SetupHighlighterType(AClearBits: Boolean);
begin
  case FInstance^.FHighlighterType of
    shtHtml:
      if FInstance^.FHighlighterMode = shmPhpCli then
      begin
        if AClearBits then
          SetRangeInt(17, 0, 0);
        FInstance^.FSYN_ATTR_COMMENT := FPhpInlineTextAttri;
        FInstance^.FSYN_ATTR_STRING := FPhpInlineTextAttri;
        FInstance^.FSYN_ATTR_WHITESPACE := FPhpInlineTextAttri;
        FInstance^.FNextProcTable := PhpCliNext;
      end else
      begin
        if AClearBits then
          SetRangeInt(17, 0, 0);
        FInstance^.FSYN_ATTR_COMMENT := FHtmlCommentAttri;
        FInstance^.FSYN_ATTR_STRING := FHtmlTagKeyValueQuotedAttri;
        FInstance^.FSYN_ATTR_WHITESPACE := FHtmlWhitespaceAttri;
        FInstance^.FNextProcTable := HtmlNext;
      end;
    shtCss:
    begin
      if AClearBits then
        SetRangeInt(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := FCssCommentAttri;
      FInstance^.FSYN_ATTR_STRING := FCssValStringAttri;
      CssUpdateBg;
      FInstance^.FNextProcTable := CssNext;
    end;
    shtEs:
    begin
      if AClearBits then
        SetRangeInt(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := FEsCommentAttri;
      FInstance^.FSYN_ATTR_STRING := FEsStringAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := FEsWhitespaceAttri;
      FInstance^.FNextProcTable := EsNext;
    end;
    else
      if AClearBits then
        SetRangeInt(12, 17, 0);
      FInstance^.FSYN_ATTR_COMMENT := FPhpCommentAttri;
      FInstance^.FSYN_ATTR_STRING := FPhpStringAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := FPhpWhitespaceAttri;
      FInstance^.FNextProcTable := PhpNext;
  end;
end;

procedure TSynWebEngine.SetLine(NewValue: String; LineNumber: Integer);
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
  FInstance^.FHighlighterType := TSynWebHighlighterType(GetRangeInt(3, 29));
  FInstance^.FPrevHighlighterType := FInstance^.FHighlighterType;
  FInstance^.FHighlighterSW := False;
  SetupHighlighterType;
{$IFNDEF UNISYNEDIT}
  FInstance^.FNextProcTable;
{$ENDIF}
end;

procedure TSynWebEngine.Next;
begin
  FInstance^.FHighlighterSW := False;
  FInstance^.FNextProcTable;
end;

function TSynWebEngine.GetToken: String;
var
  Len: longint;
begin
  Len := FInstance^.FRun - FInstance^.FTokenPos;
  SetString(Result, (FInstance^.FLine + FInstance^.FTokenPos), Len);
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynWebHtmlSyn);
  RegisterPlaceableHighlighter(TSynWebPhpCliSyn);
  RegisterPlaceableHighlighter(TSynWebCssSyn);
  RegisterPlaceableHighlighter(TSynWebEsSyn);
{$ENDIF}

end.

