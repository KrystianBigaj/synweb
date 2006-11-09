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
    function HtmlTagUndef: TSynWebTokenKind;
    function HtmlTagFunc1: TSynWebTokenKind;
    function HtmlTagFunc2: TSynWebTokenKind;
    function HtmlTagFunc8: TSynWebTokenKind;
    function HtmlTagFunc9: TSynWebTokenKind;
    function HtmlTagFunc16: TSynWebTokenKind;
    function HtmlTagFunc17: TSynWebTokenKind;
    function HtmlTagFunc18: TSynWebTokenKind;
    function HtmlTagFunc19: TSynWebTokenKind;
    function HtmlTagFunc20: TSynWebTokenKind;
    function HtmlTagFunc21: TSynWebTokenKind;
    function HtmlTagFunc23: TSynWebTokenKind;
    function HtmlTagFunc24: TSynWebTokenKind;
    function HtmlTagFunc25: TSynWebTokenKind;
    function HtmlTagFunc26: TSynWebTokenKind;
    function HtmlTagFunc27: TSynWebTokenKind;
    function HtmlTagFunc28: TSynWebTokenKind;
    function HtmlTagFunc29: TSynWebTokenKind;
    function HtmlTagFunc30: TSynWebTokenKind;
    function HtmlTagFunc31: TSynWebTokenKind;
    function HtmlTagFunc32: TSynWebTokenKind;
    function HtmlTagFunc33: TSynWebTokenKind;
    function HtmlTagFunc35: TSynWebTokenKind;
    function HtmlTagFunc37: TSynWebTokenKind;
    function HtmlTagFunc38: TSynWebTokenKind;
    function HtmlTagFunc39: TSynWebTokenKind;
    function HtmlTagFunc40: TSynWebTokenKind;
    function HtmlTagFunc41: TSynWebTokenKind;
    function HtmlTagFunc42: TSynWebTokenKind;
    function HtmlTagFunc43: TSynWebTokenKind;
    function HtmlTagFunc46: TSynWebTokenKind;
    function HtmlTagFunc47: TSynWebTokenKind;
    function HtmlTagFunc49: TSynWebTokenKind;
    function HtmlTagFunc50: TSynWebTokenKind;
    function HtmlTagFunc52: TSynWebTokenKind;
    function HtmlTagFunc53: TSynWebTokenKind;
    function HtmlTagFunc55: TSynWebTokenKind;
    function HtmlTagFunc56: TSynWebTokenKind;
    function HtmlTagFunc57: TSynWebTokenKind;
    function HtmlTagFunc64: TSynWebTokenKind;
    function HtmlTagFunc65: TSynWebTokenKind;
    function HtmlTagFunc66: TSynWebTokenKind;
    function HtmlTagFunc70: TSynWebTokenKind;
    function HtmlTagFunc76: TSynWebTokenKind;
    function HtmlTagFunc78: TSynWebTokenKind;
    function HtmlTagFunc80: TSynWebTokenKind;
    function HtmlTagFunc81: TSynWebTokenKind;
    function HtmlTagFunc82: TSynWebTokenKind;
    function HtmlTagFunc84: TSynWebTokenKind;
    function HtmlTagFunc85: TSynWebTokenKind;
    function HtmlTagFunc87: TSynWebTokenKind;
    function HtmlTagFunc89: TSynWebTokenKind;
    function HtmlTagFunc91: TSynWebTokenKind;
    function HtmlTagFunc92: TSynWebTokenKind;
    function HtmlTagFunc93: TSynWebTokenKind;
    function HtmlTagFunc94: TSynWebTokenKind;
    function HtmlTagFunc107: TSynWebTokenKind;
    function HtmlTagFunc114: TSynWebTokenKind;
    function HtmlTagFunc121: TSynWebTokenKind;
    function HtmlTagFunc128: TSynWebTokenKind;

    function HtmlAttrKeyComp(const ID: Integer): Boolean;
    function HtmlAttrCheck: TSynWebTokenKind;
    function HtmlAttrUndef: TSynWebTokenKind;
    function HtmlAttrFunc13: TSynWebTokenKind;
    function HtmlAttrFunc15: TSynWebTokenKind;
    function HtmlAttrFunc23: TSynWebTokenKind;
    function HtmlAttrFunc26: TSynWebTokenKind;
    function HtmlAttrFunc27: TSynWebTokenKind;
    function HtmlAttrFunc30: TSynWebTokenKind;
    function HtmlAttrFunc31: TSynWebTokenKind;
    function HtmlAttrFunc32: TSynWebTokenKind;
    function HtmlAttrFunc33: TSynWebTokenKind;
    function HtmlAttrFunc34: TSynWebTokenKind;
    function HtmlAttrFunc35: TSynWebTokenKind;
    function HtmlAttrFunc37: TSynWebTokenKind;
    function HtmlAttrFunc38: TSynWebTokenKind;
    function HtmlAttrFunc39: TSynWebTokenKind;
    function HtmlAttrFunc40: TSynWebTokenKind;
    function HtmlAttrFunc43: TSynWebTokenKind;
    function HtmlAttrFunc45: TSynWebTokenKind;
    function HtmlAttrFunc46: TSynWebTokenKind;
    function HtmlAttrFunc47: TSynWebTokenKind;
    function HtmlAttrFunc48: TSynWebTokenKind;
    function HtmlAttrFunc49: TSynWebTokenKind;
    function HtmlAttrFunc50: TSynWebTokenKind;
    function HtmlAttrFunc52: TSynWebTokenKind;
    function HtmlAttrFunc53: TSynWebTokenKind;
    function HtmlAttrFunc54: TSynWebTokenKind;
    function HtmlAttrFunc55: TSynWebTokenKind;
    function HtmlAttrFunc56: TSynWebTokenKind;
    function HtmlAttrFunc57: TSynWebTokenKind;
    function HtmlAttrFunc58: TSynWebTokenKind;
    function HtmlAttrFunc59: TSynWebTokenKind;
    function HtmlAttrFunc60: TSynWebTokenKind;
    function HtmlAttrFunc61: TSynWebTokenKind;
    function HtmlAttrFunc62: TSynWebTokenKind;
    function HtmlAttrFunc63: TSynWebTokenKind;
    function HtmlAttrFunc64: TSynWebTokenKind;
    function HtmlAttrFunc65: TSynWebTokenKind;
    function HtmlAttrFunc66: TSynWebTokenKind;
    function HtmlAttrFunc67: TSynWebTokenKind;
    function HtmlAttrFunc68: TSynWebTokenKind;
    function HtmlAttrFunc69: TSynWebTokenKind;
    function HtmlAttrFunc71: TSynWebTokenKind;
    function HtmlAttrFunc72: TSynWebTokenKind;
    function HtmlAttrFunc73: TSynWebTokenKind;
    function HtmlAttrFunc74: TSynWebTokenKind;
    function HtmlAttrFunc75: TSynWebTokenKind;
    function HtmlAttrFunc77: TSynWebTokenKind;
    function HtmlAttrFunc78: TSynWebTokenKind;
    function HtmlAttrFunc79: TSynWebTokenKind;
    function HtmlAttrFunc80: TSynWebTokenKind;
    function HtmlAttrFunc81: TSynWebTokenKind;
    function HtmlAttrFunc82: TSynWebTokenKind;
    function HtmlAttrFunc85: TSynWebTokenKind;
    function HtmlAttrFunc87: TSynWebTokenKind;
    function HtmlAttrFunc88: TSynWebTokenKind;
    function HtmlAttrFunc91: TSynWebTokenKind;
    function HtmlAttrFunc93: TSynWebTokenKind;
    function HtmlAttrFunc94: TSynWebTokenKind;
    function HtmlAttrFunc96: TSynWebTokenKind;
    function HtmlAttrFunc98: TSynWebTokenKind;
    function HtmlAttrFunc101: TSynWebTokenKind;
    function HtmlAttrFunc102: TSynWebTokenKind;
    function HtmlAttrFunc103: TSynWebTokenKind;
    function HtmlAttrFunc104: TSynWebTokenKind;
    function HtmlAttrFunc105: TSynWebTokenKind;
    function HtmlAttrFunc106: TSynWebTokenKind;
    function HtmlAttrFunc107: TSynWebTokenKind;
    function HtmlAttrFunc108: TSynWebTokenKind;
    function HtmlAttrFunc109: TSynWebTokenKind;
    function HtmlAttrFunc110: TSynWebTokenKind;
    function HtmlAttrFunc111: TSynWebTokenKind;
    function HtmlAttrFunc113: TSynWebTokenKind;
    function HtmlAttrFunc114: TSynWebTokenKind;
    function HtmlAttrFunc119: TSynWebTokenKind;
    function HtmlAttrFunc126: TSynWebTokenKind;
    function HtmlAttrFunc127: TSynWebTokenKind;
    function HtmlAttrFunc139: TSynWebTokenKind;
    function HtmlAttrFunc147: TSynWebTokenKind;
    function HtmlAttrFunc157: TSynWebTokenKind;
    function HtmlAttrFunc158: TSynWebTokenKind;
    function HtmlAttrFunc162: TSynWebTokenKind;

    function HtmlSpecialKeyComp(const ID: Integer): Boolean;
    function HtmlSpecialCheck(AStart, ALen: Integer): Integer;
    function HtmlSpecialUndef: Boolean;
    function HtmlSpecialFunc12: Boolean;
    function HtmlSpecialFunc16: Boolean;
    function HtmlSpecialFunc17: Boolean;
    function HtmlSpecialFunc19: Boolean;
    function HtmlSpecialFunc20: Boolean;
    function HtmlSpecialFunc22: Boolean;
    function HtmlSpecialFunc23: Boolean;
    function HtmlSpecialFunc25: Boolean;
    function HtmlSpecialFunc26: Boolean;
    function HtmlSpecialFunc27: Boolean;
    function HtmlSpecialFunc28: Boolean;
    function HtmlSpecialFunc30: Boolean;
    function HtmlSpecialFunc32: Boolean;
    function HtmlSpecialFunc33: Boolean;
    function HtmlSpecialFunc34: Boolean;
    function HtmlSpecialFunc35: Boolean;
    function HtmlSpecialFunc36: Boolean;
    function HtmlSpecialFunc38: Boolean;
    function HtmlSpecialFunc39: Boolean;
    function HtmlSpecialFunc40: Boolean;
    function HtmlSpecialFunc41: Boolean;
    function HtmlSpecialFunc42: Boolean;
    function HtmlSpecialFunc43: Boolean;
    function HtmlSpecialFunc44: Boolean;
    function HtmlSpecialFunc45: Boolean;
    function HtmlSpecialFunc46: Boolean;
    function HtmlSpecialFunc47: Boolean;
    function HtmlSpecialFunc48: Boolean;
    function HtmlSpecialFunc49: Boolean;
    function HtmlSpecialFunc50: Boolean;
    function HtmlSpecialFunc51: Boolean;
    function HtmlSpecialFunc52: Boolean;
    function HtmlSpecialFunc53: Boolean;
    function HtmlSpecialFunc54: Boolean;
    function HtmlSpecialFunc55: Boolean;
    function HtmlSpecialFunc56: Boolean;
    function HtmlSpecialFunc57: Boolean;
    function HtmlSpecialFunc58: Boolean;
    function HtmlSpecialFunc59: Boolean;
    function HtmlSpecialFunc61: Boolean;
    function HtmlSpecialFunc62: Boolean;
    function HtmlSpecialFunc63: Boolean;
    function HtmlSpecialFunc64: Boolean;
    function HtmlSpecialFunc65: Boolean;
    function HtmlSpecialFunc66: Boolean;
    function HtmlSpecialFunc67: Boolean;
    function HtmlSpecialFunc68: Boolean;
    function HtmlSpecialFunc69: Boolean;
    function HtmlSpecialFunc70: Boolean;
    function HtmlSpecialFunc71: Boolean;
    function HtmlSpecialFunc72: Boolean;
    function HtmlSpecialFunc73: Boolean;
    function HtmlSpecialFunc74: Boolean;
    function HtmlSpecialFunc75: Boolean;
    function HtmlSpecialFunc76: Boolean;
    function HtmlSpecialFunc77: Boolean;
    function HtmlSpecialFunc78: Boolean;
    function HtmlSpecialFunc79: Boolean;
    function HtmlSpecialFunc81: Boolean;
    function HtmlSpecialFunc83: Boolean;
    function HtmlSpecialFunc84: Boolean;
    function HtmlSpecialFunc85: Boolean;
    function HtmlSpecialFunc86: Boolean;
    function HtmlSpecialFunc87: Boolean;
    function HtmlSpecialFunc90: Boolean;
    function HtmlSpecialFunc91: Boolean;
    function HtmlSpecialFunc95: Boolean;
    function HtmlSpecialFunc106: Boolean;
    function HtmlSpecialFunc111: Boolean;

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
    function CssPropUndef: TSynWebTokenKind;
    function CssPropFunc29: TSynWebTokenKind;
    function CssPropFunc39: TSynWebTokenKind;
    function CssPropFunc40: TSynWebTokenKind;
    function CssPropFunc43: TSynWebTokenKind;
    function CssPropFunc51: TSynWebTokenKind;
    function CssPropFunc52: TSynWebTokenKind;
    function CssPropFunc54: TSynWebTokenKind;
    function CssPropFunc55: TSynWebTokenKind;
    function CssPropFunc56: TSynWebTokenKind;
    function CssPropFunc57: TSynWebTokenKind;
    function CssPropFunc60: TSynWebTokenKind;
    function CssPropFunc61: TSynWebTokenKind;
    function CssPropFunc62: TSynWebTokenKind;
    function CssPropFunc63: TSynWebTokenKind;
    function CssPropFunc64: TSynWebTokenKind;
    function CssPropFunc74: TSynWebTokenKind;
    function CssPropFunc76: TSynWebTokenKind;
    function CssPropFunc78: TSynWebTokenKind;
    function CssPropFunc79: TSynWebTokenKind;
    function CssPropFunc81: TSynWebTokenKind;
    function CssPropFunc82: TSynWebTokenKind;
    function CssPropFunc83: TSynWebTokenKind;
    function CssPropFunc85: TSynWebTokenKind;
    function CssPropFunc86: TSynWebTokenKind;
    function CssPropFunc87: TSynWebTokenKind;
    function CssPropFunc88: TSynWebTokenKind;
    function CssPropFunc90: TSynWebTokenKind;
    function CssPropFunc91: TSynWebTokenKind;
    function CssPropFunc93: TSynWebTokenKind;
    function CssPropFunc94: TSynWebTokenKind;
    function CssPropFunc95: TSynWebTokenKind;
    function CssPropFunc96: TSynWebTokenKind;
    function CssPropFunc97: TSynWebTokenKind;
    function CssPropFunc98: TSynWebTokenKind;
    function CssPropFunc100: TSynWebTokenKind;
    function CssPropFunc101: TSynWebTokenKind;
    function CssPropFunc102: TSynWebTokenKind;
    function CssPropFunc103: TSynWebTokenKind;
    function CssPropFunc105: TSynWebTokenKind;
    function CssPropFunc106: TSynWebTokenKind;
    function CssPropFunc107: TSynWebTokenKind;
    function CssPropFunc108: TSynWebTokenKind;
    function CssPropFunc110: TSynWebTokenKind;
    function CssPropFunc111: TSynWebTokenKind;
    function CssPropFunc112: TSynWebTokenKind;
    function CssPropFunc114: TSynWebTokenKind;
    function CssPropFunc115: TSynWebTokenKind;
    function CssPropFunc116: TSynWebTokenKind;
    function CssPropFunc117: TSynWebTokenKind;
    function CssPropFunc121: TSynWebTokenKind;
    function CssPropFunc122: TSynWebTokenKind;
    function CssPropFunc124: TSynWebTokenKind;
    function CssPropFunc126: TSynWebTokenKind;
    function CssPropFunc128: TSynWebTokenKind;
    function CssPropFunc130: TSynWebTokenKind;
    function CssPropFunc131: TSynWebTokenKind;
    function CssPropFunc136: TSynWebTokenKind;
    function CssPropFunc138: TSynWebTokenKind;
    function CssPropFunc139: TSynWebTokenKind;
    function CssPropFunc140: TSynWebTokenKind;
    function CssPropFunc141: TSynWebTokenKind;
    function CssPropFunc142: TSynWebTokenKind;
    function CssPropFunc144: TSynWebTokenKind;
    function CssPropFunc148: TSynWebTokenKind;
    function CssPropFunc149: TSynWebTokenKind;
    function CssPropFunc150: TSynWebTokenKind;
    function CssPropFunc154: TSynWebTokenKind;
    function CssPropFunc156: TSynWebTokenKind;
    function CssPropFunc158: TSynWebTokenKind;
    function CssPropFunc167: TSynWebTokenKind;
    function CssPropFunc169: TSynWebTokenKind;
    function CssPropFunc172: TSynWebTokenKind;
    function CssPropFunc173: TSynWebTokenKind;
    function CssPropFunc174: TSynWebTokenKind;
    function CssPropFunc178: TSynWebTokenKind;
    function CssPropFunc182: TSynWebTokenKind;
    function CssPropFunc187: TSynWebTokenKind;
    function CssPropFunc190: TSynWebTokenKind;
    function CssPropFunc194: TSynWebTokenKind;
    function CssPropFunc220: TSynWebTokenKind;

    function CssValKeyComp(const ID: Integer): Boolean;
    function CssValCheck: TSynWebTokenKind;
    function CssValUndef: TSynWebTokenKind;
    function CssValFunc26: TSynWebTokenKind;
    function CssValFunc27: TSynWebTokenKind;
    function CssValFunc29: TSynWebTokenKind;
    function CssValFunc31: TSynWebTokenKind;
    function CssValFunc32: TSynWebTokenKind;
    function CssValFunc33: TSynWebTokenKind;
    function CssValFunc35: TSynWebTokenKind;
    function CssValFunc36: TSynWebTokenKind;
    function CssValFunc37: TSynWebTokenKind;
    function CssValFunc38: TSynWebTokenKind;
    function CssValFunc39: TSynWebTokenKind;
    function CssValFunc40: TSynWebTokenKind;
    function CssValFunc41: TSynWebTokenKind;
    function CssValFunc42: TSynWebTokenKind;
    function CssValFunc43: TSynWebTokenKind;
    function CssValFunc44: TSynWebTokenKind;
    function CssValFunc45: TSynWebTokenKind;
    function CssValFunc46: TSynWebTokenKind;
    function CssValFunc47: TSynWebTokenKind;
    function CssValFunc48: TSynWebTokenKind;
    function CssValFunc49: TSynWebTokenKind;
    function CssValFunc50: TSynWebTokenKind;
    function CssValFunc51: TSynWebTokenKind;
    function CssValFunc52: TSynWebTokenKind;
    function CssValFunc53: TSynWebTokenKind;
    function CssValFunc54: TSynWebTokenKind;
    function CssValFunc55: TSynWebTokenKind;
    function CssValFunc56: TSynWebTokenKind;
    function CssValFunc57: TSynWebTokenKind;
    function CssValFunc59: TSynWebTokenKind;
    function CssValFunc60: TSynWebTokenKind;
    function CssValFunc61: TSynWebTokenKind;
    function CssValFunc62: TSynWebTokenKind;
    function CssValFunc63: TSynWebTokenKind;
    function CssValFunc65: TSynWebTokenKind;
    function CssValFunc67: TSynWebTokenKind;
    function CssValFunc68: TSynWebTokenKind;
    function CssValFunc69: TSynWebTokenKind;
    function CssValFunc70: TSynWebTokenKind;
    function CssValFunc71: TSynWebTokenKind;
    function CssValFunc72: TSynWebTokenKind;
    function CssValFunc73: TSynWebTokenKind;
    function CssValFunc74: TSynWebTokenKind;
    function CssValFunc75: TSynWebTokenKind;
    function CssValFunc76: TSynWebTokenKind;
    function CssValFunc77: TSynWebTokenKind;
    function CssValFunc78: TSynWebTokenKind;
    function CssValFunc79: TSynWebTokenKind;
    function CssValFunc80: TSynWebTokenKind;
    function CssValFunc81: TSynWebTokenKind;
    function CssValFunc82: TSynWebTokenKind;
    function CssValFunc83: TSynWebTokenKind;
    function CssValFunc84: TSynWebTokenKind;
    function CssValFunc85: TSynWebTokenKind;
    function CssValFunc86: TSynWebTokenKind;
    function CssValFunc87: TSynWebTokenKind;
    function CssValFunc88: TSynWebTokenKind;
    function CssValFunc89: TSynWebTokenKind;
    function CssValFunc91: TSynWebTokenKind;
    function CssValFunc92: TSynWebTokenKind;
    function CssValFunc93: TSynWebTokenKind;
    function CssValFunc95: TSynWebTokenKind;
    function CssValFunc96: TSynWebTokenKind;
    function CssValFunc97: TSynWebTokenKind;
    function CssValFunc99: TSynWebTokenKind;
    function CssValFunc100: TSynWebTokenKind;
    function CssValFunc101: TSynWebTokenKind;
    function CssValFunc102: TSynWebTokenKind;
    function CssValFunc104: TSynWebTokenKind;
    function CssValFunc105: TSynWebTokenKind;
    function CssValFunc108: TSynWebTokenKind;
    function CssValFunc109: TSynWebTokenKind;
    function CssValFunc110: TSynWebTokenKind;
    function CssValFunc113: TSynWebTokenKind;
    function CssValFunc115: TSynWebTokenKind;
    function CssValFunc116: TSynWebTokenKind;
    function CssValFunc117: TSynWebTokenKind;
    function CssValFunc118: TSynWebTokenKind;
    function CssValFunc119: TSynWebTokenKind;
    function CssValFunc120: TSynWebTokenKind;
    function CssValFunc123: TSynWebTokenKind;
    function CssValFunc125: TSynWebTokenKind;
    function CssValFunc127: TSynWebTokenKind;
    function CssValFunc135: TSynWebTokenKind;
    function CssValFunc146: TSynWebTokenKind;
    function CssValFunc151: TSynWebTokenKind;
    function CssValFunc157: TSynWebTokenKind;
    function CssValFunc158: TSynWebTokenKind;

    function CssSpecialKeyComp(const ID: Integer): Boolean;
    function CssSpecialCheck(AStart, ALen: Integer): Integer;
    function CssSpecialUndef: Boolean;
    function CssSpecialFunc16: Boolean;
    function CssSpecialFunc18: Boolean;
    function CssSpecialFunc19: Boolean;
    function CssSpecialFunc23: Boolean;
    function CssSpecialFunc25: Boolean;
    function CssSpecialFunc26: Boolean;
    function CssSpecialFunc29: Boolean;
    function CssSpecialFunc30: Boolean;
    function CssSpecialFunc32: Boolean;
    function CssSpecialFunc34: Boolean;
    function CssSpecialFunc36: Boolean;
    function CssSpecialFunc40: Boolean;
    function CssSpecialFunc42: Boolean;
    function CssSpecialFunc43: Boolean;
    function CssSpecialFunc45: Boolean;
    function CssSpecialFunc46: Boolean;
    function CssSpecialFunc50: Boolean;
    function CssSpecialFunc51: Boolean;
    function CssSpecialFunc56: Boolean;
    function CssSpecialFunc57: Boolean;
    function CssSpecialFunc59: Boolean;
    function CssSpecialFunc60: Boolean;
    function CssSpecialFunc62: Boolean;
    function CssSpecialFunc64: Boolean;
    function CssSpecialFunc65: Boolean;
    function CssSpecialFunc68: Boolean;
    function CssSpecialFunc72: Boolean;
    function CssSpecialFunc74: Boolean;
    function CssSpecialFunc77: Boolean;
    function CssSpecialFunc82: Boolean;
    function CssSpecialFunc88: Boolean;
    function CssSpecialFunc89: Boolean;
    function CssSpecialFunc91: Boolean;
    function CssSpecialFunc93: Boolean;
    function CssSpecialFunc125: Boolean;
    function CssSpecialFunc126: Boolean;
    function CssSpecialFunc133: Boolean;

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
    function EsKeywordIdent: TSynWebTokenKind;
    function EsKeywordFunc15: TSynWebTokenKind;
    function EsKeywordFunc19: TSynWebTokenKind;
    function EsKeywordFunc23: TSynWebTokenKind;
    function EsKeywordFunc28: TSynWebTokenKind;
    function EsKeywordFunc30: TSynWebTokenKind;
    function EsKeywordFunc35: TSynWebTokenKind;
    function EsKeywordFunc37: TSynWebTokenKind;
    function EsKeywordFunc39: TSynWebTokenKind;
    function EsKeywordFunc41: TSynWebTokenKind;
    function EsKeywordFunc42: TSynWebTokenKind;
    function EsKeywordFunc43: TSynWebTokenKind;
    function EsKeywordFunc44: TSynWebTokenKind;
    function EsKeywordFunc48: TSynWebTokenKind;
    function EsKeywordFunc50: TSynWebTokenKind;
    function EsKeywordFunc51: TSynWebTokenKind;
    function EsKeywordFunc52: TSynWebTokenKind;
    function EsKeywordFunc53: TSynWebTokenKind;
    function EsKeywordFunc54: TSynWebTokenKind;
    function EsKeywordFunc56: TSynWebTokenKind;
    function EsKeywordFunc57: TSynWebTokenKind;
    function EsKeywordFunc59: TSynWebTokenKind;
    function EsKeywordFunc60: TSynWebTokenKind;
    function EsKeywordFunc63: TSynWebTokenKind;
    function EsKeywordFunc64: TSynWebTokenKind;
    function EsKeywordFunc69: TSynWebTokenKind;
    function EsKeywordFunc71: TSynWebTokenKind;
    function EsKeywordFunc72: TSynWebTokenKind;
    function EsKeywordFunc79: TSynWebTokenKind;
    function EsKeywordFunc80: TSynWebTokenKind;
    function EsKeywordFunc81: TSynWebTokenKind;
    function EsKeywordFunc82: TSynWebTokenKind;
    function EsKeywordFunc84: TSynWebTokenKind;
    function EsKeywordFunc87: TSynWebTokenKind;
    function EsKeywordFunc91: TSynWebTokenKind;
    function EsKeywordFunc96: TSynWebTokenKind;
    function EsKeywordFunc98: TSynWebTokenKind;
    function EsKeywordFunc101: TSynWebTokenKind;
    function EsKeywordFunc102: TSynWebTokenKind;
    function EsKeywordFunc103: TSynWebTokenKind;
    function EsKeywordFunc106: TSynWebTokenKind;
    function EsKeywordFunc120: TSynWebTokenKind;
    function EsKeywordFunc126: TSynWebTokenKind;
    function EsKeywordFunc160: TSynWebTokenKind;

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
    function PhpKeywordIdent: TSynWebTokenKind;
    function PhpKeywordFunc14: TSynWebTokenKind;
    function PhpKeywordFunc15: TSynWebTokenKind;
    function PhpKeywordFunc16: TSynWebTokenKind;
    function PhpKeywordFunc17: TSynWebTokenKind;
    function PhpKeywordFunc18: TSynWebTokenKind;
    function PhpKeywordFunc19: TSynWebTokenKind;
    function PhpKeywordFunc20: TSynWebTokenKind;
    function PhpKeywordFunc22: TSynWebTokenKind;
    function PhpKeywordFunc23: TSynWebTokenKind;
    function PhpKeywordFunc24: TSynWebTokenKind;
    function PhpKeywordFunc25: TSynWebTokenKind;
    function PhpKeywordFunc28: TSynWebTokenKind;
    function PhpKeywordFunc29: TSynWebTokenKind;
    function PhpKeywordFunc30: TSynWebTokenKind;
    function PhpKeywordFunc31: TSynWebTokenKind;
    function PhpKeywordFunc32: TSynWebTokenKind;
    function PhpKeywordFunc33: TSynWebTokenKind;
    function PhpKeywordFunc34: TSynWebTokenKind;
    function PhpKeywordFunc35: TSynWebTokenKind;
    function PhpKeywordFunc36: TSynWebTokenKind;
    function PhpKeywordFunc37: TSynWebTokenKind;
    function PhpKeywordFunc38: TSynWebTokenKind;
    function PhpKeywordFunc39: TSynWebTokenKind;
    function PhpKeywordFunc40: TSynWebTokenKind;
    function PhpKeywordFunc41: TSynWebTokenKind;
    function PhpKeywordFunc42: TSynWebTokenKind;
    function PhpKeywordFunc43: TSynWebTokenKind;
    function PhpKeywordFunc44: TSynWebTokenKind;
    function PhpKeywordFunc45: TSynWebTokenKind;
    function PhpKeywordFunc46: TSynWebTokenKind;
    function PhpKeywordFunc47: TSynWebTokenKind;
    function PhpKeywordFunc48: TSynWebTokenKind;
    function PhpKeywordFunc49: TSynWebTokenKind;
    function PhpKeywordFunc50: TSynWebTokenKind;
    function PhpKeywordFunc51: TSynWebTokenKind;
    function PhpKeywordFunc52: TSynWebTokenKind;
    function PhpKeywordFunc53: TSynWebTokenKind;
    function PhpKeywordFunc54: TSynWebTokenKind;
    function PhpKeywordFunc55: TSynWebTokenKind;
    function PhpKeywordFunc56: TSynWebTokenKind;
    function PhpKeywordFunc57: TSynWebTokenKind;
    function PhpKeywordFunc58: TSynWebTokenKind;
    function PhpKeywordFunc59: TSynWebTokenKind;
    function PhpKeywordFunc60: TSynWebTokenKind;
    function PhpKeywordFunc61: TSynWebTokenKind;
    function PhpKeywordFunc62: TSynWebTokenKind;
    function PhpKeywordFunc63: TSynWebTokenKind;
    function PhpKeywordFunc64: TSynWebTokenKind;
    function PhpKeywordFunc65: TSynWebTokenKind;
    function PhpKeywordFunc66: TSynWebTokenKind;
    function PhpKeywordFunc67: TSynWebTokenKind;
    function PhpKeywordFunc68: TSynWebTokenKind;
    function PhpKeywordFunc69: TSynWebTokenKind;
    function PhpKeywordFunc70: TSynWebTokenKind;
    function PhpKeywordFunc71: TSynWebTokenKind;
    function PhpKeywordFunc72: TSynWebTokenKind;
    function PhpKeywordFunc73: TSynWebTokenKind;
    function PhpKeywordFunc74: TSynWebTokenKind;
    function PhpKeywordFunc75: TSynWebTokenKind;
    function PhpKeywordFunc76: TSynWebTokenKind;
    function PhpKeywordFunc77: TSynWebTokenKind;
    function PhpKeywordFunc78: TSynWebTokenKind;
    function PhpKeywordFunc79: TSynWebTokenKind;
    function PhpKeywordFunc80: TSynWebTokenKind;
    function PhpKeywordFunc81: TSynWebTokenKind;
    function PhpKeywordFunc82: TSynWebTokenKind;
    function PhpKeywordFunc83: TSynWebTokenKind;
    function PhpKeywordFunc84: TSynWebTokenKind;
    function PhpKeywordFunc85: TSynWebTokenKind;
    function PhpKeywordFunc86: TSynWebTokenKind;
    function PhpKeywordFunc87: TSynWebTokenKind;
    function PhpKeywordFunc88: TSynWebTokenKind;
    function PhpKeywordFunc89: TSynWebTokenKind;
    function PhpKeywordFunc90: TSynWebTokenKind;
    function PhpKeywordFunc91: TSynWebTokenKind;
    function PhpKeywordFunc92: TSynWebTokenKind;
    function PhpKeywordFunc93: TSynWebTokenKind;
    function PhpKeywordFunc94: TSynWebTokenKind;
    function PhpKeywordFunc95: TSynWebTokenKind;
    function PhpKeywordFunc96: TSynWebTokenKind;
    function PhpKeywordFunc97: TSynWebTokenKind;
    function PhpKeywordFunc98: TSynWebTokenKind;
    function PhpKeywordFunc99: TSynWebTokenKind;
    function PhpKeywordFunc100: TSynWebTokenKind;
    function PhpKeywordFunc101: TSynWebTokenKind;
    function PhpKeywordFunc102: TSynWebTokenKind;
    function PhpKeywordFunc103: TSynWebTokenKind;
    function PhpKeywordFunc104: TSynWebTokenKind;
    function PhpKeywordFunc105: TSynWebTokenKind;
    function PhpKeywordFunc106: TSynWebTokenKind;
    function PhpKeywordFunc107: TSynWebTokenKind;
    function PhpKeywordFunc108: TSynWebTokenKind;
    function PhpKeywordFunc109: TSynWebTokenKind;
    function PhpKeywordFunc110: TSynWebTokenKind;
    function PhpKeywordFunc111: TSynWebTokenKind;
    function PhpKeywordFunc112: TSynWebTokenKind;
    function PhpKeywordFunc113: TSynWebTokenKind;
    function PhpKeywordFunc114: TSynWebTokenKind;
    function PhpKeywordFunc115: TSynWebTokenKind;
    function PhpKeywordFunc116: TSynWebTokenKind;
    function PhpKeywordFunc117: TSynWebTokenKind;
    function PhpKeywordFunc118: TSynWebTokenKind;
    function PhpKeywordFunc119: TSynWebTokenKind;
    function PhpKeywordFunc120: TSynWebTokenKind;
    function PhpKeywordFunc121: TSynWebTokenKind;
    function PhpKeywordFunc122: TSynWebTokenKind;
    function PhpKeywordFunc123: TSynWebTokenKind;
    function PhpKeywordFunc124: TSynWebTokenKind;
    function PhpKeywordFunc125: TSynWebTokenKind;
    function PhpKeywordFunc126: TSynWebTokenKind;
    function PhpKeywordFunc127: TSynWebTokenKind;
    function PhpKeywordFunc128: TSynWebTokenKind;
    function PhpKeywordFunc129: TSynWebTokenKind;
    function PhpKeywordFunc130: TSynWebTokenKind;
    function PhpKeywordFunc131: TSynWebTokenKind;
    function PhpKeywordFunc132: TSynWebTokenKind;
    function PhpKeywordFunc133: TSynWebTokenKind;
    function PhpKeywordFunc134: TSynWebTokenKind;
    function PhpKeywordFunc135: TSynWebTokenKind;
    function PhpKeywordFunc136: TSynWebTokenKind;
    function PhpKeywordFunc137: TSynWebTokenKind;
    function PhpKeywordFunc138: TSynWebTokenKind;
    function PhpKeywordFunc139: TSynWebTokenKind;
    function PhpKeywordFunc140: TSynWebTokenKind;
    function PhpKeywordFunc141: TSynWebTokenKind;
    function PhpKeywordFunc142: TSynWebTokenKind;
    function PhpKeywordFunc143: TSynWebTokenKind;
    function PhpKeywordFunc144: TSynWebTokenKind;
    function PhpKeywordFunc145: TSynWebTokenKind;
    function PhpKeywordFunc146: TSynWebTokenKind;
    function PhpKeywordFunc147: TSynWebTokenKind;
    function PhpKeywordFunc148: TSynWebTokenKind;
    function PhpKeywordFunc149: TSynWebTokenKind;
    function PhpKeywordFunc150: TSynWebTokenKind;
    function PhpKeywordFunc151: TSynWebTokenKind;
    function PhpKeywordFunc152: TSynWebTokenKind;
    function PhpKeywordFunc153: TSynWebTokenKind;
    function PhpKeywordFunc154: TSynWebTokenKind;
    function PhpKeywordFunc155: TSynWebTokenKind;
    function PhpKeywordFunc156: TSynWebTokenKind;
    function PhpKeywordFunc157: TSynWebTokenKind;
    function PhpKeywordFunc158: TSynWebTokenKind;
    function PhpKeywordFunc159: TSynWebTokenKind;
    function PhpKeywordFunc160: TSynWebTokenKind;
    function PhpKeywordFunc161: TSynWebTokenKind;
    function PhpKeywordFunc162: TSynWebTokenKind;
    function PhpKeywordFunc163: TSynWebTokenKind;
    function PhpKeywordFunc164: TSynWebTokenKind;
    function PhpKeywordFunc165: TSynWebTokenKind;
    function PhpKeywordFunc166: TSynWebTokenKind;
    function PhpKeywordFunc167: TSynWebTokenKind;
    function PhpKeywordFunc168: TSynWebTokenKind;
    function PhpKeywordFunc169: TSynWebTokenKind;
    function PhpKeywordFunc170: TSynWebTokenKind;
    function PhpKeywordFunc171: TSynWebTokenKind;
    function PhpKeywordFunc172: TSynWebTokenKind;
    function PhpKeywordFunc173: TSynWebTokenKind;
    function PhpKeywordFunc174: TSynWebTokenKind;
    function PhpKeywordFunc175: TSynWebTokenKind;
    function PhpKeywordFunc176: TSynWebTokenKind;
    function PhpKeywordFunc177: TSynWebTokenKind;
    function PhpKeywordFunc178: TSynWebTokenKind;
    function PhpKeywordFunc179: TSynWebTokenKind;
    function PhpKeywordFunc180: TSynWebTokenKind;
    function PhpKeywordFunc181: TSynWebTokenKind;
    function PhpKeywordFunc182: TSynWebTokenKind;
    function PhpKeywordFunc183: TSynWebTokenKind;
    function PhpKeywordFunc184: TSynWebTokenKind;
    function PhpKeywordFunc185: TSynWebTokenKind;
    function PhpKeywordFunc186: TSynWebTokenKind;
    function PhpKeywordFunc187: TSynWebTokenKind;
    function PhpKeywordFunc188: TSynWebTokenKind;
    function PhpKeywordFunc189: TSynWebTokenKind;
    function PhpKeywordFunc190: TSynWebTokenKind;
    function PhpKeywordFunc191: TSynWebTokenKind;
    function PhpKeywordFunc192: TSynWebTokenKind;
    function PhpKeywordFunc193: TSynWebTokenKind;
    function PhpKeywordFunc194: TSynWebTokenKind;
    function PhpKeywordFunc195: TSynWebTokenKind;
    function PhpKeywordFunc196: TSynWebTokenKind;
    function PhpKeywordFunc197: TSynWebTokenKind;
    function PhpKeywordFunc198: TSynWebTokenKind;
    function PhpKeywordFunc199: TSynWebTokenKind;
    function PhpKeywordFunc200: TSynWebTokenKind;
    function PhpKeywordFunc201: TSynWebTokenKind;
    function PhpKeywordFunc202: TSynWebTokenKind;
    function PhpKeywordFunc203: TSynWebTokenKind;
    function PhpKeywordFunc204: TSynWebTokenKind;
    function PhpKeywordFunc205: TSynWebTokenKind;
    function PhpKeywordFunc206: TSynWebTokenKind;
    function PhpKeywordFunc207: TSynWebTokenKind;
    function PhpKeywordFunc208: TSynWebTokenKind;
    function PhpKeywordFunc209: TSynWebTokenKind;
    function PhpKeywordFunc210: TSynWebTokenKind;
    function PhpKeywordFunc211: TSynWebTokenKind;
    function PhpKeywordFunc212: TSynWebTokenKind;
    function PhpKeywordFunc213: TSynWebTokenKind;
    function PhpKeywordFunc214: TSynWebTokenKind;
    function PhpKeywordFunc215: TSynWebTokenKind;
    function PhpKeywordFunc216: TSynWebTokenKind;
    function PhpKeywordFunc217: TSynWebTokenKind;
    function PhpKeywordFunc218: TSynWebTokenKind;
    function PhpKeywordFunc219: TSynWebTokenKind;
    function PhpKeywordFunc220: TSynWebTokenKind;
    function PhpKeywordFunc221: TSynWebTokenKind;
    function PhpKeywordFunc222: TSynWebTokenKind;
    function PhpKeywordFunc223: TSynWebTokenKind;
    function PhpKeywordFunc224: TSynWebTokenKind;
    function PhpKeywordFunc225: TSynWebTokenKind;
    function PhpKeywordFunc226: TSynWebTokenKind;
    function PhpKeywordFunc227: TSynWebTokenKind;
    function PhpKeywordFunc228: TSynWebTokenKind;
    function PhpKeywordFunc229: TSynWebTokenKind;
    function PhpKeywordFunc230: TSynWebTokenKind;
    function PhpKeywordFunc231: TSynWebTokenKind;
    function PhpKeywordFunc232: TSynWebTokenKind;
    function PhpKeywordFunc233: TSynWebTokenKind;
    function PhpKeywordFunc234: TSynWebTokenKind;
    function PhpKeywordFunc235: TSynWebTokenKind;
    function PhpKeywordFunc236: TSynWebTokenKind;
    function PhpKeywordFunc237: TSynWebTokenKind;
    function PhpKeywordFunc238: TSynWebTokenKind;
    function PhpKeywordFunc239: TSynWebTokenKind;
    function PhpKeywordFunc240: TSynWebTokenKind;
    function PhpKeywordFunc241: TSynWebTokenKind;
    function PhpKeywordFunc242: TSynWebTokenKind;
    function PhpKeywordFunc243: TSynWebTokenKind;
    function PhpKeywordFunc244: TSynWebTokenKind;
    function PhpKeywordFunc245: TSynWebTokenKind;
    function PhpKeywordFunc246: TSynWebTokenKind;
    function PhpKeywordFunc247: TSynWebTokenKind;
    function PhpKeywordFunc248: TSynWebTokenKind;
    function PhpKeywordFunc249: TSynWebTokenKind;
    function PhpKeywordFunc250: TSynWebTokenKind;
    function PhpKeywordFunc251: TSynWebTokenKind;
    function PhpKeywordFunc252: TSynWebTokenKind;
    function PhpKeywordFunc253: TSynWebTokenKind;
    function PhpKeywordFunc254: TSynWebTokenKind;
    function PhpKeywordFunc255: TSynWebTokenKind;
    function PhpKeywordFunc256: TSynWebTokenKind;
    function PhpKeywordFunc257: TSynWebTokenKind;
    function PhpKeywordFunc258: TSynWebTokenKind;
    function PhpKeywordFunc259: TSynWebTokenKind;
    function PhpKeywordFunc260: TSynWebTokenKind;
    function PhpKeywordFunc261: TSynWebTokenKind;
    function PhpKeywordFunc262: TSynWebTokenKind;
    function PhpKeywordFunc263: TSynWebTokenKind;
    function PhpKeywordFunc264: TSynWebTokenKind;
    function PhpKeywordFunc265: TSynWebTokenKind;
    function PhpKeywordFunc266: TSynWebTokenKind;
    function PhpKeywordFunc267: TSynWebTokenKind;
    function PhpKeywordFunc268: TSynWebTokenKind;
    function PhpKeywordFunc269: TSynWebTokenKind;
    function PhpKeywordFunc270: TSynWebTokenKind;
    function PhpKeywordFunc271: TSynWebTokenKind;
    function PhpKeywordFunc272: TSynWebTokenKind;
    function PhpKeywordFunc273: TSynWebTokenKind;
    function PhpKeywordFunc274: TSynWebTokenKind;
    function PhpKeywordFunc275: TSynWebTokenKind;
    function PhpKeywordFunc276: TSynWebTokenKind;
    function PhpKeywordFunc277: TSynWebTokenKind;
    function PhpKeywordFunc278: TSynWebTokenKind;
    function PhpKeywordFunc279: TSynWebTokenKind;
    function PhpKeywordFunc280: TSynWebTokenKind;
    function PhpKeywordFunc281: TSynWebTokenKind;
    function PhpKeywordFunc282: TSynWebTokenKind;
    function PhpKeywordFunc283: TSynWebTokenKind;
    function PhpKeywordFunc284: TSynWebTokenKind;
    function PhpKeywordFunc285: TSynWebTokenKind;
    function PhpKeywordFunc286: TSynWebTokenKind;
    function PhpKeywordFunc287: TSynWebTokenKind;
    function PhpKeywordFunc288: TSynWebTokenKind;
    function PhpKeywordFunc289: TSynWebTokenKind;
    function PhpKeywordFunc290: TSynWebTokenKind;
    function PhpKeywordFunc291: TSynWebTokenKind;
    function PhpKeywordFunc292: TSynWebTokenKind;
    function PhpKeywordFunc293: TSynWebTokenKind;
    function PhpKeywordFunc294: TSynWebTokenKind;
    function PhpKeywordFunc295: TSynWebTokenKind;
    function PhpKeywordFunc296: TSynWebTokenKind;
    function PhpKeywordFunc297: TSynWebTokenKind;
    function PhpKeywordFunc298: TSynWebTokenKind;
    function PhpKeywordFunc299: TSynWebTokenKind;
    function PhpKeywordFunc300: TSynWebTokenKind;
    function PhpKeywordFunc302: TSynWebTokenKind;
    function PhpKeywordFunc303: TSynWebTokenKind;
    function PhpKeywordFunc304: TSynWebTokenKind;
    function PhpKeywordFunc305: TSynWebTokenKind;
    function PhpKeywordFunc306: TSynWebTokenKind;
    function PhpKeywordFunc307: TSynWebTokenKind;
    function PhpKeywordFunc308: TSynWebTokenKind;
    function PhpKeywordFunc309: TSynWebTokenKind;
    function PhpKeywordFunc310: TSynWebTokenKind;
    function PhpKeywordFunc311: TSynWebTokenKind;
    function PhpKeywordFunc312: TSynWebTokenKind;
    function PhpKeywordFunc313: TSynWebTokenKind;
    function PhpKeywordFunc314: TSynWebTokenKind;
    function PhpKeywordFunc315: TSynWebTokenKind;
    function PhpKeywordFunc316: TSynWebTokenKind;
    function PhpKeywordFunc317: TSynWebTokenKind;
    function PhpKeywordFunc318: TSynWebTokenKind;
    function PhpKeywordFunc319: TSynWebTokenKind;
    function PhpKeywordFunc321: TSynWebTokenKind;
    function PhpKeywordFunc322: TSynWebTokenKind;
    function PhpKeywordFunc323: TSynWebTokenKind;
    function PhpKeywordFunc324: TSynWebTokenKind;
    function PhpKeywordFunc325: TSynWebTokenKind;
    function PhpKeywordFunc326: TSynWebTokenKind;
    function PhpKeywordFunc327: TSynWebTokenKind;
    function PhpKeywordFunc328: TSynWebTokenKind;
    function PhpKeywordFunc329: TSynWebTokenKind;
    function PhpKeywordFunc330: TSynWebTokenKind;
    function PhpKeywordFunc331: TSynWebTokenKind;
    function PhpKeywordFunc332: TSynWebTokenKind;
    function PhpKeywordFunc333: TSynWebTokenKind;
    function PhpKeywordFunc334: TSynWebTokenKind;
    function PhpKeywordFunc335: TSynWebTokenKind;
    function PhpKeywordFunc336: TSynWebTokenKind;
    function PhpKeywordFunc337: TSynWebTokenKind;
    function PhpKeywordFunc338: TSynWebTokenKind;
    function PhpKeywordFunc339: TSynWebTokenKind;
    function PhpKeywordFunc340: TSynWebTokenKind;
    function PhpKeywordFunc341: TSynWebTokenKind;
    function PhpKeywordFunc343: TSynWebTokenKind;
    function PhpKeywordFunc344: TSynWebTokenKind;
    function PhpKeywordFunc345: TSynWebTokenKind;
    function PhpKeywordFunc346: TSynWebTokenKind;
    function PhpKeywordFunc347: TSynWebTokenKind;
    function PhpKeywordFunc348: TSynWebTokenKind;
    function PhpKeywordFunc349: TSynWebTokenKind;
    function PhpKeywordFunc351: TSynWebTokenKind;
    function PhpKeywordFunc352: TSynWebTokenKind;
    function PhpKeywordFunc353: TSynWebTokenKind;
    function PhpKeywordFunc354: TSynWebTokenKind;
    function PhpKeywordFunc357: TSynWebTokenKind;
    function PhpKeywordFunc358: TSynWebTokenKind;
    function PhpKeywordFunc359: TSynWebTokenKind;
    function PhpKeywordFunc360: TSynWebTokenKind;
    function PhpKeywordFunc361: TSynWebTokenKind;
    function PhpKeywordFunc364: TSynWebTokenKind;
    function PhpKeywordFunc366: TSynWebTokenKind;
    function PhpKeywordFunc369: TSynWebTokenKind;
    function PhpKeywordFunc370: TSynWebTokenKind;
    function PhpKeywordFunc373: TSynWebTokenKind;
    function PhpKeywordFunc374: TSynWebTokenKind;
    function PhpKeywordFunc375: TSynWebTokenKind;
    function PhpKeywordFunc376: TSynWebTokenKind;
    function PhpKeywordFunc377: TSynWebTokenKind;
    function PhpKeywordFunc385: TSynWebTokenKind;
    function PhpKeywordFunc387: TSynWebTokenKind;
    function PhpKeywordFunc393: TSynWebTokenKind;
    function PhpKeywordFunc399: TSynWebTokenKind;
    function PhpKeywordFunc402: TSynWebTokenKind;
    function PhpKeywordFunc407: TSynWebTokenKind;
    function PhpKeywordFunc426: TSynWebTokenKind;
    function PhpKeywordFunc433: TSynWebTokenKind;
    function PhpKeywordFunc439: TSynWebTokenKind;
    function PhpKeywordFunc442: TSynWebTokenKind;
    function PhpKeywordFunc443: TSynWebTokenKind;
    function PhpKeywordFunc445: TSynWebTokenKind;
    function PhpKeywordFunc450: TSynWebTokenKind;
    function PhpKeywordFunc465: TSynWebTokenKind;
    function PhpKeywordFunc466: TSynWebTokenKind;
    function PhpKeywordFunc487: TSynWebTokenKind;
    function PhpKeywordFunc491: TSynWebTokenKind;
    function PhpKeywordFunc496: TSynWebTokenKind;
    function PhpKeywordFunc520: TSynWebTokenKind;
    function PhpKeywordFunc541: TSynWebTokenKind;
    function PhpKeywordFunc560: TSynWebTokenKind;

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
  FHtmlTagIdentFuncTable[1]:=HtmlTagFunc1;
  FHtmlTagIdentFuncTable[2]:=HtmlTagFunc2;
  FHtmlTagIdentFuncTable[8]:=HtmlTagFunc8;
  FHtmlTagIdentFuncTable[9]:=HtmlTagFunc9;
  FHtmlTagIdentFuncTable[16]:=HtmlTagFunc16;
  FHtmlTagIdentFuncTable[17]:=HtmlTagFunc17;
  FHtmlTagIdentFuncTable[18]:=HtmlTagFunc18;
  FHtmlTagIdentFuncTable[19]:=HtmlTagFunc19;
  FHtmlTagIdentFuncTable[20]:=HtmlTagFunc20;
  FHtmlTagIdentFuncTable[21]:=HtmlTagFunc21;
  FHtmlTagIdentFuncTable[23]:=HtmlTagFunc23;
  FHtmlTagIdentFuncTable[24]:=HtmlTagFunc24;
  FHtmlTagIdentFuncTable[25]:=HtmlTagFunc25;
  FHtmlTagIdentFuncTable[26]:=HtmlTagFunc26;
  FHtmlTagIdentFuncTable[27]:=HtmlTagFunc27;
  FHtmlTagIdentFuncTable[28]:=HtmlTagFunc28;
  FHtmlTagIdentFuncTable[29]:=HtmlTagFunc29;
  FHtmlTagIdentFuncTable[30]:=HtmlTagFunc30;
  FHtmlTagIdentFuncTable[31]:=HtmlTagFunc31;
  FHtmlTagIdentFuncTable[32]:=HtmlTagFunc32;
  FHtmlTagIdentFuncTable[33]:=HtmlTagFunc33;
  FHtmlTagIdentFuncTable[35]:=HtmlTagFunc35;
  FHtmlTagIdentFuncTable[37]:=HtmlTagFunc37;
  FHtmlTagIdentFuncTable[38]:=HtmlTagFunc38;
  FHtmlTagIdentFuncTable[39]:=HtmlTagFunc39;
  FHtmlTagIdentFuncTable[40]:=HtmlTagFunc40;
  FHtmlTagIdentFuncTable[41]:=HtmlTagFunc41;
  FHtmlTagIdentFuncTable[42]:=HtmlTagFunc42;
  FHtmlTagIdentFuncTable[43]:=HtmlTagFunc43;
  FHtmlTagIdentFuncTable[46]:=HtmlTagFunc46;
  FHtmlTagIdentFuncTable[47]:=HtmlTagFunc47;
  FHtmlTagIdentFuncTable[49]:=HtmlTagFunc49;
  FHtmlTagIdentFuncTable[50]:=HtmlTagFunc50;
  FHtmlTagIdentFuncTable[52]:=HtmlTagFunc52;
  FHtmlTagIdentFuncTable[53]:=HtmlTagFunc53;
  FHtmlTagIdentFuncTable[55]:=HtmlTagFunc55;
  FHtmlTagIdentFuncTable[56]:=HtmlTagFunc56;
  FHtmlTagIdentFuncTable[57]:=HtmlTagFunc57;
  FHtmlTagIdentFuncTable[64]:=HtmlTagFunc64;
  FHtmlTagIdentFuncTable[65]:=HtmlTagFunc65;
  FHtmlTagIdentFuncTable[66]:=HtmlTagFunc66;
  FHtmlTagIdentFuncTable[70]:=HtmlTagFunc70;
  FHtmlTagIdentFuncTable[76]:=HtmlTagFunc76;
  FHtmlTagIdentFuncTable[78]:=HtmlTagFunc78;
  FHtmlTagIdentFuncTable[80]:=HtmlTagFunc80;
  FHtmlTagIdentFuncTable[81]:=HtmlTagFunc81;
  FHtmlTagIdentFuncTable[82]:=HtmlTagFunc82;
  FHtmlTagIdentFuncTable[84]:=HtmlTagFunc84;
  FHtmlTagIdentFuncTable[85]:=HtmlTagFunc85;
  FHtmlTagIdentFuncTable[87]:=HtmlTagFunc87;
  FHtmlTagIdentFuncTable[89]:=HtmlTagFunc89;
  FHtmlTagIdentFuncTable[91]:=HtmlTagFunc91;
  FHtmlTagIdentFuncTable[92]:=HtmlTagFunc92;
  FHtmlTagIdentFuncTable[93]:=HtmlTagFunc93;
  FHtmlTagIdentFuncTable[94]:=HtmlTagFunc94;
  FHtmlTagIdentFuncTable[107]:=HtmlTagFunc107;
  FHtmlTagIdentFuncTable[114]:=HtmlTagFunc114;
  FHtmlTagIdentFuncTable[121]:=HtmlTagFunc121;
  FHtmlTagIdentFuncTable[128]:=HtmlTagFunc128;

  pF := PSynWebIdentFuncTableFunc(@FHtmlAttrIdentFuncTable);
  for I := Low(FHtmlTagIdentFuncTable) to High(FHtmlAttrIdentFuncTable) do
  begin
    pF^ := HtmlAttrUndef;
    Inc(pF);
  end;
  FHtmlAttrIdentFuncTable[13]:=HtmlAttrFunc13;
  FHtmlAttrIdentFuncTable[15]:=HtmlAttrFunc15;
  FHtmlAttrIdentFuncTable[23]:=HtmlAttrFunc23;
  FHtmlAttrIdentFuncTable[26]:=HtmlAttrFunc26;
  FHtmlAttrIdentFuncTable[27]:=HtmlAttrFunc27;
  FHtmlAttrIdentFuncTable[30]:=HtmlAttrFunc30;
  FHtmlAttrIdentFuncTable[31]:=HtmlAttrFunc31;
  FHtmlAttrIdentFuncTable[32]:=HtmlAttrFunc32;
  FHtmlAttrIdentFuncTable[33]:=HtmlAttrFunc33;
  FHtmlAttrIdentFuncTable[34]:=HtmlAttrFunc34;
  FHtmlAttrIdentFuncTable[35]:=HtmlAttrFunc35;
  FHtmlAttrIdentFuncTable[37]:=HtmlAttrFunc37;
  FHtmlAttrIdentFuncTable[38]:=HtmlAttrFunc38;
  FHtmlAttrIdentFuncTable[39]:=HtmlAttrFunc39;
  FHtmlAttrIdentFuncTable[40]:=HtmlAttrFunc40;
  FHtmlAttrIdentFuncTable[43]:=HtmlAttrFunc43;
  FHtmlAttrIdentFuncTable[45]:=HtmlAttrFunc45;
  FHtmlAttrIdentFuncTable[46]:=HtmlAttrFunc46;
  FHtmlAttrIdentFuncTable[47]:=HtmlAttrFunc47;
  FHtmlAttrIdentFuncTable[48]:=HtmlAttrFunc48;
  FHtmlAttrIdentFuncTable[49]:=HtmlAttrFunc49;
  FHtmlAttrIdentFuncTable[50]:=HtmlAttrFunc50;
  FHtmlAttrIdentFuncTable[52]:=HtmlAttrFunc52;
  FHtmlAttrIdentFuncTable[53]:=HtmlAttrFunc53;
  FHtmlAttrIdentFuncTable[54]:=HtmlAttrFunc54;
  FHtmlAttrIdentFuncTable[55]:=HtmlAttrFunc55;
  FHtmlAttrIdentFuncTable[56]:=HtmlAttrFunc56;
  FHtmlAttrIdentFuncTable[57]:=HtmlAttrFunc57;
  FHtmlAttrIdentFuncTable[58]:=HtmlAttrFunc58;
  FHtmlAttrIdentFuncTable[59]:=HtmlAttrFunc59;
  FHtmlAttrIdentFuncTable[60]:=HtmlAttrFunc60;
  FHtmlAttrIdentFuncTable[61]:=HtmlAttrFunc61;
  FHtmlAttrIdentFuncTable[62]:=HtmlAttrFunc62;
  FHtmlAttrIdentFuncTable[63]:=HtmlAttrFunc63;
  FHtmlAttrIdentFuncTable[64]:=HtmlAttrFunc64;
  FHtmlAttrIdentFuncTable[65]:=HtmlAttrFunc65;
  FHtmlAttrIdentFuncTable[66]:=HtmlAttrFunc66;
  FHtmlAttrIdentFuncTable[67]:=HtmlAttrFunc67;
  FHtmlAttrIdentFuncTable[68]:=HtmlAttrFunc68;
  FHtmlAttrIdentFuncTable[69]:=HtmlAttrFunc69;
  FHtmlAttrIdentFuncTable[71]:=HtmlAttrFunc71;
  FHtmlAttrIdentFuncTable[72]:=HtmlAttrFunc72;
  FHtmlAttrIdentFuncTable[73]:=HtmlAttrFunc73;
  FHtmlAttrIdentFuncTable[74]:=HtmlAttrFunc74;
  FHtmlAttrIdentFuncTable[75]:=HtmlAttrFunc75;
  FHtmlAttrIdentFuncTable[77]:=HtmlAttrFunc77;
  FHtmlAttrIdentFuncTable[78]:=HtmlAttrFunc78;
  FHtmlAttrIdentFuncTable[79]:=HtmlAttrFunc79;
  FHtmlAttrIdentFuncTable[80]:=HtmlAttrFunc80;
  FHtmlAttrIdentFuncTable[81]:=HtmlAttrFunc81;
  FHtmlAttrIdentFuncTable[82]:=HtmlAttrFunc82;
  FHtmlAttrIdentFuncTable[85]:=HtmlAttrFunc85;
  FHtmlAttrIdentFuncTable[87]:=HtmlAttrFunc87;
  FHtmlAttrIdentFuncTable[88]:=HtmlAttrFunc88;
  FHtmlAttrIdentFuncTable[91]:=HtmlAttrFunc91;
  FHtmlAttrIdentFuncTable[93]:=HtmlAttrFunc93;
  FHtmlAttrIdentFuncTable[94]:=HtmlAttrFunc94;
  FHtmlAttrIdentFuncTable[96]:=HtmlAttrFunc96;
  FHtmlAttrIdentFuncTable[98]:=HtmlAttrFunc98;
  FHtmlAttrIdentFuncTable[101]:=HtmlAttrFunc101;
  FHtmlAttrIdentFuncTable[102]:=HtmlAttrFunc102;
  FHtmlAttrIdentFuncTable[103]:=HtmlAttrFunc103;
  FHtmlAttrIdentFuncTable[104]:=HtmlAttrFunc104;
  FHtmlAttrIdentFuncTable[105]:=HtmlAttrFunc105;
  FHtmlAttrIdentFuncTable[106]:=HtmlAttrFunc106;
  FHtmlAttrIdentFuncTable[107]:=HtmlAttrFunc107;
  FHtmlAttrIdentFuncTable[108]:=HtmlAttrFunc108;
  FHtmlAttrIdentFuncTable[109]:=HtmlAttrFunc109;
  FHtmlAttrIdentFuncTable[110]:=HtmlAttrFunc110;
  FHtmlAttrIdentFuncTable[111]:=HtmlAttrFunc111;
  FHtmlAttrIdentFuncTable[113]:=HtmlAttrFunc113;
  FHtmlAttrIdentFuncTable[114]:=HtmlAttrFunc114;
  FHtmlAttrIdentFuncTable[119]:=HtmlAttrFunc119;
  FHtmlAttrIdentFuncTable[126]:=HtmlAttrFunc126;
  FHtmlAttrIdentFuncTable[127]:=HtmlAttrFunc127;
  FHtmlAttrIdentFuncTable[139]:=HtmlAttrFunc139;
  FHtmlAttrIdentFuncTable[147]:=HtmlAttrFunc147;
  FHtmlAttrIdentFuncTable[157]:=HtmlAttrFunc157;
  FHtmlAttrIdentFuncTable[158]:=HtmlAttrFunc158;
  FHtmlAttrIdentFuncTable[162]:=HtmlAttrFunc162;

  pF2 := PSynWebIdent2FuncTableFunc(@FHtmlSpecialIdentFuncTable);
  for I := Low(FHtmlSpecialIdentFuncTable) to High(FHtmlSpecialIdentFuncTable) do
  begin
    pF2^ := HtmlSpecialUndef;
    Inc(pF2);
  end;
  FHtmlSpecialIdentFuncTable[12]:=HtmlSpecialFunc12;
  FHtmlSpecialIdentFuncTable[16]:=HtmlSpecialFunc16;
  FHtmlSpecialIdentFuncTable[17]:=HtmlSpecialFunc17;
  FHtmlSpecialIdentFuncTable[19]:=HtmlSpecialFunc19;
  FHtmlSpecialIdentFuncTable[20]:=HtmlSpecialFunc20;
  FHtmlSpecialIdentFuncTable[22]:=HtmlSpecialFunc22;
  FHtmlSpecialIdentFuncTable[23]:=HtmlSpecialFunc23;
  FHtmlSpecialIdentFuncTable[25]:=HtmlSpecialFunc25;
  FHtmlSpecialIdentFuncTable[26]:=HtmlSpecialFunc26;
  FHtmlSpecialIdentFuncTable[27]:=HtmlSpecialFunc27;
  FHtmlSpecialIdentFuncTable[28]:=HtmlSpecialFunc28;
  FHtmlSpecialIdentFuncTable[30]:=HtmlSpecialFunc30;
  FHtmlSpecialIdentFuncTable[32]:=HtmlSpecialFunc32;
  FHtmlSpecialIdentFuncTable[33]:=HtmlSpecialFunc33;
  FHtmlSpecialIdentFuncTable[34]:=HtmlSpecialFunc34;
  FHtmlSpecialIdentFuncTable[35]:=HtmlSpecialFunc35;
  FHtmlSpecialIdentFuncTable[36]:=HtmlSpecialFunc36;
  FHtmlSpecialIdentFuncTable[38]:=HtmlSpecialFunc38;
  FHtmlSpecialIdentFuncTable[39]:=HtmlSpecialFunc39;
  FHtmlSpecialIdentFuncTable[40]:=HtmlSpecialFunc40;
  FHtmlSpecialIdentFuncTable[41]:=HtmlSpecialFunc41;
  FHtmlSpecialIdentFuncTable[42]:=HtmlSpecialFunc42;
  FHtmlSpecialIdentFuncTable[43]:=HtmlSpecialFunc43;
  FHtmlSpecialIdentFuncTable[44]:=HtmlSpecialFunc44;
  FHtmlSpecialIdentFuncTable[45]:=HtmlSpecialFunc45;
  FHtmlSpecialIdentFuncTable[46]:=HtmlSpecialFunc46;
  FHtmlSpecialIdentFuncTable[47]:=HtmlSpecialFunc47;
  FHtmlSpecialIdentFuncTable[48]:=HtmlSpecialFunc48;
  FHtmlSpecialIdentFuncTable[49]:=HtmlSpecialFunc49;
  FHtmlSpecialIdentFuncTable[50]:=HtmlSpecialFunc50;
  FHtmlSpecialIdentFuncTable[51]:=HtmlSpecialFunc51;
  FHtmlSpecialIdentFuncTable[52]:=HtmlSpecialFunc52;
  FHtmlSpecialIdentFuncTable[53]:=HtmlSpecialFunc53;
  FHtmlSpecialIdentFuncTable[54]:=HtmlSpecialFunc54;
  FHtmlSpecialIdentFuncTable[55]:=HtmlSpecialFunc55;
  FHtmlSpecialIdentFuncTable[56]:=HtmlSpecialFunc56;
  FHtmlSpecialIdentFuncTable[57]:=HtmlSpecialFunc57;
  FHtmlSpecialIdentFuncTable[58]:=HtmlSpecialFunc58;
  FHtmlSpecialIdentFuncTable[59]:=HtmlSpecialFunc59;
  FHtmlSpecialIdentFuncTable[61]:=HtmlSpecialFunc61;
  FHtmlSpecialIdentFuncTable[62]:=HtmlSpecialFunc62;
  FHtmlSpecialIdentFuncTable[63]:=HtmlSpecialFunc63;
  FHtmlSpecialIdentFuncTable[64]:=HtmlSpecialFunc64;
  FHtmlSpecialIdentFuncTable[65]:=HtmlSpecialFunc65;
  FHtmlSpecialIdentFuncTable[66]:=HtmlSpecialFunc66;
  FHtmlSpecialIdentFuncTable[67]:=HtmlSpecialFunc67;
  FHtmlSpecialIdentFuncTable[68]:=HtmlSpecialFunc68;
  FHtmlSpecialIdentFuncTable[69]:=HtmlSpecialFunc69;
  FHtmlSpecialIdentFuncTable[70]:=HtmlSpecialFunc70;
  FHtmlSpecialIdentFuncTable[71]:=HtmlSpecialFunc71;
  FHtmlSpecialIdentFuncTable[72]:=HtmlSpecialFunc72;
  FHtmlSpecialIdentFuncTable[73]:=HtmlSpecialFunc73;
  FHtmlSpecialIdentFuncTable[74]:=HtmlSpecialFunc74;
  FHtmlSpecialIdentFuncTable[75]:=HtmlSpecialFunc75;
  FHtmlSpecialIdentFuncTable[76]:=HtmlSpecialFunc76;
  FHtmlSpecialIdentFuncTable[77]:=HtmlSpecialFunc77;
  FHtmlSpecialIdentFuncTable[78]:=HtmlSpecialFunc78;
  FHtmlSpecialIdentFuncTable[79]:=HtmlSpecialFunc79;
  FHtmlSpecialIdentFuncTable[81]:=HtmlSpecialFunc81;
  FHtmlSpecialIdentFuncTable[83]:=HtmlSpecialFunc83;
  FHtmlSpecialIdentFuncTable[84]:=HtmlSpecialFunc84;
  FHtmlSpecialIdentFuncTable[85]:=HtmlSpecialFunc85;
  FHtmlSpecialIdentFuncTable[86]:=HtmlSpecialFunc86;
  FHtmlSpecialIdentFuncTable[87]:=HtmlSpecialFunc87;
  FHtmlSpecialIdentFuncTable[90]:=HtmlSpecialFunc90;
  FHtmlSpecialIdentFuncTable[91]:=HtmlSpecialFunc91;
  FHtmlSpecialIdentFuncTable[95]:=HtmlSpecialFunc95;
  FHtmlSpecialIdentFuncTable[106]:=HtmlSpecialFunc106;
  FHtmlSpecialIdentFuncTable[111]:=HtmlSpecialFunc111;
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

function TSynWebEngine.HtmlTagUndef: TSynWebTokenKind;
begin
  Result:=stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc1: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(0) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc2: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(6) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc8: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(21) or
      HtmlTagKeyComp(34) or
      HtmlTagKeyComp(35) or
      HtmlTagKeyComp(36) or
      HtmlTagKeyComp(37) or
      HtmlTagKeyComp(38) or
      HtmlTagKeyComp(39) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc9: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(43) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc16: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(26) or
      HtmlTagKeyComp(63) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc17: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(49) or
      HtmlTagKeyComp(66) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc18: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(10) or
      HtmlTagKeyComp(28) or
      HtmlTagKeyComp(40) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc19: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(67) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc20: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(13) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc21: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(9) or
      HtmlTagKeyComp(22) or
      HtmlTagKeyComp(52) or
      HtmlTagKeyComp(88) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc23: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(1) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc24: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(23) or
      HtmlTagKeyComp(27) or
      HtmlTagKeyComp(80) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc25: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(5) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc26: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(41) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc27: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(7) or
      HtmlTagKeyComp(18) or
      HtmlTagKeyComp(60) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc28: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(83) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc29: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(45) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc30: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(19) or
      HtmlTagKeyComp(54) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc31: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(24) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc32: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(50) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc33: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(89) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc35: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(25) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc37: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(17) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc38: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(84) or
      HtmlTagKeyComp(86) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc39: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(56) or
      HtmlTagKeyComp(65) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc40: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(78) or
      HtmlTagKeyComp(87) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc41: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(90) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc42: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(47) or
      HtmlTagKeyComp(76) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc43: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(32) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc46: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(12) or
      HtmlTagKeyComp(53) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc47: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(51) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc49: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(64) or
      HtmlTagKeyComp(68) or
      HtmlTagKeyComp(91) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc50: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(72) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc52: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(31) or
      HtmlTagKeyComp(44) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc53: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(42) or
      HtmlTagKeyComp(55) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc55: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(30) or
      HtmlTagKeyComp(59) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc56: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(77) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc57: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(71) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc64: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(70) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc65: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(16) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc66: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(79) or
      HtmlTagKeyComp(85) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc70: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(3) or
      HtmlTagKeyComp(4) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc76: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(82) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc78: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(15) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc80: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(29) or
      HtmlTagKeyComp(46) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc81: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(75) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc82: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(8) or
      HtmlTagKeyComp(73) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc84: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(48) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc85: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(69) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc87: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(33) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc89: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(2) or
      HtmlTagKeyComp(62) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc91: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(57) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc92: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(14) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc93: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(74) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc94: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(81) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc107: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(20) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc114: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(58) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc121: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(11) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc128: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(61) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

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

function TSynWebEngine.HtmlAttrUndef: TSynWebTokenKind;
begin
  Result:=stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc13: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(55) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc15: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(45) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc23: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(0) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc26: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(32) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc27: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(23) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc30: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(15) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc31: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(40) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc32: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(57) or
      HtmlAttrKeyComp(65) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc33: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(7) or
      HtmlAttrKeyComp(68) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc34: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(58) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc35: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(95) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc37: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(19) or
      HtmlAttrKeyComp(51) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc38: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(39) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc39: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(18) or
      HtmlAttrKeyComp(22) or
      HtmlAttrKeyComp(46) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc40: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(107) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc43: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(5) or
      HtmlAttrKeyComp(47) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc45: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(96) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc46: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(60) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc47: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(6) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc48: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(1) or
      HtmlAttrKeyComp(33) or
      HtmlAttrKeyComp(38) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc49: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(27) or
      HtmlAttrKeyComp(104) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc50: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(106) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc52: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(53) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc53: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(9) or
      HtmlAttrKeyComp(100) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc54: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(20) or
      HtmlAttrKeyComp(24) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc55: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(73) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc56: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(41) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc57: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(16) or
      HtmlAttrKeyComp(50) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc58: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(56) or
      HtmlAttrKeyComp(101) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc59: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(105) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc60: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(49) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc61: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(82) or
      HtmlAttrKeyComp(119) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc62: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(4) or
      HtmlAttrKeyComp(12) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc63: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(26) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc64: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(124) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc65: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(66) or
      HtmlAttrKeyComp(118) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc66: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(8) or
      HtmlAttrKeyComp(36) or
      HtmlAttrKeyComp(44) or
      HtmlAttrKeyComp(69) or
      HtmlAttrKeyComp(71) or
      HtmlAttrKeyComp(115) or
      HtmlAttrKeyComp(116) or
      HtmlAttrKeyComp(123) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc67: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(21) or
      HtmlAttrKeyComp(75) or
      HtmlAttrKeyComp(76) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc68: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(59) or
      HtmlAttrKeyComp(122) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc69: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(114) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc71: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(29) or
      HtmlAttrKeyComp(42) or
      HtmlAttrKeyComp(52) or
      HtmlAttrKeyComp(113) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc72: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(11) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc73: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(103) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc74: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(17) or
      HtmlAttrKeyComp(31) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc75: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(97) or
      HtmlAttrKeyComp(99) or
      HtmlAttrKeyComp(117) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc77: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(37) or
      HtmlAttrKeyComp(125) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc78: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(109) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc79: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(61) or
      HtmlAttrKeyComp(112) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc80: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(28) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc81: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(92) or
      HtmlAttrKeyComp(110) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc82: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(74) or
      HtmlAttrKeyComp(127) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc85: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(77) or
      HtmlAttrKeyComp(108) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc87: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(13) or
      HtmlAttrKeyComp(72) or
      HtmlAttrKeyComp(126) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc88: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(43) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc91: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(3) or
      HtmlAttrKeyComp(30) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc93: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(25) or
      HtmlAttrKeyComp(78) or
      HtmlAttrKeyComp(89) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc94: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(94) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc96: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(10) or
      HtmlAttrKeyComp(88) or
      HtmlAttrKeyComp(91) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc98: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(93) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc101: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(14) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc102: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(121) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc103: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(2) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc104: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(64) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc105: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(48) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc106: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(98) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc107: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(81) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc108: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(67) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc109: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(102) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc110: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(111) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc111: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(70) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc113: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(90) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc114: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(35) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc119: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(34) or
      HtmlAttrKeyComp(54) or
      HtmlAttrKeyComp(62) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc126: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(63) or
      HtmlAttrKeyComp(79) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc127: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(120) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc139: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(87) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc147: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(80) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc157: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(84) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc158: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(83) or
      HtmlAttrKeyComp(85) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

function TSynWebEngine.HtmlAttrFunc162: TSynWebTokenKind;
begin
  if  HtmlAttrKeyComp(86) then
    Result := stkHtmlTagKey
  else
    Result := stkHtmlTagKeyUndef;
end;

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

function TSynWebEngine.HtmlSpecialUndef: Boolean;
begin
  Result:=False;
end;

function TSynWebEngine.HtmlSpecialFunc12: Boolean;
begin
  if  HtmlSpecialKeyComp(79) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc16: Boolean;
begin
  if  HtmlSpecialKeyComp(46) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc17: Boolean;
begin
  if  HtmlSpecialKeyComp(111) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc19: Boolean;
begin
  if  HtmlSpecialKeyComp(13) or
      HtmlSpecialKeyComp(129) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc20: Boolean;
begin
  if  HtmlSpecialKeyComp(28) or
      HtmlSpecialKeyComp(33) or
      HtmlSpecialKeyComp(34) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc22: Boolean;
begin
  if  HtmlSpecialKeyComp(14) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc23: Boolean;
begin
  if  HtmlSpecialKeyComp(130) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc25: Boolean;
begin
  if  HtmlSpecialKeyComp(168) or
      HtmlSpecialKeyComp(169) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc26: Boolean;
begin
  if  HtmlSpecialKeyComp(63) or
      HtmlSpecialKeyComp(64) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc27: Boolean;
begin
  if  HtmlSpecialKeyComp(80) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc28: Boolean;
begin
  if  HtmlSpecialKeyComp(24) or
      HtmlSpecialKeyComp(25) or
      HtmlSpecialKeyComp(73) or
      HtmlSpecialKeyComp(74) or
      HtmlSpecialKeyComp(75) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc30: Boolean;
begin
  if  HtmlSpecialKeyComp(12) or
      HtmlSpecialKeyComp(126) or
      HtmlSpecialKeyComp(188) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc32: Boolean;
begin
  if  HtmlSpecialKeyComp(118) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc33: Boolean;
begin
  if  HtmlSpecialKeyComp(31) or
      HtmlSpecialKeyComp(35) or
      HtmlSpecialKeyComp(65) or
      HtmlSpecialKeyComp(66) or
      HtmlSpecialKeyComp(103) or
      HtmlSpecialKeyComp(104) or
      HtmlSpecialKeyComp(152) or
      HtmlSpecialKeyComp(166) or
      HtmlSpecialKeyComp(167) or
      HtmlSpecialKeyComp(242) or
      HtmlSpecialKeyComp(243) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc34: Boolean;
begin
  if  HtmlSpecialKeyComp(2) or
      HtmlSpecialKeyComp(3) or
      HtmlSpecialKeyComp(5) or
      HtmlSpecialKeyComp(6) or
      HtmlSpecialKeyComp(105) or
      HtmlSpecialKeyComp(124) or
      HtmlSpecialKeyComp(125) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc35: Boolean;
begin
  if  HtmlSpecialKeyComp(77) or
      HtmlSpecialKeyComp(78) or
      HtmlSpecialKeyComp(92) or
      HtmlSpecialKeyComp(119) or
      HtmlSpecialKeyComp(136) or
      HtmlSpecialKeyComp(137) or
      HtmlSpecialKeyComp(180) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc36: Boolean;
begin
  if  HtmlSpecialKeyComp(29) or
      HtmlSpecialKeyComp(30) or
      HtmlSpecialKeyComp(162) or
      HtmlSpecialKeyComp(187) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc38: Boolean;
begin
  if  HtmlSpecialKeyComp(10) or
      HtmlSpecialKeyComp(11) or
      HtmlSpecialKeyComp(53) or
      HtmlSpecialKeyComp(54) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc39: Boolean;
begin
  if  HtmlSpecialKeyComp(37) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc40: Boolean;
begin
  if  HtmlSpecialKeyComp(40) or
      HtmlSpecialKeyComp(181) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc41: Boolean;
begin
  if  HtmlSpecialKeyComp(44) or
      HtmlSpecialKeyComp(45) or
      HtmlSpecialKeyComp(71) or
      HtmlSpecialKeyComp(109) or
      HtmlSpecialKeyComp(147) or
      HtmlSpecialKeyComp(148) or
      HtmlSpecialKeyComp(190) or
      HtmlSpecialKeyComp(191) or
      HtmlSpecialKeyComp(204) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc42: Boolean;
begin
  if  HtmlSpecialKeyComp(32) or
      HtmlSpecialKeyComp(42) or
      HtmlSpecialKeyComp(43) or
      HtmlSpecialKeyComp(47) or
      HtmlSpecialKeyComp(48) or
      HtmlSpecialKeyComp(87) or
      HtmlSpecialKeyComp(88) or
      HtmlSpecialKeyComp(206) or
      HtmlSpecialKeyComp(215) or
      HtmlSpecialKeyComp(216) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc43: Boolean;
begin
  if  HtmlSpecialKeyComp(94) or
      HtmlSpecialKeyComp(115) or
      HtmlSpecialKeyComp(153) or
      HtmlSpecialKeyComp(192) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc44: Boolean;
begin
  if  HtmlSpecialKeyComp(177) or
      HtmlSpecialKeyComp(178) or
      HtmlSpecialKeyComp(246) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc45: Boolean;
begin
  if  HtmlSpecialKeyComp(81) or
      HtmlSpecialKeyComp(82) or
      HtmlSpecialKeyComp(95) or
      HtmlSpecialKeyComp(96) or
      HtmlSpecialKeyComp(101) or
      HtmlSpecialKeyComp(102) or
      HtmlSpecialKeyComp(120) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc46: Boolean;
begin
  if  HtmlSpecialKeyComp(49) or
      HtmlSpecialKeyComp(128) or
      HtmlSpecialKeyComp(235) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc47: Boolean;
begin
  if  HtmlSpecialKeyComp(21) or
      HtmlSpecialKeyComp(22) or
      HtmlSpecialKeyComp(27) or
      HtmlSpecialKeyComp(170) or
      HtmlSpecialKeyComp(185) or
      HtmlSpecialKeyComp(199) or
      HtmlSpecialKeyComp(207) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc48: Boolean;
begin
  if  HtmlSpecialKeyComp(140) or
      HtmlSpecialKeyComp(141) or
      HtmlSpecialKeyComp(142) or
      HtmlSpecialKeyComp(143) or
      HtmlSpecialKeyComp(226) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc49: Boolean;
begin
  if  HtmlSpecialKeyComp(16) or
      HtmlSpecialKeyComp(17) or
      HtmlSpecialKeyComp(107) or
      HtmlSpecialKeyComp(108) or
      HtmlSpecialKeyComp(131) or
      HtmlSpecialKeyComp(201) or
      HtmlSpecialKeyComp(202) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc50: Boolean;
begin
  if  HtmlSpecialKeyComp(4) or
      HtmlSpecialKeyComp(154) or
      HtmlSpecialKeyComp(224) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc51: Boolean;
begin
  if  HtmlSpecialKeyComp(0) or
      HtmlSpecialKeyComp(1) or
      HtmlSpecialKeyComp(15) or
      HtmlSpecialKeyComp(19) or
      HtmlSpecialKeyComp(20) or
      HtmlSpecialKeyComp(67) or
      HtmlSpecialKeyComp(68) or
      HtmlSpecialKeyComp(98) or
      HtmlSpecialKeyComp(127) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc52: Boolean;
begin
  if  HtmlSpecialKeyComp(93) or
      HtmlSpecialKeyComp(200) or
      HtmlSpecialKeyComp(249) or
      HtmlSpecialKeyComp(250) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc53: Boolean;
begin
  if  HtmlSpecialKeyComp(50) or
      HtmlSpecialKeyComp(58) or
      HtmlSpecialKeyComp(89) or
      HtmlSpecialKeyComp(114) or
      HtmlSpecialKeyComp(175) or
      HtmlSpecialKeyComp(208) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc54: Boolean;
begin
  if  HtmlSpecialKeyComp(7) or
      HtmlSpecialKeyComp(8) or
      HtmlSpecialKeyComp(59) or
      HtmlSpecialKeyComp(218) or
      HtmlSpecialKeyComp(219) or
      HtmlSpecialKeyComp(231) or
      HtmlSpecialKeyComp(232) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc55: Boolean;
begin
  if  HtmlSpecialKeyComp(51) or
      HtmlSpecialKeyComp(52) or
      HtmlSpecialKeyComp(99) or
      HtmlSpecialKeyComp(100) or
      HtmlSpecialKeyComp(146) or
      HtmlSpecialKeyComp(163) or
      HtmlSpecialKeyComp(165) or
      HtmlSpecialKeyComp(183) or
      HtmlSpecialKeyComp(184) or
      HtmlSpecialKeyComp(203) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc56: Boolean;
begin
  if  HtmlSpecialKeyComp(76) or
      HtmlSpecialKeyComp(133) or
      HtmlSpecialKeyComp(209) or
      HtmlSpecialKeyComp(210) or
      HtmlSpecialKeyComp(211) or
      HtmlSpecialKeyComp(212) or
      HtmlSpecialKeyComp(217) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc57: Boolean;
begin
  if  HtmlSpecialKeyComp(36) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc58: Boolean;
begin
  if  HtmlSpecialKeyComp(39) or
      HtmlSpecialKeyComp(55) or
      HtmlSpecialKeyComp(56) or
      HtmlSpecialKeyComp(121) or
      HtmlSpecialKeyComp(198) or
      HtmlSpecialKeyComp(229) or
      HtmlSpecialKeyComp(230) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc59: Boolean;
begin
  if  HtmlSpecialKeyComp(23) or
      HtmlSpecialKeyComp(38) or
      HtmlSpecialKeyComp(69) or
      HtmlSpecialKeyComp(85) or
      HtmlSpecialKeyComp(86) or
      HtmlSpecialKeyComp(251) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc61: Boolean;
begin
  if  HtmlSpecialKeyComp(160) or
      HtmlSpecialKeyComp(161) or
      HtmlSpecialKeyComp(173) or
      HtmlSpecialKeyComp(174) or
      HtmlSpecialKeyComp(213) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc62: Boolean;
begin
  if  HtmlSpecialKeyComp(84) or
      HtmlSpecialKeyComp(90) or
      HtmlSpecialKeyComp(91) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc63: Boolean;
begin
  if  HtmlSpecialKeyComp(26) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc64: Boolean;
begin
  if  HtmlSpecialKeyComp(72) or
      HtmlSpecialKeyComp(134) or
      HtmlSpecialKeyComp(135) or
      HtmlSpecialKeyComp(205) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc65: Boolean;
begin
  if  HtmlSpecialKeyComp(122) or
      HtmlSpecialKeyComp(138) or
      HtmlSpecialKeyComp(139) or
      HtmlSpecialKeyComp(157) or
      HtmlSpecialKeyComp(158) or
      HtmlSpecialKeyComp(176) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc66: Boolean;
begin
  if  HtmlSpecialKeyComp(106) or
      HtmlSpecialKeyComp(225) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc67: Boolean;
begin
  if  HtmlSpecialKeyComp(239) or
      HtmlSpecialKeyComp(240) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc68: Boolean;
begin
  if  HtmlSpecialKeyComp(144) or
      HtmlSpecialKeyComp(145) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc69: Boolean;
begin
  if  HtmlSpecialKeyComp(110) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc70: Boolean;
begin
  if  HtmlSpecialKeyComp(172) or
      HtmlSpecialKeyComp(196) or
      HtmlSpecialKeyComp(197) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc71: Boolean;
begin
  if  HtmlSpecialKeyComp(83) or
      HtmlSpecialKeyComp(227) or
      HtmlSpecialKeyComp(228) or
      HtmlSpecialKeyComp(247) or
      HtmlSpecialKeyComp(248) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc72: Boolean;
begin
  if  HtmlSpecialKeyComp(132) or
      HtmlSpecialKeyComp(182) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc73: Boolean;
begin
  if  HtmlSpecialKeyComp(164) or
      HtmlSpecialKeyComp(179) or
      HtmlSpecialKeyComp(214) or
      HtmlSpecialKeyComp(236) or
      HtmlSpecialKeyComp(252) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc74: Boolean;
begin
  if  HtmlSpecialKeyComp(18) or
      HtmlSpecialKeyComp(62) or
      HtmlSpecialKeyComp(155) or
      HtmlSpecialKeyComp(156) or
      HtmlSpecialKeyComp(195) or
      HtmlSpecialKeyComp(233) or
      HtmlSpecialKeyComp(234) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc75: Boolean;
begin
  if  HtmlSpecialKeyComp(186) or
      HtmlSpecialKeyComp(222) or
      HtmlSpecialKeyComp(223) or
      HtmlSpecialKeyComp(244) or
      HtmlSpecialKeyComp(245) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc76: Boolean;
begin
  if  HtmlSpecialKeyComp(123) or
      HtmlSpecialKeyComp(241) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc77: Boolean;
begin
  if  HtmlSpecialKeyComp(70) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc78: Boolean;
begin
  if  HtmlSpecialKeyComp(112) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc79: Boolean;
begin
  if  HtmlSpecialKeyComp(41) or
      HtmlSpecialKeyComp(57) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc81: Boolean;
begin
  if  HtmlSpecialKeyComp(9) or
      HtmlSpecialKeyComp(159) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc83: Boolean;
begin
  if  HtmlSpecialKeyComp(151) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc84: Boolean;
begin
  if  HtmlSpecialKeyComp(117) or
      HtmlSpecialKeyComp(189) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc85: Boolean;
begin
  if  HtmlSpecialKeyComp(116) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc86: Boolean;
begin
  if  HtmlSpecialKeyComp(221) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc87: Boolean;
begin
  if  HtmlSpecialKeyComp(149) or
      HtmlSpecialKeyComp(150) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc90: Boolean;
begin
  if  HtmlSpecialKeyComp(60) or
      HtmlSpecialKeyComp(61) or
      HtmlSpecialKeyComp(113) or
      HtmlSpecialKeyComp(194) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc91: Boolean;
begin
  if  HtmlSpecialKeyComp(97) or
      HtmlSpecialKeyComp(193) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc95: Boolean;
begin
  if  HtmlSpecialKeyComp(171) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc106: Boolean;
begin
  if  HtmlSpecialKeyComp(237) or
      HtmlSpecialKeyComp(238) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc111: Boolean;
begin
  if  HtmlSpecialKeyComp(220) then
    Result := True
  else
    Result := False;
end;

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
  FCssPropIdentFuncTable[29]:=CssPropFunc29;
  FCssPropIdentFuncTable[39]:=CssPropFunc39;
  FCssPropIdentFuncTable[40]:=CssPropFunc40;
  FCssPropIdentFuncTable[43]:=CssPropFunc43;
  FCssPropIdentFuncTable[51]:=CssPropFunc51;
  FCssPropIdentFuncTable[52]:=CssPropFunc52;
  FCssPropIdentFuncTable[54]:=CssPropFunc54;
  FCssPropIdentFuncTable[55]:=CssPropFunc55;
  FCssPropIdentFuncTable[56]:=CssPropFunc56;
  FCssPropIdentFuncTable[57]:=CssPropFunc57;
  FCssPropIdentFuncTable[60]:=CssPropFunc60;
  FCssPropIdentFuncTable[61]:=CssPropFunc61;
  FCssPropIdentFuncTable[62]:=CssPropFunc62;
  FCssPropIdentFuncTable[63]:=CssPropFunc63;
  FCssPropIdentFuncTable[64]:=CssPropFunc64;
  FCssPropIdentFuncTable[74]:=CssPropFunc74;
  FCssPropIdentFuncTable[76]:=CssPropFunc76;
  FCssPropIdentFuncTable[78]:=CssPropFunc78;
  FCssPropIdentFuncTable[79]:=CssPropFunc79;
  FCssPropIdentFuncTable[81]:=CssPropFunc81;
  FCssPropIdentFuncTable[82]:=CssPropFunc82;
  FCssPropIdentFuncTable[83]:=CssPropFunc83;
  FCssPropIdentFuncTable[85]:=CssPropFunc85;
  FCssPropIdentFuncTable[86]:=CssPropFunc86;
  FCssPropIdentFuncTable[87]:=CssPropFunc87;
  FCssPropIdentFuncTable[88]:=CssPropFunc88;
  FCssPropIdentFuncTable[90]:=CssPropFunc90;
  FCssPropIdentFuncTable[91]:=CssPropFunc91;
  FCssPropIdentFuncTable[93]:=CssPropFunc93;
  FCssPropIdentFuncTable[94]:=CssPropFunc94;
  FCssPropIdentFuncTable[95]:=CssPropFunc95;
  FCssPropIdentFuncTable[96]:=CssPropFunc96;
  FCssPropIdentFuncTable[97]:=CssPropFunc97;
  FCssPropIdentFuncTable[98]:=CssPropFunc98;
  FCssPropIdentFuncTable[100]:=CssPropFunc100;
  FCssPropIdentFuncTable[101]:=CssPropFunc101;
  FCssPropIdentFuncTable[102]:=CssPropFunc102;
  FCssPropIdentFuncTable[103]:=CssPropFunc103;
  FCssPropIdentFuncTable[105]:=CssPropFunc105;
  FCssPropIdentFuncTable[106]:=CssPropFunc106;
  FCssPropIdentFuncTable[107]:=CssPropFunc107;
  FCssPropIdentFuncTable[108]:=CssPropFunc108;
  FCssPropIdentFuncTable[110]:=CssPropFunc110;
  FCssPropIdentFuncTable[111]:=CssPropFunc111;
  FCssPropIdentFuncTable[112]:=CssPropFunc112;
  FCssPropIdentFuncTable[114]:=CssPropFunc114;
  FCssPropIdentFuncTable[115]:=CssPropFunc115;
  FCssPropIdentFuncTable[116]:=CssPropFunc116;
  FCssPropIdentFuncTable[117]:=CssPropFunc117;
  FCssPropIdentFuncTable[121]:=CssPropFunc121;
  FCssPropIdentFuncTable[122]:=CssPropFunc122;
  FCssPropIdentFuncTable[124]:=CssPropFunc124;
  FCssPropIdentFuncTable[126]:=CssPropFunc126;
  FCssPropIdentFuncTable[128]:=CssPropFunc128;
  FCssPropIdentFuncTable[130]:=CssPropFunc130;
  FCssPropIdentFuncTable[131]:=CssPropFunc131;
  FCssPropIdentFuncTable[136]:=CssPropFunc136;
  FCssPropIdentFuncTable[138]:=CssPropFunc138;
  FCssPropIdentFuncTable[139]:=CssPropFunc139;
  FCssPropIdentFuncTable[140]:=CssPropFunc140;
  FCssPropIdentFuncTable[141]:=CssPropFunc141;
  FCssPropIdentFuncTable[142]:=CssPropFunc142;
  FCssPropIdentFuncTable[144]:=CssPropFunc144;
  FCssPropIdentFuncTable[148]:=CssPropFunc148;
  FCssPropIdentFuncTable[149]:=CssPropFunc149;
  FCssPropIdentFuncTable[150]:=CssPropFunc150;
  FCssPropIdentFuncTable[154]:=CssPropFunc154;
  FCssPropIdentFuncTable[156]:=CssPropFunc156;
  FCssPropIdentFuncTable[158]:=CssPropFunc158;
  FCssPropIdentFuncTable[167]:=CssPropFunc167;
  FCssPropIdentFuncTable[169]:=CssPropFunc169;
  FCssPropIdentFuncTable[172]:=CssPropFunc172;
  FCssPropIdentFuncTable[173]:=CssPropFunc173;
  FCssPropIdentFuncTable[174]:=CssPropFunc174;
  FCssPropIdentFuncTable[178]:=CssPropFunc178;
  FCssPropIdentFuncTable[182]:=CssPropFunc182;
  FCssPropIdentFuncTable[187]:=CssPropFunc187;
  FCssPropIdentFuncTable[190]:=CssPropFunc190;
  FCssPropIdentFuncTable[194]:=CssPropFunc194;
  FCssPropIdentFuncTable[220]:=CssPropFunc220;

  pF := PSynWebIdentFuncTableFunc(@FCssValIdentFuncTable);
  for I := Low(FCssValIdentFuncTable) to High(FCssValIdentFuncTable) do
  begin
    pF^ := CssValUndef;
    Inc(pF);
  end;
  FCssValIdentFuncTable[26]:=CssValFunc26;
  FCssValIdentFuncTable[27]:=CssValFunc27;
  FCssValIdentFuncTable[29]:=CssValFunc29;
  FCssValIdentFuncTable[31]:=CssValFunc31;
  FCssValIdentFuncTable[32]:=CssValFunc32;
  FCssValIdentFuncTable[33]:=CssValFunc33;
  FCssValIdentFuncTable[35]:=CssValFunc35;
  FCssValIdentFuncTable[36]:=CssValFunc36;
  FCssValIdentFuncTable[37]:=CssValFunc37;
  FCssValIdentFuncTable[38]:=CssValFunc38;
  FCssValIdentFuncTable[39]:=CssValFunc39;
  FCssValIdentFuncTable[40]:=CssValFunc40;
  FCssValIdentFuncTable[41]:=CssValFunc41;
  FCssValIdentFuncTable[42]:=CssValFunc42;
  FCssValIdentFuncTable[43]:=CssValFunc43;
  FCssValIdentFuncTable[44]:=CssValFunc44;
  FCssValIdentFuncTable[45]:=CssValFunc45;
  FCssValIdentFuncTable[46]:=CssValFunc46;
  FCssValIdentFuncTable[47]:=CssValFunc47;
  FCssValIdentFuncTable[48]:=CssValFunc48;
  FCssValIdentFuncTable[49]:=CssValFunc49;
  FCssValIdentFuncTable[50]:=CssValFunc50;
  FCssValIdentFuncTable[51]:=CssValFunc51;
  FCssValIdentFuncTable[52]:=CssValFunc52;
  FCssValIdentFuncTable[53]:=CssValFunc53;
  FCssValIdentFuncTable[54]:=CssValFunc54;
  FCssValIdentFuncTable[55]:=CssValFunc55;
  FCssValIdentFuncTable[56]:=CssValFunc56;
  FCssValIdentFuncTable[57]:=CssValFunc57;
  FCssValIdentFuncTable[59]:=CssValFunc59;
  FCssValIdentFuncTable[60]:=CssValFunc60;
  FCssValIdentFuncTable[61]:=CssValFunc61;
  FCssValIdentFuncTable[62]:=CssValFunc62;
  FCssValIdentFuncTable[63]:=CssValFunc63;
  FCssValIdentFuncTable[65]:=CssValFunc65;
  FCssValIdentFuncTable[67]:=CssValFunc67;
  FCssValIdentFuncTable[68]:=CssValFunc68;
  FCssValIdentFuncTable[69]:=CssValFunc69;
  FCssValIdentFuncTable[70]:=CssValFunc70;
  FCssValIdentFuncTable[71]:=CssValFunc71;
  FCssValIdentFuncTable[72]:=CssValFunc72;
  FCssValIdentFuncTable[73]:=CssValFunc73;
  FCssValIdentFuncTable[74]:=CssValFunc74;
  FCssValIdentFuncTable[75]:=CssValFunc75;
  FCssValIdentFuncTable[76]:=CssValFunc76;
  FCssValIdentFuncTable[77]:=CssValFunc77;
  FCssValIdentFuncTable[78]:=CssValFunc78;
  FCssValIdentFuncTable[79]:=CssValFunc79;
  FCssValIdentFuncTable[80]:=CssValFunc80;
  FCssValIdentFuncTable[81]:=CssValFunc81;
  FCssValIdentFuncTable[82]:=CssValFunc82;
  FCssValIdentFuncTable[83]:=CssValFunc83;
  FCssValIdentFuncTable[84]:=CssValFunc84;
  FCssValIdentFuncTable[85]:=CssValFunc85;
  FCssValIdentFuncTable[86]:=CssValFunc86;
  FCssValIdentFuncTable[87]:=CssValFunc87;
  FCssValIdentFuncTable[88]:=CssValFunc88;
  FCssValIdentFuncTable[89]:=CssValFunc89;
  FCssValIdentFuncTable[91]:=CssValFunc91;
  FCssValIdentFuncTable[92]:=CssValFunc92;
  FCssValIdentFuncTable[93]:=CssValFunc93;
  FCssValIdentFuncTable[95]:=CssValFunc95;
  FCssValIdentFuncTable[96]:=CssValFunc96;
  FCssValIdentFuncTable[97]:=CssValFunc97;
  FCssValIdentFuncTable[99]:=CssValFunc99;
  FCssValIdentFuncTable[100]:=CssValFunc100;
  FCssValIdentFuncTable[101]:=CssValFunc101;
  FCssValIdentFuncTable[102]:=CssValFunc102;
  FCssValIdentFuncTable[104]:=CssValFunc104;
  FCssValIdentFuncTable[105]:=CssValFunc105;
  FCssValIdentFuncTable[108]:=CssValFunc108;
  FCssValIdentFuncTable[109]:=CssValFunc109;
  FCssValIdentFuncTable[110]:=CssValFunc110;
  FCssValIdentFuncTable[113]:=CssValFunc113;
  FCssValIdentFuncTable[115]:=CssValFunc115;
  FCssValIdentFuncTable[116]:=CssValFunc116;
  FCssValIdentFuncTable[117]:=CssValFunc117;
  FCssValIdentFuncTable[118]:=CssValFunc118;
  FCssValIdentFuncTable[119]:=CssValFunc119;
  FCssValIdentFuncTable[120]:=CssValFunc120;
  FCssValIdentFuncTable[123]:=CssValFunc123;
  FCssValIdentFuncTable[125]:=CssValFunc125;
  FCssValIdentFuncTable[127]:=CssValFunc127;
  FCssValIdentFuncTable[135]:=CssValFunc135;
  FCssValIdentFuncTable[146]:=CssValFunc146;
  FCssValIdentFuncTable[151]:=CssValFunc151;
  FCssValIdentFuncTable[157]:=CssValFunc157;
  FCssValIdentFuncTable[158]:=CssValFunc158;

  pF2 := PSynWebIdent2FuncTableFunc(@FCssSpecialIdentFuncTable);
  for I := Low(FCssSpecialIdentFuncTable) to High(FCssSpecialIdentFuncTable) do
  begin
    pF2^ := CssSpecialUndef;
    Inc(pF2);
  end;
  FCssSpecialIdentFuncTable[16]:=CssSpecialFunc16;
  FCssSpecialIdentFuncTable[18]:=CssSpecialFunc18;
  FCssSpecialIdentFuncTable[19]:=CssSpecialFunc19;
  FCssSpecialIdentFuncTable[23]:=CssSpecialFunc23;
  FCssSpecialIdentFuncTable[25]:=CssSpecialFunc25;
  FCssSpecialIdentFuncTable[26]:=CssSpecialFunc26;
  FCssSpecialIdentFuncTable[29]:=CssSpecialFunc29;
  FCssSpecialIdentFuncTable[30]:=CssSpecialFunc30;
  FCssSpecialIdentFuncTable[32]:=CssSpecialFunc32;
  FCssSpecialIdentFuncTable[34]:=CssSpecialFunc34;
  FCssSpecialIdentFuncTable[36]:=CssSpecialFunc36;
  FCssSpecialIdentFuncTable[40]:=CssSpecialFunc40;
  FCssSpecialIdentFuncTable[42]:=CssSpecialFunc42;
  FCssSpecialIdentFuncTable[43]:=CssSpecialFunc43;
  FCssSpecialIdentFuncTable[45]:=CssSpecialFunc45;
  FCssSpecialIdentFuncTable[46]:=CssSpecialFunc46;
  FCssSpecialIdentFuncTable[50]:=CssSpecialFunc50;
  FCssSpecialIdentFuncTable[51]:=CssSpecialFunc51;
  FCssSpecialIdentFuncTable[56]:=CssSpecialFunc56;
  FCssSpecialIdentFuncTable[57]:=CssSpecialFunc57;
  FCssSpecialIdentFuncTable[59]:=CssSpecialFunc59;
  FCssSpecialIdentFuncTable[60]:=CssSpecialFunc60;
  FCssSpecialIdentFuncTable[62]:=CssSpecialFunc62;
  FCssSpecialIdentFuncTable[64]:=CssSpecialFunc64;
  FCssSpecialIdentFuncTable[65]:=CssSpecialFunc65;
  FCssSpecialIdentFuncTable[68]:=CssSpecialFunc68;
  FCssSpecialIdentFuncTable[72]:=CssSpecialFunc72;
  FCssSpecialIdentFuncTable[74]:=CssSpecialFunc74;
  FCssSpecialIdentFuncTable[77]:=CssSpecialFunc77;
  FCssSpecialIdentFuncTable[82]:=CssSpecialFunc82;
  FCssSpecialIdentFuncTable[88]:=CssSpecialFunc88;
  FCssSpecialIdentFuncTable[89]:=CssSpecialFunc89;
  FCssSpecialIdentFuncTable[91]:=CssSpecialFunc91;
  FCssSpecialIdentFuncTable[93]:=CssSpecialFunc93;
  FCssSpecialIdentFuncTable[125]:=CssSpecialFunc125;
  FCssSpecialIdentFuncTable[126]:=CssSpecialFunc126;
  FCssSpecialIdentFuncTable[133]:=CssSpecialFunc133;
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

function TSynWebEngine.CssPropUndef: TSynWebTokenKind;
begin
  Result:=stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc29: TSynWebTokenKind;
begin
  if  CssPropKeyComp(37) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc39: TSynWebTokenKind;
begin
  if  CssPropKeyComp(31) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc40: TSynWebTokenKind;
begin
  if  CssPropKeyComp(32) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc43: TSynWebTokenKind;
begin
  if  CssPropKeyComp(53) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc51: TSynWebTokenKind;
begin
  if  CssPropKeyComp(104) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc52: TSynWebTokenKind;
begin
  if  CssPropKeyComp(93) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc54: TSynWebTokenKind;
begin
  if  CssPropKeyComp(45) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc55: TSynWebTokenKind;
begin
  if  CssPropKeyComp(46) or
      CssPropKeyComp(75) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc56: TSynWebTokenKind;
begin
  if  CssPropKeyComp(86) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc57: TSynWebTokenKind;
begin
  if  CssPropKeyComp(52) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc60: TSynWebTokenKind;
begin
  if  CssPropKeyComp(38) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc61: TSynWebTokenKind;
begin
  if  CssPropKeyComp(39) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc62: TSynWebTokenKind;
begin
  if  CssPropKeyComp(7) or
      CssPropKeyComp(60) or
      CssPropKeyComp(83) or
      CssPropKeyComp(92) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc63: TSynWebTokenKind;
begin
  if  CssPropKeyComp(33) or
      CssPropKeyComp(114) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc64: TSynWebTokenKind;
begin
  if  CssPropKeyComp(112) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc74: TSynWebTokenKind;
begin
  if  CssPropKeyComp(67) or
      CssPropKeyComp(94) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc76: TSynWebTokenKind;
begin
  if  CssPropKeyComp(65) or
      CssPropKeyComp(105) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc78: TSynWebTokenKind;
begin
  if  CssPropKeyComp(55) or
      CssPropKeyComp(80) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc79: TSynWebTokenKind;
begin
  if  CssPropKeyComp(77) or
      CssPropKeyComp(81) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc81: TSynWebTokenKind;
begin
  if  CssPropKeyComp(68) or
      CssPropKeyComp(97) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc82: TSynWebTokenKind;
begin
  if  CssPropKeyComp(87) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc83: TSynWebTokenKind;
begin
  if  CssPropKeyComp(66) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc85: TSynWebTokenKind;
begin
  if  CssPropKeyComp(29) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc86: TSynWebTokenKind;
begin
  if  CssPropKeyComp(14) or
      CssPropKeyComp(42) or
      CssPropKeyComp(62) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc87: TSynWebTokenKind;
begin
  if  CssPropKeyComp(79) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc88: TSynWebTokenKind;
begin
  if  CssPropKeyComp(82) or
      CssPropKeyComp(109) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc90: TSynWebTokenKind;
begin
  if  CssPropKeyComp(110) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc91: TSynWebTokenKind;
begin
  if  CssPropKeyComp(34) or
      CssPropKeyComp(69) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc93: TSynWebTokenKind;
begin
  if  CssPropKeyComp(84) or
      CssPropKeyComp(100) or
      CssPropKeyComp(111) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc94: TSynWebTokenKind;
begin
  if  CssPropKeyComp(24) or
      CssPropKeyComp(40) or
      CssPropKeyComp(64) or
      CssPropKeyComp(85) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc95: TSynWebTokenKind;
begin
  if  CssPropKeyComp(48) or
      CssPropKeyComp(91) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc96: TSynWebTokenKind;
begin
  if  CssPropKeyComp(1) or
      CssPropKeyComp(30) or
      CssPropKeyComp(70) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc97: TSynWebTokenKind;
begin
  if  CssPropKeyComp(41) or
      CssPropKeyComp(90) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc98: TSynWebTokenKind;
begin
  if  CssPropKeyComp(0) or
      CssPropKeyComp(78) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc100: TSynWebTokenKind;
begin
  if  CssPropKeyComp(98) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc101: TSynWebTokenKind;
begin
  if  CssPropKeyComp(108) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc102: TSynWebTokenKind;
begin
  if  CssPropKeyComp(47) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc103: TSynWebTokenKind;
begin
  if  CssPropKeyComp(43) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc105: TSynWebTokenKind;
begin
  if  CssPropKeyComp(18) or
      CssPropKeyComp(63) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc106: TSynWebTokenKind;
begin
  if  CssPropKeyComp(13) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc107: TSynWebTokenKind;
begin
  if  CssPropKeyComp(28) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc108: TSynWebTokenKind;
begin
  if  CssPropKeyComp(51) or
      CssPropKeyComp(88) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc110: TSynWebTokenKind;
begin
  if  CssPropKeyComp(113) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc111: TSynWebTokenKind;
begin
  if  CssPropKeyComp(44) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc112: TSynWebTokenKind;
begin
  if  CssPropKeyComp(4) or
      CssPropKeyComp(22) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc114: TSynWebTokenKind;
begin
  if  CssPropKeyComp(106) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc115: TSynWebTokenKind;
begin
  if  CssPropKeyComp(99) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc116: TSynWebTokenKind;
begin
  if  CssPropKeyComp(74) or
      CssPropKeyComp(102) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc117: TSynWebTokenKind;
begin
  if  CssPropKeyComp(49) or
      CssPropKeyComp(89) or
      CssPropKeyComp(95) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc121: TSynWebTokenKind;
begin
  if  CssPropKeyComp(50) or
      CssPropKeyComp(76) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc122: TSynWebTokenKind;
begin
  if  CssPropKeyComp(56) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc124: TSynWebTokenKind;
begin
  if  CssPropKeyComp(23) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc126: TSynWebTokenKind;
begin
  if  CssPropKeyComp(12) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc128: TSynWebTokenKind;
begin
  if  CssPropKeyComp(8) or
      CssPropKeyComp(61) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc130: TSynWebTokenKind;
begin
  if  CssPropKeyComp(15) or
      CssPropKeyComp(54) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc131: TSynWebTokenKind;
begin
  if  CssPropKeyComp(17) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc136: TSynWebTokenKind;
begin
  if  CssPropKeyComp(107) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc138: TSynWebTokenKind;
begin
  if  CssPropKeyComp(25) or
      CssPropKeyComp(57) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc139: TSynWebTokenKind;
begin
  if  CssPropKeyComp(27) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc140: TSynWebTokenKind;
begin
  if  CssPropKeyComp(3) or
      CssPropKeyComp(71) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc141: TSynWebTokenKind;
begin
  if  CssPropKeyComp(73) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc142: TSynWebTokenKind;
begin
  if  CssPropKeyComp(6) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc144: TSynWebTokenKind;
begin
  if  CssPropKeyComp(36) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc148: TSynWebTokenKind;
begin
  if  CssPropKeyComp(16) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc149: TSynWebTokenKind;
begin
  if  CssPropKeyComp(19) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc150: TSynWebTokenKind;
begin
  if  CssPropKeyComp(21) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc154: TSynWebTokenKind;
begin
  if  CssPropKeyComp(101) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc156: TSynWebTokenKind;
begin
  if  CssPropKeyComp(26) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc158: TSynWebTokenKind;
begin
  if  CssPropKeyComp(72) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc167: TSynWebTokenKind;
begin
  if  CssPropKeyComp(20) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc169: TSynWebTokenKind;
begin
  if  CssPropKeyComp(59) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc172: TSynWebTokenKind;
begin
  if  CssPropKeyComp(9) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc173: TSynWebTokenKind;
begin
  if  CssPropKeyComp(11) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc174: TSynWebTokenKind;
begin
  if  CssPropKeyComp(103) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc178: TSynWebTokenKind;
begin
  if  CssPropKeyComp(35) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc182: TSynWebTokenKind;
begin
  if  CssPropKeyComp(2) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc187: TSynWebTokenKind;
begin
  if  CssPropKeyComp(96) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc190: TSynWebTokenKind;
begin
  if  CssPropKeyComp(10) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc194: TSynWebTokenKind;
begin
  if  CssPropKeyComp(5) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc220: TSynWebTokenKind;
begin
  if  CssPropKeyComp(58) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

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

function TSynWebEngine.CssValUndef: TSynWebTokenKind;
begin
  Result:=stkCssValUndef;
end;

function TSynWebEngine.CssValFunc26: TSynWebTokenKind;
begin
  if  CssValKeyComp(59) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc27: TSynWebTokenKind;
begin
  if  CssValKeyComp(28) or
      CssValKeyComp(125) or
      CssValKeyComp(130) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc29: TSynWebTokenKind;
begin
  if  CssValKeyComp(12) or
      CssValKeyComp(43) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc31: TSynWebTokenKind;
begin
  if  CssValKeyComp(91) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc32: TSynWebTokenKind;
begin
  if  CssValKeyComp(60) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc33: TSynWebTokenKind;
begin
  if  CssValKeyComp(16) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc35: TSynWebTokenKind;
begin
  if  CssValKeyComp(40) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc36: TSynWebTokenKind;
begin
  if  CssValKeyComp(25) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc37: TSynWebTokenKind;
begin
  if  CssValKeyComp(112) or
      CssValKeyComp(189) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc38: TSynWebTokenKind;
begin
  if  CssValKeyComp(170) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc39: TSynWebTokenKind;
begin
  if  CssValKeyComp(79) or
      CssValKeyComp(119) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc40: TSynWebTokenKind;
begin
  if  CssValKeyComp(3) or
      CssValKeyComp(15) or
      CssValKeyComp(161) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc41: TSynWebTokenKind;
begin
  if  CssValKeyComp(35) or
      CssValKeyComp(57) or
      CssValKeyComp(62) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc42: TSynWebTokenKind;
begin
  if  CssValKeyComp(9) or
      CssValKeyComp(50) or
      CssValKeyComp(158) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc43: TSynWebTokenKind;
begin
  if  CssValKeyComp(14) or
      CssValKeyComp(72) or
      CssValKeyComp(74) or
      CssValKeyComp(131) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc44: TSynWebTokenKind;
begin
  if  CssValKeyComp(58) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc45: TSynWebTokenKind;
begin
  if  CssValKeyComp(0) or
      CssValKeyComp(18) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc46: TSynWebTokenKind;
begin
  if  CssValKeyComp(48) or
      CssValKeyComp(97) or
      CssValKeyComp(124) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc47: TSynWebTokenKind;
begin
  if  CssValKeyComp(36) or
      CssValKeyComp(96) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc48: TSynWebTokenKind;
begin
  if  CssValKeyComp(13) or
      CssValKeyComp(51) or
      CssValKeyComp(103) or
      CssValKeyComp(190) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc49: TSynWebTokenKind;
begin
  if  CssValKeyComp(46) or
      CssValKeyComp(55) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc50: TSynWebTokenKind;
begin
  if  CssValKeyComp(26) or
      CssValKeyComp(83) or
      CssValKeyComp(90) or
      CssValKeyComp(135) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc51: TSynWebTokenKind;
begin
  if  CssValKeyComp(7) or
      CssValKeyComp(54) or
      CssValKeyComp(174) or
      CssValKeyComp(175) or
      CssValKeyComp(176) or
      CssValKeyComp(183) or
      CssValKeyComp(188) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc52: TSynWebTokenKind;
begin
  if  CssValKeyComp(82) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc53: TSynWebTokenKind;
begin
  if  CssValKeyComp(94) or
      CssValKeyComp(163) or
      CssValKeyComp(185) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc54: TSynWebTokenKind;
begin
  if  CssValKeyComp(70) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc55: TSynWebTokenKind;
begin
  if  CssValKeyComp(61) or
      CssValKeyComp(99) or
      CssValKeyComp(192) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc56: TSynWebTokenKind;
begin
  if  CssValKeyComp(17) or
      CssValKeyComp(77) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc57: TSynWebTokenKind;
begin
  if  CssValKeyComp(6) or
      CssValKeyComp(10) or
      CssValKeyComp(136) or
      CssValKeyComp(141) or
      CssValKeyComp(147) or
      CssValKeyComp(191) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc59: TSynWebTokenKind;
begin
  if  CssValKeyComp(5) or
      CssValKeyComp(42) or
      CssValKeyComp(152) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc60: TSynWebTokenKind;
begin
  if  CssValKeyComp(68) or
      CssValKeyComp(114) or
      CssValKeyComp(120) or
      CssValKeyComp(151) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc61: TSynWebTokenKind;
begin
  if  CssValKeyComp(73) or
      CssValKeyComp(75) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc62: TSynWebTokenKind;
begin
  if  CssValKeyComp(100) or
      CssValKeyComp(132) or
      CssValKeyComp(194) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc63: TSynWebTokenKind;
begin
  if  CssValKeyComp(64) or
      CssValKeyComp(111) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc65: TSynWebTokenKind;
begin
  if  CssValKeyComp(22) or
      CssValKeyComp(93) or
      CssValKeyComp(127) or
      CssValKeyComp(142) or
      CssValKeyComp(186) or
      CssValKeyComp(195) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc67: TSynWebTokenKind;
begin
  if  CssValKeyComp(8) or
      CssValKeyComp(52) or
      CssValKeyComp(67) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc68: TSynWebTokenKind;
begin
  if  CssValKeyComp(39) or
      CssValKeyComp(41) or
      CssValKeyComp(44) or
      CssValKeyComp(47) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc69: TSynWebTokenKind;
begin
  if  CssValKeyComp(38) or
      CssValKeyComp(49) or
      CssValKeyComp(145) or
      CssValKeyComp(171) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc70: TSynWebTokenKind;
begin
  if  CssValKeyComp(128) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc71: TSynWebTokenKind;
begin
  if  CssValKeyComp(129) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc72: TSynWebTokenKind;
begin
  if  CssValKeyComp(156) or
      CssValKeyComp(196) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc73: TSynWebTokenKind;
begin
  if  CssValKeyComp(84) or
      CssValKeyComp(106) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc74: TSynWebTokenKind;
begin
  if  CssValKeyComp(193) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc75: TSynWebTokenKind;
begin
  if  CssValKeyComp(4) or
      CssValKeyComp(105) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc76: TSynWebTokenKind;
begin
  if  CssValKeyComp(53) or
      CssValKeyComp(92) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc77: TSynWebTokenKind;
begin
  if  CssValKeyComp(108) or
      CssValKeyComp(148) or
      CssValKeyComp(168) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc78: TSynWebTokenKind;
begin
  if  CssValKeyComp(21) or
      CssValKeyComp(121) or
      CssValKeyComp(184) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc79: TSynWebTokenKind;
begin
  if  CssValKeyComp(78) or
      CssValKeyComp(138) or
      CssValKeyComp(143) or
      CssValKeyComp(159) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc80: TSynWebTokenKind;
begin
  if  CssValKeyComp(133) or
      CssValKeyComp(150) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc81: TSynWebTokenKind;
begin
  if  CssValKeyComp(2) or
      CssValKeyComp(110) or
      CssValKeyComp(154) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc82: TSynWebTokenKind;
begin
  if  CssValKeyComp(56) or
      CssValKeyComp(101) or
      CssValKeyComp(155) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc83: TSynWebTokenKind;
begin
  if  CssValKeyComp(29) or
      CssValKeyComp(63) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc84: TSynWebTokenKind;
begin
  if  CssValKeyComp(66) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc85: TSynWebTokenKind;
begin
  if  CssValKeyComp(19) or
      CssValKeyComp(139) or
      CssValKeyComp(144) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc86: TSynWebTokenKind;
begin
  if  CssValKeyComp(45) or
      CssValKeyComp(187) or
      CssValKeyComp(197) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc87: TSynWebTokenKind;
begin
  if  CssValKeyComp(65) or
      CssValKeyComp(107) or
      CssValKeyComp(140) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc88: TSynWebTokenKind;
begin
  if  CssValKeyComp(69) or
      CssValKeyComp(81) or
      CssValKeyComp(123) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc89: TSynWebTokenKind;
begin
  if  CssValKeyComp(23) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc91: TSynWebTokenKind;
begin
  if  CssValKeyComp(95) or
      CssValKeyComp(137) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc92: TSynWebTokenKind;
begin
  if  CssValKeyComp(85) or
      CssValKeyComp(126) or
      CssValKeyComp(146) or
      CssValKeyComp(198) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc93: TSynWebTokenKind;
begin
  if  CssValKeyComp(116) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc95: TSynWebTokenKind;
begin
  if  CssValKeyComp(1) or
      CssValKeyComp(179) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc96: TSynWebTokenKind;
begin
  if  CssValKeyComp(31) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc97: TSynWebTokenKind;
begin
  if  CssValKeyComp(34) or
      CssValKeyComp(118) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc99: TSynWebTokenKind;
begin
  if  CssValKeyComp(162) or
      CssValKeyComp(164) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc100: TSynWebTokenKind;
begin
  if  CssValKeyComp(87) or
      CssValKeyComp(109) or
      CssValKeyComp(115) or
      CssValKeyComp(117) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc101: TSynWebTokenKind;
begin
  if  CssValKeyComp(11) or
      CssValKeyComp(86) or
      CssValKeyComp(98) or
      CssValKeyComp(153) or
      CssValKeyComp(173) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc102: TSynWebTokenKind;
begin
  if  CssValKeyComp(20) or
      CssValKeyComp(157) or
      CssValKeyComp(178) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc104: TSynWebTokenKind;
begin
  if  CssValKeyComp(180) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc105: TSynWebTokenKind;
begin
  if  CssValKeyComp(160) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc108: TSynWebTokenKind;
begin
  if  CssValKeyComp(24) or
      CssValKeyComp(76) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc109: TSynWebTokenKind;
begin
  if  CssValKeyComp(113) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc110: TSynWebTokenKind;
begin
  if  CssValKeyComp(33) or
      CssValKeyComp(71) or
      CssValKeyComp(88) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc113: TSynWebTokenKind;
begin
  if  CssValKeyComp(27) or
      CssValKeyComp(181) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc115: TSynWebTokenKind;
begin
  if  CssValKeyComp(32) or
      CssValKeyComp(89) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc116: TSynWebTokenKind;
begin
  if  CssValKeyComp(149) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc117: TSynWebTokenKind;
begin
  if  CssValKeyComp(122) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc118: TSynWebTokenKind;
begin
  if  CssValKeyComp(80) or
      CssValKeyComp(182) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc119: TSynWebTokenKind;
begin
  if  CssValKeyComp(104) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc120: TSynWebTokenKind;
begin
  if  CssValKeyComp(167) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc123: TSynWebTokenKind;
begin
  if  CssValKeyComp(102) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc125: TSynWebTokenKind;
begin
  if  CssValKeyComp(37) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc127: TSynWebTokenKind;
begin
  if  CssValKeyComp(134) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc135: TSynWebTokenKind;
begin
  if  CssValKeyComp(169) or
      CssValKeyComp(172) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc146: TSynWebTokenKind;
begin
  if  CssValKeyComp(177) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc151: TSynWebTokenKind;
begin
  if  CssValKeyComp(30) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc157: TSynWebTokenKind;
begin
  if  CssValKeyComp(165) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc158: TSynWebTokenKind;
begin
  if  CssValKeyComp(166) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

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

function TSynWebEngine.CssSpecialUndef: Boolean;
begin
  Result:=False;
end;

function TSynWebEngine.CssSpecialFunc16: Boolean;
begin
  if  CssSpecialKeyComp(7) or
      CssSpecialKeyComp(8) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc18: Boolean;
begin
  if  CssSpecialKeyComp(9) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc19: Boolean;
begin
  if  CssSpecialKeyComp(33) or
      CssSpecialKeyComp(40) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc23: Boolean;
begin
  if  CssSpecialKeyComp(24) or
      CssSpecialKeyComp(38) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc25: Boolean;
begin
  if  CssSpecialKeyComp(2) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc26: Boolean;
begin
  if  CssSpecialKeyComp(30) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc29: Boolean;
begin
  if  CssSpecialKeyComp(11) or
      CssSpecialKeyComp(32) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc30: Boolean;
begin
  if  CssSpecialKeyComp(18) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc32: Boolean;
begin
  if  CssSpecialKeyComp(29) or
      CssSpecialKeyComp(31) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc34: Boolean;
begin
  if  CssSpecialKeyComp(21) or
      CssSpecialKeyComp(26) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc36: Boolean;
begin
  if  CssSpecialKeyComp(36) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc40: Boolean;
begin
  if  CssSpecialKeyComp(37) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc42: Boolean;
begin
  if  CssSpecialKeyComp(44) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc43: Boolean;
begin
  if  CssSpecialKeyComp(27) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc45: Boolean;
begin
  if  CssSpecialKeyComp(25) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc46: Boolean;
begin
  if  CssSpecialKeyComp(28) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc50: Boolean;
begin
  if  CssSpecialKeyComp(1) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc51: Boolean;
begin
  if  CssSpecialKeyComp(4) or
      CssSpecialKeyComp(17) or
      CssSpecialKeyComp(45) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc56: Boolean;
begin
  if  CssSpecialKeyComp(19) or
      CssSpecialKeyComp(42) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc57: Boolean;
begin
  if  CssSpecialKeyComp(3) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc59: Boolean;
begin
  if  CssSpecialKeyComp(5) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc60: Boolean;
begin
  if  CssSpecialKeyComp(0) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc62: Boolean;
begin
  if  CssSpecialKeyComp(39) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc64: Boolean;
begin
  if  CssSpecialKeyComp(16) or
      CssSpecialKeyComp(41) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc65: Boolean;
begin
  if  CssSpecialKeyComp(43) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc68: Boolean;
begin
  if  CssSpecialKeyComp(20) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc72: Boolean;
begin
  if  CssSpecialKeyComp(12) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc74: Boolean;
begin
  if  CssSpecialKeyComp(6) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc77: Boolean;
begin
  if  CssSpecialKeyComp(34) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc82: Boolean;
begin
  if  CssSpecialKeyComp(10) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc88: Boolean;
begin
  if  CssSpecialKeyComp(46) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc89: Boolean;
begin
  if  CssSpecialKeyComp(13) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc91: Boolean;
begin
  if  CssSpecialKeyComp(22) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc93: Boolean;
begin
  if  CssSpecialKeyComp(15) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc125: Boolean;
begin
  if  CssSpecialKeyComp(35) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc126: Boolean;
begin
  if  CssSpecialKeyComp(23) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc133: Boolean;
begin
  if  CssSpecialKeyComp(14) then
    Result := True
  else
    Result := False;
end;

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
  FEsIdentFuncTable[15]:=EsKeywordFunc15;
  FEsIdentFuncTable[19]:=EsKeywordFunc19;
  FEsIdentFuncTable[23]:=EsKeywordFunc23;
  FEsIdentFuncTable[28]:=EsKeywordFunc28;
  FEsIdentFuncTable[30]:=EsKeywordFunc30;
  FEsIdentFuncTable[35]:=EsKeywordFunc35;
  FEsIdentFuncTable[37]:=EsKeywordFunc37;
  FEsIdentFuncTable[39]:=EsKeywordFunc39;
  FEsIdentFuncTable[41]:=EsKeywordFunc41;
  FEsIdentFuncTable[42]:=EsKeywordFunc42;
  FEsIdentFuncTable[43]:=EsKeywordFunc43;
  FEsIdentFuncTable[44]:=EsKeywordFunc44;
  FEsIdentFuncTable[48]:=EsKeywordFunc48;
  FEsIdentFuncTable[50]:=EsKeywordFunc50;
  FEsIdentFuncTable[51]:=EsKeywordFunc51;
  FEsIdentFuncTable[52]:=EsKeywordFunc52;
  FEsIdentFuncTable[53]:=EsKeywordFunc53;
  FEsIdentFuncTable[54]:=EsKeywordFunc54;
  FEsIdentFuncTable[56]:=EsKeywordFunc56;
  FEsIdentFuncTable[57]:=EsKeywordFunc57;
  FEsIdentFuncTable[59]:=EsKeywordFunc59;
  FEsIdentFuncTable[60]:=EsKeywordFunc60;
  FEsIdentFuncTable[63]:=EsKeywordFunc63;
  FEsIdentFuncTable[64]:=EsKeywordFunc64;
  FEsIdentFuncTable[69]:=EsKeywordFunc69;
  FEsIdentFuncTable[71]:=EsKeywordFunc71;
  FEsIdentFuncTable[72]:=EsKeywordFunc72;
  FEsIdentFuncTable[79]:=EsKeywordFunc79;
  FEsIdentFuncTable[80]:=EsKeywordFunc80;
  FEsIdentFuncTable[81]:=EsKeywordFunc81;
  FEsIdentFuncTable[82]:=EsKeywordFunc82;
  FEsIdentFuncTable[84]:=EsKeywordFunc84;
  FEsIdentFuncTable[87]:=EsKeywordFunc87;
  FEsIdentFuncTable[91]:=EsKeywordFunc91;
  FEsIdentFuncTable[96]:=EsKeywordFunc96;
  FEsIdentFuncTable[98]:=EsKeywordFunc98;
  FEsIdentFuncTable[101]:=EsKeywordFunc101;
  FEsIdentFuncTable[102]:=EsKeywordFunc102;
  FEsIdentFuncTable[103]:=EsKeywordFunc103;
  FEsIdentFuncTable[106]:=EsKeywordFunc106;
  FEsIdentFuncTable[120]:=EsKeywordFunc120;
  FEsIdentFuncTable[126]:=EsKeywordFunc126;
  FEsIdentFuncTable[160]:=EsKeywordFunc160;
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

function TSynWebEngine.EsKeywordIdent: TSynWebTokenKind;
begin
  Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc15: TSynWebTokenKind;
begin
  if  EsKeywordComp(12) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc19: TSynWebTokenKind;
begin
  if  EsKeywordComp(6) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc23: TSynWebTokenKind;
begin
  if  EsKeywordComp(13) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc28: TSynWebTokenKind;
begin
  if  EsKeywordComp(1) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc30: TSynWebTokenKind;
begin
  if  EsKeywordComp(30) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc35: TSynWebTokenKind;
begin
  if  EsKeywordComp(2) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc37: TSynWebTokenKind;
begin
  if  EsKeywordComp(0) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc39: TSynWebTokenKind;
begin
  if  EsKeywordComp(10) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc41: TSynWebTokenKind;
begin
  if  EsKeywordComp(7) or
      EsKeywordComp(23) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc42: TSynWebTokenKind;
begin
  if  EsKeywordComp(15) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(38) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc43: TSynWebTokenKind;
begin
  if  EsKeywordComp(8) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(43) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc44: TSynWebTokenKind;
begin
  if  EsKeywordComp(47) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc48: TSynWebTokenKind;
begin
  if  EsKeywordComp(45) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc50: TSynWebTokenKind;
begin
  if  EsKeywordComp(24) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc51: TSynWebTokenKind;
begin
  if  EsKeywordComp(5) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc52: TSynWebTokenKind;
begin
  if  EsKeywordComp(29) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc53: TSynWebTokenKind;
begin
  if  EsKeywordComp(35) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc54: TSynWebTokenKind;
begin
  if  EsKeywordComp(31) or
      EsKeywordComp(39) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc56: TSynWebTokenKind;
begin
  if  EsKeywordComp(18) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc57: TSynWebTokenKind;
begin
  if  EsKeywordComp(25) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(40) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc59: TSynWebTokenKind;
begin
  if  EsKeywordComp(34) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc60: TSynWebTokenKind;
begin
  if  EsKeywordComp(26) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc63: TSynWebTokenKind;
begin
  if  EsKeywordComp(21) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(50) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc64: TSynWebTokenKind;
begin
  if  EsKeywordComp(20) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(28) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc69: TSynWebTokenKind;
begin
  if  EsKeywordComp(4) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(33) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc71: TSynWebTokenKind;
begin
  if  EsKeywordComp(32) or
      EsKeywordComp(46) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc72: TSynWebTokenKind;
begin
  if  EsKeywordComp(52) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc79: TSynWebTokenKind;
begin
  if  EsKeywordComp(9) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(53) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc80: TSynWebTokenKind;
begin
  if  EsKeywordComp(51) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc81: TSynWebTokenKind;
begin
  if  EsKeywordComp(44) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc82: TSynWebTokenKind;
begin
  if  EsKeywordComp(17) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc84: TSynWebTokenKind;
begin
  if  EsKeywordComp(19) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(27) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc87: TSynWebTokenKind;
begin
  if  EsKeywordComp(22) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc91: TSynWebTokenKind;
begin
  if  EsKeywordComp(37) or
      EsKeywordComp(42) or
      EsKeywordComp(48) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc96: TSynWebTokenKind;
begin
  if  EsKeywordComp(16) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(57) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc98: TSynWebTokenKind;
begin
  if  EsKeywordComp(36) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc101: TSynWebTokenKind;
begin
  if  EsKeywordComp(3) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc102: TSynWebTokenKind;
begin
  if  EsKeywordComp(11) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc103: TSynWebTokenKind;
begin
  if  EsKeywordComp(55) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc106: TSynWebTokenKind;
begin
  if  EsKeywordComp(14) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(49) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc120: TSynWebTokenKind;
begin
  if  EsKeywordComp(56) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc126: TSynWebTokenKind;
begin
  if  EsKeywordComp(41) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc160: TSynWebTokenKind;
begin
  if  EsKeywordComp(54) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

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
  FPhpIdentFuncTable[14]:=PhpKeywordFunc14;
  FPhpIdentFuncTable[15]:=PhpKeywordFunc15;
  FPhpIdentFuncTable[16]:=PhpKeywordFunc16;
  FPhpIdentFuncTable[17]:=PhpKeywordFunc17;
  FPhpIdentFuncTable[18]:=PhpKeywordFunc18;
  FPhpIdentFuncTable[19]:=PhpKeywordFunc19;
  FPhpIdentFuncTable[20]:=PhpKeywordFunc20;
  FPhpIdentFuncTable[22]:=PhpKeywordFunc22;
  FPhpIdentFuncTable[23]:=PhpKeywordFunc23;
  FPhpIdentFuncTable[24]:=PhpKeywordFunc24;
  FPhpIdentFuncTable[25]:=PhpKeywordFunc25;
  FPhpIdentFuncTable[28]:=PhpKeywordFunc28;
  FPhpIdentFuncTable[29]:=PhpKeywordFunc29;
  FPhpIdentFuncTable[30]:=PhpKeywordFunc30;
  FPhpIdentFuncTable[31]:=PhpKeywordFunc31;
  FPhpIdentFuncTable[32]:=PhpKeywordFunc32;
  FPhpIdentFuncTable[33]:=PhpKeywordFunc33;
  FPhpIdentFuncTable[34]:=PhpKeywordFunc34;
  FPhpIdentFuncTable[35]:=PhpKeywordFunc35;
  FPhpIdentFuncTable[36]:=PhpKeywordFunc36;
  FPhpIdentFuncTable[37]:=PhpKeywordFunc37;
  FPhpIdentFuncTable[38]:=PhpKeywordFunc38;
  FPhpIdentFuncTable[39]:=PhpKeywordFunc39;
  FPhpIdentFuncTable[40]:=PhpKeywordFunc40;
  FPhpIdentFuncTable[41]:=PhpKeywordFunc41;
  FPhpIdentFuncTable[42]:=PhpKeywordFunc42;
  FPhpIdentFuncTable[43]:=PhpKeywordFunc43;
  FPhpIdentFuncTable[44]:=PhpKeywordFunc44;
  FPhpIdentFuncTable[45]:=PhpKeywordFunc45;
  FPhpIdentFuncTable[46]:=PhpKeywordFunc46;
  FPhpIdentFuncTable[47]:=PhpKeywordFunc47;
  FPhpIdentFuncTable[48]:=PhpKeywordFunc48;
  FPhpIdentFuncTable[49]:=PhpKeywordFunc49;
  FPhpIdentFuncTable[50]:=PhpKeywordFunc50;
  FPhpIdentFuncTable[51]:=PhpKeywordFunc51;
  FPhpIdentFuncTable[52]:=PhpKeywordFunc52;
  FPhpIdentFuncTable[53]:=PhpKeywordFunc53;
  FPhpIdentFuncTable[54]:=PhpKeywordFunc54;
  FPhpIdentFuncTable[55]:=PhpKeywordFunc55;
  FPhpIdentFuncTable[56]:=PhpKeywordFunc56;
  FPhpIdentFuncTable[57]:=PhpKeywordFunc57;
  FPhpIdentFuncTable[58]:=PhpKeywordFunc58;
  FPhpIdentFuncTable[59]:=PhpKeywordFunc59;
  FPhpIdentFuncTable[60]:=PhpKeywordFunc60;
  FPhpIdentFuncTable[61]:=PhpKeywordFunc61;
  FPhpIdentFuncTable[62]:=PhpKeywordFunc62;
  FPhpIdentFuncTable[63]:=PhpKeywordFunc63;
  FPhpIdentFuncTable[64]:=PhpKeywordFunc64;
  FPhpIdentFuncTable[65]:=PhpKeywordFunc65;
  FPhpIdentFuncTable[66]:=PhpKeywordFunc66;
  FPhpIdentFuncTable[67]:=PhpKeywordFunc67;
  FPhpIdentFuncTable[68]:=PhpKeywordFunc68;
  FPhpIdentFuncTable[69]:=PhpKeywordFunc69;
  FPhpIdentFuncTable[70]:=PhpKeywordFunc70;
  FPhpIdentFuncTable[71]:=PhpKeywordFunc71;
  FPhpIdentFuncTable[72]:=PhpKeywordFunc72;
  FPhpIdentFuncTable[73]:=PhpKeywordFunc73;
  FPhpIdentFuncTable[74]:=PhpKeywordFunc74;
  FPhpIdentFuncTable[75]:=PhpKeywordFunc75;
  FPhpIdentFuncTable[76]:=PhpKeywordFunc76;
  FPhpIdentFuncTable[77]:=PhpKeywordFunc77;
  FPhpIdentFuncTable[78]:=PhpKeywordFunc78;
  FPhpIdentFuncTable[79]:=PhpKeywordFunc79;
  FPhpIdentFuncTable[80]:=PhpKeywordFunc80;
  FPhpIdentFuncTable[81]:=PhpKeywordFunc81;
  FPhpIdentFuncTable[82]:=PhpKeywordFunc82;
  FPhpIdentFuncTable[83]:=PhpKeywordFunc83;
  FPhpIdentFuncTable[84]:=PhpKeywordFunc84;
  FPhpIdentFuncTable[85]:=PhpKeywordFunc85;
  FPhpIdentFuncTable[86]:=PhpKeywordFunc86;
  FPhpIdentFuncTable[87]:=PhpKeywordFunc87;
  FPhpIdentFuncTable[88]:=PhpKeywordFunc88;
  FPhpIdentFuncTable[89]:=PhpKeywordFunc89;
  FPhpIdentFuncTable[90]:=PhpKeywordFunc90;
  FPhpIdentFuncTable[91]:=PhpKeywordFunc91;
  FPhpIdentFuncTable[92]:=PhpKeywordFunc92;
  FPhpIdentFuncTable[93]:=PhpKeywordFunc93;
  FPhpIdentFuncTable[94]:=PhpKeywordFunc94;
  FPhpIdentFuncTable[95]:=PhpKeywordFunc95;
  FPhpIdentFuncTable[96]:=PhpKeywordFunc96;
  FPhpIdentFuncTable[97]:=PhpKeywordFunc97;
  FPhpIdentFuncTable[98]:=PhpKeywordFunc98;
  FPhpIdentFuncTable[99]:=PhpKeywordFunc99;
  FPhpIdentFuncTable[100]:=PhpKeywordFunc100;
  FPhpIdentFuncTable[101]:=PhpKeywordFunc101;
  FPhpIdentFuncTable[102]:=PhpKeywordFunc102;
  FPhpIdentFuncTable[103]:=PhpKeywordFunc103;
  FPhpIdentFuncTable[104]:=PhpKeywordFunc104;
  FPhpIdentFuncTable[105]:=PhpKeywordFunc105;
  FPhpIdentFuncTable[106]:=PhpKeywordFunc106;
  FPhpIdentFuncTable[107]:=PhpKeywordFunc107;
  FPhpIdentFuncTable[108]:=PhpKeywordFunc108;
  FPhpIdentFuncTable[109]:=PhpKeywordFunc109;
  FPhpIdentFuncTable[110]:=PhpKeywordFunc110;
  FPhpIdentFuncTable[111]:=PhpKeywordFunc111;
  FPhpIdentFuncTable[112]:=PhpKeywordFunc112;
  FPhpIdentFuncTable[113]:=PhpKeywordFunc113;
  FPhpIdentFuncTable[114]:=PhpKeywordFunc114;
  FPhpIdentFuncTable[115]:=PhpKeywordFunc115;
  FPhpIdentFuncTable[116]:=PhpKeywordFunc116;
  FPhpIdentFuncTable[117]:=PhpKeywordFunc117;
  FPhpIdentFuncTable[118]:=PhpKeywordFunc118;
  FPhpIdentFuncTable[119]:=PhpKeywordFunc119;
  FPhpIdentFuncTable[120]:=PhpKeywordFunc120;
  FPhpIdentFuncTable[121]:=PhpKeywordFunc121;
  FPhpIdentFuncTable[122]:=PhpKeywordFunc122;
  FPhpIdentFuncTable[123]:=PhpKeywordFunc123;
  FPhpIdentFuncTable[124]:=PhpKeywordFunc124;
  FPhpIdentFuncTable[125]:=PhpKeywordFunc125;
  FPhpIdentFuncTable[126]:=PhpKeywordFunc126;
  FPhpIdentFuncTable[127]:=PhpKeywordFunc127;
  FPhpIdentFuncTable[128]:=PhpKeywordFunc128;
  FPhpIdentFuncTable[129]:=PhpKeywordFunc129;
  FPhpIdentFuncTable[130]:=PhpKeywordFunc130;
  FPhpIdentFuncTable[131]:=PhpKeywordFunc131;
  FPhpIdentFuncTable[132]:=PhpKeywordFunc132;
  FPhpIdentFuncTable[133]:=PhpKeywordFunc133;
  FPhpIdentFuncTable[134]:=PhpKeywordFunc134;
  FPhpIdentFuncTable[135]:=PhpKeywordFunc135;
  FPhpIdentFuncTable[136]:=PhpKeywordFunc136;
  FPhpIdentFuncTable[137]:=PhpKeywordFunc137;
  FPhpIdentFuncTable[138]:=PhpKeywordFunc138;
  FPhpIdentFuncTable[139]:=PhpKeywordFunc139;
  FPhpIdentFuncTable[140]:=PhpKeywordFunc140;
  FPhpIdentFuncTable[141]:=PhpKeywordFunc141;
  FPhpIdentFuncTable[142]:=PhpKeywordFunc142;
  FPhpIdentFuncTable[143]:=PhpKeywordFunc143;
  FPhpIdentFuncTable[144]:=PhpKeywordFunc144;
  FPhpIdentFuncTable[145]:=PhpKeywordFunc145;
  FPhpIdentFuncTable[146]:=PhpKeywordFunc146;
  FPhpIdentFuncTable[147]:=PhpKeywordFunc147;
  FPhpIdentFuncTable[148]:=PhpKeywordFunc148;
  FPhpIdentFuncTable[149]:=PhpKeywordFunc149;
  FPhpIdentFuncTable[150]:=PhpKeywordFunc150;
  FPhpIdentFuncTable[151]:=PhpKeywordFunc151;
  FPhpIdentFuncTable[152]:=PhpKeywordFunc152;
  FPhpIdentFuncTable[153]:=PhpKeywordFunc153;
  FPhpIdentFuncTable[154]:=PhpKeywordFunc154;
  FPhpIdentFuncTable[155]:=PhpKeywordFunc155;
  FPhpIdentFuncTable[156]:=PhpKeywordFunc156;
  FPhpIdentFuncTable[157]:=PhpKeywordFunc157;
  FPhpIdentFuncTable[158]:=PhpKeywordFunc158;
  FPhpIdentFuncTable[159]:=PhpKeywordFunc159;
  FPhpIdentFuncTable[160]:=PhpKeywordFunc160;
  FPhpIdentFuncTable[161]:=PhpKeywordFunc161;
  FPhpIdentFuncTable[162]:=PhpKeywordFunc162;
  FPhpIdentFuncTable[163]:=PhpKeywordFunc163;
  FPhpIdentFuncTable[164]:=PhpKeywordFunc164;
  FPhpIdentFuncTable[165]:=PhpKeywordFunc165;
  FPhpIdentFuncTable[166]:=PhpKeywordFunc166;
  FPhpIdentFuncTable[167]:=PhpKeywordFunc167;
  FPhpIdentFuncTable[168]:=PhpKeywordFunc168;
  FPhpIdentFuncTable[169]:=PhpKeywordFunc169;
  FPhpIdentFuncTable[170]:=PhpKeywordFunc170;
  FPhpIdentFuncTable[171]:=PhpKeywordFunc171;
  FPhpIdentFuncTable[172]:=PhpKeywordFunc172;
  FPhpIdentFuncTable[173]:=PhpKeywordFunc173;
  FPhpIdentFuncTable[174]:=PhpKeywordFunc174;
  FPhpIdentFuncTable[175]:=PhpKeywordFunc175;
  FPhpIdentFuncTable[176]:=PhpKeywordFunc176;
  FPhpIdentFuncTable[177]:=PhpKeywordFunc177;
  FPhpIdentFuncTable[178]:=PhpKeywordFunc178;
  FPhpIdentFuncTable[179]:=PhpKeywordFunc179;
  FPhpIdentFuncTable[180]:=PhpKeywordFunc180;
  FPhpIdentFuncTable[181]:=PhpKeywordFunc181;
  FPhpIdentFuncTable[182]:=PhpKeywordFunc182;
  FPhpIdentFuncTable[183]:=PhpKeywordFunc183;
  FPhpIdentFuncTable[184]:=PhpKeywordFunc184;
  FPhpIdentFuncTable[185]:=PhpKeywordFunc185;
  FPhpIdentFuncTable[186]:=PhpKeywordFunc186;
  FPhpIdentFuncTable[187]:=PhpKeywordFunc187;
  FPhpIdentFuncTable[188]:=PhpKeywordFunc188;
  FPhpIdentFuncTable[189]:=PhpKeywordFunc189;
  FPhpIdentFuncTable[190]:=PhpKeywordFunc190;
  FPhpIdentFuncTable[191]:=PhpKeywordFunc191;
  FPhpIdentFuncTable[192]:=PhpKeywordFunc192;
  FPhpIdentFuncTable[193]:=PhpKeywordFunc193;
  FPhpIdentFuncTable[194]:=PhpKeywordFunc194;
  FPhpIdentFuncTable[195]:=PhpKeywordFunc195;
  FPhpIdentFuncTable[196]:=PhpKeywordFunc196;
  FPhpIdentFuncTable[197]:=PhpKeywordFunc197;
  FPhpIdentFuncTable[198]:=PhpKeywordFunc198;
  FPhpIdentFuncTable[199]:=PhpKeywordFunc199;
  FPhpIdentFuncTable[200]:=PhpKeywordFunc200;
  FPhpIdentFuncTable[201]:=PhpKeywordFunc201;
  FPhpIdentFuncTable[202]:=PhpKeywordFunc202;
  FPhpIdentFuncTable[203]:=PhpKeywordFunc203;
  FPhpIdentFuncTable[204]:=PhpKeywordFunc204;
  FPhpIdentFuncTable[205]:=PhpKeywordFunc205;
  FPhpIdentFuncTable[206]:=PhpKeywordFunc206;
  FPhpIdentFuncTable[207]:=PhpKeywordFunc207;
  FPhpIdentFuncTable[208]:=PhpKeywordFunc208;
  FPhpIdentFuncTable[209]:=PhpKeywordFunc209;
  FPhpIdentFuncTable[210]:=PhpKeywordFunc210;
  FPhpIdentFuncTable[211]:=PhpKeywordFunc211;
  FPhpIdentFuncTable[212]:=PhpKeywordFunc212;
  FPhpIdentFuncTable[213]:=PhpKeywordFunc213;
  FPhpIdentFuncTable[214]:=PhpKeywordFunc214;
  FPhpIdentFuncTable[215]:=PhpKeywordFunc215;
  FPhpIdentFuncTable[216]:=PhpKeywordFunc216;
  FPhpIdentFuncTable[217]:=PhpKeywordFunc217;
  FPhpIdentFuncTable[218]:=PhpKeywordFunc218;
  FPhpIdentFuncTable[219]:=PhpKeywordFunc219;
  FPhpIdentFuncTable[220]:=PhpKeywordFunc220;
  FPhpIdentFuncTable[221]:=PhpKeywordFunc221;
  FPhpIdentFuncTable[222]:=PhpKeywordFunc222;
  FPhpIdentFuncTable[223]:=PhpKeywordFunc223;
  FPhpIdentFuncTable[224]:=PhpKeywordFunc224;
  FPhpIdentFuncTable[225]:=PhpKeywordFunc225;
  FPhpIdentFuncTable[226]:=PhpKeywordFunc226;
  FPhpIdentFuncTable[227]:=PhpKeywordFunc227;
  FPhpIdentFuncTable[228]:=PhpKeywordFunc228;
  FPhpIdentFuncTable[229]:=PhpKeywordFunc229;
  FPhpIdentFuncTable[230]:=PhpKeywordFunc230;
  FPhpIdentFuncTable[231]:=PhpKeywordFunc231;
  FPhpIdentFuncTable[232]:=PhpKeywordFunc232;
  FPhpIdentFuncTable[233]:=PhpKeywordFunc233;
  FPhpIdentFuncTable[234]:=PhpKeywordFunc234;
  FPhpIdentFuncTable[235]:=PhpKeywordFunc235;
  FPhpIdentFuncTable[236]:=PhpKeywordFunc236;
  FPhpIdentFuncTable[237]:=PhpKeywordFunc237;
  FPhpIdentFuncTable[238]:=PhpKeywordFunc238;
  FPhpIdentFuncTable[239]:=PhpKeywordFunc239;
  FPhpIdentFuncTable[240]:=PhpKeywordFunc240;
  FPhpIdentFuncTable[241]:=PhpKeywordFunc241;
  FPhpIdentFuncTable[242]:=PhpKeywordFunc242;
  FPhpIdentFuncTable[243]:=PhpKeywordFunc243;
  FPhpIdentFuncTable[244]:=PhpKeywordFunc244;
  FPhpIdentFuncTable[245]:=PhpKeywordFunc245;
  FPhpIdentFuncTable[246]:=PhpKeywordFunc246;
  FPhpIdentFuncTable[247]:=PhpKeywordFunc247;
  FPhpIdentFuncTable[248]:=PhpKeywordFunc248;
  FPhpIdentFuncTable[249]:=PhpKeywordFunc249;
  FPhpIdentFuncTable[250]:=PhpKeywordFunc250;
  FPhpIdentFuncTable[251]:=PhpKeywordFunc251;
  FPhpIdentFuncTable[252]:=PhpKeywordFunc252;
  FPhpIdentFuncTable[253]:=PhpKeywordFunc253;
  FPhpIdentFuncTable[254]:=PhpKeywordFunc254;
  FPhpIdentFuncTable[255]:=PhpKeywordFunc255;
  FPhpIdentFuncTable[256]:=PhpKeywordFunc256;
  FPhpIdentFuncTable[257]:=PhpKeywordFunc257;
  FPhpIdentFuncTable[258]:=PhpKeywordFunc258;
  FPhpIdentFuncTable[259]:=PhpKeywordFunc259;
  FPhpIdentFuncTable[260]:=PhpKeywordFunc260;
  FPhpIdentFuncTable[261]:=PhpKeywordFunc261;
  FPhpIdentFuncTable[262]:=PhpKeywordFunc262;
  FPhpIdentFuncTable[263]:=PhpKeywordFunc263;
  FPhpIdentFuncTable[264]:=PhpKeywordFunc264;
  FPhpIdentFuncTable[265]:=PhpKeywordFunc265;
  FPhpIdentFuncTable[266]:=PhpKeywordFunc266;
  FPhpIdentFuncTable[267]:=PhpKeywordFunc267;
  FPhpIdentFuncTable[268]:=PhpKeywordFunc268;
  FPhpIdentFuncTable[269]:=PhpKeywordFunc269;
  FPhpIdentFuncTable[270]:=PhpKeywordFunc270;
  FPhpIdentFuncTable[271]:=PhpKeywordFunc271;
  FPhpIdentFuncTable[272]:=PhpKeywordFunc272;
  FPhpIdentFuncTable[273]:=PhpKeywordFunc273;
  FPhpIdentFuncTable[274]:=PhpKeywordFunc274;
  FPhpIdentFuncTable[275]:=PhpKeywordFunc275;
  FPhpIdentFuncTable[276]:=PhpKeywordFunc276;
  FPhpIdentFuncTable[277]:=PhpKeywordFunc277;
  FPhpIdentFuncTable[278]:=PhpKeywordFunc278;
  FPhpIdentFuncTable[279]:=PhpKeywordFunc279;
  FPhpIdentFuncTable[280]:=PhpKeywordFunc280;
  FPhpIdentFuncTable[281]:=PhpKeywordFunc281;
  FPhpIdentFuncTable[282]:=PhpKeywordFunc282;
  FPhpIdentFuncTable[283]:=PhpKeywordFunc283;
  FPhpIdentFuncTable[284]:=PhpKeywordFunc284;
  FPhpIdentFuncTable[285]:=PhpKeywordFunc285;
  FPhpIdentFuncTable[286]:=PhpKeywordFunc286;
  FPhpIdentFuncTable[287]:=PhpKeywordFunc287;
  FPhpIdentFuncTable[288]:=PhpKeywordFunc288;
  FPhpIdentFuncTable[289]:=PhpKeywordFunc289;
  FPhpIdentFuncTable[290]:=PhpKeywordFunc290;
  FPhpIdentFuncTable[291]:=PhpKeywordFunc291;
  FPhpIdentFuncTable[292]:=PhpKeywordFunc292;
  FPhpIdentFuncTable[293]:=PhpKeywordFunc293;
  FPhpIdentFuncTable[294]:=PhpKeywordFunc294;
  FPhpIdentFuncTable[295]:=PhpKeywordFunc295;
  FPhpIdentFuncTable[296]:=PhpKeywordFunc296;
  FPhpIdentFuncTable[297]:=PhpKeywordFunc297;
  FPhpIdentFuncTable[298]:=PhpKeywordFunc298;
  FPhpIdentFuncTable[299]:=PhpKeywordFunc299;
  FPhpIdentFuncTable[300]:=PhpKeywordFunc300;
  FPhpIdentFuncTable[302]:=PhpKeywordFunc302;
  FPhpIdentFuncTable[303]:=PhpKeywordFunc303;
  FPhpIdentFuncTable[304]:=PhpKeywordFunc304;
  FPhpIdentFuncTable[305]:=PhpKeywordFunc305;
  FPhpIdentFuncTable[306]:=PhpKeywordFunc306;
  FPhpIdentFuncTable[307]:=PhpKeywordFunc307;
  FPhpIdentFuncTable[308]:=PhpKeywordFunc308;
  FPhpIdentFuncTable[309]:=PhpKeywordFunc309;
  FPhpIdentFuncTable[310]:=PhpKeywordFunc310;
  FPhpIdentFuncTable[311]:=PhpKeywordFunc311;
  FPhpIdentFuncTable[312]:=PhpKeywordFunc312;
  FPhpIdentFuncTable[313]:=PhpKeywordFunc313;
  FPhpIdentFuncTable[314]:=PhpKeywordFunc314;
  FPhpIdentFuncTable[315]:=PhpKeywordFunc315;
  FPhpIdentFuncTable[316]:=PhpKeywordFunc316;
  FPhpIdentFuncTable[317]:=PhpKeywordFunc317;
  FPhpIdentFuncTable[318]:=PhpKeywordFunc318;
  FPhpIdentFuncTable[319]:=PhpKeywordFunc319;
  FPhpIdentFuncTable[321]:=PhpKeywordFunc321;
  FPhpIdentFuncTable[322]:=PhpKeywordFunc322;
  FPhpIdentFuncTable[323]:=PhpKeywordFunc323;
  FPhpIdentFuncTable[324]:=PhpKeywordFunc324;
  FPhpIdentFuncTable[325]:=PhpKeywordFunc325;
  FPhpIdentFuncTable[326]:=PhpKeywordFunc326;
  FPhpIdentFuncTable[327]:=PhpKeywordFunc327;
  FPhpIdentFuncTable[328]:=PhpKeywordFunc328;
  FPhpIdentFuncTable[329]:=PhpKeywordFunc329;
  FPhpIdentFuncTable[330]:=PhpKeywordFunc330;
  FPhpIdentFuncTable[331]:=PhpKeywordFunc331;
  FPhpIdentFuncTable[332]:=PhpKeywordFunc332;
  FPhpIdentFuncTable[333]:=PhpKeywordFunc333;
  FPhpIdentFuncTable[334]:=PhpKeywordFunc334;
  FPhpIdentFuncTable[335]:=PhpKeywordFunc335;
  FPhpIdentFuncTable[336]:=PhpKeywordFunc336;
  FPhpIdentFuncTable[337]:=PhpKeywordFunc337;
  FPhpIdentFuncTable[338]:=PhpKeywordFunc338;
  FPhpIdentFuncTable[339]:=PhpKeywordFunc339;
  FPhpIdentFuncTable[340]:=PhpKeywordFunc340;
  FPhpIdentFuncTable[341]:=PhpKeywordFunc341;
  FPhpIdentFuncTable[343]:=PhpKeywordFunc343;
  FPhpIdentFuncTable[344]:=PhpKeywordFunc344;
  FPhpIdentFuncTable[345]:=PhpKeywordFunc345;
  FPhpIdentFuncTable[346]:=PhpKeywordFunc346;
  FPhpIdentFuncTable[347]:=PhpKeywordFunc347;
  FPhpIdentFuncTable[348]:=PhpKeywordFunc348;
  FPhpIdentFuncTable[349]:=PhpKeywordFunc349;
  FPhpIdentFuncTable[351]:=PhpKeywordFunc351;
  FPhpIdentFuncTable[352]:=PhpKeywordFunc352;
  FPhpIdentFuncTable[353]:=PhpKeywordFunc353;
  FPhpIdentFuncTable[354]:=PhpKeywordFunc354;
  FPhpIdentFuncTable[357]:=PhpKeywordFunc357;
  FPhpIdentFuncTable[358]:=PhpKeywordFunc358;
  FPhpIdentFuncTable[359]:=PhpKeywordFunc359;
  FPhpIdentFuncTable[360]:=PhpKeywordFunc360;
  FPhpIdentFuncTable[361]:=PhpKeywordFunc361;
  FPhpIdentFuncTable[364]:=PhpKeywordFunc364;
  FPhpIdentFuncTable[366]:=PhpKeywordFunc366;
  FPhpIdentFuncTable[369]:=PhpKeywordFunc369;
  FPhpIdentFuncTable[370]:=PhpKeywordFunc370;
  FPhpIdentFuncTable[373]:=PhpKeywordFunc373;
  FPhpIdentFuncTable[374]:=PhpKeywordFunc374;
  FPhpIdentFuncTable[375]:=PhpKeywordFunc375;
  FPhpIdentFuncTable[376]:=PhpKeywordFunc376;
  FPhpIdentFuncTable[377]:=PhpKeywordFunc377;
  FPhpIdentFuncTable[385]:=PhpKeywordFunc385;
  FPhpIdentFuncTable[387]:=PhpKeywordFunc387;
  FPhpIdentFuncTable[393]:=PhpKeywordFunc393;
  FPhpIdentFuncTable[399]:=PhpKeywordFunc399;
  FPhpIdentFuncTable[402]:=PhpKeywordFunc402;
  FPhpIdentFuncTable[407]:=PhpKeywordFunc407;
  FPhpIdentFuncTable[426]:=PhpKeywordFunc426;
  FPhpIdentFuncTable[433]:=PhpKeywordFunc433;
  FPhpIdentFuncTable[439]:=PhpKeywordFunc439;
  FPhpIdentFuncTable[442]:=PhpKeywordFunc442;
  FPhpIdentFuncTable[443]:=PhpKeywordFunc443;
  FPhpIdentFuncTable[445]:=PhpKeywordFunc445;
  FPhpIdentFuncTable[450]:=PhpKeywordFunc450;
  FPhpIdentFuncTable[465]:=PhpKeywordFunc465;
  FPhpIdentFuncTable[466]:=PhpKeywordFunc466;
  FPhpIdentFuncTable[487]:=PhpKeywordFunc487;
  FPhpIdentFuncTable[491]:=PhpKeywordFunc491;
  FPhpIdentFuncTable[496]:=PhpKeywordFunc496;
  FPhpIdentFuncTable[520]:=PhpKeywordFunc520;
  FPhpIdentFuncTable[541]:=PhpKeywordFunc541;
  FPhpIdentFuncTable[560]:=PhpKeywordFunc560;
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

function TSynWebEngine.PhpKeywordIdent: TSynWebTokenKind;
begin
  Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc14: TSynWebTokenKind;
begin
  if  PhpFunctionComp(272) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc15: TSynWebTokenKind;
begin
  if  PhpKeywordComp(41) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc16: TSynWebTokenKind;
begin
  if  PhpFunctionComp(795) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc17: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1057) or
      PhpFunctionComp(2520) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc18: TSynWebTokenKind;
begin
  if  PhpKeywordComp(18) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc19: TSynWebTokenKind;
begin
  if  PhpKeywordComp(6) or
      PhpKeywordComp(19) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc20: TSynWebTokenKind;
begin
  if  PhpKeywordComp(8) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc22: TSynWebTokenKind;
begin
  if  PhpFunctionComp(93) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc23: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1090) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc24: TSynWebTokenKind;
begin
  if  PhpFunctionComp(486) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc25: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3639) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc28: TSynWebTokenKind;
begin
  if  PhpKeywordComp(10) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(4079) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc29: TSynWebTokenKind;
begin
  if  PhpFunctionComp(346) or
      PhpFunctionComp(355) or
      PhpFunctionComp(2088) or
      PhpFunctionComp(2192) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc30: TSynWebTokenKind;
begin
  if  PhpFunctionComp(547) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc31: TSynWebTokenKind;
begin
  if  PhpKeywordComp(20) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(791) or
        PhpFunctionComp(3348) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc32: TSynWebTokenKind;
begin
  if  PhpKeywordComp(1) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1234) or
        PhpFunctionComp(1240) or
        PhpFunctionComp(2238) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc33: TSynWebTokenKind;
begin
  if  PhpKeywordComp(54) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(2255) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc34: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1276) or
      PhpFunctionComp(2203) or
      PhpFunctionComp(2204) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc35: TSynWebTokenKind;
begin
  if  PhpKeywordComp(11) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1091) or
        PhpFunctionComp(2214) or
        PhpFunctionComp(2293) or
        PhpFunctionComp(4755) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc36: TSynWebTokenKind;
begin
  if  PhpFunctionComp(255) or
      PhpFunctionComp(256) or
      PhpFunctionComp(1434) or
      PhpFunctionComp(1508) or
      PhpFunctionComp(2287) or
      PhpFunctionComp(2564) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc37: TSynWebTokenKind;
begin
  if  PhpKeywordComp(9) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(275) or
        PhpFunctionComp(297) or
        PhpFunctionComp(406) or
        PhpFunctionComp(772) or
        PhpFunctionComp(1111) or
        PhpFunctionComp(3322) or
        PhpFunctionComp(3899) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc38: TSynWebTokenKind;
begin
  if  PhpKeywordComp(27) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(94) or
        PhpFunctionComp(1270) or
        PhpFunctionComp(2308) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc39: TSynWebTokenKind;
begin
  if  PhpKeywordComp(37) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(779) or
        PhpFunctionComp(1790) or
        PhpFunctionComp(3873) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc40: TSynWebTokenKind;
begin
  if  PhpKeywordComp(3) or
      PhpKeywordComp(30) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(274) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc41: TSynWebTokenKind;
begin
  if  PhpKeywordComp(21) or
      PhpKeywordComp(71) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(97) or
        PhpFunctionComp(1236) or
        PhpFunctionComp(1518) or
        PhpFunctionComp(2143) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc42: TSynWebTokenKind;
begin
  if  PhpKeywordComp(35) or
      PhpKeywordComp(50) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(349) or
        PhpFunctionComp(2152) or
        PhpFunctionComp(4111) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc43: TSynWebTokenKind;
begin
  if  PhpKeywordComp(34) or
      PhpKeywordComp(46) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(250) or
        PhpFunctionComp(353) or
        PhpFunctionComp(638) or
        PhpFunctionComp(776) or
        PhpFunctionComp(4756) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc44: TSynWebTokenKind;
begin
  if  PhpFunctionComp(257) or
      PhpFunctionComp(1093) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc45: TSynWebTokenKind;
begin
  if  PhpKeywordComp(70) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(292) or
        PhpFunctionComp(407) or
        PhpFunctionComp(1124) or
        PhpFunctionComp(1438) or
        PhpFunctionComp(3900) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc46: TSynWebTokenKind;
begin
  if  PhpFunctionComp(95) or
      PhpFunctionComp(1341) or
      PhpFunctionComp(1912) or
      PhpFunctionComp(1913) or
      PhpFunctionComp(2199) or
      PhpFunctionComp(2237) or
      PhpFunctionComp(3006) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc47: TSynWebTokenKind;
begin
  if  PhpFunctionComp(294) or
      PhpFunctionComp(778) or
      PhpFunctionComp(1267) or
      PhpFunctionComp(4793) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc48: TSynWebTokenKind;
begin
  if  PhpKeywordComp(17) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(2125) or
        PhpFunctionComp(3384) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc49: TSynWebTokenKind;
begin
  if  PhpKeywordComp(14) or
      PhpKeywordComp(40) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(676) or
        PhpFunctionComp(773) or
        PhpFunctionComp(1340) or
        PhpFunctionComp(1525) or
        PhpFunctionComp(2521) or
        PhpFunctionComp(2557) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc50: TSynWebTokenKind;
begin
  if  PhpFunctionComp(347) or
      PhpFunctionComp(774) or
      PhpFunctionComp(1435) or
      PhpFunctionComp(1448) or
      PhpFunctionComp(1588) or
      PhpFunctionComp(2205) or
      PhpFunctionComp(2261) or
      PhpFunctionComp(2408) or
      PhpFunctionComp(3198) or
      PhpFunctionComp(3242) or
      PhpFunctionComp(4112) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc51: TSynWebTokenKind;
begin
  if  PhpFunctionComp(251) or
      PhpFunctionComp(276) or
      PhpFunctionComp(412) or
      PhpFunctionComp(780) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc52: TSynWebTokenKind;
begin
  if  PhpFunctionComp(200) or
      PhpFunctionComp(273) or
      PhpFunctionComp(352) or
      PhpFunctionComp(1345) or
      PhpFunctionComp(2137) or
      PhpFunctionComp(3063) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc53: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3307) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc54: TSynWebTokenKind;
begin
  if  PhpKeywordComp(0) or
      PhpKeywordComp(13) or
      PhpKeywordComp(36) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(369) or
        PhpFunctionComp(3687) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc55: TSynWebTokenKind;
begin
  if  PhpKeywordComp(52) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1344) or
        PhpFunctionComp(1385) or
        PhpFunctionComp(1439) or
        PhpFunctionComp(2570) or
        PhpFunctionComp(4281) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc56: TSynWebTokenKind;
begin
  if  PhpKeywordComp(22) or
      PhpKeywordComp(38) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(334) or
        PhpFunctionComp(785) or
        PhpFunctionComp(1272) or
        PhpFunctionComp(3936) or
        PhpFunctionComp(4338) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc57: TSynWebTokenKind;
begin
  if  PhpKeywordComp(72) or
      PhpKeywordComp(73) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(683) or
        PhpFunctionComp(761) or
        PhpFunctionComp(1215) or
        PhpFunctionComp(1238) or
        PhpFunctionComp(1860) or
        PhpFunctionComp(1914) or
        PhpFunctionComp(2100) or
        PhpFunctionComp(3743) or
        PhpFunctionComp(4113) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc58: TSynWebTokenKind;
begin
  if  PhpKeywordComp(32) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(674) or
        PhpFunctionComp(1126) or
        PhpFunctionComp(1437) or
        PhpFunctionComp(1801) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc59: TSynWebTokenKind;
begin
  if  PhpKeywordComp(51) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(101) or
        PhpFunctionComp(290) or
        PhpFunctionComp(405) or
        PhpFunctionComp(2092) or
        PhpFunctionComp(2252) or
        PhpFunctionComp(2418) or
        PhpFunctionComp(3057) or
        PhpFunctionComp(3907) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc60: TSynWebTokenKind;
begin
  if  PhpKeywordComp(49) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(271) or
        PhpFunctionComp(337) or
        PhpFunctionComp(350) or
        PhpFunctionComp(630) or
        PhpFunctionComp(1198) or
        PhpFunctionComp(1523) or
        PhpFunctionComp(2094) or
        PhpFunctionComp(2230) or
        PhpFunctionComp(2537) or
        PhpFunctionComp(3908) or
        PhpFunctionComp(4080) or
        PhpFunctionComp(4370) or
        PhpFunctionComp(4803) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc61: TSynWebTokenKind;
begin
  if  PhpFunctionComp(673) or
      PhpFunctionComp(704) or
      PhpFunctionComp(1512) or
      PhpFunctionComp(2182) or
      PhpFunctionComp(2239) or
      PhpFunctionComp(3211) or
      PhpFunctionComp(3695) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc62: TSynWebTokenKind;
begin
  if  PhpKeywordComp(25) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(194) or
        PhpFunctionComp(295) or
        PhpFunctionComp(698) or
        PhpFunctionComp(1410) or
        PhpFunctionComp(1411) or
        PhpFunctionComp(1458) or
        PhpFunctionComp(2153) or
        PhpFunctionComp(3385) or
        PhpFunctionComp(3557) or
        PhpFunctionComp(3949) or
        PhpFunctionComp(4339) or
        PhpFunctionComp(4817) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc63: TSynWebTokenKind;
begin
  if  PhpKeywordComp(7) or
      PhpKeywordComp(59) or
      PhpKeywordComp(68) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(269) or
        PhpFunctionComp(354) or
        PhpFunctionComp(389) or
        PhpFunctionComp(1220) or
        PhpFunctionComp(1524) or
        PhpFunctionComp(1771) or
        PhpFunctionComp(2292) or
        PhpFunctionComp(3003) or
        PhpFunctionComp(3299) or
        PhpFunctionComp(4293) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc64: TSynWebTokenKind;
begin
  if  PhpKeywordComp(67) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(557) or
        PhpFunctionComp(792) or
        PhpFunctionComp(2042) or
        PhpFunctionComp(4520) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc65: TSynWebTokenKind;
begin
  if  PhpKeywordComp(4) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(338) or
        PhpFunctionComp(672) or
        PhpFunctionComp(1206) or
        PhpFunctionComp(1271) or
        PhpFunctionComp(1504) or
        PhpFunctionComp(1861) or
        PhpFunctionComp(3083) or
        PhpFunctionComp(3148) or
        PhpFunctionComp(3431) or
        PhpFunctionComp(4121) or
        PhpFunctionComp(4838) or
        PhpFunctionComp(5144) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc66: TSynWebTokenKind;
begin
  if  PhpFunctionComp(482) or
      PhpFunctionComp(700) or
      PhpFunctionComp(1217) or
      PhpFunctionComp(1268) or
      PhpFunctionComp(1269) or
      PhpFunctionComp(1343) or
      PhpFunctionComp(1447) or
      PhpFunctionComp(1640) or
      PhpFunctionComp(1953) or
      PhpFunctionComp(3397) or
      PhpFunctionComp(3485) or
      PhpFunctionComp(3652) or
      PhpFunctionComp(4842) or
      PhpFunctionComp(5143) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc67: TSynWebTokenKind;
begin
  if  PhpFunctionComp(681) or
      PhpFunctionComp(686) or
      PhpFunctionComp(1442) or
      PhpFunctionComp(3942) or
      PhpFunctionComp(4799) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc68: TSynWebTokenKind;
begin
  if  PhpKeywordComp(43) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(423) or
        PhpFunctionComp(687) or
        PhpFunctionComp(786) or
        PhpFunctionComp(1202) or
        PhpFunctionComp(1441) or
        PhpFunctionComp(1456) or
        PhpFunctionComp(1510) or
        PhpFunctionComp(3464) or
        PhpFunctionComp(3910) or
        PhpFunctionComp(4000) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc69: TSynWebTokenKind;
begin
  if  PhpFunctionComp(382) or
      PhpFunctionComp(415) or
      PhpFunctionComp(444) or
      PhpFunctionComp(708) or
      PhpFunctionComp(775) or
      PhpFunctionComp(1064) or
      PhpFunctionComp(1459) or
      PhpFunctionComp(1957) or
      PhpFunctionComp(2527) or
      PhpFunctionComp(3097) or
      PhpFunctionComp(3168) or
      PhpFunctionComp(3603) or
      PhpFunctionComp(4022) or
      PhpFunctionComp(5056) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc70: TSynWebTokenKind;
begin
  if  PhpFunctionComp(702) or
      PhpFunctionComp(1136) or
      PhpFunctionComp(1201) or
      PhpFunctionComp(1950) or
      PhpFunctionComp(2682) or
      PhpFunctionComp(3359) or
      PhpFunctionComp(3620) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc71: TSynWebTokenKind;
begin
  if  PhpKeywordComp(15) or
      PhpKeywordComp(24) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(150) or
        PhpFunctionComp(193) or
        PhpFunctionComp(396) or
        PhpFunctionComp(2067) or
        PhpFunctionComp(2571) or
        PhpFunctionComp(3310) or
        PhpFunctionComp(3744) or
        PhpFunctionComp(4820) or
        PhpFunctionComp(4998) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc72: TSynWebTokenKind;
begin
  if  PhpKeywordComp(48) or
      PhpKeywordComp(63) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(577) or
        PhpFunctionComp(1235) or
        PhpFunctionComp(1522) or
        PhpFunctionComp(1922) or
        PhpFunctionComp(2090) or
        PhpFunctionComp(2207) or
        PhpFunctionComp(2208) or
        PhpFunctionComp(3480) or
        PhpFunctionComp(3950) or
        PhpFunctionComp(4282) or
        PhpFunctionComp(4866) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc73: TSynWebTokenKind;
begin
  if  PhpFunctionComp(252) or
      PhpFunctionComp(270) or
      PhpFunctionComp(408) or
      PhpFunctionComp(685) or
      PhpFunctionComp(701) or
      PhpFunctionComp(1412) or
      PhpFunctionComp(1466) or
      PhpFunctionComp(1467) or
      PhpFunctionComp(1786) or
      PhpFunctionComp(1917) or
      PhpFunctionComp(2006) or
      PhpFunctionComp(2053) or
      PhpFunctionComp(2206) or
      PhpFunctionComp(2522) or
      PhpFunctionComp(3484) or
      PhpFunctionComp(3946) or
      PhpFunctionComp(3947) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc74: TSynWebTokenKind;
begin
  if  PhpFunctionComp(100) or
      PhpFunctionComp(1131) or
      PhpFunctionComp(1356) or
      PhpFunctionComp(1638) or
      PhpFunctionComp(1900) or
      PhpFunctionComp(1949) or
      PhpFunctionComp(1979) or
      PhpFunctionComp(2012) or
      PhpFunctionComp(2073) or
      PhpFunctionComp(2098) or
      PhpFunctionComp(2174) or
      PhpFunctionComp(3558) or
      PhpFunctionComp(3764) or
      PhpFunctionComp(3923) or
      PhpFunctionComp(4337) or
      PhpFunctionComp(4829) or
      PhpFunctionComp(4839) or
      PhpFunctionComp(4931) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc75: TSynWebTokenKind;
begin
  if  PhpFunctionComp(436) or
      PhpFunctionComp(552) or
      PhpFunctionComp(646) or
      PhpFunctionComp(1528) or
      PhpFunctionComp(1632) or
      PhpFunctionComp(1750) or
      PhpFunctionComp(1919) or
      PhpFunctionComp(3065) or
      PhpFunctionComp(3750) or
      PhpFunctionComp(3786) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc76: TSynWebTokenKind;
begin
  if  PhpFunctionComp(442) or
      PhpFunctionComp(1239) or
      PhpFunctionComp(1454) or
      PhpFunctionComp(1724) or
      PhpFunctionComp(1996) or
      PhpFunctionComp(2045) or
      PhpFunctionComp(2071) or
      PhpFunctionComp(2091) or
      PhpFunctionComp(2099) or
      PhpFunctionComp(2104) or
      PhpFunctionComp(3311) or
      PhpFunctionComp(3396) or
      PhpFunctionComp(3465) or
      PhpFunctionComp(4291) or
      PhpFunctionComp(5127) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc77: TSynWebTokenKind;
begin
  if  PhpKeywordComp(56) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(339) or
        PhpFunctionComp(340) or
        PhpFunctionComp(1222) or
        PhpFunctionComp(1653) or
        PhpFunctionComp(2054) or
        PhpFunctionComp(2231) or
        PhpFunctionComp(3417) or
        PhpFunctionComp(3549) or
        PhpFunctionComp(3775) or
        PhpFunctionComp(4105) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc78: TSynWebTokenKind;
begin
  if  PhpFunctionComp(333) or
      PhpFunctionComp(603) or
      PhpFunctionComp(784) or
      PhpFunctionComp(1473) or
      PhpFunctionComp(1511) or
      PhpFunctionComp(1942) or
      PhpFunctionComp(2052) or
      PhpFunctionComp(2216) or
      PhpFunctionComp(2533) or
      PhpFunctionComp(3201) or
      PhpFunctionComp(3426) or
      PhpFunctionComp(3591) or
      PhpFunctionComp(3957) or
      PhpFunctionComp(4442) or
      PhpFunctionComp(4856) or
      PhpFunctionComp(4860) or
      PhpFunctionComp(5057) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc79: TSynWebTokenKind;
begin
  if  PhpKeywordComp(23) or
      PhpKeywordComp(26) or
      PhpKeywordComp(69) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(191) or
        PhpFunctionComp(293) or
        PhpFunctionComp(356) or
        PhpFunctionComp(414) or
        PhpFunctionComp(432) or
        PhpFunctionComp(626) or
        PhpFunctionComp(633) or
        PhpFunctionComp(709) or
        PhpFunctionComp(1247) or
        PhpFunctionComp(1353) or
        PhpFunctionComp(1503) or
        PhpFunctionComp(1943) or
        PhpFunctionComp(2476) or
        PhpFunctionComp(5168) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc80: TSynWebTokenKind;
begin
  if  PhpKeywordComp(29) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(636) or
        PhpFunctionComp(664) or
        PhpFunctionComp(768) or
        PhpFunctionComp(1061) or
        PhpFunctionComp(1209) or
        PhpFunctionComp(1244) or
        PhpFunctionComp(1354) or
        PhpFunctionComp(3398) or
        PhpFunctionComp(4524) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc81: TSynWebTokenKind;
begin
  if  PhpKeywordComp(47) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(697) or
        PhpFunctionComp(1125) or
        PhpFunctionComp(1251) or
        PhpFunctionComp(1384) or
        PhpFunctionComp(2620) or
        PhpFunctionComp(3084) or
        PhpFunctionComp(3266) or
        PhpFunctionComp(3782) or
        PhpFunctionComp(3924) or
        PhpFunctionComp(4083) or
        PhpFunctionComp(4796) or
        PhpFunctionComp(4841) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc82: TSynWebTokenKind;
begin
  if  PhpKeywordComp(65) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(253) or
        PhpFunctionComp(336) or
        PhpFunctionComp(488) or
        PhpFunctionComp(649) or
        PhpFunctionComp(782) or
        PhpFunctionComp(1237) or
        PhpFunctionComp(1245) or
        PhpFunctionComp(1263) or
        PhpFunctionComp(1457) or
        PhpFunctionComp(1552) or
        PhpFunctionComp(1608) or
        PhpFunctionComp(2096) or
        PhpFunctionComp(2106) or
        PhpFunctionComp(2264) or
        PhpFunctionComp(3117) or
        PhpFunctionComp(3405) or
        PhpFunctionComp(3785) or
        PhpFunctionComp(3853) or
        PhpFunctionComp(3925) or
        PhpFunctionComp(4760) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc83: TSynWebTokenKind;
begin
  if  PhpFunctionComp(158) or
      PhpFunctionComp(416) or
      PhpFunctionComp(690) or
      PhpFunctionComp(710) or
      PhpFunctionComp(783) or
      PhpFunctionComp(1423) or
      PhpFunctionComp(1506) or
      PhpFunctionComp(2086) or
      PhpFunctionComp(2103) or
      PhpFunctionComp(2145) or
      PhpFunctionComp(2146) or
      PhpFunctionComp(2168) or
      PhpFunctionComp(2320) or
      PhpFunctionComp(2424) or
      PhpFunctionComp(2445) or
      PhpFunctionComp(2545) or
      PhpFunctionComp(3068) or
      PhpFunctionComp(3241) or
      PhpFunctionComp(3728) or
      PhpFunctionComp(3731) or
      PhpFunctionComp(4345) or
      PhpFunctionComp(4858) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc84: TSynWebTokenKind;
begin
  if  PhpKeywordComp(5) or
      PhpKeywordComp(66) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(190) or
        PhpFunctionComp(226) or
        PhpFunctionComp(628) or
        PhpFunctionComp(757) or
        PhpFunctionComp(1348) or
        PhpFunctionComp(1370) or
        PhpFunctionComp(1720) or
        PhpFunctionComp(2149) or
        PhpFunctionComp(2157) or
        PhpFunctionComp(3069) or
        PhpFunctionComp(3124) or
        PhpFunctionComp(3267) or
        PhpFunctionComp(3318) or
        PhpFunctionComp(3532) or
        PhpFunctionComp(3579) or
        PhpFunctionComp(3637) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc85: TSynWebTokenKind;
begin
  if  PhpFunctionComp(370) or
      PhpFunctionComp(659) or
      PhpFunctionComp(689) or
      PhpFunctionComp(1346) or
      PhpFunctionComp(1349) or
      PhpFunctionComp(1369) or
      PhpFunctionComp(1469) or
      PhpFunctionComp(1585) or
      PhpFunctionComp(1789) or
      PhpFunctionComp(1955) or
      PhpFunctionComp(1989) or
      PhpFunctionComp(2066) or
      PhpFunctionComp(2212) or
      PhpFunctionComp(3581) or
      PhpFunctionComp(3749) or
      PhpFunctionComp(4292) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc86: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1204) or
      PhpFunctionComp(1347) or
      PhpFunctionComp(1388) or
      PhpFunctionComp(1502) or
      PhpFunctionComp(1737) or
      PhpFunctionComp(2011) or
      PhpFunctionComp(2014) or
      PhpFunctionComp(2175) or
      PhpFunctionComp(2234) or
      PhpFunctionComp(3139) or
      PhpFunctionComp(3184) or
      PhpFunctionComp(3200) or
      PhpFunctionComp(3759) or
      PhpFunctionComp(3941) or
      PhpFunctionComp(4450) or
      PhpFunctionComp(5150) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc87: TSynWebTokenKind;
begin
  if  PhpKeywordComp(64) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1885) or
        PhpFunctionComp(1971) or
        PhpFunctionComp(2101) or
        PhpFunctionComp(2185) or
        PhpFunctionComp(2219) or
        PhpFunctionComp(2288) or
        PhpFunctionComp(3613) or
        PhpFunctionComp(3760) or
        PhpFunctionComp(3904) or
        PhpFunctionComp(3984) or
        PhpFunctionComp(4119) or
        PhpFunctionComp(4845) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc88: TSynWebTokenKind;
begin
  if  PhpFunctionComp(164) or
      PhpFunctionComp(207) or
      PhpFunctionComp(371) or
      PhpFunctionComp(548) or
      PhpFunctionComp(678) or
      PhpFunctionComp(787) or
      PhpFunctionComp(1443) or
      PhpFunctionComp(1451) or
      PhpFunctionComp(1514) or
      PhpFunctionComp(2151) or
      PhpFunctionComp(2190) or
      PhpFunctionComp(2217) or
      PhpFunctionComp(2347) or
      PhpFunctionComp(2350) or
      PhpFunctionComp(3123) or
      PhpFunctionComp(3176) or
      PhpFunctionComp(3177) or
      PhpFunctionComp(3300) or
      PhpFunctionComp(3526) or
      PhpFunctionComp(3597) or
      PhpFunctionComp(3622) or
      PhpFunctionComp(4003) or
      PhpFunctionComp(4496) or
      PhpFunctionComp(4602) or
      PhpFunctionComp(4762) or
      PhpFunctionComp(4861) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc89: TSynWebTokenKind;
begin
  if  PhpFunctionComp(639) or
      PhpFunctionComp(1060) or
      PhpFunctionComp(1137) or
      PhpFunctionComp(1266) or
      PhpFunctionComp(1274) or
      PhpFunctionComp(1387) or
      PhpFunctionComp(1445) or
      PhpFunctionComp(1857) or
      PhpFunctionComp(1948) or
      PhpFunctionComp(1988) or
      PhpFunctionComp(2043) or
      PhpFunctionComp(2072) or
      PhpFunctionComp(2183) or
      PhpFunctionComp(2683) or
      PhpFunctionComp(3358) or
      PhpFunctionComp(3380) or
      PhpFunctionComp(3751) or
      PhpFunctionComp(4116) or
      PhpFunctionComp(4117) or
      PhpFunctionComp(4451) or
      PhpFunctionComp(5135) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc90: TSynWebTokenKind;
begin
  if  PhpFunctionComp(581) or
      PhpFunctionComp(647) or
      PhpFunctionComp(1205) or
      PhpFunctionComp(1364) or
      PhpFunctionComp(1419) or
      PhpFunctionComp(1462) or
      PhpFunctionComp(1509) or
      PhpFunctionComp(1675) or
      PhpFunctionComp(1723) or
      PhpFunctionComp(1733) or
      PhpFunctionComp(1961) or
      PhpFunctionComp(2200) or
      PhpFunctionComp(2202) or
      PhpFunctionComp(2358) or
      PhpFunctionComp(2541) or
      PhpFunctionComp(3178) or
      PhpFunctionComp(3563) or
      PhpFunctionComp(3593) or
      PhpFunctionComp(3625) or
      PhpFunctionComp(3956) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc91: TSynWebTokenKind;
begin
  if  PhpKeywordComp(33) or
      PhpKeywordComp(57) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(249) or
        PhpFunctionComp(291) or
        PhpFunctionComp(504) or
        PhpFunctionComp(1063) or
        PhpFunctionComp(1128) or
        PhpFunctionComp(1258) or
        PhpFunctionComp(1436) or
        PhpFunctionComp(1651) or
        PhpFunctionComp(2077) or
        PhpFunctionComp(2089) or
        PhpFunctionComp(2095) or
        PhpFunctionComp(2420) or
        PhpFunctionComp(2432) or
        PhpFunctionComp(3217) or
        PhpFunctionComp(3529) or
        PhpFunctionComp(3640) or
        PhpFunctionComp(3689) or
        PhpFunctionComp(3756) or
        PhpFunctionComp(3844) or
        PhpFunctionComp(3901) or
        PhpFunctionComp(4114) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc92: TSynWebTokenKind;
begin
  if  PhpFunctionComp(99) or
      PhpFunctionComp(558) or
      PhpFunctionComp(679) or
      PhpFunctionComp(1224) or
      PhpFunctionComp(1248) or
      PhpFunctionComp(1359) or
      PhpFunctionComp(1398) or
      PhpFunctionComp(2078) or
      PhpFunctionComp(2126) or
      PhpFunctionComp(2158) or
      PhpFunctionComp(2431) or
      PhpFunctionComp(2528) or
      PhpFunctionComp(3172) or
      PhpFunctionComp(3325) or
      PhpFunctionComp(3392) or
      PhpFunctionComp(3528) or
      PhpFunctionComp(3571) or
      PhpFunctionComp(3604) or
      PhpFunctionComp(3688) or
      PhpFunctionComp(3940) or
      PhpFunctionComp(4059) or
      PhpFunctionComp(4118) or
      PhpFunctionComp(4371) or
      PhpFunctionComp(4515) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc93: TSynWebTokenKind;
begin
  if  PhpKeywordComp(60) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(222) or
        PhpFunctionComp(642) or
        PhpFunctionComp(1210) or
        PhpFunctionComp(1352) or
        PhpFunctionComp(1474) or
        PhpFunctionComp(1513) or
        PhpFunctionComp(1796) or
        PhpFunctionComp(1959) or
        PhpFunctionComp(1978) or
        PhpFunctionComp(1997) or
        PhpFunctionComp(2173) or
        PhpFunctionComp(3218) or
        PhpFunctionComp(3319) or
        PhpFunctionComp(3461) or
        PhpFunctionComp(3813) or
        PhpFunctionComp(3909) or
        PhpFunctionComp(4081) or
        PhpFunctionComp(4082) or
        PhpFunctionComp(4101) or
        PhpFunctionComp(4123) or
        PhpFunctionComp(4857) or
        PhpFunctionComp(4859) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc94: TSynWebTokenKind;
begin
  if  PhpFunctionComp(476) or
      PhpFunctionComp(1546) or
      PhpFunctionComp(1880) or
      PhpFunctionComp(2269) or
      PhpFunctionComp(3098) or
      PhpFunctionComp(3430) or
      PhpFunctionComp(3634) or
      PhpFunctionComp(3841) or
      PhpFunctionComp(4230) or
      PhpFunctionComp(4235) or
      PhpFunctionComp(4240) or
      PhpFunctionComp(4804) or
      PhpFunctionComp(4875) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc95: TSynWebTokenKind;
begin
  if  PhpFunctionComp(96) or
      PhpFunctionComp(1092) or
      PhpFunctionComp(1375) or
      PhpFunctionComp(1634) or
      PhpFunctionComp(1739) or
      PhpFunctionComp(1952) or
      PhpFunctionComp(2004) or
      PhpFunctionComp(2132) or
      PhpFunctionComp(2409) or
      PhpFunctionComp(2477) or
      PhpFunctionComp(2552) or
      PhpFunctionComp(3080) or
      PhpFunctionComp(3314) or
      PhpFunctionComp(3696) or
      PhpFunctionComp(4096) or
      PhpFunctionComp(4205) or
      PhpFunctionComp(4514) or
      PhpFunctionComp(4872) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc96: TSynWebTokenKind;
begin
  if  PhpKeywordComp(62) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(395) or
        PhpFunctionComp(658) or
        PhpFunctionComp(1260) or
        PhpFunctionComp(1350) or
        PhpFunctionComp(1362) or
        PhpFunctionComp(1548) or
        PhpFunctionComp(1647) or
        PhpFunctionComp(2348) or
        PhpFunctionComp(3104) or
        PhpFunctionComp(3127) or
        PhpFunctionComp(3424) or
        PhpFunctionComp(3437) or
        PhpFunctionComp(3937) or
        PhpFunctionComp(4122) or
        PhpFunctionComp(4805) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc97: TSynWebTokenKind;
begin
  if  PhpFunctionComp(196) or
      PhpFunctionComp(197) or
      PhpFunctionComp(1360) or
      PhpFunctionComp(2068) or
      PhpFunctionComp(2191) or
      PhpFunctionComp(2224) or
      PhpFunctionComp(2555) or
      PhpFunctionComp(3022) or
      PhpFunctionComp(3205) or
      PhpFunctionComp(3315) or
      PhpFunctionComp(3550) or
      PhpFunctionComp(3650) or
      PhpFunctionComp(3805) or
      PhpFunctionComp(3870) or
      PhpFunctionComp(3903) or
      PhpFunctionComp(3954) or
      PhpFunctionComp(3985) or
      PhpFunctionComp(4094) or
      PhpFunctionComp(4129) or
      PhpFunctionComp(4753) or
      PhpFunctionComp(4763) or
      PhpFunctionComp(4846) or
      PhpFunctionComp(5133) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc98: TSynWebTokenKind;
begin
  if  PhpFunctionComp(318) or
      PhpFunctionComp(330) or
      PhpFunctionComp(480) or
      PhpFunctionComp(1058) or
      PhpFunctionComp(1118) or
      PhpFunctionComp(1259) or
      PhpFunctionComp(1372) or
      PhpFunctionComp(1433) or
      PhpFunctionComp(1689) or
      PhpFunctionComp(1728) or
      PhpFunctionComp(2313) or
      PhpFunctionComp(3206) or
      PhpFunctionComp(3209) or
      PhpFunctionComp(3236) or
      PhpFunctionComp(3468) or
      PhpFunctionComp(3477) or
      PhpFunctionComp(3739) or
      PhpFunctionComp(3840) or
      PhpFunctionComp(4379) or
      PhpFunctionComp(4798) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc99: TSynWebTokenKind;
begin
  if  PhpFunctionComp(134) or
      PhpFunctionComp(517) or
      PhpFunctionComp(1089) or
      PhpFunctionComp(1211) or
      PhpFunctionComp(1358) or
      PhpFunctionComp(1368) or
      PhpFunctionComp(1421) or
      PhpFunctionComp(1449) or
      PhpFunctionComp(1649) or
      PhpFunctionComp(1729) or
      PhpFunctionComp(1975) or
      PhpFunctionComp(1977) or
      PhpFunctionComp(1999) or
      PhpFunctionComp(3594) or
      PhpFunctionComp(4091) or
      PhpFunctionComp(4125) or
      PhpFunctionComp(4452) or
      PhpFunctionComp(4516) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc100: TSynWebTokenKind;
begin
  if  PhpFunctionComp(230) or
      PhpFunctionComp(1101) or
      PhpFunctionComp(1264) or
      PhpFunctionComp(1367) or
      PhpFunctionComp(1440) or
      PhpFunctionComp(1505) or
      PhpFunctionComp(2085) or
      PhpFunctionComp(3092) or
      PhpFunctionComp(3164) or
      PhpFunctionComp(3220) or
      PhpFunctionComp(3590) or
      PhpFunctionComp(3808) or
      PhpFunctionComp(3812) or
      PhpFunctionComp(4767) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc101: TSynWebTokenKind;
begin
  if  PhpKeywordComp(16) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(553) or
        PhpFunctionComp(663) or
        PhpFunctionComp(758) or
        PhpFunctionComp(1323) or
        PhpFunctionComp(1376) or
        PhpFunctionComp(1431) or
        PhpFunctionComp(1465) or
        PhpFunctionComp(1682) or
        PhpFunctionComp(1811) or
        PhpFunctionComp(2144) or
        PhpFunctionComp(3093) or
        PhpFunctionComp(3095) or
        PhpFunctionComp(3143) or
        PhpFunctionComp(3166) or
        PhpFunctionComp(3188) or
        PhpFunctionComp(3458) or
        PhpFunctionComp(3641) or
        PhpFunctionComp(3746) or
        PhpFunctionComp(3807) or
        PhpFunctionComp(3951) or
        PhpFunctionComp(4264) or
        PhpFunctionComp(4351) or
        PhpFunctionComp(4754) or
        PhpFunctionComp(5137) or
        PhpFunctionComp(5152) or
        PhpFunctionComp(5167) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc102: TSynWebTokenKind;
begin
  if  PhpKeywordComp(2) or
      PhpKeywordComp(39) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(212) or
        PhpFunctionComp(331) or
        PhpFunctionComp(550) or
        PhpFunctionComp(765) or
        PhpFunctionComp(1380) or
        PhpFunctionComp(1810) or
        PhpFunctionComp(1862) or
        PhpFunctionComp(2201) or
        PhpFunctionComp(2253) or
        PhpFunctionComp(2479) or
        PhpFunctionComp(2543) or
        PhpFunctionComp(3361) or
        PhpFunctionComp(3510) or
        PhpFunctionComp(3587) or
        PhpFunctionComp(3605) or
        PhpFunctionComp(3732) or
        PhpFunctionComp(3938) or
        PhpFunctionComp(4023) or
        PhpFunctionComp(4058) or
        PhpFunctionComp(4246) or
        PhpFunctionComp(4283) or
        PhpFunctionComp(4297) or
        PhpFunctionComp(4368) or
        PhpFunctionComp(4505) or
        PhpFunctionComp(4867) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc103: TSynWebTokenKind;
begin
  if  PhpFunctionComp(351) or
      PhpFunctionComp(632) or
      PhpFunctionComp(643) or
      PhpFunctionComp(675) or
      PhpFunctionComp(688) or
      PhpFunctionComp(769) or
      PhpFunctionComp(790) or
      PhpFunctionComp(1250) or
      PhpFunctionComp(1336) or
      PhpFunctionComp(1427) or
      PhpFunctionComp(1463) or
      PhpFunctionComp(1515) or
      PhpFunctionComp(1688) or
      PhpFunctionComp(1791) or
      PhpFunctionComp(1808) or
      PhpFunctionComp(2076) or
      PhpFunctionComp(2119) or
      PhpFunctionComp(2147) or
      PhpFunctionComp(2160) or
      PhpFunctionComp(2435) or
      PhpFunctionComp(2484) or
      PhpFunctionComp(3025) or
      PhpFunctionComp(3106) or
      PhpFunctionComp(3126) or
      PhpFunctionComp(3190) or
      PhpFunctionComp(3202) or
      PhpFunctionComp(3230) or
      PhpFunctionComp(3585) or
      PhpFunctionComp(3592) or
      PhpFunctionComp(3763) or
      PhpFunctionComp(4500) or
      PhpFunctionComp(4510) or
      PhpFunctionComp(4752) or
      PhpFunctionComp(4806) or
      PhpFunctionComp(4902) or
      PhpFunctionComp(4999) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc104: TSynWebTokenKind;
begin
  if  PhpFunctionComp(429) or
      PhpFunctionComp(582) or
      PhpFunctionComp(644) or
      PhpFunctionComp(706) or
      PhpFunctionComp(711) or
      PhpFunctionComp(759) or
      PhpFunctionComp(760) or
      PhpFunctionComp(1094) or
      PhpFunctionComp(1262) or
      PhpFunctionComp(1297) or
      PhpFunctionComp(1342) or
      PhpFunctionComp(1373) or
      PhpFunctionComp(1422) or
      PhpFunctionComp(1583) or
      PhpFunctionComp(1740) or
      PhpFunctionComp(1807) or
      PhpFunctionComp(1909) or
      PhpFunctionComp(2154) or
      PhpFunctionComp(2375) or
      PhpFunctionComp(2411) or
      PhpFunctionComp(3005) or
      PhpFunctionComp(3118) or
      PhpFunctionComp(3182) or
      PhpFunctionComp(3422) or
      PhpFunctionComp(3586) or
      PhpFunctionComp(3948) or
      PhpFunctionComp(4025) or
      PhpFunctionComp(4501) or
      PhpFunctionComp(4504) or
      PhpFunctionComp(4837) or
      PhpFunctionComp(4876) or
      PhpFunctionComp(4953) or
      PhpFunctionComp(5151) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc105: TSynWebTokenKind;
begin
  if  PhpKeywordComp(12) or
      PhpKeywordComp(28) or
      PhpKeywordComp(44) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(464) or
        PhpFunctionComp(641) or
        PhpFunctionComp(781) or
        PhpFunctionComp(1122) or
        PhpFunctionComp(1129) or
        PhpFunctionComp(1225) or
        PhpFunctionComp(1355) or
        PhpFunctionComp(1418) or
        PhpFunctionComp(1586) or
        PhpFunctionComp(1602) or
        PhpFunctionComp(1725) or
        PhpFunctionComp(1901) or
        PhpFunctionComp(1906) or
        PhpFunctionComp(2188) or
        PhpFunctionComp(2488) or
        PhpFunctionComp(2539) or
        PhpFunctionComp(2562) or
        PhpFunctionComp(3073) or
        PhpFunctionComp(3129) or
        PhpFunctionComp(3473) or
        PhpFunctionComp(3483) or
        PhpFunctionComp(3559) or
        PhpFunctionComp(3624) or
        PhpFunctionComp(4920) or
        PhpFunctionComp(5118) or
        PhpFunctionComp(5123) or
        PhpFunctionComp(5142) or
        PhpFunctionComp(5159) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc106: TSynWebTokenKind;
begin
  if  PhpKeywordComp(45) or
      PhpKeywordComp(58) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(214) or
        PhpFunctionComp(403) or
        PhpFunctionComp(506) or
        PhpFunctionComp(651) or
        PhpFunctionComp(798) or
        PhpFunctionComp(1444) or
        PhpFunctionComp(1455) or
        PhpFunctionComp(2485) or
        PhpFunctionComp(2618) or
        PhpFunctionComp(3066) or
        PhpFunctionComp(3186) or
        PhpFunctionComp(3562) or
        PhpFunctionComp(3692) or
        PhpFunctionComp(3729) or
        PhpFunctionComp(4131) or
        PhpFunctionComp(4233) or
        PhpFunctionComp(4238) or
        PhpFunctionComp(4243) or
        PhpFunctionComp(4298) or
        PhpFunctionComp(4508) or
        PhpFunctionComp(4960) or
        PhpFunctionComp(5121) or
        PhpFunctionComp(5138) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc107: TSynWebTokenKind;
begin
  if  PhpFunctionComp(162) or
      PhpFunctionComp(386) or
      PhpFunctionComp(490) or
      PhpFunctionComp(551) or
      PhpFunctionComp(717) or
      PhpFunctionComp(1249) or
      PhpFunctionComp(1275) or
      PhpFunctionComp(1426) or
      PhpFunctionComp(1529) or
      PhpFunctionComp(1600) or
      PhpFunctionComp(1652) or
      PhpFunctionComp(1783) or
      PhpFunctionComp(1985) or
      PhpFunctionComp(2055) or
      PhpFunctionComp(2155) or
      PhpFunctionComp(2161) or
      PhpFunctionComp(2265) or
      PhpFunctionComp(2273) or
      PhpFunctionComp(2448) or
      PhpFunctionComp(2833) or
      PhpFunctionComp(3007) or
      PhpFunctionComp(3029) or
      PhpFunctionComp(3154) or
      PhpFunctionComp(3212) or
      PhpFunctionComp(3304) or
      PhpFunctionComp(3381) or
      PhpFunctionComp(3572) or
      PhpFunctionComp(3781) or
      PhpFunctionComp(3952) or
      PhpFunctionComp(4358) or
      PhpFunctionComp(4502) or
      PhpFunctionComp(4812) or
      PhpFunctionComp(4863) or
      PhpFunctionComp(4874) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc108: TSynWebTokenKind;
begin
  if  PhpFunctionComp(440) or
      PhpFunctionComp(500) or
      PhpFunctionComp(650) or
      PhpFunctionComp(766) or
      PhpFunctionComp(1095) or
      PhpFunctionComp(1105) or
      PhpFunctionComp(1261) or
      PhpFunctionComp(1379) or
      PhpFunctionComp(1430) or
      PhpFunctionComp(1721) or
      PhpFunctionComp(1769) or
      PhpFunctionComp(1954) or
      PhpFunctionComp(3219) or
      PhpFunctionComp(3308) or
      PhpFunctionComp(3321) or
      PhpFunctionComp(3467) or
      PhpFunctionComp(3582) or
      PhpFunctionComp(3846) or
      PhpFunctionComp(4778) or
      PhpFunctionComp(4818) or
      PhpFunctionComp(4981) or
      PhpFunctionComp(5058) or
      PhpFunctionComp(5119) or
      PhpFunctionComp(5130) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc109: TSynWebTokenKind;
begin
  if  PhpFunctionComp(238) or
      PhpFunctionComp(656) or
      PhpFunctionComp(748) or
      PhpFunctionComp(939) or
      PhpFunctionComp(1152) or
      PhpFunctionComp(1213) or
      PhpFunctionComp(1246) or
      PhpFunctionComp(1683) or
      PhpFunctionComp(1734) or
      PhpFunctionComp(1792) or
      PhpFunctionComp(2117) or
      PhpFunctionComp(2250) or
      PhpFunctionComp(2258) or
      PhpFunctionComp(2272) or
      PhpFunctionComp(2532) or
      PhpFunctionComp(2651) or
      PhpFunctionComp(2685) or
      PhpFunctionComp(3400) or
      PhpFunctionComp(3512) or
      PhpFunctionComp(3607) or
      PhpFunctionComp(4217) or
      PhpFunctionComp(4234) or
      PhpFunctionComp(4239) or
      PhpFunctionComp(4244) or
      PhpFunctionComp(4453) or
      PhpFunctionComp(4878) or
      PhpFunctionComp(4922) or
      PhpFunctionComp(5084) or
      PhpFunctionComp(5107) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc110: TSynWebTokenKind;
begin
  if  PhpFunctionComp(227) or
      PhpFunctionComp(247) or
      PhpFunctionComp(434) or
      PhpFunctionComp(1017) or
      PhpFunctionComp(1109) or
      PhpFunctionComp(1145) or
      PhpFunctionComp(1450) or
      PhpFunctionComp(1470) or
      PhpFunctionComp(1659) or
      PhpFunctionComp(1678) or
      PhpFunctionComp(1736) or
      PhpFunctionComp(1762) or
      PhpFunctionComp(2319) or
      PhpFunctionComp(2410) or
      PhpFunctionComp(3096) or
      PhpFunctionComp(3115) or
      PhpFunctionComp(3149) or
      PhpFunctionComp(3167) or
      PhpFunctionComp(3351) or
      PhpFunctionComp(3360) or
      PhpFunctionComp(3390) or
      PhpFunctionComp(3425) or
      PhpFunctionComp(3772) or
      PhpFunctionComp(3823) or
      PhpFunctionComp(4061) or
      PhpFunctionComp(4445) or
      PhpFunctionComp(4490) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc111: TSynWebTokenKind;
begin
  if  PhpKeywordComp(31) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(126) or
        PhpFunctionComp(223) or
        PhpFunctionComp(235) or
        PhpFunctionComp(425) or
        PhpFunctionComp(575) or
        PhpFunctionComp(640) or
        PhpFunctionComp(682) or
        PhpFunctionComp(1228) or
        PhpFunctionComp(1468) or
        PhpFunctionComp(1731) or
        PhpFunctionComp(2001) or
        PhpFunctionComp(2087) or
        PhpFunctionComp(2102) or
        PhpFunctionComp(2213) or
        PhpFunctionComp(2240) or
        PhpFunctionComp(2551) or
        PhpFunctionComp(3038) or
        PhpFunctionComp(3383) or
        PhpFunctionComp(3386) or
        PhpFunctionComp(3469) or
        PhpFunctionComp(3795) or
        PhpFunctionComp(4021) or
        PhpFunctionComp(4758) or
        PhpFunctionComp(4891) or
        PhpFunctionComp(4901) or
        PhpFunctionComp(4923) or
        PhpFunctionComp(5146) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc112: TSynWebTokenKind;
begin
  if  PhpFunctionComp(388) or
      PhpFunctionComp(465) or
      PhpFunctionComp(654) or
      PhpFunctionComp(1691) or
      PhpFunctionComp(1693) or
      PhpFunctionComp(1992) or
      PhpFunctionComp(2047) or
      PhpFunctionComp(3067) or
      PhpFunctionComp(3075) or
      PhpFunctionComp(3114) or
      PhpFunctionComp(3856) or
      PhpFunctionComp(3917) or
      PhpFunctionComp(4128) or
      PhpFunctionComp(4387) or
      PhpFunctionComp(4819) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc113: TSynWebTokenKind;
begin
  if  PhpFunctionComp(319) or
      PhpFunctionComp(497) or
      PhpFunctionComp(629) or
      PhpFunctionComp(1797) or
      PhpFunctionComp(1802) or
      PhpFunctionComp(1899) or
      PhpFunctionComp(1924) or
      PhpFunctionComp(2015) or
      PhpFunctionComp(2016) or
      PhpFunctionComp(2082) or
      PhpFunctionComp(2184) or
      PhpFunctionComp(2395) or
      PhpFunctionComp(2529) or
      PhpFunctionComp(3320) or
      PhpFunctionComp(3445) or
      PhpFunctionComp(3459) or
      PhpFunctionComp(3475) or
      PhpFunctionComp(3602) or
      PhpFunctionComp(4032) or
      PhpFunctionComp(4084) or
      PhpFunctionComp(4130) or
      PhpFunctionComp(5147) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc114: TSynWebTokenKind;
begin
  if  PhpFunctionComp(314) or
      PhpFunctionComp(524) or
      PhpFunctionComp(578) or
      PhpFunctionComp(1150) or
      PhpFunctionComp(1452) or
      PhpFunctionComp(1497) or
      PhpFunctionComp(1933) or
      PhpFunctionComp(1951) or
      PhpFunctionComp(2062) or
      PhpFunctionComp(2097) or
      PhpFunctionComp(2228) or
      PhpFunctionComp(2327) or
      PhpFunctionComp(2368) or
      PhpFunctionComp(2442) or
      PhpFunctionComp(2530) or
      PhpFunctionComp(3229) or
      PhpFunctionComp(3410) or
      PhpFunctionComp(3436) or
      PhpFunctionComp(3471) or
      PhpFunctionComp(3494) or
      PhpFunctionComp(3534) or
      PhpFunctionComp(3784) or
      PhpFunctionComp(3842) or
      PhpFunctionComp(4143) or
      PhpFunctionComp(4225) or
      PhpFunctionComp(4509) or
      PhpFunctionComp(4823) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc115: TSynWebTokenKind;
begin
  if  PhpFunctionComp(142) or
      PhpFunctionComp(530) or
      PhpFunctionComp(703) or
      PhpFunctionComp(1156) or
      PhpFunctionComp(1184) or
      PhpFunctionComp(1365) or
      PhpFunctionComp(1521) or
      PhpFunctionComp(1633) or
      PhpFunctionComp(1759) or
      PhpFunctionComp(2107) or
      PhpFunctionComp(2229) or
      PhpFunctionComp(2624) or
      PhpFunctionComp(3004) or
      PhpFunctionComp(3039) or
      PhpFunctionComp(3071) or
      PhpFunctionComp(3072) or
      PhpFunctionComp(3116) or
      PhpFunctionComp(3128) or
      PhpFunctionComp(3362) or
      PhpFunctionComp(3399) or
      PhpFunctionComp(4024) or
      PhpFunctionComp(4270) or
      PhpFunctionComp(4955) or
      PhpFunctionComp(5134) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc116: TSynWebTokenKind;
begin
  if  PhpFunctionComp(141) or
      PhpFunctionComp(237) or
      PhpFunctionComp(431) or
      PhpFunctionComp(583) or
      PhpFunctionComp(712) or
      PhpFunctionComp(764) or
      PhpFunctionComp(1185) or
      PhpFunctionComp(1331) or
      PhpFunctionComp(1351) or
      PhpFunctionComp(1366) or
      PhpFunctionComp(1417) or
      PhpFunctionComp(1667) or
      PhpFunctionComp(1741) or
      PhpFunctionComp(1746) or
      PhpFunctionComp(1897) or
      PhpFunctionComp(2064) or
      PhpFunctionComp(2122) or
      PhpFunctionComp(2127) or
      PhpFunctionComp(2433) or
      PhpFunctionComp(3082) or
      PhpFunctionComp(3147) or
      PhpFunctionComp(3350) or
      PhpFunctionComp(3442) or
      PhpFunctionComp(3463) or
      PhpFunctionComp(3690) or
      PhpFunctionComp(3740) or
      PhpFunctionComp(3847) or
      PhpFunctionComp(4372) or
      PhpFunctionComp(4493) or
      PhpFunctionComp(5080) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc117: TSynWebTokenKind;
begin
  if  PhpFunctionComp(75) or
      PhpFunctionComp(108) or
      PhpFunctionComp(233) or
      PhpFunctionComp(479) or
      PhpFunctionComp(536) or
      PhpFunctionComp(1059) or
      PhpFunctionComp(1200) or
      PhpFunctionComp(1409) or
      PhpFunctionComp(1420) or
      PhpFunctionComp(1491) or
      PhpFunctionComp(1543) or
      PhpFunctionComp(1547) or
      PhpFunctionComp(1710) or
      PhpFunctionComp(1732) or
      PhpFunctionComp(2118) or
      PhpFunctionComp(2123) or
      PhpFunctionComp(2247) or
      PhpFunctionComp(2315) or
      PhpFunctionComp(2524) or
      PhpFunctionComp(2544) or
      PhpFunctionComp(3141) or
      PhpFunctionComp(3423) or
      PhpFunctionComp(3441) or
      PhpFunctionComp(3476) or
      PhpFunctionComp(3556) or
      PhpFunctionComp(3777) or
      PhpFunctionComp(3872) or
      PhpFunctionComp(4294) or
      PhpFunctionComp(4444) or
      PhpFunctionComp(4449) or
      PhpFunctionComp(4840) or
      PhpFunctionComp(4853) or
      PhpFunctionComp(4879) or
      PhpFunctionComp(4889) or
      PhpFunctionComp(5149) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc118: TSynWebTokenKind;
begin
  if  PhpFunctionComp(492) or
      PhpFunctionComp(725) or
      PhpFunctionComp(1104) or
      PhpFunctionComp(1279) or
      PhpFunctionComp(1405) or
      PhpFunctionComp(1520) or
      PhpFunctionComp(1613) or
      PhpFunctionComp(1818) or
      PhpFunctionComp(2110) or
      PhpFunctionComp(2316) or
      PhpFunctionComp(2328) or
      PhpFunctionComp(2364) or
      PhpFunctionComp(2519) or
      PhpFunctionComp(2523) or
      PhpFunctionComp(2534) or
      PhpFunctionComp(3207) or
      PhpFunctionComp(3366) or
      PhpFunctionComp(3388) or
      PhpFunctionComp(3574) or
      PhpFunctionComp(3631) or
      PhpFunctionComp(3635) or
      PhpFunctionComp(3796) or
      PhpFunctionComp(4086) or
      PhpFunctionComp(4120) or
      PhpFunctionComp(4127) or
      PhpFunctionComp(4893) or
      PhpFunctionComp(4952) or
      PhpFunctionComp(5090) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc119: TSynWebTokenKind;
begin
  if  PhpFunctionComp(189) or
      PhpFunctionComp(231) or
      PhpFunctionComp(313) or
      PhpFunctionComp(368) or
      PhpFunctionComp(430) or
      PhpFunctionComp(493) or
      PhpFunctionComp(796) or
      PhpFunctionComp(955) or
      PhpFunctionComp(1208) or
      PhpFunctionComp(1727) or
      PhpFunctionComp(1742) or
      PhpFunctionComp(1788) or
      PhpFunctionComp(1865) or
      PhpFunctionComp(2050) or
      PhpFunctionComp(2241) or
      PhpFunctionComp(2419) or
      PhpFunctionComp(2626) or
      PhpFunctionComp(2834) or
      PhpFunctionComp(3016) or
      PhpFunctionComp(3040) or
      PhpFunctionComp(3234) or
      PhpFunctionComp(3438) or
      PhpFunctionComp(3455) or
      PhpFunctionComp(4100) or
      PhpFunctionComp(4211) or
      PhpFunctionComp(4309) or
      PhpFunctionComp(4605) or
      PhpFunctionComp(4821) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc120: TSynWebTokenKind;
begin
  if  PhpFunctionComp(204) or
      PhpFunctionComp(604) or
      PhpFunctionComp(677) or
      PhpFunctionComp(684) or
      PhpFunctionComp(1519) or
      PhpFunctionComp(1526) or
      PhpFunctionComp(1718) or
      PhpFunctionComp(1932) or
      PhpFunctionComp(1934) or
      PhpFunctionComp(1947) or
      PhpFunctionComp(2338) or
      PhpFunctionComp(2629) or
      PhpFunctionComp(2686) or
      PhpFunctionComp(3379) or
      PhpFunctionComp(3545) or
      PhpFunctionComp(3554) or
      PhpFunctionComp(3567) or
      PhpFunctionComp(3606) or
      PhpFunctionComp(3742) or
      PhpFunctionComp(3745) or
      PhpFunctionComp(3778) or
      PhpFunctionComp(4011) or
      PhpFunctionComp(4344) or
      PhpFunctionComp(4374) or
      PhpFunctionComp(4503) or
      PhpFunctionComp(4567) or
      PhpFunctionComp(4577) or
      PhpFunctionComp(4882) or
      PhpFunctionComp(5060) or
      PhpFunctionComp(5081) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc121: TSynWebTokenKind;
begin
  if  PhpFunctionComp(130) or
      PhpFunctionComp(280) or
      PhpFunctionComp(422) or
      PhpFunctionComp(596) or
      PhpFunctionComp(793) or
      PhpFunctionComp(1232) or
      PhpFunctionComp(1363) or
      PhpFunctionComp(1381) or
      PhpFunctionComp(1646) or
      PhpFunctionComp(1726) or
      PhpFunctionComp(1800) or
      PhpFunctionComp(1859) or
      PhpFunctionComp(2469) or
      PhpFunctionComp(2535) or
      PhpFunctionComp(3204) or
      PhpFunctionComp(3215) or
      PhpFunctionComp(3376) or
      PhpFunctionComp(3418) or
      PhpFunctionComp(3478) or
      PhpFunctionComp(3514) or
      PhpFunctionComp(3569) or
      PhpFunctionComp(3817) or
      PhpFunctionComp(3850) or
      PhpFunctionComp(3932) or
      PhpFunctionComp(4090) or
      PhpFunctionComp(4245) or
      PhpFunctionComp(4266) or
      PhpFunctionComp(4669) or
      PhpFunctionComp(4868) or
      PhpFunctionComp(5086) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc122: TSynWebTokenKind;
begin
  if  PhpFunctionComp(199) or
      PhpFunctionComp(409) or
      PhpFunctionComp(438) or
      PhpFunctionComp(485) or
      PhpFunctionComp(747) or
      PhpFunctionComp(767) or
      PhpFunctionComp(1151) or
      PhpFunctionComp(1760) or
      PhpFunctionComp(1768) or
      PhpFunctionComp(1799) or
      PhpFunctionComp(1915) or
      PhpFunctionComp(1920) or
      PhpFunctionComp(1940) or
      PhpFunctionComp(1967) or
      PhpFunctionComp(2009) or
      PhpFunctionComp(2331) or
      PhpFunctionComp(2436) or
      PhpFunctionComp(2525) or
      PhpFunctionComp(2558) or
      PhpFunctionComp(2574) or
      PhpFunctionComp(2585) or
      PhpFunctionComp(2868) or
      PhpFunctionComp(3107) or
      PhpFunctionComp(3120) or
      PhpFunctionComp(3142) or
      PhpFunctionComp(3187) or
      PhpFunctionComp(3216) or
      PhpFunctionComp(3357) or
      PhpFunctionComp(3413) or
      PhpFunctionComp(3501) or
      PhpFunctionComp(3566) or
      PhpFunctionComp(3694) or
      PhpFunctionComp(3834) or
      PhpFunctionComp(4089) or
      PhpFunctionComp(4115) or
      PhpFunctionComp(4142) or
      PhpFunctionComp(4331) or
      PhpFunctionComp(4443) or
      PhpFunctionComp(4555) or
      PhpFunctionComp(4586) or
      PhpFunctionComp(4797) or
      PhpFunctionComp(4811) or
      PhpFunctionComp(5000) or
      PhpFunctionComp(5076) or
      PhpFunctionComp(5125) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc123: TSynWebTokenKind;
begin
  if  PhpFunctionComp(221) or
      PhpFunctionComp(594) or
      PhpFunctionComp(707) or
      PhpFunctionComp(1066) or
      PhpFunctionComp(1166) or
      PhpFunctionComp(1607) or
      PhpFunctionComp(1630) or
      PhpFunctionComp(1668) or
      PhpFunctionComp(1751) or
      PhpFunctionComp(1812) or
      PhpFunctionComp(2121) or
      PhpFunctionComp(2124) or
      PhpFunctionComp(2359) or
      PhpFunctionComp(2480) or
      PhpFunctionComp(2845) or
      PhpFunctionComp(3349) or
      PhpFunctionComp(3541) or
      PhpFunctionComp(3766) or
      PhpFunctionComp(3790) or
      PhpFunctionComp(3816) or
      PhpFunctionComp(3869) or
      PhpFunctionComp(3928) or
      PhpFunctionComp(3931) or
      PhpFunctionComp(3953) or
      PhpFunctionComp(4067) or
      PhpFunctionComp(4144) or
      PhpFunctionComp(4201) or
      PhpFunctionComp(4265) or
      PhpFunctionComp(4318) or
      PhpFunctionComp(4495) or
      PhpFunctionComp(4584) or
      PhpFunctionComp(4585) or
      PhpFunctionComp(4887) or
      PhpFunctionComp(5068) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc124: TSynWebTokenKind;
begin
  if  PhpFunctionComp(205) or
      PhpFunctionComp(268) or
      PhpFunctionComp(383) or
      PhpFunctionComp(466) or
      PhpFunctionComp(502) or
      PhpFunctionComp(525) or
      PhpFunctionComp(580) or
      PhpFunctionComp(680) or
      PhpFunctionComp(729) or
      PhpFunctionComp(1446) or
      PhpFunctionComp(1453) or
      PhpFunctionComp(1582) or
      PhpFunctionComp(1669) or
      PhpFunctionComp(1945) or
      PhpFunctionComp(1974) or
      PhpFunctionComp(2271) or
      PhpFunctionComp(2449) or
      PhpFunctionComp(2482) or
      PhpFunctionComp(3101) or
      PhpFunctionComp(3183) or
      PhpFunctionComp(3214) or
      PhpFunctionComp(3378) or
      PhpFunctionComp(3391) or
      PhpFunctionComp(3632) or
      PhpFunctionComp(3691) or
      PhpFunctionComp(3752) or
      PhpFunctionComp(3929) or
      PhpFunctionComp(4498) or
      PhpFunctionComp(4589) or
      PhpFunctionComp(4592) or
      PhpFunctionComp(4593) or
      PhpFunctionComp(4884) or
      PhpFunctionComp(4921) or
      PhpFunctionComp(4925) or
      PhpFunctionComp(5140) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc125: TSynWebTokenKind;
begin
  if  PhpFunctionComp(140) or
      PhpFunctionComp(234) or
      PhpFunctionComp(277) or
      PhpFunctionComp(454) or
      PhpFunctionComp(1098) or
      PhpFunctionComp(1167) or
      PhpFunctionComp(1819) or
      PhpFunctionComp(1939) or
      PhpFunctionComp(2007) or
      PhpFunctionComp(2140) or
      PhpFunctionComp(2176) or
      PhpFunctionComp(2540) or
      PhpFunctionComp(2921) or
      PhpFunctionComp(3009) or
      PhpFunctionComp(3102) or
      PhpFunctionComp(3150) or
      PhpFunctionComp(3402) or
      PhpFunctionComp(3491) or
      PhpFunctionComp(3513) or
      PhpFunctionComp(3560) or
      PhpFunctionComp(3575) or
      PhpFunctionComp(3596) or
      PhpFunctionComp(3626) or
      PhpFunctionComp(3767) or
      PhpFunctionComp(3774) or
      PhpFunctionComp(3927) or
      PhpFunctionComp(4088) or
      PhpFunctionComp(4132) or
      PhpFunctionComp(4250) or
      PhpFunctionComp(4290) or
      PhpFunctionComp(4507) or
      PhpFunctionComp(4728) or
      PhpFunctionComp(4761) or
      PhpFunctionComp(4970) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc126: TSynWebTokenKind;
begin
  if  PhpKeywordComp(42) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(131) or
        PhpFunctionComp(260) or
        PhpFunctionComp(332) or
        PhpFunctionComp(345) or
        PhpFunctionComp(348) or
        PhpFunctionComp(483) or
        PhpFunctionComp(634) or
        PhpFunctionComp(693) or
        PhpFunctionComp(726) or
        PhpFunctionComp(1157) or
        PhpFunctionComp(1188) or
        PhpFunctionComp(1416) or
        PhpFunctionComp(1758) or
        PhpFunctionComp(1986) or
        PhpFunctionComp(2018) or
        PhpFunctionComp(2063) or
        PhpFunctionComp(2093) or
        PhpFunctionComp(3023) or
        PhpFunctionComp(3100) or
        PhpFunctionComp(3221) or
        PhpFunctionComp(3481) or
        PhpFunctionComp(3598) or
        PhpFunctionComp(3866) or
        PhpFunctionComp(3877) or
        PhpFunctionComp(3878) or
        PhpFunctionComp(4102) or
        PhpFunctionComp(4162) or
        PhpFunctionComp(4357) or
        PhpFunctionComp(4441) or
        PhpFunctionComp(4862) or
        PhpFunctionComp(4924) or
        PhpFunctionComp(5122) or
        PhpFunctionComp(5124) or
        PhpFunctionComp(5126) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc127: TSynWebTokenKind;
begin
  if  PhpFunctionComp(148) or
      PhpFunctionComp(229) or
      PhpFunctionComp(236) or
      PhpFunctionComp(1785) or
      PhpFunctionComp(1966) or
      PhpFunctionComp(2079) or
      PhpFunctionComp(2284) or
      PhpFunctionComp(2286) or
      PhpFunctionComp(2567) or
      PhpFunctionComp(2627) or
      PhpFunctionComp(2844) or
      PhpFunctionComp(3051) or
      PhpFunctionComp(3169) or
      PhpFunctionComp(3227) or
      PhpFunctionComp(3365) or
      PhpFunctionComp(3427) or
      PhpFunctionComp(3674) or
      PhpFunctionComp(4248) or
      PhpFunctionComp(4380) or
      PhpFunctionComp(4396) or
      PhpFunctionComp(4522) or
      PhpFunctionComp(4601) or
      PhpFunctionComp(4759) or
      PhpFunctionComp(4898) or
      PhpFunctionComp(4958) or
      PhpFunctionComp(5104) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc128: TSynWebTokenKind;
begin
  if  PhpFunctionComp(112) or
      PhpFunctionComp(202) or
      PhpFunctionComp(393) or
      PhpFunctionComp(417) or
      PhpFunctionComp(449) or
      PhpFunctionComp(503) or
      PhpFunctionComp(549) or
      PhpFunctionComp(600) or
      PhpFunctionComp(602) or
      PhpFunctionComp(713) or
      PhpFunctionComp(750) or
      PhpFunctionComp(763) or
      PhpFunctionComp(1241) or
      PhpFunctionComp(1273) or
      PhpFunctionComp(1290) or
      PhpFunctionComp(1472) or
      PhpFunctionComp(2083) or
      PhpFunctionComp(2142) or
      PhpFunctionComp(2227) or
      PhpFunctionComp(2636) or
      PhpFunctionComp(3155) or
      PhpFunctionComp(3565) or
      PhpFunctionComp(3769) or
      PhpFunctionComp(4523) or
      PhpFunctionComp(4808) or
      PhpFunctionComp(4877) or
      PhpFunctionComp(4888) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc129: TSynWebTokenKind;
begin
  if  PhpFunctionComp(209) or
      PhpFunctionComp(437) or
      PhpFunctionComp(563) or
      PhpFunctionComp(657) or
      PhpFunctionComp(1146) or
      PhpFunctionComp(1195) or
      PhpFunctionComp(1757) or
      PhpFunctionComp(1798) or
      PhpFunctionComp(1817) or
      PhpFunctionComp(1982) or
      PhpFunctionComp(2339) or
      PhpFunctionComp(2450) or
      PhpFunctionComp(3108) or
      PhpFunctionComp(3125) or
      PhpFunctionComp(3156) or
      PhpFunctionComp(3213) or
      PhpFunctionComp(3905) or
      PhpFunctionComp(4491) or
      PhpFunctionComp(4883) or
      PhpFunctionComp(4886) or
      PhpFunctionComp(4896) or
      PhpFunctionComp(4962) or
      PhpFunctionComp(4963) or
      PhpFunctionComp(5120) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc130: TSynWebTokenKind;
begin
  if  PhpKeywordComp(61) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(303) or
        PhpFunctionComp(489) or
        PhpFunctionComp(505) or
        PhpFunctionComp(561) or
        PhpFunctionComp(662) or
        PhpFunctionComp(789) or
        PhpFunctionComp(797) or
        PhpFunctionComp(1147) or
        PhpFunctionComp(1158) or
        PhpFunctionComp(1187) or
        PhpFunctionComp(1327) or
        PhpFunctionComp(1432) or
        PhpFunctionComp(1475) or
        PhpFunctionComp(1661) or
        PhpFunctionComp(1910) or
        PhpFunctionComp(2262) or
        PhpFunctionComp(2360) or
        PhpFunctionComp(2447) or
        PhpFunctionComp(2637) or
        PhpFunctionComp(2717) or
        PhpFunctionComp(2866) or
        PhpFunctionComp(3094) or
        PhpFunctionComp(3137) or
        PhpFunctionComp(3165) or
        PhpFunctionComp(3415) or
        PhpFunctionComp(3416) or
        PhpFunctionComp(3517) or
        PhpFunctionComp(3539) or
        PhpFunctionComp(3629) or
        PhpFunctionComp(3736) or
        PhpFunctionComp(4020) or
        PhpFunctionComp(4092) or
        PhpFunctionComp(4336) or
        PhpFunctionComp(4591) or
        PhpFunctionComp(5072) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc131: TSynWebTokenKind;
begin
  if  PhpFunctionComp(118) or
      PhpFunctionComp(312) or
      PhpFunctionComp(1082) or
      PhpFunctionComp(1097) or
      PhpFunctionComp(1223) or
      PhpFunctionComp(1553) or
      PhpFunctionComp(1584) or
      PhpFunctionComp(1964) or
      PhpFunctionComp(2056) or
      PhpFunctionComp(2070) or
      PhpFunctionComp(2159) or
      PhpFunctionComp(2427) or
      PhpFunctionComp(2650) or
      PhpFunctionComp(2684) or
      PhpFunctionComp(2862) or
      PhpFunctionComp(3010) or
      PhpFunctionComp(3031) or
      PhpFunctionComp(3042) or
      PhpFunctionComp(3131) or
      PhpFunctionComp(3208) or
      PhpFunctionComp(3225) or
      PhpFunctionComp(3316) or
      PhpFunctionComp(3389) or
      PhpFunctionComp(3466) or
      PhpFunctionComp(3507) or
      PhpFunctionComp(3595) or
      PhpFunctionComp(3600) or
      PhpFunctionComp(4126) or
      PhpFunctionComp(4499) or
      PhpFunctionComp(4532) or
      PhpFunctionComp(4571) or
      PhpFunctionComp(4865) or
      PhpFunctionComp(4890) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc132: TSynWebTokenKind;
begin
  if  PhpFunctionComp(76) or
      PhpFunctionComp(198) or
      PhpFunctionComp(544) or
      PhpFunctionComp(565) or
      PhpFunctionComp(705) or
      PhpFunctionComp(799) or
      PhpFunctionComp(802) or
      PhpFunctionComp(1003) or
      PhpFunctionComp(1168) or
      PhpFunctionComp(1169) or
      PhpFunctionComp(1194) or
      PhpFunctionComp(1328) or
      PhpFunctionComp(1386) or
      PhpFunctionComp(1574) or
      PhpFunctionComp(1628) or
      PhpFunctionComp(1963) or
      PhpFunctionComp(2010) or
      PhpFunctionComp(2044) or
      PhpFunctionComp(2105) or
      PhpFunctionComp(2162) or
      PhpFunctionComp(2402) or
      PhpFunctionComp(2405) or
      PhpFunctionComp(2670) or
      PhpFunctionComp(2726) or
      PhpFunctionComp(3044) or
      PhpFunctionComp(3064) or
      PhpFunctionComp(3099) or
      PhpFunctionComp(4202) or
      PhpFunctionComp(4326) or
      PhpFunctionComp(4356) or
      PhpFunctionComp(4369) or
      PhpFunctionComp(4769) or
      PhpFunctionComp(4773) or
      PhpFunctionComp(5002) or
      PhpFunctionComp(5066) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc133: TSynWebTokenKind;
begin
  if  PhpKeywordComp(53) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(213) or
        PhpFunctionComp(357) or
        PhpFunctionComp(421) or
        PhpFunctionComp(469) or
        PhpFunctionComp(1292) or
        PhpFunctionComp(1672) or
        PhpFunctionComp(1717) or
        PhpFunctionComp(1744) or
        PhpFunctionComp(1832) or
        PhpFunctionComp(1981) or
        PhpFunctionComp(1990) or
        PhpFunctionComp(2193) or
        PhpFunctionComp(2885) or
        PhpFunctionComp(3509) or
        PhpFunctionComp(3651) or
        PhpFunctionComp(3653) or
        PhpFunctionComp(3738) or
        PhpFunctionComp(3851) or
        PhpFunctionComp(3883) or
        PhpFunctionComp(4035) or
        PhpFunctionComp(4447) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc134: TSynWebTokenKind;
begin
  if  PhpFunctionComp(411) or
      PhpFunctionComp(695) or
      PhpFunctionComp(714) or
      PhpFunctionComp(1161) or
      PhpFunctionComp(1745) or
      PhpFunctionComp(2141) or
      PhpFunctionComp(2235) or
      PhpFunctionComp(2285) or
      PhpFunctionComp(2652) or
      PhpFunctionComp(2744) or
      PhpFunctionComp(3305) or
      PhpFunctionComp(3312) or
      PhpFunctionComp(3433) or
      PhpFunctionComp(3474) or
      PhpFunctionComp(3555) or
      PhpFunctionComp(3753) or
      PhpFunctionComp(3797) or
      PhpFunctionComp(4073) or
      PhpFunctionComp(4446) or
      PhpFunctionComp(4506) or
      PhpFunctionComp(4568) or
      PhpFunctionComp(4768) or
      PhpFunctionComp(4892) or
      PhpFunctionComp(4927) or
      PhpFunctionComp(4972) or
      PhpFunctionComp(5074) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc135: TSynWebTokenKind;
begin
  if  PhpFunctionComp(80) or
      PhpFunctionComp(146) or
      PhpFunctionComp(304) or
      PhpFunctionComp(341) or
      PhpFunctionComp(413) or
      PhpFunctionComp(598) or
      PhpFunctionComp(601) or
      PhpFunctionComp(653) or
      PhpFunctionComp(730) or
      PhpFunctionComp(743) or
      PhpFunctionComp(928) or
      PhpFunctionComp(1106) or
      PhpFunctionComp(1298) or
      PhpFunctionComp(1516) or
      PhpFunctionComp(1707) or
      PhpFunctionComp(1743) or
      PhpFunctionComp(2131) or
      PhpFunctionComp(2625) or
      PhpFunctionComp(2630) or
      PhpFunctionComp(2832) or
      PhpFunctionComp(3191) or
      PhpFunctionComp(3309) or
      PhpFunctionComp(3382) or
      PhpFunctionComp(3659) or
      PhpFunctionComp(3783) or
      PhpFunctionComp(3849) or
      PhpFunctionComp(3933) or
      PhpFunctionComp(4604) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc136: TSynWebTokenKind;
begin
  if  PhpFunctionComp(117) or
      PhpFunctionComp(328) or
      PhpFunctionComp(379) or
      PhpFunctionComp(387) or
      PhpFunctionComp(491) or
      PhpFunctionComp(538) or
      PhpFunctionComp(573) or
      PhpFunctionComp(627) or
      PhpFunctionComp(1049) or
      PhpFunctionComp(1334) or
      PhpFunctionComp(1761) or
      PhpFunctionComp(1822) or
      PhpFunctionComp(2061) or
      PhpFunctionComp(2150) or
      PhpFunctionComp(2222) or
      PhpFunctionComp(2249) or
      PhpFunctionComp(2259) or
      PhpFunctionComp(2440) or
      PhpFunctionComp(3052) or
      PhpFunctionComp(3153) or
      PhpFunctionComp(3306) or
      PhpFunctionComp(3482) or
      PhpFunctionComp(3536) or
      PhpFunctionComp(3979) or
      PhpFunctionComp(4002) or
      PhpFunctionComp(4028) or
      PhpFunctionComp(4135) or
      PhpFunctionComp(4174) or
      PhpFunctionComp(4256) or
      PhpFunctionComp(4284) or
      PhpFunctionComp(4302) or
      PhpFunctionComp(4775) or
      PhpFunctionComp(5073) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc137: TSynWebTokenKind;
begin
  if  PhpFunctionComp(439) or
      PhpFunctionComp(539) or
      PhpFunctionComp(545) or
      PhpFunctionComp(665) or
      PhpFunctionComp(728) or
      PhpFunctionComp(1029) or
      PhpFunctionComp(1120) or
      PhpFunctionComp(1165) or
      PhpFunctionComp(1382) or
      PhpFunctionComp(1589) or
      PhpFunctionComp(1804) or
      PhpFunctionComp(1821) or
      PhpFunctionComp(2003) or
      PhpFunctionComp(2471) or
      PhpFunctionComp(2581) or
      PhpFunctionComp(2638) or
      PhpFunctionComp(2639) or
      PhpFunctionComp(3210) or
      PhpFunctionComp(3288) or
      PhpFunctionComp(3686) or
      PhpFunctionComp(3770) or
      PhpFunctionComp(3926) or
      PhpFunctionComp(4068) or
      PhpFunctionComp(4269) or
      PhpFunctionComp(4295) or
      PhpFunctionComp(4429) or
      PhpFunctionComp(4573) or
      PhpFunctionComp(4730) or
      PhpFunctionComp(4954) or
      PhpFunctionComp(4973) or
      PhpFunctionComp(5061) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc138: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1025) or
      PhpFunctionComp(1575) or
      PhpFunctionComp(1601) or
      PhpFunctionComp(2109) or
      PhpFunctionComp(2374) or
      PhpFunctionComp(2452) or
      PhpFunctionComp(2542) or
      PhpFunctionComp(2851) or
      PhpFunctionComp(2899) or
      PhpFunctionComp(3041) or
      PhpFunctionComp(3151) or
      PhpFunctionComp(3158) or
      PhpFunctionComp(3401) or
      PhpFunctionComp(3443) or
      PhpFunctionComp(3868) or
      PhpFunctionComp(4085) or
      PhpFunctionComp(4155) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc139: TSynWebTokenKind;
begin
  if  PhpFunctionComp(461) or
      PhpFunctionComp(481) or
      PhpFunctionComp(579) or
      PhpFunctionComp(666) or
      PhpFunctionComp(1148) or
      PhpFunctionComp(1265) or
      PhpFunctionComp(1324) or
      PhpFunctionComp(1396) or
      PhpFunctionComp(1590) or
      PhpFunctionComp(1625) or
      PhpFunctionComp(1754) or
      PhpFunctionComp(1773) or
      PhpFunctionComp(1923) or
      PhpFunctionComp(1931) or
      PhpFunctionComp(2002) or
      PhpFunctionComp(2400) or
      PhpFunctionComp(2446) or
      PhpFunctionComp(2531) or
      PhpFunctionComp(2600) or
      PhpFunctionComp(2632) or
      PhpFunctionComp(2774) or
      PhpFunctionComp(2777) or
      PhpFunctionComp(2850) or
      PhpFunctionComp(2880) or
      PhpFunctionComp(3265) or
      PhpFunctionComp(3758) or
      PhpFunctionComp(3798) or
      PhpFunctionComp(3906) or
      PhpFunctionComp(4301) or
      PhpFunctionComp(4512) or
      PhpFunctionComp(4531) or
      PhpFunctionComp(4536) or
      PhpFunctionComp(4653) or
      PhpFunctionComp(4844) or
      PhpFunctionComp(4873) or
      PhpFunctionComp(4894) or
      PhpFunctionComp(5079) or
      PhpFunctionComp(5153) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc140: TSynWebTokenKind;
begin
  if  PhpFunctionComp(78) or
      PhpFunctionComp(426) or
      PhpFunctionComp(531) or
      PhpFunctionComp(595) or
      PhpFunctionComp(599) or
      PhpFunctionComp(652) or
      PhpFunctionComp(719) or
      PhpFunctionComp(1023) or
      PhpFunctionComp(1149) or
      PhpFunctionComp(1193) or
      PhpFunctionComp(1858) or
      PhpFunctionComp(2398) or
      PhpFunctionComp(2437) or
      PhpFunctionComp(2472) or
      PhpFunctionComp(2579) or
      PhpFunctionComp(2617) or
      PhpFunctionComp(2681) or
      PhpFunctionComp(2691) or
      PhpFunctionComp(3122) or
      PhpFunctionComp(3377) or
      PhpFunctionComp(3407) or
      PhpFunctionComp(3531) or
      PhpFunctionComp(3657) or
      PhpFunctionComp(3676) or
      PhpFunctionComp(3776) or
      PhpFunctionComp(3793) or
      PhpFunctionComp(3988) or
      PhpFunctionComp(4213) or
      PhpFunctionComp(4475) or
      PhpFunctionComp(4537) or
      PhpFunctionComp(4595) or
      PhpFunctionComp(4869) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc141: TSynWebTokenKind;
begin
  if  PhpFunctionComp(147) or
      PhpFunctionComp(519) or
      PhpFunctionComp(569) or
      PhpFunctionComp(576) or
      PhpFunctionComp(585) or
      PhpFunctionComp(754) or
      PhpFunctionComp(1175) or
      PhpFunctionComp(1219) or
      PhpFunctionComp(1314) or
      PhpFunctionComp(1339) or
      PhpFunctionComp(1501) or
      PhpFunctionComp(1605) or
      PhpFunctionComp(1609) or
      PhpFunctionComp(1898) or
      PhpFunctionComp(1918) or
      PhpFunctionComp(2030) or
      PhpFunctionComp(2333) or
      PhpFunctionComp(2335) or
      PhpFunctionComp(2550) or
      PhpFunctionComp(2586) or
      PhpFunctionComp(2785) or
      PhpFunctionComp(2935) or
      PhpFunctionComp(3393) or
      PhpFunctionComp(3429) or
      PhpFunctionComp(3530) or
      PhpFunctionComp(3584) or
      PhpFunctionComp(3589) or
      PhpFunctionComp(4124) or
      PhpFunctionComp(4405) or
      PhpFunctionComp(4521) or
      PhpFunctionComp(4538) or
      PhpFunctionComp(4749) or
      PhpFunctionComp(4786) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc142: TSynWebTokenKind;
begin
  if  PhpFunctionComp(119) or
      PhpFunctionComp(128) or
      PhpFunctionComp(494) or
      PhpFunctionComp(499) or
      PhpFunctionComp(715) or
      PhpFunctionComp(738) or
      PhpFunctionComp(1046) or
      PhpFunctionComp(1183) or
      PhpFunctionComp(1554) or
      PhpFunctionComp(1684) or
      PhpFunctionComp(1815) or
      PhpFunctionComp(1875) or
      PhpFunctionComp(1881) or
      PhpFunctionComp(2005) or
      PhpFunctionComp(2022) or
      PhpFunctionComp(2263) or
      PhpFunctionComp(2336) or
      PhpFunctionComp(2349) or
      PhpFunctionComp(2406) or
      PhpFunctionComp(2526) or
      PhpFunctionComp(2575) or
      PhpFunctionComp(2635) or
      PhpFunctionComp(2874) or
      PhpFunctionComp(2976) or
      PhpFunctionComp(3076) or
      PhpFunctionComp(3492) or
      PhpFunctionComp(3638) or
      PhpFunctionComp(3794) or
      PhpFunctionComp(4146) or
      PhpFunctionComp(4216) or
      PhpFunctionComp(4381) or
      PhpFunctionComp(4539) or
      PhpFunctionComp(4813) or
      PhpFunctionComp(4864) or
      PhpFunctionComp(4899) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc143: TSynWebTokenKind;
begin
  if  PhpFunctionComp(132) or
      PhpFunctionComp(246) or
      PhpFunctionComp(443) or
      PhpFunctionComp(477) or
      PhpFunctionComp(496) or
      PhpFunctionComp(1065) or
      PhpFunctionComp(1108) or
      PhpFunctionComp(1500) or
      PhpFunctionComp(1660) or
      PhpFunctionComp(1677) or
      PhpFunctionComp(1836) or
      PhpFunctionComp(1872) or
      PhpFunctionComp(2057) or
      PhpFunctionComp(2120) or
      PhpFunctionComp(2138) or
      PhpFunctionComp(2215) or
      PhpFunctionComp(2266) or
      PhpFunctionComp(2330) or
      PhpFunctionComp(2404) or
      PhpFunctionComp(2478) or
      PhpFunctionComp(2483) or
      PhpFunctionComp(3134) or
      PhpFunctionComp(3428) or
      PhpFunctionComp(3432) or
      PhpFunctionComp(3470) or
      PhpFunctionComp(3540) or
      PhpFunctionComp(3675) or
      PhpFunctionComp(3854) or
      PhpFunctionComp(3986) or
      PhpFunctionComp(4328) or
      PhpFunctionComp(4779) or
      PhpFunctionComp(4956) or
      PhpFunctionComp(5067) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc144: TSynWebTokenKind;
begin
  if  PhpFunctionComp(120) or
      PhpFunctionComp(587) or
      PhpFunctionComp(716) or
      PhpFunctionComp(732) or
      PhpFunctionComp(1034) or
      PhpFunctionComp(1123) or
      PhpFunctionComp(1371) or
      PhpFunctionComp(1603) or
      PhpFunctionComp(1624) or
      PhpFunctionComp(2693) or
      PhpFunctionComp(3048) or
      PhpFunctionComp(3235) or
      PhpFunctionComp(3313) or
      PhpFunctionComp(3421) or
      PhpFunctionComp(3573) or
      PhpFunctionComp(3578) or
      PhpFunctionComp(3615) or
      PhpFunctionComp(3666) or
      PhpFunctionComp(3741) or
      PhpFunctionComp(3880) or
      PhpFunctionComp(3930) or
      PhpFunctionComp(4060) or
      PhpFunctionComp(4341) or
      PhpFunctionComp(4696) or
      PhpFunctionComp(4776) or
      PhpFunctionComp(4934) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc145: TSynWebTokenKind;
begin
  if  PhpFunctionComp(208) or
      PhpFunctionComp(329) or
      PhpFunctionComp(498) or
      PhpFunctionComp(605) or
      PhpFunctionComp(668) or
      PhpFunctionComp(691) or
      PhpFunctionComp(1007) or
      PhpFunctionComp(1117) or
      PhpFunctionComp(1400) or
      PhpFunctionComp(1712) or
      PhpFunctionComp(1936) or
      PhpFunctionComp(1969) or
      PhpFunctionComp(2019) or
      PhpFunctionComp(2108) or
      PhpFunctionComp(2171) or
      PhpFunctionComp(2311) or
      PhpFunctionComp(2429) or
      PhpFunctionComp(2572) or
      PhpFunctionComp(2595) or
      PhpFunctionComp(2696) or
      PhpFunctionComp(2872) or
      PhpFunctionComp(3160) or
      PhpFunctionComp(3301) or
      PhpFunctionComp(3656) or
      PhpFunctionComp(3715) or
      PhpFunctionComp(3820) or
      PhpFunctionComp(3863) or
      PhpFunctionComp(4145) or
      PhpFunctionComp(4209) or
      PhpFunctionComp(4232) or
      PhpFunctionComp(4237) or
      PhpFunctionComp(4242) or
      PhpFunctionComp(4323) or
      PhpFunctionComp(4682) or
      PhpFunctionComp(4729) or
      PhpFunctionComp(5105) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc146: TSynWebTokenKind;
begin
  if  PhpFunctionComp(113) or
      PhpFunctionComp(133) or
      PhpFunctionComp(325) or
      PhpFunctionComp(495) or
      PhpFunctionComp(541) or
      PhpFunctionComp(718) or
      PhpFunctionComp(1021) or
      PhpFunctionComp(1100) or
      PhpFunctionComp(1142) or
      PhpFunctionComp(1182) or
      PhpFunctionComp(1390) or
      PhpFunctionComp(1471) or
      PhpFunctionComp(1567) or
      PhpFunctionComp(2020) or
      PhpFunctionComp(2023) or
      PhpFunctionComp(2040) or
      PhpFunctionComp(2366) or
      PhpFunctionComp(2453) or
      PhpFunctionComp(2561) or
      PhpFunctionComp(2594) or
      PhpFunctionComp(2642) or
      PhpFunctionComp(2654) or
      PhpFunctionComp(2731) or
      PhpFunctionComp(3062) or
      PhpFunctionComp(3078) or
      PhpFunctionComp(3446) or
      PhpFunctionComp(3570) or
      PhpFunctionComp(3599) or
      PhpFunctionComp(4030) or
      PhpFunctionComp(4093) or
      PhpFunctionComp(4139) or
      PhpFunctionComp(4215) or
      PhpFunctionComp(4570) or
      PhpFunctionComp(5075) or
      PhpFunctionComp(5110) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc147: TSynWebTokenKind;
begin
  if  PhpFunctionComp(127) or
      PhpFunctionComp(300) or
      PhpFunctionComp(360) or
      PhpFunctionComp(522) or
      PhpFunctionComp(723) or
      PhpFunctionComp(1067) or
      PhpFunctionComp(1144) or
      PhpFunctionComp(1406) or
      PhpFunctionComp(1581) or
      PhpFunctionComp(1643) or
      PhpFunctionComp(1787) or
      PhpFunctionComp(1907) or
      PhpFunctionComp(1935) or
      PhpFunctionComp(2355) or
      PhpFunctionComp(2455) or
      PhpFunctionComp(2538) or
      PhpFunctionComp(2648) or
      PhpFunctionComp(2775) or
      PhpFunctionComp(2869) or
      PhpFunctionComp(2884) or
      PhpFunctionComp(3121) or
      PhpFunctionComp(3228) or
      PhpFunctionComp(3244) or
      PhpFunctionComp(3367) or
      PhpFunctionComp(3444) or
      PhpFunctionComp(3547) or
      PhpFunctionComp(3671) or
      PhpFunctionComp(3679) or
      PhpFunctionComp(3859) or
      PhpFunctionComp(4249) or
      PhpFunctionComp(4603) or
      PhpFunctionComp(4737) or
      PhpFunctionComp(4849) or
      PhpFunctionComp(5077) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc148: TSynWebTokenKind;
begin
  if  PhpFunctionComp(98) or
      PhpFunctionComp(157) or
      PhpFunctionComp(327) or
      PhpFunctionComp(692) or
      PhpFunctionComp(740) or
      PhpFunctionComp(1018) or
      PhpFunctionComp(1154) or
      PhpFunctionComp(1212) or
      PhpFunctionComp(1216) or
      PhpFunctionComp(1329) or
      PhpFunctionComp(1413) or
      PhpFunctionComp(1464) or
      PhpFunctionComp(1663) or
      PhpFunctionComp(1680) or
      PhpFunctionComp(2033) or
      PhpFunctionComp(2590) or
      PhpFunctionComp(2881) or
      PhpFunctionComp(2994) or
      PhpFunctionComp(3233) or
      PhpFunctionComp(3331) or
      PhpFunctionComp(3454) or
      PhpFunctionComp(3546) or
      PhpFunctionComp(3588) or
      PhpFunctionComp(3614) or
      PhpFunctionComp(3644) or
      PhpFunctionComp(3968) or
      PhpFunctionComp(4278) or
      PhpFunctionComp(4327) or
      PhpFunctionComp(4412) or
      PhpFunctionComp(4470) or
      PhpFunctionComp(4794) or
      PhpFunctionComp(4826) or
      PhpFunctionComp(4854) or
      PhpFunctionComp(4994) or
      PhpFunctionComp(5102) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc149: TSynWebTokenKind;
begin
  if  PhpFunctionComp(377) or
      PhpFunctionComp(515) or
      PhpFunctionComp(721) or
      PhpFunctionComp(1135) or
      PhpFunctionComp(1203) or
      PhpFunctionComp(1291) or
      PhpFunctionComp(1681) or
      PhpFunctionComp(1775) or
      PhpFunctionComp(2133) or
      PhpFunctionComp(2329) or
      PhpFunctionComp(2663) or
      PhpFunctionComp(2738) or
      PhpFunctionComp(3074) or
      PhpFunctionComp(3289) or
      PhpFunctionComp(3583) or
      PhpFunctionComp(3627) or
      PhpFunctionComp(3672) or
      PhpFunctionComp(3684) or
      PhpFunctionComp(3737) or
      PhpFunctionComp(3810) or
      PhpFunctionComp(3811) or
      PhpFunctionComp(3874) or
      PhpFunctionComp(3955) or
      PhpFunctionComp(4001) or
      PhpFunctionComp(4311) or
      PhpFunctionComp(4392) or
      PhpFunctionComp(4691) or
      PhpFunctionComp(4734) or
      PhpFunctionComp(4788) or
      PhpFunctionComp(4814) or
      PhpFunctionComp(4974) or
      PhpFunctionComp(5106) or
      PhpFunctionComp(5132) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc150: TSynWebTokenKind;
begin
  if  PhpFunctionComp(211) or
      PhpFunctionComp(244) or
      PhpFunctionComp(358) or
      PhpFunctionComp(384) or
      PhpFunctionComp(534) or
      PhpFunctionComp(648) or
      PhpFunctionComp(727) or
      PhpFunctionComp(1024) or
      PhpFunctionComp(1027) or
      PhpFunctionComp(1214) or
      PhpFunctionComp(1221) or
      PhpFunctionComp(1253) or
      PhpFunctionComp(1492) or
      PhpFunctionComp(1498) or
      PhpFunctionComp(1704) or
      PhpFunctionComp(1738) or
      PhpFunctionComp(1886) or
      PhpFunctionComp(1887) or
      PhpFunctionComp(1946) or
      PhpFunctionComp(2560) or
      PhpFunctionComp(2577) or
      PhpFunctionComp(2593) or
      PhpFunctionComp(2680) or
      PhpFunctionComp(3103) or
      PhpFunctionComp(3138) or
      PhpFunctionComp(3273) or
      PhpFunctionComp(3412) or
      PhpFunctionComp(3448) or
      PhpFunctionComp(3479) or
      PhpFunctionComp(3580) or
      PhpFunctionComp(3720) or
      PhpFunctionComp(3762) or
      PhpFunctionComp(3809) or
      PhpFunctionComp(4097) or
      PhpFunctionComp(4107) or
      PhpFunctionComp(4272) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc151: TSynWebTokenKind;
begin
  if  PhpFunctionComp(143) or
      PhpFunctionComp(1173) or
      PhpFunctionComp(1186) or
      PhpFunctionComp(1777) or
      PhpFunctionComp(1870) or
      PhpFunctionComp(2038) or
      PhpFunctionComp(2444) or
      PhpFunctionComp(2578) or
      PhpFunctionComp(2647) or
      PhpFunctionComp(2886) or
      PhpFunctionComp(2946) or
      PhpFunctionComp(3616) or
      PhpFunctionComp(3664) or
      PhpFunctionComp(3693) or
      PhpFunctionComp(3902) or
      PhpFunctionComp(4316) or
      PhpFunctionComp(5154) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc152: TSynWebTokenKind;
begin
  if  PhpFunctionComp(129) or
      PhpFunctionComp(380) or
      PhpFunctionComp(390) or
      PhpFunctionComp(669) or
      PhpFunctionComp(737) or
      PhpFunctionComp(968) or
      PhpFunctionComp(1030) or
      PhpFunctionComp(1035) or
      PhpFunctionComp(1099) or
      PhpFunctionComp(1196) or
      PhpFunctionComp(1735) or
      PhpFunctionComp(1755) or
      PhpFunctionComp(1803) or
      PhpFunctionComp(1911) or
      PhpFunctionComp(1987) or
      PhpFunctionComp(2139) or
      PhpFunctionComp(2209) or
      PhpFunctionComp(2232) or
      PhpFunctionComp(2244) or
      PhpFunctionComp(2357) or
      PhpFunctionComp(2549) or
      PhpFunctionComp(2621) or
      PhpFunctionComp(2694) or
      PhpFunctionComp(2889) or
      PhpFunctionComp(3145) or
      PhpFunctionComp(3194) or
      PhpFunctionComp(3297) or
      PhpFunctionComp(3577) or
      PhpFunctionComp(3677) or
      PhpFunctionComp(3716) or
      PhpFunctionComp(3761) or
      PhpFunctionComp(3779) or
      PhpFunctionComp(4263) or
      PhpFunctionComp(4440) or
      PhpFunctionComp(4497) or
      PhpFunctionComp(4569) or
      PhpFunctionComp(4576) or
      PhpFunctionComp(5065) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc153: TSynWebTokenKind;
begin
  if  PhpFunctionComp(123) or
      PhpFunctionComp(124) or
      PhpFunctionComp(394) or
      PhpFunctionComp(447) or
      PhpFunctionComp(456) or
      PhpFunctionComp(554) or
      PhpFunctionComp(591) or
      PhpFunctionComp(1163) or
      PhpFunctionComp(1207) or
      PhpFunctionComp(1218) or
      PhpFunctionComp(1325) or
      PhpFunctionComp(1565) or
      PhpFunctionComp(1763) or
      PhpFunctionComp(2337) or
      PhpFunctionComp(2416) or
      PhpFunctionComp(2473) or
      PhpFunctionComp(2628) or
      PhpFunctionComp(2707) or
      PhpFunctionComp(3223) or
      PhpFunctionComp(3246) or
      PhpFunctionComp(3434) or
      PhpFunctionComp(3535) or
      PhpFunctionComp(3576) or
      PhpFunctionComp(3993) or
      PhpFunctionComp(4063) or
      PhpFunctionComp(4134) or
      PhpFunctionComp(4177) or
      PhpFunctionComp(4376) or
      PhpFunctionComp(4548) or
      PhpFunctionComp(5113) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc154: TSynWebTokenKind;
begin
  if  PhpFunctionComp(90) or
      PhpFunctionComp(121) or
      PhpFunctionComp(298) or
      PhpFunctionComp(311) or
      PhpFunctionComp(487) or
      PhpFunctionComp(699) or
      PhpFunctionComp(771) or
      PhpFunctionComp(788) or
      PhpFunctionComp(1031) or
      PhpFunctionComp(1164) or
      PhpFunctionComp(1174) or
      PhpFunctionComp(1277) or
      PhpFunctionComp(1414) or
      PhpFunctionComp(1538) or
      PhpFunctionComp(1699) or
      PhpFunctionComp(1784) or
      PhpFunctionComp(1908) or
      PhpFunctionComp(2170) or
      PhpFunctionComp(2361) or
      PhpFunctionComp(2371) or
      PhpFunctionComp(2415) or
      PhpFunctionComp(2603) or
      PhpFunctionComp(2653) or
      PhpFunctionComp(2903) or
      PhpFunctionComp(2905) or
      PhpFunctionComp(2924) or
      PhpFunctionComp(3119) or
      PhpFunctionComp(3199) or
      PhpFunctionComp(3374) or
      PhpFunctionComp(3460) or
      PhpFunctionComp(3658) or
      PhpFunctionComp(3698) or
      PhpFunctionComp(3818) or
      PhpFunctionComp(3845) or
      PhpFunctionComp(4057) or
      PhpFunctionComp(4271) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc155: TSynWebTokenKind;
begin
  if  PhpFunctionComp(151) or
      PhpFunctionComp(160) or
      PhpFunctionComp(232) or
      PhpFunctionComp(731) or
      PhpFunctionComp(794) or
      PhpFunctionComp(951) or
      PhpFunctionComp(1160) or
      PhpFunctionComp(1401) or
      PhpFunctionComp(1770) or
      PhpFunctionComp(1827) or
      PhpFunctionComp(1958) or
      PhpFunctionComp(2025) or
      PhpFunctionComp(2169) or
      PhpFunctionComp(2245) or
      PhpFunctionComp(2582) or
      PhpFunctionComp(2708) or
      PhpFunctionComp(2732) or
      PhpFunctionComp(2804) or
      PhpFunctionComp(3011) or
      PhpFunctionComp(3014) or
      PhpFunctionComp(3395) or
      PhpFunctionComp(3452) or
      PhpFunctionComp(3520) or
      PhpFunctionComp(3537) or
      PhpFunctionComp(3538) or
      PhpFunctionComp(3654) or
      PhpFunctionComp(4330) or
      PhpFunctionComp(4427) or
      PhpFunctionComp(4529) or
      PhpFunctionComp(4554) or
      PhpFunctionComp(4663) or
      PhpFunctionComp(4783) or
      PhpFunctionComp(4908) or
      PhpFunctionComp(5141) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc156: TSynWebTokenKind;
begin
  if  PhpFunctionComp(258) or
      PhpFunctionComp(343) or
      PhpFunctionComp(424) or
      PhpFunctionComp(516) or
      PhpFunctionComp(564) or
      PhpFunctionComp(1069) or
      PhpFunctionComp(1134) or
      PhpFunctionComp(1428) or
      PhpFunctionComp(1461) or
      PhpFunctionComp(1481) or
      PhpFunctionComp(1490) or
      PhpFunctionComp(1622) or
      PhpFunctionComp(1748) or
      PhpFunctionComp(1984) or
      PhpFunctionComp(2074) or
      PhpFunctionComp(2414) or
      PhpFunctionComp(2500) or
      PhpFunctionComp(2649) or
      PhpFunctionComp(2658) or
      PhpFunctionComp(2664) or
      PhpFunctionComp(2697) or
      PhpFunctionComp(2730) or
      PhpFunctionComp(3189) or
      PhpFunctionComp(3697) or
      PhpFunctionComp(3887) or
      PhpFunctionComp(3981) or
      PhpFunctionComp(4212) or
      PhpFunctionComp(4397) or
      PhpFunctionComp(4615) or
      PhpFunctionComp(4836) or
      PhpFunctionComp(4935) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc157: TSynWebTokenKind;
begin
  if  PhpFunctionComp(501) or
      PhpFunctionComp(523) or
      PhpFunctionComp(880) or
      PhpFunctionComp(1143) or
      PhpFunctionComp(1317) or
      PhpFunctionComp(1321) or
      PhpFunctionComp(1483) or
      PhpFunctionComp(1929) or
      PhpFunctionComp(1998) or
      PhpFunctionComp(2148) or
      PhpFunctionComp(2236) or
      PhpFunctionComp(2373) or
      PhpFunctionComp(2609) or
      PhpFunctionComp(3024) or
      PhpFunctionComp(3237) or
      PhpFunctionComp(3317) or
      PhpFunctionComp(3394) or
      PhpFunctionComp(3449) or
      PhpFunctionComp(3548) or
      PhpFunctionComp(3733) or
      PhpFunctionComp(3843) or
      PhpFunctionComp(3861) or
      PhpFunctionComp(4007) or
      PhpFunctionComp(4053) or
      PhpFunctionComp(4231) or
      PhpFunctionComp(4236) or
      PhpFunctionComp(4241) or
      PhpFunctionComp(4578) or
      PhpFunctionComp(4588) or
      PhpFunctionComp(4747) or
      PhpFunctionComp(4790) or
      PhpFunctionComp(4895) or
      PhpFunctionComp(5083) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc158: TSynWebTokenKind;
begin
  if  PhpFunctionComp(135) or
      PhpFunctionComp(335) or
      PhpFunctionComp(428) or
      PhpFunctionComp(907) or
      PhpFunctionComp(988) or
      PhpFunctionComp(1170) or
      PhpFunctionComp(1311) or
      PhpFunctionComp(1639) or
      PhpFunctionComp(1730) or
      PhpFunctionComp(1766) or
      PhpFunctionComp(2039) or
      PhpFunctionComp(2248) or
      PhpFunctionComp(2267) or
      PhpFunctionComp(2384) or
      PhpFunctionComp(2434) or
      PhpFunctionComp(2633) or
      PhpFunctionComp(2659) or
      PhpFunctionComp(2687) or
      PhpFunctionComp(3046) or
      PhpFunctionComp(3079) or
      PhpFunctionComp(3130) or
      PhpFunctionComp(3222) or
      PhpFunctionComp(3295) or
      PhpFunctionComp(3375) or
      PhpFunctionComp(3387) or
      PhpFunctionComp(3518) or
      PhpFunctionComp(3611) or
      PhpFunctionComp(3704) or
      PhpFunctionComp(4621) or
      PhpFunctionComp(4784) or
      PhpFunctionComp(4802) or
      PhpFunctionComp(5108) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc159: TSynWebTokenKind;
begin
  if  PhpFunctionComp(77) or
      PhpFunctionComp(278) or
      PhpFunctionComp(823) or
      PhpFunctionComp(925) or
      PhpFunctionComp(1028) or
      PhpFunctionComp(1179) or
      PhpFunctionComp(1361) or
      PhpFunctionComp(1499) or
      PhpFunctionComp(1629) or
      PhpFunctionComp(1756) or
      PhpFunctionComp(1928) or
      PhpFunctionComp(1941) or
      PhpFunctionComp(2134) or
      PhpFunctionComp(2421) or
      PhpFunctionComp(2576) or
      PhpFunctionComp(2605) or
      PhpFunctionComp(2634) or
      PhpFunctionComp(2923) or
      PhpFunctionComp(3070) or
      PhpFunctionComp(3113) or
      PhpFunctionComp(3175) or
      PhpFunctionComp(3457) or
      PhpFunctionComp(3561) or
      PhpFunctionComp(3681) or
      PhpFunctionComp(3700) or
      PhpFunctionComp(4074) or
      PhpFunctionComp(4087) or
      PhpFunctionComp(4352) or
      PhpFunctionComp(4519) or
      PhpFunctionComp(4572) or
      PhpFunctionComp(4674) or
      PhpFunctionComp(4831) or
      PhpFunctionComp(5101) or
      PhpFunctionComp(5131) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc160: TSynWebTokenKind;
begin
  if  PhpFunctionComp(228) or
      PhpFunctionComp(245) or
      PhpFunctionComp(532) or
      PhpFunctionComp(610) or
      PhpFunctionComp(1062) or
      PhpFunctionComp(1229) or
      PhpFunctionComp(1407) or
      PhpFunctionComp(1563) or
      PhpFunctionComp(1604) or
      PhpFunctionComp(1806) or
      PhpFunctionComp(1937) or
      PhpFunctionComp(1980) or
      PhpFunctionComp(2000) or
      PhpFunctionComp(2065) or
      PhpFunctionComp(2268) or
      PhpFunctionComp(2278) or
      PhpFunctionComp(2324) or
      PhpFunctionComp(2369) or
      PhpFunctionComp(2692) or
      PhpFunctionComp(2698) or
      PhpFunctionComp(2867) or
      PhpFunctionComp(3018) or
      PhpFunctionComp(3045) or
      PhpFunctionComp(3515) or
      PhpFunctionComp(3523) or
      PhpFunctionComp(3568) or
      PhpFunctionComp(3623) or
      PhpFunctionComp(3660) or
      PhpFunctionComp(3667) or
      PhpFunctionComp(3718) or
      PhpFunctionComp(3857) or
      PhpFunctionComp(3881) or
      PhpFunctionComp(3998) or
      PhpFunctionComp(4268) or
      PhpFunctionComp(4273) or
      PhpFunctionComp(4303) or
      PhpFunctionComp(4574) or
      PhpFunctionComp(4764) or
      PhpFunctionComp(4771) or
      PhpFunctionComp(4932) or
      PhpFunctionComp(4985) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc161: TSynWebTokenKind;
begin
  if  PhpFunctionComp(84) or
      PhpFunctionComp(107) or
      PhpFunctionComp(262) or
      PhpFunctionComp(323) or
      PhpFunctionComp(410) or
      PhpFunctionComp(470) or
      PhpFunctionComp(543) or
      PhpFunctionComp(597) or
      PhpFunctionComp(696) or
      PhpFunctionComp(1033) or
      PhpFunctionComp(1159) or
      PhpFunctionComp(1197) or
      PhpFunctionComp(1722) or
      PhpFunctionComp(1825) or
      PhpFunctionComp(1889) or
      PhpFunctionComp(1902) or
      PhpFunctionComp(2210) or
      PhpFunctionComp(2283) or
      PhpFunctionComp(2556) or
      PhpFunctionComp(2565) or
      PhpFunctionComp(2743) or
      PhpFunctionComp(2847) or
      PhpFunctionComp(2896) or
      PhpFunctionComp(2975) or
      PhpFunctionComp(2982) or
      PhpFunctionComp(3254) or
      PhpFunctionComp(3682) or
      PhpFunctionComp(3975) or
      PhpFunctionComp(4196) or
      PhpFunctionComp(4260) or
      PhpFunctionComp(4626) or
      PhpFunctionComp(4746) or
      PhpFunctionComp(4828) or
      PhpFunctionComp(4957) or
      PhpFunctionComp(4965) or
      PhpFunctionComp(5166) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc162: TSynWebTokenKind;
begin
  if  PhpFunctionComp(89) or
      PhpFunctionComp(305) or
      PhpFunctionComp(559) or
      PhpFunctionComp(879) or
      PhpFunctionComp(1130) or
      PhpFunctionComp(1155) or
      PhpFunctionComp(1233) or
      PhpFunctionComp(1392) or
      PhpFunctionComp(1780) or
      PhpFunctionComp(1794) or
      PhpFunctionComp(1816) or
      PhpFunctionComp(2037) or
      PhpFunctionComp(2113) or
      PhpFunctionComp(2166) or
      PhpFunctionComp(2189) or
      PhpFunctionComp(2460) or
      PhpFunctionComp(2547) or
      PhpFunctionComp(2583) or
      PhpFunctionComp(2709) or
      PhpFunctionComp(2710) or
      PhpFunctionComp(2917) or
      PhpFunctionComp(2919) or
      PhpFunctionComp(3050) or
      PhpFunctionComp(3056) or
      PhpFunctionComp(3765) or
      PhpFunctionComp(4214) or
      PhpFunctionComp(4280) or
      PhpFunctionComp(4360) or
      PhpFunctionComp(4424) or
      PhpFunctionComp(4579) or
      PhpFunctionComp(4623) or
      PhpFunctionComp(4656) or
      PhpFunctionComp(4800) or
      PhpFunctionComp(5062) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc163: TSynWebTokenKind;
begin
  if  PhpFunctionComp(91) or
      PhpFunctionComp(316) or
      PhpFunctionComp(321) or
      PhpFunctionComp(467) or
      PhpFunctionComp(473) or
      PhpFunctionComp(800) or
      PhpFunctionComp(1053) or
      PhpFunctionComp(1357) or
      PhpFunctionComp(1486) or
      PhpFunctionComp(1542) or
      PhpFunctionComp(1611) or
      PhpFunctionComp(2112) or
      PhpFunctionComp(2640) or
      PhpFunctionComp(2655) or
      PhpFunctionComp(3135) or
      PhpFunctionComp(3180) or
      PhpFunctionComp(3239) or
      PhpFunctionComp(3680) or
      PhpFunctionComp(3885) or
      PhpFunctionComp(4019) or
      PhpFunctionComp(4363) or
      PhpFunctionComp(4575) or
      PhpFunctionComp(4610) or
      PhpFunctionComp(4781) or
      PhpFunctionComp(4787) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc164: TSynWebTokenKind;
begin
  if  PhpFunctionComp(435) or
      PhpFunctionComp(520) or
      PhpFunctionComp(655) or
      PhpFunctionComp(1478) or
      PhpFunctionComp(1535) or
      PhpFunctionComp(1753) or
      PhpFunctionComp(1793) or
      PhpFunctionComp(2084) or
      PhpFunctionComp(2645) or
      PhpFunctionComp(2702) or
      PhpFunctionComp(2828) or
      PhpFunctionComp(3161) or
      PhpFunctionComp(3409) or
      PhpFunctionComp(3487) or
      PhpFunctionComp(3748) or
      PhpFunctionComp(3768) or
      PhpFunctionComp(3801) or
      PhpFunctionComp(4077) or
      PhpFunctionComp(4285) or
      PhpFunctionComp(4346) or
      PhpFunctionComp(4560) or
      PhpFunctionComp(4670) or
      PhpFunctionComp(4807) or
      PhpFunctionComp(4852) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc165: TSynWebTokenKind;
begin
  if  PhpFunctionComp(635) or
      PhpFunctionComp(1319) or
      PhpFunctionComp(1749) or
      PhpFunctionComp(1976) or
      PhpFunctionComp(2060) or
      PhpFunctionComp(2225) or
      PhpFunctionComp(2425) or
      PhpFunctionComp(2536) or
      PhpFunctionComp(2573) or
      PhpFunctionComp(2589) or
      PhpFunctionComp(2752) or
      PhpFunctionComp(2795) or
      PhpFunctionComp(2838) or
      PhpFunctionComp(2985) or
      PhpFunctionComp(3253) or
      PhpFunctionComp(3327) or
      PhpFunctionComp(4494) or
      PhpFunctionComp(4511) or
      PhpFunctionComp(4683) or
      PhpFunctionComp(4825) or
      PhpFunctionComp(4848) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc166: TSynWebTokenKind;
begin
  if  PhpFunctionComp(210) or
      PhpFunctionComp(239) or
      PhpFunctionComp(301) or
      PhpFunctionComp(391) or
      PhpFunctionComp(509) or
      PhpFunctionComp(637) or
      PhpFunctionComp(660) or
      PhpFunctionComp(762) or
      PhpFunctionComp(895) or
      PhpFunctionComp(940) or
      PhpFunctionComp(1083) or
      PhpFunctionComp(1127) or
      PhpFunctionComp(1190) or
      PhpFunctionComp(1199) or
      PhpFunctionComp(1377) or
      PhpFunctionComp(1415) or
      PhpFunctionComp(1484) or
      PhpFunctionComp(1545) or
      PhpFunctionComp(1591) or
      PhpFunctionComp(1772) or
      PhpFunctionComp(1962) or
      PhpFunctionComp(2021) or
      PhpFunctionComp(2186) or
      PhpFunctionComp(2363) or
      PhpFunctionComp(2631) or
      PhpFunctionComp(2677) or
      PhpFunctionComp(2864) or
      PhpFunctionComp(2895) or
      PhpFunctionComp(2911) or
      PhpFunctionComp(3224) or
      PhpFunctionComp(3542) or
      PhpFunctionComp(3630) or
      PhpFunctionComp(3678) or
      PhpFunctionComp(3747) or
      PhpFunctionComp(3980) or
      PhpFunctionComp(4748) or
      PhpFunctionComp(4810) or
      PhpFunctionComp(5164) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc167: TSynWebTokenKind;
begin
  if  PhpFunctionComp(114) or
      PhpFunctionComp(302) or
      PhpFunctionComp(310) or
      PhpFunctionComp(1539) or
      PhpFunctionComp(1764) or
      PhpFunctionComp(1828) or
      PhpFunctionComp(1863) or
      PhpFunctionComp(1960) or
      PhpFunctionComp(2243) or
      PhpFunctionComp(2299) or
      PhpFunctionComp(2356) or
      PhpFunctionComp(2706) or
      PhpFunctionComp(2846) or
      PhpFunctionComp(3502) or
      PhpFunctionComp(3642) or
      PhpFunctionComp(3735) or
      PhpFunctionComp(3799) or
      PhpFunctionComp(3970) or
      PhpFunctionComp(4066) or
      PhpFunctionComp(4136) or
      PhpFunctionComp(4365) or
      PhpFunctionComp(4375) or
      PhpFunctionComp(4909) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc168: TSynWebTokenKind;
begin
  if  PhpFunctionComp(87) or
      PhpFunctionComp(161) or
      PhpFunctionComp(261) or
      PhpFunctionComp(363) or
      PhpFunctionComp(542) or
      PhpFunctionComp(586) or
      PhpFunctionComp(667) or
      PhpFunctionComp(671) or
      PhpFunctionComp(913) or
      PhpFunctionComp(1037) or
      PhpFunctionComp(1044) or
      PhpFunctionComp(1606) or
      PhpFunctionComp(1610) or
      PhpFunctionComp(1641) or
      PhpFunctionComp(1692) or
      PhpFunctionComp(1809) or
      PhpFunctionComp(1904) or
      PhpFunctionComp(2386) or
      PhpFunctionComp(2622) or
      PhpFunctionComp(2739) or
      PhpFunctionComp(2860) or
      PhpFunctionComp(2882) or
      PhpFunctionComp(2914) or
      PhpFunctionComp(2922) or
      PhpFunctionComp(3019) or
      PhpFunctionComp(3661) or
      PhpFunctionComp(3771) or
      PhpFunctionComp(3875) or
      PhpFunctionComp(3939) or
      PhpFunctionComp(4078) or
      PhpFunctionComp(4329) or
      PhpFunctionComp(4492) or
      PhpFunctionComp(4513) or
      PhpFunctionComp(4735) or
      PhpFunctionComp(4926) or
      PhpFunctionComp(5059) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc169: TSynWebTokenKind;
begin
  if  PhpFunctionComp(104) or
      PhpFunctionComp(1110) or
      PhpFunctionComp(1541) or
      PhpFunctionComp(1781) or
      PhpFunctionComp(2027) or
      PhpFunctionComp(2309) or
      PhpFunctionComp(2740) or
      PhpFunctionComp(2753) or
      PhpFunctionComp(2791) or
      PhpFunctionComp(2871) or
      PhpFunctionComp(3110) or
      PhpFunctionComp(3171) or
      PhpFunctionComp(3489) or
      PhpFunctionComp(3848) or
      PhpFunctionComp(4015) or
      PhpFunctionComp(4252) or
      PhpFunctionComp(4259) or
      PhpFunctionComp(4364) or
      PhpFunctionComp(4580) or
      PhpFunctionComp(4596) or
      PhpFunctionComp(4707) or
      PhpFunctionComp(4736) or
      PhpFunctionComp(4830) or
      PhpFunctionComp(4832) or
      PhpFunctionComp(4905) or
      PhpFunctionComp(5078) or
      PhpFunctionComp(5145) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc170: TSynWebTokenKind;
begin
  if  PhpFunctionComp(81) or
      PhpFunctionComp(139) or
      PhpFunctionComp(203) or
      PhpFunctionComp(734) or
      PhpFunctionComp(735) or
      PhpFunctionComp(801) or
      PhpFunctionComp(1121) or
      PhpFunctionComp(1389) or
      PhpFunctionComp(1765) or
      PhpFunctionComp(1778) or
      PhpFunctionComp(2028) or
      PhpFunctionComp(2379) or
      PhpFunctionComp(2676) or
      PhpFunctionComp(2926) or
      PhpFunctionComp(2984) or
      PhpFunctionComp(2986) or
      PhpFunctionComp(3015) or
      PhpFunctionComp(3369) or
      PhpFunctionComp(3440) or
      PhpFunctionComp(3522) or
      PhpFunctionComp(3855) or
      PhpFunctionComp(3865) or
      PhpFunctionComp(4194) or
      PhpFunctionComp(4206) or
      PhpFunctionComp(4257) or
      PhpFunctionComp(4279) or
      PhpFunctionComp(4310) or
      PhpFunctionComp(4390) or
      PhpFunctionComp(4409) or
      PhpFunctionComp(4517) or
      PhpFunctionComp(4598) or
      PhpFunctionComp(4668) or
      PhpFunctionComp(4733) or
      PhpFunctionComp(5109) or
      PhpFunctionComp(5117) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc171: TSynWebTokenKind;
begin
  if  PhpFunctionComp(570) or
      PhpFunctionComp(744) or
      PhpFunctionComp(1176) or
      PhpFunctionComp(1181) or
      PhpFunctionComp(1378) or
      PhpFunctionComp(1487) or
      PhpFunctionComp(1637) or
      PhpFunctionComp(1679) or
      PhpFunctionComp(1752) or
      PhpFunctionComp(1814) or
      PhpFunctionComp(1905) or
      PhpFunctionComp(1926) or
      PhpFunctionComp(1972) or
      PhpFunctionComp(1991) or
      PhpFunctionComp(2180) or
      PhpFunctionComp(2233) or
      PhpFunctionComp(2270) or
      PhpFunctionComp(2334) or
      PhpFunctionComp(2451) or
      PhpFunctionComp(2597) or
      PhpFunctionComp(2599) or
      PhpFunctionComp(2719) or
      PhpFunctionComp(2763) or
      PhpFunctionComp(2843) or
      PhpFunctionComp(3105) or
      PhpFunctionComp(3109) or
      PhpFunctionComp(3162) or
      PhpFunctionComp(3302) or
      PhpFunctionComp(3420) or
      PhpFunctionComp(3829) or
      PhpFunctionComp(3914) or
      PhpFunctionComp(4103) or
      PhpFunctionComp(4133) or
      PhpFunctionComp(4261) or
      PhpFunctionComp(4317) or
      PhpFunctionComp(4398) or
      PhpFunctionComp(4822) or
      PhpFunctionComp(4971) or
      PhpFunctionComp(5082) or
      PhpFunctionComp(5128) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc172: TSynWebTokenKind;
begin
  if  PhpFunctionComp(163) or
      PhpFunctionComp(446) or
      PhpFunctionComp(533) or
      PhpFunctionComp(749) or
      PhpFunctionComp(1102) or
      PhpFunctionComp(1648) or
      PhpFunctionComp(2008) or
      PhpFunctionComp(2277) or
      PhpFunctionComp(2314) or
      PhpFunctionComp(2370) or
      PhpFunctionComp(2481) or
      PhpFunctionComp(2727) or
      PhpFunctionComp(2933) or
      PhpFunctionComp(3033) or
      PhpFunctionComp(3447) or
      PhpFunctionComp(3490) or
      PhpFunctionComp(3663) or
      PhpFunctionComp(3665) or
      PhpFunctionComp(3876) or
      PhpFunctionComp(3889) or
      PhpFunctionComp(4064) or
      PhpFunctionComp(4156) or
      PhpFunctionComp(4208) or
      PhpFunctionComp(4518) or
      PhpFunctionComp(4613) or
      PhpFunctionComp(4940) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc173: TSynWebTokenKind;
begin
  if  PhpKeywordComp(55) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(324) or
        PhpFunctionComp(742) or
        PhpFunctionComp(753) or
        PhpFunctionComp(908) or
        PhpFunctionComp(942) or
        PhpFunctionComp(1702) or
        PhpFunctionComp(1921) or
        PhpFunctionComp(1973) or
        PhpFunctionComp(2226) or
        PhpFunctionComp(2257) or
        PhpFunctionComp(2340) or
        PhpFunctionComp(2601) or
        PhpFunctionComp(2613) or
        PhpFunctionComp(2756) or
        PhpFunctionComp(2878) or
        PhpFunctionComp(2883) or
        PhpFunctionComp(2897) or
        PhpFunctionComp(3030) or
        PhpFunctionComp(3037) or
        PhpFunctionComp(3089) or
        PhpFunctionComp(3152) or
        PhpFunctionComp(3243) or
        PhpFunctionComp(3544) or
        PhpFunctionComp(3788) or
        PhpFunctionComp(4104) or
        PhpFunctionComp(4267) or
        PhpFunctionComp(4471) or
        PhpFunctionComp(4816) or
        PhpFunctionComp(4900) or
        PhpFunctionComp(5129) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc174: TSynWebTokenKind;
begin
  if  PhpFunctionComp(109) or
      PhpFunctionComp(745) or
      PhpFunctionComp(755) or
      PhpFunctionComp(831) or
      PhpFunctionComp(889) or
      PhpFunctionComp(1231) or
      PhpFunctionComp(1242) or
      PhpFunctionComp(1335) or
      PhpFunctionComp(1494) or
      PhpFunctionComp(1555) or
      PhpFunctionComp(2029) or
      PhpFunctionComp(2034) or
      PhpFunctionComp(2310) or
      PhpFunctionComp(2474) or
      PhpFunctionComp(2554) or
      PhpFunctionComp(2615) or
      PhpFunctionComp(2786) or
      PhpFunctionComp(3077) or
      PhpFunctionComp(3370) or
      PhpFunctionComp(3519) or
      PhpFunctionComp(3983) or
      PhpFunctionComp(4744) or
      PhpFunctionComp(5005) or
      PhpFunctionComp(5064) or
      PhpFunctionComp(5114) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc175: TSynWebTokenKind;
begin
  if  PhpFunctionComp(159) or
      PhpFunctionComp(741) or
      PhpFunctionComp(1177) or
      PhpFunctionComp(1301) or
      PhpFunctionComp(1337) or
      PhpFunctionComp(1374) or
      PhpFunctionComp(1527) or
      PhpFunctionComp(1715) or
      PhpFunctionComp(2035) or
      PhpFunctionComp(2179) or
      PhpFunctionComp(2221) or
      PhpFunctionComp(2678) or
      PhpFunctionComp(3053) or
      PhpFunctionComp(3727) or
      PhpFunctionComp(4527) or
      PhpFunctionComp(4587) or
      PhpFunctionComp(4618) or
      PhpFunctionComp(4961) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc176: TSynWebTokenKind;
begin
  if  PhpFunctionComp(215) or
      PhpFunctionComp(540) or
      PhpFunctionComp(546) or
      PhpFunctionComp(562) or
      PhpFunctionComp(572) or
      PhpFunctionComp(608) or
      PhpFunctionComp(912) or
      PhpFunctionComp(1424) or
      PhpFunctionComp(1507) or
      PhpFunctionComp(1666) or
      PhpFunctionComp(1829) or
      PhpFunctionComp(1869) or
      PhpFunctionComp(1891) or
      PhpFunctionComp(2172) or
      PhpFunctionComp(2643) or
      PhpFunctionComp(2725) or
      PhpFunctionComp(2836) or
      PhpFunctionComp(2863) or
      PhpFunctionComp(2910) or
      PhpFunctionComp(3174) or
      PhpFunctionComp(3259) or
      PhpFunctionComp(3711) or
      PhpFunctionComp(3791) or
      PhpFunctionComp(3835) or
      PhpFunctionComp(4140) or
      PhpFunctionComp(4321) or
      PhpFunctionComp(4389) or
      PhpFunctionComp(4622) or
      PhpFunctionComp(4732) or
      PhpFunctionComp(4907) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc177: TSynWebTokenKind;
begin
  if  PhpFunctionComp(79) or
      PhpFunctionComp(195) or
      PhpFunctionComp(306) or
      PhpFunctionComp(401) or
      PhpFunctionComp(441) or
      PhpFunctionComp(471) or
      PhpFunctionComp(897) or
      PhpFunctionComp(931) or
      PhpFunctionComp(950) or
      PhpFunctionComp(1004) or
      PhpFunctionComp(1330) or
      PhpFunctionComp(1826) or
      PhpFunctionComp(1890) or
      PhpFunctionComp(2026) or
      PhpFunctionComp(2051) or
      PhpFunctionComp(2282) or
      PhpFunctionComp(2660) or
      PhpFunctionComp(2870) or
      PhpFunctionComp(2912) or
      PhpFunctionComp(2968) or
      PhpFunctionComp(2988) or
      PhpFunctionComp(3002) or
      PhpFunctionComp(3133) or
      PhpFunctionComp(3179) or
      PhpFunctionComp(3435) or
      PhpFunctionComp(3533) or
      PhpFunctionComp(3608) or
      PhpFunctionComp(3673) or
      PhpFunctionComp(3707) or
      PhpFunctionComp(3921) or
      PhpFunctionComp(4414) or
      PhpFunctionComp(4654) or
      PhpFunctionComp(4827) or
      PhpFunctionComp(4871) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc178: TSynWebTokenKind;
begin
  if  PhpFunctionComp(385) or
      PhpFunctionComp(739) or
      PhpFunctionComp(882) or
      PhpFunctionComp(1009) or
      PhpFunctionComp(1042) or
      PhpFunctionComp(1333) or
      PhpFunctionComp(1670) or
      PhpFunctionComp(1701) or
      PhpFunctionComp(1776) or
      PhpFunctionComp(1894) or
      PhpFunctionComp(2048) or
      PhpFunctionComp(2130) or
      PhpFunctionComp(2135) or
      PhpFunctionComp(2178) or
      PhpFunctionComp(2486) or
      PhpFunctionComp(2661) or
      PhpFunctionComp(2695) or
      PhpFunctionComp(2936) or
      PhpFunctionComp(3026) or
      PhpFunctionComp(3112) or
      PhpFunctionComp(3173) or
      PhpFunctionComp(3303) or
      PhpFunctionComp(3335) or
      PhpFunctionComp(3839) or
      PhpFunctionComp(4047) or
      PhpFunctionComp(4258) or
      PhpFunctionComp(4535) or
      PhpFunctionComp(4552) or
      PhpFunctionComp(4665) or
      PhpFunctionComp(4834) or
      PhpFunctionComp(4911) or
      PhpFunctionComp(4917) or
      PhpFunctionComp(4938) or
      PhpFunctionComp(4983) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc179: TSynWebTokenKind;
begin
  if  PhpFunctionComp(116) or
      PhpFunctionComp(265) or
      PhpFunctionComp(613) or
      PhpFunctionComp(1180) or
      PhpFunctionComp(1284) or
      PhpFunctionComp(1517) or
      PhpFunctionComp(1824) or
      PhpFunctionComp(2129) or
      PhpFunctionComp(2423) or
      PhpFunctionComp(2502) or
      PhpFunctionComp(2580) or
      PhpFunctionComp(2657) or
      PhpFunctionComp(2837) or
      PhpFunctionComp(2893) or
      PhpFunctionComp(3043) or
      PhpFunctionComp(3049) or
      PhpFunctionComp(3081) or
      PhpFunctionComp(3373) or
      PhpFunctionComp(3804) or
      PhpFunctionComp(3919) or
      PhpFunctionComp(3987) or
      PhpFunctionComp(4049) or
      PhpFunctionComp(4195) or
      PhpFunctionComp(4314) or
      PhpFunctionComp(4348) or
      PhpFunctionComp(4359) or
      PhpFunctionComp(4583) or
      PhpFunctionComp(4937) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc180: TSynWebTokenKind;
begin
  if  PhpFunctionComp(359) or
      PhpFunctionComp(478) or
      PhpFunctionComp(770) or
      PhpFunctionComp(985) or
      PhpFunctionComp(1055) or
      PhpFunctionComp(1716) or
      PhpFunctionComp(1719) or
      PhpFunctionComp(1884) or
      PhpFunctionComp(2081) or
      PhpFunctionComp(2438) or
      PhpFunctionComp(2644) or
      PhpFunctionComp(2764) or
      PhpFunctionComp(2877) or
      PhpFunctionComp(2898) or
      PhpFunctionComp(2952) or
      PhpFunctionComp(2960) or
      PhpFunctionComp(3034) or
      PhpFunctionComp(3140) or
      PhpFunctionComp(3185) or
      PhpFunctionComp(3336) or
      PhpFunctionComp(3499) or
      PhpFunctionComp(3647) or
      PhpFunctionComp(3969) or
      PhpFunctionComp(3990) or
      PhpFunctionComp(4051) or
      PhpFunctionComp(4456) or
      PhpFunctionComp(4547) or
      PhpFunctionComp(4563) or
      PhpFunctionComp(4590) or
      PhpFunctionComp(4597) or
      PhpFunctionComp(4795) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc181: TSynWebTokenKind;
begin
  if  PhpFunctionComp(105) or
      PhpFunctionComp(149) or
      PhpFunctionComp(512) or
      PhpFunctionComp(518) or
      PhpFunctionComp(993) or
      PhpFunctionComp(1140) or
      PhpFunctionComp(1255) or
      PhpFunctionComp(1560) or
      PhpFunctionComp(1631) or
      PhpFunctionComp(1694) or
      PhpFunctionComp(1823) or
      PhpFunctionComp(2187) or
      PhpFunctionComp(2276) or
      PhpFunctionComp(2323) or
      PhpFunctionComp(2332) or
      PhpFunctionComp(2729) or
      PhpFunctionComp(2787) or
      PhpFunctionComp(3685) or
      PhpFunctionComp(3860) or
      PhpFunctionComp(4296) or
      PhpFunctionComp(4306) or
      PhpFunctionComp(4361) or
      PhpFunctionComp(4600) or
      PhpFunctionComp(4620) or
      PhpFunctionComp(4685) or
      PhpFunctionComp(4695) or
      PhpFunctionComp(4770) or
      PhpFunctionComp(4914) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc182: TSynWebTokenKind;
begin
  if  PhpFunctionComp(74) or
      PhpFunctionComp(122) or
      PhpFunctionComp(322) or
      PhpFunctionComp(507) or
      PhpFunctionComp(1115) or
      PhpFunctionComp(1338) or
      PhpFunctionComp(1530) or
      PhpFunctionComp(1532) or
      PhpFunctionComp(1578) or
      PhpFunctionComp(1580) or
      PhpFunctionComp(1795) or
      PhpFunctionComp(2343) or
      PhpFunctionComp(2380) or
      PhpFunctionComp(2662) or
      PhpFunctionComp(2665) or
      PhpFunctionComp(2829) or
      PhpFunctionComp(2913) or
      PhpFunctionComp(3012) or
      PhpFunctionComp(3226) or
      PhpFunctionComp(3270) or
      PhpFunctionComp(3527) or
      PhpFunctionComp(3668) or
      PhpFunctionComp(4254) or
      PhpFunctionComp(4347) or
      PhpFunctionComp(4393) or
      PhpFunctionComp(4850) or
      PhpFunctionComp(4980) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc183: TSynWebTokenKind;
begin
  if  PhpFunctionComp(125) or
      PhpFunctionComp(508) or
      PhpFunctionComp(592) or
      PhpFunctionComp(893) or
      PhpFunctionComp(1008) or
      PhpFunctionComp(1162) or
      PhpFunctionComp(1254) or
      PhpFunctionComp(1256) or
      PhpFunctionComp(1320) or
      PhpFunctionComp(1561) or
      PhpFunctionComp(1623) or
      PhpFunctionComp(2341) or
      PhpFunctionComp(2372) or
      PhpFunctionComp(2390) or
      PhpFunctionComp(2399) or
      PhpFunctionComp(2428) or
      PhpFunctionComp(2439) or
      PhpFunctionComp(2559) or
      PhpFunctionComp(2611) or
      PhpFunctionComp(2612) or
      PhpFunctionComp(2674) or
      PhpFunctionComp(2704) or
      PhpFunctionComp(2859) or
      PhpFunctionComp(2945) or
      PhpFunctionComp(3296) or
      PhpFunctionComp(3628) or
      PhpFunctionComp(3800) or
      PhpFunctionComp(4176) or
      PhpFunctionComp(4541) or
      PhpFunctionComp(4689) or
      PhpFunctionComp(4847) or
      PhpFunctionComp(4885) or
      PhpFunctionComp(5069) or
      PhpFunctionComp(5165) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc184: TSynWebTokenKind;
begin
  if  PhpFunctionComp(872) or
      PhpFunctionComp(894) or
      PhpFunctionComp(1038) or
      PhpFunctionComp(1112) or
      PhpFunctionComp(1282) or
      PhpFunctionComp(1393) or
      PhpFunctionComp(1837) or
      PhpFunctionComp(1925) or
      PhpFunctionComp(2165) or
      PhpFunctionComp(2256) or
      PhpFunctionComp(2396) or
      PhpFunctionComp(2456) or
      PhpFunctionComp(2646) or
      PhpFunctionComp(2705) or
      PhpFunctionComp(2718) or
      PhpFunctionComp(2967) or
      PhpFunctionComp(2981) or
      PhpFunctionComp(2993) or
      PhpFunctionComp(3027) or
      PhpFunctionComp(3231) or
      PhpFunctionComp(3328) or
      PhpFunctionComp(3453) or
      PhpFunctionComp(3730) or
      PhpFunctionComp(4967) or
      PhpFunctionComp(4968) or
      PhpFunctionComp(5004) or
      PhpFunctionComp(5136) or
      PhpFunctionComp(5155) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc185: TSynWebTokenKind;
begin
  if  PhpFunctionComp(82) or
      PhpFunctionComp(92) or
      PhpFunctionComp(115) or
      PhpFunctionComp(752) or
      PhpFunctionComp(983) or
      PhpFunctionComp(1047) or
      PhpFunctionComp(1171) or
      PhpFunctionComp(1596) or
      PhpFunctionComp(1599) or
      PhpFunctionComp(1697) or
      PhpFunctionComp(1877) or
      PhpFunctionComp(1994) or
      PhpFunctionComp(2295) or
      PhpFunctionComp(2413) or
      PhpFunctionComp(2548) or
      PhpFunctionComp(2656) or
      PhpFunctionComp(2701) or
      PhpFunctionComp(2840) or
      PhpFunctionComp(2865) or
      PhpFunctionComp(2958) or
      PhpFunctionComp(2972) or
      PhpFunctionComp(3290) or
      PhpFunctionComp(3329) or
      PhpFunctionComp(3346) or
      PhpFunctionComp(3819) or
      PhpFunctionComp(3831) or
      PhpFunctionComp(4324) or
      PhpFunctionComp(4417) or
      PhpFunctionComp(4428) or
      PhpFunctionComp(4582) or
      PhpFunctionComp(4772) or
      PhpFunctionComp(4936) or
      PhpFunctionComp(5139) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc186: TSynWebTokenKind;
begin
  if  PhpFunctionComp(445) or
      PhpFunctionComp(722) or
      PhpFunctionComp(733) or
      PhpFunctionComp(1076) or
      PhpFunctionComp(1141) or
      PhpFunctionComp(1844) or
      PhpFunctionComp(1956) or
      PhpFunctionComp(2289) or
      PhpFunctionComp(2381) or
      PhpFunctionComp(2412) or
      PhpFunctionComp(2487) or
      PhpFunctionComp(2588) or
      PhpFunctionComp(2619) or
      PhpFunctionComp(2623) or
      PhpFunctionComp(3170) or
      PhpFunctionComp(3238) or
      PhpFunctionComp(3240) or
      PhpFunctionComp(3439) or
      PhpFunctionComp(3706) or
      PhpFunctionComp(3710) or
      PhpFunctionComp(3815) or
      PhpFunctionComp(3825) or
      PhpFunctionComp(4182) or
      PhpFunctionComp(4199) or
      PhpFunctionComp(4247) or
      PhpFunctionComp(4255) or
      PhpFunctionComp(4382) or
      PhpFunctionComp(4581) or
      PhpFunctionComp(4594) or
      PhpFunctionComp(4918) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc187: TSynWebTokenKind;
begin
  if  PhpFunctionComp(240) or
      PhpFunctionComp(645) or
      PhpFunctionComp(905) or
      PhpFunctionComp(953) or
      PhpFunctionComp(1103) or
      PhpFunctionComp(1587) or
      PhpFunctionComp(1831) or
      PhpFunctionComp(1883) or
      PhpFunctionComp(1903) or
      PhpFunctionComp(1938) or
      PhpFunctionComp(2031) or
      PhpFunctionComp(2164) or
      PhpFunctionComp(2181) or
      PhpFunctionComp(2443) or
      PhpFunctionComp(2587) or
      PhpFunctionComp(2841) or
      PhpFunctionComp(2920) or
      PhpFunctionComp(3564) or
      PhpFunctionComp(3702) or
      PhpFunctionComp(4109) or
      PhpFunctionComp(4220) or
      PhpFunctionComp(4312) or
      PhpFunctionComp(4614) or
      PhpFunctionComp(4661) or
      PhpFunctionComp(4671) or
      PhpFunctionComp(4692) or
      PhpFunctionComp(4697) or
      PhpFunctionComp(5160) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc188: TSynWebTokenKind;
begin
  if  PhpFunctionComp(192) or
      PhpFunctionComp(309) or
      PhpFunctionComp(381) or
      PhpFunctionComp(528) or
      PhpFunctionComp(584) or
      PhpFunctionComp(904) or
      PhpFunctionComp(1403) or
      PhpFunctionComp(1779) or
      PhpFunctionComp(1854) or
      PhpFunctionComp(2114) or
      PhpFunctionComp(2317) or
      PhpFunctionComp(2326) or
      PhpFunctionComp(2711) or
      PhpFunctionComp(2890) or
      PhpFunctionComp(2964) or
      PhpFunctionComp(2977) or
      PhpFunctionComp(3017) or
      PhpFunctionComp(3111) or
      PhpFunctionComp(3193) or
      PhpFunctionComp(3263) or
      PhpFunctionComp(3669) or
      PhpFunctionComp(3852) or
      PhpFunctionComp(3888) or
      PhpFunctionComp(4210) or
      PhpFunctionComp(4362) or
      PhpFunctionComp(4367) or
      PhpFunctionComp(4388) or
      PhpFunctionComp(4780) or
      PhpFunctionComp(4916) or
      PhpFunctionComp(4933) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc189: TSynWebTokenKind;
begin
  if  PhpFunctionComp(88) or
      PhpFunctionComp(264) or
      PhpFunctionComp(484) or
      PhpFunctionComp(612) or
      PhpFunctionComp(1022) or
      PhpFunctionComp(1536) or
      PhpFunctionComp(1564) or
      PhpFunctionComp(1579) or
      PhpFunctionComp(1650) or
      PhpFunctionComp(1871) or
      PhpFunctionComp(2275) or
      PhpFunctionComp(2616) or
      PhpFunctionComp(2723) or
      PhpFunctionComp(2803) or
      PhpFunctionComp(2918) or
      PhpFunctionComp(3271) or
      PhpFunctionComp(3462) or
      PhpFunctionComp(3503) or
      PhpFunctionComp(3646) or
      PhpFunctionComp(4851) or
      PhpFunctionComp(4928) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc190: TSynWebTokenKind;
begin
  if  PhpFunctionComp(254) or
      PhpFunctionComp(315) or
      PhpFunctionComp(451) or
      PhpFunctionComp(877) or
      PhpFunctionComp(1252) or
      PhpFunctionComp(1278) or
      PhpFunctionComp(1460) or
      PhpFunctionComp(1612) or
      PhpFunctionComp(1665) or
      PhpFunctionComp(1700) or
      PhpFunctionComp(1916) or
      PhpFunctionComp(2382) or
      PhpFunctionComp(2426) or
      PhpFunctionComp(2641) or
      PhpFunctionComp(2835) or
      PhpFunctionComp(3343) or
      PhpFunctionComp(3356) or
      PhpFunctionComp(3505) or
      PhpFunctionComp(3989) or
      PhpFunctionComp(4056) or
      PhpFunctionComp(4315) or
      PhpFunctionComp(4433) or
      PhpFunctionComp(4448) or
      PhpFunctionComp(4785) or
      PhpFunctionComp(5023) or
      PhpFunctionComp(5111) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc191: TSynWebTokenKind;
begin
  if  PhpFunctionComp(611) or
      PhpFunctionComp(670) or
      PhpFunctionComp(883) or
      PhpFunctionComp(885) or
      PhpFunctionComp(923) or
      PhpFunctionComp(1866) or
      PhpFunctionComp(1930) or
      PhpFunctionComp(2075) or
      PhpFunctionComp(2700) or
      PhpFunctionComp(2934) or
      PhpFunctionComp(2943) or
      PhpFunctionComp(3047) or
      PhpFunctionComp(3085) or
      PhpFunctionComp(3280) or
      PhpFunctionComp(3411) or
      PhpFunctionComp(3806) or
      PhpFunctionComp(3826) or
      PhpFunctionComp(3911) or
      PhpFunctionComp(4009) or
      PhpFunctionComp(4766) or
      PhpFunctionComp(4809) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc192: TSynWebTokenKind;
begin
  if  PhpFunctionComp(177) or
      PhpFunctionComp(458) or
      PhpFunctionComp(888) or
      PhpFunctionComp(960) or
      PhpFunctionComp(1086) or
      PhpFunctionComp(1429) or
      PhpFunctionComp(1562) or
      PhpFunctionComp(1895) or
      PhpFunctionComp(2059) or
      PhpFunctionComp(2303) or
      PhpFunctionComp(2318) or
      PhpFunctionComp(2365) or
      PhpFunctionComp(2422) or
      PhpFunctionComp(2606) or
      PhpFunctionComp(2758) or
      PhpFunctionComp(2760) or
      PhpFunctionComp(2928) or
      PhpFunctionComp(3157) or
      PhpFunctionComp(3609) or
      PhpFunctionComp(3982) or
      PhpFunctionComp(4044) or
      PhpFunctionComp(4152) or
      PhpFunctionComp(4629) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc193: TSynWebTokenKind;
begin
  if  PhpFunctionComp(453) or
      PhpFunctionComp(460) or
      PhpFunctionComp(514) or
      PhpFunctionComp(535) or
      PhpFunctionComp(720) or
      PhpFunctionComp(724) or
      PhpFunctionComp(973) or
      PhpFunctionComp(1005) or
      PhpFunctionComp(1676) or
      PhpFunctionComp(1882) or
      PhpFunctionComp(1893) or
      PhpFunctionComp(2128) or
      PhpFunctionComp(2458) or
      PhpFunctionComp(2761) or
      PhpFunctionComp(2776) or
      PhpFunctionComp(2942) or
      PhpFunctionComp(2954) or
      PhpFunctionComp(2974) or
      PhpFunctionComp(3372) or
      PhpFunctionComp(4200) or
      PhpFunctionComp(4222) or
      PhpFunctionComp(4377) or
      PhpFunctionComp(4407) or
      PhpFunctionComp(4476) or
      PhpFunctionComp(4483) or
      PhpFunctionComp(4556) or
      PhpFunctionComp(4684) or
      PhpFunctionComp(4941) or
      PhpFunctionComp(5018) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc194: TSynWebTokenKind;
begin
  if  PhpFunctionComp(144) or
      PhpFunctionComp(954) or
      PhpFunctionComp(1391) or
      PhpFunctionComp(1635) or
      PhpFunctionComp(1853) or
      PhpFunctionComp(1873) or
      PhpFunctionComp(2279) or
      PhpFunctionComp(2291) or
      PhpFunctionComp(2377) or
      PhpFunctionComp(2604) or
      PhpFunctionComp(2755) or
      PhpFunctionComp(2932) or
      PhpFunctionComp(3090) or
      PhpFunctionComp(3332) or
      PhpFunctionComp(3496) or
      PhpFunctionComp(3498) or
      PhpFunctionComp(3708) or
      PhpFunctionComp(4069) or
      PhpFunctionComp(4075) or
      PhpFunctionComp(4137) or
      PhpFunctionComp(4253) or
      PhpFunctionComp(4401) or
      PhpFunctionComp(4454) or
      PhpFunctionComp(4525) or
      PhpFunctionComp(4745) or
      PhpFunctionComp(4946) or
      PhpFunctionComp(5015) or
      PhpFunctionComp(5063) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc195: TSynWebTokenKind;
begin
  if  PhpFunctionComp(574) or
      PhpFunctionComp(932) or
      PhpFunctionComp(1068) or
      PhpFunctionComp(1302) or
      PhpFunctionComp(1496) or
      PhpFunctionComp(1706) or
      PhpFunctionComp(1879) or
      PhpFunctionComp(2036) or
      PhpFunctionComp(2389) or
      PhpFunctionComp(2888) or
      PhpFunctionComp(3091) or
      PhpFunctionComp(3337) or
      PhpFunctionComp(3456) or
      PhpFunctionComp(3486) or
      PhpFunctionComp(4611) or
      PhpFunctionComp(4824) or
      PhpFunctionComp(4897) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc196: TSynWebTokenKind;
begin
  if  PhpFunctionComp(963) or
      PhpFunctionComp(980) or
      PhpFunctionComp(1054) or
      PhpFunctionComp(1096) or
      PhpFunctionComp(1113) or
      PhpFunctionComp(1834) or
      PhpFunctionComp(1848) or
      PhpFunctionComp(1852) or
      PhpFunctionComp(2156) or
      PhpFunctionComp(2499) or
      PhpFunctionComp(2736) or
      PhpFunctionComp(2894) or
      PhpFunctionComp(2915) or
      PhpFunctionComp(3136) or
      PhpFunctionComp(3181) or
      PhpFunctionComp(3257) or
      PhpFunctionComp(3406) or
      PhpFunctionComp(3511) or
      PhpFunctionComp(3670) or
      PhpFunctionComp(4612) or
      PhpFunctionComp(4714) or
      PhpFunctionComp(4727) or
      PhpFunctionComp(4855) or
      PhpFunctionComp(5013) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc197: TSynWebTokenKind;
begin
  if  PhpFunctionComp(241) or
      PhpFunctionComp(326) or
      PhpFunctionComp(606) or
      PhpFunctionComp(970) or
      PhpFunctionComp(974) or
      PhpFunctionComp(1036) or
      PhpFunctionComp(1566) or
      PhpFunctionComp(1698) or
      PhpFunctionComp(1927) or
      PhpFunctionComp(2195) or
      PhpFunctionComp(2274) or
      PhpFunctionComp(2307) or
      PhpFunctionComp(2563) or
      PhpFunctionComp(2688) or
      PhpFunctionComp(2793) or
      PhpFunctionComp(2998) or
      PhpFunctionComp(3255) or
      PhpFunctionComp(3495) or
      PhpFunctionComp(4026) or
      PhpFunctionComp(4065) or
      PhpFunctionComp(4276) or
      PhpFunctionComp(4469) or
      PhpFunctionComp(4479) or
      PhpFunctionComp(4599) or
      PhpFunctionComp(4659) or
      PhpFunctionComp(4951) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc198: TSynWebTokenKind;
begin
  if  PhpFunctionComp(342) or
      PhpFunctionComp(455) or
      PhpFunctionComp(941) or
      PhpFunctionComp(1383) or
      PhpFunctionComp(1540) or
      PhpFunctionComp(1703) or
      PhpFunctionComp(1747) or
      PhpFunctionComp(1983) or
      PhpFunctionComp(2251) or
      PhpFunctionComp(2312) or
      PhpFunctionComp(2417) or
      PhpFunctionComp(2782) or
      PhpFunctionComp(2892) or
      PhpFunctionComp(2966) or
      PhpFunctionComp(3196) or
      PhpFunctionComp(3261) or
      PhpFunctionComp(3276) or
      PhpFunctionComp(3371) or
      PhpFunctionComp(3521) or
      PhpFunctionComp(3773) or
      PhpFunctionComp(3792) or
      PhpFunctionComp(3992) or
      PhpFunctionComp(4008) or
      PhpFunctionComp(4391) or
      PhpFunctionComp(4624) or
      PhpFunctionComp(4628) or
      PhpFunctionComp(4906) or
      PhpFunctionComp(4964) or
      PhpFunctionComp(5148) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc199: TSynWebTokenKind;
begin
  if  PhpFunctionComp(170) or
      PhpFunctionComp(420) or
      PhpFunctionComp(1072) or
      PhpFunctionComp(1088) or
      PhpFunctionComp(1243) or
      PhpFunctionComp(1309) or
      PhpFunctionComp(1774) or
      PhpFunctionComp(1820) or
      PhpFunctionComp(2220) or
      PhpFunctionComp(2383) or
      PhpFunctionComp(2853) or
      PhpFunctionComp(3247) or
      PhpFunctionComp(3283) or
      PhpFunctionComp(3508) or
      PhpFunctionComp(3524) or
      PhpFunctionComp(3723) or
      PhpFunctionComp(4186) or
      PhpFunctionComp(4192) or
      PhpFunctionComp(4262) or
      PhpFunctionComp(4386) or
      PhpFunctionComp(4558) or
      PhpFunctionComp(4662) or
      PhpFunctionComp(4666) or
      PhpFunctionComp(4667) or
      PhpFunctionComp(5169) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc200: TSynWebTokenKind;
begin
  if  PhpFunctionComp(220) or
      PhpFunctionComp(284) or
      PhpFunctionComp(433) or
      PhpFunctionComp(1395) or
      PhpFunctionComp(1593) or
      PhpFunctionComp(1598) or
      PhpFunctionComp(1965) or
      PhpFunctionComp(2167) or
      PhpFunctionComp(2211) or
      PhpFunctionComp(2246) or
      PhpFunctionComp(2598) or
      PhpFunctionComp(2754) or
      PhpFunctionComp(2909) or
      PhpFunctionComp(2948) or
      PhpFunctionComp(3472) or
      PhpFunctionComp(3725) or
      PhpFunctionComp(4062) or
      PhpFunctionComp(4076) or
      PhpFunctionComp(4207) or
      PhpFunctionComp(4218) or
      PhpFunctionComp(4425) or
      PhpFunctionComp(4679) or
      PhpFunctionComp(4738) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc201: TSynWebTokenKind;
begin
  if  PhpFunctionComp(102) or
      PhpFunctionComp(136) or
      PhpFunctionComp(448) or
      PhpFunctionComp(472) or
      PhpFunctionComp(511) or
      PhpFunctionComp(837) or
      PhpFunctionComp(911) or
      PhpFunctionComp(1544) or
      PhpFunctionComp(1685) or
      PhpFunctionComp(1782) or
      PhpFunctionComp(2322) or
      PhpFunctionComp(2401) or
      PhpFunctionComp(2720) or
      PhpFunctionComp(2766) or
      PhpFunctionComp(2990) or
      PhpFunctionComp(2995) or
      PhpFunctionComp(3408) or
      PhpFunctionComp(3618) or
      PhpFunctionComp(3827) or
      PhpFunctionComp(4043) or
      PhpFunctionComp(4533) or
      PhpFunctionComp(4606) or
      PhpFunctionComp(4607) or
      PhpFunctionComp(4655) or
      PhpFunctionComp(4705) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc202: TSynWebTokenKind;
begin
  if  PhpFunctionComp(463) or
      PhpFunctionComp(474) or
      PhpFunctionComp(934) or
      PhpFunctionComp(1296) or
      PhpFunctionComp(1312) or
      PhpFunctionComp(1408) or
      PhpFunctionComp(1830) or
      PhpFunctionComp(1845) or
      PhpFunctionComp(1851) or
      PhpFunctionComp(2017) or
      PhpFunctionComp(2354) or
      PhpFunctionComp(2475) or
      PhpFunctionComp(2842) or
      PhpFunctionComp(2904) or
      PhpFunctionComp(2965) or
      PhpFunctionComp(3355) or
      PhpFunctionComp(3419) or
      PhpFunctionComp(3551) or
      PhpFunctionComp(3552) or
      PhpFunctionComp(3828) or
      PhpFunctionComp(4353) or
      PhpFunctionComp(4464) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc203: TSynWebTokenKind;
begin
  if  PhpFunctionComp(259) or
      PhpFunctionComp(896) or
      PhpFunctionComp(1316) or
      PhpFunctionComp(1318) or
      PhpFunctionComp(1674) or
      PhpFunctionComp(2397) or
      PhpFunctionComp(2675) or
      PhpFunctionComp(2784) or
      PhpFunctionComp(2991) or
      PhpFunctionComp(3414) or
      PhpFunctionComp(3898) or
      PhpFunctionComp(3958) or
      PhpFunctionComp(4017) or
      PhpFunctionComp(4034) or
      PhpFunctionComp(4149) or
      PhpFunctionComp(4180) or
      PhpFunctionComp(4219) or
      PhpFunctionComp(4274) or
      PhpFunctionComp(4757) or
      PhpFunctionComp(4782) or
      PhpFunctionComp(4912) or
      PhpFunctionComp(5088) or
      PhpFunctionComp(5158) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc204: TSynWebTokenKind;
begin
  if  PhpFunctionComp(187) or
      PhpFunctionComp(560) or
      PhpFunctionComp(1074) or
      PhpFunctionComp(1394) or
      PhpFunctionComp(1673) or
      PhpFunctionComp(2194) or
      PhpFunctionComp(2344) or
      PhpFunctionComp(2762) or
      PhpFunctionComp(2852) or
      PhpFunctionComp(2856) or
      PhpFunctionComp(2873) or
      PhpFunctionComp(2876) or
      PhpFunctionComp(2929) or
      PhpFunctionComp(2953) or
      PhpFunctionComp(3086) or
      PhpFunctionComp(3352) or
      PhpFunctionComp(3787) or
      PhpFunctionComp(3882) or
      PhpFunctionComp(3963) or
      PhpFunctionComp(4099) or
      PhpFunctionComp(4251) or
      PhpFunctionComp(4277) or
      PhpFunctionComp(4657) or
      PhpFunctionComp(4789) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc205: TSynWebTokenKind;
begin
  if  PhpFunctionComp(188) or
      PhpFunctionComp(956) or
      PhpFunctionComp(1138) or
      PhpFunctionComp(1627) or
      PhpFunctionComp(1864) or
      PhpFunctionComp(1888) or
      PhpFunctionComp(2353) or
      PhpFunctionComp(2468) or
      PhpFunctionComp(2679) or
      PhpFunctionComp(2722) or
      PhpFunctionComp(2788) or
      PhpFunctionComp(2997) or
      PhpFunctionComp(3032) or
      PhpFunctionComp(3330) or
      PhpFunctionComp(3757) or
      PhpFunctionComp(3858) or
      PhpFunctionComp(4304) or
      PhpFunctionComp(4325) or
      PhpFunctionComp(4406) or
      PhpFunctionComp(4625) or
      PhpFunctionComp(4658) or
      PhpFunctionComp(4660) or
      PhpFunctionComp(4715) or
      PhpFunctionComp(4774) or
      PhpFunctionComp(4949) or
      PhpFunctionComp(4975) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc206: TSynWebTokenKind;
begin
  if  PhpFunctionComp(83) or
      PhpFunctionComp(263) or
      PhpFunctionComp(279) or
      PhpFunctionComp(373) or
      PhpFunctionComp(631) or
      PhpFunctionComp(886) or
      PhpFunctionComp(1476) or
      PhpFunctionComp(1577) or
      PhpFunctionComp(2351) or
      PhpFunctionComp(2855) or
      PhpFunctionComp(3260) or
      PhpFunctionComp(3344) or
      PhpFunctionComp(3705) or
      PhpFunctionComp(4027) or
      PhpFunctionComp(4029) or
      PhpFunctionComp(4106) or
      PhpFunctionComp(4275) or
      PhpFunctionComp(4542) or
      PhpFunctionComp(4545) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc207: TSynWebTokenKind;
begin
  if  PhpFunctionComp(751) or
      PhpFunctionComp(967) or
      PhpFunctionComp(1026) or
      PhpFunctionComp(1084) or
      PhpFunctionComp(2111) or
      PhpFunctionComp(2345) or
      PhpFunctionComp(2607) or
      PhpFunctionComp(2857) or
      PhpFunctionComp(2858) or
      PhpFunctionComp(2937) or
      PhpFunctionComp(3648) or
      PhpFunctionComp(3701) or
      PhpFunctionComp(3726) or
      PhpFunctionComp(3802) or
      PhpFunctionComp(4006) or
      PhpFunctionComp(4179) or
      PhpFunctionComp(4322) or
      PhpFunctionComp(4617) or
      PhpFunctionComp(4708) or
      PhpFunctionComp(4835) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc208: TSynWebTokenKind;
begin
  if  PhpFunctionComp(661) or
      PhpFunctionComp(826) or
      PhpFunctionComp(1116) or
      PhpFunctionComp(1767) or
      PhpFunctionComp(1993) or
      PhpFunctionComp(2223) or
      PhpFunctionComp(2503) or
      PhpFunctionComp(2596) or
      PhpFunctionComp(2669) or
      PhpFunctionComp(2802) or
      PhpFunctionComp(2959) or
      PhpFunctionComp(2978) or
      PhpFunctionComp(3028) or
      PhpFunctionComp(3543) or
      PhpFunctionComp(3960) or
      PhpFunctionComp(4319) or
      PhpFunctionComp(4343) or
      PhpFunctionComp(5024) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc209: TSynWebTokenKind;
begin
  if  PhpFunctionComp(288) or
      PhpFunctionComp(521) or
      PhpFunctionComp(756) or
      PhpFunctionComp(1019) or
      PhpFunctionComp(2242) or
      PhpFunctionComp(2260) or
      PhpFunctionComp(2352) or
      PhpFunctionComp(2666) or
      PhpFunctionComp(2724) or
      PhpFunctionComp(2815) or
      PhpFunctionComp(2879) or
      PhpFunctionComp(3256) or
      PhpFunctionComp(3334) or
      PhpFunctionComp(3506) or
      PhpFunctionComp(3516) or
      PhpFunctionComp(3780) or
      PhpFunctionComp(4221) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc210: TSynWebTokenKind;
begin
  if  PhpFunctionComp(225) or
      PhpFunctionComp(1257) or
      PhpFunctionComp(2290) or
      PhpFunctionComp(2891) or
      PhpFunctionComp(2979) or
      PhpFunctionComp(3146) or
      PhpFunctionComp(3195) or
      PhpFunctionComp(3920) or
      PhpFunctionComp(4004) or
      PhpFunctionComp(4016) or
      PhpFunctionComp(4349) or
      PhpFunctionComp(4354) or
      PhpFunctionComp(4366) or
      PhpFunctionComp(4557) or
      PhpFunctionComp(4698) or
      PhpFunctionComp(4915) or
      PhpFunctionComp(4976) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc211: TSynWebTokenKind;
begin
  if  PhpFunctionComp(374) or
      PhpFunctionComp(957) or
      PhpFunctionComp(1020) or
      PhpFunctionComp(1488) or
      PhpFunctionComp(1636) or
      PhpFunctionComp(1995) or
      PhpFunctionComp(2509) or
      PhpFunctionComp(2689) or
      PhpFunctionComp(2796) or
      PhpFunctionComp(2907) or
      PhpFunctionComp(2940) or
      PhpFunctionComp(2941) or
      PhpFunctionComp(3333) or
      PhpFunctionComp(3493) or
      PhpFunctionComp(3662) or
      PhpFunctionComp(4042) or
      PhpFunctionComp(4071) or
      PhpFunctionComp(4342) or
      PhpFunctionComp(4408) or
      PhpFunctionComp(4627) or
      PhpFunctionComp(4688) or
      PhpFunctionComp(4703) or
      PhpFunctionComp(4903) or
      PhpFunctionComp(4984) or
      PhpFunctionComp(5043) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc212: TSynWebTokenKind;
begin
  if  PhpFunctionComp(154) or
      PhpFunctionComp(402) or
      PhpFunctionComp(468) or
      PhpFunctionComp(804) or
      PhpFunctionComp(824) or
      PhpFunctionComp(1626) or
      PhpFunctionComp(1876) or
      PhpFunctionComp(2177) or
      PhpFunctionComp(2947) or
      PhpFunctionComp(3709) or
      PhpFunctionComp(4197) or
      PhpFunctionComp(4437) or
      PhpFunctionComp(4480) or
      PhpFunctionComp(4566) or
      PhpFunctionComp(4619) or
      PhpFunctionComp(4686) or
      PhpFunctionComp(4791) or
      PhpFunctionComp(4870) or
      PhpFunctionComp(4939) or
      PhpFunctionComp(5087) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc213: TSynWebTokenKind;
begin
  if  PhpFunctionComp(145) or
      PhpFunctionComp(529) or
      PhpFunctionComp(1970) or
      PhpFunctionComp(2568) or
      PhpFunctionComp(2703) or
      PhpFunctionComp(2798) or
      PhpFunctionComp(2961) or
      PhpFunctionComp(3159) or
      PhpFunctionComp(3163) or
      PhpFunctionComp(3338) or
      PhpFunctionComp(3354) or
      PhpFunctionComp(3404) or
      PhpFunctionComp(3488) or
      PhpFunctionComp(3945) or
      PhpFunctionComp(4700) or
      PhpFunctionComp(4702) or
      PhpFunctionComp(4706) or
      PhpFunctionComp(4815) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc214: TSynWebTokenKind;
begin
  if  PhpFunctionComp(317) or
      PhpFunctionComp(452) or
      PhpFunctionComp(537) or
      PhpFunctionComp(625) or
      PhpFunctionComp(1178) or
      PhpFunctionComp(1307) or
      PhpFunctionComp(1654) or
      PhpFunctionComp(1896) or
      PhpFunctionComp(2602) or
      PhpFunctionComp(2887) or
      PhpFunctionComp(2908) or
      PhpFunctionComp(3264) or
      PhpFunctionComp(3341) or
      PhpFunctionComp(3683) or
      PhpFunctionComp(3974) or
      PhpFunctionComp(4385) or
      PhpFunctionComp(4482) or
      PhpFunctionComp(4553) or
      PhpFunctionComp(4561) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc215: TSynWebTokenKind;
begin
  if  PhpFunctionComp(367) or
      PhpFunctionComp(607) or
      PhpFunctionComp(977) or
      PhpFunctionComp(1050) or
      PhpFunctionComp(1191) or
      PhpFunctionComp(1308) or
      PhpFunctionComp(1485) or
      PhpFunctionComp(1656) or
      PhpFunctionComp(1842) or
      PhpFunctionComp(2517) or
      PhpFunctionComp(2608) or
      PhpFunctionComp(2712) or
      PhpFunctionComp(3643) or
      PhpFunctionComp(3714) or
      PhpFunctionComp(3871) or
      PhpFunctionComp(4187) or
      PhpFunctionComp(4399) or
      PhpFunctionComp(4423) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc216: TSynWebTokenKind;
begin
  if  PhpFunctionComp(180) or
      PhpFunctionComp(206) or
      PhpFunctionComp(419) or
      PhpFunctionComp(616) or
      PhpFunctionComp(618) or
      PhpFunctionComp(910) or
      PhpFunctionComp(1056) or
      PhpFunctionComp(1425) or
      PhpFunctionComp(1477) or
      PhpFunctionComp(1805) or
      PhpFunctionComp(2032) or
      PhpFunctionComp(2281) or
      PhpFunctionComp(2306) or
      PhpFunctionComp(2362) or
      PhpFunctionComp(2394) or
      PhpFunctionComp(2614) or
      PhpFunctionComp(2902) or
      PhpFunctionComp(3144) or
      PhpFunctionComp(3192) or
      PhpFunctionComp(3248) or
      PhpFunctionComp(3890) or
      PhpFunctionComp(3991) or
      PhpFunctionComp(4457) or
      PhpFunctionComp(4559) or
      PhpFunctionComp(4710) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc217: TSynWebTokenKind;
begin
  if  PhpFunctionComp(217) or
      PhpFunctionComp(399) or
      PhpFunctionComp(803) or
      PhpFunctionComp(978) or
      PhpFunctionComp(1051) or
      PhpFunctionComp(1402) or
      PhpFunctionComp(1594) or
      PhpFunctionComp(1843) or
      PhpFunctionComp(2467) or
      PhpFunctionComp(2501) or
      PhpFunctionComp(2506) or
      PhpFunctionComp(2790) or
      PhpFunctionComp(3262) or
      PhpFunctionComp(3450) or
      PhpFunctionComp(3971) or
      PhpFunctionComp(4052) or
      PhpFunctionComp(4664) or
      PhpFunctionComp(4680) or
      PhpFunctionComp(4792) or
      PhpFunctionComp(4913) or
      PhpFunctionComp(4959) or
      PhpFunctionComp(5103) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc218: TSynWebTokenKind;
begin
  if  PhpFunctionComp(362) or
      PhpFunctionComp(364) or
      PhpFunctionComp(392) or
      PhpFunctionComp(1533) or
      PhpFunctionComp(2049) or
      PhpFunctionComp(2783) or
      PhpFunctionComp(2849) or
      PhpFunctionComp(2875) or
      PhpFunctionComp(3340) or
      PhpFunctionComp(3612) or
      PhpFunctionComp(4308) or
      PhpFunctionComp(4333) or
      PhpFunctionComp(4468) or
      PhpFunctionComp(4616) or
      PhpFunctionComp(4672) or
      PhpFunctionComp(4678) or
      PhpFunctionComp(4690) or
      PhpFunctionComp(4711) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc219: TSynWebTokenKind;
begin
  if  PhpFunctionComp(344) or
      PhpFunctionComp(838) or
      PhpFunctionComp(952) or
      PhpFunctionComp(1114) or
      PhpFunctionComp(1322) or
      PhpFunctionComp(2505) or
      PhpFunctionComp(2510) or
      PhpFunctionComp(2591) or
      PhpFunctionComp(2592) or
      PhpFunctionComp(2818) or
      PhpFunctionComp(3617) or
      PhpFunctionComp(3703) or
      PhpFunctionComp(3722) or
      PhpFunctionComp(3824) or
      PhpFunctionComp(3896) or
      PhpFunctionComp(4014) or
      PhpFunctionComp(4526) or
      PhpFunctionComp(4540) or
      PhpFunctionComp(4704) or
      PhpFunctionComp(4930) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc220: TSynWebTokenKind;
begin
  if  PhpFunctionComp(103) or
      PhpFunctionComp(137) or
      PhpFunctionComp(614) or
      PhpFunctionComp(1397) or
      PhpFunctionComp(1495) or
      PhpFunctionComp(1671) or
      PhpFunctionComp(1835) or
      PhpFunctionComp(2300) or
      PhpFunctionComp(2454) or
      PhpFunctionComp(2610) or
      PhpFunctionComp(2690) or
      PhpFunctionComp(2734) or
      PhpFunctionComp(2925) or
      PhpFunctionComp(2956) or
      PhpFunctionComp(3008) or
      PhpFunctionComp(3601) or
      PhpFunctionComp(3699) or
      PhpFunctionComp(4147) or
      PhpFunctionComp(4929) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc221: TSynWebTokenKind;
begin
  if  PhpFunctionComp(806) or
      PhpFunctionComp(808) or
      PhpFunctionComp(830) or
      PhpFunctionComp(1189) or
      PhpFunctionComp(1813) or
      PhpFunctionComp(2163) or
      PhpFunctionComp(2810) or
      PhpFunctionComp(3645) or
      PhpFunctionComp(3717) or
      PhpFunctionComp(3915) or
      PhpFunctionComp(4151) or
      PhpFunctionComp(4183) or
      PhpFunctionComp(4340) or
      PhpFunctionComp(4528) or
      PhpFunctionComp(4633) or
      PhpFunctionComp(4635) or
      PhpFunctionComp(4643) or
      PhpFunctionComp(5091) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc222: TSynWebTokenKind;
begin
  if  PhpFunctionComp(165) or
      PhpFunctionComp(171) or
      PhpFunctionComp(281) or
      PhpFunctionComp(307) or
      PhpFunctionComp(1139) or
      PhpFunctionComp(2699) or
      PhpFunctionComp(2759) or
      PhpFunctionComp(3293) or
      PhpFunctionComp(3755) or
      PhpFunctionComp(3789) or
      PhpFunctionComp(3836) or
      PhpFunctionComp(4012) or
      PhpFunctionComp(4394) or
      PhpFunctionComp(4534) or
      PhpFunctionComp(4693) or
      PhpFunctionComp(4777) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc223: TSynWebTokenKind;
begin
  if  PhpFunctionComp(267) or
      PhpFunctionComp(299) or
      PhpFunctionComp(834) or
      PhpFunctionComp(1576) or
      PhpFunctionComp(2498) or
      PhpFunctionComp(2797) or
      PhpFunctionComp(2916) or
      PhpFunctionComp(4010) or
      PhpFunctionComp(4477) or
      PhpFunctionComp(4694) or
      PhpFunctionComp(4910) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc224: TSynWebTokenKind;
begin
  if  PhpFunctionComp(266) or
      PhpFunctionComp(615) or
      PhpFunctionComp(736) or
      PhpFunctionComp(989) or
      PhpFunctionComp(1153) or
      PhpFunctionComp(1868) or
      PhpFunctionComp(2254) or
      PhpFunctionComp(2280) or
      PhpFunctionComp(2304) or
      PhpFunctionComp(2714) or
      PhpFunctionComp(2765) or
      PhpFunctionComp(2999) or
      PhpFunctionComp(3054) or
      PhpFunctionComp(3203) or
      PhpFunctionComp(3821) or
      PhpFunctionComp(3822) or
      PhpFunctionComp(3884) or
      PhpFunctionComp(3972) or
      PhpFunctionComp(4098) or
      PhpFunctionComp(4198) or
      PhpFunctionComp(4465) or
      PhpFunctionComp(4467) or
      PhpFunctionComp(4473) or
      PhpFunctionComp(4709) or
      PhpFunctionComp(4739) or
      PhpFunctionComp(4989) or
      PhpFunctionComp(5163) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc225: TSynWebTokenKind;
begin
  if  PhpFunctionComp(296) or
      PhpFunctionComp(365) or
      PhpFunctionComp(372) or
      PhpFunctionComp(2497) or
      PhpFunctionComp(2713) or
      PhpFunctionComp(2735) or
      PhpFunctionComp(2906) or
      PhpFunctionComp(2980) or
      PhpFunctionComp(4562) or
      PhpFunctionComp(4673) or
      PhpFunctionComp(5009) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc226: TSynWebTokenKind;
begin
  if  PhpFunctionComp(138) or
      PhpFunctionComp(320) or
      PhpFunctionComp(1892) or
      PhpFunctionComp(1968) or
      PhpFunctionComp(2024) or
      PhpFunctionComp(2430) or
      PhpFunctionComp(3232) or
      PhpFunctionComp(3500) or
      PhpFunctionComp(3649) or
      PhpFunctionComp(3862) or
      PhpFunctionComp(4415) or
      PhpFunctionComp(4485) or
      PhpFunctionComp(4675) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc227: TSynWebTokenKind;
begin
  if  PhpFunctionComp(457) or
      PhpFunctionComp(832) or
      PhpFunctionComp(903) or
      PhpFunctionComp(961) or
      PhpFunctionComp(1313) or
      PhpFunctionComp(1846) or
      PhpFunctionComp(2403) or
      PhpFunctionComp(2507) or
      PhpFunctionComp(2949) or
      PhpFunctionComp(3245) or
      PhpFunctionComp(3363) or
      PhpFunctionComp(3995) or
      PhpFunctionComp(4378) or
      PhpFunctionComp(5001) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc228: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1559) or
      PhpFunctionComp(1696) or
      PhpFunctionComp(1839) or
      PhpFunctionComp(3610) or
      PhpFunctionComp(4334) or
      PhpFunctionComp(4355) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc229: TSynWebTokenKind;
begin
  if  PhpFunctionComp(777) or
      PhpFunctionComp(918) or
      PhpFunctionComp(1558) or
      PhpFunctionComp(1695) or
      PhpFunctionComp(1850) or
      PhpFunctionComp(1867) or
      PhpFunctionComp(2046) or
      PhpFunctionComp(2116) or
      PhpFunctionComp(2392) or
      PhpFunctionComp(2504) or
      PhpFunctionComp(2950) or
      PhpFunctionComp(2983) or
      PhpFunctionComp(3036) or
      PhpFunctionComp(3451) or
      PhpFunctionComp(3734) or
      PhpFunctionComp(3879) or
      PhpFunctionComp(3897) or
      PhpFunctionComp(4050) or
      PhpFunctionComp(4055) or
      PhpFunctionComp(4138) or
      PhpFunctionComp(4458) or
      PhpFunctionComp(4990) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc230: TSynWebTokenKind;
begin
  if  PhpFunctionComp(248) or
      PhpFunctionComp(404) or
      PhpFunctionComp(2196) or
      PhpFunctionComp(3013) or
      PhpFunctionComp(3055) or
      PhpFunctionComp(3814) or
      PhpFunctionComp(4350) or
      PhpFunctionComp(4551) or
      PhpFunctionComp(4712) or
      PhpFunctionComp(4724) or
      PhpFunctionComp(4966) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc231: TSynWebTokenKind;
begin
  if  PhpFunctionComp(224) or
      PhpFunctionComp(378) or
      PhpFunctionComp(1618) or
      PhpFunctionComp(2667) or
      PhpFunctionComp(3345) or
      PhpFunctionComp(3633) or
      PhpFunctionComp(3712) or
      PhpFunctionComp(3867) or
      PhpFunctionComp(4299) or
      PhpFunctionComp(4402) or
      PhpFunctionComp(4726) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc232: TSynWebTokenKind;
begin
  if  PhpFunctionComp(568) or
      PhpFunctionComp(890) or
      PhpFunctionComp(1132) or
      PhpFunctionComp(1299) or
      PhpFunctionComp(1847) or
      PhpFunctionComp(1874) or
      PhpFunctionComp(2747) or
      PhpFunctionComp(2757) or
      PhpFunctionComp(2773) or
      PhpFunctionComp(2848) or
      PhpFunctionComp(2900) or
      PhpFunctionComp(3721) or
      PhpFunctionComp(3833) or
      PhpFunctionComp(4223) or
      PhpFunctionComp(4530) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc233: TSynWebTokenKind;
begin
  if  PhpFunctionComp(216) or
      PhpFunctionComp(884) or
      PhpFunctionComp(976) or
      PhpFunctionComp(2457) or
      PhpFunctionComp(2668) or
      PhpFunctionComp(2769) or
      PhpFunctionComp(2811) or
      PhpFunctionComp(2963) or
      PhpFunctionComp(2970) or
      PhpFunctionComp(3504) or
      PhpFunctionComp(3724) or
      PhpFunctionComp(5071) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc234: TSynWebTokenKind;
begin
  if  PhpFunctionComp(85) or
      PhpFunctionComp(567) or
      PhpFunctionComp(1192) or
      PhpFunctionComp(1557) or
      PhpFunctionComp(1657) or
      PhpFunctionComp(2566) or
      PhpFunctionComp(2767) or
      PhpFunctionComp(2823) or
      PhpFunctionComp(2927) or
      PhpFunctionComp(3000) or
      PhpFunctionComp(3035) or
      PhpFunctionComp(3277) or
      PhpFunctionComp(3342) or
      PhpFunctionComp(3497) or
      PhpFunctionComp(3830) or
      PhpFunctionComp(3891) or
      PhpFunctionComp(3959) or
      PhpFunctionComp(4332) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc235: TSynWebTokenKind;
begin
  if  PhpFunctionComp(620) or
      PhpFunctionComp(906) or
      PhpFunctionComp(1283) or
      PhpFunctionComp(1332) or
      PhpFunctionComp(2830) or
      PhpFunctionComp(2930) or
      PhpFunctionComp(3553) or
      PhpFunctionComp(4018) or
      PhpFunctionComp(4070) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc236: TSynWebTokenKind;
begin
  if  PhpFunctionComp(459) or
      PhpFunctionComp(1119) or
      PhpFunctionComp(1480) or
      PhpFunctionComp(1833) or
      PhpFunctionComp(2115) or
      PhpFunctionComp(2584) or
      PhpFunctionComp(2992) or
      PhpFunctionComp(3020) or
      PhpFunctionComp(3060) or
      PhpFunctionComp(3943) or
      PhpFunctionComp(3973) or
      PhpFunctionComp(4048) or
      PhpFunctionComp(4072) or
      PhpFunctionComp(4190) or
      PhpFunctionComp(4226) or
      PhpFunctionComp(4305) or
      PhpFunctionComp(4486) or
      PhpFunctionComp(4638) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc237: TSynWebTokenKind;
begin
  if  PhpFunctionComp(366) or
      PhpFunctionComp(397) or
      PhpFunctionComp(1687) or
      PhpFunctionComp(1709) or
      PhpFunctionComp(1840) or
      PhpFunctionComp(2407) or
      PhpFunctionComp(2812) or
      PhpFunctionComp(4288) or
      PhpFunctionComp(4608) or
      PhpFunctionComp(5003) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc238: TSynWebTokenKind;
begin
  if  PhpFunctionComp(219) or
      PhpFunctionComp(555) or
      PhpFunctionComp(694) or
      PhpFunctionComp(892) or
      PhpFunctionComp(916) or
      PhpFunctionComp(984) or
      PhpFunctionComp(2388) or
      PhpFunctionComp(2493) or
      PhpFunctionComp(2569) or
      PhpFunctionComp(2673) or
      PhpFunctionComp(3021) or
      PhpFunctionComp(3268) or
      PhpFunctionComp(4033) or
      PhpFunctionComp(4039) or
      PhpFunctionComp(4150) or
      PhpFunctionComp(4181) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc239: TSynWebTokenKind;
begin
  if  PhpFunctionComp(609) or
      PhpFunctionComp(1305) or
      PhpFunctionComp(1644) or
      PhpFunctionComp(1645) or
      PhpFunctionComp(2387) or
      PhpFunctionComp(2491) or
      PhpFunctionComp(2741) or
      PhpFunctionComp(2751) or
      PhpFunctionComp(2944) or
      PhpFunctionComp(2969) or
      PhpFunctionComp(3364) or
      PhpFunctionComp(4038) or
      PhpFunctionComp(5022) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc240: TSynWebTokenKind;
begin
  if  PhpFunctionComp(965) or
      PhpFunctionComp(981) or
      PhpFunctionComp(1075) or
      PhpFunctionComp(1107) or
      PhpFunctionComp(2378) or
      PhpFunctionComp(2801) or
      PhpFunctionComp(2962) or
      PhpFunctionComp(3274) or
      PhpFunctionComp(3339) or
      PhpFunctionComp(3978) or
      PhpFunctionComp(4632) or
      PhpFunctionComp(4904) or
      PhpFunctionComp(5027) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc241: TSynWebTokenKind;
begin
  if  PhpFunctionComp(86) or
      PhpFunctionComp(930) or
      PhpFunctionComp(992) or
      PhpFunctionComp(1043) or
      PhpFunctionComp(1133) or
      PhpFunctionComp(1226) or
      PhpFunctionComp(1482) or
      PhpFunctionComp(2342) or
      PhpFunctionComp(2376) or
      PhpFunctionComp(2813) or
      PhpFunctionComp(2957) or
      PhpFunctionComp(3088) or
      PhpFunctionComp(3326) or
      PhpFunctionComp(3655) or
      PhpFunctionComp(3886) or
      PhpFunctionComp(4699) or
      PhpFunctionComp(4701) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc242: TSynWebTokenKind;
begin
  if  PhpFunctionComp(111) or
      PhpFunctionComp(152) or
      PhpFunctionComp(962) or
      PhpFunctionComp(1856) or
      PhpFunctionComp(2367) or
      PhpFunctionComp(3918) or
      PhpFunctionComp(4110) or
      PhpFunctionComp(4161) or
      PhpFunctionComp(4178) or
      PhpFunctionComp(5054) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc243: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1534) or
      PhpFunctionComp(2546) or
      PhpFunctionComp(2742) or
      PhpFunctionComp(2792) or
      PhpFunctionComp(3298) or
      PhpFunctionComp(3864) or
      PhpFunctionComp(4095) or
      PhpFunctionComp(4307) or
      PhpFunctionComp(4410) or
      PhpFunctionComp(4431) or
      PhpFunctionComp(4609) or
      PhpFunctionComp(4681) or
      PhpFunctionComp(4943) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc244: TSynWebTokenKind;
begin
  if  PhpFunctionComp(400) or
      PhpFunctionComp(590) or
      PhpFunctionComp(825) or
      PhpFunctionComp(887) or
      PhpFunctionComp(969) or
      PhpFunctionComp(982) or
      PhpFunctionComp(1878) or
      PhpFunctionComp(2951) or
      PhpFunctionComp(3347) or
      PhpFunctionComp(4228) or
      PhpFunctionComp(4413) or
      PhpFunctionComp(4634) or
      PhpFunctionComp(5115) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc245: TSynWebTokenKind;
begin
  if  PhpFunctionComp(909) or
      PhpFunctionComp(936) or
      PhpFunctionComp(1080) or
      PhpFunctionComp(1081) or
      PhpFunctionComp(1281) or
      PhpFunctionComp(2385) or
      PhpFunctionComp(2808) or
      PhpFunctionComp(4565) or
      PhpFunctionComp(4765) or
      PhpFunctionComp(5042) or
      PhpFunctionComp(5048) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc246: TSynWebTokenKind;
begin
  if  PhpFunctionComp(178) or
      PhpFunctionComp(589) or
      PhpFunctionComp(805) or
      PhpFunctionComp(869) or
      PhpFunctionComp(902) or
      PhpFunctionComp(2301) or
      PhpFunctionComp(2715) or
      PhpFunctionComp(2822) or
      PhpFunctionComp(3368) or
      PhpFunctionComp(3935) or
      PhpFunctionComp(4108) or
      PhpFunctionComp(4189) or
      PhpFunctionComp(4438) or
      PhpFunctionComp(4463) or
      PhpFunctionComp(4631) or
      PhpFunctionComp(4750) or
      PhpFunctionComp(5046) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc247: TSynWebTokenKind;
begin
  if  PhpFunctionComp(110) or
      PhpFunctionComp(174) or
      PhpFunctionComp(285) or
      PhpFunctionComp(619) or
      PhpFunctionComp(958) or
      PhpFunctionComp(1306) or
      PhpFunctionComp(1310) or
      PhpFunctionComp(1549) or
      PhpFunctionComp(2748) or
      PhpFunctionComp(3001) or
      PhpFunctionComp(3837) or
      PhpFunctionComp(4419) or
      PhpFunctionComp(4466) or
      PhpFunctionComp(4687) or
      PhpFunctionComp(4986) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc248: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2441) or
      PhpFunctionComp(3525) or
      PhpFunctionComp(5016) or
      PhpFunctionComp(5036) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc249: TSynWebTokenKind;
begin
  if  PhpFunctionComp(361) or
      PhpFunctionComp(566) or
      PhpFunctionComp(617) or
      PhpFunctionComp(926) or
      PhpFunctionComp(1849) or
      PhpFunctionComp(2492) or
      PhpFunctionComp(2716) or
      PhpFunctionComp(2737) or
      PhpFunctionComp(4185) or
      PhpFunctionComp(4484) or
      PhpFunctionComp(4713) or
      PhpFunctionComp(5033) or
      PhpFunctionComp(5070) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc250: TSynWebTokenKind;
begin
  if  PhpFunctionComp(308) or
      PhpFunctionComp(556) or
      PhpFunctionComp(835) or
      PhpFunctionComp(857) or
      PhpFunctionComp(1295) or
      PhpFunctionComp(1686) or
      PhpFunctionComp(2814) or
      PhpFunctionComp(3258) or
      PhpFunctionComp(3275) or
      PhpFunctionComp(3636) or
      PhpFunctionComp(3893) or
      PhpFunctionComp(4383) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc251: TSynWebTokenKind;
begin
  if  PhpFunctionComp(475) or
      PhpFunctionComp(513) or
      PhpFunctionComp(1010) or
      PhpFunctionComp(1285) or
      PhpFunctionComp(1286) or
      PhpFunctionComp(1287) or
      PhpFunctionComp(1288) or
      PhpFunctionComp(1289) or
      PhpFunctionComp(2490) or
      PhpFunctionComp(3967) or
      PhpFunctionComp(4040) or
      PhpFunctionComp(4148) or
      PhpFunctionComp(4546) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc252: TSynWebTokenKind;
begin
  if  PhpFunctionComp(510) or
      PhpFunctionComp(1085) or
      PhpFunctionComp(1642) or
      PhpFunctionComp(2058) or
      PhpFunctionComp(2464) or
      PhpFunctionComp(2746) or
      PhpFunctionComp(3061) or
      PhpFunctionComp(3282) or
      PhpFunctionComp(3403) or
      PhpFunctionComp(4300) or
      PhpFunctionComp(4636) or
      PhpFunctionComp(4676) or
      PhpFunctionComp(4718) or
      PhpFunctionComp(5029) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc253: TSynWebTokenKind;
begin
  if  PhpFunctionComp(450) or
      PhpFunctionComp(814) or
      PhpFunctionComp(870) or
      PhpFunctionComp(1658) or
      PhpFunctionComp(2781) or
      PhpFunctionComp(2854) or
      PhpFunctionComp(3803) or
      PhpFunctionComp(3913) or
      PhpFunctionComp(4046) or
      PhpFunctionComp(4184) or
      PhpFunctionComp(4193) or
      PhpFunctionComp(4478) or
      PhpFunctionComp(4637) or
      PhpFunctionComp(4969) or
      PhpFunctionComp(5085) or
      PhpFunctionComp(5116) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc254: TSynWebTokenKind;
begin
  if  PhpFunctionComp(106) or
      PhpFunctionComp(218) or
      PhpFunctionComp(242) or
      PhpFunctionComp(851) or
      PhpFunctionComp(959) or
      PhpFunctionComp(971) or
      PhpFunctionComp(979) or
      PhpFunctionComp(991) or
      PhpFunctionComp(1303) or
      PhpFunctionComp(1664) or
      PhpFunctionComp(2831) or
      PhpFunctionComp(3286) or
      PhpFunctionComp(3964) or
      PhpFunctionComp(4036) or
      PhpFunctionComp(4175) or
      PhpFunctionComp(4719) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc255: TSynWebTokenKind;
begin
  if  PhpFunctionComp(166) or
      PhpFunctionComp(172) or
      PhpFunctionComp(418) or
      PhpFunctionComp(966) or
      PhpFunctionComp(995) or
      PhpFunctionComp(1326) or
      PhpFunctionComp(1489) or
      PhpFunctionComp(1556) or
      PhpFunctionComp(2461) or
      PhpFunctionComp(2770) or
      PhpFunctionComp(2987) or
      PhpFunctionComp(3278) or
      PhpFunctionComp(5112) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc256: TSynWebTokenKind;
begin
  if  PhpFunctionComp(822) or
      PhpFunctionComp(1713) or
      PhpFunctionComp(1841) or
      PhpFunctionComp(2136) or
      PhpFunctionComp(2780) or
      PhpFunctionComp(4395) or
      PhpFunctionComp(4543) or
      PhpFunctionComp(4644) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc257: TSynWebTokenKind;
begin
  if  PhpFunctionComp(153) or
      PhpFunctionComp(1614) or
      PhpFunctionComp(1690) or
      PhpFunctionComp(2778) or
      PhpFunctionComp(3621) or
      PhpFunctionComp(3922) or
      PhpFunctionComp(4287) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc258: TSynWebTokenKind;
begin
  if  PhpFunctionComp(850) or
      PhpFunctionComp(1016) or
      PhpFunctionComp(1230) or
      PhpFunctionComp(1493) or
      PhpFunctionComp(1621) or
      PhpFunctionComp(2325) or
      PhpFunctionComp(2728) or
      PhpFunctionComp(2771) or
      PhpFunctionComp(2806) or
      PhpFunctionComp(4031) or
      PhpFunctionComp(4153) or
      PhpFunctionComp(4418) or
      PhpFunctionComp(4630) or
      PhpFunctionComp(4647) or
      PhpFunctionComp(4677) or
      PhpFunctionComp(4988) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc259: TSynWebTokenKind;
begin
  if  PhpFunctionComp(833) or
      PhpFunctionComp(998) or
      PhpFunctionComp(1280) or
      PhpFunctionComp(2861) or
      PhpFunctionComp(2938) or
      PhpFunctionComp(2939) or
      PhpFunctionComp(3251) or
      PhpFunctionComp(3353) or
      PhpFunctionComp(4430) or
      PhpFunctionComp(4544) or
      PhpFunctionComp(4751) or
      PhpFunctionComp(4987) or
      PhpFunctionComp(5055) or
      PhpFunctionComp(5157) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc260: TSynWebTokenKind;
begin
  if  PhpFunctionComp(946) or
      PhpFunctionComp(1045) or
      PhpFunctionComp(2779) or
      PhpFunctionComp(4641) or
      PhpFunctionComp(4717) or
      PhpFunctionComp(5017) or
      PhpFunctionComp(5089) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc261: TSynWebTokenKind;
begin
  if  PhpFunctionComp(375) or
      PhpFunctionComp(588) or
      PhpFunctionComp(1551) or
      PhpFunctionComp(1592) or
      PhpFunctionComp(1595) or
      PhpFunctionComp(1597) or
      PhpFunctionComp(1705) or
      PhpFunctionComp(1855) or
      PhpFunctionComp(3272) or
      PhpFunctionComp(3323) or
      PhpFunctionComp(3754) or
      PhpFunctionComp(3965) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc262: TSynWebTokenKind;
begin
  if  PhpFunctionComp(167) or
      PhpFunctionComp(812) or
      PhpFunctionComp(919) or
      PhpFunctionComp(922) or
      PhpFunctionComp(1404) or
      PhpFunctionComp(3132) or
      PhpFunctionComp(3197) or
      PhpFunctionComp(3252) or
      PhpFunctionComp(3912) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc263: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1071) or
      PhpFunctionComp(1573) or
      PhpFunctionComp(3249) or
      PhpFunctionComp(4141) or
      PhpFunctionComp(4159) or
      PhpFunctionComp(4188) or
      PhpFunctionComp(4404) or
      PhpFunctionComp(4455) or
      PhpFunctionComp(4649) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc264: TSynWebTokenKind;
begin
  if  PhpFunctionComp(527) or
      PhpFunctionComp(943) or
      PhpFunctionComp(2931) or
      PhpFunctionComp(3287) or
      PhpFunctionComp(3292) or
      PhpFunctionComp(3713) or
      PhpFunctionComp(4421) or
      PhpFunctionComp(4651) or
      PhpFunctionComp(4944) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc265: TSynWebTokenKind;
begin
  if  PhpFunctionComp(875) or
      PhpFunctionComp(986) or
      PhpFunctionComp(1479) or
      PhpFunctionComp(2041) or
      PhpFunctionComp(2346) or
      PhpFunctionComp(2721) or
      PhpFunctionComp(2819) or
      PhpFunctionComp(3961) or
      PhpFunctionComp(4224) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc266: TSynWebTokenKind;
begin
  if  PhpFunctionComp(186) or
      PhpFunctionComp(376) or
      PhpFunctionComp(862) or
      PhpFunctionComp(1073) or
      PhpFunctionComp(1838) or
      PhpFunctionComp(3892) or
      PhpFunctionComp(4881) or
      PhpFunctionComp(5008) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc267: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1172) or
      PhpFunctionComp(1569) or
      PhpFunctionComp(2789) or
      PhpFunctionComp(2827) or
      PhpFunctionComp(4172) or
      PhpFunctionComp(4982) or
      PhpFunctionComp(5020) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc268: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1079) or
      PhpFunctionComp(1399) or
      PhpFunctionComp(4472) or
      PhpFunctionComp(4945) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc269: TSynWebTokenKind;
begin
  if  PhpFunctionComp(462) or
      PhpFunctionComp(915) or
      PhpFunctionComp(929) or
      PhpFunctionComp(3838) or
      PhpFunctionComp(4432) or
      PhpFunctionComp(4434) or
      PhpFunctionComp(4439) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc270: TSynWebTokenKind;
begin
  if  PhpFunctionComp(286) or
      PhpFunctionComp(624) or
      PhpFunctionComp(933) or
      PhpFunctionComp(2807) or
      PhpFunctionComp(3250) or
      PhpFunctionComp(4045) or
      PhpFunctionComp(4335) or
      PhpFunctionComp(4426) or
      PhpFunctionComp(4833) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc271: TSynWebTokenKind;
begin
  if  PhpFunctionComp(914) or
      PhpFunctionComp(2321) or
      PhpFunctionComp(2459) or
      PhpFunctionComp(2518) or
      PhpFunctionComp(2989) or
      PhpFunctionComp(3719) or
      PhpFunctionComp(4041) or
      PhpFunctionComp(4639) or
      PhpFunctionComp(4948) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc272: TSynWebTokenKind;
begin
  if  PhpFunctionComp(623) or
      PhpFunctionComp(924) or
      PhpFunctionComp(990) or
      PhpFunctionComp(1315) or
      PhpFunctionComp(4289) or
      PhpFunctionComp(4716) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc273: TSynWebTokenKind;
begin
  if  PhpFunctionComp(819) or
      PhpFunctionComp(1032) or
      PhpFunctionComp(1619) or
      PhpFunctionComp(2516) or
      PhpFunctionComp(2901) or
      PhpFunctionComp(3944) or
      PhpFunctionComp(3966) or
      PhpFunctionComp(4373) or
      PhpFunctionComp(4488) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc274: TSynWebTokenKind;
begin
  if  PhpFunctionComp(282) or
      PhpFunctionComp(746) or
      PhpFunctionComp(2305) or
      PhpFunctionComp(2733) or
      PhpFunctionComp(3994) or
      PhpFunctionComp(4054) or
      PhpFunctionComp(4474) or
      PhpFunctionComp(4487) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc275: TSynWebTokenKind;
begin
  if  PhpFunctionComp(243) or
      PhpFunctionComp(1077) or
      PhpFunctionComp(1531) or
      PhpFunctionComp(2996) or
      PhpFunctionComp(3996) or
      PhpFunctionComp(4436) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc276: TSynWebTokenKind;
begin
  if  PhpFunctionComp(571) or
      PhpFunctionComp(621) or
      PhpFunctionComp(948) or
      PhpFunctionComp(994) or
      PhpFunctionComp(1944) or
      PhpFunctionComp(2391) or
      PhpFunctionComp(2816) or
      PhpFunctionComp(3619) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc277: TSynWebTokenKind;
begin
  if  PhpFunctionComp(176) or
      PhpFunctionComp(2508) or
      PhpFunctionComp(2973) or
      PhpFunctionComp(3962) or
      PhpFunctionComp(4037) or
      PhpFunctionComp(4229) or
      PhpFunctionComp(4411) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc278: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1039) or
      PhpFunctionComp(1041) or
      PhpFunctionComp(1052) or
      PhpFunctionComp(3087) or
      PhpFunctionComp(4422) or
      PhpFunctionComp(4459) or
      PhpFunctionComp(4645) or
      PhpFunctionComp(4723) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc279: TSynWebTokenKind;
begin
  if  PhpFunctionComp(866) or
      PhpFunctionComp(2296) or
      PhpFunctionComp(4400) or
      PhpFunctionComp(4991) or
      PhpFunctionComp(5010) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc280: TSynWebTokenKind;
begin
  if  PhpFunctionComp(622) or
      PhpFunctionComp(2825) or
      PhpFunctionComp(3281) or
      PhpFunctionComp(3999) or
      PhpFunctionComp(4286) or
      PhpFunctionComp(4947) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc281: TSynWebTokenKind;
begin
  if  PhpFunctionComp(173) or
      PhpFunctionComp(283) or
      PhpFunctionComp(4564) or
      PhpFunctionComp(4843) or
      PhpFunctionComp(5014) or
      PhpFunctionComp(5026) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc282: TSynWebTokenKind;
begin
  if  PhpFunctionComp(836) or
      PhpFunctionComp(878) or
      PhpFunctionComp(964) or
      PhpFunctionComp(3284) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc283: TSynWebTokenKind;
begin
  if  PhpFunctionComp(858) or
      PhpFunctionComp(937) or
      PhpFunctionComp(1048) or
      PhpFunctionComp(4801) or
      PhpFunctionComp(4880) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc284: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1617) or
      PhpFunctionComp(4157) or
      PhpFunctionComp(4550) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc285: TSynWebTokenKind;
begin
  if  PhpFunctionComp(201) or
      PhpFunctionComp(891) or
      PhpFunctionComp(997) or
      PhpFunctionComp(2013) or
      PhpFunctionComp(2465) or
      PhpFunctionComp(3832) or
      PhpFunctionComp(4481) or
      PhpFunctionComp(4731) or
      PhpFunctionComp(4919) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc286: TSynWebTokenKind;
begin
  if  PhpFunctionComp(815) or
      PhpFunctionComp(818) or
      PhpFunctionComp(871) or
      PhpFunctionComp(3291) or
      PhpFunctionComp(4168) or
      PhpFunctionComp(5021) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc287: TSynWebTokenKind;
begin
  if  PhpFunctionComp(854) or
      PhpFunctionComp(1708) or
      PhpFunctionComp(4203) or
      PhpFunctionComp(4204) or
      PhpFunctionComp(4313) or
      PhpFunctionComp(4993) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc288: TSynWebTokenKind;
begin
  if  PhpFunctionComp(593) or
      PhpFunctionComp(2069) or
      PhpFunctionComp(2393) or
      PhpFunctionComp(3059) or
      PhpFunctionComp(4154) or
      PhpFunctionComp(4320) or
      PhpFunctionComp(4648) or
      PhpFunctionComp(4977) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc289: TSynWebTokenKind;
begin
  if  PhpFunctionComp(972) or
      PhpFunctionComp(1070) or
      PhpFunctionComp(2821) or
      PhpFunctionComp(3294) or
      PhpFunctionComp(4462) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc290: TSynWebTokenKind;
begin
  if  PhpFunctionComp(526) or
      PhpFunctionComp(807) or
      PhpFunctionComp(1087) or
      PhpFunctionComp(2466) or
      PhpFunctionComp(2820) or
      PhpFunctionComp(2955) or
      PhpFunctionComp(5025) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc291: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1300) or
      PhpFunctionComp(2495) or
      PhpFunctionComp(2809) or
      PhpFunctionComp(5052) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc292: TSynWebTokenKind;
begin
  if  PhpFunctionComp(852) or
      PhpFunctionComp(1293) or
      PhpFunctionComp(2768) or
      PhpFunctionComp(2805) or
      PhpFunctionComp(3285) or
      PhpFunctionComp(3916) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc293: TSynWebTokenKind;
begin
  if  PhpFunctionComp(811) or
      PhpFunctionComp(901) or
      PhpFunctionComp(1655) or
      PhpFunctionComp(2794) or
      PhpFunctionComp(4740) or
      PhpFunctionComp(4979) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc294: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1013) or
      PhpFunctionComp(3894) or
      PhpFunctionComp(4227) or
      PhpFunctionComp(5019) or
      PhpFunctionComp(5040) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc295: TSynWebTokenKind;
begin
  if  PhpFunctionComp(179) or
      PhpFunctionComp(809) or
      PhpFunctionComp(898) or
      PhpFunctionComp(899) or
      PhpFunctionComp(2971) or
      PhpFunctionComp(4642) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc296: TSynWebTokenKind;
begin
  if  PhpFunctionComp(169) or
      PhpFunctionComp(935) or
      PhpFunctionComp(2817) or
      PhpFunctionComp(4435) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc297: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4005) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc298: TSynWebTokenKind;
begin
  if  PhpFunctionComp(398) or
      PhpFunctionComp(876) or
      PhpFunctionComp(1000) or
      PhpFunctionComp(1537) or
      PhpFunctionComp(2218) or
      PhpFunctionComp(4650) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc299: TSynWebTokenKind;
begin
  if  PhpFunctionComp(155) or
      PhpFunctionComp(427) or
      PhpFunctionComp(865) or
      PhpFunctionComp(873) or
      PhpFunctionComp(1227) or
      PhpFunctionComp(1615) or
      PhpFunctionComp(2080) or
      PhpFunctionComp(2800) or
      PhpFunctionComp(3976) or
      PhpFunctionComp(4158) or
      PhpFunctionComp(4652) or
      PhpFunctionComp(4742) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc300: TSynWebTokenKind;
begin
  if  PhpFunctionComp(181) or
      PhpFunctionComp(859) or
      PhpFunctionComp(1620) or
      PhpFunctionComp(5047) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc302: TSynWebTokenKind;
begin
  if  PhpFunctionComp(944) or
      PhpFunctionComp(2749) or
      PhpFunctionComp(4721) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc303: TSynWebTokenKind;
begin
  if  PhpFunctionComp(987) or
      PhpFunctionComp(2198) or
      PhpFunctionComp(5034) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc304: TSynWebTokenKind;
begin
  if  PhpFunctionComp(863) or
      PhpFunctionComp(2489) or
      PhpFunctionComp(4164) or
      PhpFunctionComp(4741) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc305: TSynWebTokenKind;
begin
  if  PhpFunctionComp(289) or
      PhpFunctionComp(1714) or
      PhpFunctionComp(3269) or
      PhpFunctionComp(4173) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc306: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2799) or
      PhpFunctionComp(4722) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc307: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1304) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc308: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2672) or
      PhpFunctionComp(3997) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc309: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1711) or
      PhpFunctionComp(2750) or
      PhpFunctionComp(4013) or
      PhpFunctionComp(4384) or
      PhpFunctionComp(4416) or
      PhpFunctionComp(4489) or
      PhpFunctionComp(5161) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc310: TSynWebTokenKind;
begin
  if  PhpFunctionComp(947) or
      PhpFunctionComp(1002) or
      PhpFunctionComp(2302) or
      PhpFunctionComp(4461) or
      PhpFunctionComp(4640) or
      PhpFunctionComp(4646) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc311: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2297) or
      PhpFunctionComp(4160) or
      PhpFunctionComp(4163) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc312: TSynWebTokenKind;
begin
  if  PhpFunctionComp(868) or
      PhpFunctionComp(2197) or
      PhpFunctionComp(2470) or
      PhpFunctionComp(2553) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc313: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2671) or
      PhpFunctionComp(4942) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc314: TSynWebTokenKind;
begin
  if  PhpFunctionComp(156) or
      PhpFunctionComp(828) or
      PhpFunctionComp(949) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc315: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5035) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc316: TSynWebTokenKind;
begin
  if  PhpFunctionComp(927) or
      PhpFunctionComp(975) or
      PhpFunctionComp(1014) or
      PhpFunctionComp(2772) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc317: TSynWebTokenKind;
begin
  if  PhpFunctionComp(867) or
      PhpFunctionComp(3058) or
      PhpFunctionComp(4460) or
      PhpFunctionComp(4743) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc318: TSynWebTokenKind;
begin
  if  PhpFunctionComp(842) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc319: TSynWebTokenKind;
begin
  if  PhpFunctionComp(855) or
      PhpFunctionComp(5050) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc321: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1040) or
      PhpFunctionComp(4725) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc322: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2745) or
      PhpFunctionComp(3279) or
      PhpFunctionComp(4720) or
      PhpFunctionComp(5038) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc323: TSynWebTokenKind;
begin
  if  PhpFunctionComp(175) or
      PhpFunctionComp(1550) or
      PhpFunctionComp(1662) or
      PhpFunctionComp(4165) or
      PhpFunctionComp(4403) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc324: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5053) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc325: TSynWebTokenKind;
begin
  if  PhpFunctionComp(853) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc326: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1015) or
      PhpFunctionComp(1078) or
      PhpFunctionComp(1616) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc327: TSynWebTokenKind;
begin
  if  PhpFunctionComp(920) or
      PhpFunctionComp(2824) or
      PhpFunctionComp(3934) or
      PhpFunctionComp(5041) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc328: TSynWebTokenKind;
begin
  if  PhpFunctionComp(810) or
      PhpFunctionComp(2496) or
      PhpFunctionComp(2839) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc329: TSynWebTokenKind;
begin
  if  PhpFunctionComp(945) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc330: TSynWebTokenKind;
begin
  if  PhpFunctionComp(996) or
      PhpFunctionComp(1568) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc331: TSynWebTokenKind;
begin
  if  PhpFunctionComp(829) or
      PhpFunctionComp(1001) or
      PhpFunctionComp(4978) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc332: TSynWebTokenKind;
begin
  if  PhpFunctionComp(874) or
      PhpFunctionComp(2512) or
      PhpFunctionComp(3895) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc333: TSynWebTokenKind;
begin
  if  PhpFunctionComp(861) or
      PhpFunctionComp(5044) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc334: TSynWebTokenKind;
begin
  if  PhpFunctionComp(185) or
      PhpFunctionComp(846) or
      PhpFunctionComp(1294) or
      PhpFunctionComp(2511) or
      PhpFunctionComp(4170) or
      PhpFunctionComp(4996) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc335: TSynWebTokenKind;
begin
  if  PhpFunctionComp(827) or
      PhpFunctionComp(1570) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc336: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1012) or
      PhpFunctionComp(5031) or
      PhpFunctionComp(5156) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc337: TSynWebTokenKind;
begin
  if  PhpFunctionComp(847) or
      PhpFunctionComp(864) or
      PhpFunctionComp(1011) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc338: TSynWebTokenKind;
begin
  if  PhpFunctionComp(182) or
      PhpFunctionComp(845) or
      PhpFunctionComp(860) or
      PhpFunctionComp(3324) or
      PhpFunctionComp(4166) or
      PhpFunctionComp(5051) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc339: TSynWebTokenKind;
begin
  if  PhpFunctionComp(917) or
      PhpFunctionComp(2514) or
      PhpFunctionComp(2826) or
      PhpFunctionComp(4420) or
      PhpFunctionComp(5012) or
      PhpFunctionComp(5030) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc340: TSynWebTokenKind;
begin
  if  PhpFunctionComp(820) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc341: TSynWebTokenKind;
begin
  if  PhpFunctionComp(938) or
      PhpFunctionComp(4169) or
      PhpFunctionComp(5039) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc343: TSynWebTokenKind;
begin
  if  PhpFunctionComp(183) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc344: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5028) or
      PhpFunctionComp(5162) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc345: TSynWebTokenKind;
begin
  if  PhpFunctionComp(840) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc346: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1572) or
      PhpFunctionComp(5049) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc347: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3977) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc348: TSynWebTokenKind;
begin
  if  PhpFunctionComp(168) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc349: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5037) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc351: TSynWebTokenKind;
begin
  if  PhpFunctionComp(816) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc352: TSynWebTokenKind;
begin
  if  PhpFunctionComp(856) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc353: TSynWebTokenKind;
begin
  if  PhpFunctionComp(881) or
      PhpFunctionComp(4171) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc354: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2462) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc357: TSynWebTokenKind;
begin
  if  PhpFunctionComp(841) or
      PhpFunctionComp(2294) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc358: TSynWebTokenKind;
begin
  if  PhpFunctionComp(813) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc359: TSynWebTokenKind;
begin
  if  PhpFunctionComp(184) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc360: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4549) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc361: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4191) or
      PhpFunctionComp(4950) or
      PhpFunctionComp(5006) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc364: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2463) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc366: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5045) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc369: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5032) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc370: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1571) or
      PhpFunctionComp(4997) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc373: TSynWebTokenKind;
begin
  if  PhpFunctionComp(287) or
      PhpFunctionComp(821) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc374: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2298) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc375: TSynWebTokenKind;
begin
  if  PhpFunctionComp(839) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc376: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2515) or
      PhpFunctionComp(4992) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc377: TSynWebTokenKind;
begin
  if  PhpFunctionComp(843) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc385: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5007) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc387: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5092) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc393: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4167) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc399: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5097) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc402: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2494) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc407: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1006) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc426: TSynWebTokenKind;
begin
  if  PhpFunctionComp(900) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc433: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5096) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc439: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5098) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc442: TSynWebTokenKind;
begin
  if  PhpFunctionComp(921) or
      PhpFunctionComp(4995) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc443: TSynWebTokenKind;
begin
  if  PhpFunctionComp(844) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc445: TSynWebTokenKind;
begin
  if  PhpFunctionComp(999) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc450: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2513) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc465: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5099) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc466: TSynWebTokenKind;
begin
  if  PhpFunctionComp(817) or
      PhpFunctionComp(5100) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc487: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5094) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc491: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5093) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc496: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5011) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc520: TSynWebTokenKind;
begin
  if  PhpFunctionComp(5095) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc541: TSynWebTokenKind;
begin
  if  PhpFunctionComp(848) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc560: TSynWebTokenKind;
begin
  if  PhpFunctionComp(849) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

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

