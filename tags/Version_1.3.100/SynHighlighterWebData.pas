{-------------------------------------------------------------------------------
SynWeb
Copyright (C) 2006  Krystian Bigaj

*** MPL
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is Krystian Bigaj.

Alternatively, the contents of this file may be used under the terms
of the GNU Lesser General Public license (the  "LGPL License"),
in which case the provisions of LGPL License are applicable instead of those
above. If you wish to allow use of your version of this file only
under the terms of the LGPL License and not to allow others to use
your version of this file under the MPL, indicate your decision by
deleting the provisions above and replace them with the notice and
other provisions required by the LGPL License. If you do not delete
the provisions above, a recipient may use your version of this file
under either the MPL or the LGPL License.

*** LGPL
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
***

You may retrieve the latest version of this file at the SynWeb home page,
located at http://sourceforge.net/projects/synweb

Contact: krystian.bigaj@gmail.com
Homepage: http://flatdev.ovh.org      
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERWEBDATA}
unit SynHighlighterWebData;
{$ENDIF}

{$I SynWeb.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditHighlighter;
{$ELSE}
  SynEditHighlighter;

{$ENDIF}

// Global ----------------------------------------------------------------------
type
  TSynWebHashTable = array[#0..#255] of Longword;

  TSynWebHighlighterType = (
    shtML, shtCss, shtEs, shtPhpInML, shtPhpInCss, shtPhpInEs
    );

  TSynWebHighlighterTypes = set of TSynWebHighlighterType;

  TSynWebHighlighterMode = (
    shmML, shmCss, shmEs, shmPhpCli
    );

  TSynWebTokenKind = (
    // ML
    stkMLSpace, stkMLText, stkMLEscape, stkMLComment, stkMLSymbol,
    stkMLTag, stkMLTagName, stkMLTagNameUndef, stkMLTagKey,
    stkMLTagKeyUndef, stkMLTagKeyValue, stkMLTagKeyValueQuoted, stkMLError,
    // Css
    stkCssSpace, stkCssSelector, stkCssSelectorUndef, stkCssSelectorClass,
    stkCssSelectorId, stkCssSpecial, stkCssComment, stkCssProp, stkCssPropUndef,
    stkCssVal, stkCssValUndef, stkCssValString, stkCssValNumber, stkCssSymbol,
    stkCssError,
    // ECMAScript
    stkEsSpace, stkEsIdentifier, stkEsKeyword, stkEsString, stkEsComment, stkEsSymbol,
    stkEsNumber, stkEsError,
    // Php
    stkPhpSpace, stkPhpInlineText, stkPhpIdentifier, stkPhpKeyword, stkPhpFunction,
    stkPhpVariable, stkPhpConst, stkPhpString, stkPhpStringSpecial, stkPhpComment,
    stkPhpDocComment, stkPhpSymbol, stkPhpNumber, stkPhpError,
    // Other
    stkNull);

  TSynWebProcTableProc = procedure of object;

  PSynWebIdentFuncTableFunc = ^TSynWebIdentFuncTableFunc;
  TSynWebIdentFuncTableFunc = function: TSynWebTokenKind of object;

  PSynWebIdent2FuncTableFunc = ^TSynWebIdent2FuncTableFunc;
  TSynWebIdent2FuncTableFunc = function: Boolean of object;

  PSynWebTokenAttributeTable = ^TSynWebTokenAttributeTable;
  TSynWebTokenAttributeTable = array[Low(TSynWebTokenKind)..High(TSynWebTokenKind)] of
    TSynHighlighterAttributes;

// ML --------------------------------------------------------------------------
type
  TSynWebHtmlVersion = (shvHtml401Strict, shvHtml401Transitional, shvHtml401Frameset,
    shvXHtml10Strict, shvXHtml10Transitional, shvXHtml10Frameset);

  TSynWebWmlVersion = (swvWml11, swvWml12, swvWml13);

  TSynWebMLVersion = (smlhvHtml401Strict, smlhvHtml401Transitional, smlhvHtml401Frameset,
    smlhvXHtml10Strict, smlhvXHtml10Transitional, smlhvXHtml10Frameset,
    smlwvWml11, smlwvWml12, smlwvWml13, smlwvWml20);

  TSynWebMLRangeState = (srsMLText, srsMLComment, srsMLCommentClose, srsMLTag,
    srsMLTagClose, srsMLTagDOCTYPE, srsMLTagCDATA, srsMLTagKey,
    srsMLTagKeyEq, srsMLTagKeyValue, srsMLTagKeyValueQuoted1,
    srsMLTagKeyValueQuoted2);

const
  TSynWebHtmlVersionStr: array[Low(TSynWebHtmlVersion)..High(TSynWebHtmlVersion)] of String = (
    'Html 4.01 Strict',
    'Html 4.01 Transitional',
    'Html 4.01 Frameset',
    'XHtml 1.0 Strict',
    'XHtml 1.0 Transitional',
    'XHtml 1.0 Frameset'
    );

  TSynWebWMLVersionStr: array[Low(TSynWebWMLVersion)..High(TSynWebWMLVersion)] of String = (
    'Wml 1.1',
    'Wml 1.2',
    'Wml 1.3'
    );

// Css -------------------------------------------------------------------------
type
  TSynWebCssVersion = (
    scvCss1, scvCss21
    );

  TSynWebCssRangeState = (srsCssRuleset, srsCssSelectorAttrib, srsCssSelectorPseudo,
    srsCssAtKeyword, srsCssProp, srsCssPropVal, srsCssPropValStr, srsCssPropValRgb,
    srsCssPropValFunc, srsCssPropValSpecial, srsCssPropValImportant,
    srsCssPropValUrl, srsCssPropValRect,
    srsCssComment);

const
  TSynWebCssRangeStateRulesetBegin = srsCssProp;
  TSynWebCssRangeStateRulesetEnd = srsCssPropValRect;

  TSynWebCssVersionStr: array[Low(TSynWebCssVersion)..High(TSynWebCssVersion)] of String = (
    'Css 1',
    'Css 2.1'
    );

  TSynWebCssString39 = 4;
  TSynWebCssString34 = 5;

// ECMAScript ------------------------------------------------------------------
type
  TSynWebEsRangeState = (srsEsDefault, srsEsComment, srsEsCommentMulti, srsEsString34,
    srsEsString39);

// Php -------------------------------------------------------------------------
type
  TSynWebPhpVersion = (
    spvPhp4, spvPhp5
    );

  TSynWebPhpRangeState = (
    srsPhpSubProc, srsPhpDefault, srsPhpComment, srsPhpString34, srsPhpString39,
    srsPhpStringShell, srsPhpHeredoc
    );

  TSynWebPhpOpenTag = (spotPhp, spotPhpShort, spotML, spotASP);
  TSynWebPhpOpenTags = set of TSynWebPhpOpenTag;

const
  TSynWebPhpVersionStr: array[Low(TSynWebPhpVersion)..High(TSynWebPhpVersion)] of String = (
{$I SynHighlighterWeb_PhpVersion.inc}
    );

// ML --------------------------------------------------------------------------
const
  {$I SynHighlighterWeb_Tags.inc}

  {$I SynHighlighterWeb_Attrs.inc}

  {$I SynHighlighterWeb_Special.inc}
// Css -------------------------------------------------------------------------
const
  {$I SynHighlighterWeb_CssProps.inc}

  {$I SynHighlighterWeb_CssVals.inc}

  {$I SynHighlighterWeb_CssSpecial.inc}

// ECAMScript ------------------------------------------------------------------
const
  {$I SynHighlighterWeb_EsKeywords.inc}

// Php -------------------------------------------------------------------------
const
{$I SynHighlighterWeb_PhpKeywords.inc}

// Global ----------------------------------------------------------------------
const
{$I SynHighlighterWeb_Tables.inc}

  TCrc8Table: array[$00..$FF] of byte = (
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
