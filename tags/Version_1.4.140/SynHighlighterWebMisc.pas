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

{$IFNDEF QSYNHIGHLIGHTERWEBMISC}
unit SynHighlighterWebMisc;
{$ENDIF}
  
{$I SynWeb.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEdit,
  QSynEditTextBuffer,
  QSynEditHighlighter,
  QSynEditTypes,
  QSynCompletionProposal,
  QSynHighlighterWeb,
  QSynHighlighterWebData,
  QSynTokenMatch;
{$ELSE}
  SysUtils,
  StrUtils,
  Graphics,
  SynEdit,
  SynEditTextBuffer,
  SynEditHighlighter,
  SynEditTypes,
  SynCompletionProposal,
  SynHighlighterWeb,
  SynHighlighterWebData,
  SynTokenMatch;
{$ENDIF}

{
  SynEditGetMatchingToken(Ex) returns:
  -2 : Close and open token found
  -1 : Close token found
   0 : Kind not found
  +1 : Open token found
  +2 : Open and close token found
}

function SynEditGetMatchingTag(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;

function SynEditGetMatchingTagEx(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;

function SynWebGetHighlighterTypeAt(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase; APos: TBufferCoord): TSynWebHighlighterTypes;

function SynWebGetHighlighterTypeAtCursor(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;

function SynWebUpdateActiveHighlighter(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;

function SynWebFillCompletionProposal(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebHtmlSyn; ACompletion: TSynCompletionProposal;
  var CurrentInput: WideString): TSynWebHighlighterTypes;
  
implementation

type
  TSynTokenBuf = record
    Pos: TBufferCoord;
{$IFDEF UNISYNEDIT}
    Token: WideString;
{$ELSE}
    Token: String;
{$ENDIF}
  end;

var
  FMatchStack: array of TSynTokenBuf;

function SynEditGetMatchingTag(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
var
  TagID: Integer;
  Level, DeltaLevel, FMatchStackID: Integer;   
  H: TSynWebMLSyn;
  bSpecial: Boolean;

  function ScanToEndOfSpecialTag: Boolean;
  begin
    Result := False;
    with ASynEdit, H do
    begin
      Next;
      while True do
      begin
        while not GetEol do
        begin
          if (GetTokenID = stkMLError) and (GetToken = '/') then
          begin
            Next;
            if GetEol then
              Break;
            if (GetTokenID = stkMLTag) and (GetToken = '>') then
            begin
              Result := True;
              Exit;
            end;
          end else
            if (GetTokenID in [stkMLTag, stkMLError]) and (GetToken = '>') then
            begin
              Next;
              if GetTagID = -1 then
                Exit;
              Continue;
            end;
          Next;
        end;
        Inc(APoint.Line);
        if APoint.Line >= Lines.Count then
          Break;
        SetLine(Lines[APoint.Line], APoint.Line);
      end;
    end;
  end;

  function CheckToken: Boolean;
  begin
    with H do
    begin
      if (GetTokenId = stkMLTagName) and (TagID = GetTagID) then
        Inc(Level, GetTagKind);
      if Level = 0 then
      begin
        SynEditGetMatchingTag := 2;
        AMatch.CloseToken := GetToken;
        AMatch.CloseTokenPos.Line := APoint.Line + 1;
        AMatch.CloseTokenPos.Char := GetTokenPos + 1;
        Result := True;
      end else
      begin
        Next;
        Result := False;
      end;
    end;
  end;

  procedure CheckTokenBack;
  var
    OldLine: Integer;
  begin
    with H do
    begin
      if (GetTokenId = stkMLTagName) and (TagID = GetTagID) then
        case GetTagKind of
        -1:
          begin
            Dec(Level);
            if FMatchStackID >= 0 then
              Dec(FMatchStackID);
          end;
        1:
          begin                   
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Token := GetToken;
            FMatchStack[FMatchStackID].Pos.Line := APoint.Line + 1;
            FMatchStack[FMatchStackID].Pos.Char := GetTokenPos + 1;
            if bSpecial then
            begin
              OldLine := APoint.Line;
              if ScanToEndOfSpecialTag then
              begin
                Dec(FMatchStackID);
                if OldLine <> APoint.Line then
                begin
                  APoint.Line := OldLine;
                  while not GetEol do
                    Next;
                end else
                  Next;
                Exit;
              end;  
              if OldLine <> APoint.Line then
              begin
                APoint.Line := OldLine;
                while not GetEol do
                  Next;
                Exit;
              end;
            end;
            Inc(Level);
          end;
        end;
      Next;
    end;
  end;

begin
  Result := 0;
  if not (ASynEdit.Highlighter is TSynWebMLSyn) then
    Exit;
  H := TSynWebMLSyn(ASynEdit.Highlighter);
  with ASynEdit, H do
  begin       
    if Engine = nil then
      Exit;         
    Dec(APoint.Line);
    Dec(APoint.Char);
    if APoint.Line = 0 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
    SetLine(Lines[APoint.Line], APoint.Line);
    while not GetEol and (APoint.Char >= GetTokenPos + Length(GetToken)) do
      Next;         
    TagID := GetTagID;
    if GetEol or (TagID = -1) or (GetTokenID <> stkMLTagName) or
      (TSynWeb_TagsData[TagID] and (1 shl 31) <> 0) then
      Exit;
    bSpecial := TagID in [MLTagID_Script, MLTagID_Style]; 
    case GetTagKind of
    1:
      begin
        Result := 1;
        AMatch.OpenToken := GetToken;
        AMatch.OpenTokenPos.Line := APoint.Line + 1;
        AMatch.OpenTokenPos.Char := GetTokenPos + 1;
      end;
    -1:
      begin
        Result := -1;
        AMatch.CloseToken := GetToken;
        AMatch.CloseTokenPos.Line := APoint.Line + 1;
        AMatch.CloseTokenPos.Char := GetTokenPos + 1;
      end;
    end;
    AMatch.TokenKind := GetTokenKind;
    AMatch.TokenAttri := GetTokenAttribute;
    if Result = 1 then
    begin          
      if bSpecial and ScanToEndOfSpecialTag then
      begin
        Result := 0;
        Exit;
      end;
      Level := 1;
      Next;
      while True do
      begin
        while not GetEol do
          if CheckToken then
            Exit;
        Inc(APoint.Line);
        if APoint.Line >= ASynEdit.Lines.Count then
          Break;
        SetLine(Lines[APoint.Line], APoint.Line);
      end;
    end else
    begin
      if Length(FMatchStack) < 32 then
        SetLength(FMatchStack, 32);
      FMatchStackID := -1;
      Level := -1;
      if APoint.Line = 0 then
        ResetRange
      else
        SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
      SetLine(Lines[APoint.Line], APoint.Line);
      while not GetEol and (GetTokenPos < AMatch.CloseTokenPos.Char -1) do
        CheckTokenBack;
      if FMatchStackID > -1 then
      begin
        Result := -2;
        AMatch.OpenToken := FMatchStack[FMatchStackID].Token;
        AMatch.OpenTokenPos := FMatchStack[FMatchStackID].Pos;
      end else
        while APoint.Line > 0 do
        begin
          DeltaLevel := -Level - 1;
          Dec(APoint.Line);
          if APoint.Line = 0 then
            ResetRange
          else
            SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
          SetLine(Lines[APoint.Line], APoint.Line);
          FMatchStackID := -1;
          while not GetEol do
            CheckTokenBack;
          if (DeltaLevel <= FMatchStackID) then
          begin
            Result := -2;
            AMatch.OpenToken := FMatchStack[FMatchStackID - DeltaLevel].Token;
            AMatch.OpenTokenPos := FMatchStack[FMatchStackID - DeltaLevel].Pos;
            Exit;
          end;
        end;
    end;
  end;
end;

function SynEditGetMatchingTagEx(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
begin
  Result := SynEditGetMatchingTag(ASynEdit, APoint, AMatch);
  if (Result = 0) and (APoint.Char > 1) then
  begin
    Dec(APoint.Char);
    Result := SynEditGetMatchingTag(ASynEdit, APoint, AMatch);
  end;
end;

function SynWebGetHighlighterTypeAt(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase; APos: TBufferCoord): TSynWebHighlighterTypes;
begin
  with ASynEdit,ASynWeb do
  begin
    if APos.Line = 1 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[CaretY-2]);
    Result := GetActiveHighlighter(GetRange, Lines[CaretY-1], APos.Char, APos.Line);
  end;
end;

function SynWebGetHighlighterTypeAtCursor(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;
begin
  Result := SynWebGetHighlighterTypeAt(ASynEdit, ASynWeb, ASynEdit.CaretXY);
end;

function SynWebUpdateActiveHighlighter(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;
begin
  with ASynEdit,ASynWeb do
  begin
    Result := SynWebGetHighlighterTypeAtCursor(ASynEdit, ASynWeb);
    if Result <> ActiveHighlighters then
      if ActiveHighlighterSwitch then
      begin
        ActiveHighlighters := Result;
        Repaint;
      end;
  end;
end;

function SynWebFillCompletionProposal(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebHtmlSyn; ACompletion: TSynCompletionProposal;
  var CurrentInput: WideString): TSynWebHighlighterTypes;
var
  ct: WideString;
  ctk: TSynWebTokenKind;

  procedure ScanTo(APos: TBufferCoord);
  begin
    Dec(APos.Line);
    Dec(APos.Char);
    with ASynEdit, ASynWeb do
    begin
      if APos.Line = 0 then
        ResetRange
      else
        SetRange(TSynEditStringList(Lines).Ranges[APos.Line - 1]);

      SetLine(Lines[APos.Line], APos.Line + 1);

      while not GetEol and (GetTokenPos + GetTokenLen < APos.Char) do
        Next;
    end;
  end;

  procedure AddSimple(AInsert: String; AKind: String = ''; AKindColor: TColor = clBlack);
  var
    c: String;
  begin
    c := ColorToString(AKindColor);
    if AKind <> '' then
      ACompletion.ItemList.Add(Format('\color{%s}%s\column{}\color{clBlack}\style{+B}%s',[c, AKind, AInsert]))
    else
      ACompletion.ItemList.Add(AInsert);
    ACompletion.InsertList.Add(AInsert);
  end;

  function HtmlGetTagKind(AID: Integer): Integer;
  begin
    if TSynWeb_TagsData[AID] and
      (1 shl 31) = 0 then
      Result := 1
    else
      if ASynWeb.InternalInstanceData.FOptions.FMLVersion >= smlhvXHtml10Strict then
        Result := 0
      else
        Result := -1;
  end;

  procedure HtmlLoadTags(AClose: Boolean);
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);
    for i := 0 to High(TSynWeb_TagsData) do
      if ((TSynWeb_TagsData[i] and ver) <> 0) and (not AClose or
        (AClose and (TSynWeb_TagsData[i] and (1 shl 31) = 0))) and
        (TSynWeb_TagsData[i] and (1 shl 29) = 0) then
      begin
        if AClose then
          AddSimple('</' + TSynWeb_Tags[i] + '>', 'tag',
            ASynWeb.Engine.MLTagNameAttri.Foreground)
        else
          case HtmlGetTagKind(i) of
          -1:
            AddSimple('<' + TSynWeb_Tags[i] + '>', 'tag',
              ASynWeb.Engine.MLTagNameAttri.Foreground);
          0:
            AddSimple('<' + TSynWeb_Tags[i] + ' />', 'tag',
              ASynWeb.Engine.MLTagNameAttri.Foreground);
          1:
            AddSimple('<' + TSynWeb_Tags[i] + '></' + TSynWeb_Tags[i] + '>', 'tag',
               ASynWeb.Engine.MLTagNameAttri.Foreground);
          end;
      end;
  end;

  procedure HtmlLoadSpecailEntity;
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);
    for i := 0 to High(TSynWeb_SpecialData) do
      if (TSynWeb_SpecialData[i] and ver) <> 0 then
        AddSimple('&' + TSynWeb_Special[i] + ';', 'entity', ASynWeb.Engine.MLEscapeAttri.Foreground);
  end;

  procedure HtmlLoad;
  begin
    HtmlLoadTags(False);
    HtmlLoadTags(True);
    HtmlLoadSpecailEntity;
    AddSimple('<?php ; ?>', 'template');
    AddSimple('<?= ; ?>', 'template');
    AddSimple('<script language="php"></script>', 'template');
    AddSimple('<script language="javascript" type="text/javascript"></script>', 'template');
    AddSimple('<style type="text/css"></style>', 'template');
    AddSimple('<!-- -->', 'template');
  end;

  procedure HtmlLoadAttrs(ATag: Integer);
  var
    i: Integer;
    ver, ver2: Longword;
  begin
    if ATag = -1 then
      Exit;

    ver := Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);
    ver2 := 1 shl (ATag mod 32);
    ATag := ATag div 32;

    for i := 0 to High(TSynWeb_AttrsData) do
      if (TSynWeb_AttrsData[i][ver][ATag] and ver2) <> 0 then
        AddSimple(TSynWeb_Attrs[i] + '=""', 'attribute', ASynWeb.Engine.MLTagAttri.Foreground);
  end;

  procedure Html;
  begin
    with ASynEdit, ASynWeb do
    begin
    
      case ctk of
      stkMLTagName, stkMLTagNameUndef:
        begin       
          HtmlLoad;
          case GetTagKind of
          -1:
            CurrentInput := '</' + Copy(ct, 1, CaretX - 1 - GetTokenPos);
          1:
            CurrentInput := '<' + Copy(ct, 1, CaretX - 1 - GetTokenPos);
          end;
        end;
      stkMLTag:
        begin       
          HtmlLoad;
          case GetTagKind of
          -1:
            CurrentInput := '</';
          1:
            CurrentInput := '<';
          end;
        end;
      stkMLTagKey, stkMLTagKeyUndef:
        begin
          HtmlLoadAttrs(GetTagID);
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end;
      else // case
        case GetMLRange of
        srsMLText:
          HtmlLoad;
        srsMLTagKey, srsMLTagKeyEq:
          HtmlLoadAttrs(GetTagID);
        end; 
      end;   
    end;    
  end;

  procedure CssLoadTags;
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);

    for i := 0 to High(TSynWeb_TagsData) do
      if ((TSynWeb_TagsData[i] and ver) <> 0) and
        (TSynWeb_TagsData[i] and (1 shl 30) = 0) and
        (TSynWeb_TagsData[i] and (1 shl 29) = 0) then
        AddSimple(TSynWeb_Tags[i], 'tag', ASynWeb.Engine.MLTagNameAttri.Foreground);
  end;

  procedure CssLoadProp;
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FCssVersion);

    for i := 0 to High(TSynWeb_CssPropsData) do
      if (TSynWeb_CssPropsData[i] and ver) <> 0  then
        AddSimple(TSynWeb_CssProps[i] + ':', 'property', ASynWeb.Engine.CssPropAttri.Foreground);
  end;

  procedure CssLoadVal(AProp: Integer);
  var
    i: Integer;
    ver, prop: Longword;
  begin
    if AProp = -1 then
      Exit;

    ver := Longword(ASynWeb.InternalInstanceData.FOptions.FCssVersion);
    prop := 1 shl (AProp mod 32);
    AProp := AProp div 32;

    for i := 0 to High(TSynWeb_CssValsData) do
      if (TSynWeb_CssValsData[i][ver][AProp] and prop) <> 0 then
        AddSimple(TSynWeb_CssVals[i], 'value', ASynWeb.Engine.CssValAttri.Foreground);
  end;

  procedure Css;
  begin
    with ASynEdit, ASynWeb do
      case ctk of
      stkCssSelector, stkCssSelectorUndef:
        begin
          CssLoadTags;
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end;
      stkCssProp, stkCssPropUndef:
        begin
          CssLoadProp;
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end;
      stkCssVal, stkCssValUndef:
        begin
          CssLoadVal(CssGetPropertyId);
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end
      else // case
        case CssGetRange of
        srsCssRuleset:
          begin
            CssLoadTags;
            CurrentInput := '';
          end;
        srsCssProp:
          begin
            CssLoadProp;
            CurrentInput := '';
          end;
        srsCssPropVal:
          begin
            CssLoadVal(CssGetPropertyId);
            CurrentInput := '';
          end;
        end;
      end;
  end;

  procedure Php;
  var
    i: Integer;
    v: Longword;
    data: Longword;
  begin
    v := 1 shl Longword(ASynWeb.Options.PhpVersion);
    for i := 0 to High(TSynWeb_PhpKeywords) do // functions
    begin
      data := TSynWeb_PhpKeywordsData[i];
      if (Data and $0F = $08) and ((Data shr 16) and v <> 0) then
        AddSimple(TSynWeb_PhpKeywords[i] + '()', 'function', ASynWeb.Engine.PhpFunctionAttri.Foreground);
    end;

    for i := 0 to High(TSynWeb_PhpKeywords) do // keywords
    begin
      data := TSynWeb_PhpKeywordsData[i];
      if (Data and $0F = $01) and ((Data shr 16) and v <> 0) then
        AddSimple(TSynWeb_PhpKeywords[i], 'keyword', ASynWeb.Engine.PhpKeyAttri.Foreground);
    end;
    CurrentInput := ct;
  end;

begin
  Result := SynWebGetHighlighterTypeAtCursor(ASynEdit, ASynWeb);
  
  ACompletion.InsertList.Clear;
  ACompletion.ItemList.Clear;

  if ASynWeb.Engine = nil then
    Exit;

  with ASynEdit, ASynWeb do
  begin
    ScanTo(CaretXY);

    if (GetTokenPos = 0) and (CaretX = 1) then
    begin
      ct := '';
      ctk := stkNull;
    end else
    begin
      ct := GetToken;
      ctk := GetTokenID;
    end;
  end;

  ct := Trim(ct);

  if shtML in Result then
    Html
  else
    if shtCss in Result then
      Css
    else
      Php;
end;

end.

