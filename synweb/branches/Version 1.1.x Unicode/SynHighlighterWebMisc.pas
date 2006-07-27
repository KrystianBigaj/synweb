{$IFNDEF QSYNHIGHLIGHTERWEBMISC}
unit SynHighlighterWebMisc;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEdit,
  QSynEditTextBuffer,
  QSynEditHighlighter,  
  QSynEditTypes,
  QSynHighlighterWeb,
  QSynHighlighterWebData,
  QSynTokenMatch;
{$ELSE}
  SynEdit,
  SynEditTextBuffer,
  SynEditHighlighter,
  SynEditTypes,
  SynHighlighterWeb,
  SynHighlighterWebData,
  SynTokenMatch;
{$ENDIF}
  
type
{$IFDEF SYN_CLX}
  TSynWebEngineEx = class(QSynHighlighterWeb.TSynWebEngine);
{$ELSE}
  TSynWebEngineEx = class(SynHighlighterWeb.TSynWebEngine);
{$ENDIF}

{
  SynEditGetMatchingToken(Ex) returns:
  -2 : Close and open token found
  -1 : Close token found
   0 : Kind not found
  +1 : Open token found
  +2 : Open and close token found
}

function SynEditGetMatchingTag(ASynEdit: TSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;

function SynEditGetMatchingTagEx(ASynEdit: TSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;

function SynWebUpdateActiveHighlighter(ASynEdit: TSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;
  
implementation

uses
  SysUtils;

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
  FMatchOpenDup, FMatchCloseDup: array of Integer;

function SynEditGetMatchingTag(ASynEdit: TSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
var
{$IFDEF UNISYNEDIT}
  Token: WideString;
{$ELSE}
  Token: String;
{$ENDIF}
  TagID: Integer;
  Level, DeltaLevel, I, J, FMatchStackID: Integer;
  H: TSynWebHtmlSyn;

  function ScanToBeginOfTag: Boolean;
  begin
    with ASynEdit, H do
    begin
      Next;
      while True do
      begin
        while not GetEol do
        begin
          if (GetTokenID = stkHtmlTag) and (GetToken = '<') or (GetToken = '</') then
          begin
            Result := True;
            Exit;
          end;
          Next;
        end;
        Inc(APoint.Line);
        if APoint.Line >= Lines.Count then
          Break;
        SetLine(Lines[APoint.Line], APoint.Line);
      end;
    end;
    Result := False;
  end;

{$IFDEF UNISYNEDIT}         
  function ScanToEndOfTag: WideString;
{$ELSE}
  function ScanToEndOfTag: String;
{$ENDIF}
  begin
    with ASynEdit, H do
    begin
      Next;
      while True do
      begin
        while not GetEol do
        begin
          if (GetTokenID in [stkHtmlTag, stkHtmlError]) then
          begin
            Result := GetToken;
            if (Result = '>') or (Result = '/>') then
              Exit;
          end;
          Next;
        end;
        Inc(APoint.Line);
        if APoint.Line >= Lines.Count then
          Break;
        SetLine(Lines[APoint.Line], APoint.Line);
      end;
    end;
    Result := '';
  end;

  procedure CheckTokenBack;
  begin
    // TODO: Back tag matching
  end;

begin
  Result := 0;
  if not (ASynEdit.Highlighter is TSynWebHtmlSyn) then
    Exit;
  H := TSynWebHtmlSyn(ASynEdit.Highlighter);
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
    if GetEol or (TagID = -1) or (GetTokenID <> stkHtmlTagName) or
      (TSynWeb_TagsData[TagID] and (1 shl 31) <> 0) then
      Exit;
    if GetIsOpenTag then
    begin
      Result := 1;
      AMatch.OpenToken := GetToken;
      AMatch.OpenTokenPos.Line := APoint.Line + 1;
      AMatch.OpenTokenPos.Char := GetTokenPos + 1;
    end else
    begin    
      Result := -1;
      AMatch.CloseToken := GetToken;
      AMatch.OpenTokenPos.Line := APoint.Line + 1;
      AMatch.OpenTokenPos.Char := GetTokenPos + 1;
    end;
    AMatch.TokenKind := GetTokenKind;
    AMatch.TokenAttri := GetTokenAttribute;
    Token := ScanToEndOfTag;
    if Token = '' then
    begin
      Result := 0;
      Exit;
    end;    
    if Result = 1 then
    begin
      Level := 1;
      while True do
      begin
        if not ScanToBeginOfTag then
          Exit;
        Next;
        if GetTagID = TagID then
        begin
          if GetIsOpenTag then
          begin
            Token := ScanToEndOfTag;
            if Token = '>' then
              Inc(Level)
            else
              if Token = '' then
                Exit;
          end else
          begin
            AMatch.CloseToken := GetToken;
            AMatch.CloseTokenPos.Line := APoint.Line + 1;
            AMatch.CloseTokenPos.Char := GetTokenPos + 1;
            if ScanToEndOfTag = '' then
              Exit
            else
              Dec(Level);
          end;
        end else
          if ScanToEndOfTag = '' then
            Exit;
        if Level = 0 then
        begin
          Result := 2;
          Exit;
        end;
      end;
    end else
    begin
      // TODO: Back tag matching
    end;
  end;
end;

function SynEditGetMatchingTagEx(ASynEdit: TSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
begin
  Result := SynEditGetMatchingTag(ASynEdit, APoint, AMatch);
  if (Result = 0) and (APoint.Char > 1) then
  begin
    Dec(APoint.Char);
    Result := SynEditGetMatchingTag(ASynEdit, APoint, AMatch);
  end;
end;

function SynWebUpdateActiveHighlighter(ASynEdit: TSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;
begin
  with ASynEdit,ASynWeb do
  begin
    if UpdateActiveHighlighter(TSynEditStringList(Lines).Ranges[CaretY-2],
       Lines[CaretY-1], CaretX, CaretY) then
      Repaint;
    Result:=ASynWeb.ActiveHighlighters;
  end;
end;

end.

