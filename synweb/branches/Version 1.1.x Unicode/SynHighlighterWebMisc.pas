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

function SynEditGetMatchingTag(ASynEdit: TSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
var
  TagID: Integer;
  Level, DeltaLevel, FMatchStackID: Integer;   
  H: TSynWebHtmlSyn;
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
          if (GetTokenID = stkHtmlError) and (GetToken = '/') then
          begin
            Next;
            if GetEol then
              Break;
            if (GetTokenID = stkHtmlTag) and (GetToken = '>') then
            begin
              Result := True;
              Exit;
            end;
          end else
            if (GetTokenID in [stkHtmlTag, stkHtmlError]) and (GetToken = '>')
              and (GetTagID = -1) then
              Exit;
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
      if (GetTokenId = stkHtmlTagName) and (TagID = GetTagID) then
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
      if (GetTokenId = stkHtmlTagName) and (TagID = GetTagID) then
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
    bSpecial := TagID in [HtmlTagID_Script, HtmlTagID_Style]; 
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
    Result := ASynWeb.ActiveHighlighters;
  end;
end;

end.

