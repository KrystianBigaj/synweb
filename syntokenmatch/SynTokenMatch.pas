{$IFNDEF QSYNHIGHLIGHTERWEBMISC}
unit SynTokenMatch;
{$ENDIF}

{$I SynEdit.Inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEdit,
  QSynEditTextBuffer,
  QSynEditTypes,
  QSynEditHighlighter;
{$ELSE}
  SynEdit,
  SynEditTextBuffer,
  SynEditTypes,
  SynEditHighlighter;
{$ENDIF}

type
  PSynTokenMatch = ^TSynTokenMatch;
  TSynTokenMatch = record
{$IFDEF UNISYNEDIT}
    OpenToken: WideString;
    CloseToken: WideString;
{$ELSE}
    OpenToken: String;
    CloseToken: String;
{$ENDIF}
    TokenKind: Integer;
  end;

  TSynTokenMatches = record
{$IFDEF UNISYNEDIT}
    OpenToken: WideString;
    CloseToken: WideString;
{$ELSE}
    OpenToken: String;
    CloseToken: String;
{$ENDIF}
    OpenTokenPos: TBufferCoord;
    CloseTokenPos: TBufferCoord;
    TokenKind: Integer;
    TokenAttri: TSynHighlighterAttributes;
  end;

function SynEditGetMatchingToken(ASynEdit: TSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatches): Integer;

{
  SynEditGetMatchingToken returns:
  -2 : Close and open Kind found
  -1 : Close Kind found
   0 : Kind not found
  +1 : Open Kind found
  +2 : Open and close Kind found
}

function SynEditGetMatchingTokenEx(ASynEdit: TSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatches): Integer;

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

function SynEditGetMatchingToken(ASynEdit: TSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatches): Integer;
var
  TokenMatch: PSynTokenMatch;
{$IFDEF UNISYNEDIT}
  Token: WideString;
{$ELSE}
  Token: String;
{$ENDIF}
  TokenKind: Integer;
  Level, DeltaLevel, I, J, FMatchStackID, OpenDupLen, CloseDupLen: Integer;

  function IsOpenToken: Boolean;
  var
    X: Integer;
  begin
    for X := 0 to OpenDupLen - 1 do
      if Token = ATokens[FMatchOpenDup[X]].OpenToken then
      begin
        Result := True;
        Exit;
      end;
    Result := False
  end;

  function IsCloseToken: Boolean; 
  var
    X: Integer;
  begin
    for X := 0 to CloseDupLen - 1 do
      if Token = ATokens[FMatchCloseDup[X]].CloseToken then
      begin
        Result := True;
        Exit;
      end;
    Result := False
  end;

  function CheckToken: Boolean;
  begin
    with ASynEdit.Highlighter do
    begin
      if GetTokenKind = TokenMatch^.TokenKind then
      begin
        Token := LowerCase(GetToken);
        if IsCloseToken then
          Dec(Level)
        else
          if IsOpenToken then
            Inc(Level);
      end;
      if Level = 0 then
      begin
        SynEditGetMatchingToken := 2;
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
  begin
    with ASynEdit.Highlighter do
    begin
      if GetTokenKind = TokenMatch^.TokenKind then
      begin
        Token := LowerCase(GetToken);
        if IsCloseToken then
        begin
          Dec(Level);
          if FMatchStackID >= 0 then
            Dec(FMatchStackID);
        end else
          if IsOpenToken then
          begin
            Inc(Level);
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Token := GetToken;
            FMatchStack[FMatchStackID].Pos.Line := APoint.Line + 1;
            FMatchStack[FMatchStackID].Pos.Char := GetTokenPos + 1;
          end;
      end;
      Next;
    end;
  end;

begin
  Result := 0;
  if ASynEdit.Highlighter = nil then
    Exit;
  Dec(APoint.Line);
  Dec(APoint.Char);
  with ASynEdit, ASynEdit.Highlighter do
  begin
    if APoint.Line = 0 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
    SetLine(Lines[APoint.Line], APoint.Line);
    while not GetEol and (APoint.Char >= GetTokenPos + Length(GetToken)) do
      Next;
    if GetEol then
      Exit;
    TokenKind := GetTokenKind;
    I := 0;
    J := Length(ATokens);
    while I < J do
    begin
      if TokenKind = ATokens[I].TokenKind then
      begin
        Token := LowerCase(GetToken);
        if Token = ATokens[I].OpenToken then
        begin
          Result := 1;
          AMatch.OpenToken := GetToken;
          AMatch.OpenTokenPos.Line := APoint.Line + 1;
          AMatch.OpenTokenPos.Char := GetTokenPos + 1;
          Break;
        end else
          if Token = ATokens[I].CloseToken then
          begin
            Result := -1;
            AMatch.CloseToken := GetToken;
            AMatch.CloseTokenPos.Line := APoint.Line + 1;
            AMatch.CloseTokenPos.Char := GetTokenPos + 1;
            Break;
          end;
      end;
      Inc(I);
    end;
    if Result = 0 then
      Exit;
    TokenMatch := @ATokens[I];
    AMatch.TokenKind := TokenKind;
    AMatch.TokenAttri := GetTokenAttribute;
    if J > Length(FMatchOpenDup) then
    begin
      SetLength(FMatchOpenDup, J);
      SetLength(FMatchCloseDup, J);
    end;
    OpenDupLen := 0;
    CloseDupLen := 0;
    for I:=0 to J -1 do
      if (TokenKind = ATokens[I].TokenKind) then
      begin
        if (TokenMatch^.OpenToken = ATokens[I].OpenToken) then
        begin
          FMatchCloseDup[CloseDupLen] := I;
          Inc(CloseDupLen);
        end;
        if (TokenMatch^.CloseToken = ATokens[I].CloseToken) then
        begin
          FMatchOpenDup[OpenDupLen] := I;
          Inc(OpenDupLen);
        end;
      end;

    if Result = 1 then
    begin
      Level := 1;
      Next;
      while not GetEol do
        if CheckToken then
          Exit;
      Inc(APoint.Line);
      while APoint.Line < ASynEdit.Lines.Count do
      begin
        SetLine(Lines[APoint.Line], APoint.Line);
        while not GetEol do
          if CheckToken then
            Exit;
        Inc(APoint.Line);
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

function SynEditGetMatchingTokenEx(ASynEdit: TSynEdit; APoint: TBufferCoord;
  const ATokens: array of TSynTokenMatch; var AMatch: TSynTokenMatches): Integer;
begin
  Result := SynEditGetMatchingToken(ASynEdit, APoint, ATokens, AMatch);
  if (Result = 0) and (APoint.Char > 1) then
  begin
    Dec(APoint.Char);
    Result := SynEditGetMatchingToken(ASynEdit, APoint, ATokens, AMatch);
  end;
end;

initialization
  // None
finalization
  SetLength(FMatchStack, 0);
  SetLength(FMatchOpenDup, 0);
  SetLength(FMatchCloseDup, 0);
end.

