unit SynHighlighterWebMisc;

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
  SynEditHighlighter,
  SynHighlighterWeb,
  SynHighlighterWebData;

{$ENDIF}

function SynWeb_UpdateActiveHighlighter(ASynEdit: TSynEdit;
  ASynWeb: TSynWebBase): TSynHighlighterTypes;

function SynWeb_FindMatchingToken(ASynEdit: TSynEdit; ASynWeb: TSynWebBase;
  const AOpenTokens, ACloseTokens: array of string;
  const ATokenIDs: array of TtkTokenKind; AStartPoint: TBufferCoord;
  var AMatchtPoint: TBufferCoord; var ATokenIndex: integer): integer;

{
  SynWeb_FindMatchingToken returns:
  -2 : Close and open token found (open token position in AMatchtPoint)
  -1 : Close token found
   0 : Token not found
  +1 : Open token found
  +2 : Open and close token found (close token position in AMatchtPoint)
}

implementation

uses Math;

var
  fMatchStack: array of TBufferCoord;
  fMatchStackID: integer;

 // -----------------------------------------------------------------------------
 // -----------------------------------------------------------------------------
 // -----------------------------------------------------------------------------

function SynWeb_FindMatchingToken(ASynEdit: TSynEdit; ASynWeb: TSynWebBase;
  const AOpenTokens, ACloseTokens: array of string;
  const ATokenIDs: array of TtkTokenKind; AStartPoint: TBufferCoord;
  var AMatchtPoint: TBufferCoord; var ATokenIndex: integer): integer;
var
  OpenToken, CloseToken, Token: string;
  TokenID: TtkTokenKind;
  Level: integer;
  I: integer;

  function CheckToken: boolean;
  begin
    with ASynWeb do
      if GetTokenID = TokenID then
      begin
        Token := GetToken;
        if Token = CloseToken then
          Dec(Level)
        else
          if Token = OpenToken then
            Inc(Level);
      end;
    if Level = 0 then
    begin
      SynWeb_FindMatchingToken := 2;
      AMatchtPoint.Line := AStartPoint.Line + 1;
      AMatchtPoint.char := ASynWeb.GetTokenPos + 1;
      Result := True;
    end else
      Result := False;
  end;

  procedure CheckTokenBack;
  begin
    with ASynWeb do
      if GetTokenID = TokenID then
      begin
        Token := GetToken;
        if Token = CloseToken then
        begin
          Dec(Level);
          if fMatchStackID >= 0 then
            Dec(fMatchStackID);
        end else
          if Token = OpenToken then
          begin
            Inc(Level);
            Inc(fMatchStackID);
            if fMatchStackID >= Length(fMatchStack) then
              SetLength(fMatchStack, Length(fMatchStack) + 32);
            fMatchStack[fMatchStackID].Line := AStartPoint.Line + 1;
            fMatchStack[fMatchStackID].char := ASynWeb.GetTokenPos + 1;
          end;
      end;
    ASynWeb.Next;
  end;

begin
  Result := 0;
  Dec(AStartPoint.char);
  Dec(AStartPoint.Line);
  with ASynWeb, ASynEdit do
  begin
    SetRange(TSynEditStringList(Lines).Ranges[AStartPoint.Line - 1]);
    SetLine(Lines[AStartPoint.Line], +1);
  end;
  with ASynWeb do
  begin
    Level := -1;
    while not GetEol and (GetTokenPos < AStartPoint.char) do
      Next;
    if GetTokenPos <> AStartPoint.char then
      Exit;
    TokenID := GetTokenID;
    for I := 0 to Length(ATokenIDs) - 1 do
      if TokenID = ATokenIDs[I] then
      begin
        Token := ASynWeb.GetToken;
        if Token = AOpenTokens[I] then
        begin
          Result := 1;
          Break;
        end else
          if Token = ACloseTokens[I] then
          begin
            Result := -1;
            Break;
          end;
      end;
    if Result = 0 then
      Exit;
    ATokenIndex := I;
    TokenID := ATokenIDs[I];
    OpenToken := AOpenTokens[I];
    CloseToken := ACloseTokens[I];

    if Result = 1 then
    begin
      Level := 1;
      while not GetEol do
      begin
        Next;
        if CheckToken then
          Exit;
      end;
      Inc(AStartPoint.Line);
      while AStartPoint.Line < ASynEdit.Lines.Count do
      begin
        with ASynWeb, ASynEdit do
        begin
          SetRange(TSynEditStringList(Lines).Ranges[AStartPoint.Line - 1]);
          SetLine(Lines[AStartPoint.Line], +1);
        end;
        while not GetEol do
        begin
          if CheckToken then
            Exit;
          Next;
        end;
        Inc(AStartPoint.Line);
      end;
    end else
    begin
      if Length(fMatchStack) < 32 then
        SetLength(fMatchStack, 32);
      fMatchStackID := -1;
      Level := -1;
      with ASynWeb, ASynEdit do
      begin
        SetRange(TSynEditStringList(Lines).Ranges[AStartPoint.Line - 1]);
        SetLine(Lines[AStartPoint.Line], +1);
      end;
      while not GetEol and (GetTokenPos < AStartPoint.char) do
        CheckTokenBack;

      if fMatchStackID > -1 then
      begin
        Result := -2;
        AMatchtPoint := fMatchStack[fMatchStackID];
      end else
        while AStartPoint.Line > 0 do
        begin
          Dec(AStartPoint.Line);
          with ASynWeb, ASynEdit do
          begin
            SetRange(TSynEditStringList(Lines).Ranges[AStartPoint.Line - 1]);
            SetLine(Lines[AStartPoint.Line], +1);
          end;
          fMatchStackID := -1;
          while not GetEol do
            CheckTokenBack;
          if (Level >= 0) and (Level <= fMatchStackID) then
          begin
            Result := -2;
            AMatchtPoint := fMatchStack[Level];
            Break;
          end;
        end;
    end;
  end;
end;

 // -----------------------------------------------------------------------------
 // -----------------------------------------------------------------------------
 // -----------------------------------------------------------------------------

function SynWeb_UpdateActiveHighlighter(ASynEdit: TSynEdit;
  ASynWeb: TSynWebBase): TSynHighlighterTypes;
begin           //todo: reorganize
 { with ASynEdit,ASynWeb do
  begin
    if UpdateActiveHighlighter(TSynEditStringList(Lines).Ranges[CaretY-2],
       Lines[CaretY-1], CaretX, CaretY) then
      Repaint;
    Result:=ASynWeb.GetCurrentActiveHighlighters;
  end;  }
end;

initialization
  // None
finalization
  SetLength(fMatchStack, 0);
end.

