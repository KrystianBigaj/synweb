{$IFNDEF QSYNHIGHLIGHTERWEBMISC}
unit SynHighlighterWebMisc;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEdit,
  QSynEditTextBuffer,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterWeb,
  QSynHighlighterWebData;
{$ELSE}
  SynEdit,
  SynEditTextBuffer,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterWeb,
  SynHighlighterWebData;
{$ENDIF}

function SynWebUpdateActiveHighlighter(ASynEdit: TSynEdit;
  ASynWeb: TSynWebBase): TSynHighlighterTypes;

function SynWebFindMatchingToken(ASynEdit: TSynEdit; ASynWeb: TSynWebBase;
  const AOpenTokens, ACloseTokens: array of String;
  const ATokenIDs: array of TSynWebTokenKind; var AStartPoint: TBufferCoord;
  var AMatchtPoint: TBufferCoord; var ATokenIndex: Integer): Integer;

{
  SynWebFindMatchingToken returns:
  -2 : Close and open token found (open token position in AMatchtPoint)
  -1 : Close token found
   0 : Token not found
  +1 : Open token found
  +2 : Open and close token found (close token position in AMatchtPoint)
}

function SynWebFindMatchingTokenEx(ASynEdit: TSynEdit; ASynWeb: TSynWebBase;
  const AOpenTokens, ACloseTokens: array of String;
  const ATokenIDs: array of TSynWebTokenKind; var AStartPoint: TBufferCoord;
  var AMatchtPoint: TBufferCoord; var ATokenIndex: Integer): Integer;


implementation

uses Math;

var
  FMatchStack: array of TBufferCoord;
  FMatchStackID: Integer;

function SynWebFindMatchingToken(ASynEdit: TSynEdit; ASynWeb: TSynWebBase;
  const AOpenTokens, ACloseTokens: array of String;
  const ATokenIDs: array of TSynWebTokenKind; var AStartPoint: TBufferCoord;
  var AMatchtPoint: TBufferCoord; var ATokenIndex: Integer): Integer;
var
  OpenToken, CloseToken, Token: String;
  TokenID: TSynWebTokenKind;
  Level: Integer;
  ScanLine, I: Integer;

  function CheckToken: Boolean;
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
      SynWebFindMatchingToken := 2;
      AMatchtPoint.Line := ScanLine + 1;
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
          if FMatchStackID >= 0 then
            Dec(FMatchStackID);
        end else
          if Token = OpenToken then
          begin
            Inc(Level);
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Line := ScanLine + 1;
            FMatchStack[FMatchStackID].char := ASynWeb.GetTokenPos + 1;
          end;
      end;
    ASynWeb.Next;
  end;

begin
  Result := 0;
  ScanLine := AStartPoint.Line - 1;
  with ASynWeb, ASynEdit do
  begin
    SetRange(TSynEditStringList(Lines).Ranges[ScanLine - 1]);
    SetLine(Lines[ScanLine], +1);
  end;
  with ASynWeb do
  begin
    Level := -1;
    Dec(AStartPoint.Char);
    while not GetEol and (GetTokenPos < AStartPoint.Char) and
      not (AStartPoint.Char < GetTokenPos + GetTokenLen) do
      Next;
    Inc(AStartPoint.Char);
    if GetEol then
      Exit;
    TokenID := GetTokenID;
    I := 0;
    while I < Length(ATokenIDs) do
    begin
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
      Inc(I);
    end;
    if Result = 0 then
      Exit;
    AStartPoint.Char := GetTokenPos + 1;
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
      Inc(ScanLine);
      while ScanLine < ASynEdit.Lines.Count do
      begin
        with ASynWeb, ASynEdit do
        begin
          SetRange(TSynEditStringList(Lines).Ranges[ScanLine - 1]);
          SetLine(Lines[ScanLine], +1);
        end;
        while not GetEol do
        begin
          if CheckToken then
            Exit;
          Next;
        end;
        Inc(ScanLine);
      end;
    end else
    begin
      if Length(FMatchStack) < 32 then
        SetLength(FMatchStack, 32);
      FMatchStackID := -1;
      Level := -1;
      with ASynWeb, ASynEdit do
      begin
        SetRange(TSynEditStringList(Lines).Ranges[ScanLine - 1]);
        SetLine(Lines[ScanLine], +1);
      end;
      while not GetEol and (GetTokenPos < AStartPoint.Char - 1) do
        CheckTokenBack;

      if FMatchStackID > -1 then
      begin
        Result := -2;
        AMatchtPoint := FMatchStack[FMatchStackID];
      end else
        while ScanLine > 0 do
        begin
          Dec(ScanLine);
          with ASynWeb, ASynEdit do
          begin
            SetRange(TSynEditStringList(Lines).Ranges[ScanLine - 1]);
            SetLine(Lines[ScanLine], +1);
          end;
          FMatchStackID := -1;
          while not GetEol do
            CheckTokenBack;
          if (Level >= 0) and (Level <= FMatchStackID) then
          begin
            Result := -2;
            AMatchtPoint := FMatchStack[Level];
            Break;
          end;
        end;
    end;
  end;
end;

function SynWebFindMatchingTokenEx(ASynEdit: TSynEdit; ASynWeb: TSynWebBase;
  const AOpenTokens, ACloseTokens: array of String;
  const ATokenIDs: array of TSynWebTokenKind; var AStartPoint: TBufferCoord;
  var AMatchtPoint: TBufferCoord; var ATokenIndex: Integer): Integer;
begin
  Result := SynWebFindMatchingToken(ASynEdit, ASynWeb, AOpenTokens, ACloseTokens,
    ATokenIDs, AStartPoint, AMatchtPoint, ATokenIndex);
  if (Result = 0) and (AStartPoint.Char > 1) then
  begin
    Dec(AStartPoint.Char);
    Result := SynWebFindMatchingToken(ASynEdit, ASynWeb, AOpenTokens, ACloseTokens,
      ATokenIDs, AStartPoint, AMatchtPoint, ATokenIndex);
  end;
end;

function SynWebUpdateActiveHighlighter(ASynEdit: TSynEdit;
  ASynWeb: TSynWebBase): TSynHighlighterTypes;
begin
  with ASynEdit,ASynWeb do
  begin
    if UpdateActiveHighlighter(TSynEditStringList(Lines).Ranges[CaretY-2],
       Lines[CaretY-1], CaretX, CaretY) then
      Repaint;
    Result:=ASynWeb.ActiveHighlighters;
  end;  
end;

initialization
  // None
finalization
  SetLength(FMatchStack, 0);
end.

