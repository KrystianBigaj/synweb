function TSynWebSyn.ES_KeywordIdent: TtkTokenKind;
begin
  Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc15: TtkTokenKind;
begin
  if  ES_KeywordComp(12) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc19: TtkTokenKind;
begin
  if  ES_KeywordComp(6) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc23: TtkTokenKind;
begin
  if  ES_KeywordComp(13) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc28: TtkTokenKind;
begin
  if  ES_KeywordComp(1) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc30: TtkTokenKind;
begin
  if  ES_KeywordComp(30) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc35: TtkTokenKind;
begin
  if  ES_KeywordComp(2) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc37: TtkTokenKind;
begin
  if  ES_KeywordComp(0) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc39: TtkTokenKind;
begin
  if  ES_KeywordComp(10) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc41: TtkTokenKind;
begin
  if  ES_KeywordComp(7) or
      ES_KeywordComp(23) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc42: TtkTokenKind;
begin
  if  ES_KeywordComp(15) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(38) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc43: TtkTokenKind;
begin
  if  ES_KeywordComp(8) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(43) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc44: TtkTokenKind;
begin
  if  ES_KeywordComp(47) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc48: TtkTokenKind;
begin
  if  ES_KeywordComp(45) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc50: TtkTokenKind;
begin
  if  ES_KeywordComp(24) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc51: TtkTokenKind;
begin
  if  ES_KeywordComp(5) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc52: TtkTokenKind;
begin
  if  ES_KeywordComp(29) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc53: TtkTokenKind;
begin
  if  ES_KeywordComp(35) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc54: TtkTokenKind;
begin
  if  ES_KeywordComp(31) or
      ES_KeywordComp(39) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc56: TtkTokenKind;
begin
  if  ES_KeywordComp(18) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc57: TtkTokenKind;
begin
  if  ES_KeywordComp(25) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(40) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc59: TtkTokenKind;
begin
  if  ES_KeywordComp(34) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc60: TtkTokenKind;
begin
  if  ES_KeywordComp(26) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc63: TtkTokenKind;
begin
  if  ES_KeywordComp(21) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(50) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc64: TtkTokenKind;
begin
  if  ES_KeywordComp(20) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(28) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc69: TtkTokenKind;
begin
  if  ES_KeywordComp(4) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(33) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc71: TtkTokenKind;
begin
  if  ES_KeywordComp(32) or
      ES_KeywordComp(46) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc72: TtkTokenKind;
begin
  if  ES_KeywordComp(52) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc79: TtkTokenKind;
begin
  if  ES_KeywordComp(9) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(53) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc80: TtkTokenKind;
begin
  if  ES_KeywordComp(51) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc81: TtkTokenKind;
begin
  if  ES_KeywordComp(44) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc82: TtkTokenKind;
begin
  if  ES_KeywordComp(17) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc84: TtkTokenKind;
begin
  if  ES_KeywordComp(19) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(27) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc87: TtkTokenKind;
begin
  if  ES_KeywordComp(22) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc91: TtkTokenKind;
begin
  if  ES_KeywordComp(37) or
      ES_KeywordComp(42) or
      ES_KeywordComp(48) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc96: TtkTokenKind;
begin
  if  ES_KeywordComp(16) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(57) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc98: TtkTokenKind;
begin
  if  ES_KeywordComp(36) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc101: TtkTokenKind;
begin
  if  ES_KeywordComp(3) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc102: TtkTokenKind;
begin
  if  ES_KeywordComp(11) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc103: TtkTokenKind;
begin
  if  ES_KeywordComp(55) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc106: TtkTokenKind;
begin
  if  ES_KeywordComp(14) then
    Result := tkESKeyword
  else
    if  ES_KeywordComp(49) then
      Result := tkESKeyword
    else
      Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc120: TtkTokenKind;
begin
  if  ES_KeywordComp(56) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc126: TtkTokenKind;
begin
  if  ES_KeywordComp(41) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;

function TSynWebSyn.ES_KeywordFunc160: TtkTokenKind;
begin
  if  ES_KeywordComp(54) then
    Result := tkESKeyword
  else
    Result := tkESIdentifier;
end;
