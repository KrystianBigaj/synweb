function TSynWebEngine.ES_KeywordIdent: TSynWebTokenKind;
begin
  Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc15: TSynWebTokenKind;
begin
  if  ES_KeywordComp(12) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc19: TSynWebTokenKind;
begin
  if  ES_KeywordComp(6) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc23: TSynWebTokenKind;
begin
  if  ES_KeywordComp(13) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc28: TSynWebTokenKind;
begin
  if  ES_KeywordComp(1) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc30: TSynWebTokenKind;
begin
  if  ES_KeywordComp(30) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc35: TSynWebTokenKind;
begin
  if  ES_KeywordComp(2) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc37: TSynWebTokenKind;
begin
  if  ES_KeywordComp(0) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc39: TSynWebTokenKind;
begin
  if  ES_KeywordComp(10) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc41: TSynWebTokenKind;
begin
  if  ES_KeywordComp(7) or
      ES_KeywordComp(23) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc42: TSynWebTokenKind;
begin
  if  ES_KeywordComp(15) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(38) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc43: TSynWebTokenKind;
begin
  if  ES_KeywordComp(8) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(43) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc44: TSynWebTokenKind;
begin
  if  ES_KeywordComp(47) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc48: TSynWebTokenKind;
begin
  if  ES_KeywordComp(45) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc50: TSynWebTokenKind;
begin
  if  ES_KeywordComp(24) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc51: TSynWebTokenKind;
begin
  if  ES_KeywordComp(5) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc52: TSynWebTokenKind;
begin
  if  ES_KeywordComp(29) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc53: TSynWebTokenKind;
begin
  if  ES_KeywordComp(35) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc54: TSynWebTokenKind;
begin
  if  ES_KeywordComp(31) or
      ES_KeywordComp(39) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc56: TSynWebTokenKind;
begin
  if  ES_KeywordComp(18) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc57: TSynWebTokenKind;
begin
  if  ES_KeywordComp(25) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(40) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc59: TSynWebTokenKind;
begin
  if  ES_KeywordComp(34) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc60: TSynWebTokenKind;
begin
  if  ES_KeywordComp(26) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc63: TSynWebTokenKind;
begin
  if  ES_KeywordComp(21) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(50) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc64: TSynWebTokenKind;
begin
  if  ES_KeywordComp(20) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(28) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc69: TSynWebTokenKind;
begin
  if  ES_KeywordComp(4) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(33) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc71: TSynWebTokenKind;
begin
  if  ES_KeywordComp(32) or
      ES_KeywordComp(46) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc72: TSynWebTokenKind;
begin
  if  ES_KeywordComp(52) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc79: TSynWebTokenKind;
begin
  if  ES_KeywordComp(9) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(53) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc80: TSynWebTokenKind;
begin
  if  ES_KeywordComp(51) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc81: TSynWebTokenKind;
begin
  if  ES_KeywordComp(44) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc82: TSynWebTokenKind;
begin
  if  ES_KeywordComp(17) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc84: TSynWebTokenKind;
begin
  if  ES_KeywordComp(19) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(27) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc87: TSynWebTokenKind;
begin
  if  ES_KeywordComp(22) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc91: TSynWebTokenKind;
begin
  if  ES_KeywordComp(37) or
      ES_KeywordComp(42) or
      ES_KeywordComp(48) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc96: TSynWebTokenKind;
begin
  if  ES_KeywordComp(16) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(57) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc98: TSynWebTokenKind;
begin
  if  ES_KeywordComp(36) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc101: TSynWebTokenKind;
begin
  if  ES_KeywordComp(3) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc102: TSynWebTokenKind;
begin
  if  ES_KeywordComp(11) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc103: TSynWebTokenKind;
begin
  if  ES_KeywordComp(55) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc106: TSynWebTokenKind;
begin
  if  ES_KeywordComp(14) then
    Result := stkESKeyword
  else
    if  ES_KeywordComp(49) then
      Result := stkESKeyword
    else
      Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc120: TSynWebTokenKind;
begin
  if  ES_KeywordComp(56) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc126: TSynWebTokenKind;
begin
  if  ES_KeywordComp(41) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;

function TSynWebEngine.ES_KeywordFunc160: TSynWebTokenKind;
begin
  if  ES_KeywordComp(54) then
    Result := stkESKeyword
  else
    Result := stkESIdentifier;
end;
