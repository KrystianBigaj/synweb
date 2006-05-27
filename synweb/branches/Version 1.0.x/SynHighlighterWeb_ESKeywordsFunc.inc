function TSynWebEngine.Es_KeywordIdent: TSynWebTokenKind;
begin
  Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc15: TSynWebTokenKind;
begin
  if  Es_KeywordComp(12) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc19: TSynWebTokenKind;
begin
  if  Es_KeywordComp(6) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc23: TSynWebTokenKind;
begin
  if  Es_KeywordComp(13) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc28: TSynWebTokenKind;
begin
  if  Es_KeywordComp(1) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc30: TSynWebTokenKind;
begin
  if  Es_KeywordComp(30) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc35: TSynWebTokenKind;
begin
  if  Es_KeywordComp(2) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc37: TSynWebTokenKind;
begin
  if  Es_KeywordComp(0) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc39: TSynWebTokenKind;
begin
  if  Es_KeywordComp(10) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc41: TSynWebTokenKind;
begin
  if  Es_KeywordComp(7) or
      Es_KeywordComp(23) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc42: TSynWebTokenKind;
begin
  if  Es_KeywordComp(15) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(38) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc43: TSynWebTokenKind;
begin
  if  Es_KeywordComp(8) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(43) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc44: TSynWebTokenKind;
begin
  if  Es_KeywordComp(47) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc48: TSynWebTokenKind;
begin
  if  Es_KeywordComp(45) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc50: TSynWebTokenKind;
begin
  if  Es_KeywordComp(24) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc51: TSynWebTokenKind;
begin
  if  Es_KeywordComp(5) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc52: TSynWebTokenKind;
begin
  if  Es_KeywordComp(29) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc53: TSynWebTokenKind;
begin
  if  Es_KeywordComp(35) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc54: TSynWebTokenKind;
begin
  if  Es_KeywordComp(31) or
      Es_KeywordComp(39) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc56: TSynWebTokenKind;
begin
  if  Es_KeywordComp(18) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc57: TSynWebTokenKind;
begin
  if  Es_KeywordComp(25) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(40) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc59: TSynWebTokenKind;
begin
  if  Es_KeywordComp(34) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc60: TSynWebTokenKind;
begin
  if  Es_KeywordComp(26) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc63: TSynWebTokenKind;
begin
  if  Es_KeywordComp(21) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(50) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc64: TSynWebTokenKind;
begin
  if  Es_KeywordComp(20) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(28) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc69: TSynWebTokenKind;
begin
  if  Es_KeywordComp(4) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(33) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc71: TSynWebTokenKind;
begin
  if  Es_KeywordComp(32) or
      Es_KeywordComp(46) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc72: TSynWebTokenKind;
begin
  if  Es_KeywordComp(52) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc79: TSynWebTokenKind;
begin
  if  Es_KeywordComp(9) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(53) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc80: TSynWebTokenKind;
begin
  if  Es_KeywordComp(51) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc81: TSynWebTokenKind;
begin
  if  Es_KeywordComp(44) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc82: TSynWebTokenKind;
begin
  if  Es_KeywordComp(17) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc84: TSynWebTokenKind;
begin
  if  Es_KeywordComp(19) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(27) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc87: TSynWebTokenKind;
begin
  if  Es_KeywordComp(22) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc91: TSynWebTokenKind;
begin
  if  Es_KeywordComp(37) or
      Es_KeywordComp(42) or
      Es_KeywordComp(48) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc96: TSynWebTokenKind;
begin
  if  Es_KeywordComp(16) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(57) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc98: TSynWebTokenKind;
begin
  if  Es_KeywordComp(36) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc101: TSynWebTokenKind;
begin
  if  Es_KeywordComp(3) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc102: TSynWebTokenKind;
begin
  if  Es_KeywordComp(11) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc103: TSynWebTokenKind;
begin
  if  Es_KeywordComp(55) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc106: TSynWebTokenKind;
begin
  if  Es_KeywordComp(14) then
    Result := stkEsKeyword
  else
    if  Es_KeywordComp(49) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc120: TSynWebTokenKind;
begin
  if  Es_KeywordComp(56) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc126: TSynWebTokenKind;
begin
  if  Es_KeywordComp(41) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.Es_KeywordFunc160: TSynWebTokenKind;
begin
  if  Es_KeywordComp(54) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;
