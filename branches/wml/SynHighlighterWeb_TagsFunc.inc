function TSynWebEngine.MLTagUndef: TSynWebTokenKind;
begin
  Result:=stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc1: TSynWebTokenKind;
begin
  if  MLTagKeyComp(0) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc2: TSynWebTokenKind;
begin
  if  MLTagKeyComp(6) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc8: TSynWebTokenKind;
begin
  if  MLTagKeyComp(21) or
      MLTagKeyComp(34) or
      MLTagKeyComp(35) or
      MLTagKeyComp(36) or
      MLTagKeyComp(37) or
      MLTagKeyComp(38) or
      MLTagKeyComp(39) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc9: TSynWebTokenKind;
begin
  if  MLTagKeyComp(43) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc16: TSynWebTokenKind;
begin
  if  MLTagKeyComp(26) or
      MLTagKeyComp(63) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc17: TSynWebTokenKind;
begin
  if  MLTagKeyComp(49) or
      MLTagKeyComp(66) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc18: TSynWebTokenKind;
begin
  if  MLTagKeyComp(10) or
      MLTagKeyComp(28) or
      MLTagKeyComp(40) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc19: TSynWebTokenKind;
begin
  if  MLTagKeyComp(67) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc20: TSynWebTokenKind;
begin
  if  MLTagKeyComp(13) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc21: TSynWebTokenKind;
begin
  if  MLTagKeyComp(9) or
      MLTagKeyComp(22) or
      MLTagKeyComp(52) or
      MLTagKeyComp(88) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc23: TSynWebTokenKind;
begin
  if  MLTagKeyComp(1) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc24: TSynWebTokenKind;
begin
  if  MLTagKeyComp(23) or
      MLTagKeyComp(27) or
      MLTagKeyComp(80) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc25: TSynWebTokenKind;
begin
  if  MLTagKeyComp(5) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc26: TSynWebTokenKind;
begin
  if  MLTagKeyComp(41) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc27: TSynWebTokenKind;
begin
  if  MLTagKeyComp(7) or
      MLTagKeyComp(18) or
      MLTagKeyComp(60) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc28: TSynWebTokenKind;
begin
  if  MLTagKeyComp(83) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc29: TSynWebTokenKind;
begin
  if  MLTagKeyComp(45) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc30: TSynWebTokenKind;
begin
  if  MLTagKeyComp(19) or
      MLTagKeyComp(54) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc31: TSynWebTokenKind;
begin
  if  MLTagKeyComp(24) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc32: TSynWebTokenKind;
begin
  if  MLTagKeyComp(50) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc33: TSynWebTokenKind;
begin
  if  MLTagKeyComp(89) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc35: TSynWebTokenKind;
begin
  if  MLTagKeyComp(25) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc37: TSynWebTokenKind;
begin
  if  MLTagKeyComp(17) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc38: TSynWebTokenKind;
begin
  if  MLTagKeyComp(84) or
      MLTagKeyComp(86) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc39: TSynWebTokenKind;
begin
  if  MLTagKeyComp(56) or
      MLTagKeyComp(65) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc40: TSynWebTokenKind;
begin
  if  MLTagKeyComp(78) or
      MLTagKeyComp(87) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc41: TSynWebTokenKind;
begin
  if  MLTagKeyComp(90) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc42: TSynWebTokenKind;
begin
  if  MLTagKeyComp(47) or
      MLTagKeyComp(76) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc43: TSynWebTokenKind;
begin
  if  MLTagKeyComp(32) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc46: TSynWebTokenKind;
begin
  if  MLTagKeyComp(12) or
      MLTagKeyComp(53) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc47: TSynWebTokenKind;
begin
  if  MLTagKeyComp(51) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc49: TSynWebTokenKind;
begin
  if  MLTagKeyComp(64) or
      MLTagKeyComp(68) or
      MLTagKeyComp(91) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc50: TSynWebTokenKind;
begin
  if  MLTagKeyComp(72) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc52: TSynWebTokenKind;
begin
  if  MLTagKeyComp(31) or
      MLTagKeyComp(44) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc53: TSynWebTokenKind;
begin
  if  MLTagKeyComp(42) or
      MLTagKeyComp(55) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc55: TSynWebTokenKind;
begin
  if  MLTagKeyComp(30) or
      MLTagKeyComp(59) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc56: TSynWebTokenKind;
begin
  if  MLTagKeyComp(77) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc57: TSynWebTokenKind;
begin
  if  MLTagKeyComp(71) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc64: TSynWebTokenKind;
begin
  if  MLTagKeyComp(70) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc65: TSynWebTokenKind;
begin
  if  MLTagKeyComp(16) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc66: TSynWebTokenKind;
begin
  if  MLTagKeyComp(79) or
      MLTagKeyComp(85) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc70: TSynWebTokenKind;
begin
  if  MLTagKeyComp(3) or
      MLTagKeyComp(4) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc76: TSynWebTokenKind;
begin
  if  MLTagKeyComp(82) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc78: TSynWebTokenKind;
begin
  if  MLTagKeyComp(15) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc80: TSynWebTokenKind;
begin
  if  MLTagKeyComp(29) or
      MLTagKeyComp(46) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc81: TSynWebTokenKind;
begin
  if  MLTagKeyComp(75) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc82: TSynWebTokenKind;
begin
  if  MLTagKeyComp(8) or
      MLTagKeyComp(73) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc84: TSynWebTokenKind;
begin
  if  MLTagKeyComp(48) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc85: TSynWebTokenKind;
begin
  if  MLTagKeyComp(69) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc87: TSynWebTokenKind;
begin
  if  MLTagKeyComp(33) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc89: TSynWebTokenKind;
begin
  if  MLTagKeyComp(2) or
      MLTagKeyComp(62) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc91: TSynWebTokenKind;
begin
  if  MLTagKeyComp(57) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc92: TSynWebTokenKind;
begin
  if  MLTagKeyComp(14) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc93: TSynWebTokenKind;
begin
  if  MLTagKeyComp(74) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc94: TSynWebTokenKind;
begin
  if  MLTagKeyComp(81) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc107: TSynWebTokenKind;
begin
  if  MLTagKeyComp(20) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc114: TSynWebTokenKind;
begin
  if  MLTagKeyComp(58) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc121: TSynWebTokenKind;
begin
  if  MLTagKeyComp(11) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc128: TSynWebTokenKind;
begin
  if  MLTagKeyComp(61) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;
