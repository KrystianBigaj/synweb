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
  if  MLTagKeyComp(8) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc8: TSynWebTokenKind;
begin
  if  MLTagKeyComp(24) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc9: TSynWebTokenKind;
begin
  if  MLTagKeyComp(48) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc16: TSynWebTokenKind;
begin
  if  MLTagKeyComp(29) or
      MLTagKeyComp(70) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc17: TSynWebTokenKind;
begin
  if  MLTagKeyComp(54) or
      MLTagKeyComp(75) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc18: TSynWebTokenKind;
begin
  if  MLTagKeyComp(12) or
      MLTagKeyComp(32) or
      MLTagKeyComp(45) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc19: TSynWebTokenKind;
begin
  if  MLTagKeyComp(30) or
      MLTagKeyComp(77) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc20: TSynWebTokenKind;
begin
  if  MLTagKeyComp(15) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc21: TSynWebTokenKind;
begin
  if  MLTagKeyComp(11) or
      MLTagKeyComp(25) or
      MLTagKeyComp(57) or
      MLTagKeyComp(101) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc22: TSynWebTokenKind;
begin
  if  MLTagKeyComp(38) then
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
  if  MLTagKeyComp(26) or
      MLTagKeyComp(31) or
      MLTagKeyComp(91) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc25: TSynWebTokenKind;
begin
  if  MLTagKeyComp(7) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc26: TSynWebTokenKind;
begin
  if  MLTagKeyComp(18) or
      MLTagKeyComp(46) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc27: TSynWebTokenKind;
begin
  if  MLTagKeyComp(9) or
      MLTagKeyComp(21) or
      MLTagKeyComp(66) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc28: TSynWebTokenKind;
begin
  if  MLTagKeyComp(95) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc29: TSynWebTokenKind;
begin
  if  MLTagKeyComp(50) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc30: TSynWebTokenKind;
begin
  if  MLTagKeyComp(22) or
      MLTagKeyComp(59) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc31: TSynWebTokenKind;
begin
  if  MLTagKeyComp(27) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc32: TSynWebTokenKind;
begin
  if  MLTagKeyComp(55) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc33: TSynWebTokenKind;
begin
  if  MLTagKeyComp(102) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc35: TSynWebTokenKind;
begin
  if  MLTagKeyComp(28) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc36: TSynWebTokenKind;
begin
  if  MLTagKeyComp(39) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc37: TSynWebTokenKind;
begin
  if  MLTagKeyComp(20) or
      MLTagKeyComp(40) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc38: TSynWebTokenKind;
begin
  if  MLTagKeyComp(41) or
      MLTagKeyComp(96) or
      MLTagKeyComp(99) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc39: TSynWebTokenKind;
begin
  if  MLTagKeyComp(42) or
      MLTagKeyComp(61) or
      MLTagKeyComp(73) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc40: TSynWebTokenKind;
begin
  if  MLTagKeyComp(43) or
      MLTagKeyComp(89) or
      MLTagKeyComp(100) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc41: TSynWebTokenKind;
begin
  if  MLTagKeyComp(44) or
      MLTagKeyComp(103) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc42: TSynWebTokenKind;
begin
  if  MLTagKeyComp(52) or
      MLTagKeyComp(87) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc43: TSynWebTokenKind;
begin
  if  MLTagKeyComp(36) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc46: TSynWebTokenKind;
begin
  if  MLTagKeyComp(14) or
      MLTagKeyComp(58) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc47: TSynWebTokenKind;
begin
  if  MLTagKeyComp(56) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc48: TSynWebTokenKind;
begin
  if  MLTagKeyComp(104) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc49: TSynWebTokenKind;
begin
  if  MLTagKeyComp(71) or
      MLTagKeyComp(78) or
      MLTagKeyComp(105) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc50: TSynWebTokenKind;
begin
  if  MLTagKeyComp(2) or
      MLTagKeyComp(83) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc52: TSynWebTokenKind;
begin
  if  MLTagKeyComp(35) or
      MLTagKeyComp(49) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc53: TSynWebTokenKind;
begin
  if  MLTagKeyComp(47) or
      MLTagKeyComp(60) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc55: TSynWebTokenKind;
begin
  if  MLTagKeyComp(34) or
      MLTagKeyComp(65) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc56: TSynWebTokenKind;
begin
  if  MLTagKeyComp(88) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc57: TSynWebTokenKind;
begin
  if  MLTagKeyComp(82) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc59: TSynWebTokenKind;
begin
  if  MLTagKeyComp(5) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc60: TSynWebTokenKind;
begin
  if  MLTagKeyComp(63) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc61: TSynWebTokenKind;
begin
  if  MLTagKeyComp(74) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc64: TSynWebTokenKind;
begin
  if  MLTagKeyComp(80) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc65: TSynWebTokenKind;
begin
  if  MLTagKeyComp(19) or
      MLTagKeyComp(97) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc66: TSynWebTokenKind;
begin
  if  MLTagKeyComp(90) or
      MLTagKeyComp(98) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc70: TSynWebTokenKind;
begin
  if  MLTagKeyComp(4) or
      MLTagKeyComp(6) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc76: TSynWebTokenKind;
begin
  if  MLTagKeyComp(94) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc78: TSynWebTokenKind;
begin
  if  MLTagKeyComp(17) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc79: TSynWebTokenKind;
begin
  if  MLTagKeyComp(76) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc80: TSynWebTokenKind;
begin
  if  MLTagKeyComp(33) or
      MLTagKeyComp(51) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc81: TSynWebTokenKind;
begin
  if  MLTagKeyComp(86) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc82: TSynWebTokenKind;
begin
  if  MLTagKeyComp(10) or
      MLTagKeyComp(84) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc84: TSynWebTokenKind;
begin
  if  MLTagKeyComp(53) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc85: TSynWebTokenKind;
begin
  if  MLTagKeyComp(79) or
      MLTagKeyComp(81) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc87: TSynWebTokenKind;
begin
  if  MLTagKeyComp(37) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc89: TSynWebTokenKind;
begin
  if  MLTagKeyComp(3) or
      MLTagKeyComp(69) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc91: TSynWebTokenKind;
begin
  if  MLTagKeyComp(62) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc92: TSynWebTokenKind;
begin
  if  MLTagKeyComp(16) or
      MLTagKeyComp(92) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc93: TSynWebTokenKind;
begin
  if  MLTagKeyComp(85) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc94: TSynWebTokenKind;
begin
  if  MLTagKeyComp(93) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc95: TSynWebTokenKind;
begin
  if  MLTagKeyComp(67) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc106: TSynWebTokenKind;
begin
  if  MLTagKeyComp(72) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc107: TSynWebTokenKind;
begin
  if  MLTagKeyComp(23) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc114: TSynWebTokenKind;
begin
  if  MLTagKeyComp(64) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc121: TSynWebTokenKind;
begin
  if  MLTagKeyComp(13) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc128: TSynWebTokenKind;
begin
  if  MLTagKeyComp(68) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;
