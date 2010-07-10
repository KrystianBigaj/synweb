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
  if  MLTagKeyComp(11) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc6: TSynWebTokenKind;
begin
  if  MLTagKeyComp(14) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc8: TSynWebTokenKind;
begin
  if  MLTagKeyComp(33) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc9: TSynWebTokenKind;
begin
  if  MLTagKeyComp(65) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc16: TSynWebTokenKind;
begin
  if  MLTagKeyComp(39) or
      MLTagKeyComp(94) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc17: TSynWebTokenKind;
begin
  if  MLTagKeyComp(71) or
      MLTagKeyComp(100) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc18: TSynWebTokenKind;
begin
  if  MLTagKeyComp(16) or
      MLTagKeyComp(42) or
      MLTagKeyComp(60) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc19: TSynWebTokenKind;
begin
  if  MLTagKeyComp(40) or
      MLTagKeyComp(105) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc20: TSynWebTokenKind;
begin
  if  MLTagKeyComp(20) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc21: TSynWebTokenKind;
begin
  if  MLTagKeyComp(15) or
      MLTagKeyComp(34) or
      MLTagKeyComp(75) or
      MLTagKeyComp(134) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc22: TSynWebTokenKind;
begin
  if  MLTagKeyComp(53) then
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
  if  MLTagKeyComp(36) or
      MLTagKeyComp(41) or
      MLTagKeyComp(122) then
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
  if  MLTagKeyComp(24) or
      MLTagKeyComp(63) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc27: TSynWebTokenKind;
begin
  if  MLTagKeyComp(12) or
      MLTagKeyComp(27) or
      MLTagKeyComp(88) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc28: TSynWebTokenKind;
begin
  if  MLTagKeyComp(127) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc29: TSynWebTokenKind;
begin
  if  MLTagKeyComp(43) or
      MLTagKeyComp(67) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc30: TSynWebTokenKind;
begin
  if  MLTagKeyComp(28) or
      MLTagKeyComp(77) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc31: TSynWebTokenKind;
begin
  if  MLTagKeyComp(37) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc32: TSynWebTokenKind;
begin
  if  MLTagKeyComp(73) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc33: TSynWebTokenKind;
begin
  if  MLTagKeyComp(135) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc34: TSynWebTokenKind;
begin
  if  MLTagKeyComp(102) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc35: TSynWebTokenKind;
begin
  if  MLTagKeyComp(38) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc36: TSynWebTokenKind;
begin
  if  MLTagKeyComp(54) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc37: TSynWebTokenKind;
begin
  if  MLTagKeyComp(26) or
      MLTagKeyComp(55) or
      MLTagKeyComp(82) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc38: TSynWebTokenKind;
begin
  if  MLTagKeyComp(9) or
      MLTagKeyComp(56) or
      MLTagKeyComp(103) or
      MLTagKeyComp(128) or
      MLTagKeyComp(132) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc39: TSynWebTokenKind;
begin
  if  MLTagKeyComp(57) or
      MLTagKeyComp(80) or
      MLTagKeyComp(97) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc40: TSynWebTokenKind;
begin
  if  MLTagKeyComp(58) or
      MLTagKeyComp(120) or
      MLTagKeyComp(133) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc41: TSynWebTokenKind;
begin
  if  MLTagKeyComp(59) or
      MLTagKeyComp(61) or
      MLTagKeyComp(136) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc42: TSynWebTokenKind;
begin
  if  MLTagKeyComp(69) or
      MLTagKeyComp(117) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc43: TSynWebTokenKind;
begin
  if  MLTagKeyComp(51) or
      MLTagKeyComp(78) or
      MLTagKeyComp(138) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc46: TSynWebTokenKind;
begin
  if  MLTagKeyComp(19) or
      MLTagKeyComp(76) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc47: TSynWebTokenKind;
begin
  if  MLTagKeyComp(74) or
      MLTagKeyComp(129) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc48: TSynWebTokenKind;
begin
  if  MLTagKeyComp(139) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc49: TSynWebTokenKind;
begin
  if  MLTagKeyComp(95) or
      MLTagKeyComp(106) or
      MLTagKeyComp(140) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc50: TSynWebTokenKind;
begin
  if  MLTagKeyComp(2) or
      MLTagKeyComp(10) or
      MLTagKeyComp(113) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc52: TSynWebTokenKind;
begin
  if  MLTagKeyComp(50) or
      MLTagKeyComp(66) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc53: TSynWebTokenKind;
begin
  if  MLTagKeyComp(64) or
      MLTagKeyComp(79) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc55: TSynWebTokenKind;
begin
  if  MLTagKeyComp(48) or
      MLTagKeyComp(87) or
      MLTagKeyComp(137) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc56: TSynWebTokenKind;
begin
  if  MLTagKeyComp(119) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc57: TSynWebTokenKind;
begin
  if  MLTagKeyComp(111) then
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
  if  MLTagKeyComp(22) or
      MLTagKeyComp(44) or
      MLTagKeyComp(84) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc61: TSynWebTokenKind;
begin
  if  MLTagKeyComp(81) or
      MLTagKeyComp(98) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc63: TSynWebTokenKind;
begin
  if  MLTagKeyComp(31) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc64: TSynWebTokenKind;
begin
  if  MLTagKeyComp(109) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc65: TSynWebTokenKind;
begin
  if  MLTagKeyComp(25) or
      MLTagKeyComp(130) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc66: TSynWebTokenKind;
begin
  if  MLTagKeyComp(32) or
      MLTagKeyComp(47) or
      MLTagKeyComp(104) or
      MLTagKeyComp(121) or
      MLTagKeyComp(131) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc67: TSynWebTokenKind;
begin
  if  MLTagKeyComp(72) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc68: TSynWebTokenKind;
begin
  if  MLTagKeyComp(8) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc70: TSynWebTokenKind;
begin
  if  MLTagKeyComp(4) or
      MLTagKeyComp(6) or
      MLTagKeyComp(35) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc76: TSynWebTokenKind;
begin
  if  MLTagKeyComp(126) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc78: TSynWebTokenKind;
begin
  if  MLTagKeyComp(23) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc79: TSynWebTokenKind;
begin
  if  MLTagKeyComp(49) or
      MLTagKeyComp(101) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc80: TSynWebTokenKind;
begin
  if  MLTagKeyComp(45) or
      MLTagKeyComp(68) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc81: TSynWebTokenKind;
begin
  if  MLTagKeyComp(112) or
      MLTagKeyComp(116) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc82: TSynWebTokenKind;
begin
  if  MLTagKeyComp(13) or
      MLTagKeyComp(114) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc84: TSynWebTokenKind;
begin
  if  MLTagKeyComp(70) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc85: TSynWebTokenKind;
begin
  if  MLTagKeyComp(62) or
      MLTagKeyComp(107) or
      MLTagKeyComp(108) or
      MLTagKeyComp(110) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc86: TSynWebTokenKind;
begin
  if  MLTagKeyComp(46) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc87: TSynWebTokenKind;
begin
  if  MLTagKeyComp(52) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc89: TSynWebTokenKind;
begin
  if  MLTagKeyComp(3) or
      MLTagKeyComp(92) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc91: TSynWebTokenKind;
begin
  if  MLTagKeyComp(29) or
      MLTagKeyComp(83) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc92: TSynWebTokenKind;
begin
  if  MLTagKeyComp(21) or
      MLTagKeyComp(123) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc93: TSynWebTokenKind;
begin
  if  MLTagKeyComp(115) or
      MLTagKeyComp(124) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc94: TSynWebTokenKind;
begin
  if  MLTagKeyComp(85) or
      MLTagKeyComp(125) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc95: TSynWebTokenKind;
begin
  if  MLTagKeyComp(89) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc98: TSynWebTokenKind;
begin
  if  MLTagKeyComp(99) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc106: TSynWebTokenKind;
begin
  if  MLTagKeyComp(96) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc107: TSynWebTokenKind;
begin
  if  MLTagKeyComp(30) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc110: TSynWebTokenKind;
begin
  if  MLTagKeyComp(118) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc112: TSynWebTokenKind;
begin
  if  MLTagKeyComp(90) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc113: TSynWebTokenKind;
begin
  if  MLTagKeyComp(93) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc114: TSynWebTokenKind;
begin
  if  MLTagKeyComp(86) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc116: TSynWebTokenKind;
begin
  if  MLTagKeyComp(17) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc121: TSynWebTokenKind;
begin
  if  MLTagKeyComp(18) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc128: TSynWebTokenKind;
begin
  if  MLTagKeyComp(91) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;
