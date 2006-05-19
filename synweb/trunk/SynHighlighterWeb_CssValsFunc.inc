function TSynWebSyn.CSS_ValUndef: TtkTokenKind;
begin
  Result:=tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc26: TtkTokenKind;
begin
  if  CSS_ValKeyComp(59) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc27: TtkTokenKind;
begin
  if  CSS_ValKeyComp(28) or
      CSS_ValKeyComp(125) or
      CSS_ValKeyComp(130) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc29: TtkTokenKind;
begin
  if  CSS_ValKeyComp(12) or
      CSS_ValKeyComp(43) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc31: TtkTokenKind;
begin
  if  CSS_ValKeyComp(91) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc32: TtkTokenKind;
begin
  if  CSS_ValKeyComp(60) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc33: TtkTokenKind;
begin
  if  CSS_ValKeyComp(16) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc35: TtkTokenKind;
begin
  if  CSS_ValKeyComp(40) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc36: TtkTokenKind;
begin
  if  CSS_ValKeyComp(25) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc37: TtkTokenKind;
begin
  if  CSS_ValKeyComp(112) or
      CSS_ValKeyComp(189) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc38: TtkTokenKind;
begin
  if  CSS_ValKeyComp(170) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc39: TtkTokenKind;
begin
  if  CSS_ValKeyComp(79) or
      CSS_ValKeyComp(119) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc40: TtkTokenKind;
begin
  if  CSS_ValKeyComp(3) or
      CSS_ValKeyComp(15) or
      CSS_ValKeyComp(161) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc41: TtkTokenKind;
begin
  if  CSS_ValKeyComp(35) or
      CSS_ValKeyComp(57) or
      CSS_ValKeyComp(62) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc42: TtkTokenKind;
begin
  if  CSS_ValKeyComp(9) or
      CSS_ValKeyComp(50) or
      CSS_ValKeyComp(158) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc43: TtkTokenKind;
begin
  if  CSS_ValKeyComp(14) or
      CSS_ValKeyComp(72) or
      CSS_ValKeyComp(74) or
      CSS_ValKeyComp(131) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc44: TtkTokenKind;
begin
  if  CSS_ValKeyComp(58) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc45: TtkTokenKind;
begin
  if  CSS_ValKeyComp(0) or
      CSS_ValKeyComp(18) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc46: TtkTokenKind;
begin
  if  CSS_ValKeyComp(48) or
      CSS_ValKeyComp(97) or
      CSS_ValKeyComp(124) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc47: TtkTokenKind;
begin
  if  CSS_ValKeyComp(36) or
      CSS_ValKeyComp(96) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc48: TtkTokenKind;
begin
  if  CSS_ValKeyComp(13) or
      CSS_ValKeyComp(51) or
      CSS_ValKeyComp(103) or
      CSS_ValKeyComp(190) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc49: TtkTokenKind;
begin
  if  CSS_ValKeyComp(46) or
      CSS_ValKeyComp(55) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc50: TtkTokenKind;
begin
  if  CSS_ValKeyComp(26) or
      CSS_ValKeyComp(83) or
      CSS_ValKeyComp(90) or
      CSS_ValKeyComp(135) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc51: TtkTokenKind;
begin
  if  CSS_ValKeyComp(7) or
      CSS_ValKeyComp(54) or
      CSS_ValKeyComp(174) or
      CSS_ValKeyComp(175) or
      CSS_ValKeyComp(176) or
      CSS_ValKeyComp(183) or
      CSS_ValKeyComp(188) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc52: TtkTokenKind;
begin
  if  CSS_ValKeyComp(82) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc53: TtkTokenKind;
begin
  if  CSS_ValKeyComp(94) or
      CSS_ValKeyComp(163) or
      CSS_ValKeyComp(185) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc54: TtkTokenKind;
begin
  if  CSS_ValKeyComp(70) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc55: TtkTokenKind;
begin
  if  CSS_ValKeyComp(61) or
      CSS_ValKeyComp(99) or
      CSS_ValKeyComp(192) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc56: TtkTokenKind;
begin
  if  CSS_ValKeyComp(17) or
      CSS_ValKeyComp(77) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc57: TtkTokenKind;
begin
  if  CSS_ValKeyComp(6) or
      CSS_ValKeyComp(10) or
      CSS_ValKeyComp(136) or
      CSS_ValKeyComp(141) or
      CSS_ValKeyComp(147) or
      CSS_ValKeyComp(191) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc59: TtkTokenKind;
begin
  if  CSS_ValKeyComp(5) or
      CSS_ValKeyComp(42) or
      CSS_ValKeyComp(152) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc60: TtkTokenKind;
begin
  if  CSS_ValKeyComp(68) or
      CSS_ValKeyComp(114) or
      CSS_ValKeyComp(120) or
      CSS_ValKeyComp(151) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc61: TtkTokenKind;
begin
  if  CSS_ValKeyComp(73) or
      CSS_ValKeyComp(75) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc62: TtkTokenKind;
begin
  if  CSS_ValKeyComp(100) or
      CSS_ValKeyComp(132) or
      CSS_ValKeyComp(194) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc63: TtkTokenKind;
begin
  if  CSS_ValKeyComp(64) or
      CSS_ValKeyComp(111) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc65: TtkTokenKind;
begin
  if  CSS_ValKeyComp(22) or
      CSS_ValKeyComp(93) or
      CSS_ValKeyComp(127) or
      CSS_ValKeyComp(142) or
      CSS_ValKeyComp(186) or
      CSS_ValKeyComp(195) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc67: TtkTokenKind;
begin
  if  CSS_ValKeyComp(8) or
      CSS_ValKeyComp(52) or
      CSS_ValKeyComp(67) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc68: TtkTokenKind;
begin
  if  CSS_ValKeyComp(39) or
      CSS_ValKeyComp(41) or
      CSS_ValKeyComp(44) or
      CSS_ValKeyComp(47) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc69: TtkTokenKind;
begin
  if  CSS_ValKeyComp(38) or
      CSS_ValKeyComp(49) or
      CSS_ValKeyComp(145) or
      CSS_ValKeyComp(171) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc70: TtkTokenKind;
begin
  if  CSS_ValKeyComp(128) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc71: TtkTokenKind;
begin
  if  CSS_ValKeyComp(129) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc72: TtkTokenKind;
begin
  if  CSS_ValKeyComp(156) or
      CSS_ValKeyComp(196) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc73: TtkTokenKind;
begin
  if  CSS_ValKeyComp(84) or
      CSS_ValKeyComp(106) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc74: TtkTokenKind;
begin
  if  CSS_ValKeyComp(193) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc75: TtkTokenKind;
begin
  if  CSS_ValKeyComp(4) or
      CSS_ValKeyComp(105) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc76: TtkTokenKind;
begin
  if  CSS_ValKeyComp(53) or
      CSS_ValKeyComp(92) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc77: TtkTokenKind;
begin
  if  CSS_ValKeyComp(108) or
      CSS_ValKeyComp(148) or
      CSS_ValKeyComp(168) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc78: TtkTokenKind;
begin
  if  CSS_ValKeyComp(21) or
      CSS_ValKeyComp(121) or
      CSS_ValKeyComp(184) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc79: TtkTokenKind;
begin
  if  CSS_ValKeyComp(78) or
      CSS_ValKeyComp(138) or
      CSS_ValKeyComp(143) or
      CSS_ValKeyComp(159) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc80: TtkTokenKind;
begin
  if  CSS_ValKeyComp(133) or
      CSS_ValKeyComp(150) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc81: TtkTokenKind;
begin
  if  CSS_ValKeyComp(2) or
      CSS_ValKeyComp(110) or
      CSS_ValKeyComp(154) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc82: TtkTokenKind;
begin
  if  CSS_ValKeyComp(56) or
      CSS_ValKeyComp(101) or
      CSS_ValKeyComp(155) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc83: TtkTokenKind;
begin
  if  CSS_ValKeyComp(29) or
      CSS_ValKeyComp(63) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc84: TtkTokenKind;
begin
  if  CSS_ValKeyComp(66) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc85: TtkTokenKind;
begin
  if  CSS_ValKeyComp(19) or
      CSS_ValKeyComp(139) or
      CSS_ValKeyComp(144) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc86: TtkTokenKind;
begin
  if  CSS_ValKeyComp(45) or
      CSS_ValKeyComp(187) or
      CSS_ValKeyComp(197) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc87: TtkTokenKind;
begin
  if  CSS_ValKeyComp(65) or
      CSS_ValKeyComp(107) or
      CSS_ValKeyComp(140) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc88: TtkTokenKind;
begin
  if  CSS_ValKeyComp(69) or
      CSS_ValKeyComp(81) or
      CSS_ValKeyComp(123) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc89: TtkTokenKind;
begin
  if  CSS_ValKeyComp(23) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc91: TtkTokenKind;
begin
  if  CSS_ValKeyComp(95) or
      CSS_ValKeyComp(137) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc92: TtkTokenKind;
begin
  if  CSS_ValKeyComp(85) or
      CSS_ValKeyComp(126) or
      CSS_ValKeyComp(146) or
      CSS_ValKeyComp(198) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc93: TtkTokenKind;
begin
  if  CSS_ValKeyComp(116) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc95: TtkTokenKind;
begin
  if  CSS_ValKeyComp(1) or
      CSS_ValKeyComp(179) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc96: TtkTokenKind;
begin
  if  CSS_ValKeyComp(31) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc97: TtkTokenKind;
begin
  if  CSS_ValKeyComp(34) or
      CSS_ValKeyComp(118) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc99: TtkTokenKind;
begin
  if  CSS_ValKeyComp(162) or
      CSS_ValKeyComp(164) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc100: TtkTokenKind;
begin
  if  CSS_ValKeyComp(87) or
      CSS_ValKeyComp(109) or
      CSS_ValKeyComp(115) or
      CSS_ValKeyComp(117) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc101: TtkTokenKind;
begin
  if  CSS_ValKeyComp(11) or
      CSS_ValKeyComp(86) or
      CSS_ValKeyComp(98) or
      CSS_ValKeyComp(153) or
      CSS_ValKeyComp(173) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc102: TtkTokenKind;
begin
  if  CSS_ValKeyComp(20) or
      CSS_ValKeyComp(157) or
      CSS_ValKeyComp(178) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc104: TtkTokenKind;
begin
  if  CSS_ValKeyComp(180) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc105: TtkTokenKind;
begin
  if  CSS_ValKeyComp(160) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc108: TtkTokenKind;
begin
  if  CSS_ValKeyComp(24) or
      CSS_ValKeyComp(76) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc109: TtkTokenKind;
begin
  if  CSS_ValKeyComp(113) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc110: TtkTokenKind;
begin
  if  CSS_ValKeyComp(33) or
      CSS_ValKeyComp(71) or
      CSS_ValKeyComp(88) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc113: TtkTokenKind;
begin
  if  CSS_ValKeyComp(27) or
      CSS_ValKeyComp(181) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc115: TtkTokenKind;
begin
  if  CSS_ValKeyComp(32) or
      CSS_ValKeyComp(89) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc116: TtkTokenKind;
begin
  if  CSS_ValKeyComp(149) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc117: TtkTokenKind;
begin
  if  CSS_ValKeyComp(122) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc118: TtkTokenKind;
begin
  if  CSS_ValKeyComp(80) or
      CSS_ValKeyComp(182) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc119: TtkTokenKind;
begin
  if  CSS_ValKeyComp(104) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc120: TtkTokenKind;
begin
  if  CSS_ValKeyComp(167) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc123: TtkTokenKind;
begin
  if  CSS_ValKeyComp(102) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc125: TtkTokenKind;
begin
  if  CSS_ValKeyComp(37) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc127: TtkTokenKind;
begin
  if  CSS_ValKeyComp(134) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc135: TtkTokenKind;
begin
  if  CSS_ValKeyComp(169) or
      CSS_ValKeyComp(172) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc146: TtkTokenKind;
begin
  if  CSS_ValKeyComp(177) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc151: TtkTokenKind;
begin
  if  CSS_ValKeyComp(30) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc157: TtkTokenKind;
begin
  if  CSS_ValKeyComp(165) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;

function TSynWebSyn.CSS_ValFunc158: TtkTokenKind;
begin
  if  CSS_ValKeyComp(166) then
    Result := tkCssVal
  else
    Result := tkCssValUndef;
end;
