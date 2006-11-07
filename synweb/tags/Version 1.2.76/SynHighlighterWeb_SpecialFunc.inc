function TSynWebEngine.HtmlSpecialUndef: Boolean;
begin
  Result:=False;
end;

function TSynWebEngine.HtmlSpecialFunc12: Boolean;
begin
  if  HtmlSpecialKeyComp(79) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc16: Boolean;
begin
  if  HtmlSpecialKeyComp(46) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc17: Boolean;
begin
  if  HtmlSpecialKeyComp(111) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc19: Boolean;
begin
  if  HtmlSpecialKeyComp(13) or
      HtmlSpecialKeyComp(129) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc20: Boolean;
begin
  if  HtmlSpecialKeyComp(28) or
      HtmlSpecialKeyComp(33) or
      HtmlSpecialKeyComp(34) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc22: Boolean;
begin
  if  HtmlSpecialKeyComp(14) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc23: Boolean;
begin
  if  HtmlSpecialKeyComp(130) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc25: Boolean;
begin
  if  HtmlSpecialKeyComp(168) or
      HtmlSpecialKeyComp(169) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc26: Boolean;
begin
  if  HtmlSpecialKeyComp(63) or
      HtmlSpecialKeyComp(64) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc27: Boolean;
begin
  if  HtmlSpecialKeyComp(80) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc28: Boolean;
begin
  if  HtmlSpecialKeyComp(24) or
      HtmlSpecialKeyComp(25) or
      HtmlSpecialKeyComp(73) or
      HtmlSpecialKeyComp(74) or
      HtmlSpecialKeyComp(75) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc30: Boolean;
begin
  if  HtmlSpecialKeyComp(12) or
      HtmlSpecialKeyComp(126) or
      HtmlSpecialKeyComp(188) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc32: Boolean;
begin
  if  HtmlSpecialKeyComp(118) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc33: Boolean;
begin
  if  HtmlSpecialKeyComp(31) or
      HtmlSpecialKeyComp(35) or
      HtmlSpecialKeyComp(65) or
      HtmlSpecialKeyComp(66) or
      HtmlSpecialKeyComp(103) or
      HtmlSpecialKeyComp(104) or
      HtmlSpecialKeyComp(152) or
      HtmlSpecialKeyComp(166) or
      HtmlSpecialKeyComp(167) or
      HtmlSpecialKeyComp(242) or
      HtmlSpecialKeyComp(243) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc34: Boolean;
begin
  if  HtmlSpecialKeyComp(2) or
      HtmlSpecialKeyComp(3) or
      HtmlSpecialKeyComp(5) or
      HtmlSpecialKeyComp(6) or
      HtmlSpecialKeyComp(105) or
      HtmlSpecialKeyComp(124) or
      HtmlSpecialKeyComp(125) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc35: Boolean;
begin
  if  HtmlSpecialKeyComp(77) or
      HtmlSpecialKeyComp(78) or
      HtmlSpecialKeyComp(92) or
      HtmlSpecialKeyComp(119) or
      HtmlSpecialKeyComp(136) or
      HtmlSpecialKeyComp(137) or
      HtmlSpecialKeyComp(180) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc36: Boolean;
begin
  if  HtmlSpecialKeyComp(29) or
      HtmlSpecialKeyComp(30) or
      HtmlSpecialKeyComp(162) or
      HtmlSpecialKeyComp(187) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc38: Boolean;
begin
  if  HtmlSpecialKeyComp(10) or
      HtmlSpecialKeyComp(11) or
      HtmlSpecialKeyComp(53) or
      HtmlSpecialKeyComp(54) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc39: Boolean;
begin
  if  HtmlSpecialKeyComp(37) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc40: Boolean;
begin
  if  HtmlSpecialKeyComp(40) or
      HtmlSpecialKeyComp(181) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc41: Boolean;
begin
  if  HtmlSpecialKeyComp(44) or
      HtmlSpecialKeyComp(45) or
      HtmlSpecialKeyComp(71) or
      HtmlSpecialKeyComp(109) or
      HtmlSpecialKeyComp(147) or
      HtmlSpecialKeyComp(148) or
      HtmlSpecialKeyComp(190) or
      HtmlSpecialKeyComp(191) or
      HtmlSpecialKeyComp(204) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc42: Boolean;
begin
  if  HtmlSpecialKeyComp(32) or
      HtmlSpecialKeyComp(42) or
      HtmlSpecialKeyComp(43) or
      HtmlSpecialKeyComp(47) or
      HtmlSpecialKeyComp(48) or
      HtmlSpecialKeyComp(87) or
      HtmlSpecialKeyComp(88) or
      HtmlSpecialKeyComp(206) or
      HtmlSpecialKeyComp(215) or
      HtmlSpecialKeyComp(216) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc43: Boolean;
begin
  if  HtmlSpecialKeyComp(94) or
      HtmlSpecialKeyComp(115) or
      HtmlSpecialKeyComp(153) or
      HtmlSpecialKeyComp(192) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc44: Boolean;
begin
  if  HtmlSpecialKeyComp(177) or
      HtmlSpecialKeyComp(178) or
      HtmlSpecialKeyComp(246) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc45: Boolean;
begin
  if  HtmlSpecialKeyComp(81) or
      HtmlSpecialKeyComp(82) or
      HtmlSpecialKeyComp(95) or
      HtmlSpecialKeyComp(96) or
      HtmlSpecialKeyComp(101) or
      HtmlSpecialKeyComp(102) or
      HtmlSpecialKeyComp(120) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc46: Boolean;
begin
  if  HtmlSpecialKeyComp(49) or
      HtmlSpecialKeyComp(128) or
      HtmlSpecialKeyComp(235) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc47: Boolean;
begin
  if  HtmlSpecialKeyComp(21) or
      HtmlSpecialKeyComp(22) or
      HtmlSpecialKeyComp(27) or
      HtmlSpecialKeyComp(170) or
      HtmlSpecialKeyComp(185) or
      HtmlSpecialKeyComp(199) or
      HtmlSpecialKeyComp(207) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc48: Boolean;
begin
  if  HtmlSpecialKeyComp(140) or
      HtmlSpecialKeyComp(141) or
      HtmlSpecialKeyComp(142) or
      HtmlSpecialKeyComp(143) or
      HtmlSpecialKeyComp(226) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc49: Boolean;
begin
  if  HtmlSpecialKeyComp(16) or
      HtmlSpecialKeyComp(17) or
      HtmlSpecialKeyComp(107) or
      HtmlSpecialKeyComp(108) or
      HtmlSpecialKeyComp(131) or
      HtmlSpecialKeyComp(201) or
      HtmlSpecialKeyComp(202) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc50: Boolean;
begin
  if  HtmlSpecialKeyComp(4) or
      HtmlSpecialKeyComp(154) or
      HtmlSpecialKeyComp(224) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc51: Boolean;
begin
  if  HtmlSpecialKeyComp(0) or
      HtmlSpecialKeyComp(1) or
      HtmlSpecialKeyComp(15) or
      HtmlSpecialKeyComp(19) or
      HtmlSpecialKeyComp(20) or
      HtmlSpecialKeyComp(67) or
      HtmlSpecialKeyComp(68) or
      HtmlSpecialKeyComp(98) or
      HtmlSpecialKeyComp(127) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc52: Boolean;
begin
  if  HtmlSpecialKeyComp(93) or
      HtmlSpecialKeyComp(200) or
      HtmlSpecialKeyComp(249) or
      HtmlSpecialKeyComp(250) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc53: Boolean;
begin
  if  HtmlSpecialKeyComp(50) or
      HtmlSpecialKeyComp(58) or
      HtmlSpecialKeyComp(89) or
      HtmlSpecialKeyComp(114) or
      HtmlSpecialKeyComp(175) or
      HtmlSpecialKeyComp(208) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc54: Boolean;
begin
  if  HtmlSpecialKeyComp(7) or
      HtmlSpecialKeyComp(8) or
      HtmlSpecialKeyComp(59) or
      HtmlSpecialKeyComp(218) or
      HtmlSpecialKeyComp(219) or
      HtmlSpecialKeyComp(231) or
      HtmlSpecialKeyComp(232) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc55: Boolean;
begin
  if  HtmlSpecialKeyComp(51) or
      HtmlSpecialKeyComp(52) or
      HtmlSpecialKeyComp(99) or
      HtmlSpecialKeyComp(100) or
      HtmlSpecialKeyComp(146) or
      HtmlSpecialKeyComp(163) or
      HtmlSpecialKeyComp(165) or
      HtmlSpecialKeyComp(183) or
      HtmlSpecialKeyComp(184) or
      HtmlSpecialKeyComp(203) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc56: Boolean;
begin
  if  HtmlSpecialKeyComp(76) or
      HtmlSpecialKeyComp(133) or
      HtmlSpecialKeyComp(209) or
      HtmlSpecialKeyComp(210) or
      HtmlSpecialKeyComp(211) or
      HtmlSpecialKeyComp(212) or
      HtmlSpecialKeyComp(217) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc57: Boolean;
begin
  if  HtmlSpecialKeyComp(36) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc58: Boolean;
begin
  if  HtmlSpecialKeyComp(39) or
      HtmlSpecialKeyComp(55) or
      HtmlSpecialKeyComp(56) or
      HtmlSpecialKeyComp(121) or
      HtmlSpecialKeyComp(198) or
      HtmlSpecialKeyComp(229) or
      HtmlSpecialKeyComp(230) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc59: Boolean;
begin
  if  HtmlSpecialKeyComp(23) or
      HtmlSpecialKeyComp(38) or
      HtmlSpecialKeyComp(69) or
      HtmlSpecialKeyComp(85) or
      HtmlSpecialKeyComp(86) or
      HtmlSpecialKeyComp(251) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc61: Boolean;
begin
  if  HtmlSpecialKeyComp(160) or
      HtmlSpecialKeyComp(161) or
      HtmlSpecialKeyComp(173) or
      HtmlSpecialKeyComp(174) or
      HtmlSpecialKeyComp(213) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc62: Boolean;
begin
  if  HtmlSpecialKeyComp(84) or
      HtmlSpecialKeyComp(90) or
      HtmlSpecialKeyComp(91) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc63: Boolean;
begin
  if  HtmlSpecialKeyComp(26) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc64: Boolean;
begin
  if  HtmlSpecialKeyComp(72) or
      HtmlSpecialKeyComp(134) or
      HtmlSpecialKeyComp(135) or
      HtmlSpecialKeyComp(205) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc65: Boolean;
begin
  if  HtmlSpecialKeyComp(122) or
      HtmlSpecialKeyComp(138) or
      HtmlSpecialKeyComp(139) or
      HtmlSpecialKeyComp(157) or
      HtmlSpecialKeyComp(158) or
      HtmlSpecialKeyComp(176) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc66: Boolean;
begin
  if  HtmlSpecialKeyComp(106) or
      HtmlSpecialKeyComp(225) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc67: Boolean;
begin
  if  HtmlSpecialKeyComp(239) or
      HtmlSpecialKeyComp(240) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc68: Boolean;
begin
  if  HtmlSpecialKeyComp(144) or
      HtmlSpecialKeyComp(145) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc69: Boolean;
begin
  if  HtmlSpecialKeyComp(110) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc70: Boolean;
begin
  if  HtmlSpecialKeyComp(172) or
      HtmlSpecialKeyComp(196) or
      HtmlSpecialKeyComp(197) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc71: Boolean;
begin
  if  HtmlSpecialKeyComp(83) or
      HtmlSpecialKeyComp(227) or
      HtmlSpecialKeyComp(228) or
      HtmlSpecialKeyComp(247) or
      HtmlSpecialKeyComp(248) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc72: Boolean;
begin
  if  HtmlSpecialKeyComp(132) or
      HtmlSpecialKeyComp(182) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc73: Boolean;
begin
  if  HtmlSpecialKeyComp(164) or
      HtmlSpecialKeyComp(179) or
      HtmlSpecialKeyComp(214) or
      HtmlSpecialKeyComp(236) or
      HtmlSpecialKeyComp(252) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc74: Boolean;
begin
  if  HtmlSpecialKeyComp(18) or
      HtmlSpecialKeyComp(62) or
      HtmlSpecialKeyComp(155) or
      HtmlSpecialKeyComp(156) or
      HtmlSpecialKeyComp(195) or
      HtmlSpecialKeyComp(233) or
      HtmlSpecialKeyComp(234) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc75: Boolean;
begin
  if  HtmlSpecialKeyComp(186) or
      HtmlSpecialKeyComp(222) or
      HtmlSpecialKeyComp(223) or
      HtmlSpecialKeyComp(244) or
      HtmlSpecialKeyComp(245) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc76: Boolean;
begin
  if  HtmlSpecialKeyComp(123) or
      HtmlSpecialKeyComp(241) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc77: Boolean;
begin
  if  HtmlSpecialKeyComp(70) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc78: Boolean;
begin
  if  HtmlSpecialKeyComp(112) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc79: Boolean;
begin
  if  HtmlSpecialKeyComp(41) or
      HtmlSpecialKeyComp(57) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc81: Boolean;
begin
  if  HtmlSpecialKeyComp(9) or
      HtmlSpecialKeyComp(159) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc83: Boolean;
begin
  if  HtmlSpecialKeyComp(151) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc84: Boolean;
begin
  if  HtmlSpecialKeyComp(117) or
      HtmlSpecialKeyComp(189) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc85: Boolean;
begin
  if  HtmlSpecialKeyComp(116) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc86: Boolean;
begin
  if  HtmlSpecialKeyComp(221) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc87: Boolean;
begin
  if  HtmlSpecialKeyComp(149) or
      HtmlSpecialKeyComp(150) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc90: Boolean;
begin
  if  HtmlSpecialKeyComp(60) or
      HtmlSpecialKeyComp(61) or
      HtmlSpecialKeyComp(113) or
      HtmlSpecialKeyComp(194) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc91: Boolean;
begin
  if  HtmlSpecialKeyComp(97) or
      HtmlSpecialKeyComp(193) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc95: Boolean;
begin
  if  HtmlSpecialKeyComp(171) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc106: Boolean;
begin
  if  HtmlSpecialKeyComp(237) or
      HtmlSpecialKeyComp(238) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.HtmlSpecialFunc111: Boolean;
begin
  if  HtmlSpecialKeyComp(220) then
    Result := True
  else
    Result := False;
end;
