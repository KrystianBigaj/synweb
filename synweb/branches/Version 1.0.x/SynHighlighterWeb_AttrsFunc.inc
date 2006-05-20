function TSynWebEngine.HTML_AttrUndef: TtkTokenKind;
begin
  Result:=tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc13: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(55) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc15: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(45) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc23: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(0) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc26: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(32) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc27: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(23) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc30: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(15) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc31: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(40) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc32: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(57) or
      HTML_AttrKeyComp(65) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc33: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(7) or
      HTML_AttrKeyComp(68) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc34: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(58) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc35: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(95) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc37: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(19) or
      HTML_AttrKeyComp(51) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc38: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(39) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc39: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(18) or
      HTML_AttrKeyComp(22) or
      HTML_AttrKeyComp(46) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc40: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(107) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc43: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(5) or
      HTML_AttrKeyComp(47) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc45: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(96) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc46: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(60) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc47: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(6) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc48: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(1) or
      HTML_AttrKeyComp(33) or
      HTML_AttrKeyComp(38) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc49: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(27) or
      HTML_AttrKeyComp(104) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc50: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(106) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc52: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(53) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc53: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(9) or
      HTML_AttrKeyComp(100) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc54: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(20) or
      HTML_AttrKeyComp(24) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc55: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(73) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc56: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(41) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc57: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(16) or
      HTML_AttrKeyComp(50) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc58: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(56) or
      HTML_AttrKeyComp(101) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc59: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(105) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc60: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(49) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc61: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(82) or
      HTML_AttrKeyComp(119) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc62: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(4) or
      HTML_AttrKeyComp(12) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc63: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(26) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc64: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(124) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc65: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(66) or
      HTML_AttrKeyComp(118) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc66: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(8) or
      HTML_AttrKeyComp(36) or
      HTML_AttrKeyComp(44) or
      HTML_AttrKeyComp(69) or
      HTML_AttrKeyComp(71) or
      HTML_AttrKeyComp(115) or
      HTML_AttrKeyComp(116) or
      HTML_AttrKeyComp(123) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc67: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(21) or
      HTML_AttrKeyComp(75) or
      HTML_AttrKeyComp(76) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc68: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(59) or
      HTML_AttrKeyComp(122) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc69: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(114) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc71: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(29) or
      HTML_AttrKeyComp(42) or
      HTML_AttrKeyComp(52) or
      HTML_AttrKeyComp(113) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc72: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(11) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc73: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(103) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc74: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(17) or
      HTML_AttrKeyComp(31) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc75: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(97) or
      HTML_AttrKeyComp(99) or
      HTML_AttrKeyComp(117) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc77: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(37) or
      HTML_AttrKeyComp(125) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc78: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(109) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc79: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(61) or
      HTML_AttrKeyComp(112) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc80: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(28) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc81: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(92) or
      HTML_AttrKeyComp(110) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc82: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(74) or
      HTML_AttrKeyComp(127) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc85: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(77) or
      HTML_AttrKeyComp(108) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc87: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(13) or
      HTML_AttrKeyComp(72) or
      HTML_AttrKeyComp(126) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc88: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(43) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc91: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(3) or
      HTML_AttrKeyComp(30) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc93: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(25) or
      HTML_AttrKeyComp(78) or
      HTML_AttrKeyComp(89) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc94: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(94) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc96: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(10) or
      HTML_AttrKeyComp(88) or
      HTML_AttrKeyComp(91) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc98: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(93) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc101: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(14) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc102: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(121) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc103: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(2) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc104: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(64) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc105: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(48) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc106: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(98) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc107: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(81) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc108: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(67) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc109: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(102) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc110: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(111) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc111: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(70) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc113: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(90) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc114: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(35) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc119: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(34) or
      HTML_AttrKeyComp(54) or
      HTML_AttrKeyComp(62) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc126: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(63) or
      HTML_AttrKeyComp(79) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc127: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(120) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc139: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(87) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc147: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(80) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc157: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(84) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc158: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(83) or
      HTML_AttrKeyComp(85) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;

function TSynWebEngine.HTML_AttrFunc162: TtkTokenKind;
begin
  if  HTML_AttrKeyComp(86) then
    Result := tkHTMLTagKey
  else
    Result := tkHTMLTagKeyUndef;
end;
