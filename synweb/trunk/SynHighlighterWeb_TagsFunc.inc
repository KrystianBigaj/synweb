function TSynWebEngine.HtmlTagUndef: TSynWebTokenKind;
begin
  Result:=stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc1: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(0) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc2: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(6) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc8: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(21) or
      HtmlTagKeyComp(34) or
      HtmlTagKeyComp(35) or
      HtmlTagKeyComp(36) or
      HtmlTagKeyComp(37) or
      HtmlTagKeyComp(38) or
      HtmlTagKeyComp(39) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc9: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(43) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc16: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(26) or
      HtmlTagKeyComp(63) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc17: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(49) or
      HtmlTagKeyComp(66) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc18: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(10) or
      HtmlTagKeyComp(28) or
      HtmlTagKeyComp(40) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc19: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(67) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc20: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(13) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc21: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(9) or
      HtmlTagKeyComp(22) or
      HtmlTagKeyComp(52) or
      HtmlTagKeyComp(88) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc23: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(1) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc24: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(23) or
      HtmlTagKeyComp(27) or
      HtmlTagKeyComp(80) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc25: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(5) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc26: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(41) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc27: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(7) or
      HtmlTagKeyComp(18) or
      HtmlTagKeyComp(60) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc28: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(83) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc29: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(45) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc30: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(19) or
      HtmlTagKeyComp(54) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc31: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(24) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc32: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(50) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc33: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(89) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc35: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(25) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc37: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(17) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc38: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(84) or
      HtmlTagKeyComp(86) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc39: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(56) or
      HtmlTagKeyComp(65) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc40: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(78) or
      HtmlTagKeyComp(87) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc41: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(90) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc42: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(47) or
      HtmlTagKeyComp(76) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc43: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(32) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc46: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(12) or
      HtmlTagKeyComp(53) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc47: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(51) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc49: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(64) or
      HtmlTagKeyComp(68) or
      HtmlTagKeyComp(91) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc50: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(72) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc52: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(31) or
      HtmlTagKeyComp(44) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc53: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(42) or
      HtmlTagKeyComp(55) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc55: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(30) or
      HtmlTagKeyComp(59) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc56: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(77) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc57: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(71) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc64: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(70) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc65: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(16) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc66: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(79) or
      HtmlTagKeyComp(85) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc70: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(3) or
      HtmlTagKeyComp(4) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc76: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(82) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc78: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(15) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc80: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(29) or
      HtmlTagKeyComp(46) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc81: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(75) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc82: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(8) or
      HtmlTagKeyComp(73) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc84: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(48) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc85: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(69) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc87: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(33) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc89: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(2) or
      HtmlTagKeyComp(62) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc91: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(57) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc92: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(14) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc93: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(74) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc94: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(81) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc107: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(20) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc114: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(58) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc121: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(11) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.HtmlTagFunc128: TSynWebTokenKind;
begin
  if  HtmlTagKeyComp(61) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;
