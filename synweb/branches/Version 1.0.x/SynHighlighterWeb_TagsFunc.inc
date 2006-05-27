function TSynWebEngine.Html_TagUndef: TSynWebTokenKind;
begin
  Result:=stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc1: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(0) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc2: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(6) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc8: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(21) or
      Html_TagKeyComp(34) or
      Html_TagKeyComp(35) or
      Html_TagKeyComp(36) or
      Html_TagKeyComp(37) or
      Html_TagKeyComp(38) or
      Html_TagKeyComp(39) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc9: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(43) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc16: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(26) or
      Html_TagKeyComp(63) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc17: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(49) or
      Html_TagKeyComp(66) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc18: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(10) or
      Html_TagKeyComp(28) or
      Html_TagKeyComp(40) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc19: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(67) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc20: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(13) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc21: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(9) or
      Html_TagKeyComp(22) or
      Html_TagKeyComp(52) or
      Html_TagKeyComp(88) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc23: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(1) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc24: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(23) or
      Html_TagKeyComp(27) or
      Html_TagKeyComp(80) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc25: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(5) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc26: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(41) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc27: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(7) or
      Html_TagKeyComp(18) or
      Html_TagKeyComp(60) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc28: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(83) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc29: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(45) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc30: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(19) or
      Html_TagKeyComp(54) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc31: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(24) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc32: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(50) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc33: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(89) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc35: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(25) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc37: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(17) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc38: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(84) or
      Html_TagKeyComp(86) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc39: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(56) or
      Html_TagKeyComp(65) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc40: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(78) or
      Html_TagKeyComp(87) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc41: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(90) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc42: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(47) or
      Html_TagKeyComp(76) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc43: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(32) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc46: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(12) or
      Html_TagKeyComp(53) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc47: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(51) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc49: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(64) or
      Html_TagKeyComp(68) or
      Html_TagKeyComp(91) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc50: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(72) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc52: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(31) or
      Html_TagKeyComp(44) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc53: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(42) or
      Html_TagKeyComp(55) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc55: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(30) or
      Html_TagKeyComp(59) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc56: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(77) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc57: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(71) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc64: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(70) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc65: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(16) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc66: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(79) or
      Html_TagKeyComp(85) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc70: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(3) or
      Html_TagKeyComp(4) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc76: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(82) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc78: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(15) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc80: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(29) or
      Html_TagKeyComp(46) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc81: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(75) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc82: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(8) or
      Html_TagKeyComp(73) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc84: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(48) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc85: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(69) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc87: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(33) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc89: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(2) or
      Html_TagKeyComp(62) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc91: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(57) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc92: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(14) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc93: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(74) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc94: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(81) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc107: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(20) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc114: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(58) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc121: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(11) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;

function TSynWebEngine.Html_TagFunc128: TSynWebTokenKind;
begin
  if  Html_TagKeyComp(61) then
    Result := stkHtmlTagName
  else
    Result := stkHtmlTagNameUndef;
end;
