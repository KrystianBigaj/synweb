function TSynWebSyn.HTML_TagUndef: TtkTokenKind;
begin
  Result:=tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc1: TtkTokenKind;
begin
  if  HTML_TagKeyComp(0) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc2: TtkTokenKind;
begin
  if  HTML_TagKeyComp(6) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc8: TtkTokenKind;
begin
  if  HTML_TagKeyComp(21) or
      HTML_TagKeyComp(34) or
      HTML_TagKeyComp(35) or
      HTML_TagKeyComp(36) or
      HTML_TagKeyComp(37) or
      HTML_TagKeyComp(38) or
      HTML_TagKeyComp(39) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc9: TtkTokenKind;
begin
  if  HTML_TagKeyComp(43) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc16: TtkTokenKind;
begin
  if  HTML_TagKeyComp(26) or
      HTML_TagKeyComp(63) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc17: TtkTokenKind;
begin
  if  HTML_TagKeyComp(49) or
      HTML_TagKeyComp(66) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc18: TtkTokenKind;
begin
  if  HTML_TagKeyComp(10) or
      HTML_TagKeyComp(28) or
      HTML_TagKeyComp(40) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc19: TtkTokenKind;
begin
  if  HTML_TagKeyComp(67) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc20: TtkTokenKind;
begin
  if  HTML_TagKeyComp(13) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc21: TtkTokenKind;
begin
  if  HTML_TagKeyComp(9) or
      HTML_TagKeyComp(22) or
      HTML_TagKeyComp(52) or
      HTML_TagKeyComp(88) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc23: TtkTokenKind;
begin
  if  HTML_TagKeyComp(1) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc24: TtkTokenKind;
begin
  if  HTML_TagKeyComp(23) or
      HTML_TagKeyComp(27) or
      HTML_TagKeyComp(80) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc25: TtkTokenKind;
begin
  if  HTML_TagKeyComp(5) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc26: TtkTokenKind;
begin
  if  HTML_TagKeyComp(41) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc27: TtkTokenKind;
begin
  if  HTML_TagKeyComp(7) or
      HTML_TagKeyComp(18) or
      HTML_TagKeyComp(60) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc28: TtkTokenKind;
begin
  if  HTML_TagKeyComp(83) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc29: TtkTokenKind;
begin
  if  HTML_TagKeyComp(45) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc30: TtkTokenKind;
begin
  if  HTML_TagKeyComp(19) or
      HTML_TagKeyComp(54) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc31: TtkTokenKind;
begin
  if  HTML_TagKeyComp(24) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc32: TtkTokenKind;
begin
  if  HTML_TagKeyComp(50) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc33: TtkTokenKind;
begin
  if  HTML_TagKeyComp(89) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc35: TtkTokenKind;
begin
  if  HTML_TagKeyComp(25) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc37: TtkTokenKind;
begin
  if  HTML_TagKeyComp(17) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc38: TtkTokenKind;
begin
  if  HTML_TagKeyComp(84) or
      HTML_TagKeyComp(86) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc39: TtkTokenKind;
begin
  if  HTML_TagKeyComp(56) or
      HTML_TagKeyComp(65) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc40: TtkTokenKind;
begin
  if  HTML_TagKeyComp(78) or
      HTML_TagKeyComp(87) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc41: TtkTokenKind;
begin
  if  HTML_TagKeyComp(90) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc42: TtkTokenKind;
begin
  if  HTML_TagKeyComp(47) or
      HTML_TagKeyComp(76) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc43: TtkTokenKind;
begin
  if  HTML_TagKeyComp(32) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc46: TtkTokenKind;
begin
  if  HTML_TagKeyComp(12) or
      HTML_TagKeyComp(53) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc47: TtkTokenKind;
begin
  if  HTML_TagKeyComp(51) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc49: TtkTokenKind;
begin
  if  HTML_TagKeyComp(64) or
      HTML_TagKeyComp(68) or
      HTML_TagKeyComp(91) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc50: TtkTokenKind;
begin
  if  HTML_TagKeyComp(72) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc52: TtkTokenKind;
begin
  if  HTML_TagKeyComp(31) or
      HTML_TagKeyComp(44) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc53: TtkTokenKind;
begin
  if  HTML_TagKeyComp(42) or
      HTML_TagKeyComp(55) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc55: TtkTokenKind;
begin
  if  HTML_TagKeyComp(30) or
      HTML_TagKeyComp(59) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc56: TtkTokenKind;
begin
  if  HTML_TagKeyComp(77) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc57: TtkTokenKind;
begin
  if  HTML_TagKeyComp(71) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc64: TtkTokenKind;
begin
  if  HTML_TagKeyComp(70) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc65: TtkTokenKind;
begin
  if  HTML_TagKeyComp(16) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc66: TtkTokenKind;
begin
  if  HTML_TagKeyComp(79) or
      HTML_TagKeyComp(85) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc70: TtkTokenKind;
begin
  if  HTML_TagKeyComp(3) or
      HTML_TagKeyComp(4) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc76: TtkTokenKind;
begin
  if  HTML_TagKeyComp(82) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc78: TtkTokenKind;
begin
  if  HTML_TagKeyComp(15) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc80: TtkTokenKind;
begin
  if  HTML_TagKeyComp(29) or
      HTML_TagKeyComp(46) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc81: TtkTokenKind;
begin
  if  HTML_TagKeyComp(75) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc82: TtkTokenKind;
begin
  if  HTML_TagKeyComp(8) or
      HTML_TagKeyComp(73) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc84: TtkTokenKind;
begin
  if  HTML_TagKeyComp(48) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc85: TtkTokenKind;
begin
  if  HTML_TagKeyComp(69) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc87: TtkTokenKind;
begin
  if  HTML_TagKeyComp(33) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc89: TtkTokenKind;
begin
  if  HTML_TagKeyComp(2) or
      HTML_TagKeyComp(62) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc91: TtkTokenKind;
begin
  if  HTML_TagKeyComp(57) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc92: TtkTokenKind;
begin
  if  HTML_TagKeyComp(14) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc93: TtkTokenKind;
begin
  if  HTML_TagKeyComp(74) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc94: TtkTokenKind;
begin
  if  HTML_TagKeyComp(81) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc107: TtkTokenKind;
begin
  if  HTML_TagKeyComp(20) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc114: TtkTokenKind;
begin
  if  HTML_TagKeyComp(58) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc121: TtkTokenKind;
begin
  if  HTML_TagKeyComp(11) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;

function TSynWebSyn.HTML_TagFunc128: TtkTokenKind;
begin
  if  HTML_TagKeyComp(61) then
    Result := tkHTMLTagName
  else
    Result := tkHTMLTagNameUndef;
end;
