function TSynWebEngine.HTML_TagUndef: TSynWebTokenKind;
begin
  Result:=stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc1: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(0) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc2: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(6) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc8: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(21) or
      HTML_TagKeyComp(34) or
      HTML_TagKeyComp(35) or
      HTML_TagKeyComp(36) or
      HTML_TagKeyComp(37) or
      HTML_TagKeyComp(38) or
      HTML_TagKeyComp(39) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc9: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(43) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc16: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(26) or
      HTML_TagKeyComp(63) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc17: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(49) or
      HTML_TagKeyComp(66) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc18: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(10) or
      HTML_TagKeyComp(28) or
      HTML_TagKeyComp(40) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc19: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(67) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc20: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(13) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc21: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(9) or
      HTML_TagKeyComp(22) or
      HTML_TagKeyComp(52) or
      HTML_TagKeyComp(88) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc23: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(1) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc24: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(23) or
      HTML_TagKeyComp(27) or
      HTML_TagKeyComp(80) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc25: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(5) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc26: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(41) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc27: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(7) or
      HTML_TagKeyComp(18) or
      HTML_TagKeyComp(60) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc28: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(83) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc29: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(45) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc30: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(19) or
      HTML_TagKeyComp(54) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc31: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(24) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc32: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(50) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc33: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(89) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc35: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(25) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc37: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(17) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc38: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(84) or
      HTML_TagKeyComp(86) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc39: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(56) or
      HTML_TagKeyComp(65) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc40: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(78) or
      HTML_TagKeyComp(87) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc41: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(90) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc42: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(47) or
      HTML_TagKeyComp(76) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc43: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(32) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc46: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(12) or
      HTML_TagKeyComp(53) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc47: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(51) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc49: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(64) or
      HTML_TagKeyComp(68) or
      HTML_TagKeyComp(91) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc50: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(72) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc52: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(31) or
      HTML_TagKeyComp(44) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc53: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(42) or
      HTML_TagKeyComp(55) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc55: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(30) or
      HTML_TagKeyComp(59) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc56: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(77) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc57: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(71) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc64: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(70) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc65: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(16) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc66: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(79) or
      HTML_TagKeyComp(85) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc70: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(3) or
      HTML_TagKeyComp(4) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc76: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(82) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc78: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(15) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc80: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(29) or
      HTML_TagKeyComp(46) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc81: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(75) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc82: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(8) or
      HTML_TagKeyComp(73) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc84: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(48) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc85: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(69) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc87: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(33) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc89: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(2) or
      HTML_TagKeyComp(62) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc91: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(57) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc92: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(14) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc93: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(74) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc94: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(81) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc107: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(20) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc114: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(58) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc121: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(11) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;

function TSynWebEngine.HTML_TagFunc128: TSynWebTokenKind;
begin
  if  HTML_TagKeyComp(61) then
    Result := stkHTMLTagName
  else
    Result := stkHTMLTagNameUndef;
end;
