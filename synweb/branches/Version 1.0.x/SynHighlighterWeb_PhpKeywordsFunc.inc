function TSynWebEngine.Php_KeywordIdent: TSynWebTokenKind;
begin
  Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc14: TSynWebTokenKind;
begin
  if  Php_FunctionComp(272) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc15: TSynWebTokenKind;
begin
  if  Php_KeywordComp(41) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc16: TSynWebTokenKind;
begin
  if  Php_FunctionComp(795) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc17: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1057) or
      Php_FunctionComp(2520) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc18: TSynWebTokenKind;
begin
  if  Php_KeywordComp(18) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc19: TSynWebTokenKind;
begin
  if  Php_KeywordComp(6) or
      Php_KeywordComp(19) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc20: TSynWebTokenKind;
begin
  if  Php_KeywordComp(8) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc22: TSynWebTokenKind;
begin
  if  Php_FunctionComp(93) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc23: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1090) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc24: TSynWebTokenKind;
begin
  if  Php_FunctionComp(486) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc25: TSynWebTokenKind;
begin
  if  Php_FunctionComp(3639) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc28: TSynWebTokenKind;
begin
  if  Php_KeywordComp(10) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(4079) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc29: TSynWebTokenKind;
begin
  if  Php_FunctionComp(346) or
      Php_FunctionComp(355) or
      Php_FunctionComp(2088) or
      Php_FunctionComp(2192) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc30: TSynWebTokenKind;
begin
  if  Php_FunctionComp(547) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc31: TSynWebTokenKind;
begin
  if  Php_KeywordComp(20) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(791) or
        Php_FunctionComp(3348) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc32: TSynWebTokenKind;
begin
  if  Php_KeywordComp(1) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(1234) or
        Php_FunctionComp(1240) or
        Php_FunctionComp(2238) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc33: TSynWebTokenKind;
begin
  if  Php_KeywordComp(54) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(2255) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc34: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1276) or
      Php_FunctionComp(2203) or
      Php_FunctionComp(2204) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc35: TSynWebTokenKind;
begin
  if  Php_KeywordComp(11) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(1091) or
        Php_FunctionComp(2214) or
        Php_FunctionComp(2293) or
        Php_FunctionComp(4755) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc36: TSynWebTokenKind;
begin
  if  Php_FunctionComp(255) or
      Php_FunctionComp(256) or
      Php_FunctionComp(1434) or
      Php_FunctionComp(1508) or
      Php_FunctionComp(2287) or
      Php_FunctionComp(2564) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc37: TSynWebTokenKind;
begin
  if  Php_KeywordComp(9) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(275) or
        Php_FunctionComp(297) or
        Php_FunctionComp(406) or
        Php_FunctionComp(772) or
        Php_FunctionComp(1111) or
        Php_FunctionComp(3322) or
        Php_FunctionComp(3899) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc38: TSynWebTokenKind;
begin
  if  Php_KeywordComp(27) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(94) or
        Php_FunctionComp(1270) or
        Php_FunctionComp(2308) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc39: TSynWebTokenKind;
begin
  if  Php_KeywordComp(37) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(779) or
        Php_FunctionComp(1790) or
        Php_FunctionComp(3873) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc40: TSynWebTokenKind;
begin
  if  Php_KeywordComp(3) or
      Php_KeywordComp(30) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(274) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc41: TSynWebTokenKind;
begin
  if  Php_KeywordComp(21) or
      Php_KeywordComp(71) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(97) or
        Php_FunctionComp(1236) or
        Php_FunctionComp(1518) or
        Php_FunctionComp(2143) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc42: TSynWebTokenKind;
begin
  if  Php_KeywordComp(35) or
      Php_KeywordComp(50) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(349) or
        Php_FunctionComp(2152) or
        Php_FunctionComp(4111) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc43: TSynWebTokenKind;
begin
  if  Php_KeywordComp(34) or
      Php_KeywordComp(46) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(250) or
        Php_FunctionComp(353) or
        Php_FunctionComp(638) or
        Php_FunctionComp(776) or
        Php_FunctionComp(4756) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc44: TSynWebTokenKind;
begin
  if  Php_FunctionComp(257) or
      Php_FunctionComp(1093) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc45: TSynWebTokenKind;
begin
  if  Php_KeywordComp(70) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(292) or
        Php_FunctionComp(407) or
        Php_FunctionComp(1124) or
        Php_FunctionComp(1438) or
        Php_FunctionComp(3900) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc46: TSynWebTokenKind;
begin
  if  Php_FunctionComp(95) or
      Php_FunctionComp(1341) or
      Php_FunctionComp(1912) or
      Php_FunctionComp(1913) or
      Php_FunctionComp(2199) or
      Php_FunctionComp(2237) or
      Php_FunctionComp(3006) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc47: TSynWebTokenKind;
begin
  if  Php_FunctionComp(294) or
      Php_FunctionComp(778) or
      Php_FunctionComp(1267) or
      Php_FunctionComp(4793) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc48: TSynWebTokenKind;
begin
  if  Php_KeywordComp(17) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(2125) or
        Php_FunctionComp(3384) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc49: TSynWebTokenKind;
begin
  if  Php_KeywordComp(14) or
      Php_KeywordComp(40) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(676) or
        Php_FunctionComp(773) or
        Php_FunctionComp(1340) or
        Php_FunctionComp(1525) or
        Php_FunctionComp(2521) or
        Php_FunctionComp(2557) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc50: TSynWebTokenKind;
begin
  if  Php_FunctionComp(347) or
      Php_FunctionComp(774) or
      Php_FunctionComp(1435) or
      Php_FunctionComp(1448) or
      Php_FunctionComp(1588) or
      Php_FunctionComp(2205) or
      Php_FunctionComp(2261) or
      Php_FunctionComp(2408) or
      Php_FunctionComp(3198) or
      Php_FunctionComp(3242) or
      Php_FunctionComp(4112) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc51: TSynWebTokenKind;
begin
  if  Php_FunctionComp(251) or
      Php_FunctionComp(276) or
      Php_FunctionComp(412) or
      Php_FunctionComp(780) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc52: TSynWebTokenKind;
begin
  if  Php_FunctionComp(200) or
      Php_FunctionComp(273) or
      Php_FunctionComp(352) or
      Php_FunctionComp(1345) or
      Php_FunctionComp(2137) or
      Php_FunctionComp(3063) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc53: TSynWebTokenKind;
begin
  if  Php_FunctionComp(3307) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc54: TSynWebTokenKind;
begin
  if  Php_KeywordComp(0) or
      Php_KeywordComp(13) or
      Php_KeywordComp(36) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(369) or
        Php_FunctionComp(3687) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc55: TSynWebTokenKind;
begin
  if  Php_KeywordComp(52) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(1344) or
        Php_FunctionComp(1385) or
        Php_FunctionComp(1439) or
        Php_FunctionComp(2570) or
        Php_FunctionComp(4281) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc56: TSynWebTokenKind;
begin
  if  Php_KeywordComp(22) or
      Php_KeywordComp(38) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(334) or
        Php_FunctionComp(785) or
        Php_FunctionComp(1272) or
        Php_FunctionComp(3936) or
        Php_FunctionComp(4338) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc57: TSynWebTokenKind;
begin
  if  Php_KeywordComp(72) or
      Php_KeywordComp(73) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(683) or
        Php_FunctionComp(761) or
        Php_FunctionComp(1215) or
        Php_FunctionComp(1238) or
        Php_FunctionComp(1860) or
        Php_FunctionComp(1914) or
        Php_FunctionComp(2100) or
        Php_FunctionComp(3743) or
        Php_FunctionComp(4113) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc58: TSynWebTokenKind;
begin
  if  Php_KeywordComp(32) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(674) or
        Php_FunctionComp(1126) or
        Php_FunctionComp(1437) or
        Php_FunctionComp(1801) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc59: TSynWebTokenKind;
begin
  if  Php_KeywordComp(51) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(101) or
        Php_FunctionComp(290) or
        Php_FunctionComp(405) or
        Php_FunctionComp(2092) or
        Php_FunctionComp(2252) or
        Php_FunctionComp(2418) or
        Php_FunctionComp(3057) or
        Php_FunctionComp(3907) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc60: TSynWebTokenKind;
begin
  if  Php_KeywordComp(49) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(271) or
        Php_FunctionComp(337) or
        Php_FunctionComp(350) or
        Php_FunctionComp(630) or
        Php_FunctionComp(1198) or
        Php_FunctionComp(1523) or
        Php_FunctionComp(2094) or
        Php_FunctionComp(2230) or
        Php_FunctionComp(2537) or
        Php_FunctionComp(3908) or
        Php_FunctionComp(4080) or
        Php_FunctionComp(4370) or
        Php_FunctionComp(4803) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc61: TSynWebTokenKind;
begin
  if  Php_FunctionComp(673) or
      Php_FunctionComp(704) or
      Php_FunctionComp(1512) or
      Php_FunctionComp(2182) or
      Php_FunctionComp(2239) or
      Php_FunctionComp(3211) or
      Php_FunctionComp(3695) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc62: TSynWebTokenKind;
begin
  if  Php_KeywordComp(25) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(194) or
        Php_FunctionComp(295) or
        Php_FunctionComp(698) or
        Php_FunctionComp(1410) or
        Php_FunctionComp(1411) or
        Php_FunctionComp(1458) or
        Php_FunctionComp(2153) or
        Php_FunctionComp(3385) or
        Php_FunctionComp(3557) or
        Php_FunctionComp(3949) or
        Php_FunctionComp(4339) or
        Php_FunctionComp(4817) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc63: TSynWebTokenKind;
begin
  if  Php_KeywordComp(7) or
      Php_KeywordComp(59) or
      Php_KeywordComp(68) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(269) or
        Php_FunctionComp(354) or
        Php_FunctionComp(389) or
        Php_FunctionComp(1220) or
        Php_FunctionComp(1524) or
        Php_FunctionComp(1771) or
        Php_FunctionComp(2292) or
        Php_FunctionComp(3003) or
        Php_FunctionComp(3299) or
        Php_FunctionComp(4293) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc64: TSynWebTokenKind;
begin
  if  Php_KeywordComp(67) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(557) or
        Php_FunctionComp(792) or
        Php_FunctionComp(2042) or
        Php_FunctionComp(4520) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc65: TSynWebTokenKind;
begin
  if  Php_KeywordComp(4) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(338) or
        Php_FunctionComp(672) or
        Php_FunctionComp(1206) or
        Php_FunctionComp(1271) or
        Php_FunctionComp(1504) or
        Php_FunctionComp(1861) or
        Php_FunctionComp(3083) or
        Php_FunctionComp(3148) or
        Php_FunctionComp(3431) or
        Php_FunctionComp(4121) or
        Php_FunctionComp(4838) or
        Php_FunctionComp(5144) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc66: TSynWebTokenKind;
begin
  if  Php_FunctionComp(482) or
      Php_FunctionComp(700) or
      Php_FunctionComp(1217) or
      Php_FunctionComp(1268) or
      Php_FunctionComp(1269) or
      Php_FunctionComp(1343) or
      Php_FunctionComp(1447) or
      Php_FunctionComp(1640) or
      Php_FunctionComp(1953) or
      Php_FunctionComp(3397) or
      Php_FunctionComp(3485) or
      Php_FunctionComp(3652) or
      Php_FunctionComp(4842) or
      Php_FunctionComp(5143) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc67: TSynWebTokenKind;
begin
  if  Php_FunctionComp(681) or
      Php_FunctionComp(686) or
      Php_FunctionComp(1442) or
      Php_FunctionComp(3942) or
      Php_FunctionComp(4799) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc68: TSynWebTokenKind;
begin
  if  Php_KeywordComp(43) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(423) or
        Php_FunctionComp(687) or
        Php_FunctionComp(786) or
        Php_FunctionComp(1202) or
        Php_FunctionComp(1441) or
        Php_FunctionComp(1456) or
        Php_FunctionComp(1510) or
        Php_FunctionComp(3464) or
        Php_FunctionComp(3910) or
        Php_FunctionComp(4000) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc69: TSynWebTokenKind;
begin
  if  Php_FunctionComp(382) or
      Php_FunctionComp(415) or
      Php_FunctionComp(444) or
      Php_FunctionComp(708) or
      Php_FunctionComp(775) or
      Php_FunctionComp(1064) or
      Php_FunctionComp(1459) or
      Php_FunctionComp(1957) or
      Php_FunctionComp(2527) or
      Php_FunctionComp(3097) or
      Php_FunctionComp(3168) or
      Php_FunctionComp(3603) or
      Php_FunctionComp(4022) or
      Php_FunctionComp(5056) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc70: TSynWebTokenKind;
begin
  if  Php_FunctionComp(702) or
      Php_FunctionComp(1136) or
      Php_FunctionComp(1201) or
      Php_FunctionComp(1950) or
      Php_FunctionComp(2682) or
      Php_FunctionComp(3359) or
      Php_FunctionComp(3620) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc71: TSynWebTokenKind;
begin
  if  Php_KeywordComp(15) or
      Php_KeywordComp(24) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(150) or
        Php_FunctionComp(193) or
        Php_FunctionComp(396) or
        Php_FunctionComp(2067) or
        Php_FunctionComp(2571) or
        Php_FunctionComp(3310) or
        Php_FunctionComp(3744) or
        Php_FunctionComp(4820) or
        Php_FunctionComp(4998) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc72: TSynWebTokenKind;
begin
  if  Php_KeywordComp(48) or
      Php_KeywordComp(63) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(577) or
        Php_FunctionComp(1235) or
        Php_FunctionComp(1522) or
        Php_FunctionComp(1922) or
        Php_FunctionComp(2090) or
        Php_FunctionComp(2207) or
        Php_FunctionComp(2208) or
        Php_FunctionComp(3480) or
        Php_FunctionComp(3950) or
        Php_FunctionComp(4282) or
        Php_FunctionComp(4866) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc73: TSynWebTokenKind;
begin
  if  Php_FunctionComp(252) or
      Php_FunctionComp(270) or
      Php_FunctionComp(408) or
      Php_FunctionComp(685) or
      Php_FunctionComp(701) or
      Php_FunctionComp(1412) or
      Php_FunctionComp(1466) or
      Php_FunctionComp(1467) or
      Php_FunctionComp(1786) or
      Php_FunctionComp(1917) or
      Php_FunctionComp(2006) or
      Php_FunctionComp(2053) or
      Php_FunctionComp(2206) or
      Php_FunctionComp(2522) or
      Php_FunctionComp(3484) or
      Php_FunctionComp(3946) or
      Php_FunctionComp(3947) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc74: TSynWebTokenKind;
begin
  if  Php_FunctionComp(100) or
      Php_FunctionComp(1131) or
      Php_FunctionComp(1356) or
      Php_FunctionComp(1638) or
      Php_FunctionComp(1900) or
      Php_FunctionComp(1949) or
      Php_FunctionComp(1979) or
      Php_FunctionComp(2012) or
      Php_FunctionComp(2073) or
      Php_FunctionComp(2098) or
      Php_FunctionComp(2174) or
      Php_FunctionComp(3558) or
      Php_FunctionComp(3764) or
      Php_FunctionComp(3923) or
      Php_FunctionComp(4337) or
      Php_FunctionComp(4829) or
      Php_FunctionComp(4839) or
      Php_FunctionComp(4931) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc75: TSynWebTokenKind;
begin
  if  Php_FunctionComp(436) or
      Php_FunctionComp(552) or
      Php_FunctionComp(646) or
      Php_FunctionComp(1528) or
      Php_FunctionComp(1632) or
      Php_FunctionComp(1750) or
      Php_FunctionComp(1919) or
      Php_FunctionComp(3065) or
      Php_FunctionComp(3750) or
      Php_FunctionComp(3786) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc76: TSynWebTokenKind;
begin
  if  Php_FunctionComp(442) or
      Php_FunctionComp(1239) or
      Php_FunctionComp(1454) or
      Php_FunctionComp(1724) or
      Php_FunctionComp(1996) or
      Php_FunctionComp(2045) or
      Php_FunctionComp(2071) or
      Php_FunctionComp(2091) or
      Php_FunctionComp(2099) or
      Php_FunctionComp(2104) or
      Php_FunctionComp(3311) or
      Php_FunctionComp(3396) or
      Php_FunctionComp(3465) or
      Php_FunctionComp(4291) or
      Php_FunctionComp(5127) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc77: TSynWebTokenKind;
begin
  if  Php_KeywordComp(56) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(339) or
        Php_FunctionComp(340) or
        Php_FunctionComp(1222) or
        Php_FunctionComp(1653) or
        Php_FunctionComp(2054) or
        Php_FunctionComp(2231) or
        Php_FunctionComp(3417) or
        Php_FunctionComp(3549) or
        Php_FunctionComp(3775) or
        Php_FunctionComp(4105) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc78: TSynWebTokenKind;
begin
  if  Php_FunctionComp(333) or
      Php_FunctionComp(603) or
      Php_FunctionComp(784) or
      Php_FunctionComp(1473) or
      Php_FunctionComp(1511) or
      Php_FunctionComp(1942) or
      Php_FunctionComp(2052) or
      Php_FunctionComp(2216) or
      Php_FunctionComp(2533) or
      Php_FunctionComp(3201) or
      Php_FunctionComp(3426) or
      Php_FunctionComp(3591) or
      Php_FunctionComp(3957) or
      Php_FunctionComp(4442) or
      Php_FunctionComp(4856) or
      Php_FunctionComp(4860) or
      Php_FunctionComp(5057) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc79: TSynWebTokenKind;
begin
  if  Php_KeywordComp(23) or
      Php_KeywordComp(26) or
      Php_KeywordComp(69) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(191) or
        Php_FunctionComp(293) or
        Php_FunctionComp(356) or
        Php_FunctionComp(414) or
        Php_FunctionComp(432) or
        Php_FunctionComp(626) or
        Php_FunctionComp(633) or
        Php_FunctionComp(709) or
        Php_FunctionComp(1247) or
        Php_FunctionComp(1353) or
        Php_FunctionComp(1503) or
        Php_FunctionComp(1943) or
        Php_FunctionComp(2476) or
        Php_FunctionComp(5168) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc80: TSynWebTokenKind;
begin
  if  Php_KeywordComp(29) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(636) or
        Php_FunctionComp(664) or
        Php_FunctionComp(768) or
        Php_FunctionComp(1061) or
        Php_FunctionComp(1209) or
        Php_FunctionComp(1244) or
        Php_FunctionComp(1354) or
        Php_FunctionComp(3398) or
        Php_FunctionComp(4524) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc81: TSynWebTokenKind;
begin
  if  Php_KeywordComp(47) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(697) or
        Php_FunctionComp(1125) or
        Php_FunctionComp(1251) or
        Php_FunctionComp(1384) or
        Php_FunctionComp(2620) or
        Php_FunctionComp(3084) or
        Php_FunctionComp(3266) or
        Php_FunctionComp(3782) or
        Php_FunctionComp(3924) or
        Php_FunctionComp(4083) or
        Php_FunctionComp(4796) or
        Php_FunctionComp(4841) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc82: TSynWebTokenKind;
begin
  if  Php_KeywordComp(65) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(253) or
        Php_FunctionComp(336) or
        Php_FunctionComp(488) or
        Php_FunctionComp(649) or
        Php_FunctionComp(782) or
        Php_FunctionComp(1237) or
        Php_FunctionComp(1245) or
        Php_FunctionComp(1263) or
        Php_FunctionComp(1457) or
        Php_FunctionComp(1552) or
        Php_FunctionComp(1608) or
        Php_FunctionComp(2096) or
        Php_FunctionComp(2106) or
        Php_FunctionComp(2264) or
        Php_FunctionComp(3117) or
        Php_FunctionComp(3405) or
        Php_FunctionComp(3785) or
        Php_FunctionComp(3853) or
        Php_FunctionComp(3925) or
        Php_FunctionComp(4760) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc83: TSynWebTokenKind;
begin
  if  Php_FunctionComp(158) or
      Php_FunctionComp(416) or
      Php_FunctionComp(690) or
      Php_FunctionComp(710) or
      Php_FunctionComp(783) or
      Php_FunctionComp(1423) or
      Php_FunctionComp(1506) or
      Php_FunctionComp(2086) or
      Php_FunctionComp(2103) or
      Php_FunctionComp(2145) or
      Php_FunctionComp(2146) or
      Php_FunctionComp(2168) or
      Php_FunctionComp(2320) or
      Php_FunctionComp(2424) or
      Php_FunctionComp(2445) or
      Php_FunctionComp(2545) or
      Php_FunctionComp(3068) or
      Php_FunctionComp(3241) or
      Php_FunctionComp(3728) or
      Php_FunctionComp(3731) or
      Php_FunctionComp(4345) or
      Php_FunctionComp(4858) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc84: TSynWebTokenKind;
begin
  if  Php_KeywordComp(5) or
      Php_KeywordComp(66) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(190) or
        Php_FunctionComp(226) or
        Php_FunctionComp(628) or
        Php_FunctionComp(757) or
        Php_FunctionComp(1348) or
        Php_FunctionComp(1370) or
        Php_FunctionComp(1720) or
        Php_FunctionComp(2149) or
        Php_FunctionComp(2157) or
        Php_FunctionComp(3069) or
        Php_FunctionComp(3124) or
        Php_FunctionComp(3267) or
        Php_FunctionComp(3318) or
        Php_FunctionComp(3532) or
        Php_FunctionComp(3579) or
        Php_FunctionComp(3637) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc85: TSynWebTokenKind;
begin
  if  Php_FunctionComp(370) or
      Php_FunctionComp(659) or
      Php_FunctionComp(689) or
      Php_FunctionComp(1346) or
      Php_FunctionComp(1349) or
      Php_FunctionComp(1369) or
      Php_FunctionComp(1469) or
      Php_FunctionComp(1585) or
      Php_FunctionComp(1789) or
      Php_FunctionComp(1955) or
      Php_FunctionComp(1989) or
      Php_FunctionComp(2066) or
      Php_FunctionComp(2212) or
      Php_FunctionComp(3581) or
      Php_FunctionComp(3749) or
      Php_FunctionComp(4292) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc86: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1204) or
      Php_FunctionComp(1347) or
      Php_FunctionComp(1388) or
      Php_FunctionComp(1502) or
      Php_FunctionComp(1737) or
      Php_FunctionComp(2011) or
      Php_FunctionComp(2014) or
      Php_FunctionComp(2175) or
      Php_FunctionComp(2234) or
      Php_FunctionComp(3139) or
      Php_FunctionComp(3184) or
      Php_FunctionComp(3200) or
      Php_FunctionComp(3759) or
      Php_FunctionComp(3941) or
      Php_FunctionComp(4450) or
      Php_FunctionComp(5150) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc87: TSynWebTokenKind;
begin
  if  Php_KeywordComp(64) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(1885) or
        Php_FunctionComp(1971) or
        Php_FunctionComp(2101) or
        Php_FunctionComp(2185) or
        Php_FunctionComp(2219) or
        Php_FunctionComp(2288) or
        Php_FunctionComp(3613) or
        Php_FunctionComp(3760) or
        Php_FunctionComp(3904) or
        Php_FunctionComp(3984) or
        Php_FunctionComp(4119) or
        Php_FunctionComp(4845) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc88: TSynWebTokenKind;
begin
  if  Php_FunctionComp(164) or
      Php_FunctionComp(207) or
      Php_FunctionComp(371) or
      Php_FunctionComp(548) or
      Php_FunctionComp(678) or
      Php_FunctionComp(787) or
      Php_FunctionComp(1443) or
      Php_FunctionComp(1451) or
      Php_FunctionComp(1514) or
      Php_FunctionComp(2151) or
      Php_FunctionComp(2190) or
      Php_FunctionComp(2217) or
      Php_FunctionComp(2347) or
      Php_FunctionComp(2350) or
      Php_FunctionComp(3123) or
      Php_FunctionComp(3176) or
      Php_FunctionComp(3177) or
      Php_FunctionComp(3300) or
      Php_FunctionComp(3526) or
      Php_FunctionComp(3597) or
      Php_FunctionComp(3622) or
      Php_FunctionComp(4003) or
      Php_FunctionComp(4496) or
      Php_FunctionComp(4602) or
      Php_FunctionComp(4762) or
      Php_FunctionComp(4861) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc89: TSynWebTokenKind;
begin
  if  Php_FunctionComp(639) or
      Php_FunctionComp(1060) or
      Php_FunctionComp(1137) or
      Php_FunctionComp(1266) or
      Php_FunctionComp(1274) or
      Php_FunctionComp(1387) or
      Php_FunctionComp(1445) or
      Php_FunctionComp(1857) or
      Php_FunctionComp(1948) or
      Php_FunctionComp(1988) or
      Php_FunctionComp(2043) or
      Php_FunctionComp(2072) or
      Php_FunctionComp(2183) or
      Php_FunctionComp(2683) or
      Php_FunctionComp(3358) or
      Php_FunctionComp(3380) or
      Php_FunctionComp(3751) or
      Php_FunctionComp(4116) or
      Php_FunctionComp(4117) or
      Php_FunctionComp(4451) or
      Php_FunctionComp(5135) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc90: TSynWebTokenKind;
begin
  if  Php_FunctionComp(581) or
      Php_FunctionComp(647) or
      Php_FunctionComp(1205) or
      Php_FunctionComp(1364) or
      Php_FunctionComp(1419) or
      Php_FunctionComp(1462) or
      Php_FunctionComp(1509) or
      Php_FunctionComp(1675) or
      Php_FunctionComp(1723) or
      Php_FunctionComp(1733) or
      Php_FunctionComp(1961) or
      Php_FunctionComp(2200) or
      Php_FunctionComp(2202) or
      Php_FunctionComp(2358) or
      Php_FunctionComp(2541) or
      Php_FunctionComp(3178) or
      Php_FunctionComp(3563) or
      Php_FunctionComp(3593) or
      Php_FunctionComp(3625) or
      Php_FunctionComp(3956) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc91: TSynWebTokenKind;
begin
  if  Php_KeywordComp(33) or
      Php_KeywordComp(57) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(249) or
        Php_FunctionComp(291) or
        Php_FunctionComp(504) or
        Php_FunctionComp(1063) or
        Php_FunctionComp(1128) or
        Php_FunctionComp(1258) or
        Php_FunctionComp(1436) or
        Php_FunctionComp(1651) or
        Php_FunctionComp(2077) or
        Php_FunctionComp(2089) or
        Php_FunctionComp(2095) or
        Php_FunctionComp(2420) or
        Php_FunctionComp(2432) or
        Php_FunctionComp(3217) or
        Php_FunctionComp(3529) or
        Php_FunctionComp(3640) or
        Php_FunctionComp(3689) or
        Php_FunctionComp(3756) or
        Php_FunctionComp(3844) or
        Php_FunctionComp(3901) or
        Php_FunctionComp(4114) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc92: TSynWebTokenKind;
begin
  if  Php_FunctionComp(99) or
      Php_FunctionComp(558) or
      Php_FunctionComp(679) or
      Php_FunctionComp(1224) or
      Php_FunctionComp(1248) or
      Php_FunctionComp(1359) or
      Php_FunctionComp(1398) or
      Php_FunctionComp(2078) or
      Php_FunctionComp(2126) or
      Php_FunctionComp(2158) or
      Php_FunctionComp(2431) or
      Php_FunctionComp(2528) or
      Php_FunctionComp(3172) or
      Php_FunctionComp(3325) or
      Php_FunctionComp(3392) or
      Php_FunctionComp(3528) or
      Php_FunctionComp(3571) or
      Php_FunctionComp(3604) or
      Php_FunctionComp(3688) or
      Php_FunctionComp(3940) or
      Php_FunctionComp(4059) or
      Php_FunctionComp(4118) or
      Php_FunctionComp(4371) or
      Php_FunctionComp(4515) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc93: TSynWebTokenKind;
begin
  if  Php_KeywordComp(60) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(222) or
        Php_FunctionComp(642) or
        Php_FunctionComp(1210) or
        Php_FunctionComp(1352) or
        Php_FunctionComp(1474) or
        Php_FunctionComp(1513) or
        Php_FunctionComp(1796) or
        Php_FunctionComp(1959) or
        Php_FunctionComp(1978) or
        Php_FunctionComp(1997) or
        Php_FunctionComp(2173) or
        Php_FunctionComp(3218) or
        Php_FunctionComp(3319) or
        Php_FunctionComp(3461) or
        Php_FunctionComp(3813) or
        Php_FunctionComp(3909) or
        Php_FunctionComp(4081) or
        Php_FunctionComp(4082) or
        Php_FunctionComp(4101) or
        Php_FunctionComp(4123) or
        Php_FunctionComp(4857) or
        Php_FunctionComp(4859) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc94: TSynWebTokenKind;
begin
  if  Php_FunctionComp(476) or
      Php_FunctionComp(1546) or
      Php_FunctionComp(1880) or
      Php_FunctionComp(2269) or
      Php_FunctionComp(3098) or
      Php_FunctionComp(3430) or
      Php_FunctionComp(3634) or
      Php_FunctionComp(3841) or
      Php_FunctionComp(4230) or
      Php_FunctionComp(4235) or
      Php_FunctionComp(4240) or
      Php_FunctionComp(4804) or
      Php_FunctionComp(4875) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc95: TSynWebTokenKind;
begin
  if  Php_FunctionComp(96) or
      Php_FunctionComp(1092) or
      Php_FunctionComp(1375) or
      Php_FunctionComp(1634) or
      Php_FunctionComp(1739) or
      Php_FunctionComp(1952) or
      Php_FunctionComp(2004) or
      Php_FunctionComp(2132) or
      Php_FunctionComp(2409) or
      Php_FunctionComp(2477) or
      Php_FunctionComp(2552) or
      Php_FunctionComp(3080) or
      Php_FunctionComp(3314) or
      Php_FunctionComp(3696) or
      Php_FunctionComp(4096) or
      Php_FunctionComp(4205) or
      Php_FunctionComp(4514) or
      Php_FunctionComp(4872) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc96: TSynWebTokenKind;
begin
  if  Php_KeywordComp(62) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(395) or
        Php_FunctionComp(658) or
        Php_FunctionComp(1260) or
        Php_FunctionComp(1350) or
        Php_FunctionComp(1362) or
        Php_FunctionComp(1548) or
        Php_FunctionComp(1647) or
        Php_FunctionComp(2348) or
        Php_FunctionComp(3104) or
        Php_FunctionComp(3127) or
        Php_FunctionComp(3424) or
        Php_FunctionComp(3437) or
        Php_FunctionComp(3937) or
        Php_FunctionComp(4122) or
        Php_FunctionComp(4805) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc97: TSynWebTokenKind;
begin
  if  Php_FunctionComp(196) or
      Php_FunctionComp(197) or
      Php_FunctionComp(1360) or
      Php_FunctionComp(2068) or
      Php_FunctionComp(2191) or
      Php_FunctionComp(2224) or
      Php_FunctionComp(2555) or
      Php_FunctionComp(3022) or
      Php_FunctionComp(3205) or
      Php_FunctionComp(3315) or
      Php_FunctionComp(3550) or
      Php_FunctionComp(3650) or
      Php_FunctionComp(3805) or
      Php_FunctionComp(3870) or
      Php_FunctionComp(3903) or
      Php_FunctionComp(3954) or
      Php_FunctionComp(3985) or
      Php_FunctionComp(4094) or
      Php_FunctionComp(4129) or
      Php_FunctionComp(4753) or
      Php_FunctionComp(4763) or
      Php_FunctionComp(4846) or
      Php_FunctionComp(5133) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc98: TSynWebTokenKind;
begin
  if  Php_FunctionComp(318) or
      Php_FunctionComp(330) or
      Php_FunctionComp(480) or
      Php_FunctionComp(1058) or
      Php_FunctionComp(1118) or
      Php_FunctionComp(1259) or
      Php_FunctionComp(1372) or
      Php_FunctionComp(1433) or
      Php_FunctionComp(1689) or
      Php_FunctionComp(1728) or
      Php_FunctionComp(2313) or
      Php_FunctionComp(3206) or
      Php_FunctionComp(3209) or
      Php_FunctionComp(3236) or
      Php_FunctionComp(3468) or
      Php_FunctionComp(3477) or
      Php_FunctionComp(3739) or
      Php_FunctionComp(3840) or
      Php_FunctionComp(4379) or
      Php_FunctionComp(4798) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc99: TSynWebTokenKind;
begin
  if  Php_FunctionComp(134) or
      Php_FunctionComp(517) or
      Php_FunctionComp(1089) or
      Php_FunctionComp(1211) or
      Php_FunctionComp(1358) or
      Php_FunctionComp(1368) or
      Php_FunctionComp(1421) or
      Php_FunctionComp(1449) or
      Php_FunctionComp(1649) or
      Php_FunctionComp(1729) or
      Php_FunctionComp(1975) or
      Php_FunctionComp(1977) or
      Php_FunctionComp(1999) or
      Php_FunctionComp(3594) or
      Php_FunctionComp(4091) or
      Php_FunctionComp(4125) or
      Php_FunctionComp(4452) or
      Php_FunctionComp(4516) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc100: TSynWebTokenKind;
begin
  if  Php_FunctionComp(230) or
      Php_FunctionComp(1101) or
      Php_FunctionComp(1264) or
      Php_FunctionComp(1367) or
      Php_FunctionComp(1440) or
      Php_FunctionComp(1505) or
      Php_FunctionComp(2085) or
      Php_FunctionComp(3092) or
      Php_FunctionComp(3164) or
      Php_FunctionComp(3220) or
      Php_FunctionComp(3590) or
      Php_FunctionComp(3808) or
      Php_FunctionComp(3812) or
      Php_FunctionComp(4767) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc101: TSynWebTokenKind;
begin
  if  Php_KeywordComp(16) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(553) or
        Php_FunctionComp(663) or
        Php_FunctionComp(758) or
        Php_FunctionComp(1323) or
        Php_FunctionComp(1376) or
        Php_FunctionComp(1431) or
        Php_FunctionComp(1465) or
        Php_FunctionComp(1682) or
        Php_FunctionComp(1811) or
        Php_FunctionComp(2144) or
        Php_FunctionComp(3093) or
        Php_FunctionComp(3095) or
        Php_FunctionComp(3143) or
        Php_FunctionComp(3166) or
        Php_FunctionComp(3188) or
        Php_FunctionComp(3458) or
        Php_FunctionComp(3641) or
        Php_FunctionComp(3746) or
        Php_FunctionComp(3807) or
        Php_FunctionComp(3951) or
        Php_FunctionComp(4264) or
        Php_FunctionComp(4351) or
        Php_FunctionComp(4754) or
        Php_FunctionComp(5137) or
        Php_FunctionComp(5152) or
        Php_FunctionComp(5167) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc102: TSynWebTokenKind;
begin
  if  Php_KeywordComp(2) or
      Php_KeywordComp(39) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(212) or
        Php_FunctionComp(331) or
        Php_FunctionComp(550) or
        Php_FunctionComp(765) or
        Php_FunctionComp(1380) or
        Php_FunctionComp(1810) or
        Php_FunctionComp(1862) or
        Php_FunctionComp(2201) or
        Php_FunctionComp(2253) or
        Php_FunctionComp(2479) or
        Php_FunctionComp(2543) or
        Php_FunctionComp(3361) or
        Php_FunctionComp(3510) or
        Php_FunctionComp(3587) or
        Php_FunctionComp(3605) or
        Php_FunctionComp(3732) or
        Php_FunctionComp(3938) or
        Php_FunctionComp(4023) or
        Php_FunctionComp(4058) or
        Php_FunctionComp(4246) or
        Php_FunctionComp(4283) or
        Php_FunctionComp(4297) or
        Php_FunctionComp(4368) or
        Php_FunctionComp(4505) or
        Php_FunctionComp(4867) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc103: TSynWebTokenKind;
begin
  if  Php_FunctionComp(351) or
      Php_FunctionComp(632) or
      Php_FunctionComp(643) or
      Php_FunctionComp(675) or
      Php_FunctionComp(688) or
      Php_FunctionComp(769) or
      Php_FunctionComp(790) or
      Php_FunctionComp(1250) or
      Php_FunctionComp(1336) or
      Php_FunctionComp(1427) or
      Php_FunctionComp(1463) or
      Php_FunctionComp(1515) or
      Php_FunctionComp(1688) or
      Php_FunctionComp(1791) or
      Php_FunctionComp(1808) or
      Php_FunctionComp(2076) or
      Php_FunctionComp(2119) or
      Php_FunctionComp(2147) or
      Php_FunctionComp(2160) or
      Php_FunctionComp(2435) or
      Php_FunctionComp(2484) or
      Php_FunctionComp(3025) or
      Php_FunctionComp(3106) or
      Php_FunctionComp(3126) or
      Php_FunctionComp(3190) or
      Php_FunctionComp(3202) or
      Php_FunctionComp(3230) or
      Php_FunctionComp(3585) or
      Php_FunctionComp(3592) or
      Php_FunctionComp(3763) or
      Php_FunctionComp(4500) or
      Php_FunctionComp(4510) or
      Php_FunctionComp(4752) or
      Php_FunctionComp(4806) or
      Php_FunctionComp(4902) or
      Php_FunctionComp(4999) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc104: TSynWebTokenKind;
begin
  if  Php_FunctionComp(429) or
      Php_FunctionComp(582) or
      Php_FunctionComp(644) or
      Php_FunctionComp(706) or
      Php_FunctionComp(711) or
      Php_FunctionComp(759) or
      Php_FunctionComp(760) or
      Php_FunctionComp(1094) or
      Php_FunctionComp(1262) or
      Php_FunctionComp(1297) or
      Php_FunctionComp(1342) or
      Php_FunctionComp(1373) or
      Php_FunctionComp(1422) or
      Php_FunctionComp(1583) or
      Php_FunctionComp(1740) or
      Php_FunctionComp(1807) or
      Php_FunctionComp(1909) or
      Php_FunctionComp(2154) or
      Php_FunctionComp(2375) or
      Php_FunctionComp(2411) or
      Php_FunctionComp(3005) or
      Php_FunctionComp(3118) or
      Php_FunctionComp(3182) or
      Php_FunctionComp(3422) or
      Php_FunctionComp(3586) or
      Php_FunctionComp(3948) or
      Php_FunctionComp(4025) or
      Php_FunctionComp(4501) or
      Php_FunctionComp(4504) or
      Php_FunctionComp(4837) or
      Php_FunctionComp(4876) or
      Php_FunctionComp(4953) or
      Php_FunctionComp(5151) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc105: TSynWebTokenKind;
begin
  if  Php_KeywordComp(12) or
      Php_KeywordComp(28) or
      Php_KeywordComp(44) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(464) or
        Php_FunctionComp(641) or
        Php_FunctionComp(781) or
        Php_FunctionComp(1122) or
        Php_FunctionComp(1129) or
        Php_FunctionComp(1225) or
        Php_FunctionComp(1355) or
        Php_FunctionComp(1418) or
        Php_FunctionComp(1586) or
        Php_FunctionComp(1602) or
        Php_FunctionComp(1725) or
        Php_FunctionComp(1901) or
        Php_FunctionComp(1906) or
        Php_FunctionComp(2188) or
        Php_FunctionComp(2488) or
        Php_FunctionComp(2539) or
        Php_FunctionComp(2562) or
        Php_FunctionComp(3073) or
        Php_FunctionComp(3129) or
        Php_FunctionComp(3473) or
        Php_FunctionComp(3483) or
        Php_FunctionComp(3559) or
        Php_FunctionComp(3624) or
        Php_FunctionComp(4920) or
        Php_FunctionComp(5118) or
        Php_FunctionComp(5123) or
        Php_FunctionComp(5142) or
        Php_FunctionComp(5159) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc106: TSynWebTokenKind;
begin
  if  Php_KeywordComp(45) or
      Php_KeywordComp(58) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(214) or
        Php_FunctionComp(403) or
        Php_FunctionComp(506) or
        Php_FunctionComp(651) or
        Php_FunctionComp(798) or
        Php_FunctionComp(1444) or
        Php_FunctionComp(1455) or
        Php_FunctionComp(2485) or
        Php_FunctionComp(2618) or
        Php_FunctionComp(3066) or
        Php_FunctionComp(3186) or
        Php_FunctionComp(3562) or
        Php_FunctionComp(3692) or
        Php_FunctionComp(3729) or
        Php_FunctionComp(4131) or
        Php_FunctionComp(4233) or
        Php_FunctionComp(4238) or
        Php_FunctionComp(4243) or
        Php_FunctionComp(4298) or
        Php_FunctionComp(4508) or
        Php_FunctionComp(4960) or
        Php_FunctionComp(5121) or
        Php_FunctionComp(5138) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc107: TSynWebTokenKind;
begin
  if  Php_FunctionComp(162) or
      Php_FunctionComp(386) or
      Php_FunctionComp(490) or
      Php_FunctionComp(551) or
      Php_FunctionComp(717) or
      Php_FunctionComp(1249) or
      Php_FunctionComp(1275) or
      Php_FunctionComp(1426) or
      Php_FunctionComp(1529) or
      Php_FunctionComp(1600) or
      Php_FunctionComp(1652) or
      Php_FunctionComp(1783) or
      Php_FunctionComp(1985) or
      Php_FunctionComp(2055) or
      Php_FunctionComp(2155) or
      Php_FunctionComp(2161) or
      Php_FunctionComp(2265) or
      Php_FunctionComp(2273) or
      Php_FunctionComp(2448) or
      Php_FunctionComp(2833) or
      Php_FunctionComp(3007) or
      Php_FunctionComp(3029) or
      Php_FunctionComp(3154) or
      Php_FunctionComp(3212) or
      Php_FunctionComp(3304) or
      Php_FunctionComp(3381) or
      Php_FunctionComp(3572) or
      Php_FunctionComp(3781) or
      Php_FunctionComp(3952) or
      Php_FunctionComp(4358) or
      Php_FunctionComp(4502) or
      Php_FunctionComp(4812) or
      Php_FunctionComp(4863) or
      Php_FunctionComp(4874) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc108: TSynWebTokenKind;
begin
  if  Php_FunctionComp(440) or
      Php_FunctionComp(500) or
      Php_FunctionComp(650) or
      Php_FunctionComp(766) or
      Php_FunctionComp(1095) or
      Php_FunctionComp(1105) or
      Php_FunctionComp(1261) or
      Php_FunctionComp(1379) or
      Php_FunctionComp(1430) or
      Php_FunctionComp(1721) or
      Php_FunctionComp(1769) or
      Php_FunctionComp(1954) or
      Php_FunctionComp(3219) or
      Php_FunctionComp(3308) or
      Php_FunctionComp(3321) or
      Php_FunctionComp(3467) or
      Php_FunctionComp(3582) or
      Php_FunctionComp(3846) or
      Php_FunctionComp(4778) or
      Php_FunctionComp(4818) or
      Php_FunctionComp(4981) or
      Php_FunctionComp(5058) or
      Php_FunctionComp(5119) or
      Php_FunctionComp(5130) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc109: TSynWebTokenKind;
begin
  if  Php_FunctionComp(238) or
      Php_FunctionComp(656) or
      Php_FunctionComp(748) or
      Php_FunctionComp(939) or
      Php_FunctionComp(1152) or
      Php_FunctionComp(1213) or
      Php_FunctionComp(1246) or
      Php_FunctionComp(1683) or
      Php_FunctionComp(1734) or
      Php_FunctionComp(1792) or
      Php_FunctionComp(2117) or
      Php_FunctionComp(2250) or
      Php_FunctionComp(2258) or
      Php_FunctionComp(2272) or
      Php_FunctionComp(2532) or
      Php_FunctionComp(2651) or
      Php_FunctionComp(2685) or
      Php_FunctionComp(3400) or
      Php_FunctionComp(3512) or
      Php_FunctionComp(3607) or
      Php_FunctionComp(4217) or
      Php_FunctionComp(4234) or
      Php_FunctionComp(4239) or
      Php_FunctionComp(4244) or
      Php_FunctionComp(4453) or
      Php_FunctionComp(4878) or
      Php_FunctionComp(4922) or
      Php_FunctionComp(5084) or
      Php_FunctionComp(5107) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc110: TSynWebTokenKind;
begin
  if  Php_FunctionComp(227) or
      Php_FunctionComp(247) or
      Php_FunctionComp(434) or
      Php_FunctionComp(1017) or
      Php_FunctionComp(1109) or
      Php_FunctionComp(1145) or
      Php_FunctionComp(1450) or
      Php_FunctionComp(1470) or
      Php_FunctionComp(1659) or
      Php_FunctionComp(1678) or
      Php_FunctionComp(1736) or
      Php_FunctionComp(1762) or
      Php_FunctionComp(2319) or
      Php_FunctionComp(2410) or
      Php_FunctionComp(3096) or
      Php_FunctionComp(3115) or
      Php_FunctionComp(3149) or
      Php_FunctionComp(3167) or
      Php_FunctionComp(3351) or
      Php_FunctionComp(3360) or
      Php_FunctionComp(3390) or
      Php_FunctionComp(3425) or
      Php_FunctionComp(3772) or
      Php_FunctionComp(3823) or
      Php_FunctionComp(4061) or
      Php_FunctionComp(4445) or
      Php_FunctionComp(4490) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc111: TSynWebTokenKind;
begin
  if  Php_KeywordComp(31) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(126) or
        Php_FunctionComp(223) or
        Php_FunctionComp(235) or
        Php_FunctionComp(425) or
        Php_FunctionComp(575) or
        Php_FunctionComp(640) or
        Php_FunctionComp(682) or
        Php_FunctionComp(1228) or
        Php_FunctionComp(1468) or
        Php_FunctionComp(1731) or
        Php_FunctionComp(2001) or
        Php_FunctionComp(2087) or
        Php_FunctionComp(2102) or
        Php_FunctionComp(2213) or
        Php_FunctionComp(2240) or
        Php_FunctionComp(2551) or
        Php_FunctionComp(3038) or
        Php_FunctionComp(3383) or
        Php_FunctionComp(3386) or
        Php_FunctionComp(3469) or
        Php_FunctionComp(3795) or
        Php_FunctionComp(4021) or
        Php_FunctionComp(4758) or
        Php_FunctionComp(4891) or
        Php_FunctionComp(4901) or
        Php_FunctionComp(4923) or
        Php_FunctionComp(5146) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc112: TSynWebTokenKind;
begin
  if  Php_FunctionComp(388) or
      Php_FunctionComp(465) or
      Php_FunctionComp(654) or
      Php_FunctionComp(1691) or
      Php_FunctionComp(1693) or
      Php_FunctionComp(1992) or
      Php_FunctionComp(2047) or
      Php_FunctionComp(3067) or
      Php_FunctionComp(3075) or
      Php_FunctionComp(3114) or
      Php_FunctionComp(3856) or
      Php_FunctionComp(3917) or
      Php_FunctionComp(4128) or
      Php_FunctionComp(4387) or
      Php_FunctionComp(4819) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc113: TSynWebTokenKind;
begin
  if  Php_FunctionComp(319) or
      Php_FunctionComp(497) or
      Php_FunctionComp(629) or
      Php_FunctionComp(1797) or
      Php_FunctionComp(1802) or
      Php_FunctionComp(1899) or
      Php_FunctionComp(1924) or
      Php_FunctionComp(2015) or
      Php_FunctionComp(2016) or
      Php_FunctionComp(2082) or
      Php_FunctionComp(2184) or
      Php_FunctionComp(2395) or
      Php_FunctionComp(2529) or
      Php_FunctionComp(3320) or
      Php_FunctionComp(3445) or
      Php_FunctionComp(3459) or
      Php_FunctionComp(3475) or
      Php_FunctionComp(3602) or
      Php_FunctionComp(4032) or
      Php_FunctionComp(4084) or
      Php_FunctionComp(4130) or
      Php_FunctionComp(5147) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc114: TSynWebTokenKind;
begin
  if  Php_FunctionComp(314) or
      Php_FunctionComp(524) or
      Php_FunctionComp(578) or
      Php_FunctionComp(1150) or
      Php_FunctionComp(1452) or
      Php_FunctionComp(1497) or
      Php_FunctionComp(1933) or
      Php_FunctionComp(1951) or
      Php_FunctionComp(2062) or
      Php_FunctionComp(2097) or
      Php_FunctionComp(2228) or
      Php_FunctionComp(2327) or
      Php_FunctionComp(2368) or
      Php_FunctionComp(2442) or
      Php_FunctionComp(2530) or
      Php_FunctionComp(3229) or
      Php_FunctionComp(3410) or
      Php_FunctionComp(3436) or
      Php_FunctionComp(3471) or
      Php_FunctionComp(3494) or
      Php_FunctionComp(3534) or
      Php_FunctionComp(3784) or
      Php_FunctionComp(3842) or
      Php_FunctionComp(4143) or
      Php_FunctionComp(4225) or
      Php_FunctionComp(4509) or
      Php_FunctionComp(4823) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc115: TSynWebTokenKind;
begin
  if  Php_FunctionComp(142) or
      Php_FunctionComp(530) or
      Php_FunctionComp(703) or
      Php_FunctionComp(1156) or
      Php_FunctionComp(1184) or
      Php_FunctionComp(1365) or
      Php_FunctionComp(1521) or
      Php_FunctionComp(1633) or
      Php_FunctionComp(1759) or
      Php_FunctionComp(2107) or
      Php_FunctionComp(2229) or
      Php_FunctionComp(2624) or
      Php_FunctionComp(3004) or
      Php_FunctionComp(3039) or
      Php_FunctionComp(3071) or
      Php_FunctionComp(3072) or
      Php_FunctionComp(3116) or
      Php_FunctionComp(3128) or
      Php_FunctionComp(3362) or
      Php_FunctionComp(3399) or
      Php_FunctionComp(4024) or
      Php_FunctionComp(4270) or
      Php_FunctionComp(4955) or
      Php_FunctionComp(5134) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc116: TSynWebTokenKind;
begin
  if  Php_FunctionComp(141) or
      Php_FunctionComp(237) or
      Php_FunctionComp(431) or
      Php_FunctionComp(583) or
      Php_FunctionComp(712) or
      Php_FunctionComp(764) or
      Php_FunctionComp(1185) or
      Php_FunctionComp(1331) or
      Php_FunctionComp(1351) or
      Php_FunctionComp(1366) or
      Php_FunctionComp(1417) or
      Php_FunctionComp(1667) or
      Php_FunctionComp(1741) or
      Php_FunctionComp(1746) or
      Php_FunctionComp(1897) or
      Php_FunctionComp(2064) or
      Php_FunctionComp(2122) or
      Php_FunctionComp(2127) or
      Php_FunctionComp(2433) or
      Php_FunctionComp(3082) or
      Php_FunctionComp(3147) or
      Php_FunctionComp(3350) or
      Php_FunctionComp(3442) or
      Php_FunctionComp(3463) or
      Php_FunctionComp(3690) or
      Php_FunctionComp(3740) or
      Php_FunctionComp(3847) or
      Php_FunctionComp(4372) or
      Php_FunctionComp(4493) or
      Php_FunctionComp(5080) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc117: TSynWebTokenKind;
begin
  if  Php_FunctionComp(75) or
      Php_FunctionComp(108) or
      Php_FunctionComp(233) or
      Php_FunctionComp(479) or
      Php_FunctionComp(536) or
      Php_FunctionComp(1059) or
      Php_FunctionComp(1200) or
      Php_FunctionComp(1409) or
      Php_FunctionComp(1420) or
      Php_FunctionComp(1491) or
      Php_FunctionComp(1543) or
      Php_FunctionComp(1547) or
      Php_FunctionComp(1710) or
      Php_FunctionComp(1732) or
      Php_FunctionComp(2118) or
      Php_FunctionComp(2123) or
      Php_FunctionComp(2247) or
      Php_FunctionComp(2315) or
      Php_FunctionComp(2524) or
      Php_FunctionComp(2544) or
      Php_FunctionComp(3141) or
      Php_FunctionComp(3423) or
      Php_FunctionComp(3441) or
      Php_FunctionComp(3476) or
      Php_FunctionComp(3556) or
      Php_FunctionComp(3777) or
      Php_FunctionComp(3872) or
      Php_FunctionComp(4294) or
      Php_FunctionComp(4444) or
      Php_FunctionComp(4449) or
      Php_FunctionComp(4840) or
      Php_FunctionComp(4853) or
      Php_FunctionComp(4879) or
      Php_FunctionComp(4889) or
      Php_FunctionComp(5149) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc118: TSynWebTokenKind;
begin
  if  Php_FunctionComp(492) or
      Php_FunctionComp(725) or
      Php_FunctionComp(1104) or
      Php_FunctionComp(1279) or
      Php_FunctionComp(1405) or
      Php_FunctionComp(1520) or
      Php_FunctionComp(1613) or
      Php_FunctionComp(1818) or
      Php_FunctionComp(2110) or
      Php_FunctionComp(2316) or
      Php_FunctionComp(2328) or
      Php_FunctionComp(2364) or
      Php_FunctionComp(2519) or
      Php_FunctionComp(2523) or
      Php_FunctionComp(2534) or
      Php_FunctionComp(3207) or
      Php_FunctionComp(3366) or
      Php_FunctionComp(3388) or
      Php_FunctionComp(3574) or
      Php_FunctionComp(3631) or
      Php_FunctionComp(3635) or
      Php_FunctionComp(3796) or
      Php_FunctionComp(4086) or
      Php_FunctionComp(4120) or
      Php_FunctionComp(4127) or
      Php_FunctionComp(4893) or
      Php_FunctionComp(4952) or
      Php_FunctionComp(5090) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc119: TSynWebTokenKind;
begin
  if  Php_FunctionComp(189) or
      Php_FunctionComp(231) or
      Php_FunctionComp(313) or
      Php_FunctionComp(368) or
      Php_FunctionComp(430) or
      Php_FunctionComp(493) or
      Php_FunctionComp(796) or
      Php_FunctionComp(955) or
      Php_FunctionComp(1208) or
      Php_FunctionComp(1727) or
      Php_FunctionComp(1742) or
      Php_FunctionComp(1788) or
      Php_FunctionComp(1865) or
      Php_FunctionComp(2050) or
      Php_FunctionComp(2241) or
      Php_FunctionComp(2419) or
      Php_FunctionComp(2626) or
      Php_FunctionComp(2834) or
      Php_FunctionComp(3016) or
      Php_FunctionComp(3040) or
      Php_FunctionComp(3234) or
      Php_FunctionComp(3438) or
      Php_FunctionComp(3455) or
      Php_FunctionComp(4100) or
      Php_FunctionComp(4211) or
      Php_FunctionComp(4309) or
      Php_FunctionComp(4605) or
      Php_FunctionComp(4821) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc120: TSynWebTokenKind;
begin
  if  Php_FunctionComp(204) or
      Php_FunctionComp(604) or
      Php_FunctionComp(677) or
      Php_FunctionComp(684) or
      Php_FunctionComp(1519) or
      Php_FunctionComp(1526) or
      Php_FunctionComp(1718) or
      Php_FunctionComp(1932) or
      Php_FunctionComp(1934) or
      Php_FunctionComp(1947) or
      Php_FunctionComp(2338) or
      Php_FunctionComp(2629) or
      Php_FunctionComp(2686) or
      Php_FunctionComp(3379) or
      Php_FunctionComp(3545) or
      Php_FunctionComp(3554) or
      Php_FunctionComp(3567) or
      Php_FunctionComp(3606) or
      Php_FunctionComp(3742) or
      Php_FunctionComp(3745) or
      Php_FunctionComp(3778) or
      Php_FunctionComp(4011) or
      Php_FunctionComp(4344) or
      Php_FunctionComp(4374) or
      Php_FunctionComp(4503) or
      Php_FunctionComp(4567) or
      Php_FunctionComp(4577) or
      Php_FunctionComp(4882) or
      Php_FunctionComp(5060) or
      Php_FunctionComp(5081) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc121: TSynWebTokenKind;
begin
  if  Php_FunctionComp(130) or
      Php_FunctionComp(280) or
      Php_FunctionComp(422) or
      Php_FunctionComp(596) or
      Php_FunctionComp(793) or
      Php_FunctionComp(1232) or
      Php_FunctionComp(1363) or
      Php_FunctionComp(1381) or
      Php_FunctionComp(1646) or
      Php_FunctionComp(1726) or
      Php_FunctionComp(1800) or
      Php_FunctionComp(1859) or
      Php_FunctionComp(2469) or
      Php_FunctionComp(2535) or
      Php_FunctionComp(3204) or
      Php_FunctionComp(3215) or
      Php_FunctionComp(3376) or
      Php_FunctionComp(3418) or
      Php_FunctionComp(3478) or
      Php_FunctionComp(3514) or
      Php_FunctionComp(3569) or
      Php_FunctionComp(3817) or
      Php_FunctionComp(3850) or
      Php_FunctionComp(3932) or
      Php_FunctionComp(4090) or
      Php_FunctionComp(4245) or
      Php_FunctionComp(4266) or
      Php_FunctionComp(4669) or
      Php_FunctionComp(4868) or
      Php_FunctionComp(5086) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc122: TSynWebTokenKind;
begin
  if  Php_FunctionComp(199) or
      Php_FunctionComp(409) or
      Php_FunctionComp(438) or
      Php_FunctionComp(485) or
      Php_FunctionComp(747) or
      Php_FunctionComp(767) or
      Php_FunctionComp(1151) or
      Php_FunctionComp(1760) or
      Php_FunctionComp(1768) or
      Php_FunctionComp(1799) or
      Php_FunctionComp(1915) or
      Php_FunctionComp(1920) or
      Php_FunctionComp(1940) or
      Php_FunctionComp(1967) or
      Php_FunctionComp(2009) or
      Php_FunctionComp(2331) or
      Php_FunctionComp(2436) or
      Php_FunctionComp(2525) or
      Php_FunctionComp(2558) or
      Php_FunctionComp(2574) or
      Php_FunctionComp(2585) or
      Php_FunctionComp(2868) or
      Php_FunctionComp(3107) or
      Php_FunctionComp(3120) or
      Php_FunctionComp(3142) or
      Php_FunctionComp(3187) or
      Php_FunctionComp(3216) or
      Php_FunctionComp(3357) or
      Php_FunctionComp(3413) or
      Php_FunctionComp(3501) or
      Php_FunctionComp(3566) or
      Php_FunctionComp(3694) or
      Php_FunctionComp(3834) or
      Php_FunctionComp(4089) or
      Php_FunctionComp(4115) or
      Php_FunctionComp(4142) or
      Php_FunctionComp(4331) or
      Php_FunctionComp(4443) or
      Php_FunctionComp(4555) or
      Php_FunctionComp(4586) or
      Php_FunctionComp(4797) or
      Php_FunctionComp(4811) or
      Php_FunctionComp(5000) or
      Php_FunctionComp(5076) or
      Php_FunctionComp(5125) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc123: TSynWebTokenKind;
begin
  if  Php_FunctionComp(221) or
      Php_FunctionComp(594) or
      Php_FunctionComp(707) or
      Php_FunctionComp(1066) or
      Php_FunctionComp(1166) or
      Php_FunctionComp(1607) or
      Php_FunctionComp(1630) or
      Php_FunctionComp(1668) or
      Php_FunctionComp(1751) or
      Php_FunctionComp(1812) or
      Php_FunctionComp(2121) or
      Php_FunctionComp(2124) or
      Php_FunctionComp(2359) or
      Php_FunctionComp(2480) or
      Php_FunctionComp(2845) or
      Php_FunctionComp(3349) or
      Php_FunctionComp(3541) or
      Php_FunctionComp(3766) or
      Php_FunctionComp(3790) or
      Php_FunctionComp(3816) or
      Php_FunctionComp(3869) or
      Php_FunctionComp(3928) or
      Php_FunctionComp(3931) or
      Php_FunctionComp(3953) or
      Php_FunctionComp(4067) or
      Php_FunctionComp(4144) or
      Php_FunctionComp(4201) or
      Php_FunctionComp(4265) or
      Php_FunctionComp(4318) or
      Php_FunctionComp(4495) or
      Php_FunctionComp(4584) or
      Php_FunctionComp(4585) or
      Php_FunctionComp(4887) or
      Php_FunctionComp(5068) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc124: TSynWebTokenKind;
begin
  if  Php_FunctionComp(205) or
      Php_FunctionComp(268) or
      Php_FunctionComp(383) or
      Php_FunctionComp(466) or
      Php_FunctionComp(502) or
      Php_FunctionComp(525) or
      Php_FunctionComp(580) or
      Php_FunctionComp(680) or
      Php_FunctionComp(729) or
      Php_FunctionComp(1446) or
      Php_FunctionComp(1453) or
      Php_FunctionComp(1582) or
      Php_FunctionComp(1669) or
      Php_FunctionComp(1945) or
      Php_FunctionComp(1974) or
      Php_FunctionComp(2271) or
      Php_FunctionComp(2449) or
      Php_FunctionComp(2482) or
      Php_FunctionComp(3101) or
      Php_FunctionComp(3183) or
      Php_FunctionComp(3214) or
      Php_FunctionComp(3378) or
      Php_FunctionComp(3391) or
      Php_FunctionComp(3632) or
      Php_FunctionComp(3691) or
      Php_FunctionComp(3752) or
      Php_FunctionComp(3929) or
      Php_FunctionComp(4498) or
      Php_FunctionComp(4589) or
      Php_FunctionComp(4592) or
      Php_FunctionComp(4593) or
      Php_FunctionComp(4884) or
      Php_FunctionComp(4921) or
      Php_FunctionComp(4925) or
      Php_FunctionComp(5140) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc125: TSynWebTokenKind;
begin
  if  Php_FunctionComp(140) or
      Php_FunctionComp(234) or
      Php_FunctionComp(277) or
      Php_FunctionComp(454) or
      Php_FunctionComp(1098) or
      Php_FunctionComp(1167) or
      Php_FunctionComp(1819) or
      Php_FunctionComp(1939) or
      Php_FunctionComp(2007) or
      Php_FunctionComp(2140) or
      Php_FunctionComp(2176) or
      Php_FunctionComp(2540) or
      Php_FunctionComp(2921) or
      Php_FunctionComp(3009) or
      Php_FunctionComp(3102) or
      Php_FunctionComp(3150) or
      Php_FunctionComp(3402) or
      Php_FunctionComp(3491) or
      Php_FunctionComp(3513) or
      Php_FunctionComp(3560) or
      Php_FunctionComp(3575) or
      Php_FunctionComp(3596) or
      Php_FunctionComp(3626) or
      Php_FunctionComp(3767) or
      Php_FunctionComp(3774) or
      Php_FunctionComp(3927) or
      Php_FunctionComp(4088) or
      Php_FunctionComp(4132) or
      Php_FunctionComp(4250) or
      Php_FunctionComp(4290) or
      Php_FunctionComp(4507) or
      Php_FunctionComp(4728) or
      Php_FunctionComp(4761) or
      Php_FunctionComp(4970) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc126: TSynWebTokenKind;
begin
  if  Php_KeywordComp(42) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(131) or
        Php_FunctionComp(260) or
        Php_FunctionComp(332) or
        Php_FunctionComp(345) or
        Php_FunctionComp(348) or
        Php_FunctionComp(483) or
        Php_FunctionComp(634) or
        Php_FunctionComp(693) or
        Php_FunctionComp(726) or
        Php_FunctionComp(1157) or
        Php_FunctionComp(1188) or
        Php_FunctionComp(1416) or
        Php_FunctionComp(1758) or
        Php_FunctionComp(1986) or
        Php_FunctionComp(2018) or
        Php_FunctionComp(2063) or
        Php_FunctionComp(2093) or
        Php_FunctionComp(3023) or
        Php_FunctionComp(3100) or
        Php_FunctionComp(3221) or
        Php_FunctionComp(3481) or
        Php_FunctionComp(3598) or
        Php_FunctionComp(3866) or
        Php_FunctionComp(3877) or
        Php_FunctionComp(3878) or
        Php_FunctionComp(4102) or
        Php_FunctionComp(4162) or
        Php_FunctionComp(4357) or
        Php_FunctionComp(4441) or
        Php_FunctionComp(4862) or
        Php_FunctionComp(4924) or
        Php_FunctionComp(5122) or
        Php_FunctionComp(5124) or
        Php_FunctionComp(5126) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc127: TSynWebTokenKind;
begin
  if  Php_FunctionComp(148) or
      Php_FunctionComp(229) or
      Php_FunctionComp(236) or
      Php_FunctionComp(1785) or
      Php_FunctionComp(1966) or
      Php_FunctionComp(2079) or
      Php_FunctionComp(2284) or
      Php_FunctionComp(2286) or
      Php_FunctionComp(2567) or
      Php_FunctionComp(2627) or
      Php_FunctionComp(2844) or
      Php_FunctionComp(3051) or
      Php_FunctionComp(3169) or
      Php_FunctionComp(3227) or
      Php_FunctionComp(3365) or
      Php_FunctionComp(3427) or
      Php_FunctionComp(3674) or
      Php_FunctionComp(4248) or
      Php_FunctionComp(4380) or
      Php_FunctionComp(4396) or
      Php_FunctionComp(4522) or
      Php_FunctionComp(4601) or
      Php_FunctionComp(4759) or
      Php_FunctionComp(4898) or
      Php_FunctionComp(4958) or
      Php_FunctionComp(5104) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc128: TSynWebTokenKind;
begin
  if  Php_FunctionComp(112) or
      Php_FunctionComp(202) or
      Php_FunctionComp(393) or
      Php_FunctionComp(417) or
      Php_FunctionComp(449) or
      Php_FunctionComp(503) or
      Php_FunctionComp(549) or
      Php_FunctionComp(600) or
      Php_FunctionComp(602) or
      Php_FunctionComp(713) or
      Php_FunctionComp(750) or
      Php_FunctionComp(763) or
      Php_FunctionComp(1241) or
      Php_FunctionComp(1273) or
      Php_FunctionComp(1290) or
      Php_FunctionComp(1472) or
      Php_FunctionComp(2083) or
      Php_FunctionComp(2142) or
      Php_FunctionComp(2227) or
      Php_FunctionComp(2636) or
      Php_FunctionComp(3155) or
      Php_FunctionComp(3565) or
      Php_FunctionComp(3769) or
      Php_FunctionComp(4523) or
      Php_FunctionComp(4808) or
      Php_FunctionComp(4877) or
      Php_FunctionComp(4888) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc129: TSynWebTokenKind;
begin
  if  Php_FunctionComp(209) or
      Php_FunctionComp(437) or
      Php_FunctionComp(563) or
      Php_FunctionComp(657) or
      Php_FunctionComp(1146) or
      Php_FunctionComp(1195) or
      Php_FunctionComp(1757) or
      Php_FunctionComp(1798) or
      Php_FunctionComp(1817) or
      Php_FunctionComp(1982) or
      Php_FunctionComp(2339) or
      Php_FunctionComp(2450) or
      Php_FunctionComp(3108) or
      Php_FunctionComp(3125) or
      Php_FunctionComp(3156) or
      Php_FunctionComp(3213) or
      Php_FunctionComp(3905) or
      Php_FunctionComp(4491) or
      Php_FunctionComp(4883) or
      Php_FunctionComp(4886) or
      Php_FunctionComp(4896) or
      Php_FunctionComp(4962) or
      Php_FunctionComp(4963) or
      Php_FunctionComp(5120) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc130: TSynWebTokenKind;
begin
  if  Php_KeywordComp(61) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(303) or
        Php_FunctionComp(489) or
        Php_FunctionComp(505) or
        Php_FunctionComp(561) or
        Php_FunctionComp(662) or
        Php_FunctionComp(789) or
        Php_FunctionComp(797) or
        Php_FunctionComp(1147) or
        Php_FunctionComp(1158) or
        Php_FunctionComp(1187) or
        Php_FunctionComp(1327) or
        Php_FunctionComp(1432) or
        Php_FunctionComp(1475) or
        Php_FunctionComp(1661) or
        Php_FunctionComp(1910) or
        Php_FunctionComp(2262) or
        Php_FunctionComp(2360) or
        Php_FunctionComp(2447) or
        Php_FunctionComp(2637) or
        Php_FunctionComp(2717) or
        Php_FunctionComp(2866) or
        Php_FunctionComp(3094) or
        Php_FunctionComp(3137) or
        Php_FunctionComp(3165) or
        Php_FunctionComp(3415) or
        Php_FunctionComp(3416) or
        Php_FunctionComp(3517) or
        Php_FunctionComp(3539) or
        Php_FunctionComp(3629) or
        Php_FunctionComp(3736) or
        Php_FunctionComp(4020) or
        Php_FunctionComp(4092) or
        Php_FunctionComp(4336) or
        Php_FunctionComp(4591) or
        Php_FunctionComp(5072) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc131: TSynWebTokenKind;
begin
  if  Php_FunctionComp(118) or
      Php_FunctionComp(312) or
      Php_FunctionComp(1082) or
      Php_FunctionComp(1097) or
      Php_FunctionComp(1223) or
      Php_FunctionComp(1553) or
      Php_FunctionComp(1584) or
      Php_FunctionComp(1964) or
      Php_FunctionComp(2056) or
      Php_FunctionComp(2070) or
      Php_FunctionComp(2159) or
      Php_FunctionComp(2427) or
      Php_FunctionComp(2650) or
      Php_FunctionComp(2684) or
      Php_FunctionComp(2862) or
      Php_FunctionComp(3010) or
      Php_FunctionComp(3031) or
      Php_FunctionComp(3042) or
      Php_FunctionComp(3131) or
      Php_FunctionComp(3208) or
      Php_FunctionComp(3225) or
      Php_FunctionComp(3316) or
      Php_FunctionComp(3389) or
      Php_FunctionComp(3466) or
      Php_FunctionComp(3507) or
      Php_FunctionComp(3595) or
      Php_FunctionComp(3600) or
      Php_FunctionComp(4126) or
      Php_FunctionComp(4499) or
      Php_FunctionComp(4532) or
      Php_FunctionComp(4571) or
      Php_FunctionComp(4865) or
      Php_FunctionComp(4890) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc132: TSynWebTokenKind;
begin
  if  Php_FunctionComp(76) or
      Php_FunctionComp(198) or
      Php_FunctionComp(544) or
      Php_FunctionComp(565) or
      Php_FunctionComp(705) or
      Php_FunctionComp(799) or
      Php_FunctionComp(802) or
      Php_FunctionComp(1003) or
      Php_FunctionComp(1168) or
      Php_FunctionComp(1169) or
      Php_FunctionComp(1194) or
      Php_FunctionComp(1328) or
      Php_FunctionComp(1386) or
      Php_FunctionComp(1574) or
      Php_FunctionComp(1628) or
      Php_FunctionComp(1963) or
      Php_FunctionComp(2010) or
      Php_FunctionComp(2044) or
      Php_FunctionComp(2105) or
      Php_FunctionComp(2162) or
      Php_FunctionComp(2402) or
      Php_FunctionComp(2405) or
      Php_FunctionComp(2670) or
      Php_FunctionComp(2726) or
      Php_FunctionComp(3044) or
      Php_FunctionComp(3064) or
      Php_FunctionComp(3099) or
      Php_FunctionComp(4202) or
      Php_FunctionComp(4326) or
      Php_FunctionComp(4356) or
      Php_FunctionComp(4369) or
      Php_FunctionComp(4769) or
      Php_FunctionComp(4773) or
      Php_FunctionComp(5002) or
      Php_FunctionComp(5066) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc133: TSynWebTokenKind;
begin
  if  Php_KeywordComp(53) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(213) or
        Php_FunctionComp(357) or
        Php_FunctionComp(421) or
        Php_FunctionComp(469) or
        Php_FunctionComp(1292) or
        Php_FunctionComp(1672) or
        Php_FunctionComp(1717) or
        Php_FunctionComp(1744) or
        Php_FunctionComp(1832) or
        Php_FunctionComp(1981) or
        Php_FunctionComp(1990) or
        Php_FunctionComp(2193) or
        Php_FunctionComp(2885) or
        Php_FunctionComp(3509) or
        Php_FunctionComp(3651) or
        Php_FunctionComp(3653) or
        Php_FunctionComp(3738) or
        Php_FunctionComp(3851) or
        Php_FunctionComp(3883) or
        Php_FunctionComp(4035) or
        Php_FunctionComp(4447) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc134: TSynWebTokenKind;
begin
  if  Php_FunctionComp(411) or
      Php_FunctionComp(695) or
      Php_FunctionComp(714) or
      Php_FunctionComp(1161) or
      Php_FunctionComp(1745) or
      Php_FunctionComp(2141) or
      Php_FunctionComp(2235) or
      Php_FunctionComp(2285) or
      Php_FunctionComp(2652) or
      Php_FunctionComp(2744) or
      Php_FunctionComp(3305) or
      Php_FunctionComp(3312) or
      Php_FunctionComp(3433) or
      Php_FunctionComp(3474) or
      Php_FunctionComp(3555) or
      Php_FunctionComp(3753) or
      Php_FunctionComp(3797) or
      Php_FunctionComp(4073) or
      Php_FunctionComp(4446) or
      Php_FunctionComp(4506) or
      Php_FunctionComp(4568) or
      Php_FunctionComp(4768) or
      Php_FunctionComp(4892) or
      Php_FunctionComp(4927) or
      Php_FunctionComp(4972) or
      Php_FunctionComp(5074) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc135: TSynWebTokenKind;
begin
  if  Php_FunctionComp(80) or
      Php_FunctionComp(146) or
      Php_FunctionComp(304) or
      Php_FunctionComp(341) or
      Php_FunctionComp(413) or
      Php_FunctionComp(598) or
      Php_FunctionComp(601) or
      Php_FunctionComp(653) or
      Php_FunctionComp(730) or
      Php_FunctionComp(743) or
      Php_FunctionComp(928) or
      Php_FunctionComp(1106) or
      Php_FunctionComp(1298) or
      Php_FunctionComp(1516) or
      Php_FunctionComp(1707) or
      Php_FunctionComp(1743) or
      Php_FunctionComp(2131) or
      Php_FunctionComp(2625) or
      Php_FunctionComp(2630) or
      Php_FunctionComp(2832) or
      Php_FunctionComp(3191) or
      Php_FunctionComp(3309) or
      Php_FunctionComp(3382) or
      Php_FunctionComp(3659) or
      Php_FunctionComp(3783) or
      Php_FunctionComp(3849) or
      Php_FunctionComp(3933) or
      Php_FunctionComp(4604) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc136: TSynWebTokenKind;
begin
  if  Php_FunctionComp(117) or
      Php_FunctionComp(328) or
      Php_FunctionComp(379) or
      Php_FunctionComp(387) or
      Php_FunctionComp(491) or
      Php_FunctionComp(538) or
      Php_FunctionComp(573) or
      Php_FunctionComp(627) or
      Php_FunctionComp(1049) or
      Php_FunctionComp(1334) or
      Php_FunctionComp(1761) or
      Php_FunctionComp(1822) or
      Php_FunctionComp(2061) or
      Php_FunctionComp(2150) or
      Php_FunctionComp(2222) or
      Php_FunctionComp(2249) or
      Php_FunctionComp(2259) or
      Php_FunctionComp(2440) or
      Php_FunctionComp(3052) or
      Php_FunctionComp(3153) or
      Php_FunctionComp(3306) or
      Php_FunctionComp(3482) or
      Php_FunctionComp(3536) or
      Php_FunctionComp(3979) or
      Php_FunctionComp(4002) or
      Php_FunctionComp(4028) or
      Php_FunctionComp(4135) or
      Php_FunctionComp(4174) or
      Php_FunctionComp(4256) or
      Php_FunctionComp(4284) or
      Php_FunctionComp(4302) or
      Php_FunctionComp(4775) or
      Php_FunctionComp(5073) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc137: TSynWebTokenKind;
begin
  if  Php_FunctionComp(439) or
      Php_FunctionComp(539) or
      Php_FunctionComp(545) or
      Php_FunctionComp(665) or
      Php_FunctionComp(728) or
      Php_FunctionComp(1029) or
      Php_FunctionComp(1120) or
      Php_FunctionComp(1165) or
      Php_FunctionComp(1382) or
      Php_FunctionComp(1589) or
      Php_FunctionComp(1804) or
      Php_FunctionComp(1821) or
      Php_FunctionComp(2003) or
      Php_FunctionComp(2471) or
      Php_FunctionComp(2581) or
      Php_FunctionComp(2638) or
      Php_FunctionComp(2639) or
      Php_FunctionComp(3210) or
      Php_FunctionComp(3288) or
      Php_FunctionComp(3686) or
      Php_FunctionComp(3770) or
      Php_FunctionComp(3926) or
      Php_FunctionComp(4068) or
      Php_FunctionComp(4269) or
      Php_FunctionComp(4295) or
      Php_FunctionComp(4429) or
      Php_FunctionComp(4573) or
      Php_FunctionComp(4730) or
      Php_FunctionComp(4954) or
      Php_FunctionComp(4973) or
      Php_FunctionComp(5061) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc138: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1025) or
      Php_FunctionComp(1575) or
      Php_FunctionComp(1601) or
      Php_FunctionComp(2109) or
      Php_FunctionComp(2374) or
      Php_FunctionComp(2452) or
      Php_FunctionComp(2542) or
      Php_FunctionComp(2851) or
      Php_FunctionComp(2899) or
      Php_FunctionComp(3041) or
      Php_FunctionComp(3151) or
      Php_FunctionComp(3158) or
      Php_FunctionComp(3401) or
      Php_FunctionComp(3443) or
      Php_FunctionComp(3868) or
      Php_FunctionComp(4085) or
      Php_FunctionComp(4155) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc139: TSynWebTokenKind;
begin
  if  Php_FunctionComp(461) or
      Php_FunctionComp(481) or
      Php_FunctionComp(579) or
      Php_FunctionComp(666) or
      Php_FunctionComp(1148) or
      Php_FunctionComp(1265) or
      Php_FunctionComp(1324) or
      Php_FunctionComp(1396) or
      Php_FunctionComp(1590) or
      Php_FunctionComp(1625) or
      Php_FunctionComp(1754) or
      Php_FunctionComp(1773) or
      Php_FunctionComp(1923) or
      Php_FunctionComp(1931) or
      Php_FunctionComp(2002) or
      Php_FunctionComp(2400) or
      Php_FunctionComp(2446) or
      Php_FunctionComp(2531) or
      Php_FunctionComp(2600) or
      Php_FunctionComp(2632) or
      Php_FunctionComp(2774) or
      Php_FunctionComp(2777) or
      Php_FunctionComp(2850) or
      Php_FunctionComp(2880) or
      Php_FunctionComp(3265) or
      Php_FunctionComp(3758) or
      Php_FunctionComp(3798) or
      Php_FunctionComp(3906) or
      Php_FunctionComp(4301) or
      Php_FunctionComp(4512) or
      Php_FunctionComp(4531) or
      Php_FunctionComp(4536) or
      Php_FunctionComp(4653) or
      Php_FunctionComp(4844) or
      Php_FunctionComp(4873) or
      Php_FunctionComp(4894) or
      Php_FunctionComp(5079) or
      Php_FunctionComp(5153) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc140: TSynWebTokenKind;
begin
  if  Php_FunctionComp(78) or
      Php_FunctionComp(426) or
      Php_FunctionComp(531) or
      Php_FunctionComp(595) or
      Php_FunctionComp(599) or
      Php_FunctionComp(652) or
      Php_FunctionComp(719) or
      Php_FunctionComp(1023) or
      Php_FunctionComp(1149) or
      Php_FunctionComp(1193) or
      Php_FunctionComp(1858) or
      Php_FunctionComp(2398) or
      Php_FunctionComp(2437) or
      Php_FunctionComp(2472) or
      Php_FunctionComp(2579) or
      Php_FunctionComp(2617) or
      Php_FunctionComp(2681) or
      Php_FunctionComp(2691) or
      Php_FunctionComp(3122) or
      Php_FunctionComp(3377) or
      Php_FunctionComp(3407) or
      Php_FunctionComp(3531) or
      Php_FunctionComp(3657) or
      Php_FunctionComp(3676) or
      Php_FunctionComp(3776) or
      Php_FunctionComp(3793) or
      Php_FunctionComp(3988) or
      Php_FunctionComp(4213) or
      Php_FunctionComp(4475) or
      Php_FunctionComp(4537) or
      Php_FunctionComp(4595) or
      Php_FunctionComp(4869) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc141: TSynWebTokenKind;
begin
  if  Php_FunctionComp(147) or
      Php_FunctionComp(519) or
      Php_FunctionComp(569) or
      Php_FunctionComp(576) or
      Php_FunctionComp(585) or
      Php_FunctionComp(754) or
      Php_FunctionComp(1175) or
      Php_FunctionComp(1219) or
      Php_FunctionComp(1314) or
      Php_FunctionComp(1339) or
      Php_FunctionComp(1501) or
      Php_FunctionComp(1605) or
      Php_FunctionComp(1609) or
      Php_FunctionComp(1898) or
      Php_FunctionComp(1918) or
      Php_FunctionComp(2030) or
      Php_FunctionComp(2333) or
      Php_FunctionComp(2335) or
      Php_FunctionComp(2550) or
      Php_FunctionComp(2586) or
      Php_FunctionComp(2785) or
      Php_FunctionComp(2935) or
      Php_FunctionComp(3393) or
      Php_FunctionComp(3429) or
      Php_FunctionComp(3530) or
      Php_FunctionComp(3584) or
      Php_FunctionComp(3589) or
      Php_FunctionComp(4124) or
      Php_FunctionComp(4405) or
      Php_FunctionComp(4521) or
      Php_FunctionComp(4538) or
      Php_FunctionComp(4749) or
      Php_FunctionComp(4786) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc142: TSynWebTokenKind;
begin
  if  Php_FunctionComp(119) or
      Php_FunctionComp(128) or
      Php_FunctionComp(494) or
      Php_FunctionComp(499) or
      Php_FunctionComp(715) or
      Php_FunctionComp(738) or
      Php_FunctionComp(1046) or
      Php_FunctionComp(1183) or
      Php_FunctionComp(1554) or
      Php_FunctionComp(1684) or
      Php_FunctionComp(1815) or
      Php_FunctionComp(1875) or
      Php_FunctionComp(1881) or
      Php_FunctionComp(2005) or
      Php_FunctionComp(2022) or
      Php_FunctionComp(2263) or
      Php_FunctionComp(2336) or
      Php_FunctionComp(2349) or
      Php_FunctionComp(2406) or
      Php_FunctionComp(2526) or
      Php_FunctionComp(2575) or
      Php_FunctionComp(2635) or
      Php_FunctionComp(2874) or
      Php_FunctionComp(2976) or
      Php_FunctionComp(3076) or
      Php_FunctionComp(3492) or
      Php_FunctionComp(3638) or
      Php_FunctionComp(3794) or
      Php_FunctionComp(4146) or
      Php_FunctionComp(4216) or
      Php_FunctionComp(4381) or
      Php_FunctionComp(4539) or
      Php_FunctionComp(4813) or
      Php_FunctionComp(4864) or
      Php_FunctionComp(4899) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc143: TSynWebTokenKind;
begin
  if  Php_FunctionComp(132) or
      Php_FunctionComp(246) or
      Php_FunctionComp(443) or
      Php_FunctionComp(477) or
      Php_FunctionComp(496) or
      Php_FunctionComp(1065) or
      Php_FunctionComp(1108) or
      Php_FunctionComp(1500) or
      Php_FunctionComp(1660) or
      Php_FunctionComp(1677) or
      Php_FunctionComp(1836) or
      Php_FunctionComp(1872) or
      Php_FunctionComp(2057) or
      Php_FunctionComp(2120) or
      Php_FunctionComp(2138) or
      Php_FunctionComp(2215) or
      Php_FunctionComp(2266) or
      Php_FunctionComp(2330) or
      Php_FunctionComp(2404) or
      Php_FunctionComp(2478) or
      Php_FunctionComp(2483) or
      Php_FunctionComp(3134) or
      Php_FunctionComp(3428) or
      Php_FunctionComp(3432) or
      Php_FunctionComp(3470) or
      Php_FunctionComp(3540) or
      Php_FunctionComp(3675) or
      Php_FunctionComp(3854) or
      Php_FunctionComp(3986) or
      Php_FunctionComp(4328) or
      Php_FunctionComp(4779) or
      Php_FunctionComp(4956) or
      Php_FunctionComp(5067) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc144: TSynWebTokenKind;
begin
  if  Php_FunctionComp(120) or
      Php_FunctionComp(587) or
      Php_FunctionComp(716) or
      Php_FunctionComp(732) or
      Php_FunctionComp(1034) or
      Php_FunctionComp(1123) or
      Php_FunctionComp(1371) or
      Php_FunctionComp(1603) or
      Php_FunctionComp(1624) or
      Php_FunctionComp(2693) or
      Php_FunctionComp(3048) or
      Php_FunctionComp(3235) or
      Php_FunctionComp(3313) or
      Php_FunctionComp(3421) or
      Php_FunctionComp(3573) or
      Php_FunctionComp(3578) or
      Php_FunctionComp(3615) or
      Php_FunctionComp(3666) or
      Php_FunctionComp(3741) or
      Php_FunctionComp(3880) or
      Php_FunctionComp(3930) or
      Php_FunctionComp(4060) or
      Php_FunctionComp(4341) or
      Php_FunctionComp(4696) or
      Php_FunctionComp(4776) or
      Php_FunctionComp(4934) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc145: TSynWebTokenKind;
begin
  if  Php_FunctionComp(208) or
      Php_FunctionComp(329) or
      Php_FunctionComp(498) or
      Php_FunctionComp(605) or
      Php_FunctionComp(668) or
      Php_FunctionComp(691) or
      Php_FunctionComp(1007) or
      Php_FunctionComp(1117) or
      Php_FunctionComp(1400) or
      Php_FunctionComp(1712) or
      Php_FunctionComp(1936) or
      Php_FunctionComp(1969) or
      Php_FunctionComp(2019) or
      Php_FunctionComp(2108) or
      Php_FunctionComp(2171) or
      Php_FunctionComp(2311) or
      Php_FunctionComp(2429) or
      Php_FunctionComp(2572) or
      Php_FunctionComp(2595) or
      Php_FunctionComp(2696) or
      Php_FunctionComp(2872) or
      Php_FunctionComp(3160) or
      Php_FunctionComp(3301) or
      Php_FunctionComp(3656) or
      Php_FunctionComp(3715) or
      Php_FunctionComp(3820) or
      Php_FunctionComp(3863) or
      Php_FunctionComp(4145) or
      Php_FunctionComp(4209) or
      Php_FunctionComp(4232) or
      Php_FunctionComp(4237) or
      Php_FunctionComp(4242) or
      Php_FunctionComp(4323) or
      Php_FunctionComp(4682) or
      Php_FunctionComp(4729) or
      Php_FunctionComp(5105) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc146: TSynWebTokenKind;
begin
  if  Php_FunctionComp(113) or
      Php_FunctionComp(133) or
      Php_FunctionComp(325) or
      Php_FunctionComp(495) or
      Php_FunctionComp(541) or
      Php_FunctionComp(718) or
      Php_FunctionComp(1021) or
      Php_FunctionComp(1100) or
      Php_FunctionComp(1142) or
      Php_FunctionComp(1182) or
      Php_FunctionComp(1390) or
      Php_FunctionComp(1471) or
      Php_FunctionComp(1567) or
      Php_FunctionComp(2020) or
      Php_FunctionComp(2023) or
      Php_FunctionComp(2040) or
      Php_FunctionComp(2366) or
      Php_FunctionComp(2453) or
      Php_FunctionComp(2561) or
      Php_FunctionComp(2594) or
      Php_FunctionComp(2642) or
      Php_FunctionComp(2654) or
      Php_FunctionComp(2731) or
      Php_FunctionComp(3062) or
      Php_FunctionComp(3078) or
      Php_FunctionComp(3446) or
      Php_FunctionComp(3570) or
      Php_FunctionComp(3599) or
      Php_FunctionComp(4030) or
      Php_FunctionComp(4093) or
      Php_FunctionComp(4139) or
      Php_FunctionComp(4215) or
      Php_FunctionComp(4570) or
      Php_FunctionComp(5075) or
      Php_FunctionComp(5110) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc147: TSynWebTokenKind;
begin
  if  Php_FunctionComp(127) or
      Php_FunctionComp(300) or
      Php_FunctionComp(360) or
      Php_FunctionComp(522) or
      Php_FunctionComp(723) or
      Php_FunctionComp(1067) or
      Php_FunctionComp(1144) or
      Php_FunctionComp(1406) or
      Php_FunctionComp(1581) or
      Php_FunctionComp(1643) or
      Php_FunctionComp(1787) or
      Php_FunctionComp(1907) or
      Php_FunctionComp(1935) or
      Php_FunctionComp(2355) or
      Php_FunctionComp(2455) or
      Php_FunctionComp(2538) or
      Php_FunctionComp(2648) or
      Php_FunctionComp(2775) or
      Php_FunctionComp(2869) or
      Php_FunctionComp(2884) or
      Php_FunctionComp(3121) or
      Php_FunctionComp(3228) or
      Php_FunctionComp(3244) or
      Php_FunctionComp(3367) or
      Php_FunctionComp(3444) or
      Php_FunctionComp(3547) or
      Php_FunctionComp(3671) or
      Php_FunctionComp(3679) or
      Php_FunctionComp(3859) or
      Php_FunctionComp(4249) or
      Php_FunctionComp(4603) or
      Php_FunctionComp(4737) or
      Php_FunctionComp(4849) or
      Php_FunctionComp(5077) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc148: TSynWebTokenKind;
begin
  if  Php_FunctionComp(98) or
      Php_FunctionComp(157) or
      Php_FunctionComp(327) or
      Php_FunctionComp(692) or
      Php_FunctionComp(740) or
      Php_FunctionComp(1018) or
      Php_FunctionComp(1154) or
      Php_FunctionComp(1212) or
      Php_FunctionComp(1216) or
      Php_FunctionComp(1329) or
      Php_FunctionComp(1413) or
      Php_FunctionComp(1464) or
      Php_FunctionComp(1663) or
      Php_FunctionComp(1680) or
      Php_FunctionComp(2033) or
      Php_FunctionComp(2590) or
      Php_FunctionComp(2881) or
      Php_FunctionComp(2994) or
      Php_FunctionComp(3233) or
      Php_FunctionComp(3331) or
      Php_FunctionComp(3454) or
      Php_FunctionComp(3546) or
      Php_FunctionComp(3588) or
      Php_FunctionComp(3614) or
      Php_FunctionComp(3644) or
      Php_FunctionComp(3968) or
      Php_FunctionComp(4278) or
      Php_FunctionComp(4327) or
      Php_FunctionComp(4412) or
      Php_FunctionComp(4470) or
      Php_FunctionComp(4794) or
      Php_FunctionComp(4826) or
      Php_FunctionComp(4854) or
      Php_FunctionComp(4994) or
      Php_FunctionComp(5102) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc149: TSynWebTokenKind;
begin
  if  Php_FunctionComp(377) or
      Php_FunctionComp(515) or
      Php_FunctionComp(721) or
      Php_FunctionComp(1135) or
      Php_FunctionComp(1203) or
      Php_FunctionComp(1291) or
      Php_FunctionComp(1681) or
      Php_FunctionComp(1775) or
      Php_FunctionComp(2133) or
      Php_FunctionComp(2329) or
      Php_FunctionComp(2663) or
      Php_FunctionComp(2738) or
      Php_FunctionComp(3074) or
      Php_FunctionComp(3289) or
      Php_FunctionComp(3583) or
      Php_FunctionComp(3627) or
      Php_FunctionComp(3672) or
      Php_FunctionComp(3684) or
      Php_FunctionComp(3737) or
      Php_FunctionComp(3810) or
      Php_FunctionComp(3811) or
      Php_FunctionComp(3874) or
      Php_FunctionComp(3955) or
      Php_FunctionComp(4001) or
      Php_FunctionComp(4311) or
      Php_FunctionComp(4392) or
      Php_FunctionComp(4691) or
      Php_FunctionComp(4734) or
      Php_FunctionComp(4788) or
      Php_FunctionComp(4814) or
      Php_FunctionComp(4974) or
      Php_FunctionComp(5106) or
      Php_FunctionComp(5132) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc150: TSynWebTokenKind;
begin
  if  Php_FunctionComp(211) or
      Php_FunctionComp(244) or
      Php_FunctionComp(358) or
      Php_FunctionComp(384) or
      Php_FunctionComp(534) or
      Php_FunctionComp(648) or
      Php_FunctionComp(727) or
      Php_FunctionComp(1024) or
      Php_FunctionComp(1027) or
      Php_FunctionComp(1214) or
      Php_FunctionComp(1221) or
      Php_FunctionComp(1253) or
      Php_FunctionComp(1492) or
      Php_FunctionComp(1498) or
      Php_FunctionComp(1704) or
      Php_FunctionComp(1738) or
      Php_FunctionComp(1886) or
      Php_FunctionComp(1887) or
      Php_FunctionComp(1946) or
      Php_FunctionComp(2560) or
      Php_FunctionComp(2577) or
      Php_FunctionComp(2593) or
      Php_FunctionComp(2680) or
      Php_FunctionComp(3103) or
      Php_FunctionComp(3138) or
      Php_FunctionComp(3273) or
      Php_FunctionComp(3412) or
      Php_FunctionComp(3448) or
      Php_FunctionComp(3479) or
      Php_FunctionComp(3580) or
      Php_FunctionComp(3720) or
      Php_FunctionComp(3762) or
      Php_FunctionComp(3809) or
      Php_FunctionComp(4097) or
      Php_FunctionComp(4107) or
      Php_FunctionComp(4272) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc151: TSynWebTokenKind;
begin
  if  Php_FunctionComp(143) or
      Php_FunctionComp(1173) or
      Php_FunctionComp(1186) or
      Php_FunctionComp(1777) or
      Php_FunctionComp(1870) or
      Php_FunctionComp(2038) or
      Php_FunctionComp(2444) or
      Php_FunctionComp(2578) or
      Php_FunctionComp(2647) or
      Php_FunctionComp(2886) or
      Php_FunctionComp(2946) or
      Php_FunctionComp(3616) or
      Php_FunctionComp(3664) or
      Php_FunctionComp(3693) or
      Php_FunctionComp(3902) or
      Php_FunctionComp(4316) or
      Php_FunctionComp(5154) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc152: TSynWebTokenKind;
begin
  if  Php_FunctionComp(129) or
      Php_FunctionComp(380) or
      Php_FunctionComp(390) or
      Php_FunctionComp(669) or
      Php_FunctionComp(737) or
      Php_FunctionComp(968) or
      Php_FunctionComp(1030) or
      Php_FunctionComp(1035) or
      Php_FunctionComp(1099) or
      Php_FunctionComp(1196) or
      Php_FunctionComp(1735) or
      Php_FunctionComp(1755) or
      Php_FunctionComp(1803) or
      Php_FunctionComp(1911) or
      Php_FunctionComp(1987) or
      Php_FunctionComp(2139) or
      Php_FunctionComp(2209) or
      Php_FunctionComp(2232) or
      Php_FunctionComp(2244) or
      Php_FunctionComp(2357) or
      Php_FunctionComp(2549) or
      Php_FunctionComp(2621) or
      Php_FunctionComp(2694) or
      Php_FunctionComp(2889) or
      Php_FunctionComp(3145) or
      Php_FunctionComp(3194) or
      Php_FunctionComp(3297) or
      Php_FunctionComp(3577) or
      Php_FunctionComp(3677) or
      Php_FunctionComp(3716) or
      Php_FunctionComp(3761) or
      Php_FunctionComp(3779) or
      Php_FunctionComp(4263) or
      Php_FunctionComp(4440) or
      Php_FunctionComp(4497) or
      Php_FunctionComp(4569) or
      Php_FunctionComp(4576) or
      Php_FunctionComp(5065) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc153: TSynWebTokenKind;
begin
  if  Php_FunctionComp(123) or
      Php_FunctionComp(124) or
      Php_FunctionComp(394) or
      Php_FunctionComp(447) or
      Php_FunctionComp(456) or
      Php_FunctionComp(554) or
      Php_FunctionComp(591) or
      Php_FunctionComp(1163) or
      Php_FunctionComp(1207) or
      Php_FunctionComp(1218) or
      Php_FunctionComp(1325) or
      Php_FunctionComp(1565) or
      Php_FunctionComp(1763) or
      Php_FunctionComp(2337) or
      Php_FunctionComp(2416) or
      Php_FunctionComp(2473) or
      Php_FunctionComp(2628) or
      Php_FunctionComp(2707) or
      Php_FunctionComp(3223) or
      Php_FunctionComp(3246) or
      Php_FunctionComp(3434) or
      Php_FunctionComp(3535) or
      Php_FunctionComp(3576) or
      Php_FunctionComp(3993) or
      Php_FunctionComp(4063) or
      Php_FunctionComp(4134) or
      Php_FunctionComp(4177) or
      Php_FunctionComp(4376) or
      Php_FunctionComp(4548) or
      Php_FunctionComp(5113) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc154: TSynWebTokenKind;
begin
  if  Php_FunctionComp(90) or
      Php_FunctionComp(121) or
      Php_FunctionComp(298) or
      Php_FunctionComp(311) or
      Php_FunctionComp(487) or
      Php_FunctionComp(699) or
      Php_FunctionComp(771) or
      Php_FunctionComp(788) or
      Php_FunctionComp(1031) or
      Php_FunctionComp(1164) or
      Php_FunctionComp(1174) or
      Php_FunctionComp(1277) or
      Php_FunctionComp(1414) or
      Php_FunctionComp(1538) or
      Php_FunctionComp(1699) or
      Php_FunctionComp(1784) or
      Php_FunctionComp(1908) or
      Php_FunctionComp(2170) or
      Php_FunctionComp(2361) or
      Php_FunctionComp(2371) or
      Php_FunctionComp(2415) or
      Php_FunctionComp(2603) or
      Php_FunctionComp(2653) or
      Php_FunctionComp(2903) or
      Php_FunctionComp(2905) or
      Php_FunctionComp(2924) or
      Php_FunctionComp(3119) or
      Php_FunctionComp(3199) or
      Php_FunctionComp(3374) or
      Php_FunctionComp(3460) or
      Php_FunctionComp(3658) or
      Php_FunctionComp(3698) or
      Php_FunctionComp(3818) or
      Php_FunctionComp(3845) or
      Php_FunctionComp(4057) or
      Php_FunctionComp(4271) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc155: TSynWebTokenKind;
begin
  if  Php_FunctionComp(151) or
      Php_FunctionComp(160) or
      Php_FunctionComp(232) or
      Php_FunctionComp(731) or
      Php_FunctionComp(794) or
      Php_FunctionComp(951) or
      Php_FunctionComp(1160) or
      Php_FunctionComp(1401) or
      Php_FunctionComp(1770) or
      Php_FunctionComp(1827) or
      Php_FunctionComp(1958) or
      Php_FunctionComp(2025) or
      Php_FunctionComp(2169) or
      Php_FunctionComp(2245) or
      Php_FunctionComp(2582) or
      Php_FunctionComp(2708) or
      Php_FunctionComp(2732) or
      Php_FunctionComp(2804) or
      Php_FunctionComp(3011) or
      Php_FunctionComp(3014) or
      Php_FunctionComp(3395) or
      Php_FunctionComp(3452) or
      Php_FunctionComp(3520) or
      Php_FunctionComp(3537) or
      Php_FunctionComp(3538) or
      Php_FunctionComp(3654) or
      Php_FunctionComp(4330) or
      Php_FunctionComp(4427) or
      Php_FunctionComp(4529) or
      Php_FunctionComp(4554) or
      Php_FunctionComp(4663) or
      Php_FunctionComp(4783) or
      Php_FunctionComp(4908) or
      Php_FunctionComp(5141) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc156: TSynWebTokenKind;
begin
  if  Php_FunctionComp(258) or
      Php_FunctionComp(343) or
      Php_FunctionComp(424) or
      Php_FunctionComp(516) or
      Php_FunctionComp(564) or
      Php_FunctionComp(1069) or
      Php_FunctionComp(1134) or
      Php_FunctionComp(1428) or
      Php_FunctionComp(1461) or
      Php_FunctionComp(1481) or
      Php_FunctionComp(1490) or
      Php_FunctionComp(1622) or
      Php_FunctionComp(1748) or
      Php_FunctionComp(1984) or
      Php_FunctionComp(2074) or
      Php_FunctionComp(2414) or
      Php_FunctionComp(2500) or
      Php_FunctionComp(2649) or
      Php_FunctionComp(2658) or
      Php_FunctionComp(2664) or
      Php_FunctionComp(2697) or
      Php_FunctionComp(2730) or
      Php_FunctionComp(3189) or
      Php_FunctionComp(3697) or
      Php_FunctionComp(3887) or
      Php_FunctionComp(3981) or
      Php_FunctionComp(4212) or
      Php_FunctionComp(4397) or
      Php_FunctionComp(4615) or
      Php_FunctionComp(4836) or
      Php_FunctionComp(4935) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc157: TSynWebTokenKind;
begin
  if  Php_FunctionComp(501) or
      Php_FunctionComp(523) or
      Php_FunctionComp(880) or
      Php_FunctionComp(1143) or
      Php_FunctionComp(1317) or
      Php_FunctionComp(1321) or
      Php_FunctionComp(1483) or
      Php_FunctionComp(1929) or
      Php_FunctionComp(1998) or
      Php_FunctionComp(2148) or
      Php_FunctionComp(2236) or
      Php_FunctionComp(2373) or
      Php_FunctionComp(2609) or
      Php_FunctionComp(3024) or
      Php_FunctionComp(3237) or
      Php_FunctionComp(3317) or
      Php_FunctionComp(3394) or
      Php_FunctionComp(3449) or
      Php_FunctionComp(3548) or
      Php_FunctionComp(3733) or
      Php_FunctionComp(3843) or
      Php_FunctionComp(3861) or
      Php_FunctionComp(4007) or
      Php_FunctionComp(4053) or
      Php_FunctionComp(4231) or
      Php_FunctionComp(4236) or
      Php_FunctionComp(4241) or
      Php_FunctionComp(4578) or
      Php_FunctionComp(4588) or
      Php_FunctionComp(4747) or
      Php_FunctionComp(4790) or
      Php_FunctionComp(4895) or
      Php_FunctionComp(5083) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc158: TSynWebTokenKind;
begin
  if  Php_FunctionComp(135) or
      Php_FunctionComp(335) or
      Php_FunctionComp(428) or
      Php_FunctionComp(907) or
      Php_FunctionComp(988) or
      Php_FunctionComp(1170) or
      Php_FunctionComp(1311) or
      Php_FunctionComp(1639) or
      Php_FunctionComp(1730) or
      Php_FunctionComp(1766) or
      Php_FunctionComp(2039) or
      Php_FunctionComp(2248) or
      Php_FunctionComp(2267) or
      Php_FunctionComp(2384) or
      Php_FunctionComp(2434) or
      Php_FunctionComp(2633) or
      Php_FunctionComp(2659) or
      Php_FunctionComp(2687) or
      Php_FunctionComp(3046) or
      Php_FunctionComp(3079) or
      Php_FunctionComp(3130) or
      Php_FunctionComp(3222) or
      Php_FunctionComp(3295) or
      Php_FunctionComp(3375) or
      Php_FunctionComp(3387) or
      Php_FunctionComp(3518) or
      Php_FunctionComp(3611) or
      Php_FunctionComp(3704) or
      Php_FunctionComp(4621) or
      Php_FunctionComp(4784) or
      Php_FunctionComp(4802) or
      Php_FunctionComp(5108) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc159: TSynWebTokenKind;
begin
  if  Php_FunctionComp(77) or
      Php_FunctionComp(278) or
      Php_FunctionComp(823) or
      Php_FunctionComp(925) or
      Php_FunctionComp(1028) or
      Php_FunctionComp(1179) or
      Php_FunctionComp(1361) or
      Php_FunctionComp(1499) or
      Php_FunctionComp(1629) or
      Php_FunctionComp(1756) or
      Php_FunctionComp(1928) or
      Php_FunctionComp(1941) or
      Php_FunctionComp(2134) or
      Php_FunctionComp(2421) or
      Php_FunctionComp(2576) or
      Php_FunctionComp(2605) or
      Php_FunctionComp(2634) or
      Php_FunctionComp(2923) or
      Php_FunctionComp(3070) or
      Php_FunctionComp(3113) or
      Php_FunctionComp(3175) or
      Php_FunctionComp(3457) or
      Php_FunctionComp(3561) or
      Php_FunctionComp(3681) or
      Php_FunctionComp(3700) or
      Php_FunctionComp(4074) or
      Php_FunctionComp(4087) or
      Php_FunctionComp(4352) or
      Php_FunctionComp(4519) or
      Php_FunctionComp(4572) or
      Php_FunctionComp(4674) or
      Php_FunctionComp(4831) or
      Php_FunctionComp(5101) or
      Php_FunctionComp(5131) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc160: TSynWebTokenKind;
begin
  if  Php_FunctionComp(228) or
      Php_FunctionComp(245) or
      Php_FunctionComp(532) or
      Php_FunctionComp(610) or
      Php_FunctionComp(1062) or
      Php_FunctionComp(1229) or
      Php_FunctionComp(1407) or
      Php_FunctionComp(1563) or
      Php_FunctionComp(1604) or
      Php_FunctionComp(1806) or
      Php_FunctionComp(1937) or
      Php_FunctionComp(1980) or
      Php_FunctionComp(2000) or
      Php_FunctionComp(2065) or
      Php_FunctionComp(2268) or
      Php_FunctionComp(2278) or
      Php_FunctionComp(2324) or
      Php_FunctionComp(2369) or
      Php_FunctionComp(2692) or
      Php_FunctionComp(2698) or
      Php_FunctionComp(2867) or
      Php_FunctionComp(3018) or
      Php_FunctionComp(3045) or
      Php_FunctionComp(3515) or
      Php_FunctionComp(3523) or
      Php_FunctionComp(3568) or
      Php_FunctionComp(3623) or
      Php_FunctionComp(3660) or
      Php_FunctionComp(3667) or
      Php_FunctionComp(3718) or
      Php_FunctionComp(3857) or
      Php_FunctionComp(3881) or
      Php_FunctionComp(3998) or
      Php_FunctionComp(4268) or
      Php_FunctionComp(4273) or
      Php_FunctionComp(4303) or
      Php_FunctionComp(4574) or
      Php_FunctionComp(4764) or
      Php_FunctionComp(4771) or
      Php_FunctionComp(4932) or
      Php_FunctionComp(4985) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc161: TSynWebTokenKind;
begin
  if  Php_FunctionComp(84) or
      Php_FunctionComp(107) or
      Php_FunctionComp(262) or
      Php_FunctionComp(323) or
      Php_FunctionComp(410) or
      Php_FunctionComp(470) or
      Php_FunctionComp(543) or
      Php_FunctionComp(597) or
      Php_FunctionComp(696) or
      Php_FunctionComp(1033) or
      Php_FunctionComp(1159) or
      Php_FunctionComp(1197) or
      Php_FunctionComp(1722) or
      Php_FunctionComp(1825) or
      Php_FunctionComp(1889) or
      Php_FunctionComp(1902) or
      Php_FunctionComp(2210) or
      Php_FunctionComp(2283) or
      Php_FunctionComp(2556) or
      Php_FunctionComp(2565) or
      Php_FunctionComp(2743) or
      Php_FunctionComp(2847) or
      Php_FunctionComp(2896) or
      Php_FunctionComp(2975) or
      Php_FunctionComp(2982) or
      Php_FunctionComp(3254) or
      Php_FunctionComp(3682) or
      Php_FunctionComp(3975) or
      Php_FunctionComp(4196) or
      Php_FunctionComp(4260) or
      Php_FunctionComp(4626) or
      Php_FunctionComp(4746) or
      Php_FunctionComp(4828) or
      Php_FunctionComp(4957) or
      Php_FunctionComp(4965) or
      Php_FunctionComp(5166) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc162: TSynWebTokenKind;
begin
  if  Php_FunctionComp(89) or
      Php_FunctionComp(305) or
      Php_FunctionComp(559) or
      Php_FunctionComp(879) or
      Php_FunctionComp(1130) or
      Php_FunctionComp(1155) or
      Php_FunctionComp(1233) or
      Php_FunctionComp(1392) or
      Php_FunctionComp(1780) or
      Php_FunctionComp(1794) or
      Php_FunctionComp(1816) or
      Php_FunctionComp(2037) or
      Php_FunctionComp(2113) or
      Php_FunctionComp(2166) or
      Php_FunctionComp(2189) or
      Php_FunctionComp(2460) or
      Php_FunctionComp(2547) or
      Php_FunctionComp(2583) or
      Php_FunctionComp(2709) or
      Php_FunctionComp(2710) or
      Php_FunctionComp(2917) or
      Php_FunctionComp(2919) or
      Php_FunctionComp(3050) or
      Php_FunctionComp(3056) or
      Php_FunctionComp(3765) or
      Php_FunctionComp(4214) or
      Php_FunctionComp(4280) or
      Php_FunctionComp(4360) or
      Php_FunctionComp(4424) or
      Php_FunctionComp(4579) or
      Php_FunctionComp(4623) or
      Php_FunctionComp(4656) or
      Php_FunctionComp(4800) or
      Php_FunctionComp(5062) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc163: TSynWebTokenKind;
begin
  if  Php_FunctionComp(91) or
      Php_FunctionComp(316) or
      Php_FunctionComp(321) or
      Php_FunctionComp(467) or
      Php_FunctionComp(473) or
      Php_FunctionComp(800) or
      Php_FunctionComp(1053) or
      Php_FunctionComp(1357) or
      Php_FunctionComp(1486) or
      Php_FunctionComp(1542) or
      Php_FunctionComp(1611) or
      Php_FunctionComp(2112) or
      Php_FunctionComp(2640) or
      Php_FunctionComp(2655) or
      Php_FunctionComp(3135) or
      Php_FunctionComp(3180) or
      Php_FunctionComp(3239) or
      Php_FunctionComp(3680) or
      Php_FunctionComp(3885) or
      Php_FunctionComp(4019) or
      Php_FunctionComp(4363) or
      Php_FunctionComp(4575) or
      Php_FunctionComp(4610) or
      Php_FunctionComp(4781) or
      Php_FunctionComp(4787) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc164: TSynWebTokenKind;
begin
  if  Php_FunctionComp(435) or
      Php_FunctionComp(520) or
      Php_FunctionComp(655) or
      Php_FunctionComp(1478) or
      Php_FunctionComp(1535) or
      Php_FunctionComp(1753) or
      Php_FunctionComp(1793) or
      Php_FunctionComp(2084) or
      Php_FunctionComp(2645) or
      Php_FunctionComp(2702) or
      Php_FunctionComp(2828) or
      Php_FunctionComp(3161) or
      Php_FunctionComp(3409) or
      Php_FunctionComp(3487) or
      Php_FunctionComp(3748) or
      Php_FunctionComp(3768) or
      Php_FunctionComp(3801) or
      Php_FunctionComp(4077) or
      Php_FunctionComp(4285) or
      Php_FunctionComp(4346) or
      Php_FunctionComp(4560) or
      Php_FunctionComp(4670) or
      Php_FunctionComp(4807) or
      Php_FunctionComp(4852) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc165: TSynWebTokenKind;
begin
  if  Php_FunctionComp(635) or
      Php_FunctionComp(1319) or
      Php_FunctionComp(1749) or
      Php_FunctionComp(1976) or
      Php_FunctionComp(2060) or
      Php_FunctionComp(2225) or
      Php_FunctionComp(2425) or
      Php_FunctionComp(2536) or
      Php_FunctionComp(2573) or
      Php_FunctionComp(2589) or
      Php_FunctionComp(2752) or
      Php_FunctionComp(2795) or
      Php_FunctionComp(2838) or
      Php_FunctionComp(2985) or
      Php_FunctionComp(3253) or
      Php_FunctionComp(3327) or
      Php_FunctionComp(4494) or
      Php_FunctionComp(4511) or
      Php_FunctionComp(4683) or
      Php_FunctionComp(4825) or
      Php_FunctionComp(4848) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc166: TSynWebTokenKind;
begin
  if  Php_FunctionComp(210) or
      Php_FunctionComp(239) or
      Php_FunctionComp(301) or
      Php_FunctionComp(391) or
      Php_FunctionComp(509) or
      Php_FunctionComp(637) or
      Php_FunctionComp(660) or
      Php_FunctionComp(762) or
      Php_FunctionComp(895) or
      Php_FunctionComp(940) or
      Php_FunctionComp(1083) or
      Php_FunctionComp(1127) or
      Php_FunctionComp(1190) or
      Php_FunctionComp(1199) or
      Php_FunctionComp(1377) or
      Php_FunctionComp(1415) or
      Php_FunctionComp(1484) or
      Php_FunctionComp(1545) or
      Php_FunctionComp(1591) or
      Php_FunctionComp(1772) or
      Php_FunctionComp(1962) or
      Php_FunctionComp(2021) or
      Php_FunctionComp(2186) or
      Php_FunctionComp(2363) or
      Php_FunctionComp(2631) or
      Php_FunctionComp(2677) or
      Php_FunctionComp(2864) or
      Php_FunctionComp(2895) or
      Php_FunctionComp(2911) or
      Php_FunctionComp(3224) or
      Php_FunctionComp(3542) or
      Php_FunctionComp(3630) or
      Php_FunctionComp(3678) or
      Php_FunctionComp(3747) or
      Php_FunctionComp(3980) or
      Php_FunctionComp(4748) or
      Php_FunctionComp(4810) or
      Php_FunctionComp(5164) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc167: TSynWebTokenKind;
begin
  if  Php_FunctionComp(114) or
      Php_FunctionComp(302) or
      Php_FunctionComp(310) or
      Php_FunctionComp(1539) or
      Php_FunctionComp(1764) or
      Php_FunctionComp(1828) or
      Php_FunctionComp(1863) or
      Php_FunctionComp(1960) or
      Php_FunctionComp(2243) or
      Php_FunctionComp(2299) or
      Php_FunctionComp(2356) or
      Php_FunctionComp(2706) or
      Php_FunctionComp(2846) or
      Php_FunctionComp(3502) or
      Php_FunctionComp(3642) or
      Php_FunctionComp(3735) or
      Php_FunctionComp(3799) or
      Php_FunctionComp(3970) or
      Php_FunctionComp(4066) or
      Php_FunctionComp(4136) or
      Php_FunctionComp(4365) or
      Php_FunctionComp(4375) or
      Php_FunctionComp(4909) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc168: TSynWebTokenKind;
begin
  if  Php_FunctionComp(87) or
      Php_FunctionComp(161) or
      Php_FunctionComp(261) or
      Php_FunctionComp(363) or
      Php_FunctionComp(542) or
      Php_FunctionComp(586) or
      Php_FunctionComp(667) or
      Php_FunctionComp(671) or
      Php_FunctionComp(913) or
      Php_FunctionComp(1037) or
      Php_FunctionComp(1044) or
      Php_FunctionComp(1606) or
      Php_FunctionComp(1610) or
      Php_FunctionComp(1641) or
      Php_FunctionComp(1692) or
      Php_FunctionComp(1809) or
      Php_FunctionComp(1904) or
      Php_FunctionComp(2386) or
      Php_FunctionComp(2622) or
      Php_FunctionComp(2739) or
      Php_FunctionComp(2860) or
      Php_FunctionComp(2882) or
      Php_FunctionComp(2914) or
      Php_FunctionComp(2922) or
      Php_FunctionComp(3019) or
      Php_FunctionComp(3661) or
      Php_FunctionComp(3771) or
      Php_FunctionComp(3875) or
      Php_FunctionComp(3939) or
      Php_FunctionComp(4078) or
      Php_FunctionComp(4329) or
      Php_FunctionComp(4492) or
      Php_FunctionComp(4513) or
      Php_FunctionComp(4735) or
      Php_FunctionComp(4926) or
      Php_FunctionComp(5059) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc169: TSynWebTokenKind;
begin
  if  Php_FunctionComp(104) or
      Php_FunctionComp(1110) or
      Php_FunctionComp(1541) or
      Php_FunctionComp(1781) or
      Php_FunctionComp(2027) or
      Php_FunctionComp(2309) or
      Php_FunctionComp(2740) or
      Php_FunctionComp(2753) or
      Php_FunctionComp(2791) or
      Php_FunctionComp(2871) or
      Php_FunctionComp(3110) or
      Php_FunctionComp(3171) or
      Php_FunctionComp(3489) or
      Php_FunctionComp(3848) or
      Php_FunctionComp(4015) or
      Php_FunctionComp(4252) or
      Php_FunctionComp(4259) or
      Php_FunctionComp(4364) or
      Php_FunctionComp(4580) or
      Php_FunctionComp(4596) or
      Php_FunctionComp(4707) or
      Php_FunctionComp(4736) or
      Php_FunctionComp(4830) or
      Php_FunctionComp(4832) or
      Php_FunctionComp(4905) or
      Php_FunctionComp(5078) or
      Php_FunctionComp(5145) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc170: TSynWebTokenKind;
begin
  if  Php_FunctionComp(81) or
      Php_FunctionComp(139) or
      Php_FunctionComp(203) or
      Php_FunctionComp(734) or
      Php_FunctionComp(735) or
      Php_FunctionComp(801) or
      Php_FunctionComp(1121) or
      Php_FunctionComp(1389) or
      Php_FunctionComp(1765) or
      Php_FunctionComp(1778) or
      Php_FunctionComp(2028) or
      Php_FunctionComp(2379) or
      Php_FunctionComp(2676) or
      Php_FunctionComp(2926) or
      Php_FunctionComp(2984) or
      Php_FunctionComp(2986) or
      Php_FunctionComp(3015) or
      Php_FunctionComp(3369) or
      Php_FunctionComp(3440) or
      Php_FunctionComp(3522) or
      Php_FunctionComp(3855) or
      Php_FunctionComp(3865) or
      Php_FunctionComp(4194) or
      Php_FunctionComp(4206) or
      Php_FunctionComp(4257) or
      Php_FunctionComp(4279) or
      Php_FunctionComp(4310) or
      Php_FunctionComp(4390) or
      Php_FunctionComp(4409) or
      Php_FunctionComp(4517) or
      Php_FunctionComp(4598) or
      Php_FunctionComp(4668) or
      Php_FunctionComp(4733) or
      Php_FunctionComp(5109) or
      Php_FunctionComp(5117) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc171: TSynWebTokenKind;
begin
  if  Php_FunctionComp(570) or
      Php_FunctionComp(744) or
      Php_FunctionComp(1176) or
      Php_FunctionComp(1181) or
      Php_FunctionComp(1378) or
      Php_FunctionComp(1487) or
      Php_FunctionComp(1637) or
      Php_FunctionComp(1679) or
      Php_FunctionComp(1752) or
      Php_FunctionComp(1814) or
      Php_FunctionComp(1905) or
      Php_FunctionComp(1926) or
      Php_FunctionComp(1972) or
      Php_FunctionComp(1991) or
      Php_FunctionComp(2180) or
      Php_FunctionComp(2233) or
      Php_FunctionComp(2270) or
      Php_FunctionComp(2334) or
      Php_FunctionComp(2451) or
      Php_FunctionComp(2597) or
      Php_FunctionComp(2599) or
      Php_FunctionComp(2719) or
      Php_FunctionComp(2763) or
      Php_FunctionComp(2843) or
      Php_FunctionComp(3105) or
      Php_FunctionComp(3109) or
      Php_FunctionComp(3162) or
      Php_FunctionComp(3302) or
      Php_FunctionComp(3420) or
      Php_FunctionComp(3829) or
      Php_FunctionComp(3914) or
      Php_FunctionComp(4103) or
      Php_FunctionComp(4133) or
      Php_FunctionComp(4261) or
      Php_FunctionComp(4317) or
      Php_FunctionComp(4398) or
      Php_FunctionComp(4822) or
      Php_FunctionComp(4971) or
      Php_FunctionComp(5082) or
      Php_FunctionComp(5128) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc172: TSynWebTokenKind;
begin
  if  Php_FunctionComp(163) or
      Php_FunctionComp(446) or
      Php_FunctionComp(533) or
      Php_FunctionComp(749) or
      Php_FunctionComp(1102) or
      Php_FunctionComp(1648) or
      Php_FunctionComp(2008) or
      Php_FunctionComp(2277) or
      Php_FunctionComp(2314) or
      Php_FunctionComp(2370) or
      Php_FunctionComp(2481) or
      Php_FunctionComp(2727) or
      Php_FunctionComp(2933) or
      Php_FunctionComp(3033) or
      Php_FunctionComp(3447) or
      Php_FunctionComp(3490) or
      Php_FunctionComp(3663) or
      Php_FunctionComp(3665) or
      Php_FunctionComp(3876) or
      Php_FunctionComp(3889) or
      Php_FunctionComp(4064) or
      Php_FunctionComp(4156) or
      Php_FunctionComp(4208) or
      Php_FunctionComp(4518) or
      Php_FunctionComp(4613) or
      Php_FunctionComp(4940) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc173: TSynWebTokenKind;
begin
  if  Php_KeywordComp(55) then
    Result := stkPhpKeyword
  else
    if  Php_FunctionComp(324) or
        Php_FunctionComp(742) or
        Php_FunctionComp(753) or
        Php_FunctionComp(908) or
        Php_FunctionComp(942) or
        Php_FunctionComp(1702) or
        Php_FunctionComp(1921) or
        Php_FunctionComp(1973) or
        Php_FunctionComp(2226) or
        Php_FunctionComp(2257) or
        Php_FunctionComp(2340) or
        Php_FunctionComp(2601) or
        Php_FunctionComp(2613) or
        Php_FunctionComp(2756) or
        Php_FunctionComp(2878) or
        Php_FunctionComp(2883) or
        Php_FunctionComp(2897) or
        Php_FunctionComp(3030) or
        Php_FunctionComp(3037) or
        Php_FunctionComp(3089) or
        Php_FunctionComp(3152) or
        Php_FunctionComp(3243) or
        Php_FunctionComp(3544) or
        Php_FunctionComp(3788) or
        Php_FunctionComp(4104) or
        Php_FunctionComp(4267) or
        Php_FunctionComp(4471) or
        Php_FunctionComp(4816) or
        Php_FunctionComp(4900) or
        Php_FunctionComp(5129) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc174: TSynWebTokenKind;
begin
  if  Php_FunctionComp(109) or
      Php_FunctionComp(745) or
      Php_FunctionComp(755) or
      Php_FunctionComp(831) or
      Php_FunctionComp(889) or
      Php_FunctionComp(1231) or
      Php_FunctionComp(1242) or
      Php_FunctionComp(1335) or
      Php_FunctionComp(1494) or
      Php_FunctionComp(1555) or
      Php_FunctionComp(2029) or
      Php_FunctionComp(2034) or
      Php_FunctionComp(2310) or
      Php_FunctionComp(2474) or
      Php_FunctionComp(2554) or
      Php_FunctionComp(2615) or
      Php_FunctionComp(2786) or
      Php_FunctionComp(3077) or
      Php_FunctionComp(3370) or
      Php_FunctionComp(3519) or
      Php_FunctionComp(3983) or
      Php_FunctionComp(4744) or
      Php_FunctionComp(5005) or
      Php_FunctionComp(5064) or
      Php_FunctionComp(5114) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc175: TSynWebTokenKind;
begin
  if  Php_FunctionComp(159) or
      Php_FunctionComp(741) or
      Php_FunctionComp(1177) or
      Php_FunctionComp(1301) or
      Php_FunctionComp(1337) or
      Php_FunctionComp(1374) or
      Php_FunctionComp(1527) or
      Php_FunctionComp(1715) or
      Php_FunctionComp(2035) or
      Php_FunctionComp(2179) or
      Php_FunctionComp(2221) or
      Php_FunctionComp(2678) or
      Php_FunctionComp(3053) or
      Php_FunctionComp(3727) or
      Php_FunctionComp(4527) or
      Php_FunctionComp(4587) or
      Php_FunctionComp(4618) or
      Php_FunctionComp(4961) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc176: TSynWebTokenKind;
begin
  if  Php_FunctionComp(215) or
      Php_FunctionComp(540) or
      Php_FunctionComp(546) or
      Php_FunctionComp(562) or
      Php_FunctionComp(572) or
      Php_FunctionComp(608) or
      Php_FunctionComp(912) or
      Php_FunctionComp(1424) or
      Php_FunctionComp(1507) or
      Php_FunctionComp(1666) or
      Php_FunctionComp(1829) or
      Php_FunctionComp(1869) or
      Php_FunctionComp(1891) or
      Php_FunctionComp(2172) or
      Php_FunctionComp(2643) or
      Php_FunctionComp(2725) or
      Php_FunctionComp(2836) or
      Php_FunctionComp(2863) or
      Php_FunctionComp(2910) or
      Php_FunctionComp(3174) or
      Php_FunctionComp(3259) or
      Php_FunctionComp(3711) or
      Php_FunctionComp(3791) or
      Php_FunctionComp(3835) or
      Php_FunctionComp(4140) or
      Php_FunctionComp(4321) or
      Php_FunctionComp(4389) or
      Php_FunctionComp(4622) or
      Php_FunctionComp(4732) or
      Php_FunctionComp(4907) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc177: TSynWebTokenKind;
begin
  if  Php_FunctionComp(79) or
      Php_FunctionComp(195) or
      Php_FunctionComp(306) or
      Php_FunctionComp(401) or
      Php_FunctionComp(441) or
      Php_FunctionComp(471) or
      Php_FunctionComp(897) or
      Php_FunctionComp(931) or
      Php_FunctionComp(950) or
      Php_FunctionComp(1004) or
      Php_FunctionComp(1330) or
      Php_FunctionComp(1826) or
      Php_FunctionComp(1890) or
      Php_FunctionComp(2026) or
      Php_FunctionComp(2051) or
      Php_FunctionComp(2282) or
      Php_FunctionComp(2660) or
      Php_FunctionComp(2870) or
      Php_FunctionComp(2912) or
      Php_FunctionComp(2968) or
      Php_FunctionComp(2988) or
      Php_FunctionComp(3002) or
      Php_FunctionComp(3133) or
      Php_FunctionComp(3179) or
      Php_FunctionComp(3435) or
      Php_FunctionComp(3533) or
      Php_FunctionComp(3608) or
      Php_FunctionComp(3673) or
      Php_FunctionComp(3707) or
      Php_FunctionComp(3921) or
      Php_FunctionComp(4414) or
      Php_FunctionComp(4654) or
      Php_FunctionComp(4827) or
      Php_FunctionComp(4871) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc178: TSynWebTokenKind;
begin
  if  Php_FunctionComp(385) or
      Php_FunctionComp(739) or
      Php_FunctionComp(882) or
      Php_FunctionComp(1009) or
      Php_FunctionComp(1042) or
      Php_FunctionComp(1333) or
      Php_FunctionComp(1670) or
      Php_FunctionComp(1701) or
      Php_FunctionComp(1776) or
      Php_FunctionComp(1894) or
      Php_FunctionComp(2048) or
      Php_FunctionComp(2130) or
      Php_FunctionComp(2135) or
      Php_FunctionComp(2178) or
      Php_FunctionComp(2486) or
      Php_FunctionComp(2661) or
      Php_FunctionComp(2695) or
      Php_FunctionComp(2936) or
      Php_FunctionComp(3026) or
      Php_FunctionComp(3112) or
      Php_FunctionComp(3173) or
      Php_FunctionComp(3303) or
      Php_FunctionComp(3335) or
      Php_FunctionComp(3839) or
      Php_FunctionComp(4047) or
      Php_FunctionComp(4258) or
      Php_FunctionComp(4535) or
      Php_FunctionComp(4552) or
      Php_FunctionComp(4665) or
      Php_FunctionComp(4834) or
      Php_FunctionComp(4911) or
      Php_FunctionComp(4917) or
      Php_FunctionComp(4938) or
      Php_FunctionComp(4983) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc179: TSynWebTokenKind;
begin
  if  Php_FunctionComp(116) or
      Php_FunctionComp(265) or
      Php_FunctionComp(613) or
      Php_FunctionComp(1180) or
      Php_FunctionComp(1284) or
      Php_FunctionComp(1517) or
      Php_FunctionComp(1824) or
      Php_FunctionComp(2129) or
      Php_FunctionComp(2423) or
      Php_FunctionComp(2502) or
      Php_FunctionComp(2580) or
      Php_FunctionComp(2657) or
      Php_FunctionComp(2837) or
      Php_FunctionComp(2893) or
      Php_FunctionComp(3043) or
      Php_FunctionComp(3049) or
      Php_FunctionComp(3081) or
      Php_FunctionComp(3373) or
      Php_FunctionComp(3804) or
      Php_FunctionComp(3919) or
      Php_FunctionComp(3987) or
      Php_FunctionComp(4049) or
      Php_FunctionComp(4195) or
      Php_FunctionComp(4314) or
      Php_FunctionComp(4348) or
      Php_FunctionComp(4359) or
      Php_FunctionComp(4583) or
      Php_FunctionComp(4937) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc180: TSynWebTokenKind;
begin
  if  Php_FunctionComp(359) or
      Php_FunctionComp(478) or
      Php_FunctionComp(770) or
      Php_FunctionComp(985) or
      Php_FunctionComp(1055) or
      Php_FunctionComp(1716) or
      Php_FunctionComp(1719) or
      Php_FunctionComp(1884) or
      Php_FunctionComp(2081) or
      Php_FunctionComp(2438) or
      Php_FunctionComp(2644) or
      Php_FunctionComp(2764) or
      Php_FunctionComp(2877) or
      Php_FunctionComp(2898) or
      Php_FunctionComp(2952) or
      Php_FunctionComp(2960) or
      Php_FunctionComp(3034) or
      Php_FunctionComp(3140) or
      Php_FunctionComp(3185) or
      Php_FunctionComp(3336) or
      Php_FunctionComp(3499) or
      Php_FunctionComp(3647) or
      Php_FunctionComp(3969) or
      Php_FunctionComp(3990) or
      Php_FunctionComp(4051) or
      Php_FunctionComp(4456) or
      Php_FunctionComp(4547) or
      Php_FunctionComp(4563) or
      Php_FunctionComp(4590) or
      Php_FunctionComp(4597) or
      Php_FunctionComp(4795) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc181: TSynWebTokenKind;
begin
  if  Php_FunctionComp(105) or
      Php_FunctionComp(149) or
      Php_FunctionComp(512) or
      Php_FunctionComp(518) or
      Php_FunctionComp(993) or
      Php_FunctionComp(1140) or
      Php_FunctionComp(1255) or
      Php_FunctionComp(1560) or
      Php_FunctionComp(1631) or
      Php_FunctionComp(1694) or
      Php_FunctionComp(1823) or
      Php_FunctionComp(2187) or
      Php_FunctionComp(2276) or
      Php_FunctionComp(2323) or
      Php_FunctionComp(2332) or
      Php_FunctionComp(2729) or
      Php_FunctionComp(2787) or
      Php_FunctionComp(3685) or
      Php_FunctionComp(3860) or
      Php_FunctionComp(4296) or
      Php_FunctionComp(4306) or
      Php_FunctionComp(4361) or
      Php_FunctionComp(4600) or
      Php_FunctionComp(4620) or
      Php_FunctionComp(4685) or
      Php_FunctionComp(4695) or
      Php_FunctionComp(4770) or
      Php_FunctionComp(4914) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc182: TSynWebTokenKind;
begin
  if  Php_FunctionComp(74) or
      Php_FunctionComp(122) or
      Php_FunctionComp(322) or
      Php_FunctionComp(507) or
      Php_FunctionComp(1115) or
      Php_FunctionComp(1338) or
      Php_FunctionComp(1530) or
      Php_FunctionComp(1532) or
      Php_FunctionComp(1578) or
      Php_FunctionComp(1580) or
      Php_FunctionComp(1795) or
      Php_FunctionComp(2343) or
      Php_FunctionComp(2380) or
      Php_FunctionComp(2662) or
      Php_FunctionComp(2665) or
      Php_FunctionComp(2829) or
      Php_FunctionComp(2913) or
      Php_FunctionComp(3012) or
      Php_FunctionComp(3226) or
      Php_FunctionComp(3270) or
      Php_FunctionComp(3527) or
      Php_FunctionComp(3668) or
      Php_FunctionComp(4254) or
      Php_FunctionComp(4347) or
      Php_FunctionComp(4393) or
      Php_FunctionComp(4850) or
      Php_FunctionComp(4980) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc183: TSynWebTokenKind;
begin
  if  Php_FunctionComp(125) or
      Php_FunctionComp(508) or
      Php_FunctionComp(592) or
      Php_FunctionComp(893) or
      Php_FunctionComp(1008) or
      Php_FunctionComp(1162) or
      Php_FunctionComp(1254) or
      Php_FunctionComp(1256) or
      Php_FunctionComp(1320) or
      Php_FunctionComp(1561) or
      Php_FunctionComp(1623) or
      Php_FunctionComp(2341) or
      Php_FunctionComp(2372) or
      Php_FunctionComp(2390) or
      Php_FunctionComp(2399) or
      Php_FunctionComp(2428) or
      Php_FunctionComp(2439) or
      Php_FunctionComp(2559) or
      Php_FunctionComp(2611) or
      Php_FunctionComp(2612) or
      Php_FunctionComp(2674) or
      Php_FunctionComp(2704) or
      Php_FunctionComp(2859) or
      Php_FunctionComp(2945) or
      Php_FunctionComp(3296) or
      Php_FunctionComp(3628) or
      Php_FunctionComp(3800) or
      Php_FunctionComp(4176) or
      Php_FunctionComp(4541) or
      Php_FunctionComp(4689) or
      Php_FunctionComp(4847) or
      Php_FunctionComp(4885) or
      Php_FunctionComp(5069) or
      Php_FunctionComp(5165) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc184: TSynWebTokenKind;
begin
  if  Php_FunctionComp(872) or
      Php_FunctionComp(894) or
      Php_FunctionComp(1038) or
      Php_FunctionComp(1112) or
      Php_FunctionComp(1282) or
      Php_FunctionComp(1393) or
      Php_FunctionComp(1837) or
      Php_FunctionComp(1925) or
      Php_FunctionComp(2165) or
      Php_FunctionComp(2256) or
      Php_FunctionComp(2396) or
      Php_FunctionComp(2456) or
      Php_FunctionComp(2646) or
      Php_FunctionComp(2705) or
      Php_FunctionComp(2718) or
      Php_FunctionComp(2967) or
      Php_FunctionComp(2981) or
      Php_FunctionComp(2993) or
      Php_FunctionComp(3027) or
      Php_FunctionComp(3231) or
      Php_FunctionComp(3328) or
      Php_FunctionComp(3453) or
      Php_FunctionComp(3730) or
      Php_FunctionComp(4967) or
      Php_FunctionComp(4968) or
      Php_FunctionComp(5004) or
      Php_FunctionComp(5136) or
      Php_FunctionComp(5155) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc185: TSynWebTokenKind;
begin
  if  Php_FunctionComp(82) or
      Php_FunctionComp(92) or
      Php_FunctionComp(115) or
      Php_FunctionComp(752) or
      Php_FunctionComp(983) or
      Php_FunctionComp(1047) or
      Php_FunctionComp(1171) or
      Php_FunctionComp(1596) or
      Php_FunctionComp(1599) or
      Php_FunctionComp(1697) or
      Php_FunctionComp(1877) or
      Php_FunctionComp(1994) or
      Php_FunctionComp(2295) or
      Php_FunctionComp(2413) or
      Php_FunctionComp(2548) or
      Php_FunctionComp(2656) or
      Php_FunctionComp(2701) or
      Php_FunctionComp(2840) or
      Php_FunctionComp(2865) or
      Php_FunctionComp(2958) or
      Php_FunctionComp(2972) or
      Php_FunctionComp(3290) or
      Php_FunctionComp(3329) or
      Php_FunctionComp(3346) or
      Php_FunctionComp(3819) or
      Php_FunctionComp(3831) or
      Php_FunctionComp(4324) or
      Php_FunctionComp(4417) or
      Php_FunctionComp(4428) or
      Php_FunctionComp(4582) or
      Php_FunctionComp(4772) or
      Php_FunctionComp(4936) or
      Php_FunctionComp(5139) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc186: TSynWebTokenKind;
begin
  if  Php_FunctionComp(445) or
      Php_FunctionComp(722) or
      Php_FunctionComp(733) or
      Php_FunctionComp(1076) or
      Php_FunctionComp(1141) or
      Php_FunctionComp(1844) or
      Php_FunctionComp(1956) or
      Php_FunctionComp(2289) or
      Php_FunctionComp(2381) or
      Php_FunctionComp(2412) or
      Php_FunctionComp(2487) or
      Php_FunctionComp(2588) or
      Php_FunctionComp(2619) or
      Php_FunctionComp(2623) or
      Php_FunctionComp(3170) or
      Php_FunctionComp(3238) or
      Php_FunctionComp(3240) or
      Php_FunctionComp(3439) or
      Php_FunctionComp(3706) or
      Php_FunctionComp(3710) or
      Php_FunctionComp(3815) or
      Php_FunctionComp(3825) or
      Php_FunctionComp(4182) or
      Php_FunctionComp(4199) or
      Php_FunctionComp(4247) or
      Php_FunctionComp(4255) or
      Php_FunctionComp(4382) or
      Php_FunctionComp(4581) or
      Php_FunctionComp(4594) or
      Php_FunctionComp(4918) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc187: TSynWebTokenKind;
begin
  if  Php_FunctionComp(240) or
      Php_FunctionComp(645) or
      Php_FunctionComp(905) or
      Php_FunctionComp(953) or
      Php_FunctionComp(1103) or
      Php_FunctionComp(1587) or
      Php_FunctionComp(1831) or
      Php_FunctionComp(1883) or
      Php_FunctionComp(1903) or
      Php_FunctionComp(1938) or
      Php_FunctionComp(2031) or
      Php_FunctionComp(2164) or
      Php_FunctionComp(2181) or
      Php_FunctionComp(2443) or
      Php_FunctionComp(2587) or
      Php_FunctionComp(2841) or
      Php_FunctionComp(2920) or
      Php_FunctionComp(3564) or
      Php_FunctionComp(3702) or
      Php_FunctionComp(4109) or
      Php_FunctionComp(4220) or
      Php_FunctionComp(4312) or
      Php_FunctionComp(4614) or
      Php_FunctionComp(4661) or
      Php_FunctionComp(4671) or
      Php_FunctionComp(4692) or
      Php_FunctionComp(4697) or
      Php_FunctionComp(5160) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc188: TSynWebTokenKind;
begin
  if  Php_FunctionComp(192) or
      Php_FunctionComp(309) or
      Php_FunctionComp(381) or
      Php_FunctionComp(528) or
      Php_FunctionComp(584) or
      Php_FunctionComp(904) or
      Php_FunctionComp(1403) or
      Php_FunctionComp(1779) or
      Php_FunctionComp(1854) or
      Php_FunctionComp(2114) or
      Php_FunctionComp(2317) or
      Php_FunctionComp(2326) or
      Php_FunctionComp(2711) or
      Php_FunctionComp(2890) or
      Php_FunctionComp(2964) or
      Php_FunctionComp(2977) or
      Php_FunctionComp(3017) or
      Php_FunctionComp(3111) or
      Php_FunctionComp(3193) or
      Php_FunctionComp(3263) or
      Php_FunctionComp(3669) or
      Php_FunctionComp(3852) or
      Php_FunctionComp(3888) or
      Php_FunctionComp(4210) or
      Php_FunctionComp(4362) or
      Php_FunctionComp(4367) or
      Php_FunctionComp(4388) or
      Php_FunctionComp(4780) or
      Php_FunctionComp(4916) or
      Php_FunctionComp(4933) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc189: TSynWebTokenKind;
begin
  if  Php_FunctionComp(88) or
      Php_FunctionComp(264) or
      Php_FunctionComp(484) or
      Php_FunctionComp(612) or
      Php_FunctionComp(1022) or
      Php_FunctionComp(1536) or
      Php_FunctionComp(1564) or
      Php_FunctionComp(1579) or
      Php_FunctionComp(1650) or
      Php_FunctionComp(1871) or
      Php_FunctionComp(2275) or
      Php_FunctionComp(2616) or
      Php_FunctionComp(2723) or
      Php_FunctionComp(2803) or
      Php_FunctionComp(2918) or
      Php_FunctionComp(3271) or
      Php_FunctionComp(3462) or
      Php_FunctionComp(3503) or
      Php_FunctionComp(3646) or
      Php_FunctionComp(4851) or
      Php_FunctionComp(4928) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc190: TSynWebTokenKind;
begin
  if  Php_FunctionComp(254) or
      Php_FunctionComp(315) or
      Php_FunctionComp(451) or
      Php_FunctionComp(877) or
      Php_FunctionComp(1252) or
      Php_FunctionComp(1278) or
      Php_FunctionComp(1460) or
      Php_FunctionComp(1612) or
      Php_FunctionComp(1665) or
      Php_FunctionComp(1700) or
      Php_FunctionComp(1916) or
      Php_FunctionComp(2382) or
      Php_FunctionComp(2426) or
      Php_FunctionComp(2641) or
      Php_FunctionComp(2835) or
      Php_FunctionComp(3343) or
      Php_FunctionComp(3356) or
      Php_FunctionComp(3505) or
      Php_FunctionComp(3989) or
      Php_FunctionComp(4056) or
      Php_FunctionComp(4315) or
      Php_FunctionComp(4433) or
      Php_FunctionComp(4448) or
      Php_FunctionComp(4785) or
      Php_FunctionComp(5023) or
      Php_FunctionComp(5111) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc191: TSynWebTokenKind;
begin
  if  Php_FunctionComp(611) or
      Php_FunctionComp(670) or
      Php_FunctionComp(883) or
      Php_FunctionComp(885) or
      Php_FunctionComp(923) or
      Php_FunctionComp(1866) or
      Php_FunctionComp(1930) or
      Php_FunctionComp(2075) or
      Php_FunctionComp(2700) or
      Php_FunctionComp(2934) or
      Php_FunctionComp(2943) or
      Php_FunctionComp(3047) or
      Php_FunctionComp(3085) or
      Php_FunctionComp(3280) or
      Php_FunctionComp(3411) or
      Php_FunctionComp(3806) or
      Php_FunctionComp(3826) or
      Php_FunctionComp(3911) or
      Php_FunctionComp(4009) or
      Php_FunctionComp(4766) or
      Php_FunctionComp(4809) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc192: TSynWebTokenKind;
begin
  if  Php_FunctionComp(177) or
      Php_FunctionComp(458) or
      Php_FunctionComp(888) or
      Php_FunctionComp(960) or
      Php_FunctionComp(1086) or
      Php_FunctionComp(1429) or
      Php_FunctionComp(1562) or
      Php_FunctionComp(1895) or
      Php_FunctionComp(2059) or
      Php_FunctionComp(2303) or
      Php_FunctionComp(2318) or
      Php_FunctionComp(2365) or
      Php_FunctionComp(2422) or
      Php_FunctionComp(2606) or
      Php_FunctionComp(2758) or
      Php_FunctionComp(2760) or
      Php_FunctionComp(2928) or
      Php_FunctionComp(3157) or
      Php_FunctionComp(3609) or
      Php_FunctionComp(3982) or
      Php_FunctionComp(4044) or
      Php_FunctionComp(4152) or
      Php_FunctionComp(4629) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc193: TSynWebTokenKind;
begin
  if  Php_FunctionComp(453) or
      Php_FunctionComp(460) or
      Php_FunctionComp(514) or
      Php_FunctionComp(535) or
      Php_FunctionComp(720) or
      Php_FunctionComp(724) or
      Php_FunctionComp(973) or
      Php_FunctionComp(1005) or
      Php_FunctionComp(1676) or
      Php_FunctionComp(1882) or
      Php_FunctionComp(1893) or
      Php_FunctionComp(2128) or
      Php_FunctionComp(2458) or
      Php_FunctionComp(2761) or
      Php_FunctionComp(2776) or
      Php_FunctionComp(2942) or
      Php_FunctionComp(2954) or
      Php_FunctionComp(2974) or
      Php_FunctionComp(3372) or
      Php_FunctionComp(4200) or
      Php_FunctionComp(4222) or
      Php_FunctionComp(4377) or
      Php_FunctionComp(4407) or
      Php_FunctionComp(4476) or
      Php_FunctionComp(4483) or
      Php_FunctionComp(4556) or
      Php_FunctionComp(4684) or
      Php_FunctionComp(4941) or
      Php_FunctionComp(5018) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc194: TSynWebTokenKind;
begin
  if  Php_FunctionComp(144) or
      Php_FunctionComp(954) or
      Php_FunctionComp(1391) or
      Php_FunctionComp(1635) or
      Php_FunctionComp(1853) or
      Php_FunctionComp(1873) or
      Php_FunctionComp(2279) or
      Php_FunctionComp(2291) or
      Php_FunctionComp(2377) or
      Php_FunctionComp(2604) or
      Php_FunctionComp(2755) or
      Php_FunctionComp(2932) or
      Php_FunctionComp(3090) or
      Php_FunctionComp(3332) or
      Php_FunctionComp(3496) or
      Php_FunctionComp(3498) or
      Php_FunctionComp(3708) or
      Php_FunctionComp(4069) or
      Php_FunctionComp(4075) or
      Php_FunctionComp(4137) or
      Php_FunctionComp(4253) or
      Php_FunctionComp(4401) or
      Php_FunctionComp(4454) or
      Php_FunctionComp(4525) or
      Php_FunctionComp(4745) or
      Php_FunctionComp(4946) or
      Php_FunctionComp(5015) or
      Php_FunctionComp(5063) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc195: TSynWebTokenKind;
begin
  if  Php_FunctionComp(574) or
      Php_FunctionComp(932) or
      Php_FunctionComp(1068) or
      Php_FunctionComp(1302) or
      Php_FunctionComp(1496) or
      Php_FunctionComp(1706) or
      Php_FunctionComp(1879) or
      Php_FunctionComp(2036) or
      Php_FunctionComp(2389) or
      Php_FunctionComp(2888) or
      Php_FunctionComp(3091) or
      Php_FunctionComp(3337) or
      Php_FunctionComp(3456) or
      Php_FunctionComp(3486) or
      Php_FunctionComp(4611) or
      Php_FunctionComp(4824) or
      Php_FunctionComp(4897) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc196: TSynWebTokenKind;
begin
  if  Php_FunctionComp(963) or
      Php_FunctionComp(980) or
      Php_FunctionComp(1054) or
      Php_FunctionComp(1096) or
      Php_FunctionComp(1113) or
      Php_FunctionComp(1834) or
      Php_FunctionComp(1848) or
      Php_FunctionComp(1852) or
      Php_FunctionComp(2156) or
      Php_FunctionComp(2499) or
      Php_FunctionComp(2736) or
      Php_FunctionComp(2894) or
      Php_FunctionComp(2915) or
      Php_FunctionComp(3136) or
      Php_FunctionComp(3181) or
      Php_FunctionComp(3257) or
      Php_FunctionComp(3406) or
      Php_FunctionComp(3511) or
      Php_FunctionComp(3670) or
      Php_FunctionComp(4612) or
      Php_FunctionComp(4714) or
      Php_FunctionComp(4727) or
      Php_FunctionComp(4855) or
      Php_FunctionComp(5013) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc197: TSynWebTokenKind;
begin
  if  Php_FunctionComp(241) or
      Php_FunctionComp(326) or
      Php_FunctionComp(606) or
      Php_FunctionComp(970) or
      Php_FunctionComp(974) or
      Php_FunctionComp(1036) or
      Php_FunctionComp(1566) or
      Php_FunctionComp(1698) or
      Php_FunctionComp(1927) or
      Php_FunctionComp(2195) or
      Php_FunctionComp(2274) or
      Php_FunctionComp(2307) or
      Php_FunctionComp(2563) or
      Php_FunctionComp(2688) or
      Php_FunctionComp(2793) or
      Php_FunctionComp(2998) or
      Php_FunctionComp(3255) or
      Php_FunctionComp(3495) or
      Php_FunctionComp(4026) or
      Php_FunctionComp(4065) or
      Php_FunctionComp(4276) or
      Php_FunctionComp(4469) or
      Php_FunctionComp(4479) or
      Php_FunctionComp(4599) or
      Php_FunctionComp(4659) or
      Php_FunctionComp(4951) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc198: TSynWebTokenKind;
begin
  if  Php_FunctionComp(342) or
      Php_FunctionComp(455) or
      Php_FunctionComp(941) or
      Php_FunctionComp(1383) or
      Php_FunctionComp(1540) or
      Php_FunctionComp(1703) or
      Php_FunctionComp(1747) or
      Php_FunctionComp(1983) or
      Php_FunctionComp(2251) or
      Php_FunctionComp(2312) or
      Php_FunctionComp(2417) or
      Php_FunctionComp(2782) or
      Php_FunctionComp(2892) or
      Php_FunctionComp(2966) or
      Php_FunctionComp(3196) or
      Php_FunctionComp(3261) or
      Php_FunctionComp(3276) or
      Php_FunctionComp(3371) or
      Php_FunctionComp(3521) or
      Php_FunctionComp(3773) or
      Php_FunctionComp(3792) or
      Php_FunctionComp(3992) or
      Php_FunctionComp(4008) or
      Php_FunctionComp(4391) or
      Php_FunctionComp(4624) or
      Php_FunctionComp(4628) or
      Php_FunctionComp(4906) or
      Php_FunctionComp(4964) or
      Php_FunctionComp(5148) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc199: TSynWebTokenKind;
begin
  if  Php_FunctionComp(170) or
      Php_FunctionComp(420) or
      Php_FunctionComp(1072) or
      Php_FunctionComp(1088) or
      Php_FunctionComp(1243) or
      Php_FunctionComp(1309) or
      Php_FunctionComp(1774) or
      Php_FunctionComp(1820) or
      Php_FunctionComp(2220) or
      Php_FunctionComp(2383) or
      Php_FunctionComp(2853) or
      Php_FunctionComp(3247) or
      Php_FunctionComp(3283) or
      Php_FunctionComp(3508) or
      Php_FunctionComp(3524) or
      Php_FunctionComp(3723) or
      Php_FunctionComp(4186) or
      Php_FunctionComp(4192) or
      Php_FunctionComp(4262) or
      Php_FunctionComp(4386) or
      Php_FunctionComp(4558) or
      Php_FunctionComp(4662) or
      Php_FunctionComp(4666) or
      Php_FunctionComp(4667) or
      Php_FunctionComp(5169) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc200: TSynWebTokenKind;
begin
  if  Php_FunctionComp(220) or
      Php_FunctionComp(284) or
      Php_FunctionComp(433) or
      Php_FunctionComp(1395) or
      Php_FunctionComp(1593) or
      Php_FunctionComp(1598) or
      Php_FunctionComp(1965) or
      Php_FunctionComp(2167) or
      Php_FunctionComp(2211) or
      Php_FunctionComp(2246) or
      Php_FunctionComp(2598) or
      Php_FunctionComp(2754) or
      Php_FunctionComp(2909) or
      Php_FunctionComp(2948) or
      Php_FunctionComp(3472) or
      Php_FunctionComp(3725) or
      Php_FunctionComp(4062) or
      Php_FunctionComp(4076) or
      Php_FunctionComp(4207) or
      Php_FunctionComp(4218) or
      Php_FunctionComp(4425) or
      Php_FunctionComp(4679) or
      Php_FunctionComp(4738) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc201: TSynWebTokenKind;
begin
  if  Php_FunctionComp(102) or
      Php_FunctionComp(136) or
      Php_FunctionComp(448) or
      Php_FunctionComp(472) or
      Php_FunctionComp(511) or
      Php_FunctionComp(837) or
      Php_FunctionComp(911) or
      Php_FunctionComp(1544) or
      Php_FunctionComp(1685) or
      Php_FunctionComp(1782) or
      Php_FunctionComp(2322) or
      Php_FunctionComp(2401) or
      Php_FunctionComp(2720) or
      Php_FunctionComp(2766) or
      Php_FunctionComp(2990) or
      Php_FunctionComp(2995) or
      Php_FunctionComp(3408) or
      Php_FunctionComp(3618) or
      Php_FunctionComp(3827) or
      Php_FunctionComp(4043) or
      Php_FunctionComp(4533) or
      Php_FunctionComp(4606) or
      Php_FunctionComp(4607) or
      Php_FunctionComp(4655) or
      Php_FunctionComp(4705) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc202: TSynWebTokenKind;
begin
  if  Php_FunctionComp(463) or
      Php_FunctionComp(474) or
      Php_FunctionComp(934) or
      Php_FunctionComp(1296) or
      Php_FunctionComp(1312) or
      Php_FunctionComp(1408) or
      Php_FunctionComp(1830) or
      Php_FunctionComp(1845) or
      Php_FunctionComp(1851) or
      Php_FunctionComp(2017) or
      Php_FunctionComp(2354) or
      Php_FunctionComp(2475) or
      Php_FunctionComp(2842) or
      Php_FunctionComp(2904) or
      Php_FunctionComp(2965) or
      Php_FunctionComp(3355) or
      Php_FunctionComp(3419) or
      Php_FunctionComp(3551) or
      Php_FunctionComp(3552) or
      Php_FunctionComp(3828) or
      Php_FunctionComp(4353) or
      Php_FunctionComp(4464) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc203: TSynWebTokenKind;
begin
  if  Php_FunctionComp(259) or
      Php_FunctionComp(896) or
      Php_FunctionComp(1316) or
      Php_FunctionComp(1318) or
      Php_FunctionComp(1674) or
      Php_FunctionComp(2397) or
      Php_FunctionComp(2675) or
      Php_FunctionComp(2784) or
      Php_FunctionComp(2991) or
      Php_FunctionComp(3414) or
      Php_FunctionComp(3898) or
      Php_FunctionComp(3958) or
      Php_FunctionComp(4017) or
      Php_FunctionComp(4034) or
      Php_FunctionComp(4149) or
      Php_FunctionComp(4180) or
      Php_FunctionComp(4219) or
      Php_FunctionComp(4274) or
      Php_FunctionComp(4757) or
      Php_FunctionComp(4782) or
      Php_FunctionComp(4912) or
      Php_FunctionComp(5088) or
      Php_FunctionComp(5158) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc204: TSynWebTokenKind;
begin
  if  Php_FunctionComp(187) or
      Php_FunctionComp(560) or
      Php_FunctionComp(1074) or
      Php_FunctionComp(1394) or
      Php_FunctionComp(1673) or
      Php_FunctionComp(2194) or
      Php_FunctionComp(2344) or
      Php_FunctionComp(2762) or
      Php_FunctionComp(2852) or
      Php_FunctionComp(2856) or
      Php_FunctionComp(2873) or
      Php_FunctionComp(2876) or
      Php_FunctionComp(2929) or
      Php_FunctionComp(2953) or
      Php_FunctionComp(3086) or
      Php_FunctionComp(3352) or
      Php_FunctionComp(3787) or
      Php_FunctionComp(3882) or
      Php_FunctionComp(3963) or
      Php_FunctionComp(4099) or
      Php_FunctionComp(4251) or
      Php_FunctionComp(4277) or
      Php_FunctionComp(4657) or
      Php_FunctionComp(4789) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc205: TSynWebTokenKind;
begin
  if  Php_FunctionComp(188) or
      Php_FunctionComp(956) or
      Php_FunctionComp(1138) or
      Php_FunctionComp(1627) or
      Php_FunctionComp(1864) or
      Php_FunctionComp(1888) or
      Php_FunctionComp(2353) or
      Php_FunctionComp(2468) or
      Php_FunctionComp(2679) or
      Php_FunctionComp(2722) or
      Php_FunctionComp(2788) or
      Php_FunctionComp(2997) or
      Php_FunctionComp(3032) or
      Php_FunctionComp(3330) or
      Php_FunctionComp(3757) or
      Php_FunctionComp(3858) or
      Php_FunctionComp(4304) or
      Php_FunctionComp(4325) or
      Php_FunctionComp(4406) or
      Php_FunctionComp(4625) or
      Php_FunctionComp(4658) or
      Php_FunctionComp(4660) or
      Php_FunctionComp(4715) or
      Php_FunctionComp(4774) or
      Php_FunctionComp(4949) or
      Php_FunctionComp(4975) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc206: TSynWebTokenKind;
begin
  if  Php_FunctionComp(83) or
      Php_FunctionComp(263) or
      Php_FunctionComp(279) or
      Php_FunctionComp(373) or
      Php_FunctionComp(631) or
      Php_FunctionComp(886) or
      Php_FunctionComp(1476) or
      Php_FunctionComp(1577) or
      Php_FunctionComp(2351) or
      Php_FunctionComp(2855) or
      Php_FunctionComp(3260) or
      Php_FunctionComp(3344) or
      Php_FunctionComp(3705) or
      Php_FunctionComp(4027) or
      Php_FunctionComp(4029) or
      Php_FunctionComp(4106) or
      Php_FunctionComp(4275) or
      Php_FunctionComp(4542) or
      Php_FunctionComp(4545) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc207: TSynWebTokenKind;
begin
  if  Php_FunctionComp(751) or
      Php_FunctionComp(967) or
      Php_FunctionComp(1026) or
      Php_FunctionComp(1084) or
      Php_FunctionComp(2111) or
      Php_FunctionComp(2345) or
      Php_FunctionComp(2607) or
      Php_FunctionComp(2857) or
      Php_FunctionComp(2858) or
      Php_FunctionComp(2937) or
      Php_FunctionComp(3648) or
      Php_FunctionComp(3701) or
      Php_FunctionComp(3726) or
      Php_FunctionComp(3802) or
      Php_FunctionComp(4006) or
      Php_FunctionComp(4179) or
      Php_FunctionComp(4322) or
      Php_FunctionComp(4617) or
      Php_FunctionComp(4708) or
      Php_FunctionComp(4835) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc208: TSynWebTokenKind;
begin
  if  Php_FunctionComp(661) or
      Php_FunctionComp(826) or
      Php_FunctionComp(1116) or
      Php_FunctionComp(1767) or
      Php_FunctionComp(1993) or
      Php_FunctionComp(2223) or
      Php_FunctionComp(2503) or
      Php_FunctionComp(2596) or
      Php_FunctionComp(2669) or
      Php_FunctionComp(2802) or
      Php_FunctionComp(2959) or
      Php_FunctionComp(2978) or
      Php_FunctionComp(3028) or
      Php_FunctionComp(3543) or
      Php_FunctionComp(3960) or
      Php_FunctionComp(4319) or
      Php_FunctionComp(4343) or
      Php_FunctionComp(5024) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc209: TSynWebTokenKind;
begin
  if  Php_FunctionComp(288) or
      Php_FunctionComp(521) or
      Php_FunctionComp(756) or
      Php_FunctionComp(1019) or
      Php_FunctionComp(2242) or
      Php_FunctionComp(2260) or
      Php_FunctionComp(2352) or
      Php_FunctionComp(2666) or
      Php_FunctionComp(2724) or
      Php_FunctionComp(2815) or
      Php_FunctionComp(2879) or
      Php_FunctionComp(3256) or
      Php_FunctionComp(3334) or
      Php_FunctionComp(3506) or
      Php_FunctionComp(3516) or
      Php_FunctionComp(3780) or
      Php_FunctionComp(4221) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc210: TSynWebTokenKind;
begin
  if  Php_FunctionComp(225) or
      Php_FunctionComp(1257) or
      Php_FunctionComp(2290) or
      Php_FunctionComp(2891) or
      Php_FunctionComp(2979) or
      Php_FunctionComp(3146) or
      Php_FunctionComp(3195) or
      Php_FunctionComp(3920) or
      Php_FunctionComp(4004) or
      Php_FunctionComp(4016) or
      Php_FunctionComp(4349) or
      Php_FunctionComp(4354) or
      Php_FunctionComp(4366) or
      Php_FunctionComp(4557) or
      Php_FunctionComp(4698) or
      Php_FunctionComp(4915) or
      Php_FunctionComp(4976) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc211: TSynWebTokenKind;
begin
  if  Php_FunctionComp(374) or
      Php_FunctionComp(957) or
      Php_FunctionComp(1020) or
      Php_FunctionComp(1488) or
      Php_FunctionComp(1636) or
      Php_FunctionComp(1995) or
      Php_FunctionComp(2509) or
      Php_FunctionComp(2689) or
      Php_FunctionComp(2796) or
      Php_FunctionComp(2907) or
      Php_FunctionComp(2940) or
      Php_FunctionComp(2941) or
      Php_FunctionComp(3333) or
      Php_FunctionComp(3493) or
      Php_FunctionComp(3662) or
      Php_FunctionComp(4042) or
      Php_FunctionComp(4071) or
      Php_FunctionComp(4342) or
      Php_FunctionComp(4408) or
      Php_FunctionComp(4627) or
      Php_FunctionComp(4688) or
      Php_FunctionComp(4703) or
      Php_FunctionComp(4903) or
      Php_FunctionComp(4984) or
      Php_FunctionComp(5043) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc212: TSynWebTokenKind;
begin
  if  Php_FunctionComp(154) or
      Php_FunctionComp(402) or
      Php_FunctionComp(468) or
      Php_FunctionComp(804) or
      Php_FunctionComp(824) or
      Php_FunctionComp(1626) or
      Php_FunctionComp(1876) or
      Php_FunctionComp(2177) or
      Php_FunctionComp(2947) or
      Php_FunctionComp(3709) or
      Php_FunctionComp(4197) or
      Php_FunctionComp(4437) or
      Php_FunctionComp(4480) or
      Php_FunctionComp(4566) or
      Php_FunctionComp(4619) or
      Php_FunctionComp(4686) or
      Php_FunctionComp(4791) or
      Php_FunctionComp(4870) or
      Php_FunctionComp(4939) or
      Php_FunctionComp(5087) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc213: TSynWebTokenKind;
begin
  if  Php_FunctionComp(145) or
      Php_FunctionComp(529) or
      Php_FunctionComp(1970) or
      Php_FunctionComp(2568) or
      Php_FunctionComp(2703) or
      Php_FunctionComp(2798) or
      Php_FunctionComp(2961) or
      Php_FunctionComp(3159) or
      Php_FunctionComp(3163) or
      Php_FunctionComp(3338) or
      Php_FunctionComp(3354) or
      Php_FunctionComp(3404) or
      Php_FunctionComp(3488) or
      Php_FunctionComp(3945) or
      Php_FunctionComp(4700) or
      Php_FunctionComp(4702) or
      Php_FunctionComp(4706) or
      Php_FunctionComp(4815) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc214: TSynWebTokenKind;
begin
  if  Php_FunctionComp(317) or
      Php_FunctionComp(452) or
      Php_FunctionComp(537) or
      Php_FunctionComp(625) or
      Php_FunctionComp(1178) or
      Php_FunctionComp(1307) or
      Php_FunctionComp(1654) or
      Php_FunctionComp(1896) or
      Php_FunctionComp(2602) or
      Php_FunctionComp(2887) or
      Php_FunctionComp(2908) or
      Php_FunctionComp(3264) or
      Php_FunctionComp(3341) or
      Php_FunctionComp(3683) or
      Php_FunctionComp(3974) or
      Php_FunctionComp(4385) or
      Php_FunctionComp(4482) or
      Php_FunctionComp(4553) or
      Php_FunctionComp(4561) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc215: TSynWebTokenKind;
begin
  if  Php_FunctionComp(367) or
      Php_FunctionComp(607) or
      Php_FunctionComp(977) or
      Php_FunctionComp(1050) or
      Php_FunctionComp(1191) or
      Php_FunctionComp(1308) or
      Php_FunctionComp(1485) or
      Php_FunctionComp(1656) or
      Php_FunctionComp(1842) or
      Php_FunctionComp(2517) or
      Php_FunctionComp(2608) or
      Php_FunctionComp(2712) or
      Php_FunctionComp(3643) or
      Php_FunctionComp(3714) or
      Php_FunctionComp(3871) or
      Php_FunctionComp(4187) or
      Php_FunctionComp(4399) or
      Php_FunctionComp(4423) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc216: TSynWebTokenKind;
begin
  if  Php_FunctionComp(180) or
      Php_FunctionComp(206) or
      Php_FunctionComp(419) or
      Php_FunctionComp(616) or
      Php_FunctionComp(618) or
      Php_FunctionComp(910) or
      Php_FunctionComp(1056) or
      Php_FunctionComp(1425) or
      Php_FunctionComp(1477) or
      Php_FunctionComp(1805) or
      Php_FunctionComp(2032) or
      Php_FunctionComp(2281) or
      Php_FunctionComp(2306) or
      Php_FunctionComp(2362) or
      Php_FunctionComp(2394) or
      Php_FunctionComp(2614) or
      Php_FunctionComp(2902) or
      Php_FunctionComp(3144) or
      Php_FunctionComp(3192) or
      Php_FunctionComp(3248) or
      Php_FunctionComp(3890) or
      Php_FunctionComp(3991) or
      Php_FunctionComp(4457) or
      Php_FunctionComp(4559) or
      Php_FunctionComp(4710) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc217: TSynWebTokenKind;
begin
  if  Php_FunctionComp(217) or
      Php_FunctionComp(399) or
      Php_FunctionComp(803) or
      Php_FunctionComp(978) or
      Php_FunctionComp(1051) or
      Php_FunctionComp(1402) or
      Php_FunctionComp(1594) or
      Php_FunctionComp(1843) or
      Php_FunctionComp(2467) or
      Php_FunctionComp(2501) or
      Php_FunctionComp(2506) or
      Php_FunctionComp(2790) or
      Php_FunctionComp(3262) or
      Php_FunctionComp(3450) or
      Php_FunctionComp(3971) or
      Php_FunctionComp(4052) or
      Php_FunctionComp(4664) or
      Php_FunctionComp(4680) or
      Php_FunctionComp(4792) or
      Php_FunctionComp(4913) or
      Php_FunctionComp(4959) or
      Php_FunctionComp(5103) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc218: TSynWebTokenKind;
begin
  if  Php_FunctionComp(362) or
      Php_FunctionComp(364) or
      Php_FunctionComp(392) or
      Php_FunctionComp(1533) or
      Php_FunctionComp(2049) or
      Php_FunctionComp(2783) or
      Php_FunctionComp(2849) or
      Php_FunctionComp(2875) or
      Php_FunctionComp(3340) or
      Php_FunctionComp(3612) or
      Php_FunctionComp(4308) or
      Php_FunctionComp(4333) or
      Php_FunctionComp(4468) or
      Php_FunctionComp(4616) or
      Php_FunctionComp(4672) or
      Php_FunctionComp(4678) or
      Php_FunctionComp(4690) or
      Php_FunctionComp(4711) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc219: TSynWebTokenKind;
begin
  if  Php_FunctionComp(344) or
      Php_FunctionComp(838) or
      Php_FunctionComp(952) or
      Php_FunctionComp(1114) or
      Php_FunctionComp(1322) or
      Php_FunctionComp(2505) or
      Php_FunctionComp(2510) or
      Php_FunctionComp(2591) or
      Php_FunctionComp(2592) or
      Php_FunctionComp(2818) or
      Php_FunctionComp(3617) or
      Php_FunctionComp(3703) or
      Php_FunctionComp(3722) or
      Php_FunctionComp(3824) or
      Php_FunctionComp(3896) or
      Php_FunctionComp(4014) or
      Php_FunctionComp(4526) or
      Php_FunctionComp(4540) or
      Php_FunctionComp(4704) or
      Php_FunctionComp(4930) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc220: TSynWebTokenKind;
begin
  if  Php_FunctionComp(103) or
      Php_FunctionComp(137) or
      Php_FunctionComp(614) or
      Php_FunctionComp(1397) or
      Php_FunctionComp(1495) or
      Php_FunctionComp(1671) or
      Php_FunctionComp(1835) or
      Php_FunctionComp(2300) or
      Php_FunctionComp(2454) or
      Php_FunctionComp(2610) or
      Php_FunctionComp(2690) or
      Php_FunctionComp(2734) or
      Php_FunctionComp(2925) or
      Php_FunctionComp(2956) or
      Php_FunctionComp(3008) or
      Php_FunctionComp(3601) or
      Php_FunctionComp(3699) or
      Php_FunctionComp(4147) or
      Php_FunctionComp(4929) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc221: TSynWebTokenKind;
begin
  if  Php_FunctionComp(806) or
      Php_FunctionComp(808) or
      Php_FunctionComp(830) or
      Php_FunctionComp(1189) or
      Php_FunctionComp(1813) or
      Php_FunctionComp(2163) or
      Php_FunctionComp(2810) or
      Php_FunctionComp(3645) or
      Php_FunctionComp(3717) or
      Php_FunctionComp(3915) or
      Php_FunctionComp(4151) or
      Php_FunctionComp(4183) or
      Php_FunctionComp(4340) or
      Php_FunctionComp(4528) or
      Php_FunctionComp(4633) or
      Php_FunctionComp(4635) or
      Php_FunctionComp(4643) or
      Php_FunctionComp(5091) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc222: TSynWebTokenKind;
begin
  if  Php_FunctionComp(165) or
      Php_FunctionComp(171) or
      Php_FunctionComp(281) or
      Php_FunctionComp(307) or
      Php_FunctionComp(1139) or
      Php_FunctionComp(2699) or
      Php_FunctionComp(2759) or
      Php_FunctionComp(3293) or
      Php_FunctionComp(3755) or
      Php_FunctionComp(3789) or
      Php_FunctionComp(3836) or
      Php_FunctionComp(4012) or
      Php_FunctionComp(4394) or
      Php_FunctionComp(4534) or
      Php_FunctionComp(4693) or
      Php_FunctionComp(4777) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc223: TSynWebTokenKind;
begin
  if  Php_FunctionComp(267) or
      Php_FunctionComp(299) or
      Php_FunctionComp(834) or
      Php_FunctionComp(1576) or
      Php_FunctionComp(2498) or
      Php_FunctionComp(2797) or
      Php_FunctionComp(2916) or
      Php_FunctionComp(4010) or
      Php_FunctionComp(4477) or
      Php_FunctionComp(4694) or
      Php_FunctionComp(4910) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc224: TSynWebTokenKind;
begin
  if  Php_FunctionComp(266) or
      Php_FunctionComp(615) or
      Php_FunctionComp(736) or
      Php_FunctionComp(989) or
      Php_FunctionComp(1153) or
      Php_FunctionComp(1868) or
      Php_FunctionComp(2254) or
      Php_FunctionComp(2280) or
      Php_FunctionComp(2304) or
      Php_FunctionComp(2714) or
      Php_FunctionComp(2765) or
      Php_FunctionComp(2999) or
      Php_FunctionComp(3054) or
      Php_FunctionComp(3203) or
      Php_FunctionComp(3821) or
      Php_FunctionComp(3822) or
      Php_FunctionComp(3884) or
      Php_FunctionComp(3972) or
      Php_FunctionComp(4098) or
      Php_FunctionComp(4198) or
      Php_FunctionComp(4465) or
      Php_FunctionComp(4467) or
      Php_FunctionComp(4473) or
      Php_FunctionComp(4709) or
      Php_FunctionComp(4739) or
      Php_FunctionComp(4989) or
      Php_FunctionComp(5163) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc225: TSynWebTokenKind;
begin
  if  Php_FunctionComp(296) or
      Php_FunctionComp(365) or
      Php_FunctionComp(372) or
      Php_FunctionComp(2497) or
      Php_FunctionComp(2713) or
      Php_FunctionComp(2735) or
      Php_FunctionComp(2906) or
      Php_FunctionComp(2980) or
      Php_FunctionComp(4562) or
      Php_FunctionComp(4673) or
      Php_FunctionComp(5009) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc226: TSynWebTokenKind;
begin
  if  Php_FunctionComp(138) or
      Php_FunctionComp(320) or
      Php_FunctionComp(1892) or
      Php_FunctionComp(1968) or
      Php_FunctionComp(2024) or
      Php_FunctionComp(2430) or
      Php_FunctionComp(3232) or
      Php_FunctionComp(3500) or
      Php_FunctionComp(3649) or
      Php_FunctionComp(3862) or
      Php_FunctionComp(4415) or
      Php_FunctionComp(4485) or
      Php_FunctionComp(4675) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc227: TSynWebTokenKind;
begin
  if  Php_FunctionComp(457) or
      Php_FunctionComp(832) or
      Php_FunctionComp(903) or
      Php_FunctionComp(961) or
      Php_FunctionComp(1313) or
      Php_FunctionComp(1846) or
      Php_FunctionComp(2403) or
      Php_FunctionComp(2507) or
      Php_FunctionComp(2949) or
      Php_FunctionComp(3245) or
      Php_FunctionComp(3363) or
      Php_FunctionComp(3995) or
      Php_FunctionComp(4378) or
      Php_FunctionComp(5001) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc228: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1559) or
      Php_FunctionComp(1696) or
      Php_FunctionComp(1839) or
      Php_FunctionComp(3610) or
      Php_FunctionComp(4334) or
      Php_FunctionComp(4355) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc229: TSynWebTokenKind;
begin
  if  Php_FunctionComp(777) or
      Php_FunctionComp(918) or
      Php_FunctionComp(1558) or
      Php_FunctionComp(1695) or
      Php_FunctionComp(1850) or
      Php_FunctionComp(1867) or
      Php_FunctionComp(2046) or
      Php_FunctionComp(2116) or
      Php_FunctionComp(2392) or
      Php_FunctionComp(2504) or
      Php_FunctionComp(2950) or
      Php_FunctionComp(2983) or
      Php_FunctionComp(3036) or
      Php_FunctionComp(3451) or
      Php_FunctionComp(3734) or
      Php_FunctionComp(3879) or
      Php_FunctionComp(3897) or
      Php_FunctionComp(4050) or
      Php_FunctionComp(4055) or
      Php_FunctionComp(4138) or
      Php_FunctionComp(4458) or
      Php_FunctionComp(4990) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc230: TSynWebTokenKind;
begin
  if  Php_FunctionComp(248) or
      Php_FunctionComp(404) or
      Php_FunctionComp(2196) or
      Php_FunctionComp(3013) or
      Php_FunctionComp(3055) or
      Php_FunctionComp(3814) or
      Php_FunctionComp(4350) or
      Php_FunctionComp(4551) or
      Php_FunctionComp(4712) or
      Php_FunctionComp(4724) or
      Php_FunctionComp(4966) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc231: TSynWebTokenKind;
begin
  if  Php_FunctionComp(224) or
      Php_FunctionComp(378) or
      Php_FunctionComp(1618) or
      Php_FunctionComp(2667) or
      Php_FunctionComp(3345) or
      Php_FunctionComp(3633) or
      Php_FunctionComp(3712) or
      Php_FunctionComp(3867) or
      Php_FunctionComp(4299) or
      Php_FunctionComp(4402) or
      Php_FunctionComp(4726) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc232: TSynWebTokenKind;
begin
  if  Php_FunctionComp(568) or
      Php_FunctionComp(890) or
      Php_FunctionComp(1132) or
      Php_FunctionComp(1299) or
      Php_FunctionComp(1847) or
      Php_FunctionComp(1874) or
      Php_FunctionComp(2747) or
      Php_FunctionComp(2757) or
      Php_FunctionComp(2773) or
      Php_FunctionComp(2848) or
      Php_FunctionComp(2900) or
      Php_FunctionComp(3721) or
      Php_FunctionComp(3833) or
      Php_FunctionComp(4223) or
      Php_FunctionComp(4530) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc233: TSynWebTokenKind;
begin
  if  Php_FunctionComp(216) or
      Php_FunctionComp(884) or
      Php_FunctionComp(976) or
      Php_FunctionComp(2457) or
      Php_FunctionComp(2668) or
      Php_FunctionComp(2769) or
      Php_FunctionComp(2811) or
      Php_FunctionComp(2963) or
      Php_FunctionComp(2970) or
      Php_FunctionComp(3504) or
      Php_FunctionComp(3724) or
      Php_FunctionComp(5071) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc234: TSynWebTokenKind;
begin
  if  Php_FunctionComp(85) or
      Php_FunctionComp(567) or
      Php_FunctionComp(1192) or
      Php_FunctionComp(1557) or
      Php_FunctionComp(1657) or
      Php_FunctionComp(2566) or
      Php_FunctionComp(2767) or
      Php_FunctionComp(2823) or
      Php_FunctionComp(2927) or
      Php_FunctionComp(3000) or
      Php_FunctionComp(3035) or
      Php_FunctionComp(3277) or
      Php_FunctionComp(3342) or
      Php_FunctionComp(3497) or
      Php_FunctionComp(3830) or
      Php_FunctionComp(3891) or
      Php_FunctionComp(3959) or
      Php_FunctionComp(4332) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc235: TSynWebTokenKind;
begin
  if  Php_FunctionComp(620) or
      Php_FunctionComp(906) or
      Php_FunctionComp(1283) or
      Php_FunctionComp(1332) or
      Php_FunctionComp(2830) or
      Php_FunctionComp(2930) or
      Php_FunctionComp(3553) or
      Php_FunctionComp(4018) or
      Php_FunctionComp(4070) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc236: TSynWebTokenKind;
begin
  if  Php_FunctionComp(459) or
      Php_FunctionComp(1119) or
      Php_FunctionComp(1480) or
      Php_FunctionComp(1833) or
      Php_FunctionComp(2115) or
      Php_FunctionComp(2584) or
      Php_FunctionComp(2992) or
      Php_FunctionComp(3020) or
      Php_FunctionComp(3060) or
      Php_FunctionComp(3943) or
      Php_FunctionComp(3973) or
      Php_FunctionComp(4048) or
      Php_FunctionComp(4072) or
      Php_FunctionComp(4190) or
      Php_FunctionComp(4226) or
      Php_FunctionComp(4305) or
      Php_FunctionComp(4486) or
      Php_FunctionComp(4638) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc237: TSynWebTokenKind;
begin
  if  Php_FunctionComp(366) or
      Php_FunctionComp(397) or
      Php_FunctionComp(1687) or
      Php_FunctionComp(1709) or
      Php_FunctionComp(1840) or
      Php_FunctionComp(2407) or
      Php_FunctionComp(2812) or
      Php_FunctionComp(4288) or
      Php_FunctionComp(4608) or
      Php_FunctionComp(5003) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc238: TSynWebTokenKind;
begin
  if  Php_FunctionComp(219) or
      Php_FunctionComp(555) or
      Php_FunctionComp(694) or
      Php_FunctionComp(892) or
      Php_FunctionComp(916) or
      Php_FunctionComp(984) or
      Php_FunctionComp(2388) or
      Php_FunctionComp(2493) or
      Php_FunctionComp(2569) or
      Php_FunctionComp(2673) or
      Php_FunctionComp(3021) or
      Php_FunctionComp(3268) or
      Php_FunctionComp(4033) or
      Php_FunctionComp(4039) or
      Php_FunctionComp(4150) or
      Php_FunctionComp(4181) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc239: TSynWebTokenKind;
begin
  if  Php_FunctionComp(609) or
      Php_FunctionComp(1305) or
      Php_FunctionComp(1644) or
      Php_FunctionComp(1645) or
      Php_FunctionComp(2387) or
      Php_FunctionComp(2491) or
      Php_FunctionComp(2741) or
      Php_FunctionComp(2751) or
      Php_FunctionComp(2944) or
      Php_FunctionComp(2969) or
      Php_FunctionComp(3364) or
      Php_FunctionComp(4038) or
      Php_FunctionComp(5022) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc240: TSynWebTokenKind;
begin
  if  Php_FunctionComp(965) or
      Php_FunctionComp(981) or
      Php_FunctionComp(1075) or
      Php_FunctionComp(1107) or
      Php_FunctionComp(2378) or
      Php_FunctionComp(2801) or
      Php_FunctionComp(2962) or
      Php_FunctionComp(3274) or
      Php_FunctionComp(3339) or
      Php_FunctionComp(3978) or
      Php_FunctionComp(4632) or
      Php_FunctionComp(4904) or
      Php_FunctionComp(5027) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc241: TSynWebTokenKind;
begin
  if  Php_FunctionComp(86) or
      Php_FunctionComp(930) or
      Php_FunctionComp(992) or
      Php_FunctionComp(1043) or
      Php_FunctionComp(1133) or
      Php_FunctionComp(1226) or
      Php_FunctionComp(1482) or
      Php_FunctionComp(2342) or
      Php_FunctionComp(2376) or
      Php_FunctionComp(2813) or
      Php_FunctionComp(2957) or
      Php_FunctionComp(3088) or
      Php_FunctionComp(3326) or
      Php_FunctionComp(3655) or
      Php_FunctionComp(3886) or
      Php_FunctionComp(4699) or
      Php_FunctionComp(4701) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc242: TSynWebTokenKind;
begin
  if  Php_FunctionComp(111) or
      Php_FunctionComp(152) or
      Php_FunctionComp(962) or
      Php_FunctionComp(1856) or
      Php_FunctionComp(2367) or
      Php_FunctionComp(3918) or
      Php_FunctionComp(4110) or
      Php_FunctionComp(4161) or
      Php_FunctionComp(4178) or
      Php_FunctionComp(5054) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc243: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1534) or
      Php_FunctionComp(2546) or
      Php_FunctionComp(2742) or
      Php_FunctionComp(2792) or
      Php_FunctionComp(3298) or
      Php_FunctionComp(3864) or
      Php_FunctionComp(4095) or
      Php_FunctionComp(4307) or
      Php_FunctionComp(4410) or
      Php_FunctionComp(4431) or
      Php_FunctionComp(4609) or
      Php_FunctionComp(4681) or
      Php_FunctionComp(4943) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc244: TSynWebTokenKind;
begin
  if  Php_FunctionComp(400) or
      Php_FunctionComp(590) or
      Php_FunctionComp(825) or
      Php_FunctionComp(887) or
      Php_FunctionComp(969) or
      Php_FunctionComp(982) or
      Php_FunctionComp(1878) or
      Php_FunctionComp(2951) or
      Php_FunctionComp(3347) or
      Php_FunctionComp(4228) or
      Php_FunctionComp(4413) or
      Php_FunctionComp(4634) or
      Php_FunctionComp(5115) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc245: TSynWebTokenKind;
begin
  if  Php_FunctionComp(909) or
      Php_FunctionComp(936) or
      Php_FunctionComp(1080) or
      Php_FunctionComp(1081) or
      Php_FunctionComp(1281) or
      Php_FunctionComp(2385) or
      Php_FunctionComp(2808) or
      Php_FunctionComp(4565) or
      Php_FunctionComp(4765) or
      Php_FunctionComp(5042) or
      Php_FunctionComp(5048) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc246: TSynWebTokenKind;
begin
  if  Php_FunctionComp(178) or
      Php_FunctionComp(589) or
      Php_FunctionComp(805) or
      Php_FunctionComp(869) or
      Php_FunctionComp(902) or
      Php_FunctionComp(2301) or
      Php_FunctionComp(2715) or
      Php_FunctionComp(2822) or
      Php_FunctionComp(3368) or
      Php_FunctionComp(3935) or
      Php_FunctionComp(4108) or
      Php_FunctionComp(4189) or
      Php_FunctionComp(4438) or
      Php_FunctionComp(4463) or
      Php_FunctionComp(4631) or
      Php_FunctionComp(4750) or
      Php_FunctionComp(5046) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc247: TSynWebTokenKind;
begin
  if  Php_FunctionComp(110) or
      Php_FunctionComp(174) or
      Php_FunctionComp(285) or
      Php_FunctionComp(619) or
      Php_FunctionComp(958) or
      Php_FunctionComp(1306) or
      Php_FunctionComp(1310) or
      Php_FunctionComp(1549) or
      Php_FunctionComp(2748) or
      Php_FunctionComp(3001) or
      Php_FunctionComp(3837) or
      Php_FunctionComp(4419) or
      Php_FunctionComp(4466) or
      Php_FunctionComp(4687) or
      Php_FunctionComp(4986) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc248: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2441) or
      Php_FunctionComp(3525) or
      Php_FunctionComp(5016) or
      Php_FunctionComp(5036) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc249: TSynWebTokenKind;
begin
  if  Php_FunctionComp(361) or
      Php_FunctionComp(566) or
      Php_FunctionComp(617) or
      Php_FunctionComp(926) or
      Php_FunctionComp(1849) or
      Php_FunctionComp(2492) or
      Php_FunctionComp(2716) or
      Php_FunctionComp(2737) or
      Php_FunctionComp(4185) or
      Php_FunctionComp(4484) or
      Php_FunctionComp(4713) or
      Php_FunctionComp(5033) or
      Php_FunctionComp(5070) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc250: TSynWebTokenKind;
begin
  if  Php_FunctionComp(308) or
      Php_FunctionComp(556) or
      Php_FunctionComp(835) or
      Php_FunctionComp(857) or
      Php_FunctionComp(1295) or
      Php_FunctionComp(1686) or
      Php_FunctionComp(2814) or
      Php_FunctionComp(3258) or
      Php_FunctionComp(3275) or
      Php_FunctionComp(3636) or
      Php_FunctionComp(3893) or
      Php_FunctionComp(4383) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc251: TSynWebTokenKind;
begin
  if  Php_FunctionComp(475) or
      Php_FunctionComp(513) or
      Php_FunctionComp(1010) or
      Php_FunctionComp(1285) or
      Php_FunctionComp(1286) or
      Php_FunctionComp(1287) or
      Php_FunctionComp(1288) or
      Php_FunctionComp(1289) or
      Php_FunctionComp(2490) or
      Php_FunctionComp(3967) or
      Php_FunctionComp(4040) or
      Php_FunctionComp(4148) or
      Php_FunctionComp(4546) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc252: TSynWebTokenKind;
begin
  if  Php_FunctionComp(510) or
      Php_FunctionComp(1085) or
      Php_FunctionComp(1642) or
      Php_FunctionComp(2058) or
      Php_FunctionComp(2464) or
      Php_FunctionComp(2746) or
      Php_FunctionComp(3061) or
      Php_FunctionComp(3282) or
      Php_FunctionComp(3403) or
      Php_FunctionComp(4300) or
      Php_FunctionComp(4636) or
      Php_FunctionComp(4676) or
      Php_FunctionComp(4718) or
      Php_FunctionComp(5029) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc253: TSynWebTokenKind;
begin
  if  Php_FunctionComp(450) or
      Php_FunctionComp(814) or
      Php_FunctionComp(870) or
      Php_FunctionComp(1658) or
      Php_FunctionComp(2781) or
      Php_FunctionComp(2854) or
      Php_FunctionComp(3803) or
      Php_FunctionComp(3913) or
      Php_FunctionComp(4046) or
      Php_FunctionComp(4184) or
      Php_FunctionComp(4193) or
      Php_FunctionComp(4478) or
      Php_FunctionComp(4637) or
      Php_FunctionComp(4969) or
      Php_FunctionComp(5085) or
      Php_FunctionComp(5116) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc254: TSynWebTokenKind;
begin
  if  Php_FunctionComp(106) or
      Php_FunctionComp(218) or
      Php_FunctionComp(242) or
      Php_FunctionComp(851) or
      Php_FunctionComp(959) or
      Php_FunctionComp(971) or
      Php_FunctionComp(979) or
      Php_FunctionComp(991) or
      Php_FunctionComp(1303) or
      Php_FunctionComp(1664) or
      Php_FunctionComp(2831) or
      Php_FunctionComp(3286) or
      Php_FunctionComp(3964) or
      Php_FunctionComp(4036) or
      Php_FunctionComp(4175) or
      Php_FunctionComp(4719) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc255: TSynWebTokenKind;
begin
  if  Php_FunctionComp(166) or
      Php_FunctionComp(172) or
      Php_FunctionComp(418) or
      Php_FunctionComp(966) or
      Php_FunctionComp(995) or
      Php_FunctionComp(1326) or
      Php_FunctionComp(1489) or
      Php_FunctionComp(1556) or
      Php_FunctionComp(2461) or
      Php_FunctionComp(2770) or
      Php_FunctionComp(2987) or
      Php_FunctionComp(3278) or
      Php_FunctionComp(5112) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc256: TSynWebTokenKind;
begin
  if  Php_FunctionComp(822) or
      Php_FunctionComp(1713) or
      Php_FunctionComp(1841) or
      Php_FunctionComp(2136) or
      Php_FunctionComp(2780) or
      Php_FunctionComp(4395) or
      Php_FunctionComp(4543) or
      Php_FunctionComp(4644) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc257: TSynWebTokenKind;
begin
  if  Php_FunctionComp(153) or
      Php_FunctionComp(1614) or
      Php_FunctionComp(1690) or
      Php_FunctionComp(2778) or
      Php_FunctionComp(3621) or
      Php_FunctionComp(3922) or
      Php_FunctionComp(4287) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc258: TSynWebTokenKind;
begin
  if  Php_FunctionComp(850) or
      Php_FunctionComp(1016) or
      Php_FunctionComp(1230) or
      Php_FunctionComp(1493) or
      Php_FunctionComp(1621) or
      Php_FunctionComp(2325) or
      Php_FunctionComp(2728) or
      Php_FunctionComp(2771) or
      Php_FunctionComp(2806) or
      Php_FunctionComp(4031) or
      Php_FunctionComp(4153) or
      Php_FunctionComp(4418) or
      Php_FunctionComp(4630) or
      Php_FunctionComp(4647) or
      Php_FunctionComp(4677) or
      Php_FunctionComp(4988) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc259: TSynWebTokenKind;
begin
  if  Php_FunctionComp(833) or
      Php_FunctionComp(998) or
      Php_FunctionComp(1280) or
      Php_FunctionComp(2861) or
      Php_FunctionComp(2938) or
      Php_FunctionComp(2939) or
      Php_FunctionComp(3251) or
      Php_FunctionComp(3353) or
      Php_FunctionComp(4430) or
      Php_FunctionComp(4544) or
      Php_FunctionComp(4751) or
      Php_FunctionComp(4987) or
      Php_FunctionComp(5055) or
      Php_FunctionComp(5157) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc260: TSynWebTokenKind;
begin
  if  Php_FunctionComp(946) or
      Php_FunctionComp(1045) or
      Php_FunctionComp(2779) or
      Php_FunctionComp(4641) or
      Php_FunctionComp(4717) or
      Php_FunctionComp(5017) or
      Php_FunctionComp(5089) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc261: TSynWebTokenKind;
begin
  if  Php_FunctionComp(375) or
      Php_FunctionComp(588) or
      Php_FunctionComp(1551) or
      Php_FunctionComp(1592) or
      Php_FunctionComp(1595) or
      Php_FunctionComp(1597) or
      Php_FunctionComp(1705) or
      Php_FunctionComp(1855) or
      Php_FunctionComp(3272) or
      Php_FunctionComp(3323) or
      Php_FunctionComp(3754) or
      Php_FunctionComp(3965) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc262: TSynWebTokenKind;
begin
  if  Php_FunctionComp(167) or
      Php_FunctionComp(812) or
      Php_FunctionComp(919) or
      Php_FunctionComp(922) or
      Php_FunctionComp(1404) or
      Php_FunctionComp(3132) or
      Php_FunctionComp(3197) or
      Php_FunctionComp(3252) or
      Php_FunctionComp(3912) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc263: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1071) or
      Php_FunctionComp(1573) or
      Php_FunctionComp(3249) or
      Php_FunctionComp(4141) or
      Php_FunctionComp(4159) or
      Php_FunctionComp(4188) or
      Php_FunctionComp(4404) or
      Php_FunctionComp(4455) or
      Php_FunctionComp(4649) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc264: TSynWebTokenKind;
begin
  if  Php_FunctionComp(527) or
      Php_FunctionComp(943) or
      Php_FunctionComp(2931) or
      Php_FunctionComp(3287) or
      Php_FunctionComp(3292) or
      Php_FunctionComp(3713) or
      Php_FunctionComp(4421) or
      Php_FunctionComp(4651) or
      Php_FunctionComp(4944) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc265: TSynWebTokenKind;
begin
  if  Php_FunctionComp(875) or
      Php_FunctionComp(986) or
      Php_FunctionComp(1479) or
      Php_FunctionComp(2041) or
      Php_FunctionComp(2346) or
      Php_FunctionComp(2721) or
      Php_FunctionComp(2819) or
      Php_FunctionComp(3961) or
      Php_FunctionComp(4224) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc266: TSynWebTokenKind;
begin
  if  Php_FunctionComp(186) or
      Php_FunctionComp(376) or
      Php_FunctionComp(862) or
      Php_FunctionComp(1073) or
      Php_FunctionComp(1838) or
      Php_FunctionComp(3892) or
      Php_FunctionComp(4881) or
      Php_FunctionComp(5008) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc267: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1172) or
      Php_FunctionComp(1569) or
      Php_FunctionComp(2789) or
      Php_FunctionComp(2827) or
      Php_FunctionComp(4172) or
      Php_FunctionComp(4982) or
      Php_FunctionComp(5020) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc268: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1079) or
      Php_FunctionComp(1399) or
      Php_FunctionComp(4472) or
      Php_FunctionComp(4945) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc269: TSynWebTokenKind;
begin
  if  Php_FunctionComp(462) or
      Php_FunctionComp(915) or
      Php_FunctionComp(929) or
      Php_FunctionComp(3838) or
      Php_FunctionComp(4432) or
      Php_FunctionComp(4434) or
      Php_FunctionComp(4439) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc270: TSynWebTokenKind;
begin
  if  Php_FunctionComp(286) or
      Php_FunctionComp(624) or
      Php_FunctionComp(933) or
      Php_FunctionComp(2807) or
      Php_FunctionComp(3250) or
      Php_FunctionComp(4045) or
      Php_FunctionComp(4335) or
      Php_FunctionComp(4426) or
      Php_FunctionComp(4833) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc271: TSynWebTokenKind;
begin
  if  Php_FunctionComp(914) or
      Php_FunctionComp(2321) or
      Php_FunctionComp(2459) or
      Php_FunctionComp(2518) or
      Php_FunctionComp(2989) or
      Php_FunctionComp(3719) or
      Php_FunctionComp(4041) or
      Php_FunctionComp(4639) or
      Php_FunctionComp(4948) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc272: TSynWebTokenKind;
begin
  if  Php_FunctionComp(623) or
      Php_FunctionComp(924) or
      Php_FunctionComp(990) or
      Php_FunctionComp(1315) or
      Php_FunctionComp(4289) or
      Php_FunctionComp(4716) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc273: TSynWebTokenKind;
begin
  if  Php_FunctionComp(819) or
      Php_FunctionComp(1032) or
      Php_FunctionComp(1619) or
      Php_FunctionComp(2516) or
      Php_FunctionComp(2901) or
      Php_FunctionComp(3944) or
      Php_FunctionComp(3966) or
      Php_FunctionComp(4373) or
      Php_FunctionComp(4488) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc274: TSynWebTokenKind;
begin
  if  Php_FunctionComp(282) or
      Php_FunctionComp(746) or
      Php_FunctionComp(2305) or
      Php_FunctionComp(2733) or
      Php_FunctionComp(3994) or
      Php_FunctionComp(4054) or
      Php_FunctionComp(4474) or
      Php_FunctionComp(4487) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc275: TSynWebTokenKind;
begin
  if  Php_FunctionComp(243) or
      Php_FunctionComp(1077) or
      Php_FunctionComp(1531) or
      Php_FunctionComp(2996) or
      Php_FunctionComp(3996) or
      Php_FunctionComp(4436) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc276: TSynWebTokenKind;
begin
  if  Php_FunctionComp(571) or
      Php_FunctionComp(621) or
      Php_FunctionComp(948) or
      Php_FunctionComp(994) or
      Php_FunctionComp(1944) or
      Php_FunctionComp(2391) or
      Php_FunctionComp(2816) or
      Php_FunctionComp(3619) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc277: TSynWebTokenKind;
begin
  if  Php_FunctionComp(176) or
      Php_FunctionComp(2508) or
      Php_FunctionComp(2973) or
      Php_FunctionComp(3962) or
      Php_FunctionComp(4037) or
      Php_FunctionComp(4229) or
      Php_FunctionComp(4411) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc278: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1039) or
      Php_FunctionComp(1041) or
      Php_FunctionComp(1052) or
      Php_FunctionComp(3087) or
      Php_FunctionComp(4422) or
      Php_FunctionComp(4459) or
      Php_FunctionComp(4645) or
      Php_FunctionComp(4723) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc279: TSynWebTokenKind;
begin
  if  Php_FunctionComp(866) or
      Php_FunctionComp(2296) or
      Php_FunctionComp(4400) or
      Php_FunctionComp(4991) or
      Php_FunctionComp(5010) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc280: TSynWebTokenKind;
begin
  if  Php_FunctionComp(622) or
      Php_FunctionComp(2825) or
      Php_FunctionComp(3281) or
      Php_FunctionComp(3999) or
      Php_FunctionComp(4286) or
      Php_FunctionComp(4947) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc281: TSynWebTokenKind;
begin
  if  Php_FunctionComp(173) or
      Php_FunctionComp(283) or
      Php_FunctionComp(4564) or
      Php_FunctionComp(4843) or
      Php_FunctionComp(5014) or
      Php_FunctionComp(5026) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc282: TSynWebTokenKind;
begin
  if  Php_FunctionComp(836) or
      Php_FunctionComp(878) or
      Php_FunctionComp(964) or
      Php_FunctionComp(3284) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc283: TSynWebTokenKind;
begin
  if  Php_FunctionComp(858) or
      Php_FunctionComp(937) or
      Php_FunctionComp(1048) or
      Php_FunctionComp(4801) or
      Php_FunctionComp(4880) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc284: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1617) or
      Php_FunctionComp(4157) or
      Php_FunctionComp(4550) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc285: TSynWebTokenKind;
begin
  if  Php_FunctionComp(201) or
      Php_FunctionComp(891) or
      Php_FunctionComp(997) or
      Php_FunctionComp(2013) or
      Php_FunctionComp(2465) or
      Php_FunctionComp(3832) or
      Php_FunctionComp(4481) or
      Php_FunctionComp(4731) or
      Php_FunctionComp(4919) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc286: TSynWebTokenKind;
begin
  if  Php_FunctionComp(815) or
      Php_FunctionComp(818) or
      Php_FunctionComp(871) or
      Php_FunctionComp(3291) or
      Php_FunctionComp(4168) or
      Php_FunctionComp(5021) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc287: TSynWebTokenKind;
begin
  if  Php_FunctionComp(854) or
      Php_FunctionComp(1708) or
      Php_FunctionComp(4203) or
      Php_FunctionComp(4204) or
      Php_FunctionComp(4313) or
      Php_FunctionComp(4993) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc288: TSynWebTokenKind;
begin
  if  Php_FunctionComp(593) or
      Php_FunctionComp(2069) or
      Php_FunctionComp(2393) or
      Php_FunctionComp(3059) or
      Php_FunctionComp(4154) or
      Php_FunctionComp(4320) or
      Php_FunctionComp(4648) or
      Php_FunctionComp(4977) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc289: TSynWebTokenKind;
begin
  if  Php_FunctionComp(972) or
      Php_FunctionComp(1070) or
      Php_FunctionComp(2821) or
      Php_FunctionComp(3294) or
      Php_FunctionComp(4462) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc290: TSynWebTokenKind;
begin
  if  Php_FunctionComp(526) or
      Php_FunctionComp(807) or
      Php_FunctionComp(1087) or
      Php_FunctionComp(2466) or
      Php_FunctionComp(2820) or
      Php_FunctionComp(2955) or
      Php_FunctionComp(5025) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc291: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1300) or
      Php_FunctionComp(2495) or
      Php_FunctionComp(2809) or
      Php_FunctionComp(5052) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc292: TSynWebTokenKind;
begin
  if  Php_FunctionComp(852) or
      Php_FunctionComp(1293) or
      Php_FunctionComp(2768) or
      Php_FunctionComp(2805) or
      Php_FunctionComp(3285) or
      Php_FunctionComp(3916) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc293: TSynWebTokenKind;
begin
  if  Php_FunctionComp(811) or
      Php_FunctionComp(901) or
      Php_FunctionComp(1655) or
      Php_FunctionComp(2794) or
      Php_FunctionComp(4740) or
      Php_FunctionComp(4979) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc294: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1013) or
      Php_FunctionComp(3894) or
      Php_FunctionComp(4227) or
      Php_FunctionComp(5019) or
      Php_FunctionComp(5040) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc295: TSynWebTokenKind;
begin
  if  Php_FunctionComp(179) or
      Php_FunctionComp(809) or
      Php_FunctionComp(898) or
      Php_FunctionComp(899) or
      Php_FunctionComp(2971) or
      Php_FunctionComp(4642) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc296: TSynWebTokenKind;
begin
  if  Php_FunctionComp(169) or
      Php_FunctionComp(935) or
      Php_FunctionComp(2817) or
      Php_FunctionComp(4435) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc297: TSynWebTokenKind;
begin
  if  Php_FunctionComp(4005) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc298: TSynWebTokenKind;
begin
  if  Php_FunctionComp(398) or
      Php_FunctionComp(876) or
      Php_FunctionComp(1000) or
      Php_FunctionComp(1537) or
      Php_FunctionComp(2218) or
      Php_FunctionComp(4650) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc299: TSynWebTokenKind;
begin
  if  Php_FunctionComp(155) or
      Php_FunctionComp(427) or
      Php_FunctionComp(865) or
      Php_FunctionComp(873) or
      Php_FunctionComp(1227) or
      Php_FunctionComp(1615) or
      Php_FunctionComp(2080) or
      Php_FunctionComp(2800) or
      Php_FunctionComp(3976) or
      Php_FunctionComp(4158) or
      Php_FunctionComp(4652) or
      Php_FunctionComp(4742) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc300: TSynWebTokenKind;
begin
  if  Php_FunctionComp(181) or
      Php_FunctionComp(859) or
      Php_FunctionComp(1620) or
      Php_FunctionComp(5047) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc302: TSynWebTokenKind;
begin
  if  Php_FunctionComp(944) or
      Php_FunctionComp(2749) or
      Php_FunctionComp(4721) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc303: TSynWebTokenKind;
begin
  if  Php_FunctionComp(987) or
      Php_FunctionComp(2198) or
      Php_FunctionComp(5034) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc304: TSynWebTokenKind;
begin
  if  Php_FunctionComp(863) or
      Php_FunctionComp(2489) or
      Php_FunctionComp(4164) or
      Php_FunctionComp(4741) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc305: TSynWebTokenKind;
begin
  if  Php_FunctionComp(289) or
      Php_FunctionComp(1714) or
      Php_FunctionComp(3269) or
      Php_FunctionComp(4173) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc306: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2799) or
      Php_FunctionComp(4722) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc307: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1304) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc308: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2672) or
      Php_FunctionComp(3997) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc309: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1711) or
      Php_FunctionComp(2750) or
      Php_FunctionComp(4013) or
      Php_FunctionComp(4384) or
      Php_FunctionComp(4416) or
      Php_FunctionComp(4489) or
      Php_FunctionComp(5161) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc310: TSynWebTokenKind;
begin
  if  Php_FunctionComp(947) or
      Php_FunctionComp(1002) or
      Php_FunctionComp(2302) or
      Php_FunctionComp(4461) or
      Php_FunctionComp(4640) or
      Php_FunctionComp(4646) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc311: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2297) or
      Php_FunctionComp(4160) or
      Php_FunctionComp(4163) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc312: TSynWebTokenKind;
begin
  if  Php_FunctionComp(868) or
      Php_FunctionComp(2197) or
      Php_FunctionComp(2470) or
      Php_FunctionComp(2553) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc313: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2671) or
      Php_FunctionComp(4942) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc314: TSynWebTokenKind;
begin
  if  Php_FunctionComp(156) or
      Php_FunctionComp(828) or
      Php_FunctionComp(949) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc315: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5035) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc316: TSynWebTokenKind;
begin
  if  Php_FunctionComp(927) or
      Php_FunctionComp(975) or
      Php_FunctionComp(1014) or
      Php_FunctionComp(2772) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc317: TSynWebTokenKind;
begin
  if  Php_FunctionComp(867) or
      Php_FunctionComp(3058) or
      Php_FunctionComp(4460) or
      Php_FunctionComp(4743) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc318: TSynWebTokenKind;
begin
  if  Php_FunctionComp(842) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc319: TSynWebTokenKind;
begin
  if  Php_FunctionComp(855) or
      Php_FunctionComp(5050) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc321: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1040) or
      Php_FunctionComp(4725) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc322: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2745) or
      Php_FunctionComp(3279) or
      Php_FunctionComp(4720) or
      Php_FunctionComp(5038) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc323: TSynWebTokenKind;
begin
  if  Php_FunctionComp(175) or
      Php_FunctionComp(1550) or
      Php_FunctionComp(1662) or
      Php_FunctionComp(4165) or
      Php_FunctionComp(4403) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc324: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5053) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc325: TSynWebTokenKind;
begin
  if  Php_FunctionComp(853) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc326: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1015) or
      Php_FunctionComp(1078) or
      Php_FunctionComp(1616) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc327: TSynWebTokenKind;
begin
  if  Php_FunctionComp(920) or
      Php_FunctionComp(2824) or
      Php_FunctionComp(3934) or
      Php_FunctionComp(5041) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc328: TSynWebTokenKind;
begin
  if  Php_FunctionComp(810) or
      Php_FunctionComp(2496) or
      Php_FunctionComp(2839) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc329: TSynWebTokenKind;
begin
  if  Php_FunctionComp(945) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc330: TSynWebTokenKind;
begin
  if  Php_FunctionComp(996) or
      Php_FunctionComp(1568) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc331: TSynWebTokenKind;
begin
  if  Php_FunctionComp(829) or
      Php_FunctionComp(1001) or
      Php_FunctionComp(4978) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc332: TSynWebTokenKind;
begin
  if  Php_FunctionComp(874) or
      Php_FunctionComp(2512) or
      Php_FunctionComp(3895) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc333: TSynWebTokenKind;
begin
  if  Php_FunctionComp(861) or
      Php_FunctionComp(5044) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc334: TSynWebTokenKind;
begin
  if  Php_FunctionComp(185) or
      Php_FunctionComp(846) or
      Php_FunctionComp(1294) or
      Php_FunctionComp(2511) or
      Php_FunctionComp(4170) or
      Php_FunctionComp(4996) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc335: TSynWebTokenKind;
begin
  if  Php_FunctionComp(827) or
      Php_FunctionComp(1570) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc336: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1012) or
      Php_FunctionComp(5031) or
      Php_FunctionComp(5156) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc337: TSynWebTokenKind;
begin
  if  Php_FunctionComp(847) or
      Php_FunctionComp(864) or
      Php_FunctionComp(1011) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc338: TSynWebTokenKind;
begin
  if  Php_FunctionComp(182) or
      Php_FunctionComp(845) or
      Php_FunctionComp(860) or
      Php_FunctionComp(3324) or
      Php_FunctionComp(4166) or
      Php_FunctionComp(5051) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc339: TSynWebTokenKind;
begin
  if  Php_FunctionComp(917) or
      Php_FunctionComp(2514) or
      Php_FunctionComp(2826) or
      Php_FunctionComp(4420) or
      Php_FunctionComp(5012) or
      Php_FunctionComp(5030) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc340: TSynWebTokenKind;
begin
  if  Php_FunctionComp(820) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc341: TSynWebTokenKind;
begin
  if  Php_FunctionComp(938) or
      Php_FunctionComp(4169) or
      Php_FunctionComp(5039) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc343: TSynWebTokenKind;
begin
  if  Php_FunctionComp(183) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc344: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5028) or
      Php_FunctionComp(5162) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc345: TSynWebTokenKind;
begin
  if  Php_FunctionComp(840) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc346: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1572) or
      Php_FunctionComp(5049) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc347: TSynWebTokenKind;
begin
  if  Php_FunctionComp(3977) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc348: TSynWebTokenKind;
begin
  if  Php_FunctionComp(168) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc349: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5037) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc351: TSynWebTokenKind;
begin
  if  Php_FunctionComp(816) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc352: TSynWebTokenKind;
begin
  if  Php_FunctionComp(856) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc353: TSynWebTokenKind;
begin
  if  Php_FunctionComp(881) or
      Php_FunctionComp(4171) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc354: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2462) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc357: TSynWebTokenKind;
begin
  if  Php_FunctionComp(841) or
      Php_FunctionComp(2294) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc358: TSynWebTokenKind;
begin
  if  Php_FunctionComp(813) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc359: TSynWebTokenKind;
begin
  if  Php_FunctionComp(184) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc360: TSynWebTokenKind;
begin
  if  Php_FunctionComp(4549) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc361: TSynWebTokenKind;
begin
  if  Php_FunctionComp(4191) or
      Php_FunctionComp(4950) or
      Php_FunctionComp(5006) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc364: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2463) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc366: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5045) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc369: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5032) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc370: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1571) or
      Php_FunctionComp(4997) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc373: TSynWebTokenKind;
begin
  if  Php_FunctionComp(287) or
      Php_FunctionComp(821) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc374: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2298) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc375: TSynWebTokenKind;
begin
  if  Php_FunctionComp(839) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc376: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2515) or
      Php_FunctionComp(4992) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc377: TSynWebTokenKind;
begin
  if  Php_FunctionComp(843) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc385: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5007) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc387: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5092) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc393: TSynWebTokenKind;
begin
  if  Php_FunctionComp(4167) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc399: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5097) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc402: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2494) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc407: TSynWebTokenKind;
begin
  if  Php_FunctionComp(1006) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc426: TSynWebTokenKind;
begin
  if  Php_FunctionComp(900) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc433: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5096) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc439: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5098) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc442: TSynWebTokenKind;
begin
  if  Php_FunctionComp(921) or
      Php_FunctionComp(4995) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc443: TSynWebTokenKind;
begin
  if  Php_FunctionComp(844) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc445: TSynWebTokenKind;
begin
  if  Php_FunctionComp(999) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc450: TSynWebTokenKind;
begin
  if  Php_FunctionComp(2513) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc465: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5099) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc466: TSynWebTokenKind;
begin
  if  Php_FunctionComp(817) or
      Php_FunctionComp(5100) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc487: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5094) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc491: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5093) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc496: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5011) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc520: TSynWebTokenKind;
begin
  if  Php_FunctionComp(5095) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc541: TSynWebTokenKind;
begin
  if  Php_FunctionComp(848) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhP_KeywordFunc560: TSynWebTokenKind;
begin
  if  Php_FunctionComp(849) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;
