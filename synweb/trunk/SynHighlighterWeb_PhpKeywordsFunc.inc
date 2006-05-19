function TSynWebSyn.Php_KeywordIdent: TtkTokenKind;
begin
  Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc14: TtkTokenKind;
begin
  if  PHP_FunctionComp(292) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc15: TtkTokenKind;
begin
  if  PHP_FunctionComp(1852) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc16: TtkTokenKind;
begin
  if  PHP_FunctionComp(819) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc17: TtkTokenKind;
begin
  if  PHP_FunctionComp(1073) or
      PHP_FunctionComp(2586) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc18: TtkTokenKind;
begin
  if  PHP_KeywordComp(18) then
    Result := tkPhpKeyword
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc19: TtkTokenKind;
begin
  if  PHP_KeywordComp(6) or
      PHP_KeywordComp(19) then
    Result := tkPhpKeyword
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc20: TtkTokenKind;
begin
  if  PHP_KeywordComp(8) then
    Result := tkPhpKeyword
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc22: TtkTokenKind;
begin
  if  PHP_FunctionComp(88) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc23: TtkTokenKind;
begin
  if  PHP_FunctionComp(1107) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc24: TtkTokenKind;
begin
  if  PHP_FunctionComp(510) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc25: TtkTokenKind;
begin
  if  PHP_FunctionComp(3703) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc28: TtkTokenKind;
begin
  if  PHP_KeywordComp(10) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(362) or
        PHP_FunctionComp(4158) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc29: TtkTokenKind;
begin
  if  PHP_FunctionComp(368) or
      PHP_FunctionComp(377) or
      PHP_FunctionComp(2149) or
      PHP_FunctionComp(2257) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc30: TtkTokenKind;
begin
  if  PHP_FunctionComp(571) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc31: TtkTokenKind;
begin
  if  PHP_KeywordComp(20) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(814) or
        PHP_FunctionComp(3412) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc32: TtkTokenKind;
begin
  if  PHP_KeywordComp(1) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(1253) or
        PHP_FunctionComp(1259) or
        PHP_FunctionComp(2304) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc33: TtkTokenKind;
begin
  if  PHP_KeywordComp(53) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(2321) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc34: TtkTokenKind;
begin
  if  PHP_FunctionComp(1295) or
      PHP_FunctionComp(2270) or
      PHP_FunctionComp(2271) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc35: TtkTokenKind;
begin
  if  PHP_KeywordComp(11) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(1110) or
        PHP_FunctionComp(2280) or
        PHP_FunctionComp(2359) or
        PHP_FunctionComp(4820) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc36: TtkTokenKind;
begin
  if  PHP_FunctionComp(275) or
      PHP_FunctionComp(276) or
      PHP_FunctionComp(1496) or
      PHP_FunctionComp(1570) or
      PHP_FunctionComp(2353) or
      PHP_FunctionComp(2630) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc37: TtkTokenKind;
begin
  if  PHP_KeywordComp(9) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(295) or
        PHP_FunctionComp(317) or
        PHP_FunctionComp(430) or
        PHP_FunctionComp(796) or
        PHP_FunctionComp(1130) or
        PHP_FunctionComp(3386) or
        PHP_FunctionComp(3964) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc38: TtkTokenKind;
begin
  if  PHP_KeywordComp(27) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(89) or
        PHP_FunctionComp(1289) or
        PHP_FunctionComp(2374) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc39: TtkTokenKind;
begin
  if  PHP_KeywordComp(37) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(802) or
        PHP_FunctionComp(1849) or
        PHP_FunctionComp(3938) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc40: TtkTokenKind;
begin
  if  PHP_KeywordComp(3) or
      PHP_KeywordComp(30) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(294) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc41: TtkTokenKind;
begin
  if  PHP_KeywordComp(21) or
      PHP_KeywordComp(70) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(803) or
        PHP_FunctionComp(1255) or
        PHP_FunctionComp(1580) or
        PHP_FunctionComp(2208) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc42: TtkTokenKind;
begin
  if  PHP_KeywordComp(35) or
      PHP_KeywordComp(49) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(371) or
        PHP_FunctionComp(2217) or
        PHP_FunctionComp(4088) or
        PHP_FunctionComp(4190) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc43: TtkTokenKind;
begin
  if  PHP_KeywordComp(34) or
      PHP_KeywordComp(45) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(270) or
        PHP_FunctionComp(375) or
        PHP_FunctionComp(662) or
        PHP_FunctionComp(799) or
        PHP_FunctionComp(4821) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc44: TtkTokenKind;
begin
  if  PHP_FunctionComp(277) or
      PHP_FunctionComp(1112) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc45: TtkTokenKind;
begin
  if  PHP_KeywordComp(69) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(92) or
        PHP_FunctionComp(312) or
        PHP_FunctionComp(431) or
        PHP_FunctionComp(1143) or
        PHP_FunctionComp(1500) or
        PHP_FunctionComp(3965) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc46: TtkTokenKind;
begin
  if  PHP_FunctionComp(90) or
      PHP_FunctionComp(1360) or
      PHP_FunctionComp(1972) or
      PHP_FunctionComp(1973) or
      PHP_FunctionComp(2266) or
      PHP_FunctionComp(2303) or
      PHP_FunctionComp(3070) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc47: TtkTokenKind;
begin
  if  PHP_FunctionComp(314) or
      PHP_FunctionComp(801) or
      PHP_FunctionComp(1286) or
      PHP_FunctionComp(4858) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc48: TtkTokenKind;
begin
  if  PHP_KeywordComp(16) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(2190) or
        PHP_FunctionComp(3448) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc49: TtkTokenKind;
begin
  if  PHP_KeywordComp(40) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(391) or
        PHP_FunctionComp(700) or
        PHP_FunctionComp(797) or
        PHP_FunctionComp(1359) or
        PHP_FunctionComp(1587) or
        PHP_FunctionComp(2587) or
        PHP_FunctionComp(2623) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc50: TtkTokenKind;
begin
  if  PHP_FunctionComp(369) or
      PHP_FunctionComp(798) or
      PHP_FunctionComp(1497) or
      PHP_FunctionComp(1510) or
      PHP_FunctionComp(1651) or
      PHP_FunctionComp(2272) or
      PHP_FunctionComp(2327) or
      PHP_FunctionComp(2474) or
      PHP_FunctionComp(3262) or
      PHP_FunctionComp(3306) or
      PHP_FunctionComp(4191) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc51: TtkTokenKind;
begin
  if  PHP_FunctionComp(271) or
      PHP_FunctionComp(296) or
      PHP_FunctionComp(436) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc52: TtkTokenKind;
begin
  if  PHP_FunctionComp(197) or
      PHP_FunctionComp(293) or
      PHP_FunctionComp(374) or
      PHP_FunctionComp(1364) or
      PHP_FunctionComp(2202) or
      PHP_FunctionComp(3127) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc53: TtkTokenKind;
begin
  if  PHP_FunctionComp(3371) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc54: TtkTokenKind;
begin
  if  PHP_KeywordComp(0) or
      PHP_KeywordComp(13) or
      PHP_KeywordComp(36) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(392) or
        PHP_FunctionComp(3751) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc55: TtkTokenKind;
begin
  if  PHP_KeywordComp(51) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(1363) or
        PHP_FunctionComp(1404) or
        PHP_FunctionComp(1501) or
        PHP_FunctionComp(2636) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc56: TtkTokenKind;
begin
  if  PHP_KeywordComp(22) or
      PHP_KeywordComp(38) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(355) or
        PHP_FunctionComp(808) or
        PHP_FunctionComp(1291) or
        PHP_FunctionComp(4000) or
        PHP_FunctionComp(4413) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc57: TtkTokenKind;
begin
  if  PHP_KeywordComp(71) or
      PHP_KeywordComp(72) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(707) or
        PHP_FunctionComp(785) or
        PHP_FunctionComp(1234) or
        PHP_FunctionComp(1257) or
        PHP_FunctionComp(1920) or
        PHP_FunctionComp(1974) or
        PHP_FunctionComp(2161) or
        PHP_FunctionComp(3807) or
        PHP_FunctionComp(4192) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc58: TtkTokenKind;
begin
  if  PHP_KeywordComp(32) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(698) or
        PHP_FunctionComp(1145) or
        PHP_FunctionComp(1499) or
        PHP_FunctionComp(1861) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc59: TtkTokenKind;
begin
  if  PHP_KeywordComp(50) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(99) or
        PHP_FunctionComp(310) or
        PHP_FunctionComp(429) or
        PHP_FunctionComp(1436) or
        PHP_FunctionComp(2153) or
        PHP_FunctionComp(2318) or
        PHP_FunctionComp(2484) or
        PHP_FunctionComp(3121) or
        PHP_FunctionComp(3972) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc60: TtkTokenKind;
begin
  if  PHP_KeywordComp(48) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(291) or
        PHP_FunctionComp(358) or
        PHP_FunctionComp(372) or
        PHP_FunctionComp(654) or
        PHP_FunctionComp(1217) or
        PHP_FunctionComp(1585) or
        PHP_FunctionComp(2155) or
        PHP_FunctionComp(2296) or
        PHP_FunctionComp(2603) or
        PHP_FunctionComp(3973) or
        PHP_FunctionComp(4159) or
        PHP_FunctionComp(4868) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc61: TtkTokenKind;
begin
  if  PHP_FunctionComp(697) or
      PHP_FunctionComp(728) or
      PHP_FunctionComp(1574) or
      PHP_FunctionComp(2247) or
      PHP_FunctionComp(2305) or
      PHP_FunctionComp(3275) or
      PHP_FunctionComp(3759) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc62: TtkTokenKind;
begin
  if  PHP_KeywordComp(25) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(191) or
        PHP_FunctionComp(315) or
        PHP_FunctionComp(722) or
        PHP_FunctionComp(1437) or
        PHP_FunctionComp(1438) or
        PHP_FunctionComp(1520) or
        PHP_FunctionComp(2218) or
        PHP_FunctionComp(3449) or
        PHP_FunctionComp(3621) or
        PHP_FunctionComp(4013) or
        PHP_FunctionComp(4414) or
        PHP_FunctionComp(4882) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc63: TtkTokenKind;
begin
  if  PHP_KeywordComp(7) or
      PHP_KeywordComp(58) or
      PHP_KeywordComp(67) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(289) or
        PHP_FunctionComp(376) or
        PHP_FunctionComp(412) or
        PHP_FunctionComp(1239) or
        PHP_FunctionComp(1586) or
        PHP_FunctionComp(1830) or
        PHP_FunctionComp(2358) or
        PHP_FunctionComp(3067) or
        PHP_FunctionComp(3363) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc64: TtkTokenKind;
begin
  if  PHP_KeywordComp(66) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(581) or
        PHP_FunctionComp(815) or
        PHP_FunctionComp(2101) or
        PHP_FunctionComp(4591) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc65: TtkTokenKind;
begin
  if  PHP_KeywordComp(4) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(359) or
        PHP_FunctionComp(696) or
        PHP_FunctionComp(1225) or
        PHP_FunctionComp(1290) or
        PHP_FunctionComp(1456) or
        PHP_FunctionComp(1566) or
        PHP_FunctionComp(1921) or
        PHP_FunctionComp(3147) or
        PHP_FunctionComp(3212) or
        PHP_FunctionComp(3495) or
        PHP_FunctionComp(4200) or
        PHP_FunctionComp(4903) or
        PHP_FunctionComp(5212) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc66: TtkTokenKind;
begin
  if  PHP_FunctionComp(506) or
      PHP_FunctionComp(724) or
      PHP_FunctionComp(1236) or
      PHP_FunctionComp(1287) or
      PHP_FunctionComp(1288) or
      PHP_FunctionComp(1362) or
      PHP_FunctionComp(1509) or
      PHP_FunctionComp(1700) or
      PHP_FunctionComp(2013) or
      PHP_FunctionComp(3461) or
      PHP_FunctionComp(3549) or
      PHP_FunctionComp(3716) or
      PHP_FunctionComp(4907) or
      PHP_FunctionComp(5211) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc67: TtkTokenKind;
begin
  if  PHP_FunctionComp(705) or
      PHP_FunctionComp(710) or
      PHP_FunctionComp(1504) or
      PHP_FunctionComp(4007) or
      PHP_FunctionComp(4864) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc68: TtkTokenKind;
begin
  if  PHP_KeywordComp(42) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(447) or
        PHP_FunctionComp(711) or
        PHP_FunctionComp(809) or
        PHP_FunctionComp(1221) or
        PHP_FunctionComp(1503) or
        PHP_FunctionComp(1518) or
        PHP_FunctionComp(1572) or
        PHP_FunctionComp(3528) or
        PHP_FunctionComp(3975) or
        PHP_FunctionComp(4063) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc69: TtkTokenKind;
begin
  if  PHP_KeywordComp(17) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(405) or
        PHP_FunctionComp(439) or
        PHP_FunctionComp(468) or
        PHP_FunctionComp(732) or
        PHP_FunctionComp(1080) or
        PHP_FunctionComp(1521) or
        PHP_FunctionComp(2017) or
        PHP_FunctionComp(2593) or
        PHP_FunctionComp(3161) or
        PHP_FunctionComp(3232) or
        PHP_FunctionComp(3667) or
        PHP_FunctionComp(4085) or
        PHP_FunctionComp(5123) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc70: TtkTokenKind;
begin
  if  PHP_FunctionComp(726) or
      PHP_FunctionComp(1155) or
      PHP_FunctionComp(1220) or
      PHP_FunctionComp(2010) or
      PHP_FunctionComp(2748) or
      PHP_FunctionComp(3423) or
      PHP_FunctionComp(3684) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc71: TtkTokenKind;
begin
  if  PHP_KeywordComp(14) or
      PHP_KeywordComp(24) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(148) or
        PHP_FunctionComp(190) or
        PHP_FunctionComp(419) or
        PHP_FunctionComp(2128) or
        PHP_FunctionComp(2637) or
        PHP_FunctionComp(3374) or
        PHP_FunctionComp(3808) or
        PHP_FunctionComp(3921) or
        PHP_FunctionComp(4885) or
        PHP_FunctionComp(5065) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc72: TtkTokenKind;
begin
  if  PHP_KeywordComp(47) or
      PHP_KeywordComp(62) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(204) or
        PHP_FunctionComp(601) or
        PHP_FunctionComp(1254) or
        PHP_FunctionComp(1584) or
        PHP_FunctionComp(1982) or
        PHP_FunctionComp(2151) or
        PHP_FunctionComp(2274) or
        PHP_FunctionComp(3544) or
        PHP_FunctionComp(4014) or
        PHP_FunctionComp(4358) or
        PHP_FunctionComp(4931) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc73: TtkTokenKind;
begin
  if  PHP_FunctionComp(272) or
      PHP_FunctionComp(290) or
      PHP_FunctionComp(348) or
      PHP_FunctionComp(432) or
      PHP_FunctionComp(709) or
      PHP_FunctionComp(725) or
      PHP_FunctionComp(1440) or
      PHP_FunctionComp(1528) or
      PHP_FunctionComp(1529) or
      PHP_FunctionComp(1845) or
      PHP_FunctionComp(1977) or
      PHP_FunctionComp(2066) or
      PHP_FunctionComp(2114) or
      PHP_FunctionComp(2273) or
      PHP_FunctionComp(2588) or
      PHP_FunctionComp(3548) or
      PHP_FunctionComp(4011) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc74: TtkTokenKind;
begin
  if  PHP_FunctionComp(98) or
      PHP_FunctionComp(1150) or
      PHP_FunctionComp(1375) or
      PHP_FunctionComp(1698) or
      PHP_FunctionComp(1960) or
      PHP_FunctionComp(2009) or
      PHP_FunctionComp(2039) or
      PHP_FunctionComp(2134) or
      PHP_FunctionComp(2159) or
      PHP_FunctionComp(2239) or
      PHP_FunctionComp(3622) or
      PHP_FunctionComp(3828) or
      PHP_FunctionComp(3987) or
      PHP_FunctionComp(4002) or
      PHP_FunctionComp(4412) or
      PHP_FunctionComp(4894) or
      PHP_FunctionComp(4904) or
      PHP_FunctionComp(4996) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc75: TtkTokenKind;
begin
  if  PHP_FunctionComp(460) or
      PHP_FunctionComp(576) or
      PHP_FunctionComp(670) or
      PHP_FunctionComp(1109) or
      PHP_FunctionComp(1590) or
      PHP_FunctionComp(1692) or
      PHP_FunctionComp(1809) or
      PHP_FunctionComp(1979) or
      PHP_FunctionComp(3129) or
      PHP_FunctionComp(3814) or
      PHP_FunctionComp(3850) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc76: TtkTokenKind;
begin
  if  PHP_FunctionComp(466) or
      PHP_FunctionComp(1258) or
      PHP_FunctionComp(1516) or
      PHP_FunctionComp(1783) or
      PHP_FunctionComp(2056) or
      PHP_FunctionComp(2104) or
      PHP_FunctionComp(2132) or
      PHP_FunctionComp(2152) or
      PHP_FunctionComp(2160) or
      PHP_FunctionComp(2165) or
      PHP_FunctionComp(3375) or
      PHP_FunctionComp(3460) or
      PHP_FunctionComp(3529) or
      PHP_FunctionComp(4367) or
      PHP_FunctionComp(5195) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc77: TtkTokenKind;
begin
  if  PHP_KeywordComp(55) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(360) or
        PHP_FunctionComp(361) or
        PHP_FunctionComp(1241) or
        PHP_FunctionComp(1712) or
        PHP_FunctionComp(2115) or
        PHP_FunctionComp(2297) or
        PHP_FunctionComp(3481) or
        PHP_FunctionComp(3613) or
        PHP_FunctionComp(3839) or
        PHP_FunctionComp(4129) or
        PHP_FunctionComp(4184) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc78: TtkTokenKind;
begin
  if  PHP_FunctionComp(354) or
      PHP_FunctionComp(627) or
      PHP_FunctionComp(807) or
      PHP_FunctionComp(1535) or
      PHP_FunctionComp(1573) or
      PHP_FunctionComp(2002) or
      PHP_FunctionComp(2113) or
      PHP_FunctionComp(2282) or
      PHP_FunctionComp(2599) or
      PHP_FunctionComp(3265) or
      PHP_FunctionComp(3490) or
      PHP_FunctionComp(3655) or
      PHP_FunctionComp(4021) or
      PHP_FunctionComp(4514) or
      PHP_FunctionComp(4921) or
      PHP_FunctionComp(4925) or
      PHP_FunctionComp(5124) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc79: TtkTokenKind;
begin
  if  PHP_KeywordComp(23) or
      PHP_KeywordComp(26) or
      PHP_KeywordComp(68) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(188) or
        PHP_FunctionComp(313) or
        PHP_FunctionComp(378) or
        PHP_FunctionComp(438) or
        PHP_FunctionComp(456) or
        PHP_FunctionComp(650) or
        PHP_FunctionComp(657) or
        PHP_FunctionComp(733) or
        PHP_FunctionComp(1266) or
        PHP_FunctionComp(1372) or
        PHP_FunctionComp(1565) or
        PHP_FunctionComp(2003) or
        PHP_FunctionComp(2542) or
        PHP_FunctionComp(5236) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc80: TtkTokenKind;
begin
  if  PHP_KeywordComp(29) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(660) or
        PHP_FunctionComp(688) or
        PHP_FunctionComp(792) or
        PHP_FunctionComp(1077) or
        PHP_FunctionComp(1228) or
        PHP_FunctionComp(1263) or
        PHP_FunctionComp(1373) or
        PHP_FunctionComp(1493) or
        PHP_FunctionComp(3462) or
        PHP_FunctionComp(4595) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc81: TtkTokenKind;
begin
  if  PHP_KeywordComp(46) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(200) or
        PHP_FunctionComp(721) or
        PHP_FunctionComp(1144) or
        PHP_FunctionComp(1270) or
        PHP_FunctionComp(1403) or
        PHP_FunctionComp(1495) or
        PHP_FunctionComp(2686) or
        PHP_FunctionComp(3148) or
        PHP_FunctionComp(3330) or
        PHP_FunctionComp(3846) or
        PHP_FunctionComp(3988) or
        PHP_FunctionComp(4162) or
        PHP_FunctionComp(4861) or
        PHP_FunctionComp(4906) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc82: TtkTokenKind;
begin
  if  PHP_KeywordComp(64) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(273) or
        PHP_FunctionComp(357) or
        PHP_FunctionComp(512) or
        PHP_FunctionComp(673) or
        PHP_FunctionComp(805) or
        PHP_FunctionComp(1256) or
        PHP_FunctionComp(1264) or
        PHP_FunctionComp(1282) or
        PHP_FunctionComp(1471) or
        PHP_FunctionComp(1482) or
        PHP_FunctionComp(1519) or
        PHP_FunctionComp(1614) or
        PHP_FunctionComp(1668) or
        PHP_FunctionComp(2157) or
        PHP_FunctionComp(2167) or
        PHP_FunctionComp(2330) or
        PHP_FunctionComp(3181) or
        PHP_FunctionComp(3469) or
        PHP_FunctionComp(3849) or
        PHP_FunctionComp(3917) or
        PHP_FunctionComp(3989) or
        PHP_FunctionComp(4825) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc83: TtkTokenKind;
begin
  if  PHP_FunctionComp(156) or
      PHP_FunctionComp(440) or
      PHP_FunctionComp(714) or
      PHP_FunctionComp(734) or
      PHP_FunctionComp(806) or
      PHP_FunctionComp(1439) or
      PHP_FunctionComp(1463) or
      PHP_FunctionComp(1465) or
      PHP_FunctionComp(1568) or
      PHP_FunctionComp(2147) or
      PHP_FunctionComp(2164) or
      PHP_FunctionComp(2210) or
      PHP_FunctionComp(2211) or
      PHP_FunctionComp(2233) or
      PHP_FunctionComp(2386) or
      PHP_FunctionComp(2490) or
      PHP_FunctionComp(2511) or
      PHP_FunctionComp(2611) or
      PHP_FunctionComp(3132) or
      PHP_FunctionComp(3305) or
      PHP_FunctionComp(3792) or
      PHP_FunctionComp(3795) or
      PHP_FunctionComp(4420) or
      PHP_FunctionComp(4923) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc84: TtkTokenKind;
begin
  if  PHP_KeywordComp(5) or
      PHP_KeywordComp(65) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(187) or
        PHP_FunctionComp(246) or
        PHP_FunctionComp(652) or
        PHP_FunctionComp(781) or
        PHP_FunctionComp(1367) or
        PHP_FunctionComp(1389) or
        PHP_FunctionComp(1779) or
        PHP_FunctionComp(2214) or
        PHP_FunctionComp(2222) or
        PHP_FunctionComp(3133) or
        PHP_FunctionComp(3188) or
        PHP_FunctionComp(3331) or
        PHP_FunctionComp(3382) or
        PHP_FunctionComp(3596) or
        PHP_FunctionComp(3643) or
        PHP_FunctionComp(3701) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc85: TtkTokenKind;
begin
  if  PHP_FunctionComp(220) or
      PHP_FunctionComp(393) or
      PHP_FunctionComp(683) or
      PHP_FunctionComp(713) or
      PHP_FunctionComp(1365) or
      PHP_FunctionComp(1368) or
      PHP_FunctionComp(1388) or
      PHP_FunctionComp(1531) or
      PHP_FunctionComp(1648) or
      PHP_FunctionComp(1848) or
      PHP_FunctionComp(2015) or
      PHP_FunctionComp(2049) or
      PHP_FunctionComp(2127) or
      PHP_FunctionComp(2278) or
      PHP_FunctionComp(3645) or
      PHP_FunctionComp(3813) or
      PHP_FunctionComp(4368) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc86: TtkTokenKind;
begin
  if  PHP_FunctionComp(1223) or
      PHP_FunctionComp(1366) or
      PHP_FunctionComp(1407) or
      PHP_FunctionComp(1564) or
      PHP_FunctionComp(1796) or
      PHP_FunctionComp(2071) or
      PHP_FunctionComp(2073) or
      PHP_FunctionComp(2240) or
      PHP_FunctionComp(2300) or
      PHP_FunctionComp(3203) or
      PHP_FunctionComp(3248) or
      PHP_FunctionComp(3264) or
      PHP_FunctionComp(3823) or
      PHP_FunctionComp(4006) or
      PHP_FunctionComp(4522) or
      PHP_FunctionComp(5218) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc87: TtkTokenKind;
begin
  if  PHP_KeywordComp(63) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(1945) or
        PHP_FunctionComp(2031) or
        PHP_FunctionComp(2162) or
        PHP_FunctionComp(2250) or
        PHP_FunctionComp(2285) or
        PHP_FunctionComp(2354) or
        PHP_FunctionComp(3677) or
        PHP_FunctionComp(3824) or
        PHP_FunctionComp(3969) or
        PHP_FunctionComp(4048) or
        PHP_FunctionComp(4198) or
        PHP_FunctionComp(4910) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc88: TtkTokenKind;
begin
  if  PHP_FunctionComp(162) or
      PHP_FunctionComp(227) or
      PHP_FunctionComp(394) or
      PHP_FunctionComp(572) or
      PHP_FunctionComp(702) or
      PHP_FunctionComp(810) or
      PHP_FunctionComp(1505) or
      PHP_FunctionComp(1513) or
      PHP_FunctionComp(1576) or
      PHP_FunctionComp(2216) or
      PHP_FunctionComp(2255) or
      PHP_FunctionComp(2283) or
      PHP_FunctionComp(2413) or
      PHP_FunctionComp(2416) or
      PHP_FunctionComp(3187) or
      PHP_FunctionComp(3240) or
      PHP_FunctionComp(3241) or
      PHP_FunctionComp(3364) or
      PHP_FunctionComp(3590) or
      PHP_FunctionComp(3661) or
      PHP_FunctionComp(3686) or
      PHP_FunctionComp(4066) or
      PHP_FunctionComp(4568) or
      PHP_FunctionComp(4669) or
      PHP_FunctionComp(4827) or
      PHP_FunctionComp(4926) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc89: TtkTokenKind;
begin
  if  PHP_FunctionComp(663) or
      PHP_FunctionComp(1076) or
      PHP_FunctionComp(1156) or
      PHP_FunctionComp(1285) or
      PHP_FunctionComp(1293) or
      PHP_FunctionComp(1406) or
      PHP_FunctionComp(1507) or
      PHP_FunctionComp(1917) or
      PHP_FunctionComp(2008) or
      PHP_FunctionComp(2048) or
      PHP_FunctionComp(2102) or
      PHP_FunctionComp(2133) or
      PHP_FunctionComp(2248) or
      PHP_FunctionComp(2749) or
      PHP_FunctionComp(3422) or
      PHP_FunctionComp(3444) or
      PHP_FunctionComp(3815) or
      PHP_FunctionComp(4121) or
      PHP_FunctionComp(4124) or
      PHP_FunctionComp(4195) or
      PHP_FunctionComp(4196) or
      PHP_FunctionComp(4523) or
      PHP_FunctionComp(5203) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc90: TtkTokenKind;
begin
  if  PHP_FunctionComp(605) or
      PHP_FunctionComp(671) or
      PHP_FunctionComp(1224) or
      PHP_FunctionComp(1383) or
      PHP_FunctionComp(1452) or
      PHP_FunctionComp(1524) or
      PHP_FunctionComp(1571) or
      PHP_FunctionComp(1734) or
      PHP_FunctionComp(1782) or
      PHP_FunctionComp(1792) or
      PHP_FunctionComp(2021) or
      PHP_FunctionComp(2267) or
      PHP_FunctionComp(2269) or
      PHP_FunctionComp(2424) or
      PHP_FunctionComp(2607) or
      PHP_FunctionComp(3242) or
      PHP_FunctionComp(3627) or
      PHP_FunctionComp(3657) or
      PHP_FunctionComp(3689) or
      PHP_FunctionComp(4020) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc91: TtkTokenKind;
begin
  if  PHP_KeywordComp(33) or
      PHP_KeywordComp(56) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(269) or
        PHP_FunctionComp(311) or
        PHP_FunctionComp(528) or
        PHP_FunctionComp(1079) or
        PHP_FunctionComp(1147) or
        PHP_FunctionComp(1277) or
        PHP_FunctionComp(1498) or
        PHP_FunctionComp(1710) or
        PHP_FunctionComp(2138) or
        PHP_FunctionComp(2150) or
        PHP_FunctionComp(2156) or
        PHP_FunctionComp(2486) or
        PHP_FunctionComp(2498) or
        PHP_FunctionComp(3281) or
        PHP_FunctionComp(3593) or
        PHP_FunctionComp(3704) or
        PHP_FunctionComp(3753) or
        PHP_FunctionComp(3820) or
        PHP_FunctionComp(3908) or
        PHP_FunctionComp(3966) or
        PHP_FunctionComp(4193) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc92: TtkTokenKind;
begin
  if  PHP_FunctionComp(97) or
      PHP_FunctionComp(582) or
      PHP_FunctionComp(703) or
      PHP_FunctionComp(1243) or
      PHP_FunctionComp(1267) or
      PHP_FunctionComp(1378) or
      PHP_FunctionComp(1417) or
      PHP_FunctionComp(1441) or
      PHP_FunctionComp(2139) or
      PHP_FunctionComp(2191) or
      PHP_FunctionComp(2223) or
      PHP_FunctionComp(2497) or
      PHP_FunctionComp(2594) or
      PHP_FunctionComp(3236) or
      PHP_FunctionComp(3389) or
      PHP_FunctionComp(3456) or
      PHP_FunctionComp(3592) or
      PHP_FunctionComp(3635) or
      PHP_FunctionComp(3668) or
      PHP_FunctionComp(3752) or
      PHP_FunctionComp(4005) or
      PHP_FunctionComp(4126) or
      PHP_FunctionComp(4139) or
      PHP_FunctionComp(4197) or
      PHP_FunctionComp(4586) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc93: TtkTokenKind;
begin
  if  PHP_KeywordComp(59) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(95) or
        PHP_FunctionComp(242) or
        PHP_FunctionComp(666) or
        PHP_FunctionComp(1229) or
        PHP_FunctionComp(1371) or
        PHP_FunctionComp(1494) or
        PHP_FunctionComp(1536) or
        PHP_FunctionComp(1575) or
        PHP_FunctionComp(1856) or
        PHP_FunctionComp(2019) or
        PHP_FunctionComp(2038) or
        PHP_FunctionComp(2057) or
        PHP_FunctionComp(2238) or
        PHP_FunctionComp(3282) or
        PHP_FunctionComp(3383) or
        PHP_FunctionComp(3525) or
        PHP_FunctionComp(3877) or
        PHP_FunctionComp(3974) or
        PHP_FunctionComp(4160) or
        PHP_FunctionComp(4161) or
        PHP_FunctionComp(4180) or
        PHP_FunctionComp(4202) or
        PHP_FunctionComp(4922) or
        PHP_FunctionComp(4924) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc94: TtkTokenKind;
begin
  if  PHP_FunctionComp(500) or
      PHP_FunctionComp(1608) or
      PHP_FunctionComp(1940) or
      PHP_FunctionComp(2335) or
      PHP_FunctionComp(3162) or
      PHP_FunctionComp(3494) or
      PHP_FunctionComp(3698) or
      PHP_FunctionComp(3905) or
      PHP_FunctionComp(4134) or
      PHP_FunctionComp(4307) or
      PHP_FunctionComp(4312) or
      PHP_FunctionComp(4317) or
      PHP_FunctionComp(4869) or
      PHP_FunctionComp(4940) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc95: TtkTokenKind;
begin
  if  PHP_FunctionComp(93) or
      PHP_FunctionComp(208) or
      PHP_FunctionComp(1111) or
      PHP_FunctionComp(1394) or
      PHP_FunctionComp(1694) or
      PHP_FunctionComp(1798) or
      PHP_FunctionComp(2012) or
      PHP_FunctionComp(2064) or
      PHP_FunctionComp(2197) or
      PHP_FunctionComp(2475) or
      PHP_FunctionComp(2543) or
      PHP_FunctionComp(2618) or
      PHP_FunctionComp(3144) or
      PHP_FunctionComp(3378) or
      PHP_FunctionComp(3760) or
      PHP_FunctionComp(4175) or
      PHP_FunctionComp(4282) or
      PHP_FunctionComp(4585) or
      PHP_FunctionComp(4937) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc96: TtkTokenKind;
begin
  if  PHP_KeywordComp(61) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(418) or
        PHP_FunctionComp(682) or
        PHP_FunctionComp(1279) or
        PHP_FunctionComp(1369) or
        PHP_FunctionComp(1381) or
        PHP_FunctionComp(1610) or
        PHP_FunctionComp(1706) or
        PHP_FunctionComp(2414) or
        PHP_FunctionComp(3168) or
        PHP_FunctionComp(3191) or
        PHP_FunctionComp(3488) or
        PHP_FunctionComp(3501) or
        PHP_FunctionComp(4001) or
        PHP_FunctionComp(4201) or
        PHP_FunctionComp(4870) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc97: TtkTokenKind;
begin
  if  PHP_FunctionComp(193) or
      PHP_FunctionComp(194) or
      PHP_FunctionComp(219) or
      PHP_FunctionComp(1379) or
      PHP_FunctionComp(2129) or
      PHP_FunctionComp(2256) or
      PHP_FunctionComp(2290) or
      PHP_FunctionComp(2621) or
      PHP_FunctionComp(3086) or
      PHP_FunctionComp(3269) or
      PHP_FunctionComp(3379) or
      PHP_FunctionComp(3614) or
      PHP_FunctionComp(3714) or
      PHP_FunctionComp(3869) or
      PHP_FunctionComp(3935) or
      PHP_FunctionComp(3968) or
      PHP_FunctionComp(4018) or
      PHP_FunctionComp(4049) or
      PHP_FunctionComp(4173) or
      PHP_FunctionComp(4208) or
      PHP_FunctionComp(4818) or
      PHP_FunctionComp(4828) or
      PHP_FunctionComp(4911) or
      PHP_FunctionComp(5201) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc98: TtkTokenKind;
begin
  if  PHP_FunctionComp(338) or
      PHP_FunctionComp(351) or
      PHP_FunctionComp(504) or
      PHP_FunctionComp(1074) or
      PHP_FunctionComp(1137) or
      PHP_FunctionComp(1278) or
      PHP_FunctionComp(1391) or
      PHP_FunctionComp(1492) or
      PHP_FunctionComp(1748) or
      PHP_FunctionComp(1787) or
      PHP_FunctionComp(2379) or
      PHP_FunctionComp(3270) or
      PHP_FunctionComp(3273) or
      PHP_FunctionComp(3300) or
      PHP_FunctionComp(3532) or
      PHP_FunctionComp(3541) or
      PHP_FunctionComp(3803) or
      PHP_FunctionComp(3904) or
      PHP_FunctionComp(4451) or
      PHP_FunctionComp(4863) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc99: TtkTokenKind;
begin
  if  PHP_FunctionComp(132) or
      PHP_FunctionComp(210) or
      PHP_FunctionComp(541) or
      PHP_FunctionComp(1230) or
      PHP_FunctionComp(1377) or
      PHP_FunctionComp(1387) or
      PHP_FunctionComp(1454) or
      PHP_FunctionComp(1511) or
      PHP_FunctionComp(1708) or
      PHP_FunctionComp(1788) or
      PHP_FunctionComp(2035) or
      PHP_FunctionComp(2037) or
      PHP_FunctionComp(2059) or
      PHP_FunctionComp(3658) or
      PHP_FunctionComp(4170) or
      PHP_FunctionComp(4204) or
      PHP_FunctionComp(4524) or
      PHP_FunctionComp(4587) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc100: TtkTokenKind;
begin
  if  PHP_FunctionComp(250) or
      PHP_FunctionComp(1120) or
      PHP_FunctionComp(1283) or
      PHP_FunctionComp(1386) or
      PHP_FunctionComp(1502) or
      PHP_FunctionComp(1567) or
      PHP_FunctionComp(2146) or
      PHP_FunctionComp(3156) or
      PHP_FunctionComp(3228) or
      PHP_FunctionComp(3284) or
      PHP_FunctionComp(3654) or
      PHP_FunctionComp(3872) or
      PHP_FunctionComp(3876) or
      PHP_FunctionComp(4832) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc101: TtkTokenKind;
begin
  if  PHP_KeywordComp(15) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(577) or
        PHP_FunctionComp(687) or
        PHP_FunctionComp(782) or
        PHP_FunctionComp(1342) or
        PHP_FunctionComp(1395) or
        PHP_FunctionComp(1490) or
        PHP_FunctionComp(1527) or
        PHP_FunctionComp(1741) or
        PHP_FunctionComp(1871) or
        PHP_FunctionComp(2209) or
        PHP_FunctionComp(3157) or
        PHP_FunctionComp(3159) or
        PHP_FunctionComp(3207) or
        PHP_FunctionComp(3230) or
        PHP_FunctionComp(3252) or
        PHP_FunctionComp(3522) or
        PHP_FunctionComp(3705) or
        PHP_FunctionComp(3810) or
        PHP_FunctionComp(3871) or
        PHP_FunctionComp(4015) or
        PHP_FunctionComp(4341) or
        PHP_FunctionComp(4426) or
        PHP_FunctionComp(4819) or
        PHP_FunctionComp(5205) or
        PHP_FunctionComp(5220) or
        PHP_FunctionComp(5235) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc102: TtkTokenKind;
begin
  if  PHP_KeywordComp(2) or
      PHP_KeywordComp(39) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(232) or
        PHP_FunctionComp(352) or
        PHP_FunctionComp(574) or
        PHP_FunctionComp(789) or
        PHP_FunctionComp(1399) or
        PHP_FunctionComp(1870) or
        PHP_FunctionComp(1922) or
        PHP_FunctionComp(2268) or
        PHP_FunctionComp(2319) or
        PHP_FunctionComp(2545) or
        PHP_FunctionComp(2609) or
        PHP_FunctionComp(3425) or
        PHP_FunctionComp(3574) or
        PHP_FunctionComp(3651) or
        PHP_FunctionComp(3669) or
        PHP_FunctionComp(3796) or
        PHP_FunctionComp(4003) or
        PHP_FunctionComp(4086) or
        PHP_FunctionComp(4125) or
        PHP_FunctionComp(4323) or
        PHP_FunctionComp(4359) or
        PHP_FunctionComp(4372) or
        PHP_FunctionComp(4443) or
        PHP_FunctionComp(4576) or
        PHP_FunctionComp(4932) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc103: TtkTokenKind;
begin
  if  PHP_FunctionComp(373) or
      PHP_FunctionComp(656) or
      PHP_FunctionComp(667) or
      PHP_FunctionComp(699) or
      PHP_FunctionComp(712) or
      PHP_FunctionComp(793) or
      PHP_FunctionComp(813) or
      PHP_FunctionComp(1269) or
      PHP_FunctionComp(1355) or
      PHP_FunctionComp(1484) or
      PHP_FunctionComp(1489) or
      PHP_FunctionComp(1525) or
      PHP_FunctionComp(1577) or
      PHP_FunctionComp(1747) or
      PHP_FunctionComp(1850) or
      PHP_FunctionComp(1868) or
      PHP_FunctionComp(2137) or
      PHP_FunctionComp(2184) or
      PHP_FunctionComp(2212) or
      PHP_FunctionComp(2225) or
      PHP_FunctionComp(2501) or
      PHP_FunctionComp(2550) or
      PHP_FunctionComp(3089) or
      PHP_FunctionComp(3170) or
      PHP_FunctionComp(3190) or
      PHP_FunctionComp(3254) or
      PHP_FunctionComp(3266) or
      PHP_FunctionComp(3294) or
      PHP_FunctionComp(3649) or
      PHP_FunctionComp(3656) or
      PHP_FunctionComp(3827) or
      PHP_FunctionComp(4572) or
      PHP_FunctionComp(4581) or
      PHP_FunctionComp(4817) or
      PHP_FunctionComp(4871) or
      PHP_FunctionComp(4967) or
      PHP_FunctionComp(5066) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc104: TtkTokenKind;
begin
  if  PHP_FunctionComp(453) or
      PHP_FunctionComp(606) or
      PHP_FunctionComp(668) or
      PHP_FunctionComp(730) or
      PHP_FunctionComp(735) or
      PHP_FunctionComp(783) or
      PHP_FunctionComp(784) or
      PHP_FunctionComp(1113) or
      PHP_FunctionComp(1281) or
      PHP_FunctionComp(1316) or
      PHP_FunctionComp(1361) or
      PHP_FunctionComp(1392) or
      PHP_FunctionComp(1455) or
      PHP_FunctionComp(1473) or
      PHP_FunctionComp(1646) or
      PHP_FunctionComp(1799) or
      PHP_FunctionComp(1867) or
      PHP_FunctionComp(1969) or
      PHP_FunctionComp(2219) or
      PHP_FunctionComp(2441) or
      PHP_FunctionComp(2477) or
      PHP_FunctionComp(3069) or
      PHP_FunctionComp(3182) or
      PHP_FunctionComp(3246) or
      PHP_FunctionComp(3486) or
      PHP_FunctionComp(3650) or
      PHP_FunctionComp(4012) or
      PHP_FunctionComp(4089) or
      PHP_FunctionComp(4573) or
      PHP_FunctionComp(4575) or
      PHP_FunctionComp(4902) or
      PHP_FunctionComp(4941) or
      PHP_FunctionComp(5020) or
      PHP_FunctionComp(5219) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc105: TtkTokenKind;
begin
  if  PHP_KeywordComp(12) or
      PHP_KeywordComp(28) or
      PHP_KeywordComp(43) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(488) or
        PHP_FunctionComp(665) or
        PHP_FunctionComp(804) or
        PHP_FunctionComp(1141) or
        PHP_FunctionComp(1148) or
        PHP_FunctionComp(1244) or
        PHP_FunctionComp(1374) or
        PHP_FunctionComp(1451) or
        PHP_FunctionComp(1649) or
        PHP_FunctionComp(1662) or
        PHP_FunctionComp(1784) or
        PHP_FunctionComp(1961) or
        PHP_FunctionComp(1966) or
        PHP_FunctionComp(2253) or
        PHP_FunctionComp(2554) or
        PHP_FunctionComp(2605) or
        PHP_FunctionComp(2628) or
        PHP_FunctionComp(3137) or
        PHP_FunctionComp(3193) or
        PHP_FunctionComp(3537) or
        PHP_FunctionComp(3547) or
        PHP_FunctionComp(3623) or
        PHP_FunctionComp(3688) or
        PHP_FunctionComp(4140) or
        PHP_FunctionComp(4985) or
        PHP_FunctionComp(5186) or
        PHP_FunctionComp(5191) or
        PHP_FunctionComp(5210) or
        PHP_FunctionComp(5227) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc106: TtkTokenKind;
begin
  if  PHP_KeywordComp(44) or
      PHP_KeywordComp(57) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(234) or
        PHP_FunctionComp(427) or
        PHP_FunctionComp(530) or
        PHP_FunctionComp(675) or
        PHP_FunctionComp(822) or
        PHP_FunctionComp(1464) or
        PHP_FunctionComp(1506) or
        PHP_FunctionComp(1517) or
        PHP_FunctionComp(2551) or
        PHP_FunctionComp(2684) or
        PHP_FunctionComp(3130) or
        PHP_FunctionComp(3250) or
        PHP_FunctionComp(3626) or
        PHP_FunctionComp(3756) or
        PHP_FunctionComp(3793) or
        PHP_FunctionComp(4210) or
        PHP_FunctionComp(4310) or
        PHP_FunctionComp(4315) or
        PHP_FunctionComp(4320) or
        PHP_FunctionComp(4373) or
        PHP_FunctionComp(4579) or
        PHP_FunctionComp(5027) or
        PHP_FunctionComp(5189) or
        PHP_FunctionComp(5206) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc107: TtkTokenKind;
begin
  if  PHP_FunctionComp(160) or
      PHP_FunctionComp(409) or
      PHP_FunctionComp(514) or
      PHP_FunctionComp(575) or
      PHP_FunctionComp(741) or
      PHP_FunctionComp(1268) or
      PHP_FunctionComp(1294) or
      PHP_FunctionComp(1472) or
      PHP_FunctionComp(1591) or
      PHP_FunctionComp(1660) or
      PHP_FunctionComp(1711) or
      PHP_FunctionComp(1842) or
      PHP_FunctionComp(2045) or
      PHP_FunctionComp(2116) or
      PHP_FunctionComp(2220) or
      PHP_FunctionComp(2226) or
      PHP_FunctionComp(2331) or
      PHP_FunctionComp(2339) or
      PHP_FunctionComp(2514) or
      PHP_FunctionComp(2898) or
      PHP_FunctionComp(3071) or
      PHP_FunctionComp(3093) or
      PHP_FunctionComp(3218) or
      PHP_FunctionComp(3276) or
      PHP_FunctionComp(3368) or
      PHP_FunctionComp(3445) or
      PHP_FunctionComp(3636) or
      PHP_FunctionComp(3845) or
      PHP_FunctionComp(4016) or
      PHP_FunctionComp(4433) or
      PHP_FunctionComp(4877) or
      PHP_FunctionComp(4928) or
      PHP_FunctionComp(4939) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc108: TtkTokenKind;
begin
  if  PHP_FunctionComp(464) or
      PHP_FunctionComp(524) or
      PHP_FunctionComp(674) or
      PHP_FunctionComp(790) or
      PHP_FunctionComp(1114) or
      PHP_FunctionComp(1124) or
      PHP_FunctionComp(1280) or
      PHP_FunctionComp(1398) or
      PHP_FunctionComp(1488) or
      PHP_FunctionComp(1780) or
      PHP_FunctionComp(1828) or
      PHP_FunctionComp(2014) or
      PHP_FunctionComp(3283) or
      PHP_FunctionComp(3372) or
      PHP_FunctionComp(3385) or
      PHP_FunctionComp(3531) or
      PHP_FunctionComp(3646) or
      PHP_FunctionComp(3910) or
      PHP_FunctionComp(4843) or
      PHP_FunctionComp(4883) or
      PHP_FunctionComp(5048) or
      PHP_FunctionComp(5125) or
      PHP_FunctionComp(5187) or
      PHP_FunctionComp(5198) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc109: TtkTokenKind;
begin
  if  PHP_FunctionComp(258) or
      PHP_FunctionComp(680) or
      PHP_FunctionComp(772) or
      PHP_FunctionComp(955) or
      PHP_FunctionComp(1171) or
      PHP_FunctionComp(1232) or
      PHP_FunctionComp(1265) or
      PHP_FunctionComp(1458) or
      PHP_FunctionComp(1742) or
      PHP_FunctionComp(1793) or
      PHP_FunctionComp(1851) or
      PHP_FunctionComp(2182) or
      PHP_FunctionComp(2316) or
      PHP_FunctionComp(2324) or
      PHP_FunctionComp(2338) or
      PHP_FunctionComp(2598) or
      PHP_FunctionComp(2717) or
      PHP_FunctionComp(2751) or
      PHP_FunctionComp(3464) or
      PHP_FunctionComp(3576) or
      PHP_FunctionComp(3671) or
      PHP_FunctionComp(4294) or
      PHP_FunctionComp(4311) or
      PHP_FunctionComp(4316) or
      PHP_FunctionComp(4321) or
      PHP_FunctionComp(4525) or
      PHP_FunctionComp(4943) or
      PHP_FunctionComp(4987) or
      PHP_FunctionComp(5152) or
      PHP_FunctionComp(5175) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc110: TtkTokenKind;
begin
  if  PHP_FunctionComp(94) or
      PHP_FunctionComp(247) or
      PHP_FunctionComp(267) or
      PHP_FunctionComp(458) or
      PHP_FunctionComp(1033) or
      PHP_FunctionComp(1128) or
      PHP_FunctionComp(1164) or
      PHP_FunctionComp(1467) or
      PHP_FunctionComp(1512) or
      PHP_FunctionComp(1532) or
      PHP_FunctionComp(1718) or
      PHP_FunctionComp(1737) or
      PHP_FunctionComp(1795) or
      PHP_FunctionComp(1821) or
      PHP_FunctionComp(2385) or
      PHP_FunctionComp(2476) or
      PHP_FunctionComp(3160) or
      PHP_FunctionComp(3179) or
      PHP_FunctionComp(3213) or
      PHP_FunctionComp(3231) or
      PHP_FunctionComp(3415) or
      PHP_FunctionComp(3424) or
      PHP_FunctionComp(3454) or
      PHP_FunctionComp(3489) or
      PHP_FunctionComp(3836) or
      PHP_FunctionComp(3887) or
      PHP_FunctionComp(4138) or
      PHP_FunctionComp(4517) or
      PHP_FunctionComp(4562) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc111: TtkTokenKind;
begin
  if  PHP_KeywordComp(31) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(124) or
        PHP_FunctionComp(243) or
        PHP_FunctionComp(255) or
        PHP_FunctionComp(449) or
        PHP_FunctionComp(599) or
        PHP_FunctionComp(664) or
        PHP_FunctionComp(706) or
        PHP_FunctionComp(1247) or
        PHP_FunctionComp(1530) or
        PHP_FunctionComp(1790) or
        PHP_FunctionComp(2061) or
        PHP_FunctionComp(2148) or
        PHP_FunctionComp(2163) or
        PHP_FunctionComp(2279) or
        PHP_FunctionComp(2306) or
        PHP_FunctionComp(2617) or
        PHP_FunctionComp(3102) or
        PHP_FunctionComp(3447) or
        PHP_FunctionComp(3450) or
        PHP_FunctionComp(3533) or
        PHP_FunctionComp(3859) or
        PHP_FunctionComp(4084) or
        PHP_FunctionComp(4823) or
        PHP_FunctionComp(4956) or
        PHP_FunctionComp(4966) or
        PHP_FunctionComp(4988) or
        PHP_FunctionComp(5214) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc112: TtkTokenKind;
begin
  if  PHP_FunctionComp(411) or
      PHP_FunctionComp(489) or
      PHP_FunctionComp(678) or
      PHP_FunctionComp(1750) or
      PHP_FunctionComp(1752) or
      PHP_FunctionComp(2052) or
      PHP_FunctionComp(2106) or
      PHP_FunctionComp(3131) or
      PHP_FunctionComp(3139) or
      PHP_FunctionComp(3178) or
      PHP_FunctionComp(3920) or
      PHP_FunctionComp(3982) or
      PHP_FunctionComp(4207) or
      PHP_FunctionComp(4459) or
      PHP_FunctionComp(4884) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc113: TtkTokenKind;
begin
  if  PHP_FunctionComp(339) or
      PHP_FunctionComp(521) or
      PHP_FunctionComp(653) or
      PHP_FunctionComp(1857) or
      PHP_FunctionComp(1862) or
      PHP_FunctionComp(1959) or
      PHP_FunctionComp(1984) or
      PHP_FunctionComp(2074) or
      PHP_FunctionComp(2075) or
      PHP_FunctionComp(2143) or
      PHP_FunctionComp(2249) or
      PHP_FunctionComp(2461) or
      PHP_FunctionComp(2595) or
      PHP_FunctionComp(3384) or
      PHP_FunctionComp(3509) or
      PHP_FunctionComp(3523) or
      PHP_FunctionComp(3539) or
      PHP_FunctionComp(3666) or
      PHP_FunctionComp(4096) or
      PHP_FunctionComp(4163) or
      PHP_FunctionComp(4209) or
      PHP_FunctionComp(5215) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc114: TtkTokenKind;
begin
  if  PHP_FunctionComp(334) or
      PHP_FunctionComp(548) or
      PHP_FunctionComp(602) or
      PHP_FunctionComp(1169) or
      PHP_FunctionComp(1514) or
      PHP_FunctionComp(1559) or
      PHP_FunctionComp(1993) or
      PHP_FunctionComp(2011) or
      PHP_FunctionComp(2123) or
      PHP_FunctionComp(2158) or
      PHP_FunctionComp(2294) or
      PHP_FunctionComp(2393) or
      PHP_FunctionComp(2434) or
      PHP_FunctionComp(2508) or
      PHP_FunctionComp(2596) or
      PHP_FunctionComp(3293) or
      PHP_FunctionComp(3474) or
      PHP_FunctionComp(3500) or
      PHP_FunctionComp(3535) or
      PHP_FunctionComp(3558) or
      PHP_FunctionComp(3598) or
      PHP_FunctionComp(3848) or
      PHP_FunctionComp(3906) or
      PHP_FunctionComp(4222) or
      PHP_FunctionComp(4302) or
      PHP_FunctionComp(4580) or
      PHP_FunctionComp(4888) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc115: TtkTokenKind;
begin
  if  PHP_FunctionComp(140) or
      PHP_FunctionComp(554) or
      PHP_FunctionComp(727) or
      PHP_FunctionComp(1175) or
      PHP_FunctionComp(1203) or
      PHP_FunctionComp(1384) or
      PHP_FunctionComp(1583) or
      PHP_FunctionComp(1693) or
      PHP_FunctionComp(1818) or
      PHP_FunctionComp(2168) or
      PHP_FunctionComp(2295) or
      PHP_FunctionComp(2690) or
      PHP_FunctionComp(3068) or
      PHP_FunctionComp(3103) or
      PHP_FunctionComp(3135) or
      PHP_FunctionComp(3136) or
      PHP_FunctionComp(3180) or
      PHP_FunctionComp(3192) or
      PHP_FunctionComp(3426) or
      PHP_FunctionComp(3463) or
      PHP_FunctionComp(4087) or
      PHP_FunctionComp(4136) or
      PHP_FunctionComp(4347) or
      PHP_FunctionComp(5022) or
      PHP_FunctionComp(5202) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc116: TtkTokenKind;
begin
  if  PHP_FunctionComp(139) or
      PHP_FunctionComp(257) or
      PHP_FunctionComp(455) or
      PHP_FunctionComp(607) or
      PHP_FunctionComp(736) or
      PHP_FunctionComp(788) or
      PHP_FunctionComp(1204) or
      PHP_FunctionComp(1350) or
      PHP_FunctionComp(1370) or
      PHP_FunctionComp(1385) or
      PHP_FunctionComp(1449) or
      PHP_FunctionComp(1726) or
      PHP_FunctionComp(1800) or
      PHP_FunctionComp(1805) or
      PHP_FunctionComp(1957) or
      PHP_FunctionComp(2125) or
      PHP_FunctionComp(2187) or
      PHP_FunctionComp(2192) or
      PHP_FunctionComp(2499) or
      PHP_FunctionComp(3146) or
      PHP_FunctionComp(3211) or
      PHP_FunctionComp(3414) or
      PHP_FunctionComp(3506) or
      PHP_FunctionComp(3527) or
      PHP_FunctionComp(3754) or
      PHP_FunctionComp(3804) or
      PHP_FunctionComp(3911) or
      PHP_FunctionComp(4565) or
      PHP_FunctionComp(5148) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc117: TtkTokenKind;
begin
  if  PHP_FunctionComp(106) or
      PHP_FunctionComp(253) or
      PHP_FunctionComp(503) or
      PHP_FunctionComp(560) or
      PHP_FunctionComp(1075) or
      PHP_FunctionComp(1219) or
      PHP_FunctionComp(1429) or
      PHP_FunctionComp(1453) or
      PHP_FunctionComp(1553) or
      PHP_FunctionComp(1605) or
      PHP_FunctionComp(1609) or
      PHP_FunctionComp(1769) or
      PHP_FunctionComp(1791) or
      PHP_FunctionComp(2172) or
      PHP_FunctionComp(2183) or
      PHP_FunctionComp(2188) or
      PHP_FunctionComp(2313) or
      PHP_FunctionComp(2381) or
      PHP_FunctionComp(2590) or
      PHP_FunctionComp(2610) or
      PHP_FunctionComp(3205) or
      PHP_FunctionComp(3487) or
      PHP_FunctionComp(3505) or
      PHP_FunctionComp(3540) or
      PHP_FunctionComp(3620) or
      PHP_FunctionComp(3841) or
      PHP_FunctionComp(3937) or
      PHP_FunctionComp(4123) or
      PHP_FunctionComp(4369) or
      PHP_FunctionComp(4516) or
      PHP_FunctionComp(4521) or
      PHP_FunctionComp(4905) or
      PHP_FunctionComp(4918) or
      PHP_FunctionComp(4944) or
      PHP_FunctionComp(4954) or
      PHP_FunctionComp(5217) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc118: TtkTokenKind;
begin
  if  PHP_FunctionComp(516) or
      PHP_FunctionComp(749) or
      PHP_FunctionComp(1123) or
      PHP_FunctionComp(1298) or
      PHP_FunctionComp(1424) or
      PHP_FunctionComp(1582) or
      PHP_FunctionComp(1673) or
      PHP_FunctionComp(1878) or
      PHP_FunctionComp(2171) or
      PHP_FunctionComp(2382) or
      PHP_FunctionComp(2394) or
      PHP_FunctionComp(2430) or
      PHP_FunctionComp(2585) or
      PHP_FunctionComp(2589) or
      PHP_FunctionComp(2600) or
      PHP_FunctionComp(3271) or
      PHP_FunctionComp(3430) or
      PHP_FunctionComp(3452) or
      PHP_FunctionComp(3638) or
      PHP_FunctionComp(3695) or
      PHP_FunctionComp(3699) or
      PHP_FunctionComp(3860) or
      PHP_FunctionComp(4165) or
      PHP_FunctionComp(4199) or
      PHP_FunctionComp(4206) or
      PHP_FunctionComp(4958) or
      PHP_FunctionComp(5017) or
      PHP_FunctionComp(5158) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc119: TtkTokenKind;
begin
  if  PHP_FunctionComp(186) or
      PHP_FunctionComp(251) or
      PHP_FunctionComp(333) or
      PHP_FunctionComp(390) or
      PHP_FunctionComp(454) or
      PHP_FunctionComp(517) or
      PHP_FunctionComp(820) or
      PHP_FunctionComp(971) or
      PHP_FunctionComp(1227) or
      PHP_FunctionComp(1786) or
      PHP_FunctionComp(1801) or
      PHP_FunctionComp(1847) or
      PHP_FunctionComp(1925) or
      PHP_FunctionComp(2111) or
      PHP_FunctionComp(2307) or
      PHP_FunctionComp(2485) or
      PHP_FunctionComp(2692) or
      PHP_FunctionComp(2899) or
      PHP_FunctionComp(3080) or
      PHP_FunctionComp(3104) or
      PHP_FunctionComp(3298) or
      PHP_FunctionComp(3502) or
      PHP_FunctionComp(3519) or
      PHP_FunctionComp(4179) or
      PHP_FunctionComp(4288) or
      PHP_FunctionComp(4384) or
      PHP_FunctionComp(4672) or
      PHP_FunctionComp(4886) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc120: TtkTokenKind;
begin
  if  PHP_FunctionComp(224) or
      PHP_FunctionComp(628) or
      PHP_FunctionComp(701) or
      PHP_FunctionComp(708) or
      PHP_FunctionComp(1448) or
      PHP_FunctionComp(1581) or
      PHP_FunctionComp(1588) or
      PHP_FunctionComp(1777) or
      PHP_FunctionComp(1992) or
      PHP_FunctionComp(1994) or
      PHP_FunctionComp(2007) or
      PHP_FunctionComp(2404) or
      PHP_FunctionComp(2695) or
      PHP_FunctionComp(2752) or
      PHP_FunctionComp(3443) or
      PHP_FunctionComp(3609) or
      PHP_FunctionComp(3618) or
      PHP_FunctionComp(3631) or
      PHP_FunctionComp(3670) or
      PHP_FunctionComp(3806) or
      PHP_FunctionComp(3809) or
      PHP_FunctionComp(3842) or
      PHP_FunctionComp(4074) or
      PHP_FunctionComp(4127) or
      PHP_FunctionComp(4419) or
      PHP_FunctionComp(4446) or
      PHP_FunctionComp(4574) or
      PHP_FunctionComp(4635) or
      PHP_FunctionComp(4645) or
      PHP_FunctionComp(4947) or
      PHP_FunctionComp(5127) or
      PHP_FunctionComp(5149) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc121: TtkTokenKind;
begin
  if  PHP_FunctionComp(128) or
      PHP_FunctionComp(300) or
      PHP_FunctionComp(446) or
      PHP_FunctionComp(620) or
      PHP_FunctionComp(817) or
      PHP_FunctionComp(1251) or
      PHP_FunctionComp(1382) or
      PHP_FunctionComp(1400) or
      PHP_FunctionComp(1705) or
      PHP_FunctionComp(1785) or
      PHP_FunctionComp(1860) or
      PHP_FunctionComp(1919) or
      PHP_FunctionComp(2535) or
      PHP_FunctionComp(2601) or
      PHP_FunctionComp(3268) or
      PHP_FunctionComp(3279) or
      PHP_FunctionComp(3440) or
      PHP_FunctionComp(3482) or
      PHP_FunctionComp(3542) or
      PHP_FunctionComp(3578) or
      PHP_FunctionComp(3633) or
      PHP_FunctionComp(3881) or
      PHP_FunctionComp(3914) or
      PHP_FunctionComp(3996) or
      PHP_FunctionComp(4130) or
      PHP_FunctionComp(4169) or
      PHP_FunctionComp(4322) or
      PHP_FunctionComp(4343) or
      PHP_FunctionComp(4735) or
      PHP_FunctionComp(4933) or
      PHP_FunctionComp(5154) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc122: TtkTokenKind;
begin
  if  PHP_FunctionComp(196) or
      PHP_FunctionComp(433) or
      PHP_FunctionComp(462) or
      PHP_FunctionComp(509) or
      PHP_FunctionComp(771) or
      PHP_FunctionComp(791) or
      PHP_FunctionComp(1170) or
      PHP_FunctionComp(1819) or
      PHP_FunctionComp(1827) or
      PHP_FunctionComp(1859) or
      PHP_FunctionComp(1975) or
      PHP_FunctionComp(1980) or
      PHP_FunctionComp(2000) or
      PHP_FunctionComp(2027) or
      PHP_FunctionComp(2069) or
      PHP_FunctionComp(2397) or
      PHP_FunctionComp(2502) or
      PHP_FunctionComp(2591) or
      PHP_FunctionComp(2624) or
      PHP_FunctionComp(2640) or
      PHP_FunctionComp(2651) or
      PHP_FunctionComp(2933) or
      PHP_FunctionComp(3171) or
      PHP_FunctionComp(3184) or
      PHP_FunctionComp(3206) or
      PHP_FunctionComp(3251) or
      PHP_FunctionComp(3280) or
      PHP_FunctionComp(3421) or
      PHP_FunctionComp(3477) or
      PHP_FunctionComp(3565) or
      PHP_FunctionComp(3630) or
      PHP_FunctionComp(3758) or
      PHP_FunctionComp(3898) or
      PHP_FunctionComp(4131) or
      PHP_FunctionComp(4168) or
      PHP_FunctionComp(4194) or
      PHP_FunctionComp(4221) or
      PHP_FunctionComp(4406) or
      PHP_FunctionComp(4515) or
      PHP_FunctionComp(4654) or
      PHP_FunctionComp(4862) or
      PHP_FunctionComp(4876) or
      PHP_FunctionComp(5067) or
      PHP_FunctionComp(5144) or
      PHP_FunctionComp(5193) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc123: TtkTokenKind;
begin
  if  PHP_FunctionComp(241) or
      PHP_FunctionComp(618) or
      PHP_FunctionComp(731) or
      PHP_FunctionComp(1082) or
      PHP_FunctionComp(1185) or
      PHP_FunctionComp(1667) or
      PHP_FunctionComp(1690) or
      PHP_FunctionComp(1727) or
      PHP_FunctionComp(1810) or
      PHP_FunctionComp(1872) or
      PHP_FunctionComp(2186) or
      PHP_FunctionComp(2189) or
      PHP_FunctionComp(2425) or
      PHP_FunctionComp(2546) or
      PHP_FunctionComp(2910) or
      PHP_FunctionComp(3413) or
      PHP_FunctionComp(3605) or
      PHP_FunctionComp(3830) or
      PHP_FunctionComp(3854) or
      PHP_FunctionComp(3880) or
      PHP_FunctionComp(3934) or
      PHP_FunctionComp(3992) or
      PHP_FunctionComp(3995) or
      PHP_FunctionComp(4017) or
      PHP_FunctionComp(4146) or
      PHP_FunctionComp(4223) or
      PHP_FunctionComp(4278) or
      PHP_FunctionComp(4342) or
      PHP_FunctionComp(4393) or
      PHP_FunctionComp(4567) or
      PHP_FunctionComp(4652) or
      PHP_FunctionComp(4653) or
      PHP_FunctionComp(4952) or
      PHP_FunctionComp(5136) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc124: TtkTokenKind;
begin
  if  PHP_FunctionComp(225) or
      PHP_FunctionComp(288) or
      PHP_FunctionComp(406) or
      PHP_FunctionComp(490) or
      PHP_FunctionComp(526) or
      PHP_FunctionComp(549) or
      PHP_FunctionComp(604) or
      PHP_FunctionComp(704) or
      PHP_FunctionComp(753) or
      PHP_FunctionComp(1508) or
      PHP_FunctionComp(1515) or
      PHP_FunctionComp(1645) or
      PHP_FunctionComp(1728) or
      PHP_FunctionComp(2005) or
      PHP_FunctionComp(2034) or
      PHP_FunctionComp(2337) or
      PHP_FunctionComp(2515) or
      PHP_FunctionComp(2548) or
      PHP_FunctionComp(3165) or
      PHP_FunctionComp(3247) or
      PHP_FunctionComp(3278) or
      PHP_FunctionComp(3442) or
      PHP_FunctionComp(3455) or
      PHP_FunctionComp(3696) or
      PHP_FunctionComp(3755) or
      PHP_FunctionComp(3816) or
      PHP_FunctionComp(3993) or
      PHP_FunctionComp(4570) or
      PHP_FunctionComp(4657) or
      PHP_FunctionComp(4660) or
      PHP_FunctionComp(4661) or
      PHP_FunctionComp(4949) or
      PHP_FunctionComp(4986) or
      PHP_FunctionComp(4990) or
      PHP_FunctionComp(5208) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc125: TtkTokenKind;
begin
  if  PHP_FunctionComp(91) or
      PHP_FunctionComp(138) or
      PHP_FunctionComp(254) or
      PHP_FunctionComp(297) or
      PHP_FunctionComp(478) or
      PHP_FunctionComp(1117) or
      PHP_FunctionComp(1186) or
      PHP_FunctionComp(1879) or
      PHP_FunctionComp(1999) or
      PHP_FunctionComp(2067) or
      PHP_FunctionComp(2205) or
      PHP_FunctionComp(2241) or
      PHP_FunctionComp(2606) or
      PHP_FunctionComp(2986) or
      PHP_FunctionComp(3073) or
      PHP_FunctionComp(3166) or
      PHP_FunctionComp(3214) or
      PHP_FunctionComp(3466) or
      PHP_FunctionComp(3555) or
      PHP_FunctionComp(3577) or
      PHP_FunctionComp(3624) or
      PHP_FunctionComp(3639) or
      PHP_FunctionComp(3660) or
      PHP_FunctionComp(3690) or
      PHP_FunctionComp(3831) or
      PHP_FunctionComp(3838) or
      PHP_FunctionComp(3991) or
      PHP_FunctionComp(4167) or
      PHP_FunctionComp(4211) or
      PHP_FunctionComp(4327) or
      PHP_FunctionComp(4366) or
      PHP_FunctionComp(4578) or
      PHP_FunctionComp(4793) or
      PHP_FunctionComp(4826) or
      PHP_FunctionComp(5037) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc126: TtkTokenKind;
begin
  if  PHP_KeywordComp(41) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(129) or
        PHP_FunctionComp(280) or
        PHP_FunctionComp(353) or
        PHP_FunctionComp(367) or
        PHP_FunctionComp(370) or
        PHP_FunctionComp(507) or
        PHP_FunctionComp(658) or
        PHP_FunctionComp(717) or
        PHP_FunctionComp(750) or
        PHP_FunctionComp(1176) or
        PHP_FunctionComp(1207) or
        PHP_FunctionComp(1447) or
        PHP_FunctionComp(1817) or
        PHP_FunctionComp(2046) or
        PHP_FunctionComp(2077) or
        PHP_FunctionComp(2124) or
        PHP_FunctionComp(2154) or
        PHP_FunctionComp(3087) or
        PHP_FunctionComp(3164) or
        PHP_FunctionComp(3285) or
        PHP_FunctionComp(3545) or
        PHP_FunctionComp(3662) or
        PHP_FunctionComp(3931) or
        PHP_FunctionComp(3942) or
        PHP_FunctionComp(3943) or
        PHP_FunctionComp(4181) or
        PHP_FunctionComp(4432) or
        PHP_FunctionComp(4513) or
        PHP_FunctionComp(4927) or
        PHP_FunctionComp(4989) or
        PHP_FunctionComp(5190) or
        PHP_FunctionComp(5192) or
        PHP_FunctionComp(5194) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc127: TtkTokenKind;
begin
  if  PHP_FunctionComp(146) or
      PHP_FunctionComp(249) or
      PHP_FunctionComp(256) or
      PHP_FunctionComp(1844) or
      PHP_FunctionComp(2026) or
      PHP_FunctionComp(2140) or
      PHP_FunctionComp(2350) or
      PHP_FunctionComp(2352) or
      PHP_FunctionComp(2633) or
      PHP_FunctionComp(2693) or
      PHP_FunctionComp(2909) or
      PHP_FunctionComp(3115) or
      PHP_FunctionComp(3233) or
      PHP_FunctionComp(3291) or
      PHP_FunctionComp(3429) or
      PHP_FunctionComp(3491) or
      PHP_FunctionComp(3738) or
      PHP_FunctionComp(4325) or
      PHP_FunctionComp(4452) or
      PHP_FunctionComp(4468) or
      PHP_FunctionComp(4593) or
      PHP_FunctionComp(4668) or
      PHP_FunctionComp(4824) or
      PHP_FunctionComp(4963) or
      PHP_FunctionComp(5025) or
      PHP_FunctionComp(5172) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc128: TtkTokenKind;
begin
  if  PHP_FunctionComp(110) or
      PHP_FunctionComp(199) or
      PHP_FunctionComp(416) or
      PHP_FunctionComp(441) or
      PHP_FunctionComp(473) or
      PHP_FunctionComp(527) or
      PHP_FunctionComp(573) or
      PHP_FunctionComp(624) or
      PHP_FunctionComp(626) or
      PHP_FunctionComp(737) or
      PHP_FunctionComp(774) or
      PHP_FunctionComp(787) or
      PHP_FunctionComp(1260) or
      PHP_FunctionComp(1292) or
      PHP_FunctionComp(1309) or
      PHP_FunctionComp(1466) or
      PHP_FunctionComp(1534) or
      PHP_FunctionComp(2144) or
      PHP_FunctionComp(2207) or
      PHP_FunctionComp(2293) or
      PHP_FunctionComp(2702) or
      PHP_FunctionComp(3219) or
      PHP_FunctionComp(3629) or
      PHP_FunctionComp(3833) or
      PHP_FunctionComp(4594) or
      PHP_FunctionComp(4873) or
      PHP_FunctionComp(4942) or
      PHP_FunctionComp(4953) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc129: TtkTokenKind;
begin
  if  PHP_FunctionComp(217) or
      PHP_FunctionComp(229) or
      PHP_FunctionComp(461) or
      PHP_FunctionComp(587) or
      PHP_FunctionComp(681) or
      PHP_FunctionComp(1165) or
      PHP_FunctionComp(1214) or
      PHP_FunctionComp(1816) or
      PHP_FunctionComp(1858) or
      PHP_FunctionComp(1877) or
      PHP_FunctionComp(2042) or
      PHP_FunctionComp(2405) or
      PHP_FunctionComp(2516) or
      PHP_FunctionComp(3172) or
      PHP_FunctionComp(3189) or
      PHP_FunctionComp(3220) or
      PHP_FunctionComp(3277) or
      PHP_FunctionComp(3970) or
      PHP_FunctionComp(4563) or
      PHP_FunctionComp(4948) or
      PHP_FunctionComp(4951) or
      PHP_FunctionComp(4961) or
      PHP_FunctionComp(5029) or
      PHP_FunctionComp(5030) or
      PHP_FunctionComp(5188) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc130: TtkTokenKind;
begin
  if  PHP_KeywordComp(60) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(323) or
        PHP_FunctionComp(513) or
        PHP_FunctionComp(529) or
        PHP_FunctionComp(585) or
        PHP_FunctionComp(686) or
        PHP_FunctionComp(812) or
        PHP_FunctionComp(821) or
        PHP_FunctionComp(1166) or
        PHP_FunctionComp(1177) or
        PHP_FunctionComp(1206) or
        PHP_FunctionComp(1346) or
        PHP_FunctionComp(1450) or
        PHP_FunctionComp(1491) or
        PHP_FunctionComp(1537) or
        PHP_FunctionComp(1720) or
        PHP_FunctionComp(1970) or
        PHP_FunctionComp(2328) or
        PHP_FunctionComp(2426) or
        PHP_FunctionComp(2513) or
        PHP_FunctionComp(2703) or
        PHP_FunctionComp(2783) or
        PHP_FunctionComp(2931) or
        PHP_FunctionComp(3158) or
        PHP_FunctionComp(3201) or
        PHP_FunctionComp(3229) or
        PHP_FunctionComp(3479) or
        PHP_FunctionComp(3480) or
        PHP_FunctionComp(3581) or
        PHP_FunctionComp(3603) or
        PHP_FunctionComp(3693) or
        PHP_FunctionComp(3800) or
        PHP_FunctionComp(4083) or
        PHP_FunctionComp(4171) or
        PHP_FunctionComp(4411) or
        PHP_FunctionComp(4659) or
        PHP_FunctionComp(5140) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc131: TtkTokenKind;
begin
  if  PHP_FunctionComp(116) or
      PHP_FunctionComp(332) or
      PHP_FunctionComp(1100) or
      PHP_FunctionComp(1116) or
      PHP_FunctionComp(1242) or
      PHP_FunctionComp(1615) or
      PHP_FunctionComp(1647) or
      PHP_FunctionComp(2024) or
      PHP_FunctionComp(2117) or
      PHP_FunctionComp(2131) or
      PHP_FunctionComp(2224) or
      PHP_FunctionComp(2493) or
      PHP_FunctionComp(2716) or
      PHP_FunctionComp(2750) or
      PHP_FunctionComp(2927) or
      PHP_FunctionComp(3074) or
      PHP_FunctionComp(3095) or
      PHP_FunctionComp(3106) or
      PHP_FunctionComp(3195) or
      PHP_FunctionComp(3272) or
      PHP_FunctionComp(3289) or
      PHP_FunctionComp(3380) or
      PHP_FunctionComp(3453) or
      PHP_FunctionComp(3530) or
      PHP_FunctionComp(3571) or
      PHP_FunctionComp(3659) or
      PHP_FunctionComp(3664) or
      PHP_FunctionComp(4205) or
      PHP_FunctionComp(4571) or
      PHP_FunctionComp(4603) or
      PHP_FunctionComp(4639) or
      PHP_FunctionComp(4930) or
      PHP_FunctionComp(4955) or
      PHP_FunctionComp(5134) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc132: TtkTokenKind;
begin
  if  PHP_FunctionComp(75) or
      PHP_FunctionComp(195) or
      PHP_FunctionComp(567) or
      PHP_FunctionComp(589) or
      PHP_FunctionComp(729) or
      PHP_FunctionComp(823) or
      PHP_FunctionComp(826) or
      PHP_FunctionComp(1019) or
      PHP_FunctionComp(1187) or
      PHP_FunctionComp(1188) or
      PHP_FunctionComp(1213) or
      PHP_FunctionComp(1347) or
      PHP_FunctionComp(1405) or
      PHP_FunctionComp(1637) or
      PHP_FunctionComp(1688) or
      PHP_FunctionComp(2023) or
      PHP_FunctionComp(2070) or
      PHP_FunctionComp(2103) or
      PHP_FunctionComp(2166) or
      PHP_FunctionComp(2173) or
      PHP_FunctionComp(2227) or
      PHP_FunctionComp(2468) or
      PHP_FunctionComp(2471) or
      PHP_FunctionComp(2736) or
      PHP_FunctionComp(2792) or
      PHP_FunctionComp(3108) or
      PHP_FunctionComp(3128) or
      PHP_FunctionComp(3163) or
      PHP_FunctionComp(4279) or
      PHP_FunctionComp(4401) or
      PHP_FunctionComp(4431) or
      PHP_FunctionComp(4444) or
      PHP_FunctionComp(4834) or
      PHP_FunctionComp(4838) or
      PHP_FunctionComp(5069) or
      PHP_FunctionComp(5133) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc133: TtkTokenKind;
begin
  if  PHP_KeywordComp(52) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(233) or
        PHP_FunctionComp(379) or
        PHP_FunctionComp(445) or
        PHP_FunctionComp(493) or
        PHP_FunctionComp(1311) or
        PHP_FunctionComp(1731) or
        PHP_FunctionComp(1776) or
        PHP_FunctionComp(1803) or
        PHP_FunctionComp(1892) or
        PHP_FunctionComp(2041) or
        PHP_FunctionComp(2050) or
        PHP_FunctionComp(2258) or
        PHP_FunctionComp(2950) or
        PHP_FunctionComp(3573) or
        PHP_FunctionComp(3715) or
        PHP_FunctionComp(3717) or
        PHP_FunctionComp(3802) or
        PHP_FunctionComp(3915) or
        PHP_FunctionComp(3948) or
        PHP_FunctionComp(4099) or
        PHP_FunctionComp(4519) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc134: TtkTokenKind;
begin
  if  PHP_FunctionComp(435) or
      PHP_FunctionComp(719) or
      PHP_FunctionComp(738) or
      PHP_FunctionComp(1180) or
      PHP_FunctionComp(1804) or
      PHP_FunctionComp(2206) or
      PHP_FunctionComp(2301) or
      PHP_FunctionComp(2351) or
      PHP_FunctionComp(2718) or
      PHP_FunctionComp(2809) or
      PHP_FunctionComp(3369) or
      PHP_FunctionComp(3376) or
      PHP_FunctionComp(3497) or
      PHP_FunctionComp(3538) or
      PHP_FunctionComp(3619) or
      PHP_FunctionComp(3817) or
      PHP_FunctionComp(3861) or
      PHP_FunctionComp(4152) or
      PHP_FunctionComp(4518) or
      PHP_FunctionComp(4577) or
      PHP_FunctionComp(4636) or
      PHP_FunctionComp(4833) or
      PHP_FunctionComp(4957) or
      PHP_FunctionComp(4992) or
      PHP_FunctionComp(5039) or
      PHP_FunctionComp(5142) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc135: TtkTokenKind;
begin
  if  PHP_FunctionComp(76) or
      PHP_FunctionComp(144) or
      PHP_FunctionComp(324) or
      PHP_FunctionComp(363) or
      PHP_FunctionComp(437) or
      PHP_FunctionComp(622) or
      PHP_FunctionComp(625) or
      PHP_FunctionComp(677) or
      PHP_FunctionComp(754) or
      PHP_FunctionComp(767) or
      PHP_FunctionComp(944) or
      PHP_FunctionComp(1125) or
      PHP_FunctionComp(1317) or
      PHP_FunctionComp(1578) or
      PHP_FunctionComp(1766) or
      PHP_FunctionComp(1802) or
      PHP_FunctionComp(2196) or
      PHP_FunctionComp(2691) or
      PHP_FunctionComp(2696) or
      PHP_FunctionComp(2897) or
      PHP_FunctionComp(3255) or
      PHP_FunctionComp(3373) or
      PHP_FunctionComp(3446) or
      PHP_FunctionComp(3723) or
      PHP_FunctionComp(3847) or
      PHP_FunctionComp(3913) or
      PHP_FunctionComp(3997) or
      PHP_FunctionComp(4671) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc136: TtkTokenKind;
begin
  if  PHP_FunctionComp(115) or
      PHP_FunctionComp(349) or
      PHP_FunctionComp(402) or
      PHP_FunctionComp(410) or
      PHP_FunctionComp(515) or
      PHP_FunctionComp(562) or
      PHP_FunctionComp(597) or
      PHP_FunctionComp(651) or
      PHP_FunctionComp(1065) or
      PHP_FunctionComp(1353) or
      PHP_FunctionComp(1462) or
      PHP_FunctionComp(1820) or
      PHP_FunctionComp(1882) or
      PHP_FunctionComp(2122) or
      PHP_FunctionComp(2215) or
      PHP_FunctionComp(2288) or
      PHP_FunctionComp(2315) or
      PHP_FunctionComp(2325) or
      PHP_FunctionComp(2506) or
      PHP_FunctionComp(3116) or
      PHP_FunctionComp(3217) or
      PHP_FunctionComp(3370) or
      PHP_FunctionComp(3546) or
      PHP_FunctionComp(3600) or
      PHP_FunctionComp(4043) or
      PHP_FunctionComp(4065) or
      PHP_FunctionComp(4092) or
      PHP_FunctionComp(4214) or
      PHP_FunctionComp(4333) or
      PHP_FunctionComp(4360) or
      PHP_FunctionComp(4377) or
      PHP_FunctionComp(4840) or
      PHP_FunctionComp(5141) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc137: TtkTokenKind;
begin
  if  PHP_FunctionComp(463) or
      PHP_FunctionComp(563) or
      PHP_FunctionComp(569) or
      PHP_FunctionComp(689) or
      PHP_FunctionComp(752) or
      PHP_FunctionComp(1045) or
      PHP_FunctionComp(1139) or
      PHP_FunctionComp(1184) or
      PHP_FunctionComp(1401) or
      PHP_FunctionComp(1652) or
      PHP_FunctionComp(1864) or
      PHP_FunctionComp(1881) or
      PHP_FunctionComp(2063) or
      PHP_FunctionComp(2537) or
      PHP_FunctionComp(2647) or
      PHP_FunctionComp(2704) or
      PHP_FunctionComp(2705) or
      PHP_FunctionComp(3274) or
      PHP_FunctionComp(3352) or
      PHP_FunctionComp(3750) or
      PHP_FunctionComp(3834) or
      PHP_FunctionComp(3990) or
      PHP_FunctionComp(4147) or
      PHP_FunctionComp(4346) or
      PHP_FunctionComp(4370) or
      PHP_FunctionComp(4501) or
      PHP_FunctionComp(4641) or
      PHP_FunctionComp(4795) or
      PHP_FunctionComp(5021) or
      PHP_FunctionComp(5040) or
      PHP_FunctionComp(5128) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc138: TtkTokenKind;
begin
  if  PHP_FunctionComp(214) or
      PHP_FunctionComp(218) or
      PHP_FunctionComp(1041) or
      PHP_FunctionComp(1638) or
      PHP_FunctionComp(1661) or
      PHP_FunctionComp(2170) or
      PHP_FunctionComp(2440) or
      PHP_FunctionComp(2518) or
      PHP_FunctionComp(2608) or
      PHP_FunctionComp(2916) or
      PHP_FunctionComp(2964) or
      PHP_FunctionComp(3105) or
      PHP_FunctionComp(3215) or
      PHP_FunctionComp(3222) or
      PHP_FunctionComp(3465) or
      PHP_FunctionComp(3507) or
      PHP_FunctionComp(3933) or
      PHP_FunctionComp(4164) or
      PHP_FunctionComp(4234) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc139: TtkTokenKind;
begin
  if  PHP_FunctionComp(485) or
      PHP_FunctionComp(505) or
      PHP_FunctionComp(603) or
      PHP_FunctionComp(690) or
      PHP_FunctionComp(1167) or
      PHP_FunctionComp(1284) or
      PHP_FunctionComp(1343) or
      PHP_FunctionComp(1415) or
      PHP_FunctionComp(1653) or
      PHP_FunctionComp(1685) or
      PHP_FunctionComp(1813) or
      PHP_FunctionComp(1832) or
      PHP_FunctionComp(1983) or
      PHP_FunctionComp(1991) or
      PHP_FunctionComp(2062) or
      PHP_FunctionComp(2466) or
      PHP_FunctionComp(2512) or
      PHP_FunctionComp(2597) or
      PHP_FunctionComp(2666) or
      PHP_FunctionComp(2698) or
      PHP_FunctionComp(2839) or
      PHP_FunctionComp(2842) or
      PHP_FunctionComp(2915) or
      PHP_FunctionComp(2945) or
      PHP_FunctionComp(3329) or
      PHP_FunctionComp(3822) or
      PHP_FunctionComp(3862) or
      PHP_FunctionComp(3971) or
      PHP_FunctionComp(4376) or
      PHP_FunctionComp(4583) or
      PHP_FunctionComp(4602) or
      PHP_FunctionComp(4607) or
      PHP_FunctionComp(4719) or
      PHP_FunctionComp(4909) or
      PHP_FunctionComp(4938) or
      PHP_FunctionComp(4959) or
      PHP_FunctionComp(5147) or
      PHP_FunctionComp(5221) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc140: TtkTokenKind;
begin
  if  PHP_FunctionComp(202) or
      PHP_FunctionComp(450) or
      PHP_FunctionComp(555) or
      PHP_FunctionComp(619) or
      PHP_FunctionComp(623) or
      PHP_FunctionComp(676) or
      PHP_FunctionComp(743) or
      PHP_FunctionComp(1039) or
      PHP_FunctionComp(1168) or
      PHP_FunctionComp(1212) or
      PHP_FunctionComp(1443) or
      PHP_FunctionComp(1918) or
      PHP_FunctionComp(2464) or
      PHP_FunctionComp(2503) or
      PHP_FunctionComp(2538) or
      PHP_FunctionComp(2645) or
      PHP_FunctionComp(2683) or
      PHP_FunctionComp(2747) or
      PHP_FunctionComp(2757) or
      PHP_FunctionComp(3186) or
      PHP_FunctionComp(3441) or
      PHP_FunctionComp(3471) or
      PHP_FunctionComp(3595) or
      PHP_FunctionComp(3721) or
      PHP_FunctionComp(3740) or
      PHP_FunctionComp(3840) or
      PHP_FunctionComp(3857) or
      PHP_FunctionComp(4052) or
      PHP_FunctionComp(4290) or
      PHP_FunctionComp(4547) or
      PHP_FunctionComp(4608) or
      PHP_FunctionComp(4663) or
      PHP_FunctionComp(4934) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc141: TtkTokenKind;
begin
  if  PHP_FunctionComp(145) or
      PHP_FunctionComp(543) or
      PHP_FunctionComp(593) or
      PHP_FunctionComp(600) or
      PHP_FunctionComp(609) or
      PHP_FunctionComp(778) or
      PHP_FunctionComp(1194) or
      PHP_FunctionComp(1238) or
      PHP_FunctionComp(1333) or
      PHP_FunctionComp(1358) or
      PHP_FunctionComp(1563) or
      PHP_FunctionComp(1665) or
      PHP_FunctionComp(1669) or
      PHP_FunctionComp(1958) or
      PHP_FunctionComp(1978) or
      PHP_FunctionComp(2089) or
      PHP_FunctionComp(2399) or
      PHP_FunctionComp(2401) or
      PHP_FunctionComp(2616) or
      PHP_FunctionComp(2652) or
      PHP_FunctionComp(2850) or
      PHP_FunctionComp(2999) or
      PHP_FunctionComp(3457) or
      PHP_FunctionComp(3493) or
      PHP_FunctionComp(3594) or
      PHP_FunctionComp(3648) or
      PHP_FunctionComp(3653) or
      PHP_FunctionComp(4203) or
      PHP_FunctionComp(4477) or
      PHP_FunctionComp(4592) or
      PHP_FunctionComp(4609) or
      PHP_FunctionComp(4814) or
      PHP_FunctionComp(4851) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc142: TtkTokenKind;
begin
  if  PHP_FunctionComp(117) or
      PHP_FunctionComp(126) or
      PHP_FunctionComp(518) or
      PHP_FunctionComp(523) or
      PHP_FunctionComp(739) or
      PHP_FunctionComp(762) or
      PHP_FunctionComp(1062) or
      PHP_FunctionComp(1202) or
      PHP_FunctionComp(1616) or
      PHP_FunctionComp(1743) or
      PHP_FunctionComp(1875) or
      PHP_FunctionComp(1935) or
      PHP_FunctionComp(1941) or
      PHP_FunctionComp(2065) or
      PHP_FunctionComp(2081) or
      PHP_FunctionComp(2329) or
      PHP_FunctionComp(2402) or
      PHP_FunctionComp(2415) or
      PHP_FunctionComp(2472) or
      PHP_FunctionComp(2592) or
      PHP_FunctionComp(2641) or
      PHP_FunctionComp(2701) or
      PHP_FunctionComp(2939) or
      PHP_FunctionComp(3040) or
      PHP_FunctionComp(3140) or
      PHP_FunctionComp(3556) or
      PHP_FunctionComp(3702) or
      PHP_FunctionComp(3858) or
      PHP_FunctionComp(4225) or
      PHP_FunctionComp(4293) or
      PHP_FunctionComp(4453) or
      PHP_FunctionComp(4610) or
      PHP_FunctionComp(4878) or
      PHP_FunctionComp(4929) or
      PHP_FunctionComp(4964) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc143: TtkTokenKind;
begin
  if  PHP_FunctionComp(130) or
      PHP_FunctionComp(266) or
      PHP_FunctionComp(467) or
      PHP_FunctionComp(501) or
      PHP_FunctionComp(520) or
      PHP_FunctionComp(1081) or
      PHP_FunctionComp(1127) or
      PHP_FunctionComp(1487) or
      PHP_FunctionComp(1562) or
      PHP_FunctionComp(1719) or
      PHP_FunctionComp(1736) or
      PHP_FunctionComp(1896) or
      PHP_FunctionComp(1932) or
      PHP_FunctionComp(2118) or
      PHP_FunctionComp(2185) or
      PHP_FunctionComp(2203) or
      PHP_FunctionComp(2281) or
      PHP_FunctionComp(2332) or
      PHP_FunctionComp(2396) or
      PHP_FunctionComp(2470) or
      PHP_FunctionComp(2544) or
      PHP_FunctionComp(2549) or
      PHP_FunctionComp(3198) or
      PHP_FunctionComp(3492) or
      PHP_FunctionComp(3496) or
      PHP_FunctionComp(3534) or
      PHP_FunctionComp(3604) or
      PHP_FunctionComp(3739) or
      PHP_FunctionComp(3918) or
      PHP_FunctionComp(4050) or
      PHP_FunctionComp(4403) or
      PHP_FunctionComp(4844) or
      PHP_FunctionComp(5023) or
      PHP_FunctionComp(5135) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc144: TtkTokenKind;
begin
  if  PHP_FunctionComp(118) or
      PHP_FunctionComp(611) or
      PHP_FunctionComp(740) or
      PHP_FunctionComp(756) or
      PHP_FunctionComp(1050) or
      PHP_FunctionComp(1142) or
      PHP_FunctionComp(1390) or
      PHP_FunctionComp(1663) or
      PHP_FunctionComp(1684) or
      PHP_FunctionComp(2759) or
      PHP_FunctionComp(3112) or
      PHP_FunctionComp(3299) or
      PHP_FunctionComp(3377) or
      PHP_FunctionComp(3485) or
      PHP_FunctionComp(3637) or
      PHP_FunctionComp(3642) or
      PHP_FunctionComp(3679) or
      PHP_FunctionComp(3730) or
      PHP_FunctionComp(3805) or
      PHP_FunctionComp(3945) or
      PHP_FunctionComp(3994) or
      PHP_FunctionComp(4133) or
      PHP_FunctionComp(4416) or
      PHP_FunctionComp(4761) or
      PHP_FunctionComp(4841) or
      PHP_FunctionComp(4999) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc145: TtkTokenKind;
begin
  if  PHP_FunctionComp(222) or
      PHP_FunctionComp(228) or
      PHP_FunctionComp(350) or
      PHP_FunctionComp(522) or
      PHP_FunctionComp(629) or
      PHP_FunctionComp(692) or
      PHP_FunctionComp(715) or
      PHP_FunctionComp(1023) or
      PHP_FunctionComp(1136) or
      PHP_FunctionComp(1419) or
      PHP_FunctionComp(1771) or
      PHP_FunctionComp(1996) or
      PHP_FunctionComp(2029) or
      PHP_FunctionComp(2078) or
      PHP_FunctionComp(2169) or
      PHP_FunctionComp(2236) or
      PHP_FunctionComp(2377) or
      PHP_FunctionComp(2495) or
      PHP_FunctionComp(2638) or
      PHP_FunctionComp(2661) or
      PHP_FunctionComp(2762) or
      PHP_FunctionComp(2937) or
      PHP_FunctionComp(3224) or
      PHP_FunctionComp(3365) or
      PHP_FunctionComp(3720) or
      PHP_FunctionComp(3779) or
      PHP_FunctionComp(3884) or
      PHP_FunctionComp(3928) or
      PHP_FunctionComp(4224) or
      PHP_FunctionComp(4286) or
      PHP_FunctionComp(4309) or
      PHP_FunctionComp(4314) or
      PHP_FunctionComp(4319) or
      PHP_FunctionComp(4398) or
      PHP_FunctionComp(4747) or
      PHP_FunctionComp(4794) or
      PHP_FunctionComp(5173) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc146: TtkTokenKind;
begin
  if  PHP_FunctionComp(111) or
      PHP_FunctionComp(131) or
      PHP_FunctionComp(345) or
      PHP_FunctionComp(519) or
      PHP_FunctionComp(742) or
      PHP_FunctionComp(1037) or
      PHP_FunctionComp(1119) or
      PHP_FunctionComp(1161) or
      PHP_FunctionComp(1201) or
      PHP_FunctionComp(1409) or
      PHP_FunctionComp(1533) or
      PHP_FunctionComp(1630) or
      PHP_FunctionComp(2079) or
      PHP_FunctionComp(2082) or
      PHP_FunctionComp(2099) or
      PHP_FunctionComp(2432) or
      PHP_FunctionComp(2519) or
      PHP_FunctionComp(2627) or
      PHP_FunctionComp(2660) or
      PHP_FunctionComp(2708) or
      PHP_FunctionComp(2720) or
      PHP_FunctionComp(2797) or
      PHP_FunctionComp(3126) or
      PHP_FunctionComp(3142) or
      PHP_FunctionComp(3510) or
      PHP_FunctionComp(3634) or
      PHP_FunctionComp(3663) or
      PHP_FunctionComp(4094) or
      PHP_FunctionComp(4172) or
      PHP_FunctionComp(4218) or
      PHP_FunctionComp(4292) or
      PHP_FunctionComp(4638) or
      PHP_FunctionComp(5143) or
      PHP_FunctionComp(5178) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc147: TtkTokenKind;
begin
  if  PHP_FunctionComp(125) or
      PHP_FunctionComp(320) or
      PHP_FunctionComp(382) or
      PHP_FunctionComp(546) or
      PHP_FunctionComp(747) or
      PHP_FunctionComp(1083) or
      PHP_FunctionComp(1163) or
      PHP_FunctionComp(1425) or
      PHP_FunctionComp(1627) or
      PHP_FunctionComp(1644) or
      PHP_FunctionComp(1703) or
      PHP_FunctionComp(1846) or
      PHP_FunctionComp(1967) or
      PHP_FunctionComp(1995) or
      PHP_FunctionComp(2421) or
      PHP_FunctionComp(2521) or
      PHP_FunctionComp(2604) or
      PHP_FunctionComp(2714) or
      PHP_FunctionComp(2840) or
      PHP_FunctionComp(2934) or
      PHP_FunctionComp(2949) or
      PHP_FunctionComp(3185) or
      PHP_FunctionComp(3292) or
      PHP_FunctionComp(3308) or
      PHP_FunctionComp(3431) or
      PHP_FunctionComp(3508) or
      PHP_FunctionComp(3611) or
      PHP_FunctionComp(3735) or
      PHP_FunctionComp(3743) or
      PHP_FunctionComp(3924) or
      PHP_FunctionComp(4137) or
      PHP_FunctionComp(4326) or
      PHP_FunctionComp(4670) or
      PHP_FunctionComp(4802) or
      PHP_FunctionComp(4914) or
      PHP_FunctionComp(5145) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc148: TtkTokenKind;
begin
  if  PHP_FunctionComp(155) or
      PHP_FunctionComp(347) or
      PHP_FunctionComp(716) or
      PHP_FunctionComp(764) or
      PHP_FunctionComp(1034) or
      PHP_FunctionComp(1173) or
      PHP_FunctionComp(1231) or
      PHP_FunctionComp(1235) or
      PHP_FunctionComp(1348) or
      PHP_FunctionComp(1430) or
      PHP_FunctionComp(1444) or
      PHP_FunctionComp(1526) or
      PHP_FunctionComp(1722) or
      PHP_FunctionComp(1739) or
      PHP_FunctionComp(2092) or
      PHP_FunctionComp(2656) or
      PHP_FunctionComp(2946) or
      PHP_FunctionComp(3058) or
      PHP_FunctionComp(3297) or
      PHP_FunctionComp(3395) or
      PHP_FunctionComp(3518) or
      PHP_FunctionComp(3610) or
      PHP_FunctionComp(3652) or
      PHP_FunctionComp(3678) or
      PHP_FunctionComp(3708) or
      PHP_FunctionComp(4032) or
      PHP_FunctionComp(4132) or
      PHP_FunctionComp(4355) or
      PHP_FunctionComp(4402) or
      PHP_FunctionComp(4484) or
      PHP_FunctionComp(4542) or
      PHP_FunctionComp(4859) or
      PHP_FunctionComp(4891) or
      PHP_FunctionComp(4919) or
      PHP_FunctionComp(5061) or
      PHP_FunctionComp(5170) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc149: TtkTokenKind;
begin
  if  PHP_FunctionComp(201) or
      PHP_FunctionComp(400) or
      PHP_FunctionComp(539) or
      PHP_FunctionComp(745) or
      PHP_FunctionComp(1154) or
      PHP_FunctionComp(1222) or
      PHP_FunctionComp(1310) or
      PHP_FunctionComp(1740) or
      PHP_FunctionComp(1834) or
      PHP_FunctionComp(2198) or
      PHP_FunctionComp(2395) or
      PHP_FunctionComp(2729) or
      PHP_FunctionComp(2804) or
      PHP_FunctionComp(3138) or
      PHP_FunctionComp(3353) or
      PHP_FunctionComp(3647) or
      PHP_FunctionComp(3691) or
      PHP_FunctionComp(3736) or
      PHP_FunctionComp(3748) or
      PHP_FunctionComp(3801) or
      PHP_FunctionComp(3874) or
      PHP_FunctionComp(3875) or
      PHP_FunctionComp(3939) or
      PHP_FunctionComp(4019) or
      PHP_FunctionComp(4064) or
      PHP_FunctionComp(4386) or
      PHP_FunctionComp(4464) or
      PHP_FunctionComp(4756) or
      PHP_FunctionComp(4799) or
      PHP_FunctionComp(4853) or
      PHP_FunctionComp(4879) or
      PHP_FunctionComp(5041) or
      PHP_FunctionComp(5174) or
      PHP_FunctionComp(5200) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc150: TtkTokenKind;
begin
  if  PHP_FunctionComp(231) or
      PHP_FunctionComp(264) or
      PHP_FunctionComp(380) or
      PHP_FunctionComp(407) or
      PHP_FunctionComp(558) or
      PHP_FunctionComp(672) or
      PHP_FunctionComp(751) or
      PHP_FunctionComp(1040) or
      PHP_FunctionComp(1043) or
      PHP_FunctionComp(1233) or
      PHP_FunctionComp(1240) or
      PHP_FunctionComp(1272) or
      PHP_FunctionComp(1554) or
      PHP_FunctionComp(1560) or
      PHP_FunctionComp(1763) or
      PHP_FunctionComp(1797) or
      PHP_FunctionComp(1946) or
      PHP_FunctionComp(1947) or
      PHP_FunctionComp(2006) or
      PHP_FunctionComp(2626) or
      PHP_FunctionComp(2643) or
      PHP_FunctionComp(2659) or
      PHP_FunctionComp(2746) or
      PHP_FunctionComp(3167) or
      PHP_FunctionComp(3202) or
      PHP_FunctionComp(3337) or
      PHP_FunctionComp(3476) or
      PHP_FunctionComp(3512) or
      PHP_FunctionComp(3543) or
      PHP_FunctionComp(3644) or
      PHP_FunctionComp(3784) or
      PHP_FunctionComp(3826) or
      PHP_FunctionComp(3873) or
      PHP_FunctionComp(4176) or
      PHP_FunctionComp(4186) or
      PHP_FunctionComp(4349) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc151: TtkTokenKind;
begin
  if  PHP_FunctionComp(141) or
      PHP_FunctionComp(1192) or
      PHP_FunctionComp(1205) or
      PHP_FunctionComp(1836) or
      PHP_FunctionComp(1930) or
      PHP_FunctionComp(2097) or
      PHP_FunctionComp(2510) or
      PHP_FunctionComp(2644) or
      PHP_FunctionComp(2713) or
      PHP_FunctionComp(2951) or
      PHP_FunctionComp(3010) or
      PHP_FunctionComp(3680) or
      PHP_FunctionComp(3728) or
      PHP_FunctionComp(3757) or
      PHP_FunctionComp(3967) or
      PHP_FunctionComp(4391) or
      PHP_FunctionComp(5222) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc152: TtkTokenKind;
begin
  if  PHP_FunctionComp(127) or
      PHP_FunctionComp(207) or
      PHP_FunctionComp(403) or
      PHP_FunctionComp(413) or
      PHP_FunctionComp(693) or
      PHP_FunctionComp(761) or
      PHP_FunctionComp(984) or
      PHP_FunctionComp(1046) or
      PHP_FunctionComp(1051) or
      PHP_FunctionComp(1118) or
      PHP_FunctionComp(1215) or
      PHP_FunctionComp(1442) or
      PHP_FunctionComp(1794) or
      PHP_FunctionComp(1814) or
      PHP_FunctionComp(1863) or
      PHP_FunctionComp(1971) or
      PHP_FunctionComp(2047) or
      PHP_FunctionComp(2204) or
      PHP_FunctionComp(2275) or
      PHP_FunctionComp(2298) or
      PHP_FunctionComp(2310) or
      PHP_FunctionComp(2423) or
      PHP_FunctionComp(2615) or
      PHP_FunctionComp(2687) or
      PHP_FunctionComp(2760) or
      PHP_FunctionComp(2954) or
      PHP_FunctionComp(3209) or
      PHP_FunctionComp(3258) or
      PHP_FunctionComp(3361) or
      PHP_FunctionComp(3641) or
      PHP_FunctionComp(3741) or
      PHP_FunctionComp(3780) or
      PHP_FunctionComp(3825) or
      PHP_FunctionComp(3843) or
      PHP_FunctionComp(4340) or
      PHP_FunctionComp(4512) or
      PHP_FunctionComp(4569) or
      PHP_FunctionComp(4637) or
      PHP_FunctionComp(4644) or
      PHP_FunctionComp(5132) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc153: TtkTokenKind;
begin
  if  PHP_FunctionComp(121) or
      PHP_FunctionComp(122) or
      PHP_FunctionComp(209) or
      PHP_FunctionComp(417) or
      PHP_FunctionComp(471) or
      PHP_FunctionComp(480) or
      PHP_FunctionComp(578) or
      PHP_FunctionComp(615) or
      PHP_FunctionComp(1182) or
      PHP_FunctionComp(1226) or
      PHP_FunctionComp(1237) or
      PHP_FunctionComp(1344) or
      PHP_FunctionComp(1628) or
      PHP_FunctionComp(1822) or
      PHP_FunctionComp(2403) or
      PHP_FunctionComp(2482) or
      PHP_FunctionComp(2539) or
      PHP_FunctionComp(2694) or
      PHP_FunctionComp(2773) or
      PHP_FunctionComp(3287) or
      PHP_FunctionComp(3310) or
      PHP_FunctionComp(3498) or
      PHP_FunctionComp(3599) or
      PHP_FunctionComp(3640) or
      PHP_FunctionComp(4057) or
      PHP_FunctionComp(4142) or
      PHP_FunctionComp(4213) or
      PHP_FunctionComp(4254) or
      PHP_FunctionComp(4448) or
      PHP_FunctionComp(4619) or
      PHP_FunctionComp(5181) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc154: TtkTokenKind;
begin
  if  PHP_FunctionComp(85) or
      PHP_FunctionComp(119) or
      PHP_FunctionComp(318) or
      PHP_FunctionComp(331) or
      PHP_FunctionComp(511) or
      PHP_FunctionComp(723) or
      PHP_FunctionComp(795) or
      PHP_FunctionComp(811) or
      PHP_FunctionComp(1047) or
      PHP_FunctionComp(1183) or
      PHP_FunctionComp(1193) or
      PHP_FunctionComp(1296) or
      PHP_FunctionComp(1445) or
      PHP_FunctionComp(1600) or
      PHP_FunctionComp(1758) or
      PHP_FunctionComp(1843) or
      PHP_FunctionComp(1968) or
      PHP_FunctionComp(2235) or
      PHP_FunctionComp(2427) or
      PHP_FunctionComp(2437) or
      PHP_FunctionComp(2481) or
      PHP_FunctionComp(2669) or
      PHP_FunctionComp(2719) or
      PHP_FunctionComp(2968) or
      PHP_FunctionComp(2970) or
      PHP_FunctionComp(2989) or
      PHP_FunctionComp(3183) or
      PHP_FunctionComp(3263) or
      PHP_FunctionComp(3438) or
      PHP_FunctionComp(3524) or
      PHP_FunctionComp(3722) or
      PHP_FunctionComp(3762) or
      PHP_FunctionComp(3882) or
      PHP_FunctionComp(3909) or
      PHP_FunctionComp(4120) or
      PHP_FunctionComp(4348) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc155: TtkTokenKind;
begin
  if  PHP_FunctionComp(96) or
      PHP_FunctionComp(149) or
      PHP_FunctionComp(158) or
      PHP_FunctionComp(252) or
      PHP_FunctionComp(755) or
      PHP_FunctionComp(818) or
      PHP_FunctionComp(967) or
      PHP_FunctionComp(1179) or
      PHP_FunctionComp(1420) or
      PHP_FunctionComp(1829) or
      PHP_FunctionComp(1887) or
      PHP_FunctionComp(2018) or
      PHP_FunctionComp(2084) or
      PHP_FunctionComp(2234) or
      PHP_FunctionComp(2311) or
      PHP_FunctionComp(2648) or
      PHP_FunctionComp(2774) or
      PHP_FunctionComp(2798) or
      PHP_FunctionComp(2869) or
      PHP_FunctionComp(3075) or
      PHP_FunctionComp(3078) or
      PHP_FunctionComp(3459) or
      PHP_FunctionComp(3516) or
      PHP_FunctionComp(3584) or
      PHP_FunctionComp(3601) or
      PHP_FunctionComp(3602) or
      PHP_FunctionComp(3718) or
      PHP_FunctionComp(4405) or
      PHP_FunctionComp(4499) or
      PHP_FunctionComp(4600) or
      PHP_FunctionComp(4625) or
      PHP_FunctionComp(4729) or
      PHP_FunctionComp(4848) or
      PHP_FunctionComp(4973) or
      PHP_FunctionComp(5209) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc156: TtkTokenKind;
begin
  if  PHP_FunctionComp(278) or
      PHP_FunctionComp(365) or
      PHP_FunctionComp(448) or
      PHP_FunctionComp(540) or
      PHP_FunctionComp(588) or
      PHP_FunctionComp(1085) or
      PHP_FunctionComp(1153) or
      PHP_FunctionComp(1485) or
      PHP_FunctionComp(1523) or
      PHP_FunctionComp(1543) or
      PHP_FunctionComp(1552) or
      PHP_FunctionComp(1682) or
      PHP_FunctionComp(1807) or
      PHP_FunctionComp(2044) or
      PHP_FunctionComp(2135) or
      PHP_FunctionComp(2480) or
      PHP_FunctionComp(2566) or
      PHP_FunctionComp(2715) or
      PHP_FunctionComp(2724) or
      PHP_FunctionComp(2730) or
      PHP_FunctionComp(2763) or
      PHP_FunctionComp(2796) or
      PHP_FunctionComp(3253) or
      PHP_FunctionComp(3761) or
      PHP_FunctionComp(3952) or
      PHP_FunctionComp(4045) or
      PHP_FunctionComp(4289) or
      PHP_FunctionComp(4469) or
      PHP_FunctionComp(4682) or
      PHP_FunctionComp(4901) or
      PHP_FunctionComp(5000) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc157: TtkTokenKind;
begin
  if  PHP_FunctionComp(525) or
      PHP_FunctionComp(547) or
      PHP_FunctionComp(898) or
      PHP_FunctionComp(1162) or
      PHP_FunctionComp(1336) or
      PHP_FunctionComp(1340) or
      PHP_FunctionComp(1545) or
      PHP_FunctionComp(1989) or
      PHP_FunctionComp(2058) or
      PHP_FunctionComp(2213) or
      PHP_FunctionComp(2302) or
      PHP_FunctionComp(2439) or
      PHP_FunctionComp(2675) or
      PHP_FunctionComp(3088) or
      PHP_FunctionComp(3301) or
      PHP_FunctionComp(3381) or
      PHP_FunctionComp(3458) or
      PHP_FunctionComp(3513) or
      PHP_FunctionComp(3612) or
      PHP_FunctionComp(3797) or
      PHP_FunctionComp(3907) or
      PHP_FunctionComp(3926) or
      PHP_FunctionComp(4070) or
      PHP_FunctionComp(4117) or
      PHP_FunctionComp(4308) or
      PHP_FunctionComp(4313) or
      PHP_FunctionComp(4318) or
      PHP_FunctionComp(4646) or
      PHP_FunctionComp(4656) or
      PHP_FunctionComp(4812) or
      PHP_FunctionComp(4855) or
      PHP_FunctionComp(4960) or
      PHP_FunctionComp(5151) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc158: TtkTokenKind;
begin
  if  PHP_FunctionComp(133) or
      PHP_FunctionComp(356) or
      PHP_FunctionComp(452) or
      PHP_FunctionComp(923) or
      PHP_FunctionComp(1004) or
      PHP_FunctionComp(1189) or
      PHP_FunctionComp(1330) or
      PHP_FunctionComp(1699) or
      PHP_FunctionComp(1789) or
      PHP_FunctionComp(1825) or
      PHP_FunctionComp(2098) or
      PHP_FunctionComp(2314) or
      PHP_FunctionComp(2333) or
      PHP_FunctionComp(2450) or
      PHP_FunctionComp(2500) or
      PHP_FunctionComp(2699) or
      PHP_FunctionComp(2725) or
      PHP_FunctionComp(2753) or
      PHP_FunctionComp(3110) or
      PHP_FunctionComp(3143) or
      PHP_FunctionComp(3194) or
      PHP_FunctionComp(3286) or
      PHP_FunctionComp(3359) or
      PHP_FunctionComp(3439) or
      PHP_FunctionComp(3451) or
      PHP_FunctionComp(3582) or
      PHP_FunctionComp(3675) or
      PHP_FunctionComp(3768) or
      PHP_FunctionComp(4687) or
      PHP_FunctionComp(4849) or
      PHP_FunctionComp(4867) or
      PHP_FunctionComp(5176) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc159: TtkTokenKind;
begin
  if  PHP_FunctionComp(298) or
      PHP_FunctionComp(847) or
      PHP_FunctionComp(941) or
      PHP_FunctionComp(1044) or
      PHP_FunctionComp(1198) or
      PHP_FunctionComp(1380) or
      PHP_FunctionComp(1561) or
      PHP_FunctionComp(1689) or
      PHP_FunctionComp(1815) or
      PHP_FunctionComp(1988) or
      PHP_FunctionComp(2001) or
      PHP_FunctionComp(2199) or
      PHP_FunctionComp(2487) or
      PHP_FunctionComp(2642) or
      PHP_FunctionComp(2671) or
      PHP_FunctionComp(2700) or
      PHP_FunctionComp(2988) or
      PHP_FunctionComp(3134) or
      PHP_FunctionComp(3177) or
      PHP_FunctionComp(3239) or
      PHP_FunctionComp(3521) or
      PHP_FunctionComp(3625) or
      PHP_FunctionComp(3745) or
      PHP_FunctionComp(3764) or
      PHP_FunctionComp(4153) or
      PHP_FunctionComp(4166) or
      PHP_FunctionComp(4427) or
      PHP_FunctionComp(4590) or
      PHP_FunctionComp(4640) or
      PHP_FunctionComp(4740) or
      PHP_FunctionComp(4896) or
      PHP_FunctionComp(5169) or
      PHP_FunctionComp(5199) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc160: TtkTokenKind;
begin
  if  PHP_FunctionComp(248) or
      PHP_FunctionComp(265) or
      PHP_FunctionComp(556) or
      PHP_FunctionComp(634) or
      PHP_FunctionComp(1078) or
      PHP_FunctionComp(1248) or
      PHP_FunctionComp(1426) or
      PHP_FunctionComp(1625) or
      PHP_FunctionComp(1664) or
      PHP_FunctionComp(1866) or
      PHP_FunctionComp(1997) or
      PHP_FunctionComp(2040) or
      PHP_FunctionComp(2060) or
      PHP_FunctionComp(2126) or
      PHP_FunctionComp(2334) or
      PHP_FunctionComp(2344) or
      PHP_FunctionComp(2390) or
      PHP_FunctionComp(2435) or
      PHP_FunctionComp(2758) or
      PHP_FunctionComp(2764) or
      PHP_FunctionComp(2932) or
      PHP_FunctionComp(3082) or
      PHP_FunctionComp(3109) or
      PHP_FunctionComp(3579) or
      PHP_FunctionComp(3587) or
      PHP_FunctionComp(3632) or
      PHP_FunctionComp(3687) or
      PHP_FunctionComp(3724) or
      PHP_FunctionComp(3731) or
      PHP_FunctionComp(3782) or
      PHP_FunctionComp(3922) or
      PHP_FunctionComp(3946) or
      PHP_FunctionComp(4061) or
      PHP_FunctionComp(4122) or
      PHP_FunctionComp(4345) or
      PHP_FunctionComp(4350) or
      PHP_FunctionComp(4378) or
      PHP_FunctionComp(4642) or
      PHP_FunctionComp(4829) or
      PHP_FunctionComp(4836) or
      PHP_FunctionComp(4997) or
      PHP_FunctionComp(5052) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc161: TtkTokenKind;
begin
  if  PHP_FunctionComp(79) or
      PHP_FunctionComp(105) or
      PHP_FunctionComp(282) or
      PHP_FunctionComp(343) or
      PHP_FunctionComp(434) or
      PHP_FunctionComp(494) or
      PHP_FunctionComp(566) or
      PHP_FunctionComp(621) or
      PHP_FunctionComp(720) or
      PHP_FunctionComp(1049) or
      PHP_FunctionComp(1178) or
      PHP_FunctionComp(1216) or
      PHP_FunctionComp(1781) or
      PHP_FunctionComp(1885) or
      PHP_FunctionComp(1949) or
      PHP_FunctionComp(1962) or
      PHP_FunctionComp(2276) or
      PHP_FunctionComp(2349) or
      PHP_FunctionComp(2622) or
      PHP_FunctionComp(2631) or
      PHP_FunctionComp(2808) or
      PHP_FunctionComp(2912) or
      PHP_FunctionComp(2961) or
      PHP_FunctionComp(3039) or
      PHP_FunctionComp(3046) or
      PHP_FunctionComp(3318) or
      PHP_FunctionComp(3746) or
      PHP_FunctionComp(4039) or
      PHP_FunctionComp(4273) or
      PHP_FunctionComp(4337) or
      PHP_FunctionComp(4692) or
      PHP_FunctionComp(4811) or
      PHP_FunctionComp(4893) or
      PHP_FunctionComp(5024) or
      PHP_FunctionComp(5032) or
      PHP_FunctionComp(5234) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc162: TtkTokenKind;
begin
  if  PHP_FunctionComp(84) or
      PHP_FunctionComp(325) or
      PHP_FunctionComp(583) or
      PHP_FunctionComp(897) or
      PHP_FunctionComp(1149) or
      PHP_FunctionComp(1174) or
      PHP_FunctionComp(1252) or
      PHP_FunctionComp(1411) or
      PHP_FunctionComp(1839) or
      PHP_FunctionComp(1854) or
      PHP_FunctionComp(1876) or
      PHP_FunctionComp(2096) or
      PHP_FunctionComp(2178) or
      PHP_FunctionComp(2231) or
      PHP_FunctionComp(2254) or
      PHP_FunctionComp(2526) or
      PHP_FunctionComp(2613) or
      PHP_FunctionComp(2649) or
      PHP_FunctionComp(2775) or
      PHP_FunctionComp(2776) or
      PHP_FunctionComp(2982) or
      PHP_FunctionComp(2984) or
      PHP_FunctionComp(3114) or
      PHP_FunctionComp(3120) or
      PHP_FunctionComp(3829) or
      PHP_FunctionComp(4291) or
      PHP_FunctionComp(4357) or
      PHP_FunctionComp(4435) or
      PHP_FunctionComp(4496) or
      PHP_FunctionComp(4647) or
      PHP_FunctionComp(4689) or
      PHP_FunctionComp(4722) or
      PHP_FunctionComp(4865) or
      PHP_FunctionComp(5129) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc163: TtkTokenKind;
begin
  if  PHP_FunctionComp(86) or
      PHP_FunctionComp(215) or
      PHP_FunctionComp(336) or
      PHP_FunctionComp(341) or
      PHP_FunctionComp(491) or
      PHP_FunctionComp(497) or
      PHP_FunctionComp(824) or
      PHP_FunctionComp(1069) or
      PHP_FunctionComp(1376) or
      PHP_FunctionComp(1548) or
      PHP_FunctionComp(1604) or
      PHP_FunctionComp(1671) or
      PHP_FunctionComp(2177) or
      PHP_FunctionComp(2706) or
      PHP_FunctionComp(2721) or
      PHP_FunctionComp(3199) or
      PHP_FunctionComp(3244) or
      PHP_FunctionComp(3303) or
      PHP_FunctionComp(3744) or
      PHP_FunctionComp(3950) or
      PHP_FunctionComp(4082) or
      PHP_FunctionComp(4438) or
      PHP_FunctionComp(4643) or
      PHP_FunctionComp(4677) or
      PHP_FunctionComp(4846) or
      PHP_FunctionComp(4852) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc164: TtkTokenKind;
begin
  if  PHP_FunctionComp(459) or
      PHP_FunctionComp(544) or
      PHP_FunctionComp(679) or
      PHP_FunctionComp(1540) or
      PHP_FunctionComp(1597) or
      PHP_FunctionComp(1812) or
      PHP_FunctionComp(1853) or
      PHP_FunctionComp(2145) or
      PHP_FunctionComp(2711) or
      PHP_FunctionComp(2768) or
      PHP_FunctionComp(2893) or
      PHP_FunctionComp(3225) or
      PHP_FunctionComp(3473) or
      PHP_FunctionComp(3551) or
      PHP_FunctionComp(3812) or
      PHP_FunctionComp(3832) or
      PHP_FunctionComp(3865) or
      PHP_FunctionComp(4156) or
      PHP_FunctionComp(4361) or
      PHP_FunctionComp(4421) or
      PHP_FunctionComp(4630) or
      PHP_FunctionComp(4736) or
      PHP_FunctionComp(4872) or
      PHP_FunctionComp(4917) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc165: TtkTokenKind;
begin
  if  PHP_FunctionComp(659) or
      PHP_FunctionComp(1338) or
      PHP_FunctionComp(1808) or
      PHP_FunctionComp(2036) or
      PHP_FunctionComp(2121) or
      PHP_FunctionComp(2291) or
      PHP_FunctionComp(2491) or
      PHP_FunctionComp(2602) or
      PHP_FunctionComp(2639) or
      PHP_FunctionComp(2655) or
      PHP_FunctionComp(2817) or
      PHP_FunctionComp(2860) or
      PHP_FunctionComp(2903) or
      PHP_FunctionComp(3049) or
      PHP_FunctionComp(3317) or
      PHP_FunctionComp(3391) or
      PHP_FunctionComp(4566) or
      PHP_FunctionComp(4582) or
      PHP_FunctionComp(4748) or
      PHP_FunctionComp(4890) or
      PHP_FunctionComp(4913) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc166: TtkTokenKind;
begin
  if  PHP_FunctionComp(230) or
      PHP_FunctionComp(259) or
      PHP_FunctionComp(321) or
      PHP_FunctionComp(414) or
      PHP_FunctionComp(533) or
      PHP_FunctionComp(661) or
      PHP_FunctionComp(684) or
      PHP_FunctionComp(786) or
      PHP_FunctionComp(913) or
      PHP_FunctionComp(956) or
      PHP_FunctionComp(1101) or
      PHP_FunctionComp(1146) or
      PHP_FunctionComp(1209) or
      PHP_FunctionComp(1218) or
      PHP_FunctionComp(1396) or
      PHP_FunctionComp(1446) or
      PHP_FunctionComp(1546) or
      PHP_FunctionComp(1607) or
      PHP_FunctionComp(1654) or
      PHP_FunctionComp(1831) or
      PHP_FunctionComp(2022) or
      PHP_FunctionComp(2080) or
      PHP_FunctionComp(2251) or
      PHP_FunctionComp(2429) or
      PHP_FunctionComp(2697) or
      PHP_FunctionComp(2743) or
      PHP_FunctionComp(2929) or
      PHP_FunctionComp(2960) or
      PHP_FunctionComp(2976) or
      PHP_FunctionComp(3288) or
      PHP_FunctionComp(3606) or
      PHP_FunctionComp(3694) or
      PHP_FunctionComp(3742) or
      PHP_FunctionComp(3811) or
      PHP_FunctionComp(4044) or
      PHP_FunctionComp(4813) or
      PHP_FunctionComp(4875) or
      PHP_FunctionComp(5232) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc167: TtkTokenKind;
begin
  if  PHP_FunctionComp(112) or
      PHP_FunctionComp(322) or
      PHP_FunctionComp(330) or
      PHP_FunctionComp(1461) or
      PHP_FunctionComp(1601) or
      PHP_FunctionComp(1823) or
      PHP_FunctionComp(1888) or
      PHP_FunctionComp(1923) or
      PHP_FunctionComp(2020) or
      PHP_FunctionComp(2309) or
      PHP_FunctionComp(2365) or
      PHP_FunctionComp(2422) or
      PHP_FunctionComp(2772) or
      PHP_FunctionComp(2911) or
      PHP_FunctionComp(3566) or
      PHP_FunctionComp(3706) or
      PHP_FunctionComp(3799) or
      PHP_FunctionComp(3863) or
      PHP_FunctionComp(4034) or
      PHP_FunctionComp(4145) or
      PHP_FunctionComp(4215) or
      PHP_FunctionComp(4440) or
      PHP_FunctionComp(4447) or
      PHP_FunctionComp(4974) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc168: TtkTokenKind;
begin
  if  PHP_FunctionComp(82) or
      PHP_FunctionComp(159) or
      PHP_FunctionComp(281) or
      PHP_FunctionComp(385) or
      PHP_FunctionComp(565) or
      PHP_FunctionComp(610) or
      PHP_FunctionComp(691) or
      PHP_FunctionComp(695) or
      PHP_FunctionComp(929) or
      PHP_FunctionComp(1053) or
      PHP_FunctionComp(1060) or
      PHP_FunctionComp(1090) or
      PHP_FunctionComp(1666) or
      PHP_FunctionComp(1670) or
      PHP_FunctionComp(1701) or
      PHP_FunctionComp(1751) or
      PHP_FunctionComp(1869) or
      PHP_FunctionComp(1964) or
      PHP_FunctionComp(2452) or
      PHP_FunctionComp(2688) or
      PHP_FunctionComp(2805) or
      PHP_FunctionComp(2925) or
      PHP_FunctionComp(2947) or
      PHP_FunctionComp(2979) or
      PHP_FunctionComp(2987) or
      PHP_FunctionComp(3083) or
      PHP_FunctionComp(3725) or
      PHP_FunctionComp(3835) or
      PHP_FunctionComp(3940) or
      PHP_FunctionComp(4004) or
      PHP_FunctionComp(4157) or
      PHP_FunctionComp(4404) or
      PHP_FunctionComp(4564) or
      PHP_FunctionComp(4584) or
      PHP_FunctionComp(4800) or
      PHP_FunctionComp(4991) or
      PHP_FunctionComp(5126) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc169: TtkTokenKind;
begin
  if  PHP_FunctionComp(102) or
      PHP_FunctionComp(1129) or
      PHP_FunctionComp(1603) or
      PHP_FunctionComp(1840) or
      PHP_FunctionComp(2086) or
      PHP_FunctionComp(2375) or
      PHP_FunctionComp(2806) or
      PHP_FunctionComp(2818) or
      PHP_FunctionComp(2856) or
      PHP_FunctionComp(2936) or
      PHP_FunctionComp(3174) or
      PHP_FunctionComp(3235) or
      PHP_FunctionComp(3553) or
      PHP_FunctionComp(3912) or
      PHP_FunctionComp(4078) or
      PHP_FunctionComp(4329) or
      PHP_FunctionComp(4336) or
      PHP_FunctionComp(4439) or
      PHP_FunctionComp(4648) or
      PHP_FunctionComp(4664) or
      PHP_FunctionComp(4772) or
      PHP_FunctionComp(4801) or
      PHP_FunctionComp(4895) or
      PHP_FunctionComp(4897) or
      PHP_FunctionComp(4970) or
      PHP_FunctionComp(5146) or
      PHP_FunctionComp(5213) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc170: TtkTokenKind;
begin
  if  PHP_FunctionComp(137) or
      PHP_FunctionComp(223) or
      PHP_FunctionComp(758) or
      PHP_FunctionComp(759) or
      PHP_FunctionComp(825) or
      PHP_FunctionComp(1140) or
      PHP_FunctionComp(1408) or
      PHP_FunctionComp(1824) or
      PHP_FunctionComp(1837) or
      PHP_FunctionComp(2087) or
      PHP_FunctionComp(2445) or
      PHP_FunctionComp(2742) or
      PHP_FunctionComp(3048) or
      PHP_FunctionComp(3050) or
      PHP_FunctionComp(3079) or
      PHP_FunctionComp(3433) or
      PHP_FunctionComp(3504) or
      PHP_FunctionComp(3586) or
      PHP_FunctionComp(3919) or
      PHP_FunctionComp(3930) or
      PHP_FunctionComp(4271) or
      PHP_FunctionComp(4283) or
      PHP_FunctionComp(4334) or
      PHP_FunctionComp(4356) or
      PHP_FunctionComp(4385) or
      PHP_FunctionComp(4462) or
      PHP_FunctionComp(4481) or
      PHP_FunctionComp(4588) or
      PHP_FunctionComp(4665) or
      PHP_FunctionComp(4734) or
      PHP_FunctionComp(4798) or
      PHP_FunctionComp(5177) or
      PHP_FunctionComp(5185) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc171: TtkTokenKind;
begin
  if  PHP_FunctionComp(594) or
      PHP_FunctionComp(768) or
      PHP_FunctionComp(1195) or
      PHP_FunctionComp(1200) or
      PHP_FunctionComp(1397) or
      PHP_FunctionComp(1478) or
      PHP_FunctionComp(1549) or
      PHP_FunctionComp(1697) or
      PHP_FunctionComp(1738) or
      PHP_FunctionComp(1811) or
      PHP_FunctionComp(1874) or
      PHP_FunctionComp(1965) or
      PHP_FunctionComp(1986) or
      PHP_FunctionComp(2032) or
      PHP_FunctionComp(2051) or
      PHP_FunctionComp(2245) or
      PHP_FunctionComp(2299) or
      PHP_FunctionComp(2336) or
      PHP_FunctionComp(2400) or
      PHP_FunctionComp(2517) or
      PHP_FunctionComp(2663) or
      PHP_FunctionComp(2665) or
      PHP_FunctionComp(2785) or
      PHP_FunctionComp(2828) or
      PHP_FunctionComp(2908) or
      PHP_FunctionComp(3169) or
      PHP_FunctionComp(3173) or
      PHP_FunctionComp(3226) or
      PHP_FunctionComp(3366) or
      PHP_FunctionComp(3484) or
      PHP_FunctionComp(3893) or
      PHP_FunctionComp(3979) or
      PHP_FunctionComp(4182) or
      PHP_FunctionComp(4212) or
      PHP_FunctionComp(4338) or
      PHP_FunctionComp(4392) or
      PHP_FunctionComp(4470) or
      PHP_FunctionComp(4887) or
      PHP_FunctionComp(5038) or
      PHP_FunctionComp(5150) or
      PHP_FunctionComp(5196) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc172: TtkTokenKind;
begin
  if  PHP_FunctionComp(161) or
      PHP_FunctionComp(470) or
      PHP_FunctionComp(557) or
      PHP_FunctionComp(773) or
      PHP_FunctionComp(1121) or
      PHP_FunctionComp(1707) or
      PHP_FunctionComp(2068) or
      PHP_FunctionComp(2343) or
      PHP_FunctionComp(2380) or
      PHP_FunctionComp(2436) or
      PHP_FunctionComp(2547) or
      PHP_FunctionComp(2793) or
      PHP_FunctionComp(2997) or
      PHP_FunctionComp(3097) or
      PHP_FunctionComp(3511) or
      PHP_FunctionComp(3554) or
      PHP_FunctionComp(3727) or
      PHP_FunctionComp(3729) or
      PHP_FunctionComp(3941) or
      PHP_FunctionComp(3954) or
      PHP_FunctionComp(4143) or
      PHP_FunctionComp(4235) or
      PHP_FunctionComp(4285) or
      PHP_FunctionComp(4589) or
      PHP_FunctionComp(4680) or
      PHP_FunctionComp(5005) or
      PHP_FunctionComp(5018) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc173: TtkTokenKind;
begin
  if  PHP_KeywordComp(54) then
    Result := tkPhpKeyword
  else
    if  PHP_FunctionComp(344) or
        PHP_FunctionComp(766) or
        PHP_FunctionComp(777) or
        PHP_FunctionComp(924) or
        PHP_FunctionComp(958) or
        PHP_FunctionComp(1761) or
        PHP_FunctionComp(1981) or
        PHP_FunctionComp(2033) or
        PHP_FunctionComp(2292) or
        PHP_FunctionComp(2323) or
        PHP_FunctionComp(2406) or
        PHP_FunctionComp(2667) or
        PHP_FunctionComp(2679) or
        PHP_FunctionComp(2821) or
        PHP_FunctionComp(2943) or
        PHP_FunctionComp(2948) or
        PHP_FunctionComp(2962) or
        PHP_FunctionComp(3094) or
        PHP_FunctionComp(3101) or
        PHP_FunctionComp(3153) or
        PHP_FunctionComp(3216) or
        PHP_FunctionComp(3307) or
        PHP_FunctionComp(3608) or
        PHP_FunctionComp(3852) or
        PHP_FunctionComp(4183) or
        PHP_FunctionComp(4344) or
        PHP_FunctionComp(4543) or
        PHP_FunctionComp(4881) or
        PHP_FunctionComp(4965) or
        PHP_FunctionComp(5197) then
      Result := tkPhpFunction
    else
      Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc174: TtkTokenKind;
begin
  if  PHP_FunctionComp(107) or
      PHP_FunctionComp(221) or
      PHP_FunctionComp(769) or
      PHP_FunctionComp(779) or
      PHP_FunctionComp(907) or
      PHP_FunctionComp(1250) or
      PHP_FunctionComp(1261) or
      PHP_FunctionComp(1354) or
      PHP_FunctionComp(1483) or
      PHP_FunctionComp(1556) or
      PHP_FunctionComp(1617) or
      PHP_FunctionComp(2088) or
      PHP_FunctionComp(2093) or
      PHP_FunctionComp(2376) or
      PHP_FunctionComp(2540) or
      PHP_FunctionComp(2620) or
      PHP_FunctionComp(2681) or
      PHP_FunctionComp(2851) or
      PHP_FunctionComp(3141) or
      PHP_FunctionComp(3434) or
      PHP_FunctionComp(3583) or
      PHP_FunctionComp(4047) or
      PHP_FunctionComp(4809) or
      PHP_FunctionComp(5072) or
      PHP_FunctionComp(5131) or
      PHP_FunctionComp(5182) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc175: TtkTokenKind;
begin
  if  PHP_FunctionComp(157) or
      PHP_FunctionComp(216) or
      PHP_FunctionComp(765) or
      PHP_FunctionComp(1108) or
      PHP_FunctionComp(1196) or
      PHP_FunctionComp(1320) or
      PHP_FunctionComp(1356) or
      PHP_FunctionComp(1393) or
      PHP_FunctionComp(1589) or
      PHP_FunctionComp(1774) or
      PHP_FunctionComp(2094) or
      PHP_FunctionComp(2244) or
      PHP_FunctionComp(2287) or
      PHP_FunctionComp(2744) or
      PHP_FunctionComp(3117) or
      PHP_FunctionComp(3791) or
      PHP_FunctionComp(4598) or
      PHP_FunctionComp(4655) or
      PHP_FunctionComp(4684) or
      PHP_FunctionComp(5028) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc176: TtkTokenKind;
begin
  if  PHP_FunctionComp(235) or
      PHP_FunctionComp(564) or
      PHP_FunctionComp(570) or
      PHP_FunctionComp(586) or
      PHP_FunctionComp(596) or
      PHP_FunctionComp(632) or
      PHP_FunctionComp(928) or
      PHP_FunctionComp(1469) or
      PHP_FunctionComp(1569) or
      PHP_FunctionComp(1725) or
      PHP_FunctionComp(1889) or
      PHP_FunctionComp(1929) or
      PHP_FunctionComp(1951) or
      PHP_FunctionComp(2237) or
      PHP_FunctionComp(2709) or
      PHP_FunctionComp(2791) or
      PHP_FunctionComp(2901) or
      PHP_FunctionComp(2928) or
      PHP_FunctionComp(2975) or
      PHP_FunctionComp(3238) or
      PHP_FunctionComp(3323) or
      PHP_FunctionComp(3775) or
      PHP_FunctionComp(3855) or
      PHP_FunctionComp(3899) or
      PHP_FunctionComp(4219) or
      PHP_FunctionComp(4396) or
      PHP_FunctionComp(4461) or
      PHP_FunctionComp(4688) or
      PHP_FunctionComp(4797) or
      PHP_FunctionComp(4972) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc177: TtkTokenKind;
begin
  if  PHP_FunctionComp(192) or
      PHP_FunctionComp(326) or
      PHP_FunctionComp(425) or
      PHP_FunctionComp(465) or
      PHP_FunctionComp(495) or
      PHP_FunctionComp(915) or
      PHP_FunctionComp(947) or
      PHP_FunctionComp(966) or
      PHP_FunctionComp(1020) or
      PHP_FunctionComp(1349) or
      PHP_FunctionComp(1886) or
      PHP_FunctionComp(1950) or
      PHP_FunctionComp(2085) or
      PHP_FunctionComp(2112) or
      PHP_FunctionComp(2348) or
      PHP_FunctionComp(2726) or
      PHP_FunctionComp(2935) or
      PHP_FunctionComp(2977) or
      PHP_FunctionComp(3032) or
      PHP_FunctionComp(3052) or
      PHP_FunctionComp(3066) or
      PHP_FunctionComp(3197) or
      PHP_FunctionComp(3243) or
      PHP_FunctionComp(3499) or
      PHP_FunctionComp(3597) or
      PHP_FunctionComp(3672) or
      PHP_FunctionComp(3737) or
      PHP_FunctionComp(3771) or
      PHP_FunctionComp(3985) or
      PHP_FunctionComp(4486) or
      PHP_FunctionComp(4720) or
      PHP_FunctionComp(4892) or
      PHP_FunctionComp(4936) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc178: TtkTokenKind;
begin
  if  PHP_FunctionComp(408) or
      PHP_FunctionComp(763) or
      PHP_FunctionComp(900) or
      PHP_FunctionComp(1025) or
      PHP_FunctionComp(1058) or
      PHP_FunctionComp(1352) or
      PHP_FunctionComp(1729) or
      PHP_FunctionComp(1760) or
      PHP_FunctionComp(1835) or
      PHP_FunctionComp(1954) or
      PHP_FunctionComp(2107) or
      PHP_FunctionComp(2195) or
      PHP_FunctionComp(2200) or
      PHP_FunctionComp(2243) or
      PHP_FunctionComp(2552) or
      PHP_FunctionComp(2727) or
      PHP_FunctionComp(2761) or
      PHP_FunctionComp(3000) or
      PHP_FunctionComp(3090) or
      PHP_FunctionComp(3176) or
      PHP_FunctionComp(3237) or
      PHP_FunctionComp(3367) or
      PHP_FunctionComp(3399) or
      PHP_FunctionComp(3903) or
      PHP_FunctionComp(4111) or
      PHP_FunctionComp(4335) or
      PHP_FunctionComp(4606) or
      PHP_FunctionComp(4623) or
      PHP_FunctionComp(4731) or
      PHP_FunctionComp(4899) or
      PHP_FunctionComp(4976) or
      PHP_FunctionComp(4982) or
      PHP_FunctionComp(5003) or
      PHP_FunctionComp(5050) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc179: TtkTokenKind;
begin
  if  PHP_FunctionComp(114) or
      PHP_FunctionComp(211) or
      PHP_FunctionComp(285) or
      PHP_FunctionComp(637) or
      PHP_FunctionComp(1199) or
      PHP_FunctionComp(1303) or
      PHP_FunctionComp(1579) or
      PHP_FunctionComp(1884) or
      PHP_FunctionComp(2174) or
      PHP_FunctionComp(2194) or
      PHP_FunctionComp(2489) or
      PHP_FunctionComp(2568) or
      PHP_FunctionComp(2646) or
      PHP_FunctionComp(2723) or
      PHP_FunctionComp(2902) or
      PHP_FunctionComp(2958) or
      PHP_FunctionComp(3107) or
      PHP_FunctionComp(3113) or
      PHP_FunctionComp(3145) or
      PHP_FunctionComp(3437) or
      PHP_FunctionComp(3868) or
      PHP_FunctionComp(4051) or
      PHP_FunctionComp(4113) or
      PHP_FunctionComp(4272) or
      PHP_FunctionComp(4389) or
      PHP_FunctionComp(4423) or
      PHP_FunctionComp(4434) or
      PHP_FunctionComp(4651) or
      PHP_FunctionComp(5002) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc180: TtkTokenKind;
begin
  if  PHP_FunctionComp(381) or
      PHP_FunctionComp(502) or
      PHP_FunctionComp(794) or
      PHP_FunctionComp(1001) or
      PHP_FunctionComp(1071) or
      PHP_FunctionComp(1775) or
      PHP_FunctionComp(1778) or
      PHP_FunctionComp(1944) or
      PHP_FunctionComp(2142) or
      PHP_FunctionComp(2504) or
      PHP_FunctionComp(2710) or
      PHP_FunctionComp(2829) or
      PHP_FunctionComp(2942) or
      PHP_FunctionComp(2963) or
      PHP_FunctionComp(3016) or
      PHP_FunctionComp(3024) or
      PHP_FunctionComp(3098) or
      PHP_FunctionComp(3204) or
      PHP_FunctionComp(3249) or
      PHP_FunctionComp(3400) or
      PHP_FunctionComp(3563) or
      PHP_FunctionComp(3711) or
      PHP_FunctionComp(4033) or
      PHP_FunctionComp(4054) or
      PHP_FunctionComp(4115) or
      PHP_FunctionComp(4528) or
      PHP_FunctionComp(4618) or
      PHP_FunctionComp(4658) or
      PHP_FunctionComp(4860) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc181: TtkTokenKind;
begin
  if  PHP_FunctionComp(103) or
      PHP_FunctionComp(147) or
      PHP_FunctionComp(536) or
      PHP_FunctionComp(542) or
      PHP_FunctionComp(1009) or
      PHP_FunctionComp(1159) or
      PHP_FunctionComp(1274) or
      PHP_FunctionComp(1622) or
      PHP_FunctionComp(1691) or
      PHP_FunctionComp(1753) or
      PHP_FunctionComp(1883) or
      PHP_FunctionComp(2252) or
      PHP_FunctionComp(2342) or
      PHP_FunctionComp(2389) or
      PHP_FunctionComp(2398) or
      PHP_FunctionComp(2795) or
      PHP_FunctionComp(2852) or
      PHP_FunctionComp(3749) or
      PHP_FunctionComp(3925) or
      PHP_FunctionComp(4371) or
      PHP_FunctionComp(4381) or
      PHP_FunctionComp(4436) or
      PHP_FunctionComp(4667) or
      PHP_FunctionComp(4686) or
      PHP_FunctionComp(4750) or
      PHP_FunctionComp(4760) or
      PHP_FunctionComp(4835) or
      PHP_FunctionComp(4979) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc182: TtkTokenKind;
begin
  if  PHP_FunctionComp(73) or
      PHP_FunctionComp(120) or
      PHP_FunctionComp(212) or
      PHP_FunctionComp(342) or
      PHP_FunctionComp(531) or
      PHP_FunctionComp(1134) or
      PHP_FunctionComp(1357) or
      PHP_FunctionComp(1459) or
      PHP_FunctionComp(1592) or
      PHP_FunctionComp(1594) or
      PHP_FunctionComp(1641) or
      PHP_FunctionComp(1643) or
      PHP_FunctionComp(1855) or
      PHP_FunctionComp(2409) or
      PHP_FunctionComp(2446) or
      PHP_FunctionComp(2728) or
      PHP_FunctionComp(2731) or
      PHP_FunctionComp(2894) or
      PHP_FunctionComp(2978) or
      PHP_FunctionComp(3076) or
      PHP_FunctionComp(3290) or
      PHP_FunctionComp(3334) or
      PHP_FunctionComp(3591) or
      PHP_FunctionComp(3732) or
      PHP_FunctionComp(4331) or
      PHP_FunctionComp(4422) or
      PHP_FunctionComp(4465) or
      PHP_FunctionComp(4915) or
      PHP_FunctionComp(5047) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc183: TtkTokenKind;
begin
  if  PHP_FunctionComp(123) or
      PHP_FunctionComp(532) or
      PHP_FunctionComp(616) or
      PHP_FunctionComp(911) or
      PHP_FunctionComp(1024) or
      PHP_FunctionComp(1181) or
      PHP_FunctionComp(1273) or
      PHP_FunctionComp(1275) or
      PHP_FunctionComp(1339) or
      PHP_FunctionComp(1623) or
      PHP_FunctionComp(1683) or
      PHP_FunctionComp(2407) or
      PHP_FunctionComp(2438) or
      PHP_FunctionComp(2456) or
      PHP_FunctionComp(2465) or
      PHP_FunctionComp(2494) or
      PHP_FunctionComp(2505) or
      PHP_FunctionComp(2625) or
      PHP_FunctionComp(2677) or
      PHP_FunctionComp(2678) or
      PHP_FunctionComp(2740) or
      PHP_FunctionComp(2770) or
      PHP_FunctionComp(2924) or
      PHP_FunctionComp(3009) or
      PHP_FunctionComp(3360) or
      PHP_FunctionComp(3692) or
      PHP_FunctionComp(3864) or
      PHP_FunctionComp(4253) or
      PHP_FunctionComp(4612) or
      PHP_FunctionComp(4754) or
      PHP_FunctionComp(4912) or
      PHP_FunctionComp(4950) or
      PHP_FunctionComp(5137) or
      PHP_FunctionComp(5233) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc184: TtkTokenKind;
begin
  if  PHP_FunctionComp(890) or
      PHP_FunctionComp(912) or
      PHP_FunctionComp(1054) or
      PHP_FunctionComp(1131) or
      PHP_FunctionComp(1301) or
      PHP_FunctionComp(1412) or
      PHP_FunctionComp(1897) or
      PHP_FunctionComp(1985) or
      PHP_FunctionComp(2230) or
      PHP_FunctionComp(2322) or
      PHP_FunctionComp(2462) or
      PHP_FunctionComp(2522) or
      PHP_FunctionComp(2712) or
      PHP_FunctionComp(2771) or
      PHP_FunctionComp(2784) or
      PHP_FunctionComp(3031) or
      PHP_FunctionComp(3045) or
      PHP_FunctionComp(3057) or
      PHP_FunctionComp(3091) or
      PHP_FunctionComp(3295) or
      PHP_FunctionComp(3392) or
      PHP_FunctionComp(3517) or
      PHP_FunctionComp(3794) or
      PHP_FunctionComp(5034) or
      PHP_FunctionComp(5035) or
      PHP_FunctionComp(5071) or
      PHP_FunctionComp(5204) or
      PHP_FunctionComp(5223) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc185: TtkTokenKind;
begin
  if  PHP_FunctionComp(77) or
      PHP_FunctionComp(87) or
      PHP_FunctionComp(113) or
      PHP_FunctionComp(776) or
      PHP_FunctionComp(999) or
      PHP_FunctionComp(1063) or
      PHP_FunctionComp(1190) or
      PHP_FunctionComp(1659) or
      PHP_FunctionComp(1756) or
      PHP_FunctionComp(1937) or
      PHP_FunctionComp(2054) or
      PHP_FunctionComp(2361) or
      PHP_FunctionComp(2479) or
      PHP_FunctionComp(2614) or
      PHP_FunctionComp(2722) or
      PHP_FunctionComp(2767) or
      PHP_FunctionComp(2905) or
      PHP_FunctionComp(2930) or
      PHP_FunctionComp(3022) or
      PHP_FunctionComp(3036) or
      PHP_FunctionComp(3354) or
      PHP_FunctionComp(3393) or
      PHP_FunctionComp(3410) or
      PHP_FunctionComp(3883) or
      PHP_FunctionComp(3895) or
      PHP_FunctionComp(4399) or
      PHP_FunctionComp(4489) or
      PHP_FunctionComp(4500) or
      PHP_FunctionComp(4650) or
      PHP_FunctionComp(4837) or
      PHP_FunctionComp(5001) or
      PHP_FunctionComp(5207) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc186: TtkTokenKind;
begin
  if  PHP_FunctionComp(469) or
      PHP_FunctionComp(746) or
      PHP_FunctionComp(757) or
      PHP_FunctionComp(1093) or
      PHP_FunctionComp(1160) or
      PHP_FunctionComp(1904) or
      PHP_FunctionComp(2016) or
      PHP_FunctionComp(2355) or
      PHP_FunctionComp(2447) or
      PHP_FunctionComp(2478) or
      PHP_FunctionComp(2553) or
      PHP_FunctionComp(2654) or
      PHP_FunctionComp(2685) or
      PHP_FunctionComp(2689) or
      PHP_FunctionComp(3234) or
      PHP_FunctionComp(3302) or
      PHP_FunctionComp(3304) or
      PHP_FunctionComp(3503) or
      PHP_FunctionComp(3770) or
      PHP_FunctionComp(3774) or
      PHP_FunctionComp(3879) or
      PHP_FunctionComp(3889) or
      PHP_FunctionComp(4135) or
      PHP_FunctionComp(4259) or
      PHP_FunctionComp(4276) or
      PHP_FunctionComp(4324) or
      PHP_FunctionComp(4332) or
      PHP_FunctionComp(4454) or
      PHP_FunctionComp(4649) or
      PHP_FunctionComp(4662) or
      PHP_FunctionComp(4983) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc187: TtkTokenKind;
begin
  if  PHP_FunctionComp(260) or
      PHP_FunctionComp(669) or
      PHP_FunctionComp(922) or
      PHP_FunctionComp(969) or
      PHP_FunctionComp(1122) or
      PHP_FunctionComp(1650) or
      PHP_FunctionComp(1891) or
      PHP_FunctionComp(1943) or
      PHP_FunctionComp(1963) or
      PHP_FunctionComp(1998) or
      PHP_FunctionComp(2090) or
      PHP_FunctionComp(2229) or
      PHP_FunctionComp(2246) or
      PHP_FunctionComp(2509) or
      PHP_FunctionComp(2653) or
      PHP_FunctionComp(2906) or
      PHP_FunctionComp(2985) or
      PHP_FunctionComp(3628) or
      PHP_FunctionComp(3766) or
      PHP_FunctionComp(4188) or
      PHP_FunctionComp(4297) or
      PHP_FunctionComp(4387) or
      PHP_FunctionComp(4681) or
      PHP_FunctionComp(4727) or
      PHP_FunctionComp(4737) or
      PHP_FunctionComp(4757) or
      PHP_FunctionComp(4762) or
      PHP_FunctionComp(5228) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc188: TtkTokenKind;
begin
  if  PHP_FunctionComp(189) or
      PHP_FunctionComp(329) or
      PHP_FunctionComp(404) or
      PHP_FunctionComp(552) or
      PHP_FunctionComp(608) or
      PHP_FunctionComp(921) or
      PHP_FunctionComp(1422) or
      PHP_FunctionComp(1474) or
      PHP_FunctionComp(1838) or
      PHP_FunctionComp(1914) or
      PHP_FunctionComp(2179) or
      PHP_FunctionComp(2383) or
      PHP_FunctionComp(2392) or
      PHP_FunctionComp(2777) or
      PHP_FunctionComp(2955) or
      PHP_FunctionComp(3028) or
      PHP_FunctionComp(3041) or
      PHP_FunctionComp(3081) or
      PHP_FunctionComp(3175) or
      PHP_FunctionComp(3257) or
      PHP_FunctionComp(3327) or
      PHP_FunctionComp(3733) or
      PHP_FunctionComp(3916) or
      PHP_FunctionComp(3953) or
      PHP_FunctionComp(4287) or
      PHP_FunctionComp(4437) or
      PHP_FunctionComp(4442) or
      PHP_FunctionComp(4460) or
      PHP_FunctionComp(4845) or
      PHP_FunctionComp(4981) or
      PHP_FunctionComp(4998) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc189: TtkTokenKind;
begin
  if  PHP_FunctionComp(83) or
      PHP_FunctionComp(284) or
      PHP_FunctionComp(508) or
      PHP_FunctionComp(636) or
      PHP_FunctionComp(1038) or
      PHP_FunctionComp(1598) or
      PHP_FunctionComp(1626) or
      PHP_FunctionComp(1642) or
      PHP_FunctionComp(1709) or
      PHP_FunctionComp(1931) or
      PHP_FunctionComp(2341) or
      PHP_FunctionComp(2682) or
      PHP_FunctionComp(2789) or
      PHP_FunctionComp(2868) or
      PHP_FunctionComp(2983) or
      PHP_FunctionComp(3335) or
      PHP_FunctionComp(3526) or
      PHP_FunctionComp(3567) or
      PHP_FunctionComp(3710) or
      PHP_FunctionComp(4916) or
      PHP_FunctionComp(4993) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc190: TtkTokenKind;
begin
  if  PHP_FunctionComp(274) or
      PHP_FunctionComp(335) or
      PHP_FunctionComp(475) or
      PHP_FunctionComp(895) or
      PHP_FunctionComp(1271) or
      PHP_FunctionComp(1297) or
      PHP_FunctionComp(1522) or
      PHP_FunctionComp(1672) or
      PHP_FunctionComp(1724) or
      PHP_FunctionComp(1759) or
      PHP_FunctionComp(1976) or
      PHP_FunctionComp(2448) or
      PHP_FunctionComp(2492) or
      PHP_FunctionComp(2707) or
      PHP_FunctionComp(2900) or
      PHP_FunctionComp(3407) or
      PHP_FunctionComp(3420) or
      PHP_FunctionComp(3569) or
      PHP_FunctionComp(4053) or
      PHP_FunctionComp(4119) or
      PHP_FunctionComp(4390) or
      PHP_FunctionComp(4505) or
      PHP_FunctionComp(4520) or
      PHP_FunctionComp(4850) or
      PHP_FunctionComp(5090) or
      PHP_FunctionComp(5179) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc191: TtkTokenKind;
begin
  if  PHP_FunctionComp(205) or
      PHP_FunctionComp(635) or
      PHP_FunctionComp(694) or
      PHP_FunctionComp(901) or
      PHP_FunctionComp(903) or
      PHP_FunctionComp(939) or
      PHP_FunctionComp(1926) or
      PHP_FunctionComp(1990) or
      PHP_FunctionComp(2136) or
      PHP_FunctionComp(2766) or
      PHP_FunctionComp(2998) or
      PHP_FunctionComp(3007) or
      PHP_FunctionComp(3111) or
      PHP_FunctionComp(3149) or
      PHP_FunctionComp(3344) or
      PHP_FunctionComp(3475) or
      PHP_FunctionComp(3870) or
      PHP_FunctionComp(3890) or
      PHP_FunctionComp(3976) or
      PHP_FunctionComp(4072) or
      PHP_FunctionComp(4831) or
      PHP_FunctionComp(4874) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc192: TtkTokenKind;
begin
  if  PHP_FunctionComp(174) or
      PHP_FunctionComp(482) or
      PHP_FunctionComp(906) or
      PHP_FunctionComp(976) or
      PHP_FunctionComp(1104) or
      PHP_FunctionComp(1486) or
      PHP_FunctionComp(1624) or
      PHP_FunctionComp(1955) or
      PHP_FunctionComp(2120) or
      PHP_FunctionComp(2369) or
      PHP_FunctionComp(2384) or
      PHP_FunctionComp(2431) or
      PHP_FunctionComp(2488) or
      PHP_FunctionComp(2672) or
      PHP_FunctionComp(2823) or
      PHP_FunctionComp(2825) or
      PHP_FunctionComp(2992) or
      PHP_FunctionComp(3221) or
      PHP_FunctionComp(3673) or
      PHP_FunctionComp(4046) or
      PHP_FunctionComp(4108) or
      PHP_FunctionComp(4231) or
      PHP_FunctionComp(4695) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc193: TtkTokenKind;
begin
  if  PHP_FunctionComp(477) or
      PHP_FunctionComp(484) or
      PHP_FunctionComp(538) or
      PHP_FunctionComp(559) or
      PHP_FunctionComp(744) or
      PHP_FunctionComp(748) or
      PHP_FunctionComp(989) or
      PHP_FunctionComp(1021) or
      PHP_FunctionComp(1735) or
      PHP_FunctionComp(1942) or
      PHP_FunctionComp(1953) or
      PHP_FunctionComp(2193) or
      PHP_FunctionComp(2524) or
      PHP_FunctionComp(2826) or
      PHP_FunctionComp(2841) or
      PHP_FunctionComp(3006) or
      PHP_FunctionComp(3018) or
      PHP_FunctionComp(3038) or
      PHP_FunctionComp(3436) or
      PHP_FunctionComp(4277) or
      PHP_FunctionComp(4299) or
      PHP_FunctionComp(4449) or
      PHP_FunctionComp(4479) or
      PHP_FunctionComp(4548) or
      PHP_FunctionComp(4555) or
      PHP_FunctionComp(4626) or
      PHP_FunctionComp(4749) or
      PHP_FunctionComp(5006) or
      PHP_FunctionComp(5085) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc194: TtkTokenKind;
begin
  if  PHP_FunctionComp(142) or
      PHP_FunctionComp(203) or
      PHP_FunctionComp(970) or
      PHP_FunctionComp(1410) or
      PHP_FunctionComp(1479) or
      PHP_FunctionComp(1695) or
      PHP_FunctionComp(1913) or
      PHP_FunctionComp(1933) or
      PHP_FunctionComp(2345) or
      PHP_FunctionComp(2357) or
      PHP_FunctionComp(2443) or
      PHP_FunctionComp(2670) or
      PHP_FunctionComp(2820) or
      PHP_FunctionComp(2996) or
      PHP_FunctionComp(3154) or
      PHP_FunctionComp(3396) or
      PHP_FunctionComp(3560) or
      PHP_FunctionComp(3562) or
      PHP_FunctionComp(3772) or
      PHP_FunctionComp(4148) or
      PHP_FunctionComp(4154) or
      PHP_FunctionComp(4216) or
      PHP_FunctionComp(4330) or
      PHP_FunctionComp(4473) or
      PHP_FunctionComp(4526) or
      PHP_FunctionComp(4596) or
      PHP_FunctionComp(4810) or
      PHP_FunctionComp(5011) or
      PHP_FunctionComp(5082) or
      PHP_FunctionComp(5130) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc195: TtkTokenKind;
begin
  if  PHP_FunctionComp(598) or
      PHP_FunctionComp(948) or
      PHP_FunctionComp(1084) or
      PHP_FunctionComp(1321) or
      PHP_FunctionComp(1558) or
      PHP_FunctionComp(1765) or
      PHP_FunctionComp(1939) or
      PHP_FunctionComp(2095) or
      PHP_FunctionComp(2455) or
      PHP_FunctionComp(2953) or
      PHP_FunctionComp(3155) or
      PHP_FunctionComp(3401) or
      PHP_FunctionComp(3520) or
      PHP_FunctionComp(3550) or
      PHP_FunctionComp(4678) or
      PHP_FunctionComp(4889) or
      PHP_FunctionComp(4962) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc196: TtkTokenKind;
begin
  if  PHP_FunctionComp(979) or
      PHP_FunctionComp(996) or
      PHP_FunctionComp(1070) or
      PHP_FunctionComp(1115) or
      PHP_FunctionComp(1132) or
      PHP_FunctionComp(1894) or
      PHP_FunctionComp(1908) or
      PHP_FunctionComp(1912) or
      PHP_FunctionComp(2221) or
      PHP_FunctionComp(2565) or
      PHP_FunctionComp(2802) or
      PHP_FunctionComp(2959) or
      PHP_FunctionComp(2980) or
      PHP_FunctionComp(3200) or
      PHP_FunctionComp(3245) or
      PHP_FunctionComp(3321) or
      PHP_FunctionComp(3470) or
      PHP_FunctionComp(3575) or
      PHP_FunctionComp(3734) or
      PHP_FunctionComp(4679) or
      PHP_FunctionComp(4779) or
      PHP_FunctionComp(4792) or
      PHP_FunctionComp(4920) or
      PHP_FunctionComp(5080) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc197: TtkTokenKind;
begin
  if  PHP_FunctionComp(206) or
      PHP_FunctionComp(261) or
      PHP_FunctionComp(346) or
      PHP_FunctionComp(630) or
      PHP_FunctionComp(986) or
      PHP_FunctionComp(990) or
      PHP_FunctionComp(1052) or
      PHP_FunctionComp(1629) or
      PHP_FunctionComp(1757) or
      PHP_FunctionComp(1987) or
      PHP_FunctionComp(2262) or
      PHP_FunctionComp(2340) or
      PHP_FunctionComp(2373) or
      PHP_FunctionComp(2629) or
      PHP_FunctionComp(2754) or
      PHP_FunctionComp(2858) or
      PHP_FunctionComp(3062) or
      PHP_FunctionComp(3319) or
      PHP_FunctionComp(3559) or
      PHP_FunctionComp(4090) or
      PHP_FunctionComp(4144) or
      PHP_FunctionComp(4353) or
      PHP_FunctionComp(4541) or
      PHP_FunctionComp(4551) or
      PHP_FunctionComp(4666) or
      PHP_FunctionComp(4725) or
      PHP_FunctionComp(5016) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc198: TtkTokenKind;
begin
  if  PHP_FunctionComp(364) or
      PHP_FunctionComp(479) or
      PHP_FunctionComp(957) or
      PHP_FunctionComp(1402) or
      PHP_FunctionComp(1602) or
      PHP_FunctionComp(1762) or
      PHP_FunctionComp(1806) or
      PHP_FunctionComp(2043) or
      PHP_FunctionComp(2317) or
      PHP_FunctionComp(2378) or
      PHP_FunctionComp(2483) or
      PHP_FunctionComp(2847) or
      PHP_FunctionComp(2957) or
      PHP_FunctionComp(3030) or
      PHP_FunctionComp(3260) or
      PHP_FunctionComp(3325) or
      PHP_FunctionComp(3340) or
      PHP_FunctionComp(3435) or
      PHP_FunctionComp(3585) or
      PHP_FunctionComp(3837) or
      PHP_FunctionComp(3856) or
      PHP_FunctionComp(4056) or
      PHP_FunctionComp(4071) or
      PHP_FunctionComp(4463) or
      PHP_FunctionComp(4690) or
      PHP_FunctionComp(4694) or
      PHP_FunctionComp(4971) or
      PHP_FunctionComp(5031) or
      PHP_FunctionComp(5216) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc199: TtkTokenKind;
begin
  if  PHP_FunctionComp(444) or
      PHP_FunctionComp(1088) or
      PHP_FunctionComp(1106) or
      PHP_FunctionComp(1262) or
      PHP_FunctionComp(1328) or
      PHP_FunctionComp(1833) or
      PHP_FunctionComp(1880) or
      PHP_FunctionComp(2286) or
      PHP_FunctionComp(2449) or
      PHP_FunctionComp(2918) or
      PHP_FunctionComp(3311) or
      PHP_FunctionComp(3347) or
      PHP_FunctionComp(3572) or
      PHP_FunctionComp(3588) or
      PHP_FunctionComp(3787) or
      PHP_FunctionComp(4263) or
      PHP_FunctionComp(4269) or
      PHP_FunctionComp(4339) or
      PHP_FunctionComp(4458) or
      PHP_FunctionComp(4628) or
      PHP_FunctionComp(4728) or
      PHP_FunctionComp(4732) or
      PHP_FunctionComp(4733) or
      PHP_FunctionComp(5237) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc200: TtkTokenKind;
begin
  if  PHP_FunctionComp(240) or
      PHP_FunctionComp(304) or
      PHP_FunctionComp(457) or
      PHP_FunctionComp(1414) or
      PHP_FunctionComp(1658) or
      PHP_FunctionComp(2025) or
      PHP_FunctionComp(2232) or
      PHP_FunctionComp(2277) or
      PHP_FunctionComp(2312) or
      PHP_FunctionComp(2664) or
      PHP_FunctionComp(2819) or
      PHP_FunctionComp(2974) or
      PHP_FunctionComp(3012) or
      PHP_FunctionComp(3536) or
      PHP_FunctionComp(3789) or
      PHP_FunctionComp(4141) or
      PHP_FunctionComp(4155) or
      PHP_FunctionComp(4284) or
      PHP_FunctionComp(4295) or
      PHP_FunctionComp(4497) or
      PHP_FunctionComp(4744) or
      PHP_FunctionComp(4803) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc201: TtkTokenKind;
begin
  if  PHP_FunctionComp(100) or
      PHP_FunctionComp(134) or
      PHP_FunctionComp(472) or
      PHP_FunctionComp(496) or
      PHP_FunctionComp(535) or
      PHP_FunctionComp(857) or
      PHP_FunctionComp(927) or
      PHP_FunctionComp(1606) or
      PHP_FunctionComp(1744) or
      PHP_FunctionComp(1841) or
      PHP_FunctionComp(2388) or
      PHP_FunctionComp(2467) or
      PHP_FunctionComp(2786) or
      PHP_FunctionComp(2831) or
      PHP_FunctionComp(3054) or
      PHP_FunctionComp(3059) or
      PHP_FunctionComp(3472) or
      PHP_FunctionComp(3682) or
      PHP_FunctionComp(3891) or
      PHP_FunctionComp(4107) or
      PHP_FunctionComp(4604) or
      PHP_FunctionComp(4673) or
      PHP_FunctionComp(4674) or
      PHP_FunctionComp(4721) or
      PHP_FunctionComp(4770) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc202: TtkTokenKind;
begin
  if  PHP_FunctionComp(487) or
      PHP_FunctionComp(498) or
      PHP_FunctionComp(950) or
      PHP_FunctionComp(1315) or
      PHP_FunctionComp(1331) or
      PHP_FunctionComp(1427) or
      PHP_FunctionComp(1890) or
      PHP_FunctionComp(1905) or
      PHP_FunctionComp(1911) or
      PHP_FunctionComp(2076) or
      PHP_FunctionComp(2420) or
      PHP_FunctionComp(2541) or
      PHP_FunctionComp(2907) or
      PHP_FunctionComp(2969) or
      PHP_FunctionComp(3029) or
      PHP_FunctionComp(3419) or
      PHP_FunctionComp(3483) or
      PHP_FunctionComp(3615) or
      PHP_FunctionComp(3616) or
      PHP_FunctionComp(3892) or
      PHP_FunctionComp(4428) or
      PHP_FunctionComp(4536) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc203: TtkTokenKind;
begin
  if  PHP_FunctionComp(279) or
      PHP_FunctionComp(914) or
      PHP_FunctionComp(1335) or
      PHP_FunctionComp(1337) or
      PHP_FunctionComp(1733) or
      PHP_FunctionComp(2259) or
      PHP_FunctionComp(2463) or
      PHP_FunctionComp(2741) or
      PHP_FunctionComp(2849) or
      PHP_FunctionComp(3055) or
      PHP_FunctionComp(3478) or
      PHP_FunctionComp(3963) or
      PHP_FunctionComp(4022) or
      PHP_FunctionComp(4080) or
      PHP_FunctionComp(4098) or
      PHP_FunctionComp(4228) or
      PHP_FunctionComp(4257) or
      PHP_FunctionComp(4296) or
      PHP_FunctionComp(4351) or
      PHP_FunctionComp(4822) or
      PHP_FunctionComp(4847) or
      PHP_FunctionComp(4977) or
      PHP_FunctionComp(5156) or
      PHP_FunctionComp(5226) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc204: TtkTokenKind;
begin
  if  PHP_FunctionComp(184) or
      PHP_FunctionComp(584) or
      PHP_FunctionComp(1091) or
      PHP_FunctionComp(1413) or
      PHP_FunctionComp(1732) or
      PHP_FunctionComp(2175) or
      PHP_FunctionComp(2261) or
      PHP_FunctionComp(2410) or
      PHP_FunctionComp(2827) or
      PHP_FunctionComp(2917) or
      PHP_FunctionComp(2921) or
      PHP_FunctionComp(2938) or
      PHP_FunctionComp(2941) or
      PHP_FunctionComp(2993) or
      PHP_FunctionComp(3017) or
      PHP_FunctionComp(3150) or
      PHP_FunctionComp(3416) or
      PHP_FunctionComp(3851) or
      PHP_FunctionComp(3947) or
      PHP_FunctionComp(4027) or
      PHP_FunctionComp(4178) or
      PHP_FunctionComp(4328) or
      PHP_FunctionComp(4354) or
      PHP_FunctionComp(4723) or
      PHP_FunctionComp(4854) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc205: TtkTokenKind;
begin
  if  PHP_FunctionComp(185) or
      PHP_FunctionComp(972) or
      PHP_FunctionComp(1157) or
      PHP_FunctionComp(1687) or
      PHP_FunctionComp(1924) or
      PHP_FunctionComp(1948) or
      PHP_FunctionComp(2419) or
      PHP_FunctionComp(2534) or
      PHP_FunctionComp(2745) or
      PHP_FunctionComp(2788) or
      PHP_FunctionComp(2853) or
      PHP_FunctionComp(3061) or
      PHP_FunctionComp(3096) or
      PHP_FunctionComp(3394) or
      PHP_FunctionComp(3821) or
      PHP_FunctionComp(3923) or
      PHP_FunctionComp(4379) or
      PHP_FunctionComp(4400) or
      PHP_FunctionComp(4478) or
      PHP_FunctionComp(4691) or
      PHP_FunctionComp(4724) or
      PHP_FunctionComp(4726) or
      PHP_FunctionComp(4780) or
      PHP_FunctionComp(4839) or
      PHP_FunctionComp(5014) or
      PHP_FunctionComp(5042) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc206: TtkTokenKind;
begin
  if  PHP_FunctionComp(78) or
      PHP_FunctionComp(283) or
      PHP_FunctionComp(299) or
      PHP_FunctionComp(396) or
      PHP_FunctionComp(655) or
      PHP_FunctionComp(904) or
      PHP_FunctionComp(1538) or
      PHP_FunctionComp(1640) or
      PHP_FunctionComp(2417) or
      PHP_FunctionComp(2920) or
      PHP_FunctionComp(3324) or
      PHP_FunctionComp(3408) or
      PHP_FunctionComp(3769) or
      PHP_FunctionComp(4091) or
      PHP_FunctionComp(4093) or
      PHP_FunctionComp(4185) or
      PHP_FunctionComp(4352) or
      PHP_FunctionComp(4613) or
      PHP_FunctionComp(4616) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc207: TtkTokenKind;
begin
  if  PHP_FunctionComp(775) or
      PHP_FunctionComp(983) or
      PHP_FunctionComp(1042) or
      PHP_FunctionComp(1102) or
      PHP_FunctionComp(2109) or
      PHP_FunctionComp(2176) or
      PHP_FunctionComp(2411) or
      PHP_FunctionComp(2673) or
      PHP_FunctionComp(2922) or
      PHP_FunctionComp(2923) or
      PHP_FunctionComp(3001) or
      PHP_FunctionComp(3712) or
      PHP_FunctionComp(3765) or
      PHP_FunctionComp(3790) or
      PHP_FunctionComp(3866) or
      PHP_FunctionComp(4069) or
      PHP_FunctionComp(4256) or
      PHP_FunctionComp(4397) or
      PHP_FunctionComp(4773) or
      PHP_FunctionComp(4900) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc208: TtkTokenKind;
begin
  if  PHP_FunctionComp(420) or
      PHP_FunctionComp(685) or
      PHP_FunctionComp(1135) or
      PHP_FunctionComp(1826) or
      PHP_FunctionComp(2053) or
      PHP_FunctionComp(2110) or
      PHP_FunctionComp(2289) or
      PHP_FunctionComp(2569) or
      PHP_FunctionComp(2662) or
      PHP_FunctionComp(2735) or
      PHP_FunctionComp(2867) or
      PHP_FunctionComp(3023) or
      PHP_FunctionComp(3042) or
      PHP_FunctionComp(3092) or
      PHP_FunctionComp(3607) or
      PHP_FunctionComp(4024) or
      PHP_FunctionComp(4394) or
      PHP_FunctionComp(4418) or
      PHP_FunctionComp(5091) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc209: TtkTokenKind;
begin
  if  PHP_FunctionComp(308) or
      PHP_FunctionComp(545) or
      PHP_FunctionComp(780) or
      PHP_FunctionComp(1035) or
      PHP_FunctionComp(2308) or
      PHP_FunctionComp(2326) or
      PHP_FunctionComp(2418) or
      PHP_FunctionComp(2732) or
      PHP_FunctionComp(2790) or
      PHP_FunctionComp(2880) or
      PHP_FunctionComp(2944) or
      PHP_FunctionComp(3320) or
      PHP_FunctionComp(3398) or
      PHP_FunctionComp(3570) or
      PHP_FunctionComp(3580) or
      PHP_FunctionComp(3844) or
      PHP_FunctionComp(4298) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc210: TtkTokenKind;
begin
  if  PHP_FunctionComp(245) or
      PHP_FunctionComp(1276) or
      PHP_FunctionComp(2356) or
      PHP_FunctionComp(2956) or
      PHP_FunctionComp(3043) or
      PHP_FunctionComp(3210) or
      PHP_FunctionComp(3259) or
      PHP_FunctionComp(3984) or
      PHP_FunctionComp(4067) or
      PHP_FunctionComp(4079) or
      PHP_FunctionComp(4424) or
      PHP_FunctionComp(4429) or
      PHP_FunctionComp(4441) or
      PHP_FunctionComp(4627) or
      PHP_FunctionComp(4763) or
      PHP_FunctionComp(4980) or
      PHP_FunctionComp(5043) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc211: TtkTokenKind;
begin
  if  PHP_FunctionComp(397) or
      PHP_FunctionComp(973) or
      PHP_FunctionComp(1036) or
      PHP_FunctionComp(1550) or
      PHP_FunctionComp(1696) or
      PHP_FunctionComp(2055) or
      PHP_FunctionComp(2575) or
      PHP_FunctionComp(2755) or
      PHP_FunctionComp(2861) or
      PHP_FunctionComp(2972) or
      PHP_FunctionComp(3004) or
      PHP_FunctionComp(3005) or
      PHP_FunctionComp(3397) or
      PHP_FunctionComp(3557) or
      PHP_FunctionComp(3726) or
      PHP_FunctionComp(4106) or
      PHP_FunctionComp(4150) or
      PHP_FunctionComp(4417) or
      PHP_FunctionComp(4480) or
      PHP_FunctionComp(4693) or
      PHP_FunctionComp(4753) or
      PHP_FunctionComp(4768) or
      PHP_FunctionComp(4968) or
      PHP_FunctionComp(5051) or
      PHP_FunctionComp(5110) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc212: TtkTokenKind;
begin
  if  PHP_FunctionComp(152) or
      PHP_FunctionComp(426) or
      PHP_FunctionComp(492) or
      PHP_FunctionComp(828) or
      PHP_FunctionComp(1476) or
      PHP_FunctionComp(1481) or
      PHP_FunctionComp(1686) or
      PHP_FunctionComp(1936) or
      PHP_FunctionComp(2242) or
      PHP_FunctionComp(3011) or
      PHP_FunctionComp(3773) or
      PHP_FunctionComp(4274) or
      PHP_FunctionComp(4509) or
      PHP_FunctionComp(4552) or
      PHP_FunctionComp(4634) or
      PHP_FunctionComp(4685) or
      PHP_FunctionComp(4751) or
      PHP_FunctionComp(4856) or
      PHP_FunctionComp(4935) or
      PHP_FunctionComp(5004) or
      PHP_FunctionComp(5155) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc213: TtkTokenKind;
begin
  if  PHP_FunctionComp(143) or
      PHP_FunctionComp(553) or
      PHP_FunctionComp(2030) or
      PHP_FunctionComp(2634) or
      PHP_FunctionComp(2769) or
      PHP_FunctionComp(2863) or
      PHP_FunctionComp(3025) or
      PHP_FunctionComp(3223) or
      PHP_FunctionComp(3227) or
      PHP_FunctionComp(3402) or
      PHP_FunctionComp(3418) or
      PHP_FunctionComp(3468) or
      PHP_FunctionComp(3552) or
      PHP_FunctionComp(4010) or
      PHP_FunctionComp(4765) or
      PHP_FunctionComp(4767) or
      PHP_FunctionComp(4771) or
      PHP_FunctionComp(4880) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc214: TtkTokenKind;
begin
  if  PHP_FunctionComp(337) or
      PHP_FunctionComp(476) or
      PHP_FunctionComp(561) or
      PHP_FunctionComp(649) or
      PHP_FunctionComp(1197) or
      PHP_FunctionComp(1326) or
      PHP_FunctionComp(1713) or
      PHP_FunctionComp(1956) or
      PHP_FunctionComp(2668) or
      PHP_FunctionComp(2952) or
      PHP_FunctionComp(2973) or
      PHP_FunctionComp(3328) or
      PHP_FunctionComp(3405) or
      PHP_FunctionComp(3747) or
      PHP_FunctionComp(4038) or
      PHP_FunctionComp(4457) or
      PHP_FunctionComp(4554) or
      PHP_FunctionComp(4624) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc215: TtkTokenKind;
begin
  if  PHP_FunctionComp(389) or
      PHP_FunctionComp(631) or
      PHP_FunctionComp(993) or
      PHP_FunctionComp(1066) or
      PHP_FunctionComp(1210) or
      PHP_FunctionComp(1327) or
      PHP_FunctionComp(1547) or
      PHP_FunctionComp(1715) or
      PHP_FunctionComp(1902) or
      PHP_FunctionComp(2583) or
      PHP_FunctionComp(2674) or
      PHP_FunctionComp(2778) or
      PHP_FunctionComp(3707) or
      PHP_FunctionComp(3778) or
      PHP_FunctionComp(3936) or
      PHP_FunctionComp(4264) or
      PHP_FunctionComp(4471) or
      PHP_FunctionComp(4495) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc216: TtkTokenKind;
begin
  if  PHP_FunctionComp(177) or
      PHP_FunctionComp(226) or
      PHP_FunctionComp(443) or
      PHP_FunctionComp(640) or
      PHP_FunctionComp(642) or
      PHP_FunctionComp(926) or
      PHP_FunctionComp(1072) or
      PHP_FunctionComp(1470) or
      PHP_FunctionComp(1539) or
      PHP_FunctionComp(1865) or
      PHP_FunctionComp(2091) or
      PHP_FunctionComp(2347) or
      PHP_FunctionComp(2372) or
      PHP_FunctionComp(2428) or
      PHP_FunctionComp(2460) or
      PHP_FunctionComp(2680) or
      PHP_FunctionComp(2967) or
      PHP_FunctionComp(3208) or
      PHP_FunctionComp(3256) or
      PHP_FunctionComp(3312) or
      PHP_FunctionComp(3955) or
      PHP_FunctionComp(4055) or
      PHP_FunctionComp(4529) or
      PHP_FunctionComp(4629) or
      PHP_FunctionComp(4775) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc217: TtkTokenKind;
begin
  if  PHP_FunctionComp(237) or
      PHP_FunctionComp(423) or
      PHP_FunctionComp(827) or
      PHP_FunctionComp(994) or
      PHP_FunctionComp(1067) or
      PHP_FunctionComp(1421) or
      PHP_FunctionComp(1655) or
      PHP_FunctionComp(1903) or
      PHP_FunctionComp(2533) or
      PHP_FunctionComp(2567) or
      PHP_FunctionComp(2572) or
      PHP_FunctionComp(2855) or
      PHP_FunctionComp(3326) or
      PHP_FunctionComp(3514) or
      PHP_FunctionComp(4035) or
      PHP_FunctionComp(4116) or
      PHP_FunctionComp(4730) or
      PHP_FunctionComp(4745) or
      PHP_FunctionComp(4857) or
      PHP_FunctionComp(4978) or
      PHP_FunctionComp(5026) or
      PHP_FunctionComp(5171) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc218: TtkTokenKind;
begin
  if  PHP_FunctionComp(384) or
      PHP_FunctionComp(386) or
      PHP_FunctionComp(415) or
      PHP_FunctionComp(1595) or
      PHP_FunctionComp(2108) or
      PHP_FunctionComp(2848) or
      PHP_FunctionComp(2914) or
      PHP_FunctionComp(2940) or
      PHP_FunctionComp(3404) or
      PHP_FunctionComp(3676) or
      PHP_FunctionComp(4383) or
      PHP_FunctionComp(4408) or
      PHP_FunctionComp(4540) or
      PHP_FunctionComp(4683) or
      PHP_FunctionComp(4738) or
      PHP_FunctionComp(4743) or
      PHP_FunctionComp(4755) or
      PHP_FunctionComp(4776) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc219: TtkTokenKind;
begin
  if  PHP_FunctionComp(366) or
      PHP_FunctionComp(858) or
      PHP_FunctionComp(968) or
      PHP_FunctionComp(1133) or
      PHP_FunctionComp(1341) or
      PHP_FunctionComp(2571) or
      PHP_FunctionComp(2576) or
      PHP_FunctionComp(2657) or
      PHP_FunctionComp(2658) or
      PHP_FunctionComp(2883) or
      PHP_FunctionComp(3681) or
      PHP_FunctionComp(3767) or
      PHP_FunctionComp(3786) or
      PHP_FunctionComp(3888) or
      PHP_FunctionComp(3961) or
      PHP_FunctionComp(4077) or
      PHP_FunctionComp(4597) or
      PHP_FunctionComp(4611) or
      PHP_FunctionComp(4769) or
      PHP_FunctionComp(4995) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc220: TtkTokenKind;
begin
  if  PHP_FunctionComp(101) or
      PHP_FunctionComp(135) or
      PHP_FunctionComp(638) or
      PHP_FunctionComp(1416) or
      PHP_FunctionComp(1557) or
      PHP_FunctionComp(1730) or
      PHP_FunctionComp(1895) or
      PHP_FunctionComp(2366) or
      PHP_FunctionComp(2520) or
      PHP_FunctionComp(2676) or
      PHP_FunctionComp(2756) or
      PHP_FunctionComp(2800) or
      PHP_FunctionComp(2990) or
      PHP_FunctionComp(3020) or
      PHP_FunctionComp(3072) or
      PHP_FunctionComp(3665) or
      PHP_FunctionComp(3763) or
      PHP_FunctionComp(4226) or
      PHP_FunctionComp(4994) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc221: TtkTokenKind;
begin
  if  PHP_FunctionComp(830) or
      PHP_FunctionComp(832) or
      PHP_FunctionComp(852) or
      PHP_FunctionComp(1208) or
      PHP_FunctionComp(1431) or
      PHP_FunctionComp(1873) or
      PHP_FunctionComp(2228) or
      PHP_FunctionComp(2875) or
      PHP_FunctionComp(3709) or
      PHP_FunctionComp(3781) or
      PHP_FunctionComp(3980) or
      PHP_FunctionComp(4230) or
      PHP_FunctionComp(4260) or
      PHP_FunctionComp(4415) or
      PHP_FunctionComp(4599) or
      PHP_FunctionComp(4699) or
      PHP_FunctionComp(4701) or
      PHP_FunctionComp(4709) or
      PHP_FunctionComp(5159) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc222: TtkTokenKind;
begin
  if  PHP_FunctionComp(163) or
      PHP_FunctionComp(168) or
      PHP_FunctionComp(301) or
      PHP_FunctionComp(327) or
      PHP_FunctionComp(1158) or
      PHP_FunctionComp(2765) or
      PHP_FunctionComp(2824) or
      PHP_FunctionComp(3357) or
      PHP_FunctionComp(3819) or
      PHP_FunctionComp(3853) or
      PHP_FunctionComp(3900) or
      PHP_FunctionComp(4075) or
      PHP_FunctionComp(4466) or
      PHP_FunctionComp(4605) or
      PHP_FunctionComp(4758) or
      PHP_FunctionComp(4842) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc223: TtkTokenKind;
begin
  if  PHP_FunctionComp(287) or
      PHP_FunctionComp(319) or
      PHP_FunctionComp(854) or
      PHP_FunctionComp(1639) or
      PHP_FunctionComp(2564) or
      PHP_FunctionComp(2862) or
      PHP_FunctionComp(2981) or
      PHP_FunctionComp(4073) or
      PHP_FunctionComp(4549) or
      PHP_FunctionComp(4759) or
      PHP_FunctionComp(4975) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc224: TtkTokenKind;
begin
  if  PHP_FunctionComp(286) or
      PHP_FunctionComp(639) or
      PHP_FunctionComp(760) or
      PHP_FunctionComp(1005) or
      PHP_FunctionComp(1172) or
      PHP_FunctionComp(1928) or
      PHP_FunctionComp(2320) or
      PHP_FunctionComp(2346) or
      PHP_FunctionComp(2370) or
      PHP_FunctionComp(2780) or
      PHP_FunctionComp(2830) or
      PHP_FunctionComp(3063) or
      PHP_FunctionComp(3118) or
      PHP_FunctionComp(3267) or
      PHP_FunctionComp(3885) or
      PHP_FunctionComp(3886) or
      PHP_FunctionComp(3949) or
      PHP_FunctionComp(4036) or
      PHP_FunctionComp(4177) or
      PHP_FunctionComp(4275) or
      PHP_FunctionComp(4537) or
      PHP_FunctionComp(4539) or
      PHP_FunctionComp(4545) or
      PHP_FunctionComp(4774) or
      PHP_FunctionComp(4804) or
      PHP_FunctionComp(5056) or
      PHP_FunctionComp(5231) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc225: TtkTokenKind;
begin
  if  PHP_FunctionComp(316) or
      PHP_FunctionComp(387) or
      PHP_FunctionComp(395) or
      PHP_FunctionComp(2563) or
      PHP_FunctionComp(2779) or
      PHP_FunctionComp(2801) or
      PHP_FunctionComp(2971) or
      PHP_FunctionComp(3044) or
      PHP_FunctionComp(4631) or
      PHP_FunctionComp(4739) or
      PHP_FunctionComp(5076) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc226: TtkTokenKind;
begin
  if  PHP_FunctionComp(136) or
      PHP_FunctionComp(340) or
      PHP_FunctionComp(1952) or
      PHP_FunctionComp(2028) or
      PHP_FunctionComp(2083) or
      PHP_FunctionComp(2496) or
      PHP_FunctionComp(3296) or
      PHP_FunctionComp(3564) or
      PHP_FunctionComp(3713) or
      PHP_FunctionComp(3927) or
      PHP_FunctionComp(4487) or
      PHP_FunctionComp(4557) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc227: TtkTokenKind;
begin
  if  PHP_FunctionComp(481) or
      PHP_FunctionComp(853) or
      PHP_FunctionComp(920) or
      PHP_FunctionComp(977) or
      PHP_FunctionComp(1332) or
      PHP_FunctionComp(1906) or
      PHP_FunctionComp(2469) or
      PHP_FunctionComp(2573) or
      PHP_FunctionComp(3013) or
      PHP_FunctionComp(3309) or
      PHP_FunctionComp(3427) or
      PHP_FunctionComp(4058) or
      PHP_FunctionComp(4450) or
      PHP_FunctionComp(5068) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc228: TtkTokenKind;
begin
  if  PHP_FunctionComp(1621) or
      PHP_FunctionComp(1755) or
      PHP_FunctionComp(1899) or
      PHP_FunctionComp(3674) or
      PHP_FunctionComp(4409) or
      PHP_FunctionComp(4430) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc229: TtkTokenKind;
begin
  if  PHP_FunctionComp(800) or
      PHP_FunctionComp(934) or
      PHP_FunctionComp(1468) or
      PHP_FunctionComp(1620) or
      PHP_FunctionComp(1754) or
      PHP_FunctionComp(1910) or
      PHP_FunctionComp(1927) or
      PHP_FunctionComp(2105) or
      PHP_FunctionComp(2181) or
      PHP_FunctionComp(2458) or
      PHP_FunctionComp(2570) or
      PHP_FunctionComp(3014) or
      PHP_FunctionComp(3047) or
      PHP_FunctionComp(3100) or
      PHP_FunctionComp(3515) or
      PHP_FunctionComp(3798) or
      PHP_FunctionComp(3944) or
      PHP_FunctionComp(3962) or
      PHP_FunctionComp(4114) or
      PHP_FunctionComp(4217) or
      PHP_FunctionComp(4530) or
      PHP_FunctionComp(5057) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc230: TtkTokenKind;
begin
  if  PHP_FunctionComp(268) or
      PHP_FunctionComp(428) or
      PHP_FunctionComp(2263) or
      PHP_FunctionComp(3077) or
      PHP_FunctionComp(3119) or
      PHP_FunctionComp(3878) or
      PHP_FunctionComp(4425) or
      PHP_FunctionComp(4622) or
      PHP_FunctionComp(4777) or
      PHP_FunctionComp(4789) or
      PHP_FunctionComp(5033) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc231: TtkTokenKind;
begin
  if  PHP_FunctionComp(244) or
      PHP_FunctionComp(401) or
      PHP_FunctionComp(816) or
      PHP_FunctionComp(1678) or
      PHP_FunctionComp(2733) or
      PHP_FunctionComp(3409) or
      PHP_FunctionComp(3697) or
      PHP_FunctionComp(3776) or
      PHP_FunctionComp(3932) or
      PHP_FunctionComp(4374) or
      PHP_FunctionComp(4474) or
      PHP_FunctionComp(4791) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc232: TtkTokenKind;
begin
  if  PHP_FunctionComp(213) or
      PHP_FunctionComp(592) or
      PHP_FunctionComp(908) or
      PHP_FunctionComp(1151) or
      PHP_FunctionComp(1318) or
      PHP_FunctionComp(1907) or
      PHP_FunctionComp(1934) or
      PHP_FunctionComp(2812) or
      PHP_FunctionComp(2822) or
      PHP_FunctionComp(2838) or
      PHP_FunctionComp(2913) or
      PHP_FunctionComp(2965) or
      PHP_FunctionComp(3785) or
      PHP_FunctionComp(3897) or
      PHP_FunctionComp(4300) or
      PHP_FunctionComp(4601) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc233: TtkTokenKind;
begin
  if  PHP_FunctionComp(236) or
      PHP_FunctionComp(902) or
      PHP_FunctionComp(992) or
      PHP_FunctionComp(2523) or
      PHP_FunctionComp(2734) or
      PHP_FunctionComp(2834) or
      PHP_FunctionComp(2876) or
      PHP_FunctionComp(3027) or
      PHP_FunctionComp(3034) or
      PHP_FunctionComp(3568) or
      PHP_FunctionComp(3788) or
      PHP_FunctionComp(5139) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc234: TtkTokenKind;
begin
  if  PHP_FunctionComp(80) or
      PHP_FunctionComp(591) or
      PHP_FunctionComp(1211) or
      PHP_FunctionComp(1619) or
      PHP_FunctionComp(1716) or
      PHP_FunctionComp(2632) or
      PHP_FunctionComp(2832) or
      PHP_FunctionComp(2888) or
      PHP_FunctionComp(2991) or
      PHP_FunctionComp(3064) or
      PHP_FunctionComp(3099) or
      PHP_FunctionComp(3341) or
      PHP_FunctionComp(3406) or
      PHP_FunctionComp(3561) or
      PHP_FunctionComp(3894) or
      PHP_FunctionComp(3956) or
      PHP_FunctionComp(4023) or
      PHP_FunctionComp(4407) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc235: TtkTokenKind;
begin
  if  PHP_FunctionComp(644) or
      PHP_FunctionComp(1302) or
      PHP_FunctionComp(1351) or
      PHP_FunctionComp(2895) or
      PHP_FunctionComp(2994) or
      PHP_FunctionComp(3617) or
      PHP_FunctionComp(4081) or
      PHP_FunctionComp(4149) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc236: TtkTokenKind;
begin
  if  PHP_FunctionComp(483) or
      PHP_FunctionComp(1138) or
      PHP_FunctionComp(1542) or
      PHP_FunctionComp(1893) or
      PHP_FunctionComp(2180) or
      PHP_FunctionComp(2650) or
      PHP_FunctionComp(3056) or
      PHP_FunctionComp(3084) or
      PHP_FunctionComp(3124) or
      PHP_FunctionComp(4008) or
      PHP_FunctionComp(4037) or
      PHP_FunctionComp(4112) or
      PHP_FunctionComp(4151) or
      PHP_FunctionComp(4267) or
      PHP_FunctionComp(4303) or
      PHP_FunctionComp(4380) or
      PHP_FunctionComp(4558) or
      PHP_FunctionComp(4704) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc237: TtkTokenKind;
begin
  if  PHP_FunctionComp(388) or
      PHP_FunctionComp(421) or
      PHP_FunctionComp(1746) or
      PHP_FunctionComp(1768) or
      PHP_FunctionComp(1900) or
      PHP_FunctionComp(2473) or
      PHP_FunctionComp(2877) or
      PHP_FunctionComp(4128) or
      PHP_FunctionComp(4364) or
      PHP_FunctionComp(4675) or
      PHP_FunctionComp(5070) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc238: TtkTokenKind;
begin
  if  PHP_FunctionComp(239) or
      PHP_FunctionComp(579) or
      PHP_FunctionComp(718) or
      PHP_FunctionComp(910) or
      PHP_FunctionComp(932) or
      PHP_FunctionComp(1000) or
      PHP_FunctionComp(2454) or
      PHP_FunctionComp(2559) or
      PHP_FunctionComp(2635) or
      PHP_FunctionComp(2739) or
      PHP_FunctionComp(3085) or
      PHP_FunctionComp(3332) or
      PHP_FunctionComp(4097) or
      PHP_FunctionComp(4103) or
      PHP_FunctionComp(4229) or
      PHP_FunctionComp(4258) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc239: TtkTokenKind;
begin
  if  PHP_FunctionComp(633) or
      PHP_FunctionComp(1324) or
      PHP_FunctionComp(1704) or
      PHP_FunctionComp(2453) or
      PHP_FunctionComp(2557) or
      PHP_FunctionComp(2807) or
      PHP_FunctionComp(2816) or
      PHP_FunctionComp(3008) or
      PHP_FunctionComp(3033) or
      PHP_FunctionComp(3428) or
      PHP_FunctionComp(4102) or
      PHP_FunctionComp(5089) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc240: TtkTokenKind;
begin
  if  PHP_FunctionComp(981) or
      PHP_FunctionComp(997) or
      PHP_FunctionComp(1092) or
      PHP_FunctionComp(1126) or
      PHP_FunctionComp(2444) or
      PHP_FunctionComp(2866) or
      PHP_FunctionComp(3026) or
      PHP_FunctionComp(3338) or
      PHP_FunctionComp(3403) or
      PHP_FunctionComp(4042) or
      PHP_FunctionComp(4698) or
      PHP_FunctionComp(4969) or
      PHP_FunctionComp(5094) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc241: TtkTokenKind;
begin
  if  PHP_FunctionComp(81) or
      PHP_FunctionComp(946) or
      PHP_FunctionComp(1008) or
      PHP_FunctionComp(1059) or
      PHP_FunctionComp(1152) or
      PHP_FunctionComp(1245) or
      PHP_FunctionComp(1544) or
      PHP_FunctionComp(2408) or
      PHP_FunctionComp(2442) or
      PHP_FunctionComp(2878) or
      PHP_FunctionComp(3021) or
      PHP_FunctionComp(3152) or
      PHP_FunctionComp(3390) or
      PHP_FunctionComp(3719) or
      PHP_FunctionComp(3951) or
      PHP_FunctionComp(4764) or
      PHP_FunctionComp(4766) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc242: TtkTokenKind;
begin
  if  PHP_FunctionComp(74) or
      PHP_FunctionComp(109) or
      PHP_FunctionComp(150) or
      PHP_FunctionComp(978) or
      PHP_FunctionComp(1916) or
      PHP_FunctionComp(2433) or
      PHP_FunctionComp(3983) or
      PHP_FunctionComp(4189) or
      PHP_FunctionComp(4240) or
      PHP_FunctionComp(4255) or
      PHP_FunctionComp(5121) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc243: TtkTokenKind;
begin
  if  PHP_FunctionComp(1596) or
      PHP_FunctionComp(2612) or
      PHP_FunctionComp(2857) or
      PHP_FunctionComp(3362) or
      PHP_FunctionComp(3929) or
      PHP_FunctionComp(4174) or
      PHP_FunctionComp(4382) or
      PHP_FunctionComp(4482) or
      PHP_FunctionComp(4503) or
      PHP_FunctionComp(4676) or
      PHP_FunctionComp(4746) or
      PHP_FunctionComp(5008) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc244: TtkTokenKind;
begin
  if  PHP_FunctionComp(424) or
      PHP_FunctionComp(614) or
      PHP_FunctionComp(848) or
      PHP_FunctionComp(905) or
      PHP_FunctionComp(985) or
      PHP_FunctionComp(998) or
      PHP_FunctionComp(1938) or
      PHP_FunctionComp(3015) or
      PHP_FunctionComp(3411) or
      PHP_FunctionComp(4305) or
      PHP_FunctionComp(4485) or
      PHP_FunctionComp(4700) or
      PHP_FunctionComp(5183) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc245: TtkTokenKind;
begin
  if  PHP_FunctionComp(925) or
      PHP_FunctionComp(952) or
      PHP_FunctionComp(1098) or
      PHP_FunctionComp(1099) or
      PHP_FunctionComp(1300) or
      PHP_FunctionComp(2451) or
      PHP_FunctionComp(2873) or
      PHP_FunctionComp(4633) or
      PHP_FunctionComp(4830) or
      PHP_FunctionComp(5109) or
      PHP_FunctionComp(5115) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc246: TtkTokenKind;
begin
  if  PHP_FunctionComp(175) or
      PHP_FunctionComp(613) or
      PHP_FunctionComp(829) or
      PHP_FunctionComp(887) or
      PHP_FunctionComp(2367) or
      PHP_FunctionComp(2781) or
      PHP_FunctionComp(2887) or
      PHP_FunctionComp(3432) or
      PHP_FunctionComp(3999) or
      PHP_FunctionComp(4187) or
      PHP_FunctionComp(4266) or
      PHP_FunctionComp(4510) or
      PHP_FunctionComp(4535) or
      PHP_FunctionComp(4697) or
      PHP_FunctionComp(4815) or
      PHP_FunctionComp(5113) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc247: TtkTokenKind;
begin
  if  PHP_FunctionComp(108) or
      PHP_FunctionComp(171) or
      PHP_FunctionComp(305) or
      PHP_FunctionComp(643) or
      PHP_FunctionComp(974) or
      PHP_FunctionComp(1325) or
      PHP_FunctionComp(1329) or
      PHP_FunctionComp(1611) or
      PHP_FunctionComp(2813) or
      PHP_FunctionComp(3065) or
      PHP_FunctionComp(3901) or
      PHP_FunctionComp(4491) or
      PHP_FunctionComp(4538) or
      PHP_FunctionComp(4752) or
      PHP_FunctionComp(5053) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc248: TtkTokenKind;
begin
  if  PHP_FunctionComp(2507) or
      PHP_FunctionComp(3589) or
      PHP_FunctionComp(5083) or
      PHP_FunctionComp(5103) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc249: TtkTokenKind;
begin
  if  PHP_FunctionComp(383) or
      PHP_FunctionComp(590) or
      PHP_FunctionComp(641) or
      PHP_FunctionComp(942) or
      PHP_FunctionComp(1094) or
      PHP_FunctionComp(1909) or
      PHP_FunctionComp(2558) or
      PHP_FunctionComp(2782) or
      PHP_FunctionComp(2803) or
      PHP_FunctionComp(4262) or
      PHP_FunctionComp(4556) or
      PHP_FunctionComp(4778) or
      PHP_FunctionComp(5100) or
      PHP_FunctionComp(5138) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc250: TtkTokenKind;
begin
  if  PHP_FunctionComp(328) or
      PHP_FunctionComp(580) or
      PHP_FunctionComp(855) or
      PHP_FunctionComp(875) or
      PHP_FunctionComp(1314) or
      PHP_FunctionComp(1745) or
      PHP_FunctionComp(2879) or
      PHP_FunctionComp(3322) or
      PHP_FunctionComp(3339) or
      PHP_FunctionComp(3700) or
      PHP_FunctionComp(3958) or
      PHP_FunctionComp(4455) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc251: TtkTokenKind;
begin
  if  PHP_FunctionComp(499) or
      PHP_FunctionComp(537) or
      PHP_FunctionComp(1026) or
      PHP_FunctionComp(1304) or
      PHP_FunctionComp(1305) or
      PHP_FunctionComp(1306) or
      PHP_FunctionComp(1307) or
      PHP_FunctionComp(1308) or
      PHP_FunctionComp(2556) or
      PHP_FunctionComp(4031) or
      PHP_FunctionComp(4104) or
      PHP_FunctionComp(4227) or
      PHP_FunctionComp(4617) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc252: TtkTokenKind;
begin
  if  PHP_FunctionComp(534) or
      PHP_FunctionComp(1103) or
      PHP_FunctionComp(1702) or
      PHP_FunctionComp(2119) or
      PHP_FunctionComp(2530) or
      PHP_FunctionComp(2811) or
      PHP_FunctionComp(3125) or
      PHP_FunctionComp(3346) or
      PHP_FunctionComp(3467) or
      PHP_FunctionComp(4375) or
      PHP_FunctionComp(4702) or
      PHP_FunctionComp(4741) or
      PHP_FunctionComp(4783) or
      PHP_FunctionComp(5096) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc253: TtkTokenKind;
begin
  if  PHP_FunctionComp(474) or
      PHP_FunctionComp(838) or
      PHP_FunctionComp(888) or
      PHP_FunctionComp(1477) or
      PHP_FunctionComp(1717) or
      PHP_FunctionComp(2846) or
      PHP_FunctionComp(2919) or
      PHP_FunctionComp(3867) or
      PHP_FunctionComp(3978) or
      PHP_FunctionComp(4110) or
      PHP_FunctionComp(4261) or
      PHP_FunctionComp(4270) or
      PHP_FunctionComp(4550) or
      PHP_FunctionComp(4703) or
      PHP_FunctionComp(5036) or
      PHP_FunctionComp(5153) or
      PHP_FunctionComp(5184) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc254: TtkTokenKind;
begin
  if  PHP_FunctionComp(104) or
      PHP_FunctionComp(238) or
      PHP_FunctionComp(262) or
      PHP_FunctionComp(869) or
      PHP_FunctionComp(975) or
      PHP_FunctionComp(987) or
      PHP_FunctionComp(995) or
      PHP_FunctionComp(1007) or
      PHP_FunctionComp(1322) or
      PHP_FunctionComp(1723) or
      PHP_FunctionComp(2896) or
      PHP_FunctionComp(3350) or
      PHP_FunctionComp(4028) or
      PHP_FunctionComp(4100) or
      PHP_FunctionComp(4252) or
      PHP_FunctionComp(4784) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc255: TtkTokenKind;
begin
  if  PHP_FunctionComp(164) or
      PHP_FunctionComp(169) or
      PHP_FunctionComp(442) or
      PHP_FunctionComp(982) or
      PHP_FunctionComp(1011) or
      PHP_FunctionComp(1345) or
      PHP_FunctionComp(1551) or
      PHP_FunctionComp(1618) or
      PHP_FunctionComp(2527) or
      PHP_FunctionComp(2835) or
      PHP_FunctionComp(3051) or
      PHP_FunctionComp(3342) or
      PHP_FunctionComp(5180) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc256: TtkTokenKind;
begin
  if  PHP_FunctionComp(846) or
      PHP_FunctionComp(1772) or
      PHP_FunctionComp(1901) or
      PHP_FunctionComp(2201) or
      PHP_FunctionComp(2845) or
      PHP_FunctionComp(4467) or
      PHP_FunctionComp(4614) or
      PHP_FunctionComp(4710) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc257: TtkTokenKind;
begin
  if  PHP_FunctionComp(151) or
      PHP_FunctionComp(1460) or
      PHP_FunctionComp(1475) or
      PHP_FunctionComp(1674) or
      PHP_FunctionComp(1749) or
      PHP_FunctionComp(2843) or
      PHP_FunctionComp(3685) or
      PHP_FunctionComp(3986) or
      PHP_FunctionComp(4363) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc258: TtkTokenKind;
begin
  if  PHP_FunctionComp(868) or
      PHP_FunctionComp(1032) or
      PHP_FunctionComp(1249) or
      PHP_FunctionComp(1555) or
      PHP_FunctionComp(1681) or
      PHP_FunctionComp(2391) or
      PHP_FunctionComp(2794) or
      PHP_FunctionComp(2836) or
      PHP_FunctionComp(2871) or
      PHP_FunctionComp(4095) or
      PHP_FunctionComp(4232) or
      PHP_FunctionComp(4490) or
      PHP_FunctionComp(4696) or
      PHP_FunctionComp(4713) or
      PHP_FunctionComp(4742) or
      PHP_FunctionComp(5055) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc259: TtkTokenKind;
begin
  if  PHP_FunctionComp(1014) or
      PHP_FunctionComp(1299) or
      PHP_FunctionComp(2926) or
      PHP_FunctionComp(3002) or
      PHP_FunctionComp(3003) or
      PHP_FunctionComp(3315) or
      PHP_FunctionComp(3417) or
      PHP_FunctionComp(4502) or
      PHP_FunctionComp(4615) or
      PHP_FunctionComp(4816) or
      PHP_FunctionComp(5054) or
      PHP_FunctionComp(5122) or
      PHP_FunctionComp(5225) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc260: TtkTokenKind;
begin
  if  PHP_FunctionComp(962) or
      PHP_FunctionComp(1061) or
      PHP_FunctionComp(2844) or
      PHP_FunctionComp(4707) or
      PHP_FunctionComp(4782) or
      PHP_FunctionComp(5084) or
      PHP_FunctionComp(5157) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc261: TtkTokenKind;
begin
  if  PHP_FunctionComp(398) or
      PHP_FunctionComp(612) or
      PHP_FunctionComp(1613) or
      PHP_FunctionComp(1656) or
      PHP_FunctionComp(1657) or
      PHP_FunctionComp(1764) or
      PHP_FunctionComp(1915) or
      PHP_FunctionComp(3336) or
      PHP_FunctionComp(3387) or
      PHP_FunctionComp(3818) or
      PHP_FunctionComp(4029) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc262: TtkTokenKind;
begin
  if  PHP_FunctionComp(165) or
      PHP_FunctionComp(836) or
      PHP_FunctionComp(935) or
      PHP_FunctionComp(938) or
      PHP_FunctionComp(1423) or
      PHP_FunctionComp(3196) or
      PHP_FunctionComp(3261) or
      PHP_FunctionComp(3316) or
      PHP_FunctionComp(3977) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc263: TtkTokenKind;
begin
  if  PHP_FunctionComp(1087) or
      PHP_FunctionComp(1480) or
      PHP_FunctionComp(1636) or
      PHP_FunctionComp(3313) or
      PHP_FunctionComp(4220) or
      PHP_FunctionComp(4238) or
      PHP_FunctionComp(4265) or
      PHP_FunctionComp(4476) or
      PHP_FunctionComp(4527) or
      PHP_FunctionComp(4715) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc264: TtkTokenKind;
begin
  if  PHP_FunctionComp(551) or
      PHP_FunctionComp(959) or
      PHP_FunctionComp(2995) or
      PHP_FunctionComp(3351) or
      PHP_FunctionComp(3356) or
      PHP_FunctionComp(3777) or
      PHP_FunctionComp(4493) or
      PHP_FunctionComp(4717) or
      PHP_FunctionComp(5009) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc265: TtkTokenKind;
begin
  if  PHP_FunctionComp(893) or
      PHP_FunctionComp(1002) or
      PHP_FunctionComp(1541) or
      PHP_FunctionComp(2100) or
      PHP_FunctionComp(2412) or
      PHP_FunctionComp(2787) or
      PHP_FunctionComp(2884) or
      PHP_FunctionComp(4025) or
      PHP_FunctionComp(4301) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc266: TtkTokenKind;
begin
  if  PHP_FunctionComp(183) or
      PHP_FunctionComp(399) or
      PHP_FunctionComp(880) or
      PHP_FunctionComp(1089) or
      PHP_FunctionComp(1898) or
      PHP_FunctionComp(3957) or
      PHP_FunctionComp(4946) or
      PHP_FunctionComp(5075) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc267: TtkTokenKind;
begin
  if  PHP_FunctionComp(1191) or
      PHP_FunctionComp(1632) or
      PHP_FunctionComp(2854) or
      PHP_FunctionComp(2892) or
      PHP_FunctionComp(4250) or
      PHP_FunctionComp(5049) or
      PHP_FunctionComp(5087) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc268: TtkTokenKind;
begin
  if  PHP_FunctionComp(1097) or
      PHP_FunctionComp(1418) or
      PHP_FunctionComp(4544) or
      PHP_FunctionComp(5010) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc269: TtkTokenKind;
begin
  if  PHP_FunctionComp(486) or
      PHP_FunctionComp(931) or
      PHP_FunctionComp(945) or
      PHP_FunctionComp(3902) or
      PHP_FunctionComp(4504) or
      PHP_FunctionComp(4506) or
      PHP_FunctionComp(4511) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc270: TtkTokenKind;
begin
  if  PHP_FunctionComp(306) or
      PHP_FunctionComp(648) or
      PHP_FunctionComp(949) or
      PHP_FunctionComp(2872) or
      PHP_FunctionComp(3314) or
      PHP_FunctionComp(4109) or
      PHP_FunctionComp(4410) or
      PHP_FunctionComp(4498) or
      PHP_FunctionComp(4898) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc271: TtkTokenKind;
begin
  if  PHP_FunctionComp(930) or
      PHP_FunctionComp(2387) or
      PHP_FunctionComp(2525) or
      PHP_FunctionComp(2584) or
      PHP_FunctionComp(3053) or
      PHP_FunctionComp(3783) or
      PHP_FunctionComp(4105) or
      PHP_FunctionComp(4705) or
      PHP_FunctionComp(5013) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc272: TtkTokenKind;
begin
  if  PHP_FunctionComp(647) or
      PHP_FunctionComp(940) or
      PHP_FunctionComp(1006) or
      PHP_FunctionComp(1334) or
      PHP_FunctionComp(4365) or
      PHP_FunctionComp(4781) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc273: TtkTokenKind;
begin
  if  PHP_FunctionComp(843) or
      PHP_FunctionComp(1048) or
      PHP_FunctionComp(1679) or
      PHP_FunctionComp(2582) or
      PHP_FunctionComp(2966) or
      PHP_FunctionComp(4009) or
      PHP_FunctionComp(4030) or
      PHP_FunctionComp(4445) or
      PHP_FunctionComp(4560) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc274: TtkTokenKind;
begin
  if  PHP_FunctionComp(302) or
      PHP_FunctionComp(770) or
      PHP_FunctionComp(2371) or
      PHP_FunctionComp(2799) or
      PHP_FunctionComp(4118) or
      PHP_FunctionComp(4546) or
      PHP_FunctionComp(4559) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc275: TtkTokenKind;
begin
  if  PHP_FunctionComp(263) or
      PHP_FunctionComp(1095) or
      PHP_FunctionComp(1593) or
      PHP_FunctionComp(3060) or
      PHP_FunctionComp(4059) or
      PHP_FunctionComp(4508) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc276: TtkTokenKind;
begin
  if  PHP_FunctionComp(595) or
      PHP_FunctionComp(645) or
      PHP_FunctionComp(964) or
      PHP_FunctionComp(1010) or
      PHP_FunctionComp(2004) or
      PHP_FunctionComp(2457) or
      PHP_FunctionComp(2881) or
      PHP_FunctionComp(3683) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc277: TtkTokenKind;
begin
  if  PHP_FunctionComp(173) or
      PHP_FunctionComp(2574) or
      PHP_FunctionComp(3037) or
      PHP_FunctionComp(4026) or
      PHP_FunctionComp(4101) or
      PHP_FunctionComp(4306) or
      PHP_FunctionComp(4483) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc278: TtkTokenKind;
begin
  if  PHP_FunctionComp(1055) or
      PHP_FunctionComp(1057) or
      PHP_FunctionComp(1068) or
      PHP_FunctionComp(3151) or
      PHP_FunctionComp(4494) or
      PHP_FunctionComp(4531) or
      PHP_FunctionComp(4711) or
      PHP_FunctionComp(4788) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc279: TtkTokenKind;
begin
  if  PHP_FunctionComp(884) or
      PHP_FunctionComp(2362) or
      PHP_FunctionComp(4472) or
      PHP_FunctionComp(5058) or
      PHP_FunctionComp(5077) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc280: TtkTokenKind;
begin
  if  PHP_FunctionComp(646) or
      PHP_FunctionComp(2890) or
      PHP_FunctionComp(3345) or
      PHP_FunctionComp(4062) or
      PHP_FunctionComp(4362) or
      PHP_FunctionComp(5012) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc281: TtkTokenKind;
begin
  if  PHP_FunctionComp(170) or
      PHP_FunctionComp(303) or
      PHP_FunctionComp(1432) or
      PHP_FunctionComp(4632) or
      PHP_FunctionComp(4908) or
      PHP_FunctionComp(5081) or
      PHP_FunctionComp(5093) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc282: TtkTokenKind;
begin
  if  PHP_FunctionComp(856) or
      PHP_FunctionComp(896) or
      PHP_FunctionComp(980) or
      PHP_FunctionComp(3348) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc283: TtkTokenKind;
begin
  if  PHP_FunctionComp(876) or
      PHP_FunctionComp(953) or
      PHP_FunctionComp(1064) or
      PHP_FunctionComp(4866) or
      PHP_FunctionComp(4945) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc284: TtkTokenKind;
begin
  if  PHP_FunctionComp(1677) or
      PHP_FunctionComp(2260) or
      PHP_FunctionComp(4236) or
      PHP_FunctionComp(4621) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc285: TtkTokenKind;
begin
  if  PHP_FunctionComp(198) or
      PHP_FunctionComp(909) or
      PHP_FunctionComp(1013) or
      PHP_FunctionComp(2072) or
      PHP_FunctionComp(2531) or
      PHP_FunctionComp(3896) or
      PHP_FunctionComp(4553) or
      PHP_FunctionComp(4796) or
      PHP_FunctionComp(4984) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc286: TtkTokenKind;
begin
  if  PHP_FunctionComp(839) or
      PHP_FunctionComp(842) or
      PHP_FunctionComp(889) or
      PHP_FunctionComp(3355) or
      PHP_FunctionComp(4246) or
      PHP_FunctionComp(5088) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc287: TtkTokenKind;
begin
  if  PHP_FunctionComp(872) or
      PHP_FunctionComp(1457) or
      PHP_FunctionComp(1767) or
      PHP_FunctionComp(4280) or
      PHP_FunctionComp(4281) or
      PHP_FunctionComp(4388) or
      PHP_FunctionComp(5060) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc288: TtkTokenKind;
begin
  if  PHP_FunctionComp(617) or
      PHP_FunctionComp(2130) or
      PHP_FunctionComp(2459) or
      PHP_FunctionComp(3123) or
      PHP_FunctionComp(4233) or
      PHP_FunctionComp(4395) or
      PHP_FunctionComp(4714) or
      PHP_FunctionComp(5044) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc289: TtkTokenKind;
begin
  if  PHP_FunctionComp(988) or
      PHP_FunctionComp(1086) or
      PHP_FunctionComp(2886) or
      PHP_FunctionComp(3358) or
      PHP_FunctionComp(4534) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc290: TtkTokenKind;
begin
  if  PHP_FunctionComp(550) or
      PHP_FunctionComp(831) or
      PHP_FunctionComp(1105) or
      PHP_FunctionComp(2532) or
      PHP_FunctionComp(2885) or
      PHP_FunctionComp(3019) or
      PHP_FunctionComp(5092) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc291: TtkTokenKind;
begin
  if  PHP_FunctionComp(1319) or
      PHP_FunctionComp(2561) or
      PHP_FunctionComp(2874) or
      PHP_FunctionComp(5119) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc292: TtkTokenKind;
begin
  if  PHP_FunctionComp(870) or
      PHP_FunctionComp(1312) or
      PHP_FunctionComp(2833) or
      PHP_FunctionComp(2870) or
      PHP_FunctionComp(3349) or
      PHP_FunctionComp(3981) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc293: TtkTokenKind;
begin
  if  PHP_FunctionComp(835) or
      PHP_FunctionComp(919) or
      PHP_FunctionComp(1714) or
      PHP_FunctionComp(2859) or
      PHP_FunctionComp(4805) or
      PHP_FunctionComp(5046) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc294: TtkTokenKind;
begin
  if  PHP_FunctionComp(1029) or
      PHP_FunctionComp(3959) or
      PHP_FunctionComp(4304) or
      PHP_FunctionComp(5086) or
      PHP_FunctionComp(5107) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc295: TtkTokenKind;
begin
  if  PHP_FunctionComp(176) or
      PHP_FunctionComp(833) or
      PHP_FunctionComp(916) or
      PHP_FunctionComp(917) or
      PHP_FunctionComp(3035) or
      PHP_FunctionComp(4708) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc296: TtkTokenKind;
begin
  if  PHP_FunctionComp(167) or
      PHP_FunctionComp(951) or
      PHP_FunctionComp(2882) or
      PHP_FunctionComp(4507) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc297: TtkTokenKind;
begin
  if  PHP_FunctionComp(4068) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc298: TtkTokenKind;
begin
  if  PHP_FunctionComp(422) or
      PHP_FunctionComp(894) or
      PHP_FunctionComp(1016) or
      PHP_FunctionComp(1599) or
      PHP_FunctionComp(2284) or
      PHP_FunctionComp(4716) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc299: TtkTokenKind;
begin
  if  PHP_FunctionComp(153) or
      PHP_FunctionComp(451) or
      PHP_FunctionComp(883) or
      PHP_FunctionComp(891) or
      PHP_FunctionComp(1246) or
      PHP_FunctionComp(1675) or
      PHP_FunctionComp(2141) or
      PHP_FunctionComp(2865) or
      PHP_FunctionComp(4040) or
      PHP_FunctionComp(4237) or
      PHP_FunctionComp(4718) or
      PHP_FunctionComp(4807) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc300: TtkTokenKind;
begin
  if  PHP_FunctionComp(178) or
      PHP_FunctionComp(877) or
      PHP_FunctionComp(1680) or
      PHP_FunctionComp(5114) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc301: TtkTokenKind;
begin
  if  PHP_FunctionComp(1428) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc302: TtkTokenKind;
begin
  if  PHP_FunctionComp(960) or
      PHP_FunctionComp(2814) or
      PHP_FunctionComp(4786) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc303: TtkTokenKind;
begin
  if  PHP_FunctionComp(1003) or
      PHP_FunctionComp(2265) or
      PHP_FunctionComp(5101) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc304: TtkTokenKind;
begin
  if  PHP_FunctionComp(881) or
      PHP_FunctionComp(2555) or
      PHP_FunctionComp(4242) or
      PHP_FunctionComp(4806) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc305: TtkTokenKind;
begin
  if  PHP_FunctionComp(309) or
      PHP_FunctionComp(1773) or
      PHP_FunctionComp(3333) or
      PHP_FunctionComp(4251) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc306: TtkTokenKind;
begin
  if  PHP_FunctionComp(2864) or
      PHP_FunctionComp(4787) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc307: TtkTokenKind;
begin
  if  PHP_FunctionComp(1323) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc308: TtkTokenKind;
begin
  if  PHP_FunctionComp(2738) or
      PHP_FunctionComp(4060) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc309: TtkTokenKind;
begin
  if  PHP_FunctionComp(1435) or
      PHP_FunctionComp(1770) or
      PHP_FunctionComp(2815) or
      PHP_FunctionComp(4076) or
      PHP_FunctionComp(4456) or
      PHP_FunctionComp(4488) or
      PHP_FunctionComp(4561) or
      PHP_FunctionComp(5229) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc310: TtkTokenKind;
begin
  if  PHP_FunctionComp(963) or
      PHP_FunctionComp(1018) or
      PHP_FunctionComp(2368) or
      PHP_FunctionComp(4533) or
      PHP_FunctionComp(4706) or
      PHP_FunctionComp(4712) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc311: TtkTokenKind;
begin
  if  PHP_FunctionComp(2363) or
      PHP_FunctionComp(4239) or
      PHP_FunctionComp(4241) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc312: TtkTokenKind;
begin
  if  PHP_FunctionComp(886) or
      PHP_FunctionComp(2264) or
      PHP_FunctionComp(2536) or
      PHP_FunctionComp(2619) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc313: TtkTokenKind;
begin
  if  PHP_FunctionComp(2737) or
      PHP_FunctionComp(5007) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc314: TtkTokenKind;
begin
  if  PHP_FunctionComp(154) or
      PHP_FunctionComp(850) or
      PHP_FunctionComp(965) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc315: TtkTokenKind;
begin
  if  PHP_FunctionComp(5102) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc316: TtkTokenKind;
begin
  if  PHP_FunctionComp(943) or
      PHP_FunctionComp(991) or
      PHP_FunctionComp(1030) or
      PHP_FunctionComp(2837) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc317: TtkTokenKind;
begin
  if  PHP_FunctionComp(885) or
      PHP_FunctionComp(3122) or
      PHP_FunctionComp(4532) or
      PHP_FunctionComp(4808) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc318: TtkTokenKind;
begin
  if  PHP_FunctionComp(862) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc319: TtkTokenKind;
begin
  if  PHP_FunctionComp(873) or
      PHP_FunctionComp(5117) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc321: TtkTokenKind;
begin
  if  PHP_FunctionComp(1056) or
      PHP_FunctionComp(4790) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc322: TtkTokenKind;
begin
  if  PHP_FunctionComp(2810) or
      PHP_FunctionComp(3343) or
      PHP_FunctionComp(4785) or
      PHP_FunctionComp(5105) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc323: TtkTokenKind;
begin
  if  PHP_FunctionComp(172) or
      PHP_FunctionComp(1612) or
      PHP_FunctionComp(1721) or
      PHP_FunctionComp(4243) or
      PHP_FunctionComp(4475) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc324: TtkTokenKind;
begin
  if  PHP_FunctionComp(5120) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc325: TtkTokenKind;
begin
  if  PHP_FunctionComp(871) or
      PHP_FunctionComp(1433) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc326: TtkTokenKind;
begin
  if  PHP_FunctionComp(1031) or
      PHP_FunctionComp(1096) or
      PHP_FunctionComp(1434) or
      PHP_FunctionComp(1676) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc327: TtkTokenKind;
begin
  if  PHP_FunctionComp(936) or
      PHP_FunctionComp(2889) or
      PHP_FunctionComp(3998) or
      PHP_FunctionComp(5108) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc328: TtkTokenKind;
begin
  if  PHP_FunctionComp(834) or
      PHP_FunctionComp(2562) or
      PHP_FunctionComp(2904) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc329: TtkTokenKind;
begin
  if  PHP_FunctionComp(961) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc330: TtkTokenKind;
begin
  if  PHP_FunctionComp(1012) or
      PHP_FunctionComp(1631) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc331: TtkTokenKind;
begin
  if  PHP_FunctionComp(851) or
      PHP_FunctionComp(1017) or
      PHP_FunctionComp(5045) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc332: TtkTokenKind;
begin
  if  PHP_FunctionComp(892) or
      PHP_FunctionComp(2578) or
      PHP_FunctionComp(3960) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc333: TtkTokenKind;
begin
  if  PHP_FunctionComp(879) or
      PHP_FunctionComp(5111) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc334: TtkTokenKind;
begin
  if  PHP_FunctionComp(182) or
      PHP_FunctionComp(864) or
      PHP_FunctionComp(1313) or
      PHP_FunctionComp(2577) or
      PHP_FunctionComp(4248) or
      PHP_FunctionComp(5063) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc335: TtkTokenKind;
begin
  if  PHP_FunctionComp(849) or
      PHP_FunctionComp(1633) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc336: TtkTokenKind;
begin
  if  PHP_FunctionComp(1028) or
      PHP_FunctionComp(5098) or
      PHP_FunctionComp(5224) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc337: TtkTokenKind;
begin
  if  PHP_FunctionComp(865) or
      PHP_FunctionComp(882) or
      PHP_FunctionComp(1027) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc338: TtkTokenKind;
begin
  if  PHP_FunctionComp(179) or
      PHP_FunctionComp(878) or
      PHP_FunctionComp(3388) or
      PHP_FunctionComp(4244) or
      PHP_FunctionComp(5118) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc339: TtkTokenKind;
begin
  if  PHP_FunctionComp(933) or
      PHP_FunctionComp(2580) or
      PHP_FunctionComp(2891) or
      PHP_FunctionComp(4492) or
      PHP_FunctionComp(5079) or
      PHP_FunctionComp(5097) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc340: TtkTokenKind;
begin
  if  PHP_FunctionComp(844) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc341: TtkTokenKind;
begin
  if  PHP_FunctionComp(954) or
      PHP_FunctionComp(4247) or
      PHP_FunctionComp(5106) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc343: TtkTokenKind;
begin
  if  PHP_FunctionComp(180) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc344: TtkTokenKind;
begin
  if  PHP_FunctionComp(5095) or
      PHP_FunctionComp(5230) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc345: TtkTokenKind;
begin
  if  PHP_FunctionComp(860) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc346: TtkTokenKind;
begin
  if  PHP_FunctionComp(1635) or
      PHP_FunctionComp(5116) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc347: TtkTokenKind;
begin
  if  PHP_FunctionComp(4041) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc348: TtkTokenKind;
begin
  if  PHP_FunctionComp(166) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc349: TtkTokenKind;
begin
  if  PHP_FunctionComp(5104) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc351: TtkTokenKind;
begin
  if  PHP_FunctionComp(840) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc352: TtkTokenKind;
begin
  if  PHP_FunctionComp(874) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc353: TtkTokenKind;
begin
  if  PHP_FunctionComp(899) or
      PHP_FunctionComp(4249) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc354: TtkTokenKind;
begin
  if  PHP_FunctionComp(2528) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc356: TtkTokenKind;
begin
  if  PHP_FunctionComp(568) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc357: TtkTokenKind;
begin
  if  PHP_FunctionComp(861) or
      PHP_FunctionComp(2360) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc358: TtkTokenKind;
begin
  if  PHP_FunctionComp(837) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc359: TtkTokenKind;
begin
  if  PHP_FunctionComp(181) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc360: TtkTokenKind;
begin
  if  PHP_FunctionComp(4620) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc361: TtkTokenKind;
begin
  if  PHP_FunctionComp(4268) or
      PHP_FunctionComp(5015) or
      PHP_FunctionComp(5073) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc364: TtkTokenKind;
begin
  if  PHP_FunctionComp(2529) or
      PHP_FunctionComp(5019) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc366: TtkTokenKind;
begin
  if  PHP_FunctionComp(5112) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc369: TtkTokenKind;
begin
  if  PHP_FunctionComp(5099) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc370: TtkTokenKind;
begin
  if  PHP_FunctionComp(1634) or
      PHP_FunctionComp(5064) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc373: TtkTokenKind;
begin
  if  PHP_FunctionComp(307) or
      PHP_FunctionComp(845) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc374: TtkTokenKind;
begin
  if  PHP_FunctionComp(2364) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc375: TtkTokenKind;
begin
  if  PHP_FunctionComp(859) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc376: TtkTokenKind;
begin
  if  PHP_FunctionComp(2581) or
      PHP_FunctionComp(5059) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc377: TtkTokenKind;
begin
  if  PHP_FunctionComp(863) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc385: TtkTokenKind;
begin
  if  PHP_FunctionComp(5074) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc387: TtkTokenKind;
begin
  if  PHP_FunctionComp(5160) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc393: TtkTokenKind;
begin
  if  PHP_FunctionComp(4245) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc399: TtkTokenKind;
begin
  if  PHP_FunctionComp(5165) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc402: TtkTokenKind;
begin
  if  PHP_FunctionComp(2560) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc407: TtkTokenKind;
begin
  if  PHP_FunctionComp(1022) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc426: TtkTokenKind;
begin
  if  PHP_FunctionComp(918) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc433: TtkTokenKind;
begin
  if  PHP_FunctionComp(5164) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc439: TtkTokenKind;
begin
  if  PHP_FunctionComp(5166) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc442: TtkTokenKind;
begin
  if  PHP_FunctionComp(937) or
      PHP_FunctionComp(5062) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc445: TtkTokenKind;
begin
  if  PHP_FunctionComp(1015) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc450: TtkTokenKind;
begin
  if  PHP_FunctionComp(2579) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc465: TtkTokenKind;
begin
  if  PHP_FunctionComp(5167) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc466: TtkTokenKind;
begin
  if  PHP_FunctionComp(841) or
      PHP_FunctionComp(5168) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc487: TtkTokenKind;
begin
  if  PHP_FunctionComp(5162) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc491: TtkTokenKind;
begin
  if  PHP_FunctionComp(5161) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc496: TtkTokenKind;
begin
  if  PHP_FunctionComp(5078) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc520: TtkTokenKind;
begin
  if  PHP_FunctionComp(5163) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc541: TtkTokenKind;
begin
  if  PHP_FunctionComp(866) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;

function TSynWebSyn.PhP_KeywordFunc560: TtkTokenKind;
begin
  if  PHP_FunctionComp(867) then
    Result := tkPhpFunction
  else
    Result := tkPhpIdentifier;
end;
