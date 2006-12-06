{-------------------------------------------------------------------------------
SynWeb
Copyright (C) 2006  Krystian Bigaj

*** MPL
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
***

*** LGPL
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
***

You may retrieve the latest version of this file at the SynEdit home page,
located at http://sourceforge.net/projects/synweb

Contact: krystian.bigaj@gmail.com
Homepage: http://flatdev.ovh.org      
-------------------------------------------------------------------------------}

{$IFNDEF QSYNWEBREG}
unit SynWebReg;
{$ENDIF}

{$I SynWeb.inc}

interface

uses
{$IFDEF SYN_CLX}
  // SynWeb components
  QSynHighlighterWeb,
  QSynHighlighterWebData,
  QSynHighlighterWebMisc,
  QSynTokenMatch,
  QSynEditStrConst,
{$ELSE}
  // SynWeb components   
  SynHighlighterWeb,
  SynHighlighterWebData,
  SynHighlighterWebMisc,
  SynTokenMatch,
  SynEditStrConst,
{$ENDIF}
  Classes;

procedure Register;

implementation

procedure Register;
begin
// SynWeb highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynWebEngine,
    TSynWebHtmlSyn,
    TSynWebWmlSyn,
    TSynWebCssSyn,
    TSynWebEsSyn,
    TSynWebPhpCliSyn
  ]);
end;

end.
