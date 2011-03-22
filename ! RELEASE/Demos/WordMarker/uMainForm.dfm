object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'SynWebWordMarker demo'
  ClientHeight = 343
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    690
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 322
    Width = 387
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Select word to highlight all occurrences . Simpliest way is to d' +
      'ouble-click on word.'
    ExplicitTop = 233
  end
  object syn: TSynEdit
    Left = 8
    Top = 8
    Width = 564
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Lines.UnicodeStrings = 
      '{ TSynWebWordMarker }'#13#10#13#10'procedure TSynWebWordMarker.AfterConstr' +
      'uction;'#13#10'begin'#13#10'  inherited AfterConstruction;'#13#10#13#10'  FEnabled := ' +
      'True;'#13#10'  FBGColor := clYellow;'#13#10'end;'#13#10#13#10'function TSynWebWordMark' +
      'er.IsWordSelected: Boolean;'#13#10#13#10'  function IsSameBuffer(const A, ' +
      'B: TBufferCoord): Boolean;'#13#10'  begin'#13#10'    Result := (A.Line = B.L' +
      'ine) and (A.Char = B.Char);'#13#10'  end;'#13#10#13#10'begin'#13#10'  Result := Editor' +
      '.SelAvail and IsSameBuffer(Editor.BlockBegin, Editor.WordStart) ' +
      'and'#13#10'    IsSameBuffer(Editor.BlockEnd, Editor.WordEnd);'#13#10'end;'#13#10#13 +
      #10'procedure TSynWebWordMarker.SetEnabled(const Value: Boolean);'#13#10 +
      'begin'#13#10'  if Value = FEnabled then'#13#10'    Exit;'#13#10#13#10'  FEnabled := Va' +
      'lue;'#13#10'  Editor.Invalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.' +
      'SetBGColor(const Value: TColor);'#13#10'begin'#13#10'  if FBGColor = Value t' +
      'hen'#13#10'    Exit;'#13#10#13#10'  FBGColor := Value;'#13#10'  if Enabled then'#13#10'    E' +
      'ditor.Invalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.AfterPain' +
      't(ACanvas: TCanvas; const AClip: TRect;'#13#10'  FirstLine, LastLine: ' +
      'Integer);'#13#10'var'#13#10'  lDisplay: TDisplayCoord;'#13#10'  lBuffer: TBufferCo' +
      'ord;      '#13#10'  lSelStartDisplay: TDisplayCoord;'#13#10#13#10'  lLineRow: In' +
      'teger;'#13#10'  lPrevLine: Integer;'#13#10#13#10'  lLineText: UnicodeString;'#13#10'  ' +
      'lSel: UnicodeString;'#13#10'  lXY: TPoint;'#13#10'  lPos: Integer;'#13#10#13#10'  lRec' +
      't: TRect;'#13#10'  lMarginLeft: Integer;'#13#10#13#10'  function IsSameDisplay(c' +
      'onst A, B: TDisplayCoord): Boolean;'#13#10'  begin'#13#10'    Result := (A.R' +
      'ow = B.Row) and (A.Column = B.Column);'#13#10'  end;'#13#10#13#10'begin'#13#10'  if no' +
      't Enabled or not IsWordSelected then'#13#10'    Exit;'#13#10#13#10'  lSel := Edi' +
      'tor.SelText;'#13#10'  if lSel = '#39#39' then'#13#10'    Exit;'#13#10#13#10'  ACanvas.Brush.' +
      'Color := FBGColor;'#13#10'//  ACanvas.Font.Color := FFGColor;  <- not ' +
      'working with TextRect, why?'#13#10#13#10'  lPrevLine := -1;'#13#10'  lLineText :' +
      '= '#39#39';'#13#10'  lSelStartDisplay := Editor.BufferToDisplayPos(Editor.Bl' +
      'ockBegin);'#13#10#13#10'  lDisplay.Column := 1;'#13#10'  lDisplay.Row := FirstLi' +
      'ne;'#13#10#13#10'  if Editor.Gutter.Visible then'#13#10'    lMarginLeft := Edito' +
      'r.Gutter.RealGutterWidth(8) - 2'#13#10'  else'#13#10'    lMarginLeft := 2;'#13#10 +
      #13#10'  for lLineRow := FirstLine to LastLine do'#13#10'  begin'#13#10'    lDisp' +
      'lay.Column := 1;'#13#10'    lDisplay.Row := lLineRow;'#13#10#13#10'    lBuffer :' +
      '= Editor.DisplayToBufferPos(lDisplay);'#13#10'    if lPrevLine = lBuff' +
      'er.Line then'#13#10'      Continue;'#13#10#13#10'    lPrevLine := lBuffer.Line;'#13 +
      #10'    lLineText := Editor.Lines[lPrevLine - 1];'#13#10#13#10'    lPos := Po' +
      's(lSel, lLineText);'#13#10'    while lPos > 0 do'#13#10'    begin'#13#10'      if ' +
      '((lPos = 1) or Editor.IsWordBreakChar(lLineText[lPos - 1])) and'#13 +
      #10'        ((lPos + Length(lSel) = Length(lLineText)) or Editor.Is' +
      'WordBreakChar(lLineText[lPos + Length(lSel)])) then'#13#10'      begin' +
      #13#10'        lBuffer.Char := lPos;'#13#10'        lDisplay := Editor.Buff' +
      'erToDisplayPos(lBuffer);'#13#10'        lXY := Editor.RowColumnToPixel' +
      's(lDisplay);'#13#10#13#10'        if not IsSameDisplay(lSelStartDisplay, l' +
      'Display) then'#13#10'        begin'#13#10'          lRect := Rect(lXY.X, lXY' +
      '.Y,'#13#10'            lXY.X + (Editor.CharWidth * Length(lSel)),'#13#10'   ' +
      '         lXY.Y + Editor.LineHeight);'#13#10#13#10'          if lRect.Left ' +
      '< lMarginLeft then'#13#10'            lRect.Left := lMarginLeft;'#13#10#13#10'  ' +
      '        if IntersectRect(lRect, lRect, AClip) then'#13#10'            ' +
      'ACanvas.TextRect(lRect, lXY.X, lXY.Y, lSel);'#13#10'        end;'#13#10'    ' +
      '  end;'#13#10#13#10'      lPos := PosEx(lSel, lLineText, lPos + 1);'#13#10'    e' +
      'nd;'#13#10'  end;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.LinesInserted(F' +
      'irstLine, Count: Integer);'#13#10'begin'#13#10'  // nothing'#13#10'end;'#13#10#13#10'procedu' +
      're TSynWebWordMarker.LinesDeleted(FirstLine, Count: Integer);'#13#10'b' +
      'egin'#13#10'  // nothing'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.NotifySe' +
      'lChanged;'#13#10'var'#13#10'  lIsWordSelected: Boolean;'#13#10'begin'#13#10'  lIsWordSel' +
      'ected := IsWordSelected;'#13#10#13#10'  if lIsWordSelected = FIsWordSelect' +
      'ed then'#13#10'    Exit;'#13#10#13#10'  FIsWordSelected := lIsWordSelected;'#13#10'  i' +
      'f Enabled then'#13#10'    Editor.Invalidate;'#13#10'end;'
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent]
    WantTabs = True
    OnStatusChange = synStatusChange
  end
  object chkWordMarker: TCheckBox
    Left = 585
    Top = 8
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Word marker'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chkWordMarkerClick
  end
  object chkWordWrap: TCheckBox
    Left = 585
    Top = 31
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Word wrap'
    TabOrder = 2
    OnClick = chkWordWrapClick
  end
  object cbColor: TColorBox
    Left = 578
    Top = 54
    Width = 104
    Height = 22
    Anchors = [akTop, akRight]
    ItemHeight = 16
    TabOrder = 3
    OnChange = cbColorChange
  end
  object chkGutter: TCheckBox
    Left = 585
    Top = 82
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Gutter'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chkGutterClick
  end
end
