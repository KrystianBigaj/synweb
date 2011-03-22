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
      'ord;      '#13#10'  lSelStartDisp: TDisplayCoord;'#13#10#13#10'  lLineRow: Integ' +
      'er;'#13#10'  lPrevLine: Integer;'#13#10#13#10'  lLineText, lLineTextWrap: Unicod' +
      'eString;'#13#10'  lSel: UnicodeString;'#13#10'  lX, lY: Integer;'#13#10'  lPos: In' +
      'teger;'#13#10#13#10'  lRect: TRect;'#13#10'begin'#13#10'  if not Enabled or not IsWord' +
      'Selected then'#13#10'    Exit;'#13#10#13#10'  lSel := Editor.SelText;'#13#10'  if lSel' +
      ' = '#39#39' then'#13#10'    Exit;'#13#10#13#10'  ACanvas.Brush.Color := FBGColor;'#13#10'// ' +
      ' ACanvas.Font.Color := FFGColor;  <- not working with TextRect, ' +
      'why?'#13#10#13#10'  lPrevLine := -1;'#13#10'  lLineText := '#39#39';'#13#10'  lSelStartDisp ' +
      ':= Editor.BufferToDisplayPos(Editor.BlockBegin);'#13#10'  for lLineRow' +
      ' := FirstLine to LastLine do'#13#10'  begin'#13#10'    lDisplay.Column := 1;' +
      #13#10'    lDisplay.Row := lLineRow;'#13#10#13#10'    lBuffer := Editor.Display' +
      'ToBufferPos(lDisplay);'#13#10'    if lPrevLine <> lBuffer.Line then'#13#10' ' +
      '     lLineText := Editor.Lines[lBuffer.Line - 1];'#13#10#13#10'    if lBuf' +
      'fer.Char = 1 then'#13#10'      lLineTextWrap := lLineText'#13#10'    else   ' +
      '                     '#13#10'      lLineTextWrap := Copy(lLineText, lB' +
      'uffer.Char, MaxInt);'#13#10#13#10'    lPos := Pos(lSel, lLineTextWrap);'#13#10' ' +
      '   while lPos > 0 do'#13#10'    begin'#13#10'      if ((lPos = 1) or Editor.' +
      'IsWordBreakChar(lLineTextWrap[lPos - 1])) and'#13#10'        ((lPos + ' +
      'Length(lSel) = Length(lLineTextWrap)) or Editor.IsWordBreakChar(' +
      'lLineTextWrap[lPos + Length(lSel)])) then'#13#10'      begin'#13#10'        ' +
      'lX := Editor.Gutter.Width + Editor.Gutter.RightOffset +'#13#10'       ' +
      '   (Editor.CharWidth * (lPos - Editor.LeftChar));'#13#10#13#10'        if ' +
      'lX > AClip.Right then'#13#10'          Break;'#13#10#13#10'        lY := (lLineR' +
      'ow - Editor.TopLine) * Editor.LineHeight;'#13#10#13#10'        if (lSelSta' +
      'rtDisp.Row <> lLineRow) or (lSelStartDisp.Column <> lPos) then'#13#10 +
      '        begin'#13#10'          lRect := Rect(lX, lY,'#13#10'            lX +' +
      ' (Editor.CharWidth * Length(lSel)),'#13#10'            lY + Editor.Lin' +
      'eHeight);'#13#10#13#10'          if not Editor.WordWrap or (lRect.Right <=' +
      ' Editor.ClientRect.Right) then'#13#10'            if IntersectRect(lRe' +
      'ct, lRect, AClip) then'#13#10'              ACanvas.TextRect(lRect, lX' +
      ', lY, lSel);'#13#10'        end;'#13#10'      end;'#13#10#13#10'      lPos := PosEx(lS' +
      'el, lLineTextWrap, lPos + 1);'#13#10'    end;'#13#10'  end;'#13#10'end;'#13#10#13#10'procedu' +
      're TSynWebWordMarker.LinesInserted(FirstLine, Count: Integer);'#13#10 +
      'begin'#13#10'  // nothing'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.LinesDe' +
      'leted(FirstLine, Count: Integer);'#13#10'begin'#13#10'  // nothing'#13#10'end;'#13#10#13#10 +
      'procedure TSynWebWordMarker.NotifySelChanged;'#13#10'var'#13#10'  lIsWordSel' +
      'ected: Boolean;'#13#10'begin'#13#10'  lIsWordSelected := IsWordSelected;'#13#10#13#10 +
      '  if lIsWordSelected = FIsWordSelected then'#13#10'    Exit;'#13#10#13#10'  FIsW' +
      'ordSelected := lIsWordSelected;'#13#10'  if Enabled then'#13#10'    Editor.I' +
      'nvalidate;'#13#10'end;'
    OnStatusChange = synStatusChange
    ExplicitWidth = 409
    ExplicitHeight = 219
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
    ExplicitLeft = 430
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
    ExplicitLeft = 430
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
