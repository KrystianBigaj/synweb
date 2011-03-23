object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'SynWebWordMarker demo'
  ClientHeight = 510
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    778
    510)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 489
    Width = 387
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Select word to highlight all occurrences . Simpliest way is to d' +
      'ouble-click on word.'
    ExplicitTop = 233
  end
  object Label3: TLabel
    Left = 638
    Top = 8
    Width = 132
    Height = 13
    Alignment = taCenter
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Markers config'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object syn: TSynEdit
    Left = 8
    Top = 8
    Width = 626
    Height = 475
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
      'uction;'#13#10'begin'#13#10'  inherited AfterConstruction;'#13#10#13#10'  FMode := sww' +
      'mSelectedWord;'#13#10'  FEnabled := True;'#13#10'  FBGColor := clYellow;'#13#10'  ' +
      'FFGColor := clBlack;'#13#10'end;'#13#10#13#10'function TSynWebWordMarker.IsWordS' +
      'elected: Boolean;'#13#10#13#10'  function IsSameBuffer(const A, B: TBuffer' +
      'Coord): Boolean;'#13#10'  begin'#13#10'    Result := (A.Line = B.Line) and (' +
      'A.Char = B.Char);'#13#10'  end;'#13#10#13#10'begin'#13#10'  Result := Editor.SelAvail ' +
      'and IsSameBuffer(Editor.BlockBegin, Editor.WordStart) and'#13#10'    I' +
      'sSameBuffer(Editor.BlockEnd, Editor.WordEnd);'#13#10'end;'#13#10#13#10'function ' +
      'TSynWebWordMarker.GetHighlightText: TSynWebString;'#13#10'begin'#13#10'  cas' +
      'e FMode of'#13#10'  swwmSelectedWord, swwmSelectedText:'#13#10'    Result :=' +
      ' Editor.SelText;'#13#10#13#10'  swwmCustomWord, swwmCustomText:'#13#10'    Resul' +
      't := FCustomText;'#13#10'  else'#13#10'    Result := '#39#39';'#13#10'  end;'#13#10'end;'#13#10#13#10'pr' +
      'ocedure TSynWebWordMarker.SetEnabled(const Value: Boolean);'#13#10'beg' +
      'in'#13#10'  if Value = FEnabled then'#13#10'    Exit;'#13#10#13#10'  FEnabled := Value' +
      ';'#13#10'  Editor.Invalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.Set' +
      'BGColor(const Value: TColor);'#13#10'begin'#13#10'  if FBGColor = Value then' +
      #13#10'    Exit;'#13#10#13#10'  FBGColor := Value;'#13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'pr' +
      'ocedure TSynWebWordMarker.SetFGColor(const Value: TColor);'#13#10'begi' +
      'n'#13#10'  if FFGColor = Value then'#13#10'    Exit;'#13#10#13#10'  FFGColor := Value;' +
      #13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.SetMode(c' +
      'onst Value: TSynWebWordMarkerMode);'#13#10'begin'#13#10'  if FMode = Value t' +
      'hen'#13#10'    Exit;'#13#10#13#10'  FMode := Value;'#13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'pr' +
      'ocedure TSynWebWordMarker.SetPaintMode(const Value: TSynWebWordM' +
      'arkerPaintMode);'#13#10'begin'#13#10'  if FPaintMode = Value then'#13#10'    Exit;' +
      #13#10#13#10'  FPaintMode := Value;'#13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'procedure T' +
      'SynWebWordMarker.SetCustomText(const Value: TSynWebString);'#13#10'beg' +
      'in'#13#10'  if FCustomText = Value then'#13#10'    Exit;'#13#10#13#10'  FCustomText :=' +
      ' Value;'#13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.Se' +
      'tCaseSensitive(const Value: Boolean);'#13#10'begin'#13#10'  if FCaseSensitiv' +
      'e = Value then'#13#10'    Exit;'#13#10#13#10'  FCaseSensitive := Value;'#13#10'  DoInv' +
      'alidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.DoInvalidate;'#13#10'beg' +
      'in'#13#10'  if Enabled then'#13#10'    Editor.Invalidate;'#13#10'end;'#13#10#13#10'procedure' +
      ' TSynWebWordMarker.LinesInserted(FirstLine, Count: Integer);'#13#10'be' +
      'gin'#13#10'  // nothing'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.LinesDele' +
      'ted(FirstLine, Count: Integer);'#13#10'begin'#13#10'  // nothing'#13#10'end;'#13#10#13#10'pr' +
      'ocedure TSynWebWordMarker.AfterPaint(ACanvas: TCanvas; const ACl' +
      'ip: TRect;'#13#10'  FirstLine, LastLine: Integer);'#13#10'var'#13#10'  lDisplay: T' +
      'DisplayCoord;'#13#10'  lBuffer: TBufferCoord;      '#13#10#13#10'  lLineRow: Int' +
      'eger;'#13#10'  lPrevLine: Integer;'#13#10#13#10'  lLineText, lLineTextLower: TSy' +
      'nWebString;'#13#10'  lText: TSynWebString;'#13#10'  lPos: Integer;'#13#10#13#10'  lRec' +
      't: TRect;'#13#10'  lRectClip: TRect;'#13#10'  lMarginLeft: Integer;'#13#10#13#10'  fun' +
      'ction IsSameDisplay(const A, B: TDisplayCoord): Boolean;'#13#10'  begi' +
      'n'#13#10'    Result := (A.Row = B.Row) and (A.Column = B.Column);'#13#10'  e' +
      'nd;'#13#10#13#10'  function DoLowerStr(const AStr: TSynWebString): TSynWeb' +
      'String;'#13#10'  begin'#13#10'    {$IFDEF UNISYNEDIT}'#13#10'    Result := SynUnic' +
      'ode.SynWideLowerCase(AStr);'#13#10'    {$ELSE}'#13#10'    Result := LowerCas' +
      'e(AStr);'#13#10'    {$ENDIF}'#13#10'  end;'#13#10#13#10'  function IsSelOverlap: Boole' +
      'an;'#13#10'  var'#13#10'    lBegin, lEnd: TBufferCoord;'#13#10'    lTextLen: Integ' +
      'er;'#13#10'  begin'#13#10'    Result := False;'#13#10'    if not Editor.SelAvail t' +
      'hen'#13#10'      Exit;'#13#10'      '#13#10'    Result := True;'#13#10'    if Editor.IsP' +
      'ointInSelection(lBuffer) then'#13#10'      Exit;'#13#10#13#10'    lBegin := Edit' +
      'or.BlockBegin;'#13#10'    lEnd := Editor.BlockEnd;'#13#10#13#10'    lTextLen := ' +
      'Length(lText);'#13#10#13#10'    if (lBegin.Line = lEnd.Line) and (lBegin.L' +
      'ine = lBuffer.Line) and '#13#10'      (lBegin.Char >= lBuffer.Char) an' +
      'd (lEnd.Char <= lBuffer.Char + lTextLen)'#13#10'    then'#13#10'      Exit;'#13 +
      #10#13#10'    Inc(lBuffer.Char, lTextLen);'#13#10'    if Editor.IsPointInSele' +
      'ction(lBuffer) then'#13#10'      Exit;'#13#10#13#10'    Result := False;'#13#10'  end;' +
      #13#10#13#10'  function DoGetPaintMode: TSynWebWordMarkerPaintMode;'#13#10'  be' +
      'gin'#13#10'    case FPaintMode of'#13#10'    swwpFillRect:'#13#10'      if IsSelOv' +
      'erlap then'#13#10'        Result := swwpFrameRect'#13#10'      else'#13#10'       ' +
      ' Result := swwpFillRect;'#13#10'    else'#13#10'      Result := FPaintMode;'#13 +
      #10'    end;'#13#10'  end;'#13#10#13#10'  function DoIntersectRect(var ARect: TRect' +
      '; const AClip: TRect): Boolean;'#13#10'  var'#13#10'    lClipH: HRGN;'#13#10'  beg' +
      'in'#13#10'    Result := IntersectRect(ARect, ARect, AClip);'#13#10'    if no' +
      't Result then'#13#10'      Exit;'#13#10#13#10'    lClipH := CreateRectRgn(ARect.' +
      'Left, ARect.Top, ARect.Right, ARect.Bottom);'#13#10'    if lClipH <> 0' +
      ' then'#13#10'    begin'#13#10'      SelectClipRgn(ACanvas.Handle, lClipH);'#13#10 +
      '      DeleteObject(lClipH);'#13#10'    end;'#13#10'  end;'#13#10#13#10'begin'#13#10'  if not' +
      ' Enabled then'#13#10'    Exit;'#13#10#13#10'  case FMode of'#13#10'  swwmSelectedWord:' +
      #13#10'    if not IsWordSelected then'#13#10'      Exit;'#13#10#13#10'  swwmSelectedT' +
      'ext:'#13#10'    if not Editor.SelAvail or (Editor.BlockBegin.Line <> E' +
      'ditor.BlockEnd.Line) then'#13#10'      Exit;'#13#10#13#10'  swwmCustomWord, swwm' +
      'CustomText:'#13#10'    if FCustomText = '#39#39' then'#13#10'      Exit;'#13#10'  end;'#13#10 +
      #13#10'  lText := GetHighlightText;'#13#10'  if lText = '#39#39' then'#13#10'    Exit;'#13 +
      #10#13#10'  if not FCaseSensitive then'#13#10'    lText := DoLowerStr(lText);' +
      #13#10#13#10'  ACanvas.Brush.Color := FBGColor;'#13#10#13#10'  lPrevLine := -1;'#13#10'  ' +
      'lLineText := '#39#39';'#13#10'  lLineTextLower := '#39#39';'#13#10#13#10'  lDisplay.Column :' +
      '= 1;'#13#10'  lDisplay.Row := FirstLine;'#13#10#13#10'  if Editor.Gutter.Visible' +
      ' then'#13#10'    lMarginLeft := Editor.Gutter.RealGutterWidth(8) - 2'#13#10 +
      '  else'#13#10'    lMarginLeft := 2;'#13#10#13#10'  for lLineRow := FirstLine to ' +
      'LastLine do'#13#10'  begin'#13#10'    lDisplay.Column := 1;'#13#10'    lDisplay.Ro' +
      'w := lLineRow;'#13#10#13#10'    lBuffer := Editor.DisplayToBufferPos(lDisp' +
      'lay);'#13#10'    if lPrevLine = lBuffer.Line then'#13#10'      Continue;'#13#10#13#10 +
      '    lPrevLine := lBuffer.Line;'#13#10'    lLineText := Editor.Lines[lP' +
      'revLine - 1];'#13#10'    if not FCaseSensitive then'#13#10'      lLineTextLo' +
      'wer := DoLowerStr(lLineText);'#13#10#13#10'    if FCaseSensitive then'#13#10'   ' +
      '   lPos := Pos(lText, lLineText)'#13#10'    else'#13#10'      lPos := Pos(lT' +
      'ext, lLineTextLower);'#13#10'    while lPos > 0 do'#13#10'    begin'#13#10'      i' +
      'f not (FMode in [swwmSelectedWord, swwmCustomWord]) or ('#13#10'      ' +
      '  ((lPos = 1) or Editor.IsWordBreakChar(lLineText[lPos - 1])) an' +
      'd'#13#10'        ((lPos - 1 + Length(lText) = Length(lLineText)) or Ed' +
      'itor.IsWordBreakChar(lLineText[lPos + Length(lText)]))'#13#10'      ) ' +
      'then'#13#10'      begin'#13#10'        lBuffer.Char := lPos;'#13#10'        lDispl' +
      'ay := Editor.BufferToDisplayPos(lBuffer);'#13#10'        '#13#10'        lRe' +
      'ct.TopLeft := Editor.RowColumnToPixels(lDisplay);'#13#10'        lRect' +
      '.Right := lRect.Left + (Editor.CharWidth * Length(lText));'#13#10'    ' +
      '    lRect.Bottom := lRect.Top + Editor.LineHeight;'#13#10#13#10'        lR' +
      'ectClip := lRect;'#13#10#13#10'        if lRectClip.Left < lMarginLeft the' +
      'n'#13#10'          lRectClip.Left := lMarginLeft;'#13#10#13#10'        if DoInte' +
      'rsectRect(lRectClip, AClip) then'#13#10'          case DoGetPaintMode ' +
      'of'#13#10'          swwpFillRect:'#13#10'            begin'#13#10'              AC' +
      'anvas.Font.Color := FFGColor;  // DisplayToBufferPos overwrites ' +
      'Canvas.Font, so it must be set here'#13#10'              if FCaseSensi' +
      'tive then'#13#10'                ACanvas.TextRect(lRectClip, lRect.Lef' +
      't, lRect.Top, lText)'#13#10'              else'#13#10'                ACanva' +
      's.TextRect(lRectClip, lRect.Left, lRect.Top, Copy(lLineText, lPo' +
      's, Length(lText)));'#13#10'            end;'#13#10#13#10'          swwpFrameRect' +
      ':                                      '#13#10'            ACanvas.Fra' +
      'meRect(lRect);'#13#10#13#10'          swwpUnderline:'#13#10'            begin'#13#10' ' +
      '             lRect.Top := lRect.Bottom - 2;'#13#10'              ACanv' +
      'as.FillRect(lRect);'#13#10'            end;'#13#10'          end;'#13#10'      end' +
      ';'#13#10#13#10'      if FCaseSensitive then'#13#10'        lPos := PosEx(lText, ' +
      'lLineText, lPos + 1)'#13#10'      else'#13#10'        lPos := PosEx(lText, l' +
      'LineTextLower, lPos + 1);'#13#10'    end;'#13#10'  end;     '#13#10#13#10'  // Clear c' +
      'lip region for canvas'#13#10'  SelectClipRgn(ACanvas.Handle, 0);'#13#10'end;' +
      #13#10#13#10'procedure TSynWebWordMarker.NotifySelChanged;'#13#10'var'#13#10'  lIsWor' +
      'dSelected: Boolean;'#13#10'  lIsMultiLineSelection: Boolean;'#13#10'begin'#13#10' ' +
      ' lIsMultiLineSelection := Editor.BlockBegin.Line <> Editor.Block' +
      'End.Line;'#13#10#13#10'  if not lIsMultiLineSelection or not FIsMultiLineS' +
      'election then'#13#10'    case FMode of'#13#10'    swwmSelectedWord:'#13#10'      b' +
      'egin'#13#10'        lIsWordSelected := IsWordSelected;'#13#10#13#10'        if l' +
      'IsWordSelected = FIsWordSelected then'#13#10'          Exit;'#13#10#13#10'      ' +
      '  FIsWordSelected := lIsWordSelected;'#13#10'        DoInvalidate;'#13#10'  ' +
      '    end;'#13#10#13#10'    swwmSelectedText:'#13#10'      DoInvalidate;'#13#10'    end;' +
      #13#10#13#10'  FIsMultiLineSelection := lIsMultiLineSelection;'#13#10'end;'
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent]
    WantTabs = True
    OnStatusChange = synStatusChange
  end
  object chkWordWrap: TCheckBox
    Left = 640
    Top = 466
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Word wrap'
    TabOrder = 1
    OnClick = chkWordWrapClick
  end
  object chkGutter: TCheckBox
    Left = 640
    Top = 443
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Gutter'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chkGutterClick
  end
  object gbConfig: TGroupBox
    Left = 640
    Top = 40
    Width = 137
    Height = 209
    Anchors = [akTop, akRight]
    TabOrder = 3
    DesignSize = (
      137
      209)
    object lblCustomText: TLabel
      Left = 22
      Top = 153
      Width = 59
      Height = 13
      Caption = 'Custom text'
    end
    object edtCustomText: TEdit
      Left = 17
      Top = 172
      Width = 104
      Height = 21
      TabOrder = 0
      OnChange = edtCustomTextChange
    end
    object cbBGColor: TColorBox
      Left = 17
      Top = 47
      Width = 104
      Height = 22
      Hint = 'Background color'
      ItemHeight = 16
      TabOrder = 1
      OnChange = cbBGColorChange
    end
    object chkWordMarker: TCheckBox
      Left = 19
      Top = 24
      Width = 97
      Height = 17
      Hint = 'Marker painting enabled'
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkWordMarkerClick
    end
    object cbFGColor: TColorBox
      Left = 17
      Top = 75
      Width = 104
      Height = 22
      Hint = 'Foreground color'
      ItemHeight = 16
      TabOrder = 3
      OnChange = cbFGColorChange
    end
    object chkCaseSensitive: TCheckBox
      Left = 19
      Top = 130
      Width = 97
      Height = 17
      Caption = 'Case sensitive'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkCaseSensitiveClick
    end
    object cbPaintMode: TComboBox
      Left = 17
      Top = 103
      Width = 104
      Height = 21
      Hint = 'Marker paint mode'
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemHeight = 13
      TabOrder = 5
      OnChange = cbPaintModeChange
      Items.Strings = (
        'FillRect'
        'FrameRect'
        'Underline')
    end
  end
  object cbMode: TComboBox
    Left = 652
    Top = 32
    Width = 109
    Height = 21
    Hint = 'Marker type'
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Selected text'
    OnChange = cbModeChange
    Items.Strings = (
      'Selected text'
      'Selected word'
      'Custom text'
      'Custom word')
  end
end
