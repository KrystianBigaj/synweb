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
      'sSameBuffer(Editor.BlockEnd, Editor.WordEnd);'#13#10'end;'#13#10#13#10'procedure' +
      ' TSynWebWordMarker.SetEnabled(const Value: Boolean);'#13#10'begin'#13#10'  i' +
      'f Value = FEnabled then'#13#10'    Exit;'#13#10#13#10'  FEnabled := Value;'#13#10'  Ed' +
      'itor.Invalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.SetCustomT' +
      'ext(const Value: TSynWebString);'#13#10'begin'#13#10'  if FCustomText = Valu' +
      'e then'#13#10'    Exit;'#13#10#13#10'  FCustomText := Value;'#13#10'  DoInvalidate;'#13#10'e' +
      'nd;'#13#10#13#10'procedure TSynWebWordMarker.SetCaseSensitive(const Value:' +
      ' Boolean);'#13#10'begin'#13#10'  if FCaseSensitive = Value then'#13#10'    Exit;'#13#10 +
      #13#10'  FCaseSensitive := Value;'#13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'procedure' +
      ' TSynWebWordMarker.SetMode(const Value: TSynWebWordMarkerMode);'#13 +
      #10'begin'#13#10'  if FMode = Value then'#13#10'    Exit;'#13#10#13#10'  FMode := Value;'#13 +
      #10'  DoInvalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.SetBGColor' +
      '(const Value: TColor);'#13#10'begin'#13#10'  if FBGColor = Value then'#13#10'    E' +
      'xit;'#13#10#13#10'  FBGColor := Value;'#13#10'  DoInvalidate;'#13#10'end;'#13#10#13#10'procedure' +
      ' TSynWebWordMarker.SetFGColor(const Value: TColor);'#13#10'begin'#13#10'  if' +
      ' FFGColor = Value then'#13#10'    Exit;'#13#10#13#10'  FFGColor := Value;'#13#10'  DoI' +
      'nvalidate;'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.AfterPaint(ACanv' +
      'as: TCanvas; const AClip: TRect;'#13#10'  FirstLine, LastLine: Integer' +
      ');'#13#10'var'#13#10'  lDisplay: TDisplayCoord;'#13#10'  lBuffer: TBufferCoord;   ' +
      '   '#13#10'  lSelStartDisplay: TDisplayCoord;'#13#10#13#10'  lLineRow: Integer;'#13 +
      #10'  lPrevLine: Integer;'#13#10#13#10'  lLineText, lLineTextLower: TSynWebSt' +
      'ring;'#13#10'  lText: TSynWebString;'#13#10'  lXY: TPoint;'#13#10'  lPos: Integer;' +
      #13#10#13#10'  lRect: TRect;'#13#10'  lMarginLeft: Integer;'#13#10#13#10'  function IsSam' +
      'eDisplay(const A, B: TDisplayCoord): Boolean;'#13#10'  begin'#13#10'    Resu' +
      'lt := (A.Row = B.Row) and (A.Column = B.Column);'#13#10'  end;'#13#10#13#10'  fu' +
      'nction DoLowerStr(const AStr: TSynWebString): TSynWebString;'#13#10'  ' +
      'begin'#13#10'    {$IFDEF UNISYNEDIT}'#13#10'    Result := SynUnicode.SynWide' +
      'LowerCase(AStr);'#13#10'    {$ELSE}'#13#10'    Result := LowerCase(AStr);'#13#10' ' +
      '   {$ENDIF}'#13#10'  end;'#13#10#13#10'begin'#13#10'  if not Enabled then'#13#10'    Exit;'#13#10 +
      #13#10'  case FMode of'#13#10'  swwmSelectedWord:'#13#10'    if not IsWordSelecte' +
      'd then'#13#10'      Exit;'#13#10#13#10'  swwmSelectedText:'#13#10'    if not Editor.Se' +
      'lAvail or (Editor.BlockBegin.Line <> Editor.BlockEnd.Line) then'#13 +
      #10'      Exit;'#13#10#13#10'  swwmCustomWord, swwmCustomText:'#13#10'    if FCusto' +
      'mText = '#39#39' then'#13#10'      Exit;'#13#10'  end;'#13#10#13#10'  lText := GetHighlightT' +
      'ext;'#13#10'  if lText = '#39#39' then'#13#10'    Exit;'#13#10#13#10'  if not FCaseSensitive' +
      ' then'#13#10'    lText := DoLowerStr(lText);'#13#10#13#10'  ACanvas.Brush.Color ' +
      ':= FBGColor;'#13#10#13#10'  lPrevLine := -1;'#13#10'  lLineText := '#39#39';'#13#10'  lLineT' +
      'extLower := '#39#39';'#13#10'  lSelStartDisplay := Editor.BufferToDisplayPos' +
      '(Editor.BlockBegin);'#13#10#13#10'  lDisplay.Column := 1;'#13#10'  lDisplay.Row ' +
      ':= FirstLine;'#13#10#13#10'  if Editor.Gutter.Visible then'#13#10'    lMarginLef' +
      't := Editor.Gutter.RealGutterWidth(8) - 2'#13#10'  else'#13#10'    lMarginLe' +
      'ft := 2;'#13#10#13#10'  for lLineRow := FirstLine to LastLine do'#13#10'  begin'#13 +
      #10'    lDisplay.Column := 1;'#13#10'    lDisplay.Row := lLineRow;'#13#10#13#10'   ' +
      ' lBuffer := Editor.DisplayToBufferPos(lDisplay);'#13#10'    if lPrevLi' +
      'ne = lBuffer.Line then'#13#10'      Continue;'#13#10#13#10'    lPrevLine := lBuf' +
      'fer.Line;'#13#10'    lLineText := Editor.Lines[lPrevLine - 1];'#13#10'    if' +
      ' not FCaseSensitive then'#13#10'      lLineTextLower := DoLowerStr(lLi' +
      'neText);'#13#10#13#10'    if FCaseSensitive then'#13#10'      lPos := Pos(lText,' +
      ' lLineText)'#13#10'    else'#13#10'      lPos := Pos(lText, lLineTextLower);' +
      #13#10'    while lPos > 0 do'#13#10'    begin'#13#10'      if not (FMode in [swwm' +
      'SelectedWord, swwmCustomWord]) or ('#13#10'        ((lPos = 1) or Edit' +
      'or.IsWordBreakChar(lLineText[lPos - 1])) and'#13#10'        ((lPos - 1' +
      ' + Length(lText) = Length(lLineText)) or Editor.IsWordBreakChar(' +
      'lLineText[lPos + Length(lText)]))'#13#10'      ) then'#13#10'      begin'#13#10'  ' +
      '      lBuffer.Char := lPos;'#13#10'        lDisplay := Editor.BufferTo' +
      'DisplayPos(lBuffer);'#13#10'        lXY := Editor.RowColumnToPixels(lD' +
      'isplay);'#13#10'        if not Editor.SelAvail or not IsSameDisplay(lS' +
      'elStartDisplay, lDisplay) then'#13#10'        begin'#13#10'          lRect :' +
      '= Rect(lXY.X, lXY.Y,'#13#10'            lXY.X + (Editor.CharWidth * Le' +
      'ngth(lText)),'#13#10'            lXY.Y + Editor.LineHeight);'#13#10#13#10'      ' +
      '    if lRect.Left < lMarginLeft then'#13#10'            lRect.Left := ' +
      'lMarginLeft;'#13#10#13#10'          if IntersectRect(lRect, lRect, AClip) ' +
      'then'#13#10'          begin'#13#10'            ACanvas.Font.Color := FFGColo' +
      'r;  // DisplayToBufferPos overwrites Canvas.Font, so it must be ' +
      'set here'#13#10'            if FCaseSensitive then'#13#10'              ACan' +
      'vas.TextRect(lRect, lXY.X, lXY.Y, lText)'#13#10'            else'#13#10'    ' +
      '          ACanvas.TextRect(lRect, lXY.X, lXY.Y, Copy(lLineText, ' +
      'lPos, Length(lText)));'#13#10'          end;'#13#10'        end;'#13#10'      end;' +
      #13#10'                   '#13#10'      if FCaseSensitive then'#13#10'        lPo' +
      's := PosEx(lText, lLineText, lPos + 1)'#13#10'      else'#13#10'        lPos' +
      ' := PosEx(lText, lLineTextLower, lPos + 1);'#13#10'    end;'#13#10'  end;'#13#10'e' +
      'nd;'#13#10#13#10'procedure TSynWebWordMarker.DoInvalidate;'#13#10'begin'#13#10'  if En' +
      'abled then'#13#10'    Editor.Invalidate;'#13#10'end;'#13#10#13#10'function TSynWebWord' +
      'Marker.GetHighlightText: TSynWebString;'#13#10'begin'#13#10'  case FMode of'#13 +
      #10'  swwmSelectedWord, swwmSelectedText:'#13#10'    Result := Editor.Sel' +
      'Text;'#13#10#13#10'  swwmCustomWord, swwmCustomText:'#13#10'    Result := FCusto' +
      'mText;'#13#10'  else'#13#10'    Result := '#39#39';'#13#10'  end;'#13#10'end;'#13#10#13#10'procedure TSy' +
      'nWebWordMarker.LinesInserted(FirstLine, Count: Integer);'#13#10'begin'#13 +
      #10'  // nothing'#13#10'end;'#13#10#13#10'procedure TSynWebWordMarker.LinesDeleted(' +
      'FirstLine, Count: Integer);'#13#10'begin'#13#10'  // nothing'#13#10'end;'#13#10#13#10'proced' +
      'ure TSynWebWordMarker.NotifySelChanged;'#13#10'var'#13#10'  lIsWordSelected:' +
      ' Boolean;'#13#10'  lIsMultiLineSelection: Boolean;'#13#10'begin'#13#10'  lIsMultiL' +
      'ineSelection := Editor.BlockBegin.Line <> Editor.BlockEnd.Line;'#13 +
      #10#13#10'  if not lIsMultiLineSelection or not FIsMultiLineSelection t' +
      'hen'#13#10'    case FMode of'#13#10'    swwmSelectedWord:'#13#10'      begin'#13#10'    ' +
      '    lIsWordSelected := IsWordSelected;'#13#10#13#10'        if lIsWordSele' +
      'cted = FIsWordSelected then'#13#10'          Exit;'#13#10#13#10'        FIsWordS' +
      'elected := lIsWordSelected;'#13#10'        DoInvalidate;'#13#10'      end;'#13#10 +
      #13#10'    swwmSelectedText:'#13#10'      DoInvalidate;'#13#10'    end;'#13#10#13#10'  FIsM' +
      'ultiLineSelection := lIsMultiLineSelection;'#13#10'end;'
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
    Height = 185
    Anchors = [akTop, akRight]
    TabOrder = 3
    object lblCustomText: TLabel
      Left = 22
      Top = 126
      Width = 59
      Height = 13
      Caption = 'Custom text'
    end
    object edtCustomText: TEdit
      Left = 17
      Top = 145
      Width = 104
      Height = 21
      TabOrder = 0
      Text = 'Syn'
      OnChange = edtCustomTextChange
    end
    object cbBGColor: TColorBox
      Left = 17
      Top = 47
      Width = 104
      Height = 22
      ItemHeight = 16
      TabOrder = 1
      OnChange = cbBGColorChange
    end
    object chkWordMarker: TCheckBox
      Left = 19
      Top = 24
      Width = 97
      Height = 17
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
      ItemHeight = 16
      TabOrder = 3
      OnChange = cbFGColorChange
    end
    object chkCaseSensitive: TCheckBox
      Left = 19
      Top = 103
      Width = 97
      Height = 17
      Caption = 'Case sensitive'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkCaseSensitiveClick
    end
  end
  object cbMode: TComboBox
    Left = 652
    Top = 32
    Width = 109
    Height = 21
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
