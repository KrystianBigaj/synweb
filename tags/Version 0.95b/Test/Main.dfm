object Form1: TForm1
  Left = 253
  Top = 126
  Width = 770
  Height = 610
  Caption = 'TSynWeb (Multi: HTML+CSS+ES+PHP) - Demo v0.99b'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 57
    Width = 762
    Height = 507
    Align = alClient
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
    Highlighter = SynWebSyn1
    Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    TabWidth = 2
    WantTabs = True
    WordWrapGlyph.Visible = False
    OnDropFiles = SynEdit1DropFiles
    OnStatusChange = SynEdit1StatusChange
    OnPaintTransient = SynEdit1PaintTransient
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 564
    Width = 762
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Drop file on SynEdit :)'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 57
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 2
    DesignSize = (
      762
      57)
    object Label1: TLabel
      Left = 4
      Top = 24
      Width = 68
      Height = 13
      Caption = 'HTMLVersion:'
    end
    object Label2: TLabel
      Left = 224
      Top = 24
      Width = 56
      Height = 13
      Caption = 'CSSVersion'
    end
    object Label3: TLabel
      Left = 372
      Top = 24
      Width = 57
      Height = 13
      Caption = 'PHPVersion'
    end
    object Label4: TLabel
      Left = 640
      Top = 4
      Width = 85
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Highlighter MODE'
    end
    object Label5: TLabel
      Left = 143
      Top = 3
      Width = 5
      Height = 13
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 1
      Top = 43
      Width = 760
      Height = 13
      Align = alBottom
      Caption = 'Label6'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CheckBox1: TCheckBox
      Left = 4
      Top = 2
      Width = 62
      Height = 17
      Caption = '&SynWeb'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object ComboBox1: TComboBox
      Left = 72
      Top = 20
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComboBox1Change
    end
    object ComboBox2: TComboBox
      Left = 292
      Top = 20
      Width = 77
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = ComboBox2Change
    end
    object Button1: TButton
      Left = 208
      Top = 2
      Width = 81
      Height = 17
      Caption = 'C:\demo.html'
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 736
      Top = 2
      Width = 25
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'CFG'
      TabOrder = 4
      OnClick = Button2Click
    end
    object CheckBox2: TCheckBox
      Left = 72
      Top = 5
      Width = 65
      Height = 13
      Caption = 'Active &HL'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBox2Click
    end
    object ComboBox3: TComboBox
      Left = 440
      Top = 20
      Width = 77
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      OnChange = ComboBox3Change
    end
    object ComboBox4: TComboBox
      Left = 596
      Top = 20
      Width = 165
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemHeight = 13
      TabOrder = 7
      OnChange = ComboBox4Change
      Items.Strings = (
        'HTML (+CSS, +ES)'
        'CSS'
        'JS'
        'PHP (+HTML, +CSS, +ES)'
        'PHP (CGI)')
    end
    object CheckBox3: TCheckBox
      Left = 296
      Top = 2
      Width = 121
      Height = 17
      Caption = 'PHP: &Asp open tag?'
      TabOrder = 8
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 428
      Top = 2
      Width = 125
      Height = 17
      Caption = 'PHP: &Short open tag?'
      TabOrder = 9
      OnClick = CheckBox4Click
    end
    object Edit1: TEdit
      Left = 520
      Top = 18
      Width = 62
      Height = 21
      TabOrder = 10
      Text = 'Edit1'
    end
  end
  object Panel2: TPanel
    Left = 85
    Top = 105
    Width = 12
    Height = 2
    BevelOuter = bvNone
    Color = clRed
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object Panel3: TPanel
    Left = 103
    Top = 105
    Width = 12
    Height = 2
    BevelOuter = bvNone
    Color = clRed
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
  object SynWebSyn1: TSynWebSyn
    ActiveHighlighter = False
    HighlighterMode = shmPhp
    InactiveAttri.Foreground = clInactiveCaptionText
    HtmlVersion = hvXHtml10Transitional
    Html_CommentAttri.Foreground = clMedGray
    Html_EscapeAttri.Foreground = clTeal
    Html_SymbolAttri.Foreground = clBlack
    Html_TagAttri.Foreground = clNavy
    Html_TagNameAttri.Foreground = clBlue
    Html_TagNameUndefAttri.Foreground = clBlue
    Html_TagNameUndefAttri.Style = [fsUnderline]
    Html_TagKeyAttri.Foreground = clRed
    Html_TagKeyUndefAttri.Foreground = clRed
    Html_TagKeyUndefAttri.Style = [fsUnderline]
    Html_TagKeyValueAttri.Foreground = clFuchsia
    Html_TagKeyValueQuotedAttri.Foreground = clFuchsia
    Html_ErrorAttri.Foreground = clRed
    Html_ErrorAttri.Style = [fsBold, fsUnderline]
    CssVersion = cvCss21
    Css_WhitespaceAttri.Background = 15794175
    Css_RulesetWhitespaceAttri.Background = clInfoBk
    Css_SelectorAttri.Foreground = clBlue
    Css_SelectorAttri.Style = [fsBold]
    Css_SelectorUndefAttri.Foreground = clBlue
    Css_SelectorUndefAttri.Style = [fsBold, fsUnderline]
    Css_SelectorClassAttri.Foreground = 12615680
    Css_SelectorClassAttri.Style = [fsBold]
    Css_SelectorIdAttri.Foreground = clGreen
    Css_SelectorIdAttri.Style = [fsBold]
    Css_SpecialAttri.Foreground = clNavy
    Css_CommentAttri.Foreground = clMedGray
    Css_CommentAttri.Style = [fsItalic]
    Css_PropAttri.Foreground = clBlue
    Css_PropUndefAttri.Foreground = clBlue
    Css_PropUndefAttri.Style = [fsUnderline]
    Css_ValAttri.Foreground = clRed
    Css_ValUndefAttri.Foreground = clRed
    Css_ValUndefAttri.Style = [fsUnderline]
    Css_ValStringAttri.Foreground = clFuchsia
    Css_ValNumberAttri.Foreground = clGreen
    Css_SymbolAttri.Foreground = clBlack
    Css_ErrorAttri.Foreground = clRed
    Css_ErrorAttri.Style = [fsBold, fsUnderline]
    ES_WhitespaceAttri.Background = 16773360
    ES_IdentifierAttri.Foreground = clBlue
    ES_KeyAttri.Style = [fsBold]
    ES_CommentAttri.Foreground = clGreen
    ES_StringAttri.Foreground = clRed
    ES_NumberAttri.Foreground = clFuchsia
    ES_ErrorAttri.Foreground = clRed
    ES_ErrorAttri.Style = [fsBold, fsUnderline]
    PhpVersion = pvPhp5
    PhpShortOpenTag = True
    PhpAspTags = False
    Php_WhitespaceAttri.Background = 16119285
    Php_IdentifierAttri.Foreground = clMaroon
    Php_KeyAttri.Foreground = clBlue
    Php_FunctionAttri.Foreground = clRed
    Php_VariableAttri.Foreground = clTeal
    Php_ConstAttri.Style = [fsBold]
    Php_StringAttri.Foreground = clFuchsia
    Php_StringSpecialAttri.Background = 15395562
    Php_StringSpecialAttri.Foreground = clFuchsia
    Php_CommentAttri.Foreground = clGreen
    Php_CommentAttri.Style = [fsItalic]
    Php_NumberAttri.Foreground = clPurple
    Php_ErrorAttri.Foreground = clRed
    Php_ErrorAttri.Style = [fsBold, fsUnderline]
    Left = 20
    Top = 64
  end
  object SynExporterHTML1: TSynExporterHTML
    Color = clWindow
    DefaultFilter = 'HTML Documents (*.htm;*.html)|*.htm;*.html'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Highlighter = SynWebSyn1
    Title = 'Untitled'
    UseBackground = False
    Left = 20
    Top = 96
  end
  object SynEditOptionsDialog1: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 20
    Top = 128
  end
end
