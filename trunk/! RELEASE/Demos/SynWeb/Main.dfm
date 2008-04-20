object Form1: TForm1
  Left = 291
  Top = 172
  ActiveControl = CheckBox1
  Caption = 'Form1'
  ClientHeight = 472
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 45
    Width = 900
    Height = 307
    Align = alClient
    ActiveLineColor = 16448250
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
    Highlighter = SynWebHtmlSyn1
    Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoRightMouseMovesCursor, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    OnChange = SynEdit1Change
    OnStatusChange = SynEdit1StatusChange
    OnPaintTransient = SynEdit1PaintTransient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 45
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      900
      45)
    object Label1: TLabel
      Left = 4
      Top = 24
      Width = 65
      Height = 13
      Caption = 'HTMLVersion:'
    end
    object Label2: TLabel
      Left = 224
      Top = 24
      Width = 54
      Height = 13
      Caption = 'CSSVersion'
    end
    object Label3: TLabel
      Left = 372
      Top = 24
      Width = 54
      Height = 13
      Caption = 'PHPVersion'
    end
    object Label4: TLabel
      Left = 778
      Top = 4
      Width = 83
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
      Left = 524
      Top = 24
      Width = 58
      Height = 13
      Caption = 'WMLVersion'
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
      Left = 874
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
      Left = 734
      Top = 20
      Width = 165
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemHeight = 13
      TabOrder = 7
      OnChange = ComboBox4Change
      Items.Strings = (
        'HTM (+PHP, +CSS, +ES)'
        'CSS'
        'JS'
        'PHP-Cli'
        'PHP (Plain)'
        'WML (+PHP)'
        'XML (+PHP)'
        'Smarty (+CSS, +ES)')
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
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = CheckBox4Click
    end
    object Button3: TButton
      Left = 692
      Top = 24
      Width = 41
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Load def'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = Button3Click
    end
    object ComboBox5: TComboBox
      Left = 592
      Top = 20
      Width = 77
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
      OnChange = ComboBox5Change
    end
  end
  object SynWebErrorList: TListBox
    Left = 0
    Top = 352
    Width = 900
    Height = 120
    Align = alBottom
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 2
    Visible = False
    OnDblClick = SynWebErrorListDblClick
  end
  object SynWebEngine1: TSynWebEngine
    InactiveAttri.Foreground = clSilver
    Options.HtmlVersion = shvXHtml10Transitional
    Options.WmlVersion = swvWml13
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Left = 48
    Top = 232
  end
  object SynWebHtmlSyn1: TSynWebHtmlSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.HtmlVersion = shvXHtml10Transitional
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.CssEmbeded = True
    Options.PhpEmbeded = True
    Options.EsEmbeded = True
    Options.UseEngineOptions = True
    Left = 48
    Top = 264
  end
  object SynWebCSSSyn1: TSynWebCssSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.HtmlVersion = shvXHtml10Transitional
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.PhpEmbeded = True
    Options.UseEngineOptions = True
    Left = 144
    Top = 264
  end
  object SynWebESSyn1: TSynWebEsSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.PhpEmbeded = True
    Options.UseEngineOptions = True
    Left = 176
    Top = 264
  end
  object SynWebPHPCliSyn1: TSynWebPhpCliSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.UseEngineOptions = True
    Left = 208
    Top = 264
  end
  object SynExporterHTML1: TSynExporterHTML
    Color = clWindow
    DefaultFilter = 'HTML Documents (*.htm;*.html)|*.htm;*.html'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Title = 'Untitled'
    UseBackground = False
    Left = 20
    Top = 96
    TitleW = 'Untitled'
  end
  object SynEditOptionsDialog1: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 20
    Top = 128
  end
  object SynWebWmlSyn1: TSynWebWmlSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.WmlVersion = swvWml13
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.PhpEmbeded = True
    Options.UseEngineOptions = True
    Left = 80
    Top = 264
  end
  object SynWebXmlSyn1: TSynWebXmlSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = False
    Options.PhpAspTags = False
    Options.PhpEmbeded = True
    Options.UseEngineOptions = False
    Left = 112
    Top = 264
  end
  object scpDemo: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 340
    EndOfTokenChr = ';>()[] .'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'constructor'
        BiggestWordW = 'constructor'
      end>
    OnExecute = scpDemoExecute
    ShortCut = 16416
    Editor = SynEdit1
    OnAfterCodeCompletion = scpDemoAfterCodeCompletion
    Left = 52
    Top = 96
    EndOfTokenChrW = ';>()[] .'
    TriggerCharsW = '.'
  end
  object SynWebErrorTimer: TTimer
    OnTimer = SynWebErrorTimerTimer
    Left = 36
    Top = 304
  end
  object PopupMenu1: TPopupMenu
    Left = 512
    Top = 404
    object Reload1: TMenuItem
      Caption = 'Reload'
      OnClick = Reload1Click
    end
  end
  object SynWebPhpPlainSyn1: TSynWebPhpPlainSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.PhpVersion = spvPhp5
    Options.UseEngineOptions = True
    Left = 240
    Top = 264
  end
  object mar: TSynWebSmartySyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine1
    Options.SmartyLDelim = '{'
    Options.SmartyRDelim = '}'
    Options.HtmlVersion = shvXHtml10Transitional
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.CssEmbeded = True
    Options.EsEmbeded = True
    Options.UseEngineOptions = False
    Left = 280
    Top = 264
  end
end
