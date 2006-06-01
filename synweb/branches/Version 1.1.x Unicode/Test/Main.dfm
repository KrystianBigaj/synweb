object Form1: TForm1
  Left = 192
  Top = 129
  Width = 696
  Height = 480
  Caption = 'Form1'
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
    Width = 688
    Height = 408
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
    Highlighter = SynWebHtmlSyn1
    Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoRightMouseMovesCursor, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    OnDropFiles = SynEdit1DropFiles
    OnStatusChange = SynEdit1StatusChange
    OnPaintTransient = SynEdit1PaintTransient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 45
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      688
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
      Left = 566
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
      Left = 662
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
      Left = 522
      Top = 20
      Width = 165
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 7
      Text = 'HTM (+PHP, +CSS, +ES)'
      OnChange = ComboBox4Change
      Items.Strings = (
        'HTM (+PHP, +CSS, +ES)'
        'CSS'
        'JS'
        'PHP-Cli')
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
  end
  object SynWebEngine1: TSynWebEngine
    Left = 48
    Top = 228
  end
  object SynWebHtmlSyn1: TSynWebHtmlSyn
    ActiveSwitchHighlighter = False
    Engine = SynWebEngine1
    Left = 48
    Top = 260
  end
  object SynWebCSSSyn1: TSynWebCssSyn
    ActiveSwitchHighlighter = False
    Engine = SynWebEngine1
    Left = 112
    Top = 260
  end
  object SynWebESSyn1: TSynWebEsSyn
    ActiveSwitchHighlighter = False
    Engine = SynWebEngine1
    Left = 144
    Top = 260
  end
  object SynWebPHPCliSyn1: TSynWebPhpCliSyn
    ActiveSwitchHighlighter = False
    Engine = SynWebEngine1
    Left = 80
    Top = 260
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
end
