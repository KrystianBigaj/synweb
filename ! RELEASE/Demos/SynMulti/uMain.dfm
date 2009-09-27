object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'SynWeb + TSynMultiSyn Example'
  ClientHeight = 216
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SynEditTest: TSynEdit
    Left = 0
    Top = 0
    Width = 426
    Height = 216
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
    Highlighter = SynMultiSyn
    Lines.Strings = (
      '<a href="te'
      '<%'
      ''
      '/*'
      ''
      'comment'
      ''
      '*/'
      '%>'
      ''
      'st">'
      ''
      '<style>'
      ''
      '<%'
      ''
      '/*'
      ''
      'comment'
      ''
      '*/'
      '%>'
      ''
      ''
      '</style>'
      ''
      '<script>'
      '        '#39'asd'#39
      '        /* <% '
      '           // C# code           '
      '           /*           '
      '             C# multiline comment'
      '           */'
      '         %>'
      '          */'
      '</script>')
  end
  object SynWebEngine: TSynWebEngine
    Options.HtmlVersion = shvXHtml10Transitional
    Options.WmlVersion = swvWml13
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Left = 48
    Top = 64
  end
  object SynWebHtmlSyn: TSynWebHtmlSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Options.HtmlVersion = shvXHtml10Transitional
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.CssEmbeded = True
    Options.PhpEmbeded = False
    Options.EsEmbeded = True
    Options.UseEngineOptions = True
    Left = 80
    Top = 64
  end
  object SynCSSyn: TSynCSSyn
    Left = 80
    Top = 32
  end
  object SynMultiSyn: TSynMultiSyn
    Schemes = <
      item
        StartExpr = '<%'
        EndExpr = '%>'
        Highlighter = SynCSSyn
        SchemeName = 'C#'
      end>
    DefaultHighlighter = SynWebHtmlSyn
    DefaultLanguageName = 'HTML'
    OnCustomRange = SynMultiSynCustomRange
    Left = 48
    Top = 32
  end
end
