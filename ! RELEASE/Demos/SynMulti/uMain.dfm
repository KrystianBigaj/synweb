object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'SynWeb + TSynMultiSyn Example'
  ClientHeight = 545
  ClientWidth = 668
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
    Width = 668
    Height = 545
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
  end
  object SynWebEngine: TSynWebEngine
    Options.HtmlVersion = shvXHtml10Transitional
    Options.WmlVersion = swvWml13
    Options.XsltVersion = swvXslt20
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Left = 40
    Top = 80
  end
  object SynWebHtmlSyn: TSynWebHtmlSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Options.HtmlVersion = shvHtml401Transitional
    Options.CssVersion = scvCss21
    Options.PhpVersion = spvPhp5
    Options.PhpShortOpenTag = True
    Options.PhpAspTags = False
    Options.AllowASPTags = True
    Options.CssEmbeded = True
    Options.PhpEmbeded = False
    Options.EsEmbeded = True
    Options.UseEngineOptions = False
    Left = 88
    Top = 80
  end
  object SynCSSyn: TSynCSSyn
    Left = 120
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
