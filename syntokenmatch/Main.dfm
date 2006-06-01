object Form1: TForm1
  Left = 318
  Top = 163
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label6: TLabel
      Left = 0
      Top = 28
      Width = 688
      Height = 13
      Hint = 
        'Token matching (works in PHP for ('#39'{'#39', '#39'('#39', '#39'['#39', '#39'match_me_open'#39 +
        ')->(('#39'}'#39', '#39')'#39', '#39']'#39', '#39'match_me_close'#39')'
      Align = alBottom
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
  end
  object SynEdit1: TSynEdit
    Left = 0
    Top = 41
    Width = 688
    Height = 412
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn1
    Lines.WideStrings = 
      '( { comment, close brace in comment ) } )'#13#10'if( (A > 10) and // )' +
      ' ) ()( )(()()'#13#10'  (B <25)) then'#13#10'begin'#13#10'begin end'#13#10'begin end'#13#10'end' +
      #13#10#13#10#13#10#13#10#13#10'begin // unmached begin-end'#13#10#13#10#13#10#13#10'// end'
    WordWrap = True
    OnPaintTransient = SynEdit1PaintTransient
  end
  object SynPasSyn1: TSynPasSyn
    AsmAttri.Foreground = clGreen
    CommentAttri.Foreground = 8421440
    DirectiveAttri.Foreground = clTeal
    IdentifierAttri.Foreground = clBlue
    NumberAttri.Foreground = clFuchsia
    FloatAttri.Foreground = clFuchsia
    HexAttri.Foreground = clFuchsia
    StringAttri.Foreground = clRed
    CharAttri.Foreground = clRed
    Left = 40
    Top = 116
  end
end
