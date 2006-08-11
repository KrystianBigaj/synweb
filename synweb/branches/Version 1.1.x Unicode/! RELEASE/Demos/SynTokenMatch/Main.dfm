object Form1: TForm1
  Left = 227
  Top = 151
  Width = 697
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
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 689
    Height = 453
    Align = alClient
    ActiveLineColor = clInfoBk
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
    Highlighter = SynPasSyn1
    Lines.WideStrings = 
      'if ( (a > b) and // close brace in comment  ) )))))) ((('#13#10'  (b<4' +
      ') or (S='#39')( begin end ( '#39' ) ) then'#13#10'begin'#13#10'  try'#13#10'    try'#13#10'    e' +
      'xcept'#13#10'    end'#13#10'  finally'#13#10'  end;'#13#10'end;'#13#10#13#10'TSyn=class;'#13#10'...'#13#10'end' +
      ';'
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
