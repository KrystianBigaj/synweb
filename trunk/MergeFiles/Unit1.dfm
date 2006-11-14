object Form1: TForm1
  Left = 363
  Top = 145
  BorderStyle = bsToolWindow
  Caption = 'Merge Delphi $I files'
  ClientHeight = 349
  ClientWidth = 706
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    706
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 4
    Top = 332
    Width = 699
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Ready.'
  end
  object GroupBox1: TGroupBox
    Left = 240
    Top = 0
    Width = 463
    Height = 197
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Export'
    TabOrder = 0
    DesignSize = (
      463
      197)
    object Memo2: TMemo
      Left = 8
      Top = 44
      Width = 447
      Height = 145
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        '..\PHP\PHP.exe ..\Config\PHP-wPECL.ini'
        '..\HTML\HTML.exe ..\Config\HTML.ini'
        '..\HTML\HTML.exe ..\Config\HTML-Special.ini'
        '..\CSS\CSS.exe ..\Config\CSS.ini'
        '..\CSS\CSS.exe ..\Config\CSS-Special.ini'
        '..\ES\ES.exe ..\Config\ES.ini')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Button2: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Exec export'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 84
      Top = 16
      Width = 371
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Exec export && Merge'
      Default = True
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 4
    Width = 229
    Height = 193
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Merge'
    TabOrder = 1
    DesignSize = (
      229
      193)
    object Label1: TLabel
      Left = 8
      Top = 92
      Width = 63
      Height = 13
      Caption = 'Exclude files:'
    end
    object Memo1: TMemo
      Left = 8
      Top = 112
      Width = 217
      Height = 73
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'SynEdit.inc'
        'SynWeb.inc'
        'SynHighlighterWeb.pas'
        'SynHighlighterWebData.pas'
        'SynTokenMatch.pas'
        'SynHighlighterWebMisc.pas')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Button1: TButton
      Left = 72
      Top = 84
      Width = 153
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Merge'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Memo3: TMemo
      Left = 8
      Top = 16
      Width = 217
      Height = 64
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        '..\SynHighlighterWeb.pas'
        '..\SynHighlighterWebData.pas'
        '..\SynHighlighterWebMisc.pas'
        '..\QSynHighlighterWeb.pas'
        '..\QSynHighlighterWebData.pas'
        '..\QSynHighlighterWebMisc.pas'
        '..\..\SynTokenMatch\SynTokenMatch.pas'
        '..\..\SynTokenMatch\QSynTokenMatch.pas')
      ScrollBars = ssBoth
      TabOrder = 2
    end
    object CheckBox1: TCheckBox
      Left = 96
      Top = 0
      Width = 121
      Height = 16
      Caption = 'Add extra comments'
      TabOrder = 3
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 200
    Width = 697
    Height = 129
    Caption = 'Log'
    TabOrder = 2
    DesignSize = (
      697
      129)
    object Memo4: TMemo
      Left = 8
      Top = 16
      Width = 681
      Height = 105
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end
