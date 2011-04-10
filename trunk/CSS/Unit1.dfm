object Form1: TForm1
  Left = 665
  Top = 10
  Caption = 'SynHighlighterWeb - Css Data/Export v1.0b '#169'2005 FlatDev'
  ClientHeight = 703
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    342
    703)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 199
    Top = 371
    Width = 15
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '0 0'
  end
  object TreeView1: TTreeView
    Left = 4
    Top = 4
    Width = 190
    Height = 662
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    PopupMenu = PopupMenu1
    RightClickSelect = True
    RowSelect = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 199
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save(TXT)'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 199
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load(TXT)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 307
    Top = 60
    Width = 29
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'TAG'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 199
    Top = 308
    Width = 134
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 199
    Top = 84
    Width = 136
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Add ATTRIB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Button5Click
  end
  object Memo5: TMemo
    Left = 199
    Top = 104
    Width = 134
    Height = 85
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object Edit1: TEdit
    Left = 199
    Top = 60
    Width = 109
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 7
  end
  object Button11: TButton
    Left = 199
    Top = 357
    Width = 45
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Check'
    TabOrder = 8
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 251
    Top = 357
    Width = 57
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'sort TAGs'
    TabOrder = 9
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 279
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 10
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 279
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 11
    OnClick = Button14Click
  end
  object ComboBox1: TComboBox
    Left = 4
    Top = 669
    Width = 185
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    ItemIndex = 2
    TabOrder = 12
    Text = 'Css3'
    Items.Strings = (
      'Css 1'
      'Css 2.1'
      'Css3')
  end
  object CheckBox1: TCheckBox
    Left = 311
    Top = 357
    Width = 13
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'R'
    TabOrder = 13
    WordWrap = True
  end
  object Button18: TButton
    Left = 200
    Top = 332
    Width = 133
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'EXPORT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    TabOrder = 14
    OnClick = Button18Click
  end
  object Memo1: TMemo
    Left = 199
    Top = 216
    Width = 134
    Height = 85
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 15
    WordWrap = False
  end
  object Button6: TButton
    Left = 199
    Top = 196
    Width = 136
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'PARSE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 16
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 199
    Top = 460
    Width = 125
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Css: Convert'
    Enabled = False
    TabOrder = 17
    Visible = False
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 199
    Top = 432
    Width = 117
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Fix color=>[color]'
    TabOrder = 18
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 199
    Top = 488
    Width = 125
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Css: Add other (Css2)'
    Enabled = False
    TabOrder = 19
    Visible = False
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 195
    Top = 528
    Width = 97
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'find bit (level=0)'
    TabOrder = 20
    OnClick = Button10Click
  end
  object Edit2: TEdit
    Left = 195
    Top = 556
    Width = 49
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 21
    Text = '24'
  end
  object CheckBox2: TCheckBox
    Left = 192
    Top = 668
    Width = 17
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 22
    OnClick = CheckBox2Click
  end
  object iss: TCheckBox
    Left = 220
    Top = 672
    Width = 109
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Is Special (Css)?'
    TabOrder = 23
  end
  object Button15: TButton
    Left = 196
    Top = 580
    Width = 45
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Button15'
    Enabled = False
    TabOrder = 24
    Visible = False
    OnClick = Button15Click
  end
  object CheckBox3: TCheckBox
    Left = 288
    Top = 532
    Width = 13
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'CheckBox3'
    TabOrder = 25
    OnClick = CheckBox3Click
  end
  object Button16: TButton
    Left = 259
    Top = 559
    Width = 75
    Height = 25
    Caption = 'css2->css3'
    Enabled = False
    TabOrder = 26
    Visible = False
    OnClick = Button16Click
  end
  object Button17: TButton
    Left = 259
    Top = 590
    Width = 75
    Height = 25
    Caption = 'add colors'
    TabOrder = 27
    OnClick = Button17Click
  end
  object OpenDialog1: TOpenDialog
    Left = 28
    Top = 280
  end
  object SaveDialog1: TSaveDialog
    Left = 40
    Top = 292
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 36
    Top = 192
    object None1: TMenuItem
      Caption = 'None'
      Default = True
      Enabled = False
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Html401Strict1: TMenuItem
      Caption = '0 - Css 1'
      OnClick = Html401Strict1Click
    end
    object Html401Transitional1: TMenuItem
      Tag = 1
      Caption = '1 - Css 2.1'
      OnClick = Html401Strict1Click
    end
    object Html401Frameset1: TMenuItem
      Tag = 2
      Caption = '2 - Css 3'
      OnClick = Html401Strict1Click
    end
    object CLOSEPOPUP1: TMenuItem
      Caption = '<CLOSE POPUP>'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Values1: TMenuItem
      Tag = -1
      Caption = '<VALUES>'
      Enabled = False
    end
    object IDENT1: TMenuItem
      Tag = 31
      Caption = '[(function)]'
      OnClick = Html401Strict1Click
    end
    object angle6: TMenuItem
      Tag = 31
      Caption = '[angle]'
      OnClick = Html401Strict1Click
    end
    object frequency2: TMenuItem
      Tag = 30
      Caption = '[frequency]'
      OnClick = Html401Strict1Click
    end
    object time2: TMenuItem
      Tag = 29
      Caption = '[time]'
      OnClick = Html401Strict1Click
    end
    object length3: TMenuItem
      Tag = 28
      Caption = '[length]'
      OnClick = Html401Strict1Click
    end
    object length2: TMenuItem
      Tag = 27
      Caption = '[length-negative]'
      OnClick = Html401Strict1Click
    end
    object percentage3: TMenuItem
      Tag = 26
      Caption = '[percentage]'
      OnClick = Html401Strict1Click
    end
    object percentage2: TMenuItem
      Tag = 25
      Caption = '[percentage-negative]'
      OnClick = Html401Strict1Click
    end
    object number3: TMenuItem
      Tag = 24
      Caption = '[number]'
      OnClick = Html401Strict1Click
    end
    object number2: TMenuItem
      Tag = 23
      Caption = '[number-negative]'
      OnClick = Html401Strict1Click
    end
    object integer3: TMenuItem
      Tag = 22
      Caption = '[Integer]'
      OnClick = Html401Strict1Click
    end
    object integer2: TMenuItem
      Tag = 21
      Caption = '[integer-negative]'
      OnClick = Html401Strict1Click
    end
    object identifier2: TMenuItem
      Tag = 20
      Caption = '[identifier]'
      OnClick = Html401Strict1Click
    end
    object string2: TMenuItem
      Tag = 19
      Caption = '[String]'
      OnClick = Html401Strict1Click
    end
    object color2: TMenuItem
      Tag = 18
      Caption = '[color]'
      OnClick = Html401Strict1Click
    end
    object N1009002: TMenuItem
      Tag = 17
      Caption = '[100-900]'
      OnClick = Html401Strict1Click
    end
    object N4: TMenuItem
      Tag = 16
      Caption = '[,]'
      OnClick = Html401Strict1Click
    end
    object dpi1: TMenuItem
      Tag = 15
      Caption = '[dpi]'
      OnClick = Html401Strict1Click
    end
    object css21pseudo1: TMenuItem
      Tag = 14
      Caption = '[uri]'
      OnClick = Html401Strict1Click
    end
    object N3: TMenuItem
      Tag = -1
      Caption = '-'
    end
    object SPECIAL1: TMenuItem
      Tag = -1
      Caption = '<SPECIAL>'
      Enabled = False
    end
    object pseudo1: TMenuItem
      Tag = 14
      Caption = '[pseudo]'
      OnClick = Html401Strict1Click
    end
    object media1: TMenuItem
      Tag = 13
      Caption = '[media]'
      OnClick = Html401Strict1Click
    end
    object atkeyword1: TMenuItem
      Tag = 12
      Caption = '[at-keyword]'
      OnClick = Html401Strict1Click
    end
    object page2: TMenuItem
      Tag = 11
      Caption = '[page]'
    end
    object page1: TMenuItem
      Tag = 10
      Caption = '[pseudo with param]'
      OnClick = Html401Strict1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 192
    Top = 396
  end
end
