object Form1: TForm1
  Left = 439
  Top = 84
  Caption = 'SynHighlighterWeb - Php Data/Export v1.0b '#169'2005 FlatDev'
  ClientHeight = 628
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    464
    628)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 321
    Top = 423
    Width = 15
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '0 0'
  end
  object Label2: TLabel
    Left = 320
    Top = 482
    Width = 62
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Php4 Version'
  end
  object Label3: TLabel
    Left = 320
    Top = 522
    Width = 62
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Php5 Version'
  end
  object Label5: TLabel
    Left = 319
    Top = 574
    Width = 141
    Height = 14
    Align = alCustom
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = '-'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 319
    Top = 558
    Width = 141
    Height = 14
    Align = alCustom
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = '-'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 319
    Top = 590
    Width = 141
    Height = 14
    Align = alCustom
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = '-'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object TreeView1: TTreeView
    Left = 4
    Top = 4
    Width = 312
    Height = 596
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    PopupMenu = PopupMenu1
    RightClickSelect = True
    RowSelect = True
    SortType = stBoth
    TabOrder = 0
    OnCompare = TreeView1Compare
  end
  object Button1: TButton
    Left = 321
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save(TXT)'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 321
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load(TXT)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 321
    Top = 397
    Width = 134
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Delete'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button11: TButton
    Left = 349
    Top = 421
    Width = 45
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Check'
    TabOrder = 4
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 401
    Top = 420
    Width = 57
    Height = 18
    Anchors = [akRight, akBottom]
    Caption = 'sort TAGs'
    TabOrder = 5
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 401
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 6
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 377
    Top = 4
    Width = 23
    Height = 29
    Anchors = [akTop, akRight]
    Caption = 'LD ini'
    TabOrder = 7
    Visible = False
    WordWrap = True
    OnClick = Button14Click
  end
  object Button18: TButton
    Left = 322
    Top = 441
    Width = 133
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'EXPORT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    TabOrder = 8
    OnClick = Button18Click
  end
  object Memo1: TMemo
    Left = 320
    Top = 164
    Width = 137
    Height = 89
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object Button3: TButton
    Left = 320
    Top = 80
    Width = 137
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add keywords'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
    OnClick = Button3Click
  end
  object Button5: TButton
    Tag = 1
    Left = 320
    Top = 108
    Width = 137
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add consts'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    OnClick = Button3Click
  end
  object php4box: TCheckBox
    Left = 320
    Top = 60
    Width = 48
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Php4'
    Checked = True
    State = cbChecked
    TabOrder = 12
  end
  object php5box: TCheckBox
    Left = 368
    Top = 60
    Width = 48
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Php5'
    TabOrder = 13
  end
  object Button7: TButton
    Tag = 2
    Left = 320
    Top = 136
    Width = 137
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add vars'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 14
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 465
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'stay on top'
    TabOrder = 15
    OnClick = CheckBox1Click
  end
  object Edit1: TEdit
    Left = 320
    Top = 498
    Width = 141
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 16
    Text = 'Php 4'
  end
  object Edit2: TEdit
    Left = 319
    Top = 538
    Width = 141
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 17
    Text = 'Php 5'
  end
  object Button8: TButton
    Left = 4
    Top = 611
    Width = 45
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'GET 1'
    TabOrder = 18
    OnClick = Button8Click
  end
  object ComboBox1: TComboBox
    Left = 92
    Top = 611
    Width = 369
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 0
    TabOrder = 19
    OnDropDown = ComboBox1DropDown
  end
  object pb: TProgressBar
    Left = 4
    Top = 603
    Width = 312
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Smooth = True
    TabOrder = 20
  end
  object Button9: TButton
    Left = 319
    Top = 375
    Width = 133
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Delete functions'
    TabOrder = 21
    OnClick = Button9Click
  end
  object Button6: TButton
    Left = 401
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 22
    OnClick = Button6Click
  end
  object Memo2: TMemo
    Left = 319
    Top = 256
    Width = 142
    Height = 45
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'Memo'
      '2')
    ScrollBars = ssBoth
    TabOrder = 23
  end
  object Button10: TButton
    Left = 52
    Top = 611
    Width = 45
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'GET 2'
    TabOrder = 24
    OnClick = Button10Click
  end
  object peclbox: TCheckBox
    Left = 416
    Top = 60
    Width = 44
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'PECL'
    TabOrder = 25
  end
  object Memo3: TMemo
    Left = 319
    Top = 304
    Width = 80
    Height = 69
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 26
  end
  object Memo4: TMemo
    Left = 407
    Top = 304
    Width = 56
    Height = 69
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 27
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
    Top = 196
    object None1: TMenuItem
      Caption = 'None'
      Default = True
      Enabled = False
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object keyword1: TMenuItem
      Caption = 'keyword'
      OnClick = keyword1Click
    end
    object const1: TMenuItem
      Tag = 1
      Caption = 'const'
      OnClick = keyword1Click
    end
    object var1: TMenuItem
      Tag = 2
      Caption = 'var'
      OnClick = keyword1Click
    end
    object function1: TMenuItem
      Tag = 3
      Caption = 'function'
      OnClick = keyword1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object php4: TMenuItem
      Tag = 16
      AutoCheck = True
      Caption = 'php4'
      OnClick = php4Click
    end
    object php5: TMenuItem
      Tag = 17
      AutoCheck = True
      Caption = 'php5'
      OnClick = php4Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object pecl: TMenuItem
      Tag = 18
      Caption = 'PECL'
    end
    object alias: TMenuItem
      Tag = 19
      Caption = 'ALIAS'
    end
  end
end
