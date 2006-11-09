object Form1: TForm1
  Left = 575
  Top = 43
  Width = 449
  Height = 623
  Caption = 'SynHighlighterWeb - ECMAScript Data/Export v1.0b '#169'2005 FlatDev'
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
    441
    596)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 298
    Top = 360
    Width = 15
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '0 0'
  end
  object TreeView1: TTreeView
    Left = 4
    Top = 4
    Width = 289
    Height = 585
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
    Left = 298
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save(TXT)'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 298
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load(TXT)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 298
    Top = 314
    Width = 134
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Delete'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button11: TButton
    Left = 298
    Top = 342
    Width = 45
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Check'
    TabOrder = 4
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 350
    Top = 342
    Width = 57
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'sort TAGs'
    TabOrder = 5
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 378
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 6
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 354
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
    Left = 299
    Top = 410
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
    Left = 297
    Top = 164
    Width = 137
    Height = 89
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object Button3: TButton
    Left = 297
    Top = 80
    Width = 137
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add Keyword'
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
    Left = 297
    Top = 108
    Width = 137
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add FutureReserved'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    OnClick = Button3Click
  end
  object Button7: TButton
    Tag = 2
    Left = 297
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
    TabOrder = 12
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 297
    Top = 466
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'stay on top'
    TabOrder = 13
    OnClick = CheckBox1Click
  end
  object Button6: TButton
    Left = 378
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 14
    OnClick = Button6Click
  end
  object OpenDialog1: TOpenDialog
    Left = 28
    Top = 280
  end
  object SaveDialog1: TSaveDialog
    Left = 28
    Top = 312
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
      Caption = 'Keyword'
      OnClick = keyword1Click
    end
    object const1: TMenuItem
      Tag = 1
      Caption = 'FutureReservedWord'
      OnClick = keyword1Click
    end
    object var1: TMenuItem
      Tag = 2
      Caption = 'var'
      Visible = False
      OnClick = keyword1Click
    end
    object function1: TMenuItem
      Tag = 3
      Caption = 'function'
      Visible = False
      OnClick = keyword1Click
    end
  end
end
