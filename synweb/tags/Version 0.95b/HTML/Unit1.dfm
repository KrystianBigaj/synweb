object Form1: TForm1
  Left = 415
  Top = 8
  BorderStyle = bsSingle
  Caption = 'SynHighlighterWeb - HTML Data/Export v1.0b '#169'2005 FlatDev'
  ClientHeight = 703
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    569
    703)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 208
    Top = 687
    Width = 15
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '0 0'
  end
  object TreeView1: TTreeView
    Left = 4
    Top = 4
    Width = 272
    Height = 662
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    PopupMenu = PopupMenu1
    RightClickSelect = True
    RowSelect = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 428
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save(TXT)'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 428
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load(TXT)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 536
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
    Left = 428
    Top = 620
    Width = 134
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 428
    Top = 116
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
  object Button6: TButton
    Left = 428
    Top = 224
    Width = 136
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'coreattrs'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 428
    Top = 304
    Width = 136
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'i18n'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 428
    Top = 384
    Width = 136
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'events'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 428
    Top = 84
    Width = 136
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'TAG/attrs/ATTRIB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    TabOrder = 9
    OnClick = Button9Click
  end
  object Memo1: TMemo
    Left = 428
    Top = 248
    Width = 134
    Height = 53
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'id'
      'class'
      'style'
      'title')
    ScrollBars = ssBoth
    TabOrder = 10
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 428
    Top = 328
    Width = 134
    Height = 53
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'lang'
      'xml:lang'
      'dir')
    ScrollBars = ssBoth
    TabOrder = 11
    WordWrap = False
  end
  object Memo3: TMemo
    Left = 428
    Top = 408
    Width = 134
    Height = 49
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'onclick'
      'ondblclick'
      'onmousedown'
      'onmouseup'
      'onmouseover'
      'onmousemove'
      'onmouseout'
      'onkeypress'
      'onkeydown'
      'onkeyup')
    ScrollBars = ssBoth
    TabOrder = 12
    WordWrap = False
  end
  object Button10: TButton
    Left = 428
    Top = 460
    Width = 136
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'reserved'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 13
    OnClick = Button10Click
  end
  object Memo4: TMemo
    Left = 428
    Top = 484
    Width = 134
    Height = 53
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'datasrc'
      'datafld'
      'dataformatas')
    ScrollBars = ssBoth
    TabOrder = 14
    WordWrap = False
  end
  object Memo5: TMemo
    Left = 428
    Top = 136
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
  object Edit1: TEdit
    Left = 428
    Top = 60
    Width = 109
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 16
  end
  object Button11: TButton
    Left = 204
    Top = 669
    Width = 45
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Check'
    TabOrder = 17
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 252
    Top = 669
    Width = 57
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'sort TAGs'
    TabOrder = 18
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 508
    Top = 32
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 19
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 508
    Top = 4
    Width = 57
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 20
    OnClick = Button14Click
  end
  object ComboBox1: TComboBox
    Left = 4
    Top = 673
    Width = 197
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 21
    Items.Strings = (
      'HTML 4.01 Strict'
      'HTML 4.01 Transitional'
      'HTML 4.01 Frameset'
      'XHTML 1.0 Strict'
      'XHTML 1.0 Transitional'
      'XHTML 1.0 Frameset'
      'XHTML 1.1')
  end
  object Button15: TButton
    Left = 324
    Top = 669
    Width = 237
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'HTML 4.01 Transitional - > HTML 4.01 Frameset'
    TabOrder = 22
    OnClick = Button15Click
  end
  object Button16: TButton
    Left = 428
    Top = 540
    Width = 136
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'focus'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 23
    OnClick = Button16Click
  end
  object Memo6: TMemo
    Left = 428
    Top = 564
    Width = 134
    Height = 53
    Anchors = [akTop, akRight]
    Lines.Strings = (
      'accesskey'
      'tabindex'
      'onfocus'
      'onblur')
    ScrollBars = ssBoth
    TabOrder = 24
    WordWrap = False
  end
  object Button17: TButton
    Left = 324
    Top = 685
    Width = 237
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'XHTML 1.0 Transitional - > XHTML 1.0 Frameset'
    TabOrder = 25
    OnClick = Button17Click
  end
  object CheckBox1: TCheckBox
    Left = 308
    Top = 669
    Width = 13
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'R'
    TabOrder = 26
    WordWrap = True
  end
  object Button18: TButton
    Left = 429
    Top = 644
    Width = 133
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'EXPORT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clScrollBar
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    TabOrder = 27
    OnClick = Button18Click
  end
  object Memo7: TMemo
    Left = 279
    Top = 4
    Width = 145
    Height = 165
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 28
  end
  object Button19: TButton
    Left = 336
    Top = 172
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Attrs >>'
    TabOrder = 29
    OnClick = Button19Click
  end
  object Memo8: TMemo
    Left = 279
    Top = 200
    Width = 145
    Height = 165
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 30
  end
  object Button20: TButton
    Left = 279
    Top = 368
    Width = 145
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Attrs >>'
    TabOrder = 31
    OnClick = Button20Click
  end
  object Memo9: TMemo
    Left = 279
    Top = 396
    Width = 145
    Height = 165
    Anchors = [akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 32
  end
  object Button21: TButton
    Left = 279
    Top = 564
    Width = 145
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Attrs >>'
    TabOrder = 33
    OnClick = Button21Click
  end
  object Button22: TButton
    Left = 279
    Top = 588
    Width = 145
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Select No-CSS'
    TabOrder = 34
    OnClick = Button22Click
  end
  object CheckBox2: TCheckBox
    Left = 284
    Top = 612
    Width = 133
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'SetBit'
    TabOrder = 35
  end
  object Button24: TButton
    Left = 280
    Top = 172
    Width = 53
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 't:regexpr'
    TabOrder = 36
    OnClick = Button24Click
  end
  object CheckBox3: TCheckBox
    Left = 280
    Top = 644
    Width = 109
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Special: AmpList'
    TabOrder = 37
  end
  object OpenDialog1: TOpenDialog
    Left = 360
    Top = 304
  end
  object SaveDialog1: TSaveDialog
    Left = 372
    Top = 316
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 224
    Top = 264
    object None1: TMenuItem
      Caption = 'None'
      Default = True
      Enabled = False
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object HTML401Strict1: TMenuItem
      Caption = 'HTML 4.01 Strict'
      OnClick = HTML401Strict1Click
    end
    object HTML401Transitional1: TMenuItem
      Tag = 1
      Caption = 'HTML 4.01 Transitional'
      OnClick = HTML401Strict1Click
    end
    object HTML401Frameset1: TMenuItem
      Tag = 2
      Caption = 'HTML 4.01 Frameset'
      OnClick = HTML401Strict1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object XHTML10Strict1: TMenuItem
      Tag = 3
      Caption = 'XHTML 1.0 Strict'
      OnClick = HTML401Strict1Click
    end
    object XHTML10Transitional1: TMenuItem
      Tag = 4
      Caption = 'XHTML 1.0 Transitional'
      OnClick = HTML401Strict1Click
    end
    object XHTML10Frameset1: TMenuItem
      Tag = 5
      Caption = 'XHTML 1.0 Frameset'
      OnClick = HTML401Strict1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object XHTML111: TMenuItem
      Tag = 6
      Caption = 'XHTML 1.1'
      OnClick = HTML401Strict1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Otherstatus1: TMenuItem
      Caption = 'Other status'
      Enabled = False
    end
    object XHTML10TransitionalDEPRECATED1: TMenuItem
      Tag = 16
      Caption = 'XHTML 1.0 Transitional DEPRECATED'
      OnClick = HTML401Strict1Click
    end
    object XHTML10FramesetlDEPRECATED1: TMenuItem
      Tag = 17
      Caption = 'XHTML 1.0 Frameset DEPRECATED'
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object isEXT: TMenuItem
      Tag = 29
      Caption = 'IS "<?... ?>" ?'
      OnClick = HTML401Strict1Click
    end
    object NoCLASS1: TMenuItem
      Tag = 30
      Caption = 'No CLASS'
      OnClick = HTML401Strict1Click
    end
    object EMPTY1: TMenuItem
      Tag = 31
      Caption = 'Is EMPTY'
      OnClick = HTML401Strict1Click
    end
  end
end