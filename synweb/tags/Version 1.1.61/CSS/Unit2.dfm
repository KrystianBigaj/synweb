object Form2: TForm2
  Left = 422
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Export'
  ClientHeight = 142
  ClientWidth = 247
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    247
    142)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 0
    Width = 239
    Height = 138
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Export'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'Css 2.1'
      Layout = tlCenter
    end
    object Label2: TLabel
      Left = 8
      Top = 20
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'Css 1'
      Layout = tlCenter
    end
    object Bevel2: TBevel
      Left = 8
      Top = 60
      Width = 221
      Height = 5
    end
    object Bevel3: TBevel
      Left = 8
      Top = 96
      Width = 221
      Height = 5
    end
    object ht2: TCheckBox
      Left = 132
      Top = 36
      Width = 45
      Height = 17
      Caption = 'Props'
      TabOrder = 0
    end
    object ha2: TCheckBox
      Left = 184
      Top = 36
      Width = 45
      Height = 17
      Caption = 'Vals'
      TabOrder = 1
    end
    object ht1: TCheckBox
      Left = 132
      Top = 16
      Width = 45
      Height = 17
      Caption = 'Props'
      TabOrder = 2
    end
    object ha1: TCheckBox
      Left = 184
      Top = 16
      Width = 45
      Height = 17
      Caption = 'Vals'
      TabOrder = 3
    end
    object Button1: TButton
      Left = 108
      Top = 68
      Width = 61
      Height = 25
      Caption = 'Props as list'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 68
      Width = 97
      Height = 25
      Caption = 'Tree'
      TabOrder = 5
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 172
      Top = 68
      Width = 57
      Height = 25
      Caption = 'Vals as list'
      TabOrder = 6
      OnClick = Button3Click
    end
    object CheckBox3: TCheckBox
      Left = 128
      Top = 0
      Width = 81
      Height = 17
      Caption = 'All'
      TabOrder = 7
    end
    object Button6: TButton
      Left = 8
      Top = 107
      Width = 221
      Height = 25
      Caption = 'Export *.inc'
      TabOrder = 8
      OnClick = Button4Click
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 212
    Top = 4
  end
end
