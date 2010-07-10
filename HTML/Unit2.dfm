object Form2: TForm2
  Left = 508
  Top = 132
  BorderStyle = bsDialog
  Caption = 'Export'
  ClientHeight = 271
  ClientWidth = 247
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    247
    271)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 0
    Width = 239
    Height = 267
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Export'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'Html 4.01 Transitional'
      Layout = tlCenter
    end
    object Label2: TLabel
      Left = 8
      Top = 20
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'Html 4.01 Strict'
      Layout = tlCenter
    end
    object Label4: TLabel
      Left = 8
      Top = 60
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'Html 4.01 Frameset'
      Layout = tlCenter
    end
    object Label5: TLabel
      Left = 8
      Top = 104
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'XHtml 1.0 Strict'
      Layout = tlCenter
    end
    object Label8: TLabel
      Left = 8
      Top = 124
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'XHtml 1.0 Transitional'
      Layout = tlCenter
    end
    object Label9: TLabel
      Left = 8
      Top = 144
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'XHtml 1.0 Frameset'
      Layout = tlCenter
    end
    object Bevel1: TBevel
      Left = 8
      Top = 96
      Width = 221
      Height = 5
    end
    object Bevel2: TBevel
      Left = 8
      Top = 164
      Width = 221
      Height = 5
    end
    object Bevel3: TBevel
      Left = 8
      Top = 200
      Width = 221
      Height = 5
    end
    object Label3: TLabel
      Left = 8
      Top = 79
      Width = 117
      Height = 17
      AutoSize = False
      Caption = 'HTML 5'
      Enabled = False
      Layout = tlCenter
    end
    object ht2: TCheckBox
      Left = 132
      Top = 36
      Width = 45
      Height = 17
      Caption = 'Tags'
      TabOrder = 0
    end
    object ha2: TCheckBox
      Left = 184
      Top = 36
      Width = 45
      Height = 17
      Caption = 'Attrs'
      TabOrder = 1
    end
    object ht3: TCheckBox
      Left = 132
      Top = 56
      Width = 45
      Height = 17
      Caption = 'Tags'
      TabOrder = 2
    end
    object ha3: TCheckBox
      Left = 184
      Top = 56
      Width = 45
      Height = 17
      Caption = 'Attrs'
      TabOrder = 3
    end
    object ht1: TCheckBox
      Left = 132
      Top = 16
      Width = 45
      Height = 17
      Caption = 'Tags'
      TabOrder = 4
    end
    object ha1: TCheckBox
      Left = 184
      Top = 16
      Width = 45
      Height = 17
      Caption = 'Attrs'
      TabOrder = 5
    end
    object xt1: TCheckBox
      Left = 132
      Top = 104
      Width = 45
      Height = 17
      Caption = 'Tags'
      TabOrder = 6
    end
    object xa1: TCheckBox
      Left = 184
      Top = 104
      Width = 45
      Height = 17
      Caption = 'Attrs'
      TabOrder = 7
    end
    object xt2: TCheckBox
      Left = 132
      Top = 124
      Width = 45
      Height = 17
      Caption = 'Tags'
      TabOrder = 8
    end
    object xa2: TCheckBox
      Left = 184
      Top = 124
      Width = 45
      Height = 17
      Caption = 'Attrs'
      TabOrder = 9
    end
    object xt3: TCheckBox
      Left = 132
      Top = 144
      Width = 45
      Height = 17
      Caption = 'Tags'
      TabOrder = 10
    end
    object xa3: TCheckBox
      Left = 184
      Top = 144
      Width = 45
      Height = 17
      Caption = 'Attrs'
      TabOrder = 11
    end
    object Button1: TButton
      Left = 108
      Top = 172
      Width = 61
      Height = 25
      Caption = 'Tags as list'
      TabOrder = 12
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 172
      Width = 97
      Height = 25
      Caption = 'Tree'
      TabOrder = 13
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 172
      Top = 172
      Width = 57
      Height = 25
      Caption = 'Attrs as list'
      TabOrder = 14
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 208
      Width = 221
      Height = 25
      Caption = 'Export *.inc'
      TabOrder = 15
      OnClick = Button4Click
    end
    object ht5: TCheckBox
      Left = 132
      Top = 75
      Width = 45
      Height = 17
      Caption = 'Tags'
      Enabled = False
      TabOrder = 16
    end
    object ha5: TCheckBox
      Left = 184
      Top = 75
      Width = 45
      Height = 17
      Caption = 'Attrs'
      Enabled = False
      TabOrder = 17
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 212
    Top = 4
  end
end
