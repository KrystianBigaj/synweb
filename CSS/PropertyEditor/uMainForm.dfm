object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'SynWeb Css Property editor'
  ClientHeight = 453
  ClientWidth = 632
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    632
    453)
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 8
    Top = 8
    Width = 616
    Height = 437
    ActivePage = tsEditor
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsEditor: TTabSheet
      Caption = 'Editor'
      object gbProperties: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 206
        Height = 403
        Align = alLeft
        Caption = 'Properties'
        TabOrder = 0
        object lbProperties: TListBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 196
          Height = 218
          Align = alClient
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnClick = lbPropertiesClick
        end
        object gbAddProperties: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 272
          Width = 196
          Height = 126
          Align = alBottom
          Caption = 'Add multiple properties'
          TabOrder = 1
          object btnAddProperties: TButton
            AlignWithMargins = True
            Left = 5
            Top = 96
            Width = 186
            Height = 25
            Align = alBottom
            Caption = 'Add properites with current values'
            TabOrder = 0
            WordWrap = True
            OnClick = btnAddPropertiesClick
          end
          object mProperties: TMemo
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 186
            Height = 72
            Align = alClient
            TabOrder = 1
          end
        end
        object btnRemoveProperty: TButton
          AlignWithMargins = True
          Left = 5
          Top = 242
          Width = 196
          Height = 24
          Align = alBottom
          Caption = 'Remove selected proeprty'
          TabOrder = 2
          WordWrap = True
          OnClick = btnRemovePropertyClick
        end
      end
      object panClient: TPanel
        Left = 212
        Top = 0
        Width = 396
        Height = 409
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object gbValues: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 199
          Width = 390
          Height = 207
          Align = alClient
          Caption = 'Allowed identifier values (each value in separate line)'
          TabOrder = 0
          object mValues: TMemo
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 380
            Height = 184
            Align = alClient
            TabOrder = 0
            OnChange = mValuesChange
          end
        end
        object gbFlags: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 390
          Height = 190
          Align = alTop
          Caption = 'SynWeb Css property flags'
          TabOrder = 1
          object panFlags: TPanel
            Left = 2
            Top = 15
            Width = 386
            Height = 173
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lblFlags: TLabel
              Left = 11
              Top = 148
              Width = 78
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Flags:'
            end
            object cbFlag31: TCheckBox
              Tag = 31
              Left = 11
              Top = 8
              Width = 146
              Height = 17
              Caption = '31: angle'
              TabOrder = 0
              OnClick = cbFlagClick
            end
            object cbFlag30: TCheckBox
              Tag = 30
              Left = 11
              Top = 25
              Width = 146
              Height = 17
              Caption = '30: frequrency'
              TabOrder = 1
              OnClick = cbFlagClick
            end
            object cbFlag29: TCheckBox
              Tag = 29
              Left = 11
              Top = 41
              Width = 146
              Height = 17
              Caption = '29: time'
              TabOrder = 2
              OnClick = cbFlagClick
            end
            object cbFlag28: TCheckBox
              Tag = 28
              Left = 11
              Top = 57
              Width = 146
              Height = 17
              Caption = '28: length'
              TabOrder = 3
              OnClick = cbFlagClick
            end
            object cbFlag27: TCheckBox
              Tag = 27
              Left = 11
              Top = 73
              Width = 146
              Height = 17
              Caption = '27: length-negative'
              TabOrder = 4
              OnClick = cbFlagClick
            end
            object cbFlag26: TCheckBox
              Tag = 26
              Left = 11
              Top = 89
              Width = 146
              Height = 17
              Caption = '26: percentage'
              TabOrder = 5
              OnClick = cbFlagClick
            end
            object cbFlag24: TCheckBox
              Tag = 24
              Left = 11
              Top = 122
              Width = 146
              Height = 17
              Caption = '24: number'
              TabOrder = 6
              OnClick = cbFlagClick
            end
            object cbFlag23: TCheckBox
              Tag = 23
              Left = 163
              Top = 8
              Width = 146
              Height = 17
              Caption = '23: number-negative'
              TabOrder = 7
              OnClick = cbFlagClick
            end
            object cbFlag22: TCheckBox
              Tag = 22
              Left = 163
              Top = 25
              Width = 146
              Height = 17
              Caption = '22: integer'
              TabOrder = 8
              OnClick = cbFlagClick
            end
            object cbFlag21: TCheckBox
              Tag = 21
              Left = 163
              Top = 41
              Width = 146
              Height = 17
              Caption = '21: integer-negative'
              TabOrder = 9
              OnClick = cbFlagClick
            end
            object cbFlag20: TCheckBox
              Tag = 20
              Left = 163
              Top = 57
              Width = 146
              Height = 17
              Caption = '20: any identifier allowed'
              TabOrder = 10
              OnClick = cbFlagClick
            end
            object cbFlag19: TCheckBox
              Tag = 19
              Left = 163
              Top = 73
              Width = 146
              Height = 17
              Caption = '19: String allowed'
              TabOrder = 11
              OnClick = cbFlagClick
            end
            object cbFlag18: TCheckBox
              Tag = 18
              Left = 163
              Top = 89
              Width = 146
              Height = 17
              Caption = '18: color values allowed'
              TabOrder = 12
              OnClick = cbFlagClick
            end
            object cbFlag17: TCheckBox
              Tag = 17
              Left = 163
              Top = 106
              Width = 146
              Height = 17
              Caption = '17: 100-900 allowed'
              TabOrder = 13
              OnClick = cbFlagClick
            end
            object cbFlag16: TCheckBox
              Tag = 16
              Left = 163
              Top = 122
              Width = 146
              Height = 17
              Caption = '16: '#39','#39' allowed'
              TabOrder = 14
              OnClick = cbFlagClick
            end
            object edtFlags: TEdit
              Left = 95
              Top = 145
              Width = 62
              Height = 21
              TabOrder = 15
              Text = '$00000000'
              OnChange = edtFlagsChange
            end
            object cbFlag25: TCheckBox
              Tag = 25
              Left = 11
              Top = 106
              Width = 146
              Height = 17
              Caption = '25: percentage-negative'
              TabOrder = 16
              OnClick = cbFlagClick
            end
          end
        end
      end
    end
    object tsPreview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 1
    end
  end
end
