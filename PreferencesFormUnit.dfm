object PreferencesForm: TPreferencesForm
  Left = 204
  Top = 215
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Preferences'
  ClientHeight = 305
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TntPageControl1: TTntPageControl
    Left = 0
    Top = 0
    Width = 353
    Height = 273
    ActivePage = tsHotKeys
    TabOrder = 0
    object tsGeneral: TTntTabSheet
      BorderWidth = 2
      Caption = 'General'
      object TntGroupBox1: TTntGroupBox
        Left = 0
        Top = 0
        Width = 341
        Height = 49
        Caption = ' Web server : '
        TabOrder = 0
        object TntLabel1: TTntLabel
          Left = 16
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Port :'
        end
        object EditServerPort: TTntEdit
          Left = 48
          Top = 20
          Width = 89
          Height = 21
          Hint = 'Port to which the webserver listen for connections'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '1'
          OnKeyPress = EditDigitOnlyKeyPress
        end
        object UpDownServerPort: TTntUpDown
          Left = 137
          Top = 20
          Width = 16
          Height = 21
          Associate = EditServerPort
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 1
          Thousands = False
        end
        object chkEnableCompression: TCheckBox
          Left = 208
          Top = 24
          Width = 121
          Height = 17
          Hint = 'Compress dynamic webpages for faster transmission'
          Caption = 'Enable compression'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 56
        Width = 341
        Height = 185
        Caption = ' Misc : '
        TabOrder = 1
        object chkAssociateExt: TCheckBox
          Left = 16
          Top = 24
          Width = 305
          Height = 17
          Hint = 
            'Double click on a .vssprj file will automatically open the proje' +
            'ct in VisualSubSync'
          Caption = 'Associate project file (*.vssprj) with VisualSubSync'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = chkAssociateExtClick
        end
        object chkSwapSubList: TCheckBox
          Left = 16
          Top = 40
          Width = 113
          Height = 17
          Caption = 'Swap subtitles list'
          TabOrder = 1
        end
      end
    end
    object tsErrorChecking: TTntTabSheet
      Caption = 'Error checking'
      object TntBevel1: TTntBevel
        Left = 8
        Top = 176
        Width = 329
        Height = 60
      end
      object TntLabel2: TTntLabel
        Left = 8
        Top = 148
        Width = 33
        Height = 13
        Caption = 'Value :'
      end
      object lbErrorUnit: TTntLabel
        Left = 184
        Top = 148
        Width = 89
        Height = 13
        AutoSize = False
        Caption = '-'
      end
      object lbErrorDescription: TTntLabel
        Left = 16
        Top = 200
        Width = 313
        Height = 31
        AutoSize = False
        Caption = '-'
        WordWrap = True
      end
      object TntLabel3: TTntLabel
        Left = 16
        Top = 184
        Width = 73
        Height = 13
        Caption = 'Description :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ShapeErrorColor: TShape
        Left = 314
        Top = 147
        Width = 15
        Height = 15
        Shape = stCircle
      end
      object TntLabel4: TTntLabel
        Left = 280
        Top = 148
        Width = 30
        Height = 13
        Caption = 'Color :'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object ListErrorChecking: TTntCheckListBox
        Left = 8
        Top = 8
        Width = 329
        Height = 129
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListErrorCheckingClick
      end
      object EditErrorValue: TTntEdit
        Left = 48
        Top = 144
        Width = 129
        Height = 21
        TabOrder = 1
        OnChange = EditErrorValueChange
        OnKeyPress = EditDigitOnlyKeyPress
      end
    end
    object tsHotKeys: TTntTabSheet
      Caption = 'HotKeys'
    end
  end
  object bttOk: TTntButton
    Left = 200
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = bttOkClick
  end
  object bttCancel: TTntButton
    Left = 280
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = bttCancelClick
  end
end
