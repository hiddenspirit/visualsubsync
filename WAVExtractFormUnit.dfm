object ExtractWAVForm: TExtractWAVForm
  Left = 265
  Top = 90
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Extract WAV'
  ClientHeight = 297
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel3: TTntLabel
    Left = 0
    Top = 246
    Width = 47
    Height = 13
    Caption = 'Progress :'
  end
  object TntGroupBox1: TTntGroupBox
    Left = 0
    Top = 0
    Width = 425
    Height = 113
    Caption = ' Video info : '
    TabOrder = 0
    object MemoVideoInfo: TTntMemo
      Left = 8
      Top = 16
      Width = 409
      Height = 89
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object bttExtract: TTntButton
    Left = 0
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Extract'
    TabOrder = 1
    OnClick = bttExtractClick
  end
  object bttClose: TTntButton
    Left = 352
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = bttCloseClick
  end
  object ProgressBar1: TProgressBar
    Left = 56
    Top = 240
    Width = 369
    Height = 25
    Max = 1000
    Smooth = True
    TabOrder = 3
  end
  object gbSettings: TTntGroupBox
    Left = 0
    Top = 120
    Width = 425
    Height = 113
    Caption = ' Settings : '
    TabOrder = 4
    object TntLabel1: TTntLabel
      Left = 8
      Top = 24
      Width = 86
      Height = 13
      Caption = 'Stream to extract :'
    end
    object cbStreamIndex: TComboBox
      Left = 104
      Top = 20
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object rbFastConversion: TRadioButton
      Tag = 2
      Left = 8
      Top = 88
      Width = 401
      Height = 17
      Caption = 'Convert to reduce file size (Mono + Sample rate divided by 2)'
      Checked = True
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      TabStop = True
      OnClick = rbOnlyPeakClick
    end
    object rbNoConversion: TRadioButton
      Tag = 1
      Left = 8
      Top = 68
      Width = 113
      Height = 17
      Caption = 'Simple extraction'
      TabOrder = 2
      OnClick = rbOnlyPeakClick
    end
    object rbOnlyPeak: TRadioButton
      Left = 8
      Top = 48
      Width = 137
      Height = 17
      Caption = 'Only create peak file'
      TabOrder = 3
      OnClick = rbOnlyPeakClick
    end
  end
  object bttStop: TTntButton
    Left = 80
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = bttStopClick
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 368
    Top = 24
  end
end
