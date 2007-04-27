object StyleForm: TStyleForm
  Left = 253
  Top = 98
  Width = 623
  Height = 418
  Caption = 'SSA/ASS Style Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 232
    Top = 104
    Width = 185
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Horizontal'
    Color = clBtnText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel2: TTntLabel
    Left = 424
    Top = 104
    Width = 185
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Vertical'
    Color = clBtnText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel3: TTntLabel
    Left = 232
    Top = 224
    Width = 377
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Colors'
    Color = clBtnText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel4: TTntLabel
    Left = 232
    Top = 40
    Width = 377
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Font'
    Color = clBtnText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel5: TTntLabel
    Left = 232
    Top = 296
    Width = 377
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Border && Shadow'
    Color = clBtnText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlightText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
  end
  object TntLabel6: TTntLabel
    Left = 232
    Top = 240
    Width = 40
    Height = 13
    Caption = 'Primary :'
  end
  object TntLabel7: TTntLabel
    Left = 328
    Top = 240
    Width = 57
    Height = 13
    Caption = 'Secondary :'
  end
  object TntLabel8: TTntLabel
    Left = 424
    Top = 240
    Width = 39
    Height = 13
    Caption = 'Outline :'
  end
  object TntLabel9: TTntLabel
    Left = 520
    Top = 240
    Width = 31
    Height = 13
    Caption = 'Back :'
  end
  object TntLabel10: TTntLabel
    Left = 232
    Top = 12
    Width = 58
    Height = 13
    Caption = 'Style name :'
  end
  object TntLabel11: TTntLabel
    Left = 232
    Top = 316
    Width = 39
    Height = 13
    Caption = 'Outline :'
  end
  object TntLabel12: TTntLabel
    Left = 432
    Top = 316
    Width = 45
    Height = 13
    Caption = 'Shadow :'
  end
  object lstStyles: TTntListBox
    Left = 8
    Top = 8
    Width = 217
    Height = 345
    ItemHeight = 13
    TabOrder = 0
    OnClick = lstStylesClick
  end
  object stFontPreview: TTntStaticText
    Left = 232
    Top = 56
    Width = 297
    Height = 41
    AutoSize = False
    BevelKind = bkSoft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object bttFont: TTntButton
    Left = 536
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 2
    OnClick = bttFontClick
  end
  object rgVAlignment: TTntRadioGroup
    Left = 424
    Top = 120
    Width = 185
    Height = 41
    Caption = ' Alignment : '
    Columns = 3
    Items.Strings = (
      'Top'
      'Middle'
      'Bottom')
    TabOrder = 3
    OnClick = checkChangedSender
  end
  object TntGroupBox2: TTntGroupBox
    Left = 232
    Top = 168
    Width = 185
    Height = 49
    Caption = ' Margin : '
    TabOrder = 4
    object edHMargin: TTntEdit
      Left = 8
      Top = 20
      Width = 169
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object rgHAlignment: TTntRadioGroup
    Left = 232
    Top = 120
    Width = 185
    Height = 41
    Caption = ' Alignment : '
    Columns = 3
    Items.Strings = (
      'Left'
      'Center'
      'Right')
    TabOrder = 5
    OnClick = checkChangedSender
  end
  object TntGroupBox1: TTntGroupBox
    Left = 424
    Top = 168
    Width = 185
    Height = 49
    Caption = ' Margin : '
    TabOrder = 6
    object edVMargin: TTntEdit
      Left = 8
      Top = 20
      Width = 169
      Height = 21
      TabOrder = 0
      OnChange = checkChangedSender
    end
  end
  object edOutline: TTntEdit
    Left = 280
    Top = 312
    Width = 121
    Height = 21
    TabOrder = 7
    OnChange = checkChangedSender
  end
  object bttNew: TTntButton
    Left = 8
    Top = 360
    Width = 49
    Height = 25
    Caption = 'New'
    TabOrder = 8
    OnClick = bttNewClick
  end
  object bttDelete: TTntButton
    Left = 120
    Top = 360
    Width = 49
    Height = 25
    Caption = 'Delete'
    TabOrder = 9
    OnClick = bttDeleteClick
  end
  object bttClose: TTntButton
    Left = 536
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 10
    OnClick = bttCloseClick
  end
  object pnlPrimaryColor: TTntStaticText
    Left = 232
    Top = 256
    Width = 89
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 11
    OnClick = pnlColorClick
  end
  object pnlSecondaryColor: TTntStaticText
    Left = 328
    Top = 256
    Width = 89
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 12
    OnClick = pnlColorClick
  end
  object pnlOutlineColor: TTntStaticText
    Left = 424
    Top = 256
    Width = 89
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 13
    OnClick = pnlColorClick
  end
  object pnlBackColor: TTntStaticText
    Left = 520
    Top = 256
    Width = 89
    Height = 25
    AutoSize = False
    BevelKind = bkTile
    Color = clBlack
    ParentColor = False
    TabOrder = 14
    OnClick = pnlColorClick
  end
  object bttCopy: TTntButton
    Left = 64
    Top = 360
    Width = 49
    Height = 25
    Caption = 'Copy'
    TabOrder = 15
    OnClick = bttCopyClick
  end
  object edStyleName: TTntEdit
    Left = 304
    Top = 8
    Width = 305
    Height = 21
    TabOrder = 16
    OnChange = checkChangedSender
  end
  object edShadow: TTntEdit
    Left = 488
    Top = 312
    Width = 121
    Height = 21
    TabOrder = 17
    OnChange = checkChangedSender
  end
  object bttApply: TTntButton
    Left = 232
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 18
    OnClick = bttApplyClick
  end
  object bttReset: TTntButton
    Left = 312
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 19
    OnClick = bttResetClick
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects, fdScalableOnly]
    Left = 152
    Top = 264
  end
  object ColorDialog1: TColorDialog
    Left = 152
    Top = 296
  end
end
