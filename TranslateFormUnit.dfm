object TranslateForm: TTranslateForm
  Left = 306
  Top = 321
  Width = 600
  Height = 274
  Caption = 'Translate'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object bttOK: TTntButton
    Left = 432
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = bttOKClick
  end
  object bttCancel: TTntButton
    Left = 512
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = bttCancelClick
  end
  object rgSelectionType: TTntRadioGroup
    Left = 8
    Top = 8
    Width = 577
    Height = 81
    Caption = ' Subtitles to translate '
    Items.Strings = (
      'All (create a new file)'
      'Missing only (enrich current file)')
    TabOrder = 2
    OnClick = rgSelectionTypeClick
  end
  object rgTextType: TTntRadioGroup
    Left = 8
    Top = 96
    Width = 577
    Height = 113
    Caption = ' New subtitle text '
    Items.Strings = (
      'Empty'
      'Copy original text'
      'Copy tags only'
      'Custom text')
    TabOrder = 3
    OnClick = rgTextTypeClick
  end
  object edCustomText: TTntEdit
    Left = 152
    Top = 179
    Width = 425
    Height = 21
    TabOrder = 4
    Text = '** untranslated **'
  end
  object edTargetFile: TTntEdit
    Left = 144
    Top = 26
    Width = 401
    Height = 21
    TabOrder = 5
  end
  object bttBrowseTargetFile: TTntButton
    Left = 544
    Top = 24
    Width = 35
    Height = 25
    Caption = '...'
    TabOrder = 6
    OnClick = bttBrowseTargetFileClick
  end
  object TntSaveDialog1: TTntSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 552
    Top = 56
  end
end
