object SpellCheckForm: TSpellCheckForm
  Left = 410
  Top = 185
  Width = 440
  Height = 324
  BorderWidth = 4
  Caption = 'Spell check'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    424
    289)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 0
    Top = 16
    Width = 27
    Height = 13
    Caption = 'Text :'
  end
  object TntLabel2: TTntLabel
    Left = 0
    Top = 152
    Width = 64
    Height = 13
    Caption = 'Suggestions :'
  end
  object lblSub: TTntLabel
    Left = 0
    Top = 0
    Width = 385
    Height = 13
    Caption = 'Subtitle 0/0 : 00:00:00.000 -> 00:00:00.000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TntLabel3: TTntLabel
    Left = 0
    Top = 112
    Width = 63
    Height = 13
    Caption = 'Replace by : '
  end
  object reSubtitleText: TTntRichEdit
    Left = 0
    Top = 32
    Width = 337
    Height = 73
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object lbSuggestions: TTntListBox
    Left = 0
    Top = 168
    Width = 337
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnClick = lbSuggestionsClick
  end
  object bttReplace: TTntButton
    Left = 349
    Top = 32
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Replace'
    TabOrder = 3
    OnClick = bttReplaceClick
  end
  object bttReplaceAll: TTntButton
    Left = 349
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Replace all'
    TabOrder = 4
    OnClick = bttReplaceAllClick
  end
  object bttIgnore: TTntButton
    Left = 349
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ignore'
    TabOrder = 5
    OnClick = bttIgnoreClick
  end
  object bttIgnoreAll: TTntButton
    Left = 349
    Top = 136
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ignore all'
    TabOrder = 6
    OnClick = bttIgnoreAllClick
  end
  object bttAdd: TTntButton
    Left = 349
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 7
    OnClick = bttAddClick
  end
  object bttCancel: TTntButton
    Left = 349
    Top = 264
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 8
    OnClick = bttCancelClick
  end
  object edReplaceBy: TTntEdit
    Left = 0
    Top = 128
    Width = 337
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
end
