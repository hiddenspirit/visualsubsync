object SpellCheckForm: TSpellCheckForm
  Left = 410
  Top = 185
  Width = 403
  Height = 284
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
  OnShow = FormShow
  DesignSize = (
    387
    249)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 0
    Top = 0
    Width = 27
    Height = 13
    Caption = 'Text :'
  end
  object TntLabel2: TTntLabel
    Left = 0
    Top = 96
    Width = 64
    Height = 13
    Caption = 'Suggestions :'
  end
  object reSubtitleText: TTntRichEdit
    Left = 0
    Top = 16
    Width = 297
    Height = 73
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object lbSuggestions: TTntListBox
    Left = 0
    Top = 112
    Width = 297
    Height = 137
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object bttReplace: TTntButton
    Left = 312
    Top = 16
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Replace'
    TabOrder = 2
    OnClick = bttReplaceClick
  end
  object bttReplaceAll: TTntButton
    Left = 312
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Replace all'
    TabOrder = 3
  end
  object bttIgnore: TTntButton
    Left = 312
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ignore'
    TabOrder = 4
    OnClick = bttIgnoreClick
  end
  object bttIgnoreAll: TTntButton
    Left = 312
    Top = 120
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ignore all'
    TabOrder = 5
    OnClick = bttIgnoreAllClick
  end
  object bttAdd: TTntButton
    Left = 312
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 7
    OnClick = bttAddClick
  end
  object bttCancel: TTntButton
    Left = 312
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 6
    OnClick = bttCancelClick
  end
end
