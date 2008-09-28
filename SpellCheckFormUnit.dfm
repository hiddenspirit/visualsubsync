object SpellCheckForm: TSpellCheckForm
  Left = 410
  Top = 185
  Width = 480
  Height = 350
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
  PixelsPerInch = 96
  TextHeight = 13
  object TntBevel1: TTntBevel
    Left = 384
    Top = 0
    Width = 5
    Height = 315
    Align = alRight
    Shape = bsSpacer
  end
  object TntPanel1: TTntPanel
    Left = 389
    Top = 0
    Width = 75
    Height = 315
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object bttReplace: TTntButton
      Left = 0
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Replace'
      TabOrder = 0
      OnClick = bttReplaceClick
    end
    object bttReplaceAll: TTntButton
      Left = 0
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Replace all'
      TabOrder = 1
      OnClick = bttReplaceAllClick
    end
    object bttIgnore: TTntButton
      Left = 0
      Top = 104
      Width = 75
      Height = 25
      Caption = 'Ignore'
      TabOrder = 2
      OnClick = bttIgnoreClick
    end
    object bttIgnoreAll: TTntButton
      Left = 0
      Top = 136
      Width = 75
      Height = 25
      Caption = 'Ignore all'
      TabOrder = 3
      OnClick = bttIgnoreAllClick
    end
    object bttAdd: TTntButton
      Left = 0
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 4
      OnClick = bttAddClick
    end
    object TntPanel3: TTntPanel
      Left = 0
      Top = 288
      Width = 75
      Height = 27
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      DesignSize = (
        75
        27)
      object bttCancel: TTntButton
        Left = 0
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Close'
        TabOrder = 0
        OnClick = bttCancelClick
      end
    end
  end
  object TntPanel2: TTntPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 315
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object TntLabel1: TTntLabel
      Left = 0
      Top = 19
      Width = 384
      Height = 13
      Align = alTop
      Caption = 'Text :'
    end
    object TntLabel3: TTntLabel
      Left = 0
      Top = 115
      Width = 384
      Height = 13
      Align = alTop
      Caption = 'Replace by : '
    end
    object TntLabel2: TTntLabel
      Left = 0
      Top = 159
      Width = 384
      Height = 13
      Align = alTop
      Caption = 'Suggestions :'
    end
    object lblSub: TTntLabel
      Left = 0
      Top = 0
      Width = 384
      Height = 13
      Align = alTop
      Caption = 'Subtitle 0/0 : 00:00:00.000 -> 00:00:00.000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TntBevel2: TTntBevel
      Left = 0
      Top = 13
      Width = 384
      Height = 6
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel3: TTntBevel
      Left = 0
      Top = 32
      Width = 384
      Height = 4
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel4: TTntBevel
      Left = 0
      Top = 109
      Width = 384
      Height = 6
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel5: TTntBevel
      Left = 0
      Top = 128
      Width = 384
      Height = 4
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel6: TTntBevel
      Left = 0
      Top = 153
      Width = 384
      Height = 6
      Align = alTop
      Shape = bsSpacer
    end
    object TntBevel7: TTntBevel
      Left = 0
      Top = 172
      Width = 384
      Height = 4
      Align = alTop
      Shape = bsSpacer
    end
    object reSubtitleText: TTntRichEdit
      Left = 0
      Top = 36
      Width = 384
      Height = 73
      TabStop = False
      Align = alTop
      ReadOnly = True
      TabOrder = 0
    end
    object edReplaceBy: TTntEdit
      Left = 0
      Top = 132
      Width = 384
      Height = 21
      Align = alTop
      TabOrder = 1
    end
    object lbSuggestions: TTntListBox
      Left = 0
      Top = 176
      Width = 384
      Height = 139
      Align = alClient
      ItemHeight = 13
      TabOrder = 2
      OnClick = lbSuggestionsClick
    end
  end
end
