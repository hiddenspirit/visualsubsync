object SilentZoneForm: TSilentZoneForm
  Left = 246
  Top = 77
  Width = 314
  Height = 331
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = 'Silent zones'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TntBevel1: TTntBevel
    Left = 0
    Top = 49
    Width = 298
    Height = 4
    Align = alTop
    Shape = bsSpacer
  end
  object lbSilentZones: TTntListBox
    Left = 0
    Top = 53
    Width = 298
    Height = 243
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbSilentZonesDblClick
  end
  object TntGroupBox1: TTntGroupBox
    Left = 0
    Top = 0
    Width = 298
    Height = 49
    Align = alTop
    Caption = ' Settings '
    TabOrder = 1
    DesignSize = (
      298
      49)
    object TntLabel1: TTntLabel
      Left = 8
      Top = 20
      Width = 50
      Height = 13
      Caption = 'Threshold:'
    end
    object TntLabel2: TTntLabel
      Left = 112
      Top = 20
      Width = 43
      Height = 13
      Caption = 'Duration:'
    end
    object edThreshold: TTntEdit
      Left = 64
      Top = 16
      Width = 41
      Height = 21
      TabOrder = 0
      Text = '200'
    end
    object bttUpdate: TTntButton
      Left = 228
      Top = 16
      Width = 59
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Update'
      TabOrder = 1
      OnClick = bttUpdateClick
    end
    object edDuration: TTntEdit
      Left = 160
      Top = 16
      Width = 41
      Height = 21
      TabOrder = 2
      Text = '500'
    end
  end
end
