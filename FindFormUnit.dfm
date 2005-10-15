object FindForm: TFindForm
  Left = 343
  Top = 157
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Find'
  ClientHeight = 72
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 8
    Top = 14
    Width = 26
    Height = 13
    Caption = 'Find :'
  end
  object bttClose: TTntButton
    Left = 280
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = bttCloseClick
  end
  object chkMatchCase: TTntCheckBox
    Left = 40
    Top = 36
    Width = 81
    Height = 17
    Caption = 'Match case'
    TabOrder = 1
  end
  object ComboTextToFind: TTntComboBox
    Left = 40
    Top = 10
    Width = 233
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnKeyDown = ComboTextToFindKeyDown
  end
  object bttOk: TTntButton
    Left = 280
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = TntButton1Click
  end
  object chkFromCursor: TCheckBox
    Left = 136
    Top = 36
    Width = 81
    Height = 17
    Caption = 'From cursor'
    TabOrder = 4
  end
end
