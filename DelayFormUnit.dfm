object DelayForm: TDelayForm
  Left = 378
  Top = 223
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Delay'
  ClientHeight = 221
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object meDelay: TMaskEdit
    Left = 16
    Top = 60
    Width = 193
    Height = 21
    EditMask = '!99:99:99.999;1;0'
    MaxLength = 12
    TabOrder = 0
    Text = '  :  :  .   '
  end
  object rgApplyTo: TTntRadioGroup
    Left = 16
    Top = 88
    Width = 193
    Height = 81
    Caption = ' Apply to '
    ItemIndex = 0
    Items.WideStrings = (
      'All subtitles'
      'Selected subtitles'
      'All subtitles from cursor to end')
    TabOrder = 1
  end
  object rgType: TTntRadioGroup
    Left = 16
    Top = 8
    Width = 193
    Height = 41
    Caption = ' Type : '
    Columns = 2
    ItemIndex = 0
    Items.WideStrings = (
      'Positive (+)'
      'Negative (-)')
    TabOrder = 2
  end
  object bttApply: TTntButton
    Left = 24
    Top = 184
    Width = 81
    Height = 25
    Caption = 'Apply'
    TabOrder = 3
    OnClick = bttApplyClick
  end
  object bttCancel: TTntButton
    Left = 120
    Top = 184
    Width = 83
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = bttCancelClick
  end
end
