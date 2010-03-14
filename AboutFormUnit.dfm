object AboutForm: TAboutForm
  Left = 344
  Top = 148
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'About...'
  ClientHeight = 265
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object TntLabel1: TTntLabel
    Left = 0
    Top = 0
    Width = 345
    Height = 33
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'VisualSubSync'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -24
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = TntLabel1Click
  end
  object Bevel1: TBevel
    Left = 0
    Top = 32
    Width = 345
    Height = 9
    Shape = bsBottomLine
  end
  object TntLabel2: TTntLabel
    Left = 8
    Top = 72
    Width = 109
    Height = 14
    Caption = 'Copyright 2003-2010'
  end
  object TntLabel3: TTntLabel
    Left = 8
    Top = 88
    Width = 137
    Height = 14
    Cursor = crHandPoint
    Caption = 'christophe.paris@free.fr'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = TntLabel3Click
  end
  object LabelVersion: TTntLabel
    Left = 8
    Top = 48
    Width = 98
    Height = 19
    Caption = 'Version 0.0.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 0
    Top = 224
    Width = 345
    Height = 9
    Shape = bsBottomLine
  end
  object TntLabel4: TTntLabel
    Left = 8
    Top = 120
    Width = 289
    Height = 14
    AutoSize = False
    Caption = 'Thanks to the authors of the following components :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblVTV: TTntLabel
    Left = 8
    Top = 136
    Width = 217
    Height = 14
    AutoSize = False
    Caption = '- Virtual Treeview by Mike Lischke'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object TntLabel6: TTntLabel
    Left = 8
    Top = 152
    Width = 217
    Height = 14
    AutoSize = False
    Caption = '- Tnt Unicode Controls by Troy Wolbrink'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblJS: TTntLabel
    Left = 8
    Top = 168
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- JavaScript Bridge / SpiderMonkey JavaScript Engine'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object TntLabel8: TTntLabel
    Left = 8
    Top = 184
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- VSFilter by Gabest'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object TntLabel5: TTntLabel
    Left = 8
    Top = 200
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- Hunspell by L'#225'szl'#243' N'#233'meth'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object bttOk: TTntButton
    Left = 8
    Top = 240
    Width = 329
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = bttOkClick
  end
  object pnlCrashTest: TPanel
    Left = 328
    Top = 208
    Width = 17
    Height = 17
    BevelOuter = bvNone
    TabOrder = 1
    OnClick = panelCrashTestClick
  end
end
