object ProjectForm: TProjectForm
  Left = 226
  Top = 175
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'New Project'
  ClientHeight = 332
  ClientWidth = 550
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
  object bttCreateNewProject: TTntButton
    Left = 336
    Top = 304
    Width = 131
    Height = 25
    Caption = 'Create new project'
    TabOrder = 3
    OnClick = bttCreateNewProjectClick
  end
  object bttCancel: TTntButton
    Left = 472
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = bttCancelClick
  end
  object gbVideoFile: TTntGroupBox
    Left = 8
    Top = 8
    Width = 537
    Height = 49
    Caption = ' Video source file : '
    TabOrder = 0
    object bttBrowseVideoFile: TSpeedButton
      Left = 504
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseVideoFileClick
    end
    object EditVideoFilename: TTntEdit
      Left = 8
      Top = 17
      Width = 489
      Height = 21
      TabOrder = 0
    end
  end
  object gbWAVFile: TTntGroupBox
    Left = 8
    Top = 64
    Width = 537
    Height = 113
    Caption = ' WAV file : '
    TabOrder = 1
    object bttBrowseWAVFile: TSpeedButton
      Left = 504
      Top = 49
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseWAVFileClick
    end
    object bttBrowsePeakFile: TSpeedButton
      Left = 504
      Top = 77
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowsePeakFileClick
    end
    object bttExtractWAVFromVideo: TTntButton
      Left = 104
      Top = 20
      Width = 161
      Height = 25
      Caption = 'Extract from video now'
      TabOrder = 1
      OnClick = bttExtractWAVFromVideoClick
    end
    object EditWAVFilename: TTntEdit
      Left = 104
      Top = 50
      Width = 393
      Height = 21
      TabOrder = 0
    end
    object rbInternalWAV: TRadioButton
      Left = 8
      Top = 24
      Width = 65
      Height = 17
      Caption = 'Internal :'
      TabOrder = 2
      OnClick = rbInternalWAVClick
    end
    object rbExternal: TRadioButton
      Tag = 1
      Left = 8
      Top = 52
      Width = 65
      Height = 17
      Caption = 'External :'
      TabOrder = 3
      OnClick = rbInternalWAVClick
    end
    object rbPeakOnly: TRadioButton
      Tag = 2
      Left = 8
      Top = 80
      Width = 89
      Height = 17
      Caption = 'Peak file only :'
      TabOrder = 4
      OnClick = rbInternalWAVClick
    end
    object EditPeakFilename: TTntEdit
      Left = 104
      Top = 78
      Width = 393
      Height = 21
      TabOrder = 5
    end
  end
  object gbSubtitleFile: TTntGroupBox
    Left = 8
    Top = 184
    Width = 537
    Height = 49
    Caption = ' Subtitle file : '
    TabOrder = 2
    object bttBrowseSubtitleFile: TSpeedButton
      Left = 504
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseSubtitleFileClick
    end
    object EditSubtitleFilename: TTntEdit
      Left = 8
      Top = 17
      Width = 489
      Height = 21
      TabOrder = 0
    end
  end
  object gbProjectFile: TTntGroupBox
    Left = 8
    Top = 248
    Width = 537
    Height = 49
    Caption = ' Project file : '
    TabOrder = 5
    object bttBrowseProjectFile: TSpeedButton
      Left = 504
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseProjectFileClick
    end
    object EditProjectFilename: TTntEdit
      Left = 8
      Top = 17
      Width = 489
      Height = 21
      TabOrder = 0
    end
  end
  object bttOk: TTntButton
    Left = 392
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 6
    Visible = False
    OnClick = bttOkClick
  end
  object chkSaveAsUTF8: TTntCheckBox
    Left = 112
    Top = 183
    Width = 92
    Height = 17
    Caption = 'Save as UTF8'
    TabOrder = 7
  end
  object TntOpenDialog1: TTntOpenDialog
    Left = 288
    Top = 304
  end
end
