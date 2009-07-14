object ProjectForm: TProjectForm
  Left = 431
  Top = 316
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'New Project'
  ClientHeight = 438
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
    Top = 408
    Width = 131
    Height = 25
    Caption = 'Create new project'
    TabOrder = 3
    OnClick = bttCreateNewProjectClick
  end
  object bttCancel: TTntButton
    Left = 472
    Top = 408
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
    Height = 81
    Caption = ' Video source file (optional) : '
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
    object bttExtractWAVFromVideo: TTntButton
      Left = 8
      Top = 48
      Width = 521
      Height = 22
      Caption = 'Extract WAV/Peak file from video'
      TabOrder = 1
      OnClick = bttExtractWAVFromVideoClick
    end
  end
  object gbWAVFile: TTntGroupBox
    Left = 8
    Top = 104
    Width = 537
    Height = 105
    Caption = ' Audio waveform / Audio only preview (optional) : '
    TabOrder = 1
    object bttBrowseWAVFile: TSpeedButton
      Left = 504
      Top = 73
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseWAVFileClick
    end
    object bttBrowsePeakFile: TSpeedButton
      Left = 504
      Top = 45
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowsePeakFileClick
    end
    object EditWAVFilename: TTntEdit
      Left = 88
      Top = 74
      Width = 409
      Height = 21
      TabOrder = 0
      OnEnter = EditPeakFilenameEnter
      OnExit = EditPeakFilenameExit
    end
    object rbExternal: TRadioButton
      Tag = 1
      Left = 8
      Top = 76
      Width = 73
      Height = 17
      Hint = 
        'Use an external or extracted WAV file for waveform and audio onl' +
        'y preview.'
      Caption = 'WAV file :'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = rbInternalWAVClick
    end
    object rbPeakOnly: TRadioButton
      Tag = 2
      Left = 8
      Top = 48
      Width = 73
      Height = 17
      Hint = 
        'Use the video file for audio preview and a created peak file for' +
        ' the waveform display.'
      Caption = 'Peak file :'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = rbInternalWAVClick
    end
    object EditPeakFilename: TTntEdit
      Left = 88
      Top = 46
      Width = 409
      Height = 21
      TabOrder = 3
      OnEnter = EditPeakFilenameEnter
      OnExit = EditPeakFilenameExit
    end
    object rbNoWaveform: TTntRadioButton
      Left = 8
      Top = 24
      Width = 129
      Height = 17
      Hint = 
        'If available the audio track of the video file will be used for ' +
        'preview but no waveform is displayed.'
      Caption = 'No waveform display'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = rbInternalWAVClick
    end
  end
  object gbSubtitleFile: TTntGroupBox
    Left = 8
    Top = 224
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
      Width = 409
      Height = 21
      TabOrder = 0
    end
    object cbSubtitleFormat: TTntComboBox
      Left = 424
      Top = 16
      Width = 73
      Height = 21
      Hint = 'Subtitle file format'
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'SRT'
      OnChange = cbSubtitleFormatChange
      Items.Strings = (
        'SRT'
        'SSA'
        'ASS')
    end
  end
  object gbProjectFile: TTntGroupBox
    Left = 8
    Top = 344
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
    Top = 408
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 6
    Visible = False
    OnClick = bttOkClick
  end
  object chkSaveAsUTF8: TTntCheckBox
    Left = 112
    Top = 223
    Width = 92
    Height = 17
    Caption = 'Save as UTF8'
    TabOrder = 7
  end
  object gbVO: TTntGroupBox
    Left = 8
    Top = 280
    Width = 537
    Height = 49
    Caption = 'Reference VO : '
    TabOrder = 8
    object bttBrowseSubtitleVO: TSpeedButton
      Left = 504
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseVOFileClick
    end
    object EditSubtitleVO: TTntEdit
      Left = 8
      Top = 16
      Width = 489
      Height = 21
      TabOrder = 0
    end
  end
  object TntOpenDialog1: TTntOpenDialog
    Left = 40
    Top = 400
  end
end
