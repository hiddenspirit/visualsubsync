object PreferencesForm: TPreferencesForm
  Left = 255
  Top = 143
  Width = 600
  Height = 432
  BorderWidth = 4
  Caption = 'Preferences'
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControlPreferences: TTntPageControl
    Left = 0
    Top = 0
    Width = 576
    Height = 350
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTntTabSheet
      BorderWidth = 4
      Caption = 'General'
      object Bevel2: TBevel
        Left = 0
        Top = 49
        Width = 560
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object Bevel3: TBevel
        Left = 0
        Top = 140
        Width = 560
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object GroupBoxWebServer: TTntGroupBox
        Left = 0
        Top = 0
        Width = 560
        Height = 49
        Align = alTop
        Caption = ' Web server : '
        TabOrder = 0
        object TntLabel1: TTntLabel
          Left = 16
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Port :'
        end
        object EditServerPort: TTntEdit
          Left = 48
          Top = 20
          Width = 89
          Height = 21
          Hint = 'Port to which the webserver listen for connections'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '1'
        end
        object UpDownServerPort: TTntUpDown
          Left = 137
          Top = 20
          Width = 16
          Height = 21
          Associate = EditServerPort
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 1
          Thousands = False
        end
        object chkEnableCompression: TCheckBox
          Left = 280
          Top = 24
          Width = 121
          Height = 17
          Hint = 
            'Compress dynamic webpages for faster transmission (broken with I' +
            'E)'
          Caption = 'Enable compression'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
      end
      object GroupBoxMisc: TGroupBox
        Left = 0
        Top = 144
        Width = 560
        Height = 170
        Align = alClient
        Caption = ' Misc : '
        TabOrder = 1
        object chkAssociateExtVSSPRJ: TCheckBox
          Left = 16
          Top = 24
          Width = 529
          Height = 17
          Hint = 
            'Double click on a .vssprj file will automatically open the proje' +
            'ct in VisualSubSync'
          Caption = 
            'Associate VSS project files (*.vssprj) with VisualSubSync (admin righ' +
            'ts required)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = chkAssociateExtVSSPRJClick
        end
        object chkSwapSubList: TCheckBox
          Left = 16
          Top = 104
          Width = 529
          Height = 17
          Caption = 'Swap subtitles list'
          TabOrder = 1
        end
        object chkAssociateExtSRT: TCheckBox
          Left = 16
          Top = 40
          Width = 529
          Height = 17
          Caption = 'Associate SubRip files (*.srt) with VisualSubSync (admin rights required)'
          TabOrder = 2
          OnClick = chkAssociateExtSRTClick
        end
        object chkAssociateExtVTT: TCheckBox
          Left = 16
          Top = 56
          Width = 529
          Height = 17
          Caption = 'Associate WebVTT files (*.vtt) with VisualSubSync (admin rights required)'
          TabOrder = 3
          OnClick = chkAssociateExtVTTClick
        end
        object chkAssociateExtSSA: TCheckBox
          Left = 16
          Top = 72
          Width = 529
          Height = 17
          Caption = 'Associate SubStation Alpha files (*.ssa) with VisualSubSync (admin rights required)'
          TabOrder = 4
          OnClick = chkAssociateExtSSAClick
        end
        object chkAssociateExtASS: TCheckBox
          Left = 16
          Top = 88
          Width = 529
          Height = 17
          Caption = 'Associate Advanced SubStation Alpha files (*.ass) with VisualSubSync (admin rights required)'
          TabOrder = 5
          OnClick = chkAssociateExtASSClick
        end
        object chkLoadMostRecentProjectOnStartup: TCheckBox
          Left = 16
          Top = 120
          Width = 529
          Height = 17
          Caption = 'Load most recent project on startup'
          TabOrder = 6
        end
        object chkAlwaysGenerateSceneChangeFile: TCheckBox
          Left = 16
          Top = 136
          Width = 529
          Height = 17
          Caption = 
            'Always generate scene change file instead of extracting keyframe' +
            's'
          TabOrder = 7
        end
      end
      object GroupBoxBackup: TGroupBox
        Left = 0
        Top = 53
        Width = 560
        Height = 87
        Align = alTop
        Caption = ' Backup : '
        TabOrder = 2
        object Label6: TLabel
          Left = 16
          Top = 52
          Width = 153
          Height = 13
          Caption = 'Automatic save every (minutes) :'
        end
        object chkCreateBackup: TCheckBox
          Left = 16
          Top = 24
          Width = 185
          Height = 17
          Caption = 'Create backup file (*.bak) on save'
          TabOrder = 0
        end
        object bttOpenBackupDir: TButton
          Left = 288
          Top = 49
          Width = 90
          Height = 20
          Hint = 'Open backup directory in VisualSubSync directory'
          Caption = 'Show backups...'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bttOpenBackupDirClick
        end
        object EditBackupTime: TEdit
          Left = 176
          Top = 48
          Width = 57
          Height = 21
          TabOrder = 2
          Text = '0'
        end
        object UpDownBackupTime: TUpDown
          Left = 233
          Top = 48
          Width = 15
          Height = 21
          Associate = EditBackupTime
          TabOrder = 3
        end
        object chkAutoSaveWhenPlaying: TCheckBox
          Left = 232
          Top = 24
          Width = 169
          Height = 17
          Caption = 'Auto save when playing start'
          TabOrder = 4
        end
        object bttOpenBackupTempDir: TButton
          Left = 384
          Top = 49
          Width = 18
          Height = 20
          Hint = 'Open backup on any modification in TEMP directory'
          Caption = '2'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = bttOpenBackupTempDirClick
        end
      end
    end
    object tsSubtitle: TTntTabSheet
      Caption = 'Subtitle'
      object TntGroupBox1: TTntGroupBox
        Left = 0
        Top = 0
        Width = 568
        Height = 322
        Align = alClient
        TabOrder = 0
        DesignSize = (
          568
          322)
        object TntLabel8: TTntLabel
          Left = 16
          Top = 196
          Width = 140
          Height = 13
          Caption = 'Blank between subtitles (ms) :'
        end
        object TntLabel13: TTntLabel
          Left = 8
          Top = 16
          Width = 543
          Height = 33
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Common settings used when editing subtitles in normal or timing ' +
            'mode, and possibly when fixing error with JavaScript plug-ins.'
          WordWrap = True
        end
        object TntLabel2: TTntLabel
          Left = 16
          Top = 100
          Width = 143
          Height = 13
          Caption = 'Characters per second target :'
        end
        object TntLabel7: TTntLabel
          Left = 16
          Top = 132
          Width = 146
          Height = 13
          Caption = 'Minimum subtitle duration (ms) :'
        end
        object TntLabel14: TTntLabel
          Left = 16
          Top = 164
          Width = 149
          Height = 13
          Caption = 'Maximum subtitle duration (ms) :'
        end
        object TntLabel15: TTntLabel
          Left = 16
          Top = 68
          Width = 108
          Height = 13
          Caption = 'Reading speed target :'
        end
        object EditBlankBetweenSub: TTntEdit
          Left = 176
          Top = 192
          Width = 81
          Height = 21
          TabOrder = 4
          Text = '0'
        end
        object UpDownBlankBetweenSub: TTntUpDown
          Left = 257
          Top = 192
          Width = 15
          Height = 21
          Associate = EditBlankBetweenSub
          Max = 500
          Increment = 10
          TabOrder = 5
        end
        object EditCPSTarget: TTntEdit
          Left = 176
          Top = 96
          Width = 81
          Height = 21
          TabOrder = 1
          Text = '5'
        end
        object UpDownCPSTarget: TTntUpDown
          Left = 257
          Top = 96
          Width = 15
          Height = 21
          Associate = EditCPSTarget
          Min = 5
          Max = 50
          Position = 5
          TabOrder = 6
          Thousands = False
        end
        object EditMinimalDuration: TTntEdit
          Left = 176
          Top = 128
          Width = 81
          Height = 21
          TabOrder = 2
          Text = '500'
        end
        object UpDownMinimalDuration: TTntUpDown
          Left = 257
          Top = 128
          Width = 15
          Height = 21
          Associate = EditMinimalDuration
          Min = 200
          Max = 5000
          Increment = 100
          Position = 500
          TabOrder = 7
          Thousands = False
        end
        object EditMaximalDuration: TTntEdit
          Left = 176
          Top = 160
          Width = 81
          Height = 21
          TabOrder = 3
          Text = '6000'
        end
        object UpDownMaximumDuration: TTntUpDown
          Left = 257
          Top = 160
          Width = 16
          Height = 21
          Associate = EditMaximalDuration
          Min = 1000
          Max = 25000
          Increment = 100
          Position = 6000
          TabOrder = 8
          Thousands = False
        end
        object ComboBoxRSTarget: TTntComboBox
          Left = 176
          Top = 64
          Width = 95
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = 'Perfect'
          OnSelect = ComboHotkeyModeSelect
          Items.Strings = (
            'Perfect'
            'Good'
            'A bit fast'
            'Fast, acceptable')
        end
      end
    end
    object tsErrorChecking: TTntTabSheet
      BorderWidth = 4
      Caption = 'Error checking'
      object Bevel7: TBevel
        Left = 0
        Top = 198
        Width = 560
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object Bevel8: TBevel
        Left = 0
        Top = 106
        Width = 560
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object ListErrorChecking: TTntCheckListBox
        Left = 0
        Top = 0
        Width = 560
        Height = 106
        OnClickCheck = ListErrorCheckingClickCheck
        Align = alClient
        ItemHeight = 13
        PopupMenu = pmErrorChecking
        Sorted = True
        TabOrder = 0
        OnClick = ListErrorCheckingClick
      end
      object Panel2: TPanel
        Left = 0
        Top = 110
        Width = 560
        Height = 88
        Align = alBottom
        BevelOuter = bvLowered
        TabOrder = 1
        DesignSize = (
          560
          88)
        object TntLabel3: TTntLabel
          Left = 8
          Top = 7
          Width = 73
          Height = 13
          Caption = 'Description :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbErrorDescription: TTntLabel
          Left = 8
          Top = 21
          Width = 488
          Height = 59
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = '-'
          WordWrap = True
        end
        object TntLabel4: TTntLabel
          Left = 503
          Top = 4
          Width = 57
          Height = 13
          Alignment = taCenter
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 'Color :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Transparent = True
        end
        object ShapeErrorColor: TShape
          Left = 519
          Top = 24
          Width = 25
          Height = 25
          Anchors = [akTop, akRight]
          Shape = stCircle
        end
        object Bevel1: TBevel
          Left = 495
          Top = 8
          Width = 9
          Height = 72
          Anchors = [akTop, akRight, akBottom]
          Shape = bsRightLine
        end
      end
      object ListPluginParam: TVirtualStringTree
        Left = 0
        Top = 202
        Width = 560
        Height = 112
        Align = alBottom
        AnimationDuration = 1
        EditDelay = 40
        Header.AutoSizeIndex = 1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoVisible, hoAutoSpring]
        Header.Style = hsPlates
        HintAnimation = hatNone
        HintMode = hmHint
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toCenterScrollIntoView]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnCreateEditor = ListPluginParamCreateEditor
        OnEditing = ListPluginParamEditing
        OnGetText = ListPluginParamGetText
        OnGetHint = ListPluginParamGetHint
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 0
            Width = 214
            WideText = 'Parameter'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 1
            Width = 196
            WideText = 'Value'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 2
            Width = 150
            WideText = 'Unit'
          end>
      end
    end
    object tsHotKeys: TTntTabSheet
      BorderWidth = 4
      Caption = 'Hotkeys'
      object Bevel9: TBevel
        Left = 0
        Top = 268
        Width = 560
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object ListHotkeys: TTntListView
        Left = 0
        Top = 0
        Width = 560
        Height = 268
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 210
          end
          item
            Caption = 'Normal'
            Width = 100
          end
          item
            Caption = 'Timing'
            Width = 100
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnDeletion = ListHotkeysDeletion
        OnSelectItem = ListHotkeysSelectItem
      end
      object plHotkeysCtrl: TPanel
        Left = 0
        Top = 272
        Width = 560
        Height = 42
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object plHotkeysButtons: TPanel
          Left = 480
          Top = 0
          Width = 80
          Height = 42
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object bttResetAllHotkeys: TTntButton
            Left = 9
            Top = 1
            Width = 66
            Height = 20
            Caption = 'Reset all'
            TabOrder = 0
            OnClick = bttResetAllHotkeysClick
          end
          object bttSetHotkey: TTntButton
            Left = 9
            Top = 21
            Width = 33
            Height = 18
            Caption = 'Set'
            TabOrder = 1
            OnClick = bttSetHotkeyClick
          end
          object bttClearHotkey: TTntButton
            Left = 42
            Top = 21
            Width = 33
            Height = 18
            Caption = 'Clear'
            TabOrder = 2
            OnClick = bttClearHotkeyClick
          end
        end
        object plHotkeysEdit: TPanel
          Left = 0
          Top = 0
          Width = 480
          Height = 42
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object TntLabel5: TTntLabel
            Left = 2
            Top = 1
            Width = 33
            Height = 13
            Caption = 'Mode :'
          end
          object TntLabel6: TTntLabel
            Left = 122
            Top = 1
            Width = 80
            Height = 13
            AutoSize = False
            Caption = 'Hot key :'
          end
          object ComboHotkeyMode: TTntComboBox
            Left = 2
            Top = 16
            Width = 113
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 0
            Text = 'Normal'
            OnSelect = ComboHotkeyModeSelect
            Items.Strings = (
              'Normal'
              'Timing')
          end
          object HotKey1: THotKey
            Left = 122
            Top = 17
            Width = 255
            Height = 19
            HotKey = 0
            InvalidKeys = [hcNone]
            Modifiers = []
            TabOrder = 1
          end
        end
      end
    end
    object tsMouse: TTntTabSheet
      BorderWidth = 4
      Caption = 'Mouse'
      object Bevel4: TBevel
        Left = 0
        Top = 121
        Width = 560
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 560
        Height = 121
        Align = alTop
        Caption = ' Mouse wheel control : '
        TabOrder = 0
        object Label1: TLabel
          Left = 24
          Top = 44
          Width = 93
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Time scrolling :'
        end
        object Label2: TLabel
          Left = 24
          Top = 68
          Width = 93
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Vertical Zoom :'
        end
        object Label3: TLabel
          Left = 24
          Top = 92
          Width = 93
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Horizontal Zoom :'
        end
        object Label4: TLabel
          Left = 24
          Top = 22
          Width = 93
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'Actions'
          Color = clWindowText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object Label5: TLabel
          Left = 124
          Top = 22
          Width = 261
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'Modifiers'
          Color = clWindowText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object ComboWheelTimeScrollModifier: TComboBox
          Left = 124
          Top = 40
          Width = 261
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 0
          Items.Strings = (
            'Shift'
            'Alt'
            'Ctrl'
            'None')
        end
        object ComboWheelVZoomModifier: TComboBox
          Left = 124
          Top = 64
          Width = 261
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'Shift'
            'Alt'
            'Ctrl'
            'None')
        end
        object ComboWheelHZoomModifier: TComboBox
          Left = 124
          Top = 88
          Width = 261
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'Shift'
            'Alt'
            'Ctrl'
            'None')
        end
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 125
        Width = 560
        Height = 189
        Align = alClient
        Caption = ' Misc : '
        TabOrder = 1
        object chkEnableSSATimingMode: TCheckBox
          Left = 16
          Top = 24
          Width = 400
          Height = 17
          Caption = 
            'Left/right mouse button set start/stop time in Timing Mode (SSA ' +
            'mouse mode)'
          TabOrder = 0
        end
        object chkEnableMouseSnapping: TCheckBox
          Left = 16
          Top = 40
          Width = 400
          Height = 17
          Caption = 'Enable mouse snapping'
          TabOrder = 1
        end
        object chkEnableMouseAntiOverlapping: TCheckBox
          Left = 16
          Top = 56
          Width = 400
          Height = 17
          Caption = 'Enable mouse anti-overlapping'
          TabOrder = 2
        end
      end
    end
    object tsFonts: TTntTabSheet
      BorderWidth = 4
      Caption = 'Fonts'
      object Bevel5: TBevel
        Left = 0
        Top = 81
        Width = 560
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object TntGroupBox2: TTntGroupBox
        Left = 0
        Top = 0
        Width = 560
        Height = 81
        Align = alTop
        Caption = ' Subtitles list : '
        TabOrder = 0
        DesignSize = (
          560
          81)
        object EditSubListFont: TTntEdit
          Left = 16
          Top = 24
          Width = 455
          Height = 41
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubListFont: TTntButton
          Left = 478
          Top = 24
          Width = 33
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubListFontClick
        end
      end
      object TntGroupBox3: TTntGroupBox
        Left = 0
        Top = 85
        Width = 560
        Height = 229
        Align = alClient
        Caption = ' Subtitles text : '
        TabOrder = 1
        DesignSize = (
          560
          229)
        object EditSubTextFont: TTntEdit
          Left = 16
          Top = 24
          Width = 487
          Height = 41
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubTextFont: TTntButton
          Left = 510
          Top = 24
          Width = 33
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubTextFontClick
        end
      end
    end
    object tsTimingMode: TTntTabSheet
      BorderWidth = 4
      Caption = 'Timing mode'
      object TntGroupBox4: TTntGroupBox
        Left = 0
        Top = 0
        Width = 568
        Height = 318
        Align = alClient
        TabOrder = 0
        object chkEnableSubCreationWithSpaceKey: TCheckBox
          Left = 16
          Top = 56
          Width = 400
          Height = 17
          Caption = 'Enable subtitle creation with space key (toggle) in timing mode'
          TabOrder = 0
        end
        object chkSpaceKeyModifyTiming: TCheckBox
          Left = 48
          Top = 73
          Width = 369
          Height = 17
          Caption = 'Space key modify existing subtitles timing'
          TabOrder = 1
        end
        object chkDisableSubEditionInTimingMode: TCheckBox
          Left = 16
          Top = 24
          Width = 400
          Height = 17
          Caption = 'Disable subtitle edition in timing mode'
          TabOrder = 2
        end
      end
    end
    object tsWAVDisplay: TTntTabSheet
      BorderWidth = 4
      Caption = 'WAV display'
      object Bevel6: TBevel
        Left = 0
        Top = 121
        Width = 568
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object TntGroupBox5: TTntGroupBox
        Left = 0
        Top = 0
        Width = 568
        Height = 121
        Align = alTop
        Caption = ' Scene change : '
        TabOrder = 0
        object TntLabel9: TTntLabel
          Left = 16
          Top = 54
          Width = 96
          Height = 13
          Caption = 'Safety zone offsets :'
        end
        object TntLabel10: TTntLabel
          Left = 24
          Top = 76
          Width = 22
          Height = 13
          Caption = 'Start'
        end
        object TntLabel11: TTntLabel
          Left = 134
          Top = 76
          Width = 22
          Height = 13
          Caption = 'Stop'
        end
        object TntLabel12: TTntLabel
          Left = 256
          Top = 54
          Width = 94
          Height = 13
          Caption = 'Filter inside subtitle :'
        end
        object chkSceneChange: TCheckBox
          Left = 16
          Top = 24
          Width = 400
          Height = 17
          Caption = 'Show scene change'
          TabOrder = 0
        end
        object EditSCStartOffset: TEdit
          Left = 51
          Top = 72
          Width = 57
          Height = 21
          Hint = 'Start offset in ms'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = '130'
        end
        object UpDownSCStart: TTntUpDown
          Left = 108
          Top = 72
          Width = 15
          Height = 21
          Associate = EditSCStartOffset
          Max = 2000
          Increment = 10
          Position = 130
          TabOrder = 2
          Thousands = False
        end
        object EditSCStopOffset: TEdit
          Left = 161
          Top = 72
          Width = 57
          Height = 21
          Hint = 'Stop offset in ms'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '130'
        end
        object UpDownSCStop: TTntUpDown
          Left = 218
          Top = 72
          Width = 15
          Height = 21
          Associate = EditSCStopOffset
          Max = 2000
          Increment = 10
          Position = 130
          TabOrder = 4
          Thousands = False
        end
        object EditSCFilterOffset: TEdit
          Left = 256
          Top = 72
          Width = 57
          Height = 21
          Hint = 'Offset from start and stop in ms (0 to disable)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Text = '250'
        end
        object UpDownSCFilter: TTntUpDown
          Left = 313
          Top = 72
          Width = 15
          Height = 21
          Associate = EditSCFilterOffset
          Max = 2000
          Increment = 10
          Position = 250
          TabOrder = 6
          Thousands = False
        end
      end
      object TntGroupBox6: TTntGroupBox
        Left = 0
        Top = 125
        Width = 568
        Height = 193
        Align = alClient
        Caption = ' Misc : '
        TabOrder = 1
        object chkShowTextInWAVDisplay: TCheckBox
          Left = 16
          Top = 24
          Width = 400
          Height = 17
          Caption = 'Show subtitle text'
          TabOrder = 0
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 350
    Width = 576
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object TntPanel1: TTntPanel
      Left = 412
      Top = 0
      Width = 164
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bttOk: TTntButton
        Left = 3
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        TabOrder = 0
        OnClick = bttOkClick
      end
      object bttCancel: TTntButton
        Left = 85
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = bttCancelClick
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects, fdNoVectorFonts, fdScalableOnly]
    Left = 4
    Top = 360
  end
  object pmErrorChecking: TPopupMenu
    Left = 404
    Top = 160
    object pmiSelectAll: TMenuItem
      Caption = 'Select all'
      OnClick = pmiSelectAllClick
    end
    object pmiUnselectAll: TMenuItem
      Caption = 'Unselect all'
      OnClick = pmiUnselectAllClick
    end
  end
end
