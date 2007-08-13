object PreferencesForm: TPreferencesForm
  Left = 254
  Top = 141
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Preferences'
  ClientHeight = 365
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TntPageControl1: TTntPageControl
    Left = 0
    Top = 0
    Width = 434
    Height = 324
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTntTabSheet
      BorderWidth = 4
      Caption = 'General'
      object TntGroupBox1: TTntGroupBox
        Left = 0
        Top = 0
        Width = 418
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
      object GroupBox1: TGroupBox
        Left = 0
        Top = 156
        Width = 418
        Height = 132
        Align = alBottom
        Caption = ' Misc : '
        TabOrder = 1
        object chkAssociateExt: TCheckBox
          Left = 16
          Top = 24
          Width = 385
          Height = 17
          Hint = 
            'Double click on a .vssprj file will automatically open the proje' +
            'ct in VisualSubSync'
          Caption = 'Associate project file (*.vssprj) with VisualSubSync'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = chkAssociateExtClick
        end
        object chkSwapSubList: TCheckBox
          Left = 16
          Top = 40
          Width = 385
          Height = 17
          Caption = 'Swap subtitles list'
          TabOrder = 1
        end
        object chkSceneChange: TCheckBox
          Left = 16
          Top = 56
          Width = 385
          Height = 17
          Caption = 'Show scene change'
          TabOrder = 2
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 64
        Width = 418
        Height = 81
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
          Left = 296
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
          Left = 392
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
    object tsErrorChecking: TTntTabSheet
      BorderWidth = 4
      Caption = 'Error checking'
      object ListErrorChecking: TTntCheckListBox
        Left = 0
        Top = 0
        Width = 418
        Height = 105
        OnClickCheck = ListErrorCheckingClickCheck
        Align = alTop
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = ListErrorCheckingClick
      end
      object Panel2: TPanel
        Left = 0
        Top = 112
        Width = 417
        Height = 73
        BevelOuter = bvLowered
        TabOrder = 1
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
          Width = 345
          Height = 44
          AutoSize = False
          Caption = '-'
          WordWrap = True
        end
        object TntLabel4: TTntLabel
          Left = 360
          Top = 4
          Width = 57
          Height = 13
          Alignment = taCenter
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
          Left = 376
          Top = 24
          Width = 25
          Height = 25
          Shape = stCircle
        end
        object Bevel1: TBevel
          Left = 352
          Top = 8
          Width = 9
          Height = 57
          Shape = bsRightLine
        end
      end
      object ListPluginParam: TVirtualStringTree
        Left = 0
        Top = 192
        Width = 418
        Height = 96
        Align = alBottom
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
        TabOrder = 2
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toCenterScrollIntoView]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnCreateEditor = ListPluginParamCreateEditor
        OnEditing = ListPluginParamEditing
        OnGetText = ListPluginParamGetText
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 0
            Width = 164
            WideText = 'Parameter'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 1
            Width = 150
            WideText = 'Value'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 2
            Width = 100
            WideText = 'Unit'
          end>
      end
    end
    object tsHotKeys: TTntTabSheet
      BorderWidth = 4
      Caption = 'HotKeys'
      object TntLabel5: TTntLabel
        Left = 0
        Top = 248
        Width = 33
        Height = 13
        Caption = 'Mode :'
      end
      object TntLabel6: TTntLabel
        Left = 128
        Top = 248
        Width = 40
        Height = 13
        Caption = 'Hotkey :'
      end
      object ListHotkeys: TTntListView
        Left = 0
        Top = 0
        Width = 418
        Height = 241
        Align = alTop
        Columns = <
          item
            Caption = 'Name'
            Width = 180
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
      object HotKey1: THotKey
        Left = 128
        Top = 265
        Width = 209
        Height = 19
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 1
      end
      object ComboHotkeyMode: TTntComboBox
        Left = 0
        Top = 264
        Width = 113
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
        Text = 'Normal'
        OnSelect = ComboHotkeyModeSelect
        Items.Strings = (
          'Normal'
          'Timing')
      end
      object bttResetAllHotkeys: TTntButton
        Left = 352
        Top = 247
        Width = 65
        Height = 19
        Caption = 'Reset all'
        TabOrder = 5
        OnClick = bttResetAllHotkeysClick
      end
      object bttSetHotkey: TTntButton
        Left = 352
        Top = 265
        Width = 33
        Height = 19
        Caption = 'Set'
        TabOrder = 2
        OnClick = bttSetHotkeyClick
      end
      object bttClearHotkey: TTntButton
        Left = 384
        Top = 265
        Width = 33
        Height = 19
        Caption = 'Clear'
        TabOrder = 3
        OnClick = bttClearHotkeyClick
      end
    end
    object tsMouse: TTntTabSheet
      BorderWidth = 4
      Caption = 'Mouse'
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 418
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
          Color = clBtnText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
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
          Color = clBtnText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
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
        Top = 136
        Width = 418
        Height = 152
        Align = alBottom
        Caption = ' Misc : '
        TabOrder = 1
        object chkEnableSSATimingMode: TCheckBox
          Left = 16
          Top = 24
          Width = 385
          Height = 17
          Caption = 
            'Left/right mouse button set start/stop time in Timing Mode (SSA ' +
            'mouse mode)'
          TabOrder = 0
        end
        object chkEnableMouseAntiOverlapping: TCheckBox
          Left = 16
          Top = 40
          Width = 385
          Height = 17
          Caption = 'Enable mouse anti-overlapping'
          TabOrder = 1
        end
      end
    end
    object tsFonts: TTntTabSheet
      BorderWidth = 4
      Caption = 'Fonts'
      object TntGroupBox2: TTntGroupBox
        Left = 0
        Top = 0
        Width = 418
        Height = 81
        Align = alTop
        Caption = ' Subtitles list : '
        TabOrder = 0
        object EditSubListFont: TTntEdit
          Left = 16
          Top = 24
          Width = 345
          Height = 41
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubListFont: TTntButton
          Left = 368
          Top = 24
          Width = 33
          Height = 25
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubListFontClick
        end
      end
      object TntGroupBox3: TTntGroupBox
        Left = 0
        Top = 89
        Width = 418
        Height = 80
        Caption = ' Subtitles text : '
        TabOrder = 1
        object EditSubTextFont: TTntEdit
          Left = 16
          Top = 24
          Width = 345
          Height = 41
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubTextFont: TTntButton
          Left = 368
          Top = 24
          Width = 33
          Height = 25
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubTextFontClick
        end
      end
    end
    object tsTimingMode: TTntTabSheet
      BorderWidth = 4
      Caption = 'TimingMode'
      object TntGroupBox4: TTntGroupBox
        Left = 0
        Top = 0
        Width = 418
        Height = 288
        Align = alClient
        TabOrder = 0
        object TntLabel2: TTntLabel
          Left = 16
          Top = 104
          Width = 143
          Height = 13
          Caption = 'Characters per second target :'
        end
        object TntLabel7: TTntLabel
          Left = 16
          Top = 132
          Width = 140
          Height = 13
          Caption = 'Minimal subtitle duration (ms) :'
        end
        object TntLabel8: TTntLabel
          Left = 16
          Top = 160
          Width = 140
          Height = 13
          Caption = 'Blank between subtitles (ms) :'
        end
        object chkEnableSubCreationWithSpaceKey: TCheckBox
          Left = 16
          Top = 56
          Width = 385
          Height = 17
          Caption = 'Enable subtitle creation with space key (toggle) in timing mode'
          TabOrder = 0
        end
        object chkSpaceKeyModifyTiming: TCheckBox
          Left = 48
          Top = 72
          Width = 217
          Height = 17
          Caption = 'Space key modify existing subtitles timing'
          TabOrder = 1
        end
        object chkDisableSubEditionInTimingMode: TCheckBox
          Left = 16
          Top = 24
          Width = 385
          Height = 17
          Caption = 'Disable subtitle edition in timing mode'
          TabOrder = 2
        end
        object EditCPSTarget: TTntEdit
          Left = 168
          Top = 100
          Width = 81
          Height = 21
          TabOrder = 3
          Text = '5'
        end
        object EditMinimalDuration: TTntEdit
          Left = 168
          Top = 128
          Width = 81
          Height = 21
          TabOrder = 4
          Text = '500'
        end
        object UpDownCPSTarget: TTntUpDown
          Left = 249
          Top = 100
          Width = 16
          Height = 21
          Associate = EditCPSTarget
          Min = 5
          Max = 50
          Position = 5
          TabOrder = 5
          Thousands = False
        end
        object UpDownMinimalDuration: TTntUpDown
          Left = 249
          Top = 128
          Width = 16
          Height = 21
          Associate = EditMinimalDuration
          Min = 200
          Max = 5000
          Increment = 100
          Position = 500
          TabOrder = 6
          Thousands = False
        end
        object EditBlankBetweenSub: TTntEdit
          Left = 168
          Top = 156
          Width = 81
          Height = 21
          TabOrder = 7
          Text = '0'
        end
        object UpDownBlankBetweenSub: TTntUpDown
          Left = 249
          Top = 156
          Width = 16
          Height = 21
          Associate = EditBlankBetweenSub
          Max = 500
          Increment = 10
          TabOrder = 8
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 324
    Width = 434
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bttOk: TTntButton
      Left = 278
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = bttOkClick
    end
    object bttCancel: TTntButton
      Left = 358
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = bttCancelClick
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
    Top = 336
  end
end
