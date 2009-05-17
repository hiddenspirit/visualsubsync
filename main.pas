// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003-2007 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------
//
// SSA/ASS loading code contribution by Mirage (2005)
//
// -----------------------------------------------------------------------------

unit main;

interface

uses
  GlobalUnit, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, WAVDisplayerUnit, SubStructUnit, ComCtrls, Menus, Renderer,
  MiniScrollBarUnit, VirtualTrees, MiscToolsUnit, TntStdCtrls, TntMenus,
  Buttons, TntActnList, ImgList, ActnList, TntDialogs, TntComCtrls,
  PeakCreationProgressFormUnit, ProjectUnit, ServerUnit, TntExtCtrls, IniFiles,
  PreferencesFormUnit, MRUListUnit, StdActns, TntStdActns, TntButtons, TntForms,
  DetachedVideoFormUnit, XPMan, JavaScriptPluginUnit, Contnrs, DelayFormUnit,
  UndoableTaskUnit, UndoableSubTaskUnit, Types, LibHunspellUnit;

type
  // -----

  TPlayingModeType = (pmtAll, pmtSelection, pmtSelectionStart, pmtSelectionEnd);
  TTagType = (ttItalic, ttBold, ttUnderline, ttColor, ttSize);

  TMainForm = class(TTntForm, TVSSCoreEngineIntf)
    TntMainMenu1: TTntMainMenu;
    MenuItemFile: TTntMenuItem;
    MenuItemNewProject: TTntMenuItem;
    MenuItemOpenProject: TTntMenuItem;
    N1: TTntMenuItem;
    MenuItemSave: TTntMenuItem;
    MenuItemSaveAs: TTntMenuItem;
    N2: TTntMenuItem;
    MenuItemExit: TTntMenuItem;
    PanelTop: TPanel;
    PanelVideo: TPanel;
    PanelWAVDisplay: TPanel;
    SplitterWAVDisplay_Video: TSplitter;
    MenuItemEdit: TTntMenuItem;
    MenuItemView: TTntMenuItem;
    TntActionList1: TTntActionList;
    ActionZoomIn: TTntAction;
    ImageList1: TImageList;
    ActionZoomOut: TTntAction;
    ActionZoomSelection: TTntAction;
    ActionZoomAll: TTntAction;
    ActionPlay: TTntAction;
    ActionStop: TTntAction;
    ActionLoop: TTntAction;
    MenuItemZoomIn: TTntMenuItem;
    MenuItemZoomOut: TTntMenuItem;
    MenuItemZoomSelection: TTntMenuItem;
    MenuItemZoomAll: TTntMenuItem;
    ActionExit: TTntAction;
    ActionNewProject: TTntAction;
    ActionShowHideVideo: TTntAction;
    N3: TTntMenuItem;
    MenuItemShowHideVideo: TTntMenuItem;
    TntOpenDialog1: TTntOpenDialog;
    ActionOpenProject: TTntAction;
    N4: TTntMenuItem;
    MenuItemProjectProperties: TTntMenuItem;
    ActionFind: TTntAction;
    MenuItemFind: TTntMenuItem;
    ActionSave: TTntAction;
    ActionSaveAs: TTntAction;
    TntSaveDialog1: TTntSaveDialog;
    MenuItemPlayback: TTntMenuItem;
    MenuItemPlay: TTntMenuItem;
    MenuItemStop: TTntMenuItem;
    MenuItemLoop: TTntMenuItem;
    WAVDisplayPopupMenu: TTntPopupMenu;
    pmiWAVDispAddSubtitle: TTntMenuItem;
    pmiWAVDispDeleteRange: TTntMenuItem;
    SubListPopupMenu: TTntPopupMenu;
    ActionProjectProperties: TTntAction;
    pmiSubListDelete: TTntMenuItem;
    pmiWAVDispSplitAtCursor: TTntMenuItem;
    pmiSubListMerge: TTntMenuItem;
    MenuItemFindNext: TTntMenuItem;
    ActionFindNext: TTntAction;
    TimerStatusBarMsg: TTimer;
    MenuItemHelpRoot: TTntMenuItem;
    MenuItemAbout: TTntMenuItem;
    N5: TTntMenuItem;
    MenuItemPreferences: TTntMenuItem;
    PanelBottom: TPanel;
    MemoSubtitleText: TTntRichEdit;
    PanelMiddle: TPanel;
    bttPlay: TSpeedButton;
    bttStop: TSpeedButton;
    bttLoop: TSpeedButton;
    bttZoomIn: TSpeedButton;
    bttZoomOut: TSpeedButton;
    bttZoomSel: TSpeedButton;
    bttZoomAll: TSpeedButton;
    plCursorPos: TPanel;
    PanelTimeSelView: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    edSelLength: TEdit;
    edSelEnd: TEdit;
    edSelBegin: TEdit;
    edViewBegin: TEdit;
    edViewEnd: TEdit;
    edViewLength: TEdit;
    vtvSubsList: TVirtualStringTree;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    N6: TTntMenuItem;
    SubMenuItemErrorChecking: TTntMenuItem;
    MenuItemCheckErrors: TTntMenuItem;
    MenuItemErrorCheckingPreferences: TTntMenuItem;
    MenuItemDelay: TTntMenuItem;
    ActionCheckErrors: TTntAction;
    ActionDelay: TTntAction;
    ActionShowErrorReport: TTntAction;
    MenuItemShowErrorReport: TTntMenuItem;
    bttNextSub: TSpeedButton;
    bttPreviousSub: TSpeedButton;
    ActionNextSub: TTntAction;
    ActionPreviousSub: TTntAction;
    NextSub1: TTntMenuItem;
    PreviousSub1: TTntMenuItem;
    pmiSubListDelay: TTntMenuItem;
    Panel1: TPanel;
    chkAutoScrollSub: TCheckBox;
    chkAutoScrollWAVDisp: TTntCheckBox;
    lbAutoScroll: TTntLabel;
    TimerAutoScrollSub: TTimer;
    Goto1: TTntMenuItem;
    ActionGoto: TTntAction;
    SubMenuItemWebServer: TTntMenuItem;
    MenuItemStartWebServer: TTntMenuItem;
    MenuItemStopWebServer: TTntMenuItem;
    N7: TTntMenuItem;
    MenuItemShowSuggestions: TTntMenuItem;
    MenuItemOpenRecentRoot: TTntMenuItem;
    ActionPreferences: TTntAction;
    MenuItemClose: TTntMenuItem;
    ActionClose: TTntAction;
    ActionErrorPreferences: TTntAction;
    ActionShowSuggestions: TTntAction;
    MenuItemHelp: TTntMenuItem;
    N8: TTntMenuItem;
    bttCheckErrors: TSpeedButton;
    bttShowSuggestions: TSpeedButton;
    bttShowHideVideo: TSpeedButton;
    MemoSubPopupMenu: TTntPopupMenu;
    pmiMemoSubCopy: TTntMenuItem;
    pmiMemoSubCut: TTntMenuItem;
    pmiMemoSubPaste: TTntMenuItem;
    EditCut1: TTntEditCut;
    EditSelectAll1: TTntEditSelectAll;
    EditDelete1: TTntEditDelete;
    pmiMemoSubUndo: TTntMenuItem;
    N9: TTntMenuItem;
    Delete1: TTntMenuItem;
    N10: TTntMenuItem;
    SelectAll1: TTntMenuItem;
    MenuItemHelpIndex: TTntMenuItem;
    MenuItemHelpIndexWAVDisplayControl: TTntMenuItem;
    pmiWAVDispSetSubtitleTime: TTntMenuItem;
    N11: TTntMenuItem;
    ActionInsertTextFile: TTntAction;
    Inserttextfile1: TTntMenuItem;
    ActionClearSelection: TTntAction;
    PanelPlaybackControl: TPanel;
    bttWorkingMode: TTntSpeedButton;
    ActionLoopSelStart: TTntAction;
    ActionLoopSelEnd: TTntAction;
    ActionAddSubtitle: TTntAction;
    ActionSetSubtitleTime: TTntAction;
    ActionSetPlaybackRate80: TTntAction;
    ActionSetPlaybackRate90: TTntAction;
    ActionSetPlaybackRate100: TTntAction;
    N12: TTntMenuItem;
    Rate1: TTntMenuItem;
    Setplaybackrateat1001: TTntMenuItem;
    Setplaybackrateat901: TTntMenuItem;
    Setplaybackrateat801: TTntMenuItem;
    Loopselectionstart1: TTntMenuItem;
    Loopselectionend1: TTntMenuItem;
    ActionShiftStartPlus100: TTntAction;
    ActionShiftStartMinus100: TTntAction;
    ActionShiftStopPlus100: TTntAction;
    ActionShiftStopMinus100: TTntAction;
    ActionShiftStartPlus10: TTntAction;
    ActionShiftStartMinus10: TTntAction;
    ActionShiftStopPlus10: TTntAction;
    ActionShiftStopMinus10: TTntAction;
    MenuItemVerticalScaling: TTntMenuItem;
    ActionZoomVertical: TTntAction;
    bttZoomVertical: TSpeedButton;
    MemoTextPipe: TTntRichEdit;
    SplitterMemoTextPipe: TSplitter;
    ActionShowHideTextPipe: TTntAction;
    MenuItemTextPipe: TTntMenuItem;
    MenuItemLoadTextFileInPipe: TTntMenuItem;
    MenuItemShowHideTextPipe: TTntMenuItem;
    ActionLoadTextPipe: TTntAction;
    MemoTextPipePopupMenu: TTntPopupMenu;
    pmiLoadTextPipe: TTntMenuItem;
    pmiAddAsSubtitle: TTntMenuItem;
    ActionClearTextPipe: TTntAction;
    MenuItemClearTextPipe: TTntMenuItem;
    ActionSaveTextPipeAs: TTntAction;
    MenuItemSaveTextPipe: TTntMenuItem;
    ActionAddSubFromPipe: TTntAction;
    N13: TTntMenuItem;
    pmiAutoColorizeText: TTntMenuItem;
    pmiAutoDeleteText: TTntMenuItem;
    pmiAutoDeleteAllTextBefore: TTntMenuItem;
    ActionReplaceFromPipe: TTntAction;
    pmiReplaceSubtitleFromPipe: TTntMenuItem;
    TimerAutoBackup: TTimer;
    ActionDetachVideo: TTntAction;
    MenuItemDetachVideoWindow: TTntMenuItem;
    N14: TTntMenuItem;
    ShowHidelogs1: TTntMenuItem;
    ActionShowHideLogs: TTntAction;
    pmiFixError: TTntMenuItem;
    ActionFixErrorMain: TTntAction;
    MemoLinesCounter: TTntRichEdit;
    ActionStartSub: TTntAction;
    ActionStopSub: TTntAction;
    ActionStopAndStartSub: TTntAction;
    N15: TTntMenuItem;
    ShowHideTextPipe1: TTntMenuItem;
    ActionInsertKaraokeMarker: TTntAction;
    pmiInsertKaraokeMarker: TTntMenuItem;
    pmiCreateKaraoke: TTntMenuItem;
    pmiClearKaraoke: TTntMenuItem;
    ActionShowStartFrame: TTntAction;
    ActionShowStopFrame: TTntAction;
    ActionShowFrameAtCursor: TTntAction;
    N16: TTntMenuItem;
    ActionExportToSSA: TTntAction;
    MenuItemExportToWAV: TTntMenuItem;
    ActionExportToWAV: TTntAction;
    ActionPlaySelStart: TTntAction;
    ActionPlaySelEnd: TTntAction;
    Playselectionstart1: TTntMenuItem;
    Playselectionend1: TTntMenuItem;
    ActionPlaceKaraokeCursorsAtEnd: TTntAction;
    ActionPlayToEnd: TTntAction;
    bttPlayToEnd: TSpeedButton;
    MenuItemPlayToEnd: TTntMenuItem;
    bttShowPreferences: TSpeedButton;
    ActionPlay1sBefore: TTntAction;
    ActionSelectNextSub: TTntAction;
    ActionSelectPreviousSub: TTntAction;
    XPManifest1: TXPManifest;
    bttStyles: TTntButton;
    SubListHeaderPopupMenu: TTntPopupMenu;
    ActionSetSubtitleStartTime: TTntAction;
    ActionSetSubtitleStopTime: TTntAction;
    ActionSetSubtitleStopTimeAndGoToNext: TTntAction;
    cbStyles: TTntComboBox;
    ActionStyles: TTntAction;
    Styles1: TTntMenuItem;
    ActionSetPlaybackRate70: TTntAction;
    ActionSetPlaybackRate60: TTntAction;
    Setplaybackrateat701: TTntMenuItem;
    Setplaybackrateat601: TTntMenuItem;
    bttPause: TSpeedButton;
    ActionPause: TTntAction;
    MenuItemPause: TTntMenuItem;
    ActionToggleTimingMode: TTntAction;
    tbVolume: TTntTrackBar;
    lblVolume: TTntLabel;
    ActionSetEditorFocus: TTntAction;
    ActionSetEditorFocusAndSelect: TTntAction;
    MenuItemShowFrameAtCursor: TTntMenuItem;
    MenuItemShowStartFrame: TTntMenuItem;
    MenuItemShowStopFrame: TTntMenuItem;
    ActionNextError: TTntAction;
    StatusBarMainPanel: TPanel;
    ActionUndo: TTntAction;
    ActionRedo: TTntAction;
    Undo2: TTntMenuItem;
    Redo1: TTntMenuItem;
    N17: TTntMenuItem;
    ActionWAVDisplayScrollRight: TTntAction;
    ActionWAVDisplayScrollLeft: TTntAction;
    ActionShowHideSceneChange: TTntAction;
    N18: TTntMenuItem;
    ActionTextItalic: TTntAction;
    ActionTextBold: TTntAction;
    Italic1: TTntMenuItem;
    ActionTextUnderline: TTntAction;
    ActionTextColor: TTntAction;
    ActionTextSize: TTntAction;
    Otherstyle1: TTntMenuItem;
    extinbold1: TTntMenuItem;
    extinunderline1: TTntMenuItem;
    extcolor1: TTntMenuItem;
    extsize1: TTntMenuItem;
    N19: TTntMenuItem;
    ActionStripTags: TTntAction;
    pmiStriptags: TTntMenuItem;
    ActionMerge: TTntAction;
    ActionSplitAtCursor: TTntAction;
    extinitalic1: TTntMenuItem;
    Otherstyles1: TTntMenuItem;
    extinbold2: TTntMenuItem;
    extinunderline2: TTntMenuItem;
    extcolor2: TTntMenuItem;
    extsize2: TTntMenuItem;
    MenuItemOpenProjectFolder: TTntMenuItem;
    ActionOpenProjectFolder: TTntAction;
    ActionPlay1sBeforeToEnd: TTntAction;
    ActionDeleteSceneChange: TTntAction;
    pmiDeleteSC: TTntMenuItem;
    MenuItemShowHidesSeneChange: TTntMenuItem;
    ActionReload: TTntAction;
    Reload1: TTntMenuItem;
    ActionMergeWithPrevious: TTntAction;
    ActionMergeWithNext: TTntAction;
    ActionMergeDialog: TTntAction;
    ActionFixSelectedErrors: TTntAction;
    pmiSubListMergeDialog: TTntMenuItem;
    ActionMergeOnOneLine: TTntAction;
    MenuItemJSTools: TTntMenuItem;
    N20: TTntMenuItem;
    ActionLoadPresets: TTntAction;
    MenuItemLoadPresets: TTntMenuItem;
    ActionCopy: TTntAction;
    OpenDialogPresets: TTntOpenDialog;
    ActionShowSilentZones: TTntAction;
    Showsilentzones1: TTntMenuItem;
    N21: TTntMenuItem;
    ActionLiveSpellCheck: TTntAction;
    SubMenuItemSpellcheck: TTntMenuItem;
    MenuItemLiveSpellcheck: TTntMenuItem;
    N22: TTntMenuItem;
    N23: TTntMenuItem;
    MenuItemGetMoreDictionaries: TTntMenuItem;
    ActionSpellCheck: TTntAction;
    MenuItemSpellcheck: TTntMenuItem;
    StatusBarPanel1: TPanel;
    StatusBarPanel2: TPanel;
    N24: TTntMenuItem;
    ActionInsertSceneChange: TTntAction;
    pmiInsertSC: TTntMenuItem;
    ActionPaste: TTntAction;
    ActionPasteAtCursor: TTntAction;
    Pasteatcursor1: TTntMenuItem;
    procedure FormCreate(Sender: TObject);

    procedure WAVDisplayer1CursorChange(Sender: TObject);
    procedure WAVDisplayer1PlayCursorChange(Sender: TObject);
    procedure WAVDisplayer1SelectionChange(Sender: TObject);
    procedure WAVDisplayer1ViewChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRange(Sender: TObject; SelectedRange : TRange; IsDynamic : Boolean);
    procedure WAVDisplayer1SelectedRangeChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRangeChanged(Sender: TObject;
      OldStart, OldStop : Integer; NeedSort : Boolean);
    procedure WAVDisplayer1AutoScrollChange(Sender: TObject);
    procedure WAVDisplayPopup_DeleteRange(Sender: TObject);
    procedure WAVDisplayer1StartPlaying(Sender: TObject);
    procedure WAVDisplayer1KaraokeChanged(Sender: TObject;
        Range : TRange; SubTimeIndex, OldTime : Integer);
    procedure WAVDisplayer1SelectedKaraokeRange(Sender: TObject;
        Range : TRange);
    procedure WAVDisplayer1CustomDrawRange(Sender: TObject; ACanvas: TCanvas; Range : TRange; Rect : TRect);
    procedure WAVDisplayer1RangeStartDblClick(Sender: TObject; Range : TRange);
    procedure WAVDisplayer1RangeStopDblClick(Sender: TObject; Range : TRange);    
    procedure vtvSubsListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure MemoSubtitleTextChange(Sender: TObject);
    procedure vtvSubsListDblClick(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure ActionZoomSelectionExecute(Sender: TObject);
    procedure ActionZoomAllExecute(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionLoopExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionNewProjectExecute(Sender: TObject);
    procedure ActionShowHideVideoExecute(Sender: TObject);
    procedure ActionOpenProjectExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure MemoSubtitleTextSelectionChange(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionProjectPropertiesExecute(Sender: TObject);
    procedure pmiSubListDeleteClick(Sender: TObject);
    procedure SubListPopupMenuPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure WAVDisplayPopupMenuPopup(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure TimerStatusBarMsgTimer(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure chkAutoScrollWAVDispClick(Sender: TObject);
    procedure ActionCheckErrorsExecute(Sender: TObject);
    procedure ActionDelayExecute(Sender: TObject);
    procedure ActionShowErrorReportExecute(Sender: TObject);
    procedure ActionNextSubExecute(Sender: TObject);
    procedure ActionPreviousSubExecute(Sender: TObject);
    procedure vtvSubsListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure chkAutoScrollSubClick(Sender: TObject);
    procedure TimerAutoScrollSubTimer(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure MenuItemStartWebServerClick(Sender: TObject);
    procedure MenuItemStopWebServerClick(Sender: TObject);
    procedure ActionPreferencesExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionErrorPreferencesExecute(Sender: TObject);
    procedure ActionShowSuggestionsExecute(Sender: TObject);
    procedure MenuItemHelpIndexClick(Sender: TObject);
    procedure MenuItemHelpIndexWAVDisplayControlClick(Sender: TObject);
    procedure ActionInsertTextFileExecute(Sender: TObject);
    procedure ActionClearSelectionExecute(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure bttWorkingModeClick(Sender: TObject);
    procedure ActionLoopSelStartExecute(Sender: TObject);
    procedure ActionLoopSelEndExecute(Sender: TObject);
    procedure ActionAddSubtitleExecute(Sender: TObject);
    procedure ActionSetSubtitleTimeExecute(Sender: TObject);
    procedure ActionSetPlaybackRate80Execute(Sender: TObject);
    procedure ActionSetPlaybackRate90Execute(Sender: TObject);
    procedure ActionSetPlaybackRate100Execute(Sender: TObject);
    procedure ActionShiftStartPlus100Execute(Sender: TObject);
    procedure ActionShiftStartMinus100Execute(Sender: TObject);
    procedure ActionShiftStopPlus100Execute(Sender: TObject);
    procedure ActionShiftStopMinus100Execute(Sender: TObject);
    procedure ActionShiftStartPlus10Execute(Sender: TObject);
    procedure ActionShiftStartMinus10Execute(Sender: TObject);
    procedure ActionShiftStopPlus10Execute(Sender: TObject);
    procedure ActionShiftStopMinus10Execute(Sender: TObject);
    procedure ActionZoomVerticalExecute(Sender: TObject);
    procedure ActionShowHideTextPipeExecute(Sender: TObject);
    procedure ActionLoadTextPipeExecute(Sender: TObject);
    procedure ActionClearTextPipeExecute(Sender: TObject);
    procedure ActionSaveTextPipeAsExecute(Sender: TObject);
    procedure ActionAddSubFromPipeExecute(Sender: TObject);
    procedure MemoTextPipePopupMenuPopup(Sender: TObject);
    procedure ActionReplaceFromPipeExecute(Sender: TObject);
    procedure TimerAutoBackupTimer(Sender: TObject);
    procedure ActionDetachVideoExecute(Sender: TObject);
    procedure ActionShowHideLogsExecute(Sender: TObject);
    procedure vtvSubsListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ActionFixErrorMainExecute(Sender: TObject);
    procedure TntFormActivate(Sender: TObject);
    procedure MemoSubtitleTextMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ActionStartSubExecute(Sender: TObject);
    procedure ActionStopSubExecute(Sender: TObject);
    procedure ActionStopAndStartSubExecute(Sender: TObject);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TntFormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ActionInsertKaraokeMarkerExecute(Sender: TObject);
    procedure pmiCreateKaraokeClick(Sender: TObject);
    procedure pmiClearKaraokeClick(Sender: TObject);
    procedure ActionShowStartFrameExecute(Sender: TObject);
    procedure ActionShowStopFrameExecute(Sender: TObject);
    procedure ActionShowFrameAtCursorExecute(Sender: TObject);
    procedure ActionExportToWAVExecute(Sender: TObject);
    procedure ActionPlaySelStartExecute(Sender: TObject);
    procedure ActionPlaySelEndExecute(Sender: TObject);
    procedure ActionPlaceKaraokeCursorsAtEndExecute(Sender: TObject);
    procedure ActionPlayToEndExecute(Sender: TObject);
    procedure ActionPlay1sBeforeExecute(Sender: TObject);
    procedure ActionSelectNextSubExecute(Sender: TObject);
    procedure ActionSelectPreviousSubExecute(Sender: TObject);
    procedure PanelVideoResize(Sender: TObject);
    procedure ActionSetSubtitleStartTimeExecute(Sender: TObject);
    procedure ActionSetSubtitleStopTimeExecute(Sender: TObject);
    procedure ActionSetSubtitleStopTimeAndGoToNextExecute(Sender: TObject);
    procedure cbStylesSelect(Sender: TObject);
    procedure ActionStylesExecute(Sender: TObject);
    procedure ActionSetPlaybackRate70Execute(Sender: TObject);
    procedure ActionSetPlaybackRate60Execute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionToggleTimingModeExecute(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure ActionSetEditorFocusExecute(Sender: TObject);
    procedure ActionSetEditorFocusAndSelectExecute(Sender: TObject);
    procedure ActionNextErrorExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure PanelVideoDblClick(Sender: TObject);
    procedure ActionWAVDisplayScrollRightExecute(Sender: TObject);
    procedure ActionWAVDisplayScrollLeftExecute(Sender: TObject);
    procedure ActionShowHideSceneChangeExecute(Sender: TObject);
    procedure vtvSubsListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure ActionTextItalicExecute(Sender: TObject);
    procedure ActionTextBoldExecute(Sender: TObject);
    procedure ActionTextUnderlineExecute(Sender: TObject);
    procedure ActionTextColorExecute(Sender: TObject);
    procedure ActionTextSizeExecute(Sender: TObject);
    procedure ActionStripTagsExecute(Sender: TObject);
    procedure ActionMergeExecute(Sender: TObject);
    procedure ActionSplitAtCursorExecute(Sender: TObject);
    procedure pmiToggleColumn(Sender: TObject);
    procedure vtvSubsListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure ActionOpenProjectFolderExecute(Sender: TObject);
    procedure ActionPlay1sBeforeToEndExecute(Sender: TObject);
    procedure ActionDeleteSceneChangeExecute(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure ActionMergeWithPreviousExecute(Sender: TObject);
    procedure ActionMergeWithNextExecute(Sender: TObject);
    procedure ActionMergeDialogExecute(Sender: TObject);
    procedure ActionFixSelectedErrorsExecute(Sender: TObject);
    procedure ActionMergeOnOneLineExecute(Sender: TObject);
    procedure ActionLoadPresetsExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCopyUpdate(Sender: TObject);
    procedure ActionShowSilentZonesExecute(Sender: TObject);
    procedure ActionLiveSpellCheckExecute(Sender: TObject);
    procedure MemoSubPopupMenuPopup(Sender: TObject);
    procedure MenuItemGetMoreDictionariesClick(Sender: TObject);
    procedure ActionSpellCheckExecute(Sender: TObject);
    procedure ActionSpellCheckUpdate(Sender: TObject);
    procedure ActionInsertSceneChangeExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionPasteAtCursorExecute(Sender: TObject);
   
  private
    { Private declarations }
    WAVDisplayer : TWAVDisplayer;
    SubRangeFactory : TSubtitleRangeFactory;
    CurrentProject : TVSSProject;
    PeakCreationProgressForm : TPeakCreationProgressForm;
    AudioOnlyRenderer : TDShowRenderer;
    VideoRenderer : TDShowRenderer;
    SearchNodeIndex : Integer;
    SearchPos : Integer;
    OldAutoScrollIdx : Integer;

    Server : THTTPServer;
    ServerRootDir : string;

    MRUList : TMRUList;
    ConfigObject : TConfigObject;

    Swapped : Boolean;
    PlayingMode : TPlayingModeType;
    ShowingVideo : Boolean;
    FormHasBeenActivated : Boolean;

    StartSubtitleTime : Integer;
    ToggleStartSubtitleTime : Integer;
    SubtitleFileHeader : WideString;
    SubtitleFileFooter : WideString;
    VideoPreviewNeedSubtitleUpdate : Boolean;
    DisableVideoUpdatePreview : Boolean;

    StatusBarPrimaryText : WideString;
    StatusBarSecondaryText : WideString;

    GeneralJSPlugin : TSimpleJavascriptWrapper;

    UndoStack : TObjectStack;
    RedoStack : TObjectStack;
    // Used only during fix error
    UndoableMultiChangeTask : TUndoableMultiChangeTask;

    PrefFormInitialized : Boolean;

    StartupShowVideo, StartupDetachVideo : Boolean;

    FSpellChecker : THunspellChecker;

    procedure InitVTV;
    procedure InitVTVExtraColumns;
    procedure EnableControl(Enable : Boolean);
    procedure EnableStyleControls(Enable : Boolean);
    procedure LoadSubtitles(Filename: WideString; var IsUTF8 : Boolean);
    procedure SaveSubtitles(Filename: WideString; PreviousFilename : WideString;
      InUTF8 : Boolean; BackupOnly : Boolean);
    procedure SaveProject(Project : TVSSProject; SilentSave : Boolean);
    procedure UpdateLinesCounter;
    function CheckSubtitlesAreSaved : Boolean;

    procedure WAVDisplayer1OnPeakFileCreation(Sender: TObject;
      EventType : TPeakFileCreationEventType; Param : Integer);
    procedure CurrentProjectOnDirtyChange(Sender : TObject);
    procedure CurrentProjectOnDirtySet(Sender: TObject);

    procedure FullSortTreeAndSubList;

    procedure OnRecentMenuItemClick(Sender : TObject);
    procedure SetShortcut(TimingMode : Boolean);

    procedure OffsetCurrentSubtitleStartTime(Offset : Integer);
    procedure OffsetCurrentSubtitleStopTime(Offset : Integer);

    function ColorizeOrDeleteTextPipe : TUndoableTask;
    procedure ApplyMiscSettings;
    procedure ApplyMouseSettings;
    procedure ApplyAutoBackupSettings;
    procedure UpdateVideoRendererWindow;
    procedure ApplyFontSettings;

    procedure OnSubtitleRangeJSWrapperChangeStart(Sender : TSubtitleRangeJSWrapper;
      SubtitleRange : TSubtitleRange; NewValue : Integer);
    procedure OnSubtitleRangeJSWrapperChangeStop(Sender : TSubtitleRangeJSWrapper;
      SubtitleRange : TSubtitleRange;  NewValue : Integer);
    procedure OnSubtitleRangeJSWrapperChangeText(Sender : TSubtitleRangeJSWrapper;
      SubtitleRange : TSubtitleRange; NewValue : WideString);

    procedure SaveSubtitlesAsSRT(Filename, PreviousExt: WideString; InUTF8 : Boolean);
    procedure SaveSubtitlesAsSSA(Filename, PreviousExt: WideString; InUTF8 : Boolean);
    procedure SaveSubtitlesAsASS(Filename, PreviousExt: WideString; InUTF8 : Boolean);
    procedure SaveSubtitlesAsCUE(Filename: WideString);
    procedure SaveSubtitlesAsTXT(Filename, PreviousExt: WideString; InUTF8 : Boolean);
    procedure SelectPreviousSub;
    procedure SelectNextSub;
    function LoadSRT(Filename: WideString; var IsUTF8 : Boolean) : Boolean;
    function LoadASS(Filename: WideString; var IsUTF8 : Boolean; IsSSA : Boolean) : Boolean;
    procedure AdvanceToNextSubtitleAfterFocus;
    procedure SetFocusedSubtitleStopTime(Advance: Boolean; KeepStopIfSuperiorToCPSTarget : Boolean);
    procedure UpdateStylesComboboxFromSelection;
    procedure UpdateStylesComboBox;
    procedure UpdateVolume;
    procedure UpdateSubtitleForPreview(ForceUpdate: Boolean);
    procedure SetSubtitleStartTime(SetStopTime : Boolean);
    procedure Merge(MergeType : TMergeType; MergeRange : TMergeRange);

    function DetectCharsetDataLoss : Boolean;

    procedure TagText(StartTag, StopTag : WideString); overload;
    procedure TagTextMemo(StartTag, StopTag : WideString);
    procedure TagTextLines(StartTag, StopTag : WideString);
    procedure TagText(TagType : TTagType); overload;

    // General JS plugins
    procedure CallJSOnSubtitleModification;
    procedure CallJSOnSelectedSubtitle;
    procedure GetCurrentPreviousNextSubtitles(CurrentNode : PVirtualNode; var CurrentSub,
      PreviousSub, NextSub : TSubtitleRange);
    procedure UpdateStatusBar;
    procedure OnJsSetStatusBarText(const Msg : WideString);

    procedure OnJavascriptAction(Sender : TObject);
    procedure UpdateAfterJSChange;

    // Undo/Redo stuff
    procedure PushUndoableTask(UndoableTask : TUndoableTask);
    procedure ClearStack(Stack : TObjectStack);

    procedure OnUndo(Sender: TTntRichEdit; UndoTask : TUndoableTask);

    procedure LoadPresetFile(Filename : WideString);
    procedure FillPresetsMenu;
    procedure OnLoadPresetMenuItemClick(Sender: TObject);

    procedure OnSpellcheckLanguageMenuItemClick(Sender: TObject);
    procedure OnSpellcheckSuggestionMenuItemClick(Sender: TObject);
    procedure OnLoadDictionnary(Sender: TObject);
    procedure OnLoadDictionnaryTerminate(Sender: TObject);
    procedure OnSpellcheckAddToDictionaryMenuItemClick(Sender: TObject);
    procedure OnSpellcheckIgnoreMenuItemClick(Sender: TObject);

    procedure CloseProject;

    function GetTextPipeAutoOption : Integer;
    procedure SetTextPipeAutoOption(Option : Integer);

  public
    { Public declarations }
    procedure ShowStatusBarMessage(const Text : WideString; const Duration : Integer = 4000);
    procedure SelectNode(Node : PVirtualNode);
    procedure SelectNodeFromTimestamps(Start, Stop : Integer);
    procedure SelectNodeAtIndex(Index : Integer);
    procedure StartStopServer(Stop : Boolean = False);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ShowPreferences(TabSheet : TTntTabSheet);
    procedure LoadVideoSceneChange;
    procedure LoadProject(Filename : WideString);
    procedure SwapSubList(SwapSizeAlso : Boolean = True);
    procedure FinishLoadSettings;
    function CanFixError(PluginFilename : WideString) : Boolean;
    procedure FixErrorInList(ErrorList : TList);
    procedure RenameStyle(OldName, NewName : WideString);
    procedure CurrentProjectSetDirty;
    function GetVideoRendererFiltersList(list : TStrings) : Boolean;
    procedure SetStatusBarPrimaryText(const Text : WideString);
    procedure ProcessParams;

    function IsTimingMode : Boolean;
    function IsNormalMode : Boolean;

    procedure ApplyDelay(var Indexes : array of Integer;
      DelayInMs : Integer; DelayShiftType : TDelayShiftType);
    function AddSubtitle(StartTime, StopTime : Integer; Text : WideString) : PVirtualNode; overload;
    function AddSubtitle(SubRange : TSubtitleRange) : PVirtualNode; overload;
    function AddSubtitle(StartTime, StopTime : Integer; Text : WideString;
      AutoSelect : Boolean) : PVirtualNode; overload;
    procedure DeleteSubtitle(Index : Integer);
    procedure DeleteSubtitles(var Indexes : array of Integer);
    procedure CloneSubtitles(var Indexes : array of Integer; List : TList);
    procedure RestoreSubtitles(List : TList; Indexes : TIntegerDynArray = nil);
    function SetSubtitleTime(Index, NewStartTime, NewStopTime : Integer) : Integer;
    procedure SplitSubtitle(Index, SplitTime, BlankTime : Integer);
    function MergeSubtitles(var FIndexes : array of Integer; MergeType : TMergeType) : TSubtitleRange;
    procedure FocusNode(Node : PVirtualNode; WAVDisplaySelect : Boolean); overload;
    procedure FocusNodeAt(Index : Integer); overload;
    procedure ClearWAVSelection;
    procedure ProcessMultiChangeSub(ChangeList : TList; IsUndo : Boolean);
    procedure ResetFind;
    procedure ShowMatchingSubOnly(DisplayAll : Boolean);
    procedure ReplaceText;
    procedure ReplaceAllText;
    function LastFindSucceded : Boolean;
    procedure InitGeneralJSPlugin;

    procedure InsertSceneChange(SrcSCArray : TIntegerDynArray);
    function DeleteSceneChange(StartTimeMs, StopTimeMs : Integer) : TIntegerDynArray;

    procedure SetSelection(Start, Stop : Integer);
    procedure TagHighlight(RichEdit : TTntRichEdit; TagIndex : Integer);
    procedure LoadDict(Idx : Integer);
    function GetSpellChecker : THunspellChecker;

    // TVSSCoreEngineIntf
    procedure RegisterJavascriptAction(AName, ACaption, ADefaultShortcut : WideString);
    function GetSubCount : Integer;
    function GetFirst : TSubtitleRange;
    function GetNext(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetPrevious(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetSelectedCount : Integer;
    function GetFirstSelected : TSubtitleRange;
    function GetNextSelected(SubtitleRange : TSubtitleRange) : TSubtitleRange;
    function GetAt(Index : Integer) : TSubtitleRange;    
  end;

const
  StartEndPlayingDuration: Integer = 500;
  EnableExperimentalKaraoke: Boolean = True;
  UTF8BOM : array[0..2] of BYTE = ($EF,$BB,$BF);
  INDEX_COL_INDEX = 0;
  START_COL_INDEX = 1;
  STOP_COL_INDEX = 2;
  STYLE_COL_INDEX = 3;
  TEXT_COL_INDEX = 4;
  LAST_CORE_COL_INDEX = TEXT_COL_INDEX;
  COLUMN_COUNT = LAST_CORE_COL_INDEX + 1;

var
  MainForm: TMainForm;

implementation

uses ActiveX, Math, StrUtils, FindFormUnit, AboutFormUnit,
  ErrorReportFormUnit, SuggestionFormUnit, GotoFormUnit,
  VerticalScalingFormUnit, TntSysUtils, TntWindows,
  LogWindowFormUnit, CursorManager, FileCtrl, WAVFileUnit, PageProcessorUnit,
  tom_TLB, RichEdit, StyleFormUnit, SSAParserUnit, TntWideStrings, TntClasses,
  TntIniFiles, TntGraphics, TntSystem, TntRichEditCustomUndoUnit, RGBHSLColorUnit,
  SceneChangeUnit, SilentZoneFormUnit, RegExpr, SRTParserUnit, ShellAPI,
  VSSClipboardUnit, BgThreadTaskUnit, SpellCheckFormUnit, TntClipBrd, DeCAL;

{$R *.dfm}

//==============================================================================

// TODO : Configurable backup directory, backup n last version

// TODO : Test project properties dialog
// TODO : When opening a project fail, show the project properties page for modification
// TODO : Add some checking in project unit ok button

// TODO : Web : number of suggestion for current sub, and current suggestions
// TODO : Web : We need real gzip compression, deflate support is b0rked in IE

// TODO : Split at cursor for submemo
// TODO : Check HD space before extraction

// TODO : Rework project handling which is a bit messy ATM
// TODO : Separate subtitle file loading/saving, stats according to format into a new classes

//------------------------------------------------------------------------------

// TODO : auto clear karaoke timing that are out of subtitle bound ???
// TODO : update documentation (timing button)

//==============================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var i : integer;
    CustomMemo : TTntRichEditCustomUndo;
    MenuItem : TTntMenuItem;
begin
  CustomMemo := TTntRichEditCustomUndo.Create(MemoSubtitleText.Owner);
  CustomMemo.Parent := MemoSubtitleText.Parent;
  CustomMemo.Font.Assign(MemoSubtitleText.Font);
  CustomMemo.OnChange := MemoSubtitleText.OnChange;
  CustomMemo.OnMouseDown := MemoSubtitleText.OnMouseDown;
  CustomMemo.OnSelectionChange := MemoSubtitleText.OnSelectionChange;
  CustomMemo.PopupMenu := MemoSubtitleText.PopupMenu;
  CustomMemo.ScrollBars := MemoSubtitleText.ScrollBars;
  CustomMemo.Width := MemoSubtitleText.Width;
  CustomMemo.Height := MemoSubtitleText.Height;
  CustomMemo.Top := MemoSubtitleText.Top;
  CustomMemo.Left := MemoSubtitleText.Left;
  CustomMemo.Align := MemoSubtitleText.Align;
  CustomMemo.HideSelection := MemoSubtitleText.HideSelection;
  //CustomMemo.DisableWindowsUndo;
  MemoSubtitleText.Free;
  MemoSubtitleText := CustomMemo;
  CustomMemo.OnUndo := OnUndo;

  // Enable/disable experimental karaoke stuff
  pmiCreateKaraoke.Visible := EnableExperimentalKaraoke;
  pmiClearKaraoke.Visible := EnableExperimentalKaraoke;

  plCursorPos.DoubleBuffered := True;
  LogForm := TLogForm.Create(nil);

  UndoStack := TObjectStack.Create;
  RedoStack := TObjectStack.Create;
  ActionUndo.Enabled := False;
  ActionRedo.Enabled := False;

  MRUList := TMRUList.Create(MenuItemOpenRecentRoot);
  MRUList.OnRecentMenuItemClick := OnRecentMenuItemClick;
  ConfigObject := TConfigObject.Create;

  ServerRootDir := ExtractFilePath(Application.ExeName);
  ServerRootDir := IncludeTrailingPathDelimiter(ServerRootDir) + 'web\';

  vtvSubsList.Constraints.MinWidth := 600;
  PanelMiddle.Constraints.MinHeight := 125;
  PanelBottom.Constraints.MinHeight := 40;
  Splitter1.MinSize := 1;

  // Clear speed button caption text filled by action :p
  for i := 0 to Pred(PanelPlaybackControl.ControlCount) do
      if PanelPlaybackControl.Controls[i] is TSpeedButton then
        TSpeedButton(PanelPlaybackControl.Controls[i]).Caption := '';

  bttWorkingMode.Caption := 'Normal';

  SubRangeFactory := TSubtitleRangeFactory.Create;

  WAVDisplayer := TWAVDisplayer.Create(nil);
  WAVDisplayer.Left := 0;
  WAVDisplayer.Top := 0;
  WAVDisplayer.Width := PanelWAVDisplay.Width;
  WAVDisplayer.Height := PanelWAVDisplay.Height - 12;
  WAVDisplayer.Align := alClient;
  WAVDisplayer.Parent := PanelWAVDisplay;

  WAVDisplayer.OnCursorChange := WAVDisplayer1CursorChange;
  WAVDisplayer.OnPlayCursorChange := WAVDisplayer1PlayCursorChange;
  WAVDisplayer.OnSelectionChange := WAVDisplayer1SelectionChange;
  WAVDisplayer.OnViewChange := WAVDisplayer1ViewChange;
  WAVDisplayer.OnSelectedRange := WAVDisplayer1SelectedRange;
  WAVDisplayer.OnPeakFileCreation := WAVDisplayer1OnPeakFileCreation;
  WAVDisplayer.OnSelectedRangeChange := WAVDisplayer1SelectedRangeChange;
  WAVDisplayer.OnSelectedRangeChanged := WAVDisplayer1SelectedRangeChanged;
  WAVDisplayer.OnAutoScrollChange := WAVDisplayer1AutoScrollChange;
  WAVDisplayer.OnStartPlaying := WAVDisplayer1StartPlaying;
  WAVDisplayer.OnKaraokeChanged := WAVDisplayer1KaraokeChanged;
  WAVDisplayer.OnSelectedKaraokeRange := WAVDisplayer1SelectedKaraokeRange;
  WAVDisplayer.OnCustomDrawRange := WAVDisplayer1CustomDrawRange;
  WAVDisplayer.OnRangeStartDblClick := WAVDisplayer1RangeStartDblClick;
  WAVDisplayer.OnRangeStopDblClick := WAVDisplayer1RangeStopDblClick;

  WAVDisplayer.Enabled := False;
  WAVDisplayer.PopupMenu := WAVDisplayPopupMenu;

  InitVTV;

  MemoSubtitleText.Font.Name := 'Arial';
  MemoSubtitleText.Font.Style := MemoSubtitleText.Font.Style + [fsBold];
  MemoSubtitleText.Font.Size := 10;
  MemoLinesCounter.Font.Assign(MemoSubtitleText.Font);
  MemoTextPipe.Font.Assign(MemoSubtitleText.Font);

  EnableControl(False);
  EnableStyleControls(False);

  CurrentProject := TVSSProject.Create;
  CurrentProject.OnDirtyChange := CurrentProjectOnDirtyChange;
  CurrentProject.OnDirtySet := CurrentProjectOnDirtySet;
  AudioOnlyRenderer := TDShowRenderer.Create;
  AudioOnlyRenderer.SetAutoInsertCustomVSFilter(True);
  VideoRenderer := TDShowRenderer.Create;
  VideoRenderer.SetAutoInsertCustomVSFilter(True);
  ShowingVideo := False;
  VideoPreviewNeedSubtitleUpdate := False;
  DisableVideoUpdatePreview := False;

  g_GlobalContext.SubList := WAVDisplayer.RangeList;
  g_GlobalContext.CurrentProject := CurrentProject;
  StartSubtitleTime := -1;
  ToggleStartSubtitleTime := -1;
  //StartStopServer;

  Application.HintHidePause := 10000;

  StatusBarPrimaryText := '';
  StatusBarSecondaryText := '';

  // Init spellchecker menu
  FSpellChecker := THunspellChecker.Create;
  FSpellChecker.LoadPersonalDict(g_DictPath + 'perso.dic');
  FSpellChecker.FindDict(g_DictPath);
  for i := 0 to FSpellChecker.GetDictCount-1 do
  begin
    MenuItem := TTntMenuItem.Create(Self);
    MenuItem.GroupIndex := 1;
    MenuItem.RadioItem := True;
    MenuItem.AutoCheck := True;
    MenuItem.Caption := FSpellChecker.GetDict(i);
    MenuItem.Tag := i;
    MenuItem.OnClick := OnSpellcheckLanguageMenuItemClick;
    SubMenuItemSpellcheck.Insert(
      SubMenuItemSpellcheck.IndexOf(MenuItemLiveSpellcheck) + 2 + i, MenuItem);
  end;
  if (FSpellChecker.GetDictCount = 0) then
  begin
    MenuItem := TTntMenuItem.Create(Self);
    MenuItem.Caption := 'No dictionnary';
    MenuItem.Enabled := False;
    SubMenuItemSpellcheck.Insert(
      SubMenuItemSpellcheck.IndexOf(MenuItemLiveSpellcheck) + 2, MenuItem);
  end;

  // Fill the presets menu
  FillPresetsMenu;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FSpellChecker) then
  begin
    FSpellChecker.SavePersonalDict(g_DictPath + 'perso.dic');
    FreeAndNil(FSpellChecker);
  end;
  StartStopServer(True);
  if Assigned(g_VSSCoreWrapper) then
  begin
    g_VSSCoreWrapper.SetVSSCoreEngine(nil);
  end;
  FreeAndNil(GeneralJSPlugin);
  FreeAndNil(AudioOnlyRenderer);
  FreeAndNil(VideoRenderer);
  FreeAndNil(WAVDisplayer);
  FreeAndNil(SubRangeFactory);
  FreeAndNil(MRUList);
  FreeAndNil(ConfigObject);
  FreeAndNil(CurrentProject);
  ClearStack(RedoStack);
  FreeAndNil(RedoStack);
  ClearStack(UndoStack);
  FreeAndNil(UndoStack);
  if Assigned(LogForm) then
    FreeAndNil(LogForm);
end;

//------------------------------------------------------------------------------

function TMainForm.GetTextPipeAutoOption : Integer;
begin
  if pmiAutoColorizeText.Checked then
    Result := 0
  else if pmiAutoDeleteText.Checked then
    Result := 1
  else if pmiAutoDeleteAllTextBefore.Checked then
    Result := 2
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetTextPipeAutoOption(Option : Integer);
begin
  pmiAutoColorizeText.Checked := (Option = 0);
  pmiAutoDeleteText.Checked := (Option = 1);
  pmiAutoDeleteAllTextBefore.Checked := (Option = 2);
end;

//------------------------------------------------------------------------------

procedure SaveFormPosition(IniFile : TIniFile; Form : TForm);
var
  WindowPlacement: TWindowPlacement;
begin
  WindowPlacement.length := Sizeof(WindowPlacement);
  GetWindowPlacement(Form.Handle, @WindowPlacement);
  IniFile.WriteInteger('Windows', Form.Name + '_Left',
    WindowPlacement.rcNormalPosition.Left);
  IniFile.WriteInteger('Windows', Form.Name + '_Top',
    WindowPlacement.rcNormalPosition.Top);
  IniFile.WriteInteger('Windows', Form.Name + '_Width',
    WindowPlacement.rcNormalPosition.Right - WindowPlacement.rcNormalPosition.Left);
  IniFile.WriteInteger('Windows', Form.Name + '_Height',
    WindowPlacement.rcNormalPosition.Bottom - WindowPlacement.rcNormalPosition.Top);
  IniFile.WriteInteger('Windows', Form.Name + '_State', Ord(Form.WindowState));
end;

//------------------------------------------------------------------------------

procedure LoadFormPosition(IniFile : TIniFile; Form : TForm);
var i : integer;
    MonRect, FormRect, InterRect : TRect;
    FormIsVisible : Boolean;
    MainMonitor : TMonitor;
    L,T,W,H : Integer;
begin
  L := IniFile.ReadInteger('Windows', Form.Name + '_Left', Form.Left);
  T := IniFile.ReadInteger('Windows', Form.Name + '_Top', Form.Top);
  W := IniFile.ReadInteger('Windows', Form.Name + '_Width', Form.Width);
  H := IniFile.ReadInteger('Windows', Form.Name + '_Height', Form.Height);
  Form.SetBounds(L, T, W, H);
  Form.WindowState := TWindowState(IniFile.ReadInteger('Windows', Form.Name + '_State', Ord(Form.WindowState)));
  // Put the form on main screen if it's not on any screen
  FormIsVisible := False;
  MainMonitor := nil;
  for i:=0 to Screen.MonitorCount-1 do
  begin
    MonRect := Screen.Monitors[i].BoundsRect;
    InflateRect(MonRect,-10,-10);
    GetWindowRect(Form.Handle,FormRect);
    if IntersectRect(InterRect,MonRect,FormRect) = True then
    begin
      FormIsVisible := True;
      Break;
    end;
    if Screen.Monitors[i].Primary then
      MainMonitor := Screen.Monitors[i];
  end;
  if (not FormIsVisible) then
  begin
    if Assigned(MainMonitor) then
    begin
      Form.Left := MainMonitor.Left + 100;
      Form.Top := MainMonitor.Top + 100;
    end
    else
    begin
      Form.Left := 100;
      Form.Top := 100;
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetIniFilename : WideString;
var ApplicationPath, IniFilename : WideString;
begin
  // By default place the ini file in the application directory
  Result := WideChangeFileExt(TntApplication.ExeName, '.ini');

  // Check if application path and ini file is writable
  ApplicationPath := WideExtractFilePath(TntApplication.ExeName);
  if WideFileIsReadOnly(ApplicationPath) or (WideFileExists(Result) and WideFileIsReadOnly(Result)) then
  begin
    // The ini file is in read only mode, work in user application data directory
    IniFilename := GetUserApplicationDataFolder;
    IniFilename := WideIncludeTrailingPathDelimiter(IniFilename);
    IniFilename := IniFilename + 'VisualSubSync.ini';
    Result := IniFilename;
  end;
end;

//------------------------------------------------------------------------------

function GetVTVColumnByName(Vtv : TVirtualStringTree; Name : WideString) : TVirtualTreeColumn;
var i : Integer;
    Column : TVirtualTreeColumn;
begin
  for i := 0 to Vtv.Header.Columns.Count-1 do
  begin
    Column := Vtv.Header.Columns.Items[i];
    if (Column.Text = Name) then
    begin
      Result := Column;
      Exit;
    end;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSettings;
var IniFile : TIniFile;
    IniFilename : WideString;
    i : Integer;
    VisibleExtraColumnsList : TStrings;
    ColumnsPositionList : TStrings;
    ColumnName : WideString;
    Column : TVirtualTreeColumn;
begin
  IniFilename := GetIniFilename;
  try
    IniFile := TIniFile.Create(IniFilename);
    MRUList.SaveIni(IniFile, 'MRUList');
    ConfigObject.SaveIni(IniFile);
    SaveFormPosition(IniFile, MainForm);
    SaveFormPosition(IniFile, ErrorReportForm);
    SaveFormPosition(IniFile, SuggestionForm);
    SaveFormPosition(IniFile, DetachedVideoForm);
    SaveFormPosition(IniFile, LogForm);
    SaveFormPosition(IniFile, FindForm);
    if PrefFormInitialized then
      SaveFormPosition(IniFile, PreferencesFormInstance);
    if Assigned(SilentZoneForm) then
      SaveFormPosition(IniFile, SilentZoneForm);
    if Assigned(SpellCheckForm) then
      SaveFormPosition(IniFile, SpellCheckForm);

    IniFile.WriteInteger('Windows', 'MainForm_PanelTop_Height', PanelTop.Height);
    IniFile.WriteInteger('Windows', 'MainForm_PanelBottom_Height', PanelBottom.Height);
    IniFile.WriteBool('Windows', 'DetachedVideo', MenuItemDetachVideoWindow.Checked);
    IniFile.WriteBool('Windows', 'ShowVideo', ShowingVideo);

    IniFile.WriteInteger('General', 'Volume', tbVolume.Position);

    // Save column settings
    VisibleExtraColumnsList := TStringList.Create;
    for i := 0 to GeneralJSPlugin.ExtraColumnsCount-1 do
    begin
      ColumnName := GeneralJSPlugin.GetColumnTitle(LAST_CORE_COL_INDEX + 1 + i);
      Column := GetVTVColumnByName(vtvSubsList, ColumnName);
      if Assigned(Column) and (coVisible in Column.Options)  then
      begin
        VisibleExtraColumnsList.Add(ColumnName);
      end;
    end;
    IniFile.WriteString('General', 'VisibleExtraColumns', VisibleExtraColumnsList.CommaText);
    VisibleExtraColumnsList.Free;

    ColumnsPositionList := TStringList.Create;
    for i := 0 to vtvSubsList.Header.Columns.Count-1 do
    begin
      Column := vtvSubsList.Header.Columns.Items[i];
      ColumnsPositionList.Add(IntToStr(Column.Position));
    end;
    IniFile.WriteString('General', 'ColumnsPosition', ColumnsPositionList.CommaText);
    ColumnsPositionList.Free;

    // Text pipe
    IniFile.WriteBool('TextPipe', 'ShowTextPipe', MemoTextPipe.Visible);
    IniFile.WriteInteger('TextPipe', 'TextPipeAutoOption', GetTextPipeAutoOption);

    IniFile.Free;
  except
    on E: Exception do MessageBox(Handle, PChar(E.Message), nil, MB_OK or MB_ICONERROR);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadSettings;
var IniFile : TIniFile;
    IniFilename : WideString;
begin
  ConfigObject.SetDefaultHotKeys(TntActionList1);

  IniFilename := GetIniFilename;
  IniFile := TIniFile.Create(IniFilename);

  MRUList.LoadIni(IniFile, 'MRUList');

  ConfigObject.LoadIni(IniFile, False);
  SetShortcut(IsTimingMode);
  ApplyMouseSettings;
  ApplyFontSettings;
  ApplyMiscSettings;

  LoadFormPosition(IniFile,MainForm);
  LoadFormPosition(IniFile,ErrorReportForm);
  LoadFormPosition(IniFile,SuggestionForm);
  LoadFormPosition(IniFile,DetachedVideoForm);
  LoadFormPosition(IniFile,LogForm);
  LoadFormPosition(IniFile,FindForm);

  Show;

  PanelTop.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelTop_Height',
    PanelTop.Height);
  PanelBottom.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelBottom_Height',
    PanelBottom.Height);
  StatusBarMainPanel.Top := Maxint;

  tbVolume.Position := IniFile.ReadInteger('General', 'Volume', 100);

  StartupDetachVideo := IniFile.ReadBool('Windows', 'DetachedVideo', False);
  StartupShowVideo := IniFile.ReadBool('Windows', 'ShowVideo', False);

  // Text pipe
  if IniFile.ReadBool('TextPipe', 'ShowTextPipe', False) then
    ActionShowHideTextPipe.Execute;
  SetTextPipeAutoOption(IniFile.ReadInteger('TextPipe', 'TextPipeAutoOption', 0));

  IniFile.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyMiscSettings;
begin
  WAVDisplayer.SceneChangeEnabled := ConfigObject.ShowSceneChange;
  WAVDisplayer.SceneChangeStartOffset := ConfigObject.SceneChangeStartOffset;
  WAVDisplayer.SceneChangeStopOffset := ConfigObject.SceneChangeStopOffset;
  WAVDisplayer.SceneChangeFilterOffset := ConfigObject.SceneChangeFilterOffset;
  WAVDisplayer.MinimumBlank := ConfigObject.SpaceKeyBlankBetweenSubtitles;
  WAVDisplayer.UpdateView([uvfRange]);
  g_SceneChangeWrapper.SetOffsets(ConfigObject.SceneChangeStartOffset,
    ConfigObject.SceneChangeStopOffset,
    ConfigObject.SceneChangeFilterOffset);
  g_SceneChangeWrapper.SetVisible(ConfigObject.ShowSceneChange);

  g_VSSCoreWrapper.SetCpsTarget(ConfigObject.SpaceKeyCPSTarget);
  g_VSSCoreWrapper.SetMinimumDuration(ConfigObject.SpaceKeyMinimalDuration);
  g_VSSCoreWrapper.SetMinimumBlank(ConfigObject.SpaceKeyBlankBetweenSubtitles);
  g_VSSCoreWrapper.SetMaximumDuration(ConfigObject.MaximumSubtitleDuration);

  MenuItemShowHidesSeneChange.Checked := ConfigObject.ShowSceneChange;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FinishLoadSettings;
begin
  if(ConfigObject.SwapSubtitlesList) then
    Self.SwapSubList(False);

  ApplyAutoBackupSettings;
end;

//------------------------------------------------------------------------------

procedure TMainForm.InitVTV;
var Column : TVirtualTreeColumn;
begin
  with vtvSubsList do
  begin
    Font.Name := 'Arial';
    NodeDataSize := SizeOf(TTreeData);
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReportMode];
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
      [toFullRowSelect, toMultiSelect, toDisableDrawSelection {,toExtendedFocus}];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines, toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines, toUseBlendedSelection
        {, toHotTrack, toAlwaysHideSelection}];

    Header.Options := Header.Options + [hoVisible, hoAutoResize, hoDrag];
    Header.Style := hsFlatButtons;

    Column := Header.Columns.Add;
    Column.Text := '#';
    Column.Width := 50;
    Column.MinWidth := 50;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Start';
    Column.Width := 100;
    Column.MinWidth := 100;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Stop';
    Column.Width := 100;
    Column.MinWidth := 100;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Style';
    Column.Width := 100;
    Column.MinWidth := 60;
    Column.Options := Column.Options - [coAllowClick];

    Column := Header.Columns.Add;
    Column.Text := 'Text';
    Column.Options := Column.Options - [coAllowClick];
    Column.Width := 500;
    Column.MinWidth := 100;

    //FocusedColumn := 2;

    OnGetText := vtvSubsListGetText;
  end;
  vtvSubsList.Header.AutoSizeIndex := TEXT_COL_INDEX;
end;

//------------------------------------------------------------------------------

procedure TMainForm.InitVTVExtraColumns;
var Column : TVirtualTreeColumn;
    i, NewPos : Integer;
    MenuItem : TTntMenuItem;
    VisibleExtraColumnsList, ColumnsPositionList : TStrings;

    IniFile : TIniFile;
    IniFilename : WideString;
    StartupVisibleExtraColumns, StartupColumnsPosition : string;
begin
  IniFilename := GetIniFilename;
  IniFile := TIniFile.Create(IniFilename);
  StartupVisibleExtraColumns := IniFile.ReadString('General', 'VisibleExtraColumns', '');
  StartupColumnsPosition := IniFile.ReadString('General', 'ColumnsPosition', '');
  IniFile.Free;

  VisibleExtraColumnsList := TStringList.Create;
  VisibleExtraColumnsList.CommaText := StartupVisibleExtraColumns;

  for i := 0 to GeneralJSPlugin.ExtraColumnsCount-1 do
  begin
    Column := vtvSubsList.Header.Columns.Add;
    Column.Text := GeneralJSPlugin.GetColumnTitle(LAST_CORE_COL_INDEX + 1 + i);
    Column.MinWidth := GeneralJSPlugin.GetColumnSize(LAST_CORE_COL_INDEX + 1 + i);
    Column.Options := Column.Options - [coAllowClick];
    if (VisibleExtraColumnsList.IndexOf(Column.Text) = -1) then
    begin
      Column.Options := Column.Options - [coVisible];
    end;
    MenuItem := TTntMenuItem.Create(SubListHeaderPopupMenu);
    MenuItem.Caption := Column.Text;
    MenuItem.OnClick := pmiToggleColumn;
    MenuItem.Checked := (coVisible in Column.Options);
    MenuItem.Tag := Integer(Column);
    Column.Tag := Integer(MenuItem);
    SubListHeaderPopupMenu.Items.Add(MenuItem);
  end;

  VisibleExtraColumnsList.Free;

  ColumnsPositionList := TStringList.Create;
  ColumnsPositionList.CommaText := StartupColumnsPosition;
  for i := 0 to vtvSubsList.Header.Columns.Count-1 do
  begin
    Column := vtvSubsList.Header.Columns.Items[i];
    if (i < ColumnsPositionList.Count) then
    begin
      NewPos := StrToIntDef(ColumnsPositionList[I], MaxInt);
      Column.Position := NewPos;
    end;
  end;
  ColumnsPositionList.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.InitGeneralJSPlugin;
var GeneralJSFilename : WideString;
begin
  // Init VSS Core Wrapper
  g_VSSCoreWrapper.Set_INDEX_COL_IDX(INDEX_COL_INDEX);
  g_VSSCoreWrapper.Set_START_COL_IDX(START_COL_INDEX);
  g_VSSCoreWrapper.Set_STOP_COL_IDX(STOP_COL_INDEX);
  g_VSSCoreWrapper.Set_STYLE_COL_IDX(STYLE_COL_INDEX);
  g_VSSCoreWrapper.Set_TEXT_COL_IDX(TEXT_COL_INDEX);
  g_VSSCoreWrapper.Set_LAST_CORE_COL_IDX(LAST_CORE_COL_INDEX);

  g_VSSCoreWrapper.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
  g_VSSCoreWrapper.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
  g_VSSCoreWrapper.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

  g_VSSCoreWrapper.SetVSSCoreEngine(Self);

  GeneralJSPlugin := TSimpleJavascriptWrapper.Create;
  GeneralJSPlugin.CoreColumnsCount := COLUMN_COUNT;
  GeneralJSFilename := g_PluginPath + 'general\general_plugin.js';
  if WideFileExists(GeneralJSFilename) then
  begin
    GeneralJSPlugin.OnJSPluginError := LogForm.LogMsg;
    if GeneralJSPlugin.LoadScript(GeneralJSFilename) then
    begin
      GeneralJSPlugin.OnJSPluginSetStatusBarText := OnJsSetStatusBarText;
      InitVTVExtraColumns;
    end
    else
      LogForm.SilentLogMsg('Can''t load ' + GeneralJSFilename);
  end
  else
    LogForm.SilentLogMsg('Can''t find ' + GeneralJSFilename);
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnRecentMenuItemClick(Sender : TObject);
var MenuItem : TTntMenuItem;
begin
  MenuItem := Sender as TTntMenuItem;
  LoadProject(MenuItem.Caption);
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetStatusBarPrimaryText(const Text : WideString);
begin
  StatusBarPrimaryText := Text;
  UpdateStatusBar;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateStatusBar;
var NewText : WideString;
begin
  if Length(StatusBarPrimaryText) > 0 then
  begin
    if Length(StatusBarSecondaryText) > 0 then
    begin
      NewText := StatusBarSecondaryText + ' (' + StatusBarPrimaryText + ')';
    end
    else
    begin
      NewText := StatusBarPrimaryText;
    end;
  end
  else
  begin
    NewText := StatusBarSecondaryText;
  end;


  if (NewText <> StatusBarPanel2.Caption) then
  begin
    StatusBarPanel2.Caption := NewText;
    StatusBarPanel2.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowStatusBarMessage(const Text : WideString;
  const Duration : Integer);
begin
  if (Text <> StatusBarSecondaryText) then
  begin
    StatusBarSecondaryText := Text;
    UpdateStatusBar;
  end;
  TimerStatusBarMsg.Interval := Duration;
  TimerStatusBarMsg.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerStatusBarMsgTimer(Sender: TObject);
begin
  StatusBarSecondaryText := '';
  TimerStatusBarMsg.Enabled := False;
  UpdateStatusBar;
end;

//------------------------------------------------------------------------------

procedure TMainForm.EnableStyleControls(Enable : Boolean);
begin
  ActionStyles.Enabled := Enable;
  cbStyles.Enabled := Enable;
  bttStyles.Enabled := Enable;
end;

//------------------------------------------------------------------------------

procedure TMainForm.EnableControl(Enable : Boolean);
begin
  ActionPlay.Enabled := Enable and (VideoRenderer.IsOpen or AudioOnlyRenderer.IsOpen);
  ActionStop.Enabled := ActionPlay.Enabled;
  ActionLoop.Enabled := ActionPlay.Enabled;
  ActionLoopSelStart.Enabled := ActionPlay.Enabled;
  ActionLoopSelEnd.Enabled := ActionPlay.Enabled;
  ActionNextSub.Enabled := Enable;
  ActionPreviousSub.Enabled := Enable;
  ActionPlaySelStart.Enabled := ActionPlay.Enabled;
  ActionPlaySelEnd.Enabled := ActionPlay.Enabled;
  ActionPlayToEnd.Enabled := ActionPlay.Enabled;
  ActionPlay1sBefore.Enabled := ActionPlay.Enabled;
  ActionPause.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate60.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate70.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate80.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate90.Enabled := ActionPlay.Enabled;
  ActionSetPlaybackRate100.Enabled := ActionPlay.Enabled;
  
  ActionToggleTimingMode.Enabled := ActionPlay.Enabled;
  bttWorkingMode.Enabled := ActionToggleTimingMode.Enabled;

  ActionZoomIn.Enabled := Enable;
  ActionZoomOut.Enabled := Enable;
  ActionZoomSelection.Enabled := Enable;
  ActionZoomAll.Enabled := Enable;
  ActionZoomVertical.Enabled := Enable;

  ActionFind.Enabled := Enable;
  ActionFindNext.Enabled := Enable;
  ActionGoto.Enabled := Enable;
  ActionCheckErrors.Enabled := Enable;
  ActionDelay.Enabled := Enable;
  ActionInsertTextFile.Enabled := Enable;

  ActionShowHideSceneChange.Enabled := Enable;
  ActionShowHideVideo.Enabled := Enable;
  ActionDetachVideo.Enabled := Enable;
  ActionProjectProperties.Enabled := Enable;
  ActionReload.Enabled := Enable;
  ActionSaveAs.Enabled := Enable;
  ActionExportToSSA.Enabled := Enable;
  ActionExportToWAV.Enabled := Enable;
  ActionShowStartFrame.Enabled := Enable and VideoRenderer.IsOpen;
  ActionShowStopFrame.Enabled := Enable and VideoRenderer.IsOpen;
  ActionShowFrameAtCursor.Enabled := Enable and VideoRenderer.IsOpen;

  vtvSubsList.Enabled := Enable;
  MemoLinesCounter.Enabled := Enable;
  MemoSubtitleText.Enabled := Enable and
    ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);

  edSelBegin.Enabled := Enable;
  edSelEnd.Enabled := Enable;
  edSelLength.Enabled := Enable;
  edViewBegin.Enabled := Enable;
  edViewEnd.Enabled := Enable;
  edViewLength.Enabled := Enable;

  chkAutoScrollWAVDisp.Enabled := Enable;
  chkAutoScrollSub.Enabled := Enable;
  lbAutoScroll.Enabled := Enable;

  ActionClose.Enabled := Enable;
  ActionSave.Enabled := False;
  ActionOpenProjectFolder.Enabled := Enable;  


  lblVolume.Enabled := Enable;
  tbVolume.Enabled := Enable;

  PanelVideo.Enabled := Enable;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1CursorChange(Sender: TObject);
begin
  if not WAVDisplayer.IsPlaying then
    plCursorPos.Caption := TimeMsToString(WAVDisplayer.GetCursorPos);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1PlayCursorChange(Sender: TObject);
begin
  plCursorPos.Caption := TimeMsToString(WAVDisplayer.GetPlayCursorPos);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectionChange(Sender: TObject);
begin
  edSelBegin.Text := TimeMsToString(WAVDisplayer.Selection.StartTime);
  edSelEnd.Text := TimeMsToString(WAVDisplayer.Selection.StopTime);
  edSelLength.Text := TimeMsToString(WAVDisplayer.Selection.StopTime -
    WAVDisplayer.Selection.StartTime);
  if (not WAVDisplayer.SelectionIsEmpty) then
  begin
    case PlayingMode of
      pmtAll: ; // do nothing
      pmtSelection:
        WAVDisplayer.UpdatePlayRange(WAVDisplayer.Selection.StartTime,
          WAVDisplayer.Selection.StopTime);
      pmtSelectionStart:
        WAVDisplayer.UpdatePlayRange(
          WAVDisplayer.Selection.StartTime,
          Min(WAVDisplayer.Selection.StartTime + StartEndPlayingDuration,WAVDisplayer.Selection.StopTime));
      pmtSelectionEnd:
        WAVDisplayer.UpdatePlayRange(
          Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
          WAVDisplayer.Selection.StopTime);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1ViewChange(Sender: TObject);
begin
  edViewBegin.Text := TimeMsToString(WAVDisplayer.Position);
  edViewEnd.Text := TimeMsToString(WAVDisplayer.Position + WAVDisplayer.PageSize);
  edViewLength.Text := TimeMsToString(WAVDisplayer.PageSize);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedRange(Sender: TObject; SelectedRange : TRange; IsDynamic : Boolean);
var Node : PVirtualNode;
begin
  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    Node := TSubtitleRange(WAVDisplayer.SelectedRange).Node;
    vtvSubsList.ScrollIntoView(Node,True);
    if vtvSubsList.FocusedNode <> Node then
      vtvSubsList.FocusedNode := Node
    else
      // special case for Autoscroll subtitles, force focus update
      vtvSubsListFocusChanged(vtvSubsList, Node, 0);
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[Node] := True;
    if (not MemoSubtitleText.Focused) and (MemoSubtitleText.Enabled) then
    begin
      MemoSubtitleText.SetFocus;
    end;
    if ShowingVideo and (not WAVDisplayer.IsPlaying) and (not IsDynamic) then
    begin
      ActionShowStartFrameExecute(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedRangeChange(Sender: TObject);
var SubRange : TSubtitleRange;
begin
  // Update subtitle timestamps
  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
    vtvSubsList.RepaintNode(SubRange.Node);
    DisableVideoUpdatePreview := True;
    CurrentProject.IsDirty := True;
    UpdateLinesCounter;
    DisableVideoUpdatePreview := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedRangeChanged(
  Sender: TObject; OldStart, OldStop : Integer; NeedSort : Boolean);
var SubRange : TSubtitleRange;
    UndoableSetTimeTask : TUndoableSetTimeTask;
begin
  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
    UndoableSetTimeTask := TUndoableSetTimeTask.Create;
    UndoableSetTimeTask.SetData(SubRange.Node.Index, OldStart, OldStop,
      SubRange.StartTime, SubRange.StopTime);
    UndoableSetTimeTask.DoTask;
    PushUndoableTask(UndoableSetTimeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1OnPeakFileCreation(Sender: TObject;
  EventType : TPeakFileCreationEventType; Param : Integer);
begin
  case EventType of
    pfcevtStart:
      begin
        PeakCreationProgressForm := TPeakCreationProgressForm.Create(nil);
        PeakCreationProgressForm.Show;
        Self.Enabled := False;
      end;
    pfcevtProgress:
      begin
        PeakCreationProgressForm.SetProgress(Param);
        Application.ProcessMessages;
      end;
    pfcevtStop:
      begin
        Self.Enabled := True;
        PeakCreationProgressForm.Free;
        PeakCreationProgressForm := nil;
        Application.ProcessMessages;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1AutoScrollChange(Sender: TObject);
begin
  chkAutoScrollWAVDisp.Checked := WAVDisplayer.AutoScrolling;
end;

//------------------------------------------------------------------------------

procedure TMainForm.chkAutoScrollWAVDispClick(Sender: TObject);
begin
  WAVDisplayer.AutoScrolling := chkAutoScrollWAVDisp.Checked;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CurrentProjectOnDirtyChange(Sender : TObject);
var s : WideString;
begin
  if CurrentProject.IsDirty then s := '*' else s := '';
  Self.Caption := ApplicationName + ' - ' +
    WideExtractFileName(CurrentProject.Filename) + s;
  ActionSave.Enabled := CurrentProject.IsDirty;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayPopup_DeleteRange(Sender: TObject);
var DeleteIdx : Integer;
    UndoableDeleteTask : TUndoableDeleteTask;
begin
  DeleteIdx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if (DeleteIdx <> -1) then
  begin
    UndoableDeleteTask := TUndoableDeleteTask.Create;
    UndoableDeleteTask.SetCapacity(1);
    UndoableDeleteTask.AddSubtitleIndex(DeleteIdx);
    UndoableDeleteTask.DoTask;
    PushUndoableTask(UndoableDeleteTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSplitAtCursorExecute(Sender: TObject);
var Idx : Integer;
    Range : TRange;
    UndoableSplitTask : TUndoableSplitTask;
begin
  Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if (Idx <> -1) then
  begin
    UndoableSplitTask := TUndoableSplitTask.Create;
    Range := WAVDisplayer.RangeList[Idx];
    UndoableSplitTask.SetData(Idx, Range.StartTime, Range.StopTime,
      WAVDisplayer.GetCursorPos, ConfigObject.SpaceKeyBlankBetweenSubtitles);
    UndoableSplitTask.DoTask;
    PushUndoableTask(UndoableSplitTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadSubtitles(Filename: WideString; var IsUTF8 : Boolean);
var Ext : WideString;
    StyleColumn : TVirtualTreeColumn;
    EnableStyle : Boolean;
begin
  EnableStyle := False;
  StyleColumn := vtvSubsList.Header.Columns.Items[STYLE_COL_INDEX];
  g_WebRWSynchro.BeginWrite;
  try
    vtvSubsList.BeginUpdate;
    Ext := WideLowerCase(WideExtractFileExt(Filename));
    if (Ext = '.srt') then
    begin
      LoadSRT(Filename, IsUTF8);
      StyleColumn.Options := StyleColumn.Options - [coVisible];
    end
    else if (Ext = '.ass') then
    begin
      EnableStyle := True;
      LoadASS(Filename, IsUTF8, False);
      StyleColumn.Options := StyleColumn.Options + [coVisible];
      StyleFormInstance.ConfigureMode(sfmASS);
    end
    else if (Ext = '.ssa') then
    begin
      EnableStyle := True;
      LoadASS(Filename, IsUTF8, True);
      StyleColumn.Options := StyleColumn.Options + [coVisible];
      StyleFormInstance.ConfigureMode(sfmSSA);
    end;
  finally
    g_WebRWSynchro.EndWrite;
    vtvSubsList.EndUpdate;
  end;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Header.AutoFitColumns(False);
  vtvSubsList.Repaint;
  
  UpdateStylesComboBox;

  EnableStyleControls(EnableStyle);
end;

//------------------------------------------------------------------------------

function TMainForm.LoadSRT(Filename: WideString; var IsUTF8 : Boolean) : Boolean;
var
  i : integer;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  SRTParser : TSRTParser;
  SRTSub : TSRTSubtitle;
begin
  Result := False;
  if not WideFileExists(Filename) then
  begin
    WAVDisplayer.ClearRangeList;
    vtvSubsList.Clear;
    Exit;
  end;

  SubtitleFileHeader := '';
  SubtitleFileFooter := '';
  WAVDisplayer.ClearRangeList;
  
  SRTParser := TSRTParser.Create;
  SRTParser.Load(Filename);
  IsUTF8 := SRTParser.IsUTF8;
  for i := 0 to SRTParser.GetCount-1 do
  begin
    SRTSub := SRTParser.GetAt(i);
    NewRange := SubRangeFactory.CreateRangeSS(SRTSub.Start, SRTSub.Stop);
    TSubtitleRange(NewRange).Text := SRTSub.Text;
    if (EnableExperimentalKaraoke = True) then
      NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);
    WAVDisplayer.RangeList.AddAtEnd(NewRange);
  end;
  WAVDisplayer.RangeList.FullSort;

  vtvSubsList.Clear;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    Node := vtvSubsList.AddChild(nil);
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeData.Range := TSubtitleRange(WAVDisplayer.RangeList[i]);
    TSubtitleRange(WAVDisplayer.RangeList[i]).Node := Node;
  end;
  WAVDisplayer.UpdateView([uvfRange]);
  if SRTParser.AutoCorrectedFile then
    CurrentProject.IsDirty := True;
  FreeAndNil(SRTParser);
  Result := True;
end;

//------------------------------------------------------------------------------

function TMainForm.LoadASS(Filename: WideString; var IsUTF8 : Boolean; isSSA : Boolean) : Boolean;
var
  i, Start, Stop : Integer;
  SubText : WideString;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  ssaParser : TSSAParser;
begin
  Result := False;
  if not WideFileExists(Filename) then
  begin
    SubtitleFileHeader := '';
    SubtitleFileFooter := '';
    WAVDisplayer.ClearRangeList;
    vtvSubsList.Clear;
    StyleFormInstance.ClearAll;
    StyleFormInstance.AddDefaultStyle;
    Exit;
  end;

  WAVDisplayer.ClearRangeList;

  ssaParser := TSSAParser.Create;
  ssaParser.Load(Filename);
  IsUTF8 := ssaParser.GetIsUTF8;
  SubtitleFileHeader := ssaParser.GetHeaderLines;
  SubtitleFileFooter := ssaParser.GetFooterLines;

  StyleFormInstance.LoadStylesFromParser(ssaParser);

  for i := 0 to ssaParser.GetDialoguesCount-1 do
  begin
    Start := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'Start'));
    Stop := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'End'));
    if (Start <> -1) and (Stop <> -1) then
    begin
      NewRange := SubRangeFactory.CreateRangeSS(Start, Stop);
      SubText := ssaParser.GetDialogueValueAsString(i, 'Text');
      TSubtitleRange(NewRange).Text := Tnt_WideStringReplace(SubText, '\N', CRLF,[rfReplaceAll]);
      if ssaParser.GetIsASS then
        TSubtitleRange(NewRange).Layer := ssaParser.GetDialogueValueAsString(i, 'Layer')
      else
        TSubtitleRange(NewRange).Marked := ssaParser.GetDialogueValueAsString(i, 'Marked');
      TSubtitleRange(NewRange).Effect := ssaParser.GetDialogueValueAsString(i, 'Effect');
      TSubtitleRange(NewRange).RightMarg := ssaParser.GetDialogueValueAsString(i, 'MarginR');
      TSubtitleRange(NewRange).LeftMarg := ssaParser.GetDialogueValueAsString(i, 'MarginL');
      TSubtitleRange(NewRange).VertMarg := ssaParser.GetDialogueValueAsString(i, 'MarginV');
      TSubtitleRange(NewRange).Style := ssaParser.GetDialogueValueAsString(i, 'Style');
      TSubtitleRange(NewRange).Actor := ssaParser.GetDialogueValueAsString(i, 'Name');
      if (EnableExperimentalKaraoke = True) then
        NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);
      WAVDisplayer.RangeList.AddAtEnd(NewRange);
    end;
  end;
  ssaParser.Free;

  WAVDisplayer.RangeList.FullSort;

  vtvSubsList.Clear;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    Node := vtvSubsList.AddChild(nil);
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeData.Range := TSubtitleRange(WAVDisplayer.RangeList[i]);
    TSubtitleRange(WAVDisplayer.RangeList[i]).Node := Node;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  NodeData: PTreeData;
  CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  NodeData := Sender.GetNodeData(Node);

  case Column of
    -1: CellText := ''; // -1 if columns are hidden
    INDEX_COL_INDEX: CellText := IntToStr(Node.Index+1);
    START_COL_INDEX: CellText := TimeMsToString(NodeData.Range.StartTime);
    STOP_COL_INDEX: CellText := TimeMsToString(NodeData.Range.StopTime);
    STYLE_COL_INDEX: CellText := NodeData.Range.Style;
    TEXT_COL_INDEX: CellText := StringConvertCRLFToPipe(NodeData.Range.Text);
  end;

  if GeneralJSPlugin.HasColumnCustomText(Column) then
  begin
    GetCurrentPreviousNextSubtitles(Node, CurrentSub, PreviousSub, NextSub);
    CellText := GeneralJSPlugin.GetColumnText(Column, CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Msg : TCMMouseWheel;
  TargetControl : HWND;
begin
  // Recreate original CM_MOUSEWHEEL Message
  Msg.ShiftState := Shift;
  Msg.WheelDelta := WheelDelta;
  Msg.XPos := MousePos.X;
  Msg.YPos := MousePos.Y;

  // Find the control under the mouse pointer
  TargetControl := WindowFromPoint(MousePos);

  if (TargetControl = WAVDisplayer.Handle) then
  begin
    Handled := WAVDisplayer.Perform(CM_MOUSEWHEEL, TMessage(Msg).WParam,
      TMessage(Msg).LParam) <> 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagHighlight(RichEdit : TTntRichEdit; TagIndex : Integer);
var savSelStart, savSelLength : Integer;
    i, j, eventMask, len : Integer;
    WordArray : TWideStringDynArray;
    WordColor : TColor;
    TextToSpell, AWord : WideString;
    Offset : Integer;
    SpellOk : Boolean;
    WordInfo : TWordInfo;    
begin
  RichEdit.Tag := 0;
  savSelStart := RichEdit.SelStart;
  savSelLength := RichEdit.SelLength;

  TagSplit(RichEdit.Text, WordArray);

  eventMask := RichEdit.Perform(EM_SETEVENTMASK, 0, 0);

  RichEdit.Lines.BeginUpdate;

  RichEdit.SelectAll;
  SetRESelectionColor(RichEdit, clWindowText);
  SetReUnderline(RichEdit, False);

  j := 0;
  for i:=0 to Length(WordArray)-1 do
  begin
    if (i = (TagIndex*2)+1) then
      WordColor := clRed
    else if (i mod 2) = 0 then
      WordColor := clWindowText
    else
      WordColor := clGrayText;

    len := Length(WordArray[i]);
    if (WordColor <> clWindowText) then
    begin
      // Richedit count line break as 1 character only
      SetRESelection(RichEdit, j, len - WideStringCount(WordArray[i], CRLF));
      SetRESelectionColor(RichEdit, WordColor);
    end
    else
    begin
      // Spellcheck
      if (MenuItemLiveSpellcheck.Checked and FSpellChecker.IsInitialized) then
      begin
        Offset := 1;
        TextToSpell := WordArray[i];
        while (Offset <= Length(TextToSpell)) do
        begin
          GetNextWordPos(Offset, TextToSpell, WordInfo);
          AWord := Copy(TextToSpell, WordInfo.Position, WordInfo.Length);
          SpellOk := ContainDigit(AWord) or IsUpperCase(AWord) or FSpellChecker.Spell(AWord);
          if (not SpellOk) then
          begin
            SetRESelection(RichEdit, j + WordInfo.Position - 1, WordInfo.Length);
            SetReUnderline(RichEdit, not SpellOk);
          end;
          Inc(Offset);
        end;
      end;
    end;
    
    Inc(j, len);
  end;
  SetRESelection(RichEdit, savSelStart, savSelLength);
  
  RichEdit.Lines.EndUpdate;

  RichEdit.Perform(EM_SETEVENTMASK, 0, eventMask);

  RichEdit.Tag := 1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubtitleTextChange(Sender: TObject);
var NodeData: PTreeData;
    NeedUpdate : Boolean;
begin
  if Assigned(vtvSubsList.FocusedNode) and (MemoSubtitleText.Tag = 1) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

    g_WebRWSynchro.BeginWrite;
    try
      NodeData.Range.Text := MemoSubtitleText.Text;

      NeedUpdate := False;
      if (EnableExperimentalKaraoke = True) then
        NeedUpdate := NodeData.Range.UpdateSubTimeFromText(NodeData.Range.Text);

      vtvSubsList.RepaintNode(vtvSubsList.FocusedNode);
      CurrentProject.IsDirty := True;
    finally
      g_WebRWSynchro.EndWrite;
    end;

    if (NeedUpdate = True) then
    begin
      WAVDisplayer.UpdateView([uvfRange]);
    end;

    if (EnableExperimentalKaraoke = True) then
      TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
  UpdateLinesCounter;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubtitleTextSelectionChange(Sender: TObject);
var TopTask : TUndoableTask;
begin
  UpdateLinesCounter;

  { TODO
  OutputDebugString(PChar(BoolToStr(TTntRichEditCustomUndo(MemoSubtitleText).HasTextChanged, True)));
  if (UndoStack.Count > 0) then
  begin
    TopTask := TUndoableTask(UndoStack.Peek);
    if (TopTask is TUndoableSubTextTask) then
    begin
      TUndoableTextTask(TUndoableSubTextTask(TopTask).GetSubTask).DisableMerge;
    end;
  end;}
  
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateLinesCounter;
var s, StrippedText : WideString;
    i, CharCount, TotalCharCount, TotalCharCountWithCRLF, CPS : Integer;
    LineNo, LineStart, ColNo : Integer;
    NodeData: PTreeData;
    SubDuration : Integer;
    LineArray : TWideStringDynArray;
begin
  s := '';
  TotalCharCount := 0;
  TotalCharCountWithCRLF := 0;
  StrippedText := StripTagsKeepNL(MemoSubtitleText.Text);
  ExplodeW(CRLF, StrippedText, LineArray);

  for i := MemoSubtitleText.Perform(EM_GETFIRSTVISIBLELINE, 0, 0) to MemoSubtitleText.Lines.Count-1 do
  begin
    CharCount := Length(LineArray[i]);
    Inc(TotalCharCount, CharCount);
    s := s + IntToStr(CharCount) + CRLF;
  end;
  for i := MemoSubtitleText.Lines.Count to MemoSubtitleText.Perform(EM_GETLINECOUNT,0,0)-1 do
  begin
    s := s + '0' + CRLF;
  end;
  MemoLinesCounter.Text := Trim(s);

  // line return count for 2 chars
  if (MemoSubtitleText.Lines.Count > 0) then
    TotalCharCountWithCRLF := TotalCharCount + ((MemoSubtitleText.Lines.Count-1)*2);

  LineNo := MemoSubtitleText.Perform(EM_LINEFROMCHAR, -1, 0);
  LineStart := MemoSubtitleText.Perform(EM_LINEINDEX, LineNo, 0);
  ColNo := MemoSubtitleText.SelStart + MemoSubtitleText.SelLength - LineStart - LineNo + 1;

  s := 'Line: ' + IntToStr(LineNo+1) + ', Column: ' + IntToStr(ColNo);
  s := s + ' | Total: ' + IntToStr(TotalCharCount);
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    SubDuration := NodeData.Range.StopTime - NodeData.Range.StartTime;
    if (SubDuration > 0) then
      CPS := Round(TotalCharCountWithCRLF / (SubDuration / 1000))
    else
      CPS := -1;
    s := s + ', Char/s: ' + IntToStr(CPS);
  end;

  StatusBarPanel1.Caption := s;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitles(Filename: WideString; PreviousFilename : WideString;
  InUTF8 : Boolean; BackupOnly : Boolean);
var Ext, PreviousExt : WideString;
    BackupDstFilename : WideString;
begin
  if (not BackupOnly) and WideFileExists(Filename) and ConfigObject.EnableBackup then
  begin
    CheckBackupDirectory;
    BackupDstFilename := g_BackupDirectory + WideChangeFileExt(WideExtractFileName(Filename), '.bak');
    WideCopyFile(Filename, BackupDstFilename, False);
  end;

  Ext := WideLowerCase(WideExtractFileExt(Filename));
  PreviousExt := WideLowerCase(WideExtractFileExt(PreviousFilename));
  if (Ext = '.srt') then
    SaveSubtitlesAsSRT(Filename, PreviousExt, InUTF8)
  else if (Ext = '.ass') then
    SaveSubtitlesAsASS(Filename, PreviousExt, InUTF8)
  else if (Ext = '.ssa') then
    SaveSubtitlesAsSSA(Filename, PreviousExt, InUTF8)
  else if (Ext = '.cue') then
    SaveSubtitlesAsCUE(Filename)
  else if (Ext = '.txt') then
    SaveSubtitlesAsTXT(Filename, PreviousExt, InUTF8);

  if (not BackupOnly) then
  begin
    ApplyAutoBackupSettings; // Reset auto-backup timing
    CurrentProject.IsDirty := False
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListDblClick(Sender: TObject);
var NodeData: PTreeData;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    WAVDisplayer.ZoomAndSelectRange(NodeData.Range);
    if WAVDisplayer.IsPlaying then
      WAVDisplayer.AutoScrolling := False;
    if ShowingVideo and Assigned(Sender) and (not WAVDisplayer.IsPlaying) then
    begin
      ActionShowStartFrameExecute(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateVideoRendererWindow;
begin
  if (CurrentProject.VideoPanelWidth > 0) then
  begin
    PanelVideo.Width := CurrentProject.VideoPanelWidth;
  end
  else if (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
  begin
    PanelVideo.Width := (VideoRenderer.VideoWidth * PanelVideo.Height) div VideoRenderer.VideoHeight;
  end;
  
  if MenuItemDetachVideoWindow.Checked then
    VideoRenderer.SetDisplayWindow(DetachedVideoForm.Handle)
  else
    VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadVideoSceneChange;
var SceneChangeFileName : WideString;
    SCArray :TIntegerDynArray;
begin
  // Check for a '.scenechange' file
  SceneChangeFileName := WideChangeFileExt(CurrentProject.VideoSource,
    '.scenechange');
  
  if WideFileExists(SceneChangeFileName) then
  begin
    LoadSceneChange(SceneChangeFileName, SCArray);
  end
  else
  begin
    // Extract scene change
    ShowStatusBarMessage('Extracting keyframes...');
    ExtractSceneChange(CurrentProject.VideoSource, SCArray);
    SaveSceneChange(SceneChangeFileName, SCArray);
  end;

  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  g_SceneChangeWrapper.SetSceneChangeList(SCArray);
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadProject(Filename : WideString);
var
  CM : ICursorManager;
  LoadWAVOK : Boolean;
  Idx : Integer;
begin
  if (not WideFileExists(Filename)) then
  begin
    MessageBoxW(Handle, PWideChar(WideFormat('The project file %s doesn''t exist.', [Filename])),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    Exit;
  end;

  if not CheckSubtitlesAreSaved then
  begin
    Exit;
  end;
  CloseProject;

  g_WebRWSynchro.BeginWrite;
  CM := TCursorManager.Create(crHourGlass);
  try
    CurrentProject.LoadFromINIFile(Filename);

    // ----- Load waveform
    ShowStatusBarMessage('Loading waveform...');
    LoadWAVOK := False;
    if CurrentProject.WAVMode = pwmPeakOnly then
    begin
      if WideFileExists(CurrentProject.PeakFile) then
        LoadWAVOK := WAVDisplayer.LoadWAV(WideChangeFileExt(CurrentProject.PeakFile,'.wav'))
      else
      begin
        if WideFileExists(CurrentProject.WAVFile) then
        begin
          CurrentProject.WAVMode := pwmExternal;
          LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
          CurrentProject.IsDirty := True;
          // Show warning
          MessageBoxW(Handle, PWideChar(WideFormat(
          'Peak file %s hasn''t been found, project has been switched to "WAV file" mode', [CurrentProject.PeakFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end
        else
        begin
          MessageBoxW(Handle, PWideChar(WideFormat('Can''t open peak file : %s',
            [CurrentProject.PeakFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end;
      end;
    end
    else if CurrentProject.WAVMode = pwmExternal then
    begin
      if WideFileExists(CurrentProject.WAVFile) then
      begin
        LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
      end
      else
      begin
        if WideFileExists(CurrentProject.PeakFile) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          LoadWAVOK := WAVDisplayer.LoadWAV(WideChangeFileExt(CurrentProject.PeakFile,'.wav'));
          CurrentProject.IsDirty := True;
          // Show warning
          MessageBoxW(Handle, PWideChar(WideFormat(
          'WAV file %s hasn''t been found, project has been switched to "Peak file only" mode', [CurrentProject.WAVFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end
        else if WideFileExists(WideChangeFileExt(CurrentProject.WAVFile,'.peak')) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          CurrentProject.PeakFile := WideChangeFileExt(CurrentProject.WAVFile,'.peak');
          LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
          CurrentProject.IsDirty := True;
        end
        else
        begin
          MessageBoxW(Handle, PWideChar(WideFormat('Can''t open WAV file : %s',
            [CurrentProject.WAVFile])),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end;
      end;
    end;
    // TODO : report invalid wav file error
    
    WAVDisplayer.Enabled := True;

    // ----- Load subtitles
    ShowStatusBarMessage('Loading subtitles...');
    LoadSubtitles(CurrentProject.SubtitlesFile,CurrentProject.IsUTF8);

    // ----- Load audio
    ShowStatusBarMessage('Loading audio file...');
    if not AudioOnlyRenderer.Open(CurrentProject.WAVFile) then
    begin
      if AudioOnlyRenderer.Open(CurrentProject.VideoSource) then
      begin
        AudioOnlyRenderer.KillVideo;
      end;
    end;

    // ----- Load video
    ShowStatusBarMessage('Loading video file...');
    if VideoRenderer.Open(CurrentProject.VideoSource) then
    begin
      if (CurrentProject.DetachedVideo <> MenuItemDetachVideoWindow.Checked) then
        ActionDetachVideoExecute(nil);
      if (CurrentProject.ShowVideo <> ShowingVideo) then
        ActionShowHideVideoExecute(nil);
      UpdateVideoRendererWindow;
      VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
      LoadVideoSceneChange;
      DetachedVideoForm.SetNormalPos(CurrentProject.VideoWindowNormalLeft,
        CurrentProject.VideoWindowNormalTop, CurrentProject.VideoWindowNormalWidth,
        CurrentProject.VideoWindowNormalHeight);
      g_VSSCoreWrapper.SetVideoWidth(VideoRenderer.VideoWidth);
      g_VSSCoreWrapper.SetVideoHeight(VideoRenderer.VideoHeight);
    end
    else
    begin
      g_VSSCoreWrapper.SetVideoWidth(0);
      g_VSSCoreWrapper.SetVideoHeight(0);
    end;

    if ShowingVideo then
      WAVDisplayer.SetRenderer(VideoRenderer)
    else
      WAVDisplayer.SetRenderer(AudioOnlyRenderer);

    if (not LoadWAVOK) then
    begin
      if VideoRenderer.IsOpen then
      begin
        // Extract duration form video
        WAVDisplayer.Length := VideoRenderer.GetDuration;
      end
      else
      begin
        if (WAVDisplayer.RangeList.Count > 0) then
        begin
          // Use last subtitle stop time + 1 minute
          WAVDisplayer.Length := WAVDisplayer.RangeList[
            WAVDisplayer.RangeList.Count - 1].StopTime + (1 * 60 * 1000);
        end
        else
        begin
          // TODO : field to enter duration or auto extend duration ?
          WAVDisplayer.Length := 1 * 3600 * 1000; // 1 hour by default
        end;
      end;
    end;

    // ----- Load script
    if WideFileExists(WideChangeFileExt(CurrentProject.Filename,'.vssscript')) then
    begin
      ShowStatusBarMessage('Loading script file...');
      MemoTextPipe.Lines.LoadFromFile(WideChangeFileExt(CurrentProject.Filename,'.vssscript'));
      // First go to bottom, so the line we want will be at the top
      MemoTextPipe.SelStart := Length(MemoTextPipe.Text);
      Perform(EM_SCROLLCARET, 0, 0);
      MemoTextPipe.SelStart := CurrentProject.TextPipePosition;
      Perform(EM_SCROLLCARET, 0, 0);
    end;

    // ----- Restore position ---
    if (CurrentProject.WAVDisplayerPositionStartMs <> -1) and
       (CurrentProject.WAVDisplayerPositionStopMs <> -1) then
    begin
      WAVDisplayer.ZoomRange(CurrentProject.WAVDisplayerPositionStartMs,
        CurrentProject.WAVDisplayerPositionStopMs);
    end
    else
    begin
      // Zoom in at beginning
      WAVDisplayer.ZoomRange(0,15000);
    end;
    
    if (CurrentProject.FocusedTimeMs <> -1) then
    begin
      Idx := WAVDisplayer.RangeList.FindInsertPos(CurrentProject.FocusedTimeMs, -1);
      SelectNodeAtIndex(Idx - 1);
    end;

    // ----- Load dictionnary ---
    Idx := FSpellChecker.GetDictIdx(CurrentProject.Dictionnary);
    if (Idx <> -1) then
    begin
      LoadDict(Idx);
    end;

    // ------ Load presets -----
    LoadPresetFile(g_PresetsPath + CurrentProject.Presets);

    UpdateVolume;
    EnableControl(True);

    CurrentProjectOnDirtyChange(nil);
  finally
    g_GlobalContext.WavAverageBytePerSecond := WAVDisplayer.GetWAVAverageBytePerSecond;
    g_WebRWSynchro.EndWrite;
    // Hide the video
    if (not VideoRenderer.IsOpen) and ShowingVideo then
        ActionShowHideVideo.Execute;
  end;

  WAVDisplayer.UpdateView([uvfPageSize]);
  MRUList.AddFile(CurrentProject.Filename);
  ShowStatusBarMessage('Project loaded.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveProject(Project : TVSSProject; SilentSave : Boolean);
var
  ProjectFileIni : TTntIniFile;
  NodeData : PTreeData;
  FocusedTimeMs : Integer;
begin
  if WideFileIsReadOnly(Project.Filename) then
  begin
    if not SilentSave then
    begin
      MessageBoxW(Handle, PWideChar(WideFormat('The project file %s can''t be saved because it''s in read only mode.', [Project.Filename])),
        PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    end;
    Exit;
  end;

  ProjectFileIni := TTntIniFile.Create(Project.Filename);
  ProjectFileIni.WriteString('VisualSubsync','VideoSource',
    WideMakeRelativePath(Project.Filename, Project.VideoSource));
  ProjectFileIni.WriteString('VisualSubsync','WAVFile',
    WideMakeRelativePath(Project.Filename, Project.WAVFile));
  ProjectFileIni.WriteString('VisualSubsync','PeakFile',
    WideMakeRelativePath(Project.Filename, Project.PeakFile));
  ProjectFileIni.WriteInteger('VisualSubsync','WAVMode', Ord(Project.WAVMode));
  ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile',
    WideMakeRelativePath(Project.Filename, Project.SubtitlesFile));

  if Length(Trim(MemoTextPipe.Text)) > 0 then
  begin
    MemoTextPipe.PlainText := False;
    MemoTextPipe.Lines.SaveToFile(WideChangeFileExt(Project.Filename,'.vssscript'));
    // Save cursor position
    ProjectFileIni.WriteInteger('VisualSubsync', 'TextPipePosition',
      MemoTextPipe.SelStart);
    ProjectFileIni.WriteString('VisualSubsync', 'TextPipeSource',
      WideMakeRelativePath(Project.Filename, Project.TextPipeSource));
  end;

  ProjectFileIni.WriteInteger('VisualSubsync','VideoPanelWidth', Project.VideoPanelWidth);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoPanelHeight', Project.VideoPanelHeight);
  ProjectFileIni.WriteBool('VisualSubsync','ShowVideo', Project.ShowVideo);
  ProjectFileIni.WriteBool('VisualSubsync','DetachedVideo', Project.DetachedVideo);

  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalLeft', DetachedVideoForm.NormalLeft);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalTop', DetachedVideoForm.NormalTop);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalWidth', DetachedVideoForm.NormalWidth);
  ProjectFileIni.WriteInteger('VisualSubsync','VideoWindowNormalHeight', DetachedVideoForm.NormalHeight);

  ProjectFileIni.WriteInteger('VisualSubsync', 'WAVDisplayerPositionStartMs', WAVDisplayer.Position);
  ProjectFileIni.WriteInteger('VisualSubsync', 'WAVDisplayerPositionStopMs', WAVDisplayer.Position + WAVDisplayer.PageSize);

  FocusedTimeMs := -1;
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    FocusedTimeMs := NodeData.Range.StartTime + ((NodeData.Range.StopTime - NodeData.Range.StartTime) div 2);
  end;
  ProjectFileIni.WriteInteger('VisualSubsync', 'FocusedTimeMs', FocusedTimeMs);

  ProjectFileIni.WriteString('VisualSubsync', 'Dictionnary', FSpellChecker.GetCurrentDictName);

  ProjectFileIni.WriteString('VisualSubsync','Presets', Project.Presets);

  ProjectFileIni.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomInExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomIn;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomOutExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomOut;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomSelectionExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomRange(WAVDisplayer.Selection);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomAllExecute(Sender: TObject);
begin
  WAVDisplayer.ZoomAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlayExecute(Sender: TObject);
begin
  if IsTimingMode then
    PanelWAVDisplay.SetFocus;
  WAVDisplayer.AutoScrolling := True;
  UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
  if WAVDisplayer.SelectionIsEmpty then
  begin
    PlayingMode := pmtAll;
    WAVDisplayer.PlayRange(WAVDisplayer.GetCursorPos,WAVDisplayer.Length)
  end
  else
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopExecute(Sender: TObject);
begin
  WAVDisplayer.Stop;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopExecute(Sender: TObject);
begin
  if IsTimingMode then
    PanelWAVDisplay.SetFocus;
  WAVDisplayer.AutoScrolling := True;
  UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
  if WAVDisplayer.SelectionIsEmpty then
  begin
    PlayingMode := pmtAll;
    WAVDisplayer.PlayRange(WAVDisplayer.GetCursorPos,WAVDisplayer.Length,True)
  end
  else
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection,True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  if CheckSubtitlesAreSaved then
  begin
    SaveSettings;
    Application.Terminate;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNewProjectExecute(Sender: TObject);
var
  ProjectFileIni : TTntIniFile;
begin
  // First close current project
  if not CheckSubtitlesAreSaved then
  begin
    Exit;
  end;
  CloseProject;
  
  // New project
  ProjectForm.ConfigureInNewProjectMode;
  if (ProjectForm.ShowModal = mrOK) then
  begin
    // Create the project file
    ProjectFileIni := TTntIniFile.Create(ProjectForm.EditProjectFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','VideoSource',ProjectForm.EditVideoFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','WAVFile',ProjectForm.GetWAVFilename);
    ProjectFileIni.WriteString('VisualSubsync','PeakFile',ProjectForm.GetPeakFilename);
    ProjectFileIni.WriteInteger('VisualSubsync','WAVMode',Ord(ProjectForm.GetWAVMode));
    ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile',ProjectForm.EditSubtitleFilename.Text);
    ProjectFileIni.WriteBool('VisualSubsync','DetachedVideo',StartupDetachVideo);
    ProjectFileIni.WriteBool('VisualSubsync','ShowVideo',StartupShowVideo);
    ProjectFileIni.Free;

    // Load the project
    LoadProject(ProjectForm.EditProjectFilename.Text);
    CurrentProject.IsUTF8 := ProjectForm.chkSaveAsUTF8.Checked;
    // TODO : Maybe create an empty file to keep UTF8 info if we quit
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideVideoExecute(Sender: TObject);
begin
  if (not VideoRenderer.IsOpen) then
  begin
    ShowStatusBarMessage('Video file is not opened...');
    Exit;
  end;

  ShowingVideo := not ShowingVideo;

  if MenuItemDetachVideoWindow.Checked then
  begin
    if ShowingVideo then
      ShowWindow(DetachedVideoForm.Handle, SW_SHOWNOACTIVATE); // Don't give focus
    DetachedVideoForm.Visible := ShowingVideo;
  end
  else
  begin
    PanelVideo.Visible := ShowingVideo;
    SplitterWAVDisplay_Video.Visible := ShowingVideo;
  end;

  if ShowingVideo then
  begin
    UpdateSubtitleForPreview(False);
    WAVDisplayer.SetRenderer(VideoRenderer);
  end
  else
  begin
    WAVDisplayer.SetRenderer(AudioOnlyRenderer);
  end;
  WAVDisplayer.UpdateView([uvfPageSize]);

  if Assigned(CurrentProject) and (ShowingVideo <> CurrentProject.ShowVideo) then
  begin
    CurrentProject.ShowVideo := ShowingVideo;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionOpenProjectExecute(Sender: TObject);
begin
  TntOpenDialog1.Filter := 'VSS project (*.vssprj)|*.VSSPRJ' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    Self.Repaint;
    LoadProject(TntOpenDialog1.FileName);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  // Save subtitles
  if Trim(CurrentProject.SubtitlesFile) <> '' then
  begin
    if (not CurrentProject.IsUTF8) and DetectCharsetDataLoss then
    begin
      MessageBoxW(Handle, PWideChar(WideString(
        'Subtitles contain some characters that may not be saved correctly,' +
        ' the file will be saved in Unicode UTF-8 starting from now.')),
        PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
      CurrentProject.IsUTF8 := True;
    end;
    SaveSubtitles(CurrentProject.SubtitlesFile, '', CurrentProject.IsUTF8, False);
    if VideoRenderer.IsOpen then
      VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
  end;
  SaveProject(CurrentProject, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
var InputExt : WideString;
    NewExt : WideString;
begin
  TntSaveDialog1.Filter := 'SRT files (*.srt)|*.SRT' + '|' +
    'SSA files (*.ssa)|*.SSA' + '|' +
    'ASS files (*.ass)|*.ASS' + '|' +
    'CUE files (*.cue)|*.CUE' + '|' +
    'TXT files (*.txt)|*.TXT' + '|' +
    'All files (*.*)|*.*';
  TntSaveDialog1.FileName := CurrentProject.SubtitlesFile;
  // Preselect format
  InputExt := WideLowerCase(WideExtractFileExt(CurrentProject.SubtitlesFile));
  if (InputExt = '.srt') then
    TntSaveDialog1.FilterIndex := 1
  else if (InputExt = '.ssa') then
    TntSaveDialog1.FilterIndex := 2
  else if (InputExt = '.ass') then
    TntSaveDialog1.FilterIndex := 3
  else
    TntSaveDialog1.FilterIndex := 6;

  if TntSaveDialog1.Execute then
  begin
    // Force extension filter
    case TntSaveDialog1.FilterIndex of
      1 : NewExt := '.srt';
      2 : NewExt := '.ssa';
      3 : NewExt := '.ass';
      4 : NewExt := '.cue';
      5 : NewExt := '.txt';
    end;
    TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, NewExt);
    SaveSubtitles(TntSaveDialog1.FileName, CurrentProject.SubtitlesFile,
      CurrentProject.IsUTF8, False);
  end;
end;

//------------------------------------------------------------------------------

function HasExtensionChanged(OldFilename, NewFilename : WideString) : Boolean;
var OldExt, NewExt : WideString;
    OldWithDummyExt, NewWithDummyExt : WideString;
begin
  Result := False;
  OldExt := WideLowerCase(WideExtractFileExt(OldFilename));
  NewExt := WideLowerCase(WideExtractFileExt(NewFilename));
  if (OldExt = NewExt) then
    Exit;

  OldWithDummyExt := WideChangeFileExt(OldFilename, '.dummy');
  NewWithDummyExt := WideChangeFileExt(NewFilename, '.dummy');
  Result := (OldWithDummyExt = NewWithDummyExt);
end;

procedure TMainForm.ActionProjectPropertiesExecute(Sender: TObject);
var ProjectHasChanged, VideoHasChanged, SubtitleFileHasChanged : Boolean;
    PreviousSubtitlesFile : WideString;
begin
  // TODO : test this more

  ProjectForm.ConfigureInModifyProjectMode(CurrentProject);
  if (ProjectForm.ShowModal = mrOK) then
  begin
    g_WebRWSynchro.BeginWrite;

    try
      // Check the thing that have changed and update
      VideoHasChanged := False;
      ProjectHasChanged := False;
      SubtitleFileHasChanged := False;


      if (CurrentProject.SubtitlesFile <> ProjectForm.EditSubtitleFilename.Text) then
      begin
        CheckSubtitlesAreSaved;

        // Check if it's a file format change
        if HasExtensionChanged(CurrentProject.SubtitlesFile, ProjectForm.EditSubtitleFilename.Text) then
        begin
          PreviousSubtitlesFile := CurrentProject.SubtitlesFile;
          CurrentProject.SubtitlesFile := ProjectForm.EditSubtitleFilename.Text;

          if not WideFileExists(CurrentProject.SubtitlesFile) then
          begin
            if (not CurrentProject.IsUTF8) and DetectCharsetDataLoss then
            begin
              MessageBoxW(Handle, PWideChar(WideString(
                'Subtitles contain some characters that may not be saved correctly,' +
                ' the file will be saved in Unicode UTF-8 starting from now.')),
                PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
              CurrentProject.IsUTF8 := True;
            end;
            SaveSubtitles(CurrentProject.SubtitlesFile, PreviousSubtitlesFile,
              CurrentProject.IsUTF8, False);
          end;
        end;

        CurrentProject.SubtitlesFile := ProjectForm.EditSubtitleFilename.Text;
        ErrorReportForm.Clear;
        LoadSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
        
        ProjectHasChanged := True;
        SubtitleFileHasChanged := True;
      end;

      if (CurrentProject.VideoSource <> ProjectForm.EditVideoFilename.Text) then
      begin
        CurrentProject.VideoSource := ProjectForm.EditVideoFilename.Text;
        VideoRenderer.Open(CurrentProject.VideoSource);
        CurrentProject.VideoPanelWidth := 0;
        CurrentProject.VideoPanelHeight := 0;
        UpdateVideoRendererWindow;
        ProjectHasChanged := True;
        VideoHasChanged := True;
        LoadVideoSceneChange;
        g_VSSCoreWrapper.SetVideoWidth(VideoRenderer.VideoWidth);
        g_VSSCoreWrapper.SetVideoHeight(VideoRenderer.VideoHeight);
      end;

      if (CurrentProject.WAVMode <> ProjectForm.GetWAVMode) then
      begin
        CurrentProject.WAVMode := ProjectForm.GetWAVMode;
        if (CurrentProject.WAVMode = pwmExternal) then
        begin
          CurrentProject.WAVFile := ProjectForm.EditWAVFilename.Text;
          WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
          AudioOnlyRenderer.Open(CurrentProject.WAVFile);
        end
        else if (CurrentProject.WAVMode = pwmPeakOnly) then
        begin
          CurrentProject.PeakFile := ProjectForm.EditPeakFilename.Text;
          WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile, '.wav'));
          if AudioOnlyRenderer.Open(CurrentProject.VideoSource) then
            AudioOnlyRenderer.KillVideo;
        end;
        ProjectHasChanged := True;
      end;

      if (CurrentProject.WAVFile <> ProjectForm.EditWAVFilename.Text) then
      begin
        CurrentProject.WAVFile := ProjectForm.EditWAVFilename.Text;
        WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
        AudioOnlyRenderer.Open(CurrentProject.WAVFile);
        ProjectHasChanged := True;
      end;

      if (CurrentProject.PeakFile <> ProjectForm.EditPeakFilename.Text) then
      begin
        CurrentProject.PeakFile := ProjectForm.EditPeakFilename.Text;
        WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile, '.wav'));
        ProjectHasChanged := True;
      end;

      if (CurrentProject.Filename <> ProjectForm.EditProjectFilename.Text) then
      begin
        CurrentProject.Filename := ProjectForm.EditProjectFilename.Text;
      end;

      if (CurrentProject.IsUTF8 <> ProjectForm.chkSaveAsUTF8.Checked) then
      begin
        CurrentProject.IsUTF8 := ProjectForm.chkSaveAsUTF8.Checked;
        CurrentProject.IsDirty := True;
      end;

      if (ProjectHasChanged) then
      begin
        SaveProject(CurrentProject, False);
      end;

      if (VideoHasChanged or SubtitleFileHasChanged) and (VideoRenderer.IsOpen) then
      begin
        VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
      end;

      // Hide the video
      if (not VideoRenderer.IsOpen) and ShowingVideo then
      begin
          ActionShowHideVideo.Execute;
      end;
      g_GlobalContext.WavAverageBytePerSecond := WAVDisplayer.GetWAVAverageBytePerSecond;
    finally
      g_WebRWSynchro.EndWrite;
    end;

    WAVDisplayer.UpdateView([uvfPageSize]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ResetFind;
begin
  SearchNodeIndex := -1;
  SearchPos := 1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowMatchingSubOnly(DisplayAll : Boolean);
var Node : PVirtualNode;
    NodeData : PTreeData;
    Match : Boolean;
    FindText : WideString;
    RegExpr : TRegExpr;
    MatchingCount : Integer;
begin
  FindText := FindForm.GetFindText;
  MatchingCount := 0;

  RegExpr := TRegExpr.Create;
  RegExpr.Expression := FindText;
  RegExpr.ModifierI := not FindForm.MatchCase;

  Node := vtvSubsList.GetFirst;
  while (Node <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Match := True;
    if (Length(FindText) > 0) and (not DisplayAll) then
    begin
      if FindForm.UseRegExp then
      begin
        Match := RegExpr.Exec(NodeData.Range.Text);
      end
      else
      begin
        Match := WideStringFind(1, NodeData.Range.Text, FindText,
          not FindForm.MatchCase, FindForm.WholeWord) > 0;
      end;
    end;
    vtvSubsList.IsVisible[Node] := Match;
    if Match then Inc(MatchingCount);
    Node := vtvSubsList.GetNext(Node);
  end;
  RegExpr.Free;
  vtvSubsList.Repaint;
  if (not DisplayAll) then
    ShowStatusBarMessage(IntToStr(MatchingCount) + ' subtitle(s) match.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFindExecute(Sender: TObject);
begin
  if not FindForm.Showing then
  begin
    ResetFind;
    FindForm.Show;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
var SearchNode : PVirtualNode;
    NodeData : PTreeData;
    FindText : WideString;
    RegExp : TRegExpr;
    MatchLen : Integer;
begin
  FindText := FindForm.GetFindText;
  if Length(FindText) <= 0 then
    Exit;

  SearchNode := nil;
  if (SearchNodeIndex >= 0) and (SearchNodeIndex < WAVDisplayer.RangeList.Count) then
  begin
    SearchNode := TSubtitleRange(WAVDisplayer.RangeList[SearchNodeIndex]).Node;
  end;

  if FindForm.FromCursor then
  begin
    if SearchNode <> vtvSubsList.FocusedNode then
      SearchPos := 1;
    SearchNode := vtvSubsList.FocusedNode;
  end;
  if (SearchNode = nil) then
  begin
    SearchNode := vtvSubsList.GetFirst;
  end;

  RegExp := TRegExpr.Create;
  RegExp.ModifierI := not FindForm.MatchCase;
  RegExp.Expression := FindText;  
  while (SearchNode <> nil) do
  begin
    SearchNodeIndex := SearchNode.Index;
    NodeData := vtvSubsList.GetNodeData(SearchNode);
    MatchLen := Length(FindText);
    if FindForm.UseRegExp then
    begin
      RegExp.InputString := NodeData.Range.Text;
      if RegExp.ExecPos(SearchPos) then
      begin
        SearchPos := RegExp.MatchPos[0];
        MatchLen := RegExp.MatchLen[0];
      end else
        SearchPos := 0;
    end
    else
    begin
      SearchPos := WideStringFind(SearchPos, NodeData.Range.Text, FindText,
        not FindForm.MatchCase, FindForm.WholeWord);
    end;

    if (SearchPos > 0) then
    begin
      vtvSubsList.ScrollIntoView(SearchNode,True);
      vtvSubsList.FocusedNode := SearchNode;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[SearchNode] := True;
      MemoSubtitleText.SelStart := SearchPos - 1;
      MemoSubtitleText.SelLength := MatchLen;
      Inc(SearchPos, MatchLen);
      RegExp.Free;      
      Exit;
    end;
    SearchNode := vtvSubsList.GetNext(SearchNode);
    SearchPos := 1;
  end;
  RegExp.Free;
  SearchNodeIndex := -1;
  ShowStatusBarMessage('No more items.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ReplaceAllText;
var Node : PVirtualNode;
    NodeData : PTreeData;
    RegExpr : TRegExpr;
    ReplaceFlags : TReplaceFlags;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    NewText : WideString;
    SubRange : TSubtitleRange;
    TotalReplaceCount : Integer;
begin
  if Length(FindForm.GetFindText) <= 0 then
    Exit;

  TotalReplaceCount := 0;

  if FindForm.FromCursor then
    Node := vtvSubsList.FocusedNode
  else
    Node := vtvSubsList.GetFirst;

  MultiChangeTask := TUndoableMultiChangeTask.Create;
  RegExpr := TRegExpr.Create;
  RegExpr.ModifierI := (not FindForm.MatchCase);
  RegExpr.ModifierG := True;
  RegExpr.Expression := FindForm.GetFindText;

  g_WebRWSynchro.BeginWrite;
  try
    while (Node <> nil) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      SubRange := NodeData.Range;
      if FindForm.UseRegExp then
      begin
        NewText := RegExpr.Replace(NodeData.Range.Text, FindForm.GetReplaceText, True);
        TotalReplaceCount := TotalReplaceCount + RegExpr.ReplaceCount;
      end
      else
      begin
        ReplaceFlags := [rfReplaceAll];
        if (not FindForm.MatchCase) then
          Include(ReplaceFlags, rfIgnoreCase);
        TotalReplaceCount := TotalReplaceCount + WideStringCount(
          NodeData.Range.Text, FindForm.GetFindText, not FindForm.MatchCase,
          FindForm.WholeWord);
        NewText := Tnt_WideStringReplace(NodeData.Range.Text, FindForm.GetFindText,
          FindForm.GetReplaceText, ReplaceFlags, FindForm.WholeWord);
      end;
      ChangeSubData := TChangeSubData.Create(Node.Index);
      ChangeSubData.OldText := SubRange.Text;
      ChangeSubData.NewText := NewText;
      MultiChangeTask.AddData(ChangeSubData);
      NodeData.Range.Text := NewText;
      Node := vtvSubsList.GetNext(Node);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  RegExpr.Free;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
  ShowStatusBarMessage(IntToStr(TotalReplaceCount) + ' item(s) replaced.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ReplaceText;
var SelStart : Integer;
    NewText : WideString;
    RegExp : TRegExpr;
begin
  if (SearchNodeIndex = -1) then
    Exit;

  // Save selection start position
  SelStart := MemoSubtitleText.SelStart;

  TTntRichEditCustomUndo(MemoSubtitleText).SaveSelectionInfo;

  if FindForm.UseRegExp then
  begin
    RegExp := TRegExpr.Create;
    RegExp.ModifierI := (not FindForm.MatchCase);
    RegExp.Expression := FindForm.GetFindText;
    NewText := RegExp.Replace(MemoSubtitleText.SelText, FindForm.GetReplaceText, True);
    RegExp.Free;
  end
  else
  begin
    NewText := FindForm.GetReplaceText;
  end;

  MemoSubtitleText.SelText := NewText;
  
  // Restaure selection
  MemoSubtitleText.SelStart := SelStart;
  MemoSubtitleText.SelLength := Length(NewText);
end;

//------------------------------------------------------------------------------

function TMainForm.LastFindSucceded : Boolean;
begin
  Result := (SearchNodeIndex <> -1);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowErrorReportExecute(Sender: TObject);
begin
  ErrorReportForm.Visible := not ErrorReportForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionDelayExecute(Sender: TObject);
var DelayInMs : Integer;
    DelayShiftType : TDelayShiftType;
    Node : PVirtualNode;
    UndoableDelayTask : TUndoableDelayTask;
begin
  DelayForm := TDelayForm.Create(nil);

  // Prefill the dialog
  if (vtvSubsList.SelectedCount > 1) then
  begin
    DelayForm.SetDelayApplyToType(dattSelected);
  end;
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    DelayForm.SetDelayInMS(WAVDisplayer.Selection.StopTime -
      WAVDisplayer.Selection.StartTime);
  end;

  if DelayForm.ShowModal = mrOk then
  begin
    DelayInMs := DelayForm.GetDelayInMs;
    DelayShiftType := DelayForm.GetDelayShiftType;
    
    UndoableDelayTask := TUndoableDelayTask.Create;
    UndoableDelayTask.SetDelayInMs(DelayInMs);
    UndoableDelayTask.SetDelayShiftType(DelayShiftType);

    case DelayForm.GetDelayApplyToType of
      dattAll: // All subtitles
        begin
          UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount);
          Node := vtvSubsList.GetFirst;
          while Assigned(Node) do
          begin
            UndoableDelayTask.AddSubtitleIndex(Node.Index);
            Node := vtvSubsList.GetNext(Node);
          end;
        end;
      dattSelected: // Selected subs
        begin
          UndoableDelayTask.SetCapacity(vtvSubsList.SelectedCount);
          Node := vtvSubsList.GetFirstSelected;
          while Assigned(Node) do
          begin
            UndoableDelayTask.AddSubtitleIndex(Node.Index);
            Node := vtvSubsList.GetNextSelected(Node);
          end;
        end;
      dattFromCursor: // From focused subs to end
        begin
          Node := vtvSubsList.FocusedNode;
          if Assigned(Node) then
          begin
            UndoableDelayTask.SetCapacity(vtvSubsList.RootNodeCount - Node.Index);
          end;
          while Assigned(Node) do
          begin
            UndoableDelayTask.AddSubtitleIndex(Node.Index);
            Node := vtvSubsList.GetNext(Node);
          end;
        end;
    end;

    UndoableDelayTask.DoTask;
    PushUndoableTask(UndoableDelayTask);
  end;
  DelayForm.Free;
  DelayForm := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiSubListDeleteClick(Sender: TObject);
var Node : PVirtualNode;
    UndoableDeleteTask : TUndoableDeleteTask;
begin
  UndoableDeleteTask := TUndoableDeleteTask.Create;
  UndoableDeleteTask.SetCapacity(vtvSubsList.SelectedCount);
  Node := vtvSubsList.GetFirstSelected;
  while Assigned(Node) do
  begin
    UndoableDeleteTask.AddSubtitleIndex(Node.Index);
    Node := vtvSubsList.GetNextSelected(Node);
  end;
  UndoableDeleteTask.DoTask;
  PushUndoableTask(UndoableDeleteTask);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeExecute(Sender: TObject);
begin
  Merge(mtNormal, mrSelected);
end;

//------------------------------------------------------------------------------

procedure TMainForm.Merge(MergeType : TMergeType; MergeRange : TMergeRange);
var UndoableMergeTask : TUndoableMergeTask;
    Node, NodePrevious, NodeNext : PVirtualNode;
    SubRange : TSubtitleRange;
begin
  UndoableMergeTask := TUndoableMergeTask.Create;
  UndoableMergeTask.SetData(-1, MergeType);

  if (MergeRange = mrSelected) then
  begin
    UndoableMergeTask.SetCapacity(vtvSubsList.SelectedCount);
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      UndoableMergeTask.AddSubtitleIndex(Node.Index);
      Node := vtvSubsList.GetNextSelected(Node);
    end;
  end
  else if (MergeRange = mrWithPrevious) then
  begin
    Node := vtvSubsList.GetFirstSelected;
    NodePrevious := vtvSubsList.GetPrevious(Node);
    UndoableMergeTask.SetCapacity(2);
    UndoableMergeTask.AddSubtitleIndex(NodePrevious.Index);
    UndoableMergeTask.AddSubtitleIndex(Node.Index);
  end
  else if (MergeRange = mrWithNext) then
  begin
    Node := vtvSubsList.GetFirstSelected;
    NodeNext := vtvSubsList.GetNext(Node);
    UndoableMergeTask.SetCapacity(2);
    UndoableMergeTask.AddSubtitleIndex(Node.Index);
    UndoableMergeTask.AddSubtitleIndex(NodeNext.Index);
  end;

  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.SelectedRange);
    if vtvSubsList.Selected[SubRange.Node] then
    begin
      UndoableMergeTask.SetData(SubRange.Node.Index, MergeType);
    end;
  end;

  UndoableMergeTask.DoTask;
  PushUndoableTask(UndoableMergeTask);

  WAVDisplayer.UpdateView([uvfRange,uvfSelection]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ClearWAVSelection;
begin
  if Assigned(WAVDisplayer.SelectedRange) then
    WAVDisplayer.SelectedRange := nil;
  WAVDisplayer.ClearSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SubListPopupMenuPopup(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    pmiFixErrorEnabled : Boolean;
begin
  pmiSubListDelete.Enabled := (vtvSubsList.SelectedCount > 0);
  pmiSubListMerge.Enabled := (vtvSubsList.SelectedCount > 1);

  pmiFixErrorEnabled := False;
  if (vtvSubsList.SelectedCount > 0) then
  begin
    JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
    JSPEnum.OnJSPluginError := LogForm.LogMsg;
    JSPEnum.Reset;
    while (not pmiFixErrorEnabled) and JSPEnum.GetNext(JPlugin) do
    begin
      JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
      pmiFixErrorEnabled := pmiFixErrorEnabled or ((Assigned(JSPluginInfo) and
        (JSPluginInfo.Enabled = True)) and JPlugin.CanFixError);
      FreeAndNil(JPlugin);
    end;
    JSPEnum.Free;
  end;
  pmiFixError.Enabled := pmiFixErrorEnabled;
end;

//------------------------------------------------------------------------------

function TMainForm.CheckSubtitlesAreSaved : Boolean;
var res : Integer;
begin
  Result := True;
  if CurrentProject.IsDirty then
  begin
    // Ask to save the subtitles
    res := MessageBoxW(Handle, PWideChar(WideFormat(
    'The subtitle file "%s" has changed, do you want to save it before closing ?',
      [CurrentProject.SubtitlesFile])),
      PWideChar(WideString('Warning')),
      MB_YESNOCANCEL or MB_ICONWARNING);
    if (res = IDYES) then
    begin
      SaveSubtitles(CurrentProject.SubtitlesFile, '', CurrentProject.IsUTF8, False);
      SaveProject(CurrentProject, False);
    end
    else if(res = IDCANCEL) then
      Result := False
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSubtitlesAreSaved;
  if CanClose then
    SaveSettings;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayPopupMenuPopup(Sender: TObject);
begin
  pmiWAVDispAddSubtitle.Enabled := not WAVDisplayer.SelectionIsEmpty;
  pmiWAVDispSetSubtitleTime.Enabled := (not WAVDisplayer.SelectionIsEmpty) and
    Assigned(vtvSubsList.FocusedNode);

  pmiWAVDispDeleteRange.Enabled := WAVDisplayer.RangeList.GetRangeIdxAt(
    WAVDisplayer.GetCursorPos) <> -1;
  pmiWAVDispSplitAtCursor.Enabled := pmiWAVDispDeleteRange.Enabled;
  pmiInsertKaraokeMarker.Enabled := pmiWAVDispDeleteRange.Enabled;
  pmiDeleteSC.Enabled := (not WAVDisplayer.SelectionIsEmpty);
  pmiInsertSC.Enabled := WAVDisplayer.SelectionIsEmpty;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
var
  AboutForm: TAboutForm;
begin
  AboutForm := TAboutForm.Create(nil);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNode(Node : PVirtualNode);
begin
  vtvSubsList.FocusedNode := Node;
  vtvSubsList.ClearSelection;
  vtvSubsList.Selected[Node] := True;
  vtvSubsList.ScrollIntoView(Node,True);
  vtvSubsListDblClick(nil);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNextSub;
var CurrentNode, NextNode : PVirtualNode;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    WAVDisplayer.SelectNextKaraoke;
  end
  else
  begin
    if Assigned(vtvSubsList.FocusedNode) then
      CurrentNode := vtvSubsList.FocusedNode
    else
      CurrentNode := vtvSubsList.GetFirst;

    if Assigned(CurrentNode) then
    begin
      ActionStop.Execute;
      NextNode := vtvSubsList.GetNext(CurrentNode);
      if Assigned(NextNode) then
        SelectNode(NextNode)
      else
        SelectNode(CurrentNode);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNextSubExecute(Sender: TObject);
begin
  SelectNextSub;
  if (vtvSubsList.RootNodeCount > 0) then
  begin
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectPreviousSub;
var CurrentNode, PreviousNode : PVirtualNode;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    WAVDisplayer.SelectPreviousKaraoke;
  end
  else
  begin
    if Assigned(vtvSubsList.FocusedNode) then
      CurrentNode := vtvSubsList.FocusedNode
    else
      CurrentNode := vtvSubsList.GetFirst;

    if Assigned(CurrentNode) then
    begin
      ActionStop.Execute;
      PreviousNode := vtvSubsList.GetPrevious(CurrentNode);
      if Assigned(PreviousNode) then
        SelectNode(PreviousNode)
      else
        SelectNode(CurrentNode);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ActionPreviousSubExecute(Sender: TObject);
begin
  SelectPreviousSub;
  if (vtvSubsList.RootNodeCount > 0) then
  begin
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FullSortTreeAndSubList;
begin
  g_WebRWSynchro.BeginWrite;
  try
    WAVDisplayer.RangeList.FullSort;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsList.Sort(nil, 0, sdDescending, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PTreeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  Result := CompareRanges(Data1.Range,Data2.Range);
end;

//------------------------------------------------------------------------------

procedure TMainForm.chkAutoScrollSubClick(Sender: TObject);
begin
  TimerAutoScrollSub.Enabled := chkAutoScrollSub.Checked;
  OldAutoScrollIdx := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerAutoScrollSubTimer(Sender: TObject);
var Idx : Integer;
    Node : PVirtualNode;
    SubRange : TSubtitleRange;
begin
  if WAVDisplayer.IsPlaying then
  begin
    Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetPlayCursorPos);
    if (Idx <> OldAutoScrollIdx) then
    begin
      if (Idx <> -1) then
      begin
        SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
        Node := SubRange.Node;
        vtvSubsList.ScrollIntoView(Node,True);
        // Needed if subtitle node is already the focused node
        if (MemoSubtitleText.Text = '') then
        begin
          vtvSubsList.FocusedNode := nil;
        end;
        vtvSubsList.FocusedNode := Node;
        vtvSubsList.ClearSelection;
        vtvSubsList.Selected[Node] := True;
      end
      else
      begin
        MemoSubtitleText.Tag := 0;
        TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
        MemoSubtitleText.Text := '';
        TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
      end;
      OldAutoScrollIdx := Idx;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionGotoExecute(Sender: TObject);
var Idx : integer;
    i : integer;
    GotoPositionMs : Integer;
    SubRange : TSubtitleRange;
begin
  GotoForm := TGotoForm.Create(nil);
  if GotoForm.ShowModal = mrOk then
  begin
    SubRange := nil;
    if GotoForm.rbGotoLine.Checked then
    begin
      Idx := StrToIntDef(GotoForm.EditLine.Text, 0) - 1;
      Constrain(Idx, 0, WAVDisplayer.RangeList.Count-1);
      SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
    end
    else if GotoForm.rbGotoTime.Checked then
    begin
      GotoPositionMs := TimeStringToMs(GotoForm.MaskEditTime.EditText);
      if (WAVDisplayer.RangeList.Count > 0) then
      begin
        for i:=0 to WAVDisplayer.RangeList.Count-1 do
        begin
          if ( ((WAVDisplayer.RangeList[i].StartTime <= GotoPositionMs) and
            (WAVDisplayer.RangeList[i].StopTime >= GotoPositionMs)) or
            (WAVDisplayer.RangeList[i].StartTime >= GotoPositionMs)) then
          begin
            SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
            Break;
          end;
        end;
        if not Assigned(SubRange) then
        begin
          SubRange := TSubtitleRange(WAVDisplayer.RangeList[WAVDisplayer.RangeList.Count-1]);
        end;
      end
      else
      begin
        // Display 20s centered on cursor
        WAVDisplayer.ZoomCenteredOn(GotoPositionMs,20000);
      end;
      WAVDisplayer.SetCursorPos(GotoPositionMs);
    end;
    if Assigned(SubRange) then
    begin
      SelectNode(SubRange.Node);
    end;
  end;
  GotoForm.Free;
  GotoForm := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNodeFromTimestamps(Start, Stop : Integer);
var i : integer;
    SubRange : TSubtitleRange;
begin
  SubRange := nil;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    if (WAVDisplayer.RangeList[i].StartTime = Start) and
      (WAVDisplayer.RangeList[i].StopTime = Stop) then
    begin
      SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
      Break;
    end;
  end;
  if Assigned(SubRange) then
  begin
    SelectNode(SubRange.Node);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SelectNodeAtIndex(Index : Integer);
var SubRange : TSubtitleRange;
begin
  if (Index >= 0) and (Index < WAVDisplayer.RangeList.Count) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
    SelectNode(SubRange.Node);
  end
end;

//------------------------------------------------------------------------------

procedure TMainForm.StartStopServer(Stop : Boolean);
begin
  if Stop then
  begin
    if Assigned(Server) then
    begin
      Server.Free;
      Server := nil;
    end;
    MenuItemStopWebServer.Checked := True;
  end
  else
  begin
    if not Assigned(Server) then
    begin
      Server := THTTPServer.Create(ServerRootDir,ConfigObject.ServerPort);
      Server.EnableCompression := ConfigObject.EnableCompression;
      Server.Start;
    end;
    MenuItemStartWebServer.Checked := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemStartWebServerClick(Sender: TObject);
begin
  StartStopServer;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemStopWebServerClick(Sender: TObject);
begin
  StartStopServer(True);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowPreferences(TabSheet : TTntTabSheet);
var OldSwapState : Boolean;
    IniFile : TIniFile;
begin
  OldSwapState := ConfigObject.SwapSubtitlesList;
  PreferencesFormInstance.LoadConfig(ConfigObject);
  if (TabSheet <> nil) then
    PreferencesFormInstance.PageControlPreferences.ActivePage := TabSheet;

  if (not PrefFormInitialized) then
  begin
    IniFile := TIniFile.Create(GetIniFilename);
    LoadFormPosition(IniFile, PreferencesFormInstance);
    IniFile.Free;
    PrefFormInitialized := True;
  end;

  if (PreferencesFormInstance.ShowModal = mrOk) then
  begin
    PreferencesFormInstance.SaveConfig(ConfigObject);
    if Assigned(Server) then
      Server.EnableCompression := ConfigObject.EnableCompression;
    if(ConfigObject.SwapSubtitlesList <> OldSwapState) then
      Self.SwapSubList;
    MemoSubtitleText.Enabled := MemoLinesCounter.Enabled and
      ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
    SetShortcut(IsTimingMode);
    ApplyMouseSettings;
    ApplyAutoBackupSettings;
    ApplyFontSettings;
    ApplyMiscSettings;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyFontSettings;
var DC : HDC;
    MLCanvas : TCanvas;
    TxtWidth : Integer;
    Node : PVirtualNode;
begin
  // Update subtitle list font
  String2Font(ConfigObject.SubListFont, vtvSubsList.Font);
  vtvSubsList.Header.AutoFitColumns(False);
  vtvSubsList.Canvas.Font.Assign(vtvSubsList.Font);
  // Update subtitle node height
  vtvSubsList.DefaultNodeHeight := Round(vtvSubsList.Canvas.TextHeight('Tj') * 1.1);
  vtvSubsList.BeginUpdate;
  Node := vtvSubsList.GetFirst;
  while Assigned(Node) do
  begin
    vtvSubsList.NodeHeight[Node] := vtvSubsList.DefaultNodeHeight;
    Node := vtvSubsList.GetNext(Node);
  end;
  vtvSubsList.EndUpdate;

  SuggestionForm.vtvSuggestionsLst.Font.Name := vtvSubsList.Font.Name;
  SuggestionForm.vtvSuggestionsLst.Canvas.Font.Name := vtvSubsList.Font.Name;
  SuggestionForm.MemoSuggPreview.Font.Name := vtvSubsList.Font.Name;
  SuggestionForm.UpdateFonts;

  String2Font(ConfigObject.SubTextFont, MemoSubtitleText.Font);
  // TODO : check why MemoSubtitleText font is not updated automatically (need to select a new subtitle)
  MemoLinesCounter.Font.Assign(MemoSubtitleText.Font);

  // Update line counter size so we have at least 3 digit
  DC := GetDC(MemoLinesCounter.Handle);
  if(DC <> 0) then
  begin
    MLCanvas := TCanvas.Create;
    MLCanvas.Font.Assign(MemoLinesCounter.Font);
    MLCanvas.Handle := DC;
    TxtWidth := MLCanvas.TextWidth('000');
    MLCanvas.Free;
    ReleaseDC(0,DC);
    MemoLinesCounter.Width := TxtWidth + 2*4;
  end;

  // TODO ? : status bar
  // Adjust status bar panel size for people using bigger fonts
  //StatusBarPanel1.Assign(TntStatusBar1.Font);
  //StatusBarPanel1.Width :=  TntStatusBar1.Canvas.TextWidth(
  //  'Line: 99, Column: 999 | Total: 999, Char/s: 99');
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyAutoBackupSettings;
begin
  TimerAutoBackup.Enabled := False;
  TimerAutoBackup.Interval := ConfigObject.AutoBackupEvery * 60 * 1000;
  TimerAutoBackup.Enabled := (ConfigObject.AutoBackupEvery > 0);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPreferencesExecute(Sender: TObject);
begin
  ShowPreferences(nil);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionErrorPreferencesExecute(Sender: TObject);
begin
  ShowPreferences(PreferencesFormInstance.tsErrorChecking);
end;

//------------------------------------------------------------------------------

procedure TMainForm.CloseProject;
var EmptyArray : TIntegerDynArray;
begin
  chkAutoScrollSub.Checked := False;
  TimerAutoScrollSub.Enabled := False;
  SetLength(EmptyArray, 0);

  g_WebRWSynchro.BeginWrite;
  try
    SuggestionForm.Clear;
    ErrorReportForm.Clear;
    vtvSubsList.Clear;

    WAVDisplayer.ClearRangeList;
    WAVDisplayer.Enabled := False;
    WAVDisplayer.Close;
    WAVDisplayer.Invalidate;
    WAVDisplayer.VerticalScaling := 100;
    WAVDisplayer.SetSceneChangeList(EmptyArray);

    CurrentProject.Filename := '';
    CurrentProject.VideoSource := '';
    CurrentProject.WAVFile := '';
    CurrentProject.PeakFile := '';
    CurrentProject.SubtitlesFile := '';
    CurrentProject.IsDirty := False;
  finally
    g_WebRWSynchro.EndWrite;
  end;
    
  Self.Caption := ApplicationName;
  MemoLinesCounter.Text := '';
  StatusBarPanel1.Caption := '';
  TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
  MemoSubtitleText.Text := '';
  TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;

  EnableControl(False);
  cbStyles.Clear;
  EnableStyleControls(False);

  AudioOnlyRenderer.Close;
  VideoRenderer.Close;

  g_VSSCoreWrapper.SetVideoWidth(0);
  g_VSSCoreWrapper.SetVideoHeight(0);

  if Assigned(SilentZoneForm) then
    SilentZoneForm.Clear;

  ClearStack(RedoStack);
  ClearStack(UndoStack);
  ActionUndo.Enabled := False;
  ActionRedo.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  if CheckSubtitlesAreSaved then
  begin
    CloseProject;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowSuggestionsExecute(Sender: TObject);
begin
  SuggestionForm.Visible := not SuggestionForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemHelpIndexClick(Sender: TObject);
var HelpFilename : WideString;
begin
  HelpFilename := WideExtractFilePath(TntApplication.ExeName);
  HelpFilename := WideIncludeTrailingBackslash(HelpFilename);
  HelpFilename := HelpFilename + 'help\index.html';
  Tnt_ShellExecuteW(0, 'open', PWideChar(HelpFilename), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemHelpIndexWAVDisplayControlClick(
  Sender: TObject);
var HelpFilename : WideString;
begin
  HelpFilename := WideExtractFilePath(TntApplication.ExeName);
  HelpFilename := WideIncludeTrailingBackslash(HelpFilename);
  HelpFilename := HelpFilename + 'help\wavdisplaycontrol.html';
  Tnt_ShellExecuteW(0, 'open', PWideChar(HelpFilename), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionInsertTextFileExecute(Sender: TObject);
var
  Line, Ext: WideString;
  LastRange, NewRange : TRange;
  StartTime : Integer;
  Node: PVirtualNode;
  NodeData: PTreeData;
  HaveNewSub : Boolean;
  Source : TTntStringList;
  I, LineIndex, SubCountBefore : Integer;
  UndoableMultiAddTask : TUndoableMultiAddTask;
  SRTParser : TSRTParser;
  SubList : TList;
  SRTSub : TSRTSubtitle; 
begin

  TntOpenDialog1.Filter := 'Text files|*.TXT' + '|' +
    'Subtitle files (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  if not TntOpenDialog1.Execute then
    Exit;

  Source := nil;
  
  g_WebRWSynchro.BeginWrite;
  try
    // TODO : insert SSA/ASS file
    Ext := WideLowerCase(WideExtractFileExt(TntOpenDialog1.FileName));
    if (Ext = '.srt') then
    begin
      SubList := TList.Create;
      SRTParser := TSRTParser.Create;
      SRTParser.Load(TntOpenDialog1.FileName);
      for i := 0 to SRTParser.GetCount-1 do
      begin
        SRTSub := SRTParser.GetAt(i);
        NewRange := SubRangeFactory.CreateRangeSS(SRTSub.Start, SRTSub.Stop);
        TSubtitleRange(NewRange).Text := SRTSub.Text;
        NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);
        SubList.Add(NewRange);
      end;
      SRTParser.Free;

      if (SubList.Count > 0) then
      begin
        UndoableMultiAddTask := TUndoableMultiAddTask.Create;
        UndoableMultiAddTask.SetData(SubList);
        UndoableMultiAddTask.DoTask;
        PushUndoableTask(UndoableMultiAddTask);
        CurrentProject.IsDirty := True;
      end;
      FreeAndNil(SubList);
    end
    else
    begin
      HaveNewSub := False;
      Source := MyTTntStringList.Create;
      Source.LoadFromFile(TntOpenDialog1.FileName);

      // Get start time for the first new subtitle
      if (WAVDisplayer.RangeList.Count > 0) then
      begin
        LastRange := WAVDisplayer.RangeList[WAVDisplayer.RangeList.Count-1];
        StartTime := LastRange.StopTime + 2000;
      end
      else
        StartTime := 99 * 60 * 60 * 1000;

      SubCountBefore := WAVDisplayer.RangeList.Count;
      for LineIndex := 0 to Source.Count-1 do
      begin
        Line := Source[LineIndex];

        NewRange := SubRangeFactory.CreateRangeSS(StartTime, StartTime+1000);
        TSubtitleRange(NewRange).Text := Line;
        WAVDisplayer.RangeList.AddAtEnd(NewRange);
        Node := vtvSubsList.AddChild(nil);
        NodeData := vtvSubsList.GetNodeData(Node);
        NodeData.Range := TSubtitleRange(NewRange);
        TSubtitleRange(NewRange).Node := Node;

        Inc(StartTime, 2000);
        HaveNewSub := True;
      end;

      if HaveNewSub then
      begin
        CurrentProject.IsDirty := True;
        UndoableMultiAddTask := TUndoableMultiAddTask.Create;
        UndoableMultiAddTask.SetCapacity(Source.Count);
        for I := SubCountBefore to (SubCountBefore + Source.Count - 1) do
        begin
          UndoableMultiAddTask.AddSubtitleIndex(I);
        end;
        // this is a lazy task so don't "do" the task now
        PushUndoableTask(UndoableMultiAddTask);
      end;
      FreeAndNil(Source);
    end;

  finally
    g_WebRWSynchro.EndWrite;

  end;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SwapSubList(SwapSizeAlso : Boolean);
var SavHeight, SavHeight2 : Integer;
begin
  if Swapped then
  begin
    SavHeight := vtvSubsList.Height;
    SavHeight2 := MemoLinesCounter.Height;

    vtvSubsList.Parent := PanelMiddle;
    MemoLinesCounter.Parent := PanelBottom;
    MemoSubtitleText.Parent := PanelBottom;

    PanelBottom.Align := alBottom;
    Splitter1.Align := alBottom;
    PanelMiddle.Align := alClient;
    if(SwapSizeAlso) then
      PanelBottom.Height := SavHeight2;
    StatusBarMainPanel.Top := MaxInt; // make sure status bar stay at bottom
  end
  else
  begin
    SavHeight := vtvSubsList.Height;
    SavHeight2 := MemoLinesCounter.Height;

    vtvSubsList.Parent := PanelBottom;
    MemoLinesCounter.Parent := PanelMiddle;
    MemoSubtitleText.Parent := PanelMiddle;

    PanelMiddle.Align := alTop;
    Splitter1.Align := alTop;
    PanelBottom.Align := alClient;

    if(SwapSizeAlso) then
      PanelMiddle.Height := PanelMiddle.Height - SavHeight + SavHeight2;
    Splitter1.Top := MaxInt;
  end;
  Swapped := not Swapped;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionClearSelectionExecute(Sender: TObject);
begin
  WAVDisplayer.ClearSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if (Swapped) then
  begin
    Accept := (NewSize > PanelMiddle.Constraints.MinHeight) and
      ((PanelBottom.Height + PanelMiddle.Height - NewSize) > PanelBottom.Constraints.MinHeight);
  end
  else
  begin
    Accept := ((PanelMiddle.Height + PanelBottom.Height - NewSize) > PanelMiddle.Constraints.MinHeight) and
      (NewSize > PanelBottom.Constraints.MinHeight);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.bttWorkingModeClick(Sender: TObject);
begin
  ActionToggleTimingMode.Execute;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetShortcut(TimingMode : Boolean);
var i : integer;
    HLID : THotkeyListItemData;
begin
  for i:=0 to ConfigObject.ListHotkeys.Count-1 do
  begin
    HLID := ConfigObject.ListHotkeys[i];
    if TimingMode then
      HLID.Action.ShortCut := HLID.TimingShortCut
    else
      HLID.Action.ShortCut := HLID.NormalShortCut
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopSelStartExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    if IsTimingMode then
      PanelWAVDisplay.SetFocus;
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionStart;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection.StartTime,
      Min(WAVDisplayer.Selection.StartTime + StartEndPlayingDuration, WAVDisplayer.Selection.StopTime),
      True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopSelEndExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    if IsTimingMode then
      PanelWAVDisplay.SetFocus;
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionEnd;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
      WAVDisplayer.Selection.StopTime,
      True);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.AddSubtitle(StartTime, StopTime : Integer; Text : WideString) : PVirtualNode;
var NewRange, SiblingRange : TRange;
    NewNode, SiblingNode: PVirtualNode;
    NodeData: PTreeData;
    InsertPos : Integer;
begin
  NewRange := SubRangeFactory.CreateRangeSS(StartTime, StopTime);
  InsertPos := WAVDisplayer.RangeList.FindInsertPos(NewRange);
  if (InsertPos >= WAVDisplayer.RangeList.Count) then
  begin
    NewNode := vtvSubsList.AddChild(nil);
  end
  else
  begin
    SiblingRange := WAVDisplayer.RangeList[InsertPos];
    SiblingNode := TSubtitleRange(SiblingRange).Node;
    NewNode := vtvSubsList.InsertNode(SiblingNode, amInsertBefore);
  end;
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NodeData.Range := TSubtitleRange(NewRange);
  TSubtitleRange(NewRange).Node := NewNode;
  if Length(Text) > 0 then
  begin
    TSubtitleRange(NewRange).Text := Text;
  end;
  // TODO improvement : use the previously calculated InsertPos
  WAVDisplayer.AddRange(NewRange);

  // Update SearchNodeIndex (TODO : SearchPos)
  if (SearchNodeIndex >= NewNode.Index) then
    Inc(SearchNodeIndex);

  Result := NewNode;
end;

//------------------------------------------------------------------------------

function TMainForm.AddSubtitle(SubRange : TSubtitleRange) : PVirtualNode;
var NewRange : TSubtitleRange;
    NewNode: PVirtualNode;
    NodeData: PTreeData;
begin
  NewNode := AddSubtitle(SubRange.StartTime, SubRange.StopTime, '');
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NewRange := TSubtitleRange(NodeData.Range);
  NewRange.Assign(SubRange);
  Result := NewNode;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FocusNodeAt(Index : Integer);
var SubRange : TSubtitleRange;
    Node : PVirtualNode;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  Node := SubRange.Node;
  FocusNode(Node, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.FocusNode(Node : PVirtualNode; WAVDisplaySelect : Boolean);
var NodeData : PTreeData;
begin
  if (not vtvSubsList.IsVisible[Node]) then
  begin
    vtvSubsList.ScrollIntoView(Node, True);
  end;
  vtvSubsList.FocusedNode := Node;
  vtvSubsList.ClearSelection;
  vtvSubsList.Selected[Node] := True;
  vtvSubsList.Repaint;

  if WAVDisplaySelect and (WAVDisplayer.SelMode = smCoolEdit) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    WAVDisplayer.SelectedRange := NodeData.Range;
  end;

  UpdateStylesComboboxFromSelection;
end;

//------------------------------------------------------------------------------

function TMainForm.AddSubtitle(StartTime, StopTime : Integer; Text : WideString;
  AutoSelect : Boolean) : PVirtualNode;
var NewNode : PVirtualNode;
begin
  g_WebRWSynchro.BeginWrite;
  try
    NewNode := AddSubtitle(StartTime, StopTime, Text);
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  FocusNode(NewNode, AutoSelect);

  Result := NewNode;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionAddSubtitleExecute(Sender: TObject);
var UndoableAddTask : TUndoableAddTask;
begin
  UndoableAddTask := TUndoableAddTask.Create;
  UndoableAddTask.SetTime(WAVDisplayer.Selection.StartTime,
    WAVDisplayer.Selection.StopTime);
  UndoableAddTask.SetAutoSelect(True);
  UndoableAddTask.DoTask;
  PushUndoableTask(UndoableAddTask);

  if IsNormalMode and MemoSubtitleText.Enabled then
    MemoSubtitleText.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleTimeExecute(Sender: TObject);
var NodeData : PTreeData;
    FocusedNode : PVirtualNode;
    SelDuration : Integer;
    UndoableSetTimeTask : TUndoableSetTimeTask;
begin
  if WAVDisplayer.SelectionIsEmpty then
    Exit;
  SelDuration := WAVDisplayer.Selection.StopTime - WAVDisplayer.Selection.StartTime;
  if (SelDuration < 100) then
    Exit;
  FocusedNode := vtvSubsList.FocusedNode;
  if Assigned(FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(FocusedNode);
    UndoableSetTimeTask := TUndoableSetTimeTask.Create;
    UndoableSetTimeTask.SetData(FocusedNode.Index,
      NodeData.Range.StartTime, NodeData.Range.StopTime,
      WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime);
    UndoableSetTimeTask.DoTask;
    PushUndoableTask(UndoableSetTimeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate70Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(70);
  AudioOnlyRenderer.SetRate(70);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate60Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(60);
  AudioOnlyRenderer.SetRate(60);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate80Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(80);
  AudioOnlyRenderer.SetRate(80);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate90Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(90);
  AudioOnlyRenderer.SetRate(90);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetPlaybackRate100Execute(Sender: TObject);
begin
  VideoRenderer.SetRate(100);
  AudioOnlyRenderer.SetRate(100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.OffsetCurrentSubtitleStartTime(Offset : Integer);
var NodeData : PTreeData;
    R : TRange;
    bStart, bStop, OldTime : Integer;
    Idx : Integer;
    UndoableDelayTask : TUndoableDelayTask;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    R := WAVDisplayer.KaraokeSelectedRange;
    Idx := (WAVDisplayer.KaraokeSelectedIndex - 1);
    if (Idx >= 0) then
    begin
      // Calcul bounds
      if (Idx = 0) then
        bStart := R.StartTime + 10
      else
        bStart := R.SubTime[Idx-1] + 10;

      if (Idx < Length(R.SubTime)-1) then
        bStop := R.SubTime[Idx+1] - 10
      else
        bStop := R.StopTime - 10;

      OldTime := R.SubTime[Idx];
      R.SubTime[Idx] := R.SubTime[Idx] + Offset;
      Constrain(R.SubTime[Idx], bStart, bStop);
      WAVDisplayer.Selection.StartTime := R.SubTime[Idx];
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(WAVDisplayer, R, Idx, OldTime);
      WAVDisplayer1SelectionChange(WAVDisplayer);
      Exit;
    end;
  end;

  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    if(NodeData.Range.StartTime + Offset < NodeData.Range.StopTime) then
    begin
      UndoableDelayTask := TUndoableDelayTask.Create;
      UndoableDelayTask.SetDelayInMs(Offset);
      UndoableDelayTask.SetDelayShiftType(dstStartTimeOnly);
      UndoableDelayTask.SetCapacity(1);
      UndoableDelayTask.AddSubtitleIndex(vtvSubsList.FocusedNode.Index);
      UndoableDelayTask.DoTask;
      PushUndoableTask(UndoableDelayTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OffsetCurrentSubtitleStopTime(Offset : Integer);
var NodeData : PTreeData;
    R : TRange;
    bStart, bStop, OldTime : Integer;
    Idx : Integer;
    UndoableDelayTask : TUndoableDelayTask;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    R := WAVDisplayer.KaraokeSelectedRange;
    Idx := WAVDisplayer.KaraokeSelectedIndex;
    if (Idx < Length(R.SubTime)) then
    begin
      // Calcul bounds
      if (Idx = 0) then
        bStart := R.StartTime + 10
      else
        bStart := R.SubTime[Idx-1] + 10;

      if (Idx < Length(R.SubTime)-1) then
        bStop := R.SubTime[Idx+1] - 10
      else
        bStop := R.StopTime - 10;

      OldTime := R.SubTime[Idx];
      R.SubTime[Idx] := R.SubTime[Idx] + Offset;
      Constrain(R.SubTime[Idx], bStart, bStop);
      WAVDisplayer.Selection.StopTime := R.SubTime[Idx];
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(WAVDisplayer, R, Idx, OldTime);
      WAVDisplayer1SelectionChange(WAVDisplayer);
      Exit;
    end;
  end;
  
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    if(NodeData.Range.StopTime + Offset > NodeData.Range.StartTime) then
    begin
      UndoableDelayTask := TUndoableDelayTask.Create;
      UndoableDelayTask.SetDelayInMs(Offset);
      UndoableDelayTask.SetDelayShiftType(dstStopTimeOnly);
      UndoableDelayTask.SetCapacity(1);
      UndoableDelayTask.AddSubtitleIndex(vtvSubsList.FocusedNode.Index);
      UndoableDelayTask.DoTask;
      PushUndoableTask(UndoableDelayTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartPlus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartMinus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(-100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopPlus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopMinus100Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(-100);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartPlus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStartMinus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStartTime(-10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopPlus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShiftStopMinus10Execute(Sender: TObject);
begin
  OffsetCurrentSubtitleStopTime(-10);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionZoomVerticalExecute(Sender: TObject);
begin
  if (VerticalScalingForm = nil) then
    VerticalScalingForm := TVerticalScalingForm.Create(Self,WAVDisplayer);
  VerticalScalingForm.Visible := not VerticalScalingForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideTextPipeExecute(Sender: TObject);
begin
  SplitterMemoTextPipe.Visible := not SplitterMemoTextPipe.Visible;
  MemoTextPipe.Visible := not MemoTextPipe.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoadTextPipeExecute(Sender: TObject);
begin
  TntOpenDialog1.Filter :=
    'Text files (*.txt)|*.TXT' + '|' +
    'RTF files (*.rtf)|*.RTF' + '|' +
    'Subtitle files (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  // TODO : option to strip timestamps
  if TntOpenDialog1.Execute then
  begin
    MemoTextPipe.Clear;
    MemoTextPipe.Lines.LoadFromFile(TntOpenDialog1.FileName);
    CurrentProject.TextPipeSource := TntOpenDialog1.FileName;
    if (MemoTextPipe.Visible = False) then
      ActionShowHideTextPipe.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionClearTextPipeExecute(Sender: TObject);
begin
  MemoTextPipe.Clear;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveTextPipeAsExecute(Sender: TObject);
begin
  TntSaveDialog1.Filter := 'Text file (*.txt)|*.TXT|RTF file (*.rtf)|*.RTF';
  TntSaveDialog1.FileName := CurrentProject.TextPipeSource;
  if TntSaveDialog1.Execute then
  begin
    if TntSaveDialog1.FilterIndex = 1 then
      MemoTextPipe.PlainText := True
    else if TntSaveDialog1.FilterIndex = 2 then
      MemoTextPipe.PlainText := False;
    MemoTextPipe.Lines.SaveToFile(TntSaveDialog1.FileName);
    MemoTextPipe.PlainText := False;
    CurrentProject.TextPipeSource := TntSaveDialog1.FileName;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionAddSubFromPipeExecute(Sender: TObject);
var UndoableAddTask : TUndoableAddTask;
    UndoablePipeTask : TUndoableTask;
    UndoableCompositeTask : TUndoableCompositeTask;
begin
  if (MemoTextPipe.SelLength > 0) and (not WAVDisplayer.SelectionIsEmpty) then
  begin
    UndoableCompositeTask := TUndoableCompositeTask.Create;

    // Add subtitle part
    UndoableAddTask := TUndoableAddTask.Create;
    UndoableAddTask.SetTime(WAVDisplayer.Selection.StartTime,
      WAVDisplayer.Selection.StopTime);
    UndoableAddTask.SetText(Trim(MemoTextPipe.SelText));
    UndoableAddTask.SetAutoSelect(True);

    // Pipe part
    UndoablePipeTask := ColorizeOrDeleteTextPipe;

    UndoableCompositeTask.AddTask(UndoableAddTask);
    UndoableCompositeTask.AddTask(UndoablePipeTask);

    UndoableCompositeTask.DoTask;
    PushUndoableTask(UndoableCompositeTask);
    if IsNormalMode and MemoSubtitleText.Enabled then
      MemoSubtitleText.SetFocus;
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.ColorizeOrDeleteTextPipe : TUndoableTask;
var ActionType : Integer;
    NewColor : TColor;
    UndoablePipeTask : TUndoablePipeTask;
begin
  NewColor := $003333FF;
  ActionType := 0;  
  UndoablePipeTask := TUndoablePipeTask.Create;
  if pmiAutoColorizeText.Checked then
  begin
    ActionType := 0;
    if (Assigned(vtvSubsList.FocusedNode)) and
       ((vtvSubsList.AbsoluteIndex(vtvSubsList.FocusedNode) mod 2) <> 0) then
      NewColor := $00FF8000;
  end
  else if pmiAutoDeleteText.Checked then
  begin
    ActionType := 1;
  end
  else if pmiAutoDeleteAllTextBefore.Checked then
  begin
    ActionType := 2;
  end;
  UndoablePipeTask.SetData(ActionType, MemoTextPipe, NewColor);
  Result := UndoablePipeTask;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoTextPipePopupMenuPopup(Sender: TObject);
begin
  pmiAddAsSubtitle.Enabled := (MemoTextPipe.SelLength > 0) and
    (not WAVDisplayer.SelectionIsEmpty);
  pmiReplaceSubtitleFromPipe.Enabled := (MemoTextPipe.SelLength > 0) and
    (vtvSubsList.FocusedNode <> nil);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionReplaceFromPipeExecute(Sender: TObject);
var NextNode : PVirtualNode;
begin
  if (MemoTextPipe.SelLength > 0) and (vtvSubsList.FocusedNode <> nil) then
  begin
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := Trim(MemoTextPipe.SelText);
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
    ColorizeOrDeleteTextPipe;
    // Go to next subtitle
    NextNode := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    if Assigned(NextNode) then
      SelectNode(NextNode);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyMouseSettings;
begin
  WAVDisplayer.WheelTimeScroll := ConfigObject.MouseWheelTimeScrollModifier;
  WAVDisplayer.WheelVZoom := ConfigObject.MouseWheelVZoomModifier;
  WAVDisplayer.WheelHZoom := ConfigObject.MouseWheelHZoomModifier;
  if IsTimingMode then
  begin
    // Timing mode
    if ConfigObject.MouseEnableSSATimingMode then
      WAVDisplayer.SelMode := smSSA
    else
      WAVDisplayer.SelMode := smCoolEdit;
  end;
  WAVDisplayer.SnappingEnabled := ConfigObject.EnableMouseSnapping;
  WAVDisplayer.EnableMouseAntiOverlapping := ConfigObject.EnableMouseAntiOverlapping;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerAutoBackupTimer(Sender: TObject);
var BackupDstFilename : WideString;
begin
  if ActionSave.Enabled then
  begin
    CheckBackupDirectory;
    BackupDstFilename := g_BackupDirectory +
      WideChangeFileExt(WideExtractFileName(CurrentProject.SubtitlesFile), '.bak');
    SaveSubtitles(BackupDstFilename, '', CurrentProject.IsUTF8, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionDetachVideoExecute(Sender: TObject);
begin
  if MenuItemDetachVideoWindow.Checked then
  begin
    DetachedVideoForm.Visible := False;
    if VideoRenderer.IsOpen then
      VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
    PanelVideo.Visible := ShowingVideo;
    SplitterWAVDisplay_Video.Visible := ShowingVideo;
  end
  else
  begin
    PanelVideo.Visible := False;
    SplitterWAVDisplay_Video.Visible := False;
    if VideoRenderer.IsOpen then
      VideoRenderer.SetDisplayWindow(DetachedVideoForm.Handle);
    if ShowingVideo then
      ShowWindow(DetachedVideoForm.Handle, SW_SHOWNOACTIVATE); // Don't give focus
    DetachedVideoForm.Visible := ShowingVideo;
    // Workaround to missing resize event bug => force repaint
    WAVDisplayer.UpdateView([uvfPageSize]);
  end;
  MenuItemDetachVideoWindow.Checked := not MenuItemDetachVideoWindow.Checked;

  if Assigned(CurrentProject) and (MenuItemDetachVideoWindow.Checked <> CurrentProject.DetachedVideo) then
  begin
    CurrentProject.DetachedVideo := MenuItemDetachVideoWindow.Checked;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionCheckErrorsExecute(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    i : Integer;
    Start, ExecTime : Cardinal;
    ResultMsg, Msg : WideString;
    JSPluginInfo : TJSPluginInfo;
    CM : ICursorManager;
begin
  CM := TCursorManager.Create(crHourGlass);

  LogForm.Clear;
  LogForm.SilentLogMsg('Starting error checking :');

  ErrorReportForm.Clear;
  Start := GetTickCount;  
  ErrorReportForm.vtvErrorList.BeginUpdate;

  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  g_WebRWSynchro.BeginWrite; // Should not be needed cause we only read for error checking, but we never know
  try
    while JSPEnum.GetNext(JPlugin) do
    begin
      JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
      if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = False) then
      begin
        FreeAndNil(JPlugin);
        Continue;
      end;

      ConfigObject.ApplyParam(JPlugin);

      for i := 0 to WAVDisplayer.RangeList.Count-1 do
      begin
        SubRangeCurrent := TSubtitleRange(WAVDisplayer.RangeList[i]);

        if (i > 0) then
          SubRangePrevious := TSubtitleRange(WAVDisplayer.RangeList[i-1])
        else
          SubRangePrevious := nil;

        if (i < WAVDisplayer.RangeList.Count-1) then
          SubRangeNext := TSubtitleRange(WAVDisplayer.RangeList[i+1])
        else
          SubRangeNext := nil;

        ResultMsg := JPlugin.HasError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        if (ResultMsg <> '') then
        begin
          ErrorReportForm.AddError(SubRangeCurrent, JSColorToTColor(JPlugin.Color),
            JPlugin.Msg, ResultMsg, JPlugin.Filename, JPlugin.Name);
        end;
        if JPlugin.FatalError then
          Break;
      end;
      FreeAndNil(JPlugin);
    end;
    JSPEnum.Free;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  ExecTime := GetTickCount - Start;

  ErrorReportForm.vtvErrorList.EndUpdate;
  if (ErrorReportForm.vtvErrorList.TotalCount > 0) then
  begin
    ErrorReportForm.Visible := True;
    Msg := Format('%d error(s) found', [ErrorReportForm.vtvErrorList.TotalCount]);
  end
  else
    Msg := 'No error found';
  Msg := Msg + Format(' (in %d ms).', [ExecTime]);
  ErrorReportForm.TntStatusBar1.Panels[0].Text := Msg;
  LogForm.SilentLogMsg(Msg);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideLogsExecute(Sender: TObject);
begin
  LogForm.Visible := not LogForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFixSelectedErrorsExecute(Sender: TObject);
begin
  ErrorReportForm.FixSelectedErrors;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFixErrorMainExecute(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    i : integer;
    ResultMsg : WideString;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    NodeData : PTreeData;
    Node : PVirtualNode;
begin
  if not Assigned(vtvSubsList.FocusedNode) then
    Exit;

  UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;

  g_WebRWSynchro.BeginWrite;
  try
    while JSPEnum.GetNext(JPlugin) do
    begin
      JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
      if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = False) then
      begin
        FreeAndNil(JPlugin);
        Continue;
      end;

      ConfigObject.ApplyParam(JPlugin);
      // Install handler to fill UndoableMultiChangeTask
      JPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
      JPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
      JPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

      Node := vtvSubsList.GetFirstSelected;
      while Assigned(Node) do
      begin
        NodeData := vtvSubsList.GetNodeData(Node);
        SubRangeCurrent := NodeData.Range;
        i := WAVDisplayer.RangeList.IndexOf(NodeData.Range);

        if (i > 0) then
          SubRangePrevious := TSubtitleRange(WAVDisplayer.RangeList[i-1])
        else
          SubRangePrevious := nil;

        if (i < WAVDisplayer.RangeList.Count-1) then
          SubRangeNext := TSubtitleRange(WAVDisplayer.RangeList[i+1])
        else
          SubRangeNext := nil;

        ResultMsg := JPlugin.HasError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        if (ResultMsg <> '') then
        begin
          JPlugin.FixError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        end;
        Node := vtvSubsList.GetNextSelected(Node);
      end;

      FreeAndNil(JPlugin);
    end;
    JSPEnum.Free;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);

  if (UndoableMultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(UndoableMultiChangeTask);
    // Do not free the task, it's on the stack now
    UndoableMultiChangeTask := nil;
  end
  else
  begin
    FreeAndNil(UndoableMultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.CanFixError(PluginFilename : WideString) : Boolean;
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
begin
  Result := False;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  JPlugin := JSPEnum.GetPluginByFilename(PluginFilename);
  if Assigned(JPlugin) then
  begin
    JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
    Result := Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = True) and
      JPlugin.CanFixError;
    FreeAndNil(JPlugin);
  end;
  JSPEnum.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var NodeData: PTreeData;
begin
  if (Node <> nil) then
  begin
    NodeData := Sender.GetNodeData(Node);
    MemoSubtitleText.Tag := 0;
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := NodeData.Range.Text;
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
    MemoSubtitleText.Tag := 1;
    if (WAVDisplayer.KaraokeSelectedRange = NodeData.Range) then
      TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex)
    else
      TagHighlight(MemoSubtitleText, -1);
    // Update styles combobox
    UpdateStylesComboboxFromSelection;
    CallJSOnSelectedSubtitle;
  end
  else
  begin
    MemoSubtitleText.Tag := 0;
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := '';
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChangeStart(Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : Integer);
var ChangeSubData : TChangeSubData;
begin
  if Assigned(UndoableMultiChangeTask) then
  begin
    ChangeSubData := UndoableMultiChangeTask.GetData(SubtitleRange.Node.Index);
    if not Assigned(ChangeSubData) then
    begin
      ChangeSubData := TChangeSubData.Create(SubtitleRange.Node.Index);
      UndoableMultiChangeTask.AddData(ChangeSubData);
    end;
    ChangeSubData.OldStart := SubtitleRange.StartTime;
    ChangeSubData.NewStart := NewValue;
    // Subtitle match current selection, update selection accordingly
    if (SubtitleRange = WavDisplayer.SelectedRange) then
    begin
      WavDisplayer.Selection.StartTime := NewValue;
    end;
  end;
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChangeStop(Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : Integer);
var ChangeSubData : TChangeSubData;
begin
  if Assigned(UndoableMultiChangeTask) then
  begin
    ChangeSubData := UndoableMultiChangeTask.GetData(SubtitleRange.Node.Index);
    if not Assigned(ChangeSubData) then
    begin
      ChangeSubData := TChangeSubData.Create(SubtitleRange.Node.Index);
      UndoableMultiChangeTask.AddData(ChangeSubData);
    end;
    ChangeSubData.OldStop := SubtitleRange.StopTime;
    ChangeSubData.NewStop := NewValue;
    // Subtitle match current selection, update selection accordingly
    if (SubtitleRange = WavDisplayer.SelectedRange) then
    begin
      WavDisplayer.Selection.StopTime := NewValue;
    end;
  end;
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChangeText(Sender : TSubtitleRangeJSWrapper;
    SubtitleRange : TSubtitleRange; NewValue : WideString);
var ChangeSubData : TChangeSubData;
begin
  if Assigned(UndoableMultiChangeTask) then
  begin
    ChangeSubData := UndoableMultiChangeTask.GetData(SubtitleRange.Node.Index);
    if not Assigned(ChangeSubData) then
    begin
      ChangeSubData := TChangeSubData.Create(SubtitleRange.Node.Index);
      UndoableMultiChangeTask.AddData(ChangeSubData);
    end;
    ChangeSubData.OldText := SubtitleRange.Text;
    ChangeSubData.NewText := NewValue;
  end;
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FixErrorInList(ErrorList : TList);
var NodeData : PErrorTreeData;
    JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    JSPluginMap : DHashMap;
    Iter : DIterator;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    i, j : Integer;
    Start, ExecTime : Cardinal;
    FixedCount : Cardinal;
    Msg : WideString;
begin
  Start := GetTickCount;
  FixedCount := 0;
  
  UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  JSPluginMap := DHashMap.Create;

  g_WebRWSynchro.BeginWrite;
  try
    for j := 0 to ErrorList.Count-1 do
    begin
      NodeData := ErrorList[j];
      Iter := JSPluginMap.locate([NodeData.Filename]);
      if atEnd(Iter) then
      begin
        JPlugin := JSPEnum.GetPluginByFilename(NodeData.Filename);
        if Assigned(JPlugin) then
        begin
          JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
          if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = True) then
          begin
            ConfigObject.ApplyParam(JPlugin);
            // Install handler to fill UndoableMultiChangeTask
            JPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
            JPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
            JPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;
          end
          else
          begin
            FreeAndNil(JPlugin);
          end;
        end;
        JSPluginMap.putPair([NodeData.Filename, JPlugin]);
      end
      else
      begin
        JPlugin := TJavaScriptPlugin(getObject(Iter));
      end;

      if Assigned(JPlugin) then
      begin
        SubRangeCurrent := NodeData.Range;
        i := WAVDisplayer.RangeList.IndexOf(NodeData.Range);

        if (i > 0) then
          SubRangePrevious := TSubtitleRange(WAVDisplayer.RangeList[i-1])
        else
          SubRangePrevious := nil;

        if (i < WAVDisplayer.RangeList.Count-1) then
          SubRangeNext := TSubtitleRange(WAVDisplayer.RangeList[i+1])
        else
          SubRangeNext := nil;

        JPlugin.FixError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
        Inc(FixedCount);
      end;
    end;
  finally
    g_WebRWSynchro.EndWrite;
    JSPEnum.Free;
    FreeAndClear(JSPluginMap);
  end;

  ExecTime := GetTickCount - Start;

  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);

  if (UndoableMultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(UndoableMultiChangeTask);
    // Do not free the task, it's on the stack now
    UndoableMultiChangeTask := nil;
  end
  else
  begin
    FreeAndNil(UndoableMultiChangeTask);
  end;

  Msg := Format('%d error(s) fixed (in %d ms).', [FixedCount, ExecTime]);
  ErrorReportForm.TntStatusBar1.Panels[0].Text := Msg;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormActivate(Sender: TObject);
begin
  FormHasBeenActivated := True;
end;

//------------------------------------------------------------------------------
procedure TMainForm.MemoSubtitleTextMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Pt : TPoint;
    CharIndex, LineIndex : Integer;
begin
  if FormHasBeenActivated then
  begin
    Pt := Point(X, Y);
    CharIndex := MemoSubtitleText.Perform(Messages.EM_CHARFROMPOS, 0, Integer(@Pt));
    LineIndex := MemoSubtitleText.Perform(EM_LINEFROMCHAR, CharIndex, 0);
    MemoSubtitleText.SelStart := CharIndex + LineIndex;
    FormHasBeenActivated := False;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStartSubExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying then
  begin
    StartSubtitleTime := WAVDisplayer.GetPlayCursorPos;
  end
  else
    StartSubtitleTime := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopSubExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying and (StartSubtitleTime <> -1) and
    (WAVDisplayer.GetPlayCursorPos > (StartSubtitleTime + 400)) then
  begin
    AddSubtitle(StartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', False);
    StartSubtitleTime := -1;
  end
  else
    StartSubtitleTime := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopAndStartSubExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying and (StartSubtitleTime <> -1) and
    (WAVDisplayer.GetPlayCursorPos > (StartSubtitleTime + 400)) then
  begin
    AddSubtitle(StartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', False);
    StartSubtitleTime := WAVDisplayer.GetPlayCursorPos + 1;
  end
  else
    StartSubtitleTime := -1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // subtitle dynamic creation (toggle mode) in timing mode only
  if IsTimingMode and (Key = VK_SPACE) and
    (ConfigObject.EnableToggleCreation = True) and
    (WAVDisplayer.IsPlaying = True) then
  begin
    if (ToggleStartSubtitleTime = -1) then
    begin
      ToggleStartSubtitleTime := WAVDisplayer.GetPlayCursorPos;
      if ConfigObject.SpaceKeyModifyTiming then
      begin
        SetSubtitleStartTime(True);
      end;
    end;
    Key := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // subtitle dynamic creation (toggle mode) in timing mode only
  if IsTimingMode and (Key = VK_SPACE) and
    (ConfigObject.EnableToggleCreation = True) and
    (WAVDisplayer.IsPlaying = True) then
  begin
    if (ToggleStartSubtitleTime <> -1) then
    begin
      if ConfigObject.SpaceKeyModifyTiming then
      begin
        SetFocusedSubtitleStopTime(True, True);
      end
      else
      begin
        if (WAVDisplayer.GetPlayCursorPos > (ToggleStartSubtitleTime + 400)) then
        begin
          AddSubtitle(ToggleStartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', False);
        end;
      end;
      ToggleStartSubtitleTime := -1;
    end;
    Key := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1StartPlaying(Sender: TObject);
begin
  if (ConfigObject.AutoSaveWhenPlaying = True) then
    ActionSave.Execute;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionInsertKaraokeMarkerExecute(Sender: TObject);
var Idx : Integer;
    SubRange : TSubtitleRange;
begin
  Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if (Idx <> -1) then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
    SubRange.AddSubTime(WAVDisplayer.GetCursorPos);
    vtvSubsList.Repaint;
    CurrentProject.IsDirty := True;
  end;
end;

//------------------------------------------------------------------------------

// Split on white space and on '/' that is used to separate sylables
procedure WhiteSpaceSplit(const Text : WideString; var WordArray : TWideStringDynArray);
var i, j : Integer;
begin
  SetLength(WordArray, 0);
  j := 1;
  for i:=1 to Length(Text) do
  begin
    if (Text[i] = ' ') or (Text[i] = #10) or (Text[i] = '/') then
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := Tnt_WideStringReplace(
        Copy(Text, j, i-j+1),
        '/',
        '',
        [rfReplaceAll]);
      j := (i + 1);
    end;
  end;
  if (Length(Text) > 0) then
  begin
    SetLength(WordArray, Length(WordArray)+1);
    WordArray[Length(WordArray)-1] := Copy(Text, j, Length(Text)-j+1);
  end;
end;

// -----

function KaraLen(const Text : WideString) : Integer;
var i : Integer;
begin
  Result := 0;
  for i:=1 to Length(Text) do
    if (Text[i] <> #13) and (Text[i] <> #10) then
      Inc(Result);
end;

// -----

procedure CreateKaraoke(SubRange : TSubtitleRange);
var WordArray : TWideStringDynArray;
    i, j, total : Integer;
    s : WideString;
    tpl : Double; // time per letter
begin
  tpl := (SubRange.StopTime - SubRange.StartTime) /
    KaraLen(StringReplace(SubRange.Text, '/', '', [rfReplaceAll]));
  WhiteSpaceSplit(SubRange.Text, WordArray);

  SubRange.ClearSubTimes;

  // Recreate string with karaoke timing
  s :='';
  total := 0;
  if Length(WordArray) > 0 then
  begin
    for i:=0 to Length(WordArray)-1 do
    begin
      j := Round(tpl * KaraLen(WordArray[i]) / 10);
      total := total + j;
      if (i < Length(WordArray)-1) then
        SubRange.AddSubTime(SubRange.StartTime + (total*10));
      s := s + WideFormat('{\k%d}%s', [j, WordArray[i]]);
    end;
  end;

  // total should match duration (but we work in ms :] ?)

  SubRange.Text := s;
end;

// -----

procedure TMainForm.pmiCreateKaraokeClick(Sender: TObject);
var Node : PVirtualNode;
    NodeData: PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      SubtitleRange := NodeData.Range;
      ChangeSubData := TChangeSubData.Create(Node.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.OldSubTime := SubtitleRange.SubTime;
      CreateKaraoke(SubtitleRange);
      ChangeSubData.NewText := SubtitleRange.Text;
      ChangeSubData.NewSubTime := SubtitleRange.SubTime;
      MultiChangeTask.AddData(ChangeSubData);
      Node := vtvSubsList.GetNextSelected(Node);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure ClearKaraoke(SubRange : TSubtitleRange);
var WordArray : TWideStringDynArray;
    TimeArray : TIntegerDynArray;
    CleanedText : WideString;
    i : Integer;
begin
  CleanedText := '';
  KaraSplit(SubRange.Text, WordArray, TimeArray);
  for i:=0 to Length(WordArray)-1 do
  begin
    CleanedText := CleanedText + WordArray[i];
  end;
  SubRange.Text := CleanedText;
  SubRange.ClearSubTimes;
end;

// -----

procedure TMainForm.pmiClearKaraokeClick(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData: PTreeData;
  MultiChangeTask : TUndoableMultiChangeTask;
  ChangeSubData : TChangeSubData;
  SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      SubtitleRange := NodeData.Range;
      ChangeSubData := TChangeSubData.Create(Node.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.OldSubTime := SubtitleRange.SubTime;
      ClearKaraoke(NodeData.Range);
      ChangeSubData.NewText := SubtitleRange.Text;
      ChangeSubData.NewSubTime := SubtitleRange.SubTime;
      MultiChangeTask.AddData(ChangeSubData);
      Node := vtvSubsList.GetNextSelected(Node);
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

function CalculateNewTextFromSubTime(SubRange : TSubtitleRange) : WideString;
var WordArray : TWideStringDynArray;
    TimeArray : TIntegerDynArray;
    i, AccuTime : Integer;
begin
  Result := SubRange.Text;
  if Length(SubRange.SubTime) <= 0 then
    Exit;
  KaraSplit2(SubRange.Text, WordArray, TimeArray);
  if (Length(SubRange.SubTime) <> (Length(TimeArray) - 1)) then
    Exit;
  AccuTime := SubRange.StartTime;
  for i := Low(SubRange.SubTime) to High(SubRange.SubTime) do
  begin
    TimeArray[i] := Round((SubRange.SubTime[i] - AccuTime) / 10);
    AccuTime := AccuTime + (TimeArray[i] * 10);
  end;
  TimeArray[High(TimeArray)] := Round((SubRange.StopTime - AccuTime) / 10);

  Result := WordArray[0];
  for i := Low(TimeArray) to High(TimeArray) do
  begin
    Result := Result + IntToStr(TimeArray[i]) + WordArray[i + 1];
  end;
end;

procedure TMainForm.WAVDisplayer1KaraokeChanged(Sender: TObject; Range : TRange;
  SubTimeIndex, OldTime : Integer);
var SubRange : TSubtitleRange;
    NewText : WideString;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
begin
  g_WebRWSynchro.BeginWrite;
  try
    SubRange := Range as TSubtitleRange;
    NewText := CalculateNewTextFromSubTime(SubRange);
    if (NewText <> SubRange.Text) then
    begin
      if (SubTimeIndex <> -1) then
      begin
        MultiChangeTask := TUndoableMultiChangeTask.Create;
        ChangeSubData := TChangeSubData.Create(SubRange.Node.Index);
        ChangeSubData.OldSubTime := SubRange.SubTime;
        ChangeSubData.OldSubTime[SubTimeIndex] := OldTime;
        ChangeSubData.OldText := SubRange.Text;
        ChangeSubData.NewSubTime := SubRange.SubTime;
        ChangeSubData.NewText := NewText;
        MultiChangeTask.AddData(ChangeSubData);
        PushUndoableTask(MultiChangeTask);
      end;

      SubRange.Text := NewText;
      CurrentProject.IsDirty := True;
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowStartFrameExecute(Sender: TObject);
var NodeData: PTreeData;
    Fps : Double;
    FrameNumber : Double;
    FrameLen : Double;
    StartTimeFrame : Integer;
begin
  if Assigned(VideoRenderer) and Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

    StartTimeFrame := NodeData.Range.StartTime;

    // Get the video fps from VSFilter
    Fps := VideoRenderer.GetVSFilterFPS;
    if (FPS > 0) then
    begin
      // Calculate the frame duration in s
      FrameLen := (1000.0 / Fps);
      // Calculate the fame number
      FrameNumber := Ceil(StartTimeFrame / FrameLen);
      // DirectShow add a 1 frame delay ???
      FrameNumber := FrameNumber + 1;
      // Convert framenumber back in ms
      StartTimeFrame := Ceil(FrameNumber * FrameLen);
    end;

    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    // Finally show the image
    VideoRenderer.ShowImageAt(StartTimeFrame);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowStopFrameExecute(Sender: TObject);
var NodeData: PTreeData;
    Fps : Double;
    FrameNumber : Double;
    FrameLen : Double;
    StopTimeFrame : Integer;
begin
  if Assigned(VideoRenderer) and Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

    StopTimeFrame := NodeData.Range.StopTime;

    // Get the video fps from VSFilter
    Fps := VideoRenderer.GetVSFilterFPS;
    if (FPS > 0) then
    begin
      // Calculate the frame duration in s
      FrameLen := (1000.0 / Fps);
      // Calculate the fame number
      FrameNumber := Ceil(StopTimeFrame / FrameLen);
      // Don't add the one frame delay we want to see the last frame with subtitle
      // FrameNumber := FrameNumber + 1;
      // Convert framenumber back in ms
      StopTimeFrame := Ceil(FrameNumber * FrameLen);
    end;
    
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    // Finally show the image
    VideoRenderer.ShowImageAt(StopTimeFrame);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowFrameAtCursorExecute(Sender: TObject);
begin
  if Assigned(VideoRenderer) then
  begin
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    VideoRenderer.ShowImageAt(WAVDisplayer.GetCursorPos);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.DetectCharsetDataLoss : Boolean;
var i : integer;
    Subrange : TSubtitleRange;
    ansiCharString : string;
    backWideCharString : WideString;
begin
  Result := False;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    ansiCharString := WC2MB(SubRange.Text);
    backWideCharString := MB2WC(ansiCharString);
    if (backWideCharString <> SubRange.Text) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSRT(Filename, PreviousExt: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    ConvertFunc : function(Src : WideString) : WideString;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;
begin
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  if (PreviousExt = '.ass') or (PreviousExt = '.ssa') then
    ConvertFunc := @ConvertSSAToSRT
  else
    ConvertFunc := @ConvertNull;

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    WriteStringLnStream(IntToStr(i+1), FS);
    WriteStringLnStream(TimeMsToString(SubRange.StartTime,',') + ' --> ' +
      TimeMsToString(SubRange.StopTime,','), FS);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(ConvertFunc(Subrange.Text)), FS)
    else
      WriteStringLnStream(WC2MB(ConvertFunc(Subrange.Text)), FS);
    WriteStringLnStream('', FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSSA(Filename, PreviousExt: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    s : WideString;
    style : TSSAStyle;
    ConvertFunc : function(Src : WideString) : WideString;
    
    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;
begin
  // Write BOM if any
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  if (PreviousExt = '.srt') then
    ConvertFunc := @ConvertSRTToSSA
  else
    ConvertFunc := @ConvertNull;

  // Write Header
  WriteStringLnStream('[Script Info]', FS);
  WriteStringLnStream('; Written by VisualSubSync ' + g_ApplicationVersion.VersionString, FS);
  if (SubtitleFileHeader = '') then
  begin
    WriteStringLnStream('Title: <untitled>', FS);
    WriteStringLnStream('Original Script: <unknown>', FS);
    WriteStringLnStream('ScriptType: v4.00', FS);
    if VideoRenderer.IsOpen and (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
    begin
      WriteStringLnStream(Format('PlayResX: %d',[VideoRenderer.VideoWidth]), FS);
      WriteStringLnStream(Format('PlayResY: %d',[VideoRenderer.VideoHeight]), FS);
    end;
    WriteStringLnStream('PlayDepth: 0', FS);
    WriteStringLnStream('Timer: 100.0', FS);    
  end
  else
  begin
    // Make sure to write "ScriptType: v4.00"
    s := ReplaceRegExpr('ScriptType: v4\.00\+?', SubtitleFileHeader,
      'ScriptType: v4.00');
    WriteStringLnStream(Trim(s), FS);
  end;

  // Write Styles
  WriteStringLnStream('', FS);
  WriteStringLnStream('[v4 Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding', FS);
  for i:=0 to StyleFormInstance.GetCount-1 do
  begin
    style := StyleFormInstance.GetStyleAt(i);
    WriteStringLnStream(style.getAsSSA, FS);
  end;

  WriteStringLnStream('', FS);
  WriteStringLnStream('[Events]', FS);
  WriteStringLnStream('Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text', FS);

  //Dialogue: Marked=0,0:00:05.00,0:00:07.00,*Default,,0000,0000,0000,,toto tata

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    s := Format('Dialogue: %s,%s,%s,%s,%s,%s,%s,%s,%s,',
      [SubRange.Marked, TimeMsToSSAString(SubRange.StartTime),
       TimeMsToSSAString(SubRange.StopTime), SubRange.Style, SubRange.Actor,
       Subrange.LeftMarg, Subrange.RightMarg, SubRange.VertMarg, Subrange.Effect]);
    if InUTF8 then
      s := s + UTF8Encode(Tnt_WideStringReplace(ConvertFunc(Subrange.Text), CRLF, '\N', [rfReplaceAll]))
    else
      s := s + WC2MB(Tnt_WideStringReplace(ConvertFunc(Subrange.Text), CRLF, '\N', [rfReplaceAll]));
    WriteStringLnStream(s, FS);
  end;

  if (SubtitleFileFooter <> '') then
  begin
    WriteStringLnStream('', FS);
    WriteStringLnStream(Trim(SubtitleFileFooter), FS);
  end;
  
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsASS(Filename, PreviousExt: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    s : WideString;
    style : TSSAStyle;
    ConvertFunc : function(Src : WideString) : WideString;

    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;
begin
  // Write BOM if any
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  if (PreviousExt = '.srt') then
    ConvertFunc := @ConvertSRTToSSA
  else
    ConvertFunc := @ConvertNull;

  // Write Header
  WriteStringLnStream('[Script Info]', FS);
  WriteStringLnStream('; Written by VisualSubSync ' + g_ApplicationVersion.VersionString, FS);
  if (SubtitleFileHeader = '') then
  begin
    WriteStringLnStream('Title: <untitled>', FS);
    WriteStringLnStream('Original Script: <unknown>', FS);
    WriteStringLnStream('ScriptType: v4.00+', FS);
    if VideoRenderer.IsOpen and (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
    begin
      WriteStringLnStream(Format('PlayResX: %d',[VideoRenderer.VideoWidth]), FS);
      WriteStringLnStream(Format('PlayResY: %d',[VideoRenderer.VideoHeight]), FS);
    end;
    WriteStringLnStream('PlayDepth: 0', FS);
    WriteStringLnStream('Timer: 100.0', FS);
    WriteStringLnStream('WrapStyle: 0', FS);
  end
  else
  begin
    // Make sure to write "ScriptType: v4.00+"
    s := ReplaceRegExpr('ScriptType: v4\.00\+?', SubtitleFileHeader,
      'ScriptType: v4.00+');
    WriteStringLnStream(Trim(s), FS);
  end;

  // Write Styles
  WriteStringLnStream('', FS);
  WriteStringLnStream('[v4+ Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic,  Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding', FS);
  for i:=0 to StyleFormInstance.GetCount-1 do
  begin
    style := StyleFormInstance.GetStyleAt(i);
    WriteStringLnStream(style.getAsASS, FS);
  end;

  WriteStringLnStream('', FS);
  WriteStringLnStream('[Events]', FS);
  WriteStringLnStream('Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text', FS);

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    s := Format('Dialogue: %s,%s,%s,%s,%s,%s,%s,%s,%s,',
      [SubRange.Layer, TimeMsToSSAString(SubRange.StartTime),
       TimeMsToSSAString(SubRange.StopTime), SubRange.Style,SubRange.Actor,
       Subrange.LeftMarg, Subrange.RightMarg, SubRange.VertMarg, Subrange.Effect]);
    if InUTF8 then
      s := s + UTF8Encode(Tnt_WideStringReplace(ConvertFunc(Subrange.Text) ,CRLF, '\N', [rfReplaceAll]))
    else
      s := s + WC2MB(Tnt_WideStringReplace(ConvertFunc(Subrange.Text), CRLF, '\N', [rfReplaceAll]));
    WriteStringLnStream(s, FS);
  end;
  
  if (SubtitleFileFooter <> '') then
  begin
    WriteStringLnStream('', FS);
    WriteStringLnStream(Trim(SubtitleFileFooter), FS);
  end;

  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1SelectedKaraokeRange(Sender: TObject;
  Range : TRange);
var Node : PVirtualNode;
begin
  if Assigned(Range) then
  begin
    Node := TSubtitleRange(Range).Node;
    vtvSubsList.ScrollIntoView(Node, True);
    vtvSubsList.FocusedNode := Node;
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[Node] := True;
  end;
  TagHighlight(MemoSubtitleText,WAVDisplayer.KaraokeSelectedIndex);
end;

//------------------------------------------------------------------------------

type
  TAccessCanvas = class(TCanvas);

procedure WideCanvasDrawText(Canvas: TCanvas; Rect: TRect;
  const Text: WideString; AllowLineBreak : Boolean = False);
var
  Options: Cardinal;
begin
  with TAccessCanvas(Canvas) do begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := DT_END_ELLIPSIS or DT_NOPREFIX or DT_EDITCONTROL;
    if AllowLineBreak then
      Options := Options or DT_WORDBREAK;
    Windows.DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Options);
    Changed;
  end;
end;

procedure TMainForm.WAVDisplayer1CustomDrawRange(Sender: TObject;
  ACanvas: TCanvas; Range : TRange; Rect : TRect);
begin
  if (not ConfigObject.ShowTextInWAVDisplay) then
    Exit;
  if Length(Range.SubTime) > 0 then
  begin
    // TODO : karaoke support
  end
  else
  begin
    InflateRect(Rect, -5, -5);
    if (Rect.Right - Rect.Left) > 25 then
    begin
      ACanvas.Font.Assign(MemoSubtitleText.Font);
      ACanvas.Font.Color := ACanvas.Pen.Color;
      WideCanvasDrawText(ACanvas, Rect, TSubtitleRange(Range).Text);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionExportToWAVExecute(Sender: TObject);
var
  SelectedDir: string;
  WAVFile : TWAVFile;
  Node : PVirtualNode;
  NodeData: PTreeData;
  Filename : string;
  FS : TFileStream;
  PP : TPageProcessor;
  DummyEnvVars : TStringList;
  CM : ICursorManager;
  //SelectedSubList : TRangeList;
begin
  if Trim(CurrentProject.WAVFile) = '' then
  begin
    MessageBoxW(Handle,
      PWideChar(WideString('You need to create a project with a WAV file')),
      PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
  end
  else
  begin
    if SelectDirectory('Select the folder where to place wav files', '', SelectedDir) = True then
    begin
      SelectedDir := IncludeTrailingPathDelimiter(SelectedDir);
      WAVFile := TWAVFile.Create;
      if WAVFile.Open(CurrentProject.WAVFile) then
      begin
        CM := TCursorManager.Create(crHourGlass);
        ShowStatusBarMessage('Exporting to wav files, please wait...');
        {
        if (vtvSubsList.SelectedCount > 1) then
        begin
          SelectedSubList := TRangeList.Create;
          Node := vtvSubsList.GetFirstSelected
        end else begin
          SelectedSubList := nil;}
          Node := vtvSubsList.GetFirst;{
        end;
        }
        
        while (Node <> nil) do
        begin
          NodeData := vtvSubsList.GetNodeData(Node);
          Filename := Format('%s%6.6d.wav', [SelectedDir, Node.Index+1]);
          FS := TFileStream.Create(Filename, fmCreate);
          WAVFile.ExtractToStream(NodeData.Range.StartTime,
            NodeData.Range.StopTime, FS);
          FS.Free;
          {if (vtvSubsList.SelectedCount > 1) then
          begin
            SelectedSubList.Add(NodeData.Range);
            Node := vtvSubsList.GetNextSelected(Node);
          end else begin}
            Node := vtvSubsList.GetNext(Node);
          {end;}
        end;

        {if (vtvSubsList.SelectedCount > 1) then
        begin
          g_GlobalContext.SubList := SelectedSubList;
        end;}

        PP := TPageProcessor.Create;
        DummyEnvVars := TStringList.Create;
        FS := TFileStream.Create(SelectedDir + 'index_wav.html', fmCreate);
        PP.ProcessPage(ServerRootDir + 'index_wav_export.shtml', FS, DummyEnvVars);
        PP.Free;
        FS.Free;
        DummyEnvVars.Free;

        {if (vtvSubsList.SelectedCount > 1) then
        begin
          // Restore complete sub list
          g_GlobalContext.SubList := WAVDisplayer.RangeList;
          SelectedSubList.Free;
        end;}
      end;
      WAVFile.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaySelStartExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionStart;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection.StartTime,
      Min(WAVDisplayer.Selection.StartTime + StartEndPlayingDuration, WAVDisplayer.Selection.StopTime));
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaySelEndExecute(Sender: TObject);
begin
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    UpdateSubtitleForPreview(VideoPreviewNeedSubtitleUpdate);
    PlayingMode := pmtSelectionEnd;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
      WAVDisplayer.Selection.StopTime);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaceKaraokeCursorsAtEndExecute(Sender: TObject);
var NodeData: PTreeData;
    SubRange : TSubtitleRange;
    i, CompressedStart : Integer;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
const CompressedDuration : Integer = 100;
begin
  // Compress karaoke cursors at end of subtitle
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    SubRange := NodeData.Range;
    CompressedStart := SubRange.StopTime - (Length(SubRange.SubTime) * CompressedDuration);
    if (CompressedStart > SubRange.StartTime) then
    begin
      MultiChangeTask := TUndoableMultiChangeTask.Create;
      ChangeSubData := TChangeSubData.Create(vtvSubsList.FocusedNode.Index);
      MultiChangeTask.AddData(ChangeSubData);
      ChangeSubData.OldText := SubRange.Text;
      ChangeSubData.OldSubTime := SubRange.SubTime;
      try
        g_WebRWSynchro.BeginWrite;
        for i := Low(SubRange.SubTime) to High(SubRange.SubTime) do
        begin
          SubRange.SubTime[i] := CompressedStart + (CompressedDuration * i);
        end;
        SubRange.Text := CalculateNewTextFromSubTime(SubRange);
      finally
        g_WebRWSynchro.EndWrite;
      end;
      if (ChangeSubData.OldText <> SubRange.Text) then
      begin
        ChangeSubData.NewText := SubRange.Text;
        ChangeSubData.NewSubTime := SubRange.SubTime;
        PushUndoableTask(MultiChangeTask);
        CurrentProject.IsDirty := True;
        WAVDisplayer.UpdateView([uvfRange]);
        vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
        vtvSubsList.Repaint;
      end
      else
      begin
        FreeAndNil(MultiChangeTask);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlayToEndExecute(Sender: TObject);
begin
  if IsTimingMode then
    PanelWAVDisplay.SetFocus;
  WAVDisplayer.AutoScrolling := True;
  if WAVDisplayer.SelectionIsEmpty then
  begin
    PlayingMode := pmtAll;
    WAVDisplayer.PlayRange(WAVDisplayer.GetCursorPos,WAVDisplayer.Length)
  end
  else
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(WAVDisplayer.Selection.StartTime,WAVDisplayer.Length);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlay1sBeforeExecute(Sender: TObject);
var NodeData: PTreeData;
begin
  if (not WAVDisplayer.SelectionIsEmpty) then
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime - 1000, 0),
      WAVDisplayer.Selection.StopTime);
  end
  else if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(NodeData.Range.StartTime - 1000, 0),
      NodeData.Range.StopTime)
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPlay1sBeforeToEndExecute(Sender: TObject);
var NodeData: PTreeData;
begin
  if (not WAVDisplayer.SelectionIsEmpty) then
  begin
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime - 1000, 0),
      WAVDisplayer.Length);
  end
  else if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    PlayingMode := pmtSelection;
    WAVDisplayer.PlayRange(
      Max(NodeData.Range.StartTime - 1000, 0),
      WAVDisplayer.Length)
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSelectNextSubExecute(Sender: TObject);
begin
  SelectNextSub;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSelectPreviousSubExecute(Sender: TObject);
begin
  SelectPreviousSub;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsCUE(Filename: WideString);
var i : integer;
    SubRange : TSubtitleRange;
    FS : TTntFileStream;
    SubText : WideString;

    procedure WriteStringLnStream(str : string; Stream : TStream);
    begin
      str := str + #13#10;
      Stream.Write(str[1], Length(str));
    end;
begin
  FS := TTntFileStream.Create(Filename, fmCreate);

  WriteStringLnStream('FILE "YourFileHere.wav" WAVE', FS);
  WriteStringLnStream('  PERFORMER "Performer Here"',FS);
  WriteStringLnStream('  TRACK 01 AUDIO', FS);
  WriteStringLnStream('    INDEX 01 00:00:00', FS);

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    SubText := Subrange.Text;
    // Remove line breaks
    SubText := Tnt_WideStringReplace(SubText, ' ' + CRLF + ' ', ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, ' ' + CRLF, ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, CRLF + ' ', ' ', [rfReplaceAll]);
    SubText := Tnt_WideStringReplace(SubText, CRLF, ' ', [rfReplaceAll]);

    WriteStringLnStream(Format('  TRACK %2.2d AUDIO', [i*2+2]), FS);
    WriteStringLnStream(Format('    INDEX 01 %s', [TimeMsToCUE(SubRange.StartTime)]), FS);
    WriteStringLnStream(Format('    TITLE "%s"', [SubText]), FS);
    
    WriteStringLnStream(Format('  TRACK %2.2d AUDIO', [i*2+3]), FS);
    WriteStringLnStream(Format('    INDEX 01 %s', [TimeMsToCUE(SubRange.StopTime)]), FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PanelVideoResize(Sender: TObject);
begin
  if Assigned(CurrentProject) and PanelVideo.Enabled and
    (CurrentProject.VideoPanelWidth <> PanelVideo.Width)
    then
  begin
    CurrentProject.VideoPanelWidth := PanelVideo.Width;
    CurrentProject.VideoPanelHeight := PanelVideo.Height;
    SaveProject(CurrentProject, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PanelVideoDblClick(Sender: TObject);
begin
  ActionDetachVideo.Execute;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetSubtitleStartTime(SetStopTime : Boolean);
var NodeData : PTreeData;
    NewStartTime : Integer;
    DurationTarget : Integer;
    PreviousNode : PVirtualNode;
    PreviousNodeData : PTreeData;
    AutofixOverlapping : Boolean;
    MinDurationBetweenSub : Integer;
begin
  if not Assigned(vtvSubsList.FocusedNode) then
    Exit;

  try
    g_WebRWSynchro.BeginWrite;
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    if WAVDisplayer.IsPlaying then
      NewStartTime := WAVDisplayer.GetPlayCursorPos
    else
      NewStartTime := WAVDisplayer.GetCursorPos;

    // Autofix overlapping and ensure blank between subtitles
    PreviousNode := vtvSubsList.GetPrevious(vtvSubsList.FocusedNode);
    AutofixOverlapping := (ConfigObject.SpaceKeyBlankBetweenSubtitles > 0);
    if AutofixOverlapping and Assigned(PreviousNode) then
    begin
      PreviousNodeData := vtvSubsList.GetNodeData(PreviousNode);
      if (PreviousNodeData.Range.StopTime >= NewStartTime) or
         ((NewStartTime - PreviousNodeData.Range.StopTime) < ConfigObject.SpaceKeyBlankBetweenSubtitles) then
      begin
        if (PreviousNodeData.Range.StopTime >= NewStartTime) then
        begin
          MinDurationBetweenSub := ConfigObject.SpaceKeyBlankBetweenSubtitles;
          PreviousNodeData.Range.StopTime := NewStartTime - (MinDurationBetweenSub div 2);
          NewStartTime := PreviousNodeData.Range.StopTime + MinDurationBetweenSub;
        end
        else
        begin
          MinDurationBetweenSub := ConfigObject.SpaceKeyBlankBetweenSubtitles -
            (NewStartTime - PreviousNodeData.Range.StopTime);
          PreviousNodeData.Range.StopTime := PreviousNodeData.Range.StopTime - (MinDurationBetweenSub div 2);
          NewStartTime := PreviousNodeData.Range.StopTime + ConfigObject.SpaceKeyBlankBetweenSubtitles;
        end;
      end;
    end;

    NodeData.Range.StartTime := NewStartTime;
    // Fix stop time to be superior to start time
    if (NewStartTime > NodeData.Range.StopTime) then
    begin
      NodeData.Range.StopTime := NewStartTime + 100;
    end;

    if (SetStopTime) then
    begin
      // Set stop time based on CPS target
      DurationTarget := Round((Length(StripTags(NodeData.Range.Text)) / ConfigObject.SpaceKeyCPSTarget) * 1000);
      NodeData.Range.StopTime := NewStartTime + Max(DurationTarget,
        ConfigObject.SpaceKeyMinimalDuration);
    end;
    FullSortTreeAndSubList; // lazy me :) but well it's fast enough
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleStartTimeExecute(Sender: TObject);
begin
  SetSubtitleStartTime(False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetFocusedSubtitleStopTime(Advance: Boolean; KeepStopIfSuperiorToCPSTarget : Boolean);
var NodeData : PTreeData;
    NewStopTime, NewStopCPS, NewDuration : Integer;
    UpdateStop : Boolean;
begin
  if not Assigned(vtvSubsList.FocusedNode) then
  begin
    Exit;
  end;

  NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

  if WAVDisplayer.IsPlaying then
  begin
    NewStopTime := WAVDisplayer.GetPlayCursorPos;
    // When the system is slow the play cursor can hang for a while and NewStopTime can be
    // strictly equal to the previous StartTime.
    // The method would normally abort in this case (NewDuration <= 0) but we don't want that in timing mode.
    if (NewStopTime = NodeData.Range.StartTime) then
    begin
      Inc(NewStopTime);
    end;
  end
  else
    NewStopTime := WAVDisplayer.GetCursorPos;

  NewDuration := NewStopTime - NodeData.Range.StartTime;

  if (NewDuration <= 0) then
  begin
    Exit;
  end;

  UpdateStop := True;
  if KeepStopIfSuperiorToCPSTarget then
  begin
    NewStopCPS := Round(Length(StripTags(NodeData.Range.Text)) / NewDuration * 1000.0);
    if (NewStopCPS > ConfigObject.SpaceKeyCPSTarget) or
      (NewDuration <= ConfigObject.SpaceKeyMinimalDuration) then
    begin
      UpdateStop := False;
    end;
  end;

  if UpdateStop then
  begin
    try
      g_WebRWSynchro.BeginWrite;
      NodeData.Range.StopTime := NewStopTime;
      CurrentProject.IsDirty := True;
    finally
      g_WebRWSynchro.EndWrite;
    end;
  end;

  if Advance then
  begin
    AdvanceToNextSubtitleAfterFocus;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleStopTimeExecute(Sender: TObject);
begin
  SetFocusedSubtitleStopTime(False, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.AdvanceToNextSubtitleAfterFocus;
var
  NextNodeToFocus : PVirtualNode;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NextNodeToFocus := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    if Assigned(NextNodeToFocus) then
    begin
      vtvSubsList.FocusedNode := NextNodeToFocus;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[NextNodeToFocus] := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleStopTimeAndGoToNextExecute(Sender: TObject);
begin
  SetFocusedSubtitleStopTime(True, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.cbStylesSelect(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData : PTreeData;
  NewStyle : WideString;
  SubChanged : Boolean;
begin
  if cbStyles.ItemIndex = -1 then
    Exit;
  SubChanged := False;
  try
    g_WebRWSynchro.BeginWrite;
    NewStyle := cbStyles.Items.Strings[cbStyles.ItemIndex];
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      if (NodeData.Range.Style <> NewStyle) then
      begin
        NodeData.Range.Style := NewStyle;
        SubChanged := True;
      end;
      Node := vtvSubsList.GetNextSelected(node);
    end;
    if SubChanged then
    begin
      CurrentProject.IsDirty := True;
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  if SubChanged then
  begin
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateStylesComboBox;
var i : Integer;
    style : TSSAStyle;
begin
  cbStyles.Clear;
  for i := 0 to StyleFormInstance.GetCount-1 do
  begin
    style := StyleFormInstance.GetStyleAt(i);
    cbStyles.AddItem(style.name, nil);
  end;
  UpdateStylesComboboxFromSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStylesExecute(Sender: TObject);
begin
  // Try to pre sselect current subtitle style
  StyleFormInstance.PreSelect(cbStyles.Text);
  StyleFormInstance.ShowModal;
  if StyleFormInstance.HaveStylesChanged then
  begin
    UpdateStylesComboBox;
    CurrentProjectSetDirty;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateStylesComboboxFromSelection;
var NodeCursor : PVirtualNode;
    NodeCursorData, PreviousNodeData : PTreeData;
    SeveralStylesSelected : Boolean;
begin
  SeveralStylesSelected := False;
  PreviousNodeData := nil;
  NodeCursor := vtvSubsList.GetFirstSelected;
  while Assigned(NodeCursor) do
  begin
    NodeCursorData := vtvSubsList.GetNodeData(NodeCursor);
    if (PreviousNodeData <> nil) and (NodeCursorData.Range.Style <> PreviousNodeData.Range.Style) then
    begin
      SeveralStylesSelected := True;
      Break;
    end;
    PreviousNodeData := NodeCursorData;
    NodeCursor := vtvSubsList.GetNextSelected(NodeCursor);
  end;

  if SeveralStylesSelected then
    cbStyles.ItemIndex := -1
  else
  begin
    NodeCursor := vtvSubsList.GetFirstSelected;
    if Assigned(NodeCursor) then
    begin
      NodeCursorData := vtvSubsList.GetNodeData(NodeCursor);
      cbStyles.ItemIndex := cbStyles.Items.IndexOf(NodeCursorData.Range.Style);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.RenameStyle(OldName, NewName : WideString);
var Node : PVirtualNode;
    NodeData : PTreeData;
    SubChanged : Boolean;
begin
  SubChanged := False;
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirst;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      if (NodeData.Range.Style = OldName) then
      begin
        NodeData.Range.Style := NewName;
        SubChanged := True;
      end;
      Node := vtvSubsList.GetNext(Node);
    end;
    if SubChanged then
    begin
      CurrentProject.IsDirty := True;
    end;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  if SubChanged then
  begin
    vtvSubsList.Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPauseExecute(Sender: TObject);
begin
  if WAVDisplayer.IsPlaying then
  begin
    WAVDisplayer.Pause;
  end
  else
  begin
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionToggleTimingModeExecute(Sender: TObject);
begin
  if IsNormalMode then
  begin
    bttWorkingMode.Caption := 'Timing';
    bttWorkingMode.Font.Color := clRed;
    bttWorkingMode.Tag := 1;
    if ConfigObject.MouseEnableSSATimingMode then
      WAVDisplayer.SelMode := smSSA
    else
      WAVDisplayer.SelMode := smCoolEdit;
  end
  else
  begin
    bttWorkingMode.Caption := 'Normal';
    bttWorkingMode.Tag := 0;
    bttWorkingMode.Font.Color := clWindowText;
    WAVDisplayer.SelMode := smCoolEdit;
  end;
  KeyPreview := IsTimingMode;
  SetShortcut(IsTimingMode);
  MemoSubtitleText.Enabled := MemoLinesCounter.Enabled and
    ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateVolume;
begin
  AudioOnlyRenderer.SetVolume(tbVolume.Position);
  VideoRenderer.SetVolume(tbVolume.Position);
end;

//------------------------------------------------------------------------------

procedure TMainForm.tbVolumeChange(Sender: TObject);
begin
  UpdateVolume;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetEditorFocusExecute(Sender: TObject);
begin
  if MemoSubtitleText.Enabled then
    MemoSubtitleText.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetEditorFocusAndSelectExecute(Sender: TObject);
begin
  if MemoSubtitleText.Enabled then
    MemoSubtitleText.SetFocus;
  MemoSubtitleText.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateSubtitleForPreview(ForceUpdate : Boolean);
var TmpDstFilename : WideString;
    StartTime : Cardinal;
begin
  // TODO : Clean old files in temp directory
  
  if DisableVideoUpdatePreview
     or (Trim(CurrentProject.SubtitlesFile) = '')
     or (not VideoRenderer.IsOpen) then
  begin
    Exit;
  end;

  // Save current file as tmp file, reload subtitle if loaded
  StartTime := GetTickCount;
  TmpDstFilename := GetTemporaryFolder;
  if (Length(TmpDstFilename) > 0) then
  begin
    TmpDstFilename := TmpDstFilename + 'VisualSubSync\';
    // Create directory
    if not WideDirectoryExists(TmpDstFilename) then
      WideCreateDir(TmpDstFilename);
    TmpDstFilename := TmpDstFilename + WideExtractFileName(CurrentProject.SubtitlesFile);
  end
  else
  begin
    TmpDstFilename := WideExtractFilePath(CurrentProject.SubtitlesFile) + '~' +
      WideExtractFileName(CurrentProject.SubtitlesFile);
  end;
  if ShowingVideo then
  begin
    if (VideoRenderer.IsPlaying or VideoRenderer.IsPaused or ForceUpdate) then
    begin
      SaveSubtitles(TmpDstFilename, '', CurrentProject.IsUTF8, True);
      VideoRenderer.SetSubtitleFilename(TmpDstFilename);
      if VideoRenderer.IsPaused then
      begin
        VideoRenderer.UpdateImage;
      end;
      VideoPreviewNeedSubtitleUpdate := False;
      //LogForm.SilentLogMsg('Save took ' + IntToStr(GetTickCount - StartTime) +' ms');
    end
  end
  else
  begin
    VideoPreviewNeedSubtitleUpdate := True;
  end
end;

//------------------------------------------------------------------------------

procedure TMainForm.CurrentProjectOnDirtySet(Sender: TObject);
begin
  UpdateSubtitleForPreview(False);
  VideoPreviewNeedSubtitleUpdate := True;
  CallJSOnSubtitleModification;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CurrentProjectSetDirty;

begin
  g_WebRWSynchro.BeginWrite;
  try
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNextErrorExecute(Sender: TObject);
begin
  ErrorReportForm.GoToNextError;
end;

//------------------------------------------------------------------------------

function TMainForm.GetVideoRendererFiltersList(list : TStrings) : Boolean;
begin
  Result := VideoRenderer.GetFilters(list);
end;

//------------------------------------------------------------------------------

procedure TMainForm.GetCurrentPreviousNextSubtitles(CurrentNode : PVirtualNode; var CurrentSub,
  PreviousSub, NextSub : TSubtitleRange);
var NextNode, PreviousNode : PVirtualNode;
    NodeData : PTreeData;
begin
  CurrentSub := nil;
  PreviousSub := nil;
  NextSub := nil;

  if Assigned(CurrentNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(CurrentNode);
    CurrentSub := NodeData.Range;

    NextNode := vtvSubsList.GetNext(CurrentNode);
    if Assigned(NextNode) then
    begin
      NodeData := vtvSubsList.GetNodeData(NextNode);
      NextSub := NodeData.Range;
    end;

    PreviousNode := vtvSubsList.GetPrevious(CurrentNode);
    if Assigned(PreviousNode) then
    begin
      NodeData := vtvSubsList.GetNodeData(PreviousNode);
      PreviousSub := NodeData.Range;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CallJSOnSubtitleModification;
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifySubtitleModification(CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CallJSOnSelectedSubtitle;
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifySelectionModification(CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1RangeStartDblClick(Sender: TObject; Range : TRange);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    // Install handler to fill UndoableMultiChangeTask
    GeneralJSPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
    GeneralJSPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
    GeneralJSPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifyRangeStartDblClick(CurrentSub, PreviousSub, NextSub);

    GeneralJSPlugin.OnSubtitleChangeStart := nil;
    GeneralJSPlugin.OnSubtitleChangeStop := nil;
    GeneralJSPlugin.OnSubtitleChangeText := nil;

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1RangeStopDblClick(Sender: TObject; Range : TRange);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    // Install handler to fill UndoableMultiChangeTask
    GeneralJSPlugin.OnSubtitleChangeStart := OnSubtitleRangeJSWrapperChangeStart;
    GeneralJSPlugin.OnSubtitleChangeStop := OnSubtitleRangeJSWrapperChangeStop;
    GeneralJSPlugin.OnSubtitleChangeText := OnSubtitleRangeJSWrapperChangeText;

    GetCurrentPreviousNextSubtitles(vtvSubsList.FocusedNode, CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifyRangeStopDblClick(CurrentSub, PreviousSub, NextSub);

    GeneralJSPlugin.OnSubtitleChangeStart := nil;
    GeneralJSPlugin.OnSubtitleChangeStop := nil;
    GeneralJSPlugin.OnSubtitleChangeText := nil;

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnJsSetStatusBarText(const Msg : WideString);
begin
  SetStatusBarPrimaryText(Msg);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ProcessParams;
begin
  if (WideParamCount = 1) then
  begin
    MainForm.LoadProject(WideParamStr(1));
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsTXT(Filename, PreviousExt: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    Text : WideString;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;
begin
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    Text := Subrange.Text;
    // Remove line breaks
    Text := Tnt_WideStringReplace(Text, ' ' + CRLF + ' ', ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, ' ' + CRLF, ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, CRLF + ' ', ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, CRLF, ' ', [rfReplaceAll]);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(Text), FS)
    else
      WriteStringLnStream(WC2MB(Text), FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ApplyDelay(var Indexes : array of Integer;
  DelayInMs : Integer; DelayShiftType : TDelayShiftType);
var i, Idx : Integer;
    DelaySelectedRange : Boolean;
    Range : TRange;
    SubtitleRange : TSubtitleRange;
    DelayedRangeList : TList;

    procedure DelaySub(Range : TRange; DelayInMs : Integer;
      DelayShiftType : TDelayShiftType);
    var i : Integer;
    begin
      if (DelayShiftType = dstBothTime) or (DelayShiftType = dstStartTimeOnly) then
      begin
        Range.StartTime := Range.StartTime + DelayInMs;
      end;
      if (DelayShiftType = dstBothTime) or (DelayShiftType = dstStopTimeOnly) then
      begin
        Range.StopTime := Range.StopTime + DelayInMs;
      end;
      if (DelayShiftType = dstBothTime) then
      begin
        // Shift karaoke subtime
        for i:=0 to Length(Range.SubTime)-1 do
        begin
          Range.SubTime[i] := Range.SubTime[i] + DelayInMs;
        end;
      end;
    end;

begin
  DelayedRangeList := TList.Create;
  DelaySelectedRange := False;
  g_WebRWSynchro.BeginWrite;
  try
    // Apply the delay on each subtitle based on their current index
    for i := Low(Indexes) to High(Indexes) do
    begin
      Idx := Indexes[i];
      Range := WavDisplayer.RangeList[Idx];
      DelayedRangeList.Add(Range);
      DelaySub(Range, DelayInMs, DelayShiftType);
      if (Range = WavDisplayer.SelectedRange) then
      begin
        DelaySelectedRange := True;
      end;
    end;
    // Delay the selected range if necessary 
    if (DelaySelectedRange = True) and Assigned(WavDisplayer.SelectedRange) then
    begin
      DelaySub(WavDisplayer.Selection, DelayInMs, DelayShiftType);
      if (DelayShiftType in [dstStartTimeOnly, dstStopTimeOnly]) and
         (Length(WavDisplayer.SelectedRange.SubTime) > 0) then
      begin
        // TODO : check undo for this
        WAVDisplayer1KaraokeChanged(WAVDisplayer, WavDisplayer.SelectedRange, -1, -1);
      end;
    end;
    // Sort subtitles
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  // Update SearchNodeIndex  (TODO : SearchPos)  

  // Update indexes which have been changed during sorting
  for i := 0 to DelayedRangeList.Count-1 do
  begin
    SubtitleRange := TSubtitleRange(DelayedRangeList[i]);
    Indexes[i] := SubtitleRange.Node.Index;
  end;
  DelayedRangeList.Free;  

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.DeleteSubtitles(var Indexes : array of Integer);
var I, Idx : Integer;
    SubtitleRange : TSubtitleRange;
    NodeToFocus : PVirtualNode;
begin
  g_WebRWSynchro.BeginWrite;
  try
    for I := High(Indexes) downto Low(Indexes) do
    begin
      Idx := Indexes[I];
      SubtitleRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
      ErrorReportForm.DeleteError(SubtitleRange);
      vtvSubsList.DeleteNode(SubtitleRange.Node);
      WAVDisplayer.DeleteRangeAtIdx(Idx, False);

      // Update SearchNodeIndex  (TODO : SearchPos)
      if (SearchNodeIndex > Idx) then
        Dec(SearchNodeIndex);
      if (SearchNodeIndex >= Idx) then
        SearchPos := 1;
    end;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  WAVDisplayer.UpdateView([uvfRange]);

  // Set focus
  Idx := Indexes[0];
  if (Idx > 0) then
  begin
    // Get the subtitle range just before the first deleted one
    SubtitleRange := TSubtitleRange(WAVDisplayer.RangeList[Idx - 1]);
    // The node to focus is the node just after it
    NodeToFocus := vtvSubsList.GetNext(SubtitleRange.Node);
  end
  else
  begin
    NodeToFocus := vtvSubsList.GetFirst;
  end;

  if Assigned(NodeToFocus) then
  begin
    FocusNode(NodeToFocus, False);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.DeleteSubtitle(Index : Integer);
var ParamArray : array[0..0] of Integer;
begin
  ParamArray[0] := Index;
  DeleteSubtitles(ParamArray);
end;

//------------------------------------------------------------------------------

procedure TMainForm.CloneSubtitles(var Indexes : array of Integer; List : TList);
var i, idx : integer;
    Range, RangeClone : TRange;
begin
  List.Clear;
  for i := Low(Indexes) to High(Indexes) do
  begin
    idx := Indexes[i];
    Range := WAVDisplayer.RangeList[Idx];
    RangeClone := SubRangeFactory.CreateRange;
    RangeClone.Assign(Range);
    List.Add(RangeClone);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.RestoreSubtitles(List : TList; Indexes : TIntegerDynArray);
var i : integer;
    Range : TSubtitleRange;
    Node : PVirtualNode;
begin
  for i := List.Count-1 downto 0 do
  begin
    Range := List[i];
    Node := AddSubtitle(Range);
    Range.Node := Node;
  end;

  if Assigned(Indexes) then
  begin
    for i := 0 to List.Count-1 do
    begin
      Range := List[i];
      Indexes[i] := Range.Node.Index;    
    end;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SplitSubtitle(Index, SplitTime, BlankTime : Integer);
var SubRange, NewSubRange : TSubtitleRange;
    NewNode : PVirtualNode;
    NodeData : PTreeData;
    SplitTime1, SplitTime2 : Integer;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  NewSubRange := TSubtitleRange(SubRangeFactory.CreateRange);
  NewSubRange.Assign(SubRange);

  SplitTime1 := SplitTime - (BlankTime div 2);
  SplitTime2 := SplitTime + (BlankTime div 2);
  // Make sure there is enough space for the blank time
  if (SplitTime1 < SubRange.StartTime) or (SplitTime2 > SubRange.StopTime) then
  begin
    // no blank time
    SplitTime1 := SplitTime - 1;
    SplitTime2 := SplitTime;
  end;
  // Make sur there is no overlapping
  if (SplitTime1 = SplitTime2) then
  begin
    SplitTime1 := SplitTime1 - 1;
  end;
  SubRange.StopTime := SplitTime1;
  NewSubRange.StartTime := SplitTime2;

  // Update SearchIdx and SearchPos

  NewNode := vtvSubsList.InsertNode(SubRange.Node, amInsertAfter);
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NodeData.Range := NewSubRange;
  NewSubRange.Node := NewNode;
  g_WebRWSynchro.BeginWrite;
  try
    WAVDisplayer.AddRange(NewSubRange);
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsList.Repaint;
end;

// -----------------------------------------------------------------------------

function TMainForm.SetSubtitleTime(Index, NewStartTime, NewStopTime : Integer) : Integer;
var SubRange : TSubtitleRange;
    NewText : WIdeString;
begin
  try
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
    SubRange.StartTime := NewStartTime;
    SubRange.StopTime := NewStopTime;
    if (Length(SubRange.SubTime) > 0) then
    begin
      NewText := CalculateNewTextFromSubTime(SubRange);
      if (NewText <> SubRange.Text) then
        SubRange.Text := NewText;
    end;    
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if (WAVDisplayer.SelectedRange = SubRange) then
  begin
    WAVDisplayer.Selection.StartTime := NewStartTime;
    WAVDisplayer.Selection.StopTime := NewStopTime;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  if (Length(SubRange.SubTime) > 0) then
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;

  Result := SubRange.Node.Index;
end;

// -----------------------------------------------------------------------------

function TMainForm.MergeSubtitles(var FIndexes : array of Integer; MergeType : TMergeType) : TSubtitleRange;
var i, idx : integer;
    AccuText : WideString;
    LastStopTime : Integer;
    FirstRange, SubRange : TSubtitleRange;
begin
  idx := FIndexes[Low(FIndexes)];
  FirstRange := TSubtitleRange(WAVDisplayer.RangeList[idx]);
  if (MergeType = mtDialog) then
    AccuText := '- ' + FirstRange.Text
  else if (MergeType = mtOneLine) then
    AccuText := TrimRight(FirstRange.Text)
  else
    AccuText := FirstRange.Text;

  LastStopTime := FirstRange.StopTime;
  for i := Low(FIndexes) + 1 to High(FIndexes) do
  begin
    idx := FIndexes[i];
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[idx]);
    if (MergeType = mtDialog) then
      AccuText := AccuText + CRLF + '- ' + SubRange.Text
    else if (MergeType = mtOneLine) then
      AccuText := TrimRight(AccuText) + ' ' + TrimLeft(SubRange.Text)
    else
      AccuText := AccuText + CRLF + SubRange.Text;
    LastStopTime := Max(LastStopTime, SubRange.StopTime);
  end;
  Result := TSubtitleRange(SubRangeFactory.CreateRange);
  Result.Assign(FirstRange);
  Result.StopTime := LastStopTime;
  Result.Text := Trim(AccuText);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.OnUndo(Sender: TTntRichEdit; UndoTask : TUndoableTask);
var UndoableSubTextTask : TUndoableSubTextTask;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    UndoableSubTextTask := TUndoableSubTextTask.Create;
    UndoableSubTextTask.SetData(vtvSubsList.FocusedNode.Index, UndoTask);
    PushUndoableTask(UndoableSubTextTask);
  end
  else
    PushUndoableTask(UndoTask);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.ProcessMultiChangeSub(ChangeList : TList; IsUndo : Boolean);
var i : integer;
    ChangeSubData : TChangeSubData;
    SubtitleRange : TSubtitleRange;
    ModifiedRangeList : TList;
begin
  if (not Assigned(ChangeList)) or (ChangeList.Count = 0) then
    Exit;

  ModifiedRangeList := TList.Create;
  g_WebRWSynchro.BeginWrite;
  try
    for i := 0 to ChangeList.Count-1 do
    begin
      ChangeSubData := ChangeList[i];
      SubtitleRange := TSubtitleRange(WavDisplayer.RangeList[ChangeSubData.Index]);
      ModifiedRangeList.Add(SubtitleRange);
      // Apply subtitle change
      if (ChangeSubData.StartChanged) then
        SubtitleRange.StartTime := ChangeSubData.GetStart(IsUndo);
      if (ChangeSubData.StopChanged) then
        SubtitleRange.StopTime := ChangeSubData.GetStop(IsUndo);
      if (ChangeSubData.TextChanged) then
        SubtitleRange.Text := ChangeSubData.GetText(IsUndo);
      if (ChangeSubData.SubTimeChanged) then
        SubtitleRange.SubTime := ChangeSubData.GetSubTime(IsUndo);
      if (SubtitleRange = WavDisplayer.SelectedRange) then
      begin
        // Change selection
        if (ChangeSubData.StartChanged) then
          WavDisplayer.Selection.StartTime := SubtitleRange.StartTime;
        if (ChangeSubData.StopChanged) then
          WavDisplayer.Selection.StopTime := SubtitleRange.StopTime;
      end;
    end;
    // Sort subtitles
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  // Update indexes which have been changed during sorting
  for i := 0 to ModifiedRangeList.Count-1 do
  begin
    ChangeSubData := ChangeList[i];
    SubtitleRange := ModifiedRangeList[i];
    ChangeSubData.Index := SubtitleRange.Node.Index;
  end;
  ModifiedRangeList.Free;

  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
end;

//==============================================================================

procedure TMainForm.ClearStack(Stack : TObjectStack);
begin
  while (Stack.Count > 0) do
  begin
    Stack.Pop.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PushUndoableTask(UndoableTask : TUndoableTask);
var TopTask : TUndoableTask;
    TaskMerged : Boolean;
begin
  ClearStack(RedoStack);
  ActionRedo.Enabled := False;
  TaskMerged := False;
  // Try to merge the new task with the task on top of stack
  if (UndoStack.Count > 0) then
  begin
    TopTask := TUndoableTask(UndoStack.Peek);
    TaskMerged := TopTask.Merge(UndoableTask);
  end;
  if (not TaskMerged) then
  begin
    UndoStack.Push(UndoableTask);
  end
  else
  begin
    // Free the undoable task it has been merged with the top task
    UndoableTask.Free;
  end;
  ActionUndo.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionUndoExecute(Sender: TObject);
var UndoableTask : TUndoableTask;
begin
  if (UndoStack.Count > 0) then
  begin
    UndoableTask := TUndoableTask(UndoStack.Pop);
    UndoableTask.UndoTask;
    RedoStack.Push(UndoableTask);
    ActionUndo.Enabled := (UndoStack.Count > 0);
    ActionRedo.Enabled := (RedoStack.Count > 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionRedoExecute(Sender: TObject);
var UndoableTask : TUndoableTask;
begin
  if (RedoStack.Count > 0) then
  begin
    UndoableTask := TUndoableTask(RedoStack.Pop);
    UndoableTask.DoTask;
    UndoStack.Push(UndoableTask);
    ActionUndo.Enabled := (UndoStack.Count > 0);
    ActionRedo.Enabled := (RedoStack.Count > 0);
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.IsTimingMode : Boolean;
begin
  Result := (bttWorkingMode.Tag = 1)
end;

function TMainForm.IsNormalMode : Boolean;
begin
  Result := (bttWorkingMode.Tag = 0)
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionWAVDisplayScrollRightExecute(Sender: TObject);
begin
  WAVDisplayer.Scroll(25);
end;

procedure TMainForm.ActionWAVDisplayScrollLeftExecute(Sender: TObject);
begin
  WAVDisplayer.Scroll(-25);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideSceneChangeExecute(Sender: TObject);
begin
  WAVDisplayer.SceneChangeEnabled := not WAVDisplayer.SceneChangeEnabled;
  WAVDisplayer.UpdateView([uvfRange]);
  ConfigObject.ShowSceneChange := WAVDisplayer.SceneChangeEnabled;
  MenuItemShowHidesSeneChange.Checked := ConfigObject.ShowSceneChange;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagText(StartTag, StopTag : WideString);
begin
  if MemoSubtitleText.Focused then
    TagTextMemo(StartTag, StopTag)
  else
    TagTextLines(StartTag, StopTag);
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagTextMemo(StartTag, StopTag : WideString);
var SelStart, SelLength : Integer;
    NewText : WideString;
begin
  SelStart := MemoSubtitleText.SelStart;
  SelLength := MemoSubtitleText.SelLength;
  if (SelLength > 0) then
  begin
    // Put tag around selection
    NewText := Copy(MemoSubtitleText.Text, 1, SelStart) +
      StartTag + Copy(MemoSubtitleText.Text, SelStart + 1, SelLength) +
      StopTag + Copy(MemoSubtitleText.Text, SelStart + SelLength + 1, MaxInt);
  end
  else if (SelStart = 0) then
  begin
    // Put tag around the entire text
    NewText := StartTag + MemoSubtitleText.Text + StopTag;
  end
  else
  begin
    // Insert tag at the cursor place
    NewText := Copy(MemoSubtitleText.Text, 1, SelStart) + StartTag + StopTag +
      Copy(MemoSubtitleText.Text, SelStart + 1, MaxInt);
  end;

  TTntRichEditCustomUndo(MemoSubtitleText).SaveSelectionInfo;
  MemoSubtitleText.Text := NewText;

  if (SelLength > 0) then
  begin
    MemoSubtitleText.SelStart := SelStart;
    MemoSubtitleText.SelLength := SelLength + Length(StartTag) + Length(StopTag);
  end
  else if (SelStart = 0) then
  begin
    // Just place the cursor at the start
    MemoSubtitleText.SelStart := 0;
    MemoSubtitleText.SelLength := 0;
  end
  else
  begin
    // Place the cursor after the start tag
    MemoSubtitleText.SelStart := SelStart + Length(StartTag);
    MemoSubtitleText.SelLength := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagTextLines(StartTag, StopTag : WideString);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    TaggedText : WideString;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    TaggedText := StartTag + SubtitleRange.Text + StopTag;
    if (TaggedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := TaggedText;
      SubtitleRange.Text := TaggedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TagText(TagType : TTagType);
var Ext : WideString;
begin

  // italic: <i></i> or {\i1}{\i0}
  // bold: <b></b> or {\b1}{\b0}
  // underline: <u></u> or {\u1}{\u0}
  // font size: <font size="10"></font> {\fs10}
  // font color: <font color="#FF0000"></font> {\1c&H000000&}
  // {\a1} ...
  // {\pos  ...

  Ext := WideLowerCase(WideExtractFileExt(CurrentProject.SubtitlesFile));
  if (Ext = '.srt') then
  begin
    case TagType of
    ttItalic: TagText('<i>','</i>');
    ttBold: TagText('<b>','</b>');
    ttUnderline: TagText('<u>','</u>');
    ttColor: TagText('<font color="#FFFFFF">','</font>');
    ttSize: TagText('<font size="16">','</font>');
    end;
  end
  else if (Ext = '.ass') or (Ext = '.ssa') then
  begin
    case TagType of
    ttItalic: TagText('{\i1}','{\i0}');
    ttBold: TagText('{\b1}','{\b0}');
    ttUnderline: TagText('{\u1}','{\u0}');
    ttColor: TagText('{\1c&HFFFFFF&}','{\1c}');
    ttSize: TagText('{\fs16}','{\fs}');
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextItalicExecute(Sender: TObject);
begin
  TagText(ttItalic);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextBoldExecute(Sender: TObject);
begin
  TagText(ttBold);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextUnderlineExecute(Sender: TObject);
begin
  TagText(ttUnderline);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextColorExecute(Sender: TObject);
begin
  TagText(ttColor);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionTextSizeExecute(Sender: TObject);
begin
  TagText(ttSize);
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
    NewColor : TColor;
begin
  if GeneralJSPlugin.IsColumnBGColorized(Column) then
  begin
    GetCurrentPreviousNextSubtitles(Node, CurrentSub, PreviousSub, NextSub);
    NewColor := GeneralJSPlugin.GetColumnBgColor(Column, CurrentSub, PreviousSub, NextSub);
    TargetCanvas.Brush.Color := NewColor;
    TargetCanvas.FillRect(CellRect);
    if vtvSubsList.Selected[Node] then
    begin
      // No selection blending
      vtvSubsList.SelectionBlendFactor := 2;
    end
    else
    begin
      // default
      vtvSubsList.SelectionBlendFactor := 255;
    end;
  end
  else
  begin
    // default  
    vtvSubsList.SelectionBlendFactor := 255;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if GeneralJSPlugin.IsColumnBGColorized(Column) then
  begin
    // Force default text color
    TargetCanvas.Font.Color := clWindowText;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStripTagsExecute(Sender: TObject);
var Cursor : PVirtualNode;
    NodeData : PTreeData;
    MultiChangeTask : TUndoableMultiChangeTask;
    ChangeSubData : TChangeSubData;
    StrippedText : WideString;
    SubtitleRange : TSubtitleRange;
begin
  MultiChangeTask := TUndoableMultiChangeTask.Create;
  Cursor := vtvSubsList.GetFirstSelected;
  while (Cursor <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(Cursor);
    SubtitleRange := NodeData.Range;
    StrippedText := StripTags(SubtitleRange.Text);
    if (StrippedText <> SubtitleRange.Text) then
    begin
      ChangeSubData := TChangeSubData.Create(Cursor.Index);
      ChangeSubData.OldText := SubtitleRange.Text;
      ChangeSubData.NewText := StrippedText;
      SubtitleRange.Text := StrippedText;
      MultiChangeTask.AddData(ChangeSubData);
    end;
    Cursor := vtvSubsList.GetNextSelected(Cursor);
  end;

  if (MultiChangeTask.GetCount > 0) then
  begin
    PushUndoableTask(MultiChangeTask);
    CurrentProject.IsDirty := True;
    WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
    vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
    vtvSubsList.Repaint;
  end
  else
  begin
    FreeAndNil(MultiChangeTask);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiToggleColumn(Sender: TObject);
var Column : TVirtualTreeColumn;
    SelectedMenuItem : TTntMenuItem;
begin
  if (Sender is TTntMenuItem) then
  begin
    SelectedMenuItem := (Sender as TTntMenuItem);
    Column := (TObject(SelectedMenuItem.Tag) as TVirtualTreeColumn);
    if (coVisible in Column.Options) then
    begin
      Column.Options := Column.Options - [coVisible];
      SelectedMenuItem.Checked := False;
    end
    else
    begin
      Column.Options := Column.Options + [coVisible];
      SelectedMenuItem.Checked := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionOpenProjectFolderExecute(Sender: TObject);
var ProjectFolder : WideString;
begin
  ProjectFolder := WideExtractFilePath(CurrentProject.Filename);
  Tnt_ShellExecuteW(Handle, 'explore', PWideChar(ProjectFolder), nil,
    nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.InsertSceneChange(SrcSCArray : TIntegerDynArray);
var SCArray : TIntegerDynArray;
    SceneChangeFileName : WideString;
begin
  g_SceneChangeWrapper.Insert(SrcSCArray);
  SCArray := g_SceneChangeWrapper.GetSCArray;
  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  SceneChangeFileName := WideChangeFileExt(CurrentProject.VideoSource,
    '.scenechange');
  if WideFileExists(SceneChangeFileName) then
  begin
    SaveSceneChange(SceneChangeFileName, SCArray);
  end;
end;

function TMainForm.DeleteSceneChange(StartTimeMs, StopTimeMs : Integer) : TIntegerDynArray;
var SCArray :TIntegerDynArray;
    SceneChangeFileName : WideString;
begin
  Result := g_SceneChangeWrapper.Delete(StartTimeMs, StopTimeMs);
  SCArray := g_SceneChangeWrapper.GetSCArray;
  WAVDisplayer.SetSceneChangeList(SCArray);
  WAVDisplayer.UpdateView([uvfRange]);
  SceneChangeFileName := WideChangeFileExt(CurrentProject.VideoSource,
    '.scenechange');
  if WideFileExists(SceneChangeFileName) then
  begin
    SaveSceneChange(SceneChangeFileName, SCArray);
  end;
end;

procedure TMainForm.ActionDeleteSceneChangeExecute(Sender: TObject);
var UndoableDeleteSceneChange : TUndoableDeleteSceneChange;
begin
  if (WAVDisplayer.SelectionIsEmpty) then
    Exit;
  if (not g_SceneChangeWrapper.Contains(WAVDisplayer.Selection.StartTime,
      WAVDisplayer.Selection.StopTime)) then
    Exit;

  UndoableDeleteSceneChange := TUndoableDeleteSceneChange.Create;
  UndoableDeleteSceneChange.SetData(WAVDisplayer.Selection.StartTime,
    WAVDisplayer.Selection.StopTime);
  UndoableDeleteSceneChange.DoTask;
  PushUndoableTask(UndoableDeleteSceneChange);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionReloadExecute(Sender: TObject);
var Index : Integer;
begin
  if CheckSubtitlesAreSaved then
  begin    
    if Assigned(vtvSubsList.FocusedNode) then
    begin
      Index := vtvSubsList.FocusedNode.Index;
    end
    else
    begin
      Index := -1;
    end;

    ShowStatusBarMessage('Reloading subtitles...');
    LoadSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
    SelectNodeAtIndex(Index);
    ClearStack(RedoStack);
    ClearStack(UndoStack);
    ActionUndo.Enabled := False;
    ActionRedo.Enabled := False;
    CurrentProject.IsDirty := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeWithPreviousExecute(Sender: TObject);
var Node : PVirtualNode;
begin
  Node := vtvSubsList.GetFirstSelected;
  if not Assigned(Node) then Exit;
  Node := vtvSubsList.GetPrevious(Node);
  if not Assigned(Node) then Exit;
  Merge(mtNormal, mrWithPrevious);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeWithNextExecute(Sender: TObject);
var Node : PVirtualNode;
begin
  Node := vtvSubsList.GetFirstSelected;
  if not Assigned(Node) then Exit;
  Node := vtvSubsList.GetNext(Node);
  if not Assigned(Node) then Exit;
  Merge(mtNormal, mrWithNext);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeDialogExecute(Sender: TObject);
begin
  if (vtvSubsList.SelectedCount > 1) then
  begin
    Merge(mtDialog, mrSelected);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionMergeOnOneLineExecute(Sender: TObject);
begin
  if (vtvSubsList.SelectedCount > 1) then
  begin
    Merge(mtOneLine, mrSelected);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnJavascriptAction(Sender : TObject);
var JsAction : TJavascriptAction;
begin
  if (Sender is TJavascriptAction) then
  begin
    JsAction := (Sender as TJavascriptAction);

    UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;

    GeneralJSPlugin.Eval(JsAction.JSObjectName + '.onExecute();');

    if (UndoableMultiChangeTask.GetCount > 0) then
    begin
      PushUndoableTask(UndoableMultiChangeTask);
      // Do not free the task, it's on the stack now
      UndoableMultiChangeTask := nil;
      UpdateAfterJSChange;
    end
    else
    begin
      FreeAndNil(UndoableMultiChangeTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateAfterJSChange;
var SubtitleRange : TSubtitleRange;
    NodeData : PTreeData;
    SelStart, SelLength : Integer;
begin
  CallJSOnSubtitleModification;
  
  // Update MemoSubtitleText if changed
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    SubtitleRange := NodeData.Range;
    if (MemoSubtitleText.Text <> SubtitleRange.Text) then
    begin
      SelStart := MemoSubtitleText.SelStart;
      SelLength := MemoSubtitleText.SelLength;

      vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);

      MemoSubtitleText.SelStart := SelStart;
      MemoSubtitleText.SelLength := SelLength;
    end;
  end;
  // Update subtitle list and wavdisplay
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
  WAVDisplayer1SelectionChange(WAVDisplayer);
end;

//------------------------------------------------------------------------------

procedure TMainForm.RegisterJavascriptAction(AName, ACaption, ADefaultShortcut : WideString);
var JSAction : TJavascriptAction;
    NewItem : TTntMenuItem;
begin
  JSAction := TJavascriptAction.Create(TntActionList1);
  JSAction.Caption := '[js] ' + ACaption;
  JSAction.Name := AName;
  JSAction.JSObjectName := AName;
  JSAction.ShortCut := TextToShortCut(ADefaultShortcut);
  JSAction.Tag := 1;
  JSAction.OnExecute := OnJavascriptAction;
  JSAction.ActionList := TntActionList1;

  NewItem := TTntMenuItem.Create(Self);
  NewItem.Action := JSAction;
  NewItem.Caption := Tnt_WideStringReplace(NewItem.Caption, '[js] ', '', [rfReplaceAll]);
  MenuItemJSTools.Add(NewItem);
end;

function TMainForm.GetSubCount : Integer;
begin
  Result := vtvSubsList.RootNodeCount;
end;

function TMainForm.GetFirst : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetFirst;
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetNext(SubtitleRange : TSubtitleRange) : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetNext(SubtitleRange.Node);
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetPrevious(SubtitleRange : TSubtitleRange) : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetPrevious(SubtitleRange.Node);
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetSelectedCount : Integer;
begin
  Result := vtvSubsList.SelectedCount;
end;

function TMainForm.GetFirstSelected : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetFirstSelected;
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetNextSelected(SubtitleRange : TSubtitleRange) : TSubtitleRange;
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  Node := vtvSubsList.GetNextSelected(SubtitleRange.Node);
  if Assigned(Node) then
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    Result := NodeData.Range;
  end else begin
    Result := nil;
  end;
end;

function TMainForm.GetAt(Index : Integer) : TSubtitleRange;
begin
  if (Index >= 0) and (Index < WAVDisplayer.RangeList.Count) then
    Result := TSubtitleRange(WAVDisplayer.RangeList[Index])
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadPresetFile(Filename : WideString);
var IniFile : TIniFile;
    FilenameOnly : WideString;
    I : Integer;
begin
  if WideFileExists(Filename) then
  begin
    IniFile := TIniFile.Create(FileName);
    try
      ConfigObject.LoadIni(IniFile, True);
      SetShortcut(IsTimingMode);
      ApplyMouseSettings;
      ApplyFontSettings;
      ApplyMiscSettings;
    finally
      IniFile.Free;
    end;
    // Check the menu item
    FilenameOnly := WideExtractFileName(Filename);
    for i:= 0 to MenuItemLoadPresets.Count-1 do
    begin
      MenuItemLoadPresets[i].Checked := (MenuItemLoadPresets[i].Hint = FilenameOnly);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoadPresetsExecute(Sender: TObject);
begin
  OpenDialogPresets.Filter := 'Presets file|*.INI' + '|' + 'All files (*.*)|*.*';
  if not OpenDialogPresets.Execute then
    Exit;
  LoadPresetFile(OpenDialogPresets.FileName);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionCopyExecute(Sender: TObject);
var actCtrl : TWinControl;
begin
  actCtrl := Screen.ActiveControl;
  if (actCtrl is TCustomEdit) then
    TCustomEdit(actCtrl).CopyToClipboard
  else if (actCtrl is TVirtualStringTree) then
  begin
    CopyVTVToClipboard(TVirtualStringTree(actCtrl));
  end;
end;


//------------------------------------------------------------------------------

procedure TMainForm.ActionCopyUpdate(Sender: TObject);
var actCtrl : TWinControl;
begin
  actCtrl := Screen.ActiveControl;
  ActionCopy.Enabled := ((actCtrl is TCustomEdit) and (TCustomEdit(actCtrl).SelLength > 0))
    or ((actCtrl is TVirtualStringTree) and (TVirtualStringTree(actCtrl).SelectedCount > 0));
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnLoadPresetMenuItemClick(Sender: TObject);
var MenuItem : TTntMenuItem;
begin
  if (Sender is TTntMenuItem) then
  begin
    MenuItem := Sender as TTntMenuItem;
    LoadPresetFile(g_PresetsPath + MenuItem.Hint);
    CurrentProject.Presets := MenuItem.Hint;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FillPresetsMenu;
var FFindFileAttrs : Integer;
    FSearchRec : TSearchRecW;
    FileList : TTntStringList;
    NewMenuItem : TTntMenuItem;
    i : Integer;
begin
  FFindFileAttrs := faAnyFile;
  FSearchRec.FindHandle := INVALID_HANDLE_VALUE;
  FileList := TTntStringList.Create;

  if (WideFindFirst(g_PresetsPath + '*.ini', FFindFileAttrs, FSearchRec) <> 0) then
    WideFindClose(FSearchRec)
  else
  begin
    while (FSearchRec.FindHandle <> INVALID_HANDLE_VALUE) do
    begin
      FileList.Add(FSearchRec.Name);
      if (WideFindNext(FSearchRec) <> 0) then
        WideFindClose(FSearchRec);
    end;
  end;

  FileList.Sort;
  
  for i:=0 to FileList.Count-1 do
  begin
    NewMenuItem := TTntMenuItem.Create(MenuItemLoadPresets);
    NewMenuItem.AutoHotkeys := maManual;
    NewMenuItem.Caption := FileList[i];
    NewMenuItem.Hint := FileList[i];
    NewMenuItem.OnClick := OnLoadPresetMenuItemClick;
    MenuItemLoadPresets.Add(NewMenuItem);
  end;
  // Add special "Clear list" menu
  NewMenuItem := TTntMenuItem.Create(MenuItemLoadPresets);
  NewMenuItem.Caption := '-';
  MenuItemLoadPresets.Add(NewMenuItem);

  NewMenuItem := TTntMenuItem.Create(MenuItemLoadPresets);
  NewMenuItem.Action := ActionLoadPresets;
  NewMenuItem.Caption := 'Browse...';
  MenuItemLoadPresets.Add(NewMenuItem);

  FileList.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SetSelection(Start, Stop : Integer);
begin
  WAVDisplayer.ClearSelection;
  WAVDisplayer.Selection.StartTime := Start;
  WAVDisplayer.Selection.StopTime := Stop;
  WAVDisplayer.ZoomCenteredOn(Start + ((Stop - Start) div 2), 20000);
  WAVDisplayer1SelectionChange(WAVDisplayer);
  WAVDisplayer.UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowSilentZonesExecute(Sender: TObject);
var IniFile : TIniFile;
begin
  if (SilentZoneForm = nil) then
  begin
    SilentZoneForm := TSilentZoneForm.Create(Self, WAVDisplayer);
    IniFile := TIniFile.Create(GetIniFilename);
    LoadFormPosition(IniFile, SilentZoneForm);
    IniFile.Free;
  end;
  SilentZoneForm.Visible := True;
  SilentZoneForm.UpdateData;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnLoadDictionnary(Sender: TObject);
//var CM : ICursorManager;
begin
//  CM := TCursorManager.Create(crHourGlass);
  FSpellChecker.Initialize((Sender as TBgThreadTask).Param);
end;

procedure TMainForm.OnLoadDictionnaryTerminate(Sender: TObject);
var Event1, Event2 : TNotifyEvent;
    i : Integer;
begin
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
  
  // Enable dictionnary loading menus
  for i := 0 to SubMenuItemSpellcheck.Count-1 do
  begin
    Event1 := SubMenuItemSpellcheck.Items[i].OnClick;
    Event2 := Self.OnSpellcheckLanguageMenuItemClick;
    if (@Event1 = @Event2) then
    begin
      SubMenuItemSpellcheck.Items[i].Enabled := True;
    end;
  end;
end;

procedure TMainForm.LoadDict(Idx : Integer);
var i : Integer;
    BgThreadTask : TBgThreadTask;
    Event1, Event2 : TNotifyEvent;
begin
  // First remove previous dictionnary from memory
  FSpellChecker.Cleanup;

  // Update display
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;

  // Disable dictionnary loading menus
  for i := 0 to SubMenuItemSpellcheck.Count-1 do
  begin
    Event1 := SubMenuItemSpellcheck.Items[i].OnClick;
    Event2 := Self.OnSpellcheckLanguageMenuItemClick;
    if (@Event1 = @Event2) then
    begin
      SubMenuItemSpellcheck.Items[i].Enabled := False;
      if (SubMenuItemSpellcheck.Items[i].Tag = Idx) then
      begin
        SubMenuItemSpellcheck.Items[i].Checked := True;
      end;
    end;
  end;

  // Load new dictionnary in background
  BgThreadTask := TBgThreadTask.Create(True, tpLowest);
  BgThreadTask.FreeOnTerminate := True;
  BgThreadTask.OnExecute := OnLoadDictionnary;
  BgThreadTask.OnTerminate := OnLoadDictionnaryTerminate;
  BgThreadTask.Param := Idx;
  BgThreadTask.Resume;
end;

procedure TMainForm.OnSpellcheckLanguageMenuItemClick(Sender: TObject);
begin
  LoadDict( (Sender as TMenuItem).Tag );
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLiveSpellCheckExecute(Sender: TObject);
begin
  MenuItemLiveSpellcheck.Checked := not MenuItemLiveSpellcheck.Checked;
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubPopupMenuPopup(Sender: TObject);
var P : TPoint;
    AWord : WideString;
    Suggestions: TTntStrings;
    i, j: Integer;
    mi : TTntMenuItem;
    WordInfo : TWordInfo;
begin
  // Remove all old item before undo if any
  while (MemoSubPopupMenu.Items[0] <> pmiMemoSubUndo) do
  begin
    MemoSubPopupMenu.Items.Delete(0);
  end;

  if (not MenuItemLiveSpellcheck.Checked) or (not FSpellChecker.IsInitialized) then
    Exit;

  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  AWord := Copy(MemoSubtitleText.Text, WordInfo.Position, WordInfo.Length);

  if not FSpellChecker.Spell(AWord) then
  begin
    j := 0;
    mi := nil;
    Suggestions := TTntStringList.Create;
    FSpellChecker.Suggest(AWord, Suggestions);
    for i := 0 to Suggestions.Count-1 do
    begin
      mi := TTntMenuItem.Create(Self);
      mi.AutoHotkeys := maManual;
      mi.Caption := Suggestions[i];
      mi.Hint := Suggestions[i];
      mi.OnClick := OnSpellcheckSuggestionMenuItemClick;
      MemoSubPopupMenu.Items.Insert(i, mi);
      Inc(j);
    end;
    if (Suggestions.Count = 0) then
    begin
      mi := TTntMenuItem.Create(Self);
      mi.Caption := '(No suggestions)';
      mi.Enabled := False;
      MemoSubPopupMenu.Items.Insert(0, mi);
      Inc(j);
    end;
    Suggestions.Free;
    // Add to dictionary
    mi := TTntMenuItem.Create(Self);
    mi.Caption := '=> Add to dictionary';
    mi.OnClick := OnSpellcheckAddToDictionaryMenuItemClick;
    MemoSubPopupMenu.Items.Insert(j, mi);
    Inc(j);
    // Ignore
    mi := TTntMenuItem.Create(Self);
    mi.Caption := '=> Ignore';
    mi.OnClick := OnSpellcheckIgnoreMenuItemClick;
    MemoSubPopupMenu.Items.Insert(j, mi);
    Inc(j);
    // -
    mi := TTntMenuItem.Create(Self);
    mi.Caption := cLineCaption;
    MemoSubPopupMenu.Items.Insert(j, mi);
    Inc(j);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSpellcheckSuggestionMenuItemClick(Sender: TObject);
var TextBegin, TextEnd : WideString;
    P : TPoint;
    WordInfo : TWordInfo;
begin
  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  TextBegin := Copy(MemoSubtitleText.Text, 1, WordInfo.Position - 1);
  TextEnd := Copy(MemoSubtitleText.Text, WordInfo.Position + WordInfo.Length, MaxInt);
  MemoSubtitleText.Text := TextBegin + (Sender as TTntMenuItem).Hint + TextEnd;
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSpellcheckAddToDictionaryMenuItemClick(Sender: TObject);
var P : TPoint;
    WordInfo : TWordInfo;
    AWord : WideString;
begin
  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  AWord := Copy(MemoSubtitleText.Text, WordInfo.Position, WordInfo.Length);
  FSpellChecker.Add(AWord);
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSpellcheckIgnoreMenuItemClick(Sender: TObject);
var P : TPoint;
    WordInfo : TWordInfo;
    AWord : WideString;
begin
  P := MemoSubPopupMenu.PopupPoint;
  if not GetWordAt(MemoSubtitleText, P.X, P.Y, WordInfo) then
    Exit;
  AWord := Copy(MemoSubtitleText.Text, WordInfo.Position, WordInfo.Length);
  FSpellChecker.Ignore(AWord);
  if (MemoSubtitleText.Tag = 1) then
  begin
    TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemGetMoreDictionariesClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    PAnsiChar('http://wiki.services.openoffice.org/wiki/Dictionaries'),
    '', '', SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSpellCheckExecute(Sender: TObject);
var SpellUndoableTask : TUndoableMultiChangeTask;
    IniFile : TIniFile;
begin
  if (not FSpellChecker.IsInitialized) then
  begin
    Exit;
  end;

  if (SpellCheckForm = nil) then
  begin
    SpellCheckForm := TSpellCheckForm.Create(Self);
    IniFile := TIniFile.Create(GetIniFilename);
    LoadFormPosition(IniFile, SpellCheckForm);
    IniFile.Free;    
  end;
  SpellCheckForm.ShowModal;

  SpellUndoableTask := SpellCheckForm.GetUndoableTask;
  if Assigned(SpellUndoableTask) then
  begin
    if (SpellUndoableTask.GetCount > 0) then
    begin
      PushUndoableTask(SpellUndoableTask);
      CurrentProject.IsDirty := True;
      WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
      vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
      vtvSubsList.Repaint;
      SpellCheckForm.Reset;
    end
    else
    begin
      FreeAndNil(SpellUndoableTask);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TMainForm.GetSpellChecker : THunspellChecker;
begin
  Result := FSpellChecker;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSpellCheckUpdate(Sender: TObject);
begin
  ActionSpellCheck.Enabled := FSpellChecker.IsInitialized;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionInsertSceneChangeExecute(Sender: TObject);
var UndoableInsertSceneChange : TUndoableInsertSceneChange;
    CursorPos : Integer;
begin
  CursorPos := WAVDisplayer.GetCursorPos;
  if g_SceneChangeWrapper.Contains(CursorPos,CursorPos) then
    Exit;

  UndoableInsertSceneChange := TUndoableInsertSceneChange.Create;
  UndoableInsertSceneChange.SetData(CursorPos);
  UndoableInsertSceneChange.DoTask;
  PushUndoableTask(UndoableInsertSceneChange);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPasteExecute(Sender: TObject);
var actCtrl : TWinControl;
    SubList : TList;
    UndoableMultiAddTask : TUndoableMultiAddTask;
begin
  actCtrl := Screen.ActiveControl;
  if (actCtrl is TCustomEdit) then
  begin
    TCustomEdit(actCtrl).PasteFromClipboard;
  end
  else if (actCtrl is TVirtualStringTree) then
  begin
    SubList := TList.Create;
    PasteClipboard(SubList, SubRangeFactory);
    if (SubList.Count > 0) then
    begin
      // TODO : maybe check for duplicate sub when pasting ?
      UndoableMultiAddTask := TUndoableMultiAddTask.Create;
      UndoableMultiAddTask.SetData(SubList);
      UndoableMultiAddTask.DoTask;
      PushUndoableTask(UndoableMultiAddTask);
      CurrentProject.IsDirty := True;
    end;
    SubList.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPasteUpdate(Sender: TObject);
var actCtrl : TWinControl;
begin
  actCtrl := Screen.ActiveControl;
  ActionPaste.Enabled := ((actCtrl is TCustomEdit) and TntClipboard.HasFormat(CF_TEXT))
    or ((actCtrl is TVirtualStringTree) and
      (TntClipboard.HasFormat(VSSClipBoardFORMAT) or TntClipboard.HasFormat(CF_TEXT)));
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPasteAtCursorExecute(Sender: TObject);
var SubList : TList;
    UndoableMultiAddTask : TUndoableMultiAddTask;
    SubRange : TSubtitleRange;
    Delay, i : Integer;
begin
  SubList := TList.Create;
  PasteClipboard(SubList, SubRangeFactory);
  if (SubList.Count > 0) then
  begin
    // Delay each subtitle so the start time of the first subtitle match
    // the cursor position
    SubRange := SubList[0];
    Delay := SubRange.StartTime - WAVDisplayer.GetCursorPos;
    for i:=0 to SubList.Count-1 do
    begin
      SubRange := SubList[i];
      SubRange.StartTime := SubRange.StartTime - Delay;
      SubRange.StopTime := SubRange.StopTime - Delay;
    end;
    // TODO : maybe check for duplicate sub when pasting ?
    UndoableMultiAddTask := TUndoableMultiAddTask.Create;
    UndoableMultiAddTask.SetData(SubList);
    UndoableMultiAddTask.DoTask;
    PushUndoableTask(UndoableMultiAddTask);
    CurrentProject.IsDirty := True;
  end;
  SubList.Free;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------

{

TODO UNDO:
undo text pipe text modification (modifiy, clear, load)

undo for style?

TODO : display karaoke sylables in wavdisplay

// -----------------------------------------------------------------------------

ScriptLog('todo = ' + VSSCore);
ScriptLog('VSSCore = ' + VSSCore.abc);

TODO : test wav extraction with divx installed
DivX Demux - 85516702-9C45-4A9C-861B-BC4492D355DC - C:\WINDOWS\system32\DivXMedia.ax ( 0.0.0.28)

}


