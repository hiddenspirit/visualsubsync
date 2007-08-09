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
// SSA/ASS loding code contribution by Mirage (2005)
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
  UndoableTaskUnit, UndoableSubTaskUnit;

type
  TTreeData = record
    Range: TSubtitleRange;
  end;
  PTreeData = ^TTreeData;

  // -----

  TPlayingModeType = (pmtAll, pmtSelection, pmtSelectionStart, pmtSelectionEnd);

  TMainForm = class(TTntForm)
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
    EditCopy1: TTntEditCopy;
    EditPaste1: TTntEditPaste;
    EditSelectAll1: TTntEditSelectAll;
    EditDelete1: TTntEditDelete;
    Undo1: TTntMenuItem;
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
    TntStatusBar1: TTntStatusBar;
    ActionUndo: TTntAction;
    ActionRedo: TTntAction;
    Undo2: TTntMenuItem;
    Redo1: TTntMenuItem;
    N17: TTntMenuItem;
    ActionWAVDisplayScrollRight: TTntAction;
    ActionWAVDisplayScrollLeft: TTntAction;
    procedure FormCreate(Sender: TObject);

    procedure WAVDisplayer1CursorChange(Sender: TObject);
    procedure WAVDisplayer1PlayCursorChange(Sender: TObject);
    procedure WAVDisplayer1SelectionChange(Sender: TObject);
    procedure WAVDisplayer1ViewChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRange(Sender: TObject; SelectedRange : TRange; IsDynamic : Boolean);
    procedure WAVDisplayer1SelectedRangeChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRangeChanged(Sender: TObject;
      OldStart, OldStop : Integer);
    procedure WAVDisplayer1AutoScrollChange(Sender: TObject);
    procedure WAVDisplayPopup_DeleteRange(Sender: TObject);
    procedure WAVDisplayer1StartPlaying(Sender: TObject);
    procedure WAVDisplayer1KaraokeChanged(Sender: TObject;
        Range : TRange);
    procedure WAVDisplayer1SelectedKaraokeRange(Sender: TObject;
        Range : TRange);
    procedure WAVDisplayer1CustomDrawRange(Sender: TObject; ACanvas: TCanvas; Range : TRange; Rect : TRect);
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
    procedure WAVDisplayPopup_SplitAtCursor(Sender: TObject);
    procedure pmiSubListMergeClick(Sender: TObject);
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
    procedure FormResize(Sender: TObject);
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
    procedure TntStatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure PanelVideoDblClick(Sender: TObject);
    procedure ActionWAVDisplayScrollRightExecute(Sender: TObject);
    procedure ActionWAVDisplayScrollLeftExecute(Sender: TObject);
   
  private
    { Private declarations }
    WAVDisplayer : TWAVDisplayer;
    SubRangeFactory : TSubtitleRangeFactory;
    CurrentProject : TVSSProject;
    PeakCreationProgressForm : TPeakCreationProgressForm;
    AudioOnlyRenderer : TDShowRenderer;
    VideoRenderer : TDShowRenderer;
    SearchNode : PVirtualNode;
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

    procedure InitVTV;
    procedure EnableControl(Enable : Boolean);
    procedure EnableStyleControls(Enable : Boolean);
    procedure LoadSubtitles(Filename: WideString; var IsUTF8 : Boolean);
    procedure SaveSubtitles(Filename: WideString; InUTF8 : Boolean; BackupOnly : Boolean);
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

    procedure SaveSubtitlesAsSRT(Filename: WideString; InUTF8 : Boolean);
    procedure SaveSubtitlesAsSSA(Filename: WideString; InUTF8 : Boolean);
    procedure SaveSubtitlesAsASS(Filename: WideString; InUTF8 : Boolean);
    procedure SaveSubtitlesAsCUE(Filename: WideString);
    procedure SaveSubtitlesAsTXT(Filename: WideString; InUTF8 : Boolean);
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

    function DetectCharsetDataLoss : Boolean;

    // General JS plugins
    procedure CallJSOnSubtitleModification;
    procedure CallJSOnSelectedSubtitle;
    procedure GetCurrentPreviousNextSubtitles(var CurrentSub, PreviousSub, NextSub : TSubtitleRange);
    procedure UpdateStatusBar;
    procedure OnJsSetStatusBarText(const Msg : WideString);

    // Undo/Redo stuff
    procedure PushUndoableTask(UndoableTask : TUndoableTask);
    procedure ClearStack(Stack : TObjectStack);       

    procedure OnUndo(Sender: TTntRichEdit; UndoTask : TUndoableTask);
    function SimpleSetSubtitleStartTime(Index, NewTime : Integer) : Integer;
    function SimpleSetSubtitleStopTime(Index, NewTime : Integer) : Integer;
    function SimpleSetSubtitleText(Index : Integer; NewText : WideString) : WideString;
  public
    { Public declarations }
    procedure ShowStatusBarMessage(const Text : WideString; const Duration : Integer = 4000);
    procedure SelectNode(Node : PVirtualNode);
    procedure SelectNodeFromTimestamps(Start, Stop : Integer);
    procedure StartStopServer(Stop : Boolean = False);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ShowPreferences(TabSheet : TTntTabSheet);
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
    procedure RestoreSubtitles(List : TList);
    function SetSubtitleTime(Index, NewStartTime, NewStopTime : Integer) : Integer;
    procedure SplitSubtitle(Index, SplitTime : Integer);
    function MergeSubtitles(var FIndexes : array of Integer) : TSubtitleRange;
    procedure FocusNode(Node : PVirtualNode; WAVDisplaySelect : Boolean); overload;
    procedure FocusNodeAt(Index : Integer); overload;
    procedure ClearWAVSelection;
    procedure ProcessMultiChangeSub(ChangeList : TList; IsUndo : Boolean);    
  end;

const
  StartEndPlayingDuration: Integer = 500;
  EnableExperimentalKaraoke: Boolean = True;
  UTF8BOM : array[0..2] of BYTE = ($EF,$BB,$BF);

var
  MainForm: TMainForm;

implementation

uses ActiveX, Math, StrUtils, FindFormUnit, AboutFormUnit,
  ErrorReportFormUnit, SuggestionFormUnit, GotoFormUnit,
  Types, VerticalScalingFormUnit, TntSysUtils, TntWindows,
  LogWindowFormUnit, CursorManager, FileCtrl, WAVFileUnit, PageProcessorUnit,
  tom_TLB, RichEdit, StyleFormUnit, SSAParserUnit, TntWideStrings, TntClasses,
  TntIniFiles, TntGraphics, TntSystem, TntRichEditCustomUndoUnit;

{$R *.dfm}

//==============================================================================

// TODO : Configurable backup directory, backup n last version

// TODO : Test project properties dialog
// TODO : When opening a project fail show the project properties page for modification
// TODO : Add some checking in project unit ok button

// TODO : Web : number of suggestion for current sub, and current suggestions
// TODO : Web : We need real gzip compression, deflate support is b0rked in IE

// TODO : Split at cursor for submemo
// TODO : Replace dialog
// TODO : Check HD space before extraction

// TODO : Rework project handling which is a bit messy ATM
// TODO : Separate subtitle file loading/saving, stats according to format into a new classes


//------------------------------------------------------------------------------

// TODO : auto clear karaoke timing that are out of subtitle bound ???
// TODO : update documentation (timing button)

{
lovechange:
XXX> 1er: Pour le "log text pipe", afin de charger un transcript, il prend
XXX> pas par défaut le srt, on peut le faire en affichant tout les
XXX> fichiers, tu pourrait peut-etre le rajouter comme extension par
XXX> défauts.
-
> 2eme: Ce serait sympa que tu puisse rajouter des boutons dans
> l'interface principale pour les balise "italique", "placement de
> texte" (8 balise en faites puisque en bas centré est déja par défaut.
> Je pense qu'il y a la place juste en dessus du graph audio et
> pré-visualisation vidéo.
}

//==============================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var i : integer;
    GeneralJSFilename : WideString;
    CustomMemo : TTntRichEditCustomUndo;
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
  CustomMemo.DisableWindowsUndo;
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
  ConfigObject.SetDefaultHotKeys(TntActionList1);

  ServerRootDir := ExtractFilePath(Application.ExeName);
  ServerRootDir := IncludeTrailingPathDelimiter(ServerRootDir) + 'web\';

  vtvSubsList.Constraints.MinWidth := 600;
  PanelMiddle.Constraints.MinHeight := 125;
  PanelBottom.Constraints.MinHeight := 60;
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

  GeneralJSPlugin := TSimpleJavascriptWrapper.Create;
  GeneralJSFilename := g_PluginPath + 'general\general_plugin.js';
  if WideFileExists(GeneralJSFilename) then
  begin
    if GeneralJSPlugin.LoadScript(GeneralJSFilename) then
    begin
      GeneralJSPlugin.OnJSPluginError := LogForm.LogMsg;
      GeneralJSPlugin.OnJSPluginSetStatusBarText := OnJsSetStatusBarText;
    end
    else
      LogForm.SilentLogMsg('Can''t load ' + GeneralJSFilename);
  end
  else
    LogForm.SilentLogMsg('Can''t find ' + GeneralJSFilename);

  StatusBarPrimaryText := '';
  StatusBarSecondaryText := '';
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StartStopServer(True);
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

procedure TMainForm.SaveSettings;
var IniFile : TIniFile;
    IniFilename : WideString;
begin
  IniFilename := GetIniFilename;
  try
    IniFile := TIniFile.Create(IniFilename);
    MRUList.SaveIni(IniFile, 'MRUList');
    ConfigObject.SaveIni(IniFile);
    SaveFormPosition(IniFile,MainForm);
    SaveFormPosition(IniFile,ErrorReportForm);
    SaveFormPosition(IniFile,SuggestionForm);
    SaveFormPosition(IniFile,DetachedVideoForm);
    SaveFormPosition(IniFile,LogForm);

    IniFile.WriteInteger('Windows', 'MainForm_PanelTop_Height', PanelTop.Height);
    IniFile.WriteInteger('Windows', 'MainForm_PanelBottom_Height', PanelBottom.Height);

    IniFile.WriteInteger('General', 'Volume', tbVolume.Position);

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
  IniFilename := GetIniFilename;
  IniFile := TIniFile.Create(IniFilename);
  MRUList.LoadIni(IniFile, 'MRUList');
  ConfigObject.LoadIni(IniFile);
  SetShortcut(False);
  ApplyMouseSettings;
  ApplyFontSettings;
  LoadFormPosition(IniFile,MainForm);
  LoadFormPosition(IniFile,ErrorReportForm);
  LoadFormPosition(IniFile,SuggestionForm);
  LoadFormPosition(IniFile,DetachedVideoForm);
  LoadFormPosition(IniFile,LogForm);

  Show;

  PanelTop.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelTop_Height',
    PanelTop.Height);
  PanelBottom.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelBottom_Height',
    PanelBottom.Height);
  TntStatusBar1.Top := Maxint;

  tbVolume.Position := IniFile.ReadInteger('General', 'Volume', 100);

  IniFile.Free;
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
      [toFullRowSelect, toMultiSelect {,toExtendedFocus}];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines,toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
    Header.Options := Header.Options + [hoVisible, hoAutoResize] - [hoDrag];
    Header.Style := hsFlatButtons;
    Column := Header.Columns.Add;
    Column.Text := '#';
    Column.Width := 50;
    Column.MinWidth := 50;
    Column.Options := Column.Options - [coAllowClick,coDraggable];
    Column := Header.Columns.Add;
    Column.Text := 'Start';
    Column.Width := 100;
    Column.MinWidth := 100;
    Column.Options := Column.Options - [coAllowClick,coDraggable];
    Column := Header.Columns.Add;
    Column.Text := 'Stop';
    Column.Width := 100;
    Column.MinWidth := 100;
    Column.Options := Column.Options - [coAllowClick,coDraggable];

    Column := Header.Columns.Add;
    Column.Text := 'Style';
    Column.Width := 100;
    Column.MinWidth := 60;
    Column.Options := Column.Options - [coAllowClick,coDraggable];

    Column := Header.Columns.Add;
    Column.Text := 'Text';
    Column.Options := Column.Options - [coAllowClick,coDraggable];
    Column.Width := 500;
    Column.MinWidth := 100;

    //FocusedColumn := 2;

    OnGetText := vtvSubsListGetText;
  end;

end;

//------------------------------------------------------------------------------

procedure TMainForm.OnRecentMenuItemClick(Sender : TObject);
var MenuItem : TMenuItem;
begin
  // TODO : Fix MRU for unicode
  MenuItem := Sender as TMenuItem;
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

  if (NewText <> TntStatusBar1.Panels[1].Text) then
  begin
    TntStatusBar1.Panels[1].Text := NewText;
    TntStatusBar1.Repaint;
//    TCustomStatusBar.UpdatePanel
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
  ActionPlay.Enabled := Enable;
  ActionStop.Enabled := Enable;
  ActionLoop.Enabled := Enable;
  ActionNextSub.Enabled := Enable;
  ActionPreviousSub.Enabled := Enable;
  ActionPlaySelStart.Enabled := Enable;
  ActionPlaySelEnd.Enabled := Enable;
  ActionPlayToEnd.Enabled := Enable;
  ActionPlay1sBefore.Enabled := Enable;
  ActionPause.Enabled := Enable;

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

  ActionShowHideVideo.Enabled := Enable;
  ActionDetachVideo.Enabled := Enable;
  ActionProjectProperties.Enabled := Enable;
  ActionSaveAs.Enabled := Enable;
  ActionExportToSSA.Enabled := Enable;
  ActionExportToWAV.Enabled := Enable;
  ActionShowStartFrame.Enabled := Enable;
  ActionShowStopFrame.Enabled := Enable;
  ActionShowFrameAtCursor.Enabled := Enable;

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

  ActionSetPlaybackRate60.Enabled := Enable;
  ActionSetPlaybackRate70.Enabled := Enable;
  ActionSetPlaybackRate80.Enabled := Enable;
  ActionSetPlaybackRate90.Enabled := Enable;
  ActionSetPlaybackRate100.Enabled := Enable;

  ActionLoopSelStart.Enabled := Enable;
  ActionLoopSelEnd.Enabled := Enable;

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
    if not MemoSubtitleText.Focused then
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
  Sender: TObject; OldStart, OldStop : Integer);
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

procedure TMainForm.WAVDisplayPopup_SplitAtCursor(Sender: TObject);
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
      WAVDisplayer.GetCursorPos);
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
  StyleColumn := vtvSubsList.Header.Columns.Items[3];
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
      StyleForm.ConfigureMode(sfmASS);
    end
    else if (Ext = '.ssa') then
    begin
      EnableStyle := True;
      LoadASS(Filename, IsUTF8, True);
      StyleColumn.Options := StyleColumn.Options + [coVisible];
      StyleForm.ConfigureMode(sfmSSA);
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
  S: WideString;
  i, lineIndex : integer;
  Start, Stop : Integer;
  NextStart, NextStop : Integer;
  Text : WideString;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  AutoCorrectedFile : Boolean;
  Source : TTntStringList;
begin
  Result := False;
  if not WideFileExists(Filename) then
  begin
    WAVDisplayer.RangeList.Clear;
    vtvSubsList.Clear;
    Exit;
  end;

  SubtitleFileHeader := '';
  SubtitleFileFooter := '';

  WAVDisplayer.RangeList.Clear;

  Source := TTntStringList.Create;
  Source.LoadFromFile(Filename);
  IsUTF8 := (Source.LastFileCharSet <> csAnsi);

  AutoCorrectedFile := False;
  lineIndex := 0;

  // Add a blank line because TTntStringList is eating the last line
  // if it's blank. This is safe because we will trim the text later anyway.
  Source.Add('');

  // Skip lines until a timestamps line
  while (lineIndex < Source.Count) do
  begin
    S := Source[lineIndex];
    Inc(lineIndex);
    if IsTimeStampsLine(S, Start, Stop) then
      Break;
  end;

  while (lineIndex < Source.Count) do
  begin
    // Copy text until a timestamps line
    Text := '';
    while (lineIndex < Source.Count) do
    begin
      S := Source[lineIndex];
      Inc(lineIndex);
      if IsTimeStampsLine(S, NextStart, NextStop) then
        Break;
      Text := Text + Trim(S) + CRLF;
    end;
    Text := TrimRight(Text);
    if (Start <> -1) and (Stop <> -1) then
    begin
      // Auto fix timestamp if this subtitle stop time is equal
      // to next subtitle start time
      if (Stop = NextStart) then
      begin
        AutoCorrectedFile := True;
        Dec(Stop);
      end;
      // Remove the index line if any
      i := RPos(CRLF, Text);
      if ((i > 0) and (StrToIntDef(Copy(Text, i+2, MaxInt), -1) <> -1) and (lineIndex < Source.Count)) then
      begin
        Delete(Text, i, MaxInt);
      end;
      NewRange := SubRangeFactory.CreateRangeSS(Start,Stop);
      TSubtitleRange(NewRange).Text := Trim(Text);

      if (EnableExperimentalKaraoke = True) then
        NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);

      WAVDisplayer.RangeList.AddAtEnd(NewRange);
    end;
    Start := NextStart;
    Stop := NextStop;
  end;
  Source.Free;
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
  if AutoCorrectedFile then
    CurrentProject.IsDirty := True;
  Result := True;
end;

//------------------------------------------------------------------------------

function TMainForm.LoadASS(Filename: WideString; var IsUTF8 : Boolean; isSSA : Boolean) : Boolean;
var
  i, Start, Stop : Integer;
  Text : WideString;
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
    WAVDisplayer.RangeList.Clear;
    vtvSubsList.Clear;
    StyleForm.ClearAll;
    StyleForm.AddDefaultStyle;
    Exit;
  end;

  WAVDisplayer.RangeList.Clear;

  ssaParser := TSSAParser.Create;
  ssaParser.Parse(Filename);
  IsUTF8 := ssaParser.GetIsUTF8;
  SubtitleFileHeader := ssaParser.GetHeaderLines;
  SubtitleFileFooter := ssaParser.GetFooterLines;

  StyleForm.LoadStylesFromParser(ssaParser);

  for i := 0 to ssaParser.GetDialoguesCount-1 do
  begin
    Start := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'Start'));
    Stop := TimeStringToMS_SSA(ssaParser.GetDialogueValueAsString(i, 'End'));
    if (Start <> -1) and (Stop <> -1) then
    begin
      NewRange := SubRangeFactory.CreateRangeSS(Start, Stop);
      Text := ssaParser.GetDialogueValueAsString(i, 'Text');
      TSubtitleRange(NewRange).Text := Tnt_WideStringReplace(Text, '\N', CRLF,[rfReplaceAll]);
      if ssaParser.GetIsASS then
        TSubtitleRange(NewRange).Layer := ssaParser.GetDialogueValueAsString(i, 'Layer')
      else
        TSubtitleRange(NewRange).Layer := ssaParser.GetDialogueValueAsString(i, 'Marked');
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
begin
  NodeData := Sender.GetNodeData(Node);

  case Column of
    -1: CellText := ''; // -1 if columns are hidden
    0: CellText := IntToStr(Node.Index+1);
    1: CellText := TimeMsToString(NodeData.Range.StartTime);
    2: CellText := TimeMsToString(NodeData.Range.StopTime);
    3: CellText := NodeData.Range.Style;
    4: CellText := StringConvertCRLFToPipe(NodeData.Range.Text);
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

procedure TagHighlight(RichEdit : TTntRichEdit; TagIndex : Integer);
var savSelStart, savSelLength : Integer;
    i, j : Integer;
    WordArray : WideStringArray2;
begin
  RichEdit.Tag := 0;
  savSelStart := RichEdit.SelStart;
  savSelLength := RichEdit.SelLength;

  TagSplit(RichEdit.Text, WordArray);

  RichEdit.Lines.BeginUpdate;
  j := 0;
  for i:=0 to Length(WordArray)-1 do
  begin
    RichEdit.SelStart := j;
    RichEdit.SelLength := Length(WordArray[i]);
    if (i = (TagIndex*2)+1) then
      RichEdit.SelAttributes.Color := clRed
    else if (i mod 2) = 0 then
      RichEdit.SelAttributes.Color := clWindowText
    else
      RichEdit.SelAttributes.Color := clGrayText;
    Inc(j, Length(WordArray[i]));
  end;
  RichEdit.SelStart := savSelStart;
  RichEdit.SelLength := savSelLength;
  RichEdit.Lines.EndUpdate;

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
begin
  UpdateLinesCounter;
end;

//------------------------------------------------------------------------------

function MyLineLength(const s : WideString) : Integer;
var i : Integer;
    wsnotag : WideString;
begin
  Result := 0;
  wsnotag := StripTags(s);
  // Fix win98 b0rk (#13 #10 are included in the richedit text line)
  for i:=1 to Length(wsnotag) do
  begin
    if (wsnotag[i] = #13) or (wsnotag[i] = #10) then
      Break;
    Inc(Result);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateLinesCounter;
var s : string;
    i, CharCount, TotalCharCount, TotalCharCountWithCRLF, CPS : Integer;
    LineNo, LineStart, ColNo : Integer;
    NodeData: PTreeData;
    SubDuration : Integer;
begin
  s := '';
  TotalCharCount := 0;
  TotalCharCountWithCRLF := 0;

  for i := MemoSubtitleText.Perform(EM_GETFIRSTVISIBLELINE,0,0) to MemoSubtitleText.Lines.Count-1 do
  begin
    CharCount := MyLineLength(MemoSubtitleText.Lines.Strings[i]);
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

  TntStatusBar1.Panels[0].Text := s;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitles(Filename: WideString; InUTF8 : Boolean; BackupOnly : Boolean);
var Ext : WideString;
    BackupDstFilename : WideString;
begin
  if (not BackupOnly) and WideFileExists(Filename) and ConfigObject.EnableBackup then
  begin
    CheckBackupDirectory;
    BackupDstFilename := g_BackupDirectory + WideChangeFileExt(WideExtractFileName(Filename), '.bak');
    WideCopyFile(Filename, BackupDstFilename, False);
  end;

  Ext := WideLowerCase(WideExtractFileExt(Filename));
  if (Ext = '.srt') then
    SaveSubtitlesAsSRT(Filename, InUTF8)
  else if (Ext = '.ass') then
    SaveSubtitlesAsASS(Filename, InUTF8)
  else if (Ext = '.ssa') then
    SaveSubtitlesAsSSA(Filename, InUTF8)
  else if (Ext = '.cue') then
    SaveSubtitlesAsCUE(Filename)
  else if (Ext = '.txt') then
    SaveSubtitlesAsTXT(Filename, InUTF8);

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

procedure TMainForm.LoadProject(Filename : WideString);
var
  CM : ICursorManager;
  LoadWAVOK : Boolean;
begin
  if (not WideFileExists(Filename)) then
  begin
    MessageBoxW(Handle, PWideChar(WideFormat('The project file %s doesn''t exist.', [Filename])),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    Exit;
  end;

  ActionClose.Execute;

  g_WebRWSynchro.BeginWrite;
  CM := TCursorManager.Create(crHourGlass);
  try
    CurrentProject.LoadFromINIFile(Filename);
    ShowStatusBarMessage('Loading WAV form...');
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
        end
        else
        begin
          MessageBoxW(Handle, PWideChar(WideFormat('Can''t open peak file : %s',
            [CurrentProject.PeakFile])),
            PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
          // TODO : Show project page as a new project but with pre-filled data
          Exit;
        end;
      end;
    end
    else
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
            PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
          // TODO : Show project page as a new project but with pre-filled data
          Exit;
        end;
      end;
    end;
    // TODO : report invalid wav file error
    WAVDisplayer.Enabled := LoadWAVOK;

    ShowStatusBarMessage('Loading subtitles...');
    LoadSubtitles(CurrentProject.SubtitlesFile,CurrentProject.IsUTF8);

    ShowStatusBarMessage('Loading audio file...');
    if not AudioOnlyRenderer.Open(CurrentProject.WAVFile) then
    begin
      if AudioOnlyRenderer.Open(CurrentProject.VideoSource) then
      begin
        AudioOnlyRenderer.KillVideo;
      end;
    end;

    ShowStatusBarMessage('Loading video file...');
    if VideoRenderer.Open(CurrentProject.VideoSource) then
    begin
      if CurrentProject.DetachedVideo <> MenuItemDetachVideoWindow.Checked then
        ActionDetachVideoExecute(nil);
      if CurrentProject.ShowVideo <> ShowingVideo then
        ActionShowHideVideoExecute(nil);
      UpdateVideoRendererWindow;
      VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
    end;

    if ShowingVideo then
      WAVDisplayer.SetRenderer(VideoRenderer)
    else
      WAVDisplayer.SetRenderer(AudioOnlyRenderer);

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
  MRUList.AddFile(CurrentProject.Filename);
  ShowStatusBarMessage('Project loaded.');
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveProject(Project : TVSSProject; SilentSave : Boolean);
var
  ProjectFileIni : TTntIniFile;
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
  ProjectForm.ConfigureInNewProjectMode;
  if (ProjectForm.ShowModal = mrOK) then
  begin
    // Create the project file
    ProjectFileIni := TTntIniFile.Create(ProjectForm.EditProjectFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','VideoSource',ProjectForm.EditVideoFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','WAVFile',ProjectForm.EditWAVFilename.Text);
    ProjectFileIni.WriteString('VisualSubsync','PeakFile',ProjectForm.EditPeakFilename.Text);
    ProjectFileIni.WriteInteger('VisualSubsync','WAVMode',Ord(ProjectForm.GetWAVMode));
    ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile',ProjectForm.EditSubtitleFilename.Text);
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
    ShowStatusBarMessage('Video file is not open...');
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

procedure TMainForm.ActionFindExecute(Sender: TObject);
begin
  if (FindForm.ShowModal = mrOK) then
  begin
    SearchNode := nil;
    SearchPos := 1;
    ActionFindNext.Execute;
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
    SaveSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8, False);
    VideoRenderer.SetSubtitleFilename(CurrentProject.SubtitlesFile);
  end;
  SaveProject(CurrentProject, False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
var InputExt : WideString;
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
      1 : TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, '.srt');
      2 : TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, '.ssa');
      3 : TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, '.ass');
      4 : TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, '.cue');
      5 : TntSaveDialog1.FileName := WideChangeFileExt(TntSaveDialog1.FileName, '.txt');
    end;
    SaveSubtitles(TntSaveDialog1.FileName, CurrentProject.IsUTF8, False);
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
          CurrentProject.SubtitlesFile := ProjectForm.EditSubtitleFilename.Text;
          if not WideFileExists(CurrentProject.SubtitlesFile) then
            ActionSaveExecute(nil);
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

      if (VideoHasChanged or SubtitleFileHasChanged) then
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
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFindNextExecute(Sender: TObject);
var NodeData : PTreeData;
    FoundAt : Integer;
    SearchTypes : TSearchTypes;
begin
  if Length(FindForm.GetFindWord) = 0 then
    Exit;
  
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
  while (SearchNode <> nil) do
  begin
    NodeData := vtvSubsList.GetNodeData(SearchNode);
    if FindForm.MatchCase then
      SearchPos := PosEx(FindForm.GetFindWord, NodeData.Range.Text,SearchPos)
    else
      SearchPos := PosEx(WideUpperCase(FindForm.GetFindWord), WideUpperCase(NodeData.Range.Text), SearchPos);
    if (SearchPos > 0) then
    begin
      vtvSubsList.ScrollIntoView(SearchNode,True);
      vtvSubsList.FocusedNode := SearchNode;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[SearchNode] := True;
      if FindForm.MatchCase then SearchTypes := [stMatchCase] else SearchTypes := [];
      FoundAt := MemoSubtitleText.FindText(FindForm.GetFindWord,
        SearchPos-1,
        Length(MemoSubtitleText.Text),
        SearchTypes);
      if (FoundAt <> -1) then
      begin
        MemoSubtitleText.SetFocus;
        MemoSubtitleText.SelStart := FoundAt;
        MemoSubtitleText.SelLength := Length(FindForm.GetFindWord);
      end;
      Inc(SearchPos, Length(FindForm.GetFindWord));
      Exit;
    end;
    SearchNode := vtvSubsList.GetNext(SearchNode);
    SearchPos := 1;
  end;
  ShowStatusBarMessage('No more items.');
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

procedure TMainForm.pmiSubListMergeClick(Sender: TObject);
var i, Idx : integer;
    Node : PVirtualNode;
    NodeData: PTreeData;
    AccuText : WideString;
    LastStopTime : Integer;
    DeleteList : TList;

    UndoableMergeTask : TUndoableMergeTask;
begin
  UndoableMergeTask := TUndoableMergeTask.Create;
  UndoableMergeTask.SetCapacity(vtvSubsList.SelectedCount);
  Node := vtvSubsList.GetFirstSelected;
  while Assigned(Node) do
  begin
    UndoableMergeTask.AddSubtitleIndex(Node.Index);
    Node := vtvSubsList.GetNextSelected(Node);
  end;

  UndoableMergeTask.DoTask;
  PushUndoableTask(UndoableMergeTask);

  {


  AccuText := '';
  LastStopTime := 0;

  g_WebRWSynchro.BeginWrite;

  try

    // First pass
    // - maybe check if selected item are consecutive ??? TODO
    // - accumulate text
    // - get last stop time
    // - make the list of nodes to delete
    DeleteList := TList.Create;
    Node := vtvSubsList.GetFirstSelected;
    i := 0;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      LastStopTime := Max(LastStopTime, NodeData.Range.StopTime);
      if (i > 0) then
        DeleteList.Add(Node);
      AccuText := AccuText + CRLF + NodeData.Range.Text;
      Node := vtvSubsList.GetNextSelected(Node);
      Inc(i);
    end;

    // - set new settings to first node
    Node := vtvSubsList.GetFirstSelected;
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeData.Range.Text := Trim(AccuText);
    NodeData.Range.StopTime := LastStopTime;
    vtvSubsList.FocusedNode := nil;

    // Finally delete nodes
    for i := 0 to DeleteList.Count-1 do
    begin
      Node := DeleteList[i];
      NodeData := vtvSubsList.GetNodeData(Node);
      ErrorReportForm.DeleteError(NodeData.Range);
      Idx := WAVDisplayer.RangeList.IndexOf(NodeData.Range);
      WAVDisplayer.DeleteRangeAtIdx(Idx,False);
      vtvSubsList.DeleteNode(Node);
    end;
    DeleteList.Free;

    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  
  // Make sure to update display
  Node := vtvSubsList.GetFirstSelected;
  vtvSubsList.FocusedNode := Node;
  if Assigned(WAVDisplayer.SelectedRange) then
    WAVDisplayer.SelectedRange := nil;
  WAVDisplayer.ClearSelection;
  WAVDisplayer.UpdateView([uvfRange,uvfSelection]);
  vtvSubsList.Repaint;  
  }
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
      SaveSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8, False);
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
    if (Idx <> -1) then
    begin
      if (Idx <> OldAutoScrollIdx) then
      begin
        SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
        Node := SubRange.Node;
        vtvSubsList.ScrollIntoView(Node,True);
        // Needed if subtitle node is already the focused node
        vtvSubsList.FocusedNode := nil;
        vtvSubsList.FocusedNode := Node;
        vtvSubsList.ClearSelection;
        vtvSubsList.Selected[Node] := True;
      end;
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
begin
  OldSwapState := ConfigObject.SwapSubtitlesList;
  PreferencesForm.LoadConfig(ConfigObject);
  if TabSheet <> nil then
    PreferencesForm.TntPageControl1.ActivePage := TabSheet;
  if (PreferencesForm.ShowModal = mrOk) then
  begin
    PreferencesForm.SaveConfig(ConfigObject);
    if Assigned(Server) then
      Server.EnableCompression := ConfigObject.EnableCompression;
    if(ConfigObject.SwapSubtitlesList <> OldSwapState) then
      Self.SwapSubList;
    MemoSubtitleText.Enabled := MemoLinesCounter.Enabled and
      ((not ConfigObject.DisableSubtitleEdition) or IsNormalMode);
    SetShortcut(PreferencesForm.GetMode);
    ApplyMouseSettings;
    ApplyAutoBackupSettings;
    ApplyFontSettings;
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

  // Adjust status bar panel size for people using bigger fonts
  TntStatusBar1.Canvas.Font.Assign(TntStatusBar1.Font);
  TntStatusBar1.Panels[0].Width :=  TntStatusBar1.Canvas.TextWidth(
    'Line: 99, Column: 999 | Total: 999, Char/s: 99');
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
  ShowPreferences(PreferencesForm.tsErrorChecking);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ActionCloseExecute(Sender: TObject);
begin
  if CheckSubtitlesAreSaved then
  begin
    WAVDisplayer.Stop;
    chkAutoScrollSub.Checked := False;
    TimerAutoScrollSub.Enabled := False;

    g_WebRWSynchro.BeginWrite;
    try
      SuggestionForm.Clear;
      ErrorReportForm.Clear;
      vtvSubsList.Clear;

      WAVDisplayer.RangeList.Clear;
      WAVDisplayer.Enabled := False;
      WAVDisplayer.Close;
      WAVDisplayer.Repaint;
      WAVDisplayer.VerticalScaling := 100;

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
    TntStatusBar1.Panels[0].Text := '';
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := True;
    MemoSubtitleText.Text := '';
    TTntRichEditCustomUndo(MemoSubtitleText).UndoDisabled := False;

    EnableControl(False);
    cbStyles.Clear;
    EnableStyleControls(False);

    AudioOnlyRenderer.Close;
    VideoRenderer.Close;

    ClearStack(RedoStack);
    ClearStack(UndoStack);    
    ActionUndo.Enabled := False;
    ActionRedo.Enabled := False;
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
  FS : TFileStream;
  BOM : array[0..3] of BYTE;
  Line: string;
  LastRange, NewRange : TRange;
  StartTime : Integer;
  Node: PVirtualNode;
  NodeData: PTreeData;
  IsUTF8 : Boolean;
  HaveNewSub : Boolean;
begin
  g_WebRWSynchro.BeginWrite;
  try
    HaveNewSub := False;
    TntOpenDialog1.Filter := 'Text file|*.TXT' + '|' + 'All files (*.*)|*.*';
    if TntOpenDialog1.Execute then
    begin
      FS := TFileStream.Create(TntOpenDialog1.FileName,fmOpenRead);
      ZeroMemory(@BOM[0],Length(BOM));
      FS.Read(BOM,4);
      if (BOM[0] = $EF) and (BOM[1] = $BB) and (BOM[2] = $BF) then
      begin
        // UTF8
        IsUTF8 := True;
        FS.Seek(3,soFromBeginning);
      end
      else
      begin
        IsUTF8 := False;
        FS.Seek(0,soFromBeginning);
      end;

      // Get start time
      if (WAVDisplayer.RangeList.Count > 0) then
      begin
        LastRange := WAVDisplayer.RangeList[WAVDisplayer.RangeList.Count-1];
        StartTime := LastRange.StopTime + 2000;
      end
      else
        StartTime := 99 * 60 * 60 * 1000;

      while (not ReadLineStream(FS,Line)) do
      begin
        if IsUTF8 then
          Line := UTF8Decode(Text);
        NewRange := SubRangeFactory.CreateRangeSS(StartTime,StartTime+1000);
        Inc(StartTime,2000);
        TSubtitleRange(NewRange).Text := Line;
        WAVDisplayer.RangeList.AddAtEnd(NewRange);

        Node := vtvSubsList.AddChild(nil);
        NodeData := vtvSubsList.GetNodeData(Node);
        NodeData.Range := TSubtitleRange(NewRange);
        TSubtitleRange(NewRange).Node := Node;
        HaveNewSub := True;
      end;
      FS.Free;
    end;
    if HaveNewSub then
      CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
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
    TntStatusBar1.Top := MaxInt; // make sure status bar stay at bottom
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

procedure TMainForm.FormResize(Sender: TObject);
begin
  // TODO : add some autoresizing maybe
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
var NewNode : PVirtualNode;
    NodeData : PTreeData;
begin
  vtvSubsList.ScrollIntoView(Node, True);
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
  
  if IsNormalMode then
    MemoSubtitleText.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleTimeExecute(Sender: TObject);
var NodeData : PTreeData;
    NextNodeToFocus, FocusedNode : PVirtualNode;
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


    {
    NextNodeToFocus := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    try
      g_WebRWSynchro.BeginWrite;
      NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

      NodeData.Range.StartTime := WAVDisplayer.Selection.StartTime;
      NodeData.Range.StopTime := WAVDisplayer.Selection.StopTime;

      FullSortTreeAndSubList; // lazy me :) but well it's fast enough
      CurrentProject.IsDirty := True;
    finally
      g_WebRWSynchro.EndWrite;
    end;

    if Assigned(NextNodeToFocus) then
    begin
      vtvSubsList.FocusedNode := NextNodeToFocus;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[NextNodeToFocus] := True;
    end;

    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsList.Repaint;
    }
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
    bStart, bStop : Integer;
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

      R.SubTime[Idx] := R.SubTime[Idx] + Offset;
      Constrain(R.SubTime[Idx], bStart, bStop);
      WAVDisplayer.Selection.StartTime := R.SubTime[Idx];
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(WAVDisplayer,R);
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
    bStart, bStop : Integer;
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

      R.SubTime[Idx] := R.SubTime[Idx] + Offset;
      Constrain(R.SubTime[Idx], bStart, bStop);
      WAVDisplayer.Selection.StopTime := R.SubTime[Idx];
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(WAVDisplayer,R);
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
    if IsNormalMode then
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
  if (PreferencesForm.GetMode = True) then
  begin
    // Timing mode
    if ConfigObject.MouseEnableSSATimingMode then
      WAVDisplayer.SelMode := smSSA
    else
      WAVDisplayer.SelMode := smCoolEdit;
  end;
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
    SaveSubtitles(BackupDstFilename, CurrentProject.IsUTF8, True);
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
  LogForm.SilentLogMsg('');
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
    Msg := Format('%d  error(s) found.', [ErrorReportForm.vtvErrorList.TotalCount]);
  end
  else
    Msg := 'No error found.';
  Msg := Msg + Format(' (in %d ms)',[ExecTime]);
  ErrorReportForm.TntStatusBar1.Panels[0].Text := Msg;
  LogForm.SilentLogMsg(Msg);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideLogsExecute(Sender: TObject);
begin
  LogForm.Visible := not LogForm.Visible;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionFixErrorMainExecute(Sender: TObject);
var JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    i : integer;
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

        JPlugin.FixError(SubRangeCurrent, SubRangePrevious, SubRangeNext);
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
    // Save old start time if it has not been set yet
    if (not ChangeSubData.StartChanged) then
      ChangeSubData.FOldStartTime := SubtitleRange.StartTime;
    ChangeSubData.FNewStartTime := NewValue;
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
    // Save old stop time if it has not been set yet
    if (not ChangeSubData.StopChanged) then
      ChangeSubData.FOldStopTime := SubtitleRange.StopTime;
    ChangeSubData.FNewStopTime := NewValue;
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
    // Save old stop time if it has not been set yet
    if (not ChangeSubData.TextChanged) then
      ChangeSubData.FOldText := SubtitleRange.Text;
    ChangeSubData.FNewText := NewValue;
  end;
  CurrentProject.IsDirty := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FixErrorInList(ErrorList : TList);
var NodeData : PErrorTreeData;
    JSPEnum : TJavaScriptPluginEnumerator;
    JPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
    SubRangeCurrent, SubRangePrevious, SubRangeNext : TSubtitleRange;
    i,j : integer;
begin
  UndoableMultiChangeTask := TUndoableMultiChangeTask.Create;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;

  g_WebRWSynchro.BeginWrite;
  try
    for j:=0 to ErrorList.Count-1 do
    begin
      NodeData := ErrorList[j];
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
        end;
        FreeAndNil(JPlugin);
      end;
    end;
  finally
    g_WebRWSynchro.EndWrite;
    JSPEnum.Free;
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
          AddSubtitle(ToggleStartSubtitleTime, WAVDisplayer.GetPlayCursorPos, '', False);
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
procedure WhiteSpaceSplit(const Text : WideString; var WordArray : WideStringArray);
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

procedure TMainForm.pmiCreateKaraokeClick(Sender: TObject);
var Node : PVirtualNode;
    NodeData: PTreeData;
    WordArray : WideStringArray;
    i, j, total : Integer;
    s : WideString;
    tpl : Single;
begin
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      tpl := (NodeData.Range.StopTime - NodeData.Range.StartTime) /
        KaraLen(StringReplace(NodeData.Range.Text, '/', '', [rfReplaceAll]));
      WhiteSpaceSplit(NodeData.Range.Text, WordArray);

      NodeData.Range.ClearSubTimes;

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
            NodeData.Range.AddSubTime(NodeData.Range.StartTime + (total*10));
          s := s + WideFormat('{\k%d}%s', [j, WordArray[i]]);
        end;
      end;

      // total should match duration (but we work in ms :] ?)

      NodeData.Range.Text := s;

      Node := vtvSubsList.GetNextSelected(Node);
    end;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiClearKaraokeClick(Sender: TObject);
var
  Node : PVirtualNode;
  NodeData: PTreeData;
  WordArray : WideStringArray;
  TimeArray : IntegerArray;
  CleanedText : WideString;
  i : Integer;
begin
  g_WebRWSynchro.BeginWrite;
  try
    Node := vtvSubsList.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      CleanedText := '';
      KaraSplit(NodeData.Range.Text, WordArray, TimeArray);
      for i:=0 to Length(WordArray)-1 do
        CleanedText := CleanedText + WordArray[i];
      NodeData.Range.Text := CleanedText;
      NodeData.Range.ClearSubTimes;
      Node := vtvSubsList.GetNextSelected(Node);
    end;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfRange]);
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayer1KaraokeChanged(Sender: TObject; Range : TRange);
var Sub : TSubtitleRange;
    WordArray : WideStringArray;
    TimeArray : IntegerArray;
    i : Integer;
    AccuTime : Integer;
    NewText : WideString;
    savSelStart, savSelLength : Integer;
begin
  g_WebRWSynchro.BeginWrite;
  try
    Sub := Range as TSubtitleRange;
    KaraSplit2(Sub.Text, WordArray, TimeArray);
    if (Length(Sub.SubTime) > 0) and
       (Length(Sub.SubTime) = (Length(TimeArray) - 1)) then
    begin
      AccuTime := Sub.StartTime;
      for i:=0 to Length(Sub.SubTime)-1 do
      begin
        TimeArray[i] := Round((Sub.SubTime[i] - AccuTime) / 10);
        AccuTime := AccuTime + (TimeArray[i] * 10);
      end;
      TimeArray[Length(TimeArray)-1] := Round((Sub.StopTime - AccuTime) / 10);

      NewText := WordArray[0];
      for i:=0 to Length(TimeArray)-1 do
      begin
        NewText := NewText + IntToStr(TimeArray[i]) + WordArray[i+1];
      end;
      Sub.Text := NewText;
    end;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;
  savSelStart := MemoSubtitleText.SelStart;
  savSelLength := MemoSubtitleText.SelLength;
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  vtvSubsList.Repaint;
  MemoSubtitleText.SelStart := savSelStart;
  MemoSubtitleText.SelLength := savSelLength;
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
begin
  Result := False;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    ansiCharString := SubRange.Text;
    if (ansiCharString <> SubRange.Text) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSRT(Filename: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + CRLF;
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
    WriteStringLnStream(IntToStr(i+1), FS);
    WriteStringLnStream(TimeMsToString(SubRange.StartTime,',') + ' --> ' +
      TimeMsToString(SubRange.StopTime,','), FS);
    if InUTF8 then
      WriteStringLnStream(UTF8Encode(Subrange.Text), FS)
    else
      WriteStringLnStream(Subrange.Text, FS);
    WriteStringLnStream('', FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSSA(Filename: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    s : WideString;
    style : TSSAStyle;
    
    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + CRLF;
      Stream.Write(s[1],Length(s));
    end;
begin
  // Write BOM if any
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

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
    WriteStringLnStream(Trim(SubtitleFileHeader), FS);
  end;

  // Write Styles
  WriteStringLnStream('', FS);
  WriteStringLnStream('[v4 Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding', FS);
  for i:=0 to StyleForm.GetCount-1 do
  begin
    style := StyleForm.GetStyleAt(i);
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
      [SubRange.Layer, TimeMsToSSAString(SubRange.StartTime),
       TimeMsToSSAString(SubRange.StopTime), SubRange.Style, SubRange.Actor,
       Subrange.LeftMarg, Subrange.RightMarg, SubRange.VertMarg, Subrange.Effect]);
    if InUTF8 then
      s := s + UTF8Encode(Tnt_WideStringReplace(Subrange.Text,CRLF,'\N',[rfReplaceAll]))
    else
      s := s + Tnt_WideStringReplace(Subrange.Text,CRLF,'\N',[rfReplaceAll]);
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

procedure TMainForm.SaveSubtitlesAsASS(Filename: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    s : WideString;
    style : TSSAStyle;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;
begin
  // Write BOM if any
  FS := TTntFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

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
    WriteStringLnStream(Trim(SubtitleFileHeader), FS);
  end;

  // Write Styles
  WriteStringLnStream('', FS);
  WriteStringLnStream('[v4+ Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic,  Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding', FS);
  for i:=0 to StyleForm.GetCount-1 do
  begin
    style := StyleForm.GetStyleAt(i);
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
      s := s + UTF8Encode(Tnt_WideStringReplace(Subrange.Text,CRLF,'\N',[rfReplaceAll]))
    else
      s := s + Tnt_WideStringReplace(Subrange.Text,CRLF,'\N',[rfReplaceAll]);
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

procedure WideCanvasDrawText(Canvas: TCanvas; Rect: TRect; const Text: WideString);
var
  Options: Cardinal;
begin
  with TAccessCanvas(Canvas) do begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := DT_END_ELLIPSIS or DT_NOPREFIX or DT_WORDBREAK;
    Windows.DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Options);
    Changed;
  end;
end;

procedure TMainForm.WAVDisplayer1CustomDrawRange(Sender: TObject;
  ACanvas: TCanvas; Range : TRange; Rect : TRect);
begin
  // TODO : karaoke support
  InflateRect(Rect, -5, -5);
  if (Rect.Right - Rect.Left) > 25 then
  begin
    ACanvas.Font.Assign(MemoSubtitleText.Font);
    ACanvas.Font.Color := ACanvas.Pen.Color;
    WideCanvasDrawText(ACanvas, Rect, TSubtitleRange(Range).Text);
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
        Node := vtvSubsList.GetFirst;
        while (Node <> nil) do
        begin
          NodeData := vtvSubsList.GetNodeData(Node);
          Filename := Format('%s%6.6d.wav', [SelectedDir, Node.Index+1]);
          FS := TFileStream.Create(Filename, fmCreate);
          WAVFile.ExtractToStream(NodeData.Range.StartTime,
            NodeData.Range.StopTime, FS);
          FS.Free;
          Node := vtvSubsList.GetNext(Node);
        end;
        PP := TPageProcessor.Create;
        DummyEnvVars := TStringList.Create;
        FS := TFileStream.Create(SelectedDir + 'index_wav.html', fmCreate);
        PP.ProcessPage(ServerRootDir + 'index_wav_export.shtml', FS, DummyEnvVars);
        PP.Free;
        FS.Free;
        DummyEnvVars.Free;
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
    i, s, t : Integer;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    t := 100;
    s := NodeData.Range.StopTime - (Length(NodeData.Range.SubTime) * t);
    if (s > NodeData.Range.StartTime) then
    begin
      try
        g_WebRWSynchro.BeginWrite;
        for i:=0 to Length(NodeData.Range.SubTime)-1 do
        begin
          NodeData.Range.SubTime[i] := s + (t * i);
        end;
      finally
        g_WebRWSynchro.EndWrite;
      end;
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(nil, NodeData.Range);
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
    Text : WideString;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
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
    Text := Subrange.Text;
    // Remove line breaks
    Text := Tnt_WideStringReplace(Text, ' ' + CRLF + ' ', ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, ' ' + CRLF, ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, CRLF + ' ', ' ', [rfReplaceAll]);
    Text := Tnt_WideStringReplace(Text, CRLF, ' ', [rfReplaceAll]);

    WriteStringLnStream(Format('  TRACK %2.2d AUDIO', [i*2+2]), FS);
    WriteStringLnStream(Format('    INDEX 01 %s', [TimeMsToCUE(SubRange.StartTime)]), FS);
    WriteStringLnStream(Format('    TITLE "%s"', [Text]), FS);
    
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
    Exit;

  if WAVDisplayer.IsPlaying then
    NewStopTime := WAVDisplayer.GetPlayCursorPos
  else
    NewStopTime := WAVDisplayer.GetCursorPos;

  NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
  NewDuration := NewStopTime - NodeData.Range.StartTime;

  if (NewDuration <= 0) then
    Exit;

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
    AdvanceToNextSubtitleAfterFocus;

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
  for i := 0 to StyleForm.GetCount-1 do
  begin
    style := StyleForm.GetStyleAt(i);
    cbStyles.AddItem(style.name, nil);
  end;
  UpdateStylesComboboxFromSelection;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStylesExecute(Sender: TObject);
begin
  // Try to pre sselect current subtitle style
  StyleForm.PreSelect(cbStyles.Text);
  StyleForm.ShowModal;
  if StyleForm.HaveStylesChanged then
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
  PreferencesForm.SetMode(IsTimingMode);
  SetShortcut(PreferencesForm.GetMode);
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
  MemoSubtitleText.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetEditorFocusAndSelectExecute(Sender: TObject);
begin
  MemoSubtitleText.SetFocus;
  MemoSubtitleText.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateSubtitleForPreview(ForceUpdate : Boolean);
var TmpDstFilename : WideString;
    StartTime : Cardinal;
begin
  // TODO : Clean old files in temp directory
  
  if DisableVideoUpdatePreview or (Trim(CurrentProject.SubtitlesFile) = '') then
    Exit;

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
      SaveSubtitles(TmpDstFilename, CurrentProject.IsUTF8, True);
      VideoRenderer.SetSubtitleFilename(TmpDstFilename);
      if VideoRenderer.IsPaused then
      begin
        VideoRenderer.UpdateImage;
      end;
      VideoPreviewNeedSubtitleUpdate := False;
      LogForm.SilentLogMsg('Save took ' + IntToStr(GetTickCount - StartTime) +' ms');
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

procedure TMainForm.GetCurrentPreviousNextSubtitles(var CurrentSub,
  PreviousSub, NextSub : TSubtitleRange);
var Node : PVirtualNode;
    NodeData : PTreeData;
begin
  CurrentSub := nil;
  PreviousSub := nil;
  NextSub := nil;

  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    CurrentSub := NodeData.Range;

    Node := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    if Assigned(Node) then
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
      NextSub := NodeData.Range;
    end;

    Node := vtvSubsList.GetPrevious(vtvSubsList.FocusedNode);
    if Assigned(Node) then
    begin
      NodeData := vtvSubsList.GetNodeData(Node);
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
    GetCurrentPreviousNextSubtitles(CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifySubtitleModification(CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CallJSOnSelectedSubtitle;
var CurrentSub, PreviousSub, NextSub : TSubtitleRange;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    GetCurrentPreviousNextSubtitles(CurrentSub, PreviousSub, NextSub);
    GeneralJSPlugin.NotifySelectionModification(CurrentSub, PreviousSub, NextSub);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnJsSetStatusBarText(const Msg : WideString);
begin
  SetStatusBarPrimaryText(Msg);
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntStatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var TntPanel : TTntStatusPanel;
begin
  // Use an owner drawn panel to avoid the 127 bytes text limit in
  // Windows version inferior to Windows Vista
  TntPanel := Panel as TTntStatusPanel;
  WideCanvasTextRect(StatusBar.Canvas, Rect, Rect.Left + 2, Rect.Top, TntPanel.Text);
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

procedure TMainForm.SaveSubtitlesAsTXT(Filename: WideString; InUTF8 : Boolean);
var i : integer;
    Subrange : TSubtitleRange;
    FS : TTntFileStream;
    Text : WideString;

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + CRLF;
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
      WriteStringLnStream(Text, FS);
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
        WAVDisplayer1KaraokeChanged(WAVDisplayer, WavDisplayer.SelectedRange);
      end;
    end;
    // Sort subtitles
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  // Update indexes which have been changed during sorting
  for i := 0 to DelayedRangeList.Count-1 do
  begin
    SubtitleRange := TSubtitleRange(DelayedRangeList[i]);
    Indexes[i] := SubtitleRange.Node.Index;
  end;
  DelayedRangeList.Free;  

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.InvalidateColumn(1);
  vtvSubsList.InvalidateColumn(2);
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

procedure TMainForm.RestoreSubtitles(List : TList);
var i, idx : integer;
    Range : TSubtitleRange;
begin
  for i := List.Count-1 downto 0 do
  begin
    Range := List[i];
    AddSubtitle(Range);
  end;
  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SplitSubtitle(Index, SplitTime : Integer);
var SubRange, NewSubRange : TSubtitleRange;
    NewNode : PVirtualNode;
    NodeData : PTreeData;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  NewSubRange := TSubtitleRange(SubRangeFactory.CreateRange);
  NewSubRange.Assign(SubRange);
  SubRange.StopTime := SplitTime - 1;
  NewSubRange.StartTime := SplitTime;
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
var Range : TRange;
begin
  try
    Range := WAVDisplayer.RangeList[Index];
    Range.StartTime := NewStartTime;
    Range.StopTime := NewStopTime;
    FullSortTreeAndSubList;
    CurrentProject.IsDirty := True;
  finally
    g_WebRWSynchro.EndWrite;
  end;

  if WAVDisplayer.SelectedRange = Range then
  begin
    WAVDisplayer.Selection.StartTime := NewStartTime;
    WAVDisplayer.Selection.StopTime := NewStopTime;
  end;

  WAVDisplayer.UpdateView([uvfRange]);
  vtvSubsList.Repaint;

  Result := TSubtitleRange(Range).Node.Index;
end;

// -----------------------------------------------------------------------------

function TMainForm.MergeSubtitles(var FIndexes : array of Integer) : TSubtitleRange;
var i, idx : integer;
    AccuText : WideString;
    LastStopTime : Integer;
    FirstRange, SubRange : TSubtitleRange;
begin
  idx := FIndexes[Low(FIndexes)];
  FirstRange := TSubtitleRange(WAVDisplayer.RangeList[idx]);
  AccuText := FirstRange.Text;
  LastStopTime := FirstRange.StopTime;
  for i := Low(FIndexes) + 1 to High(FIndexes) do
  begin
    idx := FIndexes[i];
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[idx]);
    AccuText := AccuText + CRLF + SubRange.Text;
    LastStopTime := Max(LastStopTime, SubRange.StopTime);
  end;
  Result := TSubtitleRange(SubRangeFactory.CreateRange);
  Result.Assign(FirstRange);
  Result.StopTime := LastStopTime;
  Result.Text := AccuText;
end;

// -----------------------------------------------------------------------------

function TMainForm.SimpleSetSubtitleStartTime(Index, NewTime : Integer) : Integer;
var SubRange : TSubtitleRange;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  Result := SubRange.StartTime;
  SubRange.StartTime := NewTime;
end;

function TMainForm.SimpleSetSubtitleStopTime(Index, NewTime : Integer) : Integer;
var SubRange : TSubtitleRange;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  Result := SubRange.StopTime;
  SubRange.StopTime := NewTime;
end;

function TMainForm.SimpleSetSubtitleText(Index : Integer; NewText : WideString) : WideString;
var SubRange : TSubtitleRange;
begin
  SubRange := TSubtitleRange(WAVDisplayer.RangeList[Index]);
  Result := SubRange.Text;
  SubRange.Text := NewText;
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
      SubtitleRange := TSubtitleRange(WavDisplayer.RangeList[ChangeSubData.FIndex]);
      ModifiedRangeList.Add(SubtitleRange);
      // Apply subtitle change
      if (ChangeSubData.StartChanged) then
        SubtitleRange.StartTime := ChangeSubData.GetStart(IsUndo);
      if (ChangeSubData.StopChanged) then
        SubtitleRange.StopTime := ChangeSubData.GetStop(IsUndo);
      if (ChangeSubData.TextChanged) then
        SubtitleRange.Text := ChangeSubData.GetText(IsUndo);
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
    ChangeSubData.FIndex := SubtitleRange.Node.Index;
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
begin
  ClearStack(RedoStack);
  ActionRedo.Enabled := False;
  UndoStack.Push(UndoableTask);
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

{
TODO UNDO:
karaoke create, clear, change timestamps (with mouse or shortcut)
TODO : display karaoke sylables in wavdisplay
TODO : undo text pipe text modification (modifiy, clear, load)
TODO : undo for insert text file
TODO : undo for style?

procedure TTntCustomStatusBar.WndProc(var Msg: TMessage);
const
  SB_SIMPLEID = Integer($FF);
var
  iPart: Integer;
  szText: PAnsiChar;
  WideText: WideString;
begin
  if Win32PlatformIsUnicode and (Msg.Msg = SB_SETTEXTA) and ((Msg.WParam and SBT_OWNERDRAW) = 0)

}

end.
//------------------------------------------------------------------------------

