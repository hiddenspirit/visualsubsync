// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003 Christophe Paris
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

unit main;

interface

uses
  GlobalUnit, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, WAVDisplayerUnit, SubStructUnit, ComCtrls, Menus, Renderer,
  MiniScrollBarUnit, VirtualTrees, MiscToolsUnit, TntStdCtrls, TntMenus,
  Buttons, TntActnList, ImgList, ActnList, TntDialogs, TntComCtrls,
  PeakCreationProgressFormUnit, ProjectUnit, ServerUnit, TntExtCtrls, IniFiles,
  PreferencesFormUnit, MRUListUnit, StdActns, TntStdActns, TntButtons, TntForms,
  DetachedVideoFormUnit;

type
  TTreeData = record
    Range: TSubtitleRange;
  end;
  PTreeData = ^TTreeData;

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
    TntStatusBar1: TTntStatusBar;
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
    EditUndo1: TTntEditUndo;
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
    MenuItemExportToSSA: TTntMenuItem;
    MenuItemExportToWAV: TTntMenuItem;
    ActionExportToWAV: TTntAction;
    ActionPlaySelStart: TTntAction;
    ActionPlaySelEnd: TTntAction;
    Playselectionstart1: TTntMenuItem;
    Playselectionend1: TTntMenuItem;
    ActionPlaceKaraokeCursorsAtEnd: TTntAction;
    procedure FormCreate(Sender: TObject);

    procedure WAVDisplayer1CursorChange(Sender: TObject);
    procedure WAVDisplayer1PlayCursorChange(Sender: TObject);
    procedure WAVDisplayer1SelectionChange(Sender: TObject);
    procedure WAVDisplayer1ViewChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRange(Sender: TObject);
    procedure WAVDisplayer1SelectedRangeChange(Sender: TObject);
    procedure WAVDisplayer1AutoScrollChange(Sender: TObject);
    procedure WAVDisplayPopup_DeleteRange(Sender: TObject);
    procedure WAVDisplayer1StartPlaying(Sender: TObject);
    procedure WAVDisplayer1KaraokeChanged(Sender: TObject;
        Range : TRange);
    procedure WAVDisplayer1SelectedKaraokeRange(Sender: TObject;
        Range : TRange);
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
    procedure ActionExportToSSAExecute(Sender: TObject);
    procedure ActionExportToWAVExecute(Sender: TObject);
    procedure ActionPlaySelStartExecute(Sender: TObject);
    procedure ActionPlaySelEndExecute(Sender: TObject);
    procedure ActionPlaceKaraokeCursorsAtEndExecute(Sender: TObject);

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

    procedure InitVTV;
    procedure EnableControl(Enable : Boolean);
    procedure LoadSubtitles(Filename: string; var IsUTF8 : Boolean);
    procedure SaveSubtitles(Filename: WideString; InUTF8 : Boolean; BackupOnly : Boolean);
    procedure SaveProject(Project : TVSSProject);
    procedure UpdateLinesCounter;
    function CheckSubtitlesAreSaved : Boolean;

    procedure WAVDisplayer1OnPeakFileCreation(Sender: TObject;
      EventType : TPeakFileCreationEventType; Param : Integer);
    procedure CurrentProjectOnDirtyChange(Sender : TObject);

    procedure FullSortTreeAndSubList;

    procedure OnRecentMenuItemClick(Sender : TObject);
    procedure SetShortcut(TimingMode : Boolean);

    procedure OffsetCurrentSubtitleStartTime(Offset : Integer);
    procedure OffsetCurrentSubtitleStopTime(Offset : Integer);

    procedure ColorizeOrDeleteTextPipe;
    procedure ApplyMouseSettings;
    procedure ApplyAutoBackupSettings;
    procedure UpdateVideoRendererWindow;
    procedure ApplyFontSettings;
    procedure OnSubtitleRangeJSWrapperChange;

    procedure AddSubtitle(StartTime, StopTime : Integer;
      AutoSelect : Boolean);
    procedure SaveSubtitlesAsSSA(Filename: WideString; InUTF8 : Boolean);      
  public
    { Public declarations }
    procedure ShowStatusBarMessage(Text : WideString);
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
  end;

const
  StartEndPlayingDuration: Integer = 500;
  EnableExperimentalKaraoke: Boolean = True;

var
  MainForm: TMainForm;

implementation

uses ActiveX, Math, StrUtils, FindFormUnit, AboutFormUnit,
  ErrorReportFormUnit, DelayFormUnit, SuggestionFormUnit, GotoFormUnit,
  Types, VerticalScalingFormUnit, TntSysUtils, TntWindows, JavaScriptPluginUnit,
  LogWindowFormUnit, CursorManager, FileCtrl, WAVFileUnit, PageProcessorUnit;

{$R *.dfm}

//==============================================================================

// TODO : Test project properties dialog
// TODO : When opening a project fail show the project properties page for modification
// TODO : Add some checking in project unit ok button

// TODO : Better vobsub support (auto select file, force loading, make a customized version maybe)

// TODO : Web : number of suggestion for current sub, and current suggestions
// TODO : Web : We need real gzip compression, deflate support is b0rked in IE

// TODO : Split at cursor for submemo
// TODO : Replace dialog
// TODO : Check HD space before extraction

// TODO : Rework project handling which is a bit messy ATM
// TODO : Separate subtitle file loading/saving, stats according to format into a new classes
// TODO : Ignore tags in stats

//==============================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var i : integer;
begin
  // Enable/disable experimental karaoke stuff
  pmiCreateKaraoke.Visible := EnableExperimentalKaraoke;
  pmiClearKaraoke.Visible := EnableExperimentalKaraoke;

  plCursorPos.DoubleBuffered := True;
  LogForm := TLogForm.Create(nil);

  MRUList := TMRUList.Create(MenuItemOpenRecentRoot);
  MRUList.OnRecentMenuItemClick := OnRecentMenuItemClick;
  ConfigObject := TConfigObject.Create;
  ConfigObject.SetDefaultHotKeys(TntActionList1);

  ServerRootDir := ExtractFilePath(Application.ExeName);
  ServerRootDir := IncludeTrailingPathDelimiter(ServerRootDir) + 'web\';

  vtvSubsList.Constraints.MinWidth := 600;
  PanelMiddle.Constraints.MinHeight := 140;
  PanelBottom.Constraints.MinHeight := 60;
  Splitter1.MinSize := 1;

  // Clear speed button caption text filled by action :p
  for i := 0 to Pred(PanelPlaybackControl.ControlCount) do
      if PanelPlaybackControl.Controls[i] is TSpeedButton then
        TSpeedButton(PanelPlaybackControl.Controls[i]).Caption := '';

  bttWorkingMode.Caption := 'Normal mode';

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
  WAVDisplayer.OnAutoScrollChange := WAVDisplayer1AutoScrollChange;
  WAVDisplayer.OnStartPlaying := WAVDisplayer1StartPlaying;
  WAVDisplayer.OnKaraokeChanged := WAVDisplayer1KaraokeChanged;
  WAVDisplayer.OnSelectedKaraokeRange := WAVDisplayer1SelectedKaraokeRange;
  WAVDisplayer.Enabled := False;
  WAVDisplayer.PopupMenu := WAVDisplayPopupMenu;

  InitVTV;

  MemoSubtitleText.Font.Name := 'Arial';
  MemoSubtitleText.Font.Style := MemoSubtitleText.Font.Style + [fsBold];
  MemoSubtitleText.Font.Size := 10;
  MemoLinesCounter.Font.Assign(MemoSubtitleText.Font);
  MemoTextPipe.Font.Assign(MemoSubtitleText.Font);

  EnableControl(False);

  CurrentProject := TVSSProject.Create;
  CurrentProject.OnDirtyChange := CurrentProjectOnDirtyChange;
  AudioOnlyRenderer := TDShowRenderer.Create;
  VideoRenderer := TDShowRenderer.Create;
  ShowingVideo := False;

  g_GlobalContext.SubList := WAVDisplayer.RangeList;
  g_GlobalContext.CurrentProject := CurrentProject;
  StartSubtitleTime := -1;
  ToggleStartSubtitleTime := -1;
  //StartStopServer;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StartStopServer(True);
  AudioOnlyRenderer.Free;
  VideoRenderer.Free;
  WAVDisplayer.Free;
  SubRangeFactory.Free;
  MRUList.Free;
  ConfigObject.Free;
  CurrentProject.Free;
  if Assigned(LogForm) then
    LogForm.Free;
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

procedure TMainForm.SaveSettings;
var IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  MRUList.SaveIni(IniFile, 'MRUList');
  ConfigObject.SaveIni(IniFile);
  SaveFormPosition(IniFile,MainForm);
  SaveFormPosition(IniFile,ErrorReportForm);
  SaveFormPosition(IniFile,SuggestionForm);
  SaveFormPosition(IniFile,DetachedVideoForm);
  SaveFormPosition(IniFile,LogForm);

  IniFile.WriteInteger('Windows', 'MainForm_PanelTop_Height', PanelTop.Height);
  IniFile.WriteInteger('Windows', 'MainForm_PanelBottom_Height', PanelBottom.Height);

  IniFile.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadSettings;
var IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
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

procedure TMainForm.ShowStatusBarMessage(Text : WideString);
begin
  TntStatusBar1.Panels[1].Text := Text;
  TntStatusBar1.Repaint;
  TimerStatusBarMsg.Interval := 2500;
  TimerStatusBarMsg.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TimerStatusBarMsgTimer(Sender: TObject);
begin
  TntStatusBar1.Panels[1].Text := '';
  TimerStatusBarMsg.Enabled := False;  
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
    ((not ConfigObject.DisableSubtitleEdition) or (bttWorkingMode.Tag = 0));

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

  ActionSetPlaybackRate80.Enabled := Enable;
  ActionSetPlaybackRate90.Enabled := Enable;
  ActionSetPlaybackRate100.Enabled := Enable;

  ActionLoopSelStart.Enabled := Enable;
  ActionLoopSelEnd.Enabled := Enable;
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

procedure TMainForm.WAVDisplayer1SelectedRange(Sender: TObject);
var Node : PVirtualNode;
begin
  if Assigned(WAVDisplayer.SelectedRange) then
  begin
    Node := TSubtitleRange(WAVDisplayer.SelectedRange).Node;
    vtvSubsList.ScrollIntoView(Node,True);
    vtvSubsList.FocusedNode := Node;
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[Node] := True;
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
    CurrentProject.IsDirty := True;
    UpdateLinesCounter;
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
    DeleteNode : PVirtualNode;
begin
  DeleteIdx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if DeleteIdx <> -1 then
  begin
    DeleteNode := TSubtitleRange(WAVDisplayer.RangeList[DeleteIdx]).Node;
    ErrorReportForm.DeleteError(TSubtitleRange(WAVDisplayer.RangeList[DeleteIdx]));
    vtvSubsList.DeleteNode(DeleteNode);
    g_WebRWSynchro.BeginWrite;
    WAVDisplayer.DeleteRangeAt(WAVDisplayer.GetCursorPos);
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayPopup_SplitAtCursor(Sender: TObject);
var Idx : Integer;
    SubRange, NewSubRange : TSubtitleRange;
    NodeData : PTreeData;
    NewNode : PVirtualNode;
begin
  Idx := WAVDisplayer.RangeList.GetRangeIdxAt(WAVDisplayer.GetCursorPos);
  if Idx <> -1 then
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
    NewSubRange := TSubtitleRange(SubRangeFactory.CreateRangeSS(
      WAVDisplayer.GetCursorPos, SubRange.StopTime));
    SubRange.StopTime := WAVDisplayer.GetCursorPos - 1;
    NewSubRange.Text := SubRange.Text;
    NewNode := vtvSubsList.InsertNode(SubRange.Node, amInsertAfter);
    NodeData := vtvSubsList.GetNodeData(NewNode);
    NodeData.Range := NewSubRange;
    NewSubRange.Node := NewNode;
    g_WebRWSynchro.BeginWrite;
    WAVDisplayer.AddRange(NewSubRange);
    vtvSubsList.Repaint;
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

// TODO : unicode filename

procedure TMainForm.LoadSubtitles(Filename: string; var IsUTF8 : Boolean);
var
  S: string;
  BOM : array[0..3] of BYTE;
  i : integer;
  Start, Stop : Integer;
  NextStart, NextStop : Integer;
  Text : string;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  FS : TFileStream;
  EndOfFile : Boolean;
  AutoCorrectedFile : Boolean;
begin
  if not FileExists(Filename) then
  begin
    g_WebRWSynchro.BeginWrite;
    WAVDisplayer.RangeList.Clear;
    vtvSubsList.Clear;
    vtvSubsList.Repaint;
    WAVDisplayer.UpdateView([uvfRange]);
    g_WebRWSynchro.EndWrite;
    Exit;
  end;

  g_WebRWSynchro.BeginWrite;
  WAVDisplayer.RangeList.Clear;
  FS := TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
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

  AutoCorrectedFile := False;
  // Skip lines until a timestamps line
  repeat
    EndOfFile := ReadLineStream(FS, S);
  until IsTimeStampsLine(S, Start, Stop) or EndOfFile;
  while not EndOfFile do
  begin
    // Copy text until a timestamps line
    Text := '';
    while True do
    begin
      EndOfFile := ReadLineStream(FS, S);
      if EndOfFile or IsTimeStampsLine(S, NextStart, NextStop) then
        Break;
      S := Trim(S);        
      Text := Text + S + #13#10;
    end;
    Text := TrimRight(Text);
    if (Start <> -1) and (Stop <> -1) then
    begin
      // Remove the index line if any
      i := RPos(#13#10, Text);
      if((i > 0) and (StrToIntDef(Copy(Text,i+2,MaxInt),-1) <> -1) and (not EndOfFile)) then
      begin
        Delete(Text, i, MaxInt);
      end;
      Text := Trim(Text);      
      if (not EndOfFile) and (Stop = NextStart) then
      begin
        AutoCorrectedFile := True;
        Dec(Stop);
      end;
      NewRange := SubRangeFactory.CreateRangeSS(Start,Stop);
      if IsUTF8 then
        TSubtitleRange(NewRange).Text := UTF8Decode(Text)
      else
        TSubtitleRange(NewRange).Text := Text;

      if (EnableExperimentalKaraoke = True) then
        NewRange.UpdateSubTimeFromText(TSubtitleRange(NewRange).Text);

      WAVDisplayer.RangeList.AddAtEnd(NewRange);
    end;
    Start := NextStart;
    Stop := NextStop;
  end;
  FS.Free;
  WAVDisplayer.RangeList.FullSort;

  vtvSubsList.Clear;
  vtvSubsList.Repaint;
  vtvSubsList.BeginUpdate;
  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    Node := vtvSubsList.AddChild(nil);
    NodeData := vtvSubsList.GetNodeData(Node);
    NodeData.Range := TSubtitleRange(WAVDisplayer.RangeList[i]);
    TSubtitleRange(WAVDisplayer.RangeList[i]).Node := Node;
  end;
  vtvSubsList.EndUpdate;
  WAVDisplayer.UpdateView([uvfRange]);
  if AutoCorrectedFile then
    CurrentProject.IsDirty := True;
  vtvSubsList.Header.AutoFitColumns(False);
  vtvSubsList.Repaint;
  g_WebRWSynchro.EndWrite;
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
    3: CellText := StringConvertCRLFToPipe(NodeData.Range.Text);
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
    Handled := WavDisplayer.Perform(CM_MOUSEWHEEL, TMessage(Msg).WParam,
      TMessage(Msg).LParam) <> 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TagHighlight(RichEdit : TTntRichEdit; TagIndex : Integer);
var savSelStart, savSelLength : Integer;
    i, j : Integer;
    WordArray : WideStringArray;
begin
  if (Pos('{\k', RichEdit.Text) + Pos('{\K', RichEdit.Text)) = 0 then
    Exit;

  RichEdit.Tag := 0;
  RichEdit.Lines.BeginUpdate;
  savSelStart := RichEdit.SelStart;
  savSelLength := RichEdit.SelLength;

  KaraSplit3(RichEdit.Text, WordArray);

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
    NodeData.Range.Text := MemoSubtitleText.Text;

    NeedUpdate := False;
    if (EnableExperimentalKaraoke = True) then
      NeedUpdate := NodeData.Range.UpdateSubTimeFromText(NodeData.Range.Text);

    vtvSubsList.RepaintNode(vtvSubsList.FocusedNode);
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;
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

  // TODO : ignore tags in stats

  for i := MemoSubtitleText.Perform(EM_GETFIRSTVISIBLELINE,0,0) to MemoSubtitleText.Lines.Count-1 do
  begin
    CharCount := Length(MemoSubtitleText.Lines.Strings[i]);
    Inc(TotalCharCount, CharCount);
    s := s + IntToStr(CharCount) + #13#10;
  end;
  for i := MemoSubtitleText.Lines.Count to MemoSubtitleText.Perform(EM_GETLINECOUNT,0,0)-1 do
  begin
    s := s + '0' + #13#10;
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

procedure TMainForm.SaveSubtitles(Filename: WideString; InUTF8 : Boolean;  BackupOnly : Boolean);
var i : integer;
    SubRange : TSubtitleRange;
    FS : TFileStream;
    BackupDstFilename : WideString;
const
    UTF8BOM : array[0..2] of BYTE = ($EF,$BB,$BF);

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;
begin
  if (not BackupOnly) and WideFileExists(Filename) and ConfigObject.EnableBackup then
  begin
    CheckBackupDirectory;
    BackupDstFilename := g_BackupDirectory + WideChangeFileExt(WideExtractFileName(Filename), '.bak');
    WideCopyFile(Filename, BackupDstFilename, False);
  end;

  // TODO : FIX unicode Filename
  FS := TFileStream.Create(Filename, fmCreate);
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
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.UpdateVideoRendererWindow;
begin
  if not MenuItemDetachVideoWindow.Checked then
  begin
    if (VideoRenderer.VideoWidth > 0) and (VideoRenderer.VideoHeight > 0) then
      PanelVideo.Width := (VideoRenderer.VideoWidth * PanelVideo.Height) div VideoRenderer.VideoHeight;
    VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
  end
  else
    VideoRenderer.SetDisplayWindow(DetachedVideoForm.Handle);
end;

//------------------------------------------------------------------------------

procedure TMainForm.LoadProject(Filename : WideString);
var
  ProjectFileIni : TIniFile;
  CM : ICursorManager;
  LoadWAVOK : Boolean;
begin
  if (not WideFileExists(Filename)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The project file "' + Filename +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    Exit;
  end;

  ActionClose.Execute;

  g_WebRWSynchro.BeginWrite;
  CM := TCursorManager.Create(crHourGlass);
  try
    ProjectFileIni := TIniFile.Create(Filename);
    CurrentProject.Filename := Filename;
    CurrentProject.VideoSource := ProjectFileIni.ReadString('VisualSubsync','VideoSource','');
    CurrentProject.WAVFile := ProjectFileIni.ReadString('VisualSubsync','WAVFile','');
    CurrentProject.PeakFile := ProjectFileIni.ReadString('VisualSubsync','PeakFile','');
    CurrentProject.WAVMode := TProjectWAVMode(ProjectFileIni.ReadInteger('VisualSubsync','WAVMode',1));
    CurrentProject.SubtitlesFile := ProjectFileIni.ReadString('VisualSubsync','SubtitlesFile','');
    CurrentProject.TextPipeSource := ProjectFileIni.ReadString('VisualSubsync','TextPipeSource','');
    CurrentProject.TextPipePosition := ProjectFileIni.ReadInteger('VisualSubsync','TextPipePosition',0);
    ProjectFileIni.Free;

    ShowStatusBarMessage('Loading WAV form...');
    LoadWAVOK := False;
    if CurrentProject.WAVMode = pwmPeakOnly then
    begin
      if WideFileExists(CurrentProject.PeakFile) then
        LoadWAVOK := WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile,'.wav'))
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
          MessageBoxW(Handle, PWideChar(WideString('Can''t open peak file : ' +
            CurrentProject.PeakFile)),
            PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
          // TODO : Show project page as a new project but with pre-filled data
          Exit;
        end;
      end;
    end
    else
    begin
      if WideFileExists(CurrentProject.WAVFile) then
        LoadWAVOK := WAVDisplayer.LoadWAV(CurrentProject.WAVFile)
      else
      begin
        if WideFileExists(CurrentProject.PeakFile) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          LoadWAVOK := WAVDisplayer.LoadWAV(WideChangeFileExt(CurrentProject.PeakFile,'.wav'));
          CurrentProject.IsDirty := True;
          // Show warning
          MessageBoxW(Handle, PWideChar(WideString('WAV file ' + CurrentProject.WAVFile +
           ' hasn''t been found, project has been switched to "Peak file only" mode')),
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
          MessageBoxW(Handle, PWideChar(WideString('Can''t open WAV file : ' +
            CurrentProject.WAVFile)),
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
        AudioOnlyRenderer.KillVideo;
    end;

    ShowStatusBarMessage('Loading video file...');
    if VideoRenderer.Open(CurrentProject.VideoSource) then
    begin
      UpdateVideoRendererWindow;
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

    EnableControl(True);

    CurrentProjectOnDirtyChange(nil);
  finally
    // Hide the video
    if (not VideoRenderer.IsOpen) and ShowingVideo then
        ActionShowHideVideo.Execute;
    g_GlobalContext.WavAverageBytePerSecond := WAVDisplayer.GetWAVAverageBytePerSecond;
    g_WebRWSynchro.EndWrite;    
  end;
  MRUList.AddFile(CurrentProject.Filename);
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveProject(Project : TVSSProject);
var
  ProjectFileIni : TIniFile;
begin
  ProjectFileIni := TIniFile.Create(Project.Filename);
  ProjectFileIni.WriteString('VisualSubsync','VideoSource',Project.VideoSource);
  ProjectFileIni.WriteString('VisualSubsync','WAVFile',Project.WAVFile);
  ProjectFileIni.WriteString('VisualSubsync','PeakFile',Project.PeakFile);
  ProjectFileIni.WriteInteger('VisualSubsync','WAVMode',Ord(Project.WAVMode));
  ProjectFileIni.WriteString('VisualSubsync','SubtitlesFile',Project.SubtitlesFile);

  if Length(Trim(MemoTextPipe.Text)) > 0 then
  begin
    MemoTextPipe.PlainText := False;
    MemoTextPipe.Lines.SaveToFile(WideChangeFileExt(Project.Filename,'.vssscript'));
    // Save cursor position
    ProjectFileIni.WriteInteger('VisualSubsync', 'TextPipePosition',
      MemoTextPipe.SelStart);
    ProjectFileIni.WriteString('VisualSubsync', 'TextPipeSource', Project.TextPipeSource);
  end;

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
  WAVDisplayer.AutoScrolling := True;
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
  WAVDisplayer.AutoScrolling := True;
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
  ProjectFileIni : TIniFile;
begin
  ProjectForm.ConfigureInNewProjectMode;
  if (ProjectForm.ShowModal = mrOK) then
  begin
    // Create the project file
    ProjectFileIni := TIniFile.Create(ProjectForm.EditProjectFilename.Text);
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
var IsPlaying : Boolean;
    PlayingPos : Integer;
begin
  if (not VideoRenderer.IsOpen) then
  begin
    ShowStatusBarMessage('Video file is not open...');
    Exit;
  end;

  ShowingVideo := not ShowingVideo;

  PlayingPos := 0;
  IsPlaying := WAVDisplayer.IsPlaying;
  if IsPlaying then
    PlayingPos := WAVDisplayer.GetPlayCursorPos;

  if MenuItemDetachVideoWindow.Checked then
  begin
    DetachedVideoForm.Visible := ShowingVideo;
  end
  else
  begin
    PanelVideo.Visible := ShowingVideo;
    SplitterWAVDisplay_Video.Visible := ShowingVideo;
  end;

  if ShowingVideo then
    WAVDisplayer.SetRenderer(VideoRenderer)
  else
    WAVDisplayer.SetRenderer(AudioOnlyRenderer);
  WAVDisplayer.UpdateView([uvfPageSize]);
  if IsPlaying then
  begin
    WAVDisplayer.SetCursorPos(PlayingPos);
    ActionPlay.Execute;
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
    SaveSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8, False);
  SaveProject(CurrentProject);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
begin
  TntSaveDialog1.Filter := 'Subtitles files (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  if TntSaveDialog1.Execute then
  begin
    SaveSubtitles(TntSaveDialog1.FileName, CurrentProject.IsUTF8, False);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionProjectPropertiesExecute(Sender: TObject);
var ProjectHasChanged : Boolean;
begin
  // TODO : test this more
  ProjectForm.ConfigureInModifyProjectMode(CurrentProject);
  if (ProjectForm.ShowModal = mrOK) then
  begin
    g_WebRWSynchro.BeginWrite;

    // Check the thing that have changed and update
    ProjectHasChanged := False;

    if (CurrentProject.SubtitlesFile <> ProjectForm.EditSubtitleFilename.Text) then
    begin
      CheckSubtitlesAreSaved;
      CurrentProject.SubtitlesFile := ProjectForm.EditSubtitleFilename.Text;
      ErrorReportForm.Clear;
      LoadSubtitles(CurrentProject.SubtitlesFile,CurrentProject.IsUTF8);
      ProjectHasChanged := True;
    end;

    if (CurrentProject.VideoSource <> ProjectForm.EditVideoFilename.Text) then
    begin
      CurrentProject.VideoSource := ProjectForm.EditVideoFilename.Text;
      VideoRenderer.Open(CurrentProject.VideoSource);
      UpdateVideoRendererWindow;
      ProjectHasChanged := True;
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
      SaveProject(CurrentProject);
    end;

    // Hide the video
    if (not VideoRenderer.IsOpen) and ShowingVideo then
        ActionShowHideVideo.Execute;
    g_GlobalContext.WavAverageBytePerSecond := WAVDisplayer.GetWAVAverageBytePerSecond;

    g_WebRWSynchro.EndWrite;
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
    Node : PVirtualNode;
    NodeData : PTreeData;

    procedure DelaySub(Range : TRange; DelayInMs : Integer;
      ShiftType : Integer);
    var i : Integer;
    begin
      // ShiftType : 0 -> start & end
      //             1 -> start only
      //             2 -> end only
      if (ShiftType = 0) or (ShiftType = 1) then
      begin
        Range.StartTime := Range.StartTime + DelayInMs;
      end;
      if (ShiftType = 0) or (ShiftType = 2) then
      begin
        Range.StopTime := Range.StopTime + DelayInMs;
      end;
      if (ShiftType = 0) then
      begin
        for i:=0 to Length(Range.SubTime)-1 do
        begin
          Range.SubTime[i] := Range.SubTime[i] + DelayInMs;
        end;
      end;
    end;

begin
  DelayForm := TDelayForm.Create(nil);

  // Prefill the dialog
  if (vtvSubsList.SelectedCount > 1) then
  begin
    DelayForm.rgApplyTo.ItemIndex := 1;
  end;
  if not WAVDisplayer.SelectionIsEmpty then
  begin
    DelayForm.meDelay.Text := TimeMsToString(WAVDisplayer.Selection.StopTime -
      WAVDisplayer.Selection.StartTime)
  end;

  if DelayForm.ShowModal = mrOk then
  begin
    g_WebRWSynchro.BeginWrite;

    DelayInMs := TimeStringToMs(DelayForm.meDelay.EditText);
    if DelayForm.rgType.ItemIndex = 1 then
      DelayInMs := -DelayInMs;
    case DelayForm.rgApplyTo.ItemIndex of
      0: // All subtitles
        begin
          Node := vtvSubsList.GetFirst;
          while Assigned(Node) do
          begin
            NodeData := vtvSubsList.GetNodeData(Node);
            DelaySub(NodeData.Range, DelayInMs, DelayForm.rgType.ItemIndex);
            Node := vtvSubsList.GetNext(Node);
          end;
        end;
      1: // Selected subs
        begin
          Node := vtvSubsList.GetFirstSelected;
          while Assigned(Node) do
          begin
            NodeData := vtvSubsList.GetNodeData(Node);
            DelaySub(NodeData.Range, DelayInMs, DelayForm.rgType.ItemIndex);
            Node := vtvSubsList.GetNextSelected(Node);
          end;
          FullSortTreeAndSubList;
        end;
      2: // From focused subs to end
        begin
          Node := vtvSubsList.FocusedNode;
          while Assigned(Node) do
          begin
            NodeData := vtvSubsList.GetNodeData(Node);
            DelaySub(NodeData.Range, DelayInMs, DelayForm.rgType.ItemIndex);
            Node := vtvSubsList.GetNext(Node);
          end;
          FullSortTreeAndSubList;
        end;
    end;
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;    
    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsList.InvalidateColumn(1);
    vtvSubsList.InvalidateColumn(2);    
  end;
  DelayForm.Free;
  DelayForm := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiSubListDeleteClick(Sender: TObject);
var Idx : integer;
    Node : PVirtualNode;
    NodeData: PTreeData;
    NodeBeforeNodeToSelect, NodeToSelect : PVirtualNode;
begin
  g_WebRWSynchro.BeginWrite;
  Node := vtvSubsList.GetFirstSelected;
  NodeBeforeNodeToSelect := vtvSubsList.GetPrevious(Node);
  while Assigned(Node) do
  begin
    NodeData := vtvSubsList.GetNodeData(Node);
    ErrorReportForm.DeleteError(NodeData.Range);
    Idx := WAVDisplayer.RangeList.IndexOf(NodeData.Range);
    WAVDisplayer.DeleteRangeAtIdx(Idx,False);
    Node := vtvSubsList.GetNextSelected(Node);
  end;
  vtvSubsList.DeleteSelectedNodes;
  CurrentProject.IsDirty := True;
  WAVDisplayer.UpdateView([uvfRange]);
  g_WebRWSynchro.EndWrite;

  // Position on next subtitle
  if Assigned(NodeBeforeNodeToSelect) then
  begin
    NodeToSelect := vtvSubsList.GetNext(NodeBeforeNodeToSelect);
    if not Assigned(NodeToSelect) then
      NodeToSelect := NodeBeforeNodeToSelect;
  end
  else
    NodeToSelect := vtvSubsList.GetFirst;
  if Assigned(NodeToSelect) then
  begin
    vtvSubsList.FocusedNode := NodeToSelect;
    vtvSubsList.ClearSelection;
    vtvSubsList.Selected[NodeToSelect] := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiSubListMergeClick(Sender: TObject);
var i, Idx : integer;
    Node : PVirtualNode;
    NodeData: PTreeData;
    AccuText : WideString;
    LastStopTime : Integer;
    DeleteList : TList;
begin
  AccuText := '';
  LastStopTime := 0;

  g_WebRWSynchro.BeginWrite;

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
    AccuText := AccuText + #13#10 + NodeData.Range.Text;
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

  // Make sure to update display
  CurrentProject.IsDirty := True;
  Node := vtvSubsList.GetFirstSelected;
  vtvSubsList.FocusedNode := Node;
  if Assigned(WAVDisplayer.SelectedRange) then
    WAVDisplayer.SelectedRange := nil;
  WAVDisplayer.ClearSelection;
  WAVDisplayer.UpdateView([uvfRange,uvfSelection]);
  vtvSubsList.Repaint;

  g_WebRWSynchro.EndWrite;
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
    res := MessageBoxW(Handle, PWideChar(WideString('The subtitle file "' + CurrentProject.SubtitlesFile +
      '" has changed, do you want to save it before closing ?')), PWideChar(WideString('Warning')), MB_YESNOCANCEL or MB_ICONWARNING);
    if (res = IDYES) then
    begin
      SaveSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8, False);
      SaveProject(CurrentProject);
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
  vtvSubsListDblClick(Self);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNextSubExecute(Sender: TObject);
var CurrentNode, NextNode : PVirtualNode;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    WAVDisplayer.SelectNextKaraoke;
    ActionPlay.Execute;
    Exit;
  end;

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
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPreviousSubExecute(Sender: TObject);
var CurrentNode, PreviousNode : PVirtualNode;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    WAVDisplayer.SelectPreviousKaraoke;
    ActionPlay.Execute;
    Exit;
  end;

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
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FullSortTreeAndSubList;
begin
  g_WebRWSynchro.BeginWrite;
  WAVDisplayer.RangeList.FullSort;
  g_WebRWSynchro.EndWrite;
  vtvSubsList.Sort(nil,0,sdDescending,False);
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
        vtvSubsList.FocusedNode := Node;
        vtvSubsList.ClearSelection;
        vtvSubsList.Selected[Node] := True;
      end;
    end
    else
    begin
      MemoSubtitleText.Tag := 0;
      MemoSubtitleText.Text := '';
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
      ((not ConfigObject.DisableSubtitleEdition) or (bttWorkingMode.Tag = 0));
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
begin
  String2Font(ConfigObject.SubListFont, vtvSubsList.Font);
  vtvSubsList.Header.AutoFitColumns(False);

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
    g_WebRWSynchro.EndWrite;
    
    Self.Caption := ApplicationName;
    MemoLinesCounter.Text := '';
    TntStatusBar1.Panels[0].Text := '';
    MemoSubtitleText.Text := '';

    EnableControl(False);

    AudioOnlyRenderer.Close;
    VideoRenderer.Close;
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
  g_WebRWSynchro.EndWrite;
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
  if (bttWorkingMode.Tag = 0) then
  begin
    bttWorkingMode.Caption := 'Timing mode';
    bttWorkingMode.Font.Color := clRed;
    bttWorkingMode.Tag := 1;
    if ConfigObject.MouseEnableSSATimingMode then
      WAVDisplayer.SelMode := smSSA
    else
      WAVDisplayer.SelMode := smCoolEdit;
  end
  else
  begin
    bttWorkingMode.Caption := 'Normal mode';
    bttWorkingMode.Tag := 0;
    bttWorkingMode.Font.Color := clWindowText;
    WAVDisplayer.SelMode := smCoolEdit;
  end;
  KeyPreview := (bttWorkingMode.Tag = 1);
  PreferencesForm.SetMode(bttWorkingMode.Tag = 1);
  SetShortcut(PreferencesForm.GetMode);
  MemoSubtitleText.Enabled := MemoLinesCounter.Enabled and
    ((not ConfigObject.DisableSubtitleEdition) or (bttWorkingMode.Tag = 0));
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
    PlayingMode := pmtSelectionEnd;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
      WAVDisplayer.Selection.StopTime,
      True);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.AddSubtitle(StartTime, StopTime : Integer;
  AutoSelect : Boolean);
var NewRange, SiblingRange : TRange;
    NewNode, SiblingNode: PVirtualNode;
    NodeData: PTreeData;
    InsertPos : Integer;
begin
  g_WebRWSynchro.BeginWrite;

  NewRange := SubRangeFactory.CreateRangeSS(StartTime, StopTime);
  InsertPos := WAVDisplayer.RangeList.FindInsertPos(NewRange);
  if (InsertPos >= WAVDisplayer.RangeList.Count) then
    NewNode := vtvSubsList.AddChild(nil)
  else
  begin
    SiblingRange := WAVDisplayer.RangeList[InsertPos];
    SiblingNode := TSubtitleRange(SiblingRange).Node;
    NewNode := vtvSubsList.InsertNode(SiblingNode, amInsertBefore);
  end;
  NodeData := vtvSubsList.GetNodeData(NewNode);
  NodeData.Range := TSubtitleRange(NewRange);
  TSubtitleRange(NewRange).Node := NewNode;
  WAVDisplayer.AddRange(NewRange);

  vtvSubsList.ScrollIntoView(NewNode,True);
  vtvSubsList.FocusedNode := NewNode;
  vtvSubsList.ClearSelection;
  vtvSubsList.Selected[NewNode] := True;
  vtvSubsList.Repaint;

  if AutoSelect and (WAVDisplayer.SelMode = smCoolEdit) then
    WAVDisplayer.SelectedRange := NewRange;

  CurrentProject.IsDirty := True;

  g_WebRWSynchro.EndWrite;

//  if (vtvSubsList.ChildCount[nil] = 1) then
//  begin
//    vtvSubsList.Header.AutoFitColumns(False);
//    vtvSubsList.Repaint;
//  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionAddSubtitleExecute(Sender: TObject);
begin
  AddSubtitle(WAVDisplayer.Selection.StartTime,
    WAVDisplayer.Selection.StopTime, True);
  MemoSubtitleText.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSetSubtitleTimeExecute(Sender: TObject);
var NodeData : PTreeData;
    NextNodeToFocus : PVirtualNode;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NextNodeToFocus := vtvSubsList.GetNext(vtvSubsList.FocusedNode);
    g_WebRWSynchro.BeginWrite;
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);

    NodeData.Range.StartTime := WAVDisplayer.Selection.StartTime;
    NodeData.Range.StopTime := WAVDisplayer.Selection.StopTime;

    FullSortTreeAndSubList; // lazy me :) but well it's fast enough
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;

    if Assigned(NextNodeToFocus) then
    begin
      vtvSubsList.FocusedNode := NextNodeToFocus;
      vtvSubsList.ClearSelection;
      vtvSubsList.Selected[NextNodeToFocus] := True;
    end;

    WAVDisplayer.UpdateView([uvfRange]);
    vtvSubsList.Repaint;
  end;
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
    NewSubTime, bStart, bStop : Integer;
    Idx : Integer;
begin
  if Assigned(WAVDisplayer.KaraokeSelectedRange) then
  begin
    R := WAVDisplayer.KaraokeSelectedRange;
    Idx := (WAVDisplayer.KaraokeSelectedIndex - 1);
    if (Idx > 0) then
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
      g_WebRWSynchro.BeginWrite;
      NodeData.Range.StartTime := NodeData.Range.StartTime + Offset;
      g_WebRWSynchro.EndWrite;
      if (WAVDisplayer.SelectedRange = NodeData.Range) or
         Assigned(WAVDisplayer.KaraokeSelectedRange) then
      begin
        WAVDisplayer.Selection.StartTime := NodeData.Range.StartTime;
        WAVDisplayer1SelectionChange(WAVDisplayer);
      end;
      WAVDisplayer.UpdateView([uvfRange]);
      vtvSubsList.RepaintNode(vtvSubsList.FocusedNode);
      if (Length(NodeData.Range.SubTime) > 0) then
      begin
        WAVDisplayer1KaraokeChanged(WAVDisplayer, NodeData.Range);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OffsetCurrentSubtitleStopTime(Offset : Integer);
var NodeData : PTreeData;
    R : TRange;
    NewSubTime, bStart, bStop : Integer;
    Idx : Integer;
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
      g_WebRWSynchro.BeginWrite;
      NodeData.Range.StopTime := NodeData.Range.StopTime + Offset;
      g_WebRWSynchro.EndWrite;
      if (WAVDisplayer.SelectedRange = NodeData.Range) or
         Assigned(WAVDisplayer.KaraokeSelectedRange) then
      begin
        WAVDisplayer.Selection.StopTime := NodeData.Range.StopTime;
        WAVDisplayer1SelectionChange(WAVDisplayer);
      end;
      WAVDisplayer.UpdateView([uvfRange]);
      vtvSubsList.RepaintNode(vtvSubsList.FocusedNode);
      if (Length(NodeData.Range.SubTime) > 0) then
      begin
        WAVDisplayer1KaraokeChanged(WAVDisplayer, NodeData.Range);
      end;
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
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    MemoTextPipe.Lines.LoadFromFile(TntOpenDialog1.FileName);
    CurrentProject.TextPipeSource := TntOpenDialog1.FileName;
    if MemoTextPipe.Visible = False then
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
begin
  if (MemoTextPipe.SelLength > 0) and (not WavDisplayer.SelectionIsEmpty) then
  begin
    ActionAddSubtitle.Execute;
    MemoSubtitleText.Text := Trim(MemoTextPipe.SelText);
    ColorizeOrDeleteTextPipe;
//    // If this is the first subtitle, readjust columns
//    if (vtvSubsList.ChildCount[nil] = 1) then
//    begin
//      vtvSubsList.Header.AutoFitColumns(False);
//      vtvSubsList.Repaint;
//    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ColorizeOrDeleteTextPipe;
var NewSelLen : Integer;
    NewColor : TColor;
begin
  if pmiAutoColorizeText.Checked then
  begin
    NewColor := clRed;
    if Assigned(vtvSubsList.FocusedNode) then
    begin
      if (vtvSubsList.AbsoluteIndex(vtvSubsList.FocusedNode) mod 2) = 0 then
        NewColor := $003333FF
      else
        NewColor := $00FF8000;
    end;
    MemoTextPipe.SelAttributes.Color := NewColor;
  end
  else if pmiAutoDeleteText.Checked then
    MemoTextPipe.ClearSelection
  else if pmiAutoDeleteAllTextBefore.Checked then
  begin
    NewSelLen := MemoTextPipe.SelLength + MemoTextPipe.SelStart;
    MemoTextPipe.SelStart := 0;
    MemoTextPipe.SelLength := NewSelLen;
    MemoTextPipe.ClearSelection;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoTextPipePopupMenuPopup(Sender: TObject);
begin
  pmiAddAsSubtitle.Enabled := (MemoTextPipe.SelLength > 0) and
    (not WavDisplayer.SelectionIsEmpty);
  pmiReplaceSubtitleFromPipe.Enabled := (MemoTextPipe.SelLength > 0) and
    (vtvSubsList.FocusedNode <> nil);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionReplaceFromPipeExecute(Sender: TObject);
var NextNode : PVirtualNode;
begin
  if (MemoTextPipe.SelLength > 0) and (vtvSubsList.FocusedNode <> nil) then
  begin
    MemoSubtitleText.Text := Trim(MemoTextPipe.SelText);
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
  end;
  MenuItemDetachVideoWindow.Checked := not MenuItemDetachVideoWindow.Checked;
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
  g_WebRWSynchro.EndWrite;

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
  ShowStatusBarMessage(Msg);
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

  g_WebRWSynchro.BeginWrite;
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  while JSPEnum.GetNext(JPlugin) do
  begin
    JSPluginInfo := ConfigObject.GetJSPluginInfoByName(JPlugin.Name);
    if Assigned(JSPluginInfo) and (JSPluginInfo.Enabled = False) then
    begin
      FreeAndNil(JPlugin);
      Continue;
    end;

    ConfigObject.ApplyParam(JPlugin);
    JPlugin.OnSubtitleChange := OnSubtitleRangeJSWrapperChange;

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
  g_WebRWSynchro.EndWrite;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
  WAVDisplayer.Repaint;  
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
    MemoSubtitleText.Text := NodeData.Range.Text;
    MemoSubtitleText.Tag := 1;
    if (WAVDisplayer.KaraokeSelectedRange = NodeData.Range) then
      TagHighlight(MemoSubtitleText, WAVDisplayer.KaraokeSelectedIndex)
    else
      TagHighlight(MemoSubtitleText, -1);

  end
  else
  begin
    MemoSubtitleText.Tag := 0;
    MemoSubtitleText.Text := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnSubtitleRangeJSWrapperChange;
begin
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
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;

  g_WebRWSynchro.BeginWrite;
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
        JPlugin.OnSubtitleChange := OnSubtitleRangeJSWrapperChange;
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
  JSPEnum.Free;
  g_WebRWSynchro.EndWrite;
  vtvSubsList.Repaint;
  WAVDisplayer.UpdateView([uvfSelection, uvfRange]);
  vtvSubsListFocusChanged(vtvSubsList, vtvSubsList.FocusedNode, 0);
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormActivate(Sender: TObject);
begin
  FormHasBeenActivated := True;
end;

//------------------------------------------------------------------------------
procedure TMainForm.MemoSubtitleTextMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pt : TPoint;
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
    AddSubtitle(StartSubtitleTime, WAVDisplayer.GetPlayCursorPos, False);
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
    AddSubtitle(StartSubtitleTime, WAVDisplayer.GetPlayCursorPos, False);
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
  if (bttWorkingMode.Tag = 1) and (Key = VK_SPACE) and
    (ConfigObject.EnableToggleCreation = True) then
  begin
    if (WAVDisplayer.IsPlaying = True) and (ToggleStartSubtitleTime = -1) then
      ToggleStartSubtitleTime := WAVDisplayer.GetPlayCursorPos;
    Key := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.TntFormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // subtitle dynamic creation (toggle mode) in timing mode only
  if (bttWorkingMode.Tag = 1) and (Key = VK_SPACE) and
    (ConfigObject.EnableToggleCreation = True) then
  begin
    if (ToggleStartSubtitleTime <> -1) and
      (WAVDisplayer.GetPlayCursorPos > (ToggleStartSubtitleTime + 400)) then
    begin
      AddSubtitle(ToggleStartSubtitleTime, WAVDisplayer.GetPlayCursorPos, False);
    end;
    ToggleStartSubtitleTime := -1;
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
procedure WhiteSpaceSplit(Text : WideString; var WordArray : WideStringArray);
var i, j : Integer;
begin
  SetLength(WordArray, 0);
  j := 1;
  for i:=1 to Length(Text) do
  begin
    if (Text[i] = ' ') or (Text[i] = #10) or (Text[i] = '/') then
    begin
      SetLength(WordArray, Length(WordArray)+1);
      WordArray[Length(WordArray)-1] := WideStringReplace(
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
    WordArray[Length(WordArray)-1] := Copy(Text, j, i-j+1);
  end;
end;

// -----

function KaraLen(Text : WideString) : Integer;
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
  g_WebRWSynchro.EndWrite;
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
  g_WebRWSynchro.EndWrite;
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
  g_WebRWSynchro.EndWrite;
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
begin
  if Assigned(VideoRenderer) and Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    // +50 to make sure we are inside the subtitle (duration of 1 frame) 
    VideoRenderer.ShowImageAt(NodeData.Range.StartTime + 50);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowStopFrameExecute(Sender: TObject);
var NodeData: PTreeData;
begin
  if Assigned(VideoRenderer) and Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    VideoRenderer.ShowImageAt(NodeData.Range.StopTime);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowFrameAtCursorExecute(Sender: TObject);
begin
  if Assigned(VideoRenderer) then
  begin
    VideoRenderer.ShowImageAt(WAVDisplayer.GetCursorPos);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SaveSubtitlesAsSSA(Filename: WideString; InUTF8 : Boolean);
var i : integer;
    SubRange : TSubtitleRange;
    FS : TFileStream;
    s : WideString;
const
    UTF8BOM : array[0..2] of BYTE = ($EF,$BB,$BF);

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;
begin
  FS := TFileStream.Create(Filename, fmCreate);
  if InUTF8 then
  begin
    FS.Write(UTF8BOM[0],Length(UTF8BOM));
  end;

  WriteStringLnStream('[Script Info]', FS);
  WriteStringLnStream('; Written by VisualSubSync ' + g_ApplicationVersion.VersionString, FS);
  WriteStringLnStream('Title: <untitled>', FS);
  WriteStringLnStream('Original Script: <unknown>', FS);
  WriteStringLnStream('ScriptType: v4.00', FS);
  WriteStringLnStream('', FS);
  WriteStringLnStream('[V4 Styles]', FS);
  WriteStringLnStream('Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding', FS);
  WriteStringLnStream('Style: Default,Arial,28,65535,255,16744448,-2147483640,-1,0,1,3,0,2,30,30,30,0,128', FS);
  WriteStringLnStream('', FS);
  WriteStringLnStream('[Events]', FS);
  WriteStringLnStream('Format: Marked, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text', FS);

  //Dialogue: Marked=0,0:00:05.00,0:00:07.00,*Default,,0000,0000,0000,,toto tata

  for i:=0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    s := Format('Dialogue: Marked=0,%s,%s,*Default,,0000,0000,0000,,',
      [TimeMsToSSAString(SubRange.StartTime),
       TimeMsToSSAString(SubRange.StopTime)]);
    if InUTF8 then
      s := s + UTF8Encode(Subrange.Text)
    else
      s := s + Subrange.Text;
    WriteStringLnStream(s, FS);
  end;
  FS.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionExportToSSAExecute(Sender: TObject);
begin
  TntSaveDialog1.Filter := 'Subtitles files (*.ssa)|*.SSA' + '|' +
    'All files (*.*)|*.*';
  TntSaveDialog1.FileName := WideChangeFileExt(CurrentProject.SubtitlesFile, '.ssa');
  if TntSaveDialog1.Execute then
  begin
    SaveSubtitlesAsSSA(TntSaveDialog1.FileName, CurrentProject.IsUTF8);
  end;
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
    MessageBoxW(Handle, PWideChar(WideString('You need to create a project with a WAV file')),
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
    PlayingMode := pmtSelectionEnd;
    WAVDisplayer.PlayRange(
      Max(WAVDisplayer.Selection.StartTime, WAVDisplayer.Selection.StopTime - StartEndPlayingDuration),
      WAVDisplayer.Selection.StopTime);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ActionPlaceKaraokeCursorsAtEndExecute(Sender: TObject);
var NodeData: PTreeData;
    NeedUpdate : Boolean;
    i, s, t : Integer;
begin
  if Assigned(vtvSubsList.FocusedNode) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    t := 100;
    s := NodeData.Range.StopTime - (Length(NodeData.Range.SubTime) * t);
    if (s > NodeData.Range.StartTime) then
    begin
      g_WebRWSynchro.BeginWrite;
      for i:=0 to Length(NodeData.Range.SubTime)-1 do
      begin
        NodeData.Range.SubTime[i] := s + (t * i);
      end;
      g_WebRWSynchro.EndWrite;
      WAVDisplayer.UpdateView([uvfRange]);
      WAVDisplayer1KaraokeChanged(nil, NodeData.Range);
    end;
  end;
end;

//------------------------------------------------------------------------------

// TODO : auto clear karaoke timing that are out of subtitle bound ???
// TODO : update documentation (timing button)

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------

