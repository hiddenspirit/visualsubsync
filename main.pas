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
  PreferencesFormUnit, MRUListUnit, StdActns, TntStdActns;

type
  TTreeData = record
    Range: TSubtitleRange;
  end;
  PTreeData = ^TTreeData;

  TMainForm = class(TForm)
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
    ShowHidevideo1: TTntMenuItem;
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
    MemoLinesCounter: TTntMemo;
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
    MemoSubPopupMenu: TPopupMenu;
    pmiMemoSubCopy: TMenuItem;
    pmiMemoSubCut: TMenuItem;
    pmiMemoSubPaste: TMenuItem;
    EditCut1: TTntEditCut;
    EditCopy1: TTntEditCopy;
    EditPaste1: TTntEditPaste;
    EditSelectAll1: TTntEditSelectAll;
    EditUndo1: TTntEditUndo;
    EditDelete1: TTntEditDelete;
    Undo1: TMenuItem;
    N9: TMenuItem;
    Delete1: TMenuItem;
    N10: TMenuItem;
    SelectAll1: TMenuItem;
    MenuItemHelpIndex: TTntMenuItem;
    MenuItemHelpIndexWAVDisplayControl: TTntMenuItem;
    pmiWAVDispSetSubtitleTime: TTntMenuItem;
    N11: TTntMenuItem;
    ActionInsertTextFile: TTntAction;
    Inserttextfile1: TTntMenuItem;
    procedure FormCreate(Sender: TObject);

    procedure WAVDisplayer1CursorChange(Sender: TObject);
    procedure WAVDisplayer1PlayCursorChange(Sender: TObject);
    procedure WAVDisplayer1SelectionChange(Sender: TObject);
    procedure WAVDisplayer1ViewChange(Sender: TObject);
    procedure WAVDisplayer1SelectedRange(Sender: TObject);
    procedure WAVDisplayer1SelectedRangeChange(Sender: TObject);
    procedure WAVDisplayer1AutoScrollChange(Sender: TObject);
    procedure WAVDisplayPopup_AddRange(Sender: TObject);
    procedure WAVDisplayPopup_DeleteRange(Sender: TObject);
    procedure vtvSubsListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure vtvSubsListChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
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
    procedure PanelVideoResize(Sender: TObject);
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
    procedure pmiWAVDispSetSubtitleTimeClick(Sender: TObject);
    procedure ActionInsertTextFileExecute(Sender: TObject);

  private
    { Private declarations }
    WAVDisplayer : TWAVDisplayer;
    MiniSB : TMiniScrollBar;
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

    procedure InitVTV;
    procedure EnableControl(Enable : Boolean);
    procedure LoadSubtitles(Filename: string; var IsUTF8 : Boolean);
    procedure SaveSubtitles(Filename: string; InUTF8 : Boolean);
    procedure SaveProject(Project : TVSSProject);
    procedure UpdateLinesCounter;
    function CheckSubtitlesAreSaved : Boolean;

    procedure WAVDisplayer1OnPeakFileCreation(Sender: TObject;
      EventType : TPeakFileCreationEventType; Param : Integer);
    procedure CurrentProjectOnDirtyChange(Sender : TObject);

    procedure FullSortTreeAndSubList;

    procedure OnRecentMenuItemClick(Sender : TObject);
  public
    { Public declarations }
    procedure ShowStatusBarMessage(Text : WideString);
    procedure SelectNode(Node : PVirtualNode);
    procedure SelectNodeFromTimestamps(Start, Stop : Integer);
    procedure StartStopServer(Stop : Boolean = False);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ShowPreferences(TabSheet : TTntTabSheet);
    procedure LoadProject(Filename : string);
  end;

var
  MainForm: TMainForm;

implementation

uses ActiveX, Math, StrUtils, FindFormUnit, AboutFormUnit,
  ErrorReportFormUnit, DelayFormUnit, SuggestionFormUnit, GotoFormUnit,
  Types, ShellApi;

{$R *.dfm}

//==============================================================================

// TODO : Test project properties dialog
// TODO : When opening a project fail show the project properties page for modification
// TODO : Add some checking in project unit ok button

// TODO : Better vobsub support (auto select file, force loading, make a customized version maybe)
// TODO : Web: number of suggestion for current sub
// TODO : IE error when submitting ???
// TODO : Split at cursor for submemo
// TODO : When in ErroForm and click in MemoSubtitleText it go to 1,1
// TODO : Replace dialog
// TODO : Check HD space before extraction

// TODO : When saving add an option to create .bak file

// TODO : Rework project handling which is a bit messy ATM
// TODO : Separate subtitle file loading/saving, stats according to format into a new classes
// TODO : Ignore tags in stats

//==============================================================================

procedure TMainForm.FormCreate(Sender: TObject);
var i : integer;
begin
  plCursorPos.DoubleBuffered := True;

  MRUList := TMRUList.Create(MenuItemOpenRecentRoot);
  MRUList.OnRecentMenuItemClick := OnRecentMenuItemClick;
  ConfigObject := TConfigObject.Create;

  ServerRootDir := ExtractFilePath(Application.ExeName);
  ServerRootDir := IncludeTrailingPathDelimiter(ServerRootDir) + 'web\';

  Self.Constraints.MinHeight := 480;
  Self.Constraints.MinWidth := 640;

  // Clear speed button caption text filled by action :p
  for i := 0 to Pred(PanelMiddle.ControlCount) do
      if PanelMiddle.Controls[i] is TSpeedButton then
        TSpeedButton(PanelMiddle.Controls[i]).Caption := '';

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
  WAVDisplayer.Enabled := False;

  MiniSB := TMiniScrollBar.Create(nil);
  MiniSB.Top := WAVDisplayer.Top + WAVDisplayer.Height;
  MiniSB.Left := 0;
  MiniSB.Width := PanelWAVDisplay.Width;
  MiniSB.Height := 12;
  MiniSB.Align := alBottom;
  MiniSB.Parent := PanelWAVDisplay;
  WAVDisplayer.ScrollBar := MiniSB;

  WAVDisplayer.PopupMenu := WAVDisplayPopupMenu;

  InitVTV;

  MemoSubtitleText.Font.Name := 'Arial';
  MemoSubtitleText.Font.Style := MemoSubtitleText.Font.Style + [fsBold];
  MemoSubtitleText.Font.Size := 10;
  MemoLinesCounter.Font.Assign(MemoSubtitleText.Font);

  EnableControl(False);

  CurrentProject := TVSSProject.Create;
  CurrentProject.OnDirtyChange := CurrentProjectOnDirtyChange;
  AudioOnlyRenderer := TDShowRenderer.Create;
  VideoRenderer := TDShowRenderer.Create;

  g_GlobalContext.SubList := WAVDisplayer.RangeList;
  g_GlobalContext.CurrentProject := CurrentProject;
  //StartStopServer;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StartStopServer(True);
  AudioOnlyRenderer.Free;
  VideoRenderer.Free;
  MiniSB.Free;
  WAVDisplayer.Free;
  SubRangeFactory.Free;
  MRUList.Free;
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
  LoadFormPosition(IniFile,MainForm);
  LoadFormPosition(IniFile,ErrorReportForm);
  LoadFormPosition(IniFile,SuggestionForm);

  PanelTop.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelTop_Height',
    PanelTop.Height);
  PanelBottom.Height := IniFile.ReadInteger('Windows', 'MainForm_PanelBottom_Height',
    PanelBottom.Height);
  TntStatusBar1.Top := MainForm.Height;

  IniFile.Free;
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
      [toFullRowSelect,toMultiSelect{,toExtendedFocus}];
    TreeOptions.PaintOptions := TreeOptions.PaintOptions -
      [toShowTreeLines,toShowRoot] +
      [toHideFocusRect, toShowHorzGridLines, toShowVertGridLines];
    Header.Options := Header.Options + [hoVisible] - [hoDrag];
    Header.Style := hsFlatButtons;
    Column := Header.Columns.Add;
    Column.Text := 'Start';
    Column.Width := 80;
    Column.Options := Column.Options - [coAllowClick,coDraggable];
    Column := Header.Columns.Add;
    Column.Text := 'Stop';
    Column.Width := 80;
    Column.Options := Column.Options - [coAllowClick,coDraggable];
    Column := Header.Columns.Add;
    Column.Text := 'Text';
    Column.Options := Column.Options - [coAllowClick,coDraggable];
    Column.Width := 500;
    //FocusedColumn := 2;

    OnGetText := vtvSubsListGetText;
    OnChange := vtvSubsListChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.OnRecentMenuItemClick(Sender : TObject);
var MenuItem : TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  LoadProject(MenuItem.Caption);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ShowStatusBarMessage(Text : WideString);
begin
  TntStatusBar1.Panels[1].Text := Text;
  TntStatusBar1.Repaint;
  TimerStatusBarMsg.Interval := 2000;
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

  ActionZoomIn.Enabled := Enable;
  ActionZoomOut.Enabled := Enable;
  ActionZoomSelection.Enabled := Enable;
  ActionZoomAll.Enabled := Enable;

  ActionFind.Enabled := Enable;
  ActionFindNext.Enabled := Enable;
  ActionGoto.Enabled := Enable;
  ActionCheckErrors.Enabled := Enable;
  ActionDelay.Enabled := Enable;
  ActionInsertTextFile.Enabled := Enable;

  ActionShowHideVideo.Enabled := Enable;
  ActionProjectProperties.Enabled := Enable;
  ActionSaveAs.Enabled := Enable;

  vtvSubsList.Enabled := Enable;
  MemoLinesCounter.Enabled := Enable;
  MemoSubtitleText.Enabled := Enable;

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
var s : string;
begin
  if CurrentProject.IsDirty then s := '*' else s := '';
  Self.Caption := ApplicationName + ' - ' +
    ExtractFileName(CurrentProject.Filename) + s;
  ActionSave.Enabled := CurrentProject.IsDirty;
end;

//------------------------------------------------------------------------------

procedure TMainForm.WAVDisplayPopup_AddRange(Sender: TObject);
var NewRange, SiblingRange : TRange;
    NewNode, SiblingNode: PVirtualNode;
    NodeData: PTreeData;
    InsertPos : Integer;
begin
  g_WebRWSynchro.BeginWrite;
  
  NewRange := SubRangeFactory.CreateRangeSS(WAVDisplayer.Selection.StartTime,
    WAVDisplayer.Selection.StopTime);
  InsertPos := WAVDisplayer.RangeList.FindInsertPos(NewRange);
  if (InsertPos >= WAVDisplayer.RangeList.Count) then
    NewNode := vtvSubsList.AddChild(nil)
  else
  begin
    SiblingRange := WAVDisplayer.RangeList[InsertPos];
    SiblingNode := TSubtitleRange(SiblingRange).Node;
    NewNode := vtvSubsList.InsertNode(SiblingNode,amInsertBefore);
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

  WAVDisplayer.SelectedRange := NewRange;

  MemoSubtitleText.SetFocus;
  CurrentProject.IsDirty := True;

  g_WebRWSynchro.EndWrite;
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

procedure TMainForm.LoadSubtitles(Filename: string; var IsUTF8 : Boolean);
var
  S,SS: string;
  BOM : array[0..3] of BYTE;
  i : integer;
  Start, Stop : Integer;
  Text : string;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  FS : TFileStream;
  EndOfFile : Boolean;
begin
  if not FileExists(Filename) then
    Exit;

  g_WebRWSynchro.BeginWrite;

  WAVDisplayer.RangeList.Clear;

  FS := TFileStream.Create(Filename,fmOpenRead);
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

  EndOfFile := False;
  while not EndOfFile do
  begin
    // Skip blank lines
    repeat
      EndOfFile := ReadLineStream(FS,S);
      S := Trim(S);
    until (S <> '') or EndOfFile;
    if (EndOfFile) then
      Break;
    // we need 1 number on 1 line
    if StrToIntDef(S,-1) = -1 then
      Break;
    // we need the timestamps line
    EndOfFile := ReadLineStream(FS,S);
    if (EndOfFile) then
      Break;
    i := Pos('-->',S);
    if i = 0 then
      Break;
    SS := Trim(Copy(S,1,i-1));
    Start := TimeStringToMs(SS);
    SS := Trim(Copy(S,i+3,Length(S)));
    Stop := TimeStringToMs(SS);
    // Copy text until a blank line
    Text := '';
    repeat
      EndOfFile := ReadLineStream(FS,S);
      S := Trim(S);
      Text := Text + S + '|';
    until (S = '') or EndOfFile;
    Delete(Text,Length(Text)-1,2); // remove '|' excess
    Text := Trim(Text);
    NewRange := SubRangeFactory.CreateRangeSS(Start,Stop);
    if IsUTF8 then
      TSubtitleRange(NewRange).Text := UTF8Decode(Text)
    else
      TSubtitleRange(NewRange).Text := Text;
    WAVDisplayer.RangeList.AddAtEnd(NewRange);
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
    0: CellText := TimeMsToString(NodeData.Range.StartTime);
    1: CellText := TimeMsToString(NodeData.Range.StopTime);
    2: CellText := NodeData.Range.Text;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Fix weird Windows mouse wheel handling behaviour :p
  if WAVDisplayer.RedirectedMousewheel(Sender,Shift,WheelDelta,MousePos) then
  begin
    Handled := True;
    Exit;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.vtvSubsListChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var NodeData: PTreeData;
begin
  if (Node <> nil) then
  begin
    NodeData := Sender.GetNodeData(Node);
    MemoSubtitleText.Text := StringConvertPipeToCRLF(NodeData.Range.Text);
    MemoSubtitleText.Tag := 1;
  end
  else
  begin
    MemoSubtitleText.Tag := 0;  
    MemoSubtitleText.Text := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.MemoSubtitleTextChange(Sender: TObject);
var NodeData: PTreeData;
begin
  if Assigned(vtvSubsList.FocusedNode) and (MemoSubtitleText.Tag = 1) then
  begin
    NodeData := vtvSubsList.GetNodeData(vtvSubsList.FocusedNode);
    g_WebRWSynchro.BeginWrite;
    NodeData.Range.Text := StringConvertCRLFToPipe(MemoSubtitleText.Text);
    vtvSubsList.RepaintNode(vtvSubsList.FocusedNode);
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;    
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

procedure TMainForm.SaveSubtitles(Filename: string; InUTF8 : Boolean);
var i : integer;
    SubRange : TSubtitleRange;
    FS : TFileStream;
const
    UTF8BOM : array[0..2] of BYTE = ($EF,$BB,$BF);

    procedure WriteStringLnStream(s : string; Stream : TStream);
    begin
      s := s + #13#10;
      Stream.Write(s[1],Length(s));
    end;
begin
  FS := TFileStream.Create(Filename,fmCreate);
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
      WriteStringLnStream(UTF8Encode(StringConvertPipeToCRLF(Subrange.Text)), FS)
    else
      WriteStringLnStream(StringConvertPipeToCRLF(Subrange.Text), FS);
    WriteStringLnStream('',FS);
  end;
  FS.Free;
  CurrentProject.IsDirty := False;
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

procedure TMainForm.LoadProject(Filename : string);
var
  ProjectFileIni : TIniFile;
  SaveCursor: TCursor;
begin
  {
  g_WebRWSynchro.BeginWrite;
  CurrentProject.Filename := '';
  g_WebRWSynchro.EndWrite;
  }

  if (not FileExists(Filename)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The project file "' + Filename +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    Exit;
  end;

  ActionClose.Execute;

  g_WebRWSynchro.BeginWrite;
  SaveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
      
    ProjectFileIni := TIniFile.Create(Filename);
    CurrentProject.Filename := Filename;
    CurrentProject.VideoSource := ProjectFileIni.ReadString('VisualSubsync','VideoSource','');
    CurrentProject.WAVFile := ProjectFileIni.ReadString('VisualSubsync','WAVFile','');
    CurrentProject.PeakFile := ProjectFileIni.ReadString('VisualSubsync','PeakFile','');
    CurrentProject.WAVMode := TProjectWAVMode(ProjectFileIni.ReadInteger('VisualSubsync','WAVMode',1));
    CurrentProject.SubtitlesFile := ProjectFileIni.ReadString('VisualSubsync','SubtitlesFile','');
    ProjectFileIni.Free;

    ShowStatusBarMessage('Loading WAV form...');
    if CurrentProject.WAVMode = pwmPeakOnly then
    begin
      if FileExists(CurrentProject.PeakFile) then
        WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile,'.wav'))
      else
      begin
        if FileExists(CurrentProject.WAVFile) then
        begin
          CurrentProject.WAVMode := pwmExternal;
          WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
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
      if FileExists(CurrentProject.WAVFile) then
        WAVDisplayer.LoadWAV(CurrentProject.WAVFile)
      else
      begin
        if FileExists(CurrentProject.PeakFile) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          WAVDisplayer.LoadWAV(ChangeFileExt(CurrentProject.PeakFile,'.wav'));
          CurrentProject.IsDirty := True;
          // Show warning
          MessageBoxW(Handle, PWideChar(WideString('WAV file ' + CurrentProject.WAVFile +
           ' hasn''t been found, project has been switched to "Peak file only" mode')),
            PWideChar(WideString('Warning')), MB_OK or MB_ICONWARNING);
        end
        else if FileExists(ChangeFileExt(CurrentProject.WAVFile,'.peak')) then
        begin
          CurrentProject.WAVMode := pwmPeakOnly;
          CurrentProject.PeakFile := ChangeFileExt(CurrentProject.WAVFile,'.peak');
          WAVDisplayer.LoadWAV(CurrentProject.WAVFile);
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
    WAVDisplayer.Enabled := True;    

    ShowStatusBarMessage('Loading subtitles...');
    LoadSubtitles(CurrentProject.SubtitlesFile,CurrentProject.IsUTF8);
    vtvSubsList.Repaint;

    ShowStatusBarMessage('Loading audio file...');
    if not AudioOnlyRenderer.Open(CurrentProject.WAVFile) then
    begin
      if AudioOnlyRenderer.Open(CurrentProject.VideoSource) then
        AudioOnlyRenderer.KillVideo;
    end;
    if PanelVideo.Visible then
      WAVDisplayer.SetRenderer(VideoRenderer)
    else
      WAVDisplayer.SetRenderer(AudioOnlyRenderer);

    ShowStatusBarMessage('Loading video file...');
    if VideoRenderer.Open(CurrentProject.VideoSource) then
    begin
      PanelVideo.Width := (VideoRenderer.VideoWidth * PanelVideo.Height) div
        VideoRenderer.VideoHeight;
      VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
    end;

    EnableControl(True);

    CurrentProjectOnDirtyChange(nil);
  finally
    Screen.Cursor := SaveCursor;
    if (not VideoRenderer.IsOpen) and PanelVideo.Visible then
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
var Range : TRange;
begin
  WAVDisplayer.AutoScrolling := True;
  if WAVDisplayer.SelectionIsEmpty then
  begin
    Range := TRange.Create;
    Range.StartTime := WAVDisplayer.GetCursorPos;
    Range.StopTime := WAVDisplayer.Length;
    WAVDisplayer.PlayRange(Range);
    Range.Free;
  end
  else
    WAVDisplayer.PlayRange(WAVDisplayer.Selection);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionStopExecute(Sender: TObject);
begin
  WAVDisplayer.Stop;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionLoopExecute(Sender: TObject);
var Range : TRange;
begin
  WAVDisplayer.AutoScrolling := True;
  if WAVDisplayer.SelectionIsEmpty then
  begin
    Range := TRange.Create;
    Range.StartTime := WAVDisplayer.GetCursorPos;
    Range.StopTime := WAVDisplayer.Length;
    WAVDisplayer.PlayRange(Range,True);
    Range.Free;
  end
  else
    WAVDisplayer.PlayRange(WAVDisplayer.Selection,True);
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
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionShowHideVideoExecute(Sender: TObject);
var IsPlaying : Boolean;
    PlayingPos : Integer;
begin
  if (not VideoRenderer.IsOpen) and (not PanelVideo.Visible) then
  begin
    ShowStatusBarMessage('Video file is not open...');
    Exit;
  end;

  PlayingPos := 0;
  IsPlaying := WAVDisplayer.IsPlaying;
  if IsPlaying then
    PlayingPos := WAVDisplayer.GetPlayCursorPos;
  PanelVideo.Visible := not PanelVideo.Visible;
  SplitterWAVDisplay_Video.Visible := not SplitterWAVDisplay_Video.Visible;
  if PanelVideo.Visible then
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
    SaveSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
  SaveProject(CurrentProject);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
begin
  TntSaveDialog1.Filter := 'Subtitles files (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  if TntSaveDialog1.Execute then
  begin
    SaveSubtitles(TntSaveDialog1.FileName, CurrentProject.IsUTF8);
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
      PanelVideo.Width := (VideoRenderer.VideoWidth * PanelVideo.Height) div VideoRenderer.VideoHeight;
      VideoRenderer.SetDisplayWindow(PanelVideo.Handle);
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

    if (ProjectHasChanged) then
    begin
      SaveProject(CurrentProject);
    end;

    if (not VideoRenderer.IsOpen) and PanelVideo.Visible then
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

procedure TMainForm.ActionCheckErrorsExecute(Sender: TObject);
var i, j, CPS, LineLen, MaxLineLen, TotalCharCount : Integer;
    SubRange : TSubtitleRange;
    Msg : WideString;
begin
  // TODO : ignore tags in stats
  
  ErrorReportForm.Clear;
  ErrorReportForm.vtvErrorList.BeginUpdate;
  for i := 0 to WAVDisplayer.RangeList.Count-1 do
  begin
    SubRange := TSubtitleRange(WAVDisplayer.RangeList[i]);
    // Overlapping test
    if ConfigObject.ErrorOverlappingEnabled and
       (i < WAVDisplayer.RangeList.Count-1) then
    begin
      if (SubRange.StopTime >= WAVDisplayer.RangeList[i+1].StartTime) then
        ErrorReportForm.AddError(etOverlapping, SubRange, '');
    end;

    // Prepare stats for too short, too long...
    TotalCharCount := 0;
    LineLen := 0;
    MaxLineLen := 0;
    for j := 1 to Length(SubRange.Text) do
    begin
      if SubRange.Text[j] = '|' then
      begin
        if (LineLen > MaxLineLen) then
          MaxLineLen := LineLen;
        LineLen := 0;
        Inc(TotalCharCount,2); // new line count twice :)
      end
      else
      begin
        Inc(LineLen);
        Inc(TotalCharCount);
      end;
    end;
    if (LineLen > MaxLineLen) then
      MaxLineLen := LineLen;
    
    CPS := Round(TotalCharCount / ((SubRange.StopTime - SubRange.StartTime) / 1000));

    if ConfigObject.ErrorTooShortDisplayTimeEnabled and
       (CPS > ConfigObject.ErrorTooShortDisplayTimeValue) then
      ErrorReportForm.AddError(etTooShort, SubRange, IntToStr(CPS)+' Char/s');

    if ConfigObject.ErrorTooLongDisplayTimeEnabled and
      (CPS < ConfigObject.ErrorTooLongDisplayTimeValue) then
      ErrorReportForm.AddError(etTooLong, SubRange, IntToStr(CPS)+' Char/s');

    if ConfigObject.ErrorTooLongLineEnabled and
      (MaxLineLen > ConfigObject.ErrorTooLongLineValue) then
      ErrorReportForm.AddError(etLineTooLong, SubRange, IntToStr(MaxLineLen)+' Char.');
  end;
  ErrorReportForm.vtvErrorList.EndUpdate;
  if (ErrorReportForm.vtvErrorList.TotalCount > 0) then
  begin
    ErrorReportForm.Visible := True;
    Msg := IntToStr(ErrorReportForm.vtvErrorList.TotalCount)+' error(s) found.';
  end
  else
    Msg := 'No error found.';
  ShowStatusBarMessage(Msg);
  ErrorReportForm.TntStatusBar1.Panels[0].Text := Msg;
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
begin
  DelayForm := TDelayForm.Create(nil);
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
            NodeData.Range.StartTime := NodeData.Range.StartTime + DelayInMs;
            NodeData.Range.StopTime := NodeData.Range.StopTime + DelayInMs;
            Node := vtvSubsList.GetNext(Node);
          end;
        end;
      1: // Selected subs
        begin
          Node := vtvSubsList.GetFirstSelected;
          while Assigned(Node) do
          begin
            NodeData := vtvSubsList.GetNodeData(Node);
            NodeData.Range.StartTime := NodeData.Range.StartTime + DelayInMs;
            NodeData.Range.StopTime := NodeData.Range.StopTime + DelayInMs;
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
            NodeData.Range.StartTime := NodeData.Range.StartTime + DelayInMs;
            NodeData.Range.StopTime := NodeData.Range.StopTime + DelayInMs;
            Node := vtvSubsList.GetNext(Node);
          end;
          FullSortTreeAndSubList;
        end;
    end;
    CurrentProject.IsDirty := True;
    g_WebRWSynchro.EndWrite;    
    WAVDisplayer.UpdateView([uvfRange]);
  end;
  DelayForm.Free;
  DelayForm := nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PanelVideoResize(Sender: TObject);
begin
  VideoRenderer.UpdateDisplayWindow;
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
    NodeData.Range.Free;
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
  NodeData.Range.Text := StringConvertCRLFToPipe(Trim(AccuText));;
  NodeData.Range.StopTime := LastStopTime;

  // Finally delete nodes
  for i := 0 to DeleteList.Count-1 do
  begin
    Node := DeleteList[i];
    NodeData := vtvSubsList.GetNodeData(Node);
    ErrorReportForm.DeleteError(NodeData.Range);
    Idx := WAVDisplayer.RangeList.IndexOf(NodeData.Range);
    WAVDisplayer.DeleteRangeAtIdx(Idx,False);
    NodeData.Range.Free;
    vtvSubsList.DeleteNode(Node);
  end;

  CurrentProject.IsDirty := True;
  Node := vtvSubsList.GetFirstSelected;
  if Assigned(WAVDisplayer.SelectedRange) then
    WAVDisplayer.SelectedRange := nil;
  WAVDisplayer.ClearSelection;
  WAVDisplayer.UpdateView([uvfRange,uvfSelection]);
  vtvSubsList.FocusedNode := Node;
  vtvSubsListChange(vtvSubsList, Node);
  vtvSubsList.Repaint;

  g_WebRWSynchro.EndWrite;
end;

//------------------------------------------------------------------------------

procedure TMainForm.SubListPopupMenuPopup(Sender: TObject);
begin
  pmiSubListDelete.Enabled := vtvSubsList.SelectedCount > 0;
  pmiSubListMerge.Enabled := vtvSubsList.SelectedCount > 1;
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
      SaveSubtitles(CurrentProject.SubtitlesFile, CurrentProject.IsUTF8);
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
  vtvSubsListChange(vtvSubsList, Node);
  vtvSubsList.ScrollIntoView(Node,True);
  vtvSubsListDblClick(Self);
  vtvSubsList.Repaint;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionNextSubExecute(Sender: TObject);
var CurrentNode, NextNode : PVirtualNode;
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
    ActionPlay.Execute;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ActionPreviousSubExecute(Sender: TObject);
var CurrentNode, PreviousNode : PVirtualNode;
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
      Idx := StrToIntDef(GotoForm.EditLine.Text,0) - 1;
      Constrain(Idx, 0, WAVDisplayer.RangeList.Count-1);
      SubRange := TSubtitleRange(WAVDisplayer.RangeList[Idx]);
    end
    else if GotoForm.rbGotoTime.Checked then
    begin
      GotoPositionMs := TimeStringToMs(GotoForm.MaskEditTime.EditText);
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
begin
  PreferencesForm.LoadConfig(ConfigObject);
  if TabSheet <> nil then
    PreferencesForm.TntPageControl1.ActivePage := TabSheet;
  if (PreferencesForm.ShowModal = mrOk) then
  begin
    PreferencesForm.SaveConfig(ConfigObject);
    if Assigned(Server) then
      Server.EnableCompression := ConfigObject.EnableCompression;
  end;
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

    SuggestionForm.Clear;
    ErrorReportForm.Clear;
    vtvSubsList.Clear;

    WAVDisplayer.RangeList.Clear;
    WAVDisplayer.Enabled := False;
    WAVDisplayer.Close;
    WAVDisplayer.Repaint;

    CurrentProject.Filename := '';
    CurrentProject.VideoSource := '';
    CurrentProject.WAVFile := '';
    CurrentProject.PeakFile := '';
    CurrentProject.SubtitlesFile := '';
    Self.Caption := ApplicationName;
    MemoLinesCounter.Text := '';
    TntStatusBar1.Panels[0].Text := '';

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
var HelpFilename : string;
begin
  HelpFilename := ExtractFilePath(Application.ExeName);
  HelpFilename := IncludeTrailingPathDelimiter(HelpFilename);
  HelpFilename := HelpFilename + 'help\index.html';
  ShellExecute(0, 'open', PAnsiChar(HelpFilename), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.MenuItemHelpIndexWAVDisplayControlClick(
  Sender: TObject);
var HelpFilename : string;
begin
  HelpFilename := ExtractFilePath(Application.ExeName);
  HelpFilename := IncludeTrailingPathDelimiter(HelpFilename);
  HelpFilename := HelpFilename + 'help\wavdisplaycontrol.html';
  ShellExecute(0, 'open', PAnsiChar(HelpFilename), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TMainForm.pmiWAVDispSetSubtitleTimeClick(Sender: TObject);
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

procedure TMainForm.ActionInsertTextFileExecute(Sender: TObject);
var
  FS : TFileStream;
  BOM : array[0..3] of BYTE;
  Line: string;
  NewRange : TRange;
  Node: PVirtualNode;
  NodeData: PTreeData;
  IsUTF8 : Boolean;
begin
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

    while (not ReadLineStream(FS,Line)) do
    begin
      if IsUTF8 then
        Line := UTF8Decode(Text);
      NewRange := SubRangeFactory.CreateRangeSS(359999999,359999999);
      TSubtitleRange(NewRange).Text := Line;
      WAVDisplayer.RangeList.AddAtEnd(NewRange);

      Node := vtvSubsList.AddChild(nil);
      NodeData := vtvSubsList.GetNodeData(Node);
      NodeData.Range := TSubtitleRange(NewRange);
      TSubtitleRange(NewRange).Node := Node;
    end;
    FS.Free;
  end;
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
