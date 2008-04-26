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

unit PreferencesFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, CheckLst,
  TntCheckLst, IniFiles, ExtCtrls, TntExtCtrls, TntActnList, Menus,
  WAVDisplayerUnit, VirtualTrees, JavaScriptPluginUnit;

const
  WM_ENDEDITING = WM_USER + 736;

type
  PParamData = ^TParamData;
  TParamData = record
    Param : PJSPluginParam;
    ValueAsString : WideString;
    Changed: Boolean;
  end;

  TJSPluginInfo = class
    Enabled : Boolean;
    Name : WideString;
    Description : WideString;
    Msg: WideString;
    Color : TColor;
    ParamList : TList;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Value : TJSPluginInfo);
    procedure ClearParam;
    function GetParamByName(Name : WideString) : PParamData;
  end;

type
  TDefaultActionShortcut = record
    ActionName : string;
    ShortCut : string;
  end;

type
  TConfigObject = class
  private
    procedure ClearJSPluginInfoList;
  public
    // Web server
    ServerPort : Integer;
    EnableCompression : Boolean;
    // Misc
    SwapSubtitlesList : Boolean;
    DisableSubtitleEdition : Boolean;
    EnableToggleCreation : Boolean;
    EnableMouseAntiOverlapping : Boolean;
    EnableMouseSnapping : Boolean;
    SpaceKeyModifyTiming : Boolean;
    SpaceKeyCPSTarget : Integer;
    SpaceKeyMinimalDuration : Integer;
    SpaceKeyBlankBetweenSubtitles : Integer;
    // Hotkeys
    ListHotkeys : TList;
    ListDefaultHotkeys : TList;
    // Mouse
    MouseWheelTimeScrollModifier : TMouseWheelModifier;
    MouseWheelVZoomModifier : TMouseWheelModifier;
    MouseWheelHZoomModifier : TMouseWheelModifier;
    MouseEnableSSATimingMode : Boolean;
    // Backup
    EnableBackup : Boolean;
    AutoBackupEvery : Integer;
    AutoSaveWhenPlaying : Boolean;
    // Plugins
    ListJSPlugin : TList;
    // Fonts
    SubListFont : string;
    SubTextFont : string;
    // WAV Display
    ShowSceneChange : Boolean;
    SceneChangeStartOffset : Integer;
    SceneChangeStopOffset : Integer;
    SceneChangeFilterOffset : Integer; 
    ShowTextInWAVDisplay : Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure SetDefault;
    procedure SaveIni(IniFile : TIniFile);
    procedure LoadIni(IniFile : TIniFile; IsPresets : Boolean);
    procedure SetDefaultHotKeys(ActionList : TTntActionList);
    procedure UpdatePluginList;
    function GetJSPluginInfoByName(Name : WideString) : TJSPluginInfo;
    procedure ApplyParam(JSPlugin : TJavaScriptPlugin);
  end;

  THotkeyListItemData = class
    Action : TTntAction;
    NormalShortCut : TShortCut;
    TimingShortCut : TShortCut;
    procedure Assign(Source : THotkeyListItemData);
  end;

  TPreferencesForm = class(TForm)
    PageControlPreferences: TTntPageControl;
    tsGeneral: TTntTabSheet;
    tsErrorChecking: TTntTabSheet;
    GroupBoxWebServer: TTntGroupBox;
    EditServerPort: TTntEdit;
    TntLabel1: TTntLabel;
    ListErrorChecking: TTntCheckListBox;
    UpDownServerPort: TTntUpDown;
    GroupBoxMisc: TGroupBox;
    chkAssociateExt: TCheckBox;
    chkEnableCompression: TCheckBox;
    chkSwapSubList: TCheckBox;
    tsHotKeys: TTntTabSheet;
    ListHotkeys: TTntListView;
    tsMouse: TTntTabSheet;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboWheelTimeScrollModifier: TComboBox;
    ComboWheelVZoomModifier: TComboBox;
    ComboWheelHZoomModifier: TComboBox;
    GroupBox3: TGroupBox;
    chkEnableSSATimingMode: TCheckBox;
    GroupBoxBackup: TGroupBox;
    chkCreateBackup: TCheckBox;
    bttOpenBackupDir: TButton;
    EditBackupTime: TEdit;
    Label6: TLabel;
    UpDownBackupTime: TUpDown;
    Panel1: TPanel;
    bttOk: TTntButton;
    bttCancel: TTntButton;
    Panel2: TPanel;
    TntLabel3: TTntLabel;
    lbErrorDescription: TTntLabel;
    TntLabel4: TTntLabel;
    ShapeErrorColor: TShape;
    Bevel1: TBevel;
    ListPluginParam: TVirtualStringTree;
    tsFonts: TTntTabSheet;
    FontDialog1: TFontDialog;
    TntGroupBox2: TTntGroupBox;
    EditSubListFont: TTntEdit;
    bttSubListFont: TTntButton;
    TntGroupBox3: TTntGroupBox;
    EditSubTextFont: TTntEdit;
    bttSubTextFont: TTntButton;
    chkAutoSaveWhenPlaying: TCheckBox;
    chkEnableMouseAntiOverlapping: TCheckBox;
    chkEnableMouseSnapping: TCheckBox;
    tsTimingMode: TTntTabSheet;
    TntGroupBox4: TTntGroupBox;
    chkEnableSubCreationWithSpaceKey: TCheckBox;
    chkSpaceKeyModifyTiming: TCheckBox;
    chkDisableSubEditionInTimingMode: TCheckBox;
    bttOpenBackupTempDir: TButton;
    tsWAVDisplay: TTntTabSheet;
    TntGroupBox5: TTntGroupBox;
    chkSceneChange: TCheckBox;
    EditSCStartOffset: TEdit;
    UpDownSCStart: TTntUpDown;
    EditSCStopOffset: TEdit;
    UpDownSCStop: TTntUpDown;
    TntGroupBox6: TTntGroupBox;
    chkShowTextInWAVDisplay: TCheckBox;
    TntLabel9: TTntLabel;
    TntLabel10: TTntLabel;
    TntLabel11: TTntLabel;
    TntLabel12: TTntLabel;
    EditSCFilterOffset: TEdit;
    UpDownSCFilter: TTntUpDown;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    tsSubtitle: TTntTabSheet;
    TntGroupBox1: TTntGroupBox;
    TntLabel8: TTntLabel;
    EditBlankBetweenSub: TTntEdit;
    UpDownBlankBetweenSub: TTntUpDown;
    TntLabel13: TTntLabel;
    TntLabel2: TTntLabel;
    EditCPSTarget: TTntEdit;
    UpDownCPSTarget: TTntUpDown;
    TntLabel7: TTntLabel;
    EditMinimalDuration: TTntEdit;
    UpDownMinimalDuration: TTntUpDown;
    pmErrorChecking: TPopupMenu;
    pmiSelectAll: TMenuItem;
    pmiUnselectAll: TMenuItem;
    Panel3: TPanel;
    TntLabel5: TTntLabel;
    ComboHotkeyMode: TTntComboBox;
    HotKey1: THotKey;
    TntLabel6: TTntLabel;
    bttResetAllHotkeys: TTntButton;
    bttSetHotkey: TTntButton;
    bttClearHotkey: TTntButton;
    Bevel9: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure bttOkClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure ListErrorCheckingClick(Sender: TObject);
    procedure chkAssociateExtClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ListHotkeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ComboHotkeyModeSelect(Sender: TObject);
    procedure bttSetHotkeyClick(Sender: TObject);
    procedure bttClearHotkeyClick(Sender: TObject);
    procedure ListHotkeysDeletion(Sender: TObject; Item: TListItem);
    procedure bttResetAllHotkeysClick(Sender: TObject);
    procedure bttOpenBackupDirClick(Sender: TObject);
    procedure ListPluginParamGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ListPluginParamEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ListPluginParamCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure FormDestroy(Sender: TObject);
    procedure ListErrorCheckingClickCheck(Sender: TObject);
    procedure bttSubListFontClick(Sender: TObject);
    procedure bttSubTextFontClick(Sender: TObject);
    procedure bttOpenBackupTempDirClick(Sender: TObject);
    procedure ListPluginParamGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure pmiSelectAllClick(Sender: TObject);
    procedure pmiUnselectAllClick(Sender: TObject);

  private
    { Private declarations }
    ListDefaultHotkeys : TList;

    function GetCurrentModeShortCut(HLID : THotkeyListItemData) : TShortCut;
    function GetCurrentModeShortCutFromList : TShortCut;
    procedure SetCurrentModeShortCut(HLID : THotkeyListItemData; ShortCut : TShortCut);
    procedure SetCurrentModeShortCutFromList(ShortCut : TShortCut);
    procedure WMEndEditing(var Message: TMessage); message WM_ENDEDITING;
    procedure ClearErrorList;
  public
    { Public declarations }
    procedure LoadConfig(Config : TConfigObject);
    procedure SaveConfig(Config : TConfigObject);
  end;

  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEditControl: TWinControl; // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditIntegerKeyPress(Sender: TObject; var Key: Char);
    procedure EditFloatKeyPress(Sender: TObject; var Key: Char);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  function PreferencesFormInstance : TPreferencesForm;

implementation

uses MiscToolsUnit, GlobalUnit, ActnList, TntWindows, TntSysUtils,
  TntForms, Mask, TntClasses, LogWindowFormUnit;

{$R *.dfm}

const
  DefaultTimingShortcuts : array[0..5] of TDefaultActionShortcut = (
    (ActionName: 'ActionStop'; ShortCut: 'Esc'),
    (ActionName: 'ActionPlay'; ShortCut: 'F1'),
    (ActionName: 'ActionShowHideVideo'; ShortCut: 'F4'),
    (ActionName: 'ActionSave'; ShortCut: 'Ctrl+S'),
    (ActionName: 'ActionUndo'; ShortCut: 'Ctrl+Z'),
    (ActionName: 'ActionRedo'; ShortCut: 'Ctrl+Y')
  );

var
  PreferencesForm: TPreferencesForm = nil;

// =============================================================================

function PreferencesFormInstance : TPreferencesForm;
begin
  if (PreferencesForm = nil) then
    Application.CreateForm(TPreferencesForm, PreferencesForm);
  Result := PreferencesForm;
end;

// =============================================================================

procedure THotkeyListItemData.Assign(Source : THotkeyListItemData);
begin
  Action := Source.Action;
  NormalShortCut := Source.NormalShortCut;
  TimingShortCut := Source.TimingShortCut;
end;

// =============================================================================

constructor TJSPluginInfo.Create;
begin
  ParamList := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TJSPluginInfo.Destroy;
begin
  ClearParam;
  ParamList.Free;
end;

//------------------------------------------------------------------------------

procedure TJSPluginInfo.ClearParam;
var i : Integer;
begin
  for i:=0 to ParamList.Count-1 do
    Dispose(PJSPluginParam(ParamList[i]));
  ParamList.Clear;
end;

//------------------------------------------------------------------------------

procedure TJSPluginInfo.Assign(Value : TJSPluginInfo);
var i : Integer;
    pParamSrc, pParamDst : PJSPluginParam;
begin
  Self.Enabled := Value.Enabled;
  Self.Name := Value.Name;
  Self.Description := Value.Description;
  Self.Msg := Value.Msg;
  Self.Color := Value.Color;
  ClearParam;
  for i:=0 to Value.ParamList.Count-1 do
  begin
    pParamSrc := Value.ParamList[i];
    pParamDst := New(PJSPluginParam);
    pParamDst^ := pParamSrc^;
    Self.ParamList.Add(pParamDst);
  end;
end;

//------------------------------------------------------------------------------

function TJSPluginInfo.GetParamByName(Name : WideString) : PParamData;
var i : Integer;
begin
  Result := nil;
  for i:=0 to ParamList.Count-1 do
  begin
    if PJSPluginParam(ParamList[i]).Name = Name then
    begin
      Result := ParamList[i];
      Exit;
    end;
  end;
end;

// =============================================================================

constructor TConfigObject.Create;
begin
  ListHotkeys := TList.Create;
  ListDefaultHotkeys := TList.Create;
  ListJSPlugin := TList.Create;
  SetDefault;
end;

//------------------------------------------------------------------------------

destructor TConfigObject.Destroy;
var i : integer;
begin
  for i:= 0 to ListHotkeys.Count-1 do
    THotkeyListItemData(ListHotkeys[i]).Free;
  ListHotkeys.Free;

  for i:= 0 to ListDefaultHotkeys.Count-1 do
    THotkeyListItemData(ListDefaultHotkeys[i]).Free;
  ListDefaultHotkeys.Free;

  ClearJSPluginInfoList;
  ListJSPlugin.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TConfigObject.GetJSPluginInfoByName(Name : WideString) : TJSPluginInfo;
var i : Integer;
begin
  Result := nil;
  for i:=0 to ListJSPlugin.Count-1 do
  begin
    if TJSPluginInfo(ListJSPlugin[i]).Name = Name then
    begin
      Result := ListJSPlugin[i];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.ApplyParam(JSPlugin : TJavaScriptPlugin);
var JSPluginInfo : TJSPluginInfo;
    pParam : PJSPluginParam;
    i : Integer;
begin
  JSPluginInfo := GetJSPluginInfoByName(JSPlugin.Name);
  if Assigned(JSPluginInfo) then
  begin
    for i:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pParam := JSPluginInfo.ParamList[i];
      JSPlugin.SetParamValue(pParam.Name, GetParamValueAsWideString(pParam));
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SetDefault;
begin
  // Misc
  SwapSubtitlesList := False;
  DisableSubtitleEdition := True;
  EnableToggleCreation := True;
  EnableMouseAntiOverlapping := False;
  EnableMouseSnapping := True;
  SpaceKeyModifyTiming := True;
  SpaceKeyCPSTarget := 18;
  SpaceKeyMinimalDuration := 1000;
  SpaceKeyBlankBetweenSubtitles := 120;
  // Web server
  ServerPort := 80;
  EnableCompression := False; // Some IE version doesn't support deflate but say they does :p
  // Error
  UpdatePluginList;
  // Mouse
  MouseWheelTimeScrollModifier := mwmCtrl;
  MouseWheelVZoomModifier := mwmShift;
  MouseWheelHZoomModifier := mwmNone;
  MouseEnableSSATimingMode := False;
  // Backup
  EnableBackup := True;
  AutoBackupEvery := 0;
  AutoSaveWhenPlaying := False;
  // Fonts
  SubListFont := 'Arial,8,0,0,clWindowText';
  SubTextFont := 'Arial,10,1,0,clWindowText';
  // WAV Display
  ShowSceneChange := False;
  SceneChangeStartOffset := 130;
  SceneChangeStopOffset := 130;
  ShowTextInWAVDisplay := True;
  SceneChangeFilterOffset := 250;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.UpdatePluginList;
var JSPEnum : TJavaScriptPluginEnumerator;
    JSPlugin : TJavaScriptPlugin;
    JSPluginInfo : TJSPluginInfo;
begin
  JSPEnum := TJavaScriptPluginEnumerator.Create(g_PluginPath);
  JSPEnum.OnJSPluginError := LogForm.LogMsg;
  JSPEnum.Reset;
  while JSPEnum.GetNext(JSPlugin) do
  begin
    JSPluginInfo := TJSPluginInfo.Create;
    JSPluginInfo.Enabled := True;
    JSPluginInfo.Name := JSPlugin.Name;
    JSPluginInfo.Description := JSPlugin.Description;
    JSPluginInfo.Color := JSColorToTColor(JSPlugin.Color);
    JSPluginInfo.Msg := JSPlugin.Msg;
    JSPlugin.FillParamList(JSPluginInfo.ParamList);
    ListJSPlugin.Add(JSPluginInfo);
    FreeAndNil(JSPlugin);
  end;
  JSPEnum.Free;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.ClearJSPluginInfoList;
var i : Integer;
begin
  for i:=0 to ListJSPlugin.Count-1 do
    TJSPluginInfo(ListJSPlugin[i]).Free;
  ListJSPlugin.Clear;
end;

//------------------------------------------------------------------------------

function GetDefaultShortcut(ActionName : string; ArraySC : array of TDefaultActionShortcut) : TShortCut;
var i : integer;
begin
  Result := 0;
  for i:=0 to Length(ArraySC)-1 do
  begin
    if (ActionName = ArraySC[i].ActionName) then
    begin
      Result := TextToShortCut(ArraySC[i].ShortCut);
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SetDefaultHotKeys(ActionList : TTntActionList);
var i : integer;
    Action : TTntAction;
    HLID : THotkeyListItemData;
    SC : TShortCut;
begin
  for i:=0 to ActionList.ActionCount-1 do
  begin
    Action := TTntAction(ActionList.Actions[i]);
    if (Action.Tag = 1) then
    begin
      SC := GetDefaultShortcut(Action.Name, DefaultTimingShortcuts);

      HLID := THotkeyListItemData.Create;
      HLID.Action := Action;
      HLID.NormalShortCut := Action.ShortCut;
      HLID.TimingShortCut := SC;
      ListHotkeys.Add(HLID);

      HLID := THotkeyListItemData.Create;
      HLID.Action := Action;
      HLID.NormalShortCut := Action.ShortCut;
      HLID.TimingShortCut := SC;
      ListDefaultHotkeys.Add(HLID);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SaveIni(IniFile : TIniFile);
var i, j : integer;
    HLID : THotkeyListItemData;
    JSPluginInfo : TJSPluginInfo;
    pPluginParam : PJSPluginParam;
begin
  // Misc
  IniFile.WriteBool('Misc','SwapSubtitlesList',SwapSubtitlesList);
  IniFile.WriteBool('Misc','DisableSubtitleEdition',DisableSubtitleEdition);
  IniFile.WriteBool('Misc','EnableToggleCreation',EnableToggleCreation);
  IniFile.WriteBool('Misc','SpaceKeyModifyTiming',SpaceKeyModifyTiming);
  IniFile.WriteInteger('Misc','SpaceKeyCPSTarget',SpaceKeyCPSTarget);
  IniFile.WriteInteger('Misc','SpaceKeyMinimalDuration',SpaceKeyMinimalDuration);
  IniFile.WriteBool('Misc','EnableMouseAntiOverlapping',EnableMouseAntiOverlapping);
  IniFile.WriteBool('Misc','EnableMouseSnapping',EnableMouseSnapping);
  IniFile.WriteInteger('Misc','SpaceKeyBlankBetweenSubtitles',SpaceKeyBlankBetweenSubtitles);

  // Web server
  IniFile.WriteInteger('WebServer','Port',ServerPort);
  IniFile.WriteBool('WebServer','EnableCompression',EnableCompression);

  // Error plugin
  for i:=0 to ListJSPlugin.Count-1 do
  begin
    JSPluginInfo := ListJSPlugin[i];
    IniFile.WriteBool(JSPluginInfo.Name, 'Enabled', JSPluginInfo.Enabled);
    for j:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pPluginParam := JSPluginInfo.ParamList[j];
      case pPluginParam.ParamType of
        jsptBoolean: IniFile.WriteBool(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.BooleanValue);
        jsptInteger: IniFile.WriteInteger(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.IntegerValue);
        jsptDouble: IniFile.WriteFloat(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.DoubleValue);
        jsptWideString: IniFile.WriteString(JSPluginInfo.Name, pPluginParam.Name, UTF8Encode(pPluginParam.WideStringValue));
      end;
    end;
  end;

  // Hotkeys
  IniFile.WriteBool('Hotkeys', 'UseIntegerShortcut', True);
  for i:=0 to ListHotkeys.Count-1 do
  begin
    HLID := ListHotkeys[i];
    IniFile.WriteInteger(
      'Hotkeys',
      HLID.Action.Name + '[Normal]',
      HLID.NormalShortCut);
    IniFile.WriteInteger(
      'Hotkeys',
      HLID.Action.Name + '[Timing]',
      HLID.TimingShortCut);
  end;

  // Mouse
  IniFile.WriteInteger('Mouse','WheelTimeScrollModifier',Ord(MouseWheelTimeScrollModifier));
  IniFile.WriteInteger('Mouse','WheelVZoomModifier',Ord(MouseWheelVZoomModifier));
  IniFile.WriteInteger('Mouse','WheelHZoomModifier',Ord(MouseWheelHZoomModifier));
  IniFile.WriteBool('Mouse','EnableSSATimingMode',MouseEnableSSATimingMode);

  // Backup
  IniFile.WriteBool('Backup','EnableBackup',EnableBackup);
  IniFile.WriteInteger('Backup','AutoBackupEvery',AutoBackupEvery);
  IniFile.WriteBool('Backup','AutoSaveWhenPlaying',AutoSaveWhenPlaying);

  // Fonts
  IniFile.WriteString('Fonts', 'SubList', SubListFont);
  IniFile.WriteString('Fonts', 'SubText', SubTextFont);

  // WAV Display
  IniFile.WriteBool('WAVDisplay', 'ShowSceneChange', ShowSceneChange);
  IniFile.WriteInteger('WAVDisplay', 'SceneChangeStartOffset', SceneChangeStartOffset);
  IniFile.WriteInteger('WAVDisplay', 'SceneChangeStopOffset', SceneChangeStopOffset);
  IniFile.WriteInteger('WAVDisplay', 'SceneChangeFilterOffset', SceneChangeFilterOffset);
  IniFile.WriteBool('WAVDisplay', 'ShowTextInWAVDisplay', ShowTextInWAVDisplay);
end;

//------------------------------------------------------------------------------

procedure TConfigObject.LoadIni(IniFile : TIniFile; IsPresets : Boolean);
var i, j : integer;
    HLID : THotkeyListItemData;
    JSPluginInfo : TJSPluginInfo;
    pPluginParam : PJSPluginParam;
begin
  // Misc
  SwapSubtitlesList := IniFile.ReadBool('Misc','SwapSubtitlesList',SwapSubtitlesList);
  DisableSubtitleEdition := IniFile.ReadBool('Misc','DisableSubtitleEdition',DisableSubtitleEdition);
  EnableToggleCreation := IniFile.ReadBool('Misc','EnableToggleCreation',EnableToggleCreation);
  SpaceKeyModifyTiming := IniFile.ReadBool('Misc','SpaceKeyModifyTiming',SpaceKeyModifyTiming);
  SpaceKeyCPSTarget := IniFile.ReadInteger('Misc','SpaceKeyCPSTarget',SpaceKeyCPSTarget);
  SpaceKeyMinimalDuration := IniFile.ReadInteger('Misc','SpaceKeyMinimalDuration',SpaceKeyMinimalDuration);
  EnableMouseAntiOverlapping := IniFile.ReadBool('Misc','EnableMouseAntiOverlapping',EnableMouseAntiOverlapping);
  EnableMouseSnapping := IniFile.ReadBool('Misc','EnableMouseSnapping',EnableMouseSnapping);
  SpaceKeyBlankBetweenSubtitles := IniFile.ReadInteger('Misc','SpaceKeyBlankBetweenSubtitles',SpaceKeyBlankBetweenSubtitles);

  // Web server
  if (not IsPresets) then
  begin
    ServerPort := IniFile.ReadInteger('WebServer','Port',ServerPort);
    EnableCompression := IniFile.ReadBool('WebServer','EnableCompression',EnableCompression);
  end;

  // Error plugin
  for i:=0 to ListJSPlugin.Count-1 do
  begin
    JSPluginInfo := ListJSPlugin[i];
    JSPluginInfo.Enabled := IniFile.ReadBool(JSPluginInfo.Name, 'Enabled', JSPluginInfo.Enabled);
    for j:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pPluginParam := JSPluginInfo.ParamList[j];
      case pPluginParam.ParamType of
        jsptBoolean: pPluginParam.BooleanValue := IniFile.ReadBool(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.BooleanValue);
        jsptInteger: pPluginParam.IntegerValue := IniFile.ReadInteger(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.IntegerValue);
        jsptDouble: pPluginParam.DoubleValue := IniFile.ReadFloat(JSPluginInfo.Name, pPluginParam.Name, pPluginParam.DoubleValue);
        jsptWideString: pPluginParam.WideStringValue := UTF8Decode(IniFile.ReadString(JSPluginInfo.Name, pPluginParam.Name, UTF8Encode(pPluginParam.WideStringValue)));
      end;
    end;
  end;

  // Hotkeys
  if IniFile.ReadBool('Hotkeys', 'UseIntegerShortcut', False) = True then
  begin
    for i:=0 to ListHotkeys.Count-1 do
    begin
      HLID := ListHotkeys[i];
      HLID.NormalShortCut := IniFile.ReadInteger('Hotkeys',
          HLID.Action.Name + '[Normal]',
          HLID.NormalShortCut);
      HLID.TimingShortCut := IniFile.ReadInteger('Hotkeys',
          HLID.Action.Name + '[Timing]',
          HLID.TimingShortCut);
    end;
  end
  else
  begin
    for i:=0 to ListHotkeys.Count-1 do
    begin
      HLID := ListHotkeys[i];
      HLID.NormalShortCut := TextToShortCut(
        IniFile.ReadString('Hotkeys',
          HLID.Action.Name + '[Normal]',
          ShortCutToText(HLID.NormalShortCut)));
      HLID.TimingShortCut := TextToShortCut(
        IniFile.ReadString('Hotkeys',
          HLID.Action.Name + '[Timing]',
          ShortCutToText(HLID.TimingShortCut)));
    end;
  end;

  // Mouse
  MouseWheelTimeScrollModifier := TMouseWheelModifier(IniFile.ReadInteger('Mouse',
    'WheelTimeScrollModifier',Ord(MouseWheelTimeScrollModifier)));
  MouseWheelVZoomModifier := TMouseWheelModifier(IniFile.ReadInteger('Mouse',
    'WheelVZoomModifier',Ord(MouseWheelVZoomModifier)));
  MouseWheelHZoomModifier := TMouseWheelModifier(IniFile.ReadInteger('Mouse',
    'WheelHZoomModifier',Ord(MouseWheelHZoomModifier)));
  MouseEnableSSATimingMode := IniFile.ReadBool('Mouse',
    'EnableSSATimingMode',MouseEnableSSATimingMode);

  if (not IsPresets) then
  begin
    // Backup
    EnableBackup := IniFile.ReadBool('Backup','EnableBackup',EnableBackup);
    AutoBackupEvery := IniFile.ReadInteger('Backup','AutoBackupEvery',AutoBackupEvery);
    AutoSaveWhenPlaying := IniFile.ReadBool('Backup','AutoSaveWhenPlaying',AutoSaveWhenPlaying);

    // Fonts
    SubListFont := IniFile.ReadString('Fonts', 'SubList', SubListFont);
    SubTextFont := IniFile.ReadString('Fonts', 'SubText', SubTextFont);
  end;

  // WAV Display
  ShowSceneChange := IniFile.ReadBool('WAVDisplay', 'ShowSceneChange', ShowSceneChange);
  SceneChangeStartOffset := IniFile.ReadInteger('WAVDisplay','SceneChangeStartOffset',SceneChangeStartOffset);
  SceneChangeStopOffset := IniFile.ReadInteger('WAVDisplay','SceneChangeStopOffset',SceneChangeStopOffset);
  SceneChangeFilterOffset := IniFile.ReadInteger('WAVDisplay','SceneChangeFilterOffset',SceneChangeFilterOffset);
  ShowTextInWAVDisplay := IniFile.ReadBool('WAVDisplay', 'ShowTextInWAVDisplay', ShowTextInWAVDisplay);
end;

// =============================================================================

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
  PageControlPreferences.ActivePage := tsGeneral;
  ListPluginParam.NodeDataSize := SizeOf(TParamData);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.FormDestroy(Sender: TObject);
begin
  ClearErrorList
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.FormActivate(Sender: TObject);
var IsExtReged : Boolean;
begin
  IsExtReged := ShellIsExtensionRegistered('vssprj',ApplicationName,Application.ExeName);
  chkAssociateExt.Perform(BM_SETCHECK, Ord(IsExtReged), 0);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ClearErrorList;
var i : integer;
begin
  ListPluginParam.Clear;
  for i:=0 to ListErrorChecking.Items.Count-1 do
    TJSPluginInfo(ListErrorChecking.Items.Objects[i]).Free;
  ListErrorChecking.Clear;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.LoadConfig(Config : TConfigObject);
var i : integer;
    HLID : THotkeyListItemData;
    ListItem : TTntListItem;
    JSPluginInfoSrc, JSPluginInfoDst  : TJSPluginInfo;
begin
  // Misc
  chkSwapSubList.Checked := Config.SwapSubtitlesList;
  chkDisableSubEditionInTimingMode.Checked := Config.DisableSubtitleEdition;
  chkEnableSubCreationWithSpaceKey.Checked := Config.EnableToggleCreation;
  chkEnableMouseAntiOverlapping.Checked := Config.EnableMouseAntiOverlapping;
  chkEnableMouseSnapping.Checked := Config.EnableMouseSnapping;
  chkSpaceKeyModifyTiming.Checked := Config.SpaceKeyModifyTiming;
  UpDownCPSTarget.Position := Config.SpaceKeyCPSTarget;
  UpDownMinimalDuration.Position := Config.SpaceKeyMinimalDuration;
  UpDownBlankBetweenSub.Position := Config.SpaceKeyBlankBetweenSubtitles;

  // Web server
  UpDownServerPort.Position := Config.ServerPort;
  chkEnableCompression.Checked := Config.EnableCompression;

  // Error plugin
  ClearErrorList;
  ListErrorChecking.Sorted := True;
  for i:=0 to Config.ListJSPlugin.Count-1 do
  begin
    // We do a local copy
    JSPluginInfoSrc := Config.ListJSPlugin[i];
    JSPluginInfoDst := TJSPluginInfo.Create;
    JSPluginInfoDst.Assign(JSPluginInfoSrc);
    ListErrorChecking.AddItem(JSPluginInfoDst.Name, JSPluginInfoDst);
  end;
  ListErrorChecking.Sorted := False;
  // Do the checking now cause sorting would have messed up the order 
  for i:=0 to ListErrorChecking.Items.Count-1 do
  begin
    JSPluginInfoSrc := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    ListErrorChecking.Checked[i] := JSPluginInfoSrc.Enabled;
  end;
  // Select the first item
  if (ListErrorChecking.Items.Count > 0) then
  begin
    ListErrorChecking.ItemIndex := 0;
    ListErrorCheckingClick(nil);
  end;

  // Hotkeys
  ListHotkeys.Clear;
  for i:=0 to Config.ListHotkeys.Count-1 do
  begin
    HLID := THotkeyListItemData.Create;
    HLID.Assign(Config.ListHotkeys[i]);
    ListItem := ListHotkeys.Items.Add;
    ListItem.Data := HLID;
    ListItem.Caption := HLID.Action.Caption;
    ListItem.SubItems.Add(ShortCutToText(HLID.NormalShortCut));
    ListItem.SubItems.Add(ShortCutToText(HLID.TimingShortCut));
  end;

  // Default hotkeys never change once loaded
  // we just keep a pointer
  ListDefaultHotkeys := Config.ListDefaultHotkeys;

  // Mouse
  ComboWheelTimeScrollModifier.ItemIndex := Ord(Config.MouseWheelTimeScrollModifier);
  ComboWheelVZoomModifier.ItemIndex := Ord(Config.MouseWheelVZoomModifier);
  ComboWheelHZoomModifier.ItemIndex := Ord(Config.MouseWheelHZoomModifier);
  chkEnableSSATimingMode.Checked := Config.MouseEnableSSATimingMode;

  // Backup
  chkCreateBackup.Checked := Config.EnableBackup;
  UpDownBackupTime.Position := Config.AutoBackupEvery;
  chkAutoSaveWhenPlaying.Checked := Config.AutoSaveWhenPlaying;

  // Fonts
  EditSubListFont.Text := Config.SubListFont;
  String2Font(EditSubListFont.Text, EditSubListFont.Font);
  EditSubTextFont.Text := Config.SubTextFont;
  String2Font(EditSubTextFont.Text, EditSubTextFont.Font);

  // WAV Display
  chkSceneChange.Checked := Config.ShowSceneChange;
  UpDownSCStart.Position := Config.SceneChangeStartOffset;
  UpDownSCStop.Position := Config.SceneChangeStopOffset;
  UpDownSCFilter.Position := Config.SceneChangeFilterOffset;
  chkShowTextInWAVDisplay.Checked := Config.ShowTextInWAVDisplay;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SaveConfig(Config : TConfigObject);
var i : integer;
    HLID : THotkeyListItemData;
    JSPluginInfoSrc, JSPluginInfoDst : TJSPluginInfo;
begin
  // Misc
  Config.SwapSubtitlesList := chkSwapSubList.Checked;
  Config.DisableSubtitleEdition := chkDisableSubEditionInTimingMode.Checked;
  Config.EnableToggleCreation := chkEnableSubCreationWithSpaceKey.Checked;
  Config.EnableMouseAntiOverlapping := chkEnableMouseAntiOverlapping.Checked;
  Config.EnableMouseSnapping := chkEnableMouseSnapping.Checked;
  Config.SpaceKeyModifyTiming := chkSpaceKeyModifyTiming.Checked;
  Config.SpaceKeyCPSTarget := UpDownCPSTarget.Position;
  Config.SpaceKeyMinimalDuration := UpDownMinimalDuration.Position;
  Config.SpaceKeyBlankBetweenSubtitles := UpDownBlankBetweenSub.Position;

  // Web server
  Config.ServerPort := UpDownServerPort.Position;
  Config.EnableCompression := chkEnableCompression.Checked;

  // Error plugin
  for i:=0 to ListErrorChecking.Items.Count-1 do
  begin
    JSPluginInfoSrc := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    JSPluginInfoDst := Config.ListJSPlugin[i];
    JSPluginInfoDst.Assign(JSPluginInfoSrc);
  end;

  // Hotkeys
  for i:=0 to ListHotkeys.Items.Count-1 do
  begin
    HLID := ListHotkeys.Items.Item[i].Data;
    THotkeyListItemData(Config.ListHotkeys[i]).Assign(HLID);
  end;

  // Mouse
  Config.MouseWheelTimeScrollModifier := TMouseWheelModifier(ComboWheelTimeScrollModifier.ItemIndex);
  Config.MouseWheelVZoomModifier := TMouseWheelModifier(ComboWheelVZoomModifier.ItemIndex);
  Config.MouseWheelHZoomModifier := TMouseWheelModifier(ComboWheelHZoomModifier.ItemIndex);
  Config.MouseEnableSSATimingMode := chkEnableSSATimingMode.Checked;

  // Backup
  Config.EnableBackup := chkCreateBackup.Checked;
  Config.AutoBackupEvery := UpDownBackupTime.Position;
  Config.AutoSaveWhenPlaying := chkAutoSaveWhenPlaying.Checked;

  // Fonts
  Config.SubListFont := EditSubListFont.Text;
  Config.SubTextFont := EditSubTextFont.Text;

  // WAV Display
  Config.ShowSceneChange := chkSceneChange.Checked;
  Config.SceneChangeStartOffset := UpDownSCStart.Position;
  Config.SceneChangeStopOffset := UpDownSCStop.Position;
  Config.SceneChangeFilterOffset := UpDownSCFilter.Position;
  Config.ShowTextInWAVDisplay := chkShowTextInWAVDisplay.Checked;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOkClick(Sender: TObject);
begin
  ListPluginParam.EndEditNode;
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListErrorCheckingClick(Sender: TObject);
var i : Integer;
    JSPluginInfo : TJSPluginInfo;
    paramData : PParamData;
    pNode : PVirtualNode;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    ShapeErrorColor.Brush.Color := JSPluginInfo.Color;
    lbErrorDescription.Caption := JSPluginInfo.Description;
    ListPluginParam.Clear;
    for i:=0 to JSPluginInfo.ParamList.Count-1 do
    begin
      pNode := ListPluginParam.AddChild(nil);
      paramData := ListPluginParam.GetNodeData(pNode);
      paramData.Param := JSPluginInfo.ParamList[i];
      paramData.ValueAsString := GetParamValueAsWideString(paramData.Param);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListErrorCheckingClickCheck(Sender: TObject);
var JSPluginInfo : TJSPluginInfo;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    JSPluginInfo.Enabled := ListErrorChecking.Checked[ListErrorChecking.ItemIndex];
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.chkAssociateExtClick(Sender: TObject);
begin
  if chkAssociateExt.Checked then
  begin
    ShellRegisterExtension('vssprj',ApplicationName,Application.ExeName);
  end
  else
  begin
    ShellUnRegisterExtension('vssprj',ApplicationName,Application.ExeName);
  end;
end;

//------------------------------------------------------------------------------

function TPreferencesForm.GetCurrentModeShortCut(HLID : THotkeyListItemData) : TShortCut;
begin
  Result := 0;
  if Assigned(HLID) then
  begin
    if (ComboHotkeyMode.ItemIndex = 0) then
      Result := HLID.NormalShortCut
    else if (ComboHotkeyMode.ItemIndex = 1) then
      Result := HLID.TimingShortCut;
  end;
end;

//------------------------------------------------------------------------------

function TPreferencesForm.GetCurrentModeShortCutFromList : TShortCut;
var
  HLID : THotkeyListItemData;
begin
  if Assigned(ListHotkeys.ItemFocused) then
  begin
    HLID := ListHotkeys.ItemFocused.Data;
    Result := GetCurrentModeShortCut(HLID);
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SetCurrentModeShortCut(HLID : THotkeyListItemData; ShortCut : TShortCut);
begin
  if Assigned(HLID) then
  begin
    if (ComboHotkeyMode.ItemIndex = 0) then
      HLID.NormalShortCut := ShortCut
    else if (ComboHotkeyMode.ItemIndex = 1) then
      HLID.TimingShortCut := ShortCut;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SetCurrentModeShortCutFromList(ShortCut : TShortCut);
var
  HLID : THotkeyListItemData;
begin
  if Assigned(ListHotkeys.ItemFocused) then
  begin
    HLID := ListHotkeys.ItemFocused.Data;
    SetCurrentModeShortCut(HLID, ShortCut);
    ListHotkeys.ItemFocused.SubItems.Strings[ComboHotkeyMode.ItemIndex] := ShortCutToText(ShortCut);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListHotkeysSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  HotKey1.HotKey := GetCurrentModeShortCutFromList;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ComboHotkeyModeSelect(Sender: TObject);
begin
  HotKey1.HotKey := GetCurrentModeShortCutFromList;
end;

//------------------------------------------------------------------------------
procedure TPreferencesForm.bttSetHotkeyClick(Sender: TObject);
var i : integer;
    HLID : THotkeyListItemData;
    Shortcut : TShortCut;
begin
  // First check if the hotkey is not already used
  for i:=0 to ListHotkeys.Items.Count-1 do
  begin
    HLID := ListHotkeys.Items.Item[i].Data;
    Shortcut := GetCurrentModeShortCut(HLID);
    if (Shortcut <> 0) and (Shortcut = HotKey1.HotKey) then
    begin
      // Clear the hotkey
      SetCurrentModeShortCut(HLID, 0);
      ListHotkeys.Items.Item[i].SubItems.Strings[ComboHotkeyMode.ItemIndex] := ShortCutToText(0);
      Break;
    end;
  end;

  // Set the hotkey
  SetCurrentModeShortCutFromList(HotKey1.HotKey);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttClearHotkeyClick(Sender: TObject);
begin
  SetCurrentModeShortCutFromList(0);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListHotkeysDeletion(Sender: TObject;
  Item: TListItem);
begin
  THotkeyListItemData(Item.Data).Free;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttResetAllHotkeysClick(Sender: TObject);
var i,j : integer;
    HLID, HLID2 : THotkeyListItemData;
begin
  if not Assigned(ListDefaultHotkeys) then
    Exit;
  for i:=0 to ListHotkeys.Items.Count-1 do
  begin
    HLID := ListHotkeys.Items.Item[i].Data;
    // Search based on action, cause List is alpha. sorted
    for j:=0 to ListDefaultHotkeys.Count-1 do
    begin
      HLID2 := ListDefaultHotkeys[j];
      if (HLID.Action = HLID2.Action) then
      begin
        HLID.NormalShortCut := HLID2.NormalShortCut;
        ListHotkeys.Items.Item[i].SubItems[0] := ShortCutToText(HLID2.NormalShortCut);
        HLID.TimingShortCut := HLID2.TimingShortCut;
        ListHotkeys.Items.Item[i].SubItems[1] := ShortCutToText(HLID2.TimingShortCut);
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOpenBackupDirClick(Sender: TObject);
begin
  CheckBackupDirectory;
  Tnt_ShellExecuteW(Handle, 'explore', PWideChar(g_BackupDirectory), nil,
    nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  ParamData: PParamData;
begin
  if (TextType = ttNormal) then
  begin
    ParamData := Sender.GetNodeData(Node);
    case Column of
      0: CellText := ParamData.Param.Name;
      1: CellText := ParamData.ValueAsString;
      2: CellText := ParamData.Param.UnitStr;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  ParamData: PParamData;
begin
  with Sender do
  begin
    ParamData := GetNodeData(Node);
    Allowed := (Column = 1) and (ParamData.Param.ParamType <> jsptUnknown);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditLink.Create;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.WMEndEditing(var Message: TMessage);
begin
  ListPluginParam.EndEditNode;
end;

// =============================================================================
// Adapted from the VTV Advanced demo

destructor TPropertyEditLink.Destroy;
begin
  FEditControl.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.EditIntegerKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then // #8 is Backspace
    Key := #0; // Discard the key
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.EditFloatKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9', '.']) then // #8 is Backspace
    Key := #0; // Discard the key
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
  RootControl : TWinControl;
begin
  case Key of
    VK_RETURN:
      begin
        RootControl := FTree;
        while (RootControl.Parent <> nil) do
          RootControl := RootControl.Parent;
        PostMessage(RootControl.Handle, WM_ENDEDITING, 0, 0);
      end;
    VK_ESCAPE,
    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if FEditControl is TComboBox then
          CanAdvance := CanAdvance and not TComboBox(FEditControl).DroppedDown;

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEditControl.Show;
  FEditControl.SetFocus;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: Boolean;
begin
  Result := True;
  FEditControl.Hide;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.EndEdit: Boolean;
var
  ParamData: PParamData;
  Buffer: array[0..1024] of Char;
  S: WideString;
begin
  Result := True;

  ParamData := FTree.GetNodeData(FNode);
  if FEditControl is TComboBox then
    S := TComboBox(FEditControl).Text
  else
  begin
    GetWindowText(FEditControl.Handle, Buffer, 1024);
    S := Buffer;
  end;

  if S <> ParamData.ValueAsString then
  begin
    SetParamValueAsWideString(ParamData.Param, S);
    ParamData.ValueAsString := S;
    ParamData.Changed := True;
    FTree.InvalidateNode(FNode);
  end;
  FEditControl.Hide;
  FTree.SetFocus;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;
begin
  Result := FEditControl.BoundsRect;
end;

//------------------------------------------------------------------------------

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  ParamData: PParamData;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FEditControl.Free;
  FEditControl := nil;
  ParamData := FTree.GetNodeData(Node);
  case ParamData.Param.ParamType of
    jsptWideString:
      begin
        FEditControl := TTntEdit.Create(nil);
        with FEditControl as TTntEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := ParamData.ValueAsString;
          OnKeyDown := EditKeyDown;
        end;
      end;
    jsptBoolean:
      begin
        FEditControl := TComboBox.Create(nil);
        with FEditControl as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := ParamData.ValueAsString;
          Items.Add(BoolToStr(True));
          Items.Add(BoolToStr(False));
          OnKeyDown := EditKeyDown;
        end;
      end;
    jsptInteger, jsptDouble:
      begin
        FEditControl := TEdit.Create(nil);
        with FEditControl as TEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := ParamData.ValueAsString;
          OnKeyDown := EditKeyDown;
          if ParamData.Param.ParamType = jsptInteger then
            OnKeyPress := EditIntegerKeyPress;
          if ParamData.Param.ParamType = jsptDouble then
            OnKeyPress := EditFloatKeyPress;
        end;
      end;
  else
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);
begin
  FEditControl.WindowProc(Message);
end;

//------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEditControl.BoundsRect := R;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttSubListFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(EditSubListFont.Font);
  if FontDialog1.Execute then
  begin
    EditSubListFont.Font.Assign(FontDialog1.Font);
    EditSubListFont.Text := Font2String(EditSubListFont.Font);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttSubTextFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(EditSubTextFont.Font);
  if FontDialog1.Execute then
  begin
    EditSubTextFont.Font.Assign(FontDialog1.Font);
    EditSubTextFont.Text := Font2String(EditSubTextFont.Font);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOpenBackupTempDirClick(Sender: TObject);
var TmpFolder : WideString;
begin
  TmpFolder := GetTemporaryFolder + 'VisualSubSync\';
  if WideDirectoryExists(TmpFolder) then
  begin
    Tnt_ShellExecuteW(Handle, 'explore', PWideChar(TmpFolder), nil,
      nil, SW_SHOWNORMAL);
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListPluginParamGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
var
  ParamData: PParamData;
begin
  ParamData := Sender.GetNodeData(Node);
  case Column of
    0: HintText := ParamData.Param.Description;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.pmiSelectAllClick(Sender: TObject);
var i : Integer;
    JSPluginInfo : TJSPluginInfo;
begin
  for i := 0 to ListErrorChecking.Count-1 do
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    ListErrorChecking.Checked[i] := True;
    JSPluginInfo.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.pmiUnselectAllClick(Sender: TObject);
var i : Integer;
    JSPluginInfo : TJSPluginInfo;
begin
  for i := 0 to ListErrorChecking.Count-1 do
  begin
    JSPluginInfo := TJSPluginInfo(ListErrorChecking.Items.Objects[i]);
    ListErrorChecking.Checked[i] := False;
    JSPluginInfo.Enabled := False;
  end;
end;


//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------

