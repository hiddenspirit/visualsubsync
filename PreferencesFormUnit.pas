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
  WAVDisplayerUnit;

type
  TErrorListElemType = (eletNoParam, eletIntParam);
  TErrorListElem = record
    Name : string;
    ElemType : TErrorListElemType;
    Value : Integer;
    UnitName : string;
    Description : string;
    Msg: string;
    Color : TColor;
  end;
  PErrorListElem = ^TErrorListElem;

const
  ErrorListElemTab : array[0..3] of TErrorListElem = (
    (Name: 'Overlapping';
     ElemType: eletNoParam;
     Description: 'An error is detected when the subtitle overlap on next subtitle.';
     Msg: 'Subtitle overlap on next subtitle :';
     Color: $003737FF),
    (Name: 'Too short display time';
     ElemType: eletIntParam;
     UnitName: 'Char/s';
     Description: 'An error is detected when the number of Char/s is strictly superior to the specified value.';
     Msg: 'Subtitle display time is too short :';
     Color: $0037C4FF),
    (Name: 'Too long display time';
     ElemType: eletIntParam;
     UnitName: 'Char/s';
     Description: 'An error is detected when the number of Char/s is strictly inferior to the specified value.';
     Msg: 'Subtitle display time is too long :';
     Color: $00FF8737),
    (Name: 'Too long line';
     ElemType: eletIntParam;
     UnitName: 'Characters';
     Description: 'An error is detected when the line length in a subtitle is strictly superior to the specified value.';
     Msg: 'Subtitle has a too long line :';
     Color: $0037FFFF)
  );

type
  TDefaultActionShortcut = record
    ActionName : string;
    ShortCut : string;
  end;

const
  DefaultTimingShortcuts : array[0..1] of TDefaultActionShortcut = (
    (ActionName: 'ActionStop'; ShortCut: 'Esc'),
    (ActionName: 'ActionPlay'; ShortCut: 'F1')    
  );

type
  TConfigObject = class
    // Web server
    ServerPort : Integer;
    EnableCompression : Boolean;
    // Error
    ErrorOverlappingEnabled : Boolean;
    ErrorTooShortDisplayTimeEnabled : Boolean;
    ErrorTooShortDisplayTimeValue : Integer;
    ErrorTooLongDisplayTimeEnabled : Boolean;
    ErrorTooLongDisplayTimeValue : Integer;
    ErrorTooLongLineEnabled : Boolean;
    ErrorTooLongLineValue : Integer;
    // Misc
    SwapSubtitlesList : Boolean;
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

    constructor Create;
    destructor Destroy; override;
    procedure SetDefault;
    procedure SaveIni(IniFile : TIniFile);
    procedure LoadIni(IniFile : TIniFile);
    procedure SetDefaultHotKeys(ActionList : TTntActionList);
  end;

  THotkeyListItemData = class
    Action : TTntAction;
    NormalShortCut : TShortCut;
    TimingShortCut : TShortCut;
    procedure Assign(Source : THotkeyListItemData);
  end;

  TPreferencesForm = class(TForm)
    TntPageControl1: TTntPageControl;
    tsGeneral: TTntTabSheet;
    bttOk: TTntButton;
    bttCancel: TTntButton;
    tsErrorChecking: TTntTabSheet;
    TntGroupBox1: TTntGroupBox;
    EditServerPort: TTntEdit;
    TntLabel1: TTntLabel;
    ListErrorChecking: TTntCheckListBox;
    EditErrorValue: TTntEdit;
    TntLabel2: TTntLabel;
    UpDownServerPort: TTntUpDown;
    lbErrorUnit: TTntLabel;
    lbErrorDescription: TTntLabel;
    TntLabel3: TTntLabel;
    TntBevel1: TTntBevel;
    GroupBox1: TGroupBox;
    chkAssociateExt: TCheckBox;
    ShapeErrorColor: TShape;
    TntLabel4: TTntLabel;
    chkEnableCompression: TCheckBox;
    chkSwapSubList: TCheckBox;
    tsHotKeys: TTntTabSheet;
    ListHotkeys: TTntListView;
    HotKey1: THotKey;
    bttSetHotkey: TTntButton;
    bttClearHotkey: TTntButton;
    ComboHotkeyMode: TTntComboBox;
    TntLabel5: TTntLabel;
    bttResetAllHotkeys: TTntButton;
    TntLabel6: TTntLabel;
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
    GroupBox4: TGroupBox;
    chkCreateBackup: TCheckBox;
    bttOpenBackupDir: TButton;
    EditBackupTime: TEdit;
    Label6: TLabel;
    UpDownBackupTime: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure bttOkClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure ListErrorCheckingClick(Sender: TObject);
    procedure EditErrorValueChange(Sender: TObject);
    procedure EditDigitOnlyKeyPress(Sender: TObject; var Key: Char);
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
  private
    { Private declarations }
    TimingMode : Boolean;
    ListDefaultHotkeys : TList;
        
    function GetCurrentModeShortCut(HLID : THotkeyListItemData) : TShortCut;
    function GetCurrentModeShortCutFromList : TShortCut;
    procedure SetCurrentModeShortCut(HLID : THotkeyListItemData; ShortCut : TShortCut);
    procedure SetCurrentModeShortCutFromList(ShortCut : TShortCut);
  public
    { Public declarations }
    procedure LoadConfig(Config : TConfigObject);
    procedure SaveConfig(Config : TConfigObject);
    function GetErrorListElem(Idx : Integer) : PErrorListElem;
    function GetMode : Boolean;    
    procedure SetMode(Timing : Boolean);
  end;

var
  PreferencesForm: TPreferencesForm;

implementation

uses MiscToolsUnit, GlobalUnit, ActnList, TntWindows;

{$R *.dfm}

// =============================================================================

procedure THotkeyListItemData.Assign(Source : THotkeyListItemData);
begin
  Action := Source.Action;
  NormalShortCut := Source.NormalShortCut;
  TimingShortCut := Source.TimingShortCut;
end;

// =============================================================================

constructor TConfigObject.Create;
begin
  ListHotkeys := TList.Create;
  ListDefaultHotkeys := TList.Create;
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
  inherited;
end;

//------------------------------------------------------------------------------

procedure TConfigObject.SetDefault;
begin
  // Misc
  SwapSubtitlesList := False;
  // Web server
  ServerPort := 80;
  EnableCompression := False; // Some IE version doesn't support deflate but say they does :p
  // Error
  ErrorOverlappingEnabled := True;
  ErrorTooShortDisplayTimeEnabled := True;
  ErrorTooShortDisplayTimeValue := 25;
  ErrorTooLongDisplayTimeEnabled := True;
  ErrorTooLongDisplayTimeValue := 5;
  ErrorTooLongLineEnabled := True;
  ErrorTooLongLineValue := 60;
  // Mouse
  MouseWheelTimeScrollModifier := mwmCtrl;
  MouseWheelVZoomModifier := mwmShift;
  MouseWheelHZoomModifier := mwmNone;
  MouseEnableSSATimingMode := True;
  // Backup
  EnableBackup := True;
  AutoBackupEvery := 0;
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
var i : integer;
    HLID : THotkeyListItemData;
begin
  // Misc
  IniFile.WriteBool('Misc','SwapSubtitlesList',SwapSubtitlesList);
  // Web server
  IniFile.WriteInteger('WebServer','Port',ServerPort);
  IniFile.WriteBool('WebServer','EnableCompression',EnableCompression);

  // Error
  IniFile.WriteBool('ErrorChecking','OverlappingEnabled',ErrorOverlappingEnabled);

  IniFile.WriteBool('ErrorChecking','TooShortDisplayTimeEnabled',ErrorTooShortDisplayTimeEnabled);
  IniFile.WriteInteger('ErrorChecking','TooShortDisplayTimeValue',ErrorTooShortDisplayTimeValue);

  IniFile.WriteBool('ErrorChecking','TooLongDisplayTimeEnabled',ErrorTooLongDisplayTimeEnabled);
  IniFile.WriteInteger('ErrorChecking','TooLongDisplayTimeValue',ErrorTooLongDisplayTimeValue);

  IniFile.WriteBool('ErrorChecking','TooLongLineEnabled',ErrorTooLongLineEnabled);
  IniFile.WriteInteger('ErrorChecking','TooLongLineValue',ErrorTooLongLineValue);

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
end;

//------------------------------------------------------------------------------

procedure TConfigObject.LoadIni(IniFile : TIniFile);
var i : integer;
    HLID : THotkeyListItemData;
begin
  // Misc
  SwapSubtitlesList := IniFile.ReadBool('Misc','SwapSubtitlesList',SwapSubtitlesList);

  // Web server
  ServerPort := IniFile.ReadInteger('WebServer','Port',ServerPort);
  EnableCompression := IniFile.ReadBool('WebServer','EnableCompression',EnableCompression);

  // Error
  ErrorOverlappingEnabled := IniFile.ReadBool('ErrorChecking','OverlappingEnabled',ErrorOverlappingEnabled);

  ErrorTooShortDisplayTimeEnabled := IniFile.ReadBool('ErrorChecking','TooShortDisplayTimeEnabled',ErrorTooShortDisplayTimeEnabled);
  ErrorTooShortDisplayTimeValue := IniFile.ReadInteger('ErrorChecking','TooShortDisplayTimeValue',ErrorTooShortDisplayTimeValue);

  ErrorTooLongDisplayTimeEnabled := IniFile.ReadBool('ErrorChecking','TooLongDisplayTimeEnabled',ErrorTooLongDisplayTimeEnabled);
  ErrorTooLongDisplayTimeValue := IniFile.ReadInteger('ErrorChecking','TooLongDisplayTimeValue',ErrorTooLongDisplayTimeValue);

  ErrorTooLongLineEnabled := IniFile.ReadBool('ErrorChecking','TooLongLineEnabled',ErrorTooLongLineEnabled);
  ErrorTooLongLineValue := IniFile.ReadInteger('ErrorChecking','TooLongLineValue',ErrorTooLongLineValue);

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

  // Backup
  EnableBackup := IniFile.ReadBool('Backup','EnableBackup',EnableBackup);
  AutoBackupEvery := IniFile.ReadInteger('Backup','AutoBackupEvery',AutoBackupEvery);
end;

// =============================================================================

procedure TPreferencesForm.FormCreate(Sender: TObject);
var i : integer;
begin
  TimingMode := False;

  for i:=0 to Length(ErrorListElemTab)-1 do
  begin
    ListErrorChecking.AddItem(ErrorListElemTab[i].Name,@ErrorListElemTab[i]);
  end;
  ListErrorChecking.ItemIndex := 0;
  ListErrorCheckingClick(Self);

  TntPageControl1.ActivePage := tsGeneral;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
  chkAssociateExt.Checked := ShellIsExtensionRegistered('vssprj',ApplicationName,Application.ExeName);
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.LoadConfig(Config : TConfigObject);
var i : integer;
    HLID : THotkeyListItemData;
    ListItem : TTntListItem;
begin
  // Misc
  chkSwapSubList.Checked := Config.SwapSubtitlesList;

  // Web server
  UpDownServerPort.Position := Config.ServerPort;
  chkEnableCompression.Checked := Config.EnableCompression;

  // Error
  ListErrorChecking.Checked[0] := Config.ErrorOverlappingEnabled;

  ListErrorChecking.Checked[1] := Config.ErrorTooShortDisplayTimeEnabled;
  PErrorListElem(ListErrorChecking.Items.Objects[1]).Value := Config.ErrorTooShortDisplayTimeValue;

  ListErrorChecking.Checked[2] := Config.ErrorTooLongDisplayTimeEnabled;
  PErrorListElem(ListErrorChecking.Items.Objects[2]).Value := Config.ErrorTooLongDisplayTimeValue;

  ListErrorChecking.Checked[3] := Config.ErrorTooLongLineEnabled;
  PErrorListElem(ListErrorChecking.Items.Objects[3]).Value := Config.ErrorTooLongLineValue;

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
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SaveConfig(Config : TConfigObject);
var i : integer;
    HLID : THotkeyListItemData;
begin
  // Misc
  Config.SwapSubtitlesList := chkSwapSubList.Checked;
  
  // Web server
  Config.ServerPort := UpDownServerPort.Position;
  Config.EnableCompression := chkEnableCompression.Checked;

  // Error
  Config.ErrorOverlappingEnabled := ListErrorChecking.Checked[0];

  Config.ErrorTooShortDisplayTimeEnabled := ListErrorChecking.Checked[1];
  Config.ErrorTooShortDisplayTimeValue := PErrorListElem(ListErrorChecking.Items.Objects[1]).Value;

  Config.ErrorTooLongDisplayTimeEnabled := ListErrorChecking.Checked[2];
  Config.ErrorTooLongDisplayTimeValue := PErrorListElem(ListErrorChecking.Items.Objects[2]).Value;

  Config.ErrorTooLongLineEnabled := ListErrorChecking.Checked[3];
  Config.ErrorTooLongLineValue := PErrorListElem(ListErrorChecking.Items.Objects[3]).Value;

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
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.ListErrorCheckingClick(Sender: TObject);
var ErrorListElem : PErrorListElem;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    ErrorListElem := PErrorListElem(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    EditErrorValue.Enabled := (ErrorListElem.ElemType <> eletNoParam);
    if (EditErrorValue.Enabled) then
    begin
      EditErrorValue.Text := IntToStr(ErrorListElem.Value);
      EditErrorValue.Color := clWindow;
      lbErrorUnit.Caption := ErrorListElem.UnitName;
    end
    else
    begin
      EditErrorValue.Text := '';
      EditErrorValue.Color := cl3DLight;
      lbErrorUnit.Caption := '';
    end;
    ShapeErrorColor.Brush.Color := ErrorListElem.Color;    
    lbErrorDescription.Caption := ErrorListElem.Description;
  end;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.EditErrorValueChange(Sender: TObject);
var ErrorListElem : PErrorListElem;
begin
  if ListErrorChecking.ItemIndex <> -1 then
  begin
    ErrorListElem := PErrorListElem(ListErrorChecking.Items.Objects[ListErrorChecking.ItemIndex]);
    ErrorListElem.Value := StrToIntDef(EditErrorValue.Text,ErrorListElem.Value);
  end;  
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.EditDigitOnlyKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then // #8 is Backspace
    Key := #0; // Discard the key
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

function TPreferencesForm.GetErrorListElem(Idx : Integer) : PErrorListElem;
begin
  Result := nil;
  if (Idx >= 0) and (Idx < ListErrorChecking.Count) then
  begin
    Result := PErrorListElem(ListErrorChecking.Items.Objects[Idx]);
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

function TPreferencesForm.GetMode : Boolean;
begin
  Result := TimingMode;
end;

//------------------------------------------------------------------------------

procedure TPreferencesForm.SetMode(Timing : Boolean);
begin
  TimingMode := Timing;
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
end.
//------------------------------------------------------------------------------

