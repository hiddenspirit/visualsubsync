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

unit ProjectUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, TntStdCtrls, TntDialogs, Renderer;

type
  TProjectWAVMode = (pwmNoWaveform, pwmExternal, pwmPeakOnly);
  TVSSProject = class
  private
    FIsDirty : Boolean;
    FOnDirtyChange : TNotifyEvent;
    FOnDirtySet : TNotifyEvent;

    procedure SetIsDirty(Value : Boolean);
  public
    Filename : WideString;
    VideoSource : WideString;
    WAVFile : WideString;
    PeakFile : WideString;
    SubtitlesFile : WideString;
    WAVMode : TProjectWAVMode;
    IsUTF8 : Boolean;
    TextPipeSource : WideString;
    TextPipePosition : Integer;
    ShowVideo : Boolean;
    VideoPanelWidth : Integer;
    VideoPanelHeight : Integer;
    VideoWindowNormalLeft : Integer;
    VideoWindowNormalTop : Integer;
    VideoWindowNormalWidth : Integer;
    VideoWindowNormalHeight : Integer;
    DetachedVideo : Boolean;
    WAVDisplayerPositionStartMs : Integer;
    WAVDisplayerPositionStopMs : Integer;
    FocusedTimeMs : Integer;
    Dictionnary : WideString;
    Presets : WideString;

    procedure LoadFromINIFile(ProjectFilename : WideString);
    constructor Create;
  published
    property IsDirty : Boolean read FIsDirty write SetIsDirty;
    property OnDirtyChange : TNotifyEvent read FOnDirtyChange write FOnDirtyChange;
    property OnDirtySet : TNotifyEvent read FOnDirtySet write FOnDirtySet;
  end;

  TProjectForm = class(TForm)
    bttCreateNewProject: TTntButton;
    bttCancel: TTntButton;
    gbVideoFile: TTntGroupBox;
    bttBrowseVideoFile: TSpeedButton;
    EditVideoFilename: TTntEdit;
    gbWAVFile: TTntGroupBox;
    bttBrowseWAVFile: TSpeedButton;
    EditWAVFilename: TTntEdit;
    gbSubtitleFile: TTntGroupBox;
    bttBrowseSubtitleFile: TSpeedButton;
    EditSubtitleFilename: TTntEdit;
    gbProjectFile: TTntGroupBox;
    bttBrowseProjectFile: TSpeedButton;
    EditProjectFilename: TTntEdit;
    TntOpenDialog1: TTntOpenDialog;
    bttOk: TTntButton;
    rbExternal: TRadioButton;
    rbPeakOnly: TRadioButton;
    bttBrowsePeakFile: TSpeedButton;
    EditPeakFilename: TTntEdit;
    chkSaveAsUTF8: TTntCheckBox;
    cbSubtitleFormat: TTntComboBox;
    rbNoWaveform: TTntRadioButton;
    bttExtractWAVFromVideo: TTntButton;
    procedure bttCreateNewProjectClick(Sender: TObject);
    procedure bttCancelClick(Sender: TObject);
    procedure bttBrowseVideoFileClick(Sender: TObject);
    procedure bttBrowseWAVFileClick(Sender: TObject);
    procedure bttBrowseSubtitleFileClick(Sender: TObject);
    procedure bttBrowseProjectFileClick(Sender: TObject);
    procedure bttExtractWAVFromVideoClick(Sender: TObject);
    procedure bttOkClick(Sender: TObject);
    procedure rbInternalWAVClick(Sender: TObject);
    procedure bttBrowsePeakFileClick(Sender: TObject);
    procedure cbSubtitleFormatChange(Sender: TObject);
    procedure EditPeakFilenameEnter(Sender: TObject);
    procedure EditPeakFilenameExit(Sender: TObject);
  private
    { Private declarations }
    procedure WAVSelectMode(WavMode : TProjectWAVMode);
    procedure UpdateFormatCombobox;
    function ShowExtractForm(extType : TWAVExtractionType) : Boolean;
    function CheckData(AskForExtraction : Boolean) : Boolean;
  public
    { Public declarations }
    procedure Clear;
    procedure ConfigureInNewProjectMode;
    procedure ConfigureInModifyProjectMode(Project : TVSSProject);
    function GetWAVMode : TProjectWAVMode;
    function GetPeakFilename : WideString;
    function GetWAVFilename : WideString;
  end;

var
  ProjectForm: TProjectForm;

const
  LEAVE_EMPTY : WideString = 'Leave empty for extraction';

implementation

{$R *.dfm}

uses WAVExtractFormUnit, TntSysUtils, MiscToolsUnit, TntIniFiles, TntWindows;

//==============================================================================

function TryToCreateOrOpenFile(const fileName : WideString) : Boolean;
var fileHandle : THandle;
begin
  // Try to create it
  fileHandle := Tnt_CreateFileW(PWideChar(fileName), GENERIC_READ,
    FILE_SHARE_READ	, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (fileHandle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(fileHandle);
    Result := True;
  end
  else
    Result := False;
end;

//==============================================================================

constructor TVSSProject.Create;
begin
  inherited;
  FOnDirtyChange := nil;
  FOnDirtySet := nil;
  FIsDirty := False;
end;

procedure TVSSProject.SetIsDirty(Value : Boolean);
begin
  if FIsDirty <> Value then
  begin
    FIsDirty := Value;
    if Assigned(FOnDirtyChange) then
    begin
      FOnDirtyChange(Self);
    end;
  end;
  if Value and Assigned(FOnDirtySet) then
  begin
    FOnDirtySet(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TVSSProject.LoadFromINIFile(ProjectFilename : WideString);
var
  ProjectFileIni : TTntIniFile;
begin
    ProjectFileIni := TTntIniFile.Create(ProjectFilename);
    Filename := ProjectFilename;
    VideoSource := ProjectFileIni.ReadString('VisualSubsync','VideoSource','');
    WAVFile := ProjectFileIni.ReadString('VisualSubsync','WAVFile','');
    PeakFile := ProjectFileIni.ReadString('VisualSubsync','PeakFile','');
    WAVMode := TProjectWAVMode(ProjectFileIni.ReadInteger('VisualSubsync','WAVMode',1));
    SubtitlesFile := ProjectFileIni.ReadString('VisualSubsync','SubtitlesFile','');
    TextPipeSource := ProjectFileIni.ReadString('VisualSubsync','TextPipeSource','');
    TextPipePosition := ProjectFileIni.ReadInteger('VisualSubsync','TextPipePosition',0);

    VideoPanelWidth := ProjectFileIni.ReadInteger('VisualSubsync','VideoPanelWidth', 0);
    VideoPanelHeight := ProjectFileIni.ReadInteger('VisualSubsync','VideoPanelHeight', 0);
    ShowVideo := ProjectFileIni.ReadBool('VisualSubsync','ShowVideo', False);
    DetachedVideo := ProjectFileIni.ReadBool('VisualSubsync','DetachedVideo', False);
    VideoWindowNormalLeft := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalLeft', 0);
    VideoWindowNormalTop := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalTop', 0);
    VideoWindowNormalWidth := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalWidth', 0);
    VideoWindowNormalHeight := ProjectFileIni.ReadInteger('VisualSubsync', 'VideoWindowNormalHeight', 0);

    WAVDisplayerPositionStartMs := ProjectFileIni.ReadInteger('VisualSubsync', 'WAVDisplayerPositionStartMs', -1);
    WAVDisplayerPositionStopMs := ProjectFileIni.ReadInteger('VisualSubsync', 'WAVDisplayerPositionStopMs', -1);
    FocusedTimeMs := ProjectFileIni.ReadInteger('VisualSubsync', 'FocusedTimeMs', -1);

    Dictionnary := ProjectFileIni.ReadString('VisualSubsync','Dictionnary','');
    Presets := ProjectFileIni.ReadString('VisualSubsync','Presets','');

    ProjectFileIni.Free;

    // Resolve relative path
    VideoSource := WideResolveRelativePath(Filename, VideoSource);
    WAVFile := WideResolveRelativePath(Filename, WAVFile);
    PeakFile := WideResolveRelativePath(Filename, PeakFile);
    SubtitlesFile := WideResolveRelativePath(Filename, SubtitlesFile);
    TextPipeSource := WideResolveRelativePath(Filename, TextPipeSource);
end;

//==============================================================================

function TProjectForm.CheckData(AskForExtraction : Boolean) : Boolean;
var res : Integer;
    VideoFileExists : Boolean;
begin
  Result := False;
  VideoFileExists := (Trim(EditVideoFilename.Text) <> '') and WideFileExists(EditVideoFilename.Text);

  // We need a least subtitle filename
  if not TryToCreateOrOpenFile(EditSubtitleFilename.Text) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The subtitle file name is missing or is invalid.')),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditSubtitleFilename.SetFocus;
    Exit;
  end;

  // External WAV file mode
  if (rbExternal.Checked) then
  begin
    if (not WideFileExists(EditWAVFilename.Text)) then
    begin
      if (AskForExtraction and VideoFileExists) then
      begin
        if ShowExtractForm(wetFastConversion) = False then
          Exit;
      end
      else
      begin
        MessageBoxW(Handle, PWideChar(WideString('The WAV file "' + EditWAVFilename.Text +
          '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
        EditWAVFilename.SetFocus;
        Exit;
      end;
    end;
  end;

  // Peak file only mode
  if (rbPeakOnly.Checked) then
  begin
    if (not WideFileExists(EditPeakFilename.Text)) then
    begin
      if (AskForExtraction and VideoFileExists) then
      begin
        if ShowExtractForm(wetOnlyPeakFile) = False then
          Exit;
      end
      else
      begin
        MessageBoxW(Handle, PWideChar(WideString('The peak file "' + EditPeakFilename.Text +
          '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
        EditPeakFilename.SetFocus;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttCreateNewProjectClick(Sender: TObject);
var res : Integer;
begin
  if not CheckData(True) then
    Exit;

  if WideFileExists(EditProjectFilename.Text) then
  begin
    // ask for replacement or cancel
    res := MessageBoxW(Handle,
      PWideChar(WideString('The project file named ' +
      EditProjectFilename.Text + ' already exist.' + #13#10 +
      'Do you want to overwrite this file ?')),
      PWideChar(WideString('Please confirme :')),
      MB_YESNO or MB_ICONWARNING);
    if (res = IDNO) then
      Exit;
  end;

  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseVideoFileClick(Sender: TObject);
var WAVFilename, PeakFilename : WideString;
begin
  TntOpenDialog1.FileName := EditVideoFilename.Text;
  TntOpenDialog1.Filter :=
    'Video files|*.AVI;*.OGM;*.MKV;*.MKA;*.MP4;*.DIVX;*.RM;' +
    '*.RMVB;*.MPEG;*.MPG;*.VOB;*.AVS;*.WMV;*.MOV' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    EditVideoFilename.Text := TntOpenDialog1.FileName;
    
    // If external wav file exists with the same name use it
    WAVFilename := WideChangeFileExt(EditVideoFilename.Text,'.wav');
    PeakFilename := WideChangeFileExt(EditVideoFilename.Text,'.peak');
    if (Trim(EditWAVFilename.Text) = '') and WideFileExists(WAVFilename) then
    begin
      WAVSelectMode(pwmExternal);
      EditWAVFilename.Text := WAVFilename;
    end
    // else if peak file exists with the same name use it
    else if (Trim(EditPeakFilename.Text) = '') and WideFileExists(PeakFilename) then
    begin
      WAVSelectMode(pwmPeakOnly);
      EditPeakFilename.Text := PeakFilename;
    end
    // else choose peak file
    else
    begin
      WAVSelectMode(pwmPeakOnly);
    end;

    if Trim(EditSubtitleFilename.Text) = '' then
    begin
      EditSubtitleFilename.Text := WideChangeFileExt(EditVideoFilename.Text,'.srt');
      UpdateFormatCombobox;
    end;

    if Trim(EditProjectFilename.Text) = '' then
    begin
      EditProjectFilename.Text := WideChangeFileExt(EditVideoFilename.Text,'.vssprj');
    end;
  end;

  bttExtractWAVFromVideo.Enabled := WideFileExists(EditVideoFilename.Text);
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseWAVFileClick(Sender: TObject);
begin
  TntOpenDialog1.FileName := EditWAVFilename.Text;
  TntOpenDialog1.Filter := 'WAV files (*.wav)|*.WAV' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    EditWAVFilename.Text := TntOpenDialog1.FileName;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseSubtitleFileClick(Sender: TObject);
var Ext : WideString; 
begin
  TntOpenDialog1.FileName := EditSubtitleFilename.Text;
  TntOpenDialog1.Filter := 'SRT files (*.srt)|*.SRT' + '|' +
    'SSA/ASS files (*.ssa,*.ass)|*.SSA;*.ASS' + '|' +
    'All files (*.*)|*.*';
    
  Ext := WideLowerCase(WideExtractFileExt(EditSubtitleFilename.Text));
  if (Ext = '.srt') then
    TntOpenDialog1.FilterIndex := 1
  else if (Ext = '.ssa') or (Ext = '.ass') then
    TntOpenDialog1.FilterIndex := 2
  else
    TntOpenDialog1.FilterIndex := 3;
    
  if TntOpenDialog1.Execute then
  begin
    EditSubtitleFilename.Text := TntOpenDialog1.FileName;
    UpdateFormatCombobox;
    if Trim(EditProjectFilename.Text) = '' then
    begin
      EditProjectFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.vssprj');
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowseProjectFileClick(Sender: TObject);
begin
  TntOpenDialog1.FileName := EditProjectFilename.Text;
  TntOpenDialog1.Filter := 'VSS project (*.vssprj)|*.VSSPRJ' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    EditProjectFilename.Text := TntOpenDialog1.FileName;
  end;
end;

//------------------------------------------------------------------------------

function TProjectForm.ShowExtractForm(extType : TWAVExtractionType) : Boolean;
var
  ExtractWAVForm: TExtractWAVForm;
  modalResult : Integer;
begin
  Result := False;
  if (Trim(EditVideoFilename.Text) = '') or (not WideFileExists(EditVideoFilename.Text)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The video file "' + EditVideoFilename.Text +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditVideoFilename.SetFocus;
    Exit;
  end;
  ExtractWAVForm := TExtractWAVForm.Create(nil);
  ExtractWAVForm.VideoFilename := EditVideoFilename.Text;
  ExtractWAVForm.DestinationFilename := WideChangeFileExt(EditVideoFilename.Text,'.wav');
  ExtractWAVForm.SetExtractionType(extType);
  modalResult := ExtractWAVForm.ShowModal;
  if modalResult = mrOk then
  begin
    if ExtractWAVForm.rbOnlyPeak.Checked then
    begin
      EditPeakFilename.Text := WideChangeFileExt(EditVideoFilename.Text,'.peak');
      WAVSelectMode(pwmPeakOnly);
    end
    else
    begin
      EditWAVFilename.Text := ExtractWAVForm.DestinationFilename;
      WAVSelectMode(pwmExternal);
    end;
  end;
  Result := (modalResult = mrOk);
  ExtractWAVForm.Free;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttExtractWAVFromVideoClick(Sender: TObject);
begin
  ShowExtractForm(wetOnlyPeakFile);
end;

//------------------------------------------------------------------------------

procedure TProjectForm.Clear;
begin
  EditVideoFilename.Text := '';
  EditSubtitleFilename.Text := '';
  EditProjectFilename.Text := '';
  chkSaveAsUTF8.Checked := False;
  cbSubtitleFormat.ItemIndex := 0;
  bttExtractWAVFromVideo.Enabled := False;

  EditPeakFilename.Text := LEAVE_EMPTY;
  EditPeakFilename.Font.Color := clInactiveCaption;

  EditWAVFilename.Text := LEAVE_EMPTY;
  EditWAVFilename.Font.Color := clInactiveCaption;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.ConfigureInNewProjectMode;
begin
  Self.Caption := 'New Project';
  bttCreateNewProject.Visible := True;
  bttOk.Visible := False;
  WAVSelectMode(pwmNoWaveform);
  Self.Clear;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.ConfigureInModifyProjectMode(Project : TVSSProject);
begin
  Self.Caption := 'Project Properties';
  bttCreateNewProject.Visible := False;
  bttOk.Visible := True;
  EditVideoFilename.Text := Project.VideoSource;
  if (Project.WAVFile = '') then
    EditWAVFilename.Text := LEAVE_EMPTY
  else
    EditWAVFilename.Text := Project.WAVFile;
  EditSubtitleFilename.Text := Project.SubtitlesFile;
  EditProjectFilename.Text := Project.Filename;
  if (Project.PeakFile = '') then
    EditPeakFilename.Text := LEAVE_EMPTY
  else
    EditPeakFilename.Text := Project.PeakFile;
  WAVSelectMode(Project.WAVMode);
  chkSaveAsUTF8.Checked := Project.IsUTF8;
  UpdateFormatCombobox;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttOkClick(Sender: TObject);
begin
  if not CheckData(False) then
    Exit;
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.WAVSelectMode(WavMode : TProjectWAVMode);
begin
  rbNoWaveform.Checked := (WavMode = pwmNoWaveform);

  rbExternal.Checked := (WavMode = pwmExternal);
  EditWAVFilename.Enabled := (WavMode = pwmExternal);
  bttBrowseWAVFile.Enabled := (WavMode = pwmExternal);

  rbPeakOnly.Checked := (WavMode = pwmPeakOnly);
  EditPeakFilename.Enabled := (WavMode = pwmPeakOnly);
  bttBrowsePeakFile.Enabled := (WavMode = pwmPeakOnly);
end;

//------------------------------------------------------------------------------

procedure TProjectForm.rbInternalWAVClick(Sender: TObject);
begin
  WAVSelectMode(TProjectWAVMode((Sender as TRadioButton).Tag));
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttBrowsePeakFileClick(Sender: TObject);
begin
  TntOpenDialog1.FileName := EditPeakFilename.Text;
  TntOpenDialog1.Filter := 'Peak files (*.peak)|*.PEAK' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    EditPeakFilename.Text := TntOpenDialog1.FileName;
  end;
end;

//------------------------------------------------------------------------------

function TProjectForm.GetWAVMode : TProjectWAVMode;
begin
  if rbNoWaveform.Checked then
    Result := pwmNoWaveform
  else if rbExternal.Checked then
    Result := pwmExternal
  else
    Result := pwmPeakOnly
end;

//------------------------------------------------------------------------------

procedure TProjectForm.cbSubtitleFormatChange(Sender: TObject);
begin
  if Length(EditSubtitleFilename.Text) <= 0 then
    Exit;

  // Change subtitle format
  case cbSubtitleFormat.ItemIndex of
  0: EditSubtitleFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.srt');
  1: EditSubtitleFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.ssa');
  2: EditSubtitleFilename.Text := WideChangeFileExt(EditSubtitleFilename.Text,'.ass');
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.UpdateFormatCombobox;
var Ext : WideString;
begin
  Ext := WideLowerCase(WideExtractFileExt(EditSubtitleFilename.Text));
  if (Ext = '.srt') then
    cbSubtitleFormat.ItemIndex := 0
  else if (Ext = '.ssa') then
    cbSubtitleFormat.ItemIndex := 1
  else if (Ext = '.ass') then
    cbSubtitleFormat.ItemIndex := 2;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.EditPeakFilenameEnter(Sender: TObject);
var TntEdit : TTntEdit;
begin
  if (Sender is TTntEdit) then
  begin
    TntEdit := (Sender as TTntEdit);
    if (TntEdit.Text = LEAVE_EMPTY) then
    begin
      TntEdit.Text := '';
      TntEdit.Font.Color := clWindowText;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.EditPeakFilenameExit(Sender: TObject);
var TntEdit : TTntEdit;
begin
  if (Sender is TTntEdit) then
  begin
    TntEdit := (Sender as TTntEdit);
    if (TntEdit.Text = '') then
    begin
      TntEdit.Text := LEAVE_EMPTY;
      TntEdit.Font.Color := clInactiveCaption;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TProjectForm.GetPeakFilename : WideString;
begin
  if (EditPeakFilename.Text = LEAVE_EMPTY) then
    Result := ''
  else
    Result := EditPeakFilename.Text
end;

//------------------------------------------------------------------------------

function TProjectForm.GetWAVFilename : WideString;
begin
  if (EditWAVFilename.Text = LEAVE_EMPTY) then
    Result := ''
  else
    Result := EditWAVFilename.Text
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
