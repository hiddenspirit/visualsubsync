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
  Dialogs, StdCtrls, Buttons, TntStdCtrls, TntDialogs;

type
  TProjectWAVMode = (pwmInternal, pwmExternal, pwmPeakOnly);
  TVSSProject = class
  private
    FIsDirty : Boolean;
    FOnDirtyChange : TNotifyEvent;

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
  published
    property IsDirty : Boolean read FIsDirty write SetIsDirty;
    property OnDirtyChange : TNotifyEvent read FOnDirtyChange write FOnDirtyChange;
  end;

  TProjectForm = class(TForm)
    bttCreateNewProject: TTntButton;
    bttCancel: TTntButton;
    gbVideoFile: TTntGroupBox;
    bttBrowseVideoFile: TSpeedButton;
    EditVideoFilename: TTntEdit;
    gbWAVFile: TTntGroupBox;
    bttBrowseWAVFile: TSpeedButton;
    bttExtractWAVFromVideo: TTntButton;
    EditWAVFilename: TTntEdit;
    gbSubtitleFile: TTntGroupBox;
    bttBrowseSubtitleFile: TSpeedButton;
    EditSubtitleFilename: TTntEdit;
    gbProjectFile: TTntGroupBox;
    bttBrowseProjectFile: TSpeedButton;
    EditProjectFilename: TTntEdit;
    TntOpenDialog1: TTntOpenDialog;
    bttOk: TTntButton;
    rbInternalWAV: TRadioButton;
    rbExternal: TRadioButton;
    rbPeakOnly: TRadioButton;
    bttBrowsePeakFile: TSpeedButton;
    EditPeakFilename: TTntEdit;
    chkSaveAsUTF8: TTntCheckBox;
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
  private
    { Private declarations }
    procedure WAVSelectMode(WavMode : TProjectWAVMode);
  public
    { Public declarations }
    procedure Clear;
    procedure ConfigureInNewProjectMode;
    procedure ConfigureInModifyProjectMode(Project : TVSSProject);
    function GetWAVMode : TProjectWAVMode;    
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.dfm}

uses WAVExtractFormUnit;

//==============================================================================

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
end;

//==============================================================================

procedure TProjectForm.bttCreateNewProjectClick(Sender: TObject);
var res : Integer;
begin
  if (Trim(EditVideoFilename.Text) <> '') and (not FileExists(EditVideoFilename.Text)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The video file "' + EditVideoFilename.Text +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditVideoFilename.SetFocus;
    Exit;
  end;

  if ((Trim(EditWAVFilename.Text) <> '') or rbExternal.Checked) and
     (not FileExists(EditWAVFilename.Text)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The WAV file "' + EditWAVFilename.Text +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditWAVFilename.SetFocus;
    Exit;
  end;

  if (Trim(EditVideoFilename.Text) = '') and (Trim(EditWAVFilename.Text) = '') then
  begin
    MessageBoxW(Handle, PWideChar(WideString('You need to give at least a video file or an audio file.')),
      PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditVideoFilename.SetFocus;
    Exit;
  end;

  if ((Trim(EditPeakFilename.Text) <> '') or rbPeakOnly.Checked) and
     (not FileExists(EditPeakFilename.Text)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The peak file "' + EditWAVFilename.Text +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditPeakFilename.SetFocus;
    Exit;
  end;

  if Trim(EditSubtitleFilename.Text) = '' then
  begin
    EditSubtitleFilename.Text := ChangeFileExt(EditVideoFilename.Text,'.srt');
  end;
  if Trim(EditProjectFilename.Text) = '' then
  begin
    EditProjectFilename.Text := ChangeFileExt(EditVideoFilename.Text,'.vssprj');
  end;

  if rbInternalWAV.Checked then
  begin
    bttExtractWAVFromVideo.Click;
    if rbInternalWAV.Checked then
    begin
      MessageBoxW(Handle,
        PWideChar(WideString('You need a WAV file or a peak file.')),
        PWideChar(WideString('Error :')),
        MB_OK or MB_ICONERROR);
      Exit;
    end;
  end;

  if FileExists(EditProjectFilename.Text) then
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
begin
  TntOpenDialog1.FileName := EditVideoFilename.Text;
  TntOpenDialog1.Filter :=
    'Video files|*.AVI;*.OGM;*.MKV;*.MP4;*.DIVX;*.RM;*.RMVB' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    EditVideoFilename.Text := TntOpenDialog1.FileName;
    if (Trim(EditWAVFilename.Text) = '') and FileExists(ChangeFileExt(EditVideoFilename.Text,'.wav')) then
    begin
      WAVSelectMode(pwmExternal);
      EditWAVFilename.Text := ChangeFileExt(EditVideoFilename.Text,'.wav');
    end;
    if Trim(EditSubtitleFilename.Text) = '' then
    begin
      EditSubtitleFilename.Text := ChangeFileExt(EditVideoFilename.Text,'.srt');
    end;
    if Trim(EditProjectFilename.Text) = '' then
    begin
      EditProjectFilename.Text := ChangeFileExt(EditVideoFilename.Text,'.vssprj');
    end;
  end;
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
begin
  TntOpenDialog1.FileName := EditSubtitleFilename.Text;
  TntOpenDialog1.Filter := 'Subtitles files (*.srt)|*.SRT' + '|' +
    'All files (*.*)|*.*';
  if TntOpenDialog1.Execute then
  begin
    EditSubtitleFilename.Text := TntOpenDialog1.FileName;
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

procedure TProjectForm.bttExtractWAVFromVideoClick(Sender: TObject);
var
  ExtractWAVForm: TExtractWAVForm;
begin
  if (Trim(EditVideoFilename.Text) = '') or (not FileExists(EditVideoFilename.Text)) then
  begin
    MessageBoxW(Handle, PWideChar(WideString('The video file "' + EditVideoFilename.Text +
      '" doesn''t exist.')), PWideChar(WideString('Error')), MB_OK or MB_ICONERROR);
    EditVideoFilename.SetFocus;
    Exit;
  end;
  ExtractWAVForm := TExtractWAVForm.Create(nil);
  ExtractWAVForm.VideoFilename := EditVideoFilename.Text;
  ExtractWAVForm.DestinationFilename := ChangeFileExt(EditVideoFilename.Text,'.wav');
  if ExtractWAVForm.ShowModal = mrOk then
  begin
    if ExtractWAVForm.rbOnlyPeak.Checked then
    begin
      EditPeakFilename.Text := ChangeFileExt(EditVideoFilename.Text,'.peak');
      WAVSelectMode(pwmPeakOnly);
    end
    else
    begin
      EditWAVFilename.Text := ExtractWAVForm.DestinationFilename;
      WAVSelectMode(pwmExternal);
    end;
  end;  
  ExtractWAVForm.Free;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.Clear;
begin
  EditVideoFilename.Text := '';
  EditWAVFilename.Text := '';
  EditSubtitleFilename.Text := '';
  EditProjectFilename.Text := '';
  EditPeakFilename.Text := '';
  chkSaveAsUTF8.Checked := False;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.ConfigureInNewProjectMode;
begin
  Self.Caption := 'New Project';
  bttCreateNewProject.Visible := True;
  bttOk.Visible := False;
  WAVSelectMode(pwmInternal);
  Self.Clear;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.ConfigureInModifyProjectMode(Project : TVSSProject);
begin
  Self.Caption := 'Project Properties';
  bttCreateNewProject.Visible := False;
  bttOk.Visible := True;
  EditVideoFilename.Text := Project.VideoSource;
  EditWAVFilename.Text := Project.WAVFile;
  EditSubtitleFilename.Text := Project.SubtitlesFile;
  EditProjectFilename.Text := Project.Filename;
  EditPeakFilename.Text := Project.PeakFile;
  WAVSelectMode(Project.WAVMode);
  chkSaveAsUTF8.Checked := Project.IsUTF8;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.bttOkClick(Sender: TObject);
begin
  // TODO : do some checking
  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TProjectForm.WAVSelectMode(WavMode : TProjectWAVMode);
begin
  rbInternalWAV.Checked := (WavMode = pwmInternal);
  bttExtractWAVFromVideo.Enabled := (WavMode = pwmInternal);

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
  if rbInternalWAV.Checked then
    Result := pwmInternal
  else if rbExternal.Checked then
    Result := pwmExternal
  else
    Result := pwmPeakOnly
end;

//------------------------------------------------------------------------------
end.
//------------------------------------------------------------------------------
