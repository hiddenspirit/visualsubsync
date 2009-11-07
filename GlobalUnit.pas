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

unit GlobalUnit;

interface

uses MiscToolsUnit, Forms, WAVDisplayerUnit, ProjectUnit, SysUtils;

type
  TContext = class
    Parent : TContext;
    function GetFieldValue(Name : string; var Found : Boolean) : string; virtual; abstract;
  end;

  TGlobalContext = class(TContext)
    WavAverageBytePerSecond : Integer;
    SubList : TRangeList;
    CurrentProject : TVSSProject;
    function GetFieldValue(Name : string; var Found : Boolean) : string; override;
  end;

  procedure CheckBackupDirectory;

const
  ApplicationName : string = 'VisualSubSync';

var
  g_ApplicationVersion : TFileVersion;
  g_GlobalContext : TGlobalContext;
  // Synchronization object from application<->web server threads
  g_WebRWSynchro : TMultiReadExclusiveWriteSynchronizer;
  g_BackupDirectory : WideString;
  g_PluginPath : WideString;
  g_ApplicationPath : WideString;
  g_PresetsPath : WideString;
  g_DictPath : WideString;
  g_WavExtractorGraphDebugInfo : WideString;
  g_VideoGraphDebugInfo : WideString;

implementation

uses TntSysUtils, TntForms;

function TGlobalContext.GetFieldValue(Name : string; var Found : Boolean) : string;
begin
  Found := True;
  if (Name = 'version') then
    Result := g_ApplicationVersion.VersionString
  else if (Name = 'project-filename') then
    Result := ExtractFileName(CurrentProject.Filename)
  else
    Found := False
end;

procedure CheckBackupDirectory;
begin
  if not WideDirectoryExists(g_BackupDirectory) then
    WideForceDirectories(g_BackupDirectory);
end;

initialization
  g_ApplicationVersion := TFileVersion.Create(Application.ExeName);
  g_GlobalContext := TGlobalContext.Create;
  g_WebRWSynchro := TMultiReadExclusiveWriteSynchronizer.Create;
  g_ApplicationPath := WideIncludeTrailingBackslash(WideExtractFilePath(TntApplication.ExeName));
  g_BackupDirectory := g_ApplicationPath + 'Backup\';
  CheckBackupDirectory;
  g_PluginPath := g_ApplicationPath + 'jsplugin\';
  g_PresetsPath := g_ApplicationPath + 'presets\';
  g_DictPath := g_ApplicationPath + 'dict\';
  
finalization
  g_WebRWSynchro.Free;
  g_WebRWSynchro := nil;
  g_GlobalContext.Free;
  g_GlobalContext := nil;
  g_ApplicationVersion.Free;
  g_ApplicationVersion := nil;
  
end.

