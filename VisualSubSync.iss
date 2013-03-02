; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "VisualSubSync"
#define MyAppVersion GetDateTimeString('yyyy/mm/dd', '-', ':');
#define MyAppPublisher "Subfactory.fr"
#define MyAppURL "https://bitbucket.org/spirit/visualsubsync"
#define MyAppExeName "VisualSubSync.exe"

#define LAVFiltersInstaller "LAVFilters-0.55.3.exe"
#define Win7DSFilterTweakerExeName "Win7DSFilterTweaker_5.7.exe"
#define HaaliInstaller "MatroskaSplitter.exe"
#define VCRedistInstaller "vcredist_x86.exe"

#define ReleaseDir = "Release"
#define SetupDir = "setup"
; #define MyAppVersion GetFileVersion(AddBackslash(ReleaseDir) + MyAppExeName)
#define LAVFiltersVersion GetFileVersion(AddBackslash(SetupDir) + LAVFiltersInstaller)
#define Haali "Haali Media Splitter"
#define LAVFilters "LAV Filters"
#define LAVFIltersVerName LAVFilters + " " + LAVFiltersVersion
#define VCRedist "Microsoft Visual C++ 2010 SP1 Redistributable Package (x86)"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{8087A396-8AF5-448F-B781-07DEEF71492E}
;AppId={#MyAppName}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
;InfoBeforeFile=..\Copying.txt
OutputBaseFilename=VisualSubSync-{#MyAppVersion}-Setup
Compression=lzma2/ultra64
SolidCompression=yes
OutputDir=Redist
UninstallDisplayIcon={app}\VisualSubSync.exe
UninstallDisplayName={#MyAppName} {#MyAppVersion}
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[CustomMessages]
AdditionalSoftware=Additional software:
InstallSoftware=Install%1
InstallCodecs=Install%1 and%2
CleanInstall=Clear previous installation files and settings
AssociateExtension=Associate%1 files
OriginalVSS=Original VisualSubSync was found at the specified location and will be uninstalled.

[Tasks]
Name: "desktop_icon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{#MyAppName}:"; Flags: unchecked
Name: "associate_vssprj"; Description: "{cm:AssociateExtension, .vssprj}"; GroupDescription: "{#MyAppName}:"
Name: "clean_install"; Description: "{cm:CleanInstall}"; GroupDescription: "{#MyAppName}:"; Flags: unchecked checkedonce
Name: "codecs"; Description: "{cm:InstallCodecs, {#LAVFIltersVerName}, {#Haali}}"; GroupDescription: {cm:AdditionalSoftware}
Name: "vc_redist"; Description: "{cm:InstallSoftware, {#VCRedist}}"; GroupDescription: {cm:AdditionalSoftware}; Flags: checkedonce

[Files]
Source: "{#ReleaseDir}\VisualSubSync.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#ReleaseDir}\js3215R.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#ReleaseDir}\libhunspell.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#ReleaseDir}\VSSCustomVSFilter.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#ReleaseDir}\WavWriter.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#ReleaseDir}\dict\*"; DestDir: "{app}\dict"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#ReleaseDir}\help\*"; DestDir: "{app}\help"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#ReleaseDir}\jsplugin\*"; DestDir: "{app}\jsplugin"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#ReleaseDir}\presets\*"; DestDir: "{app}\presets"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#ReleaseDir}\vss-companion\*"; DestDir: "{app}\vss-companion"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#ReleaseDir}\web\*"; DestDir: "{app}\web"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SetupDir}\{#LAVFiltersInstaller}"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall; Tasks: codecs
Source: "{#SetupDir}\{#Win7DSFilterTweakerExeName}"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall; Tasks: codecs; MinVersion: 6.1
Source: "{#SetupDir}\{#HaaliInstaller}"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall; Tasks: codecs
Source: "{#SetupDir}\{#VCRedistInstaller}"; DestDir: "{tmp}"; Flags: ignoreversion deleteafterinstall; Tasks: vc_redist
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
;Name: "{group}\Help"; Filename: "{app}\help\index.html"
Name: "{group}\{#MyAppName} on Bitbucket"; Filename: "{#MyAppURL}"
;Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktop_icon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
Filename: "{tmp}\{#LAVFiltersInstaller}"; Parameters: "/silent"; Tasks: codecs
Filename: "{tmp}\{#Win7DSFilterTweakerExeName}"; Parameters: "/silent /h264_x86={{EE30215D-164F-4A92-A4EB-9D4C13390F9F}} /xvid_x86={{EE30215D-164F-4A92-A4EB-9D4C13390F9F}} /aac_x86={{E8E73B6B-4CB3-44A4-BE99-4F7BCB96E491}}"; Tasks: codecs; MinVersion: 6.1
Filename: "{tmp}\{#HaaliInstaller}"; Parameters: "/S"; Tasks: codecs
Filename: "{tmp}\{#VCRedistInstaller}"; Parameters: "/passive /showfinalerror"; Tasks: vc_redist

[Dirs]
;Name: "{app}"; Permissions: users-modify

[Registry]
Root: HKCR; Subkey: ".vssprj"; ValueType: string; ValueName: ""; ValueData: "VisualSubSync.Document"; Flags: uninsdeletevalue; Tasks: associate_vssprj
Root: HKCR; Subkey: "VisualSubSync.Document"; ValueType: string; ValueName: ""; ValueData: "VisualSubSync Project"; Flags: uninsdeletekey; Tasks: associate_vssprj
Root: HKCR; Subkey: "VisualSubSync.Document\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},1"; Tasks: associate_vssprj
Root: HKCR; Subkey: "VisualSubSync.Document\shell\open\command"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName} ""%1"""; Tasks: associate_vssprj

[InstallDelete]
Type: filesandordirs; Name: "{app}"; Tasks: clean_install
Type: filesandordirs; Name: "{localappdata}\VirtualStore\Program Files (x86)\{#MyAppName}"; Tasks: clean_install
Type: filesandordirs; Name: "{localappdata}\VirtualStore\Program Files\{#MyAppName}"; Tasks: clean_install
Type: filesandordirs; Name: "{group}"
Type: files; Name: "{commondesktop}\{#MyAppName}.lnk"; Tasks: clean_install
Type: files; Name: "{userdesktop}\{#MyAppName}.lnk"; Tasks: clean_install

[UninstallDelete]
Type: filesandordirs; Name: "{app}"

[Code]
const
  VCRedistKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{F0C3E5D1-1ADE-321E-8167-68EF0DE699A5}';
  LavFiltersKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\lavfilters_is1';
  CleanInstallTaskIndex = 3;
  LavFiltersTaskIndex = 5;
  VCRedistTaskIndex = 6;

var
  OriginalVSSUninstaller: String;
  OriginalVSSInstalled: Boolean;
  CheckTasksDone : Boolean;

// http://www.lextm.com/2007/08/inno-setup-script-sample-for-version.html
function GetNumber(var temp: String): Integer;
var
  part: String;
  pos1: Integer;
begin
  if Length(temp) = 0 then
  begin
    Result := -1;
    Exit;
  end;
    pos1 := Pos('.', temp);
    if (pos1 = 0) then
    begin
      Result := StrToInt(temp);
    temp := '';
    end
    else
    begin
    part := Copy(temp, 1, pos1 - 1);
      temp := Copy(temp, pos1 + 1, Length(temp));
      Result := StrToInt(part);
    end;
end;

function CompareInner(var temp1, temp2: String): Integer;
var
  num1, num2: Integer;
begin
    num1 := GetNumber(temp1);
  num2 := GetNumber(temp2);
  if (num1 = -1) or (num2 = -1) then
  begin
    Result := 0;
    Exit;
  end;
      if (num1 > num2) then
      begin
        Result := 1;
      end
      else if (num1 < num2) then
      begin
        Result := -1;
      end
      else
      begin
        Result := CompareInner(temp1, temp2);
      end;
end;

function CompareVersion(str1, str2: String): Integer;
var
  temp1, temp2: String;
begin
    temp1 := str1;
    temp2 := str2;
    Result := CompareInner(temp1, temp2);
end;

function HasTweakedAAC(): Boolean;
var
  AACAudioKey: String;
begin
  if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\DirectShow\Preferred', '{000000FF-0000-0010-8000-00AA00389B71}', AACAudioKey) then
  begin
    Result := not (AACAudioKey = '{E1F1A0B8-BEEE-490D-BA7C-066C40B5E2B9}');
  end
  else
  begin
    Result := False;
  end;
end;

function InitializeSetup(): Boolean;
begin
  CheckTasksDone := False;
  Result := True;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if CurPageID = wpSelectDir then
  begin
    OriginalVSSUninstaller := WizardDirValue() + '\VisualSubSync-uninstall.exe';
    if FileExists(OriginalVSSUninstaller) then
    begin
      OriginalVSSInstalled := True;
      if not WizardSilent() then
      begin
        MsgBox(CustomMessage('OriginalVSS'), mbInformation, MB_OK);
      end;
    end
    else
    begin
      OriginalVSSInstalled := False;
    end;
  end;
  Result := True;
end;

procedure CurPageChanged(CurPageID: Integer);
var
  LavFiltersDisplayVersion: String;
begin
  if CurPageID = wpSelectTasks then
  begin
    if OriginalVSSInstalled then
    begin
      WizardForm.TasksList.ItemEnabled[CleanInstallTaskIndex] := False;
      WizardForm.TasksList.Checked[CleanInstallTaskIndex] := True;
    end
    else if DirExists(WizardDirValue()) then
    begin
      WizardForm.TasksList.ItemEnabled[CleanInstallTaskIndex] := True;
    end
    else
    begin
      WizardForm.TasksList.ItemEnabled[CleanInstallTaskIndex] := False;
      WizardForm.TasksList.Checked[CleanInstallTaskIndex] := False;
    end;

    if not CheckTasksDone then
    begin
      if not HasTweakedAAC() then
      begin
        WizardForm.TasksList.Checked[LavFiltersTaskIndex] := True;
      end
      else if RegQueryStringValue(HKEY_LOCAL_MACHINE, LavFiltersKey, 'DisplayVersion', LavFiltersDisplayVersion) then
      begin
        WizardForm.TasksList.Checked[LavFiltersTaskIndex] := CompareVersion(LavFiltersDisplayVersion, '{#LAVFiltersVersion}') < 0;
      end;

      if RegKeyExists(HKEY_LOCAL_MACHINE, VCRedistKey) then
      begin
        WizardForm.TasksList.Checked[VCRedistTaskIndex] := False;
      end
      else
      begin
        WizardForm.TasksList.ItemEnabled[VCRedistTaskIndex] := False;
        WizardForm.TasksList.Checked[VCRedistTaskIndex] := True;
      end;

      CheckTasksDone := True;
    end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  ResultCode: Integer;
begin
  if CurStep = ssInstall then
  begin
    if OriginalVSSInstalled then
    begin
      Exec(OriginalVSSUninstaller, '/S', '', SW_SHOW, ewWaitUntilTerminated, ResultCode);
    end;
  end;
end;
