; ---------------------------------------------------------------------------
; VisualSubSync NSIS install script
; ---------------------------------------------------------------------------

!define NAME "VisualSubSync"
!define VERSION "0.2"
!define OUTFILE "Redist\VisualSubSync-${VERSION}-Setup.exe"
!define INPUT_PATH "Release\"

!define FILE1 "VisualSubSync.exe"
!define FILE2 "WavWriter.dll"

!define DIR_WEB "web\"
!define FILE_WEB1 "index.shtml"
!define FILE_WEB2 "details.shtml"
!define FILE_WEB3 "send_suggestion.shtml"

!define DIR_HELP "help\"

!define UNINST_NAME "VisualSubSync-uninstall.exe"

;--------------------------------
;Include Modern UI

  !include "MUI.nsh"

;--------------------------------
;Configuration

  ;General
  Name "${NAME}"
  OutFile "${OUTFILE}"
  Caption "${NAME} ${VERSION}"
	BrandingText " "

  ;Folder selection page
  InstallDir "$PROGRAMFILES\${NAME}"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKCU "Software\${NAME}" ""
   

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Main (required)" SecMain
	; Make section read only as it's required
	SectionIn RO

  SetOutPath "$INSTDIR"
  
  ; Main files
  File "${INPUT_PATH}${FILE1}"
  File "${INPUT_PATH}${FILE2}"

	; Web files
  SetOutPath "$INSTDIR\${DIR_WEB}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB1}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB2}"
  File "${INPUT_PATH}${DIR_WEB}${FILE_WEB3}"

	; Help files (whole help dir)
  SetOutPath "$INSTDIR\${DIR_HELP}"
  File /r "${INPUT_PATH}${DIR_HELP}*.*"
    
  ; Store install folder
  WriteRegStr HKCU "Software\VisualSubSync" "" $INSTDIR
	  
  ; Create uninstaller
	WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME} (remove only)"
	WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" '"$INSTDIR\${UNINST_NAME}"'  
  WriteUninstaller "$INSTDIR\${UNINST_NAME}"
SectionEnd

;--------------------------------

Section "Start Menu Shortcuts" SecStartShortcut
  CreateDirectory "$SMPROGRAMS\VisualSubSync"
  CreateShortCut "$SMPROGRAMS\VisualSubSync\Uninstall.lnk" "$INSTDIR\${UNINST_NAME}" "" "$INSTDIR\${UNINST_NAME}" 0
  CreateShortCut "$SMPROGRAMS\VisualSubSync\Help.lnk" "$INSTDIR\help\index.html"
  CreateShortCut "$SMPROGRAMS\VisualSubSync\VisualSubSync.lnk" "$INSTDIR\VisualSubSync.exe" "" "$INSTDIR\VisualSubSync.exe" 0
SectionEnd

;--------------------------------
;Descriptions

  LangString DESC_SecMain ${LANG_ENGLISH} "The main program."
  LangString DESC_SecStartShortcut ${LANG_ENGLISH} "Create a shortcut in the start menu."

  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecMain} $(DESC_SecMain)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecStartShortcut} $(DESC_SecStartShortcut)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

	; Delete installer
	Delete "$INSTDIR\${UNINST_NAME}"
	
	; Delete main files
	Delete "$INSTDIR\${FILE1}"
  Delete "$INSTDIR\${FILE2}"
  
  ; Delete web files and directory if empty
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB1}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB2}"
  Delete "$INSTDIR\${DIR_WEB}${FILE_WEB3}"  
  RMDir "$INSTDIR\${DIR_WEB}"
  
	; Delete whole help directory
  RMDir /r "$INSTDIR\${DIR_HELP}"
  
  ; Delete install directory if empty
  RMDir "$INSTDIR"
  
  ; Delete shortcuts
  Delete "$SMPROGRAMS\VisualSubSync\Uninstall.lnk"
  Delete "$SMPROGRAMS\VisualSubSync\Help.lnk"
  Delete "$SMPROGRAMS\VisualSubSync\VisualSubSync.lnk"  
  RMDir "$SMPROGRAMS\VisualSubSync"

	; Delete registry keys
  DeleteRegKey /ifempty HKCU "Software\VisualSubSync"
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
SectionEnd
