;GFM4 Install Script using NSIS (Nullsoft Scriptable Installation System)
;NSIS is an open source project available on sourceforge at http://nsis.sourceforge.net/
;HM NIS Edit is an open source NSIS script editor that functions as a development environment
;  It will compile and run scripts and has a variety of other advanced features.
;  It is available at http://hmne.sourceforge.net/
;Written by Steven Lottes July, 2004
;Last updated by Steven Lottes March, 2007

;---------------------------------------------
; Constant defines

!define PRODUCT_NAME "GFM"
!define PRODUCT_VERSION "4.00.01"
!define PRODUCT_PUBLISHER "Argonne National Laboratory"
!define PRODUCT_WEB_SITE "http://www.anl.gov"
!define PRODUCT_EXE "GFM.exe"
;
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\${PRODUCT_EXE}"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"
!define PRODUCT_STARTMENU_REGVAL "NSIS:StartMenuDir"

!define BUILD_DIR "C:\Argonne\GFM_Release-08-19-15"
;
;--------------------------------------------
; General settings

;SetCompressor lzma ;Uncomment to improve compression ratios, slows compression
BrandingText "ANL Glass Furnace Model Installer 4.00.01" ;Light text at bottom of page

; MUI 1.67 compatible (MUI is Modern User Interface, see NSIS documentation)
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
;!insertmacro MUI_DEFAULT MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-colorful.ico"
;!insertmacro MUI_DEFAULT MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall-colorful.ico"
!insertmacro MUI_DEFAULT MUI_ICON "${BUILD_DIR}\install_files\GFM_Install.ico"
!insertmacro MUI_DEFAULT MUI_UNICON "${BUILD_DIR}\install_files\GFM_Uninstall.ico"

;--------------------------------
;Pages (windows that appear during the install)

;      pages appear during an install in the order listed below
;      most pages have a standard format and functions, such as choosing the install directory
;      titles & labels on pages can be modified with defines of appropriate strings

; Welcome page
!insertmacro MUI_PAGE_WELCOME

; Open source license page (constructed by modifying a license page)

!define MUI_PAGE_HEADER_TEXT "Open Source License"
!define MUI_PAGE_HEADER_SUBTEXT \
        "Please review the license before installing ${PRODUCT_NAME} ${PRODUCT_VERSION}"
!define MUI_LICENSEPAGE_TEXT_TOP ""; "Open Source License"
!define MUI_LICENSEPAGE_TEXT_BOTTOM "Click Acknowledge to Continue"
;!define MUI_LICENSEPAGE_TEXT_BOTTOM "You must acknowledge accepting the license agreement"
;!define MUI_LICENSEPAGE_BUTTON "OK"
!define MUI_LICENSEPAGE_BUTTON "Acknowlege"
!insertmacro MUI_PAGE_LICENSE "${BUILD_DIR}\documents-source\Legal\GFM4_License.rtf"

;;;; License page

;;;;!insertmacro MUI_PAGE_LICENSE "${BUILD_DIR}\documents-source\Legal\GFM_Trial_License.rtf"

;;;; Product Serial Number Validation Page

;;Page custom Pre_Custom_Page_SerialNumber Post_Custom_Page_SerialNumber



; Components Page



!insertmacro MUI_PAGE_COMPONENTS

; Directory page

!insertmacro MUI_PAGE_DIRECTORY

; Start menu page

var ICONS_GROUP
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "GFM"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${PRODUCT_STARTMENU_REGVAL}"
!insertmacro MUI_PAGE_STARTMENU Application $ICONS_GROUP

; Instfiles page

!insertmacro MUI_PAGE_INSTFILES

; Finish page

!define MUI_FINISHPAGE_RUN "$INSTDIR\${PRODUCT_EXE}"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\documents\readme.txt"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages

!insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------------------
; Language files
!insertmacro MUI_LANGUAGE "English"
;--------------------------------------------
; Reserve files

  ;These files should be inserted before other files in the data block
  ;Keep these lines before any File command
  ;Only for solid compression (by default, solid compression is enabled for BZIP2 and LZMA)

;;;;;;;;;;;;;;;;;ReserveFile "ioSerialNumber.ini"
!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "GFM_Setup.exe"
InstallDir "$PROGRAMFILES\GFM"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show
;--------------------------------
;Variables

  Var INI_VALUE

;---------------------------------
;Installer Sections

Section "GFM Application" SecGFM
  SetOutPath "$INSTDIR"
  SetOverwrite ifnewer
  File "${BUILD_DIR}\${PRODUCT_EXE}"
  CreateDirectory "$SMPROGRAMS\$ICONS_GROUP"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\GFM.lnk" "$INSTDIR\${PRODUCT_EXE}"
  ;CreateShortCut "$DESKTOP\GFM.lnk" "$INSTDIR\${PRODUCT_EXE}"

  ;------------------------------
  SetOutPath "$INSTDIR\bin"
  ;------------------------------
  File "${BUILD_DIR}\bin\RunPlot.exe"
  ;File "${BUILD_DIR}\bin\comb.exe"
  File "${BUILD_DIR}\combustion\debug\comb.exe"
  ;File "${BUILD_DIR}\bin\melt.exe"
  File "${BUILD_DIR}\melt\debug\melt.exe"

  ;------------------------------
  SetOutPath "$INSTDIR\documents"
  ;------------------------------

  File "${BUILD_DIR}\documents\base_relaxfactorc.txt"
  File "${BUILD_DIR}\documents\base_relaxfactorm.txt"
  File "${BUILD_DIR}\documents-source\Legal\Argonne_BSD_Open_Source_Software_License.pdf"
  ;File "${BUILD_DIR}\documents\GFM4-Capabilities.pdf"
  File "${BUILD_DIR}\documents\GFM4-Final-Report.pdf"
  File "${BUILD_DIR}\documents\GFM4-Cycle.pdf"
  File "${BUILD_DIR}\documents\GFM4-Files.pdf"
  File "${BUILD_DIR}\documents\GFM4-Menus.pdf"
  File "${BUILD_DIR}\documents\GFM4-Tips-Procedures.pdf"
  File "${BUILD_DIR}\documents\GFM4-Using-GUI.pdf"
  File "${BUILD_DIR}\documents\GFM4_Release-Notice.pdf"
  File "${BUILD_DIR}\documents\RunPlot.pdf"
  File "${BUILD_DIR}\documents\readme.txt"
  File "${BUILD_DIR}\documents\furnace.jpg"

;/*
  ;------------------------------
  SetOutPath "$INSTDIR\combustion"
  ;------------------------------

  File "${BUILD_DIR}\combustion\Kinetic.d"
  File "${BUILD_DIR}\combustion\relaxfactorc.txt"
  File "${BUILD_DIR}\combustion\runs.dat"

  ;-------------------------------
  ;Sample case TC21 problem 40 TPD
  ;------------------------------
  SetOutPath "$INSTDIR\combustion\case0040"
  ;------------------------------

  File "${BUILD_DIR}\combustion\case0040\*.*"
  ;File "${BUILD_DIR}\combustion\case0040\case0040c.txt"
  ;File "${BUILD_DIR}\combustion\case0040\gd0040c.dat"
  ;File "${BUILD_DIR}\combustion\case0040\gd0040c.pre"
  ;File "${BUILD_DIR}\combustion\case0040\sbc0040c.dat"
  ;File "${BUILD_DIR}\combustion\case0040\it0040t.dat"

  ;------------------------------
  SetOutPath "$INSTDIR\combustion\case0041"
  ;------------------------------

  File "${BUILD_DIR}\combustion\case0041\*.*"
  ;File "${BUILD_DIR}\combustion\case0041\case0041c.txt"
  ;File "${BUILD_DIR}\combustion\case0041\gd0041c.dat"
  ;File "${BUILD_DIR}\combustion\case0041\gd0041c.pre"
  ;File "${BUILD_DIR}\combustion\case0041\sbc0041c.dat"
  ;File "${BUILD_DIR}\combustion\case0041\it0041t.dat"

  ;-------------------------------
  ;Sample case TC21 problem 25 TPD
  ;------------------------------
  SetOutPath "$INSTDIR\combustion\case0020"
  ;------------------------------

  ;File "${BUILD_DIR}\combustion\case0020\*.*"
  File "${BUILD_DIR}\combustion\case0020\case0020c.txt"
  File "${BUILD_DIR}\combustion\case0020\gd0020c.dat"
  File "${BUILD_DIR}\combustion\case0020\gd0020c.pre"
  File "${BUILD_DIR}\combustion\case0020\sbc0020c.dat"
  ;File "${BUILD_DIR}\combustion\case0020\it0020t.dat"

  ;------------------------------
  SetOutPath "$INSTDIR\combustion\case0021"
  ;------------------------------

  ;File "${BUILD_DIR}\combustion\case0021\*.*"
  File "${BUILD_DIR}\combustion\case0021\case0021c.txt"
  File "${BUILD_DIR}\combustion\case0021\gd0021c.dat"
  File "${BUILD_DIR}\combustion\case0021\gd0021c.pre"
  File "${BUILD_DIR}\combustion\case0021\sbc0021c.dat"
  ;File "${BUILD_DIR}\combustion\case0021\it0021t.dat"

  ;------------------------------
  ;SetOutPath "$INSTDIR\lib"
  ;------------------------------

  ;File "${BUILD_DIR}\lib\furnace.jpg"
  ;File "${BUILD_DIR}\lib\gfm2.cnt"
  ;File "${BUILD_DIR}\lib\gfm2.GID"
  ;File "${BUILD_DIR}\lib\GFM2.HLP"
  ;File "${BUILD_DIR}\lib\gfm2.hpj"
  ;File "${BUILD_DIR}\lib\gfm2.rtf"
  ;File "${BUILD_DIR}\lib\plot.jpg"

  ;------------------------------
  SetOutPath "$INSTDIR\melt"
  ;------------------------------

  File "${BUILD_DIR}\melt\relaxfactorm.txt"
  File "${BUILD_DIR}\melt\runs.dat"
  
  ;-------------------------------
  ;Sample case TC21 problem 40 TPD
  ;------------------------------
  SetOutPath "$INSTDIR\melt\case0040"
  ;------------------------------

  File "${BUILD_DIR}\melt\case0040\*.*"
  ;File "${BUILD_DIR}\melt\case0040\case0040m.txt"
  ;File "${BUILD_DIR}\melt\case0040\gd0040m.dat"
  ;File "${BUILD_DIR}\melt\case0040\gd0040m.pre"
  ;File "${BUILD_DIR}\melt\case0040\SBC0040M.DAT"
  ;File "${BUILD_DIR}\melt\case0040\it0040m.dat"

  ;-------------------------------
  ;Sample case TC21 problem 25 TPD
  ;------------------------------
  SetOutPath "$INSTDIR\melt\case0020"
  ;------------------------------

  ;File "${BUILD_DIR}\melt\case0020\*.*"
  File "${BUILD_DIR}\melt\case0020\case0020m.txt"
  File "${BUILD_DIR}\melt\case0020\gd0020m.dat"
  File "${BUILD_DIR}\melt\case0020\gd0020m.pre"
  File "${BUILD_DIR}\melt\case0020\SBC0020M.DAT"
  File "${BUILD_DIR}\melt\case0020\it0020m.dat"

;*/
  CreateDirectory "$INSTDIR\tmp"

SectionEnd

!include "UpgradeDLL.nsh"

!define VBFILESDIR "${BUILD_DIR}\vb6run" ;Location of VB6 runtime files
;/*
Section /o "Install VB6 Runtime Files" SecVB6Run

  !insertmacro UpgradeDLL ${VBFILESDIR}\Comcat.dll   $SYSDIR\Comcat.dll   $SYSDIR
  !insertmacro UpgradeDLL ${VBFILESDIR}\Msvbvm60.dll $SYSDIR\Msvbvm60.dll $SYSDIR
  !insertmacro UpgradeDLL ${VBFILESDIR}\Oleaut32.dll $SYSDIR\Oleaut32.dll $SYSDIR
  !insertmacro UpgradeDLL ${VBFILESDIR}\Olepro32.dll $SYSDIR\Olepro32.dll $SYSDIR
  !insertmacro UpgradeDLL ${VBFILESDIR}\COMDLG32.OCX $SYSDIR\COMDLG32.OCX $SYSDIR
  
  !define UPGRADEDLL_NOREGISTER
    !insertmacro UpgradeDLL ${VBFILESDIR}\Asycfilt.dll $SYSDIR\Asycfilt.dll $SYSDIR
    !insertmacro UpgradeDLL ${VBFILESDIR}\Stdole2.tlb $SYSDIR\Stdole2.tlb $SYSDIR
  !undef UPGRADEDLL_NOREGISTER

  ;Only increase DLL count on new installation
  ;Replace myprog.exe or use another detection method
  IfFileExists $INSTDIR\${PRODUCT_EXE} skipAddSharedDLL
    Push $SYSDIR\Asycfilt.dll
    Call AddSharedDLL
    Push $SYSDIR\Comcat.dll
    Call AddSharedDLL
    Push $SYSDIR\Msvbvm60.dll
    Call AddSharedDLL
    Push $SYSDIR\Oleaut32.dll
    Call AddSharedDLL
    Push $SYSDIR\Olepro32.dll
    Call AddSharedDLL
    Push $SYSDIR\Stdole2.tlb
    Call AddSharedDLL
    Push $SYSDIR\COMDLG32.OCX
    Call AddSharedDLL
  skipAddSharedDLL:

SectionEnd
;*/

Section /o "GFM desktop icon" SecGFMDI
  CreateShortCut "$DESKTOP\GFM.lnk" "$INSTDIR\${PRODUCT_EXE}"
SectionEnd

Section /o "RunPlot desktop icon" SecRunPlotDI
  CreateShortCut "$DESKTOP\RunPlot.lnk" "$INSTDIR\bin\RunPlot.exe"
SectionEnd


Section -AdditionalIcons
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk" "$INSTDIR\uninstall.exe"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\RunPlot.lnk" "$INSTDIR\bin\RunPlot.exe"
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninstall.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\${PRODUCT_EXE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\${PRODUCT_EXE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "${PRODUCT_STARTMENU_REGVAL}" "$ICONS_GROUP"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecGFM    ${LANG_ENGLISH} "GFM application and sample files."
  LangString DESC_SecVB6Run ${LANG_ENGLISH} "Installs VB6 runtime files, \
                                does not overwrite more current versions."
  LangString DESC_SecGFMDI ${LANG_ENGLISH} "Desktop shortcut to GFM program."
  LangString DESC_SecRunPlotDI ${LANG_ENGLISH} "Desktop shortcut to RunPlot monitoring program."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecGFM} $(DESC_SecGFM)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecVB6Run} $(DESC_SecVB6Run)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecGFMDI} $(DESC_SecGFMDI)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecRunPlotDI} $(DESC_SecRunPlotDI)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;---------------------------------------------
;Installer Functions

;Function .onInit

  ;Extract InstallOptions INI files

;;;;;;  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ioSerialNumber.ini"

;FunctionEnd

;;LangString TEXT_IO_TITLE ${LANG_ENGLISH} "GFM4 Serial Number Validation"
;LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "This is a page created using the InstallOptions plug-in."

;;;;;;;Function Pre_Custom_Page_SerialNumber

;;;;;;;  !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "" ;"$(TEXT_IO_SUBTITLE)"
;;;;;;;  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ioSerialNumber.ini"

;;;;;;;;;FunctionEnd

;;;;;;;;;Function Post_Custom_Page_SerialNumber

  ;Read a value from an InstallOptions INI file
;;  !insertmacro MUI_INSTALLOPTIONS_READ $INI_VALUE "ioSerialNumber.ini" "Field 2" "State"

  ;Display a messagebox if serial number invalid
  ;StrCmp $INI_VALUE "GFM-serial-number-string-1" Serial_OK 0
;;;  StrCmp $INI_VALUE ${SERIAL_TO_INSTALL} Serial_OK 0
;;;    MessageBox MB_OK "Serial number invalid"
;;;    Abort
;;;  Serial_OK:

;;;FunctionEnd

 ; AddSharedDLL - used to increment use count for VB6 runtime files
 ;
 ; Increments a shared DLLs reference count.
 ; Use by passing one item on the stack (the full path of the DLL).
 ;
 ; Usage:
 ;   Push $SYSDIR\sharedDll.dll
 ;   Call AddSharedDLL
 ;

 Function AddSharedDLL
   Exch $R1
   Push $R0
   ReadRegDword $R0 HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
   IntOp $R0 $R0 + 1
   WriteRegDWORD HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1 $R0
   Pop $R0
   Pop $R1
 FunctionEnd

;---------------------------------------------
;unInstaller Functions

Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) was successfully removed from your computer."
FunctionEnd

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to completely remove $(^Name) and all of its components?" IDYES +2
  Abort
FunctionEnd

; During the uninstallation, the un.DecrementSharedDLL function below is used to decrement
; the shared DLL count for VB6 runtime files (never remove the files, because the shared
; DLL count is not reliable enough to use for a decision to delete such important files).
; Use by passing one item on the stack (the full path of the DLL).

; Usage:
;   Push $SYSDIR\sharedDll.dll
;   Call un.DecrementSharedDLL

Function un.DecrementSharedDLL
  Exch $R1
  Push $R0
  ReadRegDword $R0 HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
  StrCmp $R0 "" done
    IntOp $R0 $R0 - 1
    IntCmp $R0 0 rk rk uk
    rk:
      DeleteRegValue HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1
      Goto done
    uk:
      WriteRegDWORD HKLM Software\Microsoft\Windows\CurrentVersion\SharedDLLs $R1 $R0
  done:
  Pop $R0
  Pop $R1
FunctionEnd

;--------------------------------
;Uninstaller Section

Section Uninstall
  ReadRegStr $ICONS_GROUP ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "${PRODUCT_STARTMENU_REGVAL}"
  Delete "$INSTDIR\uninst.exe"
  Delete "$INSTDIR\${PRODUCT_EXE}"

  Delete "$INSTDIR\bin\RunPlot.exe"
  Delete "$INSTDIR\bin\comb.exe"
  Delete "$INSTDIR\bin\melt.exe"
  RMDir  "$INSTDIR\bin"

  ;Delete "$INSTDIR\Blank_furnace_form.xls"
  Delete "$INSTDIR\documents\*.*"
  ;Delete "$INSTDIR\documents\base_relaxfactorm.txt"
  ;Delete "$INSTDIR\documents\GFM4-Capabilities.pdf"
  ;Delete "$INSTDIR\documents\GFM4-Changes.pdf"
  ;Delete "$INSTDIR\documents\GFM4-Cycle.pdf"
  ;Delete "$INSTDIR\documents\GFM4-Files.pdf"
  ;Delete "$INSTDIR\documents\GFM4-Menus.pdf"
  ;Delete "$INSTDIR\documents\GFM4-Procedures.pdf"
  ;Delete "$INSTDIR\documents\GFM4-WorkFlow.pdf"
  ;Delete "$INSTDIR\documents\readme.txt"
  ;Delete "$INSTDIR\documents\RunPlot.pdf"
  ;Delete "$INSTDIR\documents\furnace.jpg"
  RMDir  "$INSTDIR\documents"

;/*
  Delete "$INSTDIR\combustion\Kinetic.d"
  Delete "$INSTDIR\combustion\relaxfactorc.txt"
  Delete "$INSTDIR\combustion\runs.dat"
  ;Delete "$INSTDIR\combustion\Release\comb.exe"
  ;RMDir  "$INSTDIR\combustion\Release"

  ;Delete "$INSTDIR\lib\furnace.jpg"
  ;Delete "$INSTDIR\lib\gfm2.cnt"
  ;Delete "$INSTDIR\lib\gfm2.GID"
  ;Delete "$INSTDIR\lib\GFM2.HLP"
  ;Delete "$INSTDIR\lib\gfm2.hpj"
  ;Delete "$INSTDIR\lib\gfm2.rtf"
  ;Delete "$INSTDIR\lib\plot.jpg"
  ;RMDir  "$INSTDIR\lib"
  
  Delete "$INSTDIR\melt\relaxfactorm.txt"
  Delete "$INSTDIR\melt\runs.dat"
  ;Delete "$INSTDIR\melt\Release\melt.exe"
  ;RMDir  "$INSTDIR\melt\Release"
;*/
  Delete "$INSTDIR\tmp\*.*"
  RMDir "$INSTDIR\tmp"

; Decrement use count in registry on uninstall for VB6 runtime files
;/*
  Push $SYSDIR\Asycfilt.dll
  Call un.DecrementSharedDLL
  Push $SYSDIR\Comcat.dll
  Call un.DecrementSharedDLL
  Push $SYSDIR\Msvbvm60.dll
  Call un.DecrementSharedDLL
  Push $SYSDIR\Oleaut32.dll
  Call un.DecrementSharedDLL
  Push $SYSDIR\Olepro32.dll
  Call un.DecrementSharedDLL
  Push $SYSDIR\Stdole2.tlb
  Call un.DecrementSharedDLL
  Push $SYSDIR\COMDLG32.OCX
  Call un.DecrementSharedDLL
;*/

  Delete "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk"
  Delete "$DESKTOP\GFM.lnk"
  Delete "$DESKTOP\RunPlot.lnk"
  Delete "$SMPROGRAMS\$ICONS_GROUP\GFM.lnk"
  Delete "$SMPROGRAMS\$ICONS_GROUP\RunPlot.lnk"

  RMDir "$SMPROGRAMS\$ICONS_GROUP"

  ;RMDir "$INSTDIR"

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd
