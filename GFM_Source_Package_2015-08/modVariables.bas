Attribute VB_Name = "modVariables"

'Basic window and communication info
Public Declare Function FindWindow Lib "user32" _
Alias "FindWindowA" (ByVal lpClassName As String, _
ByVal lpWindowName As String) As Long
Public Declare Function SetPriorityClass Lib _
"kernel32" (ByVal hProcess As Long, _
ByVal dwPriorityClass As Long) As Long
Public Declare Function GetPriorityClass Lib _
"kernel32" (ByVal hProcess As Long) As Long
Public Declare Function SendMessage Lib "user32" _
Alias "SendMessageA" (ByVal hwnd As Long, _
ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) _
As Long
Public Declare Function GetThreadPriority Lib "kernel32" _
(ByVal hThread As Long) As Long
Public Declare Function SetThreadPriority Lib "kernel32" _
(ByVal hThread As Long, ByVal nPriority As Long) As Long


'Following used for opening PDFs for viewing
Public Declare Function ShellExecute Lib "shell32.dll" _
Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, _
ByVal lpFile As String, ByVal lpParameters As String, _
ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long
Public Const SW_SHOW = 5



'Declarations needed to end an application that was started
'by the shell function (originally all private)
Public Const PROCESS_ALL_ACCESS = &H1F0FFF

Public Declare Function OpenProcess Lib "kernel32" _
  (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, _
   ByVal dwProcessId As Long) As Long

Public Declare Function GetExitCodeProcess Lib "kernel32" _
   (ByVal hProcess As Long, lpExitCode As Long) As Long

Public Declare Function TerminateProcess Lib "kernel32" _
   (ByVal hProcess As Long, ByVal uExitCode As Long) As Long


'For getting list of files in a directory:
'FindFirstFile & FindNextFile
Public Type FILETIME
        dwLowDateTime As Long
        dwHighDateTime As Long
End Type
Public Const MAX_PATH = 260
Public Type WIN32_FIND_DATA
        dwFileAttributes As Long
        ftCreationTime As FILETIME
        ftLastAccessTime As FILETIME
        ftLastWriteTime As FILETIME
        nFileSizeHigh As Long
        nFileSizeLow As Long
        dwReserved0 As Long
        dwReserved1 As Long
        cFileName As String * MAX_PATH
        cAlternate As String * 14
End Type
Public Const FILE_ATTRIBUTE_DIRECTORY = &H10

Public Declare Function FindFirstFile Lib "kernel32" _
  Alias "FindFirstFileA" (ByVal lpFileName As String, _
  lpFindFileData As WIN32_FIND_DATA) As Long
Public Declare Function FindNextFile Lib "kernel32" _
  Alias "FindNextFileA" (ByVal hFindFile As Long, _
  lpFindFileData As WIN32_FIND_DATA) As Long


'Variables to communicate between forms
Public c_name As String 'case_number
Public c_domain As Integer 'case domain
    '1 => combustion space
    '2 => melter space
Public c_path As String 'application path current directory
Public c_title As String 'case string
Public c_casepath As String 'path for case folder
Public c_return As Integer '0=> success, 1=> failed
Public c_action As String 'open, delete, or simulate
Public c_close As Integer '1=> ignore, 2=> save, 3=> abort

Public c_cycleEnd As Integer 'number of cycle to run
Public c_cycle2_gitr As Integer 'second and subsequent cycle maximum gas iterations
Public c_cycle2_msitr As Integer 'second and subsequent cycle minor species iterations
Public c_cycle2_rintv As Integer 'second and subsequent cycle radiation interval
Public c_cycle2_mitr As Integer 'second and subsequent cycle maximum melt iterations
Public c_cycle2_scale_on As Integer 'number of cycles to have scaling on

'Variables for ChooseData form
'combustion data collection print flags
Public c_isum As Integer 'Summary
Public c_iinfo As Integer 'General Information
Public c_iTave As Integer 'Average Temperatures
Public c_iconv As Integer 'Mass Residual Convergence
Public c_igresid As Integer 'Equation Residuals
Public c_igresidp As Integer 'Pre-Solve Equation Residuals
Public c_igresidx As Integer 'Extra Equation Residuals
Public c_igresidxp As Integer 'Extra Pre-Solve Equation Residuals
Public c_imresid As Integer 'Minor Species Residual Convergence
Public c_irad_detail As Integer 'Radiation Details
Public c_irad_rad As Integer 'Radiosity Convergence
Public c_itwal As Integer 'Wall Temperature
Public c_irelax As Integer 'Temperature Relaxation
Public c_iflx As Integer 'Melt Surface Flux Change
Public c_ifieldview As Integer 'Output for FieldView
'melt data collection print flags
Public c_isum_m As Integer 'Summary
Public c_iinfo_m As Integer 'General Information
Public c_iTave_m As Integer 'Average Temperatures
Public c_iconv_m As Integer 'Mass Residual Convergence
Public c_igresid_m As Integer 'Equation Residuals
Public c_igresidp_m As Integer 'Pre-Solve Equation Residuals
Public c_iTchg As Integer 'Melt Surface Temperature Change
Public c_iadjf As Integer 'Melt Surface Adjusted Flux
Public c_iadjr As Integer 'Melt Surface Flux Relaxtion
Public c_ifieldview_m As Integer 'Output for FieldView

Public Function trim_nulls(x As String) As String
  a = InStr(x, vbNullChar)
  If a = 0 Then trim_nulls = x Else trim_nulls = Left(x, a - 1)
End Function


Public Function EndShelledProcess(ShellReturnValue As Long) _
   As Boolean

'PURPOSE: End a process started with VB's Shell Statement
'INPUT: Task ID returned by Shell
'RETURNS: True if succesful, false otherwise

On Error Resume Next

Dim hInst As Long
Dim hProcess As Long
Dim lExitCode As Long
Dim lRet As Long

hInst = ShellReturnValue
If hInst = 0 Then Exit Function

'Get handle to process
hProcess = OpenProcess(PROCESS_ALL_ACCESS, 0&, hInst)
If hProcess <> 0 Then
    'get exit code
    GetExitCodeProcess hProcess, lExitCode
        If lExitCode <> 0 Then
                'bye-bye
            lRet = TerminateProcess(hProcess, lExitCode)
            EndShelledProcess = lRet > 0
        End If
End If

End Function

