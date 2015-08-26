VERSION 5.00
Begin VB.Form frmExistingCase 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select Existing Case Folder"
   ClientHeight    =   6264
   ClientLeft      =   2760
   ClientTop       =   3756
   ClientWidth     =   6036
   Icon            =   "frmExistingCase.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6264
   ScaleWidth      =   6036
   ShowInTaskbar   =   0   'False
   Begin VB.DirListBox dlbCases 
      Height          =   4824
      Left            =   840
      TabIndex        =   2
      Top             =   480
      Width           =   4452
   End
   Begin VB.CommandButton CancelButton 
      BackColor       =   &H00FFFFFF&
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3720
      TabIndex        =   1
      Top             =   5640
      Width           =   1215
   End
   Begin VB.CommandButton ActionButton 
      BackColor       =   &H00FFFFFF&
      Caption         =   "Open"
      Height          =   375
      Left            =   1200
      TabIndex        =   0
      Top             =   5640
      Width           =   1215
   End
End
Attribute VB_Name = "frmExistingCase"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Dim case_number As String
Dim case_path As String
Dim case_action As String
Dim flow_domain As Integer
Dim dnm As String

Private Sub CancelButton_Click()
    Unload Me 'do not make any changes
End Sub



'Private Sub dlbCases_Change()
'    case_path = dlbCases.Path
    'Call OpenButton_Click
'End Sub

Private Sub dlbCases_Click()
    case_path = dlbCases.List(dlbCases.ListIndex)
End Sub

Private Sub Form_Load()
'Display the Select Existing Case form with info from case text file when it exists
    'Get public variables declared in the modVariables module that were set in a main
    'form procedure. (Want to use shorter and familiar names.)
    flow_domain = modVariables.c_domain
    dnm = modVariables.c_path
    case_number = ""
    case_path = ""
    case_action = modVariables.c_action
    frmExistingCase.Caption = "Select Existing Case to " & case_action
    ActionButton.Caption = case_action
    
    modVariables.c_return = 1 'return failure if something goes wrong (0=> success, 1=> failed)
    
    If flow_domain = 1 Then
        dlbCases.Path = dnm & "combustion"
    Else
        dlbCases.Path = dnm & "melt"
    End If
    case_path = dlbCases.Path
    'ChDir (dlbCases.Path)
    
End Sub

Private Sub ActionButton_Click()

    'Get case number from path
    case_number = Right(case_path, 4)
    
    If Val(case_number) < 1 Or Val(case_number) > 9999 Then
        Call MsgBox("Choose a case#### folder. Try again.", vbOKOnly, "GFM")
        Exit Sub
    End If
    
    modVariables.c_name = case_number 'pass case number to other form procedures
    modVariables.c_casepath = case_path 'pass case path to other form procedures
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub
