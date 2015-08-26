VERSION 5.00
Begin VB.Form frmCaseCreate 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Create New Case"
   ClientHeight    =   6264
   ClientLeft      =   2760
   ClientTop       =   3756
   ClientWidth     =   6036
   Icon            =   "frmCaseCreate.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6264
   ScaleWidth      =   6036
   ShowInTaskbar   =   0   'False
   Begin VB.DirListBox dlbCases 
      Height          =   3312
      Left            =   840
      TabIndex        =   4
      Top             =   2040
      Width           =   4452
   End
   Begin VB.TextBox txtCaseNum 
      BeginProperty DataFormat 
         Type            =   1
         Format          =   "0"
         HaveTrueFalseNull=   0
         FirstDayOfWeek  =   0
         FirstWeekOfYear =   0
         LCID            =   1033
         SubFormatType   =   1
      EndProperty
      Height          =   375
      Left            =   4440
      TabIndex        =   0
      Text            =   " "
      Top             =   360
      Width           =   1092
   End
   Begin VB.CommandButton CancelButton 
      BackColor       =   &H00FFFFFF&
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3720
      TabIndex        =   2
      Top             =   5640
      Width           =   1215
   End
   Begin VB.CommandButton OKButton 
      BackColor       =   &H00FFFFFF&
      Caption         =   "OK"
      Height          =   375
      Left            =   1200
      TabIndex        =   1
      Top             =   5640
      Width           =   1215
   End
   Begin VB.Label LbDontUse 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Do NOT use any of the existing case numbers shown below."
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   732
      Left            =   120
      TabIndex        =   5
      Top             =   960
      Width           =   3852
   End
   Begin VB.Label LbCase 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Enter a new case number from 1 to 9999. "
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   492
      Left            =   120
      TabIndex        =   3
      Top             =   360
      Width           =   3732
   End
End
Attribute VB_Name = "frmCaseCreate"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Dim case_number As String
Dim case_path As String
Dim flow_domain As Integer
Dim dnm As String

Private Sub CancelButton_Click()
    Unload Me 'do not make any changes
End Sub

Private Sub Form_Load()
'Display the Create New Case form with info from case text file when it exists
    'Get public variables declared in the modVariables module that were set in a main
    'form procedure. (Want to use shorter and familiar names.)
    flow_domain = modVariables.c_domain
    dnm = modVariables.c_path
    case_number = ""
    case_path = ""
    
    modVariables.c_return = 1 'return failure if something goes wrong (0=> success, 1=> failed)
    
    If flow_domain = 1 Then
        dlbCases.Path = dnm & "combustion"
    Else
        dlbCases.Path = dnm & "melt"
    End If
    ChDir (dlbCases.Path)
    
    txtCaseNum.SelLength = 0 'start text on left
    txtCaseNum.Text = ""
End Sub

Private Sub OKButton_Click()
    Dim tempstr As String
    'Verify case number is valid
    case_number = txtCaseNum.Text
    If Val(case_number) < 1 Or Val(case_number) > 9999 Then
        Call MsgBox("Case number incorrect. Try again.", vbOKOnly, "GFM")
        Exit Sub
    End If
    
    'Expand case number to 4 digits if needed
    Do While Len(case_number) < 4
        case_number = "0" & case_number
    Loop
 
    'case_path = dlbCases.Path & "\case" & case_number & "\"
    case_path = dlbCases.Path & "\case" & case_number
    tempstr = Dir$(case_path, vbDirectory)
    
    If Dir$(case_path, vbDirectory) <> "" Then
        Call MsgBox("Case number already in use.  Enter a new case number.", vbOKOnly, "GFM")
        Exit Sub
    End If

    MkDir (case_path) 'Create case folder
    
    modVariables.c_name = case_number 'pass case number to other form procedures
    modVariables.c_casepath = case_path 'pass case path to other form procedures
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub
