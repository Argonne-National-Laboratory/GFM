VERSION 5.00
Begin VB.Form frmDescription 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Case Description"
   ClientHeight    =   6260
   ClientLeft      =   2760
   ClientTop       =   3760
   ClientWidth     =   6040
   Icon            =   "frmDescription.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6260
   ScaleWidth      =   6040
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtDescription 
      Height          =   3490
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   1
      Top             =   1920
      Width           =   5772
   End
   Begin VB.TextBox txtTitle 
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Text            =   " "
      Top             =   600
      Width           =   5772
   End
   Begin VB.CommandButton CancelButton 
      BackColor       =   &H00FFFFFF&
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3720
      TabIndex        =   3
      Top             =   5640
      Width           =   1215
   End
   Begin VB.CommandButton OKButton 
      BackColor       =   &H00FFFFFF&
      Caption         =   "OK"
      Height          =   375
      Left            =   1200
      TabIndex        =   2
      Top             =   5640
      Width           =   1215
   End
   Begin VB.Label LbDescription 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Optionally enter description or notes for this case: (use ""Enter"" key for new line)"
      Height          =   490
      Left            =   120
      TabIndex        =   5
      Top             =   1320
      Width           =   5770
   End
   Begin VB.Label LbTitle 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Optionally enter case title to display on left top of screen:"
      Height          =   252
      Left            =   120
      TabIndex        =   4
      Top             =   240
      Width           =   5052
   End
End
Attribute VB_Name = "frmDescription"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Dim case_number As String
Dim case_path As String
Dim flow_domain As Integer
Dim dnm As String
Dim s0 As String, s1 As String
Dim textfile As String
Dim nl As String

Private Sub CancelButton_Click()
    Unload Me 'do not make any changes
End Sub

Private Sub Form_Load()
'Display the case description form with info from case text file when it exists
    'Get public variables declared in the modVariables module that were set in a main
    'form procedure. (Want to use shorter and familiar names.)
    case_number = modVariables.c_name
    flow_domain = modVariables.c_domain
    dnm = modVariables.c_path
    case_path = modVariables.c_casepath
    
    modVariables.c_return = 1 'return failure if something goes wrong (0=> success, 1=> failed)
    
    txtTitle.SelLength = 0 'start text on left
    txtDescription.SelLength = 0
    nl = Chr(13) + Chr(10) 'define newline character
    
    frmDescription.Caption = "Case " & case_number & " Description"
    If flow_domain = 1 Then
        textfile = case_path & "\case" & case_number & "c.txt"
    Else
        textfile = case_path & "\case" & case_number & "m.txt"
    End If
    
    'If case text file exists, then get file and display on form
    If Dir$(textfile) <> "" Then
        On Error GoTo readdone 'title and description are optional
        Open textfile For Input As #9
        Line Input #9, s0 'case number
        Line Input #9, s0 'case title
        txtTitle.Text = s0
        Line Input #9, s0 'blank line
        s0 = ""
        Do While Not EOF(9)
            Line Input #9, s1 'description lines
            s0 = s0 & s1 & nl 'add newline character between lines to display properly
        Loop
readdone:
        Close (9)
        txtDescription.Text = s0
    Else 'display blanks
        txtTitle.Text = ""
        txtDescription.Text = ""
    End If
End Sub

Private Sub OKButton_Click()
    'Create the case name text file
    Open textfile For Output As #9
    Print #9, "Case " & case_number
    Print #9, txtTitle.Text
    Print #9, ""
    Print #9, txtDescription.Text
    Close (9)
    
    modVariables.c_title = txtTitle.Text 'pass title to other form procedures
    modVariables.c_return = 0 'return successful (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub

Private Sub txtDescription_KeyPress(KeyAscii As Integer)
'Change "enter" key to "newline" key to preserve formatting
    If KeyAscii = vbKeyReturn Then
        KeyAscii = Asc(nl)
    End If
End Sub
