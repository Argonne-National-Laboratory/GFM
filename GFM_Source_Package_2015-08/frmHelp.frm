VERSION 5.00
Begin VB.Form frmHelp 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select GFM Documentation to View"
   ClientHeight    =   4380
   ClientLeft      =   2760
   ClientTop       =   3760
   ClientWidth     =   6720
   Icon            =   "frmHelp.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4380
   ScaleWidth      =   6720
   ShowInTaskbar   =   0   'False
   Begin VB.ListBox LstDocs 
      BackColor       =   &H00FFFF00&
      Height          =   2040
      Left            =   600
      TabIndex        =   2
      Top             =   1320
      Width           =   5530
   End
   Begin VB.CommandButton CancelButton 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   4320
      TabIndex        =   1
      Top             =   3720
      Width           =   1215
   End
   Begin VB.CommandButton OKButton 
      Caption         =   "OK"
      Height          =   375
      Left            =   1200
      TabIndex        =   0
      Top             =   3720
      Width           =   1215
   End
   Begin VB.Label LbBeSure 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Before selecting a document, be sure that your system has a viewer installed that can display PDF format files."
      Height          =   490
      Left            =   600
      TabIndex        =   3
      Top             =   600
      Width           =   5410
   End
End
Attribute VB_Name = "frmHelp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Dim i As Integer 'general use

Private Sub Form_Load()
'Display the 'Help' form.
    
    LstDocs.Clear
    
  'if n1 > 0 Then
    'List1.Width = 2000: List1.Height = 300 * n1
    'List1.Left = 500: List1.Top = hgts - List1.Height
    'List1.Visible = True
    'For n = 0 To n1 - 1: List1.AddItem "": Next
    
    LstDocs.AddItem "GFM4 Using the GUI to Build, Run, and Review Models: Two Examples"
    LstDocs.AddItem "GFM4 Tips and Special Procedures"
    LstDocs.AddItem "GFM4 Graphical User Interface Menus"
    LstDocs.AddItem "GFM4 File Descriptions"
    LstDocs.AddItem "GFM4 Automated Cycling Guide"
    LstDocs.AddItem "GFM4 RunPlot User Guide"
    LstDocs.AddItem "GFM4 Version 4.0 Final Report"
    
    
  'List1.List(0) = "Geometry"




   ' modVariables.c_return = 1 'initialize to return failure
End Sub

Private Sub CancelButton_Click()
    modVariables.c_return = 1 'return failure (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub

Private Sub LstDocs_Click()
    i = LstDocs.ListIndex
    
    Select Case i
    Case 0
        ShellExecute 0, "open", App.Path & "\documents\GFM4-Using-GUI.pdf", vbNull, vbNull, SW_SHOW
    Case 1
        ShellExecute 0, "open", App.Path & "\documents\GFM4-Tips-Procedures.pdf", vbNull, vbNull, SW_SHOW
    Case 2
        ShellExecute 0, "open", App.Path & "\documents\GFM4-Menus.pdf", vbNull, vbNull, SW_SHOW
    Case 3
        ShellExecute 0, "open", App.Path & "\documents\GFM4-Files.pdf", vbNull, vbNull, SW_SHOW
    Case 4
        ShellExecute 0, "open", App.Path & "\documents\GFM4-Cycle.pdf", vbNull, vbNull, SW_SHOW
    Case 5
        ShellExecute 0, "open", App.Path & "\documents\RunPlot.pdf", vbNull, vbNull, SW_SHOW
    Case 6
        ShellExecute 0, "open", App.Path & "\documents\GFM4-Final-Report.pdf", vbNull, vbNull, SW_SHOW
        'ShellExecute 0, "open", App.Path & "\lib\gfm2.hlp", vbNull, vbNull, SW_SHOW
    
        ''CD1.HelpFile = App.HelpFile
        'CD1.HelpFile = App.Path & "\lib\gfm2.hlp"
        'CD1.HelpCommand = &HB
        'CD1.ShowHelp
    End Select
    
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
    
End Sub
    

Private Sub OKButton_Click()

    'Pass parameters back to main form

    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub





