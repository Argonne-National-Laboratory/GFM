VERSION 5.00
Begin VB.Form frmCloseHow 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Determine How to Close"
   ClientHeight    =   4200
   ClientLeft      =   2760
   ClientTop       =   3760
   ClientWidth     =   6040
   Icon            =   "frmCancelHow.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4200
   ScaleWidth      =   6040
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton AbortButton 
      Caption         =   "Abort"
      Height          =   370
      Left            =   4320
      TabIndex        =   4
      Top             =   3240
      Width           =   1210
   End
   Begin VB.CommandButton SaveButton 
      BackColor       =   &H00FFFFFF&
      Cancel          =   -1  'True
      Caption         =   "Save"
      Height          =   375
      Left            =   4320
      TabIndex        =   1
      Top             =   2280
      Width           =   1215
   End
   Begin VB.CommandButton IgnoreButton 
      BackColor       =   &H00FFFFFF&
      Caption         =   "Ignore"
      Height          =   375
      Left            =   4320
      TabIndex        =   0
      Top             =   1320
      Width           =   1215
   End
   Begin VB.Label LbAbort 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Stop the simulation immediately and exit."
      Height          =   610
      Left            =   600
      TabIndex        =   6
      Top             =   3240
      Width           =   3010
   End
   Begin VB.Label LbSave 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Allow the simulation to reach a stopping point and save results before closing down."
      Height          =   610
      Left            =   600
      TabIndex        =   5
      Top             =   2280
      Width           =   2890
   End
   Begin VB.Label LbIgnore 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Ignore the close request. Continue with normal processing."
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.23
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   610
      Left            =   600
      TabIndex        =   3
      Top             =   1320
      Width           =   2770
   End
   Begin VB.Label LbChoose 
      BackColor       =   &H00E0E0E0&
      Caption         =   "A simulation run is in progress.  Your stopping criteria has not been met. Choose one of the following options:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.23
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   610
      Left            =   360
      TabIndex        =   2
      Top             =   360
      Width           =   5290
   End
End
Attribute VB_Name = "frmCloseHow"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Private Sub AbortButton_Click()
    modVariables.c_close = 3
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub

Private Sub Form_Load()
'Display the Determine How to Close form.
    'No changes to the form are needed .
    modVariables.c_return = 1 'initialize to return failure
End Sub

Private Sub IgnoreButton_Click()
    modVariables.c_close = 1
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub

Private Sub SaveButton_Click()
    modVariables.c_close = 2
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub
