VERSION 5.00
Begin VB.Form frmCycleInfo 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Provide Cycle Information"
   ClientHeight    =   6220
   ClientLeft      =   2760
   ClientTop       =   3760
   ClientWidth     =   6040
   Icon            =   "frmCycleInfo.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6220
   ScaleWidth      =   6040
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtScale 
      Height          =   288
      Left            =   4920
      TabIndex        =   14
      Text            =   " "
      Top             =   4680
      Width           =   732
   End
   Begin VB.TextBox txtMitr 
      Height          =   288
      Left            =   4920
      TabIndex        =   13
      Text            =   " "
      Top             =   4080
      Width           =   732
   End
   Begin VB.TextBox txtRintv 
      Height          =   288
      Left            =   4920
      TabIndex        =   12
      Text            =   " "
      Top             =   3240
      Width           =   732
   End
   Begin VB.TextBox txtMsitr 
      Height          =   288
      Left            =   4920
      TabIndex        =   11
      Text            =   " "
      Top             =   2520
      Width           =   732
   End
   Begin VB.TextBox txtGitr 
      Height          =   288
      Left            =   4920
      TabIndex        =   10
      Text            =   " "
      Top             =   1800
      Width           =   732
   End
   Begin VB.TextBox txtNumCycle 
      Height          =   288
      Left            =   4920
      TabIndex        =   9
      Top             =   240
      Width           =   732
   End
   Begin VB.CommandButton OKButton 
      Caption         =   "OK"
      Height          =   370
      Left            =   1320
      TabIndex        =   0
      Top             =   5520
      Width           =   1210
   End
   Begin VB.CommandButton CancelButton 
      BackColor       =   &H00FFFFFF&
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3600
      TabIndex        =   2
      Top             =   5520
      Width           =   1215
   End
   Begin VB.Label LbScale 
      BackColor       =   &H00E0E0E0&
      Caption         =   "If melt surface heat flux was initially scaled, then specify the total number of cycles to have scaling done"
      Height          =   492
      Left            =   360
      TabIndex        =   8
      Top             =   4680
      Width           =   4092
   End
   Begin VB.Label LbMitr 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Specify the number of melt iterations per cycle"
      Height          =   252
      Left            =   360
      TabIndex        =   7
      Top             =   4080
      Width           =   4092
   End
   Begin VB.Label Lbparams 
      BackColor       =   &H00E0E0E0&
      Caption         =   "The second and subsequent cycles may have the following parameters changed from their initial values:"
      Height          =   730
      Left            =   1080
      TabIndex        =   6
      Top             =   720
      Width           =   3850
   End
   Begin VB.Label LbMs 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Specify the number of minor species iterations per cycle"
      Height          =   372
      Left            =   360
      TabIndex        =   5
      Top             =   2520
      Width           =   4092
   End
   Begin VB.Label LbGitr 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Specify the number of gas phase iterations per cycle"
      Height          =   372
      Left            =   360
      TabIndex        =   4
      Top             =   1800
      Width           =   3972
   End
   Begin VB.Label LbRadIntv 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Specify the number of gas phase iterations between radiation calculations"
      Height          =   492
      Left            =   360
      TabIndex        =   3
      Top             =   3240
      Width           =   4092
   End
   Begin VB.Label LbNumberCycles 
      BackColor       =   &H00E0E0E0&
      Caption         =   "Specify number of cycles"
      Height          =   252
      Left            =   360
      TabIndex        =   1
      Top             =   240
      Width           =   3132
   End
End
Attribute VB_Name = "frmCycleInfo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Private Sub Form_Load()
'Display the Provide Cycle Information form.
    
    'set default parameters passed from main form
    txtNumCycle.Text = modVariables.c_cycleEnd
    txtGitr.Text = modVariables.c_cycle2_gitr
    txtMsitr.Text = modVariables.c_cycle2_msitr
    txtRintv.Text = modVariables.c_cycle2_rintv
    txtMitr.Text = modVariables.c_cycle2_mitr
    txtScale.Text = modVariables.c_cycle2_scale_on
    
modVariables.c_return = 1 'initialize to return failure
End Sub

Private Sub CancelButton_Click()

    modVariables.c_return = 1 'return failure (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub

Private Sub OKButton_Click()

    'Pass parameters back to main form
    modVariables.c_cycleEnd = txtNumCycle.Text
    modVariables.c_cycle2_gitr = txtGitr.Text
    modVariables.c_cycle2_msitr = txtMsitr.Text
    modVariables.c_cycle2_rintv = txtRintv.Text
    modVariables.c_cycle2_mitr = txtMitr.Text
    modVariables.c_cycle2_scale_on = txtScale.Text
    
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub
