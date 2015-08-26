VERSION 5.00
Begin VB.Form frmChooseData 
   BackColor       =   &H00E0E0E0&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Choose Data to Collect for Viewing"
   ClientHeight    =   5580
   ClientLeft      =   2760
   ClientTop       =   3760
   ClientWidth     =   5640
   Icon            =   "frmChooseData.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5580
   ScaleWidth      =   5640
   ShowInTaskbar   =   0   'False
   Begin VB.Frame fraMelt 
      BackColor       =   &H00FFFFFF&
      Caption         =   "Melt Data to Collect"
      Height          =   3372
      Left            =   840
      TabIndex        =   4
      Top             =   960
      Visible         =   0   'False
      Width           =   3850
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Output for FieldView"
         Height          =   250
         Index           =   9
         Left            =   360
         TabIndex        =   27
         Top             =   2520
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Melt Surface Flux Relaxtion"
         Height          =   250
         Index           =   8
         Left            =   360
         TabIndex        =   26
         Top             =   2280
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Melt Surface Adjusted Flux"
         Height          =   250
         Index           =   7
         Left            =   360
         TabIndex        =   25
         Top             =   2040
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Melt Surface Temperature Change"
         Height          =   250
         Index           =   6
         Left            =   360
         TabIndex        =   24
         Top             =   1800
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Pre-Solve Equation Residuals"
         Height          =   250
         Index           =   5
         Left            =   360
         TabIndex        =   23
         Top             =   1560
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Equation Residuals"
         Height          =   250
         Index           =   4
         Left            =   360
         TabIndex        =   22
         Top             =   1320
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Mass Residual Convergence"
         Height          =   250
         Index           =   3
         Left            =   360
         TabIndex        =   8
         Top             =   1080
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Mean Temperature"
         Height          =   250
         Index           =   2
         Left            =   360
         TabIndex        =   7
         Top             =   840
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Summary"
         Height          =   250
         Index           =   0
         Left            =   360
         TabIndex        =   6
         Top             =   360
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkMeltFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "General Information"
         Height          =   250
         Index           =   1
         Left            =   360
         TabIndex        =   5
         Top             =   600
         Value           =   1  'Checked
         Width           =   3400
      End
   End
   Begin VB.Frame fraComb 
      BackColor       =   &H00FFFFFF&
      Caption         =   "Combustion Data to Collect"
      Height          =   4090
      Left            =   840
      TabIndex        =   2
      Top             =   360
      Width           =   3850
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Output for FieldView"
         Height          =   250
         Index           =   14
         Left            =   360
         TabIndex        =   28
         Top             =   3720
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Melt Surface Flux Change"
         Height          =   250
         Index           =   13
         Left            =   360
         TabIndex        =   21
         Top             =   3480
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Melt Surface Temperature Relaxation"
         Height          =   250
         Index           =   12
         Left            =   360
         TabIndex        =   20
         Top             =   3240
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Wall Temperature"
         Height          =   250
         Index           =   11
         Left            =   360
         TabIndex        =   19
         Top             =   3000
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Radiosity Convergence"
         Height          =   250
         Index           =   10
         Left            =   360
         TabIndex        =   18
         Top             =   2760
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Radiation Details"
         Height          =   250
         Index           =   9
         Left            =   360
         TabIndex        =   17
         Top             =   2520
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Minor Species Residual Convergence"
         Height          =   250
         Index           =   8
         Left            =   360
         TabIndex        =   16
         Top             =   2280
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Extra Pre-Solve Equation Residuals"
         Height          =   250
         Index           =   7
         Left            =   360
         TabIndex        =   15
         Top             =   2040
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Extra Equation Residuals"
         Height          =   250
         Index           =   6
         Left            =   360
         TabIndex        =   14
         Top             =   1800
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Pre-Solve Equation Residuals"
         Height          =   250
         Index           =   5
         Left            =   360
         TabIndex        =   13
         Top             =   1560
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Equation Residuals"
         Height          =   250
         Index           =   4
         Left            =   360
         TabIndex        =   12
         Top             =   1320
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Mass Residual Convergence"
         Height          =   250
         Index           =   3
         Left            =   360
         TabIndex        =   11
         Top             =   1080
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Mean Temperature"
         Height          =   250
         Index           =   2
         Left            =   360
         TabIndex        =   10
         Top             =   840
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "General Information"
         Height          =   250
         Index           =   1
         Left            =   360
         TabIndex        =   9
         Top             =   600
         Value           =   1  'Checked
         Width           =   3400
      End
      Begin VB.CheckBox chkCombFlags 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Summary"
         Height          =   250
         Index           =   0
         Left            =   360
         TabIndex        =   3
         Top             =   360
         Value           =   1  'Checked
         Width           =   3400
      End
   End
   Begin VB.CommandButton CancelButton 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3120
      TabIndex        =   1
      Top             =   4800
      Width           =   1215
   End
   Begin VB.CommandButton OKButton 
      Caption         =   "OK"
      Height          =   375
      Left            =   1200
      TabIndex        =   0
      Top             =   4800
      Width           =   1215
   End
End
Attribute VB_Name = "frmChooseData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
'Const Checked = 1
'Const unchecked = 0

Private Sub Form_Load()
'Display the 'Choose Data' form.
    
    'set parameters passed from main form
    If modVariables.c_domain = 1 Then
        'doing combustion
        fraComb.Top = 360
        fraComb.Visible = True
        fraMelt.Visible = False
        
        chkCombFlags(0).Value = modVariables.c_isum
        chkCombFlags(1).Value = modVariables.c_iinfo
        chkCombFlags(2).Value = modVariables.c_iTave
        chkCombFlags(3).Value = modVariables.c_iconv
        chkCombFlags(4).Value = modVariables.c_igresid
        chkCombFlags(5).Value = modVariables.c_igresidp
        chkCombFlags(6).Value = modVariables.c_igresidx
        chkCombFlags(7).Value = modVariables.c_igresidxp
        chkCombFlags(8).Value = modVariables.c_imresid
        chkCombFlags(9).Value = modVariables.c_irad_detail
        chkCombFlags(10).Value = modVariables.c_irad_rad
        chkCombFlags(11).Value = modVariables.c_itwal
        chkCombFlags(12).Value = modVariables.c_irelax
        chkCombFlags(13).Value = modVariables.c_iflx
        chkCombFlags(14).Value = modVariables.c_ifieldview
    Else
        'doing melt
        fraMelt.Top = 360
        fraComb.Visible = False
        fraMelt.Visible = True
        
        chkMeltFlags(0).Value = modVariables.c_isum_m
        chkMeltFlags(1).Value = modVariables.c_iinfo_m
        chkMeltFlags(2).Value = modVariables.c_iTave_m
        chkMeltFlags(3).Value = modVariables.c_iconv_m
        chkMeltFlags(4).Value = modVariables.c_igresid_m
        chkMeltFlags(5).Value = modVariables.c_igresidp_m
        chkMeltFlags(6).Value = modVariables.c_iTchg
        chkMeltFlags(7).Value = modVariables.c_iadjf
        chkMeltFlags(8).Value = modVariables.c_iadjr
        chkMeltFlags(9).Value = modVariables.c_ifieldview_m
    End If
      
    modVariables.c_return = 1 'initialize to return failure
End Sub

Private Sub CancelButton_Click()
    modVariables.c_return = 1 'return failure (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub

Private Sub OKButton_Click()

    'Pass parameters back to main form
    If modVariables.c_domain = 1 Then
        'doing combustion
        modVariables.c_isum = chkCombFlags(0).Value
        modVariables.c_iinfo = chkCombFlags(1).Value
        modVariables.c_iTave = chkCombFlags(2).Value
        modVariables.c_iconv = chkCombFlags(3).Value
        modVariables.c_igresid = chkCombFlags(4).Value
        modVariables.c_igresidp = chkCombFlags(5).Value
        modVariables.c_igresidx = chkCombFlags(6).Value
        modVariables.c_igresidxp = chkCombFlags(7).Value
        modVariables.c_imresid = chkCombFlags(8).Value
        modVariables.c_irad_detail = chkCombFlags(9).Value
        modVariables.c_irad_rad = chkCombFlags(10).Value
        modVariables.c_itwal = chkCombFlags(11).Value
        modVariables.c_irelax = chkCombFlags(12).Value
        modVariables.c_iflx = chkCombFlags(13).Value
        modVariables.c_ifieldview = chkCombFlags(14).Value
    Else
        'doing melt
        modVariables.c_isum_m = chkMeltFlags(0).Value
        modVariables.c_iinfo_m = chkMeltFlags(1).Value
        modVariables.c_iTave_m = chkMeltFlags(2).Value
        modVariables.c_iconv_m = chkMeltFlags(3).Value
        modVariables.c_igresid_m = chkMeltFlags(4).Value
        modVariables.c_igresidp_m = chkMeltFlags(5).Value
        modVariables.c_iTchg = chkMeltFlags(6).Value
        modVariables.c_iadjf = chkMeltFlags(7).Value
        modVariables.c_iadjr = chkMeltFlags(8).Value
        modVariables.c_ifieldview_m = chkMeltFlags(9).Value
                
    End If
    modVariables.c_return = 0 'return success (0=> success, 1=> failed)
    Unload Me 'done with this form
End Sub



'Private Sub chkMeltFlags_Click(Index As Integer)
'
'    If chkMeltFlags(Index).Value = 0 Then
'        chkMeltFlags(Index).Value = 1
'    Else
'        chkMeltFlags(Index).Value = 0
'    End If
'    Exit Sub
'End Sub


'Private Sub chkCombFlags_Click(Index As Integer)
'
'    If chkCombFlags(Index).Value = 0 Then
'        chkCombFlags(Index).Value = 1
'    Else
'        chkCombFlags(Index).Value = 0
'    End If
'    Exit Sub
'End Sub

