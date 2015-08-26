VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmMain 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00FFFFFF&
   Caption         =   "Glass Furnace Model Simulator"
   ClientHeight    =   8660
   ClientLeft      =   110
   ClientTop       =   770
   ClientWidth     =   7310
   BeginProperty Font 
      Name            =   "Times New Roman"
      Size            =   11.31
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "gfm_2e.frx":0000
   LinkTopic       =   "frmMain"
   ScaleHeight     =   8660
   ScaleWidth      =   7310
   StartUpPosition =   3  'Windows Default
   Begin VB.ListBox ListCycle 
      BackColor       =   &H00FFFFC0&
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   11.83
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   300
      Left            =   360
      TabIndex        =   34
      Top             =   3600
      Visible         =   0   'False
      Width           =   1095
   End
   Begin VB.ListBox List4 
      BackColor       =   &H00FFFFFF&
      Height          =   290
      ItemData        =   "gfm_2e.frx":1CFA
      Left            =   4800
      List            =   "gfm_2e.frx":1D01
      TabIndex        =   31
      Top             =   819
      Width           =   855
   End
   Begin VB.ListBox List3 
      Height          =   290
      Left            =   3720
      TabIndex        =   30
      Top             =   840
      Width           =   975
   End
   Begin VB.TextBox tx_zc 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   6240
      TabIndex        =   24
      Text            =   "tx_zc"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_yc 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   5520
      TabIndex        =   23
      Text            =   "tx_yc"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_xc 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   4680
      TabIndex        =   22
      Text            =   "tx_xc"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_ze 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   3960
      TabIndex        =   21
      Text            =   "tx_ze"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_ye 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   3240
      TabIndex        =   20
      Text            =   "tx_ye"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_xe 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   2400
      TabIndex        =   19
      Text            =   "tx_xe"
      Top             =   2400
      Width           =   735
   End
   Begin VB.TextBox tx_zb 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   1680
      TabIndex        =   18
      Text            =   "tx_zb"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_yb 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   960
      TabIndex        =   17
      Text            =   "tx_yb"
      Top             =   2400
      Width           =   615
   End
   Begin VB.TextBox tx_xb 
      Alignment       =   1  'Right Justify
      Height          =   375
      Left            =   120
      TabIndex        =   16
      Text            =   "tx_xb"
      Top             =   2400
      Width           =   615
   End
   Begin VB.OptionButton Optz 
      Caption         =   "z"
      Height          =   250
      Left            =   1800
      TabIndex        =   8
      Top             =   240
      Width           =   495
   End
   Begin VB.OptionButton Opty 
      Caption         =   "y"
      Height          =   250
      Left            =   1320
      TabIndex        =   7
      Top             =   240
      Width           =   495
   End
   Begin VB.OptionButton Optx 
      Caption         =   "x"
      Height          =   250
      Left            =   840
      TabIndex        =   6
      Top             =   240
      Value           =   -1  'True
      Width           =   495
   End
   Begin VB.ListBox List2 
      Height          =   290
      Left            =   2520
      TabIndex        =   5
      Top             =   840
      Width           =   975
   End
   Begin VB.ListBox List1 
      BackColor       =   &H00FFFFFF&
      Height          =   290
      Left            =   1560
      TabIndex        =   4
      Top             =   840
      Width           =   855
   End
   Begin VB.Timer Timer1 
      Left            =   840
      Top             =   840
   End
   Begin MSComDlg.CommonDialog CD1 
      Left            =   120
      Top             =   120
      _ExtentX        =   853
      _ExtentY        =   853
      _Version        =   393216
   End
   Begin VB.Label LbSoot 
      Caption         =   "Soot kinetics calibration is active!"
      Height          =   255
      Left            =   3120
      TabIndex        =   35
      Top             =   3000
      Width           =   3252
   End
   Begin VB.Label LbCase 
      BackColor       =   &H80000005&
      Caption         =   " Case 0000"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.26
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   0
      TabIndex        =   33
      Top             =   3000
      Width           =   1050
   End
   Begin VB.Label LbCaseTitle 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Case title"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   9.26
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   225
      Left            =   1050
      TabIndex        =   32
      Top             =   3000
      Width           =   780
   End
   Begin VB.Label LbP 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00008000&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "LbP"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3480
      MousePointer    =   6  'Size NE SW
      TabIndex        =   29
      Top             =   2040
      Width           =   495
   End
   Begin VB.Label LbO 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00008000&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "LbO"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2880
      MousePointer    =   15  'Size All
      TabIndex        =   28
      Top             =   2040
      Width           =   495
   End
   Begin VB.Label Lb14 
      Caption         =   "Lb14"
      Height          =   255
      Left            =   2280
      TabIndex        =   27
      Top             =   2040
      Width           =   495
   End
   Begin VB.Label Lbsw 
      Alignment       =   2  'Center
      Caption         =   "sweep"
      Height          =   255
      Left            =   1560
      TabIndex        =   26
      Top             =   2040
      Width           =   615
   End
   Begin VB.Label Lb12 
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Lb12"
      Height          =   255
      Left            =   840
      TabIndex        =   25
      Top             =   2040
      Width           =   615
   End
   Begin VB.Label Lbnd 
      Alignment       =   2  'Center
      Caption         =   "node"
      Height          =   255
      Left            =   120
      TabIndex        =   15
      Top             =   2040
      Width           =   615
   End
   Begin VB.Label Lbto 
      Alignment       =   2  'Center
      Caption         =   "to"
      Height          =   255
      Left            =   5760
      TabIndex        =   14
      Top             =   1560
      Width           =   735
   End
   Begin VB.Label Lbfr 
      Alignment       =   2  'Center
      Caption         =   "from"
      Height          =   255
      Left            =   5040
      TabIndex        =   13
      Top             =   1560
      Width           =   615
   End
   Begin VB.Label Lb3d 
      Alignment       =   2  'Center
      Caption         =   "3D"
      Height          =   255
      Left            =   4320
      TabIndex        =   12
      Top             =   1560
      Width           =   615
   End
   Begin VB.Label Lbmv 
      Alignment       =   2  'Center
      Caption         =   "move"
      Height          =   255
      Left            =   3720
      TabIndex        =   11
      Top             =   1560
      Width           =   495
   End
   Begin VB.Label Lbdel 
      Alignment       =   2  'Center
      Caption         =   "del"
      Height          =   255
      Left            =   3120
      TabIndex        =   10
      Top             =   1560
      Width           =   495
   End
   Begin VB.Label Lbadd 
      Alignment       =   2  'Center
      Caption         =   "add"
      Height          =   255
      Left            =   2520
      TabIndex        =   9
      Top             =   1560
      Width           =   495
   End
   Begin VB.Label Lb4 
      Caption         =   "Lb4"
      Height          =   255
      Left            =   1920
      TabIndex        =   3
      Top             =   1560
      Width           =   495
   End
   Begin VB.Image Image1 
      Height          =   495
      Left            =   120
      Top             =   840
      Width           =   495
   End
   Begin VB.Label Lb3 
      Caption         =   "Lb3"
      Height          =   255
      Left            =   1320
      TabIndex        =   2
      Top             =   1560
      Width           =   495
   End
   Begin VB.Label Lb2 
      Caption         =   "Lb2"
      Height          =   255
      Left            =   720
      TabIndex        =   1
      Top             =   1560
      Width           =   495
   End
   Begin VB.Label Lb1 
      Caption         =   "Lb1"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   1560
      UseMnemonic     =   0   'False
      Width           =   495
   End
   Begin VB.Menu pre0 
      Caption         =   "Pre-Processor"
      Visible         =   0   'False
      Begin VB.Menu pre1 
         Caption         =   "Combustion Space"
      End
      Begin VB.Menu pre2 
         Caption         =   "Glass Melter"
      End
   End
   Begin VB.Menu mnuCase 
      Caption         =   "File"
      Begin VB.Menu mnuCaseNew 
         Caption         =   "New Case"
         Begin VB.Menu file12 
            Caption         =   "Box"
         End
         Begin VB.Menu file11 
            Caption         =   "Crown"
         End
      End
      Begin VB.Menu mnuCaseOpen 
         Caption         =   "Open Case"
      End
      Begin VB.Menu mnuCaseSave 
         Caption         =   "Save Case"
      End
      Begin VB.Menu mnuCaseSaveAs 
         Caption         =   "Save Case As (Setup Only)"
      End
      Begin VB.Menu mnuCaseSaveAsFull 
         Caption         =   "Save Case As (Full case)"
      End
      Begin VB.Menu mnuCaseDescript 
         Caption         =   "Update Case Description"
      End
      Begin VB.Menu mnuCaseDelete 
         Caption         =   "Delete Case"
      End
      Begin VB.Menu mnuCaseDelResults 
         Caption         =   "Delete Case Results"
      End
   End
   Begin VB.Menu mnuView 
      Caption         =   "View"
      Begin VB.Menu mnuFullGrid 
         Caption         =   "Grid"
      End
      Begin VB.Menu mnuFigure 
         Caption         =   "Figure"
      End
   End
   Begin VB.Menu ct0 
      Caption         =   "Construct"
      Begin VB.Menu ctgeo 
         Caption         =   "Geometry"
      End
      Begin VB.Menu ctbr 
         Caption         =   "Burner"
      End
      Begin VB.Menu ctex 
         Caption         =   "Exhaust"
      End
      Begin VB.Menu ctcpn 
         Caption         =   "Components"
         Begin VB.Menu ctsw 
            Caption         =   "Dog House"
         End
         Begin VB.Menu cteb 
            Caption         =   "Electric Booster"
         End
      End
      Begin VB.Menu ctgd 
         Caption         =   "Grid"
      End
   End
   Begin VB.Menu mnuProperties 
      Caption         =   "Properties"
      Visible         =   0   'False
      Begin VB.Menu mnuProp_GlassExitTemp 
         Caption         =   "Glass Exit Temperature"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuPropGlass 
         Caption         =   "Glass Properties"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuPropEmis 
         Caption         =   "Emissivities"
         Visible         =   0   'False
         Begin VB.Menu mnuPropEmisWall 
            Caption         =   "Side Wall Emissivity"
         End
         Begin VB.Menu mnuPropEmisCrown 
            Caption         =   "Ceiling or Crown Emissivity"
         End
         Begin VB.Menu mnuPropEmisMelt 
            Caption         =   "Melt Surface Emissivity"
         End
      End
      Begin VB.Menu mnuPropWall 
         Caption         =   "Wall Properties"
      End
      Begin VB.Menu ctsm 
         Caption         =   "Simulation Parameters"
      End
      Begin VB.Menu mnuPropSoot 
         Caption         =   "Soot Kinetics"
         Visible         =   0   'False
         Begin VB.Menu mnuPropSootAform 
            Caption         =   "Soot Formation Kinetic Constant"
         End
         Begin VB.Menu mnuPropSootEsf 
            Caption         =   "Soot Formation Activation Energy"
         End
         Begin VB.Menu mnuPropSootAoxid 
            Caption         =   "Soot Oxidation Kinetic Constant"
         End
         Begin VB.Menu mnuPropSootEso 
            Caption         =   "Soot Oxidation Activation Energy"
         End
         Begin VB.Menu mnuPropSootCali 
            Caption         =   "Calibrate Soot Kinetics"
            Checked         =   -1  'True
         End
      End
      Begin VB.Menu mnuPropCFD 
         Caption         =   "Radiation Controls"
         Visible         =   0   'False
         Begin VB.Menu mnuPropCFDvf 
            Caption         =   "Keep View Factors in Memory"
            Checked         =   -1  'True
         End
         Begin VB.Menu mnuPropCFDmaxOuter 
            Caption         =   "Radiosity Solver Outer Loop Limit"
         End
         Begin VB.Menu mnuPropCFDmaxInner 
            Caption         =   "Radiosity Solver Inner Loop Limit"
         End
         Begin VB.Menu mnuPropCFDminInner 
            Caption         =   "Radiosity Solver Inner Loop Minimum"
         End
      End
   End
   Begin VB.Menu opt0 
      Caption         =   "Options"
      Begin VB.Menu MnuOptGui 
         Caption         =   "Simulation Display Updates"
         Begin VB.Menu MnuOptGuiOn 
            Caption         =   "Simulation Progress Display On"
            Checked         =   -1  'True
         End
         Begin VB.Menu MnuOptGuiOff 
            Caption         =   "Simulation Progress Display Off"
            Checked         =   -1  'True
         End
      End
      Begin VB.Menu MnuOptCol 
         Caption         =   "Collect Run Data"
      End
      Begin VB.Menu mnuOptRunPlot 
         Caption         =   "Activate RunPlot"
      End
      Begin VB.Menu opt1 
         Caption         =   "Units"
         Begin VB.Menu opt11 
            Caption         =   "British"
         End
         Begin VB.Menu opt12 
            Caption         =   "SI"
         End
      End
      Begin VB.Menu MnuOptFed 
         Caption         =   "Feed Units"
         Begin VB.Menu MnuOptFedVol 
            Caption         =   "Volume Based (m^3/s, J/m^3)"
         End
         Begin VB.Menu MnuOptFedMas 
            Caption         =   "Mass Based (kg/s, J/kg)"
         End
      End
      Begin VB.Menu opt2 
         Caption         =   "Zoom"
         Begin VB.Menu opt21 
            Caption         =   "Length"
         End
         Begin VB.Menu opt22 
            Caption         =   "Width"
         End
         Begin VB.Menu opt23 
            Caption         =   "Height"
         End
      End
      Begin VB.Menu opt3 
         Caption         =   "Diagram Position"
         Begin VB.Menu opt31 
            Caption         =   "Horizontal"
         End
         Begin VB.Menu opt32 
            Caption         =   "Vertical"
         End
      End
      Begin VB.Menu opt4 
         Caption         =   "Grid Density"
         Begin VB.Menu opt41 
            Caption         =   "Coarser"
         End
         Begin VB.Menu opt42 
            Caption         =   "Denser"
         End
         Begin VB.Menu opt43 
            Caption         =   "Variable"
            Begin VB.Menu opt431 
               Caption         =   "Length"
            End
            Begin VB.Menu opt432 
               Caption         =   "Width"
            End
            Begin VB.Menu opt433 
               Caption         =   "Height"
            End
         End
      End
      Begin VB.Menu mnuProtectGrid 
         Caption         =   "Protect Grid Edits"
         Checked         =   -1  'True
      End
      Begin VB.Menu optplt 
         Caption         =   "Plot"
         Begin VB.Menu optpltbd 
            Caption         =   "Bounds"
            Begin VB.Menu optpltbd1 
               Caption         =   "Upper"
            End
            Begin VB.Menu optpltbd2 
               Caption         =   "Lower"
            End
         End
         Begin VB.Menu optpltadd 
            Caption         =   "Add"
            Begin VB.Menu optpltaddgd 
               Caption         =   "Grid"
            End
            Begin VB.Menu optpltaddvv 
               Caption         =   "Velocity Vectors"
            End
         End
         Begin VB.Menu optpltclr 
            Caption         =   "Display"
            Begin VB.Menu optpltclr1 
               Caption         =   "Color"
            End
            Begin VB.Menu optpltclrbw 
               Caption         =   "Black/White"
            End
         End
         Begin VB.Menu optvl 
            Caption         =   "Vector Length"
         End
      End
   End
   Begin VB.Menu sim0 
      Caption         =   "Simulation"
      Begin VB.Menu sim1 
         Caption         =   "Combustion Space"
      End
      Begin VB.Menu sim2 
         Caption         =   "Glass Melter"
      End
      Begin VB.Menu mnuCycleDomains 
         Caption         =   "Cycle Domains (Comb First)"
      End
      Begin VB.Menu mnuCycleDomainsMelt 
         Caption         =   "Cycle Domains (Melt First)"
      End
      Begin VB.Menu mnuCycleregen 
         Caption         =   "Cycle Regenerative (Comb First)"
      End
      Begin VB.Menu mnuCycleRegenMelt 
         Caption         =   "Cycle Regenerative (Melt First)"
      End
   End
   Begin VB.Menu post0 
      Caption         =   "Post-Processor"
      Begin VB.Menu postcb 
         Caption         =   "Combustion Space"
      End
      Begin VB.Menu post2 
         Caption         =   "Glass Melter"
      End
   End
   Begin VB.Menu mnuScreenPrint 
      Caption         =   "Screen Print"
   End
   Begin VB.Menu ex0 
      Caption         =   "E&xit"
   End
   Begin VB.Menu help 
      Caption         =   "&Help"
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'*******************************************************************************************
'*******************************************************************************************
'
'
'                        Copyright © 2015, UChicago Argonne, LLC
'
'                                 All Rights Reserved
'
'                        Glass Furnace Model(GFM)v4(ANL-SF-01-030)
'
'                      Steven A. Lottes, Argonne National Laboratory
'
'
'                                 OPEN SOURCE LICENSE
'
'
'   Under the terms of Contract No. DE-AC02-06CH11357 with UChicago Argonne, LLC,
'   the U.S. Government retains certain rights in this software.
'
'
'   Redistribution and use in source and binary forms, with or without modification,
'   are permitted provided that the following conditions are met:
'
'
'   1.  Redistribution of source code must retain the above copyright notice, this
'       list of conditions and the following disclaimer.
'   2.  Redistribution in binary form must reproduce the above copyright notice, this
'       list of conditions and the following disclaimer in the documentation and/or
'       other materials provided with the distribution.
'   3.  Neither the names of UChicago Argonne, LLC or the Department of Energy nor the
'       names of its contributors may be used to endorse or promote products derived
'       from this software without specific prior written permission.
'
'
'
'*******************************************************************************************
'*******************************************************************************************
'
'
'                                     DISCLAIMER
'
'
'            THE SOFTWARE IS SUPPLIED "AS IS" WITHOUT WARRANTY OF ANY KIND.
'
'
'   NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT OF ENERGY,
'   NOR UCHICAGO ARGONNE, LLC, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS
'   OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
'   COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, DATA, APPARATUS, PRODUCT, OR
'   PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
'
'
'
'*******************************************************************************************
'*******************************************************************************************
'
'
                'GFM Variables
'Startup
'-------
#Const DebugVersion = 0 'Set to zero to compile a release version,  7-26-2004
                        'Set to one for development and debugging
Dim exe_dir As String 'holds "debug" or "release" directory,  7-25-04
Dim cfd_task_id As Long
Dim wrd As String
Const gfm_version = "Version 4.00.01"
Dim dnm As String 'application path current directory


'Status Flags and Indicators
'---------------------------
Dim menu_layer As Integer 'indicates current active part of GUI
    Const InMain = 1
    Const InPreProc = 2
    Const InGridConstruction = 3
    Const InSimulation = 4
    Const InPostProc = 5
Dim flow_domain As Integer
    '1 => combustion space
    '2 => melter space
Dim chamber_type As Integer
    '0 => combustion with crown
    '1 => combustion box or plain melter
    '2 => melter with refiner
Dim melter_component As Integer 'melter component
    '1 => melter
    '2 => throat
    '3 => refiner
Dim grid_type As Integer
    Const Not_enhanced = 0
    Const Is_enhanced = 1
Dim active_view As Integer
    Const vw_grid As Integer = 1
    Const vw_fig As Integer = 2

Dim gridProtectMode As Boolean 'true => enhanced grid edit protection is active
Dim userError As Boolean 'user error flag, not always used ' 03-04-2004
Dim restart_str As String '"1" -> restart, "0" -> new start
Dim entity_str As String

Dim sweep As Boolean
Dim cancelMode As Boolean 'true if waiting for CFD shutdown after user canceled

Dim flag_save_as As Boolean
    'true if doing a file->case_save_as menu item
Dim full_save_as As Boolean
    'true if doing a full case_save_as (all files, as opposed to just the setup files)
    
Dim flag_preproc_modified As Boolean
    'true if pre-process info has been modified (not just default), but not saved
Dim flag_preproc_open As Boolean
    'true if pre-process file has been opened, even if saved
Dim flag_preproc_saved As Boolean
    'true if pre-process file has been saved and not modified again

Dim flag_grid_modified As Boolean
    'true if non-enhanced grid info has been modified or created, but not reconstucted
Dim flag_grid_constructed As Boolean
    'true if grid file has been constructed and not modified again, but has not been saved
Dim flag_grid_enhanced As Boolean
    'true if grid has been enhanced and should not be reconstructed, but has not been saved
Dim flag_grid_active As Boolean
    'true if grid file has been constructed or opened, even if saved
Dim flag_grid_saved As Boolean
    'true if grid file has been saved and not modified or enhanced again
    
Dim flag_conditions_modified As Boolean
    'true if conditions info has been modified or created, but not saved
Dim flag_conditions_modsav As Boolean
    'true if conditions info was modified before a "save case as (full case)"
Dim flag_conditions_saved As Boolean
    'true if conditions file has been saved and not modified again

Dim cycleDomains As Boolean 'true => doing automatic cycling between simulation domains with combustion first
Dim cycleDomainsMelt As Boolean 'true => doing automatic cycling between simulation domains with melt first
Dim cycleCombCnt As Integer 'count of times combustion simulation has started during auto cycling
Dim cycleMeltCnt As Integer 'count of times melt simulation has started during auto cycling
Dim userStoppedSim As Boolean 'true => User canceled or requested simulation to stop
Dim cycleEnd As Integer 'Number of times user wants to cycle thru combustion/melt simulation
    Const default_cycleEnd = 16
Dim cycleRegen As Boolean 'true => doing automatic cycling for a regenerative furnace
Dim cycleComb2Cnt As Integer 'count of times second combustion simulation in cycle has started
Dim melt_case_number As String 'case number for melt
Dim comb_case_number As String 'first case number for regenerative furnace
Dim comb2_case_number As String 'second case number for regenerative furnace
Dim comb2_case_title As String 'second case description for regenerative furnace
Dim cycleInfo As Integer 'cycle count number to put into cycleInfo file
Dim combpath As String
Dim comb2path As String
Dim meltpath As String
Dim cycle2_gitr As Integer 'second and subsequent cycle maximum gas iterations
    Const default_cycle2_gitr = 100
Dim cycle2_msitr As Integer 'second and subsequent cycle minor species iterations
    Const default_cycle2_msitr = 300
Dim cycle2_rintv As Integer 'second and subsequent cycle radiation interval
    Const default_cycle2_rintv = 100
Dim cycle2_mitr As Integer 'second and subsequent cycle maximum melt iterations
    Const default_cycle2_mitr = 300
Dim cycle2_scale_on As Integer 'number of cycles to have scaling on
    Const default_cycle2_scale_on = 8

'Case and Files
'--------------
Dim case_number As String '4 digit case number string used in filenames
Dim case_path As String 'path for case folder
Dim case_title As String
Dim case_action As String 'open, delete, or simulate
Dim melt_case_title As String
Dim comb_case_title As String

Dim fnm As String 'general purpose file name or name with path
Dim fnm_pre As String 'pre file name
Dim fnm_pre_path As String 'pre file name with path
Dim fnm_gd As String 'grid file name
Dim fnm_gd_path As String 'grid file name with path
Dim fnm_sbc As String 'conditions file name
Dim fnm_sbc_path As String 'conditions file name with path
Dim fnm_rt As String 'output file name
Dim fnm_rt_path As String  'output file name with path
Dim fnm_rg As String 'restart file name
Dim fnm_rg_path As String 'restart file name with path
Dim fnm_txt As String 'case text file name
Dim fnm_txt_path As String 'case text file name with path
Dim fnm_rr As String 'radiation restart file
Dim fnm_rr_path As String
Dim fnm_rs As String 'sub species restart file
Dim fnm_rs_path As String
Dim fnm_bt As String
Dim fnm_bt_path As String
Dim fnm_it As String 'Surface radiation heat flux into melter file
Dim fnm_it_path As String
Dim fnm_itT As String 'Surface temperature of melt file
Dim fnm_itT_path As String
Dim fnm_runs_path As String 'runs file name with path

'The following file names are for holding information while doing automatic domain cycling
Dim melt_fnm_pre As String 'pre file name
Dim melt_fnm_pre_path As String 'pre file name with path
Dim melt_fnm_gd As String 'grid file name
Dim melt_fnm_gd_path As String 'grid file name with path
Dim melt_fnm_sbc As String 'conditions file name
Dim melt_fnm_sbc_path As String 'conditions file name with path
Dim melt_fnm_rg As String 'restart file name
Dim melt_fnm_rg_path As String 'restart file name with path
Dim melt_fnm_txt As String 'case text file name
Dim melt_fnm_txt_path As String 'case text file name with path
Dim melt_fnm_runs_path As String 'runs file name with path

Dim comb_fnm_pre As String 'pre file name
Dim comb_fnm_pre_path As String 'pre file name with path
Dim comb_fnm_gd As String 'grid file name
Dim comb_fnm_gd_path As String 'grid file name with path
Dim comb_fnm_sbc As String 'conditions file name
Dim comb_fnm_sbc_path As String 'conditions file name with path
Dim comb_fnm_rg As String 'restart file name
Dim comb_fnm_rg_path As String 'restart file name with path
Dim comb_fnm_txt As String 'case text file name
Dim comb_fnm_txt_path As String 'case text file name with path
Dim comb_fnm_runs_path As String 'runs file name with path

Dim comb2_fnm_pre As String 'pre file name
Dim comb2_fnm_pre_path As String 'pre file name with path
Dim comb2_fnm_gd As String 'grid file name
Dim comb2_fnm_gd_path As String 'grid file name with path
Dim comb2_fnm_sbc As String 'conditions file name
Dim comb2_fnm_sbc_path As String 'conditions file name with path
Dim comb2_fnm_rg As String 'restart file name
Dim comb2_fnm_rg_path As String 'restart file name with path
Dim comb2_fnm_txt As String 'case text file name
Dim comb2_fnm_txt_path As String 'case text file name with path
Dim comb2_fnm_runs_path As String 'runs file name with path

Dim filename_array() As String 'dynamic array of filenames in a directory
Dim filename_array_bound As Integer 'upper bound of the array

'Pre File for combustion
'-----------------------
Dim p0 As Long, q0 As Long 'plot origin
Dim zf1 As Single, zf2 As Single, zf3 As Single 'plot scale factors
Dim theta As Single 'plot angle stored as theta/pi*180
Dim unt As String, vv0 As Single 'unit type and velocity vector length
Dim lth As Single, wdt As Single, hgt As Single, hgt_c As Single
    'furnace length, width, height, crown height ^
    
'Wall information
Dim thk(5) As Single 'wall thickness
Dim iwal As Integer, nwal As Integer 'current wall number, number of walls
Dim wa_dr() As Integer 'preceeded by wall index, wall direction
Dim wa_sg() As Single, wa_bg() As Single, wa_wd() As Single, wa_ht() As Single
    'wall geometry ^
Dim wa_d() As Single, wa_k() As Single, wa_h() As Single, wa_ta() As Single, wa_e() As Single
    'wall properties ^ thickness, conductivity, external heat trans coeff, amb temp, emissivity
    
'Burner Information
Dim nbr As Integer, ibr As Integer 'number of burners, current burner number
'burner index (in the pre file repeat this section for each index value)
Dim bty() As Integer, bdr() As Integer 'type, orientation
Dim bsg() As Single, bbg() As Single 'gaps to edges
Dim bwd() As Single, bht() As Single 'width, height
' 10-21-2004 note that bht is used for rings as the inner diameter.
'For other burner types bht is the height
Dim btg() As Single, bas() As Single 'temp, spray angle
Dim bav() As Single, bah() As Single 'ver/hor injection angle
Dim bfg() As Single, bfa() As Single 'gas/air flow rates
Dim bfo() As Single, bfn() As Single 'nitrogen/oxygen flow rates
'Note there is a second set of the above two lines in the pre file for dimension(:,2)
'Note for burner type 4, for the inner part there is a second
'set of btg, bav, bah, bsg, bbg, bwd, bht

'Exhaust Information
Dim nex As Integer, iex As Integer 'number of exhausts, current exhaust number
'exhaust index (in the pre file repeat this section for each index value)
Dim ety() As Integer, edr() As Integer 'type, orientation
Dim esg() As Single, ebg() As Single 'gaps to edges
Dim ewd() As Single, eht() As Single 'width, height
Dim exh_wall_temp_type() As Integer 'type of initial exit wall temperature for radiation
    Const default_exh_temp_type = 0 'indicates init temp is fraction of average wall temp
                                '(1=>user specified temp)
Dim exh_wall_temp_frac() As Single 'indicates fraction of near wall temp
    Const default_exh_temp_frac = 0.9
Dim exh_wall_temp_fixed() As Single 'indicates specified temp
    Const default_exh_temp_fixed = 1475#
    
'Parameter Information
Dim irstyp As Integer, maxgi As Integer 'restart indicator, maximum number of iterations
Dim bgcon As Single, id_rad As Integer 'convergence criteria, id for radiation
Dim pg0 As Single, qh0 As Single 'pres, heat
    Const default_qh0 = 32600000#

'Side well (Dog House) Information
Dim nsw As Integer, isw As Integer 'number of side wells, current number
'side well index (in the pre file repeat this section for each index value)
Dim wdr() As Integer 'orientation
Dim wsg() As Single, wbg() As Single 'gaps (wbg only used for melter)
Dim wtg() As Single, wfa() As Single 'temperature, flow rate
Dim wwd() As Single, wdp() As Single 'diameter, height (width-->diameter, depth-->height)

'Grid Size and additions to file
Dim dx0 As Single, dy0 As Single, dz0 As Single 'maximum sizes for grid cells (delta values)
Dim mp As Integer, np As Integer, lp As Integer 'maximum grid cell indexes in x,y,z directions
Dim interval_rad As Integer 'number of gas iterations between global radiation or minor species calculations
    Const default_interval_rad = 200 'also used as the default for maxgi
Dim surf_type As Integer '0=> surface temperature is specified, 1=> calculated in melt
    Const default_surf_type = 0
Dim surf_temp As Single 'surface temperature when surf_type=0
    Const default_surf_temp = 1800
Dim nwl As Integer, iwl As Integer 'number of wavelengths, current number
Dim wl() As Single 'wavelengths
'wavelength defaults are set in file11, file12, openPreFile
Dim start_temp As Single 'temperature in domain at new start'
    Const default_start_temp_c = 300
    Const default_start_temp_m = 1206
Dim cycling As Integer '1=> doing domain cycling
Dim oxy_fuel As Integer '1=> using oxy_fuel for soot option
Dim initial_gitr As Integer 'number of combustion iterations at startup
                          'before first radiation calculation is done
    Const default_initial_gitr = default_interval_rad
'Soot kinetics
Dim esf As Double 'activation energy for soot formation J/kmol
   Const default_esf = 3325720#
Dim aform As Double 'kinetic constant soot formation
    Const default_aform = 0.00353
Dim eso As Double 'activation energy for soot oxidation J/kmol
   Const default_eso = 3325720#
Dim aoxid As Double 'kinetic constant soot oxidation
    Const default_aoxid = 30#
Dim isoot_cal As Integer 'soot kinetics calibration mode
    Const default_isoot_cal = 0 '0=> do not calibrate
Dim q_melt_req As Double 'energy required for melt
    Const default_q_melt_req = 1000000#
    'Const default_q_melt_req = 746943.52735085  '(W) TC21 case0020
    'Const default_q_melt_req = 2766746.0382403  '(W) Pittston B case1703
    'Note q_melt_req is no longer passed in the Sbc file, it is now an available parameter spot,
    'but the default is put in the default it....t file.
'More CFD controls
Dim maxri2 As Integer 'maximum radiosity solver inner loop 2 iteration number
    Const default_maxri2 = 1000
Dim maxri1 As Integer 'maximum radiosity solver outer loop 1 iteration number
    Const default_maxri1 = 100
Dim minri2 As Integer 'minimum radiosity solver inner loop 2 cycles (3-20)
    Const default_minri2 = 4
Dim preset_vf As Integer 'radiosity view factor option, 1=>calc once and save, 2=>calc as needed
    Const default_preset_vf = 1
Dim eps_m As Single 'emissivity of melt surface
    Const default_eps_m = 0.95
Dim eps_c As Single 'emissivity of ceiling or crown
    Const default_eps_c = 0.8  'use wall emissivity default (hard-coded in file11/12)
Dim maxms As Integer 'maximum minor species iterations per global iteration
    Const default_maxms = 200
Dim ms As Integer '1=> minor species calculation required
    Const default_ms = 1
Dim gui_update As Integer '1=> CFD should write gfm.dat file, 0=> don't write gfm.dat
    Const default_gui_update = 0
Dim n_cells_across_exit As Integer 'number of cells in cross section of exit (both directions)
    Const default_n_cells_across_exit = 6
Dim feed_unit As String
    Const default_feed_unit = "m^3/s"
    
'Data collection control flags, 1=> collect data, otherwise 0
Dim itwal As Integer 'flag for wall temperature array
    Const default_itwal = 1
Dim irad_detail As Integer 'flag for radiation details
    Const default_irad_detail = 1
Dim isum As Integer 'flag for run summary
    Const default_isum = 1
Dim irad_rad As Integer 'flag for radiosity
    Const default_irad_rad = 1
Dim iconv As Integer 'flag for gas mass residual convergence
    Const default_iconv = 1
Dim iTave As Integer 'flag for average temperatures
    Const default_iTave = 1
Dim iinfo As Integer 'flag for run information
    Const default_iinfo = 1
Dim igresid As Integer 'flag for gas PDE residual file (pressure, momentum, enthalpy)
    Const default_igresid = 1
Dim igresidx As Integer 'flag for gas PDE residual file (species, k, epsilon)
    Const default_igresidx = 1
Dim igresidp As Integer 'flag for gas pre-solve PDE residual file (pressure, momentum, enthalpy)
    Const default_igresidp = 0
Dim igresidxp As Integer 'flag for gas pre-solve PDE residual file (species, k, epsilon)
    Const default_igresidxp = 0
Dim imresid As Integer 'flag for minor species residual convergence file
    Const default_imresid = 1
Dim iflx As Integer 'flag for melt surface flux change
    Const default_iflx = 1
Dim irelax As Integer 'flag for melt surface temperature relaxation file
    Const default_irelax = 1
Dim ifieldview As Integer 'flag for output for FieldView
    Const default_ifieldview = 0
    
'Pre File for melter
'===================
'Note: Dim statements are commented out where the variables have already been defined for the
'         combustion pre file.  They are left here to show the file structure.
'Dim p0 As Long, q0 As Long 'plot origin
'Dim zf1 As Single, zf2 As Single, zf3 As Single 'plot scale factors
'Dim theta As Single 'plot angle stored as theta/pi*180
'Dim unt As String, vv0 As Single 'unit type and velocity vector length
'chamber_type, melter_component
'for chamber_type=2:  (1)melter, (2)throat, (3)refiner
'Group these 2 lines for each melter component (except no gaps for melter)
Dim lth_r(3) As Single, wdt_r(3) As Single, hgt_r(3) As Single 'dimensions
Dim gx_r(3) As Single, gy_r(3) As Single, gz_r(3) As Single 'gaps
'for other chamber_type:
'Dim lth As Single, wdt As Single, hgt As Single
    'furnace length, width, depth ^
    
'Wall information
'Dim thk(5) As Single 'wall thickness
    
'Charger Information
'Dim nbr As Integer, ibr As Integer 'number of chargers, current charger number
'burner index (in the pre file repeat this section for each index value)
'Dim bty() As Integer, bdr() As Integer 'type, orientation
'Dim bsg() As Single, bbg() As Single 'gaps to edges
'Dim bwd() As Single, bht() As Single 'width, height
'Dim btg() As Single 'temperature
'Dim bfg(:,1) As Single, bfg(:,2) As Single 'charge rate, cullet ratio
'Dim bav() As Single, bah() As Single 'sand/cullet size     Note 'batch' is used for 'sand' for displays
Dim batch_velocity() As Double
    Const default_batch_velocity = 0.003 'm/s
'Dim cl_s As Single, h0_s As Single 'specific heat, fussion for sand"
'Dim cl_c As Single, h0_c As Single 'specific heat, fussion for cullet"
Dim h0_s As Single 'fussion for sand"
Dim h0_c As Single 'fussion for cullet"

'Outlet Information
'Dim nex As Integer, iex As Integer 'number of outlets, current outlet number
'exhaust index (in the pre file repeat this section for each index value)
'Dim ety() As Integer, edr() As Integer 'type, orientation
'Dim esg() As Single, ebg() As Single 'gaps to edges
'Dim ewd() As Single, eht() As Single 'width, height
    
'Parameter Information
'Dim irstyp As Integer, maxgi As Integer 'restart indicator, maximum number of iterations
'Dim bgcon As Single, id_rad As Integer 'convergence criteria, id for radiation
'Dim pg0 As Single, qh0 As Single 'pressure, heat

'Bubbler Information
'Dim nsw As Integer, isw As Integer 'number of bubblers, current number
'side well index (in the pre file repeat this section for each index value)
'Dim wdr() As Integer 'orientation
'Dim wsg() As Single, wbg() As Single 'gaps (wbg only used for melter)
'Dim wtg() As Single, wfa() As Single 'temperature, flow rate
'Dim wwd() As Single, wdp() As Single 'diameter, height (width-->diameter, depth-->height)

'Grid Size
'Dim dx0 As Single, dy0 As Single, dz0 As Single 'maximum sizes for grid cells (delta values)
'Dim mp As Integer, np As Integer, lp As Integer 'number of grid cells in x,y,z directions

Dim epull() As Single 'pull rate for each outlet
' "pull rate"  refers to list above

'Electric Booster
Dim neb As Integer, ieb As Integer, jeb As Integer 'number of electric boosters, current booster
'booster index (in the pre file repeat this section for each index value)
Dim ebty() As Integer, ebvt() As Single, ebpw() As Single, ebhl() As Single
    Dim ebty0 As Integer 'not in prefile
'following items given as 3 lines for eb--(:,1-3)
Dim ebsg() As Single, ebbg() As Single, ebwd() As Single, ebht() As Single, ebdr() As Integer
 
Dim nphas As Integer 'number of phases
Dim nps0_s As Integer, ips0_s As Integer 'number of sand groups, current sand group
    Dim nbs0 As Integer, ibs0 As Integer 'not in pre file
Dim tm_s As Single, ds_s As Single 'sand melt temperature and density
'Dim bav(i,1) As Single for each sand group
' "radius"  refers to list above
Dim nps0_c As Integer, ips0_c As Integer 'number of cullet groups, current cullet group
Dim tm_c As Single, ds_c As Single 'cullet melt temperature and density
'Dim bah(i,1) As Single for each cullet group
' "radius"  refers to list above
'pg0 pressure ??? again

Dim heat_flux_type As Integer 'type of heat flux input to melt surface
    '1 => uniform heat flux over surface, subject to scaling to meet glass exit temperature
    '2 => calculated from combustion domain, subject to scaling to meet glass exit temperature
    '3 => user specified heat flux over surface, not subject to adjustment
    '4 => calculated from combustion domain, not subject to adjustment
    Const default_heat_flux_type = 1
Dim qsh As Single 'heat flux to surface
    Const default_heat_flux = 7
    
'Dim iwal As Integer, nwal As Integer 'current wall number, number of walls
'Dim wa_dr() As Integer 'preceeded by wall index, wall direction
'Dim wa_sg() As Single, wa_bg() As Single, wa_wd() As Single, wa_ht() As Single
    'wall geometry ^
'Dim wa_d() As Single, wa_k() As Single, wa_h() As Single, wa_ta() As Single, wa_e() As Single
    'wall properties ^
     
'User defined functions
Dim nudf As Integer, mudf As Integer, iudf As Integer, judf As Integer 'glass properties
Dim udf_ds_n As Integer 'number of entries for density function
Dim udf_ds() As Single, udf_ds_t() As Single 'ds,T  for each number above
Dim udf_mu_n As Integer 'number of entries for viscosity function
Dim udf_mu() As Single, udf_mu_t() As Single 'mu,T for each number above
Dim udf_k_n As Integer 'number of entries for conductivity function
Dim udf_k() As Single, udf_k_t() As Single 'k,T  for each number above
Dim udf_cl_n As Integer 'number of entries for liquid spe heat function
Dim udf_cl() As Single, udf_cl_t() As Single 'Cl,T  for each number above
Dim udf_a_n As Integer 'number of entries for vol absorp function
Dim udf_a() As Single, udf_a_m() As Single 'a,wl  for each number above
Dim udf_clc_n As Integer 'number of entries for cullet specific heat function
Dim udf_clc() As Single, udf_clc_t() As Single 'cl_c,T  for each number above
Dim udf_cls_n As Integer 'number of entries for sand specific heat function
Dim udf_cls() As Single, udf_cls_t() As Single 'cl_s,T  for each number above
Dim glassExitTemp As Single 'temperature of glass at the exit
    Const default_glassExitTemp = 1750
Dim condLength_s As Single 'conductivity thickness (length) for sand
    Const default_condLength_s = 0.016
Dim condLength_c As Single 'conductivity thickness (length) for cullet
    Const default_condLength_c = 0.016
'Dim start_temp As Single 'temperature in domain at new start'
'    Const default_start_temp_c = 300
'    Const default_start_temp_m = 1206
'Dim gui_update As Integer '1=> CFD should write gfm.dat file, 0=> don't write gfm.dat
'    Const default_gui_update = 0
Dim extra_tunnel_length As Single 'length to add for tunnel exits beyond the initial wall
    Const default_extra_tunnel_length = 3#
Dim extra_tunnel_cells As Integer 'number of cells in length of exit tunnel beyond the initial wall
    Const default_extra_tunnel_cells = 3
'Dim n_cells_across_exit As Integer 'number of cells in cross section of exit (both directions)
'    Const default_n_cells_across_exit = 6

'Data collection control flags, 1=> collect data, otherwise 0
Dim iadjf As Integer 'flag for adjusted flux
    Const default_iadjf = 1
Dim iadjr As Integer 'flag for adjusted relaxation
    Const default_iadjr = 1
Dim isum_m As Integer 'flag for run summary (melt)
    Const default_isum_m = 1
Dim iconv_m As Integer 'flag for melt convergence
    Const default_iconv_m = 1
Dim iTave_m As Integer 'flag for average temperatures (melt)
    Const default_iTave_m = 1
Dim iinfo_m As Integer 'flag for run information (melt)
    Const default_iinfo_m = 1
Dim igresid_m As Integer 'flag for glass PDE equation residual file
    Const default_igresid_m = 1
Dim igresidp_m As Integer 'flag for glass pre-solve PDE equation residual file
    Const default_igresidp_m = 0
Dim iTchg As Integer 'flag for melt surface temperature change
    Const default_iTchg = 1
Dim ifieldview_m As Integer 'flag for output for FieldView
    Const default_ifieldview_m = 0

'Grid File related variables
'---------------------------
'Dim dx0 As Single, dy0 As Single, dz0 As Single 'maximum sizes for grid cells (delta values)
'Dim mp As Integer, np As Integer, lp As Integer 'maximum grid cell indexes in x,y,z directions
Dim ibc() As Integer 'stores cell type for grid
Dim xg() As Single, yg() As Single, zg() As Single
Dim nx As Integer, ny As Integer, nz As Integer 'number of grid cells in x,y,z directions
Dim ix0 As Integer, iy0 As Integer, iz0 As Integer
Dim ibc1() As Integer
Dim x0 As Single, y0 As Single, z0 As Single
Dim xg1() As Single, yg1() As Single, zg1() As Single
Dim i As Integer, j As Integer, k As Integer
Dim x1a As Single, y1a As Single, z1a As Single
Dim surf_length_c As Single 'length of combustion space (used by melt space when cycling)
Dim surf_width_c As Single  'width  of combustion space (used by melt space when cycling)

'General Purpose
'---------------
Const pi = 3.14159265358979
Dim s0 As String
Dim s1 As String
Dim ttl As String
Dim g0 As Single, g1 As Single, g2 As Single
Dim int_temp As Integer
Dim int_temp2 As Integer

'Physical constants
'------------------

Const P_ref = 101325#  'Pressure reference state (Pa)
Const T_ref = 298.15   'Temperature of reference state (K)
Const Ru = 8314.472 'Universal gas constant (J/kmol-K)
Const W_O2 = 31.9988       'oxygen molecular weight (kg/kmol)
Const W_N2 = 28.0134       'nitrogen molecular weight (kg/kmol)
Const W_fuel = 16.0426     'fuel molecular weight (kg/kmol)
Const W_air = 0.79 * W_N2 + 0.21 * W_O2 'air molecular weight (kg/kmol) consistent with assuming only composed of O2 and N2
Const rho_O2_ref = P_ref * W_O2 / (Ru * T_ref) 'Oxygen density at reference state (kg/m^3)
Const rho_N2_ref = P_ref * W_N2 / (Ru * T_ref) 'Nitrogen density at reference state (kg/m^3)
Const rho_fuel_ref = P_ref * W_fuel / (Ru * T_ref) 'Fuel density at reference state (kg/m^3)
Const rho_air_ref = P_ref * W_air / (Ru * T_ref) 'air density at reference state (kg/m^3)

'Other
'-----
    'following 3 lines for walls
    Dim wa_i1() As Integer, wa_i2() As Integer
    Dim wa_j1() As Integer, wa_j2() As Integer
    Dim wa_k1() As Integer, wa_k2() As Integer
    'following 6 lines for burners
    Dim bi1() As Integer, bi2() As Integer
    Dim bj1() As Integer, bj2() As Integer
    Dim bk1() As Integer, bk2() As Integer
    'arrays for coordinates of inner section of type 4 burners
    Dim bi1_in() As Integer, bi2_in() As Integer
    Dim bj1_in() As Integer, bj2_in() As Integer
    Dim bk1_in() As Integer, bk2_in() As Integer
    'following 2 lines for ring burners
    Dim saved_ring_inner As Single ' 10-21-2004 ring inner diameter default
    Dim saved_bht_pre_ring As Single ' 10-21-2004 default height
    'following 3 lines for exhausts
    Dim ei1() As Integer, ei2() As Integer
    Dim ej1() As Integer, ej2() As Integer
    Dim ek1() As Integer, ek2() As Integer
    Dim gexh_wall_temp_type() As Integer 'temporary - type of initial exit wall temperature for radiation
    Dim gexh_wall_temp_frac() As Single 'temporary - indicates fraction of near wall temp
    Dim gexh_wall_temp_fixed() As Single 'temporary - indicates specified temp

    'following 3 lines for side wells (dog houses)
    Dim swi1() As Integer, swi2() As Integer
    Dim swj1() As Integer, swj2() As Integer
    Dim swk1() As Integer, swk2() As Integer
    'following 3 lines electric booster
    Dim ebi1() As Integer, ebi2() As Integer
    Dim ebj1() As Integer, ebj2() As Integer
    Dim ebk1() As Integer, ebk2() As Integer
    
Dim gpull() As Single
Dim eps0 As Single
Dim rgb0 As Long
Dim p1 As Long, q1 As Long
Dim p2 As Long, q2 As Long
Dim p01 As Long, p02 As Long
Dim p11 As Long, p12 As Long
Dim p21 As Long, p22 As Long
Dim q01 As Long, q02 As Long
Dim q11 As Long, q12 As Long
Dim q21 As Long, q22 As Long
Dim hgts As Long, wdts As Long
Dim gap As Integer
Dim m_sw As Integer
Dim nxb(3) As Integer, nxe(3) As Integer
Dim nxbs(3) As Integer, nxes(3) As Integer
Dim m1_cr As Integer, m2_cr As Integer
Dim tx() As Single, fx() As Single
Dim fr_s As Single, fr_c As Single
Dim gty() As Integer, gdr() As Integer
Dim d_00 As Single, d_01 As Single
Dim d_10 As Single, d_11 As Single
Dim fp_mx As Single, fp_mn As Single, vwr As Single
Dim dz0c As Single
Dim zf1a As Single, zf2a As Single, zf3a As Single
Dim fp_a1 As Single
Dim dr0 As Single, sg0 As Single, bg0 As Single
Dim gsg() As Single, gbg() As Single
Dim gwd() As Single, ght() As Single
Dim gfg() As Single, gfa() As Single
Dim gfn() As Single, gfo() As Single
Dim rg_b() As Single
Dim fp() As Single, ug() As Single
Dim fp_ut As String, fpn As String
Dim title As String
Dim dub0 As Double 'general use double temperary variable
Dim avail_dub0 As Double 'not used but available parameter to be passed in the Sbc file
Dim iw_back As Integer 'count of exits on given wall
Dim iw_front As Integer 'count of exits on given wall
Dim iw_right As Integer 'count of exits on given wall
Dim iw_left As Integer 'count of exits on given wall
Dim iw_bot As Integer 'count of exits on given wall
Dim iw_top As Integer 'count of exits on given wall
Dim tunnel_dx As Double
Dim need_tube_check As Boolean
Dim need_burner_check As Boolean


'-------------------------------
'-------------------------------
Private Sub arch()
  Dim d0 As Single, d1 As Single, d2 As Single
  Dim n As Integer, nt As Integer
  If hgt_c <= 0 Then Exit Sub
  nt = 10
  y0 = 0: z0 = hgt: Call proj2d
  p2 = p1: q2 = q1
  d0 = wdt / nt
  d1 = wdt / 2
  For n = 1 To nt
    y0 = n * d0
    d2 = (y0 - d1) / d1
    z0 = hgt + hgt_c * (1 - d2 ^ 2)
    Call proj2d: Line (p2, q2)-(p1, q1)
    p2 = p1: q2 = q1
  Next
End Sub


'-------------------------------
'Setup components submenu item within construct menu
'-------------------------------
Private Sub ctcpn_Click()
  Call ct_ls(5, 0, 0, 0)
  List1.List(0) = "Components:"
  If flow_domain = 2 Then 'In melt preprocessor section
    List1.List(1) = "Gas Bubbler"
    List1.List(2) = "Electric Booster"
    'List1.List(3) = "Wall Properties"
    'List1.List(4) = "Glass Properties"
    If chamber_type = 2 Then melter_component = 1
  Else
    If gridProtectMode Then
      List1.List(1) = ""
    Else
      List1.List(1) = "Dog House"
    End If
    'List1.List(2) = "Wall Properties"
  End If
End Sub

'-------------------------------
'Construct electric booster
'-------------------------------
Private Sub cteb_Click()
  Call eb_ls
End Sub

'-------------------------------
'Construct grid geometry
'-------------------------------
Private Sub ctgeo_Click()
  If chamber_type = 2 Then 'melter with refiner
    Call ct_ls(4, 4, 4, 0)
  Else
    Call ct_ls(5, 0, 0, 0)
  End If
  Call geom_ls
End Sub

'-------------------------------
'Construct burner or charger
'-------------------------------
Private Sub ctbr_Click()
  If flow_domain = 2 Then
    Call ct_ls(5, 5, 5, 5)
    If chamber_type = 2 Then melter_component = 1
    Call char_ls
  Else
    Call ct_ls(5, 5, 5, 5)
    Call burn_ls
  End If
End Sub

'-------------------------------
'Construct Exhaust or Outlet
'-------------------------------
Private Sub ctex_Click()
  If flow_domain = 2 And chamber_type = 2 Then melter_component = 3
  Call ct_ls(5, 5, 5, 0)
  Call exh_ls
End Sub


Private Sub ctgd_Click()
'-------------------------------
'Construct new grid
'-------------------------------
  ttl = LCase(Caption)
  If InStr(ttl, "melter") > 0 Then
    mnuProp_GlassExitTemp.Visible = False
    mnuPropGlass.Visible = False
  Else
    mnuPropEmis.Visible = False
    mnuPropSoot.Visible = False
    mnuPropCFD.Visible = False
  End If
  Call grid(0)
  Call initialize_grid_display      ' 8-6-2004
    Call setup(2)
    Call plt_gd(0)
  flag_grid_modified = False
  flag_grid_active = True
  flag_grid_constructed = True
  flag_grid_enhanced = False
  grid_type = Not_enhanced
  MnuOptFed.Visible = False
End Sub


'-------------------------------
'Initialize_grid_display for grid construction
'-------------------------------
Private Sub initialize_grid_display() ' 8-6-2004
  menu_layer = InGridConstruction
  mnuView.Visible = True
  mnuProtectGrid.Visible = False
  active_view = vw_grid
  opt4.Visible = True: sim0.Visible = False
  List1.Visible = False: List2.Visible = False
  List3.Visible = False: List4.Visible = False
  frmMain.Caption = "Grid Construction"
  Lbadd.Caption = "add": Lbdel.Caption = "del"
  Lbmv.Caption = "move": Lbsw.Caption = "sweep"
  x1a = xg(mp) - xg(1): y1a = yg(np) - yg(1)
  z1a = zg(lp) - zg(1)
  If hgt_c > 0 Then g2 = 1 Else g2 = zf3 / zf2
  g0 = 10000 / (x1a + z1a * g2): g1 = 5000 / (y1a + z1a * g2)
  If g0 > g1 Then g0 = g1
  zf1 = g0: zf2 = g0: zf3 = g0 * g2
End Sub
 
 
'-------------------------------
'Clear out the green lists and display only those with elements specified
'-------------------------------
Private Sub ct_ls(n1 As Integer, n2 As Integer, _
n3 As Integer, n4 As Integer)
  'n1 => number of elements in List1
  'n2 => number of elements in List2
  'n3 => number of elements in List3
  'n4 => number of elements in List4
  Dim n As Integer
  List1.Clear
  If n1 > 0 Then
    List1.Width = 2000: List1.Height = 300 * n1
    List1.Left = 500: List1.Top = hgts - List1.Height
    List1.Visible = True
    For n = 0 To n1 - 1: List1.AddItem "": Next
  Else
    List1.Visible = False
  End If
  List2.Clear
  If n2 > 0 Then
    List2.Width = 2000: List2.Height = 300 * n2
    List2.Left = List1.Left + List1.Width + 500
    List2.Top = hgts - List2.Height
    List2.Visible = True
    For n = 0 To n2 - 1: List2.AddItem "": Next
  Else
    List2.Visible = False
  End If
  List3.Clear
  If n3 > 0 Then
    List3.Width = 2000: List3.Height = 300 * n3
    List3.Left = List2.Left + List2.Width + 500
    List3.Top = hgts - List3.Height
    List3.Visible = True
    For n = 0 To n3 - 1: List3.AddItem "": Next
  Else
    List3.Visible = False
  End If
  List4.Clear
  If n4 > 0 Then
    List4.Width = 2000: List4.Height = 300 * n4
    List4.Left = List3.Left + List3.Width + 500
    List4.Top = hgts - List4.Height
    List4.Visible = True
    For n = 0 To n4 - 1: List4.AddItem "": Next
  Else
    List4.Visible = False
  End If
End Sub

'-------------------------------
' Choose GlassProp in Construct-->Components menu
'-------------------------------
Private Sub ctgp_Click()
  Dim m As Integer
  nudf = 1 'identifies current glass property
  iudf = 1 'current entry index to property function
  judf = 1
  mudf = udf_ds_n 'mudf=total number of entries in given function
  ReDim tx(mudf), fx(mudf)
  For m = 1 To mudf
    fx(m) = udf_ds(m): tx(m) = udf_ds_t(m)
  Next
  Call udf_ls
End Sub

'-------------------------------
'Construct simulation parameters
'-------------------------------
Private Sub ctsm_Click()
    If flow_domain = 1 Then
        Call ct_ls(5, 5, 5, 5)
    Else
        Call ct_ls(5, 5, 5, 0)
    End If
    If chamber_type = 2 Then melter_component = 1
    Call sim_ls
End Sub

'-------------------------------
'Construct component dog house or bubbler
'-------------------------------
Private Sub ctsw_Click()
  If flow_domain = 2 Then
      Call bub_ls
      Exit Sub
  End If
  If gridProtectMode Then Exit Sub
  If nsw <= 0 Then
      Call ct_ls(4, 0, 0, 0)
      List1.List(0) = "Dog House"
      List1.List(1) = "#0/0"
      List1.List(2) = "add"
      Exit Sub
  End If

  Call ct_ls(4, 4, 0, 0)
  List1.List(0) = "Dog House:"
  List1.List(1) = "#" & isw & "/" & nsw
  List1.List(2) = "add"
  List1.List(3) = "delete"
  j = isw: Call g1_ls(wdr(j))
  If wdr(j) < 2 Then g0 = wdt Else g0 = lth
  If wwd(j) > g0 Then
    wwd(j) = g0: wsg(j) = 0
  ElseIf wsg(j) + wwd(j) > g0 Then
    wsg(j) = g0 - wwd(j)
  End If
  Call unit_l(wsg(j), s0)
  If wdr(j) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  Call unit_l(wwd(j), s0)
  List2.List(2) = "width=" & s0
  Call unit_l(wdp(j), s0)
  List2.List(3) = "depth=" & s0
End Sub

'-------------------------------
'Construct wall properties
'-------------------------------
Private Sub ctwp_Click()
  Call wa_ls
End Sub

Private Sub ex0_Click()
'-------------------------------
'Exit menu layer or program
'-------------------------------
    Dim sMessage As String
    
    If menu_layer = InMain Then
        'Normal exit from program
        'Files should be saved or simulation stopped before getting to this point
        'get rid of any leftover files
        On Error Resume Next
        Kill dnm & "tmp\*.*"
        If case_path <> "" Then
            s0 = case_path & "\"
            If FileExist(s0 & "gfm.dat") Then Kill s0 & "gfm.dat"
            If FileExist(s0 & "gfm0.dat") Then Kill s0 & "gfm0.dat"
            If FileExist(s0 & "runstop.dat") Then Kill s0 & "runstop.dat"
        End If
        End
        Exit Sub
    End If
  
  If menu_layer = InGridConstruction Or menu_layer = InPreProc Then
      If (flag_preproc_modified = True Or flag_grid_modified = True Or _
            flag_grid_constructed = True Or flag_conditions_modified = True Or _
            flag_grid_enhanced = True) And case_number <> "" Then
        'Should save files
        sMessage = "Do you want the current case saved before exiting " _
            & "the Pre-Processor?"
        If MsgBox(sMessage, vbYesNo + vbCritical, _
                "GFM Warning about Loss of User Work") = vbYes Then
            'Automatically save the case for user
            Call mnuCaseSave_Click
        Else 'no save done
            'Note: if the user has canceled out of creating a new case, then the case
            'folder will need to be deleted by the user.                   @@@ Need to explain in User Guide.
            'Problems with using RmDir will need to be investigated in detail
            'before automating the delete.                                            @@@
        End If
      End If
      flag_preproc_modified = False
      flag_preproc_open = False
      flag_preproc_saved = False
      flag_grid_active = False
      flag_grid_modified = False
      flag_grid_constructed = False
      flag_grid_enhanced = False
      grid_type = Not_enhanced
      flag_grid_saved = False
      flag_conditions_modified = False
      flag_conditions_saved = False
      'turn off grid protect mode
      mnuProtectGrid.Checked = False
      mnuProtectGrid.Enabled = True
      mnuProtectGrid.Visible = True
      gridProtectMode = False
      ctgeo.Enabled = True
      ctgd.Enabled = True
      opt4.Enabled = True 'enable density changes
      ctex.Enabled = True
      If flow_domain = 1 Then
         ctsw.Enabled = True
      End If
      'end of turn off grid protect mode
      'Leave the Pre-Processor and return to the main menu

  ElseIf menu_layer = InSimulation Then
    If Lbsw.Caption = "ending" Then
        If MsgBox("The simulation run has already been signalled to stop." _
            & Chr(10) & Chr(10) _
            & "Wait until the simulation run is at a stopping point " _
            & "and a restart file has been created.", _
            vbOKOnly + vbInformation, "GFM") = vbOK Then Exit Sub
    ElseIf Lbsw.Caption = "stop" Then
        If (cycleDomains = True Or cycleDomainsMelt = True) Then
            If MsgBox("A cycle simulation run is in progress.   " _
                    & "Do you want to finish the current cycle before stopping?", _
                    vbYesNo + vbExclamation, "GFM") = vbYes Then
                cycleEnd = cycleInfo
                s0 = CStr(cycleEnd)
                ListCycle.List(2) = "   In cycle " & s0 & " of " & s0
            Else
                'signal CDF code to stop
                If flow_domain = 1 Then
                    s0 = dnm & "combustion\runs.dat"
                Else
                    s0 = dnm & "melt\runs.dat"
                End If
                FileCopy s0, case_path & "\runstop.dat"
                userStoppedSim = True
                Lbsw.BackColor = vbYellow: Lbsw.Caption = "ending"
            End If
            Exit Sub
        Else 'not cycling
            If MsgBox("A simulation run is in progress.  Your convergence criteria has " _
                & "not been met.  Are you sure you want to save results and stop?", _
                vbYesNo + vbExclamation, "GFM") = vbYes Then
                    'signal CDF code to stop
                    If flow_domain = 1 Then
                        s0 = dnm & "combustion\runs.dat"
                    Else
                        s0 = dnm & "melt\runs.dat"
                    End If
                    FileCopy s0, case_path & "\runstop.dat"
                    userStoppedSim = True
                    Lbsw.BackColor = vbYellow: Lbsw.Caption = "ending"
            End If
            Exit Sub
        End If 'cycling check
    ElseIf Lbsw.Caption = "cont" Then
        If List1.List(0) = "Running" And List1.Visible = True Then
            List1.List(0) = "Run Done"
        End If
        'The simulation has already been stopped, so go back to the main menu
        ListCycle.Visible = False
    End If
  'If menu_layer = InPostProc Then just return to main menu
  End If
  
  'Return to main menu
  menu_layer = InMain
  ex0.Caption = "E&xit"
  optplt.Visible = False
  mnuOptRunPlot.Visible = False
  MnuOptFed.Visible = False
  MnuOptCol.Visible = False
  mnuProp_GlassExitTemp.Visible = False
  mnuPropGlass.Visible = False
  mnuPropEmis.Visible = False
  mnuPropSoot.Visible = False
  mnuPropCFD.Visible = False
  If Lb3d.Caption = "2D" Then Lb3d.Caption = "3D"
  Call display_welcome_screen
End Sub


Private Sub file11_Click()
'-------------------------------
'Get default crown furnace or refiner
'Enter here from case->new->crown or case->new->w/refiner.
'Note that feed_unit related items default to a volume based flow rate,
'whereas the CFD code is based on a mass flow rate.
'-------------------------------
    'Begin new case
    If create_new_case = False Then Exit Sub

    If flow_domain = 1 Then 'combustion
        Caption = Caption & "/" & file11.Caption
        chamber_type = 0: hgt_c = 1.2
        id_rad = 1 'default value for combustion
        interval_rad = default_interval_rad
        surf_type = default_surf_type
        surf_temp = default_surf_temp
        'wavelength defaults are hard coded
        nwl = 10: iwl = 1
        ReDim wl(nwl)
        For i = 1 To 7: wl(i) = i: Next
        wl(8) = 8.5: wl(9) = 12: wl(10) = 15
        start_temp = default_start_temp_c 'initialize new start temperature
        oxy_fuel = 0
        initial_gitr = default_initial_gitr
        esf = default_esf
        aform = default_aform
        eso = default_eso
        aoxid = default_aoxid
        isoot_cal = default_isoot_cal
        mnuPropSootCali.Checked = False
        'mnuPropSootNeed.Enabled = False
        q_melt_req = default_q_melt_req
        maxri2 = default_maxri2
        maxri1 = default_maxri1
        minri2 = default_minri2
        preset_vf = default_preset_vf
        mnuPropCFDvf.Checked = True
        eps_m = default_eps_m
        eps_c = default_eps_c
        maxms = default_maxms
        ms = default_ms
        MnuOptFed.Visible = True
        feed_unit = default_feed_unit
        MnuOptFedMas.Checked = False
        MnuOptFedVol.Checked = True
        qh0 = default_qh0
        
        'Data collection control flags set to 1
        itwal = default_itwal
        irad_detail = default_irad_detail
        isum = default_isum
        irad_rad = default_irad_rad
        iconv = default_iconv
        iTave = default_iTave
        iinfo = default_iinfo
        igresid = default_igresid
        igresidx = default_igresidx
        igresidp = default_igresidp
        igresidxp = default_igresidxp
        imresid = default_imresid
        iflx = default_iflx
        irelax = default_irelax
        ifieldview = default_ifieldview
        
    ElseIf flow_domain = 2 Then 'melt
        Caption = Caption & "/refiner"
        chamber_type = 2: melter_component = 1: hgt_c = 0
        glassExitTemp = default_glassExitTemp
        condLength_s = default_condLength_s
        condLength_c = default_condLength_c
        heat_flux_type = default_heat_flux_type
        qsh = default_heat_flux
        start_temp = default_start_temp_m 'initialize new start temperature
        extra_tunnel_cells = default_extra_tunnel_cells
        extra_tunnel_length = default_extra_tunnel_length
        MnuOptFed.Visible = False
        ebty0 = 0: neb = 0: ieb = 0: jeb = 0
                
        'Data collection control flags set to 1
        isum_m = default_isum_m
        iconv_m = default_iconv_m
        iTave_m = default_iTave_m
        iinfo_m = default_iinfo_m
        igresid_m = default_igresid_m
        igresidp_m = default_igresidp_m
        iTchg = default_iTchg
        iadjf = default_iadjf
        iadjr = default_iadjr
        ifieldview_m = default_ifieldview_m
    End If
    
    cycling = 0
    gui_update = default_gui_update
    MnuOptGui.Visible = True
    MnuOptGuiOn.Checked = False
    MnuOptGuiOff.Checked = True
    MnuOptCol.Visible = True
    n_cells_across_exit = default_n_cells_across_exit

    'set default wall properties
    ReDim wa_d(1), wa_k(1), wa_h(1), wa_ta(1), wa_e(1)
    ReDim wa_sg(1), wa_bg(1), wa_wd(1), wa_ht(1), wa_dr(1)
    nwal = 1: iwal = 1
    wa_k(1) = 2: wa_h(1) = 5: wa_ta(1) = T_ref
    wa_d(1) = 0.31: wa_dr(1) = 1: wa_e(1) = 0.8
    wa_sg(1) = 0: wa_bg(1) = 0: wa_wd(1) = 0: wa_ht(1) = 0
   
    Call file10(0) 'Draw default furnace on screen
    flag_conditions_modified = False
    mnuCaseNew.Enabled = False
    mnuCaseOpen.Enabled = False
    mnuCase.Enabled = True
    mnuCaseSave.Enabled = True
    mnuCaseSaveAs.Enabled = True
    mnuCaseSaveAsFull.Enabled = True
    mnuCaseDelete.Enabled = False
    mnuCaseDelResults.Enabled = False
End Sub

Private Sub file12_Click()
'-------------------------------
'Get default box or melter
'Enter here from case->new->box or case->new->melter.
'Note that feed_unit related items default to a volume based flow rate,
'whereas the CFD code is based on a mass flow rate.
'-------------------------------
    'Begin new case
    If create_new_case = False Then Exit Sub
    If flow_domain = 1 Then 'combustion
        Caption = Caption & "/" & file12.Caption
        id_rad = 1 'default value for combustion
        interval_rad = default_interval_rad
        surf_type = default_surf_type
        surf_temp = default_surf_temp
        'wavelength defaults are hard coded
        nwl = 10: iwl = 1
        ReDim wl(nwl)
        For i = 1 To 7: wl(i) = i: Next
        wl(8) = 8.5: wl(9) = 12: wl(10) = 15
        start_temp = default_start_temp_c 'initialize new start temperature
        oxy_fuel = 0
        initial_gitr = default_initial_gitr
        esf = default_esf
        aform = default_aform
        eso = default_eso
        aoxid = default_aoxid
        isoot_cal = default_isoot_cal
        mnuPropSootCali.Checked = False
        'mnuPropSootNeed.Enabled = False
        q_melt_req = default_q_melt_req
        maxri2 = default_maxri2
        maxri1 = default_maxri1
        minri2 = default_minri2
        preset_vf = default_preset_vf
        mnuPropCFDvf.Checked = True
        eps_m = default_eps_m
        eps_c = default_eps_c
        maxms = default_maxms
        ms = default_ms
        MnuOptFed.Visible = True
        feed_unit = default_feed_unit
        MnuOptFedMas.Checked = False
        MnuOptFedVol.Checked = True
        qh0 = default_qh0
    
        'Data collection control flags set to 1
        itwal = default_itwal
        irad_detail = default_irad_detail
        isum = default_isum
        irad_rad = default_irad_rad
        iconv = default_iconv
        iTave = default_iTave
        iinfo = default_iinfo
        igresid = default_igresid
        igresidx = default_igresidx
        igresidp = default_igresidp
        igresidxp = default_igresidxp
        imresid = default_imresid
        iflx = default_iflx
        irelax = default_irelax
        ifieldview = default_ifieldview
        
    ElseIf flow_domain = 2 Then 'melt
        glassExitTemp = default_glassExitTemp
        condLength_s = default_condLength_s
        condLength_c = default_condLength_c
        heat_flux_type = default_heat_flux_type
        qsh = default_heat_flux
        start_temp = default_start_temp_m 'initialize new start temperature
        extra_tunnel_cells = default_extra_tunnel_cells
        extra_tunnel_length = default_extra_tunnel_length
        MnuOptFed.Visible = False
        ebty0 = 0: neb = 0: ieb = 0: jeb = 0
        
        'Data collection control flags set to 1
        isum_m = default_isum_m
        iconv_m = default_iconv_m
        iTave_m = default_iTave_m
        iinfo_m = default_iinfo_m
        igresid_m = default_igresid_m
        igresidp_m = default_igresidp_m
        iTchg = default_iTchg
        iadjf = default_iadjf
        iadjr = default_iadjr
        ifieldview_m = default_ifieldview_m
    End If
    
    cycling = 0
    gui_update = default_gui_update
    MnuOptGui.Visible = True
    MnuOptGuiOn.Checked = False
    MnuOptGuiOff.Checked = True
    MnuOptCol.Visible = True
    n_cells_across_exit = default_n_cells_across_exit
    
    'set default wall properties
    ReDim wa_d(1), wa_k(1), wa_h(1), wa_ta(1), wa_e(1)
    ReDim wa_sg(1), wa_bg(1), wa_wd(1), wa_ht(1), wa_dr(1)
    nwal = 1: iwal = 1
    wa_k(1) = 2: wa_h(1) = 5: wa_ta(1) = T_ref
    wa_d(1) = 0.31: wa_dr(1) = 1: wa_e(1) = 0.8
    wa_sg(1) = 0: wa_bg(1) = 0: wa_wd(1) = 0: wa_ht(1) = 0
   
    chamber_type = 1: hgt_c = 0:
    Call file10(0) 'Draw default furnace on screen
    flag_conditions_modified = False
    mnuCaseNew.Enabled = False
    mnuCaseOpen.Enabled = False
    mnuCase.Enabled = True
    mnuCaseSave.Enabled = True
    mnuCaseSaveAs.Enabled = True
    mnuCaseSaveAsFull.Enabled = True
    mnuCaseDelete.Enabled = False
    mnuCaseDelResults.Enabled = False
End Sub

'-------------------------------
'Draw default furnace or grid on screen
'-------------------------------
Private Sub file10(n0 As Integer)
  Dim i As Integer
  If n0 = 1 Then 'Have a furnace already
      'mnuView.Visible = True ' 8-5-2004
      active_view = vw_fig
      GoTo f10a
  End If
  
  'n0=0  Create new furnace
  p0 = 2000: q0 = 4500
  theta = 70 / 180 * pi
  unt = "SI": opt12.Checked = True: opt11.Checked = False
  zf1 = 300: zf2 = zf1: zf3 = zf1 * 4
  lth = 20: wdt = 8
  For i = 0 To 5: thk(i) = 0.31: Next
  nbr = 1: ibr = 1 '1 burner
  ReDim bty(1), bdr(1), btg(1, 2)
  ReDim bsg(1, 2), bbg(1, 2), bwd(1, 2), bht(1, 2)
  ReDim bav(1, 2), bah(1, 2), bfg(1, 2)
  nex = 1: iex = 1 '1 exhaust
  ReDim ety(1), edr(1), esg(1), ebg(1), ewd(1), eht(1)
  dx0 = 0.5: dy0 = 0.4: dz0 = 0.2
  nsw = 0: isw = 0
  neb = 0: ieb = 0
  mp = 0: np = 0: lp = 0
  If flow_domain = 2 Then GoTo f10b
  
  ' continue with combustion, set default values
  thk(4) = 0: hgt = 1.2
  ReDim bas(1)
  ReDim bfa(1, 2), bfo(1, 2), bfn(1, 2)
  bty(1) = 0: bdr(1) = 2: btg(1, 1) = 300: btg(1, 2) = 300
  bsg(1, 1) = 8: bbg(1, 1) = 0.4
  bwd(1, 1) = 0.3: bht(1, 1) = 0.3
  bwd(1, 2) = 0#: bht(1, 2) = 0#   'inner tube-in-tube values will be calculated June07
  bsg(1, 2) = 0#: bbg(1, 2) = 0#
  'bwd(1, 2) = 0.1016 '4 inch inner width for regenerative burner   10-20-2004
  'bht(1, 2) = 0.0762 '3 inch inner height
  's_inner = s + w/2 - w_inner/2,  always calculated
  'b_inner = b + h/2 - h_inner/2
  ' 10-21-2004 Note that bht is used for rings as the inner diameter.
  'For other burner types bht is the height.
  saved_ring_inner = bwd(1, 1) / 2 ' 10-21-2004 ring inner diameter default
  saved_bht_pre_ring = bht(1, 1) ' 10-21-2004 default height
  'bsg(1, 2) = 8 + 0.15 - 0.0508
  'bbg(1, 2) = 0.4 + 0.15 - 0.0381
  bav(1, 1) = 0: bah(1, 1) = 0
  bav(1, 2) = 0: bah(1, 2) = 0: bas(1) = 0
  bfg(1, 1) = 0.1: bfa(1, 1) = 0.4764
  bfo(1, 1) = 0: bfn(1, 1) = 0
  bfg(1, 2) = 0: bfa(1, 2) = 0
  bfo(1, 2) = 0: bfn(1, 2) = 0
  ety(1) = 0: edr(1) = 3: esg(1) = 2: ebg(1) = 0.2
  ewd(1) = 1: eht(1) = 0.5
  ReDim exh_wall_temp_type(1), exh_wall_temp_fixed(1), exh_wall_temp_frac(1)
  exh_wall_temp_type(1) = default_exh_temp_type
  exh_wall_temp_fixed(1) = default_exh_temp_fixed
  exh_wall_temp_frac(1) = default_exh_temp_frac
  mnuView.Visible = False ' 8-5-2004
  active_view = 0
  mnuPropEmis.Visible = True
  mnuPropSoot.Visible = True
  mnuPropCFD.Visible = True
  
f10a: 'n0=1
  'Come here after file-open-pre-process has read .pre and grid
  '      old:  Or clicked exit when doing grid construction
  'Update what menu items are displayed
    
  menu_layer = InPreProc
  List1.Visible = False: List2.Visible = False
  List3.Visible = False: List4.Visible = False
  Lb14.Visible = True
  ct0.Visible = True
  mnuProperties.Visible = True
  MnuOptCol.Visible = True
  opt0.Visible = True
  opt4.Visible = False
  LbO.Left = p0 - gap: LbO.Top = q0 + gap  'position move and resize handles
  LbO.Visible = True: LbP.Visible = True
  If flag_grid_active = True Then mnuProtectGrid.Visible = True
  Call plt_cb 'draw the furnace figure on screen
  Exit Sub

f10b:   ' continue with melt
  thk(5) = 0: hgt = 1#
  bty(1) = 2: bdr(1) = 0: btg(1, 1) = 300
  'bty(1) = 2: bdr(1) = 3: btg(1, 1) = 300 'test with left wall default
  bwd(1, 1) = 6: bht(1, 1) = 0.1
  bsg(1, 1) = 1: bbg(1, 1) = hgt - bht(1, 1)
  bfg(1, 1) = 3: bfg(1, 2) = 0.5
  bav(1, 1) = 70: bah(1, 1) = 1000
  ReDim batch_velocity(1)
  batch_velocity(1) = default_batch_velocity
  ety(1) = 0: edr(1) = 1
  ewd(1) = 0.5: eht(1) = 0.25
  esg(1) = (wdt - ewd(1)) / 2: ebg(1) = 0
  ReDim epull(1)
  epull(1) = 1: ds_s = 2200: ds_c = 2200
  lth_r(1) = lth: wdt_r(1) = wdt: hgt_r(1) = hgt
  lth_r(2) = 3: wdt_r(2) = 1: hgt_r(2) = 0.2
  lth_r(3) = 5: wdt_r(3) = wdt: hgt_r(3) = hgt
  gx_r(1) = 0: gy_r(1) = 0: gz_r(1) = 0
  gx_r(2) = lth: gy_r(2) = (wdt - wdt_r(2)) / 2: gz_r(2) = 0
  gx_r(3) = lth + lth_r(2): gy_r(3) = 0: gz_r(3) = 0
  mnuProp_GlassExitTemp.Visible = True
  mnuPropGlass.Visible = True
  GoTo f10a
End Sub

'-------------------------------
'Draw chamber (furnace or melter) stick figure on screen
' Lb1 will be "Length"
' Lb2 will be "Width"
' Lb3 will be "Height" or "Depth"
' Lb4 will be "Crown Height"
'-------------------------------
Private Sub plt_cb()
  Dim n As Integer, m As Integer
  Dim ht0 As Single, wd0 As Single, r0 As Single
  Dim p4 As Single, q4 As Single, p3 As Single, q3 As Single
  Dim i1 As Integer
  
  frmMain.Picture = LoadPicture() 'clear drawing area
  If chamber_type = 2 Then 'melter with refiner
    If lth_r(3) <= 0 Then
      wdt_r(3) = wdt_r(2): hgt_r(3) = hgt_r(2)
      gy_r(3) = gy_r(2): gz_r(3) = gz_r(2)
      edr(1) = 1: esg(1) = 0: ebg(1) = 0
      ewd(1) = wdt_r(3): eht(1) = hgt_r(3)
    End If
    lth = lth_r(melter_component): wdt = wdt_r(melter_component): hgt = hgt_r(melter_component)
    x0 = lth: y0 = wdt: z0 = hgt: Call proj2d
    zf1a = zf1: zf2a = zf2: zf3a = zf3
    If melter_component >= 2 Then
      g0 = hgt_r(1) / hgt_r(melter_component)
      zf1 = zf1 * g0: zf2 = zf2 * g0: zf3 = zf3 * g0
    End If
  End If
  x0 = lth: y0 = wdt: z0 = hgt: Call proj2d
  LbP.Left = p1 + gap: LbP.Top = q1 - gap
  Optx.Visible = False: Opty.Visible = False
  Optz.Visible = False
  Lbadd.Visible = False: Lbdel.Visible = False
  Lbmv.Visible = False: Lb3d.Visible = False
  Lbfr.Visible = False: Lbto.Visible = False
  Lbnd.Visible = False: Lb12.Visible = False
  Lbsw.Visible = False: Lb14.Visible = True
  tx_xb.Visible = False: tx_yb.Visible = False
  tx_zb.Visible = False: tx_xe.Visible = False
  tx_ye.Visible = False: tx_ze.Visible = False
  tx_xc.Visible = False: tx_yc.Visible = False
  tx_zc.Visible = False
  Cls
  ForeColor = vbBlue
  DrawStyle = 0
  x0 = lth: y0 = 0: z0 = 0: Call proj2d
  Line (p0, q0)-(p1, q1)
  z0 = hgt: Call proj2d: Line -(p1, q1)
  x0 = 0: Call proj2d: Line -(p1, q1): Line -(p0, q0)
  p2 = p1: q2 = q1
  y0 = wdt: Call proj2d: Line (p2, q2)-(p1, q1)
  x0 = lth: Call proj2d: Line -(p1, q1)
  p2 = p1: q2 = q1
  y0 = 0: Call proj2d: Line -(p1, q1)
  y0 = wdt: z0 = 0: Call proj2d: Line (p2, q2)-(p1, q1)
  y0 = 0: Call proj2d: Line -(p1, q1)
  DrawStyle = 2
  x0 = 0: y0 = wdt: z0 = 0: Call proj2d
  Line (p0, q0)-(p1, q1)
  p2 = p1: q2 = q1
  z0 = hgt: Call proj2d: Line -(p1, q1)
  x0 = lth: z0 = 0: Call proj2d: Line (p2, q2)-(p1, q1)
  DrawStyle = 0
  If flow_domain = 1 And hgt_c > 0 Then
    x0 = 0: Call arch
    x0 = lth: Call arch
    x0 = 0: y0 = wdt / 2: z0 = hgt + hgt_c: Call proj2d
    p2 = p1: q2 = q1
    x0 = lth: Call proj2d: Line (p2, q2)-(p1, q1)
  End If
'
  x0 = lth / 2: y0 = 0: z0 = 0: Call proj2d
  Lb1.ForeColor = vbBlue: Lb1.Font.Size = 12
  Lb1.Left = p1 - 400: Lb1.Top = q1 + 100
  Lb1.Caption = "Length": Lb1.Visible = True
  x0 = lth: y0 = wdt / 2: z0 = 0: Call proj2d
  Lb2.ForeColor = vbBlue: Lb2.Font.Size = 12
  Lb2.Left = p1 + 400: Lb2.Top = q1 - 100
  Lb2.Caption = "Width": Lb2.Visible = True
  x0 = 0: y0 = 0: z0 = hgt / 2: Call proj2d
  Lb3.ForeColor = vbBlue: Lb3.Font.Size = 12
  Lb3.Left = p1 - 800: Lb3.Top = q1 - 100
  Lb3.Visible = True: Lb4.Visible = False
  If flow_domain = 2 Then
    Lb3.Caption = "Depth"
  Else
    Lb3.Caption = "Height"
    If hgt_c > 0 Then
      z0 = hgt + hgt_c / 2: Call proj2d
      Lb4.ForeColor = vbBlue: Lb4.Font.Size = 12
      Lb4.Left = p1 - 1200: Lb4.Top = q1 - 100
      Lb4.Caption = "Crown Height": Lb4.Visible = True
  End If: End If
' burner/charger
  If chamber_type = 2 And melter_component > 1 Then GoTo pcbx1
  FontSize = 7: ttl = LCase(List1.List(0))
  For n = 1 To nbr
    dr0 = bdr(n): ForeColor = RGB(0, 150, 0)
    If bty(n) >= 2 Then
      If bty(n) = 4 Then
         'have inner part of regenerative burner to draw   10-20-2004
         sg0 = bsg(n, 2): bg0 = bbg(n, 2)
         ht0 = bht(n, 2): wd0 = bwd(n, 2): GoSub pcb3
      End If
      sg0 = bsg(n, 1): bg0 = bbg(n, 1)
      ht0 = bht(n, 1): wd0 = bwd(n, 1): GoSub pcb3
      CurrentY = q1 + 50
      If bty(n) = 3 Then
        ht0 = ht0 / 3: bg0 = bg0 + ht0: GoSub pcb3
      End If
    Else
      sg0 = bsg(n, 1): bg0 = bbg(n, 1)
      Call projw
      If bty(n) = 1 Then
        r0 = bht(n, 1) / 2 * zf1
        Circle (p1, q1), r0, ForeColor
      End If
      r0 = bwd(n, 1) / 2 * zf1
      Circle (p1, q1), r0, ForeColor
      CurrentY = q1 + r0 * 2
    End If
    If flow_domain = 1 Then s0 = "b" Else s0 = "c"
    If n = ibr And _
      (InStr(ttl, "burn") Or InStr(ttl, "char")) _
      Then ForeColor = vbMagenta
    Print s0 & n
  Next
'exhaust/outlet
pcbx1:
  If chamber_type = 2 And melter_component <> 3 Then GoTo pcbx2
  For n = 1 To nex
    dr0 = edr(n): ForeColor = vbRed
    sg0 = esg(n): bg0 = ebg(n)
    ht0 = eht(n): wd0 = ewd(n): GoSub pcb3
    CurrentX = p1 + 50: CurrentY = q1 - 200
    If flow_domain = 1 Then s0 = "e" Else s0 = "o"
    If n = iex And (InStr(ttl, "exh") Or InStr(ttl, "out")) _
      Then ForeColor = vbBlue
    Print s0 & n
  Next
'dog house/bubbler
pcbx2:
  For n = 1 To nsw
    If flow_domain = 2 Then ForeColor = vbCyan _
    Else ForeColor = vbBlue
    If n = isw And InStr(ttl, "bub") _
      Then ForeColor = vbMagenta
    dr0 = wdr(n): g0 = wdp(n)
    If flow_domain = 2 Then
     ' sg0 = wsg(n): bg0 = wbg(n)
     ' ht0 = wdp(n): wd0 = wwd(n): GoSub pcb3
     
     'Assume bubbler comes from bottom
     'Draw diagonal plane through bubbler
      x0 = wsg(n): y0 = wbg(n): z0 = 0
      Call proj2d: p2 = p1: q2 = q1
      x0 = x0 + wwd(n): y0 = y0 + wwd(n): Call proj2d
      Line (p2, q2)-(p1, q1)
      z0 = wdp(n): Call proj2d
      Line -(p1, q1)
      x0 = wsg(n): y0 = wbg(n): Call proj2d
      Line -(p1, q1)
      Line -(p2, q2)
    Else
    If dr0 < 2 Then
      If dr0 = 0 Then g0 = -g0
      y0 = wsg(n): GoSub pcb1
      p4 = p2: q4 = q2
      y0 = y0 + wwd(n): GoSub pcb1
      Line (p4, q4)-(p2, q2)
    ElseIf dr0 < 4 Then
      If dr0 = 2 Then g0 = -g0
      x0 = wsg(n): GoSub pcb2
      p4 = p2: q4 = q2
      x0 = x0 + wwd(n): GoSub pcb2
      Line (p4, q4)-(p2, q2)
    End If: End If
  Next
'ebooster
  For n = 1 To neb
    ForeColor = RGB(180, 0, 0)
    If ebty(n) = 3 Then i1 = 3 Else i1 = 2
    For m = 1 To i1
    If n = ieb And InStr(ttl, "boo") Then
      If m = jeb Then ForeColor = vbMagenta Else _
      ForeColor = vbBlue
    End If
    dr0 = ebdr(n, m)
    If dr0 = 0 Or dr0 = 1 Then
      y0 = ebsg(n, m): z0 = ebbg(n, m): g0 = ebht(n, m)
      If dr0 = 0 Then x0 = 0 Else x0 = lth: g0 = -g0
      Call proj2d: p2 = p1: q2 = q1
      x0 = x0 + g0: Call proj2d: Line (p1, q1)-(p2, q2)
    ElseIf dr0 = 2 Or dr0 = 3 Then
      x0 = ebsg(n, m): z0 = ebbg(n, m): g0 = ebht(n, m)
      If dr0 = 2 Then y0 = 0 Else y0 = wdt: g0 = -g0
      Call proj2d: p2 = p1: q2 = q1
      y0 = y0 + g0: Call proj2d: Line (p1, q1)-(p2, q2)
    ElseIf dr0 = 4 Or dr0 = 5 Then
      x0 = ebsg(n, m): y0 = ebbg(n, m): g0 = ebht(n, m)
      If dr0 = 4 Then z0 = 0 Else z0 = hgt: g0 = -g0
      Call proj2d: p2 = p1: q2 = q1
      z0 = z0 + g0: Call proj2d: Line (p1, q1)-(p2, q2)
    End If
  Next: Next
'wall
  If InStr(ttl, "wall") > 0 And iwal > 1 Then
    n = iwal
    dr0 = wa_dr(n): ForeColor = vbBlue
    sg0 = wa_sg(n): bg0 = wa_bg(n)
    ht0 = wa_ht(n): wd0 = wa_wd(n): GoSub pcb3
  End If
  If chamber_type = 2 Then GoSub pcb4
  CurrentX = LbO.Left: CurrentY = LbO.Top + 250
  ForeColor = vbBlack: Font.Size = 12
  Exit Sub

pcb0:
  Line (p1, q1)-(p2, q2): Line (p2, q2)-(p3, q3)
  DrawStyle = 2: Line (p1, q1)-(p3, q3)
  DrawStyle = 0
  Return

pcb1:
  If dr0 = 0 Then x0 = 0 Else x0 = lth
  x0 = x0 + g0: z0 = 0: Call proj2d
  p2 = p1: q2 = q1
  x0 = x0 - g0: z0 = hgt: Call proj2d
  p3 = p1: q3 = q1
  z0 = 0: Call proj2d: GoSub pcb0
  Return

pcb2:
  If dr0 = 2 Then y0 = 0 Else y0 = wdt
  y0 = y0 + g0: z0 = 0: Call proj2d
  p2 = p1: q2 = q1
  y0 = y0 - g0: z0 = hgt: Call proj2d
  p3 = p1: q3 = q1
  z0 = 0: Call proj2d: GoSub pcb0
  Return

pcb3:
  Call projw: p2 = p1: q2 = q1
  bg0 = bg0 + ht0: Call projw
  Line (p2, q2)-(p1, q1)
  sg0 = sg0 + wd0: Call projw
  Line -(p1, q1)
  bg0 = bg0 - ht0: Call projw
  Line -(p1, q1)
  sg0 = sg0 - wd0: Call projw
  Line -(p1, q1)
  Return

pcb4:
  zf1 = 6000 / (gx_r(3) + lth_r(3))
  zf2 = zf1: zf3 = zf1 * zf3a / zf2a
  dq0 = LbO.Top - LbP.Top + 200: p1 = p0: q1 = q0
  n = 1: GoSub pcb4a
  x0 = gx_r(2): y0 = gy_r(2): z0 = gz_r(2): Call proj2d
  n = 2: GoSub pcb4a
  x0 = gx_r(3): y0 = gy_r(3): z0 = gz_r(3): Call proj2d
  n = 3: GoSub pcb4a
  ForeColor = vbBlack: zf1 = zf1a: zf2 = zf2a: zf3 = zf3a
  n = melter_component: lth = lth_r(n): wdt = wdt_r(n): hgt = hgt_r(n)
  Return

pcb4a:
  If melter_component = n Then ForeColor = vbBlue _
    Else ForeColor = RGB(150, 150, 150)
  dp = p1 - p0: dq = (q0 - q1) + dq0
  lth = lth_r(n): wdt = wdt_r(n): hgt = hgt_r(n)
  x0 = lth: y0 = 0: z0 = 0: Call proj2d
  p2 = p0 + dp: q2 = q0 - dq: GoSub pcb4a1
  y0 = wdt: Call proj2d: GoSub pcb4a1
  z0 = hgt: Call proj2d: GoSub pcb4a1
  y0 = 0: Call proj2d: GoSub pcb4a1
  x0 = 0: Call proj2d: GoSub pcb4a1
  y0 = wdt: Call proj2d: GoSub pcb4a1
  x0 = lth: Call proj2d: GoSub pcb4a1
  y0 = 0: Call proj2d: p2 = p1 + dp: q2 = q1 - dq
  z0 = 0: Call proj2d: GoSub pcb4a1
  x0 = 0: z0 = hgt: Call proj2d: p2 = p1 + dp: q2 = q1 - dq
  z0 = 0: Call proj2d: GoSub pcb4a1
  DrawStyle = 2
  y0 = wdt: Call proj2d: GoSub pcb4a1
  x0 = lth: Call proj2d: GoSub pcb4a1
  x0 = 0: Call proj2d: p2 = p1 + dp: q2 = q1 - dq
  z0 = hgt: Call proj2d: GoSub pcb4a1
  DrawStyle = 0
  Return

pcb4a1:
  p1 = p1 + dp: q1 = q1 - dq
  Line (p2, q2)-(p1, q1): p2 = p1: q2 = q1
  Return
End Sub


'-------------------------------
'Open Pre-Process file and grid file if it exists
'Input fnm_pre_path = pre file path and name
'Input fnm_gd_path = grid file path and name
'-------------------------------
Private Sub openPreFile(n0 As Integer)
  'If n0 = 1 Then read prefile and grid file, set status/menus, and display furnace
  'If n0 = 2 Then doing call during cycling and need to change one or more variables.
  '          Only read the prefile.
Dim temp_str As String

  'Read in Pre-Process file.
  On Error GoTo f210
  Open fnm_pre_path For Input As #1
  Input #1, p0, q0, s0 'plot origin
  Input #1, zf1, zf2, zf3, s0 'scale factors
  Input #1, g0, s0 'plot angle
  Input #1, unt, vv0 'unit type, vector length
  If unt = "SI" Then
    opt11.Checked = False: opt12.Checked = True
  Else 'English units
    opt11.Checked = True: opt12.Checked = False
  End If
  theta = g0 / 180 * pi
  If flow_domain = 1 Then
    'get more combustion variables
    GoSub f21a: neb = 0
  Else
    'get more melt variables
    GoSub f21b
  End If
  
  'finished reading in prefile
  If n0 = 2 Then
    Close (1)
    Exit Sub ' Only needed to read in the prefile
  End If
  
  active_view = vw_fig    ' 8-12-2004
  flag_preproc_open = True
    
  If FileExist(fnm_gd_path) = False Then
    'grid file does not exist
      'turn off grid protect mode
      mnuProtectGrid.Checked = False
      mnuProtectGrid.Enabled = True
      mnuProtectGrid.Visible = True
      gridProtectMode = False
      ctgeo.Enabled = True
      ctgd.Enabled = True
      opt4.Enabled = True 'enable density changes
      ctex.Enabled = True
      If flow_domain = 1 Then
         ctsw.Enabled = True
      End If
      'end of turn off grid protect mode
     mnuView.Visible = False ' 8-12-2004
  Else
     Call read_grid
     mnuView.Visible = True
     mnuFullGrid.Visible = True
     If grid_type = Is_enhanced Then
        'turn on grid protect mode
        mnuProtectGrid.Checked = True
        mnuProtectGrid.Enabled = True
        mnuProtectGrid.Visible = True
        gridProtectMode = True
        ctgeo.Enabled = False
        ctgd.Enabled = False
        opt4.Enabled = False 'disable density changes
        ctex.Enabled = False
        If flow_domain = 1 Then
           ctsw.Enabled = False
        End If
        If menu_layer <> InGridConstruction Then
           Call ct_ls(0, 0, 0, 0) 'clear out the green list boxes if they exist
        End If
        'end of turn on grid protect mode
     Else
        'turn off grid protect mode
        mnuProtectGrid.Checked = False
        mnuProtectGrid.Enabled = True
        mnuProtectGrid.Visible = True
        gridProtectMode = False
        ctgeo.Enabled = True
        ctgd.Enabled = True
        opt4.Enabled = True 'enable density changes
        ctex.Enabled = True
        If flow_domain = 1 Then
           ctsw.Enabled = True
        End If
        'end of turn off grid protect mode
     End If
     flag_grid_constructed = False
     flag_grid_modified = False
     flag_grid_saved = False
     flag_grid_active = True
     flag_grid_enhanced = False
  End If
  
f210: 'Fall thru lable, come here for read errors also
  Close (1)
  If n0 = 2 Then
    'an error has occurred reading the prefile during cycling
    'not sure what to do here                                         @@@
    Exit Sub
  End If
  Call file10(1) 'Draw furnace on screen
  'If grid_type = Is_enhanced And flag_save_as = False Then
  '  If MsgBox(Chr(10) & Chr(10) & "IF YOU RECONSTRUCT YOUR GRID, YOU WILL LOOSE GRID EDITS. " _
  '      & Chr(10) & Chr(10) _
  '      & "Note that the newly opened case has an enhanced grid and the " _
  '      & "Option->Protect-Grid-Edits menu item has been automatically checked." _
  '      & Chr(10) & Chr(10) _
  '      & "An enhanced grid edit " _
  '      & "consists of changing a grid node type or adding, deleting, or moving grid " _
  '      & "lines.  These edits are destroyed when the grid is reconstucted, so changes " _
  '      & "that would require grid reconstruction are disabled when the " _
  '      & "Option->Protect-Grid-Edits menu item is checked." _
  '      & Chr(10) & Chr(10) _
  '      & "Be aware that " _
  '      & "unchecking this option will allow you to destroy your enhanced grid edits.", _
  '      vbOKOnly + vbExclamation, "GFM Warning about Potential Loss of User Work") = vbOK _
  '      Then
  '          flag_grid_enhanced = True 'first time enhanced grid warning will not be repeated
  '          Exit Sub
  '  End If
  'End If
 Exit Sub

f21a: 'sub for combustion
  Input #1, lth, wdt, hgt, hgt_c, s0
  Input #1, thk(0), thk(1), thk(2), thk(3), thk(4), _
    thk(5), s0
  GoSub fob_wall
  'do burners
  Input #1, nbr, ibr, s0
  GoSub f21a1 'set burner variable dimensions
  For i = 1 To nbr
    Input #1, i
    Input #1, bty(i), bdr(i), s0
    Input #1, bsg(i, 1), bbg(i, 1), s0
    Input #1, bwd(i, 1), bht(i, 1), s0
    Input #1, btg(i, 1), bas(i), s0
    Input #1, bav(i, 1), bah(i, 1), s0
    Input #1, bfg(i, 1), bfa(i, 1), s0
    Input #1, bfo(i, 1), bfn(i, 1), s0
    Input #1, bfg(i, 2), bfa(i, 2), s0
    Input #1, bfo(i, 2), bfn(i, 2), s0
    If bty(i) = 4 Then 'have regenerative burner
      Input #1, btg(i, 2), s0
      Input #1, bav(i, 2), bah(i, 2), s0
      Input #1, bsg(i, 2), bbg(i, 2), s0
      Input #1, bwd(i, 2), bht(i, 2), s0
    End If
  Next
  'End burners
  
f21a0:  'this section used for combustion and melt
  'do exhausts
  Input #1, nex, iex, s0 'number of exhausts (outlets in melter), current number
  ReDim ety(nex), edr(nex), esg(nex), ebg(nex)
  ReDim ewd(nex), eht(nex)
  If flow_domain = 1 Then
    ReDim exh_wall_temp_type(nex), exh_wall_temp_fixed(nex), exh_wall_temp_frac(nex)
  End If
  For i = 1 To nex
    Input #1, i
    Input #1, ety(i), edr(i), s0 'type, orientation
    Input #1, esg(i), ebg(i), s0 'gaps
    Input #1, ewd(i), eht(i), s0 'width, height
    If flow_domain = 1 Then
        Input #1, exh_wall_temp_type(i), s0 'Choose from next 2 items, 0=> fraction type, 1=> fixed type
        Input #1, exh_wall_temp_fixed(i), s0 'fixed initial exhaust wall temperature for radiation
        Input #1, exh_wall_temp_frac(i), s0 'fraction of average wall temperature for exhaust wall
    End If
  Next
  'End exhausts
  Input #1, irstyp, maxgi, s0 'restart indicator, number of iterations
  Input #1, bgcon, id_rad, s0 'convergence criteria, identifier for radiation
  Input #1, pg0, qh0, s0
  'Do doghouses
  Input #1, nsw, isw, s0 'number of sidewells (bubblers in melter), current number
  ReDim wdr(nsw), wsg(nsw), wbg(nsw), wwd(nsw), wdp(nsw)
  ReDim wtg(nsw), wfa(nsw)
  For i = 1 To nsw
    Input #1, i
    Input #1, wdr(i), s0
    If flow_domain = 1 Then
      Input #1, wsg(i), s0
    Else
      Input #1, wsg(i), wbg(i), s0
      Input #1, wtg(i), wfa(i), s0
    End If
    Input #1, wwd(i), wdp(i), s0
  Next
  'End doghouses
  Input #1, dx0, dy0, dz0, s0 'maximum sizes for grid cells (delta values)
  Input #1, mp, np, lp, s0 'maximum grid cell indexes in x,y,z directions
  
  If flow_domain = 1 Then
        'Note that these items were added since the last released version of code.
        'If old versions of prefiles are being used, then there will be errors.
        On Error Resume Next
        Input #1, interval_rad, s0 'iterations between global radiation calculations'
        If interval_rad = 0 Or IsNumeric(interval_rad) = False Then
            interval_rad = default_interval_rad
        End If
        Input #1, surf_type, surf_temp, s0 'surface type 0=>fixed, then use temperature, else melt calculates'
        If IsNumeric(surf_type) = False Then
            surf_type = default_surf_type
        End If
        If surf_temp = 0 Or IsNumeric(surf_temp) = False Then
            surf_temp = default_surf_temp
        End If
        Input #1, nwl, iwl, s0 'number of radiation wavelengths
        If nwl = 0 Or IsNumeric(nwl) = False Then
            'wavelength defaults are hard coded
            nwl = 10: iwl = 1
            ReDim wl(nwl)
            For i = 1 To 7: wl(i) = i: Next
            wl(8) = 8.5: wl(9) = 12: wl(10) = 15
        Else
            ReDim wl(nwl)
            For i = 1 To nwl
                Input #1, wl(i)
            Next
        End If
        Input #1, start_temp, s0 'temperature in domain at new start
        If start_temp = 0 Or IsNumeric(start_temp) = False Then
            start_temp = default_start_temp_c
        End If
        Input #1, oxy_fuel, s0 'oxy fuel for soot option
        If oxy_fuel = 0 Or IsNumeric(oxy_fuel) = False Then
            oxy_fuel = 0 'default
        End If
        Input #1, initial_gitr, s0 'iterations on new start before the radiation starts
        If IsNumeric(initial_gitr) = False Then
            initial_gitr = default_initial_gitr
        End If
        Input #1, esf, s0 'activation energy for soot formation J/kmol
        If esf = 0 Or IsNumeric(esf) = False Then
            esf = default_esf
        End If
        Input #1, aform, s0 'kinetic constant soot formation
        If IsNumeric(aform) = False Then
            aform = default_aform
        End If
        Input #1, eso, s0 'activation energy for soot oxidation J/kmol
        If eso = 0 Or IsNumeric(eso) = False Then
            eso = default_eso
        End If
        Input #1, aoxid, s0 'kinetic constant soot oxidation
        If IsNumeric(aoxid) = False Then
            aoxid = default_aoxid
        End If
        Input #1, isoot_cal, s0 'soot kinetics calibration mode
        If IsNumeric(isoot_cal) = False Then
            isoot_cal = default_isoot_cal
        End If
        If isoot_cal = 0 Then
            mnuPropSootCali.Checked = False
        Else
            mnuPropSootCali.Checked = True
        End If
        'Input #1, q_melt_req, s0 'energy required for melt
        'If q_melt_req = 0 Or IsNumeric(q_melt_req) = False Then
        '    q_melt_req = default_q_melt_req
        'End If
        Input #1, avail_dub0, s0 'available double parameter position
        Input #1, maxri2, s0 'maximum radiosity solver inner loop 2 iteration number
        If maxri2 = 0 Or IsNumeric(maxri2) = False Then
            maxri2 = default_maxri2
        End If
        Input #1, maxri1, s0 'maximum radiosity solver outer loop 1 iteration number
        If maxri1 = 0 Or IsNumeric(maxri1) = False Then
            maxri1 = default_maxri1
        End If
        Input #1, minri2, s0 'minimum radiosity solver inner loop 2 cycles (3-20)
        If minri2 = 0 Or IsNumeric(minri2) = False Then
            minri2 = default_minri2
        End If
        Input #1, preset_vf, s0 'radiosity view factor option, 1=>calc once and save, 2=>calc as needed
        If preset_vf = 0 Or IsNumeric(preset_vf) = False Then
            preset_vf = default_preset_vf
        End If
        If preset_vf = 1 Then
            mnuPropCFDvf.Checked = True
        Else
            mnuPropCFDvf.Checked = False
        End If
        Input #1, eps_m, s0 'emissivity of melt surface
        If eps_m = 0 Or IsNumeric(eps_m) = False Then
            eps_m = default_eps_m
        End If
        Input #1, eps_c, s0 'emissivity of ceiling or crown
        If eps_c = 0 Or IsNumeric(eps_c) = False Then
            eps_c = default_eps_c
        End If
        Input #1, maxms, s0 'maximum minor species iterations per global iteration
        If IsNumeric(maxms) = False Then
            maxms = default_maxms
        End If
        Input #1, ms, s0 '1=> minor species calculation required
        If IsNumeric(ms) = False Then
            ms = default_ms
        End If
        Input #1, gui_update, s0 '1=> CFD will provide status updates via a gfm.dat file
        If IsNumeric(gui_update) = False Then
            gui_update = default_gui_update
        End If
        If gui_update = 1 Then
            MnuOptGuiOn.Checked = True
            MnuOptGuiOff.Checked = False
        Else
            MnuOptGuiOn.Checked = False
            MnuOptGuiOff.Checked = True
        End If
        Input #1, n_cells_across_exit, s0 'number of cells in cross section of exit (both directions)
        If IsNumeric(n_cells_across_exit) = False Then
            n_cells_across_exit = default_n_cells_across_exit
        End If
        Input #1, feed_unit, s0 'feed unit type (mass or volume)
        If feed_unit = "" Then
            feed_unit = default_feed_unit
        End If
        If feed_unit = "kg/s" Then
            MnuOptFedMas.Checked = True
            MnuOptFedVol.Checked = False
        Else
            MnuOptFedMas.Checked = False
            MnuOptFedVol.Checked = True
        End If
        
        'Combustion data collection control flags may be set to 0 or 1 (default)
        Input #1, isum, s0      'Summary data collection control flag
        If IsNumeric(isum) = False Then
            isum = default_isum
        End If
        Input #1, iinfo, s0    'General Information data collection control flag
        If IsNumeric(iinfo) = False Then
            iinfo = default_iinfo
        End If
        Input #1, iTave, s0     'Average Temperatures data collection control flag
        If IsNumeric(iTave) = False Then
            iTave = default_iTave
        End If
        Input #1, iconv, s0     'Mass Residual Convergence data collection control flag
        If IsNumeric(iconv) = False Then
            iconv = default_iconv
        End If
        Input #1, igresid, s0   'Equation Residuals data collection control flag
        If IsNumeric(igresid) = False Then
            igresid = default_igresid
        End If
        Input #1, igresidp, s0   'Pre-Solve Equation Residuals data collection control flag
        If IsNumeric(igresidp) = False Then
            igresidp = default_igresidp
        End If
        Input #1, igresidx, s0   ' Extra Equation Residuals data collection control flag
        If IsNumeric(igresidx) = False Then
            igresidx = default_igresidx
        End If
        Input #1, igresidxp, s0  'Extra Pre-Solve Equation Residuals data collection control flag
        If IsNumeric(igresidxp) = False Then
            igresidxp = default_igresidxp
        End If
        Input #1, imresid, s0   'Minor Species Residual Convergence data collection control flag
        If IsNumeric(imresid) = False Then
            imresid = default_imresid
        End If
        Input #1, irad_detail, s0       'Radiation Details data collection control flag
        If IsNumeric(irad_detail) = False Then
            irad_detail = default_irad_detail
        End If
        Input #1, irad_rad, s0       'Radiosity Convergence data collection control flag
        If IsNumeric(isum) = False Then
            irad_rad = default_irad_rad
        End If
        Input #1, itwal, s0       'Wall Temperature data collection control flag
        If IsNumeric(itwal) = False Then
            itwal = default_itwal
        End If
        Input #1, irelax, s0       'Temperature Relaxation data collection control flag
        If IsNumeric(irelax) = False Then
            irelax = default_irelax
        End If
        Input #1, iflx, s0       'Melt Surface Flux Change data collection control flag
        If IsNumeric(iflx) = False Then
            iflx = default_iflx
        End If
        Input #1, ifieldview, s0       'Melt Surface Flux Change data collection control flag
        If IsNumeric(ifieldview) = False Then
            ifieldview = default_ifieldview
        End If
  End If
  Return

'-----------------------
f21a1:
  'set dimensions for burner variables
  ReDim bty(nbr), bdr(nbr), bsg(nbr, 2), bbg(nbr, 2)
  ReDim bwd(nbr, 2), bht(nbr, 2), btg(nbr, 2)
  ReDim bav(nbr, 2), bah(nbr, 2), bas(nbr)
  ReDim bfg(nbr, 2), bfa(nbr, 2), bfo(nbr, 2), bfn(nbr, 2)
  ReDim batch_velocity(nbr) '(only used for melt, does not matter to define for comb)
  Return

'-----------------------
f21b: 'sub for melt
  Input #1, chamber_type, melter_component, s0
  melter_component = 1
  If chamber_type = 2 Then
    Input #1, lth_r(1), wdt_r(1), hgt_r(1), s0
    Input #1, lth_r(2), wdt_r(2), hgt_r(2), s0
    Input #1, gx_r(2), gy_r(2), gz_r(2), s0
    Input #1, lth_r(3), wdt_r(3), hgt_r(3), s0
    Input #1, gx_r(3), gy_r(3), gz_r(3), s0
  Else
    Input #1, lth, wdt, hgt, s0
  End If
  Input #1, thk(0), thk(1), thk(2), thk(3), thk(4), _
    thk(5), s0 'wall thickness
  Input #1, nbr, ibr, s0 'number of chargers, current charger index
  GoSub f21a1   'set dimensions for burner variables
  For i = 1 To nbr
    Input #1, i
    Input #1, bty(i), bdr(i), s0 'type, orientation
    Input #1, bsg(i, 1), bbg(i, 1), s0 'gaps
    Input #1, bwd(i, 1), bht(i, 1), s0 'width, height
    Input #1, btg(i, 1), s0 'temperature
    Input #1, bfg(i, 1), bfg(i, 2), s0 'charge rate, cullet ratio
    Input #1, bav(i, 1), bah(i, 1), s0 ' sand/cullet size
    Input #1, batch_velocity(i), s0 'batch velocity
    'do not input stuff for new regenerative burner when doing melt
  Next
  'Input #1, cl_s, h0_s, s0 'sand specific heat, fussion
  'Input #1, cl_c, h0_c, s0 'cullet specific heat, fussion
  Input #1, h0_s, s0 'sand fussion
  Input #1, h0_c, s0 'cullet fussion
  GoSub f21a0 'do outlets, bubblers, and a few more items
  ReDim epull(nex)
  For i = 1 To nex: Input #1, epull(i): Next 'specify pull rates
  Input #1, s0
  Input #1, neb, ieb, jeb, s0 'number of electric boosters, current number, index
  ReDim ebty(neb), ebvt(neb), ebpw(neb), ebhl(neb)
  ReDim ebsg(neb, 3), ebbg(neb, 3), ebwd(neb, 3)
  ReDim ebht(neb, 3), ebdr(neb, 3)
  For i = 1 To neb
    Input #1, ebty(i), ebvt(i), ebpw(i), ebhl(i), s0
    For j = 1 To 3
      Input #1, ebsg(i, j), ebbg(i, j), ebwd(i, j), _
      ebht(i, j), ebdr(i, j), s0
  Next: Next
  Input #1, nphas, s0 'number of phases
  Input #1, nps0_s, ips0_s, s0 'number of sand groups, current
  Input #1, tm_s, ds_s, s0 'melt temperature, density
  For i = 1 To nps0_s: Input #1, bav(i, 1): Next 'radii
  Input #1, s0
  Input #1, nps0_c, ips0_c, s0 'number of cullet groups, current
  Input #1, tm_c, ds_c, s0 'melt temperature, density
  For i = 1 To nps0_c: Input #1, bah(i, 1): Next 'radii
  Input #1, s0
  Input #1, pg0, s0 'pressure
  Input #1, heat_flux_type, qsh, s0 'surface heat flux type, specified value
  GoSub fob_wall 'do wall properties
  
  Input #1, nudf, mudf, iudf, judf, s0 'glass flow property indexes
  Input #1, n, s0 'number of density function values
  ReDim udf_ds(n), udf_ds_t(n)
  If nudf < 1 Then nudf = 1
  If nudf = 1 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_ds(i), udf_ds_t(i), s0
    If nudf = 1 Then fx(i) = udf_ds(i): tx(i) = udf_ds_t(i)
  Next: udf_ds_n = n
  Input #1, n, s0 'number of viscosity function values
  ReDim udf_mu(n), udf_mu_t(n)
  If nudf = 2 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_mu(i), udf_mu_t(i), s0
    If nudf = 2 Then fx(i) = udf_mu(i): tx(i) = udf_mu_t(i)
  Next: udf_mu_n = n
  Input #1, n, s0 'number of conductivity function values
  ReDim udf_k(n), udf_k_t(n)
  If nudf = 3 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_k(i), udf_k_t(i), s0
    If nudf = 3 Then fx(i) = udf_k(i): tx(i) = udf_k_t(i)
  Next: udf_k_n = n
  
  Input #1, n, s0 'number of liquid specific heat function values
  ReDim udf_cl(n), udf_cl_t(n)
  If nudf = 4 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_cl(i), udf_cl_t(i), s0
    If nudf = 4 Then fx(i) = udf_cl(i): tx(i) = udf_cl_t(i)
  Next: udf_cl_n = n
  
  Input #1, n, s0 'number of volume absorption function values
  ReDim udf_a_m(n), udf_a_m_t(n)
  If nudf = 5 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_a_m(i), udf_a_m_t(i), s0
    If nudf = 5 Then fx(i) = udf_a_m(i): tx(i) = udf_a_m_t(i)
  Next: udf_a_n = n
  
  Input #1, n, s0 'number of cullet specific heat function values
  ReDim udf_clc(n), udf_clc_t(n)
  If nudf = 6 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_clc(i), udf_clc_t(i), s0
    If nudf = 6 Then fx(i) = udf_clc(i): tx(i) = udf_clc_t(i)
  Next: udf_clc_n = n
  Input #1, n, s0 'number of sand specific heat function values
  ReDim udf_cls(n), udf_cls_t(n)
  If nudf = 7 Then ReDim fx(n), tx(n)
  For i = 1 To n
    Input #1, udf_cls(i), udf_cls_t(i), s0
    If nudf = 7 Then fx(i) = udf_cls(i): tx(i) = udf_cls_t(i)
  Next: udf_cls_n = n
  
  'Note that these items were added since the last released version of code.
  'Note in past version last line was grid path.
  'If old versions of prefiles are being used, then there will be errors.
  On Error Resume Next
  Input #1, glassExitTemp, s0 'temperature of glass at exit
  If glassExitTemp = 0 Or IsNumeric(glassExitTemp) = False Then
        glassExitTemp = default_glassExitTemp
  End If
  Input #1, condLength_s, condLength_c, s0 'conductivity thickness (length) for sand and cullet
  If condLength_s = 0 Or IsNumeric(condLength_s) = False Then
        condLength_s = default_condLength_s
  End If
  If condLength_c = 0 Or IsNumeric(condLength_c) = False Then
        condLength_c = default_condLength_c
  End If
  Input #1, start_temp, s0 'temperature in domain at new start'
  If start_temp = 0 Or IsNumeric(start_temp) = False Then
        start_temp = default_start_temp_m
  End If
  Input #1, gui_update, s0 '1=> CFD will provide status updates via a gfm.dat file
  If IsNumeric(gui_update) = False Then
        gui_update = default_gui_update
  End If
  If gui_update = 1 Then
    MnuOptGuiOn.Checked = True
    MnuOptGuiOff.Checked = False
  Else
    MnuOptGuiOn.Checked = False
    MnuOptGuiOff.Checked = True
  End If
  Input #1, extra_tunnel_cells, s0 'number of cells in the length of an exit tunnel beyond the initial wall
  If IsNumeric(extra_tunnel_cells) = False Then
    extra_tunnel_cells = default_extra_tunnel_cells
  End If
  Input #1, extra_tunnel_length, s0 'length of an exit tunnel beyond the initial wall
  If IsNumeric(extra_tunnel_length) = False Then
    extra_tunnel_length = default_extra_tunnel_length
  End If
  Input #1, n_cells_across_exit, s0 'number of cells in cross section of exit (both directions)
  If IsNumeric(n_cells_across_exit) = False Then
    n_cells_across_exit = default_n_cells_across_exit
  End If
  
    'Melt data collection control flags may be set to 0 or 1 (default)
    Input #1, isum_m, s0      'Summary data collection control flag
    If IsNumeric(isum_m) = False Then
        isum_m = default_isum_m
    End If
    Input #1, iinfo_m, s0     'General Information data collection control flag
    If IsNumeric(iinfo) = False Then
        iinfo_m = default_iinfo_m
    End If
    Input #1, iTave_m, s0     'Average Temperatures data collection control flag
    If IsNumeric(iTave_m) = False Then
        iTave_m = default_iTave_m
    End If
    Input #1, iconv_m, s0     'Mass Residual Convergence data collection control flag
    If IsNumeric(iconv_m) = False Then
        iconv_m = default_iconv_m
    End If
    Input #1, igresid_m, s0   'Equation Residuals data collection control flag
    If IsNumeric(igresid_m) = False Then
        igresid_m = default_igresid_m
    End If
    Input #1, igresidp_m, s0  'Pre-Solve Equation Residuals data collection control flag
    If IsNumeric(igresidp_m) = False Then
        igresidp_m = default_igresidp_m
    End If
    Input #1, iTchg, s0       'Melt Surface Temperature Change data collection control flag
    If IsNumeric(iTchg) = False Then
        iTchg = default_iTchg
    End If
    Input #1, iadjf, s0       'Melt Surface Adjusted Flux data collection control flag
    If IsNumeric(iadjf) = False Then
        iadjf = default_iadjf
    End If
    Input #1, iadjr, s0       'Melt Surface Flux Relaxation data collection control flag
    If IsNumeric(iadjr) = False Then
        iadjr = default_iadjr
    End If
    Input #1, ifieldview_m, s0       'Output for FieldView data collection control flag
    If IsNumeric(ifieldview_m) = False Then
        ifieldview_m = default_ifieldview_m
    End If
      
  Return
  
'---------------------
fob_wall:
  Input #1, iwal, nwal, s0 'number of wall, current
  ReDim wa_d(nwal), wa_k(nwal), wa_h(nwal), wa_ta(nwal)
  ReDim wa_sg(nwal), wa_bg(nwal), wa_wd(nwal), wa_ht(nwal)
  ReDim wa_dr(nwal), wa_e(nwal)
  For i = 1 To nwal
    Input #1, i, wa_dr(i), s0 'index, direction
    Input #1, wa_sg(i), wa_bg(i), wa_wd(i), wa_ht(i), s0 'geometry
    Input #1, wa_d(i), wa_k(i), wa_h(i), wa_ta(i), wa_e(i), s0 'properties
  Next
  Return
End Sub

'-------------------------------
'Open and read a grid file
'-------------------------------
Private Sub openGridFile()
  If menu_layer = InPreProc Or menu_layer = InGridConstruction Then
      Call ct_ls(0, 0, 0, 0) 'Clear out the green lists if they exist
      mnuView.Visible = True
  End If
   
  opt4.Visible = True
  If menu_layer = InPreProc Then
        menu_layer = InGridConstruction
        mnuProtectGrid.Visible = True
        flag_grid_constructed = False
        flag_grid_modified = False
        flag_grid_saved = False
  Else 'must be in post processing
        mnuProtectGrid.Visible = False
        MnuOptCol.Visible = False
  End If
  mnuFullGrid.Visible = True
  
  Call read_grid
  
  x1a = xg(mp) - xg(1): y1a = yg(np) - yg(1)
  z1a = zg(lp) - zg(1)
  If hgt_c > 0 Then g2 = 1 Else g2 = zf3 / zf2
  g0 = 10000 / (x1a + z1a * g2): g1 = 5000 / (y1a + z1a * g2)
  zf1 = g0: zf2 = g1: zf3 = g1 * g2
  unt = "SI": opt11.Checked = False: opt12.Checked = True
  Lbadd.Caption = "add": Lbdel.Caption = "del"
  Lbmv.Caption = "move": Lbsw.Caption = "sweep"
  active_view = vw_grid
    Call setup(2)
    Call plt_gd(0)
End Sub

'-------------------------------
'Open grid and output files
'Create a tmp file for each output variable
'Display the first output variable
'-------------------------------
Private Sub openOutFile()
  Dim s2 As String
  Dim iu As Integer, ud0 As Double
  
  Call openGridFile
  
  frmMain.Caption = "Post-Processor:"
  ReDim fp(nx, ny, nz), ug(nx, ny, nz, 3)
  
  Open fnm_rt_path For Input As #10 '#10 = simulation output file
  List1.Clear
  On Error GoTo f23z
  Do While Not EOF(10)
    Line Input #10, s0
    n1 = InStr(s0, """")
    If n1 > 0 Then GoSub f23a
  Loop
  
f23z:
  Close (10)
  LbCase.Caption = " Case " & case_number
  LbCase.Visible = True
  LbCaseTitle.Caption = case_title
  LbCaseTitle.Visible = True
  mnuCaseDescript.Enabled = True
  mnuCaseDescript.Visible = True
  fpn = "" 'initialize name of last output variable
  List1.ListIndex = 0 'prepare to display first item in output list
  'Call List1_Click
        'Developer Note: ???
        'Setting ListIndex to 0 causes a jump into the List1_Click routine.
        'So commented out the real call.  Otherwise user sees flash of 2 repeated displays.
  Exit Sub
 
f23a:
  s0 = LCase$(s0)
  If InStr(s0, "result") > 0 Then Return
  If InStr(s0, "vel") > 0 Then GoTo f23d
  s1 = Mid(s0, n1 + 1)
  n2 = InStr(s1, ",")
  If n2 < 1 Then n2 = InStr(s1, " ")
  'If n2 > 9 Then n2 = 9
  If n2 > 20 Then n2 = 20 'arbitrary cut off of output name, check if works for all fields
  s1 = Left(s1, n2 - 1)
  List1.AddItem s1
  s2 = dnm & "tmp\" & s1 & ".dat"
  Open s2 For Output As #1
  Print #1, s1
  s2 = ""
  If InStr(s0, "surf") > 0 Then
    GoSub f23e
  Else
    If InStr(s1, "pres") > 0 Then s2 = "Pa"
    If InStr(s1, "tem") > 0 Then s2 = "K"
    If InStr(s1, "emis") > 0 Then s2 = "W/m^3"
    Print #1, s2
    GoSub f23b
  End If
  n = 0
  For i = 1 To nx: For j = 1 To ny: For k = 1 To nz
    g0 = fp(i, j, k)
    Print #1, g0
    GoSub f23a1
  Next: Next: Next
  If fp_mx = fp_mn Then
    If fp_mx < 0 Then
      fp_mx = 0
    ElseIf fp_mx > 0 Then
      fp_mn = 0
    Else
      fp_mx = 1: fp_mn = -1
    End If
  End If
  Print #1, fp_mx
  Print #1, fp_mn
  Close (1)
  Return

f23a1:
  If ibc(i, j, k) = 1 Then Return
  If n = 0 Then
    fp_mx = g0
    fp_mn = fp_mx: n = 1
  Else
    If g0 > fp_mx Then fp_mx = g0
    If g0 < fp_mn Then fp_mn = g0
  End If
  Return

f23b: 'input line has "pres", "tem", or "emis" in it
  n2 = InStr(s0, "=")
  If n2 <= 0 Then
    g0 = 1
  Else
    s2 = Mid(s0, n2 + 1): g0 = Val(s2)
  End If
  For k = 2 To nz - 1: j1 = 1
  Do While j1 <= ny
    j2 = j1 + 10
    If j2 > ny Then j2 = ny
    GoSub f23c
    For i = 1 To nx
    Input #10, g1
    For j = j1 To j2
      Input #10, g1
      fp(i, j, k) = g1 * g0
    Next: Next
    j1 = j2 + 1
  Loop
  Next
  For i = 1 To nx: For j = 1 To ny
    fp(i, j, 1) = fp(i, j, 2)
    fp(i, j, nz) = fp(i, j, nz - 1)
  Next: Next
  Return

f23c:
  Do While Not EOF(10)
    Line Input #10, s0
    If InStr(s0, "X  /  R") > 0 Then Return
    If InStr(s0, "X  /  Y") > 0 Then Return
  Loop
  Return

f23d: 'input line has "vel" in it
  iu = iu + 1
  s1 = "vel"
  If iu = 1 Then
    List1.AddItem s1
    s2 = dnm & "tmp\" & s1 & ".dat"
    Open s2 For Output As #1
    Print #1, s1
    Print #1, "m/s"
  End If
  GoSub f23b
  n = 0
  For i = 1 To nx: For j = 1 To ny: For k = 1 To nz
    If iu = 1 Then ug(i, j, k, 0) = 0
    g0 = fp(i, j, k)
    ug(i, j, k, iu) = g0
    ug(i, j, k, 0) = ug(i, j, k, 0) + g0 * g0
    If iu = 3 Then
      ud0 = ug(i, j, k, 0)
      If ud0 < 1E-20 Then
        ud0 = 0
      Else
        ud0 = Sqr(ud0)
      End If
      Print #1, ud0
      ug(i, j, k, 0) = ud0
      g0 = ud0: GoSub f23a1
    End If
  Next: Next: Next
  If iu < 3 Then Return
  Print #1, fp_mx
  Print #1, fp_mn
  Close (1)
  Return

f23e: 'input line has "surf" in it
  s1 = LCase(s1)
  If InStr(s1, "heat") > 0 Then s2 = "W/m^2"
  If InStr(s1, "melt") > 0 Then s2 = "kg/s/m^2"
  Print #1, s2
  k = 1: j1 = 1
  Do While j1 <= ny
    j2 = j1 + 10
    If j2 > ny Then j2 = ny
    GoSub f23c
    For i = 1 To nx
    Input #10, g1
    For j = j1 To j2
      Input #10, g1
      fp(i, j, k) = g1
    Next: Next
    j1 = j2 + 1
  Loop
  For k = 2 To nz: For i = 1 To nx: For j = 1 To ny
    fp(i, j, k) = fp(i, j, 1)
  Next: Next: Next
  Return
End Sub

Private Sub savePreFile(n0 As Integer)
'-------------------------------
'If n0 = 1 Then save prefile and set flags
'If n0 = 2 Then doing call during cycling and needed to change one or more variables.
'          Only save the prefile.
'Save Pre-Process file
'Input fnm_pre_path = pre file path and name
'-------------------------------

'Do some error checking   June07
'    If n0 = 2 Then
'        For i = 1 To nbr
'        'Verify inner section is contained within outer section  June07
'        If bsg(ibr, 2) < bsg(ibr, 1) Or bbg(ibr, 2) < bbg(ibr, 1) Or _
'            (bsg(ibr, 2) + bwd(ibr, 2)) > (bsg(ibr, 1) + bwd(ibr, 1)) Or _
'            (bbg(ibr, 2) + bht(ibr, 2)) > (bbg(ibr, 1) + bht(ibr, 1)) _
'            Then MsgBox ("Warning: Inner tube is not fully contained within outer tube.")
'        Next
'    End If


  Open fnm_pre_path For Output As #1
  Write #1, p0, q0, "plot origin"
  Write #1, zf1, zf2, zf3, "scale factors"
  g0 = Format(theta / pi * 180, "0.#")
  Write #1, g0, "plot angle"
  Write #1, unt, vv0
  If flow_domain = 1 Then GoSub f31a 'do combustion pre file
  If flow_domain = 2 Then GoSub f31b 'do melt pre file
  Write #1, ""
  Close (1)
  If n0 = 2 Then Exit Sub
  flag_preproc_saved = True
  flag_preproc_modified = False
  Exit Sub
  
f31a: 'Do combustion
  Write #1, lth, wdt, hgt, hgt_c, "length, width, heights"
  Write #1, thk(0), thk(1), thk(2), thk(3), thk(4), _
    thk(5), "wall thickness"
  GoSub fsb_wall
  Write #1, nbr, ibr, "number of BURNERS"
  For i = 1 To nbr
    Write #1, i
    Write #1, bty(i), bdr(i), "type, orientation"
    Write #1, bsg(i, 1), bbg(i, 1), "gaps"
    Write #1, bwd(i, 1), bht(i, 1), "width, height"
    Write #1, btg(i, 1), bas(i), "temp, spray angle"
    Write #1, bav(i, 1), bah(i, 1), "ver/hor inj angle"
    Write #1, bfg(i, 1), bfa(i, 1), "gas/air flow rates"
    Write #1, bfo(i, 1), bfn(i, 1), "nitrogen/oxygen flow rates"
    Write #1, bfg(i, 2), bfa(i, 2), "gas/air flow rates"
    Write #1, bfo(i, 2), bfn(i, 2), "nitrogen/oxygen flow rates"
    If bty(i) = 4 Then 'have tube-in-tube burner   10-19-2004
      Write #1, btg(i, 2), "inner temp"
      Write #1, bav(i, 2), bah(i, 2), "inner ver/hor inj angle"
      Write #1, bsg(i, 2), bbg(i, 2), "inner gaps"
      Write #1, bwd(i, 2), bht(i, 2), "inner width, height"
    End If
  Next
  Write #1, nex, iex, "number of EXHAUSTS"
  
f31a1: 'used by both combustion and melt
  For i = 1 To nex
    Write #1, i
    Write #1, ety(i), edr(i), "type, orientation"
    Write #1, esg(i), ebg(i), "gaps"
    Write #1, ewd(i), eht(i), "width, height"
    If flow_domain = 1 Then
        Write #1, exh_wall_temp_type(i), "Choose from next 2 items, 0=> fraction type, 1=> fixed type"
        Write #1, exh_wall_temp_fixed(i), "fixed initial exhaust wall temperature for radiation"
        Write #1, exh_wall_temp_frac(i), "fraction of average wall temperature for exhaust wall"
    End If
  Next
  Write #1, irstyp, maxgi, "restart, iter #"
  Write #1, bgcon, id_rad, "conv, id_rad"
  Write #1, pg0, qh0, "pres, heat"
  If flow_domain = 1 Then s0 = "number of dog houses" _
  Else s0 = "number of bubblers"
  Write #1, nsw, isw, s0
  For i = 1 To nsw
    Write #1, i
    Write #1, wdr(i), "orientation"
    If flow_domain = 1 Then
      Write #1, wsg(i), "gap"
    Else
      Write #1, wsg(i), wbg(i), "gaps"
      Write #1, wtg(i), wfa(i), "temp, flow rate"
    End If
    Write #1, wwd(i), wdp(i), "diameter, height" ' 10-7-2004: width-->diameter, depth-->height
  Next
  Write #1, dx0, dy0, dz0, "max allowable sizes"
  Write #1, mp, np, lp, "grid numbers"
  If flow_domain = 1 Then
        'additions to combustion prefile
        Write #1, interval_rad, "interations between global radiation calculations"
        Write #1, surf_type, surf_temp, "surface type 0=>fixed, then use temperature, else melt calculates"
        Write #1, nwl, iwl, "number of radiation wavelengths"
        For i = 1 To nwl
            Write #1, wl(i)
        Next
        Write #1, start_temp, "temperature in domain at new start"
        Write #1, oxy_fuel, "oxy fuel for soot option"
        Write #1, initial_gitr, "iterations on new start before the radiation starts"
        Write #1, esf, "activation energy for soot formation J/kmol"
        Write #1, aform, "kinetic constant soot formation"
        Write #1, eso, "activation energy for soot oxidation J/kmol"
        Write #1, aoxid, "kinetic constant soot oxidation"
        Write #1, isoot_cal, "soot kinetics calibration mode"
        'Write #1, q_melt_req, "energy required for melt"
        Write #1, avail_dub0, "available double parameter position"
        Write #1, maxri2, "maximum radiosity solver inner loop 2 iteration number"
        Write #1, maxri1, "maximum radiosity solver outer loop 1 iteration number"
        Write #1, minri2, "minimum radiosity solver inner loop 2 cycles (3-20)"
        Write #1, preset_vf, "radiosity view factor option, 1=>calc once and save, 2=>calc as needed"
        Write #1, eps_m, "emissivity of melt surface"
        Write #1, eps_c, "emissivity of ceiling or crown"
        Write #1, maxms, "maximum minor species iterations per global iteration"
        Write #1, ms, "1=> minor species calculation required"
        Write #1, gui_update, "1=> CFD will provide status updates via a gfm.dat file"
        Write #1, n_cells_across_exit, "number of cells in cross section of exit (both directions)"
        Write #1, feed_unit, "feed unit type (mass or volume)"
        'Combustion data collection control flags may be set to 0 or 1 (default)
        Write #1, isum, "Summary data collection control flag"
        Write #1, iinfo, "General Information data collection control flag"
        Write #1, iTave, "Average Temperatures data collection control flag"
        Write #1, iconv, "Mass Residual Convergence data collection control flag"
        Write #1, igresid, "Equation Residuals data collection control flag"
        Write #1, igresidp, "Pre-Solve Equation Residuals data collection control flag"
        Write #1, igresidx, "Extra Equation Residuals data collection control flag"
        Write #1, igresidxp, "Extra Pre-Solve Equation Residuals data collection control flag"
        Write #1, imresid, "Minor Species Residual Convergence data collection control flag"
        Write #1, irad_detail, "Radiation Details data collection control flag"
        Write #1, irad_rad, "Radiosity Convergence data collection control flag"
        Write #1, itwal, "Wall Temperature data collection control flag"
        Write #1, irelax, "Temperature Relaxation data collection control flag"
        Write #1, iflx, "Melt Surface Flux Change data collection control flag"
        Write #1, ifieldview, "Output or FieldView data collection control flag"
  End If
  Return
  
f31b: 'Do melt
  Write #1, chamber_type, melter_component, "chamber type"
  If chamber_type = 2 Then
    Write #1, lth_r(1), wdt_r(1), hgt_r(1), "melter dimensions"
    Write #1, lth_r(2), wdt_r(2), hgt_r(2), "throat dimensions"
    Write #1, gx_r(2), gy_r(2), gz_r(2), "throat gaps"
    Write #1, lth_r(3), wdt_r(3), hgt_r(3), "refiner dimensions"
    Write #1, gx_r(3), gy_r(3), gz_r(3), "refiner gaps"
  Else
    Write #1, lth, wdt, hgt, "length, width, depth"
  End If
  Write #1, thk(0), thk(1), thk(2), thk(3), thk(4), _
    thk(5), "wall thickness"
  Write #1, nbr, ibr, "number of CHARGERS"
  For i = 1 To nbr
    Write #1, i
    Write #1, bty(i), bdr(i), "type, orientation"
    Write #1, bsg(i, 1), bbg(i, 1), "gaps"
    Write #1, bwd(i, 1), bht(i, 1), "width, height"
    Write #1, btg(i, 1), "temp"
    Write #1, bfg(i, 1), bfg(i, 2), "charge rate, cullet ratio"
    'Write #1, bav(i, 1), bah(i, 1), "sand/cullet size"
    Write #1, bav(i, 1), bah(i, 1), "batch/cullet size"
    Write #1, batch_velocity(i), "batch velocity"
  Next
  'Write #1, cl_s, h0_s, "spe heat, fussion/sand"
  'Write #1, cl_c, h0_c, "spe heat, fussion/cullet"
  'Write #1, h0_s, "fussion sand"
  Write #1, h0_s, "fussion batch"
  Write #1, h0_c, "fussion cullet"
  Write #1, nex, iex, "number of OUTLETS"
  GoSub f31a1
  For n = 1 To nex: Write #1, epull(n),: Next
  Write #1, "pull rate"
  Write #1, neb, ieb, jeb, "electric booster"
  For i = 1 To neb
    Write #1, ebty(i), ebvt(i), ebpw(i), ebhl(i), "ty,vt,pw,hl"
    For j = 1 To 3
    Write #1, ebsg(i, j), ebbg(i, j), ebwd(i, j), _
    ebht(i, j), ebdr(i, j), "sg,bg,wd,ht,dr"
  Next: Next
  Write #1, nphas, "# of phases"
  Write #1, nps0_s, ips0_s, "batch"
  Write #1, tm_s, ds_s, "melt, density"
  For i = 1 To nps0_s
    Write #1, bav(i, 1),
  Next
  Write #1, "radius"
  Write #1, nps0_c, ips0_c, "cullet"
  Write #1, tm_c, ds_c, "melt, density"
  For i = 1 To nps0_c
    Write #1, bah(i, 1),
  Next
  Write #1, "radius"
  Write #1, pg0, "pres"
  Write #1, heat_flux_type, qsh, "surface heat flux type, specified value"
  GoSub fsb_wall
  Write #1, nudf, mudf, iudf, judf, "glass prop"
  Write #1, udf_ds_n, "DENSITY"
  For n = 1 To udf_ds_n
    Write #1, udf_ds(n), udf_ds_t(n), "ds,T"
  Next
  Write #1, udf_mu_n, "VISCOSITY"
  For n = 1 To udf_mu_n
    Write #1, udf_mu(n), udf_mu_t(n), "mu,T"
  Next
  Write #1, udf_k_n, "CONDUCTIVITY"
  For n = 1 To udf_k_n
    Write #1, udf_k(n), udf_k_t(n), "k,T"
  Next
  Write #1, udf_cl_n, "LIQUID SPECIFIC HEAT"
  For n = 1 To udf_cl_n
    Write #1, udf_cl(n), udf_cl_t(n), "Cl,T"
  Next
  Write #1, udf_a_n, "VOLUME ABSORPTION"
  For n = 1 To udf_a_n
    Write #1, udf_a(n), udf_a_m(n), "a,wl"
  Next
  Write #1, udf_clc_n, "CULLET SPECIFIC HEAT"
  For n = 1 To udf_clc_n
    Write #1, udf_clc(n), udf_clc_t(n), "Clc,T"
  Next
  'Write #1, udf_cls_n, "SAND SPECIFIC HEAT"
  Write #1, udf_cls_n, "BATCH SPECIFIC HEAT"
  For n = 1 To udf_cls_n
    Write #1, udf_cls(n), udf_cls_t(n), "Cls,T"
  Next
  
  Write #1, glassExitTemp, "temperature of glass at exit"
  Write #1, condLength_s, condLength_c, "conductivity thickness (length) for batch and cullet"
  Write #1, start_temp, "temperature in domain at new start"
  Write #1, gui_update, "1=> CFD will provide status updates via a gfm.dat file"
  Write #1, extra_tunnel_cells, "number of cells in the length of an exit tunnel beyond the initial wall"
  Write #1, extra_tunnel_length, "length of exit tunnel beyond the initial wall"
  Write #1, n_cells_across_exit, "number of cells in cross section of exit (both directions)"
    'Melt data collection control flags may be set to 0 or 1
    Write #1, isum_m, "Summary data collection control flag"
    Write #1, iinfo_m, "General Information data collection control flag"
    Write #1, iTave_m, "Average Temperatures data collection control flag"
    Write #1, iconv_m, "Mass Residual Convergence data collection control flag"
    Write #1, igresid_m, "Equation Residuals data collection control flag"
    Write #1, igresidp_m, "Pre-Solve Equation Residuals data collection control flag"
    Write #1, iTchg, "Melt Surface Temperature Change data collection control flag"
    Write #1, iadjf, "Melt Surface Adjusted Flux data collection control flag"
    Write #1, iadjr, "Melt Surface Flux Relaxation data collection control flag"
    Write #1, ifieldview, "Output for FieldView data collection control flag"
  Return


fsb_wall:
  Write #1, iwal, nwal, "number of WALL, current index"
  For i = 1 To nwal
    Write #1, i, wa_dr(i), "wall direction"
    Write #1, wa_sg(i), wa_bg(i), wa_wd(i), wa_ht(i), "wall side gap, bottom gap, width, height"
    Write #1, wa_d(i), wa_k(i), wa_h(i), wa_ta(i), wa_e(i), "wall prop: thick, conduct, ext HT coeff, amb temp, emis"

  Next
  Return
End Sub

Private Sub saveGridFile()
'-------------------------------
'Write out (save) the gd####c.dat or gd####m.dat file
'        grid dimensions  mp,np,lp
'        grid distances   xg(),yg(),zg()
'        grid map values  ibc()
'Input fnm_gd_path = grid file path and name
'-------------------------------
  Open fnm_gd_path For Output As #1
  Print #1, title
  Print #1, "    r0, mp, np, lp, surf_length_c, surf_width_c, ndp0, npt0, nsp0, nr0, grid_type"
        'note CFD combustion code (not melt code) expects ndp0, npt0, nsp0, nr0
  If grid_type = Is_enhanced Then
    s0 = "1"
  Else
    s0 = "0"
  End If
  If chamber_type = 2 And flow_domain = 2 Then
    'have refiner, just want melter part
    surf_length_c = lth_r(1)
    surf_width_c = wdt_r(1)
  Else
    surf_length_c = lth
    surf_width_c = wdt
  End If
  Print #1, "    1.0 " & Format(mp, "###") _
    ; " " & Format(np, "###") & " " & Format(lp, "###") _
    ; " " & surf_length_c & " " & surf_width_c _
    ; " " & "0" & " " & "0" & " " & "7" & " " & "100" & " " & s0
    '; " " & "1" & " " & "1" & " " & "7" & " " & "100" & " " & s0
    'Line above would set # droplets & # particles to 1 group each
    
  Print #1, "X grid points"
  dx1 = 0: i1 = 3: g0 = xg(3): GoSub pg2
  For i = 5 To mp - 1 Step 2
    dx2 = xg(i) - xg(i - 2)
    If Abs(dx2 - dx1) > 0.00001 Then
      Print #1, s1
    End If
    dx1 = dx2
    i1 = i: g0 = xg(i): GoSub pg2
  Next
  Print #1, s1
  Print #1, "Y grid points"
  dx1 = 0: i1 = 3: g0 = yg(3): GoSub pg2
  For i = 5 To np - 1 Step 2
    dx2 = yg(i) - yg(i - 2)
    If Abs(dx2 - dx1) > 0.00001 Then
      Print #1, s1
    End If
    dx1 = dx2
    i1 = i: g0 = yg(i): GoSub pg2
  Next
  Print #1, s1
  Print #1, "Z grid points"
  dx1 = 0: i1 = 3: g0 = zg(3): GoSub pg2
  For i = 5 To lp - 1 Step 2
    dx2 = zg(i) - zg(i - 2)
    If Abs(dx2 - dx1) > 0.00001 Then
      Print #1, s1
    End If
    dx1 = dx2
    i1 = i: g0 = zg(i): GoSub pg2
  Next
  Print #1, s1
  Print #1, "Computational and blocked cells"
  For k = 2 To lp Step 2
    i1 = k: GoSub pg1
    c4 = s1
    c3 = ""
    s10 = "": s20 = ""
    For i = 2 To mp Step 2
      i1 = i: GoSub pg1: s1 = s1 + "  "
      s2 = ""
      For j = 2 To np Step 2
        s2 = s2 & ibc(i, j, k)
      Next
      If i = 2 Then
        s10 = s1: s20 = s2
        c3 = c3 & s1 & s2
        c3 = c3 & vbCrLf
      End If
      If i = mp Then
        If s20 <> s2 Then c3 = c3 & s10 & s20 & vbCrLf
        s10 = s1: s20 = s2
        c3 = c3 & s1 & s2
      End If
      If s2 <> s20 And i <> 4 Then
         c3 = c3 & s10 & s20 & vbCrLf
      End If
      s10 = s1: s20 = s2
    Next
    If k = 2 Then
      Print #1, c4
      Print #1, c3
      c30 = c3: c40 = c4
    End If
    If c3 <> c30 And k > 4 Then
      Print #1, c40
      Print #1, c30
    End If
    If k = lp Then
      Print #1, c4
      Print #1, c3
    End If
    c30 = c3: c40 = c4
  Next
    Print #1,
    Print #1, "&GRD"
    Print #1, "/"
  Close #1
  flag_grid_saved = True
  flag_grid_modified = False
  flag_grid_constructed = False
  Exit Sub
  
pg1:
  If i1 < 10 Then
    s1 = "   " & i1
  ElseIf i1 < 100 Then
    s1 = "  " & i1
  ElseIf i1 < 1000 Then
    s1 = " " & i1
  Else
    s1 = i1
  End If
  Return

pg2:
  GoSub pg1
  If g0 < 1 Then
    s1 = s1 & "   " & Format(g0, "0.####")
  ElseIf g0 < 10 Then
    s1 = s1 & "   " & Format(g0, "#.####")
  Else
    s1 = s1 & "  " & Format(g0, "##.####")
  End If
  Return
End Sub


Private Sub mnuCaseDelete_Click()
'-------------------------------
'Delete all the files from a case in the current domain
'-------------------------------
Dim case_files(31) As String
Dim del_case_number As String
Dim ii As Integer
Dim cur_dir As String
Dim cur_file As String
Dim cur_case As String
Dim temp As String
temp = ""
Dim temp2 As String
temp2 = ""
Dim upper_path As String
Dim temp3 As String
Dim shell_path As String
Dim save_directory As String
Dim requestfile As String
Dim deldir_task_id As Long
Dim tempdir As String


    'Make sure we are in the pre-processor
    If menu_layer = InMain Or menu_layer = InSimulation Or menu_layer = InPostProc _
        Then Exit Sub
    
    'Get case number to delete from user
    case_action = "Delete"
    If select_existing_case = False Then Exit Sub
    
    'Verify user wants to delete case
    sMessage = "You have requested deletion of case " & case_number _
        & ". Are you sure this is the case you want to delete?"
    If MsgBox(sMessage, vbYesNo + vbCritical, _
        "GFM Warning about Loss of User Work") = vbNo Then
        case_number = ""
        case_path = ""
        Exit Sub
    End If
    
    del_case_number = case_number
    
    'Get access to all the case files and empty out case folder
    s0 = case_path & "\*.*"
    Call get_filenames_into_array(s0)

    For ii = filename_array_bound To 1 Step -1
        nextfile = filename_array(ii)
        Kill case_path & "\" & nextfile
    Next ii
    
    
    '********************************
    '
    '   Tried many different ways to remove a directory,
    '   but unable to consistently do a remove.
    '   Skip over pieces of code from various tries.
    '
    '********************************
    
    
    GoTo doit

    requestfile = dnm & "\bin\pleasedel.txt"
    Open requestfile For Output As #1
    Write #1, case_path
    Close (1)
    
    shell_path = dnm & "\bin\deldirectory.exe"
    deldir_task_id = Shell(shell_path, vbMinimizedNoFocus)
    Call MsgBox("Case " & del_case_number & " has been deleted.", vbOKOnly, "GFM")
    '*********

    
    ChDir "F:"
    mydrive = CurDir
    RmDir case_path
    
    ChDir tempcur
    GoTo doit
    
    ChDir Left(case_path, Len(case_path) - 9)
    tempcur = CurDir
    If Dir$(case_path & "\*.*") <> "" Then
        Kill case_path & "\*.*" 'delete all files in case folder
    End If
    ChDir dnm
    tempcur2 = CurDir
    RmDir case_path
    GoTo doit
    
    tempcur = CurDir
    temp = Dir$(case_path & "\*.*")
    If temp <> "" Then
        Kill case_path & "\*.*" 'delete all files in case folder
    End If
    
    upper_path = ""
    If CurDir = case_path Then
        upper_path = Left(case_path, Len(case_path) - 9)
        ChDir upper_path
    End If
    tempcur2 = CurDir
    temp2 = Dir$(case_path & "\*.*") 'needed to avoid runtime error on RmDir - do not know why
    temp3 = case_path & "\"
    If temp2 = "" Then RmDir temp3
    
    '********************************
    
    
    

doit:

    Call MsgBox("The case" & del_case_number & " folder has been emptied." _
        & " However GFM has not been able to delete the folder." _
        & " Delete the folder manually later, after exiting GFM.", vbOKOnly, "GFM")
        
    'need to reinitialize
    case_number = ""
    case_path = ""
    Call display_welcome_screen
    If flow_domain = 1 Then
        Call pre1_Click
    Else
        Call pre2_Click
    End If
End Sub


Private Sub mnuCaseDelResults_Click()
'-------------------------------
'Refresh the case folder by removing all output files.
'Keep the case text, prefile, grid, sbc, and it files.
'Make sure that the pre and sbc files do not indicate cycling or restart.
'-------------------------------

'Not sure what to do about it files @@@   ???

Dim old_case_number As String
Dim old_case_path As String
Dim old_comb2_case_number As String
Dim ii As Integer
Dim nextfile As String
Dim newfile As String
Dim front_line As String
Dim back_line As String
Dim n As Integer, n1 As Integer

    'Make sure we are in the pre-processor
    If menu_layer = InMain Or menu_layer = InSimulation Or menu_layer = InPostProc _
        Then Exit Sub
    
    'Get case number to refresh from user
    case_action = "Delete Results"
    If select_existing_case = False Then Exit Sub
    
    'Verify user wants to delete case results
    sMessage = "You have requested deletion of all result files for case " & case_number _
        & ". Are you sure this is the case from which you want to delete all the output files?"
    If MsgBox(sMessage, vbYesNo + vbCritical, _
        "GFM Warning about Loss of User Work") = vbNo Then
        case_number = ""
        case_path = ""
        Exit Sub
    End If
        
    'Prepare for the special situation in which files for the second
    'configuration of a regenerative furnace are used in the first
    'configuration melt case folder.
    comb2_case_number = CInt(case_number) + 1
    Do While Len(comb2_case_number) < 4
        comb2_case_number = "0" & comb2_case_number
    Loop
        
    'Get access to all the case files.
    s0 = case_path & "\*.*"
    Call get_filenames_into_array(s0)

    For ii = 1 To filename_array_bound
        nextfile = filename_array(ii)
            
        n = InStr(nextfile, case_number)
        If n = 0 Then
            'file name does not have case number
            n1 = InStr(nextfile, comb2_case_number)
            If n1 = 0 Then
                'file name does not have second case number either,
                'delete file
                Kill case_path & "\" & nextfile
            Else
                'file has comb2 case number, keep only "grid" file
                front_line = Left(nextfile, n1 - 1)
                If (front_line <> "gd") Then _
                    Kill case_path & "\" & nextfile
            End If
        Else
            front_line = Left(nextfile, n - 1)
              
            If (front_line <> "gd") And _
                (front_line <> "case") And (front_line <> "sbc") Then _
                Kill case_path & "\" & nextfile
        End If
    Next ii
        
    'open prefile, change to new start & no cycling, save pre & sbc
    Call openPreFile(2)
    irstyp = 0
    irstypm = 0
    cycling = 0
    Call savePreFile(2)
    
    Call set_cycling_in_sbc(fnm_sbc_path, "0") 'turn off cycling in sbc file
    Call reset_restart_in_sbc 'turn off restart in sbc
    
    Call MsgBox("Case " & case_number & " has been refreshed." _
         & " Verify parameters are set as desired before using case.", vbOKOnly, "GFM")
         
    'need to reinitialize
    case_number = ""
    case_path = ""
    Call display_welcome_screen
    If flow_domain = 1 Then
        Call pre1_Click
    Else
        Call pre2_Click
    End If
End Sub

Private Sub mnuCaseSaveAs_Click()
'-------------------------------
'Copy files from one case into a new case and open the new case
'If the full_save_as flag is true, then copy all the files, otherwise
'only copy the setup files (case text, prefile, grid, conditions file)
'-------------------------------
Dim old_case_number As String
Dim old_case_path As String
Dim old_comb2_case_number As String
Dim ii As Integer
Dim nextfile As String
Dim newfile As String
Dim front_line As String
Dim back_line As String
Dim n As Integer, n1 As Integer
   
    old_case_number = case_number 'save current case number
    old_case_path = case_path 'save current case path
    
    userError = False
    flag_save_as = True 'set indicator so next routine will get a new case number and path
    Call mnuCaseSave_Click 'will save case text, pre, grid, and sbc files
                           'will also create default "it" file if needed
    If userError = True Then
        flag_save_as = False
        full_save_as = False
        Exit Sub
    End If
    
    'If the user had just created (but not saved) a new case before doing
    'the save-as, then that case folder will still exist without all the
    'supporting files.
    '
    'Note the user is responsible for deleting that case folder.       @@@
     
       
    '-----------------------------------------
    ' Begin extra copies for Full Case Save As
    '-----------------------------------------

    If full_save_as = True Then
    
        'Prepare for the special situation in which files for the second
        'configuration of a regenerative furnace are used in the first
        'configuration melt case folder.
        old_comb2_case_number = CInt(old_case_number) + 1
        Do While Len(old_comb2_case_number) < 4
            old_comb2_case_number = "0" & old_comb2_case_number
        Loop
        comb2_case_number = CInt(case_number) + 1
        Do While Len(comb2_case_number) < 4
            comb2_case_number = "0" & comb2_case_number
        Loop
        
        'Get access to all the old case files.
        'Use "nextfile" for old case files and "newfile" for the save-as case files
        s0 = old_case_path & "\*.*"
        Call get_filenames_into_array(s0)

        For ii = 1 To filename_array_bound
            nextfile = filename_array(ii)
            
            n = InStr(nextfile, old_case_number)
            If n = 0 Then
                'file name does not have case number
                n1 = InStr(nextfile, old_comb2_case_number)
                If n1 = 0 Then
                    'file name does not have second case number either,
                    'use full name
                    newfile = nextfile
                    front_line = ""
                Else
                    'file has old comb2 case number, change it to new case +1
                    front_line = Left(nextfile, n1 - 1)
                    back_line = Right(nextfile, Len(nextfile) - (n1 + 3))
                    newfile = front_line & comb2_case_number & back_line
                End If
            Else
                'replace case number in filename
                front_line = Left(nextfile, n - 1)
                back_line = Right(nextfile, Len(nextfile) - (n + 3))
                newfile = front_line & case_number & back_line
            End If
            'Note that the text, grid, pre, and sbc files have already been saved
            'by the earlier call to mnuCaseSave_Click
            If FileExist(case_path & "\" & newfile) = False Then
                FileCopy old_case_path & "\" & nextfile, case_path & "\" & newfile
            ElseIf front_line = "it" Then
                    FileCopy old_case_path & "\" & nextfile, case_path & "\" & newfile
            ElseIf (flag_conditions_modsav = False And front_line = "sbc") Then
                    FileCopy old_case_path & "\" & nextfile, case_path & "\" & newfile
            End If
        Next ii
        full_save_as = False
        
    'Else
    '    'not a full save, want "it" files anyway              '  @@@   Not sure about the it files, only want defaults?
    '    If FileExist(old_case_path & "\it" & old_case_number & "t.dat") Then _
    '        FileCopy old_case_path & "\it" & old_case_number & "t.dat", _
    '        case_path & "\it" & case_number & "t.dat"
    '    If FileExist(old_case_path & "\it" & old_case_number & "m.dat") Then _
    '        FileCopy old_case_path & "\it" & old_case_number & "m.dat", _
    '        case_path & "\it" & case_number & "m.dat"
           
    End If 'end of full save copies
        
    Call openPreFile(1) 'display the new case figure
    If isoot_cal = 1 Then
        mnuPropSootCali.Checked = True
        'mnuPropSootNeed.Enabled = True
    Else
        mnuPropSootCali.Checked = False
        'mnuPropSootNeed.Enabled = False
    End If
    flag_save_as = False
    Call MsgBox("Case " & case_number & " has been saved.", vbOKOnly, "GFM")
End Sub


Private Sub mnuCaseDescript_Click()
'-------------------------------
'Allow user to update case description
'-------------------------------
    'Display case description form
    modVariables.c_domain = flow_domain
    modVariables.c_name = case_number
    modVariables.c_path = dnm
    
    frmDescription.Show 1 'display form so user can update title and description

    case_title = modVariables.c_title
    LbCaseTitle.Caption = case_title
End Sub

Private Sub mnuCaseNew_Click()
'goes to sub menu Box->file12 and Crown->file11
End Sub


Private Function create_new_case() As Boolean
'-------------------------------
'Begin a new case:
'   Save existing case
'   Get new case number, title, and description from user
'   Create the case name text file
'-------------------------------
    Dim fnm_len As Integer
    Dim ttl2 As String
    create_new_case = False
        
    'If doing a save as, then do not need to save current case first, otherwise check if need to
    If flag_save_as = False Then
        'Make sure user is ready to create a new case
        If menu_layer = InGridConstruction Or menu_layer = InPreProc Then
            If (flag_preproc_modified = True Or flag_grid_modified = True Or _
                    flag_grid_constructed = True Or flag_conditions_modified = True Or _
                    flag_grid_enhanced = True) And _
                    case_number <> "" Then
                'Check if user wants to save current work
                If MsgBox("Do you want the current case saved before creating a new case?", _
                        vbYesNo + vbCritical, "GFM Warning about Loss of User Work") = vbYes Then
                    'Save the current case
                    Call mnuCaseSave_Click
                Else 'no save done
                    'user is responsible to delete any partial case if needed
                End If
            End If
        Else
            'action should not be allowed if not in pre proc
            Exit Function
        End If
    End If 'flag_save_as = False

    'Display create-new-case form, so user can provide case number
    modVariables.c_domain = flow_domain
    modVariables.c_path = dnm
    
    frmCaseCreate.Show 1
    
    If modVariables.c_return = 1 Then
        'valid case number was not given
        create_new_case = False
        Exit Function
    End If
    
    case_number = modVariables.c_name
    case_path = modVariables.c_casepath 'The case folder has been created
    
    'Display case description form, so user can provide title and description
    'modVariables.c_domain = flow_domain
    'modVariables.c_name = case_number
    'modVariables.c_path = dnm
    
    frmDescription.Show 1
    
    If modVariables.c_return = 0 Then
        'Note that form processing created the case.....txt file
        case_title = modVariables.c_title
        LbCaseTitle.Caption = case_title
        LbCase.Caption = "Case " & case_number
        LbCaseTitle.Enabled = True
        LbCase.Enabled = True
        LbCaseTitle.Visible = True
        LbCase.Visible = True
        mnuCaseDescript.Enabled = True
        flag_preproc_modified = True
        If flow_domain = 1 Then
            nphas = 1
            frmMain.Caption = "Pre-Processor: combustion space"
            mnuPropEmis.Visible = True
            mnuPropSoot.Visible = True
            mnuPropCFD.Visible = True
            fnm_txt = "case" & case_number & "c.txt" 'text file
            fnm_txt_path = case_path & "\" & fnm_txt
        ElseIf flow_domain = 2 Then
            frmMain.Caption = "Pre-Processor: melter"
            mnuProp_GlassExitTemp.Visible = True
            mnuPropGlass.Visible = True
            fnm_txt = "case" & case_number & "m.txt" 'text file
            fnm_txt_path = case_path & "\" & fnm_txt
        End If
        create_new_case = True

    Else
        create_new_case = False
    End If
End Function


Private Sub mnuCaseOpen_Click()
'-------------------------------
'Open an existing case for pre or post processing
'-------------------------------
    'Make sure user is ready to open a new case
    If menu_layer = InGridConstruction Or menu_layer = InPreProc Then
        If (flag_preproc_modified = True Or flag_grid_modified = True Or _
                flag_grid_constructed = True Or flag_conditions_modified = True Or _
                flag_grid_enhanced = True) And _
                case_number <> "" Then
            'Check if user wants to save current work
            If MsgBox("Do you want the current case saved before opening a case?", _
                    vbYesNo + vbCritical, "GFM Warning about Loss of User Work") = vbYes Then
                'Save the current case
                Call mnuCaseSave_Click
            Else 'no save done
                'User is responsible for deleting case folders if needed
            End If
        End If
    End If
    
    case_action = "Open"
    If select_existing_case = False Then Exit Sub
    
    If menu_layer = InGridConstruction Or menu_layer = InPreProc Then
        If flow_domain = 1 Then
            frmMain.Caption = "Pre-Processor: combustion space"
            mnuPropEmis.Visible = True
            mnuPropSoot.Visible = True
            mnuPropCFD.Visible = True
            MnuOptFed.Visible = True
        Else
            frmMain.Caption = "Pre-Processor: melter"
            mnuProp_GlassExitTemp.Visible = True
            mnuPropGlass.Visible = True
            MnuOptFed.Visible = False
        End If
        
        Call openPreFile(1)
        If isoot_cal = 1 Then
            mnuPropSootCali.Checked = True
            'mnuPropSootNeed.Enabled = True
        Else
            mnuPropSootCali.Checked = False
            'mnuPropSootNeed.Enabled = False
        End If
        mnuCaseSave.Enabled = True
        mnuCaseSaveAs.Enabled = True
        mnuCaseSaveAsFull.Enabled = True
        MnuOptGui.Visible = True
        
    Else 'menu_layer = InPostProc
        Call openOutFile
        mnuCaseNew.Enabled = False
        mnuCaseSave.Enabled = False
        mnuCaseSaveAs.Enabled = False
        mnuCaseSaveAsFull.Enabled = False
        MnuOptGui.Visible = False

    End If
    mnuCaseNew.Enabled = False
    mnuCaseOpen.Enabled = False
    mnuCaseDelete.Enabled = False
    mnuCaseDelResults.Enabled = False
End Sub


Private Function select_existing_case() As Boolean
'-------------------------------
'Select existing case:
'   Get old case number from user
'-------------------------------
    select_existing_case = False
    
    '------------------------------------------------
    'Display existing-case form, so user can select case number
    modVariables.c_domain = flow_domain
    modVariables.c_path = dnm
    modVariables.c_action = case_action
    
    frmExistingCase.Show 1
    
    If modVariables.c_return = 1 Then
        'valid case number was not selected
        Exit Function
    End If
    
    case_number = modVariables.c_name
    case_path = modVariables.c_casepath 'The case folder has been selected
    If flow_domain = 1 Then nphas = 1
       
    If case_action = "Delete" Then
        select_existing_case = True
        Exit Function
    End If
    
    'Determine associated file names and paths
    If flow_domain = 1 Then
        fnm_txt = "case" & case_number & "c.txt" 'text file
        fnm_txt_path = case_path & "\" & fnm_txt
        fnm_pre = "gd" & case_number & "c.pre" 'pre file
        fnm_pre_path = case_path & "\" & fnm_pre
        fnm_gd = "gd" & case_number & "c.dat" 'grid file
        fnm_gd_path = case_path & "\" & fnm_gd
        fnm_sbc = "sbc" & case_number & "c.dat" 'conditions file
        fnm_sbc_path = case_path & "\" & fnm_sbc
        fnm_rt = "rt" & case_number & "c.out" 'field variable output file
        fnm_rt_path = case_path & "\" & fnm_rt
        fnm_rg = "rg" & case_number & "c.d" 'restart file
        fnm_rg_path = case_path & "\" & fnm_rg
        fnm_runs_path = dnm & "combustion\runs.dat" 'runs file
   
    Else
        fnm_txt = "case" & case_number & "m.txt" 'text file
        fnm_txt_path = case_path & "\" & fnm_txt
        fnm_pre = "gd" & case_number & "m.pre"
        fnm_pre_path = case_path & "\" & fnm_pre
        fnm_gd = "gd" & case_number & "m.dat"
        fnm_gd_path = case_path & "\" & fnm_gd
        fnm_sbc = "sbc" & case_number & "m.dat"
        fnm_sbc_path = case_path & "\" & fnm_sbc
        fnm_rt = "rt" & case_number & "m.out"
        fnm_rt_path = case_path & "\" & fnm_rt
        fnm_rg = "rg" & case_number & "m.d" 'restart file
        fnm_rg_path = case_path & "\" & fnm_rg
        fnm_runs_path = dnm & "melt\runs.dat"
    
    End If
       
    'Make sure required files exist
    'pre file is always required.
    If FileExist(fnm_pre_path) = False Or FileExist(fnm_gd_path) = False Or FileExist(fnm_sbc_path) = False Then
        Call MsgBox("Case has been corrupted, missing one or more files." _
            & " Choose another case.", vbOKOnly, "GFM")
        Exit Function
    End If
        
    If menu_layer = InPostProc Then
        'Look for an existing output file.
        If FileExist(fnm_rt_path) = False Then
            Call MsgBox("Case output file does not exist.  Run a simulation or " _
            & "choose another case.", vbOKOnly, "GFM")
            Exit Function
        End If
    End If

    'Get case description
    Open fnm_txt_path For Input As #1
    Line Input #1, s0 'already have case name
    Line Input #1, case_title
    Close (1)
    LbCaseTitle.Caption = case_title
    LbCase.Caption = "Case " & case_number
    LbCaseTitle.Enabled = True
    LbCase.Enabled = True
    LbCaseTitle.Visible = True
    LbCase.Visible = True
    mnuCaseDescript.Enabled = True
    flag_preproc_modified = True
   
    'initialize status flags
    flag_preproc_modified = False
    flag_preproc_open = False
    flag_preproc_saved = False
    flag_grid_modified = False
    flag_grid_active = False
    flag_grid_saved = False
    flag_conditions_modified = False
    flag_conditions_saved = False
    flag_grid_enhanced = False
    
    select_existing_case = True
End Function


Private Sub mnuCaseSave_Click()
'-------------------------------
'Save the case
'To avoid certain problems later, the save will always save the pre file,
'grid file, and conditions (sbc) file.
'-------------------------------
Dim old_case_number As String
Dim old_case_path As String
    
    If case_number = "" Then
        'there is nothing to save
        Call MsgBox("No case is active.  Cannot save.", vbOKOnly, "GFM")
        userError = True
        Exit Sub
    End If
      
    'Do some case checking before the save is completed.
    If flow_domain = 1 And id_rad = 1 And ms = 0 Then
        Call MsgBox("Radiation calculation requires subspecies interations." _
            & " Cannot save case.", vbOKOnly, "GFM")
        userError = True
        Exit Sub
    End If
    If flow_domain = 1 And id_rad = 1 And interval_rad = 0 Then
        Call MsgBox("Radiation calculation requires a gas phase interval." _
            & " Cannot save case.", vbOKOnly, "GFM")
        userError = True
        Exit Sub
    End If
    If flow_domain = 1 And ms = 1 And interval_rad = 0 Then
        Call MsgBox("Subspecies calculation requires a gas phase interval." _
            & " Cannot save case.", vbOKOnly, "GFM")
        userError = True
        Exit Sub
    End If
      
    If flag_save_as = True Then
        'Display create-new-case form, so user can provide case number
        modVariables.c_domain = flow_domain
        modVariables.c_path = dnm
        
        frmCaseCreate.Show 1
        
        If modVariables.c_return = 1 Then
            'valid case number was not given
            Call MsgBox("A valid case number was not given.  Cannot save.", vbOKOnly, "GFM")
            userError = True
            Exit Sub
        End If
        
        case_number = modVariables.c_name
        case_path = modVariables.c_casepath 'The case folder has been created
        
        If flow_domain = 1 Then
            textfile = "case" & case_number & "c.txt"
        Else
            textfile = "case" & case_number & "m.txt"
        End If
        
        'copy old text file to new case folder
        Open fnm_txt_path For Input As #5
        Open case_path & "\" & textfile For Output As #6
        'Do a hand file copy but change the case number
        Line Input #5, s0 'get first line
        Print #6, "Case " & case_number
        LbCase.Caption = "Case " & case_number
        Line Input #5, s0
        Print #6, s0
        LbCaseTitle.Caption = s0
        Do While Not EOF(5) 'copy rest of file
            Line Input #5, s0
            Print #6, s0
        Loop
        Close (5)
        Close (6)
        
        fnm_txt = textfile
        fnm_txt_path = case_path & "\" & fnm_txt
    End If 'save as
    
    'Determine rest of setup associated path and filenames (text done separately)
    If flow_domain = 1 Then
        fnm_pre = "gd" & case_number & "c.pre"
        fnm_pre_path = case_path & "\" & fnm_pre
        fnm_gd = "gd" & case_number & "c.dat"
        fnm_gd_path = case_path & "\" & fnm_gd
        fnm_sbc = "sbc" & case_number & "c.dat"
        fnm_sbc_path = case_path & "\" & fnm_sbc
    Else
        fnm_pre = "gd" & case_number & "m.pre"
        fnm_pre_path = case_path & "\" & fnm_pre
        fnm_gd = "gd" & case_number & "m.dat"
        fnm_gd_path = case_path & "\" & fnm_gd
        fnm_sbc = "sbc" & case_number & "m.dat"
        fnm_sbc_path = case_path & "\" & fnm_sbc
    End If
    
    'Save pre file
    Call savePreFile(1)
    flag_preproc_modified = False
    
    'Construct grid if not already done
    '(mnuView is visible if the grid has been constructed.)
    If mnuView.Visible = False Or (flag_grid_modified = True And flag_grid_enhanced = False _
            And grid_type = Not_enhanced) Then Call grid(0)
    
    'Save grid file
    Call saveGridFile
    flag_grid_modified = False
    flag_grid_constructed = False
    flag_grid_enhanced = False
    mnuView.Visible = True
    
    'create and save sbc file
    'flag_conditions_modsav = flag_conditions_modified
    Call saveSbcFile
    flag_conditions_modified = False
        
    If flag_save_as = False Then _
        Call MsgBox("Case " & case_number & " has been saved.", vbOKOnly, "GFM")
End Sub

Private Sub mnuCaseSaveAsFull_Click()
    flag_conditions_modsav = flag_conditions_modified
    full_save_as = True
    Call mnuCaseSaveAs_Click
    full_save_as = False
End Sub

Private Sub mnuCycleDomains_Click()
'---------------------------------
'Activate automatic cycling between simulating combustion and melting
'This routine may also be called from mnuCycleRegen_Click
'---------------------------------
'Dim cycleDomains As Boolean 'true => doing automatic cycling between simulation domains with combustion first
'Dim cycleCombCnt As Integer 'count of times combustion simulation has started during auto cycling
'Dim cycleMeltCnt As Integer 'count of times melt simulation has started during auto cycling
'Dim userStoppedSim As Boolean 'true => User canceled or requested simulation to stop
'Dim cycleEnd As Integer 'Number of times user wants to cycle thru combustion/melt simulation
'Dim cycleRegen As Boolean 'true => doing automatic cycling for a regenerative furnace
'Dim cycleComb2Cnt As Integer 'count of times second combustion simulation in cycle has started
    
    cycleDomains = True
    cycleCombCnt = 0 'indicates combustion simulation has not started
    cycleMeltCnt = 0 'indicates melt simulation has not started
    
    'Ask user for number of combustion/melt cycles to run
    'cycleEnd = default_cycleEnd
    's2 = InputBox("Specify number of combustion/melt cycles to run.", _
    '    "Cycling Count", cycleEnd, 2850, 1000)
    'If s2 = "" Or IsNumeric(s2) = False Then
    '    cycleDomains = False
    '    cycleRegen = False
    '    Exit Sub 'failed to start simulation
    'End If
    'cycleEnd = CInt(s2)
    
    
    'Display cycle information form, so user can provide parameters
    modVariables.c_cycleEnd = default_cycleEnd
    modVariables.c_cycle2_gitr = default_cycle2_gitr
    modVariables.c_cycle2_msitr = default_cycle2_msitr
    modVariables.c_cycle2_rintv = default_cycle2_rintv
    modVariables.c_cycle2_mitr = default_cycle2_mitr
    modVariables.c_cycle2_scale_on = default_cycle2_scale_on
    
    frmCycleInfo.Show 1
    
    If modVariables.c_return = 1 Then
        'User canceled
        cycleDomains = False
        cycleRegen = False
        Exit Sub 'failed to start simulation
    End If
    
    cycleEnd = modVariables.c_cycleEnd
    cycle2_gitr = modVariables.c_cycle2_gitr
    cycle2_msitr = modVariables.c_cycle2_msitr
    cycle2_rintv = modVariables.c_cycle2_rintv
    cycle2_mitr = modVariables.c_cycle2_mitr
    cycle2_scale_on = modVariables.c_cycle2_scale_on
    
    
    'Start up combustion run after verifying that required files exist in both domains
    cycleInfo = 1
    Call sim1_Click
    If userError = True Then
        cycleDomains = False
        cycleRegen = False
        Exit Sub 'failed to start simulation
    End If
    
    'Note that subroutine simnew (called from sim1_click) has special code to set up
    'for the melt case as well as the combustion case (and second combustion
    'case if simulating a regenerative furnace).
              
    'Print cycle progress info on screen
    cycleCombCnt = 1 'indicates first combustion simulation has started
    ListCycle.Clear
    For i = 0 To 3
        ListCycle.AddItem ""
    Next
    ListCycle.Width = 4200
    ListCycle.Height = 1200
    ListCycle.Top = hgts - ListCycle.Height
    ListCycle.Left = wdts - 75 - ListCycle.Width
    If cycleRegen = True Then
        'ListCycle.List(0) = "Domain Cycling in Regenerative Furnace:"
        ListCycle.List(0) = "Regen Domain Cycling (Comb First):"
        ListCycle.List(1) = "   First combustion domain is active"
    Else
        ListCycle.List(0) = "Domain Cycling (Comb First):"
        ListCycle.List(1) = "   Combustion domain is active"
    End If
    ListCycle.List(2) = "   In cycle 1 of " & CStr(cycleEnd)
    ListCycle.Visible = True
    ListCycle.Enabled = True
End Sub


'---------------------------------
'Switch domain during automatic cycling between simulating combustion and melting
'This routine called when simulation for one cycle has been completed, meaning
'the CFD command window is gone.
'---------------------------------
'Dim cycleDomains As Boolean 'true => doing automatic cycling between simulation domains with combustion first
'Dim cycleDomainsMelt As Boolean 'true => doing automatic cycling between simulation domains with melt first
'Dim cycleCombCnt As Integer 'count of times combustion simulation has started during auto cycling
'Dim cycleMeltCnt As Integer 'count of times melt simulation has started during auto cycling
'Dim userStoppedSim As Boolean 'true => User canceled or requested simulation to stop
'Dim cycleEnd As Integer 'Number of times user wants to cycle thru combustion/melt simulation

Private Sub switch_domain()
Dim it_file As String
Dim comb2_it_file As String
Dim to_file As String
Dim from_file As String

    'Discard any remaining GUI<->CFD communication files
    On Error Resume Next
    s0 = case_path & "\gfm.dat": If FileExist(s0) Then Kill s0
    s0 = case_path & "\gfm0.dat": If FileExist(s0) Then Kill s0
    s0 = case_path & "\runstop.dat": If FileExist(s0) Then Kill s0
    Timer1.Enabled = False
    'Note that when the CFD code is activated the CFD code will
    'delete the runend.txt file that exists.
    
    'Print CFD error message if it exists
    If FileExist(case_path & "\runend.txt") Then
        Open case_path & "\runend.txt" For Input As #2
        Line Input #2, s0
        If InStr(s0, "normal") = 0 Then
            Do While Not EOF(2)
                Line Input #2, s1
                s0 = s0 & s1
            Loop
            Close (2)
            'abnormal termination of domain cycling
            ListCycle.List(3) = "ABNORMAL CYCLING TERMINATION."
            cycleDomains = False
            cycleDomainsMelt = False
            cycleCombCnt = 0
            cycleRegen = False
            cycleComb2Cnt = 0
            cycleMeltCnt = 0
            userStoppedSim = False
            cycleEnd = 0
            Lbsw.BackColor = vbGreen
            Lbsw.Caption = "cont"
            If List1.List(0) = "Running" And List1.Visible = True Then
                List1.List(0) = "Run Done"
            End If
            Lbsw.Visible = False
            ex0.Caption = "Done"
            Lbdel.Visible = False
            Call MsgBox("CFD ERROR, return to PreProcessor to resolve:  " & s0, _
                vbOKOnly + vbCritical, "GFM")
            Exit Sub
        End If
        Close (2)
    Else 'the runend file does not exist, CFD must have died
            ListCycle.List(3) = "ABNORMAL CYCLING TERMINATION."
            cycleDomains = False
            cycleDomainsMelt = False
            cycleCombCnt = 0
            cycleRegen = False
            cycleComb2Cnt = 0
            cycleMeltCnt = 0
            userStoppedSim = False
            cycleEnd = 0
            Lbsw.BackColor = vbGreen
            Lbsw.Caption = "cont"
            If List1.List(0) = "Running" And List1.Visible = True Then
                List1.List(0) = "Run Done"
            End If
            Lbsw.Visible = False
            ex0.Caption = "Done"
            Lbdel.Visible = False
            Call MsgBox("UNKNOWN CFD ERROR, return to PreProcessor to resolve.", _
                vbOKOnly + vbCritical, "GFM")
            Exit Sub
    End If
    
    If flow_domain = 1 Then
        'finished a combustion run
        
        'Copy heat flux file it####m.dat from combustion to melt directory
        it_file = "\it" & case_number & "m.dat"
        If FileExist(case_path & it_file) Then _
            FileCopy case_path & it_file, meltpath & it_file
                
        If cycleRegen = True And cycleCombCnt > cycleComb2Cnt Then
            'Finished first combustion case
            'need to start up second combustion case run
                           
            'Change case number
            case_number = comb2_case_number
            case_path = comb2path
            
            fnm_txt = comb2_fnm_txt
            fnm_txt_path = comb2_fnm_txt_path
            fnm_pre = comb2_fnm_pre
            fnm_pre_path = comb2_fnm_pre_path
            fnm_gd = comb2_fnm_gd
            fnm_gd_path = comb2_fnm_gd_path
            fnm_sbc = comb2_fnm_sbc
            fnm_sbc_path = comb2_fnm_sbc_path
            fnm_rg = comb2_fnm_rg
            fnm_rg_path = comb2_fnm_rg_path
            fnm_runs_path = comb2_fnm_runs_path
            
            'create a comb2 runs.dat file
            Open comb2_fnm_runs_path For Output As #4
            Print #4, "'Run Number:' "; comb2_case_number 'Place sbc file number into runs.dat file
            Print #4, "'Grid File:'  "; comb2_case_number 'Place grid file number into runs.dat file
            Print #4, "'Regenerate:' "; "0" 'regeneration indicator only used in melt domain
            Print #4, "'Run Number:' "; comb2_case_number
            Print #4, "'Grid File'   "; comb2_case_number
            Close (4) 'runs.dat file has been created for comb2
            
            If cycleComb2Cnt = 1 Then
                'will be doing second cycle of combustion simulation
                Call setSbcForCycle2
            End If
            
            cycleComb2Cnt = cycleComb2Cnt + 1 'indicates next combustion simulation has started
            cycleInfo = cycleComb2Cnt
            Call simcr_Click 'begin combustion simulation
    
            ex0.Caption = "Stop Run"
            LbCaseTitle.Caption = comb2_case_title
            LbCase.Caption = " Case " & comb2_case_number
            
            ListCycle.List(1) = "   Second combustion domain is active"
            ListCycle.List(2) = "   In cycle " & CStr(cycleComb2Cnt) & " of " & CStr(cycleEnd)
            Exit Sub
        End If
        
        If cycleEnd = cycleCombCnt And cycleDomainsMelt = True Then
            'finished domain cycling
            ListCycle.List(1) = "   Completed " & CStr(cycleEnd) & " cycles."
            ListCycle.List(2) = ""
            cycleDomains = False
            cycleDomainsMelt = False
            cycleCombCnt = 0
            cycleRegen = False
            cycleComb2Cnt = 0
            cycleMeltCnt = 0
            userStoppedSim = False
            cycleEnd = 0
            Lbsw.BackColor = vbGreen
            Lbsw.Caption = "cont"
            If List1.List(0) = "Running" And List1.Visible = True Then
                List1.List(0) = "Run Done"
            End If
            Lbsw.Visible = False
            ex0.Caption = "Done"
            Lbdel.Visible = False
            Exit Sub
        End If
        
        If cycleRegen = True And cycleCombCnt = cycleComb2Cnt Then
            'Finished second combustion case for regenerative furnace
            'Change case number
            case_number = melt_case_number
        End If
            
            
        'need to start up melt run
        flow_domain = 2
        case_path = meltpath
        menu_layer = InSimulation
        
        fnm_txt = melt_fnm_txt
        fnm_txt_path = melt_fnm_txt_path
        fnm_pre = melt_fnm_pre
        fnm_pre_path = melt_fnm_pre_path
        fnm_gd = melt_fnm_gd
        fnm_gd_path = melt_fnm_gd_path
        fnm_sbc = melt_fnm_sbc
        fnm_sbc_path = melt_fnm_sbc_path
        fnm_rg = melt_fnm_rg
        fnm_rg_path = melt_fnm_rg_path
        fnm_runs_path = melt_fnm_runs_path
     
        If cycleMeltCnt = 1 Then
            'will be doing second cycle melt simulation
            'ChDir case_path
            Call setSbcForCycle2
        ElseIf cycleMeltCnt = cycle2_scale_on Then
            'Change flux type to unscaled
            Call setfluxUnscaled
        End If
            
        cycleMeltCnt = cycleMeltCnt + 1 'indicates next melt simulation has started
        cycleInfo = cycleMeltCnt
        Call simmr_Click 'begin melt simulation
    
        ex0.Caption = "Stop Run"
        LbCaseTitle.Caption = melt_case_title
        LbCase.Caption = " Case " & melt_case_number
            
        ListCycle.List(1) = "   Melt is active"
        If cycleDomainsMelt = True Then
            ListCycle.List(2) = "   In cycle " & CStr(cycleMeltCnt) & " of " & CStr(cycleEnd)
        End If
        
        
    Else 'flow_domain=2, finished melt run

        'Copy temperature file it####t.dat from melt to combustion directory
        it_file = "\it" & case_number & "t.dat"
        If FileExist(case_path & it_file) Then
            FileCopy case_path & it_file, combpath & it_file
            
            If cycleRegen = True Then
                'Also copy temperature file it####t.dat from melt for comb2
                comb2_it_file = "\it" & comb2_case_number & "t.dat"
                FileCopy case_path & it_file, comb2path & comb2_it_file
            End If
        End If
               
        If cycleEnd = cycleMeltCnt And cycleDomains = True Then
            'finished domain cycling
            ListCycle.List(1) = "   Completed " & CStr(cycleEnd) & " cycles."
            ListCycle.List(2) = ""
            cycleDomains = False
            cycleDomainsMelt = False
            cycleCombCnt = 0
            cycleRegen = False
            cycleComb2Cnt = 0
            cycleMeltCnt = 0
            userStoppedSim = False
            cycleEnd = 0
            Lbsw.BackColor = vbGreen
            Lbsw.Caption = "cont"
            If List1.List(0) = "Running" And List1.Visible = True Then
                List1.List(0) = "Run Done"
            End If
            Lbsw.Visible = False
            ex0.Caption = "Done"
            Lbdel.Visible = False
            Exit Sub
        End If
        
        'finished melt, start up combustion run
        flow_domain = 1
        menu_layer = InSimulation
        case_path = combpath
        
        fnm_txt = comb_fnm_txt
        fnm_txt_path = comb_fnm_txt_path
        fnm_pre = comb_fnm_pre
        fnm_pre_path = comb_fnm_pre_path
        fnm_gd = comb_fnm_gd
        fnm_gd_path = comb_fnm_gd_path
        fnm_sbc = comb_fnm_sbc
        fnm_sbc_path = comb_fnm_sbc_path
        fnm_rg = comb_fnm_rg
        fnm_rg_path = comb_fnm_rg_path
        fnm_runs_path = comb_fnm_runs_path
        
        'create a comb runs.dat file
        Open comb_fnm_runs_path For Output As #4
        Print #4, "'Run Number:' "; case_number 'Place sbc file number into runs.dat file
        Print #4, "'Grid File:'  "; case_number 'Place grid file number into runs.dat file
        Print #4, "'Regenerate:' "; "0" 'regeneration indicator only used in melt domain
        Print #4, "'Run Number:' "; case_number
        Print #4, "'Grid File'   "; case_number
        Close (4) 'runs.dat file has been created for comb
        
        If cycleCombCnt = 1 Then
            'will be doing second cycle of combustion simulation
            'ChDir case_path
            Call setSbcForCycle2
        End If
        
        cycleCombCnt = cycleCombCnt + 1 'indicates next combustion simulation has started
        cycleInfo = cycleCombCnt
        Call simcr_Click 'begin combustion simulation
    
        ex0.Caption = "Stop Run"
        LbCaseTitle.Caption = comb_case_title
             
        If cycleRegen = True Then
            ListCycle.List(1) = "   First combustion domain is active"
        Else
            ListCycle.List(1) = "   Combustion is active"
        End If
        If cycleDomains = True Then
            ListCycle.List(2) = "   In cycle " & CStr(cycleCombCnt) & " of " & CStr(cycleEnd)
        End If
    End If
End Sub


'----------------------------------
'Set the restart indicator in the sbc (Conditions) file and the prefile
'If cycling, set surf_type to indicate that the melt domain calculates the surface temperature
'On input fnm_sbc_path must be the sbc file full path
'On input fnm_pre_path must be the prefile full path
'----------------------------------
Private Sub setRestartIndicator()
Dim tmp_fnm_sbc As String
Dim s00 As String
Dim front_line As String
Dim back_line As String

    'Work on sbc file
    tmp_fnm_sbc = Left(fnm_sbc_path, Len(fnm_sbc_path) - 3) & "tmp"
    Name fnm_sbc_path As tmp_fnm_sbc     'This is not working
    Open tmp_fnm_sbc For Input As #5
    Open fnm_sbc_path For Output As #6
    'Do a hand file copy but change the restart indicator
    Line Input #5, s0 'get first line   'need more error checking ???
    Print #6, s0
    Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
        Line Input #5, s0 'get another line
        Print #6, s0
    Loop
    Line Input #5, s0 'skip next line
    Print #6, s0
            
    Line Input #5, s0 'this line should have restart type
    n = InStr(s0, "IRSTYP=")
    front_line = Left(s0, n + 6)
    back_line = Right(s0, Len(s0) - (n + 7))
    restart_str = "1" 'set restart indicator to restart
    s00 = front_line & restart_str & back_line
    Print #6, s00
            
    n = 0 'also reset minor species restart indicator
    Do While n = 0
        Line Input #5, s0
        n = InStr(s0, "IRSTYPM=")
        If n <> 0 Then
            front_line = Left(s0, n + 7)
            back_line = Right(s0, Len(s0) - (n + 8))
            s00 = front_line & restart_str & back_line
            Print #6, s00
        Else
            Print #6, s0
        End If
    Loop
                
    If flow_domain = 1 Then
        n = 0 'also reset initial_gitr because it only applys to new starts
        'Note that initial_gitr must be the only variable on the line
        Do While n = 0
            Line Input #5, s0
            n = InStr(s0, "initial_gitr=")
            If n <> 0 Then
                front_line = Left(s0, n + 12)
                s00 = front_line & "0"
                Print #6, s00
            Else
                Print #6, s0
            End If
        Loop
    ElseIf cycleDomains = True Or cycleDomainsMelt = True Then
        'flow_domain=2 and cycling
        n = 0 'The iheat_flux_type variable should indicate combustion calculation
        Do While n = 0
            Line Input #5, s0
            n = InStr(s0, "iheat_flux_type=") '16 char in substring
            If n <> 0 Then
                front_line = Left(s0, n + 15)
                s1 = Mid(s0, n + 16, 1)
                If s1 = "1" Or s1 = "2" Then
                    'change uniform scaled to combustion calculated scaled
                    s00 = front_line & "2"
                Else
                    'change uniform unscaled to combustion calculated unscaled
                    s00 = front_line & "4"
                End If
                Print #6, s00
            Else
                Print #6, s0
            End If
        Loop
    End If
                
    Do While Not EOF(5) 'copy rest of file
        Line Input #5, s0
        Print #6, s0
    Loop
    
    Close (5)
    Close (6)
    Kill tmp_fnm_sbc
    
    'Work on prefile
    Call openPreFile(2) 'read in the prefile
    irstyp = 1 'indicate restart
    irstypm = 1
    If flow_domain = 1 Then
        initial_gitr = 0
        If cycleDomains = True Or cycleDomainsMelt = True Then
            'Doing combustion during cycling.
            'Also change surface melt type indicator.
            surf_type = 1 'indicates calculated in melt
        End If
    ElseIf cycleDomains = True Or cycleDomainsMelt = True Then
        'flow_domain=2 and cycling
        heat_flux_type = 2
    End If
    Call savePreFile(2)
End Sub


'----------------------------------
'Change the information in the Sbc file for the second cycle.
'On input fnm_sbc_path must be the sbc file full path
'----------------------------------
Private Sub setSbcForCycle2()
Dim tmp_fnm_sbc As String
Dim s00 As String
Dim front_line As String
Dim back_line As String
Dim mid_line As String

    'Do a hand file copy of the sbc file, replacing the 2nd. and 3rd. lines with cycle 2 information
    
    tmp_fnm_sbc = Left(fnm_sbc_path, Len(fnm_sbc_path) - 3) & "tmp"
    Name fnm_sbc_path As tmp_fnm_sbc
    Open tmp_fnm_sbc For Input As #5
    Open fnm_sbc_path For Output As #6
    
    Line Input #5, s0 'get first line
    Print #6, s0
    Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
        Line Input #5, s0 'get another line
        Print #6, s0
    Loop
    Line Input #5, s0 'skip 1st. line in namelist
    Print #6, s0
                  
    Line Input #5, s0 '2nd. line in namelist
    
    'Need to handle the two domains separately
    
    If flow_domain = 1 Then 'combustion
        
        's00 = "   IRSTYP=1,MAXGI=" & CStr(cycle2_gitr)
        'Print #6, s00
        
        'set restart indicator to restart
        n = InStr(s0, "IRSTYP=")
        front_line = Left(s0, n + 6)
        back_line = Right(s0, Len(s0) - (n + 7))
        restart_str = "1"
        s00 = front_line & restart_str & back_line
        'set number of gas iterations on same line
        n = InStr(s00, "MAXGI=")
        front_line = Left(s00, n + 5)
        mid_line = Right(s00, Len(s00) - (n + 5))
        nn = InStr(mid_line, ",")
        back_line = Right(mid_line, Len(mid_line) - (nn - 1))
        s0 = front_line & CStr(cycle2_gitr) & back_line
        Print #6, s0
        
        Line Input #5, s0 '3rd. line in namelist
        s00 = "   MS=1,IRSTYPM=1,MAXMS=" & CStr(cycle2_msitr) & ",interval_rad=" & CStr(cycle2_rintv)
        Print #6, s00
        
        n = 0 'also reset initial_gitr because it only applys to new starts
        'Note that initial_gitr must be the only variable on the line
        Do While n = 0
            Line Input #5, s0
            n = InStr(s0, "initial_gitr=")
            If n <> 0 Then
                front_line = Left(s0, n + 12)
                s00 = front_line & "0"
                Print #6, s00
            Else
                Print #6, s0
            End If
        Loop

        
    Else 'melt
        
        n = InStr(s0, "IRSTYP=")
        front_line = Left(s0, n + 6)
        back_line = Right(s0, Len(s0) - (n + 7))
        restart_str = "1" 'set restart indicator to restart
        s00 = front_line & restart_str & back_line
        Print #6, s00
    
        Line Input #5, s0 '3rd. line in namelist
        n = InStr(s0, "MAXSI=")
        front_line = Left(s0, n + 5)
        s00 = front_line & CStr(cycle2_mitr)
        Print #6, s00

        n = 0 'The iheat_flux_type variable should indicate combustion calculation
        Do While n = 0
            Line Input #5, s0
            n = InStr(s0, "iheat_flux_type=") '16 char in substring
            If n <> 0 Then
                front_line = Left(s0, n + 15)
                s1 = Mid(s0, n + 16, 1)
                If s1 = "1" Or s1 = "2" Then
                    'Change uniform scaled to combustion calculated scaled
                    'However, if user does not want scaling on anymore then change to unscaled
                    If cycle2_scale_on < 2 Then
                        s00 = front_line & "4"
                    Else
                        s00 = front_line & "2"
                    End If
                Else
                    'Change uniform unscaled to combustion calculated unscaled
                    s00 = front_line & "4"
                End If
                Print #6, s00
            Else
                Print #6, s0
            End If
        Loop
    End If
                
    Do While Not EOF(5) 'copy rest of file
        Line Input #5, s0
        Print #6, s0
    Loop
    
    Close (5)
    Close (6)
    Kill tmp_fnm_sbc
    
End Sub


'----------------------------------
'Change the melt Sbc file to indicate unscaled heat flux.
'On input fnm_sbc_path must be the sbc file full path
'----------------------------------
Private Sub setfluxUnscaled()
Dim tmp_fnm_sbc As String
Dim s00 As String
Dim front_line As String
Dim back_line As String

    'Do a hand file copy of the melt sbc file, replacing the heat flux value
    
    tmp_fnm_sbc = Left(fnm_sbc_path, Len(fnm_sbc_path) - 3) & "tmp"
    Name fnm_sbc_path As tmp_fnm_sbc
    Open tmp_fnm_sbc For Input As #5
    Open fnm_sbc_path For Output As #6
    
        n = 0
        Do While n = 0
            Line Input #5, s0
            n = InStr(s0, "iheat_flux_type=") '16 char in substring
            If n <> 0 Then
                front_line = Left(s0, n + 15)
                'Change to combustion calculated unscaled
                s00 = front_line & "4"
                Print #6, s00
            Else
                Print #6, s0
            End If
        Loop
                
    Do While Not EOF(5) 'copy rest of file
        Line Input #5, s0
        Print #6, s0
    Loop
    
    Close (5)
    Close (6)
    Kill tmp_fnm_sbc
    
End Sub


'----------------------------------
'Reset the restart indicator in the sbc (Conditions) file
'On input fnm_sbc_path must be the sbc file full path
'----------------------------------
Private Sub reset_restart_in_sbc()
Dim tmp_fnm_sbc As String
Dim s00 As String
Dim front_line As String
Dim back_line As String

    tmp_fnm_sbc = Left(fnm_sbc_path, Len(fnm_sbc_path) - 3) & "tmp"
    Name fnm_sbc_path As tmp_fnm_sbc
    Open tmp_fnm_sbc For Input As #5
    Open fnm_sbc_path For Output As #6
    'Do a hand file copy but change the restart indicator
    Line Input #5, s0 'get first line
    Print #6, s0
    Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
        Line Input #5, s0 'get another line
        Print #6, s0
    Loop
    Line Input #5, s0 'skip next line
    Print #6, s0
            
    Line Input #5, s0 'this line should have restart type
    n = InStr(s0, "IRSTYP=")
    front_line = Left(s0, n + 6)
    back_line = Right(s0, Len(s0) - (n + 7))
    restart_str = "0" 'set restart indicator to new start
    s00 = front_line & restart_str & back_line
    Print #6, s00
            
    n = 0 'also reset minor species restart indicator
    Do While n = 0
        Line Input #5, s0
        n = InStr(s0, "IRSTYPM=")
        If n <> 0 Then
            front_line = Left(s0, n + 7)
            back_line = Right(s0, Len(s0) - (n + 8))
            s00 = front_line & restart_str & back_line
            Print #6, s00
        Else
            Print #6, s0
        End If
    Loop
                               
    Do While Not EOF(5) 'copy rest of file
        Line Input #5, s0
        Print #6, s0
    Loop
    
    Close (5)
    Close (6)
    Kill tmp_fnm_sbc
    
End Sub


Private Sub mnuCycleDomainsMelt_Click()
'---------------------------------
'Activate automatic cycling between simulating melting and combustion
'This routine may also be called from mnuCycleRegenMelt_Click
'---------------------------------
'Dim cycleDomains As Boolean 'true => doing automatic cycling between simulation domains with combustion first
'Dim cycleDomainsMelt As Boolean 'true => doing automatic cycling between simulation domains with melt first
'Dim cycleCombCnt As Integer 'count of times combustion simulation has started during auto cycling
'Dim cycleMeltCnt As Integer 'count of times melt simulation has started during auto cycling
'Dim userStoppedSim As Boolean 'true => User canceled or requested simulation to stop
'Dim cycleEnd As Integer 'Number of times user wants to cycle thru combustion/melt simulation
'Dim cycleRegen As Boolean 'true => doing automatic cycling for a regenerative furnace
'Dim cycleComb2Cnt As Integer 'count of times second combustion simulation in cycle has started
    
    cycleDomainsMelt = True
    cycleMeltCnt = 0 'indicates melt simulation has not started
    cycleCombCnt = 0 'indicates combustion simulation has not started
    
    'Ask user for number of melt/combustion cycles to run
    'cycleEnd = default_cycleEnd
    's2 = InputBox("Specify number of melt/combustion cycles to run.", _
    '    "Cycling Count", cycleEnd, 2850, 1000)
    'If s2 = "" Or IsNumeric(s2) = False Then
    '    cycleDomainsMelt = False
    '    cycleRegen = False
    '    Exit Sub 'failed to start simulation
    'End If
    'cycleEnd = CInt(s2)
    
    'Display cycle information form, so user can provide parameters
    modVariables.c_cycleEnd = default_cycleEnd
    modVariables.c_cycle2_gitr = default_cycle2_gitr
    modVariables.c_cycle2_msitr = default_cycle2_msitr
    modVariables.c_cycle2_rintv = default_cycle2_rintv
    modVariables.c_cycle2_mitr = default_cycle2_mitr
    modVariables.c_cycle2_scale_on = default_cycle2_scale
    
    frmCycleInfo.Show 1
    
    If modVariables.c_return = 1 Then
        'User canceled
        cycleDomains = False
        cycleRegen = False
        Exit Sub 'failed to start simulation
    End If
    
    cycleEnd = modVariables.c_cycleEnd
    cycle2_gitr = modVariables.c_cycle2_gitr
    cycle2_msitr = modVariables.c_cycle2_msitr
    cycle2_rintv = modVariables.c_cycle2_rintv
    cycle2_mitr = modVariables.c_cycle2_mitr
    cycle2_scale_on = modVariables.c_cycle2_scale_on
       
    
    'Start up melt run after verifying that required files exist in both domains
    cycleInfo = 1
    Call sim2_Click
    If userError = True Then
        cycleDomainsMelt = False
        cycleRegen = False
        Exit Sub 'failed to start simulation
    End If
    
    'Note that subroutine simnew (called from sim2_click) has special code to set up
    'for the melt case as well as the combustion case (and second combustion
    'case if simulating a regenerative furnace).
              
    'Print cycle progress info on screen
    cycleMeltCnt = 1 'indicates first melt simulation has started
    ListCycle.Clear
    For i = 0 To 3
        ListCycle.AddItem ""
    Next
    ListCycle.Width = 4200
    ListCycle.Height = 1200
    ListCycle.Top = hgts - ListCycle.Height
    ListCycle.Left = wdts - 75 - ListCycle.Width
    If cycleRegen = True Then
        'ListCycle.List(0) = "Domain Cycling in Regenerative Furnace:"
        ListCycle.List(0) = "Regen Domain Cycling (Melt First):"
        ListCycle.List(1) = "   Melt domain is active"
    Else
        ListCycle.List(0) = "Domain Cycling (Melt First):"
        ListCycle.List(1) = "   Melt domain is active"
    End If
    ListCycle.List(2) = "   In cycle 1 of " & CStr(cycleEnd)
    ListCycle.Visible = True
    ListCycle.Enabled = True

End Sub

Private Sub mnuCycleRegen_Click()
    cycleRegen = True 'indicate doing cycling for a regenerative burner
    cycleComb2Cnt = 0 'indicates combustion simulation has not started
    Call mnuCycleDomains_Click
End Sub

Private Sub mnuCycleRegenMelt_Click()
    cycleRegen = True 'indicate doing cycling for a regenerative burner
    cycleComb2Cnt = 0 'indicates combustion simulation has not started
    Call mnuCycleDomainsMelt_Click
End Sub



Private Sub MnuOptCol_Click()
'-------------------------------
'Allow user to update data collection print flags
'-------------------------------

    If flow_domain = 1 Then 'combustion

        'Display 'choose data' form
        modVariables.c_domain = flow_domain
        modVariables.c_isum = isum
        modVariables.c_iinfo = iinfo
        modVariables.c_iTave = iTave
        modVariables.c_iconv = iconv
        modVariables.c_igresid = igresid
        modVariables.c_igresidp = igresidp
        modVariables.c_igresidx = igresidx
        modVariables.c_igresidxp = igresidxp
        modVariables.c_imresid = imresid
        modVariables.c_irad_detail = irad_detail
        modVariables.c_irad_rad = irad_rad
        modVariables.c_itwal = itwal
        modVariables.c_irelax = irelax
        modVariables.c_iflx = iflx
        modVariables.c_ifieldview = ifieldview
        
        frmChooseData.Show 1 'display form so user can update data file print flags
    
        isum = modVariables.c_isum
        iinfo = modVariables.c_iinfo
        iTave = modVariables.c_iTave
        iconv = modVariables.c_iconv
        igresid = modVariables.c_igresid
        igresidp = modVariables.c_igresidp
        igresidx = modVariables.c_igresidx
        igresidxp = modVariables.c_igresidxp
        imresid = modVariables.c_imresid
        irad_detail = modVariables.c_irad_detail
        irad_rad = modVariables.c_irad_rad
        itwal = modVariables.c_itwal
        irelax = modVariables.c_irelax
        iflx = modVariables.c_iflx
        ifieldview = modVariables.c_ifieldview
        
    Else 'melt
        'Display 'choose data' form
        modVariables.c_domain = flow_domain
        modVariables.c_isum_m = isum_m
        modVariables.c_iinfo_m = iinfo_m
        modVariables.c_iTave_m = iTave_m
        modVariables.c_iconv_m = iconv_m
        modVariables.c_igresid_m = igresid_m
        modVariables.c_igresidp_m = igresidp_m
        modVariables.c_iTchg = iTchg
        modVariables.c_iadjf = iadjf
        modVariables.c_iadjr = iadjr
        modVariables.c_ifieldview_m = ifieldview_m
                
        frmChooseData.Show 1 'display form so user can update data file print flags
    
        isum_m = modVariables.c_isum_m
        iinfo_m = modVariables.c_iinfo_m
        iTave_m = modVariables.c_iTave_m
        iconv_m = modVariables.c_iconv_m
        igresid_m = modVariables.c_igresid_m
        igresidp_m = modVariables.c_igresidp_m
        iTchg = modVariables.c_iTchg
        iadjf = modVariables.c_iadjf
        iadjr = modVariables.c_iadjr
        ifieldview_m = modVariables.c_ifieldview_m
    End If
End Sub

Private Sub MnuOptFedMas_Click()
    If feed_unit = "kg/s" Then Exit Sub
    feed_unit = "kg/s"
    MnuOptFedMas.Checked = True
    MnuOptFedVol.Checked = False
    
    For n = 1 To nbr
        'convert volume flow m^3/s to mass flow kg/s by multiplying by density at reference state
        
        'g0 = bfa(n, 1) * 0.21 * rho_O2_ref
        'g1 = bfa(n, 1) * 0.79 * rho_N2_ref
        'bfa(n, 1) = g0 + g1
        bfa(n, 1) = bfa(n, 1) * rho_air_ref
        bfg(n, 1) = bfg(n, 1) * rho_fuel_ref
        bfo(n, 1) = bfo(n, 1) * rho_O2_ref
        bfn(n, 1) = bfn(n, 1) * rho_N2_ref
    
        'g0 = bfa(n, 2) * 0.21 * rho_O2_ref
        'g1 = bfa(n, 2) * 0.79 * rho_N2_ref
        'bfa(n, 2) = g0 + g1
        bfa(n, 2) = bfa(n, 2) * rho_air_ref
        bfg(n, 2) = bfg(n, 2) * rho_fuel_ref
        bfo(n, 2) = bfo(n, 2) * rho_O2_ref
        bfn(n, 2) = bfn(n, 2) * rho_N2_ref
    Next
    'convert heat of fuel from J/m^s to J/kg
    qh0 = qh0 / rho_fuel_ref
    
    If InStr(LCase(List1.List(0)), "burn") > 0 Then
        Call burn_ls
    ElseIf InStr(LCase(List1.List(0)), "para") > 0 Then
        Call sim_ls
    End If
End Sub

Private Sub MnuOptFedVol_Click()
    If feed_unit = "m^3/s" Then Exit Sub
    feed_unit = "m^3/s"
    MnuOptFedVol.Checked = True
    MnuOptFedMas.Checked = False
    
    For n = 1 To nbr
        'convert mass flow kg/s to volume flow m^3/s by dividing by density at reference state
        
        bfa(n, 1) = bfa(n, 1) / rho_air_ref
        bfg(n, 1) = bfg(n, 1) / rho_fuel_ref
        bfo(n, 1) = bfo(n, 1) / rho_O2_ref
        bfn(n, 1) = bfn(n, 1) / rho_N2_ref
    
        bfa(n, 2) = bfa(n, 2) / rho_air_ref
        bfg(n, 2) = bfg(n, 2) / rho_fuel_ref
        bfo(n, 2) = bfo(n, 2) / rho_O2_ref
        bfn(n, 2) = bfn(n, 2) / rho_N2_ref
    Next
    
    'convert heat of fuel from J/kg to J/m^3
    qh0 = qh0 * rho_fuel_ref
    
    If InStr(LCase(List1.List(0)), "burn") > 0 Then
        Call burn_ls
    ElseIf InStr(LCase(List1.List(0)), "para") > 0 Then
        Call sim_ls
    End If
End Sub


Private Sub MnuOptGuiOff_Click()
Dim fnm_gui_path As String

    gui_update = 0
    MnuOptGuiOn.Checked = False
    MnuOptGuiOff.Checked = True
    If menu_layer = InSimulation Then
        'need to notify CFD program about GUI update status
        'If flow_domain = 1 Then
        '   fnm_gui_path = dnm & "combustion"
        'Else
        '    fnm_gui_path = dnm & "melt"
        'End If
        fnm_gui_path = case_path & "\gui_update.txt"
        Open fnm_gui_path For Output As #1
        Print #1, gui_update
        Close #1
    End If
    flag_conditions_modified = True
    flag_preproc_modified = True
End Sub

Private Sub MnuOptGuiOn_Click()
Dim fnm_gui_path As String

    gui_update = 1
    MnuOptGuiOn.Checked = True
    MnuOptGuiOff.Checked = False
    If menu_layer = InSimulation Then
        'need to notify CFD program about GUI update status
        'If flow_domain = 1 Then
        '    fnm_gui_path = dnm & "combustion"
        'Else
        '    fnm_gui_path = dnm & "melt"
        'End If
        fnm_gui_path = case_path & "\gui_update.txt"
        Open fnm_gui_path For Output As #1
        Print #1, gui_update
        Close #1
    End If
    flag_conditions_modified = True
    flag_preproc_modified = True
End Sub


Private Sub mnuOptRunPlot_Click()
Dim retval As Long
Dim save_directory As String
Dim plot_directory As String
Dim shell_path As String

    save_directory = CurDir
    
    'If flow_domain = 1 Then
    '    plot_directory = dnm & "combustion"
    'Else
    '    plot_directory = dnm & "melt"
    'End If
    'plot_directory = dnm
    'ChDir plot_directory
    shell_path = dnm & "\bin\RunPlot.exe"
  
    retval = Shell(shell_path, vbNormalFocus)
    
    ChDir save_directory
End Sub


Private Sub mnuProp_GlassExitTemp_Click()
'----------------------------------
'Get approximate temperature of glass at the exit.
'----------------------------------
    If flow_domain = 1 Then Exit Sub 'valid only for melter
    g0 = glassExitTemp
    s0 = "K": Call ucv 'convert units to British if needed for display
    s1 = "Specify approximate glass temperature ("
    s0 = s1 & s0 & ") at exit."
    
    s2 = InputBox(s0, title, g0, 3000, 6000)
    
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then
        If s2 < 0 Then
            Exit Sub
        End If
    Else
        Exit Sub
    End If
    g0 = s2
    s0 = "F": Call ucvb 'convert units to SI units if needed
    glassExitTemp = g0
    
    flag_conditions_modified = True
    flag_preproc_modified = True
End Sub


Private Sub mnuPropCFDmaxInner_Click()
    g0 = maxri2
    s0 = "Specify maximum radiosity solver inner loop 2 iteration number. (>=1)"
    
    s2 = InputBox(s0, title, g0, 2850, 1000)
    If IsNumeric(s2) = False Or s2 < 1 Then Exit Sub
    maxri2 = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropCFDmaxOuter_Click()
    g0 = maxri1
    s0 = "Specify maximum radiosity solver outer loop 1 iteration number. (>=1)"
    
    s2 = InputBox(s0, title, g0, 2850, 1000)
    If IsNumeric(s2) = False Or s2 < 1 Then Exit Sub
    maxri1 = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropCFDminInner_Click()
    g0 = minri2
    s0 = "Specify minimum radiosity solver inner loop 2 cycles. (>=1)"
    
    s2 = InputBox(s0, title, g0, 2850, 1000)
    If IsNumeric(s2) = False Or s2 < 1 Then Exit Sub
    minri2 = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropCFDvf_Click()
    If preset_vf = 1 Then
        s0 = "once and saved in memory. "
        s1 = "NOW:  KEEP VIEW FACTOR SET IN MEMORY"
    Else
        s0 = "as needed and not saved in memory. "
        s1 = "NOW:  COMPUTE VIEW FACTORS AS NEEDED"
    End If
    
    sMessage = "         " & s1 & Chr(10) & Chr(10) _
        & "To determine the radiation between each boundary cell and every other " _
        & "boundary cell, view factors are used.  If there are N boundary cells, " _
        & "then there must be N*N view factors.  There are two options for handling " _
        & "view factors.  The most efficient option is to create all the view factors " _
        & "during program initialization and save them in program memory. This option " _
        & "is the fastest, but it uses a lot of memory for dense grids.  The other " _
        & "option, to create the view factors each time they are needed, is slower, but " _
        & "does not require as much memory." _
        & Chr(10) & Chr(10) _
        & "Currently, radiation view factors will be calculated " & s0 _
        & Chr(10) & Chr(10) _
        & "Do you want to keep the current radiation view factor option? " _
        & "(Changing the option will require running in 'New Start' mode.)"

    If MsgBox(sMessage, vbYesNo, "GFM") = vbYes Then Exit Sub
    
    'change view factor setting
    If preset_vf = 1 Then
        preset_vf = 2
        mnuPropCFDvf.Checked = False
    Else
        preset_vf = 1
        mnuPropCFDvf.Checked = True
    End If
    irstyp = 0 'force new start
    flag_preproc_modified = True
    flag_conditions_modified = True

End Sub

Private Sub mnuPropEmisCrown_Click()
    g0 = eps_c
    s0 = "Specify surface emissivity of the ceiling or crown. (>0 and <=1)"
    
    s2 = InputBox(s0, title, g0, 2850, 1000)
    If IsNumeric(s2) = False Or s2 < 0 Or s2 > 1 Then Exit Sub
    eps_c = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropEmisMelt_Click()
    g0 = eps_m
    s0 = "Specify melt surface emissivity. (>0 and <=1)"
   
    s2 = InputBox(s0, title, g0, 2850, 1000)
    If IsNumeric(s2) = False Or s2 <= 0 Or s2 > 1 Then Exit Sub
    eps_m = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropEmisWall_Click()
    g0 = wa_e(1)
    s0 = "Specify surface emissivity of the side walls. (>0 and <=1)"
    
    s2 = InputBox(s0, title, g0, 2850, 1000)
    If IsNumeric(s2) = False Or s2 <= 0 Or s2 > 1 Then Exit Sub
    wa_e(1) = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropGlass_Click()
    Call ctgp_Click
End Sub

Private Sub mnuPropSootAform_Click()
    dub = aform
    s0 = "Specify kinetic constant for soot formation. (kg{soot}/(kg{fuel} s m^3))"
    
    s2 = InputBox(s0, title, dub, 2850, 1000)
    If IsNumeric(s2) = False Or s2 < 0 Then Exit Sub
    aform = s2
    flag_preproc_modified = True
    flag_conditions_modified = True

End Sub

Private Sub mnuPropSootAoxid_Click()
    dub = aoxid
    s0 = "Specify kinetic constant for soot oxidation. (kg{mix}/(kg{fuel} K^(.5) s m^3))"
    
    s2 = InputBox(s0, title, dub, 2850, 1000)
    If IsNumeric(s2) = False Or s2 < 0 Then Exit Sub
    aoxid = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropSootCali_Click()
    If isoot_cal = 1 Then
        sMessage = "Do you want to leave the soot kinetics calibration mode?"
    Else
        sMessage = "Do you want to run a soot kinetics calibration?"
    End If
    
    If MsgBox(sMessage, vbYesNo, "GFM") = vbNo Then Exit Sub
    
    'change calibration mode setting
    If isoot_cal = 1 Then
        isoot_cal = 0
        'mnuPropSootNeed.Enabled = False
        mnuPropSootCali.Checked = False
    Else
        isoot_cal = 1
        'mnuPropSootNeed.Enabled = True
        mnuPropSootCali.Checked = True
    End If
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropSootEsf_Click()
    dub = esf
    s0 = "Specify activation energy for soot formation. (J/kmol)"
    
    s2 = InputBox(s0, title, dub, 2850, 1000)
    If IsNumeric(s2) = False Or s2 <= 0 Then Exit Sub
    esf = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

Private Sub mnuPropSootEso_Click()
    dub = eso
    s0 = "Specify activation energy for soot oxidation. (J/kmol)"
    
    s2 = InputBox(s0, title, dub, 2850, 1000)
    If IsNumeric(s2) = False Or s2 <= 0 Then Exit Sub
    eso = s2
    flag_preproc_modified = True
    flag_conditions_modified = True
End Sub

'Private Sub mnuPropSootNeed_Click()
'    dub = q_melt_req
'    s0 = "Specify energy required for melt. (W)"
'
'    s2 = InputBox(s0, title, dub, 2850, 1000)
'    If IsNumeric(s2) = False Or s2 <= 0 Then Exit Sub
'    q_melt_req = s2
'    flag_preproc_modified = True
'    flag_conditions_modified = True
'
'End Sub

Private Sub mnuPropWall_Click()
    Call ctwp_Click
End Sub

'-------------------------------
'Save a bitmap of the screen image (below the menu bar)
'-------------------------------
Private Sub mnuScreenPrint_Click()
  If flow_domain = 1 Then
    CD1.InitDir = dnm & "combustion"
  ElseIf flow_domain = 2 Then
    CD1.InitDir = dnm & "melt"
  Else
    CD1.InitDir = dnm
  End If
  CD1.FileName = ""
  CD1.Filter = "Bitmap file (*.bmp)|*.bmp"
  CD1.ShowSave
  If CD1.FileName = "" Then
     Call MsgBox("No filename given, so screen save was not done.", vbOKOnly, "GFM")
     Exit Sub
  End If
  SavePicture Image, CD1.FileName
End Sub


'-------------------------------
' Create Simulation Setup and Boundary Conditions file
'Input fnm_sbc_path = conditions file path and name
'-------------------------------
Private Sub saveSbcFile()
  Dim gmfr As Single, bmcon As Single
  Dim fi As Double, vol As Double, dn As Double
  
  Call grid(1) 'fill in object grid lines and assign cell map values
  If flow_domain = 2 Then GoTo fssm 'melt
  
  '---------------------------------
  '  Combustion section of Sbc save
  '---------------------------------

  'Determine burner flow rates.
  'Note that feed_unit related items default to a volume based flow rate,
  'whereas the CFD code is based on a mass flow rate.

  ReDim gfg(nbr, 2), gfa(nbr, 2), gfo(nbr, 2), gfn(nbr, 2)
  gmfr = 0
  For n = 1 To nbr
    If feed_unit = "kg/s" Then
        bfo(n, 1) = bfo(n, 1) + bfa(n, 1) * 0.21 * W_O2 / W_air
        bfn(n, 1) = bfn(n, 1) + bfa(n, 1) * 0.79 * W_N2 / W_air
        bfa(n, 1) = 0 'air is reset to 0 because moved into N2 & O2
                      'using gfa to sum up flow
        'do not need conversion to kg/s
        gfg(n, 1) = bfg(n, 1): gfa(n, 1) = bfg(n, 1)
        gfo(n, 1) = bfo(n, 1): gfa(n, 1) = gfa(n, 1) + bfo(n, 1)
        gfn(n, 1) = bfn(n, 1): gfa(n, 1) = gfa(n, 1) + bfn(n, 1)
    Else
        bfo(n, 1) = bfo(n, 1) + bfa(n, 1) * 0.21
        bfn(n, 1) = bfn(n, 1) + bfa(n, 1) * 0.79
        bfa(n, 1) = 0 'air is reset to 0 because moved into N2 & O2
                      'using gfa to sum up flow
        'convert m^3/s by * molecular weight * reference pressure / universal constant / reference temperature
        g0 = bfg(n, 1) * rho_fuel_ref '16.0426 * 101325# / 8314.472 / 298.15
        gfg(n, 1) = g0: gfa(n, 1) = g0
        g0 = bfo(n, 1) * rho_O2_ref '31.9988 * 101325# / 8314.472 / 298.15
        gfo(n, 1) = g0: gfa(n, 1) = gfa(n, 1) + g0
        g0 = bfn(n, 1) * rho_N2_ref '28.0134 * 101325# / 8314.472 / 298.15
        gfn(n, 1) = g0: gfa(n, 1) = gfa(n, 1) + g0
    End If
    gmfr = gmfr + gfa(n, 1)
    
    If feed_unit = "kg/s" Then
        
        bfo(n, 2) = bfo(n, 2) + bfa(n, 2) * 0.21 * W_O2 / W_air
        bfn(n, 2) = bfn(n, 2) + bfa(n, 2) * 0.79 * W_N2 / W_air
        bfa(n, 2) = 0
        'do not need conversion to kg/s
        gfg(n, 2) = bfg(n, 2): gfa(n, 2) = bfg(n, 2)
        gfo(n, 2) = bfo(n, 2): gfa(n, 2) = gfa(n, 2) + bfo(n, 2)
        gfn(n, 2) = bfn(n, 2): gfa(n, 2) = gfa(n, 2) + bfn(n, 2)
    Else
        bfo(n, 2) = bfo(n, 2) + bfa(n, 2) * 0.21
        bfn(n, 2) = bfn(n, 2) + bfa(n, 2) * 0.79
        bfa(n, 2) = 0
        'convert m^3/s by * molecular weight * reference pressure / universal constant / reference temperature
        g0 = bfg(n, 2) * rho_fuel_ref '16.0426 * 101325# / 8314.472 / 298.15
        gfg(n, 2) = g0: gfa(n, 2) = g0
        g0 = bfo(n, 2) * rho_O2_ref '31.9988 * 101325# / 8314.472 / 298.15
        gfo(n, 2) = g0: gfa(n, 2) = gfa(n, 2) + g0
        g0 = bfn(n, 2) * rho_N2_ref '28.0134 * 101325# / 8314.472 / 298.15
        gfn(n, 2) = g0: gfa(n, 2) = gfa(n, 2) + g0
    End If
    gmfr = gmfr + gfa(n, 2)
  Next
  If gmfr <= 0 Then gmfr = 1 'gmfr is the sum of the mass flow or set to 1
  bmcon = 0.000000000001
  
  
  'Begin creation of the combustion conditions file
  Open fnm_sbc_path For Output As #4
  Print #4, "&INPUT"
  Print #4, "   REACT=.TRUE."
  s1 = "   IRSTYP=" & irstyp
  s1 = s1 & ",MAXGI=" & maxgi
  s1 = s1 & ",ID_RAD=" & id_rad
  Print #4, s1
  If ms > 0 Then
    s1 = "   MS=1,IRSTYPM=" & irstyp & ",MAXMS=" & maxms
  Else
    s1 = "   MS=0,IRSTYPM=" & irstyp & ",MAXMS=0"
  End If
  s1 = s1 & ",interval_rad=" & interval_rad
  Print #4, s1
  s1 = "   GMFR="
  If gmfr < 1 Then
    's1 = s1 & Format(gmfr, "0.#######E-")
    s1 = s1 & gmfr
  Else
    's1 = s1 & Format(gmfr, "0.#######")
    s1 = s1 & gmfr
  End If
  Print #4, s1
  's1 = "   BFRACT=0.75,RADLOSS=0.65874,Q0="
  s1 = "   Q0="
  'g0 = qh0 / 0.6517
  If feed_unit = "kg/s" Then 'already have correct unit for CFD
        g0 = qh0
  Else 'convert to mass based unit
        g0 = qh0 / rho_fuel_ref
  End If
  's1 = s1 & Format(f0, "0.##E+")
  s1 = s1 & g0
  Print #4, s1
  s1 = "   BGCON=" & Format(bgcon, "#.##E-")
  s1 = s1 & ",BMCON=" & Format(bmcon, "#.##E-")
  Print #4, s1
  Print #4, "   T_init=" & start_temp
  Print #4, "   cycling=0"
  'Defaulting to not doing automatic cycling
  'Will change sbc file if/when know cycling is being done
  Print #4, "   oxy_fuel=" & oxy_fuel
  Print #4, "   initial_gitr=" & initial_gitr
  Print #4, "   esf=" & esf
  Print #4, "   aform=" & aform
  Print #4, "   eso=" & eso
  Print #4, "   aoxid=" & aoxid
  Print #4, "   isoot_cal=" & isoot_cal
  'Print #4, "   q_melt_req=" & q_melt_req
  Print #4, "   available_parameter=" & avail_dub0
  Print #4, "   gui_update=" & gui_update
    'Combustion data collection control flags may be set to 0 or 1 (default)
    Print #4, "   isum=" & isum
    Print #4, "   iinfo=" & iinfo
    Print #4, "   iTave=" & iTave
    Print #4, "   iconv=" & iconv
    Print #4, "   igresid=" & igresid
    Print #4, "   igresidp=" & igresidp
    Print #4, "   igresidx=" & igresidx
    Print #4, "   igresidxp=" & igresidxp
    Print #4, "   imresid=" & imresid
    Print #4, "   irad_detail=" & irad_detail
    Print #4, "   irad_rad=" & irad_rad
    Print #4, "   itwal=" & itwal
    Print #4, "   irelax=" & irelax
    Print #4, "   iflx=" & iflx
    Print #4, "   ifieldview=" & ifieldview
  Print #4, "/"
  
  For n = 1 To nbr
    m = bdr(n) 'set s1=flow direction
    If m = 0 Or m = 1 Then s1 = " 1"
    If m = 2 Or m = 3 Then s1 = " 2"
    If m = 4 Or m = 5 Then s1 = " 3"
    i1 = bi1(n): i2 = bi2(n)
    j1 = bj1(n): j2 = bj2(n)
    k1 = bk1(n): k2 = bk2(n)
    If n = 1 Then GoSub fss1 'print pressure reference point coordinates
    GoSub fss3 'set burner inlet coordinates and boundary conditions
               'print inlet block(s) for burner # n
  Next
  Print #4, " 0 0 0 0 0 0 0 "
  
    'Print exhaust information
    Print #4, "Exhausts:"
    Print #4, nex
    For n = 1 To nex
        'Put orientation into format used by CFD code
        Select Case edr(n)
        Case 0
            s1 = "1 0 " 'orientation x normal, direction positive
        Case 1
            s1 = "1 1 "
        Case 2
            s1 = "2 0 "  'orientation y normal
        Case 3
            s1 = "2 1 "
        Case 4
            s1 = "3 0 "  'orientation z normal
        Case 5
            s1 = "3 1 "
        End Select
        s0 = ei1(n) & " " & ej1(n) & " " & ek1(n) & "  " & ei2(n) & " " & ej2(n) & " " & ek2(n)
        Print #4, s0
        s0 = exh_wall_temp_type(n) & " " & exh_wall_temp_frac(n) & " " & exh_wall_temp_fixed(n)
        Print #4, s1 & s0
        'Print #4, ei1(n), ej1(n), ek1(n), ei2(n), ej2(n), ek2(n)
        'Print #4, s1, s2, exh_wall_temp_type(n), exh_wall_temp_frac(n), exh_wall_temp_fixed(n)
      Next
  
  Print #4, "Wall Properties:"
  For n = 2 To nwal
    m = wa_dr(n)
    i1 = wa_i1(n): i2 = wa_i2(n)
    j1 = wa_j1(n): j2 = wa_j2(n)
    k1 = wa_k1(n): k2 = wa_k2(n)
    GoSub fssm2
    s0 = " " & wa_d(n) & " " & wa_k(n) & " " & wa_h(n)
    Print #4, s0 & " " & wa_ta(n) & " " & wa_e(n)
  Next
  Print #4, " 0 0 0 0 0 0 0 "
  GoSub fss_rad   'Add RADIN namelist to SBC file
  s0 = "": i = 0
  For n = 1 To nwl
    'Add wave lengths to SBC file
    i = i + 1: s0 = s0 & Format(wl(n), "0.### ")
    If i = 5 Or n = nwl Then
      Print #4, s0
      s0 = "": i = 0
    End If
  Next
  Close (4)
  flag_conditions_saved = True
  flag_conditions_modified = False
  
  'Create "it" file when user has specified a fixed surface temperature
  'input to the combustion CFD program
  If surf_type = 0 Then
    s0 = case_path & "\it" & case_number & "t.dat"
    Open s0 For Output As #4
    Print #4, "0  Distribution indicator"
    'Print #4, "0  Total Heat (W)"
    Print #4, default_q_melt_req & "  Energy required by melt (W)"
    Print #4, "0  Heat Transfer to Melter (W)"
    Print #4, surf_temp & "  Mean Surface Temperature (K)"
    Print #4, "  "
    Print #4, "  This is the default file for a fixed surface temperature."
    Close (4)
  End If
  Exit Sub
  
  
fss1: 'print pressure reference point
  i0 = Int((i1 + i2) / 4) * 2
  j0 = Int((j1 + j2) / 4) * 2
  k0 = Int((k1 + k2) / 4) * 2
  s0 = " " & i0 & " " & j0 & " " & k0 & " 0.0"
  Print #4, s0
  Return

fss3: 'set burner inlet coordinates and boundary conditions, print inlet block(s)
  If bty(n) = 1 Then GoSub fss4
                'ring: create and print inlet block for center
  If bty(n) = 3 Then GoSub fss4b
                'pipe-in-pipe:
                'create and print inlet block for reduced part of this burner
  If bty(n) = 4 Then GoSub fss5 ' 10-21-2004
                'regenerative burner:
                'create and print inlet block for inner part of this burner
               
  'create and print inlet block for main part of this burner
  s0 = " " & i1 & " " & j1 & " " & k1
  s0 = s0 & " " & i2 & " " & j2 & " " & k2
  Print #4, s0 & s1 'burner coordinates and direction
  's0 = Format(btg(n, 1), " 0")
  s0 = " " & btg(n, 1)
  g1 = gfg(n, 1) + gfo(n, 1) + gfn(n, 1)
  If g1 <= 0 Then
    s0 = s0 & " 0. 0.21 0.79 0. 0. 0."
  Else
    'g0 = gfg(n, 1) / g1: s0 = s0 & Format(g0, " 0.###")
    'g0 = gfo(n, 1) / g1: s0 = s0 & Format(g0, " 0.###")
    'g0 = gfn(n, 1) / g1: s0 = s0 & Format(g0, " 0.###")
    g0 = gfg(n, 1) / g1: s0 = s0 & " " & g0
    g0 = gfo(n, 1) / g1: s0 = s0 & " " & g0
    g0 = gfn(n, 1) / g1: s0 = s0 & " " & g0
    s0 = s0 & " 0. 0. 0."
  End If
  
  'get a0 = inlet area
  a0 = 0
  For i = i1 To i2 Step 2
  For j = j1 To j2 Step 2
  For k = k1 To k2 Step 2
    GoSub fss3a 'get single cell face area in g0
    a0 = a0 + g0
  Next: Next: Next
  If bty(n) = 3 Or bty(n) = 4 Then a0 = a0 - a0b 'subtract inner area from total
  
  'g0 = bfg(n, 1) + bfo(n, 1) + bfn(n, 1)             'NOTE bf_ was m^3/s, now some are not
  'v0 = g0 / a0 * btg(n, 1) / T_ref * P_ref / pg0
  If feed_unit = "m^3/s" Then
        sum_flow_vol = bfg(n, 1) + bfo(n, 1) + bfn(n, 1)
        v0 = sum_flow_vol / a0 * btg(n, 1) / T_ref * P_ref / pg0
  Else 'have "kg/s" so need to convert
        temp_bfg = bfg(n, 1) / rho_fuel_ref
        temp_bfo = bfo(n, 1) / rho_O2_ref
        temp_bfn = bfn(n, 1) / rho_N2_ref
        sum_flow_vol = temp_bfg + temp_bfo + temp_bfn
        v0 = sum_flow_vol / a0 * btg(n, 1) / T_ref * P_ref / pg0
  End If
  GoSub fss3b
  Return

fss3a: 'get area of inlet cell face in g0
  g0 = 0
  If ibc(i, j, k) <> 2 Then Return
  If m = 0 Or m = 1 Then
    g0 = (yg(j + 1) - yg(j - 1)) * (zg(k + 1) - zg(k - 1))
  ElseIf m = 2 Or m = 3 Then
    g0 = (xg(i + 1) - xg(i - 1)) * (zg(k + 1) - zg(k - 1))
  Else
    g0 = (xg(i + 1) - xg(i - 1)) * (yg(j + 1) - yg(j - 1))
  End If
  Return

fss3b: 'Determine and add velocities to end of s0, and print conditions
  g1 = bav(n, 1) * pi / 180
  g1 = Tan(g1)
  g2 = bah(n, 1) * pi / 180
  g2 = Tan(g2)
  If m = 0 Then
    v1 = v0: v2 = -v0 * g2: v3 = v0 * g1
  ElseIf m = 1 Then
    v1 = -v0: v2 = v0 * g2: v3 = v0 * g1
  ElseIf m = 2 Then
    v1 = v0 * g2: v2 = v0: v3 = v0 * g1
  ElseIf m = 3 Then
    v1 = -v0 * g2: v2 = -v0: v3 = v0 * g1
  ElseIf m = 4 Then
    v1 = v0 * g1: v2 = v0 * g2: v3 = v0
  Else
    v1 = v0 * g1: v2 = -v0 * g2: v3 = -v0
  End If
  's0 = s0 & " " & Format(v1, "0.## ") & Format(v2, "0.## ") _
  '& Format(v3, "0.##")
  s0 = s0 & " " & v1 & " " & v2 & " " & v3
  Print #4, s0
  Return

fss4: 'ring: calc position and conditions for one center cell ???
  'also print inlet block
  i = Int((i1 + i2) / 4) * 2
  j = Int((j1 + j2) / 4) * 2
  k = Int((k1 + k2) / 4) * 2
  s0 = " " & i & " " & j & " " & k
  s0 = s0 & " " & i & " " & j & " " & k
  Print #4, s0 & s1
  's0 = Format(btg(n, 1), " 0")
  s0 = " " & btg(n, 1)
  g1 = gfg(n, 2) + gfo(n, 2) + gfn(n, 2)
  If g1 <= 0 Then
    s0 = s0 & " 0. 0.21 0.79 0. 0. 0."
  Else
    'g0 = gfg(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    'g0 = gfo(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    'g0 = gfn(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    g0 = gfg(n, 2) / g1: s0 = s0 & " " & g0
    g0 = gfo(n, 2) / g1: s0 = s0 & " " & g0
    g0 = gfn(n, 2) / g1: s0 = s0 & " " & g0
    s0 = s0 & " 0. 0. 0."
  End If
  GoSub fss3a: a0 = g0
  ibc(i, j, k) = 12
  'g0 = bfg(n, 2) + bfo(n, 2) + bfn(n, 2)
  'v0 = g0 / a0 * btg(n, 1) / T_ref * P_ref / pg0
  If feed_unit = "m^3/s" Then
        sum_flow_vol = bfg(n, 2) + bfo(n, 2) + bfn(n, 2)
        v0 = sum_flow_vol / a0 * btg(n, 1) / T_ref * P_ref / pg0
  Else 'have "kg/s" so need to convert
        temp_bfg = bfg(n, 2) / rho_fuel_ref
        temp_bfo = bfo(n, 2) / rho_O2_ref
        temp_bfn = bfn(n, 2) / rho_N2_ref
        sum_flow_vol = temp_bfg + temp_bfo + temp_bfn
        v0 = sum_flow_vol / a0 * btg(n, 1) / T_ref * P_ref / pg0
  End If
  
  GoSub fss3b
  gfg(n, 1) = 0 'ensure no gas in ring center ?
  Return

fss4b: 'pipe-in-pipe
  'setup and print inlet block for burner reduced in size by 2 on each side
  'also set a0b = reduced area
  i1b = i1: i2b = i2
  j1b = j1: j2b = j2
  k1b = k1: k2b = k2
  If i2 > i1 Then i1b = i1 + 2: i2b = i2 - 2
  If j2 > j1 Then j1b = j1 + 2: j2b = j2 - 2
  If k2 > k1 Then k1b = k1 + 2: k2b = k2 - 2
  s0 = " " & i1b & " " & j1b & " " & k1b
  s0 = s0 & " " & i2b & " " & j2b & " " & k2b
  Print #4, s0 & s1
  's0 = Format(btg(n, 1), " 0")
  s0 = " " & btg(n, 1)
  g1 = gfg(n, 2) + gfo(n, 2) + gfn(n, 2)
  If g1 <= 0 Then
    s0 = s0 & " 0. 0.21 0.79 0. 0. 0."
  Else
    'g0 = gfg(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    'g0 = gfo(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    'g0 = gfn(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    g0 = gfg(n, 2) / g1: s0 = s0 & " " & g0
    g0 = gfo(n, 2) / g1: s0 = s0 & " " & g0
    g0 = gfn(n, 2) / g1: s0 = s0 & " " & g0
    s0 = s0 & " 0. 0. 0."
  End If
  a0 = 0
  For i = i1b To i2b Step 2
  For j = j1b To j2b Step 2
  For k = k1b To k2b Step 2
    GoSub fss3a: a0 = a0 + g0
  Next: Next: Next
  'g0 = bfg(n, 2) + bfo(n, 2) + bfn(n, 2)
  'v0 = g0 / a0 * btg(n, 1) / T_ref * P_ref / pg0
  If feed_unit = "m^3/s" Then
        sum_flow_vol = bfg(n, 2) + bfo(n, 2) + bfn(n, 2)
        v0 = sum_flow_vol / a0 * btg(n, 1) / T_ref * P_ref / pg0
  Else 'have "kg/s" so need to convert
        temp_bfg = bfg(n, 2) / rho_fuel_ref
        temp_bfo = bfo(n, 2) / rho_O2_ref
        temp_bfn = bfn(n, 2) / rho_N2_ref
        sum_flow_vol = temp_bfg + temp_bfo + temp_bfn
        v0 = sum_flow_vol / a0 * btg(n, 1) / T_ref * P_ref / pg0
  End If
  GoSub fss3b: a0b = a0
  Return

fss5: ' 10-21-2004
    'Regenerative burner type 4, n=burner #, s1=direction, m=orientation.
    'Create and print inlet block for inner part of this burner.
    
    ' 03-01-2006  burner type 4 is not fully supported, do not use.
                        'corrections are not done in this section
  i1b = bi1_in(n): i2b = bi2_in(n)
  j1b = bj1_in(n): j2b = bj2_in(n)
  k1b = bk1_in(n): k2b = bk2_in(n)
  s0 = " " & i1b & " " & j1b & " " & k1b
  s0 = s0 & " " & i2b & " " & j2b & " " & k2b
  Print #4, s0 & s1
  'create 2nd. line of inlet block
  s0 = Format(btg(n, 2), " 0")
  g1 = gfg(n, 2) + gfo(n, 2) + gfn(n, 2)
  If g1 <= 0 Then
    s0 = s0 & " 0. 0.21 0.79 0. 0. 0."
  Else
    g0 = gfg(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    g0 = gfo(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    g0 = gfn(n, 2) / g1: s0 = s0 & Format(g0, " 0.###")
    s0 = s0 & " 0. 0. 0."
  End If
  'get inlet area in a0
  a0 = 0
  For i = i1b To i2b Step 2
  For j = j1b To j2b Step 2
  For k = k1b To k2b Step 2
    GoSub fss3a 'get cell face area
    a0 = a0 + g0
  Next: Next: Next
  a0b = a0 'save inner area to later subtract from total area
  
  g0 = bfg(n, 2) + bfo(n, 2) + bfn(n, 2)
  v0 = g0 / a0 * btg(n, 2) / T_ref * P_ref / pg0
  'Determine and add velocities to end of s0
  g1 = bav(n, 2) * pi / 180
  g1 = Tan(g1)
  g2 = bah(n, 2) * pi / 180
  g2 = Tan(g2)
  If m = 0 Then
    v1 = v0: v2 = -v0 * g2: v3 = v0 * g1
  ElseIf m = 1 Then
    v1 = -v0: v2 = v0 * g2: v3 = v0 * g1
  ElseIf m = 2 Then
    v1 = v0 * g2: v2 = v0: v3 = v0 * g1
  ElseIf m = 3 Then
    v1 = -v0 * g2: v2 = -v0: v3 = v0 * g1
  ElseIf m = 4 Then
    v1 = v0 * g1: v2 = v0 * g2: v3 = v0
  Else
    v1 = v0 * g1: v2 = -v0 * g2: v3 = -v0
  End If
  s0 = s0 & " " & Format(v1, "0.## ") & Format(v2, "0.## ") _
  & Format(v3, "0.##")
  Print #4, s0
  Return

  '-----------------------------
  '  Melter section of Sbc save
  '-----------------------------

fssm: 'for melter
  'Begin creation of the melter conditions file
  Open fnm_sbc_path For Output As #4
  Print #4, nphas & "    nphas"
  fr_s = 0: fr_c = 0
  For j = 1 To nbr
    g0 = bfg(j, 1): g2 = g0 * bfg(j, 2): g1 = g0 - g2
    fr_s = fr_s + g1: fr_c = fr_c + g2
  Next
  If fr_c <= 0 Then n1 = 0 Else n1 = nps0_c
  If fr_s <= 0 Then n2 = 0 Else n2 = nps0_s
  If nphas < 3 Then n3 = 0 Else n3 = nbs0
  s0 = n1 & " " & n2 & " " & n3
  Print #4, s0 & "   (nps_c,nps_s,nbs0)"
  'rlm0 = 0.001
  'rlm0 = 1#
  If n1 > 0 Then
    s0 = "": Print #4, tm_c & " (tm_c)"
    For i = 1 To nps0_c: g0 = bah(i, 1) * 0.000001
    g0 = Format(g0, "0.#######E-##"): s0 = s0 & g0 & " ": Next
    Print #4, s0 & " (rp_c)"
  End If
  If n2 > 0 Then
    s0 = "": Print #4, tm_s & " (tm_s)"
    For i = 1 To nps0_s: g0 = bav(i, 1) * 0.000001
    'g0 = Format(g0, "0.#######E-##"): s0 = s0 & g0 & " ": Next
    g0 = Format(g0, Scientific): s0 = s0 & g0 & " ": Next
    Print #4, s0 & " (rp_s)"
  End If
  If n3 > 0 Then
    s0 = ""
    For i = 1 To nbs0: g0 = rg_b(i) * 0.000001
    g0 = Format(g0, "0.#######E-##"): s0 = s0 & g0 & " ": Next
    Print #4, s0 & " (rg_b)"
  End If
  Print #4, "&INPUT"
  s0 = "   PG0=" & pg0
  Print #4, s0
  s0 = "   IRSTYP=" & irstyp
  s0 = s0 & ",BGCON=" & Format(bgcon, "0.###E-##")
  s0 = s0 & ",ID_RAD=" & id_rad
  'If id_rad >= 0 Then s0 = s0 & ",LRX=" &interval_rad
  If id_rad >= 0 Then s0 = s0 & ",LRX=200" 'fixed value for now, radiation is not done for melt
  Print #4, s0
  Print #4, "   MAXGI=1,MAXPI=1,MAXBI=1,MAXSI=" & maxgi
  'Print #4, "   MS=0,IRSTYPM=0,NMSP=1"
  s0 = "   MS=0,IRSTYPM=" & irstyp & ",NMSP=1"
  Print #4, s0
  's0 = "   DS_C=" & ds_c & ",CL_C=" & cl_c & ",H0_C=" & h0_c
  s0 = "   DS_C=" & ds_c & ",H0_C=" & h0_c
  Print #4, s0
  s0 = "   DS_S=" & ds_s & ",H0_S=" & h0_s
  Print #4, s0
  s0 = "   W_D=" & wa_d(1) & ",W_K=" & wa_k(1)
  s0 = s0 & ",W_HA=" & wa_h(1) & ",W_TA=" & wa_ta(1)
  Print #4, s0 & ",W_E=" & wa_e(1)
  Print #4, "   cd_d_c=" & condLength_c & ",cd_d_s=" & condLength_s
  Print #4, "   iheat_flux_type=" & heat_flux_type
  Print #4, "   T_exit=" & glassExitTemp
  Print #4, "   T_init=" & start_temp
  Print #4, "   cycling=0"
  'Defaulting to not doing automatic cycling
  'Will change sbc file if/when know cycling is being done
  Print #4, "   gui_update=" & gui_update
  Print #4, "   chamber_type=" & chamber_type
  If chamber_type = 2 Then
    Print #4, "   melter_length=" & lth_r(1)
  Else
    Print #4, "   melter_length=" & lth
  End If
  
    'Melt data collection control flags may be set to 0 or 1
    Print #4, "   isum=" & isum_m
    Print #4, "   iinfo=" & iinfo_m
    Print #4, "   iTave=" & iTave_m
    Print #4, "   iconv=" & iconv_m
    Print #4, "   igresid=" & igresid_m
    Print #4, "   igresidp=" & igresidp_m
    Print #4, "   iTchg=" & iTchg
    Print #4, "   iadjf=" & iadjf
    Print #4, "   iadjr=" & iadjr
    Print #4, "   ifieldview=" & ifieldview_m
  Print #4, "/"
  
  
  Print #4, "Wall Properties:"
  For n = 2 To nwal
    m = wa_dr(n)
    i1 = wa_i1(n): i2 = wa_i2(n)
    j1 = wa_j1(n): j2 = wa_j2(n)
    k1 = wa_k1(n): k2 = wa_k2(n)
    GoSub fssm2
    s0 = " " & wa_d(n) & " " & wa_k(n) & " " & wa_h(n)
    Print #4, s0 & " " & wa_ta(n) & " " & wa_e(n)
  Next
  
  Print #4, "T(K) : Glass Density (kg/m**3):"
  For n = 1 To udf_ds_n
    'Print #4, Format(udf_ds_t(n), " 0") _
    '  & Format(udf_ds(n), " 0.0")
    Print #4, " " & udf_ds_t(n) & " " & udf_ds(n)
    Next
  Print #4, "T(K) : Glass Viscosity (Pa-s):"
  For n = 1 To udf_mu_n
    'Print #4, Format(udf_mu_t(n), " 0") _
    '  & Format(udf_mu(n), " 0.0")
    Print #4, " " & udf_mu_t(n) & " " & udf_mu(n)
  Next
  Print #4, "T(K) : Glass Conductivity (W/m/K):"
  For n = 1 To udf_k_n
    'Print #4, Format(udf_k_t(n), " 0") _
    '  & Format(udf_k(n), " 0.00")
    Print #4, " " & udf_k_t(n) & " " & udf_k(n)
  Next
  Print #4, "T(K) : Liquid Specific Heat (J/(kg K)):"
  For n = 1 To udf_cl_n
    'Print #4, Format(udf_cl_t(n), " 0") _
    '  & Format(udf_cl(n), " 0.00")
    Print #4, " " & udf_cl_t(n) & " " & udf_cl(n)
  Next
  Print #4, "WL(um) : Vol Absorp (1/cm):"
  For n = 1 To udf_a_n
    'Print #4, Format(udf_a_m(n), " 0.##") _
    '  & Format(udf_a(n), " 0.0##")
    Print #4, " " & udf_a_m(n) & " " & udf_a(n)
  Next
  Print #4, "T(K) : Cullet Specific Heat (J/(kg K)):"
  For n = 1 To udf_clc_n
    Print #4, " " & udf_clc_t(n) & " " & udf_clc(n)
  Next
  Print #4, "T(K) : Batch Specific Heat (J/(kg K)):"
  For n = 1 To udf_cls_n
    Print #4, " " & udf_cls_t(n) & " " & udf_cls(n)
  Next
  
  Print #4, "Charger Flow:"
  t0 = tm_c
  If tm_s > t0 Then t0 = tm_s
  'ug0 = 0.003
  For n = 1 To nbr
    ug0 = batch_velocity(n)
    m = bdr(n)
    i1 = bi1(n): i2 = bi2(n): dx = xg(i2 + 1) - xg(i1 - 1)
    j1 = bj1(n): j2 = bj2(n): dy = yg(j2 + 1) - yg(j1 - 1)
    k1 = bk1(n): k2 = bk2(n): dz = zg(k2 + 1) - zg(k1 - 1)
    If m = 0 Then
      s1 = " 1": a0 = dy * dz   ' s2 = " 0.003 0 0"
      s2 = " " & batch_velocity(n) & " 0 0"
    ElseIf m = 1 Then
      s1 = " 1": a0 = dy * dz   ' s2 = " -0.003 0 0"
      s2 = " -" & batch_velocity(n) & " 0 0"
    ElseIf m = 2 Then
      s1 = " 2": a0 = dx * dz   ' s2 = " 0 0.003 0"
      s2 = " 0 " & batch_velocity(n) & " 0"
    ElseIf m = 3 Then
      s1 = " 2": a0 = dx * dz   ' s2 = " 0 -0.003 0"
      s2 = " 0 -" & batch_velocity(n) & " 0"
    ElseIf m = 4 Then
      s1 = " 3": a0 = dx * dy   ' s2 = " 0 0 0.003"
      s2 = " 0 0 " & batch_velocity(n)
    Else
      s1 = " 3": a0 = dx * dy   ' s2 = " 0 0 -0.003"
      s2 = " 0 0 -" & batch_velocity(n)
    End If
    If n = 1 Then GoSub fss1 'get pressure reference cell & value
    s0 = " " & i1 & " " & j1 & " " & k1 & "  "
    s0 = s0 & i2 & " " & j2 & " " & k2 & s1
    Print #4, s0
    s0 = " " & t0 & " 0 0 0 1 0" 'the "1 0" does not appear to be read in
    Print #4, s0
    g0 = bfg(n, 1): g2 = g0 * bfg(n, 2): g1 = g0 - g2
    If n1 > 0 Then 'have cullet
      fi = g2 / nps0_c
      For i = 1 To nps0_c
        vol = 4 / 3 * pi * (bah(i, 1) * 0.000001) ^ 3
        dn = fi / vol / ug0 / a0 / ds_c
      Next
      s0 = " " & btg(n, 1) & Format(dn, " 0.###############E+") & s2
      Print #4, s0
    End If
    If n2 > 0 Then 'have sand
      fi = g1 / nps0_s
      For i = 1 To nps0_s
        vol = 4 / 3 * pi * (bav(i, 1) * 0.000001) ^ 3
        dn = fi / vol / ug0 / a0 / ds_s
      Next
      s0 = " " & btg(n, 1) & Format(dn, " 0.###############E+") & s2
      Print #4, s0
    End If
    If n3 > 0 Then 'have bubbles?
      Print #4, " " & btg(n, 1) & " 0 0 0 0"
    End If
  Next
  
  Print #4, "Exit pull rates"
  g0 = 0
  For n = 1 To nex: g0 = g0 + epull(n): Next
  If g0 < 0.01 Then
    For n = 1 To nex: epull(n) = 1 / nex: Next
  Else
    For n = 1 To nex: epull(n) = epull(n) / g0: Next
  End If
  For n = 1 To nex
    m = edr(n)
    i1 = ei1(n): i2 = ei2(n)
    j1 = ej1(n): j2 = ej2(n)
    k1 = ek1(n): k2 = ek2(n)
    GoSub fssm1
    Print #4, " " & epull(n)
  Next
  Print #4, "Bubbler Flow:"
  For n = 1 To nsw
    m = wdr(n)
    i1 = swi1(n): i2 = swi2(n)
    j1 = swj1(n): j2 = swj2(n)
    k1 = swk1(n): k2 = swk2(n)
    GoSub fssm1
    Print #4, " " & wtg(n) & " " & wfa(n)
  Next
  Print #4, "Electric Booster:"
  For n = 1 To neb
    For j = 1 To 3
    m = ebdr(n, j)
    If ebty(n) < 3 And j = 3 Then Exit For
    i1 = ebi1(n, j): i2 = ebi2(n, j)
    j1 = ebj1(n, j): j2 = ebj2(n, j)
    k1 = ebk1(n, j): k2 = ebk2(n, j)
    GoSub fssm1
    If j = 1 Then _
    Print #4, " " & ebty(n) & " " & ebvt(n) & " " & _
    ebpw(n) - ebhl(n)
  Next: Next
  Print #4, "END"
  flag_conditions_saved = True
  flag_conditions_modified = False
  Close (4)
  
  'Create "it" file when user has specified a fixed heat flux to be input to the melt surface
  If heat_flux_type = 1 Or heat_flux_type = 3 Then
    s0 = case_path & "\it" & case_number & "m.dat"
    Open s0 For Output As #4
    Print #4, "0  Distribution indicator"
    Print #4, "0  Mean Wall Temperature in Combustion Space (K)"
    Print #4, "0  Maximum Temperature in Combustion Space (K)"
    g0 = qsh * 1000000#
    Print #4, g0 & "  Heat Transfer to Melter (W)"
    Close (4)
  End If
  Exit Sub

fssm1:
  If m = 0 Or m = 1 Then
    s1 = " 1"
  ElseIf m = 2 Or m = 3 Then
    s1 = " 2"
  Else
    s1 = " 3"
  End If
  s0 = " " & i1 & " " & j1 & " " & k1 & "  "
  s0 = s0 & i2 & " " & j2 & " " & k2 & s1
  Print #4, s0
  Return

fssm2:
  If m = 0 Then
    s1 = " 1"
  ElseIf m = 1 Then
    s1 = " -1"
  ElseIf m = 2 Then
    s1 = " 2"
  ElseIf m = 3 Then
    s1 = " -2"
  ElseIf m = 4 Then
    s1 = " 3"
  Else
    s1 = " -3"
  End If
  s0 = " " & i1 & " " & j1 & " " & k1 & "  "
  s0 = s0 & i2 & " " & j2 & " " & k2 & s1
  Print #4, s0
  Return

fss_rad:
  'Add RADIN namelist to SBC file
  Print #4, "&RADIN"
  s0 = "   NWL=" & nwl & ",T_A=" & wa_ta(1)
  s0 = s0 & ",W_D=" & wa_d(1) & ",RI=1"
  's0 = s0 & ",ID_RAD=" & id_rad
  Print #4, s0
  s0 = "   H_A=" & wa_h(1) & ",H_G=10" 'internal wall heat transfer coefficient is hard coded here @@@
  s0 = s0 & ",W_K=" & Format(wa_k(1), "0.##") & ",eps_w=" & wa_e(1)
  Print #4, s0
  Print #4, "   maxri2=" & maxri2
  Print #4, "   maxri1=" & maxri1
  Print #4, "   minri2=" & minri2
  Print #4, "   preset_vf=" & preset_vf
  Print #4, "   eps_m=" & eps_m
  Print #4, "   eps_c=" & eps_c
  If hgt_c = 0 Then
    have_crown = 0
  Else
    have_crown = 1
  End If
  Print #4, "   have_crown=" & have_crown
  Print #4, "   height_to_ceiling=" & hgt
  Print #4, "/"
  Return
End Sub


'-------------------------------
'Control what happens after a drag operation is completed
'for the figure or grid handles
'-------------------------------
Private Sub Form_DragDrop(Source As Control, x As Single, y As Single)
  s0 = LCase(frmMain.Caption)
  If Source.Name = "LbO" Then
    GoSub fdd1
  ElseIf Source.Name = "LbP" Then
    GoSub fdd2
  End If
  Exit Sub

fdd1:
  If InStr(s0, "grid") > 0 Then
    GoSub fdd1a: Call plt_gd(0)
  ElseIf InStr(s0, "post") > 0 Then
    GoSub fdd1a: Call plt_clr(0)
  ElseIf InStr(s0, "pre-pro") > 0 Then
    Source.Move x, y
    p0 = x + gap: q0 = y - gap: plt_cb
  End If
Return

fdd1a:
  Source.Move x, y
  If Lb3d.Caption = "3D" Then
    p0 = x: q0 = y
  Else
    x0 = xg(nxb(1)): y0 = yg(nxb(2))
    z0 = zg(nxb(3)): proj2d
    p1 = p1 - gap: q1 = q1 + gap
    p0 = p0 + (x - p1): q0 = q0 + (y - q1)
  End If
Return

fdd2:
  If InStr(s0, "grid") > 0 Then
    GoSub fdd2a: Call plt_gd(0)
  ElseIf InStr(s0, "post") > 0 Then
    GoSub fdd2a: Call plt_clr(0)
  ElseIf InStr(s0, "pre-pro") > 0 Then
    GoSub fdd2b
  End If
  Return

fdd2a:
  p1 = x - gap: q1 = y + gap
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  dx = xg(i2) - xg(i1)
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  dy = yg(j2) - yg(j1)
  If Lb3d.Caption = "3D" Then
    GoSub fdd2a1
  Else
    GoSub fdd2a2
  End If
  Return
  
fdd2a1:
  g0 = (p1 - p0 - gap) / dx
  If g0 <= 0 Then Exit Sub
  zf1 = g0
  g0 = (q0 - gap - q1) / dy
  If g0 <= 0 Then Exit Sub
  g0 = g0 / zf2
  zf2 = zf2 * g0: zf3 = zf3 * g0
  Return

fdd2a2:
  k1 = nxb(3) - 1: k2 = nxe(3) + 1
  dz = zg(k2) - zg(k1)
  GoSub fdd2a2a
  Return

fdd2a2a:
  g0 = (p1 - p0) / (dx + dy * Sin(theta))
  If g0 <= 0 Then Exit Sub
  zf1 = g0: zf2 = g0
  g0 = q0 - q1 - dy * zf2 * Cos(theta)
  If g0 <= 0 Then Exit Sub
  zf3 = g0 / dz
  Return

fdd2b:
  p1 = x - gap: q1 = y + gap
  dx = lth: dy = wdt: dz = hgt
  GoSub fdd2a2a
  plt_cb
  Return
End Sub

'-------------------------------
'Begin the program.  Display welcome screen.
'-------------------------------
Private Sub Form_Load()

  'OK to proceed
  frmMain.WindowState = 2 'Maximize the main window
  gap = 200
  dnm = App.Path & "\": ChDir dnm 'Set current directory
  'set executable directory at compile time, 7-25-04
  #If DebugVersion = 1 Then
    exe_dir = "debug"
  #Else
    exe_dir = "release"
  #End If
  'end
    
  optplt.Visible = False
  mnuView.Visible = False ' 8-5-2004
  gridProtectMode = False
  mnuProtectGrid.Checked = False
  mnuProtectGrid.Visible = False
  MnuOptCol.Visible = False
  vwr = 10
  unt = "SI": opt11.Checked = False: opt12.Checked = True
  wrd = "c:\program files\microsoft office\office\winword "
  sweep = False: m_sw = 2
  dub0 = 1000000#
  avail_dub0 = 100000#
  List1.BackColor = &HC0FFC0
  List2.BackColor = &HC0FFC0
  List3.BackColor = &HC0FFC0
  List4.BackColor = &HC0FFC0
  zf1 = 300: zf2 = zf1: zf3 = zf1
  pg0 = P_ref: irstyp = 0: maxgi = default_interval_rad: nphas = 1
  qh0 = default_qh0: id_rad = 1: vv0 = 100
  bgcon = 0.000000000001: interval_rad = default_interval_rad
  ReDim wa_d(1), wa_k(1), wa_h(1), wa_ta(1), wa_e(1)
  ReDim wa_sg(1), wa_bg(1), wa_wd(1), wa_ht(1), wa_dr(1)
  nwal = 1: iwal = 1
  wa_k(1) = 2: wa_h(1) = 5: wa_ta(1) = T_ref
  wa_d(1) = 0.31: wa_dr(1) = 1: wa_e(1) = 0.95
  wa_sg(1) = 0: wa_bg(1) = 0: wa_wd(1) = 0: wa_ht(1) = 0
  hgts = Screen.Height - 1300
  wdts = Screen.Width
  s0 = dnm & "tmp"
  If Dir$(s0, vbDirectory) = "" Then MkDir s0
  cancelMode = False
  
  Call display_welcome_screen
  
  'initialize status flags ' 03-15-2005
  flag_save_as = False
  full_save_as = False
  flag_preproc_modified = False
  flag_preproc_open = False
  flag_preproc_saved = False
  flag_grid_modified = False
  flag_grid_active = False
  flag_grid_saved = False
  flag_conditions_modified = False
  flag_conditions_saved = False
  flag_grid_enhanced = False
  menu_layer = InMain
  
  'initialize case description label
  LbCase.Top = 0
  LbCaseTitle.Top = 0
  LbCaseTitle.Width = wdts - LbCase.Width - Lb14.Width - LbSoot.Width - 500
  LbSoot.Top = 0: LbSoot.Width = 3252
  LbSoot.Left = wdts - LbSoot.Width - Lb14.Width: LbSoot.Visible = False
  LbCase.Caption = ""
  LbCase.Visible = False
  LbCaseTitle.Caption = ""
  LbCaseTitle.Visible = False
  mnuCaseDescript.Enabled = False
  mnuCaseDescript.Visible = False
  
End Sub

'-------------------------------
'-------------------------------
Private Sub grid(n_gd)
  'n_gd=0 => new grid
  Dim i As Integer, i1 As Integer, i2 As Integer
  Dim n As Integer, j As Integer, k As Integer
  Dim mp1 As Integer
  Dim c0 As Single, c1 As Single, da0 As Single
  Dim xl As Single, xgb0() As Single
  Dim xgb1() As Single, xgb2() As Single
  Dim ygb1() As Single, zgb1() As Single
  Dim inorout As Integer '1=outer section of regenerative burner, 2=inner section
  Dim in_tunnel As Boolean
  
  For i = 1 To nbr
    'For all burners of type round or ring the side and bottom gaps
    'are at the burner center.  Alter the gaps to be at ends.
    If flow_domain = 1 And bty(i) <= 1 Then
    'If bty(i) <= 1 Then
      bsg(i, 1) = bsg(i, 1) - bwd(i, 1) / 2
      bbg(i, 1) = bbg(i, 1) - bwd(i, 1) / 2
    End If
  Next
  If n_gd > 0 Then GoTo gd01 'If not a new grid skip over basic grid construction
  GoSub gdr1 'Add grid lines for the furnace boundaries and object boundaries

  'At this point xgb1() has x positions for lines that are furnace and object boundaries.
  'ygb1() has y positions, zgb1() has z positions
  
  'Fill in lines for all grid cells
  eps0 = eps0 / 2
  da0 = dx0: n = n1
  If chamber_type = 2 Then x1a = gx_r(2): x1b = x1a + lth_r(2)
  ix1 = 1: GoSub gdr2 'get all grid lines and cell centers for x direction into xg1()
  mp = m - 1: mx = mp / 2
  ReDim xg(m): For i = 1 To m: xg(i) = xg1(i): Next
  
  'Reuse xgb1() for y and z directions so can use same subroutine
  ReDim xgb1(n2): For i = 0 To n2: xgb1(i) = ygb1(i): Next
  da0 = dy0: n = n2
  If chamber_type = 2 Then x1a = gy_r(2): x1b = x1a + wdt_r(2)
  ix1 = 2: GoSub gdr2
  np = m - 1: nx = np / 2
  ReDim yg(m): For i = 1 To m: yg(i) = xg1(i): Next
  
  ReDim xgb1(n3): For i = 0 To n3: xgb1(i) = zgb1(i): Next
  da0 = dz0: n = n3
  If chamber_type = 2 Then x1a = gz_r(2): x1b = x1a + hgt_r(2)
  ix1 = 3: GoSub gdr2
  lp = m - 1: lx = lp / 2
  ReDim zg(m): For i = 1 To m: zg(i) = xg1(i): Next
  
  ReDim ibc(mp, np, lp)
  GoSub gdr3 'assign either wall or open cell type values to ibc array


gd01:
    'Mark objects so ibc array has correct cell types
    'Note: if grid file has not been created/opened yet, but n_gd was 1,
    'then will get run errors because mp, np, lp have not been set!

  'exhaust
  'Positioning determined by closest existing grid lines
  ReDim ei1(nex), ei2(nex), ej1(nex), ej2(nex)
  ReDim ek1(nex), ek2(nex)
  For n = 1 To nex 'for all exhausts
    ii = edr(n) 'orientation
    If ii <= 1 Then 'on back or front wall
      GoSub gdr4a
    ElseIf ii <= 3 Then 'on right or left wall
      GoSub gdr4c
    Else 'on bottom or top wall
      GoSub gdr4e
    End If
  Next
  
'burner
  ReDim bi1(nbr), bi2(nbr), bj1(nbr), bj2(nbr)
  ReDim bk1(nbr), bk2(nbr)
  ' 10-21-2004 Add ability to support burner type 4 inner settings
  ReDim bi1_in(nbr), bi2_in(nbr), bj1_in(nbr), bj2_in(nbr)
  ReDim bk1_in(nbr), bk2_in(nbr)
  For n = 1 To nbr
    If bty(n) = 4 Then
        'have a tube-in-tube type burner, do inner part first
        bsg0 = bsg(n, 2): bbg0 = bbg(n, 2)
        bwd0 = bwd(n, 2): r0 = bwd0 / 2
        bht0 = bht(n, 2)
        ii = bdr(n)
        If ii = 0 Then GoSub gdr5a 'back wall
        If ii = 1 Then GoSub gdr5b 'front wall
        If ii = 2 Then GoSub gdr5c 'right wall
        If ii = 3 Then GoSub gdr5d 'left wall
        If ii = 4 Then GoSub gdr5e 'bottom wall
        If ii = 5 Then GoSub gdr5f 'top wall
        'save inner burner coordinates in b.._in arrays
        bi1_in(n) = bi1(n)
        bi2_in(n) = bi2(n)
        bj1_in(n) = bj1(n)
        bj2_in(n) = bj2(n)
        bk1_in(n) = bk1(n)
        bk2_in(n) = bk2(n)
    End If
    bsg0 = bsg(n, 1): bbg0 = bbg(n, 1)
    bwd0 = bwd(n, 1): r0 = bwd0 / 2
    If bty(n) <= 1 Then bht0 = bwd0 Else bht0 = bht(n, 1)
    ii = bdr(n)
    If ii = 0 Then GoSub gdr5a 'back wall
    If ii = 1 Then GoSub gdr5b 'front wall
    If ii = 2 Then GoSub gdr5c 'right wall
    If ii = 3 Then GoSub gdr5d 'left wall
    If ii = 4 Then GoSub gdr5e 'bottom wall
    If ii = 5 Then GoSub gdr5f 'top wall
  Next
  
'dog house
  If flow_domain = 1 Then
    For n = 1 To nsw
        If wdr(n) < 2 Then
            g1 = wsg(n): g2 = g1 + wwd(n): GoSub gds5y
            n1j = n1: n2j = n2
            If wdr(n) = 0 Then g1 = -wdp(n): g2 = 0 _
            Else g1 = lth: g2 = lth + wdp(n)
            GoSub gds5x: n1i = n1: n2i = n2
            g1 = 0: g2 = hgt: GoSub gds5z
            n1k = n1: n2k = n2
            g0 = zg(n2k + 1) - zg(n1k - 1)
            For k = n1k To n2k
                g1 = zg(n2k + 1) - zg(k)
                d0 = wdp(n) * g1 / g0
                For i = n1i To n2i
                    If wdr(n) = 0 Then d1 = xg(n2i + 1) - xg(i + 1) _
                    Else d1 = xg(i - 1) - lth
                    For j = n1j To n2j
                        If d0 > d1 Then ibc(i, j, k) = 0
                        If k = n1k Then ibc(i, j, k - 2) = 4
                    Next
                Next
            Next
        Else
            g1 = wsg(n): g2 = g1 + wwd(n): GoSub gds5x
            n1i = n1: n2i = n2
            If wdr(n) = 2 Then g1 = -wdp(n): g2 = 0 _
            Else g1 = wdt: g2 = wdt + wdp(n)
            GoSub gds5y: n1j = n1: n2j = n2
            g1 = 0: g2 = hgt: GoSub gds5z
            n1k = n1: n2k = n2
            g0 = zg(n2k + 1) - zg(n1k - 1)
            For k = n1k To n2k
                g1 = zg(n2k + 1) - zg(k)
                d0 = wdp(n) * g1 / g0
                For j = n1j To n2j
                    If wdr(n) = 2 Then d1 = yg(n2j + 1) - yg(j + 1) _
                    Else d1 = yg(j - 1) - wdt
                    For i = n1i To n2i
                        If d0 > d1 Then ibc(i, j, k) = 0
                        If k = n1k Then ibc(i, j, k - 2) = 4
                    Next
                Next
            Next
        End If
    Next
  End If
  
'bubbler
  ReDim swi1(nsw), swi2(nsw), swj1(nsw), swj2(nsw)
  ReDim swk1(nsw), swk2(nsw)
  If flow_domain = 2 Then
    For n = 1 To nsw  'nsw is the number of bubblers
        If wdr(n) < 2 Then
            g1 = wsg(n): g2 = g1 + wdp(n): GoSub gds5y
            swj1(n) = n1: swj2(n) = n2
            g1 = wbg(n): g2 = g1 + wwd(n): GoSub gds5z
            swk1(n) = n1: swk2(n) = n2
            If wdr(n) = 0 Then
                i1 = 2: i2 = np: i0 = 2
            Else
                i1 = mp: i2 = 2: i0 = -2
            End If
            For j = swj1(n) To swj2(n) Step 2
            For k = swk1(n) To swk2(n) Step 2
            For i = i1 To i2 Step i0
                If ibc(i + i0, j, k) = 0 And ibc(i, j, k) >= 1 Then _
                ibc(i, j, k) = 8: swi1(n) = i: swi2(n) = i: Exit For
            Next: Next: Next
        ElseIf wdr(n) < 4 Then
            g1 = wsg(n): g2 = g1 + wdp(n): GoSub gds5x
            swi1(n) = n1: swi2(n) = n2
            g1 = wbg(n): g2 = g1 + wwd(n): GoSub gds5z
            swk1(n) = n1: swk2(n) = n2
            If wdr(n) = 2 Then
                j1 = 2: j2 = np: j0 = 2
            Else
                j1 = np: j2 = 2: j0 = -2
            End If
            For i = swi1(n) To swi2(n) Step 2
            For k = swk1(n) To swk2(n) Step 2
            For j = j1 To j2 Step j0
                If ibc(i, j + j0, k) = 0 And ibc(i, j, k) >= 1 Then _
                ibc(i, j, k) = 8: swj1(n) = j: swj2(n) = j: Exit For
            Next: Next: Next
        Else
            'g1 = wsg(n): g2 = g1 + wdp(n): GoSub gds5x
            'swi1(n) = n1: swi2(n) = n2
            'g1 = wbg(n): g2 = g1 + wwd(n): GoSub gds5y
            'swj1(n) = n1: swj2(n) = n2
            'If wdr(n) = 4 Then
            '  k1 = 2: k2 = lp: k0 = 2
            'Else
            '  k1 = lp: k2 = 2: k0 = -2
            'End If
            'For i = swi1(n) To swi2(n) Step 2
            'For j = swj1(n) To swj2(n) Step 2
            'For k = k1 To k2 Step k0
            '  If ibc(i, j, k + k0) = 0 And ibc(i, j, k) >= 1 Then _
            '  ibc(i, j, k) = 1: swk1(n) = k: swk2(n) = k: Exit For
            ''ibc(i, j, k) = 8: swk1(n) = k: swk2(n) = k: Exit For
            'Next: Next: Next
    
            g1 = wsg(n): g2 = g1 + wwd(n): GoSub gds5x
            swi1(n) = n1: swi2(n) = n2
            g1 = wbg(n): g2 = g1 + wwd(n): GoSub gds5y
            swj1(n) = n1: swj2(n) = n2
            g1 = 0: g2 = g1 + wdp(n): GoSub gds5z
            swk1(n) = n1: swk2(n) = n2
      
            'If wdr(n) = 4 Then
            '  k1 = 2: k2 = lp: k0 = 2
            'Else
            '  k1 = lp: k2 = 2: k0 = -2
            'End If
            
            k0 = 2
            For i = swi1(n) To swi2(n) Step 2
            For j = swj1(n) To swj2(n) Step 2
            For k = swk1(n) To swk2(n) Step k0
                ibc(i, j, k) = 1
                'If ibc(i, j, k + k0) = 0 And ibc(i, j, k) >= 1 Then _
                'ibc(i, j, k) = 1: swk1(n) = k: swk2(n) = k: Exit For
                'ibc(i, j, k) = 8: swk1(n) = k: swk2(n) = k: Exit For
            Next: Next: Next
        End If
    Next
  End If
  
'ebooster
  If flow_domain = 2 Then
    ReDim ebi1(neb, 3), ebi2(neb, 3), ebj1(neb, 3), ebj2(neb, 3)
    ReDim ebk1(neb, 3), ebk2(neb, 3)
    For n = 1 To neb
        If ebty(n) < 3 Then m1 = 2 Else m1 = 3
        For jebb = 1 To m1
            ebsg0 = ebsg(n, jebb): ebwd0 = ebwd(n, jebb)
            ebbg0 = ebbg(n, jebb): ebht0 = ebht(n, jebb)
            ebdr0 = ebdr(n, jebb)
            If ebdr0 < 2 Then
                g1 = ebsg0: g2 = g1 + ebwd0: GoSub gds5y
                j1 = n1: j2 = n2
                g1 = ebbg0: g2 = g1 + ebwd0: GoSub gds5z
                k1 = n1: k2 = n2
                If ebdr0 = 0 Then g1 = 0: g2 = ebht0 _
                Else g1 = lth - ebht0: g2 = lth
                GoSub gds5x: i1 = n1: i2 = n2
            ElseIf ebdr0 < 4 Then
                g1 = ebsg0: g2 = g1 + ebwd0: GoSub gds5x
                i1 = n1: i2 = n2
                g1 = ebbg0: g2 = g1 + ebwd0: GoSub gds5z
                k1 = n1: k2 = n2
                If ebdr0 = 2 Then g1 = 0: g2 = ebht0 _
                Else g1 = wdt - ebht0: g2 = wdt
                GoSub gds5y: j1 = n1: j2 = n2
            Else
                g1 = ebsg0: g2 = g1 + ebwd0: GoSub gds5x
                i1 = n1: i2 = n2
                g1 = ebbg0: g2 = g1 + ebwd0: GoSub gds5y
                j1 = n1: j2 = n2
                If ebdr0 = 4 Then g1 = 0: g2 = ebht0 _
                Else g1 = hgt - ebht0: g2 = hgt
                GoSub gds5z: k1 = n1: k2 = n2
            End If
            For i = i1 To i2 Step 2
            For j = j1 To j2 Step 2
            For k = k1 To k2 Step 2
                ibc(i, j, k) = 5
            Next: Next: Next
            ebi1(n, jebb) = i1: ebi2(n, jebb) = i2
            ebj1(n, jebb) = j1: ebj2(n, jebb) = j2
            ebk1(n, jebb) = k1: ebk2(n, jebb) = k2
        Next
    Next
  End If
  
'wall prop
  ReDim wa_i1(nwal), wa_i2(nwal), wa_j1(nwal), wa_j2(nwal)
  ReDim wa_k1(nwal), wa_k2(nwal)
  For n = 2 To nwal
    sg0 = wa_sg(n): wd0 = wa_wd(n)
    bg0 = wa_bg(n): ht0 = wa_ht(n)
    dr0 = wa_dr(n)
    If dr0 < 2 Then
      g1 = sg0: g2 = g1 + wd0: GoSub gds5y
      j1 = n1: j2 = n2
      g1 = bg0: g2 = g1 + wd0: GoSub gds5z
      k1 = n1: k2 = n2
      If dr0 = 0 Then i1 = 2: i2 = 2 _
      Else i1 = mp: i2 = mp
    ElseIf dr0 < 4 Then
      g1 = sg0: g2 = g1 + wd0: GoSub gds5x
      i1 = n1: i2 = n2
      g1 = bg0: g2 = g1 + wd0: GoSub gds5z
      k1 = n1: k2 = n2
      If dr0 = 2 Then j1 = 2: j2 = 2 _
      Else j1 = np: j2 = np
    Else
      g1 = sg0: g2 = g1 + wd0: GoSub gds5x
      i1 = n1: i2 = n2
      g1 = bg0: g2 = g1 + wd0: GoSub gds5y
      j1 = n1: j2 = n2
      If dr0 = 4 Then k1 = 2: k2 = 2 _
      Else: k1 = lp: k2 = lp
    End If
    wa_i1(n) = i1: wa_i2(n) = i2
    wa_j1(n) = j1: wa_j2(n) = j2
    wa_k1(n) = k1: wa_k2(n) = k2
  Next
'
  For i = 1 To nbr
    If bty(i) <= 1 Then
      bsg(i, 1) = bsg(i, 1) + bwd(i, 1) / 2
      bbg(i, 1) = bbg(i, 1) + bwd(i, 1) / 2
    End If
  Next
  Exit Sub
'
'
'             end of grid routine
'
'             rest of grid code is internal subroutines


  

gdr0x: 'Inserts a line in the x-grid at position c0
  For n1a = 0 To n1
    e1 = c0 - xgb1(n1a)
    If Abs(e1) < 0.001 Then Return 'if spacing is less than 1 mm
    If e1 < 0 Then Exit For
  Next
  ReDim xgb0(n1)
  For n1b = 0 To n1: xgb0(n1b) = xgb1(n1b): Next
  n1 = n1 + 1: ReDim xgb1(n1)
  For n1b = 0 To n1a - 1: xgb1(n1b) = xgb0(n1b): Next
  xgb1(n1a) = c0
  For n1b = n1a + 1 To n1: xgb1(n1b) = xgb0(n1b - 1): Next
  Return

gdr0y:
  For n1a = 0 To n2
    e1 = c0 - ygb1(n1a)
    If Abs(e1) < 0.001 Then Return
    If e1 < 0 Then Exit For
  Next
  ReDim xgb0(n2)
  For n1b = 0 To n2: xgb0(n1b) = ygb1(n1b): Next
  n2 = n2 + 1: ReDim ygb1(n2)
  For n1b = 0 To n1a - 1: ygb1(n1b) = xgb0(n1b): Next
  ygb1(n1a) = c0
  For n1b = n1a + 1 To n2: ygb1(n1b) = xgb0(n1b - 1): Next
  Return

gdr0z:
  For n1a = 0 To n3
    e1 = c0 - zgb1(n1a)
    If Abs(e1) < 0.001 Then Return
    If e1 < 0 Then Exit For
  Next
  ReDim xgb0(n3)
  For n1b = 0 To n3: xgb0(n1b) = zgb1(n1b): Next
  n3 = n3 + 1: ReDim zgb1(n3)
  For n1b = 0 To n1a - 1: zgb1(n1b) = xgb0(n1b): Next
  zgb1(n1a) = c0
  For n1b = n1a + 1 To n3: zgb1(n1b) = xgb0(n1b - 1): Next
  Return


gdr1:
  'build the border lines of the furnace
  ReDim xgb1(0), ygb1(0), zgb1(0), xgb2(5)
  eps0 = dx0 'eps0=default x-length of cell
  n1 = 0: xgb1(0) = 0
  If chamber_type = 2 Then 'refiner with melter
    c0 = lth_r(1): GoSub gdr0x
    c0 = gx_r(2): GoSub gdr0x
    c0 = c0 + lth_r(2): GoSub gdr0x
    c0 = gx_r(3): GoSub gdr0x
    c0 = c0 + lth_r(3): GoSub gdr0x
  Else
    c0 = lth: GoSub gdr0x
  End If
  If eps0 > dy0 Then eps0 = dy0
  n2 = 0: ygb1(0) = 0
  If chamber_type = 2 Then 'refiner with melter
    c0 = wdt_r(1): GoSub gdr0y
    c0 = gy_r(2): GoSub gdr0y
    c0 = c0 + wdt_r(2): GoSub gdr0y
    c0 = gy_r(3): GoSub gdr0y
    c0 = c0 + wdt_r(3): GoSub gdr0y
  Else
    c0 = wdt: GoSub gdr0y
  End If
  If eps0 > dz0 Then eps0 = dz0
  n3 = 0: zgb1(0) = 0
  If chamber_type = 2 Then 'refiner with melter
    c0 = hgt_r(1): GoSub gdr0z
    c0 = gz_r(2): GoSub gdr0z
    c0 = c0 + hgt_r(2): GoSub gdr0z
    c0 = gz_r(3): GoSub gdr0z
    c0 = c0 + hgt_r(3): GoSub gdr0z
  Else
    c0 = hgt: GoSub gdr0z
    If hgt_c > 0 Then c0 = hgt + hgt_c: GoSub gdr0z
  End If
  If chamber_type = 2 Then lth = lth_r(1): wdt = wdt_r(1): hgt = hgt_r(1)
  
  'At this point, assuming there is no refiner, then
  ' n1=n2=n3=1
  ' xgb1(1)=lth, ygb1(1)=wdt, zgb1(1)=hgt
  ' eps0=smallest default cell dimension
  
  'Build objects in the grid by adding new grid lines for the object dimensions
  'n1=number of x grid lines, n2=number of y grid lines, n3=number of z grid lines
  
  'build or start to build burners in grid
  For i = 1 To nbr
    If bty(i) = 4 Then ' 10-22-2004
      'For tube-in-tube burners (type 4) have inner=2 and outer=1 sets of info
      inorout = 2 'work on inner part first
      If bdr(i) = 0 Or bdr(i) = 1 Then 'back or front orientation
        g1 = bsg(i, inorout): g0 = bwd(i, inorout): GoSub gdr1e
        GoSub gdr1f
      ElseIf bdr(i) = 2 Or bdr(i) = 3 Then 'right or left orientation
        GoSub gdr1d: GoSub gdr1f
      ElseIf bdr(i) = 4 Or bdr(i) = 5 Then 'bottom or top orientation
        GoSub gdr1d
        g1 = bbg(i, inorout): g0 = bht(i, inorout): GoSub gdr1e
      End If
    End If
    
    inorout = 1 'do other bruner types and outer part of type 4

    If bdr(i) = 0 Or bdr(i) = 1 Then 'back or front orientation
      g1 = bsg(i, inorout): g0 = bwd(i, inorout): GoSub gdr1e
      GoSub gdr1f
    ElseIf bdr(i) = 2 Or bdr(i) = 3 Then 'right or left orientation
      GoSub gdr1d: GoSub gdr1f
    ElseIf bdr(i) = 4 Or bdr(i) = 5 Then 'bottom or top orientation
      GoSub gdr1d
      g1 = bbg(i, inorout): g0 = bht(i, inorout): GoSub gdr1e
    End If
  Next
  
  'build dog houses (side wells) or bubblers in grid
  'for bubblers:
  '    wwd = diameter (width), wdp = height, wsg = end gap, wbg = side gap
  For i = 1 To nsw
    If eps0 > wwd(i) Then eps0 = wwd(i)
    If eps0 > wdp(i) Then eps0 = wdp(i)
    If wdr(i) < 2 Then
      c0 = wsg(i): GoSub gdr0y
      c0 = c0 + wwd(i): GoSub gdr0y
      If flow_domain = 1 Then
        If wdr(i) = 0 Then c0 = -wdp(i): GoSub gdr0x
        If wdr(i) = 1 Then c0 = lth + wdp(i): GoSub gdr0x
      Else
        c0 = wbg(i): GoSub gdr0z
        c0 = wbg(i) + wdp(i): GoSub gdr0z
      End If
    Else
      c0 = wsg(i): GoSub gdr0x
      c0 = c0 + wwd(i): GoSub gdr0x
      If flow_domain = 1 Then
        If wdr(i) = 2 Then c0 = -wdp(i): GoSub gdr0y
        If wdr(i) = 3 Then c0 = wdt + wdp(i): GoSub gdr0y
      Else 'this is the melt so we are inserting a bubbler
        c0 = wbg(i)
        If wdr(i) < 4 Then GoSub gdr0z Else GoSub gdr0y
        'c0 = wbg(i) + wdp(i)
        c0 = wbg(i) + wwd(i)
        If wdr(i) < 4 Then GoSub gdr0z Else GoSub gdr0y
        c0 = wdp(i)
        If wdr(i) >= 4 Then GoSub gdr0z 'inserts line for top of bubbler
      End If
    End If
  Next
  
  'build electric boosters
  For i = 1 To neb: For j = 1 To 3
    If j = 3 And ebty(i) < 3 Then Exit For
    ebwd0 = ebwd(i, j): ebdr0 = ebdr(i, j)
    ebsg0 = ebsg(i, j): ebbg0 = ebbg(i, j)
    ebht0 = ebht(i, j)
    If eps0 > ebwd0 Then eps0 = ebwd0
    If ebdr0 < 2 Then
      c0 = ebsg0: GoSub gdr0y
      c0 = c0 + ebwd0: GoSub gdr0y
      c0 = ebbg0: GoSub gdr0z
      c0 = c0 + ebwd0: GoSub gdr0z
      If ebdr0 = 0 Then c0 = ebht0 Else c0 = lth - ebht0
      GoSub gdr0x
    ElseIf ebdr0 < 4 Then
      c0 = ebsg0: GoSub gdr0x
      c0 = c0 + ebwd0: GoSub gdr0x
      c0 = ebbg0: GoSub gdr0z
      c0 = c0 + ebwd0: GoSub gdr0z
      If ebdr0 = 2 Then c0 = ebht0 Else c0 = wdt - ebht0
      GoSub gdr0y
    Else
      c0 = ebsg0: GoSub gdr0x
      c0 = c0 + ebwd0: GoSub gdr0x
      c0 = ebbg0: GoSub gdr0y
      c0 = c0 + ebwd0: GoSub gdr0y
      If ebdr0 = 4 Then c0 = ebht0 Else c0 = hgt - ebht0
      GoSub gdr0z
    End If
  Next: Next
  
  'build walls
  c0 = xgb1(0) - thk(0): GoSub gdr0x
  c0 = xgb1(n1) + thk(1): GoSub gdr0x
  c0 = ygb1(0) - thk(2): GoSub gdr0y
  c0 = ygb1(n2) + thk(3): GoSub gdr0y
  c0 = zgb1(0) - thk(4): GoSub gdr0z
  c0 = zgb1(n3) + thk(5): GoSub gdr0z
  
  'find which walls have exits
  iw_back = 0: iw_front = 0: iw_right = 0: iw_left = 0: iw_bot = 0: iw_top = 0
      For i = 1 To nex
        Select Case edr(i)
        Case 0
            iw_back = iw_back + 1
        Case 1
            iw_front = iw_front + 1
        Case 2
            iw_right = iw_right + 1
        Case 3
            iw_left = iw_left + 1
        Case 4
            iw_bot = iw_bot + 1
        Case 5
            iw_top = iw_top + 1
        End Select
      Next
  
  'deal with exit tunnels in melter
  If flow_domain = 1 Then
      tunnel_dx = 0 'no tunnels for combustor
  Else
      'build extensions for melt exit tunnels
      If extra_tunnel_length > 0 And extra_tunnel_cells > 0 Then
        tunnel_dx = extra_tunnel_length / extra_tunnel_cells
      Else
        tunnel_dx = 0
      End If
      
      'mark line for tunnel ends
      If iw_back > 0 Then
        c0 = xgb1(0) - extra_tunnel_length
        GoSub gdr0x
      End If
      If iw_front > 0 Then
        c0 = xgb1(n1) + extra_tunnel_length
        GoSub gdr0x
      End If
      If iw_right > 0 Then
        c0 = ygb1(0) - extra_tunnel_length
        GoSub gdr0y
      End If
      If iw_left > 0 Then
        c0 = ygb1(n2) + extra_tunnel_length
        GoSub gdr0y
      End If
      If iw_bot > 0 Then
        c0 = zgb1(0) - extra_tunnel_length
        GoSub gdr0z
      End If
      If iw_top > 0 Then
        c0 = zgb1(n3) + extra_tunnel_length
        GoSub gdr0z
      End If
   End If
   
  'build exhausts or outlets
        'at this point have lines for interior dimensions, outer walls, and end of tunnel extensions
  For i = 1 To nex
    If eps0 > ewd(i) Then eps0 = ewd(i) 'make sure cell size is not greater than exit width
    If eps0 > eht(i) Then eps0 = eht(i) 'make sure cell size is not greater than exit length
    
    If edr(i) = 0 Then 'exit on back wall (x=0 face)
      If iw_back > 0 Then 'doing first exit on this wall
        iw_back = -1
        i2 = Int(thk(0) * 2 / dx0) + 1 'i2 = twice the wall thickness divided by the maximum cell size + 1
        If i2 < 2 Then i2 = 2 'i2 minimum is 2
        dx1 = thk(0) / i2 'dx1 set to length of cells inside wall
        c0 = -thk(0): 'GoSub gdr0x 'add line for outer wall - already done
        'fill in lines within wall
        For m1 = 2 To i2: c0 = c0 + dx1: GoSub gdr0x: Next 'add lines to interior of exit wall
        If tunnel_dx > 0# Then  'Fill in lines within exit tunnel
            c0 = xgb1(0) + tunnel_dx: GoSub gdr0x
            For m1 = 2 To extra_tunnel_cells: c0 = c0 + tunnel_dx: GoSub gdr0x: Next
        End If
      End If
      GoSub gdr1a 'add lines for exit sides in other directions
      
    ElseIf edr(i) = 1 Then 'exit on front wall (x=mp face)
      If iw_front > 0 Then 'doing first exit on this wall
        iw_front = -1
        i2 = Int(thk(1) * 2 / dx0) + 1
        If i2 < 2 Then i2 = 2
        dx1 = thk(1) / i2
        If chamber_type = 2 Then c0 = gx_r(3) + lth_r(3) Else c0 = lth
        c0 = c0 + thk(1): 'GoSub gdr0x 'this line already created
        For m1 = 2 To i2: c0 = c0 - dx1: GoSub gdr0x: Next
        If tunnel_dx > 0# Then  'Fill in lines within exit tunnel
            c0 = xgb1(n1) - tunnel_dx: GoSub gdr0x
            For m1 = 2 To extra_tunnel_cells: c0 = c0 - tunnel_dx: GoSub gdr0x: Next
        End If
      End If
      GoSub gdr1a
      
    ElseIf edr(i) = 2 Then 'exit on right wall (y=0 face)
      If iw_right > 0 Then 'doing first exit on this wall
        iw_right = -1
        i2 = Int(thk(2) * 2 / dy0) + 1 'i2 = twice the wall thickness divided by the maximum cell size + 1
        If i2 < 2 Then i2 = 2 'i2 minimum is 2
        dx1 = thk(2) / i2
        If chamber_type = 2 Then c0 = gy_r(3) Else c0 = 0
        'fill in lines within wall
        For m1 = 2 To i2: c0 = c0 + dx1: GoSub gdr0y: Next 'add lines to interior of exit wall
        If tunnel_dx > 0# Then  'Fill in lines within exit tunnel
            c0 = ygb1(0) + tunnel_dx: GoSub gdr0y
            For m1 = 2 To extra_tunnel_cells: c0 = c0 + tunnel_dx: GoSub gdr0y: Next
        End If
      End If
      GoSub gdr1b
      
    ElseIf edr(i) = 3 Then 'exit on left wall (y=np face)
      If iw_left > 0 Then 'doing first exit on this wall
        iw_left = -1
        i2 = Int(thk(3) * 2 / dy0) + 1
        If i2 < 2 Then i2 = 2
        dx1 = thk(3) / i2
        If chamber_type = 2 Then c0 = gy_r(3) + wdt_r(3) Else c0 = wdt
        c0 = c0 + thk(3): 'GoSub gdr0y 'this line already created
        For m1 = 2 To i2: c0 = c0 - dx1: GoSub gdr0y: Next
        If tunnel_dx > 0# Then  'Fill in lines within exit tunnel
            c0 = ygb1(n2) - tunnel_dx: GoSub gdr0y
            For m1 = 2 To extra_tunnel_cells: c0 = c0 - tunnel_dx: GoSub gdr0y: Next
        End If
      End If
      GoSub gdr1b
      
    ElseIf edr(i) = 4 Then 'exit on bottom wall (z=0 face)
      If iw_bot > 0 Then 'doing first exit on this wall
        iw_bot = -1
        i2 = Int(thk(4) * 2 / dz0) + 1 'i2 = twice the wall thickness divided by the maximum cell size + 1
        If i2 < 2 Then i2 = 2 'i2 minimum is 2
        dx1 = thk(4) / i2
        If chamber_type = 2 Then c0 = gz_r(3) Else c0 = 0
        'fill in lines within wall
        For m1 = 2 To i2: c0 = c0 + dx1: GoSub gdr0z: Next 'add lines to interior of exit wall
        If tunnel_dx > 0# Then  'Fill in lines within exit tunnel
            c0 = zgb1(0) + tunnel_dx: GoSub gdr0z
            For m1 = 2 To extra_tunnel_cells: c0 = c0 + tunnel_dx: GoSub gdr0z: Next
        End If
      End If
      GoSub gdr1c
      
    ElseIf edr(i) = 5 Then 'exit on top wall (z=lp face)
      If iw_top > 0 Then 'doing first exit on this wall
        iw_top = -1
        i2 = Int(thk(5) * 2 / dz0) + 1
        If i2 < 2 Then i2 = 2
        dx1 = thk(5) / i2
        If chamber_type = 2 Then c0 = gz_r(3) + hgt_r(3) Else c0 = hgt
        c0 = c0 + thk(5): 'GoSub gdr0z 'this line already created
        For m1 = 2 To i2: c0 = c0 - dx1: GoSub gdr0z: Next
        If tunnel_dx > 0# Then  'Fill in lines within exit tunnel
            c0 = zgb1(n3) - tunnel_dx: GoSub gdr0z
            For m1 = 2 To extra_tunnel_cells: c0 = c0 - tunnel_dx: GoSub gdr0z: Next
        End If
      End If
      GoSub gdr1c
    End If
  Next
  Return

gdr0d6:
  For n = 1 To nbr
  If bdr(n) = bdr0 Then
    g1 = bwd(n, 1)
    If bty(n) >= 2 And g1 > bht(n, 1) Then g1 = bht(n, 1)
    If g1 < g0 Then m = n: g0 = g1
  End If
  Next
  Return

gdr0d0:
  g1 = 0: If bdr0 > 1 Then GoTo gdr0d1
  If bdr0 = 0 Then c0 = 0 Else c0 = lth
  For n1b = 0 To n1
  If Abs(xgb1(n1b) - c0) < 0.001 Then 'grid spacing size limit?
    If bdr0 = 0 Then g1 = xgb1(n1b + 1) Else _
      g1 = c0 - xgb1(n1b - 1)
    Exit For
  End If: Next

gdr0d0a:
  If g0 > dx0 Or g1 <= g0 Then Return
  If g1 < g0 * 2 Then g0 = g1 / 2
  If bdr0 = 0 Then c0 = c0 + g0 Else c0 = c0 - g0
  GoSub gdr0x
  g1 = g1 - g0: g0 = g0 * 2
  GoTo gdr0d0a

gdr0d1:
  If bdr0 = 2 Then c0 = 0 Else c0 = wdt
  For n1b = 0 To n2
  If Abs(ygb1(n1b) - c0) < 0.001 Then
    If bdr0 = 2 Then g1 = ygb1(n1b + 1) Else _
      g1 = c0 - ygb1(n1b - 1)
    Exit For
  End If: Next

gdr0d1a:
  If g0 > dy0 Or g1 <= g0 Then Return
  If g1 < g0 * 2 Then g0 = g1 / 2
  If bdr0 = 2 Then c0 = c0 + g0 Else c0 = c0 - g0
  GoSub gdr0y
  g1 = g1 - g0: g0 = g0 * 2
  GoTo gdr0d1a

gdr1a:
  'previous code:
  'c0 = esg(i): If chamber_type = 2 Then c0 = c0 + gy_r(3)
  'GoSub gdr0y
  'c0 = c0 + ewd(i): GoSub gdr0y
  'c0 = ebg(i): If chamber_type = 2 Then c0 = c0 + gz_r(3)
  'GoSub gdr0z
  'c0 = c0 + eht(i): GoSub gdr0z
  'Return
 
  'for x-face wall exits
  'add y line for starting side of exit
  c0 = esg(i): If chamber_type = 2 Then c0 = c0 + gy_r(3)
  c1 = c0 + ewd(i) 'save ending side
  GoSub gdr0y
  'add interior lines to make sure there are adequate number of cells to resolve flow
  dx1 = ewd(i) / n_cells_across_exit
  For m1 = 1 To (n_cells_across_exit - 1): c0 = c0 + dx1: GoSub gdr0y: Next
  'add y line for ending side of exit
  c0 = c1: GoSub gdr0y

  'add z line for starting side of exit
  c0 = ebg(i): If chamber_type = 2 Then c0 = c0 + gz_r(3)
  c1 = c0 + eht(i) 'save ending side
  GoSub gdr0z
  'add interior lines to make sure there are adequate number of cells to resolve flow
  dx1 = eht(i) / n_cells_across_exit
  For m1 = 1 To n_cells_across_exit - 1: c0 = c0 + dx1: GoSub gdr0z: Next
  'add z line for ending side of exit
  c0 = c1: GoSub gdr0z
  Return

gdr1b:
  'for y-face wall exits
  'add x line for starting side of exit
  c0 = esg(i): If chamber_type = 2 Then c0 = c0 + gx_r(3)
  c1 = c0 + ewd(i) 'save ending side
  GoSub gdr0x
  'add interior lines to make sure there are adequate number of cells to resolve flow
  dx1 = ewd(i) / n_cells_across_exit
  For m1 = 1 To (n_cells_across_exit - 1): c0 = c0 + dx1: GoSub gdr0x: Next
  'add x line for ending side of exit
  c0 = c1: GoSub gdr0x
  
  'add z line for starting side of exit
  c0 = ebg(i): If chamber_type = 2 Then c0 = c0 + gz_r(3)
  c1 = c0 + eht(i) 'save ending side
  GoSub gdr0z
  'add interior lines to make sure there are adequate number of cells to resolve flow
  dx1 = eht(i) / n_cells_across_exit
  For m1 = 1 To n_cells_across_exit - 1: c0 = c0 + dx1: GoSub gdr0z: Next
  'add z line for ending side of exit
  c0 = c1: GoSub gdr0z
  Return

gdr1c:
  'for z-face wall exits
  'add x line for starting side of exit
  c0 = esg(i): If chamber_type = 2 Then c0 = c0 + gx_r(3)
  c1 = c0 + ewd(i) 'save ending side
  GoSub gdr0x
  'add interior lines to make sure there are adequate number of cells to resolve flow
  dx1 = ewd(i) / n_cells_across_exit
  For m1 = 1 To (n_cells_across_exit - 1): c0 = c0 + dx1: GoSub gdr0x: Next
  'add x line for ending side of exit
  c0 = c1: GoSub gdr0x
  
  'add y line for starting side of exit
  c0 = esg(i): If chamber_type = 2 Then c0 = c0 + gy_r(3)
  c1 = c0 + ewd(i) 'save ending side
  GoSub gdr0y
  'add interior lines to make sure there are adequate number of cells to resolve flow
  dx1 = ewd(i) / n_cells_across_exit
  For m1 = 1 To (n_cells_across_exit - 1): c0 = c0 + dx1: GoSub gdr0y: Next
  'add y line for ending side of exit
  c0 = c1: GoSub gdr0y
  Return

gdr1d:
  c0 = bsg(i, inorout): GoSub gdr0x
  If bty(i) = 0 Or bty(i) = 1 Then
    GoSub gdr1g
    For j = 1 To n
      c0 = c0 + xgb2(j): GoSub gdr0x
    Next
  ElseIf bty(i) = 2 Then
    c0 = c0 + bwd(i, inorout): GoSub gdr0x
    If eps0 > bwd(i, inorout) Then eps0 = bwd(i, inorout)
  Else
    c0 = c0 + bwd(i, inorout): GoSub gdr0x
    c0 = c0 - bwd(i, inorout) * 0.08: GoSub gdr0x
    c0 = c0 - bwd(i, inorout) * 0.28: GoSub gdr0x
    c0 = c0 - bwd(i, inorout) * 0.28: GoSub gdr0x
    c0 = c0 - bwd(i, inorout) * 0.28: GoSub gdr0x
    If eps0 > bwd(i, inorout) * 0.08 Then eps0 = bwd(i, inorout) * 0.08
  End If
  Return

gdr1e:
  c0 = g1: GoSub gdr0y
  If bty(i) = 0 Or bty(i) = 1 Then
    GoSub gdr1g
    For j = 1 To n
      c0 = c0 + xgb2(j): GoSub gdr0y
    Next
  ElseIf bty(i) = 2 Then
    c0 = c0 + g0: GoSub gdr0y
    If eps0 > g0 Then eps0 = g0
  Else
    c0 = c0 + g0: GoSub gdr0y
    If bdr(i) <= 1 Then
      c0 = c0 - g0 * 0.08: GoSub gdr0y
      c0 = c0 - g0 * 0.28: GoSub gdr0y
      c0 = c0 - g0 * 0.28: GoSub gdr0y
      c0 = c0 - g0 * 0.28: GoSub gdr0y
      If eps0 > g0 * 0.08 Then eps0 = g0 * 0.08
    Else
      c0 = c0 - g0 * 0.344: GoSub gdr0y
      c0 = c0 - g0 * 0.312: GoSub gdr0y
      If eps0 > g0 * 0.312 Then eps0 = g0 * 0.312
    End If
  End If
  Return

gdr1f:
  c0 = bbg(i, inorout): GoSub gdr0z
  If bty(i) = 0 Or bty(i) = 1 Then
    GoSub gdr1g
    For j = 1 To n
      c0 = c0 + xgb2(j): GoSub gdr0z
    Next
  ElseIf bty(i) = 2 Then
    c0 = c0 + bht(i, inorout): GoSub gdr0z
    If eps0 > bht(i, inorout) Then eps0 = bht(i, inorout)
  Else
    c0 = c0 + bht(i, inorout): GoSub gdr0z
    c0 = c0 - bht(i, inorout) * 0.344: GoSub gdr0z
    c0 = c0 - bht(i, inorout) * 0.312: GoSub gdr0z
    If eps0 > bht(i, inorout) * 0.312 Then eps0 = bht(i, inorout) * 0.312
  End If
  Return

gdr1g:
  xgb2(0) = 0
  If bty(i) = 0 Then
    n = 3: g0 = bwd(i, inorout) / n
    For j = 1 To n: xgb2(j) = g0: Next
    If eps0 > g0 Then eps0 = g0
  ElseIf bty(i) = 1 Then
    n = 5: g0 = (bwd(i, inorout) - bht(i, inorout)) / 4
    For j = 1 To n: xgb2(j) = g0: Next
    xgb2(3) = bht(i, inorout)
    If eps0 > g0 Then eps0 = g0
    If eps0 > bht(i, inorout) Then eps0 = bht(i, inorout)
  End If
  Return

gdr2:
    'Come here with ix1=direction, n=number of grid lines, da0=max cell length, and
    'xgb1() has positions for lines that are furnace and object boundaries.
    'Returns with xg1() having all grid lines and cell centers in given direction.
    
  If ix1 = 3 And flow_domain = 1 Then 'doing z dir in combustion (have melt surface to deal with)
    m = 3: i10 = 1: ReDim xg1(m)
    xg1(3) = xgb1(0): xg1(1) = xg1(3) - eps0 * 2
  Else
    m = 5: i10 = 2: ReDim xg1(m)
    xg1(3) = xgb1(0): xg1(1) = xg1(3) - eps0 * 2
    xg1(5) = xgb1(1)
  End If
  
  'Add grid lines between the furnace and object boundary lines
  For i1 = i10 To n - 1 'go thru grid lines 2 (usually) to last line-1 (these lines are furnace & object boundaries)
  
myspot:
        

  
    'if in exit tunnel then do not add extra cell lines
    'If (ix1 = 1 And xgb1(i1) < -thk(0)) Or (ix1 = 1 And xgb1(i1) > (lth + thk(1))) Or _
    '   (ix1 = 2 And xgb1(i1) < -thk(2)) Or (ix1 = 2 And xgb1(i1) > (wdt + thk(3))) Or _
    '   (ix1 = 3 And xgb1(i1) < -thk(4)) Or (ix1 = 3 And xgb1(i1) > (hgt + thk(5))) Then
    
    
'    If flow_domain = 2 and _
'       ((ix1 = 1 And ((xgb1(i1) < -thk(0)) Or _
'                      (chamber_type <> 2 And xgb1(i1) > (lth + thk(1))) Or _
'                      (chamber_type = 2 and xgb1(i1) > (gx_r(3) + lth_r(3) + thk(1)))) or _
'        (ix1 = 2 And ((xgb1(i1) < -thk(2)) Or _
'                      (chamber_type <> 2 And xgb1(i1) > (wdt + thk(3))) Or _
'                      (chamber_type = 2 And xgb1(i1) > (gy_r(3) + wdt_r(3) + thk(3)))) Or _
'        (ix1 = 3 And ((xgb1(i1) < -thk(4)) Or _
'                      (chamber_type <> 2 and xgb1(i1) > (hgt + thk(5))) or _
                      (chamber_type = 2 and xgb1(i1) > (gz_r(3) + hgt_r(3) + thk(5))))) Then
    
    
    
    'if in exit tunnel then do not add extra cell lines
    in_tunnel = False
    If flow_domain = 2 Then
        If ix1 = 1 Then
            If xgb1(i1) < -thk(0) Or _
                      (chamber_type <> 2 And xgb1(i1) > (lth + thk(1))) Or _
                      (chamber_type = 2 And xgb1(i1) > (gx_r(3) + lth_r(3) + thk(1))) _
               Then in_tunnel = True
        ElseIf ix1 = 2 Then
            If xgb1(i1) < -thk(2) Or _
                      (chamber_type <> 2 And xgb1(i1) > (wdt + thk(3))) Or _
                      (chamber_type = 2 And xgb1(i1) > (gy_r(3) + wdt_r(3) + thk(3))) _
               Then in_tunnel = True
        ElseIf ix1 = 3 Then
            If xgb1(i1) < -thk(4) Or _
                      (chamber_type <> 2 And xgb1(i1) > (hgt + thk(5))) Or _
                      (chamber_type = 2 And xgb1(i1) > (gz_r(3) + hgt_r(3) + thk(5))) _
               Then in_tunnel = True
        End If
    End If
    
    If in_tunnel = True Then
        'in an exit tunnel area, just add current line keeping long cell
        ReDim xgb0(m)
        For i3 = 1 To m Step 2: xgb0(i3) = xg1(i3): Next 'save xg1 into xgb0
        m1 = m: m = m + 2: ReDim xg1(m)
        For i3 = 1 To m1 Step 2: xg1(i3) = xgb0(i3): Next 'restore xg1 up to old m
        xg1(m) = xgb1(i1) 'add current line to xg1()
              
    Else 'normal case: add lines between objects
        dx1 = xgb1(i1) - xgb1(i1 - 1) 'set dx1 = distance between current line and previous line
        If chamber_type = 2 And xgb1(i1) > x1a And xgb1(i1) <= x1b Then
          i2 = Int(dx1 * 2 / da0) + 1
        Else
          i2 = Int(dx1 / da0) + 1 'i2=(distance divided by max cell size) + 1 (= number of cells needed between lines?)
        End If
        If i2 < 1 Then i2 = 1 'i2 must at least be 1
        dx1 = dx1 / i2 'reset dx1 to be distance between cells
        ReDim xgb0(m)
        For i3 = 1 To m Step 2: xgb0(i3) = xg1(i3): Next 'save xg1 into xgb0
        m1 = m: m = m + i2 * 2: ReDim xg1(m) 'save m in m1, new m set to m + (2 * number of cells needed bewteen lines)
        For i3 = 1 To m1 Step 2: xg1(i3) = xgb0(i3): Next 'restore xg1 up to old m
        xg1(m) = xgb1(i1) 'add current line to xg1()
        For i3 = m - 2 To m1 + 2 Step -2 'fill in grid lines between current and previous object boundary lines
        xg1(i3) = xg1(i3 + 2) - dx1: Next
    End If
  Next
  
  'Set last 2 lines in xg1() and fill in cell center values
  ReDim xgb0(m)
  For i3 = 1 To m Step 2: xgb0(i3) = xg1(i3): Next
  m1 = m: m = m + 4: ReDim xg1(m)
  For i3 = 1 To m1 Step 2: xg1(i3) = xgb0(i3): Next
  xg1(m1 + 2) = xgb1(n)
  xg1(m1 + 4) = xg1(m1 + 2) + eps0 * 2
  For i3 = 2 To m - 1 Step 2
  xg1(i3) = (xg1(i3 - 1) + xg1(i3 + 1)) / 2: Next
  Return

gdr3:
  'Set ibc() with cell type values
  If flow_domain = 2 Then
    x1a = 0: y1a = 0: z1a = 0
    If chamber_type = 2 Then
      x1b = lth_r(1): y1b = wdt_r(1): z1b = hgt_r(1)
      x2a = gx_r(2): x2b = gx_r(2) + lth_r(2)
      y2a = gy_r(2): y2b = gy_r(2) + wdt_r(2)
      z2a = gz_r(2): z2b = gz_r(2) + hgt_r(2)
      x3a = gx_r(3): x3b = gx_r(3) + lth_r(3)
      y3a = gy_r(3): y3b = gy_r(3) + wdt_r(3)
      z3a = gz_r(3): z3b = gz_r(3) + hgt_r(3)
    Else
      x1b = lth: y1b = wdt: z1b = hgt
    End If
  End If
  
  For i = 2 To mp Step 2
  For j = 2 To np Step 2
  For k = 2 To lp Step 2
    x0 = xg(i): y0 = yg(j): z0 = zg(k)
    
    If flow_domain = 1 Then
       If x0 <= 0 Or y0 <= 0 Or x0 >= lth Or y0 >= wdt Then
          ibc(i, j, k) = 1
       ElseIf z0 <= 0 Then
          ibc(i, j, k) = 4
       ElseIf z0 >= hgt Then
          ibc(i, j, k) = 1
          If hgt_c > 0 Then
             d1 = wdt / 2
             d2 = (y0 - d1) / d1
             z01 = hgt + hgt_c * (1 - d2 ^ 2)
             If z0 < z01 Then ibc(i, j, k) = 0
          End If
       End If
    Else 'flow_domain = 2, melt
       ibc(i, j, k) = 1
       If x0 > x1a And x0 < x1b And y0 > y1a And y0 < y1b _
           And z0 > z1a And z0 < z1b Then ibc(i, j, k) = 0
       If chamber_type = 2 Then
           If x0 > x2a And x0 < x2b And y0 > y2a And y0 < y2b _
               And z0 > z2a And z0 < z2b Then ibc(i, j, k) = 0
           If x0 > x3a And x0 < x3b And y0 > y3a And y0 < y3b _
               And z0 > z3a And z0 < z3b Then ibc(i, j, k) = 0
           If ((x0 > x3a And x0 < x2b) Or (x0 > x2a And x0 < x1b)) _
              And y0 > y2a And y0 < y2b And z0 > z2b And z0 < z3a _
              Then ibc(i, j, k) = 0
       End If
    End If
  Next: Next: Next
  If flow_domain = 1 Then Return
  
  k = lp: k1 = lp - 2
  For i = 2 To mp Step 2: For j = 2 To np Step 2
    If ibc(i, j, k1) = 0 Then ibc(i, j, k) = 4
  Next: Next
  Return
  
  
gdr4a: 'exhaust on back or front wall
  'Sets position using nearest exiting lines and fills in ibc
  g1 = esg(n): If chamber_type = 2 Then g1 = g1 + gy_r(3)
  g2 = g1 + ewd(n): GoSub gds5y
  ej1(n) = n1: ej2(n) = n2
  g1 = ebg(n): If chamber_type = 2 Then g1 = g1 + gz_r(3)
  g2 = g1 + eht(n): GoSub gds5z
  ek1(n) = n1: ek2(n) = n2
  If ii = 0 Then
    ei1(n) = 2: ei2(n) = 2: i1 = 4: i2 = mp: i0 = 2
  Else
    ei1(n) = mp: ei2(n) = mp: i1 = mp - 2: i2 = 2: i0 = -2
  End If
  For j = ej1(n) To ej2(n) Step 2
  For k = ek1(n) To ek2(n) Step 2
    i = i1 - i0: ibc(i, j, k) = 3: i = i1
    'open up cells in wall between exit and interior
    Do While (ibc(i, j, k) > 0 And i <> i2)
      ibc(i, j, k) = 0
      i = i + i0
    Loop
  Next: Next
  Return
  
gdr4c:
  g1 = esg(n): If chamber_type = 2 Then g1 = g1 + gx_r(3)
  g2 = g1 + ewd(n): GoSub gds5x
  ei1(n) = n1: ei2(n) = n2
  g1 = ebg(n): If chamber_type = 2 Then g1 = g1 + gz_r(3)
  g2 = g1 + eht(n): GoSub gds5z
  ek1(n) = n1: ek2(n) = n2
  If ii = 2 Then
    ej1(n) = 2: ej2(n) = 2: j1 = 4: j2 = np: j0 = 2
  Else
    ej1(n) = np: ej2(n) = np: j1 = np - 2: j2 = 2: j0 = -2
  End If
  For i = ei1(n) To ei2(n) Step 2
  For k = ek1(n) To ek2(n) Step 2
    j = j1 - j0: ibc(i, j, k) = 3: j = j1
    Do While (ibc(i, j, k) > 0 And j <> j2)
      ibc(i, j, k) = 0
      j = j + j0
    Loop
  Next: Next
  Return
  
gdr4e:
  g1 = esg(n): If chamber_type = 2 Then c0 = c0 + gx_r(3)
  g2 = g1 + ewd(n): GoSub gds5x
  ei1(n) = n1: ei2(n) = n2
  g1 = ebg(n): If chamber_type = 2 Then g1 = g1 + gy_r(3)
  g2 = g1 + eht(n): GoSub gds5y
  ej1(n) = n1: ej2(n) = n2
  If ii = 4 Then
    ek1(n) = 2: ek2(n) = 2: k1 = 4: k2 = lp: k0 = 2
  Else
    ek1(n) = lp: ek2(n) = lp: k1 = lp - 2: k2 = 2: k0 = -2
  End If
  For i = ei1(n) To ei2(n) Step 2
  For j = ej1(n) To ej2(n) Step 2
    k = k1 - k0: ibc(i, j, k) = 3: k = k1
    Do While (ibc(i, j, k) > 0 And k <> k2)
      ibc(i, j, k) = 0
      k = k + k0
    Loop
  Next: Next
  Return
  
gdr5a:  'burner back wall
  i1 = 4: i2 = mp: i0 = 2
  GoTo gdr5ab

gdr5b:  'burner front wall
  i1 = mp - 2: i2 = 2: i0 = -2
gdr5ab:
  g1 = bsg0: g2 = g1 + bwd0: GoSub gds5y
  bj1(n) = n1: bj2(n) = n2: j = n1
  g1 = bbg0: g2 = g1 + bht0: GoSub gds5z
  bk1(n) = n1: bk2(n) = n2: k = n1
  i = i1
  Do While (ibc(i, j, k) > 0 And i <> i2)
    i = i + i0
  Loop
  i = i - i0: bi1(n) = i: bi2(n) = i
  yg01 = bsg0 + r0: zg01 = bbg0 + r0
  For j = bj1(n) To bj2(n) Step 2
  For k = bk1(n) To bk2(n) Step 2
    If bty(n) <= 1 Then
      g0 = (yg(j) - yg01) ^ 2 + (zg(k) - zg01) ^ 2
      If g0 < r0 ^ 2 Then ibc(i, j, k) = 2
    Else
      ibc(i, j, k) = 2
    End If
    If ibc(i, j, k) = 2 Then
    For ii = i + i0 To i2 Step i0
      If ibc(ii, j, k) <= 0 Then Exit For
      ibc(ii, j, k) = 0
    Next
    End If
  Next: Next
  Return

gdr5c:  'Burner right wall
  i1 = 4: i2 = np: i0 = 2
  GoTo gdr5cd

gdr5d:  'burner left wall
  i1 = np - 2: i2 = 2: i0 = -2
gdr5cd:
  g1 = bsg0: g2 = g1 + bwd0: GoSub gds5x
  bi1(n) = n1: bi2(n) = n2: i = n1
  g1 = bbg0: g2 = g1 + bht0: GoSub gds5z
  bk1(n) = n1: bk2(n) = n2: k = n1
  j = i1
  Do While (ibc(i, j, k) > 0 And j <> i2)
    j = j + i0
  Loop
  j = j - i0: bj1(n) = j: bj2(n) = j
  xg01 = bsg0 + r0: zg01 = bbg0 + r0
  For i = bi1(n) To bi2(n) Step 2
  For k = bk1(n) To bk2(n) Step 2
    If bty(n) <= 1 Then
      g0 = (xg(i) - xg01) ^ 2 + (zg(k) - zg01) ^ 2
      If g0 < r0 ^ 2 Then ibc(i, j, k) = 2
    Else
      ibc(i, j, k) = 2
    End If
    If ibc(i, j, k) = 2 Then
    For jj = j + i0 To i2 Step i0
      If ibc(i, jj, k) <= 0 Then Exit For
      ibc(i, jj, k) = 0
    Next
    End If
  Next: Next
  Return

gdr5e:  'burner bottom wall
  i1 = 4: i2 = lp: i0 = 2
  GoTo gdr5ef

gdr5f:  'burner top wall
  i1 = lp - 2: i2 = 2: i0 = -2
gdr5ef:
  g1 = bsg0: g2 = g1 + bwd0: GoSub gds5x
  bi1(n) = n1: bi2(n) = n2: i = n1
  g1 = bbg0: g2 = g1 + bht0: GoSub gds5y
  bj1(n) = n1: bj2(n) = n2: j = n1
  k = i1
  Do While (ibc(i, j, k) > 0 And k <> i2)
    k = k + i0
  Loop
  k = k - i0: bk1(n) = k: bk2(n) = k
  xg01 = bsg0 + r0: yg01 = bbg0 + r0
  For i = bi1(n) To bi2(n) Step 2
  For j = bj1(n) To bj2(n) Step 2
    If bty(n) <= 1 Then
      g0 = (xg(i) - xg01) ^ 2 + (yg(j) - yg01) ^ 2
      If g0 < r0 ^ 2 Then ibc(i, j, k) = 2
    Else
      ibc(i, j, k) = 2
    End If
    If ibc(i, j, k) = 2 Then
    For kk = k + i0 To i2 Step i0
      If ibc(i, j, kk) <= 0 Then Exit For
      ibc(i, j, kk) = 0
    Next
    End If
  Next: Next
  Return

gds5x:
  j1 = 0
  For m = 2 To mp Step 2
    If g1 <= xg(m) And j1 = 0 Then
      n1 = m: j1 = 1
    ElseIf g2 <= xg(m) Then
      n2 = m - 2: Return
    End If
  Next
  n1 = mp: n2 = mp
Return

gds5y:
  'Set n1=first y-line and n2=last y-line within object (else both=np)
  'Input g1=start y edge, g2=end y edge
  'Uses yg & np
  j1 = 0
  For m = 2 To np Step 2
    If g1 <= yg(m) And j1 = 0 Then
      n1 = m: j1 = 1
    ElseIf g2 <= yg(m) Then
      n2 = m - 2: Return
    End If
  Next
  n1 = np: n2 = np
Return

gds5z:
  j3 = 0
  For m = 2 To lp Step 2
    If g1 <= zg(m) And j3 = 0 Then
      n1 = m: j3 = 1
    ElseIf g2 <= zg(m) Then
      n2 = m - 2: Return
    End If
  Next
  n1 = lp: n2 = lp
Return
End Sub

'-------------------------------
'Plot the plain grid (with no output)
'-------------------------------
Private Sub plt_gd(n_p As Integer)
  s0 = LCase(frmMain.Caption)
  'If InStr(s0, "post") > 0 Then
  If InStr(s0, "grid") <= 0 Then ' 10-8-2004
    'assume caption contains the word "post"
    Call plt_clr(n_p)
    Exit Sub
  End If

  'caption contains the word "grid"
  Cls
  frmMain.Picture = LoadPicture()
  GoSub pgds3 'display current grid location in List1 box
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  k1 = nxb(3) - 1: k2 = nxe(3) + 1
  Call nodetype
  If Lb3d.Caption = "2D" Then
    GoSub pgds2: Call axes_plt: Exit Sub
  End If
  Call axes_plt
  dx = (xg(i2) - xg(i1)) * zf1
  p01 = p0 + gap: q01 = q0 - gap
  p02 = p01 + dx + gap * 2
  dy = (yg(j2) - yg(j1)) * zf2
  q02 = q01 - dy - gap
  dz = (zg(k2) - zg(k1)) * zf3
  LbP.Left = p01 + dx + gap: LbP.Top = q02
  If LbP.Left > wdts - LbP.Width Then LbP.Left = wdts - LbP.Width
  If LbP.Top < 0 Then LbP.Top = 0
  LbP.Visible = True
'
  p1 = p01
  For i = nxb(1) To nxe(1) Step 2
    dx = (xg(i + 1) - xg(i1)) * zf1
    p2 = p01 + dx
    q1 = q01: k = iz0
    For j = nxb(2) To nxe(2) Step 2
      dy = (yg(j + 1) - yg(j1)) * zf2
      q2 = q01 - dy
      GoSub pgds1
      q1 = q2
    Next
    q1 = q02: j = iy0
    For k = nxb(3) To nxe(3) Step 2
      dz = (zg(k + 1) - zg(k1)) * zf3
      q2 = q02 - dz
      GoSub pgds1
      q1 = q2
    Next
    p1 = p2
  Next
  p1 = p02: i = ix0
  For k = nxb(3) To nxe(3) Step 2
    dz = (zg(k + 1) - zg(k1)) * zf3
    p2 = p02 + dz
    q1 = q01
    For j = nxb(2) To nxe(2) Step 2
      dy = (yg(j + 1) - yg(j1)) * zf2
      q2 = q01 - dy
      GoSub pgds1
      q1 = q2
    Next
    p1 = p2
  Next
'
  dy = (yg(j2) - yg(j1)) * zf2
  dz = (zg(k2) - zg(k1)) * zf3
  ForeColor = RGB(150, 150, 150)
  For i = i1 To i2 Step 2
    dx = (xg(i) - xg(i1)) * zf1
    p1 = p01 + dx: p2 = p1
    q1 = q01: q2 = q1 - dy
    rgb0 = RGB(150, 150, 150)
    Line (p1, q1)-(p2, q2), rgb0
    q1 = q02: q2 = q02 - dz
    Line (p1, q1)-(p2, q2), rgb0
  Next
  For j = j1 To j2 Step 2
    dy = (yg(j) - yg(j1)) * zf2
    q1 = q01 - dy: q2 = q1
    p1 = p01: p2 = p1 + dx
    rgb0 = RGB(150, 150, 150)
    Line (p1, q1)-(p2, q2), rgb0
    p1 = p02: p2 = p1 + dz
    Line (p1, q1)-(p2, q2), rgb0
  Next
  For k = k1 To k2 Step 2
    dz = (zg(k) - zg(k1)) * zf3
    q1 = q02 - dz: q2 = q1
    p1 = p01: p2 = p1 + dx
    rgb0 = RGB(150, 150, 150)
    Line (p1, q1)-(p2, q2), rgb0
    p1 = p02 + dz: p2 = p1
    q1 = q01: q2 = q1 - dy
    Line (p1, q1)-(p2, q2), rgb0
  Next
  CurrentX = 4350: CurrentY = 7700
  ForeColor = vbBlack: Font.Size = 12
  Exit Sub
'
pgds0:
  If i = ix0 And j = iy0 And k = iz0 Then
    rgb0 = vbYellow
  ElseIf ibc(i, j, k) = 2 Then
    rgb0 = vbGreen
  ElseIf ibc(i, j, k) = 3 Then
    rgb0 = vbRed
  ElseIf ibc(i, j, k) = 4 Then
    rgb0 = vbCyan
  ElseIf ibc(i, j, k) = 5 Then
    rgb0 = RGB(180, 0, 0)
  ElseIf ibc(i, j, k) = 8 Then
    rgb0 = RGB(0, 180, 0)
  ElseIf ibc(i, j, k) = 1 Then
    rgb0 = vbBlack
  Else
    rgb0 = BackColor
  End If
  Return

pgds1:
  GoSub pgds0
  Line (p1, q1)-(p2, q2), rgb0, BF
  Return

pgds2:
  ForeColor = RGB(200, 200, 200)
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k2): proj2d
  PSet (p1, q1)
  x0 = xg(i2): proj2d: Line -(p1, q1)
  y0 = yg(j2): proj2d: Line -(p1, q1)
  LbP.Left = p1 + gap: LbP.Top = q1 - gap
  If LbP.Left > wdts - LbP.Width Then _
    LbP.Left = wdts - LbP.Width
  If LbP.Top < 0 Then LbP.Top = 0
  x0 = xg(i1): proj2d: Line -(p1, q1)
  y0 = yg(j1): proj2d: Line -(p1, q1)
  y0 = yg(j2): proj2d: PSet (p1, q1)
  z0 = zg(k1): proj2d: Line -(p1, q1)
  x0 = xg(i2): proj2d: Line -(p1, q1)
  y0 = yg(j1): proj2d: Line -(p1, q1)
  z0 = zg(k2): proj2d: Line -(p1, q1)
  y0 = yg(j2): proj2d: PSet (p1, q1)
  z0 = zg(k1): proj2d: Line -(p1, q1)
'
  x0 = xg(i1)
  For j = j1 To j2 Step 2
    y0 = yg(j): z0 = zg(k1): proj2d: PSet (p1, q1)
    z0 = zg(k2): proj2d: Line -(p1, q1)
  Next
  For k = k1 To k2 Step 2
    z0 = zg(k): y0 = yg(j1): proj2d: PSet (p1, q1)
    y0 = yg(j2): proj2d: Line -(p1, q1)
  Next
  y0 = yg(j2)
  For i = i1 To i2 Step 2
    x0 = xg(i): z0 = zg(k1): proj2d: PSet (p1, q1)
    z0 = zg(k2): proj2d: Line -(p1, q1)
  Next
  For k = k1 To k2 Step 2
    z0 = zg(k): x0 = xg(i1): proj2d: PSet (p1, q1)
    x0 = xg(i2): proj2d: Line -(p1, q1)
  Next
  z0 = zg(k1)
  For i = i1 To i2 Step 2
    x0 = xg(i): y0 = yg(j1): proj2d: PSet (p1, q1)
    y0 = yg(j2): proj2d: Line -(p1, q1)
  Next
  For j = j1 To j2 Step 2
    y0 = yg(j): x0 = xg(i1): proj2d: PSet (p1, q1)
    x0 = xg(i2): proj2d: Line -(p1, q1)
  Next
  ForeColor = RGB(100, 100, 100)
  If Optx Then GoSub pgds2a
  If Opty Then GoSub pgds2b
  If Optz Then GoSub pgds2c
  Return

pgds2a:
  x0 = xg(ix0): i = ix0
  For j = nxb(2) To nxe(2) Step 2
  ya1 = yg(j - 1): ya2 = yg(j + 1)
  For k = nxb(3) To nxe(3) Step 2
    za1 = zg(k - 1): za2 = zg(k + 1)
    y0 = ya1: z0 = za1: proj2d
    p1b = p1: q1b = q1
    y0 = ya1: z0 = za2: proj2d
    p2b = p1: q2b = q1
    y0 = ya2: z0 = za1: proj2d
    p1e = p1: q1e = q1
    y0 = ya2: z0 = za2: proj2d
    p2e = p1: q2e = q1
    GoSub pgds2a1
  Next: Next
  For j = j1 To j2 Step 2
    y0 = yg(j): z0 = zg(k1): proj2d
    p2 = p1: q2 = q1
    z0 = zg(k2): proj2d: Line (p1, q1)-(p2, q2)
  Next
  For k = k1 To k2 Step 2
    z0 = zg(k): y0 = yg(j1): proj2d
    p2 = p1: q2 = q1
    y0 = yg(j2): proj2d: Line (p1, q1)-(p2, q2)
  Next
  Return

pgds2a1:
    GoSub pgds0: g1 = p1e - p1b
    If g1 < 10 Then
      Line (p1b, q1b)-(p2e, q2e), rgb0
    Else
      For p1 = p1b To p1e Step 10
        g0 = (p1 - p1b) / g1
        p2 = p2b + g0 * (p2e - p2b)
        q1 = q1b + g0 * (q1e - q1b)
        q2 = q2b + g0 * (q2e - q2b)
        Line (p1, q1)-(p2, q2), rgb0
    Next: End If
  Return

pgds2b:
  y0 = yg(iy0): j = iy0
  For i = nxb(1) To nxe(1) Step 2
  xa1 = xg(i - 1): xa2 = xg(i + 1)
  For k = nxb(3) To nxe(3) Step 2
    za1 = zg(k - 1): za2 = zg(k + 1)
    x0 = xa1: z0 = za1: proj2d
    p1b = p1: q1b = q1
    x0 = xa1: z0 = za2: proj2d
    p2b = p1: q2b = q1
    x0 = xa2: z0 = za1: proj2d
    p1e = p1: q1e = q1
    x0 = xa2: z0 = za2: proj2d
    p2e = p1: q2e = q1
    GoSub pgds2a1
  Next: Next
  For i = i1 To i2 Step 2
    x0 = xg(i): z0 = zg(k1): proj2d
    p2 = p1: q2 = q1
    z0 = zg(k2): proj2d: Line (p1, q1)-(p2, q2)
  Next
  For k = k1 To k2 Step 2
    z0 = zg(k): x0 = xg(i1): proj2d
    p2 = p1: q2 = q1
    x0 = xg(i2): proj2d: Line (p1, q1)-(p2, q2)
  Next
  Return

pgds2c:
  z0 = zg(iz0): k = iz0
  For i = nxb(1) To nxe(1) Step 2
  xa1 = xg(i - 1): xa2 = xg(i + 1)
  For j = nxb(2) To nxe(2) Step 2
    ya1 = yg(j - 1): ya2 = yg(j + 1)
    x0 = xa1: y0 = ya1: proj2d
    p1b = p1: q1b = q1
    x0 = xa1: y0 = ya2: proj2d
    p2b = p1: q2b = q1
    x0 = xa2: y0 = ya1: proj2d
    p1e = p1: q1e = q1
    x0 = xa2: y0 = ya2: proj2d
    p2e = p1: q2e = q1
    GoSub pgds2a1
  Next: Next
  For i = i1 To i2 Step 2
    x0 = xg(i): y0 = yg(j1): proj2d
    p2 = p1: q2 = q1
    y0 = yg(j2): proj2d: Line (p1, q1)-(p2, q2)
  Next
  For j = j1 To j2 Step 2
    y0 = yg(j): x0 = xg(i1): proj2d
    p2 = p1: q2 = q1
    x0 = xg(i2): proj2d: Line (p1, q1)-(p2, q2)
  Next
  Return

pgds3: 'display current grid location in List1 box
  's0 = LCase(Caption)
  'If InStr(s0, "post") > 0 Then Return
  'If InStr(s0, "running") > 0 Then Return
  If menu_layer = InGridConstruction Then
    List1.Clear
    For i = 0 To 4: List1.AddItem "": Next
    List1.List(0) = "Location"
    i = ix0 - 1: Call unit_l(xg(i), s0)
    List1.List(1) = "x(" & i & ")=" & s0
    i = iy0 - 1: Call unit_l(yg(i), s0)
    List1.List(2) = "y(" & i & ")=" & s0
    i = iz0 - 1: Call unit_l(zg(i), s0)
    List1.List(3) = "z(" & i & ")=" & s0
  End If
  Return
End Sub

'-------------------------------
'-------------------------------
Private Sub axes_plt()
  ForeColor = vbBlack
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  k1 = nxb(3) - 1: k2 = nxe(3) + 1
  dx = (xg(i2) - xg(i1)) * zf1
  dy = (yg(j2) - yg(j1)) * zf2
  dz = (zg(k2) - zg(k1)) * zf3
  If Lb3d.Caption = "2D" Then
    GoSub axes2
  Else
    GoSub axes1
  End If
  Exit Sub

axes1:
  p1 = p0 + gap: q1 = q0
  p2 = p1 + dx: If p2 > wdts Then p2 = wdts
  q2 = q1
  arrow
  zf0 = zf1: GoSub axes1a
  Lb1.Caption = "x": Lb1.Font.Size = 12
  Lb1.Left = (p1 + p2) / 2
  Lb1.Top = q1 + 100
  Lb1.Visible = True
  p1 = p2 + gap * 2: p2 = p1 + dz
  If p2 > wdts Then p2 = wdts
  arrow
  zf0 = zf3: GoSub axes1a
  Lb4.Caption = "z": Lb4.Font.Size = 12
  Lb4.ForeColor = vbBlue
  Lb4.Left = (p1 + p2) / 2
  Lb4.Top = q1 + 100
  Lb4.Visible = True
  CurrentX = Lb4.Left: CurrentY = Lb4.Top
  Print "z"
  p1 = p0: q1 = q0 - gap
  p2 = p1: q2 = q1 - dy
  If q2 < 0 Then q2 = 0
  arrow
  zf0 = zf2: GoSub axes1b
  Lb2.Caption = "y": Lb2.Font.Size = 12
  Lb2.Left = p1 - 200: Lb2.Top = (q1 + q2) / 2
  Lb2.Visible = True
  q1 = q2 - gap: q2 = q1 - dz
  If q2 < 0 Then q2 = 0
  arrow
  zf0 = zf3: GoSub axes1b
  Lb3.Caption = "z": Lb3.Font.Size = 12
  Lb3.Left = p1 - 200
  Lb3.Top = (q1 + q2) / 2
  Lb3.Visible = True
  GoSub axes3x
  p01 = p0 + gap: p02 = p01 + dx + gap * 2
  q01 = q0 - gap: q02 = q01 - dy - gap
  dx = (xg(ix0 - 1) - xg(i1)) * zf1
  dy = (yg(iy0 - 1) - yg(j1)) * zf2
  dz = (zg(iz0 - 1) - zg(k1)) * zf3
  p1 = p01 + dx: p2 = p1
  q1 = q0: q2 = q1 - gap
  If Optx Then rgb0 = vbMagenta Else rgb0 = vbBlue
  Line (p1, q1)-(p2, q2), rgb0
  p1 = p02 + dz: p2 = p1
  If Optz Then rgb0 = vbMagenta Else rgb0 = vbBlue
  Line (p1, q1)-(p2, q2), rgb0
  p1 = p0: p2 = p01
  q1 = q01 - dy: q2 = q1
  If Opty Then rgb0 = vbMagenta Else rgb0 = vbBlue
  Line (p1, q1)-(p2, q2), rgb0
  q1 = q02 - dz: q2 = q1
  If Optz Then rgb0 = vbMagenta Else rgb0 = vbBlue
  Line (p1, q1)-(p2, q2), rgb0
  ForeColor = vbBlack
  Return

axes1a:
  p3 = p1: q3 = q0 - gap / 2
axes1a1:
  Line (p3, q0)-(p3, q3)
  If opt11.Checked Then
    p3 = p3 + zf0 * 0.3048
  Else
    p3 = p3 + zf0
  End If
  If (p2 - p3) <= 200 Then Return
  GoTo axes1a1

axes1b:
  q3 = q1: p3 = p0 + gap / 2

axes1b1:
  Line (p0, q3)-(p3, q3)
  If opt11.Checked Then
    q3 = q3 - zf0 * 0.3048
  Else
    q3 = q3 - zf0
  End If
  If (q3 - q2) <= 200 Then Return
  GoTo axes1b1

axes2:
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k1): proj2d
  p01 = p1: q01 = q1
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k2): GoSub axes2a
  Lb3.Left = p1 - 200
  Lb3.Top = (q1 + q2) / 2
  x0 = xg(i2): y0 = yg(j1): z0 = zg(k1): GoSub axes2a
  Lb1.Left = (p1 + p2) / 2: Lb1.Top = q1 + 100
  p01 = p2: q01 = q2
  x0 = xg(i2): y0 = yg(j2): z0 = zg(k1): GoSub axes2a
  y0 = (yg(j1) + yg(j2)) / 2: proj2d
  Lb2.Left = p1 + 100: Lb2.Top = q1
  GoSub axes3x
  Lb4.Visible = False
  If opt11.Checked Then
    g0 = 0.3048
  Else
    g0 = 1
  End If
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k1)
  Do While x0 < xg(mp) - 200 / zf1
    proj2d
    q2 = q1 + 100: Line (p1, q1)-(p1, q2)
    x0 = x0 + g0
  Loop
  x0 = xg(i2): y0 = yg(j1)
  Do While y0 < yg(np) - 200 / zf2
    proj2d
    q2 = q1 + 100: Line (p1, q1)-(p1, q2)
    y0 = y0 + g0
  Loop
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k1)
  Do While z0 < zg(lp) - 200 / zf3
    proj2d
    p2 = p1 - 100: Line (p1, q1)-(p2, q1)
    z0 = z0 + g0
  Loop
  Return

axes2a:
  proj2d
  p2 = p1: q2 = q1: p1 = p01: q1 = q01: arrow
  Return

axes3x:
  CurrentX = Lb1.Left: CurrentY = Lb1.Top
  frmMain.Font.Size = 12
  If opt11.Checked Then
    Print "x (ft)"
  Else
    Print "x (m)"
  End If
  CurrentX = Lb2.Left: CurrentY = Lb2.Top
  Print "y"
  CurrentX = Lb3.Left: CurrentY = Lb3.Top
  Print "z"
  Return
End Sub

'-------------------------------
'-------------------------------
Private Sub arrow()
  Line (p1, q1)-(p2, q2)
  a_d = Sqr((p2 - p1) ^ 2 + (q2 - q1) ^ 2)
  If a_d < 1 Then Exit Sub
  a_h = 200: a_w = 0.5 * a_h: f = a_h / a_d
  p5 = p2 - f * (p2 - p1): q5 = q2 - f * (q2 - q1)
  p3 = p5 - a_w / 2 * (q2 - q1) / a_d
  q3 = q5 + a_w / 2 * (p2 - p1) / a_d
  p4 = p5 + a_w / 2 * (q2 - q1) / a_d
  q4 = q5 - a_w / 2 * (p2 - p1) / a_d
  For T = 0 To 1 Step 0.1
    p = p3 + T * (p4 - p3)
    q = q3 + T * (q4 - q3)
    Line (p, q)-(p2, q2)
  Next
End Sub


'-------------------------------
'-------------------------------
Private Sub Form_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
  If Optx.Visible Then
    n1 = 0
    i1 = nxb(1) - 1: i2 = nxe(1) + 1
    j1 = nxb(2) - 1: j2 = nxe(2) + 1
    k1 = nxb(3) - 1: k2 = nxe(3) + 1
    dx = (xg(i2) - xg(i1)) * zf1
    dy = (yg(j2) - yg(j1)) * zf2
    dz = (zg(k2) - zg(k1)) * zf3
    GoSub fms3
    If Lb3d = "3D" Then GoTo fms1
    If Lb3d = "2D" Then GoTo fms2
  End If
  Exit Sub

fms1:
  p01 = Lb1.Left + Lb1.Width: p02 = p01 + 400
  q01 = Lb1.Top: q02 = q01 + Lb1.Height
  If x > p01 And x < p02 And y > q01 And y < q02 Then
    If opt11.Checked Then
      opt12_Click
    Else
      opt11_Click
    End If
  End If
  p01 = p0 + gap: q01 = q0 - gap
  p02 = p01 + dx + gap * 2
  q02 = q01 - dy - gap
  p01e = p01 + dx + 50: q01e = q02 - dz
  If x >= p01 - 50 And x <= p01e And y <= q0 And y >= q01e Then
    xg0 = (x - p01) / zf1 + xg(i1)
    For i = i1 To i2 Step 2
      If xg0 <= xg(i) Then GoTo fm1
    Next

fm1:
    i = i - 1: If i < nxb(1) Then i = nxb(1)
    If i > nxe(1) Then i = nxe(1)
    ix0 = i: tx_xc = ix0
    n1 = 0: Call plt_gd(1)
  End If
  p02e = p02 + dz: q01e = q01 - dy - 50
  If y <= q01 And y >= q01e And x >= p0 And x <= p02e Then
    yg0 = (q01 - y) / zf2 + yg(j1)
    For i = j1 To j2 Step 2
      If yg0 <= yg(i) Then GoTo fm2
    Next

fm2:
    i = i - 1: If i < nxb(2) Then i = nxb(2)
    If i > nxe(2) Then i = nxe(2)
    iy0 = i: tx_yc = iy0
    n1 = 0: Call plt_gd(2)
  End If
  q02e = q02 - dz - 50
  If y <= q02 + 50 And y >= q02e And x >= p0 And x <= p01e Then
    zg0 = (q02 - y) / zf3 + zg(k1)
    For i = k1 To k2 Step 2
      If zg0 <= zg(i) Then GoTo fm3
    Next

fm3:
    i = i - 1: If i < nxb(3) Then i = nxb(3)
    If i > nxe(3) Then i = nxe(3)
    iz0 = i: tx_zc = iz0
    n1 = 0: Call plt_gd(3)
  End If
  p02e = p02 + dz + 50
  If x > p02 - 50 And x <= p02e And y <= q0 And y >= q01e Then
    zg0 = (x - p02) / zf3 + zg(k1)
    For i = k1 To k2 Step 2
      If zg0 <= zg(i) Then GoTo fm4
    Next

fm4:
    i = i - 1: If i < nxb(3) Then i = nxb(3)
    If i > nxe(3) Then i = nxe(3)
    iz0 = i: tx_zc = iz0
    n1 = 0: Call plt_gd(3)
  End If
  Exit Sub

fms2:
  p01 = Lb1.Left + Lb1.Width: p02 = p01 + 400
  q01 = Lb1.Top: q02 = q01 + Lb1.Height
  If x > p01 And x < p02 And y > q01 And y < q02 Then
    If opt11.Checked Then
      opt12_Click
    Else
      opt11_Click
    End If
    Exit Sub
  End If
  If Optx Then GoTo fms2a
  If Opty Then GoTo fms2b
  If Optz Then GoTo fms2c
  Exit Sub

fms2a:
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k1)
  Call proj2d: p01 = p1: q01 = q1
  x0 = xg(i2): y0 = yg(j1): z0 = zg(k1)
  Call proj2d: p02 = p1
  If y >= q01 Then
    If x < p01 Or x > p02 Then Exit Sub
    i = i1 - 1: p1 = p01
    Do While (x > p1 And i < i2 - 2)
      i = i + 2: x0 = xg(i): proj2d
    Loop
    If i <> ix0 Then ix0 = i: tx_xc = i: Call plt_gd(0)
  Else
    p1 = x: q1 = y
    xx = xg(ix0) * zf1
    yy = (p1 - p0 - xx) / Sin(theta)
    zz = q0 - q1 - yy * Cos(theta)
    GoSub fms0y: GoSub fms0z
    If j > j1 And j < j2 And k > k1 And k < k2 Then
      iy0 = j: tx_yc = j
      iz0 = k: tx_zc = k
      Call plt_gd(0)
    End If
  End If
  Exit Sub

fms2b:
  x0 = xg(i2): y0 = yg(j1): z0 = zg(k1)
  Call proj2d: p01 = p1: q01 = q1
  x0 = xg(i2): y0 = yg(j2): z0 = zg(k1)
  Call proj2d: p02 = p1: q02 = q1
  g0 = (q01 - y) / (q01 - q02)
  p1 = p01 + g0 * (p02 - p01)
  If x >= p1 Then
    g0 = (x - p1) * Cos(theta)
    p2 = p1 + g0 * Cos(theta)
    q2 = y - g0 * Sin(theta)
    If p2 >= p01 And p2 <= p02 Then
      j = j1 - 1: q1 = q01: n1 = 1
      Do While (q2 < q1 And j < j2 - 2)
        j = j + 2: y0 = yg(j): proj2d
      Loop
      iy0 = j: tx_yc = iy0
      Call plt_gd(0)
    End If
  Else
    p1 = x: q1 = y
    yy = yg(iy0) * zf2
    xx = p1 - p0 - yy * Sin(theta)
    zz = q0 - q1 - yy * Cos(theta)
    GoSub fms0x: GoSub fms0z
    If i > i1 And i < i2 And k > k1 And k < k2 Then
      ix0 = i: tx_xc = i
      iz0 = k: tx_zc = k
      Call plt_gd(0)
    End If
  End If
  Exit Sub

fms2c:
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k1)
  Call proj2d: p01 = p1: q01 = q1
  x0 = xg(i1): y0 = yg(j1): z0 = zg(k2)
  Call proj2d: q02 = q1
  If x <= p01 Then
    If y > q01 Or y < q02 Then Exit Sub
    k = k1 - 1: q1 = q01: n1 = 1
    Do While (y < q1 And k < k2 - 2)
      k = k + 2: z0 = zg(k): proj2d
    Loop
    iz0 = k: tx_zc = iz0
    Call plt_gd(0)
  Else
    p1 = x: q1 = y
    zz = zg(iz0) * zf3
    yy = (q0 - q1 - zz) / Cos(theta)
    xx = p1 - p0 - yy * Sin(theta)
    GoSub fms0x: GoSub fms0y
    If i > i1 And i < i2 And j > j1 And j < j2 Then
      ix0 = i: tx_xc = i
      iy0 = j: tx_yc = j
      Call plt_gd(0)
    End If
  End If
  Exit Sub

fms0x:
  x0 = xx / zf1
  If x0 < xg(i1) Or x0 > xg(i2) Then i = 0: Return
  i = i1
  Do While (x0 > xg(i) And i < i2 - 1)
    i = i + 2
  Loop
  i = i - 1
  Return

fms0y:
  y0 = yy / zf2
  If y0 < yg(j1) Or y0 > yg(j2) Then j = 0: Return
  j = j1
  Do While (y0 > yg(j) And j < j2 - 1)
    j = j + 2
  Loop
  j = j - 1
  Return

fms0z:
  z0 = zz / zf3
  If z0 < zg(k1) Or z0 > zg(k2) Then k = 0: Return
  k = k1
  Do While (z0 > zg(k) And k < k2 - 1)
    k = k + 2
  Loop
  k = k - 1
  Return

fms3:
  If InStr(frmMain.Caption, "grid") > 0 Then Return
  p01 = LbP.Left + gap * 2 + 250
  p02 = p01 + 300
  q01 = LbP.Top + LbP.Height: q02 = q01 - 200
  If x > p01 And x < p02 And y < q01 And y > q02 Then
      optpltbd2_Click
  End If
  q01 = q01 - dz: q02 = q01 - 200: p02 = p02 + 200
  If x > p01 And x < p02 And y < q01 And y > q02 Then
      optpltbd1_Click
  End If
  Return
End Sub

'-------------------------------
'-------------------------------
Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
  Lb14.Caption = x & "," & y
End Sub

'-------------------------------
'Routine to display warning message that files may be lost
'when close button is clicked on the main window.
'Do same action for any UnloadMode.
' 7-18-2004 added routine
'-------------------------------
Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
Dim sMessage As String
    If menu_layer = InGridConstruction Or menu_layer = InPreProc Then
        If (flag_preproc_modified = True Or flag_grid_modified = True Or _
                flag_grid_constructed = True Or flag_conditions_modified = True Or _
                flag_grid_enhanced = True) And case_number <> "" Then
            sMessage = "Do you want the current case saved before exiting " _
                & "the GFM program?"
            If MsgBox(sMessage, vbYesNo + vbCritical, _
                    "GFM Warning about Loss of User Work") = vbYes Then
                'Automatically save the case for user
                Call mnuCaseSave_Click
            Else 'no save done
                'user is responsible for deleting any new, unsaved case
            End If
        End If
        Cancel = 0 'end program
        
    ElseIf menu_layer = InSimulation And Lbsw.Caption = "cont" Then
        Cancel = 0 'end program
        
    ElseIf menu_layer = InSimulation Then
        'Display determine-how-to-close form, so user can choose close option
        If Lbsw.Caption = "stop" Then
            frmCloseHow.LbChoose.Caption = "A simulation run is in progress.  Your stopping criteria has " _
                & "not been met. Choose one of the following options:"
        Else
            frmCloseHow.LbChoose.Caption = "A request has already been sent to save results and stop" _
                & " the simulation. There may be a delay before shutdown operations are complete." _
                & " Choose one of the following options:"
        End If
        frmCloseHow.Show 1
    
        If modVariables.c_return = 1 Or modVariables.c_close = 1 Then
            'User closed the CloseHow form, assume ignore request is desired
            'Or user clicked ignore button
            'Allow user to stay in simulation
            Cancel = 1 'do not end program
            Exit Sub
        End If
        
        If modVariables.c_close = 2 Then
            'User clicked save button
            'signal CDF code to save results and stop
            If flow_domain = 1 Then
                s0 = dnm & "combustion\runs.dat"
            Else
                s0 = dnm & "melt\runs.dat"
            End If
            FileCopy s0, case_path & "\runstop.dat"
            Cancel = 1 'do not end program
            cancelMode = True
            userStoppedSim = True
            Lbsw.BackColor = vbYellow: Lbsw.Caption = "ending"
            sMessage = "There may be a short delay before the CFD simulation program" _
                     & " completes shutdown operations."
            If MsgBox(sMessage, vbOKOnly + vbInformation, "GFM") = vbOK Then
                'wait for CFD shutdown
                Exit Sub
            End If
        End If
    
        If modVariables.c_close = 3 Then
            'User clicked abort button
            'shut down CDF code
            EndShelledProcess (cfd_task_id)
        End If
    '--------------------------------------
    GoTo fqu
    
    
        If Lbsw.Caption = "stop" Then
            If MsgBox("A simulation run is in progress.  Your stopping criteria has " _
              & "not been met.  Are you sure you want to save results and stop?", _
              vbYesNo + vbQuestion, "GFM") = vbNo Then
                'Allow user to stay in simulation
                Cancel = 1 'do not end program
                Exit Sub
            Else
                'User wants to cancel
                'signal CDF code to stop
                If flow_domain = 1 Then
                    s0 = dnm & "combustion\runs.dat"
                Else
                    s0 = dnm & "melt\runs.dat"
                End If
                FileCopy s0, case_path & "\runstop.dat"
                Cancel = 1 'do not end program
                cancelMode = True
                userStoppedSim = True
                Lbsw.BackColor = vbYellow: Lbsw.Caption = "ending"
                sMessage = "There may be a short delay before the CFD simulation program" _
                         & " completes shutdown operations."
                If MsgBox(sMessage, vbOKOnly + vbInformation, "GFM") = vbOK Then
                    'wait for CFD shutdown
                    Exit Sub
                End If
            End If
        ElseIf Lbsw.Caption = "ending" Then
            Cancel = 1 'do not end program
            cancelMode = True
            userStoppedSim = True
            Lbsw.BackColor = vbYellow: Lbsw.Caption = "ending"
            sMessage = "There may be a short delay before the CFD simulation program" _
                     & " completes shutdown operations."
            If MsgBox(sMessage, vbOKOnly + vbInformation, "GFM") = vbOK Then
                'wait for CFD shutdown
                Exit Sub
            End If
        Else
            Cancel = 0 'end program
        End If
        
        
'----------------------------------
fqu:
    End If
    Cancel = 0 'end program
    
    'Do clean-up and end program
    On Error Resume Next
    Kill dnm & "tmp\*.*"
    If case_path <> "" Then
        s0 = case_path & "\"
        If FileExist(s0 & "gfm.dat") Then Kill s0 & "gfm.dat"
        If FileExist(s0 & "gfm0.dat") Then Kill s0 & "gfm0.dat"
        If FileExist(s0 & "runstop.dat") Then Kill s0 & "runstop.dat"
    End If
    End
End Sub

'-------------------------------
'-------------------------------
Private Sub help_Click()
        
    frmHelp.Show 1 'display form so user can select document to view
    Exit Sub
    
    
    'old version

    If MsgBox("                        THE HELP SYSTEM IS CURRENTLY UNDER DEVELOPMENT." _
            & Chr(10) & Chr(10) _
            & "Refer to the following pdf files in the document subdirectory of your GFM" _
            & " installation directory:" _
            & Chr(10) & Chr(10) _
            & "GFM-WorkFlow - about general use of the GFM GUI case environment." _
            & Chr(10) & Chr(10) _
            & "CycleDescription - about automatic cycling of simulations between domains." _
            & Chr(10) & Chr(10) _
        & "RunPlot - about using a partly developed tool for runtime plotting of " _
        & "status variables." _
        & Chr(10) & Chr(10) _
        & Chr(10) & Chr(10) _
        & "Do you want to enter the version 2 help system?  Much of that information " _
        & "is obsolete," & Chr(10) _
        & "but some of the old information may be helpful.", _
        vbYesNo, "GFM") = vbNo Then Exit Sub
    
'  CD1.HelpFile = App.HelpFile
  CD1.HelpFile = App.Path & "\lib\gfm2.hlp"
  CD1.HelpCommand = &HB
  CD1.ShowHelp
End Sub

'-------------------------------
'-------------------------------
Private Sub Lb3d_Click()
  If Lb3d.Caption = "3D" Then
    Lb3d.Caption = "2D"
    x0 = xg(nxb(1)): y0 = yg(nxb(2)): z0 = zg(nxb(3))
    proj2d
    LbO.Left = p1 - gap: LbO.Top = q1 + gap
    Lbadd.Visible = False
    Lbdel.Visible = False
    Lbmv.Visible = False
  ElseIf Lb3d.Caption = "2D" Then
    Lb3d.Caption = "3D"
    LbO.Left = p0: LbO.Top = q0
    Lbadd.Visible = True
    Lbdel.Visible = True
    Lbmv.Visible = True
  End If
  plt_gd (0)
End Sub

Private Sub Lbadd_Click()
'-------------------------------
'This label is part of the grid control block
'During grid construction, "add" grid line
'During simulation, show "grid" or "plot"
'During post processing, overlay "grid" lines over plot
'-------------------------------
  Dim xdist As Single, ydist As Single, zdist As Single
  If InStr(frmMain.Caption, "Running") > 0 Then GoTo lbadds6
  If InStr(frmMain.Caption, "Post") > 0 Then GoTo lbadds4
  
  If flag_grid_enhanced = False Then
    If set_enhanced_grid_edit = False Then
        'The first enhancement checks failed to set the flag
        Exit Sub
    End If
  End If
  
  If Optx Then
      xdist = xg(ix0) 'ix0 is cell center
      add_X_plane mp, np, lp, xdist, xg, ibc
      'adjust display
      m_sw = 2
      Optx.Caption = "x: " & mp
      nxe(1) = nxe(1) + 2
      tx_xe.Text = nxe(1)
      tx_xc_DblClick
  
  'If Optx Then
  '  GoSub Lbadds1a
  '  mp = mp + 2: Optx.Caption = "x: " & mp
  '  nxe(1) = nxe(1) + 2: tx_xe.Text = nxe(1)
  '  GoSub Lbadds1b
  '  tx_xc_DblClick
  
  ElseIf Opty Then
      ydist = yg(iy0) 'iy0 is cell center
      add_y_plane mp, np, lp, ydist, yg, ibc
      'adjust display
      m_sw = 2
      Opty.Caption = "y: " & np
      nxe(2) = nxe(2) + 2: tx_ye.Text = nxe(2)
      tx_yc_dblClick
 
    'GoSub Lbadds2a
    'np = np + 2: Opty.Caption = "y: " & np
    'nxe(2) = nxe(2) + 2: tx_ye.Text = nxe(2)
    'GoSub Lbadds2b
    'tx_yc_dblClick
    
  ElseIf Optz Then
      zdist = zg(iz0) 'iz0 is cell center
      add_z_plane mp, np, lp, zdist, zg, ibc
      'adjust display
      m_sw = 2
      Optz.Caption = "z: " & lp
      nxe(3) = nxe(3) + 2: tx_ze.Text = nxe(3)
      tx_zc_dblclick
  
    'GoSub Lbadds3a
    'lp = lp + 2: Optz.Caption = "z: " & lp
    'nxe(3) = nxe(3) + 2: tx_ze.Text = nxe(3)
    'GoSub Lbadds3b
    'tx_zc_dblclick
  End If
  Exit Sub
  
lbadds4: 'in post processing, overlay grid lines on plot
  rgb0 = RGB(200, 200, 200)
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  k1 = nxb(3) - 1: k2 = nxe(3) + 1
  If Lb3d = "2D" Then GoTo lbadds5
  dx = (xg(i2) - xg(i1)) * zf1
  p01 = p0 + gap: q01 = q0 - gap
  p02 = p01 + dx + gap * 2
  dy = (yg(j2) - yg(j1)) * zf2
  q02 = q01 - dy - gap
  dz = (zg(k2) - zg(k1)) * zf3
  q1 = q01: q2 = q01 - dy
  q3 = q02: q4 = q02 - dz
  For i = nxb(1) To nxe(1) Step 2
    g0 = (xg(i + 1) - xg(i1)) * zf1
    p1 = p01 + g0
    Line (p1, q1)-(p1, q2), rgb0
    Line (p1, q3)-(p1, q4), rgb0
  Next
  For k = nxb(3) To nxe(3) Step 2
    g0 = (zg(k + 1) - zg(k1)) * zf3
    p1 = p02 + g0
    Line (p1, q1)-(p1, q2), rgb0
  Next
  p1 = p01: p2 = p1 + dx
  p3 = p02: p4 = p3 + dz
  For j = nxb(2) To nxe(2) Step 2
    g0 = (yg(j + 1) - yg(j1)) * zf2
    q1 = q01 - g0
    Line (p1, q1)-(p2, q1), rgb0
    Line (p3, q1)-(p4, q1), rgb0
  Next
  For k = nxb(3) To nxe(3) Step 2
    g0 = (zg(k + 1) - zg(k1)) * zf3
    q1 = q02 - g0
    Line (p1, q1)-(p2, q1), rgb0
  Next
  Exit Sub

lbadds5:
  If Optx Then GoTo lbadds5x
  Exit Sub

lbadds5x:
  For j = j1 To j2 Step 2
    x0 = xg(ix0): y0 = yg(j): z0 = zg(k2): proj2d
    p2 = p1: q2 = q1
    z0 = zg(k1): proj2d
    Line (p1, q1)-(p2, q2), rgb0
  Next
  For k = k1 To k2 Step 2
    x0 = xg(ix0): y0 = yg(j2): z0 = zg(k): proj2d
    p2 = p1: q2 = q1
    y0 = yg(j1): proj2d
    Line (p1, q1)-(p2, q2), rgb0
  Next
  Exit Sub

lbadds6: 'in simulation switch between showing grid or plot
  n = InStr(frmMain.Caption, ":") - 1
  s0 = Left(frmMain.Caption, n)
  If InStr(Lbadd.Caption, "grid") > 0 Then
    frmMain.Caption = s0 & ": Grid"
    Lbadd.Caption = "plot"
    Call plt_gd(0)
  Else
    frmMain.Caption = s0 & ": Post"
    Lbadd.Caption = "grid"
    Call plt_clr(0)
  End If
End Sub




Private Sub add_X_plane(xnum As Integer, ynum As Integer, znum As Integer, _
               x_dist1 As Single, xgp() As Single, ibcp() As Integer)
    ' 8-9-2004
    'Add a line in the x direction to the grid defined by the selection
    'of the arguments.
    'xnum = x direction dimension of the node type array (ibcp)
    'ynum = y direction dimension of the node type array (ibcp)
    'znum = z direction dimension of the node type array (ibcp)
    'x_dist1 = x distance at which to add the line
    'xgp = x coordinate array (dimension of array is xnum+1)
    'ibcp = node type array for grid
    Dim i As Integer, j As Integer, k As Integer
    Dim add_index As Integer 'x index for grid line just after x_dist1
    
    'make a temporary copy of the arrays
    ReDim xgtmp(xnum + 1) As Single
    ReDim ibctmp(xnum, ynum, znum) As Integer
    For i = 1 To xnum + 1: xgtmp(i) = xgp(i): Next
    For i = 2 To xnum Step 2
    For j = 2 To ynum Step 2
    For k = 2 To znum Step 2
        ibctmp(i, j, k) = ibcp(i, j, k)
    Next: Next: Next
    
    'Find index just after x_dist1
    i = 3
    Do While (x_dist1 > xgp(i) And i < xnum - 1)
       i = i + 2
    Loop
    add_index = i
    
    'Resize the arrays allowing space for the added line
    xnum = xnum + 2 'increment the x dimension for a new line
    ReDim xgp(xnum + 1), ibcp(xnum, ynum, znum)
    'Restore lower part of x distance array
    For i = 1 To add_index - 2: xgp(i) = xgtmp(i): Next
    'Adjust positions for new line
    xgp(add_index - 1) = xgtmp(add_index - 2) + ((x_dist1 - xgtmp(add_index - 2)) / 2!)
    xgp(add_index) = x_dist1
    xgp(add_index + 1) = x_dist1 + ((xgtmp(add_index) - x_dist1) / 2!)
    'Fill in rest of x distance array
    For i = add_index + 2 To xnum + 1
         xgp(i) = xgtmp(i - 2)
    Next
    
    'Restore grid node type array
    For i = 2 To xnum Step 2
    For j = 2 To ynum Step 2
    For k = 2 To znum Step 2
      If i <= add_index - 1 Then
        ibcp(i, j, k) = ibctmp(i, j, k)
        'new plane is given same node values as old (divided) plane
      Else
        ibcp(i, j, k) = ibctmp(i - 2, j, k)
      End If
    Next: Next: Next
End Sub


Private Sub add_y_plane(xnum As Integer, ynum As Integer, znum As Integer, _
               y_dist1 As Single, ygp() As Single, ibcp() As Integer)
    ' 8-9-2004
    'Add a line in the y direction to the grid defined by the selection
    'of the arguments.
    'xnum = x direction dimension of the node type array (ibcp)
    'ynum = y direction dimension of the node type array (ibcp)
    'znum = z direction dimension of the node type array (ibcp)
    'y_dist1 = x distance at which to add the line
    'ygp = y coordinate array (dimension of array is ynum+1)
    'ibcp = node type array for grid
    Dim i As Integer, j As Integer, k As Integer
    Dim add_index As Integer 'y index for grid line just after y_dist1
    
    'make a temporary copy of the arrays
    ReDim ygtmp(ynum + 1) As Single
    ReDim ibctmp(xnum, ynum, znum) As Integer
    For i = 1 To ynum + 1: ygtmp(i) = ygp(i): Next
    For i = 2 To xnum Step 2
    For j = 2 To ynum Step 2
    For k = 2 To znum Step 2
        ibctmp(i, j, k) = ibcp(i, j, k)
    Next: Next: Next
    
    'Find index just after y_dist1
    i = 3
    Do While (y_dist1 > ygp(i) And i < ynum - 1)
       i = i + 2
    Loop
    add_index = i
    
    'Resize the arrays allowing space for the added line
    ynum = ynum + 2 'increment the y dimension for a new line
    ReDim ygp(ynum + 1), ibcp(xnum, ynum, znum)
    'Restore lower part of y distance array
    For i = 1 To add_index - 2: ygp(i) = ygtmp(i): Next
    'Adjust positions for new line
    ygp(add_index - 1) = ygtmp(add_index - 2) + ((y_dist1 - ygtmp(add_index - 2)) / 2!)
    ygp(add_index) = y_dist1
    ygp(add_index + 1) = y_dist1 + ((ygtmp(add_index) - y_dist1) / 2!)
    'Fill in rest of y distance array
    For i = add_index + 2 To ynum + 1
         ygp(i) = ygtmp(i - 2)
    Next
    
    'Restore grid node type array
    For i = 2 To xnum Step 2
    For j = 2 To ynum Step 2
    For k = 2 To znum Step 2
      If j <= add_index - 1 Then
        ibcp(i, j, k) = ibctmp(i, j, k)
        'new plane is given same node values as old (divided) plane
      Else
        ibcp(i, j, k) = ibctmp(i, j - 2, k)
      End If
    Next: Next: Next
End Sub


Private Sub add_z_plane(xnum As Integer, ynum As Integer, znum As Integer, _
               z_dist1 As Single, zgp() As Single, ibcp() As Integer)
    ' 8-9-2004
    'Add a line in the z direction to the grid defined by the selection
    'of the arguments.
    'xnum = x direction dimension of the node type array (ibcp)
    'ynum = y direction dimension of the node type array (ibcp)
    'znum = z direction dimension of the node type array (ibcp)
    'z_dist1 = z distance at which to add the line
    'zgp = z coordinate array (dimension of array is znum+1)
    'ibcp = node type array for grid
    Dim i As Integer, j As Integer, k As Integer
    Dim add_index As Integer 'z index for grid line just after z_dist1
    
    'make a temporary copy of the arrays
    ReDim zgtmp(znum + 1) As Single
    ReDim ibctmp(xnum, ynum, znum) As Integer
    For i = 1 To znum + 1: zgtmp(i) = zgp(i): Next
    For i = 2 To xnum Step 2
    For j = 2 To ynum Step 2
    For k = 2 To znum Step 2
        ibctmp(i, j, k) = ibcp(i, j, k)
    Next: Next: Next
    
    'Find index just after z_dist1
    i = 3
    Do While (z_dist1 > zgp(i) And i < znum - 1)
       i = i + 2
    Loop
    add_index = i
    
    'Resize the arrays allowing space for the added line
    znum = znum + 2 'increment the z dimension for a new line
    ReDim zgp(znum + 1), ibcp(xnum, ynum, znum)
    'Restore lower part of z distance array
    For i = 1 To add_index - 2: zgp(i) = zgtmp(i): Next
    'Adjust positions for new line
    zgp(add_index - 1) = zgtmp(add_index - 2) + ((z_dist1 - zgtmp(add_index - 2)) / 2!)
    zgp(add_index) = z_dist1
    zgp(add_index + 1) = z_dist1 + ((zgtmp(add_index) - z_dist1) / 2!)
    'Fill in rest of zdistance array
    For i = add_index + 2 To znum + 1
         zgp(i) = zgtmp(i - 2)
    Next
    
    'Restore grid node type array
    For i = 2 To xnum Step 2
    For j = 2 To ynum Step 2
    For k = 2 To znum Step 2
      If k <= add_index - 1 Then
        ibcp(i, j, k) = ibctmp(i, j, k)
        'new plane is given same node values as old (divided) plane
      Else
        ibcp(i, j, k) = ibctmp(i, j, k - 2)
      End If
    Next: Next: Next
End Sub


'-------------------------------
'-------------------------------
Private Sub Lbdel_Click()
  Dim xdist As Single, ydist As Single, zdist As Single
  If InStr(frmMain.Caption, "Running") > 0 Then GoTo lbdels5
  If InStr(frmMain.Caption, "Post") > 0 Then GoTo lbdels4
 
  If flag_grid_enhanced = False Then
    If set_enhanced_grid_edit = False Then
        'The first enhancement checks failed to set the flag
        Exit Sub
    End If
  End If
  
  If Optx Then
      'ix0 is the current node index,
      'Therefore want to delete grid line at previous index
      xdist = xg(ix0 - 1)
      delete_x_planes xdist, xdist, xg, ibc, mp, np, lp
      'adjust display
      nxe(1) = nxe(1) - 2: tx_xe.Text = nxe(1)
      ix0 = ix0 - 4
      m_sw = 2
      Optx.Caption = "x: " & mp
      tx_xc_DblClick
     
  'If Optx Then
   ' Call Lbdels1a
    'mp = mp - 2: Optx.Caption = "x: " & mp
   ' nxe(1) = nxe(1) - 2: tx_xe.Text = nxe(1)
   ' Call Lbdels1b
   ' m_sw = 2: ix0 = ix0 - 4: tx_xc_DblClick
   
  ElseIf Opty Then
      'iy0 is the current node index,
      'Therefore want to delete grid line at previous index
      ydist = yg(iy0 - 1)
      delete_y_planes ydist, ydist, yg, ibc, mp, np, lp
      'adjust display
      Opty.Caption = "y: " & np
      nxe(2) = nxe(2) - 2: tx_ye.Text = nxe(2)
      m_sw = 2: iy0 = iy0 - 4: tx_yc_dblClick
      
    'GoSub Lbdels2a
    'np = np - 2: Opty.Caption = "y: " & np
    'nxe(2) = nxe(2) - 2: tx_ye.Text = nxe(2)
    'GoSub Lbdels2b
    'm_sw = 2: iy0 = iy0 - 4: tx_yc_dblClick
    
  ElseIf Optz Then
      'iz0 is the current node index,
      'Therefore want to delete grid line at previous index
      zdist = zg(iz0 - 1)
      delete_z_planes zdist, zdist, zg, ibc, mp, np, lp
      'adjust display
      Optz.Caption = "z: " & lp
      nxe(3) = nxe(3) - 2: tx_ze.Text = nxe(3)
      m_sw = 2: iz0 = iz0 - 4: tx_zc_dblclick
      
     'GoSub Lbdels3a
     'lp = lp - 2: Optz.Caption = "z: " & lp
     'nxe(3) = nxe(3) - 2: tx_ze.Text = nxe(3)
     'GoSub Lbdels3b
     'm_sw = 2: iz0 = iz0 - 4: tx_zc_dblclick
  End If
  Exit Sub

'---------------------------------------------------------
' These Lines (up to "End These Lines" marker)
' are no longer used and can be deleted
'---------------------------------------------------------
Lbdels2a:
    ReDim yg1(np + 1)
    For i = 1 To np + 1: yg1(i) = yg(i): Next
    'Call store_ibc_in_ibc1
  Return

Lbdels2b:
    ReDim yg(np + 1), ibc(mp, np, lp)
    For i = 1 To iy0 - 3: yg(i) = yg1(i): Next
    n1 = iy0 - 1: yg(n1) = yg1(iy0 + 1)
    yg(iy0 - 2) = (yg(iy0 - 3) + yg(n1)) / 2
    For i = iy0 To np + 1: yg(i) = yg1(i + 2): Next
    For i = 2 To mp Step 2: For j = 2 To np Step 2
    For k = 2 To lp Step 2
      If j <= iy0 Then
        ibc(i, j, k) = ibc1(i, j, k)
      Else
        ibc(i, j, k) = ibc1(i, j + 2, k)
      End If
    Next: Next: Next
  Return

Lbdels3a:
    ReDim zg1(lp + 1)
    For i = 1 To lp + 1: zg1(i) = zg(i): Next
    'Call store_ibc_in_ibc1
  Return

Lbdels3b:
    ReDim zg(lp + 1), ibc(mp, np, lp)
    For i = 1 To iz0 - 3: zg(i) = zg1(i): Next
    n1 = iz0 - 1: zg(n1) = zg1(iz0 + 1)
    zg(iz0 - 2) = (zg(iz0 - 3) + zg(n1)) / 2
    For i = iz0 To lp + 1: zg(i) = zg1(i + 2): Next
    For i = 2 To mp Step 2: For j = 2 To np Step 2
    For k = 2 To lp Step 2
      If k <= iz0 Then
        ibc(i, j, k) = ibc1(i, j, k)
      Else
        ibc(i, j, k) = ibc1(i, j, k + 2)
      End If
    Next: Next: Next
  Return
'---------------------------------------------------------
' End These Lines are no longer used and can be deleted
'---------------------------------------------------------

lbdels4:
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  k1 = nxb(3) - 1: k2 = nxe(3) + 1
  k = iz0: kd2 = k / 2
  For i = nxb(1) To nxe(1) Step 4
  id2 = i / 2
  For j = nxb(2) To nxe(2) Step 2
    GoSub lbdels4z
  Next: Next
  j = iy0: jd2 = j / 2
  For i = nxb(1) To nxe(1) Step 4
  id2 = i / 2
  For k = nxb(3) To nxe(3) Step 2
    GoSub lbdels4y
  Next: Next
  i = ix0: id2 = i / 2
  For j = nxb(2) To nxe(2) Step 2
  jd2 = j / 2
  For k = nxb(3) To nxe(3) Step 2
    GoSub lbdels4x
  Next: Next
  Exit Sub

lbdels4z:
  If ibc(i, j, k) = 1 Then Return
  jd2 = j / 2
  If Lb3d = "2D" Then GoTo lbdels4z1
  p1 = p01 + (xg(i) - xg(i1)) * zf1
  q1 = q01 - (yg(j) - yg(j1)) * zf2
  p2 = p1 + ug(id2, jd2, kd2, 1) * vv0
  q2 = q1 - ug(id2, jd2, kd2, 2) * vv0
  GoTo lb_arrow

lbdels4z1:
  If Not Optz Then Return
  x0 = xg(i): y0 = yg(j): z0 = zg(k): proj2d
  p2 = p1: q2 = q1
  x0 = x0 + ug(id2, jd2, kd2, 1) * vv0 / zf1
  y0 = y0 + ug(id2, jd2, kd2, 2) * vv0 / zf2
  Call proj2d: GoTo lb_arrow

lbdels4y:
  If ibc(i, j, k) = 1 Then Return
  kd2 = k / 2
  If Lb3d = "2D" Then GoTo lbdels4y1
  p1 = p01 + (xg(i) - xg(i1)) * zf1
  q1 = q02 - (zg(k) - zg(k1)) * zf3
  p2 = p1 + ug(id2, jd2, kd2, 1) * vv0
  q2 = q1 - ug(id2, jd2, kd2, 3) * vv0
  GoTo lb_arrow

lbdels4y1:
  If Not Opty Then Return
  x0 = xg(i): y0 = yg(j): z0 = zg(k): proj2d
  p2 = p1: q2 = q1
  x0 = x0 + ug(id2, jd2, kd2, 1) * vv0 / zf1
  z0 = z0 + ug(id2, jd2, kd2, 3) * vv0 / zf3
  Call proj2d: GoTo lb_arrow

lbdels4x:
  If ibc(i, j, k) = 1 Then Return
  kd2 = k / 2
  If Lb3d = "2D" Then GoTo lbdels4x1
  p1 = p02 + (zg(k) - zg(k1)) * zf3
  q1 = q01 - (yg(j) - yg(j1)) * zf2
  p2 = p1 + ug(id2, jd2, kd2, 3) * vv0
  q2 = q1 - ug(id2, jd2, kd2, 2) * vv0
  GoTo lb_arrow

lbdels4x1:
  If Not Optx Then Return
  x0 = xg(i): y0 = yg(j): z0 = zg(k): proj2d
  p2 = p1: q2 = q1
  y0 = y0 + ug(id2, jd2, kd2, 2) * vv0 / zf2
  z0 = z0 + ug(id2, jd2, kd2, 3) * vv0 / zf3
  Call proj2d: GoTo lb_arrow

lb_arrow:
  Line (p1, q1)-(p2, q2)
  a_d = Sqr((p2 - p1) ^ 2 + (q2 - q1) ^ 2)
  If a_d < 1 Then Return
  a_h = 100: a_w = 0.2 * a_h: f = a_h / a_d
  p5 = p2 - f * (p2 - p1): q5 = q2 - f * (q2 - q1)
  p3 = p5 - a_w / 2 * (q2 - q1) / a_d
  q3 = q5 + a_w / 2 * (p2 - p1) / a_d
  p4 = p5 + a_w / 2 * (q2 - q1) / a_d
  q4 = q5 - a_w / 2 * (p2 - p1) / a_d
  For T = 0 To 1 Step 0.1
    p = p3 + T * (p4 - p3)
    q = q3 + T * (q4 - q3)
    Line (p, q)-(p2, q2)
  Next
  Return

lbdels5:
  If Lbdel.Caption = "pause" Then
    Lbdel.Caption = "auto"
  Else
    Lbdel.Caption = "pause"
  End If
End Sub

 
 
'-------------------------------
'    8-9-2004
'-------------------------------
Private Sub delete_x_planes(x_dist1 As Single, x_dist2 As Single, _
         xgp() As Single, ibcp() As Integer, _
         xnum As Integer, ynum As Integer, znum As Integer)
  'Delete a range of x direction planes from the grid selected by the arguments.
  'The nearest grid line to the input x distance will be deleted on each end, as
  'well as all grid lines between them.  Only one grid line is deleted when
  'x_dist1 = x_dist2.
  'Note: The calling routine is responsible for checking that the resulting node size
  '      is not too large.
  'x_dist1 = lowest x distance of range.
  'x_dist2 = highest x distance of range.
  'xgp = x coordinate array (dimension of the array is xnum+1)
  'ibcp = node type array for grid
  'xnum = x direction dimension of the node type array
  'ynum = y direction dimension of the node type array
  'znum = z direction dimension of the node type array
    
  Dim i As Integer, j As Integer, k As Integer
  Dim index_change As Integer
  'Note: Changing the dimensions of an array initializes the elements to zero,
  '      therefore temporary storage is needed.
  ReDim xgtmp(xnum + 1) As Single
  ReDim ibctmp(xnum, ynum, znum) As Integer
  

  'Find index closest to x_dist1
  'Are loop limits OK ???
  i = 3
  Do While (x_dist1 > xgp(i) And i < xnum - 1)
    i = i + 2
  Loop
  If (x_dist1 < xgp(i - 1)) Then
    del_index1 = i - 2
  Else
    del_index1 = i
  End If
  
  'Find index closest to x_dist2
  If (x_dist1 = x_dist2) Then
    del_index2 = del_index1 'only have one grid line to delete
  Else
    i = del_index1
    Do While (x_dist2 > xgp(i) And i < xnum - 1)
       i = i + 2
    Loop
    If (x_dist2 < xgp(i - 1)) Then
       del_index2 = i - 2
    Else
       del_index2 = i
    End If
  End If
   
  'Verify lines are not needed by an object
  '     If an object is being deleted, then this code must assume that the
  '     real ibc values for the object have been reset already
  '
  ' ???

  ' 10-10-05
  'May not delete a boundary line
  If (del_index1 = 1) Then
        If MsgBox("Choose a grid line that is not the starting boundary.", _
            vbOKOnly + vbExclamation, "GFM") = vbOK Then Exit Sub
  End If
  
  'Prepare for array changes, make temporary copy of arrays
  For i = 1 To xnum + 1: xgtmp(i) = xgp(i): Next
  For i = 2 To xnum Step 2
  For j = 2 To ynum Step 2
  For k = 2 To znum Step 2
      ibctmp(i, j, k) = ibcp(i, j, k)
  Next: Next: Next
  
  index_change = del_index2 - del_index1 + 2
  xnum = xnum - index_change  'Reset grid x dimension
  ReDim xgp(xnum + 1)
  ReDim ibcp(xnum, ynum, znum)

  'Restore lower part of x distance array
  For i = 1 To del_index1 - 2
     xgp(i) = xgtmp(i)
  Next
  'Adjust center position of resulting node
  xgp(del_index1 - 1) = xgtmp(del_index1 - 2) + _
     (xgtmp(del_index2 + 2) - xgtmp(del_index1 - 2)) / 2!
  'Eliminate the hole created in the xgp array by moving higher
  'elements down.
  For i = del_index1 To xnum + 1
     xgp(i) = xgtmp(i + index_change)
  Next

  'Restore grid node type array
  For i = 2 To xnum Step 2
  For j = 2 To ynum Step 2
  For k = 2 To znum Step 2
      If i < del_index1 Then
         ibcp(i, j, k) = ibctmp(i, j, k)
      Else
         ibcp(i, j, k) = ibctmp(i + index_change, j, k)
      End If
  Next: Next: Next
End Sub
 
 
'-------------------------------
'    8-9-2004
'-------------------------------
Private Sub delete_y_planes(y_dist1 As Single, y_dist2 As Single, _
         ygp() As Single, ibcp() As Integer, _
         xnum As Integer, ynum As Integer, znum As Integer)
  'Delete a range of x direction planes from the grid selected by the arguments.
  'The nearest grid line to the input y distance will be deleted on each end, as
  'well as all grid lines between them.  Only one grid line is deleted when
  'y_dist1 = y_dist2.
  'Note: The calling routine is responsible for checking that the resulting node size
  '      is not too large.
  'y_dist1 = lowest y distance of range.
  'y_dist2 = highest y distance of range.
  'ygp = y coordinate array (dimension of the array is ynum+1)
  'ibcp = node type array for grid
  'xnum = x direction dimension of the node type array
  'ynum = y direction dimension of the node type array
  'znum = z direction dimension of the node type array
    
  Dim i As Integer, j As Integer, k As Integer
  Dim index_change As Integer
  'Note: Changing the dimensions of an array initializes the elements to zero,
  '      therefore temporary storage is needed.
  ReDim ygtmp(ynum + 1) As Single
  ReDim ibctmp(xnum, ynum, znum) As Integer
  

  'Find index closest to y_dist1
  'Are loop limits OK ???
  i = 3
  Do While (y_dist1 > ygp(i) And i < ynum - 1)
    i = i + 2
  Loop
  If (y_dist1 < ygp(i - 1)) Then
    del_index1 = i - 2
  Else
    del_index1 = i
  End If
  
  'Find index closest to y_dist2
  If (y_dist1 = y_dist2) Then
    del_index2 = del_index1 'only have one grid line to delete
  Else
    i = del_index1
    Do While (y_dist2 > ygp(i) And i < ynum - 1)
       i = i + 2
    Loop
    If (y_dist2 < ygp(i - 1)) Then
       del_index2 = i - 2
    Else
       del_index2 = i
    End If
  End If
   
  'Verify lines are not needed by an object
  '     If an object is being deleted, then this code must assume that the
  '     real ibc values for the object have been reset already
  '
  ' ???
  
  ' 10-10-05
  'May not delete a boundary line
  If (del_index1 = 1) Then
        If MsgBox("Choose a grid line that is not the starting boundary.", _
            vbOKOnly + vbExclamation, "GFM") = vbOK Then Exit Sub
  End If
  
  'Prepare for array changes, make temporary copy of arrays
  For i = 1 To ynum + 1: ygtmp(i) = ygp(i): Next
  For i = 2 To xnum Step 2
  For j = 2 To ynum Step 2
  For k = 2 To znum Step 2
      ibctmp(i, j, k) = ibcp(i, j, k)
  Next: Next: Next
  
  index_change = del_index2 - del_index1 + 2
  ynum = ynum - index_change  'Reset grid y dimension
  ReDim ygp(xnum + 1)
  ReDim ibcp(xnum, ynum, znum)

  'Restore lower part of x distance array
  For i = 1 To del_index1 - 2
     ygp(i) = ygtmp(i)
  Next
  'Adjust center position of resulting node
  ygp(del_index1 - 1) = ygtmp(del_index1 - 2) + _
     (ygtmp(del_index2 + 2) - ygtmp(del_index1 - 2)) / 2!
  'Eliminate the hole created in the ygp array by moving higher
  'elements down.
  For i = del_index1 To ynum + 1
     ygp(i) = ygtmp(i + index_change)
  Next

  'Restore grid node type array
  For i = 2 To xnum Step 2
  For j = 2 To ynum Step 2
  For k = 2 To znum Step 2
      If j < del_index1 Then
         ibcp(i, j, k) = ibctmp(i, j, k)
      Else
         ibcp(i, j, k) = ibctmp(i, j + index_change, k)
      End If
  Next: Next: Next
End Sub
 
 
'-------------------------------
'    8-9-2004
'-------------------------------
Private Sub delete_z_planes(z_dist1 As Single, z_dist2 As Single, _
         zgp() As Single, ibcp() As Integer, _
         xnum As Integer, ynum As Integer, znum As Integer)
  'Delete a range of x direction planes from the grid selected by the arguments.
  'The nearest grid line to the input z distance will be deleted on each end, as
  'well as all grid lines between them.  Only one grid line is deleted when
  'z_dist1 = z_dist2.
  'Note: The calling routine is responsible for checking that the resulting node size
  '      is not too large.
  'z_dist1 = lowest z distance of range.
  'z_dist2 = highest z distance of range.
  'zgp = z coordinate array (dimension of the array is znum+1)
  'ibcp = node type array for grid
  'xnum = x direction dimension of the node type array
  'ynum = y direction dimension of the node type array
  'znum = z direction dimension of the node type array
    
  Dim i As Integer, j As Integer, k As Integer
  Dim index_change As Integer
  'Note: Changing the dimensions of an array initializes the elements to zero,
  '      therefore temporary storage is needed.
  ReDim zgtmp(znum + 1) As Single
  ReDim ibctmp(xnum, ynum, znum) As Integer
  

  'Find index closest to z_dist1
  'Are loop limits OK ???
  i = 3
  Do While (z_dist1 > zgp(i) And i < znum - 1)
    i = i + 2
  Loop
  If (z_dist1 < zgp(i - 1)) Then
    del_index1 = i - 2
  Else
    del_index1 = i
  End If
  
  'Find index closest to z_dist2
  If (z_dist1 = z_dist2) Then
    del_index2 = del_index1 'only have one grid line to delete
  Else
    i = del_index1
    Do While (z_dist2 > zgp(i) And i < znum - 1)
       i = i + 2
    Loop
    If (z_dist2 < zgp(i - 1)) Then
       del_index2 = i - 2
    Else
       del_index2 = i
    End If
  End If
   
  'Verify lines are not needed by an object
  '     If an object is being deleted, then this code must assume that the
  '     real ibc values for the object have been reset already
  '
  ' ???
  
  ' 10-10-05
  'May not delete a boundary line
  If (del_index1 = 1) Then
        If MsgBox("Choose a grid line that is not the starting boundary.", _
            vbOKOnly + vbExclamation, "GFM") = vbOK Then Exit Sub
  End If
  
  'Prepare for array changes, make temporary copy of arrays
  For i = 1 To znum + 1: zgtmp(i) = zgp(i): Next
  For i = 2 To xnum Step 2
  For j = 2 To ynum Step 2
  For k = 2 To znum Step 2
      ibctmp(i, j, k) = ibcp(i, j, k)
  Next: Next: Next
  
  index_change = del_index2 - del_index1 + 2
  znum = znum - index_change  'Reset grid z dimension
  ReDim zgp(znum + 1)
  ReDim ibcp(xnum, ynum, znum)

  'Restore lower part of x distance array
  For i = 1 To del_index1 - 2
     zgp(i) = zgtmp(i)
  Next
  'Adjust center position of resulting node
  zgp(del_index1 - 1) = zgtmp(del_index1 - 2) + _
     (zgtmp(del_index2 + 2) - zgtmp(del_index1 - 2)) / 2!
  'Eliminate the hole created in the zgp array by moving higher
  'elements down.
  For i = del_index1 To znum + 1
     zgp(i) = zgtmp(i + index_change)
  Next

  'Restore grid node type array
  For i = 2 To xnum Step 2
  For j = 2 To ynum Step 2
  For k = 2 To znum Step 2
      If k < del_index1 Then
         ibcp(i, j, k) = ibctmp(i, j, k)
      Else
         ibcp(i, j, k) = ibctmp(i, j, k + index_change)
      End If
  Next: Next: Next
End Sub
 
 
 
'-------------------------------
'-------------------------------
Private Sub Lbfr_Click()
  If InStr(Lbnd.Caption, "select") Then
    tx_xb = ix0: nxbs(1) = ix0
    tx_yb = iy0: nxbs(2) = iy0
    tx_zb = iz0: nxbs(3) = iz0
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub Lbmv_Click()
  If InStr(frmMain.Caption, "Post") > 0 Then GoTo lbmvs4
  'Do not know if in sim or postproc!   Need to determine. ???
  
  If flag_grid_enhanced = False Then
    If set_enhanced_grid_edit = False Then
        'The first enhancement checks failed to set the flag
        Exit Sub
    End If
  End If
  
  If Optx Then
    GoSub lbmvs1: ix0 = ix0 - m_sw: tx_xc_DblClick
  ElseIf Opty Then
    GoSub lbmvs2: iy0 = iy0 - m_sw: tx_yc_dblClick
  ElseIf Optz Then
    GoSub lbmvs3: iz0 = iz0 - m_sw: tx_zc_dblclick
  End If
  Exit Sub

lbmvs1:
  n1 = ix0 - 1
  If n1 <= 3 Or n1 >= mp - 1 Then Return
  GoSub lbmvs1a
  s0 = "m": g0 = xg(n1 - 2): Call ucv
  s1 = "X-grid: move between " & g0
  s0 = "m": g0 = xg(n1 + 2): Call ucv
  s0 = s1 & " and " & g0
  g0 = xg(n1): Call val_in(-1)
  If g0 >= xg(n1 + 2) Or g0 <= xg(n1 - 2) Then _
    Call Lbdel_Click: Exit Sub
  xg(n1) = g0
  xg(n1 + 1) = (xg(n1 + 2) + g0) / 2
  xg(n1 - 1) = (xg(n1 - 2) + g0) / 2
  Return

lbmvs1a:
  q1 = q0 - gap
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  q2 = q1 - (yg(j2) - yg(j1)) * zf2
  i1 = nxb(1) - 1
  p1 = (xg(n1 - 2) - xg(i1)) * zf1 + p0 + gap
  Line (p1, q1)-(p1, q2), vbYellow
  p1 = (xg(n1 + 2) - xg(i1)) * zf1 + p0 + gap
  Line (p1, q1)-(p1, q2), vbYellow
  Return

lbmvs2:
  n1 = iy0 - 1
  If n1 <= 3 Or n1 >= np - 1 Then Return
  GoSub lbmvs2a
  s0 = "m": g0 = yg(n1 - 2): Call ucv
  s1 = "Y-grid: move between " & g0
  s0 = "m": g0 = yg(n1 + 2): Call ucv
  s0 = s1 & " and " & g0
  g0 = yg(n1): Call val_in(-1)
  If g0 >= yg(n1 + 2) Or g0 <= yg(n1 - 2) Then _
    Call Lbdel_Click: Exit Sub
  yg(n1) = g0
  yg(n1 + 1) = (yg(n1 + 2) + g0) / 2
  yg(n1 - 1) = (yg(n1 - 2) + g0) / 2
  Return

lbmvs2a:
  p1 = p0 + gap
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  p2 = p1 + (xg(i2) - xg(i1)) * zf1
  j1 = nxb(2) - 1
  q1 = q0 - gap - (yg(n1 - 2) - yg(j1)) * zf2
  Line (p1, q1)-(p2, q1), vbYellow
  q1 = q0 - gap - (yg(n1 + 2) - yg(j1)) * zf2
  Line (p1, q1)-(p2, q1), vbYellow
  Return

lbmvs3:
  n1 = iz0 - 1
  If n1 <= 3 Or n1 >= lp - 1 Then Return
  GoSub lbmvs3a
  s0 = "m": g0 = zg(n1 - 2): Call ucv
  s1 = "Z-grid: move between " & g0
  s0 = "m": g0 = zg(n1 + 2): Call ucv
  s0 = s1 & " and " & g0
  g0 = zg(n1): Call val_in(-1)
  If g0 >= zg(n1 + 2) Or g0 <= zg(n1 - 2) Then _
    Call Lbdel_Click: Exit Sub
  zg(n1) = g0
  zg(n1 + 1) = (zg(n1 + 2) + g0) / 2
  zg(n1 - 1) = (zg(n1 - 2) + g0) / 2
  Return

lbmvs3a:
  q1 = q0 - gap
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  q2 = q1 - (yg(j2) - yg(j1)) * zf2
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  p1 = (xg(i2) - xg(i1)) * zf1 + p0 + gap * 3
  k1 = nxb(3) - 1
  p1 = (zg(n1 - 2) - zg(k1)) * zf3 + p1
  Line (p1, q1)-(p1, q2), vbYellow
  p1 = (zg(n1 + 2) - zg(n1 - 2)) * zf3 + p1
  Line (p1, q1)-(p1, q2), vbYellow
  Return

lbmvs4:
  If Lbmv.Caption = "b/w" Then
    Lbmv.Caption = "color"
  Else
    Lbmv.Caption = "b/w"
  End If
  If InStr(frmMain.Caption, "grid") = 0 Then plt_clr (0)
End Sub

'-------------------------------
'-------------------------------
Private Sub Lbto_Click()
  If InStr(Lbnd.Caption, "select") Then
    tx_xe = ix0: nxes(1) = ix0
    tx_ye = iy0: nxes(2) = iy0
    tx_ze = iz0: nxes(3) = iz0
  End If
End Sub

'-------------------------------
'Specify furnace length
'or x direction or stop
'-------------------------------
Private Sub Lb1_Click()
  ttl = LCase(Lb1.Caption)
  If ttl = "length" Then
    If Not gridProtectMode Then GoTo lb1a
  ElseIf ttl = "x" Then
    Optx.Value = True
  End If
  Exit Sub

lb1a: 'length
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  If chamber_type = 2 Then zf1a = zf1: zf1 = zf1 * hgt_r(1) / hgt_r(melter_component)
  x0 = lth / 2: y0 = 0: z0 = 0: Call proj2d
  ipc = Point(p1, q1)
  If ipc = vbRed Then Exit Sub
  p2 = p0: q2 = q0
  x0 = lth: y0 = 0: z0 = 0: Call proj2d
  Call vector
  If flow_domain = 1 Then
    s0 = "furnace"
  Else
    If chamber_type = 2 And melter_component = 3 Then
      s0 = "refiner"
    ElseIf chamber_type = 2 And melter_component = 2 Then
      s0 = "throat"
    Else
      s0 = "melter"
    End If
  End If
  s0 = "Specify " & s0 & " length": g0 = lth
  If chamber_type = 2 And melter_component = 3 _
    Then Call val_in(0) Else: Call val_in(1)
  lth = g0
  If chamber_type = 2 Then lth_r(melter_component) = lth: zf1 = zf1a
'csl
  If chamber_type = 2 And melter_component = 3 And lth = 0 Then
    wdt_r(3) = wdt_r(2): hgt_r(3) = hgt_r(2)
    gy_r(3) = gy_r(2): gz_r(3) = gz_r(2)
    edr(1) = 1: esg(1) = 0: ebg(1) = 0
    ewd(1) = wdt_r(3): eht(1) = hgt_r(3)
  End If
  ' 8-12-2004 fix
  Call ctgeo_Click 'clear out lists that could otherwise remain on the screen
  Call plt_cb
End Sub

'-------------------------------
'Toggle the node label (Lbnd) to allow changing the cell type in a range of cells
'-------------------------------
Private Sub Lbnd_Click()
  If InStr(Lbnd.Caption, "current cell") Then
    'put current cell into from and to columns of grid control block
    tx_xb = ix0: tx_yb = iy0: tx_zb = iz0
    nxbs(1) = ix0: nxbs(2) = iy0: nxbs(3) = iz0
    tx_xe = ix0: tx_ye = iy0: tx_ze = iz0
    nxes(1) = ix0: nxes(2) = iy0: nxes(3) = iz0
    Lbnd.Caption = "select cells"
  ElseIf InStr(Lbnd.Caption, "select cells") Then
    'change cell type in space defined by the from and to columns of the
    'grid control block to the current cell type
    
    If flag_grid_enhanced = False Then
        If set_enhanced_grid_edit = False Then
            'The first enhancement checks failed to set the flag
            Exit Sub
        End If
    End If
        
    ibc0 = ibc(ix0, iy0, iz0)
    For i = 1 To 3
      If nxbs(i) > nxes(i) Then _
        g0 = nxbs(i): nxbs(i) = nxes(i): nxes(i) = g0
    Next
    For i = nxbs(1) To nxes(1) Step 2
    For j = nxbs(2) To nxes(2) Step 2
    For k = nxbs(3) To nxes(3) Step 2
      ibc(i, j, k) = ibc0
    Next: Next: Next
    'restore normal display in the grid control block
    plt_gd (0)
    tx_xb = nxb(1): tx_yb = nxb(2): tx_zb = nxb(3)
    tx_xe = nxe(1): tx_ye = nxe(2): tx_ze = nxe(3)
    Lbnd.Caption = "current cell"
  End If
End Sub

'-------------------------------
'Change current cell type
'-------------------------------
Private Sub Lb12_Click()
  If InStr(frmMain.Caption, "Post") > 0 Then Exit Sub
    'in post processing the label is used to display the output value at the choosen cell
    
  If flag_grid_enhanced = False Then
    If set_enhanced_grid_edit = False Then
        'The first enhancement checks failed to set the flag
        Exit Sub
    End If
  End If
  
  i1 = ibc(ix0, iy0, iz0) + 1
  If i1 = 6 Then i1 = 8
  If i1 = 9 Then i1 = 0
  ibc(ix0, iy0, iz0) = i1
  plt_gd (0)
End Sub

Private Sub nodetype()
'-------------------------------
'Set color and current cell type (ibc value) in Lb12
'-------------------------------
  Dim i1 As Integer
  s0 = "    "
  i1 = ibc(ix0, iy0, iz0)
  If i1 <= 0 Then
    rgb0 = frmMain.BackColor: s0 = "comp"
  ElseIf i1 = 1 Then
    rgb0 = RGB(150, 150, 150): s0 = "wall"
  ElseIf i1 = 2 Then
    rgb0 = vbGreen: s0 = "inlet"
  ElseIf i1 = 3 Then
    rgb0 = vbRed: s0 = "exit"
  ElseIf i1 = 4 Then
    rgb0 = vbCyan: s0 = "surf"
  ElseIf i1 = 5 Then
    rgb0 = RGB(180, 0, 0): s0 = "eboost"
  ElseIf i1 = 8 Then
    rgb0 = RGB(0, 180, 0): s0 = "bubb"
  End If
  Lb12.BackColor = rgb0: Lb12.Caption = s0
End Sub

Private Sub Lbsw_Click()
'-------------------------------
'When looking at the grid or output, toggle the sweep display.  Use 1/2 second interval.
'When doing a simulation, stop or continue the simulation run.  Use 5 second interval.
'-------------------------------
  Dim short_sbc_fnm As String
  Dim tmp_short_sbc_fnm As String
  
  If menu_layer = InSimulation Then GoTo lbsw1
  'ttl = LCase(Caption)
  'If InStr(ttl, "running") > 0 Then GoTo lbsw1
  sweep = Not sweep
  If sweep Then
    Lbsw.Caption = "stop"
    Timer1.Enabled = True: Timer1.Interval = 500 'milliseconds
  Else
    Lbsw.Caption = "sweep"
    Timer1.Enabled = False
  End If
  Exit Sub

lbsw1: 'A simulation is in progress
  If InStr(Lbsw.Caption, "cont") > 0 Then GoTo lbsw2
  If InStr(Lbsw.Caption, "ending") > 0 Then Exit Sub 'user must wait
  'Lbsw.Caption must be "stop"
  If MsgBox("A simulation run is in progress.  Your convergence criteria has " _
            & "not been met.  Are you sure you want to save results and stop?", _
            vbYesNo + vbExclamation, "GFM") = vbYes Then
        userStoppedSim = True
        Lbsw.BackColor = vbYellow: Lbsw.Caption = "ending"
        'signal CDF code to stop
        If flow_domain = 1 Then
            s0 = dnm & "combustion\runs.dat"
        Else
            s0 = dnm & "melt\runs.dat"
        End If
        FileCopy s0, case_path & "\runstop.dat"
  End If
  Exit Sub

lbsw2: 'A simulation has stopped and the user wants to continue the run
    'verify that user is able to continue
    If restart_str = "0" Then
        If MsgBox("You have made a request to continue the simulation.  However the " _
            & "run parameter still indicates a new start.  Would you like that parameter " _
            & "changed?  (Otherwise the simulation will repeat " _
            & "from the beginning again.)", _
        vbYesNo + vbQuestion, "GFM") = vbYes Then
            'need to update conditions (sbc) file
            Call setRestartIndicator
        End If
    End If

    'continue with simulation
    ex0.Caption = "Stop Run"
    If flow_domain = 1 Then Call simcr_Click Else Call simmr_Click
End Sub

'-------------------------------
'Specify furnace width
'or y direction
'-------------------------------
Private Sub Lb2_Click()
  ttl = LCase(Lb2.Caption)
  If ttl = "width" Then
    If gridProtectMode Then Exit Sub
    flag_preproc_modified = True
    'if flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If chamber_type = 2 Then
      zf1a = zf1: zf1 = zf1 * hgt_r(1) / hgt_r(melter_component)
      zf2a = zf2: zf2 = zf2 * hgt_r(1) / hgt_r(melter_component)
    End If
'    x0 = lth: y0 = wdt: z0 = 0: Call proj2d
'    ipc = Point(p1, q1)
'    Call plt_cb
'    If ipc = vbRed Then Exit Sub
    x0 = lth: y0 = 0: z0 = 0: Call proj2d
    p2 = p1: q2 = q1
    x0 = lth: y0 = wdt: z0 = 0: Call proj2d
'    ipc = Point(p1, q1)
    Call vector
'    Call ctgeo_Click
    If flow_domain = 1 Then
      s0 = "furnace"
    Else
      If chamber_type = 2 And melter_component = 2 Then
        s0 = "throat"
      ElseIf chamber_type = 2 And melter_component = 3 Then
        s0 = "refiner"
      Else
        s0 = "melter"
      End If
    End If
    s0 = "Specify " & s0 & " width"
    g0 = wdt: Call val_in(1): wdt = g0
    If chamber_type = 2 Then wdt_r(melter_component) = wdt: zf1 = zf1a: zf2 = zf2a
    ' 8-12-2004 fix
    Call ctgeo_Click 'clear out lists that could otherwise remain on the screen
    Call plt_cb
  ElseIf Lb2.Caption = "y" Then
    Opty.Value = True
  End If
End Sub

'-------------------------------
'Specify furnace height/depth
'or z direction
'-------------------------------
Private Sub Lb3_Click()
  ttl = LCase(Lb3.Caption)
  If ttl = "height" Or ttl = "depth" Then
    If gridProtectMode Then Exit Sub
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
'    x0 = 0: y0 = 0: z0 = hgt: Call proj2d
'    ipc = Point(p1, q1)
'    Call plt_cb
'    If ipc = vbRed Then Exit Sub
    If chamber_type = 2 Then zf3a = zf3: zf3 = zf3 * hgt_r(1) / hgt_r(melter_component)
    p2 = p0: q2 = q0
    x0 = 0: y0 = 0: z0 = hgt: Call proj2d
    Call vector
    'If InStr(List1.List(0), "Geo") = 0 Then Call ctgeo_Click 'replaced
    If flow_domain = 1 Then
      s0 = "Specify furnace height"
    Else
      If chamber_type = 2 And melter_component = 2 Then
        s0 = "Specify throat depth"
      ElseIf chamber_type = 2 And melter_component = 3 Then
        s0 = "Specify refiner depth"
      Else
        s0 = "Specify melter depth"
      End If
    End If
    g0 = hgt: Call val_in(1): hgt = g0
    If chamber_type = 2 Then
      hgt_r(melter_component) = hgt: zf3 = zf3a
      If melter_component = 1 Then hgt_r(3) = hgt_r(1) - gz_r(3)
      If melter_component = 3 Then gz_r(3) = hgt_r(1) - hgt_r(3)
    End If
    ' 8-12-2004 fix
    Call ctgeo_Click 'clear out lists that could otherwise remain on the screen
    Call plt_cb
  ElseIf ttl = "z" Then
    Optz.Value = True
  End If
End Sub

'-------------------------------
'Specify furnace crown height
'-------------------------------
Private Sub Lb4_Click()
  If Lb4.Caption = "z" Then Optz.Value = True: Exit Sub
  If flow_domain = 2 Then Exit Sub
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  x0 = 0: y0 = wdt / 2: z0 = hgt: Call proj2d
  ipc = Point(p1, q1)
  'Call plt_cb 'redundant
  If ipc = vbRed Then Exit Sub
  x0 = 0: y0 = wdt / 2: z0 = hgt: Call proj2d
  p2 = p1: q2 = q1
  z0 = hgt + hgt_c: Call proj2d
  Call vector
  'Call ctgeo_Click 'redundant
  s0 = "Specify furnace crown height"
  g0 = hgt_c: Call val_in(0): hgt_c = g0
  ' 8-12-2004 fix
  Call ctgeo_Click 'clear out lists that could otherwise remain on the screen
  Call plt_cb
End Sub

Private Sub List1_Click()
'-------------------------------
'Change List 1 values for pre or post processing
'-------------------------------
  Dim gtg() As Single, gav() As Single
  Dim gah() As Single, gax() As Single, gew() As Single
  Dim gbvel() As Double
  
  If menu_layer = InGridConstruction Or menu_layer = InSimulation Then Exit Sub
  ttl = LCase(frmMain.Caption)
  i = List1.ListIndex
  'If InStr(ttl, "running") > 0 Then Exit Sub
  If InStr(ttl, "post") > 0 Then GoTo ls12
  ttl = LCase(List1.List(0))
  If Not gridProtectMode Then
      If InStr(ttl, "geom") > 0 Then GoTo ls1g
      If InStr(ttl, "exh") > 0 Then GoSub ls1e
      If InStr(ttl, "dog") > 0 Then GoSub ls1w
  End If
  If InStr(ttl, "burn") > 0 Then GoSub ls1b
  If InStr(ttl, "para") > 0 Then GoSub ls1s
  If InStr(ttl, "char") > 0 Then GoSub ls1b
  If InStr(ttl, "comp") > 0 Then GoSub ls1c
  If InStr(ttl, "wall") > 0 Then GoSub ls1x
  If InStr(ttl, "glass") > 0 Then GoSub ls1f
  If InStr(ttl, "outlet") > 0 Then GoSub ls1e
  If InStr(ttl, "bub") > 0 Then GoSub ls1u
  If InStr(ttl, "boo") > 0 Then GoSub ls1z
  
ls11:
  Call plt_cb
  
  If need_burner_check = True Then
    'Note round burner gaps are measured from furnace edge to center of burner
    'whereas rectangular burner gaps are measured from furnace edge to first edge of burner
    If bty(ibr) <= 1 Then 'round shaped burner
        g0 = bwd(ibr, 1) / 2#
        g1 = g0
    Else 'rectangular shaped burner
        g0 = bwd(ibr, 1)
        g1 = bht(ibr, 1)
    End If
    If flow_domain = 1 Then
        s0 = "Warning: Burner extends beyond furnace."
    Else
        s0 = "Warning: Charger extends beyond furnace."
    End If
    Select Case bdr(ibr) 'orientation
        Case 0, 1 'back or front wall
            If bsg(ibr, 1) + g0 > wdt Or (g0 > bsg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
        Case 2 To 5 'other walls
            If bsg(ibr, 1) + g0 > lth Or (g0 > bsg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
    End Select
    Select Case bdr(ibr) 'orientation
        Case 0 To 3 'back, front, right, or left wall
            If bbg(ibr, 1) + g1 > hgt Or (g1 > bbg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
        Case 4, 5 'bottom or top wall
            If bbg(ibr, 1) + g1 > wdt Or (g1 > bbg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
    End Select
  End If
  
  If need_tube_check = True Then
    'Verify inner section is contained within outer section  June07
    If bsg(ibr, 2) < bsg(ibr, 1) Or bbg(ibr, 2) < bbg(ibr, 1) Or _
        (bsg(ibr, 2) + bwd(ibr, 2)) > (bsg(ibr, 1) + bwd(ibr, 1)) Or _
        (bbg(ibr, 2) + bht(ibr, 2)) > (bbg(ibr, 1) + bht(ibr, 1)) _
        Then MsgBox ("Warning: Inner tube is not fully contained within outer tube.")
  End If
  Exit Sub


ls12:  'In post processor
  ttl = List1.List(i)
  frmMain.Caption = "Post-Processor: " & ttl
  'If ttl <> "grid" Then
      If ttl <> fpn Then
         fpn = ttl
         s1 = dnm & "tmp\" & ttl & ".dat"
         Open s1 For Input As #1
         Input #1, s0
         Input #1, fp_ut
         For i = 1 To nx: For j = 1 To ny: For k = 1 To nz
            Input #1, fp(i, j, k)
         Next: Next: Next
         Input #1, fp_mx
         Input #1, fp_mn
         Close (1)
      'End If
      'Else ' 10-8-2004
         Call plt_clr(0)
      End If
  'Else
  '    Call plt_gd(0)
  'End If
  Exit Sub

ls1b1: 'adding a new burner or charger
  'copy burner info into g.. arrays, and add or subtract burner
  'redim tg,av,ah for new burner type   10-19-2004
  ReDim gty(nbr), gdr(nbr), gsg(nbr, 2), gbg(nbr, 2)
  ReDim gwd(nbr, 2), ght(nbr, 2), gtg(nbr, 2)
  ReDim gav(nbr, 2), gah(nbr, 2), gax(nbr)
  ReDim gfg(nbr, 2), gfa(nbr, 2), gfo(nbr, 2), gfn(nbr, 2)
  ReDim gbvel(nbr)
  For n = 1 To nbr
    gty(n) = bty(n): gdr(n) = bdr(n): gtg(n, 1) = btg(n, 1): gtg(n, 2) = btg(n, 2)
    gsg(n, 1) = bsg(n, 1): gbg(n, 1) = bbg(n, 1)
    gwd(n, 1) = bwd(n, 1): ght(n, 1) = bht(n, 1)
    gsg(n, 2) = bsg(n, 2): gbg(n, 2) = bbg(n, 2)
    gwd(n, 2) = bwd(n, 2): ght(n, 2) = bht(n, 2)
    gfg(n, 1) = bfg(n, 1): gfg(n, 2) = bfg(n, 2)
    gav(n, 1) = bav(n, 1): gah(n, 1) = bah(n, 1)
    gav(n, 2) = bav(n, 2): gah(n, 2) = bah(n, 2)
    If flow_domain = 1 Then 'combustion
      gax(n) = bas(n)
      gfa(n, 1) = bfa(n, 1): gfa(n, 2) = bfa(n, 2)
      gfo(n, 1) = bfo(n, 1): gfo(n, 2) = bfo(n, 2)
      gfn(n, 1) = bfn(n, 1): gfn(n, 2) = bfn(n, 2)
    Else 'melt
      gbvel(n) = batch_velocity(n)
    End If
  Next
  If i = 3 Then nbr = nbr + 1
  If i = 4 Then nbr = nbr - 1
  ReDim bty(nbr), bdr(nbr), btg(nbr, 2)
  ReDim bsg(nbr, 2), bbg(nbr, 2), bwd(nbr, 2), bht(nbr, 2)
  ReDim bav(nbr, 2), bah(nbr, 2), bas(nbr)
  ReDim bfg(nbr, 2), bfa(nbr, 2), bfo(nbr, 2), bfn(nbr, 2)
  If flow_domain = 2 Then ReDim batch_velocity(nbr)
  Return

ls1b2: 'restore b.. arrays from temporary g.. arrays
  bty(n1) = gty(n2): bdr(n1) = gdr(n2): btg(n1, 1) = gtg(n2, 1): btg(n1, 2) = gtg(n2, 2)
  bsg(n1, 1) = gsg(n2, 1): bbg(n1, 1) = gbg(n2, 1)
  bwd(n1, 1) = gwd(n2, 1): bht(n1, 1) = ght(n2, 1)
  bsg(n1, 2) = gsg(n2, 2): bbg(n1, 2) = gbg(n2, 2)
  bwd(n1, 2) = gwd(n2, 2): bht(n1, 2) = ght(n2, 2)
  bfg(n1, 1) = gfg(n2, 1): bfg(n1, 2) = gfg(n2, 2)
  bav(n1, 1) = gav(n2, 1): bah(n1, 1) = gah(n2, 1)
  bav(n1, 2) = gav(n2, 2): bah(n1, 2) = gah(n2, 2)
  If flow_domain = 2 Then
    batch_velocity(n1) = gbvel(n2)
  Else
    bas(n1) = gax(n2)
    bfa(n1, 1) = gfa(n2, 1): bfa(n1, 2) = gfa(n2, 1)
    bfo(n1, 1) = gfo(n2, 1): bfo(n1, 2) = gfo(n2, 2)
    bfn(n1, 1) = gfn(n2, 1): bfn(n1, 2) = gfn(n2, 2)
  End If
  Return

ls1b:  'doing burner or charger,  i=listindex
  If i = 1 Then 'next burner
    ibr = ibr + 1
    If ibr > nbr Then ibr = 1
  ElseIf i = 2 And Not gridProtectMode Then 'type of burner
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    bty(ibr) = bty(ibr) + 1
    
    'burner_type_fix June07
    '  switched comment in next 2 lines to allow type 4
    'If bty(ibr) > 3 Then bty(ibr) = 0 ' 02-28-06 burner type 4 disabled, full use not designed
    If bty(ibr) > 4 Then
        bty(ibr) = 0 ' 10-19-2004 added new type
    ElseIf bty(ibr) = 1 Or bty(ibr) = 3 Then 'ring or pipe-in-pipe are being disabled
        bty(ibr) = bty(ibr) + 1
    End If
    
    If flow_domain = 2 Then
      bty(ibr) = 2 'melt
    Else
      If bty(ibr) = 4 Then 'have tube-in-tube
        List3.List(4) = "<- outer section ->"
        If bwd(ibr, 2) = 0 And bht(ibr, 2) = 0 And bsg(ibr, 2) = 0 And bbg(ibr, 2) = 0 Then
            'have tube-in-tube, calculate centered inner values if they are not set  June07
            bwd(ibr, 2) = bwd(ibr, 1) / 2#
            bht(ibr, 2) = bht(ibr, 1) / 2#
            bsg(ibr, 2) = bsg(ibr, 1) + (bwd(ibr, 2) / 2#)
            bbg(ibr, 2) = bbg(ibr, 1) + (bht(ibr, 2) / 2#)
        End If
      End If
      'disable type 1 & 3  June07
      'If bty(ibr) = 1 Or bty(ibr) = 3 Then 'ring or pipe-in-pipe
      '  bfg(ibr, 2) = bfg(ibr, 1): bfg(ibr, 1) = 0
      'Else
      '  bfg(ibr, 1) = bfg(ibr, 2): bfg(ibr, 2) = 0
      'End If
      
      ' 10-21-2004  Handle change of bht usage for rings
      'If bty(ibr) = 1 Then
      '  'new type is ring, save previous height and use inner ring default
      '  saved_bht_pre_ring = bht(ibr, 1)
      '  bht(ibr, 1) = saved_ring_inner
      '  If saved_ring_inner >= bwd(ibr, 1) Then bht(ibr, 1) = bwd(ibr, 1) / 2
      'End If
      'If bty(ibr) = 2 Then
      '  'old type was ring, restore default height and save inner ring diameter
      '  saved_ring_inner = bht(ibr, 1)
      '  bht(ibr, 1) = saved_bht_pre_ring
      'End If
    End If
  ElseIf i = 3 And Not gridProtectMode Then 'add burner, make it the same as last one
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    'The following gosub was to nonexistent t101, so this seems the most likely: (comment was not changed)
    GoSub ls1b1 'copy b.. arrays into g.. arrays and change dimensions
    For n = 1 To nbr - 1
      n1 = n: n2 = n: GoSub ls1b2 'restore b.. arrays from temporary g.. arrays
    Next
    n1 = nbr: n2 = ibr: GoSub ls1b2
    bsg(nbr, 1) = bsg(ibr, 1) + bwd(ibr, 1) 'move new burner over
    'next line for June07
    If bty(nbr) = 4 Then bsg(nbr, 2) = bsg(ibr, 2) + bwd(ibr, 1) 'move inner part of new tube-in-tube burner over
    ibr = nbr
  ElseIf i = 4 And nbr > 1 And Not gridProtectMode Then 'delete burner
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    GoSub ls1b1
    If ibr > 1 Then
      For n = 1 To ibr - 1
        n1 = n: n2 = n: GoSub ls1b2
      Next
    End If
    For n = ibr To nbr
      n1 = n: n2 = n + 1: GoSub ls1b2
    Next
    If ibr > nbr Then ibr = nbr
  ElseIf i = 0 Then 'next object
    If gridProtectMode And flow_domain = 1 Then
         Call ctcpn_Click
    Else
         Call ctex_Click
    End If
    GoTo ls11
  End If
  If flow_domain = 1 Then Call burn_ls
  If flow_domain = 2 Then Call char_ls
  Return

ls1c: 'components
  s0 = LCase(List1)
  If Not gridProtectMode Then
      If InStr(s0, "dog") > 0 Then Call ctsw_Click
  End If
  If InStr(s0, "bub") > 0 Then Call ctsw_Click
  If InStr(s0, "boost") > 0 Then Call eb_ls
  'If InStr(s0, "comp") > 0 Then Call ctsm_Click
  If InStr(s0, "comp") > 0 Then
    If gridProtectMode Then
         Call ctbr_Click
    Else
         Call ctgeo_Click
    End If
  End If
  'If InStr(s0, "wall") > 0 Then Call wa_ls
  'If InStr(s0, "glass") > 0 Then GoSub ls1f1: Call udf_ls
  Return

ls1e: 'exhausts and outlets
  If i = 1 Then 'go to next exhaust
    iex = iex + 1
    If iex > nex Then iex = 1
  ElseIf i = 2 Then 'only one type = rectangle
    ety(iex) = 0
  ElseIf i = 3 Then 'add new exhaust
    If gridProtectMode Then Return
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    ReDim gty(nex), gdr(nex), gsg(nex), gbg(nex)
    ReDim gwd(nex), ght(nex)
    If flow_domain = 1 Then
        ReDim gexh_wall_temp_type(nex), gexh_wall_temp_fixed(nex), gexh_wall_temp_frac(nex)
    Else
        ReDim gpull(nex)
    End If
    For n = 1 To nex 'save info
      gty(n) = ety(n): gdr(n) = edr(n)
      gsg(n) = esg(n): gbg(n) = ebg(n)
      gwd(n) = ewd(n): ght(n) = eht(n)
      If flow_domain = 2 Then
        gpull(n) = epull(n) 'melt
      Else
        gexh_wall_temp_type(n) = exh_wall_temp_type(n)
        gexh_wall_temp_fixed(n) = exh_wall_temp_fixed(n)
        gexh_wall_temp_frac(n) = exh_wall_temp_frac(n)
      End If
    Next
    nex = nex + 1 'add space for new info
    ReDim ety(nex), edr(nex), esg(nex), ebg(nex)
    ReDim ewd(nex), eht(nex), epull(nex)
    If flow_domain = 1 Then
        ReDim exh_wall_temp_type(nex), exh_wall_temp_fixed(nex), exh_wall_temp_frac(nex)
    Else
        ReDim epull(nex)
    End If
    For n = 1 To nex - 1 'restore info
      ety(n) = gty(n): edr(n) = gdr(n)
      esg(n) = gsg(n): ebg(n) = gbg(n)
      ewd(n) = gwd(n): eht(n) = ght(n)
      If flow_domain = 2 Then
        epull(n) = gpull(n) 'melt
      Else
        exh_wall_temp_type(n) = gexh_wall_temp_type(n)
        exh_wall_temp_fixed(n) = gexh_wall_temp_fixed(n)
        exh_wall_temp_frac(n) = gexh_wall_temp_frac(n)
      End If
    Next
    ety(nex) = gty(iex): edr(nex) = gdr(iex) 'set new info same as current
    esg(nex) = gsg(iex): ebg(nex) = gbg(iex)
    ewd(nex) = gwd(iex): eht(nex) = ght(iex)
    If flow_domain = 2 Then
        epull(n) = 1 / nex
    Else
        exh_wall_temp_type(nex) = gexh_wall_temp_type(iex)
        exh_wall_temp_fixed(nex) = gexh_wall_temp_fixed(iex)
        exh_wall_temp_frac(nex) = gexh_wall_temp_frac(iex)
    End If
    iex = nex
  ElseIf i = 4 Then 'delete exhaust
    If gridProtectMode Then Return
    If nex <= 1 Then Exit Sub
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If iex < nex Then
      For n = iex To nex - 1
        ety(n) = ety(n + 1): edr(n) = edr(n + 1)
        esg(n) = esg(n + 1): ebg(n) = ebg(n + 1)
        ewd(n) = ewd(n + 1): eht(n) = eht(n + 1)
        If flow_domain = 2 Then
            epull(n) = epull(n + 1)
        Else
            exh_wall_temp_type(n) = exh_wall_temp_type(n + 1)
            exh_wall_temp_fixed(n) = exh_wall_temp_fixed(n + 1)
            exh_wall_temp_frac(n) = exh_wall_temp_frac(n + 1)
        End If
      Next
    Else
      iex = iex - 1
    End If
    nex = nex - 1
  ElseIf i = 0 Then
    Call ctcpn_Click
    GoTo ls11
  End If
  Call exh_ls
  Return

ls1g: 'geometry
  If chamber_type = 2 Then GoTo ls1g1
  If i = 0 Then
    Call ctbr_Click: GoTo ls11
  ElseIf i = 1 Then
    Call Lb1_Click
  ElseIf i = 2 Then
    Call Lb2_Click
  ElseIf i = 3 Then
    Call Lb3_Click
  ElseIf i = 4 Then
    If flow_domain = 1 Then Call Lb4_Click
  End If
  Exit Sub

ls1g1:
  If i = 0 Then
    Call ctbr_Click
  Else
    melter_component = i: Call geom_ls
  End If
  Call plt_cb
  Exit Sub

ls1s:  'simulation parameter information
  If i = 1 Then
    flag_conditions_modified = True
    flag_preproc_modified = True
    irstyp = irstyp + 1
    If irstyp >= 2 Then irstyp = 0
  ElseIf i = 2 Then
    flag_conditions_modified = True
    flag_preproc_modified = True
    s0 = "Specify number of iterations for CFD calculation"
    g0 = maxgi: val_in2
    If g0 > 0 Then maxgi = g0
    If flow_domain = 1 And interval_rad > maxgi And (id_rad = 1 Or ms = 1) Then
        'Radiation will not be run
        If MsgBox("Note that the number of gas phase iterations is less than the" _
            & " iteration interval between doing radiation or minor species calculations." _
            & " Radiation calculations will only be done if the radiation interval" _
            & " is equal to or less than the iterations.  Adjust as needed.", _
            vbExclamation, "GFM") = vbOK Then s0 = "" 'dummy then
    End If
  ElseIf i = 3 Then
    flag_conditions_modified = True
    flag_preproc_modified = True
    s0 = "Specify convergence criteria for CFD calculation"
    g0 = bgcon: val_in2
    If g0 >= 0 Then bgcon = g0
  ElseIf i = 4 Then
    flag_conditions_modified = True
    flag_preproc_modified = True
    If flow_domain = 1 Then
        If id_rad = 1 Then
            id_rad = -1 'no radiation calculations
            If ms = 1 Then interval_rad = maxms
        Else 'id_rad=-1
            id_rad = 1 'user wants to do radiation calculations
            If ms = 0 Then
                'radiation requires minor species calculation
                If MsgBox("Note that subspecies iterations must be greater than" _
                    & " zero if radiation calculations are being done." _
                    & " Adjust as needed.", _
                    vbExclamation, "GFM") = vbOK Then s0 = "" 'dummy then
            End If
        End If
        If (id_rad = 1 Or ms = 1) And interval_rad > maxgi Then
            'minor species or radiation will not be run
            If MsgBox("Note that the number of gas phase iterations is less than the" _
                & " iteration interval between doing radiation or minor species calculations." _
                & " Radiation calculations will only be done if the radiation interval" _
                & " is equal to or less than the iterations.  Adjust as needed.", _
                vbExclamation, "GFM") = vbOK Then s0 = "" 'dummy then
        End If
    Else 'flow_domain=2
        s0 = "K": g0 = start_temp: Call ucv
        s0 = "Specify new start temperature (" & s0 & ")"
        Call val_in2: If g0 <= 0 Then Exit Sub
        s0 = "F": Call ucvb: start_temp = g0
    End If
    
  Else 'i=0, next object
    If flow_domain = 2 Then
        Call ctgp_Click
    Else
        Call ctwp_Click
    End If
    GoTo ls11
  End If
  Call sim_ls
  Return

ls1u: 'bubbler
  If i = 1 Then
    If nsw <= 0 Then Return
    isw = isw + 1: If isw > nsw Then isw = 1
  ElseIf i = 3 Then 'add
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If nsw = 0 Then
      ReDim wdr(1), wsg(1), wbg(1), wwd(1), wdp(1)
      ReDim wtg(1), wfa(1)
      nsw = 1: isw = nsw: wtg(1) = 300: wfa(1) = 0.00000786579072 ' 10-2004 default bubbler air rate to 1 SCFH
      wdr(1) = 4: wwd(1) = 0.1: wdp(1) = 0.1
      wsg(1) = (lth - wwd(1)) / 2: wbg(1) = 1
    Else
      ReDim gdr(nsw), gsg(nsw), gbg(nsw), gwd(nsw), ght(nsw)
      ReDim gtg(nsw), gfa(nsw)
      For n = 1 To nsw
        gdr(n) = wdr(n): gsg(n) = wsg(n): gbg(n) = wbg(n)
        gwd(n) = wwd(n): ght(n) = wdp(n): gtg(n) = wtg(n)
        gfa(n) = wfa(n)
      Next
      nsw = nsw + 1
      ReDim wdr(nsw), wsg(nsw), wbg(nsw), wwd(nsw), wdp(nsw)
      ReDim wtg(nsw), wfa(nsw)
      For n = 1 To nsw - 1
        wdr(n) = gdr(n): wsg(n) = gsg(n): wbg(n) = gbg(n)
        wwd(n) = gwd(n): wdp(n) = ght(n): wtg(n) = gtg(n)
        wfa(n) = gfa(n)
      Next
      isw = nsw
      wdr(nsw) = wdr(nsw - 1): wsg(nsw) = wsg(nsw - 1)
      wwd(nsw) = wwd(nsw - 1): wdp(nsw) = wdp(nsw - 1)
      wbg(nsw) = wbg(nsw - 1) + wdp(nsw) * 2
      wtg(nsw) = wtg(nsw - 1): wfa(nsw) = wfa(nsw - 1)
    End If
  ElseIf i = 4 Then 'delete
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If nsw = 1 Then
      nsw = 0: isw = 0
    Else
      For n = isw + 1 To nsw
        wdr(n - 1) = wdr(n): wsg(n - 1) = wsg(n)
        wwd(n - 1) = wwd(n): wdp(n - 1) = wdp(n)
        wtg(n - 1) = wtg(n): wfa(n - 1) = wfa(n)
      Next
      nsw = nsw - 1
      If isw > nsw Then isw = nsw
    End If
  ElseIf i = 2 Then ' 8-11-2004 bug fix
    'can't change type, but stay with bubbler
    Exit Sub
  ElseIf i = 0 Then
    Call ctcpn_Click: Exit Sub
  End If
  Call plt_cb: Call bub_ls
  Exit Sub

ls1w: 'doghouse/sidewell
  If i = 0 Then
    Call ctcpn_Click: Exit Sub
  ElseIf i = 1 Then
    If nsw <= 0 Then Return
    isw = isw + 1: If isw > nsw Then isw = 1
  ElseIf i = 2 Then 'add
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    ReDim gdr(nsw), gsg(nsw), gwd(nsw), ght(nsw)
    For n = 1 To nsw
      gdr(n) = wdr(n): gsg(n) = wsg(n)
      gwd(n) = wwd(n): ght(n) = wdp(n)
    Next
    nsw = nsw + 1
    ReDim wdr(nsw), wsg(nsw), wwd(nsw), wdp(nsw)
    For n = 1 To nsw - 1
      wdr(n) = gdr(n): wsg(n) = gsg(n)
      wwd(n) = gwd(n): wdp(n) = ght(n)
    Next
    isw = nsw
    wdr(nsw) = 2: wsg(nsw) = 0
    wwd(nsw) = 1: wdp(nsw) = 1
  ElseIf i = 3 Then 'delete
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    For n = isw + 1 To nsw
      wdr(n - 1) = wdr(n): wsg(n - 1) = wsg(n)
      wwd(n - 1) = wwd(n): wdp(n - 1) = wdp(n)
    Next
    nsw = nsw - 1
    If isw > nsw Then isw = nsw
  Else
    Return
  End If
  Call ctsw_Click
  Return

ls1f:  'doing glass flow
  If i = 0 Then 'next object
    Call ctwp_Click
    Exit Sub
  ElseIf i = 1 Then 'next flow property
    nudf = nudf + 1: If nudf > 7 Then nudf = 1
    GoSub ls1f1
  ElseIf i = 2 Then 'next in set
    iudf = iudf + 1
  ElseIf i = 3 Then 'add
    'If gridProtectMode Then Exit Sub
    flag_preproc_modified = True
    flag_conditions_modified = True
    ReDim gav(mudf), gah(mudf)
    For n = 1 To mudf
      gav(n) = fx(n): gah(n) = tx(n)
    Next
    mudf = mudf + 1
    ReDim fx(mudf), tx(mudf)
    For n = 1 To mudf - 1
      fx(n) = gav(n): tx(n) = gah(n)
    Next
    If iudf = mudf - 1 Then
      tx(mudf) = tx(iudf) + 100
      fx(mudf) = fx(iudf)
    Else
      tx(mudf) = (tx(iudf) + tx(iudf + 1)) / 2
      fx(mudf) = (fx(iudf) + fx(iudf + 1)) / 2
    End If
    iudf = iudf + 1
  ElseIf i = 4 Then 'delete
    'If gridProtectMode Then Exit Sub
    If mudf <= 1 Then Return
    flag_preproc_modified = True
    flag_conditions_modified = True
    For n = iudf + 1 To mudf
      fx(n - 1) = fx(n): tx(n - 1) = tx(n)
    Next
    mudf = mudf - 1
    If iudf > 1 Then iudf = iudf - 1
  End If
  Call udf_ls
  Return

ls1f1:
  If nudf = 1 Then
    mudf = udf_ds_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_ds(m): tx(m) = udf_ds_t(m)
    Next
  ElseIf nudf = 2 Then
    mudf = udf_mu_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_mu(m): tx(m) = udf_mu_t(m)
    Next
  ElseIf nudf = 3 Then
    mudf = udf_k_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_k(m): tx(m) = udf_k_t(m)
    Next
  ElseIf nudf = 4 Then
    mudf = udf_cl_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_cl(m): tx(m) = udf_cl_t(m)
    Next
  ElseIf nudf = 5 Then
    mudf = udf_a_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_a(m): tx(m) = udf_a_m(m)
    Next
  ElseIf nudf = 6 Then
    mudf = udf_clc_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_clc(m): tx(m) = udf_clc_t(m)
    Next
  ElseIf nudf = 7 Then
    mudf = udf_cls_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_cls(m): tx(m) = udf_cls_t(m)
    Next
  End If
  Return

ls1x:  'wall
  If i = 0 Then 'next object
      Call ctsm_Click
      Exit Sub
  End If
  Call wa_ls
  Exit Sub
  
  'Because the use of multiple wall sections has not been validated
  'and there are known problems with it, disable adding and deleting walls
  If i = 0 Then 'next object
      Call ctsm_Click
      Exit Sub
  ElseIf i = 1 Then 'go to next wall
    iwal = iwal + 1
  ElseIf i = 2 Then 'add wall
    If gridProtectMode Then Exit Sub
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    ReDim gax(nwal), gtg(nwal), gav(nwal), gah(nwal)
    ReDim gsg(nwal), gbg(nwal), gwd(nwal), ght(nwal)
    ReDim gdr(nwal), gew(nwal)
    For n = 1 To nwal
      gdr(n) = wa_dr(n)
      gsg(n) = wa_sg(n): gbg(n) = wa_bg(n)
      gwd(n) = wa_wd(n): ght(n) = wa_ht(n)
      gtg(n) = wa_ta(n): gav(n) = wa_d(n)
      gah(n) = wa_k(n): gax(n) = wa_h(n)
      gew(n) = wa_e(n)
    Next
    nwal = nwal + 1
    ReDim wa_d(nwal), wa_k(nwal), wa_h(nwal), wa_ta(nwal)
    ReDim wa_sg(nwal), wa_bg(nwal), wa_wd(nwal), wa_ht(nwal)
    ReDim wa_dr(nwal), wa_e(nwal)
    For n = 1 To nwal - 1
      wa_dr(n) = gdr(n)
      wa_sg(n) = gsg(n): wa_bg(n) = gbg(n)
      wa_wd(n) = gwd(n): wa_ht(n) = ght(n)
      wa_ta(n) = gtg(n): wa_d(n) = gav(n)
      wa_k(n) = gah(n): wa_h(n) = gax(n)
      wa_e(n) = gew(n)
    Next
    wa_dr(nwal) = gdr(iwal)
    wa_sg(nwal) = gsg(iwal): wa_bg(nwal) = gbg(iwal)
    wa_wd(nwal) = 0: wa_ht(nwal) = 0
    wa_ta(nwal) = gtg(iwal): wa_d(nwal) = gav(iwal)
    wa_k(nwal) = gah(iwal): wa_h(nwal) = gax(iwal)
    wa_e(nwal) = gew(iwal)
    iwal = nwal
  ElseIf i = 3 Then 'delete wall
    If gridProtectMode Then Exit Sub
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If iwal = nwal Then
      iwal = iwal - 1
    Else
    For n = iwal To nwal - 1
      wa_dr(n) = wa_dr(n + 1)
      wa_sg(n) = wa_sg(n + 1): wa_bg(n) = wa_bg(n + 1)
      wa_wd(n) = wa_wd(n + 1): wa_ht(n) = wa_ht(n + 1)
      wa_ta(n) = wa_ta(n + 1): wa_d(n) = wa_d(n + 1)
      wa_k(n) = wa_k(n + 1): wa_h(n) = wa_h(n + 1)
      wa_e(n) = wa_e(n + 1)
    Next
    End If
    nwal = nwal - 1
    
  Else
    Exit Sub
  End If
  Call wa_ls
  Return

ls1z: 'electric booster
  If i = 0 Then
    Call ctcpn_Click: Exit Sub
  ElseIf i = 1 Then
    If neb > 0 Then ebty0 = ebty(ieb) Else ebty0 = 0
    ebty0 = ebty0 + 1
    If ebty0 > 3 Then ebty0 = 1
    If ebty0 = 3 Then
      ebsg(neb, 3) = ebsg(neb, 2) * 2 - ebsg(neb, 1)
      ebbg(neb, 3) = ebbg(neb, 2) * 2 - ebbg(neb, 1)
    End If
    If neb > 0 Then ebty(ieb) = ebty0
  ElseIf i = 2 Then 'add
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If neb = 0 Then
      ReDim ebty(1), ebvt(1), ebpw(1), ebhl(1)
      ReDim ebsg(1, 3), ebbg(1, 3), ebwd(1, 3)
      ReDim ebht(1, 3), ebdr(1, 3)
      ebty(1) = ebty0: ebvt(1) = 110
      ebpw(1) = 10000: ebhl(1) = 1000
      For j = 1 To 3
        ebdr(1, j) = 4: ebwd(1, j) = 0.1
        ebht(1, j) = hgt / 2: ebsg(1, j) = lth / 3
        ebbg(1, j) = wdt / 10
      Next
      g0 = ebwd(1, 1) * 4: ebsg(1, 2) = ebsg(1, 1) + g0
      If ebty0 = 3 Then ebbg(1, 3) = ebbg(1, 1) + g0
      neb = 1: ieb = 1: jeb = 1
    Else 'neb>0
      ReDim gty(neb), gtg(neb), gav(neb), gah(neb)
      ReDim gsg(neb, 3), gbg(neb, 3), gwd(neb, 3), ght(neb, 3), gdr(neb, 3)
      For n = 1 To neb
        gty(n) = ebty(n): gtg(n) = ebvt(n)
        gav(n) = ebpw(n): gah(n) = ebhl(n)
        For j = 1 To 3
          gdr(n, j) = ebdr(n, j)
          gsg(n, j) = ebsg(n, j): gbg(n, j) = ebbg(n, j)
          gwd(n, j) = ebwd(n, j): ght(n, j) = ebht(n, j)
      Next: Next
      neb = neb + 1
      ReDim ebty(neb), ebvt(neb), ebpw(neb), ebhl(neb)
      ReDim ebsg(neb, 3), ebbg(neb, 3)
      ReDim ebwd(neb, 3), ebht(neb, 3), ebdr(neb, 3)
      For n = 1 To neb - 1
        ebty(n) = gty(n): ebvt(n) = gtg(n)
        ebpw(n) = gav(n): ebhl(n) = gah(n)
        For j = 1 To 3
          ebdr(n, j) = gdr(n, j)
          ebsg(n, j) = gsg(n, j): ebbg(n, j) = gbg(n, j)
          ebwd(n, j) = gwd(n, j): ebht(n, j) = ght(n, j)
      Next: Next: n = neb - 1
      ebty(neb) = gty(n): ebvt(neb) = gtg(n)
      ebpw(neb) = gav(n): ebhl(neb) = gah(n)
      For j = 1 To 3: ebdr(neb, j) = gdr(n, j)
        ebsg(neb, j) = gsg(n, j): ebbg(neb, j) = gbg(n, j) + gwd(n, j) * 2
        ebwd(neb, j) = gwd(n, j): ebht(neb, j) = ght(n, j)
      Next: ieb = neb: jeb = 1
    End If
  ElseIf i = 3 Then 'delete
    If neb <= 0 Then Return
    flag_preproc_modified = True
    'If flag_grid_active = True Then flag_grid_modified = True
    flag_grid_modified = True
    If ieb < neb Then
    For n = ieb + 1 To neb
      ebty(n - 1) = ebty(n): ebvt(n - 1) = ebvt(n)
      ebpw(n - 1) = ebpw(n): ebhl(n - 1) = ebhl(n)
      For j = 1 To 3: ebdr(n - 1, j) = ebdr(n, j)
      ebsg(n - 1, j) = ebsg(n, j): ebbg(n - 1, j) = ebbg(n, j)
      ebwd(n - 1, j) = ebwd(n, j): ebht(n - 1, j) = ebht(n, j)
      Next
    Next: End If
    neb = neb - 1: jeb = 1
    If ieb > 1 Then ieb = ieb - 1
  ElseIf i = 4 Then
    If neb <= 0 Then Return
    ieb = ieb + 1: If ieb > neb Then ieb = 1
  End If
  Call eb_ls: Call plt_cb
End Sub

'-------------------------------
'Change List 2 values for grid construction
'-------------------------------
Private Sub List2_Click()
  Dim s1 As String, s2 As String
  Dim rp_0()
  Dim inorout As Integer '1=outer section of tube-in-tube burner, 2=inner section
  Dim old_value As Single
  need_burner_check = False
  need_tube_check = False
  ttl = LCase(List1.List(0))
  i = List2.ListIndex
  If Not gridProtectMode Then
      If InStr(ttl, "burn") > 0 Then GoSub ls2b
      If InStr(ttl, "exh") > 0 Then GoSub ls2e
      If InStr(ttl, "dog") > 0 Then GoSub ls2w
      If InStr(ttl, "char") > 0 Then GoSub ls2b
      If InStr(ttl, "out") > 0 Then GoSub ls2e
      If InStr(ttl, "bub") > 0 Then GoSub ls2u
      If InStr(ttl, "boo") > 0 Then GoSub ls2z
      If InStr(ttl, "wall") > 0 Then GoSub ls2x
      If InStr(ttl, "geo") > 0 Then GoSub ls2g
  End If
  If InStr(ttl, "para") > 0 Then GoSub ls2s
  If InStr(ttl, "glass") > 0 Then GoSub ls2f
  Call plt_cb
   
  If need_burner_check = True Then
    'Note round burner gaps are measured from furnace edge to center of burner
    'whereas rectangular burner gaps are measured from furnace edge to first edge of burner
    If bty(ibr) <= 1 Then 'round shaped burner
        g0 = bwd(ibr, 1) / 2#
        g1 = g0
    Else 'rectangular shaped burner
        g0 = bwd(ibr, 1)
        g1 = bht(ibr, 1)
    End If
   
    If flow_domain = 1 Then
        s0 = "Warning: Burner extends beyond furnace."
    Else
        s0 = "Warning: Charger extends beyond furnace."
    End If
    Select Case bdr(ibr) 'orientation
        Case 0, 1 'back or front wall
            If bsg(ibr, 1) + g0 > wdt Or (g0 > bsg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
        Case 2 To 5 'other walls
            If bsg(ibr, 1) + g0 > lth Or (g0 > bsg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
    End Select
    Select Case bdr(ibr) 'orientation
        Case 0 To 3 'back, front, right, or left wall
            If bbg(ibr, 1) + g1 > hgt Or (g1 > bbg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
        Case 4, 5 'bottom or top wall
            If bbg(ibr, 1) + g1 > wdt Or (g1 > bbg(ibr, 1) And bty(ibr) <= 1) Then _
                MsgBox (s0)
    End Select
  End If
  
  If need_tube_check = True Then
    'Verify inner section is contained within outer section  June07
    If bsg(ibr, 2) < bsg(ibr, 1) Or bbg(ibr, 2) < bbg(ibr, 1) Or _
        (bsg(ibr, 2) + bwd(ibr, 2)) > (bsg(ibr, 1) + bwd(ibr, 1)) Or _
        (bbg(ibr, 2) + bht(ibr, 2)) > (bbg(ibr, 1) + bht(ibr, 1)) _
        Then MsgBox ("Warning: Inner tube is not fully contained within outer tube.")
  End If
  Exit Sub

ls2bg:
  sg0 = sg1: bg0 = 0: Call projw
  p2 = p1: q2 = q1
  bg0 = bg1: Call projw: Call vector
  If dr0 <= 3 Then
    s0 = s0 & "bottom gap"
  Else
    s0 = s0 & "side gap"
  End If
  g0 = bg0: Call val_in(0)
  Return

ls2sg:
  sg0 = 0: bg0 = bg1: Call projw
  p2 = p1: q2 = q1
  sg0 = sg1: Call projw: Call vector
  If dr0 <= 1 Then
    s0 = s0 & "side gap"
  Else
    s0 = s0 & "end gap"
  End If
  g0 = sg0: Call val_in(0)
  Return

ls2wd:
  Call projw: p2 = p1: q2 = q1
  sg0 = sg0 + wd0: Call projw: Call vector
  s0 = s0 & "width": g0 = wd0: Call val_in(1)
  Return

ls2ht:
  Call projw: p2 = p1: q2 = q1
  bg0 = bg0 + ht0: Call projw: Call vector
  s0 = s0 & "height": g0 = ht0: Call val_in(1)
  Return

ls2s1:
  n = n + 1
  If n > 5 Then n = 0
  'do not allow exhaust into melt surface
  If n = 4 And InStr(ttl, "exh") > 0 Then n = 5
  'do not allow burner from melt surface
  If n = 4 And InStr(ttl, "burn") > 0 Then n = 5
  'do not allow outlet into combustion chamber
  If n = 5 And InStr(ttl, "out") > 0 Then n = 0
  'do not allow charger from melter bottom or top
  If (n = 4 Or n = 5) And InStr(ttl, "char") > 0 Then n = 0
  Return

ls2b: 'doing burner or charger
  If gridProtectMode Then Return
  need_burner_check = True
  j = ibr: dr0 = bdr(j): bty0 = bty(j)
  If bty(j) = 4 Then need_tube_check = True  '  June07
  If bty(j) = 4 And InStr(List3.List(4), "inner") > 0 _
        Then inorout = 2 Else inorout = 1

  If flow_domain = 1 Then
    ''if have regenerative burner, do not allow inner bsg to be modified
    'If inorout = 2 And i = 1 Then Return
    s0 = "Specify burner "
  Else
    s0 = "Specify charger "
  End If
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  
  If i = 0 Then
    n = bdr(j): GoSub ls2s1: bdr(j) = n
  ElseIf i = 1 Then
    bg1 = bbg(j, inorout): sg1 = bsg(j, inorout)
    old_value = sg1
    GoSub ls2sg: bsg(j, inorout) = g0
    If bty(j) = 4 And inorout = 1 Then  'June07
        'changed outer side gap of tube-in-tube burner, need to change inner also
        If old_value < g0 Then
            bsg(j, 2) = bsg(j, 2) + (g0 - old_value)
        ElseIf old_value > g0 Then
            bsg(j, 2) = bsg(j, 2) - (old_value - g0)
        End If
     End If
  
  ElseIf i = 2 Then
    sg1 = bsg(j, inorout): bg1 = bbg(j, inorout)
    old_value = bg1
    GoSub ls2bg: bbg(j, inorout) = g0
    If bty(j) = 4 And inorout = 1 Then  'June07
        'changed outer side gap of tube-in-tube burner, need to change inner also
        If old_value < g0 Then
            bbg(j, 2) = bbg(j, 2) + (g0 - old_value)
        ElseIf old_value > g0 Then
            bbg(j, 2) = bbg(j, 2) - (old_value - g0)
        End If
     End If
    
  ElseIf i = 3 Then
    If bty0 = 0 Then
      s0 = s0 & "diameter"
    ElseIf bty0 = 1 Then
      s0 = s0 & "outer diameter"
    Else
      s0 = s0 & "width"
    End If
    g0 = bwd(j, inorout): Call val_in(1): bwd(j, inorout) = g0
  ElseIf i = 4 Then
    If bty0 = 1 Then 'have ring
      s0 = s0 & "inner diameter"
    Else
      s0 = s0 & "height"
    End If
    g0 = bht(j, inorout): Call val_in(1)
    If flow_domain = 2 And g0 > dz0 Then g0 = dz0
    bht(j, inorout) = g0
  End If
  If flow_domain = 1 Then Call burn_ls
  If flow_domain = 2 Then Call char_ls
  Return

ls2e: 'doing exhaust or outlet
  If gridProtectMode Then Return
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  j = iex: dr0 = edr(j)
  s0 = "Specify exhaust "
  If i = 0 Then
    n = edr(j): GoSub ls2s1: edr(j) = n
  ElseIf i = 1 Then
    bg1 = ebg(j): sg1 = esg(j)
    GoSub ls2sg: esg(j) = g0
  ElseIf i = 2 Then
    sg1 = esg(j): bg1 = ebg(j)
    GoSub ls2bg: ebg(j) = g0
  ElseIf i = 3 Then
    sg0 = esg(j): bg0 = ebg(j): wd0 = ewd(j)
    GoSub ls2wd: ewd(j) = g0
  ElseIf i = 4 Then
    sg0 = esg(j): bg0 = ebg(j): ht0 = eht(j)
    GoSub ls2ht: eht(j) = g0
  End If
  Call exh_ls
  Return

ls2g: 'geometry for melter with refiner
  If i = 1 Then
    Call Lb1_Click
  ElseIf i = 2 Then
    Call Lb2_Click
  ElseIf i = 3 Then
    Call Lb3_Click
  End If
  Return

ls2s: 'simulation parameters
  flag_conditions_modified = True
  flag_preproc_modified = True
  If flow_domain = 1 Then GoSub ls2s1a
  If flow_domain = 2 Then GoSub ls2s2
  Call sim_ls
  Return

ls2s1a:
  If i = 1 Then
    s0 = "Pa": g0 = pg0: Call ucv
    s0 = "Specify furnace pressure (" & s0 & ")"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "psi": Call ucvb: pg0 = g0
  ElseIf i = 3 Then
    s0 = "J/m^3": g0 = qh0: Call ucv
    s0 = "Specify heat of combustion (" & s0 & ")"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "Btu/ft^3": Call ucvb: qh0 = g0
  ElseIf i = 4 Then
    If oxy_fuel = 0 Then oxy_fuel = 1 Else oxy_fuel = 0
  Else
    Exit Sub
  End If
  Return

ls2s2:
  s0 = List2.List(0)
  If InStr(s0, "cullet") > 0 Then GoTo ls2s2a
  'List2.List(0) = "particle: sand"
  List2.List(0) = "particle: batch"
  fr_s = 0: fr_c = 0
  For j = 1 To nbr
    g0 = bfg(j, 1): g2 = g0 * bfg(j, 2): g1 = g0 - g2
    fr_s = fr_s + g1: fr_c = fr_c + g2
  Next
  If i = 0 Then
    If fr_c > 0 Then
      List2.List(0) = "particle: cullet"
      If nps0_c < 1 Then nps0_c = 1
    ElseIf nphas = 3 Then
      List2.List(0) = "bubble"
      If nbs0 < 1 Then nbs0 = 1
    End If
  ElseIf i = 1 Then
    s0 = "Specify total # of particle groups:"
    g0 = nps0_s: Call val_in2: n1 = Int(g0)
    If n1 > nps0_s Then
      ReDim rp_0(nps0_s)
      For i = 1 To nps0_s: rp_0(i) = bav(i, 1): Next
      ReDim bav(n1)
      For i = 1 To nps0_s: bav(i, 1) = rp_0(i): Next
      For i = nps0_s + 1 To n1
      bav(i, 1) = bav(i - 1, 1) * 2: Next
    End If
    nps0_s = n1
  ElseIf i = 2 Then
    ips0_s = ips0_s + 1
    If ips0_s > nps0_s Then ips0_s = 1
  ElseIf i = 3 Then
    s0 = "Specify batch particle radius "
    g0 = bav(ips0_s, 1): Call val_in(2): bav(ips0_s, 1) = g0
  ElseIf i = 4 Then
    s1 = "Specify batch melt "
    g0 = tm_s: GoSub ls2s2a4: tm_s = g0
  End If
  Return

ls2s2a:
  If i = 0 Then
    List2.List(0) = "particle: batch"
  ElseIf i = 1 Then
    s0 = "Specify total # of " & s0 & " particle groups:"
    g0 = nps0_c: Call val_in2: n1 = Int(g0)
    If n1 > nps0_c Then
      ReDim rp_0(nps0_c)
      For i = 1 To nps0_c: rp_0(i) = bah(i, 1): Next
      ReDim bah(n1, 2)
      For i = 1 To nps0_c: bah(i, 1) = rp_0(i): Next
      For i = nps0_c + 1 To n1
      bah(i, 1) = bah(i - 1, 1) * 2: Next
    End If
    nps0_c = n1
  ElseIf i = 2 Then
    ips0_c = ips0_c + 1
    If ips0_c > nps0_c Then ips0_c = 1
  ElseIf i = 3 Then
    s0 = "Specify cullet particle radius "
    g0 = bah(ips0_c, 1): Call val_in(0): bah(ips0_c, 1) = g0
  ElseIf i = 4 Then
    s1 = "Specify cullet melt "
    g0 = tm_c: GoSub ls2s2a4: tm_c = g0
  End If
  Return

ls2s2a4:
  s0 = "K": Call ucv
  s0 = s1 & "temperature (" & s0 & ")" & vbCrLf
  s0 = s0 & "  * only positive number"
  Call val_in2: If g0 <= 0 Then Exit Sub
  s0 = "F": Call ucvb
  Return

ls2u: 'bubbler
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  j = isw: dr0 = wdr(j)
  s0 = "Specify bubbler "
  If i = 0 Then
    wdr(j) = wdr(j) + 1
    If wdr(j) > 5 Then wdr(j) = 0
  ElseIf i = 1 Then
    bg1 = wbg(j): sg1 = wsg(j)
    GoSub ls2sg: wsg(j) = g0
  ElseIf i = 2 Then
    sg1 = wsg(j): bg1 = wbg(j)
    GoSub ls2bg: wbg(j) = g0
  ElseIf i = 3 Then
    s0 = "Specify bubbler diameter"
    g0 = wwd(j): Call val_in(1): wwd(j) = g0
  ElseIf i = 4 Then
    s0 = "Specify bubbler height"
    g0 = wdp(j): Call val_in(1): wdp(j) = g0
  End If
  Call bub_ls
  Return

ls2w: 'dog house
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  j = isw: dr0 = wdr(j)
  s0 = "Specify dog house"
  If i = 0 Then
    wdr(j) = wdr(j) + 1
    If wdr(j) > 3 Then wdr(j) = 0
  ElseIf i = 1 Then
    bg1 = 0: sg1 = wsg(j)
    GoSub ls2sg: wsg(j) = g0
  ElseIf i = 2 Then
    sg0 = wsg(j): bg0 = 0: wd0 = wwd(j)
    GoSub ls2wd: wwd(j) = g0
  ElseIf i = 3 Then
    sg0 = wsg(j): bg0 = 0: Call projw
    p2 = p1: q2 = q1: z0 = 0
    If dr0 = 0 Then
      x0 = -wdp(j): y0 = sg0
    ElseIf dr0 = 1 Then
      x0 = lth + wdp(j): y0 = sg0
    ElseIf dr0 = 2 Then
      x0 = sg0: y0 = -wdp(j)
    ElseIf dr0 = 3 Then
      x0 = sg0: y0 = wdt + wdp(j)
    End If
    Call proj2d
    If dr0 = 0 Or dr0 = 2 Then
      g0 = p1: p1 = p2: p2 = g0
      g0 = q1: q1 = q2: q2 = g0
    End If
    Call vector
    s0 = "Specify dog hous depth"
    g0 = wdp(j): Call val_in(1): wdp(j) = g0
  End If
  Call ctsw_Click
  Return

ls2f: 'Increments to next item in glass properties display
  'If the green box index is greater than the number of enteries in the function then do nothing
  If i + 1 > mudf Then Exit Sub
  
  flag_preproc_modified = True
  flag_conditions_modified = True
  j = judf + i: iudf = j: g0 = fx(j)
  If nudf = 1 Then
    s0 = "kg/m^3": s1 = "lb/ft^3": s2 = "density"
  ElseIf nudf = 2 Then
    s0 = "Pa-s": s1 = "psi-s": s2 = "viscosity"
  ElseIf nudf = 3 Then
    s0 = "W/m/K": s1 = "Btu/ft/F/s"
    s2 = "thermal conductivity"
  ElseIf nudf = 4 Then
    s0 = "J/kg": s1 = "Btu/lb": s2 = "specific heat"
  ElseIf nudf = 5 Then
    s0 = "1/cm": s1 = "1/cm": s2 = "vol absorp"
  End If
  Call ucv
  s2 = s2 & " (" & s0 & ")"
  s0 = "Specify a positive number for " & s2
  Call val_in2: If g0 <= 0 Then Exit Sub
  s0 = s1: Call ucvb: fx(j) = g0
  Call udf_ls
  Return

ls2x: 'doing walls
  If i > 0 And gridProtectMode Then Return
  flag_preproc_modified = True
  flag_conditions_modified = True
  j = iwal: dr0 = wa_dr(j)
  s0 = "Specify wall "
  If i = 0 Then
    j1 = wa_dr(j) + 1
    If j1 > 5 Then j1 = 0
    wa_dr(j) = j1
  ElseIf i = 1 Then
    bg1 = wa_bg(j): sg1 = wa_sg(j)
    GoSub ls2sg: wa_sg(j) = g0
  ElseIf i = 2 Then
    sg1 = wa_sg(j): bg1 = wa_bg(j)
    GoSub ls2bg: wa_bg(j) = g0
  ElseIf i = 3 Then
    sg0 = wa_sg(j): bg0 = wa_bg(j): wd0 = wa_wd(j)
    GoSub ls2wd: wa_wd(j) = g0
  ElseIf i = 4 Then
    sg0 = wa_sg(j): bg0 = wa_bg(j): ht0 = wa_ht(j)
    GoSub ls2ht: wa_ht(j) = g0
  Else
    Return
  End If
  Call wa_ls
  Return

ls2z: 'electric booster
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  j = ieb: dr0 = ebdr(j, jeb)
  s0 = "Specify ebooster "
  If i = 0 Then
    j1 = ebdr(j, jeb) + 1
    If j1 > 5 Then j1 = 0
    ebdr(j, jeb) = j1
  ElseIf i = 1 Then
    bg1 = ebbg(j, jeb): sg1 = ebsg(j, jeb)
    GoSub ls2sg: ebsg(j, jeb) = g0
  ElseIf i = 2 Then
    sg1 = ebsg(j, jeb): bg1 = ebbg(j, jeb)
    GoSub ls2bg: ebbg(j, jeb) = g0
  ElseIf i = 3 Then
    s0 = s0 & "diameter"
    g0 = ebwd(j, jeb): Call val_in(0): ebwd(j, jeb) = g0
  ElseIf i = 4 Then
    s0 = s0 & "height"
    g0 = ebht(j, jeb): Call val_in(0): ebht(j, jeb) = g0
  End If
  Call eb_ls: Call plt_cb
  Return
End Sub

'-------------------------------
'-------------------------------
Private Sub opt10()
  s0 = LCase(frmMain.Caption)
  If InStr(s0, "grid") > 0 Then
    Call plt_gd(0): Exit Sub
  ElseIf InStr(s0, "post") > 0 Then
    Call plt_clr(0): Exit Sub
  ElseIf InStr(s0, "pre-") > 0 Then
    s0 = LCase(List1.List(0))
    If InStr(s0, "geom") > 0 Then
      Call geom_ls
    ElseIf InStr(s0, "burn") > 0 Then
      Call burn_ls
    ElseIf InStr(s0, "exh") > 0 Then
      Call exh_ls
    ElseIf InStr(s0, "para") > 0 Then
      Call sim_ls
    ElseIf InStr(s0, "char") > 0 Then
      Call char_ls
    End If
  End If
End Sub

'-------------------------------
'Change List 3 values for grid construction
'-------------------------------
Private Sub List3_Click()
  Dim inorout As Integer '1=outer section of tube-in-tube burner, 2=inner section
  ttl = LCase(List1.List(0))
  i = List3.ListIndex
  If InStr(ttl, "burn") > 0 Then GoTo ls3b
  If InStr(ttl, "para") > 0 Then GoTo ls3s
  If InStr(ttl, "char") > 0 Then GoTo ls3c
  If InStr(ttl, "bub") > 0 Then GoTo ls3u
  If InStr(ttl, "boo") > 0 Then GoTo ls3z
  If InStr(ttl, "glass") > 0 Then GoTo ls3f
  If InStr(ttl, "wall") > 0 Then GoTo ls3x
  If InStr(ttl, "out") > 0 Then GoTo ls3o
  If InStr(ttl, "exh") > 0 Then GoTo ls3o
  If InStr(ttl, "geo") > 0 Then GoTo ls3g
  Exit Sub

ls3b1:
  s2 = InputBox(s0, title, g0, 3000, 6000)
  If s2 = "" Then Exit Sub
  If IsNumeric(s2) Then g0 = s2
  Return

ls3b: 'burner
  flag_preproc_modified = True
  flag_conditions_modified = True
  j = ibr
  s1 = "Specify burner "
  If bty(j) = 4 And InStr(List3.List(4), "inner") > 0 _
      Then inorout = 2 Else inorout = 1
      'For tube-in-tube burners have inner=2 and outer=1 sets of info
  If i = 0 Then
    s0 = "K"
    g0 = btg(j, inorout): Call ucv
    s0 = s1 & "temperature (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "F": Call ucvb: btg(j, inorout) = g0
  ElseIf i = 1 Then
    g0 = bav(j, inorout)
    s0 = s1 & "vertical injection angle (deg):" & vbCrLf
    s0 = s0 & "  0: horizontal" & vbCrLf
    s0 = s0 & "  +: up" & vbCrLf
    s0 = s0 & "  -: down"
    GoSub ls3b1: bav(j, inorout) = g0
  ElseIf i = 2 Then
    g0 = bah(j, inorout)
    s0 = s1 & "horizontal injection angle (deg):" & vbCrLf
    s0 = s0 & "  0: vertical" & vbCrLf
    s0 = s0 & "  +: clockwise" & vbCrLf
    s0 = s0 & "  -: counter-clockwise"
    GoSub ls3b1: bah(j, inorout) = g0
  ElseIf i = 3 Then
'    g0 = bas(j)
'    s0 = s1 & "spray angle (deg):" & vbCrLf
'    s0 = s0 & "  * only non-nagative number"
'    GoSub ls3b1: If g0 >= 0 Then bas(j) = g0
  ElseIf i = 4 Then
    If InStr(List3.List(4), "outer") > 0 _
      Then s0 = "inner" Else s0 = "outer"
    If bty(j) = 4 Then 'have tube-in-tube burner
      List3.List(4) = "<- " & s0 & " section ->"
    Else
      List3.List(4) = s0 & " flow rates ->"
    End If
  End If
  Call burn_ls
  Exit Sub

ls3c: 'charger
  flag_preproc_modified = True
  flag_conditions_modified = True
  j = ibr
  If i = 0 Then
    s0 = "K": g0 = btg(j, 1): Call ucv
    s0 = "Specify batch temperature (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "F": Call ucvb: btg(j, 1) = g0
  ElseIf i = 1 Then
    s0 = "kg/s": g0 = bfg(j, 1): Call ucv
    s0 = "Specify batch charger rate (" & s0 & ")" _
      & vbCrLf & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "lb/hr": Call ucvb: bfg(j, 1) = g0
  ElseIf i = 2 Then
    g0 = bfg(j, 2)
    s0 = "Specify cullet ratio (0-1):"
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then g0 = s2
    If g0 < 0 Or g0 > 1 Then Exit Sub
    bfg(j, 2) = g0
  ElseIf i = 3 Then
    'dub0 = batch_velocity(j)
    g0 = batch_velocity(j)
    s0 = "m/s": Call ucv
    s0 = "Specify batch velocity (" & s0 & ")"
    's2 = InputBox(s0, title, dub0, 3000, 6000)
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    'If IsNumeric(s2) Then dub0 = s2
    'If dub0 < 0 Then Exit Sub
    's0 = "ft/s": Call ucvb
    'batch_velocity(j) = dub0
    If IsNumeric(s2) Then g0 = s2
    If g0 < 0 Then Exit Sub
    s0 = "ft/s": Call ucvb
    batch_velocity(j) = g0

  ElseIf i = 4 Then
    s0 = "J/kg"
    If InStr(List4.List(0), "cul") > 0 Then g0 = h0_c _
    Else g0 = h0_s
    Call ucv
    s1 = "Specify heat of fussion (" & s0 & ")"
    s2 = InputBox(s1, ttl, g0, 3000, 6000)
    If s2 = "" Or Not IsNumeric(s2) Then Exit Sub
    g0 = s2: Call ucvb
    If InStr(List4.List(0), "cul") > 0 Then h0_c = g0 _
    Else h0_s = g0
  End If
  Call char_ls
  Exit Sub

ls3g: 'geometry
  flag_preproc_modified = True
  'If flag_grid_active = True Then flag_grid_modified = True
  flag_grid_modified = True
  zf1a = zf1: zf2a = zf2: zf3a = zf3: rgb0 = ForeColor
  zf1 = 6000 / (gx_r(3) + lth_r(3))
  zf2 = zf1: zf3 = zf1 * zf3a / zf2a
  j = melter_component: ForeColor = vbRed
  dq0 = LbO.Top - LbP.Top + 200
  If j = 2 Then s1 = "throat" Else s1 = "refiner"
  If i = 1 Then
    x0 = 0: y0 = gy_r(j): z0 = 0
    Call proj2d: p2 = p1: q2 = q1 - dq0
    x0 = gx_r(j): Call proj2d: q1 = q1 - dq0
    Call vector
    s0 = "Specify length gap between reference and " & s1
    g0 = x0: Call val_in(0): gx_r(j) = g0
  ElseIf i = 2 Then
    x0 = gx_r(j): y0 = 0: z0 = 0
    Call proj2d: p2 = p1: q2 = q1 - dq0
    y0 = gy_r(j): Call proj2d: q1 = q1 - dq0
    Call vector
    s0 = "Specify width gap between reference and " & s1
    g0 = y0: Call val_in(-1): gy_r(j) = g0
  ElseIf i = 3 Then
    x0 = gx_r(j): y0 = gy_r(j): z0 = 0
    Call proj2d: p2 = p1: q2 = q1 - dq0
    z0 = gz_r(j): Call proj2d: q1 = q1 - dq0
    Call vector
    s0 = "Specify depth gap between reference and " & s1
    g0 = z0: Call val_in(-1)
    If j = 2 And g0 + hgt_r(2) > hgt_r(1) Then _
      g0 = hgt_r(1) - hgt_r(2)
    If j = 3 Then
      If g0 > hgt_r(1) Then g0 = hgt_r(1)
      hgt_r(3) = hgt_r(1) - g0
    End If
    gz_r(j) = g0
  End If
  zf1 = zf1a: zf2 = zf2a: zf3 = zf3a: ForeColor = rgb0
  Call geom_ls
  Call plt_cb
  Exit Sub

ls3s: 'simulation parameters
  flag_preproc_modified = True
  flag_conditions_modified = True
  If flow_domain = 2 Then GoTo ls3s2 'jump to continue with melt parameters
  'continue with combustion parameters
  If id_rad = -1 And ms = 0 Then Exit Sub 'there are no parameters to adjust
  If id_rad = 1 Then
    If i = 1 Then
        g0 = nwl
        s0 = "specify total number of wavelengths:" & vbCrLf
        s0 = s0 & "  * only positive number"
        Call val_in2: If g0 <= 0 Then Exit Sub
        nwl = g0: iwl = 1
        ReDim wl(nwl)
        For i = 1 To nwl: wl(i) = i * 10 / nwl: Next
    ElseIf i = 2 Then
        iwl = iwl + 1
        If iwl > nwl Then iwl = 1
    ElseIf i = 3 Then
        g0 = wl(iwl)
        If iwl = 1 Then g1 = 0 Else g1 = wl(iwl - 1)
        If iwl = nwl Then g2 = 100 Else g2 = wl(iwl + 1)
        s0 = "specify a new wavelength between " & _
        Format(g1, "0.##") & " and " & Format(g2, "0.##") _
        & "um"
        Call val_in2: If g0 <= g1 Or g0 >= g2 Then Exit Sub
        wl(iwl) = g0
    ElseIf i = 4 Then
        s0 = "Specify number of gas phase iterations between minor species and global radiation calculations."
        g0 = interval_rad: val_in2
        If g0 > 0 Then interval_rad = g0
        If interval_rad > maxgi Then
            'Radiation will not be run
            If MsgBox("Note that the iteration interval may not be greater than" _
                & " the number of gas phase iterations." _
                & " Adjust as needed.", _
                vbExclamation, "GFM") = vbOK Then s0 = "" 'dummy then
        End If
    End If
  ElseIf ms = 1 Then
    s0 = "Specify number of gas phase iterations between minor species calculations."
    g0 = interval_rad: val_in2
    If g0 > 0 Then interval_rad = g0
    If interval_rad > maxgi Then
        'minor species  will not be run
        If MsgBox("Note that the iteration interval may not be greater than" _
            & " the number of gas phase iterations." _
            & " Adjust as needed.", _
            vbExclamation, "GFM") = vbOK Then s0 = "" 'dummy then
    End If
  End If
  Call sim_ls
  Exit Sub
 
ls3s2: 'continue with melt parameters
  If i = 0 Then
    s0 = "Pa": g0 = pg0: Call ucv
    s0 = "Specify reference pressure (" & s0 & ")"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "psi": Call ucvb: pg0 = g0
  ElseIf i = 1 Then
    heat_flux_type = heat_flux_type + 1
    If heat_flux_type > 4 Then heat_flux_type = 1
  ElseIf i = 3 And (heat_flux_type = 1 Or heat_flux_type = 3) Then
    s0 = "MW": g0 = qsh: Call ucv
    s1 = "Specify fixed surface heat flux input (" & s0 & ")" & vbCrLf
    s0 = s1 & "  * only non-negatve number"
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then g0 = s2
    s0 = "MMBtu/hr": Call ucvb: qsh = g0
  ElseIf i = 4 Then
    If InStr(List2.List(0), "batch") > 0 Then
        s0 = "m": g0 = condLength_s: Call ucv
        s0 = "Specify conductivity thickness (length) for batch (" & s0 & ")"
        Call val_in2: If g0 <= 0 Then Exit Sub
        s0 = "ft": Call ucvb: condLength_s = g0
    Else 'must have cullet in list 2
        s0 = "m": g0 = condLength_c: Call ucv
        s0 = "Specify conductivity thickness (length) for cullet (" & s0 & ")"
        Call val_in2: If g0 <= 0 Then Exit Sub
        s0 = "ft": Call ucvb: condLength_c = g0
    End If
  End If
  Call sim_ls
  Exit Sub

ls3u: 'bubbler
  flag_preproc_modified = True
  flag_conditions_modified = True
  ' 10-7-2004 fixed below titles and spacing
  j = isw: s1 = "Specify bubbler "
  If i = 0 Then
    s0 = "K": g0 = wtg(j): Call ucv
    s0 = s1 & " temperature (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "F": Call ucvb: wtg(j) = g0
  ElseIf i = 1 Then
    s0 = "m^3/s": g0 = wfa(j): Call ucv
    s1 = s1 & " flow rate (" & s0 & ")" & vbCrLf
    s0 = s1 & "  * only non-negatve number"
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Return
    If IsNumeric(s2) Then g0 = s2
    s0 = "SCFH": Call ucvb: wfa(j) = g0
  End If
  Call bub_ls
  Exit Sub

ls3o: 'outlet or exhaust
  flag_preproc_modified = True
  flag_conditions_modified = True
  If i = 0 And flow_domain = 2 Then
    g0 = epull(iex)
    s0 = "Specify a pull rate at outlet (" & iex & ")" & vbCrLf
    s0 = s0 & "  * between 0 and 1"
    Call val_in2
    If g0 > 0 And g0 <= 1 Then epull(iex) = g0
   
  ElseIf i = 1 Then
    If flow_domain = 2 Then  'set number of cells in exit tunnel
        If gridProtectMode Then Exit Sub
        g0 = extra_tunnel_cells
        s0 = "Specify number of cells in length of exit tunnel beyond the initial wall. (Suggest 4)" _
            & Chr(10) & Chr(10) _
            & "Note that this parameter applies to all exits."
        s2 = InputBox(s0, title, g0, 3000, 6000)
        If s2 = "" Then Exit Sub
        If IsNumeric(s2) Then
            If s2 < 0 Then Exit Sub
            extra_tunnel_cells = s2
        End If
    Else 'specify exhaust wall temperature type, just toggle
        If exh_wall_temp_type(iex) = 0 Then
            exh_wall_temp_type(iex) = 1
        Else
            exh_wall_temp_type(iex) = 0
        End If
    End If
  
  ElseIf i = 2 Then
    If flow_domain = 2 Then  'set length of exit tunnel
        If gridProtectMode Then Exit Sub
        g0 = extra_tunnel_length
        s0 = "Specify length of exit tunnel beyond the initial wall." _
            & Chr(10) & Chr(10) _
            & "Note that this parameter applies to all exits."
        s2 = InputBox(s0, title, g0, 3000, 6000)
        If s2 = "" Then Exit Sub
        If IsNumeric(s2) Then
            If s2 < 0 Then Exit Sub
            extra_tunnel_length = s2
        End If
    Else 'specify exhaust wall temperature value or fraction of average wall temperature
        If exh_wall_temp_type(iex) = 0 Then 'fraction
            g0 = exh_wall_temp_frac(iex)
            s0 = "Specify the initial exhaust wall temperature for radiation as a fraction of the average wall temperature."
            s2 = InputBox(s0, title, g0, 3000, 6000)
            If s2 = "" Then Exit Sub
            If IsNumeric(s2) Then
                If s2 <= 0 Then Exit Sub
                If s2 > 1 Then Exit Sub
            End If
            exh_wall_temp_frac(iex) = s2
        Else 'fixed value
            s0 = "K"
            g0 = exh_wall_temp_fixed(iex): Call ucv
            s1 = "Specify the initial exhaust wall temperature for radiation."
            s0 = s1 & " (" & s0 & ")"
            Call val_in2: If g0 <= 0 Then Exit Sub
            s0 = "F": Call ucvb: exh_wall_temp_fixed(iex) = g0
        End If
    End If
    
  ElseIf i = 3 Then 'set number of crosswise cells in exit
    If gridProtectMode Then Exit Sub
    g0 = n_cells_across_exit
    s0 = "Specify number of cells across exits (in both crosswise directions). " _
         & "Choose an even number, 4 or greater." _
         & Chr(10) & Chr(10) _
         & "Note that this parameter applies to all exits."
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then
        int_temp = CInt(s2)
        If int_temp < 4 Then Exit Sub
        int_temp2 = int_temp Mod 2
        If int_temp2 <> 0 Then Exit Sub
        n_cells_across_exit = int_temp
    End If
    
  Else
    Exit Sub
  End If
  Call exh_ls
  Exit Sub

ls3f: 'glass properties
  'If the green box index is greater than the number of enteries in the function then do nothing
  If i + 1 > mudf Then Exit Sub

  flag_preproc_modified = True
  flag_conditions_modified = True
  j = judf + i: iudf = j
  If InStr(LCase(List1.List(1)), "absorp") > 0 Then
    g0 = tx(j): s0 = "um"
    s0 = "Specify a wave length (" _
    & s0 & ")"
    Call val_in2: If g0 <= 0 Then Exit Sub
    tx(j) = g0
  Else
    g0 = tx(j): s0 = "K": Call ucv
    s0 = "Specify a positive number for temperature (" _
    & s0 & ")"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "F": Call ucvb
    tx(j) = g0
  End If
  Call udf_ls
  Exit Sub

ls3x: ' wall properties
  flag_preproc_modified = True
  flag_conditions_modified = True
  j = iwal: s1 = "Specify wall "
  If i = 0 Then
    If gridProtectMode Then Exit Sub '??? not sure if flag should be set within the "if i"s
    s0 = s1 & "thickness "
    g0 = wa_d(j): Call val_in(0)
    wa_d(j) = g0
  ElseIf i = 1 Then
    s0 = "W/m/K": g0 = wa_k(j): Call ucv
    s0 = s1 & "conductivity (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "Btu/ft/F/s": Call ucvb: wa_k(j) = g0
  ElseIf i = 2 Then
    s0 = "W/m^2/K": g0 = wa_h(j): Call ucv
    's0 = "Specify external wall convectance (" & s0 & ")" & vbCrLf
    s0 = "Specify external wall heat transfer coefficient (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "Btu/ft^2/F": Call ucvb: wa_h(j) = g0
  ElseIf i = 3 Then
    s0 = "K": g0 = wa_ta(j): Call ucv
    s0 = "Specify ambient room temperature (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "F": Call ucvb: wa_ta(j) = g0
  ElseIf i = 4 Then
    g0 = wa_e(j)
    s0 = "specify surface emissivity of the wall (0-1)"
    Call val_in2: If g0 < 0 Or g0 > 1 Then Exit Sub
    wa_e(j) = g0
  End If
  Call wa_ls
  Exit Sub

ls3z: 'electric booster
  flag_preproc_modified = True
  flag_conditions_modified = True
  j = ieb: s1 = "Specify ebooster "
  If i = 0 Then
    jeb = jeb + 1
  ElseIf i = 1 Then
    s0 = s1 & "voltage (V)": g0 = ebvt(j)
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then ebvt(j) = s2
  ElseIf i = 2 Then
    s0 = s1 & "power rating (W)": g0 = ebpw(j)
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then ebpw(j) = s2
  ElseIf i = 3 Then
    s0 = s1 & "heat loss (W)": g0 = ebhl(j)
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then ebhl(j) = s2
  End If
  Call eb_ls
  Call plt_cb
End Sub

'-------------------------------
'Change List 4 values for grid construction
'-------------------------------
Private Sub List4_Click()
  flag_preproc_modified = True
  flag_conditions_modified = True
  ttl = LCase(List1.List(0))
  i = List4.ListIndex
  If InStr(ttl, "burn") > 0 Then GoTo ls4b
  If InStr(ttl, "para") > 0 Then GoTo ls4s
  If InStr(ttl, "char") > 0 Then GoTo ls4c
  Exit Sub

ls4b1:
  s1 = "Specify burner " & s1
  If feed_unit = "kg/s" Then s0 = "kg/s" Else s0 = "m^3/s"
  Call ucv
  s1 = s1 & " flow rate (" & s0 & ")" & vbCrLf
  s0 = s1 & "  * only non-negatve number"
  s2 = InputBox(s0, title, g0, 3000, 6000)
  If s2 = "" Then Return
  If IsNumeric(s2) Then g0 = s2
  If feed_unit = "kg/s" Then s0 = "lb/hr" Else s0 = "SCFH"
  Call ucvb
  Return

ls4b: 'burner
  j = ibr
  If InStr(List3.List(4), "inner") > 0 _
    Then n = 2 Else n = 1
  If i = 1 Then
    g0 = bfg(j, n): s1 = "natural gas"
    GoSub ls4b1: bfg(j, n) = g0
  ElseIf i = 2 Then
    g0 = bfa(j, n): s1 = "air"
    GoSub ls4b1: bfa(j, n) = g0
  ElseIf i = 3 Then
    g0 = bfo(j, n): s1 = "oxygen"
    GoSub ls4b1: bfo(j, n) = g0
  ElseIf i = 4 Then
    g0 = bfn(j, n): s1 = "nitrogen"
    GoSub ls4b1: bfn(j, n) = g0
  End If
  Call burn_ls
  Exit Sub

ls4c: 'charger
  If InStr(List4.List(0), "batch") > 0 Then n = 0 Else n = 1
  If i = 0 Then
    If n = 0 And bfg(ibr, 2) > 0 Then
      List4.List(0) = "cullet particle"
    Else
      List4.List(0) = "batch particle"
    End If
  ElseIf i = 1 Then
    s0 = "um"
    If n = 1 Then g0 = bah(ibr, 1) Else g0 = bav(ibr, 1)
    s0 = "Specify average particle radius (" & s0 & ")" & vbCrLf
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    If n = 1 Then bah(ibr, 1) = g0 Else bav(ibr, 1) = g0
  ElseIf i = 2 Then
    s0 = "K"
    If n = 1 Then g0 = tm_c Else g0 = tm_s
    Call ucv
    's0 = s1 & "specify melting temperature (" & s0 & ")" & vbCrLf
    s0 = "Specify melting temperature (" & s0 & ")" & vbCrLf ' 8-11-2004 fix
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "F": Call ucvb
    If n = 1 Then tm_c = g0 Else tm_s = g0
  ElseIf i = 3 Then
    s0 = "kg/m^3"
    If n = 1 Then g0 = ds_c Else g0 = ds_s
    Call ucv
    's0 = s1 & "specify particle density (" & s0 & ")" & vbCrLf
    s0 = "Specify particle density (" & s0 & ")" & vbCrLf ' 8-11-2004 fix
    s0 = s0 & "  * only positive number"
    Call val_in2: If g0 <= 0 Then Exit Sub
    s0 = "lb/ft^3": Call ucvb
    If n = 1 Then ds_c = g0 Else ds_s = g0
  'ElseIf i = 4 Then
    's0 = "J/kg/K"
    'If n = 1 Then g0 = cl_c Else g0 = cl_s
    'Call ucv
    ''s0 = s1 & "specify specific heat (" & s0 & ")" & vbCrLf
    's0 = "Specify specific heat (" & s0 & ")" & vbCrLf ' 8-11-2004 fix
    's0 = s0 & "  * only positive number"
    'Call val_in2: If g0 <= 0 Then Exit Sub
    's0 = "Btu/lb/R": Call ucvb
    'If n = 1 Then cl_c = g0 Else cl_s = g0
  End If
  Call char_ls
  Exit Sub

ls4s: 'simulation parameters
  If i = 0 Then
    If surf_type = 0 Then surf_type = 1 Else surf_type = 0
  ElseIf i = 1 Then
    If List4.List(0) <> "" Then
        s0 = "K": g0 = surf_temp: Call ucv
        s0 = "Specify surface temperature (" & s0 & ")"
        Call val_in2: If g0 <= 0 Then Exit Sub
        s0 = "F": Call ucvb: surf_temp = g0
    End If
  ElseIf i = 2 Then
        s0 = "K": g0 = start_temp: Call ucv
        s0 = "Specify new start temperature (" & s0 & ")"
        Call val_in2: If g0 <= 0 Then Exit Sub
        s0 = "F": Call ucvb: start_temp = g0
  ElseIf i = 3 Then
    If irstyp = 0 Then 'only do for new starts
        s0 = "Specify number of iterations before the radiation calculation begins."
        g0 = initial_gitr
        s2 = InputBox(s0, title, g0, 3000, 6000)
        If s2 = "" Then Exit Sub
        If IsNumeric(s2) = False Or s2 < 0 Then Exit Sub
        initial_gitr = s2
    End If
  ElseIf i = 4 Then
    s0 = "Specify number of minor species iterations in each global iteration interval."
    g0 = maxms
    s2 = InputBox(s0, title, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) = False Or s2 < 0 Then Exit Sub
    maxms = s2
    If maxms > 0 Then
        ms = 1
        If id_rad = -1 Then interval_rad = maxms
    Else
        ms = 0
    End If
  End If
  Call sim_ls
End Sub


Private Sub mnuFigure_Click()   ' 8-6-2004
  Select Case active_view
  Case vw_fig
      Exit Sub
  Case vw_grid
      'fig view implies full grid is available
  End Select
  ' 10-19-2004 Properties menu only visible in melter figure window
  If flow_domain = 2 Then
    mnuProp_GlassExitTemp.Visible = True
    mnuPropGlass.Visible = True
  Else
    mnuPropEmis.Visible = True
    mnuPropSoot.Visible = True
    mnuPropCFD.Visible = True
    MnuOptFed.Visible = True
  End If
  
  setup (1)
  active_view = vw_fig
  Call file10(1) 'display the figure
End Sub

Private Sub mnuFullGrid_Click()  ' 8-6-2004
  Select Case active_view
  Case vw_grid
      Exit Sub
  Case vw_fig
      'fig view implies full grid is available
  End Select
  ' 10-19-2004 if melt preprocessor turn off Properties menu going to grid view
  If flow_domain = 2 Then
    mnuProp_GlassExitTemp.Visible = True
    mnuPropGlass.Visible = True
  Else
    mnuPropEmis.Visible = True
    mnuPropSoot.Visible = True
    mnuPropCFD.Visible = True
  End If
  'Plot the grid
  Call initialize_grid_display
  Call setup(2)
  'frmMain.Caption = "grid construction"
  active_view = vw_grid
  Call plt_gd(0)
  MnuOptFed.Visible = False
End Sub

Private Sub mnuGlasstype1_Click()
' 10-20-2004 Set properties for Glasstype1
'(test setting properties off menu selection)
  udf_ds_n = 2
  ReDim udf_ds(2), udf_ds_t(2)
  'udf_ds(1) = 2913.7: udf_ds(2) = 2753.7
  udf_ds(1) = 2900#: udf_ds(2) = 2700#
  udf_ds_t(1) = 1000: udf_ds_t(2) = 2000
  udf_mu_n = 7
  ReDim udf_mu(7), udf_mu_t(7)
  udf_mu(1) = 11350: udf_mu(2) = 1742
  udf_mu(3) = 353: udf_mu(4) = 89: udf_mu(5) = 27
  udf_mu(6) = 9.4: udf_mu(7) = 3.7
  For n = 1 To udf_mu_n
  udf_mu_t(n) = 1000 + 100 * n: Next
  udf_k_n = 7
  ReDim udf_k(7), udf_k_t(7)
  udf_k(1) = 17.78: udf_k(2) = 25.57
  udf_k(3) = 34.28: udf_k(4) = 43.92: udf_k(5) = 54.48
  udf_k(6) = 65.96: udf_k(7) = 78.36
  For n = 1 To udf_k_n
  udf_k_t(n) = 1000 + 100 * n: Next
  udf_cl_n = 1
  ReDim udf_cl(1), udf_cl_t(1)
  udf_cl(1) = 1200: udf_cl_t(1) = 1100
  udf_a_n = 3
  ReDim udf_a(3), udf_a_m(3)
  udf_a_m(1) = 2.7: udf_a(1) = 0.312
  udf_a_m(2) = 4.4: udf_a(2) = 4.994
  udf_a_m(3) = 4.7: udf_a(3) = 24.789
  
  Call ctgp_Click
End Sub

Private Sub mnuProtectGrid_Click()
   If mnuProtectGrid.Checked = False Then
      'turn on grid protect mode
      mnuProtectGrid.Checked = True
      mnuProtectGrid.Enabled = True
      mnuProtectGrid.Visible = True
      gridProtectMode = True
      ctgeo.Enabled = False
      ctgd.Enabled = False
      opt4.Enabled = False 'disable density changes
      ctex.Enabled = False
      If flow_domain = 1 Then
         ctsw.Enabled = False
      End If
      If menu_layer <> InGridConstruction Then
         Call ct_ls(0, 0, 0, 0) 'clear out the green list boxes if they exist
      End If
      'end of turn on grid protect mode
      
   Else 'turn off protect mode
      If MsgBox(Chr(10) & Chr(10) & "IF YOU RECONSTRUCT YOUR GRID, YOU MAY LOOSE GRID EDITS. " _
            & Chr(10) & Chr(10) _
            & "The Option->Protect-Grid-Edits menu item is checked automatically when an " _
            & "enhanced grid edit has been made. " _
            & "An enhanced grid edit " _
            & "consists of changing a grid node type or adding, deleting, or moving grid " _
            & "lines.  These edits are destroyed when the grid is reconstucted, so changes " _
            & "that would require grid reconstruction are disabled when the " _
            & "Option->Protect-Grid-Edits menu item is checked. " _
            & "Be aware that unchecking this option " _
            & "will allow you to destroy any enhanced grid edits that have been made. " _
            & Chr(10) & Chr(10) _
            & "Are you sure you want to uncheck this option?", _
            vbYesNo + vbExclamation, "GFM Warning about Potential Loss of User Work") = vbNo _
            Then
                flag_grid_enhanced = True 'first time enhanced grid warning will not be repeated
                Exit Sub
      End If
      'turn off grid protect mode
      mnuProtectGrid.Checked = False
      mnuProtectGrid.Enabled = True
      mnuProtectGrid.Visible = True
      gridProtectMode = False
      ctgeo.Enabled = True
      ctgd.Enabled = True
      opt4.Enabled = True 'enable density changes
      ctex.Enabled = True
      If flow_domain = 1 Then
         ctsw.Enabled = True
      End If
   End If
End Sub


'-------------------------------
'Click on UserDefine in Properties menu (to input glass properties)
'10-19-2004
'-------------------------------
Private Sub mnuUserDefine_Click()
  
  'Revert to default settings when UserDefine is clicked
  udf_ds_n = 2
  ReDim udf_ds(2), udf_ds_t(2)
  udf_ds(1) = 2913.7: udf_ds(2) = 2753.7
  udf_ds_t(1) = 1000: udf_ds_t(2) = 2000
  udf_mu_n = 7
  ReDim udf_mu(7), udf_mu_t(7)
  udf_mu(1) = 11350: udf_mu(2) = 1742
  udf_mu(3) = 353: udf_mu(4) = 89: udf_mu(5) = 27
  udf_mu(6) = 9.4: udf_mu(7) = 3.7
  For n = 1 To udf_mu_n
  udf_mu_t(n) = 1000 + 100 * n: Next
  udf_k_n = 7
  ReDim udf_k(7), udf_k_t(7)
  udf_k(1) = 17.78: udf_k(2) = 25.57
  udf_k(3) = 34.28: udf_k(4) = 43.92: udf_k(5) = 54.48
  udf_k(6) = 65.96: udf_k(7) = 78.36
  For n = 1 To udf_k_n
  udf_k_t(n) = 1000 + 100 * n: Next
  udf_cl_n = 1
  ReDim udf_cl(1), udf_cl_t(1)
  udf_cl(1) = 1200: udf_cl_t(1) = 1100
  udf_a_n = 3
  ReDim udf_a(3), udf_a_m(3)
  udf_a_m(1) = 2.7: udf_a(1) = 0.312
  udf_a_m(2) = 4.4: udf_a(2) = 4.994
  udf_a_m(3) = 4.7: udf_a(3) = 24.789

  Call ctgp_Click
End Sub


'-------------------------------
'-------------------------------
Private Sub opt11_Click()
  unt = "British": opt11.Checked = True
  opt12.Checked = False
  Call opt10
End Sub

'-------------------------------
'-------------------------------
Private Sub opt12_Click()
  unt = "SI": opt12.Checked = True
  opt11.Checked = False
  Call opt10
End Sub

'-------------------------------
'-------------------------------
Private Sub opt20()
  If Lb1.Caption = "Length" Then
    plt_cb
  ElseIf Lb1.Caption = "x" Then
    plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub opt21_Click()
  s0 = "Specify zoom factor in the length direction"
  g0 = zf1: Call val_in2: zf1 = g0
  Call opt20
End Sub

'-------------------------------
'-------------------------------
Private Sub opt22_Click()
  s0 = "Specify zoom factor in the width direction"
  g0 = zf2: Call val_in2: zf2 = g0
  Call opt20
End Sub

'-------------------------------
'-------------------------------
Private Sub opt23_Click()
  s0 = "Specify zoom factor in the height direction"
  g0 = zf3: Call val_in2: zf3 = g0
  Call opt20
End Sub

'-------------------------------
'-------------------------------
Private Sub opt31_Click()
  s0 = "Specify horizontal location of the diagram position"
  g0 = p0: Call val_in2: p0 = g0
  Call opt20
End Sub

'-------------------------------
'-------------------------------
Private Sub opt32_Click()
  s0 = "Specify vertical location of the diagram position"
  g0 = q0: Call val_in2: q0 = g0
  Call opt20
End Sub

Private Sub opt41_Click()
'-------------------------------
'Increase grid density
'-------------------------------
    g0 = 1.25
    dx0 = dx0 * g0: dy0 = dy0 * g0: dz0 = dz0 * g0
    Call grid(0)
    Call setup(2)
    Call plt_gd(0)
    flag_grid_modified = True
    flag_preproc_modified = True
End Sub


Private Sub opt42_Click()
'-------------------------------
'Decrease grid density
'-------------------------------
    g0 = 0.8
    dx0 = dx0 * g0: dy0 = dy0 * g0: dz0 = dz0 * g0
    Call grid(0)
    Call setup(2)
    Call plt_gd(0)
    flag_grid_modified = True
    flag_preproc_modified = True
End Sub

'-------------------------------
'Change maximum grid cell length
'-------------------------------
Private Sub opt431_Click()
    s0 = "Specify maximum allowable node size in " _
         & "length direction"
    g0 = dx0: Call val_in(1): dx0 = g0
    Call grid(0)
    Call setup(2)
    Call plt_gd(0)
    flag_grid_modified = True
    flag_preproc_modified = True
End Sub

'-------------------------------
'Change maximum grid cell width
'-------------------------------
Private Sub opt432_Click()
    s0 = "Specify maximum allowable node size in " _
         & "width direction"
    g0 = dy0: Call val_in(1): dy0 = g0
    Call grid(0)
    Call setup(2)
    Call plt_gd(0)
    flag_grid_modified = True
    flag_preproc_modified = True
End Sub

'-------------------------------
'Change maximum grid cell height
'-------------------------------
Private Sub opt433_Click()
    s0 = "Specify maximum allowable node size in " _
         & "height direction"
    g0 = dz0: Call val_in2: dz0 = g0
    Call grid(0)
    Call setup(2)
    Call plt_gd(0)
    flag_grid_modified = True
    flag_preproc_modified = True
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltaddgd_Click()
  Lbadd_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltaddvv_Click()
  Lbdel_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltbd1_Click()
  g0 = fp_mx: s0 = fp_ut: Call ucv
  s1 = "Specify an upper bound for the plotting values ("
  s1 = s1 & s0 & "):"
  s2 = InputBox(s1, ttl, g0, 3000, 6000)
  If s2 = "" Then Exit Sub
  If IsNumeric(s2) Then
    g0 = s2: Call ucvb
    If g0 < fp_mn Then g0 = fp_mn
    fp_mx = g0
    Call plt_clr(0): Exit Sub
  End If
  s2 = "Invalid input (" & s2 & "), retry?"
  If MsgBox(s2, vbYesNo, ttl) = vbYes Then _
    Call optpltbd1_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltbd2_Click()
  g0 = fp_mn: s0 = fp_ut: Call ucv
  s1 = "Specify a lower bound for the plotting values ("
  s1 = s1 & s0 & "):"
  s2 = InputBox(s1, ttl, g0, 3000, 6000)
  If s2 = "" Then Exit Sub
  If IsNumeric(s2) Then
    g0 = s2: Call ucvb
    If g0 > fp_mx Then g0 = fp_mx
    fp_mn = g0
    Call plt_clr(0): Exit Sub
  End If
  s2 = "Invalid input (" & s2 & "), retry?"
  If MsgBox(s2, vbYesNo, ttl) = vbYes Then _
    Call optpltbd2_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltclr1_Click()
  If Lbmv.Caption = "color" Then Lbmv_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltclrbw_Click()
  If Lbmv.Caption = "b/w" Then Lbmv_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub optpltres_Click()
  If InStr(frmMain.Caption, "grid") > 0 Then Exit Sub
  g0 = 2000 / vwr
  s1 = "Specify a resolution density for plotting:"
  s2 = InputBox(s1, ttl, g0, 3000, 6000)
  If s2 = "" Then Exit Sub
  If IsNumeric(s2) Then
    g0 = s2: vwr = 2000 / g0
    Call plt_clr(0): Exit Sub
  End If
  s2 = "Invalid input (" & s2 & "), retry?"
  If MsgBox(s2, vbYesNo, ttl) = vbYes Then _
    Call optpltres_Click
End Sub




'-------------------------------
'-------------------------------
Private Sub optvl_Click()
  s0 = "Specify a length scale for plotting vector"
  g0 = vv0
  s2 = InputBox(s0, title, g0, 3000, 6000)
  If Not IsNumeric(s2) Then Exit Sub
  If s2 > 0 Then vv0 = s2
End Sub

'-------------------------------
'-------------------------------
Private Sub Optx_Click()
    s0 = LCase(Caption)
    If InStr(s0, "grid") > 0 Then Call plt_gd(0)
End Sub

'-------------------------------
'-------------------------------
Private Sub Opty_Click()
    s0 = LCase(Caption)
    If InStr(s0, "grid") > 0 Then Call plt_gd(0)
End Sub

'-------------------------------
'-------------------------------
Private Sub Optz_Click()
    s0 = LCase(Caption)
    If InStr(s0, "grid") > 0 Then Call plt_gd(0)
End Sub


Private Sub post0_Click() ' 03/14/2005
'Goes to menu Combustion Space-> postcb_click or Melter->post2_click
End Sub

'-------------------------------
'Plot results of the combustion simulation
'-------------------------------
Private Sub postcb_Click()
    flow_domain = 1
    Call setup(1)
    CD1.InitDir = dnm & "combustion"
    frmMain.Caption = "Post-Processor"
    menu_layer = InPostProc
    ex0.Caption = "Done"
    mnuCase.Enabled = True
    mnuCaseNew.Enabled = False
    mnuCaseNew.Visible = True
    mnuCaseOpen.Enabled = True
    mnuCaseOpen.Visible = True
    mnuCaseSave.Enabled = False
    mnuCaseSave.Visible = True
    mnuCaseSaveAs.Enabled = False
    mnuCaseSaveAs.Visible = True
    mnuCaseSaveAsFull.Enabled = False
    mnuCaseSaveAsFull.Visible = True
    optplt.Visible = True
    opt4.Visible = False
    mnuOptRunPlot.Visible = True
    
    Timer1.Enabled = False
    
    Call mnuCaseOpen_Click
End Sub

'-------------------------------
'Plot results of the melt simulation
'-------------------------------
Private Sub post2_Click()
    flow_domain = 2
    Call setup(1)
    CD1.InitDir = dnm & "melt"
    frmMain.Caption = "Post-Processor"
    menu_layer = InPostProc
    ex0.Caption = "Done"
    mnuCase.Enabled = True
    mnuCaseNew.Enabled = False
    mnuCaseNew.Visible = True
    mnuCaseOpen.Enabled = True
    mnuCaseOpen.Visible = True
    mnuCaseSave.Enabled = False
    mnuCaseSave.Visible = True
    mnuCaseSaveAs.Enabled = False
    mnuCaseSaveAs.Visible = True
    mnuCaseSaveAsFull.Enabled = False
    mnuCaseSaveAsFull.Visible = True
    optplt.Visible = True
    opt4.Visible = False
    mnuOptRunPlot.Visible = True
    Timer1.Enabled = False
    
    Call mnuCaseOpen_Click
End Sub

'-------------------------------
'-------------------------------
Private Sub proj2d()
  'given x0,y0,z0, scaling factors zf1,zf2,zf3, p0,q0 & theta
  'calculate xx,yy,zz p1,q1 positions on screen
  xx = x0 * zf1: yy = y0 * zf2: zz = z0 * zf3
  p1 = p0 + xx + yy * Sin(theta)
  If p1 > wdts - LbP.Width - gap Then
    p1 = wdts - LbP.Width - gap
    g0 = (p1 - p0) / (x0 + y0 * Sin(theta))
    If g0 <= 0 Then Exit Sub
    zf1 = g0: zf2 = g0
    yy = y0 * zf2: zz = z0 * zf3
  End If
  q1 = q0 - yy * Cos(theta) - zz
  If q1 < gap Then
    q1 = gap
    g0 = q0 - q1 - y0 * zf2 * Cos(theta)
    If g0 <= 0 Then Exit Sub
    zf3 = g0 / z0
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub projw()
  If dr0 = 0 Then
    x0 = 0: y0 = sg0: z0 = bg0
  ElseIf dr0 = 1 Then
    x0 = lth: y0 = sg0: z0 = bg0
  ElseIf dr0 = 2 Then
    x0 = sg0: y0 = 0: z0 = bg0
  ElseIf dr0 = 3 Then
    x0 = sg0: y0 = wdt: z0 = bg0
  ElseIf dr0 = 4 Then
    x0 = sg0: y0 = bg0: z0 = 0
  ElseIf dr0 = 5 Then
    x0 = sg0: y0 = bg0: z0 = hgt
    If hgt_c > 0 Then
      g0 = y0 / (wdt / 2) - 1
      z0 = hgt + hgt_c * (1 - g0 * g0)
    End If
  Else
    x0 = 0: y0 = 0: z0 = 0
  End If
  Call proj2d
End Sub

'-------------------------------
'  Begin Pre-Processor
'-------------------------------
Private Sub pre0_Click()
'Goes to menu Combustion Space-> pre1_click or Melter->pre2_click
 End Sub

'-------------------------------
'Initialize for combustion space furnace diagram
'-------------------------------
Private Sub pre1_Click()
    flow_domain = 1: chamber_type = 0
    title = "Combustion Space"
    entity_str = " furnace "
    Call setup(1)
    CD1.InitDir = dnm & "combustion"
    file11.Caption = "Crown Top"
    file12.Caption = "Box"
    'Set menus for Pre-Processor
    menu_layer = InPreProc
    ex0.Caption = "Done"
    mnuCase.Enabled = True
    mnuCase.Visible = True
    mnuCaseNew.Enabled = True
    mnuCaseNew.Visible = True
    mnuCaseOpen.Enabled = True
    mnuCaseOpen.Visible = True
    mnuCaseSave.Enabled = False
    mnuCaseSave.Visible = True
    mnuCaseSaveAs.Enabled = False
    mnuCaseSaveAs.Visible = True
    mnuCaseSaveAsFull.Enabled = False
    mnuCaseSaveAsFull.Visible = True
    mnuCaseDescript.Enabled = False
    mnuCaseDescript.Visible = True
    mnuCaseDelete.Enabled = True
    mnuCaseDelete.Visible = True
    mnuCaseDelResults.Enabled = True
    mnuCaseDelResults.Visible = True
    mnuOptRunPlot.Visible = False
End Sub

'-------------------------------
'Initialize for melter diagram
'-------------------------------
Private Sub pre2_Click()
    flow_domain = 2: title = "Melter"
    entity_str = " melter "
    Call setup(1)
    CD1.InitDir = dnm & "melt"
    file12.Caption = "Melter"
    file11.Caption = "Melter with Refiner"
    
    bgcon = 0.000000000001
    'interval_rad = 200 'radiation not yet implemented inside melt
    id_rad = -1 'default value for melt
    nphas = 2: nps0_s = 1: nps0_c = 1: nbs0 = 0
    ips0_s = 1: ips0_c = 1: ibs0 = 1
    ReDim bah(1, 2), bav(1, 2), rg_b(1)
    bah(1, 1) = 0.01: bav(1, 1) = 0.01: rg_b(1) = 0.00025
    
    udf_ds_n = 2
    ReDim udf_ds(2), udf_ds_t(2)
    udf_ds(1) = 2913.7: udf_ds(2) = 2753.7
    udf_ds_t(1) = 1000: udf_ds_t(2) = 2000
    udf_mu_n = 7
    ReDim udf_mu(7), udf_mu_t(7)
    udf_mu(1) = 11350: udf_mu(2) = 1742
    udf_mu(3) = 353: udf_mu(4) = 89: udf_mu(5) = 27
    udf_mu(6) = 9.4: udf_mu(7) = 3.7
    For n = 1 To udf_mu_n
    udf_mu_t(n) = 1000 + 100 * n: Next
    udf_k_n = 7
    ReDim udf_k(7), udf_k_t(7)
    udf_k(1) = 17.78: udf_k(2) = 25.57
    udf_k(3) = 34.28: udf_k(4) = 43.92: udf_k(5) = 54.48
    udf_k(6) = 65.96: udf_k(7) = 78.36
    For n = 1 To udf_k_n
    udf_k_t(n) = 1000 + 100 * n: Next
    udf_cl_n = 1
    ReDim udf_cl(1), udf_cl_t(1)
    udf_cl(1) = 1200: udf_cl_t(1) = 1100
    udf_a_n = 3
    ReDim udf_a(3), udf_a_m(3)
    udf_a_m(1) = 2.7: udf_a(1) = 0.312
    udf_a_m(2) = 4.4: udf_a(2) = 4.994
    udf_a_m(3) = 4.7: udf_a(3) = 24.789
    udf_clc_n = 2
    ReDim udf_clc(2), udf_clc_t(2)
    udf_clc(1) = 1000: udf_clc_t(1) = 303.15
    udf_clc(2) = 1300: udf_clc_t(2) = 1473.15
    udf_cls_n = 2
    ReDim udf_cls(2), udf_cls_t(2)
    udf_cls(1) = 1000: udf_cls_t(1) = 303.15
    udf_cls(2) = 1300: udf_cls_t(2) = 1473.15
   
    'tm_c = 1200: cl_c = 950: h0_c = 400000#
    'tm_s = 1200: cl_s = 950: h0_s = 400000#
    tm_c = 1200: h0_c = 400000#
    tm_s = 1200: h0_s = 400000#
    
    ' 10-20-2004 bug fix initial glass prop to show density table
    Dim m As Integer
    nudf = 1: iudf = 1: judf = 1
    mudf = udf_ds_n
    ReDim tx(mudf), fx(mudf)
    For m = 1 To mudf
      fx(m) = udf_ds(m): tx(m) = udf_ds_t(m)
    Next
      
    'Set menus for Pre-Processor
    menu_layer = InPreProc
    ex0.Caption = "Done"
    mnuCase.Enabled = True
    mnuCase.Visible = True
    mnuCaseNew.Enabled = True
    mnuCaseNew.Visible = True
    mnuCaseOpen.Enabled = True
    mnuCaseOpen.Visible = True
    mnuCaseSave.Enabled = False
    mnuCaseSave.Visible = True
    mnuCaseSaveAs.Enabled = False
    mnuCaseSaveAs.Visible = True
    mnuCaseSaveAsFull.Enabled = False
    mnuCaseSaveAsFull.Visible = True
    mnuCaseDescript.Enabled = False
    mnuCaseDescript.Visible = True
    mnuCaseDelete.Enabled = True
    mnuCaseDelete.Visible = True
    mnuCaseDelResults.Enabled = True
    mnuCaseDelResults.Visible = True
    mnuOptRunPlot.Visible = False
End Sub

Private Sub sim0_Click()
'Goes to menu Combustion Space-> sim1_click or Melter->sim2_click
End Sub

'-------------------------------
'  Begin combustion simulation
'-------------------------------
Private Sub sim1_Click()
  flow_domain = 1
  cycling = 0
  menu_layer = InSimulation
  userError = False ' 03-04-2005
  Call simnew 'get case, verify proper files exist, create runs.dat file
  If userError = False Then
    'proceed with simulation
    Call simcr_Click
    
    'Set menus for Simulation
    ex0.Caption = "Stop Run"
    mnuCase.Enabled = True
    mnuCase.Visible = True
    mnuCaseNew.Enabled = False
    mnuCaseNew.Visible = True
    mnuCaseOpen.Enabled = False
    mnuCaseOpen.Visible = True
    mnuCaseSave.Enabled = False
    mnuCaseSave.Visible = True
    mnuCaseSaveAs.Enabled = False
    mnuCaseSaveAs.Visible = True
    mnuCaseSaveAsFull.Enabled = False
    mnuCaseSaveAsFull.Visible = True
    mnuCaseDescript.Enabled = True
    mnuCaseDescript.Visible = True
    opt0.Visible = True
        mnuProtectGrid.Visible = False
        MnuOptCol.Visible = False
        MnuOptGui.Visible = True
        opt4.Visible = False
        optplt.Visible = False
    mnuOptRunPlot.Visible = True
    LbCase.Caption = " Case " & case_number
    LbCase.Visible = True
    LbCaseTitle.Caption = case_title
    LbCaseTitle.Visible = True
  Else 'had error
    LbCase.Visible = False
    LbCaseTitle.Visible = False
    mnuCaseDescript.Enabled = False
    menu_layer = InMain
  End If
End Sub

'-------------------------------
'  Begin melt simulation
'-------------------------------
Private Sub sim2_Click()
  flow_domain = 2
  cycling = 0
  menu_layer = InSimulation
  userError = False ' 03-04-2005
  Call simnew 'get case, verify proper files exist, create runs.dat file
  If userError = False Then
    'proceed with simulation
    Call simmr_Click
    
    'Set menus for Simulation
    ex0.Caption = "Stop Run"
    mnuCase.Enabled = True
    mnuCase.Visible = True
    mnuCaseNew.Enabled = False
    mnuCaseNew.Visible = True
    mnuCaseOpen.Enabled = False
    mnuCaseOpen.Visible = True
    mnuCaseSave.Enabled = False
    mnuCaseSave.Visible = True
    mnuCaseSaveAs.Enabled = False
    mnuCaseSaveAs.Visible = True
    mnuCaseSaveAsFull.Enabled = False
    mnuCaseSaveAsFull.Visible = True
    mnuCaseDescript.Enabled = True
    mnuCaseDescript.Visible = True
    opt0.Visible = True
        mnuProtectGrid.Visible = False
        MnuOptCol.Visible = False
        MnuOptGui.Visible = True
        opt4.Visible = False
        optplt.Visible = False
    mnuOptRunPlot.Visible = True
    LbCase.Caption = " Case " & case_number
    LbCase.Visible = True
    LbCaseTitle.Caption = case_title
    LbCaseTitle.Visible = True
  Else 'had error
    LbCase.Visible = False
    LbCaseTitle.Visible = False
    mnuCaseDescript.Enabled = False
    menu_layer = InMain
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub vector()
  Dim p As Integer, q As Integer
  Dim i As Integer, i1 As Integer, i2 As Integer
  Dim j As Integer, j1 As Integer, j2 As Integer
  Dim t1 As Single, t2 As Single
  ipc = ForeColor: ForeColor = vbRed
  Line (p1, q1)-(p2, q2)
  p = p2 - p1: q = q2 - q1
  g0 = p ^ 2 + q ^ 2
  If g0 < 10000 Then GoTo vec0
  If Abs(p) < 1 And q > 1 Then
    g0 = -pi / 2
  ElseIf Abs(p) < 1 And q < -1 Then
    g0 = pi / 2
  Else
    g0 = Atn(q / p)
  End If
  t1 = g0 - 0.4: t2 = g0 + 0.4
  p = p2: q = q2
  GoSub vec1
  t1 = t1 + pi: t2 = t2 + pi
  p = p1: q = q1
  GoSub vec1

vec0:
  ForeColor = ipc
  Exit Sub

vec1:
  i1 = p + 150 * Cos(t1): j1 = q + 150 * Sin(t1)
  i2 = p + 150 * Cos(t2): j2 = q + 150 * Sin(t2)
  g2 = 0
  For n = 0 To 10
    g1 = 1 - g2
    i = g1 * i1 + g2 * i2
    j = g1 * j1 + g2 * j2
    Line (p, q)-(i, j)
    g2 = g2 + 0.1
  Next
  Return
End Sub

'-------------------------------
'Construct geometry lists
'-------------------------------
Private Sub geom_ls()
  Dim a1 As Single, a2 As Single, a3 As Single
  List1.List(0) = "Geometry"
  If chamber_type = 2 Then GoTo gls2
  Call unit_l(lth, s0)
  List1.List(1) = "length=" & s0
  Call unit_l(wdt, s0)
  List1.List(2) = "width=" & s0
  Call unit_l(hgt, s0)
  If flow_domain = 2 Then GoTo gls1
  List1.List(3) = "height=" & s0
  'If InStr(Caption, "crown") > 0 Then
  If hgt_c > 0 Then ' 10-21-2004
    Call unit_l(hgt_c, s0)
    List1.List(4) = "crown=" & s0
  Else
    List1.List(4) = ""
  End If
  Exit Sub

gls1:
  List1.List(3) = "depth=" & s0
  List1.List(4) = ""
  Exit Sub

gls2:
  List1.List(1) = "melter"
  List1.List(2) = "throat"
  List1.List(3) = "refiner"
  List1.List(melter_component) = List1.List(melter_component) & "-->"
  List2.List(0) = "dimensions:"
  lth = lth_r(melter_component): wdt = wdt_r(melter_component): hgt = hgt_r(melter_component)
  Call unit_l(lth, s0)
  List2.List(1) = "length=" & s0
  Call unit_l(wdt, s0)
  List2.List(2) = "width=" & s0
  Call unit_l(hgt, s0)
  List2.List(3) = "depth=" & s0
  If melter_component <> 2 And melter_component <> 3 Then List3.Visible = False: Exit Sub
  List3.Visible = True
  List3.List(0) = "gaps:"
  Call unit_l(gx_r(melter_component), s0)
  List3.List(1) = "length=" & s0
  Call unit_l(gy_r(melter_component), s0)
  List3.List(2) = "width=" & s0
  Call unit_l(gz_r(melter_component), s0)
  List3.List(3) = "depth=" & s0
End Sub

'-------------------------------
'-------------------------------
Private Sub g0_ls(i1, n0)
  List1.List(1) = "#" & i1 & " / " & n0
  If gridProtectMode Then
      List1.List(3) = ""
      List1.List(4) = ""
  Else
      List1.List(3) = "add"
      If n0 > 1 Then
         List1.List(4) = "delete"
      Else
         List1.List(4) = ""
      End If
  End If
End Sub

'-------------------------------
'Set List 2 item 0 to wall orientation based on argument
'-------------------------------
Private Sub g1_ls(n0)
  If n0 = 0 Then
    s0 = "back wall"
  ElseIf n0 = 1 Then
    s0 = "front wall"
  ElseIf n0 = 2 Then
    s0 = "right wall"
  ElseIf n0 = 3 Then
    s0 = "left wall"
  ElseIf n0 = 4 Then
    s0 = "bottom wall"
  ElseIf n0 = 5 Then
    s0 = "top wall"
  Else
    s0 = ""
  End If
  List2.List(0) = s0
End Sub

'-------------------------------
'Construct bubbler lists
'-------------------------------
Private Sub bub_ls()
  Dim j As Integer
  If nsw > 0 Then
    Call ct_ls(5, 5, 5, 0)
  Else
    Call ct_ls(5, 0, 0, 0)
  End If
  List1.List(0) = "Bubbler"
  j = isw: Call g0_ls(j, nsw)
  List1.List(2) = "type: rectangle"
  If Not gridProtectMode Then List1.List(3) = "add"
  If nsw < 1 Then Exit Sub
  If Not gridProtectMode Then List1.List(4) = "del"
  Call g1_ls(wdr(j))
  Call unit_l(wsg(j), s0)
  If wdr(j) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  Call unit_l(wbg(j), s0)
  If wdr(j) <= 3 Then
    List2.List(2) = "bottom gap=" & s0
  Else
    List2.List(2) = "side gap=" & s0
  End If
  Call unit_l(wwd(j), s0)
  List2.List(3) = "dia.=" & s0
  Call unit_l(wdp(j), s0)
  List2.List(4) = "height=" & s0
  s0 = "K": g0 = wtg(j): Call ucv
  s0 = Format(g0, "0") & s0
  List3.List(0) = "gas temp= " & s0
  g0 = wfa(j)
  s0 = "m^3/s": Call ucv
  If s0 = "m^3/s" Then
    's1 = Format(g0, "0.###")
    s1 = Format(g0, "Scientific") ' 10-2004 make small numbers visible in user interface
  Else
    s1 = Format(g0, "0.###")
  End If
  List3.List(1) = "air= " & s1 & " " & s0
End Sub

'-------------------------------
'Construct glass property list
'-------------------------------
Private Sub udf_ls()
  Dim s1 As String, s2 As String
  Call ct_ls(5, 5, 5, 0)
  List1.List(0) = "Glass Properties"
  
 'determine glass property function to display
  If nudf = 1 Then
    s1 = "Density": s2 = "kg/m^3"
  ElseIf nudf = 2 Then
    s1 = "Viscosity": s2 = "Pa-s"
  ElseIf nudf = 3 Then
    s1 = "Conductivity": s2 = "W/m/K"
  ElseIf nudf = 4 Then
    s1 = "Liqu Spe Heat": s2 = "J/kg/K"
  ElseIf nudf = 5 Then
    s1 = "Vol Absorp": s2 = "1/cm"
  ElseIf nudf = 6 Then
    s1 = "Cull Spe Heat": s2 = "J/kg/K"
  ElseIf nudf = 7 Then
    s1 = "Batch Spe Heat": s2 = "J/kg/K"
  End If
  s0 = s2: g0 = 1: Call ucv 'convert to display units if needed
  s1 = s1 & " (" & s0 & ")"
  
  'seems to reorder entries in case of an add or delete  ?
  For n = 1 To mudf - 1
  For m = n + 1 To mudf
    If tx(m) < tx(n) Then
      g0 = tx(n): tx(n) = tx(m): tx(m) = g0
      g0 = fx(n): fx(n) = fx(m): fx(m) = g0
    ElseIf tx(m) = tx(n) Then
      For m1 = m To mudf - 1
        tx(m1) = tx(m1 + 1): fx(m1) = fx(m1 + 1)
      Next: mudf = mudf - 1
    End If
  Next: Next
  
  If iudf > mudf Then iudf = 1
  If iudf < 4 Then judf = 1 Else judf = iudf - 2
  If iudf > mudf - 2 Then judf = mudf - 4
  If judf < 1 Then judf = 1
  n2 = mudf
  If n2 > judf + 4 Then n2 = judf + 4
  If n2 > mudf Then n2 = mudf
  
  List1.List(1) = s1
  List1.List(2) = "#" & iudf & "/" & mudf
  'If Not gridProtectMode Then List1.List(3) = "add"
  'If Not gridProtectMode Then List1.List(4) = "delete"
  List1.List(3) = "add"
  List1.List(4) = "delete"
  
  'Build list 2 & 3
  For n = judf To n2
    n1 = n - judf: g0 = tx(n)
    If nudf = 5 Then
      s0 = "um"
      s1 = "WL(" & n & ") = " & Format(g0, "0.##")
    Else
      s0 = "K": Call ucv
      s1 = "T(" & n & ") = " & Format(g0, "0")
    End If
    If n1 = 0 Then s1 = s1 & " " & s0
    List3.List(n1) = s1
    s0 = s2: g0 = fx(n): Call ucv
    s1 = "F(" & n & ") = "
    If g0 > 1000 Then
      s1 = s1 & Format(g0, "0")
    ElseIf g0 > 100 Then
      s1 = s1 & Format(g0, "0.0")
    Else
      s1 = s1 & Format(g0, "0.00")
    End If
    If n = iudf Then s1 = s1 & "*"
    List2.List(n1) = s1
  Next
  
  'Update function entry storage
  If nudf = 1 Then
    udf_ds_n = mudf
    ReDim udf_ds(mudf), udf_ds_t(mudf)
    For m = 1 To mudf
      udf_ds(m) = fx(m): udf_ds_t(m) = tx(m)
    Next
  ElseIf nudf = 2 Then
    udf_mu_n = mudf
    ReDim udf_mu(mudf), udf_mu_t(mudf)
    For m = 1 To mudf
      udf_mu(m) = fx(m): udf_mu_t(m) = tx(m)
    Next
  ElseIf nudf = 3 Then
    udf_k_n = mudf
    ReDim udf_k(mudf), udf_k_t(mudf)
    For m = 1 To mudf
      udf_k(m) = fx(m): udf_k_t(m) = tx(m)
    Next
  ElseIf nudf = 4 Then
    udf_cl_n = mudf
    ReDim udf_cl(mudf), udf_cl_t(mudf)
    For m = 1 To mudf
      udf_cl(m) = fx(m): udf_cl_t(m) = tx(m)
    Next
  ElseIf nudf = 5 Then
    udf_a_n = mudf
    ReDim udf_a(mudf), udf_a_m(mudf)
    For m = 1 To mudf
      udf_a(m) = fx(m): udf_a_m(m) = tx(m)
    Next
  ElseIf nudf = 6 Then 'batch specific heat
    udf_clc_n = mudf
    ReDim udf_clc(mudf), udf_clc_t(mudf)
    For m = 1 To mudf
      udf_clc(m) = fx(m): udf_clc_t(m) = tx(m)
    Next
  ElseIf nudf = 7 Then 'sand specific heat
    udf_cls_n = mudf
    ReDim udf_cls(mudf), udf_cls_t(mudf)
    For m = 1 To mudf
      udf_cls(m) = fx(m): udf_cls_t(m) = tx(m)
    Next
  End If
End Sub

'-------------------------------
'Construct wall property list
'-------------------------------
Private Sub wa_ls()
  If nwal > 0 Then
    Call ct_ls(5, 5, 5, 0)
  Else
    Call ct_ls(5, 0, 0, 0)
  End If
  If iwal <= 0 Or iwal > nwal Then iwal = 1
  List1.List(0) = "Wall Properties"
  List1.List(1) = "#" & iwal & "/" & nwal
  'If Not gridProtectMode Then List1.List(2) = "add"  'disabled multiple walls
  List1.List(2) = ""
  Call unit_l(wa_d(iwal), s0)
  List3.List(0) = "thickness=" & s0
  g0 = wa_k(iwal): s0 = "W/m/K": Call ucv
  List3.List(1) = "cond=" & g0 & " " & s0
  g0 = wa_h(iwal): s0 = "W/m^2/K": Call ucv
  List3.List(2) = "h_ext=" & g0 & " " & s0
  g0 = wa_ta(iwal): s0 = "K": Call ucv
  List3.List(3) = "ambient temp=" & g0 & " " & s0
  s0 = Format(wa_e(iwal), "0.##")
  List3.List(4) = "wall emis= " & s0
  If iwal <= 1 Then List2.List(0) = "general": Exit Sub
  
  If Not gridProtectMode Then List1.List(3) = "del"
  Call g1_ls(wa_dr(iwal))
  Call unit_l(wa_sg(iwal), s0)
  If wa_dr(iwal) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  Call unit_l(wa_bg(iwal), s0)
  If wa_dr(iwal) <= 3 Then
    List2.List(2) = "bottom gap=" & s0
  Else
    List2.List(2) = "side gap=" & s0
  End If
  Call unit_l(wa_wd(iwal), s0)
  List2.List(3) = "width=" & s0
  Call unit_l(wa_ht(iwal), s0)
  List2.List(4) = "height=" & s0
End Sub

'-------------------------------
'Construct electronic booster lists
'-------------------------------
Private Sub eb_ls()
  If neb > 0 Then
    Call ct_ls(5, 5, 5, 0)
  Else
    Call ct_ls(5, 0, 0, 0)
  End If
  If ieb <= 0 Or ieb > neb Then ieb = 1
  If jeb <= 0 Then jeb = 1
  List1.List(0) = "EBooster"
  If neb > 0 Then
    List1.List(1) = "Phase: " & ebty(ieb)
  Else
    If ebty0 < 1 Or ebty0 > 3 Then ebty0 = 1
    List1.List(1) = "Phase: " & ebty0
  End If
  If Not gridProtectMode Then List1.List(2) = "add"
  If neb < 1 Then Exit Sub
  If Not gridProtectMode Then List1.List(3) = "del"
  List1.List(4) = "#" & ieb & "/" & neb
  If ebty(ieb) = 1 Then
    If jeb > 2 Then jeb = 1
    If jeb = 1 Then s0 = "1P" Else s0 = "1N"
  ElseIf ebty(ieb) = 2 Then
    If jeb > 2 Then jeb = 1
    If jeb = 1 Then s0 = "2N" Else s0 = "2S"
  Else
    If jeb > 3 Then jeb = 1
    If jeb = 1 Then s0 = "3R"
    If jeb = 2 Then s0 = "3S"
    If jeb = 3 Then s0 = "3T"
  End If
  List3.List(0) = "type: " & s0
  Call g1_ls(ebdr(ieb, jeb)) 'Set List 2 item 0 to wall orientation based on argument
  Call unit_l(ebsg(ieb, jeb), s0)
  If ebdr(ieb, jeb) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  Call unit_l(ebbg(ieb, jeb), s0)
  If ebdr(ieb, jeb) <= 3 Then
    List2.List(2) = "bottom gap=" & s0
  Else
    List2.List(2) = "side gap=" & s0
  End If
  Call unit_l(ebwd(ieb, jeb), s0)
  List2.List(3) = "dia=" & s0
  Call unit_l(ebht(ieb, jeb), s0)
  List2.List(4) = "height=" & s0
  If ebty(ieb) = 1 And jeb = 2 Then
    List3.List(1) = "volt= 0 V"
  Else
    List3.List(1) = "volt=" & ebvt(ieb) & " V"
  End If
  List3.List(2) = "power=" & ebpw(ieb) & " W"
  List3.List(3) = "Hloss=" & ebhl(ieb) & " W"
End Sub

'-------------------------------
'Construct charger list
'-------------------------------
Private Sub char_ls()
  Dim j As Integer
  List1.List(0) = "Batch Charger"
  j = ibr: Call g0_ls(j, nbr)
  List1.List(2) = "type: rectangle"
  Call g1_ls(bdr(j))
  Call unit_l(bsg(j, 1), s0)
  If bdr(j) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  Call unit_l(bbg(j, 1), s0)
  If bdr(j) <= 3 Then
    List2.List(2) = "bottom gap=" & s0
  Else
    List2.List(2) = "side gap=" & s0
  End If
  Call unit_l(bwd(j, 1), s0)
  List2.List(3) = "width=" & s0
  Call unit_l(bht(j, 1), s0)
  List2.List(4) = "height=" & s0
  
  s0 = "K": g0 = btg(j, 1): Call ucv
  s0 = Format(g0, "0") & s0
  List3.List(0) = "batch temp= " & s0
  s0 = "kg/s": g0 = bfg(j, 1): Call ucv: GoSub chls1
  List3.List(1) = "charge= " & s0
  List3.List(2) = "cullet ratio= " & bfg(j, 2)
  's0 = "m/s": Call ucv
  g0 = batch_velocity(j):
  s0 = "m/s": Call ucv
  s0 = Format(g0, "0.#####") & s0
  List3.List(3) = "batch vel=" & s0
  s0 = "J/kg"
  If InStr(List4.List(0), "cul") > 0 Then g0 = h0_c _
  Else g0 = h0_s
  Call ucv: GoSub chls1
  List3.List(4) = "h_f = " & s0
  
  If bfg(j, 2) <= 0 Then List4.List(0) = "batch particle"
  s0 = List4.List(0)
  If InStr(s0, "cul") Then
    'r0 = bah(j, 1): tm_0 = tm_c: ds_0 = ds_c: cl_0 = cl_c
    r0 = bah(j, 1): tm_0 = tm_c: ds_0 = ds_c
  Else
    List4.List(0) = "batch particle"
    'r0 = bav(j, 1): tm_0 = tm_s: ds_0 = ds_s: cl_0 = cl_s
    r0 = bav(j, 1): tm_0 = tm_s: ds_0 = ds_s
  End If
  List4.List(1) = "ave size= " & r0 & "um"
  s0 = "K": g0 = tm_0: Call ucv: GoSub chls1
  List4.List(2) = "melt= " & s0
  s0 = "kg/m^3": g0 = ds_0: Call ucv: GoSub chls1
  List4.List(3) = "density= " & s0
  's0 = "J/kg/K": g0 = cl_0: Call ucv: GoSub chls1
  'List4.List(4) = "sp heat= " & s0
  List4.List(4) = ""
  Exit Sub
chls1:
  If g0 >= 10000# Then
    s0 = Format(g0, "0.####E") & s0
  ElseIf g0 >= 1000 Then
    s0 = Format(g0, "0") & s0
  ElseIf g0 >= 100 Then
    s0 = Format(g0, "0.#") & s0
  ElseIf g0 >= 10 Then
    s0 = Format(g0, "0.##") & s0
  ElseIf g0 >= 1 Then
    s0 = Format(g0, "0.###") & s0
  ElseIf g0 >= 0.1 Then
    s0 = Format(g0, "0.####") & s0
  Else
    s0 = Format(g0, "0.####E-") & s0
  End If
  Return
End Sub

'-------------------------------
'Construct burner green list boxes
'-------------------------------
Private Sub burn_ls()
  Dim j As Integer, n As Integer
  Dim inorout As Integer '1=outer section of tube-in-tube burner, 2=inner section
  need_burner_check = True
  List1.List(0) = "Burner"
  j = ibr 'j is the current burner number
  'Tube-in-tube burners have inner=2 and outer=1 sets of info, others just have outer set
  If bty(j) = 4 And InStr(List3.List(4), "inner") > 0 _
      Then inorout = 2 Else inorout = 1
  Call g0_ls(j, nbr) 'fill in list 1, determine if can add/delete
  If bty(j) = 0 Then
    List1.List(2) = "type: round"
    Call unit_l(bwd(j, 1), s0)
    List2.List(3) = "diameter=" & s0
    List2.List(4) = ""
    List3.List(4) = ""
  ElseIf bty(j) = 1 Then
    List1.List(2) = "type: ring"
    Call unit_l(bwd(j, 1), s0)
    List2.List(3) = "outer diam=" & s0
    g0 = bwd(j, 1) / 2
    If bht(j, 1) > g0 Then bht(j, 1) = g0
    Call unit_l(bht(j, 1), s0)
    List2.List(4) = "inner diam=" & s0
    If List3.List(4) = "" Then _
      List3.List(4) = "outer flow rates ->"
  ElseIf bty(j) = 2 Then
    List1.List(2) = "type: rectangle"
    Call unit_l(bwd(j, 1), s0)
    List2.List(3) = "width=" & s0
    Call unit_l(bht(j, 1), s0)
    List2.List(4) = "height=" & s0
    List3.List(4) = ""
  ElseIf bty(j) = 3 Then
    List1.List(2) = "type: pipe-in-pipe"
    Call unit_l(bwd(j, 1), s0)
    List2.List(3) = "width=" & s0
    Call unit_l(bht(j, 1), s0)
    List2.List(4) = "height=" & s0
    If List3.List(4) = "" Then _
      List3.List(4) = "outer flow rates ->"
      
  Else 'New burner type 4   10-19-2004
    List1.List(2) = "type: Tube-in-tube"  ' June07
    If inorout = 1 Then List3.List(4) = "<- outer section ->"
    'List1.List(2) = "type: Regenerative"
    
    'If inorout = 2 Then
    '  'have inner section, calculate centered inner values if they are not set  June07
    '  If bwd(j, 2) = 0 And bht(j, 2) = 0 And bsg(j, 2) = 0 And bbg(j, 2) = 0 Then
    '    bwd(j, 2) = bwd(j, 1) / 2#
    '    bht(j, 2) = bht(j, 1) / 2#
    '    bsg(j, 2) = bsg(j, 1) + (bwd(j, 2) / 2#)
    '    bbg(j, 2) = bbg(j, 1) + (bht(j, 2) / 2#)
    '  End If
      ''s_inner = s + w/2 - w_inner/2
      'bsg(j, 2) = bsg(j, 1) + bwd(j, 1) / 2# - bwd(j, 2) / 2#
    'Else
    '  List3.List(4) = "<- outer section ->"
    'End If
       
    Call unit_l(bwd(j, inorout), s0) 'format unit string
    List2.List(3) = "width=" & s0
    Call unit_l(bht(j, inorout), s0)
    List2.List(4) = "height=" & s0
  End If
  
  Call g1_ls(bdr(j)) 'write orientation
  'GoSub brls2  June07
  Call unit_l(bsg(j, inorout), s0)
  If bdr(j) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  'GoSub brls3   June07
  Call unit_l(bbg(j, inorout), s0)
  If bdr(j) <= 3 Then
    List2.List(2) = "bottom gap=" & s0
  Else
    List2.List(2) = "side gap=" & s0
  End If
  'make addition for tube-in-tube type   10-19-2004
  s0 = "K": g0 = btg(j, inorout): Call ucv
  s0 = Format(g0, "0") & s0
  List3.List(0) = "flow temp= " & s0
  s0 = Format(bav(j, inorout), "0") & " deg"
  List3.List(1) = "ver inj angle= " & s0
  s0 = Format(bah(j, inorout), "0") & " deg"
  List3.List(2) = "hor inj angle= " & s0
  's0 = Format(bas(j), "0") & "deg"
'  List3.List(3) = "spray angle= " & s0

  g0 = bfg(j, inorout): GoSub brls1   'convert to display units and set s1 to be formatted g0
  List4.List(1) = "NGas= " & s1
  g0 = bfa(j, inorout): GoSub brls1
  List4.List(2) = "air= " & s1
  g0 = bfo(j, inorout): GoSub brls1
  List4.List(3) = "oxygen=" & s1
  g0 = bfn(j, inorout): GoSub brls1
  List4.List(4) = "nitrogen=" & s1
  List4.List(0) = "flow rates (" & s0 & "):"
  
  'forget about o/f stuff  June07
  'g_f = 0: g_ox = 0
  'For jj = 1 To nbr
  '  g0 = bfg(jj, 1)
  '  g1 = bfa(jj, 1) * 0.21 + bfo(jj, 1)
  '  'determine how regenerative burners should be included here ???
  '  If bty(j) = 1 Or bty(j) = 3 Or bty(j) = 4 Then
  '    g0 = g0 + bfg(jj, 2)
  '    g1 = g1 + bfa(jj, 2) * 0.21 + bfo(jj, 2)
  '  End If
  '  g_f = g_f + g0: g_ox = g_ox + g1
  '  If jj = j Then GoSub brls4: s1 = s0
  'Next
  'If nbr > 1 Then
  '  g0 = g_f: g1 = g_ox: GoSub brls4
  '  s0 = s1 & ", tot:" & s0
  'End If
  'List3.List(3) = "O/F= " & s0
  List3.List(3) = ""
  Exit Sub
  
  
brls1:
  'convert to display units if needed and set s1 to be formatted g0
  If feed_unit = "kg/s" Then s0 = "kg/s" Else s0 = "m^3/s"
  Call ucv
  If s0 = "m^3/s" Or s0 = "kg/s" Then
    s1 = Format(g0, "0.####")
  Else
    s1 = Format(g0, "0")
  End If
  Return
  
brls2:
  'modify side gap if needed?
  If bty(j) <= 1 Then r0 = bwd(j, inorout) / 2 Else r0 = 0
        'if round/ring burner then r0=burner_width/2 else r0=0
  If bdr(j) <= 1 Then g0 = wdt - r0 Else g0 = lth - r0
        'if on back or front wall g0=width-radius else g0=length-radius
  If bsg(j, inorout) > g0 Then bsg(j, inorout) = g0
        'If the side gap > available space  then set it to available space
  If bsg(j, inorout) < r0 Then bsg(j, inorout) = r0
        'If the side gap < radius  then set it to radius
  Return
  
brls3:
  'modify bottom gap if needed?
  If bty(j) <= 1 Then r0 = bwd(j, inorout) / 2 Else r0 = 0
        'if round/ring burner then r0=burner_width/2 else r0=0
  If bdr(j) <= 3 Then g0 = hgt - r0 Else g0 = wdt - r0
        'if on back, front, right, or left wall g0=height-radius else g0=width-radius
  If bbg(j, inorout) > g0 Then bbg(j, inorout) = g0
        'If the bottom gap > available space  then set it to available space
  If bbg(j, inorout) < r0 Then bbg(j, inorout) = r0
        'If the bottom gap < radius  then set it to radius
  Return
  
brls4:
  If g0 <= 0 Then
    s0 = "***"
  Else
    g0 = g1 / g0
    s0 = Format(g0, "0.##")
  End If
  Return
End Sub

'-------------------------------
'Construct exhaust or outlet lists
'-------------------------------
Private Sub exh_ls()
  Dim j As Integer
  j = iex
  If flow_domain = 1 Then s0 = "Exhaust" Else s0 = "Outlet"
  List1.List(0) = s0
  Call g0_ls(j, nex)
  If ety(j) = 1 Then
    List1.List(2) = "type: round"
  Else
    List1.List(2) = "type: rectangle"
  End If
  Call g1_ls(edr(j))
  GoSub exls1: Call unit_l(esg(j), s0)
  If edr(j) <= 1 Then
    List2.List(1) = "side gap=" & s0
  Else
    List2.List(1) = "end gap=" & s0
  End If
  GoSub exls2: Call unit_l(ebg(j), s0)
  If edr(j) <= 3 Then
    List2.List(2) = "bottom gap=" & s0
  Else
    List2.List(2) = "side gap=" & s0
  End If
  GoSub exls3: Call unit_l(ewd(j), s0)
  List2.List(3) = "width=" & s0
  GoSub exls4: Call unit_l(eht(j), s0)
  List2.List(4) = "height=" & s0
  If flow_domain = 2 Then
    List3.List(0) = "pull= " & epull(j)
    List3.List(1) = "cells in exit= " & extra_tunnel_cells
    List3.List(2) = "tunnel length= " & extra_tunnel_length
  Else
    List3.List(0) = "Init. wall temp. type:"
    If exh_wall_temp_type(j) = 0 Then
        List3.List(1) = "fraction avg. wall"
        List3.List(2) = "   " & exh_wall_temp_frac(j)
    Else
        List3.List(1) = "user set value"
        s0 = "K"
        g0 = exh_wall_temp_fixed(j): Call ucv
        List3.List(2) = "   " & g0 & " (" & s0 & ")"
    End If
  End If
  List3.List(3) = "cells across exits= " & n_cells_across_exit
  Exit Sub


exls1:
  r0 = ewd(j)
  If edr(j) <= 1 Then g0 = wdt - r0 Else g0 = lth - r0
  If esg(j) > g0 Then esg(j) = g0
  If esg(j) < 0 Then esg(j) = 0
  Return
exls2:
  r0 = eht(j): hgt0 = hgt + hgt_c
  If edr(j) <= 3 Then g0 = hgt0 - r0 Else g0 = wdt - r0
  If ebg(j) > g0 Then ebg(j) = g0
  If ebg(j) < 0 Then ebg(j) = 0
  Return
exls3:
  r0 = esg(j)
  If edr(j) <= 1 Then g0 = wdt - r0 Else g0 = lth - r0
  If ewd(j) > g0 Then ewd(j) = g0
  Return
exls4:
  r0 = ebg(j): hgt0 = hgt + hgt_c
  If edr(j) <= 3 Then g0 = hgt0 - r0 Else g0 = wdt - r0
  If eht(j) > g0 Then eht(j) = g0
  Return
End Sub

'-------------------------------
'Construct simulation parameter lists
'-------------------------------
Private Sub sim_ls()
  List1.List(0) = "Parameters" 'for simulation setup
  If irstyp = 0 Then s0 = "new " Else s0 = "re"
  List1.List(1) = s0 & "start"
  List1.List(2) = "iterations=" & maxgi
  List1.List(3) = "convergence=" & bgcon
  If flow_domain = 2 Then GoTo simls2 'jump to continue with melt parameters
  
  'continue with combustion parameters
  'If id_rad = 0 Then
  '  s0 = "calc rad emis"
  If id_rad = 1 Then
    s0 = "calc radiation"
  Else 'id_rad=-1
    s0 = "no radiation calc"
  End If
  List1.List(4) = s0
  'list2
  List2.List(0) = "Flow Properties"
  s0 = "Pa": g0 = pg0: Call ucv
  List2.List(1) = "pres= " & g0 & " " & s0
  If feed_unit = "kg/s" Then
    s0 = "J/kg"
  Else
    s0 = "J/m^3"
  End If
  g0 = qh0: Call ucv
  List2.List(2) = "heat of combustion="
  List2.List(3) = g0 & " " & s0
  If oxy_fuel = 1 Then
    List2.List(4) = "Using oxy fuel"
  Else
    List2.List(4) = "Not using oxy fuel"
  End If
  'list3
  If id_rad < 0 Then
    For i = 0 To 3: List3.List(i) = "": Next
  Else
    List3.List(0) = "Radiation Prop:"
    List3.List(1) = "total # of wl's= " & nwl
    List3.List(2) = "wavelength #:" & iwl
    List3.List(3) = "wl= " & Format(wl(iwl), "0.##") & " um"
  End If
  If id_rad = 1 Or ms = 1 Then
    List3.List(4) = "interval=" & interval_rad
  Else
    List3.List(4) = ""
    interval_rad = 0
  End If
  'list4
  If surf_type = 0 Then
    List4.List(0) = "melt surf: specified"
    s0 = "K": g0 = surf_temp: Call ucv
    List4.List(1) = "surf temp: " & g0 & " " & s0
  Else
    List4.List(0) = "melt surf: calculated"
    List4.List(1) = ""
  End If
  If irstyp = 0 Then
    s0 = "K": g0 = start_temp: Call ucv
    List4.List(2) = "start temp: " & g0 & " " & s0
    List4.List(3) = "initial iterations: " & initial_gitr
  Else
    'block out start_temp and initial_gitr if doing restart
    List4.List(2) = ""
    List4.List(3) = "": initial_gitr = 0
  End If
  If ms = 1 Then
    List4.List(4) = "subspecies iter: " & maxms
  Else
    List4.List(4) = "subspecies iter: 0"
  End If
  Exit Sub
  
simls2: 'continue with melt parameters
  'If nphas = 2 Then s0 = "w/o" Else s0 = "w" 'these 2 lines probably should be deleted ???
  'List1.List(4) = "phases: " & s0 & " bubble" 'and list1.list(4) should be blanked
  
  If irstyp = 0 Then
    s0 = "K": g0 = start_temp: Call ucv
    List1.List(4) = "start temp: " & g0 & " " & s0
  Else
    'block out start_temp if doing restart
    List1.List(4) = ""
  End If
   
  s0 = List2.List(0)
  If InStr(s0, "cullet") > 0 Then GoSub simls2b _
  Else GoSub simls2a
  
  s0 = "Pa": g0 = pg0: Call ucv
  List3.List(0) = "pressure: " & g0 & s0
  If heat_flux_type = 1 Then
    List3.List(1) = "heat flux: scaled"
    List3.List(2) = "  uniform value: "
    s0 = " MW": g0 = qsh: Call ucv: g0 = Format(g0, "0.#####")
    List3.List(3) = "  " & g0 & s0
  ElseIf heat_flux_type = 2 Then
    List3.List(1) = "heat flux: scaled"
    List3.List(2) = "  calc. in combustion"
    List3.List(3) = ""
  ElseIf heat_flux_type = 3 Then
    List3.List(1) = "heat flux: fixed"
    List3.List(2) = "  uniform value: "
    s0 = " MW": g0 = qsh: Call ucv: g0 = Format(g0, "0.#####")
    List3.List(3) = "  " & g0 & s0
  Else 'heat_flux_type=4
    List3.List(1) = "heat flux: fixed"
    List3.List(2) = "  calc. in combustion"
    List3.List(3) = ""
  End If
  Exit Sub
  
simls2a:
  List2.List(0) = "particle: batch"
  List2.List(1) = "# of groups: " & nps0_s
  List2.List(2) = "group #: " & ips0_s
  s0 = "um": g0 = bav(ips0_s, 1)
'  If opt11.Checked Then g0 = g0 / 0.0254: s0 = """"
  List2.List(3) = "radius: " & Format(g0, "0.####") & s0
  s0 = "K": g0 = tm_s: Call ucv
  List2.List(4) = "melt temp: " & Format(g0, "0") & s0
  s0 = "m": g0 = condLength_s: Call ucv
  List3.List(4) = "cond length=" & Format(g0, "0.####") & s0
  Return
  
simls2b:
  List2.List(0) = "particle: cullet"
  List2.List(1) = "# of groups: " & nps0_c
  List2.List(2) = "group #: " & ips0_c
  s0 = "um": g0 = bah(ips0_c, 1)
'  If opt11.Checked Then g0 = g0 / 0.0254: s0 = """"
  List2.List(3) = "radius: " & Format(g0, "0.####") & s0
  s0 = "K": g0 = tm_c: Call ucv
  List2.List(4) = "melt temp: " & Format(g0, "0") & s0
  s0 = "m": g0 = condLength_c: Call ucv
  List3.List(4) = "cond length=" & Format(g0, "0.####") & s0
  Return
End Sub

'-------------------------------
'-------------------------------
Private Sub unit_l(g1 As Single, s1 As String)
  Dim i1 As Integer
  If unt = "SI" Then
    s1 = Format(g1, " 0.####") & " m" ' 10-7-2004 change ## to ####
  Else
    If g1 < 0 Then g2 = -g1 Else g2 = g1
    g2 = g2 / 0.0254
    i1 = Int(g2 / 12 + 0.00001)
    If g1 < 0 Then s1 = " -" Else s1 = " "
    If i1 <> 0 Then s1 = s1 & Format(i1, "0") & "'"
    g2 = g2 - i1 * 12
    If g2 > 0.01 Then s1 = s1 & Format(g2, "0.####") & """" ' 10-7-2004 change 0.1 to 0.01
    If s1 = " " Then s1 = "0"
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub val_in(n_p As Integer)
  Dim g1 As Single
  Dim s1 As String, s2 As String, s3 As String
  If n_p = 2 Or unt = "SI" Then
    If n_p = 2 Then s1 = s0 & " (um):" _
       Else s1 = s0 & " (m):"
    s2 = InputBox(s1, ttl, g0, 3000, 6000)
    If s2 = "" Then Exit Sub
    If Not IsNumeric(s2) Then GoTo val01
    GoTo vn1
  Else
    s1 = s0 & " (ft or ' ""):"
    If g0 < 0 Then g1 = -g0 Else g1 = g0
    Call unit_l(g1, s3)
    If g0 < 0 Then s3 = "-" & s3
    s2 = InputBox(s1, ttl, s3, 3000, 6000)
    If s2 = "" Then Exit Sub
    If IsNumeric(s2) Then s2 = s2 * 0.3048: GoTo vn1
    If Left(s2, 1) = "-" Then
      g0 = -1: n = Len(s2): s2 = Mid(s2, 2, n)
    End If
    i1 = InStr(s2, "'")
    If i1 > 1 Then
      s1 = Left(s2, i1 - 1)
      If Not IsNumeric(s1) Then GoTo val01
      g1 = s1
      s1 = Mid(s2, i1 + 1)
    Else
      g1 = 0: s1 = s2
    End If
    i1 = InStr(s1, """")
    If i1 > 0 Then s1 = Left(s1, i1 - 1)
    If s1 = "" Then s1 = 0
    If Not IsNumeric(s1) Then GoTo val01
    g1 = g1 + s1 / 12
    g1 = g1 * 0.3048
    If n_p > 0 Then
      If g1 <= 0 Then GoTo val01
    ElseIf n_p = 0 Then
      If g1 < 0 Then GoTo val01
    End If
    If g0 = -1 Then g0 = -g1 Else g0 = g1
  End If
  Exit Sub
val01:
  s2 = "Invalid input (" & s2 & "), retry?"
  If MsgBox(s2, vbYesNo, ttl) = vbYes Then Call val_in(n_p)
  Exit Sub
vn1:
  If Not IsNumeric(s2) Then GoTo val01
  If n_p > 0 Then
    If s2 <= 0 Then GoTo val01
  ElseIf n_p = 0 Then
    If s2 < 0 Then GoTo val01
  End If
  g0 = s2
End Sub

'-------------------------------
'-------------------------------
Private Sub val_in2()
  'Changes input value if it is a non-zero positive number
  s2 = InputBox(s0, title, g0, 3000, 6000)
  If s2 = "" Then Exit Sub
  If IsNumeric(s2) Then
    If s2 > 0 Then
      g0 = s2: Exit Sub
    End If
  End If
End Sub


'-------------------------------
'Create the runs.dat file from the user entered case number.
'Verify required files exist.
'Move combustion grid to melt directory if needed.
'Inputs userError=false
'Outputs userError=true if cancel operation
'-------------------------------
Private Sub simnew()
Dim it_file As String
Dim iComb2Case As Integer
Dim case_len As Integer
Dim ii As Integer
Dim str_heat_flux_type As String
Dim cgrid_file As String
Dim cycleString As String

    case_action = "Simulate"
    If select_existing_case = False Then
        userError = True
        Exit Sub
    End If
    
    'If the restart indicator is set in the Sbc file
    'Then check for restart file existence
    Open fnm_sbc_path For Input As #5 'open the sbc file
    Line Input #5, s0 'get first line
    Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
        Line Input #5, s0 'get next line
    Loop
    Line Input #5, s0 'skip next line
    Line Input #5, s0 'this line should have restart type
    n = InStr(s0, "IRSTYP=")
    If n = 0 Then 'error
        userError = True
        Call MsgBox("Conditions file has error.  Go back to the Pre-Processor menu and" _
            & " verify parameters are correct.  Then resave the case.", vbOKOnly, "GFM")
        Close (5)
        Exit Sub
    End If
    restart_str = Mid(s0, n + 7, 1)
    If restart_str = "1" Then
        'should have a restart file
        If FileExist(fnm_rg_path) = False Then
            userError = True
            Call MsgBox("The Conditions file indicates a restart.  However, there is no" _
                & " restart file for this case number.", vbOKOnly, "GFM")
            Close (5)
            Exit Sub
        End If
    End If
    
    If flow_domain = 2 Then
        'Want to know the iheat_flux_type variable in the melt domain
        'so can tell whether an "it...m" file is required
        Do While (InStr(s0, "iheat_flux_type") = 0 And s0 <> "/")
            Line Input #5, s0
        Loop
        n = InStr(s0, "iheat_flux_type")
        If n = 0 Then 'error
            userError = True
            Call MsgBox("Conditions file has error.  Go back to the Pre-Processor menu and verify" _
                & " parameters are correct.  Then resave the case.", vbOKOnly, "GFM")
            Close (5)
            Exit Sub
        End If
        str_heat_flux_type = Mid(s0, n + 16, 1)
    Else
        'doing combustion, want to know if doing soot kinetics calibration
        'so can put label on screen
        Do While (InStr(s0, "isoot_cal") = 0 And s0 <> "/")
            Line Input #5, s0
        Loop
        n = InStr(s0, "isoot_cal")
        If n = 0 Then 'not provided, use default
            isoot_cal = default_isoot_cal
        Else
            If Mid(s0, n + 17, 1) = "1" Then
                isoot_cal = 1
            Else
                isoot_cal = 0
            End If
        End If
    End If
    Close (5)
  
    Open fnm_runs_path For Output As #4 'create a runs.dat file
    Print #4, "'Run Number:' "; case_number 'Place sbc file number into runs.dat file
    Print #4, "'Grid File:'  "; case_number 'Place grid file number into runs.dat file
    Print #4, "'Regenerate:' "; "0" 'regeneration indicator only set in melt domain of regenerative furnace"
    Print #4, "'Run Number:' "; case_number
    Print #4, "'Grid File'   "; case_number
    Close (4) 'runs.dat file has been created
  
    'set sbc indicator for cycling
    If cycleDomains = True Or cycleDomainsMelt = True Then
        cycleString = "1"
        'will update sbc below
    Else
        cycleString = "0"
        'make sure indicator is correct in the sbc file
        Call set_cycling_in_sbc(fnm_sbc_path, cycleString)
    End If
  
    If flow_domain = 2 Then
    '-----------------------------------------
    '   Doing melt simulation
    '-----------------------------------------
    
        meltpath = case_path
        combpath = dnm & "combustion\case" & case_number
        melt_case_number = case_number
        comb_case_number = case_number
    
        it_file = "\it" & case_number & "m.dat"
        cgrid_file = "\gd" & case_number & "c.dat"

        'Make sure files needed from combustion domain exist in melt case folder
        If FileExist(meltpath & cgrid_file) = False Then
            If FileExist(combpath & cgrid_file) = True Then
                'copy combustion grid to melt
                FileCopy combpath & cgrid_file, case_path & cgrid_file
            Else 'can't get the grid
                If str_heat_flux_type = "2" Or str_heat_flux_type = "4" Then
                    'really need grid
                    userError = True
                    Call MsgBox("The heat flux type parameter indicates that the combustion" _
                        & " domain grid file (gd....c.dat) is required in the melt case" _
                        & " folder.  However, this file could not be" _
                        & " obtained from the combustion case folder.", _
                        vbOKOnly + vbCritical, "GFM")
                    Exit Sub
                Else
                    'can do run but not able to produce it...t file
                    If MsgBox("The combustion grid for the melt case is not available, but " _
                        & "is required to produce the surface temperature output file. " _
                        & "Do you want to proceed with the simulation even though that file will not be produced?", _
                        vbYesNo + vbCritical, "GFM") = vbYes Then
                            'OK run will continue
                    Else
                        userError = True
                        Exit Sub
                    End If
                End If
            End If
        End If
        
        If FileExist(meltpath & it_file) = False Then
            If FileExist(combpath & it_file) = True Then
                'copy it...m file to melt
                FileCopy combpath & it_file, case_path & it_file
            Else 'can't get the it file
                userError = True
                Call MsgBox("The heat flux input file (it....m.dat) is not available." _
                    & " Either resave the melt case with a uniform flux type specified" _
                    & " or simulate the combustion case to produce the required file.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
        End If
        

        GoTo done_getting_cgrid 'skip this incomplete section because newer code is above
        If str_heat_flux_type = "2" Or str_heat_flux_type = "4" Then
            'Will need calculated heat flux from combustion and combustion grid.
            'Get these files from combustion directory.
            If FileExist(combpath & it_file) And FileExist(combpath & cgrid_file) Then
                'copy files from combustion directory
                FileCopy combpath & it_file, case_path & it_file
                FileCopy combpath & cgrid_file, case_path & cgrid_file
            Else 'need to warn user of error
                userError = True
                Call MsgBox("The heat flux type parameter indicates that the combustion" _
                    & " domain grid file (gd....c.dat) and heat flux file (it....m.dat)" _
                    & " are required.  However, one or both of these files could not be" _
                    & " obtained from the combustion case folder.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
        ElseIf str_heat_flux_type = "1" Or str_heat_flux_type = "3" Then
            'Just need the short version of the it....m.dat file
            If FileExist(case_path & it_file) = False Then
                userError = True
                Call MsgBox("Missing the heat flux file (it....m.dat). Saving the current" _
                    & " case with the current setting of the heat flux type parameter" _
                    & " will create the missing file.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
            'If the combustion grid exists, then still want combustion grid copied over to melt grid
            If FileExist(combpath & cgrid_file) Then
                'copy grid file from combustion directory to melt directory
                FileCopy combpath & cgrid_file, case_path & cgrid_file
            End If
        End If
done_getting_cgrid:
        
        
        '-----------------------------------------
        '  Additional processing to set up cycling with melt first
        '-----------------------------------------

        'If doing domain cycling, then prepare for combustion processing later
        If (cycleDomainsMelt = True) And cycleMeltCnt = 0 Then
           
            'save combustion file names and paths
            comb_fnm_txt = "case" & case_number & "c.txt"
            comb_fnm_txt_path = combpath & "\" & comb_fnm_txt
            comb_fnm_pre = "gd" & case_number & "c.pre" 'pre file
            comb_fnm_pre_path = combpath & "\" & comb_fnm_pre
            comb_fnm_gd = "gd" & case_number & "c.dat" 'grid file
            comb_fnm_gd_path = combpath & "\" & comb_fnm_gd
            comb_fnm_sbc = "sbc" & case_number & "c.dat" 'conditions file
            comb_fnm_sbc_path = combpath & "\" & comb_fnm_sbc
            comb_fnm_rt = "rt" & case_number & "c.out" 'field variable output file
            comb_fnm_rt_path = combpath & "\" & comb_fnm_rt
            comb_fnm_rg = "rg" & case_number & "c.d" 'restart file
            comb_fnm_rg_path = combpath & "\" & comb_fnm_rg
            comb_fnm_runs_path = dnm & "combustion\runs.dat" 'runs file
   
        
            'verify that combustion simulation files exist
            If FileExist(comb_fnm_txt_path) = False _
                    Or FileExist(comb_fnm_pre_path) = False _
                    Or FileExist(comb_fnm_gd_path) = False _
                    Or FileExist(comb_fnm_sbc_path) = False Then
                Call MsgBox("Case not defined in combustion domain." _
                    & " Create combustion case with same case number.", vbOKOnly, "GFM")
                userError = True
                Exit Sub
            End If
            
            'If the restart indicator is set in the combustion Sbc file
            'Then check for restart file existence
            Open comb_fnm_sbc_path For Input As #5 'open the sbc file
            Line Input #5, s0 'get first line   'need more error checking ???
            Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
                Line Input #5, s0 'get next line
            Loop
            Line Input #5, s0 'skip next line
            Line Input #5, s0 'this line should have restart type
            n = InStr(s0, "IRSTYP=")
            If n = 0 Then 'error
                userError = True
                Call MsgBox("Combustion conditions file has error.  Go back to the Pre-Processor menu" _
                    & " and verify parameters are correct.  Then resave the case.", _
                    vbOKOnly, "GFM")
                Close (5)
                Exit Sub
            End If
    
            If Mid(s0, n + 7, 1) = "1" Then
                'should have a restart file
                If FileExist(comb_fnm_rg_path) = False Then
                    userError = True
                    Call MsgBox("The combustion conditions file indicates a restart." _
                        & " However there is no combustion restart file for this case number.", _
                        vbOKOnly, "GFM")
                    Close (5)
                    Exit Sub
                End If
            End If
            
            'when doing combustion, want to know if doing soot kinetics calibration
            Do While (InStr(s0, "isoot_cal") = 0 And s0 <> "/")
                Line Input #5, s0
            Loop
            n = InStr(s0, "isoot_cal")
            If n = 0 Then 'not provided, use default
                isoot_cal = default_isoot_cal
            Else
                If Mid(s0, n + 17, 1) = "1" Then
                    isoot_cal = 1
                Else
                    isoot_cal = 0
                End If
            End If
            Close (5)
          
            'save combustion case description
            Open comb_fnm_txt_path For Input As #1
            Line Input #1, s0 'already have case name
            Line Input #1, comb_case_title
            Close (1)
            
            'Save a copy of the melt items for later
            melt_case_title = case_title
            melt_fnm_txt = fnm_txt
            melt_fnm_txt_path = fnm_txt_path
            melt_fnm_pre = fnm_pre
            melt_fnm_pre_path = fnm_pre_path
            melt_fnm_gd = fnm_gd
            melt_fnm_gd_path = fnm_gd_path
            melt_fnm_sbc = fnm_sbc
            melt_fnm_sbc_path = fnm_sbc_path
            melt_fnm_rg = fnm_rg
            melt_fnm_rg_path = fnm_rg_path
            melt_fnm_runs_path = fnm_runs_path
             
            'copy runs.dat melt file to combustion directory
            FileCopy fnm_runs_path, comb_fnm_runs_path
             
            If cycleRegen = True Then
                'Have a second combustion space to deal with
                Call prepare_for_regenerative_furnace_cycling
            End If
            
            Call set_cycling_in_sbc(comb_fnm_sbc_path, cycleString)
            Call set_cycling_in_sbc(melt_fnm_sbc_path, cycleString)
            
        End If 'end to cycling code
        
    Else 'flow_domain=1
    '-----------------------------------------
    '   Doing combustion simulation
    '-----------------------------------------
        
        combpath = case_path
        meltpath = dnm & "melt\case" & case_number
        melt_case_number = case_number
        comb_case_number = case_number
        
        'Make sure file it####t.dat exists in directory
        If FileExist(case_path & "\it" & case_number & "t.dat") = False Then
            If FileExist(meltpath & "\it" & case_number & "t.dat") Then
                'copy "it" file from melt directory
                FileCopy meltpath & "\it" & case_number & "t.dat", _
                    case_path & "\it" & case_number & "t.dat"
            Else
                userError = True
                Call MsgBox("The melt surface temperature file is required in the" _
                    & " combustion directory. Verify that the melt surface temperature" _
                    & " parameter is properly set and resave the combustion case.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
        End If
            
        '-----------------------------------------
        '  Additional processing to set up cycling with combustion first
        '-----------------------------------------

        'If doing domain cycling, then prepare for melt processing later
        If (cycleDomains = True) And cycleCombCnt = 0 Then
           
            'save melt file names and paths
            melt_fnm_txt = "case" & case_number & "m.txt"
            melt_fnm_txt_path = meltpath & "\" & melt_fnm_txt
            melt_fnm_pre = "gd" & case_number & "m.pre"
            melt_fnm_pre_path = meltpath & "\" & melt_fnm_pre
            melt_fnm_gd = "gd" & case_number & "m.dat"
            melt_fnm_gd_path = meltpath & "\" & melt_fnm_gd
            melt_fnm_sbc = "sbc" & case_number & "m.dat"
            melt_fnm_sbc_path = meltpath & "\" & melt_fnm_sbc
            melt_fnm_rg = "rg" & case_number & "m.d" 'restart file
            melt_fnm_rg_path = meltpath & "\" & melt_fnm_rg
            melt_fnm_runs_path = dnm & "melt\runs.dat"
            
            'verify that melt simulation files exist
            If FileExist(melt_fnm_txt_path) = False _
                    Or FileExist(melt_fnm_pre_path) = False _
                    Or FileExist(melt_fnm_gd_path) = False _
                    Or FileExist(melt_fnm_sbc_path) = False Then
                Call MsgBox("Case not defined in melt domain." _
                    & " Create melt case with same case number.", vbOKOnly, "GFM")
                userError = True
                Exit Sub
            End If
            
            'If the restart indicator is set in the melt Sbc file
            'Then check for restart file existence
            Open melt_fnm_sbc_path For Input As #5 'open the sbc file
            Line Input #5, s0 'get first line   'need more error checking ???
            Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
                Line Input #5, s0 'get next line
            Loop
            Line Input #5, s0 'skip next line
            Line Input #5, s0 'this line should have restart type
            n = InStr(s0, "IRSTYP=")
            If n = 0 Then 'error
                userError = True
                Call MsgBox("Melt conditions file has error.  Go back to the Pre-Processor menu" _
                    & " and verify parameters are correct.  Then resave the case.", _
                    vbOKOnly, "GFM")
                Close (5)
                Exit Sub
            End If
    
            If Mid(s0, n + 7, 1) = "1" Then
                'should have a restart file
                If FileExist(melt_fnm_rg_path) = False Then
                    userError = True
                    Call MsgBox("The melt conditions file indicates a restart." _
                        & " However there is no melt restart file for this case number.", _
                        vbOKOnly, "GFM")
                    Close (5)
                    Exit Sub
                End If
            End If
            
            'Want to know the iheat_flux_type variable in the melt domain
            Do While (InStr(s0, "iheat_flux_type") = 0 And s0 <> "/")
                Line Input #5, s0
            Loop
            n = InStr(s0, "iheat_flux_type")
            If n = 0 Then 'error
                userError = True
                Call MsgBox("Conditions file has error.  Go back to the Pre-Processor menu" _
                    & " and verify parameters are correct.  Then resave the case.", _
                    vbOKOnly, "GFM")
                Close (5)
                Exit Sub
            End If
            str_heat_flux_type = Mid(s0, n + 16, 1)
            Close (5)
            
            If str_heat_flux_type = "1" Or str_heat_flux_type = "3" Then
                userError = True
                Call MsgBox("The heat flux type parameter must indicate combustion" _
                    & " calculation for cycling. Reset the parameter and save the" _
                    & " melt case.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If

            'Will need calculated heat flux from combustion and combustion grid.
            'Heat flux file will be created from combustion simulation before the
            'melt simulation begins. Just need to get the combustion grid now.
            If FileExist(fnm_gd_path) Then
                FileCopy fnm_gd_path, meltpath & "\gd" & case_number & "c.dat"
            Else 'need to warn user of error
                userError = True
                Call MsgBox("Cycling requires that the combustion domain grid file" _
                    & " (gd....c.dat) " _
                    & " is in the melt directory.  However, the file" _
                    & " could not be obtained from the combustion case folder.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
           
            'save melt case description
            Open melt_fnm_txt_path For Input As #1
            Line Input #1, s0 'already have case name
            Line Input #1, melt_case_title
            Close (1)
            
            'Save a copy of the combustion items for later
            comb_case_number = case_number
            comb_case_title = case_title
            comb_fnm_txt = fnm_txt
            comb_fnm_txt_path = fnm_txt_path
            comb_fnm_pre = fnm_pre
            comb_fnm_pre_path = fnm_pre_path
            comb_fnm_gd = fnm_gd
            comb_fnm_gd_path = fnm_gd_path
            comb_fnm_sbc = fnm_sbc
            comb_fnm_sbc_path = fnm_sbc_path
            comb_fnm_rg = fnm_rg
            comb_fnm_rg_path = fnm_rg_path
            comb_fnm_runs_path = fnm_runs_path
             
            'copy runs.dat combustion file to melt directory
            FileCopy fnm_runs_path, melt_fnm_runs_path
             
            If cycleRegen = True Then
                'Have a second combustion space to deal with
                Call prepare_for_regenerative_furnace_cycling
            End If ' end to regenerative code for second combustion space
            
            Call set_cycling_in_sbc(comb_fnm_sbc_path, cycleString)
            Call set_cycling_in_sbc(melt_fnm_sbc_path, cycleString)
            
        End If 'end to cycling code
    End If
End Sub


Private Sub prepare_for_regenerative_furnace_cycling()
'Have a second combustion space to deal with when starting a regenerative simulation

    comb2_case_number = CInt(case_number) + 1
    Do While Len(comb2_case_number) < 4
        comb2_case_number = "0" & comb2_case_number
    Loop
    
    comb2path = dnm & "combustion\case" & comb2_case_number
    comb2_fnm_txt = "case" & comb2_case_number & "c.txt"
    comb2_fnm_txt_path = comb2path & "\" & comb2_fnm_txt
    comb2_fnm_pre = "gd" & comb2_case_number & "c.pre" 'pre file
    comb2_fnm_pre_path = comb2path & "\" & comb2_fnm_pre
    comb2_fnm_gd = "gd" & comb2_case_number & "c.dat" 'grid file
    comb2_fnm_gd_path = comb2path & "\" & comb2_fnm_gd
    comb2_fnm_sbc = "sbc" & comb2_case_number & "c.dat" 'conditions file
    comb2_fnm_sbc_path = comb2path & "\" & comb2_fnm_sbc
    comb2_fnm_rt = "rt" & comb2_case_number & "c.out" 'field variable output file
    comb2_fnm_rt_path = comb2path & "\" & comb2_fnm_rt
    comb2_fnm_rg = "rg" & comb2_case_number & "c.d" 'restart file
    comb2_fnm_rg_path = comb2path & "\" & comb2_fnm_rg
    comb2_fnm_runs_path = dnm & "combustion\runs.dat" 'runs file
            
    'verify that comb2 simulation files exist
    If FileExist(comb2_fnm_txt_path) = False _
            Or FileExist(comb2_fnm_pre_path) = False _
            Or FileExist(comb2_fnm_gd_path) = False _
            Or FileExist(comb2_fnm_sbc_path) = False Then
        Call MsgBox("Second regenerative combustion case is not defined." _
            & " Create new case with first case number plus one.", _
            vbOKOnly, "GFM")
        userError = True
        Exit Sub
    End If
     
    'If the restart indicator is set in the comb2 Sbc file
    'Then check for restart file existence
    Open comb2_fnm_sbc_path For Input As #5 'open the sbc file
    Line Input #5, s0 'get first line   'need more error checking ???
    Do While (s0 <> "&INPUT") And Not EOF(5) 'locate start of input namelist
        Line Input #5, s0 'get next line
    Loop
    Line Input #5, s0 'skip next line
    Line Input #5, s0 'this line should have restart type
    n = InStr(s0, "IRSTYP=")
    If n = 0 Then 'error
        userError = True
        Call MsgBox("Second case conditions file has error." _
            & " Go back to the Pre-Processor menu" _
            & " and verify parameters are correct.  Then resave the case.", _
            vbOKOnly, "GFM")
        Close (5)
        Exit Sub
    End If

    If Mid(s0, n + 7, 1) = "1" Then
        'should have a restart file
        If FileExist(comb2_fnm_rg_path) = False Then
            userError = True
            Call MsgBox("The second case conditions file indicates a restart." _
                & " However, there is" _
                & " no restart file for this case number.", vbOKOnly, "GFM")
            Close (5)
            Exit Sub
        End If
    End If
    Close (5)
                   
    'Need second combustion grid in melt directory.
    If FileExist(comb2_fnm_gd_path) Then
        FileCopy comb2_fnm_gd_path, meltpath & "\gd" & comb2_case_number & "c.dat"
    Else 'need to warn user of error
        userError = True
        Call MsgBox("Cycling requires that the second combustion domain grid file" _
            & " (gd....c.dat) " _
            & " is in the melt directory.  However, the file" _
            & " could not be obtained from the combustion case folder.", _
            vbOKOnly + vbCritical, "GFM")
        Exit Sub
    End If
   
    If cycleDomains = True Then
        'Doing combustion first, make sure file it####t.dat exists in comb2 folder
        If FileExist(comb2path & "\it" & comb2_case_number & "t.dat") = False Then
            If FileExist(meltpath & "\it" & case_number & "t.dat") Then
                'copy "it" file from melt directory
                FileCopy meltpath & "\it" & case_number & "t.dat", _
                    comb2path & "\it" & comb2_case_number & "t.dat"
            Else
                userError = True
                Call MsgBox("The melt surface temperature second case file is required in the" _
                    & " combustion directory. Verify that the melt surface temperature" _
                    & " parameter is properly set and resave the second combustion case.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
        End If
    ElseIf cycleDomainsMelt = True Then
        'Doing melt first, make sure it####m.dat for comb2 exists in melt folder
        If FileExist(meltpath & "\it" & comb2_case_number & "m.dat") = False Then
            If FileExist(meltpath & "\it" & case_number & "m.dat") Then
                'copy "it" file from melt directory
                FileCopy meltpath & "\it" & case_number & "m.dat", _
                    meltpath & "\it" & comb2_case_number & "m.dat"
            Else
                userError = True
                Call MsgBox("The second combustion case heat flux file is required in the" _
                    & " melt folder. Verify that the melt surface temperature" _
                    & " parameter is properly set and resave the second combustion case.", _
                    vbOKOnly + vbCritical, "GFM")
                Exit Sub
            End If
        End If
    End If
                    
    'save comb2 case description
    Open comb2_fnm_txt_path For Input As #1
    Line Input #1, s0 'already have case name
    Line Input #1, comb2_case_title
    Close (1)
                                   
    'create a regenerative melt runs.dat file
    Open melt_fnm_runs_path For Output As #4
    Print #4, "'Run Number:' "; case_number 'Place sbc file number into runs.dat file
    Print #4, "'Grid File:'  "; case_number 'Place grid file number into runs.dat file
    Print #4, "'Regenerate:' "; "1" 'doing regeneration
    Print #4, "'Run Number:' "; comb2_case_number
    Print #4, "'Grid File'   "; comb2_case_number
    Close (4) 'runs.dat file has been created for melt with regenerative furnace
    
    Call set_cycling_in_sbc(comb2_fnm_sbc_path, "1")
End Sub


'-------------------------------
'Update Sbc file to show that cycling is active (or not)
'-------------------------------
Private Sub set_cycling_in_sbc(sbc_path As String, cycle_str As String)
Dim tmp_fnm_sbc As String
Dim s00 As String
Dim front_line As String
Dim back_line As String

    tmp_fnm_sbc = Left(sbc_path, Len(sbc_path) - 3) & "tmp"
    Name sbc_path As tmp_fnm_sbc 'make the sbc file into a temporary file to read in
    Open tmp_fnm_sbc For Input As #5
    Open sbc_path For Output As #6 'recreate the sbc file
    'Do a hand file copy but change the cycling indicator
    n = 0
    Do While n = 0 And Not EOF(5)
        Line Input #5, s0 'get line from temporary file
        n = InStr(s0, "cycling=")
        If n <> 0 Then
            'found line containing cycling indicator
            front_line = Left(s0, n + 7)
            back_line = Right(s0, Len(s0) - (n + 8))
            s00 = front_line & cycle_str & back_line 'set cycling indicator as requested
            Print #6, s00
            Exit Do
        End If
        Print #6, s0
    Loop
                
    Do While Not EOF(5) 'copy rest of file
        Line Input #5, s0
        Print #6, s0
    Loop
    
    Close (5)
    Close (6)
    Kill tmp_fnm_sbc

End Sub



'-------------------------------
'Begin combustion simulation
'Case number and associated file/path names have been determined before entry to this routine.
'-------------------------------
Private Sub simcr_Click()
  Dim dnm1 As String, dnm2 As String
  Dim run_dir As String
 
  flow_domain = 1 'indicate doing combustion
  title = "Combustion"
  'dnm1 = dnm & "combustion\"
  'dnm2 = dnm1 & exe_dir & "\comb.exe"
  'set executable directory at compile time, 9-28-06
  #If DebugVersion = 1 Then
      run_dir = dnm & "combustion\debug\comb.exe"
  #Else
      run_dir = dnm & "bin\comb.exe"
  #End If

  'dnm2 = dnm & "bin\comb.exe"
  mnuView.Visible = False
  MnuOptCol.Visible = False
  'If InStr(Caption, "Running") > 0 Then
    ' List1.List(0) = "Reinitializing"
    'If cycleDomains = False And cycleRegen = False Then GoTo scr1
  'End If
  ChDir dnm & "combustion"
  'ChDir case_path
  Call setup(1) 'turn off initial screen items and prepare for pre-processor screen
  sim0.Visible = False
  
  'get rid of any leftover files
  On Error Resume Next
  s0 = case_path & "\runstop.dat": If FileExist(s0) Then Kill s0
  s0 = case_path & "\gfm.dat": If FileExist(s0) Then Kill s0
  s0 = case_path & "\gfm0.dat": If FileExist(s0) Then Kill s0
  
  
  'This commented out code was for earlier version before case numbers were used.
  'get grid file numbers from runs.dat file
  'fnm = dnm1 & "runs.dat"
  'Open fnm For Input As #4
  'Input #4, s0
  'fnm = "sbc" & Mid(s0, 13, 4) & "c.dat"
  'fnm_sbc = fnm 'set sbc file name
  'Input #4, s0
  'fnm = dnm1 & "gd" & Mid(s0, 13, 4) & "c.dat"
  'fnm_gd_path = fnm 'set grid path and filename
  'Close (4)
  
  Call read_grid
  
  'this section different in simmr @@@@@@@@@@
  x1a = xg(mp) - xg(1): y1a = yg(np) - yg(1)
  z1a = zg(lp) - zg(1)
  g2 = zf3 / zf2
  If hgt_c > 0 Then g2 = 1
  g0 = 10000 / (x1a + z1a * g2): g1 = 5000 / (y1a + z1a * g2)
  If g0 > g1 Then g0 = g1
  zf1 = g0: zf2 = g0: zf3 = g0 * g2
  'end of this section different in simmr @@@@@@@@@@
  
    'Plot the grid
    Call setup(2)
    Call plt_gd(0)
    
    'display 2 list boxes and prepare other two for radiation info
    List1.Clear: List2.Clear
    List3.Clear: List4.Clear
    For i = 0 To 3
        List1.AddItem ""
        List2.AddItem ""
        List3.AddItem ""
        List4.AddItem ""
    Next
    List1.Width = 1800: List2.Width = 1800
    List3.Width = 1800: List4.Width = 1800
    List1.Height = 300 * 4
    List2.Height = List1.Height: List3.Height = List1.Height: List4.Height = List1.Height
    List1.Top = hgts - List1.Height
    List2.Top = List1.Top: List3.Top = List1.Top: List4.Top = List1.Top
    If cycleDomains = True Or cycleDomainsMelt = True Then
        List1.Left = Lbsw.Left + Lbsw.Width + 100
    Else
        List1.Left = 5600
    End If
    List2.Left = List1.Left + List1.Width
    List3.Left = List2.Left + List2.Width:    List4.Left = List3.Left + List3.Width
    'List1.List(0) = "Initializing run"
    List1.List(0) = "Running"
    List2.List(0) = "Heat (MW)"
    List1.Visible = True: List2.Visible = True
    List3.Visible = False: List4.Visible = False
    If isoot_cal = 1 Then
        LbSoot.Visible = True
    Else
        LbSoot.Visible = False
    End If
    ReDim fp(nx, ny, nz)
    s0 = "Running Combustion Flow Calculation"
    frmMain.Caption = s0 & ": Grid"
   
    Lbadd.Caption = "grid"
    Lbdel.Caption = "pause"
    Lbmv.Caption = "b/w"
    Timer1.Enabled = True
    Timer1.Interval = 5000 'milliseconds

scr1: 'skipped to here when already running
    Lbsw.BackColor = RGB(255, 200, 200)
    Lbsw.Caption = "stop"
    Lbdel.Visible = True
    'Start CFD code
    If cycleDomains = True Or cycleDomainsMelt = True Then
        Open case_path & "\cycleInfo.txt" For Output As #7
        Write #7, cycleInfo, "combustion cycle count number"
        Close (7) 'cycling information file has been created
    End If
    cfd_task_id = Shell(run_dir, vbMinimizedNoFocus)
End Sub

'-------------------------------
'Begin melt simulation
'Case number and associated file/path names have been determined before entry to this routine.
'-------------------------------
Private Sub simmr_Click()
  'Dim hwnd0 As Long, hwnd1 As Long
  Dim dnm1 As String, dnm2 As String
  Dim run_dir As String
  
  flow_domain = 2  'indicate doing melter
  title = "Melt"
  'dnm1 = dnm & "melt\"
  'dnm2 = dnm1 & exe_dir & "\melt.exe"
  'set executable directory at compile time, 9-28-06
  #If DebugVersion = 1 Then
      run_dir = dnm & "melt\debug\melt.exe"
  #Else
      run_dir = dnm & "bin\melt.exe"
  #End If

  mnuView.Visible = False
  MnuOptCol.Visible = False
  'If InStr(Caption, "Running") > 0 Then
   ' List1.List(0) = "Reinitializing"
    'If cycleDomains = False And cycleRegen = False Then GoTo smr1
  'End If
  ChDir dnm & "melt"
  'ChDir case_path
  Call setup(1) 'turn off initial screen items and prepare for pre-processor screen
  sim0.Visible = False
  
  'get rid of any leftover files
  On Error Resume Next
  s0 = case_path & "\runstop.dat": If FileExist(s0) Then Kill s0
  s0 = case_path & "\gfm.dat": If FileExist(s0) Then Kill s0
  s0 = case_path & "\gfm0.dat": If FileExist(s0) Then Kill s0

  'This commented out code was for earlier version before case numbers were used.
  'get grid file numbers from runs.dat file
  'fnm = dnm1 & "runs.dat"
  'Open fnm For Input As #4
  'Input #4, s0
  'fnm = "sbc" & Mid(s0, 13, 4) & "c.dat"
  'fnm_sbc = fnm 'set sbc file name
  'Input #4, s0
  'fnm = dnm1 & "gd" & Mid(s0, 13, 4) & "c.dat"
  'fnm_gd_path = fnm 'set grid path and filename
  'Close (4)

  Call read_grid
  
  'this section different in simcr @@@@@@@@@@
  g0 = 8000 / (xg(mp) - xg(3))
  g1 = 5000 / (yg(np) - yg(3))
  If g0 >= g1 Then zf1 = g1 Else zf1 = g0
  zf2 = zf1
  If (yg(np) - yg(3)) / (zg(lp) - zg(3)) > 5 _
    Then zf3 = zf1 * 2 Else: zf3 = zf1
  'end of this section different in simcr @@@@@@@@@@
  
  'Plot the grid
    Call setup(2)
    Call plt_gd(0)

    'display 2 list boxes
    List1.Clear: List2.Clear
    List3.Clear: List4.Clear
    For i = 0 To 4
        List1.AddItem ""
        List2.AddItem ""
    Next
    List1.Width = 1800: List2.Width = 1800
    List1.Height = 300 * 5: List2.Height = List1.Height
    List1.Top = hgts - List1.Height
    List2.Top = List1.Top
    List1.Left = 5600
    'List1.List(0) = "Initializing run"
    List1.List(0) = "Running"
    List2.Left = List1.Left + List1.Width
    List1.Visible = True: List2.Visible = True
    List3.Visible = False: List4.Visible = False
  
  ReDim fp(nx, ny, nz)
  fp_mx = 1800: fp_mn = 1200
  s0 = "Running Melt Flow Calculation"
  frmMain.Caption = s0 & ": Grid"
  
  Lbadd.Caption = "grid"
  Lbdel.Caption = "pause"
  Lbmv.Caption = "b/w"
  Timer1.Enabled = True
  Timer1.Interval = 5000
  
smr1: 'skipped to here when already running
  Lbsw.BackColor = RGB(255, 200, 200)
  Lbsw.Caption = "stop"
  Lbdel.Visible = True
  LbSoot.Visible = False
  
  'Start CFD code
  If cycleDomainsMelt = True Or cycleDomains = True Then
        Open case_path & "\cycleInfo.txt" For Output As #7
        Write #7, cycleInfo, "melt cycle count number"
        Close (7) 'cycling information file has been created
  End If
  cfd_task_id = Shell(run_dir, vbMinimizedNoFocus)
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_xb_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_xb / 2) * 2
  If n1 < 2 Then n1 = 2
  If n1 > nxe(1) Then n1 = nxe(1)
  tx_xb = n1
  If n1 > ix0 Then ix0 = n1: tx_xc = ix0
  If InStr(Lbnd.Caption, "select nodes") Then
    nxbs(1) = n1
  Else
    nxb(1) = n1
    plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_yb_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_yb / 2) * 2
  If n1 < 2 Then n1 = 2
  If n1 > nxe(2) Then n1 = nxe(2)
  tx_yb = n1
  If n1 > iy0 Then iy0 = n1: tx_yc = iy0
  If InStr(Lbnd.Caption, "select nodes") Then
    nxbs(2) = n1
  Else
    nxb(2) = n1: plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_zb_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_zb / 2) * 2
  If n1 < 2 Then n1 = 2
  If n1 > nxe(3) Then n1 = nxe(3)
  tx_zb = n1
  If n1 > iz0 Then iz0 = n1: tx_zc = iz0
  If InStr(Lbnd.Caption, "select nodes") Then
    nxbs(3) = n1
  Else
    nxb(3) = n1: plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_xe_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_xe / 2) * 2
  If n1 < nxb(1) Then n1 = nxb(1)
  If n1 > mp Then n1 = mp
  tx_xe = n1
  If n1 < ix0 Then ix0 = n1: tx_xc = ix0
  If InStr(Lbnd.Caption, "select nodes") Then
    nxes(1) = n1
  Else
    nxe(1) = n1: plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_ye_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_ye / 2) * 2
  If n1 < nxb(2) Then n1 = nxb(2)
  If n1 > np Then n1 = np
  tx_ye = n1
  If n1 < iy0 Then iy0 = n1: tx_yc = iy0
  If InStr(Lbnd.Caption, "select nodes") Then
    nxes(2) = n1
  Else
    nxe(2) = n1: plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_ze_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_ze / 2) * 2
  If n1 < nxb(3) Then n1 = nxb(3)
  If n1 > lp Then n1 = lp
  tx_ze = n1
  If n1 < iz0 Then iz0 = n1: tx_zc = iz0
  If InStr(Lbnd.Caption, "select nodes") Then
    nxes(3) = n1
  Else
    nxe(3) = n1: plt_gd (0)
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_xc_DblClick()
  Dim n1 As Integer
  n1 = ix0 + m_sw
  If n1 > nxe(1) Then n1 = nxe(1): m_sw = -2
  If n1 < nxb(1) Then n1 = nxb(1): m_sw = 2
  tx_xc = n1: ix0 = n1
  plt_gd (1)
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_xc_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_xc / 2) * 2
  If n1 < nxb(1) Then n1 = nxb(1)
  If n1 > nxe(1) Then n1 = nxe(1)
  tx_xc = n1: ix0 = n1
  plt_gd (1)
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_yc_dblClick()
  Dim n1 As Integer
  n1 = iy0 + m_sw
  If n1 > nxe(2) Then n1 = nxe(2): m_sw = -2
  If n1 < nxb(2) Then n1 = nxb(2): m_sw = 2
  tx_yc = n1: iy0 = n1
  plt_gd (2)
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_yc_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_yc / 2) * 2
  If n1 < nxb(2) Then n1 = nxb(2)
  If n1 > nxe(2) Then n1 = nxe(2)
  tx_yc = n1: iy0 = n1
  plt_gd (2)
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_zc_dblclick()
  Dim n1 As Integer
  n1 = iz0 + m_sw
  If n1 > nxe(3) Then n1 = nxe(3): m_sw = -2
  If n1 < nxb(3) Then n1 = nxb(3): m_sw = 2
  tx_zc = n1: iz0 = n1
  plt_gd (3)
End Sub

'-------------------------------
'-------------------------------
Private Sub tx_zc_KeyPress(KeyAscii As Integer)
  Dim n1 As Integer
  If KeyAscii <> Asc(vbCr) Then Exit Sub
  n1 = Int(tx_zc / 2) * 2
  If n1 < nxb(3) Then n1 = nxb(3)
  If n1 > nxe(3) Then n1 = nxe(3)
  tx_zc = n1: iz0 = n1
  plt_gd (3)
End Sub

'-------------------------------
'-------------------------------
Private Sub Timer1_Timer()
'Using 1/2 second sweep timing
'Using 5 second simulation update timing
  
    If menu_layer <> InSimulation Then
        'This code used in grid construction or post processing to sweep thru the grid
        'one cell at a time in the specified direction
        If Optx Then Call tx_xc_DblClick
        If Opty Then Call tx_yc_dblClick
        If Optz Then Call tx_zc_dblclick
        Exit Sub
    End If
 
    'The simulation is running
    If Not List1.Visible Then Exit Sub 'do nothing if the List1 is not visible
    If FileExist(case_path & "\gfm.dat") = False Then
        'The gfm.dat file does not exist, is CFD done?
            'If flow_domain = 2 Then s0 = dnm & "melt\" & exe_dir & "\melt.exe" _
            'Else s0 = dnm & "combustion\" & exe_dir & "\comb.exe"
            #If DebugVersion = 1 Then
               If flow_domain = 2 Then s0 = dnm & "melt\debug\melt.exe" Else s0 = dnm & "combustion\debug\comb.exe"
            #Else
               If flow_domain = 2 Then s0 = dnm & "bin\melt.exe" Else s0 = dnm & "bin\comb.exe"
            #End If
            hwnd0 = FindWindow(vbNullString, s0)
            If hwnd0 = 0 Then  'CFD has really stopped
                If (cycleDomains = True Or cycleDomainsMelt = True) And userStoppedSim = False Then
                    Call switch_domain
                ElseIf cancelMode = True Then
                    'Do clean-up (get rid of any leftover files) and end program
                    On Error Resume Next
                    Kill dnm & "tmp\*.*"
                    s0 = case_path & "\runstop.dat": If FileExist(s0) Then Kill s0
                    s0 = case_path & "\gfm0.dat": If FileExist(s0) Then Kill s0
                    Timer1.Enabled = False
                    End
                    Exit Sub
                Else
                    Lbdel.Visible = False
                    Timer1.Enabled = False
                    ex0.Caption = "Done"
                    Lbsw.BackColor = vbGreen
                    Lbsw.Caption = "cont"
                    If List1.List(0) = "Running" And List1.Visible = True Then
                        List1.List(0) = "Run Done"
                    End If
                    If (cycleDomains = True Or cycleDomainsMelt = True) Then
                        Lbsw.Visible = False
                        If userStoppedSim = True Then
                            ListCycle.List(3) = "USER TERMINATED CYCLING"
                            cycleDomains = False
                            cycleDomainsMelt = False
                            cycleCombCnt = 0
                            cycleRegen = False
                            cycleComb2Cnt = 0
                            cycleMeltCnt = 0
                            userStoppedSim = False
                            cycleEnd = 0
                        End If
                    End If
                    'Print CFD error message if it exists
                    If FileExist(case_path & "\runend.txt") Then
                        Open case_path & "\runend.txt" For Input As #2
                        Line Input #2, s0
                        If InStr(s0, "normal") > 0 Then
                            Close (2)
                            Exit Sub
                        End If
                        Do While Not EOF(2)
                            Line Input #2, s1
                            s0 = s0 & s1
                        Loop
                        Close (2)
                        Call MsgBox("CFD ERROR, return to PreProcessor to resolve:  " & s0, _
                            vbOKOnly + vbCritical, "GFM")
                    End If
                End If
                Exit Sub
            End If 'hwnd0=0
            
        If FileExist(case_path & "\runend.txt") Then   'CFD is stopping
            Lbsw.BackColor = vbYellow:
            Lbsw.Caption = "ending"
            'continue to wait until CFD is completely stopped
        End If
        Exit Sub 'wait until gfm file is available
    End If
  
  'process gfm.dat file
  On Error GoTo tm1a1
  If FileExist(case_path & "\gfm0.dat") Then Kill case_path & "\gfm0.dat"
  Name case_path & "\gfm.dat" As case_path & "\gfm0.dat" 'Rename gfm.dat so CFD code can use it again
  Open case_path & "\gfm0.dat" For Input As #1
  Input #1, n
  Input #1, n1: Input #1, g0
  If flow_domain = 2 Then GoTo tm1a6
  'in combustion domain here
  Input #1, s0: Input #1, fp_ut
  g1 = 0
  For i = 1 To nx: For j = 1 To ny: For k = 1 To nz
    Input #1, fp(i, j, k)
    If fp(i, j, k) > g1 Then g1 = fp(i, j, k)
  Next: Next: Next
  If InStr(s0, "Temp") > 0 Then
    If fp_mn <= 0 Then fp_mn = 300:
    'fp_mx = 2200 'why hard code these min and max temps  ???
    fp_mx = g1
    GoSub tm1a3
  Else
    fp_mn = 0: fp_mx = g1 'g1 has been set to max value in reads of fp()
    If InStr(s0, "Rad") > 0 Then GoSub tm1a4
    If InStr(s0, "Wall") > 0 Then GoSub tm1a5
  End If
  Close (1)
  Kill (case_path & "\gfm0.dat")
  For i = 1 To nx: For j = 1 To ny
    If flow_domain = 1 Then fp(i, j, 1) = fp(i, j, 2)
    If flow_domain = 2 Then fp(i, j, lz) = fp(i, j, lz - 1)
  Next: Next
  If Lbdel.Caption = "auto" Then Exit Sub
  If InStr(frmMain.Caption, "Post") > 0 Then
    Call plt_clr(0)
    If Lbadd.Caption = "plot" Then Lbadd.Caption = "grid"
  End If
  Exit Sub
  
tm1a1: 'jump to here if get error reading gfm file
  Close (1)
  Exit Sub
  
tm1a3:
  'Post temperature
  If List3.Visible Then List3.Visible = False
  If List4.Visible Then List4.Visible = False
  s0 = "Running Combustion Flow Calculation"
  frmMain.Caption = s0 & ": Post (Temperature)"
  If InStr(List1.List(0), "iter") = 0 Then
    fp_mx = 2200: fp_mn = 300
  End If
  If List1.ListCount > 4 Then
    For i = 4 To List1.ListCount - 1
      List1.RemoveItem i
    Next
  End If
  List1.List(0) = "iter: " & Format(n, "0/") _
    & Format(n1, "0")
  g0 = -Log(g0) / Log(10)
  Input #1, g1: g1 = -Log(g1) / Log(10)
  s0 = "conv: " & Format(g0, "0.##") & "/"
  List1.List(1) = s0 & Format(g1, "0.#")
  Input #1, g0: t_ex = g0 '=t_av in CFD
  s0 = fp_ut: Call ucv
  List1.List(2) = "T/ex: " & Format(g0, "0") & " " & s0
  Input #1, h_in '=q_f in CFD
  List2.List(1) = "combustion: " & Format(h_in, "0.##")
  Input #1, h_net '=(qeg0-qeg_g-qew_g)*1.0d-6 in CFD
  List2.List(2) = "gas/n: " & Format(h_net, "0.##")
  Input #1, soot '=svf_r in CFD, unclear what this really is so do not display
  'List1.List(3) = "soot: " & Format(soot, "0.##E-")
  List1.List(3) = " "
  Input #1, h_sur, g_sur '=qa_s0, qa_s1 in CFD
  'seems to be current net radiation to melt surface, difference between current & previous
  s1 = "gls/n: " & Format(h_sur, "0.##/")
  If g_sur >= 0 Then
    List2.List(3) = s1 & Format(g_sur, "+0.##")
  Else
    List2.List(3) = s1 & Format(g_sur, "0.##")
  End If
  Input #1, lr0 '=count of radiation intervals
  'List2.List(0) = "Heat (MW): " & lr0  'this line does not make sense
  List2.List(0) = "Heat (MW): "
  'If n >= n1 Then GoTo tm1a2  'This line will put "cont" on screen before CFD is done! @@@
  Return
  
tm1a4:  'Post Gas Heat Absorption
  If InStr(List1.List(0), "rad") = 0 Then
    s0 = "Running Gas Emission/Absorption Calculation"
    frmMain.Caption = s0 & ": Post (Heat Absorption)"
  End If
  If Not List3.Visible Then
    List3.Visible = True: List4.Visible = True
  End If
  List3.List(0) = "cal: " & Format(n / n1, "0.#%")
  List3.List(3) = ""
tm1b4:
  Input #1, g0: GoSub tm1a4a
  List3.List(1) = "gas/e: " & s0
  Input #1, g0: GoSub tm1a4a
  List3.List(2) = "gas/a: " & s0
  Input #1, g0: GoSub tm1a4a
  List4.List(0) = "wall/e: " & s0
  Input #1, g0: GoSub tm1a4a
  List4.List(1) = "wall/a: " & s0
  Input #1, g0: GoSub tm1a4a
  List4.List(2) = "glass/e: " & s0
  Input #1, g0: GoSub tm1a4a
  List4.List(3) = "glass/a: " & s0
  Return
  
tm1a4a:
  If g0 < 1001 Then
    s0 = Format(g0, "0W")
  ElseIf g0 < 10001 Then
    g0 = g0 / 1000
    s0 = Format(g0, "0.##kW")
  ElseIf g0 < 100001 Then
    g0 = g0 / 1000
    s0 = Format(g0, "0.#kW")
  ElseIf g0 < 1000001 Then
    g0 = g0 / 1000
    s0 = Format(g0, "0kW")
  ElseIf g0 < 10000001 Then
    g0 = g0 / 1000000
    s0 = Format(g0, "0.##")
  ElseIf g0 < 100000001 Then
    g0 = g0 / 1000000
    s0 = Format(g0, "0.#")
  Else
    g0 = g0 / 1000000
    s0 = Format(g0, "0")
  End If
  Return
  
tm1a5:
  'Post wall heat absorption
  If InStr(List1.List(0), "wall") = 0 Then
    s0 = "Running Wall Emission/Absorption Calculation"
    frmMain.Caption = s0 & ": Post (Wall Heat Absorption)"
  End If
  If Not List3.Visible Then
    List3.Visible = True: List4.Visible = True
  End If
  If n1 > lp Then n1 = lp
  List3.List(0) = "iter: " & n & "(" & n1 & "/" & lp & ")"
  List3.List(3) = "conv: " & Format(g0, "0.###")
  GoTo tm1b4
  
tm1a6:
  'Running melt and just read in 3 items (n,n1,g0) from the gfm.dat file
  List1.List(0) = "iter: " & Format(n, "0/") _
    & Format(n1, "0")
  If g0 > 0 Then g0 = -Log(g0) / Log(10)
  If g0 >= 10 Then s0 = Format(g0, "0.#") _
    Else s0 = Format(g0, "0.##")
  s0 = "conv: " & s0 & "/"
  Input #1, g1: g1 = -Log(g1) / Log(10)
  List1.List(1) = s0 & Format(g1, "0.#")
  Input #1, g0: Input #1, g1: Input #1, g2
  g0 = g0 + g1 + g2: g_in = g0
  s0 = "solid in: ": n = 2: GoSub tm1a6a
  Input #1, g0: g_out = g0
  's0 = "out: ": n = 4: GoSub tm1a6a 'Remove the "out" item
  List1.List(4) = "" 'Remove the "out" item
  Input #1, g0: Input #1, g1: g_m = g0 + g1
  If g_in > 0 Then g_m = g_m / g_in * 100
  s0 = "melted: " & Format(g_m, "0.#") & "%"
  List1.List(3) = s0
  n = 0:  s0 = "rad in: ": GoSub tm1a6b
  n = 1:  s0 = "e boost: ": GoSub tm1a6b
  n = 2:  s0 = "to solids: ": GoSub tm1a6b
  n = 3:  s0 = "to liquid: ": GoSub tm1a6b
  n = 4:  s0 = "wall loss: ": GoSub tm1a6b
  'List2.List(3) = ""
  'List2.List(4) = ""
  On Error GoTo tm1a6c
  Input #1, s0: Input #1, fp_ut
  For i = 1 To nx: For j = 1 To ny: For k = 1 To nz
    Input #1, fp(i, j, k)
  Next: Next: Next
  
tm1a6c:
  Close (1)
  Kill (case_path & "\gfm0.dat")
  frmMain.Caption = "Running Melt Flow Calculation: Post (Temperature)"
  If Lbdel.Caption = "auto" Then Exit Sub
  Call plt_clr(0)
  If Lbadd.Caption = "plot" Then Lbadd.Caption = "grid"
  Exit Sub
  
tm1a6a:
  If opt11.Checked Then
    g0 = g0 * 95.05
    s0 = s0 & Format(g0, "0.#") & " tpd"
  Else
    s0 = s0 & Format(g0, "0.###") & "kg/s"
  End If
  List1.List(n) = s0
  Return
  
tm1a6b:
  Input #1, g0: g0 = g0 * 0.000001
  'If n = 1 Then
   ' If g_m > 0.95 Then g0 = g0 / g_m * 100
    'If g0 < 0 Then g0 = 0
  'End If
  'If n = 2 Then
    'If g_out > 0.95 * g_in Then g0 = g0 * g_in / g_out
  'End If
  If g0 < 1 Then
    s0 = s0 & Format(g0, "0.###") & "MW"
  ElseIf g0 < 10 Then
    s0 = s0 & Format(g0, "0.##") & "MW"
  ElseIf g0 < 100 Then
    s0 = s0 & Format(g0, "0.#") & "MW"
  Else
    s0 = s0 & Format(g0, "0") & "MW"
  End If
  List2.List(n) = s0
  Return
End Sub

'-------------------------------
'Set flag_grid_enhanced but first make sure the grid has been constructed
'and provide warning to user about the Protect-Grid-Edits option.
'When entering this function:
'   flag_grid_enhanced should be false
'This function is true if the flag has been set, else the function is false.
'-------------------------------
Private Function set_enhanced_grid_edit() As Boolean
    Dim s_tmp As String 'temporary string
    
    If flag_grid_modified = True Then
        'must construct the grid before doing enhancements
        If flow_domain = 1 Then s_tmp = "furnace" Else s_tmp = "melter"
        If MsgBox("Modifications were made to the " & s_tmp & " which are not reflected " _
            & "in the grid.  Look at a newly constructed grid before making the " _
            & "enhanced edit.  Would you like the grid constructed?", _
            vbYesNo + vbCritical, "GFM") = vbYes Then
                Call ctgd_Click
                set_enhanced_grid_edit = False
                Exit Function
        Else
            set_enhanced_grid_edit = False
            Exit Function
        End If
    End If
    
   If MsgBox(Chr(10) & Chr(10) & "IF YOU RECONSTRUCT AN ENHANCED GRID, YOU WILL LOOSE GRID EDITS. " _
        & Chr(10) & Chr(10) & "Note that making an enhanced grid edit automatically causes the " _
        & "Option->Protect-Grid-Edits menu item to be checked." _
        & Chr(10) & Chr(10) & "An enhanced grid edit " _
        & "consists of changing a grid node type or adding, deleting, or moving grid " _
        & "lines.  These edits are destroyed when the grid is reconstucted, so changes " _
        & "that would require grid reconstruction are disabled when the " _
        & "Option->Protect-Grid-Edits menu item is checked." _
        & Chr(10) & Chr(10) & "Be aware that " _
        & "unchecking this option will allow you to destroy your enhanced grid edits.  " _
        & "Are you sure you want to proceed with the enhanced edit at this time?", _
        vbYesNo + vbQuestion, " Warning about Potential Loss of User Work") = vbNo Then
            set_enhanced_grid_edit = False
            Exit Function
    End If
    
    If mnuProtectGrid.Checked = False Then
        'turn on grid protect mode
        mnuProtectGrid.Checked = True
        mnuProtectGrid.Enabled = True
        mnuProtectGrid.Visible = True
        gridProtectMode = True
        ctgeo.Enabled = False
        ctgd.Enabled = False
        opt4.Enabled = False 'disable density changes
        ctex.Enabled = False
        If flow_domain = 1 Then
           ctsw.Enabled = False
        End If
        If menu_layer <> InGridConstruction Then
           Call ct_ls(0, 0, 0, 0) 'clear out the green list boxes if they exist
        End If
        'end of turn on grid protect mode
    End If
    flag_grid_enhanced = True
    grid_type = Is_enhanced
    set_enhanced_grid_edit = True
End Function


'-------------------------------
'Prepare to display welcome screen
'-------------------------------
Private Sub display_welcome_screen()
  Dim i As Integer

  'Set startup screen with labels 1-4, furnace picture, menu bar, and more
  Cls 'Clear the screen
  frmMain.Caption = "Glass Furnace Model Simulator" ' 7-28-2004
  Lb1.AutoSize = True: Lb1.WordWrap = False
  Lb1.ForeColor = vbBlue: Lb1.BackColor = frmMain.BackColor
  Lb1.Left = 3172: Lb1.Top = 500 ' 7-28-2004 centered caption above graphic
  Lb1.Caption = "Glass Furnace Model"
  Lb1.Font.Size = 30: Lb1.Visible = True
  Lb2.AutoSize = True: Lb2.WordWrap = False
  Lb2.ForeColor = vbBlue: Lb2.BackColor = frmMain.BackColor
  Lb2.Left = 3900: Lb2.Top = 7200
  Lb2.Caption = "Argonne National Laboratory"
  Lb2.Font.Size = 15: Lb2.Visible = True
  Lb3.AutoSize = True: Lb3.WordWrap = False
  Lb3.ForeColor = vbBlue: Lb3.BackColor = frmMain.BackColor
  Lb3.Left = 4800: Lb3.Top = 7600
  Lb3.Caption = gfm_version
  Lb3.Font.Size = 12: Lb3.Visible = True
  Image1.Left = 2000: Image1.Top = 1500
  Image1.Width = 7500: Image1.Height = 5000
  Image1.Stretch = True
  fnm = dnm & "documents\furnace.jpg"
  On Error Resume Next
  Image1.Picture = LoadPicture(fnm)
  Image1.Visible = True
  Lb4.AutoSize = True: Lb4.WordWrap = False
  Lb4.BackColor = frmMain.BackColor
  Lb4.Visible = False
  Lbadd.Visible = False: Lbdel.Visible = False
  Lbmv.Visible = False: Lb3d.Visible = False
  Lbfr.Visible = False: Lbto.Visible = False
  Lbnd.Visible = False: Lb12.Visible = False
  Lbsw.Visible = False: Lbsw.Caption = "sweep"
  Lb14.Top = 0: Lb14.Width = 1200
  Lb14.Left = wdts - Lb14.Width: Lb14.Visible = False
  LbCycle.Top = 0: LbCycle.Width = 3255
  LbCycle.Left = Lb14.Left - 250 - LbCycle.Width
  LbCycle.Caption = "": LbCycle.Visible = False
  LbO.Caption = "":  LbP.Caption = "" ' 7-23-2004 remove text from figure handles
  LbO.Width = 120: LbO.Height = 120 ' 7-23-2004 Drawing resize handle size
  LbO.Visible = False: LbO.DragMode = 1
  LbP.Width = LbO.Width: LbP.Height = LbO.Height
  LbP.Visible = False: LbP.DragMode = 1
  LbSoot.Visible = False
  tx_xb.Visible = False: tx_yb.Visible = False
  tx_zb.Visible = False: tx_xe.Visible = False
  tx_ye.Visible = False: tx_ze.Visible = False
  tx_xc.Visible = False: tx_yc.Visible = False
  tx_zc.Visible = False
  Optx.Visible = False: Opty.Visible = False
  Optz.Visible = False
  List1.Visible = False: List2.Visible = False
  List3.Visible = False: List4.Visible = False
  pre0.Visible = True
  sim0.Visible = True
  post0.Visible = True
  mnuCase.Visible = False
  file0.Visible = False
  ct0.Visible = False
  mnuProperties.Visible = False
  opt0.Visible = False
  mnuView.Visible = False
      'turn off grid protect mode
      mnuProtectGrid.Checked = False
      mnuProtectGrid.Enabled = True
      mnuProtectGrid.Visible = True
      gridProtectMode = False
      ctgeo.Enabled = True
      ctgd.Enabled = True
      opt4.Enabled = True 'enable density changes
      ctex.Enabled = True
      If flow_domain = 1 Then
         ctsw.Enabled = True
      End If
      'end of turn off grid protect mode
  mnuCaseDelete.Enabled = False
  mnuCaseDelResults.Enabled = False
  Timer1.Enabled = False
  fpn = ""
  
    LbCase.Visible = False
    LbCaseTitle.Visible = False
    case_number = ""
    case_title = ""
  
    cycleDomains = False 'not doing automatic cycling between simulation domains with combustion first
    cycleDomainsMelt = False 'not doing automatic cycling between simulation domains with melt first
    cycleCombCnt = 0 'count of times combustion simulation has started during auto cycling
    cycleRegen = False 'not doing automatic cycling for regenerative furnace
    cycleComb2Cnt = 0 'count of times second combustion simulation has started
    cycleMeltCnt = 0 'count of times melt simulation has started during auto cycling
    userStoppedSim = False 'User has not canceled or requested simulation to stop
    ListCycle.Visible = False
End Sub
  

'-------------------------------
'Prepare to display screen
'-------------------------------
Private Sub setup(n0 As Integer)
  Dim i As Integer
  If n0 = 1 Then GoTo sus1 'prepare to display the pre-processor screen
  If n0 = 2 Then GoTo sus2 'prepare to display the grid/plot screen
  Exit Sub

sus1:
  'turn off some startup screen objects and put up initial pre-processor screen
  'also used before going to sus2 sometimes
  pre0.Visible = False
  sim0.Visible = False
  post0.Visible = False
  mnuCase.Visible = True
  Lb1.Visible = False
  Lb2.Visible = False
  Lb3.Visible = False
  Image1.Visible = False
  frmMain.Caption = "Pre-Processor: " & title
  theta = 70 / 180 * pi
  p0 = 2000: q0 = 4500
  If flow_domain = 1 Then
    ctbr.Caption = "Burner"
    ctex.Caption = "Exhaust"
    ctsw.Caption = "Dog House"
    cteb.Visible = False
    'ctgp.Visible = False
  ElseIf flow_domain = 2 Then
    ctbr.Caption = "Charger"
    ctex.Caption = "Outlet"
    ctsw.Caption = "Bubbler"
    cteb.Visible = True
    'ctgp.Visible = True
  End If
  Exit Sub
  
sus2:
  'setup for displaying grid/plot info on screen
  If menu_layer = InGridConstruction Then
    mnuProtectGrid.Visible = True
  Else
    mnuProtectGrid.Visible = False
  End If
  Lb1.Visible = False: Lb2.Visible = False
  Lb3.Visible = False: Lb4.Visible = False
  ct0.Visible = False
  mnuProperties.Visible = False
  nxb(1) = 2: nxe(1) = mp
  nxb(2) = 2: nxe(2) = np
  nxb(3) = 2: nxe(3) = lp
  ix0 = Int(mp / 4) * 2: iy0 = Int(np / 4) * 2
  iz0 = Int(lp / 4) * 2
  p0 = 1000: q0 = 7000
  LbO.Visible = True
  If Lb3d.Caption = "2D" Then
    x0 = xg(nxb(1)): y0 = yg(nxb(2)): z0 = zg(nxb(3))
    proj2d
    LbO.Left = p1 - gap: LbO.Top = q1 + gap
  Else
    LbO.Left = p0: LbO.Top = q0
  End If
  'construct grid control block
  Optx.Width = 1000: Opty.Width = 1000: Optz.Width = 1000
  Optx.Height = 250: Opty.Height = 250: Optz.Height = 250
  Optx.Left = 100: Opty.Left = 100: Optz.Left = 100
  Optz.Top = hgts - Optz.Height
  Opty.Top = Optz.Top - Opty.Height
  Optx.Top = Opty.Top - Optx.Height
  Optx.Caption = "x: " & mp
  Opty.Caption = "y: " & np
  Optz.Caption = "z: " & lp
  Optx.Visible = True: Opty.Visible = True
  Optz.Visible = True
  Lbfr.Width = 500: Lbfr.Height = 250
  Lbfr.Left = Optx.Left + Optx.Width
  Lbfr.Top = Optx.Top - Lbfr.Height
  Lbfr.Caption = "from": Lbfr.Visible = True
  tx_xb.Height = Optx.Height: tx_xb.Width = Lbfr.Width
  tx_xb.Left = Lbfr.Left: tx_xb.Top = Optx.Top
  tx_yb.Height = Opty.Height: tx_yb.Width = tx_xb.Width
  tx_yb.Left = tx_xb.Left: tx_yb.Top = Opty.Top
  tx_zb.Height = Optz.Height: tx_zb.Width = tx_xb.Width
  tx_zb.Left = tx_xb.Left: tx_zb.Top = Optz.Top
  tx_xb.Text = nxb(1): tx_xb.Visible = True
  tx_yb.Text = nxb(2): tx_yb.Visible = True
  tx_zb.Text = nxb(3): tx_zb.Visible = True
  Lbto.Top = Lbfr.Top: Lbto.Width = 500
  Lbto.Left = Lbfr.Left + Lbfr.Width + 10
  Lbto.Caption = "to": Lbto.Visible = True
  tx_xe.Height = Optx.Height: tx_xe.Width = Lbto.Width
  tx_xe.Left = Lbto.Left: tx_xe.Top = Optx.Top
  tx_ye.Height = Opty.Height: tx_ye.Width = tx_xe.Width
  tx_ye.Left = tx_xe.Left: tx_ye.Top = Opty.Top
  tx_ze.Height = Optz.Height: tx_ze.Width = tx_xe.Width
  tx_ze.Left = tx_xe.Left: tx_ze.Top = Optz.Top
  tx_xe.Text = nxe(1): tx_xe.Visible = True
  tx_ye.Text = nxe(2): tx_ye.Visible = True
  tx_ze.Text = nxe(3): tx_ze.Visible = True
  Lbnd.Top = Lbfr.Top: Lbnd.Width = 1100
  Lbnd.Left = Lbto.Left + Lbto.Width + 10
  Lbnd.Caption = "current cell": Lbnd.Visible = True
  tx_xc.Left = Lbnd.Left: tx_xc.Top = Optx.Top
  tx_xc.Height = Optx.Height: tx_xc.Width = 500
  tx_yc.Left = tx_xc.Left: tx_yc.Top = Opty.Top
  tx_yc.Height = Opty.Height: tx_yc.Width = tx_xc.Width
  tx_zc.Left = tx_xc.Left: tx_zc.Top = Optz.Top
  tx_zc.Height = Optz.Height: tx_zc.Width = tx_xc.Width
  tx_xc.Text = ix0: tx_xc.Visible = True
  tx_yc.Text = iy0: tx_yc.Visible = True
  tx_zc.Text = iz0: tx_zc.Visible = True
  Lb12.Width = 600: Lb12.Height = 300
  Lb12.Top = tx_xc.Top
  Lb12.Left = tx_xc.Left + tx_xc.Width
  Lb12.Visible = True
  Lbsw.Top = Lbnd.Top
  Lbsw.Left = Lbnd.Left + Lbnd.Width + 10
  If InStr(frmMain.Caption, "Pre") > 0 Then
    frmMain.Caption = "Pre-Processor: grid"
    Lbadd.Caption = "add": Lbsw.Width = 600
    Lbmv.Caption = "move": Lbdel.Caption = "del"
    'glassExitTemp = default_glassExitTemp
  ElseIf InStr(frmMain.Caption, "Post") > 0 Then
    frmMain.Caption = "Post-Processor: grid"
    Lbsw.Width = 1000
    Lbadd.Caption = "grid"
    Lbdel.Caption = "vectors"
    Lbmv.Caption = "b/w"
    opt4.Visible = False
    opt0.Visible = True ' 10-8-2004
    opt0.Enabled = True ' 10-8-2004
  End If
  Lbsw.Visible = True
  Lbadd.Top = Optx.Top: Lbadd.Width = Lbsw.Width
  Lbadd.Left = Lbsw.Left: Lbadd.Visible = True
  Lbdel.Width = Lbadd.Width
  Lbdel.Top = Opty.Top: Lbdel.Left = Lbadd.Left
  Lbdel.Visible = True
  Lbmv.Top = Optz.Top: Lbmv.Left = Lbadd.Left
  Lbmv.Width = Lbadd.Width
  Lbmv.Visible = True
  Lb14.Visible = True
  Lb3d.Width = 600: Lb3d.Top = Lbmv.Top
  Lb3d.Left = Lbmv.Left - Lb3d.Width - 20
  Lb3d.Visible = True
  'List1.Height = 1500: List1.Width = 1600
  List1.Height = 1500: List1.Width = 1800
  List1.Top = hgts - List1.Height
  List1.Left = wdts - 100 - List1.Width
  List1.Visible = True
  List2.Visible = False
End Sub

'-------------------------------
'Open and read grid file
'Input fnm_gd_path = grid file path and name
'-------------------------------
Private Sub read_grid()
  Dim g As String
  Dim m1 As Integer
  Dim ndp0, npt0, nsp0, nr0 As Integer 'dummies in GUI but read in CFD comb (not in melt)
  
  Open fnm_gd_path For Input As #10
  Line Input #10, g:  Line Input #10, g 'skip title & r0 lines
  'get grid sizes and grid enhancement type
  Input #10, g0, mp, np, lp, surf_length_c, surf_width_c, ndp0, npt0, nsp0, nr0, grid_type
  
  nx = mp / 2: ny = np / 2: nz = lp / 2 'set half sizes
  ReDim xg(mp + 1), yg(np + 1), zg(lp + 1)
  ReDim ibc(mp, np, lp)
  eps0 = 1
  m1 = mp: GoSub rgs1 'get x face distances
  For i = 3 To mp - 1: xg(i) = xg1(i): Next
  m1 = np: GoSub rgs1 'get y face distances
  For i = 3 To np - 1: yg(i) = xg1(i): Next
  m1 = lp: GoSub rgs1 'get z face distances
  For i = 3 To lp - 1: zg(i) = xg1(i): Next
  'Fill out cell center distances
  '   Question:  @@@
  'eps0 may be set inside subroutine.  So should these lines interleave lines
  'above, with re-init of eps0 each time?
  xg(1) = xg(3) - eps0: xg(mp + 1) = xg(mp - 1) + eps0
  For i = 2 To mp Step 2
  xg(i) = (xg(i + 1) + xg(i - 1)) / 2: Next
  yg(1) = yg(3) - eps0: yg(np + 1) = yg(np - 1) + eps0
  For i = 2 To np Step 2
  yg(i) = (yg(i + 1) + yg(i - 1)) / 2: Next
  zg(1) = zg(3) - eps0: zg(lp + 1) = zg(lp - 1) + eps0
  For i = 2 To lp Step 2
  zg(i) = (zg(i + 1) + zg(i - 1)) / 2: Next
  j = 0: Do While j <= 0
    Line Input #10, g: j = InStr(1, g, "cell") 'position file to after line with 'cell'
  Loop
  k1 = 0: k2 = 0
  
rg2: 'get grid map values
  Input #10, k2
  i1 = 0: i2 = 0
  Do While i2 < mp
    Line Input #10, g: i2 = Val(Mid(g, 1, 4))
    For k = k1 + 2 To k2 Step 2
    For j = 2 To np Step 2
      j1 = j / 2 + 6: h = Val(Mid(g, j1, 1))
      For i = i1 + 2 To i2 Step 2
        ibc(i, j, k) = h: Next i
    Next j: Next k
  i1 = i2: Loop
  k1 = k2: If k2 < lp Then GoTo rg2
  Close (10)
  flag_grid_active = True
  Exit Sub
  
rgs1: 'Get cell face distances one direction at a time
  ReDim xg1(m1 + 1)
  j = 0: Do While j <= 0
    Line Input #10, g: j = InStr(1, g, "grid")
  Loop
  i1 = 3: Input #10, i2, h
  For ii = i1 To i2 Step 2: xg1(ii) = h: Next
  Do While i2 < m1 - 1
    i1 = i2: Input #10, i2, h
    xg1(i2) = h
    del = (h - xg1(i1)) / (i2 - i1) * 2
    If eps0 > del Then eps0 = del
    If i2 > i1 + 2 Then
      For ii = i2 - 2 To i1 + 2 Step -2
        xg1(ii) = xg1(ii + 2) - del
    Next: End If
  Loop
  Return
End Sub


'-------------------------------
'Plot the grid with colored output in Post Processor or Simulator
'-------------------------------
Private Sub plt_clr(n_p As Integer)
  Dim i As Integer, j As Integer, k As Integer
  Dim m As Integer, n As Integer, n0 As Integer
  Dim m0_cr As Integer
  Dim d() As Single
  m2_cr = 100: m1_cr = 0: m0_cr = m2_cr - m1_cr
  i1 = nxb(1) - 1: i2 = nxe(1) + 1
  j1 = nxb(2) - 1: j2 = nxe(2) + 1
  k1 = nxb(3) - 1: k2 = nxe(3) + 1
  dx = (xg(i2) - xg(i1)) * zf1
  dy = (yg(j2) - yg(j1)) * zf2
  dz = (zg(k2) - zg(k1)) * zf3
  If n_p = 0 Or Lb3d.Caption = "2D" Then
    Cls
  Else
    rgb0 = frmMain.BackColor
    p1 = 0: p2 = p01 - 10
    q1 = 0: q2 = hgts
    Line (p1, q1)-(p2, q2), rgb0, BF
    p1 = p2: p2 = wdts - 50
    q1 = q01 + 10
    Line (p1, q1)-(p2, q2), rgb0, BF
  End If
  Call axes_plt
  If Lb3d.Caption = "2D" Then
    GoSub clr4
  Else
    LbP.Left = p01 + dx + gap: LbP.Top = q02
  End If
  If n_p = 0 Or Lb3d.Caption = "2D" Then GoSub clr0
  If n_p = 3 Or n_p = 0 Then GoSub clr3
  If n_p = 1 Or n_p = 0 Then GoSub clr1
  If n_p = 2 Or n_p = 0 Then GoSub clr2
  g0 = fp(ix0 / 2, iy0 / 2, iz0 / 2)
  s0 = fp_ut: Call ucv
  If g0 > 10 Then
    Lb12 = Format(g0, "0")
  Else
    Lb12 = Format(g0, "0e-0")
  End If
  If Lb3d.Caption = "2D" Then GoSub clr4a
Exit Sub

clr0:
  p1 = p02 + gap: q1 = q02
  dq = dz / (m0_cr + 1)
  p2 = p1 + 100
  For i = m1_cr To m2_cr
    q2 = q1 - dq
    Call color0(i)
    Line (p1, q1)-(p2, q2), rgb0, BF
    q1 = q2
  Next
  p1 = p2: p2 = p1 + 50
  q1 = q02
  Line (p1, q1)-(p2, q1)
  CurrentX = p2 + 100: CurrentY = q1 - 100
  Font.Size = 8
  g0 = fp_mn: s0 = fp_ut: GoSub clr0a: Print s1
  dq = (q1 - q2) / 4: fp_d = (fp_mx - fp_mn) / 4
  For i = 1 To 4
    q1 = q1 - dq
    Line (p1, q1)-(p2, q1)
    CurrentX = p2 + 100: CurrentY = q1 - 100
    g0 = fp_mn + i * fp_d: s0 = fp_ut: GoSub clr0a
    If i = 4 Then s1 = s1 & s0
    Print s1
  Next
  Return
  
clr0a:
  Call ucv
  g1 = Abs(g0)
  If g1 > 10000 Then
    s1 = Format(g0, "#.##E-##")
  ElseIf g1 > 1 Then
    s1 = Format(g0, "####")
  ElseIf g1 > 0.01 Then
    s1 = Format(g0, "#.###")
  ElseIf g1 = 0 Then
    s1 = "0"
  Else
    s1 = Format(g0, "#.##E-##")
  End If
  Return
  
clr1:
  ReDim d(np + 1, lp + 1)
  id2 = ix0 / 2
  For j = 2 To np Step 2
  jd2 = j / 2
  For k = 2 To lp Step 2
    d(j, k) = fp(id2, jd2, k / 2)
  Next: Next
  For j = 3 To np - 1 Step 2
    g0 = yg(j + 1) - yg(j - 1)
    If g0 <= 0 Then
      g1 = 1
    Else
      g1 = (yg(j + 1) - yg(j)) / g0
    End If
    g2 = 1 - g1
    For k = 2 To lp Step 2
      d(j, k) = g1 * d(j - 1, k) + g2 * d(j + 1, k)
    Next
  Next
  For k = 3 To lp - 1 Step 2
    g0 = zg(k + 1) - zg(k - 1)
    If g0 <= 0 Then
      g1 = 1
    Else
      g1 = (zg(k + 1) - zg(k)) / g0
    End If
    g2 = 1 - g1
    For j = 2 To np
      d(j, k) = g1 * d(j, k - 1) + g2 * d(j, k + 1)
    Next
  Next
  For k = 2 To lp
    d(1, k) = d(2, k): d(np + 1, k) = d(np, k)
  Next
  For j = 2 To np
    d(j, 1) = d(j, 2): d(j, lp + 1) = d(j, lp)
  Next
  If Lb3d.Caption = "2D" Then GoTo clr1c
  p1 = p02
  For k = k1 To k2 - 2 Step 2
    p2 = p02 + (zg(k + 2) - zg(k1)) * zf3
    If p2 - p1 > 1 Then GoSub clr1a
  Next
  Return
  
clr1a:
  q1 = q01
  For j = j1 To j2 - 2 Step 2
    q2 = q01 - (yg(j + 2) - yg(j1)) * zf2
    If q1 - q2 > 1 Then GoSub clr1b
  Next
  p1 = p2
  frmMain.Refresh
  Return
  
clr1b:
  If ibc(ix0, j + 1, k + 1) = 1 Then
    Line (p1, q1)-(p2, q2), vbBlack, BF
  Else
    d_00 = d(j, k)
    d_01 = d(j, k + 2)
    d_10 = d(j + 2, k)
    d_11 = d(j + 2, k + 2)
    GoSub clr_plt
  End If
  q1 = q2
  Return
  
clr1c:
  If Not Optx Then Return
  i = ix0: x0 = xg(ix0): Y1 = yg(j1)
  For j = j1 + 2 To j2 Step 2
    z1 = zg(k1): Y2 = yg(j)
    For k = k1 + 2 To k2 Step 2
      z2 = zg(k)
      y0 = Y1: z0 = z1: proj2d: p11 = p1: q11 = q1
      y0 = Y2: z0 = z1: proj2d: p21 = p1: q21 = q1
      y0 = Y1: z0 = z2: proj2d: p12 = p1: q12 = q1
      y0 = Y2: z0 = z2: proj2d: p22 = p1: q22 = q1
      If ibc(i, j - 1, k - 1) = 1 Then
        Call blknode
      Else
        d_00 = d(j - 2, k - 2)
        d_10 = d(j, k - 2)
        d_01 = d(j - 2, k)
        d_11 = d(j, k)
        Call clrnode
      End If
      z1 = z2
    Next
    Y1 = Y2
    If Not sweep Then Refresh
  Next
  Return
  
clr2:
  ReDim d(mp + 1, lp + 1)
  jd2 = iy0 / 2
  For i = 2 To mp Step 2
  id2 = i / 2
  For k = 2 To lp Step 2
    d(i, k) = fp(id2, jd2, k / 2)
  Next: Next
  For i = 3 To mp - 1 Step 2
    g0 = xg(i + 1) - xg(i - 1)
    If g0 <= 0 Then
      g1 = 1
    Else
      g1 = (xg(i + 1) - xg(i)) / g0
    End If
    g2 = 1 - g1
    For k = 2 To lp Step 2
      d(i, k) = g1 * d(i - 1, k) + g2 * d(i + 1, k)
    Next
  Next
  For k = 3 To lp - 1 Step 2
    g0 = zg(k + 1) - zg(k - 1)
    If g0 <= 0 Then
      g1 = 1
    Else
      g1 = (zg(k + 1) - zg(k)) / g0
    End If
    g2 = 1 - g1
    For i = 2 To mp
      d(i, k) = g1 * d(i, k - 1) + g2 * d(i, k + 1)
    Next
  Next
  For k = 2 To lp
    d(1, k) = d(2, k): d(mp + 1, k) = d(mp, k)
  Next
  For i = 2 To mp
    d(i, 1) = d(i, 2): d(i, lp + 1) = d(i, lp)
  Next
  If Lb3d.Caption = "2D" Then GoTo clr2c
  p1 = p01
  For i = i1 To i2 - 2 Step 2
    p2 = p01 + (xg(i + 2) - xg(i1)) * zf1
    If p2 - p1 > 1 Then GoSub clr2a
  Next
  Return
  
clr2a:
  q1 = q02
  For k = k1 To k2 - 2 Step 2
    q2 = q02 - (zg(k + 2) - zg(k1)) * zf3
    If q1 - q2 > 1 Then GoSub clr2b
  Next
  q2 = g1 - 20
  Line (p1, q1)-(p2, q2), vbWhite, BF
  p1 = p2
  Refresh
  Return
  
clr2b:
  If ibc(i + 1, iy0, k + 1) = 1 Then
    Line (p1, q1)-(p2, q2), vbBlack, BF
  Else
    d_00 = d(i, k)
    d_01 = d(i + 2, k)
    d_10 = d(i, k + 2)
    d_11 = d(i + 2, k + 2)
    GoSub clr_plt
  End If
  q1 = q2
  Return
  
clr2c:
  If Not Opty Then Return
  j = iy0: y0 = yg(iy0): X1 = xg(i1)
  For i = i1 + 2 To i2 Step 2
    z1 = zg(k1): X2 = xg(i)
    For k = k1 + 2 To k2 Step 2
      z2 = zg(k)
      x0 = X1: z0 = z1: proj2d: p11 = p1: q11 = q1
      x0 = X2: z0 = z1: proj2d: p21 = p1: q21 = q1
      x0 = X1: z0 = z2: proj2d: p12 = p1: q12 = q1
      x0 = X2: z0 = z2: proj2d: p22 = p1: q22 = q1
      If ibc(i - 1, j, k - 1) = 1 Then
        Call blknode
      Else
        d_00 = d(i - 2, k - 2)
        d_10 = d(i, k - 2)
        d_01 = d(i - 2, k)
        d_11 = d(i, k)
        Call clrnode
      End If
      z1 = z2
    Next
    X1 = X2
    If Not sweep Then Refresh
  Next
  Return
  
clr3:
  ReDim d(mp + 1, np + 1)
  kd2 = iz0 / 2
  For i = 2 To mp Step 2
  id2 = i / 2
  For j = 2 To np Step 2
    jd2 = j / 2: d(i, j) = fp(id2, jd2, kd2)
  Next: Next
  For i = 2 To mp Step 2
  id2 = i / 2
  For j = 2 To np Step 2
    jd2 = j / 2
    If ibc(i, j, iz0) = 0 Then
      If ibc(i - 2, j, iz0) = 1 And ibc(i, j - 2, iz0) = 1 _
      And ibc(i - 2, j - 2, iz0) = 1 Then _
        d(i - 2, j - 2) = (d(i - 2, j) + d(i, j - 2)) / 2
      If ibc(i - 2, j, iz0) = 1 And ibc(i, j + 2, iz0) = 1 _
      And ibc(i - 2, j + 2, iz0) = 1 Then _
        d(i - 2, j + 2) = (d(i - 2, j) + d(i, j + 2)) / 2
      If ibc(i + 2, j, iz0) = 1 And ibc(i, j - 2, iz0) = 1 _
      And ibc(i + 2, j - 2, iz0) = 1 Then _
        d(i + 2, j - 2) = (d(i + 2, j) + d(i, j - 2)) / 2
      If ibc(i + 2, j, iz0) = 1 And ibc(i, j + 2, iz0) = 1 _
      And ibc(i + 2, j + 2, iz0) = 1 Then _
        d(i + 2, j + 2) = (d(i + 2, j) + d(i, j + 2)) / 2
    End If
  Next: Next
  For i = 3 To mp - 1 Step 2
    g0 = xg(i + 1) - xg(i - 1)
    If g0 <= 0 Then
      g1 = 1
    Else
      g1 = (xg(i + 1) - xg(i)) / g0
    End If
    g2 = 1 - g1
    For j = 2 To np Step 2
      d(i, j) = g1 * d(i - 1, j) + g2 * d(i + 1, j)
    Next
  Next
  For j = 3 To np - 1 Step 2
    g0 = yg(j + 1) - yg(j - 1)
    If g0 <= 0 Then
      g1 = 1
    Else
      g1 = (yg(j + 1) - yg(j)) / g0
    End If
    g2 = 1 - g1
    For i = 2 To mp
      d(i, j) = g1 * d(i, j - 1) + g2 * d(i, j + 1)
    Next
  Next
  For j = 2 To np
    d(1, j) = d(2, j): d(mp + 1, j) = d(mp, j)
  Next
  For i = 2 To mp
    d(i, 1) = d(i, 2): d(i, np + 1) = d(i, np)
  Next
  If Lb3d.Caption = "2D" Then GoTo clr3c
  p1 = p01
  For i = i1 To i2 - 2 Step 2
    p2 = p01 + (xg(i + 2) - xg(i1)) * zf1
    If p2 - p1 > 1 Then GoSub clr3a
  Next
  Return
  
clr3a:
  q1 = q01
  For j = j1 To j2 - 2 Step 2
    q2 = q01 - (yg(j + 2) - yg(j1)) * zf2
    If q1 - q2 > 1 Then GoSub clr3b
  Next
  p1 = p2
  Refresh
  Return
  
clr3b:
  If ibc(i + 1, j + 1, iz0) = 1 Then
    Line (p1, q1)-(p2, q2), vbBlack, BF
  Else
    d_00 = d(i, j)
    d_01 = d(i + 2, j)
    d_10 = d(i, j + 2)
    d_11 = d(i + 2, j + 2)
    GoSub clr_plt
  End If
  q1 = q2
  Return
  
clr3c:
  If Not Optz Then Return
  k = iz0: z0 = zg(iz0): X1 = xg(i1)
  For i = i1 + 2 To i2 Step 2
    Y1 = yg(j1): X2 = xg(i)
    For j = j1 + 2 To j2 Step 2
      Y2 = yg(j)
      x0 = X1: y0 = Y1: proj2d: p11 = p1: q11 = q1
      x0 = X2: y0 = Y1: proj2d: p21 = p1: q21 = q1
      x0 = X1: y0 = Y2: proj2d: p12 = p1: q12 = q1
      x0 = X2: y0 = Y2: proj2d: p22 = p1: q22 = q1
      If ibc(i - 1, j - 1, k) = 1 Then
        Call blknode
      Else
        d_00 = d(i - 2, j - 2)
        d_10 = d(i, j - 2)
        d_01 = d(i - 2, j)
        d_11 = d(i, j)
        Call clrnode
      End If
      Y1 = Y2
    Next
    X1 = X2
    If Not sweep Then Refresh
  Next
  Return
  
clr_plt:
  dp0 = p2 - p1: dq0 = q2 - q1
  If dp0 < 1 Or dq0 > -1 Then Return
  c0 = d_00
  c1 = (d_10 - d_00) / dq0
  c2 = (d_01 - d_00) / dp0
  c3 = (d_00 + d_11 - d_01 - d_10) / dp0 / dq0
  g0 = (fp_mx - fp_mn)
  If g0 <= 0 Then
    mm = 1: nn = 1
  Else
    n_00 = (d_00 - fp_mn) / g0 * m0_cr + m1_cr
    n_01 = (d_01 - fp_mn) / g0 * m0_cr + m1_cr
    n_10 = (d_10 - fp_mn) / g0 * m0_cr + m1_cr
    n_11 = (d_11 - fp_mn) / g0 * m0_cr + m1_cr
    mm1 = n_00: mm2 = n_00
    If n_01 > mm2 Then mm2 = n_01
    If n_10 > mm2 Then mm2 = n_10
    If n_11 > mm2 Then mm2 = n_11
    If n_01 < mm1 Then mm1 = n_01
    If n_10 < mm1 Then mm1 = n_10
    If n_11 < mm1 Then mm1 = n_11
    mm1 = Int(mm2 - mm1 + 1)
    mm = Int(dp0 / vwr) + 1
    nn = Int(-dq0 / vwr) + 1
    If mm > mm1 Then mm = mm1
    If nn > mm1 Then nn = mm1
  End If
  dpm = dp0 / mm: If dpm < 10 Then dpm = 10
  dqn = dq0 / nn
  pm1 = p1: pm2 = p1
  Do While pm2 < p2
    pm2 = pm1 + dpm
    If pm2 > p2 Then pm2 = p2
    pm = (pm1 + pm2) / 2
    qn1 = q1
    Do While qn1 > q2
      qn2 = qn1 + dqn
      qn = (qn1 + qn2) / 2
      pj = pm - p1
      qi = qn - q1
      dd = c0 + c1 * qi + c2 * pj + c3 * pj * qi
      If fp_mx <= fp_mn Then
        g0 = 0
      Else
        g0 = (dd - fp_mn) / (fp_mx - fp_mn)
        If g0 > 1 Then g0 = 1
        If g0 < 0 Then g0 = 0
      End If
      n0 = Int(g0 * m0_cr) + m1_cr
      Call color0(n0)
      Line (pm1, qn1)-(pm2, qn2), rgb0, BF
      qn1 = qn2
    Loop: pm1 = pm2
  Loop
Return

clr4:
  ForeColor = RGB(200, 200, 200)
  x0 = xg(i1): y0 = yg(j2): z0 = zg(k1): proj2d
  p2 = p1: q2 = q1
  y0 = yg(j1): proj2d: Line (p2, q2)-(p1, q1)
  x0 = xg(i2): y0 = yg(j2): proj2d: Line (p2, q2)-(p1, q1)
  x0 = xg(i1): z0 = zg(k2): proj2d: Line (p2, q2)-(p1, q1)
  p2 = p1: q2 = q1
  y0 = yg(j1): proj2d: Line (p2, q2)-(p1, q1)
  x0 = xg(i2): y0 = yg(j2): proj2d: Line (p2, q2)-(p1, q1)
  LbP.Left = p1 + gap: LbP.Top = q1 - gap
  z0 = zg(k1): proj2d: Line -(p1, q1)
  p02 = p1: q02 = q1
  ForeColor = vbBlack
Return

clr4a:
  ForeColor = RGB(200, 200, 200)
  x0 = xg(i2): y0 = yg(j1): z0 = zg(k2): proj2d
  p2 = p1: q2 = q1
  x0 = xg(i1): proj2d: Line (p2, q2)-(p1, q1)
  x0 = xg(i2): y0 = yg(j2): proj2d: Line (p2, q2)-(p1, q1)
  y0 = yg(j1): z0 = zg(k1): proj2d: Line (p2, q2)-(p1, q1)
  ForeColor = vbBlack
Return
End Sub

'-------------------------------
'-------------------------------
Private Sub color0(n As Integer)
  If n < 0 Then
    rgb0 = RGB(0, 0, 0): Exit Sub
  End If
  If Lbmv.Caption = "color" Then GoTo color0a
'blue
  If n <= 25 Then
    n1 = n * 10
    rgb0 = RGB(0, n1, 250)
'cyan
  ElseIf n <= 50 Then
    If n <= 40 Then n1 = 250 - (n - 25) * 5 _
    Else: n1 = 175 - (n - 40) * 17.5
    rgb0 = RGB(0, 250, n1)
'green
  ElseIf n <= 75 Then
    If n <= 60 Then n1 = (n - 50) * 17.5 _
    Else: n1 = 175 + (n - 60) * 5
    rgb0 = RGB(n1, 250, 0)
'yellow
  ElseIf n <= 100 Then
    n1 = 250 - (n - 75) * 10
    rgb0 = RGB(250, n1, 0)
'red
  Else
    rgb0 = RGB(255, 255, 255)
  End If
  Exit Sub
color0a:
  n1 = 10 - Int(n / 10)
  n1 = n1 * 20 + 20
  rgb0 = RGB(n1, n1, n1)
End Sub

'-------------------------------
'-------------------------------
Private Sub ucvb()
  'Do unit conversion from internal storage to user display
  'opt12.Checked=True when units are SI which is the internal representation
  'opt11.Checked=True when units are British
  'input s0=unit name, g0=value in SI format
  'output s0=unit name, g0=value in user display format (SI or British)

  If opt12.Checked Then Exit Sub 'no need to convert if user display is SI format
  If s0 = "F" Then
    g0 = (g0 + 460) / 1.8
    s0 = "K"
  ElseIf s0 = "psi" Then
    g0 = g0 * P_ref / 14.6959488
    s0 = "Pa"
  ElseIf s0 = "psi-s" Then
    g0 = g0 * P_ref / 14.6959488
    s0 = "Pa-s"
  ElseIf s0 = "ft/s" Then
    g0 = g0 * 0.3048
    s0 = "m/s"
  ElseIf s0 = "lb/hr" Then
    g0 = g0 / 2.20462262 / 3600
    s0 = "kg/s"
  ElseIf s0 = "lb/ft^3" Then
    s0 = "kg/m^3"
    g0 = g0 / 2.20462262 / 0.3048 ^ 3
  ElseIf s0 = "lb/s/ft^2" Then
    s0 = "kg/s/m^2"
    g0 = g0 / 2.20462262 / 0.3048 ^ 2
  ElseIf s0 = "Btu/lb" Then
    s0 = "J/kg": g0 = g0 * 1055.05585 * 2.20462262
  ElseIf s0 = "Btu/lb/R" Then
    s0 = "J/kg/K": g0 = g0 * 1055.05585 * 2.20462262 * 1.8
  ElseIf s0 = "Btu/ft^2" Then
    s0 = "J/m^2": g0 = g0 * 1055.05585 / 0.3048 ^ 2
  ElseIf s0 = "Btu/ft^3" Then
    s0 = "J/m^3": g0 = g0 * 1055.05585 / 0.3048 ^ 3
  ElseIf s0 = "SCFH" Then
    s0 = "m^3/s": g0 = g0 / 3600 * 0.3048 ^ 3
  ElseIf s0 = "W/ft^2" Then
    s0 = "W/m^2": g0 = g0 / 0.3048 ^ 2
  ElseIf s0 = "W/ft^3" Then
    s0 = "W/m^3": g0 = g0 / 0.3048 ^ 3
  ElseIf s0 = "ft" Then
    s0 = "m": g0 = g0 * 0.3048
  ElseIf s0 = "Btu/ft^2/F/s" Then
    s0 = "W/m^2/K": g0 = g0 * 1055.05585 / 0.3048 ^ 2 * 1.8
  ElseIf s0 = "MMBtu/hr" Then
    s0 = "MW": g0 = g0 * 1055.05585 / 3600
  ElseIf s0 = "Btu/ft/F/s" Then
    s0 = "W/m/K": g0 = g0 * 1055.05585 / 0.3048 * 1.8
  End If
'  If g0 > 10000 Then
'    g0 = Format(g0, "0.####E+")
'  ElseIf g0 > 100 Then
'    g0 = Format(g0, "0")
'  ElseIf g0 > 0.1 Then
'    g0 = Format(g0, "0.####")
'  Else
'    g0 = Format(g0, "0.####E-")
'  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub ucv()
  'Do unit conversion from user display to storage/internal use
  'opt12.Checked=True when units are SI which is the internal representation
  'opt11.Checked=True when units are British
  'input s0=unit name, g0=value in display format (SI or British)
  'output s0=unit name in SI format, g0=value in SI format
  If opt12.Checked Then Exit Sub 'no need to convert if already in SI format
  If s0 = "K" Then
    g0 = g0 * 1.8 - 460
    s0 = "F"
  ElseIf s0 = "Pa" Then
    g0 = g0 / P_ref * 14.6959488
    s0 = "psi"
  ElseIf s0 = "Pa-s" Then
    g0 = g0 / P_ref * 14.6959488
    s0 = "psi-s"
  ElseIf s0 = "m/s" Then
    g0 = g0 / 0.3048
    s0 = "ft/s"
  ElseIf s0 = "kg/s" Then
    g0 = g0 * 2.20462262 * 3600
    s0 = "lb/hr"
  ElseIf s0 = "kg/m^3" Then
    s0 = "lb/ft^3"
    g0 = g0 * 2.20462262 * 0.3048 ^ 3
  ElseIf s0 = "kg/s/m^2" Then
    s0 = "lb/s/ft^2"
    g0 = g0 * 2.20462262 * 0.3048 ^ 2
  ElseIf s0 = "J/kg" Then
    s0 = "Btu/lb": g0 = g0 / 1055.05585 / 2.20462262
  ElseIf s0 = "J/kg/K" Then
    s0 = "Btu/lb/R": g0 = g0 / 1055.05585 / 2.20462262 / 1.8
  ElseIf s0 = "J/m^2" Then
    s0 = "Btu/ft^2": g0 = g0 / 1055.05585 * 0.3048 ^ 2
  ElseIf s0 = "J/m^3" Then
    s0 = "Btu/ft^3": g0 = g0 / 1055.05585 * 0.3048 ^ 3
  ElseIf s0 = "m^3/s" Then
    s0 = "SCFH": g0 = g0 * 3600 / 0.3048 ^ 3
  ElseIf s0 = "W/m^2" Then
    s0 = "W/ft^2": g0 = g0 * 0.3048 ^ 2
  ElseIf s0 = "W/m^3" Then
    s0 = "W/ft^3": g0 = g0 * 0.3048 ^ 3
  ElseIf s0 = "m" Then
    s0 = "ft": g0 = g0 / 0.3048
  ElseIf s0 = "W/m/K" Then
    s0 = "Btu/ft/F/s": g0 = g0 / 1055.05585 * 0.3048 / 1.8
  ElseIf s0 = "W/m^2/K" Then
    s0 = "Btu/ft^2/F/s": g0 = g0 / 1055.05585 * 0.3048 ^ 2 / 1.8
  ElseIf s0 = "MW" Then
    s0 = "MMBtu/hr": g0 = g0 / 1055.05585 * 3600
  End If
  If g0 > 10000 Then
    g0 = Format(g0, "0.####E+")
  ElseIf g0 > 100 Then
    g0 = Format(g0, "0")
  ElseIf g0 > 0.1 Then
    g0 = Format(g0, "0.####")
  Else
    g0 = Format(g0, "0.####E-")
  End If
End Sub

'-------------------------------
'-------------------------------
Private Sub clrnode()
  Dim dp As Integer, dq As Integer, n_p As Integer
  Dim f1p As Single, f2p As Single
  Dim f1q As Single, f2q As Single
  dp = p21 - p11: dq = q11 - q12
  If dp <= 1 Or dq <= 1 Then Exit Sub
  g0 = (m2_cr - m1_cr) / (fp_mx - fp_mn)
  c11 = (d_00 - fp_mn) * g0 + m1_cr
  c21 = (d_10 - fp_mn) * g0 + m1_cr
  c12 = (d_01 - fp_mn) * g0 + m1_cr
  c22 = (d_11 - fp_mn) * g0 + m1_cr
  nn0 = Int(dq / vwr) + 1
  mm = Int(dp / vwr) + 1
  dpm = dp / mm: p = p11
  Do While p < p21
    f1p = (p21 - p) / dp: f2p = 1 - f1p
    c1 = c11 * f1p + c21 * f2p
    c2 = c12 * f1p + c22 * f2p
    nn = Int(Abs(c1 - c2) + 1)
    If nn > nn0 Then nn = nn0
    dqn = dq / nn: q = q11
    Do While q > q12
      f1q = (q - q12) / dq: f2q = 1 - f1q
      n_p = c1 * f1q + c2 * f2q
      If n_p > m2_cr Then n_p = m2_cr
      If n_p < m1_cr Then n_p = m1_cr
      color0 (n_p)
      p1 = p11 * f1p + p21 * f2p + f2q * (p12 - p11)
      q1 = q11 * f1q + q12 * f2q + f2p * (q21 - q11)
      p2 = p1 + dpm: q2 = q1 - dqn
      Line (p1, q1)-(p2, q2), rgb0, BF
    q = q - dqn: Loop
  p = p + dpm: Loop
End Sub

'-------------------------------
'-------------------------------
Private Sub blknode()
  Dim dp As Integer, dq As Integer
  Dim f1 As Single, f2 As Single
  dp = p21 - p11: dq = q11 - q12
  If dp <= 0 Or dq <= 0 Then Exit Sub
  For q = q11 To q12 Step -10
    f1 = (q - q12) / dq: f2 = 1 - f1
    q1 = f1 * q11 + f2 * q12
    q2 = f1 * q21 + f2 * q22
    p1 = f1 * p11 + f2 * p12
    p2 = f1 * p21 + f2 * p22
    Line (p1, q1)-(p2, q2), vbBlack
  Next
End Sub



'-------------------------------
'-------------------------------
Private Sub get_filenames_into_array(path_n_pattern As String)
   Dim ffd As WIN32_FIND_DATA
   Dim h As Long
   Dim newfile As String
   'h = FindFirstFile("C:\*.*", ffd)
   h = FindFirstFile(path_n_pattern, ffd)
   If h <> -1 Then  ' we found something
      ReDim filename_array(0) 'note that entry 0 will be null
      Do
         newfile = trim_nulls(ffd.cFileName)
         If (ffd.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY) Or _
            newfile = "." Or newfile = ".." Then
         'If (ffd.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY) Then
            'Just skip directories for now
            'res = MsgBox("Directory: " & trim_nulls(ffd.cFileName))
         Else
            'res = MsgBox("File: " & trim_nulls(ffd.cFileName))
         
            filename_array_bound = UBound(filename_array) + 1
            ReDim Preserve filename_array(filename_array_bound)
            filename_array(filename_array_bound) = newfile
         End If
      Loop While FindNextFile(h, ffd)
   End If
End Sub


Function FileExist(sTestFile As String) As Boolean
   'This function does not use DIR since it is possible that you might have
   'been in the middle of running DIR against another directory in
   'an attempt to match one directory against another.

   'This function does not handle wildcard characters
   Dim lSize As Long
   On Error Resume Next
   'Preset length to -1 because files can be zero bytes in length
   lSize = -1
   'Get the length of the file
   lSize = FileLen(sTestFile)
   If lSize > -1 Then
      FileExist = True
   Else
      FileExist = False
   End If
End Function





