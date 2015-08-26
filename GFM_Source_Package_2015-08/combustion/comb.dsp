# Microsoft Developer Studio Project File - Name="comb" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=comb - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "comb.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "comb.mak" CFG="comb - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "comb - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "comb - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "comb - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "comb - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /check:overflow /check:underflow
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "comb - Win32 Release"
# Name "comb - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\Code\A0main.f90
DEP_F90_A0MAI=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Adlbl3.f90
DEP_F90_ADLBL=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Array_dif.f90
DEP_F90_ARRAY=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Deva.f90
DEP_F90_DEVA_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Dflow.f90
DEP_F90_DFLOW=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Dflow2.f90
DEP_F90_DFLOW2=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Dkel.f90
DEP_F90_DKEL_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Extr.f90
DEP_F90_EXTR_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Flx1d.f90
DEP_F90_FLX1D=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Gflow.f90
DEP_F90_GFLOW=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Grid3.f90
DEP_F90_GRID3=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Gslv3g.f90
DEP_F90_GSLV3=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Gslv3m.f90
DEP_F90_GSLV3M=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Gslv3v.f90
DEP_F90_GSLV3V=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Gslv3x.f90
DEP_F90_GSLV3X=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Hprint.f90
DEP_F90_HPRIN=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Initsv.f90
DEP_F90_INITS=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Intp.f90
DEP_F90_INTP_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Msflow.f90
DEP_F90_MSFLO=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Pflow.f90
DEP_F90_PFLOW=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Pflow2.f90
DEP_F90_PFLOW2=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Prop.f90
DEP_F90_PROP_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Qre.f90
DEP_F90_QRE_F=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Qrn.f90
DEP_F90_QRN_F=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Qrtx.f90
DEP_F90_QRTX_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Qrty.f90
DEP_F90_QRTY_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Qrtz.f90
DEP_F90_QRTZ_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Qwall.f90
DEP_F90_QWALL=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Rad0.f90
NODEP_F90_RAD0_=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Rad_emis.f90
DEP_F90_RAD_E=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Rad_fun.f90
DEP_F90_RAD_F=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Rad_gas.f90
DEP_F90_RAD_G=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Rad_soot.f90
DEP_F90_RAD_S=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Reac.f90
# End Source File
# Begin Source File

SOURCE=.\Code\Sav3f.f90
DEP_F90_SAV3F=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Sbc.f90
DEP_F90_SBC_F=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Setup.f90
DEP_F90_SETUP=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Source.f90
DEP_F90_SOURC=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Sourced.f90
DEP_F90_SOURCE=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Sourcem.f90
DEP_F90_SOURCEM=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Sourcep.f90
DEP_F90_SOURCEP=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Sphase.f90
DEP_F90_SPHAS=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Sprint.f90
DEP_F90_SPRIN=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\SprintFV.f90
DEP_F90_SPRINT=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Stdy3p.f90
DEP_F90_STDY3=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\Utility.f90
DEP_F90_UTILI=\
	".\Debug\GBL_VAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Code\var_mod.f90
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Group "combustion"

# PROP Default_Filter ""
# End Group
# End Target
# End Project
