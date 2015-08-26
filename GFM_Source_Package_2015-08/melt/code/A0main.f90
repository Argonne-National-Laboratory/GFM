!*******************************************************************************************
!*******************************************************************************************
!
!                        Copyright © 2015, UChicago Argonne, LLC
!
!                                 All Rights Reserved
!
!                        Glass Furnace Model(GFM)v4(ANL-SF-01-030)
!
!                      Steven A. Lottes, Argonne National Laboratory
!
!
!                                 OPEN SOURCE LICENSE
!
!
!   Under the terms of Contract No. DE-AC02-06CH11357 with UChicago Argonne, LLC,
!   the U.S. Government retains certain rights in this software.
!
!
!   Redistribution and use in source and binary forms, with or without modification,
!   are permitted provided that the following conditions are met:
!
!
!   1.  Redistribution of source code must retain the above copyright notice, this
!       list of conditions and the following disclaimer.
!   2.  Redistribution in binary form must reproduce the above copyright notice, this
!       list of conditions and the following disclaimer in the documentation and/or
!       other materials provided with the distribution.
!   3.  Neither the names of UChicago Argonne, LLC nor the Department of Energy nor the
!       names of its contributors may be used to endorse or promote products derived
!       from this software without specific prior written permission.
!
!
!
!*******************************************************************************************
!*******************************************************************************************
!
!
!                                     DISCLAIMER
!
!
!            THE SOFTWARE IS SUPPLIED "AS IS" WITHOUT WARRANTY OF ANY KIND.
!
!
!   NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT OF ENERGY,
!   NOR UCHICAGO ARGONNE, LLC, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS
!   OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
!   COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, DATA, APPARATUS, PRODUCT, OR
!   PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
!
!
!
!*******************************************************************************************
!*******************************************************************************************
!
!
!
!
!
!======================================================================
!
                     ! A0main.f90
!     Glass Melt Flow
!        three-dimensional, steady state
!        liquid glass, solid batch, gas bubbles
!        electric booster, bubbler
!     Rev: 4/02
!======================================================================
PROGRAM GMFLOW
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
INTEGER*2 IHR1,IHR2,IMIN1,IMIN2,ISEC1,ISEC2,IHUND1,IHUND2 
character*20 string20

!----------------------------------------------------------------------
CALL GETTIM(IHR1,IMIN1,ISEC1,IHUND1)
CPU1=(IHR1*60+IMIN1)*60+ISEC1+IHUND1*1.0D-2

!Make sure there is a runs.dat file
filename='runs.dat'
inquire (file=filename,exist=exst)
if (.not.exst) then
   write (6,*) ' e01: runs.dat file is missing.'
   casedir=""
   call stop_run("runs.dat file is missing.")
end if  

!Get the run and grid numbers from the runs.dat file
open(nu_run,file=filename)
read (nu_run,*) string20,runum
read (nu_run,*) string20,gdnum
read (nu_run,*) string20,regen
if (regen == 1) then
   read (nu_run,*) string20,runum_r
   read (nu_run,*) string20,gdnum_r
endif
close(nu_run)
crun=runum
casedir = 'case'//crun !Note that the runs.dat file is the only file
                       !that will not be the casedir subdirectory

!The runend file is used by this program to signal the GFM GUI that
!this program is stopping.
!Remove any old runend file before starting.
filename=casedir//'\runend.txt'
inquire (file=filename,exist=exst)
if (exst) n=delfilesqq(filename)

!The runstop file is used by the gfm gui to signal this program to stop.
!remove any old runstop file before starting.
filename=casedir//'\runstop.dat'
inquire (file=filename,exist=runstop_exist)
if (runstop_exist) n=delfilesqq(filename)
runstop_exist=.false.

!The gfm.dat file is created by this CFD code and read by the gfm gui.
!Remove any old gfm.dat file before starting.
filename=casedir//'\gfm.dat'
inquire (file=filename,exist=exst)
if (exst) n=delfilesqq(filename)

!-----------------------------------------------
!Perform various initializations
!Read namelist parameters for the run
!Read grid file and initialize grid arrays
CALL SETUP 

!Initialize files that stay open for monitoring program progress
if (iInfo>0) call Info_file_init !Summary information with per iteration data, Lottes 5/20/05
if (isum>0) call summary_file_init !Summary information, Lottes 4/17/06
if (iTave>0) call Tave_file_init !Mean temperature, Lottes
if (iconv>0) call convg_file_init(nug) !Mass residual, Lottes 
if (cycling>0 .and. iTchg==1) call Tchg_file_init !melt surface temperature change, Lottes 8/10/05
if (igresid>0) call gresid_file_init !glass PDE equation residuals 
if (igresidp>0) call pre_gresid_file_init !glass pre-solve PDE equation residuals 
if (igcor>0) call gcor_file_init !glass PDE correction equation residuals 
if (igcorp>0) call pre_gcor_file_init !glass pre-solve PDE correction equation residuals 

!----------------------------------------------------------------------
!     Restart Parameters
!        IRSTYP=0, fresh start
!              =1, resume steady-state calculation by reading a
!                  restart file
!     Steady State Parameters
!        SBC: to set steady boundary conditions
!        INITSV: to guess an initial flowfield
!        HPRINT: to print reference values and dimensionless parameters
!        SPHASE: to do a single phase flow calculation
!                (initial guess for a two-phase calculation
!        STDY3P: to solve three-phase flow field for one time step
!                with DTM=BIG, it computes a steady state solution
!---------------------------------------------------------------------- 
IF (IDEBUG.EQ.1) THEN
   WRITE(20,*)
   IF (IRSTYP .EQ. 0) THEN
      WRITE(20,*) 'Fresh start with no backup file'
   ELSE   
      WRITE(20,*) 'Run started from a backup file'
   ENDIF
ENDIF
IF (IRSTYP.LE.2) THEN
   TM=0
   DTM=BIG

   CALL SBC !set steady boundary conditions
   CALL INITSV    !For a fresh start, guess an initial flowfield.
                  !Else obtain flowfield variables from a restart file

   Call set_surface_conditions

   if (iglass_qual_only==1) then ! Lottes 3/15/2005
      call gl_qual ! Velocity, & other variables should be loaded
            ! from the restart file at this point
            ! so we can do some post processing.
      call stop_run("normal run ends after calling gl_qual.")
   endif 
   ! call qloss
   ! call conv_surf_temp 
   IF (NPHAS.EQ.1) THEN
      KEY=1
      CALL SPHASE
   ELSE !melt flow is 2-phase
      KEY=2
      CALL STDY3P !solve flow field
   ENDIF
   IF (IBAKUP.GT.0) CALL SAV3F !Save flow field variables for possible restart
ENDIF

IF (runstop_exist) GOTO 250
IF (STEADY ) GOTO 240
!----------------------------------------------------------------------
!     TIME STEP COMPUTATION
!        TM: time
!        DTM: time step
!        BC: sets the boundary conditions at time TM.
!----------------------------------------------------------------------
IF (IRSTYP.LE.2) THEN
   IFP=0
   TM=0
   DTM=one
ENDIF

200  continue 
IF (IBAKUP.GT.0) CALL SAV3F
ISTEP=ISTEP+1
!CALL OSAVE
!CSL         CALL ADTIME
!CSL         CALL BC
!CSL         CALL TSTEP
IF (TPRINT) CALL SPRINT
TPRINT=.FALSE.
IF (ISTEP.LE.MAXTS) GOTO 200


!----------------------------------------------------------------------
!     MSFLOW: subspecies flow (not used)
!----------------------------------------------------------------------
240  continue 
IF (MS.EQ.1) THEN
   !Lottes 2-3-06: Need an initialization routine for subspecies
   call INIT_minor_species !This routine is not complete @@@
   !CBG         CALL INITSV(2)  ! 1 JULY 03
   !CBG        CALL MSFLOW  ! 1 JULY 03
   IF (IBAKUP.GT.0) CALL save_minor_species

   !This call only does 'close(7)' @@@
   !It would make more sense if the SAV3F routine would goto 200, not 900 
   !based on value of argument
ENDIF   


!----------------------------------------------------------------------
!     SPRINT: to print computed results of the flow
!----------------------------------------------------------------------
250  continue 
IFP=1
!CBG      GOTO 252   
!cbg  CALL SPRINTFV
252  continue   
CALL SPRINT
if (ifieldview == 1) call sprintfv
if (iglass_qual==1) then
   call gl_qual
endif

if (iflox == 1) then
   call print_x_mass_flow
endif

IF (IDEBUG.GE.1) CLOSE(20)
CALL GETTIM(IHR2,IMIN2,ISEC2,IHUND2)
CPU2=(IHR2*60+IMIN2)*60+ISEC2+IHUND2*1.0D-2-CPU1
IF (CPU2.LT.0) CPU2=CPU2+86400
WRITE (6,"(T5,'Total run time =',F10.2,' s')") CPU2
if (iconv>0) close(nug) ! close convg file for liquid glass phase 
if (iTave>0) close(nu_Tave) ! close Tave file
if (igresid>0) close(nu_gres) ! close mean glass PDE residual file
if (igresidp>0) close(nu_gresp) !glass pre-solve PDE equation residuals 
if (igcor>0) close(nu_gcor) ! close mean glass correction PDE residual file
if (igcorp>0) close(nu_gcorp) !glass pre-solve PDE correction equation residuals 
if (cycling>0 .and. iTchg==1) close(nu_Tchg) !close temperature change file
if (cycling>0 .and. iTchg==1 .and. regen>0) close(nu_Tchgr) !close regen temperature change file

if (iInfo>0) then
  !Repeat column headings for easy reference
   write (nu_info,'(/"# Iter ", &
               "  Net Heat to Liquid  ", &
               "     Heat to Batch    ", &
               "    Heat to Cullet    ", &
               "     Sum Incoming     ", &
               "      Wall Loss       ", &
               "        Factor        ", &
               "    Factor Change     "/)')

   call sum_info_file_end_conditions(nu_info) !write end conditions in the information file
   close(nu_info)
endif

if (isum>0) then
   write (nu_sum,'(/)')
   write (nu_sum,'(/"# #################   Run End   #################"/)')
   call sum_info_file_end_conditions(nu_sum) !write end conditions in the summary file 
   close(nu_sum)
endif

call stop_run("normal end of melt run.")
end


!======================================================================
!======================================================================
!======================================================================
!
! stop_run creates file runend.txt to communicate to the GFM GUI that
! this code is stopping. Then it stops the GFM run.
! 
! If the stop_reason does not contain the string "normal" then the 
! GFM GUI will indicate an error has occurred.
!
! Lottes; new subroutine 07-11-05
! 
!======================================================================
subroutine stop_run(stop_reason)
use gbl_var
implicit double precision (a-h,o-z)
character(*) stop_reason

open(nu_stop,file=casedir//'\runend.txt') !signal gfm gui that simulation has stopped
write(nu_stop,*) stop_reason
close(nu_stop)
stop
end
