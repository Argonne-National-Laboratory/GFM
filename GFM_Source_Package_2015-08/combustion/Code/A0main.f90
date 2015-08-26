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
!======================================================================
! A0main.f90
!======================================================================
!     Furnace Combustion Space Flow Calculation:
!        - three-dimensional turbulent flow
!        - lumped integral combustion model
!        - subspecies transporation on precalculated flow field
!        - spectral radiation heat transfer
!     Argonne National Lab, 3/02
!======================================================================
!
!     This module contains the following routines:
!          gfm_comb_rad
!          transient       Not Used
!          track_energy
!          stop_run(stop_reason)
!
!===================================================================
program gfm_comb_rad
use gbl_var
implicit double precision (a-h,o-z)
character*20 string20

call gettim(ihr1,imin1,isec1,ihund1) !Will time the run

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
close(nu_run)
crun=runum
casedir = 'case'//crun !Note that the runs.dat, kinetics.d, and relaxfactorc.txt files are the 
                       !only files that will not be the casedir subdirectory

!The runend file is used by this program to signal the GFM GUI that
!this program is stopping.
!Remove any old runend file before starting.
filename=casedir//'\runend.txt'
inquire (file=filename,exist=exst)
if (exst) n=delfilesqq(filename)

!The runstop file is used by the gfm gui to signal this program to stop.
!Remove any old runstop file before starting.
filename=casedir//'\runstop.dat'
inquire (file=filename,exist=exst)
if (exst) n=delfilesqq(filename)
runstop_exist = .false.

!The gfm.dat file is created by this CFD code and read by the gfm gui.
!Remove any old gfm.dat file before starting.
filename=casedir//'\gfm.dat'
inquire (file=filename,exist=exst)
if (exst) n=delfilesqq(filename)

write (6,*) 'initializing simulation, please wait ...'

!----------------------------------------------------
! Initialize developer flags and control parameters

i_air_flow_only=0 !1 => init species concentration for air only, else set according to reactions
irad_test=0 !1 => special code to test radiation for case 50s
itr_gas=0 !itr_gas is the main iteration counter
id_rad=-1 !default to not doing radiation
in_run=0 !flag is zero on run start, one after initialization in loop below
i1st_rad_done=0 !=1 after 1st radiation calc. has been done, 0 otherwise
i1st_rad_qe_done=0 !=1 after 1st qe only radiation calc. has been done, 0 otherwise
info_data_start=0
!qe_scale_back=0  !keep track of qe scale backs for printing in info file
!qe_scale_back_amount=0.0D+0

!call initial_setup

!---------------------------------------------------------------------
!     Restart Parameter
!        IRSTYP=0, fresh start
!              =1, resume calculation by reading a restart file
!     Steady State Routines
!        SETUP: to initialize variables
!        SBC: to set boundary conditions
!        INITSV: to guess an initial flowfield
!        HPRINT: to print reference values and dimensionless parameters
!        SPHASE: to do a single phase flow calculation
!        STDY3P: to solve three-phase flow field 
!        SPRINT: to print computed results of the flow

!---------------------------------------------------------------------- 
! Iterate through CFD combustion, sub-species and radiation calculations
!         until ready for a melt computation or done.
!---------------------------------------------------------------------- 
do
   call setup !initialize variables

   !Initialize files that stay open for monitoring program progress
   if (in_run==0) then
      if (iTave>0) call Tave_file_init !Mean temperature
      if (iconv>0) call convg_file_init(nug) !Mass residuals 
      if (ihresid>0) call hresid_file_init 
      if (cycling>0 .and. iflx==1) call fchg_file_init !melt surface heat flux change,  8/10/05
      if (iwall_info>0) call twall_file_init
      if (igresid>0) call gresid_file_init !momentum, energy, pressure PDE residuals
      if (igresidp>0) call pre_gresid_file_init !momentum, energy, pressure pre-solve PDE residuals
      if (igresidx>0) call gresid_xtra_file_init !species and k-epsilon model residuals
      if (igresidxp>0) call pre_gresid_xtra_file_init !species and k-epsilon model pre-solve residuals
      if (id_rad>0) then
         if (irad_detail>0) call rad_file_init !create radiation files
         if (irad_rad>0)    call rad_radiosity_file_init
         if (irad_qc>0)     call rad_heat_flux_file_init
         if (irad_amb>0)    call rad_wall_loss_file_init
         if (irad_qrs>0)    call rad_melt_file_init
         if (irad_conv>0)   call rad_convection_file_init
      endif
      if (isoot_cal>0) call soot_file_init 
      if (ms==1) then
         if (imresid==1)  call mresid_file_init !minor species residual file
         if (imresidp==1) call pre_mresid_file_init !minor species pre-solve residual file
      endif
   endif

   call sbc !set boundary conditions
   call initsv !guess an initial flowfield for main variables
               !or restore an existing one from a restart file

   if (in_run==0) then
      if (ms==1) call initsvm  !guess or restore initial flowfield for sub species
      if (iInfo>0) call Info_file_init !Summary information with per iteration data,  5/20/05
      if (isum>0) call summary_file_init !Summary information,  4/17/06
   endif
   !call solver_test

   in_run=1

   !---------------------------------------------------
   ! Do main CFD calculation

   if (nphas == 1) then !always true
      key=1
      call sphase !Solve single phase gas flow field
   else
      key=2
      call stdy3p !solve three-phase flow field
   endif

   call sav3f

   !if (.not.steady) call transient
   
   if (runstop_exist) exit ! writes files and stops

   !---------------------------------------------------
   ! Do minor species calculation

   if (ms==1) then
      call initsvm  !guess or restore initial flowfield for sub species
      call msflow
      call sav3fm
   endif

   if (runstop_exist) exit ! writes files and stops

   !---------------------------------------------------
   ! Calculate volume and wall radiation

   if (id_rad>0 .and. ms==1 .and. itr_gas <= maxgi) then
   !if (id_rad>0 .and. ms==1 .and. itr_gas < maxgi) then
      call rad_emis !calculate volume and wall radiation

      if (runstop_exist) exit ! writes files and stops
   endif
    
   if (iinfo>0) call track_energy !write out energy values to information file


   !---------------------------------------------------
   ! Check for end conditions

   !if (itr_gas>=maxgi .or. (res_mass<bgcon .and. (qa_s1/q_f)<rad_con .and. bal_ms < bmcon)) then
   if (itr_gas>=maxgi .or. (res_mass<bgcon .and. imscon==1)) then
      !this is the normal exit point out of the main program loop
      exit !will exit loop & program
   endif

   !do another global iteration 
   itr_gend =itr_gend+interval_rad
enddo   
      

!---------------------------------------------------
! Write out data, close files, and finish run

call sprint !print computed results of the flow
if (ifieldview == 1) call sprintFV


call gettim(ihr2,imin2,isec2,ihund2)
cpu=ihr2-ihr1
cpu=cpu*60.0+(imin2-imin1)
cpu=cpu*60.0+(isec2-isec1)
write (6,"(t5,'total run time =',f10.2,' sec.')") cpu
if (idebug.eq.2) then
   write (20,*) 'hour = ', ihr1, '  min = ', imin1, '  sec = ', &
   isec1, '  hund = ', ihund1  
   write (20,*) 'hour = ', ihr2, '  min = ', imin2, '  sec = ', &
   isec2, '  hund = ', ihund2
   cpu=cpu+(ihund2-ihund1)*0.01
   write (20,"(t5,'total run time =',f10.2,' sec.')") cpu
   close(20)
endif

if (iconv >0)    close(nug) ! close convg file for gas phase 
if (ihresid>0)   close(nu_hres) ! close enthalpy residual file
if (igresid>0)   close(nu_gres) !momentum, energy, pressure PDE residuals
if (igresidp>0)  close(nu_gresp) !momentum, energy, pressure PDE pre-solve residuals
if (igresidx>0)  close(nu_gresx) !species and k-epsilon model residuals
if (igresidxp>0) close(nu_gresxp) !species and k-epsilon model pre-solve residuals
if (itave>0) close(nu_tave) ! close tave  file
if (cycling>0 .and. iflx==1) close(nu_flx) ! close fchg file
if (iwall_info>0) then
   close(nu_twala) ! close average wall temperature file
   !close(nu_wtot)  ! close wall energy totals file
endif
if (irad_detail>0) close(nu_rad)   ! close radiation details from volume and melt file
if (irad_rad>0)    close(nu_cwal)  ! close mean relative radiosity change file
if (irad_qc>0)     close(nu_qcave) ! close mean heat flux from volume file
if (irad_amb>0)    close(nu_qamb)  ! close mean heat loss to ambiant file
if (irad_qrs>0)    close(nu_qrs)   ! close mean heat flux to melt from walls file
if (irad_conv>0)   close(nu_qconv) ! close convection heat flux from gas flow to boundaries file
if (isoot_cal>0) close(nu_soot) ! close soot calibration file
if (ms==1) then
   if (imresid==1) close(nu_mres) ! close minor species residuals file

   if (imresidp==1) close(nu_mresp) ! close minor species pre-solve residuals file
endif

if (iinfo>0) then
   !Repeat column headings for easy reference
   write(nu_info,'("# iter      ", &
                "h flow in             ", &
                "q reaction            ", &
                "q melt                ", &
                "q exhaust             ", &
                "q wall loss         ", &
                "q rad opening loss      ", &
                "q from media          ", &
                "q gas to wall")')
   call sum_info_file_end_conditions(nu_info) !write end conditions in the information file 
   close(nu_info)
endif

if (isum>0) then
   write (nu_sum,'(/)')
   write (nu_sum,'(/"# #################   Run End   #################"/)')
   call sum_info_file_end_conditions(nu_sum) !write end conditions in the summary file 
   close(nu_sum)
endif

call stop_run("normal end of combustion run.")

end

!======================================================================
!======================================================================
!======================================================================
!
!c----------------------------------------------------------------------
!c     time step computation
!c        bc: sets the boundary conditions at time tm.
!c----------------------------------------------------------------------
!  subroutine transient
!      if (irstyp.le.2) then
!         ifp=0
!         tm=0
!         dtm=dt0
!      endif
!200   !if (ibakup.gt.0) then
!         call sav3f
!         call gettim(ihr2,imin2,isec2,ihund2)
!         cpu=ihr2-ihr1
!         cpu=cpu*60.0+(imin2-imin1)
!         cpu=cpu*60.0+(isec2-isec1)
!         cpu=cpu+(ihund2-ihund1)*0.01
!         if (cpu.ge.86400) stop
!      !endif
!      istep=istep+1
!      call osave
!c      call adtime
!c      call bc
!c      call tstep
!      if (tprint) call sprint
!      tprint=.false.
!      if (istep.le.maxts) goto 200
!  end subroutine transient
!end


!======================================================================
!======================================================================
!======================================================================
!
! write out various energy values to the information file
!
!  new subroutine 08-17-05
! 
!======================================================================
subroutine track_energy
use gbl_var
implicit double precision (a-h,o-z)

!Write out column headings only on start-up
if (info_data_start==0) then
   write(nu_info,'(/"# iter      ", &
                "h flow in             ", &
                "q reaction            ", &
                "q melt                ", &
                "q exhaust             ", &
                "q wall loss         ", &
                "q rad opening loss      ", &
                "q from media          ", &
                "q gas to wall"/)')
   info_data_start=1
endif

write(nu_info,"(1x,i5,8e22.14)") itr_gas, &
       q_h_in,   &    !energy in with flow       
       q_react,  &    !energy deposition via combustion
       q_melt,  & !energy out to melt surface
       q_exhaust,   & !energy out with flow
       q_wall_loss, & !energy out through walls
       q_rad_inlet + q_rad_outlet, & !energy out through inlets and outlets
       q_incident_total, & !net rad energy out of volume from participating media
       qconv_total !energy rate from gas to wall
return
end


!======================================================================
!======================================================================
!======================================================================
!
! Stop_run creates file runend.txt to communicate to the gfm gui that
! this code is stopping. Then it stops the gfm run.
! 
! If the stop_reason does not contain the string "normal" then the 
! gfm gui will indicate an error has occurred.
!
!  new subroutine 07-11-05
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
