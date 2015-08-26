!======================================================================
!  Utility.f90
!======================================================================
!  This module contains various utility and checking routinues.  
!
!  This file contains the following subroutines and/or functions:
!      Info_file_init
!      summary_file_init
!      sum_info_file_start_conditions(file_nu)
!      sum_info_file_end_conditions(file_nu)
!      Tave_file_init
!      gresid_file_init
!      Tchg_file_init
!      convg_file_init(iu)
!      convg_file_autocopy(iu)  - not used (fix file location)
!      convg_file_autocopy1(iu) - has detailed error handling, not used - not used (fix file location)
!      compare_rstrt_files
!
!======================================================================
!======================================================================
!======================================================================
! Routine:  Info_file_init
!
! Purpose:  Initialize run information file
!
! Outputs
!   Files opened:   'Info'//runum//'.plt'   (nu_info=98)
!
!======================================================================
subroutine Info_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

filename=casedir//'\Info'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_info,file=filename,shared,position='append')
else
   open(nu_info,file=filename,shared)
   write(nu_info,"('# Case ',a,' Information for Simulation and Evolving Global Energy Distribution')") runum
   write(nu_info,"(/'# ',T5,'Number of cells in melt grid:',T38,I7)") ncells
   write(nu_info,"('# ',T5,'Melt surface area: ',T36,E25.14,' m^2'/)") area_surf_tot
   !write(nu_info,'("# PlotData")')
endif

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  summary_file_init
!
! Purpose:  Initialize run summary file
!           (This file duplicates non-column data from the 'info'//runum//'m.plt' file)
!
! Outputs
!   Files opened:   'summary'//runum//'m.txt'   (nu_sum=35)
!
!======================================================================
subroutine summary_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

filename=casedir//'\summary'//runum//'m.txt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_sum,file=filename,shared,position='append')
else
   open(nu_sum,file=filename,shared)
   write(nu_sum,"('# Case ',a,' Summary Data for Simulation and Evolving Global Energy Distribution')") runum
   write(nu_sum,"(/'# ',T5,'Number of cells in melt grid:',T38,I7)") ncells
   write(nu_sum,"('# ',T5,'Melt surface area: ',T36,E25.14,' m^2'/)") area_surf_tot
endif

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  sum_info_file_start_conditions(file_nu)
!
! Purpose:  Enter start conditions in the summary or information file
!
! Inputs:   file_nu = Unit number of the file to write
!
!======================================================================
subroutine sum_info_file_start_conditions(file_nu)
use gbl_var
implicit real*8 (a-h,o-z)
integer file_nu

   write (file_nu,"('# ',T5,'Mean Surface Heat Flux:',T36,E25.14,' W/m^2')") qc_tot/area_surf_tot 
   write (file_nu,'(/"#   In-Flows:"/)')
   if(fr2_Lg>zero) write (file_nu,"('# ',T5,'Liquid Glass:',T36,E25.14,' kg/s')") FR2_LG
   if(pca%fr2>zero) write (file_nu,"('# ',T5,'Cullet:',T36,E25.14,' kg/s')") PCA%FR2
   if(psa%fr2>zero) write (file_nu,"('# ',T5,'Batch:',T36,E25.14,' kg/s')") PSA%FR2

   write (file_nu,'(/"#   Heat Inputs and Requirements:"/)')

   write (file_nu,"('# ',T5,'Radiation + Convection:',T36,E25.14,' W')") qc_tot
   if (regen==1) write (file_nu,"('# ',T5,'Alternate Burners Rad + Conv:',T36,E25.14,' W')") qc_tot_r
   write (file_nu,"('# ',T5,'Electric Boost:',T36,E25.14,' W')") eb_heat
	if (regen==1) then
		write (file_nu,"('# ',T5,'Total Energy In:',T36,E25.14,' W')") eb_heat + qc_tot
	else
		write (file_nu,"('# ',T5,'Total Energy In:',T36,E25.14,' W')") eb_heat + (qc_tot+qc_tot_r)/2
   endif
	if(pca%fr2>zero)then
      write (file_nu,"(/'# ',T5,'Cullet heatup:',T36,E25.14,' W')") qheat_c_need
       write (file_nu,"('# ',T5,'Cullet melt:',T36,E25.14,' W')") qmelt_c_need
      write (file_nu,"('# ',T5,'Cullet q needed:',T36,E25.14,' W')") qmelt_c_need+qheat_c_need
   endif
   if(psa%fr2>zero)then
      write (file_nu,"(/'# ',T5,'Batch heatup:',T36,E25.14,' W')") qheat_s_need
      write (file_nu,"('# ',T5,'Batch melt:',T36,E25.14,' W')") qmelt_s_need
      write (file_nu,"('# ',T5,'Batch q needed:',T36,E25.14,' W')") qmelt_s_need+qheat_s_need
   endif
   write (file_nu,"(/'# ',T5,'Liquid heatup need:',T36,E25.14,' W')") qglass_need
   write (file_nu,"('# ',T5,'Adiabatic energy need:',T36,E25.14,' W')") h_needed

   if (file_nu==nu_info) write(nu_info,'("# PlotData")')

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  sum_info_file_end_conditions(file_nu)
!
! Purpose:  Enter end conditions in the summary or information file
!
! Inputs:   file_nu = Unit number of the file to write
!
!======================================================================
subroutine sum_info_file_end_conditions(file_nu)
use gbl_var
implicit real*8 (a-h,o-z)
integer file_nu

   if (iheat_flux_type < 3) then !scaled uniform or scaled combustion calculated heat flux
      !write (file_nu,'(/"#  Heat Values at Run End (Scaled):"/)')
      write (file_nu,'(/"#  Heat Values at Run End (Scaling is on):"/)')
   else
      write (file_nu,'(/"#  Heat Values at Run End:"/)')
   endif
   write (file_nu,"('# ',T5,'Radiation + Convection:',T36,E25.14,' W')") qrs_tot
   write (file_nu,"('# ',T5,'Electric Boost:',T36,E25.14,' W')") eb_heat
   write (file_nu,"('# ',T5,'Total Energy In:',T36,E25.14,' W')") eb_heat + qrs_tot
   if(pca%fr2>zero)then
      write (file_nu,"(/'# ',T5,'Cullet heatup need:',T36,E25.14,' W')") qheat_c_need
      write (file_nu,"('# ',T5,'Cullet melt need:',T36,E25.14,' W')") qmelt_c_need
      write (file_nu,"('# ',T5,'Radiation to Cullet:',T36,E25.14,' W')") qrsp_c_tot
      write (file_nu,"('# ',T5,'Conduction to Cullet:',T36,E25.14,' W')") qcond_c_tot
      write (file_nu,"('# ',T5,'Cullet q needed:',T36,E25.14,' W')") qmelt_c_need+qheat_c_need
      write (file_nu,"('# ',T5,'Cullet q net:',T36,E25.14,' W')") qcond_c_tot+qrsp_c_tot
   endif
   if(psa%fr2>zero)then
      write (file_nu,"(/'# ',T5,'Batch heatup need:',T36,E25.14,' W')") qheat_s_need
      write (file_nu,"('# ',T5,'Batch melt need:',T36,E25.14,' W')") qmelt_s_need
      write (file_nu,"('# ',T5,'Radiation to Batch:',T36,E25.14,' W')") qrsp_s_tot
      write (file_nu,"('# ',T5,'Conduction to Batch:',T36,E25.14,' W')") qcond_s_tot
      write (file_nu,"('# ',T5,'Batch q needed:',T36,E25.14,' W')") qmelt_s_need+qheat_s_need
      write (file_nu,"('# ',T5,'Batch q net:',T36,E25.14,' W')") qcond_s_tot+qrsp_s_tot
   endif
   write (file_nu,"(/'# ',T5,'Liquid heatup need:',T36,E25.14,' W')") qglass_need
   write (file_nu,"('# ',T5,'Liquid q net:',T36,E25.14,' W')") qglass_net
   write (file_nu,"('# ',T5,'Adiabatic energy need:',T36,E25.14,' W')") h_needed
   write (file_nu,"('# ',T5,'Wall heat loss:',T36,E25.14,' W')") q_wall_loss_tot
   write (file_nu,"('# ',T5,'Net melt input energy need:',T36,E25.14,' W')") h_needed+q_wall_loss_tot
   write (file_nu,"('# ',T5,'Actual melt input energy:',T36,E25.14,' W')") qrs_tot+eb_heat
   write (file_nu,"(/'# ',T5,'Energy need divided by input:',T36,E25.14)") &
                             (h_needed+q_wall_loss_tot)/(qrs_tot+eb_heat)
   if (iheat_flux_type < 3) then !scaled uniform or scaled combustion calculated heat flux
      write (file_nu,"('# ',T5,'Scaling factor:',T36,E25.14)") &
                             (h_needed+q_wall_loss_tot)/(qc_tot+eb_heat)
                             !(h_needed+q_wall_loss_tot)/(qrs_tot_base+eb_heat)
   endif

   write (file_nu,"(/'# ',T5,'Mean glass exit temperature:',T36,E25.14,' K')") Tmean_ex

   if(pca%fr2>zero)then
      write (file_nu,"('# ',T5,'Cullet rate in:',T36,E25.14,' kg/s')") pca%fr2
   endif
   if(psa%fr2>zero)then
      write (file_nu,"('# ',T5,'Batch rate in:',T36,E25.14,' kg/s')") psa%fr2
   endif
   if(psa%fr2>zero.and.pca%fr2>zero)then
      write (file_nu,"('# ',T5,'Total solids rate in:',T36,E25.14,' kg/s')") pca%fr2+psa%fr2
   endif
   write (file_nu,"('# ',T5,'Melt rate:',T36,E25.14,' kg/s')") gfin
   write (file_nu,"('# ',T5,'Glass rate out:',T36,E25.14,' kg/s')") gfex

   if (cycling==1) write (file_nu,"(/'# ',T5,'Cycle count:',T36,i5)") cycle_count

   !if (regen==1) then
   !   write (file_nu,"('# ',T5,'Alternate scaling factor:',T36,E25.14)") facq_r
   !endif
   !      write(file_nu,"(1X,I5,7E25.14)") nlg,qrslg_tot,
   !&          qrsp_c_tot,qrsp_s_tot,
   !&          qcond_s_tot,qcond_c_tot,q_wall_loss_tot,qglass_net

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  Tave_file_init
!
! Purpose:  Initialize volume average temperature file
!
! Outputs
!   Files opened:   'Tave'//runum//'m.plt'   (nu_Tave=95)
!
!======================================================================
subroutine Tave_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

filename=casedir//'\Tave'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_Tave,file=filename,shared,position='append')
else
   open(nu_Tave,file=filename,shared)
   write(nu_Tave,*) '# Melt mean temperature (K)'
   write(nu_Tave,*) '# PlotData'
   !write(nu_Tave,*) '# Iteration   Temperature'
   write (nu_Tave,'(/"# Iter ", &
            "   Mean Melt Temp     ", &
            "   Mean Exit Temp     "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  gresid_file_init
!
! Purpose:  Initialize normalized mean glass PDE residual file
!              for enthalpy, pressure, and momentum
!
!           Used when igresid=1
!
! Outputs
!   Files opened:   'gresid'//runum//'c.plt'    nu_gres=99
!
!======================================================================
subroutine gresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
filename=casedir//'\gresid'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gres,file=filename,shared,position='append')
else
   open(nu_gres,file=filename,shared)
   write(nu_gres,*) '# Mean residuals - log plot'
   write(nu_gres,*) '# PlotData '

   write (nu_gres,'(/"# Iter ", &
            "        Enthalpy        ", &
            "        Pressure        ", &
            "       x-momentum       ", &
            "       y-momentum       ", &
            "       z-momentum       "/)')
endif

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  pre_gresid_file_init
!
! Purpose:  Initialize pre_solve normalized mean glass PDE residual file
!              for enthalpy, pressure, and momentum
!
!           Used when igresidp=1
!
! Outputs
!   Files opened:   'gresidp'//runum//'c.plt'    nu_gresp=113
!
!======================================================================
subroutine pre_gresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
filename=casedir//'\gresidp'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresp,file=filename,shared,position='append')
else
   open(nu_gresp,file=filename,shared)
   write(nu_gresp,*) '# Mean residuals (pre-solve) - log plot'
   write(nu_gresp,*) '# PlotData '

   write (nu_gresp,'(/"# Iter ", &
            "        Enthalpy        ", &
            "        Pressure        ", &
            "       x-momentum       ", &
            "       y-momentum       ", &
            "       z-momentum       "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  gcor_file_init
!
! Purpose:  Initialize normalized mean glass PDE corrections residual file
!
!           Used when igcor=1
!
! Outputs
!   Files opened:   'gcor'//runum//'m.plt'    nu_gcor=100
!
!======================================================================
subroutine gcor_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
filename=casedir//'\gcor'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gcor,file=filename,shared,position='append')
else
   open(nu_gcor,file=filename,shared)
   write(nu_gcor,*) '# Mean correction PDE residuals - log plot'
   write(nu_gcor,*) '# PlotData '
   write (nu_gcor,'(/"# Iter ", &
            "      P correction      ", &
            "    mean x-vel corr     ", &
            "    mean y-vel corr     ", &
            "    mean z-vel corr     "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  pre_gcor_file_init
!
! Purpose:  Initialize pre-solve normalized mean glass PDE corrections residual file
!
!           Used when igcorp=1
!
! Outputs
!   Files opened:   'gcorp'//runum//'m.plt'    nu_gcorp=114
!
!======================================================================
subroutine pre_gcor_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
filename=casedir//'\gcorp'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gcorp,file=filename,shared,position='append')
else
   open(nu_gcorp,file=filename,shared)
   write(nu_gcorp,*) '# Mean correction PDE residuals (pre-solve) - log plot'
   write(nu_gcorp,*) '# PlotData '

   write (nu_gcorp,'(/"# Iter ", &
            "      P correction      ", &
            "    mean x-vel corr     ", &
            "    mean y-vel corr     ", &
            "    mean z-vel corr     "/)')
endif
return
end




!======================================================================
!======================================================================
!======================================================================
! Routine:  Tchg_file_init
!
! Purpose:  Initialize temperature change file
!
! Outputs
!   Files opened:   'tchg'//runum//'m.plt'    (nu_Tchg=111)
!                   'tchg'//runum_r//'m.plt'   (nu_Tchgr=112)
!
! Only called if cycling>0
!
!======================================================================
subroutine Tchg_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!open temperature change file
filename=casedir//'\Tchg'//runum//'m.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open (nu_Tchg,file=filename,position='append')
else
   open(nu_Tchg,file=filename)
   write(nu_Tchg,*) 'Average Change in Melt Surface Temperature - log plot'
   write(nu_Tchg,'("  ")')
   write(nu_Tchg,*) '# PlotData '
   write(nu_Tchg,*) '# Iter     Temperature_change         Relative_change'
endif

if (regen>0) then
   !Also have alternate furnace configuration temperture change file to open 
   filename=casedir//'\Tchg'//runum_r//'m.plt'
   inquire(file=filename,exist=exst0)
   if (exst0.and.irstyp>0) then
      open (nu_Tchgr,file=filename,position='append')
   else
      open(nu_Tchgr,file=filename)
      write(nu_Tchgr,*) 'Average Change in Melt Surface Temperature - log plot'
      write(nu_Tchgr,'("  ")')
      write(nu_Tchgr,*) '# PlotData '
      write(nu_Tchgr,*) '# Iter     Temperature_change         Relative_change'
   endif
endif

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  convg_file_init(iu)
!
! Purpose:  Initialize specified convergence file
!
! Inputs
!   Arguments:      iu  file id for convergence file to open
!
! Outputs
!   Files written:  'convg'//crun//'.plt'
!                   'convg'//crun//'d.plt'
!                   'convg'//crun//'p.plt'
!
!======================================================================
subroutine convg_file_init(iu)
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

if (iu==nug) then
   filename=casedir//'\convg'//runum//'m.plt'
   inquire(file=filename,exist=exst0)
   if (exst0.and.irstyp>0) then
      open(iu,file=filename,shared,position='append')
   else
      open(iu,file=filename,shared)
      write(iu,*) '# Melt domain mean and max mass residual - log plot'
      write(iu,*) '# PlotData'
      write(iu,*) '# iteration  mean mass residual  max mass residual'
    endif

elseif (iu==nud) then
   filename=casedir//'\convg'//runum//'d.plt'
   inquire(file=filename,exist=exst0)
   if (exst0) then
      open(iu,file=filename,shared,position='append')
   else
      open(iu,file=filename,shared)
      write(iu,*) '# Droplet mean and max mass residual - log plot'
      write(iu,*) '# PlotData'
      write(iu,*) '# iteration  mean mass residual  max mass residual'
   endif

elseif(iu==nup)then
   filename=casedir//'\convg'//runum//'p.plt'
   inquire(file=filename,exist=exst0)
   if (exst0) then
      open(iu,file=filename,shared,position='append')
   else
      open(iu,file=filename,shared)
      write(iu,*) '# Particle mean and max mass residual - log plot'
      write(iu,*) '# PlotData'
      write(iu,*) '# iteration  mean mass residual  max mass residual'
   endif
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  convg_file_autocopy(iu)
!
! Purpose:  Make back-up copies of convergence file
!
! Inputs
!   Arguments:     iu  file identifier 
!   Files read:    'convg'//crun//'.plt'   
!                  'convg'//crun//'d.plt'
!                  'convg'//crun//'p.plt'
!
! Outputs
!   Files written: 'convg'//crun//'bg'//id0//'.plt'
!                  'convg'//crun//'bd'//id0//'.plt'
!                  'convg'//crun//'bp'//id0//'.plt'          
!
!======================================================================
subroutine convg_file_autocopy(iu)
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
integer it
real*8 resid,rmax
id0='0'
print *,'in convg_file_autocopy'
if    (iu==nug) then
  filename ='convg'//crun//'.plt'
  filename2='convg'//crun//'bg'//id0//'.plt'
elseif(iu==nud) then
  filename ='convg'//crun//'d.plt'
  filename2='convg'//crun//'bd'//id0//'.plt'
elseif(iu==nup) then
  filename ='convg'//crun//'p.plt'
  filename2='convg'//crun//'bp'//id0//'.plt'
endif

inquire(file=filename,exist=exst0)
if (exst0) then
rewind(iu)
else
   write(ncon,*) ' warning: file' ,filename, 'does not exist'
   return
endif

open(nutmp,file=filename2)
write(nutmp,*) 'iteration   mean mass residual   max mass residual'
read (iu,*) 
do
  read (iu,*,end=10) it,resid,rmax
  write(nutmp,"(i7,e25.16,e25.16)") it,resid,rmax
enddo
10    continue
close(nutmp)    

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  convg_file_autocopy1(iu)
!
! Purpose:  Make back-up copies of convergence file
!
! Inputs
!   Arguments:     iu  file identifier 
!   Files read:    'convg'//crun//'.txt'   
!                  'convg'//crun//'d.txt'
!                  'convg'//crun//'p.txt'
!
! Outputs
!   Files written: 'convg'//crun//'bg'//id0//'.txt'
!                  'convg'//crun//'bd'//id0//'.txt'
!                  'convg'//crun//'bp'//id0//'.txt'          
!
! Notes:  This routine is NOT used.
!         It is the same as routine convg_file_autocopy but
!         it has detailed error handling.
!
!======================================================================
subroutine convg_file_autocopy1(iu)
use gbl_var
implicit real*8 (a-h,o-z)

integer it
real*8 resid,rmax
logical first_time
integer iso !error number
first_time=.true.

if    (iu==nug) then; filename='convg'//crun//'b' //id0//'.txt'
elseif(iu==nud) then; filename='convg'//crun//'bd'//id0//'.txt'
elseif(iu==nup) then; filename='convg'//crun//'bp'//id0//'.txt'
endif

open(nutmp,file=filename,iostat=iso,err=20)
write(ncon,*) 'opened auto convg filename: ',filename
write(nutmp,*,iostat=ios,err=30) &
     ' iteration       mean mass residual', &
         '            max mass residual'

rewind(unit=iu,iostat=ios,err=40)
write(ncon,*) 'completed rewind of file unit: ',iu

read (iu,*) 
do
  read (iu,*,end=11,iostat=iso,err=50) it,resid,rmax
  if (first_time==.true.) write(ncon,*) 'able to read again'
  write(nutmp,"(i7,e25.16,e25.16)",iostat=iso,err=60)it,resid,rmax
  if (first_time==.true.) then
     write(ncon,*) 'wrote to tmp file'
     first_time=.false.
  endif
enddo
11    continue
close(nutmp)
return

20  write(ncon,*) 'error ',iso,' occurred during open'
goto 100 
30  write(ncon,*) 'error ',iso,' occurred during initial write'
goto 100 
40  write(ncon,*) 'error ',iso,' occurred during rewind'
goto 100 
50  write(ncon,*) 'error ',iso,' occurred during read, it= ',it
goto 100 
60  write(ncon,*) 'error ',iso,' occurred during write, it= ',it
        
100  continue    
nu_stop=110
call stop_run("Error in convg_file_autocopy1 routine.")
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  compare_rstrt_files
!
! Purpose:  Compare rs####.d files and print out the maximum
!           difference between array values.
!
! Inputs
!   Files read:    58   'rs'//crun//'.d'    
!                  59   'rs'//crun//'2.d'
!
! Outputs
!   Files written: 60   'rscomp.txt'
!
! Notes:  This routine works only if the text format restart file
!         is being used instead of the binary format.
!
!         Allocation of arrays is assumed to be the same for both files
!         and assume no more than 9 particle and 9 droplet groups. 
!
!         Initially assume the first file is rs####.d and the second
!         file is rs####2.d.
!
!         This routine may only be called by the host process and it
!         may abort without communication to other processors.
!
!======================================================================
subroutine compare_rstrt_files
use gbl_var
implicit real*8 (a-h,o-z)

character*30,allocatable :: arrayname(:)
character(1) i2char(9)
logical exst0
nxc=mz
nyc=nz
nzc=lz
allocate (buf(nxc,nyc,nzc))
allocate (buf2(nxc,nyc,nzc))
allocate (arrayname(22+5*ndg+6*npg))

i2char(1)='1'
i2char(2)='2'
i2char(3)='3'
i2char(4)='4'
i2char(5)='5'
i2char(6)='6'
i2char(7)='7'
i2char(8)='8'
i2char(9)='9'

!------------------------------
! open files
filename=casedir//'\rs'//crun//'.d'
inquire(file=filename,exist=exst0)
if (exst0) then
   nu=58
   open(nu,file=filename,form='unformatted',status='old')
else
   write(ncon,'(a,a,a)') ' abort: file rs',crun,'.d does not exist'
   call abort
endif

filename2=casedir//'\rs'//crun//'2.d'
inquire(file=filename2,exist=exst0)
if (exst0) then
   nu2=59
   open(nu2,file=filename2,form='unformatted',status='old')
else
   write(ncon,'(a,a,a)') ' abort: file rs',crun,'2.d does not exist'
   call abort
endif

nu3=60
open(nu3,file=casedir//'\rscomp.txt')


!--------------------------------
! set array names list
arrayname(1) ='x-velocity'
arrayname(2) ='y-velocity'
arrayname(3) ='z-velocity'
arrayname(4) ='gas pressure'
arrayname(5) ='gas temperature'
arrayname(6) ='gas volume fraction'
arrayname(7) ='enthalpy'
arrayname(8) ='turbulent kinetic energy'
arrayname(9) ='turbulent dissipation rate'
arrayname(10)='fuel mass fraction'
arrayname(11)='oxygen mass fraction'
arrayname(12)='hydrogen mass fraction'
arrayname(13)='inert mass fraction'
arrayname(14)='steam mass fraction'
arrayname(15)='co2 mass fraction'
arrayname(16)='co mass fraction'
iii=16
if (ivap==1) then
   iii=iii+1
   arrayname(iii)='vaporization rate'
endif
if (icond==1) then
   iii=iii+1
   arrayname(iii)='condensation rate'
endif
if (ndg>0) then
   do l=1,ndg
      iii=iii+1
      arrayname(iii)='d grp '//i2char(l)//' temperature'
   enddo
   do l=1,ndg
      iii=iii+1
      arrayname(iii)='d grp '//i2char(l)//' number density'
   enddo
   do l=1,ndg
      iii=iii+1
      arrayname(iii)='d grp '//i2char(l)//' x-velocity'
   enddo
   do l=1,ndg
      iii=iii+1
      arrayname(iii)='d grp '//i2char(l)//' y-velocity'
   enddo
   do l=1,ndg
      iii=iii+1
      arrayname(iii)='d grp '//i2char(l)//' z-velocity'
   enddo
   iii=iii+1     
   arrayname(iii)='droplet volume fraction'
endif

if (npg>0) then 
   do l=1,npg
      iii=iii+1
      arrayname(iii)='p grp '//i2char(l)//' temperature'
   enddo
   do l=1,npg
      iii=iii+1
      arrayname(iii)='p grp '//i2char(l)//' number density'
   enddo
   do l=1,npg
      iii=iii+1
      arrayname(iii)='p grp '//i2char(l)//' x-velocity'
   enddo
   do l=1,npg
      iii=iii+1
      arrayname(iii)='p grp '//i2char(l)//' y-velocity'
   enddo
   do l=1,npg
      iii=iii+1
      arrayname(iii)='p grp '//i2char(l)//' z-velocity'
   enddo
   if (icoke==1) then
      do l=1,npg
         iii=iii+1
         arrayname(iii)='p grp '//i2char(l)//' coke'
      enddo
   endif
   iii=iii+1
   arrayname(iii)='solids pressure'
   iii=iii+1  
   arrayname(iii)='particle volume fraction'
endif

if (icoke==1) then
   iii=iii+1
   arrayname(iii)='gas coke concentration'
endif


!--------------------------------
! compare arrays from the two different restart files
round_err=0.1d-13

icomptype=0
if(icomptype==0)then
   do l=1,iii
      vmax=zero
      num=0
      sdel=0
      read(nu) buf
      read(nu2) buf2
      
      do k=1,nzc
      do j=1,nyc
      do i=1,nxc
         !debug stuff
         !if (l==6) write(nubug,*) buf(i,j,k),'   ',buf2(i,j,k)
         del=abs( (buf(i,j,k)-buf2(i,j,k)) /buf(i,j,k) )
         num=num+1
         if (del>vmax)  vmax=del
         sdel=sdel+del
      enddo;enddo;enddo
      ave=sdel/num
      write(nu3,"(a,t32,a,e12.7,a,e18.13)") &
           arrayname(l),'max relative dif: ',vmax,'  ave dif: ',ave
   enddo
else
   do l=1,iii
      vmax=zero
      numdif=0
   
      read(nu) buf
      read(nu2) buf2
   
      do k=1,nzc
      do j=1,nyc
      do i=1,nxc
         !debug stuff
         !if (l==6) write(nubug,*) buf(i,j,k),'   ',buf2(i,j,k)
         dif=abs(buf(i,j,k)-buf2(i,j,k))
         if (dif>round_err) then
            numdif=numdif+1
            if (dif>vmax)  vmax=dif
         endif
      enddo;enddo;enddo
      if (numdif==0) then
          write(nu3,"(a,t32,a)") arrayname(l), 'no differences'
      else
          write(nu3,"(a,t32,a,e18.13,a,i6)") &
            arrayname(l),'maximum difference: ',vmax,'  numdif: ',numdif
      endif
   enddo
endif
!close(nubug)
close(nu)
close(nu2)
close(nu3)
deallocate(buf)
deallocate(buf2)
deallocate(arrayname)
return
end



