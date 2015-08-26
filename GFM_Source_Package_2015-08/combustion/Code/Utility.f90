!======================================================================
!  Utility.f90
!======================================================================
!  This module contains various utility and checking routinues.  
!
!  This file contains the following subroutines and/or functions:
!      Info_file_init
!      summary_file_init
!      sum_info_file_end_conditions(file_nu)
!      fchg_file_init
!      Tave_file_init - opens mean T file
!      hresid_file_init - not used
!      pre_resid_file_init - not used
!      aft_resid_file_init - not used
!      gresid_file_init
!      pre_gresid_file_init
!      gresid_xtra_file_init
!      pre_gresid_xtra_file_init
!      mresid_file_init
!      pre_mresid_file_init
!      twall_file_init
!      rad_file_init
!      rad_radiosity_file_init
!      rad_heat_flux_file_init
!      rad_wall_loss_file_init
!      rad_melt_file_init
!      rad_convection_file_init
!      soot_file_init
!      convg_file_init(iu)
!      convg_file_autocopy(iu) - not used (fix file location)
!      convg_file_autocopy1(iu) - has detailed error handling, not used (fix file location)
!      compare_rstrt_files
!      T_ave_calc
!      abort
!      osave
!======================================================================
!======================================================================
!======================================================================
! Routine:  Info_file_init
!
! Purpose:  Initialize run information file
!
! Outputs
!   Files opened:   'Info'//runum//'.plt'     nu_info=96
!
!======================================================================
subroutine Info_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
filename=casedir//'\Info'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_info,file=filename,shared,position='append')
else
   open(nu_info,file=filename,shared)
   write(nu_info,"('# Case ',a,' Information for Simulation and Evolving Global Energy Distribution')") runum
   write(nu_info,"(/'# ',T5,'Number of cells in combustion grid:',T40,I7)") ncells
   write(nu_info,"('# ',T5,'Melt surface area: ',T38,E25.14,' m^2')") area_melt_surf
   write(nu_info,"(/'# ',T5,'Fuel in flow rate: ',T38,E25.14,' kg/s')") fr_f
   write(nu_info,"('# ',T5,'Oxygen in flow rate: ',T38,E25.14,' kg/s')") fr_ox
   write(nu_info,"('# ',T5,'Nitrogen in flow rate: ',T38,E25.14,' kg/s')") fr_n2
   write(nu_info,"('# ',T5,'Total in flow rate: ',T38,E25.14,' kg/s')") fr_n2+fr_ox+fr_f
   write(nu_info,"(/'# ',T5,'Heat value in with fuel: ',T38,E25.14,' W')") fr_f*q0*h_0
   write(nu_info,'("  ")')

   write(nu_info,"('# PlotData')")
endif

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  summary_file_init
!
! Purpose:  Initialize run summary file
!           (This file duplicates non-column data from the 'info'//runum//'c.plt' file)
!
! Outputs
!   Files opened:   'summary'//runum//'c.txt'   (nu_sum=35)
!
!======================================================================
subroutine summary_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

filename=casedir//'\summary'//runum//'c.txt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_sum,file=filename,shared,position='append')
else
   open(nu_sum,file=filename,shared)
   write(nu_sum,"('# Case ',a,' Summary Data for Simulation and Evolving Global Energy Distribution')") runum
   write(nu_sum,"(/'# ',T5,'Number of cells in combustion grid:',T40,I7)") ncells
   write(nu_sum,"('# ',T5,'Melt surface area: ',T38,E25.14,' m^2')") area_melt_surf
   write(nu_sum,"(/'# ',T5,'Fuel in flow rate: ',T38,E25.14,' kg/s')") fr_f
   write(nu_sum,"('# ',T5,'Oxygen in flow rate: ',T38,E25.14,' kg/s')") fr_ox
   write(nu_sum,"('# ',T5,'Nitrogen in flow rate: ',T38,E25.14,' kg/s')") fr_n2
   write(nu_sum,"('# ',T5,'Total in flow rate: ',T38,E25.14,' kg/s')") fr_n2+fr_ox+fr_f
   write(nu_sum,"(/'# ',T5,'Heat value in with fuel: ',T38,E25.14,' W')") fr_f*q0*h_0
   write(nu_sum,'("  ")')
endif

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

   write (file_nu,'(/"# Energy Balance on Exterior Walls")')
   write (file_nu,'("# --------------------------------")')
   write (file_nu,"(/'# ',t5,'Total energy in:',t36,e25.14,' W')") q_react+q_h_in
   write (file_nu,"('# ',t9,'Energy in with flow:',t40,e25.14,' W')") q_h_in 
   write (file_nu,"('# ',t9,'Energy in via combustion:',t40,e25.14,' W')") q_react

   write (file_nu,"(/'# ',t5,'Total energy leaving furnace:',t36,e25.14,' W')") &
                                          q_exhaust+q_melt+q_wall_loss+q_rad_inlet+q_rad_outlet
   write (file_nu,"('# ',t9,'Energy out with flow:',t40,e25.14,' W')") q_exhaust
   write (file_nu,"('# ',t9,'Energy out through walls:',t40,e25.14,' W')") q_wall_loss 
   write (file_nu,"('# ',t9,'Net radiation out inlets:',t40,e25.14,' W')") q_rad_inlet 
   write (file_nu,"('# ',t9,'Net radiation out outlets:',t40,e25.14,' W')") q_rad_outlet     
   write (file_nu,"('# ',t9,'Energy out to melt:',t40,e25.14,' W')") q_melt
   write (file_nu,"('# ',t13,'from flame:',t44,e25.14,' W')") qm_flame !net rad into melt directly from flame (media) (W)
   write (file_nu,"('# ',t13,'from surfaces:',t44,e25.14,' W')") qm_bnds  !net rad into melt from boundary surfaces (W)
   write (file_nu,"('# ',t13,'from convection:',t44,e25.14,' W')") qm_conv  !net heat into melt from convection at surface (W)
   write (file_nu,"('# ',t13,'(sum of froms):',t44,e25.14,' W')") qm_flame + qm_bnds + qm_conv

   write (file_nu,'(/"# Gas Energy Balance")')
   write (file_nu,'("# --------------------------------")')
   write (file_nu,"(/'# ',t5,'Total energy in:',t36,e25.14,' W')") q_react+q_h_in
   write (file_nu,"('# ',t9,'Energy in with flow:',t40,e25.14,' W')") q_h_in 
   write (file_nu,"('# ',t9,'Energy in via combustion:',t40,e25.14,' W')") q_react

   write (file_nu,"(/'# ',t5,'Energy out with or from the gas:',t36,e25.14,' W')") q_exhaust + qconv_total + qm_conv &
                          + qa_sum_to_melt + qa_sum_to_wall + qa_sum_to_inlet + qa_sum_to_exit
   write (file_nu,"('# ',t9,'Energy out with flow',t40,e25.14,' W')") q_exhaust
   write (file_nu,"('# ',t9,'Convection to walls',t40,e25.14,' W')") qconv_total
   write (file_nu,"('# ',t9,'Convection to melt surface',t40,e25.14,' W')") qm_conv
   write (file_nu,"('# ',t9,'Radiation from flame to melt:',t40,e25.14,' W')") qa_sum_to_melt
   write (file_nu,"('# ',t9,'Radiation from flame to walls:',t40,e25.14,' W')") qa_sum_to_wall
   write (file_nu,"('# ',t9,'Radiation from flame to inlets:',t40,e25.14,' W')") qa_sum_to_inlet
   write (file_nu,"('# ',t9,'Radiation from flame to exits:',t40,e25.14,' W')") qa_sum_to_exit
   write (file_nu,'(/"# --------------------------------")')

   !write (file_nu,"(/'# ',t5,'Total energy out:',t36,e25.14,' W')") &
   !write (file_nu,"(/'# ',t5,'Total energy leaving furnace:',t36,e25.14,' W')") &
   !                                       q_exhaust+q_melt+q_wall_loss+q_rad_inlet+q_rad_outlet
   !write (file_nu,"('# ',t9,'Energy out through walls:',t40,e25.14,' W')") q_wall_loss 
   !write (file_nu,"('# ',t9,'Radiation out through inlets:',t40,e25.14,' W')") q_rad_inlet 
   !write (file_nu,"('# ',t9,'Radiation out through outlets:',t40,e25.14,' W')") q_rad_outlet     
   !write (file_nu,"('# ',t9,'Energy out to melt:',t40,e25.14,' W')") q_melt
   !write (file_nu,"('# ',t13,'from flame:',t44,e25.14,' W')") qm_flame !net rad into melt directly from flame (media) (W)
   !write (file_nu,"('# ',t13,'from surfaces:',t44,e25.14,' W')") qm_bnds  !net rad into melt from boundary surfaces (W)
   !write (file_nu,"('# ',t13,'from convection:',t44,e25.14,' W')") qm_conv  !net heat into melt from convection at surface (W)
   !write (file_nu,"('# ',t5,'energy incident to melt:',t36,e25.14,' W')") qa_melt
   !write (file_nu,"('# ',t5,'energy emitted from melt:',t36,e25.14,' W')") qe_melt
   !write (file_nu,"('# ',t9,'Energy out with flow:',t40,e25.14,' W')") q_exhaust

   !write (file_nu,"(/'# ',t5,'rad energy out of media:',t36,e25.14,' W')") q_rad_vol_tot 
   !write (file_nu,"(/'# ',t5,'Rad energy out of media:',t36,e25.14,' W')") q_incident_total 
   !write (file_nu,"(/'# ',t5,'Total energy emmitted by media:',t36,e25.14,' W')") q_incident_total 
   !write (file_nu,"('# ',t5,'Total energy absorbed by media:',t36,e25.14,' W')") qa_sum_to_open_cell 
   !write (file_nu,"('# ',t5,'Net energy leaving by radiation:',t36,e25.14,' W')") q_incident_total  
   !      !net rad energy out of volume from participating media
   !write (file_nu,"('# ',t9,'To walls',t40,e25.14,' W')") q_incident_wall
   !write (file_nu,"('# ',t9,'To inlets',t40,e25.14,' W')") q_incident_inlet
   !write (file_nu,"('# ',t9,'To exits',t40,e25.14,' W')") q_incident_exit
   !write (file_nu,"('# ',t9,'To melt surface',t40,e25.14,' W')") q_incident_melt
   !write (file_nu,"('# ',t5,'Total energy absorbed by media:',t36,e25.14,' W')") qa_sum_to_open_cell 

   !write (file_nu,"(/'# ',t5,'Total energy leaving gas:',t36,e25.14,' W')") &
   !                       qe_sum_from_open - qa_sum_to_open + qconv_total + qm_conv + q_exhaust
   !write (file_nu,"('# ',t9,'Net radiation leaving gas:',t36,e25.14,' W')") qe_sum_from_open - qa_sum_to_open
   !write (file_nu,"('# ',t9,'Convection to walls',t40,e25.14,' W')") qconv_total
   !write (file_nu,"('# ',t9,'Convection to melt surface',t40,e25.14,' W')") qm_conv
   !write (file_nu,"('# ',t9,'Convection out with flow',t40,e25.14,' W')") q_exhaust

   write (file_nu,"(/'# ',t5,'Mass in:',t36,e25.14,' kg/s')") gfin 
   write (file_nu,"('# ',t5,'Mass out:',t36,e25.14,' kg/s')") gfex
   write (file_nu,"(/'# ',t5,'Fuel mass out:',t36,e25.14,' kg/s')") fuel_out
   h_fuel_out=fuel_out*q0*h_0
   write (file_nu,"('# ',t5,'Fuel energy out:',t36,e25.14,' W')") h_fuel_out
   !if (qe_scale_back==0) then
   !    write (file_nu,"(/'# ',t5,'qe scale backs: none')") 
   !else
   !   write (file_nu,"(/'# ',t5,'qe scale backs: cnt,tot,avg:',t36,i6,2e25.14)") &
   !                          qe_scale_back,qe_scale_back_amount,qe_scale_back_amount/qe_scale_back
   !endif
   write (file_nu,"(/'# ',t5,'Mean temperature:',t36,e25.14,' K')") Tave
   write (file_nu,"('# ',t5,'Mean exit temperature:',t36,e25.14,' K')") Tmean_ex
   write (file_nu,"('# ',t5,'Mean wall temperature:',t36,e25.14,' K')") avg_wall_T

   !debug prints
   if (smf_clipped_cnt>0) then
		write (file_nu,"('# ',t5,'Maximum soot mass fraction:',t36,e25.14)") smf_max
		write (file_nu,"('# ',t5,'Not clipped soot mass fraction:',t36,e25.14)") smf_max_noclip
		write (file_nu,"('# ',t5,'Number of cells clipped:',t36,i9)") smf_clipped_cnt
   endif
   !end debug prints

   !write (file_nu,"(/'# ',t5,'No. calls to enth_to_T:',i9,'     No. times NIST set flipped:',i9)") &
   !                                                                  track_enth_calls,track_enth_flips
   if (cycling==1) write (file_nu,"(/'# ',T5,'Cycle count:',T36,i5)") cycle_count

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  fchg_file_init
!
! Purpose:  Initialize flux change file (Average Change in Melt Surface Heat Flux)
!
! Outputs
!   Files opened:   'fchg'//runum//'c.plt'     nu_flx=111
!
! Only called if cycling>0
!
!======================================================================
subroutine fchg_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!open flux change file
filename=casedir//'\fchg'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open (nu_flx,file=filename,position='append')
else
   open(nu_flx,file=filename)
   write(nu_flx,*) '# Mean Change in Melt Surface Heat Flux - log plot'
   write(nu_flx,'("  ")')
   write(nu_flx,*) '# PlotData '
   write(nu_flx,*) '# Iter          Flux_change            Relative_change'
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  Tave_file_init
!
! Purpose:  Initialize mass average temperature file
!
! Outputs
!   Files opened:   'Tave'//runum//'c.plt'   (nu_Tave=95)
!
!======================================================================
subroutine Tave_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

filename=casedir//'\Tave'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_Tave,file=filename,shared,position='append')
else
   open(nu_Tave,file=filename,shared)
   write(nu_Tave,*) '# Combustion space mean temperature'
   write(nu_Tave,*) '# PlotData '
   !write(nu_Tave,*) '# Iteration      Temperature (K)'

   write(nu_Tave,'(/"# Iter      ", &
                "Mean Gas Temp (K)     ", &
                "Mean Exit Temp        ", &
                "Mean Wall Temp"/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  hresid_file_init
!
! Purpose:  Initialize normalized mean enthalpy residual file
!
! Outputs
!   Files opened:   'hresid'//runum//'c.plt'    nu_hres=99
!
!======================================================================
subroutine hresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
filename=casedir//'\hresid'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_hres,file=filename,shared,position='append')
else
   open(nu_hres,file=filename,shared)
   write(nu_hres,*) '# Mean enthalpy residual - log plot'
   write(nu_hres,*) '# PlotData '
   write(nu_hres,'(/"# Iteration      ", &
                    "Residual         ", &
                    "(qin-qout)/qin   ", &
                    "Presolve_resid "/)')
endif
return
end


!======================================================================
!======================================================================
!               NOT USED
!======================================================================
! Routine:  pre_resid_file_init
!
! Purpose:  Initialize pre-solve residual files
!
!           Used when ipre_resid=1
!
! Outputs
!   Files opened:   'gresidp'//runum//'c.plt'       nu_gresp =120
!   Files opened:   'gresid_xtrap'//runum//'c.plt'  nu_gresxp=121
!   Files opened:   'mresidp'//runum//'c.plt'       nu_mresp =122
!
!======================================================================
subroutine pre_resid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize pre-solve normalized mean gas PDE basic residual file
!           for enthalpy, pressure, and momentum 
filename=casedir//'\gresidp'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresp,file=filename,shared,position='append')
else
   open(nu_gresp,file=filename,shared)
   write(nu_gresp,*) '# Mean residuals (pre-solve) - log plot'
   write(nu_gresp,*) '# PlotData '
   write(nu_gresp,'(/"# Iter ", &
            "      Pre-Enthalpy      ", &
            "      Pre-Pressure      ", &
            "     Pre-x-momentum     ", &
            "     Pre-y-momentum     ", &
            "     Pre-z-momentum     "/)')
endif

!Initialize pre-solve normalized mean gas PDE basic residual file
!           for species and k-epsilon model residuals
filename=casedir//'\gresid_xtrap'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresxp,file=filename,shared,position='append')
else
   open(nu_gresxp,file=filename,shared)
   write(nu_gresxp,*) '# Mean residuals: major species, k-epsilon (pre-solve) - log plot'
   write(nu_gresxp,*) '# PlotData '
   write(nu_gresxp,'(/"# Iter ", &
            "      Pre-Fuel          ", &
            "      Pre-O2            ", &
            "      Pre-CO2           ", &
            "      Pre-N2            ", &
            "      Pre-Epsilon       ", &
            "      Pre-k             "/)')
endif

!Initialize pre_solve minor species residuals file
filename=casedir//'\mresidp'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstypm>0) then
   open(nu_mresp,file=filename,shared,position='append')
else
   open(nu_mresp,file=filename,shared)
   write(nu_mresp,*) '# Minor Species Residuals (Pre-solve) - log plot'
   write(nu_mresp,*) '# PlotData '
   write(nu_mresp,'(/"# Iteration ", &
                " Pre-CH4                 ", &
                " Pre-O2                  ", &
                " Pre-CO                  ", &
                " Pre-H2O                 ", &
                " Pre-CO2                 ", &
                " Pre-N2                  ", &
                " Pre-NO                  ", &
                " Pre-Soot "/)')
endif
return
end


!======================================================================
!======================================================================
!            NOT USED
!======================================================================
! Routine:  aft_resid_file_init  
!
! Purpose:  Initialize normalized mean gas PDE basic residual file
!           for enthalpy, pressure, and momentum
!
!           Used when iaft_resid=1
!
! Outputs
!   Files opened:   'gresid'//runum//'c.plt'       nu_gres = 97
!   Files opened:   'gresid_xtra'//runum//'c.plt'  nu_gresx= 98
!   Files opened:   'mresid'//runum//'c.plt'       nu_mres =100
!
!======================================================================
subroutine aft_resid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize after-solve normalized mean gas PDE basic residual file
!           for enthalpy, pressure, and momentum 
filename=casedir//'\gresid'//runum//'c.plt'
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

!Initialize after-solve normalized mean gas PDE basic residual file
!           for species and k-epsilon model residuals
filename=casedir//'\gresid_xtra'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresx,file=filename,shared,position='append')
else
   open(nu_gresx,file=filename,shared)
   write(nu_gresx,*) '# Mean residuals: major species, k-epsilon - log plot'
   write(nu_gresx,*) '# PlotData '

   write (nu_gresx,'(/"# Iter ", &
            "        Fuel            ", &
            "        O2              ", &
            "        CO2             ", &
            "        N2              ", &
            "        Epsilon         ", &
            "        k               "/)')
endif

!Initialize after-solve minor species residuals file
filename=casedir//'\mresid'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstypm>0) then
   open(nu_mres,file=filename,shared,position='append')
else
   open(nu_mres,file=filename,shared)
   write(nu_mres,*) '# Minor Species Residuals - log plot'
   write(nu_mres,*) '# PlotData '
   write(nu_mres,'(/"# Iteration ", &
                " CH4                     ", &
                " O2                      ", &
                " CO                      ", &
                " H2O                     ", &
                " CO2                     ", &
                " N2                      ", &
                " NO                      ", &
                " Soot                    "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  gresid_file_init
!
! Purpose:  Initialize normalized mean gas PDE basic residual file
!           for enthalpy, pressure, momentum, and pressure correction
!
!           Used when igresid=1
!
! Outputs
!   Files opened:   'gresid'//runum//'c.plt'    nu_gres=97
!
!======================================================================
subroutine gresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
!Initialize after-solve normalized mean gas PDE basic residual file
!           for enthalpy, pressure, and momentum 
filename=casedir//'\gresid'//runum//'c.plt'
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
! Purpose:  Initialize pre-solve normalized mean gas PDE basic residual file
!           for enthalpy, pressure, momentum, and pressure correction
!
!           Used when igresidp=1
!
! Outputs
!   Files opened:   'gresidp'//runum//'c.plt'    nu_gresp=120
!
!======================================================================
subroutine pre_gresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
!Initialize pre-solve normalized mean gas PDE basic residual file
!           for enthalpy, pressure, and momentum 
filename=casedir//'\gresidp'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresp,file=filename,shared,position='append')
else
   open(nu_gresp,file=filename,shared)
   write(nu_gresp,*) '# Mean residuals (pre-solve) - log plot'
   write(nu_gresp,*) '# PlotData '
   write(nu_gresp,'(/"# Iter ", &
            "      Pre-Enthalpy      ", &
            "      Pre-Pressure      ", &
            "     Pre-x-momentum     ", &
            "     Pre-y-momentum     ", &
            "     Pre-z-momentum     "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  gresid_xtra_file_init
!
! Purpose:  Initialize normalized mean gas PDE basic residual file
!           for species and k-epsilon model residuals
!
!           Used when igresid=1
!
! Outputs
!   Files opened:   'gresid_xtra'//runum//'c.plt'    nu_gresx=98
!
!======================================================================
subroutine gresid_xtra_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
!Initialize after-solve normalized mean gas PDE basic residual file
!           for species and k-epsilon model residuals
filename=casedir//'\gresid_xtra'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresx,file=filename,shared,position='append')
else
   open(nu_gresx,file=filename,shared)
   write(nu_gresx,*) '# Mean residuals: major species, k-epsilon - log plot'
   write(nu_gresx,*) '# PlotData '

   write (nu_gresx,'(/"# Iter ", &
            "        Fuel            ", &
            "        O2              ", &
            "        CO2             ", &
            "        N2              ", &
            "        Epsilon         ", &
            "        k               "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  pre_gresid_xtra_file_init
!
! Purpose:  Initialize pre-solve normalized mean gas PDE basic residual file
!           for species and k-epsilon model residuals
!
!           Used when igresidp=1
!
! Outputs
!   Files opened:   'gresid_xtrap'//runum//'c.plt'    nu_gresxp=121
!
!======================================================================
subroutine pre_gresid_xtra_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
 
!Initialize pre-solve normalized mean gas PDE basic residual file
!           for species and k-epsilon model residuals
filename=casedir//'\gresid_xtrap'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_gresxp,file=filename,shared,position='append')
else
   open(nu_gresxp,file=filename,shared)
   write(nu_gresxp,*) '# Mean residuals: major species, k-epsilon (pre-solve) - log plot'
   write(nu_gresxp,*) '# PlotData '
   write(nu_gresxp,'(/"# Iter ", &
            "      Pre-Fuel          ", &
            "      Pre-O2            ", &
            "      Pre-CO2           ", &
            "      Pre-N2            ", &
            "      Pre-Epsilon       ", &
            "      Pre-k             "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  mresid_file_init
!
! Purpose:  Initialize minor species residuals file
!
!           Used when imresid=1
!
! Outputs
!   Files opened:   'mresid'//runum//'c.plt'    nu_mres=100
!
!======================================================================
subroutine mresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize after-solve minor species residuals file
filename=casedir//'\mresid'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstypm>0) then
   open(nu_mres,file=filename,shared,position='append')
else
   open(nu_mres,file=filename,shared)
   write(nu_mres,*) '# Minor Species Residuals - log plot'
   write(nu_mres,*) '# PlotData '
   write(nu_mres,'(/"# Iteration ", &
                " CH4                     ", &
                " O2                      ", &
                " CO                      ", &
                " H2O                     ", &
                " CO2                     ", &
                " N2                      ", &
                " NO                      ", &
                " Soot                    "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  pre_mresid_file_init
!
! Purpose:  Initialize minor species residuals file
!
!           Used when imresidp=1
!
! Outputs
!   Files opened:   'mresidp'//runum//'c.plt'    nu_mresp=122
!
!======================================================================
subroutine pre_mresid_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize pre_solve minor species residuals file
filename=casedir//'\mresidp'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstypm>0) then
   open(nu_mresp,file=filename,shared,position='append')
else
   open(nu_mresp,file=filename,shared)
   write(nu_mresp,*) '# Minor Species Residuals (Pre-solve) - log plot'
   write(nu_mresp,*) '# PlotData '
   write(nu_mresp,'(/"# Iteration ", &
                " Pre-CH4                 ", &
                " Pre-O2                  ", &
                " Pre-CO                  ", &
                " Pre-H2O                 ", &
                " Pre-CO2                 ", &
                " Pre-N2                  ", &
                " Pre-NO                  ", &
                " Pre-Soot "/)')
endif
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  twall_file_init
!
! Purpose:  Initialize average wall temperature file
!           Initialize wall energy totals file
!
! Outputs
!   Files opened:   'twallavg'//runum//'c.plt'   (nu_twala=22)
!                   'wall_tot'//runum//'c.plt'   (nu_wtot=25)  commented out
!
!======================================================================
subroutine twall_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize average wall temperature file
filename=casedir//'\twallavg'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_twala,file=filename,shared,position='append')
else
   open(nu_twala,file=filename,shared)
   write(nu_twala,*) '# Average Wall Temperature'
   write(nu_twala,*) '# PlotData '
   write(nu_twala,*) '# Iteration    Temperature (K)'
endif

!Initialize wall energy totals file
!filename=casedir//'\wall_tot'//runum//'c.plt'
!inquire(file=filename,exist=exst0)
!if (exst0.and.irstyp>0) then
!   open(nu_wtot,file=filename,shared,position='append')
!else
!   open(nu_wtot,file=filename,shared)
!   write(nu_wtot,*) '# Wall Energy Totals'
!   write(nu_wtot,*) '# PlotData '
!   write(nu_wtot,'(/"# iter      ", &
!                "qa:incident          ", &
!                "qe:emis+reflec      ", &
!                "qls_a:loss to amb     ", &
!                "qls_g:conv to gas     ", &
!                "qa-qe-qls_a-qls_g"/)')
!endif

return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  rad_file_init
!
! Purpose:  Initialize radiation details file
!           Used when irad_detail=1
! Outputs
!   Files opened:   'rad_detail'//runum//'c.plt'   (nu_rad=26)
!   Files opened:   'radw_detail'//runum//'c.plt'  (nu_radw=27)      commented out
!
!======================================================================
subroutine rad_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize radiation details file for volume and melt
filename=casedir//'\rad_detail'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_rad,file=filename,shared,position='append')
else
   open(nu_rad,file=filename,shared)
   write(nu_rad,*) '# Radiation Details from Volume '
   write(nu_rad,*) '# qe = radiation emission rate (W), qa radiation absorption rate (W)'
   write(nu_rad,*) '# PlotData '
   write(nu_rad,'(/"# Iter      ", &
                "qe from h20, co2      ", &
                "qe from soot          ", &
                "qe sum                ", &
                "qe from media         ", &
                "qa at melt            ", &
                "qa at wall            ", &
                "qa in media           ", &
                "qa at exit            ", &
                "qa at inlet           ", &
                "qa sum                ", &
                "q diff"/)')                
endif

!---------------------------------------------------------------------
!Initialize radiation details file for walls
!filename=casedir//'\radw_detail'//runum//'c.plt'
!inquire(file=filename,exist=exst0)
!if (exst0.and.irstyp>0) then
!   open(nu_radw,file=filename,shared,position='append')
!else
!   open(nu_radw,file=filename,shared)
!   write(nu_radw,*) '# Radiation Details from Walls'
!   write(nu_radw,*) '# PlotData '
!   write(nu_radw,'(/"# iter      ", &
!                "qe from wall          ", &
!                "qa at melt            ", &
!                "qa at wall            ", &
!                "qa at open            ", &
!                "qa at exit            ", &
!                "qa at inlet           ", &
!                "qa sum                ", &
!                "q diff"/)')                
!endif

return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  rad_radiosity_file_init
!
! Purpose:  Initialize wall boundary exchange computaion convergence file
!           Used when irad_rad=1
! Outputs
!   Files opened:   'conv_wall'//runum//'c.plt'    (nu_cwal=50)
!
!======================================================================
subroutine rad_radiosity_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize wall boundary exchange computaion convergence file
!Prints mean relative change in radiosity
filename=casedir//'\conv_wall'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_cwal,file=filename,shared,position='append')
else
   open(nu_cwal,file=filename,shared)
   write(nu_cwal,*) '# Mean relative radiosity change - log plot'
   write(nu_cwal,*) '# PlotData '
   write(nu_cwal,*) '# Iteration      (Bi-Bi_prev)/Bi_prev'
endif
return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  rad_heat_flux_file_init
!
! Purpose:  Initialize mean heat flux from volume file
!           Used when irad_qc=1
! Outputs
!   Files opened:   'qc_ave'//runum//'c.plt'       (nu_qcave=51)
!
!======================================================================
subroutine rad_heat_flux_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize mean heat flux from volume file
filename=casedir//'\qc_ave'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_qcave,file=filename,shared,position='append')
else
   open(nu_qcave,file=filename,shared)
   write(nu_qcave,*) '# Mean heat flux from volume'
   write(nu_qcave,*) '# PlotData '
   write(nu_qcave,*) '# Iteration      qc'
endif
return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  rad_wall_loss_file_init
!
! Purpose:  Initialize mean heat loss to ambient file
!           Used when irad_amb=1
! Outputs
!   Files opened:   'q_amb'//runum//'c.plt'        (nu_qamb=52)
!
!======================================================================
subroutine rad_wall_loss_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize mean heat loss to ambient file
filename=casedir//'\q_amb'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_qamb,file=filename,shared,position='append')
else
   open(nu_qamb,file=filename,shared)
   write(nu_qamb,*) '# Mean heat loss to ambient'
   write(nu_qamb,*) '# PlotData '
   write(nu_qamb,*) '# Iteration      q_amb'
endif
return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  rad_melt_file_init
!
! Purpose:  Initialize mean heat flux to melt from walls file
!           Used when irad_qrs=1
! Outputs
!   Files opened:   'qrs'//runum//'c.plt'          (nu_qrs=53)
!
!======================================================================
subroutine rad_melt_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize mean heat flux to melt from walls file
filename=casedir//'\qrs'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_qrs,file=filename,shared,position='append')
else
   open(nu_qrs,file=filename,shared)
   write(nu_qrs,*) '# Mean heat flux to melt'
   write(nu_qrs,*) '# PlotData '
   write(nu_qrs,*) '# Iteration      qrs'
endif
return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  rad_convection_file_init
!
! Purpose:  Initialize mean convection from gas flow file
!           Used when irad_conv=1
! Outputs
!   Files opened:   'q_conv'//runum//'c.plt'       (nu_qconv=54)
!
!======================================================================
subroutine rad_convection_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize mean convection from gas flow file
filename=casedir//'\q_conv'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_qconv,file=filename,shared,position='append')
else
   open(nu_qconv,file=filename,shared)
   write(nu_qconv,*) '# Mean convection from gas flow'
   write(nu_qconv,*) '# PlotData '
   write(nu_qconv,*) '# Iteration      q_conv'
endif
return
end
  

!======================================================================
!======================================================================
!======================================================================
! Routine:  soot_file_init
!
! Purpose:  Initialize soot calibration file
!
! Outputs
!   Files opened:   'soot_cal'//runum//'c.plt'   (nu_soot=31)
!
! Only called if isoot_cal > 0
!
!======================================================================
subroutine soot_file_init
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

!Initialize aoxid file
filename=casedir//'\soot_cal'//runum//'c.plt'
inquire(file=filename,exist=exst0)
if (exst0.and.irstyp>0) then
   open(nu_soot,file=filename,shared,position='append')
else
   open(nu_soot,file=filename,shared)
   write(nu_soot,*) '# Soot oxidation and formation kinetic constants '
   write(nu_soot,*) '# PlotData '
   write(nu_soot,'(/"# Iter      ", &
                "Aoxid                 ", &
                "Aform                 ", &
                "HT to melt            ", &
                "E needed by melt      ", &
                "Q ratio               ", &
                "Total soot (kg)"/)')
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
!   Files written:  'convg'//crun//'c.plt'
!                   'convg'//crun//'d.plt'
!                   'convg'//crun//'p.plt'
!
!======================================================================
subroutine convg_file_init(iu)
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0

if (iu==nug) then
   filename=casedir//'\convg'//runum//'c.plt'
   inquire(file=filename,exist=exst0)
   if (exst0.and.irstyp>0) then
      open(iu,file=filename,shared,position='append')
   else
      open(iu,file=filename,shared)
      write(iu,*) '# Combustion space mean and max mass residual - log plot'
      write(iu,*) '# PlotData'
      write(iu,*) '# iteration  average mass residual  max mass residual'
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
      write(iu,*) '# iteration  average mass residual  max mass residual'
   endif

elseif(iu==nup)then
   filename=casedir//'\convg'//runum//'p.plt'
   inquire(file=filename,exist=exst0)
   if (exst0) then
      open(iu,file=filename,shared,position='append')
   else
      open(iu,file=filename,shared)
      write(iu,*) '# Partical mean and max mass residual - log plot'
      write(iu,*) '# PlotData'
      write(iu,*) '# iteration  average mass residual  max mass residual'
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
!   Files read:    'convg'//crun//'.txt'   
!                  'convg'//crun//'d.txt'
!                  'convg'//crun//'p.txt'
!
! Outputs
!   Files written: 'convg'//crun//'bg'//ps//'.txt'
!                  'convg'//crun//'bd'//ps//'.txt'
!                  'convg'//crun//'bp'//ps//'.txt'          
!
!======================================================================
subroutine convg_file_autocopy(iu)
use gbl_var
implicit real*8 (a-h,o-z)
logical exst0
integer it
real*8 resid,rmax
ps='0'
print *,'in convg_file_autocopy'
if    (iu==nug) then
  filename ='convg'//crun//'.txt'
  filename2='convg'//crun//'bg'//ps//'.txt'
elseif(iu==nud) then
  filename ='convg'//crun//'d.txt'
  filename2='convg'//crun//'bd'//ps//'.txt'
elseif(iu==nup) then
  filename ='convg'//crun//'p.txt'
  filename2='convg'//crun//'bp'//ps//'.txt'
endif

inquire(file=filename,exist=exst0)
if (exst0) then
rewind(iu)
else
   write(ncon,*) ' warning: file' ,filename, 'does not exist'
   return
endif

open(nutmp,file=filename2)
write(nutmp,*) '# iteration average mass residual      max mass residual'
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
!   Files written: 'convg'//crun//'bg'//ps//'.txt'
!                  'convg'//crun//'bd'//ps//'.txt'
!                  'convg'//crun//'bp'//ps//'.txt'          
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

if    (iu==nug) then; filename='convg'//crun//'b' //ps//'.txt'
elseif(iu==nud) then; filename='convg'//crun//'bd'//ps//'.txt'
elseif(iu==nup) then; filename='convg'//crun//'bp'//ps//'.txt'
endif

open(nutmp,file=filename,iostat=iso,err=20)
write(ncon,*) 'opened auto convg filename: ',filename
write(nutmp,*,iostat=ios,err=30) '# iteration  average mass residual  max mass residual'

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
call stop_run("Error in convg_file_autocopy1.")
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

!======================================================================
!======================================================================
!======================================================================
! Routine:  T_ave_calc
!
! Purpose:  Calculate volume and mass mean Temperature
!
!======================================================================
subroutine T_ave_calc
use gbl_var
implicit real*8 (a-h,o-z)
Tmax_c=zero
Tave=zero
Tave_vol=zero
rho_vol_tot=zero
voltot=zero
do k=4,lp-2,2
do j=4,np-2,2
do i=4,mp-2,2
   if(ibcell(i,j,k).ne.0)cycle
   vol=vol_c(i,j,k)
   Tave= Tave+T(i,j,k)*dnst(i,j,k)*vol 
   rho_vol_tot=rho_vol_tot+dnst(i,j,k)*vol
   Tave_vol= Tave_vol+T(i,j,k)*vol 
   voltot = voltot + vol
   Tmax_c=max(Tmax_c,T(i,j,k))
enddo;enddo;enddo

Tave=Tave*T0/rho_vol_tot
Tave_vol=Tave_vol*T0/voltot
Tmax_c=Tmax_c*T0

return 
end
!======================================================================
!======================================================================
!======================================================================
! Routine:  abort
!
! Purpose:  abort run while debugging
!
!======================================================================
subroutine abort
use gbl_var
implicit real*8 (a-h,o-z)

write (ncon,*) "run aborted"
return 
end


!======================================================================
!======================================================================
!======================================================================
!    GFM does not currently have an unsteady computation capability
!
!     OSAVE saves system state at previous time step
!     Old Code NOT USED
!    
!       9/97
!======================================================================
SUBROUTINE OSAVE
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
!     Gas phase properties
!----------------------------------------------------------------------
IF (STEADY) RETURN
UGO=UG
PO=P
TGO=T
DNSTO=DNST
THETAO=THETA
GFO=GF
IF (NPHAS.EQ.1) RETURN
!----------------------------------------------------------------------
!     Liquid/Solid phase properties
!----------------------------------------------------------------------
DNO=DN
DUO=DU
DTO=DT
DCO=DC
RETURN
END

