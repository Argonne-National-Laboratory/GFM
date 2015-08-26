!======================================================================
! conv.f90     Convert between melt and combustion domains
!======================================================================
! Rewritten by Lottes 6-16-2005
!
! Routines:
!
!       set_surface_conditions
!
!       conv_heat_flux - interpolates melt surface heat flux from
!                        combustion space grid to melt space grid
!                      - scales heat flux to meet exit boundary
!                        temperature specification
!                      - treats surface larger than combustion space
!                        interface as a shaded (zero flux) free surface
!                      - called from sbc
!
!       conv_surf_temp - interpolates the melt surface temperature distribution
!                        from the melt surface grid to the combustion space 
!                        glass surface grid
!                      - writes it....t.dat file (with aternate grid number)
!                      - handles solid coverage on surface
!                      - called from sprint
!
!       conv_heat_flux_regen - interpolates melt surface heat flux from
!                        combustion space alternate burner configuration grid
!                        to the melt space grid of a regenerative burner
!                      - scales heat flux to meet exit boundary
!                        temperature specification
!                      - treats surface larger than combustion space
!                        interface as a shaded (zero flux) free surface
!                      - called from sbc
!
!       conv_surf_temp_regen - interpolates the melt surface temperature distribution
!                        from the melt surface grid to the combustion space 
!                        glass surface grid for the alternate burner configuration
!                        in a regenerative furnace
!                      - writes it....t.dat file (with alternate grid number)
!                      - handles solid coverage on surface
!                      - called from sprint
!
!======================================================================
!======================================================================
!======================================================================
!  The set_surface_conditions routine was part of the sbc routine.
!  Code was moved here and is called after the read of the restart file so
!  that items saved in the restart file are available to the following 
!  conv_heat_flux routines.
!======================================================================
subroutine set_surface_conditions
use gbl_var
implicit double precision (a-h,o-z)

!------------------------
! Determine enthalpy at glass melting point (Tmltr)

call Tlg_to_h(Tmltr,h_Tmltr)
!if (udf_cln == 1) then
!	!constant specific heat
!	h_Tmltr = udf_cl(1)%f*Tmltr
!else
!	if (Tmltr < udf_cl(1)%t) then
!		h_Tmltr = udf_cl(1)%f*Tmltr
!	else
!		h_Tmltr = udf_cl(1)%f*Tmltr
!		do n=1,udf_cln-1
!			if (Tmltr > udf_cl(n+1)%t) then
!				h_Tmltr = h_Tmltr + ((udf_cl(n)%f+udf_cl(n+1)%f)/2)*(udf_cl(n+1)%t-udf_cl(n)%t)
!			else
!				call udf_int(udf_cln,Tmltr,g0) !set g0 to the specific heat at the glass melting point		
!				h_Tmltr = h_Tmltr + ((udf_cl(n)%f+g0)/2)*(Tmltr-udf_cl(n)%t)
!				exit
!			endif
!		enddo
!		if (Tmltr > udf_cl(udf_cln)%t) then
!			h_Tmltr = h_Tmltr + udf_cl(udf_cln)%f*(Tmltr-udf_cl(udf_cln)%t)
!		endif
!	endif
!endif

!------------------------
!Find energy needed to heat molten glass (qglass_need).

if (udf_cln == 1) then
	!constant specific heat
	qglass_need = (pca%fr2+psa%fr2)*udf_cl(1)%f*(t_exit-Tmltr)
else
	qglass_need = 0
	n1st=0
	idone=0
	do n=1,udf_cln
		if (udf_cl(n)%t < Tmltr) cycle
		if (n1st==0) then
			T_lo = Tmltr
			n1st=1
		else
			T_lo = udf_cl(n-1)%t
		endif
		if (t_exit > udf_cl(n)%t) then
			T_hi = udf_cl(n)%t
		else
			T_hi = t_exit
			idone = 1
		endif
		call udf_int(udf_cln,T_lo,g0) !set g0 to the specific heat at the lo temperature
		call udf_int(udf_cln,T_hi,g1) !set g1 to the specific heat at the hi temperature
		qglass_need = qglass_need + (pca%fr2+psa%fr2)*((g0+g1)/2)*(T_hi-T_lo)
		if (idone==1) exit
	enddo

	!handle block beyond last specific heat array temperature point
	if (t_exit > udf_cl(udf_cln)%t) then
		if (Tmltr > udf_cl(udf_cln)%t) then
			T_lo = Tmltr
		else 
			T_lo = udf_cl(udf_cln)%t
		endif	 
		call udf_int(udf_cln,T_lo,g0) !set g0 to the specific heat at the lo temperature
		call udf_int(udf_cln,t_exit,g1) !set g1 to the specific heat at the hi temperature
		qglass_need = qglass_need + (pca%fr2+psa%fr2)*((g0+g1)/2)*(t_exit-T_lo)
	endif
endif

!qglass_need_new = qglass_need
!Add energy needed to heat molten glass. Assume constant specific heat and no liquid coming in. Lottes 5/19/05
!qglass_need = (pca%fr2+psa%fr2)*udf_cl(1)%f*(t_exit-Tmltr) 

!------------------------
! Determine needed energy.  H_needed was init to zero in setup.
h_needed=h_needed+qglass_need !lottes 5/19/05

!Add energy needed to fuse the batch/cullet
!h_needed=h_needed+pca%fr2*h0_c+psa%fr2*h0_s !lottes 5/19/05
qmelt_s_need=psa%fr2*h0_s !energy needed to melt sand; lottes 5/19/05
qmelt_c_need=pca%fr2*h0_c !energy needed to melt cullet; lottes 5/19/05
h_needed=h_needed+qmelt_c_need+qmelt_s_need

call conv_heat_flux
if (regen==1) call conv_heat_flux_regen

!IF(REGEN.EQ.1) CALL CONV_R(1)
H_TOT=H_TOT+Qc_tot
WRITE (6,'(/"  In-Flows:")')
IF (FR2_LG.GT.SMALL) WRITE (6,"(T5'Liquid Glass:',T30,F10.8,' kg/s')") FR2_LG
IF (PCA%FR2.GT.SMALL) WRITE (6,"(T5'Cullet:',T30,F10.8,' kg/s')") PCA%FR2
IF (PSA%FR2.GT.SMALL) WRITE (6,"(T5'Batch:',T30,F10.8,' kg/s')") PSA%FR2
IF (GB%FR2.GT.SMALL) WRITE (6,"(T5'Gas Bubble:',T30,F10.8,' mg/s')") GB%FR2*1D6
IF (Qc_tot.GT.SMALL) WRITE (6,"(T5'Heat Input:',T30,F10.8,' MW')") Qc_tot*1D-6
IF (IDEBUG.EQ.1) THEN
   WRITE (20,*) '  Liquid Glass:',FR2_LG,' kg/s'
   WRITE (20,*) '  Cullet:',PCA%FR2,' kg/s'
   WRITE (20,*) '  Batch:',PSA%FR2,' kg/s'
   WRITE (20,*) '  Bubble:',GB%FR2,' kg/s'
   WRITE (20,*) '  Heat Input:',Qc_tot,' W'
ENDIF

!write start conditions in the summary and information files
if (iInfo>0 .and. irstyp==0) call sum_info_file_start_conditions(nu_info) 
if (isum >0 .and. irstyp==0) call sum_info_file_start_conditions(nu_sum) 

if (iInfo>0) then
   write (nu_info,'(/"# Iter ", &
               "  Net Heat to Liquid  ", &
               "     Heat to Batch    ", &
               "    Heat to Cullet    ", &
               "     Sum Incoming     ", &
               "      Wall Loss       ", &
               "        Factor        ", &
               "    Factor Change     "/)')
endif

return
end


!======================================================================
!======================================================================
!======================================================================
subroutine conv_heat_flux
use gbl_var
implicit double precision (a-h,o-z)
real(8),allocatable :: qrs_bak(:,:)

qrs=zero !init here because will not be setting for monoblock edges in later loops

filename=casedir//'\it'//runum//'m.dat'
inquire(file=filename,exist=exst)
if (.not.exst) then
   write (6,*) '  Fatal error, file: ',filename,' does not exist.'
   call stop_run("Fatal error, file: it....m.dat does not exist.")
endif

open (nu_itm,file=filename)
read (nu_itm,*) itn !itn=1 means heat flux distribution from combustion space is present
read (nu_itm,*) avg_wall_T  !Mean Wall Temperature in Combustion Space (K)"
if (avg_wall_T < 800.0d0) avg_wall_T = 1500.0d0 !part of scheme to keep batch temp from dropping too low @@@
read (nu_itm,*) Tmax_c !Maximum Temperature in Combustion Space (K)
read (nu_itm,*) qc_tot !Total surface heat rate [W] incoming to melt surface

if (irstyp == 0) then
   !initialize previous cycle surface heat flux to uniform
   qrs_prev=zero
   do j=4,np-2  ,2; jd2=j/2
   do i=4,i_me,2;   id2=i/2
      if (ibcell(i,j,lp)==4) then
         qrs_prev(id2,jd2)=qc_tot/area_surf_tot
      endif
   enddo;enddo !qrs_prev is in watts/m**2
endif

if (itn>0) then ! surface heat flux distribution from combustion space case

   !call grid_c !read in combustion space grid    Now read in setup
   !allocate (qrsc(mp_c/2,np_c/2))
   !allocate (qrs_m(mz,nz))
   qrslg=zero
   qrsc=zero
   qrs_m=zero

   !-----------------------------------------------------
   !Lottes: 4/12/05 Interpolation of heat flux from combustion space
   !        to the melt space surface grid.
   !        We only get here if itn>0 (4/28/05)
   read (nu_itm,'(A)') title
   read (nu_itm,'(A)') title
   
   !Read in surface radiation flux into qrsc array in combustion space grid.
   jb=1

   do
      je=jb+10
      je=min(je,np_c/2)
      
      read (nu_itm,'(A)') title
      read (nu_itm,'(A)') title
      
      do i=1,mp_c/2
         !read (nu_itm,'(F11.3,11E12.4)') g0,(qrsc(i,j),j=jb,je)
         read (nu_itm,'(F11.3,11E11.4)') g0,(qrsc(i,j),j=jb,je)
      enddo
      
      jb=je+1
      if (jb > np_c/2) exit
   enddo
   close(nu_itm)
   !Done reading surface heat flux.

   !For purposes of interpolation between combustion space and melt space grids,
   !surface heat flux boundary edge and corner values are set equal to cell face
   !center value.
   
   if (interior_surf_wall==0) then
      !set corner values of heat flux equal to that of corner cell
      qrsc(i_mb_c/2-1,j_mb_c/2-1)=qrsc(i_mb_c/2,j_mb_c/2)
      qrsc(i_me_c/2+1,j_mb_c/2-1)=qrsc(i_me_c/2,j_mb_c/2)
      qrsc(i_mb_c/2-1,j_me_c/2+1)=qrsc(i_mb_c/2,j_me_c/2)
      qrsc(i_me_c/2+1,j_me_c/2+1)=qrsc(i_me_c/2,j_me_c/2)
      !set edge values of heat flux equal to that of edge cell
      do ic=i_mb_c/2,i_me_c/2
         qrsc(ic,j_mb_c/2-1)=qrsc(ic,j_mb_c/2)
         qrsc(ic,j_me_c/2+1)=qrsc(ic,j_me_c/2)
      enddo
      do jc=j_mb_c/2,j_me_c/2
         qrsc(i_mb_c/2-1,jc)=qrsc(i_mb_c/2,jc)
         qrsc(i_me_c/2+1,jc)=qrsc(i_me_c/2,jc)
      enddo
   else
      !@complete this later --> handle cases with nonrectangular melt surface interface @@@
      do ic=i_mb_c,i_me_c,2
      do jc=j_mb_c,j_me_c,2
         if (ibcell_c(ic,jc,2)==4.and.ibcell_c(ic-1,jc,2)==1.and.qrsc(ic/2-1,jc/2) < 1.0d-10) then
            qrsc(ic/2-1,jc/2)=qrsc(ic/2,jc/2)
         endif  
     enddo;enddo
   endif
   
   !Interpolate heat fluxes from combustion grid to melt grid
   
   ! Bilinear interpolation between grids on the melt surface:
   ! Note cell centers have even indexes, i.
   ! A linear interpolation/extrapolation to point n is: f(n) = [f(i)*(1-b)] + [f(i+2)*b]
   ! where b = [x(n)-x(i)] / [x(i+2)-x(i)]
   ! in the interior x(i)<= x(n) < x(i+1) 
   ! near boundaries x(n) can lie outside the interval [x(i),x(i+1)]
   
   jc=j_mb_c !initial start index for south combustion cell bound
   do jm=j_mb,j_me,2 !loop over cell centered melt y grid points in interface
      !Find south cell row index bound in combustion grid for melt point at jm
      do while (ys_c(jc+2) < ys_m(jm) .and. jc < j_me_c-2)
         jc=jc+2
         !if (jc == np_c) exit
      enddo

      b_sn=(ys_m(jm)-ys_c(jc))/(ys_c(jc+2)-ys_c(jc))
      ic=i_mb_c   
      do im=i_mb,i_me,2 !loop over cell centered melt x grid points in interface
         !Find west cell row index bound in combustion grid for melt point at jm
         do while (xs_c(ic+2) < xs_m(im) .and. ic < i_me_c)
            ic=ic+2
            if (ic == mp_c) exit
         enddo

         !Here xs_c(ic-1) < xs_m(im) < xs_c(ic+1)
         if (ibcell(im,jm,lp).ne.4) cycle
         b_we=(xs_m(im)-xs_c(ic))/(xs_c(ic+2)-xs_c(ic))
         qrs_w=qrsc(ic/2  ,jc/2+1)*b_sn+qrsc(ic/2  ,jc/2)*(1-b_sn)
         qrs_e=qrsc(ic/2+1,jc/2+1)*b_sn+qrsc(ic/2+1,jc/2)*(1-b_sn)
         qrs(im/2,jm/2)=qrs_e*b_we + qrs_w*(1-b_we)
         qrs_m(im/2,jm/2)=qrs(im/2,jm/2)
       enddo
   enddo
   !qrs and qrs_m are both fluxes at this point

   !Debug print to compare interpolated heat fluxes with original   
   !call sprint_m
   !call sprint_c

   !--------------------------------------------------
   ! Lottes 4-6-2005
   ! At this point qrs has the locally interpolated 
   ! radiation heat flux at the melt domain surface
   
   ! convert qrs from heat flux to heat rate (W) to a surface patch
   qcm_tot=zero
   do i=4,i_me,2
   do j=4,np-2,2
      if (mod(ibcell(i,j,lp),10)==4 .and. ibcell(i,j,lp-2) < 1) then
         qrs(i/2,j/2)=qrs(i/2,j/2)*dx(i)*dy(j) ! Resets QRS to have rad. energy rate (W)
         qcm_tot=qcm_tot+qrs(i/2,j/2)
      endif
   enddo;enddo
   !qcm_tot = total watts to surface

   !Renormalize interpolated values so total heat through melt
   !surface matches that from the combustion space (qc_tot) 
   flux_in_max=small
   flux_in_min=big     
   qrs_tot=zero
	qs_in=zero
	qs_out=zero
   do j=4,np-2,2
   do i=4,i_me,2
   !do i=2,i_me,2
   !do j=2,np,2
      if (mod(ibcell(i,j,lp),10)==4 .and. ibcell(i,j,lp-2) < 1) then
         qrs(i/2,j/2)=qrs(i/2,j/2)*qc_tot/qcm_tot 
         qrs_m(i/2,j/2)=qrs_m(i/2,j/2)*qc_tot/qcm_tot
			if (qrs(i/2,j/2) > 0) then; qs_in = qs_in + qrs(i/2,j/2); else; qs_out = qs_out - qrs(i/2,j/2); endif
         qrs_tot=qrs_tot+qrs(i/2,j/2)
         flux_in_max=max(flux_in_max,qrs_m(i/2,j/2)) !save maximum flux value  
         flux_in_min=min(flux_in_min,qrs_m(i/2,j/2)) !save minimum flux value
      endif
   enddo;enddo

   ! Lottes: here QRS has surface energy rate for each surface patch [W]
   !         qc_tot = total surface heat rate [W]
 
else !itn=0
   close(nu_itm)

   !Lottes 4/12/05: This loop initializes QRS assuming distribution of uniform energy qc_tot
   flux_in=qc_tot/area_surf_tot
   flux_in_max=flux_in
   flux_in_min=flux_in
   qrs_tot=0
	qs_out=zero
   do i=4,i_me,2;   id2=i/2
   do j=4,np-2  ,2; jd2=j/2
      if (ibcell(i,j,lp)==4) then
         qrs(id2,jd2)=flux_in*dx(i)*dy(j)
         qrs_m(id2,jd2)=flux_in
         qrs_tot=qrs_tot+qrs(id2,jd2)
      endif
   enddo;enddo !qrs is in watts
	qs_in=qrs_tot
endif
!at this point qrs is watts, qrs_m is flux, qc_tot is the total watts to the melt surface

!if restarting, use the current iteration value of wall heat loss from the restart file
if (irstyp==0) then !otherwise estimate it from the expected exit temperature
   iwall_loss_est=1 !causes qloss to calc wall loss based on T_exit at wall
   call qloss 
endif

!Lottes 7/20/05 - heat flux types
! 
! 1 - uniform, scaled
! 2 - scaled combustion space distribution
! 3 - uniform, user specified
! 4 - combustion space distriution, unscaled
if (iheat_flux_type < 3) then !scaled uniform or scaled combustion calculated heat flux
   if (qs_in > 0) then
	   scale_in=(h_needed-eb_heat+q_wall_loss_tot+scale_out*qs_out)/qs_in
   else
	   scale_in=0 ! all radiation is out of melt, so there are no positive cells to scale
	endif
   !facq=(h_needed-eb_heat+q_wall_loss_tot)/qc_tot 
else ! no scaling means both scale factors should = one
      scale_out=one
		if (qs_in > 0) then
   	   scale_in=(h_needed-eb_heat+q_wall_loss_tot+scale_out*qs_out)/qs_in
         !facq=one
		else
		   scale_in = -1.0d30 !just to notify user that no heat into the melt is not reasonable
		endif 
endif

!if cycling, we may want the most up to date wall loss value here @@@
if (irstyp==0) then !doing new start
	if(iInfo>0) then
		write (nu_info,"('# ',T5,'Specified exit temperature T_exit:',T36,E25.14,' K')") T_exit
		write (nu_info,"('# ',T5,'Estimated wall loss at T_exit:',T36,E25.14,' W')") q_wall_loss_tot
	endif
	if(isum>0)  then
		write (nu_sum,"('# ',T5,'Specified exit temperature T_exit:',T36,E25.14,' K')") T_exit
		write (nu_sum,"('# ',T5,'Estimated wall loss at T_exit:',T36,E25.14,' W')") q_wall_loss_tot
	endif
endif
flxh(3,5)=H_TOT
q_comb=h_needed-eb_heat

!qrs_tot=zero !Global holds total surface radiation + convection [W] Lottes 5/5/05
!if (flux_in_min<zero) then
!   !Want to adjust to keep qrs positive (using mean flux)
!   alpha=-flux_in_min/((qc_check/area_surf_tot)-flux_in_min)
!else
!   alpha=rfqrs !normal relaxation factor
!endif

! Scaling of the heat distribution if in scaling mode
flux_adj_max=flux_in_max
flux_adj_min=flux_in_min

if(iheat_flux_type < 3) then
   flux_adj_max=small
   flux_adj_min=big
   qrs_tot=0
   qs_in=0
   do i=4,i_me,2; id2=i/2 ! Lottes 4/28/05: i > i_me is refiner area
      flxh(i+1,5)=flxh(i-1,5)
   do j=4,np-2,2; jd2=j/2
      if (ibcell(i,j,lp).ne.4) cycle
      !qrs_m(id2,jd2)=qrs_m(id2,jd2)*facq
      !qrs(id2,jd2)=qrs(id2,jd2)*facq ! Surface energy adjusted by amount needed if scaled
      if (qrs_m(id2,jd2) > zero) then
         qrs_m(id2,jd2)=qrs_m(id2,jd2)*scale_in
         qrs(id2,jd2)=qrs(id2,jd2)*scale_in ! Surface energy adjusted by amount needed if scaled
      else
         qrs_m(id2,jd2)=qrs_m(id2,jd2)*scale_out
         qrs(id2,jd2)=qrs(id2,jd2)*scale_out ! Surface energy going out scaled down
      endif
      !relax with saved, possibly scaled, surface heat in for restart
      !if(irstyp==1) qrs(id2,jd2) = rfqrs*qrs(id2,jd2) + rfqrs_c*qrs_prev(id2,jd2)
      !if(irstyp==1) qrs(id2,jd2) = alpha*qrs(id2,jd2) + (one-alpha)*qrs_prev(id2,jd2)
      !qrslg(id2,jd2)=qrs(id2,jd2)
      flxh(i+1,5)=flxh(i+1,5)+qrs(id2,jd2)
      flux_adj_max=max(flux_adj_max,qrs_m(id2,jd2)) !save maximum flux value
      flux_adj_min=min(flux_adj_min,qrs_m(id2,jd2)) !save minimum flux value
      qrs_tot = qrs_tot + qrs(id2,jd2) !Lottes 5/5/05: (possibly scaled) total surface heat in
		qs_in =qs_in + max(qrs(id2,jd2),zero)
   enddo;enddo

	if (iadjf==1) then
	   !Print melt surface heat flux in it....m_adjflux.dat file for review
	   fmr='(/2x,''  x  /  y'',11(g13.5))'
	   filename=casedir//'\it'//runum//'m_adjflux.dat'
	   open(nu_adjf,file=filename)
	   write(nu_adjf,*) qc_tot,"  input qc_tot"
	   write(nu_adjf,*) flux_in_max,"  maximum flux input value" 
	   write(nu_adjf,*) flux_in_min,"  minimum flux input value"
	   write(nu_adjf,*) "       "
	   write(nu_adjf,*) qrs_tot,"  adjusted total qrs"
	   write(nu_adjf,*) flux_adj_max,"  maximum flux adjusted value" 
	   write(nu_adjf,*) flux_adj_min,"  minimum flux adjusted value"
	      write(nu_adjf,'(1x/t10,"adjusted surface radiation heat flux (W/m**2)")')
	
	   j1=2
	   do
	      j2=j1+20
	      j2=min(j2,np)
	      write(nu_adjf,fmr) (y(j),j=j1,j2,2)
	      do i=2,mp,2
	         write(nu_adjf,'(f11.3,11e13.5)') x(i),(qrs_m(i/2,j/2),j=j1,j2,2)
	      enddo
	      j1=j2+2
	      if (j1>np) exit
	   enddo
	   close(nu_adjf)
	   !Done writing it....m_adjflux.dat file
	endif 
endif

!--------------------------------------------------
! This code relaxes a flux distribution coming from the combustion space.
! It should probably not be done on a restart that simply resumes a melt space run.@@@
! Relaxation is used to stablize the melt surface b.c.'s during cycling runs.
! The user might not want to do this.

!if (itn>0.and.cycling==1) then
if (itn>0) then
   allocate(qrs_bak(mz,nz))

   irelax_nogood=1
   rfadj=0.8d0
   rfqrs=0.7d0
   rfqrs_c=1-rfqrs
   qrs_bak=qrs_m ! save surface heat flux distribution
   !----------------------------
   ! Surface flux relaxation loop
   do while(irelax_nogood==1)
      flux_adj_max=small
      flux_adj_min=big
      qrelxtot=0
      do j=4,np-2,2
      do i=4,i_me,2
         if (ibcell(i,j,lp).ne.4) cycle
         qrs_m(i/2,j/2) = rfqrs*qrs_bak(i/2,j/2) + rfqrs_c*qrs_prev(i/2,j/2)
         qrelxtot=qrelxtot + qrs_m(i/2,j/2)*dx(i)*dy(j)
         flux_adj_max=max(flux_adj_max,qrs_m(i/2,j/2)) !save maximum flux value
         flux_adj_min=min(flux_adj_min,qrs_m(i/2,j/2)) !save minimum flux value
      enddo;enddo
      qrelx_ave=qrelxtot/area_surf_tot

      !Find max deviation from the mean relaxed heat flux
      qdevmax=0
      do j=4,np-2,2
      do i=4,i_me,2
         if (ibcell(i,j,lp).ne.4) cycle
         qdevmax=max(abs(qrs_m(i/2,j/2)-qrelx_ave),qdevmax)
      enddo;enddo

      if (iadjr==1) then
         !Put relaxed melt surface heat flux into it....m_relax.dat file for review     
         fmr='(/2x,''  x  /  y'',11(g13.5))'
         filename=casedir//'\it'//runum//'m_relax.dat'
         open(nu_adjr,file=filename)
         write(nu_adjr,*) qc_tot,"  input qc_tot"
         write(nu_adjr,*) flux_in_max,"  maximum flux input value" 
         write(nu_adjr,*) flux_in_min,"  minimum flux input value"
         write(nu_adjr,*) "       "
         write(nu_adjr,*) qrelxtot,"  relaxed total qrs"
         write(nu_adjr,*) flux_adj_max,"  maximum flux relaxed value" 
         write(nu_adjr,*) flux_adj_min,"  minimum flux relaxed value"
         write(nu_adjr,'(1x/t10,"relaxed surface radiation heat flux (W/m**2)")')
      
         j1=2
         do
            j2=j1+20
            j2=min(j2,np)
            write(nu_adjr,fmr) (y(j),j=j1,j2,2)
            do i=2,mp,2
               write(nu_adjr,'(f11.3,11e13.5)') x(i),(qrs_m(i/2,j/2),j=j1,j2,2)
            enddo
            j1=j2+2
            if (j1>np) exit
         enddo
         close(nu_adjr)
         !Done writing it....m_relax.dat file
      endif

      if (irelax_nogood==0.or. (qdevmax/qrelx_ave < 2.0d0) .or. rfqrs < 0.2d0 ) exit      

      rfqrs=rfqrs*rfadj
      rfqrs_c=1-rfqrs

   enddo
   deallocate(qrs_bak)

endif !surface heat flux relaxation

!convert heat flux qrs_m into heat to surface patches (W), qrs array
qrs_tot=0
qs_in=0
do i=4,i_me,2; id2=i/2 ! Lottes 4/28/05: i > i_me is refiner area
   flxh(i+1,5)=flxh(i-1,5)
do j=4,np-2,2; jd2=j/2
   if (ibcell(i,j,lp).ne.4) cycle
   qrs(id2,jd2)=qrs_m(id2,jd2)*(dx(i)*dy(j))
   !qrslg(id2,jd2)=qrs(id2,jd2)
   flxh(i+1,5)=flxh(i+1,5)+qrs(id2,jd2)
   qrs_tot = qrs_tot + qrs(id2,jd2) !Lottes 5/5/05: (possibly scaled & relaxed) net surface heat in
	qs_in = qs_in + max(qrs(id2,jd2),zero) !Lottes 11/24/07 (possibly scaled & relaxed surface heat heat in
enddo;enddo



! qrs_prev is calculated from a possibly rescaled qrs immediately before saving in the restart file.

!scale_in=(h_needed-eb_heat+q_wall_loss_tot+scale_out*qs_out)/qs_in
!facq=(h_needed-eb_heat+q_wall_loss_tot)/qc_tot !set for print, even if not scaled

!Save surface distribution and total for possible rescaling when wall loss recalculated
qc_surf0=qrs !if scaling is on, then qc_surf0 is scaled
qrs_tot_base =qrs_tot ! save initial scaled net energy in through surf
qs_in_base = qs_in    ! save initial scaled energy in through surface
qrslg=qrs 

return
end

!======================================================================
!======================================================================
!======================================================================
!
! conv_surf_temp interpolates the melt surface temperature distribution
! from the melt surface grid to the combustion space glass surface grid.
!
! This routine also creates the it//runum//t.dat file
!
! Come here if routine called from sprint routine at end of melt run, 
! only if the combustion grid has been read. (Otherwise would be unable 
! to interpolate the melt grid to the combustion grid.)
!
!======================================================================
subroutine conv_surf_temp
use gbl_var
implicit double precision (a-h,o-z)

! BTnnnn.DAT will have Temperature along x direction for 
! j=4 (y-direction, probably in a wall, why?
! is it even computed?
! k=(LP+2)/2 is halfway between top and bottom
! legacy code below needs fixing, does not make sense
!iprint_bt=0
!if(iprint_bt==1)then
!   filename=casedir//'\bt'//runum//'.dat'
!   nu_bt=109
!   open(nu_bt,file=filename)
!   j=4
!   k=(Lp+2)/2
!   do i=4,mp-2,2
!      N=LG(I,J,K)%T
!      N1=LG(I,J,K)%T*1.8-460
!      WRITE (nu_bt,'(F5.2,2I5)') X(I),N,N1
!   enddo
!   close(nu_bt)
!endif

allocate(tgs_c(mp_c/2,np_c/2))
tgs_c=zero
!Interpolate temperature from melt grid to combustion grid

!Compute surface temperature in the melt. This temperature is 
!a combination of particle and liquid temperature. Where particles
!cover the surface the surface temperature should be the particle
!temperature at the top of the particle pile or the liquid T of
!liquid covering the particles. The batch model is not currently
!detailed enough to provide this temperature, so the code below attempts
!to estimate this surface T.
allocate(Tgs_m(mz,nz))
Tgs_m=zero
Tmean_surf_melt=0 !initialize to calculate mean surface temp. in melt space
!Assume X  direction chargers at left (low X)
!Charger rows could be marked...
!Detailed batch simulations have shown that the batch surface reaches batch
!reaction T = batch melt T = Tmltr very quickly, therefore Tmltr is the min 
!batch surface T.
!Try batch surf T = (1-wtip) * batch melt T + wtip * liquid at batch tip T
! Ttip = glass liquid temperature in cell beyond the tip.
do jm=j_mb,j_me,2
   itip=0
   Ttip=Tmltr !If batch extends to far wall, then surf T will equal melt T
!do im=4,i_me,2
do im=i_me,i_mb,-2
   if (ibcell(im,jm,lp) .ne. 4) cycle
   if (      nps_c > 0 .and. pc0(im/2,jm/2,lz-1)%th > 1.0d-1  &
       .and. nps_s > 0 .and. ps0(im/2,jm/2,lz-1)%th > 1.0d-1) then 
      !Tgs_m(im/2,jm/2)=(pc0(im/2,jm/2,lz-1)%th *pc(im/2,jm/2,lz-1,1)%T  &
      !     +ps0(im/2,jm/2,lz)%th *ps(im/2,jm/2,lz-1,1)%T) &
      !    /(pc0(im/2,jm/2,lz)%th+ps0(im/2,jm/2,lz-1)%th) !needs fixing for mixed batch system
      itip=1
      wcwall=0.02d0 !weight applied to combustion space mean wall T
      wtip=( (ps(im/2,jm/2,lz-1,1)%T+pc(im/2,jm/2,lz-1,1)%T)/(2*Tmltr) )**0.5d+0 !weight applied to tip T to estimate batch surf T
      Tgs_m(im/2,jm/2)=(wtip*Ttip+(1-wtip)*Tmltr)*(1-wcwall)+wcwall*avg_wall_T
   elseif(   nps_c > 0 .and. pc0(im/2,jm/2,lz-1)%th > 1.0d-1) then
      !Tgs_m(im/2,jm/2)=pc(im/2,jm/2,lz-1,1)%T  !@@@@@@@@@@@@@
      !Tgs_m(im/2,jm/2)=Tmltr
      !Tgs_m(im/2,jm/2)=(avg_wall_T+Tmltr)/2 
      itip=1
      wcwall=0.02d0 !weight applied to combustion space mean wall T
      wtip=(pc(im/2,jm/2,lz-1,1)%T/Tmltr)**0.5d+0 !weight applied to tip T to estimate batch surf T
      Tgs_m(im/2,jm/2)=(wtip*Ttip+(1-wtip)*Tmltr)*(1-wcwall)+wcwall*avg_wall_T
   elseif(   nps_s > 0 .and. ps0(im/2,jm/2,lz-1)%th > 1.0d-1) then
      !Tgs_m(im/2,jm/2)=ps(im/2,jm/2,lz-1,1)%T  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      !Tgs_m(im/2,jm/2)=Tmltr
      !Tgs_m(im/2,jm/2)=(avg_wall_T+Tmltr)/2 
      itip=1
      wcwall=0.02d0 !weight applied to combustion space mean wall T
      wtip=(ps(im/2,jm/2,lz-1,1)%T/Tmltr)**0.5d+0 !weight applied to tip T to estimate batch surf T
      Tgs_m(im/2,jm/2)=(wtip*Ttip+(1-wtip)*Tmltr)*(1-wcwall)+wcwall*avg_wall_T
   else
      Tgs_m(im/2,jm/2)=lg(im,jm,lp-2)%T
      if (itip==0) Ttip=lg(im,jm,lp-2)%T
   endif
   Tmean_surf_melt=Tmean_surf_melt + Tgs_m(im/2,jm/2)*dx(im)*dy(jm)
enddo;enddo
Tmean_surf_melt=Tmean_surf_melt/area_surf_tot  !debug check this against T_surf_mean below

!call sprint_m !debug print, Lottes 6-28-05

! Bilinear interpolation between grids on the melt surface:
! Note cell centers have even indexes, i.
! A linear interpolation/extrapolation to point n is: f(n) = [f(i)*(1-b)] + [f(i+2)*b]
! where b = [x(n)-x(i)] / [x(i+2)-x(i)]
! in the interior x(i)<= x(n) < x(i+1) 
! near boundaries x(n) can lie outside the interval [x(i),x(i+1)]
area_tot=zero
T_surf_mean=zero !initialize to calculate mean surface temp. in combustion space
jm=j_mb !initial start index for south melt cell bound
do jc=j_mb_c,j_me_c,2 !loop over cell centered melt y grid points in interface
   !Find south cell row index bound in combustion grid for melt point at jm
   do while (ys_m(jm+2) < ys_c(jc) .and. jm < j_me-2)
      jm=jm+2
      !if (jm == np) exit
   enddo

   b_sn=(ys_c(jc)-ys_m(jm))/(ys_m(jm+2)-ys_m(jm))
   im=i_mb  
   do ic=i_mb_c,i_me_c,2 !loop over cell centered melt x grid points in interface
      !Find west cell row index bound in combustion grid for melt point at jm
      do while (xs_m(im+2) < xs_c(ic) .and. im < i_me-2)
         im=im+2
         !if (im == i_me) exit
         enddo

      !Here xs_c(ic-1) < xs_m(im) < xs_c(ic+1)
      if (ibcell_c(ic,jc,2).ne.4) cycle
      b_we=(xs_c(ic)-xs_m(im))/(xs_m(im+2)-xs_m(im))

      !T_sw=Tgs_m(im/2,  jm/2)
      !T_nw=Tgs_m(im/2,  jm/2+1)
      !T_se=Tgs_m(im/2+1,jm/2)
      !T_ne=Tgs_m(im/2+1,jm/2+1)   

      T_w=Tgs_m(im/2,  jm/2+1)*b_sn+Tgs_m(im/2,  jm/2)*(1-b_sn)
      T_e=Tgs_m(im/2+1,jm/2+1)*b_sn+Tgs_m(im/2+1,jm/2)*(1-b_sn)
      Tgs_c(ic/2,jc/2)=(T_e*b_we + T_w*(1-b_we))

      area_tot = area_tot + dx_c(ic)*dy_c(jc)
      T_surf_mean = T_surf_mean + Tgs_c(ic/2,jc/2)*dx_c(ic)*dy_c(jc)
    enddo
enddo

T_surf_mean = T_surf_mean/area_tot


!------------------------------------------------------------------------
!Lottes: This section is used during automatic cycling.
!        It reads in the last surface temperature file,
!        compares the last average surface temperature with the current
!        average temperature,  
!        and saves the temperature change in the T_change file.

filename=casedir//'\it'//runum//'t.dat'
inquire (file=filename,exist=exst)
if (cycling==1 .and. exst) then
   !At this point, itn should be >0 and tgs_c contains the current
   !melt surface temperatures, area_tot is the total surface area
   allocate (last_tgs_c(mp_c/2,np_c/2))
   last_tgs_c=zero
   open(nu_surfT,file=filename)
   read (nu_surfT,*) indicates_have_array
   if (indicates_have_array > 0) then
      !Have a file with temperature array, 
      !otherwise only have default file initially so do not process.
      !Skip over items not needed    
      read (nu_surfT,*) dontneed
      read (nu_surfT,*) dontneed
      read (nu_surfT,*) dontneed
      read (nu_surfT,'(A)') title
      read (nu_surfT,'(A)') title
   
      !Read in last surface temperature Tgs array in combustion space grid.
      jb=1
      do
         je=jb+10
         je=min(je,np_c/2)
         read (nu_surfT,'(A)') title
         read (nu_surfT,'(A)') title
         do i=1,mp_c/2
            read (nu_surfT,'(F11.3,11f11.1)') dontneed,(last_tgs_c(i,j),j=jb,je)
         enddo
         jb=je+1
         if (jb > np_c/2) exit
      enddo
      close(nu_surfT)

      !Calculate average temperature differences
      sum_dif=zero
      sum_rel_dif=zero

      do i=1,mp_c/2
      do j=1,np_c/2
         area = dx_c(i) * dy_c(j)
         sum_dif = sum_dif + abs(tgs_c(i,j)-last_tgs_c(i,j))*area
         if (last_tgs_c(i,j)*area .ne. zero) then
            sum_rel_dif = sum_rel_dif + abs((tgs_c(i,j)-last_tgs_c(i,j))/last_tgs_c(i,j))*area
         endif
      enddo;enddo

      avg_temp_change = sum_dif/area_tot
      avg_rel_change = sum_rel_dif/area_tot

      !save changes in temperature change file
      if (iTchg==1) write(nu_Tchg,*) nlg,avg_temp_change,avg_rel_change !save average changes in T change file
   else
      !did not have array
      close(nu_surfT)
   endif
   deallocate (last_tgs_c)
endif


!call sprint_c
!-----------------------------------------------------
!Lottes: This section writes out surface temperature
!        interpolated to the combustion grid
!        and some global heat rate parameters

filename=casedir//'\it'//runum//'t.dat'
itn=1
open(nu_surfT,file=filename)

write(nu_surfT,*) itn," Distribution indicator"
write(nu_surfT,*) h_needed-eb_heat+q_wall_loss_tot," Energy required by melter (W)"
write(nu_surfT,*) qc_tot," Combustion Heat Transfer to Melter (W)"
write(nu_surfT,*) T_surf_mean," Mean Surface Temperature (K)"

write(nu_surfT,'(1X/T10,"Surface Temperature (K)")')

jb=2
do while (jb <= np_c)
   je=jb+20
   je=min(je,np_c)
   write(nu_surfT,' ')
   write(nu_surfT,'(" X / Y",T12,11F11.3)') (y_c(j),j=jb,je,2)
   do i=2,mp_c,2
      write(nu_surfT,'(F11.3,11F11.1)') x_c(i),(tgs_c(i/2,jc),jc=jb/2,je/2)
   enddo
   jb=je+2
enddo
close(nu_surfT)

return
end

!======================================================================
!======================================================================
!======================================================================
!
! conv_heat_flux_regen - interpolates melt surface heat flux from
!                  combustion space alternate burner configuration grid
!                  to the melt space grid of a regenerative burner
!                - scales heat flux to meet exit boundary
!                  temperature specification
!                - treats surface larger than combustion space
!                  interface as a shaded (zero flux) free surface
!                - called from sbc
!
!======================================================================
subroutine conv_heat_flux_regen
use gbl_var
implicit double precision (a-h,o-z)
real(8),allocatable :: qrs_bak(:,:)

filename=casedir//'\it'//runum_r//'m.dat'
inquire(file=filename,exist=exst)
if (.not.exst) then
   write (NU,*) '  Fatal error, file: ',filename,' does not exist.'
   call stop_run("Fatal error, second file: it....m.dat does not exist.")
endif

open (nu_itmr,file=filename)
read (nu_itmr,*) itn_r !itn_r=1 means heat flux distribution from combustion space is present
read (nu_itmr,*) avg_wall_T_r  !Mean Wall Temperature in Combustion Space (K)"
if (avg_wall_T_r < 800.0d0) avg_wall_T_r = 1500.0d0 !part of scheme to keep batch temp from dropping too low @@@
read (nu_itmr,*) Tmax_c_r !Maximum Temperature in Combustion Space (K)
Tmax_c=max(Tmax_c,Tmax_c_r)
read (nu_itmr,*) qc_tot_r !Total surface heat rate [W] incoming to melt surface

! 01-18-06
!Note that the 'cycle with melt first' capability requires that the default it####m.dat file
!be used, which means that itn_r may be 0.  


call grid_c_r !read in regen combustion space grid
allocate (qrsc_r(mp_c_r/2,np_c_r/2))
allocate (qrs_r(mz,nz))
allocate (qrs_m_r(mz,nz)) !keep unscaled version for debug print
!allocate (qrs_prev_r(mz,nz))
qrsc_r=zero
qrs_r=zero
qrs_m_r=zero

if (irstyp == 0) then
   !initialize previous cycle surface heat flux to uniform
   qrs_prev_r=zero
   do j=4,np-2  ,2; jd2=j/2
   do i=4,i_me,2;   id2=i/2
      if (ibcell(i,j,lp)==4) then
         qrs_prev_r(id2,jd2)=qc_tot_r/area_surf_tot
      endif
   enddo;enddo !qrs_prev is in watts/m**2
endif

if (itn_r>0) then ! surface heat flux distribution from second combustion space 

   !-----------------------------------------------------
   !Lottes: 4/12/05 Interpolation of heat flux from second combustion space
   !        to the melt space surface grid.
   !        We only get here if itn_r>0 (4/28/05)

   read (nu_itmr,'(A)') title
   read (nu_itmr,'(A)') title

   !Read in surface radiation flux into qrsc_r array in combustion space grid.
   jb=1

   do
      je=jb+10
      je=min(je,np_c_r/2)
   
      read (nu_itmr,'(A)') title
      read (nu_itmr,'(A)') title
   
      do i=1,mp_c_r/2
         !read (nu_itmr,'(F11.3,11e12.4)') g0,(qrsc_r(i,j),j=jb,je)
         read (nu_itmr,'(F11.3,11e11.4)') g0,(qrsc_r(i,j),j=jb,je)
      enddo
   
      jb=je+1
      if (jb > np_c_r/2) exit
   enddo
   close(nu_itmr)
   !Done reading surface heat flux.

   !For purposes of interpolation between combustion space and melt space grids,
   !surface heat flux boundary edge and corner values are set equal to cell face
   !center value.

   if (interior_surf_wall_r==0) then
      !set corner values of heat flux equal to that of corner cell
      qrsc_r(i_mb_c_r/2-1,j_mb_c_r/2-1)=qrsc_r(i_mb_c_r/2,j_mb_c_r/2)
      qrsc_r(i_me_c_r/2+1,j_mb_c_r/2-1)=qrsc_r(i_me_c_r/2,j_mb_c_r/2)
      qrsc_r(i_mb_c_r/2-1,j_me_c_r/2+1)=qrsc_r(i_mb_c_r/2,j_me_c_r/2)
      qrsc_r(i_me_c_r/2+1,j_me_c_r/2+1)=qrsc_r(i_me_c_r/2,j_me_c_r/2)
      !set edge values of heat flux equal to that of edge cell
      do ic=i_mb_c_r/2,i_me_c_r/2
         qrsc_r(ic,j_mb_c_r/2-1)=qrsc_r(ic,j_mb_c_r/2)
         qrsc_r(ic,j_me_c_r/2+1)=qrsc_r(ic,j_me_c_r/2)
      enddo
      do jc=j_mb_c_r/2,j_me_c_r/2
         qrsc_r(i_mb_c_r/2-1,jc)=qrsc_r(i_mb_c_r/2,jc)
         qrsc_r(i_me_c_r/2+1,jc)=qrsc_r(i_me_c_r/2,jc)
      enddo
   else
      !@complete this later --> handle cases with nonrectangular melt surface interface    @@@
      do ic=i_mb_c_r,i_me_c_r,2
      do jc=j_mb_c_r,j_me_c_r,2
         if (ibcell_c_r(ic,jc,2)==4.and.ibcell_c_r(ic-1,jc,2)==1.and.qrsc_r(ic/2-1,jc/2) < 1.0d-10) then
            qrsc_r(ic/2-1,jc/2)=qrsc_r(ic/2,jc/2)
         endif  
      enddo;enddo
   endif

   !Interpolate heat fluxes from combustion grid to melt grid

   ! Use basic formula:
   ! f(i) = [f(i-1)*(1-b)] + [f(i+1)*b]
   ! where b = [x(i)-x(i-1)] / [x(i+1)-x(i-1)]
   ! and 1-b = [x(i+1)-x(i)] / [x(i+1)-x(i-1)]

   jc=j_mb_c_r !initial start index for south combustion cell bound
   do jm=j_mb,j_me,2 !loop over cell centered melt y grid points in interface
      !Find south cell row index bound in combustion grid for melt point at jm
      do while (ys_c_r(jc+2) < ys_m(jm) .and. jc < j_me_c_r-2)
         jc=jc+2
         !if (jc == np_c_r) exit
      enddo

      b_sn=(ys_m(jm)-ys_c_r(jc))/(ys_c_r(jc+2)-ys_c_r(jc))
      ic=i_mb_c_r   
      do im=i_mb,i_me,2 !loop over cell centered melt x grid points in interface
         !Find west cell row index bound in combustion grid for melt point at jm
         do while (xs_c_r(ic+2) < xs_m(im) .and. ic < i_me_c_r)
            ic=ic+2
            !if (ic == mp_c_r) exit
         enddo
         !Here xs_c_r(ic-1) < xs_m(im) < xs_c_r(ic+1)
         if (ibcell(im,jm,lp).ne.4) cycle
         b_we=(xs_m(im)-xs_c_r(ic))/(xs_c_r(ic+2)-xs_c_r(ic))
         qrs_w=qrsc_r(ic/2  ,jc/2+1)*b_sn+qrsc_r(ic/2  ,jc/2)*(1-b_sn)
         qrs_e=qrsc_r(ic/2+1,jc/2+1)*b_sn+qrsc_r(ic/2+1,jc/2)*(1-b_sn)
         qrs_r(im/2,jm/2)=qrs_e*b_we + qrs_w*(1-b_we)
         qrs_m_r(im/2,jm/2)=qrs_r(im/2,jm/2) 
       enddo
   enddo

   !Debug print to compare interpolated heat fluxes with original   
   !call sprint_m
   !call sprint_c

   !-------------------------------------------------
   ! Lottes 4-6-2005
   ! At this point qrs_r has the locally interpolated 
   ! radiation heat flux at the melt domain surface

   ! convert qrs_r from heat flux to heat rate (W) to a surface patch
   qcm_tot_r=zero
   do i=4,i_me,2
   do j=4,np-2,2
      if (mod(ibcell(i,j,lp),10)==4 .and. ibcell(i,j,lp-2) < 1) then
         qrs_r(i/2,j/2)=qrs_r(i/2,j/2)*dx(i)*dy(j) ! Resets qrs_r to have rad. energy rate (W)
         qcm_tot_r=qcm_tot_r+qrs_r(i/2,j/2)
      endif
   enddo;enddo

   !Renormalize interpolated values so total heat through melt
   !surface matches that from the combustion space (qc_tot_r)   
   flux_in_max=small  
   flux_in_min=big
   qrs_tot_r=zero
	qs_in_r=zero
	qs_out_r=zero
   do i=4,i_me,2
   do j=4,np-2,2
      if (mod(ibcell(i,j,lp),10)==4 .and. ibcell(i,j,lp-2) < 1) then
         qrs_r(i/2,j/2)=qrs_r(i/2,j/2)*qc_tot_r/qcm_tot_r 
         qrs_m_r(i/2,j/2)=qrs_m_r(i/2,j/2)*qc_tot_r/qcm_tot_r 
			if (qrs_r(i/2,j/2) > 0) then; qs_in_r = qs_in_r + qrs_r(i/2,j/2); else; qs_out_r = qs_out_r - qrs_r(i/2,j/2); endif
         qrs_tot_r=qrs_tot_r+qrs_r(i/2,j/2)
         flux_in_max=max(flux_in_max,qrs_m_r(i/2,j/2)) !save maximum flux value  
         flux_in_min=min(flux_in_min,qrs_m_r(i/2,j/2)) !save minimum flux value
      endif
   enddo;enddo

   ! Lottes: here qrs_r has surface energy rate for each surface patch [W]
   !         qc_tot_r = total surface heat rate [W]
 
else !itn_r=0
   close(nu_itmr)

   !Lottes 4/12/05: This loop initializes qrs_r assuming distribution of uniform energy qc_tot_r
   flux_in=qc_tot_r/area_surf_tot
   flux_in_max=flux_in
   flux_in_min=flux_in
   qrs_tot_r=0
	qs_out_r=0
   do i=4,i_me,2;   id2=i/2
   do j=4,np-2  ,2; jd2=j/2
      if (ibcell(i,j,lp)==4) then
         qrs_r(id2,jd2)=flux_in*dx(i)*dy(j)
         qrs_m_r(id2,jd2)=flux_in
         qrs_tot_r=qrs_tot_r+qrs_r(id2,jd2)
      endif
   enddo;enddo !qrs_r is in watts here
	qs_in_r=qrs_tot_r
endif
! Lottes: here qrs_r has surface rad. energy rate [W]
!              qrs_m_r has flux,  qc_tot_r is the total watts going to the melt surface
!--------------------------------------------------------------------------------------
 

!if restarting use the current iteration value of wall heat loss from the restart file
if (irstyp==0) then !otherwise estimate it from the expected exit temperature
   iwall_loss_est=1 !causes qloss to calc wall loss based on T_exit at wall
   call qloss 
endif

!Lottes 7/20/05 - heat flux types
! 
! 1 - uniform, scaled
! 2 - scaled combustion space distribution
! 3 - uniform, user specified
! 4 - combustion space distriution, unscaled
if (iheat_flux_type < 3) then !scaled uniform or scaled combustion calculated heat flux
   if (qs_in_r > 0) then
      scale_in_r = (h_needed-eb_heat+q_wall_loss_tot+scale_out_r*qs_out_r)/qs_in_r
	else
	   scale_in_r = 0 ! no incoming energy to scale
   endif
   !facq_r=(h_needed-eb_heat+q_wall_loss_tot)/qc_tot_r
else ! no scaling means both scale factors should = one
   scale_in_r=one 
   scale_out_r=one
	!facq_r=one
endif

!if cycling, we may want the most up to date wall loss value here @@@
!Don't want to write this info out for regenerative furnace - should be nearly same for both configurations.
!if(iInfo>0) write (nu_info,"('# ',T5,'Estimated wall loss at T_exit:',T36,E25.14,' W')") q_wall_loss_tot

flxh(3,5)=H_TOT
q_comb=h_needed-eb_heat

! Scaling of the heat distribution if in scaling mode
flux_adj_max=flux_in_max
flux_adj_min=flux_in_min

if(iheat_flux_type < 3) then
   flux_adj_max=small
   flux_adj_min=big
   qrs_tot_r=0
   do i=4,i_me,2; id2=i/2 ! Lottes 4/28/05: i > i_me is refiner area
      flxh(i+1,5)=flxh(i-1,5)
   do j=4,np-2,2; jd2=j/2
      if (ibcell(i,j,lp).ne.4) cycle
      !qrs_m_r(id2,jd2)=qrs_m_r(id2,jd2)*facq
      !qrs_r(id2,jd2)=qrs_r(id2,jd2)*facq ! Surface energy adjusted by amount needed if scaled  
      if (qrs_m_r(id2,jd2) > zero) then
         qrs_m_r(id2,jd2)=qrs_m_r(id2,jd2)*scale_in_r
         qrs_r(id2,jd2)=qrs_r(id2,jd2)*scale_in_r ! Surface energy adjusted by amount needed if scaled
      else
         qrs_m_r(id2,jd2)=qrs_m_r(id2,jd2)*scale_out_r
         qrs_r(id2,jd2)=qrs_r(id2,jd2)*scale_out_r ! Surface energy adjusted by amount needed if scaled
      endif
      flxh(i+1,5)=flxh(i+1,5)+qrs_r(id2,jd2)
      flux_adj_max=max(flux_adj_max,qrs_m_r(id2,jd2)) !save maximum flux value
      flux_adj_min=min(flux_adj_min,qrs_m_r(id2,jd2)) !save minimum flux value
      qrs_tot_r = qrs_tot_r + qrs_r(id2,jd2) !Lottes 5/5/05: (possibly scaled) total surface heat in
   enddo;enddo

if (iadjf==1) then
   !Print melt surface heat flux in it....m_adjflux_r.dat file for review
   fmr='(/2x,''  x  /  y'',11(g13.5))'
   filename=casedir//'\it'//runum//'m_adjflux_r.dat'
   open(nu_adjf,file=filename)
   write(nu_adjf,*) qc_tot_r,"  input qc_tot_r"
   write(nu_adjf,*) flux_in_max,"  maximum flux input value" 
   write(nu_adjf,*) flux_in_min,"  minimum flux input value"
   write(nu_adjf,*) "       "
      write(nu_adjf,*) qrs_tot_r,"  adjusted total qrs_r"
   write(nu_adjf,*) flux_adj_max,"  maximum flux adjusted value" 
   write(nu_adjf,*) flux_adj_min,"  minimum flux adjusted value"
      write(nu_adjf,'(1x/t10,"adjusted surface radiation heat flux (W/m**2)")')

   j1=2
   do
      j2=j1+20
      j2=min(j2,np)
      write(nu_adjf,fmr) (y(j),j=j1,j2,2)
      do i=2,mp,2
         write(nu_adjf,'(f11.3,11e13.5)') x(i),(qrs_m_r(i/2,j/2),j=j1,j2,2)
      enddo
      j1=j2+2
      if (j1>np) exit
   enddo
   close(nu_adjf)
   !Done writing it....m_adjflux_r.dat file
endif 
endif

!--------------------------------------------------
! This code relaxes a flux distribution coming from the combustion space.
! It should probably not be done on a restart that simply resumes a melt space run.@@@
! Relaxation is used to stablize the melt surface b.c.'s during cycling runs.
! The user might not want to do this.

!if (itn>0.and.cycling==1) then
if (itn>0) then
   allocate(qrs_bak(mz,nz))

   irelax_nogood=1
   rfadj=0.8d0
   rfqrs=0.7d0
   rfqrs_c=1-rfqrs
   qrs_bak=qrs_m_r ! save surface heat flux distribution
   !----------------------------
   ! Surface flux relaxation loop
   do while(irelax_nogood==1)
      flux_adj_max=small
      flux_adj_min=big
      qrelxtot=0
      do j=4,np-2,2
      do i=4,i_me,2
         if (ibcell(i,j,lp).ne.4) cycle
         qrs_m_r(i/2,j/2) = rfqrs*qrs_bak(i/2,j/2) + rfqrs_c*qrs_prev_r(i/2,j/2)
         qrelxtot=qrelxtot + qrs_m_r(i/2,j/2)*dx(i)*dy(j)
         flux_adj_max=max(flux_adj_max,qrs_m_r(i/2,j/2)) !save maximum flux value
         flux_adj_min=min(flux_adj_min,qrs_m_r(i/2,j/2)) !save minimum flux value
      enddo;enddo
      qrelx_ave=qrelxtot/area_surf_tot

      !Find max deviation from the mean relaxed heat flux
      qdevmax=0
      do j=4,np-2,2
      do i=4,i_me,2
         if (ibcell(i,j,lp).ne.4) cycle
         qdevmax=max(abs(qrs_m_r(i/2,j/2)-qrelx_ave),qdevmax)
      enddo;enddo

      if (iadjr==1) then
         !Put relaxed melt surface heat flux into it....m_relax_r.dat file for review     
         fmr='(/2x,''  x  /  y'',11(g13.5))'
         filename=casedir//'\it'//runum//'m_relax_r.dat'
         open(nu_adjr,file=filename)
         write(nu_adjr,*) qc_tot_r,"  input qc_tot"
         write(nu_adjr,*) flux_in_max,"  maximum flux input value" 
         write(nu_adjr,*) flux_in_min,"  minimum flux input value"
         write(nu_adjr,*) "       "
         write(nu_adjr,*) qrelxtot,"  relaxed total qrs"
         write(nu_adjr,*) flux_adj_max,"  maximum flux relaxed value" 
         write(nu_adjr,*) flux_adj_min,"  minimum flux relaxed value"
         write(nu_adjr,'(1x/t10,"relaxed surface radiation heat flux (W/m**2)")')
      
         j1=2
         do
            j2=j1+20
            j2=min(j2,np)
            write(nu_adjr,fmr) (y(j),j=j1,j2,2)
            do i=2,mp,2
               write(nu_adjr,'(f11.3,11e13.5)') x(i),(qrs_m_r(i/2,j/2),j=j1,j2,2)
            enddo
            j1=j2+2
            if (j1>np) exit
         enddo
         close(nu_adjr)
         !Done writing it....m_relax_r.dat file
      endif

      if (irelax_nogood==0.or. (qdevmax/qrelx_ave < 2.0d0) .or. rfqrs < 0.2d0 ) exit      

      rfqrs=rfqrs*rfadj
      rfqrs_c=1-rfqrs
   enddo
   deallocate(qrs_bak)

endif !surface heat flux relaxation 


!Convert heat flux qrs_m_r into heat to surface patches (W), qrs_r array
!Also average the energy from the two combustion spaces
qrs_tot=0
qrs_tot_r=0
qs_in=0
qs_in_r=0
qs_out_1=qs_out
qs_out=0
qs_out_r=0
do i=4,i_me,2; id2=i/2 ! Lottes 4/28/05: i > i_me is refiner area
   flxh(i+1,5)=flxh(i-1,5)
do j=4,np-2,2; jd2=j/2
   if (ibcell(i,j,lp).ne.4) cycle
   qrs_r(id2,jd2)=qrs_m_r(id2,jd2)*(dx(i)*dy(j))

   qrs(id2,jd2) = (qrs(id2,jd2) + qrs_r(id2,jd2))/2 !average the energy from both combustion spaces

   flxh(i+1,5)=flxh(i+1,5)+qrs(id2,jd2)
   qrs_tot = qrs_tot + qrs(id2,jd2) !Lottes 5/5/05: (possibly scaled & relaxed) total surface heat in
   qrs_tot_r = qrs_tot_r + qrs_r(id2,jd2)
	qs_in = qs_in + max(qrs(id2,jd2),zero) !Lottes 11/24/07 (possibly scaled & relaxed surface heat in
	qs_out = qs_out + max(-qrs(id2,jd2),zero) !Lottes 11/24/07 (possibly scaled & relaxed surface heat out
	qs_in_r = qs_in_r + max(qrs_r(id2,jd2),zero) !Lottes 11/24/07 (possibly scaled & relaxed surface heat in
	qs_out_r = qs_out_r + max(-qrs_r(id2,jd2),zero) !Lottes 11/24/07 (possibly scaled & relaxed surface heat out
enddo;enddo


! qrs_prev_r is calculated from a possibly rescaled qrs
! immediately before saving in the restart file (see module Sav3f).
qrs_tot_base_comb1 = qrs_tot_base  !save qrs total base from combustion space 1

!facq_r=(h_needed-eb_heat+q_wall_loss_tot)/qc_tot_r !set for print, even if not scaled   ! @@@

!Save averaged surface distribution and total for possible rescaling when wall loss recalculated
qc_surf0=qrs 
qrs_tot_base = qrs_tot
qs_in_base_1=qs_in_base
qs_in_base = qs_in    ! save initial scaled energy in through surface
qrslg=qrs 

return
end


!======================================================================
!======================================================================
!======================================================================
!
! conv_surf_temp_regen interpolates the melt surface temperature distribution
! from the melt surface grid to the combustion space glass surface grid
! for the alternate burner configuration in a regenerative furnace.
!
! Lottes; come here if routine called from sprint
! (end of melt run)
!======================================================================
subroutine conv_surf_temp_regen

use gbl_var
implicit double precision (a-h,o-z)

allocate(tgs_c_r(mp_c_r/2,np_c_r/2))
tgs_c_r=zero
!Interpolate temperature from melt grid to combustion grid

!Surface temperature in the melt has already been computed when
!this routine is called. This temperature is 
!a combination of particle and liquid temperature. Where particles
!cover the surface the surface temperature should be the particle
!temperature. It is in array Tgs_m.

! Bilinear interpolation between grids on the melt surface:
! Note cell centers have even indexes, i.
! A linear interpolation/extrapolation to point n is: f(n) = [f(i)*(1-b)] + [f(i+2)*b]
! where b = [x(n)-x(i)] / [x(i+2)-x(i)]
! in the interior x(i)<= x(n) < x(i+1) 
! near boundaries x(n) can lie outside the interval [x(i),x(i+1)]
area_tot_r=zero
T_surf_mean_r=zero !initialize to calculate mean surface temp.
jm=j_mb !initial start index for south melt cell bound
do jc=j_mb_c_r,j_me_c_r,2 !loop over cell centered melt y grid points in interface
   !Find south cell row index bound in combustion grid for melt point at jm
   do while (ys_m(jm+2) < ys_c_r(jc) .and. jm < j_me-2)
      jm=jm+2
      !if (jm == np) exit
   enddo

   b_sn=(ys_c_r(jc)-ys_m(jm))/(ys_m(jm+2)-ys_m(jm))
   im=i_mb   
   do ic=i_mb_c_r,i_me_c_r,2 !loop over cell centered melt x grid points in interface
      !Find west cell row index bound in combustion grid for melt point at jm
      do while (xs_m(im+2) < xs_c_r(ic) .and. im < i_me-2)
         im=im+2
         !if (im == i_me) exit
      enddo

      !Here xs_c(ic-1) < xs_m(im) < xs_c(ic+1)
      if (ibcell_c_r(ic,jc,2).ne.4) cycle
      b_we=(xs_c_r(ic)-xs_m(im))/(xs_m(im+2)-xs_m(im))

      !T_sw=Tgs_m(im/2,  jm/2)
      !T_nw=Tgs_m(im/2,  jm/2+1)
      !T_se=Tgs_m(im/2+1,jm/2)
      !T_ne=Tgs_m(im/2+1,jm/2+1)   

      T_w=Tgs_m(im/2,  jm/2+1)*b_sn+Tgs_m(im/2,  jm/2)*(1-b_sn)
      T_e=Tgs_m(im/2+1,jm/2+1)*b_sn+Tgs_m(im/2+1,jm/2)*(1-b_sn)
      Tgs_c_r(ic/2,jc/2)=(T_e*b_we + T_w*(1-b_we))

      area_tot_r = area_tot_r + dx_c_r(ic)*dy_c_r(jc)
      T_surf_mean_r = T_surf_mean_r + Tgs_c_r(ic/2,jc/2)*dx_c_r(ic)*dy_c_r(jc)
    enddo
enddo

T_surf_mean_r = T_surf_mean_r/area_tot_r


!------------------------------------------------------------------------
!Lottes: This section is used during automatic cycling.
!        It reads in the last surface temperature file,
!        compares the last average surface temperature with the current
!        average temperature,  
!        and saves the temperature change in the T_change file.

filename=casedir//'\it'//runum_r//'t.dat'
inquire (file=filename,exist=exst)
if (cycling==1 .and. exst) then
   !At this point, itn should be >0 and tgs_c_r contains the current
   !melt surface temperatures, area_tot_r is the total surface area
   allocate (last_tgs_c_r(mp_c_r/2,np_c_r/2))
   last_tgs_c_r=zero
   open(nu_surfTr,file=filename)
   read (nu_surfTr,*) indicates_have_array
   if (indicates_have_array > 0) then
      !Have a file with temperature array, 
      !otherwise only have default file initially so do not process.
      !Skip over items not needed    
      read (nu_surfTr,*) dontneed
      read (nu_surfTr,*) dontneed
      read (nu_surfTr,*) dontneed
      read (nu_surfTr,'(A)') title
      read (nu_surfTr,'(A)') title
   
      !Read in last surface temperature Tgs array in combustion space grid.
      jb=1
      do
         je=jb+10
         je=min(je,np_c_r/2)
         read (nu_surfTr,'(A)') title
         read (nu_surfTr,'(A)') title
         do i=1,mp_c_r/2
            read (nu_surfTr,'(F11.3,11f11.1)') dontneed,(last_tgs_c_r(i,j),j=jb,je)
         enddo
         jb=je+1
         if (jb > np_c_r/2) exit
      enddo
      close(nu_surfTr)

      !Calculate average temperature differences
      sum_dif=zero
      sum_rel_dif=zero

      do i=1,mp_c_r/2
      do j=1,np_c_r/2
         area = dx_c_r(i) * dy_c_r(j)
         sum_dif = sum_dif + abs(tgs_c_r(i,j)-last_tgs_c_r(i,j))*area
         if (last_tgs_c_r(i,j)*area .ne. zero) then
            sum_rel_dif = sum_rel_dif + abs((tgs_c_r(i,j)-last_tgs_c_r(i,j))/last_tgs_c_r(i,j))*area
         endif
      enddo;enddo

      avg_temp_change = sum_dif/area_tot_r
      avg_rel_change = sum_rel_dif/area_tot_r

      !save changes in temperature change file
      if (iTchg==1) write(nu_Tchgr,*) nlg,avg_temp_change,avg_rel_change !save average changes in T change file
   else
      !did not have array
      close(nu_surfTr)
   endif
   deallocate (last_tgs_c_r)
endif


!call sprint_c
!-----------------------------------------------------
!Lottes: This section writes out surface temperature
!        interpolated to the combustion grid
!        and some global heat rate parameters

filename=casedir//'\it'//runum_r//'t.dat'
itn_r=1
open(nu_surfTr,file=filename)
write(nu_surfTr,*) itn_r,"  Distribution indicator"
write(nu_surfTr,*) h_needed-eb_heat+q_wall_loss_tot," Energy required by melter (W)"
write(nu_surfTr,*) qc_tot_r," Combustion Heat Transfer to Melter (W)"
write(nu_surfTr,*) T_surf_mean_r," Mean Surface Temperature (K)"

write(nu_surfTr,'(1X/T10,"Surface Temperature (K)")')

jb=2
do while (jb <= np_c_r)
   je=jb+20
   je=min(je,np_c_r)
   write(nu_surfTr,' ')
   write(nu_surfTr,'(" X / Y",T12,11F11.3)') (y_c_r(j),j=jb,je,2)
   do i=2,mp_c_r,2
      write(nu_surfTr,'(F11.3,11F11.1)') x_c_r(i),(tgs_c_r(i/2,jc),jc=jb/2,je/2)
   enddo
   jb=je+2
enddo
close(nu_surfTr)

return
end
