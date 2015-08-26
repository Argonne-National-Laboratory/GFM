!======================================================================
!======================================================================
!======================================================================
! Rad_emis.f90
!======================================================================
!     rad_emis_rr    Reads in radiation restart file
!     rad_emis       Controls radiation calculation
!                    Writes out radiation restart file
!     rad_qe_only    Calculate radiation emission only from interior volume cells
!                     
!  8/2/05      Split rad_emis into 2 separate routines
!======================================================================
!======================================================================
!======================================================================
!     rad_emis_rr    Reads in radiation restart file
!======================================================================
subroutine rad_emis_rr
use gbl_var
implicit double precision (a-h,o-z)

!Make sure there is a radiation restart file
filename=casedir//'\rr'//runum//'c.d'
inquire(file=filename,exist=exst)
if (.not.exst) then
   write (6,*) 'Radiation restart data file missing, do a new start for case.'
   call stop_run("Radiation restart data file missing, do a new start for case.")
end if  

open(nu_restr,file=filename,form="unformatted",err=900)
read (nu_restr,err=900) qe !array
!read (nu_restr,err=900) qa1
read (nu_restr,err=900) qa !array
read (nu_restr,err=900) ql !array
read (nu_restr,err=900) wall_rad_T !array
read (nu_restr,err=900) qeg0,qeg_g,qeg_w,qeg_s,qeg_io
read (nu_restr,err=900) qew0,qew_g,qew_w,qew_s,qew_io
read (nu_restr,err=900) qes0,svf_r,qa_s0,qa_s1,qls_g,qls_a
read (nu_restr,err=900) q_melt,q_wall_loss
read (nu_restr,err=900) q_exhaust,q_rad_inlet,q_rad_outlet
!read (nu_restr,err=900) qa_melt,qe_melt

read (nu_restr,err=900) b_cnt       !number of boundary patches
read (nu_restr,err=900) preset_vf   !1=>view factors are all calculated once
read (nu_restr,err=900) iterw       !wall radiation iteration counter
read (nu_restr,err=900) iwall_count !number of interior wall patches
read (nu_restr,err=900) imelt_count !number of melt surface patches
read (nu_restr,err=900) inlet_count !number of inlet patches
read (nu_restr,err=900) iexit_count !number of exit patches
read (nu_restr,err=900) wall_area_total !total area of wall  patches
read (nu_restr,err=900) melt_area_total !total area of melt patches
read (nu_restr,err=900) inlet_area_total !total area of inlet patches
read (nu_restr,err=900) exit_area_total !total area of exit patches
read (nu_restr,err=900) boundary_area_total !total area of boundary patches
read (nu_restr,err=900) q_incident_total !total energy incident on boundaries from the volume
read (nu_restr,err=900) q_incident_wall  !energy incident on walls from the volume
read (nu_restr,err=900) q_incident_inlet !energy incident on inlets from the volume
read (nu_restr,err=900) q_incident_exit  !energy incident on exits from the volume
read (nu_restr,err=900) q_incident_melt  !energy incident on melt surface from the volume
read (nu_restr,err=900) qconv_total !total convection from gas to boundaries 
read (nu_restr,err=900) aoxid,aform !kinetic constants for soot oxidation & formation
read (nu_restr,err=900) avg_wall_T  !average combustion space wall temperature

call allocate_boundary_arrays !allocate boundary patch and dependent radiation arrays

read (nu_restr,err=900) b_typ       ! boundary type = ibcell value of patches
read (nu_restr,err=900) b_i         ! i index of boundary patches
read (nu_restr,err=900) b_j         ! j index of boundary patches
read (nu_restr,err=900) b_k         ! k index of boundary patches
read (nu_restr,err=900) b_orient    ! boundary orientation (x,y,z)
read (nu_restr,err=900) b_direct    ! boundary direction (+,-)
read (nu_restr,err=900) patch_area  ! area of patches
read (nu_restr,err=900) b_face_area ! area of all patches on same cell
if (preset_vf==1) then
   read (nu_restr,err=900) vf          ! view factors
   read (nu_restr,err=900) vf_sum      ! for patch i, sum over j of vf(i,j)
endif
read (nu_restr,err=900) radiosity   ! radiosity of boundary patch 
read (nu_restr,err=900) emis        ! emissivity of boundary patch 
read (nu_restr,err=900) q_amb       ! heat flux out through walls
read (nu_restr,err=900) q_conv      ! convection heat flux from gas flow to boundaries
read (nu_restr,err=900) qc          ! heat flux from volume radiation calc
read (nu_restr,err=900) qeb         ! heat flux from known temperature boundary
read (nu_restr,err=900) b_T         ! temperature of patch
read (nu_restr,err=900) bc_i        ! i index of cell center for a patch 
read (nu_restr,err=900) bc_j        ! j index of cell center for a patch
read (nu_restr,err=900) bc_k        ! k index of cell center for a patch
read (nu_restr,err=900) b_exh	    ! exhaust number
close(nu_restr)

!need to replace wall temperatures that were updated but not saved in T 
do k=lp,2,-2
do i=2,mp,2
do j=2,np,2
   !oops! ibcell will not have special markings at this point so cannot check for them
   !if (ibcell(i,j,k).lt.100) cycle !not at a wall with open cell neighbor
   if (wall_rad_T(i,j,k) > zero) then
      T(i,j,k)=wall_rad_T(i,j,k)/t0 !convert back to normal combustion representation
      call T_to_enth(i,j,k) !update enthalpy
   endif
enddo;enddo;enddo
return

900   continue !error reading file
!  10-11-05  decided this is a fatal error to report back to user 
call stop_run("Severe error:  unable to read radiation restart file.")
!qe=zero
!close(7)
return
end


!======================================================================
!======================================================================
!     rad_emis       Controls radiation calculation
!                    Writes out radiation restart file
!======================================================================
!----------------------------------------------------------------------
!General flow of routine:
!     Mark walls with open cell neighbors
!     Adjust T
!     CALL RAD0
!     CALL QRNF
!     If rad_id=10 then
!       save qa in qa1 (not done anymore)
!       sum net melt surface radiation
!     Save radiation restart file
!     Display info on screen
!     Deallocate several arrays
!----------------------------------------------------------------------
SUBROUTINE RAD_EMIS
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

write(ncon,'(/"Begin Radiation Calculations"/)')

QES0=0
ID_RAD=10 !indicator checked within rad0 routine
itr_rad=itr_rad+1

! Note 3-20-06 
! These lines of code are no longer needed. R0 must be one and 
! MP3 was only used in subroutines that are no longer called
! Adjust grid sizes
!X=X*R0
!DX=DX*R0
!R=R*R0
!DR=DR*R0
!Z=Z*R0
!DZ=DZ*R0
!MP3(1)=MP
!MP3(2)=NP
!MP3(3)=LP

!Specially mark walls with open cell neighbors
DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IF (IBCELL(I,J,K).NE.1) CYCLE
   KM2=MAX(K-2,2)
   KP2=MIN(K+2,LP)
   JM2=MAX(J-2,2)
   JP2=MIN(J+2,NP)
   IM2=MAX(I-2,2)
   IP2=MIN(I+2,MP)

   ! 7/31/05
   !Wall markers in ibcell for walls that have open cell neighbors
   !Hundreds digit: 1 = x-normal, 2 = y-normal, 3 = z-normal
   !Tens     digit: 0 = positive direction, 1 = negative direction
   !Ones     digit: 1 = wall, as usual
   !These values are set for even,even,even indexes only

   IF (IBCELL(I,J,KM2).LE.0) THEN !Note that an additional x-normal or y-normal wall face is not recognized 
      IBCELL(I,J,K)=301 
   ELSEIF (IBCELL(I,J,KP2).LE.0) THEN
      IBCELL(I,J,K)=311
   ELSEIF (IBCELL(I,JM2,K).LE.0) THEN
      IBCELL(I,J,K)=201
   ELSEIF (IBCELL(I,JP2,K).LE.0) THEN
      IBCELL(I,J,K)=211
   ELSEIF (IBCELL(IM2,J,K).LE.0) THEN
      IBCELL(I,J,K)=101
   ELSEIF (IBCELL(IP2,J,K).LE.0) THEN
      IBCELL(I,J,K)=111
   ENDIF
enddo;enddo;enddo

!Set T to actual value (remove normalization)
T=T*T0

if (i1st_rad_done==0) then
   !First time in the radiation code.
   !Initialize the temperature in the boundary arrays. (This part of the boundary array
   !init is being done after the gas phase has computed temperatures.)

   ! Set boundary patch temperature and make initial guess for radiosity
   do n=1,b_cnt
      if (b_typ(n)==1) then
         !set patch T to T of boundary cell center 
         b_T(n)=T(bc_i(n),bc_j(n),bc_k(n))
      elseif (b_typ(n)==2) then
         b_T(n)=T(bc_i(n),bc_j(n),bc_k(n))
      elseif (b_typ(n)==3) then 
	     if (exh_type(b_exh(n))==0) then
			b_T(n)=exh_frac(b_exh(n))*avg_wall_T
		 else
		    b_T(n)=exh_fixed(b_exh(n))
		 endif
      else !Have a melt surface patch (real boundary condition)
         b_T(n)=T(b_i(n),b_j(n),b_k(n))
      endif     
      radiosity(n)=sig*b_T(n)**4 
   enddo

   calc_q_cnt = 0

   ibTsta=0
   if (ibTsta>0) then
      ! debug print out boundary face temperatures, indexes
      filename=casedir//'\bTstart'//runum//'c.txt'
      open(nu_bTsta,file=filename)
      write(nu_bTsta,*) 'Boundary Face Temperature, indexes'
      write(nu_bTsta,*) '================================='
      write(nu_bTsta,*) '  Boundary: n,    b_type,    b_T,        i,        j,        k        b_exh'
      write(nu_bTsta,*) '================================='
      do n=1,b_cnt !n is the 'near or from' patch
	     if (b_typ(n)==3) then
			write(nu_bTsta,'(i5,i5,g15.7,3i5)') n,b_typ(n),b_T(n),b_i(n),b_j(n),b_k(n),b_exh(n)
		 endif
      enddo

	  write(nu_bTsta,*) '  '
      write(nu_bTsta,*) '================================='
      write(nu_bTsta,*) '  Exhaust: n         i1        j1        k1       i2      j2    k2'
      write(nu_bTsta,*) '================================='
 	  do n=1,nex
			write(nu_bTsta,*) n,exh_i1(n),exh_j1(n),exh_k1(n),exh_i2(n),exh_j2(n),exh_k2(n)
	  enddo

	  write(nu_bTsta,*) '  '
      write(nu_bTsta,*) '================================='
      write(nu_bTsta,*) '  Exhaust: n         orient        direct     type      frac      fixed'
      write(nu_bTsta,*) '================================='
 	  do n=1,nex
			write(nu_bTsta,*) n,exh_orient(n),exh_direct(n),exh_type(n),exh_frac(n),exh_fixed(n)
	  enddo

	  write(nu_bTsta,*) '  '
	  write(nu_bTsta,*)avg_wall_T
      close(nu_bTsta)
	  ibTsta=0
   endif

else ! first radiation calculation is done, if we are restarting and cycling, we need the new melt surface T
     ! Could this be moved to the read restart routine? 
        
   do n=1,b_cnt
      if (b_typ(n)==4) then
         b_T(n)=T(b_i(n),b_j(n),b_k(n))
         radiosity(n)=sig*b_T(n)**4 
      endif     
   enddo

endif

call rad0   !set akl for all cells   !only need for open cells, but no harm in setting rest
            !rad0 also calls rad_gas and debx for open cells
            !          for full rad, calculates total emission from soot and from h2o & co2
            !          for qe_only rad, calculates volume emission in qe2   
CALL QRNF

!IF (ID_RAD.EQ.10) THEN
!   QA1=QA
!ENDIF

!   !sum up net radiation to melt surface
!   q_melt=zero
!   qa_melt=zero
!   qe_melt=zero
!   do i=2,mp,2
!   do j=2,np,2
!      if (ibcell(i,j,2).ne.4) cycle
!      q_melt = q_melt + qa(i/2,j/2,1)-qe(i/2,j/2,1)
!      qa_melt=qa_melt + qa(i/2,j/2,1)            
!      qe_melt=qe_melt + qe(i/2,j/2,1)
!   enddo;enddo
!endif

! Save radiation restart file
filename=casedir//'\rr'//runum//'c.d'
OPEN(nu_restr,FILE=FILENAME,FORM="UNFORMATTED",ERR=901)

WRITE (nu_restr) QE
!WRITE (nu_restr) QA1
WRITE (nu_restr) QA !replaces statement above
WRITE (nu_restr) QL
write (nu_restr) wall_rad_T
WRITE (nu_restr) QEG0,QEG_G,QEG_W,QEG_S,QEG_IO,QES0
WRITE (nu_restr) QEW0,QEW_G,QEW_W,QEW_S,QEW_IO
WRITE (nu_restr) QES0,SVF_R,QA_S0,QA_S1,QLS_G,QLS_A
write (nu_restr) q_melt,q_wall_loss
write (nu_restr) q_exhaust,q_rad_inlet,q_rad_outlet
!write (nu_restr) qa_melt,qe_melt

write (nu_restr) b_cnt       !number of boundary patches
write (nu_restr) preset_vf   !1=>view factors are all calculated once
write (nu_restr) iterw       !wall radiation iteration counter
write (nu_restr) iwall_count !number of interior wall patches
write (nu_restr) imelt_count !number of melt surface patches
write (nu_restr) inlet_count !number of inlet patches
write (nu_restr) iexit_count !number of exit patches
write (nu_restr) wall_area_total !total area of wall  patches
write (nu_restr) melt_area_total !total area of melt patches
write (nu_restr) inlet_area_total !total area of inlet patches
write (nu_restr) exit_area_total !total area of exit patches
write (nu_restr) boundary_area_total !total area of boundary patches
write (nu_restr) q_incident_total !total energy incident on boundaries from the volume
write (nu_restr) q_incident_wall  !energy incident on walls from the volume
write (nu_restr) q_incident_inlet !energy incident on inlets from the volume
write (nu_restr) q_incident_exit  !energy incident on exits from the volume
write (nu_restr) q_incident_melt  !energy incident on melt surface from the volume
write (nu_restr) qconv_total !total convection from gas to boundaries 
write (nu_restr) aoxid,aform !kinetic constants soot oxidation & formation
write (nu_restr) avg_wall_T  !average combustion space wall temperature

write (nu_restr) b_typ       ! boundary type = ibcell value of patches
write (nu_restr) b_i         ! i index of boundary patches
write (nu_restr) b_j         ! j index of boundary patches
write (nu_restr) b_k         ! k index of boundary patches
write (nu_restr) b_orient    ! boundary orientation (x,y,z)
write (nu_restr) b_direct    ! boundary direction (+,-)
write (nu_restr) patch_area  ! area of patches
write (nu_restr) b_face_area ! area of all patches on same cell
if (preset_vf==1) then
   write (nu_restr) vf          ! view factors
   write (nu_restr) vf_sum      ! for patch i, sum over j of vf(i,j)
endif
write (nu_restr) radiosity   ! radiosity of boundary patch 
write (nu_restr) emis        ! emissivity of boundary patch 
write (nu_restr) q_amb       ! heat flux out through walls
write (nu_restr) q_conv      ! convection heat flux from gas flow to boundaries
write (nu_restr) qc          ! heat flux from volume radiation calc
write (nu_restr) qeb         ! heat flux from known temperature boundary
write (nu_restr) b_T         ! temperature of patch
write (nu_restr) bc_i        ! i index of cell center for a patch 
write (nu_restr) bc_j        ! j index of cell center for a patch
write (nu_restr) bc_k        ! k index of cell center for a patch
write (nu_restr) b_exh		 ! exhaust number

CLOSE(nu_restr)

!CALL PRN_SCR(4)  !print wall radiation display to screen for command window

DEALLOCATE (AKL,DEB,DEB0)
!DEALLOCATE (WL)
!DEALLOCATE (QE,QEL,QEL0,QEL1,QA,QAL,QA1,QL)
!DEALLOCATE (QE,QEL,QEL0,QEL1,QA,QAL,QL)
DEALLOCATE (QEL,QEL0,QEL1,QAL)
deallocate (b_typ,b_i,b_j,b_k,b_orient,b_direct,patch_area,b_face_area)
if (preset_vf==1) deallocate (vf,vf_sum)
deallocate (radiosity,emis,q_amb,q_conv,qc,qeb,b_T,bc_i,bc_j,bc_k,b_exh)


i1st_rad_done=1

!------------------------------------------------
! readjust modified variables
  
!Unmark walls 
DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IF (IBCELL(I,J,K)>100) ibcell(i,j,k)=1
enddo;enddo;enddo

!set T to normalized value
T=T/T0

! end of readjust modified variables
!------------------------------------------------

!   itrap=0
!   if(itrap==1)then
!            !T=T*T0
!            call rad_qe_only !Only need emission radiation from open cells at this time
!   endif

RETURN

901   continue !error opening file
!  10-11-05  decided this is a fatal error to report back to user 
call stop_run("Severe error:  unable to create radiation restart file.")
!qe=zero
!close(7)
return
end


!======================================================================
!======================================================================
!======================================================================
!     rad_qe_only    Calculate radiation emission from interior volume
!                    cells (without doing complete absorption calculation
!                    and without doing wall radiation calculation)
!
!                    This routine is used to stablize the interior
!                    temperature between full radiation calculations.
!
!                    Called from sphase routine.
!======================================================================
subroutine rad_qe_only
use gbl_var
implicit double precision (a-h,o-z)

allocate (qe2(mz,nz,lz))
qe2=zero
qeg0=0
id_rad=1 !indicator checked within rad0 routine

write(ncon,'(/"Begin Interior Volume Emission Radiation Calculations"/)')
!Do not need to specially mark walls with open cell neighbors
!Do not need to use boundary arrays

!Set T to actual value (remove normalization)
T=T*T0 

call rad0   !set akl for all cells   !only need for open cells, but no harm in setting rest
            !rad0 also calls rad_gas and debx for open cells
            !          for full rad, calculates total emission from soot and from h2o & co2
            !          for qe_only rad, calculates volume emission in qe2   


!Duplicate parts of qrnf routine here to determine qe in volume
!allocate (qe2(mz,nz,lz))
!qe2=zero
!qeg0=0

!this loop computes volume radiation emission in qe2
!do i=2,mp,2; id2=i/2
!do j=2,np,2; jd2=j/2
!do k=2,lp,2
!   ibc0=ibcell(i,j,k) 
!   if (ibc0.ge.1 .or. T(i,j,k).le.tg_mn) cycle
!   !proceed only with open cells (that are warmer than the minimum temperature)
!   kd2=k/2
!
!   call debx(T(i,j,k))
!
!   !the qe portion of the qref routine is duplicated here.
!   area_cell=2*(dr(j)*dz(k)+dr(j)*dx(i)+dz(k)*dx(i))
!   sum=zero
!   do l=1,nwl 
!      sum=sum+deb(l)*akl(id2,jd2,kd2,l)
!   enddo
!   qe2(id2,jd2,kd2) = sgn*T(i,j,k)**4 * area_cell * sum
!   qeg0=qeg0+qe2(id2,jd2,kd2)
!enddo; enddo; enddo

i1st_rad_qe_done=1
T=T/T0 !restore T  
qe=qe2

deallocate (akl,e0,deb,deb0,aklg,akls,akl0) ! allocated in rad0
deallocate (qe2)
return
end
