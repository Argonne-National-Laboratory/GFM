!==============================================================================
!==============================================================================
!==============================================================================
!
! Initsv.f90
!
! Initsv initializes the computational domain
!
! This module contains the following routines:
!     initsv
!     gen_initial_guess
!     initsvm
!     melt_surf_T
!     read_restart_data
!
!======================================================================
!======================================================================
!======================================================================
! INITSV sets variable distributions over the computational domain
! for the gas phase variables (excluding minor species)
!
! For a fresh start it sets an initial guessed distribution of variables
! For restarts or global iterations in reads the distributions from a 
! restart file.
!
! The temperature distribution at the melt surface is set via 
! routine melt_surf_T
! 
!======================================================================
subroutine initsv
use gbl_var
implicit double precision (a-h,o-z)

if (irstyp == 0) then
   loop_relax=0 !set=1 if want to loop thru various relaxation settings
   do
      !adjust relaxation factor if desired
      !rfTsurf=0.6d0  
      !rfTsurf_c=one-rfTsurf
      call melt_surf_T !Set the temperature boundary condition on the melt surface
                    !Must be set before preparing for radiation calc
      if (loop_relax==0) exit
   enddo
   call gen_initial_guess
   if (id_rad>0)  call prepare_for_wall_radiation_calculation 

else 
   call read_restart_data
   if (in_run == 0) then
      !On restarts, melt_surf_T must be called after reading in previous melt surface array 
      !for relaxing the temperature in cycle runs.
      loop_relax=0 !set=1 if want to loop thru various relaxation settings
      do
         !adjust relaxation factor if desired
         !rfTsurf=0.6d0  
         !rfTsurf_c=one-rfTsurf
         call melt_surf_T !Set the temperature boundary condition on the melt surface
         if (loop_relax==0) exit
      enddo
   endif
endif

call intpv !interpolation of velocity
call extr(1) !extrapolate gas variables

!q_ls=q_in*radloss !radloss=.65874, hard coded in gui, passed in sbc file     
!cbg q_ls=8.907685d+6
!qls_s=q_ls*bfract !bfract=.75, hard coded in gui, passed in sbc file

!g1=qls_s/area
!if (i_first.eq.0) then
!   !doing a new start
!   do i=2,mp,2
!   do j=2,np,2
!   do k=2,lp,2
!      if (ibcell(i,j,k).eq.4) then
!         !init melt surface qrs to q_in*radloss*bfract/area
!         qrs(i,j)=g1
!      endif
!   enddo;enddo;enddo
!i_first=i_first+1
!endif

call intp !interpolation of scalar variables

return
end

!======================================================================
!======================================================================
!======================================================================
!
! Generate initial guess for a fresh start (no restart file)
! and set fresh start parameters
!
!======================================================================
subroutine gen_initial_guess
use gbl_var
implicit double precision (a-h,o-z)

factor=one
itr_rad=0
itr_gend=initial_gitr
iterw=0 !wall radiation iteration counter

do i=2,mp,2
do j=2,np,2
do k=2,lp,2
   ibc=ibcell(i,j,k)
   if (ibc==2) cycle !inlets
   !Moved wall check after gf has been set because gf is needed in new_T_t0_enth routine
   !if (ibc==1) then  !walls
   !   T(i,j,k)=T_init/T0 !set initial wall temperature to volume temperature
   !   call new_T_to_enth(i,j,k) !sets initial guess for enthalpy from the temperature.
   !   cycle
   !endif

   !CALL SBC3(I_REF,J_REF,K_REF,I,J,K) ! 8/29/05 calling this clobbers some variables
   ! 8/18/05
   !set initial species concentration based on estimate
   !of well mixed combustion product mixture
   !This will be much closer to final composition except in flame zones
   !and will vary with amount of excess air, the change should be significantly
   !better for oxy-fueled furnaces
   gf(i,j,k,iyf)=0
   if (i_air_flow_only==1)then
      gf(i,j,k,iyco2)=0
      gf(i,j,k,iyo2)=0.23
      gf(i,j,k,iyn2)=0.77
      gf(i,j,k,iyh2o)=0
   else
      gf(i,j,k,iyo2)=yo2_out_est
      gf(i,j,k,iyco2)=yco2_out_est
      gf(i,j,k,iyh2o)=yh2o_out_est
      gf(i,j,k,iyn2)=yn2_out_est
   endif

   if (ibc==1) then  !walls
      T(i,j,k)=T_init/T0 !set initial wall temperature to volume temperature
      call T_to_enth(i,j,k) !sets initial guess for enthalpy from the temperature.
      cycle
   endif

   ug(i,j,k,1)=0
   ug(i,j,k,2)=0
   ug(i,j,k,3)=0
   theta(i,j,k)=one
   smf(i,j,k)=zero
   !bg T(I,J,K)=1
   !if(ibcell(i,j,k).ne.4) T(I,J,K)=3.0D+0
   !  3/31/2005: initial guess for temperature is 1500 K = 3 * T0, T0 = 500 K
   ! Is this ever changed at the glass surface? NO! Fixed June 2005.
   !if(ibcell(i,j,k).ne.4) T(I,J,K)=1800D+0/T0
   !if(ibcell(i,j,k).ne.4) T(I,J,K)=300D+0/T0
   !  7/5/2005: initial guess for temperature is 1800 K = 3.6 * T0, T0 = 500 K
   !  11/10/2005: initial guess for temperature is a user input via GUI
   if(ibcell(i,j,k).ne.4) T(I,J,K)=T_init/T0
   call T_to_enth(i,j,k) !sets initial guess for enthalpy from the temperature.
   call tke(i,j,k)
   call dnst_calc(i,j,k)
   call ther(i,j,k)
enddo;enddo;enddo

if (nphas == 1) return !for GFM return is here, gas is single phase
if (fr_q.le.zero.and.fr_p.le.zero) return

!--------------------------------------------------------
!code below sets initial guess for droplets and particles
!needs to be carefully checked before use 
DO I=2,MP,2;  ID2=I/2
DO J=2,NP,2;  JD2=J/2    
DO K=2,LP,2
   IBC=IBCELL(I,J,K)
   IF (IBC.EQ.2.OR.IBC.EQ.1) cycle
   KD2=K/2    
   IF (FR_Q.GT.ZERO) then
   ANR=ZERO
   DO L=1,NDP0
      DN(ID2,JD2,KD2,L)=ZERO
      DO IU=1,3
         DU(ID2,JD2,KD2,L,IU)=UG(I,J,K,IU)
      ENDDO
      DT(ID2,JD2,KD2,L)=MIN(T(I,J,K),TB(L))
      ANR=ANR+DN(ID2,JD2,KD2,L)*RD3(L)
   ENDDO
   TH_DP(ID2,JD2,KD2)=THET0C*ANR
   endif
   IF (FR_P.LE.ZERO) then
      THETA(I,J,K)=ONE-TH_DP(ID2,JD2,KD2)-TH_PT(ID2,JD2,KD2)
      cycle
   endif
   ANR=ZERO
   DO L1=1,NPT0
      L=L1+NDP0
      !csl DN(ID2,JD2,KD2,L)=DN(I_REF/2,J_REF/2,K_REF/2,L)
      DN(ID2,JD2,KD2,L)=ONE
      DO IU=1,3
         !CSL DU(ID2,JD2,KD2,L,IU)=DU(I_REF/2,J_REF/2,K_REF/2,L,IU)
         DU(ID2,JD2,KD2,L,IU)=UG(I,J,K,IU)
      ENDDO
      DT(ID2,JD2,KD2,L)=T(I,J,K)
      ANR=ANR+DN(ID2,JD2,KD2,L)*RD3(L)
   END DO
   TH_PT(ID2,JD2,KD2)=THET0C*ANR
   THETA(I,J,K)=ONE-TH_DP(ID2,JD2,KD2)-TH_PT(ID2,JD2,KD2)
enddo;enddo;enddo

return
end

!======================================================================
!======================================================================
!======================================================================
!
! read_restart_data - Read field data from restart file
!
!   gf: general property functions
!       fuel (iyf), enthalpy (ih),
!       turbulent kinetic energy (ik), turbulent dissipation (ieps),
!       water vapor (iyh2o), inert gas (iyn2), carbon dioxide
!       (iyco2), and oxygen (iyo2)
!    lstar: starting index for general calculation
!    lend:  end index general calculation
!======================================================================
subroutine read_restart_data
use gbl_var
implicit double precision (a-h,o-z)
dimension gff(9)

!Make sure there is a restart file
filename=casedir//'\rg'//runum//'c.d'
inquire(file=filename,exist=exst)
if (.not.exst) then
   write (6,*) 'Restart data file missing, do a new start for case.'
   call stop_run("Restart data file missing, do a new start for case.")
end if  

OPEN(nu_restg,FILE=filename,FORM='UNFORMATTED')

DO I=2,MP,2;  ID2=I/2
DO J=2,NP,2;  JD2=J/2
DO K=2,LP,2;  KD2=K/2
   READ (nu_restg) G1,G2,G3,G4,G5,G6
   READ (nu_restg) (GFF(L),L=1,LEND)
   P(I,J,K)=G1
   IF (IBCELL(I,J,K).EQ.2.AND.itr_gas.LE.0) THEN
      ! at inlet and doing a real restart, not just a loop forced restart
      DO L=1,LEND
         IF (L.EQ.IK.OR.L.EQ.IEPS) GF(I,J,K,L)=GFF(L)
      ENDDO
   ELSE
      IF (I.LT.MP.AND.IBCELL(I+1,J,K).NE.2.AND.IBCELL(I+1,J,K).NE.1) UG(I+1,J,K,1)=G2
      IF (J.LT.NP.AND.IBCELL(I,J+1,K).NE.2.AND.IBCELL(I,J+1,K).NE.1) UG(I,J+1,K,2)=G3
      IF (K.LT.LP.AND.IBCELL(I,J,K+1).NE.2.AND.IBCELL(I,J,K+1).NE.1) UG(I,J,K+1,3)=G4
      if(ibcell(i,j,k).ne.4.or.in_run==1) T(I,J,K)=G5 !Don't reset melt surface boundary,  7/1/2005
      THETA(I,J,K)=G6
      DO L=1,LEND
         GF(I,J,K,L)=GFF(L)
      ENDDO
      !CALL WTDN(I,J,K,WTOJ)
      call dnst_calc(i,j,k)
      CALL THER(I,J,K)
   ENDIF
enddo;enddo;enddo

if (nphas > 1)then !read data for other phases
   DO L=1,NDNP
   DO I=2,MP,2;  ID2=I/2
   DO J=2,NP,2;  JD2=J/2
   DO K=2,LP,2;  KD2=K/2
      IF (L.EQ.1) THEN
         READ (nu_restg) EVP(ID2,JD2,KD2),CON(ID2,JD2,KD2),SP(I,J,K)
         READ (nu_restg) TH_DP(ID2,JD2,KD2),TH_PT(ID2,JD2,KD2)
      ENDIF
      IF (L.LE.NDP0) THEN
         READ (nu_restg) G1,G2,G3,G4,G5
      ELSE
         READ (nu_restg) G1,G2,G3,G4,G5,G6
         L1=L-NDP0
         DC(ID2,JD2,KD2,L1)=G6
      ENDIF
      IF (IBCELL(I,J,K).EQ.2.AND.itr_gas.LE.0) cycle
      DT(ID2,JD2,KD2,L)=G1
      DN(ID2,JD2,KD2,L)=G2
      DU(ID2,JD2,KD2,L,1)=G3
      DU(ID2,JD2,KD2,L,2)=G4
      DU(ID2,JD2,KD2,L,3)=G5
   enddo;enddo;enddo;enddo
endif

read (nu_restg) bs_hprev !previous enthalpy source used for relaxation
read (nu_restg) T_surf_prev !previous surface T used for relaxation in cycling

READ (nu_restg) FACTOR,FADJ
read (nu_restg) i1st_rad_qe_done
read (nu_restg) itemp_i1st_rad_done
if (i1st_rad_done==1 .or. itemp_i1st_rad_done==1) i1st_rad_done=1
!itr_gas_tmp=0
if (itr_gas == 0) then
   !doing a real restart, not just loop forced restart
   read(nu_restg,err=380) itr_gas_tmp
   itr_gas=itr_gas_tmp
   read(nu_restg,err=380) itr_rad,itr_gend
   read(nu_restg,err=380) interval_rad_tmp  
   if (itr_gend.le.itr_gas .or. interval_rad_tmp .ne. interval_rad) itr_gend=itr_gas+interval_rad !start new gas iteration interval
endif

380   continue !fall thru or come here on read error
CLOSE(nu_restg)
maxgi=maxgi+itr_gas_tmp  !change gas loop limit to specified max + previous iteration #

if (id_rad>0) call rad_emis_rr !read radiation restart file
! note that radiation code is only called here when doing a restart

return
end

!======================================================================
!======================================================================
!======================================================================
!     Initsvm sets variable distributions over the computational domain
!     for the (minor) sub species
!
!     For a fresh start it sets an initial guessed distribution for GFM.
!     For restarts or global iterations it reads the distributions from  
!     the 'rs'//runum//'c.d' restart file.
!
!======================================================================
SUBROUTINE INITSVM
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

filename=casedir//'\rs'//runum//'c.d'
INQUIRE (FILE=FILENAME,EXIST=EXST)
IF (IRSTYPM.EQ.0 .OR. .NOT.EXST) THEN
   !new start
   smf=zero
   GFM=ZERO
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      !IF (IBCELL(I,J,K).EQ.2) THEN
         GFM(I,J,K,1)=GF(I,J,K,IYF)
         GFM(I,J,K,2)=GF(I,J,K,IYO2)
         GFM(I,J,K,4)=GF(I,J,K,IYH2O)
         GFM(I,J,K,5)=GF(I,J,K,IYCO2)
         GFM(I,J,K,6)=GF(I,J,K,IYN2)
      !ENDIF
   enddo;enddo;enddo
ELSE
   !restart
   OPEN(nu_rests,FILE=FILENAME,FORM='UNFORMATTED')
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      READ (nu_rests) (GFM(I,J,K,L),L=1,NSP0),smf(I,J,K)
      IF (IBCELL(I,J,K).EQ.2) THEN
         GFM(I,J,K,1)=GF(I,J,K,IYF)
         GFM(I,J,K,2)=GF(I,J,K,IYO2)
         GFM(I,J,K,4)=GF(I,J,K,IYH2O)
         GFM(I,J,K,5)=GF(I,J,K,IYCO2)
         GFM(I,J,K,6)=GF(I,J,K,IYN2)
         gfm(i,j,k,3)=zero !added 4-5-06
         gfm(i,j,k,7)=zero !added 4-5-06
      ENDIF
   enddo;enddo;enddo

   read (nu_rests) bs_soot_prev
   read (nu_rests) sfp_soot_prev

   READ (nu_rests) RRMS
   read (nu_rests) itr_ms
   CLOSE(nu_rests)
   call extrm 
   imsflow_done=1
ENDIF

IGS0=0 !set to indicate to Gslv3m routine that this routine has just been run
call intpm
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!  Melt_surf_T reads in the it....t.dat file which contains the melt
!  surface temperatures, relaxes the temperatures against the previous 
!  temperatures, and normalizes the temperature before putting
!  it into the cell centers of the z=2 slab of the T array.
!
!  This routine is only called when in_run is 0.
!======================================================================
subroutine melt_surf_T
use gbl_var
implicit double precision (a-h,o-z)

real(8), allocatable :: Tgs_bak(:,:)
adj_relaxT=0.8d0 !factor to reduce relaxation factor if Tmax too big
limit_adj_relaxT=250.0d0 !max value for (Tgs_max-surfT_relaxed)
!irelax_Tsurf=0 !Don't relax surface T unless we get a T distribution

filename=casedir//'\it'//runum//'t.dat'
inquire (file=filename,exist=exst)
if (.not.exst) then
   write (6,*) ' E06: Surface Temperature File: ',filename,' missing.'
   write (6,*) ' Melt computation terminated abnormally'
   write (6,*) ' or case is corrupt.'
   call stop_run("Missing IT....T.DAT file.") !
endif

!if we aren't really starting a run, just restore the surface B.C. 
!if (irstyp==1.and.in_run==1) then
!   do j=4,np-2,2
!   do i=4,mp-2,2
!      T(i,j,2)=T_surf_prev(i/2,j/2)/T0
!   enddo;enddo
!   return
!endif

allocate(Tgs(mz,nz))
Tgs=zero
allocate(Tgs_bak(mz,nz))
Tgs_bak=zero

open(nu_itt,file=filename)
read (nu_itt,*) itn !itn=1 means the temperature distribution from the melt space is present
read (nu_itt,*) q_melt_req  !Energy required for melt   (will overwrite value in sbc file)
read (nu_itt,*) qls_s_T !Heat in to melt surface. This is used for uniform heat flux case.
read (nu_itt,*) surf_T  !Area mean surface T.

Tgs=surf_T !initialize for uniform T case

if (irstyp==0) T_surf_prev=surf_T !set previous T to surf mean if no previous
if (itn > 0) then
   !irelax_Tsurf=1
   !read (nu_itt,'(A)') title !do not need to read in blank line in unformatted read
   read (nu_itt,'(A)') title
   read (nu_itt,'(A)') title

   !Read in surface temperature Tgs array in combustion space grid.
   jb=1
   do
      je=jb+10
      je=min(je,nz) 
      !read (nu_itt,'(A)') title !do not need to read in blank line in unformatted read
      read (nu_itt,'(A)') title
      read (nu_itt,'(A)') title   
      do i=1,mz
         read (nu_itt,"(F11.3,11f11.1)") g0,(Tgs(i,j),j=jb,je)
      enddo   
      jb=je+1
      if (jb > nz) exit
   enddo
endif 
close(nu_itt)

Tgs_bak=Tgs ! save Tgs so we can relax it again if not good enough
irelax_nogood=1 !used for manual execution/debugging
!rfTsurf=0.7d0  !init in var_mod, moved to relaxation factors file
!rfTsurf_c=1-rfTsurf
do while(irelax_nogood==1)

   !Set temperature at melt surface (relaxed)
   surfT_relaxed=0
   Tgs_max=zero
   max_error=0
   do j=4,np-2,2
   do i=4,mp-2,2
      if (ibcell(i,j,2)==4)then
         !if (irelax_Tsurf == 1) then
         if (itn > 0) then
            Tgs(i/2,j/2) = rfTsurf*Tgs(i/2,j/2) + rfTsurf_c*T_surf_prev(i/2,j/2) !relax Tgs
            if (tgs(i/2,j/2) > t_mx*t0) then
               tgs(i/2,j/2)=t_mx*t0
               max_error = max_error+1
            endif
            surfT_relaxed=surfT_relaxed + Tgs(i/2,j/2)*dx(i)*dr(j)*area0
            Tgs_max=max(Tgs_max,Tgs(i/2,j/2))
         endif
         T(i,j,2)=Tgs(i/2,j/2)/T0
      endif
   enddo;enddo

   !if (irelax_Tsurf == 1) then
   if (itn > 0) then
      surfT_relaxed = surfT_relaxed/area_melt_surf

      !Preserve surface mean temperature in relaxed distribution
      Tsurf_mean_check=0
      Tgs_max=0
      do j=4,np-2,2
      do i=4,mp-2,2
         if (ibcell(i,j,2)==4)then
            Tgs(i/2,j/2) = Tgs(i/2,j/2)*surf_T/surfT_relaxed
            Tsurf_mean_check=Tsurf_mean_check + Tgs(i/2,j/2)*dx(i)*dr(j)*area0
            Tgs_max=max(Tgs_max,Tgs(i/2,j/2)) !Reset Tgs_max
            T(i,j,2)=Tgs(i/2,j/2)/T0
         endif
      enddo;enddo
      surfT_relaxed=Tsurf_mean_check/area_melt_surf

      if (irelax==1) then
         !print out relaxed surface temperature file for debugging
         filename=casedir//'\it'//runum//'T_relax.dat'
         open(nu_relax,file=filename)
         write(nu_relax,*) itn," Distribution indicator"
         write(nu_relax,*) q_melt_req," Energy required by melter (W)"
         write(nu_relax,*) qls_s_T," Combustion Heat Transfer to Melter (W)"
         write(nu_relax,*) surfT_relaxed," Mean Surface Temperature (K)"

         write(nu_relax,'(1X/T10,"Relaxed Surface Temperature (K)")')
         jb=2
         do while (jb <= np)
            je=jb+20
            je=min(je,np)
            write(nu_relax,' ')
            write(nu_relax,'(" X / Y",T12,11F11.3)') (r(j),j=jb,je,2)
            do i=2,mp,2
               write(nu_relax,'(F11.3,11F11.1)') x(i),(Tgs(i/2,j/2),j=jb,je,2)
            enddo
            jb=je+2
         enddo
         write(nu_relax,' ')
         write(nu_relax,*) surf_T," Input Mean Surface Temperature (K)"
         write(nu_relax,*) surfT_relaxed," Relaxed Mean Surface Temperature (K)"
         write(nu_relax,*) rfTsurf," Relaxation Factor"
         write(nu_relax,*) max_error," number times t is >t_mx"
         close(nu_relax)
      endif !debug
   else !(itn=0)
      exit
   endif

   if (irelax_nogood==0 .or. (Tgs_max-surfT_relaxed)<limit_adj_relaxT .or. rfTsurf<0.1) exit
   rfTsurf=rfTsurf*adj_relaxT
   rfTsurf_c=1-rfTsurf
   Tgs=Tgs_bak

enddo

T_surf_prev = Tgs !move Tgs into array for later save in restart file
deallocate(Tgs) 
deallocate(Tgs_bak) 

return 
end


