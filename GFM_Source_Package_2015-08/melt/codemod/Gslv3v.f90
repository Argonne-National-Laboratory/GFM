!======================================================================
!======================================================================
!======================================================================
! GSLV3V.F90
!     GSLV3V solves flow property states by using
!        the simple or the simpler procedures
!     revision:  11/00
!======================================================================
      SUBROUTINE GSLV3V
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3)
      REAL*8, ALLOCATABLE :: FZ0(:,:,:)
      KD=2
      SGM=ONE
!----------------------------------------------------------------------
!     Glass pseudovelocities and flux coefficients
!     IU1=1  X-DIRECTION 
!         2  Y or R-DIRECTION 
!         3  Z-DIRECTION 
!     AS(I,J,K,L,M)
!         (I,J,K) node index
!         L=1,2,3: direction index
!         M=1,2: negative or positive direction
!----------------------------------------------------------------------
      AP=ONE
      AS=ZERO
      !Pack momentum cell velocity values into fz array for all
	   !directions
      DO IU1=1,3
         !IF (LP.LT.4.AND.IU1.EQ.3) cycle !old code used for 2D
         NX=0
         NX(IU1)=1
         DO I=2+NX(1),MP-NX(1),2
         DO J=2+NX(2),NP-NX(2),2
         DO K=2+NX(3),LP-NX(3),2
            FZ(I,J,K)=LG(I,J,K)%U(IU1)
         enddo;enddo;enddo
      enddo

      DO IU1=1,3    
         !IF (LP.LT.4.AND.IU1.EQ.3) cycle !old code used for 2D
         NELG=IU1
         !Call to calculate SIMPLER pseudo-velocities 
	      !and momentum equation coefficients
         CALL GSLVX1(MI1,MI3,NJ1,NJ3,LK1,LK3,IU1)
      enddo

!----------------------------------------------------------------------
!-----Glass Pressure
!----------------------------------------------------------------------
      NELG=4 
      MI1=4
      MI3=MP_E
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2
      CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
      DO I=MI1,MI3,2
      DO J=NJ1,NJ3,2
      DO K=LK1,LK3,2
         IF (IBCELL(I,J,K).GE.1) THEN
            BS(I,J,K)=LG(I,J,K)%P
            cycle
         ENDIF
         AP(I,J,K)=-SFP(I,J,K)
         I1=I-1
         DO M=1,2
            IF (M.EQ.2) I1=I+1
            IBC1=IBCELL(I1,J,K)
            IF (IBC1.LE.0.AND.ABS(AP(I1,J,K)).GT.SMALL) THEN
               AREA=AREA_C(I,J,K,1)
               AS(I,J,K,1,M)=LG(I1,J,K)%TH**2*LG(I1,J,K)%DS*AREA**2/AP(I1,J,K)*DXAV(I1,J,K)/DX(I1)
            ENDIF
            AP(I,J,K)=AP(I,J,K)+AS(I,J,K,1,M)
         ENDDO
         !IF (NP.LE.6) cycle !old code used for 2D
         J1=J-1
         DO M=1,2
            IF (M.EQ.2) J1=J+1
            IBC1=IBCELL(I,J1,K)
            IF (IBC1.LE.0.AND.ABS(AP(I,J1,K)).GT.SMALL) THEN
               AREA=AREA_C(I,J,K,2)
               AS(I,J,K,2,M)=LG(I,J1,K)%TH**2*LG(I,J1,K)%DS*AREA**2/AP(I,J1,K)*DYAV(I,J1,K)/Dy(J1)
            ENDIF
            AP(I,J,K)=AP(I,J,K)+AS(I,J,K,2,M)
         ENDDO
         !IF (LP.LE.6) cycle !old code used for 2D
         K1=K-1
         DO M=1,2
            IF (M.EQ.2) K1=K+1
            IBC1=IBCELL(I,J,K1)
            IF (IBC1.LE.0.AND.ABS(AP(I,J,K1)).GT.SMALL) THEN
               AREA=AREA_C(I,J,K,3)
               AS(I,J,K,3,M)=LG(I,J,K1)%TH**2*LG(I,J,K1)%DS*AREA**2/AP(I,J,K1)*DZAV(I,J,K1)/DZ(K1)
            ENDIF
            AP(I,J,K)=AP(I,J,K)+AS(I,J,K,3,M)
         ENDDO
      enddo;enddo;enddo


      ALLOCATE (FZ0(MP,NP,LP))
      FZ0=LG%P
      if (igresidp==1) call calc_residual(fz0,p_resid_pre,mi1,mi3,nj1,nj3,lk1,lk3)
      CALL ADLBL3(FZ0,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
      if (igresid==1)  call calc_residual(fz0,p_resid,mi1,mi3,nj1,nj3,lk1,lk3)
      LG%P=FZ0
      DEALLOCATE (FZ0)
      DO I=MI1,MI3,2
      DO J=NJ1,NJ3,2
      DO K=LK1,LK3,2
         IF (IBCELL(I,J,K).GE.1) cycle
         CALL BOUND(LG(I,J,K)%P,0,1)
      enddo;enddo;enddo
      
!----------------------------------------------------------------------
!     Glass starred velocity - Solve Momentum Equations
!----------------------------------------------------------------------
      DO IU1=1,3
         !IF (IU1.EQ.2.AND.NP.LE.6) cycle !old code used for 2D
         !IF (IU1.EQ.3.AND.LP.LE.6) cycle !old code used for 2D
         NELG=IU1 
         NX=0
         NX(IU1)=1
         MI1=4+NX(1)
         MI3=MP_E-NX(1)
         NJ1=NJY1V(IU1)
         NJ3=NJY2V(IU1)
         LK1=LKZ1V(IU1)
         LK3=LKZ2V(IU1)
         DO I=MI1,MI3,2; IP1=I+1; IM1=I-1
         DO J=NJ1,NJ3,2; JP1=J+1; JM1=J-1
         DO K=LK1,LK3,2
            IF (IBCELL(I,J,K).GE.1) cycle
            AP(I,J,K)=AP(I,J,K)/RFL(NELG)
            VOL=LG(I,J,K)%TH*VOL_C(I,J,K)
            IF (IU1.EQ.1) THEN
               DPDX=(LG(IM1,J,K)%P-LG(IP1,J,K)%P)/DX(I)
            ELSEIF (IU1.EQ.2) THEN
               DPDX=(LG(I,JM1,K)%P-LG(I,JP1,K)%P)/dy(J)
            ELSE
               DPDX=(LG(I,J,K-1)%P-LG(I,J,K+1)%P)/DZ(K)
            ENDIF
            !BS(I,J,K)=BS(I,J,K)+DPDX*EUN*VOL+RFLC(NELG)*AP(I,J,K)*FZ(I,J,K)
            BS(I,J,K)=BS(I,J,K)+DPDX*VOL+RFLC(NELG)*AP(I,J,K)*FZ(I,J,K)
         enddo;enddo;enddo

         if (igresidp==1) call calc_residual(fz,u_resid_pre(iu1),mi1,mi3,nj1,nj3,lk1,lk3)
         CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
         if (igresid==1) call calc_residual(fz,u_resid(iu1),mi1,mi3,nj1,nj3,lk1,lk3)

         DO I=MI1,MI3,2
         DO J=NJ1,NJ3,2
         DO K=LK1,LK3,2
            IF (IBCELL(I,J,K).GE.1) cycle
            LG(I,J,K)%U(IU1)=FZ(I,J,K)
         enddo;enddo;enddo
      enddo

!----------------------------------------------------------------------
!-----PRESSURE CORRECTION EQUATION
!----------------------------------------------------------------------
      NELG=5
      MI1=4
      MI3=MP_E
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2
      CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
      DO I=2,MP,2
      DO J=2,NP,2
      DO K=2,LP,2
         FZ(I,J,K)=ZERO
      enddo;enddo;enddo

      if (igcorp==1) call calc_residual(fz,pcorr_resid_pre,mi1,mi3,nj1,nj3,lk1,lk3)
      CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
      if (igcor==1) call calc_residual(fz,pcorr_resid,mi1,mi3,nj1,nj3,lk1,lk3)

!-----CORRECT PRESSURE IF USING SIMPLE
!     Lottes 1/18/05: use of SIMPLE is not currently an option
!     careful review of the entire code is required before use
!      IF (.NOT.SMPLER) THEN
!         DO I=2,MP,2
!         DO J=2,NP,2
!         DO K=2,LP,2
!            LG(I,J,K)%P=LG(I,J,K)%P+RFL(20)*FZ(I,J,K)
!         enddo;enddo;enddo
!      ENDIF 

!----------------------------------------------------------------------
!-----VELOCITY MODIFICATION BY CORRECTION PRESSURE P'
!----------------------------------------------------------------------
      uc_mean = zero
      DO IU1=1,3
         !IF (IU1.EQ.2.AND.NP.LE.6) cycle !old code used for 2D
         !IF (IU1.EQ.3.AND.LP.LE.6) cycle !old code used for 2D
         NX=0
         NX(IU1)=1
         MI1=4+NX(1)
         MI3=MP_E-NX(1)
         NJ1=NJY1V(IU1)
         NJ3=NJY2V(IU1)
         LK1=LKZ1V(IU1)
         LK3=LKZ2V(IU1)

         DO I=MI1,MI3,2
         DO J=NJ1,NJ3,2
         DO K=LK1,LK3,2 
            IF (IBCELL(I,J,K).GE.1.OR.ABS(AP(I,J,K)).LT.SMALL) cycle
            VOL=LG(I,J,K)%TH*VOL_C(I,J,K)
            IF (IU1.EQ.1) THEN
               DPDX=(FZ(I-1,J,K)-FZ(I+1,J,K))/DX(I)
            ELSEIF (IU1.EQ.2) THEN
               DPDX=(FZ(I,J-1,K)-FZ(I,J+1,K))/dy(J)
            ELSE
               DPDX=(FZ(I,J,K-1)-FZ(I,J,K+1))/DZ(K)
            ENDIF
            !LG(I,J,K)%U(IU1)=LG(I,J,K)%U(IU1)+DPDX*EUN*VOL/(AP(I,J,K)*RFL(IU1))
            !Lottes 1/18/05: Source of relaxation factor in expression:
            !                retrieves ap before relaxation in momentum equations above
            !                Note velocity corrections are not relaxed.
            u_corr = DPDX*VOL/(AP(I,J,K)*RFL(IU1))
            LG(I,J,K)%U(IU1)=LG(I,J,K)%U(IU1)+u_corr
 
            uc_mean(iu1) = uc_mean(iu1) + abs(u_corr/LG(I,J,K)%U(IU1))*vol
            CALL BOUND(LG(I,J,K)%U(IU1),IU1,1)
         enddo;enddo;enddo
         uc_mean(iu1)=uc_mean(iu1)/vol_tot
      enddo

!----------------------------------------------------------------------
!     Velocity extrapolation and interpolation
!----------------------------------------------------------------------
      iskip=1
      if(iskip==0) then !Pflow calls qloss, vel. solve does not affect wall loss
         CALL QLOSS !compute heat loss to walls
         
         !update scaling factor and reset qrs
         
         old_facq=facq
         facq=(h_needed-eb_heat+q_wall_loss_tot)/qrs_tot_base
         !facq=(h_needed-eb_heat+q_wall_loss_tot)/qrs_tot
         !qrs_tot=zero
         facq_chg=abs(facq-old_facq)/facq
         if (iheat_flux_type < 3) then !scaled uniform or scaled combustion calculated heat flux
            qrs_tot=zero
            do i=2,i_me,2
            do j=2,np,2
               qrs(i/2,j/2)=qc_surf0(i/2,j/2)*facq
               !qrs(i/2,j/2)=qrs(i/2,j/2)*facq/old_facq
               !qrs_tot=qrs_tot+qrs(i/2,j/2)
               qrs_tot=qrs_tot+qrs(i/2,j/2)
            enddo;enddo
         endif
      endif         
      CALL FLX1D(1)
!----------------------------------------------------------------------
!   This loop creates the correction factors, PULL(N_EX,3), to adjust exit velocities
!   Correction factor is bounded between 0.5 and 2
!   PULL(N_EX,1) prescribed pull fractions
!   PULL(N_EX,2) calculated pull mass flow rates
!-----------------------------------------------------------------------
      !DO N=1,NEX0 !Loop over exits
      !   G0=PULL(N,1)*GFIN !kg/s through exit n
      !   IF (PULL(N,2).LT.G0/2) THEN
      !      PULL(N,3)=2
      !   ELSEIF (PULL(N,2).GT.G0*2) THEN
      !      PULL(N,3)=0.5D+0
      !   ELSE
      !      PULL(N,3)=G0/PULL(N,2)
      !   ENDIF
      !ENDDO
      adj_m_max=2.0d+0
      adj_m_min=0.5d+0

      do n=1,nex0 !Loop over exits
         !pull(n,1)*gfin !kg/s that should go through exit n
         pull(n,3) = max(adj_m_min,min(adj_m_max,pull(n,1)*gfin/pull(n,2)))
      enddo
!--------------------------------------------------------------------------------------
! Map (ibcell) values for exits
!
!      103 = exit in positive x-direction
!      113 = exit in negative x-direction
!      203 = exit in positive y-direction
!      213 = exit in negative y-direction
!      303 = exit in positive z-direction
!      313 = exit in negative z-direction
!--------------------------------------------------------------------------------------
! Pressure extrapolation at exits
!
! Ip_bndry = 1 if a pressure boundary condition is set at exits
!            to control the mass flow rate through the exit
!
!          = 0 otherwise
!
!--------------------------------------------------------------------------------------
      ip_bndry = 0
      if (ip_bndry == 1) then
         !Find x-direction exits
         do i=3,mp-1,2
         do j=2,np,2
         do k=2,lp,2
            ibc0=ibcell(i,j,k)
            if (ibc0.ne.103.and.ibc0.ne.113) cycle
            if (ibcell(i,j,k).eq.103) then
               n_ex=ibcell(i+1,j,k)/10
               i0=i+1
            elseif (ibcell(i,j,k).eq.113) then
               n_ex=ibcell(i-1,j,k)/10
               i0=i-1
            endif
            if (pull(n_ex,3) < one) then !too much out, increase P_ex
               lg(i,j,k)%p=lg(i,j,k)%p+(1-pull(n_ex,3))*abs(lg(i,j,k)%p)
            else                      !too little out, decrease P_ex
               lg(i,j,k)%p=lg(i,j,k)%p -(1-1/pull(n_ex,3))*abs(lg(i,j,k)%p)
            endif
            lg(i0,j,k)%p=lg(i,j,k)%p
         enddo;enddo;enddo
         
         !Find y-direction exits
         do j=3,np-1,2
         do i=2,mp,2
         do k=2,lp,2
            ibc0=ibcell(i,j,k)
            if (ibc0.ne.203.and.ibc0.ne.213) cycle
            if (ibcell(i,j,k).eq.203) then
               n_ex=ibcell(i,j+1,k)/10
               j0=j+1
            elseif (ibcell(i,j,k).eq.213) then
               n_ex=ibcell(i,j-1,k)/10
               j0=j-1
            endif
            if (pull(n_ex,3) < one) then !too much out, increase P_ex
               lg(i,j,k)%p=lg(i,j,k)%p+(1-pull(n_ex,3))*abs(lg(i,j,k)%p)
            else                      !too little out, decrease P_ex
               lg(i,j,k)%p=lg(i,j,k)%p -(1-1/pull(n_ex,3))*abs(lg(i,j,k)%p)
            endif
            lg(i,j0,k)%p=lg(i,j,k)%p
         enddo;enddo;enddo
         
         !Find z-direction exits
         do k=3,lp-1,2
         do i=2,mp,2
         do j=2,np,2
            ibc0=ibcell(i,j,k)
            if (ibc0.ne.303.and.ibc0.ne.313) cycle
            if (ibcell(i,j,k).eq.303) then
               n_ex=ibcell(i,j,k+1)/10
               k0=k+1
            elseif (ibcell(i,j,k).eq.313) then
               n_ex=ibcell(i,j,k-1)/10
               k0=k-1
            endif
            if (pull(n_ex,3) < one) then !too much out, increase P_ex
               lg(i,j,k)%p=lg(i,j,k)%p+(1-pull(n_ex,3))*abs(lg(i,j,k)%p)
            else                      !too little out, decrease P_ex
               lg(i,j,k)%p=lg(i,j,k)%p -(1-1/pull(n_ex,3))*abs(lg(i,j,k)%p)
            endif
            lg(i,j,k0)%p=lg(i,j,k)%p
         enddo;enddo;enddo

      else !Extrapolate pressure to exit

         !Find x-direction exits
         DO I=3,MP-1,2
         DO J=2,NP,2
         DO K=2,LP,2
            IBC0=IBCELL(I,J,K)
            IF (IBC0.NE.103.AND.IBC0.NE.113) cycle
            IF (IBCELL(I,J,K).EQ.103) THEN
               N_EX=IBCELL(I+1,J,K)/10
               I0=I+1
               I1=I-1
               I2=I-2
               I3=I-3
            ELSEIF (IBCELL(I,J,K).EQ.113) THEN
               N_EX=IBCELL(I-1,J,K)/10
               I0=I-1
               I1=I+1
               I2=I+2
               I3=I+3
            ENDIF
            CALL EXTR1(F1,F2,I,J,K,I1,J,K,I3,J,K,1)
            LG(I,J,K)%P=LG(I1,J,K)%P*F1+LG(I3,J,K)%P*F2
            LG(I0,J,K)%P=LG(I,J,K)%P
         enddo;enddo;enddo
         
         !Find y-direction exits
         DO J=3,NP-1,2
         DO I=2,MP,2
         DO K=2,LP,2
            IBC0=IBCELL(I,J,K)
            IF (IBC0.NE.203.AND.IBC0.NE.213) cycle
            IF (IBCELL(I,J,K).EQ.203) THEN
               N_EX=IBCELL(I,J+1,K)/10
               J0=J+1
               J1=J-1
               J2=J-2
               J3=J-3
            ELSEIF (IBCELL(I,J,K).EQ.213) THEN
               N_EX=IBCELL(I,J-1,K)/10
               J0=J-1
               J1=J+1
               J2=J+2
               J3=J+3
            ENDIF
            CALL EXTR1(F1,F2,I,J,K,I,J1,K,I,J3,K,2)
            LG(I,J,K)%P=LG(I,J1,K)%P*F1+LG(I,J3,K)%P*F2
            LG(I,J0,K)%P=LG(I,J,K)%P
         enddo;enddo;enddo
         
         !Find z-direction exits
         DO K=3,LP-1,2
         DO I=2,MP,2
         DO J=2,NP,2
            IBC0=IBCELL(I,J,K)
            IF (IBC0.NE.303.AND.IBC0.NE.313) cycle
            IF (IBCELL(I,J,K).EQ.303) THEN
               N_EX=IBCELL(I,J,K+1)/10
               K0=K+1
               K1=K-1
               K2=K-2
               K3=K-3
            ELSEIF (IBCELL(I,J,K).EQ.313) THEN
               N_EX=IBCELL(I,J,K-1)/10
               K0=K-1
               K1=K+1
               K2=K+2
               K3=K+3
            ENDIF
            CALL EXTR1(F1,F2,I,J,K,I,J,K1,I,J,K3,3)
            LG(I,J,K)%P=LG(I,J,K1)%P*F1+LG(I,J,K3)%P*F2
            LG(I,J,K0)%P=LG(I,J,K)%P
         enddo;enddo;enddo

      endif
      CALL EXTRV(0)
!----------------------------------------------------------------------
!      DATA FADJ0,F_MX,F_MN /1.0D-2,1.2D+0,0.8D+0/
!     G0=0
!      IF (GFIN.LE.SMALL) GO TO 1200
!      I_MEP2=I_ME+2
!     DO I=I_MEP2,ITEND,2
!        G0=G0+FLX(I,1)
!     ENDDO
!     G0=G0/(ITEND-I_ME-1)
!     FADJ=FADJ+(G0/GFIN-ONE)*FADJ0
!     IF (FADJ.LT.F_MN) FADJ=F_MN
!     IF (FADJ.GT.F_MX) FADJ=F_MX
!      J=NP-2
!      DO I=ITEND,MP,2
!      DO K=2,LP,2
!        IBC0=IBCELL(I,J,K)
!        IF (IBC0.GE.1) CYCLE
!        LG(I,J,K)%TH=FADJ
!     ENDDO
!     ENDDO
!1200  continue
      CALL EXTRV(1)
      CALL INTPV
      RETURN
      END


!======================================================================
!======================================================================
!======================================================================
!     Heat loss through walls and interfaces
!======================================================================
      SUBROUTINE QLOSS
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      QLS=0
      q_wall_loss_tot=0
      !DO I=4,MPM2,2; ID2=I/2
      DO I=4,i_exit,2; ID2=I/2 !no heat loss in exit tunnel @@@@
      DO J=2,NP,2;   JD2=J/2
      DO K=2,LP,2
         !Lottes 4/28/05: probably faster to check wall cells to see if they
         !       have a neighbor flow cell.
         IF (IBCELL(I,J,K).GE.1) cycle
         TT=LG(I,J,K)%T
         KD2=K/2
         if(iwall_loss_est==1) TT=T_exit
         IBC1=MOD(IBCELL(I-2,J,K),10)
         IF (IBC1.EQ.1) THEN ! Wall in minus x-direction
            IW1=IBCELL(I-2,J,K)/10
            AR0=AREA_C(I,J,K,1)
            CALL QLS_1
         ENDIF
         IBC1=MOD(IBCELL(I+2,J,K),10)
         IF (IBC1.EQ.1) THEN ! Wall in plus x-direction
            IW1=IBCELL(I+2,J,K)/10
            AR0=AREA_C(I,J,K,1)
            CALL QLS_1
         ENDIF
         IBC1=MOD(IBCELL(I,J-2,K),10)
         IF (IBC1.EQ.1) THEN ! Wall in minus y-direction 
            IW1=IBCELL(I,J-2,K)/10
            AR0=AREA_C(I,J,K,2)
            CALL QLS_1
         ENDIF
         IBC1=MOD(IBCELL(I,J+2,K),10)
         IF (IBC1.EQ.1) THEN ! Wall in plus y-dircetion
            IW1=IBCELL(I,J+2,K)/10
            AR0=AREA_C(I,J,K,2)
            CALL QLS_1
         ENDIF
         IBC1=MOD(IBCELL(I,J,K-2),10)
         IF (IBC1.EQ.1) THEN ! Wall in minus z-direction
            IW1=IBCELL(I,J,K-2)/10
            AR0=AREA_C(I,J,K,3)
            CALL QLS_1
         ENDIF
         IBC1=MOD(IBCELL(I,J,K+2),10)
         IF (IBC1.EQ.1) THEN ! Wall in plus z-direction
            IW1=IBCELL(I,J,K+2)/10
            AR0=AREA_C(I,J,K,3)
            CALL QLS_1
         ENDIF
         !cbg     QLS(ID2,JD2,KD2)=0
      enddo;enddo;enddo

      iwall_loss_est=0

      RETURN
!======================================================================
!======================================================================
!======================================================================

      CONTAINS
      SUBROUTINE QLS_1
      !Split this to speed it up: no need to do these assignments, Lottes 5/20/05 @@ 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (IW1.LE.0) THEN !next to a normal wall, use its properties, Lottes
         W_K0=W_K
         W_HA0=W_HA
         W_L0=W_D
         W_T0=W_TA
         W_E0=W_E
      ELSE !next to an exit or possibly other non-wall surface (inlet, bubbler, etc.), Lottes
         W_K0=W1_K(IW1)
         W_HA0=W1_HA(IW1)
         W_L0=W1_D(IW1)
         W_T0=W1_TA(IW1)
         W_E0=W1_E(IW1)
      ENDIF
      W_R0=W_L0/W_K0+1/W_HA0
      QLS0=(TT-W_T0)*AR0/W_R0
      QLS(ID2,JD2,KD2)=QLS(ID2,JD2,KD2)+QLS0
      q_wall_loss_tot=q_wall_loss_tot+qls0 !Total conduction losses through boundaries
      END SUBROUTINE QLS_1
      END
