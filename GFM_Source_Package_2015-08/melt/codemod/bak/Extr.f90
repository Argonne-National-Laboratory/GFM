!======================================================================
! EXTR.F90
!======================================================================
!======================================================================
!======================================================================
!     Extrapolation of calculated variables 
!     M_S=1: liquid variables 
!         2: cullet variables 
!         3: sand variables 
!         4: bubble variables 
!       7/01
!======================================================================
SUBROUTINE EXTR(M_S)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
!     IBCELL: node
!         2: inlet 
!         3: exit
!----------------------------------------------------------------------
      IF (M_S.EQ.1) GFIN0=ZERO
      DO 120 I=2,MP,2
      DO 120 J=2,NP,2
      DO 120 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.2.AND.IBC0.NE.3) CYCLE !skip if not inlet or outlet
         IF (IBC0.EQ.2.AND.M_S.NE.1) CYCLE !skip if inlet and not liquid
         CALL EXT0(I,J,K,I1,J1,K1,I2,J2,K2,IX)
         CALL EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,IX)
         IF (M_S.EQ.1) THEN
            LG(I,J,K)%P=LG(I1,J1,K1)%P*F1+LG(I2,J2,K2)%P*F2

            !Lottes 1/9/06
            !Extrapolating interior values to the inlet boundary appears to be
            !best for post processing, since values at these boundaries may
            !weight the interpolation in the interior for color plots
            LG(I,J,K)%h=LG(I1,J1,K1)%h*F1+LG(I2,J2,K2)%h*F2
            LG(I,J,K)%ds=LG(I1,J1,K1)%ds*F1+LG(I2,J2,K2)%ds*F2
            CALL THER(I,J,K) !mu, cp, k
            Lg(i,j,k)%T=Lg(i,j,k)%h/Lg(i,j,k)%c 
            if (Lg(i,j,k)%T > T_mx) then !general high bound
               Lg(i,j,k)%T = T_mx
               Lg(i,j,k)%h = Lg(i,j,k)%c * Lg(i,j,k)%T
            endif
            if (Lg(i,j,k)%T < Tmltr) then !lowest melt temperature bound
               Lg(i,j,k)%T = Tmltr
               Lg(i,j,k)%h = Lg(i,j,k)%c * Lg(i,j,k)%T
            endif
            if (Tmax_c.ne.zero .and. Lg(i,j,k)%T > Tmax_c) then !maximum temperature in combustion space
               Lg(i,j,k)%T = Tmax_c
               Lg(i,j,k)%h = Lg(i,j,k)%c * Lg(i,j,k)%T
            endif
            
            IF (IBC0.EQ.2) THEN
               AREA=AREA_C(I,J,K,IX)
               G0=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(IX)*AREA
               GFIN0=GFIN0+ABS(G0)
               CYCLE
            ENDIF
            DO L=LSTAR,LEND
               LG(I,J,K)%F(L)=LG(I1,J1,K1)%F(L)*F1+LG(I2,J2,K2)%F(L)*F2
               CALL BOUND(LG(I,J,K)%F(L),L,2)
            ENDDO
            !LG(I,J,K)%H=LG(I1,J1,K1)%H
            !LG(I,J,K)%T=LG(I1,J1,K1)%T
            !LG(I,J,K)%DS=LG(I1,J1,K1)%DS
            !CALL THER(I,J,K) !mu, cp, k
            IF (NPHAS.EQ.1) GOTO 120
            LG(I,J,K)%TH=LG(I1,J1,K1)%TH*F1+LG(I2,J2,K2)%TH*F2
            LG(I,J,K)%MR=0
!----------------------------------------------------------------------
         ELSEIF (M_S.EQ.5) THEN
            DO L=LSTARM,LENDM
               G0=GFM(I1,J1,K1,L)*F1+GFM(I2,J2,K2,L)*F2
               GFM(I,J,K,L)=MAX(G0,ZERO)
               !cz            JJ=(J+J1)/2
               !cz            GFM(I,JJ,K,L)=GFM(I,J,K,L)
            ENDDO
            !cz            IF (NBS0.GE.1) CON(ID2,JD2,KD2)=ZERO
!----------------------------------------------------------------------
         ELSE !This must be an outlet
            ID2=I/2
            JD2=J/2
            KD2=K/2
            I1=I1/2
            J1=J1/2
            K1=K1/2
            I2=I2/2
            J2=J2/2
            K2=K2/2
            !           copy batch conditions from neighbor cell at outlet
            IF (M_S.EQ.2) THEN
               DO L=1,NPS_C
                  PC(ID2,JD2,KD2,L)%U(1)=PC(I1,J1,K1,L)%U(1)
                  IF (NP.GE.6) PC(ID2,JD2,KD2,L)%U(2)=PC(I1,J1,K1,L)%U(2)
                  IF (LP.GE.6) PC(ID2,JD2,KD2,L)%U(3)=PC(I1,J1,K1,L)%U(3)
                  PC(ID2,JD2,KD2,L)%T=PC(I1,J1,K1,L)%T
                  PC(ID2,JD2,KD2,L)%DN=PC(I1,J1,K1,L)%DN
               ENDDO
               PC0(ID2,JD2,KD2)%TH=PC0(I1,J1,K1)%TH
            ELSEIF (M_S.EQ.3) THEN
               DO L=1,NPS_S
                  PS(ID2,JD2,KD2,L)%U(1)=PS(I1,J1,K1,L)%U(1)
                  IF (NP.GE.6) PS(ID2,JD2,KD2,L)%U(2)=PS(I1,J1,K1,L)%U(2)
                  IF (LP.GE.6) PS(ID2,JD2,KD2,L)%U(3)=PS(I1,J1,K1,L)%U(3)
                  PS(ID2,JD2,KD2,L)%T=PS(I1,J1,K1,L)%T
                  PS(ID2,JD2,KD2,L)%DN=PS(I1,J1,K1,L)%DN
               ENDDO
               PS0(ID2,JD2,KD2)%TH=PS0(I1,J1,K1)%TH
            ELSEIF (M_S.EQ.4) THEN
               DO L=1,NBS0
                  GB4(ID2,JD2,KD2,L)%U(1)=GB4(I1,J1,K1,L)%U(1)
                  IF (NP.GE.6) GB4(ID2,JD2,KD2,L)%U(2)=GB4(I1,J1,K1,L)%U(2)
                  IF (LP.GE.6) GB4(ID2,JD2,KD2,L)%U(3)=GB4(I1,J1,K1,L)%U(3)
                  GB4(ID2,JD2,KD2,L)%T=GB4(I1,J1,K1,L)%T
                  GB4(ID2,JD2,KD2,L)%DN=GB4(I1,J1,K1,L)%DN
               ENDDO
               GB3(ID2,JD2,KD2)%TH=GB3(I1,J1,K1)%TH
         ENDIF
        ENDIF
120   CONTINUE
!----------------------------------------------------------------------
!     IBCELL: node
!         1: wall 
!         4: free surface
!         5: elec. booster
!----------------------------------------------------------------------
      DO 160 I=2,MP,2
      DO 160 J=2,NP,2
      DO 160 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         !IF (IBC0.NE.1.AND.IBC0.NE.4.AND.IBC0.NE.5) CYCLE !Do below for walls
         IF (IBC0.NE.1.AND.IBC0.NE.5) CYCLE !Do below for walls
         CALL EXT1(I,J,K,I1,J1,K1,I2,J2,K2,IX)
         IF (IX.GT.3) CYCLE
         IF (IBC0.EQ.4) THEN
            F1=1
            F2=0
         ELSE 
            CALL EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,IX)
         ENDIF  
         IF (M_S.EQ.1) THEN
            LG(I,J,K)%P=LG(I1,J1,K1)%P*F1+LG(I2,J2,K2)%P*F2
            LG(I,J,K)%T=LG(I1,J1,K1)%T*F1+LG(I2,J2,K2)%T*F2
            LG(I,J,K)%C=LG(I1,J1,K1)%C*F1+LG(I2,J2,K2)%C*F2
            LG(I,J,K)%K=LG(I1,J1,K1)%K*F1+LG(I2,J2,K2)%K*F2
            LG(I,J,K)%MU=LG(I1,J1,K1)%MU*F1+LG(I2,J2,K2)%MU*F2
            Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
            !CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
            I10=(I+I1)/2
            J10=(J+J1)/2
            K10=(K+K1)/2
            LG(I10,J10,K10)%MU=LG(I,J,K)%MU
            CALL BOUND(LG(I,J,K)%T,4,1)
            DO L=LSTAR,LEND
               LG(I,J,K)%F(L)=LG(I1,J1,K1)%F(L)
            ENDDO
            CALL DSLG(I,J,K)
            !CZEB
            IF (IBC0.EQ.5) CYCLE
            IF (NPHAS.EQ.1) GOTO 160
            LG(I,J,K)%TH=LG(I1,J1,K1)%TH
            LG(I,J,K)%MR=LG(I1,J1,K1)%MR
            ELSEIF (M_S.EQ.5) THEN
              DO L=LSTARM,LENDM
               GFM(I,J,K,L)=GFM(I1,J1,K1,L)
               !cz            JJ=(J+J1)/2
               !cz            GFM(I,JJ,K,L)=GFM(I,J,K,L)
              ENDDO
            ID2=I/2
            JD2=J/2
            KD2=K/2
            I1=I1/2
            J1=J1/2
            K1=K1/2
            I2=I2/2
            J2=J2/2
            K2=K2/2
            IF (NBS0.GE.1) CON(ID2,JD2,KD2)=CON(I1,J1,K1)
!----------------------------------------------------------------------
         ELSE
            ID2=I/2
            JD2=J/2
            KD2=K/2
            I1=I1/2
            J1=J1/2
            K1=K1/2
            I2=I2/2
            J2=J2/2
            K2=K2/2
            IF (M_S.EQ.2) THEN
               DO L=1,NPS_C
                  PC(ID2,JD2,KD2,L)%DN=PC(I1,J1,K1,L)%DN
                  PC(ID2,JD2,KD2,L)%T=PC(I1,J1,K1,L)%T
               ENDDO
               PC0(ID2,JD2,KD2)%TH=PC0(I1,J1,K1)%TH
!----------------------------------------------------------------------
            ELSEIF (M_S.EQ.3) THEN
               DO L=1,NPS_S
                  PS(ID2,JD2,KD2,L)%DN=PS(I1,J1,K1,L)%DN
                  PS(ID2,JD2,KD2,L)%T=PS(I1,J1,K1,L)%T
               ENDDO
               PS0(ID2,JD2,KD2)%TH=PS0(I1,J1,K1)%TH
!----------------------------------------------------------------------
            ELSEIF (M_S.EQ.4) THEN
               DO L=1,NBS0
                  GB4(ID2,JD2,KD2,L)%DN=GB4(I1,J1,K1,L)%DN
                  GB4(ID2,JD2,KD2,L)%T=GB4(I1,J1,K1,L)%T
                  !cz                  GB4(ID2,JD2,KD2,L)%U(1)=GB4(I1,J1,K1,L)%U(1)
                  !cz                  IF (NP.GE.6) GB4(ID2,JD2,KD2,L)%U(2)
                  !cz     &               =GB4(I1,J1,K1,L)%U(2)
                  !cz                  IF (LP.GE.6) GB4(ID2,JD2,KD2,L)%U(3)
                  !cz     &               =GB4(I1,J1,K1,L)%U(3)
               ENDDO
               GB3(ID2,JD2,KD2)%TH=GB3(I1,J1,K1)%TH
!----------------------------------------------------------------------
         ENDIF
         ENDIF
160   CONTINUE

!----------------------------------------------------------------------
!Set free surface boundary conditions - Lottes 6/2/05

!Set pressure on free surface to atmospheric

      do i=2,mp
      do j=2,np
         if (ibcell(i,j,Lp)==4) then
            Lg(i,j,Lp)%P  =p_ref
            Lg(i,j,Lp-1)%P=p_ref
         endif
      enddo;enddo

!Set free surface normal velocity to zero
!Set free surface tangential velocity to free slip (zero gradient)
!Set free surface viscosity to zero

      do i=2,mp
      do j=2,np
         if (ibcell(i,j,Lp)==4) then
            Lg(i,j,Lp)%u(1)  =Lg(i,j,Lp-2)%u(1)
            Lg(i,j,Lp-1)%u(1)=Lg(i,j,Lp-2)%u(1)
            Lg(i,j,Lp)%u(2)  =Lg(i,j,Lp-2)%u(2)
            Lg(i,j,Lp-1)%u(2)=Lg(i,j,Lp-2)%u(2)
            Lg(i,j,Lp)%u(3)  =zero
            Lg(i,j,Lp-1)%u(3)=zero

            Lg(i,j,Lp)%mu  =zero
            Lg(i,j,Lp-1)%mu=zero
         endif
      enddo;enddo

!----------------------------------------------------------------------
! Don't know what code below is doing, Lottes 6/2/05
      DO 250 K=2,LP,2
      DO 250 I=2,MP,2
      DO 250 J=2,NP,2
         IF (IBCELL(I,J,K).NE.1) CYCLE
         IP2=I+2
         IF (IP2.GT.MP) IP2=MP
         IF (IBCELL(IP2,J,K).LE.0) CYCLE
         IM2=I-2
         IF (IM2.LT.2) IM2=2
         IF (IBCELL(IM2,J,K).LE.0) CYCLE
         JP2=J+2
         IF (JP2.GT.NP) JP2=NP
         IF (IBCELL(I,JP2,K).LE.0) CYCLE
         JM2=J-2
         IF (JM2.LT.2) JM2=2
         IF (IBCELL(I,JM2,K).LE.0) CYCLE
         KP2=K+2
         IF (KP2.GT.LP) KP2=LP
         IF (IBCELL(I,J,KP2).LE.0) CYCLE
         KM2=K-2
         IF (KM2.LT.2) KM2=2
         IF (IBCELL(I,J,KM2).LE.0) CYCLE
         IF (IBCELL(IP2,JP2,K).LE.0) THEN
            LG(I,J,K)%T=(LG(IP2,J,K)%T+LG(I,JP2,K)%T)/2
         ENDIF
         IF (IBCELL(IP2,JM2,K).LE.0) THEN
            LG(I,J,K)%T=(LG(IP2,J,K)%T+LG(I,JM2,K)%T)/2
         ENDIF
         IF (IBCELL(IM2,JP2,K).LE.0) THEN
            LG(I,J,K)%T=(LG(IM2,J,K)%T+LG(I,JP2,K)%T)/2
         ENDIF
         IF (IBCELL(IM2,JM2,K).LE.0) THEN
            LG(I,J,K)%T=(LG(IM2,J,K)%T+LG(I,JM2,K)%T)/2
         ENDIF
         IF (IBCELL(IP2,J,KP2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KP2)%T+LG(IP2,J,K)%T)/2
         ENDIF
         IF (IBCELL(IP2,J,KM2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KM2)%T+LG(IP2,J,K)%T)/2
         ENDIF
         IF (IBCELL(IM2,J,KP2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KP2)%T+LG(IM2,J,K)%T)/2
         ENDIF
         IF (IBCELL(IM2,J,KM2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KM2)%T+LG(IM2,J,K)%T)/2
         ENDIF
         IF (IBCELL(I,JP2,KP2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KP2)%T+LG(I,JP2,K)%T)/2
         ENDIF
         IF (IBCELL(I,JP2,KM2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KM2)%T+LG(I,JP2,K)%T)/2
         ENDIF
         IF (IBCELL(I,JM2,KP2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KP2)%T+LG(I,JM2,K)%T)/2
         ENDIF
         IF (IBCELL(I,JM2,KM2).LE.0) THEN
            LG(I,J,K)%T=(LG(I,J,KM2)%T+LG(I,JM2,K)%T)/2
         ENDIF
250   CONTINUE
      RETURN
      END


!======================================================================
!======================================================================
!======================================================================
      SUBROUTINE EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!======================================================================

      F2=ZERO
      IF (IX.EQ.3) THEN
         DZK=Z(K2)-Z(K1)
         IF (DZK.NE.ZERO) THEN
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.EQ.2) THEN
            K0=(K+K1)/2
         ELSE
            K0=K
         ENDIF
         F2=(Z(K0)-Z(K1))/DZK
         ENDIF
      ELSEIF (IX.EQ.2) THEN
         dyj=y(J2)-y(J1)
         IF (dyj.NE.ZERO) THEN
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.EQ.2) THEN
               J0=(J+J1)/2
            ELSE
               J0=J
            ENDIF
            F2=(y(J0)-y(J1))/dyj
         ENDIF
      ELSEIF (IX.EQ.1) THEN
         DXI=X(I2)-X(I1)
         IF (DXI.NE.ZERO) THEN
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.EQ.2) THEN
            I0=(I+I1)/2
         ELSE
            I0=I
         ENDIF
         F2=(X(I0)-X(I1))/DXI
         ENDIF
      ENDIF
      F1=ONE-F2
!----------------------------------------------------------------------
      RETURN
      END

!======================================================================
!======================================================================
!======================================================================
!     Extrapolation of exit velocity
!     Routine added by Lottes May 2005
!     Sets an outflow boundary velocity from the mean pull rate
!     at that boundary
!     Recirculation zones near exits may cause problems
!     in this formulation that are not remedied by set outflow velocity
!     This routine is used if i_out_bc_pull == 1 
!======================================================================
      SUBROUTINE EXTRV(MEX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      if (i_out_bc_pull.ne.1) then
         call EXTRV_free_flow(MEX)
         return
      endif

!----------------------------------------------------------------------
!     IBCELL: node
!        3: Exit 
!     IU=1: axial direction, 2: y direction, 3: z (verticle) direction 
!----------------------------------------------------------------------
      IF (MEX.EQ.0) then 
         GFEX=ZERO
         gfex2=zero
         Tmean_ex=zero
         Tarea_mean=0
      endif
!----------------------------------------------------------------------
!     x-direction exits
!----------------------------------------------------------------------
      IU=1 !x-direction
      DO 120 I=3,MPM1,2
      DO 120 J=2,NP,2
      DO 120 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         !IF (IBC0.NE.3) GOTO 120
         if (ibc0 .ne. 3) cycle ! if not exit, Lottes
         DO 110 M=-1,1,2 !m=-1 back, m=+1 forward
            IF (IBCELL(I+M,J,K).GE.1) goto 110 !cycle if open cell not in m-direction
            !Get here when exit is in -m direction, open cell is in m direction
            !if m=-1, exit is behind cell face, m=+1 exit is in front of cell face 
            !velocity at exit should have opposite sign of m
            I0=I-M
            I1=I+2*M
            IF (IBCELL(I+3*M,J,K).LE.0) THEN
               I2=I+4*M
            ELSE
               I2=I1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I1,J,K)%TH*LG(I1,J,K)%DS*LG(I1,J,K)%U(IU)*AREA !exit mass flow
            IF (LG(I1,J,K)%U(IU)*M.GT.SMALL) G0=ZERO !coming in exit, should disappear when converged.
            IF (MEX.EQ.0) THEN
               gfex2=gfex2-m*Lg(i,j,k)%u(iu)*Lg(i,j,k)%th*Lg(i,j,k)%ds*area
               GFEX=GFEX+ABS(G0) !first time through, just sum up exit mass flow
               Tmean_ex = Tmean_ex + LG(I1,J,K)%T*abs(g0)
               Tarea_mean = Tarea_mean + LG(I1,J,K)%T*area
               GOTO 120
            ENDIF
            N_EX=IBCELL(I-M,J,K)/10 !number of exit is in second digit of ibcell
            Lg(i,j,k)%U(iu)=-m*Gfin*Pull(n_ex,1)/(Lg(i,j,k)%th*Lg(i,j,k)%ds*area_exit(n_ex))
            !Lg(i,j,k)%U(iu)=-m*solids_in*Pull(n_ex,1)
            !&                /(Lg(i,j,k)%ds*area_exit(n_ex))
            CALL BOUND(LG(I,J,K)%U(IU),IU,1)
            LG(I0,J,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 120
110      CONTINUE
         IF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J-2,K)%U(IU)
         ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J+2,K)%U(IU)
         ELSEIF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K-2)%U(IU)
         ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K+2)%U(IU)
         ENDIF
120   CONTINUE
!----------------------------------------------------------------------
!     y-direction exits
!----------------------------------------------------------------------
      IU=2 !y-direction
      DO 140 J=3,NPM1,2
      DO 140 K=2,LP,2
      DO 140 I=2,MP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3.AND.IBC0.NE.4) GOTO 140
         DO 130 M=-1,1,2
            IF (IBCELL(I,J+M,K).GE.1) GOTO 130
            J0=J-M
            IF (IBC0.EQ.4) THEN !surface
               !Looks like attempt to enforce free slip at surface
               !but it should apply to all points in both x and y directions
               !Lottes, 5/31/05 @@@@
               J1=J+M
               LG(I,J,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J0,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J0,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J,K)%U(3)=0
               LG(I,J0,K)%U(3)=0
               GOTO 140
            ENDIF
            J1=J+2*M
            IF (IBCELL(I,J+3*M,K).LE.0) THEN
               J2=J+4*M
            ELSE
               J2=J1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I,J1,K)%TH*LG(I,J1,K)%DS*LG(I,J1,K)%U(IU)*AREA
            IF (LG(I,J1,K)%U(IU)*M.GT.SMALL) G0=0
            IF (MEX.EQ.0) THEN
               gfex2=gfex2-m*Lg(i,j,k)%u(iu)*Lg(i,j,k)%th*Lg(i,j,k)%ds*area
               GFEX=GFEX+ABS(G0) !first time through, just sum up exit mass flow
               Tmean_ex = Tmean_ex + LG(I,J1,K)%T*abs(g0)
               Tarea_mean = Tarea_mean + LG(I,J1,K)%T*area
               GOTO 140
            ENDIF
            N_EX=IBCELL(I,J-M,K)/10
            !Lg(i,j,k)%U(iu)=-m*Gfin*Pull(n_ex,1)
            !&                 /(Lg(i,j,k)%th*Lg(i,j,k)%ds*area_exit(n_ex))
            Lg(i,j,k)%U(iu)=-m*solids_in*Pull(n_ex,1)/(Lg(i,j,k)%ds*area_exit(n_ex))
            LG(I,J0,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 140
130      CONTINUE
         IF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K-2)%U(IU)
         ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K+2)%U(IU)
         ELSEIF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I-2,J,K)%U(IU)
         ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I+2,J,K)%U(IU)
         ENDIF
140   CONTINUE
!----------------------------------------------------------------------
!     z-direction exits
!----------------------------------------------------------------------
      !IF (LP.LT.6) GOTO 200 !for old 2d flow case
      IU=3 !z-directtion
      DO 160 K=3,LPM1,2
      DO 160 I=2,MP,2
      DO 160 J=2,NP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3) GOTO 160
         DO 150 M=-1,1,2
            IF (IBCELL(I,J,K+M).GE.1) GOTO 150
            K0=K-M
            K1=K+2*M
            IF (IBCELL(I,J,K+3*M).LE.0) THEN
               K2=K+4*M
            ELSE
               K2=K1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I,J,K1)%TH*LG(I,J,K1)%DS*LG(I,J,K1)%U(IU)*AREA
            IF (LG(I,J,K1)%U(IU)*M.GT.SMALL) G0=0
            IF (MEX.EQ.0) THEN
               gfex2=gfex2-m*Lg(i,j,k)%u(iu)*Lg(i,j,k)%th*Lg(i,j,k)%ds*area
               GFEX=GFEX+ABS(G0) !first time through, just sum up exit mass flow
               Tmean_ex = Tmean_ex + LG(I,J,K1)%T*abs(g0)
               Tarea_mean = Tarea_mean + LG(I,J,K1)%T*area
               GOTO 160
            ENDIF
            N_EX=IBCELL(I,J,K-M)/10
            !Lg(i,j,k)%U(iu)=-m*Gfin*Pull(n_ex,1)
            !&               /(Lg(i,j,k)%th*Lg(i,j,k)%ds*area_exit(n_ex))
            Lg(i,j,k)%U(iu)=-m*solids_in*Pull(n_ex,1)/(Lg(i,j,k)%ds*area_exit(n_ex))
            LG(I,J,K0)%U(IU)=LG(I,J,K)%U(IU)
150      CONTINUE     
         IF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I-2,J,K)%U(IU)
         ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I+2,J,K)%U(IU)
         ELSEIF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J-2,K)%U(IU)
         ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J+2,K)%U(IU)
         ENDIF
160   CONTINUE
!----------------------------------------------------------------------
200   continue
      !IF (MEX.EQ.0) GFEX=GFEX*GMFR !left from nondimensional version
      if (mex==0) then
         Tarea_mean=Tarea_mean/area_exit_tot
         if (gfex<small) then
            Tmean_ex=Tarea_mean
         else
            Tmean_ex=Tmean_ex/gfex
         endif
      endif
      RETURN
      END

!======================================================================
!======================================================================
!======================================================================
!     Extrapolation of exit velocity
!     Routine used in this form pre-Feb, 2005
!     Recirculation zones near exits may cause inflow at an exit
!     This routine is used if i_out_bc_pull .ne. 1 
!======================================================================
      SUBROUTINE EXTRV_free_flow(MEX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!----------------------------------------------------------------------
!     IBCELL: node
!        3: Exit 
!     IU=1: axial direction, 2: y direction, 3: z (verticle) direction 
!----------------------------------------------------------------------
      IF (MEX.EQ.0) then
         GFEX=ZERO
         Tmean_ex=zero
      endif
                
      IU=1 !x-direction
      DO 120 I=3,MPM1,2
      DO 120 J=2,NP,2
      DO 120 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         !IF (IBC0.NE.3) GOTO 120
         if (ibc0 .ne. 3) cycle ! if not exit, Lottes
         DO 110 M=-1,1,2
            IF (IBCELL(I+M,J,K).GE.1) GOTO 110 !wall in m-direction
            I0=I-M
            I1=I+2*M
            IF (IBCELL(I+3*M,J,K).LE.0) THEN
               I2=I+4*M
            ELSE
               I2=I1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I1,J,K)%TH*LG(I1,J,K)%DS*LG(I1,J,K)%U(IU)*AREA !exit mass flow
            IF (LG(I1,J,K)%U(IU)*M.GT.SMALL) G0=ZERO !coming in exit
            IF (MEX.EQ.0) THEN
               Tmean_ex = Tmean_ex + LG(I1,J,K)%T*abs(g0)
               GFEX=GFEX+ABS(G0) !first time through, just sum up exit mass flow
               GOTO 120
            ENDIF
            N_EX=IBCELL(I-M,J,K)/10 !number of exit is in second digit of ibcell
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU)*M.LT.SMALL) THEN
               LG(I,J,K)%U(IU)=-M*1.0d-4 !@@@ If nothing going out, U very small outflow
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            CALL BOUND(LG(I,J,K)%U(IU),IU,1)
            LG(I0,J,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 120
110      CONTINUE
         IF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J-2,K)%U(IU)
         ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J+2,K)%U(IU)
         ELSEIF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K-2)%U(IU)
         ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K+2)%U(IU)
         ENDIF
120   CONTINUE
!----------------------------------------------------------------------
      IU=2 !y-direction
      DO 140 J=3,NPM1,2
      DO 140 K=2,LP,2
      DO 140 I=2,MP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3.AND.IBC0.NE.4) GOTO 140
         DO 130 M=-1,1,2
            IF (IBCELL(I,J+M,K).GE.1) GOTO 130
            J0=J-M
            IF (IBC0.EQ.4) THEN !surface
               !Looks like attempt to enforce free slip at surface
               !but it should apply to all points in both x and y directions
               !Lottes, 5/31/05
               J1=J+M
               LG(I,J,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J0,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J0,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J,K)%U(3)=0
               LG(I,J0,K)%U(3)=0
               GOTO 140
            ENDIF
            J1=J+2*M
            IF (IBCELL(I,J+3*M,K).LE.0) THEN
               J2=J+4*M
            ELSE
               J2=J1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I,J1,K)%TH*LG(I,J1,K)%DS*LG(I,J1,K)%U(IU)*AREA
            IF (LG(I,J1,K)%U(IU)*M.GT.SMALL) G0=0
            IF (MEX.EQ.0) THEN
               Tmean_ex = Tmean_ex + LG(I,J1,K)%T*abs(g0)
               GFEX=GFEX+ABS(G0) !first time through, just sum up exit mass flow
               GOTO 140
            ENDIF
            N_EX=IBCELL(I,J-M,K)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU)*M.LT.SMALL) THEN
               LG(I,J,K)%U(IU)=-M*1.0d-4 !@@@
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            LG(I,J0,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 140
130      CONTINUE
         IF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K-2)%U(IU)
         ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K+2)%U(IU)
         ELSEIF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I-2,J,K)%U(IU)
         ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I+2,J,K)%U(IU)
         ENDIF
140   CONTINUE
!----------------------------------------------------------------------
      !IF (LP.LT.6) GOTO 200
      IU=3 !z-directtion
      DO 160 K=3,LPM1,2
      DO 160 I=2,MP,2
      DO 160 J=2,NP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3) GOTO 160
         DO 150 M=-1,1,2
            IF (IBCELL(I,J,K+M).GE.1) GOTO 150
            K0=K-M
            K1=K+2*M
            IF (IBCELL(I,J,K+3*M).LE.0) THEN
               K2=K+4*M
            ELSE
               K2=K1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I,J,K1)%TH*LG(I,J,K1)%DS*LG(I,J,K1)%U(IU)*AREA
            IF (LG(I,J,K1)%U(IU)*M.GT.SMALL) G0=0
            IF (MEX.EQ.0) THEN
               Tmean_ex = Tmean_ex + LG(I,J,K1)%T*abs(g0)
               GFEX=GFEX+ABS(G0) !first time through, just sum up exit mass flow
               GOTO 160
            ENDIF
            N_EX=IBCELL(I,J,K-M)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU)*M.LT.SMALL) THEN
               LG(I,J,K)%U(IU)=-M*1.0d-4 !@@@
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            LG(I,J,K0)%U(IU)=LG(I,J,K)%U(IU)
150      CONTINUE     
         IF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I-2,J,K)%U(IU)
         ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I+2,J,K)%U(IU)
         ELSEIF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J-2,K)%U(IU)
         ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J+2,K)%U(IU)
         ENDIF
160   CONTINUE
!----------------------------------------------------------------------
200   continue
      !IF (MEX.EQ.0) GFEX=GFEX*GMFR !left from nondimensional version
      Tmean_ex=Tmean_ex/gfex
      RETURN
      END

!======================================================================
!======================================================================
!======================================================================
!     Extrapolation indices 
!======================================================================
      SUBROUTINE EXT0(I,J,K,I1,J1,K1,I2,J2,K2,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3)
!----------------------------------------------------------------------
      DO IX=1,3
      DO II=1,3
         NX(II)=0
      ENDDO
      !     NX=0
      NX(IX)=1
      DO M=-2,2,4
         I1=I+M*NX(1)
         I1=MIN(MP,I1)
         I1=MAX(2,I1)
         J1=J+M*NX(2)
         J1=MIN(NP,J1)
         J1=MAX(2,J1)
         K1=K+M*NX(3)
         K1=MIN(LP,K1)
         K1=MAX(2,K1)
         IF (IBCELL(I1,J1,K1).LE.0) THEN
            I2=I1+M*NX(1)
            I2=MIN(MP,I2)
            I2=MAX(2,I2)
            J2=J1+M*NX(2)
            J2=MIN(NP,J2)
            J2=MAX(2,J2)
            K2=K1+M*NX(3)
            K2=MIN(LP,K2)
            K2=MAX(2,K2)
            IBC2=MOD(IBCELL(I2,J2,K2),10)
            IF (IBC2.EQ.1) THEN
               I2=I1
               J2=J1
               K2=K1
            ENDIF
            RETURN
         ENDIF
      ENDDO
      ENDDO
      RETURN
      END
!======================================================================
!======================================================================
!======================================================================
!     Extrapolation indices 
!======================================================================
      SUBROUTINE EXT1(I,J,K,I1,J1,K1,I2,J2,K2,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3)
!----------------------------------------------------------------------
      DO IX=1,3
         DO II=1,3
          NX(II)=0
         ENDDO
         !      NX=0
      NX(IX)=1
      DO M=-2,2,4
         I1=I+M*NX(1)
         I1=MIN(MP,I1)
         I1=MAX(2,I1)
         J1=J+M*NX(2)
         J1=MIN(NP,J1)
         J1=MAX(2,J1)
         K1=K+M*NX(3)
         K1=MIN(LP,K1)
         K1=MAX(2,K1)
         IBC1=MOD(IBCELL(I1,J1,K1),10)
         IF (IBC1.NE.1.AND.IBC1.NE.4) THEN
            I2=I1+M*NX(1)
            I2=MIN(MP,I2)
            I2=MAX(2,I2)
            J2=J1+M*NX(2)
            J2=MIN(NP,J2)
            J2=MAX(2,J2)
            K2=K1+M*NX(3)
            K2=MIN(LP,K2)
            K2=MAX(2,K2)
            IBC2=MOD(IBCELL(I2,J2,K2),10)
            IF (IBC2.EQ.1.OR.IBC2.EQ.4) THEN
               I2=I1
               J2=J1
               K2=K1
            ENDIF
            RETURN
         ENDIF
      ENDDO
      ENDDO
      RETURN
      END

!======================================================================
!     Routine below not used.
!======================================================================
!======================================================================
!======================================================================
!     Extrapolation of exit velocity 
!     Changed by B. Golchert winter 2005 to force outflow at exits
!     as a kind of boundary condition
!     Not used, Lottes, May 05
!======================================================================
      SUBROUTINE EXTRV_Brian_Golchert(MEX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      if (i_out_bc_pull.ne.1) then
         call EXTRV_free_flow(MEX)
         return
      endif
!----------------------------------------------------------------------
!     IBCELL: node
!        3: Exit 
!     IU=1: axial direction 
!----------------------------------------------------------------------
      IF (MEX.EQ.0) GFEX=ZERO
      IU=1
      DO 120 I=3,MPM1,2
      DO 120 J=2,NP,2
      DO 120 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3) GOTO 120
            !cbg         DO 110 M=-1,1,2   ! 9 Feb 2005
            IF (IBCELL(I+1,J,K).GE.1) GOTO 110 !NEGATIVE X DIRECTION EXITS
            I0=I+1
            I1=I+2
            IF (IBCELL(I+3,J,K).LE.0) THEN
               I2=I+4
            ELSE
               I2=I1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
           IF(LG(I1,J,K)%U(IU).GT.ZERO) THEN
                        GG=-LG(I1,J,K)%U(IU)
                        LG(I1,J,K)%U(IU)=GG
                        LG(I1+2,J,K)%U(IU)=GG
            ENDIF
            G0=LG(I1,J,K)%TH*LG(I1,J,K)%DS*LG(I1,J,K)%U(IU)*AREA
            IF (LG(I1,J,K)%U(IU).GT.SMALL) G0=ZERO
            IF (MEX.EQ.0) THEN
               GFEX=GFEX+ABS(G0)
               GOTO 120
            ENDIF
            N_EX=IBCELL(I+1,J,K)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU).LT.SMALL) THEN
               LG(I,J,K)%U(IU)=+1.0d-2 !Lottes 4/16/05: hard coded exit velocity, not good!@
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            CALL BOUND(LG(I,J,K)%U(IU),IU,1)
            LG(I0,J,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 120
110      CONTINUE
            !cbg          DO 110 M=-1,1,2
            IF (IBCELL(I-1,J,K).GE.1) GOTO 111 ! POSITIVE X DIRECTION EXITS
            I0=I+1
            I1=I-2
            IF (IBCELL(I-3,J,K).LE.0) THEN
               I2=I-4
            ELSE
               I2=I1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            IF(LG(I1,J,K)%U(IU).LT.ZERO) THEN
                        GG=-LG(I1,J,K)%U(IU)
                        LG(I1,J,K)%U(IU)=GG
                        LG(I1-2,J,K)%U(IU)=GG
            ENDIF
            G0=LG(I1,J,K)%TH*LG(I1,J,K)%DS*LG(I1,J,K)%U(IU)*AREA
            IF (LG(I1,J,K)%U(IU)*(-1.0D+0).GT.SMALL) G0=ZERO
            IF (MEX.EQ.0) THEN
               GFEX=GFEX+ABS(G0)
               GOTO 120
            ENDIF
            N_EX=IBCELL(I+1,J,K)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU)*(-1.0D+0).LT.SMALL) THEN
               LG(I,J,K)%U(IU)=1.0d-2 !Lottes 4/16/05: hard coded exit velocity, not good!@
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            CALL BOUND(LG(I,J,K)%U(IU),IU,1)
            LG(I0,J,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 120
111      CONTINUE
         IF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J-2,K)%U(IU)
         ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J+2,K)%U(IU)
         ELSEIF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K-2)%U(IU)
         ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K+2)%U(IU)
         ENDIF
120   CONTINUE
!----------------------------------------------------------------------
      IU=2
      DO 140 J=3,NPM1,2
      DO 140 K=2,LP,2
      DO 140 I=2,MP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3.AND.IBC0.NE.4) GOTO 140
            !CBG         DO 130 M=-1,1,2  !9 FEB 05
            IF (IBCELL(I,J-1,K).GE.1) GOTO 130
            J0=J+1
            IF (IBC0.EQ.4) THEN
               J1=J-1
               LG(I,J,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J0,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J0,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J,K)%U(3)=0
               LG(I,J0,K)%U(3)=0
               GOTO 140
            ENDIF
            J1=J-2
            IF (IBCELL(I,J-3,K).LE.0) THEN
               J2=J-4
            ELSE
               J2=J1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
         IF(LG(I,J1,K)%U(IU).LT.ZERO) THEN
              VEL=-LG(I,J1,K)%U(IU)
              LG(I,J1,K)%U(IU)=VEL
              LG(I,J1-2,K)%U(IU)=VEL
          ENDIF
            G0=LG(I,J1,K)%TH*LG(I,J1,K)%DS*LG(I,J1,K)%U(IU)*AREA
            IF (LG(I,J1,K)%U(IU)*(-1.0D+0).GT.SMALL) G0=0
            IF (MEX.EQ.0) THEN
               GFEX=GFEX+ABS(G0)
               GOTO 140
            ENDIF
            N_EX=IBCELL(I,J+1,K)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU).LT.SMALL) THEN
               LG(I,J,K)%U(IU)=-1.0D-2 !Lottes 4/16/05: hard coded exit velocity, not good!@
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            LG(I,J0,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 140
130      CONTINUE
            !CBG          DO 130 M=-1,1,2
            IF (IBCELL(I,J+1,K).GE.1) GOTO 131
            J0=J+1
            IF (IBC0.EQ.4) THEN
               J1=J+1
               LG(I,J,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J0,K)%U(1)=LG(I,J1,K)%U(1)
               LG(I,J,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J0,K)%U(2)=LG(I,J1,K)%U(2)
               LG(I,J,K)%U(3)=0
               LG(I,J0,K)%U(3)=0
               GOTO 140
            ENDIF
            J1=J+2
            IF (IBCELL(I,J+3,K).LE.0) THEN
               J2=J+4
            ELSE
               J2=J1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
          IF(LG(I,J1,K)%U(IU).GT.ZERO) THEN
              VEL=-LG(I,J1,K)%U(IU)
              LG(I,J1,K)%U(IU)=VEL
              LG(I,J1+2,K)%U(IU)=VEL           
          ENDIF
            G0=LG(I,J1,K)%TH*LG(I,J1,K)%DS*LG(I,J1,K)%U(IU)*AREA
            !CBG        IF (LG(I,J1,K)%U(IU)*(-1.0D+0).GT.SMALL) G0=ZERO
            IF (MEX.EQ.0) THEN
               GFEX=GFEX+ABS(G0)
               GOTO 140
            ENDIF
            N_EX=IBCELL(I,J+1,K)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (LG(I,J,K)%U(IU).LT.SMALL) THEN
               LG(I,J,K)%U(IU)=1.D-2 !Lottes 4/16/05: hard coded exit velocity, not good!@
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            LG(I,J0,K)%U(IU)=LG(I,J,K)%U(IU)
            GOTO 140
131      CONTINUE
         IF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K-2)%U(IU)
         ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J,K+2)%U(IU)
         ELSEIF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I-2,J,K)%U(IU)
         ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I+2,J,K)%U(IU)
         ENDIF
140   CONTINUE
!----------------------------------------------------------------------
      IF (LP.LT.6) GOTO 200
      IU=3
      DO 160 K=3,LPM1,2
      DO 160 I=2,MP,2
      DO 160 J=2,NP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.3) GOTO 160
         DO 150 M=-1,1,2
            IF (IBCELL(I,J,K+M).GE.1) GOTO 150
            K0=K-M
            K1=K+2*M
            IF (IBCELL(I,J,K+3*M).LE.0) THEN
               K2=K+4*M
            ELSE
               K2=K1
            ENDIF
            AREA=AREA_C(I,J,K,IU)
            G0=LG(I,J,K1)%TH*LG(I,J,K1)%DS*LG(I,J,K1)%U(IU)*AREA
            IF (LG(I,J,K1)%U(IU)*M.GT.SMALL) G0=0
            IF (MEX.EQ.0) THEN
               GFEX=GFEX+ABS(G0)
               GOTO 160
            ENDIF
            N_EX=IBCELL(I,J,K-M)/10
            LG(I,J,K)%U(IU)=G0/(LG(I,J,K)%TH*LG(I,J,K)%DS*AREA)
            IF (-LG(I,J,K)%U(IU)*M.LT.SMALL) THEN
               LG(I,J,K)%U(IU)=-M
            ELSE
               LG(I,J,K)%U(IU)=LG(I,J,K)%U(IU)*PULL(N_EX,3)
            ENDIF
            LG(I,J,K0)%U(IU)=LG(I,J,K)%U(IU)
150      CONTINUE     
         IF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I-2,J,K)%U(IU)
         ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I+2,J,K)%U(IU)
         ELSEIF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J-2,K)%U(IU)
         ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
            LG(I,J,K)%U(IU)=LG(I,J+2,K)%U(IU)
         ENDIF
160   CONTINUE
!----------------------------------------------------------------------
200   continue
      !IF (MEX.EQ.0) GFEX=GFEX*GMFR !left from nondimensional version
      RETURN
      END
