! FLX1D.F90
!     cross-sectional flow rates
!     rev:  5/01
!----------------------------------------------------------------------
      SUBROUTINE FLX1D(IFX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      GOTO(100,400,500),IFX
      RETURN
!----------------------------------------------------------------------
!     FLX: cross-sectional mass flow rate (kg/s)
!       1: total glass
!----------------------------------------------------------------------
100   PULL(1:NEX0,2)=0
      H_EX=0
      DO 140 I=3,MPM1,2
         FLX(I,1:3)=ZERO
         DO 120 J=NJY1,NJY2,2
         DO 120 K=LKZ1,LKZ2,2
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.EQ.1) GOTO 120
            AREA=AREA_C(I,J,K,1)
            G0=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(1)*AREA
            FLX(I,1)=FLX(I,1)+G0
            IF (G0.GE.ZERO) THEN
               FLX(I,2)=FLX(I,2)+G0
               IF (I.LT.MPM1.AND.IBCELL(I+2,J,K).EQ.103) THEN
                  N_EX=IBCELL(I+3,J,K)/10
                  PULL(N_EX,2)=PULL(N_EX,2)+G0
                  !H_EX=H_EX+G0*CL_G*(LG(I,J,K)%T-TM_0) !Lottes
                  H_EX=H_EX+G0*LG(I,J,K)%C*(LG(I,J,K)%T-TM_0) !use user input c
               ENDIF
            ELSE
               FLX(I,3)=FLX(I,3)-G0
               IF (I.GT.3.AND.IBCELL(I-2,J,K).EQ.113) THEN
                  N_EX=IBCELL(I-3,J,K)/10
                  PULL(N_EX,2)=PULL(N_EX,2)-G0
                  !H_EX=H_EX-G0*CL_G*(LG(I,J,K)%T-TM_0) !Lottes
                  H_EX=H_EX-G0*LG(I,J,K)%C*(LG(I,J,K)%T-TM_0) !use user input c
               ENDIF
            ENDIF
120      CONTINUE
         FLX(I,10)=FLX(I,1)
140   CONTINUE
      iwrite_flx=0 !Lottes debugging print May 05
      if (iwrite_flx==1) then
      
         open(97,file=casedir//'\flx'//runum//'m.out')
         write(97,*) "Forward Flow       Back Flow           Net Flow"
         do i=3,mpm1,2
            write(97,"(' ',I5,3E25.14)") I,flx(i,2),flx(i,3),flx(i,1)
         enddo
         close(97)
      endif
      DO 150 J=3,NPM1,2
      DO 150 I=2,MP,2
      DO 150 K=2,LP,2
         IBC0=IBCELL(I,J,K)
         IF (IBC0.EQ.203) THEN
            J2=J-2
            AREA=AREA_C(I,J2,K,2)
            G0=LG(I,J2,K)%TH*LG(I,J2,K)%DS*LG(I,J2,K)%U(2)*AREA
            IF (G0.LE.ZERO) GOTO 150
            N_EX=IBCELL(I,J+1,K)/10
            PULL(N_EX,2)=PULL(N_EX,2)+G0
            !H_EX=H_EX+G0*CL_G*(LG(I,J,K)%T-TM_0) !Lottes
            H_EX=H_EX+G0*LG(I,J,K)%C*(LG(I,J,K)%T-TM_0) !use user input c
         ELSEIF (IBC0.EQ.213) THEN
            J2=J+2
            AREA=AREA_C(I,J2,K,2)
            G0=LG(I,J2,K)%TH*LG(I,J2,K)%DS*LG(I,J2,K)%U(2)*AREA
            IF (G0.GE.ZERO) GOTO 150
            N_EX=IBCELL(I,J-1,K)/10
            PULL(N_EX,2)=PULL(N_EX,2)-G0
            !H_EX=H_EX-G0*CL_G*(LG(I,J,K)%T-TM_0) Lottes 5/7/05
            H_EX=H_EX-G0*LG(I,J,K)%C*(LG(I,J,K)%T-TM_0) !use user input c
         ENDIF
150   CONTINUE
      DO 160 K=3,LP-1,2
      DO 160 I=2,MP,2
      DO 160 J=2,NP,2
         IBC0=IBCELL(I,J,K)
         IF (IBC0.EQ.303) THEN
            K2=K-2
            AREA=AREA_C(I,J,K2,3)
            G0=LG(I,J,K2)%TH*LG(I,J,K2)%DS*LG(I,J,K2)%U(3)*AREA
            IF (G0.LE.ZERO) GOTO 160
            N_EX=IBCELL(I,J,K+1)/10
            PULL(N_EX,2)=PULL(N_EX,2)+G0
            !H_EX=H_EX+G0*CL_G*(LG(I,J,K)%T-TM_0) Lottes 5/6/05
            H_EX=H_EX+G0*LG(I,J,K)%C*(LG(I,J,K)%T-TM_0) !use user input c
         ELSEIF (IBC0.EQ.313) THEN
            K2=K+2
            AREA=AREA_C(I,J,K2,3)
            G0=LG(I,J,K2)%TH*LG(I,J,K2)%DS*LG(I,J,K2)%U(3)*AREA
            IF (G0.GE.ZERO) GOTO 160
            N_EX=IBCELL(I,J,K-1)/10
            PULL(N_EX,2)=PULL(N_EX,2)-G0
            !H_EX=H_EX-G0*CL_G*(LG(I,J,K)%T-TM_0) Lottes 5/6/05
            H_EX=H_EX-G0*LG(I,J,K)%C*(LG(I,J,K)%T-TM_0) !use user input c
         ENDIF
160   CONTINUE
      IF (NPHAS.LE.1) THEN
         GFIN=GFIN0
         GOTO 900
      ENDIF
!----------------------------------------------------------------------
!     GDMLT: total batch melt
!     GDCON: total condensation
!     MAR: total fluid volume change
!----------------------------------------------------------------------
200   GDMLT=ZERO
      GDCON=ZERO
      MAR=ZERO
      DO 220 I=4,MPM2,2
         FLX_BT=ZERO
         ID2=I/2
         DO 210 J=NJY1,NJY2,2
         JD2=J/2
         DO 210 K=LKZ1,LKZ2,2
            IF (IBCELL(I,J,K).GE.1) GOTO 210
            KD2=K/2
            VOL=VOL_C(I,J,K)
            IF (VOL.LE.ZERO) GOTO 210
            IBC1=MOD(IBCELL(I-2,J,K),10)
            IBC2=MOD(IBCELL(I+2,J,K),10)
            IF (IBC1.EQ.3.OR.IBC2.EQ.3) GOTO 210
            IBC1=MOD(IBCELL(I,J-2,K),10)
            IBC2=MOD(IBCELL(I,J+2,K),10)
            IF (NP.GE.6.AND.(IBC1.EQ.3.OR.IBC2.EQ.3)) GOTO 210
            IBC1=MOD(IBCELL(I,J,K-2),10)
            IBC2=MOD(IBCELL(I,J,K+2),10)
            IF (LP.GE.6.AND.(IBC1.EQ.3.OR.IBC2.EQ.3)) GOTO 210
            GDMLT=GDMLT+LG(I,J,K)%MR*VOL
            IF (NBS0.GE.1) THEN
               GDCON=GDCON+CON(ID2,JD2,KD2)*VOL
               GDMLT=GDMLT-CON(ID2,JD2,KD2)*VOL
            ENDIF
            IF (.NOT.STEADY) MAR=MAR+(LG(I,J,K)%TH*LG(I,J,K)%DS-LG0(I,J,K)%TH*LG0(I,J,K)%DS)*VOL
210      CONTINUE   
220   CONTINUE   
      !MAR=MAR*TRN/DTM
      MAR=MAR/DTM
      GFIN=GFIN0+GDMLT
      GOTO 900
!----------------------------------------------------------------------
!     energy flow rate
!----------------------------------------------------------------------
400   FLXH(1,6)=ZERO
      DO 440 I=3,MPM1,2
         FLXH(I,2)=ZERO
         FLXH(I,6)=FLXH(I-2,6)
         DO 420 J=NJY1,NJY2,2
         DO 420 K=LKZ1,LKZ2,2
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.EQ.1) GOTO 420
            AREA=AREA_C(I,J,K,1)
            G0=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(1)*AREA
            IF (G0.GE.ZERO) THEN
               FLXH(I,2)=FLXH(I,2)+G0*LG(I-1,J,K)%H
            ELSE
               FLXH(I,2)=FLXH(I,2)+G0*LG(I+1,J,K)%H
            ENDIF
            FLXH(I,6)=FLXH(I,6)+QLS(I/2,J/2,K/2)
420      CONTINUE
440   FLXH(I,1)=FLXH(I,2)+FLXH(I,3)+FLXH(I,6)
      RETURN
!----------------------------------------------------------------------
!     Subspecies flow rates
!     FLYM: cross-sectional mass flow rate (kg/s)
!       1,2: (+,-)
!----------------------------------------------------------------------
500   FLXM=ZERO
      DO 510 I=3,MPM1,2
      DO 510 J=4,NPM2,2
      DO 510 K=LKZ1,LKZ2,2
         IF (IBCELL(I,J,K).EQ.1) CYCLE
         AREA=AREA_C(I,J,K,1)
         G0=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(1)*AREA
         IF (G0.GE.ZERO) THEN
            ID=1
         ELSE
            ID=2
         ENDIF
         DO L=1,NMSP
            FLXM(I,L,ID)=FLXM(I,L,ID)+ABS(G0)*GFM(I+1,J,K,L)
         ENDDO
510   CONTINUE
      FLYM=ZERO
      DO J=3,NPM1,2
      DO 520 I=4,MPM2,2
      DO 520 K=LKZ1,LKZ2,2
         IF (IBCELL(I,J,K).EQ.1) CYCLE
         AREA=AREA_C(I,J,K,2)
         G0=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(2)*AREA
         IF (G0.GE.ZERO) THEN
            ID=1
         ELSE
            ID=2
         ENDIF
         DO L=1,NMSP
            FLYM(J,L,ID)=FLYM(J,L,ID)+ABS(G0)*GFM(I,J,K,L)
         ENDDO
520   CONTINUE
      ENDDO
      SMX_MS=0
      G1=0
      G2=0
      DO 540 J=2,NP,2
      DO 540 I=2,MP,2
      DO 540 K=2,LP,2
         IF (IBCELL(I,J,K).GT.0) CYCLE
         G3=VOL_C(I,J,K)
         G1=G1+G3
         G0=GFM(I,J,K,1)
         DO L=1,NMSP
            G0=G0+GFM(I,J,K,L)
         ENDDO
         G2=G2+ABS(G0-ONE)*G3
540   CONTINUE
      IF (G1.GT.SMALL) SMX_MS=G2/G1
      RETURN
      !CZ--------------
900   IF (GFIN.LE.SMALL.OR.I_RB.GE.MP) RETURN
      G0=1-FLX(I_CH,1)/GFIN
      IF (G0.LT.ZERO) G0=G0/10
      F_BUOY=F_BUOY+2.0D-5*G0
      IF (F_BUOY.LT.0) F_BUOY=0
      RETURN
      END


