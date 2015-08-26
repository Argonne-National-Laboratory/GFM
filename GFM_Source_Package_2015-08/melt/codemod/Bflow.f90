! BFLOW.F90
!----------------------------------------------------------------------
!     solves for bubble property variables (Lottes: 4/11/05)
!       DN    number density
!       U     velocity components
!       T     temperature
!     Rev: 4/02
!----------------------------------------------------------------------
      SUBROUTINE BFLOW(NEL_P)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 SMXD(6,NBS0),AVED(6,NBS0)
!----------------------------------------------------------------------
!     bubble properties
!        temperature, pressure, density, and size
!----------------------------------------------------------------------
      CALL GBFP
!----------------------------------------------------------------------
!     Bubble Number Density
!----------------------------------------------------------------------
      KD=1
      SGM=0
      NEL=13
      MI1=MP_B
      MI3=MP_E
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2
      MID1=MI1/2
      MID3=MI3/2
      NJD1=NJ1/2
      NJD3=NJ3/2
      LKD1=LK1/2
      LKD3=LK3/2
      LD=0
      DO L=1,NBS0
         KL=L
         CALL BFL_1
         CALL SOURCEB(MI1,MI3,NJ1,NJ3,LK1,LK3)
         CALL BFLO2C(MI1,MI3,NJ1,NJ3,LK1,LK3)
         CALL ADLBL3(FZ,KD,MID1,MID3,NJD1,NJD3,LKD1,LKD3)
         G2=one/WGI_B(KL)
         SMXD(1,KL)=RES(NEL,1)*G2
         AVED(1,KL)=RES(NEL,2)*G2
      g11=fz(21,22,13)*as(22,22,13,1,1)
      g12=fz(23,22,13)*as(22,22,13,1,2)
      g21=fz(22,21,13)*as(22,22,13,2,1)
      g22=fz(22,23,13)*as(22,22,13,2,2)
      g31=fz(22,22,12)*as(22,22,13,3,1)
      g32=fz(22,22,14)*as(22,22,13,3,2)
      g1=g11+g12+g21+g22+g31+g32
      g0=fz(22,22,13)*ap(22,22,13)
      GB_DN0=0
      IX=0
      JX=0
      KX=0
         CALL BFL_2
      ENDDO
!---
!        bubble volume fraction
!---
      G1=0
      DO 220 I=MP_B,MP_E,2
      ID2=I/2
      DO 220 J=NJ1,NJ3,2
      JD2=J/2
      DO 220 K=2,LKZ2,2
         IF (IBCELL(I,J,K).GE.1) CYCLE
         KD2=K/2
         TH_P0=ZERO
         DO L=1,NBS0
            RB3_0=GB4(ID2,JD2,KD2,L)%R**3
            DN0=GB4(ID2,JD2,KD2,L)%DN
            TH_P0=TH_P0+RB3_0*DN0
            G1=G1+GB4(ID2,JD2,KD2,L)%GR*VOL_C(I,J,K)
         END DO
         IF (K.LT.LP-2.AND.TH_P0.GT.4.0D-2) THEN
            G0=4.0D-2/TH_P0
            !csl         DO L=1,NBS0
            !csl            GB4(ID2,JD2,KD2,L)%DN=GB4(ID2,JD2,KD2,L)%DN*G0
            !csl         ENDDO
            TH_P0=4.0D-2
         ENDIF
         IF (K.GE.LP-2.AND.TH_P0.GT.2.0D+0) THEN
            G0=2.0D+0/TH_P0
            DO L=1,NBS0
               GB4(ID2,JD2,KD2,L)%DN=GB4(ID2,JD2,KD2,L)%DN*G0
            ENDDO
            TH_P0=2.0D+0
         ENDIF
         GB3(ID2,JD2,KD2)%TH=TH_P0
220   CONTINUE
      !G1=G1*GMFR
!----------------------------------------------------------------------
!     Bubble Velocity and temperature
!        U: (LD=1,2,3,NEL=14,15,16)
!     KL: bubble size index in COMMON block
!----------------------------------------------------------------------
300   DO 400 L=1,NBS0
      KL=L
      DO 400 LD=1,3
         IF (LD.EQ.3.AND.LZ.LT.3) CYCLE
         NEL=LD+13
         CALL BFL_1
         CALL SOURCEB(MI1,MI3,NJ1,NJ3,LK1,LK3)
         CALL BFLO3C(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
         CALL ADLBL3(FZ,KD,MID1,MID3,NJD1,NJD3,LKD1,LKD3)
         CALL BFL_2
         !CSL         SMXD(LD+1,KL)=RES(NEL,1)
         !CSL         AVED(LD+1,KL)=RES(NEL,2)
400   CONTINUE
      CALL EXTR(4)
      CALL BFLOW2(NEL_P)
      RETURN


!======================================================================
      CONTAINS
      SUBROUTINE BFL_1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 100 ID2=1,MZ
      DO 100 JD2=1,NZ
      DO 100 KD2=1,LZ
         IF (LD.EQ.0) THEN
            FZ(ID2,JD2,KD2)=GB4(ID2,JD2,KD2,KL)%DN
         ELSEIF (LD.EQ.4) THEN
            FZ(ID2,JD2,KD2)=GB4(ID2,JD2,KD2,KL)%T
         ELSE
            FZ(ID2,JD2,KD2)=GB4(ID2,JD2,KD2,KL)%U(LD)
         ENDIF
100   CONTINUE
      END SUBROUTINE BFL_1

!======================================================================
      SUBROUTINE BFL_2
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 200 I=MP_B,MP_E1,2
      ID2=I/2
      DO 200 J=NJ1,NJ3,2
      JD2=J/2
      DO 200 K=LK1,LK3,2
         IF (IBCELL(I,J,K).GE.1) CYCLE
         KD2=K/2
         G0=FZ(ID2,JD2,KD2)
         IF (LD.EQ.0) THEN
            IF (G0.GT.GB_DN0) THEN
               GB_DN0=G0
               IX=I
               JX=J
               KX=K
            ENDIF
            IF (G0.LT.0) G0=0
            GB4(ID2,JD2,KD2,KL)%DN=G0
         ELSEIF (LD.EQ.4) THEN
            GB4(ID2,JD2,KD2,KL)%T=LG(I,J,K)%T
         ELSE
            DN0=GB4(ID2,JD2,KD2,KL)%DN
            IF (DN0.LE.DN_MN) G0=LG(I,J,K)%U(LD)
            IF (K.GE.LP-2.AND.LD.EQ.3.AND.G0.GT.0) G0=0
            IF (IBCELL(I+1,J,K).EQ.103.AND.LD.EQ.1.AND.G0.LE.0) THEN
               GB4(ID2,JD2,KD2,KL)%U(LD)=0
               GB4(ID2,JD2,KD2,KL)%DN=0
            ELSE
               GB4(ID2,JD2,KD2,KL)%U(LD)=G0
            ENDIF
         ENDIF
200   CONTINUE
      END SUBROUTINE BFL_2

!-------------------------------------------------------------------
      SUBROUTINE GBFP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GB_Y(2)
      DO 120 I=2,MP,2
      ID2=I/2
      DO 120 J=2,NP,2
      JD2=J/2
      DO 120 K=2,LP,2
         IF (IBCELL(I,J,K).GT.0) CYCLE
         KD2=K/2
         TT=LG(I,J,K)%T
         PP1=(LG(I,J,K)%P+1)*PG0
         DO L=1,NBS0
            GB4(ID2,JD2,KD2,L)%T=LG(I,J,K)%T
            !R1=GB4(ID2,JD2,KD2,L)%R*RLM0
            R1=GB4(ID2,JD2,KD2,L)%R
            PP2=PP1+2*GB%SG/R1
            GB4(ID2,JD2,KD2,L)%P=PP2/PG0-1
            GB_Y(1:2)=GB4(ID2,JD2,KD2,L)%Y(1:2)
            WT0=44*GB_Y(1)+18*GB_Y(2)
            GB4(ID2,JD2,KD2,L)%WT=WT0
            G0=PP2*WT0/RU/TT
            GB4(ID2,JD2,KD2,L)%DS=G0
            GB4(ID2,JD2,KD2,L)%R=RG_B(L)*(GB%DS/G0)**THIRD
         ENDDO
120   CONTINUE
      END SUBROUTINE GBFP
      END


!======================================================================
!     Calculation of AP and AS for BUBBLE Number Density Equation
!----------------------------------------------------------------------
      SUBROUTINE BFLO2C(MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AREA(3)
      DO 150 I=MI1,MI3,2
      ID2=I/2
      DO 150 J=NJ1,NJ3,2
      JD2=J/2
      DO 150 K=LK1,LK3,2
         KD2=K/2
         AS(ID2,JD2,KD2,1:3,1:2)=ZERO
         DFS=ZERO
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.GE.1) THEN
            AP(ID2,JD2,KD2)=ONE
            GOTO 150
         ENDIF
         AP(ID2,JD2,KD2)=ZERO
         DN0=GB4(ID2,JD2,KD2,KL)%DN
         ASM=ZERO
         AREA(1)=AREA_C(I,J,K,1)
         AREA(2)=AREA_C(I,J,K,2)
         AREA(3)=AREA_C(I,J,K,3)
         DO 130 IU=1,3
            IF (IU.EQ.2.AND.NP.LE.6) GOTO 130
            IF (IU.EQ.3.AND.LP.LE.6) GOTO 130
            G1=ZERO
            DU0=GB4(ID2,JD2,KD2,KL)%U(IU)
            DO M=-1,1,2
               M1=(M+3)/2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               DU1=GB4(ID21,JD21,KD21,KL)%U(IU)
               DN1=GB4(ID21,JD21,KD21,KL)%DN
               IBC1=MOD(IBCELL(I1,J1,K1),10)
               IF (IBC1.NE.1) THEN
                  IF (IBC1.EQ.2) THEN
                     DUK=DU1
                  ELSE
                     IF (DN1.LT.SMALL) DU1=DU0
                     IF (DN0.LT.SMALL) DU0=DU1
                     CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                     DUK=F1*DU1+F2*DU0
                  ENDIF
                  !IF (IBC1.EQ.2.OR.IBC1.EQ.3) THEN
                  !   DIFF=ZERO
                  !ELSE
                  !   !DIFF=DFF0(I1,J1,K1,KL,IU) !dff0 returns zero
                  !   DIFF=zero
                  !ENDIF
                  FL=DUK*AREA(IU)
                  FL1=-FL*M
                  !czTEMP                  FL2=LG(I1,J1,K1)%U(IU)*AREA(IU)
                  !FL2=GB4(ID2,JD2,KD2,1)%U(IU)*AREA(IU)
                  !AS(ID2,JD2,KD2,IU,M1)=DA(DIFF,FL2)+MAX(ZERO,FL1) !da returns diff, and diff is zero
                  AS(ID2,JD2,KD2,IU,M1)=MAX(ZERO,FL1)
               ELSE
                  FL=DU1*AREA(IU)
                  FL1=-FL*M
                  AS(ID2,JD2,KD2,IU,M1)=ZERO
               ENDIF
               DFS=DFS-FL1
               G1=G1+AS(ID2,JD2,KD2,IU,M1)
               ASM=ASM+AS(ID2,JD2,KD2,IU,M1)
            ENDDO
            AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+G1
130      CONTINUE
         VOL=VOL_C(I,J,K)
         AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+DFS-SFP(ID2,JD2,KD2)*VOL
         BSTMP=BS(ID2,JD2,KD2)*VOL
         IF (.NOT.STEADY) THEN
            IF (KL.LE.NBS0) THEN
               G0=GBT(ID2,JD2,KD2,KL)%DN
            ENDIF
            !BSTMP=BSTMP+G0*TRN/DTM*VOL
            BSTMP=BSTMP+G0/DTM*VOL
            !AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+TRN/DTM*VOL
            AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+vol/DTM
         ENDIF
!----------------------------------------------------------------------
!     Flood node Treatment
!----------------------------------------------------------------------
         APTMP=AP(ID2,JD2,KD2)
         IF (APTMP.LT.ASM/10) THEN
            IF (ASM.LE.SMALL) THEN
               AP(ID2,JD2,KD2)=1
               AS(ID2,JD2,KD2,1:3,1:2)=0
               BS(ID2,JD2,KD2)=0
               CYCLE
            ENDIF
            APTMP=ASM/10
         ENDIF
         AP(ID2,JD2,KD2)=APTMP/RF(NEL)
         BS(ID2,JD2,KD2)=BSTMP+RFC(NEL)*FZ(ID2,JD2,KD2)*AP(ID2,JD2,KD2)
150   CONTINUE
      RETURN
      END

!----------------------------------------------------------------------
!     Calculation of AP and AS for BUBBLE Momentum and Energy Equations
!----------------------------------------------------------------------
      SUBROUTINE BFLO3C(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AREA(3)
      DO 350 I=MI1,MI3,2
      ID2=I/2
      DO 350 J=NJ1,NJ3,2 
      JD2=J/2
      DO 350 K=LK1,LK3,2
         KD2=K/2
         IBC0=MOD(IBCELL(I,J,K),10)
!--- Non-calculation cells               
         AS(ID2,JD2,KD2,1:3,1:2)=ZERO
         DN0=GB4(ID2,JD2,KD2,KL)%DN
         IF (IBC0.GE.1) THEN
            AP(ID2,JD2,KD2)=ONE
            GOTO 350
         ENDIF
!---------------------------------------------------------------
         AP(ID2,JD2,KD2)=ZERO
         DFS=ZERO
         ASM=ZERO
         AREA(1)=AREA_C(I,J,K,1)
         AREA(2)=AREA_C(I,J,K,2)
         AREA(3)=AREA_C(I,J,K,3)
         DO 330 IU=1,3
            IF (IU.EQ.2.AND.NP.LE.6) GOTO 330
            IF (IU.EQ.3.AND.LP.LE.6) GOTO 330
            DU0=GB4(ID2,JD2,KD2,KL)%U(IU)
            DO M=-1,1,2
               M1=(M+3)/2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               DN1=GB4(ID21,JD21,KD21,KL)%DN
               DU1=GB4(ID21,JD21,KD21,KL)%U(IU)
               IBC1=MOD(IBCELL(I1,J1,K1),10)
               IF (IBC1.NE.1) THEN
                  IF (IBC1.EQ.2) THEN
                     DUK=DU1
                     DNK=DN1
                  ELSE
                     IF (DN1.LT.SMALL) DU1=DU0
                     IF (DN0.LT.SMALL) DU0=DU1
                     CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                     DUK=F1*DU1+F2*DU0
                     !CSL                  IF (ABS(DUK).LE.1.0D-10) DUK=ZERO
                     IF (M*DUK.GE.ZERO) THEN
                        DNK=DN0
                     ELSE
                        DNK=DN1
                     ENDIF
                  ENDIF
                  FL=DUK*DNK*AREA(IU)
                  FL1=-FL*M
                  AS(ID2,JD2,KD2,IU,M1)=MAX(ZERO,FL1)
                  IF (LD.EQ.4.AND.IBC1.LE.0) THEN
                     DIFF=0
                     !CZTEMP                     FL2=LG(I1,J1,K1)%U(IU)*AREA(IU)
                     FL2=GB4(ID2,JD2,KD2,1)%U(IU)*AREA(IU)
                     AS(ID2,JD2,KD2,IU,M1)=AS(ID2,JD2,KD2,IU,M1)+DA(DIFF,FL2)
                  ENDIF
               ELSE
                  FL1=ZERO
               ENDIF
               DFS=DFS-FL1
               AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+AS(ID2,JD2,KD2,IU,M1)
               ASM=ASM+AS(ID2,JD2,KD2,IU,M1)
            ENDDO
330      CONTINUE
         VOL=VOL_C(I,J,K)
!----------------------------------------------------------------------
!     Stagnation Point Treatment
!----------------------------------------------------------------------
         BSTMP=BS(ID2,JD2,KD2)*VOL
         APTMP=AP(ID2,JD2,KD2)-SFP(ID2,JD2,KD2)*VOL
         !cz       IF (.NOT.STEADY) THEN
         !cz            IF (LD.EQ.4) THEN
         !cz               G0=GBT(ID2,JD2,KD2,KL)%T
         !cz            ELSE
         !cz               G0=GBT(ID2,JD2,KD2,KL)%U(LD)
         !cz            ENDIF
         !cz            BSTMP=BSTMP+GBT(ID2,JD2,KD2,KL)%DN*G0*TRN/DTM*VOL
         !cz            APTMP=APTMP+GBT(ID2,JD2,KD2,KL)%DN*TRN/DTM*VOL
         !cz       ENDIF
         IF (ABS(APTMP).LT.1.0D-15) THEN
            AS(ID2,JD2,KD2,1:3,1:2)=ZERO
            AP(ID2,JD2,KD2)=1
            !CZ            BS(ID2,JD2,KD2)=0
            IF (LD.EQ.4) THEN
               BS(ID2,JD2,KD2)=LG(I,J,K)%T
               !CZ               BS(ID2,JD2,KD2)=T_MN
            ENDIF
         ELSE
            AP(ID2,JD2,KD2)=APTMP/RF(NEL)
            BS(ID2,JD2,KD2)=BSTMP+RFC(NEL)*FZ(ID2,JD2,KD2)*AP(ID2,JD2,KD2)
            !cz            IF (LD.EQ.4) THEN
            !cz               AP(ID2,JD2,KD2)=APTMP
            !cz               BS(ID2,JD2,KD2)=BSTMP
            !cz              IF(BSTMP.NE.ZERO.AND.BS(ID2,JD2,KD2).NE.T_MN*VOL) THEN
            !cz                  BS(ID2,JD2,KD2)=ZERO
            !cz             ENDIF
            !cz            ENDIF
         ENDIF 
350   CONTINUE
      RETURN
      END
