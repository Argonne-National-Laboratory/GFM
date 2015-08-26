!---
!     X FLUXES IN +/- Y SWEEPS FOR EACH Z SLICE
!---
      SUBROUTINE QRTX(I0,J0,K0,I10,I1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A1(3),Q11(NWL),Q12(NWL)
      IDX=1
      I2=I1
      I2=MIN(I2,MP)
      CALL SANG2(IDX,I0,J0,K0,I2,J0,K0)
      DO 280 K1=4,LP-2,2  
      IF (K1.LT.K0.AND.ANG3(K1+1,2).GE.ANG0(3)) CYCLE
      IF (K1.GT.K0.AND.ANG3(K1-1,2).GE.ANG0(3)) EXIT
      KD2=K1/2
      DO 270 JPM=-2,2,4
         A11=0
         A12=0
         CALL SWEEP(2,JPM,JEND)
         DO 260 J1=J0,JEND-JPM,JPM
            IF (A11.GE.ANG0(2)) EXIT
            J12=J1+JPM/2
            A12=ANG2(J12,1)
            DANG=A12-A11
            IF (DANG.LE.1.0D-10) CYCLE
            A21=A11
            J1D2=J1/2
            IF (IQT1(J1D2,KD2)) GOTO 260
            IF (J1.EQ.J0) THEN
               Q11(1:NWL)=QTX1(J1D2,KD2,1:NWL)*HALF
               IF (JPM.GT.0) QTX1(J1D2,KD2,1:NWL)=0
            ELSE
               Q11(1:NWL)=QTX1(J1D2,KD2,1:NWL)
               QTX1(J1D2,KD2,1:NWL)=0
            ENDIF
            IF (J1.NE.J0.OR.JPM.GT.0) IQT1(J1D2,KD2)=.TRUE.
            Q12=Q11
            DO 240 J2=J1,JEND,JPM
               J2D2=J2/2
               IF (IBCELL(I1,J2,K1).GE.1) GOTO 230
               J2M=J2+JPM/2
               A22=ANG2(J2M,2)
               IF (A22.LE.A21) CYCLE
               IF (A22.LT.A12) THEN
                  FIJ=(A22-A21)/DANG
                  DO L=1,NWL
                     IF (Q11(L).LT.SMALL) CYCLE
                     Q21=Q11(L)*FIJ
                     Q2M(J2D2,KD2,L)=Q2M(J2D2,KD2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQTM(J2D2,KD2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
230            Q2M(J2D2,KD2,1:NWL)=Q2M(J2D2,KD2,1:NWL)+Q12
               IQTM(J2D2,KD2)=.FALSE.
               EXIT
240         CONTINUE
260      A11=A12
270   CONTINUE
280   CONTINUE
!--------------------------------------------------------------------
300   DO 380 J1=2,NP,2  
      IF (J1.LT.J0.AND.ANG2(J1+1,2).GE.ANG0(2)) CYCLE
      IF (J1.GT.J0.AND.ANG2(J1-1,2).GE.ANG0(2)) EXIT
      JD2=J1/2
      DO 370 KM=-2,2,4
         A11=0
         A12=0
         CALL SWEEP(3,KM,KEND)
         DO 360 K1=K0,KEND-KM,KM
            IF (A11.GE.ANG0(3)) EXIT
            K12=K1+KM/2
            A12=ANG3(K12,1)
            DANG=A12-A11
            IF (DANG.LE.1.0D-10) CYCLE
            A21=A11
            KD2=K1/2
            IF (IQTM(JD2,KD2)) GOTO 360
            IF (K1.EQ.K0) THEN
               Q11(1:NWL)=Q2M(JD2,KD2,1:NWL)*HALF
               IF (KM.GT.0) Q2M(JD2,KD2,1:NWL)=0
            ELSE
               Q11(1:NWL)=Q2M(JD2,KD2,1:NWL)
               Q2M(JD2,KD2,1:NWL)=0
            ENDIF
            IF (K1.NE.K0.OR.KM.GT.0) IQTM(JD2,KD2)=.TRUE.
            CALL IBCX(I1,J1,K1,IBC1)
            IF (IBC1.EQ.101.OR.IBC1.EQ.111) THEN
               QTX2(JD2,KD2,1:NWL)=QTX2(JD2,KD2,1:NWL)+Q11
               IQT2(JD2,KD2)=.FALSE.
               GOTO 360
            ENDIF
            Q12=Q11
            DO 340 K2=K1,KEND,KM
               K2M=K2+KM/2
               K2D2=K2/2
               IBC2=IBCELL(I1,J1,K2)
               IF (IBC1.LE.0.AND.IBC2.GE.1) GOTO 330
               IF (IBC1.GT.100.AND.IBC2.EQ.1) THEN
                  IF (J1.LT.J0) THEN
                     J2=J1+2
                  ELSEIF (J1.GT.J0) THEN
                     J2=J1-2
                  ENDIF
                  J2D2=J2/2
                  K2D2M=K2D2-KM/2
                  DO L=1,NWL
                     IF (Q12(L).LT.SMALL) CYCLE
                     Q21=Q12(L)*HALF
                     QTX2(J2D2,K2D2,L)=QTX2(J2D2,K2D2,L)+Q21
                     QTX2(JD2,K2D2M,L)=QTX2(JD2,K2D2M,L)+Q21
                  ENDDO
                  IQT2(J2D2,K2D2)=.FALSE.
                  IQT2(JD2,K2D2M)=.FALSE.
                  GOTO 360
               ENDIF
               IF (K2M.GT.LP.OR.K2M.LT.2) GOTO 330
               A22=ANG3(K2M,2)
               IF ((A22-A21).LE.1.0D-10) GOTO 340
               IF (A22.LT.A12) THEN
                  FIJ=(A22-A21)/DANG
                  DO L=1,NWL
                     IF (Q11(L).LT.SMALL) CYCLE
                     Q21=Q11(L)*FIJ
                     QTX2(JD2,K2D2,L)=QTX2(JD2,K2D2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQT2(JD2,K2D2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
330            QTX2(JD2,K2D2,1:NWL)=QTX2(JD2,K2D2,1:NWL)+Q12
               IQT2(JD2,K2D2)=.FALSE.
               EXIT
340         CONTINUE
360      A11=A12
370   CONTINUE
380   CONTINUE
900   RETURN
      END
!---
      SUBROUTINE SANG2(IDX,I0,J0,K0,I1,J1,K1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A1(3)
      IF (IDX.EQ.1) GOTO 200
      DO 120 IM=-1,1,2
         CALL SWEEP(1,IM,IEND)
         DO II=I0+IM,IEND,IM*2
            CALL SFAC(A1,I0,J0,K0,II,J1,K1,IDX)
            IF (A1(1).GE.ANG0(1)) GOTO 120
            ANG1(II,2)=A1(1)
         ENDDO
120   CONTINUE
200   IF (IDX.EQ.2) GOTO 300
      DO 220 JM=-1,1,2
         CALL SWEEP(2,JM,JEND) !determines last cell to sweep
         DO JJ=J0+JM,JEND,JM*2
            CALL SFAC(A1,I0,J0,K0,I1,JJ,K1,IDX)
            IF (A1(2).GE.ANG0(2)) GOTO 220
            ANG2(JJ,2)=A1(2)
         ENDDO
220   CONTINUE
300   IF (IDX.EQ.3) GOTO 900
      DO 320 KM=-1,1,2
         CALL SWEEP(3,KM,KEND)
         DO KK=K0+KM,KEND,KM*2
            CALL SFAC(A1,I0,J0,K0,I1,J1,KK,IDX)
            IF (A1(3).GE.ANG0(3)) GOTO 320
            ANG3(KK,2)=A1(3)
         ENDDO
320   CONTINUE
900   RETURN
      END
