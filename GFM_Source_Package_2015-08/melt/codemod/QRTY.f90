!---
      SUBROUTINE QRTY(I0,J0,K0,J10,J1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A1(3),Q11(NWL),Q12(NWL)
      IDX=2
      J2=J1  
      J2=MIN(J2,NP)
      CALL SANG2(IDX,I0,J0,K0,I0,J2,K0)
      DO 280 K1=4,LP-2,2
      IF (K1.LT.K0.AND.ANG3(K1+1,2).GE.ANG0(3)) CYCLE
      IF (K1.GT.K0.AND.ANG3(K1-1,2).GE.ANG0(3)) EXIT
      KD2=K1/2
      DO 270 IPM=-2,2,4
         A11=0
         A12=0
         CALL SWEEP(1,IPM,IEND)
         DO 260 I1=I0,IEND-IPM,IPM
            IF (A11.GE.ANG0(1)) GOTO 270
            I12=I1+IPM/2
            A12=ANG1(I12,1)
            DANG=A12-A11
            IF (DANG.LE.1.0D-10) GOTO 260
            A21=A11
            I1D2=I1/2
            IF (IQT1(I1D2,KD2)) GOTO 260
            IF (I1.EQ.I0) THEN
               Q11(1:NWL)=QTX1(I1D2,KD2,1:NWL)*HALF
               IF (IPM.GT.0) QTX1(I1D2,KD2,1:NWL)=0
            ELSE
               Q11(1:NWL)=QTX1(I1D2,KD2,1:NWL)
               QTX1(I1D2,KD2,1:NWL)=0
            ENDIF
            IF (I1.NE.I0.OR.IPM.GT.0) IQT1(I1D2,KD2)=.TRUE.
            Q12=Q11
            DO 240 I2=I1,IEND,IPM
               I2D2=I2/2
               IF (IBCELL(I2,J1,K1).GE.1) GOTO 230
               I2M=I2+IPM/2
               A22=ANG1(I2M,2)
               IF (A22.LE.A21) CYCLE
               IF (A22.LT.A12) THEN
                  FIJ=(A22-A21)/DANG
                  DO L=1,NWL
                     IF (Q11(L).LT.SMALL) CYCLE
                     Q21=Q11(L)*FIJ
                     Q2M(I2D2,KD2,L)=Q2M(I2D2,KD2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQTM(I2D2,KD2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
230            Q2M(I2D2,KD2,1:NWL)=Q2M(I2D2,KD2,1:NWL)+Q12
               IQTM(I2D2,KD2)=.FALSE.
               EXIT
240         CONTINUE
260      A11=A12
270   CONTINUE
280   CONTINUE
!--------------------------------------------------------------------
300   DO 380 I1=2,MP,2
      IF (I1.LT.I0.AND.ANG1(I1+1,2).GE.ANG0(1)) CYCLE
      IF (I1.GT.I0.AND.ANG1(I1-1,2).GE.ANG0(1)) EXIT
      ID2=I1/2
      DO 370 KM=-2,2,4
         A11=0
         A12=0
         CALL SWEEP(3,KM,KEND)
         DO 360 K1=K0,KEND-KM,KM
            IF (A11.GE.ANG0(3)) EXIT
            K12=K1+KM/2
            A12=ANG3(K12,1)
            DANG=A12-A11
            IF (DANG.LE.1.0D-10) GOTO 360
            A21=A11
            KD2=K1/2
            IF (IQTM(ID2,KD2)) GOTO 360
            IF (K1.EQ.K0) THEN
               Q11(1:NWL)=Q2M(ID2,KD2,1:NWL)*HALF
               IF (KM.GT.0) Q2M(ID2,KD2,1:NWL)=0
            ELSE
               Q11(1:NWL)=Q2M(ID2,KD2,1:NWL)
               Q2M(ID2,KD2,1:NWL)=0
            ENDIF
            IF (K1.NE.K0.OR.KM.GT.0) IQTM(ID2,KD2)=.TRUE.
            CALL IBCX(I1,J1,K1,IBC1)
            IF (IBC1.EQ.201.OR.IBC1.EQ.211) THEN
               QTX2(ID2,KD2,1:NWL)=QTX2(ID2,KD2,1:NWL)+Q11
               IQT2(ID2,KD2)=.FALSE.
               GOTO 360
            ENDIF
            Q12=Q11
            DO 340 K2=K1,KEND,KM
               K2M=K2+KM/2
               K2D2=K2/2
               IBC2=IBCELL(I1,J1,K2)
               IF (IBC1.LE.0.AND.IBC2.GE.1) GOTO 330
               IF (IBC1.GE.100.AND.IBC2.EQ.1) THEN
                  IF (I1.LT.I0) THEN
                     I2=I1+2
                  ELSEIF (I1.GT.I0) THEN
                     I2=I1-2
                  ENDIF
                  I2D2=I2/2
                  K2D2M=K2D2-KM/2
                  DO L=1,NWL
                     IF (Q12(L).LT.SMALL) CYCLE
                     Q21=Q12(L)*HALF
                     QTX2(I2D2,K2D2,L)=QTX2(I2D2,K2D2,L)+Q21
                     QTX2(ID2,K2D2M,L)=QTX2(ID2,K2D2M,L)+Q21
                  ENDDO
                  IQT2(I2D2,K2D2)=.FALSE.
                  IQT2(ID2,K2D2M)=.FALSE.
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
                     QTX2(ID2,K2D2,L)=QTX2(ID2,K2D2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQT2(ID2,K2D2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
330            QTX2(ID2,K2D2,1:NWL)=QTX2(ID2,K2D2,1:NWL)+Q12
               IQT2(ID2,K2D2)=.FALSE.
               EXIT
340      CONTINUE
360      A11=A12
370   CONTINUE
380   CONTINUE
900   RETURN
      END
