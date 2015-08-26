!---
!     Emissive Fluxes: x-,y-, and z-directions
!---
      SUBROUTINE QRE(I0,J0,K0)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA NQRE/0/
      IF (IBCELL(I0,J0,K0).GE.1) RETURN
      I0D2=I0/2
      J0D2=J0/2
      K0D2=K0/2
      TG0=TG(I0D2,J0D2,K0D2)
      IF (TG0.LE.TG_MN) RETURN
      CALL DEBX(TG0)
      G0=SIGN*TG0**4
      QEL0=DEB*AKL(I0D2,J0D2,K0D2,1:NWL)*G0
!---
!     X-Flux
!---
      IDX=1
      QEL1=QEL0*DY(J0)*DZ(K0)
      N1=NZ
      N2=LZ
      CALL QRE_A
      DO IPM=-2,2,4
         QTX1(J0D2,K0D2,1:NWL)=QEL1
         IQT1(J0D2,K0D2)=.FALSE.
         QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1
         CALL SANG1(IDX,I0,J0,K0,IPM,IEND)
         I2=I0+IPM
         DO I=I2,IEND,IPM
            ID2=I/2
            I10=I-IPM
            CALL QRTX(I0,J0,K0,I10,I)
            DO 120 J=2,NP,2
            JD2=J/2
            DO 120 K=2,LP,2
            KD2=K/2
            IF (IQT2(JD2,KD2)) GOTO 120
            DO L=1,NWL
               IF (QTX2(JD2,KD2,L).LT.SMALL) CYCLE
               IF (IBCELL(I,J,K).GE.1.OR.I.EQ.IEND) THEN
                  QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QTX2(JD2,KD2,L)
               ELSE
                  QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2,KD2,L)
                  QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QA0
                  QTX1(JD2,KD2,L)=QTX2(JD2,KD2,L)-QA0
                  IQT1(JD2,KD2)=.FALSE.
               ENDIF
               QTX2(JD2,KD2,L)=0
               IQT2(JD2,KD2)=.TRUE.
            ENDDO
120         CONTINUE
            ANG2(1:NP,1)=ANG2(1:NP,2)
            ANG3(1:LP,1)=ANG3(1:LP,2)
         ENDDO
      ENDDO
      CALL QRE_B
!---
!     Y-Fluxes
!---
200   IDX=2
      QEL1=QEL0*DX(I0)*DZ(K0)
      N1=MZ
      N2=LZ
      CALL QRE_A
      DO JPM=-2,2,4
         QTX1(I0D2,K0D2,1:NWL)=QEL1
         IQT1(I0D2,K0D2)=.FALSE.
         QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1
         CALL SANG1(IDX,I0,J0,K0,JPM,JEND)
         J2=J0+JPM
         DO J=J2,JEND,JPM
            J10=J-JPM
            CALL QRTY(I0,J0,K0,J10,J)
            JD2=J/2
            DO 220 I=2,MP,2
            ID2=I/2
            DO 220 K=2,LP,2
            KD2=K/2
            IF (IQT2(ID2,KD2)) GOTO 220
            DO L=1,NWL
               IF (QTX2(ID2,KD2,L).LT.SMALL) CYCLE
               IF (IBCELL(I,J,K).GE.1.OR.J.EQ.JEND) THEN
                  QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QTX2(ID2,KD2,L)
               ELSE
                  QA0=QTX2(ID2,KD2,L)*AKL(ID2,JD2,KD2,L)
                  QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QA0
                  QTX1(ID2,KD2,L)=QTX2(ID2,KD2,L)-QA0
                  IQT1(ID2,KD2)=.FALSE.
               ENDIF
               QTX2(ID2,KD2,L)=0
               IQT2(ID2,KD2)=.TRUE.
            ENDDO
220         CONTINUE
            ANG1(1:MP,1)=ANG1(1:MP,2)
            ANG3(1:LP,1)=ANG3(1:LP,2)
         ENDDO
      ENDDO
      CALL QRE_B
!---
!     Z-Fluxes
!---
300   IDX=3
      QEL1=QEL0*DX(I0)*DY(J0)
      N1=MZ
      N2=NZ
      CALL QRE_A
      DO 360 KPM=-2,2,4
         QTX1(I0D2,J0D2,1:NWL)=QEL1
         IQT1(I0D2,J0D2)=.FALSE.
         QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1
         CALL SANG1(IDX,I0,J0,K0,KPM,KEND)
         K2=K0+KPM
         DO 340 K=K2,KEND,KPM
            KD2=K/2
            K10=K-KPM
            CALL QRTZ(I0,J0,K0,K10,K)
            DO 320 I=2,MP,2
            ID2=I/2
            DO 320 J=2,NP,2
            JD2=J/2
            IF (IQT2(ID2,JD2)) GOTO 320
            DO L=1,NWL
               IF (QTX2(ID2,JD2,L).LT.SMALL) CYCLE
                  !cbg            IF (IBCELL(I,J,K).EQ.4) THEN
                  !cbg               QA0=QTX2(ID2,JD2,L)/2.0D+0
                  !cbg               QAL(ID2,JD2,KD2-1,L)=QAL(ID2,JD2,KD2-1,L)+QA0
                  !cbg               QTX2(ID2,JD2,L)=ZERO
                  !cbg            ENDIF
               IF (IBCELL(I,J,K).GE.1.OR.K.EQ.KEND) THEN
                  QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QTX2(ID2,JD2,L)
               ELSE
                  QA0=QTX2(ID2,JD2,L)*AKL(ID2,JD2,KD2,L)
                  QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QA0
                  QTX1(ID2,JD2,L)=QTX2(ID2,JD2,L)-QA0
                  IQT1(ID2,JD2)=.FALSE.
               ENDIF
               QTX2(ID2,JD2,L)=0
               IQT2(ID2,JD2)=.TRUE.
            ENDDO
320         CONTINUE
            ANG1(1:MP,1)=ANG1(1:MP,2)
            ANG2(1:NP,1)=ANG2(1:NP,2)
340      CONTINUE
360   CONTINUE
      CALL QRE_B
      G0=QEL(I0D2,J0D2,K0D2,1)
      DO L=2,NWL 
         G0=G0+QEL(I0D2,J0D2,K0D2,L)
      ENDDO
      QE(I0D2,J0D2,K0D2)=G0
900   RETURN


      CONTAINS
      SUBROUTINE QRE_A
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ALLOCATE (QTX1(N1,N2,NWL),Q2M(N1,N2,NWL),QTX2(N1,N2,NWL))
      ALLOCATE (IQT1(N1,N2),IQTM(N1,N2),IQT2(N1,N2))
      QTX1=0
      Q2M=0
      QTX2=0
      IQT1=.TRUE.
      IQTM=.TRUE.
      IQT2=.TRUE.
      END SUBROUTINE QRE_A
      SUBROUTINE QRE_B
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DEALLOCATE (Q2M,QTX1,QTX2,IQT1,IQTM,IQT2)
      IF (NQRE.EQ.0) RETURN
      QAS=SUM(QAL)
      QES=SUM(QEL)
      WRITE (6,901) I0,J0,K0,QES,QAS
901   FORMAT (3I5,' Total QE =',E12.5,' Total QA=',E12.5)
      END SUBROUTINE QRE_B
      END

      SUBROUTINE SFAC(ANG,I1,J1,K1,I2,J2,K2,IDX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ANG(3)
      ANG(IDX)=0
      IF (IDX.NE.1) GOTO 200
      B=ABS(X(I2)-X(I1))
      IF (B.LT.SMALL) THEN
         ANG(2)=90
         ANG(3)=90
      ELSE
         H2=ABS(Y(J2)-Y(J1))/B
         ANG(2)=ATAND(H2)
         H3=ABS(Z(K2)-Z(K1))/B
         ANG(3)=ATAND(H3)
      ENDIF
      RETURN
200   IF (IDX.NE.2) GOTO 300
      B=ABS(Y(J2)-Y(J1))
      IF (B.LT.SMALL) THEN
         ANG(1)=90
         ANG(3)=90
      ELSE
         H1=ABS(X(I2)-X(I1))/B
         ANG(1)=ATAND(H1)
         H3=ABS(Z(K2)-Z(K1))/B
         ANG(3)=ATAND(H3)
      ENDIF
      RETURN
300   IF (IDX.NE.3) GOTO 900
      B=ABS(Z(K2)-Z(K1))
      IF (B.LT.SMALL) THEN
         ANG(1)=90
         ANG(2)=90
      ELSE
         H1=ABS(X(I2)-X(I1))/B
         ANG(1)=ATAND(H1)
         H2=ABS(Y(J2)-Y(J1))/B
         ANG(2)=ATAND(H2)
      ENDIF
900   RETURN
      END

      SUBROUTINE SANG1(IDX,I0,J0,K0,LPM,LEND1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IBC0=IBCELL(I0,J0,K0)
      IF (IBCELL(I0,J0,K0).GT.10) IBC0=MOD(IBC0,10)
      IF (IBC0.LE.0) THEN
         I1=I0+LPM/2
         J1=J0+LPM/2
         K1=K0+LPM/2
         CALL SFAC(ANG0,I0,J0,K0,I1,J1,K1,IDX)
      ELSEIF (IBC0.EQ.4) THEN
         ANG0=90
         ANG0(IDX)=0
      ELSE
         ANG0=60
         ANG0(IDX)=0
      ENDIF
      IF (IDX.NE.1) THEN
         ANG1=ANG0(1)
         ANG1(I0,1:2)=0
      ENDIF
      IF (IDX.NE.2) THEN
         ANG2=ANG0(2)
         ANG2(J0,1:2)=0
      ENDIF
      IF (IDX.NE.3) THEN
         ANG3=ANG0(3)
         ANG3(K0,1:2)=0
      ENDIF
      CALL SWEEP(IDX,LPM,LEND1)
      END

      SUBROUTINE SWEEP(IDX,LPM,LEND1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (LPM.LT.0) THEN
         LEND1=2
      ELSE
         LEND1=MP3(IDX)
      ENDIF
900   RETURN
      END
