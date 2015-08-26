!---
!     Wall Emissive Fluxes: x-,y-, and z-directions
!---
      SUBROUTINE QREW(I0,J0,K0,NDR)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      I0D2=I0/2
      J0D2=J0/2
      K0D2=K0/2
      IF (NDR.EQ.1) THEN
         I2=I0+2
         GOTO 100
      ELSEIF (NDR.EQ.2) THEN
         I2=I0-2
         GOTO 100
      ELSEIF (NDR.EQ.3) THEN
         J2=J0+2
         GOTO 200
      ELSEIF (NDR.EQ.4) THEN
         J2=J0-2
         GOTO 200
      ELSEIF (NDR.EQ.5) THEN
         K2=K0+2
         GOTO 300
      ELSEIF (NDR.EQ.6) THEN
         K2=K0-2
         GOTO 300
      ENDIF
      RETURN

!---
!     X-Flux
!---
100   IDX=1
      IPM=I2-I0
      N1=NZ
      N2=LZ
      CALL QREW_A
      QTX1(J0D2,K0D2,1:NWL)=QEL0
      IQT1(J0D2,K0D2)=.FALSE.
      CALL SANG1(IDX,I0,J0,K0,IPM,IEND)
      DO I=I2,IEND,IPM
         ID2=I/2
         I10=I-IPM
         CALL QRTX(I0,J0,K0,I10,I)
         DO 120 J=2,NP,2
         JD2=J/2
         DO 120 K=2,LP,2
            KD2=K/2
            IF (IQT2(JD2,KD2)) CYCLE
            CALL IBCX(I,J,K,IBC0)
            DO L=1,NWL
               IF (QTX2(JD2,KD2,L).LT.SMALL) CYCLE
               IF (IBC0.GE.1.OR.I.EQ.IEND) THEN
                  IF (IBC0.EQ.201) THEN
                     QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2-1,KD2,L)
                     QALW(ID2,JD2-1,KD2,L)=QALW(ID2,JD2-1,KD2,L)+QA0
                  ELSEIF (IBC0.EQ.211) THEN
                     QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2+1,KD2,L)
                     QALW(ID2,JD2+1,KD2,L)=QALW(ID2,JD2+1,KD2,L)+QA0
                  ELSEIF (IBC0.EQ.301) THEN
                     QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2,KD2-1,L)
                     QALW(ID2,JD2,KD2-1,L)=QALW(ID2,JD2,KD2-1,L)+QA0
                  ELSEIF (IBC0.EQ.311) THEN
                     QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2,KD2+1,L)
                     QALW(ID2,JD2,KD2+1,L)=QALW(ID2,JD2,KD2+1,L)+QA0
                  ENDIF
                  QALW(ID2,JD2,KD2,L)=QALW(ID2,JD2,KD2,L)+QTX2(JD2,KD2,L)-QA0
               ELSE
                  QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2,KD2,L)
                  QALW(ID2,JD2,KD2,L)=QALW(ID2,JD2,KD2,L)+QA0
                  QTX1(JD2,KD2,L)=QTX2(JD2,KD2,L)-QA0
                  IQT1(JD2,KD2)=.FALSE.
               ENDIF
               QTX2(JD2,KD2,L)=0
               IQT2(JD2,KD2)=.TRUE.
            ENDDO
120      CONTINUE
         ANG2(1:NP,1)=ANG2(1:NP,2)
         ANG3(1:LP,1)=ANG3(1:LP,2)
      ENDDO
      GOTO 900
!---
!     Y-Fluxes
!---
200   IDX=2
      JPM=J2-J0
      N1=MZ
      N2=LZ
      CALL QREW_A
      QTX1(I0D2,K0D2,1:NWL)=QEL0
      IQT1(I0D2,K0D2)=.FALSE.
      CALL SANG1(IDX,I0,J0,K0,JPM,JEND)
      J2=J0+JPM
      DO J=J2,JEND,JPM
         J10=J-JPM
         JD2=J/2
         CALL QRTY(I0,J0,K0,J10,J)
         DO 220 I=2,MP,2
         ID2=I/2
         DO 220 K=2,LP,2
         KD2=K/2
         IF (IQT2(ID2,KD2)) GOTO 220
         DO L=1,NWL
            IF (QTX2(ID2,KD2,L).LT.SMALL) CYCLE
            CALL IBCX(I,J,K,IBC0)
            IF (IBC0.GE.1.OR.J.EQ.JEND) THEN
               IF (IBC0.EQ.101) THEN
                 QA0=QTX2(ID2,KD2,L)*AKL(ID2-1,JD2,KD2,L)
                 QALW(ID2-1,JD2,KD2,L)=QALW(ID2-1,JD2,KD2,L)+QA0
               ELSEIF (IBC0.EQ.111) THEN
                 QA0=QTX2(ID2,KD2,L)*AKL(ID2+1,JD2,KD2,L)
                 QALW(ID2+1,JD2,KD2,L)=QALW(ID2+1,JD2,KD2,L)+QA0
               ELSEIF (IBC0.EQ.301) THEN
                 QA0=QTX2(ID2,KD2,L)*AKL(ID2,JD2,KD2-1,L)
                 QALW(ID2,JD2,KD2-1,L)=QALW(ID2,JD2,KD2-1,L)+QA0
               ELSEIF (IBC0.EQ.311) THEN
                 QA0=QTX2(ID2,KD2,L)*AKL(ID2,JD2,KD2+1,L)
                 QALW(ID2,JD2,KD2+1,L)=QALW(ID2,JD2,KD2+1,L)+QA0
               ENDIF
               QALW(ID2,JD2,KD2,L)=QALW(ID2,JD2,KD2,L)+QTX2(ID2,KD2,L)-QA0
            ELSE
               QA0=QTX2(ID2,KD2,L)*AKL(ID2,JD2,KD2,L)
               QALW(ID2,JD2,KD2,L)=QALW(ID2,JD2,KD2,L)+QA0
               QTX1(ID2,KD2,L)=QTX2(ID2,KD2,L)-QA0
               IQT1(ID2,KD2)=.FALSE.
            ENDIF
            QTX2(ID2,KD2,L)=0
            IQT2(ID2,KD2)=.TRUE.
         ENDDO
220      CONTINUE
         ANG1(1:MP,1)=ANG1(1:MP,2)
         ANG3(1:LP,1)=ANG3(1:LP,2)
      ENDDO
      GOTO 900
!---
!     Z-Fluxes
!---
300   IDX=3
      KPM=K2-K0
      N1=MZ
      N2=NZ
      CALL QREW_A
      QTX1(I0D2,J0D2,1:NWL)=QEL0
      IQT1(I0D2,J0D2)=.FALSE.
      CALL SANG1(IDX,I0,J0,K0,KPM,KEND)
      K2=K0+KPM
      DO K=K2,KEND,KPM
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
            CALL IBCX(I,J,K,IBC0)
            IF (IBC0.GE.1.OR.K.EQ.KEND) THEN
               IF (IBC0.EQ.101) THEN
                 QA0=QTX2(ID2,JD2,L)*AKL(ID2-1,JD2,KD2,L)
                 QALW(ID2-1,JD2,KD2,L)=QALW(ID2-1,JD2,KD2,L)+QA0
               ELSEIF (IBC0.EQ.111) THEN
                 QA0=QTX2(ID2,JD2,L)*AKL(ID2+1,JD2,KD2,L)
                 QALW(ID2+1,JD2,KD2,L)=QALW(ID2+1,JD2,KD2,L)+QA0
               ELSEIF (IBC0.EQ.201) THEN
                 QA0=QTX2(ID2,JD2,L)*AKL(ID2,JD2-1,KD2,L)
                 QALW(ID2,JD2-1,KD2,L)=QALW(ID2,JD2-1,KD2,L)+QA0
               ELSEIF (IBC0.EQ.211) THEN
                 QA0=QTX2(ID2,JD2,L)*AKL(ID2,JD2+1,KD2,L)
                 QALW(ID2,JD2+1,KD2,L)=QALW(ID2,JD2+1,KD2,L)+QA0
               ENDIF
               QALW(ID2,JD2,KD2,L)=QALW(ID2,JD2,KD2,L)+QTX2(ID2,JD2,L)-QA0
            ELSE
               QA0=QTX2(ID2,JD2,L)*AKL(ID2,JD2,KD2,L)
               QALW(ID2,JD2,KD2,L)=QALW(ID2,JD2,KD2,L)+QA0
               QTX1(ID2,JD2,L)=QTX2(ID2,JD2,L)-QA0
               IQT1(ID2,JD2)=.FALSE.
            ENDIF
            QTX2(ID2,JD2,L)=0
            IQT2(ID2,JD2)=.TRUE.
         ENDDO
320      CONTINUE
         ANG1(1:MP,1)=ANG1(1:MP,2)
         ANG2(1:NP,1)=ANG2(1:NP,2)
      ENDDO
900   DEALLOCATE (QTX1,QTX2,Q2M,IQT1,IQTM,IQT2)
      RETURN

      CONTAINS
      SUBROUTINE QREW_A
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ALLOCATE (QTX1(N1,N2,NWL),QTX2(N1,N2,NWL),Q2M(N1,N2,NWL))
      ALLOCATE (IQT1(N1,N2),IQTM(N1,N2),IQT2(N1,N2))
      QTX1=0
      Q2M=0
      QTX2=0
      IQT1=.TRUE.
      IQTM=.TRUE.
      IQT2=.TRUE.
      END SUBROUTINE QREW_A
      END
