!---
!  Calculation of net radiation heat power
!     QE:  emissive power (W)
!     QA:  absorptive power (W)
!     WL: wavelength (m)
!     rev: 8/02
!--
      SUBROUTINE QRN
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL LDR(6)
7     FORMAT (5X,' QA_W:S:G:L (MW) =',-6PF12.4,3F12.4,', r=',0PF12.4)
      ALLOCATE (ANG1(MP,2),ANG2(NP,2),ANG3(LP,2))
      ALLOCATE (QEL0(NWL),QEL1(NWL))
      ALLOCATE (QALW(MZ,NZ,LZ,NWL))
      ALLOCATE (QEL(MZ,NZ,LZ,NWL),QAL(MZ,NZ,LZ,NWL))
      CPUS1=SECNDS(0.0)
!---
!     Liquid Glass Emission and Absorption
!---
      QEL=0
      IF (IRST) THEN
         CALL SAVQ(0,I0)
         CALL QRN_B
         IF (I0.GE.MP) GOTO 200
         I1=I0+2
      ELSE
         QE=0
         QES0=0
         QAL=0
         I1=2
      ENDIF
      DO I=I1,MP,2
         ID2=I/2
         DO 120 J=2,NP,2
         JD2=J/2
         DO 120 K=2,LP,2
            IBC0=IBCELL(I,J,K)
            IF (IBC0.GE.1) CYCLE
            KD2=K/2
            CALL QRE(I,J,K) 
            QES0=QES0+QE(ID2,JD2,KD2)
            !cbg         QA_HOLD=QES0/12.0D+0
120      CONTINUE
         IF (ISAV) CALL SAVQ(1,I)
         I0=I
         CALL QRN_B
      ENDDO
      DO 180 ID2=1,MZ
      DO 180 JD2=1,NZ
      DO 180 KD2=1,LZ
         G0=0
         DO L=1,NWL
            G0=G0+QAL(ID2,JD2,KD2,L)
         ENDDO
         !cbg      IF(IBCELL(2*ID2,2*JD2,2*KD2).EQ.4) THEN
         !cbg       QA(ID2,JD2,KD2)=QA_HOLD
         !cbg      ELSE
         QA(ID2,JD2,KD2)=G0
         !cbg      ENDIF
180   CONTINUE
200   CONTINUE
      !CBG 200  DEALLOCATE (AKL0)
!---
!     Wall Emission and Reflection
!---
      IF (.NOT.ALLOCATED(QL)) ALLOCATE (QL(MZ,NZ,LZ))
      QL=0
      N=0
      FAC_W=1
      !CSL      IF (IRST) THEN
      !CSL         CALL SAVQ(3,I1)
      !CSL      IF (QAS0.GT.1.0D+3) GOTO 430
      !CSL   ENDIF
300   CALL TWALL
      QALW=0
      QE_W1=0
      DO 340 I=2,MP,2
      ID2=I/2
      DO 340 J=2,NP,2
      JD2=J/2
      DO 340 K=2,LP,2
         KD2=K/2
         G0=QE(ID2,JD2,KD2)
         IF (IBCELL(I,J,K).LE.0.OR.G0.LT.SMALL) CYCLE
         QE_W1=QE_W1+G0
         LDR=.FALSE.
         MDR=0
         DO NDR=1,6
            CALL QRN_A
         ENDDO
         IF (MDR.LE.0) CYCLE
         QEL0(1:NWL)=QEL(ID2,JD2,KD2,1:NWL)*FAC_W
         QEL0=QEL0/MDR
         DO NDR=1,6
            IF (LDR(NDR)) CALL QREW(I,J,K,NDR) 
         ENDDO
340   CONTINUE
      QES0=QESG+QE_W1
      QA_G=0
      QA_S=0
      QA_IO=0
      QA_WW=0
      QA_WS=0
      DO 420 I=2,MP,2
      ID2=I/2
      DO 420 J=2,NP,2
      JD2=J/2
      DO 420 K=LP,2,-2
         KD2=K/2
         G0=0
         IBC0=MOD(IBCELL(I,J,K),10)
         DO L=1,NWL
            G0=G0+QAL(ID2,JD2,KD2,L)+QALW(ID2,JD2,KD2,L)
            IF (IBC0.EQ.1) QA_WW=QA_WW+QALW(ID2,JD2,KD2,L)
            IF (IBC0.EQ.4) QA_WS=QA_WS+QALW(ID2,JD2,KD2,L)
           ! IF (IBC0.EQ.4) then  ! changed by Golchert fall 2004
         !  QA_WS=QA_WS+QALW(ID2,JD2,KD2,L)
           !  QAL(ID2,JD2-1,KD2,L)=QALW(ID2,JD2,KD2,L)
           ! endif

         ENDDO
         QA(ID2,JD2,KD2)=G0
         IF (IBCELL(I,J,K).LE.0) THEN
            QA_G=QA_G+QA(ID2,JD2,KD2)
         ELSEIF (IBCELL(I,J,K).EQ.2.OR.IBCELL(I,J,K).EQ.3) THEN
            QA_IO=QA_IO+QA(ID2,JD2,KD2)
         ELSEIF (IBC0.EQ.4) THEN
            QA_S=QA_S+QA(ID2,JD2,KD2)
         ENDIF
420   CONTINUE
      QLS0=QLS0+QA_IO
      QAS0=QA_G+QA_S+QLS0
      !CSL      IF (ISAV) CALL SAVQ(2,I1)
430   N=N+1
      IF (QAS0.GT.1D-5) THEN
         FAC_W=QES0/QAS0
      ELSE
         FAC_W=ONE
      ENDIF
      G0=FAC_W-ONE
      CPUS2=SECNDS(CPUS1)
      WRITE (6,'(/"N="I5,T20,"CPU=",F10.1," s")') N,CPUS2
      WRITE (6,7) QES0,QA_S,QA_G,QLS0,FAC_W
      ECONV=1.0D-6
      IF (ABS(G0).LE.ECONV.OR.N.GE.NRADWALL) GOTO 480
      FAC_W=ONE
      GOTO 300
480   DEALLOCATE (QALW)
      DEALLOCATE (ANG1,ANG2,ANG3)
500   RETURN
      CONTAINS
      SUBROUTINE QRN_A
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      I2=I
      J2=J
      K2=K
      IF (NDR.EQ.1) THEN
         I2=MIN(MP,I+2)
      ELSEIF (NDR.EQ.2) THEN
         I2=MAX(2,I-2)
      ELSEIF (NDR.EQ.3) THEN
         J2=MIN(NP,J+2)
      ELSEIF (NDR.EQ.4) THEN
         J2=MAX(2,J-2)
      ELSEIF (NDR.EQ.5) THEN
         K2=MIN(LP,K+2)
      ELSEIF (NDR.EQ.6) THEN
         K2=MAX(2,K-2)
      ENDIF
      IF (IBCELL(I2,J2,K2).GT.0) RETURN
      LDR(NDR)=.TRUE.
      MDR=MDR+1
      END SUBROUTINE QRN_A
      SUBROUTINE QRN_B
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      QASG=SUM(QAL)
      QESG=QES0
      CPUS2=SECNDS(CPUS1)
      WRITE (6,5) I0,QESG,QASG,CPUS2
5     FORMAT (I5,' Total QE:QA (MW) =',-6PF12.4,F12.4,', CPU=',0PF8.2)
      END SUBROUTINE QRN_B
      END
