!---
!     Calculate wall temperature
!---
      SUBROUTINE TWALL
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA IDBG/0/
      QA_W=0
      QLS_A=0
      QLS_G=0
      QE_W=0
      DO 200 I=2,MP,2
      DO 200 J=2,NP,2
      DO 200 K=2,LP,2
         IBC0=IBCELL(I,J,K)
         IF (IBC0.GT.0) CYCLE
         T_G=LG(I,J,K)%T
         DO NDR=1,6
            CALL TWALL_A
         ENDDO
200   CONTINUE
      QLS0=QLS_A+QLS_G
      IF (IDBG.GE.1) THEN
         G0=QA_W-QLS0-QE_W
         WRITE(6,'("QA_W:QLS0:QE_W:Bal:",T25,4F10.0)') QA_W,QLS0,QE_W,G0
      ENDIF
      RETURN

!---
      CONTAINS
      SUBROUTINE TWALL_A
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      I2=I
      J2=J
      K2=K
      IF (NDR.EQ.1) THEN
         I2=MAX(2,I-2)
      ELSEIF (NDR.EQ.2) THEN
         I2=MIN(MP,I+2)
      ELSEIF (NDR.EQ.3) THEN
         J2=MAX(2,J-2)
      ELSEIF (NDR.EQ.4) THEN
         J2=MIN(NP,J+2)
      ELSEIF (NDR.EQ.5) THEN
         K2=MAX(2,K-2)
      ELSEIF (NDR.EQ.6) THEN
         K2=MIN(LP,K+2)
      ENDIF
      IBC1=MOD(IBCELL(I2,J2,K2),10)
      IF (IBC1.NE.1) RETURN
      N0=(NDR+1)/2
      AR0=AREA_C(I,J,K,N0)
      IW1=IBCELL(I2,J2,K2)/10
      IF (IW1.LE.0) THEN
         W_K0=W_K
         W_HA0=W_HA
         W_L0=W_D
         W_T0=W_TA
         W_E0=W_E
      ELSE
         W_K0=W1_K(IW1)
         W_HA0=W1_HA(IW1)
         W_L0=W1_D(IW1)
         W_T0=W1_TA(IW1)
         W_E0=W1_E(IW1)
      ENDIF
      ID2=I2/2
      JD2=J2/2
      KD2=K2/2
      QA0=QA(ID2,JD2,KD2)
      QA_W=QA_W+QA0
      EMIS=W_E0
      RFL0=ONE-EMIS
      Q_AW=QA0*EMIS
      ESIG=EMIS*SIGN
      ESA=ESIG*AR0
      TWL0=T_G
      DO M=1,100
         R1=(W_L0/W_K0+1/W_HA0)
         QL_A=AR0*(TWL0-W_T0)/R1
         QL_G=AR0*H_G*(T_G-TWL0)
         G0=Q_AW-QL_A+QL_G
         IF (G0.LE.SMALL) EXIT
         G0=G0/ESA
         TWL1=G0**0.25D+0
         DTWL=ABS(TWL0-TWL1)
         TWL0=TWL1
         IF (DTWL.LE.ONE) EXIT
      ENDDO
      TG(ID2,JD2,KD2)=TWL0
      QL(ID2,JD2,KD2)=QL(ID2,JD2,KD2)+QL_A
      QL(I/2,J/2,K/2)=QL(I/2,J/2,K/2)-QL_G
      QLS_A=QLS_A+QL_A
      QLS_G=QLS_G-QL_G
      QE(ID2,JD2,KD2)=0 
      G1=ESA*TWL0**4
      CALL DEBX(TWL0)
      DO L=1,NWL 
         QAL0=QAL(ID2,JD2,KD2,L)+QALW(ID2,JD2,KD2,L)
         QEL(ID2,JD2,KD2,L)=G1*DEB(L)+QAL0*RFL0 
         QE(ID2,JD2,KD2)=QE(ID2,JD2,KD2)+QEL(ID2,JD2,KD2,L)
         QE_W=QE_W+QEL(ID2,JD2,KD2,L)
      ENDDO
      END SUBROUTINE TWALL_A
      END
