! PFLOW2.F90
!======================================================================
!     To calculate particle mass flow at axial location 
!        revision: 10/97
!======================================================================
SUBROUTINE PFLOW2
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION SLSX(MPD2),AREA(3)
DATA DUWF,LD/0.0D+0,0/
2     FORMAT(' P',I5,E12.4,3I4,E12.4)
3     FORMAT(T6,I2,6E11.4)
4     FORMAT ('      flow rates: ',5G11.4)
!----------------------------------------------------------------------
!     particle mass entering (IN) and leaving (OUT) the system
!        TPIN:  particle flow rate in (kg/s)
!        TPOUT: particle mass rate out (kg/s)
!        DAW0: particle mass rate out through wall (kg/s)
!----------------------------------------------------------------------
TPIN=FR_P
IF (TPIN.LT.SMALL20) RETURN
!----------------------------------------------------------------------
!     DWCW: particle collision rate with walls
!----------------------------------------------------------------------
DWCW=ZERO
PFEX=ZERO
DO I=4,MPM2,2;  ID2=I/2
   DFF=ZERO
   DFFH=ZERO
   SLSX(ID2)=ZERO
   DO J=NJY1,NJY2,2;  JD2=J/2
   DO K=LKZ1,LKZ2,2
      IBC0=IBCELL(I,J,K)
      IF (IBC0.EQ.1) cycle
      KD2=K/2
      AREA(1)=AREA_C(I,J,K,1)
      AREA(2)=AREA_C(I,J,K,2)
      AREA(3)=AREA_C(I,J,K,3)
      DFFR=ZERO
      BS(ID2,JD2,KD2)=ZERO
      DO L1=1,NPT0
         L=L1+NDP0
         DO IU=1,3
            DO M=-1,1,2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               IBC1=IBCELL(I1,J1,K1)
               IF (IBC1.EQ.1) THEN
                  DIFF=ZERO
                  IF (DU(ID2,JD2,KD2,L,IU)*M.GT.ZERO) THEN
                     DUK=DU(ID2,JD2,KD2,L,IU)*DUWF
                  ELSE
                     DUK=ZERO
                  ENDIF
                  DU(ID21,JD21,KD21,L,IU)=DUK
               ELSEIF (IBC1.EQ.2) THEN
                  DIFF=ZERO
                  DUK=DU(ID21,JD21,KD21,L,IU)
               ELSEIF (IBC1.EQ.3) THEN
                  DIFF=ZERO
                  CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                  DUK=F1*DU(ID21,JD21,KD21,L,IU)+F2*DU(ID2,JD2,KD2,L,IU)
               ELSE
                  DIFF=DFF0(I1,J1,K1,L,IU)
                  CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                  DUK=F1*DU(ID21,JD21,KD21,L,IU)+F2*DU(ID2,JD2,KD2,L,IU)
               ENDIF
               FL=DUK*AREA(IU)
               IF(IBC1.EQ.1.AND.T(I1,J1,K1).GE.TTWE) FL=ZERO
               IF (M*FL.GE.ZERO) THEN
                  DNK=DN(ID2,JD2,KD2,L)
               ELSE
                  DNK=DN(ID21,JD21,KD21,L)
               ENDIF
               DDNK=DN(ID21,JD21,KD21,L)-DN(ID2,JD2,KD2,L)
               FL2=UG(I1,J1,K1,IU)*AREA(IU)
               FL1=DA(DIFF,FL2)*DDNK-FL*DNK*M
               IF (IBC1.EQ.1.AND.FL1.LT.ZERO) THEN
                  DWCW=DWCW-FL1*RD3(L)
               ENDIF
               IF (IBC1.EQ.3) THEN
                  PFEX=PFEX-FL1*RD3(L)
               ENDIF
               BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+FL1*RD3(L)
               IF (IU.EQ.1.AND.M.EQ.1) THEN
                  DFFR=DFFR-FL1*RD3(L)
                  DFFH=DFFH-FL1*RD3(L)*DT(ID2,JD2,KD2,L)
               ENDIF
            ENDDO !m=
         enddo !iu=
      enddo !L1=
      DFF=DFF+DFFR
   enddo;enddo
   SLSX(ID2)=DFF*PMFR
   IF (SLSX(ID2).LE.ZERO) THEN
      CALL PFLOW21(ID2,ZERO)
      SLSX(ID2)=ZERO
   ELSEIF (ID2.GE.2.AND.SLSX(ID2).GT.FR_P*1.05D+0) THEN
      FAC0=FR_P/SLSX(ID2)
      CALL PFLOW21(ID2,FAC0)
      SLSX(ID2)=SLSX(ID2)*FAC0
   ENDIF
   FLX(I+1,9)=SLSX(ID2)
   FLXH(I+1,4)=DFFH*PMFR*CL_P*T0
enddo
PFEX=PFEX*PMFR
TPOUT = PFEX

!-----CONDENSED PHASE MASS BALANCE
res_mass=ZERO
res_mass_max=ZERO   
GT0=ZERO
NO=0
DO I=2,MPM2,2;    ID2=I/2
DO J=NJY1,NJY2,2; JD2=J/2
DO K=LKZ1,LKZ2,2; KD2=K/2
   IF (IBCELL(I,J,K).GE.1) cycle 
   NO=NO+1
   BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)
   BTMP=ABS(BS(ID2,JD2,KD2))
   res_mass=res_mass+BTMP
   IF (BTMP.GT.res_mass_max) THEN
      res_mass_max=BTMP
      IMX=ID2
      JMX=JD2
      KMX=KD2
   ENDIF
enddo;enddo;enddo

IF (NO.GT.0) res_mass=res_mass/NO
LD=LD+1
I=IMX*2
J=JMX*2
K=KMX*2
WRITE(NU,2) LD,res_mass_max,I,J,K,res_mass
WRITE(20,2) LD,res_mass_max,I,J,K,res_mass
DO L2=1,NPT0
   L1=L2+NDP0
   G1=DU(IMX,JMX,KMX,L1,1)
   G2=DU(IMX,JMX,KMX,L1,2)
   G3=DU(IMX,JMX,KMX,L1,3)
   G4=TH_PT(IMX,JMX,KMX)
   G5=DN(IMX,JMX,KMX,L1)
   G6=DT(IMX,JMX,KMX,L1)
   WRITE(NU,3) L1,G1,G2,G3,G4,G5,G6
   WRITE(20,3) L1,G1,G2,G3,G4,G5,G6
ENDDO
TSRS=TPOUT/TPIN

WRITE(20,4) TPIN,TPOUT
WRITE(NU,4) TPIN,TPOUT
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE PFLOW21(ID2,FAC0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
I=ID2*2
DO J=NJY1,NJY2,2;  JD2=J/2
DO K=LKZ1,LKZ2,2
   IF (IBCELL(I,J,K).GE.1) cycle
   KD2=K/2
   DO L1=1,NPT0
      L=L1+NDP0
      DN(ID2,JD2,KD2,L)=DN(ID2,JD2,KD2,L)*FAC0
   ENDDO
enddo;enddo
RETURN
END
