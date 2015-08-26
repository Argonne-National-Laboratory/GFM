! Dflow2.F90
!======================================================================
!     To calculate droplet and vapor mass flow at axial location and
!        droplet collision rate with the wall
!        revision: 10/97
!======================================================================
SUBROUTINE DFLOW2
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION SLSX(MPD2),AREA(3)
DATA DUWF,LD/0.0D+0,0/
2     FORMAT(' D',I5,E12.4,3I4,E12.4)
3     FORMAT(T6,I2,6E11.4)
4     FORMAT ('      flow rates: ',5G11.4)

!----------------------------------------------------------------------
!     Droplet mass entering (IN) and leaving (OUT) the system
!        TPIN:  droplet flow rate in (kg/s)
!        TPOUT: droplet mass rate out (kg/s)
!        DAW0: droplet mass rate out through wall (kg/s)
!        FR_V: oil vapor flow rate in (kg/s)
!        VPOUT: oil vapor rate out (kg/s)  
!        DWCW: droplet collision rate with walls
!----------------------------------------------------------------------
TPIN=FR_Q
IF (TPIN.LT.SMALL20) RETURN
DWCW=ZERO
PFEX=ZERO
AFE=ZERO

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
      DO IU=1,3
         DO M=-1,1,2
            CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
            IF (IBCELL(I1,J1,K1).EQ.3) THEN
               FE=UG(I1,J1,K1,IU)*THETA(I1,J1,K1)*DNST(I1,J1,K1)*AREA(IU)
               AFE=AFE+FE*(GF(I,J,K,IYF)+GF(I,J,K,IYCO2)+GF(I,J,K,IYH2O))*M
            ENDIF
         ENDDO
      enddo
      DFFR=ZERO
      BS(ID2,JD2,KD2)=ZERO
      DO L=1,NDP0
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
                  DFFH=DFFH-FL1*RD3(L)*(CL*DT(ID2,JD2,KD2,L)*T0+H0_L)
               ENDIF
            ENDDO !m=
         enddo !iu=
      enddo !L=
      DFF=DFF+DFFR
   enddo;enddo !j=, k=
   SLSX(ID2)=DFF*DMFR
   FLX(I+1,8)=SLSX(ID2)
   FLXH(I+1,3)=DFFH*DMFR
   !CSL
   IF (SLSX(ID2).GT.FR_Q*1.05D+0) THEN
      FAC0=FR_Q/SLSX(ID2)
      CALL DFLOW21(ID2,FAC0)
      SLSX(ID2)=FR_Q
   ENDIF
   !CSL
enddo !i=
VPOUT = GMFR*AFE/FACTOR
PFEX=PFEX*DMFR
TPOUT = PFEX+VPOUT

!-----CONDENSED PHASE MASS BALANCE

res_mass=zero
res_mass_max=ZERO   
GT0=ZERO
NO=0
DO I=4,MPM2,2;    ID2=I/2
DO J=NJY1,NJY2,2; JD2=J/2
DO K=LKZ1,LKZ2,2; KD2=K/2
   IF (IBCELL(I,J,K).GE.1) cycle
   NO=NO+1
   VOL=VOL_C(I,J,K)
   BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)-EVP(ID2,JD2,KD2)/RFG*VOL
   GT0=GT0+CON(ID2,JD2,KD2)*VOL
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
DO L1=1,NDP0
   G1=DU(IMX,JMX,KMX,L1,1)
   G2=DU(IMX,JMX,KMX,L1,2)
   G3=DU(IMX,JMX,KMX,L1,3)
   G4=TH_DP(IMX,JMX,KMX)
   G5=DN(IMX,JMX,KMX,L1)
   G6=DT(IMX,JMX,KMX,L1)
   WRITE(NU,3) L1,G1,G2,G3,G4,G5,G6
   WRITE(20,3) L1,G1,G2,G3,G4,G5,G6
ENDDO
GT0=GT0*GMFR
TOUT=TPOUT+GT0 
TSRS=TOUT/TPIN

!----------------------------------------------------------------------
!     Global mass balance for condensed phase
!----------------------------------------------------------------------
!      DATA DEL1,DEL2,FADJ_S/0.1D+0,-0.1D+0,0.0D+0/
!      IF (FADJ_S.GT.ZERO) THEN
!         DEL=ONE-TSRS
!         IF (DEL.LT.DEL2) DEL=DEL2
!         IF (DEL.GT.DEL1) DEL=DEL1
!         DEL=DEL*FADJ_S
!         K=NDR0
!         DO JD2=2,NZM1 
!     NEEDS PROPER VALUE FOR KB--BG
!            DN(MZM1,JD2,1,K)=DN(MZM1,JD2,1,K)*(ONE+DEL) 
!            IF (DU(MZ,JD2,1,K).GT.1.D-10) 
!     &         DN(MZ,JD2,1,K)=DN(MZM1,JD2,1,K)
!     &         *DU(MZM1,JD2,1,K)/DU(MZ,JD2,1,K)
!         END DO
!      ENDIF
!---
WRITE(20,4) TPIN,VPOUT,GT0,PFEX,TOUT
WRITE(NU,4) TPIN,VPOUT,GT0,PFEX,TOUT
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE DFLOW21(ID2,FAC0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

I=ID2*2
DO J=NJY1,NJY2,2;  JD2=J/2
DO K=LKZ1,LKZ2,2
   IF (IBCELL(I,J,K).GE.1) cycle
   KD2=K/2
   DO L=1,NDP0
      DN(ID2,JD2,KD2,L)=DN(ID2,JD2,KD2,L)*FAC0
   ENDDO
enddo;enddo
RETURN
END
