! DFLOW.FOR
!----------------------------------------------------------------------
!     solves property variables for droplet phase
!       DN     number density
!       DU     velocity components
!       DT     temperature
!     Revision: 10/97
!----------------------------------------------------------------------
SUBROUTINE DFLOW
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION SMXD(6,NDNP),AVED(6,NDNP)
DATA SMX0/1.0D-12/

!----------------------------------------------------------------------
!     Droplet Number Density
!----------------------------------------------------------------------
!      KD=1
eqtyp=1
SGM=DSIGMA
CALL DPFLO1(SMX0)
MZM1=MPD2-1

NEL=13
MI1=4
MI3=MPM2
NJ1=NJY1
NJ3=NJY2
LK1=LKZ1
LK3=LKZ2
NJD1=NJ1/2
NJD3=NJ3/2
LKD1=LK1/2
LKD3=LK3/2

DO L=NDP0,1,-1
   KL=L
   DO ID2=1,MPD2
   DO JD2=1,NPD2
   DO KD2=1,LPD2
      FZ(ID2,JD2,KD2)=DN(ID2,JD2,KD2,KL)
   enddo;enddo;enddo  

   CALL SOURCED(MI1,MI3,NJ1,NJ3,LK1,LK3)
   CALL DPFLO2(MI1,MI3,NJ1,NJ3,LK1,LK3)
   !         CALL ADLBL3(FZ,KD,2,MZM1,NJD1,NJD3,LKD1,LKD3)
   CALL ADLBL3(FZ,2,MZM1,NJD1,NJD3,LKD1,LKD3)

   !--  Apply upper and lower bounds to calculated values
   DO I=2,MPM2,2;    ID2=I/2
   DO J=NJY1,NJY2,2; JD2=J/2
   DO K=LKZ1,LKZ2,2; KD2=K/2
      IF (IBCELL(I,J,K).GE.1) cycle
      CALL BOUND(FZ(ID2,JD2,KD2),1,3)
      DN(ID2,JD2,KD2,KL)=FZ(ID2,JD2,KD2)
   enddo;enddo;enddo  
   SMXD(1,KL)=RES(NEL,1)
   AVED(1,KL)=RES(NEL,2)
enddo

!---
!        droplet void fraction
!---
DO I=2,MPM2,2;  ID2=I/2
DO J=2,NJY2,2;  JD2=J/2
DO K=2,LKZ2,2;  KD2=K/2
   IF (IBCELL(I,J,K).GE.1) cycle
   ANR=ZERO
   DO L=1,NDP0
      ANR=ANR+DN(ID2,JD2,KD2,L)*RD3(L)
   END DO
   TH_DP(ID2,JD2,KD2)=THET0C*ANR
   CALL BOUND(TH_DP(ID2,JD2,KD2),0,2)
enddo;enddo;enddo  

!----------------------------------------------------------------------
!     Droplet Velocity 
!        DU: (LD=1,NEL=14)
!        DV: (LD=2,NEL=15)
!        DW: (LD=3,NEL=16)
!     Droplet Temperature (LD=4,NEL=17)
!       KL: droplet size index in COMMON block
!----------------------------------------------------------------------
DO L=NDP0,1,-1
   KL=L
DO LD=1,4
   IF (LD.EQ.3.AND.LPD2.LT.3) cycle
   NEL=LD+13
   DO ID2=1,MPD2
   DO JD2=1,NPD2
   DO KD2=1,LPD2
      IF (LD.EQ.4) THEN
         FZ(ID2,JD2,KD2)=DT(ID2,JD2,KD2,KL)
      ELSE
         FZ(ID2,JD2,KD2)=DU(ID2,JD2,KD2,KL,LD)
      ENDIF
   enddo;enddo;enddo  
   CALL SOURCED(MI1,MI3,NJ1,NJ3,LK1,LK3)
   CALL DPFLO3(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
   !         CALL ADLBL3(FZ,KD,2,MZM1,NJD1,NJD3,LKD1,LKD3)
   CALL ADLBL3(FZ,2,MZM1,NJD1,NJD3,LKD1,LKD3)
   !---     
   !     Check bounds of computed properties
   !---     
   DO I=2,MPM2,2;  ID2=I/2
   DO J=2,NJY2,2;  JD2=J/2
   DO K=2,LKZ2,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) cycle
      G0=FZ(ID2,JD2,KD2)
      IF (LD.EQ.4) THEN
         IF (G0.GT.TB(KL)) THEN
            G1=T(I,J,K)-TB(KL)
            G2=G0-TB(KL)
            G3=G2/G1
            IF (G1.GT.1.0D+2) G1=1.0D+2
         ENDIF
         IF (DN(ID2,JD2,KD2,KL).LE.1.0D-20) G0=T(I,J,K)
         CALL BOUND(G0,LD,3)
         IF (DT(ID2,JD2,KD2,KL).GE.TB(KL).AND.G0.LT.TB(KL)) THEN
            G1=DT(ID2,JD2,KD2,KL)-G0
            IF (G1.LE.5.0D-3) cycle
         ENDIF
         DT(ID2,JD2,KD2,KL)=G0
      ELSE
         IF (DN(ID2,JD2,KD2,KL).LE.1.0D-20) G0=UG(I,J,K,LD)
         CALL BOUND(G0,LD,1)
         IF (LD.EQ.1) THEN
            II=ID2+MPD2
            FZ(II,JD2,KD2)=G0
            DU(ID2,JD2,KD2,KL,1)=G0
         ELSEIF (LD.EQ.2) THEN
            IF (LPD2.GE.3) THEN
               JJ=JD2+NPD2
               FZ(ID2,JJ,KD2)=G0
            ELSE
               DU(ID2,JD2,KD2,KL,1)=FZ(ID2+MPD2,JD2,KD2)
               DU(ID2,JD2,KD2,KL,2)=G0
            ENDIF
         ELSEIF (LD.EQ.3) THEN
            DU(ID2,JD2,KD2,KL,1)=FZ(ID2+MPD2,JD2,KD2)
            DU(ID2,JD2,KD2,KL,2)=FZ(ID2,JD2+NPD2,KD2)
            DU(ID2,JD2,KD2,KL,3)=G0
         ENDIF
      ENDIF
   enddo;enddo;enddo  
   SMXD(LD+1,KL)=RES(NEL,1)
   AVED(LD+1,KL)=RES(NEL,2)
enddo;enddo
 
!----------------------------------------------------------------------
!     Boundary Values
!----------------------------------------------------------------------
!CSL      CALL DFBC
!-----------------------------------------------------------------------------------------
!----- SLIP CONDITION FOR HALF BLOCKED CELLS
!      DO 600 I=4,MPM2,2
!      ID2=I/2
!      DO 600 J=NJY1,NJY2,2
!      JD2=J/2
!      DO 600 K=LKZ1,LKZ2,2
!      KD2=K/2
!         IF (IBCELL(I,J,K).NE.-1) GOTO 600
!            DU0=SQRT(DU(ID2,JD2,KD2,NDR0)**2+DV(ID2,JD2,KD2,NDR0)**2)
!            ANG=ATAN(DX(I)/DR(J))
!            ANGDU=ATAN(ABS(DU(ID2,JD2,KD2,NDR0)
!  &                     /(DV(ID2,JD2,KD2,NDR0)+SMALL))) 
!            IF(IBCELL(I+1,J,K).EQ.1.AND.IBCELL(I,J+1,K).EQ.1) THEN
!              IF(DV(ID2,JD2,KD2,NDR0).LT.ZERO.AND.ANGDU.LT.ANG) GO TO 600 
!              DU(ID2,JD2,KD2,NDR0)=DU0*SIN(ANG) 
!              DV(ID2,JD2,KD2,NDR0)=-DU0*COS(ANG) 
!            ENDIF
!            IF(IBCELL(I+1,J,K).EQ.1.AND.IBCELL(I,J-1,K).EQ.1) THEN
!              IF(DV(ID2,JD2,KD2,NDR0).GT.ZERO.AND.ANGDU.LT.ANG) GO TO 600   
!              DU(ID2,JD2,KD2,NDR0)=DU0*SIN(ANG) 
!              DV(ID2,JD2,KD2,NDR0)=DU0*COS(ANG)
!            ENDIF 
!         ENDIF
!600   CONTINUE 
                      
CALL EXTR(2)
CALL DFLOW2
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION NX(3)
NX=0
NX(IU)=M
I1=I+NX(1)
I2=I1+NX(1)
ID21=I/2+NX(1)
!IF (NP.GE.6) THEN
   J1=J+NX(2)
   J2=J1+NX(2)
   JD21=J/2+NX(2)
!ELSE
!   J1=2
!   J2=2
!   JD21=1
!ENDIF
!IF (LP.GE.6) THEN
   K1=K+NX(3)
   K2=K1+NX(3)
   KD21=K/2+NX(3)
!ELSE
!   K1=2
!   K2=2
!   KD21=1
!ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION DFF0(I,J,K,L,IU)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
AREA=AREA_C(I,J,K,IU)
IF (IU.EQ.1) THEN
   DDL=DX(I)
ELSEIF (IU.EQ.2) THEN
   DDL=DR(J)*RS(J)
ELSE
   DDL=DZ(K)
ENDIF
DFF0=GDIFF(I,J,K)/PSI(L)*AREA/DDL 
!CSL      IF (IBCELL(I,J,K).LE.-1) DFF0=DFF0*TWO
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Droplet/Particle Diffusivity
!----------------------------------------------------------------------
SUBROUTINE DPFLO1(SMX0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z) 

DO I=2,MPM2,2;  IP1=I+1;  IP2=I+2
DO J=2,NJY2,2;  JP1=J+1;  JP2=J+2
DO K=2,LKZ2,2
   IF (IBCELL(I,J,K).EQ.1) cycle
   KP1=K+1
   KP2=K+2
   IF (TMU(IP1,J,K).GT.SMX0.AND.IBCELL(IP1,J,K).NE.1) THEN
      G0=TMU(IP2,J,K)*TMU(I,J,K)/TMU(IP1,J,K)*DR(J)*DZ(K)/DX(IP1)
      GDIFF(IP1,J,K)=G0/(DNST(IP1,J,K)*SGM)/REYG2
   ELSE
      GDIFF(IP1,J,K)=ZERO
   ENDIF
   IF (JP2.LE.NP) THEN
      IF (TMU(I,JP1,K).GT.SMX0.AND.IBCELL(I,JP1,K).NE.1) THEN
         G0=TMU(I,JP2,K)*TMU(I,J,K)/TMU(I,JP1,K)*DX(I)*DZ(K)/DR(JP1)
         GDIFF(I,JP1,K)=G0/(DNST(I,JP1,K)*SGM)/REYG2
      ELSE
         GDIFF(I,JP1,K)=ZERO
      ENDIF
   ENDIF
   IF (KP2.LE.LP) THEN
      IF (TMU(I,J,KP1).GT.SMX0.AND.IBCELL(I,J,KP1).NE.1) THEN
         G0=TMU(I,J,KP2)*TMU(I,J,K)/TMU(I,J,KP1)*DX(I)*DR(J)/DZ(KP1)
         GDIFF(I,J,KP1)=G0/(DNST(I,J,KP1)*SGM)/REYG2
      ELSE
         GDIFF(I,J,KP1)=ZERO
      ENDIF
   ENDIF
enddo;enddo;enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Droplet/Particle Number Density
!----------------------------------------------------------------------
SUBROUTINE DPFLO2(MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION AREA(3)

DO I=MI1,MI3,2;  ID2=I/2
DO J=NJ1,NJ3,2;  JD2=J/2
DO K=LK1,LK3,2;  KD2=K/2
   IBC0=IBCELL(I,J,K)
   !--- Non-calculation cells            
   IF (IBC0.GE.1) THEN
      AS(ID2,JD2,KD2,1:3,1:2)=ZERO
      AP(ID2,JD2,KD2)=ONE
      cycle
   ENDIF

   AP(ID2,JD2,KD2)=ZERO
   DFS=ZERO
   AREA(1)=AREA_C(I,J,K,1)
   AREA(2)=AREA_C(I,J,K,2)
   AREA(3)=AREA_C(I,J,K,3)
   DO IU=1,3
      DO M=-1,1,2
         M1=(M+3)/2
         CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
         DU_0=DU(ID2,JD2,KD2,KL,IU)
         DU_1=DU(ID21,JD21,KD21,KL,IU)
         IBC1=IBCELL(I1,J1,K1)
         IF (IBC1.NE.1) THEN
            IF (IBC1.EQ.2) THEN
               DUK=DU_1
            ELSE
               CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
               DUK=F1*DU_1+F2*DU_0
               !CSL                  IF (IU.EQ.1.AND.DU_0*DU_1.LE.1.0D-20) THEN
               !CSL                     DUK=ZERO
               !CSL                  ENDIF
            ENDIF
            IF (IBC1.EQ.2.OR.IBC1.EQ.3) THEN
               DIFF=ZERO
            ELSE
               DIFF=DFF0(I1,J1,K1,KL,IU)
            ENDIF
            FL=DUK*AREA(IU)
            FL1=-FL*M
            FL2=UG(I1,J1,K1,IU)*AREA(IU)
            AS(ID2,JD2,KD2,IU,M1)=DA(DIFF,FL2)+MAX(ZERO,FL1)
         ELSE
            FL=DU_1*AREA(IU)
            FL1=-FL*M
            AS(ID2,JD2,KD2,IU,M1)=ZERO
         ENDIF
         DFS=DFS-FL1
         AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+AS(ID2,JD2,KD2,IU,M1)
      ENDDO
   enddo

   VOL=VOL_C(I,J,K)
   AP0=AP(ID2,JD2,KD2)
   !----------------------
   !            IF (DFS.LT.0) THEN
   !               AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+
   !     &             (TRN/DTM-SFP(ID2,JD2,KD2))*VOL
   !               BTMP=(BS(ID2,JD2,KD2)+DNO(ID2,JD2,KD2,KL)*TRN/DTM)*VOL
   !     &              -DFS*FZ(ID2,JD2,KD2)
   !            ELSE
   AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+DFS+(TRN/DTM-SFP(ID2,JD2,KD2))*VOL
   BTMP=(BS(ID2,JD2,KD2)+DNO(ID2,JD2,KD2,KL)*TRN/DTM)*VOL
   !         ENDIF

   !----------------------------------------------------------------------
   !     Stagnation Point Treatment
   !----------------------------------------------------------------------
   IF (ABS(AP(ID2,JD2,KD2)).LT.1.0D-10) THEN
      AS(ID2,JD2,KD2,1:3,1:2)=ZERO
      BS(ID2,JD2,KD2)=ZERO
      !CSL            BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
      AP(ID2,JD2,KD2)=ONE
   ELSE
      APTMP=AP(ID2,JD2,KD2)
      AP(ID2,JD2,KD2)=APTMP/RF(NEL)
      BS(ID2,JD2,KD2)=BTMP+RFC(NEL)*FZ(ID2,JD2,KD2)*AP(ID2,JD2,KD2)
   ENDIF
enddo;enddo;enddo
RETURN
END
