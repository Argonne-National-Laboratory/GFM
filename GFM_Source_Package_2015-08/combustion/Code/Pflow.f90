! PFLOW.F90
!----------------------------------------------------------------------
!     solves property variables for particle phase
!       DN     number density
!       DU     velocity components
!       DT     temperature
!       DC     coke mass fraction (kg of coke/kg of cat.)
!     Revision: 10/97
!----------------------------------------------------------------------
SUBROUTINE PFLOW
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION SMXD(6,NDNP),AVED(6,NDNP)
DATA SMX0/1.0D-12/
!----------------------------------------------------------------------
!     Particle Number Density
!----------------------------------------------------------------------
!      KD=1
eqtyp=1
SGM=PSIGMA
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
DO L=NPT0,1,-1
   KL=L+NDP0
   DO ID2=1,MPD2
   DO JD2=1,NPD2
   DO KD2=1,LPD2
      FZ(ID2,JD2,KD2)=DN(ID2,JD2,KD2,KL)
   enddo;enddo;enddo
   CALL SOURCEP(MI1,MI3,NJ1,NJ3,LK1,LK3)
   CALL DPFLO2(MI1,MI3,NJ1,NJ3,LK1,LK3)
   CALL ADLBL3(FZ,2,MZM1,NJD1,NJD3,LKD1,LKD3)
   !CALL ADLBL3(FZ,KD,2,MZM1,NJD1,NJD3,LKD1,LKD3)
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
!        Check Particle void fraction
!---
DATA TH_PT1,TH_FAC/0.6D+0,1.0D+2/
DO I=2,MPM2,2;  ID2=I/2
DO J=2,NJY2,2;  JD2=J/2
DO K=2,LKZ2,2;  KD2=K/2
   IF (IBCELL(I,J,K).GE.1) cycle
   ANR=ZERO
   DO L1=1,NPT0
      L=L1+NDP0
      ANR=ANR+DN(ID2,JD2,KD2,L)*RD3(L)
   END DO
   TH_P=THET0C*ANR
   TH_P=MAX(TH_P,ZERO)

   !---  SOLID PRESSURE
   IF (TH_P.GT.TH_PT1.OR.SP(I,J,K).GT.ZERO) THEN
      SP(I,J,K)=SP(I,J,K)+(TH_P-TH_PT1)*TH_FAC
      IF (SP(I,J,K).LE.ZERO.OR.TH_P.LE.ZERO) THEN 
         SP(I,J,K)=ZERO
         GOTO 215
      ENDIF
      IF (TH_P.LE.TH_PT1) GOTO 215 
      DO L1=1,NPT0
         L=L1+NDP0
         DN(ID2,JD2,KD2,L)=DN(ID2,JD2,KD2,L)*TH_PT1/TH_P
         DU(ID2,JD2,KD2,L,1)=DU(ID2,JD2,KD2,L,1)*TH_P/TH_PT1
         DU(ID2,JD2,KD2,L,2)=DU(ID2,JD2,KD2,L,2)*TH_P/TH_PT1
         DU(ID2,JD2,KD2,L,3)=DU(ID2,JD2,KD2,L,3)*TH_P/TH_PT1
      END DO
      TH_P=TH_PT1
   ELSE        
      SP(I,J,K)=ZERO
   ENDIF

215   continue

   THETA(I,J,K)=ONE-TH_DP(ID2,JD2,KD2)-TH_P
   CALL BOUND(THETA(I,J,K),20,2)
   TH_PT(ID2,JD2,KD2)=TH_P
enddo;enddo;enddo
CALL INTPSP

!----------------------------------------------------------------------
!     Particle Velocity 
!        DU: (LD=1,NEL=14)
!        DV: (LD=2,NEL=15)
!        DW: (LD=3,NEL=16)
!     Particle Temperature DT: (LD=4,NEL=17)
!     Coke mass fraction DC: (LD=5,NEL=18)
!       KL: Particle size index in COMMON block
!----------------------------------------------------------------------
DO L=NPT0,1,-1
   KL=L+NDP0
DO LD=1,5
   IF (LD.EQ.3.AND.LPD2.LT.3) cycle
   IF (LD.EQ.5.AND.(FR_Q.LE.ZERO.OR..NOT.REACT)) cycle
   NEL=LD+13
   DO ID2=1,MPD2
   DO JD2=1,NPD2
   DO KD2=1,LPD2
      IF (LD.EQ.5) THEN
         FZ(ID2,JD2,KD2)=DC(ID2,JD2,KD2,L)
      ELSEIF (LD.EQ.4) THEN
         FZ(ID2,JD2,KD2)=DT(ID2,JD2,KD2,KL)
      ELSE
         FZ(ID2,JD2,KD2)=DU(ID2,JD2,KD2,KL,LD)
      ENDIF
   enddo;enddo;enddo
   CALL SOURCEP(MI1,MI3,NJ1,NJ3,LK1,LK3)

   CALL DPFLO3(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
   CALL ADLBL3(FZ,2,MZM1,NJD1,NJD3,LKD1,LKD3)
   !CALL ADLBL3(FZ,KD,2,MZM1,NJD1,NJD3,LKD1,LKD3)
   !---     
   !     Check bounds of computed properties
   !---     
   DO I=2,MPM2,2;  ID2=I/2
   DO J=2,NJY2,2;  JD2=J/2
   DO K=2,LKZ2,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) cycle
      G0=FZ(ID2,JD2,KD2)
      IF (LD.EQ.5) THEN
         CALL BOUND(G0,5,3)
         DC(ID2,JD2,KD2,L)=G0
      ELSEIF (LD.EQ.4) THEN
         IF (DN(ID2,JD2,KD2,KL).LE.1.0D-20) G0=T(I,J,K)
         CALL BOUND(G0,6,3)
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
!----------------------------------------------------------------------

CALL TH_GS

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
                    
CALL EXTR(3)
CALL PFLOW2
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Droplet/Particle Properties
!======================================================================
SUBROUTINE DPFLO3(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION AREA(3)
DO I=MI1,MI3,2;  ID2=I/2
DO J=NJ1,NJ3,2;  JD2=J/2
DO K=LK1,LK3,2;  KD2=K/2
   IBC0=IBCELL(I,J,K)
   !--- Non-calculation cells               
   IF (IBC0.GE.1.OR.DN(ID2,JD2,KD2,KL).LE.DN_MN) THEN
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
         IBC1=IBCELL(I1,J1,K1)
         IF (IBC1.NE.1) THEN
            IF (IBC1.EQ.2) THEN
               DUK=DU(ID21,JD21,KD21,KL,IU)
               DNK=DN(ID21,JD21,KD21,KL)
            ELSE
               CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
               DUK=F1*DU(ID21,JD21,KD21,KL,IU)+F2*DU(ID2,JD2,KD2,KL,IU)
               IF (M*DUK.GE.ZERO) THEN
                  DNK=DN(ID2,JD2,KD2,KL)
               ELSE
                  DNK=DN(ID21,JD21,KD21,KL)
               ENDIF
               !csl                  DNK=F1*DN(ID21,JD21,KD21,KL)+F2*DN(ID2,JD2,KD2,KL)
            ENDIF
            FL=DUK*DNK*AREA(IU)
            FL1=-FL*M
            AS(ID2,JD2,KD2,IU,M1)=MAX(ZERO,FL1)
         ELSE
            FL1=ZERO
            AS(ID2,JD2,KD2,IU,M1)=ZERO
         ENDIF
         DFS=DFS-FL1
         AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+AS(ID2,JD2,KD2,IU,M1)
      ENDDO !m=
   enddo !iu=
   VOL=VOL_C(I,J,K)
   AP0=AP(ID2,JD2,KD2)
   !CSL         AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+(DNO(ID2,JD2,KD2,KL)*TRN/DTM-SFP(ID2,JD2,KD2))*VOL
   !--- COKE  
   IF (LD.EQ.5) THEN
      AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+DFS
   ENDIF

!----------------------------------------------------------------------
!     Stagnation Point Treatment
!----------------------------------------------------------------------
   IF (ABS(AP(ID2,JD2,KD2)).LT.1.0D-10) THEN
      AS(ID2,JD2,KD2,1:3,1:2)=ZERO
      !csl            BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
      IF (LD.EQ.4) THEN
         BS(ID2,JD2,KD2)=T(I,J,K)
      ELSE
         BS(ID2,JD2,KD2)=UG(I,J,K,LD)
      ENDIF
      AP(ID2,JD2,KD2)=ONE
   ELSE
      APTMP=AP(ID2,JD2,KD2)+(DNO(ID2,JD2,KD2,KL)*TRN/DTM-SFP(ID2,JD2,KD2))*VOL
      !csl            APTMP=AP(ID2,JD2,KD2)
      AP(ID2,JD2,KD2)=APTMP/RF(NEL)
      IF (LD.EQ.5) THEN
         G0=DCO(ID2,JD2,KD2,KL)
      ELSEIF (LD.EQ.4) THEN
         G0=DTO(ID2,JD2,KD2,KL)
      ELSE
         G0=DUO(ID2,JD2,KD2,KL,LD)
      ENDIF
      BTMP=(BS(ID2,JD2,KD2)+DNO(ID2,JD2,KD2,KL)*G0*TRN/DTM)*VOL
      BS(ID2,JD2,KD2)=BTMP+RFC(NEL)*FZ(ID2,JD2,KD2)*AP(ID2,JD2,KD2)
   ENDIF 
enddo;enddo;enddo
RETURN
END
