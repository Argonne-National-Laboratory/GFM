! GSLV3V.F90
!======================================================================
!     GSLV3V solves flow property states by using
!        the simple or the simpler procedures
!     revision:  10/97
!======================================================================
SUBROUTINE GSLV3V
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION NX(3)
!      KD=2
eqtyp=2
SGM=ONE
!----------------------------------------------------------------------
!     Gaseous phase pseudovelocities and flux coefficients
!     IU1=1  X-DIRECTION 
!         2  Y or R-DIRECTION 
!         3  Z-DIRECTION 
!     AS(I,J,K,L,M)
!         (I,J,K) node index
!         L=1,2,3: direction index
!         M=1,2: negative or positive direction
!----------------------------------------------------------------------
AP=ONE
AS=ZERO
DO IU1=1,3
   NX=0
   NX(IU1)=1
   DO I=2+NX(1),MP-NX(1),2
   DO J=2+NX(2),NP-NX(2),2
   DO K=2+NX(3),LP-NX(3),2
      FZ(I,J,K)=UG(I,J,K,IU1)
   enddo;enddo;enddo
enddo  
DO IU1=1,3    
   NELG=IU1
   CALL GSLVX1(MI1,MI3,NJ1,NJ3,LK1,LK3,IU1)
enddo

!----------------------------------------------------------------------
!-----GAS PRESSURE
!----------------------------------------------------------------------
NELG=4 
MI1=4
MI3=MPM2
NJ1=NJY1
NJ3=NJY2
LK1=LKZ1
LK3=LKZ2
CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) THEN
      BS(I,J,K)=P(I,J,K)
      cycle
   ENDIF
   AP(I,J,K)=-SFP(I,J,K)
   I1=I-1
   DO M=1,2
      IF (M.EQ.2) I1=I+1
      IBC1=IBCELL(I1,J,K)
      IF (IBC1.LE.0.AND.ABS(AP(I1,J,K)).GT.small20) THEN
         AREA=AREA_C(I,J,K,1)
         AS(I,J,K,1,M)=THETA(I1,J,K)**2*DNST(I1,J,K)*AREA**2/AP(I1,J,K)*DXAV(I1,J,K)/DX(I1)
      ENDIF
      AP(I,J,K)=AP(I,J,K)+AS(I,J,K,1,M)
   ENDDO
   J1=J-1
   DO M=1,2
      IF (M.EQ.2) J1=J+1
      IBC1=IBCELL(I,J1,K)
      IF (IBC1.LE.0.AND.ABS(AP(I,J1,K)).GT.small20) THEN
         AREA=AREA_C(I,J,K,2)
         AS(I,J,K,2,M)=THETA(I,J1,K)**2*DNST(I,J1,K)*AREA**2/AP(I,J1,K)*DRAV(I,J1,K)/DR(J1)
      ENDIF
      AP(I,J,K)=AP(I,J,K)+AS(I,J,K,2,M)
   ENDDO
   K1=K-1
   DO M=1,2
      IF (M.EQ.2) K1=K+1
      IBC1=IBCELL(I,J,K1)
      IF (IBC1.LE.0.AND.ABS(AP(I,J,K1)).GT.small20) THEN
         AREA=AREA_C(I,J,K,3)
         AS(I,J,K,3,M)=THETA(I,J,K1)**2*DNST(I,J,K1)*AREA**2/AP(I,J,K1)*DZAV(I,J,K1)/DZ(K1)
      ENDIF
      AP(I,J,K)=AP(I,J,K)+AS(I,J,K,3,M)
   ENDDO
enddo;enddo;enddo

call calc_residual(p,p_resid_pre,mi1,mi3,nj1,nj3,lk1,lk3)
!CALL ADLBL3(P,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
CALL ADLBL3(P,MI1,MI3,NJ1,NJ3,LK1,LK3)
call calc_residual(p,p_resid,mi1,mi3,nj1,nj3,lk1,lk3)

DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) cycle
   CALL BOUND(P(I,J,K),0,1)
enddo;enddo;enddo

!----------------------------------------------------------------------
!     Gaseous phase starred velocity
!----------------------------------------------------------------------
DO IU1=1,3
   NELG=IU1 
   NX=0
   NX(IU1)=1
   MI1=4+NX(1)
   MI3=MPM2-NX(1)
   NJ1=NJY1V(IU1)
   NJ3=NJY2V(IU1)
   LK1=LKZ1V(IU1)
   LK3=LKZ2V(IU1)
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      AP(I,J,K)=AP(I,J,K)/RF(NELG)
      VOL=VOL_C(I,J,K)
      IF (IU1.EQ.1) THEN
         DPDX=(P(I-1,J,K)-P(I+1,J,K))/DX(I)
      ELSEIF (IU1.EQ.2) THEN
         DPDX=(P(I,J-1,K)-P(I,J+1,K))/DR(J)/RS(J)
      ELSE
         DPDX=(P(I,J,K-1)-P(I,J,K+1))/DZ(K)
      ENDIF
      BS(I,J,K)=BS(I,J,K)+EUN*THETA(I,J,K)*VOL*DPDX+RFC(NELG)*AP(I,J,K)*FZ(I,J,K)
   enddo;enddo;enddo

   call calc_residual(fz,u_resid_pre(iu1),mi1,mi3,nj1,nj3,lk1,lk3)
   CALL ADLBL3(FZ,MI1,MI3,NJ1,NJ3,LK1,LK3)
   !CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
   call calc_residual(fz,u_resid(iu1),mi1,mi3,nj1,nj3,lk1,lk3)

   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      CALL BOUND(FZ(I,J,K),IU1,1)
      UG(I,J,K,IU1)=FZ(I,J,K)
   enddo;enddo;enddo
enddo   

!----------------------------------------------------------------------
!-----PRESSURE CORRECTION EQUATION
!----------------------------------------------------------------------
NELG=5
MI1=4
MI3=MPM2
NJ1=NJY1
NJ3=NJY2
LK1=LKZ1
LK3=LKZ2
CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   FZ(I,J,K)=ZERO
enddo;enddo;enddo

CALL ADLBL3(FZ,MI1,MI3,NJ1,NJ3,LK1,LK3)
call calc_residual(fz,pcorr_resid,mi1,mi3,nj1,nj3,lk1,lk3)

!CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
!-----CORRECT PRESSURE IF USING SIMPLE
!IF (.NOT.SMPLER) THEN
!   DO I=2,MP,2
!   DO J=2,NP,2
!   DO K=2,LP,2
!      P(I,J,K)=P(I,J,K)+RF(20)*FZ(I,J,K)
!   enddo;enddo;enddo
!ENDIF
 
!----------------------------------------------------------------------
!-----VELOCITY MODIFICATION BY CORRECTION PRESSURE P'
!----------------------------------------------------------------------
DO IU1=1,3
   NX=0
   NX(IU1)=1
   MI1=4+NX(1)
   MI3=MPM2-NX(1)
   NJ1=NJY1V(IU1)
   NJ3=NJY2V(IU1)
   LK1=LKZ1V(IU1)
   LK3=LKZ2V(IU1)
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2 
      IF (IBCELL(I,J,K).GE.1.OR.ABS(AP(I,J,K)).LT.small20) cycle
      VOL=VOL_C(I,J,K)
      IF (IU1.EQ.1) THEN
         DPDX=(FZ(I-1,J,K)-FZ(I+1,J,K))/DX(I)
      ELSEIF (IU1.EQ.2) THEN
         DPDX=(FZ(I,J-1,K)-FZ(I,J+1,K))/DR(J)/RS(J)
      ELSE
         DPDX=(FZ(I,J,K-1)-FZ(I,J,K+1))/DZ(K)
      ENDIF
      G0=EUN*THETA(I,J,K)*DPDX*VOL/(AP(I,J,K)*RF(IU1))      
      UG(I,J,K,IU1)=UG(I,J,K,IU1)+G0
      CALL BOUND(UG(I,J,K,IU1),IU1,1)
   enddo;enddo;enddo
enddo

!----------------------------------------------------------------------
!     Velocity extrapolation and interpolation
!----------------------------------------------------------------------
!csl      GFEX=ZERO
CALL EXTRV(0)
CALL FLX1D

if (itr_gas > 10) then
   !cbg  fadj=fadj*0.1D+0
   IF (GFEX.LT.small20) THEN
      FACTOR=ONE
      FADJ=ZERO
   ELSE
      FACTOR=GFIN/GFEX
      !csl         FADJ=FADJ+(FACTOR-ONE)*FADJ0
      !csl         FACTOR=FACTOR+FADJ
      !CSL         FACTOR=ONE+FADJ
      IF (FACTOR.GT.F_MX) THEN
         FACTOR=F_MX
         FADJ=ZERO
      ELSEIF (FACTOR.LT.F_MN) THEN
         FACTOR=F_MN
         FADJ=ZERO
      ENDIF
   ENDIF
endif

CALL EXTRV(1)
CALL INTPV
RETURN
END
