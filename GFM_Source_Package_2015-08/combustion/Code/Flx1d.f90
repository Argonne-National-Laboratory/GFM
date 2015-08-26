!==============================================================================
!==============================================================================
!==============================================================================
!
! Flx1d.f90
!
! Flx1d calculates cross-sectional flow rates
!
! This module contains the following routines:
!     flx1d
!     flx1dm
!
!======================================================================
!======================================================================
!======================================================================
SUBROUTINE FLX1D
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
!     FLY: cross-sectional mass flow rate (kg/s)
!       1,2: total gas (+,-)
!----------------------------------------------------------------------

DO I=3,MPM1,2;   FLX(I,1:2)=0
DO J=2,NP,2
DO K=2,LP,2
   IF (IBCELL(I,J,K).EQ.1.OR.IBCELL(I,J,K).GE.4) CYCLE
   AREA=AREA_C(I,J,K,1)
   G0=THETA(I,J,K)*DNST(I,J,K)*UG(I,J,K,1)*AREA*GMFR
   IF (G0.GE.ZERO) THEN
      FLX(I,1)=FLX(I,1)+G0
   ELSE
      FLX(I,2)=FLX(I,2)-G0
   ENDIF
enddo;enddo;enddo

DO J=3,NPM1,2
   FLY(J,1:2)=ZERO
   FLYH(J,1:2)=ZERO
   DO I=4,MPM2,2
   DO K=LKZ1,LKZ2,2
      IF (IBCELL(I,J,K).EQ.1.OR.IBCELL(I,J,K).GE.4) cycle
      AREA=AREA_C(I,J,K,2)
      G0=THETA(I,J,K)*DNST(I,J,K)*UG(I,J,K,2)*AREA*GMFR
      FLYH0=G0*GF(I,J,K,IH)*H_0
      IF (G0.GE.ZERO) THEN
         FLY(J,1)=FLY(J,1)+G0
         FLYH(J,1)=FLYH(J,1)+FLYH0
      ELSE
         FLY(J,2)=FLY(J,2)-G0
         FLYH(J,2)=FLYH(J,2)-FLYH0
      ENDIF
   enddo;enddo
enddo

DO K=3,LPM1,2
   FLUX_Z(K,1:2)=ZERO
   FLUXH_Z(K,1:2)=ZERO
   DO I=4,MPM2,2
   DO J=NJY1,NJY2,2
      IF (IBCELL(I,J,K).EQ.1.OR.IBCELL(I,J,K).GE.4) cycle
      AREA=AREA_C(I,J,K,3)
      G0=THETA(I,J,K)*DNST(I,J,K)*UG(I,J,K,3)*AREA*GMFR
      FLUXH0=G0*GF(I,J,K,IH)*H_0
      IF (G0.GE.ZERO) THEN
         FLUX_Z(K,1)=FLUX_Z(K,1)+G0
         FLUXH_Z(K,1)=FLUXH_Z(K,1)+FLUXH0
      ELSE
         FLUX_Z(K,2)=FLUX_Z(K,2)-G0
         FLUXH_Z(K,2)=FLUXH_Z(K,2)-FLUXH0
      ENDIF
   enddo;enddo
enddo

IF (NPHAS.EQ.1) THEN
   GFIN=GFIN0*GMFR
   return
ENDIF

!----------------------------------------------------------------------
!     GDVAP: total evaporation
!     GDCON: total condensation
!     MAR: total fluid volume change
!----------------------------------------------------------------------
GDVAP=ZERO
GDCON=ZERO
MAR=ZERO
DO I=4,MPM2,2;        ID2=I/2
   CFLX=ZERO   
   DO J=NJY1,NJY2,2;  JD2=J/2
   DO K=LKZ1,LKZ2,2
      IF (IBCELL(I,J,K).GE.1) cycle
      KD2=K/2
      VOL=VOL_C(I,J,K)
      IF (VOL.LE.ZERO) cycle
      IF (IBCELL(I+2,J,K).EQ.3.OR.IBCELL(I-2,J,K).EQ.3) cycle
      IF (NP.GE.6.AND.(IBCELL(I,J+2,K).EQ.3.OR.IBCELL(I,J-2,K).EQ.3)) cycle
      IF (LP.GE.6.AND.(IBCELL(I,J,K+2).EQ.3.OR.IBCELL(I,J,K-2).EQ.3)) cycle
      GDVAP=GDVAP+(EVP(ID2,JD2,KD2)-CON(ID2,JD2,KD2))*VOL
      GDCON=GDCON+CON(ID2,JD2,KD2)*VOL
!      IF (STEADY) cycle
!      MAR=MAR+(THETA(I,J,K)*DNST(I,J,K)-THETAO(I,J,K)*DNSTO(I,J,K))*VOL
   enddo;enddo
enddo
MAR=MAR*TRN/DTM
GFIN=(GFIN0+GDVAP)*GMFR
return
END


!======================================================================
!======================================================================
!======================================================================
subroutine flx1dm
use gbl_var
implicit double precision (a-h,o-z)
!----------------------------------------------------------------------
!     subspecies flow rates
!     flym: cross-sectional mass flow rate (kg/s)
!       1,2: (+,-)
!----------------------------------------------------------------------

flxm=zero
do i=3,mpm1,2
do j=4,npm2,2
do k=lkz1,lkz2,2
   if (ibcell(i,j,k).eq.1) cycle
   area=area_c(i,j,k,1)
   g0=theta(i,j,k)*dnst(i,j,k)*ug(i,j,k,1)*area*gmfr
   if (g0.ge.zero) then
      id=1
   else
      id=2
   endif
   do l=1,nsp0
      flxm(i,l,id)=flxm(i,l,id)+abs(g0)*gfm(i,j,k,l)
   enddo
enddo;enddo;enddo

flym=zero
do j=3,npm1,2
do i=4,mpm2,2
do k=lkz1,lkz2,2
   if (ibcell(i,j,k).eq.1) cycle
   area=area_c(i,j,k,2)
   g0=theta(i,j,k)*dnst(i,j,k)*ug(i,j,k,2)*area*gmfr
   if (g0.ge.zero) then
      id=1
   else
      id=2
   endif
   do l=1,nsp0
      flym(j,l,id)=flym(j,l,id)+abs(g0)*gfm(i,j,k,l)
   enddo
enddo;enddo;enddo
return
end

