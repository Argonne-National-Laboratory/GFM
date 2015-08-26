!==============================================================================
!==============================================================================
!==============================================================================
!
! Extr.f90
!
! Extr extrapolates calculated variables 
!
! This module contains the following routines:
!     extr(iphase_type)
!     extrm
!     extr1(f1,f2,i,j,k,i1,j1,k1,i2,j2,k2,ix)
!     extrv(mex)
!     ext0(i,j,k,i1,j1,k1,i2,j2,k2,ix)
!     ext1(i,j,k,i1,j1,k1,i2,j2,k2,ix)
!
!======================================================================
!======================================================================
!======================================================================
!     Extrapolation of calculated variables 
!       9/97
!
!     iphase_type=1: gas phase variables 
!                 2: droplet variables 
!                 3: particle variables 
!     IBCELL: node
!                 1: Solid 
!                 2: Inlet 
!                 3: Exit
!======================================================================
SUBROUTINE EXTR(iphase_type)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
integer iphase_type

IF (iphase_type.EQ.1) THEN
   GFIN0=ZERO
   !H_EX=ZERO
   q_exhaust=zero
ENDIF

DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IBC0=MOD(IBCELL(I,J,K),10)
   IF (IBC0.NE.2.AND.IBC0.NE.3) cycle !not inlet and not outlet: cycle
   IF (IBC0.EQ.2.AND.iphase_type.NE.1) cycle !inlet and not gas phase variables
   CALL EXT0(I,J,K,I1,J1,K1,I2,J2,K2,IX)
   CALL EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,IX)
   IF (iphase_type.EQ.1) THEN
      GF(I,J,K,IEPS)=GF(I1,J1,K1,IEPS)
      P(I,J,K)=P(I1,J1,K1)*F1+P(I2,J2,K2)*F2
      IF (IBC0.EQ.2) THEN !this stuff gets extrapolated to inlets
         !csl               CALL WTDN(I,J,K,WTOJ)
         !csl               CALL dnst_calc(I,J,K) ! should this be commented out?
         G0=TMU(I,J,K)
         CALL THER(I,J,K)
         TMU(I,J,K)=G0
         GAMA(I,J,K)=GAMA(I1,J1,K1)
         AREA=AREA_C(I,J,K,IX)
         G0=THETA(I,J,K)*DNST(I,J,K)*UG(I,J,K,IX)*AREA
         GFIN0=GFIN0+ABS(G0)
         cycle !processing for inlets ends here
      ENDIF

      !-------------------------
      !Only get here for outlets

      GCP(I,J,K)=GCP(I1,J1,K1)*F1+GCP(I2,J2,K2)*F2
      GLAM(I,J,K)=GLAM(I1,J1,K1)*F1+GLAM(I2,J2,K2)*F2
      TMU(I,J,K)=TMU(I1,J1,K1)
      GAMA(I,J,K)=GAMA(I1,J1,K1)
      DO L=LSTAR,LEND
         IF (L.EQ.IEPS) cycle
         G0=GF(I1,J1,K1,L)*F1+GF(I2,J2,K2,L)*F2
         IF (L.EQ.IK) THEN
            GF(I,J,K,IK)=MAX(G0,1.0D-6)
         ELSEIF (L.EQ.IH) THEN
            GF(I,J,K,L)=G0
         ELSE
            GF(I,J,K,L)=MAX(G0,ZERO)
         ENDIF
      enddo
      call enth_to_T(I,J,K)
      !T(I,J,K)=T(I1,J1,K1)*F1+T(I2,J2,K2)*F2
      !should not extrapolate both enthalpy and temperature 
      !CALL WTDN(I,J,K,WTOJ) 
      call dnst_calc(i,j,k)
      AREA=AREA_C(I,J,K,IX)*AREA0
      G0=ABS(UG(I,J,K,IX))*UG0*AREA
      G0=G0*THETA(I,J,K)*DNST(I,J,K)*DNST0
      q_exhaust=q_exhaust+gf(i,j,k,ih)*h_0*g0
      !H_EX=q_exhaust
      IF (IBC0.EQ.3) THEN
         I3=(I+I1)/2
         J3=(J+J1)/2
         K3=(K+K1)/2
         CALL SBC4(I,J,K,I3,J3,K3)
      ENDIF
      IF (NPHAS.EQ.1) cycle
      THETA(I,J,K)=THETA(I1,J1,K1)*F1+THETA(I2,J2,K2)*F2
      ID2=I/2
      JD2=J/2
      KD2=K/2
      I1=I1/2
      J1=J1/2
      K1=K1/2
      I2=I2/2
      J2=J2/2
      K2=K2/2
      EVP(ID2,JD2,KD2)=ZERO
      CON(ID2,JD2,KD2)=ZERO

   ELSEIF (iphase_type.EQ.2) THEN
      ID2=I/2
      JD2=J/2
      KD2=K/2
      I1=I1/2
      J1=J1/2
      K1=K1/2
      I2=I2/2
      J2=J2/2
      K2=K2/2
      DO L=1,NDP0
         DO IU=1,3
            DU(ID2,JD2,KD2,L,IU)=DU(I1,J1,K1,L,IU)*F1+DU(I2,J2,K2,L,IU)*F2
         ENDDO
         DT(ID2,JD2,KD2,L)=DT(I1,J1,K1,L)*F1+DT(I2,J2,K2,L)*F2
         G0=DN(I1,J1,K1,L)*F1+DN(I2,J2,K2,L)*F2
         IF ((I1.GT.I2.AND.DU(I1,J1,K1,L,1).LE.ZERO).OR.(I1.LT.I2.AND.DU(I1,J1,K1,L,1).GE.ZERO)) THEN
            G0=ZERO
            DU(ID2,JD2,KD2,L,1)=ZERO
         ENDIF
         DN(ID2,JD2,KD2,L)=MAX(G0,ZERO)
      ENDDO
      G0=TH_DP(I1,J1,K1)*F1+TH_DP(I2,J2,K2)*F2
      TH_DP(ID2,JD2,KD2)=MAX(G0,ZERO)

   ELSEIF (iphase_type.EQ.3) THEN
      ID2=I/2
      JD2=J/2
      KD2=K/2
      I1=I1/2
      J1=J1/2
      K1=K1/2
      I2=I2/2
      J2=J2/2
      K2=K2/2
      DO L1=1,NPT0
         L=L1+NDP0
         DO IU=1,3
            DU(ID2,JD2,KD2,L,IU)=DU(I1,J1,K1,L,IU)*F1+DU(I2,J2,K2,L,IU)*F2
         ENDDO
         DT(ID2,JD2,KD2,L)=DT(I1,J1,K1,L)*F1+DT(I2,J2,K2,L)*F2
         DC(ID2,JD2,KD2,L1)=DC(I1,J1,K1,L1)*F1+DC(I2,J2,K2,L1)*F2
         G0=DN(I1,J1,K1,L)*F1+DN(I2,J2,K2,L)*F2
         IF ((I1.GT.I2.AND.DU(I1,J1,K1,L,1).LE.ZERO).OR.(I1.LT.I2.AND.DU(I1,J1,K1,L,1).GE.ZERO)) THEN
            G0=ZERO
            DU(ID2,JD2,KD2,L,1)=ZERO
         ENDIF
         DN(ID2,JD2,KD2,L)=MAX(G0,ZERO)
      ENDDO
      G0=TH_PT(I1,J1,K1)*F1+TH_PT(I2,J2,K2)*F2
      TH_PT(ID2,JD2,KD2)=MAX(G0,ZERO)
   ENDIF
enddo;enddo;enddo


DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IBC0=MOD(IBCELL(I,J,K),10)
   IF (IBC0.NE.1) cycle !skip if not a wall
   CALL EXT1(I,J,K,I1,J1,K1,I2,J2,K2,IX)
   CALL EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,IX)
   IF (iphase_type.EQ.1) THEN
      P(I,J,K)=P(I1,J1,K1)*F1+P(I2,J2,K2)*F2
      GCP(I,J,K)=GCP(I1,J1,K1)*F1+GCP(I2,J2,K2)*F2
      GLAM(I,J,K)=GLAM(I1,J1,K1)*F1+GLAM(I2,J2,K2)*F2
      TMU(I,J,K)=TMU(I2,J2,K2)
      GAMA(I,J,K)=GAMA(I2,J2,K2)
      !CSL            DO L=1,9
      DO L=LSTAR,LEND
         if(l.ne.ih) GF(I,J,K,L)=GF(I1,J1,K1,L)
      ENDDO
      ! 6/29/05: this is a zero temperature gradient condition at walls
      !and it is not appropriate for furnaces with heat loss through walls.
      !T(I,J,K)=T(I1,J1,K1)
      !CALL WTDN(I,J,K,WTOJ)
      !Walls are taken to be adiabatic until 1st radiation calculation is done.
      !This should actually be replaced by doing a heat balance on the wall
      !To get T at the wall based on h.t. through the wall in absence of 
      !radiation effects. 
      if (i1st_rad_done==0) then
         GF(I,J,K,ih)=GF(I1,J1,K1,ih)
         T(I,J,K)=T(I1,J1,K1)
      endif        
      call dnst_calc(i,j,k)
    
      IF (NPHAS.EQ.1) cycle
      THETA(I,J,K)=THETA(I1,J1,K1)
      ID2=I/2
      JD2=J/2
      KD2=K/2
      I1=I1/2
      J1=J1/2
      K1=K1/2
      I2=I2/2
      J2=J2/2
      K2=K2/2
      EVP(ID2,JD2,KD2)=EVP(I1,J1,K1)
      CON(ID2,JD2,KD2)=CON(I1,J1,K1)

   ELSEIF (iphase_type.EQ.2) THEN
      ID2=I/2
      JD2=J/2
      KD2=K/2
      I1=I1/2
      J1=J1/2
      K1=K1/2
      I2=I2/2
      J2=J2/2
      K2=K2/2
      DO L=1,NDP0
         DN(ID2,JD2,KD2,L)=DN(I1,J1,K1,L)
         DT(ID2,JD2,KD2,L)=DT(I1,J1,K1,L)
      ENDDO
      TH_DP(ID2,JD2,KD2)=TH_DP(I1,J1,K1)

   ELSEIF (iphase_type.EQ.3) THEN
      ID2=I/2
      JD2=J/2
      KD2=K/2
      I1=I1/2
      J1=J1/2
      K1=K1/2
      I2=I2/2
      J2=J2/2
      K2=K2/2
      DO L1=1,NPT0
         L=L1+NDP0
         DN(ID2,JD2,KD2,L)=DN(I1,J1,K1,L)
         DT(ID2,JD2,KD2,L)=DT(I1,J1,K1,L)
         DC(ID2,JD2,KD2,L1)=DC(I1,J1,K1,L1)
      ENDDO
      TH_PT(ID2,JD2,KD2)=TH_PT(I1,J1,K1)
   ENDIF
enddo;enddo;enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
! Extrapolation for subspecies concentrations 
!======================================================================
subroutine extrm
use gbl_var
implicit double precision (a-h,o-z)

do i=2,mp,2
do j=2,np,2
do k=2,lp,2
   ibc0=mod(ibcell(i,j,k),10)
   if (ibc0.ne.3) cycle !skip if not an exit
   call ext0(i,j,k,i1,j1,k1,i2,j2,k2,ix)
   call extr1(f1,f2,i,j,k,i1,j1,k1,i2,j2,k2,ix)

   do L=lstarm,lendm
      g0=gfm(i1,j1,k1,L)*f1+gfm(i2,j2,k2,L)*f2
      gfm(i,j,k,L)=max(g0,zero)
      jj=(j+j1)/2
      gfm(i,jj,k,L)=gfm(i,j,k,L)
   enddo
   smf(i,j,k)=smf(i1,j1,k1) !soot mass fraction
enddo;enddo;enddo


do i=2,mp,2
do j=2,np,2
do k=2,lp,2
   ibc0=mod(ibcell(i,j,k),10)
   if (ibc0.ne.1) cycle !skip if not a wall
   call ext1(i,j,k,i1,j1,k1,i2,j2,k2,ix)
   call extr1(f1,f2,i,j,k,i1,j1,k1,i2,j2,k2,ix)

   do L=lstarm,lendm
      gfm(i,j,k,L)=gfm(i1,j1,k1,L) !copy near wall value into wall interior
      !copy near wall value to wall face (zero flux condition)
      if (ix == 1) then ! x-normal wall
         ii=(i+i1)/2
         gfm(ii,j,k,L)=gfm(i,j,k,L)
      elseif (ix == 2) then ! y-normal wall
         jj=(j+j1)/2
         gfm(i,jj,k,L)=gfm(i,j,k,L)
      else                  ! z-normal wall
         kk=(k+k1)/2
         gfm(i,j,kk,L)=gfm(i,j,k,L)
      endif
   enddo
enddo;enddo;enddo

return
end


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,IX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

F2=ZERO
IBC0=MOD(IBCELL(I,J,K),10)
IF (IX.EQ.3) THEN
   DZK=Z(K2)-Z(K1)
   IF (DZK.NE.ZERO) THEN
      IF (IBC0.EQ.2) THEN
         K0=(K+K1)/2
      ELSE
         K0=K
      ENDIF
      F2=(Z(K0)-Z(K1))/DZK
   ENDIF
ELSEIF (IX.EQ.2) THEN
   DRJ=R(J2)-R(J1)
   IF (DRJ.NE.ZERO) THEN
      IF (IBC0.EQ.2) THEN
         J0=(J+J1)/2
      ELSE
         J0=J
      ENDIF
      F2=(R(J0)-R(J1))/DRJ
   ENDIF
ELSEIF (IX.EQ.1) THEN
   DXI=X(I2)-X(I1)
   IF (DXI.NE.ZERO) THEN
      IF (IBC0.EQ.2) THEN
         I0=(I+I1)/2
      ELSE
         I0=I
      ENDIF
      F2=(X(I0)-X(I1))/DXI
   ENDIF
ENDIF
F1=ONE-F2
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Extrapolation of exit velocity 
!======================================================================
SUBROUTINE EXTRV(MEX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!---------------------------------------------------------------------
!     IBCELL: node
!          3: Exit 
!       IU=1: axial direction 
!---------------------------------------------------------------------

if (mex==0) then
   gfex=zero
   fuel_out=zero
   Tmean_ex=zero
   Tarea_mean=zero
   area_ex=zero
endif

IU=1 !X-direction
DO I=3,MPM1,2
DO J=2,NP,2
DO K=2,LP,2
   IBC0=MOD(IBCELL(I,J,K),10)
   IF (IBC0.NE.3) cycle
   DO M=-1,1,2
      IF (IBCELL(I+M,J,K).GE.1) cycle
      I0=I-M
      I1=I+2*M
      IF (IBCELL(I+3*M,J,K).LE.0) THEN
         I2=I+4*M
      ELSE
         I2=I1
      ENDIF
      !            CALL EXTR1(F1,F2,I,J,K,I1,J,K,I2,J,K,IU)
      !CSL            DATA U_EX_M /0.0D-1/
      IF (UG(I1,J,K,IU)*M.GT.ZERO) THEN
         !CSL           UG(I1,J,K,IU)=U_EX_M*M
         UG(I1,J,K,IU)=ZERO
      ENDIF
      AREA=AREA_C(I,J,K,IU)
      area_ex=area_ex+area
      G0=THETA(I1,J,K)*DNST(I1,J,K)*UG(I1,J,K,IU)*AREA !exit mass flow
      IF (MEX.EQ.0) THEN
         GFEX=GFEX+ABS(G0)
         !H_IN_OUT=H_IN_OUT+GF(I,J,K,IYF)*G0*DNST0*UG0*AREA0*Q0*H_0
         fuel_out = fuel_out+GF(I,J,K,IYF)*abs(G0)*DNST0*UG0*AREA0
         Tmean_ex = Tmean_ex + T(i,j,k)*abs(g0)
         Tarea_mean = Tarea_mean + T(i,j,k)*area
         GOTO 120
      ENDIF
      UG(I1,J,K,IU)=UG(I1,J,K,IU)*FACTOR
      G0=G0*FACTOR
      if (ndnp .ne. 0) then
         II=(I+I1)/2
         VOL=VOL_C(II,J,K)
         ID2=II/2
         JD2=J/2
         KD2=K/2
         G0=G0+(EVP(ID2,JD2,KD2)-CON(ID2,JD2,KD2))*VOL
      endif
      UG(I,J,K,IU)=G0/(THETA(I,J,K)*DNST(I,J,K)*AREA)
      CALL BOUND(UG(I,J,K,IU),IU,1)
      UG(I0,J,K,IU)=UG(I,J,K,IU)
      GOTO 120
   enddo !m=
   IF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J-2,K,IU)
   ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J+2,K,IU)
   ELSEIF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J,K-2,IU)
   ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J,K+2,IU)
   ENDIF
   120   CONTINUE
enddo;enddo;enddo


IU=2 !Y-direction
DO J=3,NPM1,2
DO K=2,LP,2
DO I=2,MP,2
   IBC0=MOD(IBCELL(I,J,K),10)
   IF (IBC0.NE.3) cycle
   DO M=-1,1,2
      IF (IBCELL(I,J+M,K).GE.1) cycle
      J0=J-M
      J1=J+2*M
      IF (IBCELL(I,J+3*M,K).LE.0) THEN
         J2=J+4*M
      ELSE
         J2=J1
      ENDIF
      !            CALL EXTR1(F1,F2,I,J,K,I,J1,K,I,J2,K,IU)
      IF (UG(I,J1,K,IU)*M.GT.ZERO) THEN
         !CSL           UG(I,J1,K,IU)=U_EX_M*M
         UG(I,J1,K,IU)=ZERO
      ENDIF
      AREA=AREA_C(I,J,K,IU)
      area_ex=area_ex+area
      G0=THETA(I,J1,K)*DNST(I,J1,K)*UG(I,J1,K,IU)*AREA
      IF (MEX.EQ.0) THEN
         GFEX=GFEX+ABS(G0)
         fuel_out = fuel_out+GF(I,J,K,IYF)*abs(G0)*DNST0*UG0*AREA0
         !H_IN_OUT=H_IN_OUT+GF(I,J,K,IYF)*ABS(G0)*DNST0*UG0*AREA0*Q0*H_0
         !CBG         H_IN_OUT=H_IN_OUT+GF(I,J,K,IH)*H_0*G0
         Tmean_ex = Tmean_ex + T(i,j,k)*abs(g0)
         Tarea_mean = Tarea_mean + T(i,j,k)*area
         GOTO 140
      ENDIF
      UG(I,J1,K,IU)=UG(I,J1,K,IU)*FACTOR
      G0=G0*FACTOR
      if (ndnp.ne.0) then
         JJ=(J+J1)/2
         VOL=VOL_C(I,JJ,K)
         ID2=I/2
         JD2=JJ/2
         KD2=K/2
         G0=G0+(EVP(ID2,JD2,KD2)-CON(ID2,JD2,KD2))*VOL
      endif
      UG(I,J,K,IU)=G0/(THETA(I,J,K)*DNST(I,J,K)*AREA)
      CALL BOUND(UG(i,j,k,iu),iu,1)
      UG(I,J0,K,IU)=UG(I,J,K,IU)
      GOTO 140
   enddo !m=
   IF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J,K-2,IU)
   ELSEIF (K.LT.LP.AND.IBCELL(I,J,K+2).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J,K+2,IU)
   ELSEIF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I-2,J,K,IU)
   ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I+2,J,K,IU)
   ENDIF
   140   CONTINUE
enddo;enddo;enddo


IU=3 !Z-direction
DO K=3,LPM1,2
DO I=2,MP,2
DO J=2,NP,2
   IBC0=MOD(IBCELL(I,J,K),10)
   IF (IBC0.NE.3) cycle
   DO M=-1,1,2
      IF (IBCELL(I,J,K+M).GE.1) cycle
      K0=K-M
      K1=K+2*M
      IF (IBCELL(I,J,K+3*M).LE.0) THEN
         K2=K+4*M
      ELSE
         K2=K1
      ENDIF
      !            CALL EXTR1(F1,F2,I,J,K,I,J,K1,I,J,K2,IU)
      IF (UG(I,J,K1,IU)*M.GT.ZERO) THEN
         UG(I,J,K1,IU)=ZERO
         !CSL           UG(I,J,K1,IU)=U_EX_M*M
      ENDIF
      AREA=AREA_C(I,J,K,IU)
      area_ex=area_ex+area
      G0=THETA(I,J,K1)*DNST(I,J,K1)*UG(I,J,K1,IU)*AREA
      IF (MEX.EQ.0) THEN
         GFEX=GFEX+ABS(G0)
         fuel_out = fuel_out+GF(I,J,K,IYF)*abs(G0)*DNST0*UG0*AREA0
         !H_IN_OUT=H_IN_OUT+GF(I,J,K,IYF)*G0*DNST0*UG0*AREA0*Q0*H_0
         !CBG          H_IN_OUT=H_IN_OUT+GF(I,J,K,IH)*H_0*G0
         Tmean_ex = Tmean_ex + T(i,j,k)*abs(g0)
         Tarea_mean = Tarea_mean + T(i,j,k)*area
         GOTO 160
      ENDIF
      UG(I,J,K1,IU)=UG(I,J,K1,IU)*FACTOR
      G0=G0*FACTOR
      GOTO 160 !determine why the following code is skipped
      if (ndnp.ne.0) then
         KK=(K+K1)/2
         VOL=VOL_C(I,J,KK)
         ID2=I/2
         JD2=J/2
         KD2=KK/2
         G0=G0+(EVP(ID2,JD2,KD2)-CON(ID2,JD2,KD2))*VOL
      endif
      UG(I,J,K,IU)=G0/(THETA(I,J,K)*DNST(I,J,K)*AREA)
      UG(I,J,K0,IU)=UG(I,J,K,IU)
   enddo !m=     
   IF (I.GT.2.AND.IBCELL(I-2,J,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I-2,J,K,IU)
   ELSEIF (I.LT.MP.AND.IBCELL(I+2,J,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I+2,J,K,IU)
   ELSEIF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J-2,K,IU)
   ELSEIF (J.LT.NP.AND.IBCELL(I,J+2,K).LE.0) THEN
      UG(I,J,K,IU)=UG(I,J+2,K,IU)
   ENDIF
   160   CONTINUE
enddo;enddo;enddo

if (mex==0) then
   gfex=gfex*gmfr
   !Tmean_ex=Tmean_ex*T0/gfex
   !if (mex==0) then
      if (gfex<small16) then
         Tmean_ex=Tarea_mean*T0/area_ex
      else
         Tmean_ex=Tmean_ex*T0*gmfr/gfex
      endif
   !endif


endif
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Extrapolation indices 
!======================================================================
SUBROUTINE EXT0(I,J,K,I1,J1,K1,I2,J2,K2,IX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION NX(3)

DO IX=1,3
   NX=0
   NX(IX)=1
DO M=-2,2,4
   I1=I+M*NX(1)
   I1=MIN(MP,I1)
   I1=MAX(2,I1)
   J1=J+M*NX(2)
   J1=MIN(NP,J1)
   J1=MAX(2,J1)
   K1=K+M*NX(3)
   K1=MIN(LP,K1)
   K1=MAX(2,K1)
   IF (IBCELL(I1,J1,K1).LE.0) THEN
      I2=I1+M*NX(1)
      I2=MIN(MP,I2)
      I2=MAX(2,I2)
      J2=J1+M*NX(2)
      J2=MIN(NP,J2)
      J2=MAX(2,J2)
      K2=K1+M*NX(3)
      K2=MIN(LP,K2)
      K2=MAX(2,K2)
      IBC2=MOD(IBCELL(I2,J2,K2),10)
      IF (IBC2.EQ.1) THEN
         I2=I1
         J2=J1
         K2=K1
      ENDIF
      RETURN
   ENDIF
ENDDO;ENDDO
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Extrapolation indices 
!======================================================================
SUBROUTINE EXT1(I,J,K,I1,J1,K1,I2,J2,K2,IX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION NX(3)

DO IX=1,3
   NX=0
   NX(IX)=1
   DO M=-2,2,4
      I1=I+M*NX(1)
      I1=MIN(MP,I1)
      I1=MAX(2,I1)
      J1=J+M*NX(2)
      J1=MIN(NP,J1)
      J1=MAX(2,J1)
      K1=K+M*NX(3)
      K1=MIN(LP,K1)
      K1=MAX(2,K1)
      IBC1=MOD(IBCELL(I1,J1,K1),10)
      IF (IBC1.NE.1) THEN
         I2=I1+M*NX(1)
         I2=MIN(MP,I2)
         I2=MAX(2,I2)
         J2=J1+M*NX(2)
         J2=MIN(NP,J2)
         J2=MAX(2,J2)
         K2=K1+M*NX(3)
         K2=MIN(LP,K2)
         K2=MAX(2,K2)
         IBC2=MOD(IBCELL(I2,J2,K2),10)
         IF (IBC2.EQ.1) THEN
            I2=I1
            J2=J1
            K2=K1
         ENDIF
         RETURN
      ENDIF
   ENDDO
ENDDO
RETURN
END
