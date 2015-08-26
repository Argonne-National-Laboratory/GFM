! Gslv3g.f90
!======================================================================
!     solves scalar flow properties 
!     revision: 10/97
!======================================================================
SUBROUTINE GSLV3G
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!      DIMENSION CPTS(5),CPTD(5)
!      KD=2

eqtyp=2
MI1=4
MI3=MPM2
NJ1=NJY1
NJ3=NJY2
LK1=LKZ1
LK3=LKZ2
DO L=LSTAR,LEND 
   IF (L.EQ.IYF) THEN 
      NELG=6
   ELSEIF (L.EQ.IYCO2) THEN 
      NELG=7
   ELSEIF (L.EQ.IH) THEN 
      NELG=8
   ELSEIF (L.EQ.IK) THEN 
      IF (.NOT.TURB) cycle
      NELG=9
   ELSEIF (L.EQ.IEPS) THEN 
      IF (.NOT.TURB) cycle
      NELG=10
   ELSEIF (L.EQ.IYN2) THEN 
      NELG=11
   ELSEIF (L.EQ.IYO2) THEN 
      NELG=12
   ELSEIF (L.EQ.IYH2O) THEN 
      NELG=13
   ELSE 
      NELG=99
      cycle
   ENDIF
   !---------test h convergence in frozen flow -----
   !niter=1
   !if(nelg==8.and.itr_gas>20) then
   !   niter = 10000
   !endif
   !lgh=itr_gas
   !do icnt = lgh,lgh+niter 
   !nnn=0

   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      FZ(I,J,K)=GF(I,J,K,L) 
   enddo;enddo;enddo
   CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
   IF (L.EQ.LSTAR.OR.SGM.NE.SIGMA(L)) THEN
      SGM=SIGMA(L)
      CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,0)
   ELSE
      CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,1)
   ENDIF

   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      IF (ABS(AP(I,J,K)).LE.SMALL20) THEN
         AP(I,J,K)=ONE
         BS(I,J,K)=FZ(I,J,K)
         cycle
      ENDIF
      KD2=K/2
      VOL=VOL_C(I,J,K)
      BS(I,J,K)=BS(I,J,K)*VOL
      if (ndnp==0) then
         AP(I,J,K)=AP(I,J,K)-(SFP(I,J,K)*VOL)
      else
         AP(I,J,K)=AP(I,J,K)+(EVP(ID2,JD2,KD2)-CON(ID2,JD2,KD2)-SFP(I,J,K))*VOL
      endif
!      IF (.NOT.STEADY) THEN
!         BS(I,J,K)=BS(I,J,K)+APO(I,J,K)*GFO(I,J,K,L)*VOL
!         AP(I,J,K)=AP(I,J,K)+APO(I,J,K)*VOL
!      ENDIF
      AP(I,J,K)=AP(I,J,K)/RFL(L)
      BS(I,J,K)=BS(I,J,K)+RFLC(L)*FZ(I,J,K)*AP(I,J,K)
   enddo;enddo;enddo

   if(nelg==8) call calc_residual(FZ,h_resid_pre,MI1,MI3,NJ1,NJ3,LK1,LK3)
   IF (L.EQ.IK.AND.KEY.EQ.2) CALL DKEL(MI1,MI3,NJ1,NJ3,LK1,LK3)
   !         CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
   !   if(nelg==8)then
   !      call gmres_solver(FZ,MI1,MI3,NJ1,NJ3,LK1,LK3)
   !   else
   call calc_residual(FZ,gf_resid_pre(L),MI1,MI3,NJ1,NJ3,LK1,LK3)
   CALL ADLBL3(FZ,MI1,MI3,NJ1,NJ3,LK1,LK3)
   !   endif
   call calc_residual(FZ,gf_resid(L),MI1,MI3,NJ1,NJ3,LK1,LK3)

   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      CALL BOUND(FZ(I,J,K),L,2)
      GF(I,J,K,L)=FZ(I,J,K)
   enddo;enddo;enddo

!if (ihresid>0.and.nelg==8.and.niter>1)
!&  write(nu_hres,"(i7,e25.16,e25.16)") icnt,h_resid,q_global_bal
!enddo !end test
enddo
h_resid=gf_resid(ih)

return
end


!======================================================================
!======================================================================
!======================================================================
!     flux coefficients AS
!======================================================================
SUBROUTINE GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,IGS)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

if (nelg.eq.8) Qsurf=ZERO !Total heat convec/cond thru surface (W)
DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) cycle
   AP(I,J,K)=ZERO
   DO IU=1,3
      DO M=-1,1,2
         M1=(M+3)/2
         IF (IGS.ne.1) then
            CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
            IBC0=IBCELL(I2,J2,K2)
            IF (IBC0.LE.0) THEN
               AS(I,J,K,IU,M1)=FLWD(I1,J1,K1,IU,M1)
            ELSEIF (IBC0.EQ.2) THEN
               AS(I,J,K,IU,M1)=FLW(I2,J2,K2,IU,M1)
            ELSE            
               AS(I,J,K,IU,M1)=ZERO
            ENDIF
         endif
         AP(I,J,K)=AP(I,J,K)+AS(I,J,K,IU,M1)
      ENDDO ! for m=
   enddo !for iu=
enddo;enddo;enddo
RETURN
END
