! GSLV3M.F90
!======================================================================
!     GSLV3M solves governing transport equations for sub-species 
!     12/01
!======================================================================
!SUBROUTINE GSLV3M(MSE_0)
! 11/7/05 Commented out MSE_0 and L2 because did nothing
SUBROUTINE GSLV3M
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DATA IRR0,SML1/5,1.0D-25/
!      KD=3
!MSE_0=2#1111111 !127
eqtyp=3
MI1=4
MI3=MPM2
NJ1=NJY1
NJ3=NJY2
LK1=LKZ1
LK3=LKZ2
!csl  IF (MSE_0.LT.0) GOTO 700
RR_F=0
RR_C=0
DO I=MI1,MI3,2;  ID2=I/2
DO J=NJ1,NJ3,2;   JD2=J/2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GT.0) CYCLE
   KD2=K/2
   TMP=T(I,J,K)*T0
   DNST1=DNST(I,J,K)*DNST0
   VOL=DX(I)*DR(J)*DZ(K)*VOL0
   !Convert mass fractions to molar concentrations (kmol/m**3)
   GFM1=GFM(I,J,K,1)/WMS(1)*DNST1 !CH4
   GFM2=GFM(I,J,K,2)/WMS(2)*DNST1 !O2
   GFM3=GFM(I,J,K,3)/WMS(3)*DNST1 !CO
   GFM4=GFM(I,J,K,4)/WMS(4)*DNST1 !H2O
   GFM5=GFM(I,J,K,5)/WMS(5)*DNST1 !CO2
   GFM6=GFM(I,J,K,6)/WMS(6)*DNST1 !N2
   GFM7=GFM(I,J,K,7)/WMS(7)*DNST1 !NO

!--------Reaction 1
   CALL SPIN(I,J,K,1,FXMS1)
   CALL SPIN(I,J,K,2,FXMS2)
   IF (IRR0.GE.1) THEN
      IF (GFM1.LE.SML1.OR.GFM2.LE.SML1) THEN
         RRMS(ID2,JD2,KD2,1)=0
      ELSE
         RR1=GFM1**ODR(1,1)*GFM2**ODR(1,2)
         RR2=CK0_M(1)*RR1*EXP(-ERC(1)/TMP)*VOL
         IF (RR2.GT.FXMS1) RR2=FXMS1
         IF (FXMS2/1.5D+0.LT.RR2) RR2=FXMS2/1.5D+0
         RRMS(ID2,JD2,KD2,1)=RR2*0.3D+0+RRMS(ID2,JD2,KD2,1)*0.7D+0
      ENDIF
   ELSE
      RRMS(ID2,JD2,KD2,1)=0
   ENDIF

!--------Reaction 2
   IF (IRR0.GE.2) THEN
      FXMS2=FXMS2-RR2*1.5D+0
      IF (FXMS2.GT.small20.AND.GFM3.GT.small20) THEN
         RR1=GFM3**ODR(2,1)*GFM2**ODR(2,2)
         RR2=CK0_M(2)*RR1*EXP(-ERC(2)/TMP)*VOL
         IF (FXMS2*2.LT.RR2) RR2=FXMS2*2
         CALL SPIN(I,J,K,3,FXMS3)
         RR2=MIN(RR2,FXMS3)
      ELSE
         RR2=0
      ENDIF
   ELSE
      RRMS(ID2,JD2,KD2,2)=0
   ENDIF

!--------Reaction 3
   IF (IRR0.GE.3) THEN
      IF (GFM5.GT.small20) THEN
         RR1=GFM5**ODR(3,1)
         RR3=CK0_M(3)*RR1*EXP(-ERC(3)/TMP)*VOL
         CALL SPIN(I,J,K,5,FXMS5)
         RR3=MIN(RR3,FXMS5)
      ELSE
         RR3=0
      ENDIF
      RR1=RR2-RR3
      IF (RR1.LT.0) RR1=0
      !CSL         RRMS(ID2,JD2,KD2,2)=RR1
      RRMS(ID2,JD2,KD2,2)=RR1*0.3D+0+RRMS(ID2,JD2,KD2,2)*0.7D+0
   ELSE
      RRMS(ID2,JD2,KD2,2)=0
   ENDIF
   RR_F=RR_F+RRMS(ID2,JD2,KD2,1)*WMS(1)
   RR_C=RR_C+RRMS(ID2,JD2,KD2,2)*WMS(3)

!--------Reaction 4
   IF (IRR0.GE.4) THEN
      IF (GFM2.GT.small20.AND.GFM3.GT.small20.AND.GFM6.GT.small20) THEN
         RR1=GFM3**ODR(4,1)*GFM2**ODR(4,2)*GFM(I,J,K,6)/0.725D+0
         RR2=CK0_M(4)*RR1*EXP(-ERC(4)/TMP)*VOL
      ELSE
         RR2=0
      ENDIF
!--------Reaction 5
      IF (GFM2.GT.small20.AND.GFM6.GT.small20) THEN
         RR1=GFM6**ODR(5,1)*GFM2**ODR(5,2)
         RR3=CK0_M(5)*RR1*EXP(-ERC(5)/TMP)*TMP**BETA(5)
         RR3=RR3*VOL
      ELSE
         RR3=0
      ENDIF
      RR2=RR2+RR3
      RRMS(ID2,JD2,KD2,4)=RR2
   ELSE
      RRMS(ID2,JD2,KD2,4)=0
   ENDIF
enddo;enddo;enddo

vol_t=zero
change_ms=zero
!L2=MSE_0*2 !set L2=254

!----------------
! Species numbers
! 1=CH4
! 2=O2
! 3=CO
! 4=H2O
! 5=CO2
! 6=N2
! 7=NO

DO L=1,NSP0
   !CSL         IF (L.EQ.6) CYCLE       
   !L2=L2/2
   !IF (MOD(L2,2).EQ.0) CYCLE    !was never 0   
   NELM=L       
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      FZ(I,J,K)=GFM(I,J,K,L)
   enddo;enddo;enddo 

   CALL SOURCEM(MI1,MI3,NJ1,NJ3,LK1,LK3)
   SGM=SSIGMA
   IF (IGS0.EQ.0) THEN !first time here after doing initsvm
      CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,0)
      AP1=AP
      IGS0=1
   ELSE
      AP=AP1
   ENDIF

   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      VOL=VOL_C(I,J,K)
      !CSL            BS(I,J,K)=BS(I,J,K)*VOL
      if (ndnp==0) then
         AP(I,J,K)=AP(I,J,K)-(SFP(I,J,K)*VOL)
      else
         AP(I,J,K)=AP(I,J,K)+(EVP(I/2,J/2,K/2)-CON(I/2,J/2,K/2)-SFP(I,J,K))*VOL
      endif
      !IF (.NOT.STEADY) THEN
      !   !CSL            BS(I,J,K)=(BS(I,J,K)+APO(I,J,K)*GFMO(I,J,K,L))*VOL
      !   AP(I,J,K)=AP(I,J,K)+APO(I,J,K)*VOL
      !ENDIF
      AP(I,J,K)=AP(I,J,K)/RFM(L)
      BS(I,J,K)=BS(I,J,K)+RFMC(L)*FZ(I,J,K)*AP(I,J,K)
   enddo;enddo;enddo 

   call calc_residual(fz,resid_ms_pre(l),mi1,mi3,nj1,nj3,lk1,lk3)
   CALL ADLBL3(FZ,MI1,MI3,NJ1,NJ3,LK1,LK3)
   !CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
   call calc_residual(fz,resid_ms(l),mi1,mi3,nj1,nj3,lk1,lk3)

   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      IF (FZ(I,J,K).GT.ONE) FZ(I,J,K)=ONE
      IF (FZ(I,J,K).LT.0) FZ(I,J,K)=0
      IF (L.EQ.3.AND.FZ(I,J,K).GT.0.1D0) FZ(I,J,K)=0.1D0
      IF (L.EQ.7.AND.FZ(I,J,K).GT.1.0D-2) FZ(I,J,K)=1.0D-2

!      if(gfm(i,j,k,l) > zero) then
!        change_ms=change_ms + vol_c(i,j,k)*abs(fz(i,j,k)-gfm(i,j,k,l))/gfm(i,j,k,l)
!        vol_t=vol_t+vol_c(i,j,k)
!      endif
      GFM(I,J,K,L)=RFMC(L)*GFM(I,J,K,L)+RFM(L)*FZ(I,J,K)
      IF (GFM(I,J,K,L).LT.small20) GFM(I,J,K,L)=0
   enddo;enddo;enddo 
enddo
!change_ms = change_ms*vol0/(vol_t*nsp0)
!change_ms = max(change_ms, 1.0d-20)

bal_MS=0
DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) CYCLE
   !CSL     G1=GFM(I,J,K,6)
   G0=ZERO
   DO L=1,NSP0
      !CSL        IF (L.NE.6) G0=G0+GFM(I,J,K,L)
      G0=G0+GFM(I,J,K,L)
   ENDDO
   !CSL         IF (G0.GT.ONE) THEN
   !CSL            GFM(I,J,K,6)=0
   !CSL        DO L=1,NSP0
   !CSL           GFM(I,J,K,L)=GFM(I,J,K,L)/G0
   !CSL        ENDDO
   !CSL         ELSE
   !CSL        GFM(I,J,K,6)=ONE-G0
   !CSL     ENDIF
   !CSL         SMX_MS=SMX_MS+ABS(GFM(I,J,K,6)-G1)*VOL_C(I,J,K)*VOL0
   bal_MS=bal_MS+ABS(ONE-G0)*VOL_C(I,J,K)
enddo;enddo;enddo 
bal_ms=bal_ms*vol0/vol_tot


!--------------------------------------------------------------------
!     SOOT MASS FRACTION  {smf}
!--------------------------------------------------------------------
!700 continue
IF (.NOT.REACT) RETURN
L=NSP0+1
DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   FZ(I,J,K)=smf(I,J,K)
enddo;enddo;enddo 

NELM=11       
CALL SOURCEM(MI1,MI3,NJ1,NJ3,LK1,LK3)
NELM=1       
SGM=SSIGMA
CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,1)

DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) cycle
   VOL=VOL_C(I,J,K)
   BS(I,J,K)=BS(I,J,K)*VOL
   if (ndnp==0) then
      AP(I,J,K)=AP(I,J,K)-(SFP(I,J,K)*VOL)
   else
      AP(I,J,K)=AP(I,J,K)+(EVP(I/2,J/2,K/2)-CON(I/2,J/2,K/2)-SFP(I,J,K))*VOL
   endif
!   IF (.NOT.STEADY) THEN
!      !CSL         BS(I,J,K)=(BS(I,J,K)+APO(I,J,K)*GFMO(I,J,K,L))*VOL
!      AP(I,J,K)=AP(I,J,K)+APO(I,J,K)*VOL
!   ENDIF
   AP(I,J,K)=AP(I,J,K)/RFM(L)
   BS(I,J,K)=BS(I,J,K)+RFMC(L)*FZ(I,J,K)*AP(I,J,K)
enddo;enddo;enddo
 
call calc_residual(fz,resid_ms_pre(l),mi1,mi3,nj1,nj3,lk1,lk3)
CALL ADLBL3(FZ,MI1,MI3,NJ1,NJ3,LK1,LK3)
call calc_residual(fz,resid_ms(l),mi1,mi3,nj1,nj3,lk1,lk3)
!      CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)

soot_tot=0
smf_max=zero
smf_max_noclip=zero
smf_clipped_cnt=0
DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) CYCLE
   smf_max_noclip=max(smf_max_noclip,fz(i,j,k))
   IF (FZ(I,J,K).GT.soot_cap) THEN
      G0=soot_cap
	  smf_clipped_cnt=smf_clipped_cnt+1
   ELSEIF (FZ(I,J,K).GT.small20) THEN
      G0=FZ(I,J,K)
   ELSE
      G0=ZERO
   ENDIF
   smf_max=max(smf_max,g0)
   smf(I,J,K)=RFM(L)*G0+RFMC(L)*smf(I,J,K)
   vol=dx(i)*dr(j)*dz(k)
   soot_tot=soot_tot + smf(i,j,k)*dnst(i,j,k)*dnst0*vol
enddo;enddo;enddo 
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE SPIN(I,J,K,LSP,FXMS)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
G11=AS(I,J,K,1,1)*GFM(I-2,J,K,LSP)
G12=AS(I,J,K,1,2)*GFM(I+2,J,K,LSP)
G21=AS(I,J,K,2,1)*GFM(I,J-2,K,LSP)
G22=AS(I,J,K,2,2)*GFM(I,J+2,K,LSP)
G31=AS(I,J,K,3,1)*GFM(I,J,K-2,LSP)
G32=AS(I,J,K,3,2)*GFM(I,J,K+2,LSP)
!CSL  G0=AP(I,J,K)*GFM(I,J,K,LSP)
FXMS=G11+G12+G21+G22+G31+G32
FXMS=FXMS*GMFR/WMS(LSP)
RETURN
END
