!======================================================================
!======================================================================
!======================================================================
! Rad_gas.f90
!======================================================================
!                                                                   
!                           GAS RADIATION                           
!                                                                   
!             1983 - Chang and Rhee                                 
!                                                                   
!             YCO2 - molar fraction of CO2
!             YH2O - molar fraction of H2O
!             WN - wave number (#/m)
!                                                                   
!======================================================================
SUBROUTINE RAD_GAS
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DATA EPS1/1.0D-4/
!Sets akl0 = aklg+akls for use in rad0 module
!Called for every open cell

WNH2O=0
WNCO2=0
WNG=0
AKLG=zero

!work on h2o
if (yh2o.gt.eps1) then
   WNH2O(1,1)=0
   WNH2O(2,2)=1.60D+5
   WNH2O(3,2)=3.76D+5
   WNH2O(4,2)=5.35D+5
   WNH2O(5,2)=7.25D+5
   CALL AH2O !set absorptance in aah2o
   !  AAH2O=100
   WNH2O(1,2)=AAH2O(1)/2
   WNH2O(1,3)=AAH2O(1)
   DO N=2,5
      G0=AAH2O(N)/2
      WNH2O(N,1)=WNH2O(N,2)-G0
      WNH2O(N,3)=WNH2O(N,2)+G0
      IF (WNH2O(N-1,3).GT.WNH2O(N,1)) WNH2O(N-1,3)=WNH2O(N,1)
   ENDDO
endif

!work on co2
if (yco2.gt.eps1) then
   WNCO2(1,2)=6.67D+4
   WNCO2(2,2)=9.60D+4
   WNCO2(3,2)=1.06D+5
   WNCO2(4,3)=2.41D+5
   WNCO2(5,2)=3.66D+5
   WNCO2(6,2)=5.20D+5
   CALL ACO2  !set absorptance in aaco2
   !  AACO2=200
   DO N=1,6
      G0=AACO2(N)/2
      IF (N.EQ.1.AND.G0.GT.WNCO2(1,2)) THEN
         WNCO2(1,1)=0
         WNCO2(1,3)=AACO2(1)
      ELSEIF (N.EQ.4) THEN
         WNCO2(N,2)=WNCO2(N,3)-G0
         WNCO2(N,1)=WNCO2(N,2)-G0
      ELSE
         WNCO2(N,1)=WNCO2(N,2)-G0
         WNCO2(N,3)=WNCO2(N,2)+G0
      ENDIF
      IF (N.LE.1) CYCLE
      IF (WNCO2(N-1,3).GT.WNCO2(N,1)) WNCO2(N-1,3)=WNCO2(N,1)
   enddo
endif

!work on soot
CALL SOOTA !sets akls
WNG(1:5,1)=WNH2O(1:5,1)
WNG(1:5,2)=WNH2O(1:5,3)
WNG(6:11,1)=WNCO2(1:6,1)
WNG(6:11,2)=WNCO2(1:6,3)
DO N=1,10
   DO N2=N+1,11
      IF (WNG(N,1).LT.WNG(N2,1)) cycle
      G0=WNG(N,1)
      WNG(N,1)=WNG(N2,1)
      WNG(N2,1)=G0
      G0=WNG(N,2)
      WNG(N,2)=WNG(N2,2)
      WNG(N2,2)=G0
   enddo
   IF (N.LE.1) cycle
   IF (WNG(N-1,2).GT.WNG(N,1)) WNG(N-1,2)=WNG(N,1)
enddo

!work on setting aklg
WL2=10/WL(1)
E0=zero
N0=11
DO N=1,NWL
   IF (N.EQ.NWL) THEN
      WL1=0
   ELSE
      WL1=1/WL(N)
   ENDIF
   IF (WL2.LE.WL1.OR.WNG(11,2).LE.WL1) then
      WL2=WL1
      cycle
   endif
   DO N1=N0,1,-1
      IF (WNG(N1,2).LE.WL1) THEN
         N0=N1
         exit
      ENDIF
      IF (WNG(N1,2).GE.WL1.AND.WNG(N1,2).LE.WL2) THEN
         IF (WNG(N1,1).LT.WL1) THEN
            E0(N)=E0(N)+WNG(N1,2)-WL1
            WNG(N1,2)=WL1
            N0=N1
            exit
         ELSE
            E0(N)=E0(N)+WNG(N1,2)-WNG(N1,1)
         ENDIF
      ENDIF
   enddo

   IF (E0(N).LE.0.OR.WL2.LE.WL1) THEN
      AKLG(N)=zero
   ELSE
      E0(N)=E0(N)/(WL2-WL1)
      G0=1-E0(N)
      IF (G0.LE.1.0D-20) G0=1.0D-20
      AKLG(N)=-LOG(G0)/OPL
   ENDIF
   WL2=WL1
enddo
AKL0=AKLG+AKLS       
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!                                                                   
!                  BAND ABSORTANCE of WATER                         
!                                                                   
!             1973 - Edwards and Balakrishnan                       
!                                                                   
!======================================================================
SUBROUTINE AH2O
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION AA(5),B(3),ND_R(3),NG(3),V(3)
DATA NG/1,1,1/,V/3.652D+5,1.595D+5,3.756D+5/

AAH2O=0
IF(YH2O.LE.ZERO.OR.T_RAD.LE.3.0D+2) RETURN
PE=P_RAD+P_RAD*YH2O*(0.86D+1/TT0-0.5D+0)!effective pressure
!note: TT0 was set to SQRT(T_RAD/T0_R) !t0_r=1.0d+2
!W_R=P_RAD*YH2O*OPL/T_RAD/8.208D-2*18.0D+0 !density times length/temperature 
W_R=P_RAD*YH2O*OPL/T_RAD/8.208D-2*wh2o !density times length/temperature
B(1)=0.52D+4*EXP(-0.176D+2/TT0)
B(2)=0.14311D+0
B(3)=0.284D+2
ND_R(1:3)=0
CALL A1(V,NG,B,ND_R,AA(1)) !calculates band absorptance, band 1
B(1)=0.412D+2
B(2)=0.9427D-1
B(3)=0.564D+2
ND_R(2)=1
CALL A1(V,NG,B,ND_R,AA(2)) !calculates band absorptance, band 2
B(1)=0.19D+0
B(2)=0.13219D+0
B(3)=0.6D+2
ND_R(2)=2
CALL CHIWHI(T0_R,V,NG,ND_R,CHI0,WHI0) !band 3 has three overlapping bands
CALL CHIWHI(T_RAD,V,NG,ND_R,CHI,WHI)
T1=B(1)*CHI/CHI0
T4=B(2)*WHI/WHI0
B(1)=0.23D+1
ND_R(1)=1
ND_R(2)=0
CALL CHIWHI(T0_R,V,NG,ND_R,CHI0,WHI0)
CALL CHIWHI(T_RAD,V,NG,ND_R,CHI,WHI)
T2=B(1)*CHI/CHI0
T5=B(2)*WHI/WHI0
B(1)=0.224D+2
ND_R(1)=0
ND_R(3)=1
CALL CHIWHI(T0_R,V,NG,ND_R,CHI0,WHI0)
CALL CHIWHI(T_RAD,V,NG,ND_R,CHI,WHI)
T3=B(1)*CHI/CHI0
T6=B(2)*WHI/WHI0
C1=T1+T2+T3
C3=B(3)*TT0
BTA=2.D0*SQRT(C3)*(SQRT(T1*T4)+SQRT(T2*T5)+SQRT(T3*T6)) !Eq. A23
CALL BANDA(C1,BTA,C3,AA(3))
B(1)=3.0D+0
B(2)=0.8169D-1
B(3)=0.431D2
ND_R(2)=1
CALL A1(V,NG,B,ND_R,AA(4)) !calculates band absorptance, band 4
B(1)=0.25D+1
B(2)=0.11628D+0
B(3)=0.32D+2
ND_R(1)=1
ND_R(2)=0
CALL A1(V,NG,B,ND_R,AA(5))
!---  AA: 1/cm
!---  AAH2O: 1/m
AAH2O=AA*1.0D+2
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!                                                                   
!             BAND ABSORPTANCES of CARBON DIOXIDE                   
!                                                                   
!             1973 - Edwards and Balakrishnan                       
!                                                                   
!======================================================================
SUBROUTINE ACO2
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION AA(6),B(3),ND_R(3),NG(3),V(3)
DATA NG/1,2,1/,V/1.351D+5,6.67D+4,2.396D+5/

AA=0
!W_R=P_RAD*YCO2*OPL/T_RAD/8.208D-2*44.8D+0 !GAS density  
W_R=P_RAD*YCO2*OPL/T_RAD/8.208D-2*wco2 !GAS density
IF (W_R.LE.ZERO) RETURN
PE=(P_RAD+P_RAD*YCO2*0.3D0)**0.7D0 !effective pressure
B(1)=0.19D+2
B(2)=0.6157D-1
B(3)=0.127D2
ND_R(1)=0
ND_R(2)=1
ND_R(3)=0
CALL A1(V,NG,B,ND_R,AA(1)) !calculates first band absorptance
PE=(P_RAD+P_RAD*YCO2*0.3D+0)**0.8D+0
B(1)=0.36D+0
B(2)=0.4017D-1
B(3)=0.134D+2
ND_R(1)=-1
ND_R(2)=0
ND_R(3)=1
CALL A1(V,NG,B,ND_R,AA(2)) !second band absorptance
B(1)=0.24D+0
B(2)=0.11888D+0
B(3)=0.101D+2
CALL A1(V,NG,B,ND_R,AA(3)) !third band absorptance
B(1)=0.11D+3
B(2)=0.24723D+0
B(3)=0.112D+2
ND_R(1)=0
CALL A1(V,NG,B,ND_R,AA(4)) !fourth band absorptance
PE=(P_RAD+P_RAD*YCO2*0.3D0)**0.65D0
B(1)=4.0D+0
B(2)=0.13341D+0
B(3)=23.5D+0
ND_R(1)=1
CALL A1(V,NG,B,ND_R,AA(5)) !fifth band absorptance
B(1)=0.66D-1
B(2)=0.39305D+0
B(3)=0.345D+2
ND_R(1)=2
CALL A1(V,NG,B,ND_R,AA(6)) !sixth band absorptance
!---  AA: 1/cm
!---  AACO2: 1/m
AACO2=AA*1.0D+2
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!                                                                   
!                  GAS BAND ABSORPTANCE (regular bands)             
!                                                                   
!======================================================================
SUBROUTINE A1(V,NG,B,ND_R,A)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION B(3),ND_R(3),V(3),NG(3)

CALL CHIWHI(T0_R,V,NG,ND_R,CHI0,WHI0)
CALL CHIWHI(T_RAD,V,NG,ND_R,CHI,WHI)
C3=B(3)*SQRT(T_RAD/T0_R) !Ao for pure rotational bands
C1=B(1)*CHI/CHI0 !alpha(t) for pure rotational bands
C2=2.0D+0*SQRT(C1*C3*B(2)*WHI/WHI0)
CALL BANDA(C1,C2,C3,A)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!                                                                   
!             CHI and WHI functions of GAS EMISSION                 
!                                                                   
!             1968 - Weiner and Edwards                             
!          Equations A17 and A20 in Appendix                        
!======================================================================
SUBROUTINE CHIWHI(TT,V,NG,ND_R,CHI,WHI)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION V(3),NG(3),U(3),ND_R(3),NV(3)
! Returns values in arguments chi and whi

IF ((ND_R(1).NE.0).OR.(ND_R(2).NE.0).OR.(ND_R(3).NE.0)) then
   M=3
   UD=ZERO
   XD=ZERO
   XY=ZERO
   YN=ZERO
   DO K=1,M
      U(K)=HCK*V(K)/TT
      NV(K)=0
      IF (ND_R(K).LT.0) NV(K)=ABS(ND_R(K))
      NDK=ABS(ND_R(K))
      Y1=ZERO
      do
         XGAS=EXP(-U(K)*NV(K))/PER(NG(K)-1,NG(K)-1)
         X1=XGAS*PER(NV(K)+NG(K)+NDK-1,NG(K)+NDK-1)
         X2=XGAS*PER(NV(K)+NG(K)-1,NG(K)-1)
         IF (X1.LT.1.0D-10) exit !too small to add to the summation
         XY=XY+X1 !numerator of equation A17
         XD=XD+X2 !denominator of equation A17
         Y1=Y1+SQRT(X1)
         NV(K)=NV(K)+1
         IF (NV(K).ge.100) exit
      enddo
      YN=YN+Y1*Y1
      UD=UD-U(K)*ND_R(K)
   enddo
   CHI=(ONE-EXP(UD))*XY/XD !equation A16 without evaluation at To
   WHI=YN/XY/SQRT(TT0) !equation A19 without evaluation at To
else
   CHI=ONE !for pure rotational bands only
   WHI=ONE/SQRT(TT0) !for pure rotational bands only
   !note: TT0 was set to SQRT(T_RAD/T0_R) !t0_r=1.0d+2
endif
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!                                                                   
!                APPROXIMATION to GAS BAND ABSORPTANCE              
!                                                                   
!             1964 - Edwards and Menard                             
!                                                                   
!======================================================================
SUBROUTINE BANDA(ALP,BTA,A0,A)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
1     FORMAT(T20,'***** ERROR IN BANDA *****',/,T20,5D12.5)
!Returns new value in argument A

A=0
IF (A0.LE.0) RETURN
IF ((ALP.LT.ZERO).OR.(BTA.LT.ZERO).OR.(A0.LT.ZERO) .or. (W_R*PE.LT.0)) then
   WRITE(1,1)ALP,BTA,A0,PE,W_R  !negative pressure, density or length
   call stop_run ("Error in BANDA, negative pressure, density, or length.")
endif

IF (W_R*PE.eq.0) return

ETA=BTA*PE
TAU=ALP*W_R/A0
IF (ETA.GE.ONE) then
   A=ALP*W_R !A for ETA GE 1, tau LT 1
   IF (TAU.LE.1.0D+0) THEN
      A_S=TAU
   ELSE
      A_S=1.0D+0+LOG(TAU)
   ENDIF
else
   IF (TAU.LE.ETA) THEN
      A_S=TAU
   ELSEIF (TAU.LE.1/ETA) THEN
      A_S=2.0D+0*SQRT(ETA*TAU)-ETA
   ELSE
      A_S=LOG(ETA*TAU)+2.0D+0-ETA
   ENDIF
endif
A=A0*A_S
RETURN
END

