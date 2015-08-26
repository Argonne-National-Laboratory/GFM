! prop.f90
!
!     Routines:
! subroutine conc(i,j,k)
! subroutine concms           not used
! function da(d,f)
! subroutine ther(i,j,k)
! subroutine tke(i,j,k)
! subroutine dnst_calc(i,j,k)
! subroutine wmix_ms_calc(i,j,k)
! function svel(i,j,k,l)
! subroutine bound(f,il,ipp)
! subroutine vel(i,j,k,l,gvel,dvel)
! subroutine th_gs
! subroutine enth_to_t(i,j,k)
! subroutine t_to_enth(i,j,k)
! 
!======================================================================
!======================================================================
!======================================================================
!     Species conservation equation (12/98)
!        YF : fuel  (given)
!        YO2: oxydizer (given)
!        YCO2: CO2 (given)
!        YH2O: H2O  (to be calculated)
!        YN2: nitrogen  (given)
!======================================================================
SUBROUTINE CONC(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!if(oxy_fuel == 1) then
	G0=GF(I,J,K,IYF)+GF(I,J,K,IYO2)+GF(I,J,K,IYCO2)+GF(I,J,K,IYN2)
	IF(G0.GT.ONE) THEN
	   g0=g0+GF(I,J,K,IYH2O)
	   GF(I,J,K,IYF)=GF(I,J,K,IYF)/G0 ! normalizes everything
	   GF(I,J,K,IYO2)=GF(I,J,K,IYO2)/G0
	   GF(I,J,K,IYCO2)=GF(I,J,K,IYCO2)/G0 
	   GF(I,J,K,IYN2)=GF(I,J,K,IYN2)/G0 
	   GF(I,J,K,IYH2O)=GF(I,J,K,IYH2O)/g0
	!   GF(I,J,K,IYH2O)=ZERO ! 8/18/05 
	ELSE
	   GF(I,J,K,IYH2O)=ONE-G0
	ENDIF
!else !need to solve transport for h2o to do this, but probably better for air fuel furnace
!	G0=GF(I,J,K,IYF)+GF(I,J,K,IYO2)+GF(I,J,K,IYCO2)+GF(I,J,K,IYh2o)
!	IF(G0.GT.ONE) THEN
!	   g0=g0+GF(I,J,K,IYn2)
!	   GF(I,J,K,IYF)=GF(I,J,K,IYF)/G0 ! normalizes everything
!	   GF(I,J,K,IYO2)=GF(I,J,K,IYO2)/G0
!	   GF(I,J,K,IYCO2)=GF(I,J,K,IYCO2)/G0 
!	   GF(I,J,K,IYN2)=GF(I,J,K,IYN2)/G0 
!	   GF(I,J,K,IYH2O)=GF(I,J,K,IYH2O)/g0
	!   GF(I,J,K,IYH2O)=ZERO ! 8/18/05 
!	ELSE
!	   GF(I,J,K,IYn2)=ONE-G0
!	ENDIF
!endif
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!SUBROUTINE CONCMS
!USE GBL_VAR
!IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!DATA NSP1 /6/

!DO I=2,MP,2
!DO J=2,NP,2
!DO K=2,LP,2
!   IF (IBCELL(I,J,K).GT.1) cycle
!   G0=0
!   DO L=1,NSP0
!      IF (L.NE.NSP1) G0=G0+GFM(I,J,K,L)
!   ENDDO
!   IF(G0.GT.ONE) THEN
!      !       GFM(I,J,K,1:NSP0)=GFM(I,J,K,1:NSP0)/G0
!      GFM(I,J,K,NSP1)=ZERO
!   ELSE
!      GFM(I,J,K,NSP1)=ONE-G0
!   ENDIF
!enddo;enddo;enddo
!RETURN
!END


!======================================================================
!======================================================================
!======================================================================
!     Peclet function (5/96)
!       D: effective diffusivity
!       F: volume flux
!       PE: Peclet number
!======================================================================
DOUBLE PRECISION FUNCTION DA(D,F)
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
real(8) :: sixth=1.0d+0/6.0d+0

IF (D.le.1.0D-25) then
   DA=0.0D+0
   RETURN
endif

FA=ABS(F)
PE=FA/D
IF (PE.LE.1.0D-2) THEN
   !DA=D/(1.0D+0+PE*0.5D+0+PE*PE/6.D+0+PE*PE*PE/24.D+0)
   DA=D/(1.0D+0+PE*(0.5D+0+PE*(sixth+PE/24.D+0)))
ELSEIF (PE.LE.2.0D+1) THEN
   DA=FA/(EXP(PE)-1.0D+0)
ELSE
   DA=0.0D+0
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     THER calculates thermal properties
!       TMU: turbulent viscosity
!       GLAM:
!       GCP: gas specific heat
!       GAMA: diffusion coefficient
!       TRDG: TL0/T0
!======================================================================
SUBROUTINE THER(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

IF (.NOT.TURB) THEN
   TMU(I,J,K)=ZERO
ELSE
   GF(I,J,K,IEPS)=MAX(GF(I,J,K,IEPS),1.0D-12)
   G0=CMu*REYG*EPSR/(TWO*TAUR)*GF(I,J,K,IK)**2/GF(I,J,K,IEPS)*DNST(I,J,K)
   TMU(I,J,K)=G0
ENDIF
GLAM(I,J,K)=ONE
!      TT=TZ*T0
!      GCPZ=ONE
!      IF (TT.GT.2.5D+2) GLAMZ=ONE+(TT-2.5D+2)*2.827D-3
!      IF (TT.GT.4.D+2) GCPZ=ONE+(TT-4.D+2)*1.882D-4
!      IF (TT.GT.1.8D+3) THEN
!         GLAMZ=GLAMZ+GLC1*((TT-1.8D+3)/1.2D+3)**2.93D+0
!         GCPZ=GCPZ+GCP1*((TT-1.8D+3)/1.2D+3)**3.167D+0
!      ENDIF
!      GCPZ=GCPZ*GCP0/CP0
GCP(I,J,K)=CPF(1)
GAMA(I,J,K)=TWO/REYG*(TMU(I,J,K)+(T(I,J,K)/TRDG)**AMU)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     TKE initializes turbulent parameters
!======================================================================
SUBROUTINE TKE(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DATA SML0/1.0D-6/

IF (.NOT.TURB) THEN
   GF(I,J,K,IK)=ZERO
   GF(I,J,K,IEPS)=ONE
ELSE
   GFIK=UG(I,J,K,1)**2+UG(I,J,K,2)**2+UG(I,J,K,3)**2
   GF(I,J,K,IK)=MAX(GFIK,SML0)
   GF(I,J,K,IEPS)=GF(I,J,K,IK)**2
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Calculate mixture normalized density
!       8/05  used to pass back mixture molecular weight
!                    that parameter no longer needed
!                    Was previously called WTDN
!======================================================================
subroutine dnst_calc(i,j,k)
use gbl_var
implicit double precision (a-h,o-z)

call conc(i,j,k)
wt=one/(gf(i,j,k,iyf)/wf+gf(i,j,k,iyco2)/wco2+   &
        gf(i,j,k,iyh2o)/wh2o+gf(i,j,k,iyo2)/wo2+ &
        gf(i,j,k,iyn2)/wn2)
temp=(one+p(i,j,k))/t(i,j,k)*wt/wt0
dnst(i,j,k)=max(temp,small20)
return
end


!======================================================================
!======================================================================
!======================================================================
!     Calculate mixture molecular weight including minor species
!       3/22/06 
!======================================================================
subroutine wmix_ms_calc(i,j,k)
use gbl_var
implicit double precision (a-h,o-z)

sum=zero
do n=1,nsp0
   sum=sum+gfm(i,j,k,n)/wms(n)
enddo

wmix_ms=one/sum
return
end


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION SVEL(I,J,K,L)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

ID2=I/2
JD2=J/2
KD2=K/2
G0=ZERO
DO M=1,3
   G0=G0+(UG(I,J,K,M)-DU(ID2,JD2,KD2,L,M))**2
ENDDO
SVEL=SQRT(G0)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE BOUND(F,IL,IPP)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

select case (ipp)
case (1)
   IF (IL.EQ.0) THEN
      F=MIN(F,P_MX)
      F=MAX(F,P_MN)
   ELSEIF (IL.EQ.4) THEN
      F=MIN(F,T_MX)
      F=MAX(F,T_MN)
   ELSEIF (IL.EQ.5) THEN
      F=MAX(F,1.0D-6)
   ELSE
      F=MIN(F,U_MX)
      F=MAX(F,U_MN)
      IF (ABS(F).LT.1.0D-12) F=ZERO
   ENDIF
   RETURN

case (2)
   IF (IL.EQ.IK) THEN
      F=MAX(F,1.0D-6)
   ELSEIF (IL.EQ.IEPS) THEN
      F=MAX(F,1.0D-12)
   ELSEIF (IL.EQ.IH) THEN
      RETURN
   ELSEIF (IL.EQ.20) THEN
      F=MAX(F,1.0D-1)
   ELSEIF (IL.EQ.8) THEN
      F=MAX(F,1.0D-14)
   ELSE
      F=MIN(F,ONE)
      F=MAX(F,ZERO)
   ENDIF
   RETURN

case (3)
   IF (IL.EQ.1) THEN
      F=MAX(F,ZERO)
      F=MIN(F,DN_MX)
   ELSEIF (IL.EQ.4) THEN
      F=MIN(F,TB(KL))
   ELSEIF (IL.EQ.5) THEN
      F=MIN(F,ONE)
      F=MAX(F,ZERO)
   ELSEIF (IL.EQ.6) THEN
      F=MIN(F,T_MX)
      F=MAX(F,T_MN)
   ENDIF
   RETURN
end select
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE VEL(I,J,K,L,GVEL,DVEL)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
ID2=I/2
JD2=J/2
KD2=K/2
G1=ZERO
D1=ZERO
DO IU=1,3
   G1=G1+UG(I,J,K,IU)**2
   D1=D1+DU(ID2,JD2,KD2,L,IU)**2
ENDDO
GVEL=SQRT(G1)
DVEL=SQRT(D1)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE TH_GS
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
data th_e/1.0d-3/
DO I=2,MPM2,2;  ID2=I/2
DO J=2,NJY2,2;  JD2=J/2
DO K=2,LKZ2,2;  KD2=K/2
   IF (IBCELL(I,J,K).GE.1) cycle
   !csl         THETA(I,J,K)=ONE-TH_DP(ID2,JD2,KD2)-TH_PT(ID2,JD2,KD2)
   !csl     CALL BOUND(THETA(I,J,K),20,2)
   ANR=ZERO
   DO L=1,NDNP
      ANR=ANR+DN(I/2,J/2,K/2,L)*RD3(L)
   END DO
   THETA(I,J,K)=(ONE-THET0C*ANR)
   IF (THETA(I,J,K).GE.TH_MN) cycle
   D=THETA(I,J,K)-TH_MN
   THETA(I,J,K)=TH_MN-TH_E*(ONE-EXP(D))
enddo;enddo;enddo
CALL EXTR(1)
CALL INTP
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     enth_to_T 
!        Calculates temperature from enthalpy
!
!        Only called from Gflow and Extr
!        Be sure to keep T_to_enth in sync with enth_to_T
!
!
!  Using the gas phase heat capacity (Shomate Equation):
!     cp = A + B*t + C*t^2 +D*t^3 + E/t^2
!  where t = T/1000 = t_NIST
!  where values of the coefficients are provided by the
!  National Institute of Standards and Technology (NIST).
!
!
!  Use the Newton Method:  T_new = T_old - f(T_old)/f'(T_old) 
!     where f(T) = [Am*T + (Bm/2)*T**2 + (Cm/3)*T**3 + (Dm/4)*T**4 - Em/T] - h
!    
!======================================================================
SUBROUTINE enth_to_T(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

track_enth_calls = track_enth_calls + 1
!mixture molecular weight (kg/kmol)
w_mix=one/(gf(i,j,k,iyf)/wf   + gf(i,j,k,iyco2)/wco2 + gf(i,j,k,iyh2o)/wh2o+ &
           gf(i,j,k,iyo2)/wo2 + gf(i,j,k,iyn2)/wn2)

!sensible enthalpy (J/mol) = J/(kmol*1000 mol/kmol)
h_s = gf(i,j,k,ih)*h_0 !J/kg
!NIST functions use molar units, so:
h_molar = h_s*w_mix/1.0d6 !kJ/mol = J/kg * (kg/kmol) * (kmol/ 1000 mol) * (kJ/ 1000 J)

         !filename=casedir//'\in_enth'//runum//'c.txt'                         !debugging code here  
         !open(85,file=filename)
         !write(85,'(/"Inside New Enthalpy <=> Temperature: gf.ih=",e17.10,"  T=",e17.10,"  h_molar=",e17.10/)') &
         !         gf(i,j,k,ih),T(i,j,k),h_molar
         !write(85,'(/"Counter    changeT           ", &
         !                       "t_NIST            ", &
         !                       "g0_func         ", &
         !                       "g0_func_prime   ", &
         !                       "nn_flip ", &
         !                       "nn_flip2"/)')

!Calculate coefficients for the mixture
real_T= T(i,j,k)*T0
a_mix=0;  b_mix=0;  c_mix=0;  d_mix=0;  e_mix=0;  f_mix=0;  h_mix=0

do n=1,5 !loop over major species
   if (real_T < cutT(n)) then !low set of coefficients
      a_mix=a_mix + gf(i,j,k,gf_chem_index(n))*a_nist(n,1)
      b_mix=b_mix + gf(i,j,k,gf_chem_index(n))*b_nist(n,1)
      c_mix=c_mix + gf(i,j,k,gf_chem_index(n))*c_nist(n,1)
      d_mix=d_mix + gf(i,j,k,gf_chem_index(n))*d_nist(n,1)
      e_mix=e_mix + gf(i,j,k,gf_chem_index(n))*e_nist(n,1)
      f_mix=d_mix + gf(i,j,k,gf_chem_index(n))*f_nist(n,1)
      h_mix=e_mix + gf(i,j,k,gf_chem_index(n))*h_nist(n,1)
      lo_hi(n)=1 !indicate low set of coefficients for species 'n'
   else !high set of coefficients
      a_mix=a_mix + gf(i,j,k,gf_chem_index(n))*a_nist(n,2)
      b_mix=b_mix + gf(i,j,k,gf_chem_index(n))*b_nist(n,2)
      c_mix=c_mix + gf(i,j,k,gf_chem_index(n))*c_nist(n,2)
      d_mix=d_mix + gf(i,j,k,gf_chem_index(n))*d_nist(n,2)
      e_mix=e_mix + gf(i,j,k,gf_chem_index(n))*e_nist(n,2)
      f_mix=d_mix + gf(i,j,k,gf_chem_index(n))*f_nist(n,2)
      h_mix=e_mix + gf(i,j,k,gf_chem_index(n))*h_nist(n,2)
      lo_hi(n)=2 !indicate high set of coefficients for species 'n'
   endif
enddo

!Sensible enthalpy = integral Cp_mix dt from t_ref to t_mix
!where (from NIST) Cp = A + Bt + Ct^2 + Dt^3 + E/t^2
!and integral Cp = At + (B/2)t^2 + (C/3)t^3 + (D/4)t^4 - E/t

!t_ref=298.15d0/1000.0d0
!t_ref_integral = t_ref*(a_mix + t_ref*((b_mix/2.0) + t_ref*((c_mix/3.0) + t_ref*(d_mix/4.0)))) &
!               - (e_mix/t_ref)

!Also sensible enthalpy = enthalpy at t_mix - enthalpy at t_ref
!which (from NIST) = At + (B/2)t^2 + (C/3)t^3 + (D/4)t^4 - E/t + F - H      <-- call this the g0 function of t

!Want to find a t_mix such that f(t)=0 when t=t_mix
!Since h_molar = g0(t), choose f(t) = g0(t) - h_molar

T_old = real_T/1000.0d0 !set first guess to the current temperature / 1000 (as used in equation)
do n=1,nmax
   n_cnt=n
   g0_func_prime = a_mix + T_old*(b_mix + T_old*(c_mix + T_old*d_mix)) + e_mix/T_old**2
   
   if (g0_func_prime==zero) exit 

   g0_func = T_old*(a_mix + T_old*((b_mix/2.0d0) + T_old*((c_mix/3.0d0) + T_old*(d_mix/4.0d0)))) &
               - (e_mix/T_old) + f_mix - h_mix ! kJ/mol
   changeT=(g0_func-h_molar)/g0_func_prime
   T_new = T_old - changeT
   if (abs(changeT)<tolerance_enth) then
                   !write(85,'(i7,4e18.10,2i7)') n_cnt,changeT,T_new,g0_func,g0_func_prime,nn_flip,nn_flip2
      exit
   endif
   T_old=T_new

   !Change coefficient set if needed
   !nn_flip=0;  nn_flip2=0
   do nn=1,5
      if (nn==2) cycle !only one set for oxygen
      if (nn==4) cycle !only one set for nitrogen
      if (T_new*1000.0d0 < cutT(nn) .and. lo_hi(nn).ne.1) then
         !need to change lo_hi coeff. set
         !nn_flip=nn_flip+1
         track_enth_flips = track_enth_flips + 1
         a_mix=a_mix - gf(i,j,k,gf_chem_index(nn))*a_nist(nn,2)
         b_mix=b_mix - gf(i,j,k,gf_chem_index(nn))*b_nist(nn,2)
         c_mix=c_mix - gf(i,j,k,gf_chem_index(nn))*c_nist(nn,2)
         d_mix=d_mix - gf(i,j,k,gf_chem_index(nn))*d_nist(nn,2)
         e_mix=e_mix - gf(i,j,k,gf_chem_index(nn))*e_nist(nn,2)
         f_mix=d_mix - gf(i,j,k,gf_chem_index(nn))*f_nist(nn,2)
         h_mix=e_mix - gf(i,j,k,gf_chem_index(nn))*h_nist(nn,2)
         lo_hi(nn)=1 !indicate switch to low range coefficients for species 'nn'
         a_mix=a_mix + gf(i,j,k,gf_chem_index(nn))*a_nist(nn,1)
         b_mix=b_mix + gf(i,j,k,gf_chem_index(nn))*b_nist(nn,1)
         c_mix=c_mix + gf(i,j,k,gf_chem_index(nn))*c_nist(nn,1)
         d_mix=d_mix + gf(i,j,k,gf_chem_index(nn))*d_nist(nn,1)
         e_mix=e_mix + gf(i,j,k,gf_chem_index(nn))*e_nist(nn,1)
         f_mix=d_mix + gf(i,j,k,gf_chem_index(nn))*f_nist(nn,1)
         h_mix=e_mix + gf(i,j,k,gf_chem_index(nn))*h_nist(nn,1)
      endif
      if (T_new*1000.0d0 >= cutT(nn) .and. lo_hi(nn).ne.2) then
         !need to change lo_hi coeff. set
         !nn_flip2=nn_flip2+1
         track_enth_flips = track_enth_flips + 1
         a_mix=a_mix - gf(i,j,k,gf_chem_index(nn))*a_nist(nn,1)
         b_mix=b_mix - gf(i,j,k,gf_chem_index(nn))*b_nist(nn,1)
         c_mix=c_mix - gf(i,j,k,gf_chem_index(nn))*c_nist(nn,1)
         d_mix=d_mix - gf(i,j,k,gf_chem_index(nn))*d_nist(nn,1)
         e_mix=e_mix - gf(i,j,k,gf_chem_index(nn))*e_nist(nn,1)
         f_mix=d_mix - gf(i,j,k,gf_chem_index(nn))*f_nist(nn,1)
         h_mix=e_mix - gf(i,j,k,gf_chem_index(nn))*h_nist(nn,1)
         lo_hi(nn)=2 !indicate switch to high range coefficients for species 'nn'
         a_mix=a_mix + gf(i,j,k,gf_chem_index(nn))*a_nist(nn,2)
         b_mix=b_mix + gf(i,j,k,gf_chem_index(nn))*b_nist(nn,2)
         c_mix=c_mix + gf(i,j,k,gf_chem_index(nn))*c_nist(nn,2)
         d_mix=d_mix + gf(i,j,k,gf_chem_index(nn))*d_nist(nn,2)
         e_mix=e_mix + gf(i,j,k,gf_chem_index(nn))*e_nist(nn,2)
         f_mix=d_mix + gf(i,j,k,gf_chem_index(nn))*f_nist(nn,2)
         h_mix=e_mix + gf(i,j,k,gf_chem_index(nn))*h_nist(nn,2)
      endif
   enddo !change coeff.
                           !write(85,'(i7,4e18.10,2i7)') n_cnt,changeT,T_new,g0_func,g0_func_prime,nn_flip,nn_flip2

enddo
T(i,j,k)=T_new*1000.0d0/T0
                           !write(85,*) "real new T=",T_new*1000.0d0
                           !close (85)

return
end


!======================================================================
!======================================================================
!======================================================================
!     T_to_enth 
!        Calculates enthalpy from temperature
!
!        Called from Gflow, Initsv, Rad_emis, and Sbc.
!        Be sure to keep T_to_enth in sync with enth_to_T
!
!
!  Using the gas phase heat capacity (Shomate Equation):
!     cp = A + B*t + C*t^2 +D*t^3 + E/t^2
!  where t = T/1000 = t_NIST
!  where values of the coefficients are provided by the
!  National Institute of Standards and Technology (NIST).
!   
!======================================================================
SUBROUTINE T_to_enth(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
real(8) t_NIST !temperature K / 1000.0d0

!mixture molecular weight (kg/kmol)
w_mix=one/(gf(i,j,k,iyf)/wf   + gf(i,j,k,iyco2)/wco2 + gf(i,j,k,iyh2o)/wh2o + &
           gf(i,j,k,iyo2)/wo2 + gf(i,j,k,iyn2)/wn2)

!Calculate coefficients for the mixture
real_T= T(i,j,k)*T0
a_mix=0;  b_mix=0;  c_mix=0;  d_mix=0;  e_mix=0;  f_mix=0;  h_mix=0
do n=1,5 !loop over major species
   if (real_T < cutT(n)) then !low set of coefficients
      a_mix=a_mix + gf(i,j,k,gf_chem_index(n))*a_nist(n,1)
      b_mix=b_mix + gf(i,j,k,gf_chem_index(n))*b_nist(n,1)
      c_mix=c_mix + gf(i,j,k,gf_chem_index(n))*c_nist(n,1)
      d_mix=d_mix + gf(i,j,k,gf_chem_index(n))*d_nist(n,1)
      e_mix=e_mix + gf(i,j,k,gf_chem_index(n))*e_nist(n,1)
      f_mix=d_mix + gf(i,j,k,gf_chem_index(n))*f_nist(n,1)
      h_mix=e_mix + gf(i,j,k,gf_chem_index(n))*h_nist(n,1)
   else !high set of coefficients
      a_mix=a_mix + gf(i,j,k,gf_chem_index(n))*a_nist(n,2)
      b_mix=b_mix + gf(i,j,k,gf_chem_index(n))*b_nist(n,2)
      c_mix=c_mix + gf(i,j,k,gf_chem_index(n))*c_nist(n,2)
      d_mix=d_mix + gf(i,j,k,gf_chem_index(n))*d_nist(n,2)
      e_mix=e_mix + gf(i,j,k,gf_chem_index(n))*e_nist(n,2)
      f_mix=d_mix + gf(i,j,k,gf_chem_index(n))*f_nist(n,2)
      h_mix=e_mix + gf(i,j,k,gf_chem_index(n))*h_nist(n,2)
   endif
enddo

t_NIST = real_T/1000.0d0 

enth_func = t_NIST*(a_mix + t_NIST*((b_mix/2.0d0) + t_NIST*((c_mix/3.0d0) + t_NIST*(d_mix/4.0d0)))) &
            - (e_mix/t_NIST) + f_mix - h_mix ! kJ/mol

gf(i,j,k,ih) = enth_func*1.0d6/(w_mix*h_0) ! kJ/mol *(1000 mol/kmol) / (kg/kmol) * (1000 J/kJ) = J/kg / h_0 

return
end
