!======================================================================
! PROP.FOR
!======================================================================
!======================================================================
!     Peclet function (5/96)
!       D: effective diffusivity
!       F: volume flux
!       PE: Peclet number
!======================================================================
DOUBLE PRECISION FUNCTION DA(D,F)
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
DATA ZERO,ONE/0.0D+0,1.0D+0/
DA=D
RETURN
!Lottes
!code below is a mix of Patankar's hybrid and exponential schemes.
!It appears to destabalize the slow flow computation in the melt
!the return above essientially makes this an upwind scheme.
!See my notes.
if (D < 1.0D-25) then
   DA=ZERO
   RETURN
endif

FA=ABS(F)
PE=FA/D
IF (PE.LE.1.0D-2) THEN
   DA=D/(ONE+PE*0.5D+0+PE*PE/6.D+0+PE*PE*PE/24.D+0)
ELSEIF (PE.LE.2.0D+1) THEN
   DA=FA/(EXP(PE)-ONE)
ELSE
   DA=ZERO
ENDIF
RETURN
END
!======================================================================
!======================================================================
!======================================================================
!     ENTH calculates enthalpy - not used 
!======================================================================
! Lottes 5/7/05
!
! Does not handle variable specific heat
!
! Without loss of precision, take TL0 = 0, instead of the 
! current value of 1 K. Then the formula for non-dimensional
! enthalpy calculated from the temperature and specific heat is:
! lg(i,j,k)%h = lg(i,j,k)%C * lg(i,j,k)%T
!
! Backing Temperature out of the enthalpy is equally simple,
! except that we enforce temperature bounds on the Temperature,
! and adjust the enthalpy accordingly if the bound is enforced.
! The enthalpy bounds are not in a fixed ratio to temperature 
! because the specific heat may vary with temperature.
!
! Code for calculating T from h and c is in routine: GFLOW
!
! With these simplifications, this routine is no longer needed.
!
!-----------------------------------------------------------------------
SUBROUTINE ENTH(I,J,K,MTH)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!CLMIX=CL_G Lottes
CLMIX=LG(I,J,K)%C !use user input c
!Lg(i,j,k)%h=Lg(i,j,k)%c * Lg(i,j,k)%T
!return
IF (MTH.EQ.2) GOTO 200
T_LG=LG(I,J,K)%T
!T_LG=MAX(T_LG,1.1D+3) !Lottes 4/15/05: limits should not be hard coded!
!Lottes 4/15/05: This sets lower bound on liquid glass T at min melting T of particles
T_LG=MAX(T_LG,tmltr) 
!150  GF0=CLMIX*(T_LG-TL0) Lottes 5/6/05
GF0=CLMIX*T_LG
LG(I,J,K)%H=GF0
hnew=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr)
Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr)
if(abs(Lg(i,j,k)%h-hnew)>1.0e-13)then
  hnewx=0
endif
RETURN

200 continue

GF0=LG(I,J,K)%H
!T_LG=GF0/CLMIX+TL0 Lottes 5/6/05
T_LG=GF0/CLMIX
IF (T_LG.GT.T_MX) THEN
   T_LG=T_MX
   LG(I,J,K)%T=T_MX
   GOTO 250
ELSEIF (T_LG.LT.TMLTR) THEN
   T_LG=TMLTR
   LG(I,J,K)%T=TMLTR
   GOTO 250
ENDIF
LG(I,J,K)%T=T_LG
RETURN
250 continue
GF0=CLMIX*T_LG
LG(I,J,K)%H=GF0
return
END



!======================================================================
!======================================================================
!======================================================================
! h_to_Tlg 
!
! h_to_Tlg calculates the liquid glass temperature given the enthalpy.
! This calculation is based on the [temperature, specific heat] pairs of
! values provided as the user defined function (udf) for the liquid glass
! specific heat property.
!   
!======================================================================
subroutine h_to_Tlg(h_given,T_returned)
use gbl_var
implicit double precision (a-h,o-z)

udf=>udf_cl !udf points to the liquid glass specific heat array

if (h_given <= h_pts(1)) then
	!handle temperatures below the first udf point
	T_returned = h_given/udf_cl(1)%f
	return
endif

if (h_given >= h_pts(udf_cln)) then
	!handle temperatures above the last udf point
	T_returned = udf_cl(udf_cln)%t + ((h_given-h_pts(udf_cln))/udf_cl(udf_cln)%f)
	return
endif

!find the bounding udf points for the given enthalpy
do n=2,udf_cln
	if (h_given <= h_pts(n)) then
		idx_east = n
		idx_west = n-1
	endif
enddo

!e and w refer to east and west points respectively
! Let:
!      h_delta = h - hw
!		 T_delta = T - Tw
! Know that:
!	1:  (c+cw) * T_delta = 2 * h_delta
!	2:  c = cw + (((ce-cw)/(Te-Tw)) * T_delta)

h_delta = h_given - h_pts(idx_west)
c_slope = (udf_cl(idx_east)%f-udf_cl(idx_west)%f) / (udf_cl(idx_east)%t-udf_cl(idx_west)%t)

T_delta = (-udf_cl(idx_west)%f + &
           sqrt(udf_cl(idx_west)%f**2 + 2.0d0*c_slope*h_delta)) / c_slope

T_returned = udf_cl(idx_west)%t + T_delta
return
end




!======================================================================
!======================================================================
!======================================================================
! Tlg_to_h 
!
! Tlg_to_h calculates the enthalpy given the liquid glass temperature.
! This calculation is based on the [temperature, specific heat] pairs of
! values provided as the user defined function (udf) for the liquid glass
! specific heat property.
!   
!======================================================================
subroutine Tlg_to_h(T_given,h_returned)
use gbl_var
implicit double precision (a-h,o-z)

udf=>udf_cl !udf points to the liquid glass specific heat array

!handle a constant specific heat or temperatures below the first udf point
h_returned = udf_cl(1)%f*T_given
if (udf_cln == 1 .or. T_given <= udf_cl(1)%t) return
h_returned = udf_cl(1)%f * udf_cl(1)%T
do n=2,udf_cln
	if (T_given > udf_cl(n)%t) then
		!h_returned = h_returned + ((udf_cl(n-1)%f+udf_cl(n)%f)/2)*(udf_cl(n)%t-udf_cl(n-1)%t)
		h_returned = h_pts(n)
	else
		call udf_int(udf_cln,T_given,g0) !set g0 to the specific heat at the given temperatures		
		h_returned = h_returned + ((udf_cl(n-1)%f+g0)/2)*(T_given-udf_cl(n-1)%t)
		exit
	endif
enddo

if (T_given > udf_cl(udf_cln)%t) then
	h_returned = h_returned + udf_cl(udf_cln)%f*(T_given-udf_cl(udf_cln)%t)
endif

return
end



!======================================================================
!======================================================================
!======================================================================
! THER calculates thermal properties
!   MU: glass viscosity
!   C: glass specific heat
!   K: glass conductivity
!======================================================================
SUBROUTINE THER(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
T_LG=LG(I,J,K)%T
IBC0=MOD(IBCELL(I,J,K),10)
IF (IBC0.NE.4) THEN !not on the surface
   UDF => UDF_MU
   CALL UDF_INT(UDF_MUN,T_LG,G0)
ELSE
      G0=ZERO !viscosity is set to zero on the surface for free slip B.C.
ENDIF  
   !LG(I,J,K)%MU=G0/GMU0
   LG(I,J,K)%MU=G0
UDF => UDF_CL
CALL UDF_INT(UDF_CLN,T_LG,G0)
   LG(I,J,K)%C=G0
UDF => UDF_K
CALL UDF_INT(UDF_KN,T_LG,G0)
LG(I,J,K)%K=G0
!bg      LG(I,J,K)%K=ONE
RETURN
END
!======================================================================
!======================================================================
!======================================================================
! Glass density
!   6/02
!======================================================================
SUBROUTINE DSLG(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
T_LG=LG(I,J,K)%T
UDF => UDF_DS
CALL UDF_INT(UDF_DSN,T_LG,G1)
LG(I,J,K)%DS=G1
RETURN
END
!======================================================================
!======================================================================
!======================================================================
! User defined function interpolation
SUBROUTINE UDF_INT(N1,T1,G0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (N1.LE.0) RETURN
IF (T1.LT.UDF(1)%T) THEN !min is first value
   G0=UDF(1)%F
ELSEIF (T1.GE.UDF(N1)%T) THEN !max is last value (no extrapolation)
   G0=UDF(N1)%F
ELSE
   DO N=2,N1
         IF (T1.LT.UDF(N)%T) EXIT
   ENDDO
   G1=(T1-UDF(N-1)%T)/(UDF(N)%T-UDF(N-1)%T)
   G0=UDF(N-1)%F+G1*(UDF(N)%F-UDF(N-1)%F)
ENDIF
RETURN
END
!======================================================================
!======================================================================
!======================================================================
double precision function svel(i,j,k,l)
use gbl_var
implicit double precision (a-h,o-z)
svel = sqrt( (LG(I,J,K)%U(1)-PC(I/2,J/2,K/2,L)%U(1))**2 &
            +(LG(I,J,K)%U(2)-PC(I/2,J/2,K/2,L)%U(2))**2 &
            +(LG(I,J,K)%U(3)-PC(I/2,J/2,K/2,L)%U(3))**2 )
return
end
!======================================================================
!======================================================================
!======================================================================
SUBROUTINE BOUND(F,IL,IPP)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
GOTO (100,200,300) IPP
RETURN
100 continue
IF (IL.EQ.0) THEN
      F=MIN(F,P_MX)
      F=MAX(F,P_MN)
   ELSEIF (IL.EQ.4) THEN
      F=MIN(F,T_MX)
      F=MAX(F,T_MN)
      if (Tmax_c.ne.zero) f=min(f,Tmax_c)
   ELSEIF (IL.EQ.5) THEN
      F=MAX(F,1.0D-6)
   ELSE
      F=MIN(F,U_MX)
      F=MAX(F,U_MN)
   IF (ABS(F).LT.1.0D-14) F=ZERO
ENDIF
RETURN
200 continue
IF (IL.EQ.20) THEN
      F=MAX(F,4.0D-1)
ELSE
      F=MIN(F,ONE)
      F=MAX(F,ZERO)
ENDIF
RETURN
300 continue
IF (IL.EQ.1) THEN
   IF (F.LT.DN_MN) F=0
   F=MIN(F,DN_MX)
ELSEIF (IL.EQ.2) THEN
   F=MIN(F,100.0D+0)
   F=MAX(F,ZERO)
ELSEIF (IL.EQ.4) THEN
   F=MIN(F,TM_C(KL))
ELSEIF (IL.EQ.5) THEN
   F=MIN(F,ONE)
   F=MAX(F,ZERO)
ELSEIF (IL.EQ.6) THEN
   F=MIN(F,T_MX)
   F=MAX(F,T_MN)
   if (Tmax_c.ne.zero) f=min(f,Tmax_c)
ENDIF
RETURN
END
!======================================================================
!======================================================================
!======================================================================
!     4/02
!======================================================================
SUBROUTINE TH_GS
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DATA GR_F1,GR_F2/1.0D+0,0.0D+0/
B=0
IF (NBS0.GE.1) B=5D-2
!cz      IF (NBS0.GE.1) B=5D-2
A=ONE-B
RM_GR=0
RM_GR_C=0
DO 220 I=2,MPM2,2
ID2=I/2
DO 220 J=2,NJY2,2
JD2=J/2
DO 220 K=2,LKZ2,2
IF (IBCELL(I,J,K).GE.1) CYCLE
KD2=K/2
ANR=ZERO
LG(I,J,K)%MR=0
IF (NPS_C.GT.0) THEN
   DO L=1,NPS_C
      !ANR=ANR+PC(ID2,JD2,KD2,L)%DN*RP3_C(L)
      ANR=ANR+PC(ID2,JD2,KD2,L)%DN*c4d3pi*RP_C(L)**3
   END DO
   PC0MR_LG(ID2,JD2,KD2)=PC0(ID2,JD2,KD2)%MR
   LG(I,J,K)%MR=LG(I,J,K)%MR+PC0MR_LG(ID2,JD2,KD2)
ENDIF
IF (NPS_S.GT.0) THEN
   DO L=1,NPS_S
      !ANR=ANR+PS(ID2,JD2,KD2,L)%DN*RP3_S(L)
      ANR=ANR+PS(ID2,JD2,KD2,L)%DN*c4d3pi*RP_S(L)**3
   END DO
   PS0MR_LG(ID2,JD2,KD2)=A*PS0(ID2,JD2,KD2)%MR
   LG(I,J,K)%MR=LG(I,J,K)%MR+PS0MR_LG(ID2,JD2,KD2)
ENDIF
TH_P=ANR
IF (TH_P.GT.0.6D+0) TH_P=0.6D+0
G0=B*PS0(ID2,JD2,KD2)%MR
IBC1=MOD(IBCELL(I,J,K+2),10)
!bg      IF (IBC1.EQ.4) THEN
!bg         GR_C(ID2,JD2)=G0*DZ(K)*GR_F1
!bg         RM_GR_C=RM_GR_C+GR_C(ID2,JD2)*AREA_C(I,J,K,3)
!bg         ELSE
!bg         GR_M(ID2,JD2,KD2)=G0*GR_F2
!bg      ENDIF
VOL=VOL_C(I,J,K)
RM_GR=RM_GR+G0*VOL
ANR=0
DO L=1,NBS0
   ANR=ANR+GB4(ID2,JD2,KD2,L)%DN*GB4(ID2,JD2,KD2,L)%R**3
   GB4(ID2,JD2,KD2,L)%GR=G0/NBS0*0.1D+0
END DO
TH_B=ANR
!cz
TH_B=MIN(TH_B,1.0D+3)
IF (IBCELL(I,J,K+2).EQ.4) THEN
   G0=ONE-TH_P
ELSE
      G0=ONE-TH_P-TH_B
ENDIF
IF (G0.LT.TH_MN) G0=TH_MN
LG(I,J,K)%TH=G0
220   CONTINUE
CALL EXTR(1)
CALL INTP
RETURN
END
!======================================================================
!======================================================================
!======================================================================
double precision function svelc(i,j,k,l)
use gbl_var
implicit double precision (a-h,o-z)
svelc = sqrt( (lg(i,j,k)%u(1)-p1(i/2,j/2,k/2,l)%u(1))**2 &
             +(lg(i,j,k)%u(2)-p1(i/2,j/2,k/2,l)%u(2))**2 &
             +(lg(i,j,k)%u(3)-p1(i/2,j/2,k/2,l)%u(3))**2 )
return
end
!======================================================================
!======================================================================
!======================================================================
SUBROUTINE CONCMS
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DATA NSP1 /6/
DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IF (IBCELL(I,J,K).GT.1) cycle
   G0=0
   DO L=1,NMSP
      IF (L.NE.NSP1) G0=G0+GFM(I,J,K,L)
   ENDDO
   IF(G0.GT.ONE) THEN
!        GFM(I,J,K,1:NSP0)=GFM(I,J,K,1:NSP0)/G0
     GFM(I,J,K,NSP1)=ZERO
   ELSE
     GFM(I,J,K,NSP1)=ONE-G0
   ENDIF
enddo;enddo;enddo

RETURN
END
!======================================================================
!======================================================================
!======================================================================
! Conduction Heat Transfer from Liquid Glass to Batch
!
! Lottes: Only deals with "batch" that is floating on the glass surface
!         Conduction to sand or cullet in the top layer of 
!         computational cells in the melt
!----------------------------------------------------------------------
SUBROUTINE QCOND
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!!bg  DATA CD_D_C,CD_D_S/1.0D-3,2.0D-3/ !conductivity thicknesses
IF (NPHAS<2) RETURN ! Should not happen in melt code
K=LPM2;      KD2=K/2 ! top layer of melt cells
DO I=2,MP,2; ID2=I/2
DO J=2,NP,2; jd2=j/2
   IF (IBCELL(I,J,K)>0) CYCLE
   if (nps_c > 0) then !Lottes 4-11-05: skip if no cullet
      QCD_C(ID2,JD2,0:NPS_C)=ZERO
      TH_P0=PC0(ID2,JD2,KD2)%TH
      if (TH_P0 > SMALL) then
         !AREA=AREA_C(I,J,K,3)/NPS_C/NPS0
         AREA=AREA_C(I,J,K,3)*NPS_C/NPS0 !Lottes 5/24/05 reduce by fraction of size groups?
         IF (TH_P0.LT.TH_PMX) AREA=AREA*TH_P0/TH_PMX
         DO L=1,NPS_C
            IF (LG(I,J,K)%T.LE.TM_C(L)) CYCLE
            DT=(LG(I,J,K)%T-TM_C(L))
            !T_bsurf=pc(id2,jd2,kd2,L)%T
            T_bsurf=Tmltr
            gk_b=exp(-11.9646d0-39825.3d0/(T_bsurf-4064.82d0)) !Lottes 6/17/05
            QCD_C(ID2,JD2,L)=gk_b*DT/CD_D_C*AREA !heat conducted to cullet 
				!QCD_C(ID2,JD2,L)=zero   !!!!!!!!!!!!!!!!  testing only - turns off conduction
			   !QCD_C(ID2,JD2,L)=GK0_PC*DT/CD_D_C*AREA !heat conducted to cullet
         ENDDO
      endif
   endif
   if (nps_s > 0) then !Lottes 4-11-05: skip if no sand
      QCD_S(ID2,JD2,0:NPS_S)=ZERO
      TH_P0=PS0(ID2,JD2,KD2)%TH
      IF (TH_P0.LT.SMALL) CYCLE
     !AREA=AREA_C(I,J,K,3)/NPS_S/NPS0
      AREA=AREA_C(I,J,K,3)*NPS_S/NPS0 !Lottes 5/24/05 reduce by fraction of size groups?
      IF (TH_P0.LT.TH_PMX) AREA=AREA*TH_P0/TH_PMX
      DO L=1,NPS_S
         IF (LG(I,J,K)%T.LE.TM_S(L)) CYCLE
         DT=(LG(I,J,K)%T-TM_S(L))
         !T_bsurf=ps(id2,jd2,kd2,L)%T
         T_bsurf=Tmltr
         gk_b=exp(-11.9646d0-39825.3d0/(T_bsurf-4064.82d0)) !Lottes 6/17/05
         QCD_S(ID2,JD2,L)=gk_b*DT/CD_D_S*AREA
         !QCD_S(ID2,JD2,L)=zero   !!!!!!!!!!!!!!!!  testing only - turns off conduction
         !QCD_S(ID2,JD2,L)=GK0_PS*DT/CD_D_S*AREA
      ENDDO
   endif
enddo;enddo
return
end
!======================================================================
!======================================================================
!======================================================================
! Total Conduction Heat Transfer from Liquid Glass to Batch
!
! Lottes: Only deals with "batch" that is floating on the glass surface
!         Conduction to sand or cullet in the top layer of 
!         computational cells in the melt
!----------------------------------------------------------------------
SUBROUTINE qcond_sum
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (NPHAS<2) RETURN ! Should not happen in melt code.
qcond_s_tot=zero
qcond_c_tot=zero
K=LPM2; KD2=K/2 !top layer of melt cells
DO I=2,MP,2; ID2=I/2
DO J=2,NP,2; jd2=j/2
   IF (IBCELL(I,J,K)>0) CYCLE
   if (nps_c > 0) then !Lottes 4-11-05: skip if no cullet
      qcond_s_tot = qcond_s_tot
      QCD_C(ID2,JD2,0)=QCD_C(ID2,JD2,1)
      DO L=2,NPS_C
         QCD_C(ID2,JD2,0)=QCD_C(ID2,JD2,0)+QCD_C(ID2,JD2,L)
      ENDDO
      qcond_c_tot = qcond_c_tot + qcd_c(id2,jd2,0)
   endif
   if (nps_s > 0) then !Lottes 4-11-05: skip if no sand
      QCD_S(ID2,JD2,0)=QCD_S(ID2,JD2,1)
      DO L=2,NPS_S
         QCD_S(ID2,JD2,0)=QCD_S(ID2,JD2,0)+QCD_S(ID2,JD2,L)
      ENDDO
      qcond_s_tot = qcond_s_tot + qcd_s(id2,jd2,0)
   endif
enddo;enddo
return
end

