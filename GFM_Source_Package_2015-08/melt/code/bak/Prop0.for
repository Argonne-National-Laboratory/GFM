C! PROP.FOR
C======================================================================
C     Peclet function (5/96)
C       D: effective diffusivity
C       F: volume flux
C       PE: Peclet number
C======================================================================
      DOUBLE PRECISION FUNCTION DA(D,F)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA ZERO,ONE/0.0D+0,1.0D+0/
      DA=D
      RETURN
      IF (D.GT.1.0D-25) GOTO 100
      DA=ZERO
      RETURN
100   FA=ABS(F)
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
C======================================================================
C     ENTH calculates enthalpy
C======================================================================
!     Lottes 5/7/05
!
!     Without loss of precision, take TL0 = 0, instead of the 
!     current value of 1 K. Then the formula for non-dimensional
!     enthalpy calculated from the temperature and specific head is:
!     lg(i,j,k)%h = lg(i,j,k)%C * lg(i,j,k)%T
!
!     Backing Temperature out of the enthalpy is equally simple,
!     except that we enforce temperature bounds on the Temperature,
!     and adjust the enthalpy accordingly if the bound is enforced.
!     The enthalpy bounds are not in a fixed ratio to temperature 
!     because the specific heat may vary with temperature.
!
!     Code for calculating T from h and c is in routine: GFLOW
!
!     With these simplifications, this routine is no longer needed.
!
!-----------------------------------------------------------------------
      SUBROUTINE ENTH(I,J,K,MTH)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	!CLMIX=CL_G*CP0 Lottes
	CLMIX=LG(I,J,K)%C*CP0 !use user input c
      !Lg(i,j,k)%h=Lg(i,j,k)%c * Lg(i,j,k)%T
	!return
	IF (MTH.EQ.2) GOTO 200
	T_LG=LG(I,J,K)%T*T0
	!T_LG=MAX(T_LG,1.1D+3) !Lottes 4/15/05: limits should not be hard coded!
	!Lottes 4/15/05: This sets lower bound on liquid glass T at min melting T of particles
	T_LG=MAX(T_LG,tmltr) 
      !150	GF0=CLMIX*(T_LG-TL0) Lottes 5/6/05
	GF0=CLMIX*T_LG
      LG(I,J,K)%H=GF0/H_0
      hnew=Lg(i,j,k)%c * max(Lg(i,j,k)%T, Tmltr/T0)
      Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T, Tmltr/T0)
      if(abs(Lg(i,j,k)%h-hnew)>1.0e-13)then
	  hnewx=0
	endif
      RETURN
200   GF0=LG(I,J,K)%H*H_0
      !T_LG=GF0/CLMIX+TL0 Lottes 5/6/05
      T_LG=GF0/CLMIX
	IF (T_LG.GT.T_MX*T0) THEN
	   T_LG=T_MX*T0
	   LG(I,J,K)%T=T_MX
	   GOTO 250
	ELSEIF (T_LG.LT.TMLTR) THEN
	   T_LG=TMLTR
	   LG(I,J,K)%T=TMLTR/T0
	   GOTO 250
	ENDIF
	LG(I,J,K)%T=T_LG/T0
      RETURN
250	GF0=CLMIX*T_LG
      LG(I,J,K)%H=GF0/H_0
      END
C======================================================================
C     THER calculates thermal properties
C       MU: glass viscosity
C       C: glass specific heat
C       K: glass conductivity
C======================================================================
      SUBROUTINE THER(I,J,K)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	T_LG=LG(I,J,K)%T*T0
	IBC0=MOD(IBCELL(I,J,K),10)
      IF (IBC0.NE.4) THEN
	   UDF => UDF_MU
	   CALL UDF_INT(UDF_MUN,T_LG,G0)
	ELSE
         G0=ZERO
      ENDIF  
      LG(I,J,K)%MU=G0/GMU0
	UDF => UDF_CL
	CALL UDF_INT(UDF_CLN,T_LG,G0)
      LG(I,J,K)%C=G0/CP0
	UDF => UDF_K
	CALL UDF_INT(UDF_KN,T_LG,G0)
	LG(I,J,K)%K=G0/GK0
cbg      LG(I,J,K)%K=ONE
	RETURN
      END
C======================================================================
C     Glass density
C       6/02
C======================================================================
      SUBROUTINE DSLG(I,J,K)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	T_LG=LG(I,J,K)%T*T0
	UDF => UDF_DS
	CALL UDF_INT(UDF_DSN,T_LG,G1)
      LG(I,J,K)%DS=G1/DNST0
	RETURN
      END
C======================================================================
      SUBROUTINE UDF_INT(N1,T1,G0)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	IF (N1.LE.0) RETURN
      IF (T1.LT.UDF(1)%T) THEN
	   G0=UDF(1)%F
	ELSEIF (T1.GE.UDF(N1)%T) THEN
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
C======================================================================
      DOUBLE PRECISION FUNCTION SVEL(I,J,K,L)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ID2=I/2
      JD2=J/2
      KD2=K/2
	G0=ZERO
	DO M=1,3
         G0=G0+(LG(I,J,K)%U(M)-PC(ID2,JD2,KD2,L)%U(M))**2
	ENDDO
      SVEL=SQRT(G0)
      RETURN
      END
C======================================================================
      SUBROUTINE BOUND(F,IL,IPP)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	GOTO (100,200,300) IPP
	RETURN
100   IF (IL.EQ.0) THEN
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
	   IF (ABS(F).LT.1.0D-14) F=ZERO
	ENDIF
      RETURN
200   IF (IL.EQ.20) THEN
         F=MAX(F,4.0D-1)
      ELSE
         F=MIN(F,ONE)
         F=MAX(F,ZERO)
      ENDIF
      RETURN
300   IF (IL.EQ.1) THEN
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
	ENDIF
      RETURN
      END
C======================================================================
C     4/02
C======================================================================
      SUBROUTINE TH_GS
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DATA GR_F1,GR_F2/1.0D+0,0.0D+0/
	B=0
      IF (NBS0.GE.1) B=5D-2
cz      IF (NBS0.GE.1) B=5D-2
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
               ANR=ANR+PC(ID2,JD2,KD2,L)%DN*RP3_C(L)
            END DO
            PC0MR_LG(ID2,JD2,KD2)=PC0(ID2,JD2,KD2)%MR
            LG(I,J,K)%MR=LG(I,J,K)%MR+PC0MR_LG(ID2,JD2,KD2)
	   ENDIF
	   IF (NPS_S.GT.0) THEN
            DO L=1,NPS_S
               ANR=ANR+PS(ID2,JD2,KD2,L)%DN*RP3_S(L)
            END DO
            PS0MR_LG(ID2,JD2,KD2)=A*PS0(ID2,JD2,KD2)%MR
            LG(I,J,K)%MR=LG(I,J,K)%MR+PS0MR_LG(ID2,JD2,KD2)
	   ENDIF
	   TH_P=THET0C*ANR
	   IF (TH_P.GT.0.6D+0) TH_P=0.6D+0
	   G0=B*PS0(ID2,JD2,KD2)%MR
	   IBC1=MOD(IBCELL(I,J,K+2),10)
cbg	   IF (IBC1.EQ.4) THEN
cbg	      GR_C(ID2,JD2)=G0*DZ(K)*GR_F1
cbg	      RM_GR_C=RM_GR_C+GR_C(ID2,JD2)*AREA_C(I,J,K,3)*GMFR
cbg         ELSE
cbg	      GR_M(ID2,JD2,KD2)=G0*GR_F2
cbg	   ENDIF
         VOL=VOL_C(I,J,K)
	   RM_GR=RM_GR+G0*GMFR*VOL
	   ANR=0
         DO L=1,NBS0
            ANR=ANR+GB4(ID2,JD2,KD2,L)%DN*GB4(ID2,JD2,KD2,L)%R**3
	      GB4(ID2,JD2,KD2,L)%GR=G0/NBS0*0.1D+0
         END DO
	   TH_B=THET0C*ANR
cz
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
      CALL INTP(1)
	RETURN
	END
C======================================================================
      DOUBLE PRECISION FUNCTION SVELC(I,J,K,L)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ID2=I/2
      JD2=J/2
      KD2=K/2
	G0=ZERO
	DO M=1,3
         G0=G0+(LG(I,J,K)%U(M)-P1(ID2,JD2,KD2,L)%U(M))**2
	ENDDO
      SVELC=SQRT(G0)
      RETURN
	END
C======================================================================
      SUBROUTINE CONCMS
	USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DATA NSP1 /6/
	DO 120 I=2,MP,2
	DO 120 J=2,NP,2
	DO 120 K=2,LP,2
	   IF (IBCELL(I,J,K).GT.1) GOTO 120
	   G0=0
	   DO L=1,NMSP
            IF (L.NE.NSP1) G0=G0+GFM(I,J,K,L)
	   ENDDO
         IF(G0.GT.ONE) THEN
C	      GFM(I,J,K,1:NSP0)=GFM(I,J,K,1:NSP0)/G0
            GFM(I,J,K,NSP1)=ZERO
	   ELSE
            GFM(I,J,K,NSP1)=ONE-G0
	   ENDIF
120   CONTINUE
      RETURN
      END
!----------------------------------------------------------------------
!     Conduction Heat Transfer from Liquid Glass to Batch
!
!     Lottes: Only deals with "batch" that is floating on the glass surface
!             Conduction to sand or cullet in the top layer of 
!             computational cells in the melt
!----------------------------------------------------------------------
      SUBROUTINE QCOND
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !CBG	DATA CD_D_C,CD_D_S/1.0D-3,2.0D-3/ !conductivity thicknesses
	IF (NPHAS<2) RETURN ! Should not happen in melt code
	K=LPM2;      KD2=K/2 ! top layer of melt cells
      DO I=2,MP,2; ID2=I/2
      DO J=2,NP,2; jd2=j/2
	   IF (IBCELL(I,J,K)>0) CYCLE
         if (nps_c > 0) then !Lottes 4-11-05: skip if no cullet
 		   QCD_C(ID2,JD2,0:NPS_C)=ZERO
		   TH_P0=PC0(ID2,JD2,KD2)%TH
		   if (TH_P0 > SMALL) then
		      !AREA=AREA_C(I,J,K,3)*R0**2/NPS_C/NPS0
		      AREA=AREA_C(I,J,K,3)*R0**2*NPS_C/NPS0 !Lottes 5/24/05 reduce by fraction of size groups?
		      IF (TH_P0.LT.TH_PMX) AREA=AREA*TH_P0/TH_PMX
		      DO L=1,NPS_C
		         IF (LG(I,J,K)%T.LE.TM_C(L)) CYCLE
		         DT=(LG(I,J,K)%T-TM_C(L))*T0
		         QCD_C(ID2,JD2,L)=GK0_PC*DT/CD_D_C*AREA !heat conducted to cullet
		      ENDDO
	      endif
         endif
         if (nps_s > 0) then !Lottes 4-11-05: skip if no sand
	      QCD_S(ID2,JD2,0:NPS_S)=ZERO
            TH_P0=PS0(ID2,JD2,KD2)%TH
            IF (TH_P0.LT.SMALL) CYCLE
            !AREA=AREA_C(I,J,K,3)*R0**2/NPS_S/NPS0
            AREA=AREA_C(I,J,K,3)*R0**2*NPS_S/NPS0 !Lottes 5/24/05 reduce by fraction of size groups?
            IF (TH_P0.LT.TH_PMX) AREA=AREA*TH_P0/TH_PMX
	      DO L=1,NPS_S
               IF (LG(I,J,K)%T.LE.TM_S(L)) CYCLE
	         DT=(LG(I,J,K)%T-TM_S(L))*T0
	         QCD_S(ID2,JD2,L)=GK0_PS*DT/CD_D_S*AREA
            ENDDO
         endif
      enddo; enddo
      return
      end
!----------------------------------------------------------------------
!     Total Conduction Heat Transfer from Liquid Glass to Batch
!
!     Lottes: Only deals with "batch" that is floating on the glass surface
!             Conduction to sand or cullet in the top layer of 
!             computational cells in the melt
!----------------------------------------------------------------------
      SUBROUTINE qcond_sum
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	IF (NPHAS<2) RETURN ! Should not happen in melt code.
      qcond_s_tot=zero
	qcond_c_tot=zero
	K=LPM2;      KD2=K/2 !top layer of melt cells
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
      RETURN
      END
