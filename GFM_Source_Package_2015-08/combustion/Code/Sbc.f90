! SBC.F90
!======================================================================
!     SBC SETS STEADY INLET BOUNDARY CONDITIONS
!       CONC: balances species concentration
!       TKE:  calculates turbulent parameters
!       THER: calculates thermal properties
!       SBC3: copies values from one node to another
!       ENTH: calculates enthalpy
!       SBC5: assigns wall values  - not called
!     3/00
!======================================================================
!
!     On entry the sbc file has been opened and the input namelist has
!     already been read in the setup routine.
!
!======================================================================
!======================================================================
!======================================================================
SUBROUTINE SBC
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
REAL*8,ALLOCATABLE :: TM1(:),TM2(:),TM3(:),TM4(:)
DIMENSION DT_0(NDNP),DN_0(NDNP),DU_0(NDNP),DV_0(NDNP),DW_0(NDNP)
DIMENSION DC_0(NPT0)
NAMELIST /RADIN/ RI,NWL,T_A,H_A,H_G,W_K,W_D,NRADWALL, &
   eps_w,  &    !wall emissivity
   eps_m, &     !melt surface emissivity
   eps_c, &     !ceiling or crown emissivity
   have_crown, &  !1=> have a crown
   height_to_ceiling, &   !z value at ceiling or start of crown
   maxri1, &    !maximum radiosity solver outer loop 1 iteration number
   maxri2, &    !maximum radiosity solver inner loop 2 iteration number
   minri2, &    !minimum radiosity solver inner loop 2 cycles
   preset_vf    !1=>view factors are all calculated once

TM=0
DTM=BIG
YFOP=0.0D+0
Q_IN=ZERO
!----------------------------------------------------------------------
!     Inlet flow properties (IBCELL=2)
!       FR_F: fuel gas mass flow rate (kg/s)
!       FR_OX: oxydizer mass flow rate (kg/s)
!       FR_N2: inert mass flow rate (kg/s)
!       FR_P: catalyst mass flow rate (kg/s)
!       FR_Q: fuel mass flow rate (kg/s)
!---------------------------------------------------------------------- 
FR_F=ZERO
FR_OX=ZERO
FR_N2=ZERO
FR_P=ZERO
FR_Q=ZERO
SRATIO=ZERO
q_h_in = zero
DATA ICOUNT /1/

READ (nu_sbc,*) I_REF,J_REF,K_REF,P_REF
IF (IBCELL(I_REF,J_REF,K_REF).NE.2) THEN
   WRITE (6,191) I_REF,J_REF,K_REF
   191   FORMAT ('  ** ERROR: Reference point (',I3,','I3,','I3')', &
   ' in the SBC file should be an inlet cell.')      
   call stop_run("Pressure reference point not an inlet.")
ENDIF
IF (IDEBUG.EQ.2) WRITE (20,*) 'Reference pressure:',P_REF
P_REF=P_REF/PG0

do !read inlet block
   READ (nu_sbc,*) I1,J1,K1,I2,J2,K2,IU
   IF (I1.LE.0) exit !a row of zeros marks the end of normal inlet conditions
   READ (nu_sbc,*) T_0,YF_0,YOX_0,YN2_0,YP1_0,YP2_0,YCK_0,U_0,V_0,W_0
   T_0=T_0/T0
   U_0=U_0/UG0
   V_0=V_0/UG0
   W_0=W_0/UG0
   IF (NPHAS.NE.1) THEN
      DO L=1,NDNP
         IF (L.LE.NDP0) THEN
            READ (nu_sbc,*) DT_0(L),DN_0(L),DU_0(L),DV_0(L),DW_0(L)
         ELSE
            READ (nu_sbc,*) DT_0(L),DN_0(L),DU_0(L),DV_0(L),DW_0(L),G0
            L1=L-NDP0
            DC_0(L1)=G0
         ENDIF
         DT_0(L)=DT_0(L)/T0
         DN_0(L)=DN_0(L)/DND0
         DU_0(L)=DU_0(L)/UG0
         DV_0(L)=DV_0(L)/UG0
         DW_0(L)=DW_0(L)/UG0
      ENDDO
   ENDIF


   DO I=I1,I2,2;  ID2=I/2
   DO J=J1,J2,2;  JD2=J/2
   DO K=K1,K2,2
      IF (IBCELL(I,J,K).NE.2) cycle
      !At an inlet
	  !Ordering of sbc blocks is important because the blocks may overlap!
	  !Temporarily set IBCELL to indicate the cell has been processed as an inlet.
	  !In case of a later sbc block also containing the same cell that cell will retain its 
	  !previous block set of values. 
      IBCELL(I,J,K)=20
      KD2=K/2
      P(I,J,K)=P_REF
      T(I,J,K)=T_0
      GF(I,J,K,IYF)=YF_0
      GF(I,J,K,IYCO2)=YP1_0
      GF(I,J,K,IYH2O)=YP2_0
      GF(I,J,K,IYO2)=YOX_0
      GF(I,J,K,IYN2)=YN2_0
      !CALL WTDN(I,J,K,WT0J)
      call dnst_calc(i,j,k)
      UG(I,J,K,1)=U_0
      UG(I,J,K,2)=V_0
      UG(I,J,K,3)=W_0
      CALL TKE(I,J,K)
      CALL THER(I,J,K)
      !CALL ENTH(I,J,K,1)  !  8/1/05 separated ENTH into 2 routines
      call T_to_enth(i,j,k) !calculate enthalpy from temperature

      !----------------------------------------------------------------------
      !     Droplet/Solid flow
      !----------------------------------------------------------------------
      IF (NPHAS.NE.1) THEN
         ANR=ZERO
         DO L=1,NDNP
            DU(ID2,JD2,KD2,L,1)=DU_0(L)
            DU(ID2,JD2,KD2,L,2)=DV_0(L)
            DU(ID2,JD2,KD2,L,3)=DW_0(L)
            DT(ID2,JD2,KD2,L)=DT_0(L)
            DN(ID2,JD2,KD2,L)=DN_0(L)
            IF (L.GT.NDP0) THEN
               L1=L-NDP0
               DC(ID2,JD2,KD2,L1)=DC_0(L1)
            ENDIF
            ANR=ANR+DN(ID2,JD2,KD2,L)*RD3(L)
            G0=DN(ID2,JD2,KD2,L)*RD3(L)*THET0C
            AREA=AREA_C(I,J,K,IU)*AREA0
            IF (IU.EQ.2) THEN
               G0=G0*ABS(DV_0(L))*UG0*AREA
            ELSEIF (IU.EQ.3) THEN
               G0=G0*ABS(DW_0(L))*UG0*AREA
            ELSE
               G0=G0*ABS(DU_0(L))*UG0*AREA
            ENDIF
            IF (L.LE.NDP0) THEN
               FR_Q=FR_Q+G0*DDENS
               Q_IN=Q_IN+G0*DDENS*(CL*DT_0(L)*T0+H0_L)
            ELSE 
               FR_P=FR_P+G0*PDENS
               Q_IN=Q_IN+G0*PDENS*CL_P*DT_0(L)*T0
            ENDIF
            IF (L.EQ.NDP0) THEN
               TH_DP(ID2,JD2,KD2)=THET0C*ANR
               ANR=ZERO
            ENDIF
         ENDDO
         TH_PT(ID2,JD2,KD2)=THET0C*ANR
         THETA(I,J,K)=ONE-TH_DP(ID2,JD2,KD2)-TH_PT(ID2,JD2,KD2)
      ENDIF

      G0=THETA(I,J,K)*DNST(I,J,K)*DNST0
      AREA=AREA_C(I,J,K,IU)*AREA0
      IF (IU.EQ.2) THEN
         G0=G0*ABS(V_0)*UG0*AREA
      ELSEIF (IU.EQ.3) THEN
         G0=G0*ABS(W_0)*UG0*AREA
      ELSE
         G0=G0*ABS(U_0)*UG0*AREA
      ENDIF
      FR_F=FR_F+G0*GF(I,J,K,IYF)
      !FR_FI(I)=FR_FI(I)+G0*GF(I,J,K,IYF)
      FR_OX=FR_OX+G0*GF(I,J,K,IYO2)
      FR_N2=FR_N2+G0*GF(I,J,K,IYN2)
      Q_IN=Q_IN+GF(I,J,K,IH)*H_0*G0 !This is overwritten later 
      q_h_in = q_h_in + GF(I,J,K,IH)*H_0*G0 !sum up mdot * h_in
   enddo;enddo;enddo

   IF (IU.EQ.1) THEN
      I=I1
      IF (K2.GT.K1) THEN
         DO K=K1+1,K2-1,2
            KP1=K+1
            KM1=K-1
            DO J=J1,J2,2
               F1=(Z(K)-Z(KM1))/DZ(K)
               CALL SBC1(I,J,KP1,I,J,KM1,I,J,K,F1)
            enddo
         enddo
      ENDIF
      IF (J2.GT.J1) THEN
         DO J=J1+1,J2-1,2
            JP1=J+1
            JM1=J-1
            DO K=K1,K2
               F1=(R(J)-R(JM1))/DR(J)
               CALL SBC1(I,JP1,K,I,JM1,K,I,J,K,F1)
            enddo
         enddo
      ENDIF
      IP1=I+1
      IM1=I-1
      DO J=J1,J2
      DO K=K1,K2
         IF (I.LT.MP.AND.IBCELL(IP1,J,K).EQ.2) CALL SBC3(I,J,K,IP1,J,K)
         IF (I.GT.2.AND.IBCELL(IM1,J,K).EQ.2) CALL SBC3(I,J,K,IM1,J,K)
      enddo;enddo

   ELSEIF (IU.EQ.2) THEN
      J=J1
      IF (K2.GT.K1) THEN
         DO K=K1+1,K2-1,2
            KP1=K+1
            KM1=K-1
            DO I=I1,I2,2
               F1=(Z(K)-Z(KM1))/DZ(K)
               CALL SBC1(I,J,KP1,I,J,KM1,I,J,K,F1)
            enddo
         enddo
      ENDIF
      IF (I2.GT.I1) THEN
         DO I=I1+1,I2-1,2
            IP1=I+1
            IM1=I-1
            DO K=K1,K2
               F1=(X(I)-X(IM1))/DX(I)
               CALL SBC1(IP1,J,K,IM1,J,K,I,J,K,F1)
            enddo
         enddo
      ENDIF
      JP1=J+1
      JM1=J-1
      DO I=I1,I2
      DO K=K1,K2
         IF (J.LT.NP.AND.IBCELL(I,JP1,K).EQ.2) CALL SBC3(I,J,K,I,JP1,K)
         IF (J.GT.2.AND.IBCELL(I,JM1,K).EQ.2) CALL SBC3(I,J,K,I,JM1,K)
      enddo;enddo

   ELSEIF (IU.EQ.3) THEN
      K=K1
      IF (J2.GT.J1) THEN
         DO J=J1+1,J2-1,2
            JP1=J+1
            JM1=J-1
            DO I=I1,I2,2
               F1=(R(J)-R(JM1))/DR(J)
               CALL SBC1(I,JP1,K,I,JM1,K,I,J,K,F1)
            enddo
         enddo
      ENDIF
      IF (I2.GT.I1) THEN
         DO I=I1+1,I2-1,2
            IP1=I+1
            IM1=I-1
            DO J=J1,J2
               F1=(X(I)-X(IM1))/DX(I)
               CALL SBC1(IP1,J,K,IM1,J,K,I,J,K,F1)
            enddo
         enddo
      ENDIF
      KP1=K+1
      KM1=K-1
      DO I=I1,I2
      DO J=J1,J2
         IF (K.LT.LP.AND.IBCELL(I,J,KP1).EQ.2) CALL SBC3(I,J,K,I,J,KP1)
         IF (K.GT.2.AND.IBCELL(I,J,KM1).EQ.2) CALL SBC3(I,J,K,I,J,KM1)
      enddo;enddo
   ENDIF
enddo !end of inlet blocks

!----------------------------------------------------------------------
!     Get exhaust information
read (nu_sbc,'(a12)') title !reads in "Exhausts:"
read (nu_sbc,*) nex !number of exhausts
have_frac_exh_T=0
if (in_run==0) then
	allocate (exh_i1(nex),exh_j1(nex),exh_k1(nex),exh_i2(nex),exh_j2(nex),exh_k2(nex))
	allocate (exh_orient(nex),exh_direct(nex),exh_type(nex),exh_frac(nex),exh_fixed(nex))
endif
do n=1,nex
   read (nu_sbc,*) exh_i1(n),exh_j1(n),exh_k1(n),exh_i2(n),exh_j2(n),exh_k2(n)
   read (nu_sbc,*) exh_orient(n),exh_direct(n),exh_type(n),exh_frac(n),exh_fixed(n)
   if (exh_type(n)==0) have_frac_exh_T=1
enddo

!----------------------------------------------------------------------
!     Get the wall properties.  Note that there may not be
!     any wall properties provided.

READ (nu_sbc,'(A12)') TITLE !reads in "Wall Properties:"
ALLOCATE (TM1(1000),TM2(1000),TM3(1000),TM4(1000))
IBC0=1
DO N=1,1000
   READ (nu_sbc,*,ERR=910) I1,J1,K1,I2,J2,K2,IU
   IF (I1.LE.0) EXIT !a row of zeros marks the end of wall properties
   READ (nu_sbc,*) TM1(N),TM2(N),TM3(N),TM4(N)
   CALL SBC_B !set the IWALL array (if not called, then iwall is all zero)
ENDDO

910 continue !read error return point

IF (N.GT.1) THEN
   NWAL0=N-1
   IF (ALLOCATED(W1_D)) DEALLOCATE (W1_D,W1_K,W1_TA,W1_HA)
   ALLOCATE (W1_D(NWAL0),W1_K(NWAL0),W1_TA(NWAL0),W1_HA(NWAL0))
   DO N=1,NWAL0
      W1_D(N)=TM1(N)
      W1_K(N)=TM2(N)
      W1_HA(N)=TM3(N)
      W1_TA(N)=TM4(N)
   ENDDO
ENDIF
DEALLOCATE (TM1,TM2,TM3,TM4)

!set some default values for items in the radin namelist
NWL=1
T_A=350
W_D=0.31D+0
RI=1.0D+0
W_K=2
H_A=5
H_G=10
EPS_W=0.8d0
eps_m=0.95d0
eps_c=0.8d0
nradwall=6
maxri1=100
maxri2=1000
minri2=4
preset_vf=1

READ (nu_sbc,NML=RADIN) !get the RADIN namelist:
! ri       refraction index, always =1 from GUI
! nwl      number of spectral bands, from GUI
! id_rad   >0-> do radiation, =-1-> do not do radiation, from GUI
! t_a      ambient temperature, from GUI
! h_a      external wall heat transfer coefficient from GUI, used in twall routine
! h_g      internal wall heat transfer coefficient always =10 from GUI, used in twall routine
! w_k      wall conductivity from GUI 
! w_d      wall thickness from GUI 
! eps_w    wall emissivity from GUI, used to set akl array in rad0 routine
! eps_m    melt surface emissivity
! eps_c    ceiling or crown emissivity
! have_crown   1=> have a crown
! height_to_ceiling   z value at ceiling or start of crown
! nradwall =6 used in qrnf & twall routines
! maxri1    maximum radiosity solver outer loop 1 iteration number
! maxri2    maximum radiosity solver inner loop 2 iteration number
! minri2    minimum radiosity solver inner loop 2 cycles
! preset_vf 1=>view factors are all calculated once

!get the wavelength band set
if (itr_gas==0) ALLOCATE (WL(NWL))
READ (nu_sbc,*,ERR=940) WL
WL=WL*1.0D-6

CLOSE(nu_sbc) !done reading the SBC file

!Allocate permanent radiation temperature array
if (allocated(wall_rad_T) == .false. .and. id_rad > 0) then
   allocate (wall_rad_T(mp,np,lp))
   wall_rad_T=zero
endif   

DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   !restore inlet cell value
   IF (IBCELL(I,J,K).EQ.20) IBCELL(I,J,K)=2
enddo;enddo;enddo

Q_IN=Q_IN+FR_F*Q0*H_0 !overwrites value (mdot * h_in) with (fuel rate * heat value of fuel)  7/31/05
Q_F=FR_F*Q0*H_0*1.0D-6
GFIN=FR_F+FR_OX+FR_N2
GFIN0=GFIN/GMFR
IF (IDEBUG.EQ.2) THEN
   IF (FR_F.GT.ZERO) WRITE (20,*) '  FR_F:',FR_F,' kg/s'
   IF (FR_OX.GT.ZERO) WRITE (20,*) '  FR_OX:',FR_OX,' kg/s'
   IF (FR_N2.GT.ZERO) WRITE (20,*) '  FR_N2:',FR_N2,' kg/s'
   IF (FR_Q.GT.ZERO) WRITE (20,*) '  FR_Q:',FR_Q,' kg/s'
   IF (FR_P.GT.ZERO) WRITE (20,*) '  FR_P:',FR_P,' kg/s'
   WRITE (20,*) '  GFIN:',GFIN,' kg/s'
   WRITE (20,*) '  Q_IN:',Q_IN*1.0D-6,' MJ/s'
ENDIF

call est_out_mass_frac 

RETURN 

940   continue !error causes program to stop

WRITE (6,*) ' E04: Error in wave length band set in sbc file.'
call stop_run("Error in wave length band set in sbc file.")


!======================================================================
!======================================================================
!======================================================================
CONTAINS

SUBROUTINE SBC_B
! sets IWALL array values
! ibc0=1 on entry to routine
IF (ABS(IU).EQ.1) THEN
   IF (IU.EQ.1) THEN
      I20=2
      I0=-2
   ELSE
      I20=MP
      I0=2
   ENDIF
   DO J=J1,J2,2
   DO K=K1,K2,2
      DO I=I1,I20,I0
         IF (IBCELL(I,J,K).NE.IBC0) CYCLE
         IWALL(I/2,J/2,K/2)=N
         EXIT
      ENDDO
   ENDDO;ENDDO
ELSEIF (ABS(IU).EQ.2) THEN
   IF (IU.EQ.2) THEN
      J20=2
      J0=-2
   ELSE
      J20=NP
      J0=2
   ENDIF
   DO I=I1,I2,2
   DO K=K1,K2,2
      DO J=J1,J20,J0
         IF (IBCELL(I,J,K).NE.IBC0) CYCLE
         IWALL(I/2,J/2,K/2)=N
         EXIT
      ENDDO
   ENDDO;ENDDO
ELSEIF (ABS(IU).EQ.3) THEN
   IF (IU.EQ.3) THEN
      K20=2
      K0=-2
   ELSE
      K20=LP
      K0=2
   ENDIF
   DO I=I1,I2,2
   DO J=J1,J2,2
      DO K=K1,K20,K0
         IF (IBCELL(I,J,K).NE.IBC0) CYCLE
         IWALL(I/2,J/2,K/2)=N
         EXIT
      ENDDO
   ENDDO;ENDDO
ENDIF
END SUBROUTINE SBC_B

END !end of SBC


!======================================================================
!======================================================================
!======================================================================
!     SBC1 interpolate between nodes
!======================================================================
SUBROUTINE SBC1(I1,J1,K1,I2,J2,K2,I,J,K,F1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

F2=ONE-F1
P(I,J,K)=F1*P(I1,J1,K1)+F2*P(I2,J2,K2)
T(I,J,K)=F1*T(I1,J1,K1)+F2*T(I2,J2,K2)
DO L=1,9
   IF (L.LE.3) UG(I,J,K,L)=F1*UG(I1,J1,K1,L)+F2*UG(I2,J2,K2,L)
   GF(I,J,K,L)=F1*GF(I1,J1,K1,L)+F2*GF(I2,J2,K2,L)
END DO

!CALL WTDN(I,J,K,WT0J)
call dnst_calc(i,j,k)
THETA(I,J,K)=F1*THETA(I1,J1,K1)+F2*THETA(I2,J2,K2)
TMU(I,J,K)=F1*TMU(I1,J1,K1)+F2*TMU(I2,J2,K2)
GCP(I,J,K)=F1*GCP(I1,J1,K1)+F2*GCP(I2,J2,K2)
GLAM(I,J,K)=F1*GLAM(I1,J1,K1)+F2*GLAM(I2,J2,K2)
GAMA(I,J,K)=F1*GAMA(I1,J1,K1)+F2*GAMA(I2,J2,K2)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     SBC3 assign other inlet values
!======================================================================
SUBROUTINE SBC3(I1,J1,K1,I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

DO L=1,3
   UG(I,J,K,L)=UG(I1,J1,K1,L)
ENDDO
CALL SBC4(I1,J1,K1,I,J,K)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE SBC4(I1,J1,K1,I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

P(I,J,K)=P(I1,J1,K1)
T(I,J,K)=T(I1,J1,K1)
DO L=1,9
   GF(I,J,K,L)=GF(I1,J1,K1,L)
END DO
DNST(I,J,K)=DNST(I1,J1,K1)
THETA(I,J,K)=THETA(I1,J1,K1)
TMU(I,J,K)=TMU(I1,J1,K1)
GLAM(I,J,K)=GLAM(I1,J1,K1)
GCP(I,J,K)=GCP(I1,J1,K1)
GAMA(I,J,K)=GAMA(I1,J1,K1)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     SBC5 assign wall values
!======================================================================
SUBROUTINE SBC5(I1,J1,K1,I2,J2,K2)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

P(I2,J2,K2)=P(I1,J1,K1)
T(I2,J2,K2)=T(I1,J1,K1)
DO L=1,9
   IF (L.LE.3) UG(I2,J2,K2,L)=ZERO
      !  IF (K.EQ.IYF.OR.K.EQ.IYCO2.OR.K.EQ.IYH2O.OR.K.EQ.IYO2.OR.K.EQ.IYN2) THEN
      !            GF(I2,J2,K)=ZERO
      !  ELSE
   GF(I2,J2,K2,L)=GF(I1,J1,K1,L)
      !  END IF
END DO
DNST(I2,J2,K2)=DNST(I1,J1,K1)
THETA(I2,J2,K2)=THETA(I1,J1,K1)
TMU(I2,J2,K2)=TMU(I1,J1,K1)
GLAM(I2,J2,K2)=GLAM(I1,J1,K1)
GCP(I2,J2,K2)=GCP(I1,J1,K1)
GAMA(I2,J2,K2)=GAMA(I1,J1,K1)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     est_out_mass_frac
! 
!     estimate outlet mass fractions of major species 
!     based on assumption of complete combustion
!     with products well mixed
!
!    Reaction: CH4 + (2+m)O2 + n N2 ---> CO2 + 2 H2O + m O2 + n N2  
!
!======================================================================
subroutine est_out_mass_frac
use gbl_var
implicit double precision (a-h,o-z)

YN2_out_est  = fr_N2/gfin !same as in
YH2O_out_est = (2*fr_f*wh2o/wf)/gfin
YCO2_out_est = (  fr_f*wco2/wf)/gfin
YO2_out_est  = (fr_ox-2*fr_f*wo2/wf)/gfin !estimated O2 outlet mass fraction

return
end

