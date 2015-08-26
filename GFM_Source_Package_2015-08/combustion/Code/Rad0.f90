!======================================================================
!======================================================================
!======================================================================
! RAD0.F90
!
!     rad0   sets akl for all cells   
!            calls rad_gas and debx for open cells
!                  for full rad, calculates total emission from soot and from h2o & co2
!                  for qe_only rad, calculates volume emission in qe2   
!
!======================================================================
SUBROUTINE RAD0
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!  REAL*8, ALLOCATABLE :: TGS(:,:)
!----------------------------------------------------------------------
!     RI: Refraction index
!     NWL: Number of spectral bands
!     WL:  Wavelength (m)
!----------------------------------------------------------------------

ALLOCATE (AKL(MZ,NZ,LZ,NWL),E0(NWL))
AKL=zero
E0=zero
ALLOCATE (AKLG(NWL),AKLS(NWL),AKL0(NWL))
AKLG=zero
AKLS=zero
AKL0=zero
ALLOCATE (DEB(NWL),DEB0(NWL))
DEB=zero
DEB0=zero
!---  WL: wavelength (m)
SGN=SIG*RI*RI
DEB0(1:NWL)=HCK/RI/WL(1:NWL)
!----------------------------------------------------------------------
!        PRESSURE, TEMP, CO2, H2O, SOOT
!----------------------------------------------------------------------

!T0=1.4D+3 ! 8/29/05: not used in RAD, does not match CFD part

QEL_MN=1.0D+0
TG_MN=300

!----------------------------------------------------------------------
!CSL  TG=T0
!CSL  PG=P0_R
!CSL  FH2O=0
!CSL  FCO2=0
!CSL  smf=0
!CSL  NU=10
!CSL      FILENAME='IT'//RUNUM//'R.DAT'
!CSL      OPEN(NU,FILE=FILENAME)
!CSL  READ (NU,'(T30,I5)') ITN
!CSL  READ (NU,'(T30,F10.0)') Q_IN
!CSL  READ (NU,'(T30,F10.0)') Q_LS
!CSL  READ (NU,'(T30,F10.0)') QLS_S
!CSL  if(qls_s.eq.zero) qls_s=0.75*q_ls
!CSL      FMX='(G11.3,11F11.3)'
!CSL      CALL RFMT(PG)
!CSL      FMX='(G11.3,11F11.1)'
!CSL      CALL RFMT(TG)
!CSL      FMX='(G11.3,11F11.4)'
!CSL      CALL RFMT(FH2O)
!CSL      CALL RFMT(FCO2)
!CSL      FMX='(G11.3,11E11.4)'
!CSL      CALL RFMT(smf)
!CSL      CLOSE(NU)
!CSLcbg  smf=smf*SVF_F
!CSL      smf=ZERO
!CSLC----------------------------------------------------------------------
!CSL  ALLOCATE (TGS(MZ,NZ))
!CSL      FILENAME='IT'//RUNUM//'T.DAT'
!CSL      INQUIRE (FILE=FILENAME,EXIST=EXST)
!CSL  IF (EXST) THEN
!CSL        OPEN(NU,FILE=FILENAME)
!CSL    READ (NU,'(/////)')
!CSL        J1=2
!CSL150     J2=J1+20
!CSL        J2=MIN(J2,NP)
!CSL        READ (NU,'(A32)') TITLE
!CSL    J1D2=J1/2
!CSL    J2D2=J2/2
!CSL        DO I=2,MP,2
!CSL       ID2=I/2
!CSL           READ (NU,'(T12,11F11.1)') (TGS(ID2,JD2),JD2=J1D2,J2D2)
!CSL        ENDDO
!CSL        J1=J2+2
!CSL        IF (J1.LE.NP) GOTO 150
!CSL        CLOSE(NU)
!CSL      ELSE
!CSL    DO J=1,NZ
!CSL    DO I=1,MZ
!CSL       TGS(I,J)=1650.0D0
!CSL        ENDDO
!CSL    ENDDO
!CSL      ENDIF
!CSL      DO 160 I=2,MP,2
!CSL  ID2=I/2
!CSL      DO 160 J=2,NP,2
!CSL  JD2=J/2
!CSL      DO 160 K=2,LP,2
!CSL     IF (IBCELL(I,J,K).NE.4) GOTO 160
!CSL     KD2=K/2
!CSL     TG(ID2,JD2,KD2)=TGS(ID2,JD2)
!CSL160   CONTINUE
!CSL  DEALLOCATE (TGS)
!----------------------------------------------------------------------

tot_qe_h2o_co2 =zero !Total emission from h2o and co2
tot_qe_soot    =zero !Total emission from soot

DO I=2,MP,2; ID2=I/2
DO J=2,NP,2; JD2=J/2
DO K=2,LP,2; KD2=K/2
   T_RAD=T(I,J,K)
   IF (IBCELL(I,J,K).LE.0.AND.T_RAD.GE.TG_MN) THEN
      !open cell
      TT0=SQRT(T_RAD/T0_R) !t0_r=1.0d+2
      P_RAD=(ONE+P(I,J,K))*PG0/P0_R  !p0_r=1.01325d+5
      call wmix_ms_calc(i,j,k) !calc mixture molecular weight
      YCO2=GFM(I,J,K,5)*wmix_ms/wco2
      YH2O=GFM(I,J,K,4)*wmix_ms/wh2o
      FVS=smf(I,J,K)*DNST(I,J,K)*DNST0/dnst_soot !fvs = soot volume fraction
      AKL(ID2,JD2,KD2,1:NWL)=zero
      AKL0=zero
      VOL=VOL_C(I,J,K)
      SA=((DX(I)*DR(J))+(DR(J)*DZ(K))+(DX(I)*DZ(K)))*TWO !cell surface area
      IF (SA.LE.SMALL16) CYCLE
      OPL=6.0D+0*VOL/SA 
      IF (OPL.LE.1.0D-10) CYCLE

      CALL RAD_GAS !calculate gas absorptivity, akl0=alkg+akls

      call debx(T_rad)
      area_sgn_t4 = sa*sgn*T_rad**4

      if (id_rad==10) then !doing full radiation calculation
         !Want to know total emission from soot and from h2o & co2
         !Also need to set akl for use in qrnf & qref routines
         sum_deb_aklg=zero
         sum_deb_akls=zero
         do l=1,nwl
            akl(id2,jd2,kd2,l)=emf(akl0(l),opl)
            sum_deb_aklg = sum_deb_aklg + deb(l)*emf(aklg(l),opl)
            sum_deb_akls = sum_deb_akls + deb(l)*emf(akls(l),opl)
         enddo
         tot_qe_h2o_co2 = tot_qe_h2o_co2 + (area_sgn_t4 * sum_deb_aklg)
         tot_qe_soot    = tot_qe_soot    + (area_sgn_t4 * sum_deb_akls)    
      else !doing qe only calculation
         !Only want volume radiation emission in qe2 
         sum_deb_akl=zero                
         do l=1,nwl
            sum_deb_akl = sum_deb_akl + deb(l)*emf(akl0(l),opl)
         enddo
         qe2(id2,jd2,kd2) = area_sgn_t4 * sum_deb_akl
         qeg0=qeg0+qe2(id2,jd2,kd2)
      endif
   ELSEIF (IBCELL(I,J,K).EQ.4) THEN !melt surface
      AKL(ID2,JD2,KD2,1:NWL)=eps_m 
   ELSEIF (IBCELL(I,J,K).GT.100) THEN
      !wall case: emissivity=absorptivity at all wavelengths = user input scaler 
      if (z(k)>=height_to_ceiling .or. (k==lp .and. have_crown==0)) then
         akl(id2,jd2,kd2,1:nwl)=eps_c !ceiling or crown emissivity
      else
         akl(id2,jd2,kd2,1:nwl)=eps_w !wall emissivity
      endif
   ENDIF
enddo;enddo;enddo
RETURN
END


