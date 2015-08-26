! SPRINT.F90
!======================================================================
!     to print computed results of the system state of a two-phase flow
!        9/00
!======================================================================
SUBROUTINE SPRINT
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
FMR='(/2X,''  X  /  Y  '',11(G13.5))'
FMX='(G13.5,11E13.5)'
FMZ="(/' Z(',I3,') = ',G14.5)"

filename=casedir//'\rt'//runum//'c.out'
OPEN(nu_rt,FILE=FILENAME)
CALL HPRINT
WRITE(nu_rt,901) YD4
901   FORMAT ("  C_O_2:H_2_O:O_2:N_2 = ",4F10.4)
UG0B=UG0*3.2808399D+0
TINFB=T0*1.8D+0
M3=MP/2-1
N3=NP/2-1
MLAS=M3+1
NLAS=N3+1

!----------------------------------------------------------------------
! 
!        RES: residuals computed in the equation solver ADLBL
!----------------------------------------------------------------------
IF (IDEBUG.EQ.2) THEN
   WRITE(20,*)' '
   WRITE(20,*)' Equation    Max Residual    Ave Residual'
   DO JR=1,13
      WRITE(20,*) JR,'    ',RESG(JR,1),'    ',RESG(JR,2)
   ENDDO
   IF (MS.GT.0) THEN
      DO JR=1,NSP0+1
         WRITE(20,*) JR,'    ',RESid_Ms(JR)
      ENDDO
   ENDIF
ENDIF

!----------------------------------------------------------------------
!     Print title
!----------------------------------------------------------------------
IF (STEADY) THEN
   WRITE(nu_rt,221)
ELSE
   WRITE(nu_rt,222)ISTEP,TM
ENDIF
221   FORMAT(/5X,'"STEADY-STATE RESULTS OVER FLOWFIELD"')
222   FORMAT(/5X,'"RESULTS OVER FLOWFIELD',5X,'TIME STEP =',I3,' TIME=',F7.3,'"')

!----------------------------------------------------------------------
!     Print gas flow properties
!----------------------------------------------------------------------
IF (IDEBUG.EQ.1) THEN 
   NX=1
ELSE
   NX=2
ENDIF
WRITE(nu_rt,301)PG0,ISTEP
CALL PFORM(P,NX)
!WRITE(nu_rt,303)T0,TINFB,ISTEP
WRITE(nu_rt,303)1.0d+0,1.0d+0,ISTEP
bs=T*T0
CALL PFORM(bs,NX)
!CALL PFORM(T,NX)
WRITE(nu_rt,306)UG0,UG0B,ISTEP
CALL PFORMU(1,NX)
WRITE(nu_rt,307)UG0,UG0B,ISTEP
CALL PFORMU(2,NX)
IF (LP.GE.6) THEN
   WRITE(nu_rt,308)UG0,UG0B,ISTEP
   CALL PFORMU(3,NX)
ENDIF
  WRITE(nu_rt,304)CP0*(T0-TL0),ISTEP
  CALL PFORM0(IH,NX)
IF (IDEBUG.EQ.2) THEN
  WRITE(nu_rt,302)DNST0,ISTEP
  CALL PFORM(DNST,NX)
ENDIF
!cbg      WRITE(nu_rt,309)ISTEP
!cbg      CALL PFORM(THETA,NX)
301   FORMAT(/5X,'"Pressure, (P(I,J)-PG0)/PG0, PG0 =',G11.4,' kpa, TSTEP',I3,'"')
302   FORMAT(/5X,'"Density, DNST(I,J,K)/DNST0, DNST0 =',G11.4,' kg/m**3, TSTEP',I3,'"')
!303   FORMAT(1X/T10,'"Gas Temperature, T(I,J,K)/T0, T0 =',G11.4,
303   FORMAT(1X/T10,'"Gas Temperature, T(I,J,K), T0 =',G11.4,' K =',G11.4,' R, TIME STEP ',I3,'"')
304   FORMAT(1X/T10,'"Gas Enthalpy, H(I,J)/HG0, HG0 =',G11.4,' J/kg, TSTEP',I3,'"')
306   FORMAT(1X/T10,'"X-VELOCITY/UG0,INTERPOLATED CELL CENTER UG0 =', &
      G11.4,' (m/s) =',G11.4,' (ft/s), TSTEP',I3,'"')
307   FORMAT(1X/T10,'"Y-VELOCITY/UG0, INTERPOLATED CELL CENTER  UG0=', &
      G11.4,' (m/s) =',G11.4,' (ft/s), TIME STEP',I3,'"')
308   FORMAT(1X/T10,'"Z-VELOCITY/UG0, UG0=',G11.4,' (m/s) =',G11.4,' (ft/s), TIME STEP',I3,'"')
309   FORMAT(/5X,'"Gas Volume Fraction (THETA),  TSTEP',I3,'"')

!----------------------------------------------------------------------
IF (IDEBUG.EQ.2) THEN
   IF (IYF.GE.LSTAR.AND.IYF.LE.LEND) THEN
      WRITE(nu_rt,331)ISTEP
      CALL PFORM0(IYF,NX)
   ENDIF
   WRITE(nu_rt,332)ISTEP
   CALL PFORM0(IYO2,NX)
   WRITE(nu_rt,333)ISTEP
   CALL PFORM0(IYN2,NX)
   WRITE(nu_rt,786)
   DO I=1,MP
   DO J=1,NP
   DO K=1,LP
      IF (GF(I,J,K,IYO2).LT.1.0D-6) THEN
         BS(I,J,K)=zero
      ELSE
         BS(I,J,K)=GF(I,J,K,IYF)/GF(I,J,K,IYO2)
      ENDIF
   enddo;enddo;enddo
   CALL PFORM(BS,2)
   IF (REACT) THEN
      WRITE(nu_rt,334)ISTEP
      CALL PFORM0(IYCO2,NX)
      WRITE(nu_rt,335)ISTEP
      CALL PFORM0(IYH2O,NX)
   ENDIF

!   WRITE(nu_rt,785)
!   qsurface=0.0
!   DO I=2,MP,2
!   DO J=2,NP,2
!   DO K=2,LP,2
!      IF(QRN(I,J,K).GT.1.0D-8) THEN
!         BG=ZERO
!      ENDIF
!      IF(DX(I).EQ.ZERO) THEN
!         DX(I)=1.0D+20
!      ENDIF
!      IF(DR(J).EQ.ZERO) THEN
!         DR(J)=1.0D+20
!      ENDIF
!      !cbg   BS(I,J,K)=QRN(I,J,K)*GMFR*CP0*(T0-TL0)/R0**3
!      !cbg        BS(I,J,K)=BS(I,J,K)*DX(I)*DR(J)*DZ(K)/(DX(I)*DR(J)) 
!      BS(I,J,K)=QRN(I,J,K)  
!      if (k.eq.6) then
!         !               why doing k=6,  isn't surface at k=2?   
!         qsurface=qsurface+bs(i,j,k)*DX(I)*DR(J)
!      endif
!   enddo;enddo;enddo
!   CALL PFORM(BS,2)
ENDIF

IF (MS.NE.0) THEN
   WRITE(nu_rt,787)
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      BS(I,J,K)=smf(I,J,K)*DNST(I,J,K)*DNST0/dnst_soot
   enddo;enddo;enddo
   CALL PFORM(BS,2)
ENDIF
iprnt_smf=1
IF (MS.NE.0.and.iprnt_smf==1) THEN
   WRITE(nu_rt,788)
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      BS(I,J,K)=smf(I,J,K)
   enddo;enddo;enddo
   CALL PFORM(BS,2)
ENDIF
331   FORMAT(1X/T10,'Species Mass Fraction (kg of species/kg of mix)',/T10,'"Fuel,   TSTEP',I3)
332   FORMAT(1X/T10,'"Oxidizer,   TSTEP',I3) 
333   FORMAT(1X/T10,'"Nitrogen,   TSTEP',I3)
334   FORMAT(1X/T10,'"Product 1,  TIME STEP',I3)
335   FORMAT(1X/T10,'"Product 2,   TSTEP',I3)
336   FORMAT(1X/T10,'"Soot Vol Frac,   TSTEP',I3) 
337   FORMAT(1X/T10,'"Emissivity')
338   FORMAT(1X/T10, '"Emissivity 2')
777   FORMAT(1X/T10, '"Emissivity 3')
778   FORMAT(1X/T10, '"Emissivity 4')
779   FORMAT(1X/T10, '"Emissivity 5')
780   FORMAT(1X/T10, '"Emissivity 6')
781   FORMAT(1X/T10, '"Emissivity 7')
782   FORMAT(1X/T10, '"Emissivity 8')
783   FORMAT(1X/T10, '"Emissive Heat Flux')
784   FORMAT(1X/T10, '"Absorbed Heat Flux')
785   FORMAT(1X/T10, '"Net Heat Flux')
786   FORMAT(1X/T10, '"FUEL TO AIR RATIO')
787   FORMAT(1X/T10, '"Soot Vol Frac')
788   FORMAT(1X/T10, '"Soot Mass Frac')

!----------------------------------------------------------------------
!cbg      WRITE(nu_rt,341)ISTEP
!cbg      CALL PFORM(TMU,NX)
!cbg      WRITE(nu_rt,342)ISTEP
!cbg      CALL PFORM0(IK,NX)
!cbg      WRITE(nu_rt,343)ISTEP
!cbg      CALL PFORM0(IEPS,NX)
IF (IDEBUG.EQ.2) THEN
   WRITE(nu_rt,344) 
   CALL PFORM(GCP,NX)
   WRITE(nu_rt,345) 
   CALL PFORM(GLAM,NX)
   WRITE(nu_rt,346) 
   CALL PFORM(GAMA,NX)
ENDIF
341   FORMAT(1X/T10,'"TURB. VISC., TMU(I,J,K), TSTEP',I3,'"')
342   FORMAT(1X/T10,'"TURB. KINETIC ENERGY, GF(I,J,K,IK), TIME STEP',I3,'"')
343   FORMAT(1X/T10,'"TURB. DISSIPATION RATE GF(I,J,K,IEPS), TIME STEP',I3,'"')
344   FORMAT(1X/T10,'"GCP(I,J,K)"')
345   FORMAT(1X/T10,'"GLAM(I,J,K)"')
346   FORMAT(1X/T10,'"GAMA(I,J,K)"')

!----------------------------------------------------------------------
FR_0=FR_F+FR_OX+FR_N2
WRITE (nu_rt,*) ' '
WRITE (nu_rt,*) '  Inlet mass flow rates (kg/s)'
WRITE (nu_rt,*) '   fuel, oxygen, nitrogen, and total'
WRITE (nu_rt,551) FR_F,FR_OX,FR_N2,FR_0
WRITE (nu_rt,*) '  Mass flow rates (kg/s)'
WRITE (nu_rt,*) '       east, west'
WRITE(nu_rt,552) (J,FLY(J,1),FLY(J,2),J=3,NPM1,2)
WRITE(nu_rt,552) (K,FLUX_Z(K,1),FLUX_Z(K,2),K=3,LPM1,2)
G0=1.0D-6
WRITE (nu_rt,*) ' '
WRITE (nu_rt,*) '  Inlet heat flow rates (MJ/s)'
WRITE (nu_rt,551) Q_IN*G0
WRITE (nu_rt,*) ' '
WRITE (nu_rt,*) '  Heat flow rates (MJ/s)'
WRITE (nu_rt,*) '      east, west'
WRITE(nu_rt,554) (J,FLYH(J,1)*G0,FLYH(J,2)*G0,J=3,NPM1,2)
551   FORMAT (/4F10.3/)
552   FORMAT (I5,2F10.2)
554   FORMAT (I5,2F10.3)
555   FORMAT (/'Average Exit Temperature = ',F10.1, 'K')
!csl      IF (NPHAS.EQ.1) GOTO 900

!----------------------------------------------------------------------
!     Print droplet flow properties
!----------------------------------------------------------------------
IF (FR_Q.gt.ZERO .and. ndnp>0) then
   WRITE(nu_rt,441)ISTEP
   WRITE(nu_rt,442)
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      BS(I,J,K)=TH_DP(I/2,J/2,K/2)
   enddo;enddo;enddo
   CALL PFORM(BS,2)
   DO L=1,NDP0
      WRITE(nu_rt,443) L,RD(L),TB(L)*T0
      DO M=1,5
         M1=M-1
         DO I=2,MP,2
         DO J=2,NP,2
         DO K=2,LP,2
            IF (M.EQ.1) BS(I,J,K)=DN(I/2,J/2,K/2,L)
            IF (M.GE.2.AND.M.LE.4) BS(I,J,K)=DU(I/2,J/2,K/2,L,M1)
            IF (M.EQ.5) BS(I,J,K)=DT(I/2,J/2,K/2,L)
         enddo;enddo;enddo
         IF (M.EQ.1) WRITE(nu_rt,444) L
         IF (M.EQ.2) WRITE(nu_rt,445) L
         IF (M.EQ.3) WRITE(nu_rt,446) L
         IF (M.EQ.4) WRITE(nu_rt,447) L
         IF (M.EQ.5) WRITE(nu_rt,448) L
         CALL PFORM(BS,2)
      enddo
   ENDDO
   441   FORMAT(1X/T10,'"Droplet Flow Properties, TSTEP',I3,'"')
   442   FORMAT(1X/T10,'"Droplet Volume Fraction"')
   443   FORMAT(1X/T20,'"Droplet Group',I2,', Size=',F6.3,' TB=',F6.1,' K"')
   444   FORMAT(1X/T10,'"Droplet # Density   DN(I,J,K,',I2,')"')
   445   FORMAT(1X/T10,'"Droplet X-Velocity  DU(I,J,K,',I2,')"')
   446   FORMAT(1X/T10,'"Droplet Y-Velocity  DV(I,J,K,',I2,')"')
   447   FORMAT(1X/T10,'"Droplet Z-Velocity  DW(I,J,K,',I2,')"')
   448   FORMAT(1X/T10,'"Droplet Temperature DT(I,J,K,',I2,')"')

   !----------------------------------------------------------------------
   !     Droplet combustion parameters
   !----------------------------------------------------------------------
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      BS(I,J,K)=EVP(I/2,J/2,K/2)
   enddo;enddo;enddo
   WRITE(nu_rt,471)
   CALL PFORM(BS,2)
   IF(REACT) THEN
      DO I=2,MP,2
      DO J=2,NP,2
      DO K=2,LP,2
         BS(I,J,K)=CRATE(I,J,K)
      enddo;enddo;enddo
      WRITE(nu_rt,472)
      CALL PFORM(BS,2)
   ENDIF
   471   FORMAT(1X/T10,'"EVAP RATE"')
   472   FORMAT(1X/T10,'"REACTION RATE"')
endif

!----------------------------------------------------------------------
!     Print particle flow properties
!----------------------------------------------------------------------
IF (FR_P.gt.ZERO .and. ndnp>0) then
   WRITE(nu_rt,541)ISTEP
   WRITE(nu_rt,542)
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      BS(I,J,K)=TH_PT(I/2,J/2,K/2)
   enddo;enddo;enddo
   CALL PFORM(BS,2)
   DO L1=1,NPT0
      L=L1+NDP0
      WRITE(nu_rt,543) L1,RD(L)
      DO M=1,6
         IF (M.EQ.6.AND..NOT.REACT) cycle
         M1=M-1
         DO I=2,MP,2
         DO J=2,NP,2
         DO K=2,LP,2
            IF (M.EQ.1) BS(I,J,K)=DN(I/2,J/2,K/2,L)
            IF (M.GE.2.AND.M.LE.4) BS(I,J,K)=DU(I/2,J/2,K/2,L,M1)
            IF (M.EQ.5) BS(I,J,K)=DT(I/2,J/2,K/2,L)
            IF (M.EQ.6) BS(I,J,K)=DC(I/2,J/2,K/2,L1)
         enddo;enddo;enddo
         IF (M.EQ.1) WRITE(nu_rt,544) L1
         IF (M.EQ.2) WRITE(nu_rt,545) L1
         IF (M.EQ.3) WRITE(nu_rt,546) L1
         IF (M.EQ.4) WRITE(nu_rt,547) L1
         IF (M.EQ.5) WRITE(nu_rt,548) L1
         IF (M.EQ.6) WRITE(nu_rt,549) L1
         CALL PFORM(BS,2)
      enddo
   ENDDO
   541   FORMAT(1X/T10,'"Particle Flow Properties, TSTEP',I3,'"')
   542   FORMAT(1X/T10,'"Particle Volume Fraction"')
   543   FORMAT(1X/T20,'"Particle Group',I2,', Size='F6.3,' "')
   544   FORMAT(1X/T10,'"Particle # Density   DN(I,J,K,',I2,')"')
   545   FORMAT(1X/T10,'"Particle X-Velocity  DU(I,J,K,',I2,')"')
   546   FORMAT(1X/T10,'"Particle Y-Velocity  DV(I,J,K,',I2,')"')
   547   FORMAT(1X/T10,'"Particle Z-Velocity  DW(I,J,K,',I2,')"')
   548   FORMAT(1X/T10,'"Particle Temperature DT(I,J,K,',I2,')"')
   549   FORMAT(1X/T10,'"COKE (kg/kg carrier) DC(I,J,K,',I2,')"')
endif

!----------------------------------------------------------------------
!     Print subspecies concentration
!----------------------------------------------------------------------
IF (MS.eq.1) then
   WRITE(nu_rt,601)ISTEP
   DO L=LSTARM,LENDM
      IF(L.EQ.1) THEN
        WRITE(nu_rt,607)
      ELSEIF(L.EQ.2) THEN
        WRITE(nu_rt,608)
      ELSEIF(L.EQ.4) THEN
        WRITE(nu_rt,604)
      ELSEIF(L.EQ.5) THEN
        WRITE(nu_rt,605)
      ELSEIF(L.EQ.3) THEN
        WRITE(nu_rt,603)
      ELSEIF(L.EQ.6) THEN
        WRITE(nu_rt,609)
      ELSEIF(L.EQ.7) THEN
        WRITE(nu_rt,606)
      ELSE
        WRITE(nu_rt,602) L
      ENDIF
      CALL PFORMM(L,NX)
   ENDDO
   601   FORMAT(1X/T10,'Subspecies Mass Fraction (kg/kg of mixture),   TSTEP',I3)
   602   FORMAT(1X/T10,'"Subspecies ',I3)
   603   FORMAT(1X/T10,'"CO conc')
   604   FORMAT(1X/T10,'"H2O conc')
   605   FORMAT(1X/T10,'"CO2 conc')
   606   FORMAT(1X/T10,'"NO conc')
   607   FORMAT(1X/T10,'"CH4 conc')
   608   FORMAT(1X/T10,'"O2 conc')
   609   FORMAT(1X/T10,'"N2 conc')

      !----------------------------------------------------------------------
      !     Radiation
      !----------------------------------------------------------------------
   WRITE(nu_rt,"(1X/T10,'""Rad_Emis Power, QE0 = 1 W')")
   CALL PFORM1(QE)
   WRITE(nu_rt,"(1X/T10,'""Rad_Absor Power, QA0 = 1 W')")
   CALL PFORM1(QA)
   !CALL PFORM1(QA1)
endif


!----------------------------------------------------------------------
!  3-31-2005 commented out following code because it is dead code
!     the goto simply branches around it.

!  goto 108
!  IF (ITN.LE.1.and.itr_gas.lt.next_rad) THEN
!         FILENAME='IT'//RUNUM//'M.DAT'
!         OPEN(nu_rt,FILE=FILENAME)
!     WRITE(nu_rt,'(" No. of Iteration:",T30,I5)') ITN
!     WRITE(nu_rt,'(" Total Heat (W) =",T30,F20.10)') Q_IN
!     WRITE(nu_rt,'(" Heat Loss by Radiation (W) =",T30,F20.10)') Q_LS
!     WRITE(nu_rt,'(" Heat Transfer to Melter (W) =",T30,F20.10)') QLS_S
!     WRITE(nu_rt,'(" Grid: ",2I4)') MP,NP
!     WRITE(nu_rt,'(I5,E15.8)') (I,X(I),I=1,MP)
!     WRITE(nu_rt,'(I5,E15.8)') (J,R(J),J=1,NP)
!     WRITE(nu_rt,'(1X/T10,"Surface Radiation Heat Flux (J/M**2)")')
!         J1=2
!720      J2=J1+20
!         J2=MIN(J2,NP)
!         WRITE(nu_rt,FMR) (R(J),J=J1,J2,2)
!         DO I=2,MP,2
!            WRITE(nu_rt,'(F11.3,11E11.4)') X(I),(QRS(I,J),J=J1,J2,2)
!         ENDDO
!         J1=J2+2
!         IF (J1.LE.NP) GOTO 720
!         CLOSE(nu_rt)
!  ENDIF
!      FILENAME='IT'//RUNUM//'R.DAT'
!      OPEN(nu_rt,FILE=FILENAME)
!  WRITE(nu_rt,'(" No. of Iteration:",T30,I5)') ITN
!  WRITE(nu_rt,'(" Total Heat (W) =",T30,F20.10)') Q_IN
!  WRITE(nu_rt,'(" Heat Loss by Radiation (W) =",T30,F20.10)') Q_LS
!  WRITE(nu_rt,'(" Heat Transfer to Melter (W) =",T30,F20.10)') QLS_S
!  DO 730 I=2,MP,2
!  DO 730 J=2,NP,2
!  DO 730 K=2,LP,2
!     P(I,J,K)=(ONE+P(I,J,K))*PG0
!     T(I,J,K)=T(I,J,K)*T0
!     smf(I,J,K)=smf(I,J,K)*DNST(I,J,K)*DNST0/dnst_soot
!730   CONTINUE
!      FMX='(G11.3,11F11.1)'
!  WRITE(nu_rt,'(1X/T10,"Pressure (Pa)")')
!      CALL PFORM(P,2)
!      FMX='(G11.3,11F11.2)'
!  WRITE(nu_rt,'(1X/T10,"Temperatue (K)")')
!      CALL PFORM(T,2)
!  WRITE(nu_rt,'(1X/T10,"H2O Concentration")')
!      FMX='(G11.3,11F11.6)'
!      CALL PFORMM(4,2)
!  WRITE(nu_rt,'(1X/T10,"CO2 Concentration")')
!      CALL PFORMM(5,2)
!  IF(MS.EQ.1) THEN
!  WRITE(nu_rt,'(1X/T10,"Soot Vol Frac")')
!      FMX='(G11.3,11E11.4)'
!      CALL PFORM(smf,2)
!  ENDIF
!     CLOSE(nu_rt)
!108   CONTINUE

CLOSE(nu_rt)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     PFORM prints system state in general format
!
!	Note: If Pform is called outside the Sprint module, then before the
!	call the file unit nu_rt must be changed to be the value desired 
!	and after the call nu_rt must be restored!
!======================================================================
SUBROUTINE PFORM(F,NX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION F(MP,NP,LP)

DO I=1,MP
DO J=1,NP
DO K=1,LP
   IF (ABS(F(I,J,K)).LT.1.0D-50) THEN
      F(I,J,K)=ZERO
   ENDIF
enddo;enddo;enddo

IF (LP.LE.4) THEN
   K1=2
   K2=2
ELSE
   K1=4
   K2=LP-2
ENDIF
DO K=K1,K2,NX
   WRITE(nu_rt,FMZ) K,Z(K)
   J1=2
   do
      J2=J1+10*NX
      J2=MIN(J2,NP)
      WRITE(nu_rt,FMR) (R(J),J=J1,J2,NX)
      DO I=2,MP,NX
         WRITE(nu_rt,FMX) X(I),(F(I,J,K),J=J1,J2,NX)
      ENDDO
      J1=J2+NX
      IF (J1.gt.NP) exit
   enddo
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     PFORM0 prints general system state in general format
!======================================================================
SUBROUTINE PFORM0(LGF,NX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DO I=2,MP,NX
DO J=2,NP,NX
DO K=2,LP,NX
   BS(I,J,K)=GF(I,J,K,LGF)
enddo;enddo;enddo
CALL PFORM(BS,NX)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     PFORMM prints subspecies concentration
!======================================================================
SUBROUTINE PFORMM(LGF,NX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DO I=2,MP,NX
DO J=2,NP,NX
DO K=2,LP,NX
   BS(I,J,K)=GFM(I,J,K,LGF)
   !cbg            BS(I,J,K)=GFM(I,J,K,LGF)*T(I,J,K)*T0*RU/WT0/10.0d+0
enddo;enddo;enddo
CALL PFORM(BS,NX)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     PFORMU prints general system state for velocities
!======================================================================
SUBROUTINE PFORMU(LU,NX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DO I=2,MP,NX
DO J=2,NP,NX
DO K=2,LP,NX
   BS(I,J,K)=UG(I,J,K,LU)
enddo;enddo;enddo
CALL PFORM(BS,NX)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     RFORM reads system state in general format
!======================================================================
SUBROUTINE RFORM(F)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION F(MP,NP,LP)
1     FORMAT (A40)

IF (LP.LE.4) THEN
   K1=2
   K2=2
ELSE
   K1=4
   K2=LP-2
ENDIF

DO K=K1,K2,2
   READ (7,1) TITLE
   READ (7,1) TITLE
   J1=2
   do
      J2=J1+20
      J2=MIN(J2,NP)
      READ (7,1) TITLE
      READ (7,1) TITLE
      DO I=2,MP,2
         READ (7,FMX) G0,(F(I,J,K),J=J1,J2,2)
      ENDDO
      J1=J2+2
      IF (J1.gt.NP) exit
   enddo
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE PFORM1(F)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION F(MZ,NZ,LZ)

DO I=2,MP,2;  ID2=I/2
DO J=2,NP,2;  JD2=J/2
DO K=2,LP,2;  KD2=K/2
   IBC0=IBCELL(I,J,K)
   IF (IBC0.LE.0) THEN
      VOL=VOL_C(I,J,K)*vol0
      F(ID2,JD2,KD2)=F(ID2,JD2,KD2)/VOL
   ENDIF
enddo;enddo;enddo

NX=2
IF (LP.LE.4) THEN
   K1=2
   K2=2
ELSE
   K1=4
   K2=LP-2
ENDIF

DO K=K1,K2,NX
   KD2=K/2
   WRITE(nu_rt,FMZ) K,Z(K)
   J1=2
   do
      J2=J1+10*NX
      J2=MIN(J2,NP)
      WRITE(nu_rt,FMR) (R(J),J=J1,J2,NX)
      J1D=J1/2
      J2D=J2/2
      DO I=2,MP,NX
         ID2=I/2
         WRITE(nu_rt,FMX) X(I),(F(ID2,JD2,KD2),JD2=J1D,J2D)
      ENDDO
      J1=J2+NX
      IF (J1.gt.NP) exit
   enddo
ENDDO
RETURN
END



