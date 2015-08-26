! SETUP.F90
!     SETUP initializes conditions and parameters,
!           sets control variables, and
!           calculates dimensionless parameters.
!     Rev: 6/01
!======================================================================
      SUBROUTINE SETUP
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      NAMELIST /INPUT/ IRSTYP,IRSTYPM,IBAKUP,IDEBUG,IDCAC, &
        NPHAS,IH,LSTAR,LEND,MS,NMSP,LSTARM,LENDM,MAXMS, &
        BGCON,BBCON,BPCON,MAXGI,MAXPI,MAXBI,MAXSI,H0_C,H0_S, &
        REACT,GRA,PG0, &
        !REACT,GRA,RF,PG0, &
        !GMFR, & !glass mass flow rate reference, no longer used
        !RLM0, !reference particle radius, no longer used
        T_MX,T_MN, &
        DS_C, & !density of cullet (kg/m**3)
        DS_S, & !density of sand (kg/m**3)
        !CL_C, & !cullet specific heat (J/kg K) !replaced with user defined function
        !CL_S, & !sand specific heat (J/kg K)   !replaced with user defined function
        !GDF0, & !glass mass diffusion reference
        !GK0,  & !glass thermal conductivity reference
        !GMU0, & !reference viscosity - not used, should be == 1
        U_MX,EBV0,I_ME,ITEND,KMEND,QEB0, &
        W_K,W_HA,H_G,W_TA,W_D,W_E,RI,ID_RAD,LRX,IRST,ISAV, &
        CD_D_C, CD_D_S, &
        iheat_flux_type, &
        i_out_bc_pull,T_exit,T_init,cycling, &
        gui_update, & 
        chamber_type, & !2=> melter with refiner
        melter_length, & !length of melter (not including throat nor refiner)
         isum, & !Summary data collection control flag
         iinfo, & !General Information data collection control flag
         iTave, & !Average Temperatures data collection control flag
         iconv, & !Mass Residual Convergence data collection control flag
         igresid, & !Equation Residuals data collection control flag
         igresidp, & !Pre-Solve Equation Residuals data collection control flag
         iTchg, & !Melt Surface Temperature Change data collection control flag
         iadjf, & !Melt Surface Adjusted Flux data collection control flag
         iadjr,  & !Melt Surface Flux Relaxation data collection control flag
         ifieldview !Fieldview post processor file output control flag

      !& CL_G !Obsolete now a user input property function of temperature

!----------------------------------------------------------------------
!     Constants
!       RU: universal gas constant (J/kmol/K)
!       GRA: gravitational acceleration (m/s**2)
!----------------------------------------------------------------------
      !SMALL=1.0D-20
      !BIG=1.0D+20
      !ZERO=0.0D+0
      !ONE=1.0D+0
      !HALF=0.5D+0
      !TWO=2.0D+0
      !THIRD=ONE/3.0D+0
      !PI=3.141592653589793D+0
      !PI2=PI*TWO
      !PI4=PI2*TWO
      !pi6=6.0d+0*pi
      !c4d3pi=(4.0d+0/3.0d+0)*pi
      !RU=8.3143D+3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!temp set for test
      !surf_length_c=6.096d0
      !surf_width_c=5.1308d0
      !surf_length_m=5.44d0
      !surf_width_m=4.93d0


      GRA(1)=ZERO
      GRA(2)=0   
      GRA(3)=-1.0D+0       
      !DS0=1    !No definition, what is this ? Lottes 12-16-05 @@@@
      !DS0=0    !No definition, what is this ? Lottes 12-16-05 @@@@
      F_BUOY=1D-2
      W_TA=350
      W_D=0.3D+0
      W_K=2
      W_HA=5
      RI=1.0D+0
      H_G=10
      W_E=0.95D+0
      ID_RAD=-1
      LRX=20
!----------------------------------------------------------------------
!     Misc control parameters
!----------------------------------------------------------------------
      !i_out_bc_pull=0 ! if 1, then set exit velocity based on inflow
      i_out_bc_pull=1 ! if 1, then set exit velocity based on inflow
                      ! and pull split ratios, namelist variable
      idrag_on=0 ! if 1, then drag between particles and glass is turned on
      comb_grid_exist = .false. !true if a combustion grid exists
      runstop_exist = .false. !true if a runstop file exists

!----------------------------------------------------------------------
!     Restart control parameters for long computational runs
!       IRSTYP: Restart type
!          0    Fresh start with no backup file
!          1    Restart by using data from the backup file
!       IBAKUP: Back-up function type
!          0    No back-up required
!          1    Back-up at the end of iterations
!----------------------------------------------------------------------
      IRSTYP=0
      IRSTYPM=0
      IBAKUP=1
      IDEBUG=0
      NLG=0
      N_PI=0
      IRST=.FALSE.
      ISAV=.TRUE.
      ! Glass quality index computation can be turned on via setting below via namelist
      iglass_qual=0 ! =1 compute glass quality indexes before stop
      iglass_qual_only=0 ! =1 compute glass quality indexes on restart and stop
!----------------------------------------------------------------------
!-----CONVERGENCE CRITERIA AND ITERATION LIMITS
!       BGCON,BBCON,BPCON,BTCON: the convergence criteria based on the
!         average mass residual for the glass, batch, bubble 
!         (steady) and 2-phase(transient), respectively.
!       MAXGI,MAXPI,MAXBI: the maximum number of iterations in the
!         glass, particle, and bubble calculations, respectively.
!       MAXSI: the maximum number of steady state iterations.
!----------------------------------------------------------------------
      BGCON=1.0D-25
      BBCON=1.0D-25
      BPCON=1.0D-25
      BMCON=1.0D-25
      BTCON=1.0D-9
      MAXGI=1
      MAXPI=1
      MAXBI=1
      MAXSI=1000
      MAXMS=100000
      MAXTI=3
!----------------------------------------------------------------------
!     Flow type parameters
!       NU:     I/O unit number for SPRINT and OXFLOW output
!       SMPLER: .TRUE. if for the simpler algorithm (false for SIMPLE)
!       STEADY: .TRUE. for steady state computations
!       SYM:    .TRUE. if the min R boundry is symmetric
!       EB_C:   .TRUE. electric booster calculation
!----------------------------------------------------------------------
      NU=6
      NPHAS=1
      STEADY=.TRUE.
      SMPLER=.TRUE.
      SYM=.FALSE.
      EB_C=.FALSE.
!CZEB----------------------------------------------------------------------
!     electric parameterts
!       ebsg0-electric conductivity (1/ohm-m)
!       tave-average temperature (K)
!----------------------------------------------------------------------
      !cz      EBSG0=8.9D+2
      !CZ    TAVE=1.4D+3
      IDCAC=1
      QEB0=1E+6
!----------------------------------------------------------------------
!     Equation indices
!      LSTAR: starting number of species calculation
!      LEND: end number of species calculation
!      MS: 0 = don't do sub-species calculation, otherwise do it
!      NMSP: number of sub-species
!      LSTARM: start index for sub-species calculation
!      LENDM:  end index for sub-species calculation
!      REACT:  .TRUE. if reaction takes place
!----------------------------------------------------------------------
      PG0=1.01325D+5 !melt surface reference pressure (Pa)
      REACT=.FALSE.
      IH=1
      LSTAR=1
      LEND=1
      IYLG=2
      MS=0 !no subspecies calculations
      NSP0=0
      NMSP=0 !no subspecies calculations
      LSTARM=0
      LENDM=0


!----------------------------------------------------------------------
      !cbg      U_MX=1.0D+0
      !U_MX=0.2D+0
      U_MX=5.0D+0
      T_MX=3.0D+3
      !T_MN=2.0D+2
      T_MN=2.5D+2
      TMLTR=1100.0D+0
      H_NEEDED=ZERO !Total energy needed in melter including wall loses [W] Lottes 5/19/05
      qheat_s_need=zero !Energy needed to heat sand to melting point [W] Lottes 5/19/05
      qheat_c_need=zero !Energy needed to heat cullet to melting point [W] Lottes 5/19/05
      qmelt_s_need=zero !Energy needed to melt sand [W] Lottes 5/19/05
      qmelt_c_need=zero !Energy needed to melt cullet [W] Lottes 5/19/05
      qglass_need=zero  !Energy needed to heat glass from melting point to T_exit [W] Lottes 5/19/05
      itn=0 ! indicator default value 
      T_exit=1750D+0 ! Mean glass exit temperature used as a B.C. for energy (namelist) Lottes April/05
      Q_COMB=ZERO
!----------------------------------------------------------------------
!-----PROPERTIES OF GLASS SPECIES
!       CP0: reference specific heat (J/kg/K)
!       GDF0: glass phase mass diffusion coefficient (?)
!       GK0: thermal conductivity (W/m K)
!       CL_G: glass phase specific heat (J/kg)
!       GMU0: glass molecular viscosity reference (Pa-s)   
!              
!----------------------------------------------------------------------
      !CP0=one
      !CP0=1.0057D+3
      !GDF0=1.0D-0
      !GK0=52.07D+0
      !GK0=one
      !cbg   GK0=17.78D+0
      GK0_PC=0.78D+0 !Thermal conductivity of cullet (W/m K) ?@@@@
      GK0_PS=0.78D+0 !Thermal conductivity of sand   (W/m K) ?@@@@
      !GMU0=36.49D-0
      !GMU0=1.0D-0
      !CL_G=1.2D+3 Lottes 5/6/05 obsolete, now a user input LG()%C
!----------------------------------------------------------------------
!-----TRANSIENT COMPUTATION PARAMETERS
!       MAXTS: the number of time steps to run
!       TP0: reference time in seconds
!       DT0: time step as fraction of TP0
!       IPCYC: is number of time steps between sprints
!       IFP: an indicator for full print in SPRINT
!       TPRINT: controls when to print state in unsteady calculations
!               initial value is always false.
!       ISTEP: time step number
!----------------------------------------------------------------------
      TPRINT=.FALSE.
      ISTEP=0
      MAXTS=200
      !TP0=2.0D-2
      !DT0=1.0D-2
      IPCYC=20
      IFP=1
!----------------------------------------------------------------------
!     Batch & Bubble Parameters
!       NPS_S: number of sand particle size groups
!       NPS_C: number of cullet particle size groups
!       NBS0: number of gas bubble size groups
!----------------------------------------------------------------------
      NPS0=0
      NBS0=0
      !RLM0=1.0D-3 !Lottes: The particle radius size in the sbc file set
      !by user input in the GUI is in mm, so the reference radius must be 0.001 m!
      !It should not be changed in the namelist, even though it is there! 
      filename=casedir//'\sbc'//runum//'m.dat'
      OPEN (nu_sbc,FILE=FILENAME)
      READ (nu_sbc,*) NPHAS
      IF (NPHAS.LE.1) GOTO 220
      !RLM0=1.0D-2 !Lottes: This will be overwritten by namelist variable read below!!!
      READ (nu_sbc,*) NPS_C,NPS_S,NBS0
      NPS0=NPS_S+NPS_C
      CALL ALCD1
      IF (NPS_C.EQ.1) THEN
         RP_C(1)=ONE      
      ELSEIF (NPS_C.GT.1) THEN
         RP_C(1)=0.875D0/NPS_C
         DO L=2,NPS_C
            RP_C(L)=RP_C(1)+(1.75D0*(L-1))/NPS_C
         ENDDO
      ENDIF
      IF (NPS_S.EQ.1) THEN
         RP_S(1)=ONE      
      ELSEIF (NPS_S.GT.1) THEN
         RP_S(1)=0.875D0/NPS_S
         DO L=2,NPS_S
            RP_S(L)=RP_S(1)+(1.75D0*(L-1))/NPS_S
         ENDDO
      ENDIF
      IF (NBS0.EQ.1) THEN
         RG_B(1)=ONE
      ELSEIF (NBS0.GT.1) THEN
         RG_B(1)=0.875D+0/NBS0
         DO L=2,NBS0
            RG_B(L)=RG_B(1)+(1.75D0*(L-1))/NBS0
         END DO
      ENDIF
!----------------------------------------------------------------------
!     Batch Properties
!       TM:  melting temperature (K)
!       DS:  batch density (kg/m**3)
!       CL:  specific heat (J/kg/K)
!       ELH: heat of batch melting (J/kg) !Lottes: no longer used
!----------------------------------------------------------------------
      !ELH=5.5D5 !Lottes 4/13/05: not used
      IF (NPS_C.GE.1) THEN
         READ (nu_sbc,*) TM_C
         READ (nu_sbc,*) RP_C
         TMLTR=TM_C(1)
         DO L=2,NPS_C
            ! This sets the melting point to the lowest among size groups
            TMLTR=MIN(TM_C(L),TMLTR)
         ENDDO
         !ELSE !Lottes 4/13/05
         !cbg     TMLTR=TM_S(1) !Lottes 4/13/05: this needs to be after following reads
      ENDIF
      IF (NPS_S.GE.1) THEN
         READ (nu_sbc,*) TM_S
         READ (nu_sbc,*) RP_S
         if (nps_c < 1) tmltr=tm_s(1)
         DO L=2,NPS_S
            ! This sets the melting point to the lowest among size groups
            TMLTR=MIN(TM_S(L),TMLTR)
         ENDDO
      ENDIF
      IF (NBS0.GE.1) THEN
         READ (nu_sbc,*) (RG_B(L),L=1,NBS0)
      ENDIF
      PCA%DS=2.2D+3 !Default density of cullet (kg/m**3)
      PSA%DS=2.2D+3 !Default density of sand (kg/m**3)
      DS_C=0
      T_init=Tmltr+6.0d+0 !Initial guess Temperature in melt tank
                          !Can be over-ridden via namelist input


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
      !Lottes: Namelist input read here can change many parameters
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  


220   READ (nu_sbc,NML=INPUT)
      !IF (DS_C.GT.0) THEN
      !   PCA%DS=DS_C
      !   PSA%DS=DS_S !@@@ check this
      !ENDIF
      !Below appears to be correct, Lottes 8/8/05
      IF (DS_C > 0) PCA%DS=DS_C
      IF (DS_s > 0) PSA%DS=DS_S 
      IF (IDEBUG.EQ.1) OPEN(20,FILE='DEBUG.OUT')

      if (iheat_flux_type == 2 .or. iheat_flux_type == 4) then
         !Heat flux is provided by combustion calculation.
         !Must have combustion grid available to interpolate between domains
         filename=casedir//'\gd'//runum//'c.dat'
         inquire(file=filename,exist=exst)
         if (.not.exst) then
            write (6,*) '  Fatal error, file: ',filename,' does not exist.'
            call stop_run("Fatal error, combustion grid file must be copied to melt case.")
         endif
         if (regen == 1) then
            filename=casedir//'\gd'//runum_r//'c.dat'
            inquire(file=filename,exist=exst)
            if (.not.exst) then
               write (6,*) '  Fatal error, file: ',filename,' does not exist.'
               call stop_run("Fatal error, second regenerative combustion grid file must be copied to melt case.")
            endif
         endif
      endif

      if (cycling==1) then
         filename=casedir//'\cycleInfo.txt'
         inquire (file=filename,exist=exst)
         if (exst) then
            open(nu_cycle,file=filename)
            read(nu_cycle,*) cycle_count
            close(nu_cycle)
            n=delfilesqq(filename) !delete file
         endif
         !if (cycle_count > 1) maxsi=300 !@@@@@@@@@@@@@@@@@@@@@@ needs to be fixed!!!!!!!!!!!!!!!!!!!!!
      else
         cycle_count=0
      endif

!----------------------------------------------------------------------
      IF (NPHAS .EQ. 1) THEN
         WRITE(6,*)' ***** SINGLE PHASE SYSTEM *****'
      ELSE
         WRITE(6,*)' ***** MULTIPLE PHASE SYSTEM *****'
      ENDIF
      WRITE(6,*)' '
!----------------------------------------------------------------------
      GRA=9.80621D+0*GRA
      DO I=1,21
        RFLC(I)=ONE-RFL(I)
        RFC(I)=ONE-RF(I)
      ENDDO
      !CL_G=CL_G/CP0 5/6/05 Lottes: obsolete, now use LG()%C
!----------------------------------------------------------------------
!     Melter geometry parameters
!       R0: reference length (m)
!     GRID3 defines a 3-D grid system
!----------------------------------------------------------------------


      CALL GRID3  !read in melt space grid
      call grid_c !read in combustion space grid if it exists


!----------------------------------------------------------------------
!     Inlet glass flow (0, at I=2)
!        YLG:  liquid glass concentration (mass fraction)
!        PG:  pressure (Pa)
!        TG:  glass temperature (K)
!        TP:  particle temperature (K)
!        UG:  glass axial velocity (m/s)
!        UP:  batch axial velocity (m/s)
!        WG:  tangential velocity (m/s) 
!        T0: inlet refernce temperature (K)
!              ** T0 should not be equal to TL0
!        RLM: volume mean batch particle radius (m)
!        DND: batch number density (particles/m**3)
!        DNST0: reference glass density (kg/m**3)
!        GMFR: glass mass flow rate reference (kg/s)
!        TA - Ambiant temperature for wall heat flux B.C.
!        DWALL - Wall thickness for wall heat flux B.C.
!----------------------------------------------------------------------
      TA=310D+0
      DWALL=0.3D+0
      !T0=one
      !cz      T0=TMLTR
      YLG0=1.0D+0
      !DNST0=2.9137D+3-0.16D+0*(TMLTR-1.0D+3)
	   !dnst0=one
	   !dnst0=2881.70d0
      !ds0=(2.9137D+3-0.16D+0*(TMLTR-1.0D+3))/dnst0 !used in buoyancy but may be wrong
      !ds0=2500
      ds0=zero
      !ds0=one !used in buoyancy but may be wrong
      !UG0=GMFR/DNST0/R0/R0
      !UG0=one
      WRITE(6,*)' '
      WRITE(6,*)' Glass bath cross-sectional area (m**2) = ',R0*height*yle
      WRITE(6,*)' '
      !WRITE(6,*)' Reference glass flow properties'
      WRITE(6,"(' Surface pressure =    ',F10.0,' Pa')") PG0
      !WRITE(6,"('    Temperature = ',F10.0,' K')") T0
      !WRITE(6,"('    Density =     ',F10.0,' kg/m**3')") DNST0
      !WRITE(6,"('    Velocity  =   ',F10.7,' m/s')") UG0
      !WRITE(6,"('    Flow rate =   ',F10.3,' kg/s'/)") GMFR
      IF (NPHAS.LE.1) GOTO 300

!----------------------------------------------------------------------
!     Properties of dispersed phases
!----------------------------------------------------------------------
      !TL00=5.33D+2
      !TL0=ONE
      !TL0=zero
      RD_P=36.0D-2
      b_p=1.0d+1
      coverf_norm=1-exp(-b_p)
!----------------------------------------------------------------------
!     Batch Particle Parameters
!----------------------------------------------------------------------
      IF (NPS_C.LE.0) GOTO 230
      !PCA%CL=CL_C  !replaced with user defined function
      PCA%H0=H0_C
      DO L=1,NPS_C
         RP2_C(L)=RP_C(L)**2
         RP3_C(L)=RP_C(L)**3
         !WPI_C(L)=c4d3pi*PCA%DS*(RP_C(L)*RLM0)**3
         WPI_C(L)=c4d3pi*PCA%DS*RP_C(L)**3
      END DO         
      IF (NPS_C.EQ.1) THEN
         DRP_C(1)=ONE
      ELSEIF (NPS_C.GT.1) THEN
         DO L=2,NPS_C
            DRP_C(L-1)=RP_C(L)-RP_C(L-1)
         END DO
         DRP_C(NPS_C)=DRP_C(NPS_C-1)
      ENDIF
230   IF (NPS_S.LE.0) GOTO 240
      !PSA%CL=CL_S  !replaced with user defined function
      !PSA%H0=H0_C
      PSA%H0=H0_s !should use sand heat of fusion here, Lottes 8/8/05
      DO L=1,NPS_S
         RP2_S(L)=RP_S(L)**2
         RP3_S(L)=RP_S(L)**3
         !WPI_S(L)=c4d3pi*PSA%DS*(RP_S(L)*RLM0)**3
         WPI_S(L)=c4d3pi*PSA%DS*RP_S(L)**3
      END DO         
      IF (NPS_S.EQ.1) THEN
         DRP_S(1)=ONE
      ELSEIF (NPS_S.GT.1) THEN
         DO L=2,NPS_S
            DRP_S(L-1)=RP_S(L)-RP_S(L-1)
         END DO
         DRP_S(NPS_S)=DRP_S(NPS_S-1)
      ENDIF
!----------------------------------------------------------------------
!     Bubble Parameters
!----------------------------------------------------------------------
240   GB%CP=1.0057D+3
      GB%H0=4.0D+5
      GB%WT=44.0D+0 
      !GB%DS=PG0*GB%WT/RU/T0 
      t0=one
      GB%DS=PG0*GB%WT/RU/T0 !check this - what Temperature should be used? @@@@! 
      GB%SG=1.0D-1
      GB%B2=1.0D-1
      GK0_GB=0.1D+0
      IF (NBS0.LE.0) GOTO 250
      DO L=1,NBS0
         RG2_B(L)=RG_B(L)**2
         RG3_B(L)=RG2_B(L)*RG_B(L)
         !WGI_B(L)=c4d3pi*GB%DS*(RG_B(L)*RLM0)**3
         WGI_B(L)=c4d3pi*GB%DS*RG_B(L)**3
      ENDDO
      IF (NBS0.EQ.1) THEN
         DRG_B(1)=ONE
      ELSEIF (NBS0.GT.1) THEN
         DO L=2,NBS0
            DRG_B(L-1)=RG_B(L)-RG_B(L-1)
         END DO
         DRG_B(NBS0)=DRG_B(NBS0-1)
      ENDIF
250   CONTINUE
!----------------------------------------------------------------------
!     ADLBL EQUATION SOLVER ITERATION NUMBERS (NEL NUMBERS) FOR DISPERSED PHASE
!     ALSO INDEX NUMBERS FOR SOURCE ROUTINE
!     INITIALIZE RESIDUAL ARRAY TO ZERO
!
!      1: PART. DN          C  8:                   15: DV (DROP V)
!      2: PART. DU          C  9:                   16: DW
!      3: PART. DV          C 10: NITROGEN          17: DT
!      4: PART. DW          C 11: STEAM             18: DC
!      5: PART. DT            12: COKE              19:
!      6: COKE  DC            13: DN (DROP # DNST)  20:
!      7: ENTHALPY            14: DU (DROP U VEL)   21:
!----------------------------------------------------------------------
      NTIMES=1
      NTIMES(1)=5
      RES=0
!----------------------------------------------------------------------
!     ADLBL EQUATION SOLVER ITERATION NUMBERS (NEL NUMBERS) FOR Glass 
!     ALSO INDEX NUMBERS FOR SOURCE ROUTINE
!     INITIALIZE RESIDUAL ARRAY TO ZERO
!
!     1: U MOMENTUM         
!     2: V MOMENTUM         
!     3: W MOMENTUM         
!     4: PRESSURE           
!     5: P CORRECTION       
!----------------------------------------------------------------------
300   continue
!      !NTIMESG=1       !ntimesg values have been moved to the relaxfactorm.txt file
!      NTIMESG=2
!      !ntimesg(5)=4
!      ntimesg(5)=4
!      !NTIMESG(6)=2
      RESG=0
!----------------------------------------------------------------------
!     CALCULATE MIXTURE PROPERTIES AND DIMENSIONLESS PARAMETERS
!----------------------------------------------------------------------
      !REYG=TWO*DNST0*UG0*R0/GMU0 Lottes 5/6/05
      !Re0=dnst0*ug0*r0/gmu0 !More convenient def when not in cyl coord.
      !PRN=GMU0*CL_G/GK0 Lottes 5/6/05 not used.
      !Pr0=gmu0*Cp0/gk0 !Lottes 5/9/05: for use in energy equation.
      !IF (NPHAS .EQ. 1) THEN
      !   DND0=ONE
      !   THET0C=ZERO
      !ELSE
         !G0=c4d3pi*RLM0**3
      !CSL
      !CSL         TH_C=PII/0.75D+0*RLM0**3*(PCA%DS+PSA%DS)/2.0D+0*UG0
      !CSL         DND0=GMFR/TH_C  
      !CZ
         !DND0=GMFR/(G0*DNST0*UG0)
	   !dnd0=gmfr/(dnst0*ug0)
	   !dnd0=one
         !THET0C=G0*DND0
	   !thet0c=gmfr/(dnst0*ug0)
	      !thet0c=one
      !ENDIF
      !DO L=1,NBS0
      !   WGI_B(L)=WGI_B(L)*DND0/DNST0 !what is this? @@@
      !ENDDO
      PCA%FR=PCA%DS
      PSA%FR=PSA%DS
      GB%FR=GB%DS
      !SCN=GMU0/GDF0/DNST0
      !REYD=TWO*DNST0*UG0*RLM0/GMU0
      !GVN_C=PI2*GK0_PC*RLM0*DND0*R0/(CP0*DNST0*UG0)
      !GVN_C=PI2*GK0_PC
      !GVN_S=PI2*GK0_PS*RLM0*DND0*R0/(CP0*DNST0*UG0)
      !GVN_S=PI2*GK0_PS
      !GVN_B=PI2*GK0_GB*RLM0*DND0*R0/(CP0*DNST0*UG0)
      !GVN_B=PI2*GK0_GB
      !GDN=6.0D+0*PI*GMU0*RLM0*DND0*R0/(DNST0*UG0)
      !GDN=6.0D+0*PI
      !H_0=CP0*(T0-TL0)
      !H_0=CP0*T0
      !H_0=one
      !EUN=PG0/(DNST0*UG0**2)
      !EUN=one
      !RFG_C=PCA%DS
      !RFG_S=PSA%DS
!----------------------------------------------------------------------
!     non-dimensional factor for the transport eqn.:
!       for energy it should be Prandtl # prn=cp0*gmu0/gk0
!       and for species it should be Schmidt # scn=gmu0/gdf0/dnst0
!
      SIGMA=ONE
      !SIGMA(IH)=UG0*R0
      if(nps_c>0)TM_C=TM_C !Lottes 4/11/04: need to check that array allocated
      if(nps_s>0)TM_S=TM_S !Lottes 4/11/04: need to check that array allocated
      PCA%H=PCA%H0
      PSA%H=PSA%H0
      GB%H=GB%H0
      !IF (STEADY) THEN
      !   TRN=ZERO
      !ELSE
      !   TRN=R0/UG0/TP0
      !ENDIF
!      IF (.NOT.REACT) GOTO 170
!----------------------------------------------------------------------
!     Bounds of computed flow properties
!----------------------------------------------------------------------
      !P_MX=1.0D+3
      !P_MN=-0.6D+0
      P_MX=1.0D+6
      P_MN=-0.6D+4
      U_MN=-U_MX
      IF (NPHAS.GT.1) THEN
         TH_MN=0.1D+0
         TH_PMX=0.6D+0
         !CZ         TH_PMX=0.3D+0
         DN_MX=1.0D+16
         DN_MN=1.0D-10
         SP0_C=PCA%DS
         SP0_S=PSA%DS
      ENDIF
      !H0_LG=CL_G*CP0*(TMLTR-TL0) Lottes 5/6/05
      !H0_LG=CL_G*CP0*TMLTR obsolete
      UDF_DSN=2
      ALLOCATE(UDF_DS(2))
      UDF => UDF_DS
      UDF(1)%T=1000
      UDF(1)%F=2913.7D+0
      UDF(2)%T=10000
      UDF(2)%F=1473.7D+0
      UDF_MUN=7
      ALLOCATE(UDF_MU(7))
      UDF => UDF_MU
      UDF(1)%T=1100
      UDF(1)%F=11350
      UDF(2)%T=1200
      UDF(2)%F=1742
      UDF(3)%T=1300
      UDF(3)%F=353
      UDF(4)%T=1400
      UDF(4)%F=89
      UDF(5)%T=1500
      UDF(5)%F=27
      UDF(6)%T=1600
      UDF(6)%F=9.4D+0
      UDF(7)%T=1700
      UDF(7)%F=3.7D+0
      UDF_KN=7
      ALLOCATE(UDF_K(7))
      UDF => UDF_K
      UDF(1)%T=1100
      UDF(1)%F=17.78D+0
      UDF(2)%T=1200
      UDF(2)%F=25.57D+0
      UDF(3)%T=1300
      UDF(3)%F=34.28D+0
      UDF(4)%T=1400
      UDF(4)%F=43.92D+0
      UDF(5)%T=1500
      UDF(5)%F=74.48D+0
      UDF(6)%T=1600
      UDF(6)%F=85.96D+0
      UDF(7)%T=1700
      UDF(7)%F=100.0d+0
      UDF_CLN=1
      ALLOCATE(UDF_CL(1))
      UDF => UDF_CL
      UDF(1)%T=1100
      UDF(1)%F=1.2D+3

      ALLOCATE(WL(1),AKL0(1))
      WL(1)=2.7
      AKL0(1)=0.312

      UDF_CLCN=2
      ALLOCATE(UDF_CLC(2))
      UDF => UDF_CLC
      UDF(1)%T=303.15d0
      UDF(1)%F=1.1d3  
      UDF(2)%T=1473.15d0
      UDF(2)%F=1.3d3
      UDF_CLSN=2
      ALLOCATE(UDF_CLS(2))
      UDF => UDF_CLS
      UDF(1)%T=303.15d0
      UDF(1)%F=1.1d3  
      UDF(2)%T=1473.15d0
      UDF(2)%F=1.3d3

      CALL ALCC !Allocate Continuous Phase Flow Variables



!======================================================================
!======================================================================
!     Read relaxation factors file
!======================================================================

!filename=casedir//'\relaxfactor'//runum//'c.dat'
filename='relaxfactorm.txt'
open (nu_rxfact,file=filename)

read (nu_rxfact,*) title !Relaxation factors for equation loops
read (nu_rxfact,*) general_rf !general relaxation value
rfl=general_rf

read (nu_rxfact,*) rfl(1) ! u momentum in x direction 
read (nu_rxfact,*) rfl(2) ! v momentum in y direction 
read (nu_rxfact,*) rfl(3) ! w momentum in z direction 
read (nu_rxfact,*) rfl(6) ! enthalpy
read (nu_rxfact,*) rfl(9) ! electric potential energy
read (nu_rxfact,*) rfl(19)! melt rate

read (nu_rxfact,*) title !Relaxation factors for certain variables
read (nu_rxfact,*) general_rf !general relaxation value
rf=general_rf

read (nu_rxfact,*) rf(1)  ! u momentum in x direction (pressure correction)
read (nu_rxfact,*) rf(2)  ! v momentum in y direction (pressure correction)
read (nu_rxfact,*) rf(3)  ! w momentum in z direction (pressure correction)
read (nu_rxfact,*) rf(4)  ! batch-glass heat transfer
read (nu_rxfact,*) rf(5)  ! bubble-glass heat transfer
read (nu_rxfact,*) rf(13) ! particle number density
!read (nu_rxfact,*) rf(14) ! particle momentum in x direction
!read (nu_rxfact,*) rf(15) ! particle momentum in y direction
!read (nu_rxfact,*) rf(16) ! particle momentum in z direction
read (nu_rxfact,*) rf(17) ! particle temperature
read (nu_rxfact,*) rf(18) ! density
read (nu_rxfact,*) rf(19) ! melt rate
!read (nu_rxfact,*) rf(20) ! simple pressure

read (nu_rxfact,*) rfqrs ! !relaxation factor for surface heat flux
read (nu_rxfact,*) scale_out ! down scaling factor for heat flux out of melt surf.
scale_out_r=scale_out ! used same value for regen furnace alternate firing mode
!read (nu_rxfact,*) general_rf 
!rfm=general_rf !relaxation factor for minor species - not used

rfc=one-rf  !set complementary relaxation factors
rflc=one-rfl
rfqrs_c=one-rfqrs
!rfmc=one-rfm

read (nu_rxfact,*) title !Number of sweeps in equation solver for glass
read (nu_rxfact,*) general_rf !general value
ntimesg=general_rf
read (nu_rxfact,*) ntimesg(1)  !  u momentum in x direction
read (nu_rxfact,*) ntimesg(2)  !  v momentum in y direction
read (nu_rxfact,*) ntimesg(3)  !  w momentum in z direction
read (nu_rxfact,*) ntimesg(4)  !  pressure
read (nu_rxfact,*) ntimesg(5)  !  pressure correction
read (nu_rxfact,*) ntimesg(6)  !  enthalpy
read (nu_rxfact,*) ntimesg(9)  !  electric potential energy

close (nu_rxfact)


!     Previous relaxation comments are below:      
!----------------------------------------------------------------------
!                   
!     1: U(P')              13: DN          18: DNST
!     2: V(P')              14: DU          19: PMR (MELT RATE)
!     3: W(P')              15: DV          20: P (SIMPLE ONLY) 
!     4: batch-glass HT     16: DW          21: 
!     5: bubble-glass HT    17: DT          
!----------------------------------------------------------------------
!      RF=0.7D+0
!      RF=0.3D+0
!      RF(4:5)=0
!      RFM=0.9D+0
!      RFMC=ONE-RFM
!----------------------------------------------------------------------
!     1: U,  2: V,  3: W,  4: P (not used),  5: P' (not used),  6: ENTHALPY
!     9: EBV,  19: PMR (MELT RATE)
!----------------------------------------------------------------------
      !RFL=0.7D+0
      !CSL   RFL(6)=0.5D+0
      !rfl=.3d0
!      rfl=.4d0
!      RFL(9)=1.0D+0
!      RFL(19)=0.5D+0
      !GMFR=ONE
      !rfqrs=0.5d+0
!      rfqrs=1.0d+0
!      rfqrs_c=one-rfqrs


!     Previous ntimesg comments are below:      
!----------------------------------------------------------------------
!     ADLBL EQUATION SOLVER ITERATION NUMBERS (NEL NUMBERS) FOR Glass 
!     ALSO INDEX NUMBERS FOR SOURCE ROUTINE
!
!     1: U MOMENTUM         6: glass energy = enthalpy
!     2: V MOMENTUM         7:
!     3: W MOMENTUM         8:
!     4: PRESSURE           9: electric potential energy
!     5: P CORRECTION       
!----------------------------------------------------------------------
!      !NTIMESG=1
!      NTIMESG=2
!      !ntimesg(5)=4
!      ntimesg(5)=4
!      !NTIMESG(6)=2

      RETURN
      END

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!     Allocate grid variables
!----------------------------------------------------------------------
      SUBROUTINE ALCG
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ALLOCATE (X(MP),DX(MP),y(NP),dy(NP),Z(LP),DZ(LP))
      allocate (xs_m(mp),ys_m(np))
      ALLOCATE (IBCELL(MP+1,NP+1,LP+1),IBCEB(MP+1,NP+1,LP+1))
      allocate (Lcharg(2*(mz+nz),4)) !Lottes 8/3/06
      Lcharg=0
      if (regen==1) then
         allocate (qrs_prev_r(mz,nz))
      endif
      ALLOCATE (QRS(MZ,NZ),QRSLG(MZ,NZ),qrs_prev(mz,nz),qc_surf0(mz,nz))
      ALLOCATE (QRS_m(MZ,NZ))
      ALLOCATE (QRSP_C(MZ,NZ,NPS_C),QRSP_S(MZ,NZ,NPS_S))
      ALLOCATE (QCD(MZ,NZ,0:NPS0),QLS(MZ,NZ,LZ))
      ALLOCATE (QCD_C(MZ,NZ,0:NPS_C),QCD_S(MZ,NZ,0:NPS_S))
      ALLOCATE (PC0MR_LG(MZ,NZ,LZ),PS0MR_LG(MZ,NZ,LZ))
      ALLOCATE (Cullet_T(MZ,NZ),Sand_T(MZ,NZ)) !Lottes 5/25/05

      ALLOCATE (area_s_spr(mz,nz))
      area_s_spr=zero
      ALLOCATE (area_c_spr(mz,nz))
      area_c_spr=zero
      ALLOCATE (coverf(mz,nz))
      coverf=zero
      allocate(dn_c(mz,nz))
      allocate(dn_s(mz,nz))
      ALLOCATE (Surf_T(MZ,NZ)) !Lottes 5/25/05
      QRS=0
      QRSLG=0
      IF (.NOT.ALLOCATED(QE)) ALLOCATE (QE(MZ,NZ,LZ),QA(MZ,NZ,LZ))
      QE=0
      QA=0
      RETURN
      END


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!     Allocate Continuous Phase Flow Variables
!----------------------------------------------------------------------
      SUBROUTINE ALCC
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ALLOCATE (LG(MP,NP,LP))
      ALLOCATE (FLX(MP,10),FLXH(MP,6))
      ALLOCATE (FLXHP_C(MP),FLXHP_S(MP))
      !ALLOCATE (fz2(mp,np,lp)) !$$$$
      ALLOCATE (FZ(MP,NP,LP),BS(MP,NP,LP),SFP(MP,NP,LP)) !$$$$
      ALLOCATE (AP(MP,NP,LP),AS(MP,NP,LP,3,2))
	   allocate (res_mass(mz,nz,lz))
      ALLOCATE (EBV(MP,NP,LP),EBE(MP,NP,LP))
      ALLOCATE (EBJ(MP,NP,LP),EBI(MP,NP,LP))
      ALLOCATE (EBQ(MP,NP,LP),EBSG(MP,NP,LP))
      ALLOCATE (DS_BUB(MP,NP,LP),THETA_BUB(MP,NP,LP))
      LG%U(1)=ZERO
      LG%U(2)=ZERO
      LG%U(3)=ZERO
      LG%P=ZERO
      LG%T=ONE
      LG%DS=ONE
      LG%TH=ONE
      LG%F(1)=ZERO
      LG%F(2)=ZERO
      LG%F(3)=ZERO
      LG%A=ONE
      LG%C=ONE
      LG%K=ONE
      LG%MU=ONE
      FLX=ZERO
      FLXH=ZERO
      IF (NPS0.GE.1) THEN
         ALLOCATE (SP(MP,NP,LP))
         SP=ZERO
      ENDIF 
      IF (NPS_C.GE.1) THEN
         ALLOCATE (PC(MZ,NZ,LZ,NPS_C))
         ALLOCATE (PC0(MZ,NZ,LZ))
         ALLOCATE (PMRX_C(MZ),PFRX_C(MP))
         PC%DN=ZERO
         PC%U(1)=ZERO
         PC%U(2)=ZERO
         PC%U(3)=ZERO
         PC%T=3.0D+2
         PC%MR=ZERO
         PC0%TH=ZERO
         PC0%MR=ZERO
         PMRX_C=ZERO
         PFRX_C=ZERO
      ENDIF 
      IF (NPS_S.GE.1) THEN
         ALLOCATE (PS(MZ,NZ,LZ,NPS_S))
         ALLOCATE (PS0(MZ,NZ,LZ))
         ALLOCATE (PMRX_S(MZ),PFRX_S(MP))
         PS%DN=ZERO
         PS%U(1)=ZERO
         PS%U(2)=ZERO
         PS%U(3)=ZERO
         PS%T=3.0D+2
         PS%MR=ZERO
         PS0%TH=ZERO
         PS0%MR=ZERO
         PMRX_S=ZERO
         PFRX_S=ZERO
      ENDIF 
      IF (NBS0.GE.1) THEN
         ALLOCATE (GB4(MZ,NZ,LZ,NBS0),GB3(MZ,NZ,LZ),GBX(MZ))
         ALLOCATE (CON(MZ,NZ,LZ),FRX_GB(MZ),FLX_GB(MZ))
         GB4%DN=ZERO
         GB4%U(1)=ZERO
         GB4%U(2)=ZERO
         GB4%U(3)=ZERO
         GB4%T=ONE
         GB4%P=ZERO
         GB4%DS=GB%DS
         DO L=1,NBS0
            GB4(1:MZ,1:NZ,1:LZ,L)%R=RG_B(L)
         ENDDO
         GB4%Y(1)=ONE
         GB4%Y(2)=ZERO
         GB4%WT=44
         GB4%GR=0
         GB3%TH=0
         CON=ZERO
      ENDIF 
      IF (MS.EQ.1) THEN
         ALLOCATE (GFM(MP,NP,LP,NMSP),FLXM(MP,NMSP,2)) 
         ALLOCATE (RRMS(MZ,NZ,LZ,NRM0),FLYM(NP,NMSP,2)) 
         ALLOCATE (RFM(NMSP),RFMC(NMSP),RESM(NMSP,2))
         ALLOCATE (XKC(NMSP,NMSP),XKT(NMSP,NMSP),ERC(NMSP))
         ALLOCATE (ERT(NMSP),ODR(NMSP,2),DPADJ(NMSP))
         ALLOCATE (CK0_M(NMSP),BETA(NMSP),DFACTM(NMSP))
         ALLOCATE (WMS(NMSP))
         ALLOCATE (NTIMESM(NMSP))
         NTIMESM=1
      ENDIF
      IF (STEADY) RETURN
      ALLOCATE (LG0(MP,NP,LP),APO(MP,NP,LP))
      IF (NPS_C.GE.1) THEN
         ALLOCATE (PCT(MZ,NZ,LZ,NPS_C))
      ENDIF 
      IF (NPS_S.GE.1) THEN
         ALLOCATE (PST(MZ,NZ,LZ,NPS_S))
      ENDIF 
      IF (NBS0.GE.1) THEN
         ALLOCATE (GBT(MZ,NZ,LZ,NBS0))
      ENDIF 
      RETURN
      END


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!     Deallocate Continuous Phase Flow Variables
!----------------------------------------------------------------------
      SUBROUTINE DALCC
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DEALLOCATE (X,DX,y,dy,Z,DZ)
      DEALLOCATE (IBCELL,IBCEB)
      DEALLOCATE (Cullet_T,Sand_T,surf_T) !Lottes 5/25/05
      deallocate (area_s_spr,area_c_spr,coverf) !Lottes 6/5/05
      deallocate (dn_c,dn_s) !Lottes 6/6/05
      DEALLOCATE (QRS,QRSLG,qrs_prev,qc_surf0,QRSP_C,QRSP_S)
      DEALLOCATE (QCD,QCD_C,QCD_S,QLS)
      DEALLOCATE (PC0MR_LG,PS0MR_LG)
      DEALLOCATE (LG)
      DEALLOCATE (FZ,BS,SFP,AP,AS,FLX,FLXH,FLXHP_C,FLXHP_S)
      DEALLOCATE (PULL)
      deallocate (area_exit) 
	   deallocate (res_mass)
      DEALLOCATE (EBV,EBE,EBJ,EBI,EBQ,EBSG)
      DEALLOCATE (DS_BUB,THETA_BUB)
      IF (.NOT.STEADY) THEN
         DEALLOCATE (LG0,APO)
      ENDIF
      IF (NPS0.GE.1) DEALLOCATE (SP)
      IF (NPS_C.GE.1) THEN
         DEALLOCATE (RP_C,RP2_C,RP3_C,DRP_C)
         DEALLOCATE (WPI_C,TM_C)
         DEALLOCATE (PC,PC0)
         DEALLOCATE (PMRX_C,PFRX_C)
         IF (.NOT.STEADY) DEALLOCATE (PCT)
      ENDIF 
      IF (NPS_S.GE.1) THEN
         DEALLOCATE (RP_S,RP2_S,RP3_S,DRP_S)
         DEALLOCATE (WPI_S,TM_S)
         DEALLOCATE (PS,PS0)
         DEALLOCATE (PMRX_S,PFRX_S)
         IF (.NOT.STEADY) DEALLOCATE (PST)
      ENDIF 
      IF (NBS0.GE.1) THEN
         DEALLOCATE (RG_B,RG2_B,RG3_B,DRG_B)
         DEALLOCATE (WGI_B)
         DEALLOCATE (GB4,CON,FRX_GB)
         IF (.NOT.STEADY) DEALLOCATE (GBT)
      ENDIF 
      IF (MS.EQ.1) THEN
         DEALLOCATE (FLXM,RRMS)
         DEALLOCATE (FLYM,RFM,RFMC,RESM)
         DEALLOCATE (XKC,XKT,ERC,ERT,ODR)
         DEALLOCATE (DPADJ,CK0_M,BETA,DFACTM)
         DEALLOCATE (NTIMESM)
      ENDIF 
      RETURN
      END


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!     Allocate Flow Variables for Dispersed Phases (Multiphase)
!----------------------------------------------------------------------
      SUBROUTINE ALCD1
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (NPS_C.GE.1) THEN
         ALLOCATE (RP_C(NPS_C),RP2_C(NPS_C),RP3_C(NPS_C))
         ALLOCATE (DRP_C(NPS_C),WPI_C(NPS_C),TM_C(NPS_C))
      ENDIF 
      IF (NPS_S.GE.1) THEN
         ALLOCATE (RP_S(NPS_S),RP2_S(NPS_S),RP3_S(NPS_S))
         ALLOCATE (DRP_S(NPS_S),WPI_S(NPS_S),TM_S(NPS_S))
      ENDIF 
      IF (NBS0.GE.1) THEN
         ALLOCATE (RG_B(NBS0),RG2_B(NBS0),RG3_B(NBS0),DRG_B(NBS0))
         ALLOCATE (WGI_B(NBS0))
      ENDIF 
      RETURN
      END

