MODULE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!-------------------------------------------------------------------
LOGICAL SMPLER,STEADY,TPRINT,SYM,REACT,EB_C
LOGICAL EXST
LOGICAL IRST,ISAV,IPRNTALL
logical comb_grid_exist !true if a combustion grid exists
logical runstop_exist !true if a runstop file exists
!-------------------------------------------------------------------
CHARACTER*40 FILENAME,TITLE,filename2
CHARACTER*32 FMX,FMR,FMZ
CHARACTER*4 RUNUM,GDNUM,RUNUM_R,GDNUM_R
character*4 crun !Lottes 4/15/05: added for convergence file monitoring
character*8 casedir !name of case folder
character id0 !Lottes 4/15/05: added for convergence file monitoring
!-------------------------------------------------------------------
INTEGER MP !max x index = 2(# x cells)+4
INTEGER MZ != mp/2 = (# x cells)+2, (2 domain boundaries)
integer i_exit ! index of start of exit extending tunnel
INTEGER MPM1,MPM2 ! mp-1, mp-2
INTEGER NP !max y index = 2(# y cells)+4
INTEGER NZ != np/2 = (# y cells)+2, (2 domain boundaries)
INTEGER NPM1,NPM2 ! np-1, np-2
INTEGER LP !max z index = 2(# z cells)+4
INTEGER LZ != lp/2 = (# z cells)+2, (2 domain boundaries)
INTEGER LPM1,LPM2 ! lp-1, lp-2
!integer MP_C !max x index = 2(# x cells)+4 combustion space grid
!integer NP_C !max y index = 2(# y cells)+4 combustion space grid
!integer MP_C_R !max x index = combustion space grid (2nd grid regenerative furnace)
!integer NP_C_R !max y index = combustion space grid (2nd grid regenerative furnace)
integer mp3(3)
INTEGER KEY
integer MAXGI ! maximum # of glass flow iterations per global iteration
integer MAXPI ! maximum # of particle flow iterations per global iteration
integer MAXBI ! maximum # of bubble flow interations per global iteration
integer MAXSI ! maximum # of global iterations
integer MAXTI ! maximum # of global iterations per transient iteration (not used)
integer IH    ! obsolete - index for enthalpy in flow variable array
INTEGER LLK,NEL,KL,NU,NELG,KD,MP_B,MP_E,MZ_B,MZ_E
INTEGER MP_E1,MZ_E1,N_PI
INTEGER NEX0 ! Number of exits
INTEGER IYLG,IMX,JMX,KMX
INTEGER I_REF,J_REF,K_REF !Index set for pressure reference point
INTEGER NBS0  ! Number of bubble size groups
INTEGER NPS_S ! Number of sand size groups
INTEGER NPS_C ! Number of cullet size groups
INTEGER NPS_1 ! Number of sand or cullet size groups, depends on context
INTEGER NPS0  ! Total number of particle size groups = nps_c+nps_s
INTEGER IRSTYP,IBAKUP,IDEBUG,IRSTYPM,IDCAC,N3VBC
!INTEGER ICHARGE
INTEGER ITEND,KMEND
integer I_RB !refiner beginning x index
integer I_CH
INTEGER NJY1,NJY2,LKZ1,LKZ2,NCELLS
INTEGER NJY1V(3),NJY2V(3),LKZ1V(3),LKZ2V(3)
!cz      INTEGER MS,LSTARM,LENDM,NELM,NMSP
INTEGER NPHAS,LSTAR,LEND,NCONV,NLG,NLG0,LRX,LRI,ITN_R
integer itn ! 0=> There are no surface heat flux array values in the it....m file
            ! and the combustion space grid will not be read, so the it....t file
            ! cannot be created. 
            ! 1=> Surface heat flux array values are in the it....m file and the
            ! combustion space grid will be read, so the it....t file
            ! can be created.
INTEGER IRC,MAXTS,ISTEP,IFP,NEB0
INTEGER NTIMESG(20),NTIMES(20)
INTEGER MS,NMSP,NMS,MAXMS,LSTARM,LENDM,NELM,NRM0
INTEGER UDF_DSN,UDF_MUN,UDF_KN,UDF_CLN
integer udf_clcn !number of entries in the cullet specific heat user defined function
integer udf_clsn !number of entries in the sand   specific heat user defined function
INTEGER ID_RAD,NWL,NRADWALL,REGEN,ID_EB,ID_EB0,ID_EB1
integer i_out_bc_pull ! =1 enforce outflow at exits
integer iglass_qual ! set to 1 if glass quality indexes are to be computed at end
integer iglass_qual_only ! if 1, compute quality indexes from restart data and stop
integer ivap,icond,icoke,ndg,npg ! used in ported over convergence & array compare routines
integer iheat_flux_type ! type of heat flux input to melt surface
        ! 1 => uniform heat flux over surface, subject to scaling to meet glass exit temperature
        ! 2 => calculated from combustion domain, subject to scaling to meet glass exit temperature
        ! 3 => user specified heat flux over surface, not subject to adjustment
        ! 4 => calculated from combustion domain, not subject to adjustment
integer cycling ! if 1 then doing automatic cycling, if 0 then not cycling
integer cycle_count !current cycling number

!----------------------------------------------------------------------
!     Developer flags and file unit identifiers 
!----------------------------------------------------------------------
integer idrag_on ! if 1, then drag between surface sand or cullet is turned on
integer iwall_loss_est !if 1, qloss routine estimates wall loss from T_exit at wall
integer :: ialtbuoy =0 ! => use alternate buoyance forumlation

!set file unit values
integer :: ncon      =   6 ! unit for standard command window
                           !  nu '\gql'//runum//'n.dat' (nu init to 6)
integer :: nu_restg  =   7 ! unit for file 'rg'//runum//'m.d'  gas phase restart 
integer :: nu_rests  =   8 ! unit for file 'rs'//runum//'m.d'  minor species restart
integer :: nu_restr  =   9 ! unit for file 'rr'//runum//'m.d' radiation restart
integer :: nu_run    =  10 ! unit for file 'runs.dat'  inputs case number, only file in current directory
integer :: nu_sbc    =  11 ! unit for file 'sbc'//runum//'m.txt'  boundary conditions
                     !  12 ! y//runum//.out
integer :: nu_gfm    =  15 ! unit for file 'gfm.dat'  status to gui
integer :: nu_cycle  =  16 ! unit for file 'cycleInfo.txt'  cycle info from gui
integer :: nu_itm    =  18 ! unit for file 'it'//runum//'m.dat'
integer :: nu_guiup  =  19 ! unit for file 'gui_update.txt'
                     !  20 ! debug.out 
integer :: nu_adjf   =  21 ! unit for file it....m_adjflux.dat
integer :: nu_adjr   =  22 ! unit for file 'it'//runum//'m_relax.dat'
integer :: nu_savq   =  23 ! unit for file qrtmp//fnext    not used
integer :: nu_prtfv1 =  24 ! unit for file 'FV_vnodes'//runum//'m.dat   Fieldview grid nodes file
integer :: nu_prtfv2 =  25 ! unit for file 'FV_variables'//runum//'m.dat Fieldview function file for variable data
integer :: nu_prtfv3 =  26 ! unit for file 'FV_names'//runum//'m.nam Fieldview variable name file 
integer :: nu_grid   =  32 ! unit for file 'gd'//runum//'c.dat'
integer :: nu_rt     =  34 ! unit for file 'rt'//runum//'c.out'  main output of variables
integer :: nu_sum    =  35 ! unit for file 'summary'//runum//'m.txt'
integer :: nug       =  47 ! unit for melt convergence file
                     !  58 ! rs//crun//.d   used for file compare debugging
                     !  59 ! rs//crun//2.d  used for file compare debugging
                     !  60 ! rscomp.txt     used for file compare debugging
integer :: nu_Tave   =  95 ! unit for file 'Tave'//runum//'m.plt'
integer :: nu_flox   =  97 ! flox//runum//m.out   glass mass rates at yz-plane cross sections
integer :: nu_info   =  98 ! unit for file 'Info'//runum//'m.plt'
integer :: nu_gres   =  99 ! unit for file 'gresid'//runum//'c.plt'   glass PDE equation residual file
integer :: nu_gcor   = 100 ! unit for file 'gcor'//runum//'c.plt'   glass PDE correction equation residual file
integer :: nu_gc     = 101 ! unit for file 'gd'//gdnum//'c.dat'
integer :: nu_c      = 103 ! unit for file 'surf_c'//runum//'m.out'
                                      !interface condition combustion grid format file,  sprint_c not being called
integer :: nu_m      = 104 ! unit for file 'Surf_m'//runum//'m.out'
                                      !interface condition melt grid format file,  sprint_m not being called
integer :: nu_surfT  = 105 ! unit for file 'it'//runum//'t.dat'
integer :: nu_surfTr = 106 ! unit for file 'it'//runum_r//'t.dat'
integer :: nu_itmr   = 107 ! unit for file 'it'//runum_r//'m.dat'
integer :: nu_gc_r   = 108 ! unit for file 'gd'//gdnum_r//'c.dat'
integer :: nu_bt     = 109 ! unit for file 'bt'//runum//'.dat'    @@@@@@ not used
integer :: nu_stop   = 110 ! unit for file runend.txt
integer :: nu_Tchg   = 111 ! unit for file 'Tchg'//runum//'m.plt'
integer :: nu_Tchgr  = 112 ! unit for file 'Tchg'//runum_r//'m.plt'
integer :: nu_gresp  = 113 ! unit for file 'gresidp'//runum//'m.plt'   glass pre-solve PDE equation residual file
integer :: nu_gcorp  = 114 ! unit for file 'gcorp'//runum//'m.plt'   glass pre-solve PDE correction equation residual file

integer :: nu_rxfact = 115 ! unit for file 'relaxfactorm.txt'


integer :: nutmp     = 120  ! temp unit # for local use (file will not be open)
integer :: nud,nup     ! unit number for droplet and particle convergence 

!File control flags set =1 for printing
!Comments give file unit and name being controlled, {sbc} indicates specified by sbc namelist 

integer :: gui_update = 1 ! 15  {sbc} 1=> CFD will provide status updates to the gui via a gfm.dat file 
integer :: iadjf      = 1 ! 21  {sbc}  it....m_adjflux.dat
integer :: iadjr      = 1 ! 22  {sbc} 'it'//runum//'m_relax.dat'
integer :: ifieldview = 0 ! 24  {sbc} 'FV_nodes'//runum//'m.dat'   output Fieldview post processor grid nodes file
                          ! 25  {sbc} 'FV_variables'//runum//'m.dat'   output Fieldview post processor variable values file
                          ! 26  {sbc} 'FV_names'//runum//'m.nam'   output Fieldview post processor variable names file
integer :: isum       = 1 ! 35  {sbc} 'summary'//runum//'m.txt'
integer :: iconv      = 1 ! 47  {sbc} melt convergence file
integer :: iTave      = 1 ! 95  {sbc} 'Tave'//runum//'m.plt'
integer :: iflox      = 0 ! 97  flox//runum//m.out
integer :: iinfo      = 1 ! 98  {sbc} 'Info'//runum//'m.plt'
integer :: igresid    = 1 ! 99  {sbc} 'gresid'//runum//'m.plt'  glass PDE equation residual file
integer :: igcor      = 0 ! 100 'gcor'//runum//'m.plt'    glass PDE correction equation residual file
integer :: iTchg      = 1 ! 111 {sbc} 'Tchg'//runum//'m.plt'
                          ! 112 'Tchg'//runum_r//'m.plt'
integer :: igresidp   = 0 ! 113 {sbc} 'gresidp'//runum//'c.plt' glass pre-solve PDE equation residual file
integer :: igcorp     = 0 ! 114 'gcorp'//runum//'c.plt'   glass pre-solve PDE correction equation residual file
!-------------------------------------------------------------------

real(8) :: big    = 1.0d+20
real(8) :: small  = 1.0d-20
real(8) :: zero   = 0.0d+0
real(8) :: one    = 1.0d+0
real(8) :: two    = 2.0d+0
real(8) :: third  = 1.0d+0/3
real(8) :: half   = 0.5d+0
real(8) :: pi     =  3.141592653589793d+0
real(8) :: pi2    =2*3.141592653589793d+0
real(8) :: pi4    =4*3.141592653589793d+0
real(8) :: pi6    =6*3.141592653589793d+0
real(8) :: c4d3pi =4*3.141592653589793d+0/3.0d+0
real(8) :: ru     =8.3143d+3 !universal gas constant
REAL(8) GRA(3) !vector components of acceleration due to gravity (m/s**2)
!REAL(8) CP0 !Reference specific heat
!real(8) GDF0,GK0,GMU0 !reference values; no longer used
!REAL(8) Re0 !Reynolds number defined from reference values, Lottes 5/10/05
!REAL(8) Pr0 !Prandtl number defined from reference values, Lottes 5/10/05
!REAL(8) REYG old ref Reynolds number obsolete, Lottes 5/10/05
!REAL(8) GDN
!REAL(8) GVN_C,GVN_S
!REAL(8) EUN
!REAL(8) RFG_C,RFG_S
!REAL(8) REYD,SCN,TRN,PRN
REAL(8) yle !Y length of grid (y(np)-y(3)) (m)
REAL(8) XLE !X length of grid (x(mp)-x(3)) (m)
REAL(8) X0 !Appears to be used as a local variable
real(8) r0    !reference length = 1 (m)
!real(8) area0 !reference area = 1 (m**2)
REAL(8) height !z length of grid (z(lp)-z(3)) (m)
real(8) vol_tot !volume of grid (m**3)
REAL(8) GFIN0 !Liquid glass flow coming in inlets, currently not allowed (kg/s)
REAL(8) GFIN  !Liquid + solid coming in inlets (kg/s)
REAL(8) FACTOR !Function replaced by Pull array, used to scale outlet velocity profile
REAL(8) FADJ !Not used, was used to set small slope at outlet in variable value
REAL(8) GFEX !Total liquid flow crossing into cells that have an outlet face (kg/s)
REAL(8) GFEX2 !Total liquid flow crossing outlet cell faces (kg/s)
!REAL(8) GMFR !Glass Mass Flow Reference = dnst0*ug0*area0 = 1 (kg/s)
REAL(8) DN_LG0,F_BUOY
REAL(8) FR2_LG,EX_P
!REAL(8) DNST0 !Reference density, set to density at Tmeltr (kg/m**3)
!REAL(8) UG0 !Reference velocity = gmfr/(dnst0*area0) = 1/dnst0 (m/s)
REAL(8) YLG0
!REAL(8) T0 !obsolete reference temperature
REAL(8) PG0 !Absolute pressure at melt surface (Pa)
!REAL(8) H_0 !obsolete reference enthalpy
!REAL(8) TP0 !obsolete reference particle temperature
REAL(8) P_REF
REAL(8) CPMIX,H_TOT,H_EX
!REAL(8) CL_G glass specific heat - obsolete replaced by Lg()%c, Lottes 5/11/05
!REAL(8) H0_LG !No longer global, Lottes 5/11/05
REAL(8) H_NEEDED,H0_C,H0_S,EB_HEAT,Q_COMB
REAL(8) CD_D_C ! Conduction thickness (thermal bndry layer) between glass and cullet
REAL(8) CD_D_S ! Conduction thickness (thermal bndry layer) between glass and sand
REAL(8) SIGMA(12),SGM
REAL(8) SP0_1
REAL(8) GK0_1 !thermal conductivity of sand or cullet particles (W/m K)
!REAL(8) GVN_1
!REAL(8) RFG_1
REAL(8) SP0_C,GK0_PC
REAL(8) SP0_S,GK0_PS
!REAL(8) TL0 !Lottes (old droplet reference T, obsolete
!REAL(8) DND0 !obsolete reference # density
!REAL(8) RLM0,THET0C
REAL(8) T0_GB,DS0,TM_0
REAL(8) SMX,AVEB,BGCON,BBCON,BPCON,BTCON,SMXH,AVET,AVEBG
REAL(8) P_MX,P_MN,U_MX,U_MN,T_MX,T_MN,DN_MX,DN_MN,TH_MN,TH_PMX
real(8) h_Tmax !maximum liquid glass enthalpy associated with the maximum temperature 
real(8) h_Tmltr !enthalpy at glass melting point 
REAL(8) BMCON,TM,DTM
!REAL(8) DT0 !not used, time step as fraction of tp0
REAL(8) GDMLT,GDCON
REAL(8) RF(21),RFC(21),RFL(21),RFLC(21) ! Relaxation factors
real(8) general_rf !general relaxation value
REAL(8) RESG(20,2),RES(20,2)
real(8) qc_tot !net surface heat in from combustion space read from itxxxxm.dat file
real(8) qc_tot_r !net surface heat in for alternate regenerative firing mode
REAL(8) TA,DWALL,TMLTR
REAL(8) EBSG0,EBSG1,EBVP0,EBV0,QEB0
REAL(8) W_K,W_HA,H_G,W_TA,W_D,W_E
REAL(8) RI,SIG,SIGN
REAL(8) QESG,QES0,QLS0,QAS0,ECONV,TG_MN
REAL(8) ANG0(3),VOL
real(8) T_init ! Mean glass melt initial temperature used as a B.C.
real(8) T_exit ! Mean glass melt exit temperature used as a B.C.
real(8) Tmean_ex ! Calculated mean glass melt exit temperature.
real(8) q_wall_loss_tot !Total conduction heat loss through walls (inc exits, entries) Lottes 5/5/05
real(8) q_sand ! Total radiation and conduction to surface sand
real(8) q_cullet ! Total radiation and conduction to surface cullet
real(8) qs_in ! Sum positive radiation & convection in to surface [W] !Lottes 11/23/07
real(8) qs_in_r ! Sum positive radiation & convection in to surface regen alt configuration [W] !Lottes 11/23/07
real(8) qs_in_base ! Sum positive radiation & convection in to surface [W] at run start !Lottes 11/23/07
real(8) qs_in_base_1 ! Sum positive heat in to surface [W] at run start for 1st comb space in regen !Lottes 11/23/07
real(8) qs_out ! Sum negative radiation & convection out from surface [W] !Lottes 11/23/07
real(8) qs_out_1 ! Sum negative radiation & convection out from surface of 1st comb space in regen [W] !Lottes 11/23/07
real(8) qs_out_r ! Sum negative radiation & convection out from surface [W] !Lottes 11/23/07
real(8) qrs_tot_base ! Total radiation & convection to surface [W] after possible scaling & relaxation at run start !Lottes 2/15/06
real(8) qrs_tot_base_comb1 ! Total radiation & convection to surface [W] from combustion space 1 at run start
real(8) qrs_tot ! Total radiation and convection to surface [W] !Lottes 5/5/05
real(8) qrs_tot_r ! Total regen radiation and convection to surface at run start [W] !Lottes 5/5/05 
real(8) qrslg_tot ! Total radiation to surface liquid glass [W] !Lottes 5/5/05
real(8) qrsp_c_tot ! Total radiation to surface cullet [W] Lottes 5/5/05
real(8) qrsp_s_tot ! Total radiation to surface sand [W] Lottes 5/5/05
real(8) area_surf_tot !Total melt surface area [m**2]
real(8) area_exit_tot !Total exit area (m**2)
real(8) qcond_s_tot ! Total energy conducted from glass to sand [W] Lottes 5/19/05
real(8) qcond_c_tot ! Total energy conducted from glass to cullet [W] Lottes 5/19/05
real(8) qheat_c_need ! Total energy needed to heat cullet [W] Lottes 5/19/05
real(8) qheat_s_need ! Total energy needed to heat sand   [W] Lottes 5/19/05
real(8) qmelt_c_need ! Total energy needed to melt cullet [W] Lottes 5/19/05
real(8) qmelt_s_need ! Total energy needed to melt sand   [W] Lottes 5/19/05
real(8) qglass_need ! Total energy needed to raise glass from Tmelt to T_exit Lottes 5/5/05
real(8) qglass_net  ! Net energy transfered to liquid glass [W] Lottes 5/5/05
real(8) q_global_bal !(abs((q_in_tot-q_out_tot)/q_in_tot))
real(8) b_p !exponentional parameter in solids fraction coverage function
real(8) coverf_norm ! 1-exp(-b_p) used to normalize solids fraction coverage function
real(8) scale_in !scaling factor for interpolated surface heat flux, controls total heat in at surface
real(8) scale_out !scaling factor for interpolated surface heat flux, controls total heat out at surface should be < 1
real(8) scale_in_r !scaling factor for interpolated surface heat flux, controls total heat in at surface for regen
real(8) scale_out_r !scaling factor for interpolated surface heat flux, for regen, should be < 1
real(8) facq !renormalization factor for interpolated surface heat flux, controls total heat in at surface
real(8) facq_r !renormalization factor for interpolated surface heat flux,
              !in alternate configuration of regenerative furnace
real(8) facq_chg !relative change of combustion heat renormalization factor
real(8) rfqrs,rfqrs_c !relaxation factor & complement for surface heat flux
real(8) avg_wall_T !average combustion space wall temperature
real(8) avg_wall_T_r !average 2nd combustion space wall temperature
real(8) Tmax_c  !maximum T in combustion space (limit T bound in melt space)
real(8) Tmax_c_r  !maximum T in 2nd combustion space (limit T bound in melt space)
real(8) solids_in !mass flow of solids coming in sand + cullet (kg/s)
real(8) h_resid  !mean enthalpy equation residual normalized
real(8) h_resid_pre  !mean enthalpy equation residual normalized, before enthalpy solve
real(8) p_resid  !mean pressure equation residual normalized
real(8) p_resid_pre  !mean pressure equation residual normalized, before solve
real(8) pcorr_resid  !mean pressure correction equation residual normalized
real(8) pcorr_resid_pre  !mean pressure correction equation residual normalized, before solve
real(8) u_resid(3)  !mean momentum equation residuals normalized
real(8) u_resid_pre(3)  !mean momentum equation residuals normalized, before solve
real(8) uc_mean(3)  !mean velocity corrections normalized

!-------------------------------------------------------------------
LOGICAL,ALLOCATABLE :: IQT1(:,:),IQTM(:,:),IQT2(:,:)
INTEGER,ALLOCATABLE :: IBCEB(:,:,:)
!-------------------------------------------------------------------
!Melt space grid variables
!
!Lottes 5/5/05:
!Cell type map, IBCELL (old blocked cell indicator array)
!
!      (-1,9 on input)-half blocked (used in 2D, meaningless in 3D)
!      0 - open to flow
!      1 - wall, 2 - inlet,   3-exit
!      4-interface,  5-ebooster,   8-bubbler
!
!      Multiple exits, electric boosters, and bubblers are assigned numbers
!               based on the order they appear in the SBC file, and the object number
!               is saved in the tens digit of IBCELL.
integer,allocatable :: ibcell(:,:,:)

!Lcharg = list of charger inlets with orientation
!
!         the first index is the charger inlet face #
!             since chargers occupy the first layer of cells
!             a dimension of 2*(mz+nz) should be sufficient
!         the second index = 4
!
!             where Lcharg(:,1) = x index of charger face
!                   Lcharg(:,2) = y index of charger face
!                   Lcharg(:,3) = face orientation (1=positive x, 2=negative x,
!                                                   3=positive y, 4=negative y)

integer,allocatable :: Lcharg(:,:)
integer :: ncf=0 !number of charger inlet cell faces
real(8),allocatable :: x(:),dx(:),y(:),dy(:),z(:),dz(:)
integer i_mb !melt surf begin x index in melt grid
integer i_me !melt surf end   x index in melt grid
integer j_mb !melt surf begin y index in melt grid
integer j_me !melt surf end   y index in melt grid
real(8) x_cent_m !x-coordinate of center of melt interface in melt grid
real(8) y_cent_m !y-coordinate of center of melt interface in melt grid
integer chamber_type ! 2=> melter with refiner
real(8) melter_length ! length of melter (not including throat nor refiner)
real(8) surf_length_m !length of melt surface in melt grid, excluding throat and refiner
real(8) surf_width_m  !width  of melt surface in melt grid, excluding throat and refiner
real(8) :: x_mb=0.0d0 !melt surf begin x location in melt grid
real(8) :: y_mb=0.0d0 !melt surf begin y location in melt grid

real(8),allocatable :: xs_m(:)
real(8),allocatable :: ys_m(:)

!-------------------------------------------------------------------
! Combustion space grid variables
! Used in interpolation of boundary conditions
! between melt and combustion space grids
integer idebug_c
real(8) r0_c !Reference length for combustion grid
integer mp_c !max x index = 2(# x cells)+4 combustion space grid
integer np_c !max y index = 2(# y cells)+4 combustion space grid
integer lp_c !max z index = 2(# z cells)+4 combustion space grid
integer i_mb_c !melt surf begin x index in combustion grid
integer i_me_c !melt surf end   x index in combustion grid
integer j_mb_c !melt surf begin y index in combustion grid
integer j_me_c !melt surf end   y index in combustion grid
integer interior_surf_wall !0 - no walls within above bounds, 1 otherwise
real(8) :: x_mb_c=0.0d0 !melt surf begin x location in combustion grid
real(8) :: y_mb_c=0.0d0 !melt surf begin y location in combustion grid
real(8) surf_length_c !length of melt surface in combustion grid, excluding dog houses, etc.
real(8) surf_width_c  !width  of melt surface in combustion grid, excluding dog houses, etc.
real(8) x_cent_c !x-coordinate of center of melt interface in combustion grid
real(8) y_cent_c !y-coordinate of center of melt interface in combustion grid
real(8),allocatable :: x_c(:) 
real(8),allocatable :: xs_c(:) !coordinate shifted to same center as melt grid 
real(8),allocatable :: y_c(:) 
real(8),allocatable :: ys_c(:) !coordinate shifted to same center as melt grid 
real(8),allocatable :: z_c(:)
real(8),allocatable :: dx_c(:)
real(8),allocatable :: dy_c(:)
real(8),allocatable :: dz_c(:)
integer,allocatable :: ibcell_c(:,:,:)
real(8),allocatable :: bs_c(:,:,:)
real(8),allocatable :: qrs_m(:,:) !heat flux interpolated to melt grid
!-------------------------------------------------------------------
! Combustion space grid variables - for regenerative grid
! Used in interpolation of boundary conditions
! between melt and combustion space grids
integer idebug_c_r
real(8) r0_c_r !Reference length for combustion grid
integer mp_c_r !max x index = 2(# x cells)+4 combustion space grid
integer np_c_r !max y index = 2(# y cells)+4 combustion space grid
integer lp_c_r !max z index = 2(# z cells)+4 combustion space grid
!integer mz_c_r @remove on cleanup
!integer nz_c_r @remove on cleanup
integer i_mb_c_r !melt surf begin x index in combustion grid
integer i_me_c_r !melt surf end   x index in combustion grid
integer j_mb_c_r !melt surf begin y index in combustion grid
integer j_me_c_r !melt surf end   y index in combustion grid
integer interior_surf_wall_r !0 - no walls within above bounds, 1 otherwise
real(8) surf_length_c_r !length of melt surface in combustion grid, excluding dog houses, etc.
real(8) surf_width_c_r  !width  of melt surface in combustion grid, excluding dog houses, etc.
real(8) x_cent_c_r !x-coordinate of center of melt interface in combustion grid
real(8) y_cent_c_r !y-coordinate of center of melt interface in combustion grid
real(8),allocatable :: x_c_r(:) 
real(8),allocatable :: xs_c_r(:)
real(8),allocatable :: r_c_r(:) !Obsolete y or r coordinate
real(8),allocatable :: y_c_r(:) 
real(8),allocatable :: ys_c_r(:) 
real(8),allocatable :: z_c_r(:)
real(8),allocatable :: dx_c_r(:)
real(8),allocatable :: dy_c_r(:)
real(8),allocatable :: dz_c_r(:)
integer,allocatable :: ibcell_c_r(:,:,:)
real(8),allocatable :: bs_c_r(:,:,:)
real(8),allocatable :: qrs_m_r(:,:) !heat flux interpolated to melt grid
!-------------------------------------------------------------------
real(8),allocatable :: buf(:,:,:)  ! general buffer 1 
real(8),allocatable :: buf2(:,:,:) ! general buffer 2 

real(8),allocatable :: APO(:,:,:) 
real(8),allocatable :: res_mass(:,:,:) 
real(8),allocatable :: FZ(:,:,:),BS(:,:,:),SFP(:,:,:),fz2(:,:,:)
real(8),allocatable :: AP(:,:,:),AS(:,:,:,:,:)
real(8),allocatable :: FLX(:,:),FLXH(:,:),FLX_GB(:)
real(8),allocatable :: QRS(:,:),QRSLG(:,:)
real(8),allocatable :: qrs_prev(:,:),qc_surf0(:,:)
real(8),allocatable :: qrs_prev_r(:,:) !previous cycle heat transfer from combustion space into melt surface for regenerative calc
real(8),allocatable :: QRS_R(:,:) 
real(8),allocatable :: qrsc(:,:)   !surface heat flux on combustion grid
real(8),allocatable :: qrsc_r(:,:) !surface heat flux on regen combustion grid
real(8),allocatable,target :: area_s_spr(:,:)
real(8),allocatable,target :: area_c_spr(:,:)
real(8),pointer     :: area_spr_1(:,:)
REAL(8),ALLOCATABLE :: coverf(:,:)
REAL(8),ALLOCATABLE :: Cullet_T(:,:) !cullet temperature on surface
REAL(8),ALLOCATABLE :: Sand_T(:,:) !sand temperature on surface
REAL(8),ALLOCATABLE :: Surf_T(:,:)
real(8),allocatable :: dn_c(:,:)
real(8),allocatable :: dn_s(:,:)
REAL(8),ALLOCATABLE :: QCD(:,:,:)
REAL(8),ALLOCATABLE :: QLS(:,:,:) !Conduction wall heat loss through cell faces [W]
                                 !Includes other boundaries such as exits
REAL(8),ALLOCATABLE :: W1_K(:),W1_HA(:),W1_TA(:),W1_D(:),W1_E(:)
!REAL(8),ALLOCATABLE :: IXFF(:,:,:)
integer(2),allocatable :: ixff(:,:,:) !grid map for fieldview
real(8),allocatable :: Tgs_m(:,:) !surface temperature on melt grid
real(8),allocatable :: Tgs_c(:,:) !surface temperature on combustion grid
real(8),allocatable :: Tgs_c_r(:,:) !surface temperature on combustion grid alt. burner regen
real(8),allocatable :: last_Tgs_c(:,:) !last surface temperature on combustion grid
real(8),allocatable :: last_Tgs_c_r(:,:) !last surface temperature on combustion grid alt. burner regen

REAL(8),ALLOCATABLE,TARGET :: FLXHP_C(:),FLXHP_S(:)
REAL(8),ALLOCATABLE,TARGET :: QCD_C(:,:,:),QCD_S(:,:,:)
REAL(8),ALLOCATABLE,TARGET :: QRSP_C(:,:,:),QRSP_S(:,:,:)
REAL(8),ALLOCATABLE,TARGET :: PC0MR_LG(:,:,:),PS0MR_LG(:,:,:)
REAL(8),ALLOCATABLE,TARGET :: WPI_C(:),WPI_S(:)
REAL(8),ALLOCATABLE,TARGET :: PMRX_C(:),PMRX_S(:)
REAL(8),ALLOCATABLE,TARGET :: TM_C(:),TM_S(:)
REAL(8),ALLOCATABLE,TARGET :: RP_C(:),RP2_C(:),RP_S(:),RP2_S(:)
REAL(8),ALLOCATABLE,TARGET :: RP3_C(:),RP3_S(:)
REAL(8),ALLOCATABLE,TARGET :: PFRX_C(:),PFRX_S(:)
REAL(8),ALLOCATABLE :: DRP_C(:),DRP_S(:)
REAL(8),ALLOCATABLE :: CON(:,:,:),PULL(:,:)
real(8),allocatable :: area_exit(:)
REAL(8),ALLOCATABLE :: SP(:,:,:)
REAL(8),ALLOCATABLE,TARGET :: RG_B(:),RG2_B(:),RG3_B(:),DRG_B(:)
REAL(8),ALLOCATABLE,TARGET :: WGI_B(:)
REAL(8),ALLOCATABLE,TARGET :: FRX_GB(:)
REAL(8),ALLOCATABLE :: EBV(:,:,:),EBE(:,:,:)
REAL(8),ALLOCATABLE :: EBJ(:,:,:),EBI(:,:,:)
REAL(8),ALLOCATABLE :: EBQ(:,:,:),EBSG(:,:,:)
REAL(8),ALLOCATABLE :: DS_BUB(:,:,:),THETA_BUB(:,:,:)
!-------------------------------------------------------------------
REAL(8),ALLOCATABLE :: GFM(:,:,:,:),FLXM(:,:,:),ODR(:,:),WMS(:)
REAL(8),ALLOCATABLE :: SVF(:,:,:),FLYM(:,:,:),NTIMESM(:)
REAL(8),ALLOCATABLE :: XKC(:,:),XKT(:,:),ERC(:),ERT(:)
REAL(8),ALLOCATABLE :: DPADJ(:),CK0_M(:),BETA(:),DFACTM(:)
REAL(8),ALLOCATABLE :: RFM(:),RFMC(:),RESM(:,:),RRMS(:,:,:,:)
!-------------------------------------------------------------------
REAL(8),ALLOCATABLE :: QE(:,:,:),QEL(:,:,:,:)
REAL(8),ALLOCATABLE :: QEL0(:),QEL1(:)
REAL(8),ALLOCATABLE :: QA(:,:,:),QAL(:,:,:,:),QALW(:,:,:,:)
REAL(8),ALLOCATABLE :: WL(:),AKL0(:),AKL(:,:,:,:)
REAL(8),ALLOCATABLE :: TG(:,:,:),DEB(:),DEB0(:)
REAL(8),ALLOCATABLE :: ANG1(:,:),ANG2(:,:),ANG3(:,:)
REAL(8),ALLOCATABLE :: QTX1(:,:,:),Q2M(:,:,:),QTX2(:,:,:)
REAL(8),ALLOCATABLE :: QL(:,:,:)
!-------------------------------------------------------------------
REAL(8),POINTER :: PMRX_1(:)
REAL(8),POINTER :: RP_1(:),RP2_1(:),RP3_1(:),WPI_1(:)
REAL(8),POINTER :: TM_1(:),QRSP_1(:,:,:)
REAL(8),POINTER :: PFRX_1(:),QCD_1(:,:,:),FLXHP_1(:)
!-------------------------------------------------------------------
TYPE FP0
   REAL(8) U(3),T,P,DS,TH,F(3),A,C,K,MU,H,MR
END TYPE
TYPE (FP0),ALLOCATABLE :: LG(:,:,:) ! Liquid glass property array 
TYPE FP1
   REAL(8) U(3),T,P,DS,TH,F(3),H
END TYPE
TYPE (FP1),ALLOCATABLE :: LG0(:,:,:)
TYPE FP2
   REAL(8) U(3),T,DN,MR ! x,y,z velocity, temperature, number density, melting rate
END TYPE
TYPE (FP2),ALLOCATABLE,TARGET :: PC(:,:,:,:) ! Cullet properties 
TYPE (FP2),ALLOCATABLE,TARGET :: PS(:,:,:,:) ! Sand properties
TYPE (FP2),ALLOCATABLE,TARGET :: PCT(:,:,:,:),PST(:,:,:,:)
TYPE (FP2),POINTER :: P1(:,:,:,:),P1T(:,:,:,:)
TYPE FP3
   REAL(8) MR,TH ! melting rate and theta=volume fraction
END TYPE
! melting rate and volume fraction summed over size groups (pc0=cullet, ps0=sand)
TYPE (FP3),ALLOCATABLE,TARGET :: PC0(:,:,:),PS0(:,:,:)
TYPE (FP3),POINTER :: P10(:,:,:)
TYPE FP4
   REAL(8) CL,DS,H,H0,FR,FR2,MR
END TYPE
TYPE (FP4),TARGET :: PCA,PSA
TYPE (FP4),POINTER :: P1A
TYPE FP4B
   REAL(8) DN,U(3),T,P,Y(2),WT,DS,R,GR
END TYPE
TYPE (FP4B),ALLOCATABLE :: GB4(:,:,:,:),GBT(:,:,:,:)
TYPE FP3B
   REAL(8) TH
END TYPE
TYPE (FP3B),ALLOCATABLE :: GB3(:,:,:)
TYPE FPB
   REAL(8) CP,DS,H,H0,FR,FR2,WT,SG,B2
END TYPE
TYPE (FPB) :: GB
TYPE FPXB
   REAL(8) FR
END TYPE
TYPE (FPXB),ALLOCATABLE :: GBX(:)
TYPE BLR0
   REAL(8) T,FR
END TYPE
TYPE (BLR0),ALLOCATABLE :: BLR(:)
TYPE EB0
   INTEGER NTP,I1(3),I2(3),J1(3),J2(3),K1(3),K2(3)
   REAL(8) VLT,PWR
END TYPE
TYPE (EB0),ALLOCATABLE :: EB(:)
TYPE UDF0
   REAL(8) T,F
END TYPE
TYPE (UDF0),ALLOCATABLE,TARGET :: UDF_DS(:),UDF_MU(:),UDF_K(:),UDF_CL(:),udf_clc(:),udf_cls(:)
TYPE (UDF0),POINTER :: UDF(:)
real(8) cullet_cl !cullet specific heat, determined from the udf_clc user defined function
real(8) sand_cl   !sand   specific heat, determined from the udf_cls user defined function

real(8),allocatable :: h_pts(:) !enthalpy values associated with the [temperature, specific heat] pairs
										  !of values provided as the user defined function for the liquid glass
										  !specific heat property.


!-------------------------------------------------------------------
!DATA T0_R,P0_R/1.0D+2,1.01D+5/
DATA SIG,HCK/5.668D-8,1.439D-2/
!DATA CD_D_C,CD_D_S/5.0D-3,3.0D-3/ ! changed by Golchert fall 2004
!DATA CD_D_C,CD_D_S/2.0D-2,1.2D-2/ ! changed by Lottes 6/20/05
DATA CD_D_C,CD_D_S/1.6D-2,1.6D-2/ ! changed by Lottes 6/20/05
DATA NRADWALL/4/ ! changed by Golchert, fall 2004 from 3 to 4
!DATA YQUAL/.FALSE./ ! Lottes 3/23/2005: using other indicators: iglass_qual
DATA ID_EB,ID_EB0,ID_EB1/0,100,1/
END MODULE GBL_VAR  


