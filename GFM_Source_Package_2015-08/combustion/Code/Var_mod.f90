!     Var_mod.f90
!===================================================================
MODULE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!======================================================================
!     Actual Constants
!======================================================================
real(8) :: small20=1.0d-20
real(8) :: small16=1.0d-16
real(8) :: big=1.0d+20
real(8) :: zero=0.0d+0
real(8) :: one=1.0d+0
real(8) :: two=2.0d+0
real(8) :: half=0.5d+0
real(8) :: third=1.0d+0/3.0d+0
real(8) :: sixth=1.0d+0/6.0d+0
real(8) :: pi  =   3.141592653589793d+0
real(8) :: pi2 = 2*3.141592653589793d+0
real(8) :: pi4 = 4*3.141592653589793d+0

real(8) :: ru=8.3143d+3 !universal gas constant (J/kmol/K)
real(8) :: amd=1.75d+0 !exponent in temperature-dependent diffusivity eqn.
real(8) :: alphae=0.0d+0 !exponent of temperature in Arrhenius rate relation
real(8) :: acp=0.0d+0
real(8) :: amu=0.0d+0 !exponent of temperature in molecular viscosity

! Convergence criteria, iteration limits, tolerances
real(8) :: bdcon=1.0d-6 ! not used, acceptable mass residual for a droplet phase calculation
real(8) :: becon=1.0d-4 ! not used  
real(8) :: bpcon=1.0d-6 ! not used, acceptable mass residual for a particle phase calculation
real(8) :: btcon=1.0d-9 ! not used, acceptable mass residual for a transient calculation

integer :: maxti=3 !  not used, maximum allowable time step iterations

real(8) :: rad_con=1.0d-6 !radiation convergence value
real(8) :: chg_rad=1.0d+6 !change in heat flux to melt between rad iterations
integer :: rad_qe_frequency=10 !number of gas iterations between emission radiation only calculations

real(8) :: dn_mn=1.0d-10
real(8) :: f_mx=1.5d+0 !max mass flow adjustment factor at exits
real(8) :: f_mn=0.8d+0 !min mass flow adjustment factor at exits
real(8) :: p_mx=0.2d+0
real(8) :: p_mn=-0.6d+0
real(8) :: t_mnK=2.0d+2 ! temperature minimum bound (K)
real(8) :: t_mn=2.0d+2/5.0d+2 ! temperature minimum bound, normalized by t0
real(8) :: th_mn=0.4d+0 ! bound for theta

real(8) :: comp_tolerance=9.0D-15         ! comp_tolerance = tolerance for array comparison 

! Program flow type parameters
logical :: steady=.true. !.true. for steady state computations
logical :: turb=.true.
logical :: smpler=.true. !.true. if for the simpler algorithm (false for simple)
integer :: idebug=0 !  
logical :: cyl=.false. ! .true. for cylindrical coordinates, .false. for a cartesian coordinate
integer :: irc=1 !1 => calc residuals in adlbl3 module
integer :: ifp=1 ! not used, an indicator for full print in sprint
integer :: nphas=1 ! number of phases

! Adlbl equation solver iteration numbers (nelg numbers) for gas phase.
!     Also index numbers for source routine.
!     Note the Solvers module will change ntimesg, but that module is
!     not being used, so ntimesg is considered to be constant.

integer :: ntimesg(20) !values moved to relaxation factors file
!integer :: ntimesg(20)=(/2, & ! 1  u momentum in x direction
!                         2, & ! 2  v momentum in y direction
!                         2, & ! 3  w momentum in z direction
!                         6, & ! 4  pressure
!                         5, & ! 5  pressure correction
!                         2, & ! 6  fuel
!                         3, & ! 7  co2
!                        10, & ! 8  enthalpy ! 8/11/05  optimize to get stable well converged enthalpy
!                         2, & ! 9  turbulent kinectic energy
!                         2, & !10  epsilon
!                         3, & !11  n2 ! 8/18/05  optimize to get stable well converged species
!                         3, & !12  o2 ! 8/18/05  optimize to get stable well converged species
!                         2,2,2,2,2,2,2,2/) !rest not used

! Adlbl equation solver iteration numbers (nel numbers) for dispersed phase
!     Also index numbers for source routine
integer :: ntimes(20)=(/5, & ! 1  particle number density dn
                        1, & ! 2  particle du
                        1, & ! 3  particle dv
                        1, & ! 4  particle dw
                        1, & ! 5  particle dt
                        1, & ! 6  coke dc
                        1, & ! 7  enthalpy
                        1, & ! 8  turbulent kinectic energy
                        1, & ! 9  epsilon
                        1, & !10  nitrogen
                        1, & !11  steam
                        1, & !12  coke
                        1, & !13  droplet number density dn
                        1, & !14  droplet du
                        1, & !15  droplet dv
                        1, & !16  droplet dw
                        1, & !17  droplet dt
                        1, & !18  droplet dc
                        1,1/) !rest not used

! Relaxation factors for loops - moved to relaxation file
!real(8) :: rfl(21)=0.7D+0 !relaxation factors for loops
!real(8) :: rflc(21)=1.0d0-0.7D+0 !complementary relaxation factors for loops
!                             rfl(3)=0.9d+0     ! needs optimizing
!     1: fuel,               8: light oil        15:            
!     2: richness,           9: temperature,     16:   
!     3: enthalpy,          10:                  17:   
!     4: turb ke,           11:                  18: dnst
!     5: epsilon,           12:                  19: evp (evap rate)
!     7: nitrogen,          13:                  20: 
!     6: steam              14:                  21: crate

! Equation indices
integer :: iyf=1    ! equation index for fuel mass fraction
integer :: iyo2=2   ! equation index for oxygen
integer :: ih=3     ! equation index for enthalpy
integer :: ik=4     ! equation index for turbulent kinetic energy
integer :: ieps=5   ! equation index for turbulent dissipation rate
integer :: iyco2=6  ! equation index for carbon dioxide
integer :: iyn2=7   ! equation index for nitrogen
integer :: iyh2o=9  ! equation index for steam
integer :: lstar=1  ! starting number of species calculation
integer :: lend=7   ! end number of species calculation

! Combustion and radiation parameters
real(8) :: stox1=2.0d+0 !stoichiometric ratio of oxidizer to fuel (kmol ox/kmol fu)
real(8) :: stox2=1.0d+0 
real(8) :: stoxair=4.76d+0*2.0d+0 !cz, not used, stoichiometric ratio of air to fuel (kmol air/kmol ch4)
real(8) :: be=1.0d+6 !forward react. arrhenius coeff.
real(8) :: eac=-1.07d+8 !forward react. activation energy (j)
real(8) :: sig=5.668d-8 !Stefan-Boltzmann constant (W/m^2/K^4)
real(8) :: hck=1.439d-2
real(8) :: t0_r=1.0d2
real(8) :: p0_r=1.01325d5
real(8) :: dnst_soot=1.8D+3 !Soot density kg/m**3
real(8) :: gl0=7.0d-2 !  gas phase thermal conductivity (w/m k)

! w: molecular weight (kg/kmol)
real(8) :: wair=28.85d+0 !air molecular weight (kg/kmol)
real(8) :: wf=16.0426d+0     ! fuel molecular weight (kg/kmol)
real(8) :: wo2=31.9988d+0    ! oxygen molecular weight (kg/kmol)
real(8) :: wco=28.0104d+0    ! carbon monoxide molecular weight (kg/kmol)
real(8) :: wco2=44.0098d+0   ! carbon dioxide molecular weight (kg/kmol)
real(8) :: wh2o=18.0152d+0   ! steam molecular weight (kg/kmol)
real(8) :: wn2=28.0134d+0    ! nitrogen molecular weight (kg/kmol)
real(8) :: wno=30.0061d+0    ! nitric oxide molecular weight (kg/kmol)
real(8) :: stoich=2.0d0*31.9988d+0/16.0426d+0 ! =stox1*wo2/wf, the stoichiometric mass ratio of oxidizer to fuel

! cp's are coefficients for specific heats represented as 4th degree
! polynomials, constant term 1st array element, then up.
! cp's are normalized by being divided by cp0
real(8) :: cp0=1.0057d+3 !reference specific heat (j/kg/k)

real(8) :: cpf(5)=(/(2.156d+3/1.0057d+3),0.0d0,0.0d0,0.0d0,0.0d0/) ! specific heat coefficient for fuel
real(8) :: cpo2(5)=(/(0.9136d+3/1.0057d+3),0.0d0,0.0d0,0.0d0,0.0d0/) ! specific heat coefficient for oxygen
real(8) :: cpco2(5)=(/(2.0d+3/1.0057d+3),0.0d0,0.0d0,0.0d0,0.0d0/) ! specific heat coefficient for carbon dioxide
real(8) :: cph2o(5)=(/(2.0d+3/1.0057d+3),0.0d0,0.0d0,0.0d0,0.0d0/) !  specific heat coefficient for steam
real(8) :: cpn2(5)=(/(0.526d+3/1.0057d+3),0.0d0,0.0d0,0.0d0,0.0d0/) ! specific heat coefficient for nitrogen
real(8) :: dcp=(2.156d+3/1.0057d+3) &
              +31.9988d+0*2/16.0426d+0*(0.9136d+3/1.0057d+3) &
              -44.0098d+0/16.0426d+0*(2.0d+3/1.0057d+3) &
              -18.0152d+0*2/16.0426d+0*(2.0d+3/1.0057d+3)
           !dcp=cpf(1)+wo2*2/wf*cpo2(1)-wco2/wf*cpco2(1)-wh2o*2/wf*cph2o(1)

! Cubic fit constants
real(8) :: kg3= 2.0d-8    ! cubic fit for (kgas) thermal conductivity of air 300-2500  K 
real(8) :: kg2=-5.9d-5     
real(8) :: kg1= 1.1d-1     
real(8) :: kg0=-3.3262d+0 

real(8) :: gmu3= 2.0d-15    ! cubic fit for (gmu_T) viscosity of air 300-2500  K 
real(8) :: gmu2=-1.1d-11     
real(8) :: gmu1= 4.5d-8     
real(8) :: gmu0= 7.0d-6 

! Reference values
real(8) :: t0=5.0d+2     !reference temperature (k), should not be equal to tl0
real(8) :: tl0=298.15d+0 !ambient temperature
real(8) :: tl00=5.33d+2  !constant used when setting gdf0 and gmu_ref
real(8) :: trdg=298.15d+0/5.0d+2             !trdg=tl0/t0
real(8) :: trdgc=1.0d0-(298.15d+0/5.0d+2)    !trdgc=one-trdg
real(8) :: h_0=1.0057d+3*(5.0d+2-298.15d+0)  !h_0=cp0*(t0-tl0)
real(8) :: r0    =1.0d0 !reference length (m)
real(8) :: area0 =1.0d0 !reference area (m^2)   = r0**2
real(8) :: vol0  =1.0d0 !reference volume (m^3) = r0**3

real(8) :: yf0=0.50d+0  !reference fuel concentration (mass fraction)
real(8) :: yox0=0.49d+0 !reference oxidizer concentration (mass fraction)
real(8) :: yn20=0.01d+0 !reference nitrogen concentration (mass fraction)
real(8) :: yp10=0.0d0   !reference carbon dioxide concentration (mass fraction)
real(8) :: yp20=0.0d0   !reference steam concentration (mass fraction)
real(8) :: wt0=1.0d0/(0.5d0/16.0426d+0+0.0d0/44.0098d+0+0.0d0/18.0152d+0 &
                     +0.49d+0/31.9988d+0+0.01d+0/28.0134d+0)
           !wt0=one/(yf0/wf+yp10/wco2+yp20/wh2o+yox0/wo2+yn20/wn2) !reference gas molecular weight (kg/kmol)
           !Note that this seems to be an oxy-fuel reference 

! Turbulent constants
!Reference: B.E. Launder and D.B. Spalding, "The Numerical Computation of Turbulent Flows",
!           Computer Methods in Applied Mechanics and Engineering 3 (1974) 269-289, 
!           North Holland Publishing Company
real(8) :: cmu = 9.0d-2 !k-e turbulence model constant
real(8) :: kappa_cmu = 0.4d0 * 9.0d-2**0.25d+0 !turbulence model constant for energy wall function 
real(8) :: e_cmu = 9.0d0 * 9.0d-2**0.25d+0 !turbulence model constant for energy wall function 
real(8) :: c_htw !turbulence model constant for energy wall function
real(8) :: ct1=1.44d+0
real(8) :: ct2=1.92d+0
real(8) :: ctea=5.0d-2
real(8) :: ctda=1.0d0 ! (1/m) normalization constant for turbulent dissipation rate: epsi0=cdta*tk0**2
real(8) :: sigma(12)=(/0.7d+0,0.7d+0,0.7d+0,1.0d+0,1.0d+0,0.7d+0, &
                       0.7d+0,0.7d+0,0.7d+0,0.7d+0,0.7d+0,0.7d+0/)
real(8) :: dsigma=9.0d+0
real(8) :: psigma=9.0d+0
real(8) :: ssigma=0.7d+0

! Minor Species Reaction Constants
integer :: nsp0=7   !number of minor species (also called sub-species)
integer :: nrm0=5   !number of chemical reactions between sub species
integer :: lstarm=1 ! start index for sub-species calculation
integer :: lendm=7  ! end index for sub-species calculation

real(8) :: wms(7)=(/16.0426d+0, & ! 1= wf     CH4 molecular weight (kg/kmol) 
                    31.9988d+0, & ! 2= wo2    O2  molecular weight (kg/kmol)
                    28.0104d+0, & ! 3= wco    CO  molecular weight (kg/kmol)
                    18.0152d+0, & ! 4= wh2o   H2O molecular weight (kg/kmol)
                    44.0098d+0, & ! 5= wco2   CO2 molecular weight (kg/kmol)
                    28.0134d+0, & ! 6= wn2    N2  molecular weight (kg/kmol)
                    30.0061d+0/)  ! 7= wno    NO  molecular weight (kg/kmol)

real(8) :: ck0_m(5)=(/1.66d15,7.94d14,2.24d14,7.94d22,9.33d14/)
real(8) :: beta(5)=(/0d0,0d0,0d0,0d0,-0.5d0/)
real(8) :: odr(5,2)=(/1.46d0,1.6904d0,1d0,0.721d0,1d0, & ! odr(1:5,1)
                      0.5217d0,1.57d0,0d0,4.0111d0,0.5d0/) !odr(1:5,2)
real(8) :: erc(5)=(/20643d0,11613d0,62281d0,44369d0,68899d0/)


! droplet & particle constants
real(8) :: rlm0=60.0d-6 
real(8) :: ddens=9.1d+2   !droplet density (kg/m**3)
real(8) :: pdens=0.934d+3 !particle density (kg/m**3)
           !csl      pdens=1.383d+3 
real(8) :: dmfr0=1.0d0 !droplet mass flow rate reference? 
real(8) :: pmfr0=1.0d0 !pmfr0=dmfr0  particle mass flow rate reference?
real(8) :: rd_p=36.0d-6
real(8) :: ckbet=(1.0d0/0.934d3)/(1.0d0/9.1d2) !ckbet=(pmfr0/pdens)/(dmfr0/ddens)
real(8) :: ckalp=0.0d0 !not used  !catalyst deactivation coefficient (0.01125/s)
           !cz      ckalp=1.125d-2 
real(8) :: cl=3.3d+3   !droplet specific heat (j/(kg*k))
real(8) :: cl_p=3.3d+3 !particle specific heat (j/(kg*k))
real(8) :: ttwe=5.0d+2/5.0d+2 !ttwe=ttwe/t0   temperature for total drop on wall evaporation (k)
real(8) :: nt=4 !parameters in inlet droplet size distribution  -  not used
real(8) :: ns=4
real(8) :: acdd(3)= & !coefficients in psi function for drop turb. diffusivity
                   (/(2.0d+6*60.0d-6), &          ! 1   2.0d+6*rlm0
                     (6.0d+10*60.0d-6*60.0d-6), & ! 2   6.0d+10*rlm0*rlm0
                     0.0d0/)                      ! 3   not used
 
! transient computation parameters
logical :: tprint=.false. ! not used, controls when to print state in unsteady calculations, initial value is always false.
integer :: istep=0 ! not used, time step number
integer :: maxts=200 ! not used, the number of time steps to run
real(8) :: tp0=2.0d-2 !reference time in seconds
real(8) :: dt0=1.0d-2 !time step as fraction of tp0
integer :: ipcyc=20 !not used, number of time steps between sprints

! File unit identifiers 

integer :: ncon      =   6 ! unit for standard command window
integer :: nu_restg  =   7 ! unit for file 'rg'//runum//'c.d'  gas phase restart 
integer :: nu_rests  =   8 ! unit for file 'rs'//runum//'c.d'  minor species restart
integer :: nu_restr  =   9 ! unit for file 'rr'//runum//'c.d' radiation restart
integer :: nu_run    =  10 ! unit for file 'runs.dat'  inputs case number, only file in current directory
integer :: nu_sbc    =  11 ! unit for file 'sbc'//runum//'c.txt'  boundary conditions
integer :: nu_gfm    =  15 ! unit for file 'gfm.dat'  status to gui
integer :: nu_cycle  =  16 ! unit for file 'cycleInfo.txt'  cycle info from gui
integer :: nu_itt    =  17 ! unit for file 'it'//runum//'t.dat'
integer :: nu_itm    =  18 ! unit for file 'it'//runum//'m.dat'
integer :: nu_guiup  =  19 ! unit for file 'gui_update.txt'
integer :: nu_oldbug =  20 ! unit for debug.out (not used)
integer :: nu_twal   =  21 ! unit for file 'twall'//runum//'c.txt'
integer :: nu_twala  =  22 ! unit for file 'twallavg'//runum//'c.plt'
integer :: nu_wtot   =  25 ! unit for file 'wall_tot'//runum//'c.plt'            commented out
integer :: nu_rad    =  26 ! unit for file 'rad_detail'//runum//'c.plt'
integer :: nu_radw   =  27 ! unit for file 'radw_detail'//runum//'c.plt'         commented out
integer :: nu_gdw    =  30 ! unit for copy of grid file for printing wall values
!integer :: nu_aox    =  31 ! unit for file 'aoxid'//runum//'c.plt'
integer :: nu_soot    =  31 ! unit for file 'soot_cal'//runum//'c.plt'
integer :: nu_grid   =  32 ! unit for file 'gd'//runum//'c.dat'
!integer :: nu_ck0    =  33 ! unit for file 'ck0_m.dat'          no longer used
integer :: nu_rt     =  34 ! unit for file 'rt'//runum//'c.out'  main output of variables
integer :: nu_sum    =  35 ! unit for file 'summary'//runum//'c.txt'
integer :: nu_kinet  =  36 ! unit for file 'kinetic.d'
integer :: nug       =  47 ! unit for gas convergence file
integer :: nud       =  48 ! unit number for droplet convergence file (not used)
integer :: nup       =  49 ! unit number for particle convergence file (not used)
integer :: nu_cwal   =  50 ! unit for file 'conv_wall'//runum//'c.plt'
integer :: nu_qcave  =  51 ! unit for file='qc_ave'//runum//'c.plt'
integer :: nu_qamb   =  52 ! unit for file='q_amb'//runum//'c.plt'
integer :: nu_qrs    =  53 ! unit for file='qrs'//runum//'c.plt'
integer :: nu_qconv  =  54 ! unit for file='q_conv'//runum//'c.plt'
                           !   58   'rs'//crun//'.d'    used for file compare debugging
                           !   59   'rs'//crun//'2.d'   used for file compare debugging
                           !   60   'rscomp.txt'        used for file compare debugging
integer :: nu_bTsta  =  61 ! unit for '\bTstart'//runum//'c.txt'
integer :: nu_htran  =  62 ! unit for 'heat_tran_wall'//runum//'c.txt'
integer :: nu_prtfv1 =  70 ! unit for file 'FV_vnodes'//runum//'m.dat   Fieldview grid nodes file
integer :: nu_prtfv2 =  71 ! unit for file 'FV_variables'//runum//'m.dat Fieldview function file for variable data
integer :: nu_prtfv3 =  72 ! unit for file 'FV_names'//runum//'m.nam Fieldview variable name file 

integer :: nu_walbug =  80 ! unit for files debugging wall boundary arrays
integer :: nu_walvis =  81 ! unit for files for visualizing certain boundary arrays
integer :: nu_bTemp  =  82 ! unit for file 'bTemp'//runum//'c.txt'  boundary T and q values
integer :: nu_Tave   =  95 ! unit for file 'Tave'//runum//'c.plt'
integer :: nu_info   =  96 ! unit for file 'Info'//runum//'c.plt'
integer :: nu_gres   =  97 ! unit for file 'gresid'//runum//'c.plt'
integer :: nu_gresx  =  98 ! unit for file 'gresid_xtra'//runum//'c.plt'
integer :: nu_hres   =  99 ! unit for file 'hresid'//runum//'c.plt'
!integer :: nu_mscon  = 100 ! unit for file 'convms'//runum//'c.plt'
integer :: nu_mres   = 100 ! unit for file 'mresid'//runum//'c.plt'
integer :: nu_relax  = 105 ! unit for file 'it'//runum//'T_relax.dat'
integer :: nu_stop   = 110 ! unit for file runend.txt
integer :: nu_flx    = 111 ! unit for file 'fchg'//runum//'.plt'
integer :: nu_qe     = 118 ! unit for file 'qe'//runum//'c.dat'
integer :: nu_qa     = 119 ! unit for file 'qa'//runum//'c.dat'
integer :: nu_gresp  = 120 ! unit for file 'gresidp'//runum//'c.plt'
integer :: nu_gresxp = 121 ! unit for file 'gresid_xtrap'//runum//'c.plt'
integer :: nu_mresp  = 122 ! unit for file 'mresidp'//runum//'c.plt'
integer :: nu_rxfact = 123 ! unit for file 'relaxfactor'//runum//'c.txt'
integer :: nu        = 6   ! unit number for SPRINT output. Note that the value of NU may change
integer :: nutmp     ! temp unit # for local use (file will not always be open)

!File control flags set =1 for printing
!Comments give file unit and name being controlled, {sbc} indicates specified by sbc namelist 

integer :: gui_update = 1 ! 15  {sbc} 1=> CFD will provide status updates to the gui via a gfm.dat file 
integer :: itwal      = 1 ! 21 {sbc} 'twall'//runum//'c.txt'   wall temperature array
integer :: iwall_info = 0 ! 22  'twallavg'//runum//'c.plt'
                          ! 25  'wall_tot'//runum//'c.plt'            commented out
integer :: irad_detail= 1 ! 26 {sbc} 'rad_detail'//runum//'c.plt'
                          ! 27 'radw_detail'//runum//'c.plt'          commented out
integer :: isum       = 1 ! 35  {sbc} 'summary'//runum//'c.txt'
integer :: irad_rad   = 1 ! 50 {sbc} 'conv_wall'//runum//'c.plt'
integer :: irad_qc    = 0 ! 51 'qc_ave'//runum//'c.plt' 
integer :: irad_amb   = 0 ! 52 'q_amb'//runum//'c.plt'
integer :: irad_qrs   = 0 ! 53 'qrs'//runum//'c.plt'
integer :: irad_conv  = 0 ! 54 'q_conv'//runum//'c.plt'
integer :: iwalvis    = 0 ! 30 wall grid file for for visualizing boundary arrays
                         ! 81 for visualizing certain boundary arrays
integer :: isoot_cal = 0 ! 31 {sbc} 'soot_cal'//runum//'c.plt'   soot kinetics calibration
integer :: iconv     = 1 ! 47 {sbc} gas mass residual convergence file
integer :: ibTsta    = 0 ! 61 '\bTstart'//runum//'c.txt'

integer :: ihtran    = 0 ! 62 ! 'heat_tran_wall'//runum//'c.txt'
integer :: ifieldview = 0 ! 70  {sbc} 'FV_nodes'//runum//'m.dat'   output Fieldview post processor grid nodes file
                          ! 71  {sbc} 'FV_variables'//runum//'m.dat'   output Fieldview post processor variable values file
                          ! 72  {sbc} 'FV_names'//runum//'m.nam'   output Fieldview post processor variable names file

integer :: iwalbug1  = 0 ! 80 'mapb'//runum//'c.txt'  cells changed for view factors
integer :: iwalbug2  = 0 ! 80 'vf_sum'//runum//'c.txt' view factor sums
integer :: iwalbug3  = 0 ! 80 'vf'//runum//'c.txt'  view factors - hugh file
integer :: iwalbug4  = 0 ! 80 'bface'//runum//'c.txt'  boundary patch areas
integer :: ibTemp    = 0 ! 82 'bTemp'//runum//'c.txt'  boundary T and q values
integer :: iTave     = 1 ! 95 {sbc} 'Tave'//runum//'c.plt'
integer :: iInfo     = 1 ! 96 {sbc} 'Info'//runum//'c.plt'
integer :: igresid   = 1 ! 97 {sbc} 'gresid'//runum//'c.plt'       gas PDE residual file (pressure, momentum, enthalpy)
integer :: igresidx  = 1 ! 98 {sbc} 'gresid_xtra'//runum//'c.plt'  gas PDE residual file (species, k, epsilon)
integer :: ihresid   = 0 ! 99 'hresid'//runum//'c.plt'
integer :: imresid   = 1 ! 100 {sbc} 'mresid'//runum//'c.plt'      minor species residual convergence file
integer :: irelax    = 1 ! 105 {sbc} 'it'//runum//'T_relax.dat'
integer :: iflx      = 1 ! 111 {sbc} 'fchg'//runum//'.plt' melt surface flux change 
integer :: iqaqe     = 0 ! 118 'qe'//runum//'c.dat'  melt surface emission heat flux
                         ! 119 'qa'//runum//'c.dat'  melt surface absorption heat flux
integer :: igresidp  = 1 ! 120 {sbc} 'gresidp'//runum//'c.plt'       gas PDE residual file (pressure, momentum, enthalpy)
integer :: igresidxp = 1 ! 121 {sbc} 'gresid_xtrap'//runum//'c.plt'  gas PDE residual file (species, k, epsilon)
integer :: imresidp  = 0 ! 122 'mresid'//runum//'c.plt'      minor species residual convergence file
           !integer :: ipre_resid = 1 ! 120,121,122 pre-solve residual files
           !integer :: iaft_resid = 1 ! 97,98,100 after-solve residual files

!Variables used in ported solver or array comparison routines, just need definitions
integer :: myid=0 !in port of solver routines from parallel, processor #
integer :: nprocs=1 !# of processors in parallel code, always = 1 here
integer :: lastid=0 !last processor #, only used in parallel solver code
integer :: ipde_resid=0 !flag to indicate calculation of residuals in core solver
integer :: msym=9 !map value for symmetric wall B.C. -used in parmflo, not here
! Set for array_dif module use
integer :: ivap=0
integer :: icond=0
integer :: icoke=0
integer :: ndg=0
integer :: npg=0

!-----------------------------------------------
! Define variables used in the enthalpy <=> temperature calculations, using information
! from the National Institute of Standards and Technology (NIST).

integer :: nmax=10 !maximum number of Newton iterations for determining temperature from enthalpy
real(8) :: tolerance_enth=1.0d-11 !minimum change limit for temperature based on enthalpy calculation
integer :: lo_hi(5) !index determining whether low or high temperature for NIST constants are used
real(8) :: real_T !actual temperature in K, not normalized
real(8) :: h_s !sensible enthalpy
real(8) :: h_molar !molar enthalpy
real(8) :: T_old !old temperature
real(8) :: T_new !new temperature
real(8) :: changeT !change in temperature
real(8) :: enth_func !function used when determining temperature based on enthalpy
real(8) :: enth_func_prime !derivative of the enth_func
real(8) :: w_mix !molecular weight of the mixture
real(8) :: a_mix !sum of the chemical mass fractions times their A coefficients
real(8) :: b_mix !sum of the chemical mass fractions times their B coefficients
real(8) :: c_mix !sum of the chemical mass fractions times their C coefficients
real(8) :: d_mix !sum of the chemical mass fractions times their D coefficients
real(8) :: e_mix !sum of the chemical mass fractions times their E coefficients
real(8) :: f_mix !sum of the chemical mass fractions times their F coefficients
real(8) :: h_mix !sum of the chemical mass fractions times their H coefficients

real(8) :: cutT(5) = & !cut off temperature to determine NIST lo/hi coefficient set
            (/1300.0d0, &   ! fuel lo/hi cut off temperature to determine coefficients
              6000.0d0, &   ! oxygen lo/hi cut off temperature to determine coefficients
              1200.0d0, &   ! carbon dioxide lo/hi cut off temperature to determine coefficients
              6000.0d0, &   ! nitrogen lo/hi cut off temperature to determine coefficients
              1700.0d0/)    ! steam lo/hi cut off temperature to determine coefficients

integer :: gf_chem_index(5) !translates to the fourth index of array gf(:,:,:,:) for the chemicals 

!NIST coefficient arrays: 
!  1st index for chemical: CH4 (fuel), O2, CO2, N2, H2O
!  2nd index for 1=lo range, 2=hi range
real(8) :: a_nist(5,2)
real(8) :: b_nist(5,2)
real(8) :: c_nist(5,2)
real(8) :: d_nist(5,2)
real(8) :: e_nist(5,2)
real(8) :: f_nist(5,2)
real(8) :: h_nist(5,2)

! End define variables used in the enthalpy <=> temperature calculations
!-----------------------------------------------


!======================================================================
! Items that are generally constant for a run but may be modified,
!especially after namelist or restart files have been read
!======================================================================


!----------------------------------------------------
! Initialize developer flags and control parameters

integer :: in_run=0 !0 = run startup, 1 = all files for run have been read in
integer :: id_rad=-1 !default to not doing radiation, >0 when doing radiation
integer :: itr_gas=0 !gas phase iteration counter (preserved across restarts)
integer :: imsflow_done=0 !=1 after msflow has been called or the minor species restart file has been read
integer :: i1st_rad_done=0 !=1 after 1st full radiation calc. has been done, 0 otherwise
integer :: i1st_rad_qe_done=0 !=1 after 1st qe only radiation calc. has been done, 0 otherwise
integer :: info_data_start=0  !0 => need to print column heading in info file, 1 otherwise
integer :: i_air_flow_only=0  !1 => init species concentration for air only, else set according to reactions
integer :: irad_test=0 !1 => special code to test radiation for case 50s
        !integer :: qe_scale_back=0  !keep track of qe scale backs for printing in info file
        !real(8) :: qe_scale_back_amount=0.0D+0

! Program flow type parameters
logical :: REACT=.TRUE. ! {sbc} true if reaction takes place
integer :: IRSTYP=0 ! {sbc} Fresh start with no backup file, 1=>Restart by using data from the backup file
integer :: IRSTYPM=0 ! {sbc} Fresh start for minor species
integer :: MS=0 ! {sbc} 0=> don't do sub-species calculation, otherwise do it
integer :: itr_ms=0 !minor species iteration count
integer :: interval_rad=100 ! {sbc} # of gas iterations between radiation computations
integer :: initial_gitr=100 ! {sbc} !# of gas iterations to do in 1st global iteration
                            ! before the ms and radiation interval begins
integer :: cycling=0 ! {sbc} not doing automatic domain cycling
integer :: oxy_fuel=0 ! {sbc} not using oxy fuel for soot option
real(8) :: T_init=300.0d+0 ! {sbc} initial quess for temperature field (normalized)
real(8) :: aform=400.0d+0  ! {sbc}  kinetic constant soot formation, pre-exponential const. in soot formation source term
real(8) :: aoxid=26.0d+0  ! {sbc} kinetic constant soot oxidation,  pre-exponential const. in soot sink source term
                           !aform=400.0d0, aoxid=26.0d0
                           !aform=.134d+0, aoxid=500.0d+0
real(8) :: esf=5.0d+7      ! {sbc}  activation energy for soot formation J/kmol
real(8) :: eso=5.0d+7      ! {sbc}  activation energy for soot oxidation J/kmol
real(8) :: q_melt_req=1.0E+06 ! energy required for melt default (W)
           !q_melt_req=0.74694352735085E+06 ! (W) TC21 case0020
           !q_melt_req=0.27667460382403E+07 ! (W) Pittston B case1703
real(8) :: available_parameter=1.0E+06 ! not used, possible future use as a parameter passed in the Sbc file

! Convergence criteria, iteration limits, tolerances
real(8) :: BGCON=1.0D-8 ! {sbc} acceptable mass residual for a gas phase calculation
real(8) :: BMCON=1.0D-7 ! {sbc} acceptable mass residual for a minor species calculation
integer :: imscon=0 ! 1 => acceptable mass residual for each of the minor species 
integer :: MAXGI=2000 ! {sbc} maximum allowable gas phase iterations
integer :: maxgi_save ! should not be needed, is used because setup and initsv are called again after a rad calc.
integer :: MAXSI=1000 ! {sbc} maximum allowable global iterations
integer :: MAXMS=300 ! {sbc} maximum allowable sub-species iterations
real(8) :: T_MX=3.0D+3 ! a temperature maximum bound, will be normalized later
real(8) :: U_MX=1.0D+3 ! velocity maximum

! Relaxation factors (and compliment factors) for certain variables - moved to file
real(8) rf(21),rfc(21),rfl(21),rflc(21)
real(8) rfTsurf,rfTsurf_c
real(8) general_rf !general relaxation value
real(8) soot_rf ! relaxation factor for soot (temperary use, saved in last rfm)

real(8) rf_hsource ! relaxation factor for enthalpy source term
real(8) rf_soot_form_source ! relaxation factor for soot formation source term
real(8) rf_soot_oxid_source ! relaxation factor for soot oxidation source term
real(8) rf_aform !used during soot calibration for relaxing the soot formation adjustment
real(8) rf_afover !used during soot calibration for relaxing the soot formation adjustment
real(8) soot_cap !maximum allowed value of soot mass fraction
real(8) T0_sf !minumum temperature [K] at which soot formation may occur
real(8) T0_so !minumum temperature [K] at which soot oxidation may occur

integer general_ntimes
!real(8) :: rf(21)=(/0.3d0, & ! 1  u momentum in x direction (pressure correction)
!                    0.3d0, & ! 2  v momentum in y direction (pressure correction)
!                    0.3d0, & ! 3  w momentum in z direction (pressure correction)
!                    0.1d0, & ! 4  droplet-gas heat transfer
!                    1.0d0, & ! 5  particle-gas heat transfer
!                    0.7d0,0.7d0,0.7d0,0.7d0,0.7d0,0.7d0,0.7d0, & ! 6-12 not used
!                    0.3d0, & !13  number density dn
!                    0.3d0, & !14  du
!                    0.3d0, & !15  dv
!                    0.3d0, & !16  dw
!                    0.6d0, & !17  dt
!                    0.6d0, & !18  density dnst
!                    0.6d0, & !19  evp (evaporation rate)
!                    0.6d0, & !20  pressure (simple only)
!                    0.6d0/)  !21  crate
!real(8) :: rfc(21)=(/0.7d0,0.7d0,0.7d0,0.0d0,0.0d0,0.3d0,0.3d0,0.3d0,0.3d0,0.3d0,0.3d0, &
!                    0.3d0,0.7d0,0.7d0,0.7d0,0.7d0,0.4d0,0.4d0,0.4d0,0.4d0,0.4d0/)
!real(8) :: rfTsurf=0.7d0   ! relaxation factor for surface temperature 
!real(8) :: rfTsurf_c=0.3d0 ! = one-rfTsurf

! Physical properties 
real(8) :: GRA(3)=(/0.0d0,0.0d0,-9.80621D+0/) ! gravitational acceleration (m/s**2)             
           !Gravity is always in the minus z direction for this application
real(8) :: PG0=1.01325D+5 ! {sbc} pressure (pa) 
real(8) :: GMFR=1.0d0 ! {sbc} total incoming mass flow rate (kg/s)
real(8) :: GDF0=1.6D-5 ! gas phase mass diffusion coefficient (?)
real(8) :: GCP0=1.017D+3 ! not used, gas phase specific heat (J/kg)
real(8) :: GMU_ref=2.6D-5 ! {sbc} gas phase molecular viscosity
real(8) :: Q0=2.42D+6 ! {sbc} heat of combustion (J/kg-fuel), Should this be set to natural gas?
real(8) :: ELH=0.3489D+6 ! latent heat (J/kg) (oil cracking)

!----------------------------------------------------------------------
! END Items that are constant for a run but may be modified after namelist or restart files have been read
!----------------------------------------------------------------------

!LOGICAL RADGRAY
LOGICAL EXST
logical runstop_exist !true is user requested program stop
CHARACTER*80 long_title
CHARACTER*40 FILENAME,TITLE
CHARACTER*40 FILENAME2
CHARACTER*32 FMR,FMX,FMZ
CHARACTER*11 RTITLE,GTITLE
CHARACTER*4 RUNUM,GDNUM
character*4 crun
character*8 casedir !name of case folder
character ps
character*5 calc_q_cnt_string !for debugging print

integer calc_q_cnt  !for debugging print
INTEGER(2) IHR1,IHR2,IMIN1,IMIN2,ISEC1,ISEC2,IHUND1,IHUND2 
INTEGER IRUN,IRUNP1
INTEGER NEL,IDIT,KEY,KL
!INTEGER KD !Obsolete equation type indicator
integer eqtyp !equation type: 1=condensed phase, 2=gas phase, 3=minor species
INTEGER NELG
INTEGER MP,MZ,NP,NZ,LP,LZ,ITN,ITN_R
!grid sizes used in array_dif
integer nx_gbl
integer nxf
integer nyf
integer nzf
integer nxc_gbl
integer nxc
integer nyc
integer nzc
!----------------------------
INTEGER MPM1,MPM2,MPD2,NPM1,NPM2,NPD2,LPM1,LPM2,LPD2
INTEGER I_REF,J_REF,K_REF,LASTIT
INTEGER NDP0,NPT0,NDNP,NR0
INTEGER NJY1,NJY2,LKZ1,LKZ2,NCELLS
INTEGER NJY1V(3),NJY2V(3),LKZ1V(3),LKZ2V(3)
INTEGER NELM
INTEGER IGS0,L_AB,I_B
integer itr_gas_tmp !saves initial itr_gas until initsv and setup can be moved out of main loop
integer itr_gend !# of last gas iteration in a global iteration

integer imx,jmx,kmx !index set of point with maximum residual
integer cycle_count ! number of current cycle
integer :: do_rad_qe_only_cnt=0 !counter for determining whether to do emission only radiation calculation
integer :: extra_iters=10 ! number of extra combustion iterations after the radiation calculation ends
integer :: track_enth_calls=0 !number of times enth_to_T routine is called
integer :: track_enth_flips=0 !number of times NIST constant set is flipped in enth_to_T routine

integer(2), ALLOCATABLE :: IBCELL(:,:,:),IWALL(:,:,:)
integer(2), ALLOCATABLE :: NTIMESM(:)

real(8)    kgas ! thermal conductivity of the gas 
real(8)    gmu_T ! viscosity of the gas (Pa-s = kg/m s)

real(8) FR_F,FR_OX,FR_N2,FR_P,FR_Q,SRATIO
real(8) U_MN,DN_MX
real(8) YFOP,H0_L
real(8) CPMIX(5)
real(8) wmix_ms !mixture molecular weight including minor species
real(8) ALAMDA
real(8) GLC1,GCP1
real(8) fuel_out !mass flow rate of fuel through exits (kg/s)
real(8) h_fuel_out !energy rate of unreacted fuel through exits (W)
real(8) Tmean_ex !mean exit temperature
real(8) soot_tot !total soot in system (kg)
!real(8) Q_IN,Q_LS,QLS_S,Q_F,Q_EM
real(8) Q_IN,Q_LS,Q_F,Q_EM

real(8) P_REF
real(8) CKA1,CKA2,CKA3,CKB1,CKB2,CKK1,CKK2,CKR
real(8) CKBET1
real(8) surf_length_c !length of combustion space (used by melt space when cycling)
real(8) surf_width_c  !width  of combustion space (used by melt space when cycling)
real(8) SGM
real(8) RSTIME,YD4(4)
real(8) UL0,TK0,EPSI0,THETA0,DNST0,UG0
real(8) FAR,DND0,THET0C
real(8) XLE,X0,RLE,WIDTH
real(8) GFIN,GFEX,FACTOR,FADJ,GFIN0
real(8) TPIN,TOUT,TPOUT,TFDOUT,TFLOUT,GT0,POUTW,SFHW,TSRS
real(8) TKNOM 
real(8) REYG,REYG2,GCN,GDN,GVN,EUN,ECN,DAMKO,RFG,EPSR,TAUR
real(8) REYD,SCN,Z0,DNUL0,DNUC0,TRN,DMFR,PMFR
real(8) TMAX
real(8) TM,DTM,QR0
real(8) Esf_R ! soot formation activation energy / universal gas constant
real(8) Eso_R ! soot oxidation activation energy / universal gas constant
real(8) P_MASS,P_MU0,P_DND0,SP0,RFG_P
real(8) RESG(20,2),RES(20,2)
!real(8) HNORM,QSURF,BFRACT,RADLOSS
real(8) HNORM,QSURF
real(8) T_AV,T_X,SVF_R
real(8) smf_max !maximum value of soot mass fraction
real(8) smf_max_noclip !maximum value of non-clipped soot mass fraction
integer smf_clipped_cnt !number of cells with clipped soot mass fraction
REAL(8) bal_ms !mean relative mass balance for minor species between iterations volume weithted
real(8) change_ms !mean relative change for all minor species between iterations volume weighted
real(8) vol_tot !volume total of combustion space (m**3)
real(8) S_O2,S_H2O,S_CO2,S_N2,S_NO,S_CO
real(8) QES0,QEG0,QEW0,QA_S0,QA_S1
real(8) QEG_G,QEG_W,QEG_S,QEG_IO
real(8) QEW_G,QEW_W,QEW_S,QEW_IO,QLS_G,QLS_A
real(8) YO2_out_est !estimated O2 outlet mass fraction
real(8) YCO2_out_est
real(8) YN2_out_est
real(8) YH2O_out_est
real(8) res_mass !normalized mean mass residual 
real(8) res_mass_max !max normalized mass residual
real(8) h_resid_pre  !mean enthalpy equation residual normalized, before enthalpy solve
real(8) h_resid  !mean enthalpy equation residual normalized
real(8) p_resid  !mean pressure equation residual normalized
real(8) p_resid_pre  !mean pressure equation residual normalized, before solve
real(8) pcorr_resid  !mean pressure correction equation residual normalized
real(8) u_resid(3)  !mean momentum equation residuals normalized
real(8) u_resid_pre(3)  !mean momentum equation residuals normalized, before solve
real(8) uc_mean(3)  !mean velocity corrections normalized
real(8) gf_resid(10)  !species and k-epsilon model equation residuals normalized
real(8) gf_resid_pre(10)  !species and k-epsilon model equation residuals normalized, before solve
real(8) Tave     !mass   mean temperature
real(8) Tave_vol !volume mean temperature
real(8) Tmax_c  !maximum T in combustion space (limit T bound in melt space)
real(8) :: q_wall_loss=0.0d0  !loss calc. from interior surf T to ambiant room T (W)
real(8) :: q_melt=0.0d0 !net radiation to melt surface (W)
real(8) :: qm_flame=0.0d0 !net rad into melt directly from flame (media) (W)
real(8) :: qm_bnds=0.0d0  !net rad into melt from boundary surfaces (W)
real(8) :: qm_conv=0.0d0  !net heat into melt from convection at surface (W)
real(8) :: q_rad_inlet=0.0d0 !net radiation leaving thru inlets (W)
real(8) :: q_rad_outlet=0.0d0 !net radiation leaving thru outlets (W)
real(8) :: qa_melt=0.0d0   !radiation incident to melt surface (W)
real(8) :: qe_melt=0.0d0   !radiation emitted from melt surface (W)
real(8) :: q_react=0.0d0      !energy added by reaction (W)
real(8) :: q_exhaust=0.0d0    !energy out exhausts (W)
real(8) q_h_in       !energy in via enthalpy at inlets   (W)
real(8) q_e_a_tot    !total of radiation emission and absorption over all cells & surfaces (W) (should be zero)
real(8) q_rad_vol_tot !net radiation out of combustion space vol, summed over cells (W)
real(8) q_global_bal !(abs((q_in_tot-q_out_tot)/q_in_tot))
real(8) area_melt_surf !area of melt surface (m^2) 
real(8) qa_sum 
real(8) qa_sum_point
real(8) qe_sum 
real(8) qe_point
real(8) qe_wall
real(8) qa_wall
real(8) qe_wall_sum
real(8) qa_wall_sum
real(8) avg_wall_T  !average combustion space wall temperature

real(8) :: qe_sum_from_open=0.0d0
real(8) :: qe_sum_from_melt=0.0d0
real(8) :: qa_sum_to_wall=0.0d0
real(8) :: qa_sum_to_melt=0.0d0 
real(8) :: qa_sum_to_open=0.0d0 
real(8) :: qa_sum_to_inlet=0.0d0
real(8) :: qa_sum_to_exit=0.0d0 
real(8) qa_sum_to_wall_cell
real(8) qa_sum_to_melt_cell
real(8) qa_sum_to_open_cell  
real(8) qa_sum_to_inlet_cell 
real(8) qa_sum_to_exit_cell  
real(8) tot_qe_h2o_co2  !Total emission from h2o and co2
real(8) tot_qe_soot     !Total emission from soot
real(8) radi_min != sig Tmn_K**4, minimum radiosity when multiplied by emissivity  

!----------------------------------------------------------------------
!     Wall to wall radiation variables 
!----------------------------------------------------------------------

!boundary patch arrays
integer(2),allocatable :: mapb(:,:,:)  !grid map of cell types for determining boundary patches
integer(2),allocatable :: b_typ(:)  ! boundary type = ibcell value of patches
integer(2),allocatable :: b_i(:)  ! i index of boundary patches
integer(2),allocatable :: b_j(:)  ! j index of boundary patches
integer(2),allocatable :: b_k(:)  ! k index of boundary patches
integer(2),allocatable :: b_orient(:) ! boundary orientation (x,y,z)
integer(2),allocatable :: b_direct(:) ! boundary direction (+,-)
integer(2),allocatable :: bc_i(:)  ! i index of cell center for a patch
integer(2),allocatable :: bc_j(:)  ! j index of cell center for a patch
integer(2),allocatable :: bc_k(:)  ! k index of cell center for a patch
integer(2),allocatable :: sameCell1(:)   ! patch index 1 for same cell
integer(2),allocatable :: sameCell2(:)   ! patch index 2 for same cell
integer(2),allocatable :: sameCell3(:)   ! patch index 3 for same cell
integer(2),allocatable :: sameCell4(:)   ! patch index 4 for same cell
integer(2),allocatable :: b_exh(:)   ! exhaust number
real(8),allocatable :: b_face_area(:) ! area of all patches on same cell
real(8),allocatable :: patch_area(:) ! area of patches 
real(8),allocatable :: vf(:,:)       ! view factor vf(i,j) = fraction of energy 
                                    !      from boundary patch i reaching boundary patch j
real(8),allocatable :: vf_sum(:)    ! for patch i, sum over j of vf(i,j)
real(8),allocatable :: radiosity(:)  ! radiosity of boundary patch 
real(8),allocatable :: emis(:)       ! emissivity of boundary patch 
real(8),allocatable :: qc(:)         ! heat flux from the volume due to combustion
real(8),allocatable :: qeb(:)        ! heat flux from known temperature boundary
real(8),allocatable :: q_amb(:)      ! heat flux out through walls
real(8),allocatable :: q_conv(:)     ! convection heat flux from gas flow to boundaries
real(8),allocatable :: b_T(:)        ! Temperature of boundaries
real(8),allocatable :: wg(:,:,:)     ! wall grid

real(8) wall_radiosity_tolerance !tolerance for change of radiosity
real(8) radiation_boundary_tolerance !tolerance for change of radiation
real(8) rad_from_bnds !radiation from boundaries

real(8) sum_rad_vf ! sum of radiosities times view factors
real(8) prev_sum_rad_vf ! previous sum of radiosities times view factors
real(8) eps_m ! emissivity of melt surface
real(8) eps_c !ceiling or crown emissivity
real(8) height_to_ceiling !z value at ceiling or start of crown
real(8) wall_area_total !total area of wall  patches
real(8) melt_area_total !total area of melt patches
real(8) inlet_area_total !total area of inlet patches
real(8) exit_area_total !total area of exit patches
real(8) boundary_area_total !total area of boundary patches
real(8) q_incident_total !total energy incident on boundaries from the volume
real(8) q_incident_wall  !energy incident on walls from the volume
real(8) q_incident_inlet !energy incident on inlets from the volume
real(8) q_incident_exit  !energy incident on exits from the volume
real(8) q_incident_melt  !energy incident on melt surface from the volume
real(8) qconv_total !total convection from gas to boundaries 
real(8) qamb_avg !mean wall heat loss to ambient
real(8) qc_avg !mean heat flux from volume
real(8) qrs_avg !mean heat flux to the melt
real(8) qconv_avg !mean convection heat flux from gas flow to boundaries

integer have_crown  !1=> have a crown
integer iwall_count !number of interior wall patches
integer imelt_count !number of melt surface patches
integer inlet_count !number of inlet patches
integer iexit_count !number of exit patches
integer b_cnt  ! total number of boundary patches
integer iterw !wall radiation iteration counter
integer maxri1 !maximum radiosity solver outer loop 1 iteration number
integer maxri2 !maximum radiosity solver inner loop 2 iteration number
integer minri2 !minimum radiosity solver inner loop 2 cycles
integer preset_vf !1=>view factors are all calculated once
                  !0=>view factors must be created each time they are needed
!----------------------------------------------------------------------
!     End wall to wall radiation variables 
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!     Exhaust variables 
!----------------------------------------------------------------------
integer nex !number of exhausts
integer have_frac_exh_T ! 1=have an exhaust wall T that requires fractional part of average wall temperature
integer(2),allocatable :: exh_orient(:) ! exhaust orientaion: x-normal=1, y-normal=2, z_normal=3
integer(2),allocatable :: exh_direct(:) ! exhaust direction : positive=0, negative=1
integer(2),allocatable :: exh_type(:) ! exhaust initial wall temperature for radiation type:
                                      ! 0=>use fraction of average wall temperature
									  ! 1=>use fixed temperature
real(8),allocatable :: exh_frac(:) ! fraction of average wall temperature
real(8),allocatable :: exh_fixed(:) ! fixed temperature
integer,allocatable :: exh_i1(:) ! exhaust starting cell center x index
integer,allocatable :: exh_j1(:) ! exhaust starting cell center y index
integer,allocatable :: exh_k1(:) ! exhaust starting cell center z index
integer,allocatable :: exh_i2(:) ! exhaust ending cell center x index
integer,allocatable :: exh_j2(:) ! exhaust ending cell center y index
integer,allocatable :: exh_k2(:) ! exhaust ending cell center z index
!----------------------------------------------------------------------
!     End exhaust variables 
!----------------------------------------------------------------------


real(8),allocatable :: buf(:,:,:)  ! general buffer 1 
real(8),allocatable :: buf2(:,:,:) ! general buffer 2 
real(8),allocatable :: Tgs(:,:)    ! melt surface temperature 
real(8),allocatable :: T_surf_prev(:,:) ! previous melt surface temperature (last cycle or run) 
real(8), ALLOCATABLE :: X(:),DX(:),R(:),DR(:),RS(:),Z(:),DZ(:)
real(8), ALLOCATABLE :: P(:,:,:),DNST(:,:,:),THETA(:,:,:) 
real(8), ALLOCATABLE :: UG(:,:,:,:),GF(:,:,:,:) 
real(8), ALLOCATABLE :: T(:,:,:),CRATE(:,:,:),HRATE(:,:,:)
real(8), ALLOCATABLE :: PO(:,:,:),DNSTO(:,:,:),THETAO(:,:,:) 
real(8), ALLOCATABLE :: UGO(:,:,:,:),GFO(:,:,:,:) 
real(8), ALLOCATABLE :: TGO(:,:,:),APO(:,:,:)
real(8), ALLOCATABLE :: TMU(:,:,:),GCP(:,:,:),GLAM(:,:,:)
real(8), ALLOCATABLE :: GAMA(:,:,:),GDIFF(:,:,:),GTE(:,:,:)
real(8), ALLOCATABLE :: FZ(:,:,:),BS(:,:,:),SFP(:,:,:) 
real(8), allocatable :: bs_hprev(:,:,:)  ! holds previous enthalpy source for use with relaxation 
real(8), allocatable :: bs_soot_prev(:,:,:)  ! holds previous soot formation source for use with relaxation 
real(8), allocatable :: sfp_soot_prev(:,:,:)  ! holds previous soot oxidation source for use with relaxation 
real(8), ALLOCATABLE :: AP(:,:,:),AP1(:,:,:),AS(:,:,:,:,:)
real(8), ALLOCATABLE :: FLX(:,:),FLXH(:,:),FLX_H(:,:)
real(8), ALLOCATABLE :: FLY(:,:),FLYH(:,:),FLYM(:,:,:)
real(8), ALLOCATABLE :: FLUX_Z(:,:),FLUXH_Z(:,:),FLUXM_Z(:,:,:)
!real(8), ALLOCATABLE :: QRN(:,:,:)
real(8), ALLOCATABLE :: QA(:,:,:),QE(:,:,:),TW(:,:,:),QRS(:,:)
real(8), ALLOCATABLE :: QE2(:,:,:)
real(8), ALLOCATABLE :: RTIM(:),DELRT(:),HRT(:)
real(8), ALLOCATABLE :: DELR(:),TIMED(:),HRD(:)
!real(8), ALLOCATABLE :: FR_FI(:)

real(8), ALLOCATABLE :: GFM(:,:,:,:),FLXM(:,:,:)
! GFM array minor and radiating species indexes:
!         1=Fuel = CH4
!         2=O2
!         3=CO
!         4=H2O
!         5=CO2
!         6=N2
!         7=NO

real(8), ALLOCATABLE :: smf(:,:,:)
real(8), ALLOCATABLE :: RFM(:),RFMC(:),RRMS(:,:,:,:)
real(8), allocatable :: resid_ms(:) !mean residuals for subspecies equation solves
real(8), allocatable :: resid_ms_pre(:) !mean residuals for subspecies equation solves, before solves

real(8), ALLOCATABLE :: RD(:),RD2(:),RD3(:),DRD(:),PSI(:),tb(:)
real(8), ALLOCATABLE :: DN(:,:,:,:),DT(:,:,:,:),DC(:,:,:,:)
real(8), ALLOCATABLE :: DU(:,:,:,:,:),DUO(:,:,:,:,:)
real(8), ALLOCATABLE :: DNO(:,:,:,:),DTO(:,:,:,:),DCO(:,:,:,:)
real(8), ALLOCATABLE :: TH_DP(:,:,:),TH_PT(:,:,:),SP(:,:,:)
real(8), ALLOCATABLE :: EVP(:,:,:),CON(:,:,:)
real(8), ALLOCATABLE :: W1_K(:),W1_HA(:),W1_TA(:),W1_D(:)

integer(2),allocatable :: ixff(:,:,:) !grid map for fieldview

INTEGER NRADWALL
!integer MAXRI !obsolete
!integer lrx !gas iteration count, in itr_gas, for next radiation computation
integer next_rad !gas iteration # for next radiation computation
integer itr_rad ! number of radiation iterations completed
INTEGER NWL,MP3(3),ID007
real(8) SGN,RI
real(8) T_RAD,TT0,TG_MN,QEL_MN
real(8) YCO2,WNCO2(6,3),AACO2(6)
real(8) YH2O,WNH2O(5,3),AAH2O(5)
real(8) WNG(11,2),FAC_W,FVS,OPL
real(8) W_R,PE,P_RAD,ANG0(3)
real(8) T_A,H_A,H_G,W_K,W_D,EPS_W
LOGICAL, ALLOCATABLE :: IQT1(:,:),IQTM(:,:),IQT2(:,:)
real(8), ALLOCATABLE :: WL(:),AKL(:,:,:,:),E0(:)
real(8), ALLOCATABLE :: AKLG(:),AKLS(:),AKL0(:)
real(8), ALLOCATABLE :: DEB(:),DEB0(:),FCO2(:,:,:)
real(8), ALLOCATABLE :: PG(:,:,:),FH2O(:,:,:) 
real(8), ALLOCATABLE :: QEL(:,:,:,:),QAL(:,:,:,:)
!real(8), ALLOCATABLE :: QA1(:,:,:)
!real(8), ALLOCATABLE :: QALW(:,:,:,:)
real(8), ALLOCATABLE :: QALW0(:,:,:,:)
real(8), ALLOCATABLE :: QEL0(:),QEL1(:),QL(:,:,:)
real(8), ALLOCATABLE :: QTX1(:,:,:),QTX2(:,:,:),Q2M(:,:,:)
real(8), ALLOCATABLE :: ANG1(:,:),ANG2(:,:),ANG3(:,:)
real(8), allocatable :: wall_rad_T(:,:,:)

END MODULE GBL_VAR  


