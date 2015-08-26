!==============================================================================
!==============================================================================
!==============================================================================
!
! Setup.f90
!
! Setup initializes conditions and parameters,
!       sets control variables, and
!       calculates dimensionless parameters.
!
! This module contains the following routines:
!     initial_setup
!     setup
!     allocate_large_gas_phase_arrays
!     allocate_ms
!
!==============================================================================
!==============================================================================
!==============================================================================
! 
! initial_setup sets variables that are (or may be) reset after the sbc namelist has been read, and
!               sets constants that are not preset in var_mod.
!
!     Note that variables marked with "{sbc}" are in the sbc namelist.
!
!==============================================================================
subroutine initial_setup
use gbl_var
implicit double precision (a-h,o-z)

!common array write formats
FMZ="(/' Z(',I3,') = ',G12.3)"
FMR='(/2X,''  X  /  Y'',11(G15.7))'
FMX='(G15.7,11E15.7)'

! Turbulent constant
c_htw = 0.4d0*((pi/4)/sin(pi/4))*sqrt(26.0d0)*(0.7d0-one)/0.7d0**0.25d0 !for energy wall function


!-----------------------------------------------
! Set constants used in the enthalpy <=> temperature calculations, using information
! from the National Institute of Standards and Technology (NIST).
! (Variables defined in var_mod commented here for additional information)

!integer :: nmax=1000 !maximum number of Newton iterations for determining temperature from enthalpy
!real(8) :: tolerance_enth=1.0d-11 !minimum change limit for temperature based on enthalpy calculation
!integer :: lo_hi(5) !index determining whether low or high temperature for NIST constants are used
!real(8) :: real_T !actual temperature in K, not normalized
!real(8) :: h_s !sensible enthalpy
!real(8) :: h_molar !molar enthalpy
!real(8) :: T_old !old temperature
!real(8) :: T_new !new temperature
!real(8) :: changeT !change in temperature
!real(8) :: enth_func !function used when determining temperature based on enthalpy
!real(8) :: enth_func_prime !derivative of the enth_func
!real(8) :: w_mix !molecular weight of the mixture
!real(8) :: a_mix !sum of the chemical mass fractions times their A coefficients
!real(8) :: b_mix !sum of the chemical mass fractions times their B coefficients
!real(8) :: c_mix !sum of the chemical mass fractions times their C coefficients
!real(8) :: d_mix !sum of the chemical mass fractions times their D coefficients
!real(8) :: e_mix !sum of the chemical mass fractions times their E coefficients
!real(8) :: f_mix !sum of the chemical mass fractions times their F coefficients
!real(8) :: h_mix !sum of the chemical mass fractions times their H coefficients

!integer :: iyf=1    ! equation index for fuel mass fraction
!integer :: iyo2=2   ! equation index for oxygen
!integer :: iyco2=6  ! equation index for carbon dioxide
!integer :: iyn2=7   ! equation index for nitrogen
!integer :: iyh2o=9  ! equation index for steam

!real(8) :: wf=16.0426d+0     ! fuel (CH4) molecular weight (kg/kmol) 
!real(8) :: wo2=31.9988d+0    ! oxygen molecular weight (kg/kmol)
!real(8) :: wco2=44.0098d+0   ! carbon dioxide molecular weight (kg/kmol)
!real(8) :: wn2=28.0134d+0    ! nitrogen molecular weight (kg/kmol)
!real(8) :: wh2o=18.0152d+0   ! steam molecular weight (kg/kmol)

!real(8) :: cutT(5) = & !cut off temperature to determine NIST lo/hi coefficient set
!            (/1300.0d0, &   ! fuel lo/hi cut off temperature to determine coefficients
!              6000.0d0, &   ! oxygen lo/hi cut off temperature to determine coefficients
!              1200.0d0, &   ! carbon dioxide lo/hi cut off temperature to determine coefficients
!              6000.0d0, &   ! nitrogen lo/hi cut off temperature to determine coefficients
!              1700.0d0/)    ! steam lo/hi cut off temperature to determine coefficients

!integer :: gf_chem_index(5) !translates to the fourth index of array gf(:,:,:,:) for the chemicals 
   gf_chem_index(1) = iyf
   gf_chem_index(2) = iyo2
   gf_chem_index(3) = iyco2
   gf_chem_index(4) = iyn2
   gf_chem_index(5) = iyh2o

!NIST coefficient arrays: 
!  1st index for chemical: CH4 (fuel), O2, CO2, N2, H2O
!  2nd index for 1=lo range, 2=hi range
!real(8) :: a_nist(5,2)
!real(8) :: b_nist(5,2)
!real(8) :: c_nist(5,2)
!real(8) :: d_nist(5,2)
!real(8) :: e_nist(5,2)
!real(8) :: f_nist(5,2)
!real(8) :: h_nist(5,2)

!NIST coefficients for methane 
   a_nist(1,1)=-0.703029d0;  a_nist(1,2)=85.81217d0
   b_nist(1,1)=108.4773d0;   b_nist(1,2)=11.26467d0
   c_nist(1,1)=-42.52157d0;  c_nist(1,2)=-2.114146d0
   d_nist(1,1)=5.862788d0;   d_nist(1,2)=0.13819d0
   e_nist(1,1)=0.678565d0;   e_nist(1,2)=-26.42221d0
   f_nist(1,1)=-76.84376d0;  f_nist(1,2)=-153.5327d0
   h_nist(1,1)=-74.8731d0;   h_nist(1,2)=-74.8731d0
!NIST coefficients for oxygen (note high range set = low range)
   a_nist(2,1)=29.659d0;     a_nist(2,2)=29.659d0   
   b_nist(2,1)=6.137261d0;   b_nist(2,2)=6.137261d0 
   c_nist(2,1)=-1.186521d0;  c_nist(2,2)=-1.186521d0
   d_nist(2,1)=0.09578d0;    d_nist(2,2)=0.09578d0  
   e_nist(2,1)=-0.219663d0;  e_nist(2,2)=-0.219663d0
   f_nist(2,1)=-9.861391d0;  f_nist(2,2)=-9.861391d0
   h_nist(2,1)=0.0d0;       h_nist(2,2)=0.0d0     
!NIST coefficients for carbon dioxide 
   a_nist(3,1)=24.99735d0;   a_nist(3,2)=58.16639d0
   b_nist(3,1)=55.18696d0;   b_nist(3,2)=2.720074d0
   c_nist(3,1)=-33.69137d0;  c_nist(3,2)=-0.492289d0
   d_nist(3,1)=7.948387d0;   d_nist(3,2)=0.038844d0
   e_nist(3,1)=-0.136638d0;  e_nist(3,2)=-6.447293d0
   f_nist(3,1)=-403.6075d0;  f_nist(3,2)=-425.9186d0
   h_nist(3,1)=-393.5224d0;  h_nist(3,2)=-393.5224d0
!NIST coefficients for nitrogen (note high range set = low range)
   a_nist(4,1)=26.092d0;     a_nist(4,2)=26.092d0   
   b_nist(4,1)=8.218801d0;   b_nist(4,2)=8.218801d0 
   c_nist(4,1)=-1.976141d0;  c_nist(4,2)=-1.976141d0
   d_nist(4,1)=0.159274d0;   d_nist(4,2)=0.159274d0 
   e_nist(4,1)=0.044434d0;   e_nist(4,2)=0.044434d0 
   f_nist(4,1)=-7.98923d0;   f_nist(4,2)=-7.98923d0 
   h_nist(4,1)=0.0d0;        h_nist(4,2)=0.0d0      
!NIST coefficients for steam 
   a_nist(5,1)=30.092d0;     a_nist(5,2)=41.96426d0
   b_nist(5,1)=6.832514d0;   b_nist(5,2)=8.622053d0
   c_nist(5,1)=6.793435d0;   c_nist(5,2)=-1.49978d0
   d_nist(5,1)=-2.53448d0;   d_nist(5,2)=0.098119d0
   e_nist(5,1)=0.082139d0;   e_nist(5,2)=-11.15764d0
   f_nist(5,1)=-250.881d0;   f_nist(5,2)=-272.1797d0
   h_nist(5,1)=-241.8264d0;  h_nist(5,2)=-241.8264d0

! End set constants used in the enthalpy <=> temperature calculations
!-----------------------------------------------





!----------------------------------------------------------------------
! Items that are constant for a run but are modified after namelist has been read.
! So these items need to be reinit as long as the main loop includes setup thru initsv.
!----------------------------------------------------------------------

GRA=zero ! gravitational acceleration (m/s**2)
GRA(3)=-9.80621D+0 
PG0=1.01325D+5 ! {sbc} pressure (pa) 

! Convergence criteria, iteration limits, tolerances
BGCON=1.0D-8 ! {sbc} acceptable mass residual for a gas phase calculation
BMCON=1.0D-7 ! {sbc} acceptable mass residual for a minor species calculation
MAXGI=2000 ! {sbc} maximum allowable gas phase iterations
MAXSI=1000 ! {sbc} maximum allowable global iterations
MAXMS=300 ! {sbc} maximum allowable sub-species iterations
U_MX=1.0D+3 ! velocity maximum

! Program flow type parameters
NU=6 !I/O unit number for SPRINT output. Note that the value of NU may change
IRSTYP=0 ! {sbc} Fresh start with no backup file, 1=>Restart by using data from the backup file
IRSTYPM=0 ! {sbc} Fresh start for minor species
REACT=.TRUE. ! {sbc} true if reaction takes place
MS=0 ! {sbc} 0=> don't do sub-species calculation, otherwise do it
interval_rad=100 ! {sbc} # of gas iterations between radiation computations
q_melt=0.0d0 !net radiation to melt surface (W)

itr_ms=0 !minor species iteration count
cycling=0 ! {sbc} not doing automatic domain cycling
oxy_fuel=0 ! {sbc} not using oxy fuel for soot option
initial_gitr=100 ! {sbc} !# of gas iterations to do in 1st global iteration
                 ! before the ms and radiation calculations begin
T_init=300.0d+0 ! {sbc} initial quess for temperature field (normalized)
esf=5.0d+7      ! {sbc}  activation energy for soot formation J/kmol
aform=400.0d+0  ! {sbc}  kinetic constant soot formation, pre-exponential const. in soot formation source term
eso=5.0d+7      ! {sbc}  activation energy for soot oxidation J/kmol
aoxid=26.0d+0   ! {sbc} kinetic constant soot oxidation,  pre-exponential const. in soot sink source term
isoot_cal=0  ! {sbc}  soot kinetics calibration mode
!q_melt_req=0.74694352735085E+06 ! {sbc} energy required for melt (W)
!q_melt_req=0.74694352735085E+06 ! (W) TC21 case0020
!q_melt_req=0.27667460382403E+07 ! (W) Pittston B case1703
gui_update=1 ! {sbc} 1=> CFD will provide status updates to the gui via a gfm.dat file 

! Relaxation factors (and compliment factors) for certain variables
!     1: U(P')              13: DN          18: DNST
!     2: V(P')              14: DU          19: EVP (EVAP RATE)
!     3: W(P')              15: DV          20: P (SIMPLE ONLY) 
!     4: DROP-GAS HEAT TR   16: DW          21: CRATE
!     5: PART-GAS HEAT TR   17: DT          
!rf=0.7D+0 ! {sbc} relaxation factors for certain variables
!rf(1:3)=0.3D+0 ! {sbc} 
!rf(4:5)=ONE ! {sbc} 
!rf(13:16)=0.3D+0 ! {sbc}  
!rf(17:21)=0.6D+0 ! {sbc} 
!rfc=one-rf

! Combustion parameters
Q0=2.42D+6 ! {sbc} heat of combustion (J/kg-fuel), but should be set to natural gas    
ELH=0.3489D+6 ! latent heat (J/kg) (oil cracking)

! Properties of gas species
GMFR=1.0d0 ! {sbc} total incoming mass flow rate (kg/s)
GDF0=1.6D-5 ! gas phase mass diffusion coefficient (?)
GCP0=1.017D+3 ! not used, gas phase specific heat (J/kg)
GMU_ref=2.6D-5 ! {sbc} gas phase molecular viscosity
radi_min = sig*T_mnK**4 ! minimum radiosity when multiplied by emissivity  

return
end

     
!==============================================================================
!==============================================================================
!==============================================================================
SUBROUTINE SETUP
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

NAMELIST /INPUT/ &
   ID_RAD,& !-1 => do not do radiation, >0 => do radiation
   PG0,& !pressure (pa) default: 1.01325d+5
   GMFR,& !total incoming mass flow rate (kg/s) default: one
   !DMFR0,& !default: zero
   IRSTYP,& !restart type, default: 0=> fresh start, 1=> restart
   IRSTYPM,& !restart type for minor species, default: 0=> fresh start, 1=> restart
   !IDEBUG,& !default: 0=>don't do certain debug prints
   REACT,& !default: .TRUE. if reaction takes place
   !GRA,& !gravitational acceleration (m/s**2)
   !RF,& !relaxation factors for certain variables - moved to relaxation file
   BGCON,& !acceptable mass residual for a gas phase calculation
   !BDCON,& !acceptable mass residual for a droplet phase calculation
         !Use commented out
   !BECON,&
         !Not used
   !BPCON,& !acceptable mass residual for a particle phase calculation
         !Use commented out
   MAXGI,& !maximum allowable gas phase iterations
   MAXSI,& !maximum allowable global iterations
   !ELH,& !latent heat (J/kg) (oil cracking)
   Q0,& !heat of combustion (J/kg-fuel)
   !IYF,& !equation index for fuel mass fraction
   !iYCO2,& !equation index for carbon dioxide
   !IYH2O,& !equation index for steam
   !IYO2,& !equation index for oxygen
   !IYN2,& !equation index for nitrogen
   !IH,& !equation index for enthalpy
   !IK,& !equation index for turbulent kinetic energy
   !IEPS,& !equation index for turbulent dissipation rate
   !LSTAR,& !starting number of species calculation             
   !LEND,& !end number of species calculation
   !NPHAS,& !number of phases
   !CYL,& !.TRUE. for cylindrical coordinates
   MS,& !0=> don't do sub-species calculation, otherwise do it
   !LSTARM,& !start index for sub-species calculation              
   !LENDM,& !end index for sub-species calculation
   !T_MX,& !a temperature maximum bound, will be normalized later
   !T_MN,& !a temperature minimum bound, will be normalized later
   MAXMS,& !maximum allowable sub-species iterations
   !WF,& !fuel molecular weight (kg/kmol)
   !CPF,& !Specific heat coefficient for fuel
   !WCO2,& !carbon dioxide molecular weight (kg/kmol)
   !CPCO2,& !Specific heat coefficient for carbon dioxide
   !WH2O,& !steam molecular weight (kg/kmol)
   !CPH2O,& !Specific heat coefficient for steam
   !WO2,& !oxygen molecular weight (kg/kmol)
   !CPO2,& !Specific heat coefficient for oxygen
   !WN2,& !nitrogen molecular weight (kg/kmol)
   !CPN2,& !Specific heat coefficient for nitrogen
   !GDF0,& !gas phase mass diffusion coefficient (?), modified later
   !GL0,& !thermal conductivity of fuel vapor (W/m K)
   !GCP0,& !gas phase specific heat (J/kg)
   GMU_ref,& !gas phase molecular viscosity, modified later
   !U_MX,& !velocity maximum, modified later
   BMCON,& !acceptable mass residual for a minor species calculation
   !ISTOICH,&
         !Use commented out
   !RADGRAY,& !modifies ek0
   !EK0,& !not explained, modified later
   !BFRACT,&
         !Use commented out    
   !RADLOSS,&
         !Use commented out
   interval_rad,& !# of gas iterations between minor species or radiation computations default: 100            
   T_init, &   ! initial guess for temperature field
   cycling, &  ! 1=> doing domain cycling, else 0
   oxy_fuel, & ! 1=> using oxy fuel for soot option, else 0
   initial_gitr, & ! number of initial combustion iterations at startup 
                   ! before the radiation interval begins
   esf, &    ! activation energy for soot formation J/kmol
   aform, &  ! kinetic constant soot formation
   eso, &    ! activation energy for soot oxidation J/kmol
   aoxid, &  ! kinetic constant soot oxidation
   isoot_cal, &  ! soot kinetics calibration mode
   !q_melt_req,& !energy required for melt
   available_parameter,& ! not used, possible future use as a parameter passed in the Sbc file   
   gui_update,& !1=> CFD will provide status updates to the gui via a gfm.dat file 
   !Combustion data collection control flags may be set to 0 (off) or 1 (on, default)
   isum,& !Summary data collection control flag"
   iinfo,& !General Information data collection control flag"
   iTave,& !Average Temperatures data collection control flag"
   iconv,& !Mass Residual Convergence data collection control flag"
   igresid,& !Equation Residuals data collection control flag"
   igresidp,& !Pre-Solve Equation Residuals data collection control flag"
   igresidx,& !Extra Equation Residuals data collection control flag"
   igresidxp,& !Extra Pre-Solve Equation Residuals data collection control flag"
   imresid,& !Minor Species Residual Convergence data collection control flag"
   irad_detail,& !Radiation Details data collection control flag"
   irad_rad,& !Radiosity Convergence data collection control flag"
   itwal,& !Wall Temperature data collection control flag"
   irelax,& !Temperature Relaxation data collection control flag"
   iflx, & !Melt Surface Flux Change data collection control flag"
   ifieldview !Fieldview post processor file output control flag
!end of input namelist


call initial_setup !Set constants not set in var_mod and reinitialize variables 
                   !that may be modified after the sbc namelist has been read.


!======================================================================
!======================================================================
!     Read SBC INPUT namelist
!======================================================================

filename=casedir//'\sbc'//runum//'c.dat'
OPEN (nu_sbc,FILE=FILENAME)
READ (nu_sbc,NML=INPUT) !SBC Namelist variables read here -----------

! This code ensures that on an initial startup at least 1 iteration is done
! and if we are doing radiation that at least interval_rad + extra iterations are done
! in the gas phase, extra iterations are done after radiation

if (interval_rad==0) extra_iters=0
if (initial_gitr==0 .and. irstyp==0) then
   !doing new start, must have initial_gitr set, default to interval value
   !(initial_gitr is only used on a new start.)
   if (interval_rad == 0) then
      initial_gitr=maxgi   
   else
      initial_gitr=max(interval_rad,1)
   endif
elseif (initial_gitr > 0 .and. irstyp==1) then
   initial_gitr=0 !just set to indicate that combustion space has already been developed 
endif

maxgi=max(maxgi,1) !do at least one iteration

if (itr_gas==0) then ! we are just starting a run (may be new start or restart)
   maxgi=max(maxgi,initial_gitr+extra_iters,interval_rad+extra_iters) ! do extra gas iterations after the radiation calc
   maxgi_save=maxgi
else !if we are coming back here after one or more radiation calculations reset maxgi
     !to the value it got on start up, don't leave at the clobbered value just read from the namelist
   maxgi=maxgi_save
endif
     
IF (itr_gas.GT.0) THEN
   IRSTYP=1
   IRSTYPM=1
ENDIF 

IF (IDEBUG.EQ.2) THEN
   IF (itr_gas.LE.0) OPEN(20,FILE='DEBUG.OUT')
   IF (CYL) THEN
      WRITE(NU,*)' ****** CYLINDRICAL COORDINATE SYSTEM ******'
   ELSE
      WRITE(NU,*)' ****** RECTANGULAR COORDINATE SYSTEM ******'
   ENDIF
   WRITE(NU,*)' '
   IF (NPHAS .EQ. 1) THEN
      WRITE(NU,*)' ***** SINGLE PHASE SYSTEM *****'
   ELSE
      WRITE(NU,*)' ***** MULTIPLE PHASE SYSTEM *****'
   ENDIF
   WRITE(NU,*)' '
ENDIF

if (cycling==1) then
   !get additional cycling information from GUI
   filename=casedir//'\cycleInfo.txt'
   inquire (file=filename,exist=exst)
   if (exst) then
      open(nu_cycle,file=filename)
      read (nu_cycle,*) cycle_count
      close(nu_cycle)
      n=delfilesqq(filename) !delete file
   end if  
else
   cycle_count=0
endif

!----------------------------------------------------------------------
!if(oxy_fuel==1)then
!   aform=200.0d+0
!   aoxid=50.0d+0
!else
!   aform=1.0d+0 !Brian Golchert's numbers; phone call 8/31/05, he said this set is for air
!   aoxid=220.0d+0
!endif 
if (oxy_fuel==1)then
!   aform=1.0d+0
   T_MX=4.0D+3
else
!   aform=400.0d+0
   T_MX=3.0D+3
endif
T_MX=T_MX/T0
 
Esf_R=Esf/Ru 
Eso_R=Eso/Ru


!======================================================================
!======================================================================
!     Read grid and allocate main arrays
!======================================================================

if (itr_gas == 0) then !true on startup
   CALL GRID3 ! Obtain the furnace geometry parameters
              ! and define the grid system
   call allocate_large_gas_phase_arrays
   if (ms==1) call allocate_ms 
endif


!======================================================================
!======================================================================
!     Read relaxation factors file
!======================================================================

if (itr_gas == 0) then !true on startup
   !filename=casedir//'\relaxfactor'//runum//'c.dat'
   filename='relaxfactorc.txt'
   open (nu_rxfact,file=filename)

   read (nu_rxfact,*) title !Relaxation factors for equation loops
   read (nu_rxfact,*) general_rf !general relaxation value
   rfl=general_rf
   read (nu_rxfact,*) rfl(1) !equation for fuel mass fraction 
   read (nu_rxfact,*) rfl(2) !equation for oxygen
   read (nu_rxfact,*) rfl(3) !equation for enthalpy
   read (nu_rxfact,*) rfl(4) !equation for turbulent kinetic energy
   read (nu_rxfact,*) rfl(5) !equation for turbulent dissipation rate
   read (nu_rxfact,*) rfl(6) !equation for carbon dioxide
   read (nu_rxfact,*) rfl(7) !equation for nitrogen
   rflc=one-rfl !set complementary relaxation factors

   read (nu_rxfact,*) title !Relaxation factors for certain variables
   read (nu_rxfact,*) general_rf !general relaxation value
   rf=general_rf
   read (nu_rxfact,*) rf(1)  ! u momentum in x direction (pressure correction)
   read (nu_rxfact,*) rf(2)  ! v momentum in y direction (pressure correction)
   read (nu_rxfact,*) rf(3)  ! w momentum in z direction (pressure correction)
   !read (nu_rxfact,*) rf(4)  ! droplet-gas heat transfer
   !read (nu_rxfact,*) rf(5)  ! particle-gas heat transfer
   !read (nu_rxfact,*) rf(13) ! droplet or particle number density
   !read (nu_rxfact,*) rf(14) ! droplet or particle momentum in x direction
   !read (nu_rxfact,*) rf(15) ! droplet or particle momentum in y direction
   !read (nu_rxfact,*) rf(16) ! droplet or particle momentum in z direction
   !read (nu_rxfact,*) rf(17) ! droplet or particle temperature
   read (nu_rxfact,*) rf(18) ! density
   !read (nu_rxfact,*) rf(19) ! evaporation rate
   !read (nu_rxfact,*) rf(20) ! simple pressure
   read (nu_rxfact,*) rf(21) ! reaction rate
   rfc=one-rf  !set complementary relaxation factors

   read (nu_rxfact,*) rfTsurf ! initial relaxation factor for surface temperature 
   rfTsurf_c=one-rfTsurf !set complementary relaxation factors

   read (nu_rxfact,*) general_rf ! relaxation factor for minor species
   read (nu_rxfact,*) soot_rf ! relaxation factor for soot
   if (ms==1 .and. nsp0>0) then !minor species arrays are allocated
      rfm=general_rf
      rfm(nsp0+1)=soot_rf
      rfmc=one-rfm
   endif

   read (nu_rxfact,*) title !Soot related relaxation factors and bounds
   read (nu_rxfact,*) rf_hsource ! relaxation factor for enthalpy source term
   read (nu_rxfact,*) rf_soot_form_source ! relaxation factor for soot formation source term
   read (nu_rxfact,*) rf_soot_oxid_source ! relaxation factor for soot oxidation source term
   read (nu_rxfact,*) rf_aform !soot calibration relaxation factor for soot formation adjustment
   read (nu_rxfact,*) rf_afover !soot calibration relaxation factor for soot formation adjustment during overflow conditions
   read (nu_rxfact,*) soot_cap !maximum allowed value of soot mass fraction
   read (nu_rxfact,*) T0_sf !minumum temperature [K] at which soot formation may occur
   read (nu_rxfact,*) T0_so !minumum temperature [K] at which soot oxidation may occur

   read (nu_rxfact,*) title !Number of sweeps in equation solver for specified items
   read (nu_rxfact,*) general_ntimes !general value
   ntimesg=general_ntimes
   read (nu_rxfact,*) ntimesg(1)  !  u momentum in x direction
   read (nu_rxfact,*) ntimesg(2)  !  v momentum in y direction
   read (nu_rxfact,*) ntimesg(3)  !  w momentum in z direction
   read (nu_rxfact,*) ntimesg(4)  !  pressure
   read (nu_rxfact,*) ntimesg(5)  !  pressure correction
   read (nu_rxfact,*) ntimesg(6)  !  fuel
   read (nu_rxfact,*) ntimesg(7)  !  co2
   read (nu_rxfact,*) ntimesg(8)  !  enthalpy 
   read (nu_rxfact,*) ntimesg(9)  !  turbulent kinectic energy
   read (nu_rxfact,*) ntimesg(10) !  epsilon
   read (nu_rxfact,*) ntimesg(11) !  n2 
   read (nu_rxfact,*) ntimesg(12) !  o2 
   read (nu_rxfact,*) general_ntimes   !  minor species
   ntimesm=general_ntimes

   close (nu_rxfact)

endif

!======================================================================
!======================================================================

!----------------------------------------------------------------------
!     Inlet gas flow (0, at I=2)
!        YF:  fuel concentration (mass fraction)
!        YP1: CO2
!        YP2: steam
!        YOX: oxydizer 
!        YN2: nitrogen
!        WT:  gas molecular weight (kg/kmol)
!        PG:  pressure (Pa)
!        T:   gas temperature (K)
!        TL:  droplet temperature (K)
!        UG:  gas axial velocity (m/s)
!        UL:  droplet axial velocity (m/s)
!        T0: inlet reference temperature (K)
!              ** T0 should not be equal to TL0
!        RLM: volume mean droplet radius (m)
!        DND: droplet number density (drops/m**3)
!        DNST0: reference gas density (kg/m**3)
!        GMFR: total incoming mass flow rate (kg/s)
!        STOICH: the stoichiometric mass ratio of oxidizer to fuel
!        WF,WCO2,WH2O,WO2,WN2: species molecular weights (kg/kmol)
!----------------------------------------------------------------------
DNST0=PG0*WT0/RU/T0
UG0=GMFR/DNST0/AREA0
GRA=GRA/UG0/UG0*R0
!GLC1,GCP1 are pressure constants in gas thermal conductivity and specific heat correlations.
!The correlations are good for air from T=300 K to 3000 K and p = 1E5 Pa to 1E6 Pa.
GLC1=1.30*EXP(-((PG0-1.0D+5)/9.0D+5)**0.47D+0)
GCP1=1.21*EXP(-1.2D+0*((PG0-1.0D+5)/9.0D+5)**0.48D+0)
!      IF (ISTOICH.EQ.1) THEN
!        STOICH=STOXAIR*WAIR/WF
!      ELSE
!        STOICH=STOX1*WO2/WF !the stoichiometric mass ratio of oxidizer to fuel
!      ENDIF
IF (IDEBUG.EQ.2) THEN
   WRITE(NU,*)' '
   WRITE(NU,*)' Furnace cross-sectional area (m**2) = ',R0*WIDTH
   WRITE(NU,*)' '
   WRITE(NU,*)' Reference gas flow properties'
   WRITE(NU,*)'    Pressure (pa) =      ',PG0
   WRITE(NU,*)'    Temperature (K) =    ',T0
   WRITE(NU,*)'    Density (kg/m**3) =  ',DNST0
   WRITE(NU,*)'    Velocity (m/s) =     ',UG0
   WRITE(NU,*)'    Flow rate (kg/s) =   ',GMFR
   WRITE(NU,*)'    Composition (mass fraction):'
   WRITE(NU,*)'       Fuel =        ',YF0
   IF (YOX0.GT.ZERO) WRITE(NU,*)'       Oxydizer =        ',YOX0
   IF (YP10.GT.ZERO) WRITE(NU,*)'       CO2  =       ',YP10
   IF (YP20.GT.ZERO) WRITE(NU,*)'       H2O  =         ',YP20
   IF (YN20.GT.ZERO) WRITE(NU,*)'       Nitrogen =           ',YN20
   WRITE(NU,*)'    Molecular Weight (kg/kmol) = ',WT0
   WRITE(NU,*)' '
ENDIF

!----------------------------------------------------------------------
!     Properties of droplets and particles
!       DDENS: droplet density (kg/m**3)
!       PDENS: particle density (kg/m**3)
!       CL: specific heat (J/(kg*K))
!       TTWE: temperature for total drop on wall evaporation (K)
!       ACDD: coefficients in psi function for drop turb. diffusivity
!       NT,NS: parameters in inlet droplet size distribution
!----------------------------------------------------------------------
!     Heavy oil cracking (ref:Weekman)
!       reaction 1: heavy oil cracking
!       reaction 2: light oil conversion
!       CKA1,CKA2,CKA3: stoichiometric coefficients of reaction 1
!       CKB1,CKB2: stoichiometric coefficients of reaction 2
!       CKK1: rate constant of reaction 1 (1.05/s)
!       CKK2: rate constant of reaction 2 
!       CKR: ratio of rate constants, CKK1/CKK2 (0.074)
!       CKALP: catalyst deactivation coefficient (0.01125/s)
!       CKBET: carrier-to-oil volume ratio   
!       CKBET1: carrier-to-oil vapor volume ratio   
!       ELH: latent heat (J/kg)
!       TB: boiling point (K)
!----------------------------------------------------------------------
tb=6.75d+2 ! boiling point (k) 
tb=tb/t0
if (nphas>1) then
   GMFR=GMFR+DMFR0
endif
UL0=0.9D+0*UG0
!      DND0=PMFR0*.75D+0/PI/RLM0**3/DDENS/UG0/R0/WIDTH
A_IN=R0*WIDTH
TH_C=PI/0.75D+0*RD_P**3*PDENS*UL0*A_IN
DND0=PMFR0/TH_C      
DNST1=PG0*WF/RU/T0
CKBET1=CKBET*DNST1/DDENS     
!cz      CKBET1=CKBET*DNST1/DDENS*100.0d+0 


!----------------------------------------------------------------------
!     Droplet & Particle Parameters
!       NDP0: number of droplet size groups
!       NPT0: number of particle size groups
!       NDNP: total number of droplet and particle size groups
!----------------------------------------------------------------------
!CSL      NDP0=NDP
!CSL      NPT0=NPT
!CSL      NDNP=ND
IF (NDP0.EQ.1) THEN
   RD(1)=ONE      
   DRD(1)=ONE
ELSEIF (NDP0.GT.1) THEN
   RD1=.875D0/NDP0
   DO L=1,NDP0
      RD(L)=RD1+(1.75D0*(L-1))/NDP0
      IF (L.GT.1) DRD(L-1)=RD(L)-RD(L-1)
   END DO
   DRD(NDP0)=DRD(NDP0-1)
ENDIF
IF (NPT0.EQ.1) THEN
   RD(NDNP)=RD_P/RLM0
   DRD(NDNP)=ONE
ELSEIF (NPT0.GT.1) THEN
   RD1=.875D0/NPT0*RD_P/RLM0
   DO L1=1,NPT0
      L=L1+NDP0
      RD(L)=RD1+(1.75D0*(L1-1))/NDP0
      IF (L1.GT.1) DRD(L-1)=RD(L)-RD(L-1)
   END DO
   DRD(NDNP)=DRD(NDNP-1)
ENDIF
DO L=1,NDNP
   RD2(L)=RD(L)**2
   RD3(L)=RD(L)**3
   PSI(L)=ONE+ACDD(1)*RD(L)+ACDD(2)*RD(L)**2
END DO 
        


RESG=ZERO !initialize gas residual array to zero
RES=ZERO  !initialize dispersed phase residual array to zero

!----------------------------------------------------------------------
!     CALCULATE MIXTURE PROPERTIES AND DIMENSIONLESS PARAMETERS
!----------------------------------------------------------------------
!CSL      IF (itr_gas.LE.0) THEN
GDF0=GDF0*(TL00/2.98D+2)**AMD
GMU_ref=GMU_ref*(TL00/2.98D+2)**AMU
!CSL     ENDIF
TK0=CTEA*UG0**2
EPSI0=CTDA*TK0**2
REYG=TWO*DNST0*UG0*R0/GMU_ref
REYG2=REYG/TWO
IF (NPHAS .EQ. 1) THEN
   THET0C=ZERO
ELSE
   THET0C=(PI4/3D+0)*RLM0**3*DND0
ENDIF
DMFR=THET0C*DDENS*UG0*AREA0
PMFR=THET0C*PDENS*UG0*AREA0
SCN=GMU_ref/GDF0/DNST0
REYD=TWO*DNST0*UG0*RLM0/GMU_ref
!      DNUL0=ONE+.3D+0*SQRT(REYD*ABS(ONE-UL0/UG0))*SCN**THIRD
DNUL0=ONE+.276D+0*SQRT(REYD*ABS(ONE-UL0/UG0))*SCN**THIRD
DNUC0=TWO+.654D+0*SQRT(REYD*ABS(ONE-UL0/UG0))*SCN**THIRD
GCN=PI4*GL0*DNUL0*RLM0*DND0*R0/(CP0*DNST0*UG0)
GVN=PI2*GL0*DNUC0*RLM0*DND0*R0/(CP0*DNST0*UG0)
Z0=ONE+0.15D+0*(REYD*ABS(ONE-UL0/UG0))**0.687D+0
GDN=6.0D+0*PI*Z0*GMU_ref*RLM0*DND0*R0/(DNST0*UG0)
HNORM=R0/(DNST0*UG0*H_0)
Q0=Q0/H_0
ECN=UG0**2/H_0
EUN=PG0/(DNST0*UG0**2)
DAMKO=R0/UG0*BE*T0**ALPHAE*DNST0**STOX2*EXP(-EAC/(RU*T0))/WO2
RFG=THET0C*DDENS/DNST0
RFG_P=THET0C*PDENS/DNST0
EPSR=TK0/UG0**2
TAUR=EPSI0*R0/(TK0*UG0)
TB=TB/T0
TTWE=TTWE/T0
ELH=ELH/H_0
IF (STEADY) THEN
   TRN=ZERO
ELSE
   TRN=R0/UG0/TP0
ENDIF
!      WRITE(NU,*)' NORMALIZED BOILING TEMPERATURE = ',TB
!cbg      IF (.NOT.REACT) GOTO 170



!----------------------------------------------------------------------
!     Bounds of computed flow properties
!----------------------------------------------------------------------
U_MX=U_MX/UG0
U_MN=-U_MX
DN_MX=3.0D+0/(PI4*RD_P**3*DND0)*(ONE-TH_MN)*1.0D+1
P_MASS=PI/0.75D+0*(RD_P)**3*PDENS
P_DND0=0.75D+0/(PI*RD_P**3)
SP0=THET0C*PDENS*UG0*UG0
IF (NDNP.GT.0) THEN
   H0_L=(CPF(1)*CP0*(TB(1)-TRDG)-CL*TB(1))*T0-ELH*H_0
else
   h0_l=zero
ENDIF

!---  RADIATION
!QRN=zero !3 dim array
QR0=H_0*DNST0*UG0/R0
QEW0=zero
QEW_G=0
QEW_W=0
QEW_S=0
QEW_IO=0
QLS_G=0
QLS_A=0
do_rad_qe_only_cnt=0
RETURN
END


!==============================================================================
!==============================================================================
!==============================================================================
!     Allocate Large Gas Phase Arrays
!     Populate kinetics data
!==============================================================================
subroutine allocate_large_gas_phase_arrays
use gbl_var
implicit double precision (a-h,o-z)

!----------------------------
! Main Gas Phase Arrays

if (.not.allocated(p)) then !used in radiation, need to be sure allocated
   allocate (p(mp,np,lp),dnst(mp,np,lp),T(mp,np,lp)) 
   allocate (smf(mp,np,lp))
   p=zero
   dnst=one
   T=one
   smf=zero
endif
allocate(T_surf_prev(mz,nz))
   T_surf_prev=1500.0d+0 !make sure we do not start with any zero temperatures
allocate (ug(mp,np,lp,3),gf(mp,np,lp,9)) 
   gf=zero
   ug=zero
allocate (theta(mp,np,lp),crate(mp,np,lp),hrate(mp,np,lp))
   theta=one
   crate=0
   hrate=0
allocate (tmu(mp,np,lp),gcp(mp,np,lp),glam(mp,np,lp))
   tmu=zero
   gcp=zero
   glam=zero
allocate (gama(mp,np,lp),gte(mp,np,lp))
   gama=zero
   gte=0
allocate (fz(mp,np,lp),bs(mp,np,lp),sfp(mp,np,lp))
   fz=0
   bs=0
   sfp=0
allocate (bs_hprev(mp,np,lp))
   bs_hprev=zero
allocate (ap(mp,np,lp),as(mp,np,lp,3,2))
   ap=0
   as=0
allocate (fly(np,10),flyh(np,5))
   fly=0
   flyh=0
allocate (flx(mp,10),flxh(mp,5))
   flx=0
   flxh=0
allocate (flux_z(lp,10),fluxh_z(lp,5))
   flux_z=0
   fluxh_z=0

!if (.not.steady) then
!   allocate (po(mp,np,lp),dnsto(mp,np,lp),thetao(mp,np,lp))
!   po=0
!   dnsto=0
!   thetao=0 
!   allocate (ugo(mp,np,lp,3),gfo(mp,np,lp,9)) 
!   ugo=0
!   gfo=0
!   allocate (tgo(mp,np,lp),apo(mp,np,lp))
!   tgo=0
!   apo=0
!endif

!----------------------------
! Particle and Droplet Arrays

if (ndnp.ge.1) then
   allocate (rd(ndnp),rd2(ndnp),rd3(ndnp),drd(ndnp))
   allocate (psi(ndnp),tb(ndnp))
      psi=0
      tb=0
   allocate (dn(mz,nz,lz,ndnp),dt(mz,nz,lz,ndnp))
      dn=zero
      dt=zero
   allocate (du(mz,nz,lz,ndnp,3),dc(mz,nz,lz,npt0))
      du=zero
      dc=zero
   allocate (th_dp(mz,nz,lz),th_pt(mz,nz,lz),sp(mz,nz,lz))
      th_dp=0
      th_pt=0
      sp=0
   allocate (evp(mz,nz,lz),con(mz,nz,lz))
      evp=zero
      con=zero
   allocate (gdiff(mp,np,lp))
      gdiff=0

   !   if (.not.steady) then
   !      allocate (dno(mz,nz,lz,ndnp),dto(mz,nz,lz,ndnp))
   !      allocate (duo(mz,nz,lz,ndnp,3),dco(mz,nz,lz,npt0))
   !      allocate (dc(mz,nz,lz,npt0))
   !      dno=0
   !      dto=0
   !      duo=0
   !      dco=0
   !      dc=0
   !   endif
endif

!----------------------------
! Radiation Arrays

allocate (qe(mz,nz,lz),qa(mz,nz,lz),ql(mz,nz,lz))
   qe=zero
   qa=zero
   ql=zero
allocate (qrs(mp,np))
   qrs=zero


!----------------------------------------------------------------------
! Allocate and Populate Reaction Kinetics Data for Integral Reaction Model
!       rtim: input time in micro seconds converted to seconds
!       hrt: input enthalpy in kcal/mol converted to j/kmol
!       chrd: empirical coefficient for heat of reaction
!
!----------------------------------------------------------------------

allocate (rtim(nr0),delrt(nr0),hrt(nr0))
   rtim=0
   delrt=0
   hrt=0
allocate (delr(nr0),timed(nr0),hrd(nr0))
   delr=0
   timed=0
   hrd=0

data chrd /0.166d+0/
filename='kinetic.d' !note that this file is not in the case folder
open(nu_kinet,file=filename,status='old')
read(nu_kinet,*)garb
do i=1,nr0
   read(nu_kinet,*)rtim(i),delrt(i),hrt(i)
   !cbg         rtim(i)=rtim(i)*1.d-6
   rtim(i)=rtim(i)*0.7
   !cbg         hrt(i)=chrd*hrt(i)*4.1868d+6/h_0/wf
   hrt(i)=chrd*hrt(i)/h_0/wf
enddo

!tmax=rtim(nr0-1) !this tmax is probably a time max, but does not seem to be used anywhere                    
read(nu_kinet,*)garb
do i=1,nr0
   read(nu_kinet,*)delr(i),timed(i),hrd(i)
   !cbg         timed(i)=timed(i)*1.0d-6
   timed(i)=timed(i)*0.7
   !cbg         hrd(i)=chrd*hrd(i)*4.1868d+6/h_0/wf
   hrd(i)=chrd*hrd(i)/h_0/wf
enddo
close(nu_kinet)

return
end




!======================================================================
!======================================================================
!======================================================================
!
!     Allocate_ms does primary memory allocation 
!     for the (minor) sub species.
!
!======================================================================
SUBROUTINE allocate_ms
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

IF (NSP0 > 0 .and. ms==1) THEN
   ALLOCATE (GFM(MP,NP,LP,NSP0)) 
   GFM=0 !Subspecies Mass Fraction
   ALLOCATE (RRMS(MZ,NZ,LZ,NRM0)) 
   RRMS=0
   ALLOCATE (FLXM(MP,NSP0,2)) 
   FLXM=0
   ALLOCATE (FLYM(NP,NSP0,2)) 
   FLYM=0
   ALLOCATE (NTIMESM(NSP0))
   !NTIMESM=1
   ALLOCATE (RFM(NSP0+1),RFMC(NSP0+1)) !allocate for sub-species + soot
   !RFM=0.9D+0
   !RFM(NSP0+1)=0.7D+0
   !RFMC=ONE-RFM
   allocate (resid_ms(nsp0+1))
   resid_ms=1.0d-16
   allocate (resid_ms_pre(nsp0+1))
   resid_ms_pre=1.0d-16
   allocate (bs_soot_prev(mp,np,lp),sfp_soot_prev(mp,np,lp))
   bs_soot_prev=zero
   sfp_soot_prev=zero
ENDIF

return
end


