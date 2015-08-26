!======================================================================
! SPHASE.F90
!======================================================================
! Routines:
!     sphase   single phase computation
!     prn_scr  print display to screen for command window
!======================================================================
!======================================================================
!======================================================================
SUBROUTINE SPHASE
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
integer save_maxms
integer just_did_prn_scr

1     FORMAT(1X/T15,'TYPICAL ITERATION RESULT FOR GAS FLOW', &
       ' FIELD'/T2,'itr',T11,'RMX',T20,'IMX,JMX,KMX',T35,'resid',/, &
       T8,'   U(I,J,K)   V(I,J,K)   W(I,J,K)   P(I,J,K) DNST(I,J,K)  T(I,J,K)')
2     FORMAT(' G',I5,E12.4,3I4,E12.4)
3     FORMAT(T8,5E11.4,F10.2)

!----------------------------------------------------------------------
!     res_mass: mean mass residual
!     res_mass_max: max mass residual
!     MAXGI: maximal allowable gas phase iterations
!     BGCON: acceptable mass residual for a gas phase calculation
!----------------------------------------------------------------------
IF (IDEBUG.EQ.2.AND.NPHAS.EQ.1) WRITE(NU,1)

!----------------------------------------------------------------------
!     BEGIN ITERATION
!        GFLOW: calculate gas flow properties
!        OSAVE: store system values in separate arrays
!----------------------------------------------------------------------
just_did_prn_scr=0
CPUS1=SECNDS(0.0) !initialize timer
do while (itr_gas < itr_gend .and. itr_gas < maxgi)
   !DO M=1,1
   !IF (itr_gas.GT.100000) itr_gas=0
   itr_gas=itr_gas+1



   !-------------------------------
   !Code to set flag to calc residuals
   !not currently used
   !IF (NPHAS.EQ.1) THEN 
   !   IF (M.GE.MAXGI-1) IRC=1
   !   !if (.not.steady) CALL OSAVE
   !ENDIF
   CALL GFLOW 

   if (iconv>0) write(nug,"(i7,4e25.16)") itr_gas,res_mass,res_mass_max
   q_in_tot=q_react+q_h_in
   q_out_tot=q_exhaust+q_melt+q_wall_loss
   q_global_bal=abs((q_in_tot-q_out_tot)/q_in_tot)
   if (ihresid>0) write(nu_hres,"(i7,4e25.16)") itr_gas,h_resid,q_global_bal,h_resid_pre

   IF (IDEBUG.EQ.2) THEN
      WRITE(NU,2) itr_gas,res_mass_max,IMX,JMX,KMX,res_mass
      WRITE(20,2) itr_gas,res_mass_max,IMX,JMX,KMX,res_mass
      G1=UG(IMX,JMX,KMX,1)
      G2=UG(IMX,JMX,KMX,2)
      G3=UG(IMX,JMX,KMX,3)
      G4=P(IMX,JMX,KMX)
      G5=DNST(IMX,JMX,KMX)
      G6=T(IMX,JMX,KMX)*T0
      WRITE(NU,3) G1,G2,G3,G4,G5,G6
      WRITE(20,3) G1,G2,G3,G4,G5,G6
      !CSL            write(nu,*) 'surface heat loss:',qsurf,' W' 
   ELSE
      !Get the mean temperature of the exit cells into t_av 
      !and the maximum exit temperature into t_x
      G1=0
      T_AV=0
      T_X=0
      DO I=2,MP,2
      DO J=2,NP,2
      DO K=2,LP,2
         IF (IBCELL(I,J,K).NE.3) CYCLE
         G0=AREA_C(I,J,K,3)
         IF (G0.LT.small20) G0=AREA_C(I,J,K,2)
         IF (G0.LT.small20) G0=AREA_C(I,J,K,1)
         G1=G1+G0
         T_AV=T_AV+T(I,J,K)*G0
         IF (T(I,J,K).GT.T_X) T_X=T(I,J,K)
      enddo;enddo;enddo
      T_AV=T_AV*T0/G1
      T_X=T_X*T0
   ENDIF

   !-------------------------------------------------
   ! Cut and paste block below at point to check in other routines
   ! Full size arrays
   ! Serial code
   !-------------------------------------------------
   !icompare=0
   !istore=0
   !if (istore==1)then
   !      call arrays_fstore_serial
   !endif
   !if (icompare==1)then
   !      call arrays_fcompare_par
   !endif

   CPUS2=SECNDS(CPUS1) ! get elapsed time since timer set
   IF (CPUS2.GT.20) THEN
      ! the timer has run for more than 20 seconds
        
      CALL PRN_SCR(1) !print gas display to screen for command window      
 
      !---------------------------------------------------
      ! Check for communication from GUI

      filename=casedir//'\gui_update.txt'
      inquire (file=filename,exist=exst)
      if (exst) then
         !user has requested change in gui update status
         open(nu_guiup,file=filename)
         read (nu_guiup,*) gui_update
         close(nu_guiup)
         nfn1=delfilesqq(filename) !delete file
      endif
 
      filename=casedir//'\runstop.dat'
      inquire (file=filename,exist=runstop_exist)
      if (runstop_exist) then
         !user has requested to stop the program
         !nfn1=delfilesqq(filename)  !delete file
         exit ! return to main routine to write files and stop program
      endif

      !------------------------------ 
      ! Send data back to gui program

      if (gui_update==1) then   
	      filename=casedir//'\gfm.dat'
         open(nu_gfm,file=filename)
         WRITE(nu_gfm,"(I5)") itr_gas
         WRITE(nu_gfm,"(I5)") MAXGI
         WRITE(nu_gfm,"(E10.3)") res_mass
         WRITE(nu_gfm,*) 'Temperature'
         WRITE(nu_gfm,*) 'K'
         DO I=2,MP,2
         DO J=2,NP,2
         DO K=2,LP,2
            WRITE (nu_gfm,*) T(I,J,K)*T0
         enddo;enddo;enddo
         WRITE(nu_gfm,"(E10.3)") BGCON
         WRITE(nu_gfm,*) T_AV !average exit temperature
         WRITE(nu_gfm,*) Q_F !heat content of fuel
         WRITE(nu_gfm,*) (QEG0-QEG_G-QEW_G)*1.0D-6 !sum qe-qal-qalw0 for open cells
         WRITE(nu_gfm,*) SVF_R
         WRITE(nu_gfm,*) QA_S0,QA_S1
         WRITE(nu_gfm,"(I5)") itr_rad
         CLOSE(nu_gfm)
      endif
      CPUS1=SECNDS(0.0) !reinitialize timer
      just_did_prn_scr=1
   else
      just_did_prn_scr=0
   endif !timer check

   !Q_EX1=FLYH(3,2)+FLYH(NP-1,1)
   !Q_EX1Z=FLUXH_Z(3,2)+FLUXH_Z(LP-3,1)
   !IF (GFEX.LT.1.0D-10.OR.Q_EX1.LT.Q_IN*1.0D-2) cycle
   !cbg  IF (GFEX.LT.1.0D-10.OR.Q_EX1Z.LT.Q_IN*1.0D-2) GOTO 200

   ! 7/19/05 
   !There is obviously a problem here
   !Various adjustments to enthalpy via fac0 are seen below and commented out
   !The one active when I got the code, was to do the adjustment in the plane j=np/2+1
   !Q_EX1=MAX(Q_EX1,0.1*Q_IN)
   !cbg     Q_EX1=MAX(Q_EX1Z,0.1*Q_IN)
   !THIS CODE COULD BE AN ATTEMPT TO CORRECT THE ENTHALPY IMBALANCE PROBLEM,  8/7/05
   !
   !ihadj=0
   !if(ihadj==1)then !Don't adjust enthalpy unless there is an obvious need
      !FAC0=((Q_IN-Q_LS+QSURF)/GFIN)/(Q_EX1/GFEX)
      !IF (FAC0-ONE.GT.1.0D-1) FAC0=1.1D+0
      !IF (FAC0-ONE.LT.-1.0D-1) FAC0=0.9D+0
      !DO I=4,MPM2,2
      !DO K=4,LPM2,2
         !cbg            DO 170 J=4,NPM2,2
         !J=NP/2+1
         !cbg               K=LP/2+1
         !GF(I,J,K,IH)=GF(I,J,K,IH)*FAC0
      !enddo;enddo
      !QCONV=1.0D-3
      !CSL         IF (res_mass.LE.BGCON.AND.ABS(FAC0-ONE).LT.QCONV) GOTO 300
   !endif

   if (res_mass < bgcon) then
      itr_gend=itr_gas
      exit
   endif

   if (itr_gas < itr_gend .and. itr_gas < maxgi) then
      !will be staying in this do loop, may have extra things to do

      !write out energy values to information file
      if (iinfo>0) call track_energy
      !cycle
      if (id_rad>0) then !doing radiation
         !only call rad_qe_only after combustion space has developed
         !if (initial_gitr==0 .or. (initial_gitr > 0 .and. itr_gas > initial_gitr)) then
            do_rad_qe_only_cnt=do_rad_qe_only_cnt + 1
            if (do_rad_qe_only_cnt >= rad_qe_frequency .and. itr_gas > 50) then
               !if (i1st_rad_qe_done==0 .and. imsflow_done==0) then
               if (i1st_rad_qe_done==0) then
                  !if doing a new start and the rad_qe_only routine has not been called,
                  !then must do initial minor species calculation
                  call prn_scr(1) !print gas display to screen for command window
                  save_maxms=maxms
                  maxms=300
                  call initsvm  !guess or restore initial flowfield for sub species
                  call msflow
                  call sav3fm
                  maxms=save_maxms
                  if (runstop_exist) exit
               !elseif (i1st_rad_done == 0) then 
               !   maxms=20
               !   call msflow
               !   maxms=save_maxms
               endif
               call rad_qe_only !Only need emission radiation from open cells at this time
               if (runstop_exist) exit
               do_rad_qe_only_cnt=0
            endif
         !endif !initial_gitr
      endif !id_rad
   !else
   !   call sav3fm
   endif !itr_gas
enddo
      
if (just_did_prn_scr==0) call prn_scr(1) !print gas display to screen for command window      

return
end


!======================================================================
!======================================================================
!======================================================================
!     PRN_SCR(N_PS)
!        print display to screen for command window
!======================================================================
SUBROUTINE PRN_SCR(N_PS)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

select case (n_ps)
case (1) !gas flow 
   !write(ncon,'("Flow Calculation:")') 
   write(ncon,'(/"Flow Calculation:",t54,"mean temperature=",f8.1)'),Tave 
   write(ncon,'(T6,"No of Iterations:",T31,I6,T45,"/",I6)') itr_gas,MAXGI 
   write(ncon,'(5X,"Convergence (comp/set):",T31,E12.4,T45,"/",E12.4)') res_mass,BGCON        
   write(ncon,'(5X,"Exhaust Temp (ave/max):",T31,F8.0,T45,"/",F8.0," K")') T_AV,T_X   

case (2) !subspecies 
   write(ncon,'(/"Subspecies Calculation:")') 
   !write(ncon,'(5X,"Balance:",T31,E12.4,T45,"/",E12.4,I7)') bal_MS,BMCON,itr_ms        
   write(ncon,'(5X,"Exhaust O2,H2O,CO2,N2:",T33,3(F5.1,","),F5.1," %")') S_O2,S_H2O,S_CO2,S_N2
   write(ncon,'(5X,"        CO,NO,Soot:",T34,2(E10.3,","),E10.3)') S_CO,S_NO,SVF_R

case (3) !radiation, volume 
   write(ncon,'(/"Volume Radiation Heat Transfer Calculation:   heat in (MW)=",f8.2)') Q_F
   !write(ncon,'(5X,"Heat Input (MW):",T31,F8.2)') Q_F 
   G1=QEG_G+QEW_G       
   G0=G1-QEG0
   write(ncon,'(5X,"Gas emi/abs/net (MW):",T31,3(-6PF9.3))') -QEG0,G1,G0

   !--------------- this part probably should not be here
   !G1=QEG_W+QEW_W-QLS_G       
   !write(ncon,'(5X,"Wall emi/abs/loss (MW):",T31,3(-6PF9.3))') -QEW0,G1,q_wall_loss
   !G1=QEG_S+QEW_S   
   !G0=G1-QES0
   !write(ncon,'(5X,"Glass emi/abs/net (MW):",T31,3(-6PF9.3))') -QES0,G1,q_melt

   !IF (N_PS.GT.0) THEN        
   !QL0=QEG_IO+QEW_IO
   !write(ncon,'(5X,"I/O heat loss (MW):",T31,-6PF10.4)') QL0

   !G0=QEG0+QEW0+QES0
   !QAG0=QEG_G+QEW_G
   !QAW0=QEG_W+QEW_W
   !QAS0=QEG_S+QEW_S
   !IF (G0.GT.small20) THEN
   !   G0=(QAG0+QAW0+QAS0+QL0)/G0
   !ELSE
   !   G0=0
   !ENDIF
   !---------------end this part probably should not be here

   !IF (N_PS.EQ.1) THEN
   IF (L_AB.GT.NCELLS) L_AB=NCELLS
   !--------------- the balance part probably should not be here
   !write(ncon,'(5X,"Nodes computed:",T31,I6,"/",I6,T50,"Balance:",F8.3)') L_AB,NCELLS,G0
   write(ncon,'(5X,"Nodes computed:",T31,I6,"/",I6)') L_AB,NCELLS

case (4) !radiation, walls 
   write(ncon,'(/"Wall Radiation Heat Transfer Calculation:")')
   write(ncon,'(5x,"No. Patches: ",i5,"        Rad. Iter: ",i7)') b_cnt,iterw
   write(ncon,'(5x,"q incident: ",e12.4,"  q convection:",e12.4)') q_incident_total,qconv_total
   write(ncon,'(5x,"q wall loss:",e12.4,"  q to melt:   ",e12.4,"  mean wall T:",f8.1)') q_wall_loss,q_melt,avg_wall_T  

end select
            !is cpu time needed ?
   !CALL GETTIM(IHR2,IMIN2,ISEC2,IHUND2)
   !CPU=IHR2-IHR1
   !CPU=CPU*60.0+(IMIN2-IMIN1)
   !CPU=CPU*60.0+(ISEC2-ISEC1)
   !WRITE (6,'(/"CPU time =",F8.1," s")') CPU-CPU1
   !CPU1=CPU
   !IF (N_PS.EQ.0) THEN     !was anything not inside rad       
   !   WRITE(6,'(6/)')
   !ELSEIF (N_PS.EQ.1) THEN !was doing volume rad
   !   WRITE(6,'(4/)')
   !ELSE                    !was doing wall rad
   !   WRITE(6,'(3/)')
   !ENDIF

RETURN
END
