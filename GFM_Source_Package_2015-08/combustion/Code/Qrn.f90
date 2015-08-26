!==============================================================================
!==============================================================================
!==============================================================================
!
! Qrn.f90
!
!==============================================================================
!  Calculation of net radiation heat power
!     QE:  emissive power (W)
!     QA:  absorptive power (W)
!     WL: wavelength (m)
!     rev: 2/02
!
!----------------------------------------------------------------------
!General flow of routine:
!
!Allocate qe.., qa.., ang.. arrays
!Gas Emission and Absorption Section
!  If itr_gas=next_rad then id_rad=10, iter_rad=itr_rad+1, next_rad=next_rad+interval_rad
!  set 20 second timer
!  Loop thru cells
!     l_ab counts open cells
!     stay in loop only for open cells and melt surface
!     call debx(T)
!     call qref(i,j,k)
!     for open cell, add qe to qeg0
!        for surface cell, if id_rad=10, add qe to qes0
!     if timer expired then
!        if id_rad=10 then call qrn1
!           check for runstop
!  if id_rad=10 then call qrn1
!     if id_rad=1 then
!         deallocate ang.., T, qrs arrays
!      check for qe scale back
!      exit sub
!  
!Wall Emission and Reflection Section - only get here if id_rad=10
!  Allocate qalw.. arrays
!  Init some variables, qaw0=qeg_w
!  set 20 second timer
!  Loop for a max of six times
!     call twall
!     zero out qalw0 and qew0
!     loop thru wall cells (l_ab seems to be some sort of backwards k counter)
!        add qe to qew0
!        call qrew(i,j,k)
!        if timer expired then
!           call qrn2
!           check for runstop
!     set fac_w= (qeg_w+qew_w)/qaw0
!     set qaw0=qeg_w+qew_w
!     call qrn2
!     exit loop if fac_w-1<=.01
!  call qrn_prn
!     deallocate qalw.., ang.., T, qrs arrays
!  check for qe scale back
!----------------------------------------------------------------------
SUBROUTINE QRNF
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
real*8 qw_diff
real*8 qe_all,qa_all,q_diff

ALLOCATE (QEL(MZ,NZ,LZ,NWL))
QE=zero
QEL=zero
ALLOCATE (QEL0(NWL),QEL1(NWL))
QEL0=zero
QEL1=zero
ALLOCATE (QAL(MZ,NZ,LZ,NWL))
QA=zero
QAL=zero
ALLOCATE (ANG1(MP,2),ANG2(NP,2),ANG3(LP,2))
ANG1=zero
ANG2=zero
ANG3=zero
QEG0=0

qa_sum=zero
qe_sum=zero

qe_sum_from_open=zero !calculated in Qref
qe_sum_from_melt=zero !calculated in Qref
qa_sum_to_melt =zero
qa_sum_to_exit =zero
qa_sum_to_inlet=zero
qa_sum_to_wall =zero
qa_sum_to_open =zero


!-----------------------------------
!     Gas Emission and Absorption
!-----------------------------------
!I1=2  
!J1=2
!K1=0
L_AB=0 
   ! 7/20/05
!There is no case where id_rad is set to 2, so the following
!dead code has been commented out.
   !IF (ID_RAD.EQ.2) THEN
   !   CALL SAVQ(0,I1,J1,K1)
   !   IF (IDEBUG.EQ.2) CALL QRN_DB
!   ID_RAD=10
!   IF (I1.GE.MP) GOTO 160
!ELSEIF (ID_RAD.GE.1.AND.itr_gas.EQ.next_rad) THEN



!This loop computes volume radiation emission in qe
!and absorption in the volume in qa
!and radiation incidence on walls from the volume in qa
CPUS1=SECNDS(0.0) !initialize timer
DO I=2,MP,2; ID2=I/2
   !IF (I.LT.I1) CYCLE
DO J=2,NP,2; JD2=J/2
   !IF (I.EQ.I1.AND.J.LT.J1) CYCLE
DO K=2,LP,2
   IBC0=IBCELL(I,J,K) ! fixed 7/12/05 by adding line
   IF (IBC0.LT.1) L_AB=L_AB+1   
   !l_ab counts open cells
   !IF (I.EQ.I1.AND.J.EQ.J1.AND.K.LE.K1) CYCLE
   !IBC0=IBCELL(I,J,K)

   !IF (IBC0.GE.1.AND.IBC0.NE.4) CYCLE
   !Proceed only with open cells and melt surface

   IF (IBC0.GE.1) CYCLE
   !Proceed only with open cells 

   KD2=K/2
   IF (T(I,J,K).LE.TG_MN) CYCLE

   CALL DEBX(T(I,J,K))
   CALL qref(I,J,K)

   qa_sum_to_melt =qa_sum_to_melt +qa_sum_to_melt_cell
   qa_sum_to_exit =qa_sum_to_exit +qa_sum_to_exit_cell
   qa_sum_to_inlet=qa_sum_to_inlet+qa_sum_to_inlet_cell
   qa_sum_to_wall =qa_sum_to_wall +qa_sum_to_wall_cell
   qa_sum_to_open =qa_sum_to_open +qa_sum_to_open_cell

   qa_sum=qa_sum+qa_sum_point
   qe_sum=qe_sum+QE(ID2,JD2,KD2)
   IF (IBC0.LE.0) QEG0=QEG0+QE(ID2,JD2,KD2)
   ! deleted next line because now only here when have open cell
   !IF (IBC0.EQ.4.AND.ID_RAD.EQ.10) QES0=QES0+QE(ID2,JD2,KD2)
   CPUS2=SECNDS(CPUS1) ! get elapsed time since timer set
   IF (CPUS2.GT.20) THEN
      ! the timer has run for more than 20 seconds

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
 
      CALL QRN1 !create gfm.dat file to pass info to GUI for Radiation Absorption
 
      filename=casedir//'\runstop.dat'
      inquire (file=filename,exist=runstop_exist)
      if (runstop_exist) then
         !user has requested to stop the program
         !nfn1=delfilesqq(filename)  !delete file
         exit ! return to main routine to write files and stop program
      endif

   ENDIF
enddo;  if (runstop_exist) exit
enddo;  if (runstop_exist) exit
enddo
if (runstop_exist) return

!Write radiation details to file
qe_sum=tot_qe_h2o_co2+tot_qe_soot
qa_all=qa_sum_to_wall+qa_sum_to_melt+qa_sum_to_open+qa_sum_to_inlet+qa_sum_to_exit 
q_diff=qe_sum_from_open-qa_all
if (qe_sum_from_open>zero) q_diff=q_diff/qe_sum_from_open
if (irad_detail>0) write(nu_rad,"(1X,I5,11E22.14)") itr_gas, &
         tot_qe_h2o_co2, &
         tot_qe_soot, &
         qe_sum, &
         qe_sum_from_open, &
         qa_sum_to_melt, &
         qa_sum_to_wall, &
         qa_sum_to_open, & 
         qa_sum_to_exit, & 
         qa_sum_to_inlet, &
         qa_all, &
         q_diff


   !160  continue
CALL QRN1 !create gfm.dat file to pass info to GUI for Radiation Absorption
DEALLOCATE (AKLG,AKLS,AKL0,E0)


!-------------------------------------
!  New Wall Enclosure exchange calculation
!-------------------------------------
!if (id_rad == 10) then
   call wall_q_amb
   call wallq
   call qrn2
   call qrn_prn
!endif
!-------------------------------------
! End new code
!-------------------------------------

DEALLOCATE (ANG1,ANG2,ANG3)
!DEALLOCATE (T,QRS)
!shouldn't this limit the net value emission-absorption? 
!IF (QEG0.GT.Q_F*1D6) THEN !limits qeg0 to heat content of fuel
!   G0=Q_F*1.0D6/QEG0
!   qe_scale_back = qe_scale_back+1 !keep track of scale backs for printing in info file
!   qe_scale_back_amount = qe_scale_back_amount + g0
!   QE=QE*G0 !scales back emission  
!   QEG0=Q_F*1D6
!ENDIF

RETURN

!==============================================================================
!==============================================================================
!==============================================================================
CONTAINS

!==============================================================================
!==============================================================================
! QRN1
!    Reinitialize timer
!    Calc certain values
!    Create gfm.dat file to pass info to GUI for Radiation Absorption
!    Optionally, print screen display
!    Optionally, save intermediate heat flux results in QRTMP
!==============================================================================
SUBROUTINE QRN1
!IF (ID_RAD.LT.10) RETURN !Added check before calls   8/23/05
CPUS1=SECNDS(0.0) !reinitialize timer
IF (IDEBUG.EQ.2) CALL QRN_DB
QEG_G=0
QEG_W=0
QEG_S=0
QEG_IO=0
DO I0=2,MP,2;  I0D2=I0/2
DO J0=2,NP,2;  J0D2=J0/2
DO K0=2,LP,2
   IBC0=IBCELL(I0,J0,K0)
   K0D2=K0/2
   G0=0
   DO L=1,NWL
      G0=G0+QAL(I0D2,J0D2,K0D2,L)
   ENDDO
   QA(I0D2,J0D2,K0D2)=G0 !qa set to sum of qal for all cell types
   IF (IBC0.LE.0) THEN
      QEG_G=QEG_G+G0 !rad energy absorbed in open cells
      !ELSEIF (IBC0.LE.3) THEN
   ELSEIF (IBC0==2.or.IBC0==3) THEN
      QEG_IO=QEG_IO+G0 !rad energy absorbed inlets, exits
   ELSEIF (IBC0==4) THEN
      QEG_S=QEG_S+G0 !rad energy absorbed at melt surface
   ELSE
      QEG_W=QEG_W+G0 !rad energy absorbed at walls
   ENDIF
enddo;enddo;enddo

!------------------------------ 
! send data back to gui program

if (gui_update==1) then   
	filename=casedir//'\gfm.dat'
   open(nu_gfm,file=filename)
   WRITE(nu_gfm,"(I5)") L_AB
   WRITE(nu_gfm,"(I5)") NCELLS
   WRITE(nu_gfm,"(E10.3)") QEG0-QEG_G !qe for open cells - qal for open cells
   WRITE(nu_gfm,"('Radiation Absorption')")
   WRITE(nu_gfm,"('W/m**3')")

   !Write out qal/volume for open cells, else write out 0
   DO I0=2,MP,2; I0D2=I0/2
   DO J0=2,NP,2; J0D2=J0/2
   DO K0=2,LP,2; K0D2=K0/2
      G0=0
      DO L=1,NWL
         G0=G0+QAL(I0D2,J0D2,K0D2,L)
      ENDDO
      IF (IBCELL(I0,J0,K0).LE.0) THEN
         G0=G0/DX(I0)/DR(J0)/DZ(K0)/vol0
      ELSE
         G0=0
      ENDIF
      WRITE (nu_gfm,*) G0
   enddo;enddo;enddo

   !Note that some values here will not really be set the first time this routine
   !is called because they are set in the second section of the qrnf routine, but
   !this routine is called from the first section of qrnf. For subsequent calls to 
   !this routine, those values will be from the previous radiation interval 
   !calculation.
 
   WRITE(nu_gfm,*) QEG0 ! sum of qe for open cells
   WRITE(nu_gfm,*) QEG_G+QEW_G !sum of qal for open and wall cells
   WRITE(nu_gfm,*) QEW0 ! sum of qe for walls
                    ! Note qew0 is set after twall is called
                    !(but init to 0 in setup, and saved in radiation restart file) 
   WRITE(nu_gfm,*) QEG_W+QEW_W-QLS_G ! qal for walls + qalw0 for walls - convection from walls to interior
   WRITE(nu_gfm,*) QES0 !sum of qe for melt surfaces
   WRITE(nu_gfm,*) QEG_S+QEW_S !sum of qal and qalw0 for melt surfaces 
   CLOSE(nu_gfm)
endif

CALL PRN_SCR(3)  !print volume radiation display to screen for command window

END SUBROUTINE QRN1




!==============================================================================
!==============================================================================
!==============================================================================
SUBROUTINE QRN_DB

5 FORMAT (I5,' Total QE:QA (MW) =',-6PF10.6,F10.6,', CPU=',0PF8.2)

QESG=QEG0-QES0
QASG=SUM(QAL)-QES0
CPUS2=SECNDS(CPUS1)
!  WRITE (6,5) I0,QESG,QASG,CPUS2 !  3-23-2005: I0 is not a global

WRITE (6,5) QESG,QASG,CPUS2

END SUBROUTINE QRN_DB

!==============================================================================
!==============================================================================
!==============================================================================
SUBROUTINE QRN_PRN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
! 7/13/05: QRS is the heat flux at the melt surface
!Here it appears to be set to absorbtion/area
!rather than (absorbtion-emission)/area
!
!Fixed 7/20/05; emission subtracted off.
!
! qrs (heat flux to the melt) is now set in routine wallq
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! set qrs to be the melt surface heat flux
!DO I=2,MP,2; ID2=I/2
!DO J=2,NP,2
!   QRS(I,J)=zero
!   IF (IBCELL(I,J,2).NE.4) CYCLE
!   JD2=J/2
!   !G0=QA(ID2,JD2,1)
!   G0=QA(ID2,JD2,1)-QE(id2,jd2,1)
!   QRS(I,J)=G0/(DX(I)*R0*DR(J)*R0)
!ENDDO;ENDDO

if (cycling==1) call save_flux_change

!Put melt surface heat flux into it....m.dat file to pass to melt domain
FMR='(/2X,''  X  /  Y'',11(f11.3))'
filename=casedir//'\it'//runum//'m.dat'
open(nu_itm,file=filename)
itn=1

WRITE(nu_itm,*) ITN,"              Distribution indicator"
WRITE(nu_itm,*) avg_wall_T,"  Mean Wall Temperature in Combustion Space (K)"
WRITE(nu_itm,*) Tmax_c,"  Max Temperature in Combustion Space (K)"
WRITE(nu_itm,*) q_melt,"  Heat Transfer to Melter (W)"

WRITE(nu_itm,'(1X/T10,"Surface Radiation Heat Flux (W/m**2)")')

J1=2
!720   continue
do
   J2=J1+20
   J2=MIN(J2,NP)
   WRITE(nu_itm,FMR) (R(J),J=J1,J2,2)
   DO I=2,MP,2
      WRITE(nu_itm,'(F11.3,11E11.4)') X(I),(QRS(I,J),J=J1,J2,2)
   ENDDO
   J1=J2+2
   !IF (J1.LE.NP) GOTO 720
   if (j1>np) exit
enddo

CLOSE(nu_itm)
!Done writing it....m.dat file


G0=(QEG_S+QEW_S-QES0)*1.0D-6 !=(qal + qalw0 - qe) for melt surface cells 
!note qa_s0 and qa_s1 are saved in the radiation restart file, therefore:
QA_S1=G0-QA_S0 !current net radiation to melt surface - previous value
QA_S0=G0       !current net radiation to melt surface

if (iqaqe==1) then !debug print
   !write surface emission
   fmr='(/2x,''  x  /  y'',11(g11.3))'
   filename=casedir//'\qe'//runum//'c.dat'
   open(nu_qe,file=filename)
   write(nu_qe,'(1x/t10,"Surface Emission Heat Flux (W/m**2)")')
   jb=2
   do
      je=jb+20
      je=min(je,np)
      write(nu_qe,fmr) (r(j),j=jb,je,2)
      do i=2,mp,2
         write(nu_qe,'(f11.3,11e11.4)') x(i),(qe(i/2,j,1),j=jb/2,je/2,1)
      enddo
      jb=je+2
      if (jb > np) exit
   enddo
   close(nu_qe)

   !write surface absorption
   fmr='(/2x,''  x  /  y'',11(f11.3))'
   filename=casedir//'\qa'//runum//'c.dat'
   open(nu_qa,file=filename)
   write(nu_qa,'(1x/t10,"Surface Absorption Heat Flux (W/m**2)")')
   jb=2
   do
      je=jb+20
      je=min(je,np)
      write(nu_qa,fmr) (r(j),j=jb,je,2)
      do i=2,mp,2
         write(nu_qa,'(f11.3,11e11.4)') x(i),(qa(i/2,j,1),j=jb/2,je/2,1)
      enddo
      jb=je+2
      if (jb > np) exit
   enddo
   close(nu_qa)
endif !debug
return
END SUBROUTINE QRN_PRN

!======================================================================
!======================================================================
!     save_flux_change 
!
!     Want to be able to see development of melt surface heat flux when
!     doing automatic cycling. So create a file of changes to average
!     heat flux with a value for each cycle. 
!
!     If a previous version of the it....m.dat file exists, then 
!     calculate mean differences between previous and current melt
!     surface flux, and save in fchg....m.txt file. 
!
!     Only call this routine when have melt surface heat flux in the qrs
!     array and automatic cycling is active.
!======================================================================
subroutine save_flux_change
use gbl_var
implicit double precision (a-h,o-z)
real*8,allocatable :: last_qrs(:,:) 

!get previous flux from last it....m.dat file
filename=casedir//'\it'//runum//'m.dat'
inquire(file=filename,exist=exst)
if (exst) then
   allocate (last_qrs(mz,nz))
   open (nu_itm,file=filename)

   read (nu_itm,*) indicates_have_array
   if (indicates_have_array > 0) then
      !Have a file with flux array, 
      !otherwise only have default file initially so do not process.
      !Skip over items not needed    
      read (nu_itm,*) dontneed 
      read (nu_itm,*) dontneed
      read (nu_itm,*) dontneed
      !read (nu_itm,*) title !do not need to read in blank line due to way written
      read (nu_itm,*) title

      !Read in combustion space surface radiation heat flux into last_qrs array
      jb=1
      do
         je=jb+10
         je=min(je,nz)      
         read (nu_itm,'(A)') title      
         read (nu_itm,'(A)') title      
         do i=1,mz
            read (nu_itm,'(F11.3,11e11.4)') dontneed,(last_qrs(i,j),j=jb,je)
         enddo      
         jb=je+1
         if (jb > nz) exit
      enddo
      close(nu_itm)
      !Done reading previous surface heat flux.

      !Calculate average flux differences
      sum_dif=zero
      sum_rel_dif=zero
      sum_area=zero

      do i=2,mp,2
      do j=2,np,2
         if (ibcell(i,j,2).ne.4) cycle
         area = dx(i) * dr(j)
         sum_area = sum_area + area
         sum_dif = sum_dif + abs(qrs(i,j)-last_qrs(i/2,j/2))*area
         if (last_qrs(i/2,j/2)*area .ne. zero) then
            sum_rel_dif = sum_rel_dif + &
                          abs((qrs(i,j)-last_qrs(i/2,j/2))/last_qrs(i/2,j/2))*area
         endif
      enddo;enddo

      avg_flux_change = sum_dif/sum_area
      avg_rel_change = sum_rel_dif/sum_area

      !flux change file remains open
      if (iflx==1) write(nu_flx,*) itr_gas,avg_flux_change,avg_rel_change !save average changes in flux file
   else
         !do not have array in it....m.dat file
      close(nu_itm)
   endif
   deallocate (last_qrs)
endif
return
end subroutine save_flux_change

end
!          Not within Contains anymore because called from qwall  ! 12-8-05
!==============================================================================
!==============================================================================
!==============================================================================
! QRN2 - need a version of this for new wall calc to communicate with GUI  
!
! needs rewriting
! for now it is about the same except that radiation incident from
! the wall calcualtion is not available in qalw0 which no longer exists
! 
! qa no longer gets the sum of qal and qalw0
!
!    Reinitialize timer
!    Calc certain values
!    Create gfm.dat file to pass info to GUI for Wall Emission
!    Optionally, print screen display
!==============================================================================
SUBROUTINE QRN2
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

CPUS1=SECNDS(0.0)
!IF (IDEBUG.EQ.2) CALL QRN_DB !qrn_db is contained within qrnf, so cannot use

!------------------------------ 
! send data back to gui program

if (gui_update==1) then   
	filename=casedir//'\gfm.dat'
   open(nu_gfm,file=filename)
   WRITE(nu_gfm,"(I5)") ITN_R
   !WRITE(nu_gfm,"(I5)") LP-K+2
   WRITE(nu_gfm,"(I5)") 2                         
   WRITE(nu_gfm,"(E10.3)") FAC_W
   WRITE(nu_gfm,"('Wall Emission')")
   WRITE(nu_gfm,"('W/m**3')")
endif
QLS0=QLS0+QEW_IO !qls0 no longer used, was written in qrtmp file
QEW_G=0
QEW_S=0
QEW_IO=0
QEW_W=0
!Write out qal/volume for open cells, else write out 0
DO I0=2,MP,2; I0D2=I0/2
DO J0=2,NP,2; J0D2=J0/2
DO K0=2,LP,2; K0D2=K0/2
   G0=0
   G1=0
   DO L=1,NWL
      G1=G1+QAL(I0D2,J0D2,K0D2,L)
      !G0=G0+QALW0(I0D2,J0D2,K0D2,L) !walls handled in wallq
   ENDDO
   IF (IBCELL(I0,J0,K0).LE.0) THEN
      QEW_G=QEW_G+G0 !sum of qalw0 for open cells
   !ELSEIF (IBCELL(I0,J0,K0).LE.3) THEN
   ! 8/30/05   do not want to include wall cells marked with 1 here
   elseif (ibcell(i0,j0,k0)==2 .or. ibcell(i0,j0,k0)==3) then
      QEW_IO=QEW_IO+G0 !sum of qalw0 for inlets & exits
   ELSEIF (IBCELL(I0,J0,K0).LE.4) THEN
      QEW_S=QEW_S+G0 !sum of qalw0 for melt surface
   ELSE
      QEW_W=QEW_W+G0 !sum of qalw0 for walls
   ENDIF

   !For open cells write out qa/volume, for other cells write 0
   QA(I0D2,J0D2,K0D2)=G1+G0 ! sum of qal 
   
   if (gui_update==1) then   
      if (ibcell(i0,j0,k0).le.0) then
         g2=(g0+g1)/dx(i0)/dr(j0)/dz(k0)/vol0
      else
         g2=0
      endif
      write (nu_gfm,*) g2
   endif
enddo;enddo;enddo

if (gui_update==1) then   
   WRITE(nu_gfm,*) QEG0 ! sum of qe for open cells
   WRITE(nu_gfm,*) QEG_G+QEW_G !sum of qal for open and wall cells
   WRITE(nu_gfm,*) QEW0 ! sum of qe for walls
   WRITE(nu_gfm,*) QEG_W+QEW_W-QLS_G ! qal for walls + qalw0 for walls - convection from walls to interior
   WRITE(nu_gfm,*) QES0 !sum of qe for melt surfaces
   WRITE(nu_gfm,*) QEG_S+QEW_S !sum of qal and qalw0 for melt surfaces 
   CLOSE(nu_gfm)
endif

CALL PRN_SCR(4)  !print wall radiation display to screen for command window

return
end
