
! MSFLOW.F90
!======================================================================
!     Calculation of sub-species transport and reaction in
!        the converged flowfield of a
!        two-dimensional multi-phase turbulent reacting flow in a
!        furnace using a lumped component
!     Revision: 9/01
!======================================================================
SUBROUTINE MSFLOW          
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
integer just_did_ms_prn_scr
REAL*8, ALLOCATABLE :: GFM_0(:,:)

just_did_ms_prn_scr=0
40    FORMAT(1X/T15,'TYPICAL ITERATION RESULT FOR SUB-SPECIES FLOW',' FIELD'/T2,'ITR',T11,'Res',T20,'GFM')
44    FORMAT(I7,3G11.4,3G15.5)
45    FORMAT(T8,7G10.3)
99    FORMAT(I2,E15.6,F12.1)
!----------------------------------------------------------------------
!     MAXMS: maximal allowable sub-species iterations
!     BMCON: acceptable residual for a sub-species calculation
!     K=1: iner gas for reference
!----------------------------------------------------------------------
IF (IDEBUG.EQ.2) THEN
   WRITE(NU,40)
ENDIF
call flx1dm  
ALLOCATE (AP1(MP,NP,LP),GFM_0(NSP0,2))
GFM_0=0
GFMEX=0
DO L=1,NSP0
   GFM_0(L,1)=FLXM(5,L,2)+FLXM(MP-5,L,1)
   GFMEX=GFMEX+GFM_0(L,1)
ENDDO

!----------------------------------------------------------------------
!     BEGIN ITERATION
!        GSLV3M: solve sub-species governing equations
!----------------------------------------------------------------------
! 11/7/05  The following 3 statements are not needed
!DATA SMX1,II,KK,IX/1.0D-7,1,1,10/
!L2=0
!MSE_0=2#1111111 !127
CPUS1=SECNDS(0.0) !initialize timer
igs0=0 !flag used to indicate coeffs need to be calculated in gslv3m
DO L1=1,MAXMS
   itr_ms=itr_ms+1
   !CALL GSLV3M(MSE_0)
   CALL GSLV3M
   call flx1dm
   GFMEX=0
   DO L=1,NSP0
      GFM_0(L,2)=FLXM(5,L,2)+FLXM(MP-3,L,1)+FLYM(5,L,2)+FLYM(NP-3,L,1)
      GFMEX=GFMEX+GFM_0(L,2)
   ENDDO
   S_O2=GFM_0(2,2)/GFIN*100
   S_CO=GFM_0(3,2)/GFIN
   S_H2O=GFM_0(4,2)/GFIN*100
   S_CO2=GFM_0(5,2)/GFIN*100
   S_N2=GFM_0(6,2)/GFIN*100
   S_NO=GFM_0(7,2)/GFIN
   !Calculate area-average soot volume fraction at exits 
   SVF_R=0
   G1=0
   !smf_max=zero
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      !if (ibcell(i,j,k)==0) then
	  !	 if (smf(i,j,k) > smf_max) smf_max=smf(i,j,k)
	  !endif
      IF (IBCELL(I,J,K).NE.3) CYCLE
      G0=AREA_C(I,J,K,3)
      IF (G0.LT.small20) G0=AREA_C(I,J,K,2)
      IF (G0.LT.small20) G0=AREA_C(I,J,K,1)
      G1=G1+G0
      SVF_0=smf(I,J,K)*DNST(I,J,K)*DNST0/dnst_soot
      SVF_R=SVF_R+SVF_0*G0
   enddo;enddo;enddo
   SVF_R=SVF_R/G1

   ave_chg=0
   DO L=1,NSP0
      ave_chg=ave_chg+ABS(GFM_0(L,2)-GFM_0(L,1))
      GFM_0(L,1)=GFM_0(L,2)
   ENDDO
   ave_chg=ave_chg/NSP0

   IF (IDEBUG.EQ.2) THEN         
      WRITE(NU,44) itr_ms,ave_chg,GFIN,GFMEX
      IF (GFMEX.GT.small20) WRITE(NU,45) (GFM_0(L,1)/GFIN,L=1,NSP0)
   ENDIF
   
   !----------------------------------
   !write subspecies residuals to file
   if (imresid>0) then
      !if (imsconv>0) write(nu_mscon,"(i7,20e25.16)") itr_ms,bal_ms,(resid_ms(l),l=1,nsp0+1)
      write(nu_mres,"(i7,8e25.16)")  itr_ms,(resid_ms(l),l=1,nsp0+1)
   endif
   if (imresidp>0) then
      write(nu_mresp,"(i7,8e25.16)") itr_ms,(resid_ms_pre(l),l=1,nsp0+1)
   endif

   imscon=0   !new convergence check
   if (itr_ms>10) then
      imscon=1
      do l=1,nsp0+1; if(resid_ms(l) > bmcon) imscon=0; enddo
   endif
   !FACM=(GFIN-GFMEX)/GFIN ! facm is not used
   IF (imscon==1) EXIT

   !csl            FILENAME='Y'//RUNUM//'.OUT'
   !csl        OPEN (12,FILE=FILENAME)
   !csl            DO 240 L2=1,NSP0
   !csl               WRITE(12,44) L2,GFM_0(L2,1)
   !csl240        CONTINUE
   !csl            CLOSE(12)
   !csl            IF (IX.EQ.0) THEN
   !csl               FILENAME='CK'//RUNUM//'.OUT'
   !csl           OPEN (12,FILE=FILENAME)
   !csl            WRITE (12,99) (I,CK0_M(I),ODR(I,1:2),ERC(I),I=1,NSP0)
   !csl            CLOSE(12)
   !csl         ENDIF
   !csl            EXIT
   !csl         ENDIF
   call extrm
   call intpm

   CPUS2=SECNDS(CPUS1) ! get elapsed time since timer set
   IF (CPUS2.GT.20) THEN
      ! the timer has run for more than 20 seconds
        
      CALL PRN_SCR(2)  !print minor species display to screen for command window

      !---------------------------------------------------
      ! Check for communication from GUI

      filename=casedir//'\runstop.dat'
      inquire (file=filename,exist=runstop_exist)
      if (runstop_exist) then
         !user has requested to stop the program
         !nfn1=delfilesqq(filename)  !delete file
         exit ! return to main routine to write files and stop program
      endif

      filename=casedir//'\gui_update.txt'
      inquire (file=filename,exist=exst)
      if (exst) then
         !user has requested change in gui update status
         open(nu_guiup,file=filename)
         read (nu_guiup,*) gui_update
         close(nu_guiup)
         nfn1=delfilesqq(filename) !delete file
      endif
      CPUS1=SECNDS(0.0) !reinitialize timer
      just_did_ms_prn_scr=1
   else
      just_did_ms_prn_scr=0
   endif
enddo

if (just_did_ms_prn_scr==0) call prn_scr(2)  !print minor species display to screen for command window
imsflow_done=1
!csl      CALL GSLV3M(-1)
DEALLOCATE (AP1,GFM_0)
RETURN
END

