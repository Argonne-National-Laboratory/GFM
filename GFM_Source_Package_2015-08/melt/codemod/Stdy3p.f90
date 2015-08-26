! STDY3P.F90
!     to compute steady state three-phase flow
!     Rev: 4/02
!======================================================================
      SUBROUTINE STDY3P
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      logical prev_runstop_exist
      DATA IDIT2,LD2/0,0/
1     FORMAT(1X/T15,'TYPICAL ITERATION RESULT FOR TWO-PHASE FLOW', &
      ' FIELD'/T2,'LD',T11,'SMX',T20,'IMX,JMX,KMX',T35,'AVEB',/, &
      T8,'   U(I,J,K)   V(I,J,K)   W(I,J,K)   P(I,J,K) DNST(I,J,K)','  T(I,J,K)')
2     FORMAT(' D',I5,E12.4,3I4,E12.4)
3     FORMAT(T8,6E11.4)
!----------------------------------------------------------------------
!     LLK: number of iteration
!     SMX: maximal mass residual
!     AVEB: average mass residual
!     MAXSI: maximal allowable global iterations
!     MAXGI: maximal allowable glass phase iterations
!     BSCON: acceptable mass residual for a two-phase calculation
!     BGCON: acceptable mass residual for a glass phase calculation
!     BPCON: acceptable mass residual for a particle phase calculation
!     BBCON: acceptable mass residual for a bubble phase calculation
!     IDIT:  two-phase iterations/global iteration 
!----------------------------------------------------------------------
      N_BI=0
      IF (NPS0.LE.0) MAXPI=0
      IF (NBS0.LE.0) MAXBI=0
      IF (.NOT.STEADY) THEN
         DO I=2,MP
         DO J=2,NP
         DO K=2,LP
            !APO(I,J,K)=LG0(I,J,K)%TH*LG0(I,J,K)%DS*TRN/DTM
            APO(I,J,K)=LG0(I,J,K)%TH*LG0(I,J,K)%DS/DTM
         enddo;enddo;enddo
      ENDIF

      WRITE(NU,1) 
      MP_B=4
      MP_E=MPM2
      MP_E1=MP_E
      !NLG_RAD=ZERO ! comment out by Golchert, fall 2004
      m=0
      DO while (M < MAXSI); m=m+1
         NCONV=0
         CALL STDY3P_P !calls pflow, writes to info file
         CALL STDY3P_LQ !calls sphase
         IF (MS.EQ.1) CALL MSFLOW
         !cz       CALL STDY3P_BUB
         if (m.gt.20) CALL STDY3P_BUB
         !CSL     CALL STDY3P_RAD !call to radiation computations is commented out, Lottes note: 7-14-05
         !intv_stat=500
         !runstop_exist =.false.
         !if(mod(m,intv_stat)==0) CALL STDY3P_SCR !creates gfm.dat file
         !CALL STDY3P_SCR !creates gfm.dat file

         CALL STDY3P_SCR !checks for communication from GUI and creates gfm.dat file for GUI
         IF (NCONV.GE.3.OR.runstop_exist) EXIT !runstop_exist is true if runstop.dat file was found
      ENDDO

      !prev_runstop_exist=runstop_exist
      !CALL STDY3P_SCR !creates gfm.dat file
      !if (runstop_exist .or. prev_runstop_exist) then
      !   runstop_exist = .true.
      !endif
      RETURN


!====================================================================================
!====================================================================================
!====================================================================================
      CONTAINS
      SUBROUTINE STDY3P_P
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (MAXPI.LE.0) GOTO 200


      !Lottes 6/5/05
      !Compute batch surface coverage fraction functions
      !Note these should be size group based
      !but for now only one size group of sand or cullet is used.@
      if (nps_s > 0) area_s_spr=zero
      if (nps_c > 0) area_c_spr=zero
      coverf=zero
      ng=1
      do i=4,mp-2,2
      do j=4,np-2,2
         if (ibcell(i,j,Lp) .ne. 4) cycle

         theta_p=zero

         if (nps_s > 0) then 
            !area_s_spr(i/2,j/2) = ps(i/2,j/2,Lz-1,ng)%dn*pi*(rp_s(ng)*rlm0)**2/(dx(i)*dy(j)) 
            area_s_spr(i/2,j/2) = ps(i/2,j/2,Lz-1,ng)%dn*pi*rp_s(ng)**2/(dx(i)*dy(j)) 
            theta_p=theta_p+ps0(i/2,j/2,Lz-1)%th
         endif

         if (nps_c > 0) then
            !area_c_spr(i/2,j/2) = pc(i/2,j/2,Lz-1,ng)%dn*pi*(rp_c(ng)*rlm0)**2/(dx(i)*dy(j)) 
            area_c_spr(i/2,j/2) = pc(i/2,j/2,Lz-1,ng)%dn*pi*rp_c(ng)**2/(dx(i)*dy(j)) 
             theta_p=theta_p+pc0(i/2,j/2,Lz-1)%th
         endif

         coverf(i/2,j/2) = min((1-exp(-b_p*theta_p/th_pmx))/coverf_norm,one)

      enddo;enddo
      
      !Lottes 6/7/05, debug code copy number density into arrays than can be seen in debugger.
      do j=1,nz
      do i=1,mz
         if(nps_c>0) dn_c(i,j)=pc(i,j,Lz-1,ng)%dn
         if(nps_s>0) dn_s(i,j)=ps(i,j,Lz-1,ng)%dn
      enddo;enddo

      DO N=1,MAXPI
         IF (N_PI.GE.10000) N_PI=0
         N_PI=N_PI+1
         AVEB_P=0
         IF (NPS_C.GE.1) THEN
            NPS_1=NPS_C
            CALL PFLOW(1)
            AVEB_P=AVEB
         ENDIF
         IF (NPS_S.GE.1) THEN
            NPS_1=NPS_S
            CALL PFLOW(2)
            AVEB_P=MAX(AVEB_P,AVEB)
         ENDIF

         !Lottes 5/5/05: Calculate global energy distribution to surface
         !               Currently not used in other calculations
         !               Only for output to info summary file
         qrslg_tot = zero  ! Total radiation to surface liquid glass [W] !Lottes 5/5/05
         qrsp_c_tot = zero ! Total radiation to surface cullet [W] Lottes 5/5/05
         qrsp_s_tot = zero ! Total radiation to surface sand [W] Lottes 5/5/05

         do i=4,mp-2,2
         do j=4,np-2,2
            if (ibcell(i,j,lp).ne.4) cycle
            qrslg_tot = qrslg_tot + qrslg(i/2,j/2)
            do L=1,nps_c !Cycle through size groups
               qrsp_c_tot = qrsp_c_tot + qrsp_c(i/2,j/2,L)
            enddo                
            do L=1,nps_s !Cycle through size groups
               qrsp_s_tot = qrsp_s_tot + qrsp_s(i/2,j/2,L)
            enddo                
         enddo;enddo
         qglass_net=qrslg_tot+eb_heat-qcond_s_tot-qcond_c_tot-q_wall_loss_tot
         q_sand=qcond_s_tot+qrsp_s_tot
         q_cullet=qcond_c_tot+qrsp_c_tot
         qneed_bal=(h_needed-eb_heat+q_wall_loss_tot)/qrs_tot
         if(iInfo>0)then
           write(nu_info,"(1X,I5,8E22.14)") nlg,qglass_net,q_sand,q_cullet, &
                qrs_tot+eb_heat,q_wall_loss_tot,facq,facq_chg 
               !&           qglass_net+q_sand+q_cullet,q_wall_loss_tot 
               !      write(nu_info,"(1X,I5,7E25.14)") nlg,qrslg_tot,
               !&          qrsp_c_tot,qrsp_s_tot,
               !&          qcond_s_tot,qcond_c_tot,q_wall_loss_tot,qglass_net
         endif
         !Lottes 4/11/05: if sand and cullet both exist, aveb should be averaged @
         IF (AVEB_P.LE.BPCON.AND.N.LE.1) GOTO 200
      ENDDO
      RETURN

200   NCONV=NCONV+1
      RETURN
      END SUBROUTINE STDY3P_P


!====================================================================================
!====================================================================================
!====================================================================================
      SUBROUTINE STDY3P_LQ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (MAXGI.LE.0) GOTO 200
      CALL TH_GS
      !IF (.NOT.STEADY) CALL OSAVE
      LLK=0
      CALL SPHASE      
      AVEBG=AVEB
      IF (AVEB.GT.BGCON.OR.LLK.GT.1) RETURN
200   NCONV=NCONV+1
      RETURN
      END SUBROUTINE STDY3P_LQ


!====================================================================================
!====================================================================================
!====================================================================================
      SUBROUTINE STDY3P_BUB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (MAXBI.LE.0) GOTO 200
      DO N=1,MAXBI
         IF (N_BI.GE.10000) N_BI=0
         N_BI=N_BI+1
         AVEB_B=0
         NPS_1=NBS0
         CALL BFLOW(1)
         IF (AVEB.LE.BBCON) GOTO 200
      ENDDO
      RETURN
200   NCONV=NCONV+1
      RETURN
      END SUBROUTINE STDY3P_BUB


!====================================================================================
!====================================================================================
!====================================================================================
      SUBROUTINE STDY3P_RAD
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ! Change by Golchert fall 2004
      !NLG_RAD=NLG_RAD+1
      !IF (NLG_RAD.LT.LRX.OR.NWL.LE.0) RETURN
      !NLG_RAD=0

      IF (NLG.LT.LRX.OR.NWL.LE.0) RETURN

      CALL RAD0(1)
      CALL QRN
      !cz      DEALLOCATE (WL,AKL,DEB,DEB0)

      !cz    DEALLOCATE (QE,QEL,QEL0,QEL1,QA,QAL)
      DEALLOCATE (QEL,QEL0,QEL1,QAL)
      LRI=LRI+LRX
      RETURN
      END SUBROUTINE STDY3P_RAD


!====================================================================================
!====================================================================================
!====================================================================================
SUBROUTINE STDY3P_SCR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!Lottes 8/12/05 do not check for file, not checked in combustion.
!May be cause of not getting last iteration results.
!INQUIRE (FILE="GFM.DAT",EXIST=EXST)
!IF (.NOT.EXST) THEN 

filename=casedir//'\runstop.dat'
inquire (file=filename,exist=runstop_exist)
if (runstop_exist) then
   !user has requested to stop the program
   nfn1=delfilesqq(filename)  !delete file
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

if (gui_update==1) then
   !Create file to be used by GUI to update the temperature plot screen
   filename=casedir//'\gfm.dat'

   OPEN(nu_gfm,FILE=filename,ERR=160)
   WRITE(nu_gfm,"(I5)") NLG
   WRITE(nu_gfm,"(I5)") MAXSI*MAXGI+NLG0
   WRITE(nu_gfm,"(E10.3)") AVEBG
   WRITE(nu_gfm,"(E10.3)") BGCON
   WRITE(nu_gfm,*) FR2_LG
   WRITE(nu_gfm,*) PSA%FR2
   WRITE(nu_gfm,*) PCA%FR2
   !cbg      WRITE(nu_gfm,*) GFOUT   !6 December 2004
   WRITE(nu_gfm,*) GFEX
   WRITE(nu_gfm,*) PSA%MR
   WRITE(nu_gfm,*) PCA%MR
   !cbg        WRITE(nu_gfm,*) Qc_tot  !6 December 2004
   !          WRITE(nu_gfm,*) Q_COMB  !Lottes 7/17/05
   WRITE(nu_gfm,*) qrs_tot !Total radiation to surface  Lottes 7/17/05
   WRITE(nu_gfm,*) EB_HEAT
   if (nps_c>0) then !Lottes 4/11/05: make sure cullet exists
      !H_MC=PCA%MR*(PCA%CL*TM_C(1)+PCA%H0)
      udf => udf_clc                                              
      call udf_int(udf_clcn,tm_c(1),cullet_cl)
      h_mc=pca%mr*(cullet_cl*tm_c(1)+pca%h0)                
   else
      h_mc=zero
   endif
   if (nps_s>0) then !Lottes 4/11/05: make sure sand exists @@@ is this needed here.
      !H_MS=PSA%MR*(PSA%CL*TM_S(1)+PSA%H0)
      udf => udf_cls                                               
      call udf_int(udf_clsn,tm_s(1),sand_cl)
      h_ms=psa%mr*(sand_cl*tm_s(1)+psa%h0)                
   else
      h_ms=zero
   endif
   !          WRITE(nu_gfm,*) H_MC+H_MS-(H_TOT-Qc_tot) !Lottes 7/17/05
   !          WRITE(nu_gfm,*) H_EX !Lottes 7/17/05
   write(nu_gfm,*) q_sand+q_cullet !energy to solids  !Lottes 7/17/05
   write(nu_gfm,*) qrslg_tot !Total radiation to surface liquid glass [W]  !Lottes 7/17/05
   write(nu_gfm,*) q_wall_loss_tot !Total conduction heat loss through walls  !Lottes 7/17/05
   WRITE(nu_gfm,*) 'Temperature'
   WRITE(nu_gfm,*) 'K'
   DO I=2,MP,2
   DO J=2,NP,2
   DO K=2,LP,2
      WRITE (nu_gfm,*) LG(I,J,K)%T
   enddo;enddo;enddo
   160 continue
   CLOSE(nu_gfm)
endif

RETURN
END SUBROUTINE STDY3P_SCR
END


