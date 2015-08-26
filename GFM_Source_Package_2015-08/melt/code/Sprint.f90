!======================================================================
!======================================================================
!======================================================================
! SPRINT.F90
!     to print computed results of the system state of a two-phase flow
!        7/01
!======================================================================
      SUBROUTINE SPRINT
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8,ALLOCATABLE :: PRNS(:,:)

      FMR='(/2X,''  X  /  Y  '',11(G13.5))'
      FMX='(G13.5,11E13.5)'
      FMZ="(/' Z(',I3,') = ',G14.5)"

      !FMR='(/2X,'' X  /  Y   '',11(G13.5))'
      !FMZ='(/2X,'' X  /  Z   '',11(G13.5))'
      !FMX='(G13.5,11G13.5)'
      filename=casedir//'\rt'//runum//'m.out'
      OPEN(nu_rt,FILE=FILENAME)
      CALL HPRINT
      !UG0B=UG0*3.2808399D+0 !Lottes 4/19/05: Unit conversion to ft/s
!----------------------------------------------------------------------
!     Combustion flow properties at boundaries
!        RES: residuals computed in the equation solver ADLBL
!----------------------------------------------------------------------
      IF (IDEBUG.EQ.1) THEN
         WRITE(20,*)' '
         WRITE(20,*)' Equation    Max Residual    Ave Residual'
         DO JR=1,13
            WRITE(20,*) JR,'    ',RESG(JR,1),'    ',RESG(JR,2)
         ENDDO
      ENDIF
!----------------------------------------------------------------------
!     Print title
!----------------------------------------------------------------------
200   IF (STEADY) THEN
         WRITE(nu_rt,221)
      ELSE
         WRITE(nu_rt,222)ISTEP,TM
      ENDIF
221   FORMAT(/5X,'STEADY-STATE RESULTS OVER FLOWFIELD')
222   FORMAT(/5X,'RESULTS OVER FLOWFIELD',5X,'TIME STEP =',I3,' TIME=',F7.3)
!----------------------------------------------------------------------
!     Print Glass flow properties
!----------------------------------------------------------------------
      IF (IDEBUG.EQ.1) THEN
         NX=1
      ELSE
         NX=2
      ENDIF

      !DO 240 K=LP,2,-NX
      !   IF (K.GE.LP-2) THEN
      !      G0=0
      !   ELSE
      !      G0=G0-DS0*GRA(3)*(Z(K+1)-Z(K))
      !   ENDIF
      !   DO 240 I=2,MP,NX
      !   DO 240 J=2,NP,NX
      !      BS(I,J,K)=LG(I,J,K)%P+G0
      !240   CONTINUE
      !WRITE(nu_rt,301)PG0,ISTEP
      !CALL PFORM(BS,NX)

      !guage pressure
   if(ialtbuoy==1) then
   	do k=2,lp,nx
   	do j=2,np,nx
   	do i=2,mp,nx
   	   bs(i,j,k)= lg(i,j,k)%p + lg(i,j,k)%ds*gra(3)*(z(lp)-z(k))
   	enddo;enddo;enddo
   	write(nu_rt,301)pg0,istep
   	call pform(bs,nx)

      !dynamic pressure in alternate buoyance formulation
      bs=lg%p
   	write(nu_rt,309)pg0,istep
	   call pform(bs,nx)
   else
      !gage pressure pressure in standard buoyance formulation
      bs=lg%p
   	write(nu_rt,301)pg0,istep
	   call pform(bs,nx)
	endif

      BS=LG%T
      WRITE(nu_rt,303)ISTEP
      CALL PFORM(BS,NX)
      !IF (IDEBUG.EQ.1) THEN
         !WRITE(nu_rt,302)DNST0,ISTEP
         WRITE(nu_rt,302)ISTEP
         BS=LG%DS
         CALL PFORM(BS,NX)
         !WRITE(nu_rt,304)CP0*(T0-TL0),ISTEP Lottes 5/6/05
         !WRITE(nu_rt,304)CP0*T0,ISTEP
         WRITE(nu_rt,304)ISTEP
         BS=LG%H
         CALL PFORM(BS,NX)
         !WRITE(nu_rt,305)GMU0,ISTEP
         !BS=LG%MU*GMU0
         WRITE(nu_rt,305)ISTEP
         BS=LG%MU
         CALL PFORM(BS,NX)
      !ENDIF
      !WRITE(nu_rt,306)UG0,UG0B,ISTEP
      WRITE(nu_rt,306)ISTEP
      CALL PFORMU(1,NX)
      !WRITE(nu_rt,307)UG0,UG0B,ISTEP
      WRITE(nu_rt,307)ISTEP
      CALL PFORMU(2,NX)
      !IF (LP.GE.6) THEN
         !WRITE(nu_rt,308)UG0,UG0B,ISTEP
         WRITE(nu_rt,308)ISTEP
         CALL PFORMU(3,NX)
      !ENDIF
      DO 250 I=2,MP,2
      DO 250 J=2,NP,2
      DO 250 K=2,LP,2
         G0=LG(I,J,K)%U(1)**2+LG(I,J,K)%U(2)**2+LG(I,J,K)%U(3)**2
         BS(I,J,K)=SQRT(G0)
250   CONTINUE
      !CSL      WRITE(nu_rt,309)UG0,UG0B,ISTEP
      !CSL      CALL PFORM(BS,NX)
301   FORMAT(/5X,'"Pressure, PG0 =',G11.4,' pa,TSTEP',I3)
309   FORMAT(/5X,'"dynamic P, PG0 =',G11.4,' pa,TSTEP',I3)
!302   FORMAT(/5X,'"Density, DNST(I,J,K)/DNST0, DNST0 =',G11.4,' kg/m**3,TSTEP',I3)
302   FORMAT(/5X,'"Density, kg/m**3,TSTEP',I3)
303   FORMAT(1X/T10,'"Temp, TG(I,J,K), (K), TIME STEP ',I3)
!304   FORMAT(1X/T10,'"Enthalpy, H(I,J)/HG0, HG0 =',G11.4,' J/kg, TSTEP',I3)
304   FORMAT(1X/T10,'"Enthalpy, J/kg, TSTEP',I3)
!305   FORMAT(1X/T10,'"Glass Viscosity, GMU(I,J,K)/GMU0, Gmu0 =',G11.4,' Pa-s, TSTEP',I3)
305   FORMAT(1X/T10,'"Glass Viscosity, Pa-s, TSTEP',I3)
!306   FORMAT(1X/T10,'"X-VELOCITY/UG0,INTERPOLATED CELL CENTER UG0 =',G11.4,' (m/s) =',G11.4,' (ft/s), TSTEP',I3)
306   FORMAT(1X/T10,'"X-Velocity, INTERPOLATED CELL CENTER, TSTEP',I3)
!307   FORMAT(1X/T10,'"Y-VELOCITY/UG0, INTERPOLATED CELL CENTER  UG0=',G11.4,' (m/s) =',G11.4,' (ft/s), TIME STEP',I3)
307   FORMAT(1X/T10,'"Y-Velocity, INTERPOLATED CELL CENTER, TIME STEP',I3)
!308   FORMAT(1X/T10,'"Z-VELOCITY/UG0, UG0=',G11.4,' (m/s) =',G11.4,' (ft/s), TIME STEP',I3)
308   FORMAT(1X/T10,'"Z-Velocity, TIME STEP',I3)
      !CSL309   FORMAT(1X/T10,'"VELOCITY, U/UG0, UG0=',G11.4,' (m/s) =',
      !CSL     & G11.4,' (ft/s), TIME STEP',I3)
      !CZEB---------------------
      IF (EB_C) THEN
         !cz      WRITE(nu_rt,401) EBVP0
      WRITE(nu_rt,401) 
      CALL PFORM(EBV,NX)
         !cz      WRITE(nu_rt,402) 
         !cz      CALL PFORM(EBE,NX)
         !cz      WRITE(nu_rt,403) 
         !cz      CALL PFORM(EBJ,NX)
         !cz      WRITE(nu_rt,404) 
      CALL PFORM(EBSG,NX)
      WRITE(nu_rt,405) 
      CALL PFORM(EBq,NX)
         !cz      WRITE(nu_rt,406) 
         !cz      CALL PFORM(EBI,NX)
      ENDIF
401   FORMAT(/5X,'"EBV Electric Potential Energy (V)"')
      !cz402   FORMAT(/5X,'"EBE Electric Potential (V/m)"')
      !cz403   FORMAT(/5X,'"EBJ Electric Current Density (A/m**2)"')
404   FORMAT(/5X,'"EBSG Electric Conductivity (1/ohm-m)"')
405   FORMAT(/5X,'"EBQ Joulian heat (W)"')
      !cz406   FORMAT(/5X,'"EBI Electric Current (A)"')
!----------------------------------------------------------------------
      !Lottes, @ note qrs is divided by area here to get heat flux back
      !if this print is removed, qrs needs to be divided by area below
      ALLOCATE (PRNS(MZ,NZ))
      K=LPM1
      DO I=2,MP-2,2
      ID2=I/2
      DO J=2,NP,2
         JD2=J/2
         IBC0=IBCELL(I,J,K)
         A0=AREA_C(I,J,K,3)
         !CBG     IF (IBC0.NE.304.OR.A0.LE.SMALL) CYCLE
         IF (IBC0.NE.4.OR.A0.LE.SMALL) CYCLE
         QRS(ID2,JD2)=QRS(ID2,JD2)/A0
         QRSLG(ID2,JD2)=QRSLG(ID2,JD2)/A0
         DO L=1,NPS_C
            QRSP_C(ID2,JD2,L)=QRSP_C(ID2,JD2,L)/A0
         ENDDO
         DO L=1,NPS_S
            QRSP_S(ID2,JD2,L)=QRSP_S(ID2,JD2,L)/A0
         ENDDO
      ENDDO
      ENDDO
      PRNS=QRS
      WRITE(nu_rt,"(1X/T10,'""Heat_rad, Rad Heat Flux, (W/m**2), surface')")
      CALL SPXZ
      IF (NPHAS.EQ.1) GOTO 550
!----------------------------------------------------------------------
!      IF (IDEBUG.EQ.1) THEN
!         WRITE(nu_rt,"(1X/T10,'""Gla Rad Heat, (W/m**2), surface')")
!         PRNS=QRSLG
!         CALL SPXZ
!         WRITE(nu_rt,"(/5X,'""TH_g, Glass Vol Frac, TSTEP',I3)")ISTEP
!         BS=LG%TH
!         CALL PFORM(BS,NX)
!         WRITE(nu_rt,*) '          "Solid Stress"'
!         CALL PFORM(SP,NX)
!      ENDIF
!----------------------------------------------------------------------
!     Print 2D cullet flow properties
!----------------------------------------------------------------------
      IF (PCA%FR2.LE.ZERO.or.nps_c<1) GOTO 450 !Lottes: make sure we have some
      WRITE(nu_rt,"(1X/T10,'Cullet Flow Properties, TSTEP',I3)")ISTEP
      K=LP-2
      KD2=LZ-1
      DO I=2,MP-2,2
      ID2=I/2
      DO J=2,NP,2
         JD2=J/2
         PRNS(ID2,JD2)=PC0(ID2,JD2,KD2)%TH
         IF (IBCELL(I,J,K).GT.0) CYCLE
         VOL=VOL_C(I,J,K)
         AREA=AREA_C(I,J,K,3)
         IF (AREA.LT.SMALL) CYCLE
         PC0(ID2,JD2,KD2)%MR=PC0(ID2,JD2,KD2)%MR*vol/AREA !convert melt rate to area basis 8-10-06 Lottes
         DO L=0,NPS_C
            QCD_C(ID2,JD2,L)=QCD_C(ID2,JD2,L)/AREA
         ENDDO
      ENDDO
      ENDDO
      WRITE(nu_rt,"(1X/T10,'""TH_c, Cullet Vol Fraction, surface')")
      CALL SPXZ
      DO ID2=1,MZ
      DO JD2=1,NZ
         PRNS(ID2,JD2)=PC0(ID2,JD2,KD2)%MR
      ENDDO
      ENDDO
      WRITE(nu_rt,"(1X/T10,'""Melt_c, Cul Melt Rate, (kg/s/m**2), surface')")
      CALL SPXZ


!---------------------------------------------skipped output
      IF (IDEBUG.EQ.0) GOTO 450

      DO ID2=1,MZ
      DO JD2=1,NZ
         PRNS(ID2,JD2)=QCD_C(ID2,JD2,0)
      ENDDO
      ENDDO
      WRITE(nu_rt,"(1X/T10,'""Cul Cond Heat, (W/m**2), surface')")
      CALL SPXZ
      DO L=1,NPS_C
      !WRITE(nu_rt,443) L,RP_C(L),TM_C(L)*T0
      WRITE(nu_rt,443) L,RP_C(L),TM_C(L)
      DO M=1,6
         K=LP-2
         KD2=LZ-1
         IF (M.EQ.1.AND.NPS_C.GT.1) WRITE(nu_rt,444) L
         IF (M.EQ.2.AND.NPS_C.GT.1) WRITE(nu_rt,445) L
         !IF (M.EQ.3) WRITE(nu_rt,446) L,DND0
         IF (M.EQ.3) WRITE(nu_rt,446) L
         IF (M.EQ.4) WRITE(nu_rt,447) L
         IF (M.EQ.5) WRITE(nu_rt,448) L
         IF (M.EQ.6) WRITE(nu_rt,449) L
         !IF (M.EQ.5) WRITE(nu_rt,448) L,UG0,UG0B
         !IF (M.EQ.6) WRITE(nu_rt,449) L,UG0,UG0B
         J1=1
         J2=J1+10
         J2=MIN(NZ,J2)
         DO WHILE (J1.LE.NZ)
            IF (M.GT.2.OR.NPS_C.GT.1) THEN
               WRITE(nu_rt,FMR) (y(J),J=J1*2,J2*2,2)
            ENDIF
            DO ID2=1,MZ
            I=ID2*2
            IF (M.EQ.1.AND.NPS_C.GT.1) THEN !@@@@@@ why .gt. 1 ????????
               WRITE(nu_rt,FMX) X(I),(QRSP_C(ID2,JD2,L),JD2=J1,J2)
            ELSEIF (M.EQ.2.AND.NPS_C.GT.1) THEN
               WRITE(nu_rt,FMX) X(I),(QCD_C(ID2,JD2,L),JD2=J1,J2)
            ELSEIF (M.EQ.3) THEN
               WRITE(nu_rt,FMX) X(I),(PC(ID2,JD2,KD2,L)%DN,JD2=J1,J2)
            ELSEIF (M.EQ.4) THEN
               WRITE(nu_rt,FMX) X(I),(PC(ID2,JD2,KD2,L)%T,JD2=J1,J2)
            ELSEIF (M.EQ.5) THEN
               WRITE(nu_rt,FMX) X(I),(PC(ID2,JD2,KD2,L)%U(1),JD2=J1,J2)
            ELSEIF (M.EQ.6) THEN
               WRITE(nu_rt,FMX) X(I),(PC(ID2,JD2,KD2,L)%U(3),JD2=J1,J2)
            ENDIF
            ENDDO
            J1=J2+1
         ENDDO
      ENDDO
      ENDDO
443   FORMAT(1X/T20,'Cullet Group',I2,', Size=',F6.3,' TMLT=',F6.1,' K')
444   FORMAT(1X/T10,'Cul'I2' Rad Heat,   (W/m**2), surface')
445   FORMAT(1X/T10,'Cul'I2' Cond Heat,   (W/m**2), surface')
!446   FORMAT(1X/T10,'Cul'I2' # Dens, DND0='G11.4' #/m**3, surface')
446   FORMAT(1X/T10,'Cul',I2,' # Dens, #/m**3, surface')
447   FORMAT(1X/T10,'Cul',I2,' Temp, (K), surface')
!448   FORMAT(1X/T10,'Cul',I2,' X-Vel, UG0 =',G11.4,' m/s =',G11.4,' ft/s, surface')
448   FORMAT(1X/T10,'Cul',I2,' X-Vel, m/s, surface')
!449   FORMAT(1X/T10,'Cul'I2' Y-Vel, UG0 =',G11.4,' m/s =',G11.4,' ft/s, surface')
449   FORMAT(1X/T10,'Cul',I2,' Y-Vel,  m/s, surface')
!---------------------------------------------end skipped output


!----------------------------------------------------------------------
!     Print batch flow properties in 2D format - can't be handled by all postprocessors
!----------------------------------------------------------------------
450   IF (PSA%FR2.LE.ZERO.or.nps_s<1) GOTO 500 !Lottes: make sure we have some
      WRITE(nu_rt,"(1X/T10,'Batch Flow Properties, TSTEP',I3)")ISTEP

      k=lp-2
      KD2=LZ-1
      DO I=2,MP-2,2
      ID2=I/2
      DO J=2,NP,2
         JD2=J/2
         PRNS(ID2,JD2)=PS0(ID2,JD2,KD2)%TH
         IF (IBCELL(I,J,K).GT.0) CYCLE
         VOL=VOL_C(I,J,K)
         AREA=AREA_C(I,J,K,3)
         IF (AREA.LT.SMALL) CYCLE
         PS0(ID2,JD2,KD2)%MR=PS0(ID2,JD2,KD2)%MR*vol/AREA !convert melt rate to area basis
         DO L=0,NPS_S
            QCD_S(ID2,JD2,L)=QCD_S(ID2,JD2,L)/AREA
         ENDDO
      ENDDO
      ENDDO
      WRITE(nu_rt,"(1X/T10,'""TH_s, Sand Vol Fraction, surface')")
      CALL SPXZ
      DO ID2=1,MZ
      DO JD2=1,NZ
         PRNS(ID2,JD2)=PS0(ID2,JD2,KD2)%MR
      ENDDO
      ENDDO
      WRITE(nu_rt,"(1X/T10,'""Melt_s, SandMelt Rate, (kg/s/m**2), surface')")
      CALL SPXZ


      ! Skipped output------------------------------------------------------------------------
      IF (IDEBUG.EQ.0) GOTO 500
      WRITE(nu_rt,"(1X/T10,'""SandCond Heat, (W/m**2), surface')")
      DO ID2=1,MZ
      DO JD2=1,NZ
         PRNS(ID2,JD2)=QCD_S(ID2,JD2,0)
      ENDDO
      ENDDO
      DO L=1,NPS_S
      !WRITE(nu_rt,461) L,RP_S(L),TM_S(L)*T0
      WRITE(nu_rt,461) L,RP_S(L),TM_S(L)
      DO M=1,6
         K=LP-2
         KD2=LZ-1
         IF (M.EQ.1.AND.NPS_S.GT.1) WRITE(nu_rt,462) L
         IF (M.EQ.2.AND.NPS_S.GT.1) WRITE(nu_rt,463) L
         !IF (M.EQ.3) WRITE(nu_rt,464) L,DND0
         IF (M.EQ.3) WRITE(nu_rt,464) L
         IF (M.EQ.4) WRITE(nu_rt,465) L
         IF (M.EQ.5) WRITE(nu_rt,466) L
         IF (M.EQ.6) WRITE(nu_rt,467) L
         !IF (M.EQ.5) WRITE(nu_rt,466) L,UG0,UG0B
         !IF (M.EQ.6) WRITE(nu_rt,467) L,UG0,UG0B
         J1=1
         DO WHILE (J1.LE.NZ)
            J2=J1+10
            J2=MIN(NZ,J2)
            IF (M.GT.2.OR.NPS_S.GT.1) THEN
               WRITE(nu_rt,FMR) (y(J),J=J1*2,J2*2,2)
            ENDIF
            DO ID2=1,MZ
            I=ID2*2
            IF (M.EQ.1.AND.NPS_S.GT.1) THEN
               WRITE(nu_rt,FMX) X(I),(QRSP_S(ID2,JD2,L),JD2=J1,J2)
            ELSEIF (M.EQ.2.AND.NPS_S.GT.1) THEN
               WRITE(nu_rt,FMX) X(I),(QCD_S(ID2,JD2,L),JD2=J1,J2)
            ELSEIF (M.EQ.3) THEN
               WRITE(nu_rt,FMX) X(I),(PS(ID2,JD2,KD2,L)%DN,JD2=J1,J2)
            ELSEIF (M.EQ.4) THEN
               WRITE(nu_rt,FMX) X(I),(PS(ID2,JD2,KD2,L)%T,JD2=J1,J2)
            ELSEIF (M.EQ.5) THEN
               WRITE(nu_rt,FMX) X(I),(PS(ID2,JD2,KD2,L)%U(1),JD2=J1,J2)
            ELSEIF (M.EQ.6) THEN
               WRITE(nu_rt,FMX) X(I),(PS(ID2,JD2,KD2,L)%U(2),JD2=J1,J2)
            ENDIF
            ENDDO
            J1=J2+1
         ENDDO
      ENDDO
      ENDDO
461   FORMAT(1X/T20,'Sand Group',I2,', Size=',F6.3,' TMLT=',F6.1,' K')
462   FORMAT(1X/T10,'Sand'I2' Rad Heat,   (W/m**2), surface')
463   FORMAT(1X/T10,'Sand'I2' Cond Heat,   (W/m**2), surface')
!464   FORMAT(1X/T10,'Sand'I2' # Dens, DND0='G11.4' #/m**3, surface')
464   FORMAT(1X/T10,'Sand',I2,' # Dens, #/m**3, surface')
465   FORMAT(1X/T10,'Sand'I2' Temp, (K), surface')
!466   FORMAT(1X/T10,'Sand'I2' X-Vel, UG0 =',G11.4,' m/s =',G11.4,' ft/s, surface')
466   FORMAT(1X/T10,'Sand',I2,' X-Vel, m/s, surface')
!467   FORMAT(1X/T10,'Sand'I2' Y-Vel, UG0 =',G11.4,' m/s =',G11.4,' ft/s, surface')
467   FORMAT(1X/T10,'Sand',I2,' Y-Vel, m/s, surface')

! End Skipped output------------------------------------------------------------------------


500   continue
!----------------------------------------------------------------------
!     Print batch flow properties
!----------------------------------------------------------------------
      IF (PsA%FR2 > ZERO .and. nps_s >0) then !Lottes: make sure we have some
         WRITE(nu_rt,"(1X/T10,'Batch Flow Properties, TSTEP',I3)")ISTEP
   
         !Lottes 4/21/05: Print full sand volume fraction field
         write(nu_rt,"(1X/T10,'""Batch_VF, Batch Vol Fraction')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            bs(i,j,k)=ps0(i/2,j/2,k/2)%th
         enddo;enddo;enddo
         call pform(bs,2)
         
         !Lottes 4/21/05: Print full sand number density field
         write(nu_rt,"(1X/T10,'""Batch_#dn, Batch Vol Fraction')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            !bs(i,j,k)=ps(i/2,j/2,k/2,1)%dn*dnd0
            bs(i,j,k)=ps(i/2,j/2,k/2,1)%dn
         enddo;enddo;enddo
         call pform(bs,2)
   
         !Lottes 4/21/05: Print full sand number density field
         write(nu_rt,"(1X/T10,'""Batch_T, Batch Temperature')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            !bs(i,j,k)=ps(i/2,j/2,k/2,1)%dn*dnd0
            bs(i,j,k)=ps(i/2,j/2,k/2,1)%T
         enddo;enddo;enddo
         call pform(bs,2)
   
         !Lottes 4/21/05: Print full melt rate field
         write(nu_rt,"(1X/T10,'""Batch_MR, Batch Melt Rate')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            bs(i,j,k)=ps0(i/2,j/2,k/2)%mr
         enddo;enddo;enddo
         call pform(bs,2)
   
      endif
      !---------------------------------------------------------------




!----------------------------------------------------------------------
!     Print Cullet flow properties
!----------------------------------------------------------------------
      IF (PcA%FR2 > ZERO .and. nps_c >0) then !Lottes: make sure we have some
         WRITE(nu_rt,"(1X/T10,'Cullet Flow Properties, TSTEP',I3)")ISTEP
   
         !Lottes 4/21/05: Print full cullet volume fraction field
         write(nu_rt,"(1X/T10,'""Cullet_VF, Cullet Vol Fraction')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            bs(i,j,k)=pc0(i/2,j/2,k/2)%th
         enddo;enddo;enddo
         call pform(bs,2)
   
         !Lottes 4/21/05: Print full cullet number density field
         write(nu_rt,"(1X/T10,'""Cullet_#dn, Cullet Vol Fraction')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            !bs(i,j,k)=pc(i/2,j/2,k/2,1)%dn*dnd0
            bs(i,j,k)=pc(i/2,j/2,k/2,1)%dn
         enddo;enddo;enddo
         call pform(bs,2)
   
         !Lottes 4/21/05: Print full cullet number density field
         write(nu_rt,"(1X/T10,'""Cullet_T, Cullet Vol Fraction')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            !bs(i,j,k)=pc(i/2,j/2,k/2,1)%dn*dnd0
            bs(i,j,k)=pc(i/2,j/2,k/2,1)%T
         enddo;enddo;enddo
         call pform(bs,2)
   
         !Lottes 4/21/05: Print full melt rate field
         write(nu_rt,"(1X/T10,'""Cullet_MR, Cullet Melt Rate')")
         do i=2,mp,2
         do j=2,np,2
         do k=2,lp,2
            bs(i,j,k)=pc0(i/2,j/2,k/2)%mr
         enddo;enddo;enddo
         call pform(bs,2)
   
      endif

!----------------------------------------------------------------------
!Lottes 6/23/05: Print surface heat flux as 3d for post proc
!----------------------------------------------------------------------

      write(nu_rt,"(1X/T10,'""Surf Heat, Surface heat flux (W/m^2)')")
      do k=2,lp,2
      do j=2,np,2
      do i=2,mp,2
         if (k > lp-4) then
            !bs(i,j,k)=qrs(i/2,j/2)/(facq*area_c(i,j,lp,3))
            !bs(i,j,k)=qrs(i/2,j/2)/(facq)
            bs(i,j,k)=qrs(i/2,j/2)/(area_c(i,j,lp,3))
         else
            bs(i,j,k)=zero
         endif
      enddo;enddo;enddo
      call pform(bs,2)

      !Lottes @ debug print
      iskip=1
      if (itn > 0.and.iskip==0) then
         !allocate (qrs_m(mz,nz))
         iqrsc_print=1
         if (iqrsc_print == 1) then
            do i=2,i_me,2
            do j=2,np,2
               if (mod(ibcell(i,j,lp),10)==4 .and. ibcell(i,j,lp-2) < 1) then
                  !qrs_m(i/2,j/2)=qrs(i/2,j/2)/(facq*area_c(i,j,lp,3))
                  qrs_m(i/2,j/2)=qrs(i/2,j/2)/(facq)
               else
                  qrs_m(i/2,j/2)=0
               endif
            enddo;enddo
      
            !call sprint_m
         endif
         !deallocate (qrs_m)
      endif


!----------------------------------------------------------------------
!Lottes 12/19/05: Print mass residual for post proc
!----------------------------------------------------------------------

      write(nu_rt,"(1X/T10,'""Mass Residual')")
      do k=2,lp,2
      do j=2,np,2
      do i=2,mp,2
         bs(i,j,k)=res_mass(i/2,j/2,k/2)
      enddo;enddo;enddo
      call pform(bs,2)


      write(nu_rt,"(1X/T10,'""Log Mass Residual')")
      do k=2,lp,2
      do j=2,np,2
      do i=2,mp,2
	   res_mass(i/2,j/2,k/2)=max(1.0d-16,res_mass(i/2,j/2,k/2))
         bs(i,j,k)=log10(res_mass(i/2,j/2,k/2))
      enddo;enddo;enddo
      call pform(bs,2)

!----------------------------------------------------------------------
!     Print bubble properties
!----------------------------------------------------------------------
      IF (NBS0.EQ.ZERO) GOTO 550
      !cz   IF (GB%FR2.LE.ZERO) GOTO 550
      WRITE(nu_rt,541)ISTEP
      WRITE(nu_rt,542)
      DO 510 I=2,MP,2
      DO 510 J=2,NP,2
      DO 510 K=2,LP,2
         BS(I,J,K)=GB3(I/2,J/2,K/2)%TH
510   CONTINUE
      CALL PFORM(BS,2)
      !SL   IF (IDEBUG.EQ.0) GOTO 550
      DO L1=1,NBS0
         L=L1
         WRITE(nu_rt,543) L1,RG_B(L)
         DO 530 M=1,7
            IF (M.EQ.4.AND.LP.LT.6) GOTO 530
            IF (M.EQ.7.AND..NOT.REACT) GOTO 530
            M1=M-1
            DO 520 I=2,MP,2
            DO 520 J=2,NP,2
            DO 520 K=2,LP,2
               IF (M.EQ.1) BS(I,J,K)=GB4(I/2,J/2,K/2,L)%DN
               IF (M.GE.2.AND.M.LE.4) BS(I,J,K)=GB4(I/2,J/2,K/2,L1)%U(M1)
             IF(M.EQ.5) THEN
               THICK1=3.0D+3*475.0D-3*(GB4(I/2,J/2,K/2,L)%DN-1.0D-8)
               !                 DENOM=
               !              BS(I,J,K)=THICK
                  DENOM=one
               BS(I,J,K)=THICK1/denom ! Lottes 3-23-2005, probably not correct
                                         ! added to fix compile error
                                         ! analyze later if necessary
             ENDIF
               IF (M.EQ.6) BS(I,J,K)=GB4(I/2,J/2,K/2,L1)%T
520         CONTINUE
            IF (M.EQ.1) WRITE(nu_rt,544) L1
            IF (M.EQ.2) WRITE(nu_rt,545) L1
            IF (M.EQ.3) WRITE(nu_rt,546) L1
            IF (M.EQ.4) WRITE(nu_rt,547) L1
            IF (M.EQ.5) WRITE(nu_rt,549) L1
            IF (M.EQ.6) WRITE(nu_rt,548) L1
            CALL PFORM(BS,2)
530      CONTINUE
      ENDDO
541   FORMAT(1X/T10,'Bubble Flow Properties, TSTEP',I3)
542   FORMAT(1X/T10,'"Th_bub, Volume Fraction')
543   FORMAT(1X/T20,'Bubble Group',I2,', Size=',F6.3)
544   FORMAT(1X/T10,'Bubble # Density',I2,', DN(I,J,K,)"')
545   FORMAT(1X/T10,'Bubble X-Vel',I2,', U(I,J,K,)"')
546   FORMAT(1X/T10,'Bubble Y-Vel,',I2,', V(I,J,K)"')
547   FORMAT(1X/T10,'Bubble Z-Vel,',I2,', W(I,J,K)"')
548   FORMAT(1X/T10,'Bubble Temp',I2,', T(I,J,K)"')
549   FORMAT(1X/T10,'Foam Thickness',I2,', Foam(I,J,K)"')
!----------------------------------------------------------------------
550   continue
      goto 600
      G0=1
      WRITE (nu_rt,"(/'  Inlet mass flow rates',F7.3,' kg/s')") GFIN
      WRITE (nu_rt,*) '  Mass flow rates (kg/s)'
      WRITE (nu_rt,*) '         Glass,    Batch,   Total,      Melt'
      DO I=5,MPM1,2
         G1=FLX(I,1)*G0
         IF (NPHAS.GT.1) THEN
            G2=(FLX(I,4)+FLX(I,5))*G0
            G3=0
            IF (NPS_C.GT.0) G3=G3+PMRX_C(I/2)
            IF (NPS_S.GT.0) G3=G3+PMRX_S(I/2)
            G4=G1+G2
            WRITE(nu_rt,552) I,G1,G2,G4,G3
         ELSE
            WRITE(nu_rt,552) I,G1
         ENDIF
      ENDDO
      !WRITE(nu_rt,*) F_BUOY
      G0=1.0D-6
      WRITE (nu_rt,*) ' '
      WRITE (nu_rt,*) '  Inlet heat flow rates (MW)'
      WRITE (nu_rt,551) H_TOT*G0
      WRITE (nu_rt,*) ' '
      WRITE (nu_rt,*) '  Heat flow rates (MW)'
      WRITE (nu_rt,*) '        Glass,    Batch,   Loss,     Sum,     Incoming'
      DO I=5,MPM1,2
         G1=FLXH(I,2)*G0
         G2=FLXH(I,3)*G0
         G5=FLXH(I,6)*G0
         G3=G1+G2+G5
         G4=FLXH(I,5)*G0
         WRITE(nu_rt,554) I,G1,G2,G5,G3,G4
      ENDDO
551   FORMAT (/2F10.3,F10.3,2F10.3/)
552   FORMAT (I5,F10.3,3F10.3,2F10.2)
554   FORMAT (I5,5F10.3)

!----------------------------------------------------------------------
!     Print subspecies concentration
!----------------------------------------------------------------------
600   IF (MS.NE.1) GOTO 700
      WRITE(nu_rt,601)ISTEP
      DO L=LSTARM,LENDM
         IF(L.EQ.1) THEN
           WRITE(nu_rt,603)
         ELSEIF(L.EQ.2) THEN
           WRITE(nu_rt,604)
         ELSE
           WRITE(nu_rt,602) L
         ENDIF
         CALL PFORMM(L,NX)
      ENDDO
601   FORMAT(1X/T10,'Subspecies Mass Fraction (kg/kg of mixture)   TSTEP',I3)
602   FORMAT(1X/T10,'"Subspecies ',I3)
603   FORMAT(1X/T10,'"gfm1 conc')
604   FORMAT(1X/T10,'"GFM2 conc')
605   FORMAT(1X/T10,'"N2 conc')
!------------------
700   CLOSE (nu_rt)
      !if(nps_c>0) call conv_surf_temp_old
      if (comb_grid_exist) call conv_surf_temp
      !call sprint_m
      !call sprint_c
      if (regen==1) call conv_surf_temp_regen !write surf temp to 2nd grid for regen furnace
      !CALL CONV(2) 
      !IF (REGEN.EQ.1) CALL CONV_R(2) !Lottes 
      DEALLOCATE (PRNS)
      !bg   CALL DALCC
      RETURN


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      CONTAINS
      SUBROUTINE SPXZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      J1=1
      DO WHILE (J1.LE.NZ)
         J2=J1+10
         J2=MIN(NZ,J2)
         WRITE(nu_rt,FMR) (y(J),J=J1*2,J2*2,2)
         DO ID2=1,MZ
            I=ID2*2
            WRITE(nu_rt,FMX) X(I),(PRNS(ID2,JD2),JD2=J1,J2)
         ENDDO
         J1=J2+1
      ENDDO
      END SUBROUTINE SPXZ
      END SUBROUTINE SPRINT


!======================================================================
!     PFORM prints system state in general format
!======================================================================
      SUBROUTINE PFORM(F,NX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(MP,NP,LP)
      DO 100 I=1,MP
      DO 100 J=1,NP
      DO 100 K=1,LP
         IF (ABS(F(I,J,K)).LT.1.0D-50) THEN
            F(I,J,K)=ZERO
         ENDIF
100   CONTINUE
      IF (LP.LE.4) THEN
         K1=2
         K2=2
      ELSE
         K1=4
         K2=LP-2
      ENDIF
      DO 200 K=K1,K2,NX
         WRITE(nu_rt,"(/' Z(',I3,') = ',G14.5)") K,Z(K)
         J1=2
150      J2=J1+10*NX
         J2=MIN(J2,NP)
         WRITE(nu_rt,FMR) (y(J),J=J1,J2,NX)
         DO I=2,MP,NX
            WRITE(nu_rt,FMX) X(I),(F(I,J,K),J=J1,J2,NX)
         ENDDO
         J1=J2+NX
         IF (J1.LE.NP) GOTO 150
200   CONTINUE
      RETURN
      END


!======================================================================
!     PFORM0 prints general system state in general format
!======================================================================
      SUBROUTINE PFORM0(LGF,NX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 310 I=2,MP,NX
      DO 310 J=2,NP,NX
      DO 310 K=2,LP,NX
         BS(I,J,K)=LG(I,J,K)%F(LGF)
310   CONTINUE
      CALL PFORM(BS,NX)
      RETURN
      END


!======================================================================
!     PFORMU prints general system state in general format
!======================================================================
      SUBROUTINE PFORMU(LU,NX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 310 I=2,MP,NX
      DO 310 J=2,NP,NX
      DO 310 K=2,LP,NX
         BS(I,J,K)=LG(I,J,K)%U(LU)
310   CONTINUE
      CALL PFORM(BS,NX)
      RETURN
      END


!======================================================================
!     PFORMM prints subspecies concentration
!======================================================================
      SUBROUTINE PFORMM(LGF,NX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 310 I=2,MP,NX
      DO 310 J=2,NP,NX
      DO 310 K=2,LP,NX
         BS(I,J,K)=GFM(I,J,K,LGF)
         !bg            BS(I,J,K)=GFM(I,J,K,LGF)*T(I,J,K)*RU/WT0/10.0d+0
310   CONTINUE
      CALL PFORM(BS,NX)
      RETURN
      END


!======================================================================
!     HPRINT prints headings,
!        reference values, and non-dimensional numbers
!======================================================================
      SUBROUTINE HPRINT
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER(2) IDAY,MONTH,IYEAR
      INTEGER(2) IHOUR,MINUTE,ISEC,ISEC2

      CALL GETDAT(IYEAR,MONTH,IDAY)
      CALL GETTIM(IHOUR,MINUTE,ISEC,ISEC2)
      WRITE (nu_rt,20) MONTH,IDAY,IYEAR,IHOUR,MINUTE
      !WRITE (nu_rt,30) PG0,T0,DNST0,UG0
      WRITE (nu_rt,30) PG0
      WRITE (nu_rt,70) XLE,height,yle,R0
      !IF (NPHAS.GT.1) THEN
         !WRITE (nu_rt,40) RLM0,DND0,THET0C
         !IF (.NOT.STEADY) WRITE (nu_rt,60) TP0,DT0
      !ENDIF
      WRITE (nu_rt,110)
      !WRITE (nu_rt,120) REYG,SCN Lottes 5/6/05 new ref Re below
      !Calculate some sensible Reynolds number here
      Re0=one
      scn=one
      WRITE (nu_rt,120) Re0,SCN
      IF (NPHAS.GT.1) THEN
         !WRITE (nu_rt,130) GDN,GVN_C,RFG_C
         !WRITE (nu_rt,140) EUN,GVN_S,RFG_S
         !WRITE (nu_rt,130) GDN,GVN_C
         !WRITE (nu_rt,140) EUN,GVN_S
         !IF (.NOT.STEADY) WRITE (nu_rt,160) TRN
      ENDIF
20    FORMAT(T30,'CALCULATION RESULTS: Glass Melt Flow'/ &
             T40'('I3'/'I2'/'I4','I2':'I2')'/T35,'   REFERENCE PARAMETERS'/)
!30    FORMAT(T4,'PG0=',F8.0,'Pa',T21,'T0=',F8.0,'K'T38,'DNST0=', & 
30    FORMAT(T4,'PG0=',F8.0,'Pa')
!40    FORMAT(T4,'RLM0=',G11.3,T21,'DND0=',G11.3,T38,'THET0C=',F8.4)
!60    FORMAT (T4,'TP0=',G11.3,T21,'DT0=',G11.3,T38,'DNST0=',G11.3)
70    FORMAT (T4,'Length=',F8.1,'m',T21,'Height=',F8.1,'m',T38,'Depth=',F8.2,'m',T60,'R0=',F8.2,'m')
110   FORMAT(//T30,'  NON-DIMENSIONAL PARAMETERS'/)
120   FORMAT(T4,'REYNOLDS=',G11.3,T27,'SCHMIDT=',G11.3)
130   FORMAT (T4,'GD NUMBER=',G11.3,T27,'GV NUMBER=',G11.3)
      !&  ,T50,'RFG_C=',G11.3)
!140   FORMAT (T4,'EUN=',G11.3,T27,'GV NUMBER=',G11.3)
      !&  ,T50,'RFG_S=',G11.3)
!160   FORMAT (T4,'TRN= ',G11.3)
      RETURN
      END


!======================================================================
!======================================================================
!======================================================================
! SPRINT_c
!  Print combustion/melt interface variables
!  at combustion grid points
!  in 3D for interpolation check
!        
!======================================================================
      subroutine sprint_c
      use gbl_var
      implicit double precision (a-h,o-z)
      
      allocate(bs_c(mp_c,np_c,lp_c))
      fmr='(/2x,'' x  /  y   '',11(g13.5))'
      fmz='(/2x,'' x  /  z   '',11(g13.5))'
      fmx='(g13.5,11g13.5)'
      nu_c=103
      filename=casedir//'\surf_c'//runum//'m.out'
      open(nu_c,file=filename)

      !Lottes 4/21/05: print surface heat flux as 3d for post proc

      write(nu_c,"(1x/t10,'""surf heat, surface heat flux (w/m^2)')")
      do k=2,lp_c,2
      do j=2,np_c,2
      do i=2,mp_c,2
         if (k < 6) then
            bs_c(i,j,k)=qrsc(i/2,j/2)
         else
            bs_c(i,j,k)=zero
         endif
      enddo;enddo;enddo
      call pform_c(bs_c,2)

      !Lottes 4/21/05: print surface temperature as 3d for post proc

      write(nu_c,"(1x/t10,'""surf t, surface temperature (k)')")
      do k=2,lp_c,2
      do j=2,np_c,2
      do i=2,mp_c,2
         if (k < 6) then
            bs_c(i,j,k)=tgs_c(i/2,j/2)
         else
            bs_c(i,j,k)=zero
         endif
      enddo;enddo;enddo
      call pform_c(bs_c,2)


      deallocate(bs_c)
      close(nu_c)
      return
      end


!======================================================================
!======================================================================
!======================================================================
!     PFORM_c prints variables based on combustion grid
!     Used to check surface interface interpolations
!     Lottes 6/23/05
!======================================================================
      subroutine pform_c(f,nx)
      use gbl_var
      implicit double precision (a-h,o-z)
      dimension f(mp_c,np_c,lp_c)
      do i=1,mp_c
      do j=1,np_c
      do k=1,lp_c
         if (abs(f(i,j,k)) < 1.0d-50) then
            f(i,j,k)=zero
         endif
      enddo;enddo;enddo

      do k=4,lp_c-2,nx
         write(nu_c,"(/' z(',i3,') = ',g12.3)") k,z_c(k)
         jb=2
         do
            je=jb+10*nx
            je=min(je,np_c)
            write(nu_c,fmr) (y_c(j),j=jb,je,nx)
            do i=2,mp_c,nx
               write(nu_c,fmx) x_c(i),(f(i,j,k),j=jb,je,nx)
            enddo
            jb=je+nx
            if (jb > np_c) exit
         enddo
      enddo

      return
      end


!======================================================================
!======================================================================
!======================================================================
! SPRINT_m
!  Print combustion/melt interface variables
!  at melt grid points
!  in 3D for interpolation check
!        
!======================================================================
      SUBROUTINE SPRINT_m
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      

      FMR='(/2X,'' X  /  Y   '',11(G13.5))'
      FMZ='(/2X,'' X  /  Z   '',11(G13.5))'
      FMX='(G13.5,11G13.5)'
      filename=casedir//'\surf_m'//runum//'m.out'
      OPEN(NU_m,FILE=FILENAME)

      !Lottes 4/21/05: Print surface heat flux as 3d for post proc
     
      write(nu_m,"(1X/T10,'""Surf Heat, Surface heat flux (W/m^2)')")
      do k=2,lp,2
      do j=2,np,2
      do i=2,i_me,2
         if (k > lp-4) then
            bs(i,j,k)=qrs_m(i/2,j/2)
         else
            bs(i,j,k)=zero
         endif
      enddo;enddo;enddo
      call pform_m(bs,2)

      !Lottes 4/21/05: Print surface temperature as 3d for post proc

      write(nu_m,"(1X/T10,'""Surf T, Surface Temperature (K)')")
      do k=2,lp,2
      do j=2,np,2
      do i=2,i_me,2
         if (k > lp-4) then
            bs(i,j,k)=Tgs_m(i/2,j/2)
         else
            bs(i,j,k)=zero
         endif
      enddo;enddo;enddo
      call pform_m(bs,2)

      close(nu_m)
      return
      end


!======================================================================
!======================================================================
!======================================================================
!     PFORM_m prints variables based on combustion grid
!     Used to check surface interface interpolations
!     Lottes 6/23/05
!======================================================================
      SUBROUTINE PFORM_m(F,NX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(MP,NP,LP)
      DO I=1,MP
      DO J=1,NP
      DO K=1,LP
         IF (ABS(F(I,J,K)) < 1.0D-50) THEN
            F(I,J,K)=ZERO
         ENDIF
      enddo;enddo;enddo

      DO K=4,lp-2,NX
         WRITE(NU_m,"(/' Z(',I3,') = ',G12.3)") K,Z(K)
         Jb=2
         do
            Je=Jb+10*NX
            Je=MIN(Je,NP)
            WRITE(NU_m,FMR) (y(J),J=Jb,Je,NX)
            DO I=2,MP,NX
               WRITE(NU_m,FMX) X(I),(F(I,J,K),J=Jb,Je,NX)
            ENDDO
            Jb=Je+NX
            IF (Jb > NP) exit
         enddo
      enddo

      RETURN
      END
!======================================================================
!======================================================================
!======================================================================
!     print_x_mass_flow
!
!     print out glass mass rates at yz-plane cross sections
!     Lottes 1/14/06
!======================================================================
subroutine print_x_mass_flow
use gbl_var
implicit double precision (a-h,o-z)
real(8) melt_rate_x
open(nu_flox,file=casedir//'\flox'//runum//'m.out')
write (nu_flox,'(/"X_Index ", &
            "     Forward_Flow     ", &
            "      Back_Flow       ", &
            "       Net_Flow       ", &
            "     Batch_Flow       ", &
            "       Total          ", &
            "      Melt_Rate       "/)')

do i=3,mp-1,2

   batch_flow_x = zero
   melt_rate_x  = zero

   if (nps_c > 0) then !cullet exists
      batch_flow_x = batch_flow_x + pfrx_c(i)
      melt_rate_x  = melt_rate_x +  pmrx_c(i/2)
   endif

   if (nps_s > 0) then !sand exists
      batch_flow_x = batch_flow_x + pfrx_s(i)
      melt_rate_x  = melt_rate_x +  pmrx_s(i/2)
   endif

   total_flow_x=flx(i,1)+batch_flow_x

   write(nu_flox,"(' ',I5,6E22.14)") I,flx(i,2),flx(i,3),flx(i,1),batch_flow_x,total_flow_x,melt_rate_x
enddo
close(nu_flox)

return
end
