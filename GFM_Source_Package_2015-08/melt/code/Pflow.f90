!======================================================================
! PFLOW.F90
!======================================================================
!     solves property variables for batch particles
!       DN    number density
!       U     velocity components
!       T     temperature
!     Rev: 6/01
!======================================================================
      SUBROUTINE PFLOW(NEL_P)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 SMXD(6,NPS_1),AVED(6,NPS_1),TH_PX_1(NPS_1)
      !      DATA SP_F/1.0D+2/ !BG delete fall 2004
!----------------------------------------------------------------------
      IF (NEL_P.EQ.1) THEN !assign cullet arrays
         P10 => PC0
         P1 => PC !cullet calculated properties:  U, V, W, T, DN, MR
         P1A => PCA !cullet property array
         !GVN_1=GVN_C !cullet group numbers
         gk0_1=gk0_pc !cullet thermal conductivity (W/m K)
         !RFG_1=RFG_C !density ratio between particle and liquid
         SP0_1=SP0_C
         PMRX_1 => PMRX_C !cullet melting rate?
         FLXHP_1 => FLXHP_C !cullet mass flux?
         PFRX_1 => PFRX_C
         QCD_1 => QCD_C  !energy conducted to cullet [W] Lottes 5/5/05
         QRSP_1 => QRSP_C !rad energy to cullet, [W] Lottes 5/5/05
         RP_1 => RP_C !radius of cullet particle
         RP2_1 => RP2_C !square of cullet particle radius
         RP3_1 => RP3_C !cube of cullet particle radius
         TM_1 => TM_C !cullet melting temperature (K)
         WPI_1 => WPI_C
         area_spr_1 => area_c_spr
         IF (.NOT.STEADY) THEN
            P1T => PCT
         ENDIF
      ELSEIF (NEL_P.EQ.2) THEN !assign sand arrays
         P10 => PS0
         P1 => PS !sand calculated properties: U, V, W, T, DN, MR
         P1A => PSA !sand property arrays: DS, CL, H0, FR
         !GVN_1=GVN_S !sand group numbers
	      gk0_1=gk0_ps
         !RFG_1=RFG_S !density ratio between particle and liquid
         SP0_1=SP0_S
         PMRX_1 => PMRX_S !sand melting rate?
         FLXHP_1 => FLXHP_S !sand mass flux?
         PFRX_1 => PFRX_S !d
         QCD_1 => QCD_S !energy conduction from glass to sand [W] Lottes 5/5/05
         QRSP_1 => QRSP_S !rad energy to sand, Lottes 5/5/05
         RP_1 => RP_S !radius of sand particle
         RP2_1 => RP2_S !square of radius of sand particle
         RP3_1 => RP3_S !cube of radius of sand particle
         TM_1 => TM_S !sand melting temperature
         WPI_1 => WPI_S
         area_spr_1 => area_s_spr
         IF (.NOT.STEADY) THEN
            P1T => PST
         ENDIF
      ELSE
         RETURN
      ENDIF
      P1A%H=P1A%H0 !normalizes the heat of fusion
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!     Batch Particle Number Density
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      KD=1
      SGM=0
      NEL=13
!----------------------------------------------------------------------
!     Set sweep limits
!
!----------------------------------------------------------------------
      MI1=MP_B  !set at 4.....check into this
      MI3=MP_E1 !MP minus 2
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2
!----------------------------------------------------------------------
! 
!     Set scalar indices
!----------------------------------------------------------------------
      MID1=MI1/2
      MID3=MI3/2
      NJD1=NJ1/2
      NJD3=NJ3/2
      LKD1=LK1/2
      LKD3=LK3/2
      DO 200 L=NPS_1,1,-1 ! sweep over the number of particle sizes
         KL=L
         DO ID2=1,MZ
         DO JD2=1,NZ
         DO KD2=1,LZ
            FZ(ID2,JD2,KD2)=P1(ID2,JD2,KD2,KL)%DN
            !FZ2(ID2,JD2,KD2)=P1(ID2,JD2,KD2,KL)%DN
         enddo;enddo;enddo
         nd_march=1
         if (nd_march==1) then
            
            call solve_nd
            !fz=fz2
         else

            CALL SOURCEP(MI1,MI3,NJ1,NJ3,LK1,LK3,nel_p)
            CALL PFLO2C(MI1,MI3,NJ1,NJ3,LK1,LK3)
            CALL ADLBL3(FZ,KD,MID1,MID3,NJD1,NJD3,LKD1,LKD3)

         endif
         !--  Apply upper and lower bounds to calculated values
         DO 140 I=MP_B,MP_E1,2
         ID2=I/2
         DO 140 J=NJ1,NJ3,2
         JD2=J/2
         DO 140 K=LKZ1,LKZ2,2
            IF (IBCELL(I,J,K).GE.1) CYCLE
            KD2=K/2
            G0=FZ(ID2,JD2,KD2)
            CALL BOUND(G0,1,3)
            P1(ID2,JD2,KD2,KL)%DN=RF(13)*G0+RFC(13)*P1(ID2,JD2,KD2,KL)%DN
140      CONTINUE
         SMXD(1,KL)=RES(NEL,1)
         AVED(1,KL)=RES(NEL,2)
200   CONTINUE
!---
!        batch volume fraction
!---
         !CZ    IF (NEL_P.EQ.1) THEN
         !CZ       QRSTOTG=0
         !CZ       QRSS0=0
         !CZ    ENDIF
      DO I=MP_B,MP_E1,2; ID2=I/2
      DO J=NJ1,NJ3,2;    JD2=J/2
      DO K=2,LKZ2,2;     KD2=K/2
         IF (IBCELL(I,J,K).GE.1) CYCLE
         TH_P0=ZERO
         DO L=1,NPS_1 !volume fraction of each size group 
	      th_px_1(L)=P1(ID2,JD2,KD2,L)%DN*c4d3pi*RP_1(L)**3

            TH_P0=TH_P0+TH_PX_1(L) !total volume fraction of sand or cullet in cell
         END DO
         P10(ID2,JD2,KD2)%TH=TH_P0 !vol frac (vol particles/vol cell) 

         !------------------------------------------
         !Recalculate split of surface incoming heat 
         !going to particles and liquid

         IBC1=MOD(IBCELL(I,J,K+2),10)
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.EQ.1) CYCLE
         IF (K.GT.LPM2.OR.IBC1.NE.4) CYCLE !go out if not at surface cell
         !IF (NEL_P.EQ.1) THEN !Lottes: first time thru: initialize qrslg to all rad energy
         !  QRSLG(ID2,JD2)=QRS(ID2,JD2) !Lottes: sets rad to liq glass = to surface heat rate
         !CZ         QRSTOTG=QRSTOTG+QRSLG(ID2,JD2)
         !CZ         QRSS0=QRSS0+QRS(ID2,JD2)         
         !ENDIF

         !Lottes 5/1/05: above won't work if there is sand but no cullet
         !       initialize surface energy to liq glass = to surface rad heat rate
         !       This initialization could be moved up to before the pflow call.

         if(nel_p==1.or.(nel_p==2.and.nps_c==0)) then
           qrslg(id2,jd2)=qrs(id2,jd2)
         endif

         QRSP_1(ID2,JD2,1:NPS_1)=0 !Initialize energy going to sand or cullet
         IF (TH_P0.LT.SMALL) CYCLE !No particles of type in cell: all energy goes to elsewhere
         IF (QRS(ID2,JD2).LE.ZERO) CYCLE !No energy incoming: liquid can radiate out
         !NPS1=NPS_C
         !IF (NEL_P.EQ.2) NPS1=NPS_S
         !DO L=1,NPS_1 !over size groups
            !Lottes 4/28/05: q1's must sum to qrs, they don't: formulation below is wrong
         !   IF (TH_P0.GE.TH_PMX/10) THEN
               !Q1=QRS(ID2,JD2)*TH_PX_1(L)/TH_P0/NPS0/NPS1
         !      Q1=QRS(ID2,JD2)*TH_PX_1(L)/TH_P0/NPS0/NPS_1
         !   ELSE
         !      !Q1=QRS(ID2,JD2)*TH_PX_1(L)/TH_PMX*10/NPS0/NPS1
         !      Q1=QRS(ID2,JD2)*TH_PX_1(L)/TH_PMX*10/NPS0/NPS_1
         !   ENDIF
         !   QRSP_1(ID2,JD2,L)=Q1 !Radiation energy to size group
         !   QRSLG(ID2,JD2)=QRSLG(ID2,JD2)-Q1 !Net radiation energy to liquid glass
         !   IF (QRSLG(ID2,JD2).LT.ZERO) QRSLG(ID2,JD2)=ZERO 
         !ENDDO 
        
         !Lottes 5/1/05: The above code attempts to split up radiation energy among size groups
         !       according to volume fraction but does not properly account for presence
         !       of both sand and cullet. This code should be better placed 
         !       (after new number densities are known for both sand and cullet)
         !       but the following does impliment the model properly.
         !       However, in my view this is not a good model.
         !tot_th=zero
         !if (nps_c>0) tot_th=tot_th+pc0(id2,jd2,kd2)%th
         !if (nps_s>0) tot_th=tot_th+ps0(id2,jd2,kd2)%th
         !do L=1,nps_1
            !surface rad heat to particles in grp = surf rad * coverage * vol frac size group / total vol frac 
         !   if (th_p0 < th_pmx) then
         !      coverage = th_p0/th_pmx
         !   else
         !      coverage = one
         !   endif 
         !   qrsp_1(id2,jd2,L)=qrs(id2,jd2)*coverage*th_px_1(L)/tot_th
         !   qrslg(id2,jd2)=qrslg(id2,jd2)-qrsp_1(id2,jd2,L)
         !enddo


         !Lottes 6/6/05: New model puts most of radiant energy into solids
         !until the solid volume fraction is small
         area=area_c(i,j,k,3)
         theta_p=zero
         if (nps_c>0) theta_p=theta_p+pc0(id2,jd2,kd2)%th
         if (nps_s>0) theta_p=theta_p+ps0(id2,jd2,kd2)%th
         do L=1,nps_1
            if (area_s_spr(id2,jd2)+area_c_spr(id2,jd2) < area) then
               qrsp_1(id2,jd2,L)=qrs(id2,jd2)*area_spr_1(id2,jd2)/area
            else
               qrsp_1(id2,jd2,L)=qrs(id2,jd2)*coverf(id2,jd2)*th_px_1(L)/theta_p
            endif
            qrslg(id2,jd2)=qrslg(id2,jd2)-qrsp_1(id2,jd2,L)
         enddo
         qrslg(id2,jd2)=max(qrslg(id2,jd2),zero) !Don't let round off error pull this negative

         !CZ      IF (QRSLG(ID2,JD2).GT.QRS(ID2,JD2)) THEN
         !CZ       QRSLG(ID2,JD2)=QRS(ID2,JD2)
         !CZ      ENDIF
         !CZ      IF (QRSLG(ID2,JD2).LT.0.0) THEN
         !CZ       QRSLG(ID2,JD2)=0.0
         !CZ      ENDIF
      enddo;enddo;enddo

      !Lottes 5/5/05:
      !Below won't work for accumulating totals since we can come through
      !here twice or only once for sand only or cullet only.
      !Not used except for debugging data collection, so I commented it out.
      !IF (NEL_P.EQ.1) THEN
      !   QRSTOT=0
      !   QRSS1=0
      !   QRSTOTP=0
      !ENDIF
      !J=NPM2
      !DO 230 I=2,MP,2
      !ID2=I/2
      !DO 230 K=2,LP,2
      !KD2=K/2
      !   IF (IBCELL(I,J,K).EQ.1) CYCLE
      !   IF (NEL_P.EQ.2) THEN
      !      QRSTOT=QRSTOT+QRSLG(ID2,JD2)
      !      QRSS1=QRSS1+QRSLG(ID2,JD2)
      !   ENDIF
      !   DO L=1,NPS_1
      !      QRSTOT=QRSTOT+QRSP_1(ID2,JD2,L)
      !      QRSTOTP=QRSTOTP+QRSP_1(ID2,JD2,L)
      !   ENDDO 
      !230   CONTINUE
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      CALL QCOND !Lottes: compute conduction to particle size groups
      CALL QLOSS !Lottes: compute heat loss to walls

      !update scaling factor and reset qrs
      !may have to rethink this to account for negative qrs over some areas @@@ 
      !old_facq=facq
      old_facq=scale_in
		if (qs_in > 0) then
         scale_in=(h_needed-eb_heat+q_wall_loss_tot+scale_out*qs_out)/qs_in_base
         facq_chg=abs(scale_in-old_facq)/scale_in
		else
		   scale_in = 0
         facq_chg = 0
		endif
      !facq=(h_needed-eb_heat+q_wall_loss_tot)/qrs_tot_base
      !facq_chg=abs(scale_in-old_facq)/scale_in
      if (iheat_flux_type < 3) then !scaled uniform or scaled combustion calculated heat flux
         qrs_tot=zero
			qs_in=zero
         do i=2,i_me,2
         do j=2,np,2
            if (qc_surf0(i/2,j/2) > 0) then
				   qrs(i/2,j/2)=qc_surf0(i/2,j/2)*scale_in
            !else
				!   qrs(i/2,j/2)=qc_surf0(i/2,j/2)*scale_out
            endif
            qrs_tot=qrs_tot+qrs(i/2,j/2)
				qs_in = qs_in + max(qrs(i/2,j/2),zero)
      enddo;enddo
      endif

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!     Batch Velocity & Temperature Solve  
!        Particle velocity: (LD=1-3,NEL=14,15,16)
!        Batch Temperature: (LD=4,NEL=17)
!     KL: Batch particle size index global variable
!----------------------------------------------------------------------
!----------------------------------------------------------------------
300   continue
      DO 400 L=NPS_1,1,-1
      KL=L
      !The current batch model has batch moving away from the charger at the inlet velocity, which is constant.
      !Therefore, there is no need to solve batch momentum equations - until a drag model is implemented and debugged.
      !Lottes 8-2-06
      !DO 400 LD=1,4
      DO 400 LD=4,4
         !IF (LD.EQ.3.AND.LZ.LT.3) GOTO 400 !what is this, how can there be no z extent in a melt? Lottes 1-17-06 @@@@
         NEL=LD+13
         DO 310 ID2=1,MZ
         DO 310 JD2=1,NZ
         DO 310 KD2=1,LZ
            IF (LD.EQ.4) THEN
               FZ(ID2,JD2,KD2)=P1(ID2,JD2,KD2,KL)%T
            ELSE
               FZ(ID2,JD2,KD2)=P1(ID2,JD2,KD2,KL)%U(LD)
            ENDIF
310      CONTINUE               
         CALL SOURCEP(MI1,MI3,NJ1,NJ3,LK1,LK3,nel_p)
         CALL PFLO3C(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
         CALL ADLBL3(FZ,KD,MID1,MID3,NJD1,NJD3,LKD1,LKD3)
!---     
!     Check bounds of computed properties
!---     
         DO 380 I=MP_B,MP_E1,2; ID2=I/2
         DO 380 J=NJ1,NJ3,2;    JD2=J/2
         DO 380 K=2,LKZ2,2;     KD2=K/2
            IF (IBCELL(I,J,K).GE.1) CYCLE
            G0=FZ(ID2,JD2,KD2)
            DN0=P1(ID2,JD2,KD2,KL)%DN

!---
!     BATCH TEMPERATURE
!---
      !Lottes 4/21/05: This code also calculates the batch melting rate
      ! P1 points to either PC - cullet properties or PS - sand properties
      ! Do below if solid temperature equation has just been solved.
            IF (LD.NE.4) GOTO 340
            q_pTmelt=0
            PMR1=0

            !Sum up rate of particles entering cell, Lottes 5/24/05
            DO 330 IU=1,3 ! do for directions x,y,z
            DO 330 M=1,2  ! do for - and + direction 
               IF (IU.EQ.2.AND.NP.LT.6) GOTO 330 ! old - for 2D case
               IF (IU.EQ.3.AND.LP.LT.6) GOTO 330 ! old - for 2D case
               AS1=AS(ID2,JD2,KD2,IU,M)
               IF (AS1.GT.ZERO) THEN
                  PMR1=PMR1+AS1
                  IF (IU.EQ.1) THEN   
                     ID21=ID2+M*2-3
                     TP1=TM_1(KL)-P1(ID21,JD2,KD2,KL)%T
                  ELSEIF (IU.EQ.2) THEN   
                     JD21=JD2+M*2-3
                     TP1=TM_1(KL)-P1(ID2,JD21,KD2,KL)%T
                  ELSEIF (IU.EQ.3) THEN   
                     KD21=KD2+M*2-3
                     TP1=TM_1(KL)-P1(ID2,JD2,KD21,KL)%T
                  ENDIF
                  IF (TP1.GT.ZERO) q_pTmelt=q_pTmelt+AS1*TP1  !sum(#/s *(T_melt - Tp)) 
               ENDIF
330         CONTINUE
            IF (PMR1.LE.ZERO) THEN ! PMR1 = # of particles entering cell/second
               !Get here if no particles entering cell (there might be some leaving)
               !PMRL0=ZERO !Initialize melting rate
               P1(ID2,JD2,KD2,KL)%T=TM_1(KL) !Sets particle T to melt T for size group
               IF (DN0.GT.ZERO) THEN
                  !vol frac of size group in cell
	               g0=DN0*c4d3pi*RP_1(L)**3
                  G1=P10(ID2,JD2,KD2)%TH-G0
                  P10(ID2,JD2,KD2)%TH=MAX(G1,ZERO)
                  P1(ID2,JD2,KD2,KL)%DN=0
               ENDIF
               IBC1=MOD(IBCELL(I,J,K+2),10)
               IF (K.LE.LPM2.AND.IBC1.EQ.4) THEN ! True if sufrace cell
                  QCD_1(ID2,JD2,KL)=0  ! zero energy conducted to particles (J/s)             
                  QRSLG(ID2,JD2)=QRSLG(ID2,JD2)+QRSP_1(ID2,JD2,KL) ! add particle rad back to liquid
                  QRSP_1(ID2,JD2,KL)=0 ! zero out energy from radiation to particles
               ENDIF
               !cz??               GOTO 380
               P1(ID2,JD2,KD2,KL)%MR=ZERO ! No particles here then no melting
               cycle
               !GOTO 381
            ENDIF
	         !pmr1= variation of du*dn*area
            PMR1=PMR1*WPI_1(KL) ! kg/s of particles of size group entering cell
                 !WPI = mass of particle (kg)
            !------------------
            !Lottes: Below: put particle temperature solution back in particle T array
            !        with constraints Tp>=T_mn and Tp<=size group melt temp
            !        uses relaxation
            !        Note: if we don't get here, we don't put solution back in its array
            G0=MAX(G0,T_MN)
            IF (G0.LT.TM_1(KL)) THEN
               P1(ID2,JD2,KD2,KL)%T=P1(ID2,JD2,KD2,KL)%T*RF(17)+RFC(17)*G0
            ELSE
               P1(ID2,JD2,KD2,KL)%T=TM_1(KL)
            ENDIF
            if(nel_p==1)then !Test code, Lottes
              Cullet_T(id2,jd2)=p1(id2,jd2,kd2,kL)%T
            else
              Sand_T(id2,jd2)=p1(id2,jd2,kd2,kL)%T
            endif
            !------------------
            IBC1=IBCELL(I,J,K+1)
            IF (K.LE.LPM2.AND.IBC1.EQ.4) THEN ! ibc1=4 --> surface layer of cells
               !get solid specific heat
               if (nel_p==1) then
                  udf => udf_clc                                              
                  call udf_int(udf_clcn,Tm_1(kL),particle_cl)          
                  call udf_int(udf_clcn,p1(id2,jd2,kd2,kL)%T,particle_cl2)          
               else
                  udf => udf_cls                                               
                  call udf_int(udf_clsn,Tm_1(kL),particle_cl)          
                  call udf_int(udf_clsn,p1(id2,jd2,kd2,kL)%T,particle_cl2)          
               endif
 
               tol_Tm=1.0d-2
               IF (G0.LE.TM_1(KL)*(1-tol_Tm)) THEN !Mean Particle T < melt point T     
                  !Some melting still occurs due to conduction across sand/glass boundary layer
                  !QMLT=QCD_1(ID2,JD2,KL)/P1A%H0 ! (J/s)/(J/kg) = kg/s melted              
                  !This melt rate must account for energy needed to raise particles
                  !from the mean particle T to Tmelt. Lottes 5/25/05 Major fix.

                  !qmlt=qcd_1(id2,jd2,kL)/(p1a%h0+p1a%cl*(Tm_1(kL)-p1(id2,jd2,kd2,kL)%T))
                  qmlt=qcd_1(id2,jd2,kL)/(p1a%h0+((particle_cl+particle_cl2)/2)*(Tm_1(kL)-p1(id2,jd2,kd2,kL)%T)) 

                  IF (QMLT.GT.PMR1) THEN !melting rate > mass rate particles entering
                     QMLT=PMR1               
                     !QCD_1(ID2,JD2,KL)=QMLT*P1A%H0 !Reset cond. melt rate to mass rate * heat of fusion               
                     !Reset cond. melt rate to mass rate * (heat of fusion + heat to Tmelt)
                     !qcd_1(id2,jd2,kL)=pmr1*(p1a%h0+p1a%cl*(Tm_1(kL)-p1(id2,jd2,kd2,kL)%T))
                     qcd_1(id2,jd2,kL)=pmr1*(p1a%h0+((particle_cl+particle_cl2)/2)*(Tm_1(kL)-p1(id2,jd2,kd2,kL)%T))
                  ENDIF
               ELSE ! Particles in cell at melt point T
                  !q_pTmelt=Energy rate to heat particles coming in to melting point
                  !q_pTmelt=q_pTmelt*WPI_1(KL)*P1A%CL ! (#/s K)(kg/#) (J/kg K) = (W)
                  q_pTmelt=q_pTmelt*WPI_1(KL)*particle_cl ! (#/s K)(kg/#) (J/kg K) = (W)  !RML @@@@
                  !cz??                  IF (DN0.LT.1.0D-5) THEN      

                  if (p10(id2,jd2,kd2)%th < 1.0D-3) THEN ! almost no particle volume in cell 
                     !We may not need this case, conduction should melt what's left. Lottes 5/24/05
                     !IF (DN0.LT.1.0D-3) THEN ! almost no particles in cell 
                     QMLT=PMR1 !Sets melt mass rate to mass rate of particles into cell
                     QCD_1(ID2,JD2,KL)=QMLT*P1A%H0+q_pTmelt !Sets conduction q to
                             ! melting energy + energy to heat to melt point
                             ! does not account for energy to heat to local glass T
                     !All radiation should go to liquid in this case,
                     !since all melting energy is coming from liquid, Lottes 5/24/05
                     !Put any radiation to solid back in liquid
                     qrslg(id2,jd2)=qrslg(id2,jd2)+qrsp_1(id2,jd2,kL)
                     qrsp_1(id2,jd2,kL)=zero
                  ELSE !This is the most important case, but it occurs near the end of particle lifetime
                     !All energy from radiation is assumed to go into melting
                     QS0=QRSP_1(ID2,JD2,KL)+QCD_1(ID2,JD2,KL) !rad + cond power to particles (W)
                     !Energy to heat particles to melt temperature is accounted for (q_pTmelt)
                     !But energy to heat melted glass to local glass T is NOT accounted for here
                     !Also no check for melt rate >> rate of particles entering cell
                     QMLT=(QS0-q_pTmelt)/P1A%H0 ! melt rate (kg/s)
                     QMLT=MAX(QMLT,ZERO)
                     !Check if melt rate > rate particles entering cell (kg/s)
                     if (qmlt>pmr1) then !bound melt mass and energy rates by mass rate in
                        qmlt=pmr1
                        qmelt_loc=qmlt*p1a%h0+q_pTmelt
                        if (qmelt_loc < qcd_1(id2,jd2,kL) ) then !melt energy rate is less than conduction rate
                           qcd_1(id2,jd2,kL)=qmlt*p1a%h0+q_pTmelt
                           qrslg(id2,jd2)=qrslg(id2,jd2)+qrsp_1(id2,jd2,kL)
                           qrsp_1(id2,jd2,kL)=zero
                        else
                           qrad_need=qmlt*p1a%h0+q_pTmelt-qcd_1(id2,jd2,kL)
                           qrslg(id2,jd2)=qrslg(id2,jd2)+qrsp_1(id2,jd2,kL)-qrad_need
                           qrsp_1(id2,jd2,kL)=qrad_need
                        endif
                     endif
                  ENDIF
               ENDIF
               VOL=VOL_C(I,J,K)
               PMRL0=QMLT/VOL ! Only for surface layer           
            ELSE        
               PMRL0=ZERO !If not surface sets melt rate to zero
            ENDIF        
            PMRL0=RFL(19)*PMRL0+RFLC(19)*P1(ID2,JD2,KD2,KL)%MR
381         IF (PMRL0.LT.1.0D-30) THEN ! PMRL0 = 0 if jumped here
               P1(ID2,JD2,KD2,KL)%MR=ZERO
            ELSE
               ! Particle melting rate is set here
               ! Can only get here for surface layer of cells
               P1(ID2,JD2,KD2,KL)%MR=PMRL0  ! kg/(s*m**3)          
            ENDIF            
            !GOTO 380
            cycle
!---
!     BATCH VELOCITY
!---
340         IF (DN0.LE.DN_MN) G0=0
            CALL BOUND(G0,LD,1)
            IF (LD.NE.1) GOTO 360
!---
!     X-VELOCITY
!
!---
!        SP: solid pressure 
!---
!           Golchert removed solid pressure push in x-direction sometime in
!           the fall of 2004. Lottes: I agree, there is no foundation for
!           doing this.
!
!           IF (DN0.LE.DN_MN) GOTO 355
!           G1=P1(ID2-1,JD2,KD2,KL)%U(1)
!           DN1=P1(ID2-1,JD2,KD2,KL)%DN
!           IBC1=MOD(IBCELL(I-2,J,K),10)
!           IF (G1.GT.ZERO.AND.(DN1.GT.SMALL.OR.IBC1.EQ.2)) GOTO 350
!           G1=P1(ID2,JD2,KD2+1,KL)%U(1)
!           DN1=P1(ID2,JD2,KD2+1,KL)%DN
!           IF (G1.GT.ZERO.AND.DN1.GT.SMALL) GOTO 350
!           G1=P1(ID2,JD2,KD2-1,KL)%U(1)
!           DN1=P1(ID2,JD2,KD2-1,KL)%DN
!           IF (G1.GT.ZERO.AND.DN1.GT.SMALL) GOTO 350
!           SP(I,J,K)=ZERO
!           GOTO 355
!350         SP(I,J,K)=SP(I,J,K)+SP_F*(G1-G0)
!           SP(I,J,K)=MAX(SP(I,J,K),ZERO)
!           G0=G1
355         continue
            II=ID2+MZ
            FZ(II,JD2,KD2)=G0
360         IF (LD.NE.2) GOTO 370
!---
!     Y-VELOCITY
!---
            IF (LZ.GE.3) THEN
               JJ=JD2+NZ
               FZ(ID2,JJ,KD2)=G0
            ELSE
               P1(ID2,JD2,KD2,KL)%U(1)=FZ(ID2+MZ,JD2,KD2)
               P1(ID2,JD2,KD2,KL)%U(2)=G0
            ENDIF
370         IF (LD.NE.3) GOTO 380
!---
!     Z-VELOCITY
!---
            IBC0=MOD(IBCELL(I,J,K+2),10)
            IF (IBC0.EQ.4) G0=0
            P1(ID2,JD2,KD2,KL)%U(1)=FZ(ID2+MZ,JD2,KD2)
            P1(ID2,JD2,KD2,KL)%U(2)=FZ(ID2,JD2+NZ,KD2)
            P1(ID2,JD2,KD2,KL)%U(3)=G0
380      CONTINUE
         SMXD(LD+1,KL)=RES(NEL,1)
         AVED(LD+1,KL)=RES(NEL,2)
400   CONTINUE
!----------------------------------------------------------------------
!      CALL INTPSP ! Golchert commented out fall 2004
!                  ! solid pressure interpolation routine
!                  ! should not need - Lottes
!----------------------------------------------------------------------
!     Wrap up after solving particle equations for each size group
!----------------------------------------------------------------------
      call qcond_sum !total heat conduction to particles over size groups
      N=NEL_P+1
      CALL EXTR(N)
      CALL PFLOW2(NEL_P)
      NULLIFY(P10,PMRX_1,FLXHP_1,PFRX_1,QCD_1,QRSP_1)
      NULLIFY(RP_1,RP2_1,RP3_1,TM_1,P1,WPI_1)
      IF (.NOT.STEADY) NULLIFY(P1T)
      RETURN
      END


!======================================================================
!======================================================================
!     returns dx, dy, or dz depending on iu
!     L not used
!     side effect: sets area
!
!     NOT USED
!
!     Was part of a particle/droplet turbulent diffusion model
!     in a previous CFD code not needed in a laminar liquid flow
!======================================================================
!      DOUBLE PRECISION FUNCTION DFF0(I,J,K,L,IU)
!      USE GBL_VAR
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!      AREA=AREA_C(I,J,K,IU) !is this needed? where is area used? @@@@
!      IF (IU.EQ.1) THEN
!         DDL=DX(I)
!      ELSEIF (IU.EQ.2) THEN
!         DDL=dy(J)
!      ELSE
!         DDL=DZ(K)
!      ENDIF
!      DFF0=ZERO
!      RETURN
!      END


!======================================================================
!     Calculation of AP and AS for Batch Number Density Equation
!======================================================================
      SUBROUTINE PFLO2C(MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AREA(3)
      DO 150 I=MI1,MI3,2
      ID2=I/2
      DO 150 J=NJ1,NJ3,2
      JD2=J/2
      DO 150 K=LK1,LK3,2
         KD2=K/2
         AS(ID2,JD2,KD2,1:3,1:2)=ZERO
         DFS=ZERO
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.GE.1) THEN
            AP(ID2,JD2,KD2)=ONE
            GOTO 150
         ENDIF
         AP(ID2,JD2,KD2)=ZERO
         DN0=P1(ID2,JD2,KD2,KL)%DN
         ASM=ZERO
         AREA(1)=AREA_C(I,J,K,1)
         AREA(2)=AREA_C(I,J,K,2)
         AREA(3)=AREA_C(I,J,K,3)
         DO 130 IU=1,3
            !IF (IU.EQ.2.AND.NP.LE.6) GOTO 130    !old 2D mode
            !IF (IU.EQ.3.AND.LP.LE.6) GOTO 130
            G1=ZERO
            DU0=P1(ID2,JD2,KD2,KL)%U(IU)
            DO M=-1,1,2
               M1=(M+3)/2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               DU1=P1(ID21,JD21,KD21,KL)%U(IU)
               DN1=P1(ID21,JD21,KD21,KL)%DN
               IBC1=MOD(IBCELL(I1,J1,K1),10)
               IF (IBC1.NE.1) THEN
                  IF (IBC1.EQ.2) THEN
                     DUK=DU1
                  ELSE
                     IF (DN1.LT.SMALL) DU1=DU0
                     IF (DN0.LT.SMALL) DU0=DU1
                     CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                     DUK=F1*DU1+F2*DU0
                  ENDIF
                  !IF (IBC1.EQ.2.OR.IBC1.EQ.3) THEN
                  !   DIFF=ZERO
                  !ELSE
                  !   DIFF=DFF0(I1,J1,K1,KL,IU)
                  !ENDIF
                  FL=DUK*AREA(IU)
                  FL1=-FL*M
                  !FL2=LG(I1,J1,K1)%U(IU)*AREA(IU)
                  !AS(ID2,JD2,KD2,IU,M1)=DA(DIFF,FL2)+MAX(ZERO,FL1) !da returns diff and diff = 0               
                  AS(ID2,JD2,KD2,IU,M1)=MAX(ZERO,FL1)                
               ELSE
                  FL=DU1*AREA(IU)
                  FL1=-FL*M
                  !AS(ID2,JD2,KD2,IU,M1)=ZERO
                  AS(ID2,JD2,KD2,IU,M1)=max(ZERO,fl1) !should allow particles to leave into a wall 8-2-06
               ENDIF
               DFS=DFS-FL1
               G1=G1+AS(ID2,JD2,KD2,IU,M1)
               ASM=ASM+AS(ID2,JD2,KD2,IU,M1)
            ENDDO
            AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+G1
130      CONTINUE
         VOL=VOL_C(I,J,K)
         AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+DFS-SFP(ID2,JD2,KD2)*VOL
         BSTMP=BS(ID2,JD2,KD2)*VOL
         IF (.NOT.STEADY) THEN
            IF (KL.LE.NPS_1) THEN
               G0=P1T(ID2,JD2,KD2,KL)%DN
            ENDIF
            !BSTMP=BSTMP+G0*TRN/DTM*VOL
            BSTMP=BSTMP+G0/DTM*VOL
            !AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+TRN/DTM*VOL
            AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+vol/DTM
         ENDIF
!----------------------------------------------------------------------
!     Stagnation Point Treatment
!----------------------------------------------------------------------
         IF (ASM.LE.SMALL) THEN
            AP(ID2,JD2,KD2)=ONE
            BS(ID2,JD2,KD2)=ZERO
            GOTO 150
         ELSEIF (ABS(AP(ID2,JD2,KD2)).LT.1.0D-15) THEN
            AP(ID2,JD2,KD2)=ONE
            BS(ID2,JD2,KD2)=ZERO
            !CSL            BS(ID2,JD2,KD2)=BSTMP
            !CSL            AP(ID2,JD2,KD2)=ASM
            GOTO 150
         ELSE
            APTMP=AP(ID2,JD2,KD2)
         ENDIF
         AP(ID2,JD2,KD2)=APTMP/RF(NEL)
         BS(ID2,JD2,KD2)=BSTMP+RFC(NEL)*FZ(ID2,JD2,KD2)*AP(ID2,JD2,KD2)
150   CONTINUE
      RETURN
      END


!======================================================================
!     Calculation of AP and AS for Batch Momentum and Energy Equations
!======================================================================
      SUBROUTINE PFLO3C(LD,MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AREA(3)
      DO 350 I=MI1,MI3,2
      ID2=I/2
      DO 350 J=NJ1,NJ3,2 
      JD2=J/2
      DO 350 K=LK1,LK3,2
         KD2=K/2
         IBC0=MOD(IBCELL(I,J,K),10)
         AS(ID2,JD2,KD2,1:3,1:2)=ZERO
         DN0=P1(ID2,JD2,KD2,KL)%DN
!--- Non-calculation cells               
         IF (IBC0.GE.1) THEN
            AP(ID2,JD2,KD2)=ONE
            GOTO 350
         ENDIF
!----------------------------------------------------------------------
         AP(ID2,JD2,KD2)=ZERO
         DFS=ZERO
         ASM=ZERO
         AREA(1)=AREA_C(I,J,K,1)
         AREA(2)=AREA_C(I,J,K,2)
         AREA(3)=AREA_C(I,J,K,3)
         DO 330 IU=1,3
            IF (IU.EQ.2.AND.NP.LE.6) GOTO 330
            IF (IU.EQ.3.AND.LP.LE.6) GOTO 330
            DU0=P1(ID2,JD2,KD2,KL)%U(IU)
            DO M=-1,1,2
               M1=(M+3)/2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               DN1=P1(ID21,JD21,KD21,KL)%DN
               DU1=P1(ID21,JD21,KD21,KL)%U(IU)
               IBC1=MOD(IBCELL(I1,J1,K1),10)
               IF (IBC1.NE.1) THEN
                  IF (IBC1.EQ.2) THEN
                     DUK=DU1
                     DNK=DN1
                  ELSE
                     IF (DN1.LT.SMALL) DU1=DU0
                     IF (DN0.LT.SMALL) DU0=DU1
                     CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                     DUK=F1*DU1+F2*DU0
                     !CSL                  IF (ABS(DUK).LE.1.0D-10) DUK=ZERO
                     IF (M*DUK.GE.ZERO) THEN
                        DNK=DN0
                     ELSE
                        DNK=DN1
                     ENDIF
                  ENDIF
                  FL=DUK*DNK*AREA(IU)
                  FL1=-FL*M
                  AS(ID2,JD2,KD2,IU,M1)=MAX(ZERO,FL1)
                  IF (LD.EQ.4.AND.IBC1.LE.0) THEN
                     DIFF=0
                     FL2=LG(I1,J1,K1)%U(IU)*AREA(IU)
                     AS(ID2,JD2,KD2,IU,M1)=AS(ID2,JD2,KD2,IU,M1)+DA(DIFF,FL2)
                  ENDIF
               ELSE
                  FL1=ZERO
               ENDIF
               DFS=DFS-FL1
               AP(ID2,JD2,KD2)=AP(ID2,JD2,KD2)+AS(ID2,JD2,KD2,IU,M1)
               ASM=ASM+AS(ID2,JD2,KD2,IU,M1)
            ENDDO
330      CONTINUE
         VOL=VOL_C(I,J,K)
!----------------------------------------------------------------------
!     Stagnation Point Treatment
!----------------------------------------------------------------------
         BSTMP=BS(ID2,JD2,KD2)*VOL
         APTMP=AP(ID2,JD2,KD2)-SFP(ID2,JD2,KD2)*VOL
         IF (.NOT.STEADY) THEN
            IF (LD.EQ.4) THEN
               G0=P1T(ID2,JD2,KD2,KL)%T
            ELSE
               G0=P1T(ID2,JD2,KD2,KL)%U(LD)
            ENDIF
            !BSTMP=BSTMP+P1T(ID2,JD2,KD2,KL)%DN*G0*TRN/DTM*VOL
            !APTMP=APTMP+P1T(ID2,JD2,KD2,KL)%DN*TRN/DTM*VOL
            BSTMP=BSTMP+P1T(ID2,JD2,KD2,KL)%DN*G0/DTM*VOL
            APTMP=APTMP+P1T(ID2,JD2,KD2,KL)%DN*vol/DTM
         ENDIF
         IF (ABS(APTMP).LT.1.0D-15) THEN
            AS(ID2,JD2,KD2,1:3,1:2)=ZERO
            AP(ID2,JD2,KD2)=1
            IF (LD.EQ.4) THEN
               BS(ID2,JD2,KD2)=TMLTR
            ENDIF
         ELSE
            AP(ID2,JD2,KD2)=APTMP/RF(NEL)
            BS(ID2,JD2,KD2)=BSTMP+RFC(NEL)*FZ(ID2,JD2,KD2)*AP(ID2,JD2,KD2)
         ENDIF 
350   CONTINUE
      RETURN
      END
!======================================================================
!     Solve number density equation on surface using marching algorithm
!======================================================================
subroutine solve_nd
use gbl_var
implicit double precision (a-h,o-z)

k=lp-2; kd2=k/2 !index of surface layer of cells in melt

!Traverse charger face list
do icf=1,ncf
   i=Lcharg(icf,1); id2=i/2; j=Lcharg(icf,2); jd2=j/2

   dn0 = p1(id2,jd2,kd2,kl)%dn
   pmass = wpi_1(kl)

   select case (Lcharg(icf,3)) !charger direction

   case (1) !positive x-direction
        !fz2(id2+1,jd2,kd2) = max(dn0 - p1(id2+1,jd2,kd2,kl)%mr*dx(i+2)/(p1(id2+1,jd2,kd2,kl)%u(1)*pmass),zero)
        fz(id2+1,jd2,kd2) = max(dn0 - p1(id2+1,jd2,kd2,kl)%mr*dx(i+2)/(p1(id2+1,jd2,kd2,kl)%u(1)*pmass),zero)
        do ic=id2+2,mz
           if (ibcell(ic*2,j,k).ne.zero) exit
           !fz2(ic,jd2,kd2) = max(fz2(ic-1,jd2,kd2) - p1(ic,jd2,kd2,kl)%mr*dx(ic*2)/(p1(ic,jd2,kd2,kl)%u(1)*pmass),zero)
           fz(ic,jd2,kd2) = max(fz(ic-1,jd2,kd2) - p1(ic,jd2,kd2,kl)%mr*dx(ic*2)/(p1(ic,jd2,kd2,kl)%u(1)*pmass),zero)
        enddo
   case (2) !negative x-direction
        fz(id2-1,jd2,kd2) = max(dn0 - p1(id2-1,jd2,kd2,kl)%mr*dx(i-2)/(abs(p1(id2-1,jd2,kd2,kl)%u(1))*pmass),zero)
        do ic=id2-2,2,-1
           if (ibcell(ic*2,j,k).ne.zero) exit
           fz(ic,jd2,kd2) = max(fz(ic+1,jd2,kd2) - p1(ic,jd2,kd2,kl)%mr*dx(ic*2)/(abs(p1(ic,jd2,kd2,kl)%u(1))*pmass),zero)
        enddo
   case (3) !positive y-direction
        fz(id2,jd2+1,kd2) = max(dn0 - p1(id2,jd2+1,kd2,kl)%mr*dy(j+2)/(p1(id2,jd2+1,kd2,kl)%u(2)*pmass),zero)
        do jc=jd2+2,nz
           if (ibcell(i,jc*2,k).ne.zero) exit
           fz(id2,jc,kd2) = max(fz(id2,jc-1,kd2) - p1(id2,jc,kd2,kl)%mr*dy(jc*2)/(p1(id2,jc,kd2,kl)%u(2)*pmass),zero)
        enddo
   case (4) !negative y-direction
        fz(id2,jd2-1,kd2) = max(dn0 - p1(id2,jd2-1,kd2,kl)%mr*dy(j-2)/(abs(p1(id2,jd2-1,kd2,kl)%u(2))*pmass),zero)
        do jc=jd2-1,2,-1
           if (ibcell(i,jc*2,k).ne.zero) exit
           fz(id2,jc,kd2) = max(fz(id2,jc+1,kd2) - p1(id2,jc,kd2,kl)%mr*dx(jc*2)/(abs(p1(id2,jc,kd2,kl)%u(2))*pmass),zero)
        enddo

   end select

enddo

return
end