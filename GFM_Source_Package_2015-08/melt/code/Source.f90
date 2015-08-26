! SOURCE.F90
!     Calculates source terms for each liquid glass governing equation
!       10/00
!======================================================================
      SUBROUTINE SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA SMX0/1.0D-14/    

      GOTO(100,100,100,400,400,600),NELG
      RETURN

!----------------------------------------------------------------------
!     Source term for velocity
!     IU1=1  X-DIRECTION 
!         2  Y or R-DIRECTION 
!         3  Z-DIRECTION 
!----------------------------------------------------------------------
!Lottes 5/6/05 - needless tangle, we should have source_v form momentum
100   IU1=NELG
      DO 150 I=MI1,MI3,2
      DO 150 J=NJ1,NJ3,2
      DO 150 K=LK1,LK3,2
         CALL SRC1(I,J,K,IU1)
150   CONTINUE
      RETURN

!----------------------------------------------------------------------
!     Source term for 
!        NELG=4: pressure eq.
!             5: pressure correction eq.
!----------------------------------------------------------------------
400   IF (NELG.EQ.5) THEN
         SMX=ZERO
         AVEB=ZERO
         IMX=2
         JMX=2
         KMX=2
      ENDIF

      DO I=MI1,MI3,2; IP1=I+1; IM1=I-1; ID2=I/2
      DO J=NJ1,NJ3,2; JP1=J+1; JM1=J-1; JD2=J/2
      DO K=LK1,LK3,2; KP1=K+1; KM1=K-1; KD2=K/2

         SFP(I,J,K)=ZERO
         IF (IBCELL(I,J,K).GE.1) CYCLE
         AREA=AREA_C(I,J,K,1)
         FRX1=LG(IM1,J,K)%TH*LG(IM1,J,K)%DS*LG(IM1,J,K)%U(1)*area
         frx2=LG(IP1,J,K)%TH*LG(IP1,J,K)%DS*LG(IP1,J,K)%U(1)*AREA
         rx = frx1-frx2

         AREA=AREA_C(I,J,K,2)
         FRY1=LG(I,JM1,K)%TH*LG(I,JM1,K)%DS*LG(I,JM1,K)%U(2)*area
         fry2=LG(I,JP1,K)%TH*LG(I,JP1,K)%DS*LG(I,JP1,K)%U(2)*AREA
         ry = fry1-fry2

         AREA=AREA_C(I,J,K,3)
         FRZ1=LG(I,J,KM1)%TH*LG(I,J,KM1)%DS*LG(I,J,KM1)%U(3)*area
         frz2=LG(I,J,KP1)%TH*LG(I,J,KP1)%DS*LG(I,J,KP1)%U(3)*AREA
         rz = frz1-frz2

         VOL=VOL_C(I,J,K)
         !Lottes 5/24/05: This melting pressure source may not be valid
         !                in a tank with a free surface. @@@
         EVMS=LG(I,J,K)%MR*VOL
         IF (NBS0.GE.1) EVMS=EVMS-CON(ID2,JD2,KD2)*VOL
         !BS(I,J,K)=(RX+RY+RZ+EVMS)/EUN
         BS(I,J,K)=RX+RY+RZ+EVMS

         !Calculate normalized mean and max mass residual
         IF (NELG.NE.5) CYCLE
         IBC1=MOD(IBCELL(IM1,J,K),10)
         IBC2=MOD(IBCELL(IP1,J,K),10)
         IF (IBC1.EQ.3.OR.IBC2.EQ.3) CYCLE
         IBC1=MOD(IBCELL(I,JM1,K),10)
         IBC2=MOD(IBCELL(I,JP1,K),10)
         IF (NP.GE.6.AND.(IBC1.EQ.3.OR.IBC2.EQ.3)) CYCLE
         IBC1=MOD(IBCELL(I,J,KM1),10)
         IBC2=MOD(IBCELL(I,J,KP1),10)
         IF (LP.GE.6.AND.(IBC1.EQ.3.OR.IBC2.EQ.3)) CYCLE

         fmax=max(abs(frx1),abs(frx2),abs(fry1),abs(fry2),abs(frz1),abs(frz2),evms)
         if(fmax==zero)then
            bsn=zero
         else
            !BSN=ABS(BS(I,J,K))*eun/fmax
            BSN=ABS(BS(I,J,K))/fmax
         endif
         IF (BSN > SMX) THEN
            IMX=I
            JMX=J
            KMX=K
            SMX=BSN
         ENDIF
         AVEB=AVEB+BSN
	   res_mass(id2,jd2,kd2) = bsn
      enddo;enddo;enddo

      IF (NELG.EQ.5) AVEB=AVEB/NCELLS
      RETURN

!----------------------------------------------------------------------
!     Source term for  
!       NELG=6: glass enthalpy
!----------------------------------------------------------------------
600   continue
      !QRS0=0 !Lottes 11/29/05
      !QRSLG0=0 Lottes 5/5/05
      QRSCD=0
      QRSQL=0
      DO 660 I=MI1,MI3,2
      ID2=I/2
      DO 660 J=NJ1,NJ3,2
      JD2=J/2
      DO 660 K=LK1,LK3,2
         BS(I,J,K)=ZERO
         SFP(I,J,K)=ZERO
         IF (IBCELL(I,J,K).GE.1) THEN
            BS(I,J,K)=FZ(I,J,K)
            GOTO 660
         ENDIF
         KD2=K/2
!----------------------------------------------------------------------
!     QRSLG - surface radiation to liquid glass in watts for a given cell face, Lottes 5/5/05
!     QRSLG - SURFACE HEAT FLUX - No! - Lottes 5/5/05
!     QN - Net radiation heat flux (absorption - emission)
!----------------------------------------------------------------------
         VOL=VOL_C(I,J,K)
         IBC1=IBCELL(I,J,K+1)
         IF (IBC1.EQ.4) THEN !Done for surface cells only
            !cbg         G0=QRSLG(ID2,JD2)+QN(ID2,JD2,KD2)
            G0=QRSLG(ID2,JD2)
          ! Lottes 3/23/2005: requirement that qrslg>=0 added by Golchert
          ! fall 2004 
          ! QN is for rad heat transfer in melt - not functioning yet ?
            !IF (G0.LT.0) G0=0
            BS(I,J,K)=BS(I,J,K)+G0
            !QRS0=QRS0+QRSLG(ID2,JD2) !Lottes 11/29/05
            !QRSLG0=QRSLG0+QRSLG(ID2,JD2) !Lottes 5/5/05
            IF (NPS_C.GE.1) THEN !Lottes: sink for energy conducted to cullet on surface
               BS(I,J,K)=BS(I,J,K)-QCD_C(ID2,JD2,0)
               !QRS0=QRS0-QCD_C(ID2,JD2,0) !Lottes 11/29/05
               QRSCD=QRSCD+QCD_C(ID2,JD2,0)
            ENDIF
            IF (NPS_S.GE.1) THEN !Lottes: sink for energy conducted to sand on surface
               BS(I,J,K)=BS(I,J,K)-QCD_S(ID2,JD2,0)
               !QRS0=QRS0-QCD_S(ID2,JD2,0) !Lottes 11/29/05
               QRSCD=QRSCD+QCD_S(ID2,JD2,0)
            ENDIF
         ELSE
            !cbg         BS(I,J,K)=BS(I,J,K)+QN(ID2,JD2,KD2)
         ENDIF
         BS(I,J,K)=BS(I,J,K)-QLS(ID2,JD2,KD2) !Energy sink from conduction through walls
         BS(I,J,K)=BS(I,J,K)/VOL
         !QRS0=QRS0-QLS(ID2,JD2,KD2) !Lottes 11/29/05
         QRSQL=QRSQL+QLS(ID2,JD2,KD2)
!----------------------------------------------------------------------
!        Sensible enthalpy coming in with melting sand or cullet
!----------------------------------------------------------------------
         !Lottes 4/28/05: These formulas should account for enthalpy entering
         !       the glass with melting sand or cullet
         !       as below they are not correct.
         !H0_LG=CL_G*CP0*TMLTR !Moved here to make it local, Lottes 5/11/05
         !inew_src=1
			    !if(inew_src==0)then
			    !   !H0_LG=Lg(i,j,k)%c*CP0*TMLTR !use user input c, Lottes 5/11/05
			    !   !Simplify this -- Lottes 1/10/06 - @@@@!
			    !   H0_LG=Lg(i,j,k)%c*TMLTR !use user input c, Lottes 5/11/05
			    !   IF (NPS_C.GE.1.AND.PC0MR_LG(ID2,JD2,KD2).GT.ZERO) THEN
			    !      ELHG=PC0MR_LG(ID2,JD2,KD2)*(H0_LG-Lg(i,j,k)%c*(LG(I,J,K)%T-TMLTR)) !Use user input c
			    !      !&         *(H0_LG-CL_G*(LG(I,J,K)%T-TMLTR)) !Lottes 5/11/05
			    !      BS(I,J,K)=BS(I,J,K)+ELHG
			    !   ENDIF
			    !   IF (NPS_S.GE.1.AND.PS0MR_LG(ID2,JD2,KD2).GT.ZERO) THEN
			    !      ELHG=PS0MR_LG(ID2,JD2,KD2)*(H0_LG-Lg(i,j,k)%c*(LG(I,J,K)%T-TMLTR)) !Use user input c
			    !      !&         *(H0_LG-CL_G*(LG(I,J,K)%T-TMLTR)) !Lottes 5/11/05
			    !      BS(I,J,K)=BS(I,J,K)+ELHG
			    !   ENDIF
				 !endif
         !Note these forumlas do not work if size groups
         !or sand and cullet have different melting points. @@@ 

         !if(inew_src==1)then
         if (nps_c > 0 .and. pc0mr_Lg(id2,jd2,kd2) > zero) then 
            !bs(i,j,k) = bs(i,j,k) + pc0mr_Lg(id2,jd2,kd2)*Lg(i,j,k)%c*Tmltr 
            bs(i,j,k) = bs(i,j,k) + pc0mr_Lg(id2,jd2,kd2)*h_Tmltr
         endif

         if (nps_s > 0 .and. ps0mr_Lg(id2,jd2,kd2) > zero) then 
            !bs(i,j,k) = bs(i,j,k) + ps0mr_Lg(id2,jd2,kd2)*Lg(i,j,k)%c*Tmltr 
            bs(i,j,k) = bs(i,j,k) + ps0mr_Lg(id2,jd2,kd2)*h_Tmltr
         endif
         !endif


!czEB----------------------------------------------------------------------
!     ELECTRIC BooSTER HEATING
!--------NORMALIZATION????--------------------------------------------------------------
         BS(I,J,K)=BS(I,J,K)+EBQ(I,J,K)/VOL
!----------------------------------------------------------------------
!      Lottes: code below deleted by Golchert fall 2004
!     QE - RADIATION emissive heat flux
!     QA - RADIATION absorptive heat flux
!----------------------------------------------------------------------
!        BS(I,J,K)=BS(I,J,K)+
!     &      (QA(ID2,JD2,KD2)-QE(ID2,JD2,KD2))/VOL
!----------------------------------------------------------------------
660   CONTINUE
      RETURN

9000  RETURN
      END



!======================================================================
!======================================================================
!======================================================================
      !Momentum sources & sinks
      SUBROUTINE SRC1(I,J,K,IU1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VIS(3,2),DXN(3),DX1(3),DX0(3)
      DATA SMX0/1.0D-14/    
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=ZERO
         SFP(I,J,K)=ZERO  
         RETURN
      ENDIF
      IBC1=MOD(IBCELL(I,J,K+2),10)
      DXN(1)=DX(I)
      DXN(2)=dy(J)
      DXN(3)=DZ(K)
!----------------------------------------------------------------------
!     Viscous stress terms not on L.H.S.
!----------------------------------------------------------------------
      DO M=-1,1,2
         CALL VINDX(IU1,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
         IF (IU1.EQ.1) THEN
            DX0(1)=DX(I)
            DX1(1)=DX(I1)
         ELSEIF (IU1.EQ.2) THEN
            DX0(2)=dy(J)
            DX1(2)=dy(J1)
         ELSE
            DX0(3)=DZ(K)
            DX1(3)=DZ(K1)
         ENDIF
         IBC2=MOD(IBCELL(I2,J2,K2),10)
         IF (IBC2.EQ.2) THEN
            THGM=LG(I2,J2,K2)%TH*LG(I2,J2,K2)%A
            IF (IU1.EQ.1) DXN(1)=DXN(1)+ABS(X(I2)-X(I1))
            IF (IU1.EQ.2) DXN(2)=DXN(2)+ABS(y(J2)-y(J1))
            IF (IU1.EQ.3) DXN(3)=DXN(3)+ABS(Z(K2)-Z(K1))
         ELSE
            THGM=LG(I1,J1,K1)%TH*LG(I1,J1,K1)%A
         ENDIF
         IF (M.EQ.-1) THEN
            VIS(IU1,1)=THGM*(FZ(I,J,K)-FZ(I2,J2,K2))/DX1(IU1)
         ELSE
            VIS(IU1,2)=THGM*(FZ(I2,J2,K2)-FZ(I,J,K))/DX1(IU1)
         ENDIF
      ENDDO
      VMS=(VIS(IU1,2)-VIS(IU1,1))/DXN(IU1)
      DO 120 IU=1,3
         IF (IU.EQ.IU1) GOTO 120
         IF (IU.EQ.2.AND.NP.LE.6) GOTO 120
         IF (IU.EQ.3.AND.LP.LE.6) GOTO 120
         I1=I
         J1=J
         K1=K
         DO M=-1,1,2
            IF (IU.EQ.1) THEN
               I1=I+M
            ELSEIF (IU.EQ.2) THEN
               J1=J+M
            ELSE
               K1=K+M
            ENDIF
            IF (IU1.EQ.1) THEN
               DU0=FZ(I+1,J1,K1)-FZ(I-1,J1,K1)
            ELSEIF (IU1.EQ.2) THEN
               DU0=FZ(I1,J+1,K1)-FZ(I1,J-1,K1)
            ELSEIF (IU1.EQ.3) THEN
               DU0=FZ(I1,J1,K+1)-FZ(I1,J1,K-1)
            ENDIF
            M1=(M+3)/2
            VIS(IU,M1)=LG(I1,J1,K1)%TH*LG(I1,J1,K1)%A*DU0/DX0(IU1)
         ENDDO
         VMS=VMS+(VIS(IU,2)-VIS(IU,1))/DXN(IU)
120   CONTINUE
!----------------------------------------------------------------------
!       GDZ: interfacial drag
!       DUAV: momentum source from melting
!----------------------------------------------------------------------
      GDZ=ZERO
      GDZU=ZERO
      IF (KEY.EQ.1.OR.(PCA%FR2+PSA%FR2+GB%FR2).LE.ZERO) GOTO 140
      DUAV=ZERO
      DNSUM=ZERO
      !GCPA=LG(I,J,K)%C Lottes 5/10/05 not used.
      I1=I/2
      I2=I1
      J1=J/2
      J2=J1
      K1=K/2
      K2=K1
      IF (IU1.EQ.1) THEN
         I1=(I-1)/2
         I2=I1+1
         F1=(X(I+1)-X(I))/DX(I)
      ELSEIF (IU1.EQ.2) THEN
         J1=(J-1)/2
         J2=J1+1
         F1=(y(J+1)-y(J))/dy(J)
      ELSEIF (IU1.EQ.3) THEN
         K1=(K-1)/2
         K2=K1+1
         F1=(Z(K+1)-Z(K))/DZ(K)
      ENDIF
      F2=ONE-F1
      DO L=1,NPS_C
         DNK=F1*PC(I1,J1,K1,L)%DN+F2*PC(I2,J2,K2,L)%DN
         IF (DNK.LT.SMX0) CYCLE
         DUK=F1*PC(I1,J1,K1,L)%U(IU1)+F2*PC(I2,J2,K2,L)%U(IU1)
         DTK=F1*PC(I1,J1,K1,L)%T+F2*PC(I2,J2,K2,L)%T
         !ZXK=ONE+0.15D+0*(REYD*RP_C(L)*LG(I,J,K)%DS*ABS(FZ(I,J,K)-DUK)/LG(I,J,K)%MU)**0.687D+0
         ZXK=ONE+0.15D+0*(2*RP_C(L)*LG(I,J,K)%DS*ABS(FZ(I,J,K)-DUK)/LG(I,J,K)%MU)**0.687D+0
         !CSL         IF (DTK.GE.TM_C(L)-1.0D-10.AND.PCA%H.GT.ZERO) THEN
         !CSL            BK=GCPA/PCA%H*(LG(I,J,K)%T-TM_C(L))*T0/(T0-TL0)
         !CSL            BK=MAX(BK,ZERO)
         !CSL            ZXK=ZXK/(ONE+BK)
         !CSL         ENDIF
         ZXK=ZXK*pi6*LG(I,J,K)%MU*RP_C(L)*DNK*LG(I,J,K)%TH
         IF (IBC1.EQ.4) THEN
            !AR0=PI4*RP2_C(L)*RLM0*RLM0*DNK
            AR0=PI4*RP2_C(L)*DNK
            AR0=AR0/AREA_C(I,J,K,2)
            AR0=MAX(AR0,ONE)
            ZXK=ZXK/AR0
         ENDIF
         ! ZXK=ZXK*pi6*LG(I,J,K)%MU*RP_C(L)*DNK*LG(I,J,K)%TH !Golchert fall 2004
         GDZ=GDZ+ZXK
         GDZU=GDZU+ZXK*DUK  
         DUAV=DUAV+DNK*DUK   
         DNSUM=DNSUM+DNK
      ENDDO
      DO L=1,NPS_S
         DNK=F1*PS(I1,J1,K1,L)%DN+F2*PS(I2,J2,K2,L)%DN
         IF (DNK.LT.SMX0) CYCLE
         DUK=F1*PS(I1,J1,K1,L)%U(IU1)+F2*PS(I2,J2,K2,L)%U(IU1)
         DTK=F1*PS(I1,J1,K1,L)%T+F2*PS(I2,J2,K2,L)%T
         !ZXK=ONE+0.15D+0*(REYD*RP_S(L)*LG(I,J,K)%DS*ABS(FZ(I,J,K)-DUK)/LG(I,J,K)%MU)**0.687D+0
         ZXK=ONE+0.15D+0*(2*RP_S(L)*LG(I,J,K)%DS*ABS(FZ(I,J,K)-DUK)/LG(I,J,K)%MU)**0.687D+0
         !CSL         IF (DTK.GE.TM_S(L)-1.0D-10.AND.PSA%H.GT.ZERO) THEN
         !CSL            BK=GCPA/PSA%H*(LG(I,J,K)%T-TM_S(L))*T0/(T0-TL0)
         !CSL            BK=MAX(BK,ZERO)
         !CSL            ZXK=ZXK/(ONE+BK)
         !CSL         ENDIF
         ZXK=ZXK*pi6*LG(I,J,K)%MU*RP_S(L)*DNK*LG(I,J,K)%TH
         IF (IBC1.EQ.4) THEN
            !AR0=PI4*RP2_S(L)*RLM0*RLM0*DNK
            AR0=PI4*RP2_S(L)*DNK
            AR0=AR0/AREA_C(I,J,K,2)
            AR0=MAX(AR0,ONE)
            ZXK=ZXK/AR0
         ENDIF
         !ZXK=ZXK*pi6*LG(I,J,K)%MU*RP_S(L)*DNK*LG(I,J,K)%TH !Golchert fall 2004
         GDZ=GDZ+ZXK
         GDZU=GDZU+ZXK*DUK  
         DUAV=DUAV+DNK*DUK   
         DNSUM=DNSUM+DNK
      ENDDO
      IF (DNSUM.GT.SMX0) THEN
         PMR0=F1*(PC0MR_LG(I1,J1,K1)+PS0MR_LG(I1,J1,K1)) &
             +F2*(PC0MR_LG(I2,J2,K2)+PS0MR_LG(I2,J2,K2))
         DUAV=DUAV/DNSUM*PMR0
      ELSE
         PMR0=ZERO
         DUAV=ZERO
      ENDIF
140   CONTINUE
      BS(I,J,K)=VMS+GDZU+DUAV
      SFP(I,J,K)=-GDZ

      ialt_drag2=0
      if(ialt_drag2==1)then
      if (k.ne.Lp-2 .or. iu1==3 .or. area_s_spr(i/2,j/2)+area_c_spr(i/2,j/2) < area_c(i,j,k,3)) then
             bs(i,j,k)=vms+duav
             sfp(i,j,k)=zero
      else
         gdzu=gdzu*coverf(i/2,j/2)
         gdz=gdz*coverf(i/2,j/2)
         BS(I,J,K)=VMS+GDZU+DUAV
         SFP(I,J,K)=-GDZ
      endif
      endif
      !Lottes 6/8/05:
      !Alternate formulation of drag from surface layer of batch.
      !If surface layer is very thin, assume no drag effect
      !(remaining particles should flow with the liquid glass)
      !However, currently drag on particles is turned off,
      !so we leave it off here for sparse conditions also,
      !until this model can be fixed                                  @@@
      !For a thick layer, drag force for flow next to a parallel
      !plate is used.
      ialt_drag=1
      if(ialt_drag==1)then
      if (k.ne.Lp-2 .or. iu1==3) then
             bs(i,j,k)=vms+duav
             sfp(i,j,k)=zero
      elseif( area_s_spr(i/2,j/2)+area_c_spr(i/2,j/2) < area_c(i,j,k,3)) then
             bs(i,j,k)=vms+duav
             sfp(i,j,k)=zero
      else
         u_p_dir=zero
         theta_p=zero
         if(nps_s>0) then
            u_p_dir=ps(i1,j1,k1,1)%u(iu1)
            theta_p=theta_p+ps0(i/2,j/2,Lz-1)%th
         endif
         if(nps_c>0) then
            u_p_dir=pc(i1,j1,k1,1)%u(iu1)
            theta_p=theta_p+pc0(i/2,j/2,Lz-1)%th
         endif
         !theta_p=min(theta_p,one)
         theta_p=min(theta_p/th_pmx,one)
         !theta_p is a normalized measure of batch
         !shelf thickness and is used to taper off
         !the drag effect of batch as a wall in the zone
         !where the shelf becomes thin and starts to break up.
         !if (theta_p < 0.1d+0) theta_p=zero

         !bs(i,j,k)=vms+duav+Lg(i,j,k)%mu*gmu0*u_p_dir/( z(Lp) - z(Lp-2) ) &
         bs(i,j,k)=vms+duav+Lg(i,j,k)%mu*u_p_dir/( z(Lp) - z(Lp-2) ) &
                  * area_c(i,j,k,3)/(vol_c(i,j,k))*theta_p
                  !&                *coverf(i/2,j/2)/(dnst0*ug0)   

         !sfp(i,j,k)= - Lg(i,j,k)%mu*gmu0/(z(Lp)-z(Lp-2)) * area_c(i,j,k,3)/(vol_c(i,j,k))*theta_p
         sfp(i,j,k)= - Lg(i,j,k)%mu/(z(Lp)-z(Lp-2)) * area_c(i,j,k,3)/(vol_c(i,j,k))*theta_p
                     !&                *coverf(i/2,j/2)/(dnst0*ug0)   
      endif
      endif


!----------------------------------------------------------------------
      if(ialtbuoy==0)then
         !Test turning off buoyancy in the exit tunnel, no refiner case
         !if(i <= i_me)then !won't work for refiners !@@@@@
         !   BUOY=(LG(I,J,K)%DS-DS0)*GRA(IU1)
         !else
	      !   buoy=zero
         !endif
         
         !BUOY=(LG(I,J,K)%DS-DS0)*GRA(IU1)
         BUOY=(LG(I,J,K)%DS)*GRA(IU1)
         
         !CSL      IF (IU1.EQ.3.AND.IBCELL(I,J,K+2).EQ.304
         !CSL     &    .AND.I.GT.I_ME) THEN
         !      IF (IU1.EQ.3.AND.I.GE.I_RB) THEN !Golchert fall 2004
         !         BUOY=BUOY-GRA(IU1)*F_BUOY
         !     ENDIF
      else
         buoy=-gra(iu1)*(z(Lp)-z(k))*(lg(i,j,k+1)%ds-lg(i,j,k-1)%ds)/dz(k)
      endif

      BS(I,J,K)=BS(I,J,K)+BUOY

!Cz----------------------------------------------------------------------
!Cz       DUAB: momentum source from bubbler
!Cz----------------------------------------------------------------------
      ! enter at cell i,j,k
      !IF (IU1.LT.3) RETURN ! return if not z momentum
      !NBC0=0
      !DO K1=2,K-1,2 !look at cells below k position to see if one is the top (outlet) of a bubbler
      !   IBC0=MOD(IBCELL(I,J,K1),10) 
      !   IF (IBC0.EQ.8) THEN ! if ibcell/10 remainder == 8 --> bubbler outlet
      !      NBC0=IBCELL(I,J,K1)/10 ! ibcell/10 = bubbler #, index into bubbler arrays
      !      EXIT
      !   ENDIF
      !ENDDO
      !IF (NBC0.LE.0) RETURN
	  !There is a bubbler outlet below this cell, so calculate bubble bouyancy force on liquid glass
      !BUOY_BUB=ZERO
      !AREA=AREA_C(I,J,K,3) ! z-normal area of cell
      !FRAR0=100.0D+0 ! ??
      !FRAR1=BLR(NBC0)%FR/AREA !air superficial velocity (vel air would have is it filled the whole space in cell)
      !A0=0.1D+0 ! ??
      !DNSTG=LG(I,J,K)%DS !density of liquid glass
      !GRAG=ABS(GRA(IU1)) !g = 9.8 m/s**2
      !ZHG=(Z(LP)-Z(K)) !depth in melt
      !PGTOT=GRAG*ZHG+PG0+LG(I,J,K)%P*PG0 !total pressure at position i,j,k -- not right, think is should be just LG(I,J,K)%P
      !PGTOT=LG(I,J,K)%P !total pressure at position i,j,k -- not right, think is should be just LG(I,J,K)%P
      !DS_BUB(I,J,K)=(PGTOT*29.0D+0/RU/BLR(NBC0)%T) !gas density in bubble --should be (P * Mair)/(Ru * T(glass))
      !DS_BUB(I,J,K)=PGTOT*29.0D+0/RU/(LG(I,J,K)%T) !gas density in bubble --should be (P * Mair)/(Ru * T(glass))
      !CZ      THETA_BUB(I,J,K)=0.01D+0*BLR(NBC0)%FR*PG0/PGTOT
      !G0=A0*(FRAR1/FRAR0)*(PG0/PGTOT)*(LG(I,J,K)%T/3.0D+2) ! Volume fraction of gas from bubblers (I don't understand this yet)
      !G0=A0*(FRAR1/FRAR0)*(PG0/PGTOT)*(LG(I,J,K)%T) ! Volume fraction of gas from bubblers (I don't understand this yet)
      !THETA_BUB(I,J,K)=MIN(G0,0.2D+0)  !cap gas volume fraction at 20%
      !LG(I,J,K)%TH=LG(I,J,K)%TH-THETA_BUB(I,J,K) !liquid glass volume fraction is reduced by air vol. fract.
      !BUOY_BUB=THETA_BUB(I,J,K)*(DS_BUB(I,J,K)-LG(I,J,K)%DS)*GRA(IU1) ! buoyancy force per unit vol.
      !BS(I,J,K)=BS(I,J,K)+BUOY_BUB !add total buoyancy force to z momentum source term


      ! Lottes quick fix June, 2007
      ! enter at cell i,j,k
      IF (IU1.LT.3) RETURN ! return if not z momentum
      NBC0=0
      DO K1=2,K-1,2 !look at cells below k position to see if one is the top (outlet) of a bubbler
         IBC0=MOD(IBCELL(I,J,K1),10) 
         IF (IBC0.EQ.8) THEN ! if ibcell/10 remainder == 8 --> bubbler outlet
            NBC0=IBCELL(I,J,K1)/10 ! ibcell/10 = bubbler #, index into bubbler arrays
            EXIT
         ENDIF
      ENDDO
      IF (NBC0.LE.0) RETURN
	  !There is a bubbler outlet below this cell, so calculate bubble bouyancy force on liquid glass
      ! Assume bubble radius, R_bub = 0.01 m = 1 cm
	  R_bub = 0.01d+0 ![m]
	  rho_air_sc = 1.16d+0 ! [kg/m^3] density of air at standard conditions (298 K, 1 atm.)
	  w_air = 28.97 ! Molecular weight of air [kg/kmol]
	  bub_ds = Lg(i,j,k)%P * w_air/(Ru * Lg(i,j,k)%T) ! gas density assuming thermal equilibrium with glass [kg/m**3]
	  ! Calculate bubble terminal rise velocity assuming Stokes flow
	  U_bub = 2*R_bub**2*(Lg(i,j,k)%ds-bub_ds)/(9*Lg(i,j,k)%mu) ! [m/s]
      fr_mass_bub = rho_air_sc * blr(nbc0)%fr ! bubble mass flow rate [kg/s]
	  ! Calculate volume fraction of bubbles
	  bub_theta = fr_mass_bub/(bub_ds * u_bub * area_c(i,j,k,3))
	  bub_theta = min(bub_theta, 0.2d+0) ! Cap at 20% bubble volume fraction
      !Lg(i,j,k)%th = max(Lg(i,j,k)%th - bub_theta,0.8d+0) !liquid glass vol. fract. reduced by air vol. fraction in cell
      ! Calculate buoyancy force per unit volume
	  buoy_bub = max(bub_theta * (bub_ds - Lg(i,j,k)%ds)*gra(iu1),zero) ! buoyancy force must be >= 0
	  bs(i,j,k)=bs(i,j,k)+buoy_bub !add buoyancy force to z momentum source term

      RETURN
      END
