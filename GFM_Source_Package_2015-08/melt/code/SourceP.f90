! SOURCEP.F90
!======================================================================
!     Calculates source terms for each batch governing equation
!       6/01
!
!     If nel_p=1 then doing cullet, else doing sand
!======================================================================
      SUBROUTINE SOURCEP(MI1,MI3,NJ1,NJ3,LK1,LK3,nel_p)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IF (NPS_1.LE.0) RETURN
      GOTO(100,200,200,200,500),NEL-12
      RETURN

!----------------------------------------------------------------------
!     Source term for particle number density equation
!----------------------------------------------------------------------
100   IF (KL.GT.NPS_1) RETURN
      KR=MIN(KL+1,NPS_1)
      DO 150 I=MI1,MI3,2
      ID2=I/2
      DO 150 J=NJ1,NJ3,2
      JD2=J/2
      DO 150 K=LK1,LK3,2
         KD2=K/2
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         IF (IBCELL(I,J,K).GE.1) GOTO 150
         PMR0=P1(ID2,JD2,KD2,KL)%MR !vol specific melt rate (kg/s m**3)
         IF (PMR0.LE.ZERO) GOTO 120
         pmass=WPI_1(KL) !particle mass (kg/#)
         IF (pmass.LE.ZERO) GOTO 150
         BS(ID2,JD2,KD2)=-PMR0/pmass
120      CONTINUE
         !This code accounts for gain in number density from melting in next larger size group
         !It does not look correct, since particle should move to a smaller size group when 
         !a shell, dr, of mass melts. This should be similar to droplet vaporization. Compare
         !to that before using this code. Currently, particles have only one size group so
         !the code below should never add a quantity to the source term.
         !IF(NPS_1.GT.1.AND.WPI_1(NPS_1).EQ.WPI_1(NPS_1-1)) goto 150
         !IF (KR.LE.KL) GOTO 150
         !PMR0=P1(ID2,JD2,KD2,Kr)%MR
         !IF (PMR0.LE.ZERO) GOTO 150
         !pmass=WPI_1(KR)
         !IF (pmass.LE.ZERO) GOTO 150
         !BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+PMR0/pmass
150   CONTINUE
      RETURN

!----------------------------------------------------------------------
!     Source term for batch velocity
!
!       Drag formulation needs to be checked before use, Lottes 12/10/05 @@@
!
!     Note: The current batch model assumes that particles stay on the surface
!           and move straight away from a charger until they melt. So the 
!           particle velocity stays constant, and there are currently
!           no source or sink terms. So this code need not be executed.
!           8-2-06 Lottes 
!
!----------------------------------------------------------------------
200   IU=NEL-13
      DO 250 I=MI1,MI3,2
      ID2=I/2
      DO 250 J=NJ1,NJ3,2
      JD2=J/2
      DO 250 K=LK1,LK3,2
         KD2=K/2
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         IF (IBCELL(I,J,K).GE.1) CYCLE
         IBC1=MOD(IBCELL(I,J,K+2),10)
         IF (K.LE.LPM2.AND.IBC1.EQ.1) CYCLE
         DN0=P1(ID2,JD2,KD2,KL)%DN
         IF (DN0.LE.DN_MN) CYCLE
            !CSL         IF (IU.NE.ICHARGE.AND.P10(ID2,JD2,KD2)%TH.GE.0.1D+0) CYCLE
            !         IF (IU.NE.ICHARGE) CYCLE ! Golchert fall 2004
         BS(ID2,JD2,KD2)=GRA(IU)*DN0*(1-LG(I,J,K)%DS/P1A%DS) !Lottes 1-17-06 - formulation here needs checking
         !Buoyancy currently does not matter because there is no drag term to bring particles below the surface
         !and particles enter at the surface
!---  intefacial drag
         GMUX=LG(I,J,K)%MU
         !ZXKTEMP=0.15D+0*(REYD*RP_1(KL)*LG(I,J,K)%DS*ABS(LG(I,J,K)%U(IU)-FZ(ID2,JD2,KD2))/GMUX)**0.687D+0
         ZXKTEMP=0.15D+0*(2*RP_1(KL)*LG(I,J,K)%DS*ABS(LG(I,J,K)%U(IU)-FZ(ID2,JD2,KD2))/GMUX)**0.687D+0
         ZXK=ONE+ZXKTEMP
         !CSL         IF (P1(ID2,JD2,KD2,KL)%T.GE.TM_1(KL)-1.0D-10
         !CSL     &      .AND.P1A%H.GT.SMALL) THEN
         !CSL            BK=LG(I,J,K)%C/P1A%H*(LG(I,J,K)%T-TM_1(KL))*T0/(T0-TL0)
         !CSL            BK=MAX(BK,ZERO)
         !CSL            ZXK=ZXK/(ONE+BK)
         !CSL         ENDIF
         !GDZ=pi6/RFG_1*ZXK*GMUX*DN0/RP2_1(KL)*LG(I,J,K)%TH
         GDZ=pi6/p1a%ds*ZXK*GMUX*DN0/RP2_1(KL)*LG(I,J,K)%TH
         IF (IBC1.EQ.4) THEN !Lottes 4/19/05: below applies to surface cells only
            AR0=PI4*RP2_1(KL)*DN0
            AR0=AR0/AREA_C(I,J,K,2)
            AR0=MAX(AR0,ONE)
            GDZ=GDZ/AR0
         ENDIF
      !Lottes 4/19/05: The drag between batch particles and liquid glass
      !                is commented out below.
         !CSL         IF (IU.EQ.1) THEN
         !CSL            SFP(ID2,JD2,KD2)=-GDZ
         !CSL            BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+GDZ*LG(I,J,K)%U(IU)
         !CSL      ENDIF
         !csl         IF (IU.NE.ICHARGE) CYCLE

         !Lottes 4/20/05: Turn drag for all directions on
         ! if drag flag set, leave off for debugging
         if (idrag_on==1) then
            SFP(ID2,JD2,KD2)=-GDZ
            BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+GDZ*LG(I,J,K)%U(IU)
         endif
!---  solid pressure - Shen Lin's model, currently sp (solid pressure source) is zero.
         IF (IU.EQ.3.OR.ABS(SP(I,J,IU)).LT.SMALL) CYCLE
         DSP=SP(I,J,IU)
         IF (IU.EQ.1) DDL=DX(I)
         IF (IU.EQ.2) DDL=dy(J)
         BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+DSP/SP0_1/RP3_1(KL)/DDL
250   CONTINUE
      RETURN

!----------------------------------------------------------------------
!     Source term for batch temperature
!----------------------------------------------------------------------
500   continue
      !For TC21 only !@@@@@@@@@@@@@@ Lottes 2-20-06
      !Tref_cl=303 !Solid specific heat = 30/117 (Tsolid - Tref_cl) + 1000 (J/kg K) TC21 -RR4a spec
      !cl_slope=30.0d+0/117.0d+0
      !Need to add cl(T) function for solids
      !Function was added summer 2006. Lottes

      DO I=MI1,MI3,2;      ID2=I/2
      DO J=NJ1,NJ3,2;      JD2=J/2
      DO K=LK1,LK3,2;      KD2=K/2
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         DN0=P1(ID2,JD2,KD2,KL)%DN
         IF (IBCELL(I,J,K).GE.1) THEN
            BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
            cycle
         ELSEIF (DN0.LE.DN_MN) THEN !Lottes: Almost no particles in cell
            BS(ID2,JD2,KD2)=TM_1(KL) !Simply sets source term to melt point T
            cycle
         ENDIF
         IBC1=MOD(IBCELL(I,J,K+2),10)

         !get solid specific heat
         if (nel_p==1) then
            udf => udf_clc                  
            call udf_int(udf_clcn,fz(id2,jd2,kd2),particle_cl)          
         else
            udf => udf_cls                                          
            call udf_int(udf_clsn,fz(id2,jd2,kd2),particle_cl)          
         endif

         IF (IBC1 == 4) THEN !Surface cells only
            !Lottes 4/28/05: All radiation energy dumped into heating particles
            !       no check for particles at the melt temperature
            !if (p1(id2,jd2,kd2,kL)%T < tm_1(kL)*0.99d+0) then !check added 5/24/05, Lottes
            !This is ok because any excess is put back into melting energy in the
            !melt rate computation. Trying to cut energy at melt point here causes
            !a cyclic instability.
               VOL=VOL_C(I,J,K)
               !cl_solid=cl_slope*(fz(id2,jd2,kd2)-Tref_cl)+1000 !TC21 only
               QS0=QRSP_1(ID2,JD2,KL)
               IF (QS0.LE.ZERO) cycle
               BS(ID2,JD2,KD2)=QS0/VOL/(WPI_1(KL)*particle_cl) 
               !BS(ID2,JD2,KD2)=QS0/VOL/(WPI_1(KL)*p1a%cl) 
               !BS(ID2,JD2,KD2)=QS0/VOL/(WPI_1(KL)*cl_solid) !TC21 only @@@ Lottes
            !endif
         ELSE !case not currently used, no particles below surface, must review before use! @@@
            GMUX=LG(I,J,K)%MU
            GDFX=ONE
            SVEL1=SVELC(I,J,K,KL)
            !RELK=REYD*RP_1(KL)*LG(I,J,K)%DS*SVEL1/GMUX
            RELK=2*RP_1(KL)*LG(I,J,K)%DS*SVEL1/GMUX
            !SCNK=SCN*GMUX/LG(I,J,K)%DS/GDFX
            SCNK=GMUX/LG(I,J,K)%DS/GDFX
            !            PRNK=GMUX*GMU0*LG(I,J,K)%C/GK0_1
            DNUCK=(TWO+0.654D+0*SQRT(RELK)*SCNK**THIRD)
            !            DNUCK=(TWO+0.654D+0*SQRT(RELK)*PRNK**THIRD)
            !GDZ=one/P1A%CL*GVN_1/RFG_1*DNUCK*DN0/RP2_1(KL)
            GDZ=pi2*gk0_1/particle_cl/p1a%ds*DNUCK*DN0/RP2_1(KL)
            !GDZ=GDZ*RF(4)
            gdz=zero
            BS(ID2,JD2,KD2)=GDZ*LG(I,J,K)%T
            SFP(ID2,JD2,KD2)=-GDZ
         ENDIF

      enddo;enddo;enddo

      return
      end

