!======================================================================
! SBC.F90
!======================================================================

!======================================================================
!     SBC SETS STEADY INLET BOUNDARY CONDITIONS
!       CONC: balances species concentration
!       THER: calculates thermal properties
!       SBC3: copies values from one node to another
!       ENTH: calculates enthalpy
!       SBC5: assigns wall values
!     Rev: 6/01
!
!  Note that the sbc file was opened in the setup routine and the input
!  namelist has already been read in
!======================================================================
      SUBROUTINE SBC
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER,ALLOCATABLE :: IA1(:,:),IA2(:,:),JA1(:,:),JA2(:,:)
      INTEGER,ALLOCATABLE :: KA1(:,:),KA2(:,:),NM0(:)
      REAL*8,ALLOCATABLE :: T1_PC(:),DN1_PC(:),U1_PC(:,:)
      REAL*8,ALLOCATABLE :: T1_PS(:),DN1_PS(:),U1_PS(:,:)
      REAL*8,ALLOCATABLE :: T1_GB(:),DN1_GB(:),U1_GB(:,:)
      REAL*8,ALLOCATABLE :: TM1(:),TM2(:),TM3(:),TM4(:),TM5(:)
      REAL*8 U_0(3),V_0(3)
!----------------------------------------------------------------------
      H_TOT=ZERO
      FR2_LG=ZERO
      PCA%FR2=ZERO
      PSA%FR2=ZERO
      GB%FR2=ZERO
      IF (NPS_C.GE.1) ALLOCATE (T1_PC(NPS_C),DN1_PC(NPS_C),U1_PC(NPS_C,3))
      IF (NPS_S.GE.1) ALLOCATE (T1_PS(NPS_S),DN1_PS(NPS_S),U1_PS(NPS_S,3))
      IF (NBS0.GE.1)  ALLOCATE (T1_GB(NBS0),DN1_GB(NBS0),U1_GB(NBS0,3))
!---
!     wall and flow properties
!---
      ALLOCATE (TM1(1000),TM2(1000),TM3(1000),TM4(1000),TM5(1000))
      ALLOCATE (IA1(3,1000),IA2(3,1000),JA1(3,1000),JA2(3,1000))
      ALLOCATE (KA1(3,1000),KA2(3,1000),NM0(1000))
      READ (nu_sbc,*) TITLE
      IBC0=1 ! Wall properties
      DO N=1,1000
         READ (nu_sbc,*,ERR=100) I1,J1,K1,I2,J2,K2,IU
         IF (I1.LE.0) EXIT
!----
!        TM1 = WALL THICKNESS
!        TM2 = WALL CONDUCTIVITY
!        TM3 = EXTERNAL CONVECTIVE HEAT TRANSFER COEFFICIENT
!        TM4 = EXTERNAL AMBIENT TEMPERATURE NEAR WALL
!        TM5 = WALL EMISSIVITY
!
         READ (nu_sbc,*) TM1(N),TM2(N),TM3(N),TM4(N),TM5(N)
         CALL SBC_B
      ENDDO
100   IF (N.GT.1) THEN
      NWAL0=N-1
      ALLOCATE (W1_D(NWAL0),W1_K(NWAL0),W1_TA(NWAL0),W1_HA(NWAL0))
      ALLOCATE (W1_E(NWAL0))
      DO N=1,NWAL0
         W1_D(N)=TM1(N)
         W1_K(N)=TM2(N)
         W1_HA(N)=TM3(N)
         W1_TA(N)=TM4(N)
         W1_E(N)=TM5(N)
      ENDDO
      ENDIF
      NWL=0
!----
!     BEGIN TO READ IN THERMO PROPERTIES
!     DENSITY, VISCOSITY, GLASS CONDUCTIVITY, SPECIFIC HEAT, VOL ASBORPTIVITY
!
!     Read in material properties as a function of temperature
!     Property values used in computation are interpolated between these data points.
!     
!     The properties are:
!     
!     1: Glass density (kg/m^3)
!     2: Glass viscosity (Pa s)
!     3: Glass conductivity (W/(m K))
!     4: Liquid glass specific heat (J/(kg K))
!     5: Volume absorbtivity (1/cm) (not currently used)
!     6: Cullet specific heat (J/(kg K))
!     7: Batch specific heat (J/(kg K))
!
!----
      DO M=1,7
         DO N=1,1000 !Read in all pairs of values (T and material property value) for one material
            READ (nu_sbc,*,ERR=110) TM1(N),TM2(N)
         ENDDO
110      N1=N-1
         IF (N1.LE.0) CYCLE
         IF (M.EQ.1) THEN
            DEALLOCATE(UDF_DS)
            ALLOCATE(UDF_DS(N1))
            UDF_DSN=N1
            UDF => UDF_DS
         ELSEIF (M.EQ.2) THEN
            DEALLOCATE(UDF_MU)
            ALLOCATE(UDF_MU(N1))
            UDF_MUN=N1
            UDF => UDF_MU
         ELSEIF (M.EQ.3) THEN
            DEALLOCATE(UDF_K)
            ALLOCATE(UDF_K(N1))
            UDF_KN=N1
            UDF => UDF_K
         ELSEIF (M.EQ.4) THEN
            DEALLOCATE(UDF_CL)
            ALLOCATE(UDF_CL(N1))
            UDF_CLN=N1
            UDF => UDF_CL
         ELSEIF (M.EQ.5) THEN
            IF (ALLOCATED(WL)) DEALLOCATE(WL,AKL0)
            ALLOCATE(WL(N1),AKL0(N1))
            NWL=N1
            DO N=1,N1
               WL(N)=TM1(N)*1.0D-6
               AKL0(N)=TM2(N)*1.0D+2
            ENDDO
            CYCLE
         ELSEIF (M.EQ.6) THEN
            DEALLOCATE(UDF_CLC)
            ALLOCATE(UDF_CLC(N1))
            UDF_CLCN=N1
            UDF => UDF_CLC
         ELSEIF (M.EQ.7) THEN
            DEALLOCATE(UDF_CLS)
            ALLOCATE(UDF_CLS(N1))
            UDF_CLSN=N1
            UDF => UDF_CLS
         ENDIF
         DO N=1,N1  !Set values in the structure
            UDF(N)%T=TM1(N)
            UDF(N)%F=TM2(N)
         ENDDO
      ENDDO
      IF (NWL.EQ.0) ID_RAD=-1
      CALL RAD0(0)
!----------------------------------------------------------------------
!     Inlet flow properties (IBCELL=2)
!       LG: liquid glass mass flow rate (kg/s) (currently zero - only solids come in) Lottes 8-2-06
!       PC: Cullet mass flow rate (kg/s)
!       PS: Sand mass flow rate (kg/s)
!       GB: Gas bubble mass flow rate (kg/s)
!---------------------------------------------------------------------- 
      READ (nu_sbc,*) I_REF,J_REF,K_REF,P_REF
      IBC0=MOD(IBCELL(I_REF,J_REF,K_REF),10)
      WRITE (6,"('  Pressure reference 'F5.2' Pa is at cell ('I3','I3','I3') ')") P_REF,I_REF,J_REF,K_REF
      IF (IBC0.NE.2) GOTO 900
      !P_REF=P_REF/PG0

200   READ (nu_sbc,*,ERR=800) I1,J1,K1,I2,J2,K2,IU

      !IF (IU.EQ.1) ICHARGE=1      ! @@@@ Use of icharge is commented out, should delete variable.
      !IF (IU.EQ.2) ICHARGE=2      
      !IF (IU.EQ.3) ICHARGE=3      
      IF (I1.LE.0) GOTO 800 !Done with chargers and other inlets (bubblers)
!---
      READ (nu_sbc,*) T_0,U_0
      !T_0=T_0/T0
      !U_0=U_0/UG0
      IF (NPS_C.LE.0) GOTO 220 !Cullet
      READ (nu_sbc,*) G1,G2,V_0
      IF (G2.GT.ZERO) THEN
         IF (NPS_C.GT.1) THEN
            READ (nu_sbc,*) (DN1_PC(L),L=1,NPS_C)
         ELSE
            DN1_PC=ONE
         ENDIF
      ELSE
         DN1_PC=ZERO
      ENDIF
      DO L=1,NPS_C
         T1_PC(L)=G1
         DN1_PC(L)=DN1_PC(L)*G2
         U1_PC(L,1:3)=V_0(1:3)
      END DO
220   IF (NPS_S.LE.0) GOTO 240 !Batch with sand
      READ (nu_sbc,*) G1,G2,V_0 
      IF (G2.GT.ZERO) THEN
         IF (NPS_S.GT.1) THEN !dn1_ps is fraction of total in each size group
            READ (nu_sbc,*) (DN1_PS(L),L=1,NPS_S)
         ELSE
            DN1_PS=ONE
         ENDIF
      ELSE
         DN1_PS=ZERO
      ENDIF
      DO L=1,NPS_S
         T1_PS(L)=G1
         DN1_PS(L)=DN1_PS(L)*G2
         U1_PS(L,1:3)=V_0(1:3)
      END DO

240   continue 
      !Build charger list
      !note: if velocity is zero then the inlet block is for a bubbler, not a charger
      !      after all inlet blocks have been processed
      !      ncf = # of charger inlet faces
      !      
      !      and Lcharg(:,1) = x index of charger face
      !          Lcharg(:,2) = y index of charger face
      !          Lcharg(:,3) = face orientation (1=positive x, 2=negative x,
      !                                          3=positive y, 4=negative y)
      do i=i1,i2,2
      do j=j1,j2,2
         if (iu == 1) then !charger is x-direction
            if (v_0(1) > zero) then !charger is in positive x-direction
               ncf=ncf+1; Lcharg(ncf,1)=i; Lcharg(ncf,2)=j; Lcharg(ncf,3)=1 !add to charger face list
            elseif (v_0(1) < zero) then  !charger is in negative x-direction
               ncf=ncf+1; Lcharg(ncf,1)=i; Lcharg(ncf,2)=j; Lcharg(ncf,3)=2 !add to charger face list
            endif
         endif
         if (iu == 2) then !charger is y-direction
            if (v_0(2) > zero) then !charger is in positive y-direction
               ncf=ncf+1; Lcharg(ncf,1)=i; Lcharg(ncf,2)=j; Lcharg(ncf,3)=3 !add to charger face list
            elseif (v_0(2) < zero) then !charger is in negative y-direction
               ncf=ncf+1; Lcharg(ncf,1)=i; Lcharg(ncf,2)=j; Lcharg(ncf,3)=4 !add to charger face list
            endif
         endif
      enddo;enddo

      IF (NBS0.LE.0) GOTO 300 !Bubbles
      READ (nu_sbc,*) G1,G2,V_0
      IF (G2.GT.ZERO) THEN
         IF (NBS0.GT.1) THEN
            READ (nu_sbc,*) (DN1_GB(L),L=1,NBS0)
         ELSE
            !CZ         DN1_GB=ONE
            DN1_GB=ZERO
         ENDIF
      ELSE
         DN1_GB=ZERO
      ENDIF
      DO L=1,NBS0
         T1_GB(L)=G1
         DN1_GB(L)=DN1_GB(L)*G2
         U1_GB(L,1:3)=V_0(1:3)
      END DO
!---------------------------------------------------------------------- 
      psflo=0
300   DO 400 I=I1,I2,2
      ID2=I/2
      DO 400 J=J1,J2,2
      JD2=J/2
      DO 400 K=K1,K2,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.2) GOTO 400
         KD2=K/2
         LG(I,J,K)%P=P_REF
         LG(I,J,K)%T=T_0
         LG(I,J,K)%U(1:3)=U_0(1:3)
         LG(I,J,K)%F(IYLG)=1
         CALL DSLG(I,J,K)
         CALL THER(I,J,K)
         call Tlg_to_h(Lg(i,j,k)%T,Lg(i,j,k)%h) !Lottes 5/11/05 set h consistent with T and c.
         !Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
         !CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
         LG(I,J,K)%TH=ONE
!----------------------------------------------------------------------
!     Cullet flow
!----------------------------------------------------------------------
         IF (NPS_C.LE.0) GOTO 320
         ANR=ZERO
         DO L=1,NPS_C
            PC(ID2,JD2,KD2,L)%DN=DN1_PC(L)
            PC(ID2,JD2,KD2,L)%U(1:3)=U1_PC(L,1:3)

            !----------------------------------
            !Part of current simple batch model
            !Fill in particle velocity as fixed constant away from charger
            !Note: if the inlet face velocities are zero then the inlet is a bubbler, not a charger
            !
            if (iu == 1) then !charger is x-direction
               if (u1_pc(L,1) > zero) then !charger is positive x-direction
                  do ic=id2+1,mz
                     if (ibcell(ic*2,j,k).ne.zero) exit
                     pc(ic,jd2,kd2,L)%u(1:3)=u1_pc(L,1:3)
                  enddo
                  pc(ic,jd2,kd2,L)%u(1:3)=u1_pc(L,1:3) !any particles at the far wall hit it
               elseif (u1_pc(L,1) < zero) then         !charger is negative x-direction 
                  do ic=id2-1,2,-1
                     if (ibcell(ic*2,j,k).ne.zero) exit
                     pc(ic,jd2,kd2,L)%u(1:3)=u1_pc(L,1:3)
                  enddo
                  pc(ic,jd2,kd2,L)%u(1:3)=u1_pc(L,1:3) !any particles at the far wall hit it
               endif
            endif
            if (iu == 2) then !charger is y-direction
               if (u1_pc(L,1) > zero) then !charger is positive y-direction
                  do jc=jd2+1,nz
                     if (ibcell(i,jc*2,k).ne.zero) exit
                     pc(id2,jc,kd2,L)%u(1:3)=u1_pc(L,1:3)
                  enddo
                  pc(id2,jc,kd2,L)%u(1:3)=u1_pc(L,1:3) !any particles at the far wall hit it
               elseif (u1_pc(L,1) > zero) then         !charger is negative x-direction 
                  do jc=jd2-1,2,-1
                     if (ibcell(i,jc*2,k).ne.zero) exit
                     pc(id2,jc,kd2,L)%u(1:3)=u1_pc(L,1:3)
                  enddo
                  pc(id2,jc,kd2,L)%u(1:3)=u1_pc(L,1:3) !any particles at the far wall hit it
               endif
            endif
            !----------------------------------

            PC(ID2,JD2,KD2,L)%T=T1_PC(L)
            !G0=DN1_PC(L)*RP3_C(L)
            G0=DN1_Pc(L)*c4d3pi*RP_c(L)**3
            ANR=ANR+G0
            AREA=AREA_C(I,J,K,IU)
            G0=G0*PCA%DS*ABS(U1_PC(L,IU))*AREA
            !PCA%FR2=PCA%FR2+G0
            pca%fr2=pca%fr2+wpi_c(l)*DN1_Pc(L)*ABS(U1_Pc(L,IU))*area
            !H_TOT=H_TOT+G0*PCA%CL*T1_PC(L)
            udf => udf_clc                                              
            call udf_int(udf_clcn,T1_pc(l),cullet_cl)          
            h_tot=h_tot+g0*cullet_cl*T1_pc(l)

!----
!         ENERGY NEEDED TO HEAT CULLET
!----
           !H_NEEDED=H_NEEDED+G0*PCA%CL*(TMLTR-T1_PC(L)) !Lottes 5/19/05
           !qheat_c_need = qheat_c_need + g0*pca%cl*(Tmltr-T1_pc(L)) !Lottes 5/19/05         
           call udf_int(udf_clcn,Tmltr,cullet_cl2)          
           qheat_c_need = qheat_c_need + g0*((cullet_cl2+cullet_cl)/2)*(Tmltr-T1_pc(L))
         ENDDO
         PC0(ID2,JD2,KD2)%TH=ANR
         LG(I,J,K)%TH=LG(I,J,K)%TH-PC0(ID2,JD2,KD2)%TH
!----------------------------------------------------------------------
!     Sand flow
!----------------------------------------------------------------------
320      IF (NPS_S.LE.0) GOTO 340
         ANR=ZERO
         DO L=1,NPS_S
            PS(ID2,JD2,KD2,L)%DN=DN1_PS(L)
            PS(ID2,JD2,KD2,L)%U(1:3)=U1_PS(L,1:3)

            !---------------------------------- Lottes 8-2-06
            !Part of current simple batch model
            !Fill in particle velocity as fixed constant away from charger
            !
            if (iu == 1) then !charger is x-direction
               if (u1_ps(L,1) > zero) then !charger is positive x-direction
                  do ic=id2+1,mz
                     if (ibcell(ic*2,j,k).ne.zero) exit
                     ps(ic,jd2,kd2,L)%u(1:3)=u1_ps(L,1:3)
                  enddo
                  ps(ic,jd2,kd2,L)%u(1:3)=u1_ps(L,1:3) !any particles at the far wall hit it
               else                        !charger is negative x-direction 
                  do ic=id2-1,2,-1
                     if (ibcell(ic*2,j,k).ne.zero) exit
                     ps(ic,jd2,kd2,L)%u(1:3)=u1_ps(L,1:3)
                  enddo
                  ps(ic,jd2,kd2,L)%u(1:3)=u1_ps(L,1:3) !any particles at the far wall hit it
               endif
            endif
            if (iu == 2) then !charger is y-direction
               if (u1_ps(L,1) > zero) then !charger is positive y-direction
                  do jc=jd2+1,nz
                     if (ibcell(i,jc*2,k).ne.zero) exit
                     ps(id2,jc,kd2,L)%u(1:3)=u1_ps(L,1:3)
                  enddo
                  ps(id2,jc,kd2,L)%u(1:3)=u1_ps(L,1:3) !any particles at the far wall hit it
               else                        !charger is negative x-direction 
                  do jc=jd2-1,2,-1
                     if (ibcell(i,jc*2,k).ne.zero) exit
                     ps(id2,jc,kd2,L)%u(1:3)=u1_ps(L,1:3)
                  enddo
                  ps(id2,jc,kd2,L)%u(1:3)=u1_ps(L,1:3) !any particles at the far wall hit it
               endif
            endif
            !----------------------------------

            PS(ID2,JD2,KD2,L)%T=T1_PS(L)
            !G0=DN1_PS(L)*RP3_S(L)
            G0=DN1_PS(L)*c4d3pi*RP_S(L)**3
            ANR=ANR+G0
            AREA=AREA_C(I,J,K,IU)
            G0=G0*PSA%DS*ABS(U1_PS(L,IU))*AREA
            !PSA%FR2=PSA%FR2+G0
            psa%fr2=psa%fr2+wpi_s(l)*DN1_PS(L)*ABS(U1_PS(L,IU))*area
            !H_TOT=H_TOT+G0*PSA%CL*T1_PS(L)
            udf => udf_cls                                           
            call udf_int(udf_clsn,t1_ps(l),sand_cl)          
            h_tot=h_tot+g0*sand_cl*t1_ps(l)

!----
!          ENERGY NEEDED TO HEAT Sand
!----
           !H_NEEDED=H_NEEDED+G0*PSA%CL*(TMLTR-T1_PS(L)) !Lottes 5/19/05
           !qheat_s_need = qheat_s_need + g0*psa%cl*(Tmltr-T1_ps(L)) !Lottes 5/19/05
           call udf_int(udf_clsn,Tmltr,sand_cl2)          
           qheat_s_need = qheat_s_need + g0*((sand_cl+sand_cl2)/2)*(Tmltr-T1_ps(L)) 
         END DO
         PS0(ID2,JD2,KD2)%TH=ANR
         LG(I,J,K)%TH=LG(I,J,K)%TH-PS0(ID2,JD2,KD2)%TH
!----------------------------------------------------------------------
!     Bubble flow
!----------------------------------------------------------------------
340      IF (NBS0.LE.0) GOTO 360
         ANR=ZERO
         G1=(1+LG(I,J,K)%P)*PG0*GB%WT/RU 
         DO L=1,NBS0
            GB4(ID2,JD2,KD2,L)%DN=DN1_GB(L)
            GB4(ID2,JD2,KD2,L)%U(1:3)=U1_GB(L,1:3)
            GB4(ID2,JD2,KD2,L)%T=T1_GB(L)

            IF (GB%FR2.EQ.0.AND.ID2.LT.I_ME/2) THEN
               !CZ            GB4(ID2,NPM2/2,KD2,L)%DN=ONE
            IF (IBCELL(I,J,K).EQ.0) GB4(ID2,NPM2/2,KD2,L)%U(1)=ONE
               !CZ            GB(ID2,NPM2/2,KD2,L)%T=ONE
            ENDIF
            G0=DN1_GB(L)*RG_B(L)**3
            ANR=ANR+G0
            AREA=AREA_C(I,J,K,IU)
            DS_GB=G1/T1_GB(L) 
            DS_GB=1
            G0=G0*DS_GB*ABS(U1_GB(L,IU))*AREA
            GB%FR2=GB%FR2+G0
            H_TOT=H_TOT+G0*GB%CP*T1_GB(L)
         END DO
         GB3(ID2,JD2,KD2)%TH=ANR
         LG(I,J,K)%TH=LG(I,J,K)%TH-GB3(ID2,JD2,KD2)%TH
!----------------------------------------------------------------------
360      IF (LG(I,J,K)%TH.LT.TH_MN) LG(I,J,K)%TH=TH_MN
         IF (ABS(U_0(IU)).LT.SMALL) GOTO 400
         G0=LG(I,J,K)%TH*LG(I,J,K)%DS
         AREA=AREA_C(I,J,K,IU)
         G0=G0*ABS(U_0(IU))*AREA
         FR2_LG=FR2_LG+G0
         !H_TOT=H_TOT+G0*CL_G*(LG(I,J,K)%T-TL0) Lottes 5/6/05
         !H_TOT=H_TOT+G0*LG(I,J,K)%C*LG(I,J,K)%T !use user input c
         call Tlg_to_h(LG(I,J,K)%T,h_liquid)
         H_TOT=H_TOT+G0*h_liquid !use user input c
400   CONTINUE 
!End of charger processing
!----------------------------------------------------------------------
      IF (IU.EQ.1) THEN
         I=I1
         IF (K2.LE.K1) GOTO 420
         DO 410 K=K1+1,K2-1,2
            KP1=K+1
            KM1=K-1
            F1=(Z(K)-Z(KM1))/DZ(K)
            DO 410 J=J1,J2,2
               CALL SBC1(I,J,KP1,I,J,KM1,I,J,K,F1)
410       CONTINUE
420       IF (J2.LE.J1) GOTO 440
          DO 430 J=J1+1,J2-1,2
             JP1=J+1
             JM1=J-1
             F1=(y(J)-y(JM1))/dy(J)
             DO 430 K=K1,K2
                CALL SBC1(I,JP1,K,I,JM1,K,I,J,K,F1)
430      CONTINUE
440      IP1=MIN(I+1,MP)
         IM1=MAX(I-1,2)
         DO 450 J=J1,J2
         DO 450 K=K1,K2
            IBC1=MOD(IBCELL(IP1,J,K),10)
            IF (IBC1.EQ.2.AND.I.NE.IP1) CALL SBC3(I,J,K,IP1,J,K)
            IBC2=MOD(IBCELL(IM1,J,K),10)
            IF (IBC2.EQ.2.AND.I.NE.IM1) CALL SBC3(I,J,K,IM1,J,K)
450      CONTINUE
!----------------------------------------------------------------------
      ELSEIF (IU.EQ.2) THEN
         J=J1
         IF (K2.LE.K1) GOTO 520
         DO 510 K=K1+1,K2-1,2
            KP1=K+1
            KM1=K-1
            F1=(Z(K)-Z(KM1))/DZ(K)
            DO 510 I=I1,I2,2
               CALL SBC1(I,J,KP1,I,J,KM1,I,J,K,F1)
510      CONTINUE
520      IF (I2.LE.I1) GOTO 540
         DO 530 I=I1+1,I2-1,2
            IP1=I+1
            IM1=I-1
            F1=(X(I)-X(IM1))/DX(I)
            DO 530 K=K1,K2
               CALL SBC1(IP1,J,K,IM1,J,K,I,J,K,F1)
530      CONTINUE
540      JP1=MIN(J+1,NP)
         JM1=MAX(J-1,2)
         DO 550 I=I1,I2
         DO 550 K=K1,K2
            IBC1=MOD(IBCELL(I,JP1,K),10)
            IF (IBC1.EQ.2.AND.J.NE.JP1) CALL SBC3(I,J,K,I,JP1,K)
            IBC2=MOD(IBCELL(I,JM1,K),10)
            IF (IBC2.EQ.2.AND.J.NE.JM1) CALL SBC3(I,J,K,I,JM1,K)
550      CONTINUE
!----------------------------------------------------------------------
      ELSEIF (IU.EQ.3) THEN
         K=K1
         IF (J2.LE.J1) GOTO 620
         DO 610 J=J1+1,J2-1,2
            JP1=J+1
            JM1=J-1
            F1=(y(J)-y(JM1))/dy(J)
            DO 610 I=I1,I2,2
               CALL SBC1(I,JP1,K,I,JM1,K,I,J,K,F1)
610      CONTINUE        
620      IF (I2.LE.I1) GOTO 640
         DO 630 I=I1+1,I2-1,2
            IP1=I+1
            IM1=I-1
            F1=(X(I)-X(IM1))/DX(I)
            DO 630 J=J1,J2
               CALL SBC1(IP1,J,K,IM1,J,K,I,J,K,F1)
630      CONTINUE
640      KP1=MIN(K+1,LP)
         KM1=MAX(K-1,2)
         DO 650 I=I1,I2
         DO 650 J=J1,J2
            IBC1=MOD(IBCELL(I,J,KP1),10)
            IF (IBC1.EQ.2.AND.K.NE.KP1) CALL SBC3(I,J,K,I,J,KP1)
            IBC2=MOD(IBCELL(I,J,KM1),10)
            IF (IBC2.EQ.2.AND.K.NE.KM1) CALL SBC3(I,J,K,I,J,KM1)
650      CONTINUE
      ENDIF
      GOTO 200



!----------------------------------------------------------------------
!     Exit Splits
!----------------------------------------------------------------------
800   continue
      h_needed = h_needed + qheat_c_need + qheat_s_need !Energy needed to heat sand and cullet Lottes 5/19/05
      solids_in=psa%fr2+pca%fr2

      G0=0
      IBC0=3
      DO N=1,1000
         READ (nu_sbc,*,ERR=820) I1,J1,K1,I2,J2,K2,IU
         IF (I1.LE.0) EXIT
         READ (nu_sbc,*) TM1(N)
         G0=G0+TM1(N)
         CALL SBC_A
      ENDDO
820   NEX0=N-1
      IF (NEX0.LT.1) THEN
         WRITE (6,*) 'ERROR: No exit'
         call stop_run("no exit")
      ENDIF
      ALLOCATE (PULL(NEX0,3))
      allocate (area_exit(nex0))
      PULL=ZERO
      IF (G0.LE.0) THEN
         DO N=1,NEX0
            PULL(N,1)=ONE/NEX0
         ENDDO
      ELSE
         DO N=1,NEX0
            PULL(N,1)=TM1(N)/G0
         ENDDO
      ENDIF
!---
!     bubbler
!---
      IBC0=8
      DO N=1,1000
         READ (nu_sbc,*,ERR=840) I1,J1,K1,I2,J2,K2,IU
         IF (I1.LE.0) EXIT
         READ (nu_sbc,*) TM1(N),TM2(N)
         CALL SBC_A
      ENDDO
840   NBLR0=N-1
      IF (NBLR0.GT.0) THEN
         ALLOCATE (BLR(NBLR0))
         DO N=1,NBLR0
            BLR(N)%T=TM1(N)
            BLR(N)%FR=TM2(N)
         ENDDO
      ENDIF
!---
!     ebooster
!---
      IBC0=5
      DO N=1,1000
         READ (nu_sbc,*,ERR=860) IA1(1,N),JA1(1,N),KA1(1,N),IA2(1,N),JA2(1,N),KA2(1,N),IU
         IF (IA1(1,N).LE.0) EXIT
         READ (nu_sbc,*,ERR=860) NM0(N),TM1(N),TM2(N)
         READ (nu_sbc,*,ERR=860) IA1(2,N),JA1(2,N),KA1(2,N),IA2(2,N),JA2(2,N),KA2(2,N),IU
         IF (NM0(N).LE.2) CYCLE
         READ (nu_sbc,*,ERR=860) IA1(3,N),JA1(3,N),KA1(3,N),IA2(3,N),JA2(3,N),KA2(3,N),IU
      ENDDO
860   NEB0=N-1
      IF (NEB0.GT.0) THEN
         EB_C=.TRUE.
         EBV=50.0D+0
         EBJ=ZERO
         EBE=ZERO
         EBQ=ZERO
         EBI=ZERO
         EBSG=EBSG0
         eb_heat=zero !Lottes 5/13/05
         ALLOCATE (EB(NEB0))
         DO N=1,NEB0
            eb_heat=eb_heat+tm2(n) !Lottes 5/13/05: must sum eb_heat only over # of boosters
            EB(N)%VLT=TM1(N)
            EB(N)%PWR=TM2(N)
            EB(N)%NTP=NM0(N)
            DO M=1,3
               IF (M.EQ.3.AND.NM0(N).LT.3) THEN
                  M1=2
               ELSE
                  M1=M
               ENDIF
               EB(N)%I1(M)=IA1(M1,N)
               EB(N)%I2(M)=IA2(M1,N)
               EB(N)%J1(M)=JA1(M1,N)
               EB(N)%J2(M)=JA2(M1,N)
               EB(N)%K1(M)=KA1(M1,N)
               EB(N)%K2(M)=KA2(M1,N)
            ENDDO
         ENDDO
      ENDIF
      CLOSE(nu_sbc)

!******************************************************************************
!*************************    END READING SBC FILE    *************************
!******************************************************************************

      !EB_HEAT=SUM(TM2) !Lottes 4/13/05: Bug - whatever is left in TM2 is summed if there are no boosters.
      DEALLOCATE (TM1,TM2,TM3,TM4)
      DEALLOCATE (IA1,IA2,JA1,JA2,KA1,KA2,NM0)

      GFIN=FR2_LG
      GFIN0=FR2_LG

      IF (NPS_C.EQ.ZERO) THEN
         TM_0=(PSA%FR2*TM_S(1))
      ELSEIF (NPS_S.EQ.ZERO) THEN
         TM_0=(PCA%FR2*TM_C(1))
      ELSE
         TM_0=(PCA%FR2*TM_C(1)+PSA%FR2*TM_S(1))
      ENDIF
      TM_0=TM_0/(PCA%FR2+PSA%FR2)
      IF (NPS_C.GE.1) DEALLOCATE (T1_PC,DN1_PC,U1_PC)
      IF (NPS_S.GE.1) DEALLOCATE (T1_PS,DN1_PS,U1_PS)
      IF (NBS0.GE.1) DEALLOCATE (T1_GB,DN1_GB,U1_GB)
!C======================================================================
!Lottes: Initialize  electric boost property values for all cells
!       EBV: electric potential energy (V)
!       EBE: electric potential (V/m)
!       EBI: electric current (A)
!       EBJ: electric current density (A/m**2)
!       EBSG: electric conductivity (1/ohm-m)
!       EBQ: joulian heat (W)
!----------------------------------------------------------------------
!CZTEMP
!CZ      GO TO 702
!CZ   CALL ALCEB
!czcz  TG=TAVE
!cz      TG=T0
!----------------------------------------------------------------------
701   continue
      DO 710 I=4,MPM2
      DO 710 J=4,NPM2
      DO 710 K=4,LPM2
         IBC1=IBCELL(I,J,K)
         IF (IBC1.EQ.55) EBV(I,J,K)=EBV0
         IF (IBC1.EQ.65) EBV(I,J,K)=ZERO
         IF (IBC1.EQ.75) EBV(I,J,K)=ZERO
710   CONTINUE
!CZEB??----------------------------------------------------------------------
      IF (IU.EQ.1) THEN
         I=I1
         IF (K2.GT.K1) THEN
            DO 720 K=K1+1,K2-1,2
            KP1=K+1
            KM1=K-1
            DO 720 J=J1,J2,2
               F1=(Z(K)-Z(KM1))/DZ(K)
               !CALL SBCEB1(I,J,KP1,I,J,KM1,I,J,K,F1)
               EBV(I,J,K)=F1*EBV(I,J,KP1)+F2*EBV(I,J,KM1)
720         CONTINUE
         ENDIF
         IF (J2.GT.J1) THEN
            DO 730 J=J1+1,J2-1,2
            JP1=J+1
            JM1=J-1
            DO 730 K=K1,K2
               F1=(y(J)-y(JM1))/dy(J)
               F2=ONE-F1
               EBV(I,J,K)=F1*EBV(I,JP1,K)+F2*EBV(I,JM1,K)
730         CONTINUE
         ENDIF
         IP1=I+1
         IM1=I-1
         DO 740 J=J1,J2
         DO 740 K=K1,K2
         IBC1=MOD(IBCELL(IP1,J,K),10)
         IBC2=MOD(IBCELL(IM1,J,K),10)
           IF (I.LT.MP.AND.(IBC1.EQ.5.OR.IBC1.EQ.6)) EBV(IP1,J,K)=EBV(I,J,K)
           IF (I.GT.2.AND.(IBC2.EQ.5.OR.IBC2.EQ.6)) EBV(IM1,J,K)=EBV(I,J,K)
740      CONTINUE
      ELSEIF (IU.EQ.2) THEN
         J=J1
         IF (K2.GT.K1) THEN
            DO 750 K=K1+1,K2-1,2
            KP1=K+1
            KM1=K-1
            DO 750 I=I1,I2,2
               F1=(Z(K)-Z(KM1))/DZ(K)
               F2=ONE-F1
               EBV(I,J,K)=F1*EBV(I,J,KP1)+F2*EBV(I,J,KM1)
!CSL               CALL SBC1(I,J,KP1,I,J,KM1,I,J,K,F1)
750         CONTINUE
         ENDIF
         IF (I2.GT.I1) THEN
            DO 760 I=I1+1,I2-1,2
            IP1=I+1
            IM1=I-1
            DO 760 K=K1,K2
               F1=(X(I)-X(IM1))/DX(I)
               F2=ONE-F1
               EBV(I,J,K)=F1*EBV(IP1,J,K)+F2*EBV(IM1,J,K)
!CSL               CALL SBC1(IP1,J,K,IM1,J,K,I,J,K,F1)
760         CONTINUE
         ENDIF
         JP1=J+1
         JM1=J-1
         DO 770 I=I1,I2
         DO 770 K=K1,K2
         IBC1=MOD(IBCELL(I,JP1,K),10)
         IBC2=MOD(IBCELL(I,JM1,K),10)
           IF (J.LT.NP.AND.(IBC1.EQ.5.OR.IBC1.EQ.6)) EBV(I,JP1,K)=EBV(I,J,K)
           IF (J.GT.2.AND.(IBC2.EQ.5.OR.IBC2.EQ.6)) EBV(I,JM1,K)=EBV(I,J,K)
770      CONTINUE
      ELSEIF (IU.EQ.3) THEN
         K=K1
         IF (J2.GT.J1) THEN
            DO 780 J=J1+1,J2-1,2
            JP1=J+1
            JM1=J-1
            DO 780 I=I1,I2,2
               F1=(y(J)-y(JM1))/dy(J)
               F2=ONE-F1
               EBV(I,J,K)=F1*EBV(I,JP1,K)+F2*EBV(I,JM1,K)
780         CONTINUE        
         ENDIF
         IF (I2.GT.I1) THEN
            DO 790 I=I1+1,I2-1,2
            IP1=I+1
            IM1=I-1
            DO 790 J=J1,J2
               F1=(X(I)-X(IM1))/DX(I)
               F2=ONE-F1
               EBV(I,J,K)=F1*EBV(IP1,J,K)+F2*EBV(IM1,J,K)
790         CONTINUE
         ENDIF
         KP1=K+1
         KM1=K-1
         DO 791 I=I1,I2
         DO 791 J=J1,J2
         IBC1=MOD(IBCELL(I,J,KP1),10)
         IBC2=MOD(IBCELL(I,J,KM1),10)
           IF (K.LT.LP.AND.(IBC1.EQ.5.OR.IBC2.EQ.6)) EBV(I,J,KP1)=EBV(I,J,K)
           IF (K.GT.2.AND.(IBC2.EQ.5.OR.IBC2.EQ.6)) EBV(I,J,KM1)=EBV(I,J,K)
791      CONTINUE
      ENDIF
!CZ   GOTO 701
702   CONTINUE
!Lottes 6/2/05 - Calculate exit areas
      area_exit=zero
      !Search for exit x-normal faces
      do i=3,mp,2
      do j=4,np-2,2
      do k=4,lp-2,2
         if (mod(ibcell(i,j,k),10)==3) then
           if (ibcell(i,j,k)==103) then
               area_exit(ibcell(i+1,j,k)/10)= area_exit(ibcell(i+1,j,k)/10) + dy(j)*dz(k)
           else
               area_exit(ibcell(i-1,j,k)/10)= area_exit(ibcell(i-1,j,k)/10) + dy(j)*dz(k)
           endif
         endif
      enddo;enddo;enddo

      !Search for exit y-normal faces
      do i=4,mp-2,2
      do j=3,np,2
      do k=4,lp-2,2
         if (mod(ibcell(i,j,k),10)==3) then
           if (ibcell(i,j,k)==203) then
               area_exit(ibcell(i,j+1,k)/10)= area_exit(ibcell(i,j+1,k)/10) + dx(i)*dz(k)
           else
               area_exit(ibcell(i,j-1,k)/10)= area_exit(ibcell(i,j-1,k)/10) + dx(i)*dz(k)
           endif
         endif
      enddo;enddo;enddo

      !Search for exit z-normal faces
      do i=4,mp-2,2
      do j=4,np-2,2
      do k=3,lp,2
         if (mod(ibcell(i,j,k),10)==3) then
           if (ibcell(i,j,k)==303) then
               area_exit(ibcell(i,j,k+1)/10)= area_exit(ibcell(i,j,k+1)/10) + dx(i)*dy(j)
           else
               area_exit(ibcell(i,j,k-1)/10)= area_exit(ibcell(i,j,k-1)/10) + dx(i)*dy(j)
           endif
         endif
      enddo;enddo;enddo
      area_exit_tot = zero
      do n=1,nex0
         area_exit_tot = area_exit_tot + area_exit(n)
      enddo
!CZEB-------------------------------
      RETURN
900   WRITE (6,"(' ** ERROR: reference cell is not an inlet')")
      call stop_run("Error: reference cell is not an inlet.")

!======================================================================
!======================================================================

      CONTAINS
      SUBROUTINE SBC_A
      N1=N*10+IBC0
      DO 920 I=I1,I2
      DO 920 J=J1,J2
      DO 920 K=K1,K2
         IF (IBCELL(I,J,K).NE.IBC0) CYCLE
         IBCELL(I,J,K)=N1
920   CONTINUE
      END SUBROUTINE SBC_A
      SUBROUTINE SBC_B
      N1=N*10+IBC0
      IF (ABS(IU).EQ.1) THEN
         IF (IU.EQ.1) THEN
            I20=2
            I0=-2
         ELSE
            I20=MP
            I0=2
         ENDIF
         DO J=J1,J2
         DO K=K1,K2
            DO I=I1,I20,I0
               IF (IBCELL(I,J,K).NE.IBC0) CYCLE
               IBCELL(I,J,K)=N1
               EXIT
            ENDDO
         ENDDO
         ENDDO
      ELSEIF (ABS(IU).EQ.2) THEN
         IF (IU.EQ.2) THEN
            J20=2
            J0=-2
         ELSE
            J20=NP
            J0=2
         ENDIF
         DO I=I1,I2
         DO K=K1,K2
            DO J=J1,J20,J0
               IF (IBCELL(I,J,K).NE.IBC0) CYCLE
               IBCELL(I,J,K)=N1
               EXIT
            ENDDO
         ENDDO
         ENDDO
      ELSEIF (ABS(IU).EQ.3) THEN
         IF (IU.EQ.3) THEN
            K20=2
            K0=-2
         ELSE
            K20=LP
            K0=2
         ENDIF
         DO I=I1,I2
         DO J=J1,J2
            DO K=K1,K20,K0
               IF (IBCELL(I,J,K).NE.IBC0) CYCLE
               IBCELL(I,J,K)=N1
               EXIT
            ENDDO
         ENDDO
         ENDDO
      ENDIF
      END SUBROUTINE SBC_B
      END
!======================================================================
!     SBC1 interpolate between nodes
!======================================================================
      SUBROUTINE SBC1(I1,J1,K1,I2,J2,K2,I,J,K,F1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      F2=ONE-F1
      LG(I,J,K)%P=F1*LG(I1,J1,K1)%P+F2*LG(I2,J2,K2)%P
      LG(I,J,K)%T=F1*LG(I1,J1,K1)%T+F2*LG(I2,J2,K2)%T
      !CALL ENTH(I,J,K,1) Lottes 5/11/05 h should be calculated after c below
      DO L=1,3
         LG(I,J,K)%U(L)=F1*LG(I1,J1,K1)%U(L)+F2*LG(I2,J2,K2)%U(L)
      END DO
      DO L=1,LEND+1
         LG(I,J,K)%F(L)=F1*LG(I1,J1,K1)%F(L)+F2*LG(I2,J2,K2)%F(L)
      END DO
      CALL DSLG(I,J,K)
      LG(I,J,K)%TH=F1*LG(I1,J1,K1)%TH+F2*LG(I2,J2,K2)%TH
      LG(I,J,K)%C=F1*LG(I1,J1,K1)%C+F2*LG(I2,J2,K2)%C
      LG(I,J,K)%K=F1*LG(I1,J1,K1)%K+F2*LG(I2,J2,K2)%K
      call Tlg_to_h(Lg(i,j,k)%T,Lg(i,j,k)%h) !Lottes 5/11/05 set h consistent with T and c.
      !Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
      !CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
      RETURN
      END
!======================================================================
!     SBC3 assign other inlet values
!======================================================================
      SUBROUTINE SBC3(I1,J1,K1,I,J,K)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LG(I,J,K)%P=LG(I1,J1,K1)%P
      LG(I,J,K)%T=LG(I1,J1,K1)%T
      !CALL ENTH(I,J,K,1) !Lottes 5/11/05 h should be calculated after c below
 
      DO L=1,3
         LG(I,J,K)%U(L)=LG(I1,J1,K1)%U(L)
      END DO
      DO L=1,LEND+1
         LG(I,J,K)%F(L)=LG(I1,J1,K1)%F(L)
      END DO
      LG(I,J,K)%DS=LG(I1,J1,K1)%DS
      LG(I,J,K)%TH=LG(I1,J1,K1)%TH
      LG(I,J,K)%K=LG(I1,J1,K1)%K
      LG(I,J,K)%C=LG(I1,J1,K1)%C
      Tlg_to_h(Lg(i,j,k)%T,Lg(i,j,k)%h) !Lottes 5/11/05 set h consistent with T and c.
      !Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
      !CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
      RETURN
      END
!CZEB======================================================================
!     SBCEB1 interpolate between nodes
!======================================================================
      SUBROUTINE SBCEB1(I1,J1,K1,I2,J2,K2,IU)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (IU.NE.1) GOTO 200
      I=I1
      DO 120 J=J1-1,J2+1,2
      DO 120 K=K1,K2,2
         IF (IBCELL(I,J,K).NE.5) GOTO 120
         F1=(y(J)-y(J-1))/dy(J)
         F2=ONE-F1
         EBV(I,J,K)=F1*EBV(I1,J1,K1)+F2*EBV(I2,J2,K2)
120   CONTINUE
      F2=ONE-F1
      EBE(I,J,K)=F1*EBE(I1,J1,K1)+F2*EBE(I2,J2,K2)
      EBJ(I,J,K)=F1*EBJ(I1,J1,K1)+F2*EBJ(I2,J2,K2)
      EBI(I,J,K)=F1*EBI(I1,J1,K1)+F2*EBI(I2,J2,K2)
      EBSG(I,J,K)=F1*EBSG(I1,J1,K1)+F2*EBSG(I2,J2,K2)
      EBQ(I,J,K)=F1*EBQ(I1,J1,K1)+F2*EBQ(I2,J2,K2)
200   IF (IU.NE.2) GOTO 300
300   IF (IU.NE.3) GOTO 900
900   RETURN
      END


!CZEB======================================================================
      SUBROUTINE EBPROP
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 200 I=3,MP-1,2
      IM1=I-1
      IP1=I+1
      DO 200 J=2,NP,2
      DO 200 K=2,LP,2
         IF (IBCELL(I,J,K).EQ.1.OR.IBCELL(IM1,J,K).EQ.1.OR.IBCELL(IP1,J,K).EQ.1) GOTO 200
         EBE(I,J,K)=(EBV(IM1,J,K)-EBV(IP1,J,K))/DX(I)
         EBJ(I,J,K)=EBE(I,J,K)*EBSG(I,J,K)
200   CONTINUE
      IF (NP.LT.6) GOTO 310
      DO 300 J=3,NP-1,2
      JM1=J-1
      JP1=J+1
      DO 300 I=2,MP,2
      DO 300 K=2,LP,2
         IF (IBCELL(I,J,K).EQ.1.OR.IBCELL(I,JM1,K).EQ.1.OR.IBCELL(I,JP1,K).EQ.1) GOTO 300
         EBE(I,J,K)=(EBV(I,JM1,K)-EBV(I,JP1,K))/dy(J)
         EBJ(I,J,K)=EBE(I,J,K)*EBSG(I,J,K)
300   CONTINUE
310   IF (LP.LT.6) GOTO 900
      DO 400 K=3,LP-1,2
      KM1=K-1
      KP1=K+1
      DO 400 I=2,MP,2
      DO 400 J=2,NP,2
         IF (IBCELL(I,J,K).EQ.1.OR.IBCELL(I,J,KM1).EQ.1.OR.IBCELL(I,J,KP1).EQ.1) GOTO 400
         EBE(I,J,K)=(EBV(I,J,KM1)-EBV(I,J,KP1))/DZ(K)
         EBJ(I,J,K)=EBE(I,J,K)*EBSG(I,J,K)
400   CONTINUE
900   RETURN
      END



