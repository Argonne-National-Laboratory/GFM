! PFLOW2.F90
!======================================================================
!     To calculate batch and glass mass flow rates
!        at various axial locations
!        rev: 6/01
!======================================================================
      SUBROUTINE PFLOW2(NEL_P)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AREA(3),U_0(3)
1     FORMAT(' PC',I5,E12.4,3I4,E12.4,F8.4'/'F8.4)
2     FORMAT(' PS',I5,E12.4,3I4,E12.4,F8.4'/'F8.4)
3     FORMAT(T6,I2,5E11.4,F7.1)
4     FORMAT ('      flow rates: ',5G11.4)
!----------------------------------------------------------------------
!     Batch mass entering (IN) and leaving (OUT) the system
!        FR2:  particle mass flow rate in (kg/s)
!        BGOUT: batch/glass mass flow rate out (kg/s) not used, Lottes 5/31/05
!        FR2_LG: liquid glass mass flow rate in (kg/s)
!        GLSOUT: liquid glass mass flow rate out (kg/s)  
!        EX_P:   particlemass flow rate out (kg/s)  
!----------------------------------------------------------------------
      IF (P1A%FR2.LT.SMALL) RETURN
!----------------------------------------------------------------------
      P1A%MR=ZERO
      EX_P=ZERO
      AFE=ZERO
      PFRX_1(1)=P1A%FR2
      PMRX_1=ZERO
      DO 200 I=MP_B,MP_E1,2
         ID2=I/2
         DFF=ZERO
         DFFH=ZERO
         PFRX_1(ID2)=ZERO
         DO 180 J=NJY1,NJY2,2             
         JD2=J/2
         DO 180 K=LKZ1,LKZ2,2
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.GE.1) GOTO 180
            KD2=K/2
            P10(ID2,JD2,KD2)%MR=0
            DO L=1,NPS_1
               P10(ID2,JD2,KD2)%MR=P10(ID2,JD2,KD2)%MR+P1(ID2,JD2,KD2,L)%MR
            ENDDO               
            AREA(1)=AREA_C(I,J,K,1)
            AREA(2)=AREA_C(I,J,K,2)
            AREA(3)=AREA_C(I,J,K,3)
            DO 130 IU=1,3
            IF (IU.EQ.2.AND.NP.LE.6) GOTO 130
            IF (IU.EQ.3.AND.LP.LE.6) GOTO 130
            DO M=-1,1,2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               IBC1=MOD(IBCELL(I1,J1,K1),10)
               IF (IBC1.EQ.3) THEN
                  FE=LG(I1,J1,K1)%U(IU)*LG(I1,J1,K1)%TH*LG(I1,J1,K1)%DS*AREA(IU)
                  AFE=AFE+FE*M
               ENDIF
            ENDDO
130         CONTINUE
            DFFR=ZERO
            BS(ID2,JD2,KD2)=ZERO
            DO 160 L=1,NPS_1
               DN0=P1(ID2,JD2,KD2,L)%DN
               DO 150 IU=1,3
               !IF (IU.EQ.2.AND.NP.LE.6) GOTO 150  !old 2D mode
               !IF (IU.EQ.3.AND.LP.LE.6) GOTO 150
               DU0=P1(ID2,JD2,KD2,L)%U(IU)
               DO M=-1,1,2
                  CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
                  IBC1=MOD(IBCELL(I1,J1,K1),10)
                  DU1=P1(ID21,JD21,KD21,L)%U(IU)
                  DN1=P1(ID21,JD21,KD21,L)%DN
                  IF (IBC1.EQ.1) THEN
                     !DIFF=ZERO
                     DUK=ZERO
                  ELSEIF (IBC1.EQ.2) THEN
                     !DIFF=ZERO
                     DUK=DU1
                  ELSEIF (IBC1.EQ.3) THEN
                     !DIFF=ZERO
                     CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                     DUK=F1*DU1+F2*DU0
                  ELSE
                     IF (DN1.LT.SMALL) DU1=DU0
                     IF (DN0.LT.SMALL) DU0=DU1
                     !DIFF=DFF0(I1,J1,K1,L,IU)
                     CALL INTP1M(F1,F2,I1,J1,K1,IU,M)
                     DUK=F1*DU1+F2*DU0
                  ENDIF
                  FL=DUK*AREA(IU)
                  IF (IBC1.EQ.1) FL=ZERO
                  IF (M*FL.GE.-SMALL) THEN
                     DNK=DN0
                  ELSE
                     DNK=DN1
                  ENDIF
                  DDNK=DN1-DN0
                  !FL2=LG(I1,J1,K1)%U(IU)*AREA(IU)
                  !FL1=DA(DIFF,FL2)*DDNK-FL*DNK*M  !da currently returns diff
                  !Lottes 1/13/06 There is no turbulent diffusion of particles in the melt
                  !so there should be no DA term
                  !FL1=DA(DIFF,FL2)*DDNK-FL*DNK*M  !da currently returns diff
                  FL1=-FL*DNK*M  
                  IF (IBC1.EQ.3) THEN
                     !EX_P=EX_P-FL1*RP3_1(L)
                     EX_P=EX_P-FL1*wpi_1(L) !wpi=particle mass: make it a mass flow rate
                  ENDIF
                  BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+FL1*RP3_1(L)
                  IF (IU.EQ.1.AND.M.EQ.1) THEN
                     !DFFR=DFFR-FL1*RP3_1(L)
                     DFFR=DFFR-FL1*wpi_1(L) !make it a mass flow rate
                     !DFFH=DFFH-FL1*RP3_1(L)*P1A%CL*P1(ID2,JD2,KD2,L)%T
                     if (nel_p==1) then
                        udf => udf_clc                                               
                        call udf_int(udf_clcn,p1(id2,jd2,kd2,kL)%T,particle_cl)          
                     else
                        udf => udf_cls                                               
                        call udf_int(udf_clsn,p1(id2,jd2,kd2,kL)%T,particle_cl)          
                     endif
                     DFFH=DFFH-FL1*RP3_1(L)*particle_cl*P1(ID2,JD2,KD2,L)%T
                  ENDIF
               ENDDO
150            CONTINUE
160         CONTINUE
            DFF=DFF+DFFR
            VOL=VOL_C(I,J,K)
            DFF1=P10(ID2,JD2,KD2)%MR*VOL
            PMRX_1(ID2)=PMRX_1(ID2)+P10(ID2,JD2,KD2)%MR*VOL
180      CONTINUE
         !PMRX_1(ID2)=PMRX_1(ID2)
         PFRX_1(ID2)=DFF
         FLXHP_1(I+1)=DFFH
         IF (NEL_P.EQ.1) THEN
            FLX(I+1,8)=PFRX_1(ID2)
         ELSE
            FLX(I+1,8)=FLX(I+1,8)+PFRX_1(ID2)
         ENDIF
         P1A%MR=P1A%MR+PMRX_1(ID2)
200   CONTINUE
      GLSOUT =AFE/FACTOR
      IF (MP_E1.LT.MPM2) THEN
         GLSOUT=GLSOUT+FLX(MP_E1-1,1)
         EX_P=EX_P+PFRX_1(MP_E1/2)
      ENDIF
      !BGOUT=EX_P+GLSOUT !not used, Lottes 5/31/05

      !Gfex is computed in extrv and should not need to be set here unless we
      !need to add in particle mass rate out the exits. 2/20/06
      !GFEX=GLSOUT !Lottes 4/15/05: Why is gfex set here?

!-----CONDENSED PHASE MASS BALANCE
      !check this over, does not look consistent, Lottes 12-19-05
      AVEB=ZERO
      SMX=ZERO   
      NO=0
      IMX=MP_B/2
      JMX=NJY1/2
      KMX=LKZ1/2
      DO 300 I=MP_B,MP_E1,2
      ID2=I/2
      DO 300 J=NJY1,NJY2,2
      JD2=J/2
      DO 300 K=LKZ1,LKZ2,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.GE.1) GOTO 300 
         KD2=K/2
         IF (P10(ID2,JD2,KD2)%TH.LE.SMALL) GOTO 300 
         NO=NO+1
         VOL=VOL_C(I,J,K)
         BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)-P10(ID2,JD2,KD2)%MR/p1a%ds*VOL
         !&      P10(ID2,JD2,KD2)%MR/RFG_1*VOL
         BTMP=ABS(BS(ID2,JD2,KD2))
         AVEB=AVEB+BTMP
         IF (BTMP.GT.SMX) THEN
            SMX=BTMP
            IMX=ID2
            JMX=JD2
            KMX=KD2
         ENDIF
300   CONTINUE
      IF (NO.GT.0) AVEB=AVEB/NO
      I=IMX*2
      J=JMX*2
      K=KMX*2
      IF (NEL_P.EQ.2) THEN
         WRITE(NU,2) N_PI,SMX,I,J,K,AVEB,P1A%MR,P1A%FR2
      ELSE
         WRITE(NU,1) N_PI,SMX,I,J,K,AVEB,P1A%MR,P1A%FR2
      ENDIF
      DO L=1,NPS_1
         !cz         U_0(1:3)=P1(IMX,JMX,KMX,L)%U(1:3)
         G1=P1(IMX,JMX,KMX,L)%U(1)
         G2=P1(IMX,JMX,KMX,L)%U(2)
         G3=P1(IMX,JMX,KMX,L)%U(3)
         G4=P10(IMX,JMX,KMX)%TH
         G5=P1(IMX,JMX,KMX,L)%DN
         G6=P1(IMX,JMX,KMX,L)%T
         WRITE(NU,3) L,G1,G2,G3,G4,G5,G6
      ENDDO
      RETURN
      END
