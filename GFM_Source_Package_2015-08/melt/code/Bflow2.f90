! BFLOW2.F90
!======================================================================
!     To calculate bubble mass flow at axial location
!        rev: 4/02
!======================================================================
      SUBROUTINE BFLOW2(NEL_P)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AREA(3),U_0(3)
1     FORMAT(' GB',I5,E12.4,3I4,E12.4)
3     FORMAT(T6,I2,5E11.4,F7.1)

      GBX(1)%FR=GB%FR2
      DO I=MP_B,MP_E1,2
         ID2=I/2
         DFF=ZERO
         DFFH=ZERO
         GBX(ID2)%FR=ZERO
         DO 180 J=NJY1,NJY2,2             
         JD2=J/2
         DO 180 K=LKZ1,LKZ2,2
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.GE.1) CYCLE
            KD2=K/2
            AREA(1)=AREA_C(I,J,K,1)
            AREA(2)=AREA_C(I,J,K,2)
            AREA(3)=AREA_C(I,J,K,3)
            DFFR=ZERO
            BS(ID2,JD2,KD2)=ZERO
            DO L=1,NBS0
               DN0=GB4(ID2,JD2,KD2,L)%DN*WGI_B(L)
               IF (DN0.LT.SMALL) CYCLE
               DO IU=1,3
                  IF ((IU.EQ.2.AND.NP.LE.6).OR.(IU.EQ.3.AND.LP.LE.6)) CYCLE
                  DU0=GB4(ID2,JD2,KD2,L)%U(IU)
                  DO M=-1,1,2
                     CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
                     DN1=GB4(ID21,JD21,KD21,L)%DN*WGI_B(L)
                     DU1=GB4(ID21,JD21,KD21,L)%U(IU)
                     IBC1=MOD(IBCELL(I1,J1,K1),10)
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
                        !DIFF=DFF0(I1,J1,K1,L,IU) !dff0 returns zero, so diff is zero
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
                     !FL1=DA(DIFF,FL2)*DDNK-FL*DNK*M !da returns diff and diff is zero
                     FL1=-FL*DNK*M 
                     FL1R3=FL1
                     IF (IBC1.EQ.3) THEN
                        EX_P=EX_P-FL1R3
                     ENDIF
                     BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+FL1R3
                     IF (IU.EQ.1.AND.M.EQ.1) THEN
                        DFFR=DFFR-FL1R3
                        DFFH=DFFH-FL1R3*GB%CP*GB4(ID2,JD2,KD2,L)%T
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
            DFF=DFF+DFFR
180      CONTINUE
         GBX(ID2)%FR=DFF
      ENDDO
!     gas bubble mass balance
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
         IF (GB3(ID2,JD2,KD2)%TH.LE.SMALL) GOTO 300 
         NO=NO+1
         VOL=VOL_C(I,J,K)
         AREA(3)=AREA_C(I,J,K,3)
         G0=0
         DO L=1,NBS0
            G1=GB4(ID2,JD2,KD2,L)%GR
            G2=GB%B2*AREA(3)*WGI_B(L)*GB4(ID2,JD2,KD2,L)%DN
            IF (G1.GT.SMALL) G2=G2*100
            G0=G0+(G1-G2)*VOL
         ENDDO
         BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+G0
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
      WRITE(NU,1) N_PI,SMX,I,J,K,AVEB
      DO L=1,NBS0
         G1=GB4(IMX,JMX,KMX,L)%U(1)
         G2=GB4(IMX,JMX,KMX,L)%U(2)
         G3=GB4(IMX,JMX,KMX,L)%U(3)
         G4=GB3(IMX,JMX,KMX)%TH
         G5=GB4(IMX,JMX,KMX,L)%DN
         G6=GB4(IMX,JMX,KMX,L)%T
         WRITE(NU,3) L,G1,G2,G3,G4,G5,G6
      ENDDO
      RETURN
      END
