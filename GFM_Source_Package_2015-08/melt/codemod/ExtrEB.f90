! EXTREB.F90
!     Extrapolation of calculated variables 
!       10/00
!======================================================================
      SUBROUTINE EXTREB
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
!     M_S=1: liquid variables 
!         2: batch variables 
!         3: bubble variables 
!         4: subspecies concentrations 
!     IBCELL: node
!         1: wall 
!         2: inlet 
!         3: exit
!         4: free surface
!CZEB      5: plus electrode
!----------------------------------------------------------------------
      DO 120 I=2,MP,2
      DO 120 J=2,NP,2
      DO 120 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.NE.2.AND.IBC0.NE.3) GOTO 120
         CALL EXTEB0(I,J,K,I1,J1,K1,I2,J2,K2,IX)
         EBV(I,J,K)=EBV(I1,J1,K1)
120   CONTINUE
!----------------------------------------------------------------------
      DO 160 I=2,MP,2
      DO 160 J=2,NP,2
      DO 160 K=2,LP,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.EQ.5) THEN
            !CZ          IBC2=IBCELL(I,J,K)
            !CZ          IF (N3VBC.EQ.75.AND.IBC2.NE.75.OR.
            !CZ     &       N3VBC.EQ.65.AND.IBC2.NE.65.OR.
            !CZ     &       N3VBC.EQ.55.AND.IBC2.NE.55) THEN
           EBV(I-1,J,K)=EBV(I,J,K)
           EBV(I+1,J,K)=EBV(I,J,K)
           EBV(I,J-1,K)=EBV(I,J,K)
           EBV(I,J+1,K)=EBV(I,J,K)
           EBV(I,J,K-1)=EBV(I,J,K)
           EBV(I,J,K+1)=EBV(I,J,K)  
           !CZ        ENDIF
         ENDIF
         IF (IBC0.NE.1.AND.IBC0.NE.4) GOTO 160 !Do below for walls
         CALL EXTEB1(I,J,K,I1,J1,K1,I2,J2,K2,IX)
         IF (IX.GT.3) GOTO 160
         EBV(I,J,K)=EBV(I1,J1,K1)
160   CONTINUE
      DO 170 I=2,MP,2
      DO 170 K=2,LP,2
         EBV(I,2,K)=EBV(I,4,K)
         EBV(I,NP,K)=EBV(I,NP-2,K)
170   CONTINUE
      DO 180 I=2,MP,2
      DO 180 J=2,NP,2
         EBV(I,J,2)=EBV(I,J,4)
         EBV(I,J,LP)=EBV(I,J,LP-2)
180   CONTINUE
      DO 190 J=2,NP,2
      DO 190 K=2,LP,2
         EBV(2,J,K)=EBV(4,J,K)
         EBV(MP,J,K)=EBV(MP-2,J,K)
190   CONTINUE
      RETURN
      END

!======================================================================
!     Extrapolation indices 
!======================================================================
      SUBROUTINE EXTEB0(I,J,K,I1,J1,K1,I2,J2,K2,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3)

      DO IX=1,3
      DO II=1,3
         NX(II)=0
      ENDDO
      !     NX=0
      NX(IX)=1
      DO M=-2,2,4
         I1=I+M*NX(1)
         I1=MIN(MP,I1)
         I1=MAX(2,I1)
         J1=J+M*NX(2)
         J1=MIN(NP,J1)
         J1=MAX(2,J1)
         K1=K+M*NX(3)
         K1=MIN(LP,K1)
         K1=MAX(2,K1)
         IF (IBCELL(I1,J1,K1).LE.0) THEN
            RETURN
         ENDIF
      ENDDO
      ENDDO
      RETURN
      END

!======================================================================
!     Extrapolation indices 
!======================================================================
      SUBROUTINE EXTEB1(I,J,K,I1,J1,K1,I2,J2,K2,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3)

      DO IX=1,3
         DO II=1,3
          NX(II)=0
         ENDDO
         !      NX=0
      NX(IX)=1
      DO M=-2,2,4
         I1=I+M*NX(1)
         I1=MIN(MP,I1)
         I1=MAX(2,I1)
         J1=J+M*NX(2)
         J1=MIN(NP,J1)
         J1=MAX(2,J1)
         K1=K+M*NX(3)
         K1=MIN(LP,K1)
         K1=MAX(2,K1)
         IBC1=MOD(IBCELL(I1,J1,K1),10)
         IF (IBC1.NE.1.AND.IBC1.NE.4) THEN
            RETURN
         ENDIF
      ENDDO
      ENDDO
      RETURN
      END
