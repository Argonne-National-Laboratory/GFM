! GSLV3M.F90
!======================================================================
!     GSLV3M solves governing transport equations for sub-species 
!     10/97
!======================================================================
      SUBROUTINE GSLV3M(L_S,L_E)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      KD=3
      MI1=4
      MI3=MPM2
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2
      DO 600 L=1,NMSP
         NELM=L       
         DO 530 I=2,MP,2
         DO 530 J=2,NP,2
         DO 530 K=2,LP,2
530      FZ(I,J,K)=GFM(I,J,K,L) 
         CALL SOURCEM(MI1,MI3,NJ1,NJ3,LK1,LK3)
         !cz??         SGM=SSIGMA
         SGM=one

         IF (L.EQ.L_S) THEN
            CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,0)
         ELSE
            CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,1)
         ENDIF
555      DO 560 I=MI1,MI3,2
         DO 560 J=NJ1,NJ3,2
         DO 560 K=LK1,LK3,2
            IF (IBCELL(I,J,K).GE.1) GOTO 560
            !cz         IF (L.EQ.1) BS(I,J,K)=BS(I,J,K)*0.6d+0
            !cz         IF (L.EQ.2) BS(I,J,K)=BS(I,J,K)*0.4d+0
            VOL=VOL_C(I,J,K)
            BS(I,J,K)=BS(I,J,K)*VOL
            !CZ            AP(I,J,K)=AP(I,J,K)+(EVP(I/2,J/2,K/2)-
            !CZ     &         CON(I/2,J/2,K/2)-SFP(I,J,K))*VOL
            IF (.NOT.STEADY) THEN
               !CSL            BS(I,J,K)=(BS(I,J,K)+APO(I,J,K)*GFMO(I,J,K,L))*VOL
               AP(I,J,K)=AP(I,J,K)+APO(I,J,K)*VOL
            ENDIF
            !cz            AP(I,J,K)=AP(I,J,K)/RFM(L)
            !cz            BS(I,J,K)=BS(I,J,K)+RFMC(L)*FZ(I,J,K)*AP(I,J,K)
            AP(I,J,K)=AP(I,J,K)/0.9D+0
            BS(I,J,K)=BS(I,J,K)+0.1D+0*FZ(I,J,K)*AP(I,J,K)
560      CONTINUE

         CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
         DO 590 I=MI1,MI3,2
         DO 590 J=NJ1,NJ3,2
         DO 590 K=LK1,LK3,2
            IF (IBCELL(I,J,K).GE.1) GOTO 590
            FZ(I,J,K)=MAX(FZ(I,J,K),ZERO)
            FZ(I,J,K)=MIN(FZ(I,J,K),ONE)
            GFM(I,J,K,L)=0.1D+0*GFM(I,J,K,L)+0.9D+0*FZ(I,J,K)
590      CONTINUE
600   CONTINUE
      DO 650 I=MI1,MI3,2
      DO 650 J=NJ1,NJ3,2
      DO 650 K=LK1,LK3,2
         IF (IBCELL(I,J,K).GE.1) GOTO 650
         G0=0
         DO L=1,NMSP
            G0=G0+GFM(I,J,K,L)
         ENDDO
         IF (G0.LE.ONE) GOTO 650
         DO L=1,NMSP
            GFM(I,J,K,L)=GFM(I,J,K,L)/G0
         ENDDO
650   CONTINUE
900   RETURN
      END
