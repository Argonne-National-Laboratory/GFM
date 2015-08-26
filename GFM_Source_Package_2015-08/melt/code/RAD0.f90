! RAD0.F90
!======================================================================
      SUBROUTINE RAD0(MRAD)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
!     RI: Refraction index
!     NWL: Number of spectral bands
!     WL:  Wavelength (m)
!----------------------------------------------------------------------
1     FORMAT (A32)
      IF (MRAD.EQ.1) GOTO 200
      TINF=3.08D+2
      QEL_MN=10.0D+0
      TG_MN=5.0D+2
      SIGN=SIG*RI*RI
      IF (.NOT.ALLOCATED(DEB)) ALLOCATE (DEB(NWL),DEB0(NWL))
      DEB0(1:NWL)=HCK/RI/WL(1:NWL)
      RETURN


!----------------------------------------------------------------------
!     TEMP
!----------------------------------------------------------------------
200   IF (.NOT.ALLOCATED(TG)) ALLOCATE (TG(MZ,NZ,LZ))
      IF (.NOT.ALLOCATED(AKL)) THEN
         ALLOCATE (AKL(MZ,NZ,LZ,NWL))
         AKL=0
      ENDIF
      DO 240 I=2,MP,2
      ID2=I/2
      DO 240 J=2,NP,2
      JD2=J/2
      DO 240 K=2,LP,2
         KD2=K/2
         TG(ID2,JD2,KD2)=LG(I,J,K)%T
         IBC0=IBCELL(I,J,K)
         IF (IBC0.GT.0) THEN
            IBC0=MOD(IBC0,10)
            IF (IBC0.EQ.4) THEN
               AKL(ID2,JD2,KD2,1:NWL)=1
            ELSEIF (IBC0.EQ.1) THEN
               IW1=IBCELL(I,J,K)/10
               IF (IW1.GT.0) THEN
                  AKL(ID2,JD2,KD2,1:NWL)=W1_E(IW1)
               ELSE
                  AKL(ID2,JD2,KD2,1:NWL)=W_E
               ENDIF
            ENDIF
            CYCLE
         ENDIF
         VOL=VOL_C(I,J,K)
         SA=((DX(I)*DY(J))+(DY(J)*DZ(K))+(DX(I)*DZ(K)))*TWO
         IF (SA.LE.SMALL) CYCLE
         OPL=6.0D+0*VOL/SA
         IF (OPL.LE.1.0D-10) CYCLE
         DO L=1,NWL
            AKL(ID2,JD2,KD2,L)=EMF(AKL0(L),OPL)
         ENDDO
240   CONTINUE
      RETURN
      END
