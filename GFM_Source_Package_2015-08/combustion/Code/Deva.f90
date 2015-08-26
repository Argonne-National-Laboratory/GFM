! DEVA.FOR
!======================================================================
!     Calculates the evaporation rate of the spray 
!     revision: 9/98
!======================================================================
SUBROUTINE DEVA
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION FDNU(NDP0)
!----------------------------------------------------------------------
GC3=GCN*THIRD
CNU1=0.276D+0*SQRT(REYD)*SCN**THIRD
CEV1=ELH*TRDGC
DO I=4,MPM2,2; ID2=I/2
DO J=NJY1,NJY2,2; JD2=J/2
JP2=MIN(J+2,NJY2)
JM2=MAX(J-2,NJY1)
   DO K=LKZ1,LKZ2,2
   KD2=K/2
   KP2=MIN(K+2,LKZ2)
   KM2=MAX(K-2,LKZ1)
   IF (IBCELL(I,J,K).GE.1.OR.                           &
       IBCELL(I+2,J,K).EQ.3.OR.IBCELL(I-2,J,K).EQ.3.OR. &
       IBCELL(I,JP2,K).EQ.3.OR.IBCELL(I,JM2,K).EQ.3.OR. &
       IBCELL(I,J,KP2).EQ.3.OR.IBCELL(I,J,KM2).EQ.3) THEN
      EVP(ID2,JD2,KD2)=ZERO
         cycle
   END IF
   GMU=(T(I,J,K)/ONE)**AMU
   GDF=(T(I,J,K)/ONE)**AMD
   DBMU=DNST(I,J,K)/GMU
   SCIJ=(GMU/GDF/DNST(I,J,K))**THIRD
   EVWX=ZERO
   EVWS=ZERO
   EVWN=ZERO                           
   EVWZ=ZERO
!----------------------------------------------------------------------
!----Model: droplets vaporize comnpletely when
!           hitting a sufficiently hot wall (Twall > TTWE)
!----------------------------------------------------------------------
   DO L=1,NDP0
      IF(IBCELL(I+1,J,K).EQ.1.AND.T(I+1,J,K).GE.TTWE.AND.DU(ID2+1,JD2,KD2,L,1).GT.ZERO) THEN
         EVWX=EVWX+DN(ID2,JD2,KD2,L)*DU(ID2+1,JD2,KD2,L,1)*RD3(L)
      ENDIF
      IF(IBCELL(I-1,J,K).EQ.1.AND.T(I-1,J,K).GE.TTWE.AND.DU(ID2-1,JD2,KD2,L,1).LT.ZERO) THEN
         EVWX=EVWX+DN(ID2,JD2,KD2,L)*DU(ID2-1,JD2,KD2,L,1)*RD3(L)
      ENDIF                 
      IF(IBCELL(I,J+1,K).EQ.1.AND.T(I,J+1,K).GE.TTWE) THEN
         EVWN=EVWN+DN(ID2,JD2,KD2,L)*MAX(ZERO,DU(ID2,JD2+1,KD2,L,2))*RD3(L)
      ENDIF
      IF(IBCELL(I,J-1,K).EQ.1.AND.T(I,J-1,K).GE.TTWE) THEN
         EVWS=EVWS+DN(ID2,JD2,KD2,L)*MAX(ZERO,-DU(ID2,JD2-1,KD2,L,2))*RD3(L)
      ENDIF
      IF(IBCELL(I,J,K+1).EQ.1.AND.T(I,J,K+1).GE.TTWE.AND.DU(ID2,JD2,KD2+1,L,3).GT.ZERO) THEN
         EVWZ=EVWZ+DN(ID2,JD2,KD2,L)*DU(ID2,JD2,KD2+1,L,3)*RD3(L)
      ENDIF
      IF(IBCELL(I,J,K-1).EQ.1.AND.T(I,J,K-1).GE.TTWE.AND.DU(ID2,JD2,KD2-1,L,3).LT.ZERO) THEN
         EVWX=EVWX+DN(ID2,JD2,KD2,L)*DU(ID2,JD2,KD2-1,L,3)*RD3(L)
      ENDIF
      DTB=DT(ID2,JD2,KD2,L)/(TB(L)-1.0D-10)                 
      IF (DTB.GE.ONE.AND.ELH.GT.ZERO) THEN
         BK=GCP(I,J,K)*(T(I,J,K)-TB(L))/CEV1
         BK=MAX(BK,ZERO)
         FT=LOG(ONE+BK)
         CALL DEVA1(ID2,JD2,KD2,L,G0)
         FT=FT*G0
         DREK=RD(L)*DBMU*SVEL(I,J,K,L)
         DNULK=(ONE+CNU1*SQRT(DREK)*SCIJ)/DNUL0
         FDNU(L)=DN(ID2,JD2,KD2,L)*FT*DNULK/RD(L)
      ELSE
         FDNU(L)=ZERO
      ENDIF
   enddo
   VOL=VOL_C(I,J,K)               
   EVWX=EVWX*RFG*DR(J)*RS(J)*DZ(K)/VOL
   EVWS=EVWS*RFG*RS(J-1)*DX(I)*DZ(K)/VOL
   EVWN=EVWN*RFG*RS(J+1)*DX(I)*DZ(K)/VOL
   EVWZ=EVWZ*RFG*DX(I)*DR(J)*RS(J)/VOL
   !-----SUM EVAPORATION FACTOR OVER SIZE GROUPS
   EVM=RD3(1)*FDNU(1)/DRD(1)
   IF (NDP0.GT.1) THEN
   DO L=2,NDP0
      EVM=EVM+FDNU(L)/DRD(L-1)*(RD3(L)-RD3(L-1))
   END DO
   ENDIF
   !-----COMPUTE NEW NORMALIZED EVAPORATION RATE WITH RELAXATION
   TMP=GC3*GLAM(I,J,K)/GCP(I,J,K)*EVM+EVWX+EVWN+EVWS+EVWZ
   EVP(ID2,JD2,KD2)=TMP*RF(19)+RFC(19)*EVP(ID2,JD2,KD2)
enddo;enddo;enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE DEVA1(ID2,JD2,KD2,L,G0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z) 

G0=ONE
IF(DT(ID2-1,JD2,KD2,L).LT.TB(L).AND.DN(ID2-1,JD2,KD2,L)*DU(ID2-1,JD2,KD2,L,1).GT.ZERO) THEN
   G0=ZERO
   RETURN
ENDIF
IF(DT(ID2+1,JD2,KD2,L).LT.TB(L).AND.DN(ID2+1,JD2,KD2,L)*DU(ID2+1,JD2,KD2,L,1).LT.ZERO) THEN
   G0=ZERO
   RETURN
ENDIF

IF(DT(ID2,JD2-1,KD2,L).LT.TB(L).AND.DN(ID2,JD2-1,KD2,L)*DU(ID2,JD2-1,KD2,L,2).GT.ZERO) THEN
   G0=ZERO
   RETURN
ENDIF
IF(DT(ID2,JD2+1,KD2,L).LT.TB(L).AND.DN(ID2,JD2+1,KD2,L)*DU(ID2,JD2+1,KD2,L,2).LT.ZERO) THEN
   G0=ZERO
   RETURN
ENDIF

IF(DT(ID2,JD2,KD2-1,L).LT.TB(L).AND.DN(ID2,JD2,KD2-1,L)*DU(ID2,JD2,KD2-1,L,3).GT.ZERO) THEN
   G0=ZERO
   RETURN
ENDIF
IF(DT(ID2,JD2,KD2+1,L).LT.TB(L).AND.DN(ID2,JD2,KD2+1,L)*DU(ID2,JD2,KD2+1,L,3).LT.ZERO) THEN
   G0=ZERO
   RETURN
ENDIF
RETURN
END
