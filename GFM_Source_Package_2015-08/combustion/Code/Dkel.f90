! Dkel.f90
!======================================================================
!     DKEL is used to modify the gas phase turbulent
!          kinetic energy for the existence of droplets/particles
!       revision, 4/98
!======================================================================
SUBROUTINE DKEL(MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION FF1(3,2),FF2(3,2),DXLP(NDNP),DNUM(3),DNUP(3),AREA(3)

DO L=1,NDNP
   DXLP(L)=RD3(L)/PSI(L)**2
ENDDO

DO I=MI1,MI3,2;  ID2=I/2
DO J=NJ1,NJ3,2;  JD2=J/2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) cycle
   KD2=K/2
   CALL INTP1(FF1(1,1),FF2(1,1),I-1,J,K,1)
   CALL INTP1(FF1(1,2),FF2(1,2),I+1,J,K,1)
   CALL INTP1(FF1(2,1),FF2(2,1),I,J-1,K,2)
   CALL INTP1(FF1(2,2),FF2(2,2),I,J+1,K,2)
   CALL INTP1(FF1(3,1),FF2(3,1),I,J,K-1,3)
   CALL INTP1(FF1(3,2),FF2(3,2),I,J,K+1,3)
   AREA(1)=AREA_C(I,J,K,1)
   AREA(2)=AREA_C(I,J,K,2)
   AREA(3)=AREA_C(I,J,K,3)

   DO L=1,NDNP
      DNUM(1)=DU(ID2-1,JD2,KD2,L,1)*DN(ID2-1,JD2,KD2,L)
      DNUP(1)=DU(ID2+1,JD2,KD2,L,1)*DN(ID2+1,JD2,KD2,L)
      IF (NP.GT.6) THEN
         DNUM(2)=DU(ID2,JD2-1,KD2,L,2)*DN(ID2,JD2-1,KD2,L)
         DNUP(2)=DU(ID2,JD2+1,KD2,L,2)*DN(ID2,JD2+1,KD2,L)
      ELSE
         DNUM(2)=ZERO
         DNUP(2)=ZERO
      ENDIF
      IF (LP.GT.6) THEN
         DNUM(3)=DU(ID2,JD2,KD2-1,L,3)*DN(ID2,JD2,KD2-1,L)
         DNUP(3)=DU(ID2,JD2,KD2+1,L,3)*DN(ID2,JD2,KD2+1,L)
      ELSE
         DNUM(3)=ZERO
         DNUP(3)=ZERO
      ENDIF

      DO IU=1,3
         IF (IU.EQ.2.AND.NP.LE.6) cycle
         IF (IU.EQ.3.AND.LP.LE.6) cycle
         DNU0=DU(ID2,JD2,KD2,L,IU)*DN(ID2,JD2,KD2,L)
         DUL=FF1(IU,1)*DNU0+FF2(IU,1)*DNUM(IU)
         DEL=RFG*AREA(IU)*DXLP(L)*DUL
         IF (IU.EQ.2) DEL=DEL*RS(J-1)/RS(J)
         IF (DEL.GE.ZERO) THEN
            AS(I,J,K,IU,1)=AS(I,J,K,IU,1)+DEL
         ELSE
            AP(I,J,K)=AP(I,J,K)-DEL
         ENDIF
         DUR=FF1(IU,2)*DNUP(IU)+FF2(IU,2)*DNU0
         DER=RFG*AREA(IU)*DXLP(L)*DUR
         IF (IU.EQ.2) DER=DER*RS(J+1)/RS(J)
         IF (DER.LE.ZERO) THEN
            AS(I,J,K,IU,2)=AS(I,J,K,IU,2)-DER
         ELSE
            AP(I,J,K)=AP(I,J,K)+DER
         ENDIF
      enddo !iu=
   enddo !L=
enddo;enddo;enddo
RETURN
END
