! GSLV3X.F90
!======================================================================
!     Utility subprograms for GSLV3
!     IU=1: X direction
!        2: R-direction 
!        3: Z-direction
!     revision:  10/97
!======================================================================
SUBROUTINE GSLVX1(MI1,MI3,NJ1,NJ3,LK1,LK3,IU1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION NX(3),FFL(3,2)
NX=0
NX(IU1)=1
!      DO 100 I=2+NX(1),MP-NX(1),2
!      DO 100 J=2+NX(2),NP-NX(2),2
!  DO 100 K=2+NX(3),LP-NX(3),2
!100   FZ(I,J,K)=UG(I,J,K,IU1)
MI1=4+NX(1)
MI3=MPM2-NX(1)
NJ1=NJY1V(IU1)
NJ3=NJY2V(IU1)
LK1=LKZ1V(IU1)
LK3=LKZ2V(IU1)
CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
DO I=MI1,MI3,2
DO J=NJ1,NJ3,2
DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) cycle
   AP(I,J,K)=ZERO
   G0=ZERO
   CALL GSLV3X1(I,J,K,IU1,G0)
   CALL ECELL(I,J,K,IU1,FFL)

   DO IU=1,3
      IF (IU.EQ.IU1) cycle
      DO M=-1,1,2
         M1=(M+3)/2
         CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
         IF (IBCELL(I1,J1,K1).EQ.3) THEN
            AS(I,J,K,IU,M1)=ZERO
            cycle
         ENDIF
         F=ZERO
         DIFF=ZERO
         DO N=-1,1,2
            N1=(N+3)/2
            II1=I1+NX(1)*N
            JJ1=J1+NX(2)*N
            KK1=K1+NX(3)*N
            IBC1=IBCELL(II1,JJ1,KK1)
            IF (IBC1.EQ.3) cycle
            AREA=FFL(IU,N1)
            F=F+THETA(II1,JJ1,KK1)*DNST(II1,JJ1,KK1)*FZ(II1,JJ1,KK1)*AREA
            IF (IBC1.EQ.2) cycle
            DDL=FDDL(II1,JJ1,KK1,IU)
            DIFF=DIFF+THETA(II1,JJ1,KK1)*GAMA(II1,JJ1,KK1)/SGM*AREA/DDL
         enddo !n=
         !CSL            FLD=FLD+FLWD1(II1,JJ1,KK1,M1,AREA,DDL)
         IF (M1.EQ.1) THEN
            FLD=DA(DIFF,F)+MAX(ZERO,F)
         ELSE
            FLD=DA(DIFF,F)+MAX(ZERO,-F)
         ENDIF

         IF (IBCELL(II1,JJ1,KK1).EQ.2.AND.IBCELL(I2,J2,K2).EQ.1) THEN
            BS0=FLD*(UG(II1,J2,K2,IU1)-UG(I,J,K,IU1))
            BS(I,J,K)=BS(I,J,K)+BS0
            AS(I,J,K,IU,M1)=ZERO
            cycle
         ENDIF

         AS(I,J,K,IU,M1)=FLD
         AP(I,J,K)=AP(I,J,K)+FLD
         G0=G0+FLD*FZ(I2,J2,K2)
      enddo !m=
   enddo !iu=

   if (ndnp==0) then
      evpm=zero
      conm=zero
   else
      IF (IU1.EQ.1) THEN
         F2=(X(I)-X(I-1))/DX(I)
      ELSEIF (IU1.EQ.2) THEN
         F2=(R(J)-R(J-1))/DR(J)
      ELSE
         F2=(Z(K)-Z(K-1))/DZ(K)
      ENDIF
      F1=ONE-F2
      I1=(I-NX(1))/2
      I2=(I+NX(1))/2
      J1=(J-NX(2))/2
      J2=(J+NX(2))/2
      K1=(K-NX(3))/2
      K2=(K+NX(3))/2
      EVPM=EVP(I1,J1,K1)*F1+EVP(I2,J2,K2)*F2
      CONM=CON(I1,J1,K1)*F1+CON(I2,J2,K2)*F2
   endif
   VOL=VOL1(I,J,K,IU1,FFL)
   BS(I,J,K)=BS(I,J,K)*VOL
   AP(I,J,K)=AP(I,J,K)+(EVPM-CONM-SFP(I,J,K))*VOL
!   IF (.NOT.STEADY) THEN
!      APOD=APO(I,J,K)*VOL
!      BS(I,J,K)=BS(I,J,K)+APOD*UGO(I,J,K,IU1)
!      AP(I,J,K)=AP(I,J,K)+APOD
!   ENDIF
   IF (SMPLER.AND.ABS(AP(I,J,K)).GT.SMALL20) THEN
      UG(I,J,K,IU1)=(G0+BS(I,J,K))/AP(I,J,K)
   ENDIF
enddo;enddo;enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE GSLV3X1(I,J,K,IU,G0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
G0=ZERO
DO M=-1,1,2
   M1=(M+3)/2
   CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
   IBC1=IBCELL(I2,J2,K2)
   IF (IBC1.EQ.3) THEN
      !CSL         IF (IBC1.EQ.3.OR.IBC1.EQ.1) THEN
      AS(I,J,K,IU,M1)=ZERO
   ELSEIF (IBC1.EQ.2) THEN
      AS(I,J,K,IU,M1)=FLW(I2,J2,K2,IU,M1)
   ELSE
      AS(I,J,K,IU,M1)=FLWD(I1,J1,K1,IU,M1)
   ENDIF
   AP(I,J,K)=AP(I,J,K)+AS(I,J,K,IU,M1)
   G0=G0+AS(I,J,K,IU,M1)*FZ(I2,J2,K2)
ENDDO
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!        IF OUTFLOW BOUNDARY OR NEXT MOMENTUM CELL BLOCKED,
!        EXTEND CELL
!======================================================================
SUBROUTINE ECELL(I,J,K,IU,FFL)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION FFL(3,2)
IF (IU.EQ.1) THEN
   I1=I-1
   I2=I-2
   DO M=1,2
      IF (IBCELL(I2,J,K).GE.1) THEN
         FFL(1,M)=DX(I1)
      ELSE
         FFL(1,M)=ABS(X(I)-X(I1))
      ENDIF
      FFL(2,M)=FFL(1,M)*DZ(K)
      FFL(3,M)=FFL(1,M)*RS(J)*DR(J)
      I1=I+1
      I2=I+2
   ENDDO
ELSEIF (IU.EQ.2) THEN
   J1=J-1
   J2=J-2
   DO M=1,2
      IF (IBCELL(I,J2,K).GE.1) THEN
         FFL(2,M)=RS(J1)*DR(J1)
      ELSE
         FFL(2,M)=RS(J1)*ABS(R(J)-R(J1))
      ENDIF
      FFL(1,M)=FFL(2,M)*DZ(K)
      FFL(3,M)=FFL(2,M)*DX(I)
      J1=J+1
      J2=J+2
   ENDDO
ELSE
   K1=K-1
   K2=K-2
   DO M=1,2
      IF (LP.LT.6) THEN
         K2=2
         K1=1
      ENDIF
      IF (IBCELL(I,J,K2).GE.1) THEN
         FFL(3,M)=DZ(K1)
      ELSE
         FFL(3,M)=ABS(Z(K)-Z(K1))
      ENDIF
      FFL(1,M)=FFL(3,M)*RS(J)*DR(J)
      FFL(2,M)=FFL(3,M)*DX(I)
      K1=K+1
      K2=K+2
   ENDDO
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!        Diffusivity length
!======================================================================
DOUBLE PRECISION FUNCTION FDDL(I,J,K,IU)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (IU.EQ.1) THEN
   IF (IBCELL(I,J,K).LE.0) THEN
      DDL=DX(I)
   ELSEIF (IBCELL(I,J,K).GE.1) THEN
      !csl         ELSEIF (IBCELL(I,J,K).EQ.1) THEN
      IF (IBCELL(I+1,J,K).EQ.0) THEN
         DDL=X(I+1)-X(I)
      ELSEIF (IBCELL(I-1,J,K).EQ.0) THEN
         DDL=X(I)-X(I-1)
      ELSE
         DDL=BIG
      ENDIF
   ELSE
      DDL=BIG
   ENDIF
ELSEIF (IU.EQ.2) THEN
   IF (IBCELL(I,J,K).LE.0) THEN
      DDL=DR(J)*RS(J)
   ELSEIF (IBCELL(I,J,K).GE.1) THEN
      !CSL         ELSEIF (IBCELL(I,J,K).EQ.1) THEN
      IF (IBCELL(I,J+1,K).EQ.0) THEN
         DDL=(R(J+1)-R(J))*RS(J)
      ELSEIF (IBCELL(I,J-1,K).EQ.0) THEN
         DDL=(R(J)-R(J-1))*RS(J)
      ELSE
         DDL=BIG
      ENDIF
   ELSE
      DDL=BIG
   ENDIF
ELSE
   IF (IBCELL(I,J,K).LE.0) THEN
      DDL=DZ(K)
   ELSEIF (IBCELL(I,J,K).GE.1) THEN
      !CSL         ELSEIF (IBCELL(I,J,K).EQ.1) THEN
      IF (IBCELL(I,J,K+1).EQ.0) THEN
         DDL=Z(K+1)-Z(K)
      ELSEIF (IBCELL(I,J,K-1).EQ.0) THEN
         DDL=Z(K)-Z(K-1)
      ELSE
         DDL=BIG
      ENDIF
   ELSE
      DDL=BIG
   ENDIF
ENDIF
!CSL      IF(IBCELL(I,J,K).LE.-1.AND.IU.NE.1) DDL=DDL/TWO
FDDL=DDL
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION VOL1(I,J,K,IX,FFL)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION FFL(3,2)
AREA=AREA_C(I,J,K,IX)
IF (IX.EQ.1) THEN
   VOL=(FFL(IX,1)+FFL(IX,2))*AREA
   IF (IBCELL(I-1,J,K).LE.-1) VOL=VOL-DX(I-1)*AREA/TWO 
   IF (IBCELL(I+1,J,K).LE.-1) VOL=VOL-DX(I+1)*AREA/TWO 
ELSEIF (IX.EQ.2) THEN
   VOL=(FFL(IX,1)+FFL(IX,2))*AREA
   IF (IBCELL(I,J-1,K).LE.-1) VOL=VOL-RS(J-1)*DR(J-1)*AREA/TWO 
   IF (IBCELL(I,J+1,K).LE.-1) VOL=VOL-RS(J+1)*DR(J+1)*AREA/TWO 
ELSE
   VOL=(FFL(IX,1)+FFL(IX,2))*AREA
   IF (IBCELL(I,J,K-1).LE.-1) VOL=VOL-DZ(K-1)*AREA/TWO 
   IF (IBCELL(I,J,K+1).LE.-1) VOL=VOL-DZ(K+1)*AREA/TWO 
ENDIF
VOL1=VOL
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION FLW(I,J,K,IX,M)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
AREA=AREA_C(I,J,K,IX)
F=THETA(I,J,K)*DNST(I,J,K)*UG(I,J,K,IX)*AREA
IF (M.EQ.1) THEN
   FLW=MAX(ZERO,F)
ELSE
   FLW=MAX(ZERO,-F)
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION FLWD(I,J,K,IX,M)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
AREA=AREA_C(I,J,K,IX)                     
IF (IX.EQ.1) THEN
   DDL=DX(I)
ELSEIF (IX.EQ.2) THEN
   DDL=DR(J)
ELSE
   DDL=DZ(K)
ENDIF
!CSL      IF(IBCELL(I,J,K).LE.-1.AND.IX.NE.1) DDL=DDL/TWO
F=THETA(I,J,K)*DNST(I,J,K)*UG(I,J,K,IX)*AREA
DIFF=THETA(I,J,K)*GAMA(I,J,K)/SGM*AREA/DDL
IF (M.EQ.1) THEN
   FLWD=DA(DIFF,F)+MAX(ZERO,F)
ELSE
   FLWD=DA(DIFF,F)+MAX(ZERO,-F)
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!      DOUBLE PRECISION FUNCTION FLWD1(I,J,K,M,AREA,DDL)
!  USE GBL_VAR
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!      IF(IBCELL(I,J,K).LE.-1) DDL=DDL/TWO
!      F=THETA(I,J,K)*DNST(I,J,K)*FZ(I,J,K)*AREA
!      DIFF=THETA(I,J,K)*GAMA(I,J,K)/SGM*AREA/DDL
!      IF (M.EQ.1) THEN
!         FLWD1=DA(DIFF,F)+MAX(ZERO,F)
!      ELSE
!         FLWD1=DA(DIFF,F)+MAX(ZERO,-F)
!      ENDIF
!      RETURN
!      END
