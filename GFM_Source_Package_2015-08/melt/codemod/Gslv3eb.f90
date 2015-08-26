! GSLV3EB.F90
!     solves electric potential energy 
!======================================================================
      SUBROUTINE GSLV3EB
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      NELG=9 
      KD=2
      MI1=4
      MI3=MPM2
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2

      FZ=EBV
      BS=0
      CALL GSLV3EB0(MI1,MI3,NJ1,NJ3,LK1,LK3,0)
      CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
      DO 560 I=MI1,MI3,2
         DO 560 J=NJ1,NJ3,2
         DO 560 K=LK1,LK3,2
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.GE.1) GOTO 560
              EBV(I,J,K)=RFLC(9)*EBV(I,J,K)+RFL(9)*FZ(I,J,K)
560      CONTINUE
!czeb-------
      AVEEBV=RESG(NELG,1)
      RETURN
      END

!======================================================================
!---  flux coefficients AS
!======================================================================
      SUBROUTINE GSLV3EB0(MI1,MI3,NJ1,NJ3,LK1,LK3,IGS)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DO 550 I=MI1,MI3,2
      DO 550 J=NJ1,NJ3,2
      DO 550 K=LK1,LK3,2
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.GE.1) THEN
            AP(I,J,K)=1
            AS(I,J,K,1:3,1:2)=0
            BS(I,J,K)=FZ(I,J,K)
            GOTO 550
         ENDIF
551            AP(I,J,K)=ZERO
         DO 520 IU=1,3
            IF (IU.EQ.2.AND.NP.LE.6) GOTO 520
            IF (IU.EQ.3.AND.LP.LE.6) GOTO 520
            DO M=-1,1,2
               M1=(M+3)/2
               CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
               IBC0=MOD(IBCELL(I2,J2,K2),10)
               IBC2=IBCELL(I2,J2,K2)
               IF (IBC0.LE.0) THEN
                  AS(I,J,K,IU,M1)=FLWEB(I1,J1,K1,IU,M1)
                  AP(I,J,K)=AP(I,J,K)+AS(I,J,K,IU,M1)
                  !CZ               ELSEIF (IBC0.EQ.5.OR.IBC0.EQ.6) THEN
               ELSEIF (IBC0.EQ.5) THEN
                   AS(I,J,K,IU,M1)=0  
                   G0=FLWEB(I1,J1,K1,IU,M1) 
                   BS(I,J,K)=BS(I,J,K)+G0*FZ(I2,J2,K2)
                   AP(I,J,K)=AP(I,J,K)+G0
               ELSE    
                  AS(I,J,K,IU,M1)=ZERO
               ENDIF
            ENDDO
520      CONTINUE
550   CONTINUE
      RETURN
      END

!======================================================================
      DOUBLE PRECISION FUNCTION FLWEB(I,J,K,IX,M)
!======================================================================
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      AREA=AREA_C(I,J,K,IX)
       if(ix.eq.1)then
        if(m.eq.1) FlWEB=EBSG(I-1,J,K)*AREA
        if(m.eq.2)  FLWEB=EBSG(I+1,J,K)*AREA
       elseif(ix.eq.2) then
        if(m.eq.1) FLWEB=EBSG(I,J-1,K)*AREA
        if(m.eq.2)  FLWEB=EBSG(I,J+1,K)*AREA
       else
        if(m.eq.1) FLWEB=EBSG(I,J,K-1)*AREA
        if(m.eq.2)  FLWEB=EBSG(I,J,K+1)*AREA
       endif
      !cz    IF (M.EQ.1) THEN
      !cz         FLWEB=MAX(ZERO,FLWEB)
      !cz      ELSE
      !cz         FLWEB=MAX(ZERO,-FLWEB)
      !cz      ENDIF
      RETURN
      END
