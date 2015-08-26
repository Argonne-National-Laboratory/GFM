!**********************************************************************
!***                                                                ***
!***                       RANDO COMMON FUNCTIONS                   ***
!***                                                                ***
!**********************************************************************
!>>>----------------------------------------------------------------<<<
!>>>                                                                <<<
!>>>                        PERMUTATION                             <<<
!>>>                                                                <<<
!>>>----------------------------------------------------------------<<<
      REAL*8 FUNCTION PER(M,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      PER=1.0D+0
      IF (M.LT.N) GOTO 200
      IF ((M.EQ.0).OR.(N.EQ.0)) GOTO 900
      GOTO 300
200   WRITE(6,201)
201   FORMAT(' ERROR in PERMUTATION function.')
      call stop_run("Error in PERMUTATION function.")

300   DO 310 I=M-N+1,M
         PER=PER*I
         IF (PER.GE.1.0D+36) GOTO 200
310   CONTINUE
900   RETURN
      END

!>>>----------------------------------------------------------------<<<
!>>>                                                                <<<
!>>>                RADIATION INTEGRAL FUNCTION                     <<<
!>>>                                                                <<<
!>>>          1983 - Chang and Rhee                                 <<<
!>>>                                                                <<<
!>>>----------------------------------------------------------------<<<
      REAL*8 FUNCTION RIF(M,A,X)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8, ALLOCATABLE :: Y(:)
      DATA ONE,ZERO,TWO,SMALL /1.0D+0,0.0D+0,2.0D+0,1.0D-16/
      DATA PI /3.141592653589793D+0/
      ALLOCATE (Y(M+1))
      RIF=ZERO
      N=0
100   IF (M.LT.0) GOTO 200
110   IF (A.LT.ZERO) GOTO 200
      IF (X.GT.1.0D+16) RETURN
      IF (X.GT.ZERO) GOTO 300
      IF (X.EQ.ZERO) GOTO 400
200   WRITE(6,201)
201   FORMAT( ' ERROR in RIF function.')
      call stop_run("Error in RIF function.")

300   DO 310 I=0,M
310   Y(I+1)=PER(M,I)  !gamma function
      DO N=1,1000   !outer summation loop
         G0=ZERO
         DO 340 I=0,M  !inner summation loop
340      G0=G0+Y(I+1)*X**(M-I)/(A+N)**(I+1)
         G0=G0*EXP(-(A+N)*X)
         RIF=RIF+G0
         IF (ABS(G0).LE.SMALL) GOTO 900
      ENDDO
      GOTO 900
400   N=.5D6**(ONE/(M+1))-A
      G0=ZERO
      DO 420 I=1,N
420   G0=G0+ONE/(A+I)**(M+1)
      RIF=PER(M,M)*(G0+0.5D0*(ONE/(A+N+1)**M+ONE/(A+N)**M)/M)
900   RIF=RIF*1.5D+1/PI**4
      IF (RIF.GT.1.0D+0) RIF=1.0D+0
      DEALLOCATE (Y)
      RETURN
      END

!>>>----------------------------------------------------------------<<<
      REAL*8 FUNCTION EMF(XKL,XL0)
      IMPLICIT REAL*8 (A-H,O-Z)
      DATA INIT/0/
      DATA ONE,X1/1.0D+0,1.0D-3/
      IF (INIT.EQ.0) THEN
         G1=(ONE-EXP(-X1))/X1
         INIT=1
      ENDIF
      X0=XKL*XL0
      IF (X0.LE.X1) THEN
         EMF=X0*G1
      ELSE
         EMF=ONE-EXP(-X0)
      ENDIF
      RETURN
      END


      SUBROUTINE DEBX(TG0)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EB_0=0
      DO L=1,NWL-1 
         X1=DEB0(L)/TG0
         EB_1=RIF(3,ZERO,X1)
         DEB(L)=EB_1-EB_0
         IF (DEB(L).LE.1.0D-3) THEN
            DEB(L)=0
         ELSE
            EB_0=EB_1
         ENDIF
      ENDDO
      DEB(NWL)=ONE-EB_0
      RETURN
      END
