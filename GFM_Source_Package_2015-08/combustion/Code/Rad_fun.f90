!======================================================================
!======================================================================
!======================================================================
!
! Rad_fun.f90
!
!                       RANDO COMMON FUNCTIONS                   
!                                                                
!======================================================================
!                                                                
!                        PERMUTATION                             
!                                                                
!======================================================================
REAL*8 FUNCTION PER(M,N)
IMPLICIT REAL*8 (A-H,O-Z)

PER=1.0D+0
IF (M.LT.N) then
   WRITE(6,*) ' ERROR in PERMUTATION function.'
   call stop_run("Error in PERMUTATION function.")
endif

IF ((M.EQ.0).OR.(N.EQ.0)) return

DO I=M-N+1,M
   PER=PER*I
   IF (PER.GE.1.0D+36) then
      WRITE(6,*) ' ERROR in PERMUTATION function.'
      call stop_run("Error in PERMUTATION function.")
   endif
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!                                                                
!                RADIATION INTEGRAL FUNCTION                     
!                                                                
!          1983 - Chang and Rhee                                 
!                                                                
!======================================================================
REAL*8 FUNCTION RIF(M,A,X)
IMPLICIT REAL*8 (A-H,O-Z)
REAL*8, ALLOCATABLE :: Y(:)
DATA ONE,ZERO,TWO,SMALL16 /1.0D+0,0.0D+0,2.0D+0,1.0D-16/
DATA PI /3.141592653589793D+0/

RIF=ZERO

IF (M.LT.0 .or. A.LT.ZERO) then
   WRITE(6,*) ' ERROR in RIF function.'
   call stop_run("Error in RIF function.")
endif

IF (X.GT.1.0D+16) RETURN

ALLOCATE (Y(M+1))
Y=0
N=0

IF (X.EQ.ZERO) then
   N=.5D6**(ONE/(M+1))-A
   G0=ZERO
   DO I=1,N
      G0=G0+ONE/(A+I)**(M+1)
   enddo
   RIF=PER(M,M)*(G0+0.5D0*(ONE/(A+N+1)**M+ONE/(A+N)**M)/M)
else
   DO I=0,M
      Y(I+1)=PER(M,I)  !gamma function
   enddo

   DO N=1,1000   !outer summation loop
      G0=ZERO
      DO I=0,M  !inner summation loop
         G0=G0+Y(I+1)*X**(M-I)/(A+N)**(I+1)
      enddo
      G0=G0*EXP(-(A+N)*X)
      RIF=RIF+G0
      IF (ABS(G0).LE.SMALL16) exit
   ENDDO
endif

RIF=RIF*1.5D+1/PI**4
IF (RIF.GT.1.0D+0) RIF=1.0D+0
DEALLOCATE (Y)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
REAL*8 FUNCTION EMF(XKL,XL0)
IMPLICIT REAL*8 (A-H,O-Z)
DATA INIT/0/
DATA ONE,X1/1.0D+0,1.0D-3/
!first time initialization
IF (INIT.EQ.0) THEN
   G1=(ONE-EXP(-X1))/X1
   INIT=1
   !init and g1 will retain their new values between function calls
ENDIF

X0=XKL*XL0
IF (X0.LE.X1) THEN
   EMF=X0*G1
ELSE
   EMF=ONE-EXP(-X0)
ENDIF
RETURN
END


!======================================================================
!======================================================================
!  DEB = Distribution of Energy over wavelength Bands
!======================================================================
SUBROUTINE DEBX(TG0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!note:  DEB0(1:NWL)=HCK/RI/WL(1:NWL)

EB0=0
DO L=1,NWL-1 
   X1=DEB0(L)/TG0
   EB1=RIF(3,ZERO,X1)
   DEB(L)=EB1-EB0
   IF (DEB(L).LE.1.0D-3) THEN
      DEB(L)=zero
   ELSE
      EB0=EB1
   ENDIF
ENDDO
DEB(NWL)=ONE-EB0
RETURN
END
