! SOURCEM.F90
!======================================================================
!     SOURCEM calculates source terms for kinetic species in a 
!     multi-species flow which may also be reacting when those kinetic
!     species have an associated transport equation.
!       9/00
!======================================================================
      SUBROUTINE SOURCEM(MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !CZ      GOTO(100,200,300,400,500,600,700),NELM
!----------------------------------------------------------------------
!-----Sub-Species 
!----------------------------------------------------------------------
      DO 120 I=MI1,MI3,2
      DO 120 J=NJ1,NJ3,2
      DO 120 K=LK1,LK3,2
         SFP(I,J,K)=ZERO
         BS(I,J,K)=ZERO
         SFP(I,J,K)=ZERO
         BS(I,J,K)=ZERO
         IF (IBCELL(I,J,K).GT.0) CYCLE
         EVMS=LG(I,J,K)%MR
         IF (EVMS.LE.0) CYCLE
         BS(I,J,K)=EVMS 
120   CONTINUE
      RETURN
      END
