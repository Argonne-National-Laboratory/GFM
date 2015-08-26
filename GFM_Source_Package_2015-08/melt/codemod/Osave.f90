! OSAVE.F90
!     OSAVE saves system state at previous time step
!======================================================================
      SUBROUTINE OSAVE
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (STEADY) RETURN
!----------------------------------------------------------------------
!     Liquid Glass properties
!----------------------------------------------------------------------
      DO I=1,3
         LG0%U(I)=LG%U(I)
      ENDDO
      LG0%P=LG%P
      LG0%T=LG%T
      LG0%DS=LG%DS
      LG0%H=LG%H
      DO I=1,3
         LG0%F(I)=LG%F(I)
      ENDDO
      IF (NPHAS.EQ.1) RETURN
!----------------------------------------------------------------------
!     Liquid/Solid phase properties
!----------------------------------------------------------------------
      if(nps_c>0) PCT=PC
      if(nps_s>0) PST=PS
      GBT=GB4
      RETURN
      END
