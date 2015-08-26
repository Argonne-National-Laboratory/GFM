!======================================================================
!======================================================================
!======================================================================
! Rad_soot.for
!======================================================================
!**********************************************************************
!***                                                                ***
!***                     SOOT RADIATION                             ***
!***                                                                ***
!***          1983 - Chang and Rhee                                 ***
!***                                                                ***
!**********************************************************************
!>>>----------------------------------------------------------------<<<
!>>>                                                                <<<
!>>>            MEAN SPECTRAL SOOT ABSORPTIVITY                     <<<
!>>>                                                                <<<
!>>>         1966 - Howarth, Forster and Thring                     <<<
!>>>         1969 - Dalzell and Sarofim                             <<<
!>>>         1981 - Lee and Tien                                    <<<
!>>>                                                                <<<
!>>>         WL - dimensionless wave length ( ref:6u )              <<<
!>>>         FV - soot volume fraction                              <<<
!>>>         SOOTA - mean soot absorptance                          <<<
!>>>                                                                <<<
!>>>----------------------------------------------------------------<<<
SUBROUTINE SOOTA
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (FVS.LT.1.0D-14) RETURN
DO N=1,NWL
   WL0=WL(N)
   CALL SOC(WL0,XN_K,X2NK)
   G0=18.0D+0*PI*X2NK/((XN_K+TWO)**2+X2NK*X2NK)
   AKLS(N)=G0*FVS/WL0
enddo
RETURN
END
!>>>----------------------------------------------------------------<<<
!>>>                                                                <<<
!>>>               OPTICAL CONSTANTS OF SOOT                        <<<
!>>>                                                                <<<
!>>>          1981 - Lee and Tien                                   <<<
!>>>                                                                <<<
!>>>        - dimensionless operation                               <<<
!>>>        - electronic dispersion function                        <<<
!>>>        - dispersion constants, F, G & WN (Lee & Tien)          <<<
!>>>                                                                <<<
!>>>----------------------------------------------------------------<<<
SUBROUTINE SOC(WL0,XN_K,X2NK)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION SOCF(3),SOCH(3),SOCW(3)
DATA SOCF/7.2D+26,4.07D+27,4.47D+28/
DATA SOCH/1.2D+15,5.90D+15,5.60D+15/
DATA SOCW/0.0D+0,1.25D+15,7.25D+15/
DATA C_0,E2ME0/2.9984D+8,3.185D+3/
XN_K=ONE
X2NK=ZERO
W=TWO*PI*C_0/WL0
DO J=1,3
   A=SOCW(J)*SOCW(J)-W*W
   B=A*A+W*W*SOCH(J)*SOCH(J)
   IF (B.LT.1.0D-20) return
   G0=SOCF(J)*E2ME0
   XN_K=XN_K+G0*A/B
   X2NK=X2NK+G0*SOCH(J)*W/B
enddo
RETURN
END
