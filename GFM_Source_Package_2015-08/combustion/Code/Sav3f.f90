!==============================================================================
!==============================================================================
!==============================================================================
!
! SAV3F.F90
!
! Sav3f saves data in files for restart capability
!
! This module contains the following routines:
!     sav3f
!     sav3fm
!
!======================================================================
!======================================================================
!======================================================================
!     SAV3F saves data on disk for restart capability
!        GF: general property functions
!        LEND: end of GF calculation
!     Revision: 9/01
!======================================================================
SUBROUTINE SAV3F
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

filename=casedir//'\rg'//runum//'c.d'
OPEN(nu_restg,FILE=FILENAME,FORM='UNFORMATTED')
DO I=2,MP,2;  ID2=I/2
DO J=2,NP,2;  JD2=J/2
DO K=2,LP,2;  KD2=K/2
   G1=P(I,J,K)
   IF(I.EQ.MP) THEN
     G2=UG(MPM1,J,K,1)
   ELSE
     G2=UG(I+1,J,K,1)
   ENDIF
   IF(J.EQ.NP) THEN
     G3=UG(I,NPM1,K,2)
   ELSE
     G3=UG(I,J+1,K,2)
   ENDIF
   IF(K.EQ.LP) THEN
     G4=UG(I,J,LPM1,3)
   ELSE
     G4=UG(I,J,K+1,3)
   ENDIF
   G5=T(I,J,K)
   G6=THETA(I,J,K)
   WRITE (nu_restg) G1,G2,G3,G4,G5,G6
   WRITE (nu_restg) (GF(I,J,K,L),L=1,LEND)
enddo;enddo;enddo

IF (NPHAS .gt. 1) then
   DO L=1,NDNP
   DO I=2,MP,2;  ID2=I/2
   DO J=2,NP,2;  JD2=J/2
   DO K=2,LP,2;  KD2=K/2
      IF(L.EQ.1) THEN 
        WRITE (nu_restg) EVP(ID2,JD2,KD2),CON(ID2,JD2,KD2),SP(I,J,K)
        WRITE (nu_restg) TH_DP(ID2,JD2,KD2),TH_PT(ID2,JD2,KD2)
      ENDIF
      G1=DT(ID2,JD2,KD2,L)
      G2=DN(ID2,JD2,KD2,L)
      G3=DU(ID2,JD2,KD2,L,1)
      G4=DU(ID2,JD2,KD2,L,2)
      G5=DU(ID2,JD2,KD2,L,3)
      IF(L.LE.NDP0) THEN
        WRITE (nu_restg) G1,G2,G3,G4,G5
      ELSE
        L1=L-NDP0
        G6=DC(ID2,JD2,KD2,L1)
        WRITE (nu_restg) G1,G2,G3,G4,G5,G6
      ENDIF
   enddo;enddo;enddo;enddo
endif

write (nu_restg) bs_hprev
write (nu_restg) T_surf_prev

WRITE (nu_restg) FACTOR,FADJ
write (nu_restg) i1st_rad_qe_done
write (nu_restg) i1st_rad_done
WRITE (nu_restg) itr_gas
WRITE (nu_restg) itr_rad,itr_gend
write (nu_restg) interval_rad
CLOSE (nu_restg)

RETURN
END


!======================================================================
!======================================================================
!======================================================================
!  Sav3fm saves data on disk for restart capability for minor species
!======================================================================
subroutine sav3fm
use gbl_var
implicit double precision (a-h,o-z)

filename=casedir//'\rs'//runum//'c.d'
open(nu_rests,file=filename,form='unformatted')
do i=2,mp,2
do j=2,np,2
do k=2,lp,2
   write (nu_rests) (gfm(i,j,k,l),l=1,nsp0),smf(i,j,k)
enddo;enddo;enddo

write (nu_rests) bs_soot_prev
write (nu_rests) sfp_soot_prev

write (nu_rests) rrms
write (nu_rests) itr_ms
close (nu_rests)

RETURN
END
