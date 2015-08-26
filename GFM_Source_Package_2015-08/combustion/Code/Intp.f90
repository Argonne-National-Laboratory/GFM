!==============================================================================
!==============================================================================
!==============================================================================
!
! Intp.f90
!
! Intp interpolates the calculated variables
!
! This module contains the following routines:
!     intp
!     intpv
!     intp0(f1,i,j,k,i1,j1,k1,i2,j2,k2)
!     intp1(f1,f2,i,j,k,ix)
!     intp1m(f1,f2,i,j,k,ix,m)
!     intpv1(m,n,l,iu,ix)
!     intpsp
!     intpsp1(sp1,sp1p,sp1m,ibc0,iu)
!     intpm
!     intp0m(f1,i,j,k,i1,j1,k1,i2,j2,k2)
!
!======================================================================
!======================================================================
!======================================================================
!     Interpolation of main calculated variables
!       10/97
!---------------------------------------------------------------------
!        General Properties: P,T,DNST,GF,THETA
!        Boundary Conditions: linear (P), zero slope (T,GF,THETA)
!           F1,F2: linear coefficients
!           X01,X02: zero slope coefficients
!======================================================================
SUBROUTINE INTP
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!---------------------------------------------------------------------
!     Interior nodes
!---------------------------------------------------------------------
DO I=2,MP,2
   DO K=3,LPM1,2
      KP1=K+1
      KM1=K-1
      CALL INTP1(F1,F2,I,J,K,3)
      DO J=2,NP,2
         IF (IBCELL(I,J,K).EQ.1) cycle
         CALL INTP0(F1,I,J,K,I,J,KP1,I,J,KM1)
      enddo
   enddo

   DO J=3,NPM1,2
      JP1=J+1
      JM1=J-1
      CALL INTP1(F1,F2,I,J,K,2)
      DO K=2,LP
         IF (IBCELL(I,J,K).EQ.1) cycle
         CALL INTP0(F1,I,J,K,I,JP1,K,I,JM1,K)
      enddo
   enddo
enddo

DO I=3,MPM1,2
   IP1=I+1
   IM1=I-1
   CALL INTP1(F1,F2,I,J,K,1)
   DO K=2,LP
   DO J=2,NP
      IF (IBCELL(I,J,K).EQ.1) cycle
      CALL INTP0(F1,I,J,K,IP1,J,K,IM1,J,K)
      enddo
   enddo
enddo
return
end


!======================================================================
!======================================================================
!======================================================================
!     Interpolation of calculated velocity components
!       IU=1: X-velocity U
!          2: Y-velocity V
!          3: Z-velocity W
!       revision: 10/97
!======================================================================
SUBROUTINE INTPV
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

DO IU=1,3
   IF (IU.EQ.1) THEN
      M3=MP
      N3=NP
      L3=LP
   ELSEIF (IU.EQ.2) THEN
      M3=NP
      N3=LP
      L3=MP
   ELSE
      M3=LP
      N3=MP
      L3=NP
   ENDIF

   DO M=3,M3-1,2
      IF (N3.LE.3) GOTO 125
      DO L=2,L3,2
      DO N=3,N3-1,2
         IF (IU.EQ.1) THEN
            IX=2
         ELSEIF (IU.EQ.2) THEN
            IX=3
         ELSE
            IX=1
         ENDIF
         CALL INTPV1(M,N,L,IU,IX)
      enddo;enddo

125         continue

      IF (L3.LE.3) cycle
      DO L=3,L3-1,2
      DO N=2,N3
         IF (IU.EQ.1) THEN
            IX=3
         ELSEIF (IU.EQ.2) THEN
            IX=1
         ELSE
            IX=2
         ENDIF
         CALL INTPV1(M,N,L,IU,IX)
      enddo;enddo
   enddo
   DO M=4,M3,2
   DO N=2,N3
   DO L=2,L3
      CALL INTPV1(M,N,L,IU,IU)
   enddo;enddo;enddo
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTP0(F1,I,J,K,I1,J1,K1,I2,J2,K2)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

IF (IBCELL(I,J,K).EQ.3) RETURN
IF (IBCELL(I,J,K).eq.2) then
   IF (IBCELL(I1,J1,K1).EQ.2.AND.IBCELL(I2,J2,K2).EQ.0) THEN
      F1=ONE
   ELSEIF (IBCELL(I1,J1,K1).EQ.0.AND.IBCELL(I2,J2,K2).EQ.2) THEN
      F1=ZERO
   ENDIF
endif
F2=ONE-F1

P(I,J,K)=F1*P(I1,J1,K1)+F2*P(I2,J2,K2)
T(I,J,K)=F1*T(I1,J1,K1)+F2*T(I2,J2,K2)
!CSL      DO L=1,9
DO L=LSTAR,LEND
   GF(I,J,K,L)=F1*GF(I1,J1,K1,L)+F2*GF(I2,J2,K2,L)
ENDDO
CALL CONC(I,J,K)
TMU(I,J,K)=F1*TMU(I1,J1,K1)+F2*TMU(I2,J2,K2)
GCP(I,J,K)=F1*GCP(I1,J1,K1)+F2*GCP(I2,J2,K2)
GLAM(I,J,K)=F1*GLAM(I1,J1,K1)+F2*GLAM(I2,J2,K2)
GAMA(I,J,K)=F1*GAMA(I1,J1,K1)+F2*GAMA(I2,J2,K2)
THETA(I,J,K)=F1*THETA(I1,J1,K1)+F2*THETA(I2,J2,K2)
!CALL WTDN(I,J,K,WMIX)
call dnst_calc(i,j,k)
!csl      DNST(I,J,K)=(ONE+P(I,J,K))*WMIX/WT0/T(I,J,K)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTP1(F1,F2,I,J,K,IX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (IX.EQ.3) THEN
   IF (DZ(K).LE.ZERO.OR.K.GT.LP) THEN
      F1=ZERO
   ELSE
      F1=(Z(K)-Z(K-1))/DZ(K)
   ENDIF
ELSEIF (IX.EQ.2) THEN
   IF (DR(J).LE.ZERO.OR.J.GT.NP) THEN
      F1=ZERO
   ELSE
      F1=(R(J)-R(J-1))/DR(J)
   ENDIF
ELSE
   IF (DX(I).LE.ZERO.OR.I.GT.MP) THEN
      F1=ZERO
   ELSE
      F1=(X(I)-X(I-1))/DX(I)
   ENDIF
ENDIF
F2=ONE-F1

RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTP1M(F1,F2,I,J,K,IX,M)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CALL INTP1(G1,G2,I,J,K,IX)
IF (M.EQ.1) THEN
   F1=G1
   F2=G2
ELSE
   F1=G2
   F2=G1
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTPV1(M,N,L,IU,IX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (IU.EQ.1) THEN
   I=M
   J=N
   K=L
ELSEIF (IU.EQ.2) THEN
   J=M
   K=N
   I=L
ELSE
   K=M
   I=N
   J=L
ENDIF
IF (IBCELL(I,J,K).GE.1.AND.IBCELL(I,J,K).NE.4) RETURN
!Stay in routine if cell is open or a melt surface
CALL INTP1(F1,F2,I,J,K,IX)
IF (IX.EQ.1) THEN
   UG(I,J,K,IU)=UG(I+1,J,K,IU)*F1+UG(I-1,J,K,IU)*F2
ELSEIF (IX.EQ.2) THEN
   IF (IBCELL(I,J+1,K).EQ.2.AND.IBCELL(I,J,K).EQ.0) THEN
      F1=0
      F2=1
   ELSEIF (IBCELL(I,J-1,K).EQ.2.AND.IBCELL(I,J,K).EQ.0) THEN
      F1=1
      F2=0
   ENDIF
   UG(I,J,K,IU)=UG(I,J+1,K,IU)*F1+UG(I,J-1,K,IU)*F2
ELSE
   IF (IBCELL(I,J,K+1).EQ.2.AND.IBCELL(I,J,K).EQ.0) THEN
      F1=0
      F2=1
   ELSEIF (IBCELL(I,J,K-1).EQ.2.AND.IBCELL(I,J,K).EQ.0) THEN
      F1=1
      F2=0
   ENDIF
   UG(I,J,K,IU)=UG(I,J,K+1,IU)*F1+UG(I,J,K-1,IU)*F2
ENDIF
CALL BOUND(UG(I,J,K,IU),IU,1)
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTPSP
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DO I=2,MP,2
   DO K=3,LPM1,2
      K1=K+1
      K2=K-1
      !CSL            CALL INTP1(F1,F2,I,J,K,3)
      DO J=2,NP,2
         CALL INTPSP1(SP(I,J,K),SP(I,J,K1),SP(I,J,K2),IBCELL(I,J,K),3)
      enddo
   enddo

   DO J=3,NPM1,2
   J1=J+1
   J2=J-1
   !CSL            CALL INTP1(F1,F2,I,J,K,2)
      DO K=2,LP
         CALL INTPSP1(SP(I,J,K),SP(I,J1,K),SP(I,J2,K),IBCELL(I,J,K),2)
      enddo
   enddo
enddo

DO I=3,MPM1,2
   I1=I+1
   I2=I-1
   !CSL         CALL INTP1(F1,F2,I,J,K,1)
   DO J=2,NP
   DO K=2,LP
      CALL INTPSP1(SP(I,J,K),SP(I1,J,K),SP(I2,J,K),IBCELL(I,J,K),1)
   enddo;enddo
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTPSP1(SP1,SP1P,SP1M,IBC0,IU)
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
integer(2) IBC0
IF (SP1P.LE.0.0D+0.AND.SP1M.LE.0.0D+0) THEN
   SP1=0.0D+0
   RETURN
ENDIF
IF (IBC0.LE.0) THEN
   IF (IU.EQ.1.AND.SP1P.GT.0.0D+0.AND.SP1M.LE.0.0D+0) THEN
      SP1=SP1P
   ELSE
      SP1=(SP1P+SP1M)*0.5D+0
   ENDIF
   !         THETA(I,J,K)=F1*THETA(I1,J,K)+F2*THETA(I2,J,K)
ELSEIF (IBC0.EQ.1.OR.IBC0.EQ.2) THEN
   SP1=MAX(SP1P,SP1M)
ELSEIF (IBC0.EQ.3) THEN
   SP1=MAX(SP1P,SP1M)
   SP1=SP1*0.5D+0
ELSE
   SP1=0.0D+0
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Interpolation of sub-species variables GFM
!======================================================================
subroutine intpm
use gbl_var
implicit double precision (a-h,o-z)
!---------------------------------------------------------------------
!     interior nodes
!---------------------------------------------------------------------
do i=2,mp,2
   do k=3,lpm1,2
      kp1=k+1
      km1=k-1
      call intp1(f1,f2,i,j,k,3) !determine f1 & f2
      do j=2,np,2
         if (ibcell(i,j,k).eq.1) cycle
         call intp0m(f1,i,j,k,i,j,kp1,i,j,km1)
      enddo
   enddo

   do j=3,npm1,2
      jp1=j+1
      jm1=j-1
      call intp1(f1,f2,i,j,k,2)
      do k=2,lp
         if (ibcell(i,j,k).eq.1) cycle
         call intp0m(f1,i,j,k,i,jp1,k,i,jm1,k)
      enddo
   enddo
enddo

do i=3,mpm1,2
   ip1=i+1
   im1=i-1
   call intp1(f1,f2,i,j,k,1)
   do k=2,lp
   do j=2,np
      if (ibcell(i,j,k).eq.1) cycle
      call intp0m(f1,i,j,k,ip1,j,k,im1,j,k)
   enddo;enddo
enddo
return
end


!======================================================================
!======================================================================
!======================================================================
!     Sub routine for interpolation of sub-species variables GFM
!======================================================================
subroutine intp0m(f1,i,j,k,i1,j1,k1,i2,j2,k2)
use gbl_var
implicit double precision (a-h,o-z)

if (ibcell(i,j,k).eq.3) return
if (ibcell(i,j,k).eq.2) then
   if (ibcell(i1,j1,k1).eq.2.and.ibcell(i2,j2,k2).eq.0) then
      f1=one
   elseif (ibcell(i1,j1,k1).eq.0.and.ibcell(i2,j2,k2).eq.2) then
      f1=zero
   endif
endif
f2=one-f1

do L=lstarm,lendm
   gfm(i,j,k,L)=f1*gfm(i1,j1,k1,L)+f2*gfm(i2,j2,k2,L)
enddo

return
end
