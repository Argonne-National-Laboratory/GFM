!======================================================================
! INTP.F90
!     Interpolation of calculated variables
!       10/97
!
!        General Properties: PG,TG,DNST,GF,THETA
!        Boundary Conditions: linear (PG), zero slope (TG,GF,THETA)
!           F1,F2: linear coefficients
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
         IBC0=MOD(IBCELL(I,J,K),10)
         !CZEB        IF (IBC0.EQ.1) cycle
         IF (IBC0.EQ.1.OR.IBC0.EQ.5.OR.IBC0.EQ.6) cycle
         CALL INTP0(F1,I,J,K,I,J,KP1,I,J,KM1)
      enddo
   enddo

   DO J=3,NPM1,2
      JP1=J+1
      JM1=J-1
      CALL INTP1(F1,F2,I,J,K,2)
      DO K=2,LP
         IBC0=MOD(IBCELL(I,J,K),10)
         !CZEB        IF (IBC0.EQ.1) cycle
         IF (IBC0.EQ.1.OR.IBC0.EQ.5.OR.IBC0.EQ.6) cycle
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
      IBC0=MOD(IBCELL(I,J,K),10)
      !CZEB        IF (IBC0.EQ.1) cycle
      IF (IBC0.EQ.1.OR.IBC0.EQ.5.OR.IBC0.EQ.6) cycle
      CALL INTP0(F1,I,J,K,IP1,J,K,IM1,J,K)
   enddo;enddo
enddo
RETURN
END

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
      !IF (NP.LE.6) GOTO 200
      M3=NP
      N3=LP
      L3=MP
   ELSE
      !IF (LP.LE.6) GOTO 200
      M3=LP
      N3=MP
      L3=NP
   ENDIF

   DO M=3,M3-1,2
      if (n3>3) then
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
      endif

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
      enddo;;enddo
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
IBC0=MOD(IBCELL(I,J,K),10)
if (ibc0==2) then
   IBC1=MOD(IBCELL(I1,J1,K1),10)
   IBC2=MOD(IBCELL(I2,J2,K2),10)
   IF (IBC1.EQ.2.AND.IBC2.LE.0) THEN
      F1=ONE
   ELSEIF (IBC1.LE.0.AND.IBC2.EQ.2) THEN
      F1=ZERO
   ENDIF
endif

F2=ONE-F1
LG(I,J,K)%TH=F1*LG(I1,J1,K1)%TH+F2*LG(I2,J2,K2)%TH
LG(I,J,K)%P=F1*LG(I1,J1,K1)%P+F2*LG(I2,J2,K2)%P
LG(I,J,K)%T=max(F1*LG(I1,J1,K1)%T+F2*LG(I2,J2,K2)%T,Tmltr)
!CALL ENTH(I,J,K,1) Lottes 5/11/05 h should be calculated after c in ther.
CALL THER(I,J,K)
!Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
Tlg_to_h(Lg(i,j,k)%T,Lg(i,j,k)%h) !Lottes 5/11/05 set h consistent with T and c.
!CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
CALL DSLG(I,J,K)
DO L=LSTAR,LEND
   LG(I,J,K)%F(L)=F1*LG(I1,J1,K1)%F(L)+F2*LG(I2,J2,K2)%F(L)
ENDDO
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
   IF (dy(J).LE.ZERO.OR.J.GT.NP) THEN
      F1=ZERO
   ELSE
      F1=(y(J)-y(J-1))/dy(J)
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
IBC0=MOD(IBCELL(I,J,K),10)
IF (IBC0.GE.1) RETURN
CALL INTP1(F1,F2,I,J,K,IX)
IF (IX.EQ.1) THEN
   LG(I,J,K)%U(IU)=LG(I+1,J,K)%U(IU)*F1+LG(I-1,J,K)%U(IU)*F2
ELSEIF (IX.EQ.2) THEN
   IBC1=MOD(IBCELL(I,J+1,K),10)
   IBC2=MOD(IBCELL(I,J-1,K),10)
   IF (IBC1.EQ.2.AND.IBC0.EQ.0) THEN
      F1=0
      F2=1
   ELSEIF (IBC2.EQ.2.AND.IBC0.EQ.0) THEN
      F1=1
      F2=0
   ENDIF
   LG(I,J,K)%U(IU)=LG(I,J+1,K)%U(IU)*F1+LG(I,J-1,K)%U(IU)*F2
ELSE
   IBC1=MOD(IBCELL(I,J,K+1),10)
   IBC2=MOD(IBCELL(I,J,K-1),10)
   IF (IBC1.EQ.2.AND.IBC0.EQ.0) THEN
      F1=0
      F2=1
   ELSEIF (IBC2.EQ.2.AND.IBC0.EQ.0) THEN
      F1=1
      F2=0
   ENDIF
   LG(I,J,K)%U(IU)=LG(I,J,K+1)%U(IU)*F1+LG(I,J,K-1)%U(IU)*F2
ENDIF
CALL BOUND(LG(I,J,K)%U(IU),IU,1)
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
         CALL INTPSP1(I,J,K,3)
      enddo
   enddo

   DO J=3,NPM1,2
      J1=J+1
      J2=J-1
      !CSL            CALL INTP1(F1,F2,I,J,K,2)
      DO K=2,LP
         CALL INTPSP1(I,J,K,2)
      enddo
   enddo
enddo

DO I=3,MPM1,2
   I1=I+1
   I2=I-1
   !CSL         CALL INTP1(F1,F2,I,J,K,1)
   DO J=2,NP
   DO K=2,LP
      CALL INTPSP1(I,J,K,1)
   enddo;enddo
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTPSP1(I,J,K,IU)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
SP1=ZERO
IF (IU.EQ.1.AND.DX(I).GT.SMALL) THEN
   SP1P=SP(I+1,J,K)
   SP1M=SP(I-1,J,K)
   G0=(X(I+1)-X(I))/DX(I)
ELSEIF (IU.EQ.2.AND.dy(J).GT.SMALL) THEN
   SP1P=SP(I,J+1,K)
   SP1M=SP(I,J-1,K)
   G0=(y(J+1)-y(J))/dy(J)
ELSEIF (IU.EQ.3.AND.DZ(K).GT.SMALL) THEN
   SP1P=SP(I,J,K+1)
   SP1M=SP(I,J,K-1)
   G0=(Z(K+1)-Z(K))/DZ(K)
ELSE
   SP(I,J,K)=SP1
   RETURN
ENDIF

IF (SP1P.LE.ZERO.AND.SP1M.LE.ZERO) then
   SP(I,J,K)=SP1
   RETURN
endif

IBC0=MOD(IBCELL(I,J,K),10)
IF (IBC0.LE.0) THEN
   !        IF (IU.EQ.1.AND.SP1P.GT.ZERO.AND.SP1M.LE.ZERO) THEN
   !           SP1=SP1P
   !        ELSE
      G1=ONE-G0
      SP1=G1*SP1P+G0*SP1M
   !        ENDIF
ELSEIF (IBC0.EQ.1.OR.IBC0.EQ.2.OR.IBC0.EQ.4) THEN
   SP1=MAX(SP1P,SP1M)
ENDIF
SP(I,J,K)=SP1

RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE INTP_minor_species
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!---------------------------------------------------------------------

!interpolation for minor species needs to be added here


return
end