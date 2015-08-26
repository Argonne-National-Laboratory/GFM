!==============================================================================
!==============================================================================
!==============================================================================
! Qrty.f90
!
!     Y FLUXES IN +/- ? SWEEPS FOR EACH ? SLICE
!
!==============================================================================
SUBROUTINE QRTY(I0,J0,K0,J10,J1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION A1(3),Q11(NWL),Q12(NWL)
IDX=2
J2=J1+(J1-J10)/2
J2=MIN(J2,NP)
!CALL SANG2(IDX,I0,J0,K0,I0,J2,K0)
!Sang2 is expanded here with idx=2
DO IM=-1,1,2
   !CALL SWEEP(1,IM,IEND)
   if (im.lt.0) then; iend=2
                else; iend=mp; endif
   DO II=I0+IM,IEND,IM*2
      !CALL SFAC(A1,I0,J0,K0,II,j2,k0,IDX) !expanded here
      !set a1
      a1(2)=0
      b=abs(r(j2)-r(j0))
      if (b.lt.small16) then
         a1(1)=90
         a1(3)=90
      else
         h1=abs(x(ii)-x(i0))/b
         a1(1)=atand(h1)
         h3=abs(z(k0)-z(k0))/b
         a1(3)=atand(h3)
      endif
      !end SFAC expansion
      IF (A1(1).GE.ANG0(1)) exit
      ANG1(II,2)=A1(1)
   ENDDO
enddo

DO KM=-1,1,2
   !CALL SWEEP(3,KM,KEND)
   if (km.lt.0) then; kend=2
                else; kend=lp; endif
   DO KK=K0+KM,KEND,KM*2
      !CALL SFAC(A1,I0,J0,K0,i0,j2,KK,IDX) !expanded here
      !set a1
      a1(2)=0
      b=abs(r(j2)-r(j0))
      if (b.lt.small16) then
         a1(1)=90
         a1(3)=90
      else
         h1=abs(x(i0)-x(i0))/b
         a1(1)=atand(h1)
         h3=abs(z(kk)-z(k0))/b
         a1(3)=atand(h3)
      endif
      !end SFAC expansion
      IF (A1(3).GE.ANG0(3)) exit
      ANG3(KK,2)=A1(3)
   ENDDO
enddo
!end of SANG2 expansion

DO K1=4,LP-2,2
   IF (K1.LT.K0.AND.ANG3(K1+1,2).GE.ANG0(3)) CYCLE
   IF (K1.GT.K0.AND.ANG3(K1-1,2).GE.ANG0(3)) EXIT
   KD2=K1/2
   DO IPM=-2,2,4
      A11=0
      A12=0
      !CALL SWEEP(1,IPM,IEND)
      if (ipm.lt.0) then; iend=2
                    else; iend=mp; endif
      DO I1=I0,IEND-IPM,IPM
         IF (A11.GE.ANG0(1)) exit
         I12=I1+IPM/2
         A12=ANG1(I12,1)
         DANG=A12-A11
         IF (DANG.LE.1.0D-10) then
            a11=a12
            cycle
         endif
         A21=A11
         I1D2=I1/2
         IF (IQT1(I1D2,KD2)) then
            a11=a12
            cycle
         endif
         IF (I1.EQ.I0) THEN
            Q11(1:NWL)=QTX1(I1D2,KD2,1:NWL)*HALF
            IF (IPM.GT.0) QTX1(I1D2,KD2,1:NWL)=zero
         ELSE
            Q11(1:NWL)=QTX1(I1D2,KD2,1:NWL)
            QTX1(I1D2,KD2,1:NWL)=zero
         ENDIF
         IF (I1.NE.I0.OR.IPM.GT.0) IQT1(I1D2,KD2)=.TRUE.
         Q12=Q11
         DO I2=I1,IEND,IPM
            I2D2=I2/2
            IF (IBCELL(I2,J1,K1).lt.1) then
               I2M=I2+IPM/2
               A22=ANG1(I2M,2)
               !csl           CALL VANG(A11,A12,A21,A22)
               IF (A22.LE.A21) CYCLE
               IF (A22.LT.A12) THEN
                  FIJ=(A22-A21)/DANG
                  DO L=1,NWL
                     IF (Q11(L).LT.small16) CYCLE
                     Q21=Q11(L)*FIJ
                     Q2M(I2D2,KD2,L)=Q2M(I2D2,KD2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQTM(I2D2,KD2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
            endif
            Q2M(I2D2,KD2,1:NWL)=Q2M(I2D2,KD2,1:NWL)+Q12
            IQTM(I2D2,KD2)=.FALSE.
            EXIT
         enddo !i2=
         A11=A12
      enddo !i1=
   enddo !ipm=
enddo !k1=


DO I1=2,MP,2
   IF (I1.LT.I0.AND.ANG1(I1+1,2).GE.ANG0(1)) CYCLE
   IF (I1.GT.I0.AND.ANG1(I1-1,2).GE.ANG0(1)) EXIT
   ID2=I1/2
   DO KM=-2,2,4
      A11=0
      A12=0
      !CALL SWEEP(3,KM,KEND)
      if (km.lt.0) then; kend=2
                   else; kend=lp; endif

      DO K1=K0,KEND-KM,KM
         IF (A11.GE.ANG0(3)) EXIT
         K12=K1+KM/2
         A12=ANG3(K12,1)
         DANG=A12-A11
         IF (DANG.LE.1.0D-10) then
            a11=a12
            cycle
         endif
         A21=A11
         KD2=K1/2
         IF (IQTM(ID2,KD2)) then
            a11=a12
            cycle
         endif
         IF (K1.EQ.K0) THEN
            Q11(1:NWL)=Q2M(ID2,KD2,1:NWL)*HALF
            IF (KM.GT.0) Q2M(ID2,KD2,1:NWL)=zero
         ELSE
            Q11(1:NWL)=Q2M(ID2,KD2,1:NWL)
            Q2M(ID2,KD2,1:NWL)=zero
         ENDIF
         IF (K1.NE.K0.OR.KM.GT.0) IQTM(ID2,KD2)=.TRUE.
         IBC1=IBCELL(I1,J1,K1)
         IF (IBC1.EQ.201.OR.IBC1.EQ.211.OR.IBC1.EQ.4) THEN
            QTX2(ID2,KD2,1:NWL)=QTX2(ID2,KD2,1:NWL)+Q11
            IQT2(ID2,KD2)=.FALSE.
            a11=a12
            cycle
         ENDIF
         Q12=Q11
         DO K2=K1,KEND,KM
            K2M=K2+KM/2
            K2D2=K2/2
            IBC2=IBCELL(I1,J1,K2)
            IF (IBC1.LE.0.AND.IBC2.GE.1) GOTO 330
            IF (IBC1.GE.100.AND.IBC2.EQ.1) THEN
               IF (I1.LT.I0) THEN
                  I2=I1+2
               ELSEIF (I1.GT.I0) THEN
                  I2=I1-2
               ENDIF
               I2D2=I2/2
               K2D2M=K2D2-KM/2
               DO L=1,NWL
                  IF (Q12(L).LT.small16) CYCLE
                  Q21=Q12(L)*HALF
                  QTX2(I2D2,K2D2,L)=QTX2(I2D2,K2D2,L)+Q21
                  QTX2(ID2,K2D2M,L)=QTX2(ID2,K2D2M,L)+Q21
               ENDDO
               IQT2(I2D2,K2D2)=.FALSE.
               IQT2(ID2,K2D2M)=.FALSE.
               exit
            ENDIF
            IF (K2M.GT.LP.OR.K2M.LT.2) GOTO 330
            A22=ANG3(K2M,2)
            !csl           CALL VANG(A11,A12,A21,A22)
            IF ((A22-A21).LE.1.0D-10) cycle
            IF (A22.LT.A12) THEN
               FIJ=(A22-A21)/DANG
               DO L=1,NWL
                  IF (Q11(L).LT.small16) CYCLE
                  Q21=Q11(L)*FIJ
                  QTX2(ID2,K2D2,L)=QTX2(ID2,K2D2,L)+Q21
                  Q12(L)=Q12(L)-Q21
               ENDDO
               IQT2(ID2,K2D2)=.FALSE.
               A21=A22
               CYCLE
            ENDIF

            330 continue

            QTX2(ID2,K2D2,1:NWL)=QTX2(ID2,K2D2,1:NWL)+Q12
            IQT2(ID2,K2D2)=.FALSE.
            EXIT
         enddo !k2=
         a11=a12
      enddo !k1=
   enddo !km=
enddo !i1=
RETURN
END
