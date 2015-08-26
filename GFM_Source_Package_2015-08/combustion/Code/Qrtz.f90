!==============================================================================
!==============================================================================
!==============================================================================
! Qrtz.f90
!
!     Z FLUXES IN +/- ? SWEEPS FOR EACH ? SLICE
!
!==============================================================================
!---
!     K0: emission point
!     K10: previous k slab in calling routine loop
!     K1: current k slab
!     rev: 9/00
!---
SUBROUTINE QRTZ(I0,J0,K0,K10,K1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION A1(3),Q11(NWL),Q12(NWL)
IDX=3
K2=K1+(K1-K10)/2
K2=MIN(K2,LP)
!CALL SANG2(IDX,I0,J0,K0,I0,J0,K2)
!Sang2 expanded here with idx=3
DO IM=-1,1,2
   !CALL SWEEP(1,IM,IEND)
   if (im.lt.0) then; iend=2
                else; iend=mp; endif
   DO II=I0+IM,IEND,IM*2
      !CALL SFAC(A1,I0,J0,K0,II,j0,k2,IDX) !expanded here
      !set a1
      a1(3)=0
      b=abs(z(k2)-z(k0))
      if (b.lt.small16) then
         a1(1)=90
         a1(2)=90
      else
         h1=abs(x(ii)-x(i0))/b
         a1(1)=atand(h1)
         h2=abs(r(j0)-r(j0))/b
         a1(2)=atand(h2)
      endif
      !end SFAC expansion
      IF (A1(1).GE.ANG0(1)) exit
      ANG1(II,2)=A1(1)
   ENDDO
enddo

DO JM=-1,1,2
   !CALL SWEEP(2,JM,JEND)
      if (jm.lt.0) then; jend=2
                   else; jend=np; endif
   DO JJ=J0+JM,JEND,JM*2
      !CALL SFAC(A1,I0,J0,K0,i0,JJ,k2,IDX) !expanded here
      !set a1
      a1(3)=0
      b=abs(z(k2)-z(k0))
      if (b.lt.small16) then
         a1(1)=90
         a1(2)=90
      else
         h1=abs(x(i0)-x(i0))/b
         a1(1)=atand(h1)
         h2=abs(r(jj)-r(j0))/b
         a1(2)=atand(h2)
      endif
      !end SFAC expansion
      IF (A1(2).GE.ANG0(2)) exit
      ANG2(JJ,2)=A1(2)
   ENDDO
enddo
!end of Sang2 expansion





DO J1=4,NP-2,2
   IF (J1.LT.J0.AND.ANG2(J1+1,2).GE.ANG0(2)) CYCLE
   IF (J1.GT.J0.AND.ANG2(J1-1,2).GE.ANG0(2)) EXIT
   JD2=J1/2
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
         IF (IQT1(I1D2,JD2)) then
            a11=a12
            cycle
         endif
         IF (I1.EQ.I0) THEN
            Q11(1:NWL)=QTX1(I1D2,JD2,1:NWL)*HALF
            IF (IPM.GT.0) QTX1(I1D2,JD2,1:NWL)=zero
         ELSE
            Q11(1:NWL)=QTX1(I1D2,JD2,1:NWL)
            QTX1(I1D2,JD2,1:NWL)=zero
         ENDIF
         IF (I1.NE.I0.OR.IPM.GT.0) IQT1(I1D2,JD2)=.TRUE.
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
                     Q2M(I2D2,JD2,L)=Q2M(I2D2,JD2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQTM(I2D2,JD2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
            endif
            Q2M(I2D2,JD2,1:NWL)=Q2M(I2D2,JD2,1:NWL)+Q12
            IQTM(I2D2,JD2)=.FALSE.
            EXIT
         enddo !i2=
         a11=a12
      enddo !i1=
   enddo !ipm=
enddo !ji=


DO I1=2,MP,2
   IF (I1.LT.I0.AND.ANG1(I1+1,2).GE.ANG0(1)) CYCLE
   IF (I1.GT.I0.AND.ANG1(I1-1,2).GE.ANG0(1)) EXIT
   ID2=I1/2
   DO JM=-2,2,4
      A11=0
      A12=0
      !CALL SWEEP(2,JM,JEND)
         if (jm.lt.0) then; jend=2
                      else; jend=np; endif
      DO J1=J0,JEND-JM,JM
         IF (A11.GE.ANG0(2)) EXIT
         J12=J1+JM/2
         A12=ANG2(J12,1)
         DANG=A12-A11
         IF (DANG.LE.1.0D-10) then
            a11=a12
            cycle
         endif
         A21=A11
         JD2=J1/2
         IF (IQTM(ID2,JD2)) then
            a11=a12
            cycle
         endif
         IF (J1.EQ.J0) THEN
            Q11(1:NWL)=Q2M(ID2,JD2,1:NWL)*HALF
            IF (JM.GT.0) Q2M(ID2,JD2,1:NWL)=zero
         ELSE
            Q11(1:NWL)=Q2M(ID2,JD2,1:NWL)
            Q2M(ID2,JD2,1:NWL)=zero
         ENDIF
         IF (J1.NE.J0.OR.JM.GT.0) IQTM(ID2,JD2)=.TRUE.
         IBC1=IBCELL(I1,J1,K1)
         IF (IBC1.EQ.301.OR.IBC1.EQ.311.OR.IBC1.EQ.4) THEN
            QTX2(ID2,JD2,1:NWL)=QTX2(ID2,JD2,1:NWL)+Q11
            IQT2(ID2,JD2)=.FALSE.
            a11=a12
            cycle
         ENDIF
         Q12=Q11
         DO J2=J1,JEND,JM
            J2M=J2+JM/2
            J2D2=J2/2
            IBC2=IBCELL(I1,J2,K1)
            IF (IBC1.LE.0.AND.IBC2.GE.1) GOTO 330
            IF (IBC1.GT.1.AND.IBC2.EQ.1) THEN ! what about melt surf ibcell=4 and extra wall markers
               IF (I1.LT.I0) THEN
                  I2=I1+2
               ELSEIF (I1.GT.I0) THEN
                  I2=I1-2
               ENDIF
               I2D2=I2/2
               J2D2M=J2D2-JM/2
               DO L=1,NWL
                  IF (Q12(L).LT.small16) CYCLE
                  Q21=Q12(L)*HALF
                  QTX2(I2D2,J2D2,L)=QTX2(I2D2,J2D2,L)+Q21
                  QTX2(ID2,J2D2M,L)=QTX2(ID2,J2D2M,L)+Q21
               ENDDO
               IQT2(I2D2,J2D2)=.FALSE.
               IQT2(ID2,J2D2M)=.FALSE.
               exit
            ENDIF
            IF (J2M.GT.NP.OR.J2M.LT.2) GOTO 330
            A22=ANG2(J2M,2)
            !csl           CALL VANG(A11,A12,A21,A22)
            IF ((A22-A21).LE.1.0D-10) cycle
            IF (A22.LT.A12) THEN
               FIJ=(A22-A21)/DANG
               DO L=1,NWL
                  IF (Q11(L).LT.small16) CYCLE
                  Q21=Q11(L)*FIJ
                  QTX2(ID2,J2D2,L)=QTX2(ID2,J2D2,L)+Q21
                  Q12(L)=Q12(L)-Q21
               ENDDO
               IQT2(ID2,J2D2)=.FALSE.
               A21=A22
               CYCLE
            ENDIF
   
            330 continue

            QTX2(ID2,J2D2,1:NWL)=QTX2(ID2,J2D2,1:NWL)+Q12
            IQT2(ID2,J2D2)=.FALSE.
            EXIT
         enddo !j2=
         a11=a12
      enddo !j1=
   enddo !jm=
enddo !ii=
return
end
