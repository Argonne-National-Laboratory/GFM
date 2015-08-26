!==============================================================================
!==============================================================================
!==============================================================================
! Qrtx.f90
!
!     X FLUXES IN +/- Y SWEEPS FOR EACH Z SLICE
!
!     i0,j0,k0 are the indexes of the current cell
!     i10 is i1-2 if going in the positive direction, 
!            i1+2 if going in the negative direction
!     i1 goes from the slab next to the current cell to the end slab
!
!     Depends on the ang0() values set in calling routine Qref or Qrew
!==============================================================================
SUBROUTINE QRTX(I0,J0,K0,I10,I1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION A1(3),Q11(NWL),Q12(NWL)
IDX=1
I2=I1+(I1-I10)/2 !i2 is i1+1 or i1-1
I2=MIN(I2,MP)
!CALL SANG2(IDX,I0,J0,K0,I2,J0,K0)
!Sang2 is expanded here with idx=1
DO JM=-1,1,2
   !This do loop indicates going backwards and then forwards from the current cell
   !CALL SWEEP(2,JM,JEND)
   if (jm.lt.0) then; jend=2
                else; jend=np; endif
   DO JJ=J0+JM,JEND,JM*2
      !This do loop goes from the current cell (either backwards or forwards)
      !in the y direction to the end wall
      !setting local a1() and global ang2(jj,2) angles

      !CALL SFAC(A1,I0,J0,K0,i2,JJ,k0,IDX) !expanded here
      !set a1
      a1(1)=0
      b=abs(x(i2)-x(i0))
      if (b.lt.small16) then
         a1(2)=90
         a1(3)=90
      else
         h2=abs(r(jj)-r(j0))/b
         a1(2)=atand(h2)
         !h3=abs(z(k0)-z(k0))/b
         !a1(3)=atand(h3)
         a1(3)=0
      endif
      !end SFAC expansion
      IF (A1(2).GE.ANG0(2)) exit
      ANG2(JJ,2)=A1(2)
   ENDDO
enddo

DO KM=-1,1,2
   !This do loop indicates going backwards and then forwards from the current cell
   !CALL SWEEP(3,KM,KEND)
   if (km.lt.0) then; kend=2
                else; kend=lp; endif
   DO KK=K0+KM,KEND,KM*2
      !This do loop goes from the current cell (either backwards or forwards)
      !in the z direction to the end wall
      !setting local a1() and global ang3(kk,2) angles

      !CALL SFAC(A1,I0,J0,K0,i2,j0,KK,IDX) !expanded here
      a1(1)=0
      b=abs(x(i2)-x(i0))
      if (b.lt.small16) then
         a1(2)=90
         a1(3)=90
      else
         !h2=abs(r(j0)-r(j0))/b
         !a1(2)=atand(h2)
         a1(2)=0
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
   !Loop through the interior space in the z direction 
   IF (K1.LT.K0.AND.ANG3(K1+1,2).GE.ANG0(3)) CYCLE
   IF (K1.GT.K0.AND.ANG3(K1-1,2).GE.ANG0(3)) EXIT
   KD2=K1/2
   DO JPM=-2,2,4
      !loop negative, then positive 
      A11=0
      A12=0
      !CALL SWEEP(2,JPM,JEND)
      if (jpm.lt.0) then; jend=2
                    else; jend=np; endif

      DO J1=J0,JEND-JPM,JPM
         !Begin at the current cell and loop thru the interior space in the j direction
         IF (A11.GE.ANG0(2)) exit
         J12=J1+JPM/2 !j12 = j1-1 or j1+1
         A12=ANG2(J12,1)
         DANG=A12-A11
         IF (DANG.LE.1.0D-10) then
            a11=a12
            cycle
         endif
         A21=A11
         J1D2=J1/2
         IF (IQT1(J1D2,KD2)) then
            a11=a12
            cycle
         endif
         IF (J1.EQ.J0) THEN
            Q11(1:NWL)=QTX1(J1D2,KD2,1:NWL)*HALF
            IF (JPM.GT.0) QTX1(J1D2,KD2,1:NWL)=zero
         ELSE
            Q11(1:NWL)=QTX1(J1D2,KD2,1:NWL)
            QTX1(J1D2,KD2,1:NWL)=zero
         ENDIF
         IF (J1.NE.J0.OR.JPM.GT.0) IQT1(J1D2,KD2)=.TRUE.
         Q12=Q11
         DO J2=J1,JEND,JPM
            J2D2=J2/2
            IF (IBCELL(I1,J2,K1).lt.1) then
               J2M=J2+JPM/2
               A22=ANG2(J2M,2)
               !csl           CALL VANG(A11,A12,A21,A22)
               IF (A22.LE.A21) CYCLE
               IF (A22.LT.A12) THEN
                  FIJ=(A22-A21)/DANG
                  DO L=1,NWL
                     IF (Q11(L).LT.small16) CYCLE
                     Q21=Q11(L)*FIJ
                     Q2M(J2D2,KD2,L)=Q2M(J2D2,KD2,L)+Q21
                     Q12(L)=Q12(L)-Q21
                  ENDDO
                  IQTM(J2D2,KD2)=.FALSE.
                  A21=A22
                  CYCLE
               ENDIF
            endif
            Q2M(J2D2,KD2,1:NWL)=Q2M(J2D2,KD2,1:NWL)+Q12
            IQTM(J2D2,KD2)=.FALSE.
            EXIT
         enddo !j2=
         A11=A12
      enddo !j1=
   enddo !jpm=
enddo !k1=


DO J1=2,NP,2  
   IF (J1.LT.J0.AND.ANG2(J1+1,2).GE.ANG0(2)) CYCLE
   IF (J1.GT.J0.AND.ANG2(J1-1,2).GE.ANG0(2)) EXIT
   JD2=J1/2
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
         IF (IQTM(JD2,KD2)) then
            a11=a12
            cycle
         endif
         IF (K1.EQ.K0) THEN
            Q11(1:NWL)=Q2M(JD2,KD2,1:NWL)*HALF
            IF (KM.GT.0) Q2M(JD2,KD2,1:NWL)=zero
         ELSE
            Q11(1:NWL)=Q2M(JD2,KD2,1:NWL)
            Q2M(JD2,KD2,1:NWL)=zero
         ENDIF
         IF (K1.NE.K0.OR.KM.GT.0) IQTM(JD2,KD2)=.TRUE.
         IBC1=IBCELL(I1,J1,K1)
         IF (IBC1.EQ.101.OR.IBC1.EQ.111.OR.IBC1.EQ.4) THEN
            QTX2(JD2,KD2,1:NWL)=QTX2(JD2,KD2,1:NWL)+Q11
            IQT2(JD2,KD2)=.FALSE.
            a11=a12
            cycle
         ENDIF
         Q12=Q11
         DO K2=K1,KEND,KM
            K2M=K2+KM/2
            K2D2=K2/2
            IBC2=IBCELL(I1,J1,K2)
            IF (IBC1.LE.0.AND.IBC2.GE.1) GOTO 330
            IF (IBC1.GT.100.AND.IBC2.EQ.1) THEN
               IF (J1.LT.J0) THEN
                  J2=J1+2
               ELSEIF (J1.GT.J0) THEN
                  J2=J1-2
               ENDIF
               J2D2=J2/2
               K2D2M=K2D2-KM/2
               DO L=1,NWL
                  IF (Q12(L).LT.small16) CYCLE
                  Q21=Q12(L)*HALF
                  QTX2(J2D2,K2D2,L)=QTX2(J2D2,K2D2,L)+Q21
                  QTX2(JD2,K2D2M,L)=QTX2(JD2,K2D2M,L)+Q21
               ENDDO
               IQT2(J2D2,K2D2)=.FALSE.
               IQT2(JD2,K2D2M)=.FALSE.
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
                  QTX2(JD2,K2D2,L)=QTX2(JD2,K2D2,L)+Q21
                  Q12(L)=Q12(L)-Q21
               ENDDO
               IQT2(JD2,K2D2)=.FALSE.
               A21=A22
               CYCLE
            ENDIF

            330 continue

            QTX2(JD2,K2D2,1:NWL)=QTX2(JD2,K2D2,1:NWL)+Q12
            IQT2(JD2,K2D2)=.FALSE.
            EXIT
         enddo !k2
         A11=A12
      enddo !k1
   enddo !km=
enddo !j1=
RETURN
END


!================================================================
!================================================================
!================================================================
!
!   9/20/05
!     Sang2 has been expanded in place and is no longer called
!
!================================================================
SUBROUTINE SANG2(IDX,I0,J0,K0,I1,J1,K1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION A1(3)

IF (IDX.ne.1) then
   DO IM=-1,1,2
      !CALL SWEEP(1,IM,IEND)
      if (im.lt.0) then; iend=2
                   else; iend=mp; endif
      DO II=I0+IM,IEND,IM*2
         !CALL SFAC(A1,I0,J0,K0,II,J1,K1,IDX) !expanded here
         !set a1
         a1(idx)=0
         select case (idx) !know idx is not 1
         case (2)
            b=abs(r(j1)-r(j0))
            if (b.lt.small16) then
               a1(1)=90
               a1(3)=90
            else
               h1=abs(x(ii)-x(i0))/b
               a1(1)=atand(h1)
               h3=abs(z(k1)-z(k0))/b
               a1(3)=atand(h3)
            endif
         case (3)
            b=abs(z(k1)-z(k0))
            if (b.lt.small16) then
               a1(1)=90
               a1(2)=90
            else
               h1=abs(x(ii)-x(i0))/b
               a1(1)=atand(h1)
               h2=abs(r(j1)-r(j0))/b
               a1(2)=atand(h2)
            endif
         end select
         !end SFAC expansion
         IF (A1(1).GE.ANG0(1)) exit
         ANG1(II,2)=A1(1)
      ENDDO
   enddo
endif

IF (IDX.ne.2) then
   DO JM=-1,1,2
      !CALL SWEEP(2,JM,JEND)
         if (jm.lt.0) then; jend=2
                      else; jend=np; endif
      DO JJ=J0+JM,JEND,JM*2
         !CALL SFAC(A1,I0,J0,K0,I1,JJ,K1,IDX) !expanded here
         !set a1
         a1(idx)=0
         select case (idx) !know idx is not 2
         case (1)
            b=abs(x(i1)-x(i0))
            if (b.lt.small16) then
               a1(2)=90
               a1(3)=90
            else
               h2=abs(r(jj)-r(j0))/b
               a1(2)=atand(h2)
               h3=abs(z(k1)-z(k0))/b
               a1(3)=atand(h3)
            endif
         case (3)
            b=abs(z(k1)-z(k0))
            if (b.lt.small16) then
               a1(1)=90
               a1(2)=90
            else
               h1=abs(x(i1)-x(i0))/b
               a1(1)=atand(h1)
               h2=abs(r(jj)-r(j0))/b
               a1(2)=atand(h2)
            endif
         end select
         !end SFAC expansion
         IF (A1(2).GE.ANG0(2)) exit
         ANG2(JJ,2)=A1(2)
      ENDDO
   enddo
endif

IF (IDX.ne.3) then
   DO KM=-1,1,2
      !CALL SWEEP(3,KM,KEND)
      if (km.lt.0) then; kend=2
                   else; kend=lp; endif
      DO KK=K0+KM,KEND,KM*2
         !CALL SFAC(A1,I0,J0,K0,I1,J1,KK,IDX) !expanded here
         !set a1
         a1(idx)=0
         select case (idx) !know idx is not 3
         case (1)
            b=abs(x(i1)-x(i0))
            if (b.lt.small16) then
               a1(2)=90
               a1(3)=90
            else
               h2=abs(r(j1)-r(j0))/b
               a1(2)=atand(h2)
               h3=abs(z(kk)-z(k0))/b
               a1(3)=atand(h3)
            endif
         case (2)
            b=abs(r(j1)-r(j0))
            if (b.lt.small16) then
               a1(1)=90
               a1(3)=90
            else
               h1=abs(x(i1)-x(i0))/b
               a1(1)=atand(h1)
               h3=abs(z(kk)-z(k0))/b
               a1(3)=atand(h3)
            endif
         end select
         !end SFAC expansion
         IF (A1(3).GE.ANG0(3)) exit
         ANG3(KK,2)=A1(3)
      ENDDO
   enddo
endif
return
end


!CSLC---
!CSL      SUBROUTINE VANG(A11,A12,A21,A22)
!CSL      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!CSL  IF (A21.LT.A11) THEN
!CSL     A21=A11
!CSL  ENDIF
!CSL  IF (A22.GT.A12) THEN
!CSL     A22=A12
!CSL  ENDIF
!CSL  IF (A22.LT.A21) THEN
!CSL         A22=A21
!CSL  ENDIF
!CSL      RETURN
!CSL      END
