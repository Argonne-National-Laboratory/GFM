! GFLOWEB.F90
!     Calculates joule heating
!       revision: 6/02
!======================================================================
      SUBROUTINE GFLOWEB
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !CzEB----&-----------------------------------------------------------------
      !     GSOLVEEB calculates the el. bust variables
      !---------------------------------------------------------------------
      EBSG0=8.9D+1
      EBQ=ZERO
      EBSG=EBSG0      
      QEB0=ZERO
      DO 100 N=1,NEB0
      NPS=ZERO
      QEB0=QEB0+EB(N)%PWR
200      NPS=NPS+1
         IF (EB(N)%NTP.LE.2) THEN
             DO M=1,2
               I1=EB(N)%I1(M)
               I2=EB(N)%I2(M)
               J1=EB(N)%J1(M)
               J2=EB(N)%J2(M)
               K1=EB(N)%K1(M)
               K2=EB(N)%K2(M)
               DO 110 I=I1,I2
               DO 110 J=J1,J2
               DO 110 K=K1,K2
               IF (M.EQ.1) EBV(I,J,K)=EB(N)%VLT
               IF (M.EQ.2) EBV(I,J,K)=ZERO
110            ENDDO
            ENDDO
         CALL EXTREB
         DO K=1,3
            CALL GSLV3EB
         ENDDO
         ENDIF
!CZ----------3 PHASE
         IF (EB(N)%NTP.EQ.3) THEN
         IBCEB=IBCELL
             DO M=1,3
               I1=EB(N)%I1(M)
               I2=EB(N)%I2(M)
               J1=EB(N)%J1(M)
               J2=EB(N)%J2(M)
               K1=EB(N)%K1(M)
               K2=EB(N)%K2(M)
               DO 210 I=I1,I2
               DO 210 J=J1,J2
               DO 210 K=K1,K2
               IF (NPS.EQ.1) THEN
                  IF (M.EQ.1) EBV(I,J,K)=EB(N)%VLT
                  IF (M.EQ.2) EBV(I,J,K)=ZERO
                  IF (M.EQ.3) IBCELL(I,J,K)=1
               ENDIF
               IF (NPS.EQ.2) THEN
                  IF (M.EQ.1) EBV(I,J,K)=EB(N)%VLT
                  IF (M.EQ.3) EBV(I,J,K)=ZERO
                  IF (M.EQ.2) IBCELL(I,J,K)=1
               ENDIF
               IF (NPS.EQ.3) THEN
                  IF (M.EQ.2) EBV(I,J,K)=EB(N)%VLT
                  IF (M.EQ.3) EBV(I,J,K)=ZERO
                  IF (M.EQ.1) IBCELL(I,J,K)=1
               ENDIF
210            ENDDO
            ENDDO
         CALL EXTREB
         DO K=1,3
            CALL GSLV3EB
         ENDDO
         ENDIF
!--------------------------------------------------------------------
!     calculates derived variables
!     electric potential, el. current density, joule heating power 
!     density
!---------------------------------------------------------------------
         CALL EXTREB
         !CZ    GO TO 331
         ebsg1=421.80D+0
         T0EB=1400.0D+0
         DO 330 I=2,mp
         DO 330 J=2,NP
         DO 330 K=2,LP
            IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.EQ.1.OR.IBC0.EQ.5) GOTO 330 
            ebsg(i,j,k)=ebsg0-ebsg1*(1-LG(I,J,K)%T/t0eb)
            IF (EBSG(I,J,K).LT.ZERO) EBSG(I,J,K)=ZERO
330      CONTINUE
331      CONTINUE
         DO 340 I=4,mpm2,2
         DO 340 J=njy1,njy2,2
         DO 340 K=lkz1,lkz2,2
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.GE.1.AND.IBC0.NE.5) GOTO 340 
            ebe(i-1,j,k)=(ebv(i-2,j,k)-ebv(i,j,k))/(dx(i-1))
            ebe(i,j-1,k)=(ebv(i,j-2,k)-ebv(i,j,k))/(dy(j-1))
            ebe(i,j,k-1)=(ebv(i,j,k-2)-ebv(i,j,k))/(dz(k-1))
            ebj(i-1,j,k)=ebe(i-1,j,k)*ebsg(i-1,j,k)
            ebj(i,j-1,k)=ebe(i,j-1,k)*ebsg(i,j-1,k)
            ebj(i,j,k-1)=ebe(i,j,k-1)*ebsg(i,j,k-1)
            AREAX=dy(J)*DZ(K)
            AREAY=DX(I)*DZ(K)
            AREAZ=DX(I)*dy(J)
            VOLIM1=AREAX*DX(I-1)
            VOLJM1=AREAY*dy(J-1)
            VOLKM1=AREAZ*DZ(K-1)
            !CZ      IF (IBC0.EQ.5) THEN
            !CZ        AREAX=dy(J)*DZ(K)*0.5D+0
            !CZ        AREAY=DX(I)*DZ(K)*0.5D+0*0.5D+0
            !CZ        AREAZ=DX(I)*dy(J)*0.5D+0
            !CZ        VOLIM1=AREAX*DX(I-1)*0.5D+0
            !CZ          VOLJM1=AREAY*dy(J-1)
            !CZ          VOLKM1=AREAZ*DZ(K-1)*0.5D+0
            !CZ      ENDIF
            ebI(i-1,j,k)=ebJ(i-1,j,k)*AREAX
            ebI(i,j-1,k)=ebJ(i,j-1,k)*AREAY
            ebI(i,j,k-1)=ebJ(i,j,k-1)*AREAZ
            ebq(i-1,j,k)=ebe(i-1,j,k)*ebj(i-1,j,k)*VOLIM1
            ebq(i,j-1,k)=ebe(i,j-1,k)*ebj(i,j-1,k)*VOLJM1
            ebq(i,j,k-1)=ebe(i,j,k-1)*ebj(i,j,k-1)*VOLKM1
340      CONTINUE
         QEBTOTAL=ZERO
         DO 420 I=4,mpm2,2
         DO 420 J=njy1,njy2,2
         DO 420 K=lkz1,lkz2,2
            IBC0=MOD(IBCELL(I,J,K),10)
            !CZ      IF (IBC0.GE.1.AND.IBC0.NE.5) GOTO 420
            IF (IBC0.GE.1) GOTO 420
            FIM1=(x(i)-x(i-1))/dx(i-1)
            IBC5=MOD(IBCELL(I-1,J,K),10)
            IF (IBC5.EQ.5) FIM1=ONE
            FJM1=(y(j)-y(j-1))/dy(j-1)
            IBC5=MOD(IBCELL(I,J-1,K),10)
            IF (IBC5.EQ.5) FJM1=ONE
            FKM1=(z(k)-z(k-1))/dz(k-1)
            IBC5=MOD(IBCELL(I,J,K-1),10)
            IF (IBC5.EQ.5) FKM1=ONE
            FIP1=(x(i+1)-x(i))/dx(i+1)
            IBC5=MOD(IBCELL(I+1,J,K),10)
            IF (IBC5.EQ.5) FIP1=ONE
            FJP1=(y(j+1)-y(j))/dy(j+1)
            IBC5=MOD(IBCELL(I,J+1,K),10)
            IF (IBC5.EQ.5) FJP1=ONE
            FKP1=(z(k+1)-z(k))/dz(k+1)
            IBC5=MOD(IBCELL(I,J,K+1),10)
            IF (IBC5.EQ.5) FKP1=ONE
            AVG=1.0D+0
            IF (EB(N)%NTP.EQ.2) AVG=2.0D+0
            IF (EB(N)%NTP.EQ.3) AVG=3.0D+0
            ebq(i,j,k)=ebq(i,j,k)+(ebq(i-1,j,k)*FIM1+ebq(i,j-1,k)*FJM1+ebq(i,j,k-1)*FKM1 &
                      +ebq(i+1,j,k)*FIP1+ebq(i,j+1,k)*FJP1+ebq(i,j,k+1)*FKP1)/AVG
            !czCZ      ebq(i,j,k)=ebq(i,j,k)/(DX(I)*dy(J)*DZ(K))
            !CZ      IF (IDCAC.EQ.2) ebq(i,j,k)=EBQ(I,J,K)*0.5D+0
            QEBTOTAL=QEBTOTAL+EBQ(I,J,K)
420      CONTINUE
         ITER=ITER+1
         IF (EB(N)%NTP.EQ.3) THEN
           IBCELL=IBCEB
           IF (NPS.LE.2) GO TO 200
         ENDIF 
100   CONTINUE
      FACQEB=QEB0/QEBTOTAL
      QEBTOTAL=ZERO
      DO 430 I=4,mpm2,2
      DO 430 J=njy1,njy2,2
      DO 430 K=lkz1,lkz2,2
         ebq(i,j,k)=ebq(i,j,k)*FACQEB
         QEBTOTAL=QEBTOTAL+EBQ(I,J,K)
430   CONTINUE
      WRITE(NU,'("  Electric Booster Heat (W)",I5,F20.0)') ITER,QEBTOTAL
      RETURN
      END
