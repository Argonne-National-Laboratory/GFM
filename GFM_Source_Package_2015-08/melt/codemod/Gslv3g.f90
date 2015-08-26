! GSLV3G.F90
! Lottes 6/5/05
!
! This routine only solves the liquid glass energy equation
!
! It is called from gflow after the SIMPLER pressure-velocity solve
!
! This is a laminar, very low Reynolds number flow.
! Therefore, conduction can be important.
! Thermal conductivity is a function of temperature. 
!======================================================================
      SUBROUTINE GSLV3G
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      KD=2
      MI1=4
      MI3=MP_E
      NJ1=NJY1
      NJ3=NJY2
      LK1=LKZ1
      LK3=LKZ2
      DO 600 L=0,LEND 
         L1=L+6
         IF (L.EQ.0) THEN 
            NELG=6
            DO 510 I=2,MP
            DO 510 J=2,NP
            DO 510 K=2,LP
               FZ(I,J,K)=LG(I,J,K)%H
               !Lottes 5/10/05, Fix to energy equation, should not have density in denominator
               Lg(i,j,k)%a = Lg(i,j,k)%k / Lg(i,j,k)%c            
510         CONTINUE 
            sgm=one !divides diffusivity factor before gradient, can remove when all are one !@@@@ check 
         ELSE 
            NELG=99
            GOTO 600
      !Lottes 6/5/05: This is dead code. Untangle this! @@@@
            DO 530 I=2,MP,2
            DO 530 J=2,NP,2
            DO 530 K=2,LP,2
530         FZ(I,J,K)=LG(I,J,K)%F(L) 
            SGM=SIGMA(L)
      !end dead code
         ENDIF
         CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
         CALL GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,0)
555      DO 560 I=MI1,MI3,2
         ID2=I/2
         DO 560 J=NJ1,NJ3,2
         JD2=J/2
         DO 560 K=LK1,LK3,2
            IF (IBCELL(I,J,K).GE.1) GOTO 560
            KD2=K/2
            VOL=VOL_C(I,J,K)
            AP(I,J,K)=AP(I,J,K)-SFP(I,J,K)*VOL
            AP(I,J,K)=AP(I,J,K)+LG(I,J,K)%MR*VOL
            IF (NBS0.GE.1) AP(I,J,K)=AP(I,J,K)-CON(ID2,JD2,KD2)*VOL
            IF (ABS(AP(I,J,K)).LE.SMALL) THEN
               AP(I,J,K)=ONE
               BS(I,J,K)=FZ(I,J,K)
               GOTO 560
            ENDIF
            BS(I,J,K)=BS(I,J,K)*VOL
            IF (.NOT.STEADY) THEN 
               AP(I,J,K)=AP(I,J,K)+APO(I,J,K)*VOL
               IF (L.EQ.0) THEN 
                  G0=LG0(I,J,K)%H
               ELSE
                  G0=LG0(I,J,K)%F(L)
               ENDIF
               BS(I,J,K)=BS(I,J,K)+APO(I,J,K)*G0*VOL
            ENDIF
            AP(I,J,K)=AP(I,J,K)/RFL(L1)
            BS(I,J,K)=BS(I,J,K)+RFLC(L1)*FZ(I,J,K)*AP(I,J,K)
560      CONTINUE

         if (L==0 .and. igresidp==1) call calc_residual(fz,h_resid_pre,mi1,mi3,nj1,nj3,lk1,lk3)
         CALL ADLBL3(FZ,KD,MI1,MI3,NJ1,NJ3,LK1,LK3)
         DO 590 I=MI1,MI3,2
         DO 590 J=NJ1,NJ3,2
         DO 590 K=LK1,LK3,2
            IF (IBCELL(I,J,K).GE.1) CYCLE
            IF (L.EQ.0) THEN !L is always 0
               LG(I,J,K)%H=FZ(I,J,K)
            ELSE
               CALL BOUND(FZ(I,J,K),L,2)
               LG(I,J,K)%F(L)=FZ(I,J,K)
            ENDIF
590      CONTINUE


      if (L==0 .and. igresid==1) call calc_residual(fz,h_resid,mi1,mi3,nj1,nj3,lk1,lk3)
600   CONTINUE
      CALL FLX1D(2)
900   RETURN
      END


!======================================================================
!  flux coefficients AS
!======================================================================
      SUBROUTINE GSLV3G0(MI1,MI3,NJ1,NJ3,LK1,LK3,IGS)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DO 550 I=MI1,MI3,2
      DO 550 J=NJ1,NJ3,2
      DO 550 K=LK1,LK3,2
         IF (IBCELL(I,J,K).GE.1) GOTO 550
         AP(I,J,K)=ZERO
         DO 520 IU=1,3
         IF (IU.EQ.2.AND.NP.LE.6) GOTO 520
         IF (IU.EQ.3.AND.LP.LE.6) GOTO 520
         DO M=-1,1,2
            M1=(M+3)/2
            CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
            IBC2=MOD(IBCELL(I2,J2,K2),10)
            IF (IGS.EQ.1) GOTO 510
            IF (IBC2.LE.0) THEN
               AS(I,J,K,IU,M1)=FLWD(I1,J1,K1,IU,M1)
            ELSEIF (IBC2.EQ.2) THEN
               AS(I,J,K,IU,M1)=FLW(I2,J2,K2,IU,M1)
            ELSE            
               AS(I,J,K,IU,M1)=ZERO
            ENDIF
510         AP(I,J,K)=AP(I,J,K)+AS(I,J,K,IU,M1)
         ENDDO
520      CONTINUE
550   CONTINUE
900   RETURN
      END
