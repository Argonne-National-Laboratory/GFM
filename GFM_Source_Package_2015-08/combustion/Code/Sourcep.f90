! SOURCEP.F90
!======================================================================
!     Calculates source terms for each particle governing equation
!       10/97
!======================================================================
SUBROUTINE SOURCEP(MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

select case (nel-12)
case (1)
   !----------------------------------------------------------------------
   !     Source term for particle number density equation
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ENDIF
      BS(ID2,JD2,KD2)=ZERO
      SFP(ID2,JD2,KD2)=ZERO
   enddo;enddo;enddo


case (2, 3, 4)
   !----------------------------------------------------------------------
   !     Source term for particle velocity
   !       GDN,Z0 are particle constants defined in SETUP
   !----------------------------------------------------------------------
   IU=NEL-13
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
      JM1=J-1
      JM2=J-2
   DO K=LK1,LK3,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ELSEIF (DN(ID2,JD2,KD2,KL).LE.DN_MN) THEN
         BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
         !CSL        BS(ID2,JD2,KD2)=UG(I,J,K,IU)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ENDIF
      GMU=(T(I,J,K)/ONE)**AMU
      ZXK=ONE+0.15D+0*(REYD*RD(KL)*DNST(I,J,K)*ABS(UG(I,J,K,IU)-DU(ID2,JD2,KD2,KL,IU))/GMU)**0.687D+0
      GDZ=GDN/RFG_P/Z0*ZXK*GMU*DN(ID2,JD2,KD2,KL)/RD2(KL)*THETA(I,J,K)
      SFP(ID2,JD2,KD2)=-GDZ
      BS(ID2,JD2,KD2)=GDZ*UG(I,J,K,IU)+GRA(IU)*DN(ID2,JD2,KD2,KL)
      IF (RLE.LE.5.0D-2) cycle

      !---  solid pressure
      !CSL         DATA PDF,P_MU0,SP_C/6.0D-2,2.0D+1,0.0D+0/
      PDF=6.0D-2
      P_MU0=2.0+1
      BC_SV=1.0D-1
      GG0=5.0D+0
      IF (SP(I,J,K).gt.ZERO) then
         IF (IU.EQ.1) THEN
            DSP=SP(I-1,J,K)-SP(I+1,J,K)
            DDL=DX(I)
         ELSEIF (IU.EQ.2) THEN
            DSP=SP(I,J-1,K)-SP(I,J+1,K)
            DDL=DR(J)
         ELSEIF (IU.EQ.3) THEN
            DSP=SP(I,J,K-1)-SP(I,J,K+1)
            DDL=DZ(K)
         ENDIF
         BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+DSP/SP0/RD3(KL)/DDL
         !CSL     GOTO 250
      endif

      !---  shear stress
      PMU=P_MU0*DN(ID2,JD2,KD2,KL)*DND0/P_DND0
      !CSL         IF (IU.EQ.2) GOTO 230
      DO IU1=1,3
         IF (IU1.EQ.IU) cycle
         BS_P=ZERO
         SFP_P=ZERO
         DO M=-1,1,2
         CALL VINDX(IU1,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
         IF (IU1.EQ.1) THEN
            DDR=DX(I)
            DDR1=DX(I1)
         ELSEIF (IU1.EQ.2) THEN
            DDR=DR(J)*RS(J)
            DDR1=DR(J1)*RS(J1)
         ELSE
            DDR=DZ(K)
            DDR1=DZ(K1)
         ENDIF
         IF (IBCELL(I2,J2,K2).LE.0) THEN
            BS_P=BS_P+PMU*DU(ID21,JD21,KD21,KL,IU)/DDR1
            SFP_P=SFP_P-PMU/DDR1
         ELSEIF (IBCELL(I2,J2,K2).EQ.1) THEN 
            SFP_P=SFP_P-PMU*TWO/DDR*BC_SV
         ENDIF
         ENDDO
         G0=UG0/R0/(DDR*R0)/(P_MASS*DND0*UG0**2/R0)
         BS_P=BS_P*G0
         SFP_P=SFP_P*G0
         BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+BS_P
         SFP(ID2,JD2,KD2)=SFP(ID2,JD2,KD2)+SFP_P
      enddo

      !---  solid diffusion
      G0=ZERO
      DO IU2=1,3
         IF (IU2.EQ.IU) cycle
         DNU0=ABS(DU(ID2,JD2,KD2,KL,IU2))*DN(ID2,JD2,KD2,KL)
         DO M=-1,1,2
            CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
            IF (IU.EQ.1) THEN
               DDL=DX(I1)
            ELSEIF (IU.EQ.2) THEN
               DDL=DR(J1)*RS(J1)
            ELSE
               DDL=DZ(K1)
            ENDIF
            IF (IBCELL(I2,J2,K2).LE.0) THEN
               DNU1=ABS(DU(ID21,JD21,KD21,KL,IU2))*DN(ID21,JD21,KD21,KL)
               IF (SP(I2,J2,K2).LE.ZERO) THEN
                  G0=G0+M*(DNU0-DNU1)/DDL
               ENDIF
            ENDIF
         ENDDO
      enddo

      IF (TH_PT(ID2,JD2,KD2).GT.0.1D+0) THEN
         G0=G0*(0.6D+0-TH_PT(ID2,JD2,KD2))/0.5D+0
      ENDIF
      IF (ABS(G0).GT.GG0) THEN
         G0=SIGN(GG0,G0)
      ENDIF
      BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+PDF*G0
   enddo;enddo;enddo


case (5)
   !----------------------------------------------------------------------
   !     Source term for particle temperature
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ELSEIF (DN(ID2,JD2,KD2,KL).LE.DN_MN) THEN
         BS(ID2,JD2,KD2)=T(I,J,K)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ENDIF
      GMU=(T(I,J,K)/ONE)**AMU
      GDF=(T(I,J,K)/ONE)**AMD
      SVEL1=SVEL(I,J,K,KL)
      RELK=REYD*RD(KL)*DNST(I,J,K)*SVEL1/GMU
      SCNK=SCN*GMU/DNST(I,J,K)/GDF
      DNUCK=(TWO+0.654D+0*SQRT(RELK)*SCNK**THIRD)/DNUC0
      !         DNUCK=(3.D-2*RELK**1.3)/DNUC0
      GDZ=CP0/CL_P*GVN/RFG_P*GLAM(I,J,K)*DNUCK*DN(ID2,JD2,KD2,KL)/RD2(KL)
      !GDZ=GDZ*RF(5) ! 11/16/05: not proper way to relax a source term
      SFP(ID2,JD2,KD2)=-GDZ
      BS(ID2,JD2,KD2)=GDZ*T(I,J,K)
   enddo;enddo;enddo


case (6)
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR COKE
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1.OR.DN(ID2,JD2,KD2,KL).LE.DN_MN) THEN
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ENDIF
      !CSL         GDZ=CON(ID2,JD2,KD2)/RD3(KL)/RFG
      GDZ=CON(ID2,JD2,KD2)/RD3(KL)/RFG_P
      DU0=DU(ID2,JD2,KD2,KL,1)**2+DU(ID2,JD2,KD2,KL,2)**2+DU(ID2,JD2,KD2,KL,3)**2
      GU0=UG(I,J,K,1)**2+UG(I,J,K,2)**2+UG(I,J,K,3)**2
      IF (GU0.GT.1.0D-20) THEN
         G0=DU0/GU0
         GDZ=GDZ*SQRT(G0)
      ENDIF
      BS(ID2,JD2,KD2)=GDZ
      !CSL         BS(ID2,JD2,KD2)=0
      SFP(ID2,JD2,KD2)=ZERO
   enddo;enddo;enddo

end select
return
end
