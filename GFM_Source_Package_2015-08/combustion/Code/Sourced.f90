! SOURCED.F90
!======================================================================
!     Calculates source terms for each droplet governing equation
!       10/97
!======================================================================
SUBROUTINE SOURCED(MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION FB(NDP0),DNUS(NDP0)

select case (nel-12)
case (1)
   !----------------------------------------------------------------------
   !     Source term for droplet number density equation
   !----------------------------------------------------------------------
   IF (NDP0.LE.0.OR.KL.GT.NDP0) RETURN
   IF (KL.EQ.NDP0) THEN
      KR=KL
   ELSE
      KR=KL+1
   ENDIF
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ENDIF
      IF (IBCELL(I+2,J,K).EQ.3.OR.IBCELL(I-2,J,K).EQ.3.OR. &
          IBCELL(I,J+2,K).EQ.3.OR.IBCELL(I,J-2,K).EQ.3.OR. &
          IBCELL(I,J,K+2).EQ.3.OR.IBCELL(I,J,K-2).EQ.3) THEN
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      END IF
      GMU=(T(I,J,K)/ONE)**AMU
      GDF=(T(I,J,K)/ONE)**AMD
      DO L=KL,KR
         DREK=REYD*RD(L)*DNST(I,J,K)*SVEL(I,J,K,L)
         SCHK=SCN*GMU/GDF/DNST(I,J,K)
         DNUS(L)=(ONE+0.276D+0*SQRT(DREK)*SCHK**THIRD)/DNUL0
         IF (DT(ID2,JD2,KD2,L).GE.TB(L)-1.0D-10.AND.ELH.GT.ZERO) THEN
            BK=GCP(I,J,K)/ELH*(T(I,J,K)-TB(L))/TRDGC
            BK=MAX(ZERO,BK)
            FB(L)=LOG(ONE+BK)
            CALL DEVA1(ID2,JD2,KD2,L,G0)
            FB(L)=FB(L)*G0
         ELSE
            FB(L)=ZERO
         ENDIF
      enddo
      DNCSL=DNUS(KL)*FB(KL)/RD(KL) 
      IF (KL.GE.KR) THEN
         DNCSR=ZERO
      ELSE
         DNCSR=DNUS(KR)*DN(ID2,JD2,KD2,KR)*FB(KR)/RD(KR)
      ENDIF
      GRG=GCN*THIRD/RFG*GLAM(I,J,K)/GCP(I,J,K)
      BS(ID2,JD2,KD2)=GRG/DRD(KR)*DNCSR
      SFP(ID2,JD2,KD2)=-GRG/DRD(KL)*DNCSL
   enddo;enddo;enddo


case (2, 3, 4)
   !----------------------------------------------------------------------
   !     Source term for droplet velocity
   !       GDN,Z0 are droplet constants defined in SETUP
   !----------------------------------------------------------------------
   IU=NEL-13
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2;  KD2=K/2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ELSEIF (DN(ID2,JD2,KD2,KL).LE.DN_MN) THEN
         !CSL        BS(ID2,JD2,KD2)=UG(I,J,K,IU)
         BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
         SFP(ID2,JD2,KD2)=ZERO
         cycle
      ENDIF
      GMU=(T(I,J,K)/ONE)**AMU
      ZXK=ONE+0.15D+0*(REYD*RD(KL)*DNST(I,J,K)*ABS(UG(I,J,K,IU)-FZ(ID2,JD2,KD2))/GMU)**0.687D+0
      IF (DT(ID2,JD2,KD2,KL).GE.TB(KL)-1.0D-10.AND.ELH.GT.ZERO) THEN
         BK=GCP(I,J,K)/ELH*(T(I,J,K)-TB(KL))/TRDGC
         BK=MAX(BK,ZERO)
         ZXK=ZXK/(ONE+BK)
      ENDIF
      GDZ=GDN/RFG/Z0*ZXK*GMU*DN(ID2,JD2,KD2,KL)/RD2(KL)*THETA(I,J,K)
      SFP(ID2,JD2,KD2)=-GDZ
      BS(ID2,JD2,KD2)=GDZ*UG(I,J,K,IU)+GRA(IU)*DN(ID2,JD2,KD2,KL)
   enddo;enddo;enddo


case (5)
   !----------------------------------------------------------------------
   !     Source term for droplet temperature
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2;  KD2=K/2
      SFP(ID2,JD2,KD2)=ZERO
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(ID2,JD2,KD2)=ZERO
         cycle
      ELSEIF (DN(ID2,JD2,KD2,KL).LE.DN_MN) THEN
         BS(ID2,JD2,KD2)=MIN(T(I,J,K),TB(KL))
         cycle
      ENDIF
      GMU=(T(I,J,K)/ONE)**AMU
      GDF=(T(I,J,K)/ONE)**AMD
      SVEL1=SVEL(I,J,K,KL)
      RELK=REYD*RD(KL)*DNST(I,J,K)*SVEL1/GMU
      SCNK=SCN*GMU/DNST(I,J,K)/GDF
      DNUCK=(TWO+0.654D+0*SQRT(RELK)*SCNK**THIRD)/DNUC0
      !         DNUCK=(3.D-2*RELK**1.3)/DNUC0
      GDZ=CP0/CL*GVN/RFG*GLAM(I,J,K)*DNUCK*DN(ID2,JD2,KD2,KL)/RD2(KL)
      !GDZ=GDZ*RF(4) ! 11/16/05: not proper way to relax source term
      BS(ID2,JD2,KD2)=GDZ*T(I,J,K)
      SFP(ID2,JD2,KD2)=-GDZ
   enddo;enddo;enddo

end select
return
end
