! SOURCEM.F90
!======================================================================
!     SOURCEM calculates source terms for kinetic species in a 
!     multi-species flow which may also be reacting when those kinetic
!     species have an associated transport equation.
!       6/01
!======================================================================
SUBROUTINE SOURCEM(MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
ioldsoot=0
IF (NELM.ne.11) then
   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2
      SFP(I,J,K)=ZERO
      BS(I,J,K)=ZERO
      IF (IBCELL(I,J,K).GT.0) CYCLE
      KD2=K/2
      G0=0
      IF (NELM.EQ.1) THEN !-----Sub-Species (CH4)
         G0=-RRMS(ID2,JD2,KD2,1)
      ELSEIF (NELM.EQ.2) THEN !-----Sub-Species (O2)
         G0=-RRMS(ID2,JD2,KD2,1)*1.5D+0-RRMS(ID2,JD2,KD2,2)*0.5D+0-RRMS(ID2,JD2,KD2,4)
      ELSEIF (NELM.EQ.3) THEN !-----Sub-Species (CO)
         G0=RRMS(ID2,JD2,KD2,1)-RRMS(ID2,JD2,KD2,2)
      ELSEIF (NELM.EQ.4) THEN !-----Sub-Species (H20)
         G0=RRMS(ID2,JD2,KD2,1)*TWO
      ELSEIF (NELM.EQ.5) THEN !-----Sub-Species (CO2)
         G0=RRMS(ID2,JD2,KD2,2)
      ELSEIF (NELM.EQ.6) THEN !-----Sub-Species (N2)
         G0=-RRMS(ID2,JD2,KD2,4)
      ELSEIF (NELM.EQ.7) THEN !-----Sub-Species (NO)
         G0=RRMS(ID2,JD2,KD2,4)*TWO
      ELSE
         EXIT
      ENDIF
      BS(I,J,K)=G0*WMS(NELM)/GMFR
   enddo;enddo;enddo
elseif (ioldsoot==1) then !nelm=11
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR smf--soot MASS fraction
   !----------------------------------------------------------------------
   G0=R0/(DNST0*UG0)
   !      T0_OX=1250 !value in summer 2004
   T0_ox=1000D+0 ! changed to this sometime in fall 2004 by bg, no explaination
   T0_F=1000
!   T0_F=1300
   FORM0=EXP(-4.0D+2/T0_F)
   OXID0=SQRT(T0_OX)*EXP(-4.0D+2/T0_OX)
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      BS(I,J,K)=ZERO
      SFP(I,J,K)=ZERO
      IF (IBCELL(I,J,K).GE.1.OR..NOT.REACT) CYCLE
      T1=T(I,J,K)*T0
      YF1=GFM(I,J,K,1)
      YOX1=GFM(I,J,K,2)
      !CSL     YOX1=GFM(I,J,K,2)+1.0D-3
      !csl         IF (YF1.GT.0.2D+0.AND.T1.GE.1.3D+3) THEN
      !CSL         IF (YF1.GT.YOX1*4.AND.T1.GT.T0_F) THEN
      IF (YF1.GT.SMALL20.AND.T1.GT.T0_F) THEN
         FORM=AFORM*YF1*(EXP(-4.0D+2/T1)-FORM0)
         BS(I,J,K)=FORM*G0
      ENDIF
      IF (T1.GT.T0_OX) THEN
         OXID=AOXID*YOX1*(SQRT(T1)*EXP(-4.0D+2/T1)-OXID0)
         SFP(I,J,K)=-OXID*G0
      ENDIF
      ! IF(I.GT.132) THEN ! Hard coded case dependent put in by B Golchert sometime fall 2004
      !      BS(I,J,K)=zero
      ! ENDIF
   enddo;enddo;enddo

else !nelm=11 new soot model

   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR smf - soot MASS fraction
   !----------------------------------------------------------------------
   snorm=r0/(dnst0*ug0)
   !T0_sf=500 !Cut off T [K] for soot formation, moved to relaxation file
   if (T0_sf < small20) then
      E0_sf=zero
   else
      E0_sf=exp(-Esf_R/T0_sf)
   endif
   if (T0_so < small20) then
      E0_so=zero
   else
   !T0_so=700 !Cut off T [K] for soot oxidation, moved to relaxation file
      E0_so=sqrt(T0_so)*exp(-Eso_R/T0_so)
   endif

   bs=zero
   sfp=zero
   do i=mi1,mi3,2
   do j=nj1,nj3,2
   do k=lk1,lk3,2
      Tcell=T(i,j,k)*T0
      if (ibcell(i,j,k) > 0 .or..not.react) cycle

      if (Tcell>T0_sf) then
         bs(i,j,k)=aform*gfm(i,j,k,1)*(exp(-Esf_R/Tcell)-E0_sf)*snorm
		 if (itr_ms > 1) bs(i,j,k)=rf_soot_form_source*bs(i,j,k)+(one-rf_soot_form_source)*bs_soot_prev(i,j,k)
      !else
      !  bs(i,j,k)=zero
      endif

      if (Tcell>T0_so) then
         sfp(i,j,k)=-aoxid*gfm(i,j,k,2)*(sqrt(Tcell)*exp(-Eso_R/Tcell)-E0_so)*snorm
	     if (itr_ms > 1) sfp(i,j,k)=rf_soot_oxid_source*sfp(i,j,k)+(one-rf_soot_oxid_source)*sfp_soot_prev(i,j,k)
      !else
      !  sfp(i,j,k)=zero
      endif
   enddo;enddo;enddo
   bs_soot_prev=bs !save current bs for relaxing on next iteration
   sfp_soot_prev=sfp !save current sfp for relaxing on next iteration
endif

return
end

