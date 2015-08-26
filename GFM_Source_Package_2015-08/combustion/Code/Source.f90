! SOURCE.F90
!======================================================================
!     Calculates source terms for each gaseous governing equation
!       12/98
!======================================================================
SUBROUTINE SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!cbg      ALLOCATE ZKE(MP,NP,LP),CFAC(MP,NP,LP)
!cbg      DIMENSION ZKE(144,NP,LP),CFAC(144,NP,LP)
DATA SMX0/1.0D-14/    
!----------------------------------------------------------------------
!       TRDG: TL0/T0
!       TRDGC: 1-TL0/T0
!----------------------------------------------------------------------


select case (nelg)
case (1, 2, 3)
   !----------------------------------------------------------------------
   !     Source term for gas velocity
   !     IU1=1  X-DIRECTION 
   !         2  Y or R-DIRECTION 
   !         3  Z-DIRECTION 
   !----------------------------------------------------------------------
   IU1=NELG
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      CALL SRC1(I,J,K,IU1)
   enddo;enddo;enddo


case (4, 5)
   !----------------------------------------------------------------------
   !     Source term for 
   !        NELG=4: gas pressure eq.
   !             5: pressure correction eq.
   !----------------------------------------------------------------------
   if (nelg == 5) then
      res_mass=zero
      res_mass_max=zero
      imx=2
      jmx=2
      kmx=2
   endif
   do i=mi1,mi3,2
   do j=nj1,nj3,2
   do k=lk1,lk3,2
      if (ibcell(i,j,k).ge.1) cycle
      area_x=area_c(i,j,k,1)
      frx1=(theta(i-1,j,k)*dnst(i-1,j,k)*ug(i-1,j,k,1))*area_x
      frx2=(theta(i+1,j,k)*dnst(i+1,j,k)*ug(i+1,j,k,1))*area_x
      rx=frx1-frx2

      area_y=area_c(i,j,k,2)
      fry1=(theta(i,j-1,k)*dnst(i,j-1,k)*ug(i,j-1,k,2))*area_y
      fry2=(theta(i,j+1,k)*dnst(i,j+1,k)*ug(i,j+1,k,2))*area_y
      ry=fry1-fry2

      area_z=area_c(i,j,k,3)
      frz1=(theta(i,j,k-1)*dnst(i,j,k-1)*ug(i,j,k-1,3))*area_z
      frz2=(theta(i,j,k+1)*dnst(i,j,k+1)*ug(i,j,k+1,3))*area_z
      rz=frz1-frz2

      if (ndnp==0) then
         evms=zero
      else
         vol=vol_c(i,j,k)
         evms=(evp(i/2,j/2,k/2)-con(i/2,j/2,k/2))*vol
      endif
      bs(i,j,k)=(rx+ry+rz+evms)/eun
      sfp(i,j,k)=zero
      if (nelg == 5) then
         if (ibcell(i-1,j,k).eq.3.or.ibcell(i+1,j,k).eq.3.or. &
             ibcell(i,j-1,k).eq.3.or.ibcell(i,j+1,k).eq.3.or. &
             ibcell(i,j,k-1).eq.3.or.ibcell(i,j,k+1).eq.3) then
            cycle
         else
            !r_mass=abs(bs(i,j,k))
            fmax=max(abs(frx1),abs(frx2),abs(fry1),abs(fry2),abs(frz1),abs(frz2),evms)
            if(fmax==zero)then
               r_mass=zero
            else
               r_mass=abs(bs(i,j,k))*eun/fmax
            endif
         endif
         if(res_mass_max < r_mass) then
            imx=i
            jmx=j
            kmx=k
            res_mass_max = r_mass 
         endif
         res_mass=res_mass+r_mass
      endif
   enddo;enddo;enddo

   if (nelg==5) then
      res_mass=res_mass/ncells
   endif


case (6)
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR Fuel CONCENTRATION
   !----------------------------------------------------------------------
   IF (.NOT.REACT) THEN
      DO I=MI1,MI3,2
      DO J=NJ1,NJ3,2
      DO K=LK1,LK3,2
         IF (IBCELL(I,J,K).LE.0) THEN
            if (ndnp==0) then
               BS(I,J,K)=zero
            else
               BS(I,J,K)=EVP(I/2,J/2,K/2)
            endif
            SFP(I,J,K)=ZERO
         ELSE
            BS(I,J,K)=FZ(I,J,K)
            SFP(I,J,K)=ZERO
         ENDIF
      enddo;enddo;enddo
      RETURN
   ENDIF

   DO I=MI1,MI3,2
      IM2=I-2
      IP2=I+2
   DO J=NJ1,NJ3,2
      JP2=MIN(J+2,NJ3)
      JM2=MAX(J-2,NJ1)
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=FZ(I,J,K)
         SFP(I,J,K)=ZERO
         CRATE(I,J,K)=ZERO
         HRATE(I,J,K)=ZERO
         cycle
      ENDIF
      !-------- use evaporating fuel
      KP2=MIN(K+2,LK3)
      KM2=MAX(K-2,LK1)
      if (ndnp==0) then
         sc=zero
      else
         SC=EVP(I/2,J/2,K/2)
      endif

      !----------------------------------------------------------------------
      !     Integral Reaction Submodel (Jan. 94)
      !----------------------------------------------------------------------
      !     Determine flow time scale
      !      DTIME - dimensionless flow time
      !      DRTIME - flow time (s)
      !----------------------------------------------------------------------
      DTIME11=ABS(UG(I+1,J,K,1)/DX(I))
      DTIME12=ABS(UG(I-1,J,K,1)/DX(I))
      IF (NJ3.GT.6) THEN
         DTIME21=ABS(UG(I,J+1,K,2)/DR(J))
         DTIME22=ABS(UG(I,J-1,K,2)/DR(J))
      ENDIF
      IF (LK3.GT.6) THEN
         DTIME31=ABS(UG(I,J,K+1,3)/DZ(K))
         DTIME32=ABS(UG(I,J,K-1,3)/DZ(K))
      ENDIF
      DTIME=MAX(DTIME11,DTIME12,DTIME21,DTIME22,DTIME31,DTIME32,SMX0)
      DTIME=ONE/DTIME
      DRTIME=DTIME*R0/UG0

      !----------------------------------------------------------------------
      !     begin combustion kinetics
      !----------------------------------------------------------------------
      AYF=MIN(GF(I,J,K,IYF),GF(I,J,K,IYO2)/STOICH)
      !     IF(ISTOICH.EQ.1) THEN
      !       AYF=MIN(GF(I,J,K,IYF),(GF(I,J,K,IYO2)+GF(I,J,K,IYN2))/STOICH)
      !     ELSE
      !       AYF=MIN(GF(I,J,K,IYF),GF(I,J,K,IYO2)/STOICHOX)
      !     ENDIF
      IF (AYF.LT.1.D-5) THEN
         CRATE(I,J,K)=ZERO
         HRATE(I,J,K)=ZERO
         SFP(I,J,K)=ZERO
         BS(I,J,K)=ZERO
         cycle
      ENDIF
      ZP1=GF(I,J,K,IYCO2)/WCO2
      IF (ZP1.LE.small20) THEN
         DELR1=ZERO
      ELSE
         ZF=GF(I,J,K,IYF)/WF
         DELR1=ZP1/(ZF+ZP1)
      ENDIF
      !csl         DELR1=1.38D0*(GF(I,J,K,IYCO2)/WCO2/(GF(I,J,K,IYCO2)
      !csl     1           /WCO2+GF(I,J,K,IYF)/WF)-0.2756D0)
      !csl         DELR1=MAX(DELR1,ZERO)
      TIME1=ZY1(DELR,TIMED,DELR1,NR0)
      TIME2=TIME1 + DRTIME
      DELR2=ZY1(RTIM,DELRT,TIME2,NR0)
      HRD1=ZY1(DELR,HRD,DELR1,NR0)
      HRD2=ZY1(DELR,HRD,DELR2,NR0)
      !csl         TCH1=0.2967*DELR1*HRD1 + 0.8865
      !csl         TCH2=0.2967*DELR2*HRD2 + 0.8865
      AYF0=AYF/(ONE - DELR1)*DNST(I,J,K)*THETA(I,J,K)
      !csl         SR=AYF0*(DELR2*TCH1/TCH2 - DELR1)/DTIME
      SR=AYF0*(DELR2-DELR1)/DTIME
      IF (SR.LE.ZERO) THEN
         CRATE(I,J,K)=ZERO
         HRATE(I,J,K)=ZERO
         SFP(I,J,K)=ZERO
         BS(I,J,K)=SC
         cycle
      ENDIF
      !csl         N1=IP2
      !csl         IF (I.EQ.MI3) N1=I
      !csl         GFDX=(GF(N1,J,K,IYF)-GF(IM2,J,K,IYF))/(X(N1)-X(IM2))
      !csl         N1=J+2
      !csl         N2=J-2
      !csl         IF (J.EQ.NJ1) N2=J
      !csl         IF (J.EQ.NJ3) N1=J
      !csl         GFDY=(GF(I,N1,K,IYF)-GF(I,N2,K,IYF))/(R(N1)-R(N2))
      !csl         N1=K+2
      !csl         N2=K-2
      !csl         IF (K.EQ.LK1) N2=K
      !csl         IF (K.EQ.LK3) N1=K
      !csl         GFDZ=(GF(I,J,N1,IYF)-GF(I,J,N2,IYF))/(Z(N1)-Z(N2))
      !csl         cr=6.0d0
      !csl         SB=CR*SQRT(EPSR)*DNST(I,J,K)*SQRT(GF(I,J,K,IK))*
      !csl     &      SQRT(GFDX**2+GFDY**2+GFDZ**2)*THETA(I,J,K)
      !IF (itr_gas < 2) THEN
         CRATE(I,J,K)=SR
      !ELSE
      !   CRATE(I,J,K)=RF(21)*SR+RFC(21)*CRATE(I,J,K)
      !ENDIF
      SFP(I,J,K)=-CRATE(I,J,K)/(GF(I,J,K,IYF)+small20)
      BS(I,J,K)=SC
      !----------------------------------------------------------------------
      !      HRATE - dimensionless heat release rate (W/m**3)
      !----------------------------------------------------------------------
      !HRATE(I,J,K)=CRATE(I,J,K)*(Q0+DCP*(T(I,J,K)-TL0/T0))
      HRATE(I,J,K)=CRATE(I,J,K)*Q0
   enddo;enddo;enddo


case (7, 13)
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR CO2 (55% OF PRODUCTS)
   !----------------------------------------------------------------------
   IF (NELG.EQ.7) THEN
      G0=WCO2/WF
   ELSE
      G0=WH2O*TWO/WF
   ENDIF
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      SFP(I,J,K)=ZERO
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=FZ(I,J,K)
         cycle
      ELSEIF (.NOT.REACT) THEN
         BS(I,J,K)=ZERO
         cycle
      ENDIF
      BS(I,J,K)=CRATE(I,J,K)*G0
   enddo;enddo;enddo


case (8, 9)
   !----------------------------------------------------------------------
   !     Source term for  
   !       NELG=8: total gas enthalpy
   !       NELG=9: turbulent kinetic energy
   !----------------------------------------------------------------------
   IF (NELG.EQ.9.AND.LSTAR.LE.3) GOTO 900
   q_rad_vol_tot=zero
   q_react=zero
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) cycle
      IF (IBCELL(I+1,J,K).EQ.3.OR.IBCELL(I-1,J,K).EQ.3) THEN
         DUDX=ZERO
         DVDX=ZERO
         DWDX=ZERO
      ELSE
         DUDX=(UG(I+1,J,K,1)-UG(I-1,J,K,1))/DX(I)
         DVDX=(UG(I+1,J,K,2)-UG(I-1,J,K,2))/DX(I)
         DWDX=(UG(I+1,J,K,3)-UG(I-1,J,K,3))/DX(I)
      ENDIF
      IF (NJ3.LT.6.OR.IBCELL(I,J+1,K).EQ.3.OR.IBCELL(I,J-1,K).EQ.3) THEN
         DUDR=ZERO
         DVDR=ZERO
         DWDR=ZERO
      ELSE
         DUDR=(UG(I,J+1,K,1)-UG(I,J-1,K,1))/DR(J)
         DVDR=(UG(I,J+1,K,2)-UG(I,J-1,K,2))/DR(J)
         DWDR=(UG(I,J+1,K,3)-UG(I,J-1,K,3))/DR(J)
      ENDIF
      IF (LK3.LT.6.OR.IBCELL(I,J,K+1).EQ.3.OR.IBCELL(I,J,K-1).EQ.3) THEN
         DUDZ=ZERO
         DVDZ=ZERO
         DWDZ=ZERO
      ELSE
         DUDZ=(UG(I,J,K+1,1)-UG(I,J,K-1,1))/DZ(K)
         DVDZ=(UG(I,J,K+1,2)-UG(I,J,K-1,2))/DZ(K)
         DWDZ=(UG(I,J,K+1,3)-UG(I,J,K-1,3))/DZ(K)
      ENDIF
      IF (CYL) THEN
         VR=UG(I,J,K,2)/R(J)
      ELSE
         VR=ZERO
      ENDIF
      GTE(I,J,K)=TWO*(DUDX**2+DVDR**2+VR**2+DWDZ**2)+ &
         (DUDR+DVDX)**2+(DWDR+DVDZ)**2+(DUDZ+DWDX)**2-(DUDX+DVDR+DWDZ)**2/1.5D+0
   enddo;enddo;enddo

   !-----
   !     CALCULATE THE CONTRIBUTION TO THE SOURCE TERM
   !     DUE TO COMPRESSIBILITY
   !-----
   !cbg      DO 820 I=2,MP,2
   !cbg      DO 820 J=2,NP,2
   !cbg      DO 820 K=2,LP,2
   !cbg 820  ZKE(I,J,K)=(UG(I,J,K,1)**2+UG(I,J,K,2)**2+UG(I,J,K,3)**2)/TWO
   !cbg      DATA CFAC1/2.0D0/
   !cbg      DO 830 I=MI1,MI3,2
   !cbg      DO 830 J=NJ1,NJ3,2
   !cbg      DO 830 K=LK1,LK3,2
   !cbg         DFKEX=( (ZKE(I,J,K)*MAX(UG(I+1,J,K,1),ZERO) -
   !cbg     &           ZKE(I+2,J,K)*MAX(-UG(I+1,J,K,1),ZERO))
   !cbg     &          *THETA(I+1,J,K)*DNST(I+1,J,K) +
   !cbg     &           (ZKE(I,J,K)*MAX(-UG(I-1,J,K,1),ZERO) -
   !cbg     &           ZKE(I-2,J,K)*MAX(UG(I-1,J,K,1),ZERO))
   !cbg     &          *THETA(I-1,J,K)*DNST(I-1,J,K) )/DX(I)
   !cbg         DFKEY=( (ZKE(I,J,K)*MAX(UG(I,J+1,K,2),ZERO) -
   !cbg     &           ZKE(I,J+2,K)*MAX(-UG(I,J+1,K,2),ZERO))
   !cbg     &          *THETA(I,J+1,K)*DNST(I,J+1,K) +
   !cbg     &           (ZKE(I,J,K)*MAX(-UG(I,J-1,K,2),ZERO) -
   !cbg    &           ZKE(I,J-2,K)*MAX(UG(I,J-1,K,2),ZERO))
   !cbg     &          *THETA(I,J-1,K)*DNST(I,J-1,K) )/DR(J)
   !cbg     IF (LP.GE.6) THEN
   !cbg         DFKEZ=( (ZKE(I,J,K)*MAX(UG(I,J,K+1,3),ZERO) -
   !cbg     &           ZKE(I,J,K+2)*MAX(-UG(I,J,K+1,3),ZERO))
   !cbg     &          *THETA(I,J,K+1)*DNST(I,J,K+1) +
   !cbg     &           (ZKE(I,J,K)*MAX(-UG(I,J,K-1,3),ZERO) -
   !cbg     &           ZKE(I,J,K-2)*MAX(UG(I,J,K-1,3),ZERO))
   !cbg     &          *THETA(I,J,K-1)*DNST(I,J,K-1) )/DZ(K)
   !cbg     ENDIF
   !cbg         CFAC(I,J,K)=(DFKEX+DFKEY+DFKEZ)*ECN/CFAC1
   !cbg 830  CONTINUE

   IF (NELG.EQ.9) GOTO 900

   ! 01-09-06:  Note about efact, h_ex, h_rd, ek0, radgray.
   !All these variables are being commented out because:
   !  efact is set, but not used.
   !  h_ex is set in Extr and only used for determining efact
   !  h_rd is set (maybe) only after being used for determining efact
   !  ek0 is only used to determine h_rd
   !  radgray is only used to determine ek0 before radgray and ek0 may
   !    be read from the input namelist in the sbc file, otherwise
   !    radgray is not initialized
   !EFACT=(H_EX+ABS(H_RD))/(Q_IN-H_IN_OUT)
   !IF (EK0.GT.ZERO) THEN
   !   H_RD=ZERO
   !ENDIF

   DO I=MI1,MI3,2;  ID2=I/2
   DO J=NJ1,NJ3,2;  JD2=J/2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=FZ(I,J,K)
         SFP(I,J,K)=ZERO
         cycle
      ENDIF
      KD2=K/2
      GDZ=ZERO
      GDZH=ZERO
      if (key.ne.1) then
         !-----
         !     Contribution by condensed phase
         !-----
         GMU=(T(I,J,K)/ONE)**AMU
         GDF=(T(I,J,K)/ONE)**AMD
         DO L=1,NDNP
            IF (DN(ID2,JD2,KD2,L).LE.small20) cycle
            G0=ONE
            DTB=DT(ID2,JD2,KD2,L)/(TB(L)-1.0D-10)
            IF (L.LE.NDP0.AND.DTB.GE.ONE) THEN
               CALL DEVA1(ID2,JD2,KD2,L,G0)
               if (g0.gt.zero) cycle
            ENDIF
            SVEL1=SVEL(I,J,K,L)
            RELK=REYD*RD(L)*DNST(I,J,K)*SVEL1/GMU
            SCNK=SCN*GMU/DNST(I,J,K)/GDF
            DNUCK=(TWO+0.654D+0*SQRT(RELK)*SCNK**THIRD)/DNUC0
            DNUCK=GVN*GLAM(I,J,K)*DNUCK*DN(ID2,JD2,KD2,L)*RD(L)
            !IF (L.LE.NDP0) THEN
            !   DNUCK=DNUCK*RF(4) ! 11/16/05: not proper way to relax source term
            !ELSE
            !   DNUCK=DNUCK*RF(5) ! 11/16/05: not proper way to relax source term
            !ENDIF
            GDZ=GDZ+DNUCK
            GDZH=GDZH+DNUCK*(DT(ID2,JD2,KD2,L)-TRDG)/TRDGC
         enddo
      endif

      !----------------------------------------------------------------------
      !   HRATE - ACCUMULATED HEAT OF COMBUSTION
      !----------------------------------------------------------------------
      CPMIX(1)=CPF(1)*GF(I,J,K,IYF) &
              +CPCO2(1)*GF(I,J,K,IYCO2)+CPH2O(1)*GF(I,J,K,IYH2O) &
              +CPO2(1)*GF(I,J,K,IYO2)+CPN2(1)*GF(I,J,K,IYN2)
      !csl         BS(I,J,K)=GDZH+GAMA(I,J,K)*THETA(I,J,K)*ECN*GTE(I,J,K)
      !csl     &        +HRATE(I,J,K)   !-CFAC(I,J,K)
      BS(I,J,K)=GDZH+HRATE(I,J,K)
      SFP(I,J,K)=-GDZ/CPMIX(1)

      vol=vol_c(i,j,k)*vol0
      q_react = q_react + hrate(i,j,k) * qr0 * vol !heat release rate from combustion (W)

      !cbg         IF (EVP(ID2,JD2,KD2).GT.ZERO) THEN
      !cbg            ELH0=EVP(ID2,JD2,KD2)*(CPF(1)*(TB(1)-TRDG)/TRDGC-ELH)
      !cbg            BS(I,J,K)=BS(I,J,K)+ELH0
      !cbg         ENDIF
      !cbg         IF (CON(ID2,JD2,KD2).GT.ZERO) THEN
      !cbg            ECH0=CON(ID2,JD2,KD2)*CPF(1)*(T(I,J,K)-TRDG)/TRDGC 
      !cbg            BS(I,J,K)=BS(I,J,K)-ECH0
      !cbg         ENDIF

      !----------------------------------------------------------------------
      !     QE - RADIATION emissive heat rate
      !     QA - RADIATION absorptive heat rate
      !----------------------------------------------------------------------
      !VOL=VOL_C(I,J,K)*VOL0
      if (i1st_rad_done > 0 .or. i1st_rad_qe_done > 0) then
         !q_rad = QA1(ID2,JD2,KD2)-QE(ID2,JD2,KD2) !rad rate into cell (W)
         q_rad = QA(ID2,JD2,KD2)-QE(ID2,JD2,KD2) !rad rate into cell (W)
         BS(I,J,K) = BS(I,J,K) + q_rad/(vol*qr0) !qr0=h_0*dnst0*ug0/r0
         !BS(I,J,K) = BS(I,J,K) + QL(ID2,JD2,KD2)/(vol*qr0) !alternative to sequence below
         !for heat transfer from wall to gas
         !BS(I,J,K) = BS(I,J,K) + 
         !&        (QA1(ID2,JD2,KD2)-QE(ID2,JD2,KD2)+QL(ID2,JD2,KD2))
         !&        /(vol*qr0)
         q_rad_vol_tot = q_rad_vol_tot - q_rad !net radiation rate out of volume (W)
      
         ! 8/1/05 - replaced 1/28/06
         !Simple and quick model for convective heat transfer from 
         !wall to gas matches what was used in Twall routine
         !It would be better to use an energy wall function.
         !if(ibcell(i-1,j,k)==1) bs(i,j,k) = bs(i,j,k) + &
         !   h_g*dr(j)*dz(k)*area0*(T(i-2,j,k)-T(i,j,k))*T0/(vol*qr0)
      
         !if(ibcell(i+1,j,k)==1) bs(i,j,k) = bs(i,j,k) + &
         !   h_g*dr(j)*dz(k)*area0*(T(i+2,j,k)-T(i,j,k))*T0/(vol*qr0)

         !if(ibcell(i,j-1,k)==1) bs(i,j,k) = bs(i,j,k) + &
         !   h_g*dx(i)*dz(k)*area0*(T(i,j-2,k)-T(i,j,k))*T0/(vol*qr0)
      
         !if(ibcell(i,j+1,k)==1) bs(i,j,k) = bs(i,j,k) + &
         !   h_g*dx(i)*dz(k)*area0*(T(i,j+2,k)-T(i,j,k))*T0/(vol*qr0)

         !if(ibcell(i,j,k-1)==1) bs(i,j,k) = bs(i,j,k) + &
         !   h_g*dx(i)*dr(j)*area0*(T(i,j,k-2)-T(i,j,k))*T0/(vol*qr0)
      
         !if(ibcell(i,j,k+1)==1) bs(i,j,k) = bs(i,j,k) + &
         !   h_g*dx(i)*dr(j)*area0*(T(i,j,k+2)-T(i,j,k))*T0/(vol*qr0)
      endif
   enddo;enddo;enddo;

   ! Traverse boundary array and add heat sink for convective h.t. to walls & melt surf.
   if (i1st_rad_done > 0) then
   do n=1,b_cnt
      if (b_typ(n) == 1.or. b_typ(n)==4) then !wall or melt surf., not exit or inlet

         !Get indexes of flow cell center next to wall patch
         inc=b_i(n)
         jnc=b_j(n)
         knc=b_k(n)
         select case (b_orient(n))
         case(1) ! x-normal patch
            if(b_direct(n)==0) then !boundary face area vector points in + direction
              inc=inc+1
            else                    !boundary face area vector points in - direction
              inc=inc-1
            endif
            ds=dx(inc)
         case(2) ! y-normal patch
            if(b_direct(n)==0) then
              jnc=jnc+1
            else
              jnc=jnc-1
            endif
            ds=dr(jnc)
         case(3) ! z-normal patch
            if(b_direct(n)==0) then
              knc=knc+1
            else
              knc=knc-1
            endif
            ds=dz(knc)
         end select

         !bs(b_i(n),b_j(n),b_k)=bs(i,j,k)-q_conv(n)*area_face/(vol_cell*qr0)
         bs(inc,jnc,knc)=bs(inc,jnc,knc)-q_conv(n)/(ds*r0*qr0)
      endif
   enddo
   endif

   if (itr_gas > 1) then
      !Relax enthalpy source term  
      do i=mi1,mi3,2
      do j=nj1,nj3,2
      do k=lk1,lk3,2
         if (ibcell(i,j,k)>=1) cycle 
	     bs(i,j,k)=rf_hsource*bs(i,j,k)+(one-rf_hsource)*bs_hprev(i,j,k)
      enddo;enddo;enddo
   endif
   bs_hprev=bs !save current bs for relaxing on next iteration

   RETURN

   900 continue

   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR TURBULENT ENERGY EQUATION
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
   IF (IBCELL(I,J,K).GE.1) THEN
      BS(I,J,K)=FZ(I,J,K)
      SFP(I,J,K)=ZERO
      cycle
   ENDIF
   ID2=I/2
   JD2=J/2
   IF(KEY.EQ.-2)THEN
      !-----     SUM = SINK FOR TKE TRANSFER TO CONDENSED PHASE
      SUM=ZERO
      DO L=1,NDNP
         DUW=(DU(ID2-1,JD2,K/2,L,1)*DN(ID2-1,JD2,K/2,L) * (X(I)-X(I-1))+ &
              DU(ID2,JD2,K/2,L,1)*DN(ID2,JD2,K/2,L)*(X(I-1)-X(I-2)))/DX(I-1)
         DUE=(DU(ID2+1,JD2,K/2,L,1)*DN(ID2+1,JD2,K/2,L) * (X(I+1)-X(I))+ &
              DU(ID2,JD2,K/2,L,1)*DN(ID2,JD2,K/2,L)*(X(I+2)-X(I+1)))/DX(I+1)
         DVS=(DU(ID2,JD2-1,K/2,L,2)*DN(ID2,JD2-1,K/2,L) * (R(J)-R(J-1))+ &
              DU(ID2,JD2,K/2,L,2)*DN(ID2,JD2,K/2,L)*(R(J-1)-R(J-2)))/DR(J-1)
         DVN=(DU(ID2,JD2+1,K/2,L,2)*DN(ID2,JD2+1,K/2,L) * (R(J+1)-R(J))+ &
              DU(ID2,JD2,K/2,L,2)*DN(ID2,JD2,K/2,L)*(R(J+2)-R(J+1)))/DR(J+1)
         IF (LP.GE.6) THEN
         DWB=(DU(ID2,JD2,K/2-1,L,3)*DN(ID2,JD2,K/2-1,L) * (Z(K)-Z(K-1))+ &
              DU(ID2,JD2,K/2,L,3)*DN(ID2,JD2,K/2,L)*(Z(K-1)-Z(K-2)))/DZ(K-1)
         DWT=(DU(ID2,JD2,K/2+1,L,3)*DN(ID2,JD2,K/2+1,L) * (Z(K+1)-Z(K))+ &
              DU(ID2,JD2,K/2,L,3)*DN(ID2,JD2,K/2,L)*(Z(K+2)-Z(K+1)))/DZ(K+1)
         ELSE
            DWB=ZERO
            DWT=ZERO
         ENDIF 
         DXLP=RD3(L)/PSI(L)**2
         DFX=(DUE*GF(I+1,J,K,IK)-DUW*GF(I-1,J,K,IK))*DXLP
         DFY=(DVN*GF(I,J+1,K,IK)*RS(J+1)/RS(J)-DVS*GF(I,J-1,K,IK)*RS(J-1)/RS(J))*DXLP
         DFZ=(DWT*GF(I,J,K+1,IK)-DWB*GF(I,J,K-1,IK))*DXLP
         SUM=SUM+abs(DFX+DFY+DFZ)
      ENDDO   
      SUM=RFG*SUM   
   ELSE
      SUM=ZERO
   ENDIF
   BS(I,J,K)=TWO/(REYG*EPSR)*THETA(I,J,K)*TMU(I,J,K)*GTE(I,J,K)-SUM
   SFP(I,J,K)=-TAUR*THETA(I,J,K)*DNST(I,J,K)*GF(I,J,K,IEPS)/(GF(I,J,K,IK)+small20)
   enddo;enddo;enddo;


case (10)
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR TURBULENT DISSIPATION EQUATION
   !----------------------------------------------------------------------
   CBL0=2.5D+0*SQRT(EPSR)/TAUR*CMu**0.75D+0
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      SFP(I,J,K)=ZERO
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=FZ(I,J,K)
         cycle
      ENDIF
      CBL=CBL0*GF(I,J,K,IK)**1.5D+0
      BS(I,J,K)=ZERO
      IF (MP.GT.8.AND.IBCELL(I+1,J,K).EQ.1) THEN
         BS(I,J,K)=BS(I,J,K)+BIG*CBL/(X(I+1)-X(I))
         SFP(I,J,K)=-BIG
      ENDIF
      IF (MP.GT.8.AND.IBCELL(I-1,J,K).EQ.1) THEN
         BS(I,J,K)=BS(I,J,K)+BIG*CBL/(X(I)-X(I-1))
         SFP(I,J,K)=-BIG
      ENDIF
      IF (NP.GT.8.AND.IBCELL(I,J+1,K).EQ.1) THEN
         BS(I,J,K)=BS(I,J,K)+BIG*CBL/(R(J+1)-R(J))
         SFP(I,J,K)=-BIG
      ENDIF
      IF (NP.GT.8.AND.IBCELL(I,J-1,K).EQ.1) THEN
         BS(I,J,K)=BS(I,J,K)+BIG*CBL/(R(J)-R(J-1))
         SFP(I,J,K)=-BIG
      ENDIF
      IF (LP.GT.8.AND.IBCELL(I,J,K+1).EQ.1) THEN
         BS(I,J,K)=BS(I,J,K)+BIG*CBL/(Z(K+1)-Z(K))
         SFP(I,J,K)=-BIG
      ENDIF
      IF (LP.GT.8.AND.IBCELL(I,J,K-1).EQ.1) THEN
         BS(I,J,K)=BS(I,J,K)+BIG*CBL/(Z(K)-Z(K-1))
         SFP(I,J,K)=-BIG
      ENDIF
      IF (SFP(I,J,K).LE.-BIG) cycle
      BT=THETA(I,J,K)
      BS(I,J,K)=ct1*CMu*BT/TAUR*DNST(I,J,K)*GTE(I,J,K)*GF(I,J,K,IK)
      SFP(I,J,K)=-ct2*TAUR*BT*DNST(I,J,K)*GF(I,J,K,IEPS)/(GF(I,J,K,IK)+small20)
   enddo;enddo;enddo;


case (11)
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR INERT GAS SPECIES and richness
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=FZ(I,J,K)
         SFP(I,J,K)=ZERO
         cycle
      ENDIF
      BS(I,J,K)=ZERO
      SFP(I,J,K)=ZERO 
   enddo;enddo;enddo;


case (12)
   !----------------------------------------------------------------------
   !-----SOURCE TERM FOR oxidizer
   !----------------------------------------------------------------------
   DO I=MI1,MI3,2
   DO J=NJ1,NJ3,2
   DO K=LK1,LK3,2
      SFP(I,J,K)=ZERO
      IF (IBCELL(I,J,K).GE.1) THEN
         BS(I,J,K)=FZ(I,J,K)
         cycle
      ELSEIF (.NOT.REACT) THEN
         BS(I,J,K)=ZERO
         cycle
      ENDIF
      BS(I,J,K)=-CRATE(I,J,K)*STOICH
   enddo;enddo;enddo;

end select
return
end


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE SRC1(I,J,K,IU1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DIMENSION VIS(3,2),DXN(3),DX1(3),DX0(3)
DATA SMX0/1.0D-14/  
  
IF (IBCELL(I,J,K).GE.1) THEN
   BS(I,J,K)=ZERO
   SFP(I,J,K)=ZERO  
   RETURN
ENDIF
DXN(1)=DX(I)
DXN(2)=DR(J)*RS(J)
DXN(3)=DZ(K)
!csl      IF (IBCELL(I,J,K).LE.-1) THEN
!csl         DXN(2)=DXN(2)/TWO
!csl         DXN(3)=DXN(3)/TWO
!csl      ENDIF

!----------------------------------------------------------------------
!     Viscous effect
!----------------------------------------------------------------------
DO M=-1,1,2
   CALL VINDX(IU1,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
   IF (IU1.EQ.1) THEN
      DX0(1)=DX(I)
      DX1(1)=DX(I1)
   ELSEIF (IU1.EQ.2) THEN
      DX0(2)=DR(J)*RS(J)
      DX1(2)=DR(J1)*RS(J1)
   ELSE
      DX0(3)=DZ(K)
      DX1(3)=DZ(K1)
   ENDIF
   IF (IBCELL(I2,J2,K2).EQ.2) THEN
      THGM=THETA(I2,J2,K2)*GAMA(I2,J2,K2)
      IF (IU1.EQ.1) DXN(1)=DXN(1)+ABS(X(I2)-X(I1))
      IF (IU1.EQ.2) DXN(2)=DXN(2)+ABS(R(J2)-R(J1))*RS(J)
      IF (IU1.EQ.3) DXN(3)=DXN(3)+ABS(Z(K2)-Z(K1))
   ELSE
      THGM=THETA(I1,J1,K1)*GAMA(I1,J1,K1)
   ENDIF
   IF (M.EQ.-1) THEN
      VIS(IU1,1)=THGM*(FZ(I,J,K)-FZ(I2,J2,K2))/DX1(IU1)
   ELSE
      VIS(IU1,2)=THGM*(FZ(I2,J2,K2)-FZ(I,J,K))/DX1(IU1)
   ENDIF
ENDDO

VMS=(VIS(IU1,2)-VIS(IU1,1))/DXN(IU1)
DO IU=1,3
   IF (IU.EQ.IU1) cycle
   I1=I
   J1=J
   K1=K
   DO M=-1,1,2
      IF (IU.EQ.1) THEN
         I1=I+M
      ELSEIF (IU.EQ.2) THEN
         J1=J+M
      ELSE
         K1=K+M
      ENDIF
      IF (IU1.EQ.1) THEN
         DU0=FZ(I+1,J1,K1)-FZ(I-1,J1,K1)
         !               DU0=UG(I+1,J1,K1,IU)-UG(I-1,J1,K1,IU)
      ELSEIF (IU1.EQ.2) THEN
         DU0=FZ(I1,J+1,K1)-FZ(I1,J-1,K1)
         !               DU0=UG(I1,J+1,K1,IU)-UG(I1,J-1,K1,IU)
      ELSEIF (IU1.EQ.3) THEN
         DU0=FZ(I1,J1,K+1)-FZ(I1,J1,K-1)
         !              DU0=UG(I1,J1,K+1,IU)-UG(I1,J1,K-1,IU)
      ENDIF
      M1=(M+3)/2
      VIS(IU,M1)=THETA(I1,J1,K1)*GAMA(I1,J1,K1)*DU0/DX0(IU1)
      IF (IU.EQ.2) VIS(IU,M1)=VIS(IU,M1)*RS(J1)
   ENDDO
   VMS=VMS+(VIS(IU,2)-VIS(IU,1))/DXN(IU)
enddo

!----------------------------------------------------------------------
!       GDZ: interfacial drag
!       DUAV: momentum source from evaporation
!----------------------------------------------------------------------
GDZ=ZERO
GDZU=ZERO
if (key.ne.1 .or. (fr_q+fr_p).gt.zero) then
   DUAV=ZERO
   DNSUM=ZERO
   GCPA=GCP(I,J,K)
   GMU=T(I,J,K)**AMU
   I1=I/2
   I2=I1
   J1=J/2
   J2=J1
   K1=K/2
   K2=K1
   IF (IU1.EQ.1) THEN
      I1=(I-1)/2
      I2=I1+1
      F1=(X(I+1)-X(I))/DX(I)
   ELSEIF (IU1.EQ.2) THEN
      J1=(J-1)/2
      J2=J1+1
      F1=(R(J+1)-R(J))/DR(J)
   ELSEIF (IU1.EQ.3) THEN
      K1=(K-1)/2
      K2=K1+1
      F1=(Z(K+1)-Z(K))/DZ(K)
   ENDIF
   F2=ONE-F1

   DO L=1,NDNP
      DNK=F1*DN(I1,J1,K1,L)+F2*DN(I2,J2,K2,L)
      IF (DNK.LT.SMX0) cycle
      DUK=F1*DU(I1,J1,K1,L,IU1)+F2*DU(I2,J2,K2,L,IU1)
      DTK=F1*DT(I1,J1,K1,L)+F2*DT(I2,J2,K2,L)
      ZXK=ONE+0.15D+0*(REYD*RD(L)*DNST(I,J,K)*ABS(FZ(I,J,K)-DUK)/GMU)**0.687D+0
      IF (DTK.GE.TB(L)-1.0D-10.AND.L.LE.NDP0.AND.ELH.GT.ZERO) THEN
         BK=GCPA/ELH*(T(I,J,K)-TB(L))/TRDGC
         BK=MAX(BK,ZERO)
         ZXK=ZXK/(ONE+BK)
      ENDIF
      ZXK=ZXK*GDN/Z0*GMU*RD(L)*DNK*THETA(I,J,K)
      GDZ=GDZ+ZXK
      GDZU=GDZU+ZXK*DUK  
      IF (L.GT.NDP0) cycle
      DUAV=DUAV+DNK*DUK   
      DNSUM=DNSUM+DNK
   enddo

   IF (DNSUM.GT.SMX0 .and. ndnp.ne.0) THEN
      EVP0=F1*EVP(I1,J1,K1)+F2*EVP(I2,J2,K2)
      DUAV=DUAV/DNSUM*EVP0
   ELSE
      DUAV=ZERO
   ENDIF
endif

BS(I,J,K)=VMS+GDZU+DUAV
!IF (itr_gas.GT.500) ! looks like bouyancy is off for 1st 500 interations,  8/1/05 
!&   BS(I,J,K)=BS(I,J,K)+THETA(I,J,K)*DNST(I,J,K)*GRA(IU1)
BS(I,J,K)=BS(I,J,K)+THETA(I,J,K)*DNST(I,J,K)*GRA(IU1)
SFP(I,J,K)=-GDZ
RETURN
END

!======================================================================
!======================================================================
!======================================================================
FUNCTION ZY1(X,Y,X1,N)
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
DIMENSION X(200),Y(200)

IF (X1 .LT. X(1)) THEN
   ZY1=Y(1)*X1/X(1)
   return
ENDIF
DO I=2,N
   IF (X1 .LT. X(I)) THEN
      RATIO=(X1-X(I-1))/(X(I)-X(I-1))
      ZY1=Y(I-1)+(Y(I)-Y(I-1))*RATIO
      return
   ENDIF
enddo
ZY1=Y(N)
RETURN
END
