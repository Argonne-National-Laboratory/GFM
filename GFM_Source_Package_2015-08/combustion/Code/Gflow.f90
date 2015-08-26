! Gflow.f90
!======================================================================
!     Calculates the gas phase variables 
!       revision: 10/98
!======================================================================
SUBROUTINE GFLOW
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
!     Calculate transport properties
!        TMU: turbulent diffusivity
!        GAMA: diffusion coefficients
!-----WALL FUNCTION APPLIED TO ANY SOLID INTERFACE OF FLUID CELL
!----------------------------------------------------------------------

DO I=2,MP
DO J=2,NP
DO K=2,LP
   IF (IBCELL(I,J,K).GE.1) cycle
   IF (IBCELL(I-1,J,K).EQ.1) THEN
      G1=(X(I)-X(I-1))**2
      G2=(X(I+2)-X(I-1))**2
      X01=G2/(G2-G1)
      X02=G1/(G1-G2)
      THETA(I-1,J,K)=THETA(I,J,K)*X01+THETA(I+2,J,K)*X02
   ENDIF
   IF (IBCELL(I+1,J,K).EQ.1) THEN
      G1=(X(I)-X(I+1))**2
      G2=(X(I-2)-X(I+1))**2
      X01=G2/(G2-G1)
      X02=G1/(G1-G2)
      THETA(I+1,J,K)=THETA(I,J,K)*X01+THETA(I-2,J,K)*X02
   ENDIF

   IF (IBCELL(I,J-1,K).EQ.1) THEN
      G1=(R(J)-R(J-1))**2
      G2=(R(J+2)-R(J-1))**2
      X01=G2/(G2-G1)
      X02=G1/(G1-G2)
      THETA(I,J-1,K)=THETA(I,J,K)*X01+THETA(I,J+2,K)*X02
   ENDIF
   IF (IBCELL(I,J+1,K).EQ.1) THEN
      G1=(R(J)-R(J+1))**2
      G2=(R(J-2)-R(J+1))**2
      X01=G2/(G2-G1)
      X02=G1/(G1-G2)
      THETA(I,J+1,K)=THETA(I,J,K)*X01+THETA(I,J-2,K)*X02
   ENDIF
   
   IF (IBCELL(I,J,K-1).EQ.1) THEN
      G1=(Z(K)-Z(K-1))**2
      G2=(Z(K+2)-Z(K-1))**2
      X01=G2/(G2-G1)
      X02=G1/(G1-G2)
      THETA(I,J,K-1)=THETA(I,J,K)*X01+THETA(I,J,K+2)*X02
   ENDIF
   IF (IBCELL(I,J,K+1).EQ.1) THEN
      G1=(Z(K)-Z(K+1))**2
      G2=(Z(K-2)-Z(K+1))**2
      X01=G2/(G2-G1)
      X02=G1/(G1-G2)
      THETA(I,J,K+1)=THETA(I,J,K)*X01+THETA(I,J,K-2)*X02
   ENDIF
enddo;enddo;enddo


DO I=2,MP
DO J=2,NP
DO K=2,LP
   IF (IBCELL(I,J,K).NE.1) cycle
   GAMA(I,J,K)=ZERO
enddo;enddo;enddo

RT20=REYG2*SQRT(EPSR)*CMu**(0.25D+0)
DO I=2,MP
DO J=2,NP
DO K=2,LP
   IF (IBCELL(I,J,K).GE.1) cycle
   IF (IBCELL(I-1,J,K).EQ.1.OR.IBCELL(I+1,J,K).EQ.1) GOTO 50
   IF (IBCELL(I,J-1,K).EQ.1.OR.IBCELL(I,J+1,K).EQ.1) GOTO 50
   IF (IBCELL(I,J,K-1).EQ.1.OR.IBCELL(I,J,K+1).EQ.1) GOTO 50  !if not do we want to cycle

   50 continue

   C=(T(I,J,K)/TRDG)**AMU
   BLVM=RT20*DNST(I,J,K)*SQRT(GF(I,J,K,IK))/C
   VGAM=C/reyg2
   IF (IBCELL(I-1,J,K).EQ.1) THEN
      BLV=BLVM*(X(I)-X(I-1))
      GAMA0=VGAM*TGAMF(BLV)
      GAMA(I-1,J,K)=MAX(GAMA0,GAMA(I-1,J,K))
      TMU(I-1,J,K)=reyg2*GAMA(I-1,J,K)
      TMU(I-2,J,K)=TMU(I-1,J,K)
   ENDIF
   IF (IBCELL(I+1,J,K).EQ.1) THEN
      BLV=BLVM*(X(I+1)-X(I))
      GAMA0=VGAM*TGAMF(BLV)
      GAMA(I+1,J,K)=MAX(GAMA0,GAMA(I+1,J,K))
      TMU(I+1,J,K)=reyg2*GAMA(I+1,J,K)
      TMU(I+2,J,K)=TMU(I+1,J,K)
   ENDIF
   IF (IBCELL(I,J-1,K).EQ.1) THEN
      BLV=BLVM*(R(J)-R(J-1))
      GAMA0=VGAM*TGAMF(BLV)
      GAMA(I,J-1,K)=MAX(GAMA0,GAMA(I,J-1,K))
      TMU(I,J-1,K)=reyg2*GAMA(I,J-1,K)
      TMU(I,J-2,K)=TMU(I,J-1,K)
   ENDIF
   IF (IBCELL(I,J+1,K).EQ.1) THEN
      BLV=BLVM*(R(J+1)-R(J))
      GAMA0=VGAM*TGAMF(BLV)
      GAMA(I,J+1,K)=MAX(GAMA0,GAMA(I,J+1,K))
      TMU(I,J+1,K)=reyg2*GAMA(I,J+1,K)
      TMU(I,J+2,K)=TMU(I,J+1,K)
   ENDIF
   IF (IBCELL(I,J,K-1).EQ.1) THEN
      BLV=BLVM*(Z(K)-Z(K-1))
      GAMA0=VGAM*TGAMF(BLV)
      GAMA(I,J,K-1)=MAX(GAMA0,GAMA(I,J,K-1))
      TMU(I,J,K-1)=reyg2*GAMA(I,J,K-1)
      TMU(I,J,K-2)=TMU(I,J,K-1)
   ENDIF
   IF (IBCELL(I,J,K+1).EQ.1) THEN
      BLV=BLVM*(Z(K+1)-Z(K))
      GAMA0=VGAM*TGAMF(BLV)
      GAMA(I,J,K+1)=MAX(GAMA0,GAMA(I,J,K+1))
      TMU(I,J,K+1)=reyg2*GAMA(I,J,K+1)
      TMU(I,J,K+2)=TMU(I,J,K+1)
   ENDIF
enddo;enddo;enddo

!---
!      RT20=REYG2*SQRT(EPSR)*CMu**(0.25D+0)
!      DO 100 I=2,MP
!      DO 100 J=2,NP
!   DO 100 K=2,LP
!         IF (IBCELL(I,J,K).NE.1) GOTO 100
!         I1=I
!         J1=J
!         K1=K 
!         IF (IBCELL(I+1,J,K).LE.0.AND.I.LE.MPM2) THEN
!        I1=I+1
!     ENDIF
!         IF (IBCELL(I-1,J,K).LE.0.AND.I.GE.3) THEN
!        I1=I-1
!     ENDIF
!         IF (IBCELL(I,J+1,K).LE.0.AND.J.LE.NPM1) THEN
!        J1=J+1
!     ENDIF
!         IF (IBCELL(I,J-1,K).LE.0.AND.J.GE.3) THEN
!        J1=J-1
!     ENDIF
!         IF (IBCELL(I,J,K+1).LE.0.AND.K.LE.LPM1) THEN
!        K1=K+1
!     ENDIF
!         IF (IBCELL(I,J,K-1).LE.0.AND.K.GE.3) THEN
!        K1=K-1
!     ENDIF
!     IF (IBCELL(I1,J1,K1).GE.1) THEN
!        GAMA(I,J,K)=ZERO
!        TMU(I,J,K)=ZERO
!            GOTO 100
!     ENDIF
!50       C=(T(I1,J1,K1)/TRDG)**AMU
!         BLVM=RT20*DNST(I1,J1,K1)*SQRT(GF(I1,J1,K1,IK))/C
!         VGAM=C/REYG2
!     DDL=(X(I1)-X(I))**2+(R(J1)-R(J))**2+(Z(K1)-Z(K))**2
!         BLV=BLVM*SQRT(DDL)
!         GAMA(I,J,K)=VGAM*TGAMF(BLV)
!         TMU(I,J,K)=REYG2*GAMA(I,J,K)
!100   CONTINUE

!----------------------------------------------------------------------
!     GSOLVE calculates the gas phase variables
!----------------------------------------------------------------------
CALL GSLV3V

!----------------------------------------------------------------------
!     calculates derived variables
!       P: relative to the reference node
!----------------------------------------------------------------------
I=I_REF
J=J_REF
K=K_REF
I1=I
J1=J
K1=K
I2=I
J2=J
K2=K
IF (I.LT.MPM2.AND.IBCELL(I+2,J,K).EQ.0) THEN
   NX=1
   I1=I+2
   IF (IBCELL(I+4,J,K).EQ.0) THEN
      I2=I+4
   ELSE
      I2=I1
   ENDIF
ELSEIF (I.GT.2.AND.IBCELL(I-2,J,K).EQ.0) THEN
   NX=1
   I1=I-2
   IF (IBCELL(I-4,J,K).EQ.0) THEN
      I2=I-4
   ELSE
      I2=I1
   ENDIF
ELSEIF (J.LT.NPM2.AND.IBCELL(I,J+2,K).EQ.0) THEN
   NX=2
   J1=J+2
   IF (J.LT.NPM2.AND.IBCELL(I,J+4,K).LE.0) THEN
      J2=J+4
   ELSE
      J2=J1
   ENDIF
ELSEIF (J.GT.2.AND.IBCELL(I,J-2,K).LE.0) THEN
   NX=2
   J1=J-2
   IF (J.GT.4.AND.IBCELL(I,J-4,K).LE.0) THEN
      J2=J-4
   ELSE
      J2=J1
   ENDIF
ELSEIF (K.LT.LPM2.AND.IBCELL(I,J,K+2).LE.0) THEN
   NX=3
   K1=K+2
   IF (K.LT.LPM2.AND.IBCELL(I,J,K+4).LE.0) THEN
      K2=K+4
   ELSE
      K2=K1
   ENDIF
ELSEIF (K.GT.2.AND.IBCELL(I,J,K-2).LE.0) THEN
   NX=3
   K1=K-2
   IF (K.GT.4.AND.IBCELL(I,J,K-4).LE.0) THEN
      K2=K-4
   ELSE
      K2=K1
   ENDIF
ENDIF
CALL EXTR1(F1,F2,I,J,K,I1,J1,K1,I2,J2,K2,NX)
P(I,J,K)=P(I1,J1,K1)*F1+P(I2,J2,K2)*F2
G0=P(I1,J1,K1)-P_REF

!----------------------------------------------------------------------
!     T: gas temperature
!     DNST: gas density
!     THER: thermal properties
!----------------------------------------------------------------------
! 7/19/05
!Looping on gslv3g here does not appear to make sense
!because feedback between the equations solved is minimal
!If a tighter solution of the scaler transport equations is
!desired, the number of sweeps in the core solver should be increased.
!DO N=1,5 
CALL GSLV3G
!ENDDO

!if (igresid>0) write(nu_gres,"(i7,9e24.16)") itr_gas,h_resid,p_resid,(u_resid(i),i=1,3),pcorr_resid
if (igresid>0) then
   write(nu_gres,"(i7,5e24.16)") itr_gas,h_resid,p_resid,(u_resid(i),i=1,3)
endif
if (igresidp>0) then
   write(nu_gresp,"(i7,5e24.16)") itr_gas,h_resid_pre,p_resid_pre,(u_resid_pre(i),i=1,3)
endif

!if (igresidx>0) write(nu_gresx,"(i7,9e24.16)") itr_gas,(gf_resid(i),i=1,2),(gf_resid(i),i=6,7),(gf_resid(i),i=4,5)
if (igresidx>0) then
   write(nu_gresx,"(i7,6e24.16)") itr_gas,(gf_resid(i),i=1,2),(gf_resid(i),i=6,7),(gf_resid(i),i=4,5)
endif
if (igresidxp>0) then
   write(nu_gresxp,"(i7,6e24.16)") itr_gas,(gf_resid_pre(i),i=1,2),(gf_resid_pre(i),i=6,7), &
                                           (gf_resid_pre(i),i=4,5)
endif
         !goto 322
         !filename=casedir//'\enth'//runum//'c.txt'                         !debugging code here  
         !open(83,file=filename)
         !write(83,*) 'Enthalpy <=> Temperature'
         !write(83,'(/"  i   j   k    E before e2t      ", &
         !                           "T before e2t      ", &
         !                           "E after e2t       ", &
         !                           "T after e2t       ", &
         !                           "E after t2e       ", &
         !                           "T after t2e       ", &
         !                           "E after e2t       ", &
         !                          "T after e2t       "/)')

         !filename=casedir//'\newenth'//runum//'c.txt'                         !debugging code here  
         !open(84,file=filename)
         !write(84,*) 'New Enthalpy <=> Temperature'
         !write(84,'(/"  i   j   k    E before e2t      ", &
         !                           "T before e2t      ", &
         !                           "E after e2t       ", &
         !                           "T after e2t       ", &
         !                           "E after t2e       ", &
         !                           "T after t2e       ", &
         !                           "E after e2t       ", &
         !                           "T after e2t       ", &
         !                           "E after t2e       ", &
         !                           "T after t2e       "/)')
         !322   continue

DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IF (IBCELL(I,J,K).GE.1) cycle
   CALL CONC(I,J,K)
   !CALL ENTH(I,J,K,2) !  8/1/05 separated ENTH into 2 routines

   !ienth_test=0
   !if (ienth_test==0) then
      call enth_to_T(i,j,k) ! calculates temperature from enthalpy
   !else
         !goto 333
         !b4e2t_e = gf(i,j,k,ih)
         !b4e2t_t = T(i,j,k)
         !b4e2t_en = gf(i,j,k,ih)
         !b4e2t_tn = T(i,j,k)

         !call enth_to_T(i,j,k) ! calculates temperature from enthalpy
         !afe2t_e = gf(i,j,k,ih)
         !afe2t_t = T(i,j,k)

         !call T_to_enth(i,j,k)
         !aft2e_e = gf(i,j,k,ih)
         !aft2e_t = T(i,j,k)

         !call enth_to_T(i,j,k)
         !afe2t_e2 = gf(i,j,k,ih)
         !afe2t_t2 = T(i,j,k)

         !call T_to_enth(i,j,k)
         !write(83,'(3i4,10e18.10)') i,j,k,b4e2t_e,b4e2t_t,afe2t_e,afe2t_t,aft2e_e,aft2e_t,afe2t_e2,afe2t_t2,gf(i,j,k,ih),T(i,j,k)

         !repeat for new routines 

         !gf(i,j,k,ih) = b4e2t_en
         !T(i,j,k)     = b4e2t_tn
         !333      continue

         !b4e2t_en = gf(i,j,k,ih)
         !b4e2t_tn = T(i,j,k)
         !call new_enth_to_T(i,j,k) ! calculates temperature from enthalpy
         !afe2t_en = gf(i,j,k,ih)
         !afe2t_tn = T(i,j,k)

         !call new_T_to_enth(i,j,k)
         !aft2e_en = gf(i,j,k,ih)
         !aft2e_tn = T(i,j,k)

         !call new_enth_to_T(i,j,k)
         !afe2t_enn = gf(i,j,k,ih)
         !afe2t_tnn = T(i,j,k)

         !call new_T_to_enth(i,j,k)
         !write(84,'(3i4,10e18.10)') i,j,k,b4e2t_en,b4e2t_tn,afe2t_en,afe2t_tn,aft2e_en,aft2e_tn, &
         !                                 afe2t_enn,afe2t_tnn,gf(i,j,k,ih),T(i,j,k)
         !333   continue
   !endif

   IF (T(I,J,K).GT.T_MX) THEN
      T(I,J,K)=T_MX
      !CALL ENTH(I,J,K,1)  !  8/1/05 separated ENTH into 2 routines
      call T_to_enth(i,j,k) ! calculates enthalpy from temperature
   ELSEIF (T(I,J,K).LT.T_MN) THEN
      T(I,J,K)=T_MN
      !CALL ENTH(I,J,K,1)  !  8/1/05 separated ENTH into 2 routines
      call T_to_enth(i,j,k) ! calculates enthalpy from temperature
   ENDIF

   if (irad_test==1) then
      !Temporarily force volume T to 1500 K
      T(i,j,k)=1500.0D+0/t0
   endif

   P(I,J,K)=P(I,J,K)-G0
   CALL BOUND(P(I,J,K),0,1)
   !CSL         G1=DNST(I,J,K)      
   !CALL WTDN(I,J,K,WMIX)
   call dnst_calc(i,j,k)
   !CSL         DNST(I,J,K)=RF(18)*DNST(I,J,K)+RFC(18)*G1
   CALL THER(I,J,K)
enddo;enddo;enddo

         !goto 344
         !close(83) !debugging
         !close(84) !debugging
         !if (itr_gas == 8) then
         !   ijk=1
         !endif
         !344   continue


if (iTave>0) then
   call T_ave_calc
   !write(6,*)'Tave = ',Tave
   !write(nu_Tave,*) itr_gas,Tave
   !write(nu_Tave,*) itr_gas,Tave,Tave_vol
   if (i1st_rad_done==1) then
      write(nu_Tave,"(i5,3e22.14)") itr_gas,Tave,Tmean_ex,avg_wall_T
   else
      write(nu_Tave,"(i5,2e22.14)") itr_gas,Tave,Tmean_ex
   endif
endif

CALL EXTR(1)
CALL INTP
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     WALL FUNCTION (TURB GAMA FACTOR)
!======================================================================

DOUBLE PRECISION FUNCTION TGAMF(B)
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (B.LT.11.5D+0) THEN
   TGAMF=1.0D+0
ELSE
   G0=B/(LOG(9*B)*2.5D+0)
   TGAMF=MAX(G0,1.0D+0)
ENDIF
RETURN
END

