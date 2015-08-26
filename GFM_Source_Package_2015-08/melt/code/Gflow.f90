! GFLOW.F90
!     Calculates the liquid glass variables 
!     rev: 8/01
!======================================================================
      SUBROUTINE GFLOW
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !CZ      DATA NV,NTG/10,1/
      DATA NV,NTG/1,1/
!---------------------------------------------------------------------
!     GSOLVE calculates the liquid phase variables
!---------------------------------------------------------------------
      DO 120 I=2,MP
      DO 120 J=2,NP
      DO 120 K=2,LP
         IF (IBCELL(I,J,K).GT.0) GOTO 120
         lg(i,j,k)%a=lg(i,j,k)%mu
120   CONTINUE
      DO 140 I=2,MP
      DO 140 J=2,NP
      DO 140 K=2,LP
         IBC0=IBCELL(I,J,K)/10
         IF (IBC0.EQ.10) THEN
            LG(I,J,K)%A=LG(I-1,J,K)%A
         ELSEIF (IBC0.EQ.11) THEN
            LG(I,J,K)%A=LG(I+1,J,K)%A
         ELSEIF (IBC0.EQ.20) THEN
            LG(I,J,K)%A=LG(I,J-1,K)%A
         ELSEIF (IBC0.EQ.21) THEN
            LG(I,J,K)%A=LG(I,J+1,K)%A
         ELSEIF (IBC0.EQ.30) THEN
            LG(I,J,K)%A=LG(I,J,K-1)%A
         ELSEIF (IBC0.EQ.31) THEN
            LG(I,J,K)%A=LG(I,J,K+1)%A
         ENDIF
140   CONTINUE
      !CZ      NV=20
      DO N=1,NV
      CALL GSLV3V
      !CZ         WRITE(NU,*) N,AVEB
!---------------------------------------------------------------------
!     calculates derived variables
!       PG: relative to the reference node
!---------------------------------------------------------------------
200   I=I_REF
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
      !LG(I,J,K)%P=LG(I1,J1,K1)%P*F1+LG(I2,J2,K2)%P*F2
      G0=LG(I1,J1,K1)%P-P_REF
      DO 220 I=2,MP,2
      DO 220 J=2,NP,2
      DO 220 K=2,LP,2
         IF (IBCELL(I,J,K).GE.1) CYCLE
         !LG(I,J,K)%P=LG(I,J,K)%P-G0
         CALL BOUND(LG(I,J,K)%P,0,1)
220   CONTINUE
         IF (AVEB.LE.BGCON) EXIT
      ENDDO
      !CZ??      IF (LLK.GT.1.AND.AVEB.GT.2.0D-12) GOTO 350
!---------------------------------------------------------------------
!     TG: glass temperature
!     DNST: glass density
!     THER: thermal properties
!---------------------------------------------------------------------
      !Call to solve liquid glass energy equation
      DO N=1,NTG
         CALL GSLV3G
      ENDDO
      !if (igresid>0) write(nu_gres,"(i7,4e25.16)") itr_gas,h_resid,q_global_bal,h_resid_pre
      if (igresid>0) write(nu_gres,"(i7,5e24.16)") nLg,h_resid,p_resid,(u_resid(i),i=1,3)
      if (igcor>0) write(nu_gcor,"(i7,4e24.16)") nLg,pcorr_resid,(uc_mean(i),i=1,3)
      if (igresidp>0) write(nu_gresp,"(i7,5e24.16)") nLg,h_resid_pre,p_resid_pre,(u_resid_pre(i),i=1,3)
      if (igcorp>0) write(nu_gcorp,"(i7,4e24.16)") nLg,pcorr_resid_pre,(uc_mean(i),i=1,3)
         !  @@@@@@@@@@@@@@@@@@@@@@@@  does uc_mean have a pre-solve value?  ???
      AVET=zero
      Tave=zero !Average T in melt: Lottes 5/11/05

      DO 320 I=2,MP,2
      DO 320 J=2,NP,2
      DO 320 K=2,LP,2
         IF (IBCELL(I,J,K).GE.1) GOTO 320 
         T_last_iter=LG(I,J,K)%T !Previous iteration value
         !Lottes 5/7/05
         !Calculate glass temperature from enthalpy and specific heat
         !Enforce temperature bounds and adjust enthalpy if a bound 
         !enforcement occures

		   !Lg(i,j,k)%T=Lg(i,j,k)%h/Lg(i,j,k)%c !********T set here ***************************************
         call h_to_Tlg(Lg(i,j,k)%h,Lg(i,j,k)%T)

			if (Lg(i,j,k)%T > T_mx) then !general high bound
            Lg(i,j,k)%T = T_mx
            !Lg(i,j,k)%h = Lg(i,j,k)%c * Lg(i,j,k)%T
				Lg(i,j,k)%h = h_Tmax
         endif
         if (Lg(i,j,k)%T < Tmltr) then !lowest melt temperature bound
            Lg(i,j,k)%T = Tmltr
            !Lg(i,j,k)%h = Lg(i,j,k)%c * Lg(i,j,k)%T
				Lg(i,j,k)%h = h_Tmltr
         endif
         if (Tmax_c > udf_cl(udf_cln)%T .and. Lg(i,j,k)%T > Tmax_c) then !maximum temperature in combustion space
            Lg(i,j,k)%T = Tmax_c
            Lg(i,j,k)%h = h_pts(udf_cln) + udf_cl(udf_cln)%f * (Tmax_c-udf_cl(udf_cln)%T) !******check 
         endif
         !Tnew=Lg(i,j,k)%T
         !CALL ENTH(I,J,K,2) !Calculation of new T is now above, Lottes 5/11/05
         !if(Lg(i,j,k)%T-Tnew > 1e-13) then
         !  Tnew1=1
         !endif
         if (k==lp-2) surf_T(i/2,j/2)=Lg(i,j,k)%T

         Tave= Tave+Lg(i,j,k)%T*vol_c(i,j,k)

         AVET=AVET+ABS(T_last_iter-LG(I,J,K)%T) 
         CALL DSLG(I,J,K)
         CALL THER(I,J,K)
320   CONTINUE
      if (iTave==1) then
         Tave = Tave/vol_tot
         write(6,*)'Tave = ',Tave
         !write(nu_Tave,*) nLg,Tave
         write(nu_Tave,'(i5,2e22.14)') nLg,Tave,Tmean_ex
      endif
      AVET=AVET/NCELLS !average iteration T change
350   CALL EXTR(1)
      CALL INTP
!CzEB----&-----------------------------------------------------------------
!     GSOLVEEB calculates the electric boost variables
!---------------------------------------------------------------------
      ID_EB=ID_EB+1
      IF (EB_C.AND.ID_EB.EQ.ID_EB1) THEN
         CALL GFLOWEB
         ID_EB1=-2
      ENDIF
      IF (EB_C.AND.ID_EB.EQ.ID_EB0) THEN
         CALL GFLOWEB
         ID_EB=0
      ENDIF
      RETURN
      END

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!     INDEX FOR DIFF. DIRECTION (VINDX)
!---------------------------------------------------------------------
      SUBROUTINE VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3)
      DO II=1,3
         NX(II)=0
      ENDDO
      !      NX=0
      NX(IU)=M
      I1=I+NX(1)
      I2=I1+NX(1)
      ID21=I/2+NX(1)
      IF (NP.GE.6) THEN
         J1=J+NX(2)
         J2=J1+NX(2)
         JD21=J/2+NX(2)
      ELSE
         J1=2
         J2=2
         JD21=1
      ENDIF
      IF (LP.GE.6) THEN
         K1=K+NX(3)
         K2=K1+NX(3)
         KD21=K/2+NX(3)
      ELSE
         K1=2
         K2=2
         KD21=1
      ENDIF
      RETURN
      END
