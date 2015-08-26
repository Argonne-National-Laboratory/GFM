!======================================================================
! INITSV.F90
!     INITSV sets initial assumed distribution of variables
!     Rev: 6/01
!======================================================================
      SUBROUTINE INITSV
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 GFF(9),U_0(3)
1     FORMAT (A40)
!----------------------------------------------------------------------

      filename=casedir//'\rg'//runum//'m.d'
      INQUIRE (FILE=FILENAME,EXIST=EXST)
      !IF (IRSTYP.GE.1.AND.EXST) GOTO 200
      IF (IRSTYP < 1 .or. (.not.EXST)) then
         !----------------------------------------------------------------------
         !        IRSTYP=0, start without reading backup file
         !----------------------------------------------------------------------
         irstyp=0
         FACTOR=ONE
         FADJ=ONE
         LRI=LRX
         DO I=2,MP
         DO J=2,NP
         DO K=2,LP
            IBC0=MOD(IBCELL(I,J,K),10)
            IF (IBC0.EQ.2.OR.IBC0.EQ.1) cycle
            !LG(I,J,K)%T=1
            LG(I,J,K)%T=max(T_init,Tmltr) !Lottes 7/16/05: allow for user input initial guess
            !LG(I,J,K)%T=T_exit !Lottes 4/14/05: try different initial guess
            CALL DSLG(I,J,K)
            CALL THER(I,J,K)
            !Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
            call Tlg_to_h(Lg(i,j,k)%T,Lg(i,j,k)%h) !Lottes 5/11/05 set h consistent with T and c.
            !CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
         
            LG(I,J,K)%F(IYLG)=1
         enddo;enddo;enddo
         
         RETURN
      endif
!----------------------------------------------------------------------
!        IRSTYP=1, resume steady-state iteration from a restart file
!----------------------------------------------------------------------
!     Read stored data
!         GF: general property functions
!             liquid glass (IYLG)
!          LSTAR: starting index for general calculation
!          LEND:  end index general calculation
!----------------------------------------------------------------------
      OPEN(nu_restg,FILE=FILENAME,FORM='UNFORMATTED')
      DO 300 I=2,MP,2
      I1=I+1
      ID2=I/2
      DO 300 J=2,NP,2
      J1=J+1
      JD2=J/2
      DO 300 K=2,LP,2
         READ (nu_restg) G1,G2,G3,G4,G5,G6,G7,G8
         READ (nu_restg) (GFF(L),L=1,LEND+1)
         LG(I,J,K)%P=G1
         IBC0=MOD(IBCELL(I,J,K),10)
         IF (IBC0.EQ.2) GOTO 220
         IF (I.LT.MP) THEN
            IBC1=MOD(IBCELL(I1,J,K),10)
            IF (IBC1.NE.2.AND.IBC1.NE.1) LG(I1,J,K)%U(1)=G2
         ENDIF
         IF (J.LT.NP) THEN
            IBC1=MOD(IBCELL(I,J1,K),10)
            IF (IBC1.NE.2.AND.IBC1.NE.1) LG(I,J1,K)%U(2)=G3
         ENDIF
         K1=K+1
         IF (K.LT.LP) THEN
            IBC1=MOD(IBCELL(I,J,K1),10)
            IF (IBC1.NE.2.AND.IBC1.NE.1) LG(I,J,K1)%U(3)=G4
         ENDIF
         LG(I,J,K)%T=max(G5,Tmltr)
         CALL DSLG(I,J,K)
         CALL THER(I,J,K)
         call Tlg_to_h(Lg(i,j,k)%T,Lg(i,j,k)%h) !Lottes 5/11/05 set h consistent with T and c.
         !Lg(i,j,k)%h=Lg(i,j,k)%c * max(Lg(i,j,k)%T,Tmltr) !Lottes 5/11/05 set h consistent with T and c.
         !CALL ENTH(I,J,K,1) !Lottes 5/11/05 above statement = effect of enth routine
         LG(I,J,K)%TH=G6
         LG(I,J,K)%DS=G7
         !cz         EBV(I,J,K)=G8
         EBV(I,J,K)=G8 !Lottes /5/18/05; should restore machine state
         DO L=1,LEND+1
            LG(I,J,K)%F(L)=GFF(L)
         ENDDO
220      CONTINUE
         KD2=K/2
         IF (K.NE.LPM2.OR.NPHAS.EQ.1) GOTO 250
         IF (NPS0.GE.1) READ (nu_restg) SP(I,J,K)
         IF (NPS_C.LE.0) GOTO 240
         READ (nu_restg) PC0(ID2,JD2,KD2)
         DO L=1,NPS_C
            READ (nu_restg) G1,G2,G3,G4,U_0
            IF (IBC0.EQ.2) CYCLE
            PC(ID2,JD2,KD2,L)%T=G1
            PC(ID2,JD2,KD2,L)%DN=G2
            PC(ID2,JD2,KD2,L)%MR=G3
            !CZ            QRSP_C(ID2,JD2,L)=G4
            QRSP_C(ID2,JD2,L)=G4 !Lottes 5/18/05 put it back!
            PC(ID2,JD2,KD2,L)%U(1:3)=U_0(1:3)
         ENDDO
         if(k==lp-2) Cullet_T(id2,jd2)=pc(id2,jd2,lz-1,1)%T
240      IF (NPS_S.LE.0) CYCLE
         READ (nu_restg) PS0(ID2,JD2,KD2)
         DO L=1,NPS_S
            READ (nu_restg) G1,G2,G3,G4,U_0
            IF (IBC0.EQ.2) CYCLE
            PS(ID2,JD2,KD2,L)%T=G1
            PS(ID2,JD2,KD2,L)%DN=G2
            PS(ID2,JD2,KD2,L)%MR=G3
            !CZ            QRSP_S(ID2,JD2,L)=G4
            QRSP_S(ID2,JD2,L)=G4 !Lottes 5/18/05 put it back!
            PS(ID2,JD2,KD2,L)%U(1:3)=U_0(1:3)
         ENDDO
         if(k==lp-2) Sand_T(id2,jd2)=ps(id2,jd2,lz-1,1)%T
250      IF (NBS0.LE.0) CYCLE
         READ (nu_restg) GB3(ID2,JD2,KD2)%TH
         DO L=1,NBS0
            READ (nu_restg) G1,G2,U_0
            IF (IBC0.NE.0) CYCLE
            GB4(ID2,JD2,KD2,L)%T=G1
            GB4(ID2,JD2,KD2,L)%DN=G2
            GB4(ID2,JD2,KD2,L)%U(1:3)=U_0
         ENDDO
300   CONTINUE
         !CZ       DO 400 I=2,MP
         !CZ       DO 400 K=2,LP
         !CZCZ         READ (nu_restg) QRSLG(ID2,JD2)
         !CZ         READ (nu_restg) G0
400      CONTINUE
!---------------------------------------------------------------------
!     Interpolation and extrapolation of variables calculated
!        INTPV: velocity
!        INTP:  general properties
!---------------------------------------------------------------------
      read (nu_restg) qrs_prev !read previous surface heat flux for relaxation
      if (regen==1) then
         read (nu_restg) qrs_prev_r !read previous regen surface heat flux for relaxation
      endif
      read (nu_restg) q_wall_loss_tot !(W)
      READ (nu_restg) EBQ
      CALL QCOND !Lottes: calculate conduction to particles
      CALL qcond_sum !Lottes: total conduction to particles over size groups
      READ (nu_restg) FACTOR,FADJ,GFIN,GFEX
      READ (nu_restg,ERR=420) NLG,N_PI,LRI
      READ (nu_restg,ERR=420) F_BUOY
420   CLOSE(nu_restg)
      NLG0=NLG
      CALL INTPV
      CALL EXTR(1)
!---------------------------------------------------------------------
      IF (ID_RAD.GE.0) THEN
         filename=casedir//'\rr'//runum//'m.d'
         OPEN(nu_restr,FILE=FILENAME,FORM="UNFORMATTED",ERR=900)
         READ (nu_restr,ERR=900) QE
         READ (nu_restr,ERR=900) QA
         READ (nu_restr,ERR=900) QEG0,QEG_G,QEG_W,QEG_S,QEG_IO
         READ (nu_restr,ERR=900) QEW0,QEW_G,QEW_W,QEW_S,QEW_IO
         READ (nu_restr,ERR=900) QES0,SVF_R,QA_S0,QA_S1
         CLOSE(nu_restr)
      ENDIF

      CALL INTP
      RETURN

900   continue
      call stop_run("Error reading RR....M.D.")
      return
      end

!======================================================================

      SUBROUTINE INIT_minor_species
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!======================================================================
      !cz----------------
      !IF (MS.NE.1) GO TO 600
      IF (IRSTYPM.EQ.0.OR..NOT.EXST) THEN
         GFM=ZERO
         DO 550 I=2,MP,2
         DO 550 J=2,NP,2
         DO 550 K=2,LP,2
            !cz            IF (LG(I,J,K)%MR.GT.ZERO) THEN
            !CZ??        IF(IBCELL(I,J,K).EQ.2) GFM(I,J,K,1)=ONE
            !cz           DO L=1,NMSP
            !cz               GFM(I,J,K,L)=ONE
            !cz             END DO
            !cz            ENDIF
            !cz    go to 550
          IF (IBCELL(I,J,K).EQ.2) THEN
            IF (K.LT.30) then
            GFM(I,J,K,1)=0.6D+0
            GFM(I,J,K,2)=0.4D+0
            endif
            IF (K.GT.30) then
            GFM(I,J,K,1)=0.4D+0
            GFM(I,J,K,2)=0.6D+0
            endif
         ENDIF
550      CONTINUE
         RRMS=ZERO
      ELSE
         filename=casedir//'\rs'//runum//'m.d'
         OPEN(nu_rests,FILE=FILENAME,FORM='UNFORMATTED')
         DO 580 I=2,MP,2
         DO 580 J=2,NP,2
         DO 580 K=2,LP,2
            READ (nu_rests) (GFM(I,J,K,L),L=1,NMSP)
580      CONTINUE
         READ (nu_rests) RRMS
         CLOSE(nu_rests)
         CALL EXTR(4)
      ENDIF

      CALL INTP_minor_species
      RETURN

      END

