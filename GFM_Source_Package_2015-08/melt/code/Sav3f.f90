!======================================================================
! SAV3F.F90
!    SAV3F saves data on disk for restart capability
!       GF: general property functions
!           enthalpy (1),
!        LEND: end of GF calculation
!     Rev: 6/01
!======================================================================
      SUBROUTINE SAV3F
      !SUBROUTINE SAV3F(ISV)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 U_0(3)
      !IF (ISV.EQ.2) GOTO 200
      filename=casedir//'\rg'//runum//'m.d'
      OPEN(nu_restg,FILE=FILENAME,FORM='UNFORMATTED')
      DO 300 I=2,MP,2
      I1=I+1
      ID2=I/2
      IF (I.EQ.MP) I1=MPM1
      DO 300 J=2,NP,2
      J1=J+1
      IF (J.EQ.NP) J1=NPM1
      JD2=J/2
      DO 300 K=2,LP,2
         KD2=K/2
         G1=LG(I,J,K)%P
         IF (I.EQ.MP) I1=MPM1
         G2=LG(I1,J,K)%U(1)
         G3=LG(I,J1,K)%U(2)
         K1=K+1
         IF (K.EQ.LP) K1=LPM1
         G4=LG(I,J,K1)%U(3)
         G5=LG(I,J,K)%T
         G6=LG(I,J,K)%TH
         G7=LG(I,J,K)%DS
         G8=EBV(I,J,K)
         WRITE (nu_restg) G1,G2,G3,G4,G5,G6,G7,G8
         WRITE (nu_restg) (LG(I,J,K)%F(L),L=1,LEND+1)
         IF (K.NE.LPM2.OR.NPHAS.EQ.1) GOTO 230
         !IF (NPS0.GE.1) WRITE (nu_restg) SP(I,J,1:2) !Lottes 3-23-2005 ?check
         ! Changed by Golchert fall 2004, does not match read back (initsv) 
         IF (NPS0.GE.1) WRITE (nu_restg) SP(I,J,K)
         IF (NPS_C.LE.0) GOTO 220
         WRITE (nu_restg) PC0(ID2,JD2,KD2)
         DO L=1,NPS_C
            G1=PC(ID2,JD2,KD2,L)%T
            G2=PC(ID2,JD2,KD2,L)%DN
            G3=PC(ID2,JD2,KD2,L)%MR
            G4=QRSP_C(ID2,JD2,L)
            U_0(1:3)=PC(ID2,JD2,KD2,L)%U(1:3)
            WRITE (nu_restg) G1,G2,G3,G4,U_0
         ENDDO
220      IF (NPS_S.LE.0) CYCLE
         WRITE (nu_restg) PS0(ID2,JD2,KD2)
         DO L=1,NPS_S
            G1=PS(ID2,JD2,KD2,L)%T
            G2=PS(ID2,JD2,KD2,L)%DN
            G3=PS(ID2,JD2,KD2,L)%MR
            G4=QRSP_S(ID2,JD2,L)
            U_0(1:3)=PS(ID2,JD2,KD2,L)%U(1:3)
            WRITE (nu_restg) G1,G2,G3,G4,U_0
         ENDDO
230      IF (NBS0.LE.0) CYCLE
         WRITE (nu_restg) GB3(ID2,JD2,KD2)%TH
         DO L=1,NBS0
            G1=GB4(ID2,JD2,KD2,L)%T
            G2=GB4(ID2,JD2,KD2,L)%DN
            U_0(1:3)=GB4(ID2,JD2,KD2,L)%U(1:3)
            WRITE (nu_restg) G1,G2,U_0
         ENDDO
300   CONTINUE
      !retreive surface heat flux from possibly iteratively scaled surface heat rate
      if (regen==0) then
         do j=4,np-2,2
         do i=4,i_me,2
            if (ibcell(i,j,lp).ne.4) cycle
            qrs_prev(i/2,j/2) = qrs(i/2,j/2)/(dx(i)*dy(j))
         enddo;enddo
      else 
         !Have regenerative furnace, need to save flux appropriate to both
         !combustion spaces (cannot use an averaged value)
         scale_in=(h_needed-eb_heat+q_wall_loss_tot+scale_out*qs_out_1)/qs_in_base_1
         !facq  =(h_needed-eb_heat+q_wall_loss_tot)/qrs_tot_base_comb1
         scale_in_r=(h_needed-eb_heat+q_wall_loss_tot+scale_out_r*qs_out_r)/qs_in_r
         !facq_r=(h_needed-eb_heat+q_wall_loss_tot)/qrs_tot_r
         do j=4,np-2,2
         do i=4,i_me,2
            if (ibcell(i,j,lp).ne.4) cycle
            qrs_prev(i/2,j/2)   = qrs_m(i/2,j/2)*scale_in
            qrs_prev_r(i/2,j/2) = qrs_m_r(i/2,j/2)*scale_in_r
         enddo;enddo    
      endif
      write (nu_restg) qrs_prev !save surface heat flux for relaxation
      if (regen==1) write (nu_restg) qrs_prev_r !save surface heat flux for relaxation (regen)

      write (nu_restg) q_wall_loss_tot !(W)
      WRITE (nu_restg) EBQ
      !P_RF=zero !Lottes 5/18/05: not used.
      !WRITE (nu_restg) FACTOR,FADJ,GFIN,GFEX,P_RF
      WRITE (nu_restg) FACTOR,FADJ,GFIN,GFEX
      WRITE (nu_restg) NLG,N_PI,LRI
      WRITE (nu_restg) F_BUOY !Lottes 5/18/05, need to write it if we read it in initsv!
900   CLOSE (nu_restg)
      RETURN

      end

!======================================================================

      subroutine save_minor_species
      use gbl_var
      implicit double precision (a-h,o-z)

!======================================================================

      filename=casedir//'\rs'//runum//'c.d'
      OPEN(nu_rests,FILE=FILENAME,FORM='UNFORMATTED')
      DO I=2,MP,2
      DO J=2,NP,2
      DO K=2,LP,2
         WRITE (nu_rests) (GFM(I,J,K,L),L=1,NMSP),SVF(I,J,K)
      enddo;enddo;enddo

      WRITE (nu_rests) RRMS
      CLOSE (nu_rests)
      RETURN
      END
