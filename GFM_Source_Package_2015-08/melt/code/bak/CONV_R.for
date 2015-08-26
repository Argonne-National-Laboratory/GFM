!======================================================================
! conv_r.for - handle second heat flux file for regenerative furnaces
!======================================================================
!
! nc1=1 when conv is called from sbc
!
!       reads surface heat flux & interpolates from combustion to melt grid
!       sums up total energy going into melt, stops if sum < 1 MW
!
! ncl=2 when conv is called from sprint
!
!-----------------------------------------------------------------------
      subroutine conv_heat_flux_regen_old
      !SUBROUTINE CONV_R(NC1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !REAL*8,ALLOCATABLE :: QRSC(:,:)
      REAL*8,ALLOCATABLE :: QRS1(:,:)
      REAL*8,ALLOCATABLE :: TGS1_R(:,:),TGS_R(:,:)
      INTEGER,ALLOCATABLE :: NTG(:),NTG1(:)
1     FORMAT (A32)
11    FORMAT (F5.2,2I5)
21    FORMAT (16I5)
22    FORMAT (F5.2,15I5)
      !IF (NC1.GT.1) GOTO 400 !Lottes: not used. The regenerative furnace
	!uses a mean quasi-steady melt computation with only one temperature
      FILENAME='IT'//RUNUM_R//'M.DAT'
      OPEN(10,FILE=FILENAME)
	READ (10,'(T30,I5)') ITN_R
	READ (10,'(T30,F10.0)') Q_IN_R
	READ (10,'(T30,F10.0)') Q_LS_R
	READ (10,'(T30,F10.0)') QLS_S_R
	QRS0_R=0
      IF (ITN_R.GT.1) GOTO 110
	K=LPM1
	DO 60 I=2,I_ME,2
	ID2=I/2
	DO 60 J=2,NP,2
	   IF (IBCELL(I,J,K).EQ.4) THEN
            G0=AREA_C(I,J,K,3)*R0*R0
	      JD2=J/2
	      QRS(ID2,JD2)=G0
            QRS0_R=QRS0_R+G0
	   ENDIF
60    CONTINUE
      IF (ITN_R.EQ.0) GO TO 270
110	READ (10,'(T8,2I4)') MP_C_R,NP_C_R
	ALLOCATE (X_C_R(MP_C_R),R_C_R(NP_C_R))
	MZ_C_R=MP_C_R/2
	NZ_C_R=NP_C_R/2
	READ (10,'(I5,E15.8)') (I1,X_C_R(I),I=1,MP_C_R)
	DO I=6,MP_C_R-3
	   X_C_R(I)=(X_C_R(I)-X_C_R(5))/R0
	ENDDO
	X_C_R(1:5)=0
	X_C_R(MP_C_R-2:MP_C_R)=X_C_R(MP_C_R-3)
	READ (10,'(I5,E15.8)') (J1,R_C_R(J),J=1,NP_C_R)
	DO J=6,NP_C_R-3
	   R_C_R(J)=(R_C_R(J)-R_C_R(5))/R0
	ENDDO
	R_C_R(1:5)=0
	R_C_R(NP_C_R-2:NP_C_R)=R_C_R(NP_C_R-3)
      IF (ITN_R.LE.1) GOTO 270  !ABOVE SEEMS OK
C-----------------------------------------------------
	ALLOCATE (QRSC(MZ_C_R,NZ_C_R),QRS1(MZ_C_R,NZ))
	ALLOCATE (QRS_R(MZ,NZ))
      QRSLG=0
      QRSP=0
	DO I=1,2
         READ (10,1) TITLE
	ENDDO
      J1=1
120   J2=J1+10
      J2=MIN(J2,NZ_C_R)
	DO I=1,2
         READ (10,1) TITLE
	ENDDO
      DO ID2=1,MZ_C_R
         READ (10,'(F11.3,11E11.4)') G0,(QRSC(ID2,J),J=J1,J2)
      ENDDO
      J1=J2+1
      IF (J1.LE.NZ_C_R) GOTO 120
      CLOSE(10)
221	K=LP
	J0=11
	DO 220 I=6,MP_C_R-2,2
	   ID2=I/2
	   QRS1(ID2,1)=0
	   DO J=1+J0,NP-2,2
	      JD2=J/2
	      Y0=R(J)-R(J0)
	      IF (Y0.GT.R_C_R(NP_C_R)) Y0=R_C_R(NP_C_R)
	      F1=1
	      DO J1=2,NP_C_R-2,2
	         J2=J1+2
	         DY1=R_C_R(J2)-R_C_R(J1)
	         IF (Y0.LE.R_C_R(J2).AND.DY1.GT.ZERO) THEN
	            F1=(R_C_R(J2)-Y0)/DY1
                  GOTO 210
	         ENDIF
	      ENDDO
210	      F2=ONE-F1
            IF (QRSC(ID2,J1/2).LE.ZERO) THEN
	         QRS1(ID2,JD2)=QRSC(ID2,J2/2)
            ELSEIF (QRSC(ID2,J2/2).LE.ZERO) THEN
	         QRS1(ID2,JD2)=QRSC(ID2,J1/2)
	      ELSE
	         QRS1(ID2,JD2)=F1*QRSC(ID2,J1/2)+F2*QRSC(ID2,J2/2)
	      ENDIF
	   ENDDO
	   QRS1(ID2,NZ)=0
220   CONTINUE
	DO 240 J=2,NP-2,2
	   JD2=J/2
	   QRS_R(1,JD2)=0
	   DO I=6,MP,2
	      ID2=I/2
	      X0=X(I)
	      IF (X0.GT.X_C_R(MP_C_R)) X0=X_C_R(MP_C_R)
	      F1=1
	      DO I1=2,MP_C_R-2,2
	         I2=I1+2
	         DX1=X_C_R(I2)-X_C_R(I1)
	         IF (X0.LE.X_C_R(I2).AND.DX1.GT.ZERO) THEN
	            F1=(X_C_R(I2)-X0)/DX1
                  GOTO 230
	         ENDIF
	      ENDDO
230	      F2=ONE-F1
            IF (QRS1(I1/2,JD2).LE.ZERO) THEN
	         QRS_R(ID2,JD2)=QRS1(I2/2,JD2)
            ELSEIF (QRS1(I2/2,JD2).LE.ZERO) THEN
	         QRS_R(ID2,JD2)=QRS1(I1/2,JD2)
            ELSE
	         QRS_R(ID2,JD2)=F1*QRS1(I1/2,JD2)+F2*QRS1(I2/2,JD2)
            ENDIF
	   ENDDO
240   CONTINUE
	DEALLOCATE (QRSC,QRS1)
      QRS0_R=0
	K=LP
	DO 260 I=2,MP-2,2
	ID2=I/2
	DO 260 J=2,NP,2
	   JD2=J/2
	   IBC0=MOD(IBCELL(I,J,K),10)
	   IBC1=IBCELL(I,J,K-2)
	   IF (IBC0.EQ.4.AND.IBC1.LE.0) THEN
CBG            QRS(ID2,JD2)=1
            G0=QRS_R(ID2,JD2)*AREA_C(I,J,K,3)*R0*R0
            QRS_R(ID2,JD2)=G0
            QRS0_R=QRS0_R+G0
	   ELSE
            QRS_R(ID2,JD2)=0
	   ENDIF
260   CONTINUE
cbg      IF (QRS0_R.LE.1.0D+6) STOP
270   FACQ=QLS_S/QRS0_R
	FLXH(3,5)=H_TOT
      QRS0_R=0
	DO 300 I=4,MP-2,2
	   IP1=I+1
         FLXH(IP1,5)=FLXH(I-1,5)
	   IF (I.GT.I_ME) EXIT
	   ID2=I/2
	   DO 290 J=2,NP,2
	      IF (IBCELL(I,J,LPM1).NE.4) CYCLE
	      JD2=J/2
            G0=QRS_R(ID2,JD2)*FACQ
            QRS_R(ID2,JD2)=G0
            QRSLG(ID2,JD2)=G0
	      FLXH(IP1,5)=FLXH(IP1,5)+G0
            QRS0_R=QRS0_R+G0
290      CONTINUE
300   CONTINUE
      ! Lottes: This loop averages the melt surface heat flux
	! Distribution from the two combustion space runs
	! for the two firing modes of the regen furnace
	DO 311 I=4,MP-2,2
	   DO 291 J=2,NP,2
	      IF (IBCELL(I,J,LPM1).NE.4) CYCLE
	      JD2=J/2
            G0=QRS_R(ID2,JD2)+QRS(ID2,JD2)
            QRS(ID2,JD2)=G0/TWO
291      CONTINUE
311   CONTINUE
      RETURN
	end

!======================================================================
!     Lottes: this routine interpolates the temperature distribution
!             to the 2nd grid of a regenerative furnace simulation
!             for use in feeding back temperture to the combustion space
!             for the second burner cycle 
!             note that there is only one temperature distribution,
!             but the node points might be different in the 2nd
!             combustion grid.
!
C----------------------------------------------------------------------
      subroutine conv_surf_temp_regen
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !REAL*8,ALLOCATABLE :: QRSC(:,:)
      REAL*8,ALLOCATABLE :: QRS1(:,:)
      REAL*8,ALLOCATABLE :: TGS1_R(:,:),TGS_R(:,:)
      INTEGER,ALLOCATABLE :: NTG(:),NTG1(:)
1     FORMAT (A32)
11    FORMAT (F5.2,2I5)
21    FORMAT (16I5)
22    FORMAT (F5.2,15I5)

400   IF (NPHAS.LE.1) RETURN
      iprint_bt=0
	if(iprint_bt==1)then
         FILENAME='BT'//RUNUM//'.DAT'
         NU=10
         OPEN(NU,FILE=FILENAME)
	   J=4
	   K=(LP+2)/2
	   DO 410 I=4,MP-2,2
	      N=LG(I,J,K)%T
	      N1=LG(I,J,K)%T*1.8-460
	      WRITE (NU,11) X(I)*R0,N,N1
410      CONTINUE
         CLOSE (NU)
	endif
C-----------------------------------------------------
	ALLOCATE (TGS1_R(MP_C_R/2,NZ),TGS_R(MP_C_R/2,NP_C_R/2))
	K=LP-2
	KD2=K/2
	DO 440 J=2,NP,2
	   JD2=J/2
	   DO I=2,MP_C_R,2
	      ID2=I/2
	      X0=X_C_R(I)
	      IF (X0.GE.X(MP)) X0=X(MP)
	      F1=1
	      DO I1=2,MP-2,2
	         I2=I1+2
	         IF (X0.LE.X(2)) GOTO 430
	         DX1=X(I2)-X(I1)
	         IF (X0.LE.X(I2).AND.DX1.GT.ZERO) THEN
	            IF (I2.LE.4) THEN
	               F1=0
	            ELSE
	               F1=(X(I2)-X0)/DX1
	            ENDIF
                  GOTO 430
	         ENDIF
	      ENDDO
430	      F2=ONE-F1
	      IF (PC0(I1/2,JD2,KD2)%TH.GT.0.1D+0) THEN
	         T1=PC(I1/2,JD2,KD2,1)%T*T0
	      ELSE
	         T1=LG(I1,J,K)%T
	      ENDIF
	      IF (PC0(I2/2,JD2,KD2)%TH.GT.0.1D+0) THEN
	         T2=PC(I2/2,JD2,KD2,1)%T*T0
	      ELSE
	         T2=LG(I2,J,K)%T
	      ENDIF
	      TGS1_R(ID2,JD2)=F1*T1+F2*T2
	   ENDDO
440   CONTINUE
C-----------------------------------------------------
	ALLOCATE (NTG(NZ))
!	ALLOCATE (NTG1(NZ_C)) ! Lottes 3/15/2005, comment nz_c not defined, ntg1 not used
CSL02
	K0=11
	DO 460 I=2,MP_C_R-2,2
	   ID2=I/2
	   DO J=2,NP_C_R,2
	      JD2=J/2
	      Z0=R_C_R(J)
	      F1=1
	      DO K1=2,LP-2,2
	         K2=K1+2
	         IF (Z0.LE.Z(K0)) GOTO 450
	         DZ1=Z(K2)-Z(K1)
	         IF (Z0.LE.(Z(K2)-Z(K0)).AND.DZ1.GT.ZERO) THEN
	            F1=((Z(K2)-Z(K0))-Z0)/DZ1
                  GOTO 450
	         ENDIF
	      ENDDO
450	      F2=ONE-F1
	      TGS_R(ID2,JD2)=F1*TGS1_R(ID2,K1/2)+F2*TGS1_R(ID2,K2/2)
	   ENDDO
460   CONTINUE
C-----------------------------------------------------
      FILENAME='IT'//RUNUM_R//'T.DAT'
      OPEN(NU,FILE=FILENAME)
	WRITE(NU,'(" No. of Iteration:"I5)') ITN_R
	WRITE(NU,'(" Total Heat (W) =",T30,F20.10)') Q_IN_R
	WRITE(NU,'(" Heat Loss by Radiation (W) =",T30,F20.10)') Q_LS_R
	WRITE(NU,'(" Heat Transfer to Melter (W) =",T30,F20.10)') QLS_S_R
	WRITE(NU,'(1X/T10,"Surface Temperature (K)")')
      IF (ITN_R.EQ.0) GO TO 900
      R_C_R(4)=0.31/2
	DO J=5,NP_C_R
	   R_C_R(J)=R_C_R(J)*R0+0.31
	ENDDO
      X_C_R(4)=0.31/2
	DO I=5,MP_C_R
	   X_C_R(I)=X_C_R(I)*R0+0.31
	ENDDO
      J1=2
500   J2=J1+20
      J2=MIN(J2,NP_C_R)
      WRITE(NU,'(" X / R",T12,11F11.3)') (R_C_R(J),J=J1,J2,2)
	J1D2=J1/2
	J2D2=J2/2
      DO I=2,MP_C_R,2
	   ID2=I/2
         WRITE(NU,'(F11.3,11F11.1)') 
     &    X_C_R(I),(TGS_R(ID2,JD2),JD2=J1D2,J2D2)
      ENDDO
      J1=J2+2
      IF (J1.LE.NP_C_R) GOTO 500
      CLOSE(NU)
C-----------------------------------------------------
	J=NP-2
	JD2=J/2
	DO 520 I=2,MP,2
	   ID2=I/2
	   DO K=2,LP,2
	      KD2=K/2
	      IF (PC0(ID2,JD2,KD2)%TH.GT.0.1D+0) THEN
	         NTG(KD2)=PC(ID2,JD2,KD2,1)%T*T0
	      ELSE
	         NTG(KD2)=LG(I,J,K)%T
	      ENDIF
	   ENDDO
CSL	   WRITE (NU,21) I,(NTG(KD2),KD2=1,LZ)
520   CONTINUE
      CLOSE (NU)
	DEALLOCATE (NTG)
!	DEALLOCATE (NTG1) ! Lottes 3/15/2005 comment out ntg1 not used.
	DEALLOCATE (TGS1_R,X_C_R,R_C_R)
900   RETURN
      END
