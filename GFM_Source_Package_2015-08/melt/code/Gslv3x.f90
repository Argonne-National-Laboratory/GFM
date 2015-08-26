! GSLV3X.F90
!
! Computes SIMPLER algorithm pseudo-velocities
!     and  momentum equation coefficients and source terms
!
!     Utility subprograms for GSLV3
!     IU=1: X direction
!        2: R-direction 
!        3: Z-direction
!     rev:  5/01
!======================================================================
      SUBROUTINE GSLVX1(MI1,MI3,NJ1,NJ3,LK1,LK3,IU1)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NX(3),FFL(3,2)
      IF (IU1.EQ.2.AND.NP.LE.6) RETURN
      IF (IU1.EQ.3.AND.LP.LE.6) RETURN
      NX=0
      NX(IU1)=1
      MI1=4+NX(1)
      MI3=MP_E-NX(1)
      NJ1=NJY1V(IU1)
      NJ3=NJY2V(IU1)
      LK1=LKZ1V(IU1)
      LK3=LKZ2V(IU1)
      CALL SOURCE(MI1,MI3,NJ1,NJ3,LK1,LK3)
      DO 150 I=MI1,MI3,2
      DO 150 J=NJ1,NJ3,2
      DO 150 K=LK1,LK3,2
         IF (IBCELL(I,J,K).GE.1) GOTO 150
         AP(I,J,K)=ZERO
         G0=ZERO
         !Call to compute momentum equation coefficients
         CALL GSLV3X1(I,J,K,IU1,G0)
         CALL ECELL(I,J,K,IU1,FFL)
         !Compute SIMPLER algorithm pseudo-velocities
	   !These are saved in the velocity array
	   !The fz holds the real velocities during the pressure solve
         DO 140 IU=1,3
         IF (IU.EQ.IU1) GOTO 140
         !IF (IU.EQ.2.AND.NP.LE.6) GOTO 140 !for old 2d run mode
         !IF (IU.EQ.3.AND.LP.LE.6) GOTO 140 !for old 2d run mode
         DO 130 M=-1,1,2
            M1=(M+3)/2
            CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
            IBC1=MOD(IBCELL(I1,J1,K1),10)
            !CZEB            IF (IBC1.EQ.3.OR.IBC1.EQ.4) THEN
            IF (IBC1.EQ.3.OR.IBC1.EQ.4.OR.IBC1.EQ.5.OR.IBC1.EQ.6) THEN
            !Lottes 6/2/05
            !Try using outlet velocity in solve
            !IF (IBC1.EQ.4.OR.IBC1.EQ.5.OR.IBC1.EQ.6) THEN
               AS(I,J,K,IU,M1)=ZERO
               GOTO 130
            ENDIF
            F=ZERO
            DIFF=ZERO
            DO 125 N=-1,1,2
               N1=(N+3)/2
               II1=I1+NX(1)*N
               JJ1=J1+NX(2)*N
               KK1=K1+NX(3)*N
               IBC11=MOD(IBCELL(II1,JJ1,KK1),10)
               !CZEB               IF (IBC11.EQ.3.OR.IBC11.EQ.4) GOTO 125
      IF (IBC11.EQ.3.OR.IBC11.EQ.4.OR.IBC11.EQ.5.OR.IBC11.EQ.6) GOTO 125
               AREA=FFL(IU,N1)
               F=F+LG(II1,JJ1,KK1)%TH*LG(II1,JJ1,KK1)%DS*FZ(II1,JJ1,KK1)*AREA
               IF (IBC11.EQ.2) GOTO 125
               DDL=FDDL(II1,JJ1,KK1,IU)
               DIFF=DIFF+LG(II1,JJ1,KK1)%TH*LG(II1,JJ1,KK1)%A/SGM*AREA/DDL
125         CONTINUE
            !CSL            FLD=FLD+FLWD1(II1,JJ1,KK1,M1,AREA,DDL)
            IF (M1.EQ.1) THEN
               FLD=DA(DIFF,F)+MAX(ZERO,F)
            ELSE
               FLD=DA(DIFF,F)+MAX(ZERO,-F)
            ENDIF


            IBC2=MOD(IBCELL(I2,J2,K2),10)
            IF (IBCELL(II1,JJ1,KK1).EQ.2.AND.IBC2.EQ.1) THEN
               BS0=FLD*(LG(II1,J2,K2)%U(IU1)-LG(I,J,K)%U(IU1))
               BS(I,J,K)=BS(I,J,K)+BS0
               AS(I,J,K,IU,M1)=ZERO
               GOTO 130
            ENDIF


            AS(I,J,K,IU,M1)=FLD
            AP(I,J,K)=AP(I,J,K)+FLD
            G0=G0+FLD*FZ(I2,J2,K2)
130      CONTINUE
140      CONTINUE
         IF (IU1.EQ.1) THEN
            F2=(X(I)-X(I-1))/DX(I)
         ELSEIF (IU1.EQ.2) THEN
            F2=(y(J)-y(J-1))/dy(J)
         ELSE
            F2=(Z(K)-Z(K-1))/DZ(K)
         ENDIF
         F1=ONE-F2
         I1=(I-NX(1))
         I2=(I+NX(1))
         J1=(J-NX(2))
         J2=(J+NX(2))
         K1=(K-NX(3))
         K2=(K+NX(3))
         PMRM=LG(I1,J1,K1)%MR*F1+LG(I2,J2,K2)%MR*F2
         I1=I1/2
         I2=I2/2
         J1=J1/2
         J2=J2/2
         K1=K1/2
         K2=K2/2
         IF (NBS0.GT.0) THEN
            CONM=CON(I1,J1,K1)*F1+CON(I2,J2,K2)*F2
         ELSE
            CONM=ZERO
         ENDIF
         VOL=VOL1(I,J,K,IU1,FFL)
         BS(I,J,K)=BS(I,J,K)*VOL
         AP(I,J,K)=AP(I,J,K)+(PMRM-CONM-SFP(I,J,K))*VOL
         IF (.NOT.STEADY) THEN
            BS(I,J,K)=BS(I,J,K)+APO(I,J,K)*VOL*LG0(I,J,K)%U(IU1)
            AP(I,J,K)=AP(I,J,K)+APO(I,J,K)*VOL
         ENDIF
         IF (SMPLER.AND.ABS(AP(I,J,K)).GT.SMALL) THEN
            !Sets pseudo-velocities only for interior points
            LG(I,J,K)%U(IU1)=(G0+BS(I,J,K))/AP(I,J,K)
         ENDIF
150   CONTINUE
      RETURN
      END


!======================================================================
      !Called from gslvx1 - Lottes 
	!g0=0 on input
	!Computes coefficients for momentum equation
	!ap up = ae ue + aw uw + an un + as us + at ut + ab ub + b
	!ap here does not include linear slope part of source
      SUBROUTINE GSLV3X1(I,J,K,IU,G0)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      G0=ZERO
      DO M=-1,1,2
         M1=(M+3)/2
         CALL VINDX(IU,M,I,J,K,I1,J1,K1,I2,J2,K2,ID21,JD21,KD21)
         IBC2=MOD(IBCELL(I2,J2,K2),10)
         !CZEB         IF (IBC2.EQ.3.OR.IBC2.EQ.4) THEN
         IF (IBC2.EQ.3.OR.IBC2.EQ.4.OR.IBC2.EQ.5.OR.IBC2.EQ.6) THEN
         !Lottes 6/2/05, try using velocity at boundary in solve.
         !IF (IBC2.EQ.4.OR.IBC2.EQ.5.OR.IBC2.EQ.6) THEN
            AS(I,J,K,IU,M1)=ZERO
         !elseif(ibc2==3)then !test
            !AS(I,J,K,IU,M1)=FLW(I2,J2,K2,IU,M1)
         ELSEIF (IBC2.EQ.2) THEN
            AS(I,J,K,IU,M1)=FLW(I2,J2,K2,IU,M1)
         ELSE
            AS(I,J,K,IU,M1)=FLWD(I1,J1,K1,IU,M1)
         ENDIF
         AP(I,J,K)=AP(I,J,K)+AS(I,J,K,IU,M1)
         G0=G0+AS(I,J,K,IU,M1)*FZ(I2,J2,K2)
      ENDDO
      RETURN
      END


!======================================================================
!        IF OUTFLOW BOUNDARY OR NEXT MOMENTUM CELL BLOCKED,
!        EXTEND CELL
!======================================================================
      SUBROUTINE ECELL(I,J,K,IU,FFL)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FFL(3,2)
      IF (IU.EQ.1) THEN
         I1=I-1
         I2=I-2
         DO M=1,2
            IF (IBCELL(I2,J,K).GE.1) THEN
               FFL(1,M)=DX(I1)
            ELSE
               FFL(1,M)=ABS(X(I)-X(I1))
            ENDIF
            FFL(2,M)=FFL(1,M)*DZ(K)
            FFL(3,M)=FFL(1,M)*dy(J)
            I1=I+1
            I2=I+2
         ENDDO
      ELSEIF (IU.EQ.2) THEN
         J1=J-1
         J2=J-2
         DO M=1,2
            IF (IBCELL(I,J2,K).GE.1) THEN
               FFL(2,M)=dy(J1)
            ELSE
               FFL(2,M)=ABS(y(J)-y(J1))
            ENDIF
            FFL(1,M)=FFL(2,M)*DZ(K)
            FFL(3,M)=FFL(2,M)*DX(I)
            J1=J+1
            J2=J+2
         ENDDO
      ELSE
         K1=K-1
         K2=K-2
         DO M=1,2
            IF (LP.LT.6) THEN
               K2=2
               K1=1
            ENDIF
            IF (IBCELL(I,J,K2).GE.1) THEN
               FFL(3,M)=DZ(K1)
            ELSE
               FFL(3,M)=ABS(Z(K)-Z(K1))
            ENDIF
            FFL(1,M)=FFL(3,M)*dy(J)
            FFL(2,M)=FFL(3,M)*DX(I)
            K1=K+1
            K2=K+2
         ENDDO
      ENDIF
      RETURN
      END


!======================================================================
!        Diffusivity length
!======================================================================
      DOUBLE PRECISION FUNCTION FDDL(I,J,K,IU)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (IU.EQ.1) THEN
         IF (IBCELL(I,J,K).LE.0) THEN
            DDL=DX(I)
         ELSEIF (IBCELL(I,J,K).GE.1) THEN
            IF (IBCELL(I+1,J,K).EQ.0) THEN
               DDL=X(I+1)-X(I)
            ELSEIF (IBCELL(I-1,J,K).EQ.0) THEN
               DDL=X(I)-X(I-1)
            ELSE
               DDL=BIG
            ENDIF
         ELSE
            DDL=BIG
         ENDIF
      ELSEIF (IU.EQ.2) THEN
         IF (IBCELL(I,J,K).LE.0) THEN
            DDL=dy(J)
         ELSEIF (IBCELL(I,J,K).GE.1) THEN
            IF (IBCELL(I,J+1,K).EQ.0) THEN
               DDL=(y(J+1)-y(J))
            ELSEIF (IBCELL(I,J-1,K).EQ.0) THEN
               DDL=(y(J)-y(J-1))
            ELSE
               DDL=BIG
            ENDIF
         ELSE
            DDL=BIG
         ENDIF
      ELSE
         IF (IBCELL(I,J,K).LE.0) THEN
            DDL=DZ(K)
         ELSEIF (IBCELL(I,J,K).GE.1) THEN
            IF (IBCELL(I,J,K+1).EQ.0) THEN
               DDL=Z(K+1)-Z(K)
            ELSEIF (IBCELL(I,J,K-1).EQ.0) THEN
               DDL=Z(K)-Z(K-1)
            ELSE
               DDL=BIG
            ENDIF
         ELSE
            DDL=BIG
         ENDIF
      ENDIF
      !CSL      IF(IBCELL(I,J,K).LT.0.AND.IU.NE.1) DDL=DDL/TWO
      FDDL=DDL
      RETURN
      END


!======================================================================
      DOUBLE PRECISION FUNCTION VOL1(I,J,K,IX,FFL)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FFL(3,2)
      AREA=AREA_C(I,J,K,IX)                     
      IF (IX.EQ.1) THEN
         VOL=(FFL(IX,1)+FFL(IX,2))*AREA
         IF (IBCELL(I-1,J,K).LT.0) VOL=VOL-DX(I-1)*AREA/TWO 
         IF (IBCELL(I+1,J,K).LT.0) VOL=VOL-DX(I+1)*AREA/TWO 
      ELSEIF (IX.EQ.2) THEN
         VOL=(FFL(IX,1)+FFL(IX,2))*AREA
         IF (IBCELL(I,J-1,K).LT.0) VOL=VOL-dy(J-1)*AREA/TWO 
         IF (IBCELL(I,J+1,K).LT.0) VOL=VOL-dy(J+1)*AREA/TWO 
      ELSE
         VOL=(FFL(IX,1)+FFL(IX,2))*AREA
         IF (IBCELL(I,J,K-1).LT.0) VOL=VOL-DZ(K-1)*AREA/TWO 
         IF (IBCELL(I,J,K+1).LT.0) VOL=VOL-DZ(K+1)*AREA/TWO 
      ENDIF
      VOL1=VOL
      RETURN
      END


!======================================================================
      DOUBLE PRECISION FUNCTION FLW(I,J,K,IX,M)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      AREA=AREA_C(I,J,K,IX)
      F=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(IX)*AREA
      IF (M.EQ.1) THEN
         FLW=MAX(ZERO,F)
      ELSE
         FLW=MAX(ZERO,-F)
      ENDIF
      RETURN
      END


!======================================================================
      DOUBLE PRECISION FUNCTION FLWD(I,J,K,IX,M)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      AREA=AREA_C(I,J,K,IX)                                      
      IF (IX.EQ.1) THEN
         DDL=DX(I)
      ELSEIF (IX.EQ.2) THEN
         DDL=dy(J)
      ELSE
         DDL=DZ(K)
      ENDIF
      !CSL      IF(IBCELL(I,J,K).LT.0.AND.IX.NE.1) DDL=DDL/TWO
      F=LG(I,J,K)%TH*LG(I,J,K)%DS*LG(I,J,K)%U(IX)*AREA
      DIFF=LG(I,J,K)%TH*LG(I,J,K)%A/SGM*AREA/DDL
      IF (M.EQ.1) THEN
         FLWD=DA(DIFF,F)+MAX(ZERO,F)
      ELSE
         FLWD=DA(DIFF,F)+MAX(ZERO,-F)
      ENDIF
      RETURN
      END
