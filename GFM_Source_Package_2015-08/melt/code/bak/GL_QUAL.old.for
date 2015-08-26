C---
C     Calculate GLASS QUALITY INDICES
C---
      SUBROUTINE GL_QUAL
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IQUAL(50)
C---
C     START WITH PARTICLE TRACING
C           INCREMENT EACH MILLIMETER IN X AND Y 
C           AND TRACE FROM EACH CELL THAT HAS BATCH
C---
      return
      X_MIN=X(5)
	X_MAX=X(MP)
	Y_MIN=R(5)
	Y_MAX=R(NP)
	Z_MIN=Z(5)
	Z_MAX=Z(LP)
	IXSKIP=0
	IYSKIP=0
	IZSKIP=0
	NUM_SEEDS=0
	DEL_T=6.0D+1
	AVERAGE_SEED=ZERO
	TOT_SEEDTIME=ZERO
	SEEDMAX=ZERO
	SEEDMIN=1.0E+5
	NUM_OUT=ZERO
	IS_MAX=2
	JS_MAX=2
	KS_MAX=2
	IS_MIN=2
	JS_MIN=2
	KS_MIN=2
	XQ_MAX=ZERO
	YQ_MAX=ZERO
	XQ_MIN=ZERO
	YQ_MIN=ZERO
	IQUAL=ZERO
	DO 10 I=4,MP,2
	DO 10 J=4,NP,2
	DO 10 K=4,LP,2	   
	   IBC0=IBCELL(I,J,K)
	   IBC0X=IBCELL(I-2,J,K)
	   IBC0Y=IBCELL(I,J-2,K)
	   IBC0Z=IBCELL(I,J,K-2)
	   IF (IBC0.EQ.0.AND.IBC0X.EQ.1.AND.IXSKIP.EQ.0) THEN
	     X_MIN=X(I-1)
	     IXSKIP=1
	   ENDIF
	   IF (IBC0.EQ.1.AND.IBC0X.EQ.0) X_MAX=X(I-1) 
	   IF (IBC0.EQ.0.AND.IBC0Y.EQ.1.AND.IYSKIP.EQ.0) THEN
	     Y_MIN=R(J-1)
	     IYSKIP=1
	   ENDIF
	   IF (IBC0.EQ.1.AND.IBC0Y.EQ.0) Y_MAX=R(J-1) 
	   IF (IBC0.EQ.0.AND.IBC0Z.EQ.1.AND.IZSKIP.EQ.0) THEN
	     Z_MIN=Z(K-1)
	     IZSKIP=1
	   ENDIF
	   IF (IBC0.EQ.4.AND.IBC0Z.EQ.0) Z_MAX=Z(K-1) 
	   BBB=0
10    CONTINUE
	DO 100 I=2,MP,2
	   ID2=I/2
	DO 100 J=2,NP,2
	   JD2=J/2
	   K_TOP=(LP-2)/2
	   IBC0=IBCELL(I,J,K_TOP*2)
	   IF (IBC0.NE.0.or.r(j).lt.y_min.or.r(j).gt.y_max) CYCLE	  
	      TRACE_S=PS(ID2,JD2,K_TOP,1)%DN
            TRACE_C=PC(ID2,JD2,K_TOP,1)%DN 
	   IF (TRACE_S.EQ.ZERO.AND.TRACE_C.EQ.ZERO) CYCLE !FIND CELLS THAT HAVE BATCH
	   ISEED=I
	   JSEED=J
	   KSEED=K
	   ZSEED=Z_MAX_LOC
	     DO 200 XSEED=X_MIN_LOC,X_MAX_LOC,0.2
	     DO 200 YSEED=Y_MIN_LOC,Y_MAX_LOC,0.2
	          X_MIN_LOC=X(I-1)
	          X_MAX_LOC=X(I+1)
	          Y_MIN_LOC=R(J-1)
	          Y_MAX_LOC=R(J+1)
	          Z_MIN_LOC=Z(K_TOP*2-1)
	          Z_MAX_LOC=Z(K_TOP*2+1) !ABOVE SIX LINES DEFINE ORIGIN CELL
	       NUM_SEEDS=NUM_SEEDS+1
	       V_X=(LG(I-1,J,K_TOP*2)%U(1)*UG0+LG(I+1,J,K_TOP*2)
     &           %U(1)*UG0)/2.0d+0
	       XPOS=XSEED+V_X*DEL_T
	       IF(XPOS.LT.X_MIN) XPOS=X_MIN
	       IF(XPOS.GT.X_MAX) THEN
	          SEEDTIME=DEL_T
	       ENDIF
	       V_Y=(LG(I,J-1,K_TOP*2)%U(2)*UG0+LG(I,J+1,K_TOP*2)
     &           %U(2)*UG0)/2.0d+0
	       YPOS=YSEED+V_Y*DEL_T
	       IF(YPOS.LT.Y_MIN) YPOS=Y_MIN
	       IF(YPOS.GT.Y_MAX) YPOS=Y_MAX
	       V_Z=(LG(I,J,K_TOP*2-1)%U(3)*UG0+LG(I,J,K_TOP*2+1)
     &           %U(3)*UG0)/2.0d+0
	       ZPOS=ZSEED+V_Z*DEL_T
		   IF(ZPOS.LT.Z_MIN) ZPOS=Z_MIN
	       IF(ZPOS.GT.Z_MAX) ZPOS=Z_MAX
	          IS=I
	          JS=J
	          KS=K_TOP*2
	          DO 210 ISDTME=1,10000000 !MOVE SEED TILL IT LEAVES
	            V_X=(LG(IS-1,JS,KS)%U(1)*UG0+LG(IS+1,JS,KS)
     &                %U(1)*UG0)/2.0d+0
	            XPOS=XPOS+V_X*DEL_T
	            IF(XPOS.LT.X_MIN) XPOS=X_MIN
	              IF(XPOS.LT.X_MIN_LOC) THEN !CHECK TO SEE IF IT LEAVES THE CELL
				     IS=IS-2
	                 X_MAX_LOC=X_MIN_LOC
	                 X_MIN_LOC=X(IS-1)
	              ENDIF
	              IF(XPOS.GT.X_MAX_LOC) THEN
				     IS=IS+2
	                 X_MIN_LOC=X_MAX_LOC
	                 X_MAX_LOC=X(IS+1)
                    ENDIF
	            IF(XPOS.GT.X_MAX) THEN
	                 SEEDTIME=DEL_T*(ISDTME+1) !NEED TO SKIP OUT OF LOOP
	                 IF(SEEDTIME.GT.SEEDMAX) THEN
	                    SEEDMAX=SEEDTIME
	                    IS_MAX=IS
	                    JS_MAX=JS
	                    KS_MAX=KS
	                    XQ_MAX=XSEED
	                    YQ_MAX=YSEED
	                 ENDIF
	                 IF(SEEDTIME.LT.SEEDMIN) THEN
	                    SEEDMIN=SEEDTIME
	                    IS_MIN=IS
	                    JS_MIN=JS
	                    KS_MIN=KS
	                    XQ_MIN=XSEED
	                    YQ_MIN=YSEED
	                 ENDIF
	                 TOT_SEEDTIME=TOT_SEEDTIME+SEEDTIME
					 NUM_OUT=NUM_OUT+1
	                 SEEDT=SEEDTIME/60
	                 IF (SEEDT.LT.5) THEN
	                    IQUAL(1)=IQUAL(1)+1
	                 ELSEIF (SEEDT.LT.10) THEN
	                    IQUAL(2)=IQUAL(2)+1
	                 ELSEIF (SEEDT.LT.15) THEN
	                    IQUAL(3)=IQUAL(3)+1
	                 ELSEIF (SEEDT.LT.20) THEN
	                    IQUAL(4)=IQUAL(4)+1
	                 ELSEIF (SEEDT.LT.25) THEN
	                    IQUAL(5)=IQUAL(5)+1
	                 ELSEIF (SEEDT.LT.30) THEN
					    IQUAL(6)=IQUAL(6)+1
	                 ELSEIF (SEEDT.LT.35) THEN
	                    IQUAL(7)=IQUAL(7)+1
	                 ELSEIF (SEEDT.LT.40) THEN
	                    IQUAL(8)=IQUAL(8)+1
	                 ELSEIF (SEEDT.LT.45) THEN
	                    IQUAL(9)=IQUAL(9)+1
	                 ELSEIF (SEEDT.LT.50) THEN
	                    IQUAL(10)=IQUAL(10)+1
	                 ELSEIF (SEEDT.LT.55) THEN
					    IQUAL(11)=IQUAL(11)+1
	                 ELSEIF (SEEDT.LT.60) THEN
	                    IQUAL(12)=IQUAL(12)+1
	                 ELSEIF (SEEDT.LT.65) THEN
	                    IQUAL(13)=IQUAL(13)+1
	                 ELSEIF (SEEDT.LT.70) THEN
	                    IQUAL(14)=IQUAL(14)+1
	                 ELSEIF (SEEDT.LT.75) THEN
	                    IQUAL(15)=IQUAL(15)+1
	                 ELSEIF (SEEDT.LT.80) THEN
					    IQUAL(16)=IQUAL(16)+1
	                 ELSEIF (SEEDT.LT.10) THEN
	                    IQUAL(17)=IQUAL(17)+1
	                 ELSEIF (SEEDT.LT.15) THEN
	                    IQUAL(18)=IQUAL(18)+1
	                 ELSEIF (SEEDT.LT.20) THEN
	                    IQUAL(19)=IQUAL(19)+1
	                 ELSEIF (SEEDT.LT.25) THEN
	                    IQUAL(20)=IQUAL(20)+1
	                 ELSEIF (SEEDT.GT.25) THEN
	                    IQUAL(21)=IQUAL(21)+1
	                 ENDIF
	                 GOTO 200
	            ENDIF
cbg	            GOTO 210
	            V_Y=(LG(IS,JS-1,KS)%U(2)*UG0+LG(IS,JS+1,KS)
     &                %U(2)*UG0)/2.0d+0
	            YPOS=YPOS+V_Y*DEL_T
	            IF(YPOS.LT.Y_MIN) YPOS=Y_MIN
	               IF(YPOS.LT.Y_MIN_LOC) THEN !CHECK TO SEE IF IT LEAVES THE CELL
				     JS=JS-2
	                 Y_MAX_LOC=Y_MIN_LOC
	                 Y_MIN_LOC=R(JS-1)
	              ENDIF
	              IF(YPOS.GT.Y_MAX_LOC) THEN
				     JS=JS+2
	                 Y_MIN_LOC=Y_MAX_LOC
	                 Y_MAX_LOC=R(JS+1)
                    ENDIF
	            IF(YPOS.GT.Y_MAX) YPOS=Y_MAX
cbg	            GOTO 210
	            V_Z=(LG(IS,JS,KS-1)%U(3)*UG0+LG(IS,JS,KS+1)
     &                %U(3)*UG0)/2.0d+0
	            ZPOS=ZPOS+V_Z*DEL_T
		        IF(ZPOS.LT.Z_MIN) ZPOS=Z_MIN
	              IF(YPOS.LT.Y_MIN_LOC) THEN !CHECK TO SEE IF IT LEAVES THE CELL
				     KS=KS-2
	                 Z_MAX_LOC=Z_MIN_LOC
	                 Z_MIN_LOC=Z(KS-1)
	              ENDIF
	            IF(ZPOS.GT.Z_MAX) ZPOS=Z_MAX
	              IF(ZPOS.GT.Z_MAX_LOC) THEN
				     KS=KS+2
	                 Z_MIN_LOC=Z_MAX_LOC
	                 Z_MAX_LOC=Z(KS+1)
                    ENDIF
210             CONTINUE
	          BBB=0
200          CONTINUE
	   BBB=0
100   CONTINUE
      AVERAGE_SEED=TOT_SEEDTIME/NUM_OUT
	GOTO 998
999   FILENAME='GQAL'//RUNUM//'M.DAT'
      OPEN(NU,FILE=FILENAME)
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
998   FILENAME='GQL'//RUNUM//'N.DAT'
      OPEN(NU,FILE=FILENAME)
CBG      WRITE(NU,'(" X / R",T12,11F11.3)') (R_C_R(J),J=J1,J2,2)
	WRITE(NU,'("MINIMUM RESIDENCE TIME:",3X,F11.3)') SEEDMIN
	WRITE(NU,'("LOCATED AT NODE:",3X,I3,3X,I3,3X,I3)') 
     &            IS_MIN,JS_MIN,KS_MIN
	WRITE(NU,'("POSITION:",3X,F11.3,3X,F11.3)') XQ_MIN,YQ_MIN
	WRITE(NU,'("MAXIMUM RESIDENCE TIME:",3X,F11.3)') SEEDMAX
	WRITE(NU,'("LOCATED AT NODE:",3X,I3,3X,I3,3X,I3)') 
     &            IS_MAX,JS_MAX,KS_MAX
	WRITE(NU,'("POSITION:",3X,F11.3,3X,F11.3)') XQ_MAX,YQ_MAX
	WRITE(NU,'("AVERAGE RESIDENCE TIME:",3X,F11.3)') AVERAGE_SEED
	WRITE(NU,'(" ")')
	DO 997 I=1,21
      WRITE(NU,776) I,IQUAL(I)
776   FORMAT(3X,I4,3X,I4)
997   CONTINUE
      CLOSE(NU)
	CALL DALCC
	RETURN
      END
