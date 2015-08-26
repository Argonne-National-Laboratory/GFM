! STDY3P.F90
!======================================================================
!     to compute steady state three-phase flow
!        10/97
!======================================================================
SUBROUTINE STDY3P
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

1     FORMAT(1X/T15,'TYPICAL ITERATION RESULT FOR TWO-PHASE FLOW', &
       ' FIELD'/T2,'LD',T11,'RMX',T20,'IMX,JMX,KMX',T35,'mass resid',/, &
       T8,'   U(I,J,K)   V(I,J,K)   W(I,J,K)   P(I,J,K) DNST(I,J,K)  T(I,J,K)')
2     FORMAT(' D',I5,E12.4,3I4,E12.4)
3     FORMAT(T8,6E11.4)

!----------------------------------------------------------------------
!     res_mass_max: maximal mass residual
!     res_mass: average mass residual
!     MAXSI: maximal allowable global iterations
!     MAXGI: maximal allowable gas phase iterations
!     BSCON: acceptable mass residual for a two-phase calculation
!     BGCON: acceptable mass residual for a gas phase calculation
!     BDCON: acceptable mass residual for a droplet phase calculation
!     BPCON: acceptable mass residual for a particle phase calculation
!     IDIT:  two-phase iterations/global iteration 
!----------------------------------------------------------------------

DATA IDIT1,IDIT2,LD1,LD2/1,1,0,0/
LASTIT=1
IF (.NOT.STEADY) THEN
   DO I=2,MP
   DO J=2,NP
   DO K=2,LP
      APO(I,J,K)=THETAO(I,J,K)*DNSTO(I,J,K)*TRN/DTM
   enddo;enddo;enddo
ENDIF

WRITE(NU,1) 
IIRAD=0
DO M=1,MAXSI
   IIRAD=IIRAD+1
   IF (M.GE.MAXSI-1) IRC=1
   !if (.not. steady) CALL OSAVE
   NCONV=0
   CALL SPHASE 

   !-------------------------------------------------------------------
   !   CALCULATE THE RADIATION HEAT FLUX
   !-------------------------------------------------------------------
   !-------------------------------------------------------------------
   !CBG     IF (res_mass.LE.BGCON) NCONV=NCONV+1

   IF (res_mass < BGCON) NCONV=NCONV+3

   !CBG     IF (FR_Q.LE.SMALL) THEN
   !CBG        NCONV=NCONV+1
   !CBG            GOTO 160
   !CBG     ENDIF
   !CBG         DO N=1,IDIT1
   !CBG        IF (LD1.GE.10000) LD1=0
   !CBG            LD1=LD1+1
   !CBG            CALL DFLOW
   !CBG            CALL DEVA
   !CBG        IF (res_mass.LE.BDCON) THEN
   !CBG               NCONV=NCONV+1
   !CBG               GOTO 160
   !CBG            ENDIF
   !CBG         ENDDO
   ! 160 continue

   DO I=5,MP-1,2
      FLX(I,10)=FLX(I,10)+FLX(I,8)
      FLXH(I,1)=FLXH(I,1)+FLXH(I,3)          
   ENDDO

   !CBG         IF (FR_P.LE.SMALL.OR.IDIT2.LE.0) THEN
   !CBG        NCONV=NCONV+1
   !CBG            CALL TH_GS
   !CBG            GOTO 180
   !CBG     ENDIF
   !CBG         DO N=1,IDIT2
   !CBG        IF (LD2.GE.10000) LD2=0
   !CBG            LD2=LD2+1
   !CBG            CALL PFLOW
   !CBG        IF (res_mass.LE.BPCON) THEN
   !CBG               NCONV=NCONV+1
   !CBG               GOTO 180
   !CBG            ENDIF
   !CBG         ENDDO
   !         IF (M/10*10.EQ.M) THEN
   !            CALL SAV3F
   !     ENDIF
   ! 180 continue

   DO I=5,MP-1,2
      FLX(I,10)=FLX(I,10)+FLX(I,9)
      FLXH(I,1)=FLXH(I,1)+FLXH(I,4)          
   ENDDO
   IF (NCONV.GE.3) exit
enddo
if (iconv>0) write(nug,"(i7,4e25.16)") itr_gas,res_mass,res_mass_max
if (ihresid>0) write(nu_hres,"(i7,4e25.16)") itr_gas,h_resid,q_global_bal,h_resid_pre

RETURN
END
         
