!     Adlbl3.f90
!======================================================================
!======================================================================
!======================================================================
!     Linear equation solver using 
!        alternating direction line-by-line sweep with
!        the tridiagonal matrix algorithm modified for 3-D calculations
!     1/98
!======================================================================
SUBROUTINE ADLBL3(XT,IB,IE,JB,JE,KB,KE)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON /ADLBL1/IPL,IML,JBML,JEPL,NIM
DIMENSION XT(MP,NP,LP),XT2(NP,LP)

!IF (LD1.EQ.1) THEN
if (eqtyp==1) then
   NIM=NTIMES(NEL)
   LD=1
!ELSEIF (LD1.EQ.2) THEN
elseif (eqtyp==2) then
   NIM=NTIMESG(NELG)
   LD=2
ELSE
   NIM=NTIMESM(NELM)
   LD=2
ENDIF 
IF (JE.EQ.2) THEN
   JBML=JB
   JEPL=JE
ELSE       
   JBML=JB-LD
   JEPL=JE+LD
ENDIF

!----------------------------------------------------------------------
!     Forward and backward sweeps in X- and Z-domains
!----------------------------------------------------------------------
DO Nsweep=1,NIM
   DO I=IB,IE,LD
      IPL=I+LD
      IML=I-LD
      CALL ADLZ(XT,XT2,LD,I,JB,JE,KB,KE)
   enddo
   !csl         IF (LD.EQ.1) cycle
   DO I=IE,IB,-LD
      IPL=I+LD
      IML=I-LD
      CALL ADLZ(XT,XT2,LD,I,JB,JE,KB,KE)
   enddo
enddo

!----------------------------------------------------------------------
!        Check residuals
!----------------------------------------------------------------------
!Energy equation residual normalized
!if(eqtyp==2.and.nelg==8) call calc_residual(xt,h_resid,ib,ie,jb,je,kb,ke)

!Calc all residuals
IF (IRC == 1) then
   RES0=ZERO
   RESX=ZERO
   DO I=IB,IE,LD
   IPL=I+LD
   IML=I-LD
   DO J=JB,JE,LD
   JML=J-LD
   JPL=J+LD
DO K=KB,KE,LD
   IF (AP(I,J,K).EQ.ONE.or.ibcell(i,j,k)>0) cycle
      KML=K-LD
      KPL=K+LD
      RESC=AS(I,J,K,1,2)*XT(IPL,J,K)+AS(I,J,K,1,1)*XT(IML,J,K) &
          +AS(I,J,K,2,2)*XT(I,JPL,K)+AS(I,J,K,2,1)*XT(I,JML,K) &
          +AS(I,J,K,3,2)*XT(I,J,KPL)+AS(I,J,K,3,1)*XT(I,J,KML) &
          +BS(I,J,K)-AP(I,J,K)*XT(I,J,K)
      RESC=ABS(RESC)
   RESX=MAX(RESX,RESC)
      RES0=RES0+RESC
   enddo;enddo;enddo
   RES0=RES0/NCELLS            
   !IF (LD1.EQ.1) THEN
   if (eqtyp==1) then
      RES(NEL,1)=RES0
      RES(NEL,2)=RESX
   elseif (eqtyp==2) then
   !ELSEIF (LD1.EQ.2) THEN
      RESG(NELG,1)=RES0
      RESG(NELG,2)=RESX
!   ELSE
!      RESM(NELM,1)=RES0
!      RESM(NELM,2)=RESX
   ENDIF        
endif

RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE ADLZ(XT,XT2,LD,I,JB,JE,KB,KE)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON /ADLBL1/IPL,IML,JBML,JEPL,NIM
DIMENSION XT(MP,NP,LP),XT2(NP,LP),XT3(NP,LP)

IF (LP.EQ.2) THEN
   KEPL=KE
ELSE
   KEPL=KE+LD
ENDIF
DO K=KB,KEPL,LD
DO J=JB,JEPL,LD
   XT2(J,K)=XT(I,J,K)
   XT3(J,K)=XT(I,J,K)
enddo;enddo 

DO L=1,2
   DO K=KB,KE,LD
      CALL ADLZY(XT,XT2,LD,I,JB,JE,K)
   enddo
   IF (KB.GE.KE) THEN
      DO K=KB,KE,LD
      DO J=JB,JE,LD
         XT(I,J,K)=XT2(J,K)
      enddo;enddo
      return
   ENDIF
   DO J=JB,JE,LD
      CALL ADLYZ(XT,XT3,LD,I,J,KB,KE)
   enddo
   DO K=KB,KE,LD
   DO J=JB,JE,LD
      !        IF (IBCELL(I*2/LD,J*2/LD,K*2/LD).GE.1) GOTO 230
      XT(I,J,K)=(XT2(J,K)+XT3(J,K))/TWO
   enddo;enddo
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE ADLZY(XT,XT2,LD,I,JB,JE,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON /ADLBL1/IPL,IML,JBML,JEPL,NIM
DIMENSION XT(MP,NP,LP),XT2(NP,LP),PT(NP),QT(NP)

IF (JE.EQ.2) RETURN
KPL=K+LD
KML=K-LD
PT(JBML)=ZERO
QT(JBML)=XT(I,JBML,K)

DO J=JB,JE,LD
   JML=J-LD
   ALP=AP(I,J,K)-PT(JML)*AS(I,J,K,2,1)
   BSY=BS(I,J,K)
   IF (ABS(ALP).LE.SMALL20.OR.IBCELL(I*2/LD,J*2/LD,K*2/LD).GE.1) THEN
      PT(J)=ZERO
      QT(J)=XT(I,J,K)
   ELSE
      PT(J)=AS(I,J,K,2,2)/ALP
      BSY=BSY+AS(I,J,K,1,2)*XT(IPL,J,K)+AS(I,J,K,1,1)*XT(IML,J,K)
   IF (LP.GE.6) THEN
         BSY=BSY+AS(I,J,K,3,2)*XT(I,J,KPL)+AS(I,J,K,3,1)*XT(I,J,KML)
   ENDIF
      QT(J)=(BSY+AS(I,J,K,2,1)*QT(JML))/ALP
   ENDIF
enddo

DO J=JE,JB,-LD
   !     IF (IBCELL(I,J,K).GE.1) GOTO 200
   XT2(J,K)=XT2(J+LD,K)*PT(J)+QT(J)
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
SUBROUTINE ADLYZ(XT,XT3,LD,I,J,KB,KE)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON /ADLBL1/IPL,IML,JBML,JEPL,NIM
DIMENSION XT(MP,NP,LP),XT3(NP,LP),PT(LP),QT(LP)

IF (LP.EQ.2) RETURN
JPL=J+LD
JML=J-LD
KBML=KB-LD
PT(KBML)=ZERO
QT(KBML)=XT(I,J,KBML)

DO K=KB,KE,LD
   KML=K-LD
   ALP=AP(I,J,K)-PT(KML)*AS(I,J,K,3,1)
   BSY=BS(I,J,K)
   IF (ABS(ALP).LE.SMALL20.OR.IBCELL(I*2/LD,J*2/LD,K*2/LD).GE.1) THEN
      PT(K)=ZERO
      QT(K)=XT(I,J,K)
   ELSE
      PT(K)=AS(I,J,K,3,2)/ALP
      BSY=BSY+AS(I,J,K,1,2)*XT(IPL,J,K)+AS(I,J,K,1,1)*XT(IML,J,K)
      IF (NP.GE.6) THEN
         BSY=BSY+AS(I,J,K,2,2)*XT(I,JPL,K)+AS(I,J,K,2,1)*XT(I,JML,K)
   ENDIF
      QT(K)=(BSY+AS(I,J,K,3,1)*QT(KML))/ALP
   ENDIF
enddo

DO K=KE,KB,-LD
   XT3(J,K)=XT3(J,K+LD)*PT(K)+QT(K)
enddo
RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     Calculate Residual of Equation Solve 
!
!     Inputs: xt - computed solution
!             as - neighbor coefficient array
!             ap - cell point coefficient array
!             bs - source term
!             ncells - # of cell in domain
!
!             ap xt = sum(as xt_neighbor) + bs
!
!     Output: resid = mean normalized residual over domain
!======================================================================
subroutine calc_residual(xt,resid,ib,ie,jb,je,kb,ke)
use gbl_var
implicit double precision (a-h,o-z)
dimension xt(mp,np,lp)

resid=zero

do k=kb,ke,2
do j=jb,je,2
do i=ib,ie,2
   if(ibcell(i,j,k) > 0) cycle
   eq_max=max(abs(as(i,j,k,1,1)*xt(i-2,j,k)), &
              abs(as(i,j,k,1,2)*xt(i+2,j,k)), &
              abs(as(i,j,k,2,1)*xt(i,j-2,k)), &               
              abs(as(i,j,k,2,2)*xt(i,j+2,k)), &
              abs(as(i,j,k,3,1)*xt(i,j,k-2)), &               
              abs(as(i,j,k,3,2)*xt(i,j,k+2)), &               
              abs(ap(i,j,k)*xt(i,j,k)),       &         
              abs(bs(i,j,k)))               
   resid = resid                              &
           +abs(as(i,j,k,1,1)*xt(i-2,j,k)     &
              + as(i,j,k,1,2)*xt(i+2,j,k)     &
              + as(i,j,k,2,1)*xt(i,j-2,k)     &         
              + as(i,j,k,2,2)*xt(i,j+2,k)     &
              + as(i,j,k,3,1)*xt(i,j,k-2)     &         
              + as(i,j,k,3,2)*xt(i,j,k+2)     &         
              + bs(i,j,k)                     &
              - ap(i,j,k)*xt(i,j,k) )/eq_max               
enddo;enddo;enddo
resid=resid/ncells
resid=max(resid,1.0d-16)
return
end
