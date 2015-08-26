!======================================================================
! SPHASE.F90
! Liquid glass computation
! 
!======================================================================
SUBROUTINE SPHASE
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
REAL*8 GN(6)
1     FORMAT(1X/T15,'TYPICAL ITERATION RESULT FOR GLASS FLOW',&
      ' FIELD'/T2,'LG',T11,'SMX',T20,'IMX,JMX,KMX',T35,'AVEB',/,&
      T8,'   U(I,J,K)   V(I,J,K)   W(I,J,K)   P(I,J,K) DNST(I,J,K)',&
      '  T(I,J,K)')
2     FORMAT(' LG',I5,E12.4,3I4,E12.4,' Out/In:',F10.4,'/',F7.4)
!2     FORMAT(' LG',I5,E12.4,3I4,E12.4,' Out/In:',F10.4,'/',F7.4,' Pull:',F10.4)
3     FORMAT(T8,5E11.4,F7.1,F7.2)
!----------------------------------------------------------------------
!     nlg: number of iteration
!     LLK: number of iteration
!     SMX: maximal mass residual
!     AVEB: average mass residual
!     MAXGI: maximal allowable glass phase iterations
!     BGCON: acceptable mass residual for a glass phase calculation
!----------------------------------------------------------------------
SMX=ZERO
AVEB=ZERO
IF (NPHAS.EQ.1) THEN !Lottes 4/15/05: This should not be true in a melt computation.
   WRITE(NU,1)
   MP_E=MPM2
ENDIF
!----------------------------------------------------------------------
!     BEGIN ITERATION
!        GFLOW: calculate glass flow properties
!        OSAVE: store system values in transient calculation
!----------------------------------------------------------------------
DO M=1,MAXGI
   IF (NLG.GT.100000) NLG=0
   NLG=NLG+1
   LLK=LLK+1
   !IF (NPHAS.EQ.1.AND..NOT.STEADY) CALL OSAVE
   CALL GFLOW 
   pull_tot=0
   do i=1,nex0
      pull_tot=pull_tot+pull(i,2)
   enddo    
   !OPEN(NU,FILE='flx'//RUNUM//'M.OUT')
   WRITE(NU,2) NLG,SMX,IMX,JMX,KMX,AVEB,GFEX,GFIN
   !Uncomment below to verify that pull_tot = gfex (glass flow at exit)
   !WRITE(NU,2) NLG,SMX,IMX,JMX,KMX,AVEB,GFEX,GFIN,pull_tot
   GN(1:3)=LG(IMX,JMX,KMX)%U(1:3)
   GN(4)=LG(IMX,JMX,KMX)%P
   GN(5)=LG(IMX,JMX,KMX)%DS
   GN(6)=LG(IMX,JMX,KMX)%T
   WRITE(NU,3) (GN(I),I=1,6),AVET
   if (aveb > zero) then
      aveb_logplt=aveb
   else
      aveb_logplt=bgcon
   endif
   smx_logplt=bgcon
   if (smx > zero) then
      smx_logplt=smx
   else
      smx_logplt=bgcon
   endif
   if (iconv>0) write(nug,"(i7,e25.16,e25.16)") &
       nlg,aveb_logplt,smx_logplt
   IF (AVEB.LE.BGCON.AND.AVET.LT.1.0D+0) EXIT
ENDDO
RETURN
END
