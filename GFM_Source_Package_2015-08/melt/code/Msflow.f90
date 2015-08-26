! MSFLOW.F90
!======================================================================
!     Calculation of sub-species transport and reaction in
!        the converged flowfield of a
!        two-dimensional multi-phase turbulent reacting flow in a
!        FCC Riser Reactor using a lumped component
!     Revision: 9/00
!======================================================================
      SUBROUTINE MSFLOW          
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8, ALLOCATABLE :: GFM_0(:,:) 

40    FORMAT(1X/T15,'TYPICAL ITERATION RESULT FOR SUB-SPECIES FLOW', &
      ' FIELD'/T2,'ITR',T11,'SMX',T20,'GFM')
44    FORMAT(I7,3G11.4,3G15.5)
45    FORMAT(T8,7G10.3)
99    FORMAT(I2,E15.6,F12.1)
!----------------------------------------------------------------------
!     LLK: number of iteration
!     SMX: maximal mass residual
!     AVEB: average mass residual
!     MAXMS: maximal allowable sub-species iterations
!     BMCON: acceptable residual for a sub-species calculation
!     K=1: iner gas for reference
!----------------------------------------------------------------------
      !cz    DATA NSP0 /5/
      LSTARM=1
      LENDM=NMSP
      WRITE(NU,40)
      NPM3=NP-3
      CALL FLX1D(3)
      ALLOCATE (GFM_0(NMSP,2))
      GFM_0=0
      GFMEX=0
      DO L=1,NMSP
         GFM_0(L,1)=FLXM(ITEND+1,L,1)
         GFMEX=GFMEX+GFM_0(L,1)
      ENDDO
!----------------------------------------------------------------------
!     BEGIN ITERATION
!        GSLV3M: solve sub-species governing equations
!----------------------------------------------------------------------
      DATA SMX1,II,KK,IX/1.0D-12,1,1,1/
      DO 200 L1=1,MAXMS
         LLK=L1
         CALL GSLV3M(LSTARM,LENDM)
         CALL FLX1D(3)
         GFMEX=0
         DO L=1,NMSP
            GFM_0(L,2)=FLXM(ITEND+1,L,1)
            GFMEX=GFMEX+GFM_0(L,2)
         ENDDO
         SMX=0
         DO L=LSTARM,LENDM
           SMX=SMX+ABS(GFM_0(L,2)-GFM_0(L,1))
           GFM_0(L,1)=GFM_0(L,2)
         ENDDO
         SMX=SMX/(LENDM-LSTARM+1)         
         WRITE(NU,44) LLK,SMX,GFIN,GFMEX
         IF (GFMEX.GT.SMALL) WRITE(NU,45) (GFM_0(L,1)/GFMEX,L=1,NMSP)
         FACM=(GFIN-GFMEX)/GFIN
         IF (SMX.LE.BMCON.AND.LLK.GT.100) THEN
            filename=casedir//'\y'//runum//'.out'
            OPEN (12,FILE=FILENAME)
            DO 240 L2=1,NMSP
               WRITE(12,44) L2,GFM_0(L2,1)
240         CONTINUE
            CLOSE(12)
            GOTO 300
         ENDIF
260      CALL EXTR(5)
         CALL INTP_minor_species
200   CONTINUE
300   DEALLOCATE (GFM_0)
      RETURN
      END
