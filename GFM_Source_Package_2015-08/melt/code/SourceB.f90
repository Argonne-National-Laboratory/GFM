! SOURCEB.F90
!======================================================================
!     Calculates source terms for each bubble governing equation
!     4/02
!======================================================================
      SUBROUTINE SOURCEB(MI1,MI3,NJ1,NJ3,LK1,LK3)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!----------------------------------------------------------------------
      DATA SMX0/1.0D-14/    
      IF (NBS0.LE.0.OR.KL.GT.NBS0) RETURN
      GOTO(100,200,200,200,500),NEL-12
      RETURN
!----------------------------------------------------------------------
!     Source term for bubble number density equation
!----------------------------------------------------------------------
100   DO 150 I=MI1,MI3,2
      ID2=I/2
      DO 150 J=NJ1,NJ3,2
      JD2=J/2
      DO 150 K=LK1,LK3,2
         KD2=K/2
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         IF (IBCELL(I,J,K).GT.0) CYCLE
         AREA=AREA_C(I,J,K,3)
!---     gas release from sand melt
         G0=GB4(ID2,JD2,KD2,KL)%GR
         BS(ID2,JD2,KD2)=G0/WGI_B(KL)
!---     bubble burst on liquid surface
         !cz       IF (IBCELL(I,J,K+2).EQ.4.and.GB3(ID2,JD2,KD2)%TH.gt.small) THEN
         !czcz        G1=GB3(ID2,JD2,KD2)%TH*0.8D+4
         !czcz            SFP(ID2,JD2,KD2)=-G1
         !cz            SFP(ID2,JD2,KD2)=-GB%B2*AREA*100.0D+0
         !cz       ENDIF
         !cz    goto 151
         IF (G0.GT.SMX0) THEN
            SFP(ID2,JD2,KD2)=-GB%B2*AREA*100.0D+0
         !cz            SFP(ID2,JD2,KD2)=-GB%B2*AREA*GB3(ID2,JD2,KD2)%th*1.0D+4
         ELSEIF (IBCELL(I,J,K+2).EQ.4.and.GB3(ID2,JD2,KD2)%TH.gt.small) THEN
         !CZ            SFP(ID2,JD2,KD2)=-GB%B2*AREA*1.0D+0
            SFP(ID2,JD2,KD2)=-GB%B2*AREA*1.0D-1
         !cz            SFP(ID2,JD2,KD2)=-GB%B2*AREA*GB3(ID2,JD2,KD2)%th*1.0D+2
         !cz            bs(ID2,JD2,KD2)=bs(id2,jd2,kd2)-GB%B2*AREA
         ENDIF
151        IF (MS.EQ.1) THEN

         !cz       goto 150
         !cz          SUM_AI=ZERO
         !cz          FI_GB=ZERO
         !cz          DO L=1,NBS0
         !cz          SUM_AI=SUM_AI+RG_B(L)**2
         !cz          ENDDO
         !cz          DO L=1,NBS0
         !cz          FI_GB(L)=RG_B(L)**2/SUM_AI       
         !cz          ENDDO

            SUM_NIAI=ZERO
            FI_GB=ZERO
            DO L=1,NBS0
            SUM_NIAI=SUM_NIAI+GB4(ID2,JD2,KD2,L)%DN*RG_B(L)**2
            ENDDO
            !cbg         IF (SUM_NIAI.GT.SMALL) THEN
            !cbg          DO L=1,NBS0
            !cbg          FI_GB(L)=GB4(ID2,JD2,KD2,L)%DN*RG_B(L)**2/SUM_NIAI         
            !cbg          ENDDO
            !cbg          ENDIF

            !cz          SUM_NIAI=ZERO
            !cz          SUM_NI=ZERO
            !cz          DO L=1,NBS0
            !cz          SUM_NIAI=SUM_NIAI+GB4(ID2,JD2,KD2,L)%DN*4.0D+0*RG_B(L)**2
            !cz          SUM_NI=SUM_NI+GB4(ID2,JD2,KD2,L)%DN
            !cz          IF (SUM_NI.LT.SMALL) THEN
            !cz          FI_GB(L)=ZERO
            !cz          ELSE
            !cz          FI_GB(L)=SUM_NIAI/SUM_NI         
            !cz           ENDIF
            !cz          ENDDO

            !cz          WTOTAL=0.1*RRMS(ID2,JD2,KD2,1)
            WTOTAL=0.01*RRMS(ID2,JD2,KD2,1)
            !cz          WTOTAL=0.05*RRMS(ID2,JD2,KD2,1)
            IF(KL.EQ.1) THEN
              WDIFFP1=WGI_B(KL+1)-WGI_B(KL)        
              RRSIN=ZERO
               !cbg           RRSOUT=WTOTAL*FI_GB(KL)/WDIFFP1
            ELSEIF(KL.EQ.NBS0) THEN
              WDIFFM1=WGI_B(KL)-WGI_B(KL-1)
               !cbg           RRSIN=WTOTAL*FI_GB(KL-1)/WDIFFM1
              RRSOUT=ZERO
             ELSE
              WDIFFM1=WGI_B(KL)-WGI_B(KL-1)
              WDIFFP1=WGI_B(KL+1)-WGI_B(KL)        
               !cbg           RRSIN=WTOTAL*FI_GB(KL-1)/WDIFFM1
               !cbg           RRSOUT=WTOTAL*FI_GB(KL)/WDIFFP1
             ENDIF
            if (rrsin.gt.small.or.rrsout.gt.small) then
             BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+RRSIN
             DN_GB=GB4(ID2,JD2,KD2,KL)%DN
              IF (DN_GB.GT.SMALL) THEN
              SFP(ID2,JD2,KD2)=SFP(ID2,JD2,KD2)-RRSOUT/GB4(ID2,JD2,KD2,KL)%DN
             ENDIF
            endif
            !cz          BS(ID2,JD2,KD2)=BS(ID2,JD2,KD2)+0.1*RRMS(ID2,JD2,KD2,1)
         ENDIF    
150   CONTINUE
      RETURN
!----------------------------------------------------------------------
!     Source term for bubble momentum equation @@@
!     Note: bubble drag formulation needs to be checked before use, Lottes 12/10/05
!----------------------------------------------------------------------
200   IU=NEL-13
      DO 250 I=MI1,MI3,2
      ID2=I/2
      DO 250 J=NJ1,NJ3,2
      JD2=J/2
      DO 250 K=LK1,LK3,2
         KD2=K/2
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         IF (IBCELL(I,J,K).GE.1) CYCLE
         DN0=GB4(ID2,JD2,KD2,KL)%DN
         IF (DN0.LT.SMALL) CYCLE
         DS_GB=GB4(ID2,JD2,KD2,KL)%DS
!---  intefacial drag
         GMUX=LG(I,J,K)%MU
         RG0=GB4(ID2,JD2,KD2,KL)%R
         DS_0=LG(I,J,K)%DS*LG(I,J,K)%TH
         !ZXKTEMP=0.15D+0*(REYD*RG0*DS_0*ABS(LG(I,J,K)%U(IU)-FZ(ID2,JD2,KD2))/GMUX)**0.687D+0
         ZXKTEMP=0.15D+0*(2*RG0*DS_0*ABS(LG(I,J,K)%U(IU)-FZ(ID2,JD2,KD2))/GMUX)**0.687D+0
         ZXK=ONE+ZXKTEMP
         !RFG_1=DS_GB
         !GDZ=pi6/RFG_1*ZXK*GMUX*DN0/(RG0**2)*LG(I,J,K)%TH
         GDZ=pi6/ds_gb*ZXK*GMUX*DN0/(RG0**2)*LG(I,J,K)%TH

         !cz       if(iu.eq.3) GDZ=GDZ*8.0D-1
         !cz-------------------
         !cz       IF (IBCELL(I,J,K+2).EQ.4) THEN
         !cz           BS(ID2,JD2,KD2)=GDZ*PS(ID2,JD2,KD2,1)%U(IU)
         !cz       ELSE
         !cz            BS(ID2,JD2,KD2)=GDZ*LG(I,J,K)%U(IU)
         !cz       ENDIF
         BS(ID2,JD2,KD2)=GDZ*LG(I,J,K)%U(IU)

         SFP(ID2,JD2,KD2)=-GDZ
!---  buoyancy force
         IF (IU.EQ.3.AND.GRA(IU).LT.SMALL) THEN
            G0=GRA(IU)*DN0*(1-LG(I,J,K)%DS/DS_GB)
            BS(ID2,JD2,KD2)=G0+BS(ID2,JD2,KD2)
         ENDIF
250   CONTINUE
      RETURN

!----------------------------------------------------------------------
!     Source term for bubble energy equation
!     C.Zhou apparently tried a number of things here
!     that did not work.
! 
!     A good bubble heat transfer model needs to be implemented here. @@@
!     Perhaps assumning local thermal equilibrium is sufficient.
!----------------------------------------------------------------------
500   DO 550 I=MI1,MI3,2
      ID2=I/2
      DO 550 J=NJ1,NJ3,2
      JD2=J/2
      DO 550 K=LK1,LK3,2
         KD2=K/2
         BS(ID2,JD2,KD2)=ZERO
         SFP(ID2,JD2,KD2)=ZERO
         IF (IBCELL(I,J,K).GE.1) GOTO 550

         DN0=GB4(ID2,JD2,KD2,KL)%DN
         IF (IBCELL(I,J,K).GE.1) THEN
            BS(ID2,JD2,KD2)=FZ(ID2,JD2,KD2)
            !cz            GOTO 550
         ELSEIF (DN0.LE.DN_MN) THEN
            BS(ID2,JD2,KD2)=TMLTR
            GOTO 550
         ENDIF

         !cz         GOTO 550

            GMUX=LG(I,J,K)%MU
            GDFX=ONE
            SVEL1=SVELC(I,J,K,KL)
            !CZGB
            RG0=GB4(ID2,JD2,KD2,KL)%R
            !RELK=REYD*RG0*LG(I,J,K)%DS*SVEL1/GMUX
            RELK=2*RG0*LG(I,J,K)%DS*SVEL1/GMUX
            !SCNK=SCN*GMUX/LG(I,J,K)%DS/GDFX
            SCNK=GMUX/LG(I,J,K)%DS/GDFX
            !            PRNK=GMUX*GMU0*LG(I,J,K)%C/GK0_1
            DNUCK=(TWO+0.654D+0*SQRT(RELK)*SCNK**THIRD)
            !            DNUCK=(TWO+0.654D+0*SQRT(RELK)*PRNK**THIRD)
            !CZGB
            !GDZ=one/GB%CP*GVN_1/RFG_1*DNUCK*DN0/RG0**2
            GDZ=pi2*gk0_1/GB%CP/ds_gb*DNUCK*DN0/RG0**2
            !CZ          GDZ=GDZ*RF(4)
            BS(ID2,JD2,KD2)=GDZ*LG(I,J,K)%T
            SFP(ID2,JD2,KD2)=-GDZ
550   CONTINUE
      RETURN
      END
