! REAC.F90
!======================================================================
!        Calculate light oil concentration and
!          return concentration of dry gas and coke
!          reaction 1: heavy oil cracking
!          reaction 2: light oil conversion
!          F1: heavy oil concentration
!          F2: light oil concentration
!          F3: dry gas and coke concentration
!          CKA1,CKA2,CKA3: stoichiometric coefficients of reaction 1
!          CKR: ratio of rate constants, CKK2/CKK1
!          by SLC, SAL, and CQZ 3/98
!======================================================================
SUBROUTINE REAC(F1,F3,CKA1,CKR)
IMPLICIT DOUBLE PRECISION(A-H,O-Z)   
DIMENSION B(5),U(5)
DATA B/0.11846D+0,0.23932D+0,0.28444D+0,0.23932D+0,0.11846D+0/
DATA U/-0.90618D+0,-0.53847D+0,0.0D+0,0.53847D+0,0.90618D+0/

CALL BOUND(F1,0,2)
G1=(1.D+0+F1)*5.D-1
G2=(1.D+0-F1)*5.D-1
F2=0.D+0
DO I=1,5 
   F=G1+G2*U(I)
   G0=-CKR*(1.D+0/F1-1.D+0/F)
   G0=EXP(G0)
   F2=F2+B(I)*G0
enddo
F2=F2*CKA1*(1.D+0-F1)
F3=1.D+0-F1-F2
CALL BOUND(F3,0,2)
RETURN
END   

      
!======================================================================
!======================================================================
!======================================================================
!     Calculate emperical kinetic constants
!
!     This routine is not used.
!
!======================================================================
SUBROUTINE HOKIN(TMP,TH_P,F_K,CKK1,CKR,CKA1,CKA2,CKA3,CKB1,CKB2,CKBET)
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
DATA CKA0,CKA20,CKB10/0.9825D+0,0.0D+0,0.8908D+0/   
DATA TMAX_fcc,TMIN,CKT0,CKTR1,CKTR2,CKTR3 /1.2D+3,5.0D+2,7.97D+2,13.0D+3,3.52D+3,1.35D-2/
DATA CKK0,CKR0,BETA_HOKIN/2.08D+0,1.08D-1,0.2825D+0/

!CSL      IF (TMP.LE.TMIN) THEN
!CSL     CKK1=0.0D+0
!CSL     CKR=0.0D+0
!CSL     RETURN
!CSL  ENDIF
TMP=MAX(TMP,TMIN)
TMP=MIN(TMP,TMAX_fcc)

!-------coke deactivation, cat/oil ratio
DATA CO_D1,CO_D2 /3.32D+1,4.23D+1/
DATA CO_N1,CO_N2 /2.5D+0,2.3D+0/
DATA CO_A1,CO_A2 /0.5D+0,1.727D+0/
DATA TC_A1,TC_A2,TH_PP0 /1.0D-2,1.0D-2,2.0D-2/

TH_PS=TH_P/TH_PP0
!---  CKK1
G1=-(CKTR1/TMP-CKTR1/CKT0)
CKK1=CKK0*EXP(G1)
CK_0=TH_PS**CO_N1
CK_1=CK_0/(1.D+0+CO_A1*CK_0)
CK_1=CK_1*EXP(-CO_D1*F_K)+TC_A1
CKK1=CKK1*CK_1
!---  CKR
G2=-(CKTR2/TMP-CKTR2/CKT0)
CKR=CKR0*EXP(G2)
CK_0=(CKBET/7.11D+0)**CO_N2
CK_1=CK_0/(1.D+0+CO_A2*CK_0)
CK_1=CK_1*EXP(-CO_D2*F_K)+TC_A2
CKR=CKR*CK_1
!      CKR=MIN(CKR,1.0D-1)

DATA CKB11,CKTR31,CO_N3/1.5D-2,1.05D-2,1.4D-2/
IF (TMP.GE.8.11D+2) THEN
   CKB1=CKB10+CKTR3
ELSEIF (TMP.GE.7.97D+2) THEN        
   G1=(TMP-7.97D+2)/1.4D+1
   CKB1=CKB10+CKTR3*G1
ELSEIF (TMP.GE.7.83D+2) THEN        
   G1=(TMP-7.97D+2)/1.4D+1
   CKB1=CKB10+CKTR31*G1
ELSE       
   CKB1=CKB10-CKTR31
ENDIF

IF (TH_P.GE.2.47D-2) THEN
   CKB1=CKB1-CO_N3
ELSEIF (TH_P.GE.1.88D-2) THEN
   G1=(TH_P-1.88D-2)/0.59D-2
   CKB1=CKB1-CO_N3*G1
ELSEIF (TH_P.GE.1.32D-2) THEN
   G1=(1.88D-2-TH_P)/0.56D-2
   CKB1=CKB1+CKB11*G1
ELSE
   CKB1=CKB1+CKB11
ENDIF

CKB2=1.D+0-CKB1
CKA1=CKA0-BETA_HOKIN
CKA2=CKA20+BETA_HOKIN*CKB1
CKA3=1.D+0-CKA1-CKA2
!CSL  ckk1=1
!CSL  cka2=ONE-CKA1
!CSL  cka3=0
!CSL      CKR=0
!CSL  CKB1=ONE
!CSL  CKB2=0
RETURN
END

