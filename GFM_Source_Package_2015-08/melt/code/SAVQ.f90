!---
!     Save intermediate heat flux results
!---
      SUBROUTINE SAVQ(IDX,I0)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*3 FNEXT
      N1=NWL/100
      N1=MOD(N1,10)+48
      N2=MOD(NWL,100)/10+48
      N3=MOD(NWL,10)+48
      FNEXT=CHAR(N1)//CHAR(N2)//CHAR(N3)
      IF (IDX.EQ.0.OR.IDX.EQ.1) THEN
         filename=casedir//'\qrtmp.'//fnext
      else
         filename=casedir//'\qrwtmp.'//fnext
      ENDIF
      OPEN(nu_savq,FILE=FILENAME,ERR=920)
      IF (IDX.EQ.0) THEN
         READ (nu_savq,*,END=910)I0
         READ (nu_savq,*,END=910)QE
         READ (nu_savq,*,END=910)QAL
         QES0=0
         DO 200 ID2=1,MP/2
         I=ID2*2
         DO 200 JD2=1,NP/2
         J=JD2*2
         DO 200 KD2=1,LP/2
         K=KD2*2
         QA(ID2,JD2,KD2)=0
         QES0=QES0+QE(ID2,JD2,KD2)
         DO 200 L=1,NWL
            QA(ID2,JD2,KD2)=QA(ID2,JD2,KD2)+QAL(ID2,JD2,KD2,L)
200      CONTINUE
      ELSEIF (IDX.EQ.1) THEN
         WRITE (nu_savq,*) I0
         WRITE (nu_savq,*) QE
         WRITE (nu_savq,*) QAL
      ELSEIF (IDX.EQ.2) THEN
         WRITE (nu_savq,*) QES0,QA_S,QA_G,QLS0
         WRITE (nu_savq,*) QALW
      ELSEIF (IDX.EQ.3) THEN
         QES0=0
         QA_S=0
         QA_G=0
         QLS0=0
         READ (nu_savq,*,END=910) QES0,QA_S,QA_G,QLS0
         READ (nu_savq,*,END=910) QALW
         QAS0=QA_S+QA_G+QLS0
         DO 240 ID2=1,MZ
         DO 240 JD2=1,NZ
         DO 240 KD2=1,LZ
         G0=0
         DO L=1,NWL
            G0=G0+QAL(ID2,JD2,KD2,L)+QALW(ID2,JD2,KD2,L)
         ENDDO
         QA(ID2,JD2,KD2)=G0
240      CONTINUE
      ENDIF
900   CLOSE (nu_savq)
      RETURN
910   CLOSE (nu_savq)
920   I=0
      RETURN
      END