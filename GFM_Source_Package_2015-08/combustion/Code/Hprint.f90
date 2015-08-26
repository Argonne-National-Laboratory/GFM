! HPRINT.F90
!======================================================================
!     HPRINT prints headings,
!        reference values, and non-dimensional numbers
!       9/97
!======================================================================
SUBROUTINE HPRINT
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
BBQ0=Q0*CP0*(T0-TL0)/WF
IF (FR_Q.GT.ZERO) THEN
   YFOP=FR_P/FR_Q
   FAR=(FR_F+FR_OX)/FR_Q
ELSE
   YFOP=ZERO
   FAR=ZERO
ENDIF

WRITE (nu_rt,20)
20    FORMAT(//T30,'ICRKFLO3: CALCULATION RESULTS'//T30,'   REFERENCE PARAMETERS'/)
WRITE (nu_rt,30) UG0,UL0,T0,PG0
30    FORMAT(T4,'UG0=',F8.3,T21,'UL0=',F8.3,T38,'T0=',F8.1,T56,'PG0=',G11.3)
write (nu_rt,40) RLM0,DND0,TK0,EPSI0
40    FORMAT(T4,'RLM0=',G11.3,T21,'DND0=',G11.3,T38,'TK0=',F8.3,T56,'EPS0=',G10.3)
WRITE (nu_rt,50) YF0,YN20,WT0,TL0,THET0C
50    FORMAT(T4,'YF0=',F8.4,T21,'YN20=',F8.4,T38,'WT0=',F8.3,T56,'TL0=',F8.3,T72,'THET0C=',F8.4)
WRITE (nu_rt,60) TP0,DT0,DNST0,R0
60    FORMAT (T4,'TP0=',G11.3,T21,'DT0=',G11.3,T38,'DNST0=',G11.3,T56,'R0=',G11.3)
WRITE (nu_rt,70) XLE,RLE,WIDTH
70    FORMAT (T4,'XLE=',G11.3,T21,'RLE=',G11.3,T38,'WIDTH=',G11.3)
WRITE (nu_rt,110)
110   FORMAT(//T30,'  NON-DIMENSIONAL PARAMETERS'/)
WRITE (nu_rt,120) REYG,SCN,GCN,GVN
120   FORMAT(T4,'REYNOLDS=',G11.3,T27,'SCHMIDT=',G11.3,T50,'GC',1X,'NUMBER=',G11.3,T72,'GV NUMBER=',G11.3)
WRITE (nu_rt,130) GDN,ECN,DAMKO,BBQ0
130   FORMAT (T4,'GD NUMBER=',G11.3,T27,'ECKERT=',G11.3,T50,'DAMKHOLER=',G11.3,T72,'HEAT=',G11.3)
WRITE (nu_rt,140) RFG,EPSR,TAUR,FAR
140   FORMAT (T4,'RFG=',G11.3,T27,'EPSR=',G11.3,T50,'TAUR=',G11.3,T72,'G/O RATIO=',G11.3)
WRITE (nu_rt,150) EUN,DNUL0,DNUC0,YFOP
150   FORMAT (T4,'EULER=',G11.3,T27,'DNUL0=',F6.3,T50,'DNUC0=',F6.3,T72,'C/O RATIO=',F7.3)
WRITE (nu_rt,160) TRN
160   FORMAT (T4,'TRN= ',G11.3)
RETURN
END