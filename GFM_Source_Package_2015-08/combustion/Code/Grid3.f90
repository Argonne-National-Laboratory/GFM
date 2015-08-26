! Grid3.f90
!======================================================================
!======================================================================
!======================================================================
!     Grid3 obtains the furnace geometry parameters
!     and builds a 3-D grid system. 
!======================================================================
SUBROUTINE GRID3
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
INTEGER, ALLOCATABLE :: IBC(:)
COMMON /GRD1/BV_F(2),BA_F(3,2),BD_F(3,2)
NAMELIST /GRD/BV_F,BA_F,BD_F
1     FORMAT (A32)
2     FORMAT (I5,2G15.5)
3     FORMAT (I4,2X,<NP>I1)

!----------------------------------------------------------------------
!     Read in grid selection
!     R0: reference length (m)
!     *** for PC debugging
!     MZ = # of X grid points including boundary points
!     NZ = # of Y or R grid points including boundary points
!     LZ = # of Z grid points including boundary points
!     NDNP = # of droplet and particle sizes
!     NR0 = number of data points in chemical kinetics correlation
!     NSP0 = number of sub-species lumps 
!----------------------------------------------------------------------

filename=casedir//'\gd'//gdnum//'c.dat'
OPEN (nu_grid,FILE=FILENAME)

READ (nu_grid,1) TITLE
READ (nu_grid,1) TITLE
!READ (nu_grid,*) R0,MP,NP,LP,NDP0,NPT0,NSP0,NR0    !r0=1, nsp0=7 always
READ (nu_grid,*) dummy_r0,MP,NP,LP,surf_length_c,surf_width_c,NDP0,NPT0,idummy_nsp0,NR0   !r0=1, nsp0=7 always
!-------------------------------------
!grid sizes used in array_dif routines
nx_gbl=mp
nxc_gbl=mp/2
nxf=mp
nyf=np
nzf=lp
nxc=mp/2
nyc=np/2
nzc=lp/2
!-------------------------------------
MM=MP
MZ=MP/2
MPM1=MP-1
MPM2=MP-2
MPD2=MP/2
NN=NP
NZ=NP/2
NPM1=NP-1
NPM2=NP-2
NPD2=NP/2
LL=LP
LZ=LP/2
LPM1=LP-1
LPM2=LP-2
LPD2=LP/2
NDNP=NDP0+NPT0
ALLOCATE (X(MP),DX(MP),R(NP),DR(NP),RS(NP),Z(LP),DZ(LP))
X=0
DX=0
R=0
DR=0
RS=1
Z=0
DZ=0
ALLOCATE (IBCELL(MP,NP,LP),IWALL(MZ,NZ,LZ))
IBCELL=0
IWALL=0

!---  X-GRID 
!        X(I): X-coordinate of the Ith node
!        DX(I): full cell size of the Ith node
READ (nu_grid,1) TITLE
IF (IDEBUG.EQ.2) WRITE(20,1) TITLE
READ (nu_grid,*) I1,XX
DO I=1,I1
   X(I)=XX
ENDDO

120   READ (nu_grid,*) I2,XX
IF (I2.LE.I1) GOTO 120      
DEL=(XX-X(I1))/(I2-I1)
IF (I2.GT.MP.OR.DEL.LE.ZERO) THEN
   WRITE(*,*) 'EITHER AN X COMPONENT IS GREATER THAN MAX X'
   WRITE(*,*) 'OR THE X COMPONENTS ARE NOT IN NUMERICAL ORDER.'
   WRITE(*,*) 'CHECK AROUND X-COORDINATE:', I2
   call stop_run("Error in reading grid file (with i).")
ENDIF
DO I=I1+1,I2
   X(I)=X(I-1)+DEL
ENDDO
I1=I2
IF (I2.LT.MPM1) GOTO 120
DO I=1,MPM1
   X(I)=X(I)/R0
ENDDO
X(MP)=X(MPM1)
XLE=X(MP)*R0
DX(1)=ZERO
DO I=2,MPM1
   DX(I)=X(I+1)-X(I-1)
ENDDO  
DX(MP)=ZERO
IF (IDEBUG.EQ.2) WRITE (20,2) (I,X(I),DX(I),I=1,MP)

!---  R-GRID 
!        R(J): Y-coordinate of the Jth node
!        DR(J): full cell size of the Jth node
!     Cylindrical coordinate system
!        RS(j): transformation factor
READ (nu_grid,1) TITLE
IF (IDEBUG.EQ.2) WRITE(20,1) TITLE
READ (nu_grid,*) J1,RR
DO J=1,J1
   R(J)=RR
ENDDO

130   READ (nu_grid,*) J2,RR
IF (J2.LE.J1) GOTO 130      
DEL=(RR-R(J1))/(J2-J1)
IF (J2.GT.NP.OR.DEL.LE.ZERO) THEN
   WRITE(*,*) 'EITHER AN Y COMPONENT IS GREATER THAN MAX Y'
   WRITE(*,*) 'OR THE Y COMPONENTS ARE NOT IN NUMERICAL ORDER.'
   WRITE(*,*) 'CHECK AROUND Y-COORDINATE:', J2
   call stop_run("Error in reading grid file (with j).")
ENDIF
DO J=J1+1,J2
   R(J)=R(J-1)+DEL
ENDDO
J1=J2
IF (J2.LT.NPM1) GOTO 130
DO J=1,NPM1
   R(J)=R(J)/R0
ENDDO
DR(1)=ZERO
IF (NP.GE.6) THEN
   NJY1=4
   NJY2=NPM2
   NJY1V(1)=4
   NJY2V(1)=NPM2
   NJY1V(2)=5
   NJY2V(2)=NP-3
   NJY1V(3)=4
   NJY2V(3)=NPM2
   R(NP)=R(NPM1)
   RLE=(R(NP)-R(3))*R0
   DO J=2,NPM1
      DR(J)=R(J+1)-R(J-1)
   ENDDO
   DR(NP)=ZERO
ELSE
   NJY1=2
   NJY2=2
   NJY1V=2
   NJY2V=2
   R(2)=R(2)/R0
   DR(2)=R(2)-R(1)
   RLE=DR(2)*R0
ENDIF
RS=ONE
IF (CYL) THEN
   DO J=2,NP
      RS(J)=R(J)
   ENDDO
ENDIF
IF (IDEBUG.EQ.2) WRITE (20,2) (J,R(J),DR(J),J=1,NP)

!---  Z-GRID 
!        Z(K): Z-coordinate of the Kth node
!        DZ(K): full cell size of the Kth node
READ (nu_grid,1) TITLE
IF (IDEBUG.EQ.2) WRITE(20,1) TITLE
READ (nu_grid,*) K1,ZZ
DO K=1,K1
   Z(K)=ZZ
ENDDO

140   READ (nu_grid,*) K2,ZZ
IF (K2.LE.K1) GOTO 140      
DEL=(ZZ-Z(K1))/(K2-K1)
IF (K2.GT.LP.OR.DEL.LE.ZERO) THEN
   WRITE(*,*) 'EITHER AN Z COMPONENT IS GREATER THAN MAX Z'
   WRITE(*,*) 'OR THE Z COMPONENTS ARE NOT IN NUMERICAL ORDER.'
   WRITE(*,*) 'CHECK AROUND Z-COORDINATE:', K2
   call stop_run("Error in reading grid file (with k).")
ENDIF
DO K=K1+1,K2
   Z(K)=Z(K-1)+DEL
ENDDO
K1=K2
IF (K2.LT.LPM1) GOTO 140
DO K=1,LPM1
   Z(K)=Z(K)/R0
ENDDO
DZ(1)=ZERO
IF (LP.GE.6) THEN
   LKZ1=4
   LKZ2=LPM2
   LKZ1V(1)=4
   LKZ2V(1)=LPM2
   LKZ1V(2)=4
   LKZ2V(2)=LPM2
   LKZ1V(3)=5
   LKZ2V(3)=LP-3
   Z(LP)=Z(LPM1)
   DO K=2,LPM1
      DZ(K)=Z(K+1)-Z(K-1)
   ENDDO
   DZ(LP)=ZERO
   WIDTH=(Z(LPM1)-Z(3))*R0
ELSE
   LKZ1=2
   LKZ2=2
   LKZ1V=2
   LKZ2V=2
   Z(2)=Z(2)/R0
   DZ(2)=Z(2)-Z(1)
   WIDTH=DZ(2)*R0
ENDIF
IF (IDEBUG.EQ.2) WRITE (20,2) (K,Z(K),DZ(K),K=1,LP)

!---  CELL DIAGRAM
!        IBCELL: blocked cell indicator,
!           0-regular, (-1,9)-half blocked
!           1-blocked,  2-inlet,   3-exit
IBCELL=0
READ (nu_grid,1) TITLE
IF (IDEBUG.EQ.2) WRITE(20,1) TITLE
ALLOCATE (IBC(NP))
K1=0

210   READ (nu_grid,*) K2

I1=0
220   READ (nu_grid,3) I2,(IBC(J),J=2,NP,2)
DO K=K1+2,K2,2
DO I=I1+2,I2,2
DO J=2,NP,2
   IF (IBC(J).GT.5) IBC(J)=IBC(J)-10
   IBCELL(I,J,K)=IBC(J)
ENDDO;ENDDO;ENDDO
I1=I2
IF (I2.LT.MP) GOTO 220
K1=K2   
IF (K2.LT.LP) GOTO 210
BV_F=ONE
BA_F=ONE
BD_F=ONE
READ (nu_grid,NML=GRD) 
CLOSE(nu_grid)

!Count number of computational cells and set cell faces     
NCELLS=0
DO I=2,MP,2
DO J=2,NP,2
DO K=2,LP,2
   IBC1=IBCELL(I,J,K)
   IF (IBC1.LE.0) THEN
      !CSL  IDL(I/2,J/2,K/2)=1
      NCELLS=NCELLS+1
      cycle
   ENDIF
   DO I1=I-1,I+1
   DO J1=J-1,J+1
   DO K1=K-1,K+1
      IF (I1.GT.MP.OR.J1.GT.NP.OR.K1.GT.LP) cycle
      IF (IBCELL(I1,J1,K1).NE.1) IBCELL(I1,J1,K1)=IBC1
   enddo;enddo;enddo
enddo;enddo;enddo

IF (IDEBUG.EQ.2) THEN
   DO K=2,LP
      WRITE(20,3) K
      DO I=2,MP
         WRITE(20,3) I,(IBCELL(I,J,K),J=2,NP)
      ENDDO
   ENDDO
ENDIF

vol_tot=zero !total volume of open cells
!Compute surface area of melt in main tank (exclude refiner)  6/5/05
area_melt_surf = zero
do i=2,mp,2
do j=2,np,2
   if (ibcell(i,j,2)==4) area_melt_surf = area_melt_surf + dx(i)*dr(j)*area0
   do k=2,lp,2
      if (ibcell(i,j,k) == 0) vol_tot=vol_tot+vol_c(i,j,k)*vol0
   enddo
enddo;enddo

DEALLOCATE (IBC)

RETURN
END


!======================================================================
!======================================================================
!======================================================================
!     DXAV: AXIAL MOMENTUM CELL WIDTH 
!     DRAV: RADIAL MOMENTUM CELL HEIGHT
!     DZAV: Z MOMENTUM CELL WIDTH 
!======================================================================
DOUBLE PRECISION FUNCTION DXAV(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (IBCELL(I-2,J,K).GE.1) THEN
   DXAV=X(I+1)-X(I-2)
ELSEIF (IBCELL(I+2,J,K).GE.1) THEN
   DXAV=X(I+2)-X(I-1)
ELSE
   DXAV=DX(I)
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION DRAV(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (IBCELL(I,J-2,K).GE.1) THEN
   DRC=R(J+1)-R(J-2)
   DRAV=(RS(J+1)+RS(J-2))*0.5D0/RS(J)*DRC
ELSEIF (IBCELL(I,J+2,K).GE.1) THEN
   DRC=R(J+2)-R(J-1)
   DRAV=(RS(J+2)+RS(J-1))*0.5D0/RS(J)*DRC
ELSE
   DRAV=DR(J)
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION DZAV(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (IBCELL(I,J,K-2).GE.1) THEN
   DZAV=Z(K+1)-Z(K-2)
ELSEIF (IBCELL(I,J,K+2).GE.1) THEN
   DZAV=Z(K+2)-Z(K-1)
ELSE
   DZAV=DZ(K)
ENDIF
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION VOL_C(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
IF (I/2*2.NE.I) THEN
   VOL_M=VOL_C0(I-1,J,K)
   IF (IBCELL(I-2,J,K).LE.0) VOL_M=VOL_M*0.5D+0
   VOL_P=VOL_C0(I+1,J,K)
   IF (IBCELL(I+2,J,K).LE.0) VOL_P=VOL_P*0.5D+0
   VOL=VOL_P+VOL_M
ELSEIF (J/2*2.NE.J) THEN
   VOL_M=VOL_C0(I,J-1,K)
   IF (IBCELL(I,J-2,K).LE.0) VOL_M=VOL_M*0.5D+0
   VOL_P=VOL_C0(I,J+1,K)
   IF (IBCELL(I,J+2,K).LE.0) VOL_P=VOL_P*0.5D+0
   VOL=VOL_P+VOL_M
ELSEIF (K/2*2.NE.K) THEN
   VOL_M=VOL_C0(I,J,K-1)
   IF (IBCELL(I,J,K-2).LE.0) VOL_M=VOL_M*0.5D+0
   VOL_P=VOL_C0(I,J,K+1)
   IF (IBCELL(I,J,K+2).LE.0) VOL_P=VOL_P*0.5D+0
   VOL=VOL_P+VOL_M
ELSE
   VOL=VOL_C0(I,J,K)
ENDIF
VOL_C=VOL
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION VOL_C0(I,J,K)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON /GRD1/BV_F(2),BA_F(3,2),BD_F(3,2)
VOL=DX(I)*DR(J)*RS(J)*DZ(K)
IF (IBCELL(I,J,K).LE.-1) THEN
   IBC=-IBCELL(I,J,K)
   VOL=VOL*BV_F(IBC)
ENDIF
   VOL_C0=VOL
RETURN
END


!======================================================================
!======================================================================
!======================================================================
DOUBLE PRECISION FUNCTION AREA_C(I,J,K,IX)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON /GRD1/BV_F(2),BA_F(3,2),BD_F(3,2)
!ix is the 1=x direction, 2=y direction, 3=z direction

!If the cell is a wall then return 0
IF (IBCELL(I,J,K).EQ.1) THEN
   AREA_C=ZERO
   RETURN
ENDIF

!If the cell is open then return the area of the face on the specified direction
IF (IX.EQ.1) THEN
   AREA=DR(J)*RS(J)*DZ(K)
ELSEIF (IX.EQ.2) THEN
   AREA=DX(I)*DZ(K)
ELSE
   AREA=DX(I)*DR(J)*RS(J)
ENDIF
AREA_C=AREA
IF (IBCELL(I,J,K).EQ.0) RETURN

!If the cell or the cell neighbor value is negative then apply a ba_f factor
IF (IBCELL(I,J,K).LE.-1) THEN
   IBC=-IBCELL(I,J,K)
   AREA_C=AREA_C*BA_F(IBC,IX)
   RETURN
ENDIF


IF (IX.EQ.1) THEN
   IF (I.LE.MPM2.AND.IBCELL(I+2,J,K).LE.-1) THEN
      IBC=-IBCELL(I+2,J,K)
   ELSEIF (I.GE.4.AND.IBCELL(I-2,J,K).LE.-1) THEN
      IBC=-IBCELL(I-2,J,K)
   ELSE
      RETURN
   ENDIF
ELSEIF (IX.EQ.2) THEN
   IF (J.LE.NPM2.AND.IBCELL(I,J+2,K).LE.-1) THEN
      IBC=-IBCELL(I,J+2,K)
   ELSEIF (J.GE.4.AND.IBCELL(I,J-2,K).LE.-1) THEN
      IBC=-IBCELL(I,J-2,K)
   ELSE
      RETURN
   ENDIF
ELSE
   IF (K.LE.LPM2.AND.IBCELL(I,J,K+2).LE.-1) THEN
      IBC=-IBCELL(I,J,K+2)
   ELSEIF (K.GE.4.AND.IBCELL(I,J,K-2).LE.-1) THEN
      IBC=-IBCELL(I,J,K-2)
   ELSE
      RETURN
   ENDIF
ENDIF

AREA_C=AREA_C*BA_F(IBC,IX)
RETURN
END

