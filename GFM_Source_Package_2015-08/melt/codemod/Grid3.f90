! GRID3.F90
!     GRID3 builds a 3-D grid system 
!    Rev: 4/02
!======================================================================
      SUBROUTINE GRID3
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1, ALLOCATABLE :: IBC(:)
      COMMON /GRD1/BV_F(2),BA_F(3,2),BD_F(3,2)
      NAMELIST /GRD/BV_F,BA_F,BD_F
1     FORMAT (A32)
!2     FORMAT (I5,2G15.5)
!3     FORMAT (I4,2X,<NP>A1)
!----------------------------------------------------------------------
!     Read in grid selection
!        R0: reference length (m)
!----------------------------------------------------------------------
      filename=casedir//'\gd'//gdnum//'m.dat'
      OPEN (nu_grid,FILE=FILENAME)
      READ (nu_grid,1) TITLE
      READ (nu_grid,1) TITLE
      READ (nu_grid,*) R0,MP,NP,LP,surf_length_m,surf_width_m
      !area0=r0*r0
      MP3(1)=MP
      MP3(2)=NP
      MP3(3)=LP
      MPM1=MP-1
      MPM2=MP-2
      MZ=MP/2
      NPM1=NP-1
      NPM2=NP-2
      NZ=NP/2
      LPM1=LP-1
      LPM2=LP-2
      LZ=LP/2
      CALL ALCG
!---  X-GRID 
!        X(I): X-coordinate of the Ith node
!        DX(I): full cell size of the Ith node
      READ (nu_grid,1) TITLE
      READ (nu_grid,*) I1,XX
      DO I=1,I1
         X(I)=XX
      ENDDO
120   READ (nu_grid,*) I2,XX
         IF (I2.LE.I1) GOTO 120      
         DEL=(XX-X(I1))/(I2-I1)
         IF (I2.GT.MP.OR.DEL.LE.ZERO) then
            call stop_run(" Error (with i) in Grid read.")
         endif
         DO I=I1+1,I2
            X(I)=X(I-1)+DEL
         ENDDO
         I1=I2
      IF (I2.LT.MPM1) GOTO 120
      X(MP)=X(MPM1)
      XLE=(X(MP)-X(3)) !Lottes 6/2/05
      !Calculate cell widths
      DX(1)=ZERO
      DO I=2,MPM1
         DX(I)=X(I+1)-X(I-1)
      ENDDO  
      DX(MP)=ZERO
!---  y-GRID 
!        y(J): Y-coordinate of the Jth node
!        dy(J): full cell size of the Jth node
      READ (nu_grid,1) TITLE
      READ (nu_grid,*) J1,yy
      DO J=1,J1
         y(J)=yy
      ENDDO
130   READ (nu_grid,*) J2,yy
         IF (J2.LE.J1) GOTO 130      
         DEL=(yy-y(J1))/(J2-J1)
         IF (J2.GT.NP.OR.DEL.LE.ZERO) then
            call stop_run("Error (with j) in Grid read.")
         endif
         DO J=J1+1,J2
            y(J)=y(J-1)+DEL
         ENDDO
         J1=J2
      IF (J2.LT.NPM1) GOTO 130
      dy(1)=ZERO
      IF (NP.GE.6) THEN
         NJY1=4
         NJY2=NPM2
         NJY1V(1)=4
         NJY2V(1)=NPM2
         NJY1V(2)=5
         NJY2V(2)=NP-3
         NJY1V(3)=4
         NJY2V(3)=NPM2
         y(NP)=y(NPM1)
         yle=(y(NP)-y(3))
         DO J=2,NPM1
            dy(J)=y(J+1)-y(J-1)
         ENDDO
         dy(NP)=ZERO
      ELSE
         NJY1=2
         NJY2=2
         DO II=1,3
          NJY1V(II)=2
            NJY2V(II)=2
         ENDDO
         Dy(2)=y(2)-y(1)
         yle=Dy(2)
      ENDIF
!---  Z-GRID 
!        Z(K): Z-coordinate of the Kth node
!        DZ(K): full cell size of the Kth node
      READ (nu_grid,1) TITLE
      READ (nu_grid,*) K1,ZZ
      DO K=1,K1
         Z(K)=ZZ
      ENDDO
140   READ (nu_grid,*) K2,ZZ
         IF (K2.LE.K1) GOTO 140      
         DEL=(ZZ-Z(K1))/(K2-K1)
         IF (K2.GT.LP.OR.DEL.LE.ZERO) then
            call stop_run("Error (with k) in Grid read.")
         endif
         DO K=K1+1,K2
            Z(K)=Z(K-1)+DEL
         ENDDO
         K1=K2
      IF (K2.LT.LPM1) GOTO 140
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
         height=(Z(LPM1)-Z(3)) !Lottes 6/2/05
      ELSE
         LKZ1=2
         LKZ2=2
         DO II=1,3
            LKZ1V(II)=2
            LKZ2V(II)=2
         ENDDO
         DZ(2)=Z(2)-Z(1)
         height=DZ(2) !Lottes 6/2/05
      ENDIF
      !Lottes 5/5/05:
      !Cell type map, IBCELL (old blocked cell indicator array)
      !
      !      (-1,9 on input)-half blocked (used in 2D, meaningless in 3D)
      !      0 - open to flow
      !      1 - wall, 2 - inlet,   3-exit
      !      4-interface,  5-ebooster,   8-bubbler
      !
      !      Multiple exits, electric boosters, and bubblers are assigned numbers
      !               based on the order they appear in the SBC file, and the object number
      !               is saved in the tens digit of IBCELL.

      ALLOCATE (IBC(NP))
      IBCELL=0
      IBC=""
      READ (nu_grid,1) TITLE
      K1=0
      !  This is really the start of a double loop
210   READ (nu_grid,*) K2
      I1=0
220   J1=2
      DO WHILE (J1.LT.NP)
         J2=J1+138
         IF (J2.GT.NP) J2=NP
         M=(J2-J1)/2+1
         IF (J1.EQ.2) THEN
            READ (nu_grid,'(I4,2X,<M>A1)') I2,(IBC(J),J=J1,J2,2)
         ELSE
            READ (nu_grid,'(6X,<M>A1)') I2,(IBC(J),J=J1,J2,2)
         ENDIF
         J1=J2+2
      ENDDO
      DO 225 K=K1+2,K2,2
      DO 225 I=I1+2,I2,2
      DO 225 J=2,NP,2
         IF (IBC(J).EQ."1") IBCELL(I,J,K)=1
         IF (IBC(J).EQ."2") IBCELL(I,J,K)=2
         IF (IBC(J).EQ."3") IBCELL(I,J,K)=3
         IF (IBC(J).EQ."4") IBCELL(I,J,K)=4
         IF (IBC(J).EQ."5") IBCELL(I,J,K)=5
         IF (IBC(J).EQ."8") IBCELL(I,J,K)=8
         IF (IBC(J).EQ."9") IBCELL(I,J,K)=-1
         !csl         IF (IBC(J).EQ."r") IBCELL(I,J,K)=55
         !csl         IF (IBC(J).EQ."s") IBCELL(I,J,K)=65
         !csl         IF (IBC(J).EQ."t") IBCELL(I,J,K)=75
         !csl         IF (IBC(J).EQ."r") EB_C=.TRUE.
225   CONTINUE
      I1=I2
      IF (I2.LT.MP) GOTO 220
      K1=K2   
      IF (K2.LT.LP) GOTO 210
      !  This is the end of a double loop


      READ (nu_grid,NML=GRD) !Get grd namelist
      CLOSE(10)

      !     Determine number of computational cells in grid (ncells)
      !     Set grid cell type values on faces and edges
      NCELLS=0
      vol_tot=zero
      DO I=2,MP,2
      DO J=2,NP,2
      DO K=2,LP,2
         IBC1=IBCELL(I,J,K)
         IF (IBC1.LE.0) THEN
            NCELLS=NCELLS+1
            vol_tot=vol_tot+dx(i)*dy(j)*dz(k)
            cycle
         ENDIF
         DO I1=I-1,I+1
         DO J1=J-1,J+1
         DO K1=K-1,K+1
            IF (IBCELL(I1,J1,K1).NE.1) IBCELL(I1,J1,K1)=IBC1
         enddo;enddo;enddo
      enddo;enddo;enddo
      IF (IDEBUG.GE.1) call print_grid !print out grid 
!----------------------------------------------------------------------
300   CONTINUE
      !  Adjust exit cell types to indicate exit direction
      DO 310 I=3,MP-1,2
      DO 310 J=2,NP
      DO 310 K=2,LP
         IF (IBCELL(I,J,K).NE.3) GOTO 310
         IF (IBCELL(I+1,J,K).GE.1.AND.IBCELL(I-1,J,K).GE.1) GOTO 310
         IBCELL(I,J,K)=IBCELL(I,J,K)+100 !Lottes: x-direction exit
         IF (IBCELL(I+1,J,K).LE.0) IBCELL(I,J,K)=IBCELL(I,J,K)+10 !negative direction exit
310   CONTINUE
      DO 320 J=3,NP-1,2
      DO 320 I=2,MP
      DO 320 K=2,LP
         IF (IBCELL(I,J,K).NE.3) GOTO 320
         IF (IBCELL(I,J+1,K).GE.1.AND.IBCELL(I,J-1,K).GE.1) GOTO 320
         IBCELL(I,J,K)=IBCELL(I,J,K)+200 !Lottes: y-direction exit
         IF (IBCELL(I,J+1,K).LE.0) IBCELL(I,J,K)=IBCELL(I,J,K)+10 !negative direction exit
320   CONTINUE
      DO 330 K=3,LP-1,2
      DO 330 I=2,MP
      DO 330 J=2,NP
         IF (IBCELL(I,J,K).NE.3) GOTO 330
         IF (IBCELL(I,J,K+1).GE.1.AND.IBCELL(I,J,K-1).GE.1) GOTO 330
         IBCELL(I,J,K)=IBCELL(I,J,K)+300 !Lottes: z-direction exit
         IF (IBCELL(I,J,K+1).LE.0) IBCELL(I,J,K)=IBCELL(I,J,K)+10 !negative direction exit
330   CONTINUE
      DEALLOCATE (IBC)

      call interface_center_m !Finds interface bounds and creates centered x & y grids

      !------------------ @@@ need a value passed in from the gui
      i_exit = i_me !for now set start of exit tunnels in x-direction to end of melt tank

!Lottes 6/5/05: Finds main melt tank ending index in x-direction
!               Hunts down centerline, won't work for all geometries!
!
!     K=LP
!     I_ME=2
!     DO 350 J=NP/2-4,NP/2+4,2
!        I1=0
!        DO I=2,MP,2
!           IBC0=IBCELL(I,J,K)
!           IF (I1.EQ.0.AND.IBC0.EQ.4) THEN
!               I1=I
!           ELSEIF (I1.GT.0.AND.IBC0.EQ.1) THEN
!              I1=I-2
!              EXIT
!           ENDIF
!        ENDDO
!        IF (I1.GT.I_ME) I_ME=I1
!350   CONTINUE
      !------------------------------------------------------------------
      !Lottes 6/5/05: Finds refiner beginning index (i_rb) in x-direction
      I_RB=MP
      DO J=2,NP,2
         I1=0
         DO I=MP,2,-2
            IBC0=IBCELL(I,J,lp)
            IF (I1.EQ.0.AND.IBC0.EQ.4) THEN
               I1=I
            ELSEIF (I1.GT.0.AND.IBC0.EQ.1) THEN
               I1=I+2
               EXIT
            ENDIF
         ENDDO
         IF (I1.LT.I_RB.AND.I1.GT.0) I_RB=I1
      enddo

      IF (I_RB.LT.I_ME) THEN
         I_RB=MP
         I_CH=MP
         F_BUOY=0 !Lottes 6/5/05 What is this? @@@
      ELSE
         I_CH=(I_ME+I_RB)/2+1
      ENDIF
      !-------------------------------------------------------------------------
      !Compute surface area of melt in main tank (exclude refiner) Lottes 6/5/05
      area_surf_tot = zero
      do i=2,i_me,2
      do j=2,np,2
         if(ibcell(i,j,lp) .ne. 4) cycle
           area_surf_tot = area_surf_tot + dx(i)*dy(j)
      enddo;enddo

      !Normal return point Lottes
      RETURN
      end

!======================================================================
!     Print grid
!======================================================================
      SUBROUTINE print_grid
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1, ALLOCATABLE :: IBC(:)

2     FORMAT (I5,2G15.5)
3     FORMAT (I4,2X,<NP>A1)

      allocate(ibc(np))
      WRITE(20,*) "X:"
      WRITE (20,2) (I,X(I),DX(I),I=1,MP)
      WRITE(20,*) "Y:"
      WRITE (20,2) (J,y(J),dy(J),J=1,NP)
      WRITE(20,*) "Z:"
      WRITE (20,2) (K,Z(K),DZ(K),K=1,LP)
      WRITE(20,*) "IBCELL:"
      DO K=2,LP
         WRITE(20,2) K
         DO I=2,MP
            DO J=2,NP
               IF (IBCELL(I,J,K).EQ.0) IBC(J)="0"
               IF (IBCELL(I,J,K).EQ.1) IBC(J)="1"
               IF (IBCELL(I,J,K).EQ.2) IBC(J)="2"
               IF (IBCELL(I,J,K).EQ.3) IBC(J)="3"
               IF (IBCELL(I,J,K).EQ.4) IBC(J)="4"
               IF (IBCELL(I,J,K).EQ.5) IBC(J)="5"
               IF (IBCELL(I,J,K).EQ.6) IBC(J)="6"
               IF (IBCELL(I,J,K).EQ.7) IBC(J)="7"
               IF (IBCELL(I,J,K).EQ.8) IBC(J)="8"
               IF (IBCELL(I,J,K).EQ.-1) IBC(J)="9"
                  !csl               IF (IBCELL(I,J,K).EQ.55) IBC(J)="r"
                  !csl               IF (IBCELL(I,J,K).EQ.65) IBC(J)="s"
                  !csl               IF (IBCELL(I,J,K).EQ.75) IBC(J)="t"
            ENDDO
            WRITE(20,3) I,(IBC(J),J=2,NP)
         ENDDO
      ENDDO
      DEALLOCATE (IBC)
      return
      END

!======================================================================
!     DXAV: x MOMENTUM CELL WIDTH 
!     DyAV: y momentum CELL width
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
      DOUBLE PRECISION FUNCTION DYAV(I,J,K)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (IBCELL(I,J-2,K).GE.1) THEN
         DYAV=y(J+1)-y(J-2)
      ELSEIF (IBCELL(I,J+2,K).GE.1) THEN
         DYAV=y(J+2)-y(J-1)
      ELSE
         DYAV=Dy(J)
      ENDIF
      RETURN
      END


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
      DOUBLE PRECISION FUNCTION VOL_C0(I,J,K)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /GRD1/BV_F(2),BA_F(3,2),BD_F(3,2)
      VOL=DX(I)*dy(J)*DZ(K)
      IF (IBCELL(I,J,K).LE.-1) THEN
         IBC=-IBCELL(I,J,K)
         VOL=VOL*BV_F(IBC)
      ENDIF
      VOL_C0=VOL
      RETURN
      END


!======================================================================
      DOUBLE PRECISION FUNCTION AREA_C(I,J,K,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /GRD1/BV_F(2),BA_F(3,2),BD_F(3,2)
      IBC0=MOD(IBCELL(I,J,K),10)
      IF (IBC0.EQ.1) THEN
         AREA_C=ZERO
         RETURN
      ENDIF
      IF (IX.EQ.1) THEN
         AREA=dy(J)*DZ(K)
      ELSEIF (IX.EQ.2) THEN
         AREA=DX(I)*DZ(K)
      ELSE
         AREA=DX(I)*dy(J)
      ENDIF
      AREA_C=AREA
      IF (IBCELL(I,J,K).EQ.0) RETURN
      IF (IBCELL(I,J,K).LE.-1) THEN
         IBC=-IBCELL(I,J,K)
         GOTO 200
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
200   CONTINUE
      !CZ      AREA_C=AREA_C*BA_F(IBC,IX)
      RETURN
      END


!======================================================================
      SUBROUTINE IBCX(I,J,K,IX)
      USE GBL_VAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IBC0=IBCELL(I,J,K)
      IF (IBC0.LE.0) RETURN
      IX=IBC0
      IBC0=MOD(IBC0,10)
      IF (IBC0.NE.1) RETURN
      DO NDR=1,6
         I2=I
         J2=J
         K2=K
         IF (NDR.EQ.1) THEN
            I2=MAX(2,I-2)
            IX=101
         ELSEIF (NDR.EQ.2) THEN
            I2=MIN(MP,I+2)
            IX=111
         ELSEIF (NDR.EQ.3) THEN
            J2=MAX(2,J-2)
            IX=201
         ELSEIF (NDR.EQ.4) THEN
            J2=MIN(NP,J+2)
            IX=211
         ELSEIF (NDR.EQ.5) THEN
            K2=MAX(2,K-2)
            IX=301
         ELSEIF (NDR.EQ.6) THEN
            K2=MIN(LP,K+2)
            IX=311
         ENDIF
         IF (IBCELL(I2,J2,K2).LE.0) RETURN
      ENDDO
      IX=1
      END


      !======================================================================
      !======================================================================
      !======================================================================
      !Subroutine interface_center_m
      !
      ! Sets up variables for interpolation between combustion and melt
      ! grid surface interface
      !
      ! Finds center coordinates of melt interface area in combustion grid
      ! Finds least and greatest index of melt surface interface
      ! in x and y directions
      !
      ! Computes shifted x & y grid vectors relative to melt interface center
      !======================================================================
      subroutine interface_center_m
      use gbl_var
      implicit double precision (a-h,o-z)
      
      !Find main melt tank ending index in x-direction

      !do i=4,mp-2,2
      !   if (x(i)<melter_length) cycle
      !   if (x(i)>=melter_length) then
      !      i_me=i
      !      exit
      !   endif
      !enddo

      !i_me=4
      !do j=4,np-2,2
      !   i_me_y=0
      !   do i=4,mp-2,2
      !      if (i_me_y==0.and.ibcell(i,j,lp)==4) then
      !         i_me_y=i
      !      elseif (i_me_y>0.and.ibcell(i,j,lp)==1) then
      !         i_me_y=i-2
      !         exit
      !      endif
      !   enddo
      !   if (i_me_y > i_me) i_me=i_me_y
      !enddo

      x_me = x_mb+surf_length_m !x value of end of main melt surface
      y_me = y_mb+surf_width_m  !y value of end of main melt surface
      !Find i index of least x cell center of main melt surface
      do i=2,mp-2,2
         i_mb=i
         if(x(i)>x_mb) exit
      enddo
      
      !Find i index of greatest x cell center of main melt surface
      do i=mp,4,-2
         i_me=i
         if(x(i)<x_mb+surf_length_m) exit
      enddo

      !Find j index of least y cell center of main melt surface
      do j=2,np-2,2
         j_mb=j
         if(y(j)>y_mb) exit
      enddo

      !Find j index of greatest y cell center of main melt surface
      do j=np,4,-2
         j_me=j
         if(y(j)<y_mb+surf_width_m) exit
      enddo

        

      x_cent_m=(x_mb+surf_length_m)/2
      y_cent_m=(y_mb+surf_width_m)/2

      !i_mb=mp-2
      !j_mb=np-2
      !j_me=4
      !do i=4,mp-2,2
      !do j=4,np-2,2
      !   if (ibcell(i,j,lp)==4) then
      !      if(i_mb > i) i_mb = i !least x index of melt interface
      !      if(j_mb > j) j_mb = j !least y index of melt interface
      !      if(j_me < j) j_me = j !greatest y index of melt interface
      !   endif
      !enddo;enddo
      !x_surfb_m=x(i_mb-1)
      !x_surfe_m=x(i_me+1)
      !y_surfb_m=y(j_mb-1)
      !y_surfe_m=y(j_me+1)
      
      !x_cent_m=(x_surfb_m+x_surfe_m)/2
      !y_cent_m=(y_surfb_m+y_surfe_m)/2
      
      !Compute shifted x & y grid point vectors relative to melt interface center
      do i=2,mp
         xs_m(i)=x(i)-x_cent_m
      enddo
      
      do j=2,np
         ys_m(j)=y(j)-y_cent_m
      enddo

      return
      end
