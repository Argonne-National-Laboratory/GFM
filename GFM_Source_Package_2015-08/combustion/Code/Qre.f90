!==============================================================================
!==============================================================================
!==============================================================================
!
! Qre.f90
!
!==============================================================================
!
!     Emissive Fluxes: x-,y-, and z-directions
!
!-----------------------------------------------------
!General flow of routine:
!  set qel=0
!  return if not open or surface cell
!  set qel0 based on deb * absorptivity {akl} * Stefan-Boltzmann constant * T^4  {min clipped}
!
!  if id_rad<10 & surface cell then return
!  if id_rad<10 & open cell then 
!     set qel1 = qel0 * 2 * area in each x,y,z direction
!     set qel = sum of qel1
!     set qe = sum of qel over wave bands
!  if rad_id=10 & surface cell then
!     do Z fluxes - sets qel,qal,ang, calls a few subroutines
!     set qe = sum of qel over wave bands
!  if rad_id=10 & open cell then
!     do X fluxes
!     do Y fluxes
!     do Z fluxes
!     set qe = sum of qel over wave bands
!-----------------------------------------------------
SUBROUTINE QREF(I0,J0,K0)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!Called only from grnf routine triple-do in Gas Emission and Absorption Section
!i0,j0,k0 are all even
!Called only for open cells    10-26-05

idebug_qref=0
qa_cell=zero
qa_sum_point=zero
qa_sum_bnd=zero
qa_sum_vol=zero

qa_sum_to_open_cell=zero
qa_sum_to_melt_cell=zero
qa_sum_to_inlet_cell=zero
qa_sum_to_exit_cell=zero
qa_sum_to_wall_cell=zero

I0D2=I0/2
J0D2=J0/2
K0D2=K0/2
QEL(I0D2,J0D2,K0D2,1:NWL)=zero
IBC0=IBCELL(I0,J0,K0)

!  10-26-05 radiation from melt cells done in Qwall now
   !IF (IBC0.GE.1.AND.IBC0.NE.4) RETURN !leave if wall, inlet, or exit
   !stay in this routine only for open cells and melt surface

IF (IBC0.GE.1) RETURN !leave if wall, inlet, exit, or melt surface

G0=SGN*T(I0,J0,K0)**4
if(i0==20.and.j0==8.and.k0==2)then
  check=0
endif
!qel0=rate of emission per unit area in wavelength band L
!deb adds to one, so sum qel0 should be qe/(cell area) at the point
area_cell=2*(dr(j0)*dz(k0)+dr(j0)*dx(i0)+dz(k0)*dx(i0))
qe_point2=0

DO L=1,NWL
   QEL0(L)=DEB(L)*AKL(I0D2,J0D2,K0D2,L)*G0 !cell emission/area for wavelength band L
                                           !qel0=deb*akl*sgn*T^4
   !IF (QEL0(L).LE.QEL_MN) QEL0(L)=zero !if qel0 <= one, then change to =0  
   qe_point2=qe_point2 + qel0(l)*area_cell
ENDDO

!IF (ID_RAD.GE.10) GOTO 110 !jump if vol calc  !This routine is only called with id_rad=10

!IF (IBC0.EQ.4) RETURN !leave for melt surf

!QEL1=QEL0*DR(J0)*DZ(K0)*2
!QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1
!QEL1=QEL0*DX(I0)*DZ(K0)*2
!QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1
!QEL1=QEL0*DX(I0)*DR(J0)*2
!QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1
!Here qel has total rate of emission [W] for each wavelength band from a cell
!GOTO 800

!110 continue 

!IF (IBCELL(I0,J0,K0).EQ.4) GOTO 300 !Do z flux only
!IF (IBCELL(I0,J0,K0).EQ.4) return !Do not trace radiation from the melt surface
                                  !it is now handled in wallq.

!-----------------------------------------------------
!     X-Flux
!-----------------------------------------------------
IDX=1
QEL1=QEL0*DR(J0)*DZ(K0) !rad energy emission rate through cell face
ALLOCATE (QTX1(NZ,LZ,NWL),QTX2(NZ,LZ,NWL),Q2M(NZ,LZ,NWL))
ALLOCATE (IQT1(NZ,LZ),IQTM(NZ,LZ),IQT2(NZ,LZ))
QTX1=zero
Q2M=zero
QTX2=zero
IQT1=.TRUE.
IQTM=.TRUE.
IQT2=.TRUE.
DO IPM=-2,2,4
   QTX1(J0D2,K0D2,1:NWL)=QEL1 !power through x face of j0,k0 available for absorption
   IQT1(J0D2,K0D2)=.FALSE. !why do this inside the loop?
   QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1 !accumulates total emission from i0,j0,k0

   !CALL SANG1(IDX,I0,J0,K0,IPM,IEND)
   !sang1 expanded here with idx=1
   !sets angles and loop limits
   if (ibcell(i0,j0,k0).eq.4) then !melt surface
      ang0=90
      ang0(1)=0
   elseif (ibcell(i0,j0,k0).ge.100) then !wall
      ang0=60
      ang0(1)=0
   else
      i1=i0+ipm/2
      j1=j0+ipm/2
      k1=k0+ipm/2
      !call sfac(ang0,i0,j0,k0,i1,j1,k1,idx) !expanded here
      !set ang0
      ang0(1)=0
      b=abs(x(i1)-x(i0))
      if (b.lt.small16) then
         ang0(2)=90
         ang0(3)=90
      else
         h2=abs(r(j1)-r(j0))/b
         ang0(2)=atand(h2)
         h3=abs(z(k1)-z(k0))/b
         ang0(3)=atand(h3)
      endif
      !end sfac expansion
   endif

   ang2=ang0(2)
   ang2(j0,1:2)=zero
   ang3=ang0(3)
   ang3(k0,1:2)=zero
   !call sweep(idx,ipm,iend) !sets iend to loop limit
   if (ipm.lt.0) then; iend=2
                 else; iend=mp; endif
   !end of Sang1 expansion

   !debug code
   if(idebug_qref==1)then
   x_face_pow=0
   do ll=1,nwl
      x_face_pow=x_face_pow+qtx1(j0d2,k0d2,ll)
   enddo
   endif

   I2=I0+IPM !slab in front of or behind cell
   DO I=I2,IEND,IPM !from next slab to end wall
      ID2=I/2
      I10=I-IPM

      !debug code
      if(idebug_qref==1)then
      qtx1_sum=0
      do j=2,np,2
      do k=2,lp,2
      do l=1,nwl
         qtx1_sum=qtx1_sum+qtx1(j/2,k/2,l)
      enddo;enddo;enddo
      endif

      CALL QRTX(I0,J0,K0,I10,I) !computes arrival rate of rad energy at cells or faces in next slab?
                                !qtx2(j,k,l) has arrival rate from (i0,j0,k0) for wave band l?
   
      !debug code
      if(idebug_qref==1)then
      qtx2_sum=0
      do j=2,np,2
      do k=2,lp,2
      do l=1,nwl
         qtx2_sum=qtx2_sum+qtx2(j/2,k/2,l)
      enddo;enddo;enddo
      endif

      DO J=2,NP,2; JD2=J/2
      DO K=2,LP,2; KD2=K/2
         IF (IQT2(JD2,KD2)) cycle
         DO L=1,NWL
            IF (QTX2(JD2,KD2,L).LT.small16) CYCLE
            IF (IBCELL(I,J,K).GE.1.OR.I.EQ.IEND) THEN
               QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QTX2(JD2,KD2,L)
               qa_sum_bnd=qa_sum_bnd+QTX2(JD2,KD2,L)
               if (ibcell(i,j,k).eq.4) then
                  qa_sum_to_melt_cell=qa_sum_to_melt_cell+QTX2(JD2,KD2,L)
               elseif (ibcell(i,j,k).eq.3) then
                  qa_sum_to_exit_cell=qa_sum_to_exit_cell+QTX2(JD2,KD2,L)
               elseif (ibcell(i,j,k).eq.2) then
                  qa_sum_to_inlet_cell=qa_sum_to_inlet_cell+QTX2(JD2,KD2,L)
               else
                  qa_sum_to_wall_cell=qa_sum_to_wall_cell+QTX2(JD2,KD2,L)
               endif
            ELSE
               QA0=QTX2(JD2,KD2,L)*AKL(ID2,JD2,KD2,L)
               QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QA0
               QTX1(JD2,KD2,L)=QTX2(JD2,KD2,L)-QA0
               IQT1(JD2,KD2)=.FALSE.
               qa_sum_vol=qa_sum_vol+qa0
               qa_sum_to_open_cell=qa_sum_to_open_cell+qa0
            ENDIF
            QTX2(JD2,KD2,L)=0
            IQT2(JD2,KD2)=.TRUE.
         ENDDO
         qa_cell=qa_sum_vol+qa_sum_bnd !debug code
      enddo;enddo
      ANG2(1:NP,1)=ANG2(1:NP,2)
      ANG3(1:LP,1)=ANG3(1:LP,2)
   enddo
enddo
DEALLOCATE (QTX1,QTX2,Q2M,IQT1,IQTM,IQT2)
!CSL  QAS=SUM(QAL)
!CSL  QES=SUM(QEL)
!CSL  WRITE (6,'(3I5,2E12.4)') I0,J0,K0,QES,QAS

!-----------------------------------------------------
!     Y-Flux
!-----------------------------------------------------

IDX=2
QEL1=QEL0*DX(I0)*DZ(K0)
ALLOCATE (QTX1(MZ,LZ,NWL),QTX2(MZ,LZ,NWL),Q2M(MZ,LZ,NWL))
ALLOCATE (IQT1(MZ,LZ),IQT2(MZ,LZ),IQTM(MZ,LZ))
QTX1=zero
Q2M=zero
QTX2=zero
IQT1=.TRUE.
IQTM=.TRUE.
IQT2=.TRUE.

DO JPM=-2,2,4
   QTX1(I0D2,K0D2,1:NWL)=QEL1
   IQT1(I0D2,K0D2)=.FALSE.
   QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1

   !CALL SANG1(IDX,I0,J0,K0,JPM,JEND)
   ! sang1 expanded here with idx=2 
   !sets angles and loop limits
   if (ibcell(i0,j0,k0).eq.4) then !melt surface
      ang0=90
      ang0(2)=0
   elseif (ibcell(i0,j0,k0).ge.100) then !wall
      ang0=60
      ang0(2)=0
   else
      i1=i0+jpm/2
      j1=j0+jpm/2
      k1=k0+jpm/2
      !call sfac(ang0,i0,j0,k0,i1,j1,k1,idx) !expanded here
      !set ang0
      ang0(2)=0
      b=abs(r(j1)-r(j0))
      if (b.lt.small16) then
         ang0(1)=90
         ang0(3)=90
      else
         h1=abs(x(i1)-x(i0))/b
         ang0(1)=atand(h1)
         h3=abs(z(k1)-z(k0))/b
         ang0(3)=atand(h3)
      endif
      !end sfac expansion
   endif
   ang1=ang0(1)
   ang1(i0,1:2)=zero
   ang3=ang0(3)
   ang3(k0,1:2)=zero
   if (jpm.lt.0) then; jend=2
                 else; jend=np; endif
   !end of sang1 expansion


   !debug code
   if(idebug_qref==1)then
   y_face_pow=0
   do ll=1,nwl
      y_face_pow=y_face_pow+qtx1(i0d2,k0d2,ll)
   enddo
   endif

   J2=J0+JPM
   DO J=J2,JEND,JPM
      JD2=J/2
      J10=J-JPM

      !debug code
      if(idebug_qref==1)then
      qtx1_sum=0
      do i=2,mp,2
      do k=2,lp,2
      do l=1,nwl
         qtx1_sum=qtx1_sum+qtx1(i/2,k/2,l)
      enddo;enddo;enddo !end debug
      endif

      CALL QRTY(I0,J0,K0,J10,J)

      !debug code
      if(idebug_qref==1)then
      qtx2_sum=0
      do i=2,mp,2
      do k=2,lp,2
      do l=1,nwl
         qtx2_sum=qtx2_sum+qtx2(i/2,k/2,l)
      enddo;enddo;enddo !end debug
      endif

      DO I=2,MP,2; ID2=I/2
      DO K=2,LP,2; KD2=K/2
         IF (IQT2(ID2,KD2)) cycle
         DO L=1,NWL
            IF (QTX2(ID2,KD2,L).LT.small16) CYCLE
            IF (IBCELL(I,J,K).GE.1.OR.J.EQ.JEND) THEN
               QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QTX2(ID2,KD2,L)
               qa_sum_bnd=qa_sum_bnd+QTX2(iD2,KD2,L)
               if (ibcell(i,j,k).eq.4) then
                  qa_sum_to_melt_cell=qa_sum_to_melt_cell+qtx2(id2,kd2,L)
               elseif (ibcell(i,j,k).eq.3) then
                  qa_sum_to_exit_cell=qa_sum_to_exit_cell+qtx2(id2,kd2,L)
               elseif (ibcell(i,j,k).eq.2) then
                  qa_sum_to_inlet_cell=qa_sum_to_inlet_cell+qtx2(id2,kd2,L)
               else
                  qa_sum_to_wall_cell=qa_sum_to_wall_cell+qtx2(id2,kd2,L)
               endif
            ELSE
               QA0=QTX2(ID2,KD2,L)*AKL(ID2,JD2,KD2,L)
               QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QA0
               QTX1(ID2,KD2,L)=QTX2(ID2,KD2,L)-QA0
               IQT1(ID2,KD2)=.FALSE.
               qa_sum_vol=qa_sum_vol+qa0
               qa_sum_to_open_cell=qa_sum_to_open_cell+qa0
            ENDIF
            QTX2(ID2,KD2,L)=0
            IQT2(ID2,KD2)=.TRUE.
         ENDDO
         qa_cell=qa_sum_vol+qa_sum_bnd !debug code
      enddo;enddo
      ANG1(1:MP,1)=ANG1(1:MP,2)
      ANG3(1:LP,1)=ANG3(1:LP,2)
   enddo  
enddo
DEALLOCATE (QTX1,QTX2,Q2M,IQT1,IQTM,IQT2)
!CSL  QAS=SUM(QAL)
!CSL  QES=SUM(QEL)
!CSL  WRITE (6,'(3I5,2E12.4)') I0,J0,K0,QES,QAS

!-----------------------------------------------------
!     Z-Flux
!-----------------------------------------------------

300 continue

IDX=3
QEL1=QEL0*DX(I0)*DR(J0)
ALLOCATE (QTX1(MZ,NZ,NWL),QTX2(MZ,NZ,NWL),Q2M(MZ,NZ,NWL))
ALLOCATE (IQT1(MZ,NZ),IQT2(MZ,NZ),IQTM(MZ,NZ))
QTX1=zero
Q2M=zero
QTX2=zero
IQT1=.TRUE.
IQTM=.TRUE.
IQT2=.TRUE.
DO KPM=-2,2,4 !do plus and minus side of cell
   !IF (IBCELL(I0,J0,K0).EQ.4.AND.KPM.EQ.-2) cycle !only go in + direction from melt surface
   QTX1(I0D2,J0D2,1:NWL)=QEL1
   IQT1(I0D2,J0D2)=.FALSE.
   QEL(I0D2,J0D2,K0D2,1:NWL)=QEL(I0D2,J0D2,K0D2,1:NWL)+QEL1

   !CALL SANG1(IDX,I0,J0,K0,KPM,KEND)
   !sang1 expanded here with idx=3
   !sets angles and loop limits
   if (ibcell(i0,j0,k0).eq.4) then !melt surface
      ang0=90
      ang0(3)=0
   elseif (ibcell(i0,j0,k0).ge.100) then !wall
      ang0=60
      ang0(3)=0
   else
      i1=i0+kpm/2
      j1=j0+kpm/2
      k1=k0+kpm/2
      !call sfac(ang0,i0,j0,k0,i1,j1,k1,idx) !expanded here
      !set ang0
      ang0(3)=0
      b=abs(z(k1)-z(k0))
      if (b.lt.small16) then
         ang0(1)=90
         ang0(2)=90
      else
         h1=abs(x(i1)-x(i0))/b
         ang0(1)=atand(h1)
         h2=abs(r(j1)-r(j0))/b
         ang0(2)=atand(h2)
      endif
      !end sfac expansion
   endif

   ang1=ang0(1)
   ang1(i0,1:2)=zero
   ang2=ang0(2)
   ang2(j0,1:2)=zero

   if (kpm.lt.0) then; kend=2
                 else; kend=lp; endif
   !end of sang1 expansion


   !debug code
   if(idebug_qref==1)then
   z_face_pow=0
   do ll=1,nwl
      z_face_pow=z_face_pow+qtx1(i0d2,j0d2,ll)
   enddo
   endif

   K2=K0+KPM
   DO K=K2,KEND,KPM !do for each z slab away from cell
      KD2=K/2
      K10=K-KPM

      !debug code
      if(idebug_qref==1)then
      qtx1_sum=0
      do j=2,np,2
      do i=2,mp,2
      do l=1,nwl
         qtx1_sum=qtx1_sum+qtx1(i/2,j/2,l)
      enddo;enddo;enddo !end debug
      endif

      CALL QRTZ(I0,J0,K0,K10,K)

      !debug code
      if(idebug_qref==1)then
      qtx2_sum=0
      do j=2,np,2
      do i=2,mp,2
      do l=1,nwl
         qtx2_sum=qtx2_sum+qtx2(i/2,j/2,l)
      enddo;enddo;enddo !end debug
      endif

      DO I=2,MP,2; ID2=I/2 !do for each 
      DO J=2,NP,2; JD2=J/2 !cell in slab
         IF (IQT2(ID2,JD2)) cycle
         DO L=1,NWL
            IF (QTX2(ID2,JD2,L).LT.small16) CYCLE
            IF (IBCELL(I,J,K).GE.1.OR.K.EQ.KEND) THEN
               QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QTX2(ID2,JD2,L)
               qa_sum_bnd=qa_sum_bnd+QTX2(iD2,jD2,L)

               !When do we account for reflection at boundaries

               if (ibcell(i,j,k).eq.4) then
                  qa_sum_to_melt_cell=qa_sum_to_melt_cell+qtx2(id2,jd2,L)
               elseif (ibcell(i,j,k).eq.3) then
                  qa_sum_to_exit_cell=qa_sum_to_exit_cell+qtx2(id2,jd2,L)
               elseif (ibcell(i,j,k).eq.2) then
                  qa_sum_to_inlet_cell=qa_sum_to_inlet_cell+qtx2(id2,jd2,L)
               else
                  qa_sum_to_wall_cell=qa_sum_to_wall_cell+qtx2(id2,jd2,L)
               endif
            ELSE
               QA0=QTX2(ID2,JD2,L)*AKL(ID2,JD2,KD2,L)
               QAL(ID2,JD2,KD2,L)=QAL(ID2,JD2,KD2,L)+QA0
               QTX1(ID2,JD2,L)=QTX2(ID2,JD2,L)-QA0
               IQT1(ID2,JD2)=.FALSE.
               qa_sum_vol=qa_sum_vol+qa0
               qa_sum_to_open_cell=qa_sum_to_open_cell+qa0
            ENDIF
            QTX2(ID2,JD2,L)=0
            IQT2(ID2,JD2)=.TRUE.
         ENDDO
         qa_cell=qa_sum_vol+qa_sum_bnd !debug code
      enddo;enddo
      ANG1(1:MP,1)=ANG1(1:MP,2)
      ANG2(1:NP,1)=ANG2(1:NP,2)
   enddo
enddo
DEALLOCATE (Q2M,QTX1,QTX2,IQT1,IQTM,IQT2)
!CSL  QAS=SUM(QAL)
!CSL  QES=SUM(QEL)
!CSL  WRITE (6,'(3I5,2E12.4)') I0,J0,K0,QES,QAS

800   continue

!set qe = sum of qel over wave bands
QE(I0D2,J0D2,K0D2)=QEL(I0D2,J0D2,K0D2,1)
DO L=2,NWL 
   QE(I0D2,J0D2,K0D2)=QE(I0D2,J0D2,K0D2)+QEL(I0D2,J0D2,K0D2,L)
ENDDO
qe_point=QE(I0D2,J0D2,K0D2)
qa_cell=qa_sum_vol+qa_sum_bnd
qa_sum_point=qa_cell

!if (ibc0.eq.4) then
!   qe_sum_from_melt=qe_sum_from_melt+qe(i0d2,j0d2,k0d2)
!else
   qe_sum_from_open=qe_sum_from_open+qe(i0d2,j0d2,k0d2)
!endif
RETURN
END


!===================================================================
!===================================================================
!===================================================================
!
!   9/21/05
!     Sang1 has been expanded in place and is no longer called
!
!===================================================================
SUBROUTINE SANG1(IDX,I0,J0,K0,LPM,LEND1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!sets angles and loop limits
IF (IBCELL(I0,J0,K0).EQ.4) THEN !melt surface
   ANG0=90
   ANG0(IDX)=0
ELSEIF (IBCELL(I0,J0,K0).GE.100) THEN !wall
   ANG0=60
   ANG0(IDX)=0
ELSE
   I1=I0+LPM/2
   J1=J0+LPM/2
   K1=K0+LPM/2
   !CALL SFAC(ANG0,I0,J0,K0,I1,J1,K1,IDX) !expanded here
   !set ang0
   ang0(idx)=0
   select case (idx)
   case (1)
      b=abs(x(i1)-x(i0))
      if (b.lt.small16) then
         ang0(2)=90
         ang0(3)=90
      else
         h2=abs(r(j1)-r(j0))/b
         ang0(2)=atand(h2)
         h3=abs(z(k1)-z(k0))/b
         ang0(3)=atand(h3)
      endif
   case (2)
      b=abs(r(j1)-r(j0))
      if (b.lt.small16) then
         ang0(1)=90
         ang0(3)=90
      else
         h1=abs(x(i1)-x(i0))/b
         ang0(1)=atand(h1)
         h3=abs(z(k1)-z(k0))/b
         ang0(3)=atand(h3)
      endif
   case (3)
      b=abs(z(k1)-z(k0))
      if (b.lt.small16) then
         ang0(1)=90
         ang0(2)=90
      else
         h1=abs(x(i1)-x(i0))/b
         ang0(1)=atand(h1)
         h2=abs(r(j1)-r(j0))/b
         ang0(2)=atand(h2)
      endif
   end select
   !end SFAC expansion
ENDIF

IF (IDX.NE.1) THEN
   ANG1=ANG0(1)
   ANG1(I0,1:2)=zero
ENDIF
IF (IDX.NE.2) THEN
   ANG2=ANG0(2)
   ANG2(J0,1:2)=zero
ENDIF
IF (IDX.NE.3) THEN
   ANG3=ANG0(3)
   ANG3(K0,1:2)=zero
ENDIF
!CALL SWEEP(IDX,LPM,LEND) !sets lend to loop limit
if (lpm.lt.0) then; lend1=2
              else; lend1=mp3(idx); endif
RETURN
END



!===================================================================
!===================================================================
!   9/20/05
!     sfac has been placed inline, it is no longer called
!===================================================================
subroutine sfac(ang,i0,j0,k0,i1,j1,k1,idx)
use gbl_var
implicit double precision (a-h,o-z)
dimension ang(3)

ang(idx)=0
select case (idx)
case (1)
   b=abs(x(i1)-x(i0))
   if (b.lt.small16) then
      ang(2)=90
      ang(3)=90
   else
      h2=abs(r(j1)-r(j0))/b
      ang(2)=atand(h2)
      h3=abs(z(k1)-z(k0))/b
      ang(3)=atand(h3)
   endif
   return

case (2)
   b=abs(r(j1)-r(j0))
   if (b.lt.small16) then
      ang(1)=90
      ang(3)=90
   else
      h1=abs(x(i1)-x(i0))/b
      ang(1)=atand(h1)
      h3=abs(z(k1)-z(k0))/b
      ang(3)=atand(h3)
   endif
   return

case (3)
   b=abs(z(k1)-z(k0))
   if (b.lt.small16) then
      ang(1)=90
      ang(2)=90
   else
      h1=abs(x(i1)-x(i0))/b
      ang(1)=atand(h1)
      h2=abs(r(j1)-r(j0))/b
      ang(2)=atand(h2)
   endif
   return
end select
end




!===================================================================
!===================================================================
!===================================================================
!
!   9/20/05
!     Sweep has been placed inline, it is no longer called
!
!===================================================================
SUBROUTINE SWEEP(IDX,LPM,LEND1)
USE GBL_VAR
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!If second arg is negative, then sets third arg to 2
!                           else sets third arg to grid dimension of first arg direction
IF (LPM.LT.0) THEN
   LEND1=2
ELSE
   LEND1=MP3(IDX)
ENDIF

RETURN
END




