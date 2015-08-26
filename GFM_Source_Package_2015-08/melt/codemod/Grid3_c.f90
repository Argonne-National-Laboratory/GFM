!======================================================================
! Grid_c.f90
!======================================================================
! Lottes 6/19/05 - Port of grid routine from combustion space
!
! This routine reads in the combustion space grid.
! Combustion space grid and related variables are made unique
! by appending _c to variable names.
!
! The grid is used to provide geometry information for interpolation
! of boundary conditions at the melt surface interface between the 
! two spaces
!
! Grid_c reads the combustion grid from file, completes the 
! cell type map, ibcell_c, and builds other grid geometry arrays 
!
!======================================================================
!======================================================================
!======================================================================
subroutine grid_c
use gbl_var
implicit double precision (a-h,o-z)
integer, allocatable :: ibc_c(:)

1     format (A32)
2     format (I5,2G15.5)
3     format (I4,2X,<np_c>i1)

idebug_c=0
!----------------------------------------------------------------------
!     Read in combustion space grid 
!     r0_c: reference length (m)
!     
!     mp_c/2 = # of X grid points including boundary points
!     np_c/2 = # of Y or R grid points including boundary points
!     lp_c/2 = # of Z grid points including boundary points
!----------------------------------------------------------------------
filename=casedir//'\gd'//gdnum//'c.dat'
inquire(file=filename,exist=comb_grid_exist)
if (.not.comb_grid_exist) then
   if (iheat_flux_type==2 .or. iheat_flux_type==4) then
      !This is an error because the heat flux should be calculated by combustion code
      write(6,*)'Combustion grid file,', filename, 'not found in melt directory.'
      write(6,*)'This file is needed for boundary condition interpolation'
      write(6,*)'at the interface between the melt and combustion space.'
      call stop_run(" Combustion grid file gd....c.dat not found in melt directory.")
   endif
   return
endif
open (nu_gc,file=filename)
read (nu_gc,1) title
read (nu_gc,1) title
read (nu_gc,*) r0_c,mp_c,np_c,lp_c,surf_length_c,surf_width_c

allocate (x_c(mp_c),dx_c(mp_c),y_c(np_c),dy_c(np_c),z_c(lp_c),dz_c(lp_c))
allocate (xs_c(mp_c),ys_c(np_c))
x_c=0
xs_c=0
dx_c=0
y_c=0
ys_c=0
dy_c=0
z_c=0
dz_c=0
allocate (ibcell_c(mp_c,np_c,lp_c))
ibcell_c=0

!--- Combustion space x-grid ----------------------------------------
!        x_c(i):  x-coordinate of the i-th node
!        dx_c(i): i-th cell x-width
read (nu_gc,1) title
read (nu_gc,*) i1,xx
do i=1,i1
   x_c(i)=xx
enddo

do !read in x-coordinates
   read (nu_gc,*) i2,xx
   if (i2.LE.i1) cycle
   del=(xx-x_c(i1))/(i2-i1)
   if (i2 > mp_c.or.del.LE.zero) then
      write(*,*) 'Either a x_c component is greater than max x_c'
      write(*,*) 'or the x_c components are not in numerical order.'
      write(*,*) 'check around x_c coordinate:', i2, 'in the grid file.'
      call stop_run("Error in Grid_c read (with i).")
   endif
   do i=i1+1,i2
      x_c(i)=x_c(i-1)+del
   enddo
   i1=i2
   if (i2 > mp_c-2) exit
enddo

!Normalize distances
do i=1,mp_c-1
   x_c(i)=x_c(i)/r0_c
enddo
x_c(mp_c)=x_c(mp_c-1)

!Calculate cell widths
dx_c(1)=zero
do i=2,mp_c-1
   dx_c(i)=x_c(i+1)-x_c(i-1)
enddo  
dx_c(mp_c)=zero

if (idebug_c==2) write (20,2) (i,x_c(i),dx_c(i),i=1,mp_c)

!--- Combustion Space y-grid ------------------------------------------
!    y(j): y-coordinate of the j-th node
!    dy(j): j-th cell y-width
read (nu_gc,1) title
if (idebug_c==2) write(20,1) title
read (nu_gc,*) j1,yy
do j=1,j1
   y_c(j)=yy
enddo

do !read in y-coordinates
   read (nu_gc,*) j2,yy
   
   if (j2.LE.j1) cycle
   del=(yy-y_c(j1))/(j2-j1)
   if (j2.GT.np_c.OR.del.LE.zero) then
      write(*,*) 'Either a y_c component is greater than max y'
      write(*,*) 'or the y_c components are not in numerical order.'
      write(*,*) 'check around y_c coordinate:', j2
      call stop_run("Error in Grid_c read (with j).")
   endif
   
   do j=j1+1,j2
      y_c(j)=y_c(j-1)+del
   enddo
   j1=j2
   if (j2 > np_c-2) exit
enddo

!Normalize distances
do j=1,np_c-1
   y_c(j)=y_c(j)/r0_c
enddo
y_c(np_c)=y_c(np_c-1)

!Calculate cell widths
dy_c(1)=zero
do j=2,np_c-1
   dy_c(j)=y_c(j+1)-y_c(j-1)
enddo
dy_c(np_c)=zero

if (idebug_c==2) write (20,2) (j,y_c(j),dy_c(j),j=1,np_c)

!---Combustion Space z-grid ----------------------------------------------------
!     z_c(k):  z-coordinate of the kth node
!     dz_c(k): k-th cell z-height
read (nu_gc,1) title
if (idebug_c==2) write(20,1) title
read (nu_gc,*) k1,zz
do k=1,k1
   z_c(k)=zz
enddo

do !read in z-coordinates
   read (nu_gc,*) k2,zz
   if (k2.LE.k1) cycle   
   del=(zz-z_c(k1))/(k2-k1)
   if (k2 > lp_c .OR. del.LE.zero) then
      write(*,*) 'Either a z_c component is greater than max z_c'
      write(*,*) 'or the z_c components are not in numerical order.'
      write(*,*) 'check around z_c coordinate:', k2, 'in the grid file.'
      call stop_run("Error in Grid_c read (with k).")
   endif
   
   do k=k1+1,k2
      z_c(k)=z_c(k-1)+del
   enddo
   k1=k2
   if (k2 > lp_c-2) exit
enddo
!Normalize distances
do k=1,lp_c-1
   z_c(k)=z_c(k)/r0_c
enddo
z_c(lp_c)=z_c(lp_c-1)         

!Calculate cell widths
dz_c(1)=zero

do k=2,lp_c-1
   dz_c(k)=z_c(k+1)-z_c(k-1)
enddo
dz_c(lp_c)=zero

if (idebug_c==2) write (20,2) (k,z_c(k),dz_c(k),k=1,lp_c)

!---  Cell Map ----------------------------------------------
!     ibcell_c: blocked cell indicator,
!     0-regular, (-1,9)-half blocked
!     1-blocked,  2-inlet,   3-exit, 4-free surface interface

ibcell_c=0
read (nu_gc,1) title
if (idebug_c==2) write(20,1) title
allocate (ibc_c(np_c))
k1=0

do !read in cell center values of cell map
   read (nu_gc,*) k2
   i1=0
   
   do
      read (nu_gc,3) i2,(ibc_c(j),j=2,np_c,2)
      do k=k1+2,k2,2
      do I=i1+2,i2,2
      do j=2,np_c,2
         !if (ibc_c(j).GT.5) ibc_c(j)=ibc_c(j)-10
         ibcell_c(i,j,k)=ibc_c(j)
      enddo;enddo;enddo
      
      i1=i2
      if (i2 > mp_c-1) exit
   enddo
   k1=k2   
   if (k2 > lp_c-1) exit
enddo
close(nu_gc)

!     Determine number of computational cells in grid (ncells_c)
!     Set grid cell type values on faces and edges
ncells_c=0
do i=2,mp_c,2
do j=2,np_c,2
do k=2,lp_c,2
   ibc1=ibcell_c(i,j,k)
   if (ibc1 < 1) then
      ncells_c=ncells_c+1
      cycle
   endif
   do i1=i-1,i+1
   do j1=j-1,j+1
   do k1=k-1,k+1
      if (i1 > mp_c .OR. j1 > np_c .OR. k1 > lp_c) cycle
      if (ibcell_c(i1,j1,k1).NE.1) ibcell_c(i1,j1,k1)=ibc1
   enddo;enddo;enddo
enddo;enddo;enddo

if (idebug_c==2) then
   do k=2,lp_c
      write(20,3) k
      do i=2,mp_c
         write(20,3) i,(ibcell_c(i,j,k),j=2,np_c)
      enddo
   enddo
endif

deallocate (ibc_c)
call interface_center_c

allocate (qrsc(mp_c/2,np_c/2))

return
end

!======================================================================
!======================================================================
!======================================================================
!Subroutine interface_center_c
!
! Sets up variables for interpolation between combustion and melt
! grid surface interface
!
! Finds center coordinates of melt interface area in combustion grid
! Finds least and greatest index of melt surface interface
! in x and y directions
!
! Checks for interior walls
!
! Computes shifted x & y grid vectors relative to melt interface center
!======================================================================
subroutine interface_center_c
use gbl_var
implicit double precision (a-h,o-z)

      !Find i index of least x cell center of main melt surface
      do i=2,mp_c-2,2
         i_mb_c=i
         if(x_c(i)>x_mb_c) exit
      enddo
      
      !Find i index of greatest x cell center of main melt surface
      do i=mp_c,4,-2
         i_me_c=i
         if(x_c(i)<x_mb+surf_length_c) exit
      enddo

      !Find j index of least y cell center of main melt surface
      do j=2,np_c-2,2
         j_mb_c=j
         if(y_c(j)>y_mb_c) exit
      enddo

      !Find j index of greatest y cell center of main melt surface
      do j=np_c,4,-2
         j_me_c=j
         if(y_c(j)<y_mb_c+surf_width_c) exit
      enddo

      x_cent_c=(x_mb_c+surf_length_c)/2
      y_cent_c=(y_mb_c+surf_width_c)/2

!...........................................................................................................
!outdated code below does not handle dog houses
!find first x-row with interface cells
!
!i_mb_c=mp_c
!i_me_c=2
!j_mb_c=np_c
!j_me_c=2
!do i=2,mp_c,2
!do j=2,np_c,2
   !Note that this code may be a problem if there are dog houses with melt surface.
   !What is needed here is to find the greatest and least extend of the near rectanbular
   !melt surface area. The edges may be slightly curved, which would appear as stair steps in the grid. @@@
!   if (ibcell_c(i,j,2)==4) then
!      if(i_mb_c > i) i_mb_c = i !least x index of melt interface
!      if(i_me_c < i) i_me_c = i !greatest x index of melt interface
!      if(j_mb_c > j) j_mb_c = j !least y index of melt interface
!      if(j_me_c < j) j_me_c = j !greatest y index of melt interface
!   endif
!enddo;enddo
!x_surfb_c=x_c(i_mb_c-1)
!x_surfe_c=x_c(i_me_c+1)
!y_surfb_c=y_c(j_mb_c-1)
!y_surfe_c=y_c(j_me_c+1)

!x_cent_c=(x_surfb_c+x_surfe_c)/2
!y_cent_c=(y_surfb_c+y_surfe_c)/2
!...........................................................................................................


!Compute shifted x & y grid point vectors relative to melt interface center
do i=2,mp_c
   !xs_c(i)=x_c(i)-x_cent_c*surf_length_m/surf_length_c
   xs_c(i)=x_c(i)-x_cent_c
enddo

do j=2,np_c
   !ys_c(j)=y_c(j)-y_cent_c*surf_width_m/surf_width_c
   ys_c(j)=y_c(j)-y_cent_c
enddo


!Scale combustion space surface grid so that the edges of the main melt tank, excluding dog houses, etc. match
!those of the main melt tank of the melt grid.

do i=2,mp_c
   xs_c(i) = xs_c(i)*surf_length_m/surf_length_c
enddo

do j=2,np_c
   ys_c(j) = ys_c(j)*surf_width_m/surf_width_c
enddo

interior_surf_wall=0
do i=i_mb_c,i_me_c,2
do j=j_mb_c,j_me_c,2
   if(ibcell_c(i,j,2)==1) interior_surf_wall=1
enddo;enddo
return
end

!======================================================================
!======================================================================
!======================================================================
subroutine grid_c_r
use gbl_var
implicit double precision (a-h,o-z)
integer, allocatable :: ibc_c(:)

1     format (A32)
2     format (I5,2G15.5)
3     format (I4,2X,<np_c_r>i1)

idebug_c_r=0
!----------------------------------------------------------------------
!     Read in second combustion space grid for a regenerative furnace
!     r0_c_r: reference length (m)
!     
!     mp_c_r/2 = # of X grid points including boundary points
!     np_c_r/2 = # of Y or R grid points including boundary points
!     lp_c_r/2 = # of Z grid points including boundary points
!----------------------------------------------------------------------
filename=casedir//'\gd'//gdnum_r//'c.dat'
inquire(file=filename,exist=exst)
if (.not.exst) then
   !this is an error if we are doing a regenerative furnace run
   if (itn>0) then
      write(6,*)'Combustion grid file,', filename, 'not found in melt directory.'
      write(6,*)'This file is needed for boundary condition interpolation'
      write(6,*)'at the interface between the melt and combustion space.'
      write(6,*)'The grid for the alternate burner configuration for.'
      write(6,*)'a regenerative furnace case is missing.'
      call stop_run("Combustion grid 2 file gd....c.dat not found in melt directory.")
   endif
   return
endif
open (nu_gc_r,file=filename)
read (nu_gc_r,1) title
read (nu_gc_r,1) title
read (nu_gc_r,*) r0_c_r,mp_c_r,np_c_r,lp_c_r,surf_length_c_r,surf_width_c_r

allocate (x_c_r(mp_c_r),dx_c_r(mp_c_r))
allocate (y_c_r(np_c_r),dy_c_r(np_c_r))
allocate (z_c_r(lp_c_r),dz_c_r(lp_c_r))
allocate (xs_c_r(mp_c_r),ys_c_r(np_c_r))
x_c_r=0
xs_c_r=0
dx_c_r=0
y_c_r=0
ys_c_r=0
dy_c_r=0
z_c_r=0
dz_c_r=0
allocate (ibcell_c_r(mp_c_r,np_c_r,lp_c_r))
ibcell_c_r=0

!--- Combustion space x-grid alternater burner configuration ------------
!        x_c_r(i):  x-coordinate of the i-th node
!        dx_c_r(i): i-th cell x-width
read (nu_gc_r,1) title
read (nu_gc_r,*) i1,xx
do i=1,i1
   x_c_r(i)=xx
enddo

do !read in x-coordinates
   read (nu_gc_r,*) i2,xx
   if (i2.LE.i1) cycle
   del=(xx-x_c_r(i1))/(i2-i1)
   if (i2 > mp_c_r.or.del.LE.zero) then
      write(*,*) 'Either a x_c_r component is greater than max x_c_r'
      write(*,*) 'or the x_c_r components are not in numerical order.'
      write(*,*) 'check around x_c_r coordinate:', j2, 'in the grid file.'
      call stop_run("Error in Grid_c_r read (with i).")
   endif
   do i=i1+1,i2
      x_c_r(i)=x_c_r(i-1)+del
   enddo
   i1=i2
   if (i2 > mp_c_r-2) exit
enddo

!Normalize distances
do i=1,mp_c_r-1
   x_c_r(i)=x_c_r(i)/r0_c_r
enddo
x_c_r(mp_c_r)=x_c_r(mp_c_r-1)

!Calculate cell widths
dx_c_r(1)=zero
do i=2,mp_c_r-1
   dx_c_r(i)=x_c_r(i+1)-x_c_r(i-1)
enddo  
dx_c_r(mp_c_r)=zero

if (idebug_c_r==2) write (20,2) (i,x_c_r(i),dx_c_r(i),i=1,mp_c_r)

!--- Combustion Space y-grid ------------------------------------------
!    y_c_r(j): y-coordinate of the j-th node
!    dy_c_r(j): j-th cell y-width
read (nu_gc_r,1) title
if (idebug_c==2) write(20,1) title
read (nu_gc_r,*) j1,yy
do j=1,j1
   y_c_r(j)=yy
enddo

do !read in y-coordinates
   read (nu_gc_r,*) j2,yy
   
   if (j2.LE.j1) cycle
   del=(yy-y_c_r(j1))/(j2-j1)
   if (j2.GT.np_c_r.OR.del.LE.zero) then
      write(*,*) 'Either a y_c_r component is greater than max y'
      write(*,*) 'or the y_c_r components are not in numerical order.'
      write(*,*) 'check around y_c_r coordinate:', j2
      call stop_run("Error in Grid_c_r read (with j).")
   endif
   
   do j=j1+1,j2
      y_c_r(j)=y_c_r(j-1)+del
   enddo
   j1=j2
   if (j2 > np_c_r-2) exit
enddo

!Normalize distances
do j=1,np_c_r-1
   y_c_r(j)=y_c_r(j)/r0_c_r
enddo
y_c_r(np_c_r)=y_c_r(np_c_r-1)

!Calculate cell widths
dy_c_r(1)=zero
do j=2,np_c_r-1
   dy_c_r(j)=y_c_r(j+1)-y_c_r(j-1)
enddo
dy_c_r(np_c_r)=zero

if (idebug_c_r==2) write (20,2) (j,y_c_r(j),dy_c_r(j),j=1,np_c_r)

!---Combustion Space z-grid ----------------------------------------------------
!     z_c_r(k):  z-coordinate of the kth node
!     dz_c_r(k): k-th cell z-height
read (nu_gc_r,1) title
if (idebug_c_r==2) write(20,1) title
read (nu_gc_r,*) k1,zz
do k=1,k1
   z_c_r(k)=zz
enddo

do !read in z-coordinates
   read (nu_gc_r,*) k2,zz
   if (k2.LE.k1) cycle   
   del=(zz-z_c_r(k1))/(k2-k1)
   if (k2 > lp_c_r .OR. del.LE.zero) then
      write(*,*) 'Either a z_c_r component is greater than max z_c_r'
      write(*,*) 'or the z_c_r components are not in numerical order.'
      write(*,*) 'check around z_c_r coordinate:', k2, 'in the grid file.'
      call stop_run("Error in Grid_c_r read (with k).")
   endif
   
   do k=k1+1,k2
      z_c_r(k)=z_c_r(k-1)+del
   enddo
   k1=k2
   if (k2 > lp_c_r-2) exit
enddo
!Normalize distances
do k=1,lp_c_r-1
   z_c_r(k)=z_c_r(k)/r0_c_r
enddo
z_c_r(lp_c_r)=z_c_r(lp_c_r-1)

!Calculate cell widths
dz_c_r(1)=zero

do k=2,lp_c_r-1
   dz_c_r(k)=z_c_r(k+1)-z_c_r(k-1)
enddo
dz_c_r(lp_c_r)=zero

if (idebug_c_r==2) write (20,2) (k,z_c_r(k),dz_c_r(k),k=1,lp_c_r)

!---  Cell Map ----------------------------------------------
!     ibcell_c_r: cell type indicator,
!     0-regular, (-1,9)-half blocked
!     1-blocked,  2-inlet,   3-exit, 4-free surface interface

ibcell_c_r=0
read (nu_gc_r,1) title
if (idebug_c_r==2) write(20,1) title
allocate (ibc_c(np_c_r))
k1=0

do !read in cell center values of cell map
   read (nu_gc_r,*) k2
   i1=0
   
   do
      read (nu_gc_r,3) i2,(ibc_c(j),j=2,np_c_r,2)
      do k=k1+2,k2,2
      do i=i1+2,i2,2
      do j=2,np_c_r,2
         if (ibc_c(j) > 5) ibc_c(j)=ibc_c(j)-10
         ibcell_c_r(i,j,k)=ibc_c(j)
      enddo;enddo;enddo
      
      i1=i2
      if (i2 > mp_c_r-1) exit
   enddo
   k1=k2   
   if (k2 > lp_c_r-1) exit
enddo
close(nu_gc_r)

!     Determine number of computational cells in grid (ncells_c_r)
!     Set grid cell type values on faces and edges
ncells_c_r=0
do i=2,mp_c_r,2
do j=2,np_c_r,2
do k=2,lp_c_r,2
   ibc1=ibcell_c_r(i,j,k)
   if (ibc1 < 1) then
      ncells_c_r=ncells_c_r+1
      cycle
   endif
   do i1=i-1,i+1
   do j1=j-1,j+1
   do k1=k-1,k+1
      if (i1 > mp_c_r .or. j1 > np_c_r .or. k1 > lp_c_r) cycle
      if (ibcell_c_r(i1,j1,k1).ne.1) ibcell_c_r(i1,j1,k1)=ibc1
   enddo;enddo;enddo
enddo;enddo;enddo

if (idebug_c_r==2) then
   do k=2,lp_c_r
      write(20,3) k
      do i=2,mp_c_r
         write(20,3) i,(ibcell_c_r(i,j,k),j=2,np_c_r)
      enddo
   enddo
endif

deallocate (ibc_c)
call interface_center_c_r

return
end

!======================================================================
!======================================================================
!======================================================================
!Subroutine interface_center_c_r
!
! Sets up variables for interpolation between combustion and melt
! grid surface interface for the alternate regenerative burner configuration.
!
! Finds center coordinates of melt interface area in combustion grid
! Finds least and greatest index of melt surface interface
! in x and y directions
!
! Checks for interior walls
!
! Computes shifted x & y grid vectors relative to melt interface center
!======================================================================
subroutine interface_center_c_r
use gbl_var
implicit double precision (a-h,o-z)

!find first x-row with interface cells

i_mb_c_r=mp_c_r
i_me_c_r=2
j_mb_c_r=np_c_r
j_me_c_r=2
do i=2,mp_c_r,2
do j=2,np_c_r,2
   if (ibcell_c_r(i,j,2)==4) then
      if(i_mb_c_r > i) i_mb_c_r = i !least x index of melt interface
      if(i_me_c_r < i) i_me_c_r = i !greatest x index of melt interface
      if(j_mb_c_r > j) j_mb_c_r = j !least y index of melt interface
      if(j_me_c_r < j) j_me_c_r = j !greatest y index of melt interface
   endif
enddo;enddo
x_surfb_c=x_c_r(i_mb_c_r-1)
x_surfe_c=x_c_r(i_me_c_r+1)
y_surfb_c=y_c_r(j_mb_c_r-1)
y_surfe_c=y_c_r(j_me_c_r+1)

x_cent_c_r=(x_surfb_c+x_surfe_c)/2
y_cent_c_r=(y_surfb_c+y_surfe_c)/2

!Compute shifted x & y grid point vectors relative to melt interface center
do i=2,mp_c_r
do j=2,np_c_r
   xs_c_r(i)=x_c_r(i)-x_cent_c_r
   ys_c_r(j)=y_c_r(j)-y_cent_c_r
enddo;enddo
interior_surf_wall_r=0
do i=i_mb_c_r,i_me_c_r,2
do j=j_mb_c_r,j_me_c_r,2
   if(ibcell_c_r(i,j,2)==1) interior_surf_wall=1
enddo;enddo
return
end
