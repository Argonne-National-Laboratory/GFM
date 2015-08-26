!==============================================================================
!==============================================================================
!==============================================================================
!
! Qwall.f90
!
! Qwall calculates the radiation from each boundary patch to all
! other boundary patches
!
! This module contains the following routines:
!     prepare_for_wall_radiation_calculation
!     allocate_boundary_arrays
!     wallq
!     calc_q_conv_wall
!     get_radiation_from_boundaries(i)
!     wall_q_amb
!     wall_display(array_indicator)
!
!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  prepare_for_wall_radiation_calculation
!
! Purpose:  Locate and make boundary arrays for inlets, exits, melt surfaces, 
!           and walls which have an open neighbor.
!
!           Calculate the view factors between the boundary patches
!
! Called from initsv on new start.
!
! Uses file unit 80 (nu_walbug) for debugging prints.
!
! Algorithm: 
!     Step 1:  Set mapb = copy of grid cell type array
!     Step 2:  Find exits and modify mapb to delete exit tunnels
!     Step 3:  Count the number of boundary faces (patches).
!     Step 4:  Allocate memory for the boundary arrays.
!     Step 5:  Find the faces again and add their pertinent information to the boundary arrays.
!	  Step 6:  Associate exhaust numbers with boundary patches
!     Step 7:  Calculate view factors if indicated to do so
!     Step 8:  Save total face area for patches on same cells 
!     Step 9:  Create wall grid file for viewing wall values - optional
!
! Notes about exits:
!     Exits are always rectangles on the outer enclosing wall of the grid. The exit cell faces
!     normal to the exit flow are maked as exits, but the exit<->other_wall faces are marked as
!     walls. There is a tunnel from each exit through the real wall to the interior of the
!     furnace. The number of cells in a tunnel is determined by the positioning of the exit. 
!     For example, there will be more tunnel cells for an exit near the lower part 
!     of a crown than for an exit from a flat wall.  There are no exits on the bottom 
!     boundary (z=2) because that is the melt surface.
! 
!==============================================================================
subroutine prepare_for_wall_radiation_calculation
use gbl_var
implicit double precision (a-h,o-z)

integer n ! current boundary patch number (from)
integer m ! current boundary patch number (to)
integer istart,iend,inew !temporary
integer jstart,jend,jnew !temporary
integer kstart,kend,knew !temporary
integer grid_type !temporary
real*8  denominator ! temporary denominator in calculation
character*60 text_line

1     format (a60)
3     format (i4,2x,<np>i1)
avg_wall_T=1600.0d0 !initial guess

!-----------------------------------------
! Step 1:  Set mapb = copy of grid cell type array
!-----------------------------------------
allocate (mapb(mp,np,lp))
do i=1,mp
do j=1,np
do k=1,lp
   mapb(i,j,k)=ibcell(i,j,k)
enddo;enddo;enddo

!-----------------------------------------
! Step 2:  Find exits and modify mapb to delete exit tunnels
!-----------------------------------------

!---------------------------------
! Look for exits on the back wall
i=2  !using cell centers at this stage
do j=2,np,2
do k=2,lp,2
   if (mapb(i,j,k).ne.3) cycle
   !At one corner of an exit, find other corners
   kstart=k
   do kk=k+2,lp,2
      if (mapb(i,j,kk).ne.3) then
         kend=kk-2
         exit
      endif
   enddo  
   jstart=j
   do jj=j+2,np,2
      if (mapb(i,jj,k).ne.3) then
         jend=jj-2
         exit
      endif
   enddo
   !Determine interior end of exit tunnel (note this end may be slanted)
   do ii=4,mp,2
      if (mapb(ii,jstart-2,kstart-2)==0 .or. mapb(ii,jend+2,kend+2)==0 .or. &
          mapb(ii,jstart-2,kend+2)==0   .or. mapb(ii,jend+2,kstart-2)==0) then 
         !found beginning of interior
         inew=ii-2
         exit   
      endif
   enddo
   !Put exit at interior end of tunnel 
   do ii=inew,inew+1
   do jj=jstart,jend
   do kk=kstart,kend
      mapb(ii,jj,kk)=3
   enddo;enddo;enddo
   !Put walls in rest of exit tunnel
   do ii=2,inew-1
   do jj=jstart,jend
   do kk=kstart,kend
      mapb(ii,jj,kk)=1
   enddo;enddo;enddo
enddo;enddo

!---------------------------------
! Look for exits on the front wall
i=mp
do j=2,np,2
do k=2,lp,2
   if (mapb(i,j,k).ne.3) cycle
   !At one corner of an exit, find other corners
   kstart=k
   do kk=k+2,lp,2
      if (mapb(i,j,kk).ne.3) then
         kend=kk-2
         exit
      endif
   enddo  
   jstart=j
   do jj=j+2,np,2
      if (mapb(i,jj,k).ne.3) then
         jend=jj-2
         exit
      endif
   enddo
   !Determine interior end of exit tunnel (note this end may be slanted)
   do ii=mp-2,2,-2
      if (mapb(ii,jstart-2,kstart-2)==0 .or. mapb(ii,jend+2,kend+2)==0 .or. &
          mapb(ii,jstart-2,kend+2)==0   .or. mapb(ii,jend+2,kstart-2)==0) then 
         !found beginning of interior
         inew=ii+2
         exit   
      endif
   enddo
   !Put exit at interior end of tunnel (now include faces)
   do ii=inew-1,inew
   do jj=jstart,jend
   do kk=kstart,kend
      mapb(ii,jj,kk)=3
   enddo;enddo;enddo
   !Put walls in rest of exit tunnel
   do ii=inew+1,mp
   do jj=jstart,jend
   do kk=kstart,kend
      mapb(ii,jj,kk)=1
   enddo;enddo;enddo
enddo;enddo

!---------------------------------
! Look for exits on the left wall
j=2  !using cell centers at this stage
do i=2,mp,2
do k=2,lp,2
   if (mapb(i,j,k).ne.3) cycle
   !At one corner of an exit, find other corners
   kstart=k
   do kk=k+2,lp,2
      if (mapb(i,j,kk).ne.3) then
         kend=kk-2
         exit
      endif
   enddo  
   istart=i
   do ii=i+2,mp,2
      if (mapb(ii,j,k).ne.3) then
         iend=ii-2
         exit
      endif
   enddo
   !Determine interior end of exit tunnel (note this end may be slanted)
   do jj=4,np,2
      if (mapb(istart-2,jj,kstart-2)==0 .or. mapb(iend+2,jj,kend+2)==0 .or. &
          mapb(istart-2,jj,kend+2)==0   .or. mapb(iend+2,jj,kstart-2)==0) then 
         !found beginning of interior
         jnew=jj-2
         exit   
      endif
   enddo
   !Put exit at interior end of tunnel (now include faces)
   do ii=istart,iend
   do jj=jnew,jnew+1
   do kk=kstart,kend
      mapb(ii,jj,kk)=3
   enddo;enddo;enddo
   !Put walls in rest of exit tunnel
   do ii=istart,iend
   do jj=2,jnew-1
   do kk=kstart,kend
      mapb(ii,jj,kk)=1
   enddo;enddo;enddo
enddo;enddo

!---------------------------------
! Look for exits on the right wall
j=np
do i=2,mp,2
do k=2,lp,2
   if (mapb(i,j,k).ne.3) cycle
   !At one corner of an exit, find other corners
   kstart=k
   do kk=k+2,lp,2
      if (mapb(i,j,kk).ne.3) then
         kend=kk-2
         exit
      endif
   enddo  
   istart=i
   do ii=i+2,mp,2
      if (mapb(ii,j,k).ne.3) then
         iend=ii-2
         exit
      endif
   enddo
   !Determine interior end of exit tunnel (note this end may be slanted)
   do jj=np-2,2,-2
      if (mapb(istart-2,jj,kstart-2)==0 .or. mapb(iend+2,jj,kend+2)==0 .or. &
          mapb(istart-2,jj,kend+2)==0   .or. mapb(iend+2,jj,kstart-2)==0) then 
         !found beginning of interior
         jnew=jj+2
         exit   
      endif
   enddo
   !Put exit at interior end of tunnel (now include faces)
   do ii=istart,iend
   do jj=jnew-1,jnew
   do kk=kstart,kend
      mapb(ii,jj,kk)=3
   enddo;enddo;enddo
   !Put walls in rest of exit tunnel
   do ii=istart,iend
   do jj=jnew+1,np
   do kk=kstart,kend
      mapb(ii,jj,kk)=1
   enddo;enddo;enddo
enddo;enddo

!---------------------------------
! Look for exits on the top wall
k=lp
do i=2,mp,2
do j=2,np,2
   if (mapb(i,j,k).ne.3) cycle
   !At one corner of an exit, find other corners
   istart=i
   do ii=i+2,mp,2
      if (mapb(ii,j,k).ne.3) then
         iend=ii-2
         exit
      endif
   enddo
   jstart=j
   do jj=j+2,np,2
      if (mapb(i,jj,k).ne.3) then
         jend=jj-2
         exit
      endif
   enddo
   !Determine interior end of exit tunnel (note this end may be slanted)
   do kk=lp-2,2,-2
      if (mapb(istart-2,jstart-2,kk)==0 .or. mapb(iend+2,jend+2,kk)==0 .or. &
          mapb(istart-2,jend+2,kk)==0   .or. mapb(iend+2,jstart-2,kk)==0) then 
         !found beginning of interior
         knew=kk+2
         exit   
      endif
   enddo
   !Put exit at interior end of tunnel (now include faces)
   do ii=istart,iend
   do jj=jstart,jend
   do kk=knew-1,knew
      mapb(ii,jj,kk)=3
   enddo;enddo;enddo
   !Put walls in rest of exit tunnel
   do ii=istart,iend
   do jj=jstart,jend
   do kk=knew+1,lp
      mapb(ii,jj,kk)=1
   enddo;enddo;enddo
enddo;enddo


if (iwalbug1==1) then
   !debug check new exits
   filename=casedir//'\mapb'//runum//'c.txt'
   open(nu_walbug,file=filename)
   write(nu_walbug,*) 'Cells Changed for Exits for View Factors'
   write(nu_walbug,*) '   i    j    k ibcell mapb'
   !write(nu_walbug,*) '   i    j    k ibcell mapb mbi+ mbj+ mbk+ mbi- mbj- mbk-'
   do i=2,mp,2
   do j=2,np,2
   do k=2,lp,2
      if (ibcell(i,j,k) .ne. mapb(i,j,k)) write(nu_walbug,'(5i5)') i,j,k,ibcell(i,j,k),mapb(i,j,k)
      !if (ibcell(i,j,k) .ne. mapb(i,j,k)) write(nu_walbug,'(11i5)') i,j,k,ibcell(i,j,k),mapb(i,j,k), &
      !   mapb(min(mp,i+1),j,k),mapb(i,min(np,j+1),k),mapb(i,j,min(lp,k+1)), &
      !   mapb(max(2,i-1),j,k),mapb(i,max(2,j-1),k),mapb(i,j,max(2,k-1))
   enddo;enddo;enddo
   close(nu_walbug)
 endif !end debug


!-----------------------------------------
! Step 3: Count number of boundary patches
!-----------------------------------------

b_cnt=0   ! total number of boundary patches (exit patches are included in the boundary list)
!  look at x-normal faces to check for boundary cells.
do k=2,lp,2
do j=2,np,2
do i=3,mp-1,2
   if (mapb(i,j,k)<1) cycle
   !at an inlet, exit, or blocked (wall) cell
   !look back and forward into cells to see if at boundary
   if (mapb(i-1,j,k)<1 .or. mapb(i+1,j,k)<1) then
      !have found boundary cell
      b_cnt=b_cnt+1
   endif
enddo;enddo;enddo

!  look at y-normal faces to check for boundary patches.
do k=2,lp,2
do j=3,np-1,2
do i=2,mp,2
   if (mapb(i,j,k)<1) cycle
   !at an inlet, exit, melt surface, or blocked (wall) face
   !look back and forward into cells to see if at boundary
   if (mapb(i,j-1,k)<1 .or. mapb(i,j+1,k)<1) then
      !have found boundary patch
      b_cnt=b_cnt+1
   endif
enddo;enddo;enddo

!  look at z-normal faces to check for boundary patches.
do k=3,lp-1,2
do j=2,np,2
do i=2,mp,2
   if (mapb(i,j,k)<1) cycle
   !at an inlet, exit, melt surface, or blocked (wall) face
   !look back and forward into cells to see if at boundary
   if (mapb(i,j,k-1)<1 .or. mapb(i,j,k+1)<1) then
      !have found boundary patch
      b_cnt=b_cnt+1
   endif
enddo;enddo;enddo


!-----------------------------------------
! Step 4: Create boundary patch (face) arrays
!-----------------------------------------

call allocate_boundary_arrays

! Also need temporary arrays on new start
allocate (sameCell1(b_cnt))   ! patch index 1 for same cell !these are temporary   @@@   ???
allocate (sameCell2(b_cnt))   ! patch index 2 for same cell
allocate (sameCell3(b_cnt))   ! patch index 3 for same cell
allocate (sameCell4(b_cnt))   ! patch index 4 for same cell


!-----------------------------------------
! Step 5:  Add pertinent information to the boundary arrays 
!-----------------------------------------
! Find boundary faces again
! Determine and save boundary face indexes
!                             map: wall=1, inlet=2, exit=3, melt surface=4
!                             orientaion: x-normal=1, y-normal=2, z_normal=3
!                             direction : positive=0, negative=1
!                             patch area
!                             emissivity
!                             radiosity (initial guess)
!                             face_area: total patch area for same boundary cell 
n=0
!  look at x-normal faces to check for boundary cells.
do k=2,lp,2
do j=2,np,2
do i=3,mp-1,2
   if (mapb(i,j,k)<1) cycle
   !at an inlet, exit, or blocked (wall) cell
   !look back and forward into cells to see if at boundary
   if (mapb(i-1,j,k)>0 .and. mapb(i+1,j,k)>0) cycle
   !have found boundary face
   n=n+1
   b_typ(n)=mapb(i,j,k)
   if (mapb(i,j,k)==1) then
      if (z(k)>=height_to_ceiling .or. (k==lp .and. have_crown==0)) then
         emis(n)=eps_c !ceiling or crown emissivity
      else
         emis(n)=eps_w !wall emissivity
      endif
   else
      emis(n)=one !inlets and exits treated as black body (melt surface is z normal only)
   endif
   b_orient(n)=1 !x-normal
   if (mapb(i+1,j,k)==0) then
      b_direct(n)=0 !boundary face area vector points in + direction
      bc_i(n)=i-1 !save boundary cell center indexes
      bc_j(n)=j
      bc_k(n)=k
   else !mapb(i-1,j,k)==0
      b_direct(n)=1 !boundary face area vector points in - direction
      bc_i(n)=i+1 !save boundary cell center indexes
      bc_j(n)=j
      bc_k(n)=k
   endif
   !save patch position
   b_i(n)=i
   b_j(n)=j
   b_k(n)=k  
   patch_area(n)=dr(j)*dz(k)
enddo;enddo;enddo

!  look at y-normal faces to check for boundary patches.
do k=2,lp,2
do j=3,np-1,2
do i=2,mp,2
   if (mapb(i,j,k)<1) cycle
   !at an inlet, exit, or blocked (wall) face
   !look back and forward into cells to see if at boundary
   if (mapb(i,j-1,k)>0 .and. mapb(i,j+1,k)>0) cycle
   !have found boundary face
   n=n+1
   b_typ(n)=mapb(i,j,k)
   if (mapb(i,j,k)==1) then
      if (z(k)>=height_to_ceiling .or. (k==lp .and. have_crown==0)) then
         emis(n)=eps_c !ceiling or crown emissivity
      else
         emis(n)=eps_w !wall emissivity
      endif
   else
      emis(n)=one !inlets and exits treated as black body
   endif
   b_orient(n)=2 !y-normal
   if (mapb(i,j+1,k)==0) then
      b_direct(n)=0 !boundary face area vector points in + direction     
      bc_i(n)=i   !save boundary cell center indexes
      bc_j(n)=j-1
      bc_k(n)=k
   else !mapb(i,j-1,k)==0
      b_direct(n)=1 !boundary face area vector points in - direction
      bc_i(n)=i   !save boundary cell center indexes
      bc_j(n)=j+1
      bc_k(n)=k
   endif
   !save patch position
   b_i(n)=i
   b_j(n)=j
   b_k(n)=k  
   patch_area(n)=dx(i)*dz(k)
enddo;enddo;enddo

!  look at z-normal faces to check for boundary patches.
do k=3,lp-1,2
do j=2,np,2
do i=2,mp,2
   if (mapb(i,j,k)<1) cycle
   !at an inlet, exit, melt surface, or blocked (wall) face
   !look back and forward into cells to see if at boundary
   if (mapb(i,j,k-1)>0 .and. mapb(i,j,k+1)>0) cycle
   !have found boundary face
   n=n+1
   b_typ(n)=mapb(i,j,k)
   if (mapb(i,j,k)==1) then
      if (z(k)>=height_to_ceiling .or. (k==lp-1 .and. have_crown==0)) then
         emis(n)=eps_c !ceiling or crown emissivity
      else
         emis(n)=eps_w !wall emissivity
      endif
   elseif (mapb(i,j,k)==4) then
      emis(n)=eps_m !melt surface emissivity
   else
      emis(n)=one !inlets and exits treated as black body
   endif
   b_orient(n)=3 !z-normal
   if (mapb(i,j,k+1)==0) then
      b_direct(n)=0 !boundary face area vector points in + direction       
      bc_i(n)=i   !save boundary cell center indexes
      bc_j(n)=j
      bc_k(n)=k-1
   else !mapb(i,j,k-1)==0
      b_direct(n)=1 !boundary face area vector points in - direction
      bc_i(n)=i   !save boundary cell center indexes
      bc_j(n)=j
      bc_k(n)=k+1
   endif
   !save patch position
   b_i(n)=i
   b_j(n)=j
   b_k(n)=k
   patch_area(n)=dx(i)*dr(j)
enddo;enddo;enddo

! following line for debugging only   
!if (n.ne.b_cnt) then
!   write (ncon,*) 'Mismatch in boundary counts. b_cnt= ',b_cnt,'  n= ',n 
!else 
!   write (ncon,*) 'Match in boundary counts. b_cnt= ',b_cnt,'  n= ',n 
!endif
! end debugging only  

!---------------------------------
! Set temperature and make initial guess for radiosity
!
! This part of the initialization has been moved to the start of the radiation
! routine so that the gas phase computations will have calculated temperatures.
!---------------------------------
!do n=1,b_cnt
!   b_T(n)=T(bc_i(n),bc_j(n),bc_k(n))*t0  !use t0 because not in radiation code yet
!   radiosity(n)=sig*b_T(n)**4 
!enddo


!-----------------------------------------
! Step 6:  Associate exhaust numbers with boundary patches
!-----------------------------------------
do n=1,b_cnt
	if (b_typ(n)/=3) cycle !only need to change exit patches
	do m=1,nex
		if (b_orient(n)/=exh_orient(m) .or. b_direct(n)/=exh_direct(m)) cycle
		select case (b_orient(n)) 
		case (1) !x-normal
			if (bc_j(n) >= exh_j1(m) .and. bc_j(n) <= exh_j2(m) .and. bc_k(n) >= exh_k1(m) .and. bc_k(n) <= exh_k2(m)) then
				b_exh(n)=m !current patch is in current exhaust 
				exit
			endif
		case (2) !y-normal
			if (bc_i(n) >= exh_i1(m) .and. bc_i(n) <= exh_i2(m) .and. bc_k(n) >= exh_k1(m) .and. bc_k(n) <= exh_k2(m)) then
				b_exh(n)=m !current patch is in current exhaust 
				exit
			endif
		case (3) !z-normal
			if (bc_i(n) >= exh_i1(m) .and. bc_i(n) <= exh_i2(m) .and. bc_j(n) >= exh_j1(m) .and. bc_j(n) <= exh_j2(m)) then
				b_exh(n)=m !current patch is in current exhaust 
				exit
			endif
		end select
	enddo
enddo

!-----------------------------------------
! Step 7:  Calculate view factors if indicated to do so
!-----------------------------------------
if (preset_vf==1) then
   vf=zero
   vf_sum=zero

   if (iwalbug2==1) then   !debug
      filename=casedir//'\vf_sum'//runum//'c.txt'
      open(nu_walbug,file=filename)
      write(nu_walbug,*) 'Sum of View Factors for Boundary Patches'
      write(nu_walbug,*) '================================='
      write(nu_walbug,*) '  Boundary: n SumViewFactor i1 j1 k1'
      write(nu_walbug,*) '================================='
   endif !end debug

   do n=1,b_cnt !n is the 'near or from' patch
      do m=1,b_cnt !m is the 'far or to' patch
         if (n==m) cycle !at same patch (they can't see themselves)
         denominator = pi * ((x(b_i(n))-x(b_i(m)))**2 &
                                   + (r(b_j(n))-r(b_j(m)))**2 &
                                   + (z(b_k(n))-z(b_k(m)))**2)**2
         select case (b_orient(n))
         case (1) !x-normal patch
            !View factor is 0 if far patch is on same plane or out of view
            if (b_direct(n)==0 .and. b_i(m)<=b_i(n)) cycle 
            if (b_direct(n)==1 .and. b_i(m)>=b_i(n)) cycle 
            select case (b_orient(m))
            case (1) !x-normal far patch
               vf(n,m) = (x(b_i(m))-x(b_i(n)))**2 * patch_area(m) / denominator
            case (2) !y-normal far patch
               vf(n,m) = abs((x(b_i(m))-x(b_i(n))) * (r(b_j(m))-r(b_j(n)))) &
                  * patch_area(m) / denominator
            case (3) !z-normal far patch
               vf(n,m) = abs((x(b_i(m))-x(b_i(n))) * (z(b_k(m))-z(b_k(n)))) &
                  * patch_area(m) / denominator
            end select

         case (2) !y-normal patch
            !View factor is 0 if far patch is on same plane or out of view
            if (b_direct(n)==0 .and. b_j(m)<=b_j(n)) cycle 
            if (b_direct(n)==1 .and. b_j(m)>=b_j(n)) cycle 
            select case (b_orient(m))
            case (1) !x-normal far patch
               vf(n,m) = abs((r(b_j(m))-r(b_j(n))) * (x(b_i(m))-x(b_i(n)))) &
                  * patch_area(m) / denominator
            case (2) !y-normal far patch
               vf(n,m) = (r(b_j(m))-r(b_j(n)))**2 * patch_area(m) / denominator
            case (3) !z-normal far patch
               vf(n,m) = abs((r(b_j(m))-r(b_j(n))) * (z(b_k(m))-z(b_k(n)))) &
                  * patch_area(m) / denominator
            end select

         case (3) !z-normal patch
            !View factor is 0 if far patch is on same plane or out of view
            if (b_direct(n)==0 .and. b_k(m)<=b_k(n)) cycle 
            if (b_direct(n)==1 .and. b_k(m)>=b_k(n)) cycle 
            select case (b_orient(m))
            case (1) !x-normal far patch
               vf(n,m) = abs((z(b_k(m))-z(b_k(n))) * (x(b_i(m))-x(b_i(n)))) &
                  * patch_area(m) / denominator
            case (2) !y-normal far patch
               vf(n,m) = abs((z(b_k(m))-z(b_k(n))) * (r(b_j(m))-r(b_j(n)))) &
                  * patch_area(m) / denominator
            case (3) !z-normal far patch
               vf(n,m) = (z(b_k(m))-z(b_k(n)))**2 * patch_area(m) / denominator
            end select
         end select
         vf_sum(n)=vf_sum(n) + vf(n,m)
      enddo !m=

      !debug
      if (iwalbug2==1) write(nu_walbug,'(i5,f22.18,3i5)') n,vf_sum(n),b_i(n),b_j(n),b_k(n)

      !if (vf_sum(n) < 0.8d+0 .or. vf_sum(n) > 1.2d+0) then
      !   write (ncon,*) 'sum vf from patch ', n, ' = ', vf_sum(n)  !debug
      !endif
      !end debug
   enddo !n=
   if (iwalbug2==1) close(nu_walbug) !debug

   !---------------------------------
   ! Normalize view factors 
   !---------------------------------

   if (iwalbug3==1) then !debug print out view factors - very very large file
      filename=casedir//'\vf'//runum//'c.txt'
      open(nu_walbug,file=filename)
      write(nu_walbug,*) 'View Factors for Boundary Patches'
   endif !debug
   do n=1,b_cnt !n is the 'near or from' patch
      if (iwalbug3==1) then 
         write(nu_walbug,*) '================================='
         write(nu_walbug,*) '  Boundary (normalized): n m ViewFactor  i1 j1 k1 i2 j2 k2'
         write(nu_walbug,*) '================================='
      endif !debug
      do m=1,b_cnt !m is the 'far or to' patch
         if (vf_sum(n)==zero) cycle
         vf(n,m)=vf(n,m)/vf_sum(n)
         if (iwalbug3==1) then 
            write(nu_walbug,'(2i5,f22.18,6i5)') n,m,vf(n,m), &
               b_i(n),b_j(n),b_k(n),b_i(m),b_j(m),b_k(m)
         endif !debug
   enddo;enddo
   if (iwalbug3==1) close(nu_walbug)

endif

!---------------------------------
!     Step 8:  Save total face area for patches on same cells 
!---------------------------------

sameCell1=0
sameCell2=0
sameCell3=0
sameCell4=0
iwall_count=0 !number of interior wall patches
imelt_count=0 !number of melt surface patches
inlet_count=0 !number of inlet patches
iexit_count=0 !number of exit patches
wall_area_total=zero !total area of wall  patches
melt_area_total=zero !total area of melt patches
inlet_area_total=zero !total area of inlet patches
exit_area_total=zero !total area of exit patches

! Mark patches with the same cell centers
do n=1,b_cnt
   do m=n+1,b_cnt
      if (bc_i(n)==bc_i(m).and.bc_j(n)==bc_j(m).and.bc_k(n)==bc_k(m)) then
         !patches have same cell center
         if (sameCell1(n)==0) then
            sameCell1(n)=m;  sameCell1(m)=n
         elseif (sameCell2(n)==0) then
            if (sameCell1(n)==m) cycle
            sameCell2(n)=m;  sameCell1(m)=n
            sameCell2(m)=sameCell1(n); sameCell2(sameCell1(n))=m
         elseif (sameCell3(n)==0) then
            if (sameCell1(n)==m .or. sameCell2(n)==m) cycle
            sameCell3(n)=m;  sameCell1(m)=n
            sameCell2(m)=sameCell1(n); sameCell3(sameCell2(n))=m
            sameCell3(m)=sameCell2(n); 
         elseif (sameCell4(n)==0) then
            if (sameCell1(n)==m .or. sameCell2(n)==m .or. sameCell3(n)==m) cycle
            sameCell4(n)=m;  sameCell1(m)=n
            sameCell2(m)=sameCell1(n); sameCell4(sameCell2(n))=m
            sameCell3(m)=sameCell2(n); sameCell4(sameCell3(n))=m
            sameCell4(m)=sameCell3(n); 
         endif
      endif
   enddo   
enddo

! Set total patch face area for patch center cell
do n=1,b_cnt
   if (sameCell1(n)==0) then
      b_face_area(n)=patch_area(n)

   elseif (sameCell2(n)==0) then
      b_face_area(n)=patch_area(n)+patch_area(sameCell1(n))

   elseif (sameCell3(n)==0) then
      b_face_area(n)=patch_area(n)+patch_area(sameCell1(n))+patch_area(sameCell2(n))
   
   elseif (sameCell4(n)==0) then
      b_face_area(n)=patch_area(n)+patch_area(sameCell1(n)) &
                        +patch_area(sameCell2(n))+patch_area(sameCell3(n))
   else
      b_face_area(n)=patch_area(n)+patch_area(sameCell1(n))+patch_area(sameCell2(n)) &
                        +patch_area(sameCell3(n))+patch_area(sameCell4(n))
   endif

   !keep track of total patch area
   if (b_typ(n)==1) then !wall
      iwall_count=iwall_count+1
      wall_area_total=wall_area_total+patch_area(n)
   elseif (b_typ(n)==4) then !melt surface
      imelt_count=imelt_count+1
      melt_area_total=melt_area_total+patch_area(n)
   elseif (b_typ(n)==2) then !inlet
      inlet_count=inlet_count+1
      inlet_area_total=inlet_area_total+patch_area(n)
   else !exit
      iexit_count=iexit_count+1
      exit_area_total=exit_area_total+patch_area(n)
   endif      
enddo
boundary_area_total=wall_area_total+melt_area_total+inlet_area_total+exit_area_total

if (iwalbug4==1) then
   ! debug print out boundary face areas
   filename=casedir//'\bface'//runum//'c.txt'
   open(nu_walbug,file=filename)
   write(nu_walbug,*) 'Total Patch Face Area for Boundary Cells'
   write(nu_walbug,*) '================================='
   write(nu_walbug,*) '  Boundary: n i1 j1 k1 patch faces same1 same2 same3 same4'
   write(nu_walbug,*) '================================='
   do n=1,b_cnt !n is the 'near or from' patch
      write(nu_walbug,'(4i5,2f22.18,4i5)') n,b_i(n),b_j(n),b_k(n),patch_area(n), &
         b_face_area(n),sameCell1(n),sameCell2(n),sameCell3(n),sameCell4(n)
   enddo
   close(nu_walbug)
endif !debug

deallocate (sameCell1,sameCell2,sameCell3,sameCell4)


!---------------------------------
!     Step 9:  Create wall grid file for viewing wall values - optional
!---------------------------------

if (iwalvis==1) then
   ! Change mapb so that boundary cells become open, but keep melt cells as is for now
   do n=1,b_cnt
      select case (b_orient(n))
      case(1) ! x-normal patch
         if(b_direct(n)==0) then
            mapb(b_i(n)-2,b_j(n),b_k(n))=mapb(b_i(n),b_j(n),b_k(n)) ! move patch face
            mapb(b_i(n)-1,b_j(n),b_k(n))=0 !open cell 
            if (b_typ(n)==2) mapb(b_i(n)-3,b_j(n),b_k(n))=2 !move inlet center also 
            if (b_typ(n)==3) then !restore exit position
               mapb(2,b_j(n),b_k(n))=3 
               mapb(3,b_j(n),b_k(n))=3 
               do nn=4,b_i(n)-2;  mapb(nn,b_j(n),b_k(n))=0;  enddo 
            endif
         else
            mapb(b_i(n)+2,b_j(n),b_k(n))=mapb(b_i(n),b_j(n),b_k(n)) ! move patch face
            mapb(b_i(n)+1,b_j(n),b_k(n))=0 !open cell 
            if (b_typ(n)==2) mapb(b_i(n)+3,b_j(n),b_k(n))=2 !move inlet center also 
            if (b_typ(n)==3) then !restore exit position
               mapb(mp,b_j(n),b_k(n))=3 
               mapb(mp-1,b_j(n),b_k(n))=3 
               do nn=mp-2,b_i(n)-2,-1;  mapb(nn,b_j(n),b_k(n))=0;  enddo 
            endif
         endif
      case(2) ! y-normal patch
         if(b_direct(n)==0) then
            mapb(b_i(n),b_j(n)-2,b_k(n))=mapb(b_i(n),b_j(n),b_k(n)) ! move patch face
            mapb(b_i(n),b_j(n)-1,b_k(n))=0 !open cell 
            if (b_typ(n)==2) mapb(b_i(n),b_j(n)-3,b_k(n))=2 !move inlet center also 
            if (b_typ(n)==3) then !restore exit position
               mapb(b_i(n),2,b_k(n))=3 
               mapb(b_i(n),3,b_k(n))=3 
               do nn=4,b_j(n)-2;  mapb(b_i(n),nn,b_k(n))=0;  enddo 
            endif
         else
            mapb(b_i(n),b_j(n)+2,b_k(n))=mapb(b_i(n),b_j(n),b_k(n)) ! move patch face
            mapb(b_i(n),b_j(n)+1,b_k(n))=0 !open cell 
            if (b_typ(n)==2) mapb(b_i(n),b_j(n)+3,b_k(n))=2 !move inlet center also 
            if (b_typ(n)==3) then !restore exit position
               mapb(b_i(n),np,b_k(n))=3 
               mapb(b_i(n),np-1,b_k(n))=3 
               do nn=np-2,b_j(n)-2,-1;  mapb(b_j(n),nn,b_k(n))=0;  enddo 
            endif
         endif
      case(3) ! z-normal patch
         if(b_direct(n)==0) then
            !must be melt surface, already at lowest index
            !do nothing at this time
         else
            mapb(b_i(n),b_j(n),b_k(n)+2)=mapb(b_i(n),b_j(n),b_k(n)) ! move patch face
            mapb(b_i(n),b_j(n),b_k(n)+1)=0 !open cell 
            if (b_typ(n)==2) mapb(b_i(n),b_j(n),b_k(n)+3)=2 !move inlet center also
            if (b_typ(n)==3) then !restore exit position
               mapb(b_i(n),b_j(n),lp)=3 
               mapb(b_i(n),b_j(n),lp-1)=3 
               do nn=lp-2,b_k(n)-2,-1;  mapb(b_i(n),b_j(n),nn)=0;  enddo 
            endif          
         endif
      end select 
   enddo   


   !Copy the real grid top portion to the new wall grid file, modifying the wall grid
   !file to have an extra duplicated melt surface slab

   filename=casedir//'\gd'//gdnum//'c.dat' !original grid file
   open (nu_grid,file=filename)

   filename2=casedir//'\gdw'//gdnum//'c.dat' !new wall grid file
   open (nu_gdw,file=filename2) !new wall grid file

   read (nu_grid,1) text_line
   write (nu_gdw,1) text_line
   read (nu_grid,1) text_line
   write (nu_gdw,1) text_line
   !read (nu_grid,*) r0,mp,np,lp,ndp0,npt0,nsp0,nr0,grid_type   !r0=1, nsp0=7 always
   read (nu_grid,*) dummy_r0,mp,np,lp,ndp0,npt0,idummy_nsp0,nr0,grid_type
   write (nu_gdw,'(4x,f3.1,8i5)') r0,mp,np,lp+2,ndp0,npt0,nsp0,nr0,grid_type !adding extra z-plane

   !---------------------  x-grid 
   read (nu_grid,1) text_line
   write (nu_gdw,1) text_line
   read (nu_grid,*) i1,xx
   write (nu_gdw,'(i5,f12.5)') i1,xx

   do
      read (nu_grid,*) i2,xx
      write (nu_gdw,'(i5,f12.5)') i2,xx
      if (i2.le.i1) cycle      
      i1=i2
      if (i2 > mp-2) exit
   enddo

   !---------------------  r-grid 
   read (nu_grid,1) text_line
   write (nu_gdw,1) text_line
   read (nu_grid,*) j1,rr
   write (nu_gdw,'(i5,f12.5)') j1,rr

   do
      read (nu_grid,*) j2,rr
      write (nu_gdw,'(i5,f12.5)') j2,rr
      if (j2.le.j1) cycle      
      j1=j2
      if (j2 > np-2) exit
   enddo

   !---------------------  z-grid 
   read (nu_grid,1) text_line
   write (nu_gdw,1) text_line
   read (nu_grid,*) k1,zz ! this will be '3  0.0)
   zzz=-0.31
   write (nu_gdw,'(i5,f12.5)') k1,zzz !adding wall slab
   write (nu_gdw,'(i5,f12.5)') k1+2,zz

   do
      read (nu_grid,*) k2,zz
      write (nu_gdw,'(i5,f12.5)') k2+2,zz
      if (k2.le.k1) cycle      
      k1=k2
      if (k2 > lp-2) exit
   enddo

   !---------------------  cell diagram, replace with entire mapb grid
   read (nu_grid,1) text_line
   write (nu_gdw,1) text_line
   close (nu_grid) !do not need rest of original grid

   !Write out first z slab
   k=2 
   write(nu_gdw,3) k
   do i=2,mp,2
      write(nu_gdw,3) i,(mapb(i,j,k),j=2,np,2)
   enddo

   !Write out a duplicate of the first slab and continue to write out all the 
   !following slabs
   do k=4,lp+2,2
      write(nu_gdw,3) k
      do i=2,mp,2
         write(nu_gdw,3) i,(mapb(i,j,k-2),j=2,np,2)
      enddo
   enddo

   !Finish up writing the file
   write (nu_gdw,*) '  ' 
   write (nu_gdw,*) '&GRD' 
   write (nu_gdw,*) '/' 
   close (nu_gdw) 

   deallocate (mapb)
endif !iwalvis=1

return
end


!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  allocate_boundary_arrays
!
! Purpose:  Allocate boundary patch arrays and associated radiation arrays
!
! Called from:  prepare_for_wall_radiation_calculation on new start
!               rad_emis_rr on restart
!
! Note:  b_cnt (number of boundary patches) must be set before this routine is called.
!
!==============================================================================
subroutine allocate_boundary_arrays
use gbl_var
implicit double precision (a-h,o-z)

allocate (b_typ(b_cnt))    ! boundary type = ibcell value of patches
allocate (b_i(b_cnt))  ! i index of boundary patches
allocate (b_j(b_cnt))  ! j index of boundary patches
allocate (b_k(b_cnt))  ! k index of boundary patches
allocate (b_orient(b_cnt)) ! boundary orientation (x,y,z)
allocate (b_direct(b_cnt)) ! boundary direction (+,-)
allocate (patch_area(b_cnt)) ! area of patches
allocate (b_face_area(b_cnt)) ! area of all patches on same cell
if (preset_vf==1) then
   allocate (vf(b_cnt,b_cnt))   ! view factors
   allocate (vf_sum(b_cnt))     ! for patch i, sum over j of vf(i,j)
endif
allocate (radiosity(b_cnt))  ! radiosity of boundary patch 
allocate (emis(b_cnt))       ! emissivity of boundary patch 
allocate (q_amb(b_cnt))      ! heat flux out through boundaries
allocate (q_conv(b_cnt))     ! convection heat flux from gas flow to boundaries
allocate (qc(b_cnt))         ! heat flux from volume radiation calc
allocate (qeb(b_cnt))        ! heat flux from known temperature boundary
allocate (b_T(b_cnt))        ! temperature of patch
allocate (bc_i(b_cnt))  ! i index of cell center for a patch !these could be temporary 
allocate (bc_j(b_cnt))  ! j index of cell center for a patch
allocate (bc_k(b_cnt))  ! k index of cell center for a patch
allocate (b_exh(b_cnt))  ! exhaust number
b_exh=0 ! most patches will not be exhausts so init whole array now
return
end


!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  wallq
!
! Purpose:  Solve boundary patch equations for radiosities in order to compute
!           wall temperatures and
!           melt surface heat flux
!
!           qa an input for wall "cells" has the radiation transfer rate [W] 
!              from the volume (it is not a flux) and these cells may have
!              more than one face bordering open cells. If so, assume that the
!              flux for these faces is uniform - a reasonable approximation
!              since the grid does not conform to the actual boundary
!
!==============================================================================
subroutine wallq
use gbl_var
implicit double precision (a-h,o-z)
integer save_nu_rt !temporary

!------------------------------------------------------------------------------------------------
! Set heat flux from the volume into the qc array 

q_incident_total=zero
q_incident_wall=zero
q_incident_inlet=zero
q_incident_exit=zero
q_incident_melt=zero
do n=1,b_cnt

   qc(n)=qa(bc_i(n)/2,bc_j(n)/2,bc_k(n)/2)/b_face_area(n) 
   q_incident_total=q_incident_total + (qc(n)*patch_area(n))
   ityp=b_typ(n)
   select case (ityp)
   case (1) !wall
      q_incident_wall=q_incident_wall+(qc(n)*patch_area(n))
   case (2) !inlet
      q_incident_inlet=q_incident_inlet+(qc(n)*patch_area(n))
      qeb(n)=sig*b_T(n)**4 !emission from inlet boundary into furnace
   case (3) !exit
      q_incident_exit=q_incident_exit+(qc(n)*patch_area(n))
      qeb(n)=sig*b_T(n)**4 !emission from outlet boundary into furnace
   case (4) !melt
      q_incident_melt=q_incident_melt+(qc(n)*patch_area(n))
      qeb(n)=emis(n)*sig*b_T(n)**4 !emission from melt surface into furnace
      qe(bc_i(n)/2,bc_j(n)/2,bc_k(n)/2)=qeb(n) !save for print out
   end select
enddo

qc_avg=q_incident_total/boundary_area_total   
if (irad_qc>0) write(nu_qcave,*) itr_gas,qc_avg

!write out file for visualizing qc
if (iwalvis==1) call wall_display(1)


!------------------------------------------------------------------------------------------------
! Gauss-Seidel iteration to solve linear equation set for radiation exchange between wall patches

wall_radiosity_tolerance=1.0d-10                               
radiation_boundary_tolerance=1.0d-10
!wall_radiosity_tolerance=1.0d-7
!radiation_boundary_tolerance=1.0d-7
prev_q_melt_chg=1.0d+6 !set so do not jump out first time

!Solve for new radiosities with new qc & qconv (for nn=1)
!Iterate until q_melt is converged (nn>1)
cpus1=secnds(0.0) !reinitialize timer

!do nn=1,maxri1
nn=1
nnextra=maxri1
do while (nn <= maxri1 .and. nn <= nnextra)
   minradi=minri2
   prev_rad_chg_mean=1.0d+6 !set so do not jump out first time

   call calc_q_conv_wall

   !Repeat Gauss-Seidel sweeps until radiosity is converged
   !do n=1,maxri2
   n=1
   nextra=maxri2
   do while (n <= maxri2.and.n <= nextra)
      rel_rad_chg_mean=zero
      iterw=iterw+1

      !Gauss-Seidel sweep over each boundary patch i
      do i=1,b_cnt
         ! Calculate sum of radiosity times view factors
         ! Equals radiation flux incident from other surfaces
         rad_from_bnds=zero
         if (b_typ(i)==1 .or. emis(i)<one) then
            call get_radiation_from_boundaries(i)
         endif

         rel_change=radiosity(i) !save old to calc relative change    
         ! Calculate new radiosity
         if (b_typ(i)==1) then !have a wall, which has a known heat flux B.C.
            radiosity(i) = max(qc(i) + q_conv(i) - q_amb(i) + rad_from_bnds, emis(i)*radi_min)
         else !not a wall (melt surf, inlet, or exit, which has known T B.C.; radiosity=emision+reflection
            !radiosity(i) = emis(i)*sig*T(b_i(i),b_j(i),b_k(i))**4 + (one-emis(i))*(rad_from_bnds + qc(i) )
            radiosity(i) = qeb(i) + (one-emis(i))*(rad_from_bnds + qc(i) )
         endif 
      

         if (rel_change < wall_radiosity_tolerance) then        
            rel_change=zero !should never hit this
         else
            rel_change = abs((rel_change - radiosity(i))/rel_change)
            rel_rad_chg_mean = rel_rad_chg_mean + rel_change
         endif

      enddo !i= 
      rel_rad_chg_mean = rel_rad_chg_mean/b_cnt
      if (irad_rad>0) write(nu_cwal,*) iterw,rel_rad_chg_mean

      ! Exit loop if change tolerance limit met
      !if (rel_rad_chg_mean >= prev_rad_chg_mean .and. &
      !    rel_rad_chg_mean < wall_radiosity_tolerance .and. n>minradi) exit
      if (rel_rad_chg_mean < wall_radiosity_tolerance .and. n>minradi) then
         !exit        
         if(nextra==maxri2) nextra = n + 3
      endif
      prev_rad_chg_mean=rel_rad_chg_mean

      cpus2=secnds(cpus1) ! get elapsed time since timer set
      if (cpus2>20) then
         ! the timer has run for more than 20 seconds

         !---------------------------------------------------
         ! Check for communication from GUI

         filename=casedir//'\gui_update.txt'
         inquire (file=filename,exist=exst)
         if (exst) then
            !user has requested change in gui update status
            open(nu_guiup,file=filename)
            read (nu_guiup,*) gui_update
            close(nu_guiup)
            nfn1=delfilesqq(filename) !delete file
         endif
 
         filename=casedir//'\runstop.dat'
         inquire (file=filename,exist=runstop_exist)
         if (runstop_exist) then
            !user has requested to stop the program
            !nfn1=delfilesqq(filename)  !delete file
            exit ! return to main routine to write files and stop program
         endif
         call qrn2
      endif
      n=n+1
   enddo !n=
   minradi=1

   !---------------------------------------------------
   ! Update wall temperature and melt surface heat flux

   !wall_rad_T=zero !init array to save wall temperatures
   !wall_TxArea_total=zero 
   prev_q_melt=q_melt
   q_melt=zero
   !q_rad_inlet=zero
   !q_rad_outlet=zero
   
   do i=1,b_cnt
      ityp=b_typ(i)
      select case (ityp)
      case (1) !have a wall, compute its temperature
         !b_T(i)=( (radiosity(i) )/sig )**0.25d+0
         b_T(i)=( max(zero,(radiosity(i) - (q_amb(i)-q_conv(i))*(one-emis(i))/emis(i))/sig) )**0.25d+0
         b_T(i)=max(T_mnK,b_T(i))
      !   T(b_i(i),b_j(i),b_k(i)) = b_T(i)

         !save new T to replace later in already saved T
      !   wall_rad_T(bc_i(i),bc_j(i),bc_k(i))=b_T(i)
               !need cell centers in wall_rad_T, init to zero
               !if not zero, then could we sum (somehow) the caluclation from this patch    
      !   wall_TxArea_total=wall_TxArea_total + (b_T(i) * patch_area(i))
      
      !case (2) ! have inlet
      !   call get_radiation_from_boundaries(i)
      !   q_rad_inlet = q_rad_inlet + (qc(i) + rad_from_bnds - qeb(i))*patch_area(i) 
               
      !case (3) ! have outlet
      !   call get_radiation_from_boundaries(i)
      !   q_rad_outlet = q_rad_outlet + (qc(i) + rad_from_bnds - qeb(i))*patch_area(i)
     
      case (4) !have a melt surface, compute its heat flux
         call get_radiation_from_boundaries(i)
         !b_T(i)=T(b_i(i),b_j(i),b_k(i)) !melt surface temperature is a B.C.
         qrs(b_i(i),b_j(i)) = qc(i) + q_conv(i) + rad_from_bnds - radiosity(i)
         !qrs(b_i(i),b_j(i)) = emis(i)*(qc(i) + rad_from_bnds - sig*b_T(i)**4)
         q_melt=q_melt + (qrs(b_i(i),b_j(i)) * patch_area(i)) !Total loss to melt surface
      end select 
   enddo

   call wall_q_amb
   if (irad_amb>0) write(nu_qamb,*) iterw,qamb_avg
   qrs_avg=q_melt/melt_area_total
   if (irad_qrs>0) write(nu_qrs,*) iterw,qrs_avg
   !avg_wall_T=wall_TxArea_total/wall_area_total  

   ! Exit loop if change tolerance limit met
   q_melt_chg=abs((prev_q_melt-q_melt)/(q_melt+small16))
   !if (q_melt_chg >= prev_q_melt_chg .and. q_melt_chg < radiation_boundary_tolerance) exit
   if (q_melt_chg < radiation_boundary_tolerance) then ! exit
      if (nnextra == maxri1) nnextra = nn+1
   endif
   prev_q_melt_chg=q_melt_chg

   if (runstop_exist) exit ! writes files and stops
   nn=nn+1
enddo !nn=looping on q to boundaries and convective q to melt

!------------------------------------------------------------------------------------------------
! end of Gauss-Seidel iteration to solve linear equation set for radiation exchange between wall patches
!------------------------------------------------------------------------------------------------

q_rad_outlet=0.0d0
q_rad_inlet=0.0d0
wall_TxArea_total=0.0d0
qm_flame=0.0d0
qm_bnds=0.0d0
qm_conv=0.0d0

do i=1,b_cnt
   ityp=b_typ(i)
   select case (ityp)
   case (1) !have a wall, put T back in the 3D field array - needed ? or does next line handle this? 
      T(b_i(i),b_j(i),b_k(i)) = b_T(i)

      !save new T to replace later in already saved T
      wall_rad_T(bc_i(i),bc_j(i),bc_k(i))=b_T(i)
            !need cell centers in wall_rad_T, init to zero
            !if not zero, then could we sum (somehow) the caluclation from this patch  
      wall_TxArea_total=wall_TxArea_total + (b_T(i) * patch_area(i))
   
   case (2) ! have inlet
      call get_radiation_from_boundaries(i)
      q_rad_inlet = q_rad_inlet + (qc(i) + rad_from_bnds - qeb(i))*patch_area(i) 
            
   case (3) ! have outlet
      call get_radiation_from_boundaries(i)
      q_rad_outlet = q_rad_outlet + (qc(i) + rad_from_bnds - qeb(i))*patch_area(i)
  
   case (4) !have a melt surface, compute its heat flux
      call get_radiation_from_boundaries(i) 
      !b_T(i)=T(b_i(i),b_j(i),b_k(i)) !melt surface temperature is a B.C.
      !qrs(b_i(i),b_j(i)) = qc(i) + q_conv(i) + rad_from_bnds - radiosity(i) !just did this in the loop
      !qrs(b_i(i),b_j(i)) = emis(i)*(qc(i) + rad_from_bnds - sig*b_T(i)**4)
      !q_melt=q_melt + (qrs(b_i(i),b_j(i)) * patch_area(i)) !Total loss to melt surface, just did this in the loop
      !qm_absorb=emis(i)*(qc(i)+rad_from_bnds)+q_conv(i)
      f_melt=1.0d0-qeb(i)/(emis(i)*(qc(i)+rad_from_bnds)+q_conv(i))
      qm_flame=qm_flame+f_melt*emis(i)*qc(i)*patch_area(i)       !rad into melt directly from flame (media)
      qm_bnds=qm_bnds+f_melt*emis(i)*rad_from_bnds*patch_area(i) !rad into melt from boundary surfaces
      qm_conv=qm_conv+f_melt*q_conv(i)*patch_area(i)     !heat into melt from convection at surface
   end select 
enddo

avg_wall_T=wall_TxArea_total/wall_area_total 

!if any exhaust wall temperature type is a fraction of the average wall temperature, then reset the exhaust 
if (have_frac_exh_T==1) then
	do i=1,b_cnt
		 if (b_typ(i)/=3) cycle
	     if (exh_type(b_exh(i))==0) then
			b_T(i)=exh_frac(b_exh(i))*avg_wall_T
		 else
		    b_T(i)=exh_fixed(b_exh(i))
		 endif
	enddo
endif 

   ibTsta=0
   if (ibTsta>0) then
      ! debug print out boundary face temperatures, indexes
      filename=casedir//'\bTstart2'//runum//'c.txt'
      open(nu_bTsta,file=filename)
      write(nu_bTsta,*) 'Boundary Face Temperature, indexes'
      write(nu_bTsta,*) '================================='
      write(nu_bTsta,*) '  Boundary: n,    b_type,    b_T,        i,        j,        k        b_exh'
      write(nu_bTsta,*) '================================='
      do n=1,b_cnt !n is the 'near or from' patch
	     if (b_typ(n)==3) then
			write(nu_bTsta,'(i5,i5,g15.7,3i5)') n,b_typ(n),b_T(n),b_i(n),b_j(n),b_k(n),b_exh(n)
		 endif
      enddo

	  write(nu_bTsta,*) '  '
      write(nu_bTsta,*) '================================='
      write(nu_bTsta,*) '  Exhaust: n         i1        j1        k1       i2      j2    k2'
      write(nu_bTsta,*) '================================='
 	  do n=1,nex
			write(nu_bTsta,*) n,exh_i1(n),exh_j1(n),exh_k1(n),exh_i2(n),exh_j2(n),exh_k2(n)
	  enddo

	  write(nu_bTsta,*) '  '
      write(nu_bTsta,*) '================================='
      write(nu_bTsta,*) '  Exhaust: n         orient        direct     type      frac      fixed'
      write(nu_bTsta,*) '================================='
 	  do n=1,nex
			write(nu_bTsta,*) n,exh_orient(n),exh_direct(n),exh_type(n),exh_frac(n),exh_fixed(n)
	  enddo
	  write(nu_bTsta,*) '  '
	  write(nu_bTsta,*)avg_wall_T
      close(nu_bTsta)
   endif

!write out convective heat transfer to wall average
qconv_avg=qconv_total/boundary_area_total
if (irad_conv>0) write(nu_qconv,*) itr_gas,qconv_avg

!write out file for visualizing qconv
if (iwalvis==1) call wall_display(2)

!recalibrate soot oxidation kinetic constant
!q_melt_req=0.74694352735085E+06 ! (W) TC21 case0020
!q_melt_req=0.27667460382403E+07 ! (W) Pittston B case1703
tol_aoxid=1.0d-3 
if (isoot_cal == 1) then
   !a_adj = max(min(q_melt/q_melt_req,2.0d+0),0.5d+0)
   if (q_melt<zero) q_melt=zero
   q_ratio=q_melt/q_melt_req
   !if (q_ratio > 1.1d0 .or. q_ratio < 0.9d0) then
      a_adj = max(min(q_ratio,2.0d+0),0.5d+0)
   !else
   !   a_adj = max(min(q_ratio**2,5.0d+0),0.2d+0)
   !endif
   !if (abs(a_adj-1) < tol_aoxid) then     
      !call stop_run("Normal completion of soot calibration.")
   !endif
   !aoxid = aoxid * a_adj !adjust soot oxidation pre-exponential kinetic constant
   !rf_aform=0.3d0 !moved to relaxation file
   !rf_afover=0.8d0 !moved to relaxation file
   !rf_aform=0.0d0
   if (a_adj > one) then
      aform = rf_afover*aform/a_adj+(1.0d0-rf_afover)*aform
   else
      aform = rf_aform*aform/a_adj+(1.0d0-rf_aform)*aform
   endif
   !keep running list of aoxid values
   !write(nu_aox,'(i5,5e22.14)') itr_gas,aoxid,aform,q_melt,q_melt_req,soot_tot
   write(nu_soot,'(i5,6e22.14)') itr_gas,aoxid,aform,q_melt,q_melt_req,q_ratio,soot_tot
endif

!write out file for visualizing b_T
if (iwalvis==1) call wall_display(3)

if (itwal==1) then
   !write out wall temperature info (wall_rad_T array) to twall file
   filename=casedir//'\twall'//runum//'c.txt'
   open(nu_twal,file=filename)
         write(nu_twal,"(/5x,a,i6,a,i6)") 'Number of interior wall cells: ',iwall_count, &
                                        '  Number of total boundary patches: ',b_cnt 
         write(nu_twal,"(/5x,a,f11.3)") 'Average wall temperature: ',avg_wall_T
   write(nu_twal,"(/5x,a)") 'Wall temperature array:'

   save_nu_rt=nu_rt 
   nu_rt=nu_twal !set nu_rt for using pform routine
   call pform(wall_rad_T,2)
   nu_rt=save_nu_rt !restore main output file unit number
   close(nu_twal)
endif

!also keep running list of average wall temperature
if (iwall_info==1) write(nu_twala,*) itr_gas,avg_wall_T

if (ibTemp==1) then
   ! debug print out boundary face temperatures, wall loss, rad heat from volume
   filename=casedir//'\bTemp'//runum//'c.txt'
   open(nu_bTemp,file=filename)
   write(nu_bTemp,*) 'Boundary Face Temperature, q_ambient, qc, q_conv'
   write(nu_bTemp,*) '================================='
   write(nu_bTemp,*) '  Boundary: n,    b_type,    b_T,        q_amb,        qc,        q_conv'
   write(nu_bTemp,*) '================================='
   do n=1,b_cnt !n is the 'near or from' patch
      write(nu_bTemp,'(i5,i5,g15.7,3e22.14)') n,b_typ(n),b_T(n),q_amb(n),qc(n),q_conv(n)
   enddo
   close(nu_bTemp)
endif !debug

return
end

!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  calculate convective heat transfer to walls
!
! Purpose:  Use turbulent energy wall function to compute
!           convective heat transfer to walls
!           Value cannot be less than the conduction h.t. rate
!
!==============================================================================
subroutine calc_q_conv_wall
use gbl_var
implicit double precision (a-h,o-z)
real(8) delta_T

! FYI
!cmu = 9.0d-2 !k-e turbulence model constant
!kappa_cmu = 0.4d0 * 9.0d-2**0.25d+0 !for energy wall function 
!E_cmu = 9.0d0 * 9.0d-2**0.25d+0 !for energy wall function 
!c_htw = 0.4d0*((pi/4)/sin(pi/4))*sqrt(26.0d0)*(0.7d0-one)/0.7d0**0.25d0 !for energy wall function

qconv_total=zero !total convection from gas to boundaries 

if (ihtran==1) then 
   !Start debug print out heat transfer to wall info
   !Note that file is replaced each time this routine is called
   calc_q_cnt = calc_q_cnt + 1
   write(calc_q_cnt_string,'(i3)') calc_q_cnt
   filename=casedir//'\heat_tran_wall'//runum//'c.txt'
   open(nu_htran,file=filename)
   write(nu_htran,*) 'Heat transfer to wall items, call number ',calc_q_cnt_string
   write(nu_htran,*) '================================='
   write(nu_htran,*) 'Boundary: n, b_typ, i, j, k, b_T, Tgas, inc, jnc, knc, delta_T, gmu_T, h_t,kgas/ds'
   write(nu_htran,*) '================================='
endif


do n=1,b_cnt
   if (b_typ(n)==2 .or. b_typ(n)==3) then
      q_conv(n)=zero
      cycle
   endif
   !Get indexes of flow cell center next to wall patch
   inc=b_i(n)
   jnc=b_j(n)
   knc=b_k(n)
   select case (b_orient(n))
   case(1) ! x-normal patch
      if(b_direct(n)==0) then !boundary face area vector points in + direction
        inc=inc+1
        ds=x(b_i(n)+1)-x(b_i(n)) !set distance from boundary patch to neighbor cell center     
      else                    !boundary face area vector points in - direction
        inc=inc-1
        ds=x(b_i(n))-x(b_i(n)-1) !set distance from boundary patch to neighbor cell center     
      endif
   case(2) ! y-normal patch
      if(b_direct(n)==0) then
        jnc=jnc+1
        ds=r(b_j(n)+1)-r(b_j(n)) !set distance from boundary patch to neighbor cell center     
      else
        jnc=jnc-1
        ds=r(b_j(n))-r(b_j(n)-1) !set distance from boundary patch to neighbor cell center     
      endif
   case(3) ! z-normal patch
      if(b_direct(n)==0) then
        knc=knc+1
        ds=z(b_k(n)+1)-z(b_k(n)) !set distance from boundary patch to neighbor cell center     
      else
        knc=knc-1
        ds=z(b_k(n))-z(b_k(n)-1) !set distance from boundary patch to neighbor cell center     
      endif
   end select
   !(inc,jnc,knc) = cell center index set of point next to wall patch

   cpmix(1)=cp0*(cpf(1)*gf(inc,jnc,knc,iyf) & !cp's are still normalized here
           +cpco2(1)*gf(inc,jnc,knc,iyco2)+cph2o(1)*gf(inc,jnc,knc,iyh2o) &
           +cpo2(1)*gf(inc,jnc,knc,iyo2)+cpn2(1)*gf(inc,jnc,knc,iyn2))

   Tgas = T(inc,jnc,knc) !T is in K here

   gmu_T = ((gmu3*Tgas+gmu2)*Tgas+gmu1)*Tgas+gmu0 !Viscosity of gas next to wall



   if (tk0*gf(inc,jnc,knc,ik) < 0.1d+0) then
      h_t=zero
   else

   sqrt_tke = sqrt(tk0*gf(inc,jnc,knc,ik))

      dtmp= (log(E_cmu*ds*dnst0*dnst(inc,jnc,knc)*sqrt_tke/gmu_T) &
             +(c_htw/sqrt_tke)) !need to check origin of this second term in literature 

      if (dtmp < 0.01d+0) then
         h_t=zero
      else
         h_t = min(50.0d0,cpmix(1)*kappa_cmu*dnst0*dnst(inc,jnc,knc)*sqrt_tke/dtmp)
      endif
   endif
   !kgas = ((kg3*Tgas+kg2)*Tgas+kg1)*Tgas+kg0 !Thermal conductivity of gas next to wall

   !q_conv is positive into the wall, i.e. when T(wall neighbor cell center) < b_T 
   delta_T = Tgas - b_T(n) !T diff between open neighbor cell and boundary patch  

   !q_conv is max of convective or conductive heat flux into wall from gas
   !User supplied h.t. coefficient, h_g, is used if larger than h calc. from turb. wall function
   !q_conv(n)=max(h_g,h_t,kgas/ds)*delta_T
   q_conv(n)=max(h_g,h_t)*delta_T
   qconv_total=qconv_total+(q_conv(n)*patch_area(n))
   if (ihtran==1) write(nu_htran,'(5i5,2g15.7,3i5,4e15.7)') n,b_typ(n),b_i(n),b_j(n),b_k(n),b_T(n), &
            Tgas,inc,jnc,knc,delta_T,gmu_T,h_t,kgas/ds
enddo
if (ihtran==1) close(nu_htran)

return
end


!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  get_radiation_from_boundaries(i)
!
! Purpose:  Calculate radiation from boundaries by multiplying radiosity by 
!           view factors.  Create view factors if they have not been presaved.
!
! Argument:  i  index of the "from" boundary patch 
!               
!==============================================================================
subroutine get_radiation_from_boundaries(i)
use gbl_var
implicit double precision (a-h,o-z)
dimension one_row_vf(b_cnt)
real(8) one_row_vf_sum

!i is the 'near or from' patch

rad_from_bnds=zero

if (preset_vf==1) then
   !have view factors already calculated
   do j=1,b_cnt
      rad_from_bnds = rad_from_bnds + radiosity(j)*vf(i,j)
   enddo
else
   !must calculate view factors row by row when needed
   one_row_vf=zero
   one_row_vf_sum=zero

   do j=1,b_cnt !j is the 'far or to' patch
      if (i==j) cycle !at same patch (they can't see themselves)
      denominator = pi * ((x(b_i(i))-x(b_i(j)))**2 &
                                + (r(b_j(i))-r(b_j(j)))**2 &
                                + (z(b_k(i))-z(b_k(j)))**2)**2
      select case (b_orient(i))
      case (1) !x-normal patch
         !View factor is 0 if far patch is on same plane or out of view
         if (b_direct(i)==0 .and. b_i(j)<=b_i(i)) cycle 
         if (b_direct(i)==1 .and. b_i(j)>=b_i(i)) cycle 
         select case (b_orient(j))
         case (1) !x-normal far patch
            one_row_vf(j) = (x(b_i(j))-x(b_i(i)))**2 * patch_area(j) / denominator
         case (2) !y-normal far patch
            one_row_vf(j) = abs((x(b_i(j))-x(b_i(i))) * (r(b_j(j))-r(b_j(i)))) &
               * patch_area(j) / denominator
         case (3) !z-normal far patch
            one_row_vf(j) = abs((x(b_i(j))-x(b_i(i))) * (z(b_k(j))-z(b_k(i)))) &
               * patch_area(j) / denominator
         end select

      case (2) !y-normal patch
         !View factor is 0 if far patch is on same plane or out of view
         if (b_direct(i)==0 .and. b_j(j)<=b_j(i)) cycle 
         if (b_direct(i)==1 .and. b_j(j)>=b_j(i)) cycle 
         select case (b_orient(j))
         case (1) !x-normal far patch
            one_row_vf(j) = abs((r(b_j(j))-r(b_j(i))) * (x(b_i(j))-x(b_i(i)))) &
               * patch_area(j) / denominator
         case (2) !y-normal far patch
            one_row_vf(j) = (r(b_j(j))-r(b_j(i)))**2 * patch_area(j) / denominator
         case (3) !z-normal far patch
            one_row_vf(j) = abs((r(b_j(j))-r(b_j(i))) * (z(b_k(j))-z(b_k(i)))) &
               * patch_area(j) / denominator
         end select

      case (3) !z-normal patch
         !View factor is 0 if far patch is on same plane or out of view
         if (b_direct(i)==0 .and. b_k(j)<=b_k(i)) cycle 
         if (b_direct(i)==1 .and. b_k(j)>=b_k(i)) cycle 
         select case (b_orient(j))
         case (1) !x-normal far patch
            one_row_vf(j) = abs((z(b_k(j))-z(b_k(i))) * (x(b_i(j))-x(b_i(i)))) &
               * patch_area(j) / denominator
         case (2) !y-normal far patch
            one_row_vf(j) = abs((z(b_k(j))-z(b_k(i))) * (r(b_j(j))-r(b_j(i)))) &
               * patch_area(j) / denominator
         case (3) !z-normal far patch
            one_row_vf(j) = (z(b_k(j))-z(b_k(i)))**2 * patch_area(j) / denominator
         end select
      end select
      one_row_vf_sum=one_row_vf_sum + one_row_vf(j)
   enddo !j=

   !Normalize view factors and calculate radiation
   do j=1,b_cnt !j is the 'far or to' patch
      if (one_row_vf_sum==zero) cycle
      one_row_vf(j)=one_row_vf(j)/one_row_vf_sum
      rad_from_bnds = rad_from_bnds + radiosity(j)*one_row_vf(j)
   enddo
endif

return
end


!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  wall_q_amb
!
! Purpose:  Calculate wall heat loss to ambient
!
! Called from:  qrnf
!               
!==============================================================================
subroutine wall_q_amb
use gbl_var
implicit double precision (a-h,o-z)

q_wall_loss=zero
do n=1,b_cnt

   if (b_typ(n).ne.1) then
      !not at a wall
      q_amb(n)=zero
      cycle
   endif

   ! get wall parameters:
   !   w_k0   wall conductivity (W/m k)
   !   w_d0   wall thickness    (m)
   !   t_a0   ambient temperature (k)
   !   h_a0   external wall heat transfer coefficient (W/m^2 k)
   !   u_a0 = zero if wall conductivity or external heat transfer coeff. < 1d-10
   !        = 1 / [w_d0/w_k0 + 1/h_a0], otherwise (W/m^2 k)
   !        = overall heat transfer coefficient between interior surface and ambiant in room
   nwall=iwall(bc_i(n)/2,bc_j(n)/2,bc_k(n)/2)
   if (nwall > 0) then
      w_k0=w1_k(nwall)
      h_a0=w1_ha(nwall)
      w_d0=w1_d(nwall)
      t_a0=w1_ta(nwall)
   else
      w_k0=w_k
      h_a0=h_a
      w_d0=w_d
      t_a0=t_a
   endif
   if (w_k0 < 1.0d-20 .or. h_a0 < 1.0d-20) then
      u_a0=zero
      !u_io=zero
   else
      u_a0=one/(w_d0/w_k0+one/h_a0)
      !if (h_g < 1.0d-20) then
      !   u_io=zero
      !else
      !   u_io=one/(one/h_g+w_d0/w_k0+one/h_a0)
      !endif

      !if (irad_test==1) then
      !      u_io=zero !debug code
      !      u_a0=zero !debug code 
      !endif
   endif

   q_amb(n)=U_a0*(b_T(n)-T_a0)  !loss rate to ambiant [W/m**2]  This is really a flux.
   !q_amb(n)=zero !adiabatic condition for testing!
   q_wall_loss = q_wall_loss + (q_amb(n) * patch_area(n)) !total wall loss to ambient
enddo

qamb_avg=q_wall_loss/wall_area_total !have average wall heat loss (will print from inside radiosity solver)

return
end


!==============================================================================
!==============================================================================
!==============================================================================
! Routine:  wall_display(array_indicator)
!
! Purpose:  Write out file for visualizing a boundary array
!
! Arguments:   array_indicator identifies the boundary array
!                 1 => qc arrary
!                 2 => qconv arrary
!                 3 => b_T arrary
! 
! Using file unit 81 (nu_walvis) 
!               
!==============================================================================
subroutine wall_display(array_indicator)
use gbl_var
implicit double precision (a-h,o-z)
integer array_indicator
allocate (wg(mp,np,lp+2)) !wall grid (The wall grid has a duplicated melt surface z slab.)
wg=zero

!Place q_array boundary patch values into the wg array
!Note that an extra z slab was added to the wall grid 
select case (array_indicator)
case (1) !qc 
   do n=1,b_cnt
      wg(b_i(n),b_j(n),b_k(n)+2)=qc(n) !copy over to patch position
      wg(bc_i(n),bc_j(n),bc_k(n)+2)=qc(n) !copy over to patch cell center position
   enddo
   filename=casedir//'\qc_wall'//runum//'c.txt'
   long_title='Radiation incident on boundary patches from the volume media:'

case (2) !qconv 
   do n=1,b_cnt
      wg(b_i(n),b_j(n),b_k(n)+2)=q_conv(n) !copy over to patch position
      wg(bc_i(n),bc_j(n),bc_k(n)+2)=q_conv(n) !copy over to patch cell center position
   enddo
   filename=casedir//'\qconv_wall'//runum//'c.txt'
   long_title='Convection from gas flow:'

case (3) !b_T 
   do n=1,b_cnt
      wg(b_i(n),b_j(n),b_k(n)+2)=b_T(n) !copy over to patch position
      wg(bc_i(n),bc_j(n),bc_k(n)+2)=b_T(n) !copy over to patch cell center position
   enddo
   filename=casedir//'\b_T_wall'//runum//'c.txt'
   long_title='Wall temperature'
end select

!write out file  
open(nu_walvis,file=filename)
write(nu_walvis,"(/5x,a)") long_title

do k=4,lp+2,2
   write(nu_walvis,"(/' Z(',i3,') = ',g12.3)") k,z(k-2)
   j1=2
   do
      j2=j1+10*2
      j2=min(j2,np)
      write(nu_walvis,"(/2x,'  X  /  Y',11(g11.3))") (r(j),j=j1,j2,2)
      do i=2,mp,2
         write(nu_walvis,"(g11.3,11e11.3)") x(i),(wg(i,j,k),j=j1,j2,2)
      enddo
      j1=j2+2
      if (j1.gt.np) exit
   enddo
enddo

close(nu_walvis)
deallocate (wg)
return
end


