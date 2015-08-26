!  Solvers.f90
!======================================================================
!  Alternating Direction Implicit (ADI)
!
!  Linear equation solvers using
!        general conjugate residual method (gcr) 
!        alternating direction line-by-line sweep (adi) 
!        using tridiagonal matrix algorithm in yz-planes
!        & Gauss-Seidel forward and backward sweeps in x-direction
!
!  This file contains the following routines:
!      gcr_solver(xt,ib,ie,jb,je,kb,ke)
!      adi_solver(xt,ib,ie,jb,je,kb,ke)
!      adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
!      adi_y_lines(xt,xt2,ld,i,jb,je,k)
!      adi_z_lines(xt,xt3,ld,i,j,kb,ke)
!
!  The following routines are the same as the above routines, 
!  except that they are only for the energy equation to provide special 
!  handling for conductive walls and walls with boundary conditions:
!      en_adi_solver(xt,ib,ie,jb,je,kb,ke)
!      en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
!      en_adi_y_lines(xt,xt2,ld,i,jb,je,k)
!      en_adi_z_lines(xt,xt3,ld,i,j,kb,ke)
!
!======================================================================
!======================================================================
!======================================================================
! Routine:  gcr_solver(xt,ib,ie,jb,je,kb,ke)
!
! Purpose:  solve linearized equations using
!           general conjugate residual method 
!           with overlapped swapping of ghost planes initiated
!           as soon as the plane to be swapped is solved
!
! Inputs
!   Arguments:    xt    function array to solve
!                 ib    beginning position x direction
!                 ie    ending    position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt
!  
! Notes:  ap, as, and bs arrays must be properly set on entry
!
!         solves: ap xt_p = sum (as xt_neighbor) + bs
!======================================================================
subroutine gcr_solver(xt,ib,ie,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
real*8, allocatable :: ro(:,:,:)
real*8, allocatable :: rn(:,:,:)
real*8, allocatable :: v(:,:,:,:)
real*8, allocatable :: s(:,:,:,:)
real*8, allocatable :: sf(:,:,:)
real*8, allocatable :: bs_tmp(:,:,:)
dimension xt(mp,np,lp)

kmax=8
allocate (ro(mz,nz,lz))
allocate (rn(mz,nz,lz))
allocate (v(mz,nz,lz,kmax))
allocate (s(mz,nz,lz,kmax))
allocate (sf(mp,np,lp))
allocate (bs_tmp(mp,np,lp))

ieh=mz-1
jeh=nz-1
keh=lz-1

if     (ib==5) then
   ieh=mz-2
elseif (jb==5) then
   jeh=nz-2
elseif (kb==5) then
   keh=lz-2
endif

s=zero

do k=2,lp;do j=2,np;do i=2,mp
   bs_tmp(i,j,k)=bs(i,j,k)
enddo;enddo;enddo


do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
!   bs(i,j,k)=ap(i,j,k)*p(i,j,k)   &
!            -p(i-2,j,k)*as(i,j,k,1,1)-p(i+2,j,k)*as(i,j,k,1,2) &
!            -p(i,j-2,k)*as(i,j,k,2,1)-p(i,j+2,k)*as(i,j,k,2,2) &
!            -p(i,j,k-2)*as(i,j,k,3,1)-p(i,j,k+2)*as(i,j,k,3,2)
   ro(id,jh,kh)=bs(i,j,k)-(ap(i,j,k)*xt(i,j,k)   &
            -xt(i-2,j,k)*as(i,j,k,1,1)-xt(i+2,j,k)*as(i,j,k,1,2) &
            -xt(i,j-2,k)*as(i,j,k,2,1)-xt(i,j+2,k)*as(i,j,k,2,2) &
            -xt(i,j,k-2)*as(i,j,k,3,1)-xt(i,j,k+2)*as(i,j,k,3,2))
   bs(i,j,k)=ro(id,jh,kh)
enddo;enddo;enddo
!eqtyp=1
!eqn=1
do kk=1,kmax
   sf=0
   call gauss_seidel(sf,ib,ie,jb,je,kb,ke)
   !call adi_solver(sf,ib,ie,jb,je,kb,ke)
   !call adi_solver(s(1,1,1,kk),2,mz-1,2,np-1,2,lp-1)
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      s(id,jh,kh,kk)=sf(i,j,k)
   enddo;enddo;enddo
   
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      !if (ibcell(i,j,k)>0) cycle
      v(id,jh,kh,kk)=(ap(i,j,k)*s(id,jh,kh,kk)     &
            -s(id-1,jh,  kh,  kk)*as(i,j,k,1,1) &
            -s(id+1,jh,  kh,  kk)*as(i,j,k,1,2) &
            -s(id,  jh-1,kh,  kk)*as(i,j,k,2,1) &
            -s(id,  jh+1,kh,  kk)*as(i,j,k,2,2) &
            -s(id,  jh,  kh-1,kk)*as(i,j,k,3,1) &
            -s(id,  jh,  kh+1,kk)*as(i,j,k,3,2))
   enddo;enddo;enddo
   
   do ii=1,kk-1

      alpha=0 !alpha = (vkk,vii)
      do k=2,keh;do j=2,jeh;do i=2,ieh
       !  if (ibcell(i,j,k)>0) cycle
         alpha=alpha+v(i,j,k,kk)*v(i,j,k,ii)
      enddo;enddo;enddo

      !vkk = vkk- alpha vii
      do k=2,keh;do j=2,jeh;do i=2,ieh
        ! if (ibcell(i,j,k)>0) cycle
         v(i,j,k,kk)=v(i,j,k,kk)-alpha*v(i,j,k,ii)
      enddo;enddo;enddo

      !skk = skk- alpha sii
      do k=2,keh;do j=2,jeh;do i=2,ieh
         !if (ibcell(i,j,k)>0) cycle
         s(i,j,k,kk)=s(i,j,k,kk)-alpha*s(i,j,k,ii)
      enddo;enddo;enddo
      
   enddo

   beta0=0 !beta0 = sqrt((vkk,vii))
   do k=2,keh;do j=2,jeh;do i=2,ieh
      !if (ibcell(i,j,k)>0) cycle
      beta0=beta0+v(i,j,k,kk)*v(i,j,k,kk)
   enddo;enddo;enddo
   beta0=sqrt(beta0)

   !vkk = vkk/beta0
   do k=2,keh;do j=2,jeh;do i=2,ieh
      !if (ibcell(i,j,k)>0) cycle
      v(i,j,k,kk)=v(i,j,k,kk)/beta0
   enddo;enddo;enddo

   !skk = skk/beta0
   do k=2,keh;do j=2,jeh;do i=2,ieh
      !if (ibcell(i,j,k)>0) cycle
      s(i,j,k,kk)=s(i,j,k,kk)/beta0
   enddo;enddo;enddo

   gamma=0 !gamma = (ro,vkk)
   do k=2,keh;do j=2,jeh;do i=2,ieh
      !if (ibcell(i,j,k)>0) cycle
      gamma=gamma+ro(i,j,k)*v(i,j,k,kk)
   enddo;enddo;enddo

   !gamma=1
   !gamma=beta0
   
   !xt = xt + gamma skk
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      !if (ibcell(i,j,k)>0) cycle
      xt(i,j,k)=xt(i,j,k)+gamma*s(id,jh,kh,kk)
   enddo;enddo;enddo

   !r = r + gamma vkk
   r_norm=zero
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      !if (ibcell(i,j,k)>0) cycle
      bs(i,j,k)=bs(i,j,k)-gamma*v(id,jh,kh,kk)
      r_norm=r_norm+(bs(i,j,k)*bs(i,j,k))
   enddo;enddo;enddo
   r_norm=sqrt(r_norm)/sqrt((mz-2.0d0)*(nz-2)*(lz-2))
continue
enddo

do k=2,lp;do j=2,np;do i=2,mp
   bs(i,j,k)=bs_tmp(i,j,k)
enddo;enddo;enddo

deallocate (ro,r,v,s,sf,bs_tmp)

return
end
!======================================================================
!======================================================================
!======================================================================
! Routine:  gmres_solver(xt,ib,ie,jb,je,kb,ke)
!
! Purpose:  solve linearized equations using
!           general minimal residual method 
!           with overlapped swapping of ghost planes initiated
!           as soon as the plane to be swapped is solved
!
! Inputs
!   Arguments:    xt    function array to solve
!                 ib    beginning position x direction
!                 ie    ending    position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt
!  
! Notes:  ap, as, and bs arrays must be properly set on entry
!
!         solves: ap xt_p = sum (as xt_neighbor) + bs
!======================================================================
subroutine gmres_solver(xt,ib,ie,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
dimension xt(mp,np,lp)

real*8, allocatable :: rn(:,:,:)
real*8, allocatable :: w(:,:,:)
real*8, allocatable :: h(:,:), g(:), c(:), s(:)
real*8, allocatable :: v(:,:,:,:), zg(:,:,:,:)
real*8, allocatable :: zf(:,:,:), bs_tmp(:,:,:)

kmax=8 
allocate (rn(mz,nz,lz),w(mz,nz,lz))
allocate (h(kmax,kmax),g(kmax+1),c(kmax),s(kmax))
allocate (v(mz,nz,lz,kmax),zg(mz,nz,lz,kmax))
allocate (zf(mp,np,lp))
allocate (bs_tmp(mp,np,lp))

ieh=mz-1
jeh=nz-1
keh=lz-1

if     (ib==5) then
   ieh=mz-2
elseif (jb==5) then
   jeh=nz-2
elseif (kb==5) then
   keh=lz-2
endif

s=zero

do k=2,lp;do j=2,np;do i=2,mp
   bs_tmp(i,j,k)=bs(i,j,k)
enddo;enddo;enddo

g(1)=0
do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
   rn(id,jh,kh)=bs(i,j,k)-(ap(i,j,k)*xt(i,j,k)   &
            -xt(i-2,j,k)*as(i,j,k,1,1)-xt(i+2,j,k)*as(i,j,k,1,2) &
            -xt(i,j-2,k)*as(i,j,k,2,1)-xt(i,j+2,k)*as(i,j,k,2,2) &
            -xt(i,j,k-2)*as(i,j,k,3,1)-xt(i,j,k+2)*as(i,j,k,3,2))
   g(1) = g(1) + rn(id,jh,kh)*rn(id,jh,kh)
enddo;enddo;enddo
g(1) = sqrt(g(1))
temp = 1/g(1)
v(:,:,:,1) = rn*temp

!eqtyp=1
!eqn=1
do kk=1,kmax
   zf=0
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      bs(i,j,k)=v(id,jh,kh,kk)
   enddo;enddo;enddo
   call gauss_seidel(zf,ib,ie,jb,je,kb,ke)
   !call adi_solver(zf,ib,ie,jb,je,kb,ke)
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      zg(id,jh,kh,kk)=zf(i,j,k)
   enddo;enddo;enddo
   
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      !if (ibcell(i,j,k)>0) cycle
     w(id,jh,kh)=(ap(i,j,k)*zg(id,jh,kh,kk)     &
            -zg(id-1,jh,  kh,  kk)*as(i,j,k,1,1) &
            -zg(id+1,jh,  kh,  kk)*as(i,j,k,1,2) &
            -zg(id,  jh-1,kh,  kk)*as(i,j,k,2,1) &
            -zg(id,  jh+1,kh,  kk)*as(i,j,k,2,2) &
            -zg(id,  jh,  kh-1,kk)*as(i,j,k,3,1) &
            -zg(id,  jh,  kh+1,kk)*as(i,j,k,3,2))
   enddo;enddo;enddo
   
   do ii=1,kk
      h(ii,kk)=0
      do k=2,keh;do j=2,jeh;do i=2,ieh
       !  if (ibcell(i,j,k)>0) cycle
         h(ii,kk)=h(ii,kk)+w(i,j,k)*v(i,j,k,ii)
      enddo;enddo;enddo
      do k=2,keh;do j=2,jeh;do i=2,ieh
        ! if (ibcell(i,j,k)>0) cycle
         w(i,j,k)=w(i,j,k)-h(ii,kk)*v(i,j,k,ii)
      enddo;enddo;enddo
   enddo

   do ii=1,kk-1
      temp = h(ii,kk)
      h(ii,  kk) =  c(ii) * temp + s(ii) * h(ii+1,kk)
      h(ii+1,kk) = -s(ii) * temp + c(ii) * h(ii+1,kk)
   enddo

   alpha = 0
   do k=2,keh;do j=2,jeh;do i=2,ieh
     ! if (ibcell(i,j,k)>0) cycle
      alpha = alpha + w(i,j,k)*w(i,j,k)
   enddo;enddo;enddo
   alpha = sqrt(alpha)
   if(alpha==0) exit

   hl = sqrt(h(kk,kk)*h(kk,kk)+alpha*alpha)
   c(kk) = h(kk,kk) / hl
   s(kk) = alpha / hl
   h(kk,kk) = hl
   g(kk+1) = -s(kk) * g(kk)
   g(kk  ) =  c(kk) * g(kk)

   if(kk==kmax) exit

   alpha = 1/alpha
   v(:,:,:,kk+1) = w * alpha
enddo

do ii=kk,1,-1
   temp = g(ii)
   do jj=ii+1,kk
      temp=temp-h(ii,jj)*c(jj)
   enddo
   c(ii)=temp/h(ii,ii)
enddo

do ii=1,kk
   do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
      xt(i,j,k)=xt(i,j,k)+c(ii)*zg(id,jh,kh,ii)
   enddo;enddo;enddo
enddo 

do k=2,lp;do j=2,np;do i=2,mp
   bs(i,j,k)=bs_tmp(i,j,k)
enddo;enddo;enddo

g(1)=0
do k=kb,ke,2;kh=k/2;do j=jb,je,2;jh=j/2;do i=ib,ie,2;id=i/2
   rn(id,jh,kh)=bs(i,j,k)-(ap(i,j,k)*xt(i,j,k)   &
            -xt(i-2,j,k)*as(i,j,k,1,1)-xt(i+2,j,k)*as(i,j,k,1,2) &
            -xt(i,j-2,k)*as(i,j,k,2,1)-xt(i,j+2,k)*as(i,j,k,2,2) &
            -xt(i,j,k-2)*as(i,j,k,3,1)-xt(i,j,k+2)*as(i,j,k,3,2))
   g(1) = g(1) + rn(id,jh,kh)*rn(id,jh,kh)
enddo;enddo;enddo
g(1) = sqrt(g(1))

      !Energy equation residual normalized
      if(eqtyp==2.and.nelg==8)then
         h_resid=zero
	   do k=kb,ke,2
	   do j=jb,je,2
      do i=ib,ie,2
         if(ibcell(i,j,k) > 0) cycle
            eq_max=max(abs(as(i,j,k,1,1)*xt(i-2,j,k)), &
                       abs(as(i,j,k,1,2)*xt(i+2,j,k)), &
                       abs(as(i,j,k,2,1)*xt(i,j-2,k)), &	              
                       abs(as(i,j,k,2,2)*xt(i,j+2,k)), &
                       abs(as(i,j,k,3,1)*xt(i,j,k-2)), &	              
                       abs(as(i,j,k,3,2)*xt(i,j,k+2)), &	              
                       abs(ap(i,j,k)*xt(i,j,k)),	    &         
                       abs(bs(i,j,k)))	              
            h_resid = h_resid                          &
                    +abs(as(i,j,k,1,1)*xt(i-2,j,k)     &
                       + as(i,j,k,1,2)*xt(i+2,j,k)     &
                       + as(i,j,k,2,1)*xt(i,j-2,k)	    &          
                       + as(i,j,k,2,2)*xt(i,j+2,k)     &
                       + as(i,j,k,3,1)*xt(i,j,k-2)	    &          
                       + as(i,j,k,3,2)*xt(i,j,k+2)	    &          
                       + bs(i,j,k)	                   & 
                       - ap(i,j,k)*xt(i,j,k) )/eq_max	              
         enddo;enddo;enddo
         h_resid=h_resid/ncells
      endif

deallocate (rn,w)
deallocate (h,g,c,s)
deallocate (v,zg)
deallocate (zf)
deallocate (bs_tmp)

return
end
!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_solver(xt,ib,ie,jb,je,kb,ke)
!
! Purpose:  do Gauss-Sidel solve sweeps over partition planes 
!           with overlapped swapping of ghost planes initiated
!           as soon as the plane to be swapped is solved
!
! Inputs
!   Arguments:    xt    function array to solve
!                 ib    beginning position x direction
!                 ie    ending    position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt
!  
! Notes:  ap, as, and bs arrays must be properly set on entry
!
!         solves: ap xt_p = sum (as xt_neighbor) + bs
!======================================================================
subroutine adi_solver(xt,ib,ie,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp)
real*8,allocatable :: xt2(:,:) !yz plane
real*8,allocatable :: xt4(:,:) !xz plane    
allocate(xt2(np,lp))
allocate(xt4(mp,lp))                        

!if(timing==1) call tim_start(tim_label)

if (eqtyp==1) then
   nim=ntimes(nel)
   ld=1
elseif (eqtyp==2) then
   nim=ntimesg(nelg)
   ld=2
else
   nim=ntimesm(nelm)
   ld=2
endif 


!if (eqtyp==1) then     !droplet
!   nim=ntimesd(eqn)
!   ld=1
!elseif (eqtyp==2) then !gas velocity
!   nim=ntimesv(eqn)
!   ld=2
!elseif (eqtyp==3) then !minor species
!   nim=ntimesm(eqn)
!   ld=2
!elseif (eqtyp==4) then !gf[turb]
!   nim=ntimesg(eqn)
!   ld=2
!elseif (eqtyp==5) then !gy[species]
!   nim=ntimesy(eqn)
!   ld=2
!elseif (eqtyp==6) then !particle
!   nim=ntimesp(eqn)
!   ld=1
!else
!   write(ncon,*) 'bad equation type:',eqtyp
!endif 

ibml=ib-ld   
iepl=ie+ld   
jbml=jb-ld
jepl=je+ld

if (nprocs==1) then !serial processing

   !----------------------------------------------------------------------
   !     Forward and backward sweeps in x direction
   !----------------------------------------------------------------------
   
   do kpass=1,nim
   	!if (timing==1) call tim_start('adi pass')

      !----------------------------------------------------------------------
      !     Forward and backward sweeps in x direction
      !----------------------------------------------------------------------
      do i=ib,ie,ld
         ipl=i+ld
         iml=i-ld
   		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
   	enddo
   	do i=ie,ib,-ld
         ipl=i+ld
         iml=i-ld
   		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
   	enddo

      !----------------------------------------------------------------------
      !     Forward and backward sweeps in y direction
      !----------------------------------------------------------------------

      goto 40  ! Comment out this line if want to do sweeps in y direction 
      do j=jb,je,ld
         jpl=j+ld
         jml=j-ld
   		call adi_xz_planes(xt,xt4,ld,ib,ie,j,kb,ke)
   	enddo
   	do j=je,jb,-ld
         jpl=j+ld
         jml=j-ld
   		call adi_xz_planes(xt,xt4,ld,ib,ie,j,kb,ke)
   	enddo
40    continue
   	!if(timing==1) call tim_stop('adi pass')
   enddo
   goto 50
endif   


!----------------------------------------------------------------------
!     Forward and backward sweeps in x direction
!----------------------------------------------------------------------
!	ib_l: for left most slab, ghost for some other proc
!	ib_m, ie_m: middle slabs
!	ib_r: for right most slab, ghost for some other proc

ib_loc=ib
ie_loc=ie

ib_l=ib_loc
ib_r=ie_loc
if (myid==0) then 
	ib_m=ib_loc
else
	ib_m=ib_l+ld
endif
if (myid==lastid) then
	ie_m=ie_loc
else
	ie_m=ib_r-ld
endif

do kpass=1,nim
!	if(timing==1) call tim_start('adi pass')
   ! calculate left-most slab and swap it
	if (myid.ne.0 .and. kpass.ne.1) then
		i=ib_l
      ipl=i+ld
      iml=i-ld
		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	endif
   !call swap_ghost_side_start(xt,ld,0,ib_loc,jb,kb)
	do i=ib_m,ie_m,ld
      ipl=i+ld
      iml=i-ld
		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	enddo
   !		ghost data of next proc needed, so finish swap
   !call swap_ghost_side_end(xt,ld,0,ib_loc,jb,kb)
   !		calculate right-most slab (twice) and swap it
	if (myid.ne.lastid) then
		i=ib_r
         ipl=i+ld
         iml=i-ld
		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
      !start of backward solver sweep
		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	endif
	!call swap_ghost_side_start(xt,ld,1,ib_loc,jb,kb)
	do i=ie_m,ib_m,-ld
         ipl=i+ld
         iml=i-ld
		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	enddo
   !		ghost data of prev proc needed, so finish swap
	!call swap_ghost_side_end(xt,ld,1,ib_loc,jb,kb)
   !		calculate left-most slab
	if (myid.ne.0) then
		i=ib_l
		ipl=i+ld
         iml=i-ld
		call adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	endif
!	if(timing==1) call tim_stop('adi pass')
enddo

! complete final backward swap
!call swap_ghost_side_start(xt,ld,0,ib_loc,jb,kb)
!call swap_ghost_side_end(xt,ld,0,ib_loc,jb,kb)

!----------------------------------------------------------------------
!  Check residuals !this part has been disabled by code in global_iter
!   needs some thought 
!----------------------------------------------------------------------
if (ipde_resid>0) then
   res0=zero
   resx=zero
   do i=ib,ie,ld
      ipl=i+ld
      iml=i-ld
   do j=jb,je,ld
      jml=j-ld
      jpl=j+ld
   do k=kb,ke,ld
      if (ap(i,j,k)==one) cycle
      kml=k-ld
      kpl=k+ld
      resc=as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k)
      resc=resc+as(i,j,k,2,2)*xt(i,jpl,k)+as(i,j,k,2,1)*xt(i,jml,k)
      resc=resc+as(i,j,k,3,2)*xt(i,j,kpl)+as(i,j,k,3,1)*xt(i,j,kml)
      resc=resc+bs(i,j,k)-ap(i,j,k)*xt(i,j,k)
      if (abs(bs(i,j,k))>=1.0d+16) cycle
      resc=abs(resc)
      resx=max(resx,resc)
      res0=res0+resc
   enddo;enddo;enddo

!   call mpi_allreduce(res0,res0_all,1,mpi_double_precision,mpi_sum,comm,ierr)
!   res0=res0_all
!   call mpi_allreduce(resx,resx_all,1,mpi_double_precision,mpi_max,comm,ierr)
!   resx=resx_all

!   if (myid==0) then
!      res0=res0/ncells            
!      if (eqtyp==1) then
!         resd(eqn,1)=res0
!         resd(eqn,2)=resx
!      elseif (eqtyp==2) then
!         resv(eqn,1)=res0
!         resv(eqn,2)=resx
!      elseif (eqtyp==3) then
!         resm(eqn,1)=res0
!         resm(eqn,2)=resx
!      elseif (eqtyp==4) then
!         resg(eqn,1)=res0
!         resg(eqn,2)=resx
!      elseif (eqtyp==5) then
!         resy(eqn,1)=res0
!         resy(eqn,2)=resx
!      elseif (eqtyp==6) then
!         resp(eqn,1)=res0
!         resp(eqn,2)=resx
!      else
!         write(ncon,*) 'bad equation number (residual):',eqtyp
!      endif        
!   endif
endif          

50   continue
!if(timing==1) call tim_stop(tim_label)
deallocate(xt2)
deallocate(xt4)  
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
!
! Purpose:  Solve linear equations for one yz plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt2   yz plane of xt array
!                 ld    1==cell centered array, 2==full array
!                 i     position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt,xt2
!
!======================================================================
subroutine adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp),xt2(np,lp)
real*8,allocatable :: xt3(:,:)
allocate(xt3(np,lp))

kepl=ke+ld
do k=kb,kepl,ld
do j=jb,jepl,ld
   xt2(j,k)=xt(i,j,k)
   xt3(j,k)=xt(i,j,k)
enddo;enddo

!do l=1,2     
   do k=kb,ke,ld
      call adi_y_lines(xt,xt2,ld,i,jb,je,k) 
   enddo
   do j=jb,je,ld
      call adi_z_lines_for_yz(xt,xt3,ld,i,j,kb,ke) 
   enddo
   do k=kb,ke,ld
   do j=jb,je,ld
   if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) cycle
      xt(i,j,k)=(xt2(j,k)+xt3(j,k))/two
   enddo;enddo
!enddo

deallocate(xt3)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_xz_planes(xt,xt4,ld,ib,ie,j,kb,ke)
!
! Purpose:  Solve linear equations for one xz plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt4   xz plane of xt array
!                 ld    1==cell centered array, 2==full array
!                 ib    beginning position x direction
!                 ie    ending    position x direction
!                 j               position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt,xt4
!
!======================================================================
subroutine adi_xz_planes(xt,xt4,ld,ib,ie,j,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp),xt4(mp,lp)
real*8,allocatable :: xt5(:,:)

allocate(xt5(mp,lp))

kepl=ke+ld
do k=kb,kepl,ld
do i=ib,iepl,ld
   xt4(i,k)=xt(i,j,k)
   xt5(i,k)=xt(i,j,k)
enddo;enddo

do l=1,2      
   do k=kb,ke,ld
      call adi_x_lines(xt,xt4,ld,ib,ie,j,k) 
   enddo
   do i=ib,ie,ld
      call adi_z_lines_for_xz(xt,xt5,ld,i,j,kb,ke) 
   enddo
   do k=kb,ke,ld
   do i=ib,ie,ld
      if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) cycle
      xt(i,j,k)=(xt4(i,k)+xt5(i,k))/two
   enddo;enddo
enddo

deallocate(xt5)
return
end



!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_y_lines(xt,xt2,ld,i,jb,je,k)
!
! Purpose:  Tridiagonal matrix solve of y direction lines in yz-plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt2   yz plane of xt array
!                 ld    1==cell centered array, 2==full array
!                 i     position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 k     position z direction
!
! Outputs
!   Arguments:    xt2
!
!======================================================================
subroutine adi_y_lines(xt,xt2,ld,i,jb,je,k) 
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp),xt2(np,lp),pj(np),qj(np)

kpl=k+ld
kml=k-ld
pj(jbml)=zero
qj(jbml)=xt(i,jbml,k)
do j=jb,je,ld
   jml=j-ld
   alp=ap(i,j,k)-pj(jml)*as(i,j,k,2,1)
   bsy=bs(i,j,k)
   !if (abs(alp)<=small20.or.ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) then
   !   pj(j)=zero
   !   qj(j)=xt(i,j,k)
   !else
      pj(j)=as(i,j,k,2,2)/alp
      bsy=bsy &
         +as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k) &
         +as(i,j,k,3,2)*xt(i,j,kpl)+as(i,j,k,3,1)*xt(i,j,kml)
      qj(j)=(bsy+as(i,j,k,2,1)*qj(jml))/alp
   !endif
enddo
do j=je,jb,-ld
   !if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) cycle
   xt2(j,k)=xt2(j+ld,k)*pj(j)+qj(j)
enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_z_lines_for_yz(xt,xt3,ld,i,j,kb,ke)
!
! Purpose:  Tridiagonal matrix solve of z direction lines in yz-plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt3   yz plane of xt array
!                 ld    1==celled centered array, 2==full array
!                 i     position x direction
!                 j     position y direction
!                 kb    beginning position y direction
!                 ke    ending    position y direction
!
! Outputs
!   Arguments:    xt3
!======================================================================
subroutine adi_z_lines_for_yz(xt,xt3,ld,i,j,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp),xt3(np,lp),pj(lp),qj(lp) 

jpl=j+ld
jml=j-ld
kbml=kb-ld
pj(kbml)=zero
qj(kbml)=xt(i,j,kbml)
do k=kb,ke,ld
   kml=k-ld
   alp=ap(i,j,k)-pj(kml)*as(i,j,k,3,1)
   bsy=bs(i,j,k)
   !if (abs(alp)<=small20.or.ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) then
   !   pj(k)=zero
   !   qj(k)=xt(i,j,k)
   !else
      pj(k)=as(i,j,k,3,2)/alp
      bsy=bsy &
         +as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k) &
         +as(i,j,k,2,2)*xt(i,jpl,k)+as(i,j,k,2,1)*xt(i,jml,k)
      qj(k)=(bsy+as(i,j,k,3,1)*qj(kml))/alp
   !endif
enddo
do k=ke,kb,-ld
   !if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) cycle
   xt3(j,k)=xt3(j,k+ld)*pj(k)+qj(k)
enddo
return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_z_lines_for_xz(xt,xt5,ld,i,j,kb,ke)
!
! Purpose:  Tridiagonal matrix solve of z direction lines in xz-plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt5   xz plane of xt array
!                 ld    1==celled centered array, 2==full array
!                 i     position x direction
!                 j     position y direction
!                 kb    beginning position y direction
!                 ke    ending    position y direction
!
! Outputs
!   Arguments:    xt5
!======================================================================
subroutine adi_z_lines_for_xz(xt,xt5,ld,i,j,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp),xt5(mp,lp),pj(lp),qj(lp) 

ipl=i+ld
iml=i-ld
kbml=kb-ld
pj(kbml)=zero
qj(kbml)=xt(i,j,kbml)
do k=kb,ke,ld
   kml=k-ld
   alp=ap(i,j,k)-pj(kml)*as(i,j,k,3,1)
   bsy=bs(i,j,k)
   if (abs(alp)<=small20.or.ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) then
      pj(k)=zero
      qj(k)=xt(i,j,k)
   else
      pj(k)=as(i,j,k,3,2)/alp
      bsy=bsy &
         +as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k) &
         +as(i,j,k,2,2)*xt(i,jpl,k)+as(i,j,k,2,1)*xt(i,jml,k)
      qj(k)=(bsy+as(i,j,k,3,1)*qj(kml))/alp
   endif
enddo
do k=ke,kb,-ld
   if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) cycle
   xt5(i,k)=xt5(i,k+ld)*pj(k)+qj(k)
enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  adi_x_lines(xt,xt4,ld,ib,ie,j,k)
!
! Purpose:  Tridiagonal matrix solve of x direction lines in xz-plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt4   xz plane of xt array
!                 ld    1==cell centered array, 2==full array
!                 ib    beginning position x direction
!                 ie    ending    position x direction
!                 j     position y direction
!                 k     position z direction
!
! Outputs
!   Arguments:    xt4
!
!======================================================================
subroutine adi_x_lines(xt,xt4,ld,ib,ie,j,k) 
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
dimension xt(mp,np,lp),xt4(mp,lp),pii(mp),qii(mp)

kpl=k+ld
kml=k-ld
pii(ibml)=zero
qii(ibml)=xt(ibml,j,k)
do i=ib,ie,ld
   iml=i-ld
   alp=ap(i,j,k)-pii(iml)*as(i,j,k,1,1)
   bsy=bs(i,j,k)
   if (abs(alp)<=small20.or.ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) then
      pii(i)=zero
      qii(i)=xt(i,j,k)
   else
      pii(i)=as(i,j,k,1,2)/alp
      bsy=bsy &
         +as(i,j,k,2,2)*xt(i,jpl,k)+as(i,j,k,2,1)*xt(i,jml,k) &
         +as(i,j,k,3,2)*xt(i,j,kpl)+as(i,j,k,3,1)*xt(i,j,kml)
      qii(i)=(bsy+as(i,j,k,1,1)*qii(iml))/alp
   endif
enddo
do i=ie,ib,-ld
   if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1) cycle
   xt4(i,k)=xt4(i+ld,k)*pii(i)+qii(i)
enddo
return
end


!================================================================
!================================================================
!    
!
!  The following en_... routines are only used for the energy
!  equation (eqtyp=4, eqn=1 for enthalpy (ih)).
!
!
!======================================================================
!======================================================================
!======================================================================
! Routine:  en_adi_solver(xt,ib,ie,jb,je,kb,ke)
!
! Purpose:  do Gauss-Sidel solve sweeps over partition planes 
!           with overlapped swapping of ghost planes initiated
!           as soon as the plane to be swapped is solved
!
! Inputs
!   Arguments:    xt    function array to solve
!                 ib    beginning position x direction
!                 ie    ending    position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt
!  
! Notes:  ap, as, and bs arrays must be properly set on entry
!
!======================================================================
subroutine en_adi_solver(xt,ib,ie,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
!common /adi1/ipl,iml,jbml,jepl,nim
dimension xt(mp,np,lp)
real*8,allocatable :: xt2(:,:)
allocate(xt2(np,lp))

!if(timing==1) call tim_start(tim_label)

!eqtyp==4, eqn==1 gf(,,,ih) enthalpy
nim=ntimesg(1)
ld=2

ib_loc=ib
ie_loc=ie

jbml=jb-ld
jepl=je+ld

!----------------------------------------------------------------------
!     forward and backward sweeps in x- and z-domains
!----------------------------------------------------------------------
!	ib_l: for left most slab, ghost for some other proc
!	ib_m, ie_m: middle slabs
!	ib_r: for right most slab, ghost for some other proc
ib_l=ib_loc
ib_r=ie_loc
if (myid==0) then 
	ib_m=ib_loc
else
	ib_m=ib_l+ld
endif
if (myid==lastid) then
	ie_m=ie_loc
else
	ie_m=ib_r-ld
endif

do kpass=1,nim
!	if(timing==1) call tim_start('en_adi pass')
   !		calculate left-most slab and swap it
	if (myid.ne.0 .and. kpass.ne.1) then
		i=ib_l
      ipl=i+ld
      iml=i-ld
		call en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	endif
	!call swap_ghost_side_start(xt,ld,0,ib_loc,jb,kb)
	do i=ib_m,ie_m,ld
      ipl=i+ld
      iml=i-ld
		call en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	enddo
   !		ghost data of next proc needed, so finish swap
	!call swap_ghost_side_end(xt,ld,0,ib_loc,jb,kb)
   !		calculate right-most slab (twice) and swap it
	if (myid.ne.lastid) then
		i=ib_r
      ipl=i+ld
      iml=i-ld
		call en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
         !start of backward solver sweep
		call en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)  
	endif
	!call swap_ghost_side_start(xt,ld,1,ib_loc,jb,kb)
	do i=ie_m,ib_m,-ld
      ipl=i+ld
      iml=i-ld
		call en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	enddo
   !		ghost data of prev proc needed, so finish swap
	!call swap_ghost_side_end(xt,ld,1,ib_loc,jb,kb)
   !		calculate left-most slab
	if (myid.ne.0) then
		i=ib_l
		ipl=i+ld
      iml=i-ld
		call en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
	endif
	!if(timing==1) call tim_stop('en_adi pass')
enddo

! complete final backward swap
!call swap_ghost_side_start(xt,ld,0,ib_loc,jb,kb)
!call swap_ghost_side_end(xt,ld,0,ib_loc,jb,kb)

!----------------------------------------------------------------------
!  Check residuals !this part has been disabled by code in global_iter
!   needs some thought 
!----------------------------------------------------------------------
if (ipde_resid>0) then
   res0=zero
   resx=zero
   do i=ib,ie,ld
      ipl=i+ld
      iml=i-ld
   do j=jb,je,ld
      jml=j-ld
      jpl=j+ld
   do k=kb,ke,ld
      if (ap(i,j,k)==one) cycle
      kml=k-ld
      kpl=k+ld
      resc=as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k)
      resc=resc+as(i,j,k,2,2)*xt(i,jpl,k)+as(i,j,k,2,1)*xt(i,jml,k)
      resc=resc+as(i,j,k,3,2)*xt(i,j,kpl)+as(i,j,k,3,1)*xt(i,j,kml)
      resc=resc+bs(i,j,k)-ap(i,j,k)*xt(i,j,k)
      if (abs(bs(i,j,k))>=1.0d+16) cycle
      resc=abs(resc)
      resx=max(resx,resc)
      res0=res0+resc
   enddo;enddo;enddo

!   call mpi_allreduce(res0,res0_all,1,mpi_double_precision,mpi_sum,comm,ierr)
!   res0=res0_all
!   call mpi_allreduce(resx,resx_all,1,mpi_double_precision,mpi_max,comm,ierr)
!   resx=resx_all
   if(myid==0) then
      res0=res0/ncells            
      resg(nelg,1)=res0
      resg(nelg,2)=resx
   endif
endif   
       
!if(timing==1) call tim_stop(tim_label)
deallocate(xt2)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
!
! Purpose:  Solve linear equations for one yz plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt2   yz plane of xt array
!                 ld    1==celled centered array, 2==full array
!                 i     position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 kb    beginning position z direction
!                 ke    ending    position z direction
!
! Outputs
!   Arguments:    xt,xt2
!
!======================================================================
subroutine en_adi_yz_planes(xt,xt2,ld,i,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
!common /adi1/ipl,iml,jbml,jepl,nim
dimension xt(mp,np,lp),xt2(np,lp)
real*8,allocatable :: xt3(:,:)
allocate(xt3(np,lp))

kepl=ke+ld
do k=kb,kepl,ld
do j=jb,jepl,ld
   xt2(j,k)=xt(i,j,k)
   xt3(j,k)=xt(i,j,k)
enddo;enddo

!do l=1,2            
   do k=kb,ke,ld
      call en_adi_y_lines(xt,xt2,ld,i,jb,je,k)
   enddo
   do j=jb,je,ld
      call en_adi_z_lines(xt,xt3,ld,i,j,kb,ke)
   enddo
   do k=kb,ke,ld
   do j=jb,je,ld
      if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1 .and. &
          ibcell(i*2/ld,j*2/ld,k*2/ld)<msym) cycle
      xt(i,j,k)=(xt2(j,k)+xt3(j,k))/two
   enddo;enddo
!enddo

deallocate(xt3)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  en_adi_y_lines(xt,xt2,ld,i,jb,je,k)
!
! Purpose:  Tridiagonal matrix solve of y direction lines in yz-plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt2   yz plane of xt array
!                 ld    1==celled centered array, 2==full array
!                 i     position x direction
!                 jb    beginning position y direction
!                 je    ending    position y direction
!                 k     position z direction
!
! Outputs
!   Arguments:    xt2
!
!======================================================================
subroutine en_adi_y_lines(xt,xt2,ld,i,jb,je,k)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
!common /adi1/ipl,iml,jbml,jepl,nim
dimension xt(mp,np,lp),xt2(np,lp),pj(np),qj(np)

kpl=k+ld
kml=k-ld
pj(jbml)=zero
qj(jbml)=xt(i,jbml,k)
do j=jb,je,ld
   jml=j-ld
   alp=ap(i,j,k)-pj(jml)*as(i,j,k,2,1)
   bsy=bs(i,j,k)
   if (abs(alp)<=small20 .or. &
      (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1 .and. &
       ibcell(i*2/ld,j*2/ld,k*2/ld)<msym)) then
      pj(j)=zero
      qj(j)=xt(i,j,k)
   else
      pj(j)=as(i,j,k,2,2)/alp
      bsy=bsy &
         +as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k) &
         +as(i,j,k,3,2)*xt(i,j,kpl)+as(i,j,k,3,1)*xt(i,j,kml)
      qj(j)=(bsy+as(i,j,k,2,1)*qj(jml))/alp
   endif
enddo
do j=je,jb,-ld
if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1 .and. &
       ibcell(i*2/ld,j*2/ld,k*2/ld)<msym) cycle
   xt2(j,k)=xt2(j+ld,k)*pj(j)+qj(j)
enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  en_adi_z_lines(xt,xt3,ld,i,j,kb,ke)
!
! Purpose:  Tridiagonal matrix solve of z direction lines in yz-plane
!
! Inputs
!   Arguments:    xt    function array to solve
!                 xt3   yz plane of xt array
!                 ld    1==celled centered array, 2==full array
!                 i     position x direction
!                 j     position y direction
!                 kb    beginning position y direction
!                 ke    ending    position y direction
!
! Outputs
!   Arguments:    xt3
!
!======================================================================
subroutine en_adi_z_lines(xt,xt3,ld,i,j,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
common /adi1/ipl,iml,jbml,jepl,nim,ibml,iepl,jml,jpl
!common /adi1/ipl,iml,jbml,jepl,nim
dimension xt(mp,np,lp),xt3(np,lp),pj(lp),qj(lp) 

jpl=j+ld
jml=j-ld
kbml=kb-ld
pj(kbml)=zero
qj(kbml)=xt(i,j,kbml)
do k=kb,ke,ld
   kml=k-ld
   alp=ap(i,j,k)-pj(kml)*as(i,j,k,3,1)
   bsy=bs(i,j,k)

   if (abs(alp)<=small20 .or. &
      (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1 .and. &
       ibcell(i*2/ld,j*2/ld,k*2/ld)<msym)) then
      pj(k)=zero
      qj(k)=xt(i,j,k)
   else
      pj(k)=as(i,j,k,3,2)/alp
      bsy=bsy &
         +as(i,j,k,1,2)*xt(ipl,j,k)+as(i,j,k,1,1)*xt(iml,j,k) &
         +as(i,j,k,2,2)*xt(i,jpl,k)+as(i,j,k,2,1)*xt(i,jml,k)
      qj(k)=(bsy+as(i,j,k,3,1)*qj(kml))/alp
   endif
enddo
do k=ke,kb,-ld
if (ibcell(i*2/ld,j*2/ld,k*2/ld)>=1 .and. &
       ibcell(i*2/ld,j*2/ld,k*2/ld)<msym) cycle
   xt3(j,k)=xt3(j,k+ld)*pj(k)+qj(k)
enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  real_inner_product(ara1,ara2)
!
! Purpose:  Sum product of corresponding elements in 2 arrays
!
! Inputs
!   Arguments:    ara1  first array
!                 ara2  second array 
!   Files read:      
!
! Outputs
!   Arguments: 
!   Files written: 
!
! Notes: 
!
!======================================================================
subroutine real_inner_product(ara1,ara2)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
dimension ara1(mp,np,lp),ara2(mp,np,lp) 

rip_sum=0

do k=2,mp
do j=2,np
do i=2,lp
   rip_sum=rip_sum + ara1(i,j,k)*ara2(i,j,k)
enddo;enddo;enddo

!call mpi_allreduce(rip_sum,rip_sum_all,1,mpi_double_precision,mpi_sum,comm,ierr)


! need following line in var_mod 
!      real*8 rip_sum_all ! real inner product sum over all processors

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  solver_test2
!
! Purpose:  routine to test the linear solvers
!
! Inputs
!   Arguments:  
!               
!   Files read:      
!
! Outputs
!   Arguments: 
!   Files written: 
!
! Notes: Not used
!
!======================================================================
subroutine solver_test2
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
real*8, allocatable :: ro(:,:,:)
real*8, allocatable :: rn(:,:,:)
real*8, allocatable :: v(:,:,:,:)
real*8, allocatable :: s(:,:,:,:)
real*8, allocatable :: sf(:,:,:)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
goto 11
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
kmax=8
allocate (ro(mz,nz,lz))
allocate (rn(mz,nz,lz))
allocate (v(mz,nz,lz,kmax))
allocate (s(mz,nz,lz,kmax))
allocate (sf(mp,np,lp))
s=zero
bs=0
goto 10
do kk=1,kmax
do j=1,nz
do k=1,lz
   s(1,j,k,kk)=one
   s(mz,j,k,kk)=one
enddo;enddo
do i=1,mz
do k=1,lz
   s(i,1,k,kk)=one
   s(i,nz,k,kk)=one
enddo;enddo
do j=1,nz
do i=1,mz
   s(i,j,1,kk)=one
   s(i,j,lz,kk)=one
enddo;enddo
enddo
10 continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
11 continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!adi stuff
as=zero
!call random_number(fz)
fz=0.0d0
do j=2,np,2
do k=2,lp,2
   fz(2,j,k)=one
   fz(mp,j,k)=one
   fz(6,j,k)=one
   fz(mp-2,j,k)=one
enddo;enddo
do i=2,mp,2
do k=2,lp,2
   fz(i,2,k)=one
   fz(i,np,k)=one
   fz(i,4,k)=one
   fz(i,np-2,k)=one
enddo;enddo
do j=2,np,2
do i=2,mp,2
   fz(i,j,2)=one
   fz(i,j,lp)=one
enddo;enddo
do k=2,lp,2
do j=2,np,2
do i=2,mp,2
   if(ibcell(i,j,k)>0) fz(i,j,k)=3.0d+0
enddo;enddo;enddo

p=two
!do i=8,mp-4,2
!   p(i,:,:)=i/2-3.0d0
!enddo


call random_number(as)

do n=1,2
do m=1,3
do k=1,lp
do j=1,np
do i=1,mp
   if (as(i,j,k,m,n)==zero) as(i,j,k,m,n)=one
enddo;enddo;enddo;enddo;enddo
goto 24
do k=4,lp-2,2
do j=4,np-2,2
do i=4,mp-2,2
   if (ibcell(i-2,j,k)>0) as(i,j,k,1,1)=zero
   if (ibcell(i+2,j,k)>0) as(i,j,k,1,2)=zero
   if (ibcell(i,j-2,k)>0) as(i,j,k,2,1)=zero
   if (ibcell(i,j+2,k)>0) as(i,j,k,2,2)=zero
   if (ibcell(i,j,k-2)>0) as(i,j,k,3,1)=zero
   if (ibcell(i,j,k+2)>0) as(i,j,k,3,2)=zero
enddo;enddo;enddo
24 continue

do i=4,mp-2,2
do j=4,npm2, 2
do k=4,lpm2, 2

   ap(i,j,k)=as(i,j,k,1,1)+as(i,j,k,1,2) &
            +as(i,j,k,2,1)+as(i,j,k,2,2) &
            +as(i,j,k,3,1)+as(i,j,k,3,2)
   bs(i,j,k)=ap(i,j,k)*p(i,j,k)   &
            -p(i-2,j,k)*as(i,j,k,1,1)-p(i+2,j,k)*as(i,j,k,1,2) &
            -p(i,j-2,k)*as(i,j,k,2,1)-p(i,j+2,k)*as(i,j,k,2,2) &
            -p(i,j,k-2)*as(i,j,k,3,1)-p(i,j,k+2)*as(i,j,k,3,2)
enddo;enddo;enddo
eqtyp=2 !gas phase
nelg=8
ntimesg(nelg)=2
!eqn=1
!end adi stuff

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
go to 12

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do mm=1,10
do k=4,lpm2,2;do j=4,npm2,2;do i=4,mp-2,2
   bs(i,j,k)=ap(i,j,k)*p(i,j,k)   &
            -p(i-2,j,k)*as(i,j,k,1,1)-p(i+2,j,k)*as(i,j,k,1,2) &
            -p(i,j-2,k)*as(i,j,k,2,1)-p(i,j+2,k)*as(i,j,k,2,2) &
            -p(i,j,k-2)*as(i,j,k,3,1)-p(i,j,k+2)*as(i,j,k,3,2)
   ro(i/2,j/2,k/2)=bs(i,j,k)-(ap(i,j,k)*fz(i,j,k)   &
            -fz(i-2,j,k)*as(i,j,k,1,1)-fz(i+2,j,k)*as(i,j,k,1,2) &
            -fz(i,j-2,k)*as(i,j,k,2,1)-fz(i,j+2,k)*as(i,j,k,2,2) &
            -fz(i,j,k-2)*as(i,j,k,3,1)-fz(i,j,k+2)*as(i,j,k,3,2))
   bs(i,j,k)=ro(i/2,j/2,k/2)
enddo;enddo;enddo
!eqtyp=1
!eqn=1
do kk=1,kmax
   sf=0
   !do k=2,lp,2;do j=2,np,2;do i=2,mp,2
      !sf(i,j,k)=s(i/2,j/2,k/2,kk)
   !enddo;enddo;enddo
!do k=4,lpm2,2;do j=4,npm2,2;do i=4,mp-2,2
!   bs(i,j,k)=ap(i,j,k)*p(i,j,k)   &
!            -p(i-2,j,k)*as(i,j,k,1,1)-p(i+2,j,k)*as(i,j,k,1,2) &
!            -p(i,j-2,k)*as(i,j,k,2,1)-p(i,j+2,k)*as(i,j,k,2,2) &
!            -p(i,j,k-2)*as(i,j,k,3,1)-p(i,j,k+2)*as(i,j,k,3,2)
!   bs(i,j,k)=bs(i,j,k)-(ap(i,j,k)*fz(i,j,k)   &
!            -fz(i-2,j,k)*as(i,j,k,1,1)-fz(i+2,j,k)*as(i,j,k,1,2) &
!            -fz(i,j-2,k)*as(i,j,k,2,1)-fz(i,j+2,k)*as(i,j,k,2,2) &
!            -fz(i,j,k-2)*as(i,j,k,3,1)-fz(i,j,k+2)*as(i,j,k,3,2))
!enddo;enddo;enddo
   call adi_solver(sf,4,mp-2,4,npm2,4,lpm2)
!   call adi_solver(s(1,1,1,kk),4,mp-2,4,npm2,4,lpm2)
!   call adi_solver(s(1,1,1,kk),2,mz-1,2,np-1,2,lp-1)
   do k=4,lpm2,2;do j=4,npm2,2;do i=4,mp-2,2
      s(i/2,j/2,k/2,kk)=sf(i,j,k)
   enddo;enddo;enddo
   
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      v(i,j,k,kk)=(ap(i*2,j*2,k*2)*s(i,j,k,kk)   &
            -s(i-1,j,k,kk)*as(i*2,j*2,k*2,1,1) &
            -s(i+1,j,k,kk)*as(i*2,j*2,k*2,1,2) &
            -s(i,j-1,k,kk)*as(i*2,j*2,k*2,2,1) &
            -s(i,j+1,k,kk)*as(i*2,j*2,k*2,2,2) &
            -s(i,j,k-1,kk)*as(i*2,j*2,k*2,3,1) &
            -s(i,j,k+1,kk)*as(i*2,j*2,k*2,3,2))
   enddo;enddo;enddo
   
   do ii=1,kk-1

      alpha=0 !alpha = (vkk,vii)
      do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
         alpha=alpha+v(i,j,k,kk)*v(i,j,k,ii)
      enddo;enddo;enddo

      !vkk = vkk- alpha vii
      do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
         v(i,j,k,kk)=v(i,j,k,kk)-alpha*v(i,j,k,ii)
      enddo;enddo;enddo

      !skk = skk- alpha sii
      do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
         s(i,j,k,kk)=s(i,j,k,kk)-alpha*s(i,j,k,ii)
      enddo;enddo;enddo
      
   enddo

   beta0=0 !beta0 = sqrt((vkk,vii))
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      beta0=beta0+v(i,j,k,kk)*v(i,j,k,kk)
   enddo;enddo;enddo
   beta0=sqrt(beta0)

   !vkk = vkk/beta0
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      v(i,j,k,kk)=v(i,j,k,kk)/beta0
   enddo;enddo;enddo

   !skk = skk/beta0
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      s(i,j,k,kk)=s(i,j,k,kk)/beta0
   enddo;enddo;enddo

   gamma=0 !gamma = (ro,vkk)
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      gamma=gamma+ro(i,j,k)*v(i,j,k,ii)
   enddo;enddo;enddo
   !gamma=1

   !fz = fz + gamma skk
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      fz(i*2,j*2,k*2)=fz(i*2,j*2,k*2)+gamma*s(i,j,k,ii)
   enddo;enddo;enddo

   !r = r + gamma vkk
   do k=2,lz-1;do j=2,nz-1;do i=2,mz-1
      bs(i*2,j*2,k*2)=bs(i*2,j*2,k*2)-gamma*v(i,j,k,ii)
   enddo;enddo;enddo
continue
enddo
enddo
stop

12 continue
do kk=1,1000
!   do ll=1,8
    call adi_solver(fz,4,mp-2,4,npm2,4,lpm2)
!     enddo
!   call gcr_solver(fz,4,mp-2,4,npm2,4,lpm2)
!   call gmres_solver(fz,4,mp-2,4,npm2,4,lpm2)
   xx=0
enddo

stop

end
!======================================================================
!======================================================================
!======================================================================
! Routine: Gauss-Seidel Solver
!
!
!======================================================================
subroutine gauss_seidel(xt,ib,ie,jb,je,kb,ke)
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)
dimension xt(mp,np,lp)

do i=ib,ie,2
do k=kb,ke,2
do j=jb,je,2
   xt(i,j,k)=(bs(i,j,k)   &
            +xt(i-2,j,k)*as(i,j,k,1,1)+xt(i+2,j,k)*as(i,j,k,1,2) &
            +xt(i,j-2,k)*as(i,j,k,2,1)+xt(i,j+2,k)*as(i,j,k,2,2) &
            +xt(i,j,k-2)*as(i,j,k,3,1)+xt(i,j,k+2)*as(i,j,k,3,2))/ap(i,j,k)

enddo;enddo;enddo

return
end





!======================================================================
!======================================================================
!======================================================================
! Routine:  solver_test
!
! Purpose:  routine to test the linear solvers
!
! Inputs
!   Arguments:  
!               
!   Files read:      
!
! Outputs
!   Arguments: 
!   Files written: 
!
! Notes: 
!
!======================================================================
subroutine solver_test
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)

real*8, allocatable :: bs0(:,:,:)
real*8, allocatable :: ap0(:,:,:)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!adi stuff
allocate(bs0(mp,np,lp))
allocate(ap0(mp,np,lp))
as=zero
!call random_number(fz)
fz=0.0d0 !fz is the initial quess
do j=2,np,2
do k=2,lp,2
   fz(2,j,k)=one
   fz(mp,j,k)=one
   fz(6,j,k)=one
   fz(mp-2,j,k)=one
enddo;enddo
do i=2,mp,2
do k=2,lp,2
   fz(i,2,k)=one
   fz(i,np,k)=one
   fz(i,4,k)=one
   fz(i,np-2,k)=one
enddo;enddo
do j=2,np,2
do i=2,mp,2
   fz(i,j,2)=one
   fz(i,j,lp)=one
enddo;enddo
do k=2,lp,2
do j=2,np,2
do i=2,mp,2
   if(ibcell(i,j,k)>0) fz(i,j,k)=15.0d+0
enddo;enddo;enddo

!p=2000.0d0 !p is the solution
do i=8,mp-4,2
   p(i,:,:)=i/2-3.0d0
enddo


call random_number(as)

do n=1,2
do m=1,3
do k=1,lp
do j=1,np
do i=1,mp
   if (as(i,j,k,m,n)==zero) as(i,j,k,m,n)=one
enddo;enddo;enddo;enddo;enddo

!zero coef. matrix beyond edges of domain
do k=2,lp
do j=2,np
  as(4,   j,k,1,1)=0
  as(mp-2,j,k,1,2)=0
enddo;enddo
do k=2,lp
do i=2,mp
  as(i,4,   k,1,1)=0
  as(i,np-2,k,1,2)=0
enddo;enddo
do j=2,np
do i=2,mp
  as(i,j,4,   1,1)=0
  as(i,j,lp-2,1,2)=0
enddo;enddo

!coef of unknown
goto 10
call random_number(ap)
do k=1,lp
do j=1,np
do i=1,mp
   ap(i,j,k)=ap(i,j,k)*1000
   if(ap(i,j,k)<0.1d0)ap(i,j,k)=one
enddo;enddo;enddo
10 continue

call random_number(ap)
!ap=0
do k=4,lp-2,2
do j=4,np-2,2
do i=4,mp-2,2
   ap(i,j,k)=ap(i,j,k)*10+as(i,j,k,1,1)+as(i,j,k,1,2) &
            +as(i,j,k,2,1)+as(i,j,k,2,2) &
            +as(i,j,k,3,1)+as(i,j,k,3,2)

enddo;enddo;enddo

goto 24
do k=4,lp-2,2
do j=4,np-2,2
do i=4,mp-2,2
   if (ibcell(i-2,j,k)>0) as(i,j,k,1,1)=zero
   if (ibcell(i+2,j,k)>0) as(i,j,k,1,2)=zero
   if (ibcell(i,j-2,k)>0) as(i,j,k,2,1)=zero
   if (ibcell(i,j+2,k)>0) as(i,j,k,2,2)=zero
   if (ibcell(i,j,k-2)>0) as(i,j,k,3,1)=zero
   if (ibcell(i,j,k+2)>0) as(i,j,k,3,2)=zero
enddo;enddo;enddo
24 continue

do i=4,mp-2,2
do j=4,np-2,2
do k=4,lp-2,2

   goto 25
   ap(i,j,k)=as(i,j,k,1,1)+as(i,j,k,1,2) &
            +as(i,j,k,2,1)+as(i,j,k,2,2) &
            +as(i,j,k,3,1)+as(i,j,k,3,2)
   25 continue
   bs0(i,j,k)=ap(i,j,k)*p(i,j,k)   &
            -p(i-2,j,k)*as(i,j,k,1,1)-p(i+2,j,k)*as(i,j,k,1,2) &
            -p(i,j-2,k)*as(i,j,k,2,1)-p(i,j+2,k)*as(i,j,k,2,2) &
            -p(i,j,k-2)*as(i,j,k,3,1)-p(i,j,k+2)*as(i,j,k,3,2)

enddo;enddo;enddo
eqtyp=2 !gas phase
nelg=8
ntimesg(nelg)=2
!eqn=1
!end adi stuff
!===================================================================
fz=1
rf0=0.70d0
rfc0=1-rf0
ap0=ap
do kk=1,1000

   do k=4,lp-2,2
   do j=4,np-2,2
   do i=4,mp-2,2
      ap(i,j,k)=ap0(i,j,k)/rf0
      bs(i,j,k)=bs0(i,j,k)+rfc0*fz(i,j,k)*ap(i,j,k)
   enddo;enddo;enddo
   !ap=ap0
   !bs=bs0

!   call gauss_seidel(fz,4,mp-2,4,np-2,4,lp-2)
!   call adi_solver(fz,4,mp-2,4,np-2,4,lp-2)
!   call gcr_solver(fz,4,mp-2,4,npm2,4,lpm2)
   call gmres_solver(fz,4,mp-2,4,npm2,4,lpm2)
   xx=0
enddo

stop

end
