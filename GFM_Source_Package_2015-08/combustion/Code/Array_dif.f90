!======================================================================
!  Array_dif.f90 - Array compare routines utility source file
!======================================================================
!  Array_dif provides debugging routines to compare array contents.
!
!  This file contains the following subroutines and/or functions:
!      array_dif_fstore(xt,ld,id)
!      array_dif_fstore_f(xt,ld,id)  
!      array_dif_fstore_c(xt,id)  
!      array_dif_fcompare(xt,ld,id)
!      array_dif_fcompare_f(xt,ld,id)
!      array_dif_fcompare_c(xt,id)
!      arrays_fstore_serial
!      arrays_fcompare_par
!      coef_cons_fstore_serial
!      coef_cons_fcompare_par 
!      coef_cons_fstore_serial_half 
!      coef_cons_fcompare_par_half 
!      arrays_fstore_serial_transpose_xy
!      transpose_xy(xt,xt_t)
!      transpose_xy2(xt,xt_t)
!      arrays_fstore_serial_transpose_xz
!      transpose_xz(xt,xt_t)
!      transpose_xz2(xt,xt_t)
!
!======================================================================
!======================================================================
!======================================================================
! Routine:  array_dif_fstore(xt,ld,id)  
!
! Purpose:  Called in serial mode to save an array for comparison 
!           to the same array in a parallel run
!
! Inputs
!   Arguments:    xt    function array to store
!                 ld    1==cell centered array, 2==full array
!                 id    file identifier
!
! Outputs
!   Files written:   55 'array_dif'//id//'.bin' 
!
! Notes: nxf=nx_gbl in serial
!
!======================================================================
subroutine array_dif_fstore(xt,ld,id)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
character*3 id
real*8 xt(0:nx_gbl*nyf*nzf-1)
integer ld

integer i,j,k,iu

iu=55
open(iu,file=casedir//'\array_dif'//id//'.bin',form='unformatted')

if(ld==2)then
   do k=0,nzf-1
   do j=0,nyf-1
   do i=0,nx_gbl-1

      if (i==15) then
         ijk=0
      endif

      write(iu) xt((k*nyf+j)*nx_gbl+i)
   enddo;enddo;enddo
else
   do k=0,nzc-1
   do j=0,nyc-1
   do i=0,nxc_gbl-1
      write(iu) xt((k*nyc+j)*nxc_gbl+i)
   enddo;enddo;enddo
endif

close(iu)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  array_dif_fstore_f(xt,ld,id)  
!
! Purpose:  Called in serial mode to save an array for comparison 
!           to the same array in a parallel run
!
! Inputs
!   Arguments:    xt    function array to store
!                 ld    1==cell centered array, 2==full array
!                 id    file identifier
!
! Outputs
!   Files written:   55 'array_dif'//id//'.bin' 
!
! Notes: nxf=nx_gbl in serial
!
!======================================================================
subroutine array_dif_fstore_f(xt,ld,id)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
character*3 id
real*8 xt(nx_gbl,nyf,nzf)
integer ld

integer i,j,k,iu

iu=55
open(iu,file=casedir//'\array_dif'//id//'.bin',form='unformatted')

if(ld==2)then
   do k=1,nzf
   do j=1,nyf
   do i=1,nx_gbl
      write(iu) xt(i,j,k)
   enddo;enddo;enddo
else
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc_gbl
      write(iu) xt(i,j,k)
   enddo;enddo;enddo
endif

close(iu)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  array_dif_fstore_c(xt,id)  
!
! Purpose:  Called in serial mode to save an array for comparison 
!           to the same array in a parallel run
!
! Inputs
!   Arguments:    xt    function array to store
!                 id    file identifier
!
! Outputs
!   Files written:   55 'array_dif'//id//'.bin' 
!
! Notes: nxf=nx_gbl in serial
!
!
!======================================================================
subroutine array_dif_fstore_c(xt,id)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
character*3 id
real*8 xt(nxc_gbl,nyc,nzc)

integer i,j,k

iu=55
open(iu,file=casedir//'\array_dif'//id//'.bin',form='unformatted')

do k=1,nzc
do j=1,nyc
do i=1,nxc_gbl
   write(iu) xt(i,j,k)
enddo;enddo;enddo

close(iu)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  array_dif_fcompare(xt,ld,id)
!
! Purpose:  Compares an array that has been gathered back to proc 0
!           to a version of that array that had been previously saved to file
!
! Inputs
!   Arguments:   xt  buffer that holds array that has been gathered
!                ld  1==cell centered array, 2==full array
!                id  3 character file identifier
!   Files read:  55  'array_dif'//id//'.bin'      
!
! Outputs
!   Files written: 57   'array_dif'//id//'.txt'  shows differences in array
!  
! Notes:  Called from processor 0 only
!
!======================================================================
subroutine array_dif_fcompare(xt,ld,id)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
character*3 id
real*8 xt(0:nx_gbl*nyf*nzf-1)
real*8 td
real*8 ratio,maxr,aver
integer ld
integer i,j,k
integer ndif
!logical exst

iu1=55
 inquire(file=casedir//'\array_dif'//id//'.bin',exist=exst)
if (exst) then
   open(iu1,file=casedir//'\array_dif'//id//'.bin',form='unformatted')
else
   write(ncon,'(a,a,a)') &
           ' abort: file array_dif',id,'.bin does not exist'
   pause
   call abort
endif

iu=57
open(iu,file=casedir//'\array_dif'//id//'.txt')

write(iu,*) ' i,  j,  k           file', &
            '                     array', &
            '                    ratio'
ndif=0
maxr=one
imax=0
jmax=0
kmax=0
aver=zero
if (ld==2)then
   do k=0,nzf-1
   do j=0,nyf-1
   do i=0,nx_gbl-1
      read(iu1) td
      if (xt((k*nyf+j)*nx_gbl+i).ne.td) then
         ratio=xt((k*nyf+j)*nx_gbl+i)/td
         if ((one-comp_tolerance)<ratio .and. ratio<(one+comp_tolerance)) cycle
         if (.not.isnan(ratio) .and. ratio>zero &
                .and. td.ne.zero) then
            if (abs(log(ratio))>abs(log(maxr))) then
               maxr=ratio
               imax=i
               jmax=j
               kmax=k
            endif
            aver=aver+abs(log(ratio))
         endif
         ndif=ndif+1
         write(iu,100) i+1,j+1,k+1,td,xt((k*nyf+j)*nx_gbl+i),ratio
      endif
   enddo;enddo;enddo
else
   do k=0,nzc-1
   do j=0,nyc-1
   do i=0,nxc_gbl-1
      read(iu1) td
      if (xt((k*nyc+j)*nxc_gbl+i).ne.td) then
         ratio=xt((k*nyc+j)*nxc_gbl+i)/td
         if ((one-comp_tolerance)<ratio.and.ratio<(one+comp_tolerance)) cycle
         if (.not.isnan(ratio) .and. ratio>zero.and.td.ne.zero) then
            if (abs(log(ratio))>abs(log(maxr))) then
               maxr=ratio
               imax=i
               jmax=j
               kmax=k
            endif
            aver=aver+abs(log(ratio))
         endif
         ndif=ndif+1
         write(iu,100) i+1,j+1,k+1,td,xt((k*nyc+j)*nxc_gbl+i),xt((k*nyc+j)*nxc_gbl+i)/td
      endif
   enddo;enddo;enddo
endif

if(ndif==0) then
   write(iu,*) 'exact match for ',id 
else
   aver=exp(aver/ndif)
   write(iu,*) 'dif summary for ',id 
   write(iu,150) ndif
   write(iu,160) maxr
   write(iu,170) aver
   write(iu,*) 'max at i,j,k:', imax,jmax,kmax
endif

close(iu)
close(iu1)

if(ndif==0) then
   print *, 'exact match for ',id 
else
   print *, 'dif summary for ',id 
   print 150, ndif
   print 160, maxr
   print 170, aver
endif

100   format(i3,','i3,',',i3,e25.16,e25.16,e25.16)
150   format(i5,' differences found')
160   format('worst ratio: ',e25.16)
170   format('average ratio: ',e25.16)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  array_dif_fcompare_f(xt,ld,id)
!
! Purpose:  Compares an array that has been gathered back to proc 0
!           to a version of that array that had been previously saved to file
!
! Inputs
!   Arguments:   xt  buffer that holds array that has been gathered
!                ld  1==cell centered array, 2==full array
!                id  3 character file identifier
!   Files read:  55  'array_dif'//id//'.bin'      
!
! Outputs
!   Files written: 57   'array_dif'//id//'.txt'  shows differences in array
!  
! Notes:  Called from processor 0 only
!
!======================================================================
subroutine array_dif_fcompare_f(xt,ld,id)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
character*3 id
real*8 xt(1:nx_gbl,1:nyf,1:nzf)
real*8 td
real*8 ratio,maxr,aver
integer ld
integer i,j,k
integer ndif
!logical exst

if (id=='ax-') write(6,'(a,e25.16)') 'at entry to array_dif_fcompare_f xt(2,5,1)=',xt(2,5,1)

iu1=55
 inquire(file=casedir//'\array_dif'//id//'.bin',exist=exst)
if (exst) then
   open(iu1,file=casedir//'\array_dif'//id//'.bin',form='unformatted')
else
   write(ncon,'(a,a,a)') &
           ' abort: file array_dif',id,'.bin does not exist'
   pause
   call abort
endif

iu=57
open(iu,file=casedir//'\array_dif'//id//'.txt')

write(iu,*) ' i,  j,  k           file', &
            '                     array', &
            '                    ratio'
ndif=0
maxr=one
imax=0
jmax=0
kmax=0
aver=zero
if (ld==2)then
   do k=1,nzf
   do j=1,nyf
   do i=1,nx_gbl
      read(iu1) td
      if (xt(i,j,k).ne.td) then
         ratio=xt(i,j,k)/td
         if ((one-comp_tolerance)<ratio .and. ratio<(one+comp_tolerance)) cycle
         if (.not.isnan(ratio) .and. ratio>zero &
                .and. td.ne.zero) then
            if (abs(log(ratio))>abs(log(maxr))) then
               maxr=ratio
               imax=i
               jmax=j
               kmax=k
            endif
            aver=aver+abs(log(ratio))
         endif
         ndif=ndif+1
         write(iu,100) i,j,k,td,xt(i,j,k),ratio
      endif
   enddo;enddo;enddo
else
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc_gbl
      read(iu1) td
      if (id=='ax-'.and.i==2.and.j==5.and.k==1) then
         write(6,'(a,e25.16)') 'in array_dif_fcompare_f read in as(2,5,1..)=',td
         write(6,'(a,e25.16)') 'xt(i,j,k)=',xt(i,j,k)
         write(6,'(a,e25.16)') 'zero',zero
         write(6,'(a,e25.16)') 'comp_tolerance=',comp_tolerance
         write(6,'(a,e25.16)') 'xt(i,j,k)/td',xt(i,j,k)/td
      endif
      if (xt(i,j,k).ne.td) then
         ratio=xt(i,j,k)/td
         if ((one-comp_tolerance)<ratio.and.ratio<(one+comp_tolerance)) cycle
         if (.not.isnan(ratio) .and. ratio>zero.and.td.ne.zero) then
            if (abs(log(ratio))>abs(log(maxr))) then
               maxr=ratio
               imax=i
               jmax=j
               kmax=k
            endif
            aver=aver+abs(log(ratio))
         endif
         ndif=ndif+1
         write(iu,100) i,j,k,td,xt(i,j,k),ratio
      endif
   enddo;enddo;enddo
endif

if(ndif==0) then
   write(iu,*) 'exact match for ',id 
else
   aver=exp(aver/ndif)
   write(iu,*) 'dif summary for ',id 
   write(iu,150) ndif
   write(iu,160) maxr
   write(iu,170) aver
   write(iu,*) 'max at i,j,k:', imax,jmax,kmax
endif

close(iu)
close(iu1)

if(ndif==0) then
   print *, 'exact match for ',id 
else
   print *, 'dif summary for ',id 
   print 150, ndif
   print 160, maxr
   print 170, aver
endif

100   format(i3,','i3,',',i3,e25.16,e25.16,e25.16)
150   format(i5,' differences found')
160   format('worst ratio: ',e25.16)
170   format('average ratio: ',e25.16)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  array_dif_fcompare_c(xt,id)
!
! Purpose:  Compares an array that has been gathered back to proc 0
!           to a version of that array that had been previously saved to file
!           Used for cell centered sized arrays
!
! Inputs
!   Arguments:   xt  buffer that holds array that has been gathered
!                id  3 character file identifier
!   Files read:  55  'array_dif'//id//'.bin'      
!
! Outputs
!   Files written: 57   'array_dif'//id//'.txt'  shows differences in array
!  
! Notes:  Called from processor 0 only
!
!======================================================================
subroutine array_dif_fcompare_c(xt,id)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
character*3 id
real*8 xt(1:nxc_gbl,1:nyc,1:nzc)
real*8 td
real*8 ratio,maxr,aver
integer i,j,k
integer ndif
!logical exst

iu1=55
 inquire(file=casedir//'\array_dif'//id//'.bin',exist=exst)
if (exst) then
   open(iu1,file=casedir//'\array_dif'//id//'.bin',form='unformatted')
else
   write(ncon,'(a,a,a)') &
           ' abort: file array_dif',id,'.bin does not exist'
   pause
   call abort
endif

iu=57
open(iu,file=casedir//'\array_dif'//id//'.txt')

write(iu,*) ' i,  j,  k           file', &
               '                     array', &
               '                    ratio'
ndif=0
maxr=one
imax=0
jmax=0
kmax=0
aver=zero
do k=1,nzc
do j=1,nyc
do i=1,nxc_gbl
   read(iu1) td
   if (xt(i,j,k).ne.td) then
      ratio=xt(i,j,k)/td
      if ((one-comp_tolerance)<ratio.and.ratio<(one+comp_tolerance)) cycle
      if (.not.isnan(ratio) .and. ratio>zero.and.td.ne.zero) then
         if (abs(log(ratio))>abs(log(maxr))) then
            maxr=ratio
            imax=i
            jmax=j
            kmax=k
         endif
         aver=aver+abs(log(ratio))
      endif
      ndif=ndif+1
      write(iu,100) i,j,k,td,xt(i,j,k),ratio
   endif
enddo;enddo;enddo

if(ndif==0) then
   write(iu,*) 'exact match for ',id 
else
   aver=exp(aver/ndif)
   write(iu,*) 'dif summary for ',id 
   write(iu,150) ndif
   write(iu,160) maxr
   write(iu,170) aver
   write(iu,*) 'max at i,j,k:', imax,jmax,kmax
endif

close(iu)
close(iu1)

if(ndif==0) then
   print *, 'exact match for ',id 
else
   print *, 'dif summary for ',id 
   print 150, ndif
   print 160, maxr
   print 170, aver
endif

100   format(i3,','i3,',',i3,e25.16,e25.16,e25.16)
150   format(i5,' differences found')
160   format('worst ratio: ',e25.16)
170   format('average ratio: ',e25.16)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  arrays_fstore_serial
!
! Purpose:  Called in serial mode to save most arrays for comparison 
!           to the same arrays in a parallel run
!
! Notes:  Comment out arrays that are not of interest
!         Uncomment arrays of interest
!         Make sure arrays of interest are allocated for your case
!
!
! gf array indexes:
!  iyf=1
!  iyo2=2
!  ih=3
!  ik=4
!  ieps=5
!  iyco2=6
!  iyn2=7
!  iyh2o=9
!
!======================================================================
subroutine arrays_fstore_serial
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

call array_dif_fstore_f(p,2,'_pg')
call array_dif_fstore_f(t,2,'_tg')
call array_dif_fstore_f(ug(1,1,1,1),2,'_ux')
call array_dif_fstore_f(ug(1,1,1,2),2,'_uy')
call array_dif_fstore_f(ug(1,1,1,3),2,'_uz')
call array_dif_fstore_f(gf(1,1,1,ih),2,'_ih')
call array_dif_fstore_f(gf(1,1,1,ik),2,'_ik')
call array_dif_fstore_f(gf(1,1,1,ieps),2,'eps')

if (iyf>0) call array_dif_fstore_f(gf(1,1,1,iyf),2,'_yf')
if (iyo2>0) call array_dif_fstore_f(gf(1,1,1,iyo2),2,'_o2')
!if (iyh2>0) call array_dif_fstore_f(gf(1,1,1,iyh2),2,'_h2')
if (iyn2>0) call array_dif_fstore_f(gf(1,1,1,iyn2),2,'_yn')
if (iyh2o>0) call array_dif_fstore_f(gf(1,1,1,iyh2o),2,'h2o')
if (iyco2>0) call array_dif_fstore_f(gf(1,1,1,iyco2),2,'co2')
!if (iyco>0) call array_dif_fstore_f(gf(1,1,1,iyco),2,'_co')

call array_dif_fstore_f(dnst,2,'rho')
call array_dif_fstore_f(theta,2,'gvf')
if(ivap==1)call array_dif_fstore_f(gcp,2,'gcp')
call array_dif_fstore_f(gama,2,'gam')
!if(ivap==1)call array_dif_fstore_f(kd_film,2,'kdf')
call array_dif_fstore_f(tmu,2,'tmu')
if (react) call array_dif_fstore_f(hrate,2,'hrt')
if(ivap==1)call array_dif_fstore_c(evp,'evp')
if(icond==1)call array_dif_fstore_c(con,'con')
!call array_dif_fstore_c(dgk,'dgk')

if(ndg>0)then
   call array_dif_fstore_c(th_d,'thd')
   call array_dif_fstore_f(gdiff,2,'gdf')
   call array_dif_fstore_c(dn(1,1,1,1),'dn1')
   call array_dif_fstore_c(dn(1,1,1,2),'dn2')
   call array_dif_fstore_c(dn(1,1,1,3),'dn3')
   call array_dif_fstore_c(du(1,1,1,1,1),'xu1')
   call array_dif_fstore_c(du(1,1,1,1,2),'yu1')
   call array_dif_fstore_c(du(1,1,1,1,3),'zu1')
   call array_dif_fstore_c(dt(1,1,1,1),'dt1')
   call array_dif_fstore_c(dt(1,1,1,2),'dt2')
   call array_dif_fstore_c(dt(1,1,1,3),'dt3')
endif

!if(npg>0)then
!   call array_dif_fstore_c(th_p,'thp')
!   call array_dif_fstore_c(pn(1,1,1,1),'pn1')
!  call array_dif_fstore_c(pn(1,1,1,2),'pn2')
!  call array_dif_fstore_c(pn(1,1,1,3),'pn3')
!  call array_dif_fstore_c(pu(1,1,1,1,1),'px1')
!  call array_dif_fstore_c(pu(1,1,1,1,2),'py1')
!  call array_dif_fstore_c(pu(1,1,1,1,3),'pz1')
!   call array_dif_fstore_c(pt(1,1,1,1),'pt1')
!  call array_dif_fstore_c(pt(1,1,1,2),'pt2')
!endif

if(steady==0)then
!   call array_dif_fstore_c(ugox(1,1,1),'uxo')
!   call array_dif_fstore_c(ugoy(1,1,1),'uyo')
!   call array_dif_fstore_c(ugoz(1,1,1),'uzo')
   call array_dif_fstore_c(gfo(1,1,1,ih),'iho')
   call array_dif_fstore_c(gfo(1,1,1,ik),'iko')
   call array_dif_fstore_c(gfo(1,1,1,ieps),'epo')
!   if (iyf>0)  call array_dif_fstore_c(gyo(1,1,1,iyf),'_yf')
!   if (iyo2>0) call array_dif_fstore_c(gyo(1,1,1,iyo2),'o2o')
  !if (iyh2>0) call array_dif_fstore_c(gyo(1,1,1,iyh2),'_h2')
!   if (iyn2>0)  call array_dif_fstore_c(gyo(1,1,1,iyn2),'yno')

  !if (iyh2o>0) call array_dif_fstore_c(gyo(1,1,1,iyh2o),'h2o')
  !if (iyco2>0) call array_dif_fstore_c(gyo(1,1,1,iyco2),'co2')
  !if (iyco>0) call array_dif_fstore_f(gyo(1,1,1,iyco),'_co')
   call array_dif_fstore_c(dnsto,'rh0')
   call array_dif_fstore_c(thetao,'vfo')
   if (npg>0)then
!      call array_dif_fstore_c(pno(1,1,1,1),'pno')
      !call array_dif_fstore_c(pno(1,1,1,2),'pn2')
      !call array_dif_fstore_c(pno(1,1,1,3),'pn3')
      !call array_dif_fstore_c(puo(1,1,1,1,1),'px1')
      !call array_dif_fstore_c(puo(1,1,1,1,2),'py1')
      !call array_dif_fstore_c(puo(1,1,1,1,3),'pz1')
!      call array_dif_fstore_c(pto(1,1,1,1),'pto')
      !call array_dif_fstore_c(pto(1,1,1,2),'pt2')
   endif    
endif     
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  arrays_fcompare_par
!
! Purpose:  Compares most arrays that have been gathered back to proc 0
!           to arrays that have been previously saved to file
!
! Notes:  Set local skip flags appropriately for arrays of interest
!         Make sure arrays of interest are allocated for your case
!         Make sure arrays of interest are saved in serial by fstore routine
!
!======================================================================
subroutine arrays_fcompare_par
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

print *, 'entered arrays_fcompare_par'

if (myid==0)then
   allocate (buf(nxc_gbl,nyc,nzc))
   allocate (buf2(nx_gbl,nyf,nzf))
else
   allocate (buf(nxc,nyc,nzc))
   allocate (buf2(nxf,nyf,nzf))
endif

print *, 'allocated buf, buf2'
print *, myid,' nxc_gbl ',nxc_gbl
! gas pressure
do k=1,nzf
!      if(myid==0)print *, 'packing pressure into buf2 k=',k
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=p(i,j,k)
enddo;enddo;enddo
!      print *, 'packed pressure into buf2'
if(myid==0) print *, 'gathering p'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_pg')

! gas temperature
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=t(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering t'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_tg')

! x gas velocity
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=ug(i,j,k,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ux'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_ux')

! y gas velocity
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=ug(i,j,k,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering uy'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_uy')

! z gas velocity
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=ug(i,j,k,3)
enddo;enddo;enddo
if(myid==0) print *, 'gathering uz'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_uz')

! gas enthalpy
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=gf(i,j,k,ih)
enddo;enddo;enddo
if(myid==0) print *, 'gathering h'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_ih')

!     turbulent kinetic energy
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=gf(i,j,k,ik)
enddo;enddo;enddo
if(myid==0) print *, 'gathering tke'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_ik')

!gas turbulent dissipation rate
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=gf(i,j,k,ieps)
enddo;enddo;enddo
if(myid==0) print *, 'gathering eps'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'eps')

if (iyf>0) then
   !  fuel mass fraction
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gf(i,j,k,iyf)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering yf'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'_yf')
endif

if (iyo2>0) then
   !  oxygen mass fraction
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gf(i,j,k,iyo2)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering yo2'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'_o2')
endif

!iskip=1 !******** skip flag **************
!if (iyh2>0.and.iskip==0) then
!   !  hydrogen mass fraction
!   do k=1,nzf
!   do j=1,nyf
!   do i=1,nxf
!      buf2(i,j,k)=gf(i,j,k,iyh2)
!   enddo;enddo;enddo
!   if(myid==0) print *, 'gathering yh2'
!   call gather_data(buf2,2)
!   if(myid==0) call array_dif_fcompare_f(buf2,2,'_h2')
!endif

if (iyn2>0) then
   !  inert mass fraction
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gf(i,j,k,iyn2)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering yn'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'_yn')
endif

if (iyh2o>0) then
   !  steam mass fraction
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gf(i,j,k,iyh2o)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering yh2o'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'h2o')
endif

iskip=1 !******** skip flag **************
if (iyco2>0.and.iskip==0) then
   !  co2 mass fraction
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gf(i,j,k,iyco2)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering yco2'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'co2')
endif

!iskip=1 !******** skip flag **************
!if (iyco>0.and.iskip==0) then
!   !  co mass fraction
!   do k=1,nzf
!   do j=1,nyf
!   do i=1,nxf
!      buf2(i,j,k)=gf(i,j,k,iyco)
!   enddo;enddo;enddo
!   if(myid==0) print *, 'gathering yco'
!   call gather_data(buf2,2)
!   if(myid==0) call array_dif_fcompare_f(buf2,2,'_co')
!endif

!gas density
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=dnst(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering dnst'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'rho')

!gas volume fraction
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=theta(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering theta'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'gvf')

if (ivap==1) then
!gas film specific heat for a vaporizing liquid
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gcp(i,j,k)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering gcp'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'gcp')
endif

!gas effective diffusivity
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=gama(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering gama'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'gam')

!if (ivap==1) then
!  kd_film droplet film thermal conductivity
!   do k=1,nzf
!   do j=1,nyf
!   do i=1,nxf
!      buf2(i,j,k)=kd_film(i,j,k)
!   enddo;enddo;enddo
!   if(myid==0) print *, 'gathering kd_film'
!   call gather_data(buf2,2)
!   if(myid==0) call array_dif_fcompare_f(buf2,2,'kdf')
!endif

!gas turbulent viscosity
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=tmu(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering tmu'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'tmu')

! heat release rate from reaction
if (react) then
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=hrate(i,j,k)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering hrt'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'hrt')
endif

if (ivap==1) then
   !droplet vaporization rate
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=evp(i,j,k)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering evp'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'evp')
endif

if (icond==1) then
   !condensation rate
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=con(i,j,k)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering con'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'con')
endif

!call array_dif_fcompare(dgk,1,'dgk')

if (ndg>0) then

   !droplet volume fraction
   !do k=1,nzc
   !do j=1,nyc
   !do i=1,nxc
   !   buf(i,j,k)=th_d(i,j,k)
   !enddo;enddo;enddo
   !if(myid==0) print *, 'gathering th_d'
   !call gather_data(buf,1)
   !if(myid==0) call array_dif_fcompare_c(buf,'thd')

   ! droplet turbulent diffusivity
   do k=1,nzf
   do j=1,nyf
   do i=1,nxf
      buf2(i,j,k)=gdiff(i,j,k)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering gdf'
   call gather_data(buf2,2)
   if(myid==0) call array_dif_fcompare_f(buf2,2,'gdf')

   !droplet number density size group 1
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=dn(i,j,k,1)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering dn1'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'dn1')

   !droplet number density size group 2
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=dn(i,j,k,2)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering dn2'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'dn2')

   !droplet number density size group 3
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=dn(i,j,k,3)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering dn3'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'dn3')

   !droplet x velocity size group 1
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=du(i,j,k,1,1)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering xu1'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'xu1')

   !droplet y velocity size group 1
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=du(i,j,k,1,2)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering yu1'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'yu1')

   !droplet z velocity size group 1
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=du(i,j,k,1,3)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering zu1'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'zu1')

   !droplet temperature size group 1
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=dt(i,j,k,1)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering dt1'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'dt1')

   !droplet temperature size group 2
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=dt(i,j,k,2)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering dt2'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'dt2')

   !droplet temperature size group 3
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
      buf(i,j,k)=dt(i,j,k,3)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering dt3'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'dt3')

endif
if (npg>0) then 

   !particle volume fraction
   do k=1,nzc
   do j=1,nyc
   do i=1,nxc
!      buf(i,j,k)=th_p(i,j,k)
   enddo;enddo;enddo
   if(myid==0) print *, 'gathering th_p'
   call gather_data(buf,1)
   if(myid==0) call array_dif_fcompare_c(buf,'thp')

   !particle number density size group 1
   !do k=1,nzc
   !do j=1,nyc
   !do i=1,nxc
   !   buf(i,j,k)=pn(i,j,k,1)
   !enddo;enddo;enddo
   !if(myid==0) print *, 'gathering pn1'
   !call gather_data(buf,1)
   !if(myid==0) call array_dif_fcompare_c(buf,'pn1')

   !convert below to parallel before use
   !call array_dif_fcompare_c(pn(1,1,1,2),'pn2')
   !call array_dif_fcompare_c(pn(1,1,1,3),'pn3')
   !call array_dif_fcompare_c(pu(1,1,1,1,1),'px1')
   !call array_dif_fcompare_c(pu(1,1,1,1,2),'py1')
   !call array_dif_fcompare_c(pu(1,1,1,1,3),'pz1')

   !particle temperature size group 1
   !do k=1,nzc
   !do j=1,nyc
   !do i=1,nxc
   !   buf(i,j,k)=pt(i,j,k,1)
   !enddo;enddo;enddo
   !if(myid==0) print *, 'gathering pt1'
   !call gather_data(buf,1)
   !if(myid==0) call array_dif_fcompare_c(buf,'pt1')

   !call array_dif_fcompare_c(pt(1,1,1,2),'pt2')
endif

if(steady==0)then 
   !convert below to parallel before use
   !call array_dif_fcompare_c(ugox(1,1,1),'uxo')
   !call array_dif_fcompare_c(ugoy(1,1,1),'uyo')
   !call array_dif_fcompare_c(ugoz(1,1,1),'uzo')
   !call array_dif_fcompare_c(gfo(1,1,1,ih),'iho')
   !call array_dif_fcompare_c(gfo(1,1,1,ik),'iko')
   !call array_dif_fcompare_c(gfo(1,1,1,ieps),'epo')
   !if (iyf>0) call array_dif_fcompare_c(gyo(1,1,1,iyf),'_yf')
   !if (iyo2>0) call array_dif_fcompare_c(gyo(1,1,1,iyo2),'o2o')
   !if (iyh2>0) then
   !call array_dif_fcompare_c(gyo(1,1,1,iyh2),'_h2')
   !endif
   !if (iyn2>0) then
   !   call array_dif_fcompare_c(gyo(1,1,1,iyn2),'yno')
   !endif
   !if (iyh2o>0) then
   !call array_dif_fcompare_c(gyo(1,1,1,iyh2o),'h2o')
   !endif
   !if (iyco2>0) then
   !call array_dif_fcompare_c(gyo(1,1,1,iyco2),'co2')
   !endif
   !if (iyco>0) then
   !call array_dif_fcompare_c(gyo(1,1,1,iyco),'_co')
   !endif 
   call array_dif_fcompare_c(dnsto,'rh0')
   call array_dif_fcompare_c(thetao,'vfo')
   if (npg>0) then
      !call array_dif_fcompare_c(pn(1,1,1,1),'pno')
      !call array_dif_fcompare_c(pn(1,1,1,2),1,'pn2')
      !call array_dif_fcompare_c(pn(1,1,1,3),1,'pn3')
      !call array_dif_fcompare_c(pu(1,1,1,1,1),1,'px1')
      !call array_dif_fcompare_c(pu(1,1,1,1,2),1,'py1')
      !call array_dif_fcompare_c(pu(1,1,1,1,3),1,'pz1')
      !call array_dif_fcompare_c(pt(1,1,1,1),'pto')
      !call array_dif_fcompare_c(pt(1,1,1,2),1,'pt2')
   endif
endif

deallocate(buf,buf2)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine: coef_cons_fstore_serial 
!
! Purpose: save A matrix and constant vector bs from serial run to file 
!          use for gas phase variables that fill up bs, ap, sfp, & as
!
!======================================================================
subroutine coef_cons_fstore_serial
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

call array_dif_fstore_f(bs,2,'_bs')
call array_dif_fstore_f(ap,2,'_ap')
call array_dif_fstore_f(sfp,2,'sfp')
call array_dif_fstore_f(as(1,1,1,1,1),2,'ax-')
call array_dif_fstore_f(as(1,1,1,1,2),2,'ax+')
call array_dif_fstore_f(as(1,1,1,2,1),2,'ay-')
call array_dif_fstore_f(as(1,1,1,2,2),2,'ay+')
call array_dif_fstore_f(as(1,1,1,3,1),2,'az-')
call array_dif_fstore_f(as(1,1,1,3,2),2,'az+')

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine: coef_cons_fcompare_par 
!
! Purpose: compare A matrix and constant vector bs 
!
! Notes: Call in parallel at same iteration as corresponding serial store
!        Use for gas phase variables with filled out bs, sfp, ap, & as
!        
!======================================================================
subroutine coef_cons_fcompare_par
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

print *, 'entered coef_cons_fcompare_par'

if (myid==0)then
   allocate (buf2(nx_gbl,nyf,nzf))
else
   allocate (buf2(nxf,nyf,nzf))
endif
print *, 'allocated buf2'

! bs source array
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=bs(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering bs'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_bs')

! ap source array
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=ap(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ap'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'_ap')

! sfp source array
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=sfp(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering sfp'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'sfp')

!  as_x- 
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=as(i,j,k,1,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ax-'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'ax-')

!  as_x+ 
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=as(i,j,k,1,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ax+'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'ax+')

!  as_y- 
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=as(i,j,k,2,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ay-'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'ay-')

!  as_y+ 
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=as(i,j,k,2,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ay+'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'ay+')

!  as_z- 
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=as(i,j,k,3,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering az-'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'az-')

!  as_z+ 
do k=1,nzf
do j=1,nyf
do i=1,nxf
   buf2(i,j,k)=as(i,j,k,3,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering az+'
call gather_data(buf2,2)
if(myid==0) call array_dif_fcompare_f(buf2,2,'az+')

deallocate(buf2)

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine: coef_cons_fstore_serial_half 
!
! Purpose: save A matrix and constant vector bs from serial run to file 
!          for condensed phase variables (droplet, particle)
!
!======================================================================
subroutine coef_cons_fstore_serial_half
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

call array_dif_fstore_f(bs,1,'_bs')
call array_dif_fstore_f(ap,1,'_ap')
call array_dif_fstore_f(sfp,1,'sfp')

   write(6,'(i2,a)') myid,' before store ax- id=2,5,1'
   write(6,'(i2,a,e11.4)') myid,' as=',as(2,5,1,1,1)

call array_dif_fstore_f(as(1,1,1,1,1),1,'ax-')
call array_dif_fstore_f(as(1,1,1,1,2),1,'ax+')
call array_dif_fstore_f(as(1,1,1,2,1),1,'ay-')
call array_dif_fstore_f(as(1,1,1,2,2),1,'ay+')
call array_dif_fstore_f(as(1,1,1,3,1),1,'az-')
call array_dif_fstore_f(as(1,1,1,3,2),1,'az+')

return
end

!======================================================================
!======================================================================
!======================================================================
! Routine: coef_cons_fcompare_par_half 
!
! Purpose: compare A matrix and constant vector bs
!          for condensed phase variables (droplet, particle)
!
! Notes: Call in parallel at same iteration as corresponding serial store
!
!======================================================================
subroutine coef_cons_fcompare_par_half
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

print *, 'entered coef_cons_fcompare_par_half'

if (myid==0)then
   allocate ( buf(nxc_gbl,nyc,nzc))
else
   allocate ( buf(nxc,nyc,nzc))
endif
print *, 'allocated buf'

! bs source array
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=bs(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering bs'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'_bs')

! ap source array
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=ap(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ap'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'_ap')

! sfp source array
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=sfp(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering sfp'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'sfp')

!  as_x- 
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=as(i,j,k,1,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ax-'

   write(6,'(i2,a)') myid,' before gather data ax-  id=2,5,1'
   write(6,'(i2,a,e11.4)') myid,' as=',as(2,5,1,1,1)

call gather_data(buf,1)

   write(6,'(i2,a)') myid,' after gather data ax-  id=2,5,1'
   write(6,'(i2,a,e11.4)') myid,' as=',as(2,5,1,1,1)


if(myid==0) call array_dif_fcompare_c(buf,'ax-')

!  as_x+ 
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=as(i,j,k,1,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ax+'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ax+')

!  as_y- 
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=as(i,j,k,2,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ay-'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ay-')

!  as_y+ 
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=as(i,j,k,2,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ay+'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ay+')

!  as_z- 
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=as(i,j,k,3,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering az-'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'az-')

!  as_z+ 
do k=1,nzc
do j=1,nyc
do i=1,nxc
   buf(i,j,k)=as(i,j,k,3,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering az+'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'az+')

deallocate(buf)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine: coef_cons_fstore_serial_half_c 
!
! Purpose: save A matrix and constant vector bs from serial run to file 
!          for condensed phase variables (droplet, particle)
!
!======================================================================
!subroutine coef_cons_fstore_serial_half_c
!use par_var
!use gbl_var
!implicit real*8 (a-h,o-z)

!call array_dif_fstore_c(bs_c,'_bs')
!call array_dif_fstore_c(ap_c,'_ap')
!call array_dif_fstore_c(sfp_c,'sfp')

!   write(6,'(i2,a)') myid,' before store ax- id=2,5,1'
!   write(6,'(i2,a,e11.4)') myid,' as=',as_c(2,5,1,1,1)

!call array_dif_fstore_c(as_c(1,1,1,1,1),'ax-')
!call array_dif_fstore_c(as_c(1,1,1,1,2),'ax+')
!call array_dif_fstore_c(as_c(1,1,1,2,1),'ay-')
!call array_dif_fstore_c(as_c(1,1,1,2,2),'ay+')
!call array_dif_fstore_c(as_c(1,1,1,3,1),'az-')
!call array_dif_fstore_c(as_c(1,1,1,3,2),'az+')

!return
!end

!======================================================================
!======================================================================
!======================================================================
! Routine: coef_cons_fcompare_par_half_c 
!
! Purpose: compare A matrix and constant vector bs
!          for condensed phase variables (droplet, particle)
!
! Notes: Call in parallel at same iteration as corresponding serial store
!
!======================================================================
subroutine coef_cons_fcompare_par_half_c
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

print *, 'entered coef_cons_fcompare_par_half_c'

if (myid==0)then
   allocate ( buf(nxc_gbl,nyc,nzc))
else
   allocate ( buf(nxc,nyc,nzc))
endif
print *, 'allocated buf'

! bs source array
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=bs_c(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering bs'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'_bs')

! ap source array
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=ap_c(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ap'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'_ap')

! sfp source array
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=sfp_c(i,j,k)
enddo;enddo;enddo
if(myid==0) print *, 'gathering sfp'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'sfp')

!  as_x- 
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=as_c(i,j,k,1,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ax-'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ax-')

!  as_x+ 
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=as_c(i,j,k,1,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ax+'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ax+')

!  as_y- 
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=as_c(i,j,k,2,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ay-'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ay-')

!  as_y+ 
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=as_c(i,j,k,2,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering ay+'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'ay+')

!  as_z- 
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=as_c(i,j,k,3,1)
enddo;enddo;enddo
if(myid==0) print *, 'gathering az-'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'az-')

!  as_z+ 
do k=1,nzc
do j=1,nyc
do i=1,nxc
!   buf(i,j,k)=as_c(i,j,k,3,2)
enddo;enddo;enddo
if(myid==0) print *, 'gathering az+'
call gather_data(buf,1)
if(myid==0) call array_dif_fcompare_c(buf,'az+')

deallocate(buf)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  arrays_fstore_serial_transpose_xy
!
! Purpose:  Called in serial mode to save xy transpose of arrays  
!           comparison to the same arrays in an xy transposed grid run
!
!======================================================================
subroutine arrays_fstore_serial_transpose_xy
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

allocate(buf(nxc_gbl,nyc,nzc))
allocate(buf2(nx_gbl,nyf,nzf))

call transpose_xy2(p,buf2)
call array_dif_fstore(buf2,2,'_pg')
call transpose_xy2(t,buf2)
call array_dif_fstore(buf2,2,'_tg')
call transpose_xy2(ug(1,1,1,1),buf2) !ux and uy change roles also
call array_dif_fstore(buf2,2,'_uy')
call transpose_xy2(ug(1,1,1,2),buf2) !ux and uy change roles also
call array_dif_fstore(buf2,2,'_ux')
call transpose_xy2(ug(1,1,1,3),buf2)
call array_dif_fstore(buf2,2,'_uz')
call transpose_xy2(gf(1,1,1,ih),buf2)
call array_dif_fstore(buf2,2,'_ih')
call transpose_xy2(gf(1,1,1,ik),buf2)
call array_dif_fstore(buf2,2,'_ik')
call transpose_xy2(gf(1,1,1,ieps),buf2)
call array_dif_fstore(buf2,2,'eps')

if (iyf>0) call transpose_xy2(gf(1,1,1,iyf),buf2)
if (iyf>0) call array_dif_fstore(buf2,2,'_yf')
if (iyo2>0) call transpose_xy2(gf(1,1,1,iyo2),buf2)
if (iyo2>0) call array_dif_fstore(buf2,2,'_o2')
!if (iyh2>0) call array_dif_fstore(gf(1,1,1,iyh2),2,'_h2')
if (iyn2>0) call transpose_xy2(gf(1,1,1,iyn2),buf2)
if (iyn2>0) call array_dif_fstore(buf2,2,'_yn')
if (iyh2o>0) call transpose_xy2(gf(1,1,1,iyh2o),buf2)
if (iyh2o>0) call array_dif_fstore(buf2,2,'h2o')
!if (iyco2>0) call array_dif_fstore(gf(1,1,1,iyco2),2,'co2')
!if (iyco>0) call array_dif_fstore(gf(1,1,1,iyco),2,'_co')
call transpose_xy2(dnst,buf2)
call array_dif_fstore(buf2,2,'rho')
call transpose_xy2(theta,buf2)
call array_dif_fstore(buf2,2,'gvf')
if(ivap==1)call transpose_xy2(gcp,buf2)
if(ivap==1)call array_dif_fstore(buf2,2,'gcp')
call transpose_xy2(gama,buf2)
call array_dif_fstore(buf2,2,'gam')
!if(ivap==1)call transpose_xy2(kd_film,buf2)
if(ivap==1)call array_dif_fstore(buf2,2,'kdf')
call transpose_xy2(tmu,buf2)
call array_dif_fstore(buf2,2,'tmu')
if(react)call transpose_xy2(hrate,buf2)
if(react)call array_dif_fstore(buf2,2,'hrt')
if (ivap==1) then
   call transpose_xy(evp,buf)
   call array_dif_fstore(buf,1,'evp')
   if (npg>0) then
      call transpose_xy(vap_p,buf)
      call array_dif_fstore(buf,1,'vpp')
   endif
endif
if(icond==1)call transpose_xy(con,buf)
if(icond==1)call array_dif_fstore(buf,1,'con')
!call array_dif_fstore(dgk,1,'dgk')
if(ndg>1)then
   call transpose_xy(th_d,buf)
   call array_dif_fstore(buf,1,'thd')
   call transpose_xy(gdiff,buf)
   call array_dif_fstore(gdiff,1,'gdf')
   call transpose_xy(dn(1,1,1,1),buf)
   call array_dif_fstore(buf,1,'dn1')
   call transpose_xy(dn(1,1,1,2),buf)
   call array_dif_fstore(buf,1,'dn2')
   call transpose_xy(dn(1,1,1,3),buf)
   call array_dif_fstore(buf,1,'dn3')
   call transpose_xy(du(1,1,1,1,1),buf)
   call array_dif_fstore(buf,1,'xu1')
   call transpose_xy(du(1,1,1,1,2),buf)
   call array_dif_fstore(buf,1,'yu1')
   call transpose_xy(du(1,1,1,1,3),buf)
   call array_dif_fstore(buf,1,'zu1')
   call transpose_xy(dt(1,1,1,1),buf)
   call array_dif_fstore(buf,1,'dt1')
   call transpose_xy(dt(1,1,1,2),buf)
   call array_dif_fstore(buf,1,'dt2')
endif
if(npg>0)then
!   call array_dif_fstore(th_p,1,'thp')
!   call array_dif_fstore(pn(1,1,1,1),1,'pn1')
!  call array_dif_fstore(pn(1,1,1,2),1,'pn2')
!  call array_dif_fstore(pn(1,1,1,3),1,'pn3')
!  call array_dif_fstore(pu(1,1,1,1,1),1,'px1')
!  call array_dif_fstore(pu(1,1,1,1,2),1,'py1')
!  call array_dif_fstore(pu(1,1,1,1,3),1,'pz1')
!   call array_dif_fstore(pt(1,1,1,1),1,'pt1')
!               call array_dif_fstore(pt(1,1,1,2),1,'pt2')
endif
if(steady==0)then
!   call array_dif_fstore(ugox(1,1,1),1,'uxo')
!   call array_dif_fstore(ugoy(1,1,1),1,'uyo')
!   call array_dif_fstore(ugoz(1,1,1),1,'uzo')
   call array_dif_fstore(gfo(1,1,1,ih),1,'iho')
   call array_dif_fstore(gfo(1,1,1,ik),1,'iko')
   call array_dif_fstore(gfo(1,1,1,ieps),1,'epo')
!   if (iyf>0)  call array_dif_fstore(gyo(1,1,1,iyf),1,'_yf')
!   if (iyo2>0) call array_dif_fstore(gyo(1,1,1,iyo2),1,'o2o')
  !if (iyh2>0) call array_dif_fstore(gyo(1,1,1,iyh2),1,'_h2')
!   if (iyn2>0)  call array_dif_fstore(gyo(1,1,1,iyn2),1,'yno')

  !if (iyh2o>0) then
  !call array_dif_fstore(gyo(1,1,1,iyh2o),1,'h2o')
  !endif
   !if (iyco2>0) then
      !call array_dif_fstore(gf(1,1,1,iyco2),2,'co2')
      !endif
   !if (iyco>0) then
      !call array_dif_fstore(gf(1,1,1,iyco),2,'_co')
   !endif 
   call array_dif_fstore(dnsto,1,'rh0')
   call array_dif_fstore(thetao,1,'vfo')
!   if (npg>0)then
!      call array_dif_fstore(pn(1,1,1,1),1,'pno')
      !call array_dif_fstore(pn(1,1,1,2),1,'pn2')
      !call array_dif_fstore(pn(1,1,1,3),1,'pn3')
      !call array_dif_fstore(pu(1,1,1,1,1),1,'px1')
      !call array_dif_fstore(pu(1,1,1,1,2),1,'py1')
      !call array_dif_fstore(pu(1,1,1,1,3),1,'pz1')
!      call array_dif_fstore(pt(1,1,1,1),1,'pto')
      !call array_dif_fstore(pt(1,1,1,2),1,'pt2')
!  endif    
endif     

deallocate(buf,buf2)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  transpose_xy(xt,ld,id)  
!
! Purpose:  transpose x and y positions in variable matrix
!           prepare for file compare to check direction independence
!
! Inputs
!   Arguments:    xt    function array 
!   Arguments:    xt_t  function array to store transpose
!
! Outputs: xt_t
!
! Notes: nxc=nxc_gbl in serial, must have nxc=nyc
!
!======================================================================
subroutine transpose_xy(xt,xt_t)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
real*8 xt(nxc_gbl,nyc,nzc),xt_t(nxc_gbl,nyc,nzc)

do k=1,nzc
do j=1,nyc
do i=1,nxc_gbl
   xt_t(i,j,k)=xt(j,i,k)
enddo;enddo;enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  transpose_xy2(xt,xt_t)  
!
! Purpose:  transpose x and y positions in double size variable matrix
!           prepare for file compare to check direction independence
!
! Inputs
!   Arguments:    xt    function array 
!   Arguments:    xt_t  function array to store transpose
!
! Outputs: xt_t
!
! Notes: nxf=nx_gbl in serial, must have nxf=nyf
!
!======================================================================
subroutine transpose_xy2(xt,xt_t)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
real*8 xt(nx_gbl,nyf,nzf),xt_t(nx_gbl,nyf,nzf)

do k=1,nzf
do j=1,nyf
do i=1,nx_gbl
   xt_t(i,j,k)=xt(j,i,k)
enddo;enddo;enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  arrays_fstore_serial_transpose_xz
!
! Purpose:  Called in serial mode to save xz transpose of arrays  
!           comparison to the same arrays in an xz transposed grid run
!
!======================================================================
subroutine arrays_fstore_serial_transpose_xz
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)

allocate(buf(nxc_gbl,nyc,nzc))
allocate(buf2(nx_gbl,nyf,nzf))

call transpose_xz2(p,buf2)
call array_dif_fstore(buf2,2,'_pg')
call transpose_xz2(t,buf2)
call array_dif_fstore(buf2,2,'_tg')
call transpose_xz2(ug(1,1,1,1),buf2) !ux and uz change roles also
call array_dif_fstore(buf2,2,'_uz')
call transpose_xz2(ug(1,1,1,2),buf2) 
call array_dif_fstore(buf2,2,'_uy')
call transpose_xz2(ug(1,1,1,3),buf2) !ux and uz change roles also
call array_dif_fstore(buf2,2,'_ux')
call transpose_xz2(gf(1,1,1,ih),buf2)
call array_dif_fstore(buf2,2,'_ih')
call transpose_xz2(gf(1,1,1,ik),buf2)
call array_dif_fstore(buf2,2,'_ik')
call transpose_xz2(gf(1,1,1,ieps),buf2)
call array_dif_fstore(buf2,2,'eps')

if (iyf>0) call transpose_xz2(gf(1,1,1,iyf),buf2)
if (iyf>0) call array_dif_fstore(buf2,2,'_yf')
if (iyo2>0) call transpose_xz2(gf(1,1,1,iyo2),buf2)
if (iyo2>0) call array_dif_fstore(buf2,2,'_o2')
!if (iyh2>0) call array_dif_fstore(gf(1,1,1,iyh2),2,'_h2')
if (iyn2>0) call transpose_xz2(gf(1,1,1,iyn2),buf2)
if (iyn2>0) call array_dif_fstore(buf2,2,'_yn')
if (iyh2o>0) call transpose_xz2(gf(1,1,1,iyh2o),buf2)
if (iyh2o>0) call array_dif_fstore(buf2,2,'h2o')
!if (iyco2>0) call array_dif_fstore(gf(1,1,1,iyco2),2,'co2')
!if (iyco>0) call array_dif_fstore(gf(1,1,1,iyco),2,'_co')
call transpose_xz2(dnst,buf2)
call array_dif_fstore(buf2,2,'rho')
call transpose_xz2(theta,buf2)
call array_dif_fstore(buf2,2,'gvf')
if(ivap==1)call transpose_xz2(gcp,buf2)
if(ivap==1)call array_dif_fstore(buf2,2,'gcp')
call transpose_xz2(gama,buf2)
call array_dif_fstore(buf2,2,'gam')
!if(ivap==1)call transpose_xz2(kd_film,buf2)
if(ivap==1)call array_dif_fstore(buf2,2,'kdf')
call transpose_xz2(tmu,buf2)
call array_dif_fstore(buf2,2,'tmu')
if(react)call transpose_xz2(hrate,buf2)
if(react)call array_dif_fstore(buf2,2,'hrt')
if (ivap==1) then
   call transpose_xz(evp,buf)
   call array_dif_fstore(buf,1,'evp')
   if (npg>0) then
      call transpose_xz(vap_p,buf)
      call array_dif_fstore(buf,1,'vpp')
   endif
endif
if(icond==1)call transpose_xz(con,buf)
if(icond==1)call array_dif_fstore(buf,1,'con')
!call array_dif_fstore(dgk,1,'dgk')
if(ndg>1)then
   call transpose_xz(th_d,buf)
   call array_dif_fstore(buf,1,'thd')
   call transpose_xz(gdiff,buf)
   call array_dif_fstore(gdiff,1,'gdf')
   call transpose_xz(dn(1,1,1,1),buf)
   call array_dif_fstore(buf,1,'dn1')
   call transpose_xz(dn(1,1,1,2),buf)
   call array_dif_fstore(buf,1,'dn2')
   call transpose_xz(dn(1,1,1,3),buf)
   call array_dif_fstore(buf,1,'dn3')
   call transpose_xz(du(1,1,1,1,1),buf)
   call array_dif_fstore(buf,1,'xu1')
   call transpose_xz(du(1,1,1,1,2),buf)
   call array_dif_fstore(buf,1,'yu1')
   call transpose_xz(du(1,1,1,1,3),buf)
   call array_dif_fstore(buf,1,'zu1')
   call transpose_xz(dt(1,1,1,1),buf)
   call array_dif_fstore(buf,1,'dt1')
   call transpose_xz(dt(1,1,1,2),buf)
   call array_dif_fstore(buf,1,'dt2')
endif
if(npg>0)then
!   call array_dif_fstore(th_p,1,'thp')
!   call array_dif_fstore(pn(1,1,1,1),1,'pn1')
!  call array_dif_fstore(pn(1,1,1,2),1,'pn2')
!  call array_dif_fstore(pn(1,1,1,3),1,'pn3')
!  call array_dif_fstore(pu(1,1,1,1,1),1,'px1')
!  call array_dif_fstore(pu(1,1,1,1,2),1,'py1')
!  call array_dif_fstore(pu(1,1,1,1,3),1,'pz1')
!   call array_dif_fstore(pt(1,1,1,1),1,'pt1')
!               call array_dif_fstore(pt(1,1,1,2),1,'pt2')
endif
if(.not.steady)then
!   call array_dif_fstore(ugox(1,1,1),1,'uxo')
!   call array_dif_fstore(ugoy(1,1,1),1,'uyo')
!   call array_dif_fstore(ugoz(1,1,1),1,'uzo')
   call array_dif_fstore(gfo(1,1,1,ih),1,'iho')
   call array_dif_fstore(gfo(1,1,1,ik),1,'iko')
   call array_dif_fstore(gfo(1,1,1,ieps),1,'epo')
!   if (iyf>0)  call array_dif_fstore(gyo(1,1,1,iyf),1,'_yf')
!   if (iyo2>0) call array_dif_fstore(gyo(1,1,1,iyo2),1,'o2o')
  !if (iyh2>0) call array_dif_fstore(gyo(1,1,1,iyh2),1,'_h2')
!   if (iyn2>0)  call array_dif_fstore(gyo(1,1,1,iyn2),1,'yno')

  !if (iyh2o>0) then
  !call array_dif_fstore(gyo(1,1,1,iyh2o),1,'h2o')
  !endif
   !if (iyco2>0) then
      !call array_dif_fstore(gf(1,1,1,iyco2),2,'co2')
      !endif
   !if (iyco>0) then
      !call array_dif_fstore(gf(1,1,1,iyco),2,'_co')
   !endif 
   call array_dif_fstore(dnsto,1,'rh0')
   call array_dif_fstore(thetao,1,'vfo')
   if (npg>0)then
!      call array_dif_fstore(pn(1,1,1,1),1,'pno')
      !call array_dif_fstore(pn(1,1,1,2),1,'pn2')
      !call array_dif_fstore(pn(1,1,1,3),1,'pn3')
      !call array_dif_fstore(pu(1,1,1,1,1),1,'px1')
      !call array_dif_fstore(pu(1,1,1,1,2),1,'py1')
      !call array_dif_fstore(pu(1,1,1,1,3),1,'pz1')
!      call array_dif_fstore(pt(1,1,1,1),1,'pto')
      !call array_dif_fstore(pt(1,1,1,2),1,'pt2')
   endif    
endif     

deallocate(buf,buf2)
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  transpose_xz(xt,xt_t)  
!
! Purpose:  transpose x and z positions in variable matrix
!           prepare for file compare to check direction independence
!
! Inputs
!   Arguments:    xt    function array 
!   Arguments:    xt_t  function array to store transpose
!
! Outputs: xt_t
!
! Notes: nxc=nxc_gbl in serial, must have nxc=nzc
!
!======================================================================
subroutine transpose_xz(xt,xt_t)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
real*8 xt(nxc_gbl,nyc,nzc),xt_t(nxc_gbl,nyc,nzc)

do k=1,nzc
do j=1,nyc
do i=1,nxc_gbl
   xt_t(i,j,k)=xt(k,j,i)
enddo;enddo;enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  transpose_xz2(xt,xt_t)  
!
! Purpose:  transpose x and z positions in double size variable matrix
!           prepare for file compare to check direction independence
!
! Inputs
!   Arguments:    xt    function array 
!   Arguments:    xt_t  function array to store transpose
!
! Outputs: xt_t
!
! Notes: nxf=nx_gbl in serial, must have nxf=nzf
!
!======================================================================
subroutine transpose_xz2(xt,xt_t)
!use par_var
use gbl_var
implicit real*8 (a-h,o-z)
real*8 xt(nx_gbl,nyf,nzf),xt_t(nx_gbl,nyf,nzf)

do k=1,nzf
do j=1,nyf
do i=1,nx_gbl
   xt_t(i,j,k)=xt(k,j,i)
enddo;enddo;enddo
return
end

!======================================================================
!======================================================================
!======================================================================
! Routine:  sample_debug_code
!
! Purpose:  Use array compares for parallel debugging
!
!======================================================================
subroutine sample_debug_code
use gbl_var
!use par_var
implicit real*8 (a-h,o-z)

!-------------------------------------------------
! Cut and paste block below at point to check in other routines
! Cell center arrays
!-------------------------------------------------
icompare=0 !local skip flag
if (icompare==1)then
   if (nprocs>1)then
!      call coef_cons_fcompare_par_half_c
   else
!      call coef_cons_fstore_serial_half_c
   endif 
   iabort_flag=1 !set to signal abort
!   call mpi_allreduce(iabort_flag,iflag_all,1,mpi_integer,mpi_sum,comm,ierr)
!   if (iflag_all>0) call abort
endif
!-------------------------------------------------

!-------------------------------------------------
! Cut and paste block below at point to check in other routines
! Full size arrays
!-------------------------------------------------
icompare=0
if (icompare==1)then
   if (nprocs>1)then
      call arrays_fcompare_par
   else
      call arrays_fstore_serial
   endif 
   iabort_flag=1 !set to signal abort
!   call mpi_allreduce(iabort_flag,iflag_all,1,mpi_integer,mpi_sum,comm,ierr)
!   if (iflag_all>0) call abort
   call abort
endif
!--------------------------------------------------------------------- 
!-------------------------------------------------
! Cut and paste block below at point to check in other routines
! Full size arrays
! Serial code
!-------------------------------------------------
icompare=0
if (icompare==1)then
   if (nprocs>1)then
      call arrays_fcompare_par
   else
      call arrays_fstore_serial
   endif 
   iabort_flag=1 !set to signal abort
   if (iabort_flag>0) call abort
endif
!--------------------------------------------------------------------- 

return
end


!======================================================================
!======================================================================
!======================================================================
! Routine:  gather_data(xt,ld)
!
! THIS ROUTINE IS FROM PARMFLOW AND IS HERE BECAUSE IT IS USED IN
! ARRAY_DIF. CALLS TO IT ARE TIED OFF--IT SIMPLY RETURNS.
!
! Purpose:  Collect xt array data from all processors to be saved from 
!           processor 0
!
! Inputs
!   Arguments:    xt   array being collected to processor 0 
!                 ld   1--> half size array, 2-->full size array   
!
! Notes: 
!     Example gathers: gather_data(p1,p2)
!
!     p1 = starting point for gather, usually an array name 
!          but could be a position within an array - see examples
!
!     p2 = 1 for cell centered array; dimensioned with nxc, nyc, nzc
!        = 2 for full size array;     dimensioned with nx, ny, nz
!
!     Simple examples:
!     
!  call gather_data(dnst,2)
!  call gather_data(evp,1)
!
!     Examples of gathering arrays with more than 3 dimensions:
!
!  do i=1,3
!     do j=1,2
!        call gather_data(as(1,1,1,i,j),2)
!     enddo
!  enddo
!
!  do i=1,ndg
!     do j=1,3
!        call gather_data(du(1,1,1,i,j),1)
!     enddo
!  enddo
!
!  do i=1,npg
!     call gather_data(pn(1,1,1,i),1)
!  enddo
!
!======================================================================
subroutine gather_data(xt,ld)
use gbl_var
!use par_var

real*8  xt(nx_gbl,nyF,nzF)

return
end
