!======================================================================
!  GL_Qual.f90 - source file
!======================================================================
!
!  Calculates post processing massless particle (seed) traces
!  to determine the following glass quality parameters
!  for the melt based on the CFD flow field.
!
!======================================================================
!======================================================================
!======================================================================

subroutine gl_qual
use gbl_var
implicit real*8 (a-h,o-z)
dimension iqual(50)

wall_delta = 0.03d+0 ! closest seeds can be to a wall
                     ! at wall glass vel = 0 so seed won't move
exit_delta = 0.01d+0 ! seeds within exit_delta of exit plane considered out

x_min=x(5); x_max=x(mp-3) ! initial guess for x wall positions
y_min=y(5); y_max=y(np-3) ! initial guess for y wall positions
z_min=Z(5); z_max=z(lp-3) ! initial guess for z wall & surface positions
num_seeds=0               ! number of seeds traced
num_stagnant = 0          ! number of seeds that end up in a stagnant zone 
stag_tol = 1.0d-9         ! normalized displacement tolerance for stagnant seed condition
num_dead_zone=0           ! number of seeds in a dead zone (recirculation or bouncing at stagnation point)
del_t=1.00d+1              ! initial time step
nx_traverses = 20         ! max # of x melt traverses at mean vel. allowed for non-stagnant seed
ux_mean = 1.0d-3          ! mean glass flow rate in melt (calc as ux_mean = sum_yz(mdot/(rho Ayz))

average_seed=zero
tot_seedtime=zero
seedmax=zero
seedmin=0.10d+5
num_out=0
is_max=2
js_max=2
ks_max=2
is_min=2
js_min=2
ks_min=2
xq_max=zero
yq_max=zero
xq_min=zero
yq_min=zero
iqual=zero

! Assuming the glass melt is in a rectangular tank (with some openings)
! find the wall positions.
!
ixskip=0; iyskip=0; izskip=0
do i=4,mp-2,2
do j=4,np-2,2
do k=4,lp-2,2     
   if (ibcell(i,j,k)==0.and.ibcell(i-2,j,k)==1.and.ixskip==0) then
     x_min=x(I-1)
     ixskip=1
   endif
   if (ibcell(i,j,k)==1.and.ibcell(i-2,j,k)==0) x_max=x(i-1) 
   if (ibcell(i,j,k)==0.and.ibcell(i,j-2,k)==1.and.iyskip==0) then
     y_min=y(J-1)
     iyskip=1
   endif
   if (ibcell(i,j,k)==1.and.ibcell(i,j-2,k)==0) y_max=y(j-1) 
   if (ibcell(i,j,k)==0.and.ibcell(i,j,k-2)==1.and.izskip==0) then
     z_min=z(K-1)
     izskip=1
   endif
   if (ibcell(i,j,k)==4.and.ibcell(i,j,k-2)==0) z_max=z(k-1) 
   enddo;enddo;enddo

   ! max number of time steps allowed before a seed is considered to be in a dead zone
   max_ts=nx_traverses*(x_max-x_min)/(ux_mean*del_t)

   igrp=1 ! size group for particle tracking

   !  sweep over surface layer, an x-y plane
   !  (i,j,k) is the cell center index set of the particle source cell
   k=lp-2
   do i=4,mp-2,2
   do j=4,np-2,2
      !needs @@@ to protect against ps or pc not allocated
      if (ibcell(i,j,k).ne.0.or.y(j)<y_min.or.y(j)>y_max) cycle
      if (ps(i/2,j/2,k/2,igrp)%dn==zero.and. &
             pc(i/2,j/2,k/2,igrp)%dn==zero) cycle 

      ! we have a cell with batch on top

      ! sweep over surface cell with batch 
      ! start and trace seeds at x intervals of x_inc
      !                     and  y intervals of y_inc

      x_inc = 0.05d+0   ! 20 cm - could be user input
      y_inc = 0.05d+0   ! 20 cm - could be user input

      ! don't launch particles from cell interfaces because some of them are walls
      ! and the glass velocity is zero there == the particle will not move

      z_src = z(k-1)     ! seed launch z position in cell (fixed)
      x_src = x(i-1) + x_inc   ! seed launch x position in cell (initial)
      if (x_src > x(i)) x_src=x(i)
      z_src_b = z(lp-3) ! z(bottom face of batch cells)

      do while (x_src < x(i+1))
         y_src = y(j-1) + y_inc  ! seed launch y position in cell (initial for column) 
         if (y_src > y(j)) y_src=y(j)
         do while (y_src < y(j+1) )

         num_seeds=num_seeds+1 ! set up to trace new seed

         is=i; js=j; ks=k ! (is,js,ks) index set of cell where seed is located   

         ! initialize (xs,ys,zs) = seed position vector
         xs = x_src; ys = y_src; zs = z_src 
         itime = 0
         ! solve o.d.e. for particle trace to far wall
         do while (itime < max_ts) 
            itime = itime +1
               call vel_at_point(xs,ys,zs,is,js,ks,ux_pa,uy_pa,uz_pa)
            ax_RK4 = del_t*ux_pa
            ay_RK4 = del_t*uy_pa
            az_RK4 = del_t*uz_pa

            xpa = max(min(xs + ax_RK4/2, x_max),x_min)
            ypa = max(min(ys + ay_RK4/2, y_max),y_min)
            zpa = max(min(zs + az_RK4/2, z_max),z_min)

            ip=is; jp=js; kp=ks
            do while (xpa < x(ip-1)); ip = ip-2; enddo  ! position west of cell bound
            do while (xpa > x(ip+1)); ip = ip+2; enddo  ! position east of cell bound
            do while (ypa < y(jp-1)); jp = jp-2; enddo  ! position south of cell bound
            do while (ypa > y(jp+1)); jp = jp+2; enddo  ! position north of cell bound
            do while (zpa < z(kp-1)); kp = kp-2; enddo  ! position below of cell bound
            do while (zpa > z(kp+1)); kp = kp+2; enddo  ! position above of cell bound

            call vel_at_point(xpa,ypa,zpa,ip,jp,kp,ux_pb,uy_pb,uz_pb)

            bx_RK4 = del_t*ux_pb
            by_RK4 = del_t*uy_pb
            bz_RK4 = del_t*uz_pb

            xp = max(min(xs + bx_RK4/2, x_max),x_min)
            yp = max(min(ys + by_RK4/2, y_max),y_min)
            zp = max(min(zs + bz_RK4/2, z_max),z_min)

            ip=is; jp=js; kp=ks
            do while (xp < x(ip-1)); ip = ip-2; enddo  ! position west of cell bound
            do while (xp > x(ip+1)); ip = ip+2; enddo  ! position east of cell bound
            do while (yp < y(jp-1)); jp = jp-2; enddo  ! position south of cell bound
            do while (yp > y(jp+1)); jp = jp+2; enddo  ! position north of cell bound
            do while (zp < z(kp-1)); kp = kp-2; enddo  ! position below of cell bound
            do while (zp > z(kp+1)); kp = kp+2; enddo  ! position above of cell bound

            call vel_at_point(xp,yp,zp,ip,jp,kp,ux_pc,uy_pc,uz_pc)

            cx_RK4 = del_t*ux_pc
            cy_RK4 = del_t*uy_pc
            cz_RK4 = del_t*uz_pc

            xp = max(min(xs + cx_RK4, x_max),x_min)
            yp = max(min(ys + cy_RK4, y_max),y_min)
            zp = max(min(zs + cz_RK4, z_max),z_min)

            ip=is; jp=js; kp=ks
            do while (xp < x(ip-1)); ip = ip-2; enddo  ! position west of cell bound
            do while (xp > x(ip+1)); ip = ip+2; enddo  ! position east of cell bound
            do while (yp < y(jp-1)); jp = jp-2; enddo  ! position south of cell bound
            do while (yp > y(jp+1)); jp = jp+2; enddo  ! position north of cell bound
            do while (zp < z(kp-1)); kp = kp-2; enddo  ! position below of cell bound
            do while (zp > z(kp+1)); kp = kp+2; enddo  ! position above of cell bound

            call vel_at_point(xp,yp,zp,ip,jp,kp,ux_pd,uy_pd,uz_pd)

            dx_RK4 = del_t*ux_pd
            dy_RK4 = del_t*uy_pd
            dz_RK4 = del_t*uz_pd

            xs_0 = xs; ys_0 = ys; zs_0 = zs ! save last time step position for stagnant seed check

            xs = xs + (ax_RK4 + 2*(bx_RK4 + cx_RK4) + dx_RK4)/6
            ys = ys + (ay_RK4 + 2*(by_RK4 + cy_RK4) + dy_RK4)/6
            zs = zs + (az_RK4 + 2*(bz_RK4 + cz_RK4) + dz_RK4)/6
            
            if (xs >= x_max-exit_delta) then ! particle has left system        

               ! save particle data
               seedtime = del_t*itime

               if (seedtime > seedmax) then
               seedmax=seedtime
               is_max = i; js_max = j; ks_max = k ! start cell for max res. time
               xq_max = x_src; yq_max = y_src ! start pos of seed with max res. time
               endif

               if (seedtime < seedmin) then
               seedmin=seedtime
               is_min = i; js_min = j; ks_min = k ! start cell for min res. time
               xq_min = x_src; yq_min = y_src ! start pos of seed with min res. time
               endif

               tot_seedtime = tot_seedtime + seedtime

               num_out = num_out +1
             
               !Build histogram array of seed residence times
               !hist_interval = time interval in minutes in array
     
               hist_interval = 5 ! minutes
               num_hist=21       ! intervals in histogram
               
               ihist = seedtime/(60 * hist_interval) + 1
               ihist = min(ihist, num_hist)
     
               iqual(ihist) = iqual(ihist) + 1
     
               exit !leave seed trace do loop
            endif

            !check position limits

            if (xs < x_min+wall_delta) xs = x_min + wall_delta 
            !if (xs > x_max) xs = x_max - wall_delta ! case already handled
            if (ys < y_min+wall_delta) ys = y_min + wall_delta 
            if (ys > y_max-wall_delta) ys = y_max - wall_delta 
            if (zs < z_min+wall_delta) zs = z_min + wall_delta 
            if (zs > z_max-wall_delta) zs = z_max - wall_delta 

            !check for stagnated seed

            if (abs((xs-xs_0)/xs) < stag_tol .and. &
                abs((ys-ys_0)/ys) < stag_tol .and. &
                abs((zs-zs_0)/zs) < stag_tol) then !seed is in a stagnant zone

                num_stagnant = num_stagnant +1
                print *,'Stagnant seed # =', num_stagnant,'x=',x_src,'y=',y_src

                exit ! done with this seed trace
            endif

            !check transition to new cell, account for skipped cells

            do while (xs < x(is-1)); is = is-2; enddo  !position west of cell bound
            do while (xs > x(is+1).and.is<mp-2); is = is+2; enddo  !position east of cell bound
            do while (ys < y(js-1)); js = js-2; enddo  !position south of cell bound
            do while (ys > y(js+1)); js = js+2; enddo  !position north of cell bound
            do while (zs < z(ks-1)); ks = ks-2; enddo  !position below of cell bound
            do while (zs > z(ks+1)); ks = ks+2; enddo  !position above of cell bound

         enddo !end of particle trace solver loop

         if (itime >= max_ts) then
         num_dead_zone = num_dead_zone + 1
         print *,'Dead zone seed # =', num_dead_zone,'x=',x_src,'y=',y_src
         endif
         y_src = y_src + y_inc
      enddo !end of seed source y position sweep in cell
      x_src = x_src + x_inc
   enddo !end of seed source x position sweep in cell

   !end seed generation sweep over surface cell        

enddo;enddo ! end sweep over surface layer looking for cells with batch


average_seed=tot_seedtime/num_out
filename=casedir//'\gql'//runum//'n.dat'
open(nu,file=filename)

write(nu,'("Minimum Residence Time:",3X,F11.3)') seedmin
write(nu,'("Located at node:",3X,I3,3X,I3,3X,I3)') is_min,js_min,ks_min
write(nu,'("Position:",3X,F11.3,3X,F11.3)') xq_min,yq_min
write(nu,'("Maximum Residence Time:",3X,F11.3)') seedmax
write(nu,'("Located at node:",3X,I3,3X,I3,3X,I3)') is_max,js_max,ks_max
write(nu,'("Position:",3X,F11.3,3X,F11.3)') xq_max,yq_max
write(nu,'("Average Residence Time:",3X,F11.3)') average_seed
write(nu,'(" ")')

do i=1,21
   write(nu,'(3X,I4,3X,I4)') i,iqual(I)
enddo

close(nu)
call stop_run("normal finish writing gql....n.dat.")

!@check to see that this is a clean termination
end
!======================================================================
!======================================================================
!======================================================================
! Subroutine vel_at_point
!
! Trilinear velocity interpolation from corner points of a rectilinear
! cell to a point in the interior
!
! Use 8 point interpolation to establish velocity at the seed location
! which is located within the current cell.
! Use basic formula:
! f(i) = [f(i-1)*(1-b)] + [f(i+1)*b]
! where b = [x(i)-x(i-1)] / [x(i+1)-x(i-1)]
! and 1-b = [x(i+1)-x(i)] / [x(i+1)-x(i-1)]
!======================================================================

subroutine vel_at_point(xp,yp,zp,ic,jc,kc,ux_p,uy_p,uz_p)
use gbl_var
implicit real*8 (A-H,O-Z)

! interpolation between bottom/top interface
if (kc > lp-4) then ! don't interpolate to z-plane in top layer
   ux_nw = lg(ic-1,jc+1,kc)%u(1)
   ux_sw = lg(ic-1,jc-1,kc)%u(1)
   ux_ne = lg(ic+1,jc+1,kc)%u(1)
   ux_se = lg(ic+1,jc-1,kc)%u(1)

   uy_nw = lg(ic-1,jc+1,kc)%u(2)
   uy_sw = lg(ic-1,jc-1,kc)%u(2)
   uy_ne = lg(ic+1,jc+1,kc)%u(2)
   uy_se = lg(ic+1,jc-1,kc)%u(2)

   uz_nw = lg(ic-1,jc+1,kc)%u(3)
   uz_sw = lg(ic-1,jc-1,kc)%u(3)
   uz_ne = lg(ic+1,jc+1,kc)%u(3)
   uz_se = lg(ic+1,jc-1,kc)%u(3)
else
   b_bt = (zp - z(kc-1))/(z(kc+1)-z(kc-1))

   ux_nw = lg(ic-1,jc+1,kc-1)%u(1)*(1-b_bt) + lg(ic-1,jc+1,kc+1)%u(1)*b_bt
   ux_sw = lg(ic-1,jc-1,kc-1)%u(1)*(1-b_bt) + lg(ic-1,jc-1,kc+1)%u(1)*b_bt
   ux_ne = lg(ic+1,jc+1,kc-1)%u(1)*(1-b_bt) + lg(ic+1,jc+1,kc+1)%u(1)*b_bt
   ux_se = lg(ic+1,jc-1,kc-1)%u(1)*(1-b_bt) + lg(ic+1,jc-1,kc+1)%u(1)*b_bt

   uy_nw = lg(ic-1,jc+1,kc-1)%u(2)*(1-b_bt) + lg(ic-1,jc+1,kc+1)%u(2)*b_bt
   uy_sw = lg(ic-1,jc-1,kc-1)%u(2)*(1-b_bt) + lg(ic-1,jc-1,kc+1)%u(2)*b_bt
   uy_ne = lg(ic+1,jc+1,kc-1)%u(2)*(1-b_bt) + lg(ic+1,jc+1,kc+1)%u(2)*b_bt
   uy_se = lg(ic+1,jc-1,kc-1)%u(2)*(1-b_bt) + lg(ic+1,jc-1,kc+1)%u(2)*b_bt

   uz_nw = lg(ic-1,jc+1,kc-1)%u(3)*(1-b_bt) + lg(ic-1,jc+1,kc+1)%u(3)*b_bt
   uz_sw = lg(ic-1,jc-1,kc-1)%u(3)*(1-b_bt) + lg(ic-1,jc-1,kc+1)%u(3)*b_bt
   uz_ne = lg(ic+1,jc+1,kc-1)%u(3)*(1-b_bt) + lg(ic+1,jc+1,kc+1)%u(3)*b_bt
   uz_se = lg(ic+1,jc-1,kc-1)%u(3)*(1-b_bt) + lg(ic+1,jc-1,kc+1)%u(3)*b_bt

endif

!interpolation between west/east interface

b_we = (xp - x(ic-1))/(x(ic+1)-x(ic-1))

ux_n = ux_nw*(1-b_we)+ux_ne*b_we
ux_s = ux_sw*(1-b_we)+ux_se*b_we

uy_n = uy_nw*(1-b_we)+uy_ne*b_we
uy_s = uy_sw*(1-b_we)+uy_se*b_we

uz_n = uz_nw*(1-b_we)+uz_ne*b_we
uz_s = uz_sw*(1-b_we)+uz_se*b_we

!interpolation between south/north interface

b_sn  = (yp - y(jc-1))/(y(jc+1)-y(jc-1))

ux_p = ux_s*(1-b_sn)+b_sn*ux_n
uy_p = uy_s*(1-b_sn)+b_sn*uy_n
uz_p = uz_s*(1-b_sn)+b_sn*uz_n
       
return
end
