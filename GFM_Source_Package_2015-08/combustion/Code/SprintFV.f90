!======================================================================
!======================================================================
!======================================================================
! SPRINTFV.F90
!     to print computed results of the system state for the combustion space
!     in FieldView postprocessor format
!        9-25-2006
!======================================================================
      subroutine sprintfv  
      use gbl_var
      implicit double precision (a-h,o-z)
      allocate(ixff(mp,np,lp))

776   format(1x,i4)
777   format(1x,f10.5)
778   format(1x,e14.6)

      ! Write out Fieldview grid nodes file
      filename=casedir//'\FV_nodes'//runum//'c.dat'
      open(nu_prtfv1,file=filename)
      do i=1,mp
      do j=1,np
      do k=1,lp
         if(ibcell(i,j,k).eq.0.or.ibcell(i,j,k).eq.2.or.ibcell(i,j,k).eq.3)then
            ixff(i,j,k)=1
         else
            ixff(i,j,k)=2
         endif
      enddo;enddo;enddo
      write(nu_prtfv1,776)mp-1
      write(nu_prtfv1,776)np-1
      write(nu_prtfv1,776)lp-1
      do kd5=3,lp-1,2
      do jd5=3,np-1,2
      do idi=3,mp-1,2
         write(nu_prtfv1,777)x(idi)
      enddo;enddo;enddo
      do kd5=3,lp-1,2
      do jdi=3,np-1,2
      do idi=3,mp-1,2
         write(nu_prtfv1,777)r(jdi)
      enddo;enddo;enddo
      do kdi=3,lp-1,2
      do jd5=3,np-1,2
      do id4=3,mp-1,2
         write(nu_prtfv1,777)z(kdi)
      enddo;enddo;enddo
      write(nu_prtfv1,776)(((ixff(i,j,k),i=1,mp-1,2),j=1,np-1,2),k=1,lp-1,2)
      close(nu_prtfv1)
      deallocate(ixff)

      ! Write out Fieldview variable values file
      filename=casedir//'\FV_values'//runum//'c.dat'
      numb=5
      open(nu_prtfv2,file=filename)
      write(nu_prtfv2,776)mp-1
      write(nu_prtfv2,776)np-1
      write(nu_prtfv2,776)lp-1
      write(nu_prtfv2,776)numb
      write(nu_prtfv2,777)(((T(i,j,k)*T0,i=3,mp-1,2),j=3,np-1,2),k=3,lp-1,2)
      write(nu_prtfv2,778)((((one+P(i,j,k))*pg0,i=3,mp-1,2),j=3,np-1,2),k=3,lp-1,2)
      write(nu_prtfv2,778)(((ug(i,j,k,1)*ug0,i=3,mp-1,2),j=3,np-1,2),k=3,lp-1,2)
      write(nu_prtfv2,778)(((ug(i,j,k,2)*ug0,i=3,mp-1,2),j=3,np-1,2),k=3,lp-1,2) 
      write(nu_prtfv2,778)(((ug(i,j,k,3)*ug0,i=3,mp-1,2),j=3,np-1,2),k=3,lp-1,2)
      close(nu_prtfv2)

      ! Write out Fieldview variable names file
      filename=casedir//'\FV_names'//runum//'c.nam'
      open(nu_prtfv3,file=filename)
      write(nu_prtfv3,*)'temperature (k)'
      write(nu_prtfv3,*)'pressure'
      write(nu_prtfv3,*)'u-velocity;velocity'
      write(nu_prtfv3,*)'v-velocity'
      write(nu_prtfv3,*)'w-velocity'
      close(nu_prtfv3)

      return
      end
