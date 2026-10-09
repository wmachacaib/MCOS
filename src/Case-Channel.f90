!Copyright (c) 2012-2022, Xcompact3d
!This file is part of Xcompact3d (xcompact3d.com)
!SPDX-License-Identifier: BSD 3-Clause

module channel

  use decomp_2d_constants
  use decomp_2d_mpi
  use decomp_2d
  use variables
  use param

  implicit none

  integer :: FS
  character(len=100) :: fileformat
  character(len=1),parameter :: NL=char(10) !new line character

  PRIVATE ! All functions/subroutines private by default
  PUBLIC :: init_channel, boundary_conditions_channel, postprocess_channel, &
            visu_channel, visu_channel_init, momentum_forcing_channel, &
            geomcomplex_channel

contains
  !############################################################################
  subroutine init_channel (ux1,uy1,uz1,ep1,phi1)

    use decomp_2d_io
    use variables
    use param
    use MPI
    use mhd, only : mhd_equation,Bm,Bmean

    implicit none

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: ux1,uy1,uz1,ep1
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: ux0,uy0,uz0
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: p2d_ux1,p2d_uy1,p2d_uz1
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: p3d_pbeta_ux1,p3d_pbeta_uy1,p3d_pbeta_uz1
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: p3d_mbeta_ux1,p3d_mbeta_uy1,p3d_mbeta_uz1    
    real(mytype),dimension(xsize(1),xsize(2),xsize(3),numscalar) :: phi1,phi10,p2d_phi1,p3d_pbeta_phi1,p3d_mbeta_phi1

    real(mytype) :: y,r,um,r3,x,z,h,ct
    real(mytype) :: cx0,cy0,cz0,hg,lg
    real(mytype) :: ftent
    real(mytype) :: ra
    integer :: k,j,i,fh,ierror,ii,is,it,code, jj

    if (idir_stream /= 1 .and. idir_stream /= 3) then
       if (nrank == 0) then
          write(*,*) '!! ERROR in imposing sorce term for momentum !!'
          write(*,*) '!! idir_stream ', idir_stream
          write(*,*) '!! idir_stream has to be:'
          write(*,*) '!! - 1 for streamwise direction in X'
          write(*,*) '!! - 3 for streamwise direction in Z'
          write(*,*) '!! Y is not supported and other values do not make sense'
          write(*,*) '!! Calculation will be now stop'
        endif
        call MPI_ABORT(MPI_COMM_WORLD,code,ierror); stop
    endif

    phi1(:,:,:,:) = zero

		ra = ri(1) * re*re * sc(1)

    ux1=zero
    uy1=zero
    uz1=zero
    
    call base_flow(ux0,uy0,uz0,ra) ! for Uc = Ub and Tc = RemPrAh adimensionalization.
    
    call perturbation(p2d_ux1,p2d_uy1,p2d_uz1,alpha_ci,zero,amp_2d,urread_2d,uiread_2d,vrread_2d,viread_2d,wrread_2d,wiread_2d)
    
    call perturbation(p3d_pbeta_ux1, p3d_pbeta_uy1, p3d_pbeta_uz1, alpha_ci, beta_ci, amp_3d, urread_3d_pbeta, uiread_3d_pbeta, &
    								  vrread_3d_pbeta, viread_3d_pbeta, wrread_3d_pbeta, wiread_3d_pbeta)
    								  
    call perturbation(p3d_mbeta_ux1, p3d_mbeta_uy1, p3d_mbeta_uz1, alpha_ci, (-1) * beta_ci, amp_3d, urread_3d_mbeta, uiread_3d_mbeta, &
    								  vrread_3d_mbeta, viread_3d_mbeta, wrread_3d_mbeta, wiread_3d_mbeta)    								  

!####################################################################################################################    
    ! NOTA:
    ! MODIFICACIÓN 06/03/25. 
    ! La multiplicación por 2/3 de las componentes de la velocidad y la multiplicación por (-2/3 Re_o Pr) está 
    ! asociado a que tanto los flujos base como las perturbaciones están calculadas a partir de la ecuaciones
    ! que resultan de la forma adimensional de las ecuaciones de conservación de masa, momento y energía del paper 
    ! de Chen 1996. Pablo Szuban utiliza la misma adimensionalización, la cual es diferente a la adimensionalización 
    ! que yo utilizo y que está configurado en esta versión del codigo de XC3D. 
    ! OJO: take into account that the perturbations are normalizad with 1, so maybe they shouldn't be multiplied by 2/3. I will check this later.
    ! Yes do not multiply by 2/3 and RePr                             
!####################################################################################################################       
    
    ux1 = (ux0) * (two/three)  + p2d_ux1 + 0.5 * (p3d_pbeta_ux1 + p3d_mbeta_ux1) 
    uy1 = (uy0) * (two/three)  + p2d_uy1 + 0.5 * (p3d_pbeta_uy1 + p3d_mbeta_uy1)
    uz1 = (uz0) * (two/three)  + p2d_uz1 + 0.5 * (p3d_pbeta_uz1 + p3d_mbeta_uz1)

	 if (iscalar==1) then
			
			call base_flow_scalar(phi10,ra) ! for Uc = Ub and Tc = RemPrAh adimensionalization.

    	call perturbation_scalar(p2d_phi1,alpha_ci,zero,amp_2d,phirread_2d,phiiread_2d)
    	call perturbation_scalar(p3d_pbeta_phi1,alpha_ci,beta_ci,amp_3d,phirread_3d_pbeta,phiiread_3d_pbeta)
    	call perturbation_scalar(p3d_mbeta_phi1,alpha_ci,(-1) * beta_ci,amp_3d,phirread_3d_mbeta,phiiread_3d_mbeta) 

    	phi1 = (phi10) * ( - (two/three) * re * sc(1) ) + p2d_phi1 + 0.5 * (p3d_pbeta_phi1 + p3d_mbeta_phi1)

    end if 
    
    
    return    
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  end subroutine init_channel
  !############################################################################
  !############################################################################
  subroutine boundary_conditions_channel (ux,uy,uz,phi)

    use param
    use var, only : di2
    use variables
    use mhd, only : Bm, mhd_equation 

    implicit none

    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: ux,uy,uz
    real(mytype),dimension(xsize(1),xsize(2),xsize(3),numscalar) :: phi

    if (.not. cpg ) then ! if not constant pressure gradient
       if (idir_stream == 1) then
          call channel_cfr(ux,two/three)
       else
          call channel_cfr(uz,two/three)
       endif
    end if

    if (iscalar /= 0) then
       if (iimplicit <= 0) then
          if ((nclyS1 == 2).and.(xstart(2) == 1)) then
             !! Generate a hot patch on bottom boundary
         ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
            ! phi(:,1,:,:) = one
             phi(:,1,:,:) = zero
         ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
          endif
          if ((nclySn == 2).and.(xend(2) == ny)) THEN
             phi(:,xsize(2),:,:) = zero
          endif
       else
          !
          ! Implicit boundary conditions are usually given in input file
          ! It is possible to modify g_sc here
          ! It is not possible to modify alpha_sc and beta_sc here
          !
          ! Bottom temperature if alpha_sc(:,1)=1 and beta_sc(:,1)=0 (default)
          !if (nclyS1.eq.2) g_sc(:,1) = one
          ! Top temperature if alpha_sc(:,2)=1 and beta_sc(:,2)=0 (default)
          !if (nclySn.eq.2) g_sc(:,2) = zero
       endif
    endif

    if( mhd_active .and. iimplicit<=0 .and. mhd_equation=='induction' ) then
       ! FIXME add a test
       ! This is valid only when nclyB*1 = 2
       if (xstart(2) == 1) then
          Bm(:,1,:,1)  = zero
          Bm(:,1,:,2)  = zero
          Bm(:,1,:,3)  = zero
       endif
       ! FIXME add a test
       ! This is valid only when nclyB*n = 2
       if (xend(2) == ny) then
          Bm(:,xsize(2),:,1) = zero
          Bm(:,xsize(2),:,2) = zero
          Bm(:,xsize(2),:,3) = zero
       endif
    endif

  end subroutine boundary_conditions_channel
  !############################################################################
  !!
  !!  SUBROUTINE: channel_cfr
  !!      AUTHOR: Kay Schäfer
  !! DESCRIPTION: Inforces constant flow rate without need of data transposition
  !!
  !############################################################################
  subroutine channel_cfr (ux, constant)

    use MPI

    implicit none

    real(mytype), dimension(xsize(1),xsize(2),xsize(3)) :: ux
    real(mytype), intent(in) :: constant

    integer :: code, i, j, k, jloc
    real(mytype) :: can, ub, coeff

    ub = zero
    coeff = dy / (yly * real(xsize(1) * zsize(3), kind=mytype))

    do k = 1, xsize(3)
       do jloc = 1, xsize(2)
          j = jloc + xstart(2) - 1
          do i = 1, xsize(1)
            ub = ub + ux(i,jloc,k) / ppy(j)
          enddo
       enddo
    enddo

    ub = ub * coeff

    call MPI_ALLREDUCE(MPI_IN_PLACE,ub,1,real_type,MPI_SUM,MPI_COMM_WORLD,code)

    can = - (constant - ub)

    if (nrank==0.and.(mod(itime, ilist) == 0 .or. itime == ifirst .or. itime == ilast)) &
       write(*,*) 'UT', ub, can

    do k=1,xsize(3)
      do j=1,xsize(2)
        do i=1,xsize(1)
          ux(i,j,k) = ux(i,j,k) - can
        enddo
      enddo
    enddo

  end subroutine channel_cfr

  !############################################################################
  !############################################################################
  subroutine postprocess_channel(ux1,uy1,uz1,pp3,phi1,ep1)

    use var, ONLY : nzmsize,xnu
    USE variables, only: nx,ny,nz

    use MPI

    implicit none

    real(mytype), intent(in), dimension(xsize(1),xsize(2),xsize(3)) :: ux1, uy1, uz1, ep1
    real(mytype), intent(in), dimension(xsize(1),xsize(2),xsize(3),numscalar) :: phi1
    real(mytype), intent(in), dimension(ph1%zst(1):ph1%zen(1),ph1%zst(2):ph1%zen(2),nzmsize,npress) :: pp3

  end subroutine postprocess_channel

  subroutine visu_channel_init(visu_initialised)

    use decomp_2d_io, only : decomp_2d_register_variable
    use visu, only : io_name, output2D
    use param, only : mhd_active
    
    implicit none

    logical, intent(out) :: visu_initialised

    call decomp_2d_register_variable(io_name, "critq", 1, 0, output2D, mytype)

    if (mhd_active) then
       call decomp_2d_register_variable(io_name, "J_x", 1, 0, output2D, mytype)
       call decomp_2d_register_variable(io_name, "J_y", 1, 0, output2D, mytype)
       call decomp_2d_register_variable(io_name, "J_z", 1, 0, output2D, mytype)
       call decomp_2d_register_variable(io_name, "B_x", 1, 0, output2D, mytype)
       call decomp_2d_register_variable(io_name, "B_y", 1, 0, output2D, mytype)
       call decomp_2d_register_variable(io_name, "B_z", 1, 0, output2D, mytype)
    endif

    visu_initialised = .true.
    
  end subroutine visu_channel_init
  !############################################################################
  !!
  !!  SUBROUTINE: visu_channel
  !!      AUTHOR: FS
  !! DESCRIPTION: Performs channel-specific visualization
  !!
  !############################################################################
  subroutine visu_channel(ux1, uy1, uz1, pp3, phi1, ep1, num)

    use var, only : ux2, uy2, uz2, ux3, uy3, uz3
    use var, only : ta1,tb1,tc1,td1,te1,tf1,tg1,th1,ti1,di1
    use var, only : ta2,tb2,tc2,td2,te2,tf2,di2,ta3,tb3,tc3,td3,te3,tf3,di3
    use var, ONLY : nzmsize
    use visu, only : write_field
    use param, only : mhd_active
    use mhd, only : mhd_equation,Je, Bm
    
    use ibm_param, only : ubcx,ubcy,ubcz

    implicit none

    real(mytype), intent(in), dimension(xsize(1),xsize(2),xsize(3)) :: ux1, uy1, uz1
    real(mytype), intent(in), dimension(ph1%zst(1):ph1%zen(1),ph1%zst(2):ph1%zen(2),nzmsize,npress) :: pp3
    real(mytype), intent(in), dimension(xsize(1),xsize(2),xsize(3),numscalar) :: phi1
    real(mytype), intent(in), dimension(xsize(1),xsize(2),xsize(3)) :: ep1
    integer, intent(in) :: num

    ! Write vorticity as an example of post processing

    ! Perform communications if needed
    if (sync_vel_needed) then
      call transpose_x_to_y(ux1,ux2)
      call transpose_x_to_y(uy1,uy2)
      call transpose_x_to_y(uz1,uz2)
      call transpose_y_to_z(ux2,ux3)
      call transpose_y_to_z(uy2,uy3)
      call transpose_y_to_z(uz2,uz3)
      sync_vel_needed = .false.
    endif

    !x-derivatives
    call derx (ta1,ux1,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,ubcx)
    call derx (tb1,uy1,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,ubcy)
    call derx (tc1,uz1,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,ubcz)
    !y-derivatives
    call dery (ta2,ux2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,ubcx)
    call dery (tb2,uy2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,ubcy)
    call dery (tc2,uz2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,ubcz)
    !!z-derivatives
    call derz (ta3,ux3,di3,sz,ffzp,fszp,fwzp,zsize(1),zsize(2),zsize(3),1,ubcx)
    call derz (tb3,uy3,di3,sz,ffzp,fszp,fwzp,zsize(1),zsize(2),zsize(3),1,ubcy)
    call derz (tc3,uz3,di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,ubcz)
    !!all back to x-pencils
    call transpose_z_to_y(ta3,td2)
    call transpose_z_to_y(tb3,te2)
    call transpose_z_to_y(tc3,tf2)
    call transpose_y_to_x(td2,tg1)
    call transpose_y_to_x(te2,th1)
    call transpose_y_to_x(tf2,ti1)
    call transpose_y_to_x(ta2,td1)
    call transpose_y_to_x(tb2,te1)
    call transpose_y_to_x(tc2,tf1)
    !du/dx=ta1 du/dy=td1 and du/dz=tg1
    !dv/dx=tb1 dv/dy=te1 and dv/dz=th1
    !dw/dx=tc1 dw/dy=tf1 and dw/dz=ti1

    !Q=-0.5*(ta1**2+te1**2+di1**2)-td1*tb1-tg1*tc1-th1*tf1
    di1 = zero
    di1(:,:,:) = - half*(ta1(:,:,:)**2 + te1(:,:,:)**2 + ti1(:,:,:)**2) &
                 - td1(:,:,:) * tb1(:,:,:) &
                 - tg1(:,:,:) * tc1(:,:,:) &
                 - th1(:,:,:) * tf1(:,:,:)
    call write_field(di1, ".", "critq", num, flush = .true.) ! Reusing temporary array, force flush

    if (mhd_active .and. mhd_equation=='induction') then
      call write_field(Je(:,:,:,1), ".", "J_x", num, flush = .true.)
      call write_field(Je(:,:,:,2), ".", "J_y", num, flush = .true.)
      call write_field(Je(:,:,:,3), ".", "J_z", num, flush = .true.)
      call write_field(Bm(:,:,:,1), ".", "B_x", num, flush = .true.)
      call write_field(Bm(:,:,:,2), ".", "B_y", num, flush = .true.)
      call write_field(Bm(:,:,:,3), ".", "B_z", num, flush = .true.)
    endif
    
  end subroutine visu_channel
  !############################################################################
  !############################################################################
  !!
  !!  SUBROUTINE: momentum_forcing
  !!      AUTHOR: Paul Bartholomew
  !! DESCRIPTION: Applies rotation for t < spinup_time.
  !!
  !############################################################################
  subroutine momentum_forcing_channel(dux1, duy1, duz1, ux1, uy1, uz1)

    implicit none

    real(mytype), intent(in), dimension(xsize(1), xsize(2), xsize(3)) :: ux1, uy1, uz1
    real(mytype), dimension(xsize(1), xsize(2), xsize(3), ntime) :: dux1, duy1, duz1

    if (cpg) then
        !! fcpg: add constant pressure gradient in streamwise direction
        if (idir_stream == 1) then
           dux1(:,:,:,1) = dux1(:,:,:,1) + fcpg !* (re/re_cent)**2
        else
           duz1(:,:,:,1) = duz1(:,:,:,1) + fcpg !* (re/re_cent)**2
        endif
    endif

    ! To update to take into account possible flow in z dir
    if (itime < spinup_time .and. iin <= 2) then
       if (nrank==0.and.(mod(itime, ilist) == 0 .or. itime == ifirst .or. itime == ilast)) &
          write(*,*) 'Rotating turbulent channel at speed ',wrotation
       dux1(:,:,:,1) = dux1(:,:,:,1) - wrotation*uy1(:,:,:)
       duy1(:,:,:,1) = duy1(:,:,:,1) + wrotation*ux1(:,:,:)
    endif

  end subroutine momentum_forcing_channel
  !############################################################################
  !############################################################################
  subroutine geomcomplex_channel(epsi,nxi,nxf,ny,nyi,nyf,nzi,nzf,yp,remp)

    use param, only : zero, one, two, ten
    use ibm

    implicit none

    integer                    :: nxi,nxf,ny,nyi,nyf,nzi,nzf
    real(mytype),dimension(nxi:nxf,nyi:nyf,nzi:nzf) :: epsi
    real(mytype),dimension(ny) :: yp
    real(mytype)               :: remp
    integer                    :: j
    real(mytype)               :: ym
    real(mytype)               :: zeromach
    real(mytype)               :: h

    epsi(:,:,:) = zero
    h = (yly - two) / two

    zeromach=one
    do while ((one + zeromach / two) .gt. one)
       zeromach = zeromach/two
    end do
    zeromach = ten*zeromach

    do j=nyi,nyf
       ym=yp(j)
       if ((ym.le.h).or.(ym.ge.(h+two))) then
          epsi(:,j,:)=remp
       endif
    enddo

    return
  end subroutine geomcomplex_channel
  !############################################################################
  !############################################################################
  subroutine sem_init_channel(ux1, uy1, uz1)

    implicit none

    ! Arguments
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: ux1,uy1,uz1

    ! Local variables
    integer :: i, j, k, ii, jj
    real(mytype) :: x, y, z, ftent, um
    integer ( kind = 4 ), parameter :: nsemini = 1000 ! For the moment we fix it but after this can go in the input file
    real(mytype), dimension(3,nsemini) :: eddy, posvor
    real(mytype)     :: volsemini, rrand, ddx, ddy, ddz, lsem, upr, vpr, wpr
    real(mytype), dimension(3) :: dim_min, dim_max

     dim_min(1) = zero
     dim_min(2) = zero
     dim_min(3) = zero
     dim_max(1) = xlx
     dim_max(2) = yly
     dim_max(3) = zlz
     volsemini = xlx * yly * zlz
     ! 3 int to get different random numbers
     do jj = 1, nsemini
        ! Vortex Position
        do ii = 1, 3
           call random_number(rrand)
           posvor(ii,jj) = dim_min(ii)+(dim_max(ii)-dim_min(ii))*rrand
        enddo
        ! Eddy intensity
        do ii = 1, 3
           call random_number(rrand)
           if (rrand <= zpfive) then
              eddy(ii,jj) = -one
           else
              eddy(ii,jj) = +one
           endif 
        enddo
     enddo
     ! Loops to apply the fluctuations 
     do k = 1, xsize(3)
        z = real((k+xstart(3)-1-1),mytype)*dz
        do j = 1, xsize(2)
           if (istret==0) y=real(j+xstart(2)-2,mytype)*dy
           if (istret/=0) y=yp(j+xstart(2)-1)
           do i = 1, xsize(1)
              x = real(i-1,mytype)*dx
              lsem = 0.15_mytype ! For the moment we keep it constant
              upr = zero
              vpr = zero
              wpr = zero
              do jj = 1, nsemini
                 ddx = abs(x-posvor(1,jj))
                 ddy = abs(y-posvor(2,jj))
                 ddz = abs(z-posvor(3,jj))
                 if (ddx < lsem .and. ddy < lsem .and. ddz < lsem) then
                    ! coefficients for the intensity of the fluctuation
                    ftent = (one-ddx/lsem)*(one-ddy/lsem)*(one-ddz/lsem)
                    ftent = ftent / (sqrt(two/three*lsem))**3
                    upr = upr + eddy(1,jj) * ftent
                    vpr = vpr + eddy(2,jj) * ftent
                    wpr = wpr + eddy(3,jj) * ftent
                 endif
              enddo
              upr = upr * sqrt(volsemini/nsemini)
              vpr = vpr * sqrt(volsemini/nsemini)
              wpr = wpr * sqrt(volsemini/nsemini)
              ! 
              um  = one-(y-yly*half)**2 ! we can use a better arroximation 
              if (idir_stream == 1) then
                 ux1(i,j,k)=upr*sqrt(two/three*init_noise*um) + um
                 uy1(i,j,k)=vpr*sqrt(two/three*init_noise*um)
                 uz1(i,j,k)=wpr*sqrt(two/three*init_noise*um)
              else
                 uz1(i,j,k)=upr*sqrt(two/three*init_noise*um) + um
                 uy1(i,j,k)=vpr*sqrt(two/three*init_noise*um)
                 ux1(i,j,k)=wpr*sqrt(two/three*init_noise*um)
              endif
           enddo
        enddo
     enddo

  end subroutine sem_init_channel
  !############################################################################


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

! SUBROUTINES FOR INPUT IN INITIAL CONDITIONS for laminar-turbulent transition mixed convection channel flow.
! OJO: This base flow is calculated for Uc = Ub and Tc = RemPrAh adimensionalization. 
	subroutine base_flow(ux,uy,uz,ra)
	
    use decomp_2d
    use variables
    use param
    use MPI
	
    implicit none
    real(mytype),dimension(xsize(1),xsize(2),xsize(3)),intent(out) :: ux,uy,uz
    
    integer :: i, j, k
    real(mytype) :: x, y, z, F, m, ra,ka, E

    ! Inicializar con ceros
    ux = zero
    uy = zero
    uz = zero
		
    ! Aplicar condiciones iniciales
    do k = 1, xsize(3)
        do j = 1, xsize(2)
            if (istret == 0) then
                y = real(j + xstart(2) - 2, mytype) * dy - yly * half
            else
                y = yp(j + xstart(2) - 1) - yly * half
            endif
            do i = 1, xsize(1)
                if (gravx < 0.0) then
                      ka = (ra)**0.25 / sqrt(2.0)
                      E = -2.0 * ka * sqrt(ra) * (cosh(2.0 * ka) + cos(2.0 * ka)) / (sinh(2.0 * ka) - sin(2.0 * ka))
                      ux(i, j, k) = -E / sqrt(ra) * ( (sinh(ka * (1.0 + y)) * sin(ka * (1.0 - y))) + (sinh(ka * (1.0 - y)) * sin(ka * (1.0 + y)))) / (cosh(2.0 * ka) + cos(2.0 * ka) )
                else
                      m = (ra)**0.25
                      F = (2.0 * (m)**3) / (tanh(m) - tan(m))
                      ux(i, j, k) = ( F / (2.0 * m**2) ) * ((cosh(m * y) / cosh(m)) - (cos(m * y) / cos(m)))
                endif
            enddo
        enddo
    enddo

	end subroutine base_flow


subroutine base_flow_scalar(phi,ra)
	
    use decomp_2d
    use variables
    use param
    use MPI
	
    implicit none
    
    real(mytype),intent(out),dimension(xsize(1),xsize(2),xsize(3),numscalar) :: phi
    integer :: i, j, k
    real(mytype) :: x, y, z, F, m,E,ka
		real(mytype),intent(in) :: ra
    ! Inicializar con ceros
		phi = zero
    ! Aplicar condiciones iniciales
    do k = 1, xsize(3)
        do j = 1, xsize(2)
            if (istret == 0) then
                y = real(j + xstart(2) - 2, mytype) * dy - yly * half
            else
                y = yp(j + xstart(2) - 1) - yly * half
            endif
            do i = 1, xsize(1)
             		if  (gravx < 0.0)  then
                   ka = (ra)**0.25 / sqrt(2.0)
                   E = -2.0 * ka * sqrt(ra) * (cosh(2.0 * ka) + cos(2.0 * ka)) / (sinh(2.0 * ka) - sin(2.0 * ka))
                   phi(i, j, k, :) = E / ra * ( 1.0 - ( ( (cosh(ka * (1.0 + y)) * cos(ka * (1.0 - y))) + (cosh(ka * (1.0 - y)) * cos(ka * (1.0 + y))) ) / (cosh(2.0 * ka) + cos(2.0 * ka)) ) )
                else
                   m = (ra)**0.25
                   F = (2.0 * m**3) / (tanh(m) - tan(m))
                   phi(i, j, k, :) = ( F / 2.0 * m**4) * ((cosh(m * y) / cosh(m)) + (cos(m * y) / cos(m)) - 2.0)
                end if 		
            enddo
        enddo
    enddo
  
  end subroutine base_flow_scalar

  
	subroutine perturbation(p_ux,p_uy,p_uz,alf,bet,amp,urread,uiread,vrread,viread,wrread,wiread)
	
    use decomp_2d
    use variables
    use param
    use MPI
    
    implicit none
    real(mytype), intent(in) :: amp,alf,bet
    real(mytype), intent(in) :: urread(:), uiread(:), vrread(:), viread(:), wrread(:), wiread(:)
    real(mytype), intent(out) :: p_ux(:,:,:), p_uy(:,:,:), p_uz(:,:,:)
    
    integer :: i, j, k
    real(mytype) :: x, y, z

    ! Inicializar con ceros
    p_ux = zero
    p_uy = zero
    p_uz = zero

    ! Aplicar condiciones iniciales
    do k = 1, xsize(3)
        z = real(k + xstart(3) - 2, mytype) * dz !- zlz * half
        do j = 1, xsize(2)
            !if (istret == 0) then
            !    y = real(j + xstart(2) - 2, mytype) * dy - yly * half
            !else
            !    y = yp(j + xstart(2) - 1) - yly * half
            !endif
            do i = 1, xsize(1)
                x = real(i + xstart(1) - 2, mytype) * dx !- xlx * half
                
                p_ux(i, j, k) = amp * (urread(j + xstart(2) - 1) * cos(alf*x + bet*z) &
                               - uiread(j + xstart(2) - 1) * sin(alf*x + bet*z))
                p_uy(i, j, k) = amp * (vrread(j + xstart(2) - 1) * cos(alf*x + bet*z) &
                               - viread(j + xstart(2) - 1) * sin(alf*x + bet*z))
                p_uz(i, j, k) = amp * (wrread(j + xstart(2) - 1) * cos(alf*x + bet*z) &
                               - wiread(j + xstart(2) - 1) * sin(alf*x + bet*z))
            enddo
        enddo
    enddo
	end subroutine perturbation


	subroutine perturbation_scalar(p_phi,alf,bet,amp,phirread,phiiread)
	
    use decomp_2d
    use variables
    use param
    use MPI
    
    implicit none
    real(mytype), intent(in) :: amp,alf,bet
    real(mytype), intent(in) :: phirread(:), phiiread(:)
    real(mytype),intent(out),dimension(xsize(1),xsize(2),xsize(3),numscalar) :: p_phi
    
    integer :: i, j, k
    real(mytype) :: x, y, z

    ! Inicializar con ceros

    p_phi = zero

    ! Aplicar condiciones iniciales
    do k = 1, xsize(3)
        z = real(k + xstart(3) - 2, mytype) * dz !- zlz * half
        do j = 1, xsize(2)
            !if (istret == 0) then
            !    y = real(j + xstart(2) - 2, mytype) * dy - yly * half
            !else
            !    y = yp(j + xstart(2) - 1) - yly * half
            !endif
            do i = 1, xsize(1)
                x = real(i + xstart(1) - 2, mytype) * dx !- xlx * half
                p_phi(i, j, k,:) = amp * (phirread(j + xstart(2) - 1) * cos(alf*x + bet*z) &
                                   - phiiread(j + xstart(2) - 1) * sin(alf*x + bet*z))
            enddo
        enddo
    enddo
	end subroutine perturbation_scalar 
  
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   
end module channel
