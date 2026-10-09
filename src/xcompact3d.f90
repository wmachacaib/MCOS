!Copyright (c) 2012-2022, Xcompact3d
!This file is part of Xcompact3d (xcompact3d.com)
!SPDX-License-Identifier: BSD 3-Clause

program xcompact3d

  use var
  use case

  use transeq, only : calculate_transeq_rhs
  use time_integrators, only : int_time
  use navier, only : velocity_to_momentum, momentum_to_velocity, pre_correc, &
       calc_divu_constraint, solve_poisson, cor_vel
  use tools, only : restart, simu_stats, apply_spatial_filter, read_inflow
  use turbine, only : compute_turbines
  use ibm_param
  use ibm, only : body
  use genepsi, only : genepsi3d
  use mhd,    only : Bm,mhd_equation,test_magnetic, &
                     solve_poisson_mhd
  use param, only : mhd_active
  use particle, only : intt_particles

!xxxxxxxxxxxxxxxxxxxxxxx
  use user_specific
!xxxxxxxxxxxxxxxxxxxxxxx

  implicit none


  call init_xcompact3d()

  do itime=ifirst,ilast
     !t=itime*dt
     t=t0 + (itime0 + itime + 1 - ifirst)*dt
     
     call simu_stats(2)

     if (iturbine.ne.0) call compute_turbines(ux1, uy1, uz1)

     if (iin.eq.3.and.mod(itime,ntimesteps)==1) then
        call read_inflow(ux_inflow,uy_inflow,uz_inflow,itime/ntimesteps)
     endif

     if ((itype.eq.itype_abl.or.iturbine.ne.0).and.(ifilter.ne.0).and.(ilesmod.ne.0)) then
        call filter(C_filter)
        call apply_spatial_filter(ux1,uy1,uz1,phi1)
     endif

     do itr=1,iadvance_time

        call set_fluid_properties(rho1,mu1)
        call boundary_conditions(rho1,ux1,uy1,uz1,phi1,ep1)

        if (imove.eq.1) then ! update epsi for moving objects
          if ((iibm.eq.2).or.(iibm.eq.3)) then
             call genepsi3d(ep1)
          else if (iibm.eq.1) then
             call body(ux1,uy1,uz1,ep1)
          endif
        endif
        call calculate_transeq_rhs(drho1,dux1,duy1,duz1,dphi1,rho1,ux1,uy1,uz1,ep1,phi1,divu3)

#ifdef DEBG
        call check_transients()
#endif
        
        if (ilmn) then
           !! XXX N.B. from this point, X-pencil velocity arrays contain momentum (LMN only).
           call velocity_to_momentum(rho1,ux1,uy1,uz1)
        endif

        call int_time(rho1,ux1,uy1,uz1,phi1,drho1,dux1,duy1,duz1,dphi1)
        call pre_correc(ux1,uy1,uz1,ep1)

        call calc_divu_constraint(divu3,rho1,phi1)
        call solve_poisson(pp3,px1,py1,pz1,rho1,ux1,uy1,uz1,ep1,drho1,divu3)
        call cor_vel(ux1,uy1,uz1,px1,py1,pz1)

        if(mhd_active .and. mhd_equation == 'induction') then
          call solve_poisson_mhd()
        endif

        if (ilmn) then
           call momentum_to_velocity(rho1,ux1,uy1,uz1)
           !! XXX N.B. from this point, X-pencil velocity arrays contain velocity (LMN only).
           !! Note - all other solvers work on velocity always
        endif
        
        call test_flow(rho1,ux1,uy1,uz1,phi1,ep1,drho1,divu3)

        if(mhd_active) call test_magnetic

     enddo !! End sub timesteps

     if(particle_active) then
       call intt_particles(ux1,uy1,uz1,t)
     endif

     call restart(ux1,uy1,uz1,dux1,duy1,duz1,ep1,pp3(:,:,:,1),phi1,dphi1,px1,py1,pz1,rho1,drho1,mu1,1)

     call simu_stats(3)

     call postprocessing(rho1,ux1,uy1,uz1,pp3,phi1,ep1)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   if (itime.gt.initstat)then
     call module_user_write(phG,ph1,ph2,ph3,ph4)!actualiza los valores de *mean.dat a partir de 100000 y lo promedia con el inicio en beg_stat (esto es importante pq el beg_stat esta en user_module seteado y usado en mystat en todas las actualizaciones del campo de promediados.)	
     if (mod(itime,icheckpoint)==0) then
       call module_user_post(phG,ph1,ph2,ph3,ph4)! escribe los archivos ya promediados
	    ! por eso ponemos para que lo escriba cada icheckpoint!!
       call compute_balance_2d() ! hagarra los archivos promediados y los promedia en las direcciones homogeneas y escribe el promediado dependiente sólo de $y$	y ya calculado como el promedio de fluctuaciones. Aqui realmente saca por ejemplo el u'u'.
     endif
   endif

   if (mod(itime,200).eq.0) then     
    call module_user_spat(phG,ph1,ph2,ph3,ph4) ! to calculate the statistics at each time step
    call compute_balance_spat_2d() ! use the ms1d calculated in module_user_spat
   endif   
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx     
  enddo !! End time loop

  call finalise_xcompact3d()

end program xcompact3d
!########################################################################
!########################################################################
subroutine init_xcompact3d()

  use MPI
  use decomp_2d
  use decomp_2d_io, only : decomp_2d_io_init
  USE decomp_2d_poisson, ONLY : decomp_2d_poisson_init
  use case
  use sandbox, only : init_sandbox
  use forces

  use var

  use navier, only : calc_divu_constraint
  use tools, only : test_speed_min_max, test_scalar_min_max, &
       restart, &
       simu_stats, compute_cfldiff, &
       init_inflow_outflow, read_inflow

  use param, only : ilesmod, jles,itype
  use param, only : irestart, mhd_active
  use param, only : periodic_bc

  use variables, only : nx, ny, nz, nxm, nym, nzm
  use variables, only : p_row, p_col
  use variables, only : nstat, nvisu, ilist

  use les, only: init_explicit_les
  use turbine, only: init_turbines

  use visu, only : visu_init, visu_ready

  use genepsi, only : genepsi3d, epsi_init
  use ibm, only : body

  use probes, only : init_probes

  use mhd, only: mhd_init
  use particle,  only : particle_report,local_domain_size

!xxxxxxxxxxxxxxxxxxxxxxx
  use user_specific
!xxxxxxxxxxxxxxxxxxxxxxx

  implicit none

  integer :: ierr

  integer :: nargin, FNLength, status, DecInd
  logical :: back
  character(len=80) :: InputFN, FNBase
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  integer :: code, j
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  
    
  !! Initialise MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,nrank,ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)

  ! Handle input file like a boss -- GD
  nargin=command_argument_count()
  if (nargin <1) then
     InputFN='input.i3d'
     if (nrank==0) write(*,*) 'Xcompact3d is run with the default file -->', trim(InputFN)
  elseif (nargin >= 1) then
     call get_command_argument(1,InputFN,FNLength,status)
     back=.true.
     FNBase=inputFN((index(InputFN,'/',back)+1):len(InputFN))
     DecInd=index(FNBase,'.',back)
     if (DecInd >1) then
        FNBase=FNBase(1:(DecInd-1))
     end if
     if (nrank==0) write(*,*) 'Xcompact3d is run with the provided file -->', trim(InputFN)
  endif

#ifdef ADIOS2
  if (nrank .eq. 0) then
     print *, " WARNING === WARNING === WARNING === WARNING === WARNING"
     print *, " WARNING: Running Xcompact3d with ADIOS2"
     print *, "          this is currently experimental"
     print *, "          for safety of results it is recommended"
     print *, "          to run the default build as this feature"
     print *, "          is developed. Thank you for trying it."
     print *, " WARNING === WARNING === WARNING === WARNING === WARNING"
  endif
#endif
  
  call parameter(InputFN)

  call decomp_2d_init(nx,ny,nz,p_row,p_col,periodic_bc)

  call decomp_2d_io_init()
  call init_coarser_mesh_statS(nstat,nstat,nstat,.true.)    !start from 1 == true
  call init_coarser_mesh_statV(nvisu,nvisu,nvisu,.true.)    !start from 1 == true
  !div: nx ny nz --> nxm ny nz --> nxm nym nz --> nxm nym nzm
  call decomp_info_init(nxm, nym, nzm, ph1)
  call decomp_info_init(nxm, ny, nz, ph4)
  !gradp: nxm nym nzm -> nxm nym nz --> nxm ny nz --> nx ny nz
  call decomp_info_init(nxm, ny, nz, ph2)
  call decomp_info_init(nxm, nym, nz, ph3)

  call init_variables()

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

		! Linear Stability Eigenfunction Reading 19/02/2025

    if (nrank==0) then
            OPEN(11, FILE="2d_eigenfun.dat", STATUS='OLD', ACTION='READ')
            DO j = 1, ny
                if (iscalar==1) then
                		READ(11, *) urread_2d(j), uiread_2d(j), vrread_2d(j), viread_2d(j), wrread_2d(j), wiread_2d(j),phirread_2d(j), phiiread_2d(j)
                		!print *,j,urread_2d(j),uiread_2d(j),vrread_2d(j),viread_2d(j),wrread_2d(j),wiread_2d(j),phirread_2d(j), phiiread_2d(j)
                else
                		READ(11, *) urread_2d(j), uiread_2d(j), vrread_2d(j), viread_2d(j), wrread_2d(j), wiread_2d(j)
                		!print *,j,urread_2d(j),uiread_2d(j),vrread_2d(j),viread_2d(j),wrread_2d(j),wiread_2d(j)
                end if
            END DO
            CLOSE(11)
    endif
    
    call MPI_Bcast(urread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(uiread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(vrread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(viread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(wrread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(wiread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    
    if (iscalar==1) then
    	call MPI_Bcast(phirread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    	call MPI_Bcast(phiiread_2d, ny, real_type, 0, MPI_COMM_WORLD, code)
    end if
    
    if (nrank==0) then
    		print *,'==========================================================='
    		print *, 'File of Eigenfunctions 2D read and shared with Bcast'
    		print *,'==========================================================='
  	endif
  	
  	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  	
  	    if (nrank==0) then
            OPEN(11, FILE="3d_pbeta_eigenfun.dat", STATUS='OLD', ACTION='READ')
            DO j = 1, ny
            		if (iscalar==1) then
                		READ(11, *) urread_3d_pbeta(j), uiread_3d_pbeta(j), vrread_3d_pbeta(j), viread_3d_pbeta(j), wrread_3d_pbeta(j), wiread_3d_pbeta(j), phirread_3d_pbeta(j),phiiread_3d_pbeta(j)
                		!print *,j,urread_3d_pbeta(j),uiread_3d_pbeta(j),vrread_3d_pbeta(j),viread_3d_pbeta(j),wrread_3d_pbeta(j),wiread_3d_pbeta(j),phirread_3d_pbeta(j),phiiread_3d_pbeta(j)
                else
                		READ(11, *) urread_3d_pbeta(j), uiread_3d_pbeta(j), vrread_3d_pbeta(j), viread_3d_pbeta(j), wrread_3d_pbeta(j), wiread_3d_pbeta(j)
                		!print *,j,urread_3d_pbeta(j),uiread_3d_pbeta(j),vrread_3d_pbeta(j),viread_3d_pbeta(j),wrread_3d_pbeta(j),wiread_3d_pbeta(j)
                end if
            END DO
            CLOSE(11)
    endif
    
    call MPI_Bcast(urread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(uiread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code) 
    call MPI_Bcast(vrread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(viread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(wrread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(wiread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    
    if (iscalar==1) then
    	call MPI_Bcast(phirread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    	call MPI_Bcast(phiiread_3d_pbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    end if
    
    if (nrank==0) then
    		print *,'============================================================'
    		print *, 'File of Eigenfunctions 3D (+beta) read and shared with Bcast'
    		print *,'============================================================'
  	endif
  	
  	!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  	
  	    if (nrank==0) then
            OPEN(11, FILE="3d_mbeta_eigenfun.dat", STATUS='OLD', ACTION='READ')
            DO j = 1, ny
            		if (iscalar==1) then
               			READ(11, *) urread_3d_mbeta(j), uiread_3d_mbeta(j), vrread_3d_mbeta(j), viread_3d_mbeta(j), wrread_3d_mbeta(j), wiread_3d_mbeta(j), phirread_3d_mbeta(j), phiiread_3d_mbeta(j)
                		!print *,j,urread_3d_mbeta(j),uiread_3d_mbeta(j),vrread_3d_mbeta(j),viread_3d_mbeta(j),wrread_3d_mbeta(j),wiread_3d_mbeta(j), phirread_3d_mbeta(j), phiiread_3d_mbeta(j)
                else
                		READ(11, *) urread_3d_mbeta(j), uiread_3d_mbeta(j), vrread_3d_mbeta(j), viread_3d_mbeta(j), wrread_3d_mbeta(j), wiread_3d_mbeta(j)
                		!print *,j,urread_3d_mbeta(j),uiread_3d_mbeta(j),vrread_3d_mbeta(j),viread_3d_mbeta(j),wrread_3d_mbeta(j),wiread_3d_mbeta(j)
                end if
            END DO
            CLOSE(11)
    endif
    
    call MPI_Bcast(urread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(uiread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(vrread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(viread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(wrread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    call MPI_Bcast(wiread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    
		if (iscalar==1) then
    	call MPI_Bcast(phirread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    	call MPI_Bcast(phiiread_3d_mbeta, ny, real_type, 0, MPI_COMM_WORLD, code)
    end if
    
    if (nrank==0) then
    		print *,'============================================================'
    		print *, 'File of Eigenfunctions 3D (-beta) read and shared with Bcast'
    		print *,'============================================================'
  	endif
  	
  	
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  call schemes()

  call decomp_2d_poisson_init()
  call decomp_info_init(nxm,nym,nzm,phG)

  if (ilesmod.ne.0) then
     if (jles.gt.0)  call init_explicit_les()
  endif

  if ((iibm.eq.2).or.(iibm.eq.3)) then
     call genepsi3d(ep1)
  else if (iibm.eq.1) then
     call epsi_init(ep1)
     call body(ux1,uy1,uz1,ep1)
  endif

  if (iforces.eq.1) then
     call init_forces()
     if (irestart==1) then
        call restart_forces(0)
     endif
  endif

  !####################################################################
  ! initialise mhd
  if (mhd_active) call mhd_init()

  !####################################################################
  ! initialise particles
  if (particle_active) then
    call particle_report('input')

    call local_domain_size
  endif


  !####################################################################
  ! initialise visu
  if (ivisu.ne.0) then
     call visu_init()
     call visu_case_init() !! XXX: If you get error about uninitialised IO, look here.
                           !! Ensures additional case-specific variables declared for IO
     call visu_ready()
  end if
  ! compute diffusion number of simulation
  call compute_cfldiff()
  !####################################################################
  if (irestart==0) then
     call init(rho1,ux1,uy1,uz1,ep1,phi1,drho1,dux1,duy1,duz1,dphi1,pp3,px1,py1,pz1)
     itime = 0
     call preprocessing(rho1,ux1,uy1,uz1,pp3,phi1,ep1)
  else
     itr=1
     if (itype == itype_sandbox) then
        call init_sandbox(ux1,uy1,uz1,ep1,phi1,1)
     end if
     call restart(ux1,uy1,uz1,dux1,duy1,duz1,ep1,pp3(:,:,:,1),phi1,dphi1,px1,py1,pz1,rho1,drho1,mu1,0)
  endif

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call module_user_init(phG,ph1,ph2,ph3,ph4)
  !print *,nrank,'After module_user_init'
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  if ((ioutflow.eq.1).or.(iin.eq.3)) then
     call init_inflow_outflow()
     if ((irestart==1).and.(iin.eq.3)) then
        call read_inflow(ux_inflow,uy_inflow,uz_inflow,itime/ntimesteps)
     endif
  end if

  if ((iibm.eq.2).or.(iibm.eq.3)) then
     call genepsi3d(ep1)
  else if (iibm.eq.1) then
     call body(ux1,uy1,uz1,ep1)
  endif

  if (mod(itime, ilist) == 0 .or. itime == ifirst) then
     call test_speed_min_max(ux1,uy1,uz1)
     if (iscalar==1) call test_scalar_min_max(phi1)
  endif

  call simu_stats(1)

  call calc_divu_constraint(divu3, rho1, phi1)

  call init_probes()

  if (iturbine.ne.0) call init_turbines(ux1, uy1, uz1)

  if (itype==2) then
     if(nrank.eq.0)then
        open(42,file='time_evol.dat',form='formatted')
        if(mhd_active) then
           open(43,file='mhd_time_evol.dat',form='formatted')
        endif
     endif
  endif

  if (iforces == 1) then
     if(nrank.eq.0)then
        open(38,file='forces.dat',form='formatted')
     endif
  endif
  
  if (itype==10) then
     if(nrank.eq.0)then
        open(42,file='shear.dat',form='formatted')
     endif
  endif

endsubroutine init_xcompact3d
!########################################################################
!########################################################################
subroutine finalise_xcompact3d()

  use MPI
  use decomp_2d_mpi
  use decomp_2d
  use decomp_2d_io, only : decomp_2d_io_finalise

  use tools, only : simu_stats
  use param, only : itype, jles, ilesmod, mhd_active
  use probes, only : finalize_probes
  use visu, only : visu_finalise
  use les, only: finalise_explicit_les
  use mhd, only: mhd_fin
  use case, only: visu_case_finalise
  use forces, only: iforces

  implicit none

  integer :: ierr
  
  if (itype==2) then
     if(nrank.eq.0)then
        close(42)
        if(mhd_active) then
           close(43)
        endif
     endif
  endif

  if (iforces == 1) then
     if(nrank.eq.0)then
        close(38)
     endif
  endif

  if (itype==10) then
     if(nrank.eq.0)then
        close(42)
     endif
  endif
  
  call simu_stats(4)
  call finalize_probes()
  call visu_case_finalise()
  call visu_finalise()
  if (mhd_active) call mhd_fin()
  if (ilesmod.ne.0) then
     if (jles.gt.0) call finalise_explicit_les()
  endif
  call decomp_2d_io_finalise()
  call decomp_2d_finalize
  CALL MPI_FINALIZE(ierr)

endsubroutine finalise_xcompact3d

subroutine check_transients()

  use decomp_2d_constants, only : mytype
  use mpi
  use var
  
  implicit none

  real(mytype) :: dep
  integer :: code
   
  dep=maxval(abs(dux1))
  call MPI_ALLREDUCE(MPI_IN_PLACE,dep,1,real_type,MPI_MAX,MPI_COMM_WORLD,code)
  if (nrank == 0) write(*,*)'## MAX dux1 ', dep
 
  dep=maxval(abs(duy1))
  call MPI_ALLREDUCE(MPI_IN_PLACE,dep,1,real_type,MPI_MAX,MPI_COMM_WORLD,code)
  if (nrank == 0) write(*,*)'## MAX duy1 ', dep
 
  dep=maxval(abs(duz1))
  call MPI_ALLREDUCE(MPI_IN_PLACE,dep,1,real_type,MPI_MAX,MPI_COMM_WORLD,code)
  if (nrank == 0) write(*,*)'## MAX duz1 ', dep
  
end subroutine check_transients
