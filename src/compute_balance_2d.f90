subroutine compute_balance_2d()
! We work in x pencil
  use decomp_2d_constants, only : mytype
  use param, only : iscalar
  use user_stats
  ! Pression sur grille décalée
!  use decomp_2d, only : transpose_z_to_y, transpose_y_to_x
!  use variables, only : ph3

  implicit none

  integer :: i,j,k,n
  ! Ordre 1
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: u,v,w
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dpdx, dpdy, dpdz
  ! Ordre 2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: uu,vv,ww
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: uv,uw,vw
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: udpdx, udpdy, udpdz
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: vdpdx, vdpdy, vdpdz
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: wdpdx, wdpdy, wdpdz
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudx2, dudy2, dudz2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dvdx2, dvdy2, dvdz2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dwdx2, dwdy2, dwdz2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudvdx, dudwdx, dvdwdx
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudvdy, dudwdy, dvdwdy
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudvdz, dudwdz, dvdwdz
  ! Ordre 3
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: uuu, uvv, uww
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: vuu, vvv, vww
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: wuu, wvv, www
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: uvw

  ! Scalaire
  ! Ordre 1
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: phi
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dphidx, dphidy, dphidz
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: deltaphi
  ! Ordre 2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: uphi, vphi, wphi, phiphi
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dphidx2, dphidy2, dphidz2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: phidpdx, phidpdy, phidpdz
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudphidx, dvdphidx, dwdphidx
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudphidy, dvdphidy, dwdphidy
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudphidz, dvdphidz, dwdphidz
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: udeltaphi, vdeltaphi, wdeltaphi
  ! Ordre 3
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: uphi2,vphi2,wphi2
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: phiuu,phivv,phiww
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: phiuv,phiuw,phivw

  ! Pression
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: pres, pres2

  ! Pression sur grille décalée
!  real(mytype), dimension(ph3%xst(2):ph3%xen(2)) :: presi, pres2i
!  real(mytype), dimension(ph3%xst(1):ph3%xen(1),ph3%xst(2):ph3%xen(2),ph3%xst(3):ph3%xen(3)) :: pp3mx, pp32mx
!  real(mytype), dimension(ph3%yst(1):ph3%yen(1),ph3%yst(2):ph3%yen(2),ph3%yst(3):ph3%yen(3)) :: pp3my

  ! Gradient vitesse
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudx, dvdx, dwdx
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudy, dvdy, dwdy ! Ojo en mystas tb hay una variable como esta, sin embargo vive en una de sus subrutinas, no vive en user_stat como variable global o al menos eso parece
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudz, dvdz, dwdz
  !dudx=0.; dvdx=0.; dwdx=0.;!in IO problem this quantity doesn't is zero.
  !dudz=0.; dvdz=0.; dwdz=0.;

  ! Ordre 1
  if (.true.) then
!
  ! Init Ordre 1
  u=0.;		v=0.;		w=0.
  dpdx=0.;	dpdy=0.;	dpdz=0.;
  dudy=0.;      dvdy=0.;        dwdy=0.;
  dudx=0.; dvdx=0.; dwdx=0.;!in IO problem this quantity doesn't is zero.
  dudz=0.; dvdz=0.; dwdz=0.;
!
  ! Compute Average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    u(i,j)	=u(i,j)	+um(i,j,k)
    v(i,j)	=v(i,j)	+vm(i,j,k)
    w(i,j)	=w(i,j)	+wm(i,j,k)
    dpdx(i,j)	=dpdx(i,j)+dpdxm(i,j,k)
    dpdy(i,j)	=dpdy(i,j)+dpdym(i,j,k)
    dpdz(i,j)	=dpdz(i,j)+dpdzm(i,j,k)
!	Falta d*dx
    dudy(i,j)     =dudy(i,j)+dudym(i,j,k)
    dvdy(i,j)     =dvdy(i,j)+dvdym(i,j,k)
    dwdy(i,j)     =dwdy(i,j)+dwdym(i,j,k)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    dudx(i,j)     =dudx(i,j)+dudxm(i,j,k)
    dvdx(i,j)     =dvdx(i,j)+dvdxm(i,j,k)
    dwdx(i,j)     =dwdx(i,j)+dwdxm(i,j,k)

    dudz(i,j)     =dudz(i,j)+dudzm(i,j,k)
    dvdz(i,j)     =dvdz(i,j)+dvdzm(i,j,k)
    dwdz(i,j)     =dwdz(i,j)+dwdzm(i,j,k)

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  end do
  end do
  end do
!
  ! Average ordre 1
	! Only average in z direction
!
  ! Write data
  call write_myaverage_real_1d(u   ,xst3,xen3,xst2,xen2,   'um2d')
  call write_myaverage_real_1d(v   ,xst3,xen3,xst2,xen2,   'vm2d')
  call write_myaverage_real_1d(w   ,xst3,xen3,xst2,xen2,   'wm2d')
  call write_myaverage_real_1d(dpdx,xst3,xen3,xst2,xen2,'dpdxm2d')
  call write_myaverage_real_1d(dpdy,xst3,xen3,xst2,xen2,'dpdym2d')
  call write_myaverage_real_1d(dpdz,xst3,xen3,xst2,xen2,'dpdzm2d')
  call write_myaverage_real_1d(dudy,xst3,xen3,xst2,xen2,'dudym2d')
  call write_myaverage_real_1d(dvdy,xst3,xen3,xst2,xen2,'dvdym2d')
  call write_myaverage_real_1d(dwdy,xst3,xen3,xst2,xen2,'dwdym2d')
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call write_myaverage_real_1d(dudx,xst3,xen3,xst2,xen2,'dudxm2d')
  call write_myaverage_real_1d(dvdx,xst3,xen3,xst2,xen2,'dvdxm2d')
  call write_myaverage_real_1d(dwdx,xst3,xen3,xst2,xen2,'dwdxm2d')
  call write_myaverage_real_1d(dudz,xst3,xen3,xst2,xen2,'dudzm2d')
  call write_myaverage_real_1d(dvdz,xst3,xen3,xst2,xen2,'dvdzm2d')
  call write_myaverage_real_1d(dwdz,xst3,xen3,xst2,xen2,'dwdzm2d')

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
  endif ! Ordre 1

  ! Ordre 2
  if (.true.) then
!
  ! Init ordre 2
  uu=0.; 	 vv=0.; 	ww=0.; 
  uv=0.; 	 uw=0.; 	vw=0.; 
  udpdx=0.; 	 udpdy=0.; 	udpdz=0.; 
  vdpdx=0.; 	 vdpdy=0.; 	vdpdz=0.; 
  wdpdx=0.; 	 wdpdy=0.; 	wdpdz=0.; 
  dudx2=0.; 	 dudy2=0.; 	dudz2=0.; 
  dvdx2=0.; 	 dvdy2=0.; 	dvdz2=0.; 
  dwdx2=0.; 	 dwdy2=0.; 	dwdz2=0.; 
  dudvdx=0.; 	 dudwdx=0.; 	dvdwdx=0.; 
  dudvdy=0.; 	 dudwdy=0.; 	dvdwdy=0.; 
  dudvdz=0.; 	 dudwdz=0.; 	dvdwdz=0.;
!
  ! Compute average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    uu(i,j)	=uu(i,j)		+uum(i,j,k)
    vv(i,j)	=vv(i,j)		+vvm(i,j,k)
    ww(i,j)	=ww(i,j)		+wwm(i,j,k)
    uv(i,j)	=uv(i,j)		+uvm(i,j,k)
    uw(i,j)	=uw(i,j)		+uwm(i,j,k)
    vw(i,j)	=vw(i,j)		+vwm(i,j,k)
!
    udpdx(i,j)	=udpdx(i,j)	+udpdxm(i,j,k)
    udpdy(i,j)	=udpdy(i,j)	+udpdym(i,j,k)
    udpdz(i,j)	=udpdz(i,j)	+udpdzm(i,j,k)
!
    vdpdx(i,j)	=vdpdx(i,j)	+vdpdxm(i,j,k)
    vdpdy(i,j)	=vdpdy(i,j)	+vdpdym(i,j,k)
    vdpdz(i,j)	=vdpdz(i,j)	+vdpdzm(i,j,k)
!
    wdpdx(i,j)	=wdpdx(i,j)	+wdpdxm(i,j,k)
    wdpdy(i,j)	=wdpdy(i,j)	+wdpdym(i,j,k)
    wdpdz(i,j)	=wdpdz(i,j)	+wdpdzm(i,j,k)
!
    dudx2(i,j)	=dudx2(i,j)	+dudx2m(i,j,k)
    dudy2(i,j)	=dudy2(i,j)	+dudy2m(i,j,k)
    dudz2(i,j)	=dudz2(i,j)	+dudz2m(i,j,k)
!
    dvdx2(i,j)	=dvdx2(i,j)	+dvdx2m(i,j,k)
    dvdy2(i,j)	=dvdy2(i,j)	+dvdy2m(i,j,k)
    dvdz2(i,j)	=dvdz2(i,j)	+dvdz2m(i,j,k)
!
    dwdx2(i,j)	=dwdx2(i,j)	+dwdx2m(i,j,k)
    dwdy2(i,j)	=dwdy2(i,j)	+dwdy2m(i,j,k)
    dwdz2(i,j)	=dwdz2(i,j)	+dwdz2m(i,j,k)
!
    dudvdx(i,j)	 =dudvdx(i,j)	+dudvdxm(i,j,k)
    dudwdx(i,j)	 =dudwdx(i,j)	+dudwdxm(i,j,k)
    dvdwdx(i,j)	 =dvdwdx(i,j)	+dvdwdxm(i,j,k)
!
    dudvdy(i,j)	 =dudvdy(i,j)	+dudvdym(i,j,k)
    dudwdy(i,j)	 =dudwdy(i,j)	+dudwdym(i,j,k)
    dvdwdy(i,j)	 =dvdwdy(i,j)	+dvdwdym(i,j,k)
!
    dudvdz(i,j)	 =dudvdz(i,j)	+dudvdzm(i,j,k)
    dudwdz(i,j)	 =dudwdz(i,j)	+dudwdzm(i,j,k)
    dvdwdz(i,j)	 =dvdwdz(i,j)	+dvdwdzm(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 2
!
  ! Write data
  call write_myaverage_real_1d(uu,xst3,xen3,xst2,xen2,'uum2d')
  call write_myaverage_real_1d(vv,xst3,xen3,xst2,xen2,'vvm2d')
  call write_myaverage_real_1d(ww,xst3,xen3,xst2,xen2,'wwm2d')
  call write_myaverage_real_1d(uv,xst3,xen3,xst2,xen2,'uvm2d')
  call write_myaverage_real_1d(uw,xst3,xen3,xst2,xen2,'uwm2d')
  call write_myaverage_real_1d(vw,xst3,xen3,xst2,xen2,'vwm2d')
!
  call write_myaverage_real_1d(udpdx,xst3,xen3,xst2,xen2,'udpdxm2d')
  call write_myaverage_real_1d(udpdy,xst3,xen3,xst2,xen2,'udpdym2d')
  call write_myaverage_real_1d(udpdz,xst3,xen3,xst2,xen2,'udpdzm2d')
  call write_myaverage_real_1d(vdpdx,xst3,xen3,xst2,xen2,'vdpdxm2d')
  call write_myaverage_real_1d(vdpdy,xst3,xen3,xst2,xen2,'vdpdym2d')
  call write_myaverage_real_1d(vdpdz,xst3,xen3,xst2,xen2,'vdpdzm2d')
  call write_myaverage_real_1d(wdpdx,xst3,xen3,xst2,xen2,'wdpdxm2d')
  call write_myaverage_real_1d(wdpdy,xst3,xen3,xst2,xen2,'wdpdym2d')
  call write_myaverage_real_1d(wdpdz,xst3,xen3,xst2,xen2,'wdpdzm2d')
!
  call write_myaverage_real_1d(dudx2,xst3,xen3,xst2,xen2,'dudx2m2d')
  call write_myaverage_real_1d(dudy2,xst3,xen3,xst2,xen2,'dudy2m2d')
  call write_myaverage_real_1d(dudz2,xst3,xen3,xst2,xen2,'dudz2m2d')
  call write_myaverage_real_1d(dvdx2,xst3,xen3,xst2,xen2,'dvdx2m2d')
  call write_myaverage_real_1d(dvdy2,xst3,xen3,xst2,xen2,'dvdy2m2d')
  call write_myaverage_real_1d(dvdz2,xst3,xen3,xst2,xen2,'dvdz2m2d')
  call write_myaverage_real_1d(dwdx2,xst3,xen3,xst2,xen2,'dwdx2m2d')
  call write_myaverage_real_1d(dwdy2,xst3,xen3,xst2,xen2,'dwdy2m2d')
  call write_myaverage_real_1d(dwdz2,xst3,xen3,xst2,xen2,'dwdz2m2d')
!
  call write_myaverage_real_1d(dudvdx,xst3,xen3,xst2,xen2,'dudvdxm2d')
  call write_myaverage_real_1d(dudvdy,xst3,xen3,xst2,xen2,'dudvdym2d')
  call write_myaverage_real_1d(dudvdz,xst3,xen3,xst2,xen2,'dudvdzm2d')
  call write_myaverage_real_1d(dudwdx,xst3,xen3,xst2,xen2,'dudwdxm2d')
  call write_myaverage_real_1d(dudwdy,xst3,xen3,xst2,xen2,'dudwdym2d')
  call write_myaverage_real_1d(dudwdz,xst3,xen3,xst2,xen2,'dudwdzm2d')
  call write_myaverage_real_1d(dvdwdx,xst3,xen3,xst2,xen2,'dvdwdxm2d')
  call write_myaverage_real_1d(dvdwdy,xst3,xen3,xst2,xen2,'dvdwdym2d')
  call write_myaverage_real_1d(dvdwdz,xst3,xen3,xst2,xen2,'dvdwdzm2d')!
!
  endif ! Ordre 2

  ! Ordre 3
  if (.true.) then
!
  ! Init ordre 3
  uuu=0; uvv=0; uww=0;
  vuu=0; vvv=0; vww=0;
  wuu=0; wvv=0; www=0;
  uvw=0;
!
  ! Compute average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    uuu(i,j)	=uuu(i,j)		+uuum(i,j,k)
    uvv(i,j)	=uvv(i,j)		+uvvm(i,j,k)
    uww(i,j)	=uww(i,j)		+uwwm(i,j,k)
!
    vuu(i,j)	=vuu(i,j)		+vuum(i,j,k)
    vvv(i,j)	=vvv(i,j)		+vvvm(i,j,k)
    vww(i,j)	=vww(i,j)		+vwwm(i,j,k)
!
    wuu(i,j)	=wuu(i,j)		+wuum(i,j,k)
    wvv(i,j)	=wvv(i,j)		+wvvm(i,j,k)
    www(i,j)	=www(i,j)		+wwwm(i,j,k)
!
    uvw(i,j)	=uvw(i,j)		+uvwm(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 3
!
  ! Write data
  call write_myaverage_real_1d(uuu,xst3,xen3,xst2,xen2,'uuum2d')
  call write_myaverage_real_1d(uvv,xst3,xen3,xst2,xen2,'uvvm2d')
  call write_myaverage_real_1d(uww,xst3,xen3,xst2,xen2,'uwwm2d')
!
  call write_myaverage_real_1d(vuu,xst3,xen3,xst2,xen2,'vuum2d')
  call write_myaverage_real_1d(vvv,xst3,xen3,xst2,xen2,'vvvm2d')
  call write_myaverage_real_1d(vww,xst3,xen3,xst2,xen2,'vwwm2d')
!
  call write_myaverage_real_1d(wuu,xst3,xen3,xst2,xen2,'wuum2d')
  call write_myaverage_real_1d(wvv,xst3,xen3,xst2,xen2,'wvvm2d')
  call write_myaverage_real_1d(www,xst3,xen3,xst2,xen2,'wwwm2d')
!
  call write_myaverage_real_1d(uvw,xst3,xen3,xst2,xen2,'uvwm2d')
!
  endif ! Ordre 3

  ! Scalaire
  if (iscalar.eq.1) then

  ! Ordre 1
  if (.true.) then
!
  ! Init ordre 1
  phi=0.;
  dphidy=0.;
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  dphidx=0.;
  dphidz=0.;
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  deltaphi=0.;
!
  ! Compute Average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    phi(i,j)	=phi(i,j)		+phim(i,j,k)
    dphidy(i,j)	=dphidy(i,j)	+dphidym(i,j,k)
!xxxxxxxxxxxxxxxxxxxxxxxxxxx	
    dphidx(i,j)	=dphidx(i,j)	+dphidxm(i,j,k)
    dphidz(i,j)	=dphidz(i,j)	+dphidzm(i,j,k)
!xxxxxxxxxxxxxxxxxxxxxxxxxxx
    deltaphi(i,j)	=deltaphi(i,j)	+dphidyym(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 1
!
  ! Write data
  call write_myaverage_real_1d(phi	,xst3,xen3,xst2,xen2,     'phim2d')
  call write_myaverage_real_1d(dphidy	,xst3,xen3,xst2,xen2,  'dphidym2d')
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call write_myaverage_real_1d(dphidx	,xst3,xen3,xst2,xen2,  'dphidxm2d')
  call write_myaverage_real_1d(dphidz	,xst3,xen3,xst2,xen2,  'dphidzm2d')
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call write_myaverage_real_1d(deltaphi	,xst3,xen3,xst2,xen2,'deltaphim2d')
!
  !dphidx=0.; dphidz=0.;! In IO thid quantity isn't zero
!
  endif ! Ordre 1
!
  ! Ordre 2
  if (.true.) then
!
  ! Init ordre 2
  uphi=0.; vphi=0.; wphi=0.; phiphi=0.;
  dphidx2=0.; dphidy2=0.; dphidz2=0.;
  phidpdx=0.; phidpdy=0.; phidpdz=0.;
  dudphidx=0.; dvdphidx=0.; dwdphidx=0.;
  dudphidy=0.; dvdphidy=0.; dwdphidy=0.;
  dudphidz=0.; dvdphidz=0.; dwdphidz=0.;
  udeltaphi=0.; vdeltaphi=0.; wdeltaphi=0.;
!
  ! Compute Average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    uphi(i,j)		=uphi(i,j)	+uphim(i,j,k)
    vphi(i,j)		=vphi(i,j)	+vphim(i,j,k)
    wphi(i,j)		=wphi(i,j)	+wphim(i,j,k)
    phiphi(i,j)		=phiphi(i,j)	+phiphim(i,j,k)
    dphidx2(i,j)		=dphidx2(i,j)	+dphidx2m(i,j,k)
    dphidy2(i,j)		=dphidy2(i,j)	+dphidy2m(i,j,k)
    dphidz2(i,j)		=dphidz2(i,j)	+dphidz2m(i,j,k)
    phidpdx(i,j)		=phidpdx(i,j)	+phidpdxm(i,j,k)
    phidpdy(i,j)		=phidpdy(i,j)	+phidpdym(i,j,k)
    phidpdz(i,j)		=phidpdz(i,j)	+phidpdzm(i,j,k)
    dudphidx(i,j)		=dudphidx(i,j)	+dudphidxm(i,j,k)
    dvdphidx(i,j)		=dvdphidx(i,j)	+dvdphidxm(i,j,k)
    dwdphidx(i,j)		=dwdphidx(i,j)	+dwdphidxm(i,j,k)
    dudphidy(i,j)		=dudphidy(i,j)	+dudphidym(i,j,k)
    dvdphidy(i,j)		=dvdphidy(i,j)	+dvdphidym(i,j,k)
    dwdphidy(i,j)		=dwdphidy(i,j)	+dwdphidym(i,j,k)
    dudphidz(i,j)		=dudphidz(i,j)	+dudphidzm(i,j,k)
    dvdphidz(i,j)		=dvdphidz(i,j)	+dvdphidzm(i,j,k)
    dwdphidz(i,j)		=dwdphidz(i,j)	+dwdphidzm(i,j,k)
    udeltaphi(i,j)	=udeltaphi(i,j)&
 +udtdxxm(i,j,k)+udtdyym(i,j,k)+udtdzzm(i,j,k)
    vdeltaphi(i,j)	=vdeltaphi(i,j)&
 +vdtdxxm(i,j,k)+vdtdyym(i,j,k)+vdtdzzm(i,j,k)
    wdeltaphi(i,j)	=wdeltaphi(i,j)&
 +wdtdxxm(i,j,k)+wdtdyym(i,j,k)+wdtdzzm(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 2
!
  ! Write data
  call write_myaverage_real_1d(uphi		,xst3,xen3,xst2,xen2,      'uphim2d')
  call write_myaverage_real_1d(vphi		,xst3,xen3,xst2,xen2,      'vphim2d')
  call write_myaverage_real_1d(wphi		,xst3,xen3,xst2,xen2,      'wphim2d')
  call write_myaverage_real_1d(phiphi		,xst3,xen3,xst2,xen2,    'phiphim2d')
  call write_myaverage_real_1d(dphidx2	,xst3,xen3,xst2,xen2,  'dphidx2m2d')
  call write_myaverage_real_1d(dphidy2	,xst3,xen3,xst2,xen2,  'dphidy2m2d')
  call write_myaverage_real_1d(dphidz2	,xst3,xen3,xst2,xen2,  'dphidz2m2d')
  call write_myaverage_real_1d(phidpdx	,xst3,xen3,xst2,xen2,  'phidpdxm2d')
  call write_myaverage_real_1d(phidpdy	,xst3,xen3,xst2,xen2,  'phidpdym2d')
  call write_myaverage_real_1d(phidpdz	,xst3,xen3,xst2,xen2,  'phidpdzm2d')
  call write_myaverage_real_1d(dudphidx	,xst3,xen3,xst2,xen2, 'dudphidxm2d')
  call write_myaverage_real_1d(dvdphidx	,xst3,xen3,xst2,xen2, 'dvdphidxm2d')
  call write_myaverage_real_1d(dwdphidx	,xst3,xen3,xst2,xen2, 'dwdphidxm2d')
  call write_myaverage_real_1d(dudphidy	,xst3,xen3,xst2,xen2, 'dudphidym2d')
  call write_myaverage_real_1d(dvdphidy	,xst3,xen3,xst2,xen2, 'dvdphidym2d')
  call write_myaverage_real_1d(dwdphidy	,xst3,xen3,xst2,xen2, 'dwdphidym2d')
  call write_myaverage_real_1d(dudphidz	,xst3,xen3,xst2,xen2, 'dudphidzm2d')
  call write_myaverage_real_1d(dvdphidz	,xst3,xen3,xst2,xen2, 'dvdphidzm2d')
  call write_myaverage_real_1d(dwdphidz	,xst3,xen3,xst2,xen2, 'dwdphidzm2d')
  call write_myaverage_real_1d(udeltaphi	,xst3,xen3,xst2,xen2,'udeltaphim2d')
  call write_myaverage_real_1d(vdeltaphi	,xst3,xen3,xst2,xen2,'vdeltaphim2d')
  call write_myaverage_real_1d(wdeltaphi	,xst3,xen3,xst2,xen2,'wdeltaphim2d')
!
  endif ! Ordre 2

  ! Ordre 3
  if (.true.) then
!
  ! Init ordre 3
  uphi2=0.; vphi2=0.; wphi2=0.; 
  phiuu=0.; phivv=0.; phiww=0.; 
  phiuv=0.; phiuw=0.; phivw=0.; 
!
  ! Compute Average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    uphi2(i,j) = uphi2(i,j) + uphi2m(i,j,k)
    vphi2(i,j) = vphi2(i,j) + vphi2m(i,j,k)
    wphi2(i,j) = wphi2(i,j) + wphi2m(i,j,k)
    phiuu(i,j) = phiuu(i,j) + phiuum(i,j,k)
    phivv(i,j) = phivv(i,j) + phivvm(i,j,k)
    phiww(i,j) = phiww(i,j) + phiwwm(i,j,k)
    phiuv(i,j) = phiuv(i,j) + phiuvm(i,j,k)
    phiuw(i,j) = phiuw(i,j) + phiuwm(i,j,k)
    phivw(i,j) = phivw(i,j) + phivwm(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 3
!
  ! Write data
  call write_myaverage_real_1d(uphi2  ,xst3,xen3,xst2,xen2,'uphi2m2d')
  call write_myaverage_real_1d(vphi2  ,xst3,xen3,xst2,xen2,'vphi2m2d')
  call write_myaverage_real_1d(wphi2  ,xst3,xen3,xst2,xen2,'wphi2m2d')
  call write_myaverage_real_1d(phiuu      ,xst3,xen3,xst2,xen2,'phiuum2d')
  call write_myaverage_real_1d(phivv      ,xst3,xen3,xst2,xen2,'phivvm2d')
  call write_myaverage_real_1d(phiww      ,xst3,xen3,xst2,xen2,'phiwwm2d')
  call write_myaverage_real_1d(phiuv,xst3,xen3,xst2,xen2,'phiuvm2d')
  call write_myaverage_real_1d(phiuw,xst3,xen3,xst2,xen2,'phiuwm2d')
  call write_myaverage_real_1d(phivw,xst3,xen3,xst2,xen2,'phivwm2d')
!
  endif ! Ordre 3

  endif ! Scalaire

  ! Pression
  if (.true.) then !(.true.) then
!
  ! Init
  pres=0.; pres2=0.;
!
  ! Compute average
  do k=xst3,xen3
  do j=xst2,xen2
  do i=xst1,xen1
    pres(i,j)  = pres(i,j)  + presm(i,j,k)
    pres2(i,j) = pres2(i,j) + pres2m(i,j,k)
  end do
  end do
  end do
!
  ! Average
!
  ! Write data
  call write_myaverage_real_1d(pres             ,xst3,xen3,xst2,xen2, 'presm2d')
  call write_myaverage_real_1d(pres2,xst3,xen3,xst2,xen2,'pres2m2d')
!
  endif

! Pression sur maillage décalé
!
!if (.true.) then
!
  ! Init
!  presi=0.; pres2i=0.;
!
  ! From z-pencil to x-pencil
!  call transpose_z_to_y(pp3m, pp3my,ph3)
!  call transpose_y_to_x(pp3my,pp3mx,ph3)
!  call transpose_z_to_y(pp32m, pp3my,ph3)
!  call transpose_y_to_x(pp3my,pp32mx,ph3)
!
  ! Compute average
!  do k=ph3%xst(3),ph3%xen(3)
!  do j=ph3%xst(2),ph3%xen(2)
!  do i=ph3%xst(1),ph3%xen(1)
!    presi(j)  = presi(j)  + pp3mx(i,j,k)
!    pres2i(j) = pres2i(j) + pp32mx(i,j,k)
!  end do
!  end do
!  end do
!
  ! Average
!  n=(ph3%xen(3)-ph3%xst(3)+1)*(ph3%xen(1)-ph3%xst(1)+1)
  !
!  presi=presi/n; pres2i=pres2i/n;
!
  ! Write data
!  call write_myaverage_real_1di(presi               ,ph3%xen(2)-ph3%xst(2)+1,n,ph3%xst(2),ph3%xen(2), 'presim2d')
!  call write_myaverage_real_1di(pres2i - presi*presi,ph3%xen(2)-ph3%xst(2)+1,n,ph3%xst(2),ph3%xen(2),'pres2im2d')
!
!endif
!

end subroutine compute_balance_2d

subroutine write_myaverage_real_1d(my_var,kst,ken,jst,jen,filename)
! ojo que estamos en forma paralela en lapiz x
! my_var: variable
! var_size: tamaño de variable
! average_size: promedio en dirección homogénea en cada proceso
! jst: donde empieza 
! jen: donde termina
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
use decomp_2d, only : xsize
use decomp_2d_constants, only : mytype, real_type
use decomp_2d_mpi, only : nrank
use variables, only : yp
use MPI
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

implicit none

integer, intent(in) :: jst,jen,kst,ken
real(mytype), intent(in) :: my_var(xsize(1),jst:jen)! 2d slices averaged in z direction
character(len=*), intent(IN) :: filename

integer, parameter :: num_unit_average=1111
integer :: total_size,total_sizez, jmax, jmin, kmax, kmin ! total size in y direction
integer :: ierror, i, j, count
integer, allocatable, dimension(:,:)      :: compteur, tmp_int ! variables to store average sizes in z direction for each process
real(mytype), allocatable, dimension(:,:) :: glob_var, tmp_dbe ! variables to store the 2d slices

!print *, 'size in x', xsize(1), xsize(2), xsize(3)
! Compute total size
call MPI_AllReduce(jst,jmin,1,MPI_INTEGER,MPI_MIN,MPI_COMM_WORLD,ierror) ! minimum number in y direction
call MPI_AllReduce(jen,jmax,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,ierror) ! maximun number in y direction
call MPI_AllReduce(kst,kmin,1,MPI_INTEGER,MPI_MIN,MPI_COMM_WORLD,ierror) ! minimum number in y direction
call MPI_AllReduce(ken,kmax,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,ierror) ! maximun number in y direction

! Init
total_size = jmax - jmin + 1 ! total size in y direction
total_sizez = kmax - kmin + 1 ! total size in y direction
allocate(glob_var(xsize(1),jmin:jmax),tmp_dbe(xsize(1),jmin:jmax))! this variable has the total size and live in each process
glob_var = 0.
tmp_dbe = 0.
allocate(compteur(xsize(1),jmin:jmax),tmp_int(xsize(1),jmin:jmax)) !
compteur = 0
tmp_int = 0

! Affect
!glob_var vive en todos los procesos y adquiere valores sólo en los nodos de cada proceso
! por eso esta bien hacer un MPI_reduce, pq en algunos procesos algunos nodos de glob_var cerán ceros
! y en otros, esos mismos nodos, cerán números. Por lo tanto está bien hacer el promedio
glob_var(1:xsize(1),jst:jen) = my_var(1:xsize(1),jst:jen)

! Regroup data on rank 0
! in tmp_int compute the sum of compteur (size in each process)
! in tmp_dbe is the sum of glob_var (variable in each process)
call MPI_Reduce(compteur,tmp_int,xsize(1)*total_size,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierror)
call MPI_Reduce(glob_var,tmp_dbe,xsize(1)*total_size,real_type,MPI_SUM,0,MPI_COMM_WORLD,ierror)
if (nrank.eq.0) then
  compteur = tmp_int
  glob_var = tmp_dbe
endif
deallocate(tmp_int,tmp_dbe)

! Rank 0 compute the average then write data in text format
if (nrank.eq.0) then
    glob_var = glob_var / total_sizez
  ! Write average
!  open(num_unit_average,file=filename//'.dat',form='formatted', status='unknown')!ok ascii
!  do j=jmin,jmax
!    do i=1,xsize(1)	
!      write(num_unit_average,*)glob_var(i,j)
!    enddo
!  enddo
!  close(num_unit_average)
!
! al parecer en binario es importante poner acceso directo y recl, dado que, al parecer, guarda aleatoriamente los datos
! 
  open(num_unit_average,FILE='stats2D/'//filename//'.dat',FORM='UNFORMATTED', ACCESS='DIRECT', RECL=8, STATUS='REPLACE')!ok binary, falta leer
  COUNT = 1
  do j=jmin,jmax
    do i=1,xsize(1)	
      write(num_unit_average,REC=COUNT) glob_var(i,j)
      COUNT = COUNT + 1
    enddo
  enddo
  close(num_unit_average)

endif

deallocate(glob_var,compteur)

end subroutine write_myaverage_real_1d
