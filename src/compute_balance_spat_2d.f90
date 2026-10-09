subroutine compute_balance_spat_2d()
! We work in x pencil
  use decomp_2d_constants, only : mytype
  use param, only : iscalar, itime
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
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  real(mytype), dimension(xst1:xen1,xst2:xen2) :: dudt, dvdt, dwdt
  character(len=10) :: timer
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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
    u(i,j)	=u(i,j)	+ums(i,j,k)
    v(i,j)	=v(i,j)	+vms(i,j,k)
    w(i,j)	=w(i,j)	+wms(i,j,k)
    dpdx(i,j)	=dpdx(i,j)+dpdxms(i,j,k)
    dpdy(i,j)	=dpdy(i,j)+dpdyms(i,j,k)
    dpdz(i,j)	=dpdz(i,j)+dpdzms(i,j,k)
!	Falta d*dx
    dudy(i,j)     =dudy(i,j)+dudyms(i,j,k)
    dvdy(i,j)     =dvdy(i,j)+dvdyms(i,j,k)
    dwdy(i,j)     =dwdy(i,j)+dwdyms(i,j,k)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    dudt(i,j)     =dudt(i,j)+dudtms(i,j,k)
    dvdt(i,j)     =dvdt(i,j)+dvdtms(i,j,k)
    dwdt(i,j)     =dwdt(i,j)+dwdtms(i,j,k)

    dudx(i,j)     =dudx(i,j)+dudxms(i,j,k)
    dvdx(i,j)     =dvdx(i,j)+dvdxms(i,j,k)
    dwdx(i,j)     =dwdx(i,j)+dwdxms(i,j,k)

    dudz(i,j)     =dudz(i,j)+dudzms(i,j,k)
    dvdz(i,j)     =dvdz(i,j)+dvdzms(i,j,k)
    dwdz(i,j)     =dwdz(i,j)+dwdzms(i,j,k)

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  end do
  end do
  end do
!
  ! Average ordre 1
	! Only average in z direction
!
  ! Write data
 154 format('t',I9.9)
  write(timer,154) itime
  call write_myaverage_real_2d(u   ,xst3,xen3,xst2,xen2,   'ums2d'//timer//'')
  call write_myaverage_real_2d(v   ,xst3,xen3,xst2,xen2,   'vms2d'//timer//'')
  call write_myaverage_real_2d(w   ,xst3,xen3,xst2,xen2,   'wms2d'//timer//'')
  call write_myaverage_real_2d(dpdx,xst3,xen3,xst2,xen2,'dpdxms2d'//timer//'')
  call write_myaverage_real_2d(dpdy,xst3,xen3,xst2,xen2,'dpdyms2d'//timer//'')
  call write_myaverage_real_2d(dpdz,xst3,xen3,xst2,xen2,'dpdzms2d'//timer//'')
  call write_myaverage_real_2d(dudy,xst3,xen3,xst2,xen2,'dudyms2d'//timer//'')
  call write_myaverage_real_2d(dvdy,xst3,xen3,xst2,xen2,'dvdyms2d'//timer//'')
  call write_myaverage_real_2d(dwdy,xst3,xen3,xst2,xen2,'dwdyms2d'//timer//'')
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call write_myaverage_real_2d(dudt,xst3,xen3,xst2,xen2,'dudtms2d'//timer//'')
  call write_myaverage_real_2d(dvdt,xst3,xen3,xst2,xen2,'dvdtms2d'//timer//'')
  call write_myaverage_real_2d(dwdt,xst3,xen3,xst2,xen2,'dwdtms2d'//timer//'')
  call write_myaverage_real_2d(dudx,xst3,xen3,xst2,xen2,'dudxms2d'//timer//'')
  call write_myaverage_real_2d(dvdx,xst3,xen3,xst2,xen2,'dvdxms2d'//timer//'')
  call write_myaverage_real_2d(dwdx,xst3,xen3,xst2,xen2,'dwdxms2d'//timer//'')
  call write_myaverage_real_2d(dudz,xst3,xen3,xst2,xen2,'dudzms2d'//timer//'')
  call write_myaverage_real_2d(dvdz,xst3,xen3,xst2,xen2,'dvdzms2d'//timer//'')
  call write_myaverage_real_2d(dwdz,xst3,xen3,xst2,xen2,'dwdzms2d'//timer//'')

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
    uu(i,j)	=uu(i,j)		+uums(i,j,k)
    vv(i,j)	=vv(i,j)		+vvms(i,j,k)
    ww(i,j)	=ww(i,j)		+wwms(i,j,k)
    uv(i,j)	=uv(i,j)		+uvms(i,j,k)
    uw(i,j)	=uw(i,j)		+uwms(i,j,k)
    vw(i,j)	=vw(i,j)		+vwms(i,j,k)
!
    udpdx(i,j)	=udpdx(i,j)	+udpdxms(i,j,k)
    udpdy(i,j)	=udpdy(i,j)	+udpdyms(i,j,k)
    udpdz(i,j)	=udpdz(i,j)	+udpdzms(i,j,k)
!
    vdpdx(i,j)	=vdpdx(i,j)	+vdpdxms(i,j,k)
    vdpdy(i,j)	=vdpdy(i,j)	+vdpdyms(i,j,k)
    vdpdz(i,j)	=vdpdz(i,j)	+vdpdzms(i,j,k)
!
    wdpdx(i,j)	=wdpdx(i,j)	+wdpdxms(i,j,k)
    wdpdy(i,j)	=wdpdy(i,j)	+wdpdyms(i,j,k)
    wdpdz(i,j)	=wdpdz(i,j)	+wdpdzms(i,j,k)
!
    dudx2(i,j)	=dudx2(i,j)	+dudx2ms(i,j,k)
    dudy2(i,j)	=dudy2(i,j)	+dudy2ms(i,j,k)
    dudz2(i,j)	=dudz2(i,j)	+dudz2ms(i,j,k)
!
    dvdx2(i,j)	=dvdx2(i,j)	+dvdx2ms(i,j,k)
    dvdy2(i,j)	=dvdy2(i,j)	+dvdy2ms(i,j,k)
    dvdz2(i,j)	=dvdz2(i,j)	+dvdz2ms(i,j,k)
!
    dwdx2(i,j)	=dwdx2(i,j)	+dwdx2ms(i,j,k)
    dwdy2(i,j)	=dwdy2(i,j)	+dwdy2ms(i,j,k)
    dwdz2(i,j)	=dwdz2(i,j)	+dwdz2ms(i,j,k)
!
    dudvdx(i,j)	 =dudvdx(i,j)	+dudvdxms(i,j,k)
    dudwdx(i,j)	 =dudwdx(i,j)	+dudwdxms(i,j,k)
    dvdwdx(i,j)	 =dvdwdx(i,j)	+dvdwdxms(i,j,k)
!
    dudvdy(i,j)	 =dudvdy(i,j)	+dudvdyms(i,j,k)
    dudwdy(i,j)	 =dudwdy(i,j)	+dudwdyms(i,j,k)
    dvdwdy(i,j)	 =dvdwdy(i,j)	+dvdwdyms(i,j,k)
!
    dudvdz(i,j)	 =dudvdz(i,j)	+dudvdzms(i,j,k)
    dudwdz(i,j)	 =dudwdz(i,j)	+dudwdzms(i,j,k)
    dvdwdz(i,j)	 =dvdwdz(i,j)	+dvdwdzms(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 2
!
  ! Write data
 155 format('t',I9.9)
  write(timer,155) itime
  call write_myaverage_real_2d(uu,xst3,xen3,xst2,xen2,'uums2d'//timer//'')
  call write_myaverage_real_2d(vv,xst3,xen3,xst2,xen2,'vvms2d'//timer//'')
  call write_myaverage_real_2d(ww,xst3,xen3,xst2,xen2,'wwms2d'//timer//'')
  call write_myaverage_real_2d(uv,xst3,xen3,xst2,xen2,'uvms2d'//timer//'')
  call write_myaverage_real_2d(uw,xst3,xen3,xst2,xen2,'uwms2d'//timer//'')
  call write_myaverage_real_2d(vw,xst3,xen3,xst2,xen2,'vwms2d'//timer//'')
!
  call write_myaverage_real_2d(udpdx,xst3,xen3,xst2,xen2,'udpdxms2d'//timer//'')
  call write_myaverage_real_2d(udpdy,xst3,xen3,xst2,xen2,'udpdyms2d'//timer//'')
  call write_myaverage_real_2d(udpdz,xst3,xen3,xst2,xen2,'udpdzms2d'//timer//'')
  call write_myaverage_real_2d(vdpdx,xst3,xen3,xst2,xen2,'vdpdxms2d'//timer//'')
  call write_myaverage_real_2d(vdpdy,xst3,xen3,xst2,xen2,'vdpdyms2d'//timer//'')
  call write_myaverage_real_2d(vdpdz,xst3,xen3,xst2,xen2,'vdpdzms2d'//timer//'')
  call write_myaverage_real_2d(wdpdx,xst3,xen3,xst2,xen2,'wdpdxms2d'//timer//'')
  call write_myaverage_real_2d(wdpdy,xst3,xen3,xst2,xen2,'wdpdyms2d'//timer//'')
  call write_myaverage_real_2d(wdpdz,xst3,xen3,xst2,xen2,'wdpdzms2d'//timer//'')
!
  call write_myaverage_real_2d(dudx2,xst3,xen3,xst2,xen2,'dudx2ms2d'//timer//'')
  call write_myaverage_real_2d(dudy2,xst3,xen3,xst2,xen2,'dudy2ms2d'//timer//'')
  call write_myaverage_real_2d(dudz2,xst3,xen3,xst2,xen2,'dudz2ms2d'//timer//'')
  call write_myaverage_real_2d(dvdx2,xst3,xen3,xst2,xen2,'dvdx2ms2d'//timer//'')
  call write_myaverage_real_2d(dvdy2,xst3,xen3,xst2,xen2,'dvdy2ms2d'//timer//'')
  call write_myaverage_real_2d(dvdz2,xst3,xen3,xst2,xen2,'dvdz2ms2d'//timer//'')
  call write_myaverage_real_2d(dwdx2,xst3,xen3,xst2,xen2,'dwdx2ms2d'//timer//'')
  call write_myaverage_real_2d(dwdy2,xst3,xen3,xst2,xen2,'dwdy2ms2d'//timer//'')
  call write_myaverage_real_2d(dwdz2,xst3,xen3,xst2,xen2,'dwdz2ms2d'//timer//'')
!
  call write_myaverage_real_2d(dudvdx,xst3,xen3,xst2,xen2,'dudvdxms2d'//timer//'')
  call write_myaverage_real_2d(dudvdy,xst3,xen3,xst2,xen2,'dudvdyms2d'//timer//'')
  call write_myaverage_real_2d(dudvdz,xst3,xen3,xst2,xen2,'dudvdzms2d'//timer//'')
  call write_myaverage_real_2d(dudwdx,xst3,xen3,xst2,xen2,'dudwdxms2d'//timer//'')
  call write_myaverage_real_2d(dudwdy,xst3,xen3,xst2,xen2,'dudwdyms2d'//timer//'')
  call write_myaverage_real_2d(dudwdz,xst3,xen3,xst2,xen2,'dudwdzms2d'//timer//'')
  call write_myaverage_real_2d(dvdwdx,xst3,xen3,xst2,xen2,'dvdwdxms2d'//timer//'')
  call write_myaverage_real_2d(dvdwdy,xst3,xen3,xst2,xen2,'dvdwdyms2d'//timer//'')
  call write_myaverage_real_2d(dvdwdz,xst3,xen3,xst2,xen2,'dvdwdzms2d'//timer//'')!
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
    uuu(i,j)	=uuu(i,j)		+uuums(i,j,k)
    uvv(i,j)	=uvv(i,j)		+uvvms(i,j,k)
    uww(i,j)	=uww(i,j)		+uwwms(i,j,k)
!
    vuu(i,j)	=vuu(i,j)		+vuums(i,j,k)
    vvv(i,j)	=vvv(i,j)		+vvvms(i,j,k)
    vww(i,j)	=vww(i,j)		+vwwms(i,j,k)
!
    wuu(i,j)	=wuu(i,j)		+wuums(i,j,k)
    wvv(i,j)	=wvv(i,j)		+wvvms(i,j,k)
    www(i,j)	=www(i,j)		+wwwms(i,j,k)
!
    uvw(i,j)	=uvw(i,j)		+uvwms(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 3
!
  ! Write data
 156 format('t',I9.9)
  write(timer,156) itime
  call write_myaverage_real_2d(uuu,xst3,xen3,xst2,xen2,'uuums2d'//timer//'')
  call write_myaverage_real_2d(uvv,xst3,xen3,xst2,xen2,'uvvms2d'//timer//'')
  call write_myaverage_real_2d(uww,xst3,xen3,xst2,xen2,'uwwms2d'//timer//'')
!
  call write_myaverage_real_2d(vuu,xst3,xen3,xst2,xen2,'vuums2d'//timer//'')
  call write_myaverage_real_2d(vvv,xst3,xen3,xst2,xen2,'vvvms2d'//timer//'')
  call write_myaverage_real_2d(vww,xst3,xen3,xst2,xen2,'vwwms2d'//timer//'')
!
  call write_myaverage_real_2d(wuu,xst3,xen3,xst2,xen2,'wuums2d'//timer//'')
  call write_myaverage_real_2d(wvv,xst3,xen3,xst2,xen2,'wvvms2d'//timer//'')
  call write_myaverage_real_2d(www,xst3,xen3,xst2,xen2,'wwwms2d'//timer//'')
!
  call write_myaverage_real_2d(uvw,xst3,xen3,xst2,xen2,'uvwms2d'//timer//'')
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
    phi(i,j)	=phi(i,j)		+phims(i,j,k)
    dphidy(i,j)	=dphidy(i,j)	+dphidyms(i,j,k)
!xxxxxxxxxxxxxxxxxxxxxxxxxxx	
    dphidx(i,j)	=dphidx(i,j)	+dphidxms(i,j,k)
    dphidz(i,j)	=dphidz(i,j)	+dphidzms(i,j,k)
!xxxxxxxxxxxxxxxxxxxxxxxxxxx
    deltaphi(i,j)	=deltaphi(i,j)	+dphidyyms(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 1
!
  ! Write data
 158 format('t',I9.9)
  write(timer,158) itime
  call write_myaverage_real_2d(phi	,xst3,xen3,xst2,xen2,     'phims2d'//timer//'')
  call write_myaverage_real_2d(dphidy	,xst3,xen3,xst2,xen2,  'dphidyms2d'//timer//'')
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call write_myaverage_real_2d(dphidx	,xst3,xen3,xst2,xen2,  'dphidxms2d'//timer//'')
  call write_myaverage_real_2d(dphidz	,xst3,xen3,xst2,xen2,  'dphidzms2d'//timer//'')
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  call write_myaverage_real_2d(deltaphi	,xst3,xen3,xst2,xen2,'deltaphims2d'//timer//'')
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
    uphi(i,j)		=uphi(i,j)	+uphims(i,j,k)
    vphi(i,j)		=vphi(i,j)	+vphims(i,j,k)
    wphi(i,j)		=wphi(i,j)	+wphims(i,j,k)
    phiphi(i,j)		=phiphi(i,j)	+phiphims(i,j,k)
    dphidx2(i,j)		=dphidx2(i,j)	+dphidx2ms(i,j,k)
    dphidy2(i,j)		=dphidy2(i,j)	+dphidy2ms(i,j,k)
    dphidz2(i,j)		=dphidz2(i,j)	+dphidz2ms(i,j,k)
    phidpdx(i,j)		=phidpdx(i,j)	+phidpdxms(i,j,k)
    phidpdy(i,j)		=phidpdy(i,j)	+phidpdyms(i,j,k)
    phidpdz(i,j)		=phidpdz(i,j)	+phidpdzms(i,j,k)
    dudphidx(i,j)		=dudphidx(i,j)	+dudphidxms(i,j,k)
    dvdphidx(i,j)		=dvdphidx(i,j)	+dvdphidxms(i,j,k)
    dwdphidx(i,j)		=dwdphidx(i,j)	+dwdphidxms(i,j,k)
    dudphidy(i,j)		=dudphidy(i,j)	+dudphidyms(i,j,k)
    dvdphidy(i,j)		=dvdphidy(i,j)	+dvdphidyms(i,j,k)
    dwdphidy(i,j)		=dwdphidy(i,j)	+dwdphidyms(i,j,k)
    dudphidz(i,j)		=dudphidz(i,j)	+dudphidzms(i,j,k)
    dvdphidz(i,j)		=dvdphidz(i,j)	+dvdphidzms(i,j,k)
    dwdphidz(i,j)		=dwdphidz(i,j)	+dwdphidzms(i,j,k)
    udeltaphi(i,j)	=udeltaphi(i,j)&
 +udtdxxms(i,j,k)+udtdyyms(i,j,k)+udtdzzms(i,j,k)
    vdeltaphi(i,j)	=vdeltaphi(i,j)&
 +vdtdxxms(i,j,k)+vdtdyyms(i,j,k)+vdtdzzms(i,j,k)
    wdeltaphi(i,j)	=wdeltaphi(i,j)&
 +wdtdxxms(i,j,k)+wdtdyyms(i,j,k)+wdtdzzms(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 2
!
  ! Write data
 159 format('t',I9.9)
  write(timer,159) itime
  call write_myaverage_real_2d(uphi		,xst3,xen3,xst2,xen2,      'uphims2d'//timer//'')
  call write_myaverage_real_2d(vphi		,xst3,xen3,xst2,xen2,      'vphims2d'//timer//'')
  call write_myaverage_real_2d(wphi		,xst3,xen3,xst2,xen2,      'wphims2d'//timer//'')
  call write_myaverage_real_2d(phiphi		,xst3,xen3,xst2,xen2,    'phiphims2d'//timer//'')
  call write_myaverage_real_2d(dphidx2	,xst3,xen3,xst2,xen2,  'dphidx2ms2d'//timer//'')
  call write_myaverage_real_2d(dphidy2	,xst3,xen3,xst2,xen2,  'dphidy2ms2d'//timer//'')
  call write_myaverage_real_2d(dphidz2	,xst3,xen3,xst2,xen2,  'dphidz2ms2d'//timer//'')
  call write_myaverage_real_2d(phidpdx	,xst3,xen3,xst2,xen2,  'phidpdxms2d'//timer//'')
  call write_myaverage_real_2d(phidpdy	,xst3,xen3,xst2,xen2,  'phidpdyms2d'//timer//'')
  call write_myaverage_real_2d(phidpdz	,xst3,xen3,xst2,xen2,  'phidpdzms2d'//timer//'')
  call write_myaverage_real_2d(dudphidx	,xst3,xen3,xst2,xen2, 'dudphidxms2d'//timer//'')
  call write_myaverage_real_2d(dvdphidx	,xst3,xen3,xst2,xen2, 'dvdphidxms2d'//timer//'')
  call write_myaverage_real_2d(dwdphidx	,xst3,xen3,xst2,xen2, 'dwdphidxms2d'//timer//'')
  call write_myaverage_real_2d(dudphidy	,xst3,xen3,xst2,xen2, 'dudphidyms2d'//timer//'')
  call write_myaverage_real_2d(dvdphidy	,xst3,xen3,xst2,xen2, 'dvdphidyms2d'//timer//'')
  call write_myaverage_real_2d(dwdphidy	,xst3,xen3,xst2,xen2, 'dwdphidyms2d'//timer//'')
  call write_myaverage_real_2d(dudphidz	,xst3,xen3,xst2,xen2, 'dudphidzms2d'//timer//'')
  call write_myaverage_real_2d(dvdphidz	,xst3,xen3,xst2,xen2, 'dvdphidzms2d'//timer//'')
  call write_myaverage_real_2d(dwdphidz	,xst3,xen3,xst2,xen2, 'dwdphidzms2d'//timer//'')
  call write_myaverage_real_2d(udeltaphi	,xst3,xen3,xst2,xen2,'udeltaphims2d'//timer//'')
  call write_myaverage_real_2d(vdeltaphi	,xst3,xen3,xst2,xen2,'vdeltaphims2d'//timer//'')
  call write_myaverage_real_2d(wdeltaphi	,xst3,xen3,xst2,xen2,'wdeltaphims2d'//timer//'')
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
    uphi2(i,j) = uphi2(i,j) + uphi2ms(i,j,k)
    vphi2(i,j) = vphi2(i,j) + vphi2ms(i,j,k)
    wphi2(i,j) = wphi2(i,j) + wphi2ms(i,j,k)
    phiuu(i,j) = phiuu(i,j) + phiuums(i,j,k)
    phivv(i,j) = phivv(i,j) + phivvms(i,j,k)
    phiww(i,j) = phiww(i,j) + phiwwms(i,j,k)
    phiuv(i,j) = phiuv(i,j) + phiuvms(i,j,k)
    phiuw(i,j) = phiuw(i,j) + phiuwms(i,j,k)
    phivw(i,j) = phivw(i,j) + phivwms(i,j,k)
  end do
  end do
  end do
!
  ! Average ordre 3
!
  ! Write data
 160 format('t',I9.9)
  write(timer,160) itime
  call write_myaverage_real_2d(uphi2  ,xst3,xen3,xst2,xen2,'uphi2ms2d'//timer//'')
  call write_myaverage_real_2d(vphi2  ,xst3,xen3,xst2,xen2,'vphi2ms2d'//timer//'')
  call write_myaverage_real_2d(wphi2  ,xst3,xen3,xst2,xen2,'wphi2ms2d'//timer//'')
  call write_myaverage_real_2d(phiuu      ,xst3,xen3,xst2,xen2,'phiuums2d'//timer//'')
  call write_myaverage_real_2d(phivv      ,xst3,xen3,xst2,xen2,'phivvms2d'//timer//'')
  call write_myaverage_real_2d(phiww      ,xst3,xen3,xst2,xen2,'phiwwms2d'//timer//'')
  call write_myaverage_real_2d(phiuv,xst3,xen3,xst2,xen2,'phiuvms2d'//timer//'')
  call write_myaverage_real_2d(phiuw,xst3,xen3,xst2,xen2,'phiuwms2d'//timer//'')
  call write_myaverage_real_2d(phivw,xst3,xen3,xst2,xen2,'phivwms2d'//timer//'')
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
    pres(i,j)  = pres(i,j)  + presms(i,j,k)
    pres2(i,j) = pres2(i,j) + pres2ms(i,j,k)
  end do
  end do
  end do
!
  ! Average
!
  ! Write data
 157 format('t',I9.9)
  write(timer,157) itime
  call write_myaverage_real_2d(pres             ,xst3,xen3,xst2,xen2, 'presms2d'//timer//'')
  call write_myaverage_real_2d(pres2,xst3,xen3,xst2,xen2,'pres2ms2d'//timer//'')
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
!  call write_myaverage_real_2di(presi               ,ph3%xen(2)-ph3%xst(2)+1,n,ph3%xst(2),ph3%xen(2), 'presims2d'//timer//'')
!  call write_myaverage_real_2di(pres2i - presi*presi,ph3%xen(2)-ph3%xst(2)+1,n,ph3%xst(2),ph3%xen(2),'pres2ims2d'//timer//'')
!
!endif
!

end subroutine compute_balance_spat_2d

subroutine write_myaverage_real_2d(my_var,kst,ken,jst,jen,filename)
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
  open(num_unit_average,FILE='statsInst2D/'//filename//'.dat',FORM='UNFORMATTED', ACCESS='DIRECT', RECL=8, STATUS='REPLACE')!ok binary, falta leer
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

end subroutine write_myaverage_real_2d
